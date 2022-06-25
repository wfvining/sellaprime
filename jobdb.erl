-module(jobdb).

-export([initialize/1, start/1]).
-export([job_done/1, get_job/1, assign_job/3, jobs/1, init_load/1, delete_load/1, testers/0, reset/0]).

-record(job, {id, n, from, tester}).
-record(load, {tester, jobs}).

initialize(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(job, [{attributes, record_info(fields, job)},
                              {ram_copies, Nodes}]),
    mnesia:create_table(load, [{attributes, record_info(fields, load)},
                               {ram_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

start(Nodes) ->
    mnesia:wait_for_tables([job, load], 10000),
    NumWorkers = mnesia:table_info(load, size),
    if
        NumWorkers =:= 0 ->
            ok;
        NumWorkers > 0 ->
            {error, already_up}
    end.

reset() ->
    mnesia:clear_table(job),
    mnesia:clear_table(load).

jobs(Tester) ->
    mnesia:dirty_select(job, [{{job, '$1', '$2', '$3', '$4'},
                               [{'=:=', '$4', Tester}],
                               [['$1', '$2', '$3']]}]).

job_done(JobId) ->
    mnesia:dirty_delete(job, JobId),
    mnesia:dirty_update_counter(load, self(), -1).

assign_job(JobId, N, From) ->
    Tester = get_lowest_load(),
    mnesia:dirty_write(#job{id = JobId,
                            n = N,
                            from = From,
                            tester = Tester}),
    mnesia:dirty_update_counter(load, Tester, 1),
    Tester.

get_lowest_load() ->
    Loads = mnesia:dirty_select(load, [{{load, '$1', '$2'}, [], [{{'$1', '$2'}}]}]),
    element(1, keymin(2, Loads)).

keymin(K, [Y|List]) ->
    lists:foldl(fun(X, Acc) ->
                        if element(K, X) < element(K, Acc) -> X;
                           true -> Acc
                        end
                end, Y, List).

init_load(Tester) ->
    mnesia:dirty_write(#load{tester=Tester, jobs=0}).

delete_load(Tester) ->
    mnesia:dirty_delete(load, Tester).

get_job(JobId) ->
    [Job] = mnesia:dirty_read(job, JobId),
    {Job#job.n, Job#job.from}.

testers() ->
    mnesia:dirty_select(load, [{{load, '$1', '_'}, [], ['$1']}]).
