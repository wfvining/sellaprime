-module(isaprime).

-behaviour(gen_server).

-export([start_link/0, test/1, work_done/2, get_job/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(POOLSIZE, 32).

-record(state, {work, next_job = 1}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ?POOLSIZE, []).

test(N) ->
    gen_server:call(?SERVER, {is_prime, N}).

work_done(WorkTable, JobId) ->
    %% This could be a problem since it is not atomic, if the server
    %% is killed externally then the load could be inconsistent.
    ets:delete(WorkTable, JobId),
    decrease_load(self(), WorkTable).

get_job(WorkTable, JobId) ->
    [{JobId, N, From, _}] = ets:lookup(WorkTable, JobId),
    {N, From}.

init(PoolSize) ->
    %% Set up a table of work
    T = ets:new(work, [set, public, {write_concurrency, true}]),  % only write concurrency since reads and writes are interleaved
    Testers = [element(2, prime_tester_server:start_link(T)) || _ <- lists:seq(1, PoolSize)],
    % Initialize load counters for each test server
    lists:foreach(fun(Tester) -> ets:insert(T, {Tester, 0}) end, Testers),
    {ok, #state{work = T}}.

handle_call({is_prime, N}, From, State=#state{ work = Work, next_job = JobId }) ->
    Tester = lowest_load(Work),
    ets:insert(Work, {JobId, N, From, Tester}),
    prime_tester_server:is_prime(Tester, JobId),
    increase_load(Tester, Work),
    {noreply, State#state{next_job = JobId + 1}}.

handle_cast(Request, State) ->
    logger:warning("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    logger:warning("Unnexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

decrease_load(Tester, Work) ->
    ets:update_counter(Work, Tester, -1).
increase_load(Tester, Work) ->
    ets:update_counter(Work, Tester, 1).

lowest_load(Work) ->
    element(1, keymin(2, ets:select(Work, [{{'$1', '_'}, [{is_pid, '$1'}], ['$_']}]))).

keymin(K, [Y|List]) ->
    lists:foldl(fun(X, Acc) ->
                        if element(K, X) < element(K, Acc) -> X;
                           true -> Acc
                        end
                end, Y, List).
