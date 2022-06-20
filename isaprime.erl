-module(isaprime).

-behaviour(gen_server).

-export([start_link/0, test/1, work_done/1, get_job/1, register_worker/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, handle_continue/2]).

-define(SERVER, ?MODULE).

-record(state, {next_job = 1, monitors = #{}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

test(N) ->
    gen_server:call(?SERVER, {is_prime, N}).

work_done(JobId) ->
    jobdb:job_done(JobId).

get_job(JobId) ->
    jobdb:get_job(JobId).

register_worker() ->
    gen_server:call(?SERVER, {register, self()}).

init([]) ->
    %% Set up a table of work
    case jobdb:start([node(), 'backup@alazani']) of
        ok -> {ok, #state{}};
        {error, already_up} -> {ok, #state{}, {continue, recover}}
    end.

handle_continue(recover, State = #state{ monitors = M }) ->
    Testers = jobdb:testers(),
    Monitors = lists:foldl(
                 fun(Tester, Monitors) ->
                         case is_process_alive(Tester) of
                             true -> Monitors#{ erlang:monitor(process, Tester) => Tester };
                             false -> recover_tester(Tester), Monitors
                         end
                 end, M, Testers),
    %% TODO get the max job ID
    {noreply, State#state{ monitors = Monitors }}.

handle_call({register, Worker}, _From,
            State = #state{ monitors = Monitors }) ->
    Ref = erlang:monitor(process, Worker),
    jobdb:init_load(Worker),
    {reply, ok, State#state{ monitors = Monitors#{ Ref => Worker }}};
handle_call({is_prime, N}, From, State=#state{ next_job = JobId }) ->
    assign_job(JobId, N, From),
    {noreply, State#state{next_job = JobId + 1}}.

handle_cast(Request, State) ->
    logger:warning("Unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info({reassign, JobId, N, From}, State) ->
    assign_job(JobId, N, From),
    {noreply, State};
handle_info({recover_tester, Tester, Attempt}, State=#state{ monitors = #{} })
  when Attempt =< 3 ->
    %% If no testers have been registered. Wait and try again.
    timer:send_after(10 * Attempt, {recover_tester, Tester, Attempt + 1}),
    {noreply, State};
handle_info({recover_tester, Tester, _}, State) ->
    redistribute_jobs(Tester),
    {noreply, State};
handle_info({'DOWN', Ref, process, _, _},
            State = #state{ monitors = Monitors}) ->
    {Tester, NewMonitors} = maps:take(Ref, Monitors),
    %% Distribute the work to other workers
    redistribute_jobs(Tester),
    {noreply, State#state{monitors = NewMonitors}};
handle_info(Info, State) ->
    logger:warning("Unnexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

redistribute_jobs(Tester) ->
    jobdb:delete_load(Tester),
    Jobs = jobdb:jobs(Tester),
    lists:foreach(
      fun([JobId, N, From]) ->
              %% reassign asynchronously, one at a time, to allow for
              %% a new tester (or testers) to come up. Otherwise we
              %% could end up with a huge load imbalance.
              self() ! {reassign, JobId, N, From}
      end,
      Jobs).

assign_job(JobId, N, From) ->
    Tester = jobdb:assign_job(JobId, N, From),
    prime_tester_server:is_prime(Tester, JobId).

recover_tester(Tester) ->
    self() ! {recover_tester, Tester, 0}.
