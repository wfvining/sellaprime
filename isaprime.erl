-module(isaprime).

-behaviour(gen_server).

-export([start_link/0, test/1, work_done/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(POOLSIZE, 10).

-record(state, {tester_load = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ?POOLSIZE, []).

test(N) ->
    gen_server:call(?SERVER, {is_prime, N}).

work_done(Tester) ->
    gen_server:cast(?SERVER, {work_done, Tester}).

init(PoolSize) ->
    Testers = [prime_tester_server:start_link() || _ <- lists:seq(1, PoolSize)],
    {ok, #state{tester_load = [{Pid, 0} || {ok, Pid} <- Testers ]}}.

handle_call({is_prime, N}, From, State=#state{ tester_load = Load }) ->
    Tester = lowest_load(Load),
    prime_tester_server:is_prime(Tester, N, From),
    {noreply, State#state{ tester_load = increase_load(Tester, Load) }}.

handle_cast({work_done, Tester}, #state{tester_load=Load}) ->
    {noreply, #state{tester_load=reduce_load(Tester, Load)}};
handle_cast(Request, _State) ->
    logger:warning("Unexpected cast: ~p", [Request]).

handle_info(Info, State) ->
    logger:warning("Unnexpected info: ~p", [Info]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

update_load(Tester, Load, Increment) ->
    {value, {_, L}, Load2} = lists:keytake(Tester, 1, Load),
    lists:keysort(2, [{Tester, L + Increment}|Load2]).

reduce_load(Tester, Load) ->
    update_load(Tester, Load, -1).
increase_load(Tester, Load) ->
    update_load(Tester, Load, +1).

lowest_load([{T, _}|_]) -> T.
