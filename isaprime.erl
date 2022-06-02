-module(isaprime).

-behaviour(gen_server).

-export([start_link/0, test/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(POOLSIZE, 10).

-record(state, {queue = queue:new(),
                available_servers = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, ?POOLSIZE, []).

test(N) ->
    gen_server:call(?SERVER, {is_prime, N}).

init(PoolSize) ->
    {ok, #state{available_servers = [element(2, {ok, _Pid} = prime_tester_server:start_link()) || _ <- lists:seq(1, PoolSize)]}}.

handle_call({is_prime, N}, From, State = #state{ available_servers = [],
                                                  queue = Q}) ->
    {noreply, State#state{queue = queue:in({N, From}, Q)}};
handle_call({is_prime, N}, From, State = #state{ available_servers = [S|Servers] }) ->
    do_test_prime(S, From, N),
    {noreply, State#state{ available_servers = Servers }}.

handle_cast(Request, _State) ->
    logger:warning("Unexpected cast: ~p", [Request]).

handle_info({ready, S}, State = #state{ available_servers = Servers, queue = Q }) ->
    case queue:out(Q) of
        {empty, _} ->
            {noreply, State#state{ available_servers = [S|Servers]}};
        {{value, {N, From}}, NewQ} ->
            do_test_prime(S, From, N),
            {noreply, State#state{ queue = NewQ }}
    end.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

do_test_prime(S, From, N) ->
    spawn_link(fun() ->
                       IsPrime = prime_tester_server:is_prime(S, N),
                       gen_server:reply(From, IsPrime),
                       ?SERVER ! {ready, S}
               end).
