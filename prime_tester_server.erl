 %%%-------------------------------------------------------------------
%%% @author Will Vining <wfvining@alazani>
%%% @copyright (C) 2022, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2022 by Will Vining <wfvining@alazani>
%%%-------------------------------------------------------------------
-module(prime_tester_server).

-behaviour(gen_server).

%% API
-export([start_link/0, is_prime/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state, {queue = queue:new(),
                worker :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec is_prime(S :: pid(), JobId :: pos_integer()) -> ok.
is_prime(S, JobId) ->
    gen_server:cast(S, {is_prime, JobId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([]) ->
    isaprime:register_worker(),
    Self = self(),
    {ok, #state{worker = spawn_link(fun() -> worker(Self) end)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call(get_work, _From, State=#state{queue=Q}) ->
    case queue:peek(Q) of
        empty ->
            {reply, no_work, State};
        {value, V} ->
            {reply, {work, V}, State}
    end;
handle_call(Request, _From, State) ->
    logger:debug("Unexpected request ~p~n", [Request]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast(work_done, State=#state{queue = Q}) ->
    {{value, Request}, NewQ} = queue:out(Q),
    isaprime:work_done(Request),
    {noreply, State#state{queue = NewQ}};
handle_cast({is_prime, Request}, State=#state{queue=Q, worker=W}) ->
    case queue:is_empty(Q) of
        true -> alert_worker(W);
        false -> ok
    end,
    {noreply, State#state{queue=queue:in(Request, Q)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(normal, #state{worker=W}) ->
    W ! stop;
terminate(_, _) ->
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_work(Server) ->
    gen_server:call(Server, get_work).

work_done(Server) ->
    gen_server:cast(Server, work_done).

alert_worker(W) ->
    W ! work.

worker(Server) ->
    receive
        work ->
            do_work(Server),
            worker(Server);
        stop ->
            ok
    end.

do_work(Server) ->
    case get_work(Server) of
        {work, JobId} ->
            {N, From} = isaprime:get_job(JobId),
            IsPrime = lib_primes:is_prime(N),
            isaprime:respond(From, IsPrime),
            work_done(Server),
            do_work(Server);
        no_work ->
            waiting
    end.
