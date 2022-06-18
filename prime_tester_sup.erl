%%%-------------------------------------------------------------------
%%% @author Will Vining <wfv@vining.dev>
%%% @copyright (C) 2022, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 18 Jun 2022 by Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------
-module(prime_tester_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(NUM_WORKERS, 32).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 5},

    {ok, {SupFlags,
          [#{id => list_to_atom(lists:flatten(io_lib:format("tester~p", [I]))),
             start => {prime_tester_server, start_link, []},
             restart => permanent,
             shutdown => 1000,
             type => worker,
             modules => [prime_tester_server]}
           || I <- lists:seq(1, ?NUM_WORKERS)]}}.

%% TODO rewrite prime_tester_server so it registers with the load balancer when it starts.
%% TODO implement tester failure/restart logic

%%%===================================================================
%%% Internal functions
%%%===================================================================
