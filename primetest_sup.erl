%%%-------------------------------------------------------------------
%%% @author Will Vining <wfv@vining.dev>
%%% @copyright (C) 2022, Will Vining
%%% @doc
%%%
%%% @end
%%% Created : 18 Jun 2022 by Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------
-module(primetest_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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

    SupFlags = #{strategy => rest_for_one,
                 intensity => 1,
                 period => 5},

    {ok, {SupFlags,
          [#{id => load_balancer,
             start => {isaprime, start_link, []},
             restart => permanent,
             shutdown => 1000,
             type => worker,
             modules => [isaprime]},
           #{id => tester_sup,
             start => {prime_tester_sup, start_link, []},
             restart => permanent,
             shutdown => 1000,
             type => supervisor,
             modules => [prime_tester_sup]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
