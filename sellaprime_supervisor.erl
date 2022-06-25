%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(sellaprime_supervisor).
-behaviour(supervisor). % see erl -man supervisor
-export([start/0, recover/0, start_in_shell_for_testing/0, start_link/1, init/1]).

-define(SELLAPRIME_CHILDREN,
        [{area_server,
          {area_server, start_link, []},
          permanent,
          10000,
          worker,
          [area_server]},
         {prime_server,
          {prime_server, start_link, []},
          permanent,
          10000,
          worker,
          [prime_server]},
         {primetest_sup,
          {primetest_sup, start_link, []},
          permanent,
          1000,
          supervisor,
          [primetest_sup]}]).

start() ->
    spawn(fun() ->
          supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
      end).
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).
init(Nodes) ->
    %% Install my personal error handler
     gen_event:swap_handler(alarm_handler,
                                   {alarm_handler, swap},
                   {my_alarm_handler, xyz}),
    PrimaryNode = proplists:get_value(primary, Nodes),
    BackupNode = proplists:get_value(backup, Nodes),
    SupSpec = {one_for_one, 5, 10},
    ChildSpecs = if node() =:= PrimaryNode ->
                         ?SELLAPRIME_CHILDREN;
                    node() =:= BackupNode ->
                         [{watchdog,
                           {watchdog_server, start_link, [PrimaryNode]},
                           transient,
                           10000,
                           worker,
                           [watchdog_server]}]
                 end,
    {ok, {SupSpec, ChildSpecs}}.

recover() ->
    lists:foreach(
      fun(ChildSpec) -> supervisor:start_child(?MODULE, ChildSpec) end,
      ?SELLAPRIME_CHILDREN).

%% When the supervisor is started, it calls init(Arg).
%% This function should return {ok, {SupFlags, Children}}.
%%
%% SupFlags : {supervision_strategy(), maxR(), maxT()}
%% supervision_strategy() : one_for_one | one_for_all | simple_one_for_one
%%
%% Children : [ChildStartSpecification]
%% ChildStartSpecification : {internal_name(),
%%                            {module(), function(), args()},
%%                            shutdown_time(),
%%                            child_type(),
%%                            modules()}
%% See erl -man supervisor for more details.
%%
%% A word on MaxR/MaxT:
%% Choosing a good restart frequency is difficult. The following
%% reasoning might help:
%%
%% - MaxR should be low enough that escalation is not needlessly delayed.
%%   Remember that if you have multiple levels of supervisors, MaxR will
%%   multiply; you might want to set MaxR=0 for most higher supervisors.
%% - MaxT should be low enough that unrelated restarts aren't counted as
%%   looping restart (think: how long does it take for the effects of a
%%   problem to "heal"?); if MaxT is too low, there may not be time enough
%%   for MaxR restarts.
%%
%% In general, think about what should happen if a certain process restars.
%% Some processes may be designed as "kernel processes", such that the only
%% reasonable course of action, should they crash, is to terminate the node.
%%
%% I've chosen three restarts in 10 seconds.
%%
