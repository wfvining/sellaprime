%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(sellaprime_app).
-behaviour(application).
-export([start/2, stop/1]).
start(_Type, StartArgs) ->
    PrimaryNode = proplists:get_value(primary, StartArgs),
    BackupNode = proplists:get_value(backup, StartArgs),
    if
        PrimaryNode =:= node() ->
            rpc:call(BackupNode, application, start, [sellaprime]),
            sellaprime_supervisor:start_link(StartArgs);
        true ->
            sellaprime_supervisor:start_link(StartArgs)
    end.
stop(_State) ->
    ok.


