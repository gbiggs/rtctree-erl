% Callbacks placed on components and ports.
% Callbacks are placed in equivalent places to built-in OpenRTM callbacks.
% When a callback is registered, the value of the point of interest will be
% updated at the given rate, and a check made on whether it has changed. When
% it changes, the callback will be called.

-module(test).
-export([run/0, cb/4]).
-include("/home/geoff/research/src/local/rtctree-erl/include/nodes.hrl").

run() ->
    ok = rtctree:start(),
    ok = rtctree:add_servers(["localhost"]),
    io:format("======~n~p~n", [rtctree:pretty_print()]),
    {ok, C} = rtctree:get_node_by_path(["/", "localhost", "me.host_cxt",
            "ConsoleOut0.rtc"]),
    io:format("Name is ~p~n", [node:get_name(C)]),
    setup_cb(C),
    io:format("Added CB~n"),
    sleep(15000),
    remove_cb(C),
    io:format("Removed CB~n"),
    sleep(3000).


cb(New, Old, Node, FP) ->
    io:format("State of ~p changed from ~p to ~p~n", [FP, Old, New]).


setup_cb(C) ->
    ok = component:add_cb(C, fun ?MODULE:cb/4, state, 1000).


remove_cb(C) ->
    ok = component:remove_cbs(C, []).


sleep(T) ->
    receive
    after
        T ->
            ok
    end.

