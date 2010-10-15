% rtctree-erl
%
% Copyright (C) 2009-2010
%     Geoffrey Biggs
%     RT-Synthesis Research Group
%     Intelligent Systems Research Institute,
%     National Institute of Advanced Industrial Science and Technology (AIST),
%     Japan
%     All rights reserved.
% Licensed under the Eclipse Public License -v 1.0 (EPL)
% http://www.opensource.org/licenses/eclipse-1.0.txt
%
% Base module of rtctree application.

-module(rtctree).

% External use exports
-export([start/0, stop/0, add_servers/1, get_root/0, get_node_by_path/1,
        reparse/0, iterate/2, pretty_print/0, connect_by_path/2]).

% Internal use exports
-export([init/0, handle_msg/2, terminate/1, call/1]).
-export([test_setup/0, test_cleanup/0, test_start_stop/0,
        test_add_bad_server/0, test_add_good_server/0, test_get_root/0,
        test_one_server/0, test_empty_server/0, test_one_dir/0,
        test_one_dir_comps/0, test_one_dir_five_comps_mgr/0,
        test_node_functions/0, test_get_nodes/0, test_reparse/0,
        test_iterate/0]).
-include("include/nodes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API


connect_by_path(Path1, Path2) ->
    {P1Path, [P1Port]} = lists:split(length(Path1) - 1, Path1),
    {P2Path, [P2Port]} = lists:split(length(Path2) - 1, Path2),
    io:format("~p:~p~n", [P1Path, P1Port]),
    io:format("~p:~p~n", [P2Path, P2Port]),
    {ok, C1} = get_node_by_path(P1Path),
    {ok, C2} = get_node_by_path(P2Path),
    P1 = dict:fetch(P1Port, component:get_ports(C1)),
    P2 = dict:fetch(P2Port, component:get_ports(C2)),
    port:connect(P1, P2, "", "", dict:new()).


start() ->
    true = register(rtctree, node:start(fun init/0, [], fun terminate/1,
            fun handle_msg/2, nil)),
    ok.


stop() ->
    call(stop).


add_servers(Servers) ->
    call({add_servers, Servers}).


get_root() ->
    whereis(rtctree).


get_node_by_path(P) when is_list(P) ->
    node:get_node_by_path(P, get_root()).


get_node_by_obj({'IOP_IOR', _, _} = O) ->
    IsObj = fun(#node{obj=Obj}) -> case Obj of O -> true; _ -> false end end,
    [N] = iterate(fun(N) -> N end, [IsObj]),
    N.


reparse() ->
    node:reparse(get_root()).


pretty_print() ->
    node:pretty_print(get_root(), 0).


iterate(Fun, Preds) ->
    node:iterate(Fun, Preds, get_root()).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal API - node process

init() ->
    ok = start_orber(),
    create_root().


handle_msg({add_servers, Servers}, Tree) ->
    {ok, parse_servers(Servers, Tree)};
handle_msg(reparse, _) ->
    ok.


terminate(Tree) ->
    node:stop_children(Tree),
    ok.


call(Msg) ->
    node:call(whereis(rtctree), self(), Msg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

create_root() ->
    #node{name="/", fullpath=["/"], type=directory, obj=nil}.


parse_servers(Servers, Root) ->
    Parsed = lists:map(fun parse_server/1, Servers),
    NewC = lists:foldl(fun({N, P}, Acc) -> dict:store(N, P, Acc) end,
        Root#node.children, Parsed),
    Root#node{children=NewC}.


parse_server(S) ->
    Pid = server:start(S, ["/"], 1),
    Name = node:get_name(Pid),
    {Name, Pid}.


start_orber() ->
    mnesia:start(),
    corba:orb_init([{domain, "OpenRTM-erlang"}, {iiop_port,2809},
            {iiop_connection_timeout, 120}]),
    orber:install([node()], [{ifr_storage_type, ram_copies}]),
    orber:start(),
    oe_SDOPackage:oe_register(),
    oe_RTC:oe_register(),
    oe_OpenRTM:oe_register(),
    oe_DataPort:oe_register(),
    oe_BasicDataType:oe_register(),
    oe_Manager:oe_register(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test_start_stop() ->
    ok = start(),
    ok = stop().


test_add_bad_server() ->
    ok = start(),
    error = try add_servers(["blurgle"])
    catch
        _:_ -> error
    end,
    ok = stop().


test_add_good_server() ->
    start(),
    ok = add_servers(["localhost"]),
    stop(),
    ok.


test_get_root() ->
    test_setup(),
    get_root(),
    test_cleanup(),
    ok.


test_one_server() ->
    test_setup(),
    1 = length(dict:fetch_keys(node:get_children(get_root()))),
    test_cleanup(),
    ok.


test_empty_server() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    0 = length(dict:fetch_keys(node:get_children(S))),
    test_cleanup(),
    ok.


test_one_dir() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    1 = length(dict:fetch_keys(node:get_children(S))),
    test_cleanup(),
    ok.


test_one_dir_comps() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    D = dict:fetch("blurgle.host", node:get_children(S)),
    8 = length(dict:fetch_keys(node:get_children(D))),
    test_cleanup(),
    ok.


test_one_dir_five_comps_mgr() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    D = dict:fetch("stellvia.host_cxt", node:get_children(S)),
    1 = length(dict:fetch_keys(node:get_children(D))),
    M = dict:fetch("manager.mgr", node:get_children(D)),
    0 = length(dict:fetch_keys(node:get_children(M))),
    test_cleanup(),
    ok.

test_node_functions() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    D1 = dict:fetch("blurgle.host", node:get_children(S)),
    D2 = dict:fetch("stellvia.host_cxt", node:get_children(S)),
    C1 = dict:fetch("ConsoleIn0.rtc", node:get_children(D1)),
    C2 = dict:fetch("ConsoleOut0.rtc", node:get_children(D1)),
    %C3 = dict:fetch("Sensor0.rtc", node:get_children(D1)),
    %C4 = dict:fetch("Controller0.rtc", node:get_children(D1)),
    %C5 = dict:fetch("Motor0.rtc", node:get_children(D1)),
    M = dict:fetch("manager.mgr", node:get_children(D2)),
    test_name(S, D1, M, C1),
    test_full_path(S, D1, M, C1),
    test_children(S, D1, D2, M, C1),
    test_depth(S, D1, D2, M, C1),
    test_type(S, D1, D2, M, C1),
    test_corba_obj(S, D1, D2, M, C1),
    test_is_child_by_pid(S, D1, D2, M, C1, C2),
    test_is_child_by_path(S, D1, D2, M, C1, C2),
    test_is_parent_by_pid(S, D1, D2, M, C1, C2),
    test_is_parent_by_path(S, D1, D2, M, C1, C2),
    test_is_descendant_by_pid(S, D1, D2, M, C1, C2),
    test_is_descendant_by_path(S, D1, D2, M, C1, C2),
    test_is_ancestor_by_pid(S, D1, D2, M, C1, C2),
    test_is_ancestor_by_path(S, D1, D2, M, C1, C2),
    test_parent_name(S, D1, D2, M, C1),
    test_root(S, D1, D2, M, C1),
    test_get_ns(get_root(), S, D1, D2, C1, M),
    test_is_comp(get_root(), S, D1, D2, C1, M),
    test_is_dir(get_root(), S, D1, D2, C1, M),
    test_is_mgr(get_root(), S, D1, D2, C1, M),
    test_is_ns(get_root(), S, D1, D2, C1, M),
    test_is_unknown(get_root(), S, D1, D2, C1, M),
    test_cleanup(),
    ok.


test_get_nodes() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    D1 = dict:fetch("blurgle.host", node:get_children(S)),
    D2 = dict:fetch("stellvia.host_cxt", node:get_children(S)),
    C1 = dict:fetch("ConsoleIn0.rtc", node:get_children(D1)),
    C2 = dict:fetch("ConsoleOut0.rtc", node:get_children(D1)),
    M = dict:fetch("manager.mgr", node:get_children(D2)),
    test_get_node_from_root(S, D1, D2, C1, C2, M),
    test_get_node_relative(S, D1, D2, C1, C2, M),
    test_get_node_by_obj(node:get_corba_obj(C1), C1),
    test_cleanup(),
    ok.


test_reparse() ->
    test_setup(),
    S = dict:fetch("localhost", node:get_children(get_root())),
    D1 = dict:fetch("blurgle.host", node:get_children(S)),
    D2 = dict:fetch("stellvia.host_cxt", node:get_children(S)),
    C1 = dict:fetch("ConsoleIn0.rtc", node:get_children(D1)),
    M = dict:fetch("manager.mgr", node:get_children(D2)),
    test_reparse(S, D1, C1, M),
    test_cleanup(),
    ok.


test_iterate() ->
    test_setup(),
    IsComp = fun(#node{type=component}) -> true; (_) -> false end,
    GetName = fun(#node{name=N}) -> N end,
    Res = iterate(GetName, [IsComp]),
    true = lists:member("ConsoleIn0.rtc", Res),
    true = lists:member("ConsoleOut0.rtc", Res),
    true = lists:member("Sensor0.rtc", Res),
    true = lists:member("Controller0.rtc", Res),
    true = lists:member("Motor0.rtc", Res),
    true = lists:member("ConfigSample0.rtc", Res),
    true = lists:member("MyServiceProvider0.rtc", Res),
    true = lists:member("MyServiceConsumer0.rtc", Res),
    test_cleanup(),
    ok.


test_setup() ->
    start(),
    add_servers(["localhost"]).


test_cleanup() ->
    stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

test_name(S, D1, M, C1) ->
    "/" = node:get_name(get_root()),
    "localhost" = node:get_name(S),
    "blurgle.host" = node:get_name(D1),
    "ConsoleIn0.rtc" = node:get_name(C1),
    "manager.mgr" = node:get_name(M),
    ok.


test_full_path(S, D1, M, C1) ->
    ["/"] = node:get_full_path(get_root()),
    ["/", "localhost"] = node:get_full_path(S),
    ["/", "localhost", "blurgle.host"] = node:get_full_path(D1),
    ["/", "localhost", "stellvia.host_cxt", "manager.mgr"] = node:get_full_path(M),
    ["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"] = node:get_full_path(C1),
    ok.


test_children(S, D1, D2, M, C1) ->
    1 = length(dict:fetch_keys(node:get_children(get_root()))),
    2 = length(dict:fetch_keys(node:get_children(S))),
    8 = length(dict:fetch_keys(node:get_children(D1))),
    1 = length(dict:fetch_keys(node:get_children(D2))),
    1 = length(dict:fetch_keys(node:get_children(M))),
    0 = length(dict:fetch_keys(node:get_children(C1))),
    ok.


test_depth(S, D1, D2, M, C1) ->
    0 = node:get_depth(get_root()),
    1 = node:get_depth(S),
    2 = node:get_depth(D1),
    2 = node:get_depth(D2),
    3 = node:get_depth(C1),
    3 = node:get_depth(M),
    ok.


test_type(S, D1, D2, M, C1) ->
    directory = node:get_type(get_root()),
    nameserver = node:get_type(S),
    directory = node:get_type(D1),
    directory = node:get_type(D2),
    manager = node:get_type(M),
    component = node:get_type(C1),
    ok.


test_corba_obj(S, D1, D2, M, C1) ->
    nil = node:get_corba_obj(get_root()),
    {'IOP_IOR', _, _} = node:get_corba_obj(S),
    {'IOP_IOR', _, _} = node:get_corba_obj(D1),
    {'IOP_IOR', _, _} = node:get_corba_obj(D2),
    {'IOP_IOR', _, _} = node:get_corba_obj(C1),
    {'IOP_IOR', _, _} = node:get_corba_obj(M),
    ok.


test_get_node_by_obj(O, N) ->
    io:format("Looking for ~p~n", [N]),
    N = get_node_by_obj(O),
    ok.


test_is_child_by_pid(S, D1, D2, M, C1, C2) ->
    io:format("root->S~n"),
    true = node:is_child(S, get_root()),
    io:format("D1->S~n"),
    true = node:is_child(D1, S),
    io:format("D2->S~n"),
    true = node:is_child(D2, S),
    io:format("C1->root~n"),
    false = node:is_child(C1, get_root()),
    io:format("C1->S~n"),
    false = node:is_child(C1, S),
    io:format("C1->D1~n"),
    true = node:is_child(C1, D1),
    io:format("C1->D2~n"),
    false = node:is_child(C1, D2),
    io:format("C1->C2~n"),
    false = node:is_child(C1, C2),
    io:format("M->root~n"),
    false = node:is_child(M, get_root()),
    io:format("M->S~n"),
    false = node:is_child(M, S),
    io:format("M->D2~n"),
    true = node:is_child(M, D2),
    io:format("M->D1~n"),
    false = node:is_child(M, D1),
    io:format("M->C1~n"),
    false = node:is_child(M, C1),
    io:format("S->D1~n"),
    false = node:is_child(S, D1),
    io:format("root->D1~n"),
    false = node:is_child(get_root(), D1),
    io:format("S->C1~n"),
    false = node:is_child(S, C1),
    io:format("S->M~n"),
    false = node:is_child(S, M),
    ok.


test_is_child_by_path(S, D1, D2, M, C1, C2) ->
    io:format("[localhost]->root~n"),
    true = node:is_child(["localhost"], get_root()),
    io:format("[blurgle.host]->S~n"),
    true = node:is_child(["blurgle.host"], S),
    io:format("[stellvia.host_cxt]->S~n"),
    true = node:is_child(["stellvia.host_cxt"], S),
    io:format("[localhost, blurgle.host, ConsoleIn0.rtc]->root~n"),
    false = node:is_child(["localhost", "blurgle.host", "ConsoleIn0.rtc"],
        get_root()),
    io:format("[blurgle.host, ConsoleIn0.rtc]->S~n"),
    false = node:is_child(["blurgle.host", "ConsoleIn0.rtc"], S),
    io:format("[ConsoleIn0.rtc]->D1~n"),
    true = node:is_child(["ConsoleIn0.rtc"], D1),
    io:format("[ConsoleIn0.rtc]->D2~n"),
    false = node:is_child(["ConsoleIn0.rtc"], D2),
    io:format("[ConsoleIn0.rtc]->C2~n"),
    false = node:is_child(["ConsoleIn0.rtc"], C2),
    io:format("[localhost, stellvia.host_cxt, manager.mgr]->root~n"),
    false = node:is_child(["localhost", "stellvia.host_cxt", "manager.mgr"],
        get_root()),
    io:format("[stellvia.host_cxt, manager.mgr]->S~n"),
    false = node:is_child(["stellvia.host_cxt", "manager.mgr"], S),
    io:format("[manager.mgr]->D2~n"),
    true = node:is_child(["manager.mgr"], D2),
    io:format("[manager.mgr]->D1~n"),
    false = node:is_child(["manager.mgr"], D1),
    io:format("[stellvia.host_cxt, manager.mgr]->C1~n"),
    false = node:is_child(["stellvia.host_cxt", "manager.mgr"], C1),
    io:format("[localhost]->D1~n"),
    false = node:is_child(["localhost"], D1),
    io:format("[/]->D1~n"),
    false = node:is_child(["/"], D1),
    io:format("[localhost]->C1~n"),
    false = node:is_child(["localhost"], C1),
    io:format("[stellvia.host_cxt, localhost]->M~n"),
    false = node:is_child(["stellvia.host_cxt", "localhost"], M),
    ok.



test_is_parent_by_pid(S, D1, D2, M, C1, C2) ->
    io:format("root->S~n"),
    true = node:is_parent(get_root(), S),
    io:format("S->D1~n"),
    true = node:is_parent(S, D1),
    io:format("S->D2~n"),
    true = node:is_parent(S, D2),
    io:format("root->C1~n"),
    false = node:is_parent(get_root(), C1),
    io:format("S->C1~n"),
    false = node:is_parent(S, C1),
    io:format("D1->C1~n"),
    true = node:is_parent(D1, C1),
    io:format("D2->C1~n"),
    false = node:is_parent(D2, C1),
    io:format("C2->C1~n"),
    false = node:is_parent(C2, C1),
    io:format("root->M~n"),
    false = node:is_parent(get_root(), M),
    io:format("S->M~n"),
    false = node:is_parent(S, M),
    io:format("D2->M~n"),
    true = node:is_parent(D2, M),
    io:format("D1->M~n"),
    false = node:is_parent(D1, M),
    io:format("C1->M~n"),
    false = node:is_parent(C1, M),
    io:format("D1->S~n"),
    false = node:is_parent(D1, S),
    io:format("D1->root~n"),
    false = node:is_parent(D1, get_root()),
    io:format("C1->S~n"),
    false = node:is_parent(C1, S),
    io:format("M->S~n"),
    false = node:is_parent(M, S),
    ok.


test_is_parent_by_path(S, D1, D2, M, C1, C2) ->
    io:format("/->S~n"),
    true = node:is_parent(["/"], S),
    io:format("/localhost->D1~n"),
    true = node:is_parent(["/", "localhost"], D1),
    io:format("/localhost->D2~n"),
    true = node:is_parent(["/", "localhost"], D2),
    io:format("/->C1~n"),
    false = node:is_parent(["/"], C1),
    io:format("/localhost->C1~n"),
    false = node:is_parent(["/", "localhost"], C1),
    io:format("/localhost/blurgle.host->C1~n"),
    true = node:is_parent(["/", "localhost", "blurgle.host"], C1),
    io:format("/localhost/stellvia.host_cxt->C1~n"),
    false = node:is_parent(["/", "localhost", "stellvia.host_cxt"], C1),
    io:format("/localhost/blurgle.host/ConsoleIn0.rtc->C2~n"),
    false = node:is_parent(["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"], C2),
    io:format("/->M~n"),
    false = node:is_parent(["/"], M),
    io:format("/localhost->M~n"),
    false = node:is_parent(["/", "localhost"], M),
    io:format("/localhost/stellvia.host_cxt->M~n"),
    true = node:is_parent(["/", "localhost", "stellvia.host_cxt"], M),
    io:format("/localhost/blurgle.host->M~n"),
    false = node:is_parent(["/", "localhost", "blurgle.host"], M),
    io:format("/localhost/blurgle.host_cxt/ConsoleIn0.rtc->M~n"),
    false = node:is_parent(["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"], M),
    io:format("/localhost/blurgle.host_cxt->D1~n"),
    false = node:is_parent(["/", "localhost", "blurgle.host_cxt"], D1),
    io:format("/localhost/blurgle.host_cxt->root~n"),
    false = node:is_parent(["/", "localhost", "blurgle.host_cxt"], get_root()),
    io:format("/localhost/blurgle.host_cxt/ConsoleIn0.rtc->S~n"),
    false = node:is_parent(["/", "localhost", "blurgle.host_cxt", "ConsoleIn0.rtc"], S),
    io:format("/localhost/stellvia.host_cxt/manager.mgr->S~n"),
    false = node:is_parent(["/", "localhost", "stellvia.host_cxt",
            "manager.mgr"], S),
    ok.



test_is_descendant_by_pid(S, D1, D2, M, C1, C2) ->
    io:format("root->S~n"),
    true = node:is_descendant(S, get_root()),
    io:format("D1->S~n"),
    true = node:is_descendant(D1, S),
    io:format("D2->S~n"),
    true = node:is_descendant(D2, S),
    io:format("C1->root~n"),
    true = node:is_descendant(C1, get_root()),
    io:format("C1->S~n"),
    true = node:is_descendant(C1, S),
    io:format("C1->D1~n"),
    true = node:is_descendant(C1, D1),
    io:format("C1->D2~n"),
    false = node:is_descendant(C1, D2),
    io:format("C1->C2~n"),
    false = node:is_descendant(C1, C2),
    io:format("M->root~n"),
    true = node:is_descendant(M, get_root()),
    io:format("M->S~n"),
    true = node:is_descendant(M, S),
    io:format("M->D2~n"),
    true = node:is_descendant(M, D2),
    io:format("M->D1~n"),
    false = node:is_descendant(M, D1),
    io:format("M->C1~n"),
    false = node:is_descendant(M, C1),
    io:format("S->D1~n"),
    false = node:is_descendant(S, D1),
    io:format("root->D1~n"),
    false = node:is_descendant(get_root(), D1),
    io:format("S->C1~n"),
    false = node:is_descendant(S, C1),
    io:format("S->M~n"),
    false = node:is_descendant(S, M),
    ok.


test_is_descendant_by_path(S, D1, D2, M, C1, C2) ->
    io:format("[localhost]->root~n"),
    true = node:is_descendant(["localhost"], get_root()),
    io:format("[blurgle.host]->S~n"),
    true = node:is_descendant(["blurgle.host"], S),
    io:format("[stellvia.host_cxt]->S~n"),
    true = node:is_descendant(["stellvia.host_cxt"], S),
    io:format("[localhost, blurgle.host, ConsoleIn0.rtc]->root~n"),
    true = node:is_descendant(["localhost", "blurgle.host", "ConsoleIn0.rtc"],
        get_root()),
    io:format("[blurgle.host, ConsoleIn0.rtc]->S~n"),
    true = node:is_descendant(["blurgle.host", "ConsoleIn0.rtc"], S),
    io:format("[ConsoleIn0.rtc]->D1~n"),
    true = node:is_descendant(["ConsoleIn0.rtc"], D1),
    io:format("[ConsoleIn0.rtc]->D2~n"),
    false = node:is_descendant(["ConsoleIn0.rtc"], D2),
    io:format("[ConsoleIn0.rtc]->C2~n"),
    false = node:is_descendant(["ConsoleIn0.rtc"], C2),
    io:format("[localhost, stellvia.host_cxt, manager.mgr]->root~n"),
    true = node:is_descendant(["localhost", "stellvia.host_cxt", "manager.mgr"],
        get_root()),
    io:format("[stellvia.host_cxt, manager.mgr]->S~n"),
    true = node:is_descendant(["stellvia.host_cxt", "manager.mgr"], S),
    io:format("[manager.mgr]->D2~n"),
    true = node:is_descendant(["manager.mgr"], D2),
    io:format("[manager.mgr]->D1~n"),
    false = node:is_descendant(["manager.mgr"], D1),
    io:format("[stellvia.host_cxt, manager.mgr]->C1~n"),
    false = node:is_descendant(["stellvia.host_cxt", "manager.mgr"], C1),
    io:format("[localhost]->D1~n"),
    false = node:is_descendant(["localhost"], D1),
    io:format("[/]->D1~n"),
    false = node:is_descendant(["/"], D1),
    io:format("[localhost]->C1~n"),
    false = node:is_descendant(["localhost"], C1),
    io:format("[stellvia.host_cxt, localhost]->M~n"),
    false = node:is_descendant(["stellvia.host_cxt", "localhost"], M),
    ok.



test_is_ancestor_by_pid(S, D1, D2, M, C1, C2) ->
    io:format("root->S~n"),
    true = node:is_ancestor(get_root(), S),
    io:format("S->D1~n"),
    true = node:is_ancestor(S, D1),
    io:format("S->D2~n"),
    true = node:is_ancestor(S, D2),
    io:format("root->C1~n"),
    true = node:is_ancestor(get_root(), C1),
    io:format("S->C1~n"),
    true = node:is_ancestor(S, C1),
    io:format("D1->C1~n"),
    true = node:is_ancestor(D1, C1),
    io:format("D2->C1~n"),
    false = node:is_ancestor(D2, C1),
    io:format("C2->C1~n"),
    false = node:is_ancestor(C2, C1),
    io:format("root->M~n"),
    true = node:is_ancestor(get_root(), M),
    io:format("S->M~n"),
    true = node:is_ancestor(S, M),
    io:format("D2->M~n"),
    true = node:is_ancestor(D2, M),
    io:format("D1->M~n"),
    false = node:is_ancestor(D1, M),
    io:format("C1->M~n"),
    false = node:is_ancestor(C1, M),
    io:format("D1->S~n"),
    false = node:is_ancestor(D1, S),
    io:format("D1->root~n"),
    false = node:is_ancestor(D1, get_root()),
    io:format("C1->S~n"),
    false = node:is_ancestor(C1, S),
    io:format("M->S~n"),
    false = node:is_ancestor(M, S),
    ok.


test_is_ancestor_by_path(S, D1, D2, M, C1, C2) ->
    io:format("/->S~n"),
    true = node:is_ancestor(["/"], S),
    io:format("/localhost->D1~n"),
    true = node:is_ancestor(["/", "localhost"], D1),
    io:format("/localhost->D2~n"),
    true = node:is_ancestor(["/", "localhost"], D2),
    io:format("/->C1~n"),
    true = node:is_ancestor(["/"], C1),
    io:format("/localhost->C1~n"),
    true = node:is_ancestor(["/", "localhost"], C1),
    io:format("/localhost/blurgle.host->C1~n"),
    true = node:is_ancestor(["/", "localhost", "blurgle.host"], C1),
    io:format("/localhost/stellvia.host_cxt->C1~n"),
    false = node:is_ancestor(["/", "localhost", "stellvia.host_cxt"], C1),
    io:format("/localhost/blurgle.host/ConsoleIn0.rtc->C2~n"),
    false = node:is_ancestor(["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"], C2),
    io:format("/->M~n"),
    true = node:is_ancestor(["/"], M),
    io:format("/localhost->M~n"),
    true = node:is_ancestor(["/", "localhost"], M),
    io:format("/localhost/stellvia.host_cxt->M~n"),
    true = node:is_ancestor(["/", "localhost", "stellvia.host_cxt"], M),
    io:format("/localhost/blurgle.host->M~n"),
    false = node:is_ancestor(["/", "localhost", "blurgle.host"], M),
    io:format("/localhost/blurgle.host_cxt/ConsoleIn0.rtc->M~n"),
    false = node:is_ancestor(["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"], M),
    io:format("/localhost/blurgle.host_cxt->D1~n"),
    false = node:is_ancestor(["/", "localhost", "blurgle.host_cxt"], D1),
    io:format("/localhost/blurgle.host_cxt->root~n"),
    false = node:is_ancestor(["/", "localhost", "blurgle.host_cxt"], get_root()),
    io:format("/localhost/blurgle.host_cxt/ConsoleIn0.rtc->S~n"),
    false = node:is_ancestor(["/", "localhost", "blurgle.host_cxt", "ConsoleIn0.rtc"], S),
    io:format("/localhost/stellvia.host_cxt/manager.mgr->S~n"),
    false = node:is_ancestor(["/", "localhost", "stellvia.host_cxt",
            "manager.mgr"], S),
    ok.


test_parent_name(S, D1, D2, M, C1) ->
    "" = node:get_parent_name(get_root()),
    "/" = node:get_parent_name(S),
    "localhost" = node:get_parent_name(D1),
    "localhost" = node:get_parent_name(D2),
    "blurgle.host" = node:get_parent_name(C1),
    "stellvia.host_cxt" = node:get_parent_name(M),
    ok.


test_root(S, D1, D2, M, C1) ->
    R = get_root(),
    {ok, R} = node:get_root(S),
    {ok, R} = node:get_root(D1),
    {ok, R} = node:get_root(D2),
    {ok, R} = node:get_root(C1),
    {ok, R} = node:get_root(M),
    ok.


test_get_ns(R, S, D1, D2, C1, M) ->
    {error, nonode} = node:get_nameserver(R),
    {ok, SP} = node:get_nameserver(S),
    true = is_pid(SP),
    {ok, D1P} = node:get_nameserver(D1),
    true = is_pid(D1P),
    {ok, D2P} = node:get_nameserver(D2),
    true = is_pid(D2P),
    {ok, C1P} = node:get_nameserver(C1),
    true = is_pid(C1P),
    {ok, MP} = node:get_nameserver(M),
    true = is_pid(MP),
    "localhost" = node:get_name(SP),
    "localhost" = node:get_name(D1P),
    "localhost" = node:get_name(D2P),
    "localhost" = node:get_name(C1P),
    "localhost" = node:get_name(MP),
    ok.


test_is_comp(R, S, D1, D2, C1, M) ->
    false = node:is_component(R),
    false = node:is_component(S),
    false = node:is_component(D1),
    false = node:is_component(D2),
    true = node:is_component(C1),
    false = node:is_component(M),
    ok.


test_is_dir(R, S, D1, D2, C1, M) ->
    true = node:is_directory(R),
    false = node:is_directory(S),
    true = node:is_directory(D1),
    true = node:is_directory(D2),
    false = node:is_directory(C1),
    true = node:is_directory(M),
    ok.


test_is_mgr(R, S, D1, D2, C1, M) ->
    false = node:is_manager(R),
    false = node:is_manager(S),
    false = node:is_manager(D1),
    false = node:is_manager(D2),
    false = node:is_manager(C1),
    true = node:is_manager(M),
    ok.


test_is_ns(R, S, D1, D2, C1, M) ->
    false = node:is_nameserver(R),
    true = node:is_nameserver(S),
    false = node:is_nameserver(D1),
    false = node:is_nameserver(D2),
    false = node:is_nameserver(C1),
    false = node:is_nameserver(M),
    ok.


test_is_unknown(R, S, D1, D2, C1, M) ->
    false = node:is_unknown(R),
    false = node:is_unknown(S),
    false = node:is_unknown(D1),
    false = node:is_unknown(D2),
    false = node:is_unknown(C1),
    false = node:is_unknown(M),
    ok.


test_get_node_from_root(S, D1, D2, C1, C2, M) ->
    R = get_root(),
    io:format("/~n"),
    {ok, R} = get_node_by_path(["/"]),
    io:format("/localhost~n"),
    {ok, S} = get_node_by_path(["/", "localhost"]),
    io:format("/localhost/blurgle.host~n"),
    {ok, D1} = get_node_by_path(["/", "localhost", "blurgle.host"]),
    io:format("/localhost/stellvia.host_cxt~n"),
    {ok, D2} = get_node_by_path(["/", "localhost", "stellvia.host_cxt"]),
    io:format("/localhost/blurgle.host/ConsoleIn0.rtc~n"),
    {ok, C1} = get_node_by_path(["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"]),
    io:format("/localhost/blurgle.host/ConsoleOut0.rtc~n"),
    {ok, C2} = get_node_by_path(["/", "localhost", "blurgle.host", "ConsoleOut0.rtc"]),
    io:format("/localhost/stellvia.host_cxt/manager.mgr~n"),
    {ok, M} = get_node_by_path(["/", "localhost", "stellvia.host_cxt",
            "manager.mgr"]),
    ok.


test_get_node_relative(S, D1, D2, C1, C2, M) ->
    io:format("localhost->root~n"),
    {error, nonode} = node:get_node_by_path(["localhost"], get_root()),
    io:format("/localhost->root~n"),
    {ok, S} = node:get_node_by_path(["/", "localhost"], get_root()),
    io:format("localhost/blurgle.host->root~n"),
    {error, nonode} = node:get_node_by_path(["localhost", "blurgle.host"], get_root()),
    io:format("/localhost/blurgle.host->root~n"),
    {ok, D1} = node:get_node_by_path(["/", "localhost", "blurgle.host"], get_root()),
    io:format("blurgle.host->S~n"),
    {error, nonode} = node:get_node_by_path(["blurgle.host"], S),
    io:format("localhost/blurgle.host->S~n"),
    {ok, D1} = node:get_node_by_path(["localhost", "blurgle.host"], S),
    io:format("localhost/stellvia.host_cxt->root~n"),
    {error, nonode} = node:get_node_by_path(["localhost", "stellvia.host_cxt"], get_root()),
    io:format("/localhost/stellvia.host_cxt->root~n"),
    {ok, D2} = node:get_node_by_path(["/", "localhost", "stellvia.host_cxt"], get_root()),
    io:format("stellvia.host_cxt->S~n"),
    {error, nonode} = node:get_node_by_path(["stellvia.host_cxt"], S),
    io:format("localhost/stellvia.host_cxt->S~n"),
    {ok, D2} = node:get_node_by_path(["localhost", "stellvia.host_cxt"], S),
    io:format("localhost/blurgle.host/ConsoleIn0.rtc->root~n"),
    {error, nonode} = node:get_node_by_path(["localhost", "blurgle.host", "ConsoleIn0.rtc"], get_root()),
    io:format("/localhost/blurgle.host/ConsoleIn0.rtc->root~n"),
    {ok, C1} = node:get_node_by_path(["/", "localhost", "blurgle.host", "ConsoleIn0.rtc"], get_root()),
    io:format("localhost/blurgle.host/ConsoleIn0.rtc->S~n"),
    {ok, C1} = node:get_node_by_path(["localhost", "blurgle.host", "ConsoleIn0.rtc"], S),
    io:format("blurgle.host/ConsoleIn0.rtc->S~n"),
    {error, nonode} = node:get_node_by_path(["blurgle.host", "ConsoleIn0.rtc"], S),
    io:format("blurgle.host/ConsoleIn0.rtc->D1~n"),
    {ok, C1} = node:get_node_by_path(["blurgle.host", "ConsoleIn0.rtc"], D1),
    io:format("ConsoleIn0.rtc->D1~n"),
    {error, nonode} = node:get_node_by_path(["ConsoleIn0.rtc"], D1),
    io:format("localhost/blurgle.host/ConsoleOut0.rtc->root~n"),
    {error, nonode} = node:get_node_by_path(["localhost", "blurgle.host", "ConsoleOut0.rtc"], get_root()),
    io:format("/localhost/blurgle.host/ConsoleOut0.rtc->root~n"),
    {ok, C2} = node:get_node_by_path(["/", "localhost", "blurgle.host", "ConsoleOut0.rtc"], get_root()),
    io:format("localhost/blurgle.host/ConsoleOut0.rtc->S~n"),
    {ok, C2} = node:get_node_by_path(["localhost", "blurgle.host", "ConsoleOut0.rtc"], S),
    io:format("blurgle.host/ConsoleOut0.rtc->S~n"),
    {error, nonode} = node:get_node_by_path(["blurgle.host", "ConsoleOut0.rtc"], S),
    io:format("blurgle.host/ConsoleOut0.rtc->D1~n"),
    {ok, C2} = node:get_node_by_path(["blurgle.host", "ConsoleOut0.rtc"], D1),
    io:format("ConsoleOut0.rtc->D1~n"),
    {error, nonode} = node:get_node_by_path(["ConsoleOut0.rtc"], D1),
    io:format("localhost/stellvia.host_cxt/manager.mgr->root~n"),
    {error, nonode} = node:get_node_by_path(["localhost", "stellvia.host_cxt", "manager.mgr"], get_root()),
    io:format("/localhost/stellvia.host_cxt/manager.mgr->root~n"),
    {ok, M} = node:get_node_by_path(["/", "localhost", "stellvia.host_cxt", "manager.mgr"], get_root()),
    io:format("localhost/stellvia.host_cxt/manager.mgr->S~n"),
    {ok, M} = node:get_node_by_path(["localhost", "stellvia.host_cxt", "manager.mgr"], S),
    io:format("stellvia.host_cxt/manager.mgr->S~n"),
    {error, nonode} = node:get_node_by_path(["stellvia.host_cxt", "manager.mgr"], S),
    io:format("stellvia.host_cxt/manager.mgr->D1~n"),
    {ok, M} = node:get_node_by_path(["stellvia.host_cxt", "manager.mgr"], D2),
    io:format("manager.mgr.g->D1~n"),
    {error, nonode} = node:get_node_by_path(["manager.mgr.g"], D1),
    io:format("ConsoleIn0.rtc->root~n"),
    {error, nonode} = node:get_node_by_path(["ConsoleIn0.rtc"], get_root()),
    io:format("ConsoleIn0.rtc->S~n"),
    {error, nonode} = node:get_node_by_path(["ConsoleIn0.rtc"], S),
    io:format("manager.mgr->D1~n"),
    {error, nonode} = node:get_node_by_path(["manager.mgr"], D1),
    io:format("blurgle.host->D2~n"),
    {error, nonode} = node:get_node_by_path(["blurgle.host"], D2),
    io:format("ConsoleIn0.rtc->D2~n"),
    {error, nonode} = node:get_node_by_path(["ConsoleIn0.rtc"], D2),
    io:format("localhost->D2~n"),
    {error, nonode} = node:get_node_by_path(["localhost"], D2),
    ok.


test_reparse(_, D, _, _) ->
    DL = length(dict:fetch_keys(node:get_children(D))),
    ok = node:reparse(D),
    DL = length(dict:fetch_keys(node:get_children(D))),
    ok.

