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
% Generic node functionality.

-module(node).

% External use exports
-export([reparse/1, pretty_print/2, get_name/1, get_full_path/1,
        get_children/1, get_depth/1, get_type/1, get_corba_obj/1, is_child/2,
        is_parent/2, is_descendant/2, is_ancestor/2, get_parent_name/1,
        get_root/1, is_component/1, is_directory/1, is_manager/1,
        is_nameserver/1, is_unknown/1, get_nameserver/1, get_node_by_path/2]).

% Internal use exports
-export([start/5, stop/1, call/3, send/2, reply/2, stop_children/1,
        init_node/5, loop/1, iterate/3]).
-include("include/nodes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - node process

start(Init, Args, Cleanup, Handler, Parent) ->
    spawn_link(?MODULE, init_node, [Init, Args, Cleanup, Handler, Parent]).


stop(To) ->
    call(To, self(), stop).


call(To, Sender, Msg) ->
    To ! {request, Sender, Msg},
    receive
        {response, To, Reply} ->
            Reply
    after
        5000 ->
            {error, timeout}
    end.


send(To, Msg) ->
    To ! {oneway, Msg}.


reply(To, Msg) ->
    To ! {response, self(), Msg}.


stop_children(#node{children=Children}) ->
    dict:map(fun(_K, Pid) -> stop(Pid) end, Children).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - node functions

reparse(Pid) when is_pid(Pid) ->
    call(Pid, self(), reparse).


pretty_print(Pid, Depth) when is_pid(Pid) ->
    call(Pid, self(), {pretty_print, Depth}).


get_name(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_name).


get_full_path(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_full_path).


get_children(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_children).


get_depth(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_depth).


get_type(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_type).


get_corba_obj(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_corba_obj).


is_child(OtherNode, Pid) when is_pid(Pid), is_pid(OtherNode);
        is_pid(Pid), is_list(OtherNode) ->
    call(Pid, self(), {is_child, OtherNode}).


is_parent(OtherNode, Pid) when is_pid(Pid), is_pid(OtherNode);
        is_pid(Pid), is_list(OtherNode) ->
    call(Pid, self(), {is_parent, OtherNode}).


is_descendant(OtherNode, Pid) when is_pid(Pid), is_pid(OtherNode);
        is_pid(Pid), is_list(OtherNode) ->
    call(Pid, self(), {is_descendant, OtherNode}).


is_ancestor(OtherNode, Pid) when is_pid(Pid), is_pid(OtherNode);
        is_pid(Pid), is_list(OtherNode) ->
    call(Pid, self(), {is_ancestor, OtherNode}).


get_parent_name(Pid) when is_pid(Pid) ->
    call(Pid, self(), parent_name).


get_root(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_root).


is_component(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), is_comp).


is_directory(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), is_dir).


is_manager(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), is_mgr).


is_nameserver(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_ns).


is_unknown(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), is_unknown).


get_nameserver(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_ns).


get_node_by_path(P, Pid) when is_pid(Pid) andalso is_list(P) ->
    call(Pid, self(), {get_node_by_path, P}).


iterate(Fun, Preds, Pid) ->
    call(Pid, self(), {iterate, Fun, Preds}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - process interface

init_node(Init, [], Cleanup, Handler, Parent) ->
    Node = Init(),
    loop(Node#node{cleanup=Cleanup, handler=Handler, parent=Parent});
init_node(Init, Args, Cleanup, Handler, Parent) ->
    Node = Init(Args),
    loop(Node#node{cleanup=Cleanup, handler=Handler, parent=Parent}).


loop(Node) ->
    receive
        {request, From, stop} ->
            Cleanup = Node#node.cleanup,
            reply(From, Cleanup(Node));
        {request, From, Msg} ->
            {Reply, NewNode} = handle_msg(Msg, Node),
            reply(From, Reply),
            loop(NewNode);
        {oneway, Msg} ->
            NewNode = handle_msg(Msg, Node),
            loop(NewNode);
        Other ->
            io:format("~p received unhandled message ~p~n", [Node#node.fullpath, Other]),
            loop(Node)
    end.


handle_msg(get_name, #node{name=Name} = Node) ->
    {Name, Node};
handle_msg(get_full_path, #node{fullpath=FP} = Node) ->
    {FP, Node};
handle_msg(get_children, #node{children=C} = Node) ->
    {C, Node};
handle_msg(get_depth, #node{depth=D} = Node) ->
    {D, Node};
handle_msg(get_type, #node{type=T} = Node) ->
    {T, Node};
handle_msg(get_corba_obj, #node{obj=O} = Node) ->
    {O, Node};
handle_msg({is_child, Other}, Node) ->
    {is_child_int(Other, Node), Node};
handle_msg({is_parent, Other}, Node) ->
    {is_parent_int(Other, Node), Node};
handle_msg({is_descendant, Other}, Node) ->
    {is_descendant_int(Other, Node), Node};
handle_msg({is_ancestor, Other}, Node) ->
    {is_ancestor_int(Other, Node), Node};
handle_msg(parent_name, Node) ->
    {parent_name_int(Node), Node};
handle_msg(get_root, Node) ->
    {get_root_int(Node), Node};
handle_msg(is_comp, Node) ->
    {is_comp_int(Node), Node};
handle_msg(is_dir, Node) ->
    {is_dir_int(Node), Node};
handle_msg(is_mgr, Node) ->
    {is_mgr_int(Node), Node};
handle_msg(is_ns, Node) ->
    {is_ns_int(Node), Node};
handle_msg(is_unknown, Node) ->
    {is_unknown_int(Node), Node};
handle_msg(get_ns, Node) ->
    {get_ns(Node), Node};
handle_msg({get_node_by_path, P}, Node) ->
    {get_node_by_path_int(P, Node), Node};
handle_msg({iterate, Fun, Preds}, Node) ->
    {iterate_int(Fun, Preds, Node, []), Node};
handle_msg({pretty_print, Depth}, Node) ->
    {pretty_print_int(Node, Depth), Node};
handle_msg(Msg, #node{handler=Handler} = Node) ->
    %io:format("Functional request to ~p: ~p~n", [self(), Msg]),
    Handler(Msg, Node).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - Node functionality

is_child_int(Pid, #node{children=C}) when is_pid(Pid) ->
    Test = fun(_, P, Acc) -> case P of Pid -> true; _ -> Acc end end,
    dict:fold(Test, false, C);
is_child_int([], _) ->
    false;
is_child_int(["/"], _) ->
    % The root is not a child of anyone
    false;
is_child_int([H], #node{children=C}) ->
    dict:is_key(H, C);
is_child_int(["/"|_] = Other, #node{fullpath=FP}) ->
     length(FP) == (length(Other) - 1) andalso lists:prefix(FP, Other);
is_child_int([_|_], _) ->
    % Multi-element paths cannot be immediate children
    false.


is_parent_int(Pid, #node{parent=Pid}) ->
    true;
is_parent_int(Pid, _Node) when is_pid(Pid) ->
    false;
is_parent_int(Other, #node{fullpath=P}) when is_list(Other) ->
    % The other path must be a prefix of this, and one element shorter.
    length(Other) == (length(P) - 1) andalso lists:prefix(Other, P).


descendant_search(_, []) ->
    false;
descendant_search(Pid, [{_, Pid}|_]) ->
    true;
descendant_search(OPid, [{_, CPid}|T]) ->
    case is_descendant(OPid, CPid)
        of true ->
            true
         ; false ->
            descendant_search(OPid, T)
    end.


is_descendant_int(Pid, #node{children=C}) when is_pid(Pid) ->
    descendant_search(Pid, dict:to_list(C));
is_descendant_int([], _) ->
    false;
is_descendant_int(["/"], _) ->
    % The root node is not a descendant of anyone
    false;
is_descendant_int([H], #node{children=C}) ->
    dict:is_key(H, C);
is_descendant_int(["/"|_] = Path, #node{fullpath=FP}) ->
    % Case for an absolute path
    lists:prefix(FP, Path);
is_descendant_int([H|T], #node{children=C}) ->
    case dict:is_key(H, C)
        of true ->
            P = dict:fetch(H, C),
            is_descendant(T, P)
         ; false ->
            false
    end.


is_ancestor_int(Pid, #node{parent=Pid}) when is_pid(Pid) ->
    true;
is_ancestor_int(Pid, #node{parent=nil}) when is_pid(Pid) ->
    % Have reached a node below the root, and that doesn't match, so false.
    false;
is_ancestor_int(Pid, #node{parent=P}) when is_pid(Pid) ->
    % Recurse.
    node:is_ancestor(Pid, P);
is_ancestor_int(Other, #node{fullpath=P}) when is_list(Other) ->
    lists:prefix(Other, P).


parent_name_int(#node{parent=nil}) ->
    "";
parent_name_int(#node{parent=P}) ->
    node:get_name(P).


get_root_int(#node{depth=0}) ->
    {ok, self()};
get_root_int(#node{parent=P}) ->
    node:get_root(P).


is_comp_int(#node{type=component}) ->
    true;
is_comp_int(_) ->
    false.


is_dir_int(#node{type=directory}) ->
    true;
is_dir_int(#node{type=manager}) ->
    true;
is_dir_int(_) ->
    false.


is_mgr_int(#node{type=manager}) ->
    true;
is_mgr_int(_) ->
    false.


is_ns_int(#node{type=nameserver}) ->
    true;
is_ns_int(_) ->
    false.


is_unknown_int(#node{type=unknown}) ->
    true;
is_unknown_int(_) ->
    false.


get_ns(#node{depth=0}) ->
    % The root node doesn't have a name server.
    {error, nonode};
get_ns(#node{depth=1, type=nameserver}) ->
    % Found the top-level name server.
    {ok, self()};
get_ns(#node{parent=P}) ->
    % Recurse up the tree
    get_nameserver(P).


get_node_by_path_int([H], #node{name=H}) ->
    {ok, self()};
get_node_by_path_int([_], _) ->
    {error, nonode};
get_node_by_path_int([H|T], #node{name=H, children=C}) ->
    [N|_] = T,
    case dict:is_key(N, C)
        of true ->
            P = dict:fetch(N, C),
            get_node_by_path(T, P)
         ; false ->
            {error, nonode}
    end;
get_node_by_path_int(_, _) ->
    % Catches cases where the top of the path doesn't match the node name
    {error, nonode}.


iterate_int(Fun, Preds, #node{children=Children} = Node, Acc) ->
    % Collect children results, append them to Acc (can't use [|] because if
    % the results are strings, we can't flatten the list).
    CItr = fun({_, C}, CAcc) -> lists:append(iterate(Fun, Preds, C), CAcc) end,
    NewAcc = lists:append(Acc, lists:foldl(CItr, [], dict:to_list(Children))),
    case test_preds(Preds, Node, false)
        of true ->
            % Append own result to Acc and return
            [Fun(Node)|NewAcc]
         ; false ->
            NewAcc
    end.


pretty_print_int(#node{name=Name, children=C}, Depth) ->
    FC = dict:map(fun(_, N) -> node:pretty_print(N, Depth + 1) end, C),
    FC1 = lists:map(fun({_, V}) -> V end, dict:to_list(FC)),
    PC = string:join(FC1, io_lib:nl()),
    Name1 = string:join([indent(Depth, ""), Name], ""),
    case PC
        of [] ->
            Name1
         ; _ ->
            string:join([Name1, PC], io_lib:nl())
    end.


indent(0, Acc) ->
    Acc;
indent(D, Acc) ->
    indent(D - 1, [$ |Acc]).


test_preds([], _, Acc) ->
    Acc;
test_preds([H|T], Node, Acc) ->
    case H(Node)
        of true ->
            test_preds(T, Node, true)
         ; false ->
            test_preds(T, Node, Acc)
    end.

