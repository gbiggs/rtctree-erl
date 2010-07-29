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
% Directory functionality.

-module(directory).

% External use exports
-export([unbind/2]).
-export([parse_context/3, init/1, terminate/1, handle_msg/2]).

% Internal use exports
-export([test/0]).
-include("include/nodes.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - node functions

unbind(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    node:call(Pid, self(), {unbind, Name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal API - node process

parse_context(NC, PP, Depth) ->
    {ok, BList, BIter} = 'CosNaming_NamingContext':list(NC, 10),
    parse_iters(NC, PP, Depth, BList, BIter, []).


init([ID, Kind, Obj, PP, Depth]) ->
    NodeName = lists:concat([ID, ".", Kind]),
    FP = lists:append(PP, [NodeName]),
    C = lists:map(fun name_child/1, parse_context(Obj, FP, Depth + 1)),
    NewC = lists:foldl(fun({N, P}, Acc) -> dict:store(N, P, Acc) end,
        dict:new(), C),
    #node{name=NodeName, fullpath=FP, depth=Depth, type=directory,
        children=NewC, obj=Obj}.


terminate(Node) ->
    node:stop_children(Node),
    ok.


handle_msg({unbind, Name}, Node) ->
    {unbind_name(Name, Node), Node};
handle_msg(reparse, Node) ->
    reparse(Node).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - directory functionality

reparse(#node{obj=Obj, fullpath=FP, depth=Depth} = Node) ->
    % Stop and delete all children
    node:stop_children(Node),
    % Parse the directory again
    NewC = lists:map(fun name_child/1, directory:parse_context(Obj, FP,
            Depth + 1)),
    NewCDict = lists:foldl(fun({N, P}, Acc) -> dict:store(N, P, Acc) end,
        dict:new(), NewC),
    {ok, Node#node{children=NewCDict}}.


unbind_name(Name, #node{obj=NC}) ->
    % Get ID and Kind from the name
    [ID, Kind] = string:tokens(Name, "."),
    % Build a CosNaming_NameComponent from them
    NameComp = #'CosNaming_NameComponent'{id=ID, kind=Kind},
    io:format("Attempting to unbind ~p~n", [NameComp]),
    'CosNaming_NamingContext':unbind(NC, [NameComp]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - parsing

parse_iters(_, _, _, [], ?ORBER_NIL_OBJREF, Acc) ->
    Acc;
parse_iters(NC, PP, Depth, [], BIter, Acc) ->
    BList = case 'CosNaming_BindingOperator':next_n(BIter, 10)
        of {false, []} ->
            'CosNaming_BindingIterator':destroy(BIter),
            []
         ; {true, List} ->
            List
    end,
    parse_iters(NC, PP, Depth, BList, BIter, Acc);
parse_iters(NC, PP, Depth, BList, BIter, Acc) ->
    Parser = fun(B) -> parse_binding(NC, B, PP, Depth) end,
    Acc2 = lists:append(Acc, lists:map(Parser, BList)),
    parse_iters(NC, PP, Depth, [], BIter, Acc2).


parse_binding(NC, #'CosNaming_Binding'{binding_name=Name,
        binding_type=nobject}, PP, Depth) ->
    parse_object(NC, Name, PP, Depth);
parse_binding(NC, #'CosNaming_Binding'{binding_name=Name,
        binding_type=ncontext}, PP, Depth) ->
    parse_context_int(NC, Name, PP, Depth).


parse_object(NC, [#'CosNaming_NameComponent'{id=ID, kind="rtc"}] = Name, PP,
        Depth) ->
    Obj = 'CosNaming_NamingContext':resolve(NC, Name),
    component:start(ID, Obj, PP, Depth);
parse_object(NC, [#'CosNaming_NameComponent'{id=ID, kind="mgr"}] = Name, PP,
        Depth) ->
    Obj = 'CosNaming_NamingContext':resolve(NC, Name),
    manager:start(ID, Obj, PP, Depth);
parse_object(NC, [#'CosNaming_NameComponent'{id=ID, kind=""}] = Name, PP,
        Depth) ->
    Obj = 'CosNaming_NamingContext':resolve(NC, Name),
    unknown:start(ID, "", Obj, PP, Depth);
parse_object(NC, [#'CosNaming_NameComponent'{id=ID, kind=Kind}] = Name, PP,
        Depth) ->
    Obj = 'CosNaming_NamingContext':resolve(NC, Name),
    unknown:start(ID, Kind, Obj, PP, Depth).


parse_context_int(NC, [#'CosNaming_NameComponent'{id=ID, kind=Kind}] = Name,
        PP, Depth) ->
    Obj = 'CosNaming_NamingContext':resolve(NC, Name),
    node:start(fun init/1, [ID, Kind, Obj, PP, Depth], fun terminate/1,
        fun handle_msg/2, self()).


name_child(P) ->
    {node:get_name(P), P}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    rtctree:test_setup(),
    S = dict:fetch("localhost", node:get_children(rtctree:get_root())),
    D1 = dict:fetch("blurgle.host", node:get_children(S)),
    test_unbind(D1),
    rtctree:test_cleanup().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test functions

test_unbind(D) ->
    ok = unbind(D, "ConsoleIn0.rtc"),
    node:reparse(D),
    4 = length(dict:fetch_keys(node:get_children(D))),
    ok.


