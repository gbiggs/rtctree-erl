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
% Server node functionality.

-module(server).

% Internal use exports
-export([start/3, init/1, terminate/1, handle_msg/2, test/0]).
-include("include/nodes.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - node functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal API - node process

start(S, PP, Depth) ->
    node:start(fun init/1, [S, PP, Depth], fun terminate/1, fun handle_msg/2,
        self()).


init([Address, ParentPath, Depth]) ->
    NS = corba:string_to_object(full_address(Address)),
    parse_server_int(NS, ParentPath, Depth).


terminate(Node) ->
    node:stop_children(Node),
    ok.


handle_msg(reparse, Node) ->
    {ok, Node}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

full_address(A) ->
    lists:append(["corbaloc:iiop:", A, "/NameService"]).


full_path(PP, Address, Port) ->
    lists:append(PP, [lists:append(Address, Port)]).


parse_server_int(NS, PP, Depth) ->
    {_, _, [{_, _, {_, _, Address, Port, _}}]} = NS,
    PortStr = case Port
        of 2809 ->
            ""
         ; _ ->
            lists:concat([":", Port])
    end,
    FP = full_path(PP, Address, PortStr),
    Name = lists:append(Address, PortStr),
    C = lists:map(fun name_child/1, directory:parse_context(NS, FP,
            Depth + 1)),
    NewC = lists:foldl(fun({N, P}, Acc) -> dict:store(N, P, Acc) end,
        dict:new(), C),
    #node{name=Name,
          fullpath=FP,
          depth=Depth,
          type=nameserver,
          obj=NS,
          children=NewC}.


name_child(P) ->
    {node:get_name(P), P}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    test_full_address(),
    test_full_path().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

test_full_address() ->
    "corbaloc:iiop:localhost/NameService" = full_address("localhost"),
    ok.


test_full_path() ->
    ["/", "localhost:2809"] = full_path(["/"], "localhost", ":2809"),
    ok.

