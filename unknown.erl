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
% Unknown object functionality.

-module(unknown).
% Internal use exports
-export([start/5, init/1, terminate/1, handle_msg/2]).
-include("include/nodes.hrl").
-include_lib("idl/RTC.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - node functions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal API - node process

start(ID, Kind, Obj, PP, Depth) ->
    node:start(fun init/1, [ID, Kind, Obj, PP, Depth], fun terminate/1,
        fun handle_msg/2, self()).


init([ID, Kind, Obj, PP, Depth]) ->
    NodeName = lists:concat([ID, ".", Kind]),
    FP = lists:append(PP, [NodeName]),
    #node{name=NodeName, fullpath=FP, depth=Depth, type=unknown, obj=Obj}.


terminate(_Node) ->
    ok.


handle_msg(reparse, Node) ->
    {ok, Node}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

