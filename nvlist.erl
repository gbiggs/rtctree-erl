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
% SDO name/value list conversion functions.

-module(nvlist).

% External use exports
-export([to_dict/1, from_dict/1]).

% Internal use exports
-export([test/0]).
-include("idl/SDOPackage.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API

to_dict(NVList) ->
    dict:from_list(lists:map(fun pair_to_tuple/1, NVList)).


from_dict(Dict) ->
    lists:map(fun tuple_to_pair/1, dict:to_list(Dict)).


pair_to_tuple(#'SDOPackage_NameValue'{name=Name, value=Any}) ->
    {Name, any:get_value(Any)}.


tuple_to_pair({Name, Value}) when is_list(Value) ->
    TC = orber_tc:string(0),
    Any = any:create(TC, Value),
    #'SDOPackage_NameValue'{name=Name, value=Any}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    D = dict:from_list([{"One", "1"}, {"Two", "2"}, {"Three", "3"}]),
    NewD = to_dict(from_dict(D)),
    {ok, "1"} = dict:find("One", NewD),
    {ok, "2"} = dict:find("Two", NewD),
    {ok, "3"} = dict:find("Three", NewD),
    ok.

