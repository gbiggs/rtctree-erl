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
% Connection functionality.

-module(connection).

% External use exports
-export([disconnect/1, has_port/2]).

% Internal use exports
-export([parse_conn_profile/1]).
-include("include/nodes.hrl").
-include_lib("idl/RTC.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Connection functions

disconnect(#conn_prof{conn_id=Id, ports=[H|_]}) ->
    'RTC_OK' = 'RTC_PortService':disconnect(H, Id),
    ok.


has_port(#conn_prof{ports=P}, {'IOP_IOR', _, _} = Other) ->
    lists:member(Other, P);
has_port(#conn_prof{ports=P}, Other) when is_pid(Other) ->
    lists:member(port:get_corba_obj(Other), P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External functions - parsing

parse_conn_profile({P, Owner}) ->
    #conn_prof{name=P#'RTC_ConnectorProfile'.name,
        conn_id=P#'RTC_ConnectorProfile'.connector_id,
        %ports=lists:map(parse_port_fun(Owner),
            %P#'RTC_ConnectorProfile'.ports)),
        ports = P#'RTC_ConnectorProfile'.ports,
        owner=Owner,
        props=nvlist:to_dict(P#'RTC_ConnectorProfile'.properties)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - Connection functions

%parse_port_fun(Owner) ->
    %fun parse_port(P) ->
        %Prof = 'RTC_PortService':get_port_profile(H),
        %Name = Prof#'RTC_PortProfile'.name,
        %case Owner
            %of nil ->
                %{Name, P}
             %; Obj ->
                %{Name, 


