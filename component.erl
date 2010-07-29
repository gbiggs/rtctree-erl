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
% Component functionality.

-module(component).

% External use exports
-export([activate/2, deactivate/2, reset/2, get_owned_ecs/1, get_part_ecs/1,
        get_state_in_ec/2, is_alive/1, get_owned_ec_states/1,
        get_part_ec_states/1, get_state/1]).
-export([get_ports/1, get_in_ports/1, get_out_ports/1, get_corba_ports/1,
        get_conned_ports/1, get_conned_in_ports/1, get_conned_out_ports/1,
        get_conned_corba_ports/1, get_port_by_name/2, get_port_by_obj/2,
        has_port_by_name/2, has_port_by_obj/2, disconnect_all/1]).
-export([get_inst_name/1, get_prof/1, get_config/1]).
-export([add_cb/4, remove_cbs/2, num_cbs/2]).

% Internal use exports
-export([start/4, init/1, terminate/1, handle_msg/2, test/0, exec_cb/5]).
-include("include/nodes.hrl").
-include_lib("idl/RTC.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Execution context management

activate(Pid, Index) when is_pid(Pid) andalso Index > 0 ->
    node:call(Pid, self(), {activate, Index}).


deactivate(Pid, Index) when is_pid(Pid) andalso Index > 0 ->
    node:call(Pid, self(), {deactivate, Index}).


reset(Pid, Index) when is_pid(Pid) andalso Index > 0 ->
    node:call(Pid, self(), {reset, Index}).


get_ec_index(Pid, Handle) when is_pid(Pid) ->
    node:call(Pid, self(), {get_ec_index, Handle}).


get_owned_ecs(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_owned_ecs).


get_part_ecs(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_part_ecs).


get_state_in_ec(Pid, Index) when is_pid(Pid) andalso Index > 0 ->
    node:call(Pid, self(), {get_state_in_ec, Index}).


get_owned_ec_states(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_owned_ec_states).


get_part_ec_states(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_part_ec_states).


is_alive(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), is_alive).


get_state(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Port management

get_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_ports).


get_in_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_in_ports).


get_out_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_out_ports).


get_corba_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_corba_ports).


get_conned_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_conned_ports).


get_conned_in_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_conned_in_ports).


get_conned_out_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_conned_out_ports).


get_conned_corba_ports(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_conned_corba_ports).


get_port_by_name(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    node:call(Pid, self(), {get_port_by_name, Name}).


get_port_by_obj(Pid, {'IOP_IOR', _, _} = Obj) when is_pid(Pid) ->
    node:call(Pid, self(), {get_port_by_obj, Obj}).


has_port_by_name(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    case get_port_by_name(Pid, Name)
        of {error, no_port} ->
            false
         ; _ ->
            true
    end.


has_port_by_obj(Pid, {'IOP_IOR', _, _} = Obj) when is_pid(Pid) ->
    case get_port_by_obj(Pid, Obj)
        of {error, no_port} ->
            false
         ; _ ->
            true
    end.


disconnect_all(Pid) when is_pid(Pid) ->
    dict:map(fun(_, P) -> port:disconnect_all(P) end, get_ports(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Component management

get_inst_name(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_inst_name).


get_prof(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_prof).


get_config(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_config).


reparse(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), reparse).


update_state(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), update_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - callback management

add_cb(Pid, CB, Type, Rate) when
        is_pid(Pid) and is_function(CB) and is_integer(Rate) and is_atom(Type) ->
    node:call(Pid, self(), {add_comp_cb, CB, Type, Rate}).


% By must be a list of tuples, e.g.:
% [{cb, CB}, {rate, Rate}, {type, Type}]
% [{rate, Rate}, {type, Type}]
remove_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {remove_comp_cbs, By}).


num_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {num_comp_cbs, By}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal API - Component process

start(ID, Obj, PP, Depth) ->
    node:start(fun init/1, [ID, Obj, PP, Depth], fun terminate/1,
        fun handle_msg/2, self()).


init([ID, Obj, PP, Depth]) ->
    NodeName = lists:concat([ID, ".rtc"]),
    FP = lists:append(PP, [NodeName]),
    Data = parse_comp(Obj),
    Data2 = Data#component{state=get_state_int(Obj, Data)},
    #node{name=NodeName, fullpath=FP, depth=Depth, type=component, obj=Obj,
        data=Data2}.


terminate(#node{data=#component{owned_ecs=OEC, part_ecs=PEC, in_ports=IP, out_ports=OP,
            corba_ports=CP}}) ->
    StopPort = fun({_, P}) -> port:stop(P) end,
    lists:foreach(fun exec_context:stop/1, OEC),
    lists:foreach(fun exec_context:stop/1, PEC),
    lists:foreach(StopPort, dict:to_list(IP)),
    lists:foreach(StopPort, dict:to_list(OP)),
    lists:foreach(StopPort, dict:to_list(CP)),
    ok.


% EC management
handle_msg({activate, I}, #node{obj=O, data=D} = Node) ->
    {activate(O, D, I), Node};
handle_msg({deactivate, I}, #node{obj=O, data=D} = Node) ->
    {deactivate(O, D, I), Node};
handle_msg({reset, I}, #node{obj=O, data=D} = Node) ->
    {reset(O, D, I), Node};
handle_msg({get_ec_index, H}, #node{data=D} = Node) ->
    {get_ec_index_int(D, H), Node};
handle_msg(get_owned_ecs, #node{data=#component{owned_ecs=ECs}} = Node) ->
    {ECs, Node};
handle_msg(get_part_ecs, #node{data=#component{part_ecs=ECs}} = Node) ->
    {ECs, Node};
handle_msg({get_state_in_ec, I}, #node{obj=O, data=D} = Node) ->
    {get_state_in_ec_int(O, D, I), Node};
handle_msg(get_owned_ec_states,
        #node{obj=O, data=#component{owned_ecs=ECs}} = Node) ->
    {get_ec_states(O, ECs, []), Node};
handle_msg(get_part_ec_states,
        #node{obj=O, data=#component{part_ecs=ECs}} = Node) ->
    {get_ec_states(O, ECs, []), Node};
handle_msg(is_alive, #node{obj=O, data=D} = Node) ->
    {is_alive(O, D), Node};
handle_msg(get_state, #node{obj=O, data=D} = Node) ->
    {get_state_int(O, D), Node};
% Port management
handle_msg(get_ports, #node{data=D} = Node) ->
    {get_all_ports(D), Node};
handle_msg(get_in_ports, #node{data=#component{in_ports=P}} = Node) ->
    {P, Node};
handle_msg(get_out_ports, #node{data=#component{out_ports=P}} = Node) ->
    {P, Node};
handle_msg(get_corba_ports, #node{data=#component{corba_ports=P}} = Node) ->
    {P, Node};
handle_msg(get_conned_ports, #node{data=D} = Node) ->
    {get_conned_ports_int(D), Node};
handle_msg(get_conned_in_ports, #node{data=D} = Node) ->
    {get_conned_in_ports_int(D), Node};
handle_msg(get_conned_out_ports, #node{data=D} = Node) ->
    {get_conned_out_ports_int(D), Node};
handle_msg(get_conned_corba_ports, #node{data=D} = Node) ->
    {get_conned_corba_ports_int(D), Node};
handle_msg({get_port_by_name, Name}, #node{data=D} = Node) ->
    {get_port_by_name_int(Name, D), Node};
handle_msg({get_port_by_obj, Obj}, #node{data=D} = Node) ->
    {get_port_by_obj_int(Obj, D), Node};
% Component management
handle_msg(get_inst_name,
    #node{data=#component{profile=#comp_profile{instance_name=IN}}} = Node) ->
    {IN, Node};
handle_msg(get_prof, #node{data=#component{profile=P}} = Node) ->
    {P, Node};
handle_msg(get_config, #node{data=#component{config=C}} = Node) ->
    {C, Node};
% Callback management
handle_msg({add_comp_cb, CB, T, R}, Node) ->
    add_cb_int(CB, T, R, Node);
handle_msg({remove_comp_cbs, By}, Node) ->
    remove_cbs_int(By, Node);
handle_msg({num_comp_cbs, By}, #node{data=D} = Node) ->
    {num_cbs_int(By, D), Node};
% Callback triggers
handle_msg(state_tick, Node) ->
    do_state_cb(Node);
% Other
handle_msg(update_state, Node) ->
    {ok, Node};
handle_msg(reparse, Node) ->
    reparse_int(Node).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - execution context management

activate(Obj, C, Index) ->
    EC = get_ec_by_index(C, Index),
    exec_context:activate_comp(EC, Obj).


deactivate(Obj, C, Index) ->
    EC = get_ec_by_index(C, Index),
    exec_context:deactivate_comp(EC, Obj).


reset(Obj, C, Index) ->
    EC = get_ec_by_index(C, Index),
    exec_context:reset_comp(EC, Obj).


get_ec_by_index(#component{owned_ecs=OEC, part_ecs=PEC}, Index) when
        Index > length(OEC) ->
    lists:nth(Index - length(OEC), PEC);
get_ec_by_index(#component{owned_ecs=OEC}, Index) ->
    lists:nth(Index, OEC).


get_all_ecs(#component{owned_ecs=OEC, part_ecs=PEC}) ->
    lists:append(OEC, PEC).


get_ec_index_int(C, ECH) ->
    AllECs = get_all_ecs(C),
    get_ec_index_int1(ECH, AllECs, 1).

get_ec_index_int1(_, [], _) ->
    0;
get_ec_index_int1(ECH, [H|T], C) ->
    case exec_context:get_handle(H)
        of ECH ->
            C
         ; _ ->
            get_ec_index_int1(ECH, T, C + 1)
    end.


get_state_in_ec_int(Obj, #component{owned_ecs=OEC, part_ecs=PEC}, Index) when
        Index > length(OEC) ->
    exec_context:get_comp_state(lists:nth(Index - length(OEC), PEC), Obj);
get_state_in_ec_int(Obj, #component{owned_ecs=OEC}, Index) ->
    exec_context:get_comp_state(lists:nth(Index, OEC), Obj).


get_ec_states(_, [], Acc) ->
    lists:reverse(Acc);
get_ec_states(Obj, [H|T], Acc) ->
    get_ec_states(Obj, T, [exec_context:get_comp_state(H, Obj)|Acc]).


is_alive(Obj, #component{owned_ecs=OEC, part_ecs=PEC}) ->
    case is_alive1(Obj, OEC)
        of true ->
            true
         ; false ->
            is_alive1(Obj, PEC)
    end.

is_alive1(_, []) ->
    false;
is_alive1(Obj, [H|T]) ->
    case 'RTC_RTObject':is_alive(Obj, exec_context:get_corba_obj(H))
        of true ->
            true
         ; false ->
            is_alive1(Obj, T)
    end.


merge_state(_New, 'ERROR_STATE') ->
    'ERROR_STATE';
merge_state('ERROR_STATE', _Old) ->
    'ERROR_STATE';
merge_state(_New, 'ACTIVE_STATE') ->
    'ACTIVE_STATE';
merge_state('ACTIVE_STATE', _Old) ->
    'ACTIVE_STATE';
merge_state('INACTIVE_STATE', _Old) ->
    'INACTIVE_STATE';
merge_state('CREATED_STATE', _Old) ->
    'CREATED_STATE'.


get_state_int(Obj, C) ->
    GetState = fun(EC) -> exec_context:get_comp_state(EC, Obj) end,
    AllECs = get_all_ecs(C),
    lists:foldl(fun merge_state/2, 'CREATED_STATE',
        lists:map(GetState, AllECs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - port management

get_all_ports(#component{in_ports=IP, out_ports=OP, corba_ports=CP}) ->
    M = fun(_, P1, P2) -> {P1, P2} end,
    dict:merge(M, CP, dict:merge(M, IP, OP)).


get_conned_ports_int(C) ->
    M = fun(_, P1, P2) -> {P1, P2} end,
    dict:merge(M, get_conned_corba_ports_int(C),
        dict:merge(M, get_conned_in_ports_int(C), get_conned_out_ports_int(C))).


get_conned_in_ports_int(#component{in_ports=IP}) ->
    dict:filter(fun(_, P) -> port:is_connected(P) end, IP).


get_conned_out_ports_int(#component{out_ports=OP}) ->
    dict:filter(fun(_, P) -> port:is_connected(P) end, OP).


get_conned_corba_ports_int(#component{corba_ports=CP}) ->
    dict:filter(fun(_, P) -> port:is_connected(P) end, CP).


get_port_by_name_int(Name, #component{in_ports=IP} = C) ->
    case dict:find(Name, IP)
        of {ok, P} ->
            P
         ; error ->
            get_port_by_name_int1(Name, C)
    end.

get_port_by_name_int1(Name, #component{out_ports=OP} = C) ->
    case dict:find(Name, OP)
        of {ok, P} ->
            P
         ; error ->
            get_port_by_name_int2(Name, C)
    end.

get_port_by_name_int2(Name, #component{corba_ports=CP}) ->
    case dict:find(Name, CP)
        of {ok, P} ->
            P
         ; error ->
            {error, no_port}
    end.


obj_match(Obj) ->
    fun(_, P) ->
        case port:get_corba_obj(P)
            of Obj ->
                true
             ; _ ->
                false
        end
    end.


get_port_by_obj_int(Obj, #component{in_ports=IP} = C) ->
    P = dict:fetch_keys(dict:filter(obj_match(Obj), IP)),
    case P
        of [N] ->
            dict:fetch(N, IP)
         ; [] ->
            get_port_by_obj_int1(Obj, C)
    end.

get_port_by_obj_int1(Obj, #component{out_ports=OP} = C) ->
    P = dict:fetch_keys(dict:filter(obj_match(Obj), OP)),
    case P
        of [N] ->
            dict:fetch(N, OP)
         ; [] ->
            get_port_by_obj_int2(Obj, C)
    end.

get_port_by_obj_int2(Obj, #component{corba_ports=CP}) ->
    P = dict:fetch_keys(dict:filter(obj_match(Obj), CP)),
    case P
        of [N] ->
            dict:fetch(N, CP)
         ; [] ->
            {error, no_port}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback management

add_cb_int(CB, state, Rate, #node{data=D}=Node) ->
    CBs = D#component.cbs,
    case dict:is_key(state, CBs)
        of true ->
            {_, _, Pid} = dict:fetch(state, CBs),
            Pid ! {new_rate, Rate},
            NewCBs = dict:update(state, fun(V) ->
                        add_cb_to_list(CB, Rate, V) end, nil, CBs),
            NewD = D#component{cbs=NewCBs},
            {ok, Node#node{data = NewD}}
         ; false ->
            NewCBs = dict:store(state, {Rate, [CB],
                    ticker:start_ticker(Rate, state_tick)}, CBs),
            NewD = D#component{cbs=NewCBs},
            {ok, Node#node{data=NewD}}
    end.


add_cb_to_list(CB, Rate, {_, CBs, Pid}) ->
    {Rate, [CB|CBs], Pid}.


remove_cbs_int([], #node{data=#component{cbs=CBs}=Data}=Node) ->
    dict:map(fun(_, {_, _, P}) -> ticker:stop_ticker(P) end,
        CBs),
    {ok, Node#node{data=Data#component{cbs=dict:new()}}};
remove_cbs_int(By, #node{data=#component{cbs=CBs}=Data}=Node) ->
    LiveCBs = dict:filter(fun(K, V) ->
                is_not_in_by(By, K, V) end, CBs),
    DeadCBs = dict:filter(fun(K, V) ->
                is_in_by(By, K, V) end, CBs),
    dict:map(fun stop_empty_timers/2, DeadCBs),
    Node#node{data=Data#component{cbs=LiveCBs}}.


stop_empty_timers(_, {_, [], Pid}=V) ->
    ticker:stop_ticker(Pid),
    V;
stop_empty_timers(_, V) ->
    V.


is_not_in_by([], _, _) ->
    true;
is_not_in_by([{type, Type}|_], Type, _) ->
    false;
is_not_in_by([{cb, CB}|_], _K, {_Rate, CBs, _Pid}) ->
    case lists:member(CB, CBs)
        of true ->
            false
         ; false ->
            true
    end;
is_not_in_by([{rate, Rate}|_], _K, {Rate, _CBs, _Pid}) ->
    false;
is_not_in_by([_|T], K, V) ->
    is_not_in_by(T, K, V).


is_in_by(By, K, V) ->
    case is_not_in_by(By, K, V)
        of true ->
            false
         ; false ->
            true
    end.


num_cbs_int([], #node{data=#component{cbs=CBs}}) ->
    length(CBs);
num_cbs_int(By, #node{data=#component{cbs=CBs}}) ->
    CBs2 = dict:filter(fun(K, V) -> is_in_by(By, K, V) end, CBs),
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback calling

do_state_cb(#node{obj=O, data=D, fullpath=FP} = Node) ->
    CBs = D#component.cbs,
    case dict:find(state, CBs)
        of {ok, {_, CBList, _}} ->
            Current = D#component.state,
            case get_state_int(O, D)
                of Current ->
                    Node
                 ; New ->
                    % Trigger the callbacks in new processes
                    lists:foreach(fun(CB) -> spawn_link(?MODULE, exec_cb,
                                    [New, Current, self(), FP, CB]) end,
                        CBList),
                    NewD = D#component{state=New},
                    Node#node{data=NewD}
            end
         ; error ->
            % No callbacks registered (so why is there a ticker?)
            Node
    end.


exec_cb(NewVal, OldVal, NodePid, FP, CB) ->
    CB (NewVal, OldVal, NodePid, FP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - parsing

reparse_int(#node{obj=Obj} = N) ->
    terminate(N),
    {ok, N#node{data=parse_comp(Obj)}}.


parse_comp(C) ->
    Ports = parse_ports(C),
    InPorts = lists:filter(fun({_, P}) -> port:is_in_port(P) end, Ports),
    OutPorts = lists:filter(fun({_, P}) -> port:is_out_port(P) end, Ports),
    CorbaPorts = lists:filter(fun({_, P}) -> port:is_corba_port(P) end, Ports),
    #component{profile=parse_profile(C),
        owned_ecs=parse_owned_ecs(C),
        part_ecs=parse_participating_ecs(C),
        in_ports=dict:from_list(InPorts),
        out_ports=dict:from_list(OutPorts),
        corba_ports=dict:from_list(CorbaPorts),
        config=parse_configuration(C),
        cbs=dict:new()}.


parse_profile(C) ->
    P = 'RTC_RTObject':get_component_profile(C),
    Parent = get_corba_obj_name(P#'RTC_ComponentProfile'.parent),
    Properties = nvlist:to_dict(P#'RTC_ComponentProfile'.properties),
    #comp_profile{category=P#'RTC_ComponentProfile'.category,
        description=P#'RTC_ComponentProfile'.description,
        instance_name=P#'RTC_ComponentProfile'.instance_name,
        type_name=P#'RTC_ComponentProfile'.type_name,
        vendor=P#'RTC_ComponentProfile'.vendor,
        version=P#'RTC_ComponentProfile'.version,
        parent=Parent,
        properties=Properties}.


parse_ports(C) ->
    Objs = 'RTC_RTObject':get_ports(C),
    % Start all port processes before asking for names
    Ps = lists:map(fun port:start/1,
        lists:map(fun(O) -> {O, self()} end, Objs)),
    lists:map(fun(P) -> {port:get_name(P), P} end, Ps).


get_corba_obj_name(?ORBER_NIL_OBJREF) ->
    "";
get_corba_obj_name(C) ->
    P = 'RTC_RTObject':get_component_profile(C),
    P#'RTC_ComponentProfile'.instance_name.


parse_owned_ecs(C) ->
    Objs = 'RTC_RTObject':get_owned_contexts(C),
    Handles = lists:map(fun(O) -> 'RTC_RTObject':get_context_handle(C, O) end,
        Objs),
    Combine = fun(Obj, H) -> {Obj, H, self()} end,
    lists:map(fun exec_context:start/1, lists:zipwith(Combine, Objs, Handles)).


parse_participating_ecs(C) ->
    Objs = 'RTC_RTObject':get_participating_contexts(C),
    Handles = lists:map(fun(O) -> 'RTC_RTObject':get_context_handle(C, O) end,
        Objs),
    Combine = fun(Obj, H) -> {Obj, H, nil} end,
    lists:map(fun exec_context:start/1, lists:zipwith(Combine, Objs, Handles)).


parse_configuration(C) ->
    configuration:start('RTC_RTObject':get_configuration(C)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    rtctree:test_setup(),
    S = dict:fetch("localhost", node:get_children(rtctree:get_root())),
    D = dict:fetch("blurgle.host", node:get_children(S)),
    C_IP = dict:fetch("ConsoleOut0.rtc", node:get_children(D)),
    C_OP = dict:fetch("ConsoleIn0.rtc", node:get_children(D)),
    C_IOP = dict:fetch("Controller0.rtc", node:get_children(D)),
    C_IOP2 = dict:fetch("Motor0.rtc", node:get_children(D)),
    C_CP = dict:fetch("MyServiceProvider0.rtc", node:get_children(D)),
    C_CP2 = dict:fetch("MyServiceConsumer0.rtc", node:get_children(D)),
    C1 = dict:fetch("ConfigSample0.rtc", node:get_children(D)),
    test_get_config(C_IP),
    test_get_owned_ecs(C_IP),
    test_get_part_ecs(C_IP),
    test_get_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_in_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_out_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_corba_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_port_by_name(C_IP, C_OP, C_IOP, C_CP),
    test_get_port_by_obj(C_IP, C_OP, C_IOP, C_CP),
    test_has_port_by_name(C_IP, C_OP, C_IOP, C_CP),
    test_has_port_by_obj(C_IP, C_OP, C_IOP, C_CP),
    conn_ports(C_IOP, C_IOP2, C_CP, C_CP2),
    test_get_conned_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_conned_in_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_conned_out_ports(C_IP, C_OP, C_IOP, C_CP),
    test_get_conned_corba_ports(C_IP, C_OP, C_IOP, C_CP),
    dis_ports(C_IOP, C_CP),
    test_get_conned_ports2(C_IOP, C_CP),
    test_get_conned_in_ports2(C_IOP, C_CP),
    test_get_conned_out_ports2(C_IOP, C_CP),
    test_get_conned_corba_ports2(C_IOP, C_CP),
    test_update_state(C_IP),
    test_reparse(C_IP),
    test_get_inst_name(C_IP),
    test_get_prof(C_IP),
    test_get_ec_index(C_IP),
    test_get_state_in_ec(C_IP, C1),
    test_get_owned_ec_states(C_IP),
    test_get_part_ec_states(C_IP),
    test_is_alive(C_IP),
    test_get_state(C_IP, C1),
    test_activate_comp(C_IP),
    test_deactivate_comp(C_IP),
    test_reset_comp(C_IP),
    test_callbacks(C_IP),
    rtctree:test_cleanup(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

conn_ports(C1, C2, C3, C4) ->
    P1 = get_port_by_name(C1, "out"),
    P2 = get_port_by_name(C2, "in"),
    port:connect(P1, P2),
    P3 = get_port_by_name(C3, "MyService"),
    P4 = get_port_by_name(C4, "MyService"),
    port:connect(P3, P4),
    ok.


dis_ports(C1, C3) ->
    P1 = get_port_by_name(C1, "out"),
    port:disconnect_all(P1),
    P3 = get_port_by_name(C3, "MyService"),
    port:disconnect_all(P3),
    ok.


test_get_config(C) ->
    Config = get_config(C),
    true = is_pid(Config),
    ok.


test_get_owned_ecs(C) ->
    [EC] = get_owned_ecs(C),
    true = is_pid(EC),
    ok.


test_get_part_ecs(C) ->
    [] = get_part_ecs(C),
    ok.


test_get_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("All: C_IP "),
    Ps1 = get_ports(C_IP),
    1 = length(dict:fetch_keys(Ps1)),
    P1 = dict:fetch("in", Ps1),
    "in" = port:get_name(P1),
    io:format("C_OP "),
    Ps2 = get_ports(C_OP),
    1 = length(dict:fetch_keys(Ps2)),
    P2 = dict:fetch("out", Ps2),
    "out" = port:get_name(P2),
    io:format("C_IOP "),
    Ps3 = get_ports(C_IOP),
    2 = length(dict:fetch_keys(Ps3)),
    P3 = dict:fetch("in", Ps3),
    "in" = port:get_name(P3),
    P4 = dict:fetch("out", Ps3),
    "out" = port:get_name(P4),
    io:format("C_CP~n"),
    Ps4 = get_ports(C_CP),
    1 = length(dict:fetch_keys(Ps4)),
    P5 = dict:fetch("MyService", Ps4),
    "MyService" = port:get_name(P5),
    ok.


test_get_in_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("In: C_IP "),
    Ps1 = get_in_ports(C_IP),
    1 = length(dict:fetch_keys(Ps1)),
    P1 = dict:fetch("in", Ps1),
    "in" = port:get_name(P1),
    io:format("C_OP "),
    Ps2 = get_in_ports(C_OP),
    [] = dict:fetch_keys(Ps2),
    io:format("C_IOP "),
    Ps3 = get_in_ports(C_IOP),
    1 = length(dict:fetch_keys(Ps3)),
    P2 = dict:fetch("in", Ps3),
    "in" = port:get_name(P2),
    io:format("C_CP~n"),
    Ps4 = get_in_ports(C_CP),
    [] = dict:fetch_keys(Ps4),
    ok.


test_get_out_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("Out: C_IP "),
    Ps1 = get_out_ports(C_IP),
    [] = dict:fetch_keys(Ps1),
    io:format("C_OP "),
    Ps2 = get_out_ports(C_OP),
    1 = length(dict:fetch_keys(Ps2)),
    P1 = dict:fetch("out", Ps2),
    "out" = port:get_name(P1),
    io:format("C_IOP "),
    Ps3 = get_out_ports(C_IOP),
    1 = length(dict:fetch_keys(Ps3)),
    P2 = dict:fetch("out", Ps3),
    "out" = port:get_name(P2),
    io:format("C_CP~n"),
    Ps4 = get_out_ports(C_CP),
    [] = dict:fetch_keys(Ps4),
    ok.


test_get_corba_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("CORBA: C_IP "),
    Ps1 = get_corba_ports(C_IP),
    [] = dict:fetch_keys(Ps1),
    io:format("C_OP "),
    Ps2 = get_corba_ports(C_OP),
    [] = dict:fetch_keys(Ps2),
    io:format("C_IOP "),
    Ps3 = get_corba_ports(C_IOP),
    [] = dict:fetch_keys(Ps3),
    io:format("C_CP~n"),
    Ps4 = get_corba_ports(C_CP),
    1 = length(dict:fetch_keys(Ps4)),
    P = dict:fetch("MyService", Ps4),
    "MyService" = port:get_name(P),
    ok.


test_get_port_by_name(C_IP, C_OP, C_IOP, C_CP) ->
    P1 = get_port_by_name(C_IP, "in"),
    "in" = port:get_name(P1),
    P2 = get_port_by_name(C_OP, "out"),
    "out" = port:get_name(P2),
    P3 = get_port_by_name(C_IOP, "in"),
    "in" = port:get_name(P3),
    P4 = get_port_by_name(C_CP, "MyService"),
    "MyService" = port:get_name(P4),
    {error, no_port} = get_port_by_name(C_IP, "blag"),
    ok.


test_get_port_by_obj(C_IP, C_OP, C_IOP, C_CP) ->
    P1 = get_port_by_name(C_IP, "in"),
    "in" = port:get_name(get_port_by_obj(C_IP, port:get_corba_obj(P1))),
    P2 = get_port_by_name(C_OP, "out"),
    "out" = port:get_name(get_port_by_obj(C_OP, port:get_corba_obj(P2))),
    P3 = get_port_by_name(C_IOP, "in"),
    "in" = port:get_name(get_port_by_obj(C_IOP, port:get_corba_obj(P3))),
    P4 = get_port_by_name(C_CP, "MyService"),
    "MyService" = port:get_name(get_port_by_obj(C_CP, port:get_corba_obj(P4))),
    {error, no_port} = get_port_by_obj(C_IP, port:get_corba_obj(P4)),
    ok.


test_has_port_by_name(C_IP, C_OP, C_IOP, C_CP) ->
    true = has_port_by_name(C_IP, "in"),
    true = has_port_by_name(C_OP, "out"),
    true = has_port_by_name(C_IOP, "in"),
    true = has_port_by_name(C_CP, "MyService"),
    false = has_port_by_name(C_IP, "blag"),
    ok.


test_has_port_by_obj(C_IP, C_OP, C_IOP, C_CP) ->
    P1 = get_port_by_name(C_IP, "in"),
    true = has_port_by_obj(C_IP, port:get_corba_obj(P1)),
    P2 = get_port_by_name(C_OP, "out"),
    true = has_port_by_obj(C_OP, port:get_corba_obj(P2)),
    P3 = get_port_by_name(C_IOP, "in"),
    true = has_port_by_obj(C_IOP, port:get_corba_obj(P3)),
    P4 = get_port_by_name(C_CP, "MyService"),
    true = has_port_by_obj(C_CP, port:get_corba_obj(P4)),
    false = has_port_by_obj(C_IP, port:get_corba_obj(P4)),
    ok.


test_get_conned_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("All: C_IP "),
    Ps1 = get_conned_ports(C_IP),
    1 = length(dict:fetch_keys(Ps1)),
    P1 = dict:fetch("in", Ps1),
    "in" = port:get_name(P1),
    io:format("C_OP "),
    Ps2 = get_conned_ports(C_OP),
    1 = length(dict:fetch_keys(Ps2)),
    P2 = dict:fetch("out", Ps2),
    "out" = port:get_name(P2),
    io:format("C_IOP "),
    Ps3 = get_conned_ports(C_IOP),
    1 = length(dict:fetch_keys(Ps3)),
    P4 = dict:fetch("out", Ps3),
    "out" = port:get_name(P4),
    io:format("C_CP~n"),
    Ps4 = get_conned_ports(C_CP),
    1 = length(dict:fetch_keys(Ps4)),
    P5 = dict:fetch("MyService", Ps4),
    "MyService" = port:get_name(P5),
    ok.


test_get_conned_in_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("In: C_IP "),
    Ps1 = get_conned_in_ports(C_IP),
    1 = length(dict:fetch_keys(Ps1)),
    P1 = dict:fetch("in", Ps1),
    "in" = port:get_name(P1),
    io:format("C_OP "),
    Ps2 = get_conned_in_ports(C_OP),
    [] = dict:fetch_keys(Ps2),
    io:format("C_IOP "),
    Ps3 = get_conned_in_ports(C_IOP),
    0 = length(dict:fetch_keys(Ps3)),
    io:format("C_CP~n"),
    Ps4 = get_conned_in_ports(C_CP),
    [] = dict:fetch_keys(Ps4),
    ok.


test_get_conned_out_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("Out: C_IP "),
    Ps1 = get_conned_out_ports(C_IP),
    [] = dict:fetch_keys(Ps1),
    io:format("C_OP "),
    Ps2 = get_conned_out_ports(C_OP),
    1 = length(dict:fetch_keys(Ps2)),
    P1 = dict:fetch("out", Ps2),
    "out" = port:get_name(P1),
    io:format("C_IOP "),
    Ps3 = get_conned_out_ports(C_IOP),
    1 = length(dict:fetch_keys(Ps3)),
    P2 = dict:fetch("out", Ps3),
    "out" = port:get_name(P2),
    io:format("C_CP~n"),
    Ps4 = get_conned_out_ports(C_CP),
    [] = dict:fetch_keys(Ps4),
    ok.


test_get_conned_corba_ports(C_IP, C_OP, C_IOP, C_CP) ->
    io:format("CORBA: C_IP "),
    Ps1 = get_conned_corba_ports(C_IP),
    [] = dict:fetch_keys(Ps1),
    io:format("C_OP "),
    Ps2 = get_conned_corba_ports(C_OP),
    [] = dict:fetch_keys(Ps2),
    io:format("C_IOP "),
    Ps3 = get_conned_corba_ports(C_IOP),
    [] = dict:fetch_keys(Ps3),
    io:format("C_CP~n"),
    Ps4 = get_conned_corba_ports(C_CP),
    1 = length(dict:fetch_keys(Ps4)),
    P = dict:fetch("MyService", Ps4),
    "MyService" = port:get_name(P),
    ok.


test_get_conned_ports2(C_IOP, C_CP) ->
    io:format("All: C_IOP "),
    Ps1 = get_conned_ports(C_IOP),
    0 = length(dict:fetch_keys(Ps1)),
    io:format("C_CP~n"),
    Ps2 = get_conned_ports(C_CP),
    0 = length(dict:fetch_keys(Ps2)),
    ok.


test_get_conned_in_ports2(C_IOP, C_CP) ->
    io:format("In: C_IOP "),
    Ps1 = get_conned_in_ports(C_IOP),
    0 = length(dict:fetch_keys(Ps1)),
    io:format("C_CP~n"),
    Ps2 = get_conned_in_ports(C_CP),
    0 = length(dict:fetch_keys(Ps2)),
    ok.


test_get_conned_out_ports2(C_IOP, C_CP) ->
    io:format("Out: C_IOP "),
    Ps1 = get_conned_out_ports(C_IOP),
    0 = length(dict:fetch_keys(Ps1)),
    io:format("C_CP~n"),
    Ps2 = get_conned_out_ports(C_CP),
    0 = length(dict:fetch_keys(Ps2)),
    ok.


test_get_conned_corba_ports2(C_IOP, C_CP) ->
    io:format("CORBA: C_IOP "),
    Ps1 = get_conned_corba_ports(C_IOP),
    0 = length(dict:fetch_keys(Ps1)),
    io:format("C_CP~n"),
    Ps2 = get_conned_corba_ports(C_CP),
    0 = length(dict:fetch_keys(Ps2)),
    ok.


test_update_state(C) ->
    ok = update_state(C).


test_reparse(C) ->
    L = length(dict:fetch_keys(get_ports(C))),
    ok = reparse(C),
    L = length(dict:fetch_keys(get_ports(C))),
    ok.


test_get_inst_name(C) ->
    "ConsoleOut0" = get_inst_name(C),
    ok.

test_get_prof(C) ->
    #comp_profile{instance_name="ConsoleOut0"} = get_prof(C),
    ok.


test_get_ec_index(C) ->
    [EC] = get_owned_ecs(C),
    1 = get_ec_index(C, exec_context:get_handle(EC)),
    ok.


test_get_state_in_ec(C1, C2) ->
    'INACTIVE_STATE' = get_state_in_ec(C1, 1),
    'ACTIVE_STATE' = get_state_in_ec(C2, 1),
    ok.


test_is_alive(C) ->
    true = is_alive(C),
    ok.


test_get_owned_ec_states(C) ->
    ['INACTIVE_STATE'] = get_owned_ec_states(C),
    ok.

test_get_part_ec_states(C) ->
    [] = get_part_ec_states(C),
    ok.


test_get_state(C1, C2) ->
    'INACTIVE_STATE' = get_state(C1),
    'ACTIVE_STATE' = get_state(C2),
    ok.


test_activate_comp(C) ->
    ok = activate(C, 1),
    timer:sleep(1000),
    'ACTIVE_STATE' = get_state(C),
    ok.


test_deactivate_comp(C) ->
    ok = deactivate(C, 1),
    timer:sleep(1000),
    'INACTIVE_STATE' = get_state(C),
    ok.


test_reset_comp(C) ->
    {error, precondition_not_met} = reset(C, 1),
    ok.


test_callbacks(C) ->
    CB = fun(S) -> io:format("~p~n", [S]) end,
    ok = add_cb(C, CB, state, 1000),
    sleep(1500),
    ok = activate(C, 1),
    sleep(1500),
    ok = remove_cbs(C, [{cb, CB}]),
    ok = add_cb(C, CB, state, 1000),
    ok = remove_cbs(C, [{type, state}]),
    ok = add_cb(C, CB, state, 1000),
    ok = remove_cbs(C, [{rate, 1000}]),
    ok = add_cb(C, CB, state, 1000),
    ok = add_cb(C, CB, state, 250),
    sleep(1500),
    ok = activate(C, 1),
    sleep(1500),
    ok = remove_cbs(C, [{cb, CB}, {rate, 1000}]),
    sleep(1500),
    ok = activate(C, 1),
    sleep(1500),
    ok = remove_cbs(C, []),
    0 = num_cbs(C, []).


sleep(T) ->
    receive
    after
        T ->
            ok
    end.

