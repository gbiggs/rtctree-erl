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
% Port functionality.

-module(port).

% External use exports
-export([connect/5, connect/2, disconnect_all/1, get_conns/1,
        get_conn_by_dest/2, get_conn_by_name/2, get_corba_obj/1, get_name/1,
        get_owner/1, get_owner_obj/1, get_props/1, get_type/1, is_connected/1,
        is_data_port/1, is_in_port/1, is_out_port/1, is_corba_port/1,
        reparse/1, reparse_conns/1]).
-export([get_intf_by_inst_name/2, get_intfs/1]).
-export([add_cb/4, remove_cbs/2, num_cbs/2]).

% Internal use exports
-export([start/1, init/1, test/0, exec_cb/5, stop/1]).
-include("include/nodes.hrl").
-include_lib("idl/RTC.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Port functions

connect(Pid, Dest, Name, Id, Props) when is_pid(Pid) andalso is_pid(Dest) ->
    call(Pid, self(), {connect, Dest, Name, Id, Props}).

connect(Pid, Dest) when is_pid(Pid) and is_pid(Dest) ->
    connect(Pid, Dest, "", "", dict:new()).


disconnect_all(Pid) when is_pid(Pid) ->
    call(Pid, self(), disconnect_all).


get_conns(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_conns).


get_conn_by_dest(Pid, Dest) when is_pid(Pid) ->
    call(Pid, self(), {get_conn_by_dest, Dest}).


get_conn_by_name(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    call(Pid, self(), {get_conn_by_name, Name}).


get_name(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_name).


get_corba_obj(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_obj).


get_owner(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_owner).


get_owner_obj(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_owner_obj).


get_props(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_props).


get_type(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_type).


is_connected(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_connected).


is_data_port(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_data_port).


is_in_port(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_in_port).


is_out_port(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_out_port).


is_corba_port(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_corba_port).


reparse(Pid) when is_pid(Pid) ->
    call(Pid, self(), reparse).


reparse_conns(Pid) when is_pid(Pid) ->
    call(Pid, self(), reparse_conns).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - CORBA port functions

get_intf_by_inst_name(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    call(Pid, self(), {get_intf_by_inst_name, Name}).


get_intfs(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_intfs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - port process

start(P) ->
    spawn_link(?MODULE, init, [P]).


init({P, Owner}) ->
    loop(parse_port(P, Owner)).


stop(Pid) when is_pid(Pid) ->
    call(Pid, self(), stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - callback management

add_cb(Pid, CB, Type, Rate) when
        is_pid(Pid) and is_function(CB) and is_integer(Rate) and is_atom(Type) ->
    node:call(Pid, self(), {add_port_cb, CB, Type, Rate}).


% By must be a list of tuples, e.g.:
% [{cb, CB}, {rate, Rate}, {type, Type}]
% [{rate, Rate}, {type, Type}]
remove_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {remove_port_cbs, By}).


num_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {num_port_cbs, By}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - port process

call(To, Sender, Msg) ->
    To ! {request, Sender, Msg},
    receive
        {response, To, Reply} ->
            Reply
    after
        5000 ->
            {error, timeout}
    end.


reply(To, Msg) ->
    To ! {response, self(), Msg}.


terminate(_P) ->
    ok.


loop(P) ->
    receive
        {request, From, stop} ->
            reply(From, terminate(P));
        {request, From, Msg} ->
            {Reply, NewP} = handle_msg(Msg, P),
            reply(From, Reply),
            loop(NewP);
        {oneway, Msg} ->
            NewP = handle_msg(Msg, P),
            loop(NewP);
        Other ->
            io:format("~p received unhandled message ~p~n", [P, Other]),
            loop(P)
    end.


% CORBA port functions
handle_msg({get_intf_by_inst_name, N}, P) ->
    {get_intf_by_inst_name_int(N, P), P};
handle_msg(get_intfs, P) ->
    {get_intfs_int(P), P};
% Generic port functions
handle_msg({connect, Dest, Name, Id, Props}, P) ->
    connect_int(P, Dest, get_type(Dest), {Name, Id, Props});
handle_msg(disconnect_all, P) ->
    disconnect_all_int(P);
handle_msg(get_conns, #port{conn_profs=C} = P) ->
    {C, P};
handle_msg({get_conn_by_dest, D}, #port{conn_profs=C} = P) ->
    {get_conn_by_dest_int(D, C), P};
handle_msg({get_conn_by_name, N}, #port{conn_profs=C} = P) ->
    {get_conn_by_name_int(N, C), P};
handle_msg(get_name, #port{name=N} = P) ->
    {N, P};
handle_msg(get_obj, #port{obj=O} = P) ->
    {O, P};
handle_msg(get_owner, #port{owner=O} = P) ->
    {O, P};
handle_msg(get_owner_obj, #port{owner_obj=O} = P) ->
    {O, P};
handle_msg(get_props, #port{props=Props} = P) ->
    {Props, P};
handle_msg(get_type, #port{type=T} = P) ->
    {T, P};
handle_msg(is_connected, P) ->
    {is_connected_int(P), P};
handle_msg(is_data_port, P) ->
    {is_data_port_int(P), P};
handle_msg(is_in_port, P) ->
    {is_in_port_int(P), P};
handle_msg(is_out_port, P) ->
    {is_out_port_int(P), P};
handle_msg(is_corba_port, P) ->
    {is_corba_port_int(P), P};
% Callback management
handle_msg({add_port_cb, CB, T, R}, P) ->
    add_cb_int(CB, T, R, P);
handle_msg({remove_port_cbs, By}, P) ->
    remove_cbs_int(By, P);
handle_msg({num_port_cbs, By}, P) ->
    {num_cbs_int(By, P), P};
% Callback triggers
handle_msg(conns_tick, P) ->
    do_conns_cb(P);
% Other
handle_msg(reparse, P) ->
    reparse_int(P);
handle_msg(reparse_conns, P) ->
    reparse_conns_int(P);
handle_msg(Msg, #port{name=N} = P) ->
    io:format("Unhandled message for port ~p: ~p~n", [N, Msg]),
    {ok, P}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - connecting ports

connect_int(#port{type=corba} = S, D, corba = _DestType, Opts) ->
    connect_corba_ports(S, D, Opts);
connect_int(#port{type=dataout} = S, D, datain = _DestType, Opts) ->
    connect_data_ports(S, D, Opts);
connect_int(#port{type=datain} = S, D, dataout = _DestType, Opts) ->
    connect_data_ports(S, D, Opts);
connect_int(S, _, _, _) ->
    {{error, wrong_port_type}, S}.


% Data port connection validation
connect_data_ports(#port{props=SP} = S, D, {N, I, Props}) ->
    NewProps = lists:foldl(fun({PN, PV}, A) ->
                case dict:is_key(PN, A) of true -> A;
                    false -> dict:store(PN, PV, A) end end,
        Props, default_data_props(dict:fetch("dataport.data_type", SP))),
    connect_ports(S, D, {N, I, NewProps}).


default_data_props(DataType) ->
    [{"dataport.dataflow_type", "push"},
        {"dataport.interface_type", "corba_cdr"},
        {"dataport.subscription_type", "flush"},
        {"dataport.data_type", DataType}].


% CORBA port connection validation

connect_corba_ports(#port{intfs=SI} = S, D, Opts) when length(SI) =/= 0 ->
    case length(get_intfs(D))
        of 0 ->
            {{error, mismatched_intfs}, S}
         ; _ ->
            connect_corba_ports1(S, D, Opts)
    end;
connect_corba_ports(#port{intfs=SI} = S, D, Opts) when length(SI) =:= 0 ->
    case length(get_intfs(D))
        of 0 ->
            connect_corba_ports1(S, D, Opts)
         ; _ ->
            {{error, mismatched_intfs}, S}
    end.

connect_corba_ports1(#port{intfs=SI} = S, D, Opts) ->
    case check_intfs_match(SI, D)
        of false ->
            {{error, mismatched_intfs}, S}
         ; true ->
            connect_corba_ports2(S, D, Opts)
    end.

connect_corba_ports2(S, D, {N, I, Props}) ->
    NewProps = case dict:is_key("port.port_type", Props)
        of true ->
            Props
         ; false ->
            dict:store("port.port_type", "CorbaPort", Props)
    end,
    connect_ports(S, D, {N, I, NewProps}).


check_intfs_match(SI, D) ->
    lists:foldl(fun(I, A) -> case check_intf_matches(I, D) of true -> true;
                    false -> A end end, false, SI).


check_intf_matches(#port_intf_prof{inst_name=IN, polarity=P}, D) ->
    case get_intf_by_inst_name(D, IN)
        of {error, _} ->
            false
         ; #port_intf_prof{polarity=P} ->
            % Polarity should be opposite
            false
         ; _ ->
            true
    end.


% Port connection

connect_ports(#port{name=SN} = S, D, {"", I, P}) ->
    DN = get_name(D),
    connect_ports1(S, D, {SN ++ "_" ++ DN, I, P}).

connect_ports1(#port{type=T, props=SProps} = S, D,
        {_, _, Props} = Opts) when T =:= datain orelse T =:= dataout ->
    DProps = get_props(D),
    case check_props(dict:fetch_keys(Props), Props, SProps)
        of true ->
            case check_props(dict:fetch_keys(Props), Props, DProps)
                of true ->
                    connect_ports2(S, D, Opts)
                 ; false ->
                    {{error, incompatible_dest_props}, S}
            end
         ; false ->
            {{error, incompatible_source_props}, S}
    end;
connect_ports1(S, D, Opts) ->
    connect_ports2(S, D, Opts).

connect_ports2(#port{obj=Obj} = S, D, {Name, Id, Props}) ->
    CP = #'RTC_ConnectorProfile'{name=Name, connector_id=Id,
        ports=[Obj, get_corba_obj(D)], properties=nvlist:from_dict(Props)},
    Res = 'RTC_PortService':connect(Obj, CP),
    ok = reparse_conns(D),
    {ok, NewS} = reparse_conns_int(S),
    case Res
        of {'RTC_OK', _} ->
            {ok, NewS}
         ; _ ->
            {{error, unknown_error}, NewS}
    end.


check_props([], _, _) ->
    true;
check_props([H|T], Props1, Props2) ->
    Prop1 = dict:fetch(H, Props1),
    Prop2 = dict:fetch(H, Props2),
    case string:str(Prop2, Prop1)
        of 0 ->
            case string:str(Prop2, "Any")
                of 0 ->
                    false
                 ; _ ->
                    check_props(T, Props1, Props2)
            end
         ; _ ->
            check_props(T, Props1, Props2)
    end.


disconnect_all_int(#port{conn_profs=CP, obj=Obj} = P) ->
    Dis = fun(#conn_prof{conn_id=Id}) -> 'RTC_PortService':disconnect(Obj, Id) end,
    lists:foreach(Dis, CP),
    reparse_conns_int(P). % Already returns {ok, NewP}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - generic port functionality

get_conn_by_dest_int({'IOP_IOR', _, _} = D, C) ->
    get_conn_by_dest_int1(D, C);
get_conn_by_dest_int(D, C) when is_pid(D) ->
    get_conn_by_dest_int1(get_corba_obj(D), C).

get_conn_by_dest_int1(_, []) ->
    {error, bad_conn_dest};
get_conn_by_dest_int1(D, [#conn_prof{ports=P} = C|T]) ->
    case lists:member(D, P)
        of true ->
            C
         ; false ->
            get_conn_by_dest_int1(D, T)
    end.


get_conn_by_name_int(_, []) ->
    {error, bad_conn_name};
get_conn_by_name_int(N, [#conn_prof{name=N} = C|_]) ->
    C;
get_conn_by_name_int(N, [_|T]) ->
    get_conn_by_name(N, T).


is_connected_int(#port{conn_profs=[]}) ->
    false;
is_connected_int(_) ->
    true.


is_data_port_int(#port{type=Type}) when
        Type == datain orelse Type == dataout ->
    true;
is_data_port_int(_) ->
    false.


is_in_port_int(#port{type=datain}) ->
    true;
is_in_port_int(_) ->
    false.


is_out_port_int(#port{type=dataout}) ->
    true;
is_out_port_int(_) ->
    false.


is_corba_port_int(#port{type=corba}) ->
    true;
is_corba_port_int(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - CORBA port functionality

get_intf_by_inst_name_int(N, #port{type=corba, intfs=I}) ->
    get_intf_by_inst_name_int1(N, I);
get_intf_by_inst_name_int(_, _) ->
    {error, wrong_port_type}.

get_intf_by_inst_name_int1(_, []) ->
    {error, bad_inst_name};
get_intf_by_inst_name_int1(N, [#port_intf_prof{inst_name=N} = I|_]) ->
    I;
get_intf_by_inst_name_int1(N, [_|T]) ->
    get_intf_by_inst_name_int1(N, T).


get_intfs_int(#port{type=corba, intfs=I}) ->
    I;
get_intfs_int(_) ->
    {error, wrong_port_type}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback management

add_cb_int(CB, conns, Rate, P) ->
    CBs = P#port.cbs,
    case dict:is_key(conns, CBs)
        of true ->
            {_, _, Pid} = dict:fetch(conns, CBs),
            Pid ! {new_rate, Rate},
            NewCBs = dict:update(conns, fun(V) ->
                        add_cb_to_list(CB, Rate, V) end, nil, CBs),
            {ok, P#port{cbs=NewCBs}}
         ; false ->
            NewCBs = dict:store(conns, {Rate, [CB],
                    ticker:start_ticker(Rate, conns_tick)}, CBs),
            {ok, P#port{cbs=NewCBs}}
    end.


add_cb_to_list(CB, Rate, {_, CBs, Pid}) ->
    {Rate, [CB|CBs], Pid}.


remove_cbs_int([], #port{cbs=CBs}=P) ->
    dict:map(fun(_, {_, _, T}) -> ticker:stop_ticker(T) end, CBs),
    {ok, P#port{cbs=dict:new()}};
remove_cbs_int(By, #port{cbs=CBs}=P) ->
    LiveCBs = dict:filter(fun(K, V) ->
                is_not_in_by(By, K, V) end, CBs),
    DeadCBs = dict:filter(fun(K, V) ->
                is_in_by(By, K, V) end, CBs),
    dict:map(fun stop_empty_timers/2, DeadCBs),
    {ok, P#port{cbs=LiveCBs}}.


stop_empty_timers(_, {_, [], Pid}) ->
    ticker:stop_ticker(Pid);
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


num_cbs_int([], #port{cbs=CBs}) ->
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs);
num_cbs_int(By, #port{cbs=CBs}) ->
    CBs2 = dict:filter(fun(K, V) -> is_in_by(By, K, V) end, CBs),
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback calling

do_conns_cb(#port{obj=O, cbs=CBs, name=Name}=Port) ->
    case dict:find(conns, CBs)
        of {ok, {_, CBList, _}} ->
            Current = Port#port.conn_profs,
            case parse_cps(O, self())
                of Current ->
                    Port
                 ; New ->
                    % Trigger the callbacks in new processes
                    lists:foreach(fun(CB) -> spawn_link(?MODULE, exec_cb,
                            [New, Current, self(), Name, CB]) end,
                        CBList),
                    Port#port{conn_profs=New}
            end
         ; error ->
            % No callbacks registered (so why is there a ticker?)
            Port
    end.


exec_cb(NewVal, OldVal, NodePid, FP, CB) ->
    CB(NewVal, OldVal, NodePid, FP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - parsing

parse_port_intf_prof(P) ->
    IN = P#'RTC_PortInterfaceProfile'.instance_name,
    TN = P#'RTC_PortInterfaceProfile'.type_name,
    #port_intf_prof{inst_name=IN, type_name=TN,
        polarity=P#'RTC_PortInterfaceProfile'.polarity}.


parse_name(N) ->
    case string:rchr(N, $.)
        of 0 ->
            N
         ; Dot ->
            string:substr(N, Dot + 1)
    end.


parse_port(P, Owner) ->
    Prof = 'RTC_PortService':get_port_profile(P),
    Intfs = lists:map(fun parse_port_intf_prof/1,
        Prof#'RTC_PortProfile'.interfaces),
    Props=nvlist:to_dict(Prof#'RTC_PortProfile'.properties),
    #port{name=parse_name(Prof#'RTC_PortProfile'.name),
        type=port_type_to_atom(dict:fetch("port.port_type", Props)),
        intfs=Intfs,
        conn_profs=parse_cps(P, self()),
        props=Props,
        owner=Owner,
        owner_obj=Prof#'RTC_PortProfile'.owner,
        obj=P,
        cbs=dict:new()}.


parse_cps(P, Owner) ->
    lists:map(fun connection:parse_conn_profile/1,
        lists:map(fun(C) -> {C, Owner} end,
            'RTC_PortService':get_connector_profiles(P))).


reparse_int(#port{owner=Owner, obj=Obj}) ->
    {ok, parse_port(Obj, Owner)}.


reparse_conns_int(#port{obj=Obj} = P) ->
    {ok, P#port{conn_profs=parse_cps(Obj, self())}}.


port_type_to_atom("DataInPort") ->
    datain;
port_type_to_atom("DataOutPort") ->
    dataout;
port_type_to_atom("CorbaPort") ->
    corba;
port_type_to_atom(_) ->
    unknown.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    rtctree:test_setup(),
    S = dict:fetch("localhost", node:get_children(rtctree:get_root())),
    D = dict:fetch("blurgle.host", node:get_children(S)),
    IC = dict:fetch("ConsoleOut0.rtc", node:get_children(D)),
    OC = dict:fetch("ConsoleIn0.rtc", node:get_children(D)),
    C1 = dict:fetch("Controller0.rtc", node:get_children(D)),
    C2 = dict:fetch("Motor0.rtc", node:get_children(D)),
    SC = dict:fetch("MyServiceProvider0.rtc", node:get_children(D)),
    SC2 = dict:fetch("MyServiceConsumer0.rtc", node:get_children(D)),
    test_in_port(IC),
    test_out_port(OC),
    test_corba_port(SC),
    test_reparse(IC),
    Ps1 = component:get_in_ports(IC),
    P1 = dict:fetch("in", Ps1),
    Ps2 = component:get_out_ports(OC),
    P2 = dict:fetch("out", Ps2),
    Ps3 = component:get_corba_ports(SC),
    P3 = dict:fetch("MyService", Ps3),
    test_reparse_conns(P1),
    test_get_conns(P1),
    test_get_conn_by_dest(P1, P2),
    test_get_conn_by_name(P1),
    test_is_connected(P1, P3),
    test_get_owner(P1, IC),
    test_get_owner_obj(P1, IC),
    test_get_intfs(P3),
    test_get_intf_by_inst_name(P3),
    test_connect_data_ports(C1, C2),
    test_connect_corba_ports(SC, SC2),
    test_disconnect_all(C2),
    test_has_port(SC),
    test_disconnect(SC),
    test_callbacks(P1),
    rtctree:test_cleanup(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

test_in_port(C) ->
    Ps = component:get_in_ports(C),
    P = dict:fetch("in", Ps),
    test_generic("in", datain, P),
    true = is_data_port(P),
    true = is_in_port(P),
    false = is_out_port(P),
    false = is_corba_port(P),
    ok.


test_out_port(C) ->
    Ps = component:get_out_ports(C),
    P = dict:fetch("out", Ps),
    test_generic("out", dataout, P),
    true = is_data_port(P),
    false = is_in_port(P),
    true = is_out_port(P),
    false = is_corba_port(P),
    ok.


test_corba_port(C) ->
    Ps = component:get_corba_ports(C),
    P = dict:fetch("MyService", Ps),
    test_generic("MyService", corba, P),
    false = is_data_port(P),
    false = is_in_port(P),
    false = is_out_port(P),
    true = is_corba_port(P),
    ok.


test_generic(N, T, P) ->
    test_get_name(N, P),
    test_get_corba_obj(P),
    test_get_type(T, P),
    test_get_props(P),
    ok.


test_get_name(N, P) ->
    N = get_name(P),
    ok.


test_get_corba_obj(P) ->
    {'IOP_IOR', _, _} = get_corba_obj(P),
    ok.


test_get_type(T, P) ->
    T = get_type(P),
    ok.


test_get_props(P) ->
    Props = get_props(P),
    true = dict:is_key("port.port_type", Props),
    ok.


test_reparse(P) ->
    N = get_name(P),
    ok = reparse(P),
    N = get_name(P),
    ok.


test_reparse_conns(P) ->
    C = get_conns(P),
    ok = reparse_conns(P),
    C = get_conns(P),
    ok.


test_get_conns(P) ->
    [#conn_prof{name="out_in",
            ports=[{'IOP_IOR', _, _}, {'IOP_IOR', _, _}]}] = get_conns(P),
    ok.


test_get_conn_by_dest(P, D) ->
    #conn_prof{name="out_in"} = get_conn_by_dest(P, D),
    #conn_prof{name="out_in"} = get_conn_by_dest(P, get_corba_obj(D)),
    ok.


test_get_conn_by_name(P) ->
    #conn_prof{name="out_in"} = get_conn_by_name(P, "out_in"),
    ok.


test_is_connected(P1, P2) ->
    true = is_connected(P1),
    false = is_connected(P2),
    ok.


test_get_owner(P, C) ->
    C = get_owner(P),
    ok.


test_get_owner_obj(P, C) ->
    CObj = node:get_corba_obj(C),
    CObj = get_owner_obj(P),
    ok.


test_get_intfs(P) ->
    [#port_intf_prof{inst_name="myservice0", type_name="MyService",
            polarity='PROVIDED'}] = get_intfs(P),
    ok.


test_get_intf_by_inst_name(P) ->
    #port_intf_prof{inst_name="myservice0", type_name="MyService",
        polarity='PROVIDED'} = get_intf_by_inst_name(P, "myservice0"),
    ok.


test_connect_data_ports(C1, C2) ->
    Ps1 = component:get_out_ports(C1),
    P1 = dict:fetch("out", Ps1),
    Ps2 = component:get_in_ports(C2),
    P2 = dict:fetch("in", Ps2),
    ok = connect(P1, P2, "", "", dict:new()),
    ok.


test_connect_corba_ports(C1, C2) ->
    Ps1 = component:get_corba_ports(C1),
    P1 = dict:fetch("MyService", Ps1),
    Ps2 = component:get_corba_ports(C2),
    P2 = dict:fetch("MyService", Ps2),
    ok = connect(P1, P2, "", "", dict:new()),
    ok.


test_disconnect_all(C) ->
    Ps = component:get_in_ports(C),
    P = dict:fetch("in", Ps),
    disconnect_all(P),
    0 = length(get_conns(P)),
    ok.


test_has_port(C) ->
    Ps = component:get_corba_ports(C),
    P = dict:fetch("MyService", Ps),
    CP = get_conn_by_name(P, "MyService_MyService"),
    true = connection:has_port(CP, P),
    true = connection:has_port(CP, get_corba_obj(P)),
    ok.


test_disconnect(C) ->
    Ps = component:get_corba_ports(C),
    P = dict:fetch("MyService", Ps),
    CP = get_conn_by_name(P, "MyService_MyService"),
    ok = connection:disconnect(CP),
    ok = reparse_conns(P),
    0 = length(get_conns(P)),
    ok.


test_callbacks(C) ->
    CB = fun(N, O, P, FP) -> io:format("Port callback: ~p~n",
                [{N, O, P, FP}]) end,
    ok = add_cb(C, CB, conns, 1000),
    io:format("Added port callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(C, [{cb, CB}]),
    ok = add_cb(C, CB, conns, 1000),
    ok = remove_cbs(C, [{type, conns}]),
    ok = add_cb(C, CB, conns, 1000),
    ok = remove_cbs(C, [{rate, 1000}]),
    ok = add_cb(C, CB, conns, 1000),
    ok = add_cb(C, CB, conns, 250),
    io:format("Added port callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(C, [{cb, CB}, {rate, 1000}]),
    io:format("Removed port callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(C, []),
    0 = num_cbs(C, []),
    io:format("Done.~n"),
    ok.


sleep(T) ->
    receive
    after
        T ->
            ok
    end.

