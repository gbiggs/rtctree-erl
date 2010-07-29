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
% Execution context functionality.

-module(exec_context).

% External use exports
-export([activate_comp/2, deactivate_comp/2, reset_comp/2, get_comp_state/2,
        get_kind/1, get_kind_as_string/1, is_running/1, get_corba_obj/1,
        get_handle/1, get_owner/1, get_owner_obj/1, get_owner_name/1,
        get_participants/1, get_participant_names/1, get_rate/1,
        get_properties/1, reparse/1, stop/1]).
-export([add_cb/4, remove_cbs/2, num_cbs/2]).

% Internal use exports
-export([start/1, init/1, test/0, exec_cb/5]).
-include("include/nodes.hrl").
-include_lib("idl/RTC.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - execution context functions

activate_comp(Pid, Comp) when is_pid(Pid) ->
    call(Pid, self(), {activate_comp, Comp}).


deactivate_comp(Pid, Comp) when is_pid(Pid) ->
    call(Pid, self(), {deactivate_comp, Comp}).


reset_comp(Pid, Comp) when is_pid(Pid) ->
    call(Pid, self(), {reset_comp, Comp}).


get_comp_state(Pid, Comp) when is_pid(Pid) ->
    call(Pid, self(), {get_comp_state, Comp}).


get_kind(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_kind).


get_kind_as_string(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_kind_as_string).


is_running(Pid) when is_pid(Pid) ->
    call(Pid, self(), is_running).


get_corba_obj(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_corba_obj).


get_handle(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_handle).


get_owner(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_owner).


get_owner_obj(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_owner_obj).


get_owner_name(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_owner_name).


get_participants(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_parts).


get_participant_names(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_part_names).


get_rate(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_rate).


get_properties(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_props).


reparse(Pid) when is_pid(Pid) ->
    call(Pid, self(), reparse).


stop(Pid) when is_pid(Pid) ->
    call(Pid, self(), stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - callback management

add_cb(Pid, CB, Type, Rate) when
        is_pid(Pid) and is_function(CB) and is_integer(Rate) and is_atom(Type) ->
    node:call(Pid, self(), {add_ec_cb, CB, Type, Rate}).


% By must be a list of tuples, e.g.:
% [{cb, CB}, {rate, Rate}, {type, Type}]
% [{rate, Rate}, {type, Type}]
remove_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {remove_ec_cbs, By}).


num_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {num_ec_cbs, By}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - execution context process

start(EC) ->
    spawn_link(?MODULE, init, [EC]).

init({Obj, Handle, Owner}) ->
    Prof = 'RTC_ExecutionContextService':get_profile(Obj),
    EC = #exec_context{handle=Handle,
        running='RTC_ExecutionContextService':is_running(Obj),
        kind=Prof#'RTC_ExecutionContextProfile'.kind,
        rate=Prof#'RTC_ExecutionContextProfile'.rate,
        owner=Owner,
        owner_obj=Prof#'RTC_ExecutionContextProfile'.owner,
        parts=Prof#'RTC_ExecutionContextProfile'.participants,
        props=nvlist:to_dict(Prof#'RTC_ExecutionContextProfile'.properties),
        obj=Obj,
        cbs=dict:new()},
    loop(EC).


reparse_int(#exec_context{handle=Handle, owner=PriorOwner,
        owner_obj=PriorOwnerObj, obj=Obj, cbs=CBs}) ->
    Prof = 'RTC_ExecutionContextService':get_profile(Obj),
    {Owner, OwnerObj} = update_owner(PriorOwner, PriorOwnerObj,
        Prof#'RTC_ExecutionContextProfile'.owner),
    EC = #exec_context{handle=Handle,
        running='RTC_ExecutionContextService':is_running(Obj),
        kind=Prof#'RTC_ExecutionContextProfile'.kind,
        rate=Prof#'RTC_ExecutionContextProfile'.rate,
        owner=Owner,
        owner_obj=OwnerObj,
        parts=Prof#'RTC_ExecutionContextProfile'.participants,
        props=nvlist:to_dict(Prof#'RTC_ExecutionContextProfile'.properties),
        obj=Obj,
        cbs=CBs},
    {ok, EC}.


update_owner(Owner, OwnerObj, OwnerObj) ->
    {Owner, OwnerObj};
update_owner(_, _, NewOwnerObj) ->
    {nil, NewOwnerObj}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - process

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


terminate(_EC) ->
    ok.


loop(EC) ->
    receive
        {request, From, stop} ->
            reply(From, terminate(EC));
        {request, From, Msg} ->
            {Reply, NewP} = handle_msg(Msg, EC),
            reply(From, Reply),
            loop(NewP);
        {oneway, Msg} ->
            NewEC = handle_msg(Msg, EC),
            loop(NewEC);
        Other ->
            io:format("~p received unhandled message ~p~n", [EC, Other]),
            loop(EC)
    end.


handle_msg({activate_comp, Comp}, EC) ->
    {activate_comp_int(Comp, EC), EC};
handle_msg({deactivate_comp, Comp}, EC) ->
    {deactivate_comp_int(Comp, EC), EC};
handle_msg({reset_comp, Comp}, EC) ->
    {reset_comp_int(Comp, EC), EC};
handle_msg({get_comp_state, Comp}, EC) ->
    {get_comp_state_int(Comp, EC), EC};
handle_msg(get_kind, #exec_context{kind=Kind} = EC) ->
    {Kind, EC};
handle_msg(get_kind_as_string, #exec_context{kind=Kind} = EC) ->
    {kind_to_string(Kind), EC};
handle_msg(is_running, #exec_context{running=R} = EC) ->
    {R, EC};
handle_msg(get_corba_obj, #exec_context{obj=O} = EC) ->
    {O, EC};
handle_msg(get_handle, #exec_context{handle=H} = EC) ->
    {H, EC};
handle_msg(get_owner, #exec_context{owner=O} = EC) ->
    {O, EC};
handle_msg(get_owner_obj, #exec_context{owner_obj=O} = EC) ->
    {O, EC};
handle_msg(get_owner_name, #exec_context{owner_obj=O} = EC) ->
    {get_comp_obj_name(O), EC};
handle_msg(get_parts, #exec_context{parts=P} = EC) ->
    {P, EC};
handle_msg(get_part_names, #exec_context{parts=P} = EC) ->
    {lists:map(fun get_comp_obj_name/1, P), EC};
handle_msg(get_rate, #exec_context{rate=R} = EC) ->
    {R, EC};
handle_msg(get_props, #exec_context{props=P} = EC) ->
    {P, EC};
% Callback management
handle_msg({add_ec_cb, CB, T, R}, EC) ->
    add_cb_int(CB, T, R, EC);
handle_msg({remove_ec_cbs, By}, EC) ->
    remove_cbs_int(By, EC);
handle_msg({num_ec_cbs, By}, EC) ->
    {num_cbs_int(By, EC), EC};
% Callback triggers
handle_msg(ec_running_tick, EC) ->
    do_ec_running_cb(EC);
handle_msg(ec_rate_tick, EC) ->
    do_ec_rate_cb(EC);
% Other
handle_msg(reparse, EC) ->
    reparse_int(EC);
handle_msg(Msg, EC) ->
    io:format("Unhandled message for EC ~p: ~p~n", [EC, Msg]),
    {ok, EC}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - execution context functionality

activate_comp_int(C, #exec_context{obj=Obj}) ->
    case 'RTC_ExecutionContextService':activate_component(Obj, C)
        of 'RTC_OK' ->
            ok
         ; 'PRECONDITION_NOT_MET' ->
            {error, precondition_not_met}
    end.


deactivate_comp_int(C, #exec_context{obj=Obj}) ->
    case 'RTC_ExecutionContextService':deactivate_component(Obj, C)
        of 'RTC_OK' ->
            ok
         ; 'PRECONDITION_NOT_MET' ->
            {error, precondition_not_met}
    end.


reset_comp_int(C, #exec_context{obj=Obj}) ->
    case 'RTC_ExecutionContextService':reset_component(Obj, C)
        of 'RTC_OK' ->
            ok
         ; 'PRECONDITION_NOT_MET' ->
            {error, precondition_not_met}
    end.


get_comp_state_int(C, #exec_context{obj=Obj}) ->
    'RTC_ExecutionContextService':get_component_state(Obj, C).


kind_to_string('PERIODIC') ->
    "Periodic";
kind_to_string('EVENT_DRIVEN') ->
    "Event-driven";
kind_to_string('OTHER') ->
    "Other".


get_comp_obj_name({'IOP_IOR', [], []}) ->
    "";
get_comp_obj_name(Obj) ->
    P = 'RTC_RTObject':get_component_profile(Obj),
    P#'RTC_ComponentProfile'.instance_name.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback management

add_cb_int(CB, Type, Rate, #exec_context{cbs=CBs}=EC) ->
    case dict:is_key(Type, CBs)
        of true ->
            {_, _, Pid} = dict:fetch(Type, CBs),
            Pid ! {new_rate, Rate},
            NewCBs = dict:update(Type, fun(V) ->
                        add_cb_to_list(CB, Rate, V) end, nil, CBs),
            {ok, EC#exec_context{cbs=NewCBs}}
         ; false ->
            NewCBs = dict:store(Type, {Rate, [CB],
                    ticker:start_ticker(Rate, ec_cb_tick(Type))}, CBs),
            {ok, EC#exec_context{cbs=NewCBs}}
    end.


ec_cb_tick(ec_running) ->
    ec_running_tick;
ec_cb_tick(ec_rate) ->
    ec_rate_tick.


add_cb_to_list(CB, Rate, {_, CBs, Pid}) ->
    {Rate, [CB|CBs], Pid}.


remove_cbs_int([], #exec_context{cbs=CBs}=EC) ->
    dict:map(fun(_, {_, _, T}) -> ticker:stop_ticker(T) end, CBs),
    {ok, EC#exec_context{cbs=dict:new()}};
remove_cbs_int(By, #exec_context{cbs=CBs}=EC) ->
    LiveCBs = dict:filter(fun(K, V) ->
                is_not_in_by(By, K, V) end, CBs),
    DeadCBs = dict:filter(fun(K, V) ->
                is_in_by(By, K, V) end, CBs),
    dict:map(fun stop_empty_timers/2, DeadCBs),
    {ok, EC#exec_context{cbs=LiveCBs}}.


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


num_cbs_int([], #exec_context{cbs=CBs}) ->
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs);
num_cbs_int(By, #exec_context{cbs=CBs}) ->
    CBs2 = dict:filter(fun(K, V) -> is_in_by(By, K, V) end, CBs),
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback calling

do_ec_running_cb(#exec_context{obj=O, cbs=CBs, handle=Handle,
        running=Current}=EC) ->
    case dict:find(ec_running, CBs)
        of {ok, {_, CBList, _}} ->
            case 'RTC_ExecutionContextService':is_running(O)
                of Current ->
                    EC
                 ; New ->
                    % Trigger the callbacks in new processes
                    lists:foreach(fun(CB) -> spawn_link(?MODULE, exec_cb,
                            [New, Current, self(), Handle, CB]) end,
                        CBList),
                    EC#exec_context{running=New}
            end
         ; error ->
            % No callbacks registered (so why is there a ticker?)
            EC
    end.


do_ec_rate_cb(#exec_context{obj=O, cbs=CBs, handle=Handle,
        rate=Current}=EC) ->
    case dict:find(ec_rate, CBs)
        of {ok, {_, CBList, _}} ->
            Prof = 'RTC_ExecutionContextService':get_profile(O),
            case Prof#'RTC_ExecutionContextProfile'.rate
                of Current ->
                    EC
                 ; New ->
                    % Trigger the callbacks in new processes
                    lists:foreach(fun(CB) -> spawn_link(?MODULE, exec_cb,
                            [New, Current, self(), Handle, CB]) end,
                        CBList),
                    EC#exec_context{running=New}
            end
         ; error ->
            % No callbacks registered (so why is there a ticker?)
            EC
    end.


exec_cb(NewVal, OldVal, NodePid, FP, CB) ->
    CB(NewVal, OldVal, NodePid, FP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    rtctree:test_setup(),
    S = dict:fetch("localhost", node:get_children(rtctree:get_root())),
    D = dict:fetch("blurgle.host", node:get_children(S)),
    C = dict:fetch("ConsoleOut0.rtc", node:get_children(D)),
    CompObj = node:get_corba_obj(C),
    [EC] = component:get_owned_ecs(C),
    test_comp_state(CompObj, EC),
    test_activate(CompObj, EC),
    test_deactivate(CompObj, EC),
    %test_reset(CompObj, EC),
    test_get_kind(EC),
    test_get_kind_as_string(EC),
    test_is_running(EC),
    test_get_handle(EC),
    test_get_owner(EC, C),
    test_get_owner_obj(EC),
    test_get_owner_name(EC),
    test_get_parts(EC),
    test_get_part_names(EC),
    test_get_rate(EC),
    test_get_props(EC),
    test_reparse(EC),
    test_get_corba_obj(EC),
    test_callbacks(EC),
    rtctree:test_cleanup(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests


test_activate(C, EC) ->
    ok = activate_comp(EC, C),
    timer:sleep(1000),
    'ACTIVE_STATE' = get_comp_state(EC, C),
    ok.


test_deactivate(C, EC) ->
    ok = deactivate_comp(EC, C),
    timer:sleep(1000),
    'INACTIVE_STATE' = get_comp_state(EC, C),
    ok.


test_reset(C, EC) ->
    ok = reset_comp(EC, C),
    timer:sleep(1000),
    'INACTIVE_STATE' = get_comp_state(EC, C),
    ok.


test_comp_state(C, EC) ->
    'INACTIVE_STATE' = get_comp_state(EC, C),
    ok.


test_get_kind(EC) ->
    'PERIODIC' = get_kind(EC),
    ok.


test_get_kind_as_string(EC) ->
    "Periodic" = get_kind_as_string(EC),
    ok.


test_is_running(EC) ->
    true = is_running(EC),
    ok.


test_get_handle(EC) ->
    0 = get_handle(EC),
    ok.


test_get_owner(EC, C) ->
    C = get_owner(EC),
    ok.


test_get_owner_obj(EC) ->
    {'IOP_IOR', _, _} = get_owner_obj(EC),
    ok.


test_get_owner_name(EC) ->
    "ConsoleOut0" = get_owner_name(EC),
    ok.


test_get_parts(EC) ->
    [] = get_participants(EC),
    ok.


test_get_part_names(EC) ->
    [] = get_participant_names(EC),
    ok.


test_get_rate(EC) ->
    1000.0 = get_rate(EC),
    ok.


test_get_props(EC) ->
    Props = dict:new(),
    Props = get_properties(EC),
    ok.


test_reparse(EC) ->
    {H, Ru, K, Ra, O, OO, Pa, Pr} = {get_handle(EC),
                                     is_running(EC),
                                     get_kind(EC),
                                     get_rate(EC),
                                     get_owner(EC),
                                     get_owner_obj(EC),
                                     get_participants(EC),
                                     get_properties(EC)},
    reparse(EC),
    {H, Ru, K, Ra, O, OO, Pa, Pr} = {get_handle(EC),
                                     is_running(EC),
                                     get_kind(EC),
                                     get_rate(EC),
                                     get_owner(EC),
                                     get_owner_obj(EC),
                                     get_participants(EC),
                                     get_properties(EC)},
    ok.


test_get_corba_obj(EC) ->
    {'IOP_IOR', _, _} = get_corba_obj(EC),
    ok.


test_callbacks(EC) ->
    CB = fun(N, O, P, H) -> io:format("Execution context callback: ~p~n",
                [{N, O, P, H}]) end,
    ok = add_cb(EC, CB, ec_running, 1000),
    io:format("Added EC callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(EC, [{cb, CB}]),
    ok = add_cb(EC, CB, ec_running, 1000),
    ok = remove_cbs(EC, [{type, ec_running}]),
    ok = add_cb(EC, CB, ec_running, 1000),
    ok = remove_cbs(EC, [{rate, 1000}]),
    ok = add_cb(EC, CB, ec_running, 1000),
    ok = add_cb(EC, CB, ec_rate, 250),
    io:format("Added EC callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(EC, [{cb, CB}, {rate, 1000}]),
    0 = num_cbs(EC, []),
    io:format("Removed EC callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(EC, []),
    0 = num_cbs(EC, []),
    io:format("Done.~n"),
    ok.


sleep(T) ->
    receive
    after
        T ->
            ok
    end.

