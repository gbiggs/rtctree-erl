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
% Configuration functionality.

-module(configuration).

% External use exports
-export([activate_set/2, get_active_set/1, get_active_set_name/1, get_set/2,
        get_sets/1, get_set_names/1, get_param/3, set_param/4]).
-export([add_cb/4, remove_cbs/2, num_cbs/2]).

% Internal use exports
-export([start/1, init/1, test/0, exec_cb/5]).
-include("include/nodes.hrl").
-include_lib("idl/SDOPackage.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - configuration functions

activate_set(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    call(Pid, self(), {activate_set, Name}).


get_active_set(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_active_set).


get_active_set_name(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_active_set_name).


get_set(Pid, Name) when is_pid(Pid) andalso is_list(Name) ->
    call(Pid, self(), {get_set, Name}).


get_sets(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_sets).


get_set_names(Pid) when is_pid(Pid) ->
    call(Pid, self(), get_set_names).


get_param(Pid, Set, Param) when
        is_pid(Pid) andalso is_list(Set) andalso is_list(Param) ->
    call(Pid, self(), {get_param, Set, Param}).


set_param(Pid, Set, Param, Value) when
        is_pid(Pid) andalso is_list(Set) andalso is_list(Param) ->
    call(Pid, self(), {set_param, Set, Param, Value}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - callback management

add_cb(Pid, CB, Type, Rate) when
        is_pid(Pid) and is_function(CB) and is_integer(Rate) and is_atom(Type) ->
    node:call(Pid, self(), {add_config_cb, CB, Type, Rate}).


% By must be a list of tuples, e.g.:
% [{cb, CB}, {rate, Rate}, {type, Type}]
% [{rate, Rate}, {type, Type}]
remove_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {remove_config_cbs, By}).


num_cbs(Pid, By) when is_pid(Pid) and is_list(By) ->
    node:call(Pid, self(), {num_config_cbs, By}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - configuration process

start(Obj) ->
    spawn_link(?MODULE, init, [Obj]).

init(Obj) ->
    CSObjs = 'SDOPackage_Configuration':get_configuration_sets(Obj),
    Cs = dict:from_list(lists:map(fun parse_config_set/1, CSObjs)),
    ActiveObj = 'SDOPackage_Configuration':get_active_configuration_set(Obj),
    {ActiveName, _} = parse_config_set(ActiveObj),
    C = #config{sets=Cs, active_set_name=ActiveName, obj=Obj, cbs=dict:new()},
    loop(C).


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


terminate(_C) ->
    ok.


loop(C) ->
    receive
        {request, From, stop} ->
            reply(From, terminate(C));
        {request, From, Msg} ->
            {Reply, NewC} = handle_msg(Msg, C),
            reply(From, Reply),
            loop(NewC);
        {oneway, Msg} ->
            NewC = handle_msg(Msg, C),
            loop(NewC);
        Other ->
            io:format("~p received unhandled message ~p~n", [C, Other]),
            loop(C)
    end.


handle_msg({activate_set, Name}, C) ->
    activate_set_int(Name, C);
handle_msg(get_active_set, C) ->
    {get_active_set_int(C), C};
handle_msg(get_active_set_name, C) ->
    {get_active_set_name_int(C), C};
handle_msg({get_set, Name}, C) ->
    {get_set_int(Name, C), C};
handle_msg(get_sets, C) ->
    {get_sets_int(C), C};
handle_msg(get_set_names, C) ->
    {get_set_names_int(C), C};
handle_msg({get_param, Set, Param}, C) ->
    {get_param_int(Set, Param, C), C};
handle_msg({set_param, Set, Param, Val}, C) ->
    set_param_int(Set, Param, Val, C);
% Callback management
handle_msg({add_config_cb, CB, T, R}, C) ->
    add_cb_int(CB, T, R, C);
handle_msg({remove_config_cbs, By}, C) ->
    remove_cbs_int(By, C);
handle_msg({num_config_cbs, By}, C) ->
    {num_cbs_int(By, C), C};
% Callback triggers
handle_msg(config_param_tick, P) ->
    do_changed_param_cb(P);
handle_msg(config_set_tick, P) ->
    do_changed_set_cb(P);
% Other
handle_msg(Msg, C) ->
    io:format("Unhandled message for CS ~p: ~p~n", [C, Msg]),
    {ok, C}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - configuration functionality

activate_set_int(Name, #config{obj=Obj, sets=Sets} = C) ->
    case dict:is_key(Name, Sets)
        of true ->
            'SDOPackage_Configuration':activate_configuration_set(Obj, Name),
            {ok, C#config{active_set_name=Name}}
         ; false ->
            {error, bad_set_name}
    end.


get_active_set_int(#config{sets=Sets, active_set_name=A}) ->
    int_set_to_ext_set(dict:fetch(A, Sets)).


get_active_set_name_int(#config{active_set_name=A}) ->
    A.


get_set_names_int(#config{sets=Sets}) ->
    NoInts = fun([$_, $_|_]) -> false; (_) -> true end,
    lists:filter(NoInts, dict:fetch_keys(Sets)).


get_sets_int(#config{sets=Sets}) ->
    NoInts = fun([$_, $_|_]) -> false; (_) -> true end,
    NameToResult = fun(N) -> int_set_to_ext_set(dict:fetch(N, Sets)) end,
    lists:map(NameToResult, lists:filter(NoInts, dict:fetch_keys(Sets))).


get_set_int(Name, #config{sets=Sets}) ->
    case dict:find(Name, Sets)
        of error ->
            {error, bad_set_name}
         ; {ok, S} ->
            {ok, int_set_to_ext_set(S)}
    end.


get_param_int(SetName, Param, #config{sets=Sets}) ->
    case dict:find(SetName, Sets)
        of error ->
            {error, bad_set_name}
         ; {ok, S} ->
            get_param_from_set(Param, S)
    end.


get_param_from_set(Param, #config_set{data=D}) ->
    case dict:find(Param, D)
        of error ->
            {error, bad_param_name}
         ; {ok, Value} ->
            {ok, Value}
    end.


set_param_int(SetName, Param, Val, #config{sets=Sets} = C) ->
    case dict:find(SetName, Sets)
        of error ->
            {error, bad_set_name}
         ; {ok, S} ->
            case set_param_in_set(Param, Val, S)
                of {error, _} = E ->
                    E
                 ; {ok, NewS} ->
                    update_remote_set(NewS, C),
                    {ok, C#config{sets=dict:store(SetName, NewS,
                                Sets)}}
            end
    end.


set_param_in_set(Param, Val, #config_set{data=D} = S) ->
    case dict:is_key(Param, D)
        of true ->
            {ok, S#config_set{data=dict:store(Param, Val, D)}}
         ; false ->
            {error, bad_param_name}
    end.


update_remote_set(#config_set{data=D, id=ID, desc=Desc},
        #config{obj=Obj}) ->
    RemoteSet = #'SDOPackage_ConfigurationSet'{id=ID, description=Desc,
        configuration_data=nvlist:from_dict(D)},
    'SDOPackage_Configuration':set_configuration_set_values(Obj, RemoteSet).


int_set_to_ext_set(#config_set{id=Id, desc=Desc, data=D}) ->
    [Id, Desc, dict:to_list(D)].


ext_set_to_int_set([Id, Desc, Data]) ->
    #config_set{id=Id, desc=Desc, data=dict:from_list(Data)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback management

add_cb_int(CB, Type, Rate, C) ->
    CBs = C#config.cbs,
    case dict:is_key(Type, CBs)
        of true ->
            {_, _, Pid} = dict:fetch(Type, CBs),
            Pid ! {new_rate, Rate},
            NewCBs = dict:update(Type, fun(V) ->
                        add_cb_to_list(CB, Rate, V) end, nil, CBs),
            {ok, C#config{cbs=NewCBs}}
         ; false ->
            NewCBs = dict:store(Type, {Rate, [CB],
                    ticker:start_ticker(Rate, type_to_tick(Type))}, CBs),
            {ok, C#config{cbs=NewCBs}}
    end.


type_to_tick(config_param) ->
    config_param_tick;
type_to_tick(config_set) ->
    config_set_tick.


add_cb_to_list(CB, Rate, {_, CBs, Pid}) ->
    {Rate, [CB|CBs], Pid}.


remove_cbs_int([], #config{cbs=CBs}=C) ->
    dict:map(fun(_, {_, _, T}) -> ticker:stop_ticker(T) end, CBs),
    {ok, C#config{cbs=dict:new()}};
remove_cbs_int(By, #config{cbs=CBs}=C) ->
    LiveCBs = dict:filter(fun(K, V) ->
                is_not_in_by(By, K, V) end, CBs),
    DeadCBs = dict:filter(fun(K, V) ->
                is_in_by(By, K, V) end, CBs),
    dict:map(fun stop_empty_timers/2, DeadCBs),
    {ok, C#config{cbs=LiveCBs}}.


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


num_cbs_int([], #config{cbs=CBs}) ->
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs);
num_cbs_int(By, #config{cbs=CBs}) ->
    CBs2 = dict:filter(fun(K, V) -> is_in_by(By, K, V) end, CBs),
    dict:fold(fun(_, {_, CBList, _}, _) -> length(CBList) end, 0, CBs2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - callback calling

do_changed_param_cb(#config{obj=O, cbs=CBs, sets=S}=C) ->
    case dict:find(config_param, CBs)
        of {ok, {_, CBList, _}} ->
            NewSets = dict:from_list(lists:map(fun parse_config_set/1,
                    'SDOPackage_Configuration':get_configuration_sets(O))),
            case find_changes(S, NewSets)
                of [] ->
                    C
                 ; Changed ->
                    lists:foreach(fun({Old, New}) -> exec_cbs(CBList,
                                    New, Old) end,
                        Changed),
                    C#config{sets=NewSets}
            end
         ; error ->
            % No callbacks registered (so why is there a ticker?)
            C
    end.


do_changed_set_cb(#config{obj=O, cbs=CBs, active_set_name=ASN}=C) ->
    case dict:find(config_set, CBs)
        of {ok, {_, CBList, _}} ->
            NewAS = 'SDOPackage_Configuration':get_active_configuration_set(O),
            {NewASN, _} = parse_config_set(NewAS),
            case NewASN
                of ASN ->
                    C
                 ; _ ->
                    % Trigger the callbacks in new processes
                    lists:foreach(fun(CB) -> spawn_link(?MODULE, exec_cb,
                            [NewASN, ASN, self(), nil, CB]) end, CBList),
                    C#config{active_set_name=NewASN}
            end
         ; error ->
            % No callbacks registered (so why is there a ticker?)
            C
    end.


exec_cbs(CBList, New, Old) ->
    lists:foreach(fun(CB) -> spawn_link(?MODULE, exec_cb,
                    [New, Old, self(), nil, CB]) end, CBList).


exec_cb(NewVal, OldVal, NodePid, FP, CB) ->
    CB(NewVal, OldVal, NodePid, FP).


find_changes(S, NS) ->
    find_changes1(dict:to_list(S), dict:to_list(NS), []).

find_changes1([], _, Acc) ->
    Acc;
find_changes1([S|S_T], NS, Acc) ->
    case find_match(S, NS)
        of error ->
            find_changes1(S_T, NS, [{S, nil}|Acc])
         ; Match ->
            case Match
                of S ->
                    find_changes1(S_T, NS, Acc)
                 ; _ ->
                    find_changes1(S_T, NS, [{S, Match}|Acc])
            end
    end.


find_match(_, []) ->
    error;
find_match({ID, _}, [{ID, _}=Match|_]) ->
    Match;
find_match(S, [_|T]) ->
    find_match(S, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - parsing

parse_config_set(C) ->
    {C#'SDOPackage_ConfigurationSet'.id,
        #config_set{id=C#'SDOPackage_ConfigurationSet'.id,
            desc=C#'SDOPackage_ConfigurationSet'.description,
            data=nvlist:to_dict(C#'SDOPackage_ConfigurationSet'.configuration_data)}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    rtctree:test_setup(),
    S = dict:fetch("localhost", node:get_children(rtctree:get_root())),
    D1 = dict:fetch("blurgle.host", node:get_children(S)),
    C = dict:fetch("ConfigSample0.rtc", node:get_children(D1)),
    Config = component:get_config(C),
    test_active_set(Config),
    test_active_set_name(Config),
    test_set_names(Config),
    test_sets(Config),
    test_get_param(Config),
    test_set_param(Config),
    test_activate_set(Config),
    test_callbacks(Config),
    rtctree:test_cleanup(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

test_active_set(C) ->
    ["default", "", Params] = get_active_set(C),
    true = lists:member({"int_param0", "0"}, Params),
    true = lists:member({"int_param1", "1"}, Params),
    true = lists:member({"str_param0", "default"}, Params),
    true = lists:member({"str_param1", "default set in conf file"}, Params),
    true = lists:member({"double_param0", "0.99"}, Params),
    true = lists:member({"double_param1", "-0.99"}, Params),
    true = lists:member({"vector_param0", "0.0,0.1,0.2,0.3,0.4,0.5,0.6"},
        Params),
    ok.


test_active_set_name(C) ->
    "default" = get_active_set_name(C),
    ok.


test_sets(C) ->
    Sets = get_sets(C),
    IsSet = fun(N) -> fun (S, Acc) -> case S of [N, _, _] -> true;
                        _ -> Acc end end end,
    true = lists:foldl(IsSet("default"), false, Sets),
    true = lists:foldl(IsSet("mode0"), false, Sets),
    true = lists:foldl(IsSet("mode1"), false, Sets),
    ok.


test_set_names(C) ->
    Names = get_set_names(C),
    true = lists:member("default", Names),
    true = lists:member("mode0", Names),
    true = lists:member("mode1", Names),
    ok.


test_get_param(C) ->
    {ok, "1"} = get_param(C, "default", "int_param1"),
    ok.


test_set_param(C) ->
    ok = set_param(C, "default", "int_param1", "42"),
    {ok, "42"} = get_param(C, "default", "int_param1"),
    ok.


test_activate_set(C) ->
    ok = activate_set(C, "mode0"),
    ["mode0", "", Params] = get_active_set(C),
    true = lists:member({"int_param0", "12345"}, Params),
    true = lists:member({"int_param1", "98765"}, Params),
    true = lists:member({"str_param0", "mode0"}, Params),
    true = lists:member({"str_param1", "foo"}, Params),
    true = lists:member({"double_param0", "3.141592653589793238462643383279"}, Params),
    true = lists:member({"double_param1", "2.718281828459045235360287471352"}, Params),
    true = lists:member({"vector_param0", "0.0,0.1,0.2,0.3,0.4"}, Params),
    ok.


test_callbacks(C) ->
    CB = fun(N, O, P, FP) -> io:format("Config param change callback: ~p~n",
                [{N, O, P, FP}]) end,
    CB2 = fun(N, O, P, FP) -> io:format("Config set change callback: ~p~n",
                [{N, O, P, FP}]) end,
    ok = add_cb(C, CB, config_param, 1000),
    io:format("Added config callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(C, [{cb, CB}]),
    ok = add_cb(C, CB, config_param, 1000),
    ok = remove_cbs(C, [{type, config_param}]),
    ok = add_cb(C, CB, config_param, 1000),
    ok = remove_cbs(C, [{rate, 1000}]),
    ok = add_cb(C, CB, config_param, 1000),
    ok = add_cb(C, CB2, config_set, 250),
    io:format("Added config callback; sleeping for 5s.~n"),
    sleep(5000),
    ok = remove_cbs(C, [{cb, CB}, {rate, 1000}]),
    io:format("Removed config param callback; sleeping for 5s.~n"),
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

