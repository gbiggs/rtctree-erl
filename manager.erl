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
% Manager functionality.

-module(manager).

% External use exports
-export([create_comp/2, del_comp/2, load_mod/3, unload_mod/2, get_fact_profs/1,
        get_loadable_mods/1, get_loaded_mods/1, get_profile/1, is_master/1,
        get_config/1, set_config_param/3, get_masters/1, add_master/2,
        remove_master/2, reparse/1, reparse_children/1]).
-export([fork/1, shutdown/1, restart/1]).

% Internal use exports
-export([start/4, init/1, terminate/1, handle_msg/2, test/0]).
-include("include/nodes.hrl").
-include_lib("idl/RTC.hrl").
-include_lib("idl/RTM.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Manager functions

create_comp(Pid, ModName) when is_pid(Pid) andalso is_list(ModName) ->
    node:call(Pid, self(), {create_comp, ModName}).


del_comp(Pid, InstName) when is_pid(Pid) andalso is_list(InstName) ->
    node:call(Pid, self(), {del_comp, InstName}).


load_mod(Pid, Path, InitFunc) when
        is_pid(Pid) andalso is_list(Path) andalso is_list(InitFunc) ->
    node:call(Pid, self(), {load_mod, Path, InitFunc}).


unload_mod(Pid, Path) when is_pid(Pid) andalso is_list(Path) ->
    node:call(Pid, self(), {unload_mod, Path}).


get_fact_profs(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_fact_profs).


get_loadable_mods(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_loadable_mods).


get_loaded_mods(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_loaded_mods).


get_profile(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_profile).


is_master(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), is_master).


get_config(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_config).


set_config_param(Pid, Param, Value) when is_pid(Pid) andalso is_list(Param) ->
    node:call(Pid, self(), {set_config_param, Param, Value}).


get_masters(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), get_masters).


add_master(Pid, {'IOP_IOR', _, _} = Mgr) when is_pid(Pid) ->
    node:call(Pid, self(), {add_master, Mgr}).


remove_master(Pid, {'IOP_IOR', _, _} = Mgr) when is_pid(Pid) ->
    node:call(Pid, self(), {remove_master, Mgr}).


reparse(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), reparse).


reparse_children(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), reparse_children).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - Undocumented functions - probably not safe to use

fork(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), fork).


shutdown(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), shutdown).


restart(Pid) when is_pid(Pid) ->
    node:call(Pid, self(), restart).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal API - Manager process

start(ID, Obj, PP, Depth) ->
    node:start(fun init/1, [ID, Obj, PP, Depth], fun terminate/1,
        fun handle_msg/2, self()).


init([ID, Obj, PP, Depth]) ->
    NodeName = lists:concat([ID, ".mgr"]),
    FP = lists:append(PP, [NodeName]),
    C = parse_children(Obj, Depth, FP),
    Data = parse_manager(Obj),
    #node{name=NodeName, fullpath=FP, depth=Depth, type=manager,
        children=C, obj=Obj, data=Data}.


terminate(Node) ->
    node:stop_children(Node),
    ok.


handle_msg({create_comp, MN}, Node) ->
    create_comp_int(MN, Node);
handle_msg({del_comp, IN}, Node) ->
    del_comp_int(IN, Node);
handle_msg({load_mod, MP, IF}, Node) ->
    load_mod_int(MP, IF, Node);
handle_msg({unload_mod, MP}, Node) ->
    unload_mod_int(MP, Node);
handle_msg(get_fact_profs, #node{data=#manager{fact_profs=FP}} = Node) ->
    {FP, Node};
handle_msg(get_loadable_mods, #node{data=#manager{loadable_mods=M}} = Node) ->
    {M, Node};
handle_msg(get_loaded_mods, #node{data=#manager{loaded_mods=M}} = Node) ->
    {M, Node};
handle_msg(get_profile, #node{data=#manager{profile=P}} = Node) ->
    {P, Node};
handle_msg(is_master, #node{obj=Obj} = Node) ->
    {'RTM_Manager':is_master(Obj), Node};
handle_msg(get_config, #node{obj=Obj} = Node) ->
    {get_config_int(Obj), Node};
handle_msg({set_config_param, P, V}, #node{obj=Obj} = Node) ->
    {set_config_param_int(P, V, Obj), Node};
handle_msg(get_masters, #node{data=#manager{masters=M}} = Node) ->
    {M, Node};
handle_msg({add_master, Mgr}, Node) ->
    add_master_int(Mgr, Node);
handle_msg({remove_master, Mgr}, Node) ->
    remove_master_int(Mgr, Node);
handle_msg(fork, #node{obj=Obj} = Node) ->
    {fork_int(Obj), Node};
handle_msg(shutdown, #node{obj=Obj} = Node) ->
    {shutdown_int(Obj), Node};
handle_msg(restart, #node{obj=Obj} = Node) ->
    {restart_int(Obj), Node};
handle_msg(reparse_children, Node) ->
    reparse_children_int(Node);
handle_msg(reparse, Node) ->
    reparse_int(Node).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - Manager functionality

create_comp_int(ModName, #node{obj=Obj} = Node) ->
    case 'RTM_Manager':create_component(Obj, ModName)
        of {'IOP_IOR', _, _} = NewC ->
            add_child(NewC, Node)
         ; _ ->
            {error, Node}
    end.


del_comp_int(InstName, #node{obj=Obj} = Node) ->
    case 'RTM_Manager':delete_component(Obj, InstName)
        of 'RTC_OK' ->
            reparse_children_int(Node)
         ; _ ->
             {error, Node}
    end.


load_mod_int(ModPath, InitFunc, #node{obj=Obj} = Node) ->
    case 'RTM_Manager':load_module(Obj, ModPath, InitFunc)
        of 'RTC_OK' ->
            reparse_loaded_mods_int(Node)
         ; _ ->
            {error, Node}
    end.


unload_mod_int(ModPath, #node{obj=Obj} = Node) ->
    try
        case 'RTM_Manager':unload_module(Obj, ModPath)
            of 'RTC_OK' ->
                reparse_loaded_mods_int(Node)
             ; _ ->
                {error, Node}
        end
    catch
        throw:{'EXCEPTION', {'UNKNOWN', _, _, _}} ->
            {{error, corba_unknown}, Node}
    end.


get_config_int(Obj) ->
    nvlist:to_dict('RTM_Manager':get_configuration(Obj)).


set_config_param_int(P, V, Obj) ->
    case 'RTM_Manager':set_configuration(Obj, P, V)
        of 'RTC_OK' ->
            ok
         ; _ ->
            error
    end.


add_master_int(Mgr, #node{obj=Obj} = Node) ->
    'RTM_Manager':add_master_manager(Obj, Mgr),
    reparse_masters(Node).


remove_master_int(Mgr, #node{obj=Obj} = Node) ->
    'RTM_Manager':remove_master_manager(Obj, Mgr),
    reparse_masters(Node).


fork_int(Obj) ->
    'RTM_Manager':fork(Obj).


shutdown_int(Obj) ->
    'RTM_Manager':shutdown(Obj).


restart_int(Obj) ->
    'RTM_Manager':restart(Obj).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions - parsing

parse_manager(Obj) ->
    ProfR = 'RTM_Manager':get_profile(Obj),
    Prof = nvlist:to_dict(ProfR#'RTM_ManagerProfile'.properties),
    FactProfRs = 'RTM_Manager':get_factory_profiles(Obj),
    Map = fun(P) -> nvlist:to_dict(P#'RTM_ModuleProfile'.properties) end,
    FactProfs = lists:map(Map, FactProfRs),
    LoadableModRs = 'RTM_Manager':get_loadable_modules(Obj),
    LoadableMods = lists:map(Map, LoadableModRs),
    LoadedModRs = 'RTM_Manager':get_loaded_modules(Obj),
    LoadedMods = lists:map(Map, LoadedModRs),
    Masters = 'RTM_Manager':get_master_managers(Obj),
    #manager{config=nvlist:to_dict('RTM_Manager':get_configuration(Obj)),
        profile=Prof,
        fact_profs=FactProfs,
        loadable_mods=LoadableMods,
        loaded_mods=LoadedMods,
        masters=Masters,
        obj=Obj}.


parse_children(Obj, Depth, PP) ->
    CompChildren = 'RTM_Manager':get_components(Obj),
    CCList = lists:map(fun(C) -> parse_comp_child(C, Depth, PP) end,
        CompChildren),
    CCDict = dict:from_list(CCList),
    MgrChildren = 'RTM_Manager':get_slave_managers(Obj),
    MCList = lists:map(fun(M) -> parse_mgr_child(M, Depth, PP) end,
        MgrChildren),
    MCDict = dict:from_list(MCList),
    dict:merge(fun(_, C1, _) -> C1 end, CCDict, MCDict).


parse_comp_child(C, Depth, PP) ->
    Prof = 'RTC_RTObject':get_component_profile(C),
    Name = Prof#'RTC_ComponentProfile'.instance_name,
    {Name, component:start(Name, C, PP, Depth)}.


parse_mgr_child(M, Depth, PP) ->
    Prof = 'RTM_Manager':get_profile(M),
    Props = nvlist:to_dict(Prof#'RTM_ManagerProfile'.properties),
    Name = dict:fetch("name", Props),
    {Name, manager:start(Name, M, PP, Depth)}.


add_child(C, #node{depth=Depth, fullpath=PP, children=Children} = Node) ->
    {Name, NewC} = parse_comp_child(C, Depth, PP),
    {ok, Node#node{children=dict:store(Name, NewC, Children)}}.


reparse_int(#node{depth=Depth, fullpath=FP, obj=Obj} = Node) ->
    terminate(Node),
    {ok, Node#node{data=parse_manager(Obj),
            children=parse_children(Obj, Depth, FP)}}.


reparse_children_int(#node{obj=Obj, depth=Depth, fullpath=PP} = Node) ->
    node:stop_children(Node),
    {ok, Node#node{children=parse_children(Obj, Depth, PP)}}.


reparse_loaded_mods_int(#node{obj=Obj, data=M} = Node) ->
    Map = fun(P) -> nvlist:to_dict(P#'RTM_ModuleProfile'.properties) end,
    LoadedModRs = 'RTM_Manager':get_loaded_modules(Obj),
    LoadedMods = lists:map(Map, LoadedModRs),
    {ok, Node#node{data=M#manager{loaded_mods=LoadedMods}}}.

reparse_masters(#node{obj=Obj, data=M} = Node) ->
    {ok, Node#node{data=M#manager{masters='RTM_Manager':get_master_managers(Obj)}}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    rtctree:test_setup(),
    S = dict:fetch("localhost", node:get_children(rtctree:get_root())),
    D = dict:fetch("stellvia.host_cxt", node:get_children(S)),
    M = dict:fetch("manager.mgr", node:get_children(D)),
    test_is_master(M),
    test_reparse(M),
    test_reparse_children(M),
    test_get_fact_profs(M),
    test_get_config(M),
    test_set_config(M),
    test_get_profile(M),
    test_get_loadable_mods(M),
    test_get_loaded_mods(M),
    test_get_masters(M),
    test_create_comp(M),
    test_del_comp(M),
    test_load_mod(M),
    test_unload_mod(M),
    rtctree:test_cleanup(),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

test_is_master(M) ->
    true = is_master(M),
    ok.


test_reparse(M) ->
    Conf = get_config(M),
    ok = reparse(M),
    Conf = get_config(M),
    ok.


test_reparse_children(M) ->
    L = dict:size(node:get_children(M)),
    ok = reparse_children(M),
    L = dict:size(node:get_children(M)),
    ok.


test_get_fact_profs(M) ->
    [FP|_] = get_fact_profs(M),
    true = dict:is_key("version", FP),
    ok.


test_get_config(M) ->
    Conf = get_config(M),
    true = dict:is_key("manager.name", Conf),
    ok.


test_set_config(M) ->
    set_config_param(M, "manager.modules.config_path", "/tmp"),
    Conf = get_config(M),
    "/tmp" = dict:fetch("manager.modules.config_path", Conf),
    ok.


test_get_profile(M) ->
    Prof = get_profile(M),
    true = dict:is_key("name", Prof),
    ok.


test_get_loadable_mods(M) ->
    [Mod|_] = get_loadable_mods(M),
    true = dict:is_key("module_file_name", Mod),
    ok.


test_get_loaded_mods(M) ->
    [Mod|_] = get_loaded_mods(M),
    true = dict:is_key("file_path", Mod),
    ok.


test_get_masters(M) ->
    [] = get_masters(M),
    ok.


test_create_comp(M) ->
    ok = create_comp(M, "Controller?instance_name=CreatedComp"),
    true = dict:is_key("CreatedComp", node:get_children(M)),
    ok.


test_del_comp(M) ->
    ok = del_comp(M, "CreatedComp"),
    %% TODO this is a bug in the manager - it doesn't delete children at the
    %% moment. Fix that bug, then change true to false.
    true = dict:is_key("CreatedComp", node:get_children(M)),
    ok.


test_load_mod(M) ->
    L = length(get_loaded_mods(M)),
    ok = load_mod(M, "SeqOut.so", "SeqOutInit"),
    NewL = L + 1,
    NewL = length(get_loaded_mods(M)),
    ok.


test_unload_mod(M) ->
    {error, corba_unknown} = unload_mod(M, "bluglesplurt"),
    L = length(get_loaded_mods(M)),
    ok = unload_mod(M, "/home/geoff/share/OpenRTM-aist/examples/rtcs//SeqOut.so"),
    NewL = L - 1,
    NewL = length(get_loaded_mods(M)),
    ok.

