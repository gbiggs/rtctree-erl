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
% Records defining nodes.

% Node internal state record.
% name: Name of the node (i.e. its path entry).
% fullpath: Full path up to this point, as a list of names.
% depth: Integer depth of this node.
% type: Type of the node. One of
%       directory | nameserver | manager | component | unknown
% obj: The IOR reference to the object this node represents.
% data: Additional node data. If no data, will be nil.
% cleanup: Clean-up callback function.
% handler: Message handler callback function.
% parent: PID of the parent node, if any.
% children: Dictionary of child nodes; node name is used as the key, the value
%           is a PID.
-record(node, {name, fullpath, depth=0, type, obj, data=nil, cleanup=nil,
        handler=nil, parent=nil, children=dict:new()}).

% Component record.
% profile: The component's profile. Name, vendor, etc.
% owned_ecs: A list of execution contexts this component owns.
% participating_ecs: A list of execution contexts this component is
%       participating in.
% in_data_ports: A list of the component's input data ports.
% out_data_ports: A list of the component's output data ports.
% corba_ports: A list of the component's CORBA ports.
% configuration: The component's configuration data.
% cbs: Dictionary of callback functions that will be called when relevant.
% state: The latest state of the component.
-record(component, {profile, owned_ecs, part_ecs, in_ports, out_ports,
        cbs=nil, corba_ports, config, state}).

% Component profile record.
% category: The category in which the component belongs.
% description: A description of the component.
% instance_name: The instance name of the component.
% parent: The name of the component's parent object, if it has one. An empty
%       string if it does not.
% properties: The component's extra properties, in a dictionary.
% type_name: The type name of the component.
% vendor: The component's vendor.
% version: The component's version.
-record(comp_profile, {category, description, instance_name, parent,
        type_name, vendor, version, properties}).

% Port service record.
% name: Name of the port.
% type: The port type. One of datain | dataout | corba | other.
% intfs: A list of the interfaces exposed by the port.
% conn_profs: A list of the ConnectorProfile's of this port.
% props: The port's extra properties, in a dictionary.
% owner: The PID of the owning component.
% owner_obj: The IOR reference to the RTObject that owns this port.
% obj: The IOR reference to the PortService object.
-record(port, {name, type, intfs, conn_profs, props, owner, owner_obj, obj,
        cbs=nil}).

% Port interface profile record.
% instance_name: Instance name of the port interface.
% type_name: Type name of the port interface.
% polarity: Polarity of the port. One of provided | required.
-record(port_intf_prof, {inst_name, type_name, polarity}).

% Connector profile record.
% name: The name of the connector.
% connector_id: The connector's unique identifier.
% ports: A list of the IOR port object references involved in this connection.
% owner: The PID of the owning port's process.
% properties: The connection's extra properties, in a dictionary.
-record(conn_prof, {name, conn_id, ports, owner, props}).

% Execution context record.
% handle: The context's unique identifier.
% running: true if the context is running, otherwise false.
% kind: The type of execution context. One of periodic | event_driven | other.
% rate: The exection rate of the context. Only used if kind is periodic.
% owner: The PID of the owning component, if known (nil for participating
%   contexts).
% owner_obj: The IOR reference to the RTObject that owns this context, if any.
% parts: A list of IOR references to RTObjects participating in this context.
% props: The context's extra properties, in a dictionary.
% obj: The IOR reference to the ExecutionContextService object.
-record(exec_context, {handle, running, kind, rate, owner, owner_obj,
        parts, props, obj, cbs=nil}).

% Configuration record.
% conf_sets: A list of the component's configuration sets.
% active_set_name: The name of the active configuration set.
% obj: The IOR reference to the configuration object.
-record(config, {sets, active_set_name, obj, cbs=nil}).

% Configuration set record.
% id: The set's identification.
% desc: A description of the set.
% data: The configuration set data, as a dictionary.
-record(config_set, {id, desc, data}).

% Manager record.
% config: The manager's configuration, as a dictionary.
% profile: The manager's profile, as a dictionary.
% fact_profs: A list of factory profiles, each as a dictionary.
% loadable_mods: A list of loadable module profiles, each as a dictionary.
% loaded_mods: A list of loaded module profiles, each as a dictionary.
% masters: A list of IOR references to the manager's masters.
% obj: The IOR reference to the Manager object.
-record(manager, {config, profile, fact_profs, loadable_mods, loaded_mods,
        masters, obj}).

