rtctree-erl
===============================================================================

rtctree-erl is an Erlang library providing an easy-to-use API for interacting with
running RT Components and RTM-based systems running on OpenRTM-aist-1.0. It
allows developers to manage these systems from other programs without needing to
learn the CORBA API. Components can be started, stopped, connected together,
have their configuration changed, and so on.

A key feature of the library is its provision of a callback-based monitoring
API mirroring many of the internal callbacks of OpenRTM-aist. These callbacks
can be used to create a reactive coordinator that monitors a set of RT
Components running on the local computer or distributed across a network. The
callbacks are able to effect changes in the RT Components, such as activating
and deactivating components, altering connections, and even creating and
destroying components through manager deamons.

This is version 1.0 of the library. It is a highly experimental library
investigating Erlang as a tool for monitoring robot systems. There may be bugs
and flaws in the API. We encourage any feedback to help us improve this library
and its API.

This software is developed at the National Institute of Advanced Industrial
Science and Technology. Approval number H22PRO-1150. The development was
financially supported by a JSPS young scientist B grant, for development of
languages for the coordination of RT Middleware-based robot systems
(科研費補助金：RTミドルウエア技術をベースとしたロボットシステム統合のための
プログラム言語). This software is licensed under the Eclipse Public License -v
1.0 (EPL). See LICENSE.TXT.


Requirements
------------

rtctree-erl requires Erlang R13 or newer. All necessary software is included in
the Erlang distribution, including the ORB and IDL compiler.


Installation
------------

Extract the source to a directory and compile it. The library can be started
from that directory.

1. Extract the source.

   $ tar -xvzf rtctree-erl-1.0.0.tar.gz

2. Change to the "idl" directory below the extracted directory.

   $ cd rtctree-erl-1.0.0/idl/

3. Compile the IDL files.

   $ for ii in `ls *.idl`; do erlc ${ii}; done
   $ for ii in `ls *.erl`; do erlc ${ii}; done

4. Change to the library directory.

   $ cd ..

5. Compile the library using the included Makefile.

   $ make


The RTC Tree
------------

The core of the library is the RTC Tree:

    ok = rtctree:start(),
    ok = rtctree:add_servers(["localhost"]).

This is a file system-like tree built by parsing name servers to find
directories, components and managers. You can treat it exactly the same way as
you treat a normal file system. The tree represents the naming contexts,
managers and components registered all on known name servers in a tree
structure:

\
|-+localhost
| |-+naming_context
| | |--ConsoleIn0.rtc
| | |--ConsoleOut0.rtc
| |
| |--another_naming_context
| |--Sensor0.rtc
|
|-+192.168.0.5
  |--Motor0.rtc
  |--Controller0.rtc

Each ``directory'' in the tree represents a naming context, which may be a
normal naming context or the root context of a name server. These are
represented by NameServer and Directory objects.

Name servers are treated as directories off the root directory, /.
Below them are ``files'' and sub-directories. A sub-directory represents a
naming context below the root naming context of a name server.

Files are components and managers, represented by the Component and Manager
classes, respectively.

Component objects store a variety of information about the component they
represent. You can access the component's ports, configuration sets, and so on.
Use these objects to change configuration values, connect ports to each other,
start and stop components, etc.

All nodes in the tree also store the CORBA object reference to the object they
represent. By accessing this object, you can call the IDL methods. If something
is not currently available in rtctree, calling the IDL method on the CORBA
object directly will be able to achieve what you want to do.


Building the tree

Start the library by calling the rtctree:start function:

 > rtctree:start().

Name servers can be added to the tree at any time:

 > rtctree:add_servers(["localhost", "192.168.0.1:12345"]).

As name servers are added, their objects are parsed into components, managers
and unknowns.


Paths

Nodes in the tree are addressed using paths. A path is a list of strings, each
representing a level in the tree one deeper than the previous list item.
Absolute paths are necessary to address into the tree object. Addressing from
nodes allows relative paths, provided that the path exists below the node.

When represented as text, these paths resemble file system paths. The root of
the tree is represented by / (\ on Windows systems). The first level of entries
are name server addresses. Entries below the first level are components,
managers and naming contexts (which are represented as directories). The
utility function parse_path will parse a text string path into a list of path
entries that can be used to address nodes in the tree.

For example, the path
 /localhost/naming_context/ConsoleIn0.rtc
represents the component ConsoleIn0.rtc, registered in the naming_context
naming context on the name server running at localhost. When used to find the
node in the tree representing this component, the path should be a list:

 ["/", "localhost", "naming_context", "ConsoleIn0.rtc"]


Useful functions

Useful member functions of the RTCTree class and node classes that will be of
particular interest are shown below. This is not a complete list of all
available functionality. Users are encouraged to check the full API
documentation for additional functionality.

rtctree:start             Starts the rtctree application.
rtctree:stop              Shuts down the rtctree.
rtctree:add_servers       Adds servers to the tree.
rtctree:get_root          Retrieves the root node of the tree.
rtctree:get_node_by_path  Retrieves a node by its path.
rtctree:iterate           Execute a function on all nodes for which given
                          predicates return true. Return the results in a list.
rtctree:connect_by_path   Connect two nodes specified by their paths.

component:activate          Activate a component.
component:get_state         Get the state of a component.
component:get_ports         Get all the ports provided by a component.
component:get_port_by_name  Get a specific port by its name.
component:disconnect_all    Remove all connections to a component.
component:get_config        Get the configuration sets of a component.
component:add_cb            Register a callback on a component.
component:remove_cbs        Remove all callbacks or just some by properties.

configuration:activate_set  Activate a configuration set.
configuration:get_param     Get the value of a parameter.
configuration:set_param     Change a parameter in a configuration set.
configuration:add_cb        Register a callback on a configuration.

connection:disconnect  Remove a specific connection.

directory:unbind  Remove a binding from a naming context, effectively deleting
                  the object. Be careful with this; you can unbind entire
                  branches of the RTC Tree.

exec_context:is_running        Test if an execution context is running.
exec_context:get_participants  Get a list of the components participating in an
                               execution context.
exec_context:get_rate          Get the rate at which a periodic context is
                               executing.
exec_context:add_cb            Register a callback on an execution context.

manager:create_comp  Create a new component instance.
manager:del_comp     Remove a component instance.
manager:load_mod     Load a shared module.
manager:unload_mod   Unload a shared module.

node:get_name        Get the name of a node (it's path entry).
node:get_type        Get the type of a node.
node:get_depth       Get the depth of a node in the tree.
node:is_component    Test if a node is a component.

nvlist:to_dict    Convert a CORBA name-value list into an Erlang dictionary.
nvlist:from_dict  Make a CORBA name-value list from an Erlang dictionary.

path:parse  Parse a string representation of a path into a list representation.

port:connect       Connect a port to another port.
port:get_name      Get the name of a port.
port:is_connected  Test if a port is connected.
port:is_in_port    Test if a port is an input port.
port:add_cb        Register a callback on a port.

utils:build_attr_string  Create a string to colourise terminal output.


Callbacks
---------

Several slots are available throughout the library for registering callbacks.
These are used to monitor aspects of the component network at runtime, allowing
custom responses to changes in the system to be written. The following
callbacks are currently available (listed as "module":"type"):

component:state             Change in a component's state.
configuration:config_param  Change in a configuration parameter's value.
configuration:config_set    Change in the active configuration set.
exec_context:ec_running     Change in the state of an execution context.
exec_context:ec_rate        Change in the execution rate of a context.
port:conns                  Change in the connections on a port.

To register a callback, provide the desired type when calling the add_cb/4
function.

A callback function must conform to a specific signature. It must take four
arguments:

1. The new value.
2. The old value.
3. The PID of the node/object that executed the callback.
4. A value depending on the type:
   component:state - The full path of the component.
   configuration:config_param - Nothing
   configuration:config_set - Nothing
   exec_context:ec_running - The handle of the execution context.
   exec_context:ec_rate - The handle of the execution context.
   port:conns - The name of the port.

Two examples of setting callbacks are provided in the examples/ directory.


API naming conventions
----------------------

Each module in rtctree-erl exports two sets of functions. One is for external
use, the other is exported by the other modules of the library. Users should
only use those functions marked as exported for external use.


Repository
----------

The latest source is stored in a Git repository at github, available at
http://github.com/gbiggs/rtctree-erl. You can download it as a zip file or
tarball by clicking the "Download Source" link in the top right of the page.
Alternatively, use Git to clone the repository. This is better if you wish to
contribute patches.

 $ git clone git://github.com/gbiggs/rtctree-erl.git


Changelog
---------

