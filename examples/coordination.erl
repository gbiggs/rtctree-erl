-module(test2).
-export([run/0, cb/4]).
-include("/home/geoff/research/src/local/rtctree-erl/include/nodes.hrl").

run() ->
    ok = rtctree:start(),
    ok = rtctree:add_servers(["localhost"]),
    {ok, C} = rtctree:get_node_by_path(["/", "localhost", "laser0.rtc"]),
    component:add_cb(C, fun ?MODULE:cb/4, state, 1000).


cb('ERROR_STATE', _, C, _) ->
    % Disconnect and shut down the old localiser and laser
    ok = component:reset(C, 1),
    {ok, Loc} = rtctree:get_node_by_path(["/", "localhost", "laser_localise0.rtc"]),
    ok = component:disconnect_all(Loc),
    ok = component:deactivate(Loc, 1),
    % Create and connect the new localiser
    rtctree:connect_by_path(["/", "localhost", "odometry0.rtc", "odo"],
        ["/", "localhost", "odo_localise0.rtc", "odo"]),
    rtctree:connect_by_path(["/", "localhost", "gyro0.rtc", "gyro"],
        ["/", "localhost", "odo_localise0.rtc", "gyro"]),
    % Activate the new localiser
    {ok, OdoLoc} = rtctree:get_node_by_path(["/", "localhost", "odo_localise0.rtc"]),
    component:activate(OdoLoc, 1);
cb(_, _, _, _) ->
    ok.

