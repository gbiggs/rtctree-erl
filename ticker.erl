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
% Ticker for triggering callbacks.

-module(ticker).

% Internal use exports
-export([clock_tick/3, stop_ticker/1, start_ticker/2, change_ticker/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API

clock_tick(Rate, TargetProc, Msg) ->
    receive
        stop ->
            ok;
        {new_rate, NewR} ->
            clock_tick(NewR, TargetProc, Msg);
        _ ->
            exit(bad_msg_to_timer)
    after
        Rate ->
            TargetProc ! {oneway, Msg},
            clock_tick(Rate, TargetProc, Msg)
    end.


stop_ticker(nil) ->
    ok;
stop_ticker(Pid) ->
    Pid ! stop,
    ok.


start_ticker(0, _) ->
    nil;
start_ticker(Rate, Msg) ->
    spawn_link(?MODULE, clock_tick, [Rate, self(), Msg]).


change_ticker(Ticker, NewRate) when is_pid(Ticker) and is_integer(NewRate) ->
    Ticker ! {new_rate, NewRate}.

