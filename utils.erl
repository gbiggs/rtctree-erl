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
% Utilities.

-module(utils).

% External use exports
-export([build_attr_string/2]).

% Internal use exports
-export([test/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API - execution context functions

build_attr_string(_, false) ->
    "";
build_attr_string(Attrs, _) ->
    build_attr_string_int(Attrs, [8#33, $[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions

attrs() ->
    dict:from_list([{reset, "00"},
                    {bold, "01"},
                    {faint, "02"},
                    {underline, "04"},
                    {blink, "05"},
                    {blinkfast, "06"},
                    {negative, "07"},
                    {normal, "22"},
                    {nounderline, "24"},
                    {noblink, "25"},
                    {positive, "27"},
                    {black, "30"},
                    {red, "31"},
                    {green, "32"},
                    {brown, "33"},
                    {blue, "34"},
                    {purple, "35"},
                    {cyan, "36"},
                    {white, "37"},
                    {bgblack, "40"},
                    {bgred, "41"},
                    {bggreen, "42"},
                    {bgbrown, "43"},
                    {bgblue, "44"},
                    {bgpurple, "45"},
                    {bgcyan, "46"},
                    {bgwhite, "47"}]).


build_attr_string_int([], Acc) ->
    string:concat(string:substr(Acc, 1, length(Acc) - 1), "m");
build_attr_string_int([H|T], Acc) ->
    build_attr_string_int(T,
        string:concat(string:concat(Acc, dict:fetch(H, attrs())), ";")).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests

test() ->
    test_no_support(),
    test_support(),
    ok.


test_no_support() ->
    "" = build_attr_string([faint, underline, blinkfast, cyan, bgcyan], false),
    ok.


test_support() ->
    [8#33] ++ "[01;31;47m" = build_attr_string([bold, red, bgwhite], true),
    ok.

