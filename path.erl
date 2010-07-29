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
% Path parsing.

-module(path).

% External use exports
-export([parse/1]).

% Internal use exports
-export([test/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API

parse("/") ->
    {["/"], nil};
parse(P) ->
    Segs = string:tokens(P, "/"),
    Last = lists:last(Segs),
    case string:tokens(Last, ":")
        of [Comp, Port] ->
            Segs1 = lists:delete(Last, Segs),
            Segs2 = lists:append(Segs1, [Comp]),
            {["/"|Segs2], Port}
         ; [_] ->
            {["/"|Segs], nil}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test entry functions

test() ->
    test1(),
    test2(),
    test3(),
    test4(),
    test5(),
    test6(),
    test7(),
    test8(),
    test9(),
    test10().

test1() ->
    {["/"], nil} = parse("/"),
    ok.

test2() ->
    {["/", "localhost"], nil} = parse("/localhost"),
    ok.

test3() ->
    {["/", "localhost"], nil} = parse("/localhost/"),
    ok.

test4() ->
    {["/", "localhost", "blurgle.host"], nil} = parse("/localhost/blurgle.host"),
    ok.

test5() ->
    {["/", "localhost", "blurgle.host"], nil} = parse("/localhost/blurgle.host/"),
    ok.

test6() ->
    {["/", "localhost", "blurgle.host", "Comp"], nil} = parse("/localhost/blurgle.host/Comp"),
    ok.

test7() ->
    {["/", "localhost", "blurgle.host", "Comp."], nil} = parse("/localhost/blurgle.host/Comp."),
    ok.

test8() ->
    {["/", "localhost", "blurgle.host", "Comp.rtc"], nil} = parse("/localhost/blurgle.host/Comp.rtc"),
    ok.

test9() ->
    {["/", "localhost", "blurgle.host", "Comp"], "port"} = parse("/localhost/blurgle.host/Comp:port"),
    ok.

test10() ->
    {["/", "localhost", "blurgle.host", "Comp.rtc"], "Port"} = parse("/localhost/blurgle.host/Comp.rtc:Port"),
    ok.

