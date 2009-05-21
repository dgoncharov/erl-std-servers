%% This module implements the daytime protocol over tcp.
%% See rfc867 for details.
%% 
%% Copyright Dmitry Goncharov 2009
%% Use, modification, and distribution is subject to the BSD license
%% http://www.opensource.org/licenses/bsd-license.php

-module(daytime).
-export([start/1]).
-export([main/1, main/0]).

main([Port]) ->
    {ok, Listen} = start(list_to_integer(atom_to_list(Port))),
    common:exec_when_stdin_closed(fun() -> gen_tcp:close(Listen), init:stop() end).

main() ->
    {ok, Listen} = start(daytime),
    common:exec_when_stdin_closed(fun() -> gen_tcp:close(Listen), init:stop() end).

start(Port) ->
    io:format("~p starting to listen on port ~p~n", [self(), Port]),
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, once}]),
    io:format("~p successfully started to listen on ~p~n", [self(), Listen]),
    spawn(fun() -> accept(Listen) end),
    {ok, Listen}.

accept(Listen) ->
    io:format("~p accepting on ~p~n", [self(), Listen]),
    accept(gen_tcp:accept(Listen), Listen).

accept({ok, Sock}, Listen) ->
    io:format("~p accepted connection ~p~n", [self(), Sock]),
    spawn(fun() -> accept(Listen) end),
    P = httpd_util:rfc1123_date(calendar:universal_time()) ++ [$\r, $\n],
    io:format("~p sending ~p to ~p~n", [self(), P, Sock]),
    ok = gen_tcp:send(Sock, P),
    gen_tcp:close(Sock);

accept({error, closed}, _Listen) ->
    io:format("the listening socket ~p closed~n", [_Listen]).

