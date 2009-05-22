%% This module implements the daytime protocol over udp.
%% See rfc867 for details.
%% 
%% Copyright Dmitry Goncharov 2009
%% Use, modification, and distribution is subject to the BSD license
%% http://www.opensource.org/licenses/bsd-license.php

-module(daytime_udp).
-export([start/1]).
-export([main/1, main/0]).

main([Port]) ->
    spawn(fun() -> start(list_to_integer(atom_to_list(Port))) end),
    common:exec_when_stdin_closed(fun() -> init:stop() end).

main() ->
    spawn(fun() -> start(daytime) end),
    common:exec_when_stdin_closed(fun() -> init:stop() end).

start(Port) ->
    io:format("~p opening udp port ~p~n", [self(), Port]),
    {ok, Sock} = gen_udp:open(Port, [binary, {active, once}]),
    io:format("~p successfully opened udp port ~p: ~p~n", [self(), Port, Sock]),
    serve_sock(Sock).

serve_sock(Sock) ->
    io:format("~p waiting on ~p~n", [self(), Sock]),
    receive
    {udp, Sock, Host, Port, _} ->
        io:format("~p dgram on socket ~p from ~p:~p~n", [self(), Sock, Host, Port]),
        P = httpd_util:rfc1123_date(calendar:universal_time()) ++ [$\r, $\n],
        io:format("~p sending to ~p:~p ~p bytes: ~p~n", [self(), Host, Port, length(P), P]),
        ok = gen_udp:send(Sock, Host, Port, P),
	inet:setopts(Sock, [{active, once}]),
        serve_sock(Sock);
    Unexpected ->
        io:format("~p received unexpected message ~p~n", [self(), Unexpected])
    end.

