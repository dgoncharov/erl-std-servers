%% This module implements the discard protocol over udp.
%% See rfc863 for details.
%% 
%% Copyright Dmitry Goncharov 2009
%% Use, modification, and distribution is subject to the BSD license
%% http://www.opensource.org/licenses/bsd-license.php

-module(discard_udp).
-export([start/1]).
-export([main/1, main/0]).

main([Port]) ->
    spawn(fun() -> start(list_to_integer(atom_to_list(Port))) end),
    block_till_stdin_closed(false).

main() ->
    spawn(fun() -> start(discard) end),
    block_till_stdin_closed(false).

block_till_stdin_closed(false) ->
    block_till_stdin_closed(io:get_line('') =:= "q\n");

block_till_stdin_closed(true) ->
    init:stop().

start(Port) ->
    io:format("~p opening udp port ~p~n", [self(), Port]),
    {ok, Sock} = gen_udp:open(Port, [binary, {active, once}]),
    io:format("~p successfully opened udp port ~p: ~p~n", [self(), Port, Sock]),
    serve_sock(Sock).

serve_sock(Sock) ->
    io:format("~p waiting on ~p~n", [self(), Sock]),
    receive
    {udp, Sock, Host, Port, Payload} ->
        io:format("~p dgram on socket ~p from ~p:~p, payload: ~p~n", [self(), Sock, Host, Port, Payload]),
	inet:setopts(Sock, [{active, once}]),
        serve_sock(Sock);
    Unexpected ->
        io:format("~p received unexpected message ~p~n", [self(), Unexpected])
    end.

