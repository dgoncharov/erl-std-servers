%% This module implements the echo protocol over tcp.
%% See rfc862 for details.
%% 
%% Copyright Dmitry Goncharov 2009
%% Use, modification, and distribution is subject to the BSD license
%% http://www.opensource.org/licenses/bsd-license.php

-module(echo).
-export([start/1]).
-export([main/1]).

main([Port]) ->
    {ok, Listen} = start(list_to_integer(atom_to_list(Port))),
    block_till_stdin_closed(false, Listen).

block_till_stdin_closed(false, Listen) ->
    block_till_stdin_closed(io:get_line('') =:= "q\n", Listen);

block_till_stdin_closed(true, Listen) ->
    gen_tcp:close(Listen),
    init:stop().

start(Port) ->
    io:format("~p starting to listen on port ~B~n", [self(), Port]),
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
    serve_conn(Sock);

accept({error, closed}, _Listen) ->
    io:format("the listening socket ~p closed~n", [_Listen]).

serve_conn(Sock) ->
    io:format("~p waiting on ~p~n", [self(), Sock]),
    receive
    {tcp, Sock, Payload} ->
        io:format("~p received from ~p: ~p~n", [self(), Sock, Payload]),
        ok = gen_tcp:send(Sock, Payload),
	inet:setopts(Sock, [{active, once}]),
        serve_conn(Sock);
    {tcp_closed, Sock} ->
        io:format("~p connection ~p closed by peer~n", [self(), Sock]),
	gen_tcp:close(Sock);
    Unexpected ->
        io:format("~p received unexpected message ~p~n", [self(), Unexpected])
    end.

