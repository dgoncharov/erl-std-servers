%% This module implements the chargen protocol over tcp.
%% See rfc864 for details.
%% 
%% Copyright Dmitry Goncharov 2009
%% Use, modification, and distribution is subject to the BSD license
%% http://www.opensource.org/licenses/bsd-license.php

-module(chargen).
-export([start/1]).
-export([main/1, main/0]).

main([Port]) ->
    {ok, Listen} = start(list_to_integer(atom_to_list(Port))),
    block_till_stdin_closed(false, Listen).

main() ->
    {ok, Listen} = start(chargen),
    block_till_stdin_closed(false, Listen).

block_till_stdin_closed(false, Listen) ->
    block_till_stdin_closed(io:get_line('') =:= "q\n", Listen);

block_till_stdin_closed(true, Listen) ->
    gen_tcp:close(Listen),
    init:stop().

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
    serve_conn(Sock, 0);

accept({error, closed}, _Listen) ->
    io:format("the listening socket ~p closed~n", [_Listen]).

serve_conn(Sock, N) ->
    io:format("~p waiting on ~p~n", [self(), Sock]),
    receive
    {tcp, Sock, _} ->
        io:format("~p received from ~p~n", [self(), Sock]),
	P = gen_seq(N),
        io:format("~p sending ~p bytes ~p to ~p~n", [self(), length(P), P, Sock]),
        ok = gen_tcp:send(Sock, P),
	inet:setopts(Sock, [{active, once}]),
        serve_conn(Sock, N + 1);
    {tcp_closed, Sock} ->
        io:format("~p connection ~p closed by peer~n", [self(), Sock]),
	gen_tcp:close(Sock);
    Unexpected ->
        io:format("~p received unexpected message ~p~n", [self(), Unexpected])
    end.

gen_seq(N) ->
    Chars = lists:seq(32, 32 + 94),
    Low = N rem 95,
    Up = (N + 71) rem 95,
%    io:format("~p - ~p~n", [Low, Up]),
    L = case Up > Low of
    true ->
        lists:sublist(Chars, Low + 1, Up - Low + 1);
    false ->
        L1 = lists:sublist(Chars, Low, 10000),
	L2 = lists:sublist(Chars, 1, Up),
%	io:format("L1: ~p, L2: ~p~n", [L1, L2]),
        L1 ++ L2
    end,
    L ++ [$\r, $\n].

