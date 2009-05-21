-module(common).
-export([exec_when_stdin_closed/1]).
-export([gen_charseq/1]).

exec_when_stdin_closed(Fun) ->
    exec_when_stdin_closed(Fun, false).

exec_when_stdin_closed(Fun, false) ->
    exec_when_stdin_closed(Fun, io:get_line('') =:= "q\n");

exec_when_stdin_closed(Fun, true) ->
    Fun().

gen_charseq(N) ->
    Maxchars = 95, % There are 95 ascii chars which can be printed.
    Linelen = 72, % The number of chars on a line.
    Chars = lists:seq($\s, $\s + Maxchars - 1),
    Low = N rem Maxchars,
    Up = (N + Linelen - 1) rem Maxchars,
    %    io:format("~p - ~p~n", [Low, Up]),
    L = case Up > Low of
    true ->
        lists:sublist(Chars, Low + 1, Up - Low + 1);
    false ->
        L1 = lists:sublist(Chars, Low, Maxchars),
        L2 = lists:sublist(Chars, 1, Up),
%       io:format("L1: ~p, L2: ~p~n", [L1, L2]),
        L1 ++ L2
    end,
    L ++ [$\r, $\n].

