%%%-------------------------------------------------------------------
%%% @author Szymon Wieczorek
%%% @copyright (C) 2018, SW
%%% @doc
%%%
%%% @end
%%% Created : 19. kwi 2018 11:24
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Szymon Wieczorek").

%% API
-export([start/0,stop/0,play/1,a/0,b/0]).

start() ->
  register(ping,spawn(pingpong,a,[])),
  register(pong,spawn(pingpong,b,[])).

a() ->
  receive
    stop -> io:format("Zakonczono PING~n");
    0 -> io:format("PING:Odebrano liczbe: 0~n"), a();
    N ->
      timer:sleep(200),
      io:format("PING:Odebrano liczbe: ~w~n",[N]),
      timer:sleep(200),
      pong ! N-1,
      io:format("PING:Wyslano liczbe: ~w~n",[N-1]),
      timer:sleep(200),
      a()
  after
    20000 -> io:format("Zakonczono PING~n")
  end.

b() ->
  receive
    stop -> io:format("Zakonczono PONG~n");
    0 -> io:format("PONG:Odebrano liczbe: 0~n"), b();
    N ->
      timer:sleep(200),
      io:format("PONG:Odebrano liczbe: ~w~n",[N]),
      timer:sleep(200),
      ping ! N-1,
      io:format("PONG:Wyslano liczbe: ~w~n",[N-1]),
      timer:sleep(200),
      b()
  after
    20000 -> io:format("Zakonczono PONG~n")
  end.

play(N) ->
  ping ! N.

stop() ->
  ping ! stop,
  pong ! stop.
