%%%-------------------------------------------------------------------
%%% @author Szymon Wieczorek
%%% @copyright (C) 2018, SW
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2018 15:31
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-import(pollution_server,[start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2,
getDailyMean/2, getMaximumVariationStation/1, init/0]).

-author("Szymon Wieczorek").

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  ?assertEqual(start(), ok),
  stop().

stop_test() ->
  start(),
  ?assertEqual(stop(), ok).

add_remove_test() ->
  start(),
  ?assertEqual(addStation("S1",{123,321}), ok),
  ?assertEqual(addValue("S1",12,"PM10",7), ok),
  ?assertEqual(removeValue("S1",12,"PM10"), ok),
  stop().

getOneValue_test() ->
  start(),
  Time = calendar:local_time(), Time2 = calendar:local_time(),
  addStation("S1",{123,321}),
  addStation("S2",{193,321}),
  addValue("S1",Time,"PM10",7),
  addValue("S2",Time2,"PM10",5),
  ?assertEqual(getOneValue("S1",Time,"PM10"), 7),
  ?assertEqual(getOneValue("S2",Time2,"PM10"), 5),
  stop().

getStationMean_test() ->
  start(),
  Time = calendar:local_time(),
  {{K,L,M},{X,Y,Z}} = Time,
  Time2 = {{K,L,M},{X,Y,Z+3}},
  addStation("S1",{123,321}),
  addStation("S2",{193,321}),
  addValue("S1",Time,"PM10",7),
  addValue("S2",Time2,"PM10",5),
  addValue("S1",Time2,"PM10",3),
  addValue("S2",Time,"PM10",3),
  ?assertEqual(5.0, getStationMean("S1","PM10")),
  ?assertEqual(4.0, getStationMean("S2","PM10")),
  ?assertEqual(io:format("No elements found~n"), getStationMean("S2","PM2.5")),
  stop().

getDailyMean_test() ->
  start(),
  Time = calendar:local_time(),
  {{K,L,M},{X,Y,Z}} = Time,
  Time2 = {{K,L,M},{X,Y,Z+3}},
  addStation("S1",{123,321}),
  addStation("S2",{193,321}),
  addValue("S1",Time,"PM10",7),
  addValue("S2",Time2,"PM10",5),
  addValue("S1",Time2,"PM2.5",3),
  addValue("S2",Time,"PM10",3),
  ?assertEqual(5.0, getDailyMean({K,L,M},"PM10")),
  ?assertEqual(io:format("No elements found~n"), getDailyMean({K+1,L,M},"PM10")),
  stop().

getMaximumVariationStation_test() ->
  start(),
  Time = calendar:local_time(),
  addStation("A",{1,1}),
  addValue("A",Time,"PM2.5",3),
  addValue("A",123,"PM2.5",25),
  addStation("B",{2,2}),
  addValue("B",Time,"PM2.5",3),
  ?assertEqual({22,"A"},getMaximumVariationStation("PM2.5")),
  ?assertEqual(0,getMaximumVariationStation("PM3")),
  stop().