%%%-------------------------------------------------------------------
%%% @author Szymon Wieczorek
%%% @copyright (C) 2018, SW
%%% @doc
%%%
%%% @end
%%% Created : 19. kwi 2018 02:25
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("Szymon Wieczorek").

-include_lib("eunit/include/eunit.hrl").

-record(station, {name="", xy, measurements=#{}}).
-record(measurement, {type, value, date}).
-record(monitor, {locations=#{},stations=#{}}).

createMonitor_test() ->
  ?assertEqual(#monitor{},pollution:createMonitor()).

addStation_test() ->
  P = pollution:createMonitor(),
  S1 = #station{name = "A",xy = {1,1}},
  S2 = #station{name = "B", xy = {2,2}},
  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addStation("B",{2,2},P1),

  ?assertEqual(maps:get("A",P1#monitor.stations),S1),
  ?assertEqual(#monitor{locations = #{{1,1}=>"A",{2,2}=>"B"}, stations = #{"A"=>S1,"B"=>S2}},P2),

  P3 = pollution:addStation("A",{3,3},P2),
  P4 = pollution:addStation("C",{2,2},P3),

  ?assertEqual(#monitor{locations = #{{1,1}=>"A",{2,2}=>"B"}, stations = #{"A"=>S1,"B"=>S2}},P3),
  ?assertEqual(#monitor{locations = #{{1,1}=>"A",{2,2}=>"B"}, stations = #{"A"=>S1,"B"=>S2}},P4).

addValue_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),

  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addValue("A",Time,"PM10",123,P1),
  P3 = pollution:addValue("A",Time,"PM10",321,P2),
  P4 = pollution:addValue("B",Time,"PM2.5",124,P3),

  ?assertEqual(maps:get("A",P2#monitor.stations),#station{name = "A", xy = {1,1},
    measurements = #{{Time,"PM10"}=>#measurement{type = "PM10",value = 123,date = Time}}}),
  ?assertEqual(P2,P3,P4).

removeValue_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),

  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addValue("A",Time,"PM10",123,P1),
  P3 = pollution:removeValue("A",Time,"PM10",P2),
  P4 = pollution:removeValue("B",Time,"PM10",P3),

  ?assertEqual(P3,P1,P4).

getOneValue_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),

  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addValue("A",Time,"PM10",123,P1),
  A = pollution:getOneValue("A",Time,"PM10",P2),
  B = pollution:getOneValue("B",Time,"PM10",P2),
  C = pollution:getOneValue("A",123,"PM10",P2),
  ?assertEqual(123,A),
  ?assertEqual(io:format("There is no B station~n"),B),
  ?assertEqual(io:format("There is no record in given date or type (STATION: A)~n"),C).

getStationMean_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),
  {{K,L,M},{X,Y,Z}} = Time,
  Time1 = {{K,L,M},{X,Y,Z+3}},

  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addValue("A",Time,"PM10",10,P1),
  P3 = pollution:addValue("A",Time1,"PM10",20,P2),
  P4 = pollution:addValue("A",Time1,"PM2.5",1,P3),

  ?assertEqual(15.0,pollution:getStationMean("A","PM10",P4)),
  ?assertEqual(pollution:getStationMean("A","PM3",P4),io:format("No elements found")),
  ?assertEqual(pollution:getStationMean("B","PM3",P4),io:format("There is no B station")).

getDailyMean_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),
  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addValue("A",Time,"PM2.5",3,P1),
  P3 = pollution:addValue("A",Time,"PM2.5",25,P2),
  P4 = pollution:addStation("B",{2,2},P3),
  P5 = pollution:addValue("B",Time,"PM2.5",3,P4),

  ?assertEqual(3.0,pollution:getDailyMean(pollution:dayFromDate(Time),"PM2.5",P5)),
  ?assertEqual(pollution:getDailyMean(1111,"PM2.5",P5),io:format("No elements found")),
  ?assertEqual(pollution:getDailyMean(pollution:dayFromDate(Time),"PM10",P5),io:format("No elements found")).

getMaximumVariationStation_test() ->
  P = pollution:createMonitor(),
  Time = calendar:local_time(),
  P1 = pollution:addStation("A",{1,1},P),
  P2 = pollution:addValue("A",Time,"PM2.5",3,P1),
  P3 = pollution:addValue("A",123,"PM2.5",25,P2),
  P4 = pollution:addStation("B",{2,2},P3),
  P5 = pollution:addValue("B",Time,"PM2.5",3,P4),


  ?assertEqual({22,"A"},pollution:getMaximumVariationStation("PM2.5",P5)),
  ?assertEqual(0,pollution:getMaximumVariationStation("PM3",P5)).












