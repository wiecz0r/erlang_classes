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








