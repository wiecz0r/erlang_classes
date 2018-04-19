%%%-------------------------------------------------------------------
%%% @author Szymon Wieczorek
%%% @copyright (C) 2018, SW
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2018 01:15
%%%-------------------------------------------------------------------
-module(pollution).
-author("Szymon Wieczorek").

%% API
-export([createMonitor/0, addStation/3, addValue/5,removeValue/4,
  getOneValue/4,getStationMean/3,getDailyMean/3,getMaximumVariationStation/2]).

-record(station, {name="", xy, measurements=#{}}).
-record(measurement, {type, value, date}).
-record(monitor, {locations=#{},stations=#{}}).

createMonitor() -> #monitor{}.

addStation(Name,{_,_}=Geo,Monitor) ->
  case maps:is_key(Name,Monitor#monitor.stations) or maps:is_key(Geo,Monitor#monitor.locations) of
    true -> Monitor;
    false -> #monitor{
      locations = maps:put(Geo,Name,Monitor#monitor.locations),
      stations = maps:put(Name,#station{name=Name, xy = Geo},Monitor#monitor.stations)
    }
  end.

addValue({_,_}=Geo,Date,Type,Value,Monitor) ->
  case maps:get(Geo,Monitor#monitor.locations,no_val) of
    no_val -> Monitor;
    Name ->
      addValue(Name,Date,Type,Value,Monitor)
  end;

addValue(Name,Date,Type,Value,Monitor) ->
  case maps:get(Name,Monitor#monitor.stations,no_val) of
    no_val -> Monitor;
    Station ->
      case maps:is_key({Date,Type},Station#station.measurements) of
        true -> Monitor;
        false ->
          NewMeasurement = #measurement{type = Type, date = Date, value = Value},
          NewStation = Station#station{measurements = maps:put({Date,Type},NewMeasurement,Station#station.measurements)},
          Monitor#monitor{stations = maps:put(Name,NewStation,Monitor#monitor.stations)}
      end
  end.

removeValue({_,_}=Geo,Date,Type,Monitor) ->
  case maps:get(Geo,Monitor#monitor.locations,no_val) of
    no_val -> "alamakota";
    Name ->
      removeValue(Name,Date,Type,Monitor)
  end;

removeValue(Name,Date,Type,Monitor) ->
  case maps:get(Name,Monitor#monitor.stations,no_val) of
    no_val -> Monitor;
    Station ->
      case maps:find({Date,Type},Station#station.measurements) of
        error -> Monitor;
        _ ->
          NewMeasurements = maps:remove({Date,Type},Station#station.measurements),
          NewStations = maps:update(Name,Station#station{measurements = NewMeasurements},Monitor#monitor.stations),
          Monitor#monitor{stations = NewStations}
      end
  end.

getOneValue(Name,Data,Type,Monitor) ->
  case maps:get(Name,Monitor#monitor.stations,no_val) of
    no_val -> io:format("There is no ~s station~n",[Name]);
    Station ->
      case maps:get({Data,Type},Station#station.measurements,no_val) of
        no_val -> io:format("There is no record in given date or type (STATION: ~s)~n",[Name]);
        {_,_,Value,_} -> Value
      end
  end.

getStationMean(Name,Type,Monitor) ->
  case maps:get(Name,Monitor#monitor.stations,no_val) of
    no_val -> io:format("There is no ~s station~n",[Name]);
    Station ->
      Filtered = maps:filter(fun (_,Type1)->Type1#measurement.type == Type end,Station#station.measurements),
      Summing = fun (_,V,Sum) -> Sum + V#measurement.value end,
      Count = maps:size(Filtered),
      Sum = maps:fold(Summing,0,Filtered),
      case Count of
        0 -> io:format("No elements found~n");
        _ -> Sum/Count
      end
  end.

getDailyMean(Day,Type,Monitor) ->
  Summing = fun (_,V,{Sum,Count}) -> {Sum + V#measurement.value, Count + 1 } end,
  FilterBool = fun(_,V) -> (V#measurement.type == Type) and (dayFromDate(V#measurement.date) == Day) end,
  Mapped = maps:map(fun(_,Station) -> maps:fold(Summing,{0,0},maps:filter(FilterBool,Station#station.measurements)) end, Monitor#monitor.stations),

  {Sum,Count} = maps:fold(fun(_,{Sum,Count},{Sum1,Count1}) -> {Sum + Sum1,Count + Count1} end,{0,0},Mapped),

  case Count of
    0 -> io:format("No elements found~n");
    _ -> Sum/Count
  end.

getMaximumVariationStation(Type,Monitor) ->
  FilterBool = fun(_,V) -> V#measurement.type == Type end,

  HighLow = fun (_,V,{Highest,Lowest}) ->
    case {Highest,Lowest} == {0,0} of
      true -> {V#measurement.value,V#measurement.value};
      false ->
        case Highest < V#measurement.value of
          true -> {V#measurement.value,Lowest};
          false ->
            case Lowest > V#measurement.value of
              true -> {Highest,V#measurement.value};
              false -> {Highest, Lowest}
            end
        end
    end
     end,

  Mapped = maps:map(fun(_,Station) ->
    {High,Low} = maps:fold(HighLow,{0,0},maps:filter(FilterBool,Station#station.measurements)),
    {High-Low,Station#station.name} end, Monitor#monitor.stations),

  HighLowAll = fun (_,{Value,Name},{Val,Name1}) ->
    case Value < Val of
      true -> {Val, Name1};
      false -> {Value, Name}
    end
               end,
  {Val,Name} =maps:fold(HighLowAll,{0,""},Mapped),
  io:format("Najwieksza roznica wynosi ~w dla stacji ~s~n",[Val,Name]),
  {Val,Name}.


dayFromDate(Date) ->
  {Day,_} = Date,
  Day.






























