%%%-------------------------------------------------------------------
%%% @author Szymon Wieczorek
%%% @copyright (C) 2018, SW
%%% @doc
%%%
%%% @end
%%% Created : 19. kwi 2018 12:04
%%%-------------------------------------------------------------------
-module(pollution_server).
-import(pollution,[createMonitor/0, addStation/3, addValue/5,removeValue/4,
getOneValue/4,getStationMean/3,getDailyMean/3,getMaximumVariationStation/2,dayFromDate/1]).

-author("Szymon Wieczorek").

%% API
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2,
  getDailyMean/2, getMaximumVariationStation/1, init/0]).

%%
%% CLIENT
%%
start() ->
  register(server,spawn_link(?MODULE,init,[])),
  io:format("Pollution server STARTED~n").

init() ->
  io:format("Monitor created~n"),
  main_loop(pollution:createMonitor()).

stop() ->
  handle(stop,[]),
  io:format("Pollution server STOPPED~n").

addStation(Name,{_,_}=Geo) ->
  handle(add_station,[Name,Geo]).

addValue(Name_geo,Date,Type,Value) ->
  handle(add_value,[Name_geo,Date,Type,Value]).

removeValue(Name_geo,Date,Type) ->
  handle(remove_value,[Name_geo,Date,Type]).

getOneValue(Name,Date,Type) ->
  handle(get_one_value,[Name,Date,Type]).

getStationMean(Name,Type) ->
  handle(get_station_mean,[Name,Type]).

getDailyMean(Day,Type) ->
  handle(get_daily_mean,[Day,Type]).

getMaximumVariationStation(Type) ->
  handle(get_max_var_station,[Type]).

handle(Msg,Args) ->
  server ! {req,self(),Msg,Args},
  receive
    {rep,M} -> M;
    {rep,error,E} -> E
  after
    1000 -> {error,timeout}
  end.

%%
%% SERVER
%%

main_loop(P) ->
  receive
    {req,Pid,add_station,[Name,Geo]} ->
      P1 = pollution:addStation(Name,Geo,P),
      Pid ! {rep,ok},
      main_loop(P1);
    {req,Pid,add_value,[Name_geo,Date,Type,Value]} ->
      P1 = addValue(Name_geo,Date,Type,Value,P),
      Pid ! {rep, ok},
      main_loop(P1);
    {req,Pid,remove_value,[Name_geo,Date,Type]} ->
      P1 = removeValue(Name_geo,Date,Type,P),
      Pid ! {rep,ok},
      main_loop(P1);
    {req,Pid,get_one_value,[Name,Date,Type]} ->
      Val = getOneValue(Name,Date,Type,P),
      Pid ! {rep,Val},
      main_loop(P);
    {req,Pid,get_station_mean,[Name,Type]} ->
      Mean = getStationMean(Name,Type,P),
      Pid ! {rep,Mean},
      main_loop(P);
    {req,Pid,get_daily_mean,[Day,Type]} ->
      Mean = getDailyMean(Day,Type,P),
      Pid ! {rep,Mean},
      main_loop(P);
    {req,Pid,get_max_var_station,[Type]} ->
      Station = getMaximumVariationStation(Type,P),
      Pid ! {rep,Station},
      main_loop(P);
    {req,Pid,stop,[]} ->
      Pid ! {rep,server_stopped};
    {req,Pid,_,_} ->
      Pid ! {rep,error,unknown_request}
  end.
