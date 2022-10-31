-module(vp_prometheus).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    declare_metrics(),
    {ok, Timer} = timer:send_after(1_000, refresh_data),
    {ok, Timer}.

handle_call(collect_data, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_data, _State) ->
    collect_metrics(),
    {ok, Timer} = timer:send_after(30_000, refresh_data),
    {noreply, Timer};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

declare_metrics() ->
     prometheus_gauge:declare([{name, vp_temperature_c}, {help, "Temperature in C"}, {labels, [location]}])
    ,prometheus_gauge:declare([{name, vp_wind_speed_kph}, {help, "Wind speed in kilometers per hour"}])
    ,prometheus_gauge:declare([{name, vp_wind_direction_degrees}, {help, "Wind direction in degrees from north"}])
    ,prometheus_gauge:declare([{name, vp_rainfall_mmph}, {help, "Rainfall in mm/hour"}])
    ,prometheus_gauge:declare([{name, vp_humidity_percent}, {help, "Relative humidity"}, {labels, [location]}]).

collect_metrics() ->
    {data, #{
         outside_temperature := OutsideTemp 
        ,inside_temperature := InsideTemp
        ,outside_humidity := OutsideHumid
        ,inside_humidity := InsideHumid
        ,wind_speed := WindSpeed
        ,wind_direction := WindDirection
        ,rain_rate := RainRate
    }} = vp_connection:get_data()
    ,set_or_unset(vp_temperature_c, [outside], OutsideTemp)
    ,set_or_unset(vp_temperature_c, [inside], InsideTemp)
    ,set_or_unset(vp_humidity_percent, [outside], OutsideHumid)
    ,set_or_unset(vp_humidity_percent, [inside], InsideHumid)
    ,set_or_unset(vp_wind_direction_degrees, [], WindDirection)
    ,set_or_unset(vp_rainfall_mmph, [], RainRate)
    ,set_or_unset(vp_wind_speed_kph, [], WindSpeed).

set_or_unset(Name, Labels, undefined) ->
    prometheus_gauge:remove(default, Name, Labels);
set_or_unset(Name, Labels, Value) ->
    prometheus_gauge:set(Name, Labels, Value).