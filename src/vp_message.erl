-module(vp_message).
-export([
     decode_time/1
    ,decode_loop/1
]).
-include_lib("kernel/include/logger.hrl").

-spec decode_time(binary()) -> calendar:datetime().
decode_time(Bin) ->
    % << CheckBuffer/bits>> = Bin,
    % ok = vp_checksum:check(CheckBuffer),
    <<Prelude:16/bits,Sec:8,Min:8,Hr:8,Day:8,Mon:8,Yr:8,Rest/binary>> = Bin,
    Date = {1900 + Yr, Mon, Day},
    Time = {Hr, Min, Sec},
    DateTime = {Date, Time},
    ?LOG_DEBUG(#{msg => "decoded time", datetime=>DateTime, leftover_bytes=>Rest, prelude_byes=>Prelude}),
    DateTime.

decode_loop(<<
    "LOO", 
    BarTrend:8/unsigned-integer-little, 
    PacketType:8/unsigned-integer-little, 
    NextRecord:16/unsigned-integer-little,
    Barometer:16/unsigned-integer-little,
    InsideTemperature:16/signed-integer-little,
    InsideHumidity:8/unsigned-integer-little,
    OutsideTemperature:16/signed-integer-little,
    WindSpeed:8/unsigned-integer-little,
    WindSpeed10mAvg:8/unsigned-integer-little,
    WindDirection:16/unsigned-integer-little,
    ExtraTemperature:7/binary,
    SoilTemperature:4/binary,
    LeafTemperature:4/binary,
    OutsideHumidity:8/unsigned-integer-little,
    ExtraHumidity:7/binary,
    RainRate:16/unsigned-integer-little,
    UVIndex:8/unsigned-integer-little,
    SolarRadiation:16/unsigned-integer-little,
    StormRain:16/unsigned-integer-little,
    StormStartDate:16/unsigned-integer-little,
    DayRain:16/unsigned-integer-little,
    MonthRain:16/unsigned-integer-little,
    YearRain:16/unsigned-integer-little,
    DayET:16/unsigned-integer-little,
    MonthET:16/unsigned-integer-little,
    YearET:16/unsigned-integer-little,
    SoilMoisture:32/unsigned-integer-little,
    LeafWetness:32/unsigned-integer-little,
    InsideAlarms:8/bits,
    RainAlarms:8/bits,
    OutsideAlarms:16/bits,
    ExtraAlarms:8/binary,
    SoilLeafAlarms:4/binary,
    TransmitterBattery:8/unsigned-integer-little,
    ConsoleBattery:16/unsigned-integer-little,
    ForecastIcons:8/unsigned-integer-little,
    ForecastRuleNumber:8/unsigned-integer-little,
    SunriseTime:16/bits,
    SunsetTime:16/bits,
    "\n","\r",
    CRC:16/unsigned-integer-little
>>) ->
    % CRC check?
    #{
        bar_trend => bar_trend(BarTrend), 
        packet_type => packet_type(PacketType), 
        next_record => NextRecord,
        barometer => Barometer,
        inside_temperature => temperature(InsideTemperature),
        inside_humidity => humidity(InsideHumidity),
        outside_temperature => temperature(OutsideTemperature),
        wind_speed => wind_speed(WindSpeed),
        wind_speed_10m_avg => WindSpeed10mAvg,
        wind_direction => wind_direction(WindDirection),
        extra_temperatures => ExtraTemperature,
        soil_temperatures => SoilTemperature,
        leaf_temperatures => LeafTemperature,
        outside_humidity => humidity(OutsideHumidity),
        extra_humidities => ExtraHumidity,
        rain_rate => rain_rate(RainRate),
        uv_index => UVIndex,
        solar_radiation => SolarRadiation,
        storm_rain => StormRain,
        storm_start => StormStartDate,
        rain_daily => DayRain,
        rain_monthly => MonthRain,
        rain_yearly => YearRain,
        et_daily => DayET,
        et_monthly => MonthET,
        et_yearly => YearET,
        soil_moisture => SoilMoisture,
        leaf_wetness => LeafWetness,
        inside_alarms => InsideAlarms,
        rain_alarms => RainAlarms,
        outside_alarms => OutsideAlarms,
        extra_alarms => ExtraAlarms,
        soil_and_leaf_alarms => SoilLeafAlarms,
        transmitter_battery => TransmitterBattery,
        console_battery => ConsoleBattery,
        forecast_icons => forecast_icons(ForecastIcons),
        forecast => forecast_rule(ForecastRuleNumber),
        sunrise => SunriseTime,
        sunset => SunsetTime,
        crc => CRC
    }.

temperature(F10) ->
    C10 = (F10 - 320) / 1.8,
    C = round(C10) / 10,
    if 
        C >= -40 andalso C =< 60 -> C;
        true -> undefined
    end.

humidity(H) when H >= 0 andalso H =< 100 ->
    H;
humidity(_H) ->
    undefined.

wind_speed(MPH) ->
    KPH100 = MPH * 160.9344,
    KPH = round(KPH100) / 100,
    if 
        KPH >= 0 andalso KPH =< 100 -> KPH;
        true -> undefined
    end.

wind_direction(D) when D > 0 andalso D < 360 -> D;
wind_direction(360) -> 0;
wind_direction(0) -> undefined;
wind_direction(_D) -> undefined.

rain_rate(Clicks) -> Clicks * 0.2.

packet_type(0) -> loop;
packet_type(1) -> loop2.

bar_trend(196) -> falling_rapidly;
bar_trend(236) -> falling_slowly;
bar_trend(0) -> steady;
bar_trend(20) -> rising_slowly;
bar_trend(60) -> rising_rapidly;
bar_trend(80) -> unsupported;
bar_trend(_) -> insufficient_data.

forecast_icons(8) -> [sun];
forecast_icons(6) -> [sun, cloud];
forecast_icons(2) -> [cloud];
forecast_icons(3) -> [cloud, rain];
forecast_icons(18) -> [cloud, snow];
forecast_icons(19) -> [cloud, rain, snow];
forecast_icons(7) -> [sun, cloud, rain];
forecast_icons(22) -> [sun, cloud, snow];
forecast_icons(23) -> [sun, cloud, rain, snow].

forecast_rule(0)   -> "Mostly clear and cooler.";
forecast_rule(1)   -> "Mostly clear with little temperature change.";
forecast_rule(2)   -> "Mostly clear for 12 hrs. with little temperature change.";
forecast_rule(3)   -> "Mostly clear for 12 to 24 hrs. and cooler.";
forecast_rule(4)   -> "Mostly clear with little temperature change.";
forecast_rule(5)   -> "Partly cloudy and cooler.";
forecast_rule(6)   -> "Partly cloudy with little temperature change.";
forecast_rule(7)   -> "Partly cloudy with little temperature change.";
forecast_rule(8)   -> "Mostly clear and warmer.";
forecast_rule(9)   -> "Partly cloudy with little temperature change.";
forecast_rule(10)  -> "Partly cloudy with little temperature change.";
forecast_rule(11)  -> "Mostly clear with little temperature change.";
forecast_rule(12)  -> "Increasing clouds and warmer. Precipitation possible within 24 to 48 hrs.";
forecast_rule(13)  -> "Partly cloudy with little temperature change.";
forecast_rule(14)  -> "Mostly clear with little temperature change.";
forecast_rule(15)  -> "Increasing clouds with little temperature change. Precipitation possible within 24 hrs.";
forecast_rule(16)  -> "Mostly clear with little temperature change.";
forecast_rule(17)  -> "Partly cloudy with little temperature change.";
forecast_rule(18)  -> "Mostly clear with little temperature change.";
forecast_rule(19)  -> "Increasing clouds with little temperature change. Precipitation possible within 12 hrs.";
forecast_rule(20)  -> "Mostly clear with little temperature change.";
forecast_rule(21)  -> "Partly cloudy with little temperature change.";
forecast_rule(22)  -> "Mostly clear with little temperature change.";
forecast_rule(23)  -> "Increasing clouds and warmer. Precipitation possible within 24 hrs.";
forecast_rule(24)  -> "Mostly clear and warmer. Increasing winds.";
forecast_rule(25)  -> "Partly cloudy with little temperature change.";
forecast_rule(26)  -> "Mostly clear with little temperature change.";
forecast_rule(27)  -> "Increasing clouds and warmer. Precipitation possible within 12 hrs. Increasing winds.";
forecast_rule(28)  -> "Mostly clear and warmer. Increasing winds.";
forecast_rule(29)  -> "Increasing clouds and warmer.";
forecast_rule(30)  -> "Partly cloudy with little temperature change.";
forecast_rule(31)  -> "Mostly clear with little temperature change.";
forecast_rule(32)  -> "Increasing clouds and warmer. Precipitation possible within 12 hrs. Increasing winds.";
forecast_rule(33)  -> "Mostly clear and warmer. Increasing winds.";
forecast_rule(34)  -> "Increasing clouds and warmer.";
forecast_rule(35)  -> "Partly cloudy with little temperature change.";
forecast_rule(36)  -> "Mostly clear with little temperature change.";
forecast_rule(37)  -> "Increasing clouds and warmer. Precipitation possible within 12 hrs. Increasing winds.";
forecast_rule(38)  -> "Partly cloudy with little temperature change.";
forecast_rule(39)  -> "Mostly clear with little temperature change.";
forecast_rule(40)  -> "Mostly clear and warmer. Precipitation possible within 48 hrs.";
forecast_rule(41)  -> "Mostly clear and warmer.";
forecast_rule(42)  -> "Partly cloudy with little temperature change.";
forecast_rule(43)  -> "Mostly clear with little temperature change.";
forecast_rule(44)  -> "Increasing clouds with little temperature change. Precipitation possible within 24 to 48 hrs.";
forecast_rule(45)  -> "Increasing clouds with little temperature change.";
forecast_rule(46)  -> "Partly cloudy with little temperature change.";
forecast_rule(47)  -> "Mostly clear with little temperature change.";
forecast_rule(48)  -> "Increasing clouds and warmer. Precipitation possible within 12 to 24 hrs.";
forecast_rule(49)  -> "Partly cloudy with little temperature change.";
forecast_rule(50)  -> "Mostly clear with little temperature change.";
forecast_rule(51)  -> "Increasing clouds and warmer. Precipitation possible within 12 to 24 hrs. Windy.";
forecast_rule(52)  -> "Partly cloudy with little temperature change.";
forecast_rule(53)  -> "Mostly clear with little temperature change.";
forecast_rule(54)  -> "Increasing clouds and warmer. Precipitation possible within 12 to 24 hrs. Windy.";
forecast_rule(55)  -> "Partly cloudy with little temperature change.";
forecast_rule(56)  -> "Mostly clear with little temperature change.";
forecast_rule(57)  -> "Increasing clouds and warmer. Precipitation possible within 6 to 12 hrs.";
forecast_rule(58)  -> "Partly cloudy with little temperature change.";
forecast_rule(59)  -> "Mostly clear with little temperature change.";
forecast_rule(60)  -> "Increasing clouds and warmer. Precipitation possible within 6 to 12 hrs. Windy.";
forecast_rule(61)  -> "Partly cloudy with little temperature change.";
forecast_rule(62)  -> "Mostly clear with little temperature change.";
forecast_rule(63)  -> "Increasing clouds and warmer. Precipitation possible within 12 to 24 hrs. Windy.";
forecast_rule(64)  -> "Partly cloudy with little temperature change.";
forecast_rule(65)  -> "Mostly clear with little temperature change.";
forecast_rule(66)  -> "Increasing clouds and warmer. Precipitation possible within 12 hrs.";
forecast_rule(67)  -> "Partly cloudy with little temperature change.";
forecast_rule(68)  -> "Mostly clear with little temperature change.";
forecast_rule(69)  -> "Increasing clouds and warmer. Precipitation likely.";
forecast_rule(70)  -> "clearing and cooler. Precipitation ending within 6 hrs.";
forecast_rule(71)  -> "Partly cloudy with little temperature change.";
forecast_rule(72)  -> "clearing and cooler. Precipitation ending within 6 hrs.";
forecast_rule(73)  -> "Mostly clear with little temperature change.";
forecast_rule(74)  -> "Clearing and cooler. Precipitation ending within 6 hrs.";
forecast_rule(75)  -> "Partly cloudy and cooler.";
forecast_rule(76)  -> "Partly cloudy with little temperature change.";
forecast_rule(77)  -> "Mostly clear and cooler.";
forecast_rule(78)  -> "clearing and cooler. Precipitation ending within 6 hrs.";
forecast_rule(79)  -> "Mostly clear with little temperature change.";
forecast_rule(80)  -> "Clearing and cooler. Precipitation ending within 6 hrs.";
forecast_rule(81)  -> "Mostly clear and cooler.";
forecast_rule(82)  -> "Partly cloudy with little temperature change.";
forecast_rule(83)  -> "Mostly clear with little temperature change.";
forecast_rule(84)  -> "Increasing clouds with little temperature change. Precipitation possible within 24 hrs.";
forecast_rule(85)  -> "Mostly cloudy and cooler. Precipitation continuing.";
forecast_rule(86)  -> "Partly cloudy with little temperature change.";
forecast_rule(87)  -> "Mostly clear with little temperature change.";
forecast_rule(88)  -> "Mostly cloudy and cooler. Precipitation likely.";
forecast_rule(89)  -> "Mostly cloudy with little temperature change. Precipitation continuing.";
forecast_rule(90)  -> "Mostly cloudy with little temperature change. Precipitation likely.";
forecast_rule(91)  -> "Partly cloudy with little temperature change.";
forecast_rule(92)  -> "Mostly clear with little temperature change.";
forecast_rule(93)  -> "Increasing clouds and cooler. Precipitation possible and windy within 6 hrs.";
forecast_rule(94)  -> "Increasing clouds with little temperature change. Precipitation possible and windy within 6 hrs.";
forecast_rule(95)  -> "Mostly cloudy and cooler. Precipitation continuing. Increasing winds.";
forecast_rule(96)  -> "Partly cloudy with little temperature change.";
forecast_rule(97)  -> "Mostly clear with little temperature change.";
forecast_rule(98)  -> "Mostly cloudy and cooler. Precipitation likely. Increasing winds.";
forecast_rule(99)  -> "Mostly cloudy with little temperature change. Precipitation continuing. Increasing winds.";
forecast_rule(100) -> "Mostly cloudy with little temperature change. Precipitation likely. Increasing winds.";
forecast_rule(101) -> "Partly cloudy with little temperature change.";
forecast_rule(102) -> "Mostly clear with little temperature change.";
forecast_rule(103) -> "Increasing clouds and cooler. Precipitation possible within 12 to 24 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(104) -> "Increasing clouds with little temperature change. Precipitation possible within 12 to 24 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(105) -> "Partly cloudy with little temperature change.";
forecast_rule(106) -> "Mostly clear with little temperature change.";
forecast_rule(107) -> "Increasing clouds and cooler. Precipitation possible within 6 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(108) -> "Increasing clouds with little temperature change. Precipitation possible within 6 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(109) -> "Mostly cloudy and cooler. Precipitation ending within 12 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(110) -> "Mostly cloudy and cooler. Possible wind shift to the W, NW, or N.";
forecast_rule(111) -> "Mostly cloudy with little temperature change. Precipitation ending within 12 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(112) -> "Mostly cloudy with little temperature change. Possible wind shift to the W, NW, or N.";
forecast_rule(113) -> "Mostly cloudy and cooler. Precipitation ending within 12 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(114) -> "Partly cloudy with little temperature change.";
forecast_rule(115) -> "Mostly clear with little temperature change.";
forecast_rule(116) -> "Mostly cloudy and cooler. Precipitation possible within 24 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(117) -> "Mostly cloudy with little temperature change. Precipitation ending within 12 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(118) -> "Mostly cloudy with little temperature change. Precipitation possible within 24 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(119) -> "clearing, cooler and windy. Precipitation ending within 6 hrs.";
forecast_rule(120) -> "clearing, cooler and windy.";
forecast_rule(121) -> "Mostly cloudy and cooler. Precipitation ending within 6 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(122) -> "Mostly cloudy and cooler. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(123) -> "clearing, cooler and windy.";
forecast_rule(124) -> "Partly cloudy with little temperature change.";
forecast_rule(125) -> "Mostly clear with little temperature change.";
forecast_rule(126) -> "Mostly cloudy with little temperature change. Precipitation possible within 12 hrs. Windy.";
forecast_rule(127) -> "Partly cloudy with little temperature change.";
forecast_rule(128) -> "Mostly clear with little temperature change.";
forecast_rule(129) -> "Increasing clouds and cooler. Precipitation possible within 12 hrs., possibly heavy at times. Windy.";
forecast_rule(130) -> "Mostly cloudy and cooler. Precipitation ending within 6 hrs. Windy.";
forecast_rule(131) -> "Partly cloudy with little temperature change.";
forecast_rule(132) -> "Mostly clear with little temperature change.";
forecast_rule(133) -> "Mostly cloudy and cooler. Precipitation possible within 12 hrs. Windy.";
forecast_rule(134) -> "Mostly cloudy and cooler. Precipitation ending in 12 to 24 hrs.";
forecast_rule(135) -> "Mostly cloudy and cooler.";
forecast_rule(136) -> "Mostly cloudy and cooler. Precipitation continuing, possible heavy at times. Windy.";
forecast_rule(137) -> "Partly cloudy with little temperature change.";
forecast_rule(138) -> "Mostly clear with little temperature change.";
forecast_rule(139) -> "Mostly cloudy and cooler. Precipitation possible within 6 to 12 hrs. Windy.";
forecast_rule(140) -> "Mostly cloudy with little temperature change. Precipitation continuing, possibly heavy at times. Windy.";
forecast_rule(141) -> "Partly cloudy with little temperature change.";
forecast_rule(142) -> "Mostly clear with little temperature change.";
forecast_rule(143) -> "Mostly cloudy with little temperature change. Precipitation possible within 6 to 12 hrs. Windy.";
forecast_rule(144) -> "Partly cloudy with little temperature change.";
forecast_rule(145) -> "Mostly clear with little temperature change.";
forecast_rule(146) -> "Increasing clouds with little temperature change. Precipitation possible within 12 hrs., possibly heavy at times. Windy.";
forecast_rule(147) -> "Mostly cloudy and cooler. Windy.";
forecast_rule(148) -> "Mostly cloudy and cooler. Precipitation continuing, possibly heavy at times. Windy.";
forecast_rule(149) -> "Partly cloudy with little temperature change.";
forecast_rule(150) -> "Mostly clear with little temperature change.";
forecast_rule(151) -> "Mostly cloudy and cooler. Precipitation likely, possibly heavy at times. Windy.";
forecast_rule(152) -> "Mostly cloudy with little temperature change. Precipitation continuing, possibly heavy at times. Windy.";
forecast_rule(153) -> "Mostly cloudy with little temperature change. Precipitation likely, possibly heavy at times. Windy.";
forecast_rule(154) -> "Partly cloudy with little temperature change.";
forecast_rule(155) -> "Mostly clear with little temperature change.";
forecast_rule(156) -> "Increasing clouds and cooler. Precipitation possible within 6 hrs. Windy.";
forecast_rule(157) -> "Increasing clouds with little temperature change. Precipitation possible within 6 hrs. windy";
forecast_rule(158) -> "Increasing clouds and cooler. Precipitation continuing. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(159) -> "Partly cloudy with little temperature change.";
forecast_rule(160) -> "Mostly clear with little temperature change.";
forecast_rule(161) -> "Mostly cloudy and cooler. Precipitation likely. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(162) -> "Mostly cloudy with little temperature change. Precipitation continuing. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(163) -> "Mostly cloudy with little temperature change. Precipitation likely. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(164) -> "Increasing clouds and cooler. Precipitation possible within 6 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(165) -> "Partly cloudy with little temperature change.";
forecast_rule(166) -> "Mostly clear with little temperature change.";
forecast_rule(167) -> "Increasing clouds and cooler. Precipitation possible within 6 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(168) -> "Increasing clouds with little temperature change. Precipitation possible within 6 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(169) -> "Increasing clouds with little temperature change. Precipitation possible within 6 hrs. Possible wind shift to the W, NW, or N.";
forecast_rule(170) -> "Partly cloudy with little temperature change.";
forecast_rule(171) -> "Mostly clear with little temperature change.";
forecast_rule(172) -> "Increasing clouds and cooler. Precipitation possible within 6 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(173) -> "Increasing clouds with little temperature change. Precipitation possible within 6 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(174) -> "Partly cloudy with little temperature change.";
forecast_rule(175) -> "Mostly clear with little temperature change.";
forecast_rule(176) -> "Increasing clouds and cooler. Precipitation possible within 12 to 24 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(177) -> "Increasing clouds with little temperature change. Precipitation possible within 12 to 24 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(178) -> "Mostly cloudy and cooler. Precipitation possibly heavy at times and ending within 12 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(179) -> "Partly cloudy with little temperature change.";
forecast_rule(180) -> "Mostly clear with little temperature change.";
forecast_rule(181) -> "Mostly cloudy and cooler. Precipitation possible within 6 to 12 hrs., possibly heavy at times. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(182) -> "Mostly cloudy with little temperature change. Precipitation ending within 12 hrs. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(183) -> "Mostly cloudy with little temperature change. Precipitation possible within 6 to 12 hrs., possibly heavy at times. Windy with possible wind shift to the W, NW, or N.";
forecast_rule(184) -> "Mostly cloudy and cooler. Precipitation continuing.";
forecast_rule(185) -> "Partly cloudy with little temperature change.";
forecast_rule(186) -> "Mostly clear with little temperature change.";
forecast_rule(187) -> "Mostly cloudy and cooler. Precipitation likely, windy with possible wind shift to the W, NW, or N.";
forecast_rule(188) -> "Mostly cloudy with little temperature change. Precipitation continuing.";
forecast_rule(189) -> "Mostly cloudy with little temperature change. Precipitation likely.";
forecast_rule(190) -> "Partly cloudy with little temperature change.";
forecast_rule(191) -> "Mostly clear with little temperature change.";
forecast_rule(192) -> "Mostly cloudy and cooler. Precipitation possible within 12 hours, possibly heavy at times. Windy.";
forecast_rule(193) -> "FORECAST REQUIRES 3 HRS. OF RECENT DATA";
forecast_rule(194) -> "Mostly clear and cooler.".