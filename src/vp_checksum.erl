-module(vp_checksum).

-export([check/1]).

-include_lib("kernel/include/logger.hrl").

-spec check(binary()) -> ok | error.
check(Bin) ->
    check(Bin, <<0>>).

-spec check(binary(), binary()) -> ok | error.
check(<<>>, <<0>>) ->
    ?LOG_DEBUG(#{msg=>"checksum OK"}),
    ok;
check(<<>>, CRC) ->
    ?LOG_DEBUG(#{msg=>"checksum bad", crc => CRC}),
    error;
check(<<Char:8, Rest/binary>>, CRC) ->
    RightShifted = shift_right(CRC, 8),
    LeftShifted = shift_left(CRC, 8),
    TableIndex = binary:decode_unsigned(RightShifted) bxor Char,
    ?LOG_DEBUG(#{char=>Char, rest=>Rest, crc => CRC, right_shifted => RightShifted, left_shifted => LeftShifted, table_index => TableIndex}),
    TableValue = crc_table(TableIndex),
    NewCRC = TableValue bxor binary:decode_unsigned(LeftShifted),
    check(Rest, binary:encode_unsigned(NewCRC)).

-spec shift_left(binary(), non_neg_integer()) -> binary().
shift_left(Bin, Bits) when Bits >= 0 ->
    ?LOG_DEBUG(#{msg => "left shift", bin => binary:decode_unsigned(Bin), bits => Bits}),
    <<_:Bits, Rest/binary>> = Bin,
    <<Rest/binary, 0:Bits>>.

-spec shift_right(binary(), non_neg_integer()) -> binary().
shift_right(Bin, Bits) when Bits >= 0 andalso is_binary(Bin) ->
    ?LOG_DEBUG(#{msg => "right shift", bin => binary:decode_unsigned(Bin), bits => Bits}),
    Keep = erlang:bit_size(<<Bin/bitstring>>) - Bits,
    <<Rest:Keep/bits, _:Bits>> = Bin,
    <<0:Bits, Rest/binary>>.

-spec crc_table(non_neg_integer()) -> non_neg_integer().
crc_table(0)   -> 16#0;
crc_table(1)   -> 16#1021;
crc_table(2)   -> 16#2042;
crc_table(3)   -> 16#3063;
crc_table(4)   -> 16#4084;
crc_table(5)   -> 16#50a5;
crc_table(6)   -> 16#60c6;
crc_table(7)   -> 16#70e7;
crc_table(8)   -> 16#8108;
crc_table(9)   -> 16#9129;
crc_table(10)  -> 16#a14a;
crc_table(11)  -> 16#b16b;
crc_table(12)  -> 16#c18c;
crc_table(13)  -> 16#d1ad;
crc_table(14)  -> 16#e1ce;
crc_table(15)  -> 16#f1ef;
crc_table(16)  -> 16#1231;
crc_table(17)  -> 16#210;
crc_table(18)  -> 16#3273;
crc_table(19)  -> 16#2252;
crc_table(20)  -> 16#52b5;
crc_table(21)  -> 16#4294;
crc_table(22)  -> 16#72f7;
crc_table(23)  -> 16#62d6;
crc_table(24)  -> 16#9339;
crc_table(25)  -> 16#8318;
crc_table(26)  -> 16#b37b;
crc_table(27)  -> 16#a35a;
crc_table(28)  -> 16#d3bd;
crc_table(29)  -> 16#c39c;
crc_table(30)  -> 16#f3ff;
crc_table(31)  -> 16#e3de;
crc_table(32)  -> 16#2462;
crc_table(33)  -> 16#3443;
crc_table(34)  -> 16#420;
crc_table(35)  -> 16#1401;
crc_table(36)  -> 16#64e6;
crc_table(37)  -> 16#74c7;
crc_table(38)  -> 16#44a4;
crc_table(39)  -> 16#5485;
crc_table(40)  -> 16#a56a;
crc_table(41)  -> 16#b54b;
crc_table(42)  -> 16#8528;
crc_table(43)  -> 16#9509;
crc_table(44)  -> 16#e5ee;
crc_table(45)  -> 16#f5cf;
crc_table(46)  -> 16#c5ac;
crc_table(47)  -> 16#d58d;
crc_table(48)  -> 16#3653;
crc_table(49)  -> 16#2672;
crc_table(50)  -> 16#1611;
crc_table(51)  -> 16#630;
crc_table(52)  -> 16#76d7;
crc_table(53)  -> 16#66f6;
crc_table(54)  -> 16#5695;
crc_table(55)  -> 16#46b4;
crc_table(56)  -> 16#b75b;
crc_table(57)  -> 16#a77a;
crc_table(58)  -> 16#9719;
crc_table(59)  -> 16#8738;
crc_table(60)  -> 16#f7df;
crc_table(61)  -> 16#e7fe;
crc_table(62)  -> 16#d79d;
crc_table(63)  -> 16#c7bc;
crc_table(64)  -> 16#48c4;
crc_table(65)  -> 16#58e5;
crc_table(66)  -> 16#6886;
crc_table(67)  -> 16#78a7;
crc_table(68)  -> 16#840;
crc_table(69)  -> 16#1861;
crc_table(70)  -> 16#2802;
crc_table(71)  -> 16#3823;
crc_table(72)  -> 16#c9cc;
crc_table(73)  -> 16#d9ed;
crc_table(74)  -> 16#e98e;
crc_table(75)  -> 16#f9af;
crc_table(76)  -> 16#8948;
crc_table(77)  -> 16#9969;
crc_table(78)  -> 16#a90a;
crc_table(79)  -> 16#b92b;
crc_table(80)  -> 16#5af5;
crc_table(81)  -> 16#4ad4;
crc_table(82)  -> 16#7ab7;
crc_table(83)  -> 16#6a96;
crc_table(84)  -> 16#1a71;
crc_table(85)  -> 16#a50;
crc_table(86)  -> 16#3a33;
crc_table(87)  -> 16#2a12;
crc_table(88)  -> 16#dbfd;
crc_table(89)  -> 16#cbdc;
crc_table(90)  -> 16#fbbf;
crc_table(91)  -> 16#eb9e;
crc_table(92)  -> 16#9b79;
crc_table(93)  -> 16#8b58;
crc_table(94)  -> 16#bb3b;
crc_table(95)  -> 16#ab1a;
crc_table(96)  -> 16#6ca6;
crc_table(97)  -> 16#7c87;
crc_table(98)  -> 16#4ce4;
crc_table(99)  -> 16#5cc5;
crc_table(100) -> 16#2c22;
crc_table(101) -> 16#3c03;
crc_table(102) -> 16#c60;
crc_table(103) -> 16#1c41;
crc_table(104) -> 16#edae;
crc_table(105) -> 16#fd8f;
crc_table(106) -> 16#cdec;
crc_table(107) -> 16#ddcd;
crc_table(108) -> 16#ad2a;
crc_table(109) -> 16#bd0b;
crc_table(110) -> 16#8d68;
crc_table(111) -> 16#9d49;
crc_table(112) -> 16#7e97;
crc_table(113) -> 16#6eb6;
crc_table(114) -> 16#5ed5;
crc_table(115) -> 16#4ef4;
crc_table(116) -> 16#3e13;
crc_table(117) -> 16#2e32;
crc_table(118) -> 16#1e51;
crc_table(119) -> 16#e70;
crc_table(120) -> 16#ff9f;
crc_table(121) -> 16#efbe;
crc_table(122) -> 16#dfdd;
crc_table(123) -> 16#cffc;
crc_table(124) -> 16#bf1b;
crc_table(125) -> 16#af3a;
crc_table(126) -> 16#9f59;
crc_table(127) -> 16#8f78;
crc_table(128) -> 16#9188;
crc_table(129) -> 16#81a9;
crc_table(130) -> 16#b1ca;
crc_table(131) -> 16#a1eb;
crc_table(132) -> 16#d10c;
crc_table(133) -> 16#c12d;
crc_table(134) -> 16#f14e;
crc_table(135) -> 16#e16f;
crc_table(136) -> 16#1080;
crc_table(137) -> 16#a1;
crc_table(138) -> 16#30c2;
crc_table(139) -> 16#20e3;
crc_table(140) -> 16#5004;
crc_table(141) -> 16#4025;
crc_table(142) -> 16#7046;
crc_table(143) -> 16#6067;
crc_table(144) -> 16#83b9;
crc_table(145) -> 16#9398;
crc_table(146) -> 16#a3fb;
crc_table(147) -> 16#b3da;
crc_table(148) -> 16#c33d;
crc_table(149) -> 16#d31c;
crc_table(150) -> 16#e37f;
crc_table(151) -> 16#f35e;
crc_table(152) -> 16#2b1;
crc_table(153) -> 16#1290;
crc_table(154) -> 16#22f3;
crc_table(155) -> 16#32d2;
crc_table(156) -> 16#4235;
crc_table(157) -> 16#5214;
crc_table(158) -> 16#6277;
crc_table(159) -> 16#7256;
crc_table(160) -> 16#b5ea;
crc_table(161) -> 16#a5cb;
crc_table(162) -> 16#95a8;
crc_table(163) -> 16#8589;
crc_table(164) -> 16#f56e;
crc_table(165) -> 16#e54f;
crc_table(166) -> 16#d52c;
crc_table(167) -> 16#c50d;
crc_table(168) -> 16#34e2;
crc_table(169) -> 16#24c3;
crc_table(170) -> 16#14a0;
crc_table(171) -> 16#481;
crc_table(172) -> 16#7466;
crc_table(173) -> 16#6447;
crc_table(174) -> 16#5424;
crc_table(175) -> 16#4405;
crc_table(176) -> 16#a7db;
crc_table(177) -> 16#b7fa;
crc_table(178) -> 16#8799;
crc_table(179) -> 16#97b8;
crc_table(180) -> 16#e75f;
crc_table(181) -> 16#f77e;
crc_table(182) -> 16#c71d;
crc_table(183) -> 16#d73c;
crc_table(184) -> 16#26d3;
crc_table(185) -> 16#36f2;
crc_table(186) -> 16#691;
crc_table(187) -> 16#16b0;
crc_table(188) -> 16#6657;
crc_table(189) -> 16#7676;
crc_table(190) -> 16#4615;
crc_table(191) -> 16#5634;
crc_table(192) -> 16#d94c;
crc_table(193) -> 16#c96d;
crc_table(194) -> 16#f90e;
crc_table(195) -> 16#e92f;
crc_table(196) -> 16#99c8;
crc_table(197) -> 16#89e9;
crc_table(198) -> 16#b98a;
crc_table(199) -> 16#a9ab;
crc_table(200) -> 16#5844;
crc_table(201) -> 16#4865;
crc_table(202) -> 16#7806;
crc_table(203) -> 16#6827;
crc_table(204) -> 16#18c0;
crc_table(205) -> 16#8e1;
crc_table(206) -> 16#3882;
crc_table(207) -> 16#28a3;
crc_table(208) -> 16#cb7d;
crc_table(209) -> 16#db5c;
crc_table(210) -> 16#eb3f;
crc_table(211) -> 16#fb1e;
crc_table(212) -> 16#8bf9;
crc_table(213) -> 16#9bd8;
crc_table(214) -> 16#abbb;
crc_table(215) -> 16#bb9a;
crc_table(216) -> 16#4a75;
crc_table(217) -> 16#5a54;
crc_table(218) -> 16#6a37;
crc_table(219) -> 16#7a16;
crc_table(220) -> 16#af1;
crc_table(221) -> 16#1ad0;
crc_table(222) -> 16#2ab3;
crc_table(223) -> 16#3a92;
crc_table(224) -> 16#fd2e;
crc_table(225) -> 16#ed0f;
crc_table(226) -> 16#dd6c;
crc_table(227) -> 16#cd4d;
crc_table(228) -> 16#bdaa;
crc_table(229) -> 16#ad8b;
crc_table(230) -> 16#9de8;
crc_table(231) -> 16#8dc9;
crc_table(232) -> 16#7c26;
crc_table(233) -> 16#6c07;
crc_table(234) -> 16#5c64;
crc_table(235) -> 16#4c45;
crc_table(236) -> 16#3ca2;
crc_table(237) -> 16#2c83;
crc_table(238) -> 16#1ce0;
crc_table(239) -> 16#cc1;
crc_table(240) -> 16#ef1f;
crc_table(241) -> 16#ff3e;
crc_table(242) -> 16#cf5d;
crc_table(243) -> 16#df7c;
crc_table(244) -> 16#af9b;
crc_table(245) -> 16#bfba;
crc_table(246) -> 16#8fd9;
crc_table(247) -> 16#9ff8;
crc_table(248) -> 16#6e17;
crc_table(249) -> 16#7e36;
crc_table(250) -> 16#4e55;
crc_table(251) -> 16#5e74;
crc_table(252) -> 16#2e93;
crc_table(253) -> 16#3eb2;
crc_table(254) -> 16#ed1;
crc_table(255) -> 16#1ef0.

