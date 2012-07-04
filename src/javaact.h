// $Id: javaact.h,v 1.19 2001/09/16 01:38:25 ericb Exp $ -*- c++ -*-
// DO NOT MODIFY THIS FILE - it is generated using jikespg on java.g.
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef HEADERS

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

void Parser::InitRuleAction()
{
    rule_action[0] = &Parser::BadAction;
#else // HEADERS
    void BadAction(void);
    void NoAction(void);
    void NullAction(void);
    void MakeArrayType(void);
    void MakeSimpleName(void);
    void MakeFieldAccess(void);
    void MakeQualifiedSuper(void);
    void MakeQualifiedNew(void);
    void SetSym1ToSym2(void);
    void MakeEmptyStatement(void);
    void MakeLabeledStatement(void);
    void MakeExpressionStatement(void);
    void MakeIfThenElseStatement(void);
    void MakeWhileStatement(void);
    void MakeForStatement(void);
    void MakeArrayCreationExpression(void);
    void MakeSuperFieldAccess(void);
    void MakeSuperDoubleFieldAccess(void);
    void MakeArrayAccess(void);
    void MakeCastExpression(void);
#endif // HEADERS

#ifndef HEADERS
    rule_action[1] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[2] = &Parser::Act2;
#else
    void Act2(void);
#endif

#ifndef HEADERS
    rule_action[3] = &Parser::Act3;
#else
    void Act3(void);
#endif

#ifndef HEADERS
    rule_action[4] = &Parser::Act4;
#else
    void Act4(void);
#endif

#ifndef HEADERS
    rule_action[5] = &Parser::Act5;
#else
    void Act5(void);
#endif

#ifndef HEADERS
    rule_action[6] = &Parser::Act6;
#else
    void Act6(void);
#endif

#ifndef HEADERS
    rule_action[7] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[8] = &Parser::Act8;
#else
    void Act8(void);
#endif

#ifndef HEADERS
    rule_action[9] = &Parser::Act9;
#else
    void Act9(void);
#endif

#ifndef HEADERS
    rule_action[10] = &Parser::Act10;
#else
    void Act10(void);
#endif

#ifndef HEADERS
    rule_action[11] = &Parser::Act11;
#else
    void Act11(void);
#endif

#ifndef HEADERS
    rule_action[12] = &Parser::Act12;
#else
    void Act12(void);
#endif

#ifndef HEADERS
    rule_action[13] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[14] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[15] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[16] = &Parser::Act16;
#else
    void Act16(void);
#endif

#ifndef HEADERS
    rule_action[17] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[18] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[19] = &Parser::Act19;
#else
    void Act19(void);
#endif

#ifndef HEADERS
    rule_action[20] = &Parser::Act20;
#else
    void Act20(void);
#endif

#ifndef HEADERS
    rule_action[21] = &Parser::Act21;
#else
    void Act21(void);
#endif

#ifndef HEADERS
    rule_action[22] = &Parser::Act22;
#else
    void Act22(void);
#endif

#ifndef HEADERS
    rule_action[23] = &Parser::Act23;
#else
    void Act23(void);
#endif

#ifndef HEADERS
    rule_action[24] = &Parser::Act24;
#else
    void Act24(void);
#endif

#ifndef HEADERS
    rule_action[25] = &Parser::Act25;
#else
    void Act25(void);
#endif

#ifndef HEADERS
    rule_action[26] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[27] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[28] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[29] = &Parser::MakeArrayType;
#endif

#ifndef HEADERS
    rule_action[30] = &Parser::MakeArrayType;
#endif

#ifndef HEADERS
    rule_action[31] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[32] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[33] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[34] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[35] = &Parser::MakeSimpleName;
#endif

#ifndef HEADERS
    rule_action[36] = &Parser::MakeFieldAccess;
#endif

#ifndef HEADERS
    rule_action[37] = &Parser::Act37;
#else
    void Act37(void);
#endif

#ifndef HEADERS
    rule_action[38] = &Parser::Act38;
#else
    void Act38(void);
#endif

#ifndef HEADERS
    rule_action[39] = &Parser::Act39;
#else
    void Act39(void);
#endif

#ifndef HEADERS
    rule_action[40] = &Parser::Act40;
#else
    void Act40(void);
#endif

#ifndef HEADERS
    rule_action[41] = &Parser::Act41;
#else
    void Act41(void);
#endif

#ifndef HEADERS
    rule_action[42] = &Parser::Act42;
#else
    void Act42(void);
#endif

#ifndef HEADERS
    rule_action[43] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[44] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[45] = &Parser::Act45;
#else
    void Act45(void);
#endif

#ifndef HEADERS
    rule_action[46] = &Parser::Act46;
#else
    void Act46(void);
#endif

#ifndef HEADERS
    rule_action[47] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[48] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[49] = &Parser::Act49;
#else
    void Act49(void);
#endif

#ifndef HEADERS
    rule_action[50] = &Parser::Act50;
#else
    void Act50(void);
#endif

#ifndef HEADERS
    rule_action[51] = &Parser::Act51;
#else
    void Act51(void);
#endif

#ifndef HEADERS
    rule_action[52] = &Parser::Act52;
#else
    void Act52(void);
#endif

#ifndef HEADERS
    rule_action[53] = &Parser::Act53;
#else
    void Act53(void);
#endif

#ifndef HEADERS
    rule_action[54] = &Parser::Act54;
#else
    void Act54(void);
#endif

#ifndef HEADERS
    rule_action[55] = &Parser::Act55;
#else
    void Act55(void);
#endif

#ifndef HEADERS
    rule_action[56] = &Parser::Act56;
#else
    void Act56(void);
#endif

#ifndef HEADERS
    rule_action[57] = &Parser::Act57;
#else
    void Act57(void);
#endif

#ifndef HEADERS
    rule_action[58] = &Parser::Act58;
#else
    void Act58(void);
#endif

#ifndef HEADERS
    rule_action[59] = &Parser::Act59;
#else
    void Act59(void);
#endif

#ifndef HEADERS
    rule_action[60] = &Parser::Act60;
#else
    void Act60(void);
#endif

#ifndef HEADERS
    rule_action[61] = &Parser::Act61;
#else
    void Act61(void);
#endif

#ifndef HEADERS
    rule_action[62] = &Parser::Act62;
#else
    void Act62(void);
#endif

#ifndef HEADERS
    rule_action[63] = &Parser::Act63;
#else
    void Act63(void);
#endif

#ifndef HEADERS
    rule_action[64] = &Parser::SetSym1ToSym2;
#endif

#ifndef HEADERS
    rule_action[65] = &Parser::SetSym1ToSym2;
#endif

#ifndef HEADERS
    rule_action[66] = &Parser::Act66;
#else
    void Act66(void);
#endif

#ifndef HEADERS
    rule_action[67] = &Parser::Act67;
#else
    void Act67(void);
#endif

#ifndef HEADERS
    rule_action[68] = &Parser::Act68;
#else
    void Act68(void);
#endif

#ifndef HEADERS
    rule_action[69] = &Parser::Act69;
#else
    void Act69(void);
#endif

#ifndef HEADERS
    rule_action[70] = &Parser::Act70;
#else
    void Act70(void);
#endif

#ifndef HEADERS
    rule_action[71] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[72] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[73] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[74] = &Parser::Act74;
#else
    void Act74(void);
#endif

#ifndef HEADERS
    rule_action[75] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[76] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[77] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[78] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[79] = &Parser::Act79;
#else
    void Act79(void);
#endif

#ifndef HEADERS
    rule_action[80] = &Parser::Act80;
#else
    void Act80(void);
#endif

#ifndef HEADERS
    rule_action[81] = &Parser::Act81;
#else
    void Act81(void);
#endif

#ifndef HEADERS
    rule_action[82] = &Parser::Act82;
#else
    void Act82(void);
#endif

#ifndef HEADERS
    rule_action[83] = &Parser::Act83;
#else
    void Act83(void);
#endif

#ifndef HEADERS
    rule_action[84] = &Parser::Act84;
#else
    void Act84(void);
#endif

#ifndef HEADERS
    rule_action[85] = &Parser::Act85;
#else
    void Act85(void);
#endif

#ifndef HEADERS
    rule_action[86] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[87] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[88] = &Parser::Act88;
#else
    void Act88(void);
#endif

#ifndef HEADERS
    rule_action[89] = &Parser::Act89;
#else
    void Act89(void);
#endif

#ifndef HEADERS
    rule_action[90] = &Parser::Act90;
#else
    void Act90(void);
#endif

#ifndef HEADERS
    rule_action[91] = &Parser::Act91;
#else
    void Act91(void);
#endif

#ifndef HEADERS
    rule_action[92] = &Parser::Act92;
#else
    void Act92(void);
#endif

#ifndef HEADERS
    rule_action[93] = &Parser::Act93;
#else
    void Act93(void);
#endif

#ifndef HEADERS
    rule_action[94] = &Parser::Act94;
#else
    void Act94(void);
#endif

#ifndef HEADERS
    rule_action[95] = &Parser::Act95;
#else
    void Act95(void);
#endif

#ifndef HEADERS
    rule_action[96] = &Parser::SetSym1ToSym2;
#endif

#ifndef HEADERS
    rule_action[97] = &Parser::Act97;
#else
    void Act97(void);
#endif

#ifndef HEADERS
    rule_action[98] = &Parser::Act98;
#else
    void Act98(void);
#endif

#ifndef HEADERS
    rule_action[99] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[100] = &Parser::MakeEmptyStatement;
#endif

#ifndef HEADERS
    rule_action[101] = &Parser::Act101;
#else
    void Act101(void);
#endif

#ifndef HEADERS
    rule_action[102] = &Parser::Act102;
#else
    void Act102(void);
#endif

#ifndef HEADERS
    rule_action[103] = &Parser::Act103;
#else
    void Act103(void);
#endif

#ifndef HEADERS
    rule_action[104] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[105] = &Parser::Act105;
#else
    void Act105(void);
#endif

#ifndef HEADERS
    rule_action[106] = &Parser::Act106;
#else
    void Act106(void);
#endif

#ifndef HEADERS
    rule_action[107] = &Parser::Act107;
#else
    void Act107(void);
#endif

#ifndef HEADERS
    rule_action[108] = &Parser::MakeQualifiedSuper;
#endif

#ifndef HEADERS
    rule_action[109] = &Parser::MakeQualifiedSuper;
#endif

#ifndef HEADERS
    rule_action[110] = &Parser::Act110;
#else
    void Act110(void);
#endif

#ifndef HEADERS
    rule_action[111] = &Parser::SetSym1ToSym2;
#endif

#ifndef HEADERS
    rule_action[112] = &Parser::Act112;
#else
    void Act112(void);
#endif

#ifndef HEADERS
    rule_action[113] = &Parser::Act113;
#else
    void Act113(void);
#endif

#ifndef HEADERS
    rule_action[114] = &Parser::Act114;
#else
    void Act114(void);
#endif

#ifndef HEADERS
    rule_action[115] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[116] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[117] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[118] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[119] = &Parser::Act119;
#else
    void Act119(void);
#endif

#ifndef HEADERS
    rule_action[120] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[121] = &Parser::Act121;
#else
    void Act121(void);
#endif

#ifndef HEADERS
    rule_action[122] = &Parser::Act122;
#else
    void Act122(void);
#endif

#ifndef HEADERS
    rule_action[123] = &Parser::Act123;
#else
    void Act123(void);
#endif

#ifndef HEADERS
    rule_action[124] = &Parser::Act124;
#else
    void Act124(void);
#endif

#ifndef HEADERS
    rule_action[125] = &Parser::Act125;
#else
    void Act125(void);
#endif

#ifndef HEADERS
    rule_action[126] = &Parser::Act126;
#else
    void Act126(void);
#endif

#ifndef HEADERS
    rule_action[127] = &Parser::Act127;
#else
    void Act127(void);
#endif

#ifndef HEADERS
    rule_action[128] = &Parser::Act128;
#else
    void Act128(void);
#endif

#ifndef HEADERS
    rule_action[129] = &Parser::Act129;
#else
    void Act129(void);
#endif

#ifndef HEADERS
    rule_action[130] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[131] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[132] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[133] = &Parser::Act133;
#else
    void Act133(void);
#endif

#ifndef HEADERS
    rule_action[134] = &Parser::Act134;
#else
    void Act134(void);
#endif

#ifndef HEADERS
    rule_action[135] = &Parser::Act135;
#else
    void Act135(void);
#endif

#ifndef HEADERS
    rule_action[136] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[137] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[138] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[139] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[140] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[141] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[142] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[143] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[144] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[145] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[146] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[147] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[148] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[149] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[150] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[151] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[152] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[153] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[154] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[155] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[156] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[157] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[158] = &Parser::MakeEmptyStatement;
#endif

#ifndef HEADERS
    rule_action[159] = &Parser::MakeLabeledStatement;
#endif

#ifndef HEADERS
    rule_action[160] = &Parser::MakeLabeledStatement;
#endif

#ifndef HEADERS
    rule_action[161] = &Parser::Act161;
#else
    void Act161(void);
#endif

#ifndef HEADERS
    rule_action[162] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[163] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[164] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[165] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[166] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[167] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[168] = &Parser::MakeExpressionStatement;
#endif

#ifndef HEADERS
    rule_action[169] = &Parser::Act169;
#else
    void Act169(void);
#endif

#ifndef HEADERS
    rule_action[170] = &Parser::MakeIfThenElseStatement;
#endif

#ifndef HEADERS
    rule_action[171] = &Parser::MakeIfThenElseStatement;
#endif

#ifndef HEADERS
    rule_action[172] = &Parser::Act172;
#else
    void Act172(void);
#endif

#ifndef HEADERS
    rule_action[173] = &Parser::Act173;
#else
    void Act173(void);
#endif

#ifndef HEADERS
    rule_action[174] = &Parser::Act174;
#else
    void Act174(void);
#endif

#ifndef HEADERS
    rule_action[175] = &Parser::Act175;
#else
    void Act175(void);
#endif

#ifndef HEADERS
    rule_action[176] = &Parser::Act176;
#else
    void Act176(void);
#endif

#ifndef HEADERS
    rule_action[177] = &Parser::Act177;
#else
    void Act177(void);
#endif

#ifndef HEADERS
    rule_action[178] = &Parser::Act178;
#else
    void Act178(void);
#endif

#ifndef HEADERS
    rule_action[179] = &Parser::Act179;
#else
    void Act179(void);
#endif

#ifndef HEADERS
    rule_action[180] = &Parser::Act180;
#else
    void Act180(void);
#endif

#ifndef HEADERS
    rule_action[181] = &Parser::Act181;
#else
    void Act181(void);
#endif

#ifndef HEADERS
    rule_action[182] = &Parser::Act182;
#else
    void Act182(void);
#endif

#ifndef HEADERS
    rule_action[183] = &Parser::Act183;
#else
    void Act183(void);
#endif

#ifndef HEADERS
    rule_action[184] = &Parser::MakeWhileStatement;
#endif

#ifndef HEADERS
    rule_action[185] = &Parser::MakeWhileStatement;
#endif

#ifndef HEADERS
    rule_action[186] = &Parser::Act186;
#else
    void Act186(void);
#endif

#ifndef HEADERS
    rule_action[187] = &Parser::MakeForStatement;
#endif

#ifndef HEADERS
    rule_action[188] = &Parser::MakeForStatement;
#endif

#ifndef HEADERS
    rule_action[189] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[190] = &Parser::Act190;
#else
    void Act190(void);
#endif

#ifndef HEADERS
    rule_action[191] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[192] = &Parser::Act192;
#else
    void Act192(void);
#endif

#ifndef HEADERS
    rule_action[193] = &Parser::Act193;
#else
    void Act193(void);
#endif

#ifndef HEADERS
    rule_action[194] = &Parser::Act194;
#else
    void Act194(void);
#endif

#ifndef HEADERS
    rule_action[195] = &Parser::Act195;
#else
    void Act195(void);
#endif

#ifndef HEADERS
    rule_action[196] = &Parser::Act196;
#else
    void Act196(void);
#endif

#ifndef HEADERS
    rule_action[197] = &Parser::Act197;
#else
    void Act197(void);
#endif

#ifndef HEADERS
    rule_action[198] = &Parser::Act198;
#else
    void Act198(void);
#endif

#ifndef HEADERS
    rule_action[199] = &Parser::Act199;
#else
    void Act199(void);
#endif

#ifndef HEADERS
    rule_action[200] = &Parser::Act200;
#else
    void Act200(void);
#endif

#ifndef HEADERS
    rule_action[201] = &Parser::Act201;
#else
    void Act201(void);
#endif

#ifndef HEADERS
    rule_action[202] = &Parser::Act202;
#else
    void Act202(void);
#endif

#ifndef HEADERS
    rule_action[203] = &Parser::Act203;
#else
    void Act203(void);
#endif

#ifndef HEADERS
    rule_action[204] = &Parser::Act204;
#else
    void Act204(void);
#endif

#ifndef HEADERS
    rule_action[205] = &Parser::Act205;
#else
    void Act205(void);
#endif

#ifndef HEADERS
    rule_action[206] = &Parser::Act206;
#else
    void Act206(void);
#endif

#ifndef HEADERS
    rule_action[207] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[208] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[209] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[210] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[211] = &Parser::Act211;
#else
    void Act211(void);
#endif

#ifndef HEADERS
    rule_action[212] = &Parser::Act212;
#else
    void Act212(void);
#endif

#ifndef HEADERS
    rule_action[213] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[214] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[215] = &Parser::Act215;
#else
    void Act215(void);
#endif

#ifndef HEADERS
    rule_action[216] = &Parser::Act216;
#else
    void Act216(void);
#endif

#ifndef HEADERS
    rule_action[217] = &Parser::Act217;
#else
    void Act217(void);
#endif

#ifndef HEADERS
    rule_action[218] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[219] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[220] = &Parser::Act220;
#else
    void Act220(void);
#endif

#ifndef HEADERS
    rule_action[221] = &Parser::MakeQualifiedNew;
#endif

#ifndef HEADERS
    rule_action[222] = &Parser::MakeQualifiedNew;
#endif

#ifndef HEADERS
    rule_action[223] = &Parser::Act223;
#else
    void Act223(void);
#endif

#ifndef HEADERS
    rule_action[224] = &Parser::Act224;
#else
    void Act224(void);
#endif

#ifndef HEADERS
    rule_action[225] = &Parser::MakeArrayCreationExpression;
#endif

#ifndef HEADERS
    rule_action[226] = &Parser::MakeArrayCreationExpression;
#endif

#ifndef HEADERS
    rule_action[227] = &Parser::Act227;
#else
    void Act227(void);
#endif

#ifndef HEADERS
    rule_action[228] = &Parser::Act228;
#else
    void Act228(void);
#endif

#ifndef HEADERS
    rule_action[229] = &Parser::Act229;
#else
    void Act229(void);
#endif

#ifndef HEADERS
    rule_action[230] = &Parser::Act230;
#else
    void Act230(void);
#endif

#ifndef HEADERS
    rule_action[231] = &Parser::Act231;
#else
    void Act231(void);
#endif

#ifndef HEADERS
    rule_action[232] = &Parser::Act232;
#else
    void Act232(void);
#endif

#ifndef HEADERS
    rule_action[233] = &Parser::MakeFieldAccess;
#endif

#ifndef HEADERS
    rule_action[234] = &Parser::MakeSuperFieldAccess;
#endif

#ifndef HEADERS
    rule_action[235] = &Parser::MakeSuperDoubleFieldAccess;
#endif

#ifndef HEADERS
    rule_action[236] = &Parser::Act236;
#else
    void Act236(void);
#endif

#ifndef HEADERS
    rule_action[237] = &Parser::Act237;
#else
    void Act237(void);
#endif

#ifndef HEADERS
    rule_action[238] = &Parser::Act238;
#else
    void Act238(void);
#endif

#ifndef HEADERS
    rule_action[239] = &Parser::Act239;
#else
    void Act239(void);
#endif

#ifndef HEADERS
    rule_action[240] = &Parser::MakeArrayAccess;
#endif

#ifndef HEADERS
    rule_action[241] = &Parser::MakeArrayAccess;
#endif

#ifndef HEADERS
    rule_action[242] = &Parser::MakeArrayAccess;
#endif

#ifndef HEADERS
    rule_action[243] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[244] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[245] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[246] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[247] = &Parser::Act247;
#else
    void Act247(void);
#endif

#ifndef HEADERS
    rule_action[248] = &Parser::Act248;
#else
    void Act248(void);
#endif

#ifndef HEADERS
    rule_action[249] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[250] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[251] = &Parser::Act251;
#else
    void Act251(void);
#endif

#ifndef HEADERS
    rule_action[252] = &Parser::Act252;
#else
    void Act252(void);
#endif

#ifndef HEADERS
    rule_action[253] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[254] = &Parser::Act254;
#else
    void Act254(void);
#endif

#ifndef HEADERS
    rule_action[255] = &Parser::Act255;
#else
    void Act255(void);
#endif

#ifndef HEADERS
    rule_action[256] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[257] = &Parser::Act257;
#else
    void Act257(void);
#endif

#ifndef HEADERS
    rule_action[258] = &Parser::Act258;
#else
    void Act258(void);
#endif

#ifndef HEADERS
    rule_action[259] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[260] = &Parser::MakeCastExpression;
#endif

#ifndef HEADERS
    rule_action[261] = &Parser::Act261;
#else
    void Act261(void);
#endif

#ifndef HEADERS
    rule_action[262] = &Parser::MakeCastExpression;
#endif

#ifndef HEADERS
    rule_action[263] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[264] = &Parser::Act264;
#else
    void Act264(void);
#endif

#ifndef HEADERS
    rule_action[265] = &Parser::Act265;
#else
    void Act265(void);
#endif

#ifndef HEADERS
    rule_action[266] = &Parser::Act266;
#else
    void Act266(void);
#endif

#ifndef HEADERS
    rule_action[267] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[268] = &Parser::Act268;
#else
    void Act268(void);
#endif

#ifndef HEADERS
    rule_action[269] = &Parser::Act269;
#else
    void Act269(void);
#endif

#ifndef HEADERS
    rule_action[270] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[271] = &Parser::Act271;
#else
    void Act271(void);
#endif

#ifndef HEADERS
    rule_action[272] = &Parser::Act272;
#else
    void Act272(void);
#endif

#ifndef HEADERS
    rule_action[273] = &Parser::Act273;
#else
    void Act273(void);
#endif

#ifndef HEADERS
    rule_action[274] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[275] = &Parser::Act275;
#else
    void Act275(void);
#endif

#ifndef HEADERS
    rule_action[276] = &Parser::Act276;
#else
    void Act276(void);
#endif

#ifndef HEADERS
    rule_action[277] = &Parser::Act277;
#else
    void Act277(void);
#endif

#ifndef HEADERS
    rule_action[278] = &Parser::Act278;
#else
    void Act278(void);
#endif

#ifndef HEADERS
    rule_action[279] = &Parser::Act279;
#else
    void Act279(void);
#endif

#ifndef HEADERS
    rule_action[280] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[281] = &Parser::Act281;
#else
    void Act281(void);
#endif

#ifndef HEADERS
    rule_action[282] = &Parser::Act282;
#else
    void Act282(void);
#endif

#ifndef HEADERS
    rule_action[283] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[284] = &Parser::Act284;
#else
    void Act284(void);
#endif

#ifndef HEADERS
    rule_action[285] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[286] = &Parser::Act286;
#else
    void Act286(void);
#endif

#ifndef HEADERS
    rule_action[287] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[288] = &Parser::Act288;
#else
    void Act288(void);
#endif

#ifndef HEADERS
    rule_action[289] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[290] = &Parser::Act290;
#else
    void Act290(void);
#endif

#ifndef HEADERS
    rule_action[291] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[292] = &Parser::Act292;
#else
    void Act292(void);
#endif

#ifndef HEADERS
    rule_action[293] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[294] = &Parser::Act294;
#else
    void Act294(void);
#endif

#ifndef HEADERS
    rule_action[295] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[296] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[297] = &Parser::Act297;
#else
    void Act297(void);
#endif

#ifndef HEADERS
    rule_action[298] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[299] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[300] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[301] = &Parser::Act301;
#else
    void Act301(void);
#endif

#ifndef HEADERS
    rule_action[302] = &Parser::Act302;
#else
    void Act302(void);
#endif

#ifndef HEADERS
    rule_action[303] = &Parser::Act303;
#else
    void Act303(void);
#endif

#ifndef HEADERS
    rule_action[304] = &Parser::Act304;
#else
    void Act304(void);
#endif

#ifndef HEADERS
    rule_action[305] = &Parser::Act305;
#else
    void Act305(void);
#endif

#ifndef HEADERS
    rule_action[306] = &Parser::Act306;
#else
    void Act306(void);
#endif

#ifndef HEADERS
    rule_action[307] = &Parser::Act307;
#else
    void Act307(void);
#endif

#ifndef HEADERS
    rule_action[308] = &Parser::Act308;
#else
    void Act308(void);
#endif

#ifndef HEADERS
    rule_action[309] = &Parser::Act309;
#else
    void Act309(void);
#endif

#ifndef HEADERS
    rule_action[310] = &Parser::Act310;
#else
    void Act310(void);
#endif

#ifndef HEADERS
    rule_action[311] = &Parser::Act311;
#else
    void Act311(void);
#endif

#ifndef HEADERS
    rule_action[312] = &Parser::Act312;
#else
    void Act312(void);
#endif

#ifndef HEADERS
    rule_action[313] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[314] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[315] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[316] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[317] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[318] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[319] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[320] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[321] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[322] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[323] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[324] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[325] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[326] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[327] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[328] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[329] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[330] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[331] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[332] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[333] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[334] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[335] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[336] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[337] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[338] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[339] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[340] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[341] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[342] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[343] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[344] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[345] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[346] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[347] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[348] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[349] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[350] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[351] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[352] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[353] = &Parser::NullAction;
#endif

#ifndef HEADERS
    rule_action[354] = &Parser::NoAction;
#endif

#ifndef HEADERS
    rule_action[355] = &Parser::Act355;
#else
    void Act355(void);
#endif

#ifndef HEADERS
    rule_action[356] = &Parser::Act356;
#else
    void Act356(void);
#endif

#ifndef HEADERS
    return;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // ! HEADERS
