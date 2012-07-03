// $Id: getclass.h,v 1.4 1999/08/26 15:34:08 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef getclass_INCLUDED
#define getclass_INCLUDED

#include "config.h"
#include "semantic.h"
#include "long.h"
#include "double.h"

class Cp_Info
{
public:
    enum
    {
        CONSTANT_Class              = 7,
        CONSTANT_Fieldref           = 9,
        CONSTANT_Methodref          = 10,
        CONSTANT_InterfaceMethodref = 11,
        CONSTANT_String             = 8,
        CONSTANT_Integer            = 3,
        CONSTANT_Float              = 4,
        CONSTANT_Long               = 5,
        CONSTANT_Double             = 6,
        CONSTANT_NameAndType        = 12,
        CONSTANT_Utf8               = 1
    };

    static u1 Tag(char *buffer) { return (u1) *buffer; }
};

class Constant_Utf8_info : public Cp_Info
{
public:
    static u2 Length(char *buffer) { return Semantic::GetU2(buffer + 1); } // skip tag
    static char *Bytes(char *buffer) { return buffer + 3; } // skip tag and length
};


class Constant_Class_info : public Cp_Info
{
public:
    static u2 NameIndex(char *buffer) { return Semantic::GetU2(buffer + 1); }
};


class Constant_Fieldref_info : public Cp_Info
{
public:
    static u2 ClassIndex(char *buffer) { return Semantic::GetU2(buffer + 1); }
    static u2 NameAndTypeIndex(char *buffer) { return Semantic::GetU2(buffer + 3); }
};


class Constant_Methodref_info : public Cp_Info
{
public:
    static u2 ClassIndex(char *buffer) { return Semantic::GetU2(buffer + 1); }
    static u2 NameAndTypeIndex(char *buffer) { return Semantic::GetU2(buffer + 3); }
};


class Constant_InterfaceMethodref_info : public Cp_Info
{
public:
    static u2 ClassIndex(char *buffer) { return Semantic::GetU2(buffer + 1); }
    static u2 NameAndTypeIndex(char *buffer) { return Semantic::GetU2(buffer + 3); }
};


class Constant_NameAndType_info : public Cp_Info
{
public:
    static u2 NameIndex(char *buffer) { return Semantic::GetU2(buffer + 1); }
    static u2 DescriptorIndex(char *buffer) { return Semantic::GetU2(buffer + 3); }
};


class Constant_String_info : public Cp_Info
{
public:
    static u2 StringIndex(char *buffer) { return Semantic::GetU2(buffer + 1); }
};


class Constant_Integer_info : public Cp_Info
{
public:
    static u4 Bytes(char *buffer) { return Semantic::GetU4(buffer + 1); }

    static int Value(char *buffer)
    {
        union field
        {
            u4 u;
            int i;
        } value;
        value.u = Bytes(buffer);
        return value.i;
    }
};


class Constant_Float_info : public Cp_Info
{
public:
    static u4 Bytes(char *buffer) { return Semantic::GetU4(buffer + 1); }

    static IEEEfloat Value(char *buffer)
    {
        return IEEEfloat(Semantic::GetU4(buffer+1));
    }
};


class Constant_Long_info : public Cp_Info
{
public:
    static u4 HighBytes(char *buffer) { return Semantic::GetU4(buffer + 1); }
    static u4 LowBytes(char *buffer) { return Semantic::GetU4(buffer + 5); }

    static LongInt Value(char *buffer)
    {
        return LongInt(HighBytes(buffer), LowBytes(buffer));
    }
};


class Constant_Double_info : public Cp_Info
{
public:
    static u4 HighBytes(char *buffer) { return Semantic::GetU4(buffer + 1); }
    static u4 LowBytes(char *buffer) { return Semantic::GetU4(buffer + 5); }

    static IEEEdouble Value(char *buffer)
    {
        return IEEEdouble(HighBytes(buffer), LowBytes(buffer));
    }
};
#endif
