// $Id: op.h,v 1.12 2001/09/14 05:31:34 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef op_INCLUDED
#define op_INCLUDED

#include "platform.h"
#include "tuple.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class cp_info;

// code dealing with describing and listing byte code
class Operators
{
public:
    enum operators
    {
        OP_NOP = 0x00,
        OP_ACONST_NULL = 0x01,
        OP_ICONST_M1 = 0x02,
        OP_ICONST_0 = 0x03,
        OP_ICONST_1 = 0x04,
        OP_ICONST_2 = 0x05,
        OP_ICONST_3 = 0x06,
        OP_ICONST_4 = 0x07,
        OP_ICONST_5 = 0x08,
        OP_LCONST_0 = 0x09,
        OP_LCONST_1 = 0x0a,
        OP_FCONST_0 = 0x0b,
        OP_FCONST_1 = 0x0c,
        OP_FCONST_2 = 0x0d,
        OP_DCONST_0 = 0x0e,
        OP_DCONST_1 = 0x0f,
        OP_BIPUSH = 0x10,
        OP_SIPUSH = 0x11,
        OP_LDC = 0x12,
        OP_LDC_W = 0x13,
        OP_LDC2_W = 0x14,
        OP_ILOAD = 0x15,
        OP_LLOAD = 0x16,
        OP_FLOAD = 0x17,
        OP_DLOAD = 0x18,
        OP_ALOAD = 0x19,
        OP_ILOAD_0 = 0x1a,
        OP_ILOAD_1 = 0x1b,
        OP_ILOAD_2 = 0x1c,
        OP_ILOAD_3 = 0x1d,
        OP_LLOAD_0 = 0x1e,
        OP_LLOAD_1 = 0x1f,
        OP_LLOAD_2 = 0x20,
        OP_LLOAD_3 = 0x21,
        OP_FLOAD_0 = 0x22,
        OP_FLOAD_1 = 0x23,
        OP_FLOAD_2 = 0x24,
        OP_FLOAD_3 = 0x25,
        OP_DLOAD_0 = 0x26,
        OP_DLOAD_1 = 0x27,
        OP_DLOAD_2 = 0x28,
        OP_DLOAD_3 = 0x29,
        OP_ALOAD_0 = 0x2a,
        OP_ALOAD_1 = 0x2b,
        OP_ALOAD_2 = 0x2c,
        OP_ALOAD_3 = 0x2d,
        OP_IALOAD = 0x2e,
        OP_LALOAD = 0x2f,
        OP_FALOAD = 0x30,
        OP_DALOAD = 0x31,
        OP_AALOAD = 0x32,
        OP_BALOAD = 0x33,
        OP_CALOAD = 0x34,
        OP_SALOAD = 0x35,
        OP_ISTORE = 0x36,
        OP_LSTORE = 0x37,
        OP_FSTORE = 0x38,
        OP_DSTORE = 0x39,
        OP_ASTORE = 0x3a,
        OP_ISTORE_0 = 0x3b,
        OP_ISTORE_1 = 0x3c,
        OP_ISTORE_2 = 0x3d,
        OP_ISTORE_3 = 0x3e,
        OP_LSTORE_0 = 0x3f,
        OP_LSTORE_1 = 0x40,
        OP_LSTORE_2 = 0x41,
        OP_LSTORE_3 = 0x42,
        OP_FSTORE_0 = 0x43,
        OP_FSTORE_1 = 0x44,
        OP_FSTORE_2 = 0x45,
        OP_FSTORE_3 = 0x46,
        OP_DSTORE_0 = 0x47,
        OP_DSTORE_1 = 0x48,
        OP_DSTORE_2 = 0x49,
        OP_DSTORE_3 = 0x4a,
        OP_ASTORE_0 = 0x4b,
        OP_ASTORE_1 = 0x4c,
        OP_ASTORE_2 = 0x4d,
        OP_ASTORE_3 = 0x4e,
        OP_IASTORE = 0x4f,
        OP_LASTORE = 0x50,
        OP_FASTORE = 0x51,
        OP_DASTORE = 0x52,
        OP_AASTORE = 0x53,
        OP_BASTORE = 0x54,
        OP_CASTORE = 0x55,
        OP_SASTORE = 0x56,
        OP_POP = 0x57,
        OP_POP2 = 0x58,
        OP_DUP = 0x59,
        OP_DUP_X1 = 0x5a,
        OP_DUP_X2 = 0x5b,
        OP_DUP2 = 0x5c,
        OP_DUP2_X1 = 0x5d,
        OP_DUP2_X2 = 0x5e,
        OP_SWAP = 0x5f,
        OP_IADD = 0x60,
        OP_LADD = 0x61,
        OP_FADD = 0x62,
        OP_DADD = 0x63,
        OP_ISUB = 0x64,
        OP_LSUB = 0x65,
        OP_FSUB = 0x66,
        OP_DSUB = 0x67,
        OP_IMUL = 0x68,
        OP_LMUL = 0x69,
        OP_FMUL = 0x6a,
        OP_DMUL = 0x6b,
        OP_IDIV = 0x6c,
        OP_LDIV = 0x6d,
        OP_FDIV = 0x6e,
        OP_DDIV = 0x6f,
        OP_IREM = 0x70,
        OP_LREM = 0x71,
        OP_FREM = 0x72,
        OP_DREM = 0x73,
        OP_INEG = 0x74,
        OP_LNEG = 0x75,
        OP_FNEG = 0x76,
        OP_DNEG = 0x77,
        OP_ISHL = 0x78,
        OP_LSHL = 0x79,
        OP_ISHR = 0x7a,
        OP_LSHR = 0x7b,
        OP_IUSHR = 0x7c,
        OP_LUSHR = 0x7d,
        OP_IAND = 0x7e,
        OP_LAND = 0x7f,
        OP_IOR = 0x80,
        OP_LOR = 0x81,
        OP_IXOR = 0x82,
        OP_LXOR = 0x83,
        OP_IINC = 0x84,
        OP_I2L = 0x85,
        OP_I2F = 0x86,
        OP_I2D = 0x87,
        OP_L2I = 0x88,
        OP_L2F = 0x89,
        OP_L2D = 0x8a,
        OP_F2I = 0x8b,
        OP_F2L = 0x8c,
        OP_F2D = 0x8d,
        OP_D2I = 0x8e,
        OP_D2L = 0x8f,
        OP_D2F = 0x90,
        OP_I2B = 0x91,
        OP_I2C = 0x92,
        OP_I2S = 0x93,
        OP_LCMP = 0x94,
        OP_FCMPL = 0x95,
        OP_FCMPG = 0x96,
        OP_DCMPL = 0x97,
        OP_DCMPG = 0x98,
        OP_IFEQ = 0x99,
        OP_IFNE = 0x9a,
        OP_IFLT = 0x9b,
        OP_IFGE = 0x9c,
        OP_IFGT = 0x9d,
        OP_IFLE = 0x9e,
        OP_IF_ICMPEQ = 0x9f,
        OP_IF_ICMPNE = 0xa0,
        OP_IF_ICMPLT = 0xa1,
        OP_IF_ICMPGE = 0xa2,
        OP_IF_ICMPGT = 0xa3,
        OP_IF_ICMPLE = 0xa4,
        OP_IF_ACMPEQ = 0xa5,
        OP_IF_ACMPNE = 0xa6,
        OP_GOTO = 0xa7,
        OP_JSR = 0xa8,
        OP_RET = 0xa9,
        OP_TABLESWITCH = 0xaa,
        OP_LOOKUPSWITCH = 0xab,
        OP_IRETURN = 0xac,
        OP_LRETURN = 0xad,
        OP_FRETURN = 0xae,
        OP_DRETURN = 0xaf,
        OP_ARETURN = 0xb0,
        OP_RETURN = 0xb1,
        OP_GETSTATIC = 0xb2,
        OP_PUTSTATIC = 0xb3,
        OP_GETFIELD = 0xb4,
        OP_PUTFIELD = 0xb5,
        OP_INVOKEVIRTUAL = 0xb6,
        OP_INVOKENONVIRTUAL = 0xb7,
        OP_INVOKESTATIC = 0xb8,
        OP_INVOKEINTERFACE = 0xb9,
        OP_XXXUNUSEDXXX = 0xba,
        OP_NEW = 0xbb,
        OP_NEWARRAY = 0xbc,
        OP_ANEWARRAY = 0xbd,
        OP_ARRAYLENGTH = 0xbe,
        OP_ATHROW = 0xbf,
        OP_CHECKCAST = 0xc0,
        OP_INSTANCEOF = 0xc1,
        OP_MONITORENTER = 0xc2,
        OP_MONITOREXIT = 0xc3,
        OP_WIDE = 0xc4,
        OP_MULTIANEWARRAY = 0xc5,
        OP_IFNULL = 0xc6,
        OP_IFNONNULL = 0xc7,
        OP_GOTO_W = 0xc8,
        OP_JSR_W = 0xc9,
        OP_SOFTWARE = 0xfe,
        OP_HARDWARE = 0xff
    };

    static void opdmp(Tuple<cp_info *> &, Tuple<u1> &);

protected:

    static int stack_effect[];

private:

    enum
    {
        INFO_NONE = 0,
        INFO_LOCAL = 1,
        INFO_CONST = 2,
        INFO_DONE  = 3
    };

    static void opdesc (int opc, const char **name, const char **desc);

    inline static signed char get_i1(Tuple<u1> &code, int pc)
    {
        return code[pc];
    }

    inline static short get_i2(Tuple<u1> &code, int pc)
    {
        return  code[pc] << 8 | code[pc + 1];
    }

    inline static int get_i4(Tuple<u1> &code, int pc)
    {
        return  code[pc] << 24 | code[pc + 1] << 16 | code[pc + 2] << 8 | code[pc + 3];
    }

    inline static unsigned get_u1(Tuple<u1> &code, int pc)
    {
        return code[pc];
    }

    inline static unsigned get_u2(Tuple<u1> &code, int pc)
    {
        return (unsigned) (code[pc] << 8 | code[pc + 1]);
    }

    inline static unsigned get_u4(Tuple<u1> &code, int pc)
    {
        return (unsigned) (code[pc] << 24 | code[pc + 1] << 16 | code[pc + 2] << 8 | code[pc + 3]);
    }

    static void opline(Tuple<cp_info *> &, const char *, int, int,
                       const char *, char *, const char *, int, int);
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // op_INCLUDED
