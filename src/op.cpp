// $Id: op.cpp,v 1.20 2002/02/01 06:46:19 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "op.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


void Operators::OpDesc(Opcode opc, const char **name, const char **desc)
{
    struct op_entry
    {
        const char *op_name;
        const char *op_desc;
    };

    static struct op_entry table[] =
    {
        {"nop",  "do nothing"},
        {"aconst_null",  "push null"},
        {"iconst_m1",  "push int constant -1"},
        {"iconst_0",  "push int constant 0"},
        {"iconst_1",  "push int constant 1"},
        {"iconst_2",  "push int constant 2"},
        {"iconst_3",  "push int constant 3"},
        {"iconst_4",  "push int constant 4"},
        {"iconst_5",  "push int constant 5"},
        {"lconst_0",  "push long constant 0"},
        {"lconst_1",  "push long constant 1"},
        {"fconst_0",  "push float 0.0"},
        {"fconst_1",  "push float 1.0"},
        {"fconst_2",  "push float 2.0"},
        {"dconst_0",  "push double 0.0 onto stack"},
        {"dconst_1",  "push double 1.0 onto stack"},
        {"bipush",  "push byte"},
        {"sipush",  "push short"},
        {"ldc",  "push item from constant pool"},
        {"ldc_w",  "push item from constant pool (wide index)"},
        {"ldc2_w",  "push long or double from constant pool (wide index)"},
        {"iload",  "load int from local variable"},
        {"lload",  "load long from local variable"},
        {"fload",  "load float from local variable"},
        {"dload",  "load double from local variable"},
        {"aload",  "load reference from local variable"},
        {"iload_0",  "load int from local variable 0"},
        {"iload_1",  "load int from local variable 1"},
        {"iload_2",  "load int from local variable 2"},
        {"iload_3",  "load int from local variable 3"},
        {"lload_0",  "load long from local variable 0"},
        {"lload_1",  "load long from local variable 1"},
        {"lload_2",  "load long from local variable 2"},
        {"lload_3",  "load long from local variable 3"},
        {"fload_0",  "load float from local variable 0"},
        {"fload_1",  "load float from local variable 1"},
        {"fload_2",  "load float from local variable 2"},
        {"fload_3",  "load float from local variable 3"},
        {"dload_0",  "load double from local variable 0"},
        {"dload_1",  "load double from local variable 1"},
        {"dload_2",  "load double from local variable 2"},
        {"dload_3",  "load double from local variable 3"},
        {"aload_0",  "load reference from local variable 0"},
        {"aload_1",  "load reference from local variable 1"},
        {"aload_2",  "load reference from local variable 2"},
        {"aload_3",  "load reference from local variable 3"},
        {"iaload",  "load int from array"},
        {"laload",  "load long from array"},
        {"faload",  "load float from array"},
        {"daload",  "load double from array"},
        {"aaload",  "load reference from array"},
        {"baload",  "load byte from array"},
        {"caload",  "load char from array"},
        {"saload",  "load short from array"},
        {"istore",  "store int into local variable"},
        {"lstore",  "store long into local variable"},
        {"fstore",  "store float into local variable"},
        {"dstore",  "store double into local variable"},
        {"astore",  "store reference into local variable"},
        {"istore_0",  "store int into local variable 0"},
        {"istore_1",  "store int into local variable 1"},
        {"istore_2",  "store int into local variable 2"},
        {"istore_3",  "store int into local variable 3"},
        {"lstore_0",  "store long into local variable 0"},
        {"lstore_1",  "store long into local variable 1"},
        {"lstore_2",  "store long into local variable 2"},
        {"lstore_3",  "store long into local variable 3"},
        {"fstore_0",  "store float into local variable 0"},
        {"fstore_1",  "store float into local variable 1"},
        {"fstore_2",  "store float into local variable 2"},
        {"fstore_3",  "store float into local variable 3"},
        {"dstore_0",  "store double into local variable 0"},
        {"dstore_1",  "store double into local variable 1"},
        {"dstore_2",  "store double into local variable 2"},
        {"dstore_3",  "store double into local variable 3"},
        {"astore_0",  "store reference into local variable 0"},
        {"astore_1",  "store reference into local variable 1"},
        {"astore_2",  "store reference into local variable 2"},
        {"astore_3",  "store reference into local variable 3"},
        {"iastore",  "store into int array"},
        {"lastore",  "store into long array"},
        {"fastore",  "store into float array"},
        {"dastore",  "store into double array"},
        {"aastore",  "store into reference array"},
        {"bastore",  "store into byte array"},
        {"castore",  "store into char array"},
        {"sastore",  "store into short array"},
        {"pop",  "pop top operand stack word"},
        {"pop2",  "pop two two operand stack words"},
        {"dup",  "duplicate top operand stack word"},
        {"dup_x1",  "duplicate top operand stack word and put two down"},
        {"dup_x2",  "duplicate top operand stack word and put three down"},
        {"dup2",  "duplicate top two operand stack words"},
        {"dup2_x1",  "duplicate top two operand stack words and put three down"},
        {"dup2_x2",  "duplicate top two operand stack words and put four down"},
        {"swap",  "swap top two operand stack words"},
        {"iadd",  "add int"},
        {"ladd",  "add long"},
        {"fadd",  "add float"},
        {"dadd",  "add double"},
        {"isub",  "subtract int"},
        {"lsub",  "subtract long"},
        {"fsub",  "substract float"},
        {"dsub",  "subtract double"},
        {"imul",  "multiply int"},
        {"lmul",  "multiply long"},
        {"fmul",  "multiply float"},
        {"dmul",  "multiply double"},
        {"idiv",  "divide int"},
        {"ldiv",  "divide long"},
        {"fdiv",  "divide float"},
        {"ddiv",  "divide double"},
        {"irem",  "remainder int"},
        {"lrem",  "remainder long"},
        {"frem",  "remainder float"},
        {"drem",  "remainder double"},
        {"ineg",  "negate int"},
        {"lneg",  "negate long"},
        {"fneg",  "negate float"},
        {"dneg",  "negate double"},
        {"ishl",  "shift left int"},
        {"lshl",  "shift left long"},
        {"ishr",  "shift right int"},
        {"lshr",  "shift right long"},
        {"iushr",  "logical shift right int"},
        {"lushr",  "logical shift right long"},
        {"iand",  "boolean and int"},
        {"land",  "boolean and long"},
        {"ior",  "boolean or int"},
        {"lor",  "boolean or long"},
        {"ixor",  "boolean xor int"},
        {"lxor",  "boolean xor long"},
        {"iinc",  "increment local variable by constant"},
        {"i2l",  "convert int to long"},
        {"i2f",  "convert int to float"},
        {"i2d",  "convert int to double"},
        {"l2i",  "convert long to int"},
        {"l2f",  "convert long to float"},
        {"l2d",  "convert long to double"},
        {"f2i",  "convert float to int"},
        {"f2l",  "convert float to long"},
        {"f2d",  "convert float to double"},
        {"d2i",  "convert double to int"},
        {"d2l",  "convert double to long"},
        {"d2f",  "convert double to float"},
        {"i2b",  "convert int to byte"},
        {"i2c",  "convert int to char"},
        {"i2s",  "convert int to short"},
        {"lcmp",  "compare long"},
        {"fcmpl",  "compare float less"},
        {"fcmpg",  "compare float greater"},
        {"dcmpl",  "compare double less"},
        {"dcmpg",  "compare double greater"},
        {"ifeq",  "branch if int eq zero"},
        {"ifne",  "branch if int ne zero"},
        {"iflt",  "branch if int lt zero"},
        {"ifge",  "branch if int ge zero"},
        {"ifgt",  "branch if int gt zero"},
        {"ifle",  "branch if int le zero"},
        {"if_icmpeq",  "branch if int comparison eq"},
        {"if_icmpne",  "branch if int comparison ne"},
        {"if_icmplt",  "branch if int comparison lt"},
        {"if_icmpge",  "branch if int comparison ge"},
        {"if_icmpgt",  "branch if int comparison gt"},
        {"if_icmple",  "branch if int comparison le"},
        {"if_acmpeq",  "branch if reference comparison eq"},
        {"if_acmpne",  "branch if reference comparison ne"},
        {"goto",  "branch always"},
        {"jsr",  "jump subroutine"},
        {"ret",  "return from subroutine"},
        {"tableswitch",  "access jump table by index and jump"},
        {"lookupswitch",  "access jump table by key match and jump"},
        {"ireturn",  "return int from method"},
        {"lreturn",  "return long from method"},
        {"freturn",  "return float from method"},
        {"dreturn",  "return double from method"},
        {"areturn",  "return reference from method"},
        {"return",  "return void from method"},
        {"getstatic",  "get static field from class"},
        {"putstatic",  "set static field in class"},
        {"getfield",  "fetch field from object"},
        {"putfield",  "set field in object"},
        {"invokevirtual",  "invoke polymorphic instance method; dispatch based on runtime type"},
        {"invokespecial",  "invoke constructor, private, or superclass instance method; dispatch based on compile-time type"},
        {"invokestatic",  "invoke a class (static) method"},
        {"invokeinterface",  "invoke interface method"},
        {"xxxunusedxxx",  "xxxunusedxxx"},
        {"new",  "create new object"},
        {"newarray",  "create new array"},
        {"anewarray",  "create new array of references"},
        {"arraylength",  "get length of array"},
        {"athrow",  "throw exception or error"},
        {"checkcast",  "check whether object is of given type"},
        {"instanceof",  "determine if object is of given type"},
        {"monitorenter",  "enter monitor for object"},
        {"monitorexit",  "exit monitor for object"},
        {"wide",  "extend local variable index by additional bytes"},
        {"multianewarray",  "create new multidimensional array"},
        {"ifnull",  "branch if reference is null"},
        {"ifnonnuull",  "branch if reference not null"},
        {"goto_w",  "branch always (wide index) ?"},
        {"jsr_w",  "jump subroutine (wide index)"}
    };

    if (opc == OP_SOFTWARE)
    {
        if (name)
            *name = "software";
        if (desc)
            *desc = "software";
    }
    else if (opc == OP_HARDWARE)
    {
        if (name)
            *name = "hardware";
        if (desc)
            *desc = "hardware";
    }
    else if (opc >= 0 && opc <= 0xc9)
    {
        if (name)
            *name = table[opc].op_name;
        if (desc)
            *desc = table[opc].op_desc;
    }
    else
    {
        if (name)
            *name = "illegal";
        if (desc)
            *desc = "illegal";
    }
}

void Operators::OpLine(Tuple<cp_info *> &constant_pool, const char *hdr,
                       int pc, Opcode opc, const char *name, char *args,
                       const char *desc, int info_kind, int info_index)
{
    // generate line of opcode dump, info is extra info

    Coutput << *hdr;
    Coutput.width(4);
    Coutput << pc;
    Coutput << "\t" << name;
    int len = strlen(name);
    if (strlen(args))
    {
        Coutput << " " << args;
        len += (strlen(args) + 1);
    }
    if (len < 8)
         Coutput << "\t\t\t";
    else if (len < 16)
         Coutput << "\t\t";
    else Coutput << "\t";

    switch (info_kind)
    {
        case INFO_CONST:
             Coutput << " ";
             if (info_index <= 0 || info_index > constant_pool.Length())
                Coutput << "OUT-OF_BOUNDS CONSTANT_POOL-INDEX " << info_index;
            else {
//              constant_pool[info_index] -> describe(constant_pool);
            }
            break;
        case INFO_LOCAL:
            Coutput << " local#" << info_index;
            break;
    }
//
// DS (17 jun) - skip desc for now: it's too long and should only
// be written at user request.
//  Coutput << " " << desc;
//
    Coutput << endl;
}

void Operators::OpDmp(Tuple<cp_info *> &constant_pool, Tuple<u1> &code)
{
    int pc = 0;
    while (pc < code.Length())
    {
        int info_kind = INFO_NONE; // assume no extra info
        int info_index = 0;
        int pc_start = pc;
        Opcode opc = (Opcode) GetU1(code, pc);
        const char *name, *desc; // set to name (mnemonic) and description of opcode.
        OpDesc((Opcode) code[pc_start], &name, &desc);
        pc++;

        char argdesc[100];
        argdesc[0] = U_NULL; // assume no argument description needed

        unsigned au1;
        int ai1,
            ai2,
            ai4;
        switch (opc)
        {
            case OP_BIPUSH:
                 ai1 = GetI1(code, pc); pc++;
                 sprintf(argdesc, "%d", ai1);
                 break;
            case OP_SIPUSH:
                 ai2 = GetI2(code, pc); pc +=2;
                 sprintf(argdesc, "%d", ai2);
                 break;
            case OP_LDC:
                 info_index = GetU1(code, pc); pc++;
                 sprintf(argdesc,"%d", info_index);
                 info_kind = INFO_CONST;
                 break;
            case OP_LDC_W:
            case OP_LDC2_W:
                 info_index = GetU2(code, pc);pc +=2;
                 sprintf(argdesc, "%u", info_index);
                 info_kind = INFO_CONST;
                 break;
            case OP_ILOAD:
            case OP_LLOAD:
            case OP_FLOAD:
            case OP_DLOAD:
            case OP_ALOAD:
            case OP_ISTORE:
            case OP_LSTORE:
            case OP_FSTORE:
            case OP_DSTORE:
            case OP_ASTORE:
                 info_index = GetU1(code, pc);pc++;
                 sprintf(argdesc, "%u", info_index);
                 info_kind = INFO_LOCAL;
                 break;
            case OP_ILOAD_0:
            case OP_LLOAD_0:
            case OP_FLOAD_0:
            case OP_DLOAD_0:
            case OP_ALOAD_0:
            case OP_ISTORE_0:
            case OP_LSTORE_0:
            case OP_FSTORE_0:
            case OP_DSTORE_0:
            case OP_ASTORE_0:
                 info_kind = INFO_LOCAL;
                 info_index = 0;
                 break;
            case OP_ILOAD_1:
            case OP_LLOAD_1:
            case OP_FLOAD_1:
            case OP_DLOAD_1:
            case OP_ALOAD_1:
            case OP_ISTORE_1:
            case OP_LSTORE_1:
            case OP_FSTORE_1:
            case OP_DSTORE_1:
            case OP_ASTORE_1:
                 info_kind = INFO_LOCAL;
                 info_index = 1;
                 break;
            case OP_ILOAD_2:
            case OP_LLOAD_2:
            case OP_FLOAD_2:
            case OP_DLOAD_2:
            case OP_ALOAD_2:
            case OP_ISTORE_2:
            case OP_LSTORE_2:
            case OP_DSTORE_2:
            case OP_FSTORE_2:
            case OP_ASTORE_2:
                 info_kind = INFO_LOCAL;
                 info_index = 2;
                 break;
            case OP_ILOAD_3:
            case OP_LLOAD_3:
            case OP_FLOAD_3:
            case OP_DLOAD_3:
            case OP_ALOAD_3:
            case OP_ISTORE_3:
            case OP_LSTORE_3:
            case OP_FSTORE_3:
            case OP_DSTORE_3:
            case OP_ASTORE_3:
                 info_kind= INFO_LOCAL;
                 info_index = 3;
                 break;
            case OP_IINC:
                 info_index = GetU1(code, pc); pc++;
                 au1 = GetU1(code, pc); pc++;
                 ai1 = GetI1(code, pc); pc++;
                 info_kind = INFO_LOCAL;
                 sprintf(argdesc, "%d %d", au1, ai1);
                 break;
            case OP_IFEQ:
            case OP_IFNE:
            case OP_IFLT:
            case OP_IFGE:
            case OP_IFGT:
            case OP_IFLE:
            case OP_IF_ICMPEQ:
            case OP_IF_ICMPNE:
            case OP_IF_ICMPLT:
            case OP_IF_ICMPGE:
            case OP_IF_ICMPGT:
            case OP_IF_ICMPLE:
            case OP_IF_ACMPEQ:
            case OP_IF_ACMPNE:
            case OP_GOTO:
            case OP_JSR:
            case OP_IFNULL:
            case OP_IFNONNULL:
                 ai2 = GetI2(code, pc);
                 sprintf(argdesc, "%d", (int) ( ai2+pc_start)); // compute branch target
                 pc +=2;
                 break;
            case OP_RET:
                 au1 = GetU1(code, pc);
                 pc++;
                 sprintf(argdesc, "%d", (int) au1);
                 break;
            case OP_TABLESWITCH:
                 {
                     int def,
                         low,
                         high,
                         len,
                         val,
                         pc_this;
                     Opcode op_this;
                     unsigned iu1;
                     op_this = OP_TABLESWITCH;
                     // account for padding
                     while (pc % 4)
                     {
                         iu1 = GetU1(code, pc);
                         pc++;
                     }
                     def = GetI4(code, pc); pc += 4;
                     low = GetI4(code, pc); pc +=4;
                     high = GetI4(code, pc); pc += 4;
                     sprintf(argdesc, "default:%d low:%d high:%d",
                             def + pc_start, low, high);
                     OpLine(constant_pool, " ", pc_start, opc, name,
                            argdesc, desc, info_kind, info_index);
                     len =  high - low + 1;
                     while (len)
                     {
                         pc_this = pc;
                         val = GetI4(code, pc);
                         sprintf(argdesc, "match:%d offset:%d",
                                 low++, val + pc_start);
                         OpLine(constant_pool,"*",pc_this, op_this, name,
                                argdesc, desc, INFO_NONE, 0);
                         pc += 4;
                         len--;
                     }
                     info_kind = INFO_DONE;
                 }
                 break;
            case OP_LOOKUPSWITCH:
                 {
                     int def,
                         npairs,
                         len,
                         match,
                         offset;
                     Opcode op_this;

                     // account for padding
                     unsigned iu1;
                     op_this = OP_LOOKUPSWITCH;
                     while (pc % 4)
                     {
                         iu1 = GetU1(code, pc);
                         pc++;
                     }
                     def = GetI4(code, pc); pc += 4;
                     npairs = GetI4(code, pc); pc +=4;
                     sprintf(argdesc, "default:%d npairs:%d",
                             def + pc_start, npairs);
                     OpLine(constant_pool, " ", pc_start, op_this, name,
                            argdesc, desc, info_kind, info_index);
                     len = npairs;
                     while (npairs)
                     {
                         int pcb = pc;
                         match = GetI4(code, pc); pc +=4 ;
                         offset = GetI4(code, pc); pc +=4;
                         sprintf(argdesc, "match:%d offset:%d ",
                                 match, offset + pc_start);
                         OpLine(constant_pool, "*", pcb, op_this, name,
                                argdesc, desc, INFO_NONE, 0);
                         npairs--;
                     }
                     info_kind = INFO_DONE;
                 }
                 break;
            case OP_GETSTATIC:
            case OP_PUTSTATIC:
            case OP_GETFIELD:
            case OP_PUTFIELD:
            case OP_INVOKEVIRTUAL:
            case OP_INVOKESPECIAL:
            case OP_INVOKESTATIC:
            case OP_NEW:
            case OP_ANEWARRAY:
            case OP_CHECKCAST:
            case OP_INSTANCEOF:
                 info_index = GetU2(code, pc); pc += 2;
                 sprintf(argdesc, "%d", info_index);
                 info_kind = INFO_CONST;
                 break;
            case OP_INVOKEINTERFACE:
                 {
                     int nargs;
                     info_index = GetU2(code, pc); pc += 2;
                     au1 = GetU1(code, pc); pc++;
                     nargs = au1;
                     au1 = GetU1(code, pc); pc++;

                     assert((! au1) && "...zero byte required in this position");

                     sprintf(argdesc, "%d %d", nargs,info_index);
                     info_kind=INFO_CONST;
                 }
                 break;
            case OP_NEWARRAY:
                 au1 = GetU1(code, pc); pc++;
                 switch (au1)
                 {
                     case 4: sprintf(argdesc, "%s", "BOOLEAN");break;
                     case 5: sprintf(argdesc, "%s", "CHAR");break;
                     case 6: sprintf(argdesc, "%s", "FLOAT");break;
                     case 7: sprintf(argdesc, "%s", "DOUBLE");break;
                     case 8: sprintf(argdesc, "%s", "BYTE");break;
                     case 9: sprintf(argdesc, "%s", "SHORT");break;
                     case 10: sprintf(argdesc, "%s", "INT");break;
                     case 11: sprintf(argdesc, "%s", "LONG");break;
                     default:
                         sprintf(argdesc, "%s", "<UNKNOWN>");break;
                 }
                 break;
            case OP_WIDE:
                 assert(false && "dmp for op_wide not supported yet");
                 break;
            case OP_MULTIANEWARRAY:
                 info_index = GetU2(code, pc); pc += 2;
                 au1 = GetU1(code, pc); pc++;
                 info_kind = INFO_CONST;
                 // au1 gives dimensions
                 sprintf(argdesc, "%u", au1);
                 break;
            case OP_GOTO_W:
            case OP_JSR_W:
                 ai4 = GetI4(code, pc); pc += 4;
                 // ai4 gives offset (wide) of branch target
                 sprintf(argdesc, "%d", pc_start + ai4);
                 break;
            default:
                 break;
        }

        // output first part of description
        if (info_kind != INFO_DONE)
            OpLine(constant_pool, " ", pc_start, opc, name,
                   argdesc, desc, info_kind, info_index);
    }
}


//
// stack_effect gives effect on stack of executing an opcode
//
int Operators::stack_effect[] =
{
    0,  // OP_NOP
    1,  // OP_ACONST_NULL
    1,  // OP_ICONST_M1
    1,  // OP_ICONST_0
    1,  // OP_ICONST_1
    1,  // OP_ICONST_2
    1,  // OP_ICONST_3
    1,  // OP_ICONST_4
    1,  // OP_ICONST_5
    2,  // OP_LCONST_0
    2,  // OP_LCONST_1
    1,  // OP_FCONST_0
    1,  // OP_FCONST_1
    1,  // OP_FCONST_2
    2,  // OP_DCONST_0
    2,  // OP_DCONST_1
    1,  // OP_BIPUSH
    1,  // OP_SIPUSH
    1,  // OP_LDC
    1,  // OP_LDC_W
    2,  // OP_LDC2_W
    1,  // OP_ILOAD
    2,  // OP_LLOAD
    1,  // OP_FLOAD
    2,  // OP_DLOAD
    1,  // OP_ALOAD
    1,  // OP_ILOAD_0
    1,  // OP_ILOAD_1
    1,  // OP_ILOAD_2
    1,  // OP_ILOAD_3
    2,  // OP_LLOAD_0
    2,  // OP_LLOAD_1
    2,  // OP_LLOAD_2
    2,  // OP_LLOAD_3
    1,  // OP_FLOAD_0
    1,  // OP_FLOAD_1
    1,  // OP_FLOAD_2
    1,  // OP_FLOAD_3
    2,  // OP_DLOAD_0
    2,  // OP_DLOAD_1
    2,  // OP_DLOAD_2
    2,  // OP_DLOAD_3
    1,  // OP_ALOAD_0
    1,  // OP_ALOAD_1
    1,  // OP_ALOAD_2
    1,  // OP_ALOAD_3
   -1,  // OP_IALOAD
    0,  // OP_LALOAD
   -1,  // OP_FALOAD
    0,  // OP_DALOAD
   -1,  // OP_AALOAD
   -1,  // OP_BALOAD
   -1,  // OP_CALOAD
   -1,  // OP_SALOAD
   -1,  // OP_ISTORE
   -2,  // OP_LSTORE
   -1,  // OP_FSTORE
   -2,  // OP_DSTORE
   -1,  // OP_ASTORE
   -1,  // OP_ISTORE_0
   -1,  // OP_ISTORE_1
   -1,  // OP_ISTORE_2
   -1,  // OP_ISTORE_3
   -2,  // OP_LSTORE_0
   -2,  // OP_LSTORE_1
   -2,  // OP_LSTORE_2
   -2,  // OP_LSTORE_3
   -1,  // OP_FSTORE_0
   -1,  // OP_FSTORE_1
   -1,  // OP_FSTORE_2
   -1,  // OP_FSTORE_3
   -2,  // OP_DSTORE_0
   -2,  // OP_DSTORE_1
   -2,  // OP_DSTORE_2
   -2,  // OP_DSTORE_3
   -1,  // OP_ASTORE_0
   -1,  // OP_ASTORE_1
   -1,  // OP_ASTORE_2
   -1,  // OP_ASTORE_3
   -3,  // OP_IASTORE
   -4,  // OP_LASTORE
   -3,  // OP_FASTORE
   -4,  // OP_DASTORE
   -3,  // OP_AASTORE
   -3,  // OP_BASTORE
   -3,  // OP_CASTORE
   -3,  // OP_SASTORE
   -1,  // OP_POP
   -2,  // OP_POP2
    1,  // OP_DUP
    1,  // OP_DUP_X1
    1,  // OP_DUP_X2
    2,  // OP_DUP2
    2,  // OP_DUP2_X1
    2,  // OP_DUP2_X2
    0,  // OP_SWAP
   -1,  // OP_IADD
   -2,  // OP_LADD
   -1,  // OP_FADD
   -2,  // OP_DADD
   -1,  // OP_ISUB
   -2,  // OP_LSUB
   -1,  // OP_FSUB
   -2,  // OP_DSUB
   -1,  // OP_IMUL
   -2,  // OP_LMUL
   -1,  // OP_FMUL
   -2,  // OP_DMUL
   -1,  // OP_IDIV
   -2,  // OP_LDIV
   -1,  // OP_FDIV
   -2,  // OP_DDIV
   -1,  // OP_IREM
   -2,  // OP_LREM
   -1,  // OP_FREM
   -2,  // OP_DREM
    0,  // OP_INEG
    0,  // OP_LNEG
    0,  // OP_FNEG
    0,  // OP_DNEG
   -1,  // OP_ISHL
   -1,  // OP_LSHL
   -1,  // OP_ISHR
   -1,  // OP_LSHR
   -1,  // OP_IUSHR
   -1,  // OP_LUSHR
   -1,  // OP_IAND
   -2,  // OP_LAND
   -1,  // OP_IOR
   -2,  // OP_LOR
   -1,  // OP_IXOR
   -2,  // OP_LXOR
    0,  // OP_IINC
    1,  // OP_I2L
    0,  // OP_I2F
    1,  // OP_I2D
   -1,  // OP_L2I
   -1,  // OP_L2F
    0,  // OP_L2D
    0,  // OP_F2I
    1,  // OP_F2L
    1,  // OP_F2D
   -1,  // OP_D2I
    0,  // OP_D2L
   -1,  // OP_D2F
    0,  // OP_I2B
    0,  // OP_I2C
    0,  // OP_I2S
   -3,  // OP_LCMP
   -1,  // OP_FCMPL
   -1,  // OP_FCMPG
   -3,  // OP_DCMPL
   -3,  // OP_DCMPG
   -1,  // OP_IFEQ
   -1,  // OP_IFNE
   -1,  // OP_IFLT
   -1,  // OP_IFGE
   -1,  // OP_IFGT
   -1,  // OP_IFLE
   -2,  // OP_IF_ICMPEQ
   -2,  // OP_IF_ICMPNE
   -2,  // OP_IF_ICMPLT
   -2,  // OP_IF_ICMPGE
   -2,  // OP_IF_ICMPGT
   -2,  // OP_IF_ICMPLE
   -2,  // OP_IF_ACMPEQ
   -2,  // OP_IF_ACMPNE
    0,  // OP_GOTO
    0,  // OP_JSR, caller must adjust stack by +1 at jsr target
    0,  // OP_RET
   -1,  // OP_TABLESWITCH
   -1,  // OP_LOOKUPSWITCH
   -1,  // OP_IRETURN
   -2,  // OP_LRETURN
   -1,  // OP_FRETURN
   -2,  // OP_DRETURN
   -1,  // OP_ARETURN
    0,  // OP_RETURN
    1,  // OP_GETSTATIC, caller must adjust +1 if long or double
   -1,  // OP_PUTSTATIC, caller must adjust -1 if long or double
    0,  // OP_GETFIELD,  caller must adjust +1 if long or double
   -2,  // OP_PUTFIELD,  caller must adjust -1 if long or double
   -1,  // OP_INVOKEVIRTUAL,   caller must adjust +return-args_length
   -1,  // OP_INVOKESPECIAL,   caller must adjust +return-args_length
    0,  // OP_INVOKESTATIC,    caller must adjust +return-args_length
   -1,  // OP_INVOKEINTERFACE, caller must adjust +return-args_length
    0,  // OP_XXXUNUSEDXXX
    1,  // OP_NEW
    0,  // OP_NEWARRAY
    0,  // OP_ANEWARRAY
    0,  // OP_ARRAYLENGTH
   -1,  // OP_ATHROW
    0,  // OP_CHECKCAST
    0,  // OP_INSTANCEOF
   -1,  // OP_MONITORENTER
   -1,  // OP_MONITOREXIT
    0,  // OP_WIDE
    0,  // OP_MULTIANEWARRAY, caller must adjust 1-dims
   -1,  // OP_IFNULL
   -1,  // OP_IFNONNULL
    0,  // OP_GOTO_W
    0,  // OP_JSR_W, caller must adjust stack by +1 at jsr target
    0,  // OP_SOFTWARE
    0   // OP_HARDWARE
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

