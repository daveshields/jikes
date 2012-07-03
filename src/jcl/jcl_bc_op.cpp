// $Id: jcl_bc_op.cpp,v 1.1 1999/11/04 18:48:03 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include <stdio.h>
#include <stdlib.h>
#include <iostream.h>
#include <string.h>
#include "jcl_bc_op.h"


void opdesc(int opc,char **name, char **desc)
{
        struct op_entry {
                char *op_name;
                char *op_desc;
        };
        struct op_entry table[] = {
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
  {"invokevirtual",  "invoke instance method; dispatch based on runtime type"},
  {"invokenonvirtual",  "invoke instance method; dispatch based on compile-time type"},
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

        if (opc==OP_SOFTWARE) {                                 // software
                 *name = *desc = "software";
         }
         else if (opc == OP_HARDWARE) {                 // hardware
                 *name = *desc = "hardware";
         }
         else if (opc >=0 && opc <= 0xc9) {
                 *name = table[opc].op_name;
                 *desc = table[opc].op_desc;
         }
         else {
                 *name = *desc = "illegal";
         }
}
#define INFO_NONE       0
#define INFO_LOCAL      1
#define INFO_CONST      2
#define INFO_DONE       3
// set code to be array of opcodes, codei to be code index
static void chaos(char * str) {
        cout << "chaos: " << str << "\n";
        cerr << "chaos: " << str << "\n";
        exit(1);
}

signed char get_i1(DynamicArray<u1>& code, int pc) {
        return code[pc];
}
short get_i2(DynamicArray<u1>& code, int pc) {
        return  code[pc] << 8 | code[pc+1];
}
int get_i4(DynamicArray<u1>& code, int pc) {
        return  code[pc] << 24 | code[pc+1] << 16 | code[pc+2] << 8 | code[pc+3];
}
unsigned int get_u1(DynamicArray<u1>& code, int pc) {
        unsigned val;
        val = code[pc];
        return val;
}
unsigned int get_u2(DynamicArray<u1>& code, int pc) {
        unsigned int val;
        val = (code[pc] << 8 | code[pc+1]);
        return val;
}
unsigned int get_u4(DynamicArray<u1>& code, int pc) {
        unsigned int val;
        val = (code[pc] << 24 | code[pc+1] << 16 | code[pc+2] << 8 | code[pc+3]);
        return val;
}
extern void const_string(DynamicArray<cp_info *>& constant_pool, int indx);
void opline(DynamicArray<cp_info *>& constant_pool,char * hdr, int pc, int opc, char *name,
                        char* args, char* desc,
                        int info_kind, int info_index)
{
        int len;
        // generate line of opcode dump, info is extra info

        cout<< *hdr;
        cout.width(4);
        cout << pc;
        cout << "\t" << name;
        len = strlen(name);
        if (strlen(args)) {
                cout << " " << args;
                len += (strlen(args) + 1);
        }
        if (len < 8) cout << "\t\t\t";
        else if (len < 16) cout << "\t\t";
//      else if (len <= 24) cout << "\t";
        else cout << "\t";
        switch (info_kind) {
                case INFO_CONST:
                        cout << " ";
                        if (info_index<=0 || info_index >constant_pool.length()) {
                                cout << "OUT-OF_BOUNDS CONSTANT_POOL-INDEX " << info_index;
                        }
                        else {
//                              constant_pool[info_index]->describe(constant_pool);
                        }
                        break;
                case INFO_LOCAL:
                        cout << " local#" << info_index;
                        break;
        }
// DS (17 jun) - skip desc for now: it's too long and shoud only
        // be written at user request.
//      cout <<" " << desc;
        cout << "\n";
}

extern void opdesc(unsigned opc, char ** name, char ** desc);

void  opdmp(DynamicArray<cp_info *>& constant_pool, DynamicArray<u1>& code){
        char argdesc[100];
        int pc_max = code.length();
        int pc_start;
        unsigned opc;
        unsigned int au1,au2;
        int ai1,ai2,ai4;
        int info_kind;
        int info_index;
        char *name, *desc;                      // set to name (mnemonic) and description of opcode.
        int pc=0;
        while (pc <pc_max) {
//      for (pc=0; pc<pc_max; pc++) {
                info_kind = INFO_NONE;                  // assume no extra info
                info_index=0;
                pc_start = pc;
                opc = get_u1(code,pc);
                opdesc(code[pc_start], &name, &desc);
                pc++;
                argdesc[0] = '\0';                              // assume no argument description needed
                switch (opc) {
                        case OP_BIPUSH:
                                ai1 = get_i1(code,pc); pc++;
                                sprintf(argdesc,"%d",ai1);
                                break;
                        case OP_SIPUSH:
                                ai2 = get_i2(code,pc); pc +=2;
                                sprintf(argdesc,"%d",ai2);
                                break;
                        case OP_LDC:
                                info_index = get_u1(code,pc); pc++;
                                sprintf(argdesc,"%d",info_index);
                                info_kind = INFO_CONST;
                                break;
                        case OP_LDC_W:
                        case OP_LDC2_W:
                                info_index = get_u2(code,pc);pc +=2;
                                sprintf(argdesc, "%u",info_index);
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
                                info_index = get_u1(code,pc);pc++;
                                sprintf(argdesc, "%u",info_index);
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
//                              info_index = get_u1(code,pc); pc++;
                                au1 = get_u1(code,pc); pc++;
                                ai1 = get_i1(code,pc); pc++;
                                info_kind = INFO_LOCAL;
                                sprintf(argdesc,"%d %d",au1,ai1);
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
                                ai2 = get_i2(code, pc);
                                sprintf(argdesc,"%d",(int) ( ai2+pc_start)); // compute branch target
                                pc +=2;
                                break;
                        case OP_RET:
                                au1 = get_u1(code,pc);
                                pc++;
                                sprintf(argdesc,"%d",(int)au1);
                                break;
                        case OP_TABLESWITCH:
                        {

                                int def,low,high,len,val,pc_this,op_this;
                                unsigned iu1;
                                op_this = OP_TABLESWITCH;
                                // account for padding
                                while (pc % 4) {
                                        iu1 = get_u1(code, pc);
                                        pc++;
                                }
                                def = get_i4(code, pc); pc += 4;
                                low = get_i4(code,pc); pc +=4;
                                high = get_i4(code,pc); pc += 4;
                                sprintf(argdesc,"default:%d low:%d high:%d",def+pc_start,low,high);
                                opline(constant_pool," ",pc_start, opc, name, argdesc, desc,info_kind,info_index);
                                len =  high - low + 1;
                                while (len) {
                                        pc_this = pc;
                                        val = get_i4(code,pc);
                                        sprintf(argdesc,"match:%d offset:%d",low++,val+pc_start);
                                        opline(constant_pool,"*",pc_this, op_this,name,argdesc,desc,INFO_NONE,0);
                                        pc += 4;
                                        len--;
                                }
                                info_kind=INFO_DONE;
                        }
                        break;
                        case OP_LOOKUPSWITCH:
                        {
                                int def,npairs,len,match,offset,op_this;
                                // account for padding
                                unsigned iu1;
                                op_this = OP_LOOKUPSWITCH;
                                while (pc % 4) {
                                        iu1 = get_u1(code, pc);
                                        pc++;
                                }
                                def = get_i4(code, pc); pc += 4;
                                npairs = get_i4(code,pc); pc +=4;
                                sprintf(argdesc,"default:%d npairs:%d",(def + pc_start), npairs);
                                opline(constant_pool," ",pc_start, op_this, name, argdesc, desc, info_kind,
                                           info_index);
                                len = npairs;
                                while (npairs) {int pcb;
                                    pcb = pc;
                                        match = get_i4(code,pc); pc +=4 ;
                                        offset = get_i4(code,pc); pc +=4;
                                        sprintf(argdesc,"match:%d offset:%d ",match,offset+pc_start);
                                        opline(constant_pool,"*", pcb, op_this, name, argdesc, desc, INFO_NONE,0);
                                        npairs--;
                                }
                                info_kind=INFO_DONE;
                        }
                        break;
                        case OP_GETSTATIC:
                        case OP_PUTSTATIC:
                        case OP_GETFIELD:
                        case OP_PUTFIELD:
                        case OP_INVOKEVIRTUAL:
                        case OP_INVOKENONVIRTUAL:
                        case OP_INVOKESTATIC:
                        case OP_NEW:
                        case OP_ANEWARRAY:
                        case OP_CHECKCAST:
                        case OP_INSTANCEOF:
                                info_index = get_u2(code,pc); pc += 2;
                                sprintf(argdesc,"%d",info_index);
                                // generate const_stringing from au2
                                info_kind = INFO_CONST;
                                break;
                        case OP_INVOKEINTERFACE:
                        {

                                int nargs;
                                info_index = get_u2(code,pc); pc += 2;
                                au1 = get_u1(code,pc); pc++;
                                nargs = au1;
                                au1 = get_u1(code,pc); pc++;
                                if (au1) {
                                        chaos("zero byte required in this position");
                                }
                                sprintf(argdesc,"%d %d",nargs,info_index);
                                info_kind=INFO_CONST;
                        }
                        break;
                        case OP_NEWARRAY:
                                au1 = get_u1(code,pc); pc++;
                                switch(au1) {
                                  case 4: sprintf(argdesc,"%s","BOOLEAN");break;
                                  case 5: sprintf(argdesc,"%s","CHAR");break;
                                  case 6: sprintf(argdesc,"%s","FLOAT");break;
                                  case 7: sprintf(argdesc,"%s","DOUBLE");break;
                                  case 8: sprintf(argdesc,"%s","BYTE");break;
                                  case 9: sprintf(argdesc,"%s","SHORT");break;
                                  case 10: sprintf(argdesc,"%s","INT");break;
                                  case 11: sprintf(argdesc,"%s","LONG");break;
                                  default:
                                        sprintf(argdesc,"%s","<UNKNOWN>");break;
                                }
                                break;
                        case OP_WIDE:
                                chaos("dmp for op_wide not supported yet");
                                break;
                        case OP_MULTIANEWARRAY:
                                info_index = get_u2(code,pc); pc += 2;
                                au1 = get_u1(code,pc); pc++;
                                info_kind = INFO_CONST;
                                // au1 gives dimensions
                                sprintf(argdesc,"%u",au1);
                                break;
                        case OP_GOTO_W:
                        case OP_JSR_W:
                                ai4 = get_i4(code,pc); pc += 4;
                                // ai4 gives offset (wide) of branch target
                                sprintf(argdesc,"%d", pc_start + ai4);
                                break;
                        default:
                                break;
                }
                // output first part of description
                if (info_kind!=INFO_DONE) opline(constant_pool," ",pc_start, opc, name, argdesc, desc,info_kind,info_index);
        }
}
