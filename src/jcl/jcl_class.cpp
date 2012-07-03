// $Id: jcl_class.cpp,v 1.1 1999/11/04 18:48:03 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include <iostream.h>
#include <string.h>
#include "jcl_class.h"
#include "jcl_long.h"


#ifdef TEST
        //virtual
                        void ClassFile::print() {
                int i;
                cout << "magic " << hex << magic << dec
                         << "  major_version " << major_version
                         << "  minor_version " << minor_version << "\n";
                AccessFlags::print();
//              access_flags.print(constant_pool);
                cout << "\n";
                cout << "  this_class " << this_class << "  super_class " << super_class <<"\n";
                cout << "  constant_pool: " << constant_pool.length() << "\n";
                for (i=1;i<constant_pool.length();i++) {
                        cout << "  " << i << "  ";
                        constant_pool[i]->print(constant_pool);
            if (constant_pool[i] -> tag == CONSTANT_Long ||
                constant_pool[i] -> tag == CONSTANT_Double) {
                i++;; // skip the next entry for eight-byte constants
            }
                }
                cout << "  interfaces " << interfaces.length() <<": ";
                for (i=0;i<interfaces.length();i++) cout << "  " << (int) interfaces[i];
                cout <<"\n";
                for (i=0;i<fields.length();i++) {
                        cout << "field " << i << "\n";
                        fields[i].print(constant_pool);
                }
                cout << "methods length " << methods.length() << "\n";
                for (i=0;i<methods.length();i++) {
                        cout << "method " << i << "\n";
                        methods[i].print(constant_pool);
                }
                for (i=0;i<attributes.length();i++) {
                        cout << "attribute " << i << "\n";
                        attributes[i]->print(constant_pool);
                }
                cout << "\n";
        } 
//virtual
   void ClassFile::print_const(DynamicArray<cp_info *>& constant_pool,int i) 
{

// return string identifying constant pool entry
        if (i<1 || i > constant_pool.length()) {
                cout << " constant_pool index out of range" << i << "\n:";
                return;
        }
        else if (constant_pool[i]==NULL) { // if this entry to be skipped
      cout << "NO-SUCH-ENTRY";
      return;
        }
        // save this code if want brief form
        if (i<0 | i > constant_pool.length()) {
                cout << "illegal cp index (" << i << ")" ;
                return;
        }
        constant_pool[i]->describe(constant_pool);
}

 void ClassFile::print_const(int i) 
{

// return string identifying constant pool entry
        if (i<1 || i > constant_pool.length()) {
                cout << " constant_pool index out of range" << i << "\n:";
                return;
        }
        else if (constant_pool[i]==NULL) { // if this entry to be skipped
      cout << "NO-SUCH-ENTRY";
      return;
        }
        // save this code if want brief form
        if (i<0 | i > constant_pool.length()) {
                cout << "illegal cp index (" << i << ")" ;
                return;
        }
        cp_info *cp=constant_pool[i];
    switch(cp->tag) {
        case ClassFile::CONSTANT_Class:{
            CONSTANT_Class_info * ccp = (CONSTANT_Class_info *) cp;
            cout << " Class:";
                print_const(ccp->name_index);
            break;
            }
        case ClassFile::CONSTANT_Fieldref: {
            CONSTANT_Fieldref_info * ccp = (CONSTANT_Fieldref_info *) cp;
            cout << "Field:" << ccp->class_index;
            print_const(ccp->class_index);
            cout << ".";
            print_const(ccp->name_and_type_index);
            break;
            }
        case ClassFile::CONSTANT_Methodref: {
            CONSTANT_Methodref_info * ccp = (CONSTANT_Methodref_info *) cp;
            cout << "Methodref:";
            print_const(ccp->class_index);
            cout << ".";
            print_const(ccp->name_and_type_index);
            break;
            }
        case ClassFile::CONSTANT_InterfaceMethodref: {
            CONSTANT_InterfaceMethodref_info * ccp = (CONSTANT_InterfaceMethodref_info *) cp;
            cout << "InterfaceMethodref: " ;
            print_const(ccp->class_index);
            cout << ".";
            print_const(ccp->name_and_type_index);
            break;
            }
        case ClassFile::CONSTANT_String: {
            CONSTANT_String_info * ccp = (CONSTANT_String_info *) cp;
//          cout << " U:";
            print_const(ccp->string_index);
            break; 
            }
        case ClassFile::CONSTANT_Integer: {
            CONSTANT_Integer_info * ccp = (CONSTANT_Integer_info *) cp;
            int *ip;
            ip = (int *) & ccp->bytes;
            cout << "Integer:";
            cout << *ip;
            break;
            }
        case ClassFile::CONSTANT_Float: {
            CONSTANT_Float_info * ccp = (CONSTANT_Float_info *) cp;
            float *fp;
            fp = (float *) &ccp->bytes;
            cout << "Float:";
                cout<< *fp;
            break;
            }
        case ClassFile::CONSTANT_Long: {
            CONSTANT_Long_info * ccp = (CONSTANT_Long_info *) cp;
            char  longstr[65];
            cout << "Long:";
            Long(ccp->high_bytes, ccp->low_bytes).String(longstr);
            cout << longstr;
            print_const(constant_pool,i);
            break;
            }
        case ClassFile::CONSTANT_Double: {
            CONSTANT_Double_info * ccp = (CONSTANT_Double_info *) cp;
            Long lval = Long(ccp->high_bytes,ccp->low_bytes);
            cout << "Double:";
            cout << lval.DoubleView();
            print_const(constant_pool,i);
            break;
            }
        case ClassFile::CONSTANT_NameAndType: {
            CONSTANT_NameAndType_info * ccp = (CONSTANT_NameAndType_info *) cp;
            cout << " NameAndType " ;
            print_const(ccp->name_index);
            cout << " " ;
            print_const(ccp->descriptor_index);
            break;
            }
        case ClassFile::CONSTANT_Utf8: {
            CONSTANT_Utf8_info * ccp = (CONSTANT_Utf8_info *) cp;
                int i;
                cout << " \"";
                for (i=0;i<ccp->length();i++) {
                        Unicode::Cout(ccp->bytes[i]);
                }
                cout << "\"";
            break;
            }
                
        default:
            cout << "uknown constant kind " << cp->tag  << " for pool index " << i << "\n";
            cerr << "uknown constant kind " << cp->tag  << " for pool index " << i << "\n";
        }

}

#endif
