// $Id: jcl_iclass.h,v 1.1 1999/11/04 18:48:03 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_iclass_INCLUDED
#define jcl_iclass_INCLUDED
#include <stdio.h>
#include "jcl_class.h"

class InputClassFile : public ClassFile
{
private:
public:


    FILE *classfile;

    u1 get_u1();
    u2 get_u2();
    u4 get_u4();
 
    CONSTANT_Class_info *get_CONSTANT_Class(u1 &tag);
    CONSTANT_Fieldref_info *get_CONSTANT_Fieldref(u1 &tag);
    CONSTANT_Methodref_info *get_CONSTANT_Methodref(u1 &tag);
    CONSTANT_InterfaceMethodref_info *get_CONSTANT_InterfaceMethodref(u1 &tag);
    CONSTANT_String_info *get_CONSTANT_String(u1 &tag);
    CONSTANT_Integer_info *get_CONSTANT_Integer(u1 &tag);
    CONSTANT_Float_info *get_CONSTANT_Float(u1 &tag);
    CONSTANT_Long_info *get_CONSTANT_Long(u1 &tag);
    CONSTANT_Double_info *get_CONSTANT_Double(u1 &tag);
    CONSTANT_NameAndType_info *get_CONSTANT_NameAndType(u1 &tag);
    CONSTANT_Utf8_info *get_CONSTANT_Utf8(u1 &tag);
    cp_info *get_cp_info();
        void reset(){           // reset to empty state.
                constant_pool.reset();
                fields.reset();
                methods.reset();
                attributes.reset();
                classfile = (FILE *)0;
                this_class = super_class = 0;
        }
    SourceFile_attribute *get_SourceFile_attribute(u2 &name_index, u4 &length);
    Exceptions_attribute *get_Exceptions_attribute(u2 &name_index, u4 &length);
    ConstantValue_attribute *get_ConstantValue_attribute(u2 &name_index, u4 &length);
    Code_attribute *get_Code_attribute(u2 &name_index, u4 &length);
    LineNumberTable_attribute *get_LineNumberTable_attribute(u2 &name_index, u4 &length);
    LocalVariableTable_attribute *get_LocalVariableTable_attribute(u2 &name_index, u4 &length);
    Synthetic_attribute *get_Synthetic_attribute(u2 &name_index, u4 &length);
    Deprecated_attribute *get_Deprecated_attribute(u2 &name_index, u4 &length);
    InnerClasses_attribute *get_InnerClasses_attribute(u2 &name_index, u4 &length);
    GenericAttribute_info *get_GenericAttribute(u2 &name_index, u4 &length);
    attribute_info *get_attribute_info();
        
    void get_field_info(field_info &f);
    void get_method_info(method_info &f);

public:
        InputClassFile() {};
    InputClassFile(char* filename);
    ~InputClassFile();
};


#endif
