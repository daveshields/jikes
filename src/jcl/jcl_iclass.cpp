// $Id: jcl_iclass.cpp,v 1.2 1999/12/09 18:02:26 lord Exp $
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
#include "jcl_iclass.h"


inline u1 InputClassFile::get_u1()
{
    int c = getc(classfile);
    if (c == EOF)
    {
cerr << "END-OF-FILE REACHED PREMATURELY !!!\n";
        /* crash !!! */
    }
    return (u1) c;
}


inline u2 InputClassFile::get_u2()
{
    u2 i = get_u1();
    return (i << 8) + get_u1();
}


inline u4 InputClassFile::get_u4()
{
    u4 i = get_u1();
    i = (i << 8) + get_u1();
    i = (i << 8) + get_u1();
    return (i << 8) + get_u1();
}



CONSTANT_Class_info *InputClassFile::get_CONSTANT_Class(u1 &tag)
{
    CONSTANT_Class_info *p = new CONSTANT_Class_info(tag);

    p -> name_index = get_u2();

    return p;
}


CONSTANT_Fieldref_info *InputClassFile::get_CONSTANT_Fieldref(u1 &tag)
{
    CONSTANT_Fieldref_info *p = new CONSTANT_Fieldref_info(tag);

    p -> class_index = get_u2();
    p -> name_and_type_index = get_u2();

    return p;
}


CONSTANT_Methodref_info *InputClassFile::get_CONSTANT_Methodref(u1 &tag)
{
    CONSTANT_Methodref_info *p = new CONSTANT_Methodref_info(tag);

    p -> class_index = get_u2();
    p -> name_and_type_index = get_u2();

    return p;
}


CONSTANT_InterfaceMethodref_info *InputClassFile::get_CONSTANT_InterfaceMethodref(u1 &tag)
{
    CONSTANT_InterfaceMethodref_info *p = new CONSTANT_InterfaceMethodref_info(tag);

    p -> class_index = get_u2();
    p -> name_and_type_index = get_u2();

    return p;
}



CONSTANT_String_info *InputClassFile::get_CONSTANT_String(u1 &tag)
{
    CONSTANT_String_info *p = new CONSTANT_String_info(tag);

    p -> string_index = get_u2();

    return p;
}


CONSTANT_Integer_info *InputClassFile::get_CONSTANT_Integer(u1 &tag)
{
    CONSTANT_Integer_info *p = new CONSTANT_Integer_info(tag);
#ifdef TBSL
        // first cut by ds at using value directly
        unsigned char  bytes[4];
        int i;
        unsigned uv;
        for (i=0;i<4;i++) bytes[i] = get_u1();
//    bytes = get_u4();
                 uv = bytes[0];
        for (i=1;i<4;i++) uv = uv << 8 | bytes[i];
                   
        p->value = (int) uv; 
#endif
        p->bytes = get_u4();
    return p;
}


CONSTANT_Float_info *InputClassFile::get_CONSTANT_Float(u1 &tag)
{
    CONSTANT_Float_info *p = new CONSTANT_Float_info(tag);

    p -> bytes = get_u4();

    return p;
}


CONSTANT_Long_info *InputClassFile::get_CONSTANT_Long(u1 &tag)
{
    CONSTANT_Long_info *p = new CONSTANT_Long_info(tag);

    p -> high_bytes = get_u4();
    p -> low_bytes = get_u4();

    return p;
}


CONSTANT_Double_info *InputClassFile::get_CONSTANT_Double(u1 &tag)
{
    CONSTANT_Double_info *p = new CONSTANT_Double_info(tag);

    p -> high_bytes = get_u4();
    p -> low_bytes = get_u4();

    return p;
}

CONSTANT_NameAndType_info *InputClassFile::get_CONSTANT_NameAndType(u1 &tag)
{
    CONSTANT_NameAndType_info *p = new CONSTANT_NameAndType_info(tag);

    p -> name_index = get_u2();
    p -> descriptor_index = get_u2();

    return p;
}

CONSTANT_Utf8_info *InputClassFile::get_CONSTANT_Utf8(u1 &tag)
{
    CONSTANT_Utf8_info *p = new CONSTANT_Utf8_info(tag);

    p -> length_ = get_u2();
    p -> bytes = new char[p -> length() + 1];

    for (int i = 0; i < p -> length(); i++)
         p -> bytes[i] = get_u1();
    p -> bytes[p -> length()] = '\0';

    return p;
}

cp_info *InputClassFile::get_cp_info()
{
    u1 tag = get_u1();

    switch(tag)
    {
        case CONSTANT_Class:
             return get_CONSTANT_Class(tag);
             break;
        case CONSTANT_Fieldref:
             return get_CONSTANT_Fieldref(tag);
             break;
        case CONSTANT_Methodref:
             return get_CONSTANT_Methodref(tag);
             break;
        case CONSTANT_String:
             return get_CONSTANT_String(tag);
             break;
        case CONSTANT_Integer:
             return get_CONSTANT_Integer(tag);
             break;
        case CONSTANT_Float:
             return get_CONSTANT_Float(tag);
             break;
        case CONSTANT_Long:
             return get_CONSTANT_Long(tag);
             break;
        case CONSTANT_Double:
             return get_CONSTANT_Double(tag);
             break;
        case CONSTANT_InterfaceMethodref:
             return get_CONSTANT_InterfaceMethodref(tag);
             break;
        case CONSTANT_NameAndType:
             return get_CONSTANT_NameAndType(tag);
             break;
        case CONSTANT_Utf8:
             return get_CONSTANT_Utf8(tag);
             break;
        default:
cerr << "CODE \"" << (int) tag << "\" is an invalid tag !!!\n";
             break;
    }

    return NULL;
}


SourceFile_attribute *InputClassFile::get_SourceFile_attribute(u2 &name_index, u4 &length)
{
    SourceFile_attribute *p = new SourceFile_attribute(name_index, length);

    p -> sourcefile_index = get_u2();

    return p;
}


Exceptions_attribute *InputClassFile::get_Exceptions_attribute(u2 &name_index, u4 &length)
{
    Exceptions_attribute *p = new Exceptions_attribute(name_index, length);

    u2 number_of_exceptions = get_u2();

    p -> exception_index_table.resize(number_of_exceptions);
    for (int i = 0; i < number_of_exceptions; i++)
        p -> exception_index_table[i] = get_u2();

    return p;
}


ConstantValue_attribute *InputClassFile::get_ConstantValue_attribute(u2 &name_index, u4 &length)
{
    ConstantValue_attribute *p = new ConstantValue_attribute(name_index, length);

    p -> constantvalue_index = get_u2();

    return p;
}


Code_attribute *InputClassFile::get_Code_attribute(u2 &name_index, u4 &length)
{
    Code_attribute *p = new Code_attribute(name_index, length);

    p -> max_stack = get_u2();
    p -> max_locals = get_u2();

    u4 code_length;
    code_length = get_u4();
    p -> code.resize(code_length);
    int i;
    for (i = 0; i < code_length; i++)
        p -> code[i] = get_u1();
    u2 exception_table_length = get_u2();
    p -> exception_table.resize(exception_table_length);
    for (i = 0; i < exception_table_length; i++)
    {
        p -> exception_table[i].start_pc   = get_u2();
        p -> exception_table[i].end_pc     = get_u2();
        p -> exception_table[i].handler_pc = get_u2();
        p -> exception_table[i].catch_type = get_u2();
    }
    u2 attributes_count = get_u2();
    p -> attributes.resize(attributes_count);
    for (i = 0; i < attributes_count; i++)
        p -> attributes[i] = get_attribute_info();

    return p;
}

LineNumberTable_attribute *InputClassFile::get_LineNumberTable_attribute(u2 &name_index, u4 &length)
{
    LineNumberTable_attribute *p = new LineNumberTable_attribute(name_index, length);

    u2 line_number_table_length = get_u2();
    p -> line_number_table.resize(line_number_table_length);
    for (int i = 0; i < line_number_table_length; i++)
    {
        p -> line_number_table[i].start_pc = get_u2();
        p -> line_number_table[i].line_number = get_u2();
    }

    return p;
}


LocalVariableTable_attribute *InputClassFile::get_LocalVariableTable_attribute(u2 &name_index, u4 &length)
{
    LocalVariableTable_attribute *p = new LocalVariableTable_attribute(name_index, length);

    u2 local_variable_table_length = get_u2();
    p -> local_variable_table.resize(local_variable_table_length);
    for (int i = 0; i < local_variable_table_length; i++)
    {
        p -> local_variable_table[i].start_pc = get_u2();
        p -> local_variable_table[i].length = get_u2();
        p -> local_variable_table[i].name_index = get_u2();
        p -> local_variable_table[i].descriptor_index = get_u2();
        p -> local_variable_table[i].index = get_u2();
    }

    return p;
}

InnerClasses_attribute *InputClassFile::get_InnerClasses_attribute(u2 &name_index, u4 &length)
{
    InnerClasses_attribute *p = new InnerClasses_attribute(name_index, length);

    u2 inner_classes_length = get_u2();
    p -> inner_classes.resize(inner_classes_length);
    for (int i = 0; i < inner_classes_length; i++)
    {
        p -> inner_classes[i].inner_class_info_index = get_u2();
        p -> inner_classes[i].outer_class_info_index = get_u2();
        p -> inner_classes[i].inner_name_index = get_u2();
        p -> inner_classes[i].inner_class_access_flags = get_u2();
    }

    return p;
}

Synthetic_attribute *InputClassFile::get_Synthetic_attribute(u2 &name_index, u4 &length)
{
    Synthetic_attribute *p = new Synthetic_attribute(name_index, length);


    return p;
}

Deprecated_attribute *InputClassFile::get_Deprecated_attribute(u2 &name_index, u4 &length)
{
    Deprecated_attribute *p = new Deprecated_attribute(name_index, length);


    return p;
}


GenericAttribute_info *InputClassFile::get_GenericAttribute(u2 &name_index, u4 &length)
{
    GenericAttribute_info *p = new GenericAttribute_info(name_index, length);

    p -> info.resize(length);
    for (int i = 0; i < length; i++)
        p -> info[i] = get_u1();

    return p;
}

attribute_info *InputClassFile::get_attribute_info()
{
    u2 name_index = get_u2();
    u4 length = get_u4();

    cp_info *p = constant_pool[name_index];
    CONSTANT_Utf8_info *q = (CONSTANT_Utf8_info *) p;
 
    switch(q -> length())
    {
        case 4:
             {
                               if (strcmp(q -> bytes, "Code") == 0)
                     return get_Code_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        case 9:
             {
                 if (strcmp(q -> bytes, "Synthetic") == 0)
                      return get_Synthetic_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        case 10:
             {
                 if (strcmp(q -> bytes, "SourceFile") == 0)
                      return get_SourceFile_attribute(name_index, length);
                 else if (strcmp(q -> bytes, "Exceptions") == 0)
                      return get_Exceptions_attribute(name_index, length);
                 else if (strcmp(q -> bytes, "Deprecated") == 0)
                      return get_Deprecated_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        case 12:
             {
                 if (strcmp(q -> bytes, "InnerClasses") == 0)
                      return get_InnerClasses_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        case 13:
             {
                 if (strcmp(q -> bytes, "ConstantValue") == 0)
                     return get_ConstantValue_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        case 15:
             {
                 if (strcmp(q -> bytes, "LineNumberTable") == 0)
                     return get_LineNumberTable_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        case 18:
             {
                 if (strcmp(q -> bytes, "LocalVariableTable") == 0)
                     return get_LocalVariableTable_attribute(name_index, length);
                 else
                     /* crash !!! */ ;
             }
             break;
        default:
             /* crash !!! */ ;
             break;
     }

cerr << "String \"" << q -> bytes << "\" is not a valid attribute code !!!\n";
exit(1);

    return get_GenericAttribute(name_index, length);
}


void InputClassFile::get_field_info(field_info &f)
{
    f.access_flags = get_u2();
    f.name_index = get_u2();
    f.descriptor_index = get_u2();

    u2 attributes_count = get_u2();
    f.attributes.resize(attributes_count);
    for (int i = 0; i < attributes_count; i++)
        f.attributes[i] = get_attribute_info();

    return;
}


void InputClassFile::get_method_info(method_info &m)
{
    m.access_flags = get_u2();
    m.name_index = get_u2();
    m.descriptor_index = get_u2();

    u2 attributes_count = get_u2();
    m.attributes.resize(attributes_count);
    for (int i = 0; i < attributes_count; i++)
        m.attributes[i] = get_attribute_info();

    return;
}


InputClassFile::~InputClassFile()
{
    int i;
    for (i = 1; i < constant_pool.length(); i++)
        delete constant_pool[i];

    for (i = 0; i < attributes.length(); i++)
        delete attributes[i];

    if (classfile)
        fclose(classfile);

    return;
}

InputClassFile::InputClassFile(char* filename) 
{
    magic = minor_version = major_version = this_class = super_class = 0;
    if ((classfile = fopen(filename, "rb")) == NULL)
        return;

    magic = get_u4();
    minor_version = get_u2();
    major_version = get_u2();

    u2 constant_pool_count = get_u2();
    constant_pool.resize(constant_pool_count);
    constant_pool[0] = NULL;
    int i;
    for (i = 1; i < constant_pool_count; i++)
    {
        constant_pool[i] = get_cp_info();
        if (constant_pool[i] -> tag == CONSTANT_Long ||
            constant_pool[i] -> tag == CONSTANT_Double)
             constant_pool[++i] = NULL; // skip the next entry for eight-byte constants
    }

    access_flags = get_u2();
    this_class = get_u2();
    super_class = get_u2();

    u2 interfaces_count = get_u2();
    interfaces.resize(interfaces_count);
    for (i = 0; i < interfaces_count; i++)
        interfaces[i] = get_u2();

    u2 fields_count = get_u2();
    fields.resize(fields_count);
    for (i = 0; i < fields_count; i++)
        get_field_info(fields[i]);

    u2 methods_count = get_u2();
    methods.resize(methods_count);
    for (i = 0; i < methods_count; i++)
        get_method_info(methods[i]);

    u2 attributes_count = get_u2();
    attributes.resize(attributes_count);
    for (i = 0; i < attributes_count; i++)
        attributes[i] = get_attribute_info();

    return;
}


