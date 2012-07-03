// $Id: jcl_class.h,v 1.1 1999/11/04 18:48:03 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_class_INCLUDED
#define jcl_class_INCLUDED

#include <stdio.h>
#include "jcl_access.h"
#include "jcl_dynamic.h"
#include "jcl_unicode.h"


class cp_info
{
public:
    u1 tag;

    cp_info(u1 _tag) : tag(_tag) {}

    virtual ~cp_info() {}
#ifdef TEST
    virtual void print(DynamicArray<cp_info *>& constant_pool) {
            cout << (int) tag;
    }
    virtual void describe(DynamicArray<cp_info *>& constant_pool) {
            cout << (int) tag;
    }
#endif
};


class CONSTANT_Class_info : public cp_info
{
public:
    /* u1 tag; */
    u2 name_index;

    CONSTANT_Class_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Class_info() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_Class_info: name_index " << name_index << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                cout << "Class:";  constant_pool[name_index]->describe(constant_pool);
        }
#endif  
};




class CONSTANT_Fieldref_info : public cp_info
{
public:
    /* u1 tag; */
    u2 class_index;
    u2 name_and_type_index;

    CONSTANT_Fieldref_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Fieldref_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_Fieldref_info: class_index: " << class_index
                         << ", name_and_type_index: " << name_and_type_index << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                constant_pool[class_index]->describe(constant_pool); cout << ".";
                constant_pool[name_and_type_index]->describe(constant_pool);
        }
#endif  
};


class CONSTANT_Methodref_info : public cp_info
{
public:
    /* u1 tag; */
    u2 class_index;
    u2 name_and_type_index;


    CONSTANT_Methodref_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Methodref_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_Methodref_info: class_index: " << class_index
                         << ", name_and_type_index: " << name_and_type_index << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                constant_pool[class_index]->describe(constant_pool); cout << ".";
                constant_pool[name_and_type_index]->describe(constant_pool);
        }
#endif  
};


class CONSTANT_InterfaceMethodref_info : public cp_info
{
public:
    /* u1 tag; */
    u2 class_index;
    u2 name_and_type_index;

    CONSTANT_InterfaceMethodref_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_InterfaceMethodref_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_InterfaceMethodref_info: class_index: " << class_index
                         << ", name_and_type_index: " << name_and_type_index << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                constant_pool[class_index]->describe(constant_pool); cout << ".";
                constant_pool[name_and_type_index]->describe(constant_pool);
        }
#endif  
};


class CONSTANT_String_info : public cp_info
{
public:
    /* u1 tag; */
    u2 string_index;

    CONSTANT_String_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_String_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_String_info: string_index: " << string_index << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                constant_pool[string_index]->describe(constant_pool);
        }
#endif
};


class CONSTANT_Integer_info : public cp_info
{
public:
    /* u1 tag; */
    u4 bytes;

    CONSTANT_Integer_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Integer_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
        int val;
        val = ((bytes>>24)&0xff)<<24 | ((bytes>>16)&0xff)<<16
                | ((bytes>>8)&0xff)<<8 | (bytes&0xff);
//              cout << "CONSTANT_Integer_info: bytes " <<  value << "\n";
                cout << "CONSTANT_Integer_info: bytes " << val << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                cout << "I:";
//              constant_pool[string_index]->describe(constant_pool);
        }
#endif  
};


class CONSTANT_Float_info : public cp_info
{
public:
    /* u1 tag; */
    u4 bytes;

    CONSTANT_Float_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Float_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_Float_info: bytes " << (float) bytes << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                cout << "F:";
//              constant_pool[string_index]->describe(constant_pool);
        }
#endif  
};


class CONSTANT_Long_info : public cp_info
{
public:
    /* u1 tag; */
    u4 high_bytes;
    u4 low_bytes;

    CONSTANT_Long_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Long_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_Long_info: bytes \n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                cout << "L:";
//              constant_pool[string_index]->describe(constant_pool);
        }
#endif  
};


class CONSTANT_Double_info : public cp_info
{
public:
    /* u1 tag; */
    u4 high_bytes;
    u4 low_bytes;

    CONSTANT_Double_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_Double_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_Float_info: bytes  \n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                cout << "D:";
//              constant_pool[string_index]->describe(constant_pool);
        }
#endif  
};
class CONSTANT_NameAndType_info : public cp_info
{
public:
    /* u1 tag; */
    u2 name_index;
    u2 descriptor_index;

    CONSTANT_NameAndType_info(u1 _tag) : cp_info(_tag) {}
    virtual ~CONSTANT_NameAndType_info() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "CONSTANT_NameAndType_info: name_index: " << name_index
                         << ", descriptor_index: " << descriptor_index << "\n";
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                constant_pool[name_index]->describe(constant_pool);
                cout << " ";
                constant_pool[descriptor_index]->describe(constant_pool);
        }
#endif  
};

class CONSTANT_Utf8_info : public cp_info
{
public:
    /* u1 tag; */
    u2 length() { return length_; }
    char *bytes; /* bytes[length+1] ... after input a '\0' will be added. */

    CONSTANT_Utf8_info(u1 _tag) : cp_info(_tag) {}

    virtual ~CONSTANT_Utf8_info()
    {
        delete [] bytes;
    }
public:
    u2 length_;
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                
                cout << "CONSTANT_Utf8_info: length: " << length_<< " ";
                for (int i=0;i<length_;i++) Unicode::Cout(bytes[i]);
                cout << "\n";
                // should only do packing when actually write the string out!!
                // so have same common internal form on input and output.
        }
        virtual void describe(DynamicArray<cp_info *>& constant_pool) {
                
                cout << "\"";
                for (int i=0;i<length_;i++) Unicode::Cout(bytes[i]);
                cout << "\"";
//              Unicode::Cout(bytes);
        }
#endif  
};

// field_info and method_infoshould be defined here, but they contain attributes, so it is necessary
// to define the attributes first.


class attribute_info
{
public:
    enum 
    {
        Generic,
        SourceFile,
        ConstantValue,
        Code,
        Exceptions,
        LineNumberTable,
        LocalVariableTable,
        Synthetic,
        Deprecated,
        InnerClasses
    };

    u1 tag;
    u2 attribute_name_index;
    u4 attribute_length;

    attribute_info(u1 _tag) : tag(_tag) {}
        attribute_info(u1 _tag, u2 _name_index, u4 _length) :
                        tag(_tag), attribute_name_index(_name_index),
                        attribute_length(_length) {}

    virtual ~attribute_info() {};
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "print for attribute info tag " << tag << " not defined\n";
        }
#endif  
};

class GenericAttribute_info : public attribute_info
{
public:
//    u2 attribute_name_index;
    u4 attribute_length() { return info.length(); }
    DynamicArray<u1> info; /* info[attribute_length] */

    GenericAttribute_info(u2 &_name_index, u4 &_length)
           : attribute_info(Generic, _name_index, _length) {}

    virtual ~GenericAttribute_info() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "print for attribute info tag " << tag << " not defined\n";
        }
#endif  
};

class SourceFile_attribute : public attribute_info
{
public:
//    u2 attribute_name_index;
//    u4 attribute_length; /* must be 2 */
    u2 sourcefile_index;

    SourceFile_attribute(u2 &_name_index, u4 &_length)
          : attribute_info(SourceFile,_name_index,_length) {}

    virtual ~SourceFile_attribute() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "SourceFile_attribute attribute_name_index " << attribute_name_index
                         << " length " << attribute_length
                         << " sourcefile_index " << sourcefile_index << "\n";
        }
#endif  
};

class ConstantValue_attribute : public attribute_info
{
public:
    ConstantValue_attribute(u2 &_name_index, u4 &_length)
            : attribute_info(ConstantValue,_name_index,_length) {}

//    u2 attribute_name_index;
//    u4 attribute_length; /* must be 2 */
    u2 constantvalue_index;

    virtual ~ConstantValue_attribute() {}
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "ConstantValue_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length
                         << " constantvalue_index " << constantvalue_index << "\n";
        }
#endif  
};

class Code_attribute : public attribute_info
{
public:
    Code_attribute(u2 &_name_index, u4 &_length)
         : attribute_info(Code,_name_index,_length){}

//    u2 attribute_name_index;
//    u4 attribute_length;
    u2 max_stack;
    u2 max_locals;
    /* u4 code_length; */
    DynamicArray<u1> code; /* code[code_length] */
    u2 exception_table_length() { return exception_table.length(); }
    struct exception_element
    {
        u2 start_pc;
        u2 end_pc;
        u2 handler_pc;
        u2 catch_type;
    };
    DynamicArray<exception_element> exception_table; /* exceptiontable[exception_table_length] */
    u2 attributes_count() { return attributes.length(); }
    DynamicArray<attribute_info *> attributes; /* attributes[attributes_count] */
        Code_attribute() : attribute_info(Code,0,0),
    max_stack(0),max_locals(0)
                        {}
    virtual ~Code_attribute()
     {
         for (int i = 0; i < attributes.length(); i++)
             if (attributes[i])
                 delete attributes[i];
     }
#ifdef TEST     
virtual void  print(DynamicArray<cp_info *>& constant_pool)
        {

    void  opdmp(DynamicArray<cp_info *>& constant_pool, DynamicArray<u1>& code);
                int i;
                cout << "Code_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
                cout << " max_stack " << max_stack <<
                        " max_locals "  << max_locals <<
                        " code_length "  << code.length() << "\n";
                if (exception_table.length()) {
                        cout << " exception_table: " << exception_table.length() << " entries\n";
                        for (i=0; i<exception_table.length(); i++) {
                                cout <<
                                "  start_pc " << exception_table[i].start_pc <<
                                "  end_pc " << exception_table[i].end_pc <<
                                "  handler_pc " << exception_table[i].handler_pc <<
                                "  catch_type " << exception_table[i].catch_type << "\n";
//                              const_string(cp,exception_table[i].catch_type);
                        }
                }
                opdmp(constant_pool,code);
                cout << "  \n";
                for (i=0;i<attributes.length();i++) {
                        attributes[i]->print(constant_pool);
                }
        }
#endif  

};

class Exceptions_attribute : public attribute_info
{
public:
//    u2 attribute_name_index;
//    u4 attribute_length; /* must be 2 */
     u2 number_of_exceptions() { return exception_index_table.length(); }
    DynamicArray<u2> exception_index_table; /* exception_index_table[number_of_exceptions] */

    Exceptions_attribute(u2 &_name_index, u4 &_length)
              : attribute_info(Exceptions,_name_index, _length) {}
     virtual ~Exceptions_attribute() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "Exceptions_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
                for (int i=0; i<exception_index_table.length(); i++) {
                        cout << "    " << exception_index_table[i];
                }
                cout << "\n";
        }
#endif  
};

class LineNumberTable_attribute : public attribute_info
{
public:
    LineNumberTable_attribute(u2 &_name_index, u4 &_length)
                   : attribute_info(LineNumberTable,_name_index,_length) {}

//    u2 attribute_name_index;
//    u4 attribute_length;
     u2 line_number_table_length() { return line_number_table.length();}
    struct line_number_element
    {
        u2 start_pc;
        u2 line_number;
    };
    DynamicArray<line_number_element> line_number_table; /* line_number_table[line_number_table_length] */

    virtual ~LineNumberTable_attribute() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "LineNumberTable_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
                cout << " line_number_table_length " << line_number_table.length() <<"\n";
                for (int i=0; i<line_number_table.length(); i++) {
                        cout << "     " << i << "  start_pc " << line_number_table[i].start_pc
                                        << "  line_number " << line_number_table[i].line_number << "\n";
                }
        }
#endif  
};


class LocalVariableTable_attribute : public attribute_info
{
public:
    LocalVariableTable_attribute(u2 &_name_index, u4 &_length)
                 : attribute_info(LocalVariableTable,_name_index,_length){}

//    u2 attribute_name_index;
//    u4 attribute_length;
     u2 local_variable_table_length() { return local_variable_table.length();}
    struct local_variable_element
    {
        u2 start_pc;
        u2 length;
        u2 name_index;
        u2 descriptor_index;
        u2 index;
    };
    DynamicArray<local_variable_element> local_variable_table; /* local_variable_table[local_variable_table_length] */

    virtual ~LocalVariableTable_attribute() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "LocalVariableTable_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
                cout << " local_variable_table_length " << local_variable_table.length() <<"\n";
                for (int i=0; i<local_variable_table.length(); i++) {
                        cout << "     " << i << "  start_pc " << local_variable_table[i].start_pc
                                 << "  length " << local_variable_table[i].length
                                 << "  name_index " << local_variable_table[i].name_index
                                 << "  descriptor_index " << local_variable_table[i].descriptor_index
                                 << "  index " << local_variable_table[i].index << "\n";
                }
        }
#endif  
};

class InnerClasses_attribute : public attribute_info
{
public:
    InnerClasses_attribute(u2 &_name_index, u4 &_length)
                 : attribute_info(InnerClasses,_name_index,_length){}

//    u2 attribute_name_index;
//    u4 attribute_length;
      u2 number_of_classes;
     u2 inner_classes_length() { return inner_classes.length();}
    struct local_variable_element
    {
        u2 inner_class_info_index;
        u2 outer_class_info_index;
        u2 inner_name_index;
        u2 inner_class_access_flags;
    };
    DynamicArray<local_variable_element> inner_classes; /* inner_classes[inner_classes_length] */

    virtual ~InnerClasses_attribute() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "InnerClasses_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
                cout << " inner_classes_length " << inner_classes.length() <<"\n";
                for (int i=0; i<inner_classes.length(); i++) {
                        cout << "     " << i << "  inner_class_info_index " << inner_classes[i].inner_class_info_index
                                 << "  outer_class_info_index " << inner_classes[i].outer_class_info_index
                                 << "  inner_name_index " << inner_classes[i].inner_name_index
                                 << "  inner_class_access_flags " << inner_classes[i].inner_class_access_flags
                                 << "\n";
                }
        }
#endif  
};

class Synthetic_attribute : public attribute_info
{
public:
    Synthetic_attribute(u2 &_name_index, u4 &_length)
                 : attribute_info(Synthetic,_name_index,_length){}

//    u2 attribute_name_index;
//    u4 attribute_length;

    virtual ~Synthetic_attribute() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "Synthetic_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
        }
#endif  
};

class Deprecated_attribute : public attribute_info
{
public:
    Deprecated_attribute(u2 &_name_index, u4 &_length)
                 : attribute_info(Deprecated, _name_index,_length){}

//    u2 attribute_name_index;
//    u4 attribute_length;

    virtual ~Deprecated_attribute() { }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "Deprecated_attribute attribute_name_index " << attribute_name_index
                         << " attribute_length " << attribute_length << "\n";
        }
#endif  
};

class field_info : public AccessFlags
{
public:
    /* u2 access_flags; */
    u2 name_index;
    u2 descriptor_index;
     u2 attributes_count() { return attributes.length();}
    DynamicArray<attribute_info *> attributes; /* attributes[attributes_count] */

    virtual ~field_info()
     {
         for (int i = 0; i < attributes.length(); i++)
             if (attributes[i])
                 delete attributes[i];
     }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "field_info  name_index " << name_index
                         << "  descriptor_index " << descriptor_index << "\n";
                AccessFlags::print();
                for (int i=0; i<attributes.length(); i++) {
                        attributes[i]->print(constant_pool);
                }
                cout << "\n";
        }
#endif  
};


class method_info : public AccessFlags
{
public:
    /* u2 access_flags; */
    u2 name_index;
    u2 descriptor_index;
     u2 attributes_count() { return attributes.length(); }
    DynamicArray<attribute_info *> attributes; /* attributes[attributes_count] */

        // shouldn't have to access ast to define class.h
        //      initialize_method_info(Ast_MethodDeclaration *p);
    method_info(u2 & _access_flags, u2 & _name_index, u2 & _descriptor_index)
                 : AccessFlags(_access_flags),
                                   name_index(_name_index),
                                   descriptor_index(_descriptor_index) {}

        method_info() : AccessFlags(), name_index(0), descriptor_index(0) {}
    virtual ~method_info()
     {
         for (int i = 0; i < attributes.length(); i++)
             if (attributes[i])
                 delete attributes[i];
     }
#ifdef TEST
        virtual void print(DynamicArray<cp_info *>& constant_pool) {
                cout << "method_info  name_index " << name_index
                         << "  descriptor_index " << descriptor_index << "\n";
                AccessFlags::print();
                for (int i=0; i<attributes.length(); i++) {
                        attributes[i]->print(constant_pool);
                }
                cout << "\n";
        }
#endif  
};




class ClassFile : public AccessFlags
{
private:
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

    u4 magic;
    u2 minor_version;
    u2 major_version;
    u2 constant_pool_count() { return constant_pool.length(); }
    DynamicArray<cp_info *> constant_pool; /* cp_info[constant_pool_count] */
/*    u2 access_flags; */
    u2 this_class;
    u2 super_class;
    u2 interfaces_count() { return interfaces.length(); }
    DynamicArray<u2> interfaces; /* interfaces[interfaces_count] */
    u2 fields_count() { return fields.length(); }
    DynamicArray<field_info> fields; /* fields[fields_count] */
    u2 methods_count() { return methods.length();}
    DynamicArray<method_info> methods; /* methods[methods_count] */
    u2 attributes_count() { return attributes.length(); }
    DynamicArray<attribute_info *> attributes; /* attributes[attributes_count] */

        virtual void reset(){           // reset to empty state.
                constant_pool.reset();
                fields.reset();
                methods.reset();
                attributes.reset();
                this_class = super_class = 0;
        }

public:
    ~ClassFile(){}; 
#ifdef TEST
    virtual void print();
    virtual void print_const(DynamicArray<cp_info *>& constant_pool,int i);
    virtual void print_const(int i);
#endif                          
};
#endif
