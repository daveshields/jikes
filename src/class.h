// $Id: class.h,v 1.22 2001/09/14 05:31:32 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef class_INCLUDED
#define class_INCLUDED

#include "platform.h"
#include "semantic.h"
#include "access.h"
#include "tuple.h"
#include "op.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


class cp_info
{
protected:
    u1 tag;

    //
    //    u1 info[]
    //
    // cp_info can be viewed as a generic (abstract, in Java talk) class that must
    // be "extended" to implement the real constant pool objects. The real objects
    // contain the info...
    //

public:

    cp_info(u1 _tag) : tag(_tag) {}
    virtual ~cp_info() {}

    u1 Tag() { return tag; }

    virtual void Put(OutputBuffer &output_buffer)
    {
         assert("trying to put unsupported attribute kind" == NULL);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *>& constant_pool)
    {
        Coutput << (int) tag;
    }

    virtual void Describe(Tuple<cp_info *>& constant_pool)
    {
        Coutput << (int) tag;
    }
#endif
};


class CONSTANT_Class_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 name_index;

public:

    CONSTANT_Class_info(u1 _tag, u2 _name_index) : cp_info(_tag),
                                                   name_index(_name_index)
    {}
    virtual ~CONSTANT_Class_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB2(name_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *>& constant_pool)
    {
        Coutput << "CONSTANT_Class_info: name_index " << (unsigned) name_index << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "Class:";  constant_pool[name_index] -> Describe(constant_pool);
    }
#endif
};


class CONSTANT_Double_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u4 high_bytes;
    u4 low_bytes;

public:

    CONSTANT_Double_info(u1 _tag, u4 _high_bytes, u4 _low_bytes) : cp_info(_tag),
                                                                   high_bytes(_high_bytes),
                                                                   low_bytes(_low_bytes)
    {}
    virtual ~CONSTANT_Double_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB4(high_bytes);
        output_buffer.PutB4(low_bytes);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_Double_info: bytes " << BaseLong(high_bytes, low_bytes).DoubleView() << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "D:";
    }
#endif
};


class CONSTANT_Fieldref_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 class_index;
    u2 name_and_type_index;

public:

    CONSTANT_Fieldref_info(u1 _tag, u2 _class_index, u2 _name_and_type_index) : cp_info(_tag),
                                                                                class_index(_class_index),
                                                                                name_and_type_index(_name_and_type_index)
    {}
    virtual ~CONSTANT_Fieldref_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
         output_buffer.PutB1(tag);
         output_buffer.PutB2(class_index);
         output_buffer.PutB2(name_and_type_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_Fieldref_info: class_index: "
                << (unsigned) class_index
                << ", name_and_type_index: "
                << (unsigned) name_and_type_index
                << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        constant_pool[class_index] -> Describe(constant_pool);
        Coutput << ".";
        constant_pool[name_and_type_index] -> Describe(constant_pool);
    }
#endif
};


class CONSTANT_Float_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u4 bytes;

public:

    CONSTANT_Float_info(u1 _tag, u4 _bytes) : cp_info(_tag),
                                              bytes(_bytes)
    {}
    virtual ~CONSTANT_Float_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB4(bytes);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_Float_info: bytes " << (float) bytes << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "F:";
    }
#endif
};


class CONSTANT_Integer_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u4 bytes;

public:

    CONSTANT_Integer_info(u1 _tag, u4 _bytes) : cp_info(_tag),
                                                bytes(_bytes)
    {}
    virtual ~CONSTANT_Integer_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB4(bytes);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        int val = ((bytes >> 24) & 0xff) << 24 | ((bytes >> 16) & 0xff) << 16 | ((bytes >> 8) & 0xff) << 8 | (bytes & 0xff);
        Coutput << "CONSTANT_Integer_info: bytes "
                << val
                << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "I:";
    }
#endif
};


class CONSTANT_InterfaceMethodref_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 class_index;
    u2 name_and_type_index;

public:

    CONSTANT_InterfaceMethodref_info(u1 _tag, u2 _class_index, u2 _name_and_type_index) : cp_info(_tag),
                                                                                          class_index(_class_index),
                                                                                          name_and_type_index(_name_and_type_index)
    {}
    virtual ~CONSTANT_InterfaceMethodref_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB2(class_index);
        output_buffer.PutB2(name_and_type_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_InterfaceMethodref_info: class_index: "
                << (unsigned) class_index
                << ", name_and_type_index: "
                << (unsigned) name_and_type_index
                << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        constant_pool[class_index] -> Describe(constant_pool);
        Coutput << ".";
        constant_pool[name_and_type_index] -> Describe(constant_pool);
    }
#endif
};


class CONSTANT_Long_info : public cp_info
{
    //
    // u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u4 high_bytes;
    u4 low_bytes;

public:

    CONSTANT_Long_info(u1 _tag, u4 _high_bytes, u4 _low_bytes) : cp_info(_tag),
                                                                 high_bytes(_high_bytes),
                                                                 low_bytes(_low_bytes)
    {}
    virtual ~CONSTANT_Long_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB4(high_bytes);
        output_buffer.PutB4(low_bytes);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_Long_info: bytes" << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "L:";
    }
#endif
};


class CONSTANT_Methodref_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 class_index;
    u2 name_and_type_index;

public:

    CONSTANT_Methodref_info(u1 _tag, u2 _class_index, u2 _name_and_type_index) : cp_info(_tag),
                                                                                 class_index(_class_index),
                                                                                 name_and_type_index(_name_and_type_index)
    {}
    virtual ~CONSTANT_Methodref_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB2(class_index);
        output_buffer.PutB2(name_and_type_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_Methodref_info: class_index: "
                << (unsigned) class_index
                << ", name_and_type_index: "
                << (unsigned) name_and_type_index
                << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        constant_pool[class_index] -> Describe(constant_pool);
        Coutput << ".";
        constant_pool[name_and_type_index] -> Describe(constant_pool);
    }
#endif
};


class CONSTANT_NameAndType_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 name_index;
    u2 descriptor_index;

public:

    CONSTANT_NameAndType_info(u1 _tag, u2 _name_index, u2 _descriptor_index) : cp_info(_tag),
                                                                               name_index(_name_index),
                                                                               descriptor_index(_descriptor_index)
    {}
    virtual ~CONSTANT_NameAndType_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB2(name_index);
        output_buffer.PutB2(descriptor_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_NameAndType_info: name_index: "
                << (unsigned) name_index
                << ", descriptor_index: "
                << (unsigned) descriptor_index
                << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        constant_pool[name_index] -> Describe(constant_pool);
        Coutput << " ";
        constant_pool[descriptor_index] -> Describe(constant_pool);
    }
#endif
};


class CONSTANT_String_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 string_index;

public:
    CONSTANT_String_info(u1 _tag, u2 _string_index) : cp_info(_tag),
                                                      string_index(_string_index)
    {}
    virtual ~CONSTANT_String_info() {}

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB2(string_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_String_info: string_index: "
                << (unsigned) string_index
                << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        constant_pool[string_index] -> Describe(constant_pool);
    }
#endif
};


class CONSTANT_Utf8_info : public cp_info
{
    //
    //    u1 tag;
    //
    // The tag is inherited from cp_info
    //
    u2 length_;
    char *bytes; /* bytes[length + 1] ... after input a '\0' will be added. */

public:

    CONSTANT_Utf8_info(u1 _tag, char *_bytes, int _length) : cp_info(_tag),
                                                             length_(_length)
    {
        bytes = new char[_length];
        for (int i = 0; i < _length; i++)
            bytes[i] = _bytes[i];

        return;
    }
    virtual ~CONSTANT_Utf8_info()
    {
        delete [] bytes;
    }

    u2 length() { return length_; }

    virtual void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB1(tag);
        output_buffer.PutB2(length());
        for (int i = 0; i < length(); i++)
            output_buffer.PutB1(bytes[i]);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "CONSTANT_Utf8_info: length: "
                << (unsigned) length_
                << " ";

        for (int i = 0; i < length_; i++)
            Coutput << (char) bytes[i];
        Coutput << endl;
    }

    virtual void Describe(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "\"";
        for (int i = 0; i < length_; i++)
            Coutput << (char) bytes[i];
        Coutput << "\"";
    }
#endif
};


//
// field_info and method_info should be defined here, but they contain attributes, so it is necessary
// to define the attributes first.
//

class attribute_info
{
protected:
    u1 tag; // this field was added in order to distinguish the attributes...

    u2 attribute_name_index;

    //
    //    u2 attribute_length;
    //    u1 info[attribute_length];
    //
    // attribute_info can be viewed as a generic (abstract, in Java talk) class that must
    // be "extended" to implement the real attribute objects. The real objects
    // contain the info...
    //

public:
    enum
    {
        Generic,
        Code,
        ConstantValue,
        Deprecated,
        Exceptions,
        InnerClasses,
        LineNumberTable,
        LocalVariableTable,
        SourceFile,
        Synthetic
    };

    attribute_info(u1 _tag, u2 _name_index) : tag(_tag),
                                              attribute_name_index(_name_index)
    {}
    virtual ~attribute_info() {};

    u1 Tag() { return tag; }
    u2 AttributeNameIndex() { return attribute_name_index; }

    virtual u4 AttributeLength() { assert(false); return 0; } // abstract method: should not be invoked.

    virtual void Put(OutputBuffer &output_buffer) { assert(false); } // abstract method: should not be invoked.

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "print for attribute info tag "
                << (unsigned) tag
                << " not defined"
                << endl;
        Coutput.flush();
        assert(false); // abstract method: should not be invoked.
    }
#endif
};


class Code_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The attribut_name_index is inherited from attribute_info
    //
    u4 attribute_length;
    u2 max_stack;
    u2 max_locals;

    //
    //    u4 code_length;
    //
    // code_length can be computed as code.Length();
    //
    Tuple<u1> code; /* code[code_length] */

    //
    //    u2 exception_table_length;
    //
    // exception_table_length can be computed as exception_table.Length();
    //
    struct ExceptionElement
    {
        u2 start_pc;
        u2 end_pc;
        u2 handler_pc;
        u2 catch_type;
    };
    Tuple<ExceptionElement> exception_table; /* exceptiontable[exception_table_length] */

    //
    //    u2 attribute_count;
    //
    // attribute_count can be computed as attributes.Length();
    //
    Tuple<attribute_info *> attributes; /* attributes[attributes_count] */

public:

    Code_attribute(u2 _name_index, u2 _max_locals) : attribute_info(Code, _name_index),
                                                     attribute_length(0),
                                                     max_stack(0),
                                                     max_locals(_max_locals),
                                                     code(8, 4),
                                                     exception_table(6, 16),
                                                     attributes(6, 16)
    {}
    virtual ~Code_attribute()
    {
        for (int i = 0; i < attributes.Length(); i++)
            delete attributes[i];
    }

    virtual u4 AttributeLength()
    {
        if (attribute_length == 0) // if not yet computed...
        {
            attribute_length = + 2                            // for max_stack
                               + 2                            // for max_locals
                               + 4                            // for code_length
                               + code.Length()                // for code
                               + 2                            // for exception_table_length
                               + exception_table.Length() * 8 // for exception table
                               + 2;                           // for attributes_count
            //
            // std. fields of attribute_info
            //
            for (int i = 0; i < attributes.Length(); i++)
            {
                if (attributes[i] -> AttributeLength() > 0)
                    attribute_length += (attributes[i] -> AttributeLength() + 6);
            }
        }

        return attribute_length;
    }

    u2 MaxStack() { return max_stack; }
    void SetMaxStack(u2 val) { max_stack = val; }

    u2 MaxLocals() { return max_locals; }
    void ResetMaxLocals(u2 val) { max_locals = val; }

    u2 CodeLength() { return code.Length(); }

    void ResetCode(int i, u1 byte)
    {
        code[i] = byte;
    }

    void AddCode(u1 byte)
    {
        code.Next() = byte;
    }

    u2 ExceptionTableLength() { return exception_table.Length(); }

    void AddException(u2 start_pc, u2 end_pc, u2 handler_pc, u2 catch_type)
    {
        int exception_index = exception_table.NextIndex();

        exception_table[exception_index].start_pc = start_pc;
        exception_table[exception_index].end_pc = end_pc;
        exception_table[exception_index].handler_pc = handler_pc;
        exception_table[exception_index].catch_type = catch_type;
    }

    u2 AttributesCount() { return attributes.Length(); }

    void AddAttribute(attribute_info *attribute)
    {
        attributes.Next() = attribute;
    }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(max_stack);
        output_buffer.PutB2(max_locals);
        output_buffer.PutB4(code.Length());

        for (int i = 0; i < code.Length(); i++)
            output_buffer.PutB1(code[i]);
        output_buffer.PutB2(exception_table.Length());

        for (int j = 0; j < exception_table.Length(); j++)
        {
            output_buffer.PutB2(exception_table[j].start_pc);
            output_buffer.PutB2(exception_table[j].end_pc);
            output_buffer.PutB2(exception_table[j].handler_pc);
            output_buffer.PutB2(exception_table[j].catch_type);
        }

        output_buffer.PutB2(attributes.Length());
        for (int k = 0; k < attributes.Length(); k++)
            attributes[k] -> Put(output_buffer);

        return;
    }

#ifdef JIKES_DEBUG
    virtual void  Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "Code_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " attribute_length "
                << (unsigned) attribute_length
                << endl
                << " max_stack "
                << (unsigned) max_stack
                << " max_locals "
                << (unsigned) max_locals
                << " code_length "
                << code.Length()
                << endl;

        if (exception_table.Length())
        {
            Coutput << " exception_table: " << exception_table.Length()
                    << " entries" << endl;
            for (int i  = 0; i < exception_table.Length(); i++)
            {
                Coutput << "  start_pc "
                        << (unsigned) exception_table[i].start_pc
                        << "  end_pc "
                        << (unsigned) exception_table[i].end_pc
                        << "  handler_pc "
                        << (unsigned) exception_table[i].handler_pc
                        << "  catch_type "
                        << (unsigned) exception_table[i].catch_type
                        << endl;
            }
        }

        Operators::opdmp(constant_pool, code);

        Coutput << "  " << endl;

        for (int i = 0; i < attributes.Length(); i++)
            attributes[i] -> Print(constant_pool);
    }
#endif
};


class ConstantValue_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The field attribute_name_index is inherited from attribute_info
    //
    //    u4 attribute_length;
    //
    // The value of attribute_length is always 2
    //
    u2 constantvalue_index;

public:

    ConstantValue_attribute(u2 _name_index, u2 _constantvalue_index) : attribute_info(ConstantValue, _name_index),
                                                                       constantvalue_index(_constantvalue_index)
    {}
    virtual ~ConstantValue_attribute() {}

    virtual u4 AttributeLength() { return 2; }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(constantvalue_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "ConstantValue_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " attribute_length "
                << AttributeLength()
                << " constantvalue_index "
                << (unsigned) constantvalue_index
                << endl;
    }
#endif
};


class Exceptions_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index is inherited from attribute_info
    //
    //    u4 attribute_length;
    //
    // The value of attribute_length is derived from the exception_index_table. See below
    //
    //    u2 number_of_exceptions;
    //
    // The value of number_of_exceptions is derived from the exception_index_table. See below
    //

    Tuple<u2> exception_index_table; /* exception_index_table[number_of_exceptions] */

public:

    Exceptions_attribute(u2 _name_index) : attribute_info(Exceptions, _name_index)
    {}
    virtual ~Exceptions_attribute() {}

    virtual u4 AttributeLength()
    {
        return exception_index_table.Length() * 2 + 2;
    }

    u2 NumberOfExceptions() { return exception_index_table.Length(); }

    void AddExceptionIndex(u2 index)
    {
        exception_index_table.Next() = index;
    }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(exception_index_table.Length());
        for (int i = 0; i < exception_index_table.Length(); i++)
            output_buffer.PutB2(exception_index_table[i]);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "Exceptions_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " attribute_length "
                << AttributeLength()
                << endl;

        for (int i = 0; i < exception_index_table.Length(); i++)
            Coutput << "    "
                    << (unsigned) exception_index_table[i];
        Coutput << endl;
    }
#endif
};


class InnerClasses_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index is inherited from attribute_info
    //
    //    u4 attribute_length;
    //
    // The value of attribute_length is derived from inner_classes. See below
    //
    //    u2 number_of_classes;
    //
    // The value of number_of_classes is derived from inner_classes. See below
    //

    struct inner_classes_element
    {
        u2 inner_class_info_index;
        u2 outer_class_info_index;
        u2 inner_name_index;
        u2 inner_class_access_flags;
    };
    Tuple<inner_classes_element> inner_classes; /* inner_classes_table[inner_classes_table_length] */

public:

    InnerClasses_attribute(u2 _name_index) : attribute_info(InnerClasses, _name_index),
                                             inner_classes(6, 16)
    {}
    virtual ~InnerClasses_attribute() {}

    virtual u4 AttributeLength()
    {
        return inner_classes.Length() * 8 + 2;
    }

    u2 InnerClassesLength() { return inner_classes.Length();}

    void AddInnerClass(u2 inner_class_info_index, u2 outer_class_info_index, u2 inner_name_index, u2 inner_class_access_flags)
    {
        int index = inner_classes.NextIndex();

        inner_classes[index].inner_class_info_index = inner_class_info_index;
        inner_classes[index].outer_class_info_index = outer_class_info_index;
        inner_classes[index].inner_name_index = inner_name_index;
        inner_classes[index].inner_class_access_flags = inner_class_access_flags;
    }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(inner_classes.Length());
        for (int i = 0; i < inner_classes.Length(); i++)
        {
            output_buffer.PutB2(inner_classes[i].inner_class_info_index);
            output_buffer.PutB2(inner_classes[i].outer_class_info_index);
            output_buffer.PutB2(inner_classes[i].inner_name_index);
            output_buffer.PutB2(inner_classes[i].inner_class_access_flags);
        }
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "InnerClasses_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " attribute_length "
                << AttributeLength()
                << endl
                << " inner_classes_length "
                << inner_classes.Length()
                << endl;

        for (int i = 0; i < inner_classes.Length(); i++)
        {
            Coutput << "     "
                    << i
                    << "  inner_class_info_index "
                    << (unsigned) inner_classes[i].inner_class_info_index
                    << "  outer_class_info_index "
                    << (unsigned) inner_classes[i].outer_class_info_index
                    << "  inner_name_index "
                    << (unsigned) inner_classes[i].inner_name_index
                    << "  inner_class_access_flags "
                    << (unsigned) inner_classes[i].inner_class_access_flags
                    << endl;
        }
    }
#endif
};


class LineNumberTable_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index is inherited from attribute_info
    //
    //    u4 attribute_length;
    //
    // The value of attribute_length is derived from line_number_table. See below
    //
    //    u2 line_number_table_length;
    //
    // The value of line_number_table_length is derived from line_number_table. See below
    //

    struct line_number_element
    {
        u2 start_pc;
        u2 line_number;
    };
    Tuple<line_number_element> line_number_table; /* line_number_table[line_number_table_length] */

public:

    LineNumberTable_attribute(u2 _name_index) : attribute_info(LineNumberTable, _name_index),
                                                line_number_table(6, 16)
    {}
    virtual ~LineNumberTable_attribute() {}

    virtual u4 AttributeLength()
    {
        return line_number_table.Length() * 4 + 2;
    }

    u2 LineNumberTableLength()
    {
        return line_number_table.Length();
    }

    void AddLineNumber(u2 start_pc, u2 line_number)
    {
        int line_number_index = line_number_table.NextIndex();

        line_number_table[line_number_index].start_pc = start_pc;
        line_number_table[line_number_index].line_number = line_number;
    }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(line_number_table.Length());
        for (int i = 0; i < line_number_table.Length(); i++)
        {
            output_buffer.PutB2(line_number_table[i].start_pc);
            output_buffer.PutB2(line_number_table[i].line_number);
        }
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
     {
        Coutput << "LineNumberTable_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " attribute_length "
                << AttributeLength()
                << endl
                << " line_number_table_length "
                << line_number_table.Length()
                << endl;

        for (int i = 0; i < line_number_table.Length(); i++)
        {
            Coutput << "     "
                    << i
                    << "  start_pc "
                    << (unsigned) line_number_table[i].start_pc
                    << "  line_number "
                    << (unsigned) line_number_table[i].line_number
                    << endl;
        }
    }
#endif
};


class LocalVariableTable_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index is inherited from attribute_info
    //
    //    u4 attribute_length;
    //
    // The value of attribute_length is derived from local_variable_table. See below
    //
    //    u2 local_variable_length;
    //
    // The value of local_variable_length is derived from local_variable_length. See below
    //

    struct local_variable_element
    {
        u2 start_pc;
        u2 length;
        u2 name_index;
        u2 descriptor_index;
        u2 index;
    };
    Tuple<local_variable_element> local_variable_table; /* local_variable_table[local_variable_table_length] */

public:

    LocalVariableTable_attribute(u2 _name_index) : attribute_info(LocalVariableTable, _name_index)
    {}
    virtual ~LocalVariableTable_attribute() {}

    virtual u4 AttributeLength()
    {
        return local_variable_table.Length() * 10 + 2;
    }

    u2 LocalVariableTableLength() { return local_variable_table.Length(); }

    //
    // make entry in local variable table
    //
    void AddLocalVariable(u2 start, u2 end, u2 name, u2 descriptor, u2 index)
    {
        assert(end >= start);

        if (end > start)
        {
            int local_index = local_variable_table.NextIndex();

            local_variable_table[local_index].start_pc = start;
            local_variable_table[local_index].length = end - start;
            local_variable_table[local_index].name_index = name;
            local_variable_table[local_index].descriptor_index = descriptor;
            local_variable_table[local_index].index = index;
        }
else 
end = end;

        return;
    }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(local_variable_table.Length());
        for (int i = 0; i < local_variable_table.Length(); i++)
        {
            output_buffer.PutB2(local_variable_table[i].start_pc);
            output_buffer.PutB2(local_variable_table[i].length);
            output_buffer.PutB2(local_variable_table[i].name_index);
            output_buffer.PutB2(local_variable_table[i].descriptor_index);
            output_buffer.PutB2(local_variable_table[i].index);
        }
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "LocalVariableTable_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " attribute_length "
                << AttributeLength()
                << endl
                << " local_variable_table_length "
                << local_variable_table.Length()
                << endl;

        for (int i = 0; i < local_variable_table.Length(); i++)
        {
            Coutput << "     "
                    << i
                    << "  start_pc "
                    << (unsigned) local_variable_table[i].start_pc
                    << "  length "
                    << (unsigned) local_variable_table[i].length
                    << "  name_index "
                    << (unsigned) local_variable_table[i].name_index
                    << "  descriptor_index "
                    << (unsigned) local_variable_table[i].descriptor_index
                    << "  index "
                    << (unsigned) local_variable_table[i].index
                    << endl;
        }
    }
#endif
};


class SourceFile_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index is inherited from attribute_info
    //
    // The attribute_length is always 2.
    //
    u2 sourcefile_index;

public:

    SourceFile_attribute(u2 _name_index, u2 _sourcefile_index) : attribute_info(SourceFile, _name_index),
                                                                 sourcefile_index(_sourcefile_index)
    {}
    virtual ~SourceFile_attribute() {}

    virtual u4 AttributeLength() { return 2; }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
        output_buffer.PutB2(sourcefile_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "SourceFile_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " length "
                << AttributeLength()
                << " sourcefile_index "
                << (unsigned) sourcefile_index
                << endl;
    }
#endif
};


class Synthetic_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index and attribute_length are inherited from attribute_info
    //
    // The attribute_length is always 0.
    //

public:

    Synthetic_attribute(u2 _name_index) : attribute_info(Synthetic, _name_index)
    {}
    virtual ~Synthetic_attribute() {}

    virtual u4 AttributeLength() { return 0; }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "Synthetic_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " length "
                << AttributeLength()
                << endl;
    }
#endif
};


class Deprecated_attribute : public attribute_info
{
    //
    //    u2 attribute_name_index;
    //
    // The fields attribute_name_index and attribute_length are inherited from attribute_info
    //
    // The attribute_length is always 0.
    //

public:

    Deprecated_attribute(u2 _name_index) : attribute_info(Deprecated, _name_index)
    {}
    virtual ~Deprecated_attribute() {}

    virtual u4 AttributeLength() { return 0; }

    virtual void Put(OutputBuffer &output_buffer)
    {
        assert(attribute_name_index != 0);

        output_buffer.PutB2(attribute_name_index);
        output_buffer.PutB4(AttributeLength());
    }

#ifdef JIKES_DEBUG
    virtual void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "Deprecated_attribute attribute_name_index "
                << (unsigned) attribute_name_index
                << " length "
                << AttributeLength()
                << endl;
    }
#endif
};


class field_info : public AccessFlags
{
    //
    // u2 access_flags;
    //
    // The access_flags is inherited from AccessFlags
    //
    u2 name_index;
    u2 descriptor_index;

    //
    // attributes_count can be computed as attributes.Length();
    //
    Tuple<attribute_info *> attributes; /* attributes[attributes_count] */

public:

     ~field_info()
     {
         for (int i = 0; i < attributes.Length(); i++)
             delete attributes[i];
     }

    inline void SetNameIndex(u2 _name_index) { name_index = _name_index; }
    inline void SetDescriptorIndex(u2 _descriptor_index) { descriptor_index = _descriptor_index; }

    inline u2 AttributesCount() { return attributes.Length(); }
    inline void AddAttribute(attribute_info *attribute) { attributes.Next() = attribute; }

    inline void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB2(access_flags);
        output_buffer.PutB2(name_index);
        output_buffer.PutB2(descriptor_index);
        output_buffer.PutB2(attributes.Length());

        for (int ai = 0; ai < attributes.Length(); ai++)
            attributes[ai] -> Put(output_buffer);
    }

#ifdef JIKES_DEBUG
    void Print(Tuple<cp_info *> &constant_pool)
    {
        Coutput << "field_info  name_index "
                << (unsigned) name_index
                << "  descriptor_index "
                << (unsigned) descriptor_index
                << endl;

        AccessFlags::Print();

        for (int i = 0; i < attributes.Length(); i++)
            attributes[i] -> Print(constant_pool);
        Coutput << endl;
    }
#endif
};


class method_info : public AccessFlags
{
    //
    // u2 access_flags;
    //
    // The access_flags is inherited from AccessFlags
    //
    u2 name_index;
    u2 descriptor_index;

    //
    // attributes_count can be computed as attributes.Length();
    //
    Tuple<attribute_info *> attributes; /* attributes[attributes_count] */

public:

    ~method_info()
    {
        for (int i = 0; i < attributes.Length(); i++)
            delete attributes[i];
    }

    inline void SetNameIndex(u2 _name_index) { name_index = _name_index; }
    inline void SetDescriptorIndex(u2 _descriptor_index) { descriptor_index = _descriptor_index; }

    inline u2 AttributesCount() { return attributes.Length(); }
    inline void AddAttribute(attribute_info *attribute) { attributes.Next() = attribute; }

    inline void Put(OutputBuffer &output_buffer)
    {
        output_buffer.PutB2(access_flags);
        output_buffer.PutB2(name_index);
        output_buffer.PutB2(descriptor_index);
        output_buffer.PutB2(attributes.Length());

        for (int i = 0; i < attributes.Length(); i++)
            attributes[i] -> Put(output_buffer);
    }

#ifdef JIKES_DEBUG
     void Print(Tuple<cp_info *> &constant_pool)
     {
        Coutput << "method_info  name_index "
                << (unsigned) name_index
                << "  descriptor_index "
                << (unsigned) descriptor_index
                << endl;

        AccessFlags::Print();

        for (int i = 0; i < attributes.Length(); i++)
            attributes[i] -> Print(constant_pool);
        Coutput << endl;
    }
#endif
};


class ClassFile : public AccessFlags
{
public:
    enum ConstantKind
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
    u2 ConstantPoolCount() { return constant_pool.Length(); }
    Tuple<cp_info *> constant_pool; /* cp_info[constant_pool_count] */
    //
    // u2 access_flags;
    //
    // The access_flags is inherited from AccessFlags
    //
    u2 this_class;
    u2 super_class;
    u2 InterfacesCount() { return interfaces.Length(); }
    Tuple<u2> interfaces; /* interfaces[interfaces_count] */
    u2 FieldsCount() { return fields.Length(); }
    Tuple<field_info> fields; /* fields[fields_count] */
    u2 MethodsCount() { return methods.Length();}
    Tuple<method_info> methods; /* methods[methods_count] */
    u2 AttributesCount() { return attributes.Length(); }
    Tuple<attribute_info *> attributes; /* attributes[attributes_count] */

    ClassFile(TypeSymbol *unit_type_) : constant_pool(8, 4),
                                        fields(6, 16),
                                        methods(6, 16),
                                        unit_type(unit_type_)
    {}

    ~ClassFile()
    {
        for (int i = 1; i < constant_pool.Length(); i++)
            delete constant_pool[i];

        for (int j = 0; j < attributes.Length(); j++)
            delete attributes[j];

        return;
    }

    void Write()
    {
        Semantic *sem = unit_type -> semantic_environment -> sem;
        Control &control = sem -> control;

        if (! control.option.nowrite)
        {
            output_buffer.PutB4(magic);
            output_buffer.PutB2(minor_version);
            output_buffer.PutB2(major_version);

            //
            // write constant pool
            //
            output_buffer.PutB2(constant_pool.Length());
            for (int i = 1; i < constant_pool.Length(); i++)
            {
                assert(constant_pool[i]);

                constant_pool[i] -> Put(output_buffer);
                if (constant_pool[i] -> Tag()  ==  CONSTANT_Long || constant_pool[i] -> Tag()  ==  CONSTANT_Double)
                    i++;; // skip the next entry for eight-byte constants
            }

            output_buffer.PutB2(access_flags);
            output_buffer.PutB2(this_class);
            output_buffer.PutB2(super_class);

            //
            // write interfaces
            //
            output_buffer.PutB2(interfaces.Length());
            for (int j = 0; j < interfaces.Length(); j++)
                output_buffer.PutB2(interfaces[j]);

            //
            // write field members
            //
            output_buffer.PutB2(fields.Length());
            for (int k = 0; k < fields.Length(); k++)
                fields[k].Put(output_buffer);

            //
            // write method members
            //
            output_buffer.PutB2(methods.Length());
            for (int l = 0; l < methods.Length(); l++)
                methods[l].Put(output_buffer);

            //
            // write attributes
            //
            output_buffer.PutB2(attributes.Length());
            for (int m = 0; m < attributes.Length(); m++)
                attributes[m] -> Put(output_buffer);

            char *class_file_name = unit_type -> ClassName();
            if (control.option.verbose)
            {
                Coutput << "[write "
                        << class_file_name
                        << "]" << endl;
            }

            if (! output_buffer.WriteToFile(class_file_name))
            {
                int length = strlen(class_file_name);
                wchar_t *name = new wchar_t[length + 1];
                for (int i = 0; i < length; i++)
                    name[i] = class_file_name[i];
                name[length] = U_NULL;

                sem -> ReportSemError(SemanticError::CANNOT_WRITE_FILE,
                                     unit_type -> declaration -> LeftToken(),
                                     unit_type -> declaration -> RightToken(),
                                     name);
                delete [] name;
            }
        }

        return;
    }

protected:
    TypeSymbol *unit_type;
    OutputBuffer output_buffer;
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // class_INCLUDED

