// $Id: class.h,v 1.37 2004/01/20 04:10:22 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef class_INCLUDED
#define class_INCLUDED

#include "platform.h"
#include "access.h"
#include "tuple.h"
#include "long.h"
#include "double.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

//
// This header defines the .class file format, for both reading in and writing
// out valid classfiles.
//
class ClassFile;
class CPInfo;
class CPUtf8Info;
class TypeSymbol;


//
// Specialization of Tuple to more gracefully handle invalid indices. This
// is because slot 0 and slots following a long or double entry are invalid.
//
class ConstantPool : private Tuple<CPInfo*>
{
    static CPInfo* invalid;
    class CPInvalid;

public:
    ConstantPool(unsigned estimate = 0)
        : Tuple<CPInfo*>(estimate)
    {
        Next() = NULL; // The 0th entry is unused.
    }
    ConstantPool(unsigned log_blksize, unsigned base_increment)
        : Tuple<CPInfo*>(log_blksize, base_increment)
    {
        Next() = NULL; // The 0th entry is unused.
    }
    ~ConstantPool() {}

    // Inherit only the methods we know are safe.
    using Tuple<CPInfo*>::Length;
    using Tuple<CPInfo*>::SpaceAllocated;
    using Tuple<CPInfo*>::SpaceUsed;

    //
    // Note that unlike in Tuple<>, ConstantPool[] does not return an lvalue;
    // use SetNext(CPInfo*) instead.
    //
    inline const CPInfo* operator[](const unsigned i) const
    {
        if (i >= top)
            return invalid;
        const CPInfo* result = Tuple<CPInfo*>::operator[](i);
        return result ? result : invalid;
    }

    inline void Reset() { Tuple<CPInfo*>::Reset(); }

    void SetNext(CPInfo*);

    inline bool Valid(const unsigned i)
    {
        return i < top && Tuple<CPInfo*>::operator[](i);
    }
    bool Check() const;
};


//
// The prefix CP stands for Constant Pool. This is the base class of all
// constant pool information; subclasses give more detail. JVMS 4.4.
//
class CPInfo
{
protected:
    const u1 tag;

public:
    enum ConstantPoolTag
    {
        CONSTANT_Class = 7,
        CONSTANT_Fieldref = 9,
        CONSTANT_Methodref = 10,
        CONSTANT_InterfaceMethodref = 11,
        CONSTANT_String = 8,
        CONSTANT_Integer = 3,
        CONSTANT_Float = 4,
        CONSTANT_Long = 5,
        CONSTANT_Double = 6,
        CONSTANT_NameAndType = 12,
        CONSTANT_Utf8 = 1
    };

    CPInfo(u1 _tag) : tag(_tag) {}
    virtual ~CPInfo() {}

    u1 Tag() const { return tag; }
    bool Large() const
    {
        return tag == CONSTANT_Long || tag == CONSTANT_Double;
    }
    bool Constant() const
    {
        return ((tag >= CPInfo::CONSTANT_Integer &&
                 tag <= CPInfo::CONSTANT_Double) ||
                tag == CPInfo::CONSTANT_String);
    }

    //
    // This reads the next constant pool entry from buffer, advancing it to
    // the end of the entry. The user is responsible for deleting the result.
    //
    static CPInfo* AllocateCPInfo(ClassFile&);

    virtual void Put(OutputBuffer&) const
    {
        assert(false && "trying to put unsupported attribute kind");
    }

    //
    // Allows subclasses to perform data validation, once the entire constant
    // pool has been read. Return false on failure.
    //
    virtual bool Check(const ConstantPool&) const { return true; }

#ifdef JIKES_DEBUG
    //
    // Prints all information about the constant value, ending the line.
    //
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "unknown tag: " << (unsigned) tag << endl;
    }

    //
    // Prints shortened information, no newline.
    //
    virtual void Describe(const ConstantPool&) const
    {
        Coutput << "unknown tag: " << (unsigned) tag;
    }
#endif // JIKES_DEBUG
};


//
// Describes a UTF8 sequence. JVMS 4.4.7.
//
class CPUtf8Info : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const u2 len;
    u1* bytes; // bytes[length]

    //
    // Flattened to null-terminated char sequence, for debug output. Also
    // used by attribute decoding, since all recognized attributes are ASCII.
    //
    ConvertibleArray<char> contents;
    bool valid;

public:
    CPUtf8Info(const char* _bytes, unsigned length)
        : CPInfo(CONSTANT_Utf8)
        , len(length)
        , contents(length)
    {
        bytes = new u1[len ? len : 1];
        for (unsigned i = 0; i < len; i++)
            bytes[i] = (u1) _bytes[i];
        Init();
        assert(valid);
    }
    CPUtf8Info(ClassFile&);
    virtual ~CPUtf8Info()
    {
        delete [] bytes;
    }

    //
    // We guarantee that Bytes() is null-terminated and does not contain
    // intermediate null characters. Furthermore, if the constant pool entry
    // contains only printing ASCII (\u0020 through \u007f), with the
    // exception of '"' or '\\', then strlen(Bytes()) == Length(). Otherwise,
    // Bytes() is a stylized representation of the entry, which parses by
    // the same rules as a Java String literal using escape characters.
    //
    const char* Bytes() const { return contents.Array(); }
    u2 Length() const { return len; }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU2(len);
        output_buffer.PutN(bytes, len);
    }

    virtual bool Check(const ConstantPool&) const { return valid; }

private:
    void Init();

#ifdef JIKES_DEBUG
public:
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "Utf8: length: " << (unsigned) len
                << ", bytes[]: " << contents.Array() << endl;
    }

    virtual void Describe(const ConstantPool&) const
    {
        Coutput << contents.Array();
    }
#endif // JIKES_DEBUG
};


//
// Describes a class or interface name. JVMS 4.4.1.
//
class CPClassInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const u2 name_index;

    TypeSymbol* type; // the associated type

public:
    CPClassInfo(u2 _name_index)
        : CPInfo(CONSTANT_Class)
        , name_index(_name_index)
        , type(NULL)
    {}
    CPClassInfo(ClassFile&);
    virtual ~CPClassInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU2(name_index);
    }

    virtual bool Check(const ConstantPool& constant_pool) const
    {
        return constant_pool[name_index] -> Tag() == CONSTANT_Utf8;
    }

    const char* TypeName(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[name_index] -> Tag() == CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[name_index]) -> Bytes();
    }
    u2 TypeNameLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[name_index] -> Tag() == CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[name_index]) -> Length();
    }

    void SetType(TypeSymbol* t)
    {
        assert(! type);
        type = t;
    }
    TypeSymbol* Type() { return type; }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool) const
    {
        Coutput << "Class: name: " << (unsigned) name_index;
        if (constant_pool[name_index] -> Tag() == CONSTANT_Utf8)
        {
            Coutput << "=\"";
            constant_pool[name_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
        Coutput << endl;
    }

    virtual void Describe(const ConstantPool& constant_pool) const
    {
        if (constant_pool[name_index] -> Tag() == CONSTANT_Utf8)
            constant_pool[name_index] -> Describe(constant_pool);
        else Coutput << "(invalid)";
    }
#endif // JIKES_DEBUG
};


//
// Describes a field or method's declaring class, name, and type. JVMS 4.4.2.
//
class CPMemberInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const u2 class_index;
    const u2 name_and_type_index;

public:
    CPMemberInfo(u1 _tag, u2 _class_index, u2 _name_and_type_index)
        : CPInfo(_tag)
        , class_index(_class_index)
        , name_and_type_index(_name_and_type_index)
    {}
    CPMemberInfo(u1, ClassFile&);
    virtual ~CPMemberInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU2(class_index);
        output_buffer.PutU2(name_and_type_index);
    }

    virtual bool Check(const ConstantPool& constant_pool) const
    {
        return constant_pool[class_index] -> Tag() == CONSTANT_Class &&
            (constant_pool[name_and_type_index] -> Tag() ==
             CONSTANT_NameAndType);
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool) const
    {
        switch (tag)
        {
        case CONSTANT_Fieldref:
            Coutput << "Field";
            break;
        case CONSTANT_InterfaceMethodref:
            Coutput << "Interface";
            // fallthrough
        case CONSTANT_Methodref:
            Coutput << "Method";
            break;
        default:
            assert(false && "bad constant pool member tag");
        }
        Coutput << ": class: " << (unsigned) class_index;
        if (constant_pool[class_index] -> Tag() == CONSTANT_Class)
        {
            Coutput << '=';
            constant_pool[class_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << endl << "        name_and_type: "
                << (unsigned) name_and_type_index;
        if (constant_pool[name_and_type_index] -> Tag() ==
            CONSTANT_NameAndType)
        {
            Coutput << "=<";
            constant_pool[name_and_type_index] -> Describe(constant_pool);
            Coutput << ">";
        }
        else Coutput << "(invalid)";
        Coutput << endl;
    }

    virtual void Describe(const ConstantPool& constant_pool) const
    {
        if (constant_pool[class_index] -> Tag() == CONSTANT_Class)
            constant_pool[class_index] -> Describe(constant_pool);
        else Coutput << "(invalid)";
        Coutput << ".";
        if (constant_pool[name_and_type_index] -> Tag() ==
            CONSTANT_NameAndType)
        {
            constant_pool[name_and_type_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
    }
#endif // JIKES_DEBUG
};


//
// Describes a String literal. JVMS 4.4.3.
//
class CPStringInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const u2 string_index;

public:
    CPStringInfo(u2 _string_index)
        : CPInfo(CONSTANT_String)
        , string_index(_string_index)
    {}
    CPStringInfo(ClassFile&);
    virtual ~CPStringInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU2(string_index);
    }

    virtual bool Check(const ConstantPool& constant_pool) const
    {
        return constant_pool[string_index] -> Tag() == CONSTANT_Utf8;
    }

    const char* Bytes(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[string_index] -> Tag() == CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[string_index]) -> Bytes();
    }
    u2 Length(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[string_index] -> Tag() == CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[string_index]) -> Length();
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool) const
    {
        Coutput << "String: string: " << (unsigned) string_index;
        if (constant_pool[string_index] -> Tag() == CONSTANT_Utf8)
        {
            Coutput << "=\"";
            constant_pool[string_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
        Coutput << endl;
    }

    virtual void Describe(const ConstantPool& constant_pool) const
    {
        if (constant_pool[string_index] -> Tag() == CONSTANT_Utf8)
        {
            Coutput << '"';
            constant_pool[string_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
    }
#endif // JIKES_DEBUG
};


//
// Describes an int literal. JVMS 4.4.4.
//
class CPIntegerInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const u4 bytes;

public:
    CPIntegerInfo(u4 _bytes)
        : CPInfo(CONSTANT_Integer)
        , bytes(_bytes)
    {}
    CPIntegerInfo(ClassFile&);
    virtual ~CPIntegerInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU4(bytes);
    }

    i4 Value() const { return (i4) bytes; }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "Integer: bytes: 0x"
                << IntToString(bytes, 8).String() << " (" << (int) bytes
                << ')' << endl;
    }

    virtual void Describe(const ConstantPool&) const
    {
        Coutput << (int) bytes;
    }
#endif // JIKES_DEBUG
};


//
// Describes a float literal. JVMS 4.4.4.
//
class CPFloatInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const IEEEfloat value; // u4 bytes

public:
    CPFloatInfo(u4 bytes)
        : CPInfo(CONSTANT_Float)
        , value(bytes)
    {}
    CPFloatInfo(ClassFile&);
    virtual ~CPFloatInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU4(value.Word());
    }

    const IEEEfloat& Value() const { return value; }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "Float: bytes: 0x"
                << IntToString(value.Word(), 8).String() << " ("
                << FloatToString(value).String() << "F)" << endl;
    }

    virtual void Describe(const ConstantPool&) const
    {
        Coutput << FloatToString(value).String() << 'F';
    }
#endif // JIKES_DEBUG
};


//
// Describes a long literal. JVMS 4.4.5.
//
class CPLongInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const LongInt value; // u4 high_bytes, low_bytes;

public:
    CPLongInfo(u4 high_bytes, u4 low_bytes)
        : CPInfo(CONSTANT_Long)
        , value(high_bytes, low_bytes)
    {}
    CPLongInfo(ClassFile&);
    virtual ~CPLongInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU4(value.HighWord());
        output_buffer.PutU4(value.LowWord());
    }

    const LongInt& Value() const { return value; }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "Long: high: 0x"
                << IntToString(value.HighWord(), 8).String()
                << ", low: 0x" << IntToString(value.LowWord(), 8).String()
                << " (" << LongToString(value).String() << "L)" << endl;
    }

    virtual void Describe(const ConstantPool&) const
    {
        Coutput << LongToString(value).String() << 'L';
    }
#endif // JIKES_DEBUG
};


//
// Describes a double literal. JVMS 4.4.5.
//
class CPDoubleInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const IEEEdouble value; // u4 high_bytes, low_bytes;

public:
    CPDoubleInfo(u4 high_bytes, u4 low_bytes)
        : CPInfo(CONSTANT_Double)
        , value(high_bytes, low_bytes)
    {}
    CPDoubleInfo(ClassFile&);
    virtual ~CPDoubleInfo() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU4(value.HighWord());
        output_buffer.PutU4(value.LowWord());
    }

    const IEEEdouble& Value() const { return value; }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "Double: high: 0x"
                << IntToString(value.HighWord(), 8).String()
                << ", low: 0x" << IntToString(value.LowWord(), 8).String()
                << " (" << DoubleToString(value).String() << ')' << endl;
    }

    virtual void Describe(const ConstantPool&) const
    {
        Coutput << DoubleToString(value).String();
    }
#endif // JIKES_DEBUG
};


//
// Describes a member name and its signature. JVMS 4.4.6.
//
class CPNameAndTypeInfo : public CPInfo
{
    // u1 tag; // inherited from CPInfo
    const u2 name_index;
    const u2 descriptor_index;

public:
    CPNameAndTypeInfo(u2 _name_index, u2 _descriptor_index)
        : CPInfo(CONSTANT_NameAndType)
        , name_index(_name_index)
        , descriptor_index(_descriptor_index)
    {}
    CPNameAndTypeInfo(ClassFile&);
    virtual ~CPNameAndTypeInfo() {}

    const char* Signature(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[descriptor_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[descriptor_index]) ->
            Bytes();
    }
    u2 SignatureLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[descriptor_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[descriptor_index]) ->
            Length();
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU1(tag);
        output_buffer.PutU2(name_index);
        output_buffer.PutU2(descriptor_index);
    }

    virtual bool Check(const ConstantPool& constant_pool) const
    {
        return constant_pool[name_index] -> Tag() == CONSTANT_Utf8 &&
            constant_pool[descriptor_index] -> Tag() == CONSTANT_Utf8;
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool) const
    {
        Coutput << "NameAndType: name: "
                << (unsigned) name_index;
        if (constant_pool[name_index] -> Tag() == CONSTANT_Utf8)
        {
            Coutput << "=\"";
            constant_pool[name_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
        Coutput << endl << "        type: " << (unsigned) descriptor_index;
        if (constant_pool[descriptor_index] -> Tag() == CONSTANT_Utf8)
        {
            Coutput << "=\"";
            constant_pool[descriptor_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
        Coutput << endl;
    }

    virtual void Describe(const ConstantPool& constant_pool) const
    {
        if (constant_pool[name_index] -> Tag() == CONSTANT_Utf8)
            constant_pool[name_index] -> Describe(constant_pool);
        else Coutput << "(invalid)";
        Coutput << " ";
        if (constant_pool[descriptor_index] -> Tag() == CONSTANT_Utf8)
            constant_pool[descriptor_index] -> Describe(constant_pool);
        else Coutput << "(invalid)";
    }
#endif // JIKES_DEBUG
};


//
// FieldInfo and MethodInfo come next in the JVMS, but must appear after
// we know about attributes.
//
class FieldInfo;
class MethodInfo;

//
// Describes an arbitrary attribute. We subclass this for known attributes.
//
class AttributeInfo
{
public:
    enum AttributeInfoTag
    {
        ATTRIBUTE_ConstantValue,
        ATTRIBUTE_Code,
        ATTRIBUTE_Exceptions,
        ATTRIBUTE_InnerClasses,
        ATTRIBUTE_Synthetic,
        ATTRIBUTE_SourceFile,
        ATTRIBUTE_LineNumberTable,
        ATTRIBUTE_LocalVariableTable,
        ATTRIBUTE_Deprecated,
        ATTRIBUTE_Signature, // defined in JSR 14
        ATTRIBUTE_StackMap, // defined in JSR 139
        ATTRIBUTE_Generic, // all others are currently unknown to jikes
        // next 5 are defined in JSR 175, not yet implemented
        ATTRIBUTE_AnnotationDefault = ATTRIBUTE_Generic,
        ATTRIBUTE_RuntimeInvisibleAnnotations = ATTRIBUTE_Generic,
        ATTRIBUTE_RuntimeInvisibleParameterAnnotations = ATTRIBUTE_Generic,
        ATTRIBUTE_RuntimeVisibleAnnotations = ATTRIBUTE_Generic,
        ATTRIBUTE_RuntimeVisibleParameterAnnotations = ATTRIBUTE_Generic,
        // next 2 defined in JSR 202?, not yet implemented
        ATTRIBUTE_EnclosingMethod = ATTRIBUTE_Generic,
        ATTRIBUTE_LocalVariableTypeTable = ATTRIBUTE_Generic
    };
protected:
    const AttributeInfoTag tag;
    const u2 attribute_name_index;
    u4 attribute_length;
private:
    // only used for ATTRIBUTE_Generic; subclasses have their own storage
    u1* info; // u1 info[attribute_length];

public:
    AttributeInfo(AttributeInfoTag _tag, u2 _name_index, u4 length = 0)
        : tag(_tag)
        , attribute_name_index(_name_index)
        , attribute_length(length)
        , info(NULL)
    {
        assert(tag != ATTRIBUTE_Generic);
    }
    AttributeInfo(AttributeInfoTag, ClassFile&);
    virtual ~AttributeInfo() { delete [] info; }

    AttributeInfoTag Tag() const { return tag; }
    u2 AttributeNameIndex() const { return attribute_name_index; }
    u4 AttributeLength() const { return attribute_length; }

    static AttributeInfoTag Tag(const CPUtf8Info* name);

    //
    // This reads the next attribute from the buffer, advancing it to the
    // end of the attribute. The user is responsible for deleting the result.
    //
    static AttributeInfo* AllocateAttributeInfo(ClassFile&);

    // Subclasses must override if attribute_length != 0.
    virtual void Put(OutputBuffer& output_buffer) const
    {
        assert(tag == ATTRIBUTE_Generic
               ? (! attribute_length || info) : ! info);
        assert(attribute_name_index);
        output_buffer.PutU2(attribute_name_index);
        output_buffer.PutU4(attribute_length);
        if (info)
            output_buffer.PutN(info, attribute_length);
    }

#ifdef JIKES_DEBUG
    void PrintPrefix(const char* category, const ConstantPool& constant_pool,
                     int fill) const
    {
        Coutput.width(fill);
        Coutput << "" << category << ": name: "
                << (unsigned) attribute_name_index;
        if (constant_pool[attribute_name_index] -> Tag() ==
            CPInfo::CONSTANT_Utf8)
        {
            Coutput << "=\"";
            constant_pool[attribute_name_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
        Coutput << ", length: " << (unsigned) attribute_length;
    }

    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        PrintPrefix("Unrecognized attribute", constant_pool, fill);
        if (info)
        {
            Coutput << endl;
            Coutput.width(fill);
            Coutput << "" << " info: ";
            for (unsigned i = 0; i < attribute_length; i++)
                if (info[i] < 0x20 || info[i] >= 0x7f)
                    Coutput << "\\x" << IntToString(info[i], 2).String();
                else Coutput << (char) info[i];
        }
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Valid only on final fields, this marks the initial value of a constant
// variable. JVMS 4.7.2.
//
class ConstantValueAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    const u2 constantvalue_index;

public:
    ConstantValueAttribute(u2 _name_index, u2 _constantvalue_index)
        : AttributeInfo(ATTRIBUTE_ConstantValue, _name_index, 2)
        , constantvalue_index(_constantvalue_index)
    {}
    ConstantValueAttribute(ClassFile&);
    virtual ~ConstantValueAttribute() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(constantvalue_index);
    }

    const CPInfo* Value(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[constantvalue_index] -> Constant());
        return constant_pool[constantvalue_index];
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("ConstantValue", constant_pool, 0);
        Coutput << ", value: " << (unsigned) constantvalue_index << " (";
        if (constant_pool[constantvalue_index] -> Constant())
        {
            constant_pool[constantvalue_index] -> Describe(constant_pool);
        }
        else Coutput << "invalid";
        Coutput << ')' << endl;
    }
#endif // JIKES_DEBUG
};


//
// Valid only on methods, this is the throws clause. JVMS 4.7.4.
//
class ExceptionsAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    // u2 number_of_exceptions; // computed as exception_index_table.Length() 
    // exception_index_table[number_of_exceptions]
    Tuple<u2> exception_index_table;

public:
    ExceptionsAttribute(u2 _name_index)
        : AttributeInfo(ATTRIBUTE_Exceptions, _name_index, 2)
        // +2 for number_of_exceptions
        , exception_index_table(6, 16)
    {}
    ExceptionsAttribute(ClassFile&);
    virtual ~ExceptionsAttribute() {}

    u2 NumberOfExceptions() const { return exception_index_table.Length(); }
    const CPClassInfo* Exception(u2 i, const ConstantPool& constant_pool) const
    {
        assert(constant_pool[exception_index_table[i]] -> Tag() ==
               CPInfo::CONSTANT_Class);
        return (const CPClassInfo*) constant_pool[exception_index_table[i]];
    }

    void AddExceptionIndex(u2 index)
    {
        attribute_length += 2;
        exception_index_table.Next() = index;
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(exception_index_table.Length());
        for (unsigned i = 0; i < exception_index_table.Length(); i++)
            output_buffer.PutU2(exception_index_table[i]);
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("Exceptions", constant_pool, 0);
        Coutput << ", count: " << (unsigned) exception_index_table.Length()
                << endl;
        for (unsigned i = 0; i < exception_index_table.Length(); i++)
        {
            Coutput << "  " << (unsigned) exception_index_table[i];
            if (constant_pool[exception_index_table[i]] -> Tag() ==
                CPInfo::CONSTANT_Class)
            {
                Coutput << '=';
                constant_pool[exception_index_table[i]] ->
                    Describe(constant_pool);
            }
            else Coutput << "(invalid)";                    
            Coutput << endl;
        }
    }
#endif // JIKES_DEBUG
};


//
// Valid only on classes, this lists information about all nested classes
// mentioned in the constant pool. JVMS 4.7.5.
//
class InnerClassesAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    // u2 number_of_classes; // computed as classes.Length()
    struct InnerClassesElement
    {
        u2 inner_class_info_index;
        u2 outer_class_info_index;
        u2 inner_name_index;
        AccessFlags inner_class_access_flags; // u2 inner_class_access_flags;
    };
    Tuple<InnerClassesElement> classes; // classes[number_of_classes]

public:
    InnerClassesAttribute(u2 _name_index)
        : AttributeInfo(ATTRIBUTE_InnerClasses, _name_index, 2)
        // +2 for number_of_classes
        , classes(6, 16)
    {}
    InnerClassesAttribute(ClassFile&);
    virtual ~InnerClassesAttribute() {}

    u2 InnerClassesLength() const { return classes.Length();}

    void AddInnerClass(u2 inner_class_info_index, u2 outer_class_info_index,
                       u2 inner_name_index, u2 inner_class_access_flags)
    {
        attribute_length += 8;
        InnerClassesElement& entry = classes.Next();
        entry.inner_class_info_index = inner_class_info_index;
        entry.outer_class_info_index = outer_class_info_index;
        entry.inner_name_index = inner_name_index;
        entry.inner_class_access_flags.SetFlags(inner_class_access_flags);
    }

    const CPClassInfo* Inner(u2 i, const ConstantPool& constant_pool) const
    {
        assert(constant_pool[classes[i].inner_class_info_index] -> Tag() ==
               CPInfo::CONSTANT_Class);
        return (const CPClassInfo*)
            constant_pool[classes[i].inner_class_info_index];
    }
    const CPClassInfo* Outer(u2 i, const ConstantPool& constant_pool) const
    {
        assert(! classes[i].outer_class_info_index ||
               (constant_pool[classes[i].outer_class_info_index] -> Tag() ==
                CPInfo::CONSTANT_Class));
        return classes[i].outer_class_info_index
            ? ((const CPClassInfo*)
               constant_pool[classes[i].outer_class_info_index])
            : (const CPClassInfo*) NULL;
    }
    const char* Name(u2 i, const ConstantPool& constant_pool) const
    {
        assert(! classes[i].inner_name_index ||
               (constant_pool[classes[i].inner_name_index] -> Tag() ==
                CPInfo::CONSTANT_Utf8));
        return classes[i].inner_name_index
            ? ((const CPUtf8Info*)
               constant_pool[classes[i].inner_name_index]) -> Bytes()
            : (const char*) NULL;
    }
    u2 NameLength(u2 i, const ConstantPool& constant_pool) const
    {
        assert(! classes[i].inner_name_index ||
               (constant_pool[classes[i].inner_name_index] -> Tag() ==
                CPInfo::CONSTANT_Utf8));
        return classes[i].inner_name_index
            ? ((const CPUtf8Info*)
               constant_pool[classes[i].inner_name_index]) -> Length() : 0;
    }
    const AccessFlags& Flags(u2 i) const
    {
        return classes[i].inner_class_access_flags;
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(classes.Length());
        for (unsigned i = 0; i < classes.Length(); i++)
        {
            output_buffer.PutU2(classes[i].inner_class_info_index);
            output_buffer.PutU2(classes[i].outer_class_info_index);
            output_buffer.PutU2(classes[i].inner_name_index);
            output_buffer.PutU2(classes[i].inner_class_access_flags.Flags());
        }
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("InnerClasses", constant_pool, 0);
        Coutput << ", count: " << (unsigned) classes.Length() << endl;
        for (unsigned i = 0; i < classes.Length(); i++)
        {
            Coutput << "  inner: "
                    << (unsigned) classes[i].inner_class_info_index;
            if (constant_pool[classes[i].inner_class_info_index] -> Tag() ==
                CPInfo::CONSTANT_Class)
            {
                Coutput << '=';
                constant_pool[classes[i].inner_class_info_index] ->
                    Describe(constant_pool);
            }
            else Coutput << "(invalid)";
            Coutput << ", outer: "
                    << (unsigned) classes[i].outer_class_info_index;
            if (! classes[i].outer_class_info_index)
                Coutput << "(inner is local)";
            else if (constant_pool[classes[i].outer_class_info_index] ->
                     Tag() == CPInfo::CONSTANT_Class)
            {
                Coutput << '=';
                constant_pool[classes[i].outer_class_info_index] ->
                    Describe(constant_pool);
            }
            else Coutput << "(invalid)";
            Coutput << endl << "   name: "
                    << (unsigned) classes[i].inner_name_index;
            if (! classes[i].inner_name_index)
                Coutput << "(anonymous)";
            else if (constant_pool[classes[i].inner_name_index] -> Tag() ==
                     CPInfo::CONSTANT_Utf8)
            {
                Coutput << '=';
                constant_pool[classes[i].inner_name_index] ->
                    Describe(constant_pool);
            }
            else Coutput << "(invalid)";
            Coutput << ", ";
            classes[i].inner_class_access_flags.Print();
            Coutput << endl;
        }
    }
#endif // JIKES_DEBUG
};


//
// Valid for classes, methods, and fields, this marks a compiler-generated
// entity that has no counterpart in source. JVMS 4.7.6.
//
class SyntheticAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo

public:
    SyntheticAttribute(u2 _name_index)
        : AttributeInfo(ATTRIBUTE_Synthetic, _name_index)
    {}
    SyntheticAttribute(ClassFile&);
    virtual ~SyntheticAttribute() {}

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("Synthetic", constant_pool, 0);
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Valid only for classes, this names the source file. JVMS 4.7.7.
//
class SourceFileAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    const u2 sourcefile_index;

public:
    SourceFileAttribute(u2 _name_index, u2 _sourcefile_index)
        : AttributeInfo(ATTRIBUTE_SourceFile, _name_index, 2),
          sourcefile_index(_sourcefile_index)
    {}
    SourceFileAttribute(ClassFile&);
    virtual ~SourceFileAttribute() {}

    const char* SourceFile(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[sourcefile_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        return ((CPUtf8Info*) constant_pool[sourcefile_index]) -> Bytes();
    }
    u2 SourceFileLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[sourcefile_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        return ((CPUtf8Info*) constant_pool[sourcefile_index]) -> Length();
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(sourcefile_index);
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("SourceFile", constant_pool, 0);
        Coutput << ", name: " << (unsigned) sourcefile_index;
        if (constant_pool[sourcefile_index] -> Tag() == CPInfo::CONSTANT_Utf8)
        {
            Coutput << "=\"";
            constant_pool[sourcefile_index] -> Describe(constant_pool);
            Coutput << '"';
        }
        else Coutput << "(invalid)";
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Valid only for code, this correlates bytecode offsets to source file line
// numbers. JVMS 4.7.8.
//
class LineNumberTableAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    // u2 line_number_table_length; // computed as line_number_table.Length()
    struct LineNumberElement
    {
        u2 start_pc;
        u2 line_number;
    };
    // line_number_table[line_number_table_length]
    Tuple<LineNumberElement> line_number_table;

public:
    LineNumberTableAttribute(u2 _name_index)
        : AttributeInfo(ATTRIBUTE_LineNumberTable, _name_index, 2)
        // +2 for line_number_table_length
        , line_number_table(6, 16)
    {}
    LineNumberTableAttribute(ClassFile&);
    virtual ~LineNumberTableAttribute() {}

    u2 LineNumberTableLength() const
    {
        return line_number_table.Length();
    }

    void AddLineNumber(u2 start_pc, u2 line_number)
    {
        attribute_length += 4;
        LineNumberElement& entry = line_number_table.Next();
        entry.start_pc = start_pc;
        entry.line_number = line_number;
    }

    void SetMax(u2 max_pc)
    {
        unsigned i = line_number_table.Length();
        while(--i > 0 && line_number_table[i].start_pc > max_pc)
            line_number_table[i].start_pc = max_pc;
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(line_number_table.Length());
        for (unsigned i = 0; i < line_number_table.Length(); i++)
        {
            output_buffer.PutU2(line_number_table[i].start_pc);
            output_buffer.PutU2(line_number_table[i].line_number);
        }
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(fill == 2);
        PrintPrefix("LineNumberTable", constant_pool, 2);
        Coutput << ", count: " << (unsigned) line_number_table.Length()
                << endl;
        for (unsigned i = 0; i < line_number_table.Length(); i++)
        {
            Coutput << "    pc: "
                    << (unsigned) line_number_table[i].start_pc
                    << ", line_number: "
                    << (unsigned) line_number_table[i].line_number << endl;
        }
    }
#endif // JIKES_DEBUG
};


//
// Valid only for code, this gives local variable name, scope, and type
// information relative to bytecode offsets. JVMS 4.7.10.
//
class LocalVariableTableAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    // u2 local_variable_length; // computed as local_variable_table.Length()
    struct LocalVariableElement
    {
        u2 start_pc;
        u2 length;
        u2 name_index;
        u2 descriptor_index;
        u2 index;
    };
    // local_variable_table[local_variable_table_length]
    Tuple<LocalVariableElement> local_variable_table;

public:
    LocalVariableTableAttribute(u2 _name_index)
        : AttributeInfo(ATTRIBUTE_LocalVariableTable, _name_index, 2)
        // +2 for local_variable_table_length
        , local_variable_table(6, 16)
    {}
    LocalVariableTableAttribute(ClassFile&);
    virtual ~LocalVariableTableAttribute() {}

    u2 LocalVariableTableLength() const
    {
        return local_variable_table.Length();
    }

    //
    // make entry in local variable table
    //
    void AddLocalVariable(u2 start, u2 end, u2 name, u2 descriptor, u2 index)
    {
        assert(end >= start);

        if (end > start)
        {
            attribute_length += 10;
            LocalVariableElement& entry = local_variable_table.Next();
            entry.start_pc = start;
            entry.length = end - start;
            entry.name_index = name;
            entry.descriptor_index = descriptor;
            entry.index = index;
        }
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(local_variable_table.Length());
        for (unsigned i = 0; i < local_variable_table.Length(); i++)
        {
            output_buffer.PutU2(local_variable_table[i].start_pc);
            output_buffer.PutU2(local_variable_table[i].length);
            output_buffer.PutU2(local_variable_table[i].name_index);
            output_buffer.PutU2(local_variable_table[i].descriptor_index);
            output_buffer.PutU2(local_variable_table[i].index);
        }
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(fill == 2);
        PrintPrefix("LocalVariableTable", constant_pool, 2);
        Coutput << ", count: " << (unsigned) local_variable_table.Length()
                << endl;
        for (unsigned i = 0; i < local_variable_table.Length(); i++)
        {
            unsigned index = local_variable_table[i].name_index;
            Coutput << "    pc: "
                    << (unsigned) local_variable_table[i].start_pc
                    << ", length: "
                    << (unsigned) local_variable_table[i].length << "(pc: "
                    << (unsigned) (local_variable_table[i].start_pc + 
                                   local_variable_table[i].length)
                    << "), name: " << index;
            if (constant_pool[index] -> Tag() == CPInfo::CONSTANT_Utf8)
            {
                Coutput << '=';
                constant_pool[index] -> Describe(constant_pool);
            }
            else Coutput << "(invalid)";
            index = local_variable_table[i].descriptor_index;
            Coutput << endl << "     type: " << index;
            if (constant_pool[index] -> Tag() == CPInfo::CONSTANT_Utf8)
            {
                Coutput << '=';
                constant_pool[index] -> Describe(constant_pool);
            }
            else Coutput << "(invalid)";
            Coutput << ", index: " << (unsigned) local_variable_table[i].index
                    << endl;
        }
    }
#endif // JIKES_DEBUG
};


//
// Valid for classes, methods, and fields, this marks an entity as
// deprecated. JVMS 4.7.10.
//
class DeprecatedAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo

public:
    DeprecatedAttribute(u2 _name_index)
        : AttributeInfo(ATTRIBUTE_Deprecated, _name_index)
    {}
    DeprecatedAttribute(ClassFile&);
    virtual ~DeprecatedAttribute() {}

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("Deprecated", constant_pool, 0);
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Valid for classes, methods, and fields, this gives the parameterized type
// signature for a generic type. JSR 14 section 7.
//
class SignatureAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    const u2 signature_index;

public:
    SignatureAttribute(u2 name_index, u2 index)
        : AttributeInfo(ATTRIBUTE_Signature, name_index, 2)
        , signature_index(index)
    {}
    SignatureAttribute(ClassFile&);
    virtual ~SignatureAttribute() {}

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(signature_index);
    }

    const CPUtf8Info* Signature(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[signature_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        return (const CPUtf8Info*) constant_pool[signature_index];
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(! fill);
        PrintPrefix("Signature", constant_pool, 0);
        Coutput << endl << " signature: " << (unsigned) signature_index;
        if (constant_pool[signature_index] -> Tag() == CPInfo::CONSTANT_Utf8)
        {
            Coutput << '=';
            constant_pool[signature_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Valid for code, this speeds up verification by listing the types of all
// locals and stack elements at the start of each basic block. JSR 139
// appendix 1 section 2.1 (or JSR 30 section 5.3.1.2).
//
class StackMapAttribute : public AttributeInfo
{
public:
    class StackMapFrame
    {
        class VerificationTypeInfo
        {
        public:
            enum VerificationTypeInfoTag
            {
                TYPE_Top = 0, // also called Bogus
                TYPE_Integer = 1,
                TYPE_Float = 2,
                TYPE_Long = 4,
                TYPE_Double = 3,
                TYPE_Null = 5,
                TYPE_UninitializedThis = 6,
                TYPE_Object = 7,
                TYPE_Uninitialized = 8
            };

        private:
            VerificationTypeInfoTag tag;
            // u2 cpool_index for Object,
            // u2 offset for Unitialized
            // unused for remaining types
            u2 info;

        public:
            VerificationTypeInfo()
                : tag(TYPE_Top)
            {}
            void Read(ClassFile&);

            u2 Size() const { return tag >= TYPE_Object ? 3 : 1; }

            void Put(OutputBuffer& output_buffer) const
            {
                output_buffer.PutU1(tag);
                if (tag >= TYPE_Object)
                    output_buffer.PutU2(info);
            }

#ifdef JIKES_DEBUG
            void Print(const ConstantPool& constant_pool,
                       unsigned& index) const
            {
                Coutput << index++ << ':';
                switch (tag)
                {
                case TYPE_Top:
                    Coutput << "void";
                    break;
                case TYPE_Integer:
                    Coutput << "int";
                    break;
                case TYPE_Float:
                    Coutput << "float";
                    break;
                case TYPE_Long:
                    Coutput << "long";
                    index++;
                    break;
                case TYPE_Double:
                    Coutput << "double";
                    index++;
                    break;
                case TYPE_Null:
                    Coutput << "null";
                    break;
                case TYPE_UninitializedThis:
                    Coutput << "unitialized this";
                    break;
                case TYPE_Object:
                    Coutput << (unsigned) info;
                    if (constant_pool[info] -> Tag() == CPInfo::CONSTANT_Class)
                    {
                        Coutput << '=';
                        constant_pool[info] -> Describe(constant_pool);
                    }
                    else Coutput << "(invalid)";
                    break;
                case TYPE_Uninitialized:
                    Coutput << "unitialized since " << (unsigned) info;
                    break;
                default:
                    assert(false && "invalid VerificationTypeInfoTag");
                }
            }
#endif // JIKES_DEBUG
        };

    private:
        u2 offset;
        // u2 locals_size; // computed as locals.Length()
        Tuple<VerificationTypeInfo> locals;
        // u2 stack_size; // computed as stack.Length()
        Tuple<VerificationTypeInfo> stack;

        u2 frame_size; // needed since VerificationTypeInfo is variable size

    public:
        StackMapFrame(u2 _offset)
            : offset(_offset)
            , locals(6, 16)
            , stack(6, 16)
            , frame_size(6)
            // +2 for offset, +2 for locals_size, +2 for stack_size
        {}
        StackMapFrame(ClassFile&);

        // Note that LocalsSize() + StackSize() + 6 <= FrameSize()
        u2 LocalsSize() const { return locals.Length(); }
        u2 StackSize() const { return stack.Length(); }
        u2 FrameSize() const { return frame_size; }

        void AddLocal(const VerificationTypeInfo& entry)
        {
            frame_size += entry.Size();
            locals.Next() = entry;
        }
        void AddStack(const VerificationTypeInfo& entry)
        {
            frame_size += entry.Size();
            stack.Next() = entry;
        }

        void Put(OutputBuffer& output_buffer) const
        {
            output_buffer.PutU2(offset);
            output_buffer.PutU2(locals.Length());
            for (unsigned i = 0; i < locals.Length(); i++)
                locals[i].Put(output_buffer);
            output_buffer.PutU2(stack.Length());
            for (unsigned j = 0; j < stack.Length(); j++)
                stack[j].Put(output_buffer);
        }

#ifdef JIKES_DEBUG
        void Print(const ConstantPool& constant_pool) const
        {
            Coutput << "    pc: " << (unsigned) offset << ", locals_size: "
                    << (unsigned) locals.Length() << ", stack_size: "
                    << (unsigned) stack.Length() << endl;
            unsigned index = 0;
            if (locals.Length())
            {
                Coutput << "      locals: ";
                locals[0].Print(constant_pool, index);
                for (unsigned i = 1; i < locals.Length(); i++)
                {
                    Coutput << ", ";
                    locals[i].Print(constant_pool, index);
                }
                Coutput << endl;
            }
            index = 0;
            if (stack.Length())
            {
                Coutput << "      stack: ";
                stack[0].Print(constant_pool, index);
                for (unsigned i = 1; i < stack.Length(); i++)
                {
                    Coutput << ", ";
                    stack[i].Print(constant_pool, index);
                }
                Coutput << endl;
            }
        }
#endif // JIKES_DEBUG
    };

private:
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    // u2 frame_count; // computed as frames.Length();
    Tuple<StackMapFrame*> frames; // stack_map_frame[frame_count]

public:
    StackMapAttribute(u2 name)
        : AttributeInfo(ATTRIBUTE_StackMap, name, 2)
        // +2 for frame_count
        , frames(6, 16)
    {}
    StackMapAttribute(ClassFile&);
    virtual ~StackMapAttribute()
    {
        for (unsigned i = 0; i < frames.Length(); i++)
            delete frames[i];
    }

    u2 FrameCount() const { return frames.Length(); }

    void AddFrame(StackMapFrame* frame)
    {
        frames.Next() = frame;
        attribute_length += frame -> FrameSize();
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(frames.Length());
        for (unsigned i = 0; i < frames.Length(); i++)
            frames[i] -> Put(output_buffer);
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const
    {
        assert(fill == 2);
        PrintPrefix("StackMap", constant_pool, 2);
        Coutput << ", frames: "
                << (unsigned) frames.Length() << endl;
        for (unsigned i = 0; i < frames.Length(); i++)
            frames[i] -> Print(constant_pool);
    }
#endif // JIKES_DEBUG
};


//
// Valid only on non-abstract non-native methods, this is the method
// implementation. JVMS 4.7.3.
//
class CodeAttribute : public AttributeInfo
{
    // u2 attribute_name_index; // inherited from AttributeInfo
    // u4 attribute_length; // inherited from AttributeInfo
    u2 max_stack;
    u2 max_locals;
    // u4 code_length; // computed as code.Length();
    Tuple<u1> code; // code[code_length]
    // u2 exception_table_length; // computed as exception_table.Length();
    struct ExceptionElement
    {
        u2 start_pc;
        u2 end_pc;
        u2 handler_pc;
        u2 catch_type;
    };
    // exception_table[exception_table_length]
    Tuple<ExceptionElement> exception_table;
    // u2 attribute_count; // computed as attributes.Length();
    Tuple<AttributeInfo*> attributes; // attributes[attributes_count]

    // Remember location of known attributes.
    LineNumberTableAttribute* attr_linenumbers;
    LocalVariableTableAttribute* attr_locals;
    StackMapAttribute* attr_stackmap;

public:
    CodeAttribute(u2 _name_index, u2 _max_locals)
        : AttributeInfo(ATTRIBUTE_Code, _name_index, 12)
        // +2 for max_stack, +2 for max_locals, +4 for code_length,
        // +2 for exception_table_length, +2 for attributes_count
        , max_stack(0) // filled in later
        , max_locals(_max_locals)
        , code(8, 4)
        , exception_table(6, 16)
        , attributes(6, 16)
        , attr_linenumbers(NULL)
        , attr_locals(NULL)
        , attr_stackmap(NULL)
    {}
    CodeAttribute(ClassFile&);
    virtual ~CodeAttribute()
    {
        for (unsigned i = 0; i < attributes.Length(); i++)
            delete attributes[i];
    }

    u2 MaxStack() const { return max_stack; }
    void SetMaxStack(u2 val) { max_stack = val; }

    u2 MaxLocals() const { return max_locals; }
    void SetMaxLocals(u2 val) { max_locals = val; }

    u2 CodeLength() const
    {
        return CodeLengthExceeded() ? 65535 : (u2) code.Length();
    }
    bool CodeLengthExceeded() const { return code.Length() > 65535; }

    void ResetCode(unsigned i, u1 byte) { code[i] = byte; }

    void AddCode(u1 byte)
    {
        code.Next() = byte;
        attribute_length++;
    }

    void DeleteCode(unsigned count)
    {
        assert(count <= code.Length());
        code.Reset(code.Length() - count);
        attribute_length -= count;
    }

    u2 ExceptionTableLength() const { return exception_table.Length(); }

    void AddException(u2 start_pc, u2 end_pc, u2 handler_pc, u2 catch_type)
    {
        if (start_pc == end_pc)
            return;
        if (exception_table.Length())
        {
            ExceptionElement& other = exception_table.Top();
            if (end_pc == other.start_pc && handler_pc == other.handler_pc &&
                catch_type == other.catch_type)
            {
                other.start_pc = start_pc;
                return;
            }
        }
        attribute_length += 8;
        ExceptionElement& entry = exception_table.Next();
        entry.start_pc = start_pc;
        entry.end_pc = end_pc;
        entry.handler_pc = handler_pc;
        entry.catch_type = catch_type;
    }

    u2 AttributesCount() const { return attributes.Length(); }

    void AddAttribute(AttributeInfo* attribute)
    {
        attributes.Next() = attribute;
        attribute_length += 6 + attribute -> AttributeLength();
        // +6 for name and length of attribute
        switch (attribute -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_LineNumberTable:
            assert(! attr_linenumbers);
            attr_linenumbers = (LineNumberTableAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_LocalVariableTable:
            assert(! attr_locals);
            attr_locals = (LocalVariableTableAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_StackMap:
            assert(! attr_stackmap);
            attr_stackmap = (StackMapAttribute*) attribute;
            break;
        default:
            assert(false && "adding unexpected code attribute");
        }
    }

    virtual void Put(OutputBuffer& output_buffer) const
    {
        unsigned i;
        AttributeInfo::Put(output_buffer);
        output_buffer.PutU2(max_stack);
        output_buffer.PutU2(max_locals);
        output_buffer.PutU4(code.Length());
        for (i = 0; i < code.Length(); i++)
            output_buffer.PutU1(code[i]);
        output_buffer.PutU2(exception_table.Length());
        for (i = 0; i < exception_table.Length(); i++)
        {
            output_buffer.PutU2(exception_table[i].start_pc);
            output_buffer.PutU2(exception_table[i].end_pc);
            output_buffer.PutU2(exception_table[i].handler_pc);
            output_buffer.PutU2(exception_table[i].catch_type);
        }
        output_buffer.PutU2(attributes.Length());
        for (i = 0; i < attributes.Length(); i++)
            attributes[i] -> Put(output_buffer);
    }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool& constant_pool, int fill = 0) const;
#endif // JIKES_DEBUG
};


//
// Describes a member field. JVMS 4.5.
//
class FieldInfo : public AccessFlags
{
    // u2 access_flags; // inherited from AccessFlags
    u2 name_index;
    u2 descriptor_index;
    // u2 attributes_count; // computed as attributes.Length();
    Tuple<AttributeInfo*> attributes; // attributes[attributes_count]

    // Remember location of known attributes.
    SyntheticAttribute* attr_synthetic;
    DeprecatedAttribute* attr_deprecated;
    SignatureAttribute* attr_signature;
    ConstantValueAttribute* attr_constantvalue;

public:
    FieldInfo()
        : name_index(0)
        , descriptor_index(0)
        , attr_synthetic(NULL)
        , attr_deprecated(NULL)
        , attr_signature(NULL)
        , attr_constantvalue(NULL)
    {}
    FieldInfo(ClassFile&);
    ~FieldInfo()
    {
        for (unsigned i = 0; i < attributes.Length(); i++)
            delete attributes[i];
    }

    inline void SetNameIndex(u2 _name_index) { name_index = _name_index; }
    const char* Name(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[name_index] -> Tag() == CPInfo::CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[name_index]) -> Bytes();
    }
    u2 NameLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[name_index] -> Tag() == CPInfo::CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[name_index]) -> Length();
    }

    inline void SetDescriptorIndex(u2 index) { descriptor_index = index; }
    const char* Signature(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[descriptor_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        const CPUtf8Info* sig = attr_signature
            ? attr_signature -> Signature(constant_pool)
            : (const CPUtf8Info*) constant_pool[descriptor_index];
        return sig -> Bytes();
    }
    u2 SignatureLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[descriptor_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        const CPUtf8Info* sig = attr_signature
            ? attr_signature -> Signature(constant_pool)
            : (const CPUtf8Info*) constant_pool[descriptor_index];
        return sig -> Length();
    }

    inline u2 AttributesCount() const { return attributes.Length(); }
    inline void AddAttribute(AttributeInfo* attribute)
    {
        attributes.Next() = attribute;
        switch (attribute -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_Synthetic:
            assert(! attr_synthetic);
            attr_synthetic = (SyntheticAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Deprecated:
            assert(! attr_deprecated);
            attr_deprecated = (DeprecatedAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Signature:
            assert(! attr_signature);
            attr_signature = (SignatureAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_ConstantValue:
            assert(! attr_constantvalue);
            attr_constantvalue = (ConstantValueAttribute*) attribute;
            break;
        default:
            assert(false && "adding unexpected field attribute");
        }
    }
    bool Synthetic() const { return attr_synthetic != NULL; }
    bool Deprecated() const { return attr_deprecated != NULL; }
    const CPInfo* ConstantValue(const ConstantPool& constant_pool) const
    {
        return attr_constantvalue ? attr_constantvalue -> Value(constant_pool)
            : (const CPInfo*) NULL;
    }
    

    inline void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU2(access_flags);
        output_buffer.PutU2(name_index);
        output_buffer.PutU2(descriptor_index);
        output_buffer.PutU2(attributes.Length());
        for (unsigned i = 0; i < attributes.Length(); i++)
            attributes[i] -> Put(output_buffer);
    }

#ifdef JIKES_DEBUG
    void Print(const ConstantPool& constant_pool) const
    {
        Coutput << "field: ";
        AccessFlags::Print();
        Coutput << ", name: " << (unsigned) name_index;
        if (constant_pool[name_index] -> Tag() == CPInfo::CONSTANT_Utf8)
        {
            Coutput << '=';
            constant_pool[name_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << endl << " type: " << (unsigned) descriptor_index;
        if (constant_pool[descriptor_index] -> Tag() == CPInfo::CONSTANT_Utf8)
        {
            Coutput << '=';
            constant_pool[descriptor_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << ", attributes: " << (unsigned) attributes.Length() << endl;
        for (unsigned i = 0; i < attributes.Length(); i++)
            attributes[i] -> Print(constant_pool);
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Describes a member method, including constructors and the static
// initializer. JVMS 4.6.
//
class MethodInfo : public AccessFlags
{
    // u2 access_flags; // inherited from AccessFlags
    u2 name_index;
    u2 descriptor_index;
    // u2 attributes_count; // computed as attributes.Length();
    Tuple<AttributeInfo*> attributes; // attributes[attributes_count]

    // Remember location of known attributes.
    SyntheticAttribute* attr_synthetic;
    DeprecatedAttribute* attr_deprecated;
    SignatureAttribute* attr_signature;
    CodeAttribute* attr_code;
    ExceptionsAttribute* attr_exceptions;

public:
    MethodInfo()
        : name_index(0)
        , descriptor_index(0)
        , attr_synthetic(NULL)
        , attr_deprecated(NULL)
        , attr_signature(NULL)
        , attr_code(NULL)
        , attr_exceptions(NULL)
    {}
    MethodInfo(ClassFile&);
    ~MethodInfo()
    {
        for (unsigned i = 0; i < attributes.Length(); i++)
            delete attributes[i];
    }

    inline void SetNameIndex(u2 _name_index) { name_index = _name_index; }
    const char* Name(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[name_index] -> Tag() == CPInfo::CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[name_index]) -> Bytes();
    }
    u2 NameLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[name_index] -> Tag() == CPInfo::CONSTANT_Utf8);
        return ((const CPUtf8Info*) constant_pool[name_index]) -> Length();
    }

    inline void SetDescriptorIndex(u2 index) { descriptor_index = index; }
    const char* Signature(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[descriptor_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        const CPUtf8Info* sig = attr_signature
            ? attr_signature -> Signature(constant_pool)
            : (const CPUtf8Info*) constant_pool[descriptor_index];
        return sig -> Bytes();
    }
    u2 SignatureLength(const ConstantPool& constant_pool) const
    {
        assert(constant_pool[descriptor_index] -> Tag() ==
               CPInfo::CONSTANT_Utf8);
        const CPUtf8Info* sig = attr_signature
            ? attr_signature -> Signature(constant_pool)
            : (const CPUtf8Info*) constant_pool[descriptor_index];
        return sig -> Length();
    }

    inline u2 AttributesCount() const { return attributes.Length(); }
    inline void AddAttribute(AttributeInfo* attribute)
    {
        attributes.Next() = attribute;
        switch (attribute -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_Synthetic:
            assert(! attr_synthetic);
            attr_synthetic = (SyntheticAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Deprecated:
            assert(! attr_deprecated);
            attr_deprecated = (DeprecatedAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Signature:
            assert(! attr_signature);
            attr_signature = (SignatureAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Code:
            assert(! attr_code);
            attr_code = (CodeAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Exceptions:
            assert(! attr_exceptions);
            attr_exceptions = (ExceptionsAttribute*) attribute;
            break;
        default:
            assert(false && "adding unexpected method attribute");
        }
    }
    bool Synthetic() const { return attr_synthetic != NULL; }
    bool Deprecated() const { return attr_deprecated != NULL; }
    const CodeAttribute* Code() const { return attr_code; }
    const ExceptionsAttribute* Exceptions() const { return attr_exceptions; }

    inline void Put(OutputBuffer& output_buffer) const
    {
        output_buffer.PutU2(access_flags);
        output_buffer.PutU2(name_index);
        output_buffer.PutU2(descriptor_index);
        output_buffer.PutU2(attributes.Length());
        for (unsigned i = 0; i < attributes.Length(); i++)
            attributes[i] -> Put(output_buffer);
    }

#ifdef JIKES_DEBUG
     void Print(const ConstantPool& constant_pool) const
     {
        Coutput << "method: ";
        AccessFlags::Print();
        Coutput << ", name: " << (unsigned) name_index;
        if (constant_pool[name_index] -> Tag() == CPInfo::CONSTANT_Utf8)
        {
            Coutput << '=';
            constant_pool[name_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << endl << " type: " << (unsigned) descriptor_index;
        if (constant_pool[descriptor_index] -> Tag() == CPInfo::CONSTANT_Utf8)
        {
            Coutput << '=';
            constant_pool[descriptor_index] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << ", attributes: " << (unsigned) attributes.Length() << endl;
        for (unsigned i = 0; i < attributes.Length(); i++)
            attributes[i] -> Print(constant_pool);
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};


//
// Here we have the complete definition of the .class file format. JVMS 4.1.
//
class ClassFile : public AccessFlags
{
    enum
    {
        MAGIC = 0xcafebabe
    };
    bool valid; // true unless input file not valid
    const char* buffer; // current location during input (NULL for output)
    const char* const buffer_tail; // last location during input

protected:
    u4 magic;
    u2 minor_version;
    u2 major_version;

    // u2 constant_pool_count; // computed as constant_pool.Length()
    // cp_info constant_pool[constant_pool_count - 1]
    ConstantPool constant_pool; // note that constant_pool[0] is invalid

    // u2 access_flags; // inherited from AccessFlags
    u2 this_class;
    u2 super_class;

    // u2 interfaces_count // computed as interfaces.Length()
    Tuple<u2> interfaces; // u2 interfaces[interfaces_count]

    // u2 fields_count // computed as fields.Length()
    Tuple<FieldInfo*> fields; // field_info fields[fields_count]

    // u2 methods_count // computed as methods.Length()
    Tuple<MethodInfo*> methods; // method_info methods[methods_count]

    // u2 attributes_count // computed as attributes.Length()
    // u2 attribute_info attributes[attributes_count]
    Tuple<AttributeInfo*> attributes;

    // Remember location of known attributes.
    SyntheticAttribute* attr_synthetic;
    DeprecatedAttribute* attr_deprecated;
    SignatureAttribute* attr_signature;
    SourceFileAttribute* attr_sourcefile;
    InnerClassesAttribute* attr_innerclasses;

public:
    //
    // Construct a class file for output, given the finished type.
    //
    ClassFile()
        : valid(true)
        , buffer(NULL)
        , buffer_tail(NULL)
        , magic(MAGIC)
        , constant_pool(8, 4)
        , interfaces(6, 16)
        , fields(6, 16)
        , methods(6, 16)
        , attributes(6, 16)
        , attr_synthetic(NULL)
        , attr_deprecated(NULL)
        , attr_signature(NULL)
        , attr_sourcefile(NULL)
        , attr_innerclasses(NULL)
    {}

    //
    // Read a class from input. Note that we try to parse to the end of the
    // file, even after encountering an error, for purposes of decompiling.
    //
    ClassFile(const char*, unsigned);

    ~ClassFile()
    {
        unsigned i;
        for (i = 1; i < constant_pool.Length(); i++)
            if (constant_pool.Valid(i))
                delete constant_pool[i];
        for (i = 0; i < fields.Length(); i++)
            delete fields[i];
        for (i = 0; i < methods.Length(); i++)
            delete methods[i];
        for (i = 0; i < attributes.Length(); i++)
            delete attributes[i];
    }

    u2 ConstantPoolCount() const { return constant_pool.Length(); }
    const ConstantPool& Pool() const { return constant_pool; }
    CPClassInfo* ThisClass()
    {
        return constant_pool[this_class] -> Tag() == CPInfo::CONSTANT_Class
            ? (CPClassInfo*) constant_pool[this_class]
            : (CPClassInfo*) NULL;
    }
    CPClassInfo* SuperClass()
    {
        return constant_pool[super_class] -> Tag() == CPInfo::CONSTANT_Class
            ? (CPClassInfo*) constant_pool[super_class]
            : (CPClassInfo*) NULL;
    }
    u2 InterfacesCount() const { return interfaces.Length(); }
    CPClassInfo* Interface(u2 i)
    {
        return constant_pool[interfaces[i]] -> Tag() == CPInfo::CONSTANT_Class
            ? (CPClassInfo*) constant_pool[interfaces[i]]
            : (CPClassInfo*) NULL;
    }
    u2 FieldsCount() const { return fields.Length(); }
    const FieldInfo* Field(u2 i) const { return fields[i]; }
    u2 MethodsCount() const { return methods.Length();}
    const MethodInfo* Method(u2 i) const { return methods[i]; }

    u2 AttributesCount() const { return attributes.Length(); }
    const AttributeInfo* Attribute(u2 i) const { return attributes[i]; }
    inline void AddAttribute(AttributeInfo* attribute)
    {
        attributes.Next() = attribute;
        switch (attribute -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_Synthetic:
            assert(! attr_synthetic);
            attr_synthetic = (SyntheticAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Deprecated:
            assert(! attr_deprecated);
            attr_deprecated = (DeprecatedAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_Signature:
            assert(! attr_signature);
            attr_signature = (SignatureAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_SourceFile:
            assert(! attr_sourcefile);
            attr_sourcefile = (SourceFileAttribute*) attribute;
            break;
        case AttributeInfo::ATTRIBUTE_InnerClasses:
            assert(! attr_innerclasses);
            attr_innerclasses = (InnerClassesAttribute*) attribute;
            break;
        default:
            assert(false && "adding unexpected class attribute");
        }
    }
    bool Synthetic() const { return attr_synthetic != NULL; }
    bool Deprecated() const { return attr_deprecated != NULL; }
    const char* SourceFile() const
    {
        return attr_sourcefile ? attr_sourcefile -> SourceFile(constant_pool)
            : (const char*) NULL;
    }
    u2 SourceFileLength() const
    {
        return attr_sourcefile
            ? attr_sourcefile -> SourceFileLength(constant_pool) : 0;
    }
    const InnerClassesAttribute* InnerClasses() const
    {
        return attr_innerclasses;
    }

    void Write(TypeSymbol* unit_type) const;

    bool Valid() const { return valid; }
    void MarkInvalid() { valid = false; }

    inline u1 GetU1()
    {
        if (buffer < buffer_tail)
            return (u1) *buffer++;
        valid = false;
        return 0;
    }
    inline u2 GetU2()
    {
        valid &= (buffer + 1 < buffer_tail);
        u2 i = buffer == buffer_tail ? 0 : (u1) *buffer++;
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        return i;
    }
    inline u2 PeekU2()
    {
        valid &= (buffer + 1 < buffer_tail);
        u2 i = buffer == buffer_tail ? 0 : (u1) *buffer;
        i = (i << 8) | (buffer + 1 == buffer_tail ? 0 : (u1) buffer[1]);
        return i;
    }
    inline u4 GetU4()
    {
        valid &= (buffer + 3 < buffer_tail);
        u4 i = buffer == buffer_tail ? 0 : (u1) *buffer++;
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        return i;
    }
    inline BaseLong GetU8()
    {
        valid &= (buffer + 7 < buffer_tail);
        BaseLong i = buffer == buffer_tail ? 0 : (u1) *buffer++;
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        i = (i << 8) | (buffer == buffer_tail ? 0 : (u1) *buffer++);
        return i;
    }

#ifdef JIKES_DEBUG
public:
    void Print()
    {
        unsigned i;
        if (! valid)
            Coutput << " *** Not a valid Java .class file! ***" << endl;

        Coutput << "Magic number: 0x";
        for (i = 32; i; i -= 4)
        {
            char c = (magic >> (i - 4)) & 0xf;
            Coutput << (char) (c < 10 ? c + '0' : c + 'a' - 10);
        }
        Coutput << ", version: " << major_version << '.' << minor_version
                << endl << endl << "### Constant pool: constant_pool_count: "
                << (unsigned) constant_pool.Length() << endl;
        for (i = 1; i < constant_pool.Length(); i++)
        {
            if (! constant_pool.Valid(i))
                continue;
            Coutput.width(5);
            Coutput << i << ": ";
            constant_pool[i] -> Print(constant_pool);
        }
        Coutput << endl << "### ";
        AccessFlags::Print();
        Coutput << endl << "this_class: " << (unsigned) this_class;
        if (constant_pool[this_class] -> Tag() == CPInfo::CONSTANT_Class)
        {
            Coutput << '=';
            constant_pool[this_class] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << endl << "super_class: " << (unsigned) super_class;
        if (! super_class)
            Coutput << "(none)";
        else if (constant_pool[super_class] -> Tag() == CPInfo::CONSTANT_Class)
        {
            Coutput << '=';
            constant_pool[super_class] -> Describe(constant_pool);
        }
        else Coutput << "(invalid)";
        Coutput << endl << "interfaces_count: "
                << (unsigned) interfaces.Length() << endl;
        for (i = 0; i < interfaces.Length(); i++)
        {
            Coutput << "  " << (unsigned) interfaces[i];
            if (constant_pool[interfaces[i]] -> Tag() ==
                CPInfo::CONSTANT_Class)
            {
                Coutput << '=';
                constant_pool[interfaces[i]] -> Describe(constant_pool);
            }
            else Coutput << "(invalid)";
            Coutput << endl;
        }
        Coutput << endl << "### Fields: fields_count: "
                << (unsigned) fields.Length() << endl;
        for (i = 0; i < fields.Length(); i++)
            fields[i] -> Print(constant_pool);
        Coutput << endl << "### Methods: methods_count: "
                << (unsigned) methods.Length() << endl;
        for (i = 0; i < methods.Length(); i++)
            methods[i] -> Print(constant_pool);
        Coutput << endl << "### Attributes: attributes_count: "
                << (unsigned) attributes.Length() << endl;
        for (i = 0; i < attributes.Length(); i++)
            attributes[i] -> Print(constant_pool);
    }
#endif // JIKES_DEBUG
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // class_INCLUDED

