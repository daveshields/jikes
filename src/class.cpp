// $Id: class.cpp,v 1.3 2004/01/31 17:16:02 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "class.h"
#include "code.h"
#include "control.h"
#include "op.h"
#include "option.h"
#include "semantic.h"
#include "zip.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


class ConstantPool::CPInvalid : public CPInfo
{
public:
    CPInvalid() : CPInfo(0) {}
    ~CPInvalid() {}
    virtual void Put(OutputBuffer&) const
    {
        assert(false && "trying to put invalid ConstantPool entry");
    }
    virtual bool Check(const ConstantPool&) const { return false; }

#ifdef JIKES_DEBUG
    virtual void Print(const ConstantPool&) const
    {
        Coutput << "invalid index" << endl;
    }

    virtual void Describe(const ConstantPool&) const
    {
        Coutput << "(invalid)";
    }
#endif // JIKES_DEBUG
};
CPInfo* ConstantPool::invalid = new CPInvalid();

void ConstantPool::SetNext(CPInfo* constant)
{
    Next() = constant;
    if (constant -> Large())
        Next() = NULL; // entries following Long or Double are unused
}

bool ConstantPool::Check() const
{
    for (unsigned i = 1; i < top; i++)
    {
        CPInfo* entry = Tuple<CPInfo*>::operator[](i);
        if (entry && ! entry -> Check(*this))
            return false;
    }
    return true;
}


CPInfo* CPInfo::AllocateCPInfo(ClassFile& buffer)
{
    u1 tag = buffer.GetU1();
    switch (tag)
    {
    case CONSTANT_Utf8:
        return new CPUtf8Info(buffer);
    case CONSTANT_Integer:
        return new CPIntegerInfo(buffer);
    case CONSTANT_Float:
        return new CPFloatInfo(buffer);
    case CONSTANT_Long:
        return new CPLongInfo(buffer);
    case CONSTANT_Double:
        return new CPDoubleInfo(buffer);
    case CONSTANT_Class:
        return new CPClassInfo(buffer);
    case CONSTANT_String:
        return new CPStringInfo(buffer);
    case CONSTANT_Fieldref:
    case CONSTANT_Methodref:
    case CONSTANT_InterfaceMethodref:
        return new CPMemberInfo(tag, buffer);
    case CONSTANT_NameAndType:
        return new CPNameAndTypeInfo(buffer);
    default:
        return new CPInfo(tag);
    }
}


CPClassInfo::CPClassInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_Class),
      name_index(buffer.GetU2()),
      type(NULL)
{}


CPMemberInfo::CPMemberInfo(u1 _tag, ClassFile& buffer)
    : CPInfo(_tag),
      class_index(buffer.GetU2()),
      name_and_type_index(buffer.GetU2())
{}


CPStringInfo::CPStringInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_String),
      string_index(buffer.GetU2())
{}


CPIntegerInfo::CPIntegerInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_Integer),
      bytes(buffer.GetU4())
{}


CPFloatInfo::CPFloatInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_Float),
      value(buffer.GetU4())
{}


CPLongInfo::CPLongInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_Long),
      value(buffer.GetU8())
{}


CPDoubleInfo::CPDoubleInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_Double),
      value(buffer.GetU8())
{}


CPNameAndTypeInfo::CPNameAndTypeInfo(ClassFile& buffer)
    : CPInfo(CONSTANT_NameAndType),
      name_index(buffer.GetU2()),
      descriptor_index(buffer.GetU2())
{}


CPUtf8Info::CPUtf8Info(ClassFile& buffer)
    : CPInfo(CONSTANT_Utf8),
      len(buffer.GetU2())
{
    int i;
    bytes = new u1[len];
    for (i = 0; i < len; i++)
        bytes[i] = buffer.GetU1();
    Init();
    if (! valid)
        buffer.MarkInvalid();
}

void CPUtf8Info::Init()
{
    const char* tmp;
    valid = true;
    for (u2 i = 0; i < len; i++)
    {
        switch (bytes[i])
        {
        case 0x00:
        case 0xf0: case 0xf1: case 0xf2: case 0xf3: case 0xf4: case 0xf5:
        case 0xf6: case 0xf7: case 0xf8: case 0xf9: case 0xfa: case 0xfb:
        case 0xfc: case 0xfd: case 0xfe: case 0xff: // invalid
            valid = false;
            contents.Next() = '\\';
            contents.Next() = 'x';
            tmp = IntToString(bytes[i], 2).String();
            contents.Next() = tmp[0];
            contents.Next() = tmp[1];
            break;
        case 0x09:
            contents.Next() = '\\';
            contents.Next() = 't';
            break;
        case 0x0a:
            contents.Next() = '\\';
            contents.Next() = 'n';
            break;
        case 0x0d:
            contents.Next() = '\\';
            contents.Next() = 'r';
            break;
        case 0x22:
            contents.Next() = '\\';
            contents.Next() = '"';
            break;
        case 0x5c:
            contents.Next() = '\\';
            contents.Next() = '\\';
            break;
        case 0x01: case 0x02: case 0x03: case 0x04: case 0x05: case 0x06:
        case 0x07: case 0x08: case 0x0b: case 0x0c: case 0x0e: case 0x0f:
        case 0x10: case 0x11: case 0x12: case 0x13: case 0x14: case 0x15:
        case 0x16: case 0x17: case 0x18: case 0x19: case 0x1a: case 0x1b:
        case 0x1c: case 0x1d: case 0x1e: case 0x1f: case 0x7f:
            // non-printing ASCII
            contents.Next() = '\\';
            contents.Next() = 'u';
            tmp = IntToString(bytes[i], 4).String();
            contents.Next() = tmp[0];
            contents.Next() = tmp[1];
            contents.Next() = tmp[2];
            contents.Next() = tmp[3];
            break;
        default:
            if (bytes[i] < 0x7f) // printing ASCII
                contents.Next() = bytes[i];
            else if (bytes[i] <= 0xdf) // 2-byte source
            {
                contents.Next() = '\\';
                if (i + 1 == len || (bytes[i + 1] & 0xc0) != 0x80)
                {
                    valid = false;
                    contents.Next() = 'x';
                    tmp = IntToString(bytes[i], 2).String();
                    contents.Next() = tmp[0];
                    contents.Next() = tmp[1];
                    break;
                }
                u2 value = (bytes[i] & 0x1f) << 6;
                value |= bytes[++i] & 0x3f;
                contents.Next() = 'u';
                tmp = IntToString(value, 4).String();
                contents.Next() = tmp[0];
                contents.Next() = tmp[1];
                contents.Next() = tmp[2];
                contents.Next() = tmp[3];
            }                
            else // 3-byte source
            {
                assert((bytes[i] & 0xf0) == 0xe0);
                contents.Next() = '\\';
                if (i + 2 >= len ||
                    (bytes[i + 1] & 0xc0) != 0x80 ||
                    (bytes[i + 2] & 0xc0) != 0x80)
                {
                    valid = false;
                    contents.Next() = 'x';
                    tmp = IntToString(bytes[i], 2).String();
                    contents.Next() = tmp[0];
                    contents.Next() = tmp[1];
                    break;
                }
                u2 value = (bytes[i] & 0x0f) << 12;
                value |= (bytes[++i] & 0x3f) << 6;
                value |= bytes[++i] & 0x3f;
                contents.Next() = 'u';
                tmp = IntToString(value, 4).String();
                contents.Next() = tmp[0];
                contents.Next() = tmp[1];
                contents.Next() = tmp[2];
                contents.Next() = tmp[3];
            }
        }
    }
    if (! valid)
    {
        contents.Next() = '\\';
        contents.Next() = 'i';
        contents.Next() = 'n';
        contents.Next() = 'v';
        contents.Next() = 'a';
        contents.Next() = 'l';
        contents.Next() = 'i';
        contents.Next() = 'd';
        contents.Next() = '\\';
    }
    contents.Next() = 0;
}


FieldInfo::FieldInfo(ClassFile& buffer)
    : AccessFlags(buffer.GetU2()),
      name_index(buffer.GetU2()),
      descriptor_index(buffer.GetU2()),
      attr_synthetic(NULL),
      attr_deprecated(NULL),
      attr_signature(NULL),
      attr_constantvalue(NULL)
{
    unsigned count = buffer.GetU2();
    while (count--)
    {
        AttributeInfo* attr = AttributeInfo::AllocateAttributeInfo(buffer);
        attributes.Next() = attr;
        switch (attr -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_Synthetic:
            if (attr_synthetic)
                buffer.MarkInvalid();
            attr_synthetic = (SyntheticAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Deprecated:
            if (attr_deprecated)
                buffer.MarkInvalid();
            attr_deprecated = (DeprecatedAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_ConstantValue:
            if (attr_constantvalue || ! ACC_FINAL())
                buffer.MarkInvalid();
            attr_constantvalue = (ConstantValueAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Signature:
            if (attr_signature)
                buffer.MarkInvalid();
            attr_signature = (SignatureAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Generic:
            // ignore
            break;
        default:
            // invalid field attribute
            buffer.MarkInvalid();
        }
    }
}


MethodInfo::MethodInfo(ClassFile& buffer)
    : AccessFlags(buffer.GetU2()),
      name_index(buffer.GetU2()),
      descriptor_index(buffer.GetU2()),
      attr_synthetic(NULL),
      attr_deprecated(NULL),
      attr_signature(NULL),
      attr_code(NULL),
      attr_exceptions(NULL)
{
    unsigned count = buffer.GetU2();
    while (count--)
    {
        AttributeInfo* attr = AttributeInfo::AllocateAttributeInfo(buffer);
        attributes.Next() = attr;
        switch (attr -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_Synthetic:
            if (attr_synthetic)
                buffer.MarkInvalid();
            attr_synthetic = (SyntheticAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Deprecated:
            if (attr_deprecated)
                buffer.MarkInvalid();
            attr_deprecated = (DeprecatedAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Code:
            if (attr_code || ACC_NATIVE() || ACC_ABSTRACT())
                buffer.MarkInvalid();
            attr_code = (CodeAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Signature:
            if (attr_signature)
                buffer.MarkInvalid();
            attr_signature = (SignatureAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Exceptions:
            if (attr_exceptions)
                buffer.MarkInvalid();
            attr_exceptions = (ExceptionsAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Generic:
            // ignore
            break;
        default:
            // invalid method attribute
            buffer.MarkInvalid();
        }
    }
    if (! ACC_NATIVE() && ! ACC_ABSTRACT() && ! attr_code)
        buffer.MarkInvalid();
}


AttributeInfo::AttributeInfo(AttributeInfoTag _tag, ClassFile& buffer)
    : tag(_tag),
      attribute_name_index(buffer.GetU2()),
      attribute_length(buffer.GetU4())
{
    if (tag == ATTRIBUTE_Generic)
    {
        info = new u1[attribute_length];
        for (unsigned i = 0; i < attribute_length; i++)
            info[i] = buffer.GetU1();
    }
    else info = NULL;
}

AttributeInfo::AttributeInfoTag AttributeInfo::Tag(const CPUtf8Info* name)
{
    switch (name -> Length())
    {
    case 4:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_Code))
            return ATTRIBUTE_Code;
        break;
    case 8:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_StackMap))
            return ATTRIBUTE_StackMap;
        break;
    case 9:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_Signature))
            return ATTRIBUTE_Signature;
        if (! strcmp(name -> Bytes(), StringConstant::U8S_Synthetic))
            return ATTRIBUTE_Synthetic;
        break;
    case 10:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_Deprecated))
            return ATTRIBUTE_Deprecated;
        if (! strcmp(name -> Bytes(), StringConstant::U8S_Exceptions))
            return ATTRIBUTE_Exceptions;
        if (! strcmp(name -> Bytes(), StringConstant::U8S_SourceFile))
            return ATTRIBUTE_SourceFile;
        break;
    case 12:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_InnerClasses))
            return ATTRIBUTE_InnerClasses;
        break;
    case 13:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_ConstantValue))
            return ATTRIBUTE_ConstantValue;
        break;
    case 15:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_LineNumberTable))
            return ATTRIBUTE_LineNumberTable;
        if (! strcmp(name -> Bytes(), StringConstant::U8S_EnclosingMethod))
            return ATTRIBUTE_EnclosingMethod;
        break;
    case 17:
        if (! strcmp(name -> Bytes(),
                     StringConstant::U8S_AnnotationDefault))
            return ATTRIBUTE_AnnotationDefault;
        break;
    case 18:
        if (! strcmp(name -> Bytes(), StringConstant::U8S_LocalVariableTable))
            return ATTRIBUTE_LocalVariableTable;
        break;
    case 22:
        if (! strcmp(name -> Bytes(),
                     StringConstant::U8S_LocalVariableTypeTable))
            return ATTRIBUTE_LocalVariableTypeTable;
        break;
    case 25:
        if (! strcmp(name -> Bytes(),
                     StringConstant::U8S_RuntimeVisibleAnnotations))
            return ATTRIBUTE_RuntimeVisibleAnnotations;
        break;
    case 27:
        if (! strcmp(name -> Bytes(),
                     StringConstant::U8S_RuntimeInvisibleAnnotations))
            return ATTRIBUTE_RuntimeInvisibleAnnotations;
        break;
    case 34:
        if (! strcmp(name -> Bytes(),
                     StringConstant::U8S_RuntimeVisibleParameterAnnotations))
            return ATTRIBUTE_RuntimeVisibleParameterAnnotations;
        break;
    case 36:
        if (! strcmp(name -> Bytes(),
                     StringConstant::U8S_RuntimeInvisibleParameterAnnotations))
            return ATTRIBUTE_RuntimeInvisibleParameterAnnotations;
    }
    return ATTRIBUTE_Generic;
}

AttributeInfo* AttributeInfo::AllocateAttributeInfo(ClassFile& buffer)
{
    u2 index = buffer.PeekU2();
    if (buffer.Pool()[index] -> Tag() != CPInfo::CONSTANT_Utf8)
        return new AttributeInfo(ATTRIBUTE_Generic, buffer);
    switch (Tag((CPUtf8Info*) buffer.Pool()[index]))
    {
    case ATTRIBUTE_ConstantValue:
        return new ConstantValueAttribute(buffer);
    case ATTRIBUTE_Code:
        return new CodeAttribute(buffer);
    case ATTRIBUTE_Exceptions:
        return new ExceptionsAttribute(buffer);
    case ATTRIBUTE_InnerClasses:
        return new InnerClassesAttribute(buffer);
    case ATTRIBUTE_SourceFile:
        return new SourceFileAttribute(buffer);
    case ATTRIBUTE_Synthetic:
        return new SyntheticAttribute(buffer);
    case ATTRIBUTE_LineNumberTable:
        return new LineNumberTableAttribute(buffer);
    case ATTRIBUTE_LocalVariableTable:
        return new LocalVariableTableAttribute(buffer);
    case ATTRIBUTE_Deprecated:
        return new DeprecatedAttribute(buffer);
    case ATTRIBUTE_Signature:
        return new SignatureAttribute(buffer);
    case ATTRIBUTE_StackMap:
        return new StackMapAttribute(buffer);
    default:
        return new AttributeInfo(ATTRIBUTE_Generic, buffer);
    }
}


ConstantValueAttribute::ConstantValueAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_ConstantValue, buffer),
      constantvalue_index(buffer.GetU2())
{
    if (attribute_length != 2 ||
        ! buffer.Pool()[constantvalue_index] -> Constant())
    {
        buffer.MarkInvalid();
    }
}


CodeAttribute::CodeAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_Code, buffer),
      max_stack(buffer.GetU2()),
      max_locals(buffer.GetU2()),
      code(8, 4),
      exception_table(6, 16),
      attributes(6, 16),
      attr_linenumbers(NULL),
      attr_locals(NULL),
      attr_stackmap(NULL)
{
    unsigned remaining = attribute_length - 12;
    // +2 for max_stack, +2 for max_locals, +4 for code_length,
    // +2 for exception_table_length, +2 for attributes_count
    u4 code_length = buffer.GetU4();
    remaining -= code_length;
    while (code_length--)
        code.Next() = buffer.GetU1();

    u2 exception_table_length = buffer.GetU2();
    remaining -= exception_table_length * 8;
    while (exception_table_length--)
    {
        ExceptionElement& entry = exception_table.Next();
        entry.start_pc = buffer.GetU2();
        entry.end_pc = buffer.GetU2();
        entry.handler_pc = buffer.GetU2();
        entry.catch_type = buffer.GetU2();
        if (entry.catch_type &&
            buffer.Pool()[entry.catch_type] -> Tag() != CPInfo::CONSTANT_Class)
        {
            buffer.MarkInvalid();
        }
    }

    u2 attributes_count = buffer.GetU2();
    while (attributes_count--)
    {
        AttributeInfo* attr = AllocateAttributeInfo(buffer);
        remaining -= 6 + attr -> AttributeLength();
        attributes.Next() = attr;
        switch (attr -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_LineNumberTable:
            if (attr_linenumbers)
                buffer.MarkInvalid();
            attr_linenumbers = (LineNumberTableAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_LocalVariableTable:
            if (attr_locals)
                buffer.MarkInvalid();
            attr_locals = (LocalVariableTableAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_StackMap:
            if (attr_stackmap)
                buffer.MarkInvalid();
            attr_stackmap = (StackMapAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Generic:
            // ignore
            break;
        default:
            // invalid code attribute
            buffer.MarkInvalid();
        }
    }
    if (remaining)
        buffer.MarkInvalid();
}


#ifdef JIKES_DEBUG
void CodeAttribute::Print(const ConstantPool& constant_pool, int fill) const
{
    assert(! fill);
    PrintPrefix("Code", constant_pool, 0);
    Coutput << endl << " max_stack: " << (unsigned) max_stack
            << ", max_locals: " << (unsigned) max_locals
            << ", code_length: " << (unsigned) code.Length() << endl;
    Operators::OpDmp(constant_pool, code);
    Coutput << " exception_table_length: "
            << (unsigned) exception_table.Length() << endl;
    for (unsigned i = 0; i < exception_table.Length(); i++)
    {
        Coutput << "  start " << (unsigned) exception_table[i].start_pc
                << ", end " << (unsigned) exception_table[i].end_pc
                << ", handler " << (unsigned) exception_table[i].handler_pc
                << ", catch_type " << (unsigned) exception_table[i].catch_type;
        if (! exception_table[i].catch_type)
            Coutput << " (all)";
        else if (constant_pool[exception_table[i].catch_type] -> Tag() ==
                 CPInfo::CONSTANT_Class)
        {
            Coutput << '=';
            constant_pool[exception_table[i].catch_type] ->
                Describe(constant_pool);
        }
        else Coutput << "(invalid)";                    
        Coutput << endl;
    }
    Coutput << " attribute_count: " << (unsigned) attributes.Length() << endl;
    for (unsigned j = 0; j < attributes.Length(); j++)
        attributes[j] -> Print(constant_pool, 2);
}
#endif // JIKES_DEBUG


ExceptionsAttribute::ExceptionsAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_Exceptions, buffer),
      exception_index_table(6, 16)
{
    unsigned count = buffer.GetU2();
    if (attribute_length != count * 2 + 2)
        buffer.MarkInvalid();
    while (count--)
    {
        u2 index = buffer.GetU2();
        exception_index_table.Next() = index;
        if (buffer.Pool()[index] -> Tag() != CPInfo::CONSTANT_Class)
            buffer.MarkInvalid();
    }
}


InnerClassesAttribute::InnerClassesAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_InnerClasses, buffer),
      classes(6, 16)
{
    unsigned count = buffer.GetU2();
    if (attribute_length != count * 8 + 2)
        buffer.MarkInvalid();
    while (count--)
    {
        InnerClassesElement& entry = classes.Next();
        entry.inner_class_info_index = buffer.GetU2();
        entry.outer_class_info_index = buffer.GetU2();
        entry.inner_name_index = buffer.GetU2();
        entry.inner_class_access_flags.SetFlags(buffer.GetU2());
        if ((buffer.Pool()[entry.inner_class_info_index] -> Tag() !=
             CPInfo::CONSTANT_Class) ||
            (entry.outer_class_info_index &&
             (buffer.Pool()[entry.outer_class_info_index] -> Tag() !=
              CPInfo::CONSTANT_Class)) ||
            (entry.inner_name_index &&
             (buffer.Pool()[entry.inner_name_index] -> Tag() !=
              CPInfo::CONSTANT_Utf8)) ||
            ! entry.inner_class_access_flags.LegalAccess())
        {
            buffer.MarkInvalid();
        }
    }
}


SyntheticAttribute::SyntheticAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_Synthetic, buffer)
{
    if (attribute_length)
        buffer.MarkInvalid();
}


SourceFileAttribute::SourceFileAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_SourceFile, buffer),
      sourcefile_index(buffer.GetU2())
{
    if (attribute_length != 2 ||
        buffer.Pool()[sourcefile_index] -> Tag() != CPInfo::CONSTANT_Utf8)
    {
        buffer.MarkInvalid();
    }
}


LineNumberTableAttribute::LineNumberTableAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_LineNumberTable, buffer),
      line_number_table(6, 16)
{
    unsigned count = buffer.GetU2();
    if(attribute_length != count * 4 + 2)
        buffer.MarkInvalid();
    while (count--)
    {
        LineNumberElement& entry = line_number_table.Next();
        entry.start_pc = buffer.GetU2();
        entry.line_number = buffer.GetU2();
    }
}        


LocalVariableTableAttribute::LocalVariableTableAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_LocalVariableTable, buffer),
      local_variable_table(6, 16)
{
    unsigned count = buffer.GetU2();
    if (attribute_length != count * 10 + 2)
        buffer.MarkInvalid();
    while (count--)
    {
        LocalVariableElement& entry = local_variable_table.Next();
        entry.start_pc = buffer.GetU2();
        entry.length = buffer.GetU2();
        entry.name_index = buffer.GetU2();
        entry.descriptor_index = buffer.GetU2();
        entry.index = buffer.GetU2();
        if ((buffer.Pool()[entry.name_index] -> Tag() !=
             CPInfo::CONSTANT_Utf8) ||
            (buffer.Pool()[entry.descriptor_index] -> Tag() !=
             CPInfo::CONSTANT_Utf8))
        {
            buffer.MarkInvalid();
        }
    }
}


DeprecatedAttribute::DeprecatedAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_Deprecated, buffer)
{
    if (attribute_length)
        buffer.MarkInvalid();
}


SignatureAttribute::SignatureAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_Signature, buffer),
      signature_index(buffer.GetU2())
{
    if (attribute_length != 2 ||
        buffer.Pool()[signature_index] -> Tag() != CPInfo::CONSTANT_Utf8)
    {
        buffer.MarkInvalid();
    }
}


void StackMapAttribute::StackMapFrame::VerificationTypeInfo::Read(ClassFile& buffer)
{
    int type = buffer.GetU1();
    tag = (VerificationTypeInfoTag) type;
    if (type < TYPE_Top || type > TYPE_Uninitialized)
    {
        type = TYPE_Top;
        buffer.MarkInvalid();
    }
    if (type >= TYPE_Object)
    {
        info = buffer.GetU2();
        if (type == TYPE_Object &&
            buffer.Pool()[info] -> Tag() != CPInfo::CONSTANT_Class)
        {
            buffer.MarkInvalid();
        }
    }
}

StackMapAttribute::StackMapFrame::StackMapFrame(ClassFile& buffer)
    : offset(buffer.GetU2()),
      locals(6, 16),
      stack(6, 16),
      frame_size(6)
    // +2 for offset, +2 for locals_size, +2 for stack_size
{
    unsigned count = buffer.GetU2();
    while (count--)
    {
        unsigned index = locals.NextIndex();
        locals[index].Read(buffer);
        frame_size += locals[index].Size();
    }
    count = buffer.GetU2();
    while (count--)
    {
        unsigned index = stack.NextIndex();
        stack[index].Read(buffer);
        frame_size += stack[index].Size();
    }
}

StackMapAttribute::StackMapAttribute(ClassFile& buffer)
    : AttributeInfo(ATTRIBUTE_StackMap, buffer),
      frames(6, 16)
{
    unsigned remaining = attribute_length - 2; // -2 for frame_count
    unsigned count = buffer.GetU2();
    while (count--)
    {
        unsigned index = frames.NextIndex();
        frames[index] = new StackMapFrame(buffer);
        remaining -= frames[index] -> FrameSize();
    }
    if (remaining)
        buffer.MarkInvalid();
}


ClassFile::ClassFile(const char* buf, unsigned buf_size)
    : valid(true),
      buffer(buf),
      buffer_tail(buf + buf_size),
      magic(GetU4()),
      minor_version(GetU2()),
      major_version(GetU2()),
      constant_pool(8, 4),
      interfaces(6, 16),
      fields(6, 16),
      methods(6, 16),
      attributes(6, 16),
      attr_synthetic(NULL),
      attr_deprecated(NULL),
      attr_signature(NULL),
      attr_sourcefile(NULL),
      attr_innerclasses(NULL)
{
    if (magic != MAGIC || major_version < 45) // Unknown class format.
        valid = false;
    u2 count = GetU2();
    if (! count)
        valid = false;
    else
    {
        while (--count) // skip entry 0
        {
            CPInfo* entry = CPInfo::AllocateCPInfo(*this);
            if (entry -> Large())
                count--; // skip next entry for eight-byte constants
            constant_pool.SetNext(entry);
        }
    }
    if (! constant_pool.Check())
        valid = false;
    access_flags = GetU2();
    if (! LegalAccess())
        valid = false;
    this_class = GetU2();
    if (constant_pool[this_class] -> Tag() != CPInfo::CONSTANT_Class)
        valid = false;
    super_class = GetU2();
    if (super_class &&
        constant_pool[super_class] -> Tag() != CPInfo::CONSTANT_Class)
    {
        valid = false;
    }
    count = GetU2();
    while (count--)
    {
        u2 inter = GetU2();
        if (constant_pool[inter] -> Tag() != CPInfo::CONSTANT_Class)
            valid = false;
        interfaces.Next() = inter;
    }

    count = GetU2();
    while (count--)
        fields.Next() = new FieldInfo(*this);
    count = GetU2();
    while (count--)
        methods.Next() = new MethodInfo(*this);
    count = GetU2();
    while (count--)
    {
        AttributeInfo* attr = AttributeInfo::AllocateAttributeInfo(*this);
        attributes.Next() = attr;
        switch (attr -> Tag())
        {
        case AttributeInfo::ATTRIBUTE_Synthetic:
            if (attr_synthetic)
                valid = false;
            attr_synthetic = (SyntheticAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Deprecated:
            if (attr_deprecated)
                valid = false;
            attr_deprecated = (DeprecatedAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Signature:
            if (attr_signature)
                valid = false;
            attr_signature = (SignatureAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_SourceFile:
            if (attr_sourcefile)
                valid = false;
            attr_sourcefile = (SourceFileAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_InnerClasses:
            if (attr_innerclasses)
                valid = false;
            attr_innerclasses = (InnerClassesAttribute*) attr;
            break;
        case AttributeInfo::ATTRIBUTE_Generic:
            // ignore
            break;
        default:
            // invalid method attribute
            valid = false;
        }
    }
}          

void ClassFile::Write(TypeSymbol* unit_type) const
{
    Semantic* sem = unit_type -> semantic_environment -> sem;
    Control& control = sem -> control;
    OutputBuffer output_buffer;

    const char* class_file_name = unit_type -> ClassName();
    if (control.option.verbose)
        Coutput << "[write " << class_file_name << "]" << endl;
    assert (valid);
    if (control.option.nowrite)
        return;

    unsigned i;
    output_buffer.PutU4(MAGIC);
    output_buffer.PutU2(minor_version);
    output_buffer.PutU2(major_version);

    output_buffer.PutU2(constant_pool.Length());
    assert(constant_pool.Check());
    for (i = 1; i < constant_pool.Length(); i++)
    {
        constant_pool[i] -> Put(output_buffer);
        if (constant_pool[i] -> Large())
            i++; // skip the next entry for eight-byte constants
    }

    output_buffer.PutU2(access_flags);
    output_buffer.PutU2(this_class);
    output_buffer.PutU2(super_class);
    output_buffer.PutU2(interfaces.Length());
    for (i = 0; i < interfaces.Length(); i++)
        output_buffer.PutU2(interfaces[i]);
    output_buffer.PutU2(fields.Length());
    for (i = 0; i < fields.Length(); i++)
        fields[i] -> Put(output_buffer);
    output_buffer.PutU2(methods.Length());
    for (i = 0; i < methods.Length(); i++)
        methods[i] -> Put(output_buffer);
    output_buffer.PutU2(attributes.Length());
    for (i = 0; i < attributes.Length(); i++)
        attributes[i] -> Put(output_buffer);

    // Now output to file
    if (! output_buffer.WriteToFile(class_file_name))
    {
        int length = strlen(class_file_name);
        wchar_t* name = new wchar_t[length + 1];
        for (int j = 0; j < length; j++)
            name[j] = class_file_name[j];
        name[length] = U_NULL;

        sem -> ReportSemError(SemanticError::CANNOT_WRITE_FILE,
                              unit_type -> declaration -> LeftToken(),
                              unit_type -> declaration -> RightToken(),
                              name);
        delete [] name;
    }
}


//
// This processes a descriptor, and returns the associated type, or else
// control.no_type if the descriptor is bad. Signature is assumed to be null
// terminated, and this adjusts the pointer to the spot just after the type
// parsed.
//
TypeSymbol* Semantic::ProcessSignature(TypeSymbol* base_type,
                                       const char*& signature,
                                       LexStream::TokenIndex tok)
{
    TypeSymbol* type;
    int num_dimensions = 0;
    for ( ; *signature == U_LEFT_BRACKET; signature++)
        num_dimensions++;
    switch (*signature++)
    {
    case U_B:
        type = control.byte_type;
        break;
    case U_C:
        type = control.char_type;
        break;
    case U_D:
        type = control.double_type;
        break;
    case U_F:
        type = control.float_type;
        break;
    case U_I:
        type = control.int_type;
        break;
    case U_J:
        type = control.long_type;
        break;
    case U_S:
        type = control.short_type;
        break;
    case U_Z:
        type = control.boolean_type;
        break;
    case U_V:
        if (num_dimensions)
            return control.no_type;
        type = control.void_type;
        break;
    case U_L:
        {
            const char* str = signature;
            while (*str && *str != U_SEMICOLON)
                str++;
            if (! *str)
            {
                signature = str;
                return control.no_type;
            }
            type = ReadTypeFromSignature(base_type, signature,
                                         str - signature, tok);
            signature = str + 1;
        }
        break;
    case U_T:
        assert(false && "generics not implemented yet");
    case U_NULL: // oops, already exceeded string
        signature--;
        // fallthrough
    default:
        return control.no_type;
    }
    return type -> GetArrayType(this, num_dimensions);
}

//
// This returns the type associated with a CONSTANT_Class entry, or
// control.no_type if there was an error. Unless the type is bad, it is
// necessarily a subtype of java.lang.Object.
//
TypeSymbol* Semantic::GetType(TypeSymbol* base_type, CPClassInfo* class_info,
                              const ConstantPool& constant_pool,
                              LexStream::TokenIndex tok)
{
    if (! class_info -> Type())
    {
        const char* str = class_info -> TypeName(constant_pool);
        if (*str == U_LEFT_BRACKET)
            class_info -> SetType(ProcessSignature(base_type, str, tok));
        else
        {
            u2 length = class_info -> TypeNameLength(constant_pool);
            class_info -> SetType(ReadTypeFromSignature(base_type, str,
                                                        length, tok));
        }
    }
    return class_info -> Type();
}

//
// Searches for a nested type with the simple name name_symbol located within
// base_type, when base_type was loaded from a .class file.
//
TypeSymbol* Semantic::ProcessNestedType(TypeSymbol* base_type,
                                        NameSymbol* name_symbol,
                                        LexStream::TokenIndex tok)
{
    TypeSymbol* inner_type = base_type -> FindTypeSymbol(name_symbol);
    if (! inner_type)
    {
        int length = base_type -> ExternalNameLength() + 1 +
            name_symbol -> NameLength(); // +1 for $,... +1 for $
        wchar_t* external_name = new wchar_t[length + 1]; // +1 for '\0';
        wcscpy(external_name, base_type -> ExternalName());
        wcscat(external_name, StringConstant::US_DS);
        wcscat(external_name, name_symbol -> Name());
        NameSymbol* external_name_symbol =
            control.FindOrInsertName(external_name, length);

        delete [] external_name;

        inner_type = base_type -> InsertNestedTypeSymbol(name_symbol);
        inner_type -> outermost_type = base_type -> outermost_type;
        inner_type -> supertypes_closure = new SymbolSet;
        inner_type -> subtypes = new SymbolSet;
        inner_type -> SetExternalIdentity(external_name_symbol);
        inner_type -> SetOwner(base_type);
        inner_type -> SetSignature(control);

        FileSymbol* file_symbol =
            Control::GetFile(control, base_type -> ContainingPackage(),
                             external_name_symbol);
        if (file_symbol)
        {
            inner_type -> file_symbol = file_symbol;
            inner_type -> SetLocation();

            ReadClassFile(inner_type, tok);
        }
        else
        {
            // this symbol table will only contain a default constructor
            inner_type -> SetSymbolTable(1);
            inner_type -> super = control.Object();
            inner_type -> MarkBad();
            AddDefaultConstructor(inner_type);
            ReportSemError(SemanticError::TYPE_NOT_FOUND, tok,
                           inner_type -> ContainingPackageName(),
                           inner_type -> ExternalName());
        }
    }

    return inner_type;
}

//
// Retrieves the innermost nested type from a signature containing '$' as
// the type nesting separator.
//
TypeSymbol* Semantic::RetrieveNestedTypes(TypeSymbol* base_type,
                                          wchar_t* signature,
                                          LexStream::TokenIndex tok)
{
    int len;
    for (len = 0;
         signature[len] != U_NULL && signature[len] != U_DOLLAR; len++)
        ;
    NameSymbol* name_symbol = control.FindOrInsertName(signature, len);
    TypeSymbol* inner_type = ProcessNestedType(base_type, name_symbol, tok);

    return (signature[len] == U_DOLLAR
            ? RetrieveNestedTypes(inner_type, &signature[len + 1], tok)
            : inner_type);
}

//
// Given a UTF8 signature, this finds the type that it refers to, loading
// the type into memory or causing a type not found error.
//
TypeSymbol* Semantic::ReadTypeFromSignature(TypeSymbol* base_type,
                                            const char* utf8_signature,
                                            int length,
                                            LexStream::TokenIndex tok)
{
    TypeSymbol* type = control.type_table.FindType(utf8_signature, length);

    if (type)
    {
        if (type -> SourcePending())
            control.ProcessHeaders(type -> file_symbol);
    }
    else
    {
        wchar_t* signature = new wchar_t[length + 1];
        Control::ConvertUtf8ToUnicode(signature, utf8_signature, length);

        int total_length;
        for (total_length = 0;
             signature[total_length] != U_NULL &&
                 signature[total_length] != U_DOLLAR; total_length++)
            ;

        if (signature[total_length] != U_NULL &&
            Code::IsDigit(signature[total_length + 1]))
        {
            // an anonymous or a local type?
            for (total_length += 2;
                 Code::IsDigit(signature[total_length]); total_length++)
                // will stop at next '$' or '\0' !!!
                ;
            if (signature[total_length] != U_NULL)
            {
                // not an anonymous type? then scan local type name
                for (total_length++;
                     signature[total_length] != U_NULL &&
                         signature[total_length] != U_DOLLAR; total_length++)
                    ;
            }
        }

        int len;
        for (len = total_length - 1;
             len >= 0 && signature[len] != U_SLASH; len--)
            ;

        wchar_t* name = &(signature[len + 1]);

        //
        // When a package name is specified in the signature, we look for the
        // type in question in that package, i.e., we redefine package.
        // Otherwise, we search for the type in the unnamed package.
        //
        PackageSymbol* package = NULL;

        //
        // Which package?
        //
        if (len >= 0)
        {
            wchar_t* package_name = new wchar_t[len + 1];
            for (int i = 0; i < len; i++)
                package_name[i] = signature[i];
            package_name[len] = U_NULL;
            package = control.ProcessPackage(package_name);

            if (package -> directory.Length() == 0)
            {
                ReportSemError(SemanticError::PACKAGE_NOT_FOUND,
                               tok,
                               tok,
                               package -> PackageName());
            }
            delete [] package_name;
        }
        else package = control.unnamed_package;

        //
        // Process type
        //
        NameSymbol* name_symbol =
            control.FindOrInsertName(name, total_length - (len + 1));
        type = package -> FindTypeSymbol(name_symbol);
        if (type)
        {
            if (type -> SourcePending())
                control.ProcessHeaders(type -> file_symbol);
        }
        else
        {
            FileSymbol* file_symbol = Control::GetFile(control, package,
                                                       name_symbol);
            //
            // If a file_symbol was not found, ReadType will issue an error
            // message
            //
            type = ReadType(file_symbol, package, name_symbol, tok);

            //
            // If we have to do a full check and we have a case where a
            // ".class" file depends on a ".java" file then we should signal
            // that the ".java" file associated with the ".class" file should
            // be recompiled.
            //
            if (control.option.full_check && ! control.option.depend &&
                file_symbol && file_symbol -> IsJava() &&
                ! file_symbol -> IsZip())
            {
                control.recompilation_file_set.AddElement(file_symbol);
                if (! control.option.incremental && control.option.pedantic)
                {
                    ReportSemError(SemanticError::RECOMPILATION, tok,
                                   base_type -> ContainingPackageName(),
                                   base_type -> ExternalName(),
                                   type -> ContainingPackageName(),
                                   type -> ExternalName());
                }
            }
        }

        if (signature[total_length] == U_DOLLAR)
            type = RetrieveNestedTypes(type, &signature[total_length + 1],
                                       tok);

        delete [] signature;
    }

    //
    // Establish a dependence from base_type (read from a class file) to type.
    //
    AddDependence(base_type, type);

    return type;
}

//
// This is called when a type needs to be read from a .class file. It reads
// the file and fills in the symbol table of type.
//
void Semantic::ReadClassFile(TypeSymbol* type, LexStream::TokenIndex tok)
{
#ifdef JIKES_DEBUG
    control.class_files_read++;
#endif

    FileSymbol* file_symbol = type -> file_symbol;

    if (control.option.verbose)  {
        Coutput << "[read "
                << file_symbol -> FileName()
                << "]" << endl;
    }

    if (file_symbol -> IsZip())
    {
        ZipFile* zipfile = new ZipFile(file_symbol);

        if (zipfile -> Buffer() == NULL)
        {
            // this symbol table will only contain a default constructor
            type -> SetSymbolTable(1);
            if (type != control.Object())
                type -> super = (type == control.Throwable()
                                 ? control.Object() : control.Throwable());
            type -> MarkBad();
            AddDefaultConstructor(type);

            ReportSemError(SemanticError::COMPRESSED_ZIP_FILE, tok,
                           file_symbol -> PathSym() -> Name(),
                           type -> ContainingPackageName(),
                           type -> ExternalName());
        }
        else ProcessClassFile(type, zipfile -> Buffer(),
                              file_symbol -> uncompressed_size, tok);
        delete zipfile;
    }
    else
    {
        // Get a ReadObject from the API that contains the file's data.
        JikesAPI::FileReader* classFile =
            JikesAPI::getInstance() -> read(file_symbol -> FileName());
        if (classFile == NULL)
        {
            // this symbol table will only contain a default constructor
            type -> SetSymbolTable(1);
            if (type != control.Object())
                type -> super = (type == control.Throwable()
                                 ? control.Object() : control.Throwable());
            type -> MarkBad();
            AddDefaultConstructor(type);

            ReportSemError(SemanticError::CANNOT_OPEN_CLASS_FILE, tok,
                           type -> ContainingPackageName(),
                           type -> ExternalName());
        }

        else
        {
            // Process the file data.
            unsigned size = classFile -> getBufferSize();

#if defined(WIN32_FILE_SYSTEM)
            size = ((0xFFFFFFFF && GetLastError()) != NO_ERROR) ? 0 : size;
#endif
            ProcessClassFile(type, classFile -> getBuffer(), size, tok);
            delete classFile;
        }
    }
}

//
// Attempts to read in a type from a buffer representing a .class file.
//
void Semantic::ProcessClassFile(TypeSymbol* type, const char* buffer,
                                unsigned buffer_size,
                                LexStream::TokenIndex tok)
{
    assert(! type -> HeaderProcessed());
    ClassFile* class_data = new ClassFile(buffer, buffer_size);
    if (! class_data -> Valid())
    {
        ReportSemError(SemanticError::INVALID_CLASS_FILE, tok,
                       type -> file_symbol -> PathSym() -> Name(),
                       type -> ExternalName(),
                       type -> ContainingPackageName());
        type -> MarkBad();
        delete class_data;
        return;
    }

    //
    // See if we read the expected type.
    //
    const ConstantPool& constant_pool = class_data -> Pool();
    CPClassInfo* class_info = class_data -> ThisClass();
    if (strcmp(type -> fully_qualified_name -> value,
               class_info -> TypeName(constant_pool)))
    {
        wchar_t* str =
            new wchar_t[class_info -> TypeNameLength(constant_pool) + 1];
        control.
            ConvertUtf8ToUnicode(str, class_info -> TypeName(constant_pool), 
                                 class_info -> TypeNameLength(constant_pool));
        // Passing NULL as the filename allows the error format to use the
        // platform dependent directory delimiter.
        ReportSemError(SemanticError::WRONG_TYPE_IN_CLASSFILE, tok, tok,
                       type -> ExternalName(),
                       type -> file_symbol -> PathSym() -> Name(),
                       NULL, str);
        type -> MarkBad();
        delete [] str;
        delete class_data;
        return;
    }
                       
    type -> MarkHeaderProcessed();
    type -> MarkConstructorMembersProcessed();
    type -> MarkMethodMembersProcessed();
    type -> MarkFieldMembersProcessed();
    type -> MarkLocalClassProcessingCompleted();
    type -> MarkSourceNoLongerPending();
                       
    //
    // Since the unnamed packages includes all directories on the CLASSPATH,
    // check that types are not duplicated between directories.
    //
    if (! type -> IsNested() &&
        type -> ContainingPackage() == control.unnamed_package)
    {
        TypeSymbol* old_type = (TypeSymbol*)
            control.unnamed_package_types.Image(type -> Identity());
        if (! old_type)
            control.unnamed_package_types.AddElement(type);
        else
        {
            ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                           tok, tok, type -> Name(),
                           old_type -> FileLoc());
        }
    }

    //
    // On systems such as NT and Win-95 that are not case-sensitive,
    // we need to confirm that the type name specified matches the name
    // in the class file.
    // TODO: Is this necessary, or did the previous check filter this?
//      assert(type_name_length - (n + 1) == type -> ExternalNameLength());
//      int i;
//      for (i = 0; i < type -> ExternalNameLength(); i++)
//      {
//          if (type_name[n + 1 + i] != type -> ExternalName()[i])
//              break;
//      }
//      if (i < type -> ExternalNameLength())
//      {
//          wchar_t* name = new wchar_t[type_name_length + 1];
//          for (int k = 0; k < type_name_length; k++)
//              name[k] = type_name[k];
//          name[type_name_length] = U_NULL;
//          ReportSemError(SemanticError::TYPE_NAME_MISMATCH, tok,
//                         type -> ContainingPackageName(),
//                         type -> ExternalName(), name);
//          delete [] name;
//      }

    //
    // We now have enough information to make a good estimate for the
    // size of the symbol table we need for this class.
    //
    int i;
    const InnerClassesAttribute* inner_classes = class_data -> InnerClasses();
    type -> SetSymbolTable(class_data -> FieldsCount() +
                           class_data -> MethodsCount() +
                           (inner_classes
                            ? inner_classes -> InnerClassesLength() : 0));

    //
    // Read the attributes. We do this now to learn about inner classes and
    // type parameterization, since this information may be needed when
    // processing supertypes, field signatures, and method signatures. Also,
    // we learn the access flags of this type.
    //
    type -> SetFlags(class_data -> Flags());
    type -> ResetACC_SUPER();
    if (class_data -> Synthetic())
        type -> MarkSynthetic();
    if (class_data -> Deprecated())
        type -> MarkDeprecated();
    if (inner_classes)
    {
        for (i = inner_classes -> InnerClassesLength() - 1; i >= 0; i--)
        {
            const CPClassInfo* inner =
                inner_classes -> Inner(i, constant_pool);
            const CPClassInfo* outer =
                inner_classes -> Outer(i, constant_pool);
            if (inner == class_info)
            {
                type -> SetFlags(inner_classes -> Flags(i));
                if (! outer)
                {
                    // TODO: Make this nicer?
                    // This type is local or anonymous, so we shouldn't be
                    // reading it from the .class file. Marking it synthetic
                    // works to make it unusable (but the error message is not
                    // the best).
                    //
                    type -> MarkSynthetic();
                }
            }
            else if (outer == class_info &&
                     inner_classes -> Name(i, constant_pool) &&
                     inner_classes -> NameLength(i, constant_pool))
            {
                //
                // Some idiot compilers give anonymous classes the name "" and
                // an outer class, rather than obeying JVMS 4.7.5.  For
                // example, mail.jar in Sun's javamail 1.3.1 includes
                // javax.mail.Service with this property. The check for
                // NameLength above skips those invalid entries.
                //
                type -> AddNestedTypeSignature((inner_classes ->
                                                Name(i, constant_pool)),
                                               (inner_classes ->
                                                NameLength(i, constant_pool)));
            }
        }
    }
    if (control.option.full_check &&
        (control.option.unzip || ! type -> file_symbol -> IsZip()))
    {
        type -> ProcessNestedTypeSignatures(this, tok);
    }

    //
    // Tie this type to its supertypes.
    //
    class_info = class_data -> SuperClass();
    if ((type == control.Object()) != (class_info == NULL))
        type -> MarkBad();
    if (class_info)
    {
        type -> super = GetType(type, class_info, constant_pool, tok);
        if (type -> super -> ACC_INTERFACE() || type -> super -> IsArray() ||
            type -> super -> Bad())
        {
            type -> MarkBad();
        }
        else
        {
            type -> supertypes_closure -> AddElement(type -> super);
            type -> supertypes_closure -> Union(*type -> super ->
                                                supertypes_closure);
        }
    }
    for (i = class_data -> InterfacesCount() - 1; i >= 0; i--)
    {
        class_info = class_data -> Interface(i);
        assert(class_info);
        TypeSymbol* interf = GetType(type, class_info, constant_pool, tok);
        type -> AddInterface(interf);
        type -> supertypes_closure -> AddElement(interf);
        type -> supertypes_closure -> Union(*interf -> supertypes_closure);
        if (! interf -> ACC_INTERFACE())
            type -> MarkBad();
    }

    //
    // Read the fields.
    //
    for (i = class_data -> FieldsCount() - 1; i >= 0; i--)
    {
        const FieldInfo* field = class_data -> Field(i);
        if (field -> Synthetic())
            continue; // No point reading these - the user can't access them.
        NameSymbol* name_symbol =
            control.ConvertUtf8ToUnicode(field -> Name(constant_pool),
                                         field -> NameLength(constant_pool));
        VariableSymbol* symbol = new VariableSymbol(name_symbol);
        symbol -> SetOwner(type);
        symbol -> MarkComplete();
        symbol -> MarkInitialized();
        symbol -> SetFlags(field -> Flags());
        symbol -> SetSignatureString(field -> Signature(constant_pool),
                                     field -> SignatureLength(constant_pool));
        if (field -> Deprecated())
            symbol -> MarkDeprecated();
        const CPInfo* value = field -> ConstantValue(constant_pool);
        if (value)
        {
            switch (value -> Tag())
            {
            case CPInfo::CONSTANT_Integer:
                symbol -> initial_value = control.int_pool.
                    FindOrInsert(((CPIntegerInfo*) value) -> Value());
                break;
            case CPInfo::CONSTANT_Long:
                symbol -> initial_value = control.long_pool.
                    FindOrInsert(((CPLongInfo*) value) -> Value());
                break;
            case CPInfo::CONSTANT_Float:
                symbol -> initial_value = control.float_pool.
                    FindOrInsert(((CPFloatInfo*) value) -> Value());
                break;
            case CPInfo::CONSTANT_Double:
                symbol -> initial_value = control.double_pool.
                    FindOrInsert(((CPDoubleInfo*) value) -> Value());
                break;
            case CPInfo::CONSTANT_String:
                {
                    CPStringInfo* str_value = (CPStringInfo*) value;
                    symbol -> initial_value = control.Utf8_pool.
                        FindOrInsert(str_value -> Bytes(constant_pool),
                                     str_value -> Length(constant_pool));
                }
                break;
            default:
                assert(false && "unexpected constant pool entry");
            }
        }
        type -> InsertVariableSymbol(symbol);
        if (control.option.full_check &&
            (control.option.unzip || ! type -> file_symbol -> IsZip()))
        {
            symbol -> ProcessVariableSignature(this, tok);
        }
    }

    //
    // Read the methods.
    //
    for (i = class_data -> MethodsCount() - 1; i >= 0; i--)
    {
        const MethodInfo* method = class_data -> Method(i);
        NameSymbol* name_symbol =
            control.ConvertUtf8ToUnicode(method -> Name(constant_pool),
                                         method -> NameLength(constant_pool));
        if (method -> Synthetic() || name_symbol == control.clinit_name_symbol)
            continue; // No point reading these - the user can't access them.
        MethodSymbol* symbol = new MethodSymbol(name_symbol);
        symbol -> SetContainingType(type);
        symbol -> SetFlags(method -> Flags());
        Utf8LiteralValue* sig = control.Utf8_pool.
            FindOrInsert(method -> Signature(constant_pool),
                         method -> SignatureLength(constant_pool));
        symbol -> SetSignature(sig);
        if (method -> Deprecated())
            symbol -> MarkDeprecated();
        const ExceptionsAttribute* throws_clause = method -> Exceptions();
        if (throws_clause)
        {
            for (int j = throws_clause -> NumberOfExceptions() - 1;
                 j >= 0; j--)
            {
                const CPClassInfo* exception =
                    throws_clause -> Exception(j, constant_pool);
                symbol -> AddThrowsSignature(exception ->
                                             TypeName(constant_pool),
                                             exception ->
                                             TypeNameLength(constant_pool));
            }
        }

        type -> InsertMethodSymbol(symbol);
        if (control.option.full_check &&
            (control.option.unzip || ! type -> file_symbol -> IsZip()))
        {
            symbol -> ProcessMethodSignature(this, tok);
        }
    }

    //
    // If requested by +F, suck in all types referred to in the constant pool
    // (both in CONSTANT_Class and in descriptors of CONSTANT_NameAndType).
    //
    if (control.option.full_check &&
        (control.option.unzip || ! type -> file_symbol -> IsZip()))
    {
        for (i = constant_pool.Length() - 1; i > 0; i--)
        {
            if (constant_pool[i] -> Tag() == CPInfo::CONSTANT_Class)
                GetType(type, (CPClassInfo*) constant_pool[i], constant_pool,
                        tok);
            else if (constant_pool[i] -> Tag() == CPInfo::CONSTANT_NameAndType)
            {
                const char* signature =
                    ((CPNameAndTypeInfo*) constant_pool[i]) ->
                    Signature(constant_pool);
                if (*signature != U_LEFT_PARENTHESIS)
                    // no '(' indicates a field descriptor
                    ProcessSignature(type, signature, tok);
                else // a method descriptor
                {
                    while (*signature && *signature++ != U_RIGHT_PARENTHESIS);
                    ProcessSignature(type, signature, tok);
                }
            }
        }
    }

    delete class_data;
    type -> CompressSpace();
}


#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif
