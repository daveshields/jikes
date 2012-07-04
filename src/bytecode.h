// $Id: bytecode.h,v 1.53 2002/10/07 22:06:11 ericb Exp $ -*- c++ -*-
//
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef bytecode_INCLUDED
#define bytecode_INCLUDED

#include "platform.h"
#include "tuple.h"
#include "ast.h"
#include "class.h"
#include "op.h"
#include "segment.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class TypeSymbol;
class Control;
class Semantic;

class Label
{
public:

    class LabelUse
    {
    public:
        int use_length; // length of use (2 or 4 bytes)
        int op_offset; // length of use from opcode starting instruction
        int use_offset; // offset in code stream of use

        LabelUse() : use_length(0), op_offset(0), use_offset(0) {}

        LabelUse(int _length,
                 int _op_offset,
                 int _use) : use_length(_length),
                             op_offset(_op_offset),
                             use_offset(_use)
        {}
    };

    bool defined; // boolean, set when value is known
    u2 definition; // offset of definition point of label
    Tuple<LabelUse> uses;

    Label() : defined(false), definition(0) {}

    void Reset()
    {
       uses.Reset();
       defined = false;
       definition = 0;
    }
};


//
//
//
class MethodStack
{
public:

    void Push(AstBlock *block)
    {
        assert(block -> nesting_level < stack_size &&
               (top_index == 0 ||
                (block -> nesting_level - 1 == nesting_level[top_index - 1])));

        nesting_level[top_index] = block -> nesting_level;
        break_labels[block -> nesting_level].uses.Reset();
        continue_labels[block -> nesting_level].uses.Reset();
        finally_labels[block -> nesting_level].uses.Reset();
        handler_range_start[block -> nesting_level].Reset();
        handler_range_end[block -> nesting_level].Reset();
        blocks[block -> nesting_level] = block;
        if (size)
            memset(local_variables_start_pc[block -> nesting_level],
                   0xFF, size * sizeof(u2));
        top_index++;
    }

    void Pop()
    {
        if (top_index > 0)
        {
            top_index--;
#ifdef JIKES_DEBUG
            int level = nesting_level[top_index];

            nesting_level[top_index] = 0;
            break_labels[level].Reset();
            continue_labels[level].Reset();
            finally_labels[level].Reset();
            handler_range_start[level].Reset();
            handler_range_end[level].Reset();
            blocks[level] = NULL;
            if (size)
                memset(local_variables_start_pc[level], 0xFF,
                       size * sizeof(u2));
#endif
        }
        else assert(false);
    }

    int Size() { return top_index; }

#ifdef JIKES_DEBUG
    void AssertIndex(int k)
    {
        for (int i = 0; i < Size(); i++)
            if (nesting_level[i] == k)
                return;
        assert(0);
    }
#else
#define AssertIndex(x) /* nop */
#endif

    int TopNestingLevel()
    {
        assert(top_index > 0);
        return nesting_level[top_index - 1];
    }
    int NestingLevel(int i) { AssertIndex(i); return nesting_level[i]; }

    Label &TopBreakLabel() { return break_labels[TopNestingLevel()]; }
    Label &BreakLabel(int i) { AssertIndex(i); return break_labels[i]; }

    Label &TopContinueLabel() { return continue_labels[TopNestingLevel()]; }
    Label &ContinueLabel(int i) { AssertIndex(i); return continue_labels[i]; }

    Label &TopFinallyLabel() { return finally_labels[TopNestingLevel()]; }
    Label &FinallyLabel(int i) { AssertIndex(i); return finally_labels[i]; }

    Tuple<u2> &TopHandlerRangeStart()
    {
        return handler_range_start[TopNestingLevel()];
    }
    Tuple<u2> &HandlerRangeStart(int i)
    {
        AssertIndex(i);
        return handler_range_start[i];
    }

    Tuple<u2> &TopHandlerRangeEnd()
    {
        return handler_range_end[TopNestingLevel()];
    }
    Tuple<u2> &HandlerRangeEnd(int i)
    {
        AssertIndex(i);
        return handler_range_end[i];
    }

    AstBlock *TopBlock() { return blocks[TopNestingLevel()]; }
    AstBlock *Block(int i) { AssertIndex(i); return blocks[i]; }

    u2 *TopLocalVariablesStartPc()
    {
        return (u2 *) local_variables_start_pc[TopNestingLevel()];
    }
    u2 &StartPc(VariableSymbol *variable)
    {
        assert(variable -> LocalVariableIndex() >= 0 &&
               variable -> LocalVariableIndex() < size);
        return TopLocalVariablesStartPc()[variable -> LocalVariableIndex()];
    }

    MethodStack(int stack_size_, int size_) : stack_size(stack_size_),
                                              size(size_),
                                              top_index(0)
    {
        nesting_level = new int[stack_size];
        break_labels = new Label[stack_size];
        continue_labels = new Label[stack_size];
        finally_labels = new Label[stack_size];
        handler_range_start = new Tuple<u2>[stack_size];
        handler_range_end = new Tuple<u2>[stack_size];
        blocks = new AstBlock *[stack_size];

        local_variables_start_pc = new u2 *[stack_size];
        for (int i = 0; i < stack_size; i++)
            local_variables_start_pc[i] = new u2[size];
    }
    ~MethodStack()
    {
        delete [] nesting_level;
        delete [] break_labels;
        delete [] continue_labels;
        delete [] finally_labels;
        delete [] handler_range_start;
        delete [] handler_range_end;
        delete [] blocks;

        for (int i = 0; i < stack_size; i++)
            delete [] local_variables_start_pc[i];
        delete [] local_variables_start_pc;
    }

private:
    int *nesting_level;

    Label *break_labels;
    Label *continue_labels;
    Label *finally_labels;
    Tuple<u2> *handler_range_start;
    Tuple<u2> *handler_range_end;

    AstBlock **blocks; // block symbols for current block

    u2 **local_variables_start_pc;
    int stack_size,
        size,
        top_index;
};


class ByteCode : public ClassFile, public StringConstant, public Operators
{
    //
    // A heuristic level for generating code to handle conditional branches
    // crossing more than 32767 bytes of code. In one test case, 54616 was
    // required to generate that much code, so 10000 seems like a conservative
    // value.
    //
    enum { TOKEN_WIDTH_REQUIRING_GOTOW = 10000 };

    Control& control;
    Semantic& semantic;

    void CompileClass();
    void CompileInterface();

    int line_number,
        last_label_pc,        // pc for last (closest to end) label
        last_op_pc,           // pc of last operation emitted
        stack_depth,          // current stack depth;
        max_stack,
        max_block_depth;
    MethodStack *method_stack;

    bool string_overflow,
         library_method_not_found,
         last_op_goto;        // set if last operation was GOTO or GOTO_W.
    //
    // This variable is non-zero only in constructors of local classes; it
    // gives the offset where variable shadow parameters begin.
    //
    u2 shadow_parameter_offset;

    Code_attribute *code_attribute; // code for current method ?
    LineNumberTable_attribute *line_number_table_attribute;
    LocalVariableTable_attribute *local_variable_table_attribute;
    InnerClasses_attribute *inner_classes_attribute;

    void MethodInitialization()
    {
        last_label_pc = 0;
        last_op_pc = 0;
        last_op_goto = false;
        stack_depth = 0;
        max_stack = 0;
    }

    bool ProcessAbruptExit(int, u2, TypeSymbol * = NULL);
    void CompleteLabel(Label &lab);
    void DefineLabel(Label &lab);
    void UseLabel(Label &lab, int length, int op_offset);

    bool IsLabelUsed(Label &lab)
    {
        return lab.uses.Length() > 0;
    }


    //
    // Does p refer to a non-null reference type?
    //
    bool IsReferenceType(TypeSymbol *p)
    {
        return ! p -> Primitive() && p != control.null_type;
    }


    //
    // See if operand is constant zero (including -0.0).
    //
    bool IsZero(AstExpression *p)
    {
        TypeSymbol *type = p -> Type();
        if (p -> IsConstant() && type != control.String())
        {
            if (control.IsSimpleIntegerValueType(type) ||
                type == control.boolean_type)
            {
                return (DYNAMIC_CAST<IntLiteralValue *> (p -> value)) ->
                    value == 0;
            }
            else if (type == control.long_type)
            {
                return (DYNAMIC_CAST<LongLiteralValue *> (p -> value)) ->
                    value == 0;
            }
            else if (type == control.float_type)
            {
                return (DYNAMIC_CAST<FloatLiteralValue *> (p -> value)) ->
                    value == 0;
            }
            else
            {
                assert(type == control.double_type);
                return (DYNAMIC_CAST<DoubleLiteralValue *> (p -> value)) ->
                    value == 0;
            }
        }
        return false;
    }


    //
    // See if operand is constant one.
    //
    bool IsOne(AstExpression *p)
    {
        TypeSymbol *type = p -> Type();
        if (p -> IsConstant() && type != control.String())
        {
            if (control.IsSimpleIntegerValueType(type) ||
                type == control.boolean_type)
            {
                return (DYNAMIC_CAST<IntLiteralValue *> (p -> value)) ->
                    value == 1;
            }
            else if (type == control.long_type)
            {
                return (DYNAMIC_CAST<LongLiteralValue *> (p -> value)) ->
                    value == 1;
            }
            else if (type == control.float_type)
            {
                return (DYNAMIC_CAST<FloatLiteralValue *> (p -> value)) ->
                    value == 1;
            }
            else
            {
                assert(type == control.double_type);
                return (DYNAMIC_CAST<DoubleLiteralValue *> (p -> value)) ->
                    value == 1;
            }
        }
        return false;
    }


    //
    // memory access: reference either
    // constant (literal)
    // name (includes local varable, or class variable, or field access)
    // array
    //
    enum VariableCategory
    {
        LHS_LOCAL =  0, // local variable
        LHS_ARRAY =  1, // array (of any kind)
        LHS_FIELD =  2, // instance variable
        LHS_STATIC = 3, // class variable
        LHS_METHOD = 4 // access to private variable
    };

    VariableCategory GetLhsKind(AstExpression *expression)
    {
        AstAssignmentExpression *assignment =
            expression -> AssignmentExpressionCast();
        AstPreUnaryExpression *pre = expression -> PreUnaryExpressionCast();
        AstPostUnaryExpression *post = expression -> PostUnaryExpressionCast();

        AstExpression *lhs = (assignment
                              ? (assignment -> write_method
                                 ? (AstExpression *) NULL
                                 : assignment -> left_hand_side)
                              : pre
                              ? (pre -> write_method
                                 ? (AstExpression *) NULL : pre -> expression)
                              : post
                              ? (post -> write_method
                                 ? (AstExpression *) NULL : post -> expression)
                              : expression);

        //
        // Find symbol that is associated with expression. If the
        // subexpression has to be referenced via an access method then the
        // symbol is null.
        //
        if (lhs && lhs -> CastExpressionCast())
            lhs = ((AstCastExpression *) lhs) -> expression;
        while (lhs && lhs -> ParenthesizedExpressionCast())
            lhs = ((AstParenthesizedExpression *) lhs) -> expression;
        Symbol *sym = lhs ? lhs -> symbol : (Symbol *) NULL;

        //
        // If the expression associated with the left-hand side is null,
        // then we have an access method. Otherwise, a left-hand side is
        // either an array access, a field access or a name. In the case of
        // a FieldAccess or name, the left-hand side is resolved into a
        // variable. For an array access, it is resolved into a type.
        //
        VariableSymbol *var = (sym ? sym -> VariableCast()
                               : (VariableSymbol *) NULL);
        return (! lhs ? LHS_METHOD
                : ! var ? LHS_ARRAY
                : var -> owner -> MethodCast() ? LHS_LOCAL
                : var -> ACC_STATIC() ? LHS_STATIC
                : LHS_FIELD);
    }


    int GetTypeWords(TypeSymbol *type)
    {
        return control.IsDoubleWordType(type) ? 2 : 1;
    }


    //
    // methods to load and store values
    //
    void LoadLocal(int varno, TypeSymbol *);
    void StoreLocal(int varno, TypeSymbol *);
    void LoadLiteral(LiteralValue *, TypeSymbol *);
    void LoadImmediateInteger(int);
    int LoadVariable(VariableCategory, AstExpression *, bool = true);
    int LoadArrayElement(TypeSymbol *);
    void StoreArrayElement(TypeSymbol *);
    void StoreField(AstExpression *);
    void StoreVariable(VariableCategory, AstExpression *);

    void LoadConstantAtIndex(u2 index)
    {
        if (index <= 255)
        {
            PutOp(OP_LDC);
            PutU1((u1) index);
        }
        else
        {
            PutOp(OP_LDC_W);
            PutU2(index);
        }
    }

    //
    // These pools are sets that keep track of elements that have
    // already been inserted in the constant pool.
    //
    SegmentPool segment_pool;

    Pair *double_constant_pool_index,
         *integer_constant_pool_index,
         *long_constant_pool_index,
         *float_constant_pool_index,
         *string_constant_pool_index,

         utf8_constant_pool_index,
         class_constant_pool_index;

    Triplet *name_and_type_constant_pool_index,
            *fieldref_constant_pool_index,
            *methodref_constant_pool_index;

    u2 RegisterNameAndType(Utf8LiteralValue *name, Utf8LiteralValue *type_name)
    {
        assert(name && type_name && "null argument to RegisterNameAndType");

        if (! name_and_type_constant_pool_index)
            name_and_type_constant_pool_index =
                new Triplet(segment_pool,
                            control.Utf8_pool.symbol_pool.Length());

        u2 index = name_and_type_constant_pool_index ->
            Image(name -> index, type_name -> index);
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            name_and_type_constant_pool_index ->
                Image(name -> index, type_name -> index) = index;
            constant_pool[i] =
                new CONSTANT_NameAndType_info(CONSTANT_NameAndType,
                                              RegisterUtf8(name),
                                              RegisterUtf8(type_name));
        }

        return index;
    }


    u2 RegisterFieldref(Utf8LiteralValue *class_name,
                        Utf8LiteralValue *field_name,
                        Utf8LiteralValue *field_type_name)
    {
        assert(class_name && field_name && field_type_name &&
               "null argument to RegisterFieldref");

        if (! fieldref_constant_pool_index)
            fieldref_constant_pool_index =
                new Triplet(segment_pool,
                            control.Utf8_pool.symbol_pool.Length());

        u2 name_type_index = RegisterNameAndType(field_name, field_type_name);
        u2 index = fieldref_constant_pool_index ->
            Image(class_name -> index, name_type_index);
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            fieldref_constant_pool_index ->
                Image(class_name -> index, name_type_index) = index;
            constant_pool[i] =
                new CONSTANT_Fieldref_info(CONSTANT_Fieldref,
                                           RegisterClass(class_name),
                                           name_type_index);
        }

        return index;
    }


    u2 RegisterFieldref(TypeSymbol *type, VariableSymbol *variable)
    {
        assert(variable -> owner -> TypeCast());

        return RegisterFieldref(type -> fully_qualified_name,
                                variable -> ExternalIdentity() -> Utf8_literal,
                                variable -> Type() -> signature);
    }


    u2 RegisterFieldref(VariableSymbol *variable)
    {
        assert(variable -> owner -> TypeCast());

        return RegisterFieldref(variable -> ContainingType() -> fully_qualified_name,
                                variable -> ExternalIdentity() -> Utf8_literal,
                                variable -> Type() -> signature);
    }


    u2 RegisterMethodref(ConstantKind kind,
                         Utf8LiteralValue *class_name,
                         Utf8LiteralValue *method_name,
                         Utf8LiteralValue *method_type_name)
    {
        assert(class_name && method_name && method_type_name &&
               "null argument to RegisterMethodref");

        if (! methodref_constant_pool_index)
            methodref_constant_pool_index =
                new Triplet(segment_pool,
                            control.Utf8_pool.symbol_pool.Length());

        u2 name_type_index = RegisterNameAndType(method_name, method_type_name);
        u2 index = methodref_constant_pool_index ->
            Image(class_name -> index, name_type_index);
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            methodref_constant_pool_index -> Image(class_name -> index,
                                                   name_type_index) = index;

            u2 class_name_index = RegisterClass(class_name);
            constant_pool[i] = kind == CONSTANT_Methodref
                ? (cp_info *) new CONSTANT_Methodref_info(CONSTANT_Methodref,
                                                          class_name_index,
                                                          name_type_index)
                : (cp_info *) new CONSTANT_InterfaceMethodref_info(CONSTANT_InterfaceMethodref,
                                                                   class_name_index,
                                                                   name_type_index);
        }

        return index;
    }


    u2 RegisterMethodref(TypeSymbol *class_type,
                         NameSymbol *method_name,
                         MethodSymbol *method_type)
    {
        return RegisterMethodref(CONSTANT_Methodref,
                                 class_type -> fully_qualified_name,
                                 method_name -> Utf8_literal,
                                 method_type -> signature);
    }

    u2 RegisterInterfaceMethodref(TypeSymbol *interface_name,
                                  NameSymbol *method_name,
                                  MethodSymbol *method_type_name)
    {
        return RegisterMethodref(CONSTANT_InterfaceMethodref,
                                 interface_name -> fully_qualified_name,
                                 method_name -> Utf8_literal,
                                 method_type_name -> signature);
    }


    u2 RegisterLibraryMethodref(MethodSymbol *method)
    {
        // The library method must exist. If it does not, flag an error.
        if (method)
            return RegisterMethodref(CONSTANT_Methodref,
                                     method -> containing_type -> fully_qualified_name,
                                     method -> ExternalIdentity()-> Utf8_literal,
                                     method -> signature);
        library_method_not_found = true;

        return 0;
    }

    u2 RegisterDouble(DoubleLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterDouble");

        if (! double_constant_pool_index)
            double_constant_pool_index =
                new Pair(segment_pool,
                         control.double_pool.symbol_pool.Length());

        u2 index = (*double_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            constant_pool.Next() = NULL; // extra slop for double-word entry
            index = i;
            (*double_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Double_info(CONSTANT_Double,
                                                        lit -> value.HighWord(),
                                                        lit -> value.LowWord());
        }

        return index;
    }


    u2 RegisterInteger(IntLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterInteger");

        if (! integer_constant_pool_index)
            integer_constant_pool_index =
                new Pair(segment_pool, control.int_pool.symbol_pool.Length());

        u2 index = (*integer_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            (*integer_constant_pool_index)[lit -> index] = index;
            int val = lit -> value;
            u4 bytes = (((unsigned) (val >> 24)) << 24) |
                ((val >> 16 & 0xff) << 16) |
                ((val >> 8 & 0xff) ) << 8 | (val & 0xff);
            constant_pool[i] = new CONSTANT_Integer_info(CONSTANT_Integer,
                                                         bytes);
        }

        return index;
    }


    u2 FindInteger(IntLiteralValue *lit)
    {
        return (lit && integer_constant_pool_index
                ? (*integer_constant_pool_index)[lit -> index] : 0);
    }


    u2 RegisterLong(LongLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterLong");

        if (! long_constant_pool_index)
            long_constant_pool_index =
                new Pair(segment_pool, control.long_pool.symbol_pool.Length());

        u2 index = (*long_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            constant_pool.Next() = NULL; // extra slop for double-word entry
            index = i;
            (*long_constant_pool_index)[lit -> index] = index;
            constant_pool[i] =
                new CONSTANT_Long_info(CONSTANT_Long,
                                       lit -> value.HighWord(),
                                       lit -> value.LowWord());
        }

        return index;
    }


    u2 RegisterFloat(FloatLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterFloat");

        if (! float_constant_pool_index)
            float_constant_pool_index =
                new Pair(segment_pool, control.float_pool.symbol_pool.Length());

        u2 index = (*float_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            (*float_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Float_info(CONSTANT_Float,
                                                       lit -> value.Word());
        }

        return index;
    }


    u2 RegisterUtf8(Utf8LiteralValue *lit)
    {
        assert(lit != NULL && "null argument to RegisterUtf8");

        u2 index = utf8_constant_pool_index[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            utf8_constant_pool_index[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Utf8_info(CONSTANT_Utf8,
                                                      lit -> value,
                                                      lit -> length);
            if (lit -> length > 0xffff)
                string_overflow = true;
        }

        return index;
    }

    u2 RegisterName(NameSymbol *sym)
    {
        return RegisterUtf8(sym -> Utf8_literal);
    }

    u2 RegisterString(Utf8LiteralValue *lit)
    {
        assert(lit && "null argument to RegisterString");

        //
        // The domain of these maps is an index in the constant_pool.
        // For a valid program, the size of the constant pool is limited
        // to 65k elements.
        //
        if (! string_constant_pool_index)
            string_constant_pool_index =
                new Pair(segment_pool, control.Utf8_pool.symbol_pool.Length());

        u2 index = (*string_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            (*string_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_String_info(CONSTANT_String,
                                                        RegisterUtf8(lit));
        }

        return index;
    }


    u2 RegisterClass(Utf8LiteralValue *lit)
    {
        assert(lit && "null argument to RegisterClass");

        u2 index = class_constant_pool_index[lit -> index];
        if (index == 0)
        {
            // We cannot use the variable "index" here as it might be truncated
            int i = constant_pool.NextIndex();
            index = i;
            class_constant_pool_index[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Class_info(CONSTANT_Class,
                                                       RegisterUtf8(lit));
        }

        return index;
    }

    u2 RegisterClass(TypeSymbol *sym)
    {
        return RegisterClass(sym -> fully_qualified_name);
    }


    //
    //  Methods to write out the byte code
    //
    Deprecated_attribute *CreateDeprecatedAttribute()
    {
        return new Deprecated_attribute(RegisterUtf8(control.Deprecated_literal));
    }


    Synthetic_attribute *CreateSyntheticAttribute()
    {
        return new Synthetic_attribute(RegisterUtf8(control.Synthetic_literal));
    }


    //
    // Methods to generate expressions.
    //
    int EmitExpression(AstExpression *, bool = true);
    int EmitArrayCreationExpression(AstArrayCreationExpression *, bool = true);
    int EmitAssignmentExpression(AstAssignmentExpression *, bool);
    int EmitBinaryExpression(AstBinaryExpression *);
    int EmitCastExpression(AstCastExpression *, bool);
    void EmitCast(TypeSymbol *, TypeSymbol *);
    int EmitInstanceCreationExpression(AstClassInstanceCreationExpression *,
                                        bool);
    int EmitConditionalExpression(AstConditionalExpression *, bool);
    int EmitFieldAccess(AstFieldAccess *, bool = true);
    AstExpression *VariableExpressionResolution(AstExpression *);
    TypeSymbol *VariableTypeResolution(AstExpression *, VariableSymbol *);
    TypeSymbol *MethodTypeResolution(AstExpression *, MethodSymbol *);
    void EmitFieldAccessLhsBase(AstExpression *);
    void EmitFieldAccessLhs(AstExpression *);
    void EmitMethodInvocation(AstMethodInvocation *);
    void EmitNewArray(int, TypeSymbol *);
    int EmitPostUnaryExpression(AstPostUnaryExpression *, bool);
    void EmitPostUnaryExpressionArray(AstPostUnaryExpression *, bool);
    void EmitPostUnaryExpressionField(VariableCategory,
                                      AstPostUnaryExpression *, bool);
    void EmitPostUnaryExpressionSimple(VariableCategory,
                                       AstPostUnaryExpression *, bool);
    int EmitPreUnaryExpression(AstPreUnaryExpression *, bool);
    void EmitPreUnaryIncrementExpression(AstPreUnaryExpression *, bool);
    void EmitPreUnaryIncrementExpressionArray(AstPreUnaryExpression *, bool);
    void EmitPreUnaryIncrementExpressionField(VariableCategory,
                                              AstPreUnaryExpression *, bool);
    void EmitPreUnaryIncrementExpressionSimple(VariableCategory,
                                               AstPreUnaryExpression *, bool);
    void EmitThisInvocation(AstThisCall *);
    void EmitSuperInvocation(AstSuperCall *);
    void ConcatenateString(AstBinaryExpression *);
    void AppendString(AstExpression *);
    void EmitStringAppendMethod(TypeSymbol *);
    void ChangeStack(int);
    void ResolveAccess(AstExpression *);
    int GenerateClassAccess(AstFieldAccess *, bool);
    void GenerateClassAccessMethod(MethodSymbol *);
    void GenerateAssertVariableInitializer(TypeSymbol *, VariableSymbol *);
    void EmitCheckForNull(AstExpression *expr, bool = true);

    //
    // Methods to process statements
    //
    void CompileConstructor(AstConstructorDeclaration *,
                            Tuple<AstVariableDeclarator *> &, bool);

    void BeginMethod(int, MethodSymbol *);
    void EndMethod(int, MethodSymbol *);
    void DeclareField(VariableSymbol *);
    void InitializeVariable(AstVariableDeclarator *);
    void InitializeArray(TypeSymbol *, AstArrayInitializer *, bool = true);
    void DeclareLocalVariable(AstVariableDeclarator *);
    bool EmitStatement(AstStatement *);
    void EmitReturnStatement(AstReturnStatement *);
    bool EmitSynchronizedStatement(AstSynchronizedStatement *);
    bool EmitBlockStatement(AstBlock *);
    void EmitStatementExpression(AstExpression *);
    void EmitSwitchStatement(AstSwitchStatement *);
    void EmitTryStatement(AstTryStatement *);
    void EmitAssertStatement(AstAssertStatement *);
    void EmitBranchIfExpression(AstExpression *, bool, Label &,
                                AstStatement * = NULL);
    void EmitBranch(Opcode, Label &, AstStatement * = NULL);
    void CompleteCall(MethodSymbol *, int, TypeSymbol * = NULL);

    AstExpression *StripNops(AstExpression *);
    bool IsNop(AstBlock *);

    void EmitArrayAccessLhs(AstArrayAccess *expression)
    {
        EmitExpression(expression -> base);
        EmitExpression(expression -> expression);
    }


    int EmitArrayAccessRhs(AstArrayAccess *expression)
    {
        EmitArrayAccessLhs(expression); // get array address and index
        return LoadArrayElement(expression -> Type());
    }

    // Return the OP_IF... bytecode that has the opposite meaning
    Opcode InvertIfOpCode(Opcode opc)
    {
        //
        // Unfortunately, the JVMS does not nicely specify symmetric opcodes;
        // we must treat even-odd and odd-even pairs differently.
        //
        if (opc >= OP_IFNULL)
        {
            assert(opc <= OP_IFNONNULL);
            return (Opcode) (opc ^ 1);
        }
        assert(OP_IFEQ <= opc && opc <= OP_IF_ACMPNE);
        return (Opcode) (((opc + 1) ^ 1) - 1);
    }

    void GenerateReturn(TypeSymbol *type)
    {
        PutOp((control.IsSimpleIntegerValueType(type) ||
               type == control.boolean_type) ? OP_IRETURN
              : type == control.long_type ? OP_LRETURN
              : type == control.float_type ? OP_FRETURN
              : type == control.double_type ? OP_DRETURN
              : OP_ARETURN);
    }


#ifdef JIKES_DEBUG
    void PrintCode();
#endif

    void PutOp(Opcode);

    void PutOpWide(Opcode, u2 var);

    void PutOpIINC(u2 var, int val);

    //
    //  Methods to insert values into byte code
    //
    void PutI1(i1 i)
    {
        code_attribute -> AddCode(i & 0xff);
    }

    void PutI2(i2 i)
    {
        code_attribute -> AddCode((i >> 8) & 0xff);
        code_attribute -> AddCode(i & 0xff);
    }

    void PutU1(u1 u)
    {
        code_attribute -> AddCode(u & 0xff);
    }

    void PutU2(u2 u)
    {
        code_attribute -> AddCode((u >> 8) & 0xff);
        code_attribute -> AddCode(u & 0xff);
    }

    void PutU4(u4 u)
    {
        code_attribute -> AddCode((u >> 24));
        code_attribute -> AddCode((u >> 16) & 0xff);
        code_attribute -> AddCode((u >>  8) & 0xff);
        code_attribute -> AddCode(u & 0xff);
    }

    void FinishCode(TypeSymbol *);

    void Reset()
    {
        constant_pool.Reset();
        fields.Reset();
        methods.Reset();
        attributes.Reset();
        this_class = super_class = 0;
    }

public:
    ByteCode(TypeSymbol *);

    ~ByteCode()
    {
        delete double_constant_pool_index;
        delete integer_constant_pool_index;
        delete long_constant_pool_index;
        delete float_constant_pool_index;
        delete string_constant_pool_index;

        delete name_and_type_constant_pool_index;
        delete fieldref_constant_pool_index;
        delete methodref_constant_pool_index;
    }

    inline void GenerateCode()
    {
        if (unit_type -> ACC_INTERFACE())
             CompileInterface();
        else CompileClass();
    }
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // bytecode_INCLUDED

