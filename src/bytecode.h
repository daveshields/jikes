// $Id: bytecode.h,v 1.24 2001/05/07 06:33:59 cabbey Exp $
//
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

//
#ifndef bytecode_INCLUDED
#define bytecode_INCLUDED

#include "platform.h"
#include "tuple.h"
#include "ast.h"
#include "class.h"
#include "option.h"
#include "long.h"
#include "op.h"
#include "segment.h"

#ifdef	HAVE_JIKES_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
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
        int use_length, // length of use (2 or 4 bytes)
            op_offset,  // length of use from opcode starting instruction
            use_offset; // offset in code stream of use

        LabelUse() : use_length(0), op_offset(0), use_offset(0) {}

        LabelUse(int _length, int _op_offset, int _use) : use_length(_length), op_offset(_op_offset), use_offset(_use) {}
    };

    bool defined;   // boolean, set when value is known
    int definition; // offset of definition point of label
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
              (top_index == 0 || ((block -> nesting_level - 1) == nesting_level[top_index - 1])));

        nesting_level[top_index] = block -> nesting_level;
        break_labels[block -> nesting_level].uses.Reset();
        continue_labels[block -> nesting_level].uses.Reset();
        finally_labels[block -> nesting_level].uses.Reset();
        monitor_labels[block -> nesting_level].uses.Reset();
        blocks[block -> nesting_level] = block;

#ifdef JIKES_DEBUG
        (void) memset(local_variables_start_pc[block -> nesting_level], 0xFF, size * sizeof(u2));
#endif
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
            monitor_labels[level].Reset();
            blocks[level] = NULL;
            (void) memset(local_variables_start_pc[level], 0xFF, size * sizeof(u2));
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
#define AssertIndex(x)
#endif

    int TopNestingLevel()   { assert(top_index > 0); return nesting_level[top_index - 1]; }
    int NestingLevel(int i) { AssertIndex(i); return nesting_level[i]; }

    Label &TopBreakLabel()    { return break_labels[TopNestingLevel()]; }
    Label &BreakLabel(int i)  { AssertIndex(i); return break_labels[i]; }

    Label &TopContinueLabel()   { return continue_labels[TopNestingLevel()]; }
    Label &ContinueLabel(int i) { AssertIndex(i); return continue_labels[i]; }

    Label &TopFinallyLabel()    { return finally_labels[TopNestingLevel()]; }
    Label &FinallyLabel(int i)  { AssertIndex(i); return finally_labels[i]; }

    Label &TopMonitorLabel()   { return monitor_labels[TopNestingLevel()]; }
    Label &MonitorLabel(int i) { AssertIndex(i); return monitor_labels[i]; }

    AstBlock *TopBlock()   { return blocks[TopNestingLevel()]; }
    AstBlock *Block(int i) { AssertIndex(i); return blocks[i]; }

    //
    //
    //
    u2 *TopLocalVariablesStartPc() { return (u2 *) local_variables_start_pc[TopNestingLevel()]; }
    u2 &StartPc(VariableSymbol *variable)
    {
        assert(variable -> LocalVariableIndex() >= 0 && variable -> LocalVariableIndex() < size);
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
        monitor_labels = new Label[stack_size];
        blocks = new AstBlock *[stack_size];

        local_variables_start_pc = new u2*[stack_size];
        for (int i = 0; i < stack_size; i++)
            local_variables_start_pc[i] = new u2[size];
    }
    ~MethodStack()
    {
        delete [] nesting_level;

        delete [] break_labels;
        delete [] continue_labels;
        delete [] finally_labels;
        delete [] monitor_labels;

        delete [] blocks;

        for (int i = 0; i < stack_size; i++)
            delete [] local_variables_start_pc[i];
        delete [] local_variables_start_pc;
    }

private:
    int *nesting_level;

    Label *break_labels,
          *continue_labels,
          *finally_labels,
          *monitor_labels;

    AstBlock **blocks; // block symbols for current block

    u2 **local_variables_start_pc;
    int stack_size,
        size,
        top_index;
};


class ByteCode : public ClassFile, public StringConstant, public Operators
{
    Control& this_control;
    Semantic& this_semantic;

    void CompileClass();
    void CompileInterface();

    int line_number,
        last_label_pc,        // pc for last (closest to end) label
        last_op_pc,           // pc of last operation emitted
        last_op_nop,          // set if last operation was NOP.
        stack_depth,          // current stack depth;
        max_stack,
        max_block_depth,
        last_parameter_index; // set to local variable index of last parameter
    MethodStack *method_stack;

    bool string_overflow,
         library_method_not_found;

    Code_attribute *code_attribute; // code for current method ?
    LineNumberTable_attribute *line_number_table_attribute;
    LocalVariableTable_attribute *local_variable_table_attribute;
    InnerClasses_attribute *inner_classes_attribute;

    void MethodInitialization()
    {
        last_label_pc = 0;
        last_op_pc = 0;
        last_op_nop = 0;

        stack_depth = 0;

        max_stack = 0;

        return;
    }

    void    ProcessAbruptExit(int, TypeSymbol * = NULL);
    void    CompleteLabel(Label &lab);
    void    DefineLabel(Label &lab);
    void    UseLabel(Label &lab, int length, int op_offset);

    bool IsLabelUsed(Label &lab)
    {
        return (lab.uses.Length() > 0);
    }


    //
    // see if operand is null. The front-end will have inserted a cast
    // of null to the present type
    //
    bool IsNull(AstExpression *p)
    {
        return (p -> CastExpressionCast() ? (p -> CastExpressionCast() -> expression -> Type() == this_control.null_type) : false);
    }


    //
    // Does p refer to a non-null reference type?
    //
    bool IsReferenceType(TypeSymbol *p)
    {
        return (! (p -> Primitive() || p == this_control.null_type));
    }


    //
    // see if operand is integer type and is zero
    //
    bool IsZero(AstExpression *p)
    {
        if (p -> IsConstant() && (p -> Type() == this_control.int_type || p -> Type() == this_control.boolean_type))
        {
            IntLiteralValue *vp = (IntLiteralValue *) (p -> value);
            return (vp -> value == 0);
        }

        return false;
    }


    //
    // memory access: reference either
    // constant (literal)
    // name (includes local varable, or class variable, or field access)
    // array
    //
    enum
    {
        LHS_LOCAL =  0, // local variable
        LHS_ARRAY =  1, // array (of any kind)
        LHS_FIELD =  2, // instance variable
        LHS_STATIC = 3, // class variable
        LHS_METHOD = 4 // access to private variable
    };

    int GetLhsKind(AstExpression *expression)
    {
        AstAssignmentExpression *assignment = expression -> AssignmentExpressionCast();
        AstPreUnaryExpression *pre = expression -> PreUnaryExpressionCast();
        AstPostUnaryExpression *post = expression -> PostUnaryExpressionCast();

        AstExpression *lhs = assignment ? (assignment -> write_method ? (AstExpression *) NULL : assignment -> left_hand_side)
                                        : pre ? (pre -> write_method ? (AstExpression *) NULL : pre -> expression)
                                              : post ? (post -> write_method ? (AstExpression *) NULL : post -> expression)
                                                     : expression;

        //
        // Find symbol that is associated with expression. If the subexpression has
        // to be referenced via an access method then the symbol is null
        //
        AstCastExpression *cast = (lhs ? lhs -> CastExpressionCast() : (AstCastExpression *) NULL);
        Symbol *sym = cast ? cast -> expression -> symbol : (lhs ? lhs -> symbol : (Symbol *) NULL);

        //
        // If the expression associated with the left-hand side is null,
        // then we have an access method.
        // Otherwise, a left-hand side is either an array access,
        // a field access or a name. In the case of a FieldAccess
        // or name, the left-hand side is resolved into a variable.
        // In the case of an array access, it is resolved into a type.
        //
        VariableSymbol *var = (sym ? sym -> VariableCast() : (VariableSymbol *) NULL);
        return ((! lhs) ? LHS_METHOD
                        : (! var) ? LHS_ARRAY
                                  : var -> owner -> MethodCast() ? LHS_LOCAL
                                                                 : var -> ACC_STATIC() ? LHS_STATIC
                                                                                       : LHS_FIELD);
    }


    int GetTypeWords(TypeSymbol *type)
    {
        return this_control.IsDoubleWordType(type) ? 2 : 1;
    }


    //
    // methods to load and store values
    //
    void LoadLocal(int varno, TypeSymbol *);
    void StoreLocal(int varno, TypeSymbol *);
    void LoadReference(AstExpression *);
    void LoadLiteral(LiteralValue *, TypeSymbol *);
    void LoadImmediateInteger(int);
    int  LoadVariable(int, AstExpression *);
    int  LoadArrayElement(TypeSymbol *);
    void StoreArrayElement(TypeSymbol *);
    void StoreField(AstExpression *);
    void StoreVariable(int, AstExpression *);

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

        return;
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
        assert((name != NULL && type_name != NULL) && "null argument to RegisterNameAndType");

        if (! name_and_type_constant_pool_index)
            name_and_type_constant_pool_index = new Triplet(segment_pool, this_control.Utf8_pool.symbol_pool.Length());

        u2 index = name_and_type_constant_pool_index -> Image(name -> index, type_name -> index);
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            name_and_type_constant_pool_index -> Image(name -> index, type_name -> index) = index;
            constant_pool[i] = new CONSTANT_NameAndType_info(CONSTANT_NameAndType, RegisterUtf8(name), RegisterUtf8(type_name));
        }

        return index;
    }


    u2 RegisterFieldref(Utf8LiteralValue *class_name,
                        Utf8LiteralValue *field_name,
                        Utf8LiteralValue *field_type_name)
    {
        assert((class_name != NULL && field_name != NULL && field_type_name != NULL) && "null argument to RegisterFieldref");

        if (! fieldref_constant_pool_index)
            fieldref_constant_pool_index = new Triplet(segment_pool, this_control.Utf8_pool.symbol_pool.Length());

        u2 name_type_index = RegisterNameAndType(field_name, field_type_name),
           index = fieldref_constant_pool_index -> Image(class_name -> index, name_type_index);
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            fieldref_constant_pool_index -> Image(class_name -> index, name_type_index) = index;
            constant_pool[i] = new CONSTANT_Fieldref_info(CONSTANT_Fieldref, RegisterClass(class_name), name_type_index);
        }

        return index;
    }


    u2 RegisterFieldref(TypeSymbol *type, VariableSymbol *variable_symbol)
    {
        assert(variable_symbol -> owner -> TypeCast());

        return RegisterFieldref(type -> fully_qualified_name,
                                variable_symbol -> ExternalIdentity() -> Utf8_literal,
                                variable_symbol -> Type() -> signature);
    }


    u2 RegisterFieldref(VariableSymbol *variable_symbol)
    {
        assert(variable_symbol -> owner -> TypeCast());

        return RegisterFieldref(variable_symbol -> owner -> TypeCast() -> fully_qualified_name,
                                variable_symbol -> ExternalIdentity() -> Utf8_literal,
                                variable_symbol -> Type() -> signature);
    }


    u2 RegisterMethodref(ConstantKind kind,
                         Utf8LiteralValue *class_name,
                         Utf8LiteralValue *method_name,
                         Utf8LiteralValue *method_type_name)
    {
        assert((class_name != NULL && method_name != NULL && method_type_name != NULL) && "null argument to RegisterMethodref");

        if (! methodref_constant_pool_index)
            methodref_constant_pool_index = new Triplet(segment_pool, this_control.Utf8_pool.symbol_pool.Length());

        u2 name_type_index = RegisterNameAndType(method_name, method_type_name),
           index = methodref_constant_pool_index -> Image(class_name -> index, name_type_index);
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            methodref_constant_pool_index -> Image(class_name -> index, name_type_index) = index;

            u2 class_name_index = RegisterClass(class_name);
            constant_pool[i] = (kind == CONSTANT_Methodref
                                      ? (cp_info *) new CONSTANT_Methodref_info(CONSTANT_Methodref,
                                                                                class_name_index,
                                                                                name_type_index)
                                      : (cp_info *) new CONSTANT_InterfaceMethodref_info(CONSTANT_InterfaceMethodref,
                                                                                         class_name_index,
                                                                                         name_type_index));
        }

        return index;
    }


    u2 RegisterMethodref(Utf8LiteralValue *class_name, Utf8LiteralValue *method_name, Utf8LiteralValue *method_type_name)
    {
        return RegisterMethodref(CONSTANT_Methodref, class_name, method_name, method_type_name);
    }

    u2 RegisterInterfaceMethodref(Utf8LiteralValue *class_name, Utf8LiteralValue *method_name, Utf8LiteralValue *method_type_name)
    {
        return RegisterMethodref(CONSTANT_InterfaceMethodref, class_name, method_name, method_type_name);
    }


    u2 RegisterLibraryMethodref(MethodSymbol *method)
    {
        if (method) // The library method must exist. If it is not, flag an error.
            return RegisterMethodref(CONSTANT_Methodref, method -> containing_type -> fully_qualified_name,
                                                         method -> ExternalIdentity()-> Utf8_literal,
                                                         method -> signature);
        library_method_not_found = true;

        return 0;
    }

    u2 RegisterDouble(DoubleLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterDouble");

        if (! double_constant_pool_index)
            double_constant_pool_index = new Pair(segment_pool, this_control.double_pool.symbol_pool.Length());

        u2 index = (*double_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            constant_pool.Next() = NULL;       // extra slop for double-word entry
            index = i;
            (*double_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Double_info(CONSTANT_Double, lit -> value.HighWord(), lit -> value.LowWord());
        }

        return index;
    }


    u2 RegisterInteger(IntLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterInteger");

        if (! integer_constant_pool_index)
            integer_constant_pool_index = new Pair(segment_pool, this_control.int_pool.symbol_pool.Length());

        u2 index = (*integer_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            (*integer_constant_pool_index)[lit -> index] = index;
            int val = lit -> value;
            u4 bytes = (((unsigned) (val >> 24)) << 24) | ((val >> 16 & 0xff) << 16) | ((val >> 8 & 0xff) ) << 8 | (val & 0xff);
            constant_pool[i] = new CONSTANT_Integer_info(CONSTANT_Integer, bytes);
        }

        return index;
    }


    u2 FindInteger(IntLiteralValue *lit)
    {
        return (lit && integer_constant_pool_index ? (*integer_constant_pool_index)[lit -> index] : 0);
    }


    u2 RegisterLong(LongLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterLong");

        if (! long_constant_pool_index)
            long_constant_pool_index = new Pair(segment_pool, this_control.long_pool.symbol_pool.Length());

        u2 index = (*long_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            constant_pool.Next() = NULL;       // extra slop for double-word entry
            index = i;
            (*long_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Long_info(CONSTANT_Long, lit -> value.HighWord(), lit -> value.LowWord());
        }

        return index;
    }


    u2 RegisterFloat(FloatLiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterFloat");

        if (! float_constant_pool_index)
            float_constant_pool_index = new Pair(segment_pool, this_control.float_pool.symbol_pool.Length());

        u2 index = (*float_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            (*float_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Float_info(CONSTANT_Float, lit -> value.Word());
        }

        return index;
    }


    u2 RegisterUtf8(Utf8LiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterUtf8");

        u2 index = utf8_constant_pool_index[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            utf8_constant_pool_index[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Utf8_info(CONSTANT_Utf8, lit -> value, lit -> length);
        }

        return index;
    }


    u2 RegisterString(Utf8LiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterString");

        //
        // The domain of these maps is an index in the constant_pool.
        // For a valid program, the size of the constant pool is limited
        // to 65k elements.
        //
        if (! string_constant_pool_index)
            string_constant_pool_index = new Pair(segment_pool, this_control.Utf8_pool.symbol_pool.Length());

        u2 index = (*string_constant_pool_index)[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            (*string_constant_pool_index)[lit -> index] = index;
            constant_pool[i] = new CONSTANT_String_info(CONSTANT_String, RegisterUtf8(lit));
        }

        return index;
    }


    u2 RegisterClass(Utf8LiteralValue *lit)
    {
        assert((lit != NULL) && "null argument to RegisterClass");

        u2 index = class_constant_pool_index[lit -> index];
        if (index == 0)
        {
            int i = constant_pool.NextIndex(); // We cannot use the variable "index" here as it might be truncated
            index = i;
            class_constant_pool_index[lit -> index] = index;
            constant_pool[i] = new CONSTANT_Class_info(CONSTANT_Class, RegisterUtf8(lit));
        }

        return index;
    }


    //
    //  Methods to write out the byte code
    //
    Deprecated_attribute *CreateDeprecatedAttribute()
    {
        return new Deprecated_attribute(RegisterUtf8(this_control.Deprecated_literal));
    }


    Synthetic_attribute *CreateSyntheticAttribute()
    {
        return new Synthetic_attribute(RegisterUtf8(this_control.Synthetic_literal));
    }


    //
    // Methods to generate expressions.
    //
    int  EmitExpression(AstExpression *);
    int  EmitArrayCreationExpression(AstArrayCreationExpression *);
    int  EmitAssignmentExpression(AstAssignmentExpression *, bool);
    int  EmitBinaryExpression(AstBinaryExpression *);
    int  EmitCastExpression(AstCastExpression *);
    void EmitCast(TypeSymbol *, TypeSymbol *);
    int  EmitClassInstanceCreationExpression(AstClassInstanceCreationExpression *, bool);
    int  EmitConditionalExpression(AstConditionalExpression *);
    int  EmitFieldAccess(AstFieldAccess *);
    AstExpression *VariableExpressionResolution(AstExpression *);
    TypeSymbol *VariableTypeResolution(AstExpression *, VariableSymbol *);
    TypeSymbol *MethodTypeResolution(AstExpression *, MethodSymbol *);
    void EmitFieldAccessLhsBase(AstExpression *);
    void EmitFieldAccessLhs(AstExpression *);
    void EmitMethodInvocation(AstMethodInvocation *);
    void EmitNewArray(int, TypeSymbol *);
    int  EmitPostUnaryExpression(AstPostUnaryExpression *, bool);
    void EmitPostUnaryExpressionArray(AstPostUnaryExpression *, bool);
    void EmitPostUnaryExpressionField(int, AstPostUnaryExpression *, bool);
    void EmitPostUnaryExpressionSimple(int, AstPostUnaryExpression *, bool);
    int  EmitPreUnaryExpression(AstPreUnaryExpression *, bool);
    void EmitPreUnaryIncrementExpression(AstPreUnaryExpression *expression, bool);
    void EmitPreUnaryIncrementExpressionArray(AstPreUnaryExpression *expression, bool);
    void EmitPreUnaryIncrementExpressionField(int, AstPreUnaryExpression *expression, bool);
    void EmitPreUnaryIncrementExpressionSimple(int, AstPreUnaryExpression *expression, bool);
    void EmitThisInvocation(AstThisCall *);
    void EmitSuperInvocation(AstSuperCall *);
    void ConcatenateString(AstBinaryExpression *);
    void AppendString(AstExpression *);
    void EmitStringAppendMethod(TypeSymbol *);
    void GenerateAccessMethod(MethodSymbol *);
    void ChangeStack (int);
    void ResolveAccess(AstExpression *);
    int  GenerateClassAccess(AstFieldAccess *);
    void GenerateClassAccessMethod(MethodSymbol *);
    void EmitCheckForNull(AstExpression *);

    //
    // Methods to process statements
    //
    void CompileConstructor(AstConstructorDeclaration *, Tuple<AstVariableDeclarator *> &);

    void BeginMethod(int, MethodSymbol *);
    void EndMethod(int, MethodSymbol *);
    void DeclareField(VariableSymbol *);
    void InitializeClassVariable(AstVariableDeclarator *);
    void InitializeInstanceVariable(AstVariableDeclarator *);
    void InitializeArray(TypeSymbol *, AstArrayInitializer *);
    void DeclareLocalVariable(AstVariableDeclarator *);
    void EmitStatement(AstStatement *);
    void EmitReturnStatement(AstReturnStatement *);
    void EmitSynchronizedStatement(AstSynchronizedStatement *);
    void EmitBlockStatement(AstBlock *);
    void EmitStatementExpression(AstExpression *);
    void EmitSwitchStatement(AstSwitchStatement *);
    void EmitTryStatement(AstTryStatement *);
    void EmitBranchIfExpression(AstExpression *, bool, Label &);
    void CompleteCall(MethodSymbol *, int, TypeSymbol * = NULL);


    //
    // called when expression has been parenthesized to removed
    // parantheses and expose true structure.
    //
    AstExpression *UnParenthesize(AstExpression *expr)
    {
        while(! (expr -> IsConstant()) && expr -> ParenthesizedExpressionCast())
            expr = expr -> ParenthesizedExpressionCast() -> expression;

        return expr;
    }


    void EmitArrayAccessLhs(AstArrayAccess *expression)
    {
        LoadReference(expression -> base);
        EmitExpression(expression -> expression);

        return;
    }


    int EmitArrayAccessRhs(AstArrayAccess *expression)
    {
        EmitArrayAccessLhs(expression); // get array address and index
        return LoadArrayElement(expression -> Type());
    }


    void EmitBranch(unsigned int opc, Label& lab)
    {
        PutOp(opc);
        UseLabel(lab, 2, 1);

        return;
    }


    void GenerateReturn(TypeSymbol *type)
    {
        PutOp(this_control.IsSimpleIntegerValueType(type) || type == this_control.boolean_type
                  ? OP_IRETURN
                  : type == this_control.long_type
                          ? OP_LRETURN
                          : type == this_control.float_type
                                  ? OP_FRETURN
                                  : type == this_control.double_type
                                          ? OP_DRETURN
                                          : OP_ARETURN);
        return;
    }


#ifdef JIKES_DEBUG
    void PrintCode();
#endif

    void PutOp(unsigned char opc);

    void PutOpWide(unsigned char opc, u2 var);

    void PutOpIINC(u2 var, int val);

    //
    //  Methods to insert values into byte code
    //
    void PutI1(i1 i)
    {
        code_attribute -> AddCode(i & 0xff);

        return;
    }


    void PutI2(i2 i)
    {
        code_attribute -> AddCode((i >> 8) & 0xff);
        code_attribute -> AddCode(i & 0xff);

        return;
    }


    void PutU1(u1 u)
    {
        code_attribute -> AddCode(u & 0xff);

        return;
    }


    void PutU2(u2 u)
    {
        code_attribute -> AddCode((u >> 8) & 0xff);
        code_attribute -> AddCode(u & 0xff);

        return;
    }


    void PutU4(u4 u)
    {
        code_attribute -> AddCode((u >> 24));
        code_attribute -> AddCode((u >> 16) & 0xff);
        code_attribute -> AddCode((u >>  8) & 0xff);
        code_attribute -> AddCode(u & 0xff);

        return;
    }


    //
    // emit NOP. The NOP can be replaced by the next instruction if
    // optional is set; otherwise it must be kept.
    //
    void PutNop(int optional)
    {
        PutOp(OP_NOP);

        //
        // this optimization is causing more trouble than it's worth.
        // latest problem (27 jan 97) was reported by Derek, in that
        // nop just before label definition, resulted in operation generated
        // after label def. being moved before the def! Since it's such a sin
        // to generate junk code, disable the "nop" optimization.
        //  if (optional) last_op_nop = 1;
        //

        return;
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

#ifdef	HAVE_JIKES_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif

