// $Id: semantic.h,v 1.34 2000/07/25 11:32:33 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef semantic_INCLUDED
#define semantic_INCLUDED

#include "platform.h"
#include "ast.h"
#include "diagnose.h"
#include "error.h"
#include "symbol.h"
#include "control.h"
#include "tuple.h"
#include "set.h"

/*
//FIXME: include stuff
#ifdef HAVE_WCHAR_H
# include <wchar.h>
#endif
*/

#ifdef	HAVE_NAMESPACES
namespace Jikes {	// Open namespace Jikes block
#endif


class cp_info;
class TypeShadowSymbol;

//
//
//
class SymbolTableStack
{
public:
    void Push(SymbolTable *symtab) { table.Next() = symtab; }
    void Pop()                     { if (table.Length() > 0) table.Reset(table.Length() - 1); }
    int Size()                     { return table.Length(); }
    SymbolTable *Top()             { return (SymbolTable *) (table.Length() > 0 ? table[table.Length() - 1] : NULL); }

    SymbolTable *operator[](const int i) { return table[i]; } /*  */

    //
    // Search for a variable in a stack of symbol tables starting at the current symbol table
    // and ending with the symbol table of the method from which this call originates.
    //
    VariableSymbol *FindVariableSymbol(NameSymbol *name_symbol)
    {
        for (int i = table.Length() - 1; i >= 0; i--)
        {
            VariableSymbol *symbol = table[i] -> FindVariableSymbol(name_symbol);
            if (symbol)
                return symbol;
        }
        return (VariableSymbol *) NULL;
    }

    //
    // Search for a type in a stack of symbol tables starting at the current symbol table
    // and ending with the symbol table of the method from which this call originates.
    //
    TypeSymbol* FindTypeSymbol(NameSymbol *name_symbol)
    {
        for (int i = table.Length() - 1; i >= 0; i--)
        {
            TypeSymbol *symbol = table[i] -> FindTypeSymbol(name_symbol);
            if (symbol)
                return symbol;
        }
        return (TypeSymbol *) NULL;
    }

    //
    // Search for a label in a stack of symbol tables starting at the current symbol table
    // and ending with the symbol table of the method from which this call originates.
    //
    LabelSymbol* FindLabelSymbol(NameSymbol *name_symbol)
    {
        for (int i = table.Length() - 1; i >= 0; i--)
        {
            LabelSymbol *label = table[i] -> FindLabelSymbol(name_symbol);
            if (label)
                return label;
        }
        return (LabelSymbol *) NULL;
    }

private:
    Tuple<SymbolTable *> table;
};


//
//
//
class ExceptionTableStack
{
public:
    void Push(SymbolSet *set) { table.Next() = set; }
    void       Pop()          { if (table.Length() > 0) table.Reset(table.Length() - 1); }
    int        Size()         { return table.Length(); }
    SymbolSet *Top()          { return (SymbolSet *) (table.Length() > 0 ? table[table.Length() - 1] : NULL); }

private:
    Tuple<SymbolSet *> table;
};


//
//
//
class StatementStack
{
public:
    void Push(Ast *stmt) { info.Next() = stmt; }
    void Pop()           { if (info.Length() > 0) info.Reset(info.Length() - 1); }
    int Size()           { return info.Length(); }
    Ast *Top()           { return (Ast *) (info.Length() > 0 ? info[info.Length() - 1] : NULL); }

    Ast *operator[](const int i) { return info[i]; }

private:
    Tuple<Ast *> info;
};


//
//
//
class BlockStack
{
public:
    int max_size;

    void Push(AstBlock *block_)
    {
        block.Next() = block_;
        index.Next() = 0;
        if (block.Length() > max_size)
            max_size = block.Length();
    }

    void Pop()
    {
        int len = block.Length() - 1;
        if (len >= 0)
        {
            block.Reset(len);
            index.Reset(len);
        }
    }

    int Size() { return block.Length(); }
    AstBlock *TopBlock() { return (AstBlock *) (block.Length() > 0 ? block[block.Length() - 1] : NULL); }

    AstBlock *operator[](const int i) { return block[i]; }

    int &TopMaxEnclosedVariableIndex()
    {
        if (index.Length() <= 0)
            assert(false);
        return index[index.Length() - 1];
    }

    BlockStack() : max_size(0) {}

private:
    Tuple<AstBlock *> block;
    Tuple<int> index;
};


//
//
//
class DefiniteFinalAssignmentStack
{
public:
    void Push() { info.Next().Reset(); }
    void Pop()  { if (info.Length() > 0) info.Reset(info.Length() - 1); }
    int Size()  { return info.Length(); }
    Tuple <AstExpression *> &Top()  { if (info.Length() == 0) assert(false); return info[info.Length() - 1]; }

private:
    Tuple< Tuple<AstExpression *> > info;
};


//
//
//
class DefiniteSets
{
public:
    DefiniteSets(int set_size) : break_set(set_size),
                                 continue_set(set_size),
                                 return_set(set_size),
                                 throw_set(set_size)
    {}

    BitSet break_set,
           continue_set,
           return_set,
           throw_set;

    void UniverseInit()
    {
        break_set.SetUniverse();
        continue_set.SetUniverse();
        return_set.SetUniverse();
        throw_set.SetUniverse();
    }

    void EmptyInit()
    {
        break_set.SetEmpty();
        continue_set.SetEmpty();
        return_set.SetEmpty();
        throw_set.SetEmpty();
    }
};


//
//
//
class DefiniteBlockStack
{
public:

    void Push(AstBlock *block_)
    {
        definite_sets[top_index] -> UniverseInit();
        final_sets[top_index] -> EmptyInit();

        if (locally_defined_variables)
        {
            if (top_index == 0)
            {
                memset(local_variables[top_index], 0, locally_defined_variables[top_index] -> Size() * sizeof(VariableSymbol *));
                locally_defined_variables[top_index] -> SetEmpty();
            }
            else
            {
                memmove(local_variables[top_index],
                        local_variables[top_index - 1],
                        locally_defined_variables[top_index] -> Size() * sizeof(VariableSymbol *));
                *locally_defined_variables[top_index] = *locally_defined_variables[top_index - 1];
            }
        }

        block[top_index] = block_;
        top_index++;
    }

    void Pop()
    {
        if (top_index > 0)
             top_index--;
        else assert(false);
    }

    int Size()                 { return top_index; }
    AstBlock *Block(int i)     { return block[i]; }
    AstBlock *TopBlock()       { assert(top_index > 0); return block[top_index - 1]; }

    VariableSymbol **TopLocalVariables() { assert(top_index > 0 && local_variables); return local_variables[top_index - 1]; }
    BitSet *TopLocallyDefinedVariables() { assert(top_index > 0 && locally_defined_variables); return locally_defined_variables[top_index - 1]; }

    BitSet &BreakSet(int i)    { return definite_sets[i] -> break_set; }
    BitSet &ContinueSet(int i) { return definite_sets[i] -> continue_set; }
    BitSet &ReturnSet(int i)   { return definite_sets[i] -> return_set; }
    BitSet &ThrowSet(int i)    { return definite_sets[i] -> throw_set; }

    BitSet &TopBreakSet()      { assert(top_index > 0); return definite_sets[top_index - 1] -> break_set; }
    BitSet &TopContinueSet()   { assert(top_index > 0); return definite_sets[top_index - 1] -> continue_set; }
    BitSet &TopReturnSet()     { assert(top_index > 0); return definite_sets[top_index - 1] -> return_set; }
    BitSet &TopThrowSet()      { assert(top_index > 0); return definite_sets[top_index - 1] -> throw_set; }

    BitSet &TopExitSet(BitSet &start_set)
    {
        assert(top_index > 0);

        exit_set  = start_set;
        exit_set *= TopBreakSet();
        exit_set *= TopContinueSet();
        exit_set *= TopReturnSet();
        exit_set *= TopThrowSet();

        return exit_set;
    }

    BitSet &FinalBreakSet(int i)    { return final_sets[i] -> break_set; }
    BitSet &FinalContinueSet(int i) { return final_sets[i] -> continue_set; }
    BitSet &FinalReturnSet(int i)   { return final_sets[i] -> return_set; }
    BitSet &FinalThrowSet(int i)    { return final_sets[i] -> throw_set; }

    BitSet &TopFinalBreakSet()      { assert(top_index > 0); return final_sets[top_index - 1] -> break_set; }
    BitSet &TopFinalContinueSet()   { assert(top_index > 0); return final_sets[top_index - 1] -> continue_set; }
    BitSet &TopFinalReturnSet()     { assert(top_index > 0); return final_sets[top_index - 1] -> return_set; }
    BitSet &TopFinalThrowSet()      { assert(top_index > 0); return final_sets[top_index - 1] -> throw_set; }

    BitSet &TopFinalExitSet(BitSet &start_set)
    {
        assert(top_index > 0);

        exit_set  = start_set;
        exit_set += TopFinalBreakSet();
        exit_set += TopFinalContinueSet();
        exit_set += TopFinalReturnSet();
        exit_set += TopFinalThrowSet();

        return exit_set;
    }

    DefiniteBlockStack(Control &control, int stack_size_, int set_size) : stack_size(stack_size_),
                                                                          top_index(0),
                                                                          exit_set(set_size)
    {
        block = new AstBlock*[stack_size];
        definite_sets = new DefiniteSets*[stack_size];
        final_sets = new DefiniteSets*[stack_size];
        local_variables = (VariableSymbol ***) (control.option.g ? new VariableSymbol**[stack_size] : NULL);
        locally_defined_variables = (BitSet **) (control.option.g ? new BitSet*[stack_size] : NULL);

        for (int i = 0; i < stack_size; i++)
        {
            definite_sets[i] = new DefiniteSets(set_size);
            final_sets[i] = new DefiniteSets(set_size);
            if (local_variables)
            {
                local_variables[i] = new VariableSymbol*[set_size];
                locally_defined_variables[i] = new BitSet(set_size);
            }
        }
    }

    ~DefiniteBlockStack()
    {
        for (int i = 0; i < stack_size; i++)
        {
            delete definite_sets[i];
            delete final_sets[i];
            if (local_variables)
            {
                delete [] local_variables[i];
                delete locally_defined_variables[i];
            }
        }

        delete [] block;
        delete [] definite_sets;
        delete [] final_sets;
        delete [] local_variables;
        delete [] locally_defined_variables;
    }

private:

    int stack_size,
        top_index;
    AstBlock **block;

    DefiniteSets **definite_sets,
                 **final_sets;

    BitSet **locally_defined_variables;
    VariableSymbol ***local_variables;

    BitSet exit_set;
};


//
//
//
class DefiniteTryStack
{
public:

    void Push(AstTryStatement *try_statement_)
    {
        this -> try_statement[top_index] = try_statement_;
        top_index++;
    }

    void Pop()
    {
        if (top_index > 0)
             top_index--;
        else assert(false);
    }

    int Size()                           { return top_index; }
    AstTryStatement *TryStatement(int i) { return try_statement[i]; }
    AstBlock *Block(int i)               { return block[i]; }
    AstBlock *TopBlock()                 { assert(top_index > 0); return block[top_index - 1]; }
    void SetTopBlock(AstBlock *block_)   { assert(top_index > 0); block[top_index - 1] = block_; }

    DefiniteTryStack(int stack_size_) : stack_size(stack_size_),
                                        top_index(0)
    {
        block = new AstBlock*[stack_size];
        try_statement = new AstTryStatement*[stack_size];
    }

    ~DefiniteTryStack()
    {
        delete [] block;
        delete [] try_statement;
    }

private:

    int stack_size,
        top_index;
    AstBlock **block;
    AstTryStatement **try_statement;
};


//
//
//
class SemanticEnvironment
{
public:

    Semantic *sem;
    SemanticEnvironment *previous;

    MethodSymbol   *this_method;
    VariableSymbol *this_variable;
    Ast            *explicit_constructor_invocation;
    Ast            *ast_construct;

    SymbolTableStack symbol_table; // Points to symbol table on top of stack
    ExceptionTableStack try_exception_table_stack;
    StatementStack try_statement_stack,
                   breakable_statement_stack,
                   continuable_statement_stack;
    BlockStack block_stack;

    SemanticEnvironment(Semantic *sem_, TypeSymbol *type_, SemanticEnvironment *previous_ = NULL)
            : sem(sem_),
              previous(previous_),
              this_method(NULL),
              this_variable(NULL),
              explicit_constructor_invocation(NULL),
              ast_construct(NULL),
              _type(type_),
              next(NULL)
    {}


    ~SemanticEnvironment()
    {
        delete next; // if there was any clone, get rid of it
    }

    //
    // Clone the immediate environment of "this" Semantic environment.
    // The immediate environment consists primarily of the stack of symbol
    // tables that are necessary for looking up local variables in the immediate
    // environment.
    //
    SemanticEnvironment *GetEnvironment(Ast *ast)
    {
        SemanticEnvironment *clone = new SemanticEnvironment(sem, _type, NULL);
        clone -> this_method = this -> this_method;
        clone -> this_variable = this -> this_variable;
        clone -> ast_construct = ast;
        for (int i = 0; i < this -> symbol_table.Size(); i++)
            clone -> symbol_table.Push(this -> symbol_table[i]);
        clone -> next = this -> next;
        this -> next = clone;

        return clone;
    }

    TypeSymbol *Type() { return _type; }

    //
    // Are we in a static area ?
    //
    inline bool StaticRegion()
    {
        return (this_variable && this_variable -> ACC_STATIC()) ||
               (this_method   && this_method -> ACC_STATIC())   ||
               (_type -> ACC_INTERFACE());
    }

private:

    TypeSymbol *_type;
    SemanticEnvironment *next; // use to link an environment to its clones.
};


//
//
//
class SemanticEnvironmentStack
{
public:
    void Push(SemanticEnvironment *env) { info.Next() = env; }

    void Pop()
    {
        if (info.Length() > 0)
            info.Reset(info.Length() - 1);

        return;
    }

    int Size() { return info.Length(); }

    SemanticEnvironment *Top() { return (SemanticEnvironment *) (info.Length() > 0 ? info[info.Length() - 1] : NULL); }

    SemanticEnvironment *operator[](const int i) { return info[i]; }

private:
    Tuple<SemanticEnvironment *> info;
};


class Semantic
{
public:
    //
    //
    //
    Control &control;
    FileSymbol *source_file_symbol;
    LexStream *lex_stream;
    AstCompilationUnit *compilation_unit;
    DirectorySymbol *directory_symbol;

    SymbolSet types_to_be_processed;

    int return_code;

    PackageSymbol *Package() { return this_package; }

    void CheckPackage();
    void ProcessTypeNames();
    void ProcessImports();
    void ProcessSuperTypeDependences(AstClassDeclaration *);
    void ProcessSuperTypeDependences(AstInterfaceDeclaration *);

    LiteralValue *ComputeFinalValue(AstVariableDeclarator *);

    Semantic(Control &control_, FileSymbol *file_symbol_) : control(control_),
                                                            source_file_symbol(file_symbol_),
                                                            lex_stream(file_symbol_ -> lex_stream),
                                                            compilation_unit(file_symbol_ -> compilation_unit),
                                                            directory_symbol(file_symbol_ -> directory_symbol),
                                                            return_code(0),
                                                            error(NULL),
                                                            this_package(file_symbol_ -> package),
                                                            definitely_assigned_variables(NULL),
                                                            possibly_assigned_finals(NULL),
                                                            universe(NULL),
                                                            definite_block_stack(NULL),
                                                            definite_try_stack(NULL),
                                                            definite_final_assignment_stack(NULL),
                                                            definite_visible_variables(NULL)
    {
        ProcessExprOrStmt[Ast::LOCAL_VARIABLE_DECLARATION] = &Semantic::ProcessLocalVariableDeclarationStatement;
        ProcessExprOrStmt[Ast::BLOCK]                      = &Semantic::ProcessBlock;
        ProcessExprOrStmt[Ast::EXPRESSION_STATEMENT]       = &Semantic::ProcessExpressionStatement;
        ProcessExprOrStmt[Ast::SYNCHRONIZED_STATEMENT]     = &Semantic::ProcessSynchronizedStatement;
        ProcessExprOrStmt[Ast::IF]                         = &Semantic::ProcessIfStatement;
        ProcessExprOrStmt[Ast::WHILE]                      = &Semantic::ProcessWhileStatement;
        ProcessExprOrStmt[Ast::FOR]                        = &Semantic::ProcessForStatement;
        ProcessExprOrStmt[Ast::SWITCH]                     = &Semantic::ProcessSwitchStatement;
        ProcessExprOrStmt[Ast::DO]                         = &Semantic::ProcessDoStatement;
        ProcessExprOrStmt[Ast::BREAK]                      = &Semantic::ProcessBreakStatement;
        ProcessExprOrStmt[Ast::CONTINUE]                   = &Semantic::ProcessContinueStatement;
        ProcessExprOrStmt[Ast::RETURN]                     = &Semantic::ProcessReturnStatement;
        ProcessExprOrStmt[Ast::THROW]                      = &Semantic::ProcessThrowStatement;
        ProcessExprOrStmt[Ast::TRY]                        = &Semantic::ProcessTryStatement;
        ProcessExprOrStmt[Ast::EMPTY_STATEMENT]            = &Semantic::ProcessEmptyStatement;
        ProcessExprOrStmt[Ast::CLASS]                      = &Semantic::ProcessClassDeclaration;

        ProcessExprOrStmt[Ast::IDENTIFIER]               = &Semantic::ProcessSimpleName;
        ProcessExprOrStmt[Ast::DOT]                      = &Semantic::ProcessFieldAccess;
        ProcessExprOrStmt[Ast::INTEGER_LITERAL]          = &Semantic::ProcessIntegerLiteral;
        ProcessExprOrStmt[Ast::LONG_LITERAL]             = &Semantic::ProcessLongLiteral;
        ProcessExprOrStmt[Ast::FLOATING_POINT_LITERAL]   = &Semantic::ProcessFloatingPointLiteral;
        ProcessExprOrStmt[Ast::DOUBLE_LITERAL]           = &Semantic::ProcessDoubleLiteral;
        ProcessExprOrStmt[Ast::TRUE_LITERAL]             = &Semantic::ProcessTrueLiteral;
        ProcessExprOrStmt[Ast::FALSE_LITERAL]            = &Semantic::ProcessFalseLiteral;
        ProcessExprOrStmt[Ast::STRING_LITERAL]           = &Semantic::ProcessStringLiteral;
        ProcessExprOrStmt[Ast::CHARACTER_LITERAL]        = &Semantic::ProcessCharacterLiteral;
        ProcessExprOrStmt[Ast::NULL_LITERAL]             = &Semantic::ProcessNullLiteral;
        ProcessExprOrStmt[Ast::ARRAY_ACCESS]             = &Semantic::ProcessArrayAccess;
        ProcessExprOrStmt[Ast::CALL]                     = &Semantic::ProcessMethodInvocation;
        ProcessExprOrStmt[Ast::THIS_EXPRESSION]          = &Semantic::ProcessThisExpression;
        ProcessExprOrStmt[Ast::SUPER_EXPRESSION]         = &Semantic::ProcessSuperExpression;
        ProcessExprOrStmt[Ast::PARENTHESIZED_EXPRESSION] = &Semantic::ProcessParenthesizedExpression;
        ProcessExprOrStmt[Ast::CLASS_CREATION]           = &Semantic::ProcessClassInstanceCreationExpression;
        ProcessExprOrStmt[Ast::ARRAY_CREATION]           = &Semantic::ProcessArrayCreationExpression;
        ProcessExprOrStmt[Ast::POST_UNARY]               = &Semantic::ProcessPostUnaryExpression;
        ProcessExprOrStmt[Ast::PRE_UNARY]                = &Semantic::ProcessPreUnaryExpression;
        ProcessExprOrStmt[Ast::CAST]                     = &Semantic::ProcessCastExpression;
        ProcessExprOrStmt[Ast::BINARY]                   = &Semantic::ProcessBinaryExpression;
        ProcessExprOrStmt[Ast::TYPE]                     = &Semantic::ProcessTypeExpression;
        ProcessExprOrStmt[Ast::CONDITIONAL]              = &Semantic::ProcessConditionalExpression;
        ProcessExprOrStmt[Ast::ASSIGNMENT]               = &Semantic::ProcessAssignmentExpression;

        DefiniteStmt[Ast::LOCAL_VARIABLE_DECLARATION] = &Semantic::DefiniteLocalVariableDeclarationStatement;
        DefiniteStmt[Ast::BLOCK]                      = &Semantic::DefiniteBlock;
        DefiniteStmt[Ast::EXPRESSION_STATEMENT]       = &Semantic::DefiniteExpressionStatement;
        DefiniteStmt[Ast::SYNCHRONIZED_STATEMENT]     = &Semantic::DefiniteSynchronizedStatement;
        DefiniteStmt[Ast::IF]                         = &Semantic::DefiniteIfStatement;
        DefiniteStmt[Ast::WHILE]                      = &Semantic::DefiniteWhileStatement;
        DefiniteStmt[Ast::FOR]                        = &Semantic::DefiniteForStatement;
        DefiniteStmt[Ast::SWITCH]                     = &Semantic::DefiniteSwitchStatement;
        DefiniteStmt[Ast::DO]                         = &Semantic::DefiniteDoStatement;
        DefiniteStmt[Ast::BREAK]                      = &Semantic::DefiniteBreakStatement;
        DefiniteStmt[Ast::CONTINUE]                   = &Semantic::DefiniteContinueStatement;
        DefiniteStmt[Ast::RETURN]                     = &Semantic::DefiniteReturnStatement;
        DefiniteStmt[Ast::THROW]                      = &Semantic::DefiniteThrowStatement;
        DefiniteStmt[Ast::TRY]                        = &Semantic::DefiniteTryStatement;
        DefiniteStmt[Ast::EMPTY_STATEMENT]            = &Semantic::DefiniteEmptyStatement;
        DefiniteStmt[Ast::CLASS]                      = &Semantic::DefiniteClassDeclaration;

        DefiniteExpr[Ast::IDENTIFIER]               = &Semantic::DefiniteSimpleName;
        DefiniteExpr[Ast::DOT]                      = &Semantic::DefiniteFieldAccess;
        DefiniteExpr[Ast::ARRAY_ACCESS]             = &Semantic::DefiniteArrayAccess;
        DefiniteExpr[Ast::CALL]                     = &Semantic::DefiniteMethodInvocation;
        DefiniteExpr[Ast::PARENTHESIZED_EXPRESSION] = &Semantic::DefiniteParenthesizedExpression;
        DefiniteExpr[Ast::CLASS_CREATION]           = &Semantic::DefiniteClassInstanceCreationExpression;
        DefiniteExpr[Ast::ARRAY_CREATION]           = &Semantic::DefiniteArrayCreationExpression;
        DefiniteExpr[Ast::POST_UNARY]               = &Semantic::DefinitePostUnaryExpression;
        DefiniteExpr[Ast::PRE_UNARY]                = &Semantic::DefinitePreUnaryExpression;
        DefiniteExpr[Ast::CAST]                     = &Semantic::DefiniteCastExpression;
        DefiniteExpr[Ast::CHECK_AND_CAST]           = &Semantic::DefiniteCastExpression;
        DefiniteExpr[Ast::BINARY]                   = &Semantic::DefiniteBinaryExpression;
        DefiniteExpr[Ast::CONDITIONAL]              = &Semantic::DefiniteConditionalExpression;
        DefiniteExpr[Ast::ASSIGNMENT]               = &Semantic::DefiniteAssignmentExpression;

        DefiniteExpr[Ast::INTEGER_LITERAL]          = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::LONG_LITERAL]             = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::FLOATING_POINT_LITERAL]   = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::DOUBLE_LITERAL]           = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::TRUE_LITERAL]             = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::FALSE_LITERAL]            = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::STRING_LITERAL]           = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::CHARACTER_LITERAL]        = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::NULL_LITERAL]             = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::THIS_EXPRESSION]          = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::SUPER_EXPRESSION]         = &Semantic::DefiniteDefaultExpression;
        DefiniteExpr[Ast::TYPE]                     = &Semantic::DefiniteDefaultExpression;

        DefiniteBinaryExpr[AstBinaryExpression::PLUS]                 = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::LEFT_SHIFT]           = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::RIGHT_SHIFT]          = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::UNSIGNED_RIGHT_SHIFT] = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::LESS]                 = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::GREATER]              = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::LESS_EQUAL]           = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::GREATER_EQUAL]        = &Semantic::DefiniteDefaultBinaryExpression;

        //
        // TODO: Remove this comment as well as the functions:
        //
        //     DefiniteAND
        //     DefiniteXOR
        //     DefiniteIOR
        //
        //     DefiniteEQUAL
        //     DefiniteNOT_EQUAL
        //
        // when revised spec is released !!!
        //
        //****************************************************************************************
        //
        // To: Martin Odersky <Martin.Odersky@epfl.ch>
        // cc: David Shields/Watson/IBM@IBMUS, compiler@eng.sun.com,
        // gilad.bracha@eng.sun.com, jrose@eng.sun.com,
        // innerclass-comments@lukasiewicz.eng.sun.com, guy.steele@east.sun.com,
        // peter.kessler@eng.sun.com
        // Subject: Re: Query #32 to Sun: Verification problem
        //
        //
        //
        //
        // On Thu, 29 Jul 1999, Martin Odersky wrote:
        //
        // > It seems a very undesirable state of affairs if the Java spec for |,
        // > &, ^ and the JVM spec for iand, ior, ixor differ in their definite
        // > assignment properties. Assuming that it's unreasonable to make the
        // > verifier implement the current JLS spec one-to-one, would it be
        // > possible to tighten the JLS so that definite assignment for |, & ,^ is
        // > done in the way the verifier does it? I doubt that such a tightening
        // > would invalidate any real Java programs.
        //
        // Gilad and I decided today that ammending the JLS was the best course of
        // action, and then discovered that Guy had already done so in our working
        // draft of the revised specification.  We anticipate that when a draft is
        // made
        // available for public review, it will drop sections 16.1.6, 16.1.7, and
        // 16.1.8, which specify special-case rules for the &, |, and ^ operators.
        // They will be covered instead by the general case for expressions with
        // subexpressions, which reads as follows in the current working draft:
        //
        //  If the expression has subexpressions, V is [un]assigned
        //  after the expression iff V is [un]assigned after its rightmost
        //  immediate subexpression.
        //
        // Note that the when-true and when-false cases, currently distinguished
        // for these operators, are now coalesced, as in the general case.
        //
        // Pending an official change to the specification, we recommend that
        // compiler implementors follow the proposed ammendments.
        //
        // Due to the fact that 'javac' has never been in compliance with the
        // current JLS on this issue, nor will much (all?) code that
        // relies on the difference actually verify and execute, compatibility
        // implications should be minimal.
        //
        // --Bill
        //
        //------------------------------------------------------------------------------------------------
        //
        // To: David Shields/Watson/IBM@IBMUS
        // cc: Martin Odersky <Martin.Odersky@epfl.ch>, compiler@eng.sun.com, gilad.bracha@eng.sun.com,
        // jrose@eng.sun.com, innerclass-comments@lukasiewicz.eng.sun.com, guy.steele@east.sun.com,
        // peter.kessler@eng.sun.com
        // Subject: Re: Query #32 to Sun: Verification problem
        //
        // On Mon, 2 Aug 1999 shieldsd@us.ibm.com wrote:
        //
        // > Do you still plan to retain 16.1.3, 16.1.4 and 16.1.11? Since we follow them as
        // > currently written, we accept some of the positive 1.2 JCK tests that javac
        // > currently rejects, but the resulting class files fail verification. Examples
        // > include dasg02901, dasg03001, dasg03301, dasg03303, dasg03401, dasg03501,
        // > dasg04501, dasg04601, dasg04701 and dasg05001).
        // >
        // > dave and philippe
        //
        // We plan to keep these.  However, the special rules for '==' (16.1.9) and
        // '!=' (16.1.10) have also been dropped, again because the bytecodes for these
        // operators create materialized boolean values on the stack at runtime.
        //
        // Guy and Gilad:  In my copy of the draft, the second-to-last sentence on
        // page p.395 claims otherwise, but sections 16.1.9 and 16.1.10 have in fact
        // been deleted.
        //
        // --Bill
        //
        DefiniteBinaryExpr[AstBinaryExpression::AND]                  = &Semantic::DefiniteDefaultBinaryExpression;// &Semantic::DefiniteAND;
        DefiniteBinaryExpr[AstBinaryExpression::XOR]                  = &Semantic::DefiniteDefaultBinaryExpression;// &Semantic::DefiniteXOR;
        DefiniteBinaryExpr[AstBinaryExpression::IOR]                  = &Semantic::DefiniteDefaultBinaryExpression;// &Semantic::DefiniteIOR;

        DefiniteBinaryExpr[AstBinaryExpression::EQUAL_EQUAL]          = &Semantic::DefiniteDefaultBinaryExpression; // &Semantic::DefiniteEQUAL_EQUAL;
        DefiniteBinaryExpr[AstBinaryExpression::NOT_EQUAL]            = &Semantic::DefiniteDefaultBinaryExpression; // &Semantic::DefiniteNOT_EQUAL;

        DefiniteBinaryExpr[AstBinaryExpression::AND_AND]              = &Semantic::DefiniteAND_AND;
        DefiniteBinaryExpr[AstBinaryExpression::OR_OR]                = &Semantic::DefiniteOR_OR;
        DefiniteBinaryExpr[AstBinaryExpression::STAR]                 = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::MINUS]                = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::SLASH]                = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::MOD]                  = &Semantic::DefiniteDefaultBinaryExpression;
        DefiniteBinaryExpr[AstBinaryExpression::INSTANCEOF]           = &Semantic::DefiniteDefaultBinaryExpression;

        DefinitePreUnaryExpr[AstPreUnaryExpression::PLUS]             = &Semantic::DefiniteDefaultPreUnaryExpression;
        DefinitePreUnaryExpr[AstPreUnaryExpression::MINUS]            = &Semantic::DefiniteDefaultPreUnaryExpression;
        DefinitePreUnaryExpr[AstPreUnaryExpression::TWIDDLE]          = &Semantic::DefiniteDefaultPreUnaryExpression;
        DefinitePreUnaryExpr[AstPreUnaryExpression::NOT]              = &Semantic::DefiniteNOT;
        DefinitePreUnaryExpr[AstPreUnaryExpression::PLUSPLUS]         = &Semantic::DefinitePLUSPLUSOrMINUSMINUS;
        DefinitePreUnaryExpr[AstPreUnaryExpression::MINUSMINUS]       = &Semantic::DefinitePLUSPLUSOrMINUSMINUS;

        ProcessBinaryExpr[AstBinaryExpression::PLUS]                 = &Semantic::ProcessPLUS;
        ProcessBinaryExpr[AstBinaryExpression::LEFT_SHIFT]           = &Semantic::ProcessLEFT_SHIFT;
        ProcessBinaryExpr[AstBinaryExpression::RIGHT_SHIFT]          = &Semantic::ProcessRIGHT_SHIFT;
        ProcessBinaryExpr[AstBinaryExpression::UNSIGNED_RIGHT_SHIFT] = &Semantic::ProcessUNSIGNED_RIGHT_SHIFT;
        ProcessBinaryExpr[AstBinaryExpression::LESS]                 = &Semantic::ProcessLESS;
        ProcessBinaryExpr[AstBinaryExpression::GREATER]              = &Semantic::ProcessGREATER;
        ProcessBinaryExpr[AstBinaryExpression::LESS_EQUAL]           = &Semantic::ProcessLESS_EQUAL;
        ProcessBinaryExpr[AstBinaryExpression::GREATER_EQUAL]        = &Semantic::ProcessGREATER_EQUAL;
        ProcessBinaryExpr[AstBinaryExpression::AND]                  = &Semantic::ProcessAND;
        ProcessBinaryExpr[AstBinaryExpression::XOR]                  = &Semantic::ProcessXOR;
        ProcessBinaryExpr[AstBinaryExpression::IOR]                  = &Semantic::ProcessIOR;
        ProcessBinaryExpr[AstBinaryExpression::AND_AND]              = &Semantic::ProcessAND_AND;
        ProcessBinaryExpr[AstBinaryExpression::OR_OR]                = &Semantic::ProcessOR_OR;
        ProcessBinaryExpr[AstBinaryExpression::EQUAL_EQUAL]          = &Semantic::ProcessEQUAL_EQUAL;
        ProcessBinaryExpr[AstBinaryExpression::NOT_EQUAL]            = &Semantic::ProcessNOT_EQUAL;
        ProcessBinaryExpr[AstBinaryExpression::STAR]                 = &Semantic::ProcessSTAR;
        ProcessBinaryExpr[AstBinaryExpression::MINUS]                = &Semantic::ProcessMINUS;
        ProcessBinaryExpr[AstBinaryExpression::SLASH]                = &Semantic::ProcessSLASH;
        ProcessBinaryExpr[AstBinaryExpression::MOD]                  = &Semantic::ProcessMOD;
        ProcessBinaryExpr[AstBinaryExpression::INSTANCEOF]           = &Semantic::ProcessINSTANCEOF;

        ProcessPreUnaryExpr[AstPreUnaryExpression::PLUS]       = &Semantic::ProcessPLUS;
        ProcessPreUnaryExpr[AstPreUnaryExpression::MINUS]      = &Semantic::ProcessMINUS;
        ProcessPreUnaryExpr[AstPreUnaryExpression::TWIDDLE]    = &Semantic::ProcessTWIDDLE;
        ProcessPreUnaryExpr[AstPreUnaryExpression::NOT]        = &Semantic::ProcessNOT;
        ProcessPreUnaryExpr[AstPreUnaryExpression::PLUSPLUS]   = &Semantic::ProcessPLUSPLUSOrMINUSMINUS;
        ProcessPreUnaryExpr[AstPreUnaryExpression::MINUSMINUS] = &Semantic::ProcessPLUSPLUSOrMINUSMINUS;
    }

    ~Semantic() { delete error; }

    void ReportSemError(SemanticError::SemanticErrorKind kind,
                        LexStream::TokenIndex ltok,
                        LexStream::TokenIndex rtok,
                        wchar_t *s1 = NULL,
                        wchar_t *s2 = NULL,
                        wchar_t *s3 = NULL,
                        wchar_t *s4 = NULL,
                        wchar_t *s5 = NULL,
                        wchar_t *s6 = NULL,
                        wchar_t *s7 = NULL,
                        wchar_t *s8 = NULL,
                        wchar_t *s9 = NULL)
    {
        if (! error)
            error = new SemanticError(control, source_file_symbol);
        error -> Report(kind, ltok, rtok, s1, s2, s3, s4, s5, s6, s7, s8, s9);
    }

    int NumErrors() { return (error ? error -> num_errors : 0); }

    //
    // If we had a bad compilation unit, print the parser messages.
    // If semantic errors were detected print them too.
    // Set the return code.
    //
    void PrintMessages()
    {
        if (this != control.system_semantic)
        {
            if (lex_stream -> NumBadTokens() > 0)
            {
                lex_stream -> PrintMessages();
                return_code = 1;
            }

            if ((! compilation_unit) || compilation_unit -> BadCompilationUnitCast())
            {
                DiagnoseParser *diagnose_parser = new DiagnoseParser(control, lex_stream);
                return_code = 1;
                delete diagnose_parser;
            }

            if (! control.option.nocleanup)
            if (compilation_unit)
                CleanUp();
        }

        if (error && error -> error.Length() > 0 && error -> PrintMessages() > return_code)
            return_code = 1;

        //
        // Once we have processed the errors, reset the error object
        //
        delete error;
        error = NULL;

        return;
    }

    TypeSymbol *ProcessSignature(TypeSymbol *, const char *, LexStream::TokenIndex);
    TypeSymbol *ReadType(FileSymbol *, PackageSymbol *, NameSymbol *, LexStream::TokenIndex);
    TypeSymbol *ReadTypeFromSignature(TypeSymbol *, const char *, int, LexStream::TokenIndex);
    TypeSymbol *ProcessNestedType(TypeSymbol *, NameSymbol *, LexStream::TokenIndex);

private:

    SemanticError *error;

    void CleanUp();
    void CleanUpType(TypeSymbol *);

    void SetDefaultSuperType(AstClassDeclaration *);
    void SetDefaultSuperType(AstInterfaceDeclaration *);
    void ProcessTypeHeader(AstClassDeclaration *);
    void MarkCircularNest(TypeSymbol *);
    void ProcessSuperTypesOfOuterType(TypeSymbol *);
    void ProcessSuperTypesOfInnerType(TypeSymbol *, Tuple<TypeSymbol *> &);
    void ProcessTypeHeaders(AstClassDeclaration *);
    TypeSymbol *FindTypeInLayer(Ast *, SymbolSet &);
    void ProcessNestedSuperTypes(TypeSymbol *);
    void ProcessNestedTypeHeaders(TypeSymbol *, AstClassBody *);
    void ProcessTypeHeader(AstInterfaceDeclaration *);
    void ProcessTypeHeaders(AstInterfaceDeclaration *);
    void ProcessNestedTypeHeaders(AstInterfaceDeclaration *);
    void ProcessConstructorMembers(AstClassBody *);
    void ProcessMethodMembers(AstClassBody *);
    void ProcessFieldMembers(AstClassBody *);
    void ProcessMembers(SemanticEnvironment *, AstClassBody *);
    void CompleteSymbolTable(SemanticEnvironment *, LexStream::TokenIndex, AstClassBody *);
    void ProcessExecutableBodies(SemanticEnvironment *, AstClassBody *);
    void ProcessExecutableBodies(AstInterfaceDeclaration *);

    void ProcessMethodMembers(AstInterfaceDeclaration *);
    void ProcessFieldMembers(AstInterfaceDeclaration *);
    void ProcessMembers(AstInterfaceDeclaration *);
    void CompleteSymbolTable(AstInterfaceDeclaration *);

    friend class TypeSymbol;

    Tuple<Symbol *> import_on_demand_packages;
    Tuple<TypeSymbol *> single_type_imports;

    //
    // Where am I?
    //
    PackageSymbol  *this_package;

    TypeSymbol *ThisType()                        { assert(state_stack.Size()); return state_stack.Top() -> Type(); }
    MethodSymbol *&ThisMethod()                   { assert(state_stack.Size()); return state_stack.Top() -> this_method; }
    VariableSymbol *&ThisVariable()               { assert(state_stack.Size()); return state_stack.Top() -> this_variable; }
    Ast *&ExplicitConstructorInvocation()         { assert(state_stack.Size()); return state_stack.Top() -> explicit_constructor_invocation; }
    SymbolTableStack &LocalSymbolTable()          { assert(state_stack.Size()); return state_stack.Top() -> symbol_table; }
    ExceptionTableStack &TryExceptionTableStack() { assert(state_stack.Size()); return state_stack.Top() -> try_exception_table_stack; }
    StatementStack &TryStatementStack()           { assert(state_stack.Size()); return state_stack.Top() -> try_statement_stack; }
    StatementStack &BreakableStatementStack()     { assert(state_stack.Size()); return state_stack.Top() -> breakable_statement_stack; }
    StatementStack &ContinuableStatementStack()   { assert(state_stack.Size()); return state_stack.Top() -> continuable_statement_stack; }
    BlockStack &LocalBlockStack()                 { assert(state_stack.Size()); return state_stack.Top() -> block_stack; }
    SemanticEnvironment *GetEnvironment(Ast *ast) { assert(state_stack.Size()); return state_stack.Top() -> GetEnvironment(ast); }
    bool StaticRegion()                           { assert(state_stack.Size()); return state_stack.Top() -> StaticRegion(); }

    SemanticEnvironmentStack state_stack;

    BitSet *definitely_assigned_variables,
           *possibly_assigned_finals,
           *universe;
    DefiniteBlockStack *definite_block_stack;
    DefiniteTryStack *definite_try_stack;
    DefiniteFinalAssignmentStack *definite_final_assignment_stack;
    SymbolSet *definite_visible_variables;

    bool IsIntValueRepresentableInType(AstExpression *, TypeSymbol *);

    void CheckClassMembers(TypeSymbol *, AstClassBody *);
    void CheckNestedTypeDuplication(SemanticEnvironment *, LexStream::TokenIndex);
    TypeSymbol *ProcessNestedClassName(TypeSymbol *, AstClassDeclaration *);
    void CheckInterfaceMembers(TypeSymbol *, AstInterfaceDeclaration *);
    TypeSymbol *ProcessNestedInterfaceName(TypeSymbol *, AstInterfaceDeclaration *);
    TypeSymbol *FindTypeInShadow(TypeShadowSymbol *, LexStream::TokenIndex);
    void ReportTypeInaccessible(LexStream::TokenIndex, LexStream::TokenIndex, TypeSymbol *);
    void ReportTypeInaccessible(Ast *ast, TypeSymbol *type) { ReportTypeInaccessible(ast -> LeftToken(), ast -> RightToken(), type); }
    TypeSymbol *GetBadNestedType(TypeSymbol *, LexStream::TokenIndex);
    TypeSymbol *FindNestedType(TypeSymbol *, LexStream::TokenIndex);
    TypeSymbol *MustFindNestedType(TypeSymbol *, Ast *);
    void ProcessImportQualifiedName(AstExpression *);
    void ProcessPackageOrType(AstExpression *);
    void ProcessTypeImportOnDemandDeclaration(AstImportDeclaration *);
    AstExpression *FindFirstType(Ast *);
    TypeSymbol *FindSimpleNameType(PackageSymbol *, LexStream::TokenIndex);
    void ProcessSingleTypeImportDeclaration(AstImportDeclaration *);
    AccessFlags ProcessClassModifiers(AstClassDeclaration *);
    AccessFlags ProcessLocalClassModifiers(AstClassDeclaration *);
    AccessFlags ProcessNestedClassModifiers(AstClassDeclaration *);
    AccessFlags ProcessStaticNestedClassModifiers(AstClassDeclaration *);
    AccessFlags ProcessInterfaceModifiers(AstInterfaceDeclaration *);
    AccessFlags ProcessNestedInterfaceModifiers(AstInterfaceDeclaration *);
    AccessFlags ProcessStaticNestedInterfaceModifiers(AstInterfaceDeclaration *);
    AccessFlags ProcessFieldModifiers(AstFieldDeclaration *);
    AccessFlags ProcessLocalModifiers(AstLocalVariableDeclarationStatement *);
    AccessFlags ProcessFormalModifiers(AstFormalParameter *);
    AccessFlags ProcessMethodModifiers(AstMethodDeclaration *);
    AccessFlags ProcessConstructorModifiers(AstConstructorDeclaration *);
    AccessFlags ProcessConstantModifiers(AstFieldDeclaration *);
    AccessFlags ProcessInterfaceMethodModifiers(AstMethodDeclaration *);
    void AddDefaultConstructor(TypeSymbol *);
    void ProcessConstructorDeclaration(AstConstructorDeclaration *);
    void ProcessMethodDeclaration(AstMethodDeclaration *);
    void ProcessFieldDeclaration(AstFieldDeclaration *);
    void ProcessFormalParameters(BlockSymbol *, AstMethodDeclarator *);
    TypeSymbol *ImportType(LexStream::TokenIndex, NameSymbol *);
    TypeSymbol *FindPrimitiveType(AstPrimitiveType *);
    TypeSymbol *FindTypeInEnvironment(SemanticEnvironment *, NameSymbol *);
    TypeSymbol *FindType(LexStream::TokenIndex);
    TypeSymbol *MustFindType(Ast *);
    void ProcessInterface(TypeSymbol *, AstExpression *);

    void InitializeVariable(AstFieldDeclaration *, Tuple<VariableSymbol *> &);
    void ProcessInitializer(AstBlock *, AstBlock *, MethodSymbol *, Tuple<VariableSymbol *> &);
    bool NeedsInitializationMethod(AstFieldDeclaration *);
    void ProcessStaticInitializers(AstClassBody *);
    void ProcessBlockInitializers(AstClassBody *);

    bool CanWideningPrimitiveConvert(TypeSymbol *, TypeSymbol *);
    bool CanNarrowingPrimitiveConvert(TypeSymbol *, TypeSymbol *);
    bool CanCastConvert(TypeSymbol *, TypeSymbol *, LexStream::TokenIndex);
    bool CanMethodInvocationConvert(TypeSymbol *, TypeSymbol *);
    bool CanAssignmentConvert(TypeSymbol *, AstExpression *);
    bool CanAssignmentConvertReference(TypeSymbol *, TypeSymbol *);
    LiteralValue *CastPrimitiveValue(TypeSymbol *, AstExpression *);
    LiteralValue *CastValue(TypeSymbol *, AstExpression *);
    AstExpression *ConvertToType(AstExpression *, TypeSymbol *);
    AstExpression *PromoteUnaryNumericExpression(AstExpression *);
    void BinaryNumericPromotion(AstAssignmentExpression *);
    void BinaryNumericPromotion(AstBinaryExpression *);
    void BinaryNumericPromotion(AstConditionalExpression *);

    void (Semantic::*DefiniteStmt[Ast::_num_kinds])(Ast *);
    inline void DefiniteStatement(Ast *);

    void DefiniteLoopBody(AstStatement *);

    void DefiniteBlock(Ast *);
    void DefiniteLocalVariableDeclarationStatement(Ast *);
    void DefiniteExpressionStatement(Ast *);
    void DefiniteSynchronizedStatement(Ast *);
    void DefiniteIfStatement(Ast *);
    void DefiniteWhileStatement(Ast *);
    void DefiniteForStatement(Ast *);
    void DefiniteSwitchStatement(Ast *);
    void DefiniteDoStatement(Ast *);
    void DefiniteBreakStatement(Ast *);
    void DefiniteContinueStatement(Ast *);
    void DefiniteReturnStatement(Ast *);
    void DefiniteThrowStatement(Ast *);
    void DefiniteTryStatement(Ast *);
    void DefiniteEmptyStatement(Ast *);
    void DefiniteClassDeclaration(Ast *);

    VariableSymbol *DefiniteFinal(AstFieldAccess *);

    DefiniteAssignmentSet *(Semantic::*DefiniteExpr[Ast::_num_expression_kinds])(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteSimpleName(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteArrayAccess(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteMethodInvocation(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteClassInstanceCreationExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteArrayCreationExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefinitePreUnaryExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefinitePostUnaryExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteBinaryExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteConditionalExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteAssignmentExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteDefaultExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteFieldAccess(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteParenthesizedExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteCastExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteExpression(AstExpression *, BitSet &);

    DefiniteAssignmentSet *(Semantic::*DefinitePreUnaryExpr[AstPreUnaryExpression::_num_kinds])(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteDefaultPreUnaryExpression(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteNOT(AstExpression *, BitSet &);
    DefiniteAssignmentSet *DefinitePLUSPLUSOrMINUSMINUS(AstExpression *, BitSet &);

    DefiniteAssignmentSet *(Semantic::*DefiniteBinaryExpr[AstBinaryExpression::_num_kinds])(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteDefaultBinaryExpression(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteAND(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteIOR(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteXOR(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteAND_AND(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteOR_OR(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteEQUAL_EQUAL(AstBinaryExpression *, BitSet &);
    DefiniteAssignmentSet *DefiniteNOT_EQUAL(AstBinaryExpression *, BitSet &);

    DefiniteAssignmentSet *DefiniteAssignmentAND(TypeSymbol *, BitSet *, BitSet &, DefiniteAssignmentSet *, DefiniteAssignmentSet *);
    DefiniteAssignmentSet *DefiniteAssignmentIOR(TypeSymbol *, BitSet *, BitSet &, DefiniteAssignmentSet *, DefiniteAssignmentSet *);
    DefiniteAssignmentSet *DefiniteAssignmentXOR(TypeSymbol *, BitSet *, BitSet &, DefiniteAssignmentSet *, DefiniteAssignmentSet *);

    void DefiniteArrayInitializer(AstArrayInitializer *);
    void DefiniteVariableInitializer(AstVariableDeclarator *);
    void DefiniteBlockStatements(AstBlock *);
    void DefiniteMethodBody(AstMethodDeclaration *, Tuple<VariableSymbol *> &);
    void DefiniteConstructorBody(AstConstructorDeclaration *, Tuple<VariableSymbol *> &);
    void DefiniteBlockInitializer(AstBlock *, int, Tuple<VariableSymbol *> &);
    void DefiniteVariableInitializer(AstVariableDeclarator *, Tuple<VariableSymbol *> &);

    void ProcessBlockStatements(AstBlock *);
    void ProcessThisCall(AstThisCall *);
    void ProcessSuperCall(AstSuperCall *);
    void CheckThrow(AstExpression *);
    void ProcessMethodBody(AstMethodDeclaration *);
    void ProcessConstructorBody(AstConstructorDeclaration *, bool);
    bool CatchableException(TypeSymbol *);
    void ReportMethodNotFound(Ast *ast, wchar_t *);
    MethodSymbol *FindConstructor(TypeSymbol *, Ast *, LexStream::TokenIndex, LexStream::TokenIndex);
    bool MoreSpecific(MethodSymbol *, MethodSymbol *);
    bool MoreSpecific(MethodSymbol *, Tuple<MethodSymbol *> &);
    bool NoMethodMoreSpecific(Tuple<MethodSymbol *> &, MethodSymbol *);
    bool IsMethodAccessible(AstFieldAccess *, TypeSymbol *, MethodSymbol *);
    void SearchForMethodInEnvironment(Tuple<MethodSymbol *> &, SemanticEnvironment *&, SemanticEnvironment *, AstMethodInvocation *);
    MethodSymbol *FindMisspelledMethodName(TypeSymbol *, AstMethodInvocation *, NameSymbol *);
    MethodSymbol *FindMethodInEnvironment(SemanticEnvironment *&, SemanticEnvironment *, AstMethodInvocation *);
    MethodSymbol *FindMethodInType(TypeSymbol *, AstMethodInvocation *, NameSymbol * = NULL);

    void ReportAccessedFieldNotFound(AstFieldAccess *, TypeSymbol *);
    void SearchForVariableInEnvironment(Tuple<VariableSymbol *> &, SemanticEnvironment *&,
                                        SemanticEnvironment *, NameSymbol *, LexStream::TokenIndex);
    VariableSymbol *FindMisspelledVariableName(TypeSymbol *, LexStream::TokenIndex);
    VariableSymbol *FindVariableInEnvironment(SemanticEnvironment *&, SemanticEnvironment *, LexStream::TokenIndex);
    VariableSymbol *FindVariableInType(TypeSymbol *, AstFieldAccess *, NameSymbol * = NULL);
    VariableSymbol *FindInstance(TypeSymbol *, TypeSymbol *);
    AstExpression *CreateAccessToType(Ast *, TypeSymbol *);
    void CreateAccessToScopedVariable(AstSimpleName *, TypeSymbol *);
    void CreateAccessToScopedMethod(AstMethodInvocation *, TypeSymbol *);

    void TypeAccessCheck(Ast *, TypeSymbol *);
    void TypeNestAccessCheck(AstExpression *);
    void ConstructorAccessCheck(AstClassInstanceCreationExpression *, MethodSymbol *);
    void MemberAccessCheck(AstFieldAccess *, TypeSymbol *, Symbol *);
    void SimpleNameAccessCheck(AstSimpleName *, TypeSymbol *, Symbol *);

    void (Semantic::*ProcessPreUnaryExpr[AstPreUnaryExpression::_num_kinds])(AstPreUnaryExpression *);
    void ProcessPLUS(AstPreUnaryExpression *);
    void ProcessMINUS(AstPreUnaryExpression *);
    void ProcessTWIDDLE(AstPreUnaryExpression *);
    void ProcessNOT(AstPreUnaryExpression *);
    void ProcessPLUSPLUSOrMINUSMINUS(AstPreUnaryExpression *);

    void (Semantic::*ProcessBinaryExpr[AstBinaryExpression::_num_kinds])(AstBinaryExpression *);
    void ProcessPLUS(AstBinaryExpression *);
    void ProcessLEFT_SHIFT(AstBinaryExpression *);
    void ProcessRIGHT_SHIFT(AstBinaryExpression *);
    void ProcessUNSIGNED_RIGHT_SHIFT(AstBinaryExpression *);
    void ProcessLESS(AstBinaryExpression *);
    void ProcessGREATER(AstBinaryExpression *);
    void ProcessLESS_EQUAL(AstBinaryExpression *);
    void ProcessGREATER_EQUAL(AstBinaryExpression *);
    void ProcessAND(AstBinaryExpression *);
    void ProcessXOR(AstBinaryExpression *);
    void ProcessIOR(AstBinaryExpression *);
    void ProcessAND_AND(AstBinaryExpression *);
    void ProcessOR_OR(AstBinaryExpression *);
    void ProcessEQUAL_EQUAL(AstBinaryExpression *);
    void ProcessNOT_EQUAL(AstBinaryExpression *);
    void ProcessSTAR(AstBinaryExpression *);
    void ProcessMINUS(AstBinaryExpression *);
    void ProcessSLASH(AstBinaryExpression *);
    void ProcessMOD(AstBinaryExpression *);
    void ProcessINSTANCEOF(AstBinaryExpression *);

    MethodSymbol *FindMethodMember(TypeSymbol *, TypeSymbol *, AstMethodInvocation *);
    void ProcessMethodName(AstMethodInvocation *);
    void (Semantic::*ProcessExprOrStmt[Ast::_num_kinds])(Ast *);
    inline void ProcessStatement(AstStatement *stmt)
    {
        (this ->* ProcessExprOrStmt[stmt -> kind])(stmt);
    }

    inline void ProcessExpression(AstExpression *expr)
    {
        (this ->* ProcessExprOrStmt[expr -> kind])(expr);
    }

    inline void ProcessExpressionOrStringConstant(AstExpression *expr)
    {
        (this ->* ProcessExprOrStmt[expr -> kind])(expr);
        //
        // If the expression is of type String, check whether or not it is
        // constant, and if so, compute the result.
        //
        if (expr -> symbol == control.String() && (! expr -> IsConstant()))
            control.Utf8_pool.CheckStringConstant(expr);

        return;
    }

    void ProcessLocalVariableDeclarationStatement(Ast *);
    void ProcessBlock(Ast *);
    void ProcessForStatement(Ast *);
    void ProcessSwitchStatement(Ast *);
    void ProcessThrowStatement(Ast *);
    void ProcessTryStatement(Ast *);
    void ProcessExpressionStatement(Ast *);
    void ProcessSynchronizedStatement(Ast *);
    void ProcessIfStatement(Ast *);
    void ProcessWhileStatement(Ast *);
    void ProcessDoStatement(Ast *);
    void ProcessBreakStatement(Ast *);
    void ProcessContinueStatement(Ast *);
    void ProcessReturnStatement(Ast *);
    void ProcessEmptyStatement(Ast *);
    TypeSymbol *GetLocalType(AstClassDeclaration *);
    void ProcessClassDeclaration(Ast *);
    void GenerateLocalConstructor(MethodSymbol *);

    void CheckSimpleName(AstSimpleName *, SemanticEnvironment *where_found);
    void ProcessSimpleName(Ast *);
    void FindVariableMember(TypeSymbol *, TypeSymbol *, AstFieldAccess *);
    void ProcessAmbiguousName(Ast *);
    void ProcessFieldAccess(Ast *);
    void ProcessIntegerLiteral(Ast *);
    void ProcessLongLiteral(Ast *);
    void ProcessFloatingPointLiteral(Ast *);
    void ProcessDoubleLiteral(Ast *);
    void ProcessTrueLiteral(Ast *);
    void ProcessFalseLiteral(Ast *);
    void ProcessStringLiteral(Ast *);
    void ProcessCharacterLiteral(Ast *);
    void ProcessArrayAccess(Ast *);
    void ProcessMethodInvocation(Ast *);
    void ProcessNullLiteral(Ast *);
    void ProcessThisExpression(Ast *);
    void ProcessSuperExpression(Ast *);
    void ProcessParenthesizedExpression(Ast *);
    void UpdateGeneratedLocalConstructor(MethodSymbol *);
    void UpdateLocalConstructors(TypeSymbol *);
    void GetAnonymousConstructor(AstClassInstanceCreationExpression *, TypeSymbol *);
    TypeSymbol *GetAnonymousType(AstClassInstanceCreationExpression *, TypeSymbol *);
    void ProcessClassInstanceCreationExpression(Ast *);
    void ProcessArrayCreationExpression(Ast *);
    void ProcessPostUnaryExpression(Ast *);
    void ProcessPreUnaryExpression(Ast *);
    void ProcessCastExpression(Ast *);
    void ProcessBinaryExpression(Ast *);
    void ProcessTypeExpression(Ast *);
    void ProcessConditionalExpression(Ast *);
    void ProcessAssignmentExpression(Ast *);

    void ProcessVariableInitializer(AstVariableDeclarator *);
    void ProcessArrayInitializer(AstArrayInitializer *, TypeSymbol *);

    void CheckInheritedMethodThrows(AstMethodDeclaration *, MethodSymbol *);
    void CheckMethodOverride(AstMethodDeclaration *, MethodSymbol *);
    void CheckInheritedMethodThrows(AstClassDeclaration *, MethodSymbol *, MethodSymbol *);
    void CheckMethodOverride(AstClassDeclaration *, MethodSymbol *, MethodSymbol *);
    void AddInheritedTypes(TypeSymbol *, TypeSymbol *);
    void AddInheritedFields(TypeSymbol *, TypeSymbol *);
    void AddInheritedMethods(TypeSymbol *, TypeSymbol *, LexStream::TokenIndex);
    void ComputeTypesClosure(TypeSymbol *, LexStream::TokenIndex);
    void ComputeFieldsClosure(TypeSymbol *, LexStream::TokenIndex);
    void ComputeMethodsClosure(TypeSymbol *, LexStream::TokenIndex);

    inline bool InRange(const char *buffer_ptr, const char *buffer_tail, int size) { return ((buffer_ptr + size) <= buffer_tail); }
    TypeSymbol *RetrieveNestedTypes(TypeSymbol *, wchar_t *, LexStream::TokenIndex);
    TypeSymbol *GetClassPool(TypeSymbol *, TypeSymbol **, const char **, int, LexStream::TokenIndex);
    void ProcessBadClass(TypeSymbol *, LexStream::TokenIndex);
    bool ProcessClassFile(TypeSymbol *, const char *, int, LexStream::TokenIndex);
    void ReadClassFile(TypeSymbol *, LexStream::TokenIndex);

    //
    // Any exception that is neither RuntimeException or one of its subclasses nor
    // Error or one of its subclasses is a checked exception.
    //
    inline bool CheckedException(TypeSymbol *exception)
    {
        return (! (exception -> IsSubclass(control.RuntimeException()) || exception -> IsSubclass(control.Error())));
    }

public:

    static inline u1 GetU1(const char *);
    static inline u2 GetU2(const char *);
    static inline u4 GetU4(const char *);

    static inline u1 GetAndSkipU1(const char *&);
    static inline u2 GetAndSkipU2(const char *&);
    static inline u4 GetAndSkipU4(const char *&);
    static inline void Skip(const char *&, int);

    inline void AddDependence(TypeSymbol *, TypeSymbol *, LexStream::TokenIndex, bool = false);
    inline void SetObjectSuperType(TypeSymbol *, LexStream::TokenIndex);
    inline void AddStringConversionDependence(TypeSymbol *, LexStream::TokenIndex);
};


inline void Semantic::AddDependence(TypeSymbol *base_type_, TypeSymbol *parent_type_, LexStream::TokenIndex tok, bool static_access)
{
    if (base_type_ != control.no_type)
    {
        TypeSymbol *base_type = base_type_ -> outermost_type,
                   *parent_type = parent_type_ -> outermost_type;

        parent_type -> dependents -> AddElement(base_type);
        if (static_access)
             base_type -> static_parents -> AddElement(parent_type);
        else base_type -> parents -> AddElement(parent_type);

        if (control.option.pedantic)
        {
            if (parent_type -> ContainingPackage() == control.unnamed_package &&
                base_type -> ContainingPackage() != control.unnamed_package)
            {
                error -> Report(SemanticError::PARENT_TYPE_IN_UNNAMED_PACKAGE,
                                tok,
                                tok,
                                parent_type_ -> ContainingPackage() -> PackageName(),
                                parent_type_ -> ExternalName());
            }
        }
    }

    return;
}

inline void Semantic::SetObjectSuperType(TypeSymbol *type, LexStream::TokenIndex tok)
{
    type -> super = control.Object();
    AddDependence(type, type -> super, tok);
}

inline void Semantic::AddStringConversionDependence(TypeSymbol *type, LexStream::TokenIndex tok)
{
    if (type == control.null_type)
         ;
    else if (type == control.boolean_type)
         AddDependence(ThisType(), control.Boolean(), tok);
    else if (type == control.char_type)
         AddDependence(ThisType(), control.Character(), tok);
    else if (type == control.int_type)
         AddDependence(ThisType(), control.Integer(), tok);
    else if (type == control.long_type)
         AddDependence(ThisType(), control.Long(), tok);
    else if (type == control.float_type)
         AddDependence(ThisType(), control.Float(), tok);
    else // (type == control.double_type)
         AddDependence(ThisType(), control.Double(), tok);
}

#ifdef	HAVE_NAMESPACES
}			// Close namespace Jikes block
#endif

#endif

