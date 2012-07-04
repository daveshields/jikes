// $Id: ast.cpp,v 1.40 2002/08/05 23:56:24 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "ast.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

#ifdef JIKES_DEBUG
unsigned Ast::count = 0;
#endif

//
// Allocate another block of storage for the VariableSymbol array.
//
void VariableSymbolArray::AllocateMoreSpace()
{
    //
    //
    // The variable size always indicates the maximum number of
    // elements that has been allocated for the array.
    // Initially, it is set to 0 to indicate that the array is empty.
    // The pool of available elements is divided into segments of size
    // 2**log_blksize each. Each segment is pointed to by a slot in
    // the array base.
    //
    // By dividing size by the size of the segment we obtain the
    // index for the next segment in base. If base is full, it is
    // reallocated.
    //
    //
    size_t k = size >> log_blksize; /* which segment? */

    //
    // If the base is overflowed, reallocate it and initialize the new elements to NULL.
    //
    if (k == base_size)
    {
        int old_base_size = base_size;
        T **old_base = base;

        base_size += base_increment;

        assert(base_size <= pool -> Blksize()); // There must be enough room to allocate base

        base = (T **) pool -> Alloc(sizeof(T *) * base_size);

        if (old_base != NULL)
        {
            memmove(base, old_base, old_base_size * sizeof(T *));
// STG:
//                delete [] old_base;
        }
        memset(&base[old_base_size], 0, (base_size - old_base_size) * sizeof(T *));
    }

    //
    // We allocate a new segment and place its adjusted address in
    // base[k]. The adjustment allows us to index the segment directly,
    // instead of having to perform a subtraction for each reference.
    // See operator[] below.
    //
    assert(Blksize() <= pool -> Blksize()); // There must be enough room to allocate block

    base[k] = (T *) pool -> Alloc(sizeof(T) * Blksize());
    base[k] -= size;

    //
    // Finally, we update size.
    //
    size += Blksize();
}


VariableSymbolArray::VariableSymbolArray(StoragePool *pool_, unsigned estimate = 0) : pool(pool_)
{
    assert(pool -> Blksize() >= 256); // There must be enough space in the storage pool to move !!!

    if (estimate == 0)
        log_blksize = 6; // take a guess
    else
    {
        for (log_blksize = 1; (((unsigned) 1 << log_blksize) < estimate) && (log_blksize < 31); log_blksize++)
            ;
    }

    //
    // Increment a base_increment size that is big enough not to have to
    // be reallocated. Find a block size that is smaller that the block
    // size of the pool.
    //
    base_increment = (Blksize() > pool -> Blksize() ? Blksize() / pool -> Blksize() : 1) * 2;
    while (Blksize() >= pool -> Blksize())
        log_blksize--;

    base_size = 0;
    size = 0;
    top = 0;
    base = NULL;
}


void AstCompilationUnit::FreeAst()
{
     delete ast_pool;
}

//
// This procedure uses a  quick sort algorithm to sort the cases
// in a switch statement.
//
void AstSwitchStatement::SortCases()
{
    int lower,
        upper,
        lostack[32],
        histack[32];

    int top,
        i,
        j;

    CaseElement pivot, temp;

    AstArray<CaseElement *> &map = *cases;

    top = 0;
    lostack[top] = 0;
    histack[top] = map.Length() - 1;

    while (top >= 0)
    {
        lower = lostack[top];
        upper = histack[top];
        top--;

        while (upper > lower)
        {
            //
            // The array is most-likely almost sorted. Therefore,
            // we use the middle element as the pivot element.
            //
            i = (lower + upper) / 2;
            pivot = *map[i];
            *map[i] = *map[lower];

            //
            // Split the array section indicated by LOWER and UPPER
            // using ARRAY(LOWER) as the pivot.
            //
            i = lower;
            for (j = lower + 1; j <= upper; j++)
                if (map[j] -> Value() < pivot.Value() ||
                    (map[j] -> Value() == pivot.Value() && map[j] -> index < pivot.index)) // keep the sort stable
                {
                    temp = *map[++i];
                    *map[i] = *map[j];
                    *map[j] = temp;
                }
            *map[lower] = *map[i];
            *map[i] = pivot;

            top++;
            if ((i - lower) < (upper - i))
            {
                lostack[top] = i + 1;
                histack[top] = upper;
                upper = i - 1;
            }
            else
            {
                histack[top] = i - 1;
                lostack[top] = lower;
                lower = i + 1;
            }
        }
    }
}


Ast *Ast::Clone(StoragePool *ast_pool)
{
    return (Ast *) NULL;
}

Ast *AstBlock::Clone(StoragePool *ast_pool)
{
    AstBlock *clone = ast_pool -> GenBlock();
    CloneInto(clone, ast_pool);
    return clone;
}

void AstBlock::CloneInto(AstBlock *clone, StoragePool *ast_pool)
{
    clone -> label_opt = label_opt;
    clone -> nesting_level = nesting_level;
    clone -> left_brace_token = left_brace_token;
    if (NumStatements() == 0)
        clone -> block_statements = NULL;
    else
    {
        for (int j = 0; j < NumStatements(); j++)
            clone -> AddStatement((AstStatement *) Statement(j) -> Clone(ast_pool));
    }
    clone -> right_brace_token = right_brace_token;
    clone -> no_braces = no_braces;
}

Ast *AstPrimitiveType::Clone(StoragePool *ast_pool)
{
    AstPrimitiveType *clone = ast_pool -> GenPrimitiveType(kind, primitive_kind_token);

    return clone;
}

Ast *AstArrayType::Clone(StoragePool *ast_pool)
{
    AstArrayType *clone = ast_pool -> GenArrayType();

    clone -> type = type -> Clone(ast_pool);
    clone -> AllocateBrackets(NumBrackets());
    for (int i = 0; i < NumBrackets(); i++)
        clone -> AddBrackets((AstBrackets *) Brackets(i) -> Clone(ast_pool));

    return clone;
}

Ast *AstSimpleName::Clone(StoragePool *ast_pool)
{
    AstSimpleName *clone = ast_pool -> GenSimpleName(identifier_token);
    clone -> resolution_opt = (AstExpression *) (resolution_opt ? resolution_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstPackageDeclaration::Clone(StoragePool *ast_pool)
{
    AstPackageDeclaration *clone = ast_pool -> GenPackageDeclaration();

    clone -> package_token = package_token;
    clone -> name = (AstExpression *) name -> Clone(ast_pool);
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstImportDeclaration::Clone(StoragePool *ast_pool)
{
    AstImportDeclaration *clone = ast_pool -> GenImportDeclaration();

    clone -> import_token = import_token;
    clone -> name = (AstExpression *) name -> Clone(ast_pool);
    clone -> star_token_opt = star_token_opt;
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstCompilationUnit::Clone(StoragePool *ast_pool)
{
    AstCompilationUnit *clone = ast_pool -> GenCompilationUnit();

    clone -> package_declaration_opt = (AstPackageDeclaration *)
                                       (package_declaration_opt
                                              ? package_declaration_opt -> Clone(ast_pool) : NULL);
    for (int i = 0; i < NumImportDeclarations(); i++)
        clone -> AddImportDeclaration((AstImportDeclaration *) ImportDeclaration(i) -> Clone(ast_pool));
    for (int k = 0; k < NumTypeDeclarations(); k++)
        clone -> AddTypeDeclaration(TypeDeclaration(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstModifier::Clone(StoragePool *ast_pool)
{
    AstModifier *clone = ast_pool -> GenModifier(kind, modifier_kind_token);

    return clone;
}

Ast *AstEmptyDeclaration::Clone(StoragePool *ast_pool)
{
    AstEmptyDeclaration *clone = ast_pool -> GenEmptyDeclaration(semicolon_token);

    return clone;
}

Ast *AstClassBody::Clone(StoragePool *ast_pool)
{
    AstClassBody *clone = ast_pool -> GenClassBody();

    clone -> left_brace_token = left_brace_token;
    for (int i = 0; i < NumClassBodyDeclarations(); i++)
        clone -> AddClassBodyDeclarationNicely(ClassBodyDeclaration(i) -> Clone(ast_pool));
    clone -> right_brace_token = right_brace_token;

    return clone;
}

Ast *AstClassDeclaration::Clone(StoragePool *ast_pool)
{
    AstClassDeclaration *clone = ast_pool -> GenClassDeclaration();

    for (int i = 0; i < NumClassModifiers(); i++)
        clone -> AddClassModifier((AstModifier *) ClassModifier(i) -> Clone(ast_pool));
    clone -> class_token = class_token;
    clone -> identifier_token = identifier_token;
    clone -> super_opt = (Ast *) (super_opt ? super_opt -> Clone(ast_pool) : NULL);
    for (int k = 0; k < NumInterfaces(); k++)
        clone -> AddInterface((AstExpression *) Interface(k) -> Clone(ast_pool));
    clone -> class_body = (AstClassBody *) class_body -> Clone(ast_pool);

    return clone;
}

Ast *AstArrayInitializer::Clone(StoragePool *ast_pool)
{
    AstArrayInitializer *clone = ast_pool -> GenArrayInitializer();

    clone -> left_brace_token = left_brace_token;
    for (int k = 0; k < NumVariableInitializers(); k++)
        clone -> AddVariableInitializer(VariableInitializer(k) -> Clone(ast_pool));
    clone -> right_brace_token = right_brace_token;

    return clone;
}

Ast *AstBrackets::Clone(StoragePool *ast_pool)
{
    AstBrackets *clone = ast_pool -> GenBrackets(left_bracket_token, right_bracket_token);

    return clone;
}

Ast *AstVariableDeclaratorId::Clone(StoragePool *ast_pool)
{
    AstVariableDeclaratorId *clone = ast_pool -> GenVariableDeclaratorId();

    clone -> identifier_token = identifier_token;
    clone -> AllocateBrackets(NumBrackets());
    for (int i = 0; i < NumBrackets(); i++)
        clone -> AddBrackets((AstBrackets *) Brackets(i) -> Clone(ast_pool));

    return clone;
}

Ast *AstVariableDeclarator::Clone(StoragePool *ast_pool)
{
    AstVariableDeclarator *clone = ast_pool -> GenVariableDeclarator();

    clone -> variable_declarator_name = (AstVariableDeclaratorId *) variable_declarator_name -> Clone(ast_pool);
    clone -> variable_initializer_opt = (Ast *) (variable_initializer_opt
                                                       ? variable_initializer_opt -> Clone(ast_pool)
                                                       : NULL);

    return clone;
}

Ast *AstFieldDeclaration::Clone(StoragePool *ast_pool)
{
    AstFieldDeclaration *clone = ast_pool -> GenFieldDeclaration();

    for (int i = 0; i < NumVariableModifiers(); i++)
        clone -> AddVariableModifier((AstModifier *) VariableModifier(i) -> Clone(ast_pool));
    clone -> type = type -> Clone(ast_pool);
    for (int k = 0; k < NumVariableDeclarators(); k++)
        clone -> AddVariableDeclarator((AstVariableDeclarator *) VariableDeclarator(k) -> Clone(ast_pool));
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstFormalParameter::Clone(StoragePool *ast_pool)
{
    AstFormalParameter *clone = ast_pool -> GenFormalParameter();

    if (NumParameterModifiers() == 0)
        clone -> parameter_modifiers = NULL;
    else
    {
        for (int i = 0; i < NumParameterModifiers(); i++)
            clone -> AddParameterModifier((AstModifier *) ParameterModifier(i) -> Clone(ast_pool));
    }
    clone -> type = type -> Clone(ast_pool);
    clone -> formal_declarator = (AstVariableDeclarator *) formal_declarator -> Clone(ast_pool);

    return clone;
}

Ast *AstMethodDeclarator::Clone(StoragePool *ast_pool)
{
    AstMethodDeclarator *clone = ast_pool -> GenMethodDeclarator();

    clone -> identifier_token = identifier_token;
    clone -> left_parenthesis_token = left_parenthesis_token;
    clone -> AllocateFormalParameters(NumFormalParameters());
    for (int i = 0; i < NumFormalParameters(); i++)
        clone -> AddFormalParameter((AstFormalParameter *) FormalParameter(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = right_parenthesis_token;
    clone -> AllocateBrackets(NumBrackets());
    for (int k = 0; k < NumBrackets(); k++)
        clone -> AddBrackets((AstBrackets *) Brackets(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstMethodDeclaration::Clone(StoragePool *ast_pool)
{
    AstMethodDeclaration *clone = ast_pool -> GenMethodDeclaration();

    for (int i = 0; i < NumMethodModifiers(); i++)
        clone -> AddMethodModifier((AstModifier *) MethodModifier(i) -> Clone(ast_pool));
    clone -> type = type -> Clone(ast_pool);
    clone -> method_declarator = (AstMethodDeclarator *) method_declarator -> Clone(ast_pool);
    for (int k = 0; k < NumThrows(); k++)
        clone -> AddThrow((AstExpression *) Throw(k) -> Clone(ast_pool));
    clone -> method_body = (AstStatement *) method_body -> Clone(ast_pool);

    return clone;
}

Ast *AstStaticInitializer::Clone(StoragePool *ast_pool)
{
    AstStaticInitializer *clone = ast_pool -> GenStaticInitializer();

    clone -> static_token = static_token;
    clone -> block = (AstMethodBody *) block -> Clone(ast_pool);

    return clone;
}

Ast *AstThisCall::Clone(StoragePool *ast_pool)
{
    AstThisCall *clone = ast_pool -> GenThisCall();

    clone -> this_token = this_token;
    clone -> left_parenthesis_token = left_parenthesis_token;
    clone -> AllocateArguments(NumArguments());
    for (int i = 0; i < NumArguments(); i++)
        clone -> AddArgument((AstExpression *) Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = right_parenthesis_token;
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstSuperCall::Clone(StoragePool *ast_pool)
{
    AstSuperCall *clone = ast_pool -> GenSuperCall();

    clone -> base_opt = (AstExpression *) (base_opt ? base_opt -> Clone(ast_pool) : NULL);
    clone -> dot_token_opt = dot_token_opt;
    clone -> super_token = super_token;
    clone -> left_parenthesis_token = left_parenthesis_token;
    clone -> AllocateArguments(NumArguments());
    for (int i = 0; i < NumArguments(); i++)
        clone -> AddArgument((AstExpression *) Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = right_parenthesis_token;
    clone -> semicolon_token = semicolon_token;
    clone -> AllocateLocalArguments(NumLocalArguments());
    for (int k = 0; k < NumLocalArguments(); k++)
        clone -> AddLocalArgument((AstExpression *) LocalArgument(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstMethodBody::Clone(StoragePool *ast_pool)
{
    AstMethodBody *clone = ast_pool -> GenMethodBody();
    CloneInto(clone, ast_pool);

    clone -> explicit_constructor_opt = (AstStatement *)
        (explicit_constructor_opt ? explicit_constructor_opt -> Clone(ast_pool)
         : NULL);

    return clone;
}

Ast *AstConstructorDeclaration::Clone(StoragePool *ast_pool)
{
    AstConstructorDeclaration *clone = ast_pool -> GenConstructorDeclaration();

    for (int i = 0; i < NumConstructorModifiers(); i++)
        clone -> AddConstructorModifier((AstModifier *) ConstructorModifier(i) -> Clone(ast_pool));
    clone -> constructor_declarator = (AstMethodDeclarator *) constructor_declarator -> Clone(ast_pool);
    for (int k = 0; k < NumThrows(); k++)
        clone -> AddThrow((AstExpression *) Throw(k) -> Clone(ast_pool));
    clone -> constructor_body = (AstMethodBody *) constructor_body -> Clone(ast_pool);

    return clone;
}

Ast *AstInterfaceDeclaration::Clone(StoragePool *ast_pool)
{
    AstInterfaceDeclaration *clone = ast_pool -> GenInterfaceDeclaration();

    for (int i = 0; i < NumInterfaceModifiers(); i++)
        clone -> AddInterfaceModifier((AstModifier *) InterfaceModifier(i) -> Clone(ast_pool));
    clone -> interface_token = interface_token;
    clone -> identifier_token = identifier_token;
    for (int k = 0; k < NumExtendsInterfaces(); k++)
        clone -> AddExtendsInterface((AstExpression *) ExtendsInterface(k) -> Clone(ast_pool));
    clone -> left_brace_token = left_brace_token;
    for (int l = 0; l < NumExtendsInterfaces(); l++)
        clone -> AddInterfaceMemberDeclaration((AstExpression *) InterfaceMemberDeclaration(l) -> Clone(ast_pool));
    clone -> right_brace_token = right_brace_token;

    return clone;
}

Ast *AstLocalVariableDeclarationStatement::Clone(StoragePool *ast_pool)
{
    AstLocalVariableDeclarationStatement *clone = ast_pool -> GenLocalVariableDeclarationStatement();

    for (int i = 0; i < NumLocalModifiers(); i++)
        clone -> AddLocalModifier((AstModifier *) LocalModifier(i) -> Clone(ast_pool));
    clone -> type = type -> Clone(ast_pool);
    for (int k = 0; k < NumVariableDeclarators(); k++)
        clone -> AddVariableDeclarator((AstVariableDeclarator *) VariableDeclarator(k) -> Clone(ast_pool));
    clone -> semicolon_token_opt = semicolon_token_opt;

    return clone;
}

Ast *AstIfStatement::Clone(StoragePool *ast_pool)
{
    AstIfStatement *clone = ast_pool -> GenIfStatement();

    clone -> if_token = if_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> true_statement = (AstStatement *) true_statement -> Clone(ast_pool);
    clone -> false_statement_opt = (AstStatement *)
                                   (false_statement_opt
                                          ? false_statement_opt -> Clone(ast_pool)
                                          : NULL);

    return clone;
}

Ast *AstEmptyStatement::Clone(StoragePool *ast_pool)
{
    AstEmptyStatement *clone = ast_pool -> GenEmptyStatement(semicolon_token);

    return clone;
}

Ast *AstExpressionStatement::Clone(StoragePool *ast_pool)
{
    AstExpressionStatement *clone = ast_pool -> GenExpressionStatement();

    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> semicolon_token_opt = semicolon_token_opt;

    return clone;
}

Ast *AstCaseLabel::Clone(StoragePool *ast_pool)
{
    AstCaseLabel *clone = ast_pool -> GenCaseLabel();

    clone -> case_token = case_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> colon_token = colon_token;
    clone -> map_index = map_index;

    return clone;
}

Ast *AstDefaultLabel::Clone(StoragePool *ast_pool)
{
    AstDefaultLabel *clone = ast_pool -> GenDefaultLabel();

    clone -> default_token = default_token;
    clone -> colon_token = colon_token;

    return clone;
}

Ast *AstSwitchBlockStatement::Clone(StoragePool *ast_pool)
{
    AstSwitchBlockStatement *clone = ast_pool -> GenSwitchBlockStatement();
    CloneInto(clone, ast_pool);
    clone -> AllocateSwitchLabels(NumSwitchLabels());
    for (int i = 0; i < NumSwitchLabels(); i++)
        clone -> AddSwitchLabel(SwitchLabel(i) -> Clone(ast_pool));
    return clone;
}

Ast *AstSwitchStatement::Clone(StoragePool *ast_pool)
{
    AstSwitchStatement *clone = ast_pool -> GenSwitchStatement();

    clone -> switch_token = switch_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> switch_block = (AstBlock *) switch_block -> Clone(ast_pool);

    return clone;
}

Ast *AstWhileStatement::Clone(StoragePool *ast_pool)
{
    AstWhileStatement *clone = ast_pool -> GenWhileStatement();

    clone -> while_token = while_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> statement = (AstStatement *) statement -> Clone(ast_pool);

    return clone;
}

Ast *AstDoStatement::Clone(StoragePool *ast_pool)
{
    AstDoStatement *clone = ast_pool -> GenDoStatement();

    clone -> do_token = do_token;
    clone -> statement = (AstStatement *) statement -> Clone(ast_pool);
    clone -> while_token = while_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstForStatement::Clone(StoragePool *ast_pool)
{
    AstForStatement *clone = ast_pool -> GenForStatement();

    clone -> for_token = for_token;
    for (int i = 0; i < NumForInitStatements(); i++)
        clone -> AddForInitStatement((AstStatement *) ForInitStatement(i) -> Clone(ast_pool));
    clone -> end_expression_opt = (AstExpression *)
                                  (end_expression_opt
                                         ? end_expression_opt -> Clone(ast_pool)
                                         : NULL);
    for (int k = 0; k < NumForUpdateStatements(); k++)
        clone -> AddForUpdateStatement((AstExpressionStatement *) ForUpdateStatement(k) -> Clone(ast_pool));
    clone -> statement = (AstStatement *) statement -> Clone(ast_pool);

    return clone;
}

Ast *AstBreakStatement::Clone(StoragePool *ast_pool)
{
    AstBreakStatement *clone = ast_pool -> GenBreakStatement();

    clone -> break_token = break_token;
    clone -> identifier_token_opt = identifier_token_opt;
    clone -> semicolon_token = semicolon_token;
    clone -> nesting_level = nesting_level;

    return clone;
}

Ast *AstContinueStatement::Clone(StoragePool *ast_pool)
{
    AstContinueStatement *clone = ast_pool -> GenContinueStatement();

    clone -> continue_token = continue_token;
    clone -> identifier_token_opt = identifier_token_opt;
    clone -> semicolon_token = semicolon_token;
    clone -> nesting_level = nesting_level;

    return clone;
}

Ast *AstReturnStatement::Clone(StoragePool *ast_pool)
{
    AstReturnStatement *clone = ast_pool -> GenReturnStatement();

    clone -> return_token = return_token;
    clone -> expression_opt = (AstExpression *) (expression_opt ? expression_opt -> Clone(ast_pool) : NULL);
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstThrowStatement::Clone(StoragePool *ast_pool)
{
    AstThrowStatement *clone = ast_pool -> GenThrowStatement();

    clone -> throw_token = throw_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstSynchronizedStatement::Clone(StoragePool *ast_pool)
{
    AstSynchronizedStatement *clone = ast_pool -> GenSynchronizedStatement();

    clone -> synchronized_token = synchronized_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> block = (AstBlock *) block -> Clone(ast_pool);

    return clone;
}

Ast *AstAssertStatement::Clone(StoragePool *ast_pool)
{
    AstAssertStatement *clone = ast_pool -> GenAssertStatement();

    clone -> assert_token = assert_token;
    clone -> condition = (AstExpression *) condition -> Clone(ast_pool);
    clone -> message_opt = (message_opt
                            ? (AstExpression *) message_opt -> Clone(ast_pool)
                            : (AstExpression *) NULL);
    clone -> semicolon_token = semicolon_token;

    return clone;
}

Ast *AstCatchClause::Clone(StoragePool *ast_pool)
{
    AstCatchClause *clone = ast_pool -> GenCatchClause();

    clone -> catch_token = catch_token;
    clone -> formal_parameter = (AstFormalParameter *) formal_parameter -> Clone(ast_pool);
    clone -> block = (AstBlock *) block -> Clone(ast_pool);

    return clone;
}

Ast *AstFinallyClause::Clone(StoragePool *ast_pool)
{
    AstFinallyClause *clone = ast_pool -> GenFinallyClause();

    clone -> finally_token = finally_token;
    clone -> block = (AstBlock *) block -> Clone(ast_pool);

    return clone;
}

Ast *AstTryStatement::Clone(StoragePool *ast_pool)
{
    AstTryStatement *clone = ast_pool -> GenTryStatement();

    clone -> try_token = try_token;
    clone -> block = (AstBlock *) block -> Clone(ast_pool);
    for (int k = 0; k < NumCatchClauses(); k++)
        clone -> AddCatchClause((AstCatchClause *) CatchClause(k) -> Clone(ast_pool));
    clone -> finally_clause_opt = (AstFinallyClause *)
                                  (finally_clause_opt
                                         ? finally_clause_opt -> Clone(ast_pool)
                                         : NULL);

    return clone;
}

Ast *AstIntegerLiteral::Clone(StoragePool *ast_pool)
{
    AstIntegerLiteral *clone = ast_pool -> GenIntegerLiteral(integer_literal_token);

    return clone;
}

Ast *AstLongLiteral::Clone(StoragePool *ast_pool)
{
    AstLongLiteral *clone = ast_pool -> GenLongLiteral(long_literal_token);

    return clone;
}

Ast *AstFloatLiteral::Clone(StoragePool *ast_pool)
{
    AstFloatLiteral *clone = ast_pool -> GenFloatLiteral(float_literal_token);

    return clone;
}

Ast *AstDoubleLiteral::Clone(StoragePool *ast_pool)
{
    AstDoubleLiteral *clone = ast_pool -> GenDoubleLiteral(double_literal_token);

    return clone;
}

Ast *AstTrueLiteral::Clone(StoragePool *ast_pool)
{
    AstTrueLiteral *clone = ast_pool -> GenTrueLiteral(true_literal_token);

    return clone;
}

Ast *AstFalseLiteral::Clone(StoragePool *ast_pool)
{
    AstFalseLiteral *clone = ast_pool -> GenFalseLiteral(false_literal_token);

    return clone;
}

Ast *AstStringLiteral::Clone(StoragePool *ast_pool)
{
    AstStringLiteral *clone = ast_pool -> GenStringLiteral(string_literal_token);

    return clone;
}

Ast *AstCharacterLiteral::Clone(StoragePool *ast_pool)
{
    AstCharacterLiteral *clone = ast_pool -> GenCharacterLiteral(character_literal_token);

    return clone;
}

Ast *AstNullLiteral::Clone(StoragePool *ast_pool)
{
    AstNullLiteral *clone = ast_pool -> GenNullLiteral(null_token);

    return clone;
}

Ast *AstThisExpression::Clone(StoragePool *ast_pool)
{
    AstThisExpression *clone = ast_pool -> GenThisExpression(this_token);

    return clone;
}

Ast *AstSuperExpression::Clone(StoragePool *ast_pool)
{
    AstSuperExpression *clone = ast_pool -> GenSuperExpression(super_token);

    return clone;
}

Ast *AstParenthesizedExpression::Clone(StoragePool *ast_pool)
{
    AstParenthesizedExpression *clone = ast_pool -> GenParenthesizedExpression();

    clone -> left_parenthesis_token = left_parenthesis_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> right_parenthesis_token = right_parenthesis_token;

    return clone;
}

Ast *AstTypeExpression::Clone(StoragePool *ast_pool)
{
    AstTypeExpression *clone = ast_pool -> GenTypeExpression(type -> Clone(ast_pool));

    return clone;
}

Ast *AstClassInstanceCreationExpression::Clone(StoragePool *ast_pool)
{
    AstClassInstanceCreationExpression *clone = ast_pool -> GenClassInstanceCreationExpression();

    clone -> base_opt = (AstExpression *) (base_opt ? base_opt -> Clone(ast_pool) : NULL);
    clone -> dot_token_opt = dot_token_opt;
    clone -> new_token = new_token;
    clone -> class_type = (AstTypeExpression *) class_type -> Clone(ast_pool);
    clone -> left_parenthesis_token = left_parenthesis_token;
    clone -> AllocateArguments(NumArguments());
    for (int i = 0; i < NumArguments(); i++)
        clone -> AddArgument((AstExpression *) Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = right_parenthesis_token;
    clone -> class_body_opt = (AstClassBody *) (class_body_opt ? class_body_opt -> Clone(ast_pool) : NULL);
    clone -> AllocateLocalArguments(NumLocalArguments());
    for (int k = 0; k < NumLocalArguments(); k++)
        clone -> AddLocalArgument((AstExpression *) LocalArgument(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstDimExpr::Clone(StoragePool *ast_pool)
{
    AstDimExpr *clone = ast_pool -> GenDimExpr();

    clone -> left_bracket_token = left_bracket_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> right_bracket_token = right_bracket_token;

    return clone;
}

Ast *AstArrayCreationExpression::Clone(StoragePool *ast_pool)
{
    AstArrayCreationExpression *clone = ast_pool -> GenArrayCreationExpression();

    clone -> new_token = new_token;
    clone -> array_type = array_type -> Clone(ast_pool);
    clone -> AllocateDimExprs(NumDimExprs());
    for (int i = 0; i < NumDimExprs(); i++)
        clone -> AddDimExpr((AstDimExpr *) DimExpr(i) -> Clone(ast_pool));
    clone -> AllocateBrackets(NumBrackets());
    for (int k = 0; k < NumBrackets(); k++)
        clone -> AddBrackets((AstBrackets *) Brackets(k) -> Clone(ast_pool));
    clone -> array_initializer_opt = (AstArrayInitializer *)
                                     (array_initializer_opt ? array_initializer_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstFieldAccess::Clone(StoragePool *ast_pool)
{
    AstFieldAccess *clone = ast_pool -> GenFieldAccess(field_access_tag);

    clone -> base = (AstExpression *) base -> Clone(ast_pool);
    clone -> dot_token = dot_token;
    clone -> identifier_token = identifier_token;
    clone -> resolution_opt = (AstExpression *) (resolution_opt ? resolution_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstMethodInvocation::Clone(StoragePool *ast_pool)
{
    AstMethodInvocation *clone = ast_pool -> GenMethodInvocation();

    clone -> method = (AstExpression *) method -> Clone(ast_pool);
    clone -> left_parenthesis_token = left_parenthesis_token;
    clone -> AllocateArguments(NumArguments());
    for (int i = 0; i < NumArguments(); i++)
        clone -> AddArgument((AstExpression *) Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = right_parenthesis_token;
    clone -> resolution_opt = (AstExpression *) (resolution_opt ? resolution_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstArrayAccess::Clone(StoragePool *ast_pool)
{
    AstArrayAccess *clone = ast_pool -> GenArrayAccess();

    clone -> base = (AstExpression *) base -> Clone(ast_pool);
    clone -> left_bracket_token = left_bracket_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> right_bracket_token = right_bracket_token;

    return clone;
}

Ast *AstPostUnaryExpression::Clone(StoragePool *ast_pool)
{
    AstPostUnaryExpression *clone = ast_pool -> GenPostUnaryExpression(post_unary_tag);

    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);
    clone -> post_operator_token = post_operator_token;

    return clone;
}

Ast *AstPreUnaryExpression::Clone(StoragePool *ast_pool)
{
    AstPreUnaryExpression *clone = ast_pool -> GenPreUnaryExpression(pre_unary_tag);

    clone -> pre_operator_token = pre_operator_token;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);

    return clone;
}

Ast *AstCastExpression::Clone(StoragePool *ast_pool)
{
    AstCastExpression *clone = ast_pool -> GenCastExpression();

    clone -> left_parenthesis_token_opt = left_parenthesis_token_opt;
    clone -> type_opt = (Ast *) (type_opt ? type_opt -> Clone(ast_pool) : NULL);
    clone -> AllocateBrackets(NumBrackets());
    for (int i = 0; i < NumBrackets(); i++)
        clone -> AddBrackets((AstBrackets *) Brackets(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token_opt = right_parenthesis_token_opt;
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);

    return clone;
}

Ast *AstBinaryExpression::Clone(StoragePool *ast_pool)
{
    AstBinaryExpression *clone = ast_pool -> GenBinaryExpression(binary_tag);

    clone -> left_expression = (AstExpression *) left_expression -> Clone(ast_pool);
    clone -> binary_operator_token = binary_operator_token;
    clone -> right_expression = (AstExpression *) right_expression -> Clone(ast_pool);

    return clone;
}

Ast *AstConditionalExpression::Clone(StoragePool *ast_pool)
{
    AstConditionalExpression *clone = ast_pool -> GenConditionalExpression();

    clone -> test_expression = (AstExpression *) test_expression -> Clone(ast_pool);
    clone -> question_token = question_token;
    clone -> true_expression = (AstExpression *) true_expression -> Clone(ast_pool);
    clone -> colon_token = colon_token;
    clone -> false_expression = (AstExpression *) false_expression -> Clone(ast_pool);

    return clone;
}

Ast *AstAssignmentExpression::Clone(StoragePool *ast_pool)
{
    AstAssignmentExpression *clone = ast_pool -> GenAssignmentExpression(assignment_tag, assignment_operator_token);

    clone -> left_hand_side = (AstExpression *) left_hand_side -> Clone(ast_pool);
    clone -> expression = (AstExpression *) expression -> Clone(ast_pool);

    return clone;
}

#ifdef JIKES_DEBUG
void Ast::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (Ast):  Node number " << (int) kind
            << " does not contain a print routine" << endl;
}

void AstBlock::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (";
    if (label_opt)
        Coutput << lex_stream.NameString(label_opt) << ": ";
    Coutput << "Block at level " << nesting_level;
    if (block_symbol)
        Coutput << ", max_variable_index "
                << block_symbol -> max_variable_index
                << ", try_or_synchronized_variable_index "
                << block_symbol -> try_or_synchronized_variable_index;
    else Coutput << ", BLOCK_SYMBOL NOT SET";
    Coutput << ")";

    if (NumStatements() > 0)
    {
        Coutput << "    {";
        for (int j = 0; j < NumStatements(); j++)
        {
            if (j % 10 == 0)
                Coutput << endl << "        ";
            Coutput << " #" << Statement(j) -> id;
        }
        Coutput << "    }" << endl;
        for (int k = 0; k < NumStatements(); k++)
            Statement(k) -> Print(lex_stream);
    }
    else
        Coutput << endl;
}

void AstPrimitiveType::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (PrimitiveType):  "
            << lex_stream.NameString(primitive_kind_token) << endl;
}

void AstArrayType::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ArrayType):  "
            << "#" << type -> id << " (";
    for (int i = 0; i < NumBrackets(); i++)
        Coutput << " #" << Brackets(i) -> id;
    Coutput << ")" << endl;
    type -> Print(lex_stream);
    for (int j = 0; j < NumBrackets(); j++)
        Brackets(j) -> Print(lex_stream);
}

void AstSimpleName::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (SimpleName):  "
            << lex_stream.NameString(identifier_token) << endl;
}

void AstPackageDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (PackageDeclaration):  "
            << lex_stream.NameString(package_token)
            << " #" << name -> id << endl;
    name -> Print(lex_stream);
}

void AstImportDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ImportDeclaration):  "
            << lex_stream.NameString(import_token)
            << " #" << name -> id;
    if (star_token_opt)
        Coutput << "." << lex_stream.NameString(star_token_opt);
    Coutput << endl;
    name -> Print(lex_stream);
}

void AstCompilationUnit::Print(LexStream& lex_stream)
{
    Coutput << endl << "AST structure for "
            << lex_stream.FileName()
            << ":" << endl << endl
            << "#" << id << " (CompilationUnit):  "
            << "#" << (package_declaration_opt ? package_declaration_opt -> id : 0)
            << " (";
    for (int i = 0; i < NumImportDeclarations(); i++)
        Coutput << " #" << ImportDeclaration(i) -> id;
    Coutput << " ) (";
    for (int k = 0; k < NumTypeDeclarations(); k++)
        Coutput << " #" << TypeDeclaration(k) -> id;
    Coutput << ")" << endl;

    if (package_declaration_opt)
        package_declaration_opt -> Print(lex_stream);
    for (int m = 0; m < NumImportDeclarations(); m++)
        ImportDeclaration(m) -> Print(lex_stream);
    for (int n = 0; n < NumTypeDeclarations(); n++)
        TypeDeclaration(n) -> Print(lex_stream);
}

void AstModifier::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (Modifier):  "
            << lex_stream.NameString(modifier_kind_token) << endl;
}

void AstEmptyDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (EmptyDeclaration):  "
            << lex_stream.NameString(semicolon_token) << endl;
}

void AstClassBody::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ClassBody):  "
            << endl << "    {";
    for (int i = 0; i < NumClassBodyDeclarations(); i++)
    {
        if (i % 10 == 0)
            Coutput << endl << "       ";
        Coutput << " #" << ClassBodyDeclaration(i) -> id;
    }
    Coutput << endl << "    }" << endl;

    for (int k = 0; k < NumClassBodyDeclarations(); k++)
        ClassBodyDeclaration(k) -> Print(lex_stream);
}

void AstClassDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ClassDeclaration):  ";
    for (int i = 0; i < NumClassModifiers(); i++)
    {
        Coutput << lex_stream.NameString(ClassModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << lex_stream.NameString(class_token) << " "
            << lex_stream.NameString(identifier_token)
            << " #" << (super_opt ? super_opt -> id : 0) << "(";
    for (int j = 0; j < NumInterfaces(); j++)
        Coutput << " #" << Interface(j) -> id;
    Coutput << ") #" << class_body -> id << endl;

    if (super_opt)
        super_opt -> Print(lex_stream);
    for (int k = 0; k < NumInterfaces(); k++)
        Interface(k) -> Print(lex_stream);
    class_body -> Print(lex_stream);
}

void AstArrayInitializer::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ArrayInitializer):  "
            << endl << "    {";
    for (int i = 0; i < NumVariableInitializers(); i++)
    {
        if (i % 10 == 0)
            Coutput << endl << "       ";
        Coutput << " #" << VariableInitializer(i) -> id;
    }
    Coutput << endl << "    }" << endl;

    for (int k = 0; k < NumVariableInitializers(); k++)
        VariableInitializer(k) -> Print(lex_stream);
}

void AstBrackets::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (Brackets):  "
            << lex_stream.NameString(left_bracket_token)
            << lex_stream.NameString(right_bracket_token) << endl;
}

void AstVariableDeclaratorId::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (VariableDeclaratorId):  "
            << lex_stream.NameString(identifier_token) << "(";
    for (int i = 0; i < NumBrackets(); i++)
        Coutput << " #" << Brackets(i) -> id;
    Coutput << " )" << endl;
    for (int j = 0; j < NumBrackets(); j++)
        Brackets(j) -> Print(lex_stream);
}

void AstVariableDeclarator::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (VariableDeclarator):  " << "#"
            << variable_declarator_name -> id << " #"
            << (variable_initializer_opt ? variable_initializer_opt -> id : 0)
            << endl;
    variable_declarator_name -> Print(lex_stream);
    if (variable_initializer_opt)
        variable_initializer_opt -> Print(lex_stream);

}

void AstFieldDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (FieldDeclaration):  ";
    for (int i = 0; i < NumVariableModifiers(); i++)
    {
        Coutput << lex_stream.NameString(VariableModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << " #" << type -> id << "(";
    for (int j = 0; j < NumVariableDeclarators(); j++)
        Coutput << " #" << VariableDeclarator(j) -> id;
    Coutput << ")" << endl;

    type -> Print(lex_stream);
    for (int k = 0; k < NumVariableDeclarators(); k++)
        VariableDeclarator(k) -> Print(lex_stream);
}

void AstFormalParameter::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (FormalParameter):  ";
    for (int i = 0; i < NumParameterModifiers(); i++)
    {
        Coutput << lex_stream.NameString(ParameterModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << "#" << type -> id
            << " #" << formal_declarator -> id << endl;
    type -> Print(lex_stream);
    formal_declarator -> Print(lex_stream);
}

void AstMethodDeclarator::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (MethodDeclarator):  "
            << lex_stream.NameString(identifier_token)
            << " (";
    for (int k = 0; k < NumFormalParameters(); k++)
        Coutput << " #" << FormalParameter(k) -> id;
    Coutput << " ) (";
    for (int i = 0; i < NumBrackets(); i++)
        Coutput << " #" << Brackets(i) -> id;
    Coutput << " )" << endl;

    for (int j = 0; j < NumFormalParameters(); j++)
        FormalParameter(j) -> Print(lex_stream);
    for (int l = 0; l < NumBrackets(); l++)
        Brackets(l) -> Print(lex_stream);
}

void AstMethodDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (MethodDeclaration):  ";
    for (int i = 0; i < NumMethodModifiers(); i++)
    {
        Coutput << lex_stream.NameString(MethodModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << " #" << type -> id
            << " #" << method_declarator -> id
            << " throws: (";
    for (int j = 0; j < NumThrows(); j++)
        Coutput << " #" << Throw(j) -> id;
    Coutput << ") #" << method_body -> id << endl;

    type -> Print(lex_stream);
    method_declarator -> Print(lex_stream);
    for (int k = 0; k < NumThrows(); k++)
        Throw(k) -> Print(lex_stream);
    method_body -> Print(lex_stream);
}

void AstStaticInitializer::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (StaticInitializer):  "
            << lex_stream.NameString(static_token)
            << " #" << block -> id << endl;
    block -> Print(lex_stream);
}

void AstThisCall::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ThisCall):  "
            << lex_stream.NameString(this_token)
            << " (";
    for (int i = 0; i < NumArguments(); i++)
        Coutput << " #" << Argument(i) -> id;
    Coutput << " )" << endl;

    for (int j = 0; j < NumArguments(); j++)
        Argument(j) -> Print(lex_stream);
}

void AstSuperCall::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (SuperCall):  ";
    if (base_opt)
    {
        Coutput << "#" << base_opt -> id
                << lex_stream.NameString(dot_token_opt);
    }
    Coutput << lex_stream.NameString(super_token)
            << " (";
    for (int i = 0; i < NumArguments(); i++)
        Coutput << " #" << Argument(i) -> id;
    Coutput << " )" << endl;

    if (base_opt)
        base_opt -> Print(lex_stream);

    for (int j = 0; j < NumArguments(); j++)
        Argument(j) -> Print(lex_stream);
}

void AstMethodBody::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (MethodBody):  ";
    if (explicit_constructor_opt)
        Coutput << " #" << explicit_constructor_opt -> id << endl;
    else Coutput << " #0" << endl;
    AstBlock::Print(lex_stream);

    if (explicit_constructor_opt)
        explicit_constructor_opt -> Print(lex_stream);
}

void AstConstructorDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ConstructorDeclaration):  ";
    for (int i = 0; i < NumConstructorModifiers(); i++)
    {
        Coutput << lex_stream.NameString(ConstructorModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << " #" << constructor_declarator -> id
            << " throws: (";
    for (int j = 0; j < NumThrows(); j++)
        Coutput << " #" << Throw(j) -> id;
    Coutput << ") #" << constructor_body -> id
            << endl;

    constructor_declarator -> Print(lex_stream);
    for (int k = 0; k < NumThrows(); k++)
        Throw(k) -> Print(lex_stream);
    constructor_body -> Print(lex_stream);
}

void AstInterfaceDeclaration::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (InterfaceDeclaration):  ";
    for (int i = 0; i < NumInterfaceModifiers(); i++)
    {
        Coutput << lex_stream.NameString(InterfaceModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << lex_stream.NameString(interface_token)
            << " "
            << lex_stream.NameString(identifier_token)
            << "(";
    for (int j = 0; j < NumExtendsInterfaces(); j++)
        Coutput << " #" << ExtendsInterface(j) -> id;
    Coutput << ") {";
    for (int m = 0; m < NumInterfaceMemberDeclarations(); m++)
        Coutput << " #" << InterfaceMemberDeclaration(m) -> id;
    Coutput << "}" << endl;

    for (int k = 0; k < NumExtendsInterfaces(); k++)
        ExtendsInterface(k) -> Print(lex_stream);
    for (int n = 0; n < NumInterfaceMemberDeclarations(); n++)
        InterfaceMemberDeclaration(n) -> Print(lex_stream);
}

void AstLocalVariableDeclarationStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (LocalVariableDeclarationStatement):  ";
    for (int i = 0; i < NumLocalModifiers(); i++)
    {
        Coutput << lex_stream.NameString(LocalModifier(i) -> modifier_kind_token)
                << " ";
    }
    Coutput << "#" << type -> id
            << "(";
    for (int j = 0; j < NumVariableDeclarators(); j++)
        Coutput << " #" << VariableDeclarator(j) -> id;
    Coutput << ")" << endl;

    type -> Print(lex_stream);
    for (int k = 0; k < NumVariableDeclarators(); k++)
        VariableDeclarator(k) -> Print(lex_stream);
}

void AstIfStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (IfStatement):  "
            << lex_stream.NameString(if_token)
            << " ( #" << expression -> id << " ) #" << true_statement -> id;
    if (false_statement_opt)
        Coutput << " else #" << false_statement_opt -> id;
    else Coutput << " #0";
    Coutput << endl;

    expression -> Print(lex_stream);
    true_statement -> Print(lex_stream);
    if (false_statement_opt)
        false_statement_opt -> Print(lex_stream);
}

void AstEmptyStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (EmptyStatement):  "
            << lex_stream.NameString(semicolon_token)
            << endl;
}

void AstExpressionStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ExpressionStatement):  " << "#" << expression -> id << endl;
    expression -> Print(lex_stream);
}

void AstCaseLabel::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (CaseLabel):  "
            << lex_stream.NameString(case_token)
            << " #" << expression -> id << ":" << endl;
    expression -> Print(lex_stream);
    Coutput << "    map_index: " << map_index << endl;
}

void AstDefaultLabel::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (DefaultLabel):  "
            << lex_stream.NameString(default_token)
            << ":" << endl;
}

void AstSwitchBlockStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (SwitchBlockStatement): ";
    for (int i = 0; i < NumSwitchLabels(); i++)
    {
        if (i % 10 == 0)
            Coutput << endl << "        ";
        Coutput << " #" << SwitchLabel(i) -> id << ':';
    }
    Coutput << endl;
    AstBlock::Print(lex_stream);

    for (int j = 0; j < NumSwitchLabels(); j++)
        SwitchLabel(j) -> Print(lex_stream);
}

void AstSwitchStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (SwitchStatement):  "
            << lex_stream.NameString(switch_token)
            << " ( #" << expression -> id << " ) #" << switch_block -> id << endl;

    Coutput << "default case: index " << default_case.index << endl;
    for (int i = 0; i < cases -> Length(); i++)
    {
        Coutput << "case: " << i << "  index: " << (*cases)[i] -> index << "  value: " << (*cases)[i] -> Value() << endl;
    }

    expression -> Print(lex_stream);
    switch_block -> Print(lex_stream);
}

void AstWhileStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (WhileStatement):  "
            << lex_stream.NameString(while_token)
            << " ( #" << expression -> id << " ) #" << statement -> id << endl;
    expression -> Print(lex_stream);
    statement -> Print(lex_stream);
}

void AstDoStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (DoStatement):  "
            << lex_stream.NameString(do_token)
            << " { #" << statement -> id << " } "
            << lex_stream.NameString(while_token)
            << " ( #" << expression -> id << " ) #" << endl;

    statement -> Print(lex_stream);
    expression -> Print(lex_stream);
}

void AstForStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ForStatement):  ("
            << lex_stream.NameString(for_token);
    for (int i = 0; i < NumForInitStatements(); i++)
        Coutput << " #" << ForInitStatement(i) -> id;
    Coutput << "; #" << (end_expression_opt ? end_expression_opt -> id : 0) << ";";
    for (int k = 0; k < NumForUpdateStatements(); k++)
        Coutput << " #" << ForUpdateStatement(k) -> id;
    Coutput << ") #" << statement -> id << endl;

    for (int m = 0; m < NumForInitStatements(); m++)
        ForInitStatement(m) -> Print(lex_stream);
    if (end_expression_opt)
        end_expression_opt -> Print(lex_stream);
    for (int n = 0; n < NumForUpdateStatements(); n++)
        ForUpdateStatement(n) -> Print(lex_stream);
    statement -> Print(lex_stream);
}

void AstBreakStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (BreakStatement):  "
            << lex_stream.NameString(break_token)
            << " "
            << (identifier_token_opt ? lex_stream.NameString(identifier_token_opt) : L"")
            << " at nesting_level " << nesting_level << endl;
}

void AstContinueStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ContinueStatement):  "
            << lex_stream.NameString(continue_token)
            << " "
            << (identifier_token_opt ? lex_stream.NameString(identifier_token_opt) : L"")
            << " at nesting_level " << nesting_level << endl;
}

void AstReturnStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ReturnStatement):  "
            << lex_stream.NameString(return_token)
            << " "
            << " #" << (expression_opt ? expression_opt -> id : 0) << endl;
    if (expression_opt)
        expression_opt -> Print(lex_stream);
}

void AstThrowStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ThrowStatement):  "
            << lex_stream.NameString(throw_token)
            << " "
            << " #" << expression -> id << endl;
    expression -> Print(lex_stream);
}

void AstSynchronizedStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (SynchronizedStatement):  "
            << lex_stream.NameString(synchronized_token)
            << " ( #" << expression -> id
            << " ) #" << block -> id << endl;
    expression -> Print(lex_stream);
    block -> Print(lex_stream);
}

void AstAssertStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (AssertStatement):  "
            << lex_stream.NameString(assert_token)
            << " ( #" << condition -> id;
    if (message_opt)
        Coutput << " : " << message_opt -> id;
    else Coutput << " #0";
    Coutput << " ;" << endl;
    condition -> Print(lex_stream);
    if (message_opt)
        message_opt -> Print(lex_stream);
}

void AstCatchClause::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (CatchClause):  "
            << lex_stream.NameString(catch_token)
            << " #" << formal_parameter -> id
            << " #" << block -> id << endl;
    formal_parameter -> Print(lex_stream);
    block -> Print(lex_stream);
}

void AstFinallyClause::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (FinallyClause):  "
            << lex_stream.NameString(finally_token)
            << " #" << block -> id << endl;
    block -> Print(lex_stream);
}

void AstTryStatement::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (TryStatement):  "
            << lex_stream.NameString(try_token)
            << " #" << block -> id
            << " catch (";
    for (int i = 0; i < NumCatchClauses(); i++)
        Coutput << " #" << CatchClause(i) -> id;
    Coutput << ") finally " << "#" << (finally_clause_opt ? finally_clause_opt -> id : 0) << endl;

    block -> Print(lex_stream);
    for (int k = 0; k < NumCatchClauses(); k++)
        CatchClause(k) -> Print(lex_stream);
    if (finally_clause_opt)
        finally_clause_opt -> Print(lex_stream);
}

void AstIntegerLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (IntegerLiteral):  "
            << lex_stream.NameString(integer_literal_token)
            << endl;
}

void AstLongLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (LongLiteral):  "
            << lex_stream.NameString(long_literal_token)
            << endl;
}

void AstFloatLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (FloatLiteral):  "
            << lex_stream.NameString(float_literal_token)
            << endl;
}

void AstDoubleLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (DoubleLiteral):  "
            << lex_stream.NameString(double_literal_token)
            << endl;
}

void AstTrueLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (TrueLiteral):  "
            << lex_stream.NameString(true_literal_token)
            << endl;
}

void AstFalseLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (FalseLiteral):  "
            << lex_stream.NameString(false_literal_token)
            << endl;
}

void AstStringLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (StringLiteral):  "
            << lex_stream.NameString(string_literal_token)
            << endl;
}

void AstCharacterLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (CharacterLiteral):  "
            << lex_stream.NameString(character_literal_token)
            << endl;
}

void AstNullLiteral::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (NullLiteral):  "
            << lex_stream.NameString(null_token)
            << endl;
}

void AstThisExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ThisExpression):  "
            << lex_stream.NameString(this_token)
            << endl;
}

void AstSuperExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (SuperExpression):  "
            << lex_stream.NameString(super_token)
            << endl;
}

void AstParenthesizedExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ParenthesizedExpression):  "
            << lex_stream.NameString(left_parenthesis_token)
            << "#" << expression -> id
            << lex_stream.NameString(right_parenthesis_token)
            << endl;
    expression -> Print(lex_stream);
}

void AstTypeExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (TypeExpression):  "
            << " #" << type -> id << endl;
    type -> Print(lex_stream);
}

void AstClassInstanceCreationExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ClassInstanceCreationExpression):  ";
    if (base_opt)
    {
        Coutput << "#" << base_opt -> id
                << lex_stream.NameString(dot_token_opt) << " ";
    }
    Coutput << " ";
    Coutput << lex_stream.NameString(new_token)
            << " #" << class_type -> id
            << " (";
    for (int i = 0; i < NumArguments(); i++)
        Coutput << " #" << Argument(i) -> id;
    Coutput << " ) "
            << "#" << (class_body_opt ? class_body_opt -> id : 0) << endl;

    if (base_opt)
        base_opt -> Print(lex_stream);

    class_type -> Print(lex_stream);
    for (int j = 0; j < NumArguments(); j++)
        Argument(j) -> Print(lex_stream);

    if (class_body_opt)
        class_body_opt -> Print(lex_stream);
}

void AstDimExpr::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (DimExpr):  [ #" << expression -> id << " ]" << endl;
    expression -> Print(lex_stream);
}

void AstArrayCreationExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ArrayCreationExpression):  "
            << lex_stream.NameString(new_token)
            << " #" << array_type -> id << "( ";
    for (int i = 0; i < NumDimExprs(); i++)
        Coutput << " [#" << DimExpr(i) -> id << "]";
    Coutput << " ) (";
    for (int k = 0; k < NumBrackets(); k++)
        Coutput << " #" << Brackets(k) -> id;
    Coutput << " ) #"
            << (array_initializer_opt ? array_initializer_opt -> id : 0) << endl;

    array_type -> Print(lex_stream);
    for (int j = 0; j < NumDimExprs(); j++)
        DimExpr(j) -> Print(lex_stream);
    for (int l = 0; l < NumBrackets(); l++)
        Brackets(l) -> Print(lex_stream);
}

void AstFieldAccess::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (FieldAccess):  "
            << " #" << base -> id << " "
            << lex_stream.NameString(identifier_token)
            << endl;

    base -> Print(lex_stream);
}

void AstMethodInvocation::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (MethodInvocation):  "
            << "#" << method -> id
            << " (";
    for (int i = 0; i < NumArguments(); i++)
        Coutput << " #" << Argument(i) -> id;
    Coutput << " )" << endl;

    method -> Print(lex_stream);
    for (int j = 0; j < NumArguments(); j++)
        Argument(j) -> Print(lex_stream);
}

void AstArrayAccess::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ArrayAccess):  "
            << "#" << base -> id
            << " [ #" << expression -> id << " ]" << endl;

    base -> Print(lex_stream);
    expression -> Print(lex_stream);
}

void AstPostUnaryExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (PostUnaryExpression):  "
            << "#" << expression -> id
            << lex_stream.NameString(post_operator_token)
            << endl;

    expression -> Print(lex_stream);
}

void AstPreUnaryExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (PreUnaryExpression):  "
            << lex_stream.NameString(pre_operator_token)
            << " #" << expression -> id << endl;

    expression -> Print(lex_stream);
}

void AstCastExpression::Print(LexStream& lex_stream)
{
    if (type_opt)
    {
        Coutput << "#" << id
                << (kind == CAST ? " (CastExpression: just cast):  "
                    : " (CastExpression: check and cast):  ")
                << "( #" << type_opt -> id;
        for (int i = 0; i < NumBrackets(); i++)
            Coutput << " #" << Brackets(i) -> id;
        Coutput << " ) #" << expression -> id << endl;
        if (type_opt)
            type_opt -> Print(lex_stream);
        for (int j = 0; j < NumBrackets(); j++)
            Brackets(j) -> Print(lex_stream);
    }
    else
    {
        Coutput << "#" << id << " (Java Semantic Cast to " << Type() -> Name()
                << "):  #" << expression -> id << endl;
    }

    expression -> Print(lex_stream);
}

void AstBinaryExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (BinaryExpression):  "
            << "#" << left_expression -> id << " "
            << lex_stream.NameString(binary_operator_token)
            << " #" << right_expression -> id << endl;

    left_expression -> Print(lex_stream);
    right_expression -> Print(lex_stream);
}

void AstConditionalExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (ConditionalExpression):  "
            << "#" << test_expression -> id
            << " ? #" << true_expression -> id
            << " : #" << false_expression -> id << endl;

    test_expression -> Print(lex_stream);
    true_expression -> Print(lex_stream);
    false_expression -> Print(lex_stream);
}

void AstAssignmentExpression::Print(LexStream& lex_stream)
{
    Coutput << "#" << id << " (AssignmentExpression):  "
            << "#" << left_hand_side -> id << " "
            << lex_stream.NameString(assignment_operator_token)
            << " #" << expression -> id << endl;

    left_hand_side -> Print(lex_stream);
    expression -> Print(lex_stream);
}

#endif // JIKES_DEBUG

//
// NOTE: Since Ast objects are created from a pool, and the pool does its own
// memory management, it is easier to just free the entire pool rather than
// try to destruct the Ast objects. Hence, for now, all Ast destructors cause
// an assertion failure, and you should never create an Ast object with the
// regular new operator.
//

Ast::~Ast()
{
    assert(false);
}

AstStatement::~AstStatement()
{
    assert(false);
}

AstExpression::~AstExpression()
{
    assert(false);
}

AstBlock::~AstBlock()
{
    assert(false);
}

AstPrimitiveType::~AstPrimitiveType()
{
    assert(false);
}

AstArrayType::~AstArrayType()
{
    assert(false);
}

AstSimpleName::~AstSimpleName()
{
    assert(false);
}

AstPackageDeclaration::~AstPackageDeclaration()
{
    assert(false);
}

AstImportDeclaration::~AstImportDeclaration()
{
    assert(false);
}

AstCompilationUnit::~AstCompilationUnit()
{
    assert(false);
}

AstModifier::~AstModifier()
{
    assert(false);
}

AstEmptyDeclaration::~AstEmptyDeclaration()
{
    assert(false);
}

AstClassBody::~AstClassBody()
{
    assert(false);
}

AstClassDeclaration::~AstClassDeclaration()
{
    assert(false);
}

AstArrayInitializer::~AstArrayInitializer()
{
    assert(false);
}

AstBrackets::~AstBrackets()
{
    assert(false);
}

AstVariableDeclaratorId::~AstVariableDeclaratorId()
{
    assert(false);
}

AstVariableDeclarator::~AstVariableDeclarator()
{
    assert(false);
}

AstFieldDeclaration::~AstFieldDeclaration()
{
    assert(false);
}

AstFormalParameter::~AstFormalParameter()
{
    assert(false);
}

AstMethodDeclarator::~AstMethodDeclarator()
{
    assert(false);
}

AstMethodDeclaration::~AstMethodDeclaration()
{
    assert(false);
}

AstStaticInitializer::~AstStaticInitializer()
{
    assert(false);
}

AstThisCall::~AstThisCall()
{
    assert(false);
}

AstSuperCall::~AstSuperCall()
{
    assert(false);
}

AstMethodBody::~AstMethodBody()
{
    assert(false);
}

AstConstructorDeclaration::~AstConstructorDeclaration()
{
    assert(false);
}

AstInterfaceDeclaration::~AstInterfaceDeclaration()
{
    assert(false);
}

AstLocalVariableDeclarationStatement::~AstLocalVariableDeclarationStatement()
{
    assert(false);
}

AstIfStatement::~AstIfStatement()
{
    assert(false);
}

AstEmptyStatement::~AstEmptyStatement()
{
    assert(false);
}

AstExpressionStatement::~AstExpressionStatement()
{
    assert(false);
}

AstCaseLabel::~AstCaseLabel()
{
    assert(false);
}

AstDefaultLabel::~AstDefaultLabel()
{
    assert(false);
}

AstSwitchBlockStatement::~AstSwitchBlockStatement()
{
    assert(false);
}

AstSwitchStatement::~AstSwitchStatement()
{
    assert(false);
}

AstWhileStatement::~AstWhileStatement()
{
    assert(false);
}

AstDoStatement::~AstDoStatement()
{
    assert(false);
}

AstForStatement::~AstForStatement()
{
    assert(false);
}

AstBreakStatement::~AstBreakStatement()
{
    assert(false);
}

AstContinueStatement::~AstContinueStatement()
{
    assert(false);
}

AstReturnStatement::~AstReturnStatement()
{
    assert(false);
}

AstThrowStatement::~AstThrowStatement()
{
    assert(false);
}

AstSynchronizedStatement::~AstSynchronizedStatement()
{
    assert(false);
}

AstAssertStatement::~AstAssertStatement()
{
    assert(false);
}

AstCatchClause::~AstCatchClause()
{
    assert(false);
}

AstFinallyClause::~AstFinallyClause()
{
    assert(false);
}

AstTryStatement::~AstTryStatement()
{
    assert(false);
}

AstIntegerLiteral::~AstIntegerLiteral()
{
    assert(false);
}

AstLongLiteral::~AstLongLiteral()
{
    assert(false);
}

AstFloatLiteral::~AstFloatLiteral()
{
    assert(false);
}

AstDoubleLiteral::~AstDoubleLiteral()
{
    assert(false);
}

AstTrueLiteral::~AstTrueLiteral()
{
    assert(false);
}

AstFalseLiteral::~AstFalseLiteral()
{
    assert(false);
}

AstStringLiteral::~AstStringLiteral()
{
    assert(false);
}

AstCharacterLiteral::~AstCharacterLiteral()
{
    assert(false);
}

AstNullLiteral::~AstNullLiteral()
{
    assert(false);
}

AstThisExpression::~AstThisExpression()
{
    assert(false);
}

AstSuperExpression::~AstSuperExpression()
{
    assert(false);
}

AstParenthesizedExpression::~AstParenthesizedExpression()
{
    assert(false);
}

AstTypeExpression::~AstTypeExpression()
{
    assert(false);
}

AstClassInstanceCreationExpression::~AstClassInstanceCreationExpression()
{
    assert(false);
}

AstDimExpr::~AstDimExpr()
{
    assert(false);
}

AstArrayCreationExpression::~AstArrayCreationExpression()
{
    assert(false);
}

AstFieldAccess::~AstFieldAccess()
{
    assert(false);
}

AstMethodInvocation::~AstMethodInvocation()
{
    assert(false);
}

AstArrayAccess::~AstArrayAccess()
{
    assert(false);
}

AstPostUnaryExpression::~AstPostUnaryExpression()
{
    assert(false);
}

AstPreUnaryExpression::~AstPreUnaryExpression()
{
    assert(false);
}

AstCastExpression::~AstCastExpression()
{
    assert(false);
}

AstBinaryExpression::~AstBinaryExpression()
{
   assert(false);
}

AstConditionalExpression::~AstConditionalExpression()
{
    assert(false);
}

AstAssignmentExpression::~AstAssignmentExpression()
{
    assert(false);
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

