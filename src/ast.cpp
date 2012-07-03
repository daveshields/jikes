// $Id: ast.cpp,v 1.15 1999/10/13 16:17:40 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "config.h"
#include "ast.h"
#ifdef TEST
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
    int k = size >> log_blksize; /* which segment? */

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

    return;
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

    while(top >= 0)
    {
        lower = lostack[top];
        upper = histack[top];
        top--;

        while(upper > lower)
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

    return;
}


Ast *Ast::Clone(StoragePool *ast_pool)
{
    return (Ast *) NULL;
}

Ast *AstBlock::Clone(StoragePool *ast_pool)
{
    AstBlock *clone = ast_pool -> GenBlock();

    for (int i = 0; i < this -> NumLabels(); i++)
        clone -> AddLabel(this -> Label(i));
    clone -> nesting_level = this -> nesting_level;
    clone -> left_brace_token = this -> left_brace_token;
    if (this -> NumStatements() == 0)
        clone -> block_statements = NULL;
    else
    {
        for (int j = 0; j < this -> NumStatements(); j++)
            clone -> AddStatement(this -> Statement(j) -> Clone(ast_pool));
    }
    clone -> right_brace_token = this -> right_brace_token;

    return clone;
}

Ast *AstPrimitiveType::Clone(StoragePool *ast_pool)
{
    AstPrimitiveType *clone = ast_pool -> GenPrimitiveType(this -> kind, this -> primitive_kind_token);

    return clone;
}

Ast *AstArrayType::Clone(StoragePool *ast_pool)
{
    AstArrayType *clone = ast_pool -> GenArrayType();

    clone -> type = this -> type -> Clone(ast_pool);
    clone -> AllocateBrackets(this -> NumBrackets());
    for (int i = 0; i < this -> NumBrackets(); i++)
        clone -> AddBrackets((AstBrackets *) this -> Brackets(i) -> Clone(ast_pool));

    return clone;
}

Ast *AstSimpleName::Clone(StoragePool *ast_pool)
{
    AstSimpleName *clone = ast_pool -> GenSimpleName(this -> identifier_token);
    clone -> resolution_opt = (AstExpression *) (this -> resolution_opt ? this -> resolution_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstPackageDeclaration::Clone(StoragePool *ast_pool)
{
    AstPackageDeclaration *clone = ast_pool -> GenPackageDeclaration();

    clone -> package_token = this -> package_token;
    clone -> name = (AstExpression *) this -> name -> Clone(ast_pool);
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstImportDeclaration::Clone(StoragePool *ast_pool)
{
    AstImportDeclaration *clone = ast_pool -> GenImportDeclaration();

    clone -> import_token = this -> import_token;
    clone -> name = (AstExpression *) this -> name -> Clone(ast_pool);
    clone -> star_token_opt = this -> star_token_opt;
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstCompilationUnit::Clone(StoragePool *ast_pool)
{
    AstCompilationUnit *clone = ast_pool -> GenCompilationUnit();

    clone -> package_declaration_opt = (AstPackageDeclaration *)
                                       (this -> package_declaration_opt
                                              ? this -> package_declaration_opt -> Clone(ast_pool) : NULL);
    for (int i = 0; i < this -> NumImportDeclarations(); i++)
        clone -> AddImportDeclaration((AstImportDeclaration *) this -> ImportDeclaration(i) -> Clone(ast_pool));
    for (int k = 0; k < this -> NumTypeDeclarations(); k++)
        clone -> AddTypeDeclaration(this -> TypeDeclaration(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstModifier::Clone(StoragePool *ast_pool)
{
    AstModifier *clone = ast_pool -> GenModifier(this -> kind, this -> modifier_kind_token);

    return clone;
}

Ast *AstEmptyDeclaration::Clone(StoragePool *ast_pool)
{
    AstEmptyDeclaration *clone = ast_pool -> GenEmptyDeclaration(this -> semicolon_token);

    return clone;
}

Ast *AstClassBody::Clone(StoragePool *ast_pool)
{
    AstClassBody *clone = ast_pool -> GenClassBody();

    clone -> left_brace_token = this -> left_brace_token;
    for (int i = 0; i < this -> NumClassBodyDeclarations(); i++)
        clone -> AddClassBodyDeclaration(this -> ClassBodyDeclaration(i) -> Clone(ast_pool));
    clone -> right_brace_token = this -> right_brace_token;

    return clone;
}

Ast *AstClassDeclaration::Clone(StoragePool *ast_pool)
{
    AstClassDeclaration *clone = ast_pool -> GenClassDeclaration();

    for (int i = 0; i < this -> NumClassModifiers(); i++)
        clone -> AddClassModifier((AstModifier *) this -> ClassModifier(i) -> Clone(ast_pool));
    clone -> class_token = this -> class_token;
    clone -> identifier_token = this -> identifier_token;
    clone -> super_opt = (Ast *) (this -> super_opt ? this -> super_opt -> Clone(ast_pool) : NULL);
    for (int k = 0; k < this -> NumInterfaces(); k++)
        clone -> AddInterface((AstExpression *) this -> Interface(k) -> Clone(ast_pool));
    clone -> class_body = (AstClassBody *) this -> class_body -> Clone(ast_pool);

    return clone;
}

Ast *AstArrayInitializer::Clone(StoragePool *ast_pool)
{
    AstArrayInitializer *clone = ast_pool -> GenArrayInitializer();

    clone -> left_brace_token = this -> left_brace_token;
    for (int k = 0; k < this -> NumVariableInitializers(); k++)
        clone -> AddVariableInitializer(this -> VariableInitializer(k) -> Clone(ast_pool));
    clone -> right_brace_token = this -> right_brace_token;

    return clone;
}

Ast *AstBrackets::Clone(StoragePool *ast_pool)
{
    AstBrackets *clone = ast_pool -> GenBrackets(this -> left_bracket_token, this -> right_bracket_token);

    return clone;
}

Ast *AstVariableDeclaratorId::Clone(StoragePool *ast_pool)
{
    AstVariableDeclaratorId *clone = ast_pool -> GenVariableDeclaratorId();

    clone -> identifier_token = this -> identifier_token;
    clone -> AllocateBrackets(this -> NumBrackets());
    for (int i = 0; i < this -> NumBrackets(); i++)
        clone -> AddBrackets((AstBrackets *) this -> Brackets(i) -> Clone(ast_pool));

    return clone;
}

Ast *AstVariableDeclarator::Clone(StoragePool *ast_pool)
{
    AstVariableDeclarator *clone = ast_pool -> GenVariableDeclarator();

    clone -> variable_declarator_name = (AstVariableDeclaratorId *) this -> variable_declarator_name -> Clone(ast_pool);
    clone -> variable_initializer_opt = (Ast *) (this -> variable_initializer_opt
                                                       ? this -> variable_initializer_opt -> Clone(ast_pool)
                                                       : NULL);

    return clone;
}

Ast *AstFieldDeclaration::Clone(StoragePool *ast_pool)
{
    AstFieldDeclaration *clone = ast_pool -> GenFieldDeclaration();

    for (int i = 0; i < this -> NumVariableModifiers(); i++)
        clone -> AddVariableModifier((AstModifier *) this -> VariableModifier(i) -> Clone(ast_pool));
    clone -> type = this -> type -> Clone(ast_pool);
    for (int k = 0; k < this -> NumVariableDeclarators(); k++)
        clone -> AddVariableDeclarator((AstVariableDeclarator *) this -> VariableDeclarator(k) -> Clone(ast_pool));
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstFormalParameter::Clone(StoragePool *ast_pool)
{
    AstFormalParameter *clone = ast_pool -> GenFormalParameter();

    if (this -> NumParameterModifiers() == 0)
        clone -> parameter_modifiers = NULL;
    else
    {
        for (int i = 0; i < this -> NumParameterModifiers(); i++)
            clone -> AddParameterModifier((AstModifier *) this -> ParameterModifier(i) -> Clone(ast_pool));
    }
    clone -> type = this -> type -> Clone(ast_pool);
    clone -> formal_declarator = (AstVariableDeclarator *) this -> formal_declarator -> Clone(ast_pool);

    return clone;
}

Ast *AstMethodDeclarator::Clone(StoragePool *ast_pool)
{
    AstMethodDeclarator *clone = ast_pool -> GenMethodDeclarator();

    clone -> identifier_token = this -> identifier_token;
    clone -> left_parenthesis_token = this -> left_parenthesis_token;
    clone -> AllocateFormalParameters(this -> NumFormalParameters());
    for (int i = 0; i < this -> NumFormalParameters(); i++)
        clone -> AddFormalParameter((AstFormalParameter *) this -> FormalParameter(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = this -> right_parenthesis_token;
    clone -> AllocateBrackets(this -> NumBrackets());
    for (int k = 0; k < this -> NumBrackets(); k++)
        clone -> AddBrackets((AstBrackets *) this -> Brackets(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstMethodDeclaration::Clone(StoragePool *ast_pool)
{
    AstMethodDeclaration *clone = ast_pool -> GenMethodDeclaration();

    for (int i = 0; i < this -> NumMethodModifiers(); i++)
        clone -> AddMethodModifier((AstModifier *) this -> MethodModifier(i) -> Clone(ast_pool));
    clone -> type = this -> type -> Clone(ast_pool);
    clone -> method_declarator = (AstMethodDeclarator *) this -> method_declarator -> Clone(ast_pool);
    for (int k = 0; k < this -> NumThrows(); k++)
        clone -> AddThrow((AstExpression *) this -> Throw(k) -> Clone(ast_pool));
    clone -> method_body = (AstStatement *) this -> method_body -> Clone(ast_pool);

    return clone;
}

Ast *AstStaticInitializer::Clone(StoragePool *ast_pool)
{
    AstStaticInitializer *clone = ast_pool -> GenStaticInitializer();

    clone -> static_token = this -> static_token;
    clone -> block = (AstBlock *) this -> block -> Clone(ast_pool);

    return clone;
}

Ast *AstThisCall::Clone(StoragePool *ast_pool)
{
    AstThisCall *clone = ast_pool -> GenThisCall();

    clone -> this_token = this -> this_token;
    clone -> left_parenthesis_token = this -> left_parenthesis_token;
    clone -> AllocateArguments(this -> NumArguments());
    for (int i = 0; i < this -> NumArguments(); i++)
        clone -> AddArgument((AstExpression *) this -> Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = this -> right_parenthesis_token;
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstSuperCall::Clone(StoragePool *ast_pool)
{
    AstSuperCall *clone = ast_pool -> GenSuperCall();

    clone -> base_opt = (AstExpression *) (this -> base_opt ? this -> base_opt -> Clone(ast_pool) : NULL);
    clone -> dot_token_opt = this -> dot_token_opt;
    clone -> super_token = this -> super_token;
    clone -> left_parenthesis_token = this -> left_parenthesis_token;
    clone -> AllocateArguments(this -> NumArguments());
    for (int i = 0; i < this -> NumArguments(); i++)
        clone -> AddArgument((AstExpression *) this -> Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = this -> right_parenthesis_token;
    clone -> semicolon_token = this -> semicolon_token;
    clone -> AllocateLocalArguments(this -> NumLocalArguments());
    for (int k = 0; k < this -> NumLocalArguments(); k++)
        clone -> AddLocalArgument((AstExpression *) this -> LocalArgument(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstConstructorBlock::Clone(StoragePool *ast_pool)
{
    AstConstructorBlock *clone = ast_pool -> GenConstructorBlock();

    clone -> left_brace_token = this -> left_brace_token;
    clone -> explicit_constructor_invocation_opt = (Ast *)
                                                   (this -> explicit_constructor_invocation_opt
                                                          ? this -> explicit_constructor_invocation_opt -> Clone(ast_pool)
                                                          : NULL);
    clone -> block = (AstBlock *) this -> block -> Clone(ast_pool);
    clone -> right_brace_token = this -> right_brace_token;

    return clone;
}

Ast *AstConstructorDeclaration::Clone(StoragePool *ast_pool)
{
    AstConstructorDeclaration *clone = ast_pool -> GenConstructorDeclaration();

    for (int i = 0; i < this -> NumConstructorModifiers(); i++)
        clone -> AddConstructorModifier((AstModifier *) this -> ConstructorModifier(i) -> Clone(ast_pool));
    clone -> constructor_declarator = (AstMethodDeclarator *) this -> constructor_declarator -> Clone(ast_pool);
    for (int k = 0; k < this -> NumThrows(); k++)
        clone -> AddThrow((AstExpression *) this -> Throw(k) -> Clone(ast_pool));
    clone -> constructor_body = (AstConstructorBlock *) this -> constructor_body -> Clone(ast_pool);

    return clone;
}

Ast *AstInterfaceDeclaration::Clone(StoragePool *ast_pool)
{
    AstInterfaceDeclaration *clone = ast_pool -> GenInterfaceDeclaration();

    for (int i = 0; i < this -> NumInterfaceModifiers(); i++)
        clone -> AddInterfaceModifier((AstModifier *) this -> InterfaceModifier(i) -> Clone(ast_pool));
    clone -> interface_token = this -> interface_token;
    clone -> identifier_token = this -> identifier_token;
    for (int k = 0; k < this -> NumExtendsInterfaces(); k++)
        clone -> AddExtendsInterface((AstExpression *) this -> ExtendsInterface(k) -> Clone(ast_pool));
    clone -> left_brace_token = this -> left_brace_token;
    for (int l = 0; l < this -> NumExtendsInterfaces(); l++)
        clone -> AddInterfaceMemberDeclaration((AstExpression *) this -> InterfaceMemberDeclaration(l) -> Clone(ast_pool));
    clone -> right_brace_token = this -> right_brace_token;

    return clone;
}

Ast *AstLocalVariableDeclarationStatement::Clone(StoragePool *ast_pool)
{
    AstLocalVariableDeclarationStatement *clone = ast_pool -> GenLocalVariableDeclarationStatement();

    for (int i = 0; i < this -> NumLocalModifiers(); i++)
        clone -> AddLocalModifier((AstModifier *) this -> LocalModifier(i) -> Clone(ast_pool));
    clone -> type = this -> type -> Clone(ast_pool);
    for (int k = 0; k < this -> NumVariableDeclarators(); k++)
        clone -> AddVariableDeclarator((AstVariableDeclarator *) this -> VariableDeclarator(k) -> Clone(ast_pool));
    clone -> semicolon_token_opt = this -> semicolon_token_opt;

    return clone;
}

Ast *AstIfStatement::Clone(StoragePool *ast_pool)
{
    AstIfStatement *clone = ast_pool -> GenIfStatement();

    clone -> if_token = this -> if_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> true_statement = (AstStatement *) this -> true_statement -> Clone(ast_pool);
    clone -> false_statement_opt = (AstStatement *)
                                   (this -> false_statement_opt
                                          ? this -> false_statement_opt -> Clone(ast_pool)
                                          : NULL);

    return clone;
}

Ast *AstEmptyStatement::Clone(StoragePool *ast_pool)
{
    AstEmptyStatement *clone = ast_pool -> GenEmptyStatement(this -> semicolon_token);

    return clone;
}

Ast *AstExpressionStatement::Clone(StoragePool *ast_pool)
{
    AstExpressionStatement *clone = ast_pool -> GenExpressionStatement();

    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> semicolon_token_opt = this -> semicolon_token_opt;

    return clone;
}

Ast *AstCaseLabel::Clone(StoragePool *ast_pool)
{
    AstCaseLabel *clone = ast_pool -> GenCaseLabel();

    clone -> case_token = this -> case_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> colon_token = this -> colon_token;
    clone -> map_index = this -> map_index;

    return clone;
}

Ast *AstDefaultLabel::Clone(StoragePool *ast_pool)
{
    AstDefaultLabel *clone = ast_pool -> GenDefaultLabel();

    clone -> default_token = this -> default_token;
    clone -> colon_token = this -> colon_token;

    return clone;
}

Ast *AstSwitchBlockStatement::Clone(StoragePool *ast_pool)
{
    AstSwitchBlockStatement *clone = ast_pool -> GenSwitchBlockStatement();

    clone -> AllocateSwitchLabels(this -> NumSwitchLabels());
    for (int i = 0; i < this -> NumSwitchLabels(); i++)
        clone -> AddSwitchLabel(this -> SwitchLabel(i) -> Clone(ast_pool));

    clone -> AllocateBlockStatements(this -> NumStatements());
    for (int k = 0; k < this -> NumStatements(); k++)
        clone -> AddStatement((AstStatement *) this -> Statement(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstSwitchStatement::Clone(StoragePool *ast_pool)
{
    AstSwitchStatement *clone = ast_pool -> GenSwitchStatement();

    clone -> switch_token = this -> switch_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> switch_block = (AstBlock *) this -> switch_block -> Clone(ast_pool);

    return clone;
}

Ast *AstWhileStatement::Clone(StoragePool *ast_pool)
{
    AstWhileStatement *clone = ast_pool -> GenWhileStatement();

    clone -> while_token = this -> while_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> statement = (AstStatement *) this -> statement -> Clone(ast_pool);

    return clone;
}

Ast *AstDoStatement::Clone(StoragePool *ast_pool)
{
    AstDoStatement *clone = ast_pool -> GenDoStatement();

    clone -> do_token = this -> do_token;
    clone -> statement = (AstStatement *) this -> statement -> Clone(ast_pool);
    clone -> while_token = this -> while_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstForStatement::Clone(StoragePool *ast_pool)
{
    AstForStatement *clone = ast_pool -> GenForStatement();

    clone -> for_token = this -> for_token;
    for (int i = 0; i < this -> NumForInitStatements(); i++)
        clone -> AddForInitStatement((AstStatement *) this -> ForInitStatement(i) -> Clone(ast_pool));
    clone -> end_expression_opt = (AstExpression *)
                                  (this -> end_expression_opt
                                         ? this -> end_expression_opt -> Clone(ast_pool)
                                         : NULL);
    for (int k = 0; k < this -> NumForUpdateStatements(); k++)
        clone -> AddForUpdateStatement((AstExpressionStatement *) this -> ForUpdateStatement(k) -> Clone(ast_pool));
    clone -> statement = (AstStatement *) this -> statement -> Clone(ast_pool);

    return clone;
}

Ast *AstBreakStatement::Clone(StoragePool *ast_pool)
{
    AstBreakStatement *clone = ast_pool -> GenBreakStatement();

    clone -> break_token = this -> break_token;
    clone -> identifier_token_opt = this -> identifier_token_opt;
    clone -> semicolon_token = this -> semicolon_token;
    clone -> nesting_level = this -> nesting_level;

    return clone;
}

Ast *AstContinueStatement::Clone(StoragePool *ast_pool)
{
    AstContinueStatement *clone = ast_pool -> GenContinueStatement();

    clone -> continue_token = this -> continue_token;
    clone -> identifier_token_opt = this -> identifier_token_opt;
    clone -> semicolon_token = this -> semicolon_token;
    clone -> nesting_level = this -> nesting_level;

    return clone;
}

Ast *AstReturnStatement::Clone(StoragePool *ast_pool)
{
    AstReturnStatement *clone = ast_pool -> GenReturnStatement();

    clone -> return_token = this -> return_token;
    clone -> expression_opt = (AstExpression *) (this -> expression_opt ? this -> expression_opt -> Clone(ast_pool) : NULL);
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstThrowStatement::Clone(StoragePool *ast_pool)
{
    AstThrowStatement *clone = ast_pool -> GenThrowStatement();

    clone -> throw_token = this -> throw_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> semicolon_token = this -> semicolon_token;

    return clone;
}

Ast *AstSynchronizedStatement::Clone(StoragePool *ast_pool)
{
    AstSynchronizedStatement *clone = ast_pool -> GenSynchronizedStatement();

    clone -> synchronized_token = this -> synchronized_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> block = (AstBlock *) this -> block -> Clone(ast_pool);

    return clone;
}

Ast *AstCatchClause::Clone(StoragePool *ast_pool)
{
    AstCatchClause *clone = ast_pool -> GenCatchClause();

    clone -> catch_token = this -> catch_token;
    clone -> formal_parameter = (AstFormalParameter *) this -> formal_parameter -> Clone(ast_pool);
    clone -> block = (AstBlock *) this -> block -> Clone(ast_pool);

    return clone;
}

Ast *AstFinallyClause::Clone(StoragePool *ast_pool)
{
    AstFinallyClause *clone = ast_pool -> GenFinallyClause();

    clone -> finally_token = this -> finally_token;
    clone -> block = (AstBlock *) this -> block -> Clone(ast_pool);

    return clone;
}

Ast *AstTryStatement::Clone(StoragePool *ast_pool)
{
    AstTryStatement *clone = ast_pool -> GenTryStatement();

    clone -> try_token = this -> try_token;
    clone -> block = (AstBlock *) this -> block -> Clone(ast_pool);
    for (int k = 0; k < this -> NumCatchClauses(); k++)
        clone -> AddCatchClause((AstCatchClause *) this -> CatchClause(k) -> Clone(ast_pool));
    clone -> finally_clause_opt = (AstFinallyClause *)
                                  (this -> finally_clause_opt
                                         ? this -> finally_clause_opt -> Clone(ast_pool)
                                         : NULL);

    return clone;
}

Ast *AstIntegerLiteral::Clone(StoragePool *ast_pool)
{
    AstIntegerLiteral *clone = ast_pool -> GenIntegerLiteral(this -> integer_literal_token);

    return clone;
}

Ast *AstLongLiteral::Clone(StoragePool *ast_pool)
{
    AstLongLiteral *clone = ast_pool -> GenLongLiteral(this -> long_literal_token);

    return clone;
}

Ast *AstFloatingPointLiteral::Clone(StoragePool *ast_pool)
{
    AstFloatingPointLiteral *clone = ast_pool -> GenFloatingPointLiteral(this -> floating_point_literal_token);

    return clone;
}

Ast *AstDoubleLiteral::Clone(StoragePool *ast_pool)
{
    AstDoubleLiteral *clone = ast_pool -> GenDoubleLiteral(this -> double_literal_token);

    return clone;
}

Ast *AstTrueLiteral::Clone(StoragePool *ast_pool)
{
    AstTrueLiteral *clone = ast_pool -> GenTrueLiteral(this -> true_literal_token);

    return clone;
}

Ast *AstFalseLiteral::Clone(StoragePool *ast_pool)
{
    AstFalseLiteral *clone = ast_pool -> GenFalseLiteral(this -> false_literal_token);

    return clone;
}

Ast *AstStringLiteral::Clone(StoragePool *ast_pool)
{
    AstStringLiteral *clone = ast_pool -> GenStringLiteral(this -> string_literal_token);

    return clone;
}

Ast *AstCharacterLiteral::Clone(StoragePool *ast_pool)
{
    AstCharacterLiteral *clone = ast_pool -> GenCharacterLiteral(this -> character_literal_token);

    return clone;
}

Ast *AstNullLiteral::Clone(StoragePool *ast_pool)
{
    AstNullLiteral *clone = ast_pool -> GenNullLiteral(this -> null_token);

    return clone;
}

Ast *AstThisExpression::Clone(StoragePool *ast_pool)
{
    AstThisExpression *clone = ast_pool -> GenThisExpression(this -> this_token);

    return clone;
}

Ast *AstSuperExpression::Clone(StoragePool *ast_pool)
{
    AstSuperExpression *clone = ast_pool -> GenSuperExpression(this -> super_token);

    return clone;
}

Ast *AstParenthesizedExpression::Clone(StoragePool *ast_pool)
{
    AstParenthesizedExpression *clone = ast_pool -> GenParenthesizedExpression();

    clone -> left_parenthesis_token = this -> left_parenthesis_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> right_parenthesis_token = this -> right_parenthesis_token;

    return clone;
}

Ast *AstTypeExpression::Clone(StoragePool *ast_pool)
{
    AstTypeExpression *clone = ast_pool -> GenTypeExpression(this -> type -> Clone(ast_pool));

    return clone;
}

Ast *AstClassInstanceCreationExpression::Clone(StoragePool *ast_pool)
{
    AstClassInstanceCreationExpression *clone = ast_pool -> GenClassInstanceCreationExpression();

    clone -> base_opt = (AstExpression *) (this -> base_opt ? this -> base_opt -> Clone(ast_pool) : NULL);
    clone -> dot_token_opt = this -> dot_token_opt;
    clone -> new_token = this -> new_token;
    clone -> class_type = (AstTypeExpression *) this -> class_type -> Clone(ast_pool);
    clone -> left_parenthesis_token = this -> left_parenthesis_token;
    clone -> AllocateArguments(this -> NumArguments());
    for (int i = 0; i < this -> NumArguments(); i++)
        clone -> AddArgument((AstExpression *) this -> Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = this -> right_parenthesis_token;
    clone -> class_body_opt = (AstClassBody *) (this -> class_body_opt ? this -> class_body_opt -> Clone(ast_pool) : NULL);
    clone -> AllocateLocalArguments(this -> NumLocalArguments());
    for (int k = 0; k < this -> NumLocalArguments(); k++)
        clone -> AddLocalArgument((AstExpression *) this -> LocalArgument(k) -> Clone(ast_pool));

    return clone;
}

Ast *AstDimExpr::Clone(StoragePool *ast_pool)
{
    AstDimExpr *clone = ast_pool -> GenDimExpr();

    clone -> left_bracket_token = this -> left_bracket_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> right_bracket_token = this -> right_bracket_token;

    return clone;
}

Ast *AstArrayCreationExpression::Clone(StoragePool *ast_pool)
{
    AstArrayCreationExpression *clone = ast_pool -> GenArrayCreationExpression();

    clone -> new_token = this -> new_token;
    clone -> array_type = this -> array_type -> Clone(ast_pool);
    clone -> AllocateDimExprs(this -> NumDimExprs());
    for (int i = 0; i < this -> NumDimExprs(); i++)
        clone -> AddDimExpr((AstDimExpr *) this -> DimExpr(i) -> Clone(ast_pool));
    clone -> AllocateBrackets(this -> NumBrackets());
    for (int k = 0; k < this -> NumBrackets(); k++)
        clone -> AddBrackets((AstBrackets *) this -> Brackets(k) -> Clone(ast_pool));
    clone -> array_initializer_opt = (AstArrayInitializer *)
                                     (this -> array_initializer_opt ? this -> array_initializer_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstFieldAccess::Clone(StoragePool *ast_pool)
{
    AstFieldAccess *clone = ast_pool -> GenFieldAccess(this -> field_access_tag);

    clone -> base = (AstExpression *) this -> base -> Clone(ast_pool);
    clone -> dot_token = this -> dot_token;
    clone -> identifier_token = this -> identifier_token;
    clone -> resolution_opt = (AstExpression *) (this -> resolution_opt ? this -> resolution_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstMethodInvocation::Clone(StoragePool *ast_pool)
{
    AstMethodInvocation *clone = ast_pool -> GenMethodInvocation();

    clone -> method = (AstExpression *) this -> method -> Clone(ast_pool);
    clone -> left_parenthesis_token = this -> left_parenthesis_token;
    clone -> AllocateArguments(this -> NumArguments());
    for (int i = 0; i < this -> NumArguments(); i++)
        clone -> AddArgument((AstExpression *) this -> Argument(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token = this -> right_parenthesis_token;
    clone -> resolution_opt = (AstExpression *) (this -> resolution_opt ? this -> resolution_opt -> Clone(ast_pool) : NULL);

    return clone;
}

Ast *AstArrayAccess::Clone(StoragePool *ast_pool)
{
    AstArrayAccess *clone = ast_pool -> GenArrayAccess();

    clone -> base = (AstExpression *) this -> base -> Clone(ast_pool);
    clone -> left_bracket_token = this -> left_bracket_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> right_bracket_token = this -> right_bracket_token;

    return clone;
}

Ast *AstPostUnaryExpression::Clone(StoragePool *ast_pool)
{
    AstPostUnaryExpression *clone = ast_pool -> GenPostUnaryExpression(this -> post_unary_tag);

    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);
    clone -> post_operator_token = this -> post_operator_token;

    return clone;
}

Ast *AstPreUnaryExpression::Clone(StoragePool *ast_pool)
{
    AstPreUnaryExpression *clone = ast_pool -> GenPreUnaryExpression(this -> pre_unary_tag);

    clone -> pre_operator_token = this -> pre_operator_token;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);

    return clone;
}

Ast *AstCastExpression::Clone(StoragePool *ast_pool)
{
    AstCastExpression *clone = ast_pool -> GenCastExpression();

    clone -> left_parenthesis_token_opt = this -> left_parenthesis_token_opt;
    clone -> type_opt = (Ast *) (this -> type_opt ? this -> type_opt -> Clone(ast_pool) : NULL);
    clone -> AllocateBrackets(this -> NumBrackets());
    for (int i = 0; i < this -> NumBrackets(); i++)
        clone -> AddBrackets((AstBrackets *) this -> Brackets(i) -> Clone(ast_pool));
    clone -> right_parenthesis_token_opt = this -> right_parenthesis_token_opt;
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);

    return clone;
}

Ast *AstBinaryExpression::Clone(StoragePool *ast_pool)
{
    AstBinaryExpression *clone = ast_pool -> GenBinaryExpression(this -> binary_tag);

    clone -> left_expression = (AstExpression *) this -> left_expression -> Clone(ast_pool);
    clone -> binary_operator_token = this -> binary_operator_token;
    clone -> right_expression = (AstExpression *) this -> right_expression -> Clone(ast_pool);

    return clone;
}

Ast *AstConditionalExpression::Clone(StoragePool *ast_pool)
{
    AstConditionalExpression *clone = ast_pool -> GenConditionalExpression();

    clone -> test_expression = (AstExpression *) this -> test_expression -> Clone(ast_pool);
    clone -> question_token = this -> question_token;
    clone -> true_expression = (AstExpression *) this -> true_expression -> Clone(ast_pool);
    clone -> colon_token = this -> colon_token;
    clone -> false_expression = (AstExpression *) this -> false_expression -> Clone(ast_pool);

    return clone;
}

Ast *AstAssignmentExpression::Clone(StoragePool *ast_pool)
{
    AstAssignmentExpression *clone = ast_pool -> GenAssignmentExpression(this -> assignment_tag, this -> assignment_operator_token);

    clone -> left_hand_side = (AstExpression *) this -> left_hand_side -> Clone(ast_pool);
    clone -> expression = (AstExpression *) this -> expression -> Clone(ast_pool);

    return clone;
}

#ifdef TEST
    void Ast::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (Ast):  "
                << "Node number " << (int) kind << " does not contain a print routine\n";
    }

    void AstBlock::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (";
        for (int i = 0; i < this -> NumLabels(); i++)
        {
             Coutput << lex_stream.NameString(this -> Label(i))
                     << ": ";
        }
        Coutput << "Block at level " << nesting_level;
        if (block_symbol)
             Coutput << ", max_variable_index " << block_symbol -> max_variable_index
                     << ", try_or_synchronized_variable_index " << block_symbol -> try_or_synchronized_variable_index;
        else Coutput << ", BLOCK_SYMBOL NOT SET";
        Coutput << ")";

        if (NumStatements() > 0)
        {
            Coutput << "    {";
            for (int j = 0; j < this -> NumStatements(); j++)
            {
                if (j % 10 == 0)
                    Coutput << "\n        ";
                Coutput << " #" << this -> Statement(j) -> id;
            }
            Coutput << "    }\n";
            for (int k = 0; k < this -> NumStatements(); k++)
                this -> Statement(k) -> Print(lex_stream);
        }
        else Coutput <<"\n";
    }

    void AstPrimitiveType::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (PrimitiveType):  "
                << lex_stream.NameString(primitive_kind_token)
                << "\n";
    }

    void AstArrayType::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ArrayType):  "
                << "#" << type -> id;
        for (int i = 0; i < this -> NumBrackets(); i++)
             Coutput << " []";
        Coutput << "\n";
        type -> Print(lex_stream);
    }

    void AstSimpleName::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (SimpleName):  "
                << lex_stream.NameString(identifier_token)
                << "\n";
    }

    void AstPackageDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (PackageDeclaration):  "
                << lex_stream.NameString(package_token)
                << " #" << name -> id << "\n";
        name -> Print(lex_stream);
    }

    void AstImportDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ImportDeclaration):  "
                << lex_stream.NameString(import_token)
                << " #" << name -> id << (star_token_opt ? "." : "")
                << (star_token_opt ? lex_stream.NameString(star_token_opt) : L"")
                << "\n";
        name -> Print(lex_stream);
    }

    void AstCompilationUnit::Print(LexStream& lex_stream)
    {
        Coutput << "\nAST structure for "
                << lex_stream.FileName()
                << ":\n\n"
                << "#" << this -> id << " (CompilationUnit):  "
                << "#" << (package_declaration_opt ? package_declaration_opt -> id : 0)
                << " (";
        for (int i = 0; i < this -> NumImportDeclarations(); i++)
            Coutput << " #" << this -> ImportDeclaration(i) -> id;
        Coutput << " ) (";
        for (int k = 0; k < this -> NumTypeDeclarations(); k++)
            Coutput << " #" << this -> TypeDeclaration(k) -> id;
        Coutput << ")\n";

        if (package_declaration_opt)
            package_declaration_opt -> Print(lex_stream);
        for (int m = 0; m < this -> NumImportDeclarations(); m++)
            this -> ImportDeclaration(m) -> Print(lex_stream);
        for (int n = 0; n < this -> NumTypeDeclarations(); n++)
            this -> TypeDeclaration(n) -> Print(lex_stream);
    }

    void AstModifier::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (Modifier):  "
                << lex_stream.NameString(modifier_kind_token)
                << "\n";
    }

    void AstEmptyDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (EmptyDeclaration):  "
                << lex_stream.NameString(semicolon_token)
                << "\n";
    }

    void AstClassBody::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ClassBody):  "
                << "\n    {";
        for (int i = 0; i < this -> NumClassBodyDeclarations(); i++)
        {
            if (i % 10 == 0)
                 Coutput << "\n       ";
            Coutput << " #" << this -> ClassBodyDeclaration(i) -> id;
        }
        Coutput << "\n    }\n";

        for (int k = 0; k < this -> NumClassBodyDeclarations(); k++)
            this -> ClassBodyDeclaration(k) -> Print(lex_stream);
    }

    void AstClassDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ClassDeclaration):  ";
        for (int i = 0; i < this -> NumClassModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> ClassModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << lex_stream.NameString(class_token)
                << " "
                << lex_stream.NameString(identifier_token)
                << " #" << (super_opt ? super_opt -> id : 0)
                << "(";
        for (int j = 0; j < NumInterfaces(); j++)
            Coutput << " #" << this -> Interface(j) -> id;
        Coutput << ") #" << class_body -> id << "\n";

        if (super_opt)
            super_opt -> Print(lex_stream);
        for (int k = 0; k < NumInterfaces(); k++)
            this -> Interface(k) -> Print(lex_stream);
        class_body -> Print(lex_stream);
    }

    void AstArrayInitializer::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ArrayInitializer):  "
                << "\n    {";
        for (int i = 0; i < NumVariableInitializers(); i++)
        {
            if (i % 10 == 0)
                 Coutput << "\n       ";
            Coutput << " #" << this -> VariableInitializer(i) -> id;
        }
        Coutput << "\n    }\n";

        for (int k = 0; k < NumVariableInitializers(); k++)
            this -> VariableInitializer(k) -> Print(lex_stream);
    }

    void AstBrackets::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (Brackets):  []" << "\n";
    }

    void AstVariableDeclaratorId::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (VariableDeclaratorId):  "
                << lex_stream.NameString(identifier_token);
        for (int i = 0; i < NumBrackets(); i++)
             Coutput << " []";
        Coutput << "\n";
    }

    void AstVariableDeclarator::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (VariableDeclarator):  " << "#" << variable_declarator_name -> id << " #" <<
                   (variable_initializer_opt ? variable_initializer_opt -> id : 0) << "\n";
        variable_declarator_name -> Print(lex_stream);
        if (variable_initializer_opt)
            variable_initializer_opt -> Print(lex_stream);

    }

    void AstFieldDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (FieldDeclaration):  ";
        for (int i = 0; i < this -> NumVariableModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> VariableModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << " #" << type -> id
                << "(";
        for (int j = 0; j < this -> NumVariableDeclarators(); j++)
            Coutput << " #" << this -> VariableDeclarator(j) -> id;
        Coutput << ") \n";

        type -> Print(lex_stream);
        for (int k = 0; k < this -> NumVariableDeclarators(); k++)
            this -> VariableDeclarator(k) -> Print(lex_stream);
    }

    void AstFormalParameter::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (FormalParameter):  ";
        for (int i = 0; i < this -> NumParameterModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> ParameterModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << "#" << type -> id
                << " #" << formal_declarator -> id << "\n";
        type -> Print(lex_stream);
        formal_declarator -> Print(lex_stream);
    }

    void AstMethodDeclarator::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (MethodDeclarator):  "
                << lex_stream.NameString(identifier_token)
                << " (";
        for (int k = 0; k < this -> NumFormalParameters(); k++)
            Coutput << " #" << this -> FormalParameter(k) -> id;
        Coutput << " )";
        for (int i = 0; i < NumBrackets(); i++)
             Coutput << " []";
        Coutput <<  "\n";

        for (int j = 0; j < this -> NumFormalParameters(); j++)
            this -> FormalParameter(j) -> Print(lex_stream);
    }

    void AstMethodDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (MethodDeclaration):  ";
        for (int i = 0; i < this -> NumMethodModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> MethodModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << " #" << type -> id
                << " #" << method_declarator -> id
                << " throws: (";
        for (int j = 0; j < this -> NumThrows(); j++)
            Coutput << " #" << this -> Throw(j) -> id;
        Coutput << ") #" << method_body -> id << "\n";

        type -> Print(lex_stream);
        method_declarator -> Print(lex_stream);
        for (int k = 0; k < this -> NumThrows(); k++)
            this -> Throw(k) -> Print(lex_stream);
        method_body -> Print(lex_stream);
    }

    void AstStaticInitializer::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (StaticInitializer):  "
                << lex_stream.NameString(static_token)
                << " #" << block -> id << "\n";
        block -> Print(lex_stream);
    }

    void AstThisCall::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ThisCall):  ";
        if (base_opt)
        {
            Coutput << "#" << base_opt -> id
                    << lex_stream.NameString(dot_token_opt);
        }
        Coutput << lex_stream.NameString(this_token)
                << " (";
        for (int i = 0; i < this -> NumArguments(); i++)
            Coutput << " #" << this -> Argument(i) -> id;
        Coutput << " ) \n";

        if (base_opt)
            base_opt -> Print(lex_stream);

        for (int j = 0; j < NumArguments(); j++)
            this -> Argument(j) -> Print(lex_stream);
    }

    void AstSuperCall::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (SuperCall):  ";
        if (base_opt)
        {
            Coutput << "#" << base_opt -> id
                    << lex_stream.NameString(dot_token_opt);
        }
        Coutput << lex_stream.NameString(super_token)
                << " (";
        for (int i = 0; i < this -> NumArguments(); i++)
            Coutput << " #" << this -> Argument(i) -> id;
        Coutput << " ) \n";

        if (base_opt)
            base_opt -> Print(lex_stream);

        for (int j = 0; j < NumArguments(); j++)
            this -> Argument(j) -> Print(lex_stream);
    }

    void AstConstructorBlock::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ConstructorBlock):  ";
        if (explicit_constructor_invocation_opt)
             Coutput << " #" << explicit_constructor_invocation_opt -> id;
        else Coutput << " #0";
        Coutput << " #" << block -> id
                << "\n";

        if (explicit_constructor_invocation_opt)
            explicit_constructor_invocation_opt -> Print(lex_stream);
        block -> Print(lex_stream);
    }

    void AstConstructorDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ConstructorDeclaration):  ";
        for (int i = 0; i < this -> NumConstructorModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> ConstructorModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << " #" << constructor_declarator -> id
                << " throws: (";
        for (int j = 0; j < this -> NumThrows(); j++)
            Coutput << " #" << this -> Throw(j) -> id;
        Coutput << ") #" << constructor_body -> id
                << "\n";

        constructor_declarator -> Print(lex_stream);
        for (int k = 0; k < this -> NumThrows(); k++)
            this -> Throw(k) -> Print(lex_stream);
        constructor_body -> Print(lex_stream);
    }

    void AstInterfaceDeclaration::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (InterfaceDeclaration):  ";
        for (int i = 0; i < this -> NumInterfaceModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> InterfaceModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << lex_stream.NameString(interface_token)
                << " "
                << lex_stream.NameString(identifier_token)
                << "(";
        for (int j = 0; j < NumExtendsInterfaces(); j++)
            Coutput << " #" << this -> ExtendsInterface(j) -> id;
        Coutput << ") {";
        for (int m = 0; m < NumInterfaceMemberDeclarations(); m++)
            Coutput << " #" << this -> InterfaceMemberDeclaration(m) -> id;
        Coutput << "}\n";

        for (int k = 0; k < NumExtendsInterfaces(); k++)
            this -> ExtendsInterface(k) -> Print(lex_stream);
        for (int n = 0; n < NumInterfaceMemberDeclarations(); n++)
            this -> InterfaceMemberDeclaration(n) -> Print(lex_stream);
    }

    void AstLocalVariableDeclarationStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (LocalVariableDeclarationStatement):  ";
        for (int i = 0; i < this -> NumLocalModifiers(); i++)
        {
            Coutput << lex_stream.NameString(this -> LocalModifier(i) -> modifier_kind_token)
                    << " ";
        }
        Coutput << "#" << type -> id
                << "(";
        for (int j = 0; j < this -> NumVariableDeclarators(); j++)
            Coutput << " #" << this -> VariableDeclarator(j) -> id;
        Coutput << ") \n";

        type -> Print(lex_stream);
        for (int k = 0; k < this -> NumVariableDeclarators(); k++)
            this -> VariableDeclarator(k) -> Print(lex_stream);
    }

    void AstIfStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (IfStatement):  "
                << lex_stream.NameString(if_token)
                << " ( #" << expression -> id << " ) #" << true_statement -> id;
        if (false_statement_opt)
             Coutput << " else #" << false_statement_opt -> id;
        else Coutput << " #0";
        Coutput << "\n";

        expression -> Print(lex_stream);
        true_statement -> Print(lex_stream);
        if (false_statement_opt)
            false_statement_opt -> Print(lex_stream);
    }

    void AstEmptyStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (EmptyStatement):  "
                << lex_stream.NameString(semicolon_token)
                << "\n";
    }

    void AstExpressionStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ExpressionStatement):  " << "#" << expression -> id << "\n";
        expression -> Print(lex_stream);
    }

    void AstCaseLabel::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (CaseLabel):  "
                << lex_stream.NameString(case_token)
                << " #" << expression -> id << ":\n";
        expression -> Print(lex_stream);
        Coutput << "    map_index: " << map_index << "\n";
    }

    void AstDefaultLabel::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (DefaultLabel):  "
                << lex_stream.NameString(default_token)
                << ":\n";
    }

    void AstSwitchBlockStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (SwitchBlockStatement): ";
        for (int i = 0; i < NumSwitchLabels(); i++)
        {
            if (i % 10 == 0)
                 Coutput << "\n        ";
            Coutput << " #" << this -> SwitchLabel(i) -> id << ':';
        }
        Coutput << "\n";
        for (int k = 0; k < NumStatements(); k++)
        {
            if (k % 10 == 0)
                 Coutput << "\n            ";
            Coutput << " #" << this -> Statement(k) -> id;
        }
        Coutput << "\n";

        for (int j = 0; j < NumSwitchLabels(); j++)
            this -> SwitchLabel(j) -> Print(lex_stream);
        for (int l = 0; l < NumStatements(); l++)
            this -> Statement(l) -> Print(lex_stream);
    }

    void AstSwitchStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (SwitchStatement):  "
                << lex_stream.NameString(switch_token)
                << " ( #" << expression -> id << " ) #" << switch_block -> id << "\n";

        Coutput << "default case: index " << default_case.index << "\n";
        for (int i = 0; i < cases -> Length(); i++)
        {
            Coutput << "case: " << i << "  index: " << (*cases)[i] -> index << "  value: " << (*cases)[i] -> Value() << "\n";
        }

        expression -> Print(lex_stream);
        switch_block -> Print(lex_stream);
    }

    void AstWhileStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (WhileStatement):  "
                << lex_stream.NameString(while_token)
                << " ( #" << expression -> id << " ) #" << statement -> id << "\n";
        expression -> Print(lex_stream);
        statement -> Print(lex_stream);
    }

    void AstDoStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (DoStatement):  "
                << lex_stream.NameString(do_token)
                << " { #" << statement -> id << " } "
                << lex_stream.NameString(while_token)
                << " ( #" << expression -> id << " ) #\n";

        statement -> Print(lex_stream);
        expression -> Print(lex_stream);
    }

    void AstForStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ForStatement):  ("
                << lex_stream.NameString(for_token);
        for (int i = 0; i < this -> NumForInitStatements(); i++)
            Coutput << " #" << this -> ForInitStatement(i) -> id;
        Coutput << "; #" << (end_expression_opt ? end_expression_opt -> id : 0) << ";";
        for (int k = 0; k < this -> NumForInitStatements(); k++)
            Coutput << " #" << this -> ForUpdateStatement(k) -> id;
        Coutput << ") #" << statement -> id << "\n";

        for (int m = 0; m < this -> NumForInitStatements(); m++)
            this -> ForInitStatement(m) -> Print(lex_stream);
        if (end_expression_opt)
            end_expression_opt -> Print(lex_stream);
        for (int n = 0; n < this -> NumForUpdateStatements(); n++)
            this -> ForUpdateStatement(n) -> Print(lex_stream);
        statement -> Print(lex_stream);
    }

    void AstBreakStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (BreakStatement):  "
                << lex_stream.NameString(break_token)
                << " "
                << (identifier_token_opt ? lex_stream.NameString(identifier_token_opt) : L"")
                << " at nesting_level " << nesting_level << "\n";
    }

    void AstContinueStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ContinueStatement):  "
                << lex_stream.NameString(continue_token)
                << " "
                << (identifier_token_opt ? lex_stream.NameString(identifier_token_opt) : L"")
                << " at nesting_level " << nesting_level << "\n";
    }

    void AstReturnStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ReturnStatement):  "
                << lex_stream.NameString(return_token)
                << " "
                << " #" << (expression_opt ? expression_opt -> id : 0) << "\n";
        if (expression_opt)
            expression_opt -> Print(lex_stream);
    }

    void AstThrowStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ThrowStatement):  "
                << lex_stream.NameString(throw_token)
                << " "
                << " #" << expression -> id << "\n";
        expression -> Print(lex_stream);
    }

    void AstSynchronizedStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (SynchronizedStatement):  "
                << lex_stream.NameString(synchronized_token)
                << " ( #" << expression -> id
                << " ) #" << block -> id << "\n";
        expression -> Print(lex_stream);
        block -> Print(lex_stream);
    }

    void AstCatchClause::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (CatchClause):  "
                << lex_stream.NameString(catch_token)
                << " #" << formal_parameter -> id
                << " #" << block -> id << "\n";
        formal_parameter -> Print(lex_stream);
        block -> Print(lex_stream);
    }

    void AstFinallyClause::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (FinallyClause):  "
                << lex_stream.NameString(finally_token)
                << " #" << block -> id << "\n";
        block -> Print(lex_stream);
    }

    void AstTryStatement::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (TryStatement):  "
                << lex_stream.NameString(try_token)
                << " #" << block -> id
                << " catch (";
        for (int i = 0; i < this -> NumCatchClauses(); i++)
            Coutput << " #" << this -> CatchClause(i) -> id;
        Coutput << ") finally " << "#" << (finally_clause_opt ? finally_clause_opt -> id : 0) << "\n";

        block -> Print(lex_stream);
        for (int k = 0; k < this -> NumCatchClauses(); k++)
            this -> CatchClause(k) -> Print(lex_stream);
        if (finally_clause_opt)
            finally_clause_opt -> Print(lex_stream);
    }

    void AstIntegerLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (IntegerLiteral):  "
                << lex_stream.NameString(integer_literal_token)
                << "\n";
    }

    void AstLongLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (LongLiteral):  "
                << lex_stream.NameString(long_literal_token)
                << "\n";
    }

    void AstFloatingPointLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (FloatingPointLiteral):  "
                << lex_stream.NameString(floating_point_literal_token)
                << "\n";
    }

    void AstDoubleLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (DoubleLiteral):  "
                << lex_stream.NameString(double_literal_token)
                << "\n";
    }

    void AstTrueLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (TrueLiteral):  "
                << lex_stream.NameString(true_literal_token)
                << "\n";
    }

    void AstFalseLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (FalseLiteral):  "
                << lex_stream.NameString(false_literal_token)
                << "\n";
    }

    void AstStringLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (StringLiteral):  "
                << lex_stream.NameString(string_literal_token)
                << "\n";
    }

    void AstCharacterLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (CharacterLiteral):  "
                << lex_stream.NameString(character_literal_token)
                << "\n";
    }

    void AstNullLiteral::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (NullLiteral):  "
                << lex_stream.NameString(null_token)
                << "\n";
    }

    void AstThisExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ThisExpression):  "
                << lex_stream.NameString(this_token)
                << "\n";
    }

    void AstSuperExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (SuperExpression):  "
                << lex_stream.NameString(super_token)
                << "\n";
    }

    void AstParenthesizedExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ParenthesizedExpression):  "
                << lex_stream.NameString(left_parenthesis_token)
                << "#" << expression -> id
                << lex_stream.NameString(right_parenthesis_token)
                << "\n";
        expression -> Print(lex_stream);
    }

    void AstTypeExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (TypeExpression):  "
                << " #" << type -> id << "\n";
        type -> Print(lex_stream);
    }

    void AstClassInstanceCreationExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ClassInstanceCreationExpression):  ";
        if (base_opt)
        {
            Coutput << "#" << base_opt -> id
                    << lex_stream.NameString(dot_token_opt) << " ";
        }
        Coutput << lex_stream.NameString(new_token)
                << " #" << class_type -> id
                << " (";
        for (int i = 0; i < this -> NumArguments(); i++)
            Coutput << " #" << this -> Argument(i) -> id;
        Coutput << " ) "
                << "#" << (class_body_opt ? class_body_opt -> id : 0) << "\n";

        if (base_opt)
            base_opt -> Print(lex_stream);

        class_type -> Print(lex_stream);
        for (int j = 0; j < NumArguments(); j++)
            this -> Argument(j) -> Print(lex_stream);

        if (class_body_opt)
            class_body_opt -> Print(lex_stream);
    }

    void AstDimExpr::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (DimExpr):  [ #" << expression -> id << " ]\n";
        expression -> Print(lex_stream);
    }

    void AstArrayCreationExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ArrayCreationExpression):  "
                << lex_stream.NameString(new_token)
                << " #" << array_type -> id;
        for (int i = 0; i < NumDimExprs(); i++)
            Coutput << " [#" << DimExpr(i) -> id << "]";
        for (int k = 0; k < NumBrackets(); k++)
             Coutput << " []";
        Coutput << " "
                << "#" << (array_initializer_opt ? array_initializer_opt -> id : 0) << "\n";

        array_type -> Print(lex_stream);
        for (int j = 0; j < this -> NumDimExprs(); j++)
            DimExpr(j) -> Print(lex_stream);
    }

    void AstFieldAccess::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (FieldAccess):  "
                << " #" << base -> id << " "
                << lex_stream.NameString(identifier_token)
                << "\n";

        base -> Print(lex_stream);
    }

    void AstMethodInvocation::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (MethodInvocation):  "
                << "#" << method -> id
                << " (";
        for (int i = 0; i < this -> NumArguments(); i++)
            Coutput << " #" << this -> Argument(i) -> id;
        Coutput << " ) \n";

        method -> Print(lex_stream);
        for (int j = 0; j < NumArguments(); j++)
            this -> Argument(j) -> Print(lex_stream);
    }

    void AstArrayAccess::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ArrayAccess):  "
                << "#" << base -> id
                << " [ #" << expression -> id << " ]\n";

        base -> Print(lex_stream);
        expression -> Print(lex_stream);
    }

    void AstPostUnaryExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (PostUnaryExpression):  "
                << "#" << expression -> id
                << lex_stream.NameString(post_operator_token)
                << "\n";

        expression -> Print(lex_stream);
    }

    void AstPreUnaryExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (PreUnaryExpression):  "
                << lex_stream.NameString(pre_operator_token)
                << " #" << expression -> id << "\n";

        expression -> Print(lex_stream);
    }

    void AstCastExpression::Print(LexStream& lex_stream)
    {
        if (left_parenthesis_token_opt)
        {
          Coutput << "#" << this -> id << (kind == CAST ? " (CastExpression: just cast):  " : " (CastExpression: check and cast):  ")
                  << "( #" << (type_opt ? type_opt -> id : 0);
            for (int i = 0; i < NumBrackets(); i++)
                 Coutput << " []";
            Coutput << " ) #" << expression -> id << "\n";
            if (type_opt)
                type_opt -> Print(lex_stream);
        }
        else
        {
            Coutput << "#" << this -> id << " (Java Semantic Cast to " << Type() -> Name()
                    << "):  #" << expression -> id << "\n";
        }

        expression -> Print(lex_stream);
    }

    void AstBinaryExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (BinaryExpression):  "
                << "#" << left_expression -> id << " "
                << lex_stream.NameString(binary_operator_token)
                << " #" << right_expression -> id << "\n";

        left_expression -> Print(lex_stream);
        right_expression -> Print(lex_stream);
    }

    void AstConditionalExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (ConditionalExpression):  "
                << "#" << test_expression -> id
                << " ? #" << true_expression -> id
                << " : #" << false_expression -> id << "\n";

        test_expression -> Print(lex_stream);
        true_expression -> Print(lex_stream);
        false_expression -> Print(lex_stream);
    }

    void AstAssignmentExpression::Print(LexStream& lex_stream)
    {
        Coutput << "#" << this -> id << " (AssignmentExpression):  "
                << "#" << left_hand_side -> id << " "
                << lex_stream.NameString(assignment_operator_token)
                << " #" << expression -> id << "\n";

        left_hand_side -> Print(lex_stream);
        expression -> Print(lex_stream);
    }

#endif

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
    //    delete block_statements;
}

AstPrimitiveType::~AstPrimitiveType()
{
    assert(false);
}

AstArrayType::~AstArrayType()
{
    assert(false);
    //    delete type;
    //    delete brackets;
}

AstSimpleName::~AstSimpleName()
{
    assert(false);
    //    delete resolution_opt;
}

AstPackageDeclaration::~AstPackageDeclaration()
{
    assert(false);
    //    delete name;
}

AstImportDeclaration::~AstImportDeclaration()
{
    assert(false);
    //    delete name;
}

AstCompilationUnit::~AstCompilationUnit()
{
    assert(false);
    //    delete package_declaration_opt;
    //    delete import_declarations;
    //    delete type_declarations;
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
    //    delete default_constructor;
    //    delete instance_variables;
    //    delete class_variables;
    //    delete methods;
    //    delete constructors;
    //    delete static_initializers;
    //    delete inner_classes;
    //    delete inner_interfaces;
    //    delete blocks;
    //    delete class_body_declarations;
    //    delete this_block;
}

AstClassDeclaration::~AstClassDeclaration()
{
    assert(false);
    //    delete class_modifiers;
    //    delete super_opt;
    //    delete interfaces;
    //    delete class_body;
}

AstArrayInitializer::~AstArrayInitializer()
{
    assert(false);
    //    delete variable_initializers;
}

AstBrackets::~AstBrackets()
{
    assert(false);
}

AstVariableDeclaratorId::~AstVariableDeclaratorId()
{
    assert(false);
    //    delete brackets;
}

AstVariableDeclarator::~AstVariableDeclarator()
{
    assert(false);
    //    delete variable_declarator_name;
    //    delete variable_initializer_opt;
}

AstFieldDeclaration::~AstFieldDeclaration()
{
    assert(false);
    //    delete variable_modifiers;
    //    delete type;
    //    delete variable_declarators;
}

AstFormalParameter::~AstFormalParameter()
{
    assert(false);
    //    delete parameter_modifiers;
    //    delete type;
    //    delete variable_declarator_name;
}

AstMethodDeclarator::~AstMethodDeclarator()
{
    assert(false);
    //    delete formal_parameters;
    //    delete brackets;
}

AstMethodDeclaration::~AstMethodDeclaration()
{
    assert(false);
    //    delete method_modifiers;
    //    delete type;
    //    delete method_declarator;
    //    delete throws;
    //    delete method_body;
}

AstStaticInitializer::~AstStaticInitializer()
{
    assert(false);
    //    delete block;
}

AstThisCall::~AstThisCall()
{
    assert(false);
    //    delete arguments;
    //    delete base_opt;
    //    delete local_arguments_opt;
}

AstSuperCall::~AstSuperCall()
{
    assert(false);
    //    delete base_opt;
    //    delete arguments;
    //    delete local_arguments_opt;
}

AstConstructorBlock::~AstConstructorBlock()
{
    assert(false);
    //    delete explicit_constructor_invocation_opt;
    //    delete block;
    //    delete local_init_block;
    //    delete original_constructor_invocation;
}

AstConstructorDeclaration::~AstConstructorDeclaration()
{
    assert(false);
    //    delete constructor_modifiers;
    //    delete constructor_declarator;
    //    delete throws;
    //    delete constructor_body;
}

AstInterfaceDeclaration::~AstInterfaceDeclaration()
{
    assert(false);
    //    delete class_variables;
    //    delete abstract_methods;
    //    delete inner_classes;
    //    delete inner_interfaces;
    //    delete interface_modifiers;
    //    delete extends_interfaces;
    //    delete interface_member_declarations;
}

AstLocalVariableDeclarationStatement::~AstLocalVariableDeclarationStatement()
{
    assert(false);
    //    delete local_modifiers;
    //    delete type;
    //    delete variable_declarators;
}

AstIfStatement::~AstIfStatement()
{
    assert(false);
    //    delete expression;
    //    delete true_statement;
    //    delete false_statement_opt;
}

AstEmptyStatement::~AstEmptyStatement()
{
    assert(false);
}

AstExpressionStatement::~AstExpressionStatement()
{
    assert(false);
    //    delete expression;
}

AstCaseLabel::~AstCaseLabel()
{
    assert(false);
    //    delete expression;
}

AstDefaultLabel::~AstDefaultLabel()
{
    assert(false);
}

AstSwitchBlockStatement::~AstSwitchBlockStatement()
{
    assert(false);
    //    delete switch_labels;
    //    delete block_statements;
}

AstSwitchStatement::~AstSwitchStatement()
{
    assert(false);
    //    delete expression;
    //    delete switch_block;
}

AstWhileStatement::~AstWhileStatement()
{
    assert(false);
    //    delete expression;
    //    delete statement;
}

AstDoStatement::~AstDoStatement()
{
    assert(false);
    //    delete statement;
    //    delete expression;
}

AstForStatement::~AstForStatement()
{
    assert(false);
    //    delete for_init_statements;
    //    delete end_expression_opt;
    //    delete for_update_statements;
    //    delete statement;
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
    //    delete expression_opt;
}

AstThrowStatement::~AstThrowStatement()
{
    assert(false);
    //    delete expression;
}

AstSynchronizedStatement::~AstSynchronizedStatement()
{
    assert(false);
    //    delete expression;
    //    delete block;
}

AstCatchClause::~AstCatchClause()
{
    assert(false);
    //    delete formal_parameter;
    //    delete block;
}

AstFinallyClause::~AstFinallyClause()
{
    assert(false);
    //    delete block;
}

AstTryStatement::~AstTryStatement()
{
    assert(false);
    //    delete block;
    //    delete catch_clauses;
    //    delete finally_clause_opt;
}

AstIntegerLiteral::~AstIntegerLiteral()
{
    assert(false);
}

AstLongLiteral::~AstLongLiteral()
{
    assert(false);
}

AstFloatingPointLiteral::~AstFloatingPointLiteral()
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
    //    delete expression;
}

AstTypeExpression::~AstTypeExpression()
{
    assert(false);
    //    delete type;
}

AstClassInstanceCreationExpression::~AstClassInstanceCreationExpression()
{
    assert(false);
    //    delete base_opt;
    //    delete class_type;
    //    delete arguments;
    //    delete class_body_opt;
    //    delete local_arguments_opt;
}

AstDimExpr::~AstDimExpr()
{
    assert(false);
    //    delete expression;
}

AstArrayCreationExpression::~AstArrayCreationExpression()
{
    assert(false);
    //    delete array_type;
    //    delete dim_exprs;
    //    delete brackets;
    //    delete array_initializer_opt;
}

AstFieldAccess::~AstFieldAccess()
{
    assert(false);
    //    delete base;
    //    delete resolution_opt;
}

AstMethodInvocation::~AstMethodInvocation()
{
    assert(false);
    //    delete method;
    //    delete arguments;
    //    delete resolution_opt;
}

AstArrayAccess::~AstArrayAccess()
{
    assert(false);
    //    delete base;
    //    delete expression;
}

AstPostUnaryExpression::~AstPostUnaryExpression()
{
    assert(false);
    //    delete expression;
}

AstPreUnaryExpression::~AstPreUnaryExpression()
{
    assert(false);
    //    delete expression;
}

AstCastExpression::~AstCastExpression()
{
    assert(false);
    //    delete type_opt;
    //    delete brackets;
    //    delete expression;
}

AstBinaryExpression::~AstBinaryExpression()
{
   assert(false);
   //    delete left_expression;
   //    delete right_expression;
}

AstConditionalExpression::~AstConditionalExpression()
{
    assert(false);
    //    delete test_expression;
    //    delete true_expression;
    //    delete false_expression;
}

AstAssignmentExpression::~AstAssignmentExpression()
{
    assert(false);
    //    delete left_hand_side;
    //    delete expression;
}
