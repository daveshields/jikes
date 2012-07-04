// $Id: body.cpp,v 1.62 2002/07/01 17:15:59 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"
#include "semantic.h"
#include "control.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

void Semantic::ProcessBlockStatements(AstBlock *block_body)
{
    //
    // An empty block that is not a switch block can complete normally
    // iff it is reachable. A nonempty block that is not a switch
    // block can complete normally iff the last statement in it can
    // complete normally.
    //
    if (block_body -> NumStatements() == 0)
         block_body -> can_complete_normally = block_body -> is_reachable;
    else
    {
        //
        // The first statement in a nonempty block that is not a
        // switch block is reachable iff the block is reachable.
        // Every other statement S in a nonempty block that is not a
        // switch block is reachable iff the statement preceeding S
        // can complete normally.
        //
        AstStatement *statement = (AstStatement *) block_body -> Statement(0);
        statement -> is_reachable = block_body -> is_reachable;
        AstStatement *first_unreachable_statement =
            (AstStatement *) (statement -> is_reachable ? NULL : statement);
        ProcessStatement(statement);
        for (int i = 1; i < block_body -> NumStatements(); i++)
        {
            AstStatement *previous_statement = statement;
            statement = (AstStatement *) block_body -> Statement(i);
            statement -> is_reachable =
                previous_statement -> can_complete_normally;
            if (! statement -> is_reachable &&
                first_unreachable_statement == NULL)
            {
                first_unreachable_statement = statement;
            }
            ProcessStatement(statement);
        }

        if (statement -> can_complete_normally)
            block_body -> can_complete_normally = true;

        //
        // If we have one or more unreachable statements that are contained in
        // a reachable block then issue message. (If the enclosing block is
        // not reachable the message will be issued later for the enclosing
        // block.)
        //
        if (first_unreachable_statement &&
            LocalBlockStack().TopBlock() -> is_reachable)
        {
            if (first_unreachable_statement == statement)
            {
                ReportSemError(SemanticError::UNREACHABLE_STATEMENT,
                               statement -> LeftToken(),
                               statement -> RightToken());
            }
            else
            {
                ReportSemError(SemanticError::UNREACHABLE_STATEMENTS,
                               first_unreachable_statement -> LeftToken(),
                               statement -> RightToken());
            }
        }

        //
        // If an enclosed block has a higher max_variable_index than the
        // current block, update max_variable_index in the current_block,
        // accordingly.
        //
        BlockSymbol *block = block_body -> block_symbol;
        if (block -> max_variable_index <
            LocalBlockStack().TopMaxEnclosedVariableIndex())
        {
            block -> max_variable_index =
                LocalBlockStack().TopMaxEnclosedVariableIndex();
        }
    }
}


void Semantic::ProcessBlock(Ast *stmt)
{
    AstBlock *block_body = (AstBlock *) stmt;

    AstBlock *enclosing_block = LocalBlockStack().TopBlock();

    //
    // Guess that the number of elements will not exceed the number of
    // statements + 3. The +3 takes into account one label + one ForInit
    // declaration and one extra something else.
    //
    int table_size = block_body -> NumStatements() + 3;
    BlockSymbol *block =
        LocalSymbolTable().Top() -> InsertBlockSymbol(table_size);
    //
    // enclosing_block is not present only when we are processing the block
    // of a static initializer
    //
    block -> max_variable_index =
        enclosing_block
        ? enclosing_block -> block_symbol -> max_variable_index : 1;
    LocalSymbolTable().Push(block -> Table());

    block_body -> block_symbol = block;
    block_body -> nesting_level = LocalBlockStack().Size();
    LocalBlockStack().Push(block_body);

    //
    // Note that in constructing the Ast, the parser encloses each
    // labeled statement in its own block. Therefore the declaration
    // of this label will not conflict with the declaration of another
    // label with the same name declared at the same nesting level.
    //
    // For example, the following sequence of statements is legal:
    //
    //     l: a = b;
    //     l: b = c;
    //
    if (block_body -> label_opt != lex_stream -> BadToken())
    {
        NameSymbol *name_symbol =
            lex_stream -> NameSymbol(block_body -> label_opt);
        if (LocalSymbolTable().FindLabelSymbol(name_symbol))
        {
            ReportSemError(SemanticError::DUPLICATE_LABEL,
                           block_body -> label_opt,
                           block_body -> label_opt,
                           name_symbol -> Name());
        }
        else
        {
            LabelSymbol *label =
                LocalSymbolTable().Top() -> InsertLabelSymbol(name_symbol);
            label -> block = block_body;
            label -> nesting_level = block_body -> nesting_level;
        }
    }

    ProcessBlockStatements(block_body);

    LocalBlockStack().Pop();
    LocalSymbolTable().Pop();

    //
    // Update the information for the block that immediately encloses the
    // current block.
    //
    if (enclosing_block && (LocalBlockStack().TopMaxEnclosedVariableIndex() <
                            block -> max_variable_index))
    {
        LocalBlockStack().TopMaxEnclosedVariableIndex() =
            block -> max_variable_index;
    }

    block -> CompressSpace(); // space optimization
}


void Semantic::ProcessLocalVariableDeclarationStatement(Ast *stmt)
{
    AstLocalVariableDeclarationStatement *local_decl =
        (AstLocalVariableDeclarationStatement *) stmt;

    AstArrayType *array_type = local_decl -> type -> ArrayTypeCast();
    Ast *actual_type = (array_type ? array_type -> type : local_decl -> type);

    AccessFlags access_flags = ProcessLocalModifiers(local_decl);

    AstPrimitiveType *primitive_type = actual_type -> PrimitiveTypeCast();
    TypeSymbol *field_type = (primitive_type
                              ? FindPrimitiveType(primitive_type)
                              : MustFindType(actual_type));

    for (int i = 0; i < local_decl -> NumVariableDeclarators(); i++)
    {
        AstVariableDeclarator *variable_declarator =
            local_decl -> VariableDeclarator(i);
        AstVariableDeclaratorId *name =
            variable_declarator -> variable_declarator_name;
        NameSymbol *name_symbol =
            lex_stream -> NameSymbol(name -> identifier_token);

        //
        // According to JLS2 14.4.2, only check for a duplicate in
        // the local class scope; don't worry about enclosing classes
        //
        if (LocalSymbolTable().FindVariableSymbol(name_symbol))
        {
            ReportSemError(SemanticError::DUPLICATE_LOCAL_VARIABLE_DECLARATION,
                           name -> identifier_token,
                           name -> identifier_token,
                           name_symbol -> Name());
        }
        else
        {
            VariableSymbol *symbol =
                LocalSymbolTable().Top() -> InsertVariableSymbol(name_symbol);
            variable_declarator -> symbol = symbol;

            int num_dimensions = (array_type
                                  ? array_type -> NumBrackets() : 0) +
                name -> NumBrackets();
            if (num_dimensions == 0)
                 symbol -> SetType(field_type);
            else symbol -> SetType(field_type -> GetArrayType(this,
                                                              num_dimensions));
            symbol -> SetFlags(access_flags);
            symbol -> SetOwner(ThisMethod());
            symbol -> declarator = variable_declarator;
            BlockSymbol *block = LocalBlockStack().TopBlock() -> block_symbol;

            symbol -> SetLocalVariableIndex(block -> max_variable_index++);
            if (control.IsDoubleWordType(symbol -> Type()))
                block -> max_variable_index++;

            ProcessVariableInitializer(variable_declarator);
        }
    }

    //
    // A local variable declaration statement can complete normally
    // iff it is reachable.
    //
    local_decl -> can_complete_normally = local_decl -> is_reachable;
}


void Semantic::ProcessExpressionStatement(Ast *stmt)
{
    AstExpressionStatement *expression_statement =
        (AstExpressionStatement *) stmt;

    ProcessExpression(expression_statement -> expression);

    //
    // An expression statement can complete normally iff it is reachable.
    //
    expression_statement -> can_complete_normally =
        expression_statement -> is_reachable;
}


void Semantic::ProcessSynchronizedStatement(Ast *stmt)
{
    AstSynchronizedStatement *synchronized_statement =
        (AstSynchronizedStatement *) stmt;

    //
    // Notice that in the case of a complex string constant, it is
    // vital to inline correctly; otherwise, we would not be using
    // the correct object as the monitor.
    //
    ProcessExpressionOrStringConstant(synchronized_statement -> expression);

    synchronized_statement -> block -> is_reachable =
        synchronized_statement -> is_reachable;

    if (synchronized_statement -> expression -> Type() -> Primitive() ||
        synchronized_statement -> expression -> symbol == control.null_type)
    {
        ReportSemError(SemanticError::TYPE_NOT_REFERENCE,
                       synchronized_statement -> expression -> LeftToken(),
                       synchronized_statement -> expression -> RightToken(),
                       synchronized_statement -> expression -> Type() -> Name());
    }

    AstBlock *enclosing_block = LocalBlockStack().TopBlock(),
             *block_body = synchronized_statement -> block;

    //
    // Synchronized blocks require one special local variable slot for the
    // monitor. However, since a try-finally may require up to four slots, we
    // reserve them all at this time.  Otherwise, the sequence {synchronized;
    // variable declaration; try-finally} within the same enclosing block will
    // cause a VerifyError. The VM should not care if some of these special
    // slots are unused.
    //
    // TODO: Is it worth optimizing this and try-finally to avoid wasting
    // variable slots?
    //
    BlockSymbol *enclosing_block_symbol = enclosing_block -> block_symbol;
    // first such statement encountered in enclosing block?
    if (enclosing_block_symbol -> try_or_synchronized_variable_index == 0)
    {
        enclosing_block_symbol -> try_or_synchronized_variable_index =
            enclosing_block_symbol -> max_variable_index;
        enclosing_block_symbol -> max_variable_index += 2;
        if (ThisMethod() -> Type() != control.void_type)
        {
            if (control.IsDoubleWordType(ThisMethod() -> Type()))
                enclosing_block_symbol -> max_variable_index += 2;
            else enclosing_block_symbol -> max_variable_index += 1;
        }
    }

    //
    // Guess that the number of elements will not exceed the number of
    // statements + 3.
    //
    BlockSymbol *block = LocalSymbolTable().Top() ->
        InsertBlockSymbol(block_body -> NumStatements() + 3);
    block -> max_variable_index = enclosing_block_symbol -> max_variable_index;
    LocalSymbolTable().Push(block -> Table());

    block_body -> block_symbol = block;
    block_body -> nesting_level = LocalBlockStack().Size();
    LocalBlockStack().Push(block_body);

    ProcessBlockStatements(block_body);

    LocalBlockStack().Pop();
    LocalSymbolTable().Pop();

    if (LocalBlockStack().TopMaxEnclosedVariableIndex() <
        block -> max_variable_index)
    {
        LocalBlockStack().TopMaxEnclosedVariableIndex() =
            block -> max_variable_index;
    }

    synchronized_statement -> can_complete_normally =
        synchronized_statement -> block -> can_complete_normally;

    block -> CompressSpace(); // space optimization
}


void Semantic::ProcessIfStatement(Ast *stmt)
{
    AstIfStatement *if_statement = (AstIfStatement *) stmt;

    ProcessExpression(if_statement -> expression);

    if (if_statement -> expression -> Type() != control.no_type &&
        if_statement -> expression -> Type() != control.boolean_type)
    {
        ReportSemError(SemanticError::TYPE_NOT_BOOLEAN,
                       if_statement -> expression -> LeftToken(),
                       if_statement -> expression -> RightToken(),
                       if_statement -> expression -> Type() -> Name());
    }

    //
    // Recall that the parser ensures that the statements that appear in an
    // if-statement (both the true and false statement) are enclosed in a
    // block.
    //
    if_statement -> true_statement -> is_reachable =
        if_statement -> is_reachable;
    ProcessBlock(if_statement -> true_statement);

    if (if_statement -> false_statement_opt)
    {
        if_statement -> false_statement_opt -> is_reachable =
            if_statement -> is_reachable;
        ProcessBlock(if_statement -> false_statement_opt);

        if_statement -> can_complete_normally =
            if_statement -> true_statement -> can_complete_normally ||
            if_statement -> false_statement_opt -> can_complete_normally;
    }
    else if_statement -> can_complete_normally = if_statement -> is_reachable;
}


void Semantic::ProcessWhileStatement(Ast *stmt)
{
    AstWhileStatement *while_statement = (AstWhileStatement *) stmt;

    //
    // Recall that each while statement is enclosed in a unique block by the
    // parser
    //
    BreakableStatementStack().Push(LocalBlockStack().TopBlock());
    ContinuableStatementStack().Push(LocalBlockStack().TopBlock());

    AstStatement *enclosed_statement = while_statement -> statement;
    enclosed_statement -> is_reachable = while_statement -> is_reachable;

    ProcessExpression(while_statement -> expression);
    if (while_statement -> expression -> Type() == control.boolean_type)
    {
        if (IsConstantFalse(while_statement -> expression))
        {
            if (while_statement -> is_reachable)
                while_statement -> can_complete_normally = true;
            enclosed_statement -> is_reachable = false;
        }
        else if (! IsConstantTrue(while_statement -> expression) &&
                 while_statement -> is_reachable)
        {
            while_statement -> can_complete_normally = true;
        }
    }
    else if (while_statement -> expression -> Type() != control.no_type)
    {
        ReportSemError(SemanticError::TYPE_NOT_BOOLEAN,
                       while_statement -> expression -> LeftToken(),
                       while_statement -> expression -> RightToken(),
                       while_statement -> expression -> Type() -> Name());
    }

    ProcessStatement(enclosed_statement);

    if (! enclosed_statement -> is_reachable &&
        while_statement -> is_reachable)
    {
        ReportSemError(SemanticError::UNREACHABLE_STATEMENT,
                       enclosed_statement -> LeftToken(),
                       enclosed_statement -> RightToken());
    }

    //
    // If the while statement contained a reachable break statement,
    // then the while statement can complete normally. It is marked
    // here only for completeness, as marking the enclosing block is
    // enough to propagate the proper information upward.
    //
    AstBlock *block_body = (AstBlock *) BreakableStatementStack().Top();
    if (block_body -> can_complete_normally)
        while_statement -> can_complete_normally = true;

    BreakableStatementStack().Pop();
    ContinuableStatementStack().Pop();
}


void Semantic::ProcessForStatement(Ast *stmt)
{
    AstForStatement *for_statement = (AstForStatement *) stmt;

    //
    // Note that in constructing the Ast, the parser encloses each
    // for-statement whose for-init-statements starts with a local
    // variable declaration in its own block. Therefore a redeclaration
    // of another local variable with the same name in a different loop
    // at the same nesting level will not cause any conflict.
    //
    // For example, the following sequence of statements is legal:
    //
    //     for (int i = 0; i < 10; i++);
    //     for (int i = 10; i < 20; i++);
    //
    for (int i = 0; i < for_statement -> NumForInitStatements(); i++)
        ProcessStatement(for_statement -> ForInitStatement(i));

    //
    // Recall that each for statement is enclosed in a unique block by the
    // parser
    //
    BreakableStatementStack().Push(LocalBlockStack().TopBlock());
    ContinuableStatementStack().Push(LocalBlockStack().TopBlock());

    //
    // Assume that if the for_statement is reachable then its
    // contained statement is also reachable. If it turns out that the
    // condition (end) expression is a constant FALSE expression we will
    // change the assumption...
    //
    AstStatement *enclosed_statement = for_statement -> statement;
    enclosed_statement -> is_reachable = for_statement -> is_reachable;

    if (for_statement -> end_expression_opt)
    {
        ProcessExpression(for_statement -> end_expression_opt);
        if (for_statement -> end_expression_opt -> Type() ==
            control.boolean_type)
        {
            if (IsConstantFalse(for_statement -> end_expression_opt))
            {
                if (for_statement -> is_reachable)
                    for_statement -> can_complete_normally = true;
                enclosed_statement -> is_reachable = false;
            }
            else if (! IsConstantTrue(for_statement -> end_expression_opt) &&
                     for_statement -> is_reachable)
            {
                for_statement -> can_complete_normally = true;
            }
        }
        else if (for_statement -> end_expression_opt -> Type() !=
                 control.no_type)
        {
            ReportSemError(SemanticError::TYPE_NOT_BOOLEAN,
                           for_statement -> end_expression_opt -> LeftToken(),
                           for_statement -> end_expression_opt -> RightToken(),
                           for_statement -> end_expression_opt -> Type() -> Name());
        }
    }

    ProcessStatement(enclosed_statement);

    if (! enclosed_statement -> is_reachable &&
        for_statement -> is_reachable)
    {
        ReportSemError(SemanticError::UNREACHABLE_STATEMENT,
                       enclosed_statement -> LeftToken(),
                       enclosed_statement -> RightToken());
    }

    for (int j = 0; j < for_statement -> NumForUpdateStatements(); j++)
        ProcessExpressionStatement(for_statement -> ForUpdateStatement(j));

    //
    // If the for statement contained a reachable break statement,
    // then the for statement can complete normally. It is marked
    // here only for completeness, as marking the enclosing block is
    // enough to propagate the proper information upward.
    //
    AstBlock *block_body = (AstBlock *) BreakableStatementStack().Top();
    if (block_body -> can_complete_normally)
        for_statement -> can_complete_normally = true;

    BreakableStatementStack().Pop();
    ContinuableStatementStack().Pop();
}


void Semantic::ProcessSwitchStatement(Ast *stmt)
{
    AstSwitchStatement *switch_statement = (AstSwitchStatement *) stmt;

    AstBlock *enclosing_block = LocalBlockStack().TopBlock();

    //
    // We estimate a size for the switch symbol table based on the number of
    // lines in it.
    //
    AstBlock *block_body = switch_statement -> switch_block;
    BlockSymbol *block = LocalSymbolTable().Top() -> InsertBlockSymbol();
    block -> max_variable_index =
        enclosing_block -> block_symbol -> max_variable_index;
    LocalSymbolTable().Push(block -> Table());

    block_body -> block_symbol = block;
    block_body -> nesting_level = LocalBlockStack().Size();
    LocalBlockStack().Push(block_body);
    BreakableStatementStack().Push(block_body);

    ProcessExpression(switch_statement -> expression);
    TypeSymbol *type = switch_statement -> expression -> Type();

    if (type != control.int_type  && type != control.short_type &&
        type != control.char_type && type != control.byte_type &&
        type != control.no_type)
    {
        ReportSemError(SemanticError::TYPE_NOT_VALID_FOR_SWITCH,
                       switch_statement -> expression -> LeftToken(),
                       switch_statement -> expression -> RightToken(),
                       type -> ContainingPackage() -> PackageName(),
                       type -> ExternalName());
    }

    switch_statement -> default_case.switch_block_statement = NULL;

    //
    // Count the number case labels in this switch statement
    //
    int num_case_labels = 0;
    for (int i = 0; i < block_body -> NumStatements(); i++)
    {
        AstSwitchBlockStatement *switch_block_statement =
            (AstSwitchBlockStatement *) block_body -> Statement(i);
        num_case_labels += switch_block_statement -> NumSwitchLabels();
    }
    switch_statement -> AllocateCases(num_case_labels);

    //
    // A switch block is reachable iff its switch statement is reachable.
    //
    block_body -> is_reachable = switch_statement -> is_reachable;
    for (int j = 0; j < block_body -> NumStatements(); j++)
    {
        AstSwitchBlockStatement *switch_block_statement =
            (AstSwitchBlockStatement *) block_body -> Statement(j);

        for (int k = 0; k < switch_block_statement -> NumSwitchLabels(); k++)
        {
            AstCaseLabel *case_label =
                switch_block_statement -> SwitchLabel(k) -> CaseLabelCast();

            if (case_label)
            {
                ProcessExpression(case_label -> expression);

                if (case_label -> expression -> Type() != control.no_type)
                {
                    if (! case_label -> expression -> IsConstant())
                    {
                        ReportSemError(SemanticError::EXPRESSION_NOT_CONSTANT,
                                       case_label -> expression -> LeftToken(),
                                       case_label -> expression -> RightToken());
                        case_label -> expression -> symbol = control.no_type;
                    }
                    else if (type != control.no_type)
                    {
                        if (CanAssignmentConvert(type,
                                                 case_label -> expression))
                        {
                            if (case_label -> expression -> Type() != type)
                                case_label -> expression =
                                    ConvertToType(case_label -> expression,
                                                  type);

                            case_label -> map_index =
                                switch_statement -> NumCases();

                            CaseElement *case_element = compilation_unit ->
                                ast_pool -> GenCaseElement();
                            switch_statement -> AddCase(case_element);

                            case_element -> expression =
                                case_label -> expression;
                            case_element -> switch_block_statement =
                                switch_block_statement;
                            // use this index to keep sort stable !
                            case_element -> index = case_label -> map_index;
                        }
                        else if ((case_label -> expression -> Type() ==
                                  control.int_type) &&
                                 ! IsIntValueRepresentableInType(case_label -> expression, type))
                        {
                            IntToWstring value(((IntLiteralValue *) (case_label -> expression -> value)) -> value);

                            ReportSemError(SemanticError::VALUE_NOT_REPRESENTABLE_IN_SWITCH_TYPE,
                                           case_label -> expression -> LeftToken(),
                                           case_label -> expression -> RightToken(),
                                           value.String(),
                                           type -> Name());
                        }
                        else
                        {
                            ReportSemError(SemanticError::TYPE_NOT_CONVERTIBLE_TO_SWITCH_TYPE,
                                           case_label -> expression -> LeftToken(),
                                           case_label -> expression -> RightToken(),
                                           case_label -> expression -> Type() -> Name(),
                                           type -> Name());
                        }
                    }
                }
            }
            else if (! switch_statement -> default_case.switch_block_statement)
                switch_statement -> default_case.switch_block_statement =
                    switch_block_statement;
            else
            {
                ReportSemError(SemanticError::MULTIPLE_DEFAULT_LABEL,
                               ((AstDefaultLabel *) switch_block_statement -> SwitchLabel(k)) -> LeftToken(),
                               ((AstDefaultLabel *) switch_block_statement -> SwitchLabel(k)) -> RightToken());
            }
        }

        //
        // The parser ensures that a switch block statement always has one
        // statement. When a switch block ends with a sequence of switch
        // labels that are not followed by any executable statements, an
        // artificial "empty" statement is added by the parser.
        //
        assert(switch_block_statement -> NumStatements() > 0);

        //
        // A statement in a switch block is reachable iff its
        // switch statement is reachable and at least one of the
        // following is true:
        //
        // . it bears a case or default label
        // . there is a statement preceeding it in the switch block and that
        // preceeding  statement can compile normally.
        //
        AstStatement *statement =
            (AstStatement *) switch_block_statement -> Statement(0);
        statement -> is_reachable = switch_statement -> is_reachable;
        AstStatement *first_unreachable_statement =
            (AstStatement *) (statement -> is_reachable ? NULL : statement);
        ProcessStatement(statement);
        for (int j = 1; j < switch_block_statement -> NumStatements(); j++)
        {
            AstStatement *previous_statement = statement;
            statement =
                (AstStatement *) switch_block_statement -> Statement(j);
            if (switch_statement -> is_reachable)
            {
                statement -> is_reachable =
                    previous_statement -> can_complete_normally;
                if (! statement -> is_reachable &&
                    first_unreachable_statement == NULL)
                {
                    first_unreachable_statement = statement;
                }
            }

            ProcessStatement(statement);
        }

        if (first_unreachable_statement)
        {
            if (first_unreachable_statement == statement)
            {
                ReportSemError(SemanticError::UNREACHABLE_STATEMENT,
                               statement -> LeftToken(),
                               statement -> RightToken());
            }
            else
            {
                ReportSemError(SemanticError::UNREACHABLE_STATEMENTS,
                               first_unreachable_statement -> LeftToken(),
                               statement -> RightToken());
            }
        }
    }

    //
    // A switch statement can complete normally iff at least one of the
    // following is true:
    //
    // . The last statement in the switch block can complete normally.
    // . The switch block is empty or contains only switch labels.
    // . There is at least one switch label after the last switch block
    //   statement group.
    // . The switch block does not contain a default label.
    // . There is a reachable break statement that exits the switch
    //   statement. (See ProcessBreakStatement)
    //
    if (block_body -> can_complete_normally)
        switch_statement -> can_complete_normally = true;
    else if (switch_statement -> default_case.switch_block_statement == NULL)
        switch_statement -> can_complete_normally = true;
    else
    {
        AstSwitchBlockStatement *last_switch_block_statement =
            (AstSwitchBlockStatement *) block_body ->
            Statement(block_body -> NumStatements() - 1);

        assert(last_switch_block_statement -> NumStatements() > 0);

        AstStatement *last_statement =
            (AstStatement *) last_switch_block_statement ->
            Statement(last_switch_block_statement -> NumStatements() - 1);
        if (last_statement -> can_complete_normally)
            switch_statement -> can_complete_normally = true;
    }

    switch_statement -> SortCases();
    for (int k = 1; k < switch_statement -> NumCases(); k++)
    {
        if (switch_statement -> Case(k) -> Value() ==
            switch_statement -> Case(k - 1) -> Value())
        {
            IntToWstring value(switch_statement -> Case(k) -> Value());

            ReportSemError(SemanticError::DUPLICATE_CASE_VALUE,
                           switch_statement -> Case(k) -> expression -> LeftToken(),
                           switch_statement -> Case(k) -> expression -> RightToken(),
                           value.String());
        }
    }

    //
    // If an enclosed block has a higher max_variable_index than the current
    // block, update max_variable_index in the current_block, accordingly.
    // Also, update the information for the block that immediately encloses
    // the current block.
    //
    if (block -> max_variable_index <
        LocalBlockStack().TopMaxEnclosedVariableIndex())
    {
        block -> max_variable_index =
            LocalBlockStack().TopMaxEnclosedVariableIndex();
    }

    BreakableStatementStack().Pop();
    LocalBlockStack().Pop();
    LocalSymbolTable().Pop();

    if (enclosing_block && (LocalBlockStack().TopMaxEnclosedVariableIndex() <
                            block -> max_variable_index))
    {
        LocalBlockStack().TopMaxEnclosedVariableIndex() =
            block -> max_variable_index;
    }

    block -> CompressSpace(); // space optimization
}


void Semantic::ProcessDoStatement(Ast *stmt)
{
    AstDoStatement *do_statement = (AstDoStatement *) stmt;

    //
    // Recall that each Do statement is enclosed in a unique block by the
    // parser
    //
    BreakableStatementStack().Push(LocalBlockStack().TopBlock());
    ContinuableStatementStack().Push(LocalBlockStack().TopBlock());

    AstStatement *enclosed_statement = do_statement -> statement;
    enclosed_statement -> is_reachable = do_statement -> is_reachable;

    ProcessStatement(enclosed_statement);

    ProcessExpression(do_statement -> expression);

    if (do_statement -> expression -> Type() != control.boolean_type &&
        do_statement -> expression -> Type() != control.no_type)
    {
        ReportSemError(SemanticError::TYPE_NOT_BOOLEAN,
                       do_statement -> expression -> LeftToken(),
                       do_statement -> expression -> RightToken(),
                       do_statement -> expression -> Type() -> Name());
    }

    //
    // A do statement can complete normally, iff at least one of the following
    // is true:
    //     1. The contained statement can complete normally and the condition
    //        expression is not a constant expression with the value true
    //     2. There is a reachable break statement that exits the do statement
    //        (This condition is true if the block that immediately encloses
    //        this do statement can complete normally. See
    //        ProcessBreakStatement)
    //
    AstBlock *block_body = (AstBlock *) BreakableStatementStack().Top();
    do_statement -> can_complete_normally =
        ((enclosed_statement -> can_complete_normally &&
          ! IsConstantTrue(do_statement -> expression)) ||
         block_body -> can_complete_normally);

    BreakableStatementStack().Pop();
    ContinuableStatementStack().Pop();
}


void Semantic::ProcessBreakStatement(Ast *stmt)
{
    AstBreakStatement *break_statement = (AstBreakStatement *) stmt;

    //
    // Recall that it is possible to break out of any labeled statement even
    // if it is not a do, for, while or switch statement.
    //
    if (break_statement -> identifier_token_opt)
    {
        NameSymbol *name_symbol =
            lex_stream -> NameSymbol(break_statement -> identifier_token_opt);
        LabelSymbol *label_symbol =
            LocalSymbolTable().FindLabelSymbol(name_symbol);

        if (label_symbol)
        {
            break_statement -> nesting_level = label_symbol -> nesting_level;
            AstBlock *block_body = label_symbol -> block;
            //
            // A labeled statement can complete normally if there is a
            // reachable break statement that exits the labeled statement.
            // If the break occurs in a try or catch block with a finally
            // block that completes abruptly, the break is discarded.
            //
            if (block_body && break_statement -> is_reachable &&
                AbruptFinallyStack().Top() < block_body -> nesting_level)
            {
                block_body -> can_complete_normally = true;
            }
        }
        else
        {
            AstBlock *block_body = (AstBlock *) LocalBlockStack().TopBlock();
            break_statement -> nesting_level = block_body -> nesting_level;
            ReportSemError(SemanticError::UNDECLARED_LABEL,
                           break_statement -> identifier_token_opt,
                           break_statement -> identifier_token_opt,
                           lex_stream -> NameString(break_statement -> identifier_token_opt));
        }
    }
    else
    {
        AstBlock *block_body = (AstBlock *) (BreakableStatementStack().Size() > 0
                                             ? BreakableStatementStack().Top()
                                             : LocalBlockStack().TopBlock());
        break_statement -> nesting_level = block_body -> nesting_level;
        if (BreakableStatementStack().Size() > 0)
        {
            if (break_statement -> is_reachable &&
                AbruptFinallyStack().Top() < block_body -> nesting_level)
            {
                block_body -> can_complete_normally = true;
            }
        }
        else ReportSemError(SemanticError::MISPLACED_BREAK_STATEMENT,
                            break_statement -> LeftToken(),
                            break_statement -> RightToken());
    }
}


void Semantic::ProcessContinueStatement(Ast *stmt)
{
    AstContinueStatement *continue_statement = (AstContinueStatement *) stmt;

    //
    // The loop statement that is to be continued.
    //
    Ast *loop_statement = NULL;

    if (ContinuableStatementStack().Size() <= 0)
    {
        ReportSemError(SemanticError::MISPLACED_CONTINUE_STATEMENT,
                       continue_statement -> LeftToken(),
                       continue_statement -> RightToken());
    }
    else if (continue_statement -> identifier_token_opt)
    {
        NameSymbol *name_symbol = lex_stream ->
            NameSymbol(continue_statement -> identifier_token_opt);
        LabelSymbol *label_symbol =
            LocalSymbolTable().FindLabelSymbol(name_symbol);

        if (label_symbol)
        {
            continue_statement -> nesting_level =
                label_symbol -> nesting_level;

            assert(label_symbol -> block -> NumStatements() > 0);

            loop_statement = label_symbol -> block -> Statement(0);
        }
        else
        {
            AstBlock *block_body = (AstBlock *) LocalBlockStack().TopBlock();
            continue_statement -> nesting_level = block_body -> nesting_level;
            ReportSemError(SemanticError::UNDECLARED_LABEL,
                           continue_statement -> identifier_token_opt,
                           continue_statement -> identifier_token_opt,
                           lex_stream -> NameString(continue_statement -> identifier_token_opt));
        }
    }
    else
    {
        AstBlock *block_body = (AstBlock *) ContinuableStatementStack().Top();
        loop_statement = block_body -> Statement(0);
        continue_statement -> nesting_level = block_body -> nesting_level;
    }

    //
    // If this is a valid continue statement, it is associated with a loop
    // statement. Since the loop can be continued, its enclosed statement
    // "can complete normally". However, if the continue occurs in a try or
    // catch block that completes abruptly, the continue is discarded.
    //
    if (loop_statement)
    {
        AstDoStatement *do_statement = loop_statement -> DoStatementCast();
        AstForStatement *for_statement = loop_statement -> ForStatementCast();
        AstWhileStatement *while_statement =
            loop_statement -> WhileStatementCast();

        AstStatement *enclosed_statement = (do_statement
                                            ? do_statement -> statement
                                            : for_statement
                                            ? for_statement -> statement
                                            : while_statement
                                            ? while_statement -> statement
                                            : (AstStatement *) NULL);
        if (enclosed_statement)
        {
            if (AbruptFinallyStack().Top() <
                continue_statement -> nesting_level)
            {
                enclosed_statement -> can_complete_normally = true;
            }
        }
        else
        {
            assert(continue_statement -> identifier_token_opt);

            ReportSemError(SemanticError::INVALID_CONTINUE_TARGET,
                           continue_statement -> LeftToken(),
                           continue_statement -> RightToken(),
                           lex_stream -> NameString(continue_statement -> identifier_token_opt));
        }
    }
}


void Semantic::ProcessReturnStatement(Ast *stmt)
{
    AstReturnStatement *return_statement = (AstReturnStatement *) stmt;

    MethodSymbol *this_method = ThisMethod();

    if (this_method -> name_symbol == control.clinit_name_symbol ||
        this_method -> name_symbol == control.block_init_name_symbol)
    {
        ReportSemError(SemanticError::RETURN_STATEMENT_IN_INITIALIZER,
                       return_statement -> LeftToken(),
                       return_statement -> RightToken());
    }
    else if (return_statement -> expression_opt)
    {
        ProcessExpressionOrStringConstant(return_statement -> expression_opt);

        if (this_method -> Type() == control.void_type)
        {
            ReportSemError(SemanticError::MISPLACED_RETURN_WITH_EXPRESSION,
                           return_statement -> LeftToken(),
                           return_statement -> RightToken());
        }
        else if (return_statement -> expression_opt -> Type() !=
                 control.no_type)
        {
            if (this_method -> Type() !=
                return_statement -> expression_opt -> Type())
            {
                if (CanAssignmentConvert(this_method -> Type(),
                                         return_statement -> expression_opt))
                    return_statement -> expression_opt =
                        ConvertToType(return_statement -> expression_opt,
                                      this_method -> Type());
                else
                {
                    ReportSemError(SemanticError::MISMATCHED_RETURN_AND_METHOD_TYPE,
                                   return_statement -> expression_opt -> LeftToken(),
                                   return_statement -> expression_opt -> RightToken(),
                                   return_statement -> expression_opt -> Type() -> ContainingPackage() -> PackageName(),
                                   return_statement -> expression_opt -> Type() -> ExternalName(),
                                   this_method -> Type() -> ContainingPackage() -> PackageName(),
                                   this_method -> Type() -> ExternalName());
                }
            }
        }
    }
    else if (this_method -> Type() != control.void_type)
    {
        ReportSemError(SemanticError::MISPLACED_RETURN_WITH_NO_EXPRESSION,
                       return_statement -> LeftToken(),
                       return_statement -> RightToken());
    }
}


bool Semantic::UncaughtException(TypeSymbol *exception)
{
    //
    // An unchecked exception or a bad type is ok !!
    //
    if (! CheckedException(exception))
        return false;

    //
    // Firstly, check the stack of try statements to see if the exception in
    // question is catchable.
    //
    for (int i = TryStatementStack().Size() - 1; i >= 0; i--)
    {
        AstTryStatement *try_statement =
            (AstTryStatement *) TryStatementStack()[i];

        //
        // If a try statement contains a finally clause that can't complete
        // normally then the exception is discarded, hence it is considered
        // catchable. See Spec 11.3.
        //
        if (try_statement -> finally_clause_opt &&
            (! try_statement -> finally_clause_opt -> block ->
             can_complete_normally))
        {
            return false;
        }

        //
        // Check each catch clause in turn.
        //
        for (int k = 0; k < try_statement -> NumCatchClauses(); k++)
        {
            AstCatchClause *clause = try_statement -> CatchClause(k);
            VariableSymbol *symbol = clause -> parameter_symbol;
            if (CanAssignmentConvertReference(symbol -> Type(), exception))
                return false;
        }
    }

    //
    // Check if the current method declares this in the throws clause (note
    // that field initializers are not in a current method).
    //
    MethodSymbol *this_method = ThisMethod();
    if (this_method)
    {
        for (int l = this_method -> NumThrows() - 1; l >= 0; l--)
        {
            if (CanAssignmentConvertReference(this_method -> Throws(l),
                                              exception))
                return false;
        }
    }

    //
    // In the special case of instance field initializers, and instance
    // initializer blocks, check if all constructors declare the exception
    // in the throws clause.
    //
    if ((this_method &&
         this_method -> Identity() == control.block_init_name_symbol) ||
        (ThisVariable() && ! ThisVariable() -> ACC_STATIC()))
    {
        TypeSymbol *this_type = ThisType();
        MethodSymbol *ctor = this_type -> FindConstructorSymbol();
        if (this_type -> declaration -> ClassDeclarationCast())
        {
            for ( ; ctor; ctor = ctor -> next_method)
            {
                int k;
                for (k = ctor -> NumThrows() - 1; k >= 0; k--)
                {
                    if (CanAssignmentConvertReference(ctor -> Throws(k),
                                                      exception))
                        break;
                }
                if (k < 0) // No hit was found in constructor.
                    break;
            }
            return ctor != NULL; // Did all constructors catch exception?
        }
        else
        {
            assert(this_type -> declaration -> ClassInstanceCreationExpressionCast() &&
                   this_type -> Anonymous() && ctor);

            int k = 0;
            for (k = ctor -> NumThrows() - 1; k >= 0; k--)
            {
                if (CanAssignmentConvertReference(ctor -> Throws(k),
                                                  exception))
                    break;
            }
            //
            // Anonymous classes must generate the constructor to handle all
            // possible initialization exceptions; this is possible because
            // a class instance can only be created at one point, so the
            // exception can be caught in the enclosing class. If we don't
            // find the exception, we must add it.
            //
            if (k < 0)
                ctor -> AddThrows(exception);
            return false;
        }
    }

    return true; // Nothing can catch the exception.
}


wchar_t *Semantic::UncaughtExceptionContext()
{
    ErrorString s;
    MethodSymbol *this_method = ThisMethod();
    if (this_method)
    {
        s << " must be enclosed in a try statement that catches the "
          << "exception, ";
        if (this_method -> Identity() == control.clinit_name_symbol)
        {
            s << "since static initializers cannot throw checked exceptions.";
        }
        else
        {
            if (this_method -> Identity() == control.block_init_name_symbol)
            {
                assert(! ThisType() -> Anonymous());
                s << "or else every constructor in this class";
            }
            else if (this_method -> Identity() == control.init_name_symbol)
                s << "or else this constructor";
            else s << "or else this method";
            s << " must be declared to throw the exception.";
        }
    }
    else
    {
        VariableSymbol *this_variable = ThisVariable();
        assert(this_variable);
        if (this_variable -> ACC_STATIC())
            s << "must be moved to a static initializer and enclosed in a try "
              << "statement which catches the exception, since static "
              << "initializers cannot throw checked exceptions.";
        else
        {
            assert(! ThisType() -> Anonymous());
            s << "must be moved to an instance initializer or constructor and "
              << "enclosed in a try statement which catches the exception, or "
              << "else every constructor in this class must be declared to "
              << "throw the exception.";
        }
    }
    return s.Array();
}


void Semantic::ProcessThrowStatement(Ast *stmt)
{
    AstThrowStatement *throw_statement = (AstThrowStatement *) stmt;

    ProcessExpression(throw_statement -> expression);
    TypeSymbol *type = throw_statement -> expression -> Type();

    if (type != control.no_type &&
        ! CanAssignmentConvertReference(control.Throwable(), type))
    {
        ReportSemError(SemanticError::EXPRESSION_NOT_THROWABLE,
                       throw_statement -> LeftToken(),
                       throw_statement -> RightToken());
    }

    SymbolSet *exception_set = TryExceptionTableStack().Top();
    if (exception_set)
        exception_set -> AddElement(type);

    if (UncaughtException(type))
        ReportSemError(SemanticError::UNCAUGHT_THROWN_EXCEPTION,
                       throw_statement -> LeftToken(),
                       throw_statement -> RightToken(),
                       type -> ContainingPackage() -> PackageName(),
                       type -> ExternalName(),
                       UncaughtExceptionContext());
}


void Semantic::ProcessTryStatement(Ast *stmt)
{
    AstTryStatement *try_statement = (AstTryStatement *) stmt;

    //
    // A try_statement containing a finally clause requires some extra local
    // variables in its immediately enclosing block. The first holds an
    // uncaught exception from the try or catch block.  The second holds the
    // return address of the jsr.  And if the method has a return type, 1-2
    // more slots are needed to hold the return value in the case of an
    // abrupt exit from a try or catch block.
    //
    // Meanwhile, statements within try or catch blocks cannot share local
    // variables with the finally block, because of a potential VerifyError if
    // the finally overwrites a register holding a monitor of an enclosed
    // synchronized statement during an abrupt exit.
    //
    AstBlock *enclosing_block = LocalBlockStack().TopBlock();
    int max_variable_index =
        enclosing_block -> block_symbol -> max_variable_index;

    if (try_statement -> finally_clause_opt)
    {
        BlockSymbol *enclosing_block_symbol = enclosing_block -> block_symbol;
        if (enclosing_block_symbol -> try_or_synchronized_variable_index == 0)
        {
            // first such statement encountered in enclosing block?
            enclosing_block_symbol -> try_or_synchronized_variable_index =
                enclosing_block_symbol -> max_variable_index;
            enclosing_block_symbol -> max_variable_index += 2;
            if (ThisMethod() -> Type() != control.void_type)
            {
                 if (control.IsDoubleWordType(ThisMethod() -> Type()))
                      enclosing_block_symbol -> max_variable_index += 2;
                 else enclosing_block_symbol -> max_variable_index += 1;
            }
        }

        //
        // A finally block is processed in the environment of its immediate
        // enclosing block (as opposed to the environment of its associated
        // try block).
        //
        // Note that the finally block must be processed prior to the other
        // blocks in the try statement, because the computation of whether or
        // not an exception is catchable in a try statement depends on the
        // termination status of the associated finally block. See the
        // UncaughtException function. In addition, any variables used in
        // the finally block cannot be safely used in the other blocks.
        //
        AstBlock *block_body = try_statement -> finally_clause_opt -> block;
        block_body -> is_reachable = try_statement -> is_reachable;
        ProcessBlock(block_body);
        max_variable_index = block_body -> block_symbol -> max_variable_index;

        //
        // If the finally ends abruptly, then it discards any throw generated
        // by the try or catch blocks.
        //
        if (! block_body -> can_complete_normally)
        {
            TryExceptionTableStack().Push(new SymbolSet());
            AbruptFinallyStack().Push(block_body -> nesting_level);
        }
    }

    //
    // Note that the catch clauses are processed first - prior to processing
    // the main block - so that we can have their parameters available when we
    // are processing the main block, in case that block contains a throw
    // statement. See ProcessThrowStatement for more information.
    //
    // Also, recall that the body of the catch blocks must not be
    // processed within the environment of the associated try whose
    // exceptions they are supposed to catch but within the immediate enclosing
    // block (which may itself be a try block).
    //
    for (int i = 0; i < try_statement -> NumCatchClauses(); i++)
    {
        AstCatchClause *clause = try_statement -> CatchClause(i);
        AstFormalParameter *parameter = clause -> formal_parameter;

        TypeSymbol *parm_type;

        if (parameter -> type -> PrimitiveTypeCast())
        {
            ReportSemError(SemanticError::CATCH_PRIMITIVE_TYPE,
                           parameter -> LeftToken(),
                           parameter -> RightToken());
            parm_type = control.Error();
        }
        else if (parameter -> type -> ArrayTypeCast())
        {
            ReportSemError(SemanticError::CATCH_ARRAY_TYPE,
                           parameter -> LeftToken(),
                           parameter -> RightToken());
            parm_type = control.Error();
        }
        else parm_type = MustFindType(parameter -> type);

        if (! parm_type -> IsSubclass(control.Throwable()))
        {
            ReportSemError(SemanticError::TYPE_NOT_THROWABLE,
                           parameter -> LeftToken(),
                           parameter -> RightToken(),
                           parm_type -> ContainingPackage() -> PackageName(),
                           parm_type -> ExternalName());
        }

        AstVariableDeclaratorId *name =
            parameter -> formal_declarator -> variable_declarator_name;
        NameSymbol *name_symbol =
            lex_stream -> NameSymbol(name -> identifier_token);
        if (LocalSymbolTable().FindVariableSymbol(name_symbol))
        {
            ReportSemError(SemanticError::DUPLICATE_LOCAL_VARIABLE_DECLARATION,
                           name -> identifier_token,
                           name -> identifier_token,
                           name_symbol -> Name());
        }

        AstBlock *block_body = clause -> block;
        //
        // Guess that the number of elements in the table will not exceed the
        // number of statements + the clause parameter.
        //
        BlockSymbol *block = LocalSymbolTable().Top() ->
            InsertBlockSymbol(block_body -> NumStatements() + 1);
        block -> max_variable_index = max_variable_index;
        LocalSymbolTable().Push(block -> Table());

        AccessFlags access_flags = ProcessFormalModifiers(parameter);

        VariableSymbol *symbol =
            LocalSymbolTable().Top() -> InsertVariableSymbol(name_symbol);
        symbol -> SetFlags(access_flags);
        symbol -> SetType(parm_type);
        symbol -> SetOwner(ThisMethod());
        symbol -> SetLocalVariableIndex(block -> max_variable_index++);
        symbol -> MarkComplete();
        symbol -> declarator = parameter -> formal_declarator;

        clause -> parameter_symbol = symbol;

        //
        // Note that for the purpose of semantic checking we assume that
        // the body of the catch block is reachable. Whether or not the catch
        // statement can be executed at all is checked later.
        //
        block_body -> is_reachable = true;

        block_body -> block_symbol = block;
        block_body -> nesting_level = LocalBlockStack().Size();
        LocalBlockStack().Push(block_body);

        ProcessBlockStatements(block_body);

        LocalBlockStack().Pop();
        LocalSymbolTable().Pop();

        //
        // Update the information for the block that immediately encloses
        // the current block.
        //
        if (LocalBlockStack().TopMaxEnclosedVariableIndex() <
            block -> max_variable_index)
        {
            LocalBlockStack().TopMaxEnclosedVariableIndex() =
                block -> max_variable_index;
        }

        //
        // If a catch clause block can complete normally, we assume
        // that the try statement can complete normally. This may
        // prove to be false later if we find out that the finally
        // clause cannot complete normally...
        //
        if (block_body -> can_complete_normally)
            try_statement -> can_complete_normally = true;

        block -> CompressSpace(); // space optimization
    }

    //
    //
    //
    TryStatementStack().Push(try_statement);
    SymbolSet *exception_set = new SymbolSet;
    TryExceptionTableStack().Push(exception_set);

    try_statement -> block -> is_reachable = try_statement -> is_reachable;
    AstBlock *block_body = try_statement -> block;
    //
    // Guess that the number of elements in the table will not exceed the
    // number of statements + 3. This padding allows extra variable
    // declarations in things like for-init, without expensive reallocation.
    //
    BlockSymbol *block = LocalSymbolTable().Top() ->
        InsertBlockSymbol(block_body -> NumStatements() + 3);
    block -> max_variable_index = max_variable_index;
    LocalSymbolTable().Push(block -> Table());

    block_body -> block_symbol = block;
    block_body -> nesting_level = LocalBlockStack().Size();
    LocalBlockStack().Push(block_body);

    ProcessBlockStatements(block_body);

    LocalBlockStack().Pop();
    LocalSymbolTable().Pop();

    //
    // Update the information for the block that immediately encloses the
    // current block.
    //
    if (LocalBlockStack().TopMaxEnclosedVariableIndex() <
        block -> max_variable_index)
    {
        LocalBlockStack().TopMaxEnclosedVariableIndex() =
            block -> max_variable_index;
    }

    block -> CompressSpace(); // space optimization

    if (try_statement -> block -> can_complete_normally)
        try_statement -> can_complete_normally = true;

    //
    // A catch block is reachable iff both of the following are true:
    //     . Some expression or throw statement in the try block is reachable
    //       and can throw an exception that is assignable to the parameter
    //       of the catch clause C. Note that every try block, including an
    //       empty one, is considered to throw unchecked exceptions.
    //     . There is no earlier catch block A in the try statement such that
    //       the type of C's parameter is the same as or a subclass of the
    //       type of A's parameter.
    //
    // Note that the use of the word assignable here is slightly misleading.
    // It does not mean assignable in the strict sense defined in section 5.2.
    // Using the strict definition of 5.2, the rule can be more accurately
    // stated as follows:
    //
    //    . Catchable Exception:
    //      Some expression or throw statement in the try block is reachable
    //      and can throw an exception S that is assignable to the parameter
    //      with type T (S is a subclass of T) of the catch clause C.
    //      In this case, when S is thrown it will definitely be caught by
    //      clause C.
    //
    //    . Convertible Exception:
    //      The type T of the parameter of the catch clause C is assignable to
    //      the type S (T is a subclass of S) of an exception that can be
    //      thrown by some expression or throw statement in the try block that
    //      is reachable. This rule captures the idea that at run time an
    //      object declared to be of type S can actually be an instance of an
    //      object of type T in which case it will be caught by clause C.
    //
    Tuple<TypeSymbol *> catchable_exceptions,
                        convertible_exceptions;
    exception_set -> AddElement(control.Error());
    exception_set -> AddElement(control.RuntimeException());
    for (int l = 0; l < try_statement -> NumCatchClauses(); l++)
    {
        AstCatchClause *clause = try_statement -> CatchClause(l);
        TypeSymbol *type = clause -> parameter_symbol -> Type();
        int initial_length = (catchable_exceptions.Length() +
                              convertible_exceptions.Length());

        for (TypeSymbol *exception = (TypeSymbol *) exception_set -> FirstElement();
             exception;
             exception = (TypeSymbol *) exception_set -> NextElement())
        {
            if (CanAssignmentConvertReference(type, exception))
                catchable_exceptions.Next() = exception;
            else if (CanAssignmentConvertReference(exception, type))
                convertible_exceptions.Next() = exception;
        }

        //
        // No exception was found which can be caught by this clause.
        //
        if (catchable_exceptions.Length() + convertible_exceptions.Length() ==
            initial_length)
        {
            ReportSemError(SemanticError::UNREACHABLE_CATCH_CLAUSE,
                           clause -> formal_parameter -> LeftToken(),
                           clause -> formal_parameter -> RightToken(),
                           type -> ContainingPackage() -> PackageName(),
                           type -> ExternalName());
        }
        else
        {
            //
            // Search to see if this clause duplicates a prior one.
            //
            AstCatchClause *previous_clause;
            int k;
            for (k = 0; k < l; k++)
            {
                previous_clause = try_statement -> CatchClause(k);
                if (type -> IsSubclass(previous_clause -> parameter_symbol -> Type()))
                    break;
            }

            if (k < l)
            {
                FileLocation loc(lex_stream, previous_clause -> formal_parameter -> RightToken());
                ReportSemError(SemanticError::BLOCKED_CATCH_CLAUSE,
                               clause -> formal_parameter -> LeftToken(),
                               clause -> formal_parameter -> RightToken(),
                               type -> ContainingPackage() -> PackageName(),
                               type -> ExternalName(),
                               loc.location);
            }
            else clause -> block -> is_reachable = true;
        }
    }

    TryStatementStack().Pop();
    TryExceptionTableStack().Pop();
    if (TryExceptionTableStack().Top())
    {
        //
        // First, remove all the thrown exceptions that are definitely caught
        // by the enclosing try statement. Then, add the remaining ones to the
        // set that must be caught by the immediately enclosing try statement.
        //
        for (int i = 0; i < catchable_exceptions.Length(); i++)
            exception_set -> RemoveElement(catchable_exceptions[i]);
        TryExceptionTableStack().Top() -> Union(*exception_set);
    }
    delete exception_set;

    //
    // A try statement cannot complete normally if it contains a finally
    // clause that cannot complete normally. Clean up from above.
    //
    if (try_statement -> finally_clause_opt &&
        ! try_statement -> finally_clause_opt -> block -> can_complete_normally)
    {
        try_statement -> can_complete_normally = false;
        delete TryExceptionTableStack().Top();
        TryExceptionTableStack().Pop();
        AbruptFinallyStack().Pop();
    }
}


void Semantic::ProcessAssertStatement(Ast *stmt)
{
    AstAssertStatement *assert_statement = (AstAssertStatement *) stmt;

    ProcessExpression(assert_statement -> condition);

    if (assert_statement -> condition -> Type() != control.no_type &&
        assert_statement -> condition -> Type() != control.boolean_type)
    {
        ReportSemError(SemanticError::TYPE_NOT_BOOLEAN,
                       assert_statement -> condition -> LeftToken(),
                       assert_statement -> condition -> RightToken(),
                       assert_statement -> condition -> Type() -> Name());
    }
    //
    // If the condition is not constant true, store a reference to this class's
    // assert variable (creating it if necessary as a side-effect)
    //
    else if (! IsConstantTrue(assert_statement -> condition))
        assert_statement -> assert_variable =
            ThisType() -> FindOrInsertAssertVariable();

    if (assert_statement -> message_opt)
    {
        ProcessExpression(assert_statement -> message_opt);
        if (assert_statement -> message_opt -> Type() == control.void_type)
            ReportSemError(SemanticError::TYPE_IS_VOID,
                           assert_statement -> message_opt -> LeftToken(),
                           assert_statement -> message_opt -> RightToken(),
                           assert_statement -> message_opt -> Type() -> Name());
    }

    //
    // Asserts can complete normally iff reachable.
    //
    assert_statement -> can_complete_normally =
        assert_statement -> is_reachable;
}


void Semantic::ProcessEmptyStatement(Ast *stmt)
{
    AstEmptyStatement *empty_statement = (AstEmptyStatement *) stmt;

    //
    // An empty statement can complete normally iff it is reachable.
    //
    empty_statement -> can_complete_normally = empty_statement -> is_reachable;
}


TypeSymbol *Semantic::GetLocalType(AstClassDeclaration *class_declaration)
{
    NameSymbol *name_symbol =
        lex_stream -> NameSymbol(class_declaration -> identifier_token);
    TypeSymbol *type =
        LocalSymbolTable().Top() -> InsertNestedTypeSymbol(name_symbol);

    TypeSymbol *this_type = ThisType();
    TypeSymbol *outermost_type = this_type -> outermost_type;
    if (! this_type -> local)
        this_type -> local = new SymbolSet;

    //
    // Anonymous and local classes can clash if we don't use both when
    // determining the id number of this class.
    //
    IntToWstring value(this_type -> NumLocalTypes() +
                       this_type -> NumAnonymousTypes() + 1);
    int length = this_type -> ExternalNameLength() + 1 + value.Length() +
        name_symbol -> NameLength(); // +1 for $
    wchar_t *external_name = new wchar_t[length + 1]; // +1 for '\0';
    wcscpy(external_name, this_type -> ExternalName());
    wcscat(external_name, StringConstant::US_DS);
    wcscat(external_name, value.String());
    wcscat(external_name, name_symbol -> Name());

    type -> outermost_type = outermost_type;
    type -> SetExternalIdentity(control.FindOrInsertName(external_name,
                                                         length));
    this_type -> local -> AddElement(type);

    delete [] external_name;

    return type;
}


void Semantic::ProcessClassDeclaration(Ast *stmt)
{
    AstClassDeclaration *class_declaration = (AstClassDeclaration *) stmt;
    AstClassBody *class_body = class_declaration -> class_body;

    class_declaration -> MarkLocal();
    CheckNestedTypeDuplication(state_stack.Top(),
                               class_declaration -> identifier_token);

    TypeSymbol *inner_type = GetLocalType(class_declaration);
    inner_type -> outermost_type = ThisType() -> outermost_type;
    inner_type -> supertypes_closure = new SymbolSet;
    inner_type -> subtypes_closure = new SymbolSet;
    inner_type -> subtypes = new SymbolSet;
    inner_type -> semantic_environment =
        new SemanticEnvironment((Semantic *) this, inner_type,
                                state_stack.Top());
    inner_type -> declaration = class_declaration;
    inner_type -> file_symbol = source_file_symbol;
    inner_type -> SetFlags(ProcessLocalClassModifiers(class_declaration));
    inner_type -> SetOwner(ThisMethod());
    //
    // Add 3 extra elements for padding. May need a default constructor and
    // other support elements.
    //
    inner_type -> SetSymbolTable(class_body -> NumClassBodyDeclarations() + 3);
    inner_type -> SetLocation();
    inner_type -> SetSignature(control);

    //
    // If a local class is not in a static region, it needs a place to store
    // the enclosing instance.
    //
    if (! StaticRegion())
        inner_type -> InsertThis0();

    // Save environment for processing bodies later.
    class_declaration -> semantic_environment =
        inner_type -> semantic_environment;

    CheckClassMembers(inner_type, class_body);

    ProcessTypeHeaders(class_declaration);

    ProcessMembers(class_declaration -> semantic_environment, class_body);
    CompleteSymbolTable(class_declaration -> semantic_environment,
                        class_declaration -> identifier_token, class_body);
    ProcessExecutableBodies(class_declaration -> semantic_environment,
                            class_body);

    UpdateLocalConstructors(inner_type);
}


void Semantic::ProcessThisCall(AstThisCall *this_call)
{
    TypeSymbol *this_type = ThisType();

    // Signal that we are about to process an explicit constructor invocation.
    ExplicitConstructorInvocation() = this_call;

    bool no_bad_argument = true;
    int i;

    for (i = 0; i < this_call -> NumArguments(); i++)
    {
        AstExpression *expr = this_call -> Argument(i);
        ProcessExpressionOrStringConstant(expr);
        no_bad_argument = no_bad_argument && expr -> Type() != control.no_type;
    }
    if (no_bad_argument)
    {
        MethodSymbol *constructor = FindConstructor(this_type, this_call,
                                                    this_call -> this_token,
                                                    this_call -> RightToken());
        if (constructor)
        {
            this_call -> symbol = constructor;

            for (i = 0; i < this_call -> NumArguments(); i++)
            {
                AstExpression *expr = this_call -> Argument(i);
                if (expr -> Type() !=
                    constructor -> FormalParameter(i) -> Type())
                {
                    this_call -> Argument(i) =
                        ConvertToType(expr,
                                      constructor -> FormalParameter(i) -> Type());
                }
            }

            for (i = constructor -> NumThrows() - 1; i >= 0; i--)
            {
                TypeSymbol *exception = constructor -> Throws(i);
                if (UncaughtException(exception))
                    ReportSemError(SemanticError::UNCAUGHT_EXPLICIT_THIS_EXCEPTION,
                                   this_call -> this_token,
                                   this_call -> this_token,
                                   exception -> ContainingPackage() -> PackageName(),
                                   exception -> ExternalName());
            }

            //
            // Not all shadowed variables are known yet, but there is no
            // need to save context, since all shadow variables required by
            // the target constructor can just be passed along.
            //
        }
    }

    //
    // Signal that we are no longer processing an explicit constructor
    // invocation.
    //
    ExplicitConstructorInvocation() = NULL;
}


void Semantic::ProcessSuperCall(AstSuperCall *super_call)
{
    TypeSymbol *this_type = ThisType();
    // Signal that we are about to process an explicit constructor invocation.
    ExplicitConstructorInvocation() = super_call;

    TypeSymbol *super_type = this_type -> super;
    if (! super_type)
    {
        assert(this_type == control.Object());
        ReportSemError(SemanticError::OBJECT_HAS_NO_SUPER_TYPE,
                       super_call -> super_token,
                       super_call -> super_token);
        return;
    }

    if (super_call -> base_opt)
    {
        ProcessExpression(super_call -> base_opt);

        TypeSymbol *expr_type = super_call -> base_opt -> Type();
        if (expr_type != control.no_type)
        {
            TypeSymbol *containing_type = super_type -> EnclosingType();
            if (super_type -> Bad()) // error already reported
                super_call -> base_opt -> symbol = control.no_type;
            else if (! containing_type)
            {
                ReportSemError(SemanticError::SUPER_TYPE_NOT_INNER_CLASS,
                               super_call -> base_opt -> LeftToken(),
                               super_call -> base_opt -> RightToken(),
                               super_type -> ContainingPackage() -> PackageName(),
                               super_type -> ExternalName(),
                               this_type -> ContainingPackage() -> PackageName(),
                               this_type -> ExternalName(),
                               expr_type -> ContainingPackage() -> PackageName(),
                               expr_type -> ExternalName());
                super_call -> base_opt -> symbol = control.no_type;
            }
            //
            // JLS2 8.8.5.1: For an enclosing class O of the superclass,
            // the qualifying primary must be of type O or a subclass.
            //
            else if (! expr_type -> IsSubclass(containing_type))
            {
                ReportSemError(SemanticError::INVALID_ENCLOSING_INSTANCE,
                               super_call -> base_opt -> LeftToken(),
                               super_call -> base_opt -> RightToken(),
                               this_type -> ContainingPackage() -> PackageName(),
                               this_type -> ExternalName(),
                               containing_type -> ContainingPackage() -> PackageName(),
                               containing_type -> ExternalName(),
                               expr_type -> ContainingPackage() -> PackageName(),
                               expr_type -> ExternalName());
                super_call -> base_opt -> symbol = control.no_type;
            }
        }
    }
    else // (! super_call -> base_opt)
    {
        if (super_type && super_type -> EnclosingType())
            super_call -> base_opt =
                CreateAccessToType(super_call, super_type -> EnclosingType());
    }

    bool no_bad_argument = true;
    for (int i = 0; i < super_call -> NumArguments(); i++)
    {
        AstExpression *expr = super_call -> Argument(i);
        ProcessExpressionOrStringConstant(expr);
        no_bad_argument = no_bad_argument && expr -> Type() != control.no_type;
    }

    MethodSymbol *constructor = NULL;
    if (no_bad_argument)
    {
        constructor = FindConstructor(super_type, super_call,
                                      super_call -> super_token,
                                      super_call -> RightToken());
    }

    if (constructor)
    {
        if (constructor -> ACC_PRIVATE())
        {
            //
            // Create accessor constructor, and add extra null
            // to the constructor invocation.
            //
            constructor = super_type -> GetReadAccessConstructor(constructor);
            super_call -> AddNullArgument();
        }

        super_call -> symbol = constructor;
        if (super_call -> base_opt)
        {
            assert(CanAssignmentConvertReference(super_type -> EnclosingType(),
                                                 super_call -> base_opt -> Type()));

            super_call -> base_opt =
                ConvertToType(super_call -> base_opt,
                              super_type -> EnclosingType());
        }

        for (int i = 0; i < super_call -> NumArguments(); i++)
        {
            AstExpression *expr = super_call -> Argument(i);
            TypeSymbol *ctor_param_type =
                constructor -> FormalParameter(i) -> Type();
            if (expr -> Type() != ctor_param_type)
                super_call -> Argument(i) =
                    ConvertToType(expr, ctor_param_type);
        }

        //
        // Make sure that the throws signature of the constructor is
        // processed.
        //
        for (int k = constructor -> NumThrows() - 1; k >= 0; k--)
        {
            TypeSymbol *exception = constructor -> Throws(k);
            if (UncaughtException(exception))
                ReportSemError(SemanticError::UNCAUGHT_EXPLICIT_SUPER_EXCEPTION,
                               super_call -> LeftToken(),
                               super_call -> RightToken(),
                               exception -> ContainingPackage() -> PackageName(),
                               exception -> ExternalName(),
                               constructor -> containing_type -> ContainingPackage() -> PackageName(),
                               constructor -> containing_type -> ExternalName());
        }

        //
        // A local super type may use enclosed local variables. If so, we
        // must add the parameters which allow the local type to
        // initialize its shadows.
        //
        if (super_type -> IsLocal())
        {
            int param_count = super_type -> NumConstructorParameters();
            if (super_type -> LocalClassProcessingCompleted() && param_count)
            {
                super_call -> AllocateLocalArguments(param_count);
                for (int i = 0; i < param_count; i++)
                {
                    //
                    // We may need to create a shadow in the outermost
                    // local class enclosing the variable.
                    //
                    AstSimpleName *simple_name = compilation_unit ->
                        ast_pool -> GenSimpleName(super_call -> super_token);
                    VariableSymbol *accessor =
                        FindLocalVariable(super_type -> ConstructorParameter(i),
                                          this_type);
                    simple_name -> symbol = accessor;
                    TypeSymbol *owner = accessor -> ContainingType();
                    if (owner != this_type)
                        CreateAccessToScopedVariable(simple_name, owner);
                    super_call -> AddLocalArgument(simple_name);
                }
            }
            else
            {
                //
                // We are within body of super_type; save processing for
                // later, since not all shadows may be known yet. See
                // ProcessClassDeclaration.
                //
                super_type -> AddLocalConstructorCallEnvironment(GetEnvironment(super_call));
            }
        }
    }

    //
    // Signal that we are no longer processing an explicit constructor
    // invocation.
    //
    ExplicitConstructorInvocation() = NULL;
}


void Semantic::CheckThrow(AstExpression *throw_expression)
{
    TypeSymbol *throw_type = throw_expression -> symbol -> TypeCast();

    assert(throw_type);

    if (throw_type -> ACC_INTERFACE())
    {
        ReportSemError(SemanticError::NOT_A_CLASS,
                       throw_expression -> LeftToken(),
                       throw_expression -> RightToken(),
                       throw_type -> ContainingPackage() -> PackageName(),
                       throw_type -> ExternalName());
    }
    else if (! throw_type -> IsSubclass(control.Throwable()))
    {
        ReportSemError(SemanticError::TYPE_NOT_THROWABLE,
                       throw_expression -> LeftToken(),
                       throw_expression -> RightToken(),
                       throw_type -> ContainingPackage() -> PackageName(),
                       throw_type -> ExternalName());
    }
}


void Semantic::ProcessMethodBody(AstMethodDeclaration *method_declaration)
{
    MethodSymbol *this_method = ThisMethod();

    for (int k = 0; k < method_declaration -> NumThrows(); k++)
        CheckThrow(method_declaration -> Throw(k));

    if (method_declaration -> method_body -> MethodBodyCast())
    {
        AstMethodBody *block_body =
            (AstMethodBody *) method_declaration -> method_body;

        if (block_body -> explicit_constructor_opt)
            ReportSemError(SemanticError::MISPLACED_EXPLICIT_CONSTRUCTOR,
                           block_body -> explicit_constructor_opt -> LeftToken(),
                           block_body -> explicit_constructor_opt -> RightToken());

        block_body -> block_symbol = this_method -> block_symbol;
        block_body -> nesting_level = LocalBlockStack().Size();
        LocalBlockStack().Push(block_body);

        ProcessBlockStatements(block_body);

        LocalBlockStack().Pop();

        if (block_body -> can_complete_normally)
        {
            if (this_method -> Type() == control.void_type)
            {
                AstReturnStatement *return_statement =
                    compilation_unit -> ast_pool -> GenReturnStatement();
                return_statement -> return_token =
                    block_body -> right_brace_token;
                return_statement -> expression_opt = NULL;
                return_statement -> semicolon_token =
                    block_body -> right_brace_token;
                return_statement -> is_reachable = true;
                block_body -> can_complete_normally = false;
                block_body -> AddStatement(return_statement);
            }
            else
            {
                ReportSemError(SemanticError::TYPED_METHOD_WITH_NO_RETURN,
                               method_declaration -> type -> LeftToken(),
                               method_declaration -> method_declarator -> identifier_token,
                               this_method -> Header(),
                               this_method -> Type() -> Name());
            }
        }

        if (this_method -> ACC_ABSTRACT() || this_method -> ACC_NATIVE())
        {
            ReportSemError(SemanticError::ABSTRACT_METHOD_WITH_BODY,
                           method_declaration -> LeftToken(),
                           method_declaration -> RightToken(),
                           this_method -> Header());
        }
    }
    else if (! (this_method -> ACC_ABSTRACT() || this_method -> ACC_NATIVE()))
    {
        ReportSemError(SemanticError::NON_ABSTRACT_METHOD_WITHOUT_BODY,
                       method_declaration -> LeftToken(),
                       method_declaration -> RightToken(),
                       this_method -> Header());
    }

    this_method -> block_symbol -> CompressSpace(); // space optimization
}


void Semantic::ProcessConstructorBody(AstConstructorDeclaration *constructor_declaration)
{
    TypeSymbol *this_type = ThisType();
    MethodSymbol *this_method = ThisMethod();

    for (int k = 0; k < constructor_declaration -> NumThrows(); k++)
        CheckThrow(constructor_declaration -> Throw(k));

    AstMethodBody *constructor_block =
        constructor_declaration -> constructor_body;

    AstSuperCall *super_call = NULL;
    TypeSymbol *super_type = this_type -> super;

    if (constructor_block -> explicit_constructor_opt)
    {
        super_call =
            constructor_block -> explicit_constructor_opt -> SuperCallCast();
        if (super_call)
            ProcessSuperCall(super_call);
        else
        {
            AstThisCall *this_call = constructor_block ->
                explicit_constructor_opt -> ThisCallCast();
            assert(this_call);
            ProcessThisCall(this_call);
        }
    }
    else if (super_type)
    {
        LexStream::TokenIndex loc = constructor_block -> LeftToken();
        super_call = compilation_unit -> ast_pool -> GenSuperCall();
        super_call -> base_opt = NULL;
        super_call -> dot_token_opt = loc;
        super_call -> super_token = loc;
        super_call -> left_parenthesis_token = loc;
        super_call -> right_parenthesis_token = loc;
        super_call -> semicolon_token = loc;

        constructor_block -> explicit_constructor_opt = super_call;

        ProcessSuperCall(super_call);
    }

    //
    // Guess that the number of elements will not exceed the number of
    // statements.
    //
    int table_size = constructor_block -> NumStatements();
    BlockSymbol *block = LocalSymbolTable().Top() ->
        InsertBlockSymbol(table_size);
    block -> max_variable_index =
        this_method -> block_symbol -> max_variable_index;
    LocalSymbolTable().Push(block -> Table());

    constructor_block -> block_symbol = block;
    constructor_block -> nesting_level = LocalBlockStack().Size();
    LocalBlockStack().Push(constructor_block);

    ProcessBlockStatements(constructor_block);

    if (constructor_block -> can_complete_normally)
    {
        AstReturnStatement *return_statement =
            compilation_unit -> ast_pool -> GenReturnStatement();
        return_statement -> return_token =
            constructor_block -> right_brace_token;
        return_statement -> expression_opt = NULL;
        return_statement -> semicolon_token =
            constructor_block -> right_brace_token;
        return_statement -> is_reachable = true;
        constructor_block -> can_complete_normally = false;
        constructor_block -> AddStatement(return_statement);
    }

    LocalBlockStack().Pop();
    LocalSymbolTable().Pop();

    //
    // Update the local variable info for the main block associated with this
    // constructor.
    //
    if (this_method -> block_symbol -> max_variable_index <
        block -> max_variable_index)
    {
        this_method -> block_symbol -> max_variable_index =
            block -> max_variable_index;
    }

    block -> CompressSpace(); // space optimization
}


void Semantic::ProcessExecutableBodies(SemanticEnvironment *environment,
                                       AstClassBody *class_body)
{
    if (compilation_unit -> kind == Ast::BAD_COMPILATION)
        return; // errors were detected, exit now

    state_stack.Push(environment);
    TypeSymbol *this_type = ThisType();
    int i;

    assert(this_type -> HeaderProcessed());
    assert(this_type -> ConstructorMembersProcessed());
    assert(this_type -> MethodMembersProcessed());
    assert(this_type -> FieldMembersProcessed());

    // All variable declarations have already been processed.
    ThisVariable() = NULL;

    //
    // Compute the set of instance final variables which must be assigned in
    // every constructor.
    //
    Tuple<VariableSymbol *> unassigned_finals(FinalFields() -> Length());
    for (i = 0; i < FinalFields() -> Length(); i++)
    {
        VariableSymbol *variable_symbol = (*FinalFields())[i];
        if (! DefinitelyAssignedVariables() -> da_set[i])
        {
            assert(! variable_symbol -> ACC_STATIC());
            unassigned_finals.Next() = variable_symbol;
        }
    }

    if (class_body -> NumConstructors() == 0)
    {
        //
        // Issue an error for each unassigned final.
        //
        for (i = 0; i < unassigned_finals.Length(); i++)
        {
            ReportSemError(SemanticError::UNINITIALIZED_FINAL_VARIABLE,
                           unassigned_finals[i] -> declarator -> LeftToken(),
                           unassigned_finals[i] -> declarator -> RightToken(),
                           unassigned_finals[i] -> Name());
        }

        //
        // Process the body of the default constructor, if there is one (if
        // the class is invalid, one might not exist).
        //
        if (class_body -> default_constructor)
        {
            ThisMethod() =
                class_body -> default_constructor -> constructor_symbol;
            LocalSymbolTable().Push(ThisMethod() -> block_symbol -> Table());
            ProcessConstructorBody(class_body -> default_constructor);
            LocalSymbolTable().Pop();
            ThisMethod() -> max_block_depth = 1;
        }
    }
    else
    {
        DefinitePair initial_state(*DefinitelyAssignedVariables());
        for (i = 0; i < class_body -> NumConstructors(); i++)
        {
            AstConstructorDeclaration *constructor_decl =
                class_body -> Constructor(i);

            MethodSymbol *this_method = constructor_decl -> constructor_symbol;
            if (! this_method)
                continue;
            ThisMethod() = this_method;
            AstMethodBody *constructor_block =
                constructor_decl -> constructor_body;

            LocalSymbolTable().Push(this_method -> block_symbol -> Table());
            LocalBlockStack().max_size = 0;
            ProcessConstructorBody(constructor_decl);

            LocalSymbolTable().Pop();
            this_method -> max_block_depth = LocalBlockStack().max_size;

            //
            // Each constructor starts from the same initial definite
            // assignment status, except those which call this() start with
            // all fields definitely assigned.
            //
            if (constructor_block -> explicit_constructor_opt &&
                constructor_block -> explicit_constructor_opt -> ThisCallCast())
            {
                DefinitelyAssignedVariables() -> AssignAll();
                DefiniteConstructorBody(constructor_decl);
            }
            else
            {
                *DefinitelyAssignedVariables() = initial_state;
                DefiniteConstructorBody(constructor_decl);
                for (int k = 0; k < unassigned_finals.Length(); k++)
                {
                    VariableSymbol *variable_symbol = unassigned_finals[k];
                    if (! DefinitelyAssignedVariables() ->
                        da_set[variable_symbol -> LocalVariableIndex()])
                    {
                        ReportSemError(SemanticError::UNINITIALIZED_FINAL_VARIABLE_IN_CONSTRUCTOR,
                                       constructor_decl -> LeftToken(),
                                       constructor_decl -> RightToken(),
                                       variable_symbol -> Name());
                    }
                }
            }
        }
        ConstructorCycleChecker cycle_checker(class_body);
    }
    //
    // No need to worry about private access constructors, as we have already
    // done all necessary processing when creating them. Following all
    // constructors, all fields are definitely assigned, and are no longer
    // treated as blank finals.
    //
    DefinitelyAssignedVariables() -> AssignAll();
    BlankFinals() -> SetEmpty();

    for (i = 0; i < class_body -> NumMethods(); i++)
    {
        AstMethodDeclaration *method_decl = class_body -> Method(i);

        ThisMethod() = method_decl -> method_symbol;
        MethodSymbol *this_method = ThisMethod();
        if (this_method)
        {
            LocalSymbolTable().Push(this_method -> block_symbol -> Table());
            LocalBlockStack().max_size = 0;

            int start_num_errors = NumErrors();
            ProcessMethodBody(method_decl);

            LocalSymbolTable().Pop();
            this_method -> max_block_depth = LocalBlockStack().max_size;

            if (NumErrors() == start_num_errors)
                DefiniteMethodBody(method_decl);
        }
    }

    //
    // We are done with all the methods, indicate that there is no method
    // being currently compiled in this environment.
    //
    ThisMethod() = NULL;

    //
    // Recursively process all inner types
    //
    for (i = 0; i < class_body -> NumNestedClasses(); i++)
    {
        AstClassDeclaration *class_declaration = class_body -> NestedClass(i);
        if (class_declaration -> semantic_environment)
            ProcessExecutableBodies(class_declaration -> semantic_environment,
                                    class_declaration -> class_body);
    }

    for (i = 0; i < class_body -> NumNestedInterfaces(); i++)
    {
        if (class_body -> NestedInterface(i) -> semantic_environment)
            ProcessExecutableBodies(class_body -> NestedInterface(i));
    }

    DefiniteCleanUp();
    state_stack.Pop();
}


void Semantic::ProcessExecutableBodies(AstInterfaceDeclaration *interface_declaration)
{
    state_stack.Push(interface_declaration -> semantic_environment);
    TypeSymbol *this_type = ThisType();

    assert(this_type -> HeaderProcessed());
    assert(this_type -> MethodMembersProcessed());
    assert(this_type -> FieldMembersProcessed());

    for (int k = 0; k < this_type -> NumVariableSymbols(); k++)
    {
        VariableSymbol *variable_symbol = this_type -> VariableSym(k);
        if (! DefinitelyAssignedVariables() -> da_set[k])
        {
            ReportSemError(SemanticError::UNINITIALIZED_FINAL_VARIABLE_IN_INTERFACE,
                           variable_symbol -> declarator -> LeftToken(),
                           variable_symbol -> declarator -> RightToken(),
                           variable_symbol -> Name());
        }
    }

    //
    // Recursively process all inner types
    //
    for (int m = 0; m < interface_declaration -> NumNestedClasses(); m++)
    {
        AstClassDeclaration *class_declaration =
            interface_declaration -> NestedClass(m);
        if (class_declaration -> semantic_environment)
            ProcessExecutableBodies(class_declaration -> semantic_environment,
                                    class_declaration -> class_body);
    }

    for (int n = 0; n < interface_declaration -> NumNestedInterfaces(); n++)
    {
        if (interface_declaration -> NestedInterface(n) ->
            semantic_environment)
        {
            ProcessExecutableBodies(interface_declaration ->
                                    NestedInterface(n));
        }
    }

    DefiniteCleanUp();
    state_stack.Pop();
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

