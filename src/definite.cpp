// $Id: definite.cpp,v 1.17 2000/07/25 11:32:32 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "platform.h"
#include "semantic.h"

#ifdef	HAVE_NAMESPACES
using namespace Jikes;
#endif

DefiniteAssignmentSet *Semantic::DefiniteExpression(AstExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *definite = NULL;

    //
    // Is the expression a constant expression of type boolean?
    // Recall that a constant expression may not contain an
    // assignment statement.
    //
    if (expr -> IsConstant() && expr -> Type() == control.boolean_type)
    {
        definite = new DefiniteAssignmentSet(universe -> Size());
        IntLiteralValue *result = (IntLiteralValue *) expr -> value;
        if (result -> value)
        {
            definite -> true_set  = set;
            definite -> false_set = *universe;
        }
        else
        {
            definite -> true_set  = *universe;
            definite -> false_set = set;
        }
    }
    else if (expr -> symbol != control.no_type)
        definite = (this ->* DefiniteExpr[expr -> kind])(expr, set);

    return definite;
}


DefiniteAssignmentSet *Semantic::DefiniteSimpleName(AstExpression *expression, BitSet &set)
{
    AstSimpleName *simple_name = (AstSimpleName *) expression;

    if (simple_name -> resolution_opt)
        return DefiniteExpression(simple_name -> resolution_opt, set);

    //
    // Some simple names are undefined. e.g., the simple name in a method call.
    //
    VariableSymbol *variable = (simple_name -> symbol ? simple_name -> symbol -> VariableCast() : (VariableSymbol *) NULL);
    if (variable && (variable -> IsLocal(ThisMethod()) || variable -> IsFinal(ThisType())))
    {
        if (! set[variable -> LocalVariableIndex()])
        {
            ReportSemError(SemanticError::VARIABLE_NOT_DEFINITELY_ASSIGNED,
                           simple_name -> identifier_token,
                           simple_name -> identifier_token,
                           variable -> Name());

            if (variable -> IsLocal(ThisMethod())) // to avoid cascading errors!
                set.AddElement(variable -> LocalVariableIndex());
        }
    }

    return (DefiniteAssignmentSet *) NULL;
}


DefiniteAssignmentSet *Semantic::DefiniteArrayAccess(AstExpression *expression, BitSet &set)
{
    AstArrayAccess *array_access = (AstArrayAccess *) expression;

    DefiniteAssignmentSet *after_base = DefiniteExpression(array_access -> base, set);
    if (after_base) // if this is true then something is wrong
    {
        set = after_base -> true_set * after_base -> false_set;
        delete after_base;
    }

    DefiniteAssignmentSet *after_expr = DefiniteExpression(array_access -> expression, set);
    if (after_expr) // if this is true then something is wrong
    {
        set = after_expr -> true_set * after_expr -> false_set;
        delete after_expr;
    }

    return (DefiniteAssignmentSet *) NULL;
}


DefiniteAssignmentSet *Semantic::DefiniteMethodInvocation(AstExpression *expression, BitSet &set)
{
    AstMethodInvocation *method_call = (AstMethodInvocation *) expression;

    DefiniteAssignmentSet *after_method = DefiniteExpression(method_call -> method, set);
    if (after_method)
    {
        set = after_method -> true_set * after_method -> false_set;
        delete after_method;
    }

    for (int i = 0; i < method_call -> NumArguments(); i++)
    {
        AstExpression *expr = method_call -> Argument(i);
        DefiniteAssignmentSet *after_expr = DefiniteExpression(expr, set);
        if (after_expr)
        {
            set = after_expr -> true_set * after_expr -> false_set;
            delete after_expr;
        }
    }

    return (DefiniteAssignmentSet *) NULL;
}


DefiniteAssignmentSet *Semantic::DefiniteClassInstanceCreationExpression(AstExpression *expression, BitSet &set)
{
    AstClassInstanceCreationExpression *class_creation = (AstClassInstanceCreationExpression *) expression;

    if (class_creation -> base_opt)
    {
        DefiniteAssignmentSet *after_expr = DefiniteExpression(class_creation -> base_opt, set);
        if (after_expr)
        {
            set = after_expr -> true_set * after_expr -> false_set;
            delete after_expr;
        }
    }

    for (int i = 0; i < class_creation -> NumLocalArguments(); i++)
    {
        AstExpression *expr = class_creation -> LocalArgument(i);
        DefiniteAssignmentSet *after_expr = DefiniteExpression(expr, set);
        if (after_expr)
        {
            set = after_expr -> true_set * after_expr -> false_set;
            delete after_expr;
        }
    }

    for (int k = 0; k < class_creation -> NumArguments(); k++)
    {
        AstExpression *expr = class_creation -> Argument(k);
        DefiniteAssignmentSet *after_expr = DefiniteExpression(expr, set);
        if (after_expr)
        {
            set = after_expr -> true_set * after_expr -> false_set;
            delete after_expr;
        }
    }

    return (DefiniteAssignmentSet *) NULL;
}


DefiniteAssignmentSet *Semantic::DefiniteArrayCreationExpression(AstExpression *expression, BitSet &set)
{
    AstArrayCreationExpression *array_creation = (AstArrayCreationExpression *) expression;

    for (int i = 0; i < array_creation -> NumDimExprs(); i++)
    {
        AstDimExpr *dim_expr = array_creation -> DimExpr(i);
        DefiniteAssignmentSet *after_expr = DefiniteExpression(dim_expr -> expression, set);
        if (after_expr)
        {
            set = after_expr -> true_set * after_expr -> false_set;
            delete after_expr;
        }
    }

    if (array_creation -> array_initializer_opt)
        DefiniteArrayInitializer(array_creation -> array_initializer_opt);

    return (DefiniteAssignmentSet *) NULL;
}


inline VariableSymbol *Semantic::DefiniteFinal(AstFieldAccess *field_access)
{
    if (field_access -> resolution_opt)
        field_access = field_access -> resolution_opt -> FieldAccessCast();

    if (field_access)
    {
        VariableSymbol *variable = (field_access -> symbol ? field_access -> symbol -> VariableCast() : (VariableSymbol *) NULL);
        if (variable && variable -> IsFinal(ThisType()))
        {
            if (variable -> ACC_STATIC()) // there is exactly one copy of a static variable, so, it's always the right one.
                return variable;

            AstFieldAccess *sub_field_access = field_access -> base -> FieldAccessCast();
            if (field_access -> base -> ThisExpressionCast() || (sub_field_access && sub_field_access -> IsThisAccess()))
                return variable;
        }
    }

    return NULL;
}


DefiniteAssignmentSet *Semantic::DefinitePLUSPLUSOrMINUSMINUS(AstExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *definite = DefiniteExpression(expr, set);
    if (definite) // if this is true then something is wrong
    {
        set = definite -> true_set * definite -> false_set;
        delete definite;
    }

    VariableSymbol *variable = NULL;
    if (! expr -> ArrayAccessCast()) // some kind of name
    {
        MethodSymbol *read_method = NULL;
        AstSimpleName *simple_name = expr -> SimpleNameCast();
        if (simple_name)
        {
            if (simple_name -> resolution_opt)
               read_method = simple_name -> resolution_opt -> symbol -> MethodCast();
        }
        else
        {
            AstFieldAccess *field_access = expr -> FieldAccessCast();

            assert(field_access);

            if (field_access -> resolution_opt)
                read_method = field_access -> resolution_opt -> symbol -> MethodCast();
        }

        variable = (read_method ? (VariableSymbol *) read_method -> accessed_member : expr -> symbol -> VariableCast());
    }

    //
    // If we have a variable and it is final then...
    //
    if (variable && variable -> ACC_FINAL())
    {
        ReportSemError(SemanticError::TARGET_VARIABLE_IS_FINAL,
                       expr -> LeftToken(),
                       expr -> RightToken(),
                       variable -> Name());

        if (variable -> IsFinal(ThisType())) // if the final variable in contained in this type, then mark it assigned
            possibly_assigned_finals -> AddElement(variable -> LocalVariableIndex());
    }

    return (DefiniteAssignmentSet *) NULL;
}


DefiniteAssignmentSet *Semantic::DefinitePostUnaryExpression(AstExpression *expression, BitSet &set)
{
    AstPostUnaryExpression *postfix_expression = (AstPostUnaryExpression *) expression;
    return DefinitePLUSPLUSOrMINUSMINUS(postfix_expression -> expression, set);
}


DefiniteAssignmentSet *Semantic::DefiniteNOT(AstExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_expr = DefiniteExpression(expr, set);
    if (after_expr) // is the expression is a complex boolean expression?
    {
        BitSet temp(after_expr -> true_set);
        after_expr -> true_set = after_expr -> false_set;
        after_expr -> false_set = temp;
    }

    return after_expr;
}


//
// The default pre unary operators are +, -, and ~.
// As these operators are not applicable to boolean expressions,
// we do not need to invoke DefiniteExpression to process them.
//
DefiniteAssignmentSet *Semantic::DefiniteDefaultPreUnaryExpression(AstExpression *expr, BitSet &set)
{
    return (this ->* DefiniteExpr[expr -> kind])(expr, set);
}


DefiniteAssignmentSet *Semantic::DefinitePreUnaryExpression(AstExpression *expression, BitSet &set)
{
    AstPreUnaryExpression *prefix_expression = (AstPreUnaryExpression *) expression;
    return (this ->* DefinitePreUnaryExpr[prefix_expression -> pre_unary_tag])(prefix_expression -> expression, set);
}


DefiniteAssignmentSet *Semantic::DefiniteAssignmentAND(TypeSymbol *type,
                                                       BitSet *before_right,
                                                       BitSet &set,
                                                       DefiniteAssignmentSet *after_left,
                                                       DefiniteAssignmentSet *after_right)
{
    if (after_left || after_right) // one of the subexpressions is a complex boolean expression
    {
        if (! after_left)
        {
            after_left = new DefiniteAssignmentSet(universe -> Size());
            after_left -> true_set = *before_right;
            after_left -> false_set = *before_right;
        }

        if (! after_right)
        {
            after_right = new DefiniteAssignmentSet(universe -> Size());
            after_right -> true_set = set;
            after_right -> false_set = set;
        }

        after_left -> true_set += after_right -> true_set;   // definitely assigned after left when true and after right when true
        after_left -> false_set *= after_right -> false_set; // definitely assigned after left when false and after right when false
        after_right -> true_set *= after_right -> false_set; // definitely assigned after right
        after_left -> false_set += after_right -> true_set;

        delete after_right;
    }

    delete before_right; // nothing happens if before_right is null

    return after_left;
}


DefiniteAssignmentSet *Semantic::DefiniteAssignmentIOR(TypeSymbol *type,
                                                       BitSet *before_right,
                                                       BitSet &set,
                                                       DefiniteAssignmentSet *after_left,
                                                       DefiniteAssignmentSet *after_right)
{
    if (after_left || after_right) // one of the subexpressions is a complex boolean expression
    {
        if (! after_left)
        {
            after_left = new DefiniteAssignmentSet(universe -> Size());
            after_left -> true_set = *before_right;
            after_left -> false_set = *before_right;
        }

        if (! after_right)
        {
            after_right = new DefiniteAssignmentSet(universe -> Size());
            after_right -> true_set = set;
            after_right -> false_set = set;
        }

        after_left -> true_set *= after_right -> true_set;   // after a when true and after b when true
        after_right -> true_set *= after_right -> false_set; // definitely assigned after b
        after_left -> true_set += after_right -> true_set;
        after_left -> false_set += after_right -> false_set; // after a when false or after b when false

        delete after_right;
    }

    delete before_right; // nothing happens if before_right is null

    return after_left;
}


DefiniteAssignmentSet *Semantic::DefiniteAssignmentXOR(TypeSymbol *type,
                                                       BitSet *before_right,
                                                       BitSet &set,
                                                       DefiniteAssignmentSet *after_left,
                                                       DefiniteAssignmentSet *after_right)
{
    DefiniteAssignmentSet *definite = NULL;

    if (after_left || after_right) // one of the subexpressions is a complex boolean expression
    {
        definite = new DefiniteAssignmentSet(universe -> Size());

        if (! after_left)
        {
            after_left = new DefiniteAssignmentSet(universe -> Size());
            after_left -> true_set = *before_right;
            after_left -> false_set = *before_right;
        }

        if (! after_right)
        {
            after_right = new DefiniteAssignmentSet(universe -> Size());
            after_right -> true_set = set;
            after_right -> false_set = set;
        }

        //
        // compute definitely assigned after right.
        //
        definite -> true_set = definite -> false_set = (after_right -> true_set * after_right -> false_set);

        //
        // compute definitely assigned after left when true and after right when true
        // compute definitely assigned after left when false and after right when false
        // add the union of these two sets to true_set;
        //
        definite -> true_set += (after_left -> true_set * after_right -> true_set)
                              + (after_left -> false_set * after_right -> false_set);
        //
        // compute definitely assigned after left when true and after right when false
        // compute definitely assigned after left when false and after right when true
        // add the union of these two sets to false_set;
        //
        definite -> false_set += (after_left -> true_set * after_right -> false_set)
                               + (after_left -> false_set * after_right -> true_set);

        delete after_left;
        delete after_right;
    }

    delete before_right;  // nothing happens if before_right is NULL

    return definite;
}


DefiniteAssignmentSet *Semantic::DefiniteAND(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> true_set * after_left -> false_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    return DefiniteAssignmentAND(expr -> left_expression -> Type(), before_right, set, after_left, after_right);
}


DefiniteAssignmentSet *Semantic::DefiniteIOR(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> true_set * after_left -> false_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    return DefiniteAssignmentIOR(expr -> left_expression -> Type(), before_right, set, after_left, after_right);
}


DefiniteAssignmentSet *Semantic::DefiniteXOR(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> true_set * after_left -> false_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    return DefiniteAssignmentXOR(expr -> left_expression -> Type(), before_right, set, after_left, after_right);
}


DefiniteAssignmentSet *Semantic::DefiniteAND_AND(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> true_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    if (after_left)
    {
        if (after_right)
        {
            after_left -> true_set += after_right -> true_set;
            after_left -> false_set *= after_right -> false_set;
            delete after_right;
        }
        else
        {
            after_left -> true_set += set;
            after_left -> false_set *= set;
        }

        after_right = after_left;
    }
    else
    {
        if (! after_right)
        {
            after_right = new DefiniteAssignmentSet(universe -> Size());

            after_right -> true_set = set;
            after_right -> false_set = set;
        }
        after_right -> true_set += *before_right;
        after_right -> false_set *= *before_right;

        delete before_right;
    }

    return after_right;
}


DefiniteAssignmentSet *Semantic::DefiniteOR_OR(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> false_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    if (after_left)
    {
        if (after_right)
        {
            after_left -> true_set *= after_right -> true_set;
            after_left -> false_set += after_right -> false_set;
            delete after_right;
        }
        else
        {
            after_left -> true_set *= set;
            after_left -> false_set += set;
        }

        after_right = after_left;
    }
    else
    {
        if (! after_right)
        {
            after_right = new DefiniteAssignmentSet(universe -> Size());

            after_right -> true_set = set;
            after_right -> false_set = set;
        }
        after_right -> true_set *= *before_right;
        after_right -> false_set += *before_right;

        delete before_right;
    }

    return after_right;
}


DefiniteAssignmentSet *Semantic::DefiniteEQUAL_EQUAL(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> true_set * after_left -> false_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    DefiniteAssignmentSet *definite = NULL;

    if (after_left || after_right) // one of the subexpressions is a complex boolean expression
    {
        definite = new DefiniteAssignmentSet(universe -> Size());

        if (! after_left)
        {
            after_left = new DefiniteAssignmentSet(universe -> Size());
            after_left -> true_set = *before_right;
            after_left -> false_set = *before_right;
        }

        if (! after_right)
        {
            after_right = new DefiniteAssignmentSet(universe -> Size());
            after_right -> true_set = set;
            after_right -> false_set = set;
        }

        //
        // compute definitely assigned after right.
        //
        definite -> true_set = definite -> false_set = (after_right -> true_set * after_right -> false_set);

        //
        // compute definitely assigned after left when true and after right when false
        // compute definitely assigned after left when false and after right when true
        // and add their union to true_set
        //
        definite -> true_set += (after_left -> true_set * after_right -> false_set)
                              + (after_left -> false_set * after_right -> true_set);

        //
        // compute definitely assigned after left when true and after right when true
        // compute definitely assigned after left when false and after right when false
        // and add their union to false_set
        //
        definite -> false_set += (after_left -> true_set * after_right -> true_set)
                               + (after_left -> false_set * after_right -> false_set);

        delete after_left;
        delete after_right;
    }

    delete before_right; // nothing happens if before_right is NULL

    return definite;
}


DefiniteAssignmentSet *Semantic::DefiniteNOT_EQUAL(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    BitSet *before_right = NULL;
    if (after_left)
         set = after_left -> true_set * after_left -> false_set;
    else before_right = new BitSet(set);
    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);

    //
    // NOT_EQUAL has same definite assignment rules as XOR
    //
    return DefiniteAssignmentXOR(expr -> left_expression -> Type(), before_right, set, after_left, after_right);
}


DefiniteAssignmentSet *Semantic::DefiniteDefaultBinaryExpression(AstBinaryExpression *expr, BitSet &set)
{
    DefiniteAssignmentSet *after_left = DefiniteExpression(expr -> left_expression, set);
    if (after_left) // if this is true there was a mistake !!!
    {
        set = after_left -> true_set * after_left -> false_set;
        delete after_left;
    }

    DefiniteAssignmentSet *after_right = DefiniteExpression(expr -> right_expression, set);
    if (after_right) // if this is true there was a mistake !!!
    {
        set = (after_right -> true_set * after_right -> false_set);
        delete after_right;
    }

    return (DefiniteAssignmentSet *) NULL;
}


DefiniteAssignmentSet *Semantic::DefiniteBinaryExpression(AstExpression *expression, BitSet &set)
{
    AstBinaryExpression *binary_expression = (AstBinaryExpression *) expression;
    return (this ->* DefiniteBinaryExpr[binary_expression -> binary_tag])(binary_expression, set);
}


DefiniteAssignmentSet *Semantic::DefiniteConditionalExpression(AstExpression *expression, BitSet &set)
{
    AstConditionalExpression *conditional_expression = (AstConditionalExpression *) expression;

    DefiniteAssignmentSet *after_condition = DefiniteExpression(conditional_expression -> test_expression, set);
    BitSet *before_expressions = NULL;

    if (after_condition)
         set = after_condition -> true_set;
    else before_expressions = new BitSet(set);
    DefiniteAssignmentSet *after_true = DefiniteExpression(conditional_expression -> true_expression, set);
    BitSet *after_true_set = (BitSet *) (after_true ? NULL : new BitSet(set));

    if (after_condition)
         set = after_condition -> false_set;
    else set = *before_expressions;
    DefiniteAssignmentSet *after_false = DefiniteExpression(conditional_expression -> false_expression, set);

    DefiniteAssignmentSet *definite = NULL;

    if (conditional_expression -> Type() == control.boolean_type)
    {
        definite = new DefiniteAssignmentSet(universe -> Size());

        //
        //
        //
        if (after_true)
        {
            //
            // before "true" expr or after it when true
            //
            definite -> true_set = after_true -> true_set + (after_condition ? after_condition -> true_set : *before_expressions);
        }
        else definite -> true_set = *after_true_set;

        if (after_false)
        {
            //
            // before "false" expr or after it when true
            //
            definite -> true_set *= (after_false -> true_set +
                                    (after_condition ? after_condition -> false_set : *before_expressions));
        }
        else definite -> true_set *= set;

        //
        //
        //
        if (after_true)
        {
            //
            // before "true" expr or after it when false
            //
            definite -> false_set = after_true -> false_set + (after_condition ? after_condition -> true_set : *before_expressions);
        }
        else definite -> false_set = *after_true_set;

        if (after_false)
        {
            //
            // before "false" expr or after it when true
            //
            definite -> true_set *= (after_false -> false_set +
                                     (after_condition ? after_condition -> false_set : *before_expressions));
        }
        else definite -> false_set *= set;
    }
    else
    {
        if (after_false)
            set = after_false -> true_set * after_false -> false_set; // definitely assigned after the false expression ...

        // ... and definitely assigned after the true expression
        if (after_true)
             set *= (after_true -> true_set * after_true -> false_set);
        else set *= *after_true_set;
    }

    delete after_condition;    // nothing happens if after_condition is NULL
    delete before_expressions; // nothing happens if before_expressions is NULL
    delete after_true;         // nothing happens if after_true is NULL
    delete after_true_set;     // nothing happens if after_true_set is NULL
    delete after_false;        // nothing happens if after_false is NULL

    return definite;
}


DefiniteAssignmentSet *Semantic::DefiniteAssignmentExpression(AstExpression *expression, BitSet &set)
{
    AstAssignmentExpression *assignment_expression = (AstAssignmentExpression *) expression;

    AstExpression *left_hand_side = assignment_expression -> left_hand_side;
    AstSimpleName *simple_name = left_hand_side -> SimpleNameCast();
    if (simple_name)
    {
        if (simple_name -> resolution_opt)
        {
            left_hand_side = simple_name -> resolution_opt;
            simple_name = left_hand_side -> SimpleNameCast();
        }
    }
    else
    {
        AstFieldAccess *field_access = left_hand_side -> FieldAccessCast();
        if (field_access && field_access -> resolution_opt)
            left_hand_side = field_access -> resolution_opt;
    }

    VariableSymbol *variable = (left_hand_side -> symbol ? left_hand_side -> symbol -> VariableCast() : (VariableSymbol *) NULL);

    //
    // An array access is never considered to be final. Since no variable
    // is associated with the array access, the testing for the presence of variable
    // takes care of that possibility.
    //
    if (variable)
    {
        if (variable -> IsLocal(ThisMethod()) || variable -> IsFinal(ThisType()))
        {
            //
            // If the debug option "g" is set and we have a simple assignment
            // statement whose left-hand side is a local variable that has not
            // yet been defined, mark this assignment as a definition assignment.
            //
            if (control.option.g &&
                assignment_expression -> assignment_tag == AstAssignmentExpression::SIMPLE_EQUAL &&
                variable -> IsLocal(ThisMethod()) &&
                (! set[variable -> LocalVariableIndex()]))
            {
                assignment_expression -> assignment_tag = AstAssignmentExpression::DEFINITE_EQUAL;
                definite_block_stack -> TopLocallyDefinedVariables() -> AddElement(variable -> LocalVariableIndex());
                AstBlock *block = definite_block_stack -> TopBlock();
                block -> AddLocallyDefinedVariable(variable);
#ifdef DUMP
Coutput << "(1) Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(assignment_expression -> LeftToken())
        << "\n";
#endif
            }

            //
            // If we have a compound assignment then the variable must have
            // been set prior to such an assignment. otherwise, an error occurs.
            //
            if (! (assignment_expression -> SimpleAssignment() || set[variable -> LocalVariableIndex()]))
            {
                ReportSemError(SemanticError::VARIABLE_NOT_DEFINITELY_ASSIGNED,
                               assignment_expression -> left_hand_side -> LeftToken(),
                               assignment_expression -> left_hand_side -> RightToken(),
                               variable -> Name());
            }
            else if (variable -> ACC_FINAL())
            {
                //
                // If the final variable may have already been initialized, issue an error.
                //
                if ((! assignment_expression -> SimpleAssignment()) ||
                    ((*possibly_assigned_finals) [variable -> LocalVariableIndex()]))
                {
                    ReportSemError(SemanticError::TARGET_VARIABLE_IS_FINAL,
                                   assignment_expression -> left_hand_side -> LeftToken(),
                                   assignment_expression -> left_hand_side -> RightToken(),
                                   variable -> Name());
                }
                else if (definite_final_assignment_stack -> Size() > 0) // are we processing the body of a loop ?
                     definite_final_assignment_stack -> Top().Next() = left_hand_side;
            }
        }
        else if (variable -> ACC_FINAL()) // attempt to assign a value to a final field member!
        {
            ReportSemError(SemanticError::TARGET_VARIABLE_IS_FINAL,
                           assignment_expression -> left_hand_side -> LeftToken(),
                           assignment_expression -> left_hand_side -> RightToken(),
                           variable -> Name());
        }
    }

    DefiniteAssignmentSet *after_left = NULL;
    BitSet *before_right = NULL;
    //
    // The left-hand-side of an assignment expression is either a simple name,
    // a field access or an array access.
    //
    if (! simple_name)
    {
        AstFieldAccess *field_access = left_hand_side -> FieldAccessCast();
        after_left = DefiniteExpression((field_access ? field_access -> base : left_hand_side), set);
    }

    if (after_left)
         set = after_left -> true_set * after_left -> false_set;
    else before_right = new BitSet(set);

    DefiniteAssignmentSet *after_right = DefiniteExpression(assignment_expression -> expression, set);

    if (left_hand_side -> Type() == control.boolean_type)
    {
        DefiniteAssignmentSet *definite = NULL;

        if (assignment_expression -> assignment_tag == AstAssignmentExpression::AND_EQUAL)
             definite = DefiniteAssignmentAND(control.boolean_type, before_right, set, after_left, after_right);
        else if (assignment_expression -> assignment_tag == AstAssignmentExpression::XOR_EQUAL)
             definite = DefiniteAssignmentXOR(control.boolean_type, before_right, set, after_left, after_right);
        else if (assignment_expression -> assignment_tag == AstAssignmentExpression::IOR_EQUAL)
             definite = DefiniteAssignmentIOR(control.boolean_type, before_right, set, after_left, after_right);
        else
        {
            delete after_left;
            delete before_right;
            definite = after_right;
        }

        if (variable && (variable -> IsLocal(ThisMethod()) || variable -> IsFinal(ThisType())))
        {
            if (definite)
            {
                definite -> true_set.AddElement(variable -> LocalVariableIndex());
                definite -> false_set.AddElement(variable -> LocalVariableIndex());
            }
            else set.AddElement(variable -> LocalVariableIndex());

            if (variable -> ACC_FINAL())
                possibly_assigned_finals -> AddElement(variable -> LocalVariableIndex());
        }

        return definite;
    }

    if (after_right)
        set = after_right -> true_set * after_right -> false_set;

    if (variable && (variable -> IsLocal(ThisMethod()) || variable -> IsFinal(ThisType())))
    {
        set.AddElement(variable -> LocalVariableIndex());
        if (variable -> ACC_FINAL())
            possibly_assigned_finals -> AddElement(variable -> LocalVariableIndex());
    }

    delete after_left;   // nothing happens if after_left is NULL
    delete before_right; // nothing happens if before_right is NULL
    delete after_right;  // nothing happens if after_right is NULL

    return (DefiniteAssignmentSet *) NULL;
}

DefiniteAssignmentSet *Semantic::DefiniteDefaultExpression(AstExpression *expr, BitSet &set)
{
    return (DefiniteAssignmentSet *) NULL;
}

DefiniteAssignmentSet *Semantic::DefiniteParenthesizedExpression(AstExpression *expression, BitSet &set)
{
    AstParenthesizedExpression *expr = (AstParenthesizedExpression *) expression;

    return DefiniteExpression(expr -> expression, set);
}

DefiniteAssignmentSet *Semantic::DefiniteFieldAccess(AstExpression *expression, BitSet &set)
{
    AstFieldAccess *expr = (AstFieldAccess *) expression;

    VariableSymbol *variable = DefiniteFinal(expr);
    if (variable)
    {
        if (! set[variable -> LocalVariableIndex()])
        {
            ReportSemError(SemanticError::VARIABLE_NOT_DEFINITELY_ASSIGNED,
                           expr -> LeftToken(),
                           expr -> RightToken(),
                           variable -> Name());
            set.AddElement(variable -> LocalVariableIndex());
        }
    }

    return DefiniteExpression((expr -> resolution_opt ? expr -> resolution_opt : expr -> base), set);
}

DefiniteAssignmentSet *Semantic::DefiniteCastExpression(AstExpression *expression, BitSet &set)
{
    AstCastExpression *expr = (AstCastExpression *) expression;

    return DefiniteExpression(expr -> expression, set);
}

void Semantic::DefiniteArrayInitializer(AstArrayInitializer *array_initializer)
{
    for (int i = 0; i < array_initializer -> NumVariableInitializers(); i++)
    {
        AstArrayInitializer *sub_array_initializer = array_initializer -> VariableInitializer(i) -> ArrayInitializerCast();

        if (sub_array_initializer)
            DefiniteArrayInitializer(sub_array_initializer);
        else
        {
            AstExpression *init = (AstExpression *) array_initializer -> VariableInitializer(i);
            DefiniteAssignmentSet *after_init = DefiniteExpression(init, *definitely_assigned_variables);
            if (after_init)
            {
                *definitely_assigned_variables = after_init -> true_set * after_init -> false_set;
                delete after_init;
            }
        }
    }

    return;
}


inline void Semantic::DefiniteVariableInitializer(AstVariableDeclarator *variable_declarator)
{
    AstArrayInitializer *array_initializer = variable_declarator -> variable_initializer_opt -> ArrayInitializerCast();
    if (array_initializer)
        DefiniteArrayInitializer(array_initializer);
    else
    {
        AstExpression *init = (AstExpression *) variable_declarator -> variable_initializer_opt;
        DefiniteAssignmentSet *after_init = DefiniteExpression(init, *definitely_assigned_variables);
        if (after_init)
        {
            *definitely_assigned_variables = after_init -> true_set * after_init -> false_set;
            delete after_init;
        }
    }

    return;
}


inline void Semantic::DefiniteStatement(Ast *ast)
{
    (this ->* DefiniteStmt[ast -> kind])(ast);

    return;
}

inline void Semantic::DefiniteBlockStatements(AstBlock *block_body)
{
    if (control.option.g && block_body -> NumStatements() > 0)
    {
        AstStatement *statement = (AstStatement *) block_body -> Statement(0);
        DefiniteStatement(statement);
        for (int i = 1; i < block_body -> NumStatements(); i++)
        {
            statement = (AstStatement *) block_body -> Statement(i);
            if (statement -> is_reachable)
            {
                //
                // All variables that were assigned a value in the previous
                // statement must be defined
                BitSet &locally_defined_variables = *definite_block_stack -> TopLocallyDefinedVariables();
                for (int k = 0; k < definitely_assigned_variables -> Size(); k++)
                {
                    VariableSymbol *variable = definite_block_stack -> TopLocalVariables()[k];
                    if (variable) // a variable that is visible in this block? (i.e. not one declare in a non-enclosing block)
                    {
                        if ((*definitely_assigned_variables)[k] && (! locally_defined_variables[k]))
                        {
#ifdef DUMP
Coutput << "(2) Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(statement -> LeftToken())
        << "\n";
#endif
                            statement -> AddDefinedVariable(variable);
                            locally_defined_variables.AddElement(k);
                            block_body -> AddLocallyDefinedVariable(variable);
                        }
                    }
                }

                DefiniteStatement(statement);
            }
            else break;
        }
    }
    else
    {
        for (int i = 0; i < block_body -> NumStatements(); i++)
        {
            AstStatement *statement = (AstStatement *) block_body -> Statement(i);
            if (statement -> is_reachable)
                 DefiniteStatement(statement);
            else break;
        }
    }

    return;
}


void Semantic::DefiniteBlock(Ast *stmt)
{
    AstBlock *block_body = (AstBlock *) stmt;

    definite_block_stack -> Push(block_body);

    for (int i = 0; i < block_body -> block_symbol -> NumVariableSymbols(); i++)
    {
        VariableSymbol *variable = block_body -> block_symbol -> VariableSym(i);

        possibly_assigned_finals -> RemoveElement(variable -> LocalVariableIndex());
        definitely_assigned_variables -> RemoveElement(variable -> LocalVariableIndex());

        definite_visible_variables -> AddElement(variable);
    }

    DefiniteBlockStatements(block_body);

#ifdef DUMP
if (control.option.g && block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(3) At Line " << lex_stream -> Line(block_body -> RightToken())
        << " the range for the following variables end:\n\n";
for (int j = 0; j < block_body -> NumLocallyDefinedVariables(); j++)
Coutput << "    \"" << block_body -> LocallyDefinedVariable(j) -> Name() << "\"\n";
}
#endif
    for (int k = 0; k < block_body -> block_symbol -> NumVariableSymbols(); k++)
        definite_visible_variables -> RemoveElement(block_body -> block_symbol -> VariableSym(k));

    //
    // Note that in constructing the Ast, the parser encloses each
    // labeled statement in its own block... Therefore, only blocks
    // are labeled.
    //
    if (block_body -> NumLabels() > 0)
        *definitely_assigned_variables *= definite_block_stack -> TopBreakSet();

    definite_block_stack -> Pop();

    return;
}


void Semantic::DefiniteLocalVariableDeclarationStatement(Ast *stmt)
{
    AstLocalVariableDeclarationStatement *local_decl = (AstLocalVariableDeclarationStatement *) stmt;

    for (int i = 0; i < local_decl -> NumVariableDeclarators(); i++)
    {
        AstVariableDeclarator *variable_declarator = local_decl -> VariableDeclarator(i);
        VariableSymbol *variable_symbol = variable_declarator -> symbol;
        if (variable_symbol)
        {
            if (control.option.g)
            {
                assert(definite_block_stack -> TopLocalVariables()[variable_symbol -> LocalVariableIndex()] == NULL);
#ifdef DUMP
Coutput << "(3.5) Local Variable \"" << variable_symbol -> Name() << " #" << variable_symbol -> LocalVariableIndex()
        << "\" is declared at line " << lex_stream -> Line(variable_declarator -> LeftToken())
        << "\n";
#endif
                definite_block_stack -> TopLocalVariables()[variable_symbol -> LocalVariableIndex()] = variable_symbol;
            }

            if (variable_declarator -> variable_initializer_opt)
            {
                DefiniteVariableInitializer(variable_declarator);
                definitely_assigned_variables -> AddElement(variable_symbol -> LocalVariableIndex());
                if (variable_symbol -> ACC_FINAL())
                    possibly_assigned_finals -> AddElement(variable_symbol -> LocalVariableIndex());

                if (control.option.g)
                {
                    definite_block_stack -> TopLocallyDefinedVariables() -> AddElement(variable_symbol -> LocalVariableIndex());
                    AstBlock *block = definite_block_stack -> TopBlock();
                    block -> AddLocallyDefinedVariable(variable_symbol);
#ifdef DUMP
Coutput << "(4) Variable \"" << variable_symbol -> Name() << " #" << variable_symbol -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(variable_declarator -> LeftToken())
        << "\n";
#endif
                }
            }
            else definitely_assigned_variables -> RemoveElement(variable_symbol -> LocalVariableIndex());
        }
    }

    return;
}


void Semantic::DefiniteExpressionStatement(Ast *stmt)
{
    AstExpressionStatement *expression_statement = (AstExpressionStatement *) stmt;

    DefiniteAssignmentSet *after_expr = DefiniteExpression(expression_statement -> expression, *definitely_assigned_variables);
    if (after_expr)
    {
        *definitely_assigned_variables = after_expr -> true_set * after_expr -> false_set;
        delete after_expr;
    }

   return;
}


void Semantic::DefiniteSynchronizedStatement(Ast *stmt)
{
    AstSynchronizedStatement *synchronized_statement = (AstSynchronizedStatement *) stmt;

    DefiniteAssignmentSet *after_expr = DefiniteExpression(synchronized_statement -> expression, *definitely_assigned_variables);
    if (after_expr)
    {
        *definitely_assigned_variables = after_expr -> true_set * after_expr -> false_set;
        delete after_expr;
    }

    DefiniteBlock(synchronized_statement -> block);

    return;
}


void Semantic::DefiniteIfStatement(Ast *stmt)
{
    AstIfStatement *if_statement = (AstIfStatement *) stmt;

    DefiniteAssignmentSet *after_expr = DefiniteExpression(if_statement -> expression, *definitely_assigned_variables);
    BitSet after_expr_finals(*possibly_assigned_finals);
    BitSet *starting_set = (after_expr ? (BitSet *) NULL : new BitSet(*definitely_assigned_variables));
    if (after_expr)
         *definitely_assigned_variables = after_expr -> true_set;

    //
    // If the expression in the if-statement is a boolean constant expression then get its value.
    //
    IntLiteralValue *if_constant_expr = (if_statement -> expression -> Type() == control.boolean_type &&
                                         if_statement -> expression -> IsConstant()
                                                                     ? (IntLiteralValue *) if_statement -> expression -> value
                                                                     : (IntLiteralValue *) NULL);
    //
    // If either the expression is not constant or its value is true, then
    // check the statement that make up the true part of the if statement
    //
    if ((! if_constant_expr) || if_constant_expr -> value)
        DefiniteBlock(if_statement -> true_statement);

    //
    // Recall that the parser ensures that the statements that appear in an if-statement
    // (both the true and false statement) are enclosed in a block.
    //
    if (! if_statement -> false_statement_opt) // no else part ?
    {
        *definitely_assigned_variables *= (after_expr ? after_expr -> false_set : *starting_set);
        *possibly_assigned_finals += after_expr_finals;
    }
    else
    {
        BitSet after_true_finals(*possibly_assigned_finals);
        *possibly_assigned_finals = after_expr_finals;

        BitSet true_set(*definitely_assigned_variables);
        *definitely_assigned_variables = (after_expr ? after_expr -> false_set : *starting_set);

        if ((! if_constant_expr) || (! if_constant_expr -> value)) // either the expression is not constant or its value is false?
            DefiniteBlock(if_statement -> false_statement_opt);

        *definitely_assigned_variables *= true_set;
        *possibly_assigned_finals += after_true_finals;
    }

    delete after_expr;   // nothing happens if after_expr is NULL
    delete starting_set; // nothing happens if starting_set is NULL

    return;
}


void Semantic::DefiniteLoopBody(AstStatement *statement)
{
    definite_final_assignment_stack -> Push();

    BitSet starting_set(*possibly_assigned_finals);

    DefiniteStatement(statement);

    BitSet exit_set(*possibly_assigned_finals);
    exit_set += definite_block_stack -> TopFinalContinueSet();

    //
    // The set of variables that may have been possibly assigned within the body of a loop
    // consists of the set of variables that was possibly assigned at the end of the block
    // UNION the set of variables that was possibly assigned prior to a continue statement
    // within the body of the loop MINUS the set of variables that were possibly assigned
    // prior to entering the loop.
    //
    BitSet new_set(exit_set);
    new_set -= starting_set;

    for (int k = 0; k < definite_final_assignment_stack -> Top().Length(); k++)
    {
        AstExpression *name = definite_final_assignment_stack -> Top()[k];
        VariableSymbol *variable = (VariableSymbol *) name -> symbol;

        if (definite_visible_variables -> IsElement(variable) && new_set[variable -> LocalVariableIndex()])
        {
            ReportSemError(SemanticError::FINAL_VARIABLE_TARGET_IN_LOOP,
                           name -> LeftToken(),
                           name -> RightToken(),
                           variable -> Name());
        }
    }

    exit_set += definite_block_stack -> TopFinalBreakSet();
    *possibly_assigned_finals = exit_set;
    definite_final_assignment_stack -> Pop();

    return;
}


void Semantic::DefiniteWhileStatement(Ast *stmt)
{
    AstWhileStatement *while_statement = (AstWhileStatement *) stmt;

    BreakableStatementStack().Push(definite_block_stack -> TopBlock());
    ContinuableStatementStack().Push(stmt);

    DefiniteAssignmentSet *after_expr = DefiniteExpression(while_statement -> expression, *definitely_assigned_variables);

    bool while_expr = true;
    if (while_statement -> expression -> Type() == control.boolean_type && while_statement -> expression -> IsConstant())
    {
        IntLiteralValue *literal = (IntLiteralValue *) while_statement -> expression -> value;
        if (! literal -> value)
            while_expr = false;
    }

    if (after_expr)
    {
        *definitely_assigned_variables = after_expr -> true_set;
        //
        // If the expression is always false, the body of the loop is not reachable and therefore will never execute.
        //
        if (while_expr)
            DefiniteLoopBody(while_statement -> statement);
        *definitely_assigned_variables = (after_expr -> false_set * definite_block_stack -> TopBreakSet());
        delete after_expr;
    }
    else
    {
        BitSet starting_set(*definitely_assigned_variables);
        //
        // If the expression is always false, the body of the loop is not reachable and therefore will never execute.
        //
        if (while_expr)
            DefiniteLoopBody(while_statement -> statement);
        *definitely_assigned_variables = (starting_set * definite_block_stack -> TopBreakSet());
    }

    ContinuableStatementStack().Pop();
    BreakableStatementStack().Pop();

    return;
}


void Semantic::DefiniteForStatement(Ast *stmt)
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
    if (control.option.g && for_statement -> NumForInitStatements() > 0)
    {
        AstStatement *statement = (AstStatement *) for_statement -> ForInitStatement(0);
        DefiniteStatement(statement);
        for (int i = 1; i < for_statement -> NumForInitStatements(); i++)
        {
            statement = (AstStatement *) for_statement -> ForInitStatement(i);

            //
            // All variables that were assigned a value in the previous
            // statement must be defined
            //
            BitSet &locally_defined_variables = *definite_block_stack -> TopLocallyDefinedVariables();
            for (int k = 0; k < definitely_assigned_variables -> Size(); k++)
            {
                VariableSymbol *variable = definite_block_stack -> TopLocalVariables()[k];
                if (variable) // a variable that is visible in this block? (i.e. not one declare in a non-enclosing block)
                {
                    if ((*definitely_assigned_variables)[k] && (! locally_defined_variables[k]))
                    {
#ifdef DUMP
Coutput << "(5) Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(statement -> LeftToken())
        << "\n";
#endif
                        statement -> AddDefinedVariable(variable);
                        locally_defined_variables.AddElement(k);
                        AstBlock *block = definite_block_stack -> TopBlock();
                        block -> AddLocallyDefinedVariable(variable);
                    }
                }
            }

            DefiniteStatement(statement);
        }
    }
    else
    {
        for (int i = 0; i < for_statement -> NumForInitStatements(); i++)
        {
            DefiniteStatement(for_statement -> ForInitStatement(i));
        }
    }

    BreakableStatementStack().Push(definite_block_stack -> TopBlock());
    ContinuableStatementStack().Push(stmt);

    DefiniteAssignmentSet *after_end_expression = NULL;
    BitSet before_statement(universe -> Size());

    bool for_expr = true;
    if (for_statement -> end_expression_opt)
    {
        after_end_expression = DefiniteExpression(for_statement -> end_expression_opt, *definitely_assigned_variables);

        if (for_statement -> end_expression_opt -> Type() == control.boolean_type &&
            for_statement -> end_expression_opt -> IsConstant())
        {
            IntLiteralValue *literal = (IntLiteralValue *) for_statement -> end_expression_opt -> value;
            if (! literal -> value)
                for_expr = false;
        }
    }

    if (after_end_expression)
         *definitely_assigned_variables = after_end_expression -> true_set;
    else before_statement = *definitely_assigned_variables;

    //
    // If the expression is always false, the body of the loop is not reachable and therefore will never execute.
    //
    if (for_expr)
        DefiniteLoopBody(for_statement -> statement);

    //
    // Compute the set of variables that are definitely assigned after the
    // contained statement and after every continue statement that may exit
    // the body of the for statement.
    //
    *definitely_assigned_variables *= definite_block_stack -> TopContinueSet();
    for (int j = 0; j < for_statement -> NumForUpdateStatements(); j++)
        DefiniteExpressionStatement(for_statement -> ForUpdateStatement(j));

    //
    // Compute the set of variables that belongs to both sets below:
    //
    //    . the universe if no condition expression is present;
    //      otherwise, the set of variables that is definitely assigned when
    //      the condition expression is false.
    //
    //    . the set of variables that is definitely assigned before every
    //      break statement that may exit the for statement.
    //
    *definitely_assigned_variables = (for_statement -> end_expression_opt
                                                     ? (after_end_expression ? after_end_expression -> false_set : before_statement)
                                                     : *universe); // set of variables that depend on the condition

    //
    // The replacement
    //
    *definitely_assigned_variables *= definite_block_stack -> TopBreakSet();

    delete after_end_expression; // nothing happens if after_end_expression is NULL

    ContinuableStatementStack().Pop();
    BreakableStatementStack().Pop();

    return;
}


void Semantic::DefiniteDoStatement(Ast *stmt)
{
    AstDoStatement *do_statement = (AstDoStatement *) stmt;

    BreakableStatementStack().Push(definite_block_stack -> TopBlock());
    ContinuableStatementStack().Push(stmt);

    bool do_expr = true;
    if (do_statement -> expression -> Type() == control.boolean_type && do_statement -> expression -> IsConstant())
    {
        IntLiteralValue *literal = (IntLiteralValue *) do_statement -> expression -> value;
        if (! literal -> value)
            do_expr = false;
    }

    if (do_expr)
         DefiniteLoopBody(do_statement -> statement);
    else DefiniteStatement(do_statement -> statement); // The expression is always false therefore, loop will execute exactly once.

    BitSet after_stmt(*definitely_assigned_variables);
    *definitely_assigned_variables *= definite_block_stack -> TopContinueSet();
    DefiniteAssignmentSet *after_expr = DefiniteExpression(do_statement -> expression, *definitely_assigned_variables);
    if (after_expr)
    {
        *definitely_assigned_variables = (after_expr -> false_set * definite_block_stack -> TopBreakSet());
        delete after_expr;
    }
    else *definitely_assigned_variables *= definite_block_stack -> TopBreakSet();

    ContinuableStatementStack().Pop();
    BreakableStatementStack().Pop();

    return;
}


void Semantic::DefiniteSwitchStatement(Ast *stmt)
{
    AstSwitchStatement *switch_statement = (AstSwitchStatement *) stmt;

    AstBlock *block_body = switch_statement -> switch_block;
    definite_block_stack -> Push(block_body);
    BreakableStatementStack().Push(block_body);

    DefiniteAssignmentSet *after_expr = DefiniteExpression(switch_statement -> expression, *definitely_assigned_variables);
    if (after_expr)
    {
        *definitely_assigned_variables = after_expr -> true_set * after_expr -> false_set;
        delete after_expr;
    }

    BitSet starting_set(*definitely_assigned_variables),
           after_expr_finals(*possibly_assigned_finals),
           switch_finals_union(*possibly_assigned_finals);

    for (int i = 0; i < block_body -> NumStatements(); i++)
    {
        AstSwitchBlockStatement *switch_block_statement = (AstSwitchBlockStatement *) block_body -> Statement(i);

        *definitely_assigned_variables = starting_set;
        *possibly_assigned_finals = after_expr_finals;

        if (control.option.g && switch_block_statement -> NumStatements() > 0)
        {
            BitSet &locally_defined_variables = *definite_block_stack -> TopLocallyDefinedVariables();
            AstStatement *statement = (AstStatement *) switch_block_statement -> Statement(0);
            DefiniteStatement(statement);
            for (int i = 1; i < switch_block_statement -> NumStatements(); i++)
            {
                statement = (AstStatement *) switch_block_statement -> Statement(i);
                if (statement -> is_reachable)
                {
                    //
                    // All variables that were assigned a value in the previous
                    // statement must be defined
                    //
                    for (int k = 0; k < definitely_assigned_variables -> Size(); k++)
                    {
                        VariableSymbol *variable = definite_block_stack -> TopLocalVariables()[k];
                        if (variable) // a variable that is visible in this block? (i.e. not one declare in a non-enclosing block)
                        {
                            if ((*definitely_assigned_variables)[k] && (! locally_defined_variables[k]))
                            {
#ifdef DUMP
Coutput << "Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(statement -> LeftToken())
        << "\n";
#endif
                                statement -> AddDefinedVariable(variable);
                                locally_defined_variables.AddElement(k);
                                block_body -> AddLocallyDefinedVariable(variable);
                            }
                        }
                    }

                    DefiniteStatement(statement);
                }
                else break;
            }

#ifdef DUMP
if (control.option.g && block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(5.5) At Line " << lex_stream -> Line(statement -> RightToken())
        << " the range for the following variables end:\n\n";
for (int j = 0; j < block_body -> NumLocallyDefinedVariables(); j++)
Coutput << "    \"" << block_body -> LocallyDefinedVariable(j) -> Name() << "\"\n";
}
#endif
            //
            // At the end of a switch block statement, we always close the range of all local
            // variables that have been defined. That's because even if this switch block statement
            // entends into the next block, it is not the only entry point into that block. Therefore,
            // in order for a variable to be definitely defined at the starting point of a switch
            // block statement, it must have been defined prior to the switch statement.
            //
            for (int k = 0; k < block_body -> NumLocallyDefinedVariables(); k++)
                locally_defined_variables.RemoveElement(block_body -> LocallyDefinedVariable(k) -> LocalVariableIndex());
            block_body -> TransferLocallyDefinedVariablesTo(switch_block_statement);
        }
        else
        {
            for (int i = 0; i < switch_block_statement -> NumStatements(); i++)
            {
                AstStatement *statement = (AstStatement *) switch_block_statement -> Statement(i);
                if (statement -> is_reachable)
                     DefiniteStatement(statement);
                else break;
            }
        }

        //
        // Update possibly_assigned_finals here. If a continue, break (of an enclosing statement), return or throw
        // statement was encountered...
        //
        switch_finals_union += definite_block_stack -> TopFinalExitSet(*possibly_assigned_finals);
    }

    if (switch_statement -> default_case.switch_block_statement) // Is there a default case?
         *definitely_assigned_variables *= definite_block_stack -> TopBreakSet();
    else *definitely_assigned_variables = starting_set;

    *possibly_assigned_finals = switch_finals_union;

    BreakableStatementStack().Pop();
    definite_block_stack -> Pop();

    return;
}


void Semantic::DefiniteBreakStatement(Ast *stmt)
{
    AstBreakStatement *break_statement = (AstBreakStatement *) stmt;

    //
    // Compute the set of variables that are definitely assigned prior to executing the break.
    //
    definite_block_stack -> BreakSet(break_statement -> nesting_level) *= (*definitely_assigned_variables);
    definite_block_stack -> FinalBreakSet(break_statement -> nesting_level) += (*possibly_assigned_finals);

    //
    // After execution of a break statement, it is vacuously true
    // that every variable has definitely been assigned and no final
    // variable has been possibly assigned (as nothing is reachable
    // any way).
    //
    *definitely_assigned_variables = *universe;
    possibly_assigned_finals -> SetEmpty();

    return;
}


void Semantic::DefiniteContinueStatement(Ast *stmt)
{
    AstContinueStatement *continue_statement = (AstContinueStatement *) stmt;

    //
    // Compute the set of variables that are definitely assigned prior to executing the continue.
    //
    definite_block_stack -> ContinueSet(continue_statement -> nesting_level) *= (*definitely_assigned_variables);
    definite_block_stack -> FinalContinueSet(continue_statement -> nesting_level) += (*possibly_assigned_finals);

    //
    // After execution of a continue statement, it is vacuously true
    // that every variable has definitely been assigned and no final
    // variable has been possibly assigned (as nothing is reachable
    // any way).
    //
    *definitely_assigned_variables = *universe;
    possibly_assigned_finals -> SetEmpty();

    return;
}


void Semantic::DefiniteReturnStatement(Ast *stmt)
{
    AstReturnStatement *return_statement = (AstReturnStatement *) stmt;

    if (return_statement -> expression_opt)
    {
        DefiniteAssignmentSet *after_expr = DefiniteExpression(return_statement -> expression_opt, *definitely_assigned_variables);
        if (after_expr)
            delete after_expr;
    }

    //
    // Compute the set of variables that are definitely assigned prior to executing
    // this return statement. Note that this set is only relevant to the method or
    // constructor block containing this statement.
    //
    // TODO: Do we really need this?
    //
    //    definite_block_stack -> TopReturnSet() *= (*definitely_assigned_variables);
    //

    //
    // Compute the set of variables that are possibly assigned prior to executing this return statement.
    // We have a few cases to consider:
    //
    //    1. The return statement is not contained in a try statement - the possibly-assigned set is only relevant
    //       to the enclosing method (or constructor) block. The definitely assigned set is updated as if the return
    //       statement was a break statement out of the method (or constructor) block.
    //
    //    2. If the return statement is contained in a try main block or a try
    //       catch block that contains a finally clause - the possibly-assigned
    //       block is relevant to that main try block or catch block.
    //
    //    3. otherwise, treat the return statement as if it immediately followed its containing try statement
    //
    if (definite_try_stack -> Size() == 0)
        definite_block_stack -> ReturnSet(0) *= (*definitely_assigned_variables);
    else
    {
        for (int i = definite_try_stack -> Size() - 1; i >= 0; i--)
        {
            AstTryStatement *try_statement = definite_try_stack -> TryStatement(i);
            //
            // Is the return statement enclosed in a try main block or catch block
            // that  contains a finally clause. Note that a try statement is removed from
            // the definite_try_stack before its finally clause is processed. thus, a return
            // statement that is enclosed in a finally clause will appear in an enclosing
            // try statement, if any...
            //
            if (try_statement -> finally_clause_opt)
            {
                int k;
                for (k = definite_block_stack -> Size() - 1;
                     definite_block_stack -> Block(k) != definite_try_stack -> Block(i);
                     k--)
                    ;

                assert(k >= 0);

                definite_block_stack -> FinalReturnSet(k) += (*possibly_assigned_finals);
                break;
            }
        }
    }

    //
    // After execution of a return statement, it is vacuously true
    // that every variable has definitely been assigned and no final
    // variable has been possibly assigned (as nothing is reachable
    // any way).
    //
    *definitely_assigned_variables = *universe;
    possibly_assigned_finals -> SetEmpty();

    return;
}


void Semantic::DefiniteThrowStatement(Ast *stmt)
{
    AstThrowStatement *throw_statement = (AstThrowStatement *) stmt;

    DefiniteAssignmentSet *after_expr = DefiniteExpression(throw_statement -> expression, *definitely_assigned_variables);
    if (after_expr)
        delete after_expr;

    //
    // Compute the set of variables that are definitely assigned prior to executing
    // this throw statement. Note that this set is only relevant to the method or
    // constructor block containing this statement.
    //
    // TODO: Do we really need this?
    //
    //    definite_block_stack -> TopThrowSet() *= (*definitely_assigned_variables);
    //

    //
    // Compute the set of variables that are possibly assigned prior to executing this throw statement
    // and update the proper enclosing block appropriately.
    //
    // We have a few cases to consider:
    //
    //    1. The throw statement is not contained in a try statement - the possibly-assigned
    //       set is only relevant to the enclosing method (or constructor) block. If the
    //       containing function in question is a method (i.e., not a constructor) then the
    //       definitely assigned set is updated as if the throw statement was a break statement
    //       out of the method block.
    //
    //    2. The throw statement is enclosed in a try statement main block or catch clause.
    //
    //        2a. if the nearest try-block that encloses the throw statement is a main try-block -
    //            the possibly-assigned block is relevant to that main block.
    //
    //        2b. if the nearest try-block that encloses the throw statement is a catch-block and
    //            the try block contains a finally clause - the possibly-assigned block is relevant
    //            to the catch-block
    //
    //        2c. otherwise, treat the throw statement as if it immediately followed its containing
    //            try statement
    //
    if (definite_try_stack -> Size() == 0)
    {
        if (ThisMethod() -> Identity() != control.init_name_symbol) // Not a constructor
            definite_block_stack -> ThrowSet(0) *= (*definitely_assigned_variables);
    }
    else
    {
        for (int i = definite_try_stack -> Size() - 1; i >= 0; i--)
        {
            AstTryStatement *try_statement = definite_try_stack -> TryStatement(i);
            //
            // Is the return statement enclosed in a try main block or catch block
            // that  contains a finally clause. Note that a try statement is removed from
            // the definite_try_stack before its finally clause is processed. thus, a return
            // statement that is enclosed in a finally clause will appear in an enclosing
            // try statement, if any...
            //
            if (try_statement -> block == definite_try_stack -> Block(i)) // Is the throw statement enclosed in main try block?
            {
                int k;
                for (k = definite_block_stack -> Size() - 1; definite_block_stack -> Block(k) != try_statement -> block; k--)
                    ;

                assert(k >= 0);

                definite_block_stack -> FinalThrowSet(k) += (*possibly_assigned_finals);
                break;
            }
            else if (try_statement -> finally_clause_opt)
            {
                int k;
                for (k = definite_block_stack -> Size() - 1;
                     definite_block_stack -> Block(k) != definite_try_stack -> Block(i);
                     k--)
                    ;

                assert(k >= 0);

                definite_block_stack -> FinalThrowSet(k) += (*possibly_assigned_finals);
                break;
            }
        }
    }

    //
    // After execution of a throw statement, it is vacuously true
    // that every variable has definitely been assigned and no final
    // variable has been possibly assigned (as nothing is reachable
    // any way).
    //
    *definitely_assigned_variables = *universe;
    possibly_assigned_finals -> SetEmpty();

    return;
}


void Semantic::DefiniteTryStatement(Ast *stmt)
{
    AstTryStatement *try_statement = (AstTryStatement *) stmt;
    definite_try_stack -> Push(try_statement);

    BitSet starting_set(*definitely_assigned_variables);

    AstBlock *try_block_body = try_statement -> block;
    definite_block_stack -> Push(try_block_body);
    definite_try_stack -> SetTopBlock(try_block_body);

    for (int j = 0; j < try_block_body -> block_symbol -> NumVariableSymbols(); j++)
    {
        VariableSymbol *variable = try_block_body -> block_symbol -> VariableSym(j);

        possibly_assigned_finals -> RemoveElement(variable -> LocalVariableIndex());
        definitely_assigned_variables -> RemoveElement(variable -> LocalVariableIndex());
        definite_visible_variables -> AddElement(variable);
    }

    BitSet before_try_finals(*possibly_assigned_finals);

    DefiniteBlockStatements(try_block_body);

#ifdef DUMP
if (control.option.g && try_block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(6) At Line " << lex_stream -> Line(try_block_body -> RightToken())
        << " the range for the following variables end:\n\n";
for (int k = 0; k < try_block_body -> NumLocallyDefinedVariables(); k++)
Coutput << "    \"" << try_block_body -> LocallyDefinedVariable(k) -> Name() << "\"\n";
}
#endif
    BitSet &exit_set = definite_block_stack -> TopFinalExitSet(*possibly_assigned_finals),
           before_catch_finals(exit_set),
           possibly_finals_union(exit_set);

    //
    // Once we are done with a block, its enclosed local variables are no longer visible.
    //
    for (int l = 0; l < try_block_body -> block_symbol -> NumVariableSymbols(); l++)
        definite_visible_variables -> RemoveElement(try_block_body -> block_symbol -> VariableSym(l));

    definite_block_stack -> Pop();

    //
    // We initilize the variable after_blocks here. It is used to calculate intersection
    // of the set of variables that are definitely assigned by all the blocks: the try block,
    // all the catch blocks, if any, and the finally block, if there is one.
    //
    BitSet after_blocks(*definitely_assigned_variables);

    //
    // Recall that the body of the catch blocks must not be
    // processed within the environment of the associated try whose
    // exceptions they are supposed to catch but within the immediate
    // enclosing block (which may itself be a try block).
    //
    for (int i = 0; i < try_statement -> NumCatchClauses(); i++)
    {
        *definitely_assigned_variables = starting_set;

        //
        // We process the catch block here instead of invoking DefiniteBlock,
        // in order to make sure that the formal parameter (which is declared)
        // inside the block is identified as having been definitely assigned.
        //
        AstCatchClause *clause = try_statement -> CatchClause(i);

        AstBlock *clause_block_body = clause -> block;
        definite_block_stack -> Push(clause_block_body);
        definite_try_stack -> SetTopBlock(clause_block_body);

        for (int j = 0; j < clause_block_body -> block_symbol -> NumVariableSymbols(); j++)
        {
            VariableSymbol *variable = clause_block_body -> block_symbol -> VariableSym(j);

            possibly_assigned_finals -> RemoveElement(variable -> LocalVariableIndex());
            definitely_assigned_variables -> RemoveElement(variable -> LocalVariableIndex());
            definite_visible_variables -> AddElement(variable);
        }

        //
        // The parameter must be (re) added after removing all variables in the block
        // from the set !!!
        //
        definitely_assigned_variables -> AddElement(clause -> parameter_symbol -> LocalVariableIndex());
        if (control.option.g)
        {
            VariableSymbol *variable = clause -> parameter_symbol;
            definite_block_stack -> TopLocallyDefinedVariables() -> AddElement(variable -> LocalVariableIndex());
#ifdef DUMP
Coutput << "(7) Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(clause -> formal_parameter -> LeftToken())
        << "\n";
#endif
        }
        *possibly_assigned_finals = before_catch_finals;
        if (clause -> parameter_symbol -> ACC_FINAL())
            possibly_assigned_finals -> AddElement(clause -> parameter_symbol -> LocalVariableIndex());

        DefiniteBlockStatements(clause_block_body);

#ifdef DUMP
if (control.option.g && clause_block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(8) At Line " << lex_stream -> Line(clause_block_body -> RightToken())
        << " the range for the following variables end:\n\n";
for (int l = 0; l < clause_block_body -> NumLocallyDefinedVariables(); l++)
Coutput << "    \"" << clause_block_body -> LocallyDefinedVariable(l) -> Name() << "\"\n";
}
#endif
        //
        // Once we are done with a block, its enclosed local variables are no longer visible.
        //
        for (int k = 0; k < clause_block_body -> block_symbol -> NumVariableSymbols(); k++)
            definite_visible_variables -> RemoveElement(clause_block_body -> block_symbol -> VariableSym(k));

        possibly_finals_union += definite_block_stack -> TopFinalExitSet(*possibly_assigned_finals);

        definite_block_stack -> Pop();

        //
        // Process the set of variables that were definitely assigned
        // after this catch block
        //
        after_blocks *= *definitely_assigned_variables;
    }

    *possibly_assigned_finals = possibly_finals_union;
    definite_try_stack -> Pop();

    //
    // Like the catch clauses, a finally block must not be processed
    // in the environment of its associated try block but in the
    // environment of its immediate enclosing block.
    //
    if (try_statement -> finally_clause_opt)
    {
        *definitely_assigned_variables = starting_set;

        DefiniteBlock(try_statement -> finally_clause_opt -> block);

        *definitely_assigned_variables += after_blocks;
    }
    else *definitely_assigned_variables = after_blocks;

    return;
}


void Semantic::DefiniteEmptyStatement(Ast *stmt)
{
    return;
}


void Semantic::DefiniteClassDeclaration(Ast *decl)
{
    //
    // All the methods within the body of a local class are processed when
    // the class is compiled.
    //

    return;
}


void Semantic::DefiniteMethodBody(AstMethodDeclaration *method_declaration, Tuple<VariableSymbol *> &finals)
{
    if (! method_declaration -> method_body -> EmptyStatementCast())
    {
#ifdef DUMP
if (control.option.g)
Coutput << "(9) Processing method \"" << method_declaration -> method_symbol -> Name()
        << "\" in " << ThisType() -> ContainingPackage() -> PackageName() << "/"
        << ThisType() -> ExternalName() << "\n";
#endif
        AstConstructorBlock *constructor_block = method_declaration -> method_body -> ConstructorBlockCast();
        AstBlock *block_body = (constructor_block ? constructor_block -> block : (AstBlock *) method_declaration -> method_body);

        universe = new BitSet(block_body -> block_symbol -> max_variable_index + finals.Length(), BitSet::UNIVERSE);
        definite_block_stack = new DefiniteBlockStack(control,
                                                      method_declaration -> method_symbol -> max_block_depth,
                                                      universe -> Size());
        definite_try_stack = new DefiniteTryStack(method_declaration -> method_symbol -> max_block_depth);
        definite_final_assignment_stack =  new DefiniteFinalAssignmentStack();
        definite_visible_variables = new SymbolSet(universe -> Size());
        definitely_assigned_variables = new BitSet(universe -> Size(), BitSet::EMPTY);
        possibly_assigned_finals = new BitSet(universe -> Size(), BitSet::EMPTY);
        for (int i = 0; i < finals.Length(); i++) // Assume that all final instance variables have been assigned a value.
        {
            int index = block_body -> block_symbol -> max_variable_index + i;
            finals[i] -> SetLocalVariableIndex(index);
            definitely_assigned_variables -> AddElement(index);
            possibly_assigned_finals -> AddElement(index);
            definite_visible_variables -> AddElement(finals[i]);
        }

        definite_block_stack -> Push(block_body);

        AstMethodDeclarator *method_declarator = method_declaration -> method_declarator;
        for (int k = 0; k < method_declarator -> NumFormalParameters(); k++)
        {
            AstVariableDeclarator *formal_declarator = method_declarator -> FormalParameter(k) -> formal_declarator;
            definitely_assigned_variables -> AddElement(formal_declarator -> symbol -> LocalVariableIndex());
            if (control.option.g)
            {
                VariableSymbol *variable = formal_declarator -> symbol;
                definite_block_stack -> TopLocallyDefinedVariables() -> AddElement(variable -> LocalVariableIndex());
#ifdef DUMP
Coutput << "(10) Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(formal_declarator -> LeftToken())
        << "\n";
#endif
            }

            if (formal_declarator -> symbol -> ACC_FINAL())
                possibly_assigned_finals -> AddElement(formal_declarator -> symbol -> LocalVariableIndex());
            definite_visible_variables -> AddElement(formal_declarator -> symbol);
        }

        for (int l = 0; l < block_body -> block_symbol -> NumVariableSymbols(); l++)
        {
            VariableSymbol *variable = block_body -> block_symbol -> VariableSym(l);
            definite_visible_variables -> AddElement(variable);
        }

        DefiniteBlockStatements(block_body);

#ifdef DUMP
if (control.option.g && block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(11) At Line " << lex_stream -> Line(block_body -> RightToken())
        << " the range for the following variables end:\n\n";
for (int j = 0; j < block_body -> NumLocallyDefinedVariables(); j++)
Coutput << "    \"" << block_body -> LocallyDefinedVariable(j) -> Name() << "\"\n";
}
#endif
        definite_block_stack -> Pop();

        delete universe;
        delete definitely_assigned_variables;
        delete definite_block_stack;
        delete definite_try_stack;
        delete definite_final_assignment_stack;
        delete definite_visible_variables;
        delete possibly_assigned_finals;
    }

    return;
}


void Semantic::DefiniteConstructorBody(AstConstructorDeclaration *constructor_declaration, Tuple<VariableSymbol *> &finals)
{
#ifdef DUMP
if (control.option.g)
Coutput << "(12) Processing constructor \"" << constructor_declaration -> constructor_symbol -> Name()
        << "\" in " << ThisType() -> ContainingPackage() -> PackageName() << "/"
        << ThisType() -> ExternalName() << "\n";
#endif
    AstConstructorBlock *constructor_block = constructor_declaration -> constructor_body;
    AstBlock *block_body = constructor_block -> block;

    universe = new BitSet(block_body -> block_symbol -> max_variable_index + finals.Length(), BitSet::UNIVERSE);
    definite_block_stack = new DefiniteBlockStack(control,
                                                  constructor_declaration -> constructor_symbol -> max_block_depth,
                                                  universe -> Size());
    definite_try_stack = new DefiniteTryStack(constructor_declaration -> constructor_symbol -> max_block_depth);
    definite_final_assignment_stack =  new DefiniteFinalAssignmentStack();
    definite_visible_variables = new SymbolSet(universe -> Size());
    definitely_assigned_variables = new BitSet(universe -> Size(), BitSet::EMPTY);
    possibly_assigned_finals = new BitSet(universe -> Size(), BitSet::EMPTY);
    for (int i = 0; i < finals.Length(); i++)
    {
        int index = block_body -> block_symbol -> max_variable_index + i;

        finals[i] -> SetLocalVariableIndex(index);
        if (finals[i] -> IsDefinitelyAssigned())
        {
            definitely_assigned_variables -> AddElement(index);
            possibly_assigned_finals -> AddElement(index);
        }
        else if (finals[i] -> IsPossiblyAssigned())
            possibly_assigned_finals -> AddElement(index);
        definite_visible_variables -> AddElement(finals[i]);
    }

    //
    // As an explicit constructor call cannot refer to any locally declared variables
    // other than formal parameters, no local variable can be assigned within it (other
    // than a formal parameter which is considered to have been assigned anyway). Therefore,
    // the following code is not necessary:
    //
    //    if (this_call)
    //         DefiniteThisCall(this_call);
    //    else if (super_call)
    //         DefiniteSuperCall(super_call);
    //

    definite_block_stack -> Push(block_body);
    if (control.option.g)
    {
        //
        // We need this initialization to prevent debug info from being generated for 
        // final fields (since they are not truly local variables).
        //
        BitSet &locally_defined_variables = *definite_block_stack -> TopLocallyDefinedVariables();
        for (int i = 0; i < finals.Length(); i++) // Assume that all final instance variables have been assigned a value.
        {
            int index = block_body -> block_symbol -> max_variable_index + i;
            locally_defined_variables.AddElement(index);
        }
    }

    AstMethodDeclarator *constructor_declarator = constructor_declaration -> constructor_declarator;
    for (int j = 0; j < constructor_declarator -> NumFormalParameters(); j++)
    {
        AstVariableDeclarator *formal_declarator = constructor_declarator -> FormalParameter(j) -> formal_declarator;
        definitely_assigned_variables -> AddElement(formal_declarator -> symbol -> LocalVariableIndex());
        if (control.option.g)
        {
            VariableSymbol *variable = formal_declarator -> symbol;
            definite_block_stack -> TopLocallyDefinedVariables() -> AddElement(variable -> LocalVariableIndex());
#ifdef DUMP
Coutput << "(13) Variable \"" << variable -> Name() << " #" << variable -> LocalVariableIndex()
        << "\" is defined at line " << lex_stream -> Line(formal_declarator -> LeftToken())
        << "\n";
#endif
        }

        if (formal_declarator -> symbol -> ACC_FINAL())
            possibly_assigned_finals -> AddElement(formal_declarator -> symbol -> LocalVariableIndex());
        definite_visible_variables -> AddElement(formal_declarator -> symbol);
    }

    for (int l = 0; l < block_body -> block_symbol -> NumVariableSymbols(); l++)
    {
        VariableSymbol *variable = block_body -> block_symbol -> VariableSym(l);
        definite_visible_variables -> AddElement(variable);
    }

    DefiniteBlockStatements(block_body);

#ifdef DUMP
if (control.option.g && block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(14) At Line " << lex_stream -> Line(block_body -> RightToken())
        << " the range for the following variables end:\n\n";
for (int j = 0; j < block_body -> NumLocallyDefinedVariables(); j++)
Coutput << "    \"" << block_body -> LocallyDefinedVariable(j) -> Name() << "\"\n";
}
#endif
    //
    // Compute the set of finals that has definitely been assigned in this constructor
    //
    BitSet &exit_set = definite_block_stack -> TopExitSet(*definitely_assigned_variables);
    for (int k = 0; k < finals.Length(); k++)
    {
        int index = block_body -> block_symbol -> max_variable_index + k;
        if (exit_set[index])
            finals[k] -> MarkDefinitelyAssigned();
    }

    definite_block_stack -> Pop();

    delete universe;
    delete definitely_assigned_variables;
    delete definite_block_stack;
    delete definite_try_stack;
    delete definite_final_assignment_stack;
    delete definite_visible_variables;
    delete possibly_assigned_finals;

    return;
}


void Semantic::DefiniteBlockInitializer(AstBlock *block_body, int stack_size, Tuple<VariableSymbol *> &finals)
{
#ifdef DUMP
if (control.option.g)
Coutput << "(15) Processing Initializer block "
        << " in " << ThisType() -> ContainingPackage() -> PackageName() << "/"
        << ThisType() -> ExternalName() << "\n";
#endif
    universe = new BitSet(block_body -> block_symbol -> max_variable_index + finals.Length(), BitSet::UNIVERSE);
    definite_block_stack = new DefiniteBlockStack(control, stack_size + 1, universe -> Size()); // +1 for absent method block
    definite_try_stack = new DefiniteTryStack(stack_size + 1);
    definite_final_assignment_stack =  new DefiniteFinalAssignmentStack();
    definite_visible_variables = new SymbolSet(universe -> Size());
    definitely_assigned_variables = new BitSet(universe -> Size(), BitSet::EMPTY);
    possibly_assigned_finals = new BitSet(universe -> Size(), BitSet::EMPTY);
    for (int i = 0; i < finals.Length(); i++)
    {
        int index = block_body -> block_symbol -> max_variable_index + i;

        finals[i] -> SetLocalVariableIndex(index);
        if (finals[i] -> IsDefinitelyAssigned())
        {
            definitely_assigned_variables -> AddElement(index);
            possibly_assigned_finals -> AddElement(index);
        }
        else if (finals[i] -> IsPossiblyAssigned())
            possibly_assigned_finals -> AddElement(index);
        definite_visible_variables -> AddElement(finals[i]);
    }

    definite_block_stack -> Push(NULL); // No method available
    definite_block_stack -> Push(block_body);
    if (control.option.g)
    {
        //
        // We need this initialization to prevent debug info from being generated for 
        // final fields (since they are not truly local variables).
        //
        BitSet &locally_defined_variables = *definite_block_stack -> TopLocallyDefinedVariables();
        for (int i = 0; i < finals.Length(); i++) // Assume that all final instance variables have been assigned a value.
        {
            int index = block_body -> block_symbol -> max_variable_index + i;
            locally_defined_variables.AddElement(index);
        }
    }

    for (int j = 0; j < block_body -> block_symbol -> NumVariableSymbols(); j++)
    {
        VariableSymbol *variable = block_body -> block_symbol -> VariableSym(j);
        definite_visible_variables -> AddElement(variable);
    }

    DefiniteBlockStatements(block_body);

#ifdef DUMP
if (control.option.g && block_body -> NumLocallyDefinedVariables() > 0)
{
Coutput << "(16) At Line " << lex_stream -> Line(block_body -> RightToken())
        << " the range for the following variables end:\n\n";
for (int j = 0; j < block_body -> NumLocallyDefinedVariables(); j++)
Coutput << "    \"" << block_body -> LocallyDefinedVariable(j) -> Name() << "\"\n";
}
#endif
    //
    // For each final that has definitely been assigned a value in this block,
    // mark it appropriately.
    //
    BitSet &exit_set = definite_block_stack -> TopExitSet(*definitely_assigned_variables);
    for (int k = 0; k < finals.Length(); k++)
    {
        int index = block_body -> block_symbol -> max_variable_index + k;
        if (exit_set[index])
            finals[k] -> MarkDefinitelyAssigned();
    }

    //
    // For each final that may have possibly been assigned a value in this block,
    // mark it appropriately.
    //
    exit_set = definite_block_stack -> TopFinalExitSet(*possibly_assigned_finals);
    for (int l = 0; l < finals.Length(); l++)
    {
        int index = block_body -> block_symbol -> max_variable_index + l;
        if (exit_set[index])
            finals[l] -> MarkPossiblyAssigned();
    }

    definite_block_stack -> Pop();
    definite_block_stack -> Pop(); // remove NULL that was pushed to indicate that no method is available

    delete universe;
    delete definitely_assigned_variables;
    delete definite_block_stack;
    delete definite_try_stack;
    delete definite_final_assignment_stack;
    delete definite_visible_variables;
    delete possibly_assigned_finals;

    return;
}


void Semantic::DefiniteVariableInitializer(AstVariableDeclarator *variable_declarator, Tuple<VariableSymbol *> &finals)
{
    universe = new BitSet(finals.Length(), BitSet::UNIVERSE);
    definite_block_stack = NULL;
    definite_try_stack = NULL;
    definite_final_assignment_stack =  new DefiniteFinalAssignmentStack();
    definite_visible_variables = new SymbolSet(universe -> Size());
    definitely_assigned_variables = new BitSet(universe -> Size(), BitSet::EMPTY);
    possibly_assigned_finals = new BitSet(universe -> Size(), BitSet::EMPTY);
    for (int i = 0; i < finals.Length(); i++)
    {
        finals[i] -> SetLocalVariableIndex(i);
        if (finals[i] -> IsDefinitelyAssigned())
        {
            definitely_assigned_variables -> AddElement(i);
            possibly_assigned_finals -> AddElement(i);
        }
        else if (finals[i] -> IsPossiblyAssigned())
            possibly_assigned_finals -> AddElement(i);
        definite_visible_variables -> AddElement(finals[i]);
    }

    DefiniteVariableInitializer(variable_declarator);
    VariableSymbol *symbol = variable_declarator -> symbol;
    if (symbol -> ACC_FINAL())
        symbol -> MarkDefinitelyAssigned();
    //
    // Also, update any other finals that may have been initialized as
    // a side-effect in an assignment embedded within the initializer
    // expression.
    //
    BitSet &exit_set = *definitely_assigned_variables;
    for (int k = 0; k < finals.Length(); k++)
    {
        if (exit_set[k])
            finals[k] -> MarkDefinitelyAssigned();
    }

    delete universe;
    delete definitely_assigned_variables;
    delete definite_final_assignment_stack;
    delete definite_visible_variables;
    delete possibly_assigned_finals;

    return;
}
