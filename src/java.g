%options scopes,act,an=javaact.cpp,hn=javaact.h,em,tab,gp=c++,
%options fp=java,escape=$,prefix=TK_,em,defer,output-size=125
%options hblockb=\:,hblocke=:\,nogoto-default,single-productions
%options la=1,names=max,jikes
-- $Id: java.g,v 1.39 2004/01/23 12:07:01 ericb Exp $
-- This software is subject to the terms of the IBM Jikes Compiler
-- License Agreement available at the following URL:
-- http://ibm.com/developerworks/opensource/jikes.
-- Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
-- You must accept the terms of that agreement to use this software.

------------------------------------------------------------------------
--
--                               J A V A
--
-- This Java grammar started from the grammar defined in chapter 19 of the
-- first edition of the Java Language Specification manual.  It has since been
-- updated with several additional rules to cover additional language features,
-- as well as simplified in places where it makes sense to share code. Comments
-- are given where this grammar differs from the original. Note that the
-- second edition JLS grammar (chapter 18) is completely broken.  This grammar
-- is in JIKES PG format with semantic actions following each rule. In
-- specifying the rules, the symbols are enclosed in single quotes, and the
-- keywords are all caps, so that they can be quickly distinguished from
-- non-terminals. Optional symbols are suffixed with "opt" and the rules
-- expanding such definitions can be found at the end. Also, some syntactic
-- markers have been added to aid the parser in skipping irrelevant
-- components during different parse phases.
--
-- The file javaact.h produced by JIKESPG from this file (java.g) contains a
-- very readable definition of the grammar rules together with their
-- associated semantic actions. That file is marked with appropriate
-- location directives (automatically generated) which instructs the C++
-- compiler to issue error messages in terms of this file (java.g).
-- Therefore, though the user is encouraged to read javaact.h, no
-- change should ever be made to that file. Instead, changes should
-- always be made in this file and javaact.h should be regenerated
-- using JIKESPG.
--
------------------------------------------------------------------------

$Define

--
-- This macro generates a header for an action function consisting
-- of the rule in question (commented) and a location directive.
--
$location
/.

//
// Rule $rule_number:  $rule_text
//
#line $next_line "$input_file"./

--
-- This macro is used to initialize the rule_action array
-- to an unnamed function. A name is generated using the
-- number of the rule in question.
--
$action
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::Act$rule_number;
#else
    void Act$rule_number();
#endif
./

--
-- These macros are used to initialize the rule_action array
-- to a specific named function.
--
$MakeArrayType
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeArrayType;
#endif
./

$MakeImportDeclaration
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeImportDeclaration;
#endif
./

$MakeClassBody
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeClassBody;
#endif
./

$MakeQualifiedSuper
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeQualifiedSuper;
#endif
./

$MakeArrayInitializer
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeArrayInitializer;
#endif
./

$MakeLocalVariable
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeLocalVariable;
#endif
./

$MakeQualifiedNew
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeQualifiedNew;
#endif
./

$MakeMethodDeclaration
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeMethodDeclaration;
#endif
./

$MakeMethodHeader
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeMethodHeader;
#endif
./

$MakeMethodDeclarator
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeMethodDeclarator;
#endif
./

$MakeConstructorDeclaration
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeConstructorDeclaration;
#endif
./

$MakeLabeledStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeLabeledStatement;
#endif
./

$MakeExpressionStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeExpressionStatement;
#endif
./

$MakeIfThenElseStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeIfThenElseStatement;
#endif
./

$MakeSwitchLabel
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeSwitchLabel;
#endif
./

$MakeWhileStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeWhileStatement;
#endif
./

$MakeForStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeForStatement;
#endif
./

$MakeAssertStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeAssertStatement;
#endif
./

$MakeTryStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeTryStatement;
#endif
./

$MakeParenthesizedExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeParenthesizedExpression;
#endif
./

$MakeClassLiteral
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeClassLiteral;
#endif
./

$MakeArrayCreationUninitialized
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeArrayCreationUninitialized;
#endif
./

$MakeFieldAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeFieldAccess;
#endif
./

$MakeSuperFieldAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeSuperFieldAccess;
#endif
./

$MakeQualifiedSuperFieldAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeQualifiedSuperFieldAccess;
#endif
./

$MakeArrayAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeArrayAccess;
#endif
./

$MakePreUnaryExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakePreUnaryExpression;
#endif
./

$MakeCastExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeCastExpression;
#endif
./

$MakeBinaryExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeBinaryExpression;
#endif
./

$MakeInstanceofExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeInstanceofExpression;
#endif
./

$MakeConditionalExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeConditionalExpression;
#endif
./

$SetSym1ToSym2
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::SetSym1ToSym2;
#endif
./

$StartList
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::StartList;
#endif
./

$AddList2
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::AddList2;
#endif
./

$AddList3
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::AddList3;
#endif
./

--
-- This macro is used to initialize the rule_action array
-- to the NullAction function.
--
$NullAction
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::NullAction;
#endif
./

--
-- This macro is used to initialize the rule_action array
-- to the NoAction function.
--
$NoAction
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::NoAction;
#endif
./

--
-- This macro generates a header for a named action function that is
-- already defined and will be shared.
--
$shared_function
/.

//
// Rule $rule_number:  $rule_text./

--
-- This macro generates a header for a rule that invokes the
-- no_function routine.
--
$shared_NoAction
/.

//
// Rule $rule_number:  $rule_text
//
// void NoAction();
//./

--
-- This macro generates a header for a rule that invokes the
-- null_function routine.
--
$shared_NullAction
/.

//
// Rule $rule_number:  $rule_text
//
// void NullAction();
//./

--
-- This macro generates a header for a rule that invokes the
-- StartList routine.
--
$shared_StartList
/.

//
// Rule $rule_number:  $rule_text
//
// void StartList();
//./

--
-- This macro generates a header for a rule that invokes the
-- AddList2 routine.
--
$shared_AddList2
/.

//
// Rule $rule_number:  $rule_text
//
// void AddList2();
//./

--
-- This macro generates a header for a rule that invokes the
-- AddList3 routine.
--
$shared_AddList3
/.

//
// Rule $rule_number:  $rule_text
//
// void AddList3();
//./

--
-- This macro generates a header for MakePreUnaryExpression.
--
$shared_Unary
/.

//
// Rule $rule_number:  $rule_text
//
// void MakePreUnaryExpression();
//./
--
-- This macro generates a header for MakeBinaryExpression.
--
$shared_Binary
/.

//
// Rule $rule_number:  $rule_text
//
// void MakeBinaryExpression();
//./

$Terminals

    abstract assert boolean break byte case catch char class continue
    default do double else extends false final finally float for
    if implements import instanceof int interface long native new null
    package private protected public return short static strictfp super switch
    synchronized this throw throws transient true try void volatile while

    Identifier IntegerLiteral LongLiteral FloatLiteral DoubleLiteral
    CharacterLiteral StringLiteral

    PLUS_PLUS
    MINUS_MINUS
    EQUAL_EQUAL
    LESS_EQUAL
    GREATER_EQUAL
    NOT_EQUAL
    LEFT_SHIFT
    RIGHT_SHIFT
    UNSIGNED_RIGHT_SHIFT
    PLUS_EQUAL
    MINUS_EQUAL
    MULTIPLY_EQUAL
    DIVIDE_EQUAL
    AND_EQUAL
    OR_EQUAL
    XOR_EQUAL
    REMAINDER_EQUAL
    LEFT_SHIFT_EQUAL
    RIGHT_SHIFT_EQUAL
    UNSIGNED_RIGHT_SHIFT_EQUAL
    OR_OR
    AND_AND
    PLUS
    MINUS
    NOT
    REMAINDER
    XOR
    AND
    MULTIPLY
    OR
    TWIDDLE
    DIVIDE
    GREATER
    LESS
    LPAREN
    RPAREN
    LBRACE
    RBRACE
    LBRACKET
    RBRACKET
    SEMICOLON
    QUESTION
    COLON
    COMMA
    DOT
    EQUAL

-- This is a special token that allows us to do a 2-pass parse.
    BodyMarker

-- These tokens will be used in JDK 1.5, but are not used now.
    enum
    AT
    ELLIPSIS

-- These remaining tokens are not used in the grammar.
    const
    goto
    ERROR
    EOF

$Alias

    '++'   ::= PLUS_PLUS
    '--'   ::= MINUS_MINUS
    '=='   ::= EQUAL_EQUAL
    '<='   ::= LESS_EQUAL
    '>='   ::= GREATER_EQUAL
    '!='   ::= NOT_EQUAL
    '<<'   ::= LEFT_SHIFT
    '>>'   ::= RIGHT_SHIFT
    '>>>'  ::= UNSIGNED_RIGHT_SHIFT
    '+='   ::= PLUS_EQUAL
    '-='   ::= MINUS_EQUAL
    '*='   ::= MULTIPLY_EQUAL
    '/='   ::= DIVIDE_EQUAL
    '&='   ::= AND_EQUAL
    '|='   ::= OR_EQUAL
    '^='   ::= XOR_EQUAL
    '%='   ::= REMAINDER_EQUAL
    '<<='  ::= LEFT_SHIFT_EQUAL
    '>>='  ::= RIGHT_SHIFT_EQUAL
    '>>>=' ::= UNSIGNED_RIGHT_SHIFT_EQUAL
    '||'   ::= OR_OR
    '&&'   ::= AND_AND
    '+'    ::= PLUS
    '-'    ::= MINUS
    '!'    ::= NOT
    '%'    ::= REMAINDER
    '^'    ::= XOR
    '&'    ::= AND
    '*'    ::= MULTIPLY
    '|'    ::= OR
    '~'    ::= TWIDDLE
    '/'    ::= DIVIDE
    '>'    ::= GREATER
    '<'    ::= LESS
    '('    ::= LPAREN
    ')'    ::= RPAREN
    '{'    ::= LBRACE
    '}'    ::= RBRACE
    '['    ::= LBRACKET
    ']'    ::= RBRACKET
    ';'    ::= SEMICOLON
    '?'    ::= QUESTION
    ':'    ::= COLON
    ','    ::= COMMA
    '.'    ::= DOT
    '='    ::= EQUAL

    '@'    ::= AT
    '...'  ::= ELLIPSIS

    $EOF   ::= EOF
    $ERROR ::= ERROR

$Start

    Goal

$Rules

\:
// $Id: java.g,v 1.39 2004/01/23 12:07:01 ericb Exp $ -*- c++ -*-
// DO NOT MODIFY THIS FILE - it is generated using jikespg on java.g.
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef HEADERS

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

void Parser::InitRuleAction()
{
    rule_action[0] = &Parser::BadAction;
#else // HEADERS
    AstType* MakeArrayType(int tokennum);
    AstName* MakeSimpleName(int tokennum);
    AstArguments* MakeArguments(int tokennum);
    void MakeLocalVariable(AstModifiers* modifiers, AstType* type,
                           AstListNode* variables);
    AstBlock* MakeBlock(int tokennum);
    AstStatement* MakeSwitchBlockStatement(AstListNode* labels,
                                           AstListNode* statements = NULL);
    void MakeMethodInvocation(int tokennum);
    void MakeCastExpression(AstType* type, int tokennum);

    void BadAction();
    void NoAction();
    void NullAction();
    void SetSym1ToSym2();
    void StartList();
    void AddList2();
    void AddList3();
    void MakeArrayType();
    void MakeImportDeclaration();
    void MakeClassBody();
    void MakeQualifiedSuper();
    void MakeArrayInitializer();
    void MakeLocalVariable();
    void MakeQualifiedNew();
    void MakeMethodDeclaration();
    void MakeMethodHeader();
    void MakeMethodDeclarator();
    void MakeConstructorDeclaration();
    void MakeLabeledStatement();
    void MakeExpressionStatement();
    void MakeIfThenElseStatement();
    void MakeSwitchLabel();
    void MakeWhileStatement();
    void MakeForStatement();
    void MakeAssertStatement();
    void MakeTryStatement();
    void MakeParenthesizedExpression();
    void MakeClassLiteral();
    void MakeArrayCreationUninitialized();
    void MakeFieldAccess();
    void MakeSuperFieldAccess();
    void MakeQualifiedSuperFieldAccess();
    void MakePreUnaryExpression();
    void MakeCastExpression();
    void MakeArrayAccess();
    void MakeBinaryExpression();
    void MakeInstanceofExpression();
    void MakeConditionalExpression();
#endif // HEADERS

:\

/.#line $next_line "$input_file"
// $Id: java.g,v 1.39 2004/01/23 12:07:01 ericb Exp $
// DO NOT MODIFY THIS FILE - it is generated using jikespg on java.g.
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 2004 IBM Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"
#include "parser.h"
#include "ast.h"

#undef HEADERS
#include "javaact.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

//**************************************************************************//
//**************************************************************************//
//*                                                                        *//
//* Below, we show each rule of the Java grammar together with the         *//
//* semantic action that is invoked when the parser performs a reduction   *//
//* by that rule.                                                          *//
//**************************************************************************//
//**************************************************************************//

./

--18.2 Productions from 2.3: The syntactic Grammar

Goal ::= CompilationUnit
\:$NoAction:\
/.
//
// The parse was bad. Give up now.
//
void Parser::BadAction() { assert(false); }

$location
//
// Given a rule of the form A ::= x1 x2 ... xn        n >= 1
// Do nothing - Whatever Ast was produced for x1 is inherited by A.
//
void Parser::NoAction() {}
./

Goal ::= BodyMarker MethodBody
\:$SetSym1ToSym2:\
/.
//
// This next rule was added to allow the parser to recognize the body of a
// funtion (constructor, method, or initializer) out of context. Note that
// the artificial terminal BodyMarker is added here to prevent an ordinary
// parse from accepting a body as a valid input - i.e., to recognize a body
// out-of-context, the BodyMarker terminal must be inserted in front of the
// input stream containing the body in question.
$location
//
// Given a rule of the form A ::= x1 x2, inherit the result from x2.
//
void Parser::SetSym1ToSym2() { Sym(1) = Sym(2); }
./


--18.3 Productions from 3: Lexical Structure

Literal ::= IntegerLiteral
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewIntegerLiteral(Token(1));
}
./

Literal ::= LongLiteral
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewLongLiteral(Token(1));
}
./

Literal ::= FloatLiteral
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewFloatLiteral(Token(1));
}
./

Literal ::= DoubleLiteral
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewDoubleLiteral(Token(1));
}
./

Literal ::= BooleanLiteral
\:$NoAction:\
/.$shared_NoAction./

Literal ::= CharacterLiteral
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewCharacterLiteral(Token(1));
}
./

Literal ::= StringLiteral
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewStringLiteral(Token(1));
}
./

Literal ::= 'null'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewNullLiteral(Token(1));
}
./

BooleanLiteral ::= 'true'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewTrueLiteral(Token(1));
}
./

BooleanLiteral ::= 'false'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewFalseLiteral(Token(1));
}
./

--18.4 Productions from 4: Types, Values and Variables

Type ::= PrimitiveType
\:$NoAction:\
/.$shared_NoAction./

Type ::= ReferenceType
\:$NoAction:\
/.$shared_NoAction./

PrimitiveType ::= NumericType
\:$NoAction:\
/.$shared_NoAction./

PrimitiveType ::= 'boolean'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::BOOLEAN, Token(1));
}
./

NumericType ::= IntegralType
\:$NoAction:\
/.$shared_NoAction./

NumericType ::= FloatingPointType
\:$NoAction:\
/.$shared_NoAction./

IntegralType ::= 'byte'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::BYTE, Token(1));
}
./

IntegralType ::= 'short'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::SHORT, Token(1));
}
./

IntegralType ::= 'int'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::INT, Token(1));
}
./

IntegralType ::= 'long'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::LONG, Token(1));
}
./

IntegralType ::= 'char'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::CHAR, Token(1));
}
./

FloatingPointType ::= 'float'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::FLOAT, Token(1));
}
./

FloatingPointType ::= 'double'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::DOUBLE, Token(1));
}
./

ReferenceType ::= ClassOrInterfaceType
\:$NoAction:\
/.$shared_NoAction./

ReferenceType ::= ArrayType
\:$NoAction:\
/.$shared_NoAction./

--
-- Simplify.
--
--ClassOrInterfaceType ::= ClassType
--ClassOrInterfaceType ::= InterfaceType
--ClassType ::= Name
--InterfaceType ::= Name
ClassOrInterfaceType ::= Name
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewTypeName(DYNAMIC_CAST<AstName*> (Sym(1)));
}
./

--
-- These rules have been rewritten to avoid some conflicts introduced
-- by adding the 1.1 features, and to simplify syntax tree generation.
--
-- JLS1 lists:
--ArrayType ::= PrimitiveType '[' ']'
--ArrayType ::= Name '[' ']'
--ArrayType ::= ArrayType '[' ']'
-- JLS2 lists:
--ArrayType ::= Type '[' ']'
--
ArrayType ::= PrimitiveType Dims
\:$MakeArrayType:\
/.$location
void Parser::MakeArrayType() { Sym(1) = MakeArrayType(1); }

//
// Used on variants of "Type Dimsopt". If this type has dimensions, make an
// array type; otherwise return the type name.
//
AstType* Parser::MakeArrayType(int tokennum)
{
    AstType* p = Sym(tokennum) -> NameCast()
        ? ast_pool -> NewTypeName(DYNAMIC_CAST<AstName*> (Sym(tokennum)))
        : DYNAMIC_CAST<AstType*> (Sym(tokennum));
    return ! Sym(tokennum + 1) ? p
        : ast_pool -> NewArrayType(p, (DYNAMIC_CAST<AstBrackets*>
                                       (Sym(tokennum + 1))));
}
./

ArrayType ::= ClassOrInterfaceType Dims
\:$MakeArrayType:\
/.$shared_function
//
// void MakeArrayType();
//./

--
-- Simplify the syntax tree.
--
--ClassType ::= ClassOrInterfaceType
--InterfaceType ::= ClassOrInterfaceType

--18.5 Productions from 6: Names

Name ::= 'Identifier'
\:$action:\
/.$location
void Parser::Act$rule_number() { MakeSimpleName(1); }

//
// Used on "Identifier", and sets the corresponding symbol to a simple name.
//
AstName* Parser::MakeSimpleName(int tokennum)
{
    AstName* name = ast_pool -> NewName(Token(tokennum));
    Sym(tokennum) = name;
    return name;
}
./

Name ::= Name '.' 'Identifier'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstName* p = ast_pool -> NewName(Token(3));
    p -> base_opt = DYNAMIC_CAST<AstName*> (Sym(1));
    Sym(1) = p;
}
./

--18.6 Productions from 7: Packages

CompilationUnit ::= PackageDeclarationopt ImportDeclarationsopt
                    TypeDeclarationsopt
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstCompilationUnit* p = ast_pool -> NewCompilationUnit();
    p -> package_declaration_opt =
        DYNAMIC_CAST<AstPackageDeclaration*> (Sym(1));
    if (Sym(2))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(2));
        p -> AllocateImportDeclarations(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddImportDeclaration(DYNAMIC_CAST<AstImportDeclaration*>
                                      (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    if (Sym(3))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(3));
        p -> AllocateTypeDeclarations(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddTypeDeclaration(DYNAMIC_CAST<AstDeclaredType*>
                                    (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

ImportDeclarations ::= ImportDeclaration
\:$StartList:\
/.$location
//
// This starts a list containing a single element.
// Note that the list is circular so as to preserve the order of the elements.
//
void Parser::StartList()
{
    AstListNode* p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;
    Sym(1) = p;
}
./

ImportDeclarations ::= ImportDeclarations ImportDeclaration
\:$AddList2:\
/.$location
//
// This adds token 2 to an existing list.
// Note that the list is circular so as to preserve the order of the elements.
//
void Parser::AddList2()
{
    AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(1));
    AstListNode* p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;
    p -> next = tail -> next;
    tail -> next = p;
    Sym(1) = p;
}

//
// This adds token 3 to an existing list (thus, token 2 was a delimiter).
// Note that the list is circular so as to preserve the order of the elements.
//
void Parser::AddList3()
{
    AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(1));
    AstListNode* p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;
    p -> next = tail -> next;
    tail -> next = p;
    Sym(1) = p;
}
./

TypeDeclarations ::= TypeDeclaration
\:$StartList:\
/.$shared_StartList./

TypeDeclarations ::= TypeDeclarations TypeDeclaration
\:$AddList2:\
/.$shared_AddList2./

PackageDeclaration ::= 'package' Name PackageHeaderMarker ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstPackageDeclaration* p = ast_pool -> NewPackageDeclaration();
    p -> package_token = Token(1);
    p -> name = DYNAMIC_CAST<AstName*> (Sym(2));
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

ImportDeclaration ::= SingleTypeImportDeclaration
\:$NoAction:\
/.$shared_NoAction./

ImportDeclaration ::= TypeImportOnDemandDeclaration
\:$NoAction:\
/.$shared_NoAction./

--
-- Note that semantically, Name must be qualified to be valid (since simple
-- type names are not in scope). However, the grammar accepts simple names.
-- The use of Marker allows us to share code.
--
--SingleTypeImportDeclaration ::= 'import' Name ';'
SingleTypeImportDeclaration ::= 'import' Name Marker Marker ';'
\:$MakeImportDeclaration:\
/.$location
void Parser::MakeImportDeclaration()
{
    AstImportDeclaration* p = ast_pool -> NewImportDeclaration();
    p -> import_token = Token(1);
    p -> name = DYNAMIC_CAST<AstName*> (Sym(2));
    p -> star_token_opt = Token(3) == Token(4) ? 0 : Token(4);
    p -> semicolon_token = Token(5);
    Sym(1) = p;
}
./

TypeImportOnDemandDeclaration ::= 'import' Name '.' '*' ';'
\:$MakeImportDeclaration:\
/.$shared_function
//
// void MakeImportDeclaration();
//./

TypeDeclaration ::= ClassDeclaration
\:$NoAction:\
/.$shared_NoAction./

TypeDeclaration ::= InterfaceDeclaration
\:$NoAction:\
/.$shared_NoAction./

TypeDeclaration ::= ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}
./

--18.7 Only in the LALR(1) Grammar
-- Remember that we do semantic filtering on modifiers, for every context
-- they can appear in. For better error messages, we also accept all modifiers
-- for initializer blocks, formal parameters, and local variable declarations.
--
--ClassModifiers ::= Modifiers
--FieldModifiers ::= Modifiers
--MethodModifiers ::= Modifiers
--ConstructorModifiers ::= Modifiers
--InterfaceModifiers ::= Modifiers
--ConstantModifiers ::= Modifiers
--AbstractMethodModifiers ::= Modifiers
Modifiers ::= Modifier
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstModifiers* p = ast_pool -> NewModifier(Token(1));
    if (lex_stream -> Kind(Token(1)) == TK_static)
        p -> static_token_opt = Token(1);
    Sym(1) = p;
}
./

Modifiers ::= Modifiers Modifier
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstModifiers* p = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> right_modifier_token = Token(2);
    if (! p -> static_token_opt && lex_stream -> Kind(Token(2)) == TK_static)
        p -> static_token_opt = Token(1);
}
./

Modifier ::= 'public'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'protected'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'private'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'static'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'abstract'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'final'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'native'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'strictfp'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'synchronized'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'transient'
\:$NoAction:\
/.$shared_NoAction./

Modifier ::= 'volatile'
\:$NoAction:\
/.$shared_NoAction./

--18.8 Productions from 8: Class Declarations
--18.8.1 Productions from 8.1: Class Declarations

--
-- The use of Marker is in anticipation of implementing generics.
--
--ClassDeclaration ::= ClassModifiersopt 'class' 'Identifier' Superopt
--                     Interfacesopt ClassBody
ClassDeclaration ::= Modifiersopt 'class' 'Identifier' Marker Superopt
                     Interfacesopt ClassBody
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstClassDeclaration* p = ast_pool -> NewClassDeclaration();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> class_token = Token(2);
    p -> super_opt = DYNAMIC_CAST<AstTypeName*> (Sym(5));
    if (Sym(6))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(6));
        p -> AllocateInterfaces(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddInterface(DYNAMIC_CAST<AstTypeName*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> class_body = DYNAMIC_CAST<AstClassBody*> (Sym(7));
    p -> class_body -> identifier_token = Token(3);
    p -> class_body -> owner = p;
    Sym(1) = p;
}
./

--
-- Simplify.
--
--Super ::= 'extends' ClassType
Super ::= 'extends' ClassOrInterfaceType
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2();
//./

--
-- Simplify.
--
--Interfaces ::= 'implements' InterfaceTypeList
Interfaces ::= 'implements' TypeList
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2();
//./

--InterfaceTypeList ::= InterfaceType
--ClassTypeList ::= ClassType
TypeList ::= ClassOrInterfaceType
\:$StartList:\
/.$shared_StartList./

--InterfaceTypeList ::= InterfaceTypeList ',' InterfaceType
--ClassTypeList ::= ClassTypeList ',' ClassType
TypeList ::= TypeList ',' ClassOrInterfaceType
\:$AddList3:\
/.$shared_AddList3./

ClassBody ::= '{' ClassBodyDeclarationsopt '}'
\:$MakeClassBody:\
/.$location
void Parser::MakeClassBody()
{
    AstClassBody* p = ast_pool -> NewClassBody();
    if (parse_header_only)
        p -> MarkUnparsed();

    p -> left_brace_token = Token(1);
    if (Sym(2))
    {
        int num_instance_variables = 0,
            num_class_variables = 0,
            num_methods = 0,
            num_constructors = 0,
            num_static_initializers = 0,
            num_instance_initializers = 0,
            num_inner_classes = 0,
            num_inner_interfaces = 0,
            num_empty_declarations = 0;

        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(2));
        p -> AllocateClassBodyDeclarations(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            AstDeclared* declaration =
                DYNAMIC_CAST<AstDeclared*> (root -> element);
            p -> AddClassBodyDeclaration(declaration);
            AstFieldDeclaration* field_declaration =
                declaration -> FieldDeclarationCast();
            AstInitializerDeclaration* initializer =
                declaration -> InitializerDeclarationCast();
            if (field_declaration)
            {
                if (field_declaration -> modifiers_opt &&
                    field_declaration -> modifiers_opt -> static_token_opt)
                {
                    field_declaration -> MarkStatic();
                }
                //
                // Interface fields were already marked static.
                //
                if (field_declaration -> StaticFieldCast())
                    num_class_variables++;
                else num_instance_variables++;
            }
            else if (declaration -> MethodDeclarationCast())
                num_methods++;
            else if (declaration -> ConstructorDeclarationCast())
                num_constructors++;
            else if (initializer)
            {
                if (initializer -> modifiers_opt &&
                    initializer -> modifiers_opt -> static_token_opt)
                {
                    initializer -> MarkStatic();
                    num_static_initializers++;
                }
                else num_instance_initializers++;
            }
            else if (declaration -> ClassDeclarationCast())
                num_inner_classes++;
            else if (declaration -> InterfaceDeclarationCast())
                num_inner_interfaces++;
            else num_empty_declarations++;
        } while (root != tail);

        p -> AllocateInstanceVariables(num_instance_variables);
        p -> AllocateClassVariables(num_class_variables);
        p -> AllocateMethods(num_methods);
        p -> AllocateConstructors(num_constructors);
        p -> AllocateStaticInitializers(num_static_initializers);
        p -> AllocateInstanceInitializers(num_instance_initializers);
        p -> AllocateNestedClasses(num_inner_classes);
        p -> AllocateNestedInterfaces(num_inner_interfaces);
        p -> AllocateEmptyDeclarations(num_empty_declarations);

        root = tail;
        do
        {
            root = root -> next;
            AstDeclared* declaration =
                DYNAMIC_CAST<AstDeclared*> (root -> element);
            AstFieldDeclaration* field_declaration =
                declaration -> FieldDeclarationCast();
            AstMethodDeclaration* method_declaration =
                declaration -> MethodDeclarationCast();
            AstConstructorDeclaration* constructor_declaration =
                declaration -> ConstructorDeclarationCast();
            AstInitializerDeclaration* initializer =
                declaration -> InitializerDeclarationCast();
            AstClassDeclaration* class_declaration =
                declaration -> ClassDeclarationCast();
            AstInterfaceDeclaration* interface_declaration =
                declaration -> InterfaceDeclarationCast();

            if (field_declaration)
            {
                if (field_declaration -> StaticFieldCast())
                    p -> AddClassVariable(field_declaration);
                else p -> AddInstanceVariable(field_declaration);
            }
            else if (method_declaration)
                p -> AddMethod(method_declaration);
            else if (constructor_declaration)
                p -> AddConstructor(constructor_declaration);
            else if (initializer)
            {
                if (initializer -> StaticInitializerCast())
                     p -> AddStaticInitializer(initializer);
                else p -> AddInstanceInitializer(initializer);
            }
            else if (class_declaration)
                p -> AddNestedClass(class_declaration);
            else if (interface_declaration)
                p -> AddNestedInterface(interface_declaration);
            else
            {
                p -> AddEmptyDeclaration(DYNAMIC_CAST<AstEmptyDeclaration*>
                                         (root -> element));
            }
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token = Token(3);
    // from now on, this is the storage pool to use for this type
    p -> pool = body_pool;
    Sym(1) = p;
}
./

ClassBodyDeclarations ::= ClassBodyDeclaration
\:$StartList:\
/.$shared_StartList./

ClassBodyDeclarations ::= ClassBodyDeclarations ClassBodyDeclaration
\:$AddList2:\
/.$shared_AddList2./

--
-- For nicer semantic error messages, we treat class and interface
-- members identically, giving errors if an interface forgets a field
-- initializer or adds a method body.
--
--ClassBodyDeclaration ::= ClassMemberDeclaration
ClassBodyDeclaration ::= MemberDeclaration
\:$NoAction:\
/.$shared_NoAction./

ClassBodyDeclaration ::= ConstructorDeclaration
\:$NoAction:\
/.$shared_NoAction./

--
-- For nicer semantic error messages, we lump static and instance initializers
-- together. Also, we parse arbitrary modifiers, but semantically only accept
-- static or no modifiers.
--
--ClassBodyDeclaration ::= StaticInitializer
--ClassBodyDeclaration ::= MethodBody
ClassBodyDeclaration ::= InitializerDeclaration
\:$NoAction:\
/.$shared_NoAction./

--ClassMemberDeclaration ::= FieldDeclaration
MemberDeclaration ::= FieldDeclaration
\:$NoAction:\
/.$shared_NoAction./

--ClassMemberDeclaration ::= MethodDeclaration
MemberDeclaration ::= MethodDeclaration
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
--
-- Consolidate.
--ClassMemberDeclaration ::= ClassDeclaration
--ClassMemberDeclaration ::= InterfaceDeclaration
--ClassMemberDeclaration ::= ';'
--
MemberDeclaration ::= TypeDeclaration
\:$NoAction:\
/.$shared_NoAction./

--18.8.2 Productions from 8.3: Field Declarations

--
-- The use of Marker allows us to share code.
--
--FieldDeclaration ::= FieldModifiersopt Type VariableDeclarators ';'
FieldDeclaration ::= Modifiersopt Marker Type VariableDeclarators ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstFieldDeclaration* p = ast_pool -> NewFieldDeclaration();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> type = DYNAMIC_CAST<AstType*> (Sym(3));
    AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(4));
    p -> AllocateVariableDeclarators(tail -> index + 1);
    AstListNode* root = tail;
    do
    {
        root = root -> next;
        p -> AddVariableDeclarator(DYNAMIC_CAST<AstVariableDeclarator*>
                                   (root -> element));
    } while (root != tail);
    FreeCircularList(tail);
    p -> semicolon_token = Token(5);
    Sym(1) = p;
}
./

VariableDeclarators ::= VariableDeclarator
\:$StartList:\
/.$shared_StartList./

VariableDeclarators ::= VariableDeclarators ',' VariableDeclarator
\:$AddList3:\
/.$shared_AddList3./

VariableDeclarator ::= VariableDeclaratorId
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstVariableDeclarator* p = ast_pool -> NewVariableDeclarator();
    p -> variable_declarator_name =
        DYNAMIC_CAST<AstVariableDeclaratorId*> (Sym(1));
    Sym(1) = p;
}
./

VariableDeclarator ::= VariableDeclaratorId '=' VariableInitializer
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstVariableDeclarator* p = ast_pool -> NewVariableDeclarator();
    p -> variable_declarator_name =
        DYNAMIC_CAST<AstVariableDeclaratorId*> (Sym(1));
    p -> variable_initializer_opt = Sym(3);
    Sym(1) = p;
}
./

VariableDeclaratorId ::= 'Identifier' Dimsopt
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstVariableDeclaratorId* p = ast_pool -> NewVariableDeclaratorId();
    p -> identifier_token = Token(1);
    p -> brackets_opt = DYNAMIC_CAST<AstBrackets*> (Sym(2));
    Sym(1) = p;
}
./

VariableInitializer ::= Expression
\:$NoAction:\
/.$shared_NoAction./

VariableInitializer ::= ArrayInitializer
\:$NoAction:\
/.$shared_NoAction./

--18.8.3 Productions from 8.4: Method Declarations
--
-- We use "MethodHeaderMarker" to speed up parsing while minimizing memory.
-- During the first pass, we only care about declarations, so we skip
-- everything inside { }. On the second pass, we parse only one method at a
-- time (see the production of Goal above).
--
-- Also, we expanded MethodBody inline to enable the sharing of MethodBody
-- between methods, constructors, and initializers. Note that MethodBody
-- can support an explicit constructor call; so it requires semantic filtering.
--
--MethodDeclaration ::= MethodHeader MethodBody
MethodDeclaration ::= MethodHeader MethodHeaderMarker MethodBody
\:$MakeMethodDeclaration:\
/.$location
void Parser::MakeMethodDeclaration()
{
    DYNAMIC_CAST<AstMethodDeclaration*> (Sym(1)) -> method_body_opt =
        DYNAMIC_CAST<AstMethodBody*> (Sym(3));
}
./

--
-- The use of Marker allows us to share code.
--
MethodDeclaration ::= MethodHeader MethodHeaderMarker Marker ';'
\:$MakeMethodDeclaration:\
/.$shared_function
//
// void MakeMethodDeclaration();
//./

--
-- The use of Marker allows us to share code.
--
--MethodHeader ::= MethodModifiersopt Type MethodDeclarator Throwsopt
MethodHeader ::= Modifiersopt Marker Type MethodDeclarator Throwsopt
\:$MakeMethodHeader:\
/.$location
void Parser::MakeMethodHeader()
{
    AstMethodDeclaration* p = ast_pool -> NewMethodDeclaration();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> type = DYNAMIC_CAST<AstType*> (Sym(3));
    p -> method_declarator = DYNAMIC_CAST<AstMethodDeclarator*> (Sym(4));
    if (Sym(5))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(5));
        p -> AllocateThrows(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddThrow(DYNAMIC_CAST<AstTypeName*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code.
--
--MethodHeader ::= MethodModifiersopt 'void' MethodDeclarator Throwsopt
MethodHeader ::= Modifiersopt Marker 'void' MethodDeclarator Throwsopt
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(3) = ast_pool -> NewPrimitiveType(Ast::VOID_TYPE, Token(3));
    MakeMethodHeader();
}
./

MethodDeclarator ::= 'Identifier' '(' FormalParameterListopt ')' Dimsopt
\:$MakeMethodDeclarator:\
/.$location
void Parser::MakeMethodDeclarator()
{
    AstMethodDeclarator* p = ast_pool -> NewMethodDeclarator();
    p -> identifier_token = Token(1);
    p -> left_parenthesis_token = Token(2);
    if (Sym(3))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(3));
        p -> AllocateFormalParameters(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddFormalParameter(DYNAMIC_CAST<AstFormalParameter*>
                                    (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(4);
    p -> brackets_opt = DYNAMIC_CAST<AstBrackets*> (Sym(5));
    Sym(1) = p;
}
./

FormalParameterList ::= FormalParameter
\:$StartList:\
/.$shared_StartList./

FormalParameterList ::= FormalParameterList ',' FormalParameter
\:$AddList3:\
/.$shared_AddList3./

--
-- For nicer error messages, we accept all modifiers, even though only
-- 'final' is valid.
--
--FormalParameter ::= finalopt Type VariableDeclaratorId
FormalParameter ::= Modifiersopt Type VariableDeclaratorId
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstFormalParameter* p = ast_pool -> NewFormalParameter();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> type = DYNAMIC_CAST<AstType*> (Sym(2));
    AstVariableDeclarator* formal_declarator =
        ast_pool -> NewVariableDeclarator();
    formal_declarator -> variable_declarator_name =
        DYNAMIC_CAST<AstVariableDeclaratorId*> (Sym(3));
    p -> formal_declarator = formal_declarator;
    Sym(1) = p;
}
./

--
-- Simplify.
--
--Throws ::= 'throws' ClassTypeList
Throws ::= 'throws' TypeList
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2();
//./

--
-- Notice that we filter out an initial explicit constructor invocation,
-- since we have modified Statement to include this() and super(). Other
-- explicit constructor calls are ignored now, and flagged as errors
-- during semantic analysis.
--
MethodBody ::= '{' BlockStatementsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstMethodBody* p = ast_pool -> NewMethodBody();
    p -> left_brace_token = Token(1);
    p -> right_brace_token = Token(3);

    if (Sym(2))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(2));
        // Allocate 1 extra for possible generated return statement.
        p -> AllocateStatements(tail -> index + 2);
        AstListNode* root = tail -> next;
        if (root -> element -> IsExplicitConstructorInvocation())
            p -> explicit_constructor_opt =
                DYNAMIC_CAST<AstStatement*> (root -> element);
        else
            p -> AddStatement(DYNAMIC_CAST<AstStatement*> (root -> element));
        while (root != tail)
        {
            root = root -> next;
            p -> AddStatement(DYNAMIC_CAST<AstStatement*> (root -> element));
        }
        FreeCircularList(tail);
    }
    else p -> AllocateStatements(1);
    Sym(1) = p;
}
./

--
-- Instead of directly including this rule, we have expanded it inline above.
--
--MethodBody ::= ';'
--

--18.8.4 Productions from 8.5: Static Initializers
--
-- For nicer error messages, we accept arbitrary modifiers. Thus this rule can
-- parse static and instance initializers. The use of Marker allows us to
-- share code, and MethodHeaderMarker allows the 2-pass parsing. See
-- comments of MethodDeclaration.
--
--StaticInitializer ::= 'static' MethodBody
InitializerDeclaration ::= Modifiersopt Marker MethodHeaderMarker MethodBody
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstInitializerDeclaration* p = ast_pool -> NewInitializerDeclaration();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> block = DYNAMIC_CAST<AstMethodBody*> (Sym(4));
    Sym(1) = p;
}
./

--18.8.5 Productions from 8.6: Constructor Declarations
--
-- Rewritten to implement generics. The use of Marker allows us to share code,
-- MethodHeaderMarker allows us to do 2-pass parsing, and MethodBody was
-- rewritten to handle constructor bodies. See comments above.
--
--ConstructorDeclaration ::= ConstructorModifiersopt ConstructorDeclarator
--                           Throwsopt ConstructorBody
ConstructorDeclaration ::= Modifiersopt Marker ConstructorDeclarator
                           Throwsopt MethodHeaderMarker MethodBody
\:$MakeConstructorDeclaration:\
/.$location
void Parser::MakeConstructorDeclaration()
{
    AstConstructorDeclaration* p = ast_pool -> NewConstructorDeclaration();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> constructor_declarator = DYNAMIC_CAST<AstMethodDeclarator*> (Sym(3));
    if (Sym(4))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(4));
        p -> AllocateThrows(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddThrow(DYNAMIC_CAST<AstTypeName*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> constructor_body = DYNAMIC_CAST<AstMethodBody*> (Sym(6));

    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code. Also, we got rid of SimpleName.
--
--ConstructorDeclarator ::= SimpleName '(' FormalParameterListopt ')'
ConstructorDeclarator ::= 'Identifier' '(' FormalParameterListopt ')' Marker
\:$MakeMethodDeclarator:\
/.$shared_function
//
// void MakeMethodDeclarator();
//./

--
-- For better error reporting, we have coalesced ExplicitConstructorInvocation
-- into BlockStatement. Therefore, we do not need a rule for ConstructorBody,
-- since MethodBody does the same amount of work. During semantic analysis,
-- we then check calls of an explicit constructor invocation out of context.
--
--ConstructorBody ::= '{' ExplicitConstructorInvocationopt
--                    BlockStatementsopt '}'
--

ExplicitConstructorInvocation ::= 'this' '(' ArgumentListopt ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstThisCall* p = ast_pool -> NewThisCall();
    p -> this_token = Token(1);
    p -> arguments = MakeArguments(2);
    p -> semicolon_token = Token(5);
    Sym(1) = p;
}

AstArguments* Parser::MakeArguments(int tokennum)
{
    AstArguments* p = ast_pool -> NewArguments(Token(tokennum),
                                               Token(tokennum + 2));
    if (Sym(tokennum + 1))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(tokennum + 1));
        p -> AllocateArguments(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument(DYNAMIC_CAST<AstExpression*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    return p;
}
./

ExplicitConstructorInvocation ::= 'super' '(' ArgumentListopt ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstSuperCall* p = ast_pool -> NewSuperCall();
    p -> super_token = Token(1);
    p -> arguments = MakeArguments(2);
    p -> semicolon_token = Token(5);
    Sym(1) = p;
}
./

--1.1 feature
ExplicitConstructorInvocation ::= Primary '.' 'super' '(' ArgumentListopt ')'
                                  ';'
\:$MakeQualifiedSuper:\
/.$location
void Parser::MakeQualifiedSuper()
{
    AstSuperCall* p = ast_pool -> NewSuperCall();
    p -> base_opt = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> super_token = Token(3);
    p -> arguments = MakeArguments(4);
    p -> semicolon_token = Token(7);
    Sym(1) = p;
}
./

--1.1 feature
ExplicitConstructorInvocation ::= Name '.' 'super' '(' ArgumentListopt ')' ';'
\:$MakeQualifiedSuper:\
/.$shared_function
//
// void MakeQualifiedSuper();
//./

--18.9 Productions from 9: Interface Declarations
--18.9.1 Productions from 9.1: Interface Declarations
--
-- The use of Marker is in anticipation of implementing generics.
--
--InterfaceDeclaration ::= InterfaceModifiersopt 'interface' 'Identifier'
--                         ExtendsInterfacesopt InterfaceBody
InterfaceDeclaration ::= Modifiersopt 'interface' 'Identifier' Marker
                         ExtendsInterfacesopt InterfaceBody
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstInterfaceDeclaration* p = ast_pool -> NewInterfaceDeclaration();
    p -> modifiers_opt = DYNAMIC_CAST<AstModifiers*> (Sym(1));
    p -> interface_token = Token(2);
    if (Sym(5))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(5));
        p -> AllocateInterfaces(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddInterface(DYNAMIC_CAST<AstTypeName*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> class_body = DYNAMIC_CAST<AstClassBody*> (Sym(6));
    p -> class_body -> identifier_token = Token(3);
    p -> class_body -> owner = p;
    Sym(1) = p;
}
./

--
-- Simplify.
--
--ExtendsInterfaces ::= 'extends' InterfaceTypeList
ExtendsInterfaces ::= 'extends' TypeList
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2();
//./

InterfaceBody ::= '{' InterfaceMemberDeclarationsopt '}'
\:$MakeClassBody:\
/.$shared_function
//
// void MakeClassBody();
//./

--
-- For less code duplication and better semantic messages, we treat all
-- interface members as class members now, then do a semantic check that
-- this was valid.
--
--InterfaceMemberDeclarations ::= InterfaceMemberDeclaration
InterfaceMemberDeclarations ::= MemberDeclaration
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstFieldDeclaration* field = Sym(1) -> FieldDeclarationCast();
    if (field)
        field -> MarkStatic();
    StartList();
}
./

--InterfaceMemberDeclarations ::= InterfaceMemberDeclarations
--                                InterfaceMemberDeclaration
InterfaceMemberDeclarations ::= InterfaceMemberDeclarations
                                MemberDeclaration
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstFieldDeclaration* field = Sym(2) -> FieldDeclarationCast();
    if (field)
        field -> MarkStatic();
    AddList2();
}
./

--
-- See description of MemberDeclaration.
--
--InterfaceMemberDeclaration ::= ConstantDeclaration
--InterfaceMemberDeclaration ::= AbstractMethodDeclaration
--InterfaceMemberDeclaration ::= ClassDeclaration
--InterfaceMemberDeclaration ::= InterfaceDeclaration
--InterfaceMemberDeclaration ::= ';'
--ConstantDeclaration ::= FieldDeclaration
--AbstractMethodDeclaration ::= MethodHeader ';'

--18.10 Productions from 10: Arrays
--
-- NOTE that the rule VariableInitializersopt was expanded inline below
-- to make the grammar lalr(1). The use of Marker allows us to share code.
--
-- ArrayInitializer ::= '{' VariableInitializersopt ,opt '}'
ArrayInitializer ::= '{' Marker ,opt '}'
\:$MakeArrayInitializer:\
/.$location
void Parser::MakeArrayInitializer()
{
    AstArrayInitializer* p = ast_pool -> NewArrayInitializer();
    p -> left_brace_token = Token(1);
    if (Sym(2))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(2));
        p -> AllocateVariableInitializers(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableInitializer(root -> element);
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token = Token(4);
    Sym(1) = p;
}
./

ArrayInitializer ::= '{' VariableInitializers ,opt '}'
\:$MakeArrayInitializer:\
/.$shared_function
//
// void MakeArrayInitializer();
//./

VariableInitializers ::= VariableInitializer
\:$StartList:\
/.$shared_StartList./

VariableInitializers ::= VariableInitializers ',' VariableInitializer
\:$AddList3:\
/.$shared_AddList3./

--18.11 Productions from 13: Blocks and Statements

Block ::= '{' BlockStatementsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstBlock* p = ast_pool -> NewBlock();
    p -> left_brace_token = Token(1);
    if (Sym(2))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(2));
        p -> AllocateStatements(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddStatement(DYNAMIC_CAST<AstStatement*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token = Token(3);
    Sym(1) = p;
}
./

BlockStatements ::= BlockStatement
\:$StartList:\
/.$shared_StartList./

BlockStatements ::= BlockStatements BlockStatement
\:$AddList2:\
/.$shared_AddList2./

BlockStatement ::= LocalVariableDeclarationStatement
\:$NoAction:\
/.$shared_NoAction./

BlockStatement ::= Statement
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
BlockStatement ::= ClassDeclaration
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool ->
        NewLocalClassDeclarationStatement(DYNAMIC_CAST<AstClassDeclaration*>
                                          (Sym(1)));
}
./

--
-- NOTE: This rule is not in the original grammar. We added it, and changed
-- the rule for ConstructorBody, in order to issue a nicer error message
-- when this() or super() is encountered out of context.
--
BlockStatement ::= ExplicitConstructorInvocation
\:$NoAction:\
/.$shared_NoAction./

LocalVariableDeclarationStatement ::= LocalVariableDeclaration ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    DYNAMIC_CAST<AstLocalVariableDeclarationStatement*> (Sym(1)) ->
        semicolon_token_opt = Token(2);
}
./

--
-- To separate Type vs. Name ambiguities, we have to expand this inline.
--
--LocalVariableDeclaration ::= Type VariableDeclarators
LocalVariableDeclaration ::= PrimitiveType Dimsopt VariableDeclarators
\:$MakeLocalVariable:\
/.$location
void Parser::MakeLocalVariable()
{
    MakeLocalVariable(NULL, MakeArrayType(1),
                      DYNAMIC_CAST<AstListNode*> (Sym(3)));
}

//
// Creates a local variable declaration and places it in Sym(1).
//
void Parser::MakeLocalVariable(AstModifiers* modifiers, AstType* type,
                               AstListNode* variables)
{
    AstLocalVariableDeclarationStatement* p =
        ast_pool -> NewLocalVariableDeclarationStatement();
    p -> modifiers_opt = modifiers;
    p -> type = type;
    AstListNode* tail = variables;
    p -> AllocateVariableDeclarators(tail -> index + 1);
    AstListNode* root = tail;
    do
    {
        root = root -> next;
        p -> AddVariableDeclarator(DYNAMIC_CAST<AstVariableDeclarator*>
                                   (root -> element));
    } while (root != tail);
    FreeCircularList(tail);
    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code.
--
--LocalVariableDeclaration ::= Name VariableDeclarators
LocalVariableDeclaration ::= Name Marker VariableDeclarators
\:$MakeLocalVariable:\
/.$shared_function
//
// void MakeLocalVariable();
//./

LocalVariableDeclaration ::= Name Dims VariableDeclarators
\:$MakeLocalVariable:\
/.$shared_function
//
// void MakeLocalVariable();
//./

--1.1 feature
--
-- For nicer error messages, we accept all modifiers, even though only
-- 'final' is valid.
--
--LocalVariableDeclaration ::= finalopt Type VariableDeclarators
LocalVariableDeclaration ::= Modifiers Type VariableDeclarators
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    MakeLocalVariable(DYNAMIC_CAST<AstModifiers*> (Sym(1)),
                      DYNAMIC_CAST<AstType*> (Sym(2)),
                      DYNAMIC_CAST<AstListNode*> (Sym(3)));
}
./

Statement ::= StatementWithoutTrailingSubstatement
\:$NoAction:\
/.$shared_NoAction./

Statement ::= LabeledStatement
\:$NoAction:\
/.$shared_NoAction./

Statement ::= IfThenStatement
\:$NoAction:\
/.$shared_NoAction./

Statement ::= IfThenElseStatement
\:$NoAction:\
/.$shared_NoAction./

Statement ::= WhileStatement
\:$NoAction:\
/.$shared_NoAction./

Statement ::= ForStatement
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf ::= StatementWithoutTrailingSubstatement
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf ::= LabeledStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf ::= IfThenElseStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf ::= WhileStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf ::= ForStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= Block
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= EmptyStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= ExpressionStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= SwitchStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= DoStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= BreakStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= ContinueStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= ReturnStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= SynchronizedStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= ThrowStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= TryStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement ::= AssertStatement
\:$NoAction:\
/.$shared_NoAction./

EmptyStatement ::= ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewEmptyStatement(Token(1));
}
./

LabeledStatement ::= 'Identifier' ':' Statement
\:$MakeLabeledStatement:\
/.$location
void Parser::MakeLabeledStatement()
{
    AstBlock* p = Sym(3) -> BlockCast();

    if (! p || p -> label_opt)
    {
        //
        // When a statement is labeled, it is enclosed in a block.
        // This is necessary in order to allow the same name to be
        // reused to label a subsequent statement at the same nesting
        // level... See ProcessBlock, ProcessStatement,...
        //
        p = ast_pool -> GenBlock();
        p -> AllocateStatements(1); // allocate 1 element
        p -> left_brace_token = Token(1);
        p -> AddStatement(DYNAMIC_CAST<AstStatement*> (Sym(3)));
        p -> right_brace_token = Sym(3) -> RightToken();
    }

    p -> label_opt = Token(1); // add label to statement
    Sym(1) = p; // The final result is a block containing the labeled-statement
}
./

LabeledStatementNoShortIf ::= 'Identifier' ':' StatementNoShortIf
\:$MakeLabeledStatement:\
/.$shared_function
//
// void MakeLabeledStatement();
//./

ExpressionStatement ::= StatementExpression ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    DYNAMIC_CAST<AstExpressionStatement*> (Sym(1)) -> semicolon_token_opt =
        Token(2);
}
./

StatementExpression ::= Assignment
\:$MakeExpressionStatement:\
/.$location
void Parser::MakeExpressionStatement()
{
    AstExpressionStatement* p = ast_pool -> NewExpressionStatement();
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(1));
    Sym(1) = p;
}
./

StatementExpression ::= PreIncrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement();
//./

StatementExpression ::= PreDecrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement();
//./

StatementExpression ::= PostIncrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement();
//./

StatementExpression ::= PostDecrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement();
//./

StatementExpression ::= MethodInvocation
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement();
//./

StatementExpression ::= ClassInstanceCreationExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement();
//./

--
-- The use of Marker allows us to share code.
--
--IfThenStatement ::= 'if' '(' Expression ')' Statement
IfThenStatement ::= 'if' '(' Expression ')' Statement Marker Marker
\:$MakeIfThenElseStatement:\
/.$location
void Parser::MakeIfThenElseStatement()
{
    //
    // We wrap the true and false statements in a block, to make the semantic
    // pass easier.
    //
    AstIfStatement* p = ast_pool -> NewIfStatement();
    p -> if_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    p -> true_statement = MakeBlock(5);
    p -> false_statement_opt = Sym(7) ? MakeBlock(7) : NULL;
    Sym(1) = p;
}

AstBlock* Parser::MakeBlock(int tokennum)
{
    AstBlock* block = Sym(tokennum) -> BlockCast();
    if (! block)
    {
        block = ast_pool -> GenBlock();
        block -> AllocateStatements(1); // allocate 1 element
        block -> left_brace_token = Token(tokennum);
        block -> AddStatement(DYNAMIC_CAST<AstStatement*> (Sym(tokennum)));
        block -> right_brace_token = Sym(tokennum) -> RightToken();
    }
    return block;
}
./

IfThenElseStatement ::= 'if' '(' Expression ')' StatementNoShortIf
                        'else' Statement
\:$MakeIfThenElseStatement:\
/.$shared_function
//
// void MakeIfThenElseStatement();
//./

IfThenElseStatementNoShortIf ::= 'if' '(' Expression ')' StatementNoShortIf
                                 'else' StatementNoShortIf
\:$MakeIfThenElseStatement:\
/.$shared_function
//
// void MakeIfThenElseStatement();
//./

SwitchStatement ::= 'switch' '(' Expression ')' SwitchBlock
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstSwitchStatement* p = DYNAMIC_CAST<AstSwitchStatement*> (Sym(5));
    p -> switch_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    Sym(1) = p;
}
./

--
-- To avoid ambiguity with consecutive optional items, and to special
-- case trailing labels, we expand this inline.
--
--SwitchBlock ::= '{' SwitchBlockStatementsopt SwitchLabelsopt '}'
--
SwitchBlock ::= '{' SwitchBlockStatements SwitchLabelsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstSwitchStatement* p = ast_pool -> NewSwitchStatement();
    AstBlock* block = ast_pool -> NewBlock();
    block -> left_brace_token = Token(1);
    AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(2));
    block -> AllocateStatements(tail -> index + (Sym(3) ? 2 : 1));
    AstListNode* root = tail;
    do
    {
        root = root -> next;
        block -> AddStatement(DYNAMIC_CAST<AstStatement*> (root -> element));
    } while (root != tail);
    FreeCircularList(tail);
    if (Sym(3))
        block -> AddStatement
            (MakeSwitchBlockStatement(DYNAMIC_CAST<AstListNode*> (Sym(3))));
    block -> right_brace_token = Token(4);
    block -> block_tag = AstBlock::SWITCH;
    p -> switch_block = block;
    Sym(1) = p;
}
./

SwitchBlock ::= '{' SwitchLabelsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstSwitchStatement* p = ast_pool -> NewSwitchStatement();
    AstBlock* block = ast_pool -> NewBlock();
    block -> AllocateStatements(1); // allocate 1 element
    block -> left_brace_token = Token(1);
    if (Sym(2))
        block -> AddStatement
            (MakeSwitchBlockStatement(DYNAMIC_CAST<AstListNode*> (Sym(2))));
    block -> right_brace_token = Token(3);
    block -> block_tag = AstBlock::SWITCH;

    p -> switch_block = block;

    Sym(1) = p;
}
./

SwitchBlockStatements ::= SwitchBlockStatement
\:$StartList:\
/.$shared_StartList./

SwitchBlockStatements ::= SwitchBlockStatements SwitchBlockStatement
\:$AddList2:\
/.$shared_AddList2./

SwitchBlockStatement ::= SwitchLabels BlockStatements
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = MakeSwitchBlockStatement(DYNAMIC_CAST<AstListNode*> (Sym(1)),
                                      DYNAMIC_CAST<AstListNode*> (Sym(2)));
}

AstStatement* Parser::MakeSwitchBlockStatement(AstListNode* labels,
                                               AstListNode* statements)
{
    AstSwitchBlockStatement* p = ast_pool -> NewSwitchBlockStatement();
    assert(labels);
    AstListNode* tail = labels;
    p -> AllocateSwitchLabels(tail -> index + 1);
    AstListNode* root = tail;
    do
    {
        root = root -> next;
        p -> AddSwitchLabel(DYNAMIC_CAST<AstSwitchLabel*> (root -> element));
    } while (root != tail);
    FreeCircularList(tail);
    if (statements)
    {
        tail = statements;
        p -> AllocateStatements(tail -> index + 1);
        root = tail;
        do
        {
            root = root -> next;
            p -> AddStatement(DYNAMIC_CAST<AstStatement*> (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    else
    {
        p -> AllocateStatements(1);
        p -> AddStatement(ast_pool -> GenEmptyStatement(labels ->
                                                        RightToken()));
    }
    p -> right_brace_token =
        p -> Statement(p -> NumStatements() - 1) -> RightToken();
    return p;
}
./

SwitchLabels ::= SwitchLabel
\:$StartList:\
/.$shared_StartList./

SwitchLabels ::= SwitchLabels SwitchLabel
\:$AddList2:\
/.$shared_AddList2./

--
-- Simplify.
--
--SwitchLabel ::= 'case' ConstantExpression ':'
SwitchLabel ::= 'case' Expression ':'
\:$MakeSwitchLabel:\
/.$location
void Parser::MakeSwitchLabel()
{
    AstSwitchLabel* p = ast_pool -> NewSwitchLabel();
    p -> case_token = Token(1);
    p -> expression_opt = DYNAMIC_CAST<AstExpression*> (Sym(2));
    p -> colon_token = Token(3);
    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code.
--
--SwitchLabel ::= 'default' ':'
SwitchLabel ::= 'default' Marker ':'
\:$MakeSwitchLabel:\
/.$shared_function
//
// void MakeSwitchLabel();
//./

WhileStatement ::= 'while' '(' Expression ')' Statement
\:$MakeWhileStatement:\
/.$location
void Parser::MakeWhileStatement()
{
    //
    // We wrap the loop statement in a block, to make the semantic pass easier.
    //
    AstWhileStatement* p = ast_pool -> NewWhileStatement();
    p -> while_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    p -> statement = MakeBlock(5);

    //
    // We also wrap the loop in a block, to make the semantic pass easier.
    //
    AstBlock* block = ast_pool -> GenBlock();
    block -> AllocateStatements(1); // allocate 1 element
    block -> left_brace_token = Token(1);
    block -> AddStatement(p);
    block -> right_brace_token = Sym(5) -> RightToken();
    Sym(1) = block;
}
./

WhileStatementNoShortIf ::= 'while' '(' Expression ')' StatementNoShortIf
\:$MakeWhileStatement:\
/.$shared_function
//
// void MakeWhileStatement();
//./

DoStatement ::= 'do' Statement 'while' '(' Expression ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    //
    // We wrap the loop statement in a block, to make the semantic pass easier.
    //
    AstDoStatement* p = ast_pool -> NewDoStatement();
    p -> do_token = Token(1);
    p -> statement = MakeBlock(2);
    p -> while_token = Token(3);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(5));
    p -> semicolon_token = Token(7);

    //
    // We also wrap the loop in a block, to make the semantic pass easier.
    //
    AstBlock* block = ast_pool -> GenBlock();
    block -> AllocateStatements(1); // allocate 1 element
    block -> left_brace_token = Token(1);
    block -> AddStatement(p);
    block -> right_brace_token = Token(7);
    Sym(1) = block;
}
./

ForStatement ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')'
                 Statement
\:$MakeForStatement:\
/.$location
void Parser::MakeForStatement()
{
    //
    // We wrap the loop statement in a block, to make the semantic pass easier.
    //
    AstForStatement* p = ast_pool -> NewForStatement();
    p -> for_token = Token(1);
    if (Sym(3))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(3));
        p -> AllocateForInitStatements(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddForInitStatement(DYNAMIC_CAST<AstStatement*>
                                     (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> end_expression_opt = DYNAMIC_CAST<AstExpression*> (Sym(5));
    if (Sym(7))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(7));
        p -> AllocateForUpdateStatements(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddForUpdateStatement(DYNAMIC_CAST<AstExpressionStatement*>
                                       (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    p -> statement = MakeBlock(9);

    //
    // We also wrap the loop in a block, to make the semantic pass easier. In
    // particular, this lets us correctly handle "for(int i;;);for(int i;;);".
    //
    AstBlock* block = ast_pool -> NewBlock();
    block -> AllocateStatements(1); // allocate 1 element
    block -> left_brace_token = Token(1);
    block -> AddStatement(p);
    block -> right_brace_token = Sym(9) -> RightToken();
    block -> no_braces = true;
    Sym(1) = block;
}
./

ForStatementNoShortIf ::= 'for' '(' ForInitopt ';' Expressionopt ';'
                          ForUpdateopt ')' StatementNoShortIf
\:$MakeForStatement:\
/.$shared_function
//
// void MakeForStatement();
//./

ForInit ::= StatementExpressionList
\:$NoAction:\
/.$shared_NoAction./

ForInit ::= LocalVariableDeclaration
\:$StartList:\
/.$shared_StartList./

ForUpdate ::= StatementExpressionList
\:$NoAction:\
/.$shared_NoAction./

StatementExpressionList ::= StatementExpression
\:$StartList:\
/.$shared_StartList./

StatementExpressionList ::= StatementExpressionList ',' StatementExpression
\:$AddList3:\
/.$shared_AddList3./

--
-- Assert statements were added in JDK 1.4, as part of JSR 41.
-- The use of Marker allows us to share code.
--
--AssertStatement ::= 'assert' Expression ';'
AssertStatement ::= 'assert' Expression Marker Marker ';'
\:$MakeAssertStatement:\
/.$location
void Parser::MakeAssertStatement()
{
    AstAssertStatement* p = ast_pool -> NewAssertStatement();
    p -> assert_token = Token(1);
    p -> condition = DYNAMIC_CAST<AstExpression*> (Sym(2));
    p -> message_opt = DYNAMIC_CAST<AstExpression*> (Sym(4));
    p -> semicolon_token = Token(5);
    Sym(1) = p;
}
./

AssertStatement ::= 'assert' Expression ':' Expression ';'
\:$MakeAssertStatement:\
/.$shared_function
//
// void MakeAssertStatement();
//./

BreakStatement ::= 'break' Identifieropt ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstBreakStatement* p = ast_pool -> NewBreakStatement();
    p -> break_token = Token(1);
    p -> identifier_token_opt = Token(2) == Token(3) ? 0 : Token(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

ContinueStatement ::= 'continue' Identifieropt ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstContinueStatement* p = ast_pool -> NewContinueStatement();
    p -> continue_token = Token(1);
    p -> identifier_token_opt = Token(2) == Token(3) ? 0 : Token(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

ReturnStatement ::= 'return' Expressionopt ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstReturnStatement* p = ast_pool -> NewReturnStatement();
    p -> return_token = Token(1);
    p -> expression_opt = DYNAMIC_CAST<AstExpression*> (Sym(2));
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

ThrowStatement ::= 'throw' Expression ';'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstThrowStatement* p = ast_pool -> NewThrowStatement();
    p -> throw_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(2));
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

SynchronizedStatement ::= 'synchronized' '(' Expression ')' Block
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstSynchronizedStatement* p = ast_pool -> NewSynchronizedStatement();
    p -> synchronized_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    p -> block = DYNAMIC_CAST<AstBlock*> (Sym(5));
    p -> block -> block_tag = AstBlock::SYNCHRONIZED;

    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code.
--
--TryStatement ::= 'try' Block Catches
TryStatement ::= 'try' Block Catches Marker
\:$MakeTryStatement:\
/.$location
void Parser::MakeTryStatement()
{
    AstTryStatement* p = ast_pool -> NewTryStatement();
    p -> try_token = Token(1);
    p -> block = DYNAMIC_CAST<AstBlock*> (Sym(2));
    if (Sym(3))
    {
        AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(3));
        p -> AllocateCatchClauses(tail -> index + 1);
        AstListNode* root = tail;
        do
        {
            root = root -> next;
            p -> AddCatchClause(DYNAMIC_CAST<AstCatchClause*>
                                (root -> element));
        } while (root != tail);
        FreeCircularList(tail);
    }
    if (Sym(4))
    {
        p -> block -> block_tag = AstBlock::TRY_CLAUSE_WITH_FINALLY;
        for (unsigned i = 0; i < p -> NumCatchClauses(); i++)
            p -> CatchClause(i) -> block -> block_tag =
                AstBlock::TRY_CLAUSE_WITH_FINALLY;
        p -> finally_clause_opt = DYNAMIC_CAST<AstFinallyClause*> (Sym(4));
    }
    Sym(1) = p;
}
./

TryStatement ::= 'try' Block Catchesopt Finally
\:$MakeTryStatement:\
/.$shared_function
//
// void MakeTryStatement();
//./

Catches ::= CatchClause
\:$StartList:\
/.$shared_StartList./

Catches ::= Catches CatchClause
\:$AddList2:\
/.$shared_AddList2./

CatchClause ::= 'catch' '(' FormalParameter ')' Block
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstCatchClause* p = ast_pool -> NewCatchClause();
    p -> catch_token = Token(1);
    p -> formal_parameter = DYNAMIC_CAST<AstFormalParameter*> (Sym(3));
    p -> block = DYNAMIC_CAST<AstBlock*> (Sym(5));

    Sym(1) = p;
}
./

Finally ::= 'finally' Block
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstFinallyClause* p = ast_pool -> NewFinallyClause();
    p -> finally_token = Token(1);
    p -> block = DYNAMIC_CAST<AstBlock*> (Sym(2));
    p -> block -> block_tag = AstBlock::FINALLY;

    Sym(1) = p;
}
./

--18.12 Productions from 14: Expressions

Primary ::= PrimaryNoNewArray
\:$NoAction:\
/.$shared_NoAction./

--1.2 feature
--
-- It is legal to access an element of an initialized array, as in
-- new int[] {0}[0]; this requires splitting the original rule for
-- array creation into two.
--
--Primary ::= ArrayCreationExpression
Primary ::= ArrayCreationUninitialized
\:$NoAction:\
/.$shared_NoAction./

Primary ::= ArrayCreationInitialized
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray ::= Literal
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray ::= 'this'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewThisExpression(Token(1));
}
./

--
-- We split this into two rules to allow better parsing of parenthesized
-- expressions vs. casts.  All expressions have a dual *NotName form, so that
-- the decision of whether "(name)" starts a cast or is a primary does not
-- cause parsing ambiguities. The use of Marker allows us to share code.
-- Also note that splitting this rule aids in parsing generics.
--
--PrimaryNoNewArray ::= '(' Expression ')'
PrimaryNoNewArray ::= '(' Name Marker ')'
\:$MakeParenthesizedExpression:\
/.$location
void Parser::MakeParenthesizedExpression()
{
    AstParenthesizedExpression* p = ast_pool -> NewParenthesizedExpression();
    p -> left_parenthesis_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(2));
    p -> right_parenthesis_token = Token(4);
    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code.
--
PrimaryNoNewArray ::= '(' ExpressionNotName Marker ')'
\:$MakeParenthesizedExpression:\
/.$shared_function
//
// void MakeParenthesizedExpression();
//./

PrimaryNoNewArray ::= ClassInstanceCreationExpression
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray ::= FieldAccess
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
--
-- Note that we had to rework this to avoid ambiguity
--
--PrimaryNoNewArray ::= ClassType '.' 'this'
PrimaryNoNewArray ::= Name '.' 'this'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstThisExpression* p = ast_pool -> NewThisExpression(Token(3));
    p -> base_opt = ast_pool -> NewTypeName(DYNAMIC_CAST<AstName*> (Sym(1)));
    Sym(1) = p;
}
./

--1.1 feature
--
-- Note that we had to rework this to avoid ambiguity.
--
--PrimaryNoNewArray ::= Type '.' 'class'
PrimaryNoNewArray ::= PrimitiveType Dimsopt '.' 'class'
\:$MakeClassLiteral:\
/.$location
void Parser::MakeClassLiteral()
{
    AstClassLiteral* p = ast_pool -> NewClassLiteral(Token(4));
    p -> type = MakeArrayType(1);
    Sym(1) = p;
}
./

PrimaryNoNewArray ::= Name Dims '.' 'class'
\:$MakeClassLiteral:\
/.$shared_function
//
// void MakeClassLiteral();
//./

--
-- The use of Marker allows us to share code.
--
PrimaryNoNewArray ::= Name '.' Marker 'class'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(2) = NULL;
    MakeClassLiteral();
}
./

--
-- The use of Marker allows us to share code.
--
--PrimaryNoNewArray ::= 'void' '.' 'class'
PrimaryNoNewArray ::= 'void' '.' Marker 'class'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::VOID_TYPE, Token(1));
    Sym(2) = NULL;
    MakeClassLiteral();
}
./

PrimaryNoNewArray ::= MethodInvocation
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray ::= ArrayAccess
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
--
-- In Java 1.0 a ClassBody could not appear at all in a
-- ClassInstanceCreationExpression.
--
ClassInstanceCreationExpression ::= 'new' ClassOrInterfaceType '('
                                    ArgumentListopt ')' ClassBodyopt
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstClassInstanceCreationExpression* p =
        ast_pool -> NewClassInstanceCreationExpression();
    p -> new_token = Token(1);
    p -> class_type = DYNAMIC_CAST<AstTypeName*> (Sym(2));
    p -> arguments = MakeArguments(3);
    p -> class_body_opt = DYNAMIC_CAST<AstClassBody*> (Sym(6));
    if (p -> class_body_opt)
        p -> class_body_opt -> identifier_token =
            p -> class_type -> IdentifierToken();
    Sym(1) = p;
}
./

--1.1 feature
--
-- The use of Marker is in anticipation of implementing generics.
--
--ClassInstanceCreationExpression ::= Primary '.' 'new' 'Identifier' '('
--                                    ArgumentListopt ')' ClassBodyopt
ClassInstanceCreationExpression ::= Primary '.' 'new' 'Identifier'
                                    Marker '(' ArgumentListopt
                                    ')' ClassBodyopt
\:$MakeQualifiedNew:\
/.$location
void Parser::MakeQualifiedNew()
{
    AstClassInstanceCreationExpression* p =
        ast_pool -> NewClassInstanceCreationExpression();
    p -> base_opt = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> new_token = Token(3);
    p -> class_type = ast_pool -> NewTypeName(MakeSimpleName(4));
    p -> arguments = MakeArguments(6);
    p -> class_body_opt = DYNAMIC_CAST<AstClassBody*> (Sym(9));
    if (p -> class_body_opt)
        p -> class_body_opt -> identifier_token = Token(4);
    Sym(1) = p;
}
./

--1.1 feature
--
-- The use of Marker is in anticipation of implementing generics.
--
--ClassInstanceCreationExpression ::= Name '.' 'new' 'Identifier' '('
--                                    ArgumentListopt ')' ClassBodyopt
ClassInstanceCreationExpression ::= Name '.' 'new' 'Identifier'
                                    Marker '(' ArgumentListopt
                                    ')' ClassBodyopt
\:$MakeQualifiedNew:\
/.$shared_function
//
// void MakeQualifiedNew();
//./

ArgumentList ::= Expression
\:$StartList:\
/.$shared_StartList./

ArgumentList ::= ArgumentList ',' Expression
\:$AddList3:\
/.$shared_AddList3./

--1.2 feature
--
-- ArrayCreationExpression is split into two parsing categories, to
-- allow array access on an initialized array.  See above.
--
--ArrayCreationExpression ::= 'new' PrimitiveType DimExprs Dimsopt
ArrayCreationUninitialized ::= 'new' PrimitiveType DimExprs Dimsopt
\:$MakeArrayCreationUninitialized:\
/.$location
void Parser::MakeArrayCreationUninitialized()
{
    AstArrayCreationExpression* p = ast_pool -> NewArrayCreationExpression();
    p -> new_token = Token(1);
    p -> array_type = DYNAMIC_CAST<AstType*> (Sym(2));
    AstListNode* tail = DYNAMIC_CAST<AstListNode*> (Sym(3));
    p -> AllocateDimExprs(tail -> index + 1);
    AstListNode* root = tail;
    do
    {
        root = root -> next;
        p -> AddDimExpr(DYNAMIC_CAST<AstDimExpr*> (root -> element));
    } while (root != tail);
    FreeCircularList(tail);
    p -> brackets_opt = DYNAMIC_CAST<AstBrackets*> (Sym(4));
    Sym(1) = p;
}
./

--ArrayCreationExpression ::= 'new' ClassOrInterfaceType DimExprs Dimsopt
ArrayCreationUninitialized ::= 'new' ClassOrInterfaceType DimExprs Dimsopt
\:$MakeArrayCreationUninitialized:\
/.$shared_function
//
// void MakeArrayCreationUninitialized();
//./

--1.1 feature
--
--ArrayCreationExpression ::= 'new' ArrayType ArrayInitializer
ArrayCreationInitialized ::= 'new' ArrayType ArrayInitializer
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstArrayCreationExpression* p = ast_pool -> NewArrayCreationExpression();
    p -> new_token = Token(1);
    p -> array_type = DYNAMIC_CAST<AstType*> (Sym(2));
    p -> array_initializer_opt = DYNAMIC_CAST<AstArrayInitializer*> (Sym(3));
    Sym(1) = p;
}
./

DimExprs ::= DimExpr
\:$StartList:\
/.$shared_StartList./

DimExprs ::= DimExprs DimExpr
\:$AddList2:\
/.$shared_AddList2./

DimExpr ::= '[' Expression ']'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstDimExpr* p = ast_pool -> NewDimExpr();
    p -> left_bracket_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(2));
    p -> right_bracket_token = Token(3);
    Sym(1) = p;
}
./

Dims ::= '[' ']'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    Sym(1) = ast_pool -> NewBrackets(Token(1), Token(2));
}
./

Dims ::= Dims '[' ']'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstBrackets* p = DYNAMIC_CAST<AstBrackets*> (Sym(1));
    p -> right_bracket_token = Token(2);
    p -> dims++;
}
./

FieldAccess ::= Primary '.' 'Identifier'
\:$MakeFieldAccess:\
/.$location
void Parser::MakeFieldAccess()
{
    AstFieldAccess* p = ast_pool -> NewFieldAccess();
    p -> base = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> identifier_token = Token(3);
    Sym(1) = p;
}
./

FieldAccess ::= 'super' '.' 'Identifier'
\:$MakeSuperFieldAccess:\
/.$location
void Parser::MakeSuperFieldAccess()
{
    Sym(1) = ast_pool -> NewSuperExpression(Token(1));
    MakeFieldAccess();
}
./

--1.2 feature
--
-- Technically, only ClassType is allowed instead of Name, but that would be
-- ambiguous with qualified names
--
--FieldAccess ::= ClassType '.' 'super' '.' 'Identifier'
FieldAccess ::= Name '.' 'super' '.' 'Identifier'
\:$MakeQualifiedSuperFieldAccess:\
/.$location
void Parser::MakeQualifiedSuperFieldAccess()
{
    AstSuperExpression* q = ast_pool -> NewSuperExpression(Token(3));
    q -> base_opt = ast_pool -> NewTypeName(DYNAMIC_CAST<AstName*> (Sym(1)));
    AstFieldAccess* p = ast_pool -> NewFieldAccess();
    p -> base = q;
    p -> identifier_token = Token(5);
    Sym(1) = p;
}
./

MethodInvocation ::= Name '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number() { MakeMethodInvocation(2); }

//
// This function treats Sym(1) as a method name, and builds the method
// invocation starting with the '('.
//
void Parser::MakeMethodInvocation(int tokennum)
{
    AstMethodInvocation* p = ast_pool -> NewMethodInvocation();
    p -> method = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> arguments = MakeArguments(tokennum);
    Sym(1) = p;
}
./

MethodInvocation ::= Primary '.' 'Identifier' '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    MakeFieldAccess();
    MakeMethodInvocation(4);
}
./

MethodInvocation ::= 'super' '.' 'Identifier' '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    MakeSuperFieldAccess();
    MakeMethodInvocation(4);
}
./

--1.2 feature
--
-- Technically, only ClassType is allowed instead of Name, but that would be
-- ambiguous with qualified names
--
--MethodInvocation ::= ClassType '.' 'super' '.' 'Identifier' '('
--                     ArgumentListopt ')'
MethodInvocation ::= Name '.' 'super' '.' 'Identifier' '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    MakeQualifiedSuperFieldAccess();
    MakeMethodInvocation(6);
}
./

ArrayAccess ::= Name '[' Expression ']'
\:$MakeArrayAccess:\
/.$location
void Parser::MakeArrayAccess()
{
    AstArrayAccess* p = ast_pool -> NewArrayAccess();
    p -> base = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> left_bracket_token = Token(2);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    p -> right_bracket_token = Token(4);
    Sym(1) = p;
}
./

ArrayAccess ::= PrimaryNoNewArray '[' Expression ']'
\:$MakeArrayAccess:\
/.$shared_function
//
// void MakeArrayAccess();
//./

--1.2 feature
--
-- Access of an initialized array is legal.  See above.
--
ArrayAccess ::= ArrayCreationInitialized '[' Expression ']'
\:$MakeArrayAccess:\
/.$shared_function
//
// void MakeArrayAccess();
//./

PostfixExpression ::= Primary
\:$NoAction:\
/.$shared_NoAction./

PostfixExpression ::= Name
\:$NoAction:\
/.$shared_NoAction./

PostfixExpression ::= PostIncrementExpression
\:$NoAction:\
/.$shared_NoAction./

PostfixExpression ::= PostDecrementExpression
\:$NoAction:\
/.$shared_NoAction./

PostfixExpressionNotName ::= Primary
\:$NoAction:\
/.$shared_NoAction./

PostfixExpressionNotName ::= PostIncrementExpression
\:$NoAction:\
/.$shared_NoAction./

PostfixExpressionNotName ::= PostDecrementExpression
\:$NoAction:\
/.$shared_NoAction./

PostIncrementExpression ::= PostfixExpression '++'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstPostUnaryExpression* p =
        ast_pool -> NewPostUnaryExpression(AstPostUnaryExpression::PLUSPLUS);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> post_operator_token = Token(2);
    Sym(1) = p;
}
./

PostDecrementExpression ::= PostfixExpression '--'
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstPostUnaryExpression* p =
        ast_pool -> NewPostUnaryExpression(AstPostUnaryExpression::MINUSMINUS);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> post_operator_token = Token(2);
    Sym(1) = p;
}
./

UnaryExpression ::= PreIncrementExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpression ::= PreDecrementExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpression ::= '+' UnaryExpression
\:$MakePreUnaryExpression:\
/.$location
void Parser::MakePreUnaryExpression()
{
    AstPreUnaryExpression::PreUnaryExpressionTag tag;
    switch (lex_stream -> Kind(Token(1)))
    {
    case TK_PLUS_PLUS: tag = AstPreUnaryExpression::PLUSPLUS; break;
    case TK_MINUS_MINUS: tag = AstPreUnaryExpression::MINUSMINUS; break;
    case TK_PLUS: tag = AstPreUnaryExpression::PLUS; break;
    case TK_MINUS: tag = AstPreUnaryExpression::MINUS; break;
    case TK_TWIDDLE: tag = AstPreUnaryExpression::TWIDDLE; break;
    case TK_NOT: tag = AstPreUnaryExpression::NOT; break;
    default: tag = AstPreUnaryExpression::NONE;
    }
    AstPreUnaryExpression* p = ast_pool -> NewPreUnaryExpression(tag);
    p -> pre_operator_token = Token(1);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(2));
    Sym(1) = p;
}
./

UnaryExpression ::= '-' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpression ::= UnaryExpressionNotPlusMinus
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotName ::= PreIncrementExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotName ::= PreDecrementExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotName ::= '+' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotName ::= '-' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotName ::= UnaryExpressionNotPlusMinusNotName
\:$NoAction:\
/.$shared_NoAction./

PreIncrementExpression ::= '++' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

PreDecrementExpression ::= '--' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotPlusMinus ::= PostfixExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotPlusMinus ::= '~' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotPlusMinus ::= '!' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotPlusMinus ::= CastExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotPlusMinusNotName ::= PostfixExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotPlusMinusNotName ::= '~' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotPlusMinusNotName ::= '!' UnaryExpression
\:$MakePreUnaryExpression:\
/.$shared_Unary./

UnaryExpressionNotPlusMinusNotName ::= CastExpression
\:$NoAction:\
/.$shared_NoAction./

--
-- The grammar of JLS 15 is ambiguous with "(a" starting a cast or being a
-- parenthesized expression.  JLS1 proposed one way to rewrite the grammar,
-- requiring a semantic check that the cast expression really is a type.
-- However, we settle for a different solution (partly in anticipation of
-- LALR(1) parsing of generics), made possible by the way we factored
-- parenthesized expressions in Primary.
--
-- JLS 15 lists:
--CastExpression ::= '(' PrimitiveType ')' UnaryExpression
--CastExpression ::= '(' ReferenceType ')' UnaryExpressionNotPlusMinus
-- JLS1 suggests:
--CastExpression ::= '(' PrimitiveType Dimsopt ')' UnaryExpression
--CastExpression ::= '(' Expression ')' UnaryExpressionNotPlusMinus
--CastExpression ::= '(' Name Dims ')' UnaryExpressionNotPlusMinus
--
CastExpression ::= '(' PrimitiveType Dimsopt ')' UnaryExpression
\:$MakeCastExpression:\
/.$location
void Parser::MakeCastExpression() { MakeCastExpression(MakeArrayType(2), 4); }

//
// Builds a cast expression. type must be the target AstType, and tokennum
// should point to the ')'.
//
void Parser::MakeCastExpression(AstType* type, int tokennum)
{
    AstCastExpression* p = ast_pool -> NewCastExpression();
    p -> left_parenthesis_token = Token(1);
    p -> type = type;
    p -> right_parenthesis_token = Token(tokennum);
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(tokennum + 1));
    Sym(1) = p;
}
./

--
-- The use of Marker allows us to share code.
--
CastExpression ::= '(' Name Marker ')' UnaryExpressionNotPlusMinus
\:$MakeCastExpression:\
/.$shared_function
//
// void MakeCastExpression();
//./

CastExpression ::= '(' Name Dims ')' UnaryExpressionNotPlusMinus
\:$MakeCastExpression:\
/.$shared_function
//
// void MakeCastExpression();
//./

MultiplicativeExpression ::= UnaryExpression
\:$NoAction:\
/.$shared_NoAction./

MultiplicativeExpression ::= MultiplicativeExpression '*' UnaryExpression
\:$MakeBinaryExpression:\
/.$location
//
// This creates a binary expression of the named type.
//
void Parser::MakeBinaryExpression()
{
    AstBinaryExpression::BinaryExpressionTag tag;
    switch (lex_stream -> Kind(Token(2)))
    {
    case TK_MULTIPLY: tag = AstBinaryExpression::STAR; break;
    case TK_DIVIDE: tag = AstBinaryExpression::SLASH; break;
    case TK_REMAINDER: tag = AstBinaryExpression::MOD; break;
    case TK_PLUS: tag = AstBinaryExpression::PLUS; break;
    case TK_MINUS: tag = AstBinaryExpression::MINUS; break;
    case TK_LEFT_SHIFT: tag = AstBinaryExpression::LEFT_SHIFT; break;
    case TK_RIGHT_SHIFT: tag = AstBinaryExpression::RIGHT_SHIFT; break;
    case TK_UNSIGNED_RIGHT_SHIFT:
        tag = AstBinaryExpression::UNSIGNED_RIGHT_SHIFT; break;
    case TK_LESS: tag = AstBinaryExpression::LESS; break;
    case TK_GREATER: tag = AstBinaryExpression::GREATER; break;
    case TK_LESS_EQUAL: tag = AstBinaryExpression::LESS_EQUAL; break;
    case TK_GREATER_EQUAL: tag = AstBinaryExpression::GREATER_EQUAL; break;
    case TK_EQUAL_EQUAL: tag = AstBinaryExpression::EQUAL_EQUAL; break;
    case TK_NOT_EQUAL: tag = AstBinaryExpression::NOT_EQUAL; break;
    case TK_AND: tag = AstBinaryExpression::AND; break;
    case TK_XOR: tag = AstBinaryExpression::XOR; break;
    case TK_OR: tag = AstBinaryExpression::IOR; break;
    case TK_AND_AND: tag = AstBinaryExpression::AND_AND; break;
    case TK_OR_OR: tag = AstBinaryExpression::OR_OR; break;
    default: tag = AstBinaryExpression::NONE;
    }
    AstBinaryExpression* p = ast_pool -> NewBinaryExpression(tag);
    p -> left_expression = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> binary_operator_token = Token(2);
    p -> right_expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    Sym(1) = p;
}
./

MultiplicativeExpression ::= MultiplicativeExpression '/' UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpression ::= MultiplicativeExpression '%' UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpressionNotName ::= UnaryExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

MultiplicativeExpressionNotName ::= MultiplicativeExpressionNotName '*'
                                    UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpressionNotName ::= Name '*' UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpressionNotName ::= MultiplicativeExpressionNotName '/'
                                    UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpressionNotName ::= Name '/' UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpressionNotName ::= MultiplicativeExpressionNotName '%'
                                    UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

MultiplicativeExpressionNotName ::= Name '%' UnaryExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AdditiveExpression ::= MultiplicativeExpression
\:$NoAction:\
/.$shared_NoAction./

AdditiveExpression ::= AdditiveExpression '+' MultiplicativeExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AdditiveExpression ::= AdditiveExpression '-' MultiplicativeExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AdditiveExpressionNotName ::= MultiplicativeExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

AdditiveExpressionNotName ::= AdditiveExpressionNotName '+'
                              MultiplicativeExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AdditiveExpressionNotName ::= Name '+' MultiplicativeExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AdditiveExpressionNotName ::= AdditiveExpressionNotName '-'
                              MultiplicativeExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AdditiveExpressionNotName ::= Name '-' MultiplicativeExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpression ::= AdditiveExpression
\:$NoAction:\
/.$shared_NoAction./

ShiftExpression ::= ShiftExpression '<<' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpression ::= ShiftExpression '>>' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpression ::= ShiftExpression '>>>' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpressionNotName ::= AdditiveExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

ShiftExpressionNotName ::= ShiftExpressionNotName '<<' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpressionNotName ::= Name '<<' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpressionNotName ::= ShiftExpressionNotName '>>' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpressionNotName ::= Name '>>' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpressionNotName ::= ShiftExpressionNotName '>>>' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ShiftExpressionNotName ::= Name '>>>' AdditiveExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpression ::= ShiftExpression
\:$NoAction:\
/.$shared_NoAction./

RelationalExpression ::= RelationalExpression '<' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpression ::= RelationalExpression '>' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpression ::= RelationalExpression '<=' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpression ::= RelationalExpression '>=' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpression ::= RelationalExpression 'instanceof' ReferenceType
\:$MakeInstanceofExpression:\
/.$location
void Parser::MakeInstanceofExpression()
{
    AstInstanceofExpression* p = ast_pool -> NewInstanceofExpression();
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> instanceof_token = Token(2);
    p -> type = DYNAMIC_CAST<AstType*> (Sym(3));
    Sym(1) = p;
}
./

RelationalExpressionNotName ::= ShiftExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

RelationalExpressionNotName ::= RelationalExpressionNotName '<' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= Name '<' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= RelationalExpressionNotName '>' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= Name '>' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= RelationalExpressionNotName '<='
                                ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= Name '<=' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= RelationalExpressionNotName '>='
                                ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= Name '>=' ShiftExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

RelationalExpressionNotName ::= RelationalExpressionNotName 'instanceof'
                                ReferenceType
\:$MakeInstanceofExpression:\
/.$shared_function
//
// void MakeInstanceofExpression();
//./

RelationalExpressionNotName ::= Name 'instanceof' ReferenceType
\:$MakeInstanceofExpression:\
/.$shared_function
//
// void MakeInstanceofExpression();
//./

EqualityExpression ::= RelationalExpression
\:$NoAction:\
/.$shared_NoAction./

EqualityExpression ::= EqualityExpression '==' RelationalExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

EqualityExpression ::= EqualityExpression '!=' RelationalExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

EqualityExpressionNotName ::= RelationalExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

EqualityExpressionNotName ::= EqualityExpressionNotName '=='
                              RelationalExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

EqualityExpressionNotName ::= Name '==' RelationalExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

EqualityExpressionNotName ::= EqualityExpressionNotName '!='
                              RelationalExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

EqualityExpressionNotName ::= Name '!=' RelationalExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AndExpression ::= EqualityExpression
\:$NoAction:\
/.$shared_NoAction./

AndExpression ::= AndExpression '&' EqualityExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AndExpressionNotName ::= EqualityExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

AndExpressionNotName ::= AndExpressionNotName '&' EqualityExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

AndExpressionNotName ::= Name '&' EqualityExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ExclusiveOrExpression ::= AndExpression
\:$NoAction:\
/.$shared_NoAction./

ExclusiveOrExpression ::= ExclusiveOrExpression '^' AndExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ExclusiveOrExpressionNotName ::= AndExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

ExclusiveOrExpressionNotName ::= ExclusiveOrExpressionNotName '^' AndExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ExclusiveOrExpressionNotName ::= Name '^' AndExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

InclusiveOrExpression ::= ExclusiveOrExpression
\:$NoAction:\
/.$shared_NoAction./

InclusiveOrExpression ::= InclusiveOrExpression '|' ExclusiveOrExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

InclusiveOrExpressionNotName ::= ExclusiveOrExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

InclusiveOrExpressionNotName ::= InclusiveOrExpressionNotName '|'
                                 ExclusiveOrExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

InclusiveOrExpressionNotName ::= Name '|' ExclusiveOrExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalAndExpression ::= InclusiveOrExpression
\:$NoAction:\
/.$shared_NoAction./

ConditionalAndExpression ::= ConditionalAndExpression '&&'
                             InclusiveOrExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalAndExpressionNotName ::= InclusiveOrExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

ConditionalAndExpressionNotName ::= ConditionalAndExpressionNotName '&&'
                                    InclusiveOrExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalAndExpressionNotName ::= Name '&&' InclusiveOrExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalOrExpression ::= ConditionalAndExpression
\:$NoAction:\
/.$shared_NoAction./

ConditionalOrExpression ::= ConditionalOrExpression '||'
                            ConditionalAndExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalOrExpressionNotName ::= ConditionalAndExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

ConditionalOrExpressionNotName ::= ConditionalOrExpressionNotName '||'
                                   ConditionalAndExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalOrExpressionNotName ::= Name '||' ConditionalAndExpression
\:$MakeBinaryExpression:\
/.$shared_Binary./

ConditionalExpression ::= ConditionalOrExpression
\:$NoAction:\
/.$shared_NoAction./

ConditionalExpression ::= ConditionalOrExpression '?' Expression ':'
                          ConditionalExpression
\:$MakeConditionalExpression:\
/.$location
void Parser::MakeConditionalExpression()
{
    AstConditionalExpression* p = ast_pool -> NewConditionalExpression();
    p -> test_expression = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> question_token = Token(2);
    p -> true_expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    p -> colon_token = Token(4);
    p -> false_expression = DYNAMIC_CAST<AstExpression*> (Sym(5));
    Sym(1) = p;
}
./

ConditionalExpressionNotName ::= ConditionalOrExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

ConditionalExpressionNotName ::= ConditionalOrExpressionNotName '?' Expression
                                 ':' ConditionalExpression
\:$MakeConditionalExpression:\
/.$shared_function
//
// void MakeConditionalExpression();
//./

ConditionalExpressionNotName ::= Name '?' Expression ':' ConditionalExpression
\:$MakeConditionalExpression:\
/.$shared_function
//
// void MakeConditionalExpression();
//./

AssignmentExpression ::= ConditionalExpression
\:$NoAction:\
/.$shared_NoAction./

AssignmentExpression ::= Assignment
\:$NoAction:\
/.$shared_NoAction./

AssignmentExpressionNotName ::= ConditionalExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

AssignmentExpressionNotName ::= Assignment
\:$NoAction:\
/.$shared_NoAction./

--
-- The original grammar uses LeftHandSide, instead of PostfixExpression.
-- However, parenthesized variables were added in JLS 2, and the
-- grammar is ambiguous unless we include all non-assignment
-- expressions. The semantic pass will filter out bad left-hand sides.
--
--Assignment ::= LeftHandSide AssignmentOperator AssignmentExpression
Assignment ::= PostfixExpression AssignmentOperator AssignmentExpression
\:$action:\
/.$location
void Parser::Act$rule_number()
{
    AstAssignmentExpression::AssignmentExpressionTag tag;
    switch (lex_stream -> Kind(Token(2)))
    {
    case TK_EQUAL: tag = AstAssignmentExpression::SIMPLE_EQUAL; break;
    case TK_MULTIPLY_EQUAL: tag = AstAssignmentExpression::STAR_EQUAL; break;
    case TK_DIVIDE_EQUAL: tag = AstAssignmentExpression::SLASH_EQUAL; break;
    case TK_REMAINDER_EQUAL: tag = AstAssignmentExpression::MOD_EQUAL; break;
    case TK_PLUS_EQUAL: tag = AstAssignmentExpression::PLUS_EQUAL; break;
    case TK_MINUS_EQUAL: tag = AstAssignmentExpression::MINUS_EQUAL; break;
    case TK_LEFT_SHIFT_EQUAL:
        tag = AstAssignmentExpression::LEFT_SHIFT_EQUAL; break;
    case TK_RIGHT_SHIFT_EQUAL:
        tag = AstAssignmentExpression::RIGHT_SHIFT_EQUAL; break;
    case TK_UNSIGNED_RIGHT_SHIFT_EQUAL:
        tag = AstAssignmentExpression::UNSIGNED_RIGHT_SHIFT_EQUAL; break;
    case TK_AND_EQUAL: tag = AstAssignmentExpression::AND_EQUAL; break;
    case TK_XOR_EQUAL: tag = AstAssignmentExpression::XOR_EQUAL; break;
    case TK_OR_EQUAL: tag = AstAssignmentExpression::IOR_EQUAL; break;
    default: tag = AstAssignmentExpression::NONE;
    }
    AstAssignmentExpression* p =
        ast_pool -> NewAssignmentExpression(tag, Token(2));
    p -> left_hand_side = DYNAMIC_CAST<AstExpression*> (Sym(1));
    p -> expression = DYNAMIC_CAST<AstExpression*> (Sym(3));
    Sym(1) = p;
}
./

--
-- See comments above for Assignment - LeftHandSide is now a useless rule.
--
--LeftHandSide -> Name
--LeftHandSide -> FieldAccess
--LeftHandSide -> ArrayAccess

AssignmentOperator ::= '='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '*='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '/='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '%='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '+='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '-='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '<<='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '>>='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '>>>='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '&='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '^='
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '|='
\:$NoAction:\
/.$shared_NoAction./

Expression ::= AssignmentExpression
\:$NoAction:\
/.$shared_NoAction./

ExpressionNotName ::= AssignmentExpressionNotName
\:$NoAction:\
/.$shared_NoAction./

--
-- Simplify the syntax tree.
--
--ConstantExpression ::= Expression

-----------------------------------------------------------------------------
--
-- The following rules are for optional productions. Most place NULL on
-- the stack if the production was empty.
--
-----------------------------------------------------------------------------

,opt ::= $empty
\:$NoAction:\
/.$shared_NoAction./

,opt ::= ','
\:$NoAction:\
/.$shared_NoAction./

Identifieropt ::= $empty
\:$NoAction:\
/.$shared_NoAction./

Identifieropt ::= 'Identifier'
\:$NoAction:\
/.$shared_NoAction./

PackageDeclarationopt ::= $empty
\:$NullAction:\
/.$location
//
// Given a rule of the form A ::= x1 x2 ... xn
//
// Construct a NULL Ast for A.
//
void Parser::NullAction() { Sym(1) = NULL; }
./

PackageDeclarationopt ::= PackageDeclaration
\:$NoAction:\
/.$shared_NoAction./

Superopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Superopt ::= Super
\:$NoAction:\
/.$shared_NoAction./

Expressionopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Expressionopt ::= Expression
\:$NoAction:\
/.$shared_NoAction./

ClassBodyopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ClassBodyopt ::= ClassBody
\:$NoAction:\
/.$shared_NoAction./

ImportDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ImportDeclarationsopt ::= ImportDeclarations
\:$NoAction:\
/.$shared_NoAction./

TypeDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

TypeDeclarationsopt ::= TypeDeclarations
\:$NoAction:\
/.$shared_NoAction./

ClassBodyDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ClassBodyDeclarationsopt ::= ClassBodyDeclarations
\:$NoAction:\
/.$shared_NoAction./

Modifiersopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Modifiersopt ::= Modifiers
\:$NoAction:\
/.$shared_NoAction./

BlockStatementsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

BlockStatementsopt ::= BlockStatements
\:$NoAction:\
/.$shared_NoAction./

Dimsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Dimsopt ::= Dims
\:$NoAction:\
/.$shared_NoAction./

ArgumentListopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ArgumentListopt ::= ArgumentList
\:$NoAction:\
/.$shared_NoAction./

SwitchLabelsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

SwitchLabelsopt ::= SwitchLabels
\:$NoAction:\
/.$shared_NoAction./

Throwsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Throwsopt ::= Throws
\:$NoAction:\
/.$shared_NoAction./

FormalParameterListopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

FormalParameterListopt ::= FormalParameterList
\:$NoAction:\
/.$shared_NoAction./

Interfacesopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Interfacesopt ::= Interfaces
\:$NoAction:\
/.$shared_NoAction./

InterfaceMemberDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

InterfaceMemberDeclarationsopt ::= InterfaceMemberDeclarations
\:$NoAction:\
/.$shared_NoAction./

ForInitopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ForInitopt ::= ForInit
\:$NoAction:\
/.$shared_NoAction./

ForUpdateopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ForUpdateopt ::= ForUpdate
\:$NoAction:\
/.$shared_NoAction./

ExtendsInterfacesopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ExtendsInterfacesopt ::= ExtendsInterfaces
\:$NoAction:\
/.$shared_NoAction./

Catchesopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Catchesopt ::= Catches
\:$NoAction:\
/.$shared_NoAction./

PackageHeaderMarker ::= $empty
\:$action:\
/.$location
//
// When this function is invoked, if the "parse_package_header_only" flag
// is turned on, we skip to the end-of-file token.
//
void Parser::Act$rule_number()
{
    if (parse_package_header_only)
        // point to the EOF token
        lex_stream -> Reset(lex_stream -> NumTokens() - 1);
    Sym(1) = NULL;
}
./

MethodHeaderMarker ::= $empty
\:$action:\
/.$location
//
// When this function is invoked, if the "parse_header_only" flag
// is turned on, the body of the method being parsed is skipped.
//
void Parser::Act$rule_number()
{
    if (parse_header_only)
    {
        TokenObject token = Token(1);

        //
        // If the first token immediately following the method header
        // is not an open brace, then we have a syntactic error. Do
        // nothing and let the error recovery take care of it.
        //
        if (lex_stream -> Kind(token) == TK_LBRACE)
            lex_stream -> Reset(lex_stream -> MatchingBrace(token));
    }

    Sym(1) = NULL;
}
./

--
-- This rule exists solely to put NULL on the symbol stack, allowing us to
-- share productions that differ by the presence or absence of a rule.
--
Marker ::= $empty
\:$NullAction:\
/.$shared_NullAction./


------ Finish off the files
/.
#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif
./

\:
#ifndef HEADERS
    return;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // ! HEADERS
:\

-- Names allow diagnose.cpp debug output to be more legible
$names

BodyMarker ::= '"class Identifier { ... MethodHeader "'

PLUS_PLUS ::= '++'
MINUS_MINUS ::= '--'
EQUAL_EQUAL ::= '=='
LESS_EQUAL ::= '<='
GREATER_EQUAL ::= '>='
NOT_EQUAL ::= '!='
LEFT_SHIFT ::= '<<'
RIGHT_SHIFT ::= '>>'
UNSIGNED_RIGHT_SHIFT ::= '>>>'
PLUS_EQUAL ::= '+='
MINUS_EQUAL ::= '-='
MULTIPLY_EQUAL ::= '*='
DIVIDE_EQUAL ::= '/='
AND_EQUAL ::= '&='
OR_EQUAL ::= '|='
XOR_EQUAL ::= '^='
REMAINDER_EQUAL ::= '%='
LEFT_SHIFT_EQUAL ::= '<<='
RIGHT_SHIFT_EQUAL ::= '>>='
UNSIGNED_RIGHT_SHIFT_EQUAL ::= '>>>='
OR_OR ::= '||'
AND_AND ::= '&&'
PLUS ::= '+'
MINUS ::= '-'
NOT ::= '!'
REMAINDER ::= '%'
XOR ::= '^'
AND ::= '&'
MULTIPLY ::= '*'
OR ::= '|'
TWIDDLE ::= '~'
DIVIDE ::= '/'
GREATER ::= '>'
LESS ::= '<'
LPAREN ::= '('
RPAREN ::= ')'
LBRACE ::= '{'
RBRACE ::= '}'
LBRACKET ::= '['
RBRACKET ::= ']'
SEMICOLON ::= ';'
QUESTION ::= '?'
COLON ::= ':'
COMMA ::= ','
DOT ::= '.'
EQUAL ::= '='
AT ::= '@'
ELLIPSIS ::= '...'

StatementNoShortIf ::= 'Statement'
StatementWithoutTrailingSubstatement ::= 'Statement'
LabeledStatementNoShortIf ::= 'LabeledStatement'
IfThenElseStatementNoShortIf ::= 'IfThenElseStatement'
WhileStatementNoShortIf ::= 'WhileStatement'
ForStatementNoShortIf ::= 'ForStatement'
UnaryExpressionNotPlusMinus ::= 'UnaryExpression'

PostfixExpressionNotName ::= 'PostfixExpression'
UnaryExpressionNotName ::= 'UnaryExpression'
UnaryExpressionNotPlusMinusNotName ::= 'UnaryExpression'
MultiplicativeExpressionNotName ::= 'MultiplicativeExpression'
AdditiveExpressionNotName ::= 'AdditiveExpression'
ShiftExpressionNotName ::= 'ShiftExpression'
RelationalExpressionNotName ::= 'RelationalExpression'
EqualityExpressionNotName ::= 'EqualityExpression'
AndExpressionNotName ::= 'AndExpression'
ExclusiveOrExpressionNotName ::= 'ExclusiveOrExpression'
InclusiveOrExpressionNotName ::= 'InclusiveOrExpression'
ConditionalAndExpressionNotName ::= 'ConditionalAndExpression'
ConditionalOrExpressionNotName ::= 'ConditionalOrExpression'
ConditionalExpressionNotName ::= 'ConditionalExpression'
AssignmentExpressionNotName ::= 'AssignmentExpression'
ExpressionNotName ::= 'Expression'

$end
