%options scopes,act,an=javaact.cpp,hn=javaact.h,em,tab,gp=c++,
%options fp=java,escape=$,prefix=TK_,em,defer,output-size=125
%options hblockb=\:,hblocke=:\
%options nogoto-default
%options single-productions
%options la=2,names=max
-- $Id: java.g,v 1.18 2000/07/22 21:02:51 mdejong Exp $
-- This software is subject to the terms of the IBM Jikes Compiler
-- License Agreement available at the following URL:
-- http://www.ibm.com/research/jikes.
-- Copyright (C) 1996, 1999, International Business Machines Corporation
-- and others.  All Rights Reserved.
-- You must accept the terms of that agreement to use this software.

------------------------------------------------------------------------
--
--                               J A V A
--
-- This Java grammar is almost identical to the grammar defined in
-- chapter 19 of the Java Language Specification manual together with
-- the additional rules found in the 1.1 document. It is written here
-- in JIKES PG format with semantic actions following each rule. In
-- specifying the rules we enclosed all terminal symbols in single
-- quotes so that they can be quickly distinguished from
-- non-terminals. Optional symbols are suffixed with a question mark (?)
-- and the rules expanding such definitions can be found at the end.
--
-- This grammar is totally faithful to the original rules except that
-- some syntactic markers: PackageHeaderMarker, MethodHeaderMarker and
-- BodyMarker, were added to allow the parser to skip certain irrelevant
-- syntactic components when they are not needed.
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
    void Act$rule_number(void);
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

$MakeSimpleName
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeSimpleName;
#endif
./

$MakeFieldAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeFieldAccess;
#endif
./

$MakeQualifiedSuper
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeQualifiedSuper;
#endif
./

$MakeQualifiedNew
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeQualifiedNew;
#endif
./

$SetSym1ToSym2
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::SetSym1ToSym2;
#endif
./

$MakeEmptyStatement
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeEmptyStatement;
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

$MakeArrayCreationExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeArrayCreationExpression;
#endif
./

$MakeSuperFieldAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeSuperFieldAccess;
#endif
./

$MakeSuperDoubleFieldAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeSuperDoubleFieldAccess;
#endif
./

$MakeArrayAccess
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeArrayAccess;
#endif
./

$MakeCastExpression
/.
#ifndef HEADERS
    rule_action[$rule_number] = &Parser::MakeCastExpression;
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
// void NoAction(void);
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
// void NullAction(void);
//./

$Terminals

    BodyMarker

    Identifier

    abstract boolean break byte case catch char class const
    continue default do double else extends false final finally float
    for goto if implements import instanceof int
    interface long native new null package private
    protected public return short static strictfp super switch
    synchronized this throw throws transient true try void
    volatile while

    IntegerLiteral
    LongLiteral
    FloatingPointLiteral
    DoubleLiteral
    CharacterLiteral
    StringLiteral

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

    ERROR
    EOF
    EOL

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

    $EOF   ::= EOF
    $ERROR ::= ERROR

    $EOL   ::= ;

$Start

    Goal

$Rules

\:
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef HEADERS

#ifdef	HAVE_NAMESPACE
namespace Jikes {	// Open namespace Jikes block
#endif

void Parser::InitRuleAction()
{
    rule_action[0] = &Parser::BadAction;
#else
    void BadAction(void);
    void NoAction(void);
    void NullAction(void);
    void MakeArrayType(void);
    void MakeSimpleName(void);
    void MakeFieldAccess(void);
    void MakeQualifiedSuper(void);
    void MakeQualifiedNew(void);
    void SetSym1ToSym2(void);
    void MakeEmptyStatement(void);
    void MakeLabeledStatement(void);
    void MakeExpressionStatement(void);
    void MakeIfThenElseStatement(void);
    void MakeWhileStatement(void);
    void MakeForStatement(void);
    void MakeArrayCreationExpression(void);
    void MakeSuperFieldAccess(void);
    void MakeSuperDoubleFieldAccess(void);
    void MakeArrayAccess(void);
    void MakeCastExpression(void);
#endif

:\

/.#line $next_line "$input_file"
//
// This software is subject to the terms of the IBM Jikes Compiler Open
// Source License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "platform.h"
#include "parser.h"
#include "ast.h"

#undef HEADERS
#include "javaact.h"

#ifdef	HAVE_NAMESPACE
using namespace Jikes;
#endif

#ifdef	HAVE_NAMESPACE
using namespace Jikes;
#endif

//****************************************************************************//
//****************************************************************************//
//*                                                                          *//
//* Below, we show each rule of the Java grammar together with the semantic  *//
//* action that is invoked when the parser performs a reduction by that rule.*//
//*                                                                          *//
//****************************************************************************//
//****************************************************************************//

./

--18.2 Productions from 2.3: The syntactic Grammar

Goal -> CompilationUnit
\:$NoAction:\
/.$location
//
// Given a rule of the form A ::= x1 x2 ... xn        n >= 1
//
// Do nothing - Whatever Ast was produced for x1 is inherited by A.
//
void Parser::BadAction(void) { assert(false); }
void Parser::NoAction(void) {}
./

Goal ::= BodyMarker ConstructorBody
\:$action:\
/.$location
//
// This rule was added to allow the parser to recognize the body of a
// funtion (constructor or method, as the definition of the body of a
// method is subsumed by the definition of the body of a constructor)
// out of context. Note that the artificial terminal BodyMarker is
// added here to prevent an ordinary parse from accepting a body as
// a valid input - i.e., to recognize a body out-of-context, the
// BodyMarker terminal must be inserted in front of the input stream
// containing the body in question.
//
void Parser::Act$rule_number(void)
{
    Sym(1) = Sym(2);
}
./


--18.3 Productions from 3: Lexical Structure
--
-- Expand the definition IntegerLiteral and BooleanLiteral
--

Literal ::= IntegerLiteral
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewIntegerLiteral(Token(1));
}
./

Literal ::= LongLiteral
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewLongLiteral(Token(1));
}
./

Literal ::= FloatingPointLiteral
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewFloatingPointLiteral(Token(1));
}
./

Literal ::= DoubleLiteral
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewDoubleLiteral(Token(1));
}
./

Literal -> BooleanLiteral
\:$NoAction:\
/.$shared_NoAction./

Literal ::= CharacterLiteral
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewCharacterLiteral(Token(1));
}
./

Literal ::= StringLiteral
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewStringLiteral(Token(1));
}
./

Literal ::= null
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewNullLiteral(Token(1));
}
./

BooleanLiteral ::= true
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewTrueLiteral(Token(1));
}
./

BooleanLiteral ::= false
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewFalseLiteral(Token(1));
}
./

--18.4 Productions from 4: Types, Values and Variables

Type -> PrimitiveType
\:$NoAction:\
/.$shared_NoAction./

Type -> ReferenceType
\:$NoAction:\
/.$shared_NoAction./

PrimitiveType -> NumericType
\:$NoAction:\
/.$shared_NoAction./

PrimitiveType ::= 'boolean'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::BOOLEAN, Token(1));
}
./

NumericType -> IntegralType
\:$NoAction:\
/.$shared_NoAction./

NumericType -> FloatingPointType
\:$NoAction:\
/.$shared_NoAction./

IntegralType ::= 'byte'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::BYTE, Token(1));
}
./

IntegralType ::= 'short'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::SHORT, Token(1));
}
./

IntegralType ::= 'int'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::INT, Token(1));
}
./

IntegralType ::= 'long'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::LONG, Token(1));
}
./

IntegralType ::= 'char'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::CHAR, Token(1));
}
./

FloatingPointType ::= 'float'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::FLOAT, Token(1));
}
./

FloatingPointType ::= 'double'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::DOUBLE, Token(1));
}
./

ReferenceType -> ClassOrInterfaceType
\:$NoAction:\
/.$shared_NoAction./

ReferenceType -> ArrayType
\:$NoAction:\
/.$shared_NoAction./

ClassOrInterfaceType -> Name
\:$NoAction:\
/.$shared_NoAction./

--
-- These rules have been rewritten to avoid some conflicts introduced
-- by adding the 1.1 features
--
-- ArrayType ::= PrimitiveType '[' ']'
-- ArrayType ::= Name '[' ']'
-- ArrayType ::= ArrayType '[' ']'
--
ArrayType ::= PrimitiveType Dims
\:$MakeArrayType:\
/.$location
void Parser::MakeArrayType(void)
{
    AstArrayType *p = ast_pool -> NewArrayType();
    p -> type     = Sym(1);
    //
    // The list of modifiers is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateBrackets(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddBrackets((AstBrackets *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

ArrayType ::= Name Dims
\:$MakeArrayType:\
/.$shared_function
//
// void MakeArrayType(void);
//./

ClassType -> ClassOrInterfaceType
\:$NoAction:\
/.$shared_NoAction./

InterfaceType -> ClassOrInterfaceType
\:$NoAction:\
/.$shared_NoAction./

--18.5 Productions from 6: Names

Name -> SimpleName
\:$NoAction:\
/.$shared_NoAction./

Name -> QualifiedName
\:$NoAction:\
/.$shared_NoAction./

SimpleName ::= 'Identifier'
\:$MakeSimpleName:\
/.$location
void Parser::MakeSimpleName(void)
{
    Sym(1) = ast_pool -> NewSimpleName(Token(1));
}
./

QualifiedName ::= Name '.' 'Identifier'
\:$MakeFieldAccess:\
/.$location
void Parser::MakeFieldAccess(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess();
    p -> base = (AstExpression *) Sym(1);
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}
./

--18.6 Productions from 7: Packages

CompilationUnit ::= PackageDeclarationopt ImportDeclarationsopt TypeDeclarationsopt
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstCompilationUnit *p = ast_pool -> NewCompilationUnit();
    p -> package_declaration_opt = (AstPackageDeclaration *) Sym(1);
    if (Sym(2) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateImportDeclarations(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddImportDeclaration((AstImportDeclaration *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateTypeDeclarations(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddTypeDeclaration(root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

ImportDeclarations ::= ImportDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

ImportDeclarations ::= ImportDeclarations ImportDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

TypeDeclarations ::= TypeDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

TypeDeclarations ::= TypeDeclarations TypeDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

PackageDeclaration ::= 'package' Name PackageHeaderMarker ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPackageDeclaration *p = ast_pool -> NewPackageDeclaration();
    p -> package_token   = Token(1);
    p -> name            = (AstExpression *) Sym(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

ImportDeclaration -> SingleTypeImportDeclaration
\:$NoAction:\
/.$shared_NoAction./

ImportDeclaration -> TypeImportOnDemandDeclaration
\:$NoAction:\
/.$shared_NoAction./

SingleTypeImportDeclaration ::= 'import' Name ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstImportDeclaration *p = ast_pool -> NewImportDeclaration();
    p -> import_token    = Token(1);
    p -> name            = (AstExpression *) Sym(2);
    p -> star_token_opt  = 0;
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

TypeImportOnDemandDeclaration ::= 'import' Name '.' '*' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstImportDeclaration *p = ast_pool -> NewImportDeclaration();
    p -> import_token         = Token(1);
    p -> name                 = (AstExpression *) Sym(2);
    p -> star_token_opt       = Token(4);
    p -> semicolon_token      = Token(5);
    Sym(1) = p;
}
./

TypeDeclaration -> ClassDeclaration
\:$NoAction:\
/.$shared_NoAction./

TypeDeclaration -> InterfaceDeclaration
\:$NoAction:\
/.$shared_NoAction./

TypeDeclaration ::= ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}
./

--18.7 Only in the LALR(1) Grammar

Modifiers ::= Modifier
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

Modifiers ::= Modifiers Modifier
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

Modifier ::= 'public'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::PUBLIC, Token(1));
}
./

Modifier ::= 'protected'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::PROTECTED, Token(1));
}
./

Modifier ::= 'private'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::PRIVATE, Token(1));
}
./

Modifier ::= 'static'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::STATIC, Token(1));
}
./

Modifier ::= 'abstract'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::ABSTRACT, Token(1));
}
./

Modifier ::= 'final'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::FINAL, Token(1));
}
./

Modifier ::= 'native'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::NATIVE, Token(1));
}
./

Modifier ::= 'strictfp'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::STRICTFP, Token(1));
}
./

Modifier ::= 'synchronized'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::SYNCHRONIZED, Token(1));
}
./

Modifier ::= 'transient'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::TRANSIENT, Token(1));
}
./

Modifier ::= 'volatile'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::VOLATILE, Token(1));
}
./

--18.8 Productions from 8: Class Declarations
--ClassModifier ::=
--      'abstract'
--    | 'final'
--    | 'public'
--18.8.1 Productions from 8.1: Class Declarations

ClassDeclaration ::= Modifiersopt 'class' 'Identifier' Superopt Interfacesopt ClassBody
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstClassDeclaration *p = ast_pool -> NewClassDeclaration();
    if (Sym(1) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateClassModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddClassModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> class_token          = Token(2);
    p -> identifier_token     = Token(3);
    p -> super_opt            = (AstExpression *) Sym(4);
    if (Sym(5) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(5);
        p -> AllocateInterfaces(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddInterface((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> class_body = (AstClassBody *) Sym(6);
    Sym(1) = p;
}
./

Super ::= 'extends' ClassType
\:$SetSym1ToSym2:\
/.$location
void Parser::SetSym1ToSym2(void) { Sym(1) = Sym(2); }
./

Interfaces ::= 'implements' InterfaceTypeList
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2(void);
//./

InterfaceTypeList ::= InterfaceType
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

InterfaceTypeList ::= InterfaceTypeList ',' InterfaceType
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

ClassBody ::= '{' ClassBodyDeclarationsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstClassBody *p = ast_pool -> NewClassBody();
    if (parse_header_only)
        p -> mark_unparsed();

    p -> left_brace_token = Token(1);
    if (Sym(2) != NULL)
    {
        int num_instance_variables = 0,
            num_class_variables = 0,
            num_methods = 0,
            num_constructors = 0,
            num_static_initializers = 0,
            num_inner_classes = 0,
            num_inner_interfaces = 0,
            num_blocks = 0,
            num_empty_declarations = 0;

        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateClassBodyDeclarations(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddClassBodyDeclaration(root -> element);

            AstFieldDeclaration *field_declaration = root -> element -> FieldDeclarationCast();
            if (field_declaration)
            {
                for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
                {
                    if (field_declaration -> VariableModifier(i) -> kind == Ast::STATIC)
                    {
                        field_declaration -> MarkStatic();
                        break;
                    }
                }
                if (field_declaration -> StaticFieldCast())
                     num_class_variables++;
                else num_instance_variables++;
            }
            else if (root -> element -> MethodDeclarationCast())
            {
                num_methods++;
            }
            else if (root -> element -> ConstructorDeclarationCast())
            {
                num_constructors++;
            }
            else if (root -> element -> StaticInitializerCast())
            {
                num_static_initializers++;
            }
            else if (root -> element -> ClassDeclarationCast())
            {
                num_inner_classes++;
            }
            else if (root -> element -> InterfaceDeclarationCast())
            {
                num_inner_interfaces++;
            }
            else if (root -> element -> BlockCast())
            {
                num_blocks++;
            }
            else num_empty_declarations++;
        } while(root != tail);

        p -> AllocateInstanceVariables(num_instance_variables);
        p -> AllocateClassVariables(num_class_variables);
        p -> AllocateMethods(num_methods);
        p -> AllocateConstructors(num_constructors);
        p -> AllocateStaticInitializers(num_static_initializers);
        p -> AllocateNestedClasses(num_inner_classes);
        p -> AllocateNestedInterfaces(num_inner_interfaces);
        p -> AllocateBlocks(num_blocks);
        p -> AllocateEmptyDeclarations(num_empty_declarations);

        root = tail;
        do
        {
            root = root -> next;

            AstFieldDeclaration *field_declaration;
            AstMethodDeclaration *method_declaration;
            AstConstructorDeclaration *constructor_declaration;
            AstStaticInitializer *static_initializer;
            AstClassDeclaration *class_declaration;
            AstInterfaceDeclaration *interface_declaration;
            AstBlock *block;

            if ((field_declaration = root -> element -> FieldDeclarationCast()))
            {
                if (field_declaration -> StaticFieldCast())
                     p -> AddClassVariable(field_declaration);
                else p -> AddInstanceVariable(field_declaration);
            }
            else if ((method_declaration = root -> element -> MethodDeclarationCast()))
            {
                p -> AddMethod(method_declaration);
            }
            else if ((constructor_declaration = root -> element -> ConstructorDeclarationCast()))
            {
                p -> AddConstructor(constructor_declaration);
            }
            else if ((static_initializer = root -> element -> StaticInitializerCast()))
            {
                p -> AddStaticInitializer(static_initializer);
            }
            else if ((class_declaration = root -> element -> ClassDeclarationCast()))
            {
                p -> AddNestedClass(class_declaration);
            }
            else if ((interface_declaration = root -> element -> InterfaceDeclarationCast()))
            {
                p -> AddNestedInterface(interface_declaration);
            }
            else if ((block = root -> element -> BlockCast()))
            {
                p -> AddBlock(block);
            }
            else // assert(block = root -> element -> EmptyDeclarationCast())
            {
                p -> AddEmptyDeclaration((AstEmptyDeclaration *) root -> element);
            }
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token = Token(3);
    p -> pool = body_pool; // from now on, this is the storage pool to use for this type
    Sym(1) = p;
}
./

ClassBodyDeclarations ::= ClassBodyDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

ClassBodyDeclarations ::= ClassBodyDeclarations ClassBodyDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

ClassBodyDeclaration -> ClassMemberDeclaration
\:$NoAction:\
/.$shared_NoAction./

ClassBodyDeclaration -> StaticInitializer
\:$NoAction:\
/.$shared_NoAction./

ClassBodyDeclaration -> ConstructorDeclaration
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
ClassBodyDeclaration ::= MethodHeaderMarker Block
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = Sym(2);
}
./

ClassMemberDeclaration -> FieldDeclaration
\:$NoAction:\
/.$shared_NoAction./

ClassMemberDeclaration -> MethodDeclaration
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
ClassMemberDeclaration -> ClassDeclaration
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
ClassMemberDeclaration -> InterfaceDeclaration
\:$NoAction:\
/.$shared_NoAction./

--
-- Empty declarations are not valid Java ClassMemberDeclarations.
-- However, since the current (2/14/97) Java compiler accepts them
-- (in fact, some of the official tests contain this erroneous
-- syntax), we decided to accept them as valid syntax and flag them
-- as a warning during semantic processing.
--
ClassMemberDeclaration ::= ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}
./

--18.8.2 Productions from 8.3: Field Declarations
--VariableModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--    | 'static'
--    | 'final'
--    | 'transient'
--    | 'volatile'

FieldDeclaration ::= Modifiersopt Type VariableDeclarators ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFieldDeclaration *p = ast_pool -> NewFieldDeclaration();
    if (Sym(1) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateVariableModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> type = Sym(2);
    //
    // The list of declarators is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateVariableDeclarators(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableDeclarator((AstVariableDeclarator *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> semicolon_token      = Token(4);
    Sym(1) = p;
}
./

VariableDeclarators ::= VariableDeclarator
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

VariableDeclarators ::= VariableDeclarators ',' VariableDeclarator
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

VariableDeclarator ::= VariableDeclaratorId
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstVariableDeclarator *p = ast_pool -> NewVariableDeclarator();
    p -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(1);
    p -> variable_initializer_opt = NULL;
    Sym(1) = p;
}
./

VariableDeclarator ::= VariableDeclaratorId '=' VariableInitializer
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstVariableDeclarator *p = ast_pool -> NewVariableDeclarator();
    p -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(1);
    p -> variable_initializer_opt = Sym(3);
    Sym(1) = p;
}
./

VariableDeclaratorId ::= 'Identifier' Dimsopt
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstVariableDeclaratorId *p = ast_pool -> NewVariableDeclaratorId();
    p -> identifier_token = Token(1);
    if (Sym(2) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateBrackets(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddBrackets((AstBrackets *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

VariableInitializer -> Expression
\:$NoAction:\
/.$shared_NoAction./

VariableInitializer -> ArrayInitializer
\:$NoAction:\
/.$shared_NoAction./

--18.8.3 Productions from 8.4: Method Declarations
--MethodModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--    | 'static'
--    | 'abstract'
--    | 'final'
--    | 'native'
--    | 'synchronized'
--
-- The original rule does not contain the "MethodHeaderMarker.
-- See explanation above.
--
-- MethodDeclaration ::= MethodHeader MethodBody
--

MethodDeclaration ::= MethodHeader MethodHeaderMarker MethodBody
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    ((AstMethodDeclaration *) Sym(1)) -> method_body = (AstStatement *) Sym(3);
}
./

MethodHeader ::= Modifiersopt Type MethodDeclarator Throwsopt
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstMethodDeclaration *p = ast_pool -> NewMethodDeclaration();
    if (Sym(1) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateMethodModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddMethodModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> type              = Sym(2);
    p -> method_declarator = (AstMethodDeclarator *) Sym(3);
    if (Sym(4) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(4);
        p -> AllocateThrows(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddThrow((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

MethodHeader ::= Modifiersopt 'void' MethodDeclarator Throwsopt
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstMethodDeclaration *p = ast_pool -> NewMethodDeclaration();
    if (Sym(1) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateMethodModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddMethodModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> type              = ast_pool -> NewPrimitiveType(Ast::VOID_TYPE, Token(2));
    p -> method_declarator = (AstMethodDeclarator *) Sym(3);
    if (Sym(4) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(4);
        p -> AllocateThrows(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddThrow((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

MethodDeclarator ::= 'Identifier' '(' FormalParameterListopt ')' Dimsopt
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstMethodDeclarator *p = ast_pool -> NewMethodDeclarator();
    p -> identifier_token        = Token(1);
    p -> left_parenthesis_token  = Token(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateFormalParameters(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddFormalParameter((AstFormalParameter *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(4);
    if (Sym(5) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(5);
        p -> AllocateBrackets(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddBrackets((AstBrackets *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

FormalParameterList ::= FormalParameter
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

FormalParameterList ::= FormalParameterList ',' FormalParameter
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

FormalParameter ::= Type VariableDeclaratorId
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFormalParameter *p = ast_pool -> NewFormalParameter();
    p -> type = Sym(1);

    AstVariableDeclarator *formal_declarator = ast_pool -> NewVariableDeclarator();
    formal_declarator -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(2);
    formal_declarator -> variable_initializer_opt = NULL;

    p -> formal_declarator = formal_declarator;

    Sym(1) = p;
}
./

--1.1 feature
FormalParameter ::= Modifiers Type VariableDeclaratorId
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFormalParameter *p = ast_pool -> NewFormalParameter();
    //
    // The list of modifiers is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateParameterModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddParameterModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    p -> type = Sym(2);

    AstVariableDeclarator *formal_declarator = ast_pool -> NewVariableDeclarator();
    formal_declarator -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(3);
    formal_declarator -> variable_initializer_opt = NULL;

    p -> formal_declarator = formal_declarator;

    Sym(1) = p;
}
./

Throws ::= 'throws' ClassTypeList
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2(void);
//./

ClassTypeList ::= ClassType
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

ClassTypeList ::= ClassTypeList ',' ClassType
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

MethodBody -> Block
\:$NoAction:\
/.$shared_NoAction./

MethodBody ::= ';'
\:$MakeEmptyStatement:\
/.$location
void Parser::MakeEmptyStatement(void)
{
    Sym(1) = ast_pool -> NewEmptyStatement(Token(1));
}
./

--18.8.4 Productions from 8.5: Static Initializers

StaticInitializer ::= 'static' MethodHeaderMarker Block
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstStaticInitializer *p = ast_pool -> NewStaticInitializer();
    p -> static_token = Token(1);
    p -> block        = (AstBlock *) Sym(3);
    Sym(1) = p;
}
./

--18.8.5 Productions from 8.6: Constructor Declarations
--ConstructorModifier ::=
--      'public'
--    | 'protected'
--    | 'private'
--
--
-- The original rule does not contain a "MethodHeaderMarker". See
-- explanation above.
--
-- ConstructorDeclaration ::= Modifiersopt ConstructorDeclarator Throwsopt ConstructorBody
--

ConstructorDeclaration ::= Modifiersopt ConstructorDeclarator Throwsopt MethodHeaderMarker ConstructorBody
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstConstructorBlock *block = Sym(5) -> ConstructorBlockCast();
    if (! block)
    {
        block = ast_pool -> NewConstructorBlock();
        block -> left_brace_token                    = Sym(5) -> LeftToken();
        block -> explicit_constructor_invocation_opt = NULL;
        block -> block                               = (AstBlock *) Sym(5);
        block -> right_brace_token                   = Sym(5) -> RightToken();
    }

    AstConstructorDeclaration *p = ast_pool -> NewConstructorDeclaration();

    if (Sym(1) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateConstructorModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddConstructorModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> constructor_declarator = (AstMethodDeclarator *) Sym(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateThrows(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddThrow((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> constructor_body       = block;

    Sym(1) = p;
}
./

--
-- The original rule specifies SimpleName but it appears to be an
-- error as the rule for a method declarator uses an Identifier.
--...Until further notice, ...
--
-- ConstructorDeclarator ::= SimpleName '(' FormalParameterListopt ')'
--

ConstructorDeclarator ::= 'Identifier' '(' FormalParameterListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstMethodDeclarator *p = ast_pool -> NewMethodDeclarator();
    p -> identifier_token        = Token(1);
    p -> left_parenthesis_token  = Token(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateFormalParameters(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddFormalParameter((AstFormalParameter *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(4);
    Sym(1) = p;
}
./

--
-- NOTE that the rules ExplicitConstructorInvocationopt has been expanded
-- in the rule below in order to make the grammar lalr(1).
--
-- ConstructorBody ::= '{' ExplicitConstructorInvocationopt BlockStatementsopt '}'
--
ConstructorBody -> Block
\:$NoAction:\
/.$shared_NoAction./

ConstructorBody ::= '{' ExplicitConstructorInvocation BlockStatementsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBlock *block = ast_pool -> NewBlock();
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        block -> AllocateBlockStatements(tail -> index + 1);
        AstListNode *root = tail;
        block -> left_brace_token  = root -> element -> LeftToken();
        block -> right_brace_token = tail -> element -> RightToken();
        do
        {
            root = root -> next;
            block -> AddStatement((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    else
    {
        block -> left_brace_token  = Token(4);
        block -> right_brace_token = Token(4);
    }

    AstConstructorBlock *p = ast_pool -> NewConstructorBlock();
    p -> left_brace_token                    = Token(1);
    p -> explicit_constructor_invocation_opt = Sym(2);
    p -> block                               = block;
    p -> right_brace_token                   = Token(4);
    Sym(1) = p;
}
./

ExplicitConstructorInvocation ::= 'this' '(' ArgumentListopt ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstThisCall *p = ast_pool -> NewThisCall();
    p -> base_opt                = NULL;
    p -> dot_token_opt           = 0;
    p -> this_token              = Token(1);
    p -> left_parenthesis_token  = Token(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(4);
    p -> semicolon_token         = Token(5);
    Sym(1) = p;
}
./

ExplicitConstructorInvocation ::= 'super' '(' ArgumentListopt ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSuperCall *p = ast_pool -> NewSuperCall();
    p -> base_opt                = NULL;
    p -> dot_token_opt           = 0;
    p -> super_token             = Token(1);
    p -> left_parenthesis_token  = Token(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(4);
    p -> semicolon_token         = Token(5);
    Sym(1) = p;
}
./

--1.2 feature
ExplicitConstructorInvocation ::= Primary '.' 'this' '(' ArgumentListopt ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstThisCall *p = ast_pool -> NewThisCall();
    p -> base_opt               = (AstExpression *) Sym(1);
    p -> dot_token_opt          = Token(2);
    p -> this_token             = Token(3);
    p -> left_parenthesis_token = Token(4);
    if (Sym(5) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(5);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(6);
    p -> semicolon_token         = Token(7);
    Sym(1) = p;
}
./

--1.1 feature
ExplicitConstructorInvocation ::= Primary '.' 'super' '(' ArgumentListopt ')' ';'
\:$MakeQualifiedSuper:\
/.$location
void Parser::MakeQualifiedSuper(void)
{
    AstSuperCall *p = ast_pool -> NewSuperCall();
    p -> base_opt                = (AstExpression *) Sym(1);
    p -> dot_token_opt           = Token(2);
    p -> super_token             = Token(3);
    p -> left_parenthesis_token  = Token(4);
    if (Sym(5) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(5);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(6);
    p -> semicolon_token         = Token(7);
    Sym(1) = p;
}
./

--1.1 feature
ExplicitConstructorInvocation ::= Name '.' 'super' '(' ArgumentListopt ')' ';'
\:$MakeQualifiedSuper:\
/.$shared_function
//
// void MakeQualifiedSuper(void);
//./

--18.9 Productions from 9: Interface Declarations

--18.9.1 Productions from 9.1: Interface Declarations
--InterfaceModifier ::=
--      'public'
--    | 'abstract'
--
InterfaceDeclaration ::= Modifiersopt 'interface' 'Identifier' ExtendsInterfacesopt InterfaceBody
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstInterfaceDeclaration *p = (AstInterfaceDeclaration *) Sym(5);
    if (Sym(1) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateInterfaceModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddInterfaceModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> interface_token  = Token(2);
    p -> identifier_token = Token(3);
    if (Sym(4) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(4);
        p -> AllocateExtendsInterfaces(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddExtendsInterface((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

ExtendsInterfaces ::= 'extends' InterfaceTypeList
\:$SetSym1ToSym2:\
/.$shared_function
//
// void SetSym1ToSym2(void);
//./

InterfaceBody ::= '{' InterfaceMemberDeclarationsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstInterfaceDeclaration *p = ast_pool -> NewInterfaceDeclaration();
    if (parse_header_only)
        p -> mark_unparsed();

    p -> left_brace_token = Token(1);
    if (Sym(2) != NULL)
    {
        int num_class_variables = 0,
            num_methods = 0,
            num_inner_classes = 0,
            num_inner_interfaces = 0,
            num_empty_declarations = 0;

        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateInterfaceMemberDeclarations(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddInterfaceMemberDeclaration(root -> element);

            AstFieldDeclaration *field_declaration = root -> element -> FieldDeclarationCast();
            if (field_declaration)
            {
                field_declaration -> MarkStatic();
                num_class_variables++;
            }
            else if (root -> element -> MethodDeclarationCast())
            {
                num_methods++;
            }
            else if (root -> element -> ClassDeclarationCast())
            {
                num_inner_classes++;
            }
            else if (root -> element -> InterfaceDeclarationCast())
            {
                num_inner_interfaces++;
            }
            else num_empty_declarations++;
        } while(root != tail);

        p -> AllocateClassVariables(num_class_variables);
        p -> AllocateMethods(num_methods);
        p -> AllocateNestedClasses(num_inner_classes);
        p -> AllocateNestedInterfaces(num_inner_interfaces);
        p -> AllocateEmptyDeclarations(num_empty_declarations);

        root = tail;
        do
        {
            root = root -> next;

            AstFieldDeclaration *field_declaration;
            AstMethodDeclaration *method_declaration;
            AstClassDeclaration *class_declaration;
            AstInterfaceDeclaration *interface_declaration;

            if ((field_declaration = root -> element -> FieldDeclarationCast()))
            {
                p -> AddClassVariable(field_declaration);
            }
            else if ((method_declaration = root -> element -> MethodDeclarationCast()))
            {
                p -> AddMethod(method_declaration);
            }
            else if ((class_declaration = root -> element -> ClassDeclarationCast()))
            {
                p -> AddNestedClass(class_declaration);
            }
            else if ((interface_declaration = root -> element -> InterfaceDeclarationCast()))
            {
                p -> AddNestedInterface(interface_declaration);
            }
            else // assert(interface_declaration = root -> element -> EmptyDeclarationCast())
            {
                p -> AddEmptyDeclaration((AstEmptyDeclaration *) root -> element);
            }
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token = Token(3);
    p -> pool = body_pool; // from now on, this is the storage pool to use for this type
    Sym(1) = p;
}
./

InterfaceMemberDeclarations ::= InterfaceMemberDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

InterfaceMemberDeclarations ::= InterfaceMemberDeclarations InterfaceMemberDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

InterfaceMemberDeclaration -> ConstantDeclaration
\:$NoAction:\
/.$shared_NoAction./

InterfaceMemberDeclaration -> AbstractMethodDeclaration
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
InterfaceMemberDeclaration -> ClassDeclaration
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
InterfaceMemberDeclaration -> InterfaceDeclaration
\:$NoAction:\
/.$shared_NoAction./

--
-- Empty declarations are not valid Java InterfaceMemberDeclarations.
-- However, since the current (2/14/97) Java compiler accepts them
-- (in fact, some of the official tests contain this erroneous
-- syntax), we decided to accept them as valid syntax and flag them
-- as a warning during semantic processing.
--
InterfaceMemberDeclaration ::= ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}
./

ConstantDeclaration -> FieldDeclaration
\:$NoAction:\
/.$shared_NoAction./

AbstractMethodDeclaration ::= MethodHeader ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    ((AstMethodDeclaration *) Sym(1)) -> method_body = ast_pool -> NewEmptyStatement(Token(2));
}
./

--18.10 Productions from 10: Arrays

--
-- NOTE that the rules VariableInitializersopt and ,opt have been expanded,
-- where appropriate, in the rule below in order to make the grammar lalr(1).
--
-- ArrayInitializer ::= '{' VariableInitializersopt ,opt '}'
--
ArrayInitializer ::= '{' ,opt '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstArrayInitializer *p = ast_pool -> NewArrayInitializer();
    p -> left_brace_token      = Token(1);
    p -> right_brace_token     = Token(3);
    Sym(1) = p;
}
./

ArrayInitializer ::= '{' VariableInitializers '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstArrayInitializer *p = ast_pool -> NewArrayInitializer();
    p -> left_brace_token      = Token(1);
    if (Sym(2) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateVariableInitializers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableInitializer(root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token     = Token(3);
    Sym(1) = p;
}
./

ArrayInitializer ::= '{' VariableInitializers , '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstArrayInitializer *p = ast_pool -> NewArrayInitializer();
    p -> left_brace_token      = Token(1);
    if (Sym(2) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateVariableInitializers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableInitializer(root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token     = Token(4);
    Sym(1) = p;
}
./

VariableInitializers ::= VariableInitializer
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

VariableInitializers ::= VariableInitializers ',' VariableInitializer
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

--18.11 Productions from 13: Blocks and Statements

Block ::= '{' BlockStatementsopt '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBlock *p = ast_pool -> NewBlock();
    p -> left_brace_token  = Token(1);
    if (Sym(2) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateBlockStatements(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddStatement((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_brace_token = Token(3);
    Sym(1) = p;
}
./

BlockStatements ::= BlockStatement
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

BlockStatements ::= BlockStatements BlockStatement
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

BlockStatement -> LocalVariableDeclarationStatement
\:$NoAction:\
/.$shared_NoAction./

BlockStatement -> Statement
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
BlockStatement -> ClassDeclaration
\:$NoAction:\
/.$shared_NoAction./

LocalVariableDeclarationStatement ::= LocalVariableDeclaration ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    ((AstLocalVariableDeclarationStatement *) Sym(1)) -> semicolon_token_opt = Token(2);
}
./

LocalVariableDeclaration ::= Type VariableDeclarators
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstLocalVariableDeclarationStatement *p = ast_pool -> NewLocalVariableDeclarationStatement();
    p -> type                 = Sym(1);
    //
    // The list of declarators is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateVariableDeclarators(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableDeclarator((AstVariableDeclarator *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> semicolon_token_opt  = 0;
    Sym(1) = p;
}
./

--1.1 feature
LocalVariableDeclaration ::= Modifiers Type VariableDeclarators
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstLocalVariableDeclarationStatement *p = ast_pool -> NewLocalVariableDeclarationStatement();
    //
    // The list of modifiers is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateLocalModifiers(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddLocalModifier((AstModifier *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> type = Sym(2);
    //
    // The list of declarators is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateVariableDeclarators(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddVariableDeclarator((AstVariableDeclarator *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> semicolon_token_opt  = 0;
    Sym(1) = p;
}
./

Statement -> StatementWithoutTrailingSubstatement
\:$NoAction:\
/.$shared_NoAction./

Statement -> LabeledStatement
\:$NoAction:\
/.$shared_NoAction./

Statement -> IfThenStatement
\:$NoAction:\
/.$shared_NoAction./

Statement -> IfThenElseStatement
\:$NoAction:\
/.$shared_NoAction./

Statement -> WhileStatement
\:$NoAction:\
/.$shared_NoAction./

Statement -> ForStatement
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf -> StatementWithoutTrailingSubstatement
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf -> LabeledStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf -> IfThenElseStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf -> WhileStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementNoShortIf -> ForStatementNoShortIf
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> Block
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> EmptyStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> ExpressionStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> SwitchStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> DoStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> BreakStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> ContinueStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> ReturnStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> SynchronizedStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> ThrowStatement
\:$NoAction:\
/.$shared_NoAction./

StatementWithoutTrailingSubstatement -> TryStatement
\:$NoAction:\
/.$shared_NoAction./

EmptyStatement ::= ';'
\:$MakeEmptyStatement:\
/.$shared_function
//
// void MakeEmptyStatement(void);
//./

LabeledStatement ::= 'Identifier' ':' Statement
\:$MakeLabeledStatement:\
/.$location
void Parser::MakeLabeledStatement(void)
{
    AstBlock *p = Sym(3) -> BlockCast();

    if (! (p && p -> NumStatements() == 1 &&
           (p -> Statement(0) -> kind == Ast::FOR   ||
            p -> Statement(0) -> kind == Ast::WHILE ||
            p -> Statement(0) -> kind == Ast::DO)))
    {
        //
        // When a statement is labeled, it is enclosed in a block.
        // This is necessary in order to allow the same name to be
        // reused to label a subsequent statement at the same nesting
        // level... See ProcessBlock, ProcessStatement,...
        //
        p = ast_pool -> NewBlock();
        p -> AllocateBlockStatements(1); // allocate 1 element
        p -> left_brace_token  = Token(1);
        p -> AddStatement((AstStatement *) Sym(3));
        p -> right_brace_token = Sym(3) -> RightToken();
    }

    p -> AddLabel(Token(1)); // add label to statement
    Sym(1) = p; // The final result is a block containing the labeled-statement
}
./

LabeledStatementNoShortIf ::= 'Identifier' ':' StatementNoShortIf
\:$MakeLabeledStatement:\
/.$shared_function
//
// void MakeLabeledStatement(void);
//./

ExpressionStatement ::= StatementExpression ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    ((AstExpressionStatement *) Sym(1)) -> semicolon_token_opt = Token(2);
}
./

StatementExpression ::= Assignment
\:$MakeExpressionStatement:\
/.$location
void Parser::MakeExpressionStatement(void)
{
    AstExpressionStatement *p = ast_pool -> NewExpressionStatement();
    p -> expression          = (AstExpression *) Sym(1);
    p -> semicolon_token_opt = 0;
    Sym(1) = p;
}
./

StatementExpression ::= PreIncrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement(void);
//./

StatementExpression ::= PreDecrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement(void);
//./

StatementExpression ::= PostIncrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement(void);
//./

StatementExpression ::= PostDecrementExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement(void);
//./

StatementExpression ::= MethodInvocation
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement(void);
//./

StatementExpression ::= ClassInstanceCreationExpression
\:$MakeExpressionStatement:\
/.$shared_function
//
// void MakeExpressionStatement(void);
//./

IfThenStatement ::=  'if' '(' Expression ')' Statement
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBlock *block = Sym(5) -> BlockCast();
    if (! block)
    {
        block = ast_pool -> NewBlock();
        block -> AllocateBlockStatements(1); // allocate 1 element
        block -> left_brace_token  = Token(5);
        block -> AddStatement((AstStatement *) Sym(5));
        block -> right_brace_token = Sym(5) -> RightToken();
    }

    AstIfStatement *p = ast_pool -> NewIfStatement();
    p -> if_token            = Token(1);
    p -> expression          = (AstExpression *) Sym(3);
    p -> true_statement      = block;
    p -> false_statement_opt = NULL;
    Sym(1) = p;
}
./

IfThenElseStatement ::=  'if' '(' Expression ')' StatementNoShortIf 'else' Statement
\:$MakeIfThenElseStatement:\
/.$location
void Parser::MakeIfThenElseStatement(void)
{
    AstBlock *true_block = Sym(5) -> BlockCast();
    if (! true_block)
    {
        true_block = ast_pool -> NewBlock();
        true_block -> AllocateBlockStatements(1); // allocate 1 element
        true_block -> left_brace_token  = Token(5);
        true_block -> AddStatement((AstStatement *) Sym(5));
        true_block -> right_brace_token = Sym(5) -> RightToken();
    }

    AstBlock *false_block = Sym(7) -> BlockCast();
    if (! false_block)
    {
        false_block = ast_pool -> NewBlock();
        false_block -> AllocateBlockStatements(1); // allocate 1 element
        false_block -> left_brace_token  = Token(7);
        false_block -> AddStatement((AstStatement *) Sym(7));
        false_block -> right_brace_token = Sym(7) -> RightToken();
    }

    AstIfStatement *p = ast_pool -> NewIfStatement();
    p -> if_token            = Token(1);
    p -> expression          = (AstExpression *) Sym(3);
    p -> true_statement      = true_block;
    p -> false_statement_opt = false_block;
    Sym(1) = p;
}
./

IfThenElseStatementNoShortIf ::=  'if' '(' Expression ')' StatementNoShortIf 'else' StatementNoShortIf
\:$MakeIfThenElseStatement:\
/.$shared_function
//
// void MakeIfThenElseStatement(void);
//./

SwitchStatement ::= 'switch' '(' Expression ')' SwitchBlock
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSwitchStatement *p = (AstSwitchStatement *) Sym(5);
    p -> switch_token = Token(1);
    p -> expression   = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

SwitchBlock ::= '{' '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSwitchStatement *p = ast_pool -> NewSwitchStatement();

    AstBlock *block = ast_pool -> NewBlock();
    block -> left_brace_token  = Token(1);
    block -> right_brace_token = Token(2);

    p -> switch_block = block;

    Sym(1) = p;
}
./

SwitchBlock ::= '{' SwitchBlockStatements '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSwitchStatement *p = ast_pool -> NewSwitchStatement();

    AstBlock *block = ast_pool -> NewBlock();
    block -> left_brace_token  = Token(1);
    if (Sym(2) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        block -> AllocateBlockStatements(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            block -> AddStatement((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    block -> right_brace_token = Token(3);

    p -> switch_block  = block;

    Sym(1) = p;
}
./

SwitchBlock ::= '{' SwitchLabels '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSwitchStatement *p = ast_pool -> NewSwitchStatement();

    AstSwitchBlockStatement *q = ast_pool -> NewSwitchBlockStatement();
    q -> AddStatement(ast_pool -> NewEmptyStatement(Sym(2) -> RightToken()));

    //
    // The list of SwitchBlockStatements is never null
    //
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        q -> AllocateSwitchLabels(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            q -> AddSwitchLabel((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    AstBlock *block = ast_pool -> NewBlock();
    block -> AllocateBlockStatements(1); // allocate 1 element
    block -> left_brace_token  = Token(1);
    block -> AddStatement(q);
    block -> right_brace_token = Token(3);

    p -> switch_block  = block;

    Sym(1) = p;
}
./

SwitchBlock ::= '{' SwitchBlockStatements SwitchLabels '}'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSwitchStatement *p = ast_pool -> NewSwitchStatement();

    AstBlock *block = ast_pool -> NewBlock();
    block -> left_brace_token  = Token(1);
    //
    // The list of SwitchBlockStatements is never null
    //
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        block -> AllocateBlockStatements(tail -> index + 2); // +1 because of extra statement for additional SwithLabels
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            block -> AddStatement((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    AstSwitchBlockStatement *q = ast_pool -> NewSwitchBlockStatement();
    q -> AddStatement(ast_pool -> NewEmptyStatement(Sym(3) -> RightToken()));

    //
    // The list of SwitchLabels is never null
    //
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        q -> AllocateSwitchLabels(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            q -> AddSwitchLabel(root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    block -> AddStatement(q);
    block -> right_brace_token = Token(4);

    p -> switch_block  = block;

    Sym(1) = p;
}
./

SwitchBlockStatements ::= SwitchBlockStatement
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

SwitchBlockStatements ::= SwitchBlockStatements SwitchBlockStatement
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

SwitchBlockStatement ::= SwitchLabels BlockStatements
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSwitchBlockStatement *p = ast_pool -> NewSwitchBlockStatement();
    //
    // The list of SwitchLabels is never null
    //
    {
        AstListNode *tail = (AstListNode *) Sym(1);
        p -> AllocateSwitchLabels(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddSwitchLabel(root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    //
    // The list of SwitchBlockStatements is never null
    //
    {
        AstListNode *tail = (AstListNode *) Sym(2);
        p -> AllocateBlockStatements(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddStatement((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    Sym(1) = p;
}
./

SwitchLabels ::= SwitchLabel
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

SwitchLabels ::= SwitchLabels SwitchLabel
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

SwitchLabel ::= 'case' ConstantExpression ':'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstCaseLabel *p = ast_pool -> NewCaseLabel();
    p -> case_token  = Token(1);
    p -> expression  = (AstExpression *) Sym(2);
    p -> colon_token = Token(3);
    Sym(1) = p;
}
./

SwitchLabel ::= 'default' ':'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstDefaultLabel *p = ast_pool -> NewDefaultLabel();
    p -> default_token = Token(1);
    p -> colon_token   = Token(2);
    Sym(1) = p;
}
./

WhileStatement ::= 'while' '(' Expression ')' Statement
\:$MakeWhileStatement:\
/.$location
void Parser::MakeWhileStatement(void)
{
    AstWhileStatement *p = ast_pool -> NewWhileStatement();
    p -> while_token = Token(1);
    p -> expression  = (AstExpression *) Sym(3);
    p -> statement   = (AstStatement *) Sym(5);

    AstBlock *block = ast_pool -> NewBlock();
    block -> AllocateBlockStatements(1); // allocate 1 element
    block -> left_brace_token  = Token(1); // point to 'FOR' keyword
    block -> AddStatement(p);
    block -> right_brace_token = Sym(5) -> RightToken(); // point to last token in statement

    Sym(1) = block;
}
./

WhileStatementNoShortIf ::= 'while' '(' Expression ')' StatementNoShortIf
\:$MakeWhileStatement:\
/.$shared_function
//
// void MakeWhileStatement(void);
//./

DoStatement ::= 'do' Statement 'while' '(' Expression ')' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstDoStatement *p = ast_pool -> NewDoStatement();
    p -> do_token        = Token(1);
    p -> statement       = (AstStatement *) Sym(2);
    p -> while_token     = Token(3);
    p -> expression      = (AstExpression *) Sym(5);
    p -> semicolon_token = Token(7);

    AstBlock *block = ast_pool -> NewBlock();
    block -> AllocateBlockStatements(1); // allocate 1 element
    block -> left_brace_token  = Token(1);
    block -> AddStatement(p);
    block -> right_brace_token = Token(7);

    Sym(1) = block;
}
./

ForStatement ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')' Statement
\:$MakeForStatement:\
/.$location
void Parser::MakeForStatement(void)
{
    AstForStatement *p = ast_pool -> NewForStatement();
    p -> for_token = Token(1);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateForInitStatements(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddForInitStatement((AstStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> end_expression_opt = (AstExpression *) Sym(5);
    if (Sym(7) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(7);
        p -> AllocateForUpdateStatements(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddForUpdateStatement((AstExpressionStatement *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> statement = (AstStatement *) Sym(9);

    AstBlock *block = ast_pool -> NewBlock();
    block -> AllocateBlockStatements(1); // allocate 1 element
    block -> left_brace_token  = Token(1);
    block -> AddStatement(p);
    block -> right_brace_token = Sym(9) -> RightToken();

    Sym(1) = block;
}
./

ForStatementNoShortIf ::= 'for' '(' ForInitopt ';' Expressionopt ';' ForUpdateopt ')' StatementNoShortIf
\:$MakeForStatement:\
/.$shared_function
//
// void MakeForStatement(void);
//./

ForInit -> StatementExpressionList
\:$NoAction:\
/.$shared_NoAction./

ForInit ::= LocalVariableDeclaration
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

ForUpdate -> StatementExpressionList
\:$NoAction:\
/.$shared_NoAction./

StatementExpressionList ::= StatementExpression
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

StatementExpressionList ::= StatementExpressionList ',' StatementExpression
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

--
-- NOTE that the rule Identifieropt was expanded in line in the two
-- contexts where it appeared: Break and Continue statements.
-- This was done because there is no straightforward way of passing
-- optional token information in the parse stack.
--
BreakStatement ::= 'break' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBreakStatement *p = ast_pool -> NewBreakStatement();
    p -> break_token          = Token(1);
    p -> identifier_token_opt = 0;
    p -> semicolon_token      = Token(2);
    Sym(1) = p;
}
./

BreakStatement ::= 'break' 'Identifier' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBreakStatement *p = ast_pool -> NewBreakStatement();
    p -> break_token          = Token(1);
    p -> identifier_token_opt = Token(2);
    p -> semicolon_token      = Token(3);
    Sym(1) = p;
}
./

ContinueStatement ::= 'continue' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstContinueStatement *p = ast_pool -> NewContinueStatement();
    p -> continue_token       = Token(1);
    p -> identifier_token_opt = 0;
    p -> semicolon_token      = Token(2);
    Sym(1) = p;
}
./

ContinueStatement ::= 'continue' 'Identifier' ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstContinueStatement *p = ast_pool -> NewContinueStatement();
    p -> continue_token       = Token(1);
    p -> identifier_token_opt = Token(2);
    p -> semicolon_token      = Token(3);
    Sym(1) = p;
}
./

ReturnStatement ::= 'return' Expressionopt ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstReturnStatement *p = ast_pool -> NewReturnStatement();
    p -> return_token    = Token(1);
    p -> expression_opt  = (AstExpression *) Sym(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

ThrowStatement ::= 'throw' Expression ';'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstThrowStatement *p = ast_pool -> NewThrowStatement();
    p -> throw_token     = Token(1);
    p -> expression      = (AstExpression *) Sym(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}
./

SynchronizedStatement ::= 'synchronized' '(' Expression ')' Block
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstSynchronizedStatement *p = ast_pool -> NewSynchronizedStatement();
    p -> synchronized_token = Token(1);
    p -> expression         = (AstExpression *) Sym(3);
    p -> block              = (AstBlock *) Sym(5);
    p -> block -> block_tag = AstBlock::SYNCHRONIZED;

    Sym(1) = p;
}
./

TryStatement ::= 'try' Block Catches
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstTryStatement *p = ast_pool -> NewTryStatement();
    p -> try_token          = Token(1);
    p -> block              = (AstBlock *) Sym(2);

    //
    // The list of modifiers is guaranteed not empty
    //
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateCatchClauses(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddCatchClause((AstCatchClause *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> finally_clause_opt = NULL;
    Sym(1) = p;
}
./

TryStatement ::= 'try' Block Catchesopt Finally
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstTryStatement *p = ast_pool -> NewTryStatement();
    p -> try_token      = Token(1);
    p -> block          = (AstBlock *) Sym(2);
    p -> block -> block_tag = AstBlock::TRY_CLAUSE_WITH_FINALLY;

    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateCatchClauses(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddCatchClause((AstCatchClause *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    for (int i = 0; i < p -> NumCatchClauses(); i++)
        p -> CatchClause(i) -> block -> block_tag = AstBlock::TRY_CLAUSE_WITH_FINALLY;

    p -> finally_clause_opt = (AstFinallyClause *) Sym(4);

    Sym(1) = p;
}
./

Catches ::= CatchClause
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

Catches ::= Catches CatchClause
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

CatchClause ::= 'catch' '(' FormalParameter ')' Block
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstCatchClause *p = ast_pool -> NewCatchClause();
    p -> catch_token      = Token(1);
    p -> formal_parameter = (AstFormalParameter *) Sym(3);
    p -> block            = (AstBlock *) Sym(5);

    Sym(1) = p;
}
./

Finally ::= 'finally' Block
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFinallyClause *p     = ast_pool -> NewFinallyClause();
    p -> finally_token      = Token(1);
    p -> block              = (AstBlock *) Sym(2);
    p -> block -> block_tag = AstBlock::FINALLY;

    Sym(1) = p;
}
./

--18.12 Productions from 14: Expressions

Primary -> PrimaryNoNewArray
\:$NoAction:\
/.$shared_NoAction./

Primary -> ArrayCreationExpression
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray -> Literal
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray ::= this
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewThisExpression(Token(1));
}
./

PrimaryNoNewArray ::= '(' Expression ')'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstParenthesizedExpression *p = ast_pool -> NewParenthesizedExpression();
    p -> left_parenthesis_token = Token(1);
    p -> expression = (AstExpression *) Sym(2);
    p -> right_parenthesis_token = Token(3);
    Sym(1) = p;
}
./

PrimaryNoNewArray -> ClassInstanceCreationExpression
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray -> FieldAccess
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
PrimaryNoNewArray ::= Name '.' 'this'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess(AstFieldAccess::THIS_TAG);
    p -> base = (AstExpression *) Sym(1);
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}
./

--1.1 feature
PrimaryNoNewArray ::= Type '.' 'class'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess(AstFieldAccess::CLASS_TAG);
    p -> base = ast_pool -> NewTypeExpression(Sym(1));
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}
./

--1.1 feature
PrimaryNoNewArray ::= 'void' '.' 'class'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess(AstFieldAccess::CLASS_TAG);
    p -> base = ast_pool -> NewTypeExpression(ast_pool -> NewPrimitiveType(Ast::VOID_TYPE, Token(1)));
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}
./

PrimaryNoNewArray -> MethodInvocation
\:$NoAction:\
/.$shared_NoAction./

PrimaryNoNewArray -> ArrayAccess
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
--
-- In Java 1.0 a ClassBody could not appear at all in a
-- ClassInstanceCreationExpression.
--
ClassInstanceCreationExpression ::= 'new' ClassType '(' ArgumentListopt ')' ClassBodyopt
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstClassInstanceCreationExpression *p = ast_pool -> NewClassInstanceCreationExpression();
    p -> base_opt                = NULL;
    p -> dot_token_opt           = 0;
    p -> new_token               = Token(1);
    p -> class_type              = ast_pool -> NewTypeExpression(Sym(2));
    p -> left_parenthesis_token  = Token(3);
    if (Sym(4) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(4);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(5);
    p -> class_body_opt          = (AstClassBody *) Sym(6);
    Sym(1) = p;
}
./

--1.1 feature
ClassInstanceCreationExpression ::= Primary '.' 'new' SimpleName '(' ArgumentListopt ')' ClassBodyopt
\:$MakeQualifiedNew:\
/.$location
void Parser::MakeQualifiedNew(void)
{
    AstClassInstanceCreationExpression *p = ast_pool -> NewClassInstanceCreationExpression();
    p -> base_opt                = (AstExpression *) Sym(1);
    p -> dot_token_opt           = Token(2);
    p -> new_token               = Token(3);
    p -> class_type              = ast_pool -> NewTypeExpression(Sym(4));
    p -> left_parenthesis_token  = Token(5);
    if (Sym(6) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(6);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(7);
    p -> class_body_opt          = (AstClassBody *) Sym(8);
    Sym(1) = p;
}
./

--1.1 feature
ClassInstanceCreationExpression ::= Name '.' 'new' SimpleName '(' ArgumentListopt ')' ClassBodyopt
\:$MakeQualifiedNew:\
/.$shared_function
//
// void MakeQualifiedNew(void);
//./

ArgumentList ::= Expression
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

ArgumentList ::= ArgumentList ',' Expression
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

ArrayCreationExpression ::= 'new' PrimitiveType DimExprs Dimsopt
\:$MakeArrayCreationExpression:\
/.$location
void Parser::MakeArrayCreationExpression(void)
{
    AstArrayCreationExpression *p = ast_pool -> NewArrayCreationExpression();
    p -> new_token             = Token(1);
    p -> array_type            = Sym(2);
    //
    // The list of DimExprs is never null
    //
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateDimExprs(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddDimExpr((AstDimExpr *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }

    if (Sym(4) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(4);
        p -> AllocateBrackets(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddBrackets((AstBrackets *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> array_initializer_opt = NULL;
    Sym(1) = p;
}
./

ArrayCreationExpression ::= 'new' ClassOrInterfaceType DimExprs Dimsopt
\:$MakeArrayCreationExpression:\
/.$shared_function
//
// void MakeArrayCreationExpression(void);
//./

--1.1 feature
ArrayCreationExpression ::= 'new' ArrayType ArrayInitializer
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstArrayCreationExpression *p = ast_pool -> NewArrayCreationExpression();
    p -> new_token             = Token(1);
    p -> array_type            = Sym(2);
    p -> array_initializer_opt = (AstArrayInitializer *) Sym(3);
    Sym(1) = p;
}
./

DimExprs ::= DimExpr
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}
./

DimExprs ::= DimExprs DimExpr
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

DimExpr ::= '[' Expression ']'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstDimExpr *p = ast_pool -> NewDimExpr();
    p -> left_bracket_token  = Token(1);
    p -> expression          = (AstExpression *) Sym(2);
    p -> right_bracket_token = Token(3);
    Sym(1) = p;
}
./

Dims ::= '[' ']'
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = ast_pool -> NewBrackets(Token(1), Token(2));
    p -> index = 0;

    Sym(1) = p;
}
./

Dims ::= Dims '[' ']'
\:$action:\
/.$location
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act$rule_number(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = ast_pool -> NewBrackets(Token(2), Token(3));
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}
./

FieldAccess ::= Primary '.' 'Identifier'
\:$MakeFieldAccess:\
/.$shared_function
//
// void MakeFieldAccess(void);
//./

FieldAccess ::= 'super' '.' 'Identifier'
\:$MakeSuperFieldAccess:\
/.$location
void Parser::MakeSuperFieldAccess(void)
{
    Sym(1) = ast_pool -> NewSuperExpression(Token(1));

    MakeFieldAccess();
}
./

--1.2 feature
FieldAccess ::= Name '.' 'super' '.' 'Identifier'
\:$MakeSuperDoubleFieldAccess:\
/.$location
void Parser::MakeSuperDoubleFieldAccess(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess();

         AstFieldAccess *q = ast_pool -> NewFieldAccess(AstFieldAccess::SUPER_TAG);
         q -> base = (AstExpression *) Sym(1);
         q -> dot_token = Token(2);
         q -> identifier_token = Token(3);

    p -> base = q;
    p -> dot_token = Token(4);
    p -> identifier_token = Token(5);

    Sym(1) = p;
}
./

MethodInvocation ::= Name '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstMethodInvocation *p = ast_pool -> NewMethodInvocation();
    p -> method                  = (AstExpression *) Sym(1);
    p -> left_parenthesis_token  = Token(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(4);
    Sym(1) = p;
}
./

MethodInvocation ::= Primary '.' 'Identifier' '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    MakeFieldAccess();

    AstMethodInvocation *p = ast_pool -> NewMethodInvocation();
    p -> method                  = (AstExpression *) Sym(1);
    p -> left_parenthesis_token  = Token(4);
    if (Sym(5) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(5);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(6);
    Sym(1) = p;
}
./

MethodInvocation ::= 'super' '.' 'Identifier' '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    MakeSuperFieldAccess();

    AstMethodInvocation *p = ast_pool -> NewMethodInvocation();
    p -> method                  = (AstExpression *) Sym(1);
    p -> left_parenthesis_token  = Token(4);
    if (Sym(5) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(5);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(6);
    Sym(1) = p;
}
./

--1.2 feature
MethodInvocation ::= Name '.' 'super' '.' 'Identifier' '(' ArgumentListopt ')'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    MakeSuperDoubleFieldAccess();

    AstMethodInvocation *p = ast_pool -> NewMethodInvocation();
    p -> method                  = (AstExpression *) Sym(1);
    p -> left_parenthesis_token  = Token(6);
    if (Sym(7) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(7);
        p -> AllocateArguments(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddArgument((AstExpression *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token = Token(8);
    Sym(1) = p;
}
./

ArrayAccess ::= Name '[' Expression ']'
\:$MakeArrayAccess:\
/.$location
void Parser::MakeArrayAccess(void)
{
    AstArrayAccess *p = ast_pool -> NewArrayAccess();
    p -> base                = (AstExpression *) Sym(1);
    p -> left_bracket_token  = Token(2);
    p -> expression          = (AstExpression *) Sym(3);
    p -> right_bracket_token = Token(4);
    Sym(1) = p;
}
./

ArrayAccess ::= PrimaryNoNewArray '[' Expression ']'
\:$MakeArrayAccess:\
/.$shared_function
//
// void MakeArrayAccess(void);
//./

PostfixExpression -> Primary
\:$NoAction:\
/.$shared_NoAction./

PostfixExpression -> Name
\:$NoAction:\
/.$shared_NoAction./

PostfixExpression -> PostIncrementExpression
\:$NoAction:\
/.$shared_NoAction./

PostfixExpression -> PostDecrementExpression
\:$NoAction:\
/.$shared_NoAction./

PostIncrementExpression ::= PostfixExpression '++'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPostUnaryExpression *p = ast_pool -> NewPostUnaryExpression(AstPostUnaryExpression::PLUSPLUS);
    p -> expression          = (AstExpression *) Sym(1);
    p -> post_operator_token = Token(2);
    Sym(1) = p;
}
./

PostDecrementExpression ::= PostfixExpression '--'
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPostUnaryExpression *p = ast_pool -> NewPostUnaryExpression(AstPostUnaryExpression::MINUSMINUS);
    p -> expression          = (AstExpression *) Sym(1);
    p -> post_operator_token = Token(2);
    Sym(1) = p;
}
./

UnaryExpression -> PreIncrementExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpression -> PreDecrementExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpression ::= '+' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::PLUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}
./

UnaryExpression ::= '-' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::MINUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}
./

UnaryExpression -> UnaryExpressionNotPlusMinus
\:$NoAction:\
/.$shared_NoAction./

PreIncrementExpression ::= '++' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::PLUSPLUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}
./

PreDecrementExpression ::= '--' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::MINUSMINUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}
./

UnaryExpressionNotPlusMinus -> PostfixExpression
\:$NoAction:\
/.$shared_NoAction./

UnaryExpressionNotPlusMinus ::= '~' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::TWIDDLE);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}
./

UnaryExpressionNotPlusMinus ::= '!' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::NOT);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}
./

UnaryExpressionNotPlusMinus -> CastExpression
\:$NoAction:\
/.$shared_NoAction./

CastExpression ::= '(' PrimitiveType Dimsopt ')' UnaryExpression
\:$MakeCastExpression:\
/.$location
void Parser::MakeCastExpression(void)
{
    AstCastExpression *p = ast_pool -> NewCastExpression();
    p -> left_parenthesis_token_opt  = Token(1);
    p -> type_opt                    = Sym(2);
    if (Sym(3) != NULL)
    {
        AstListNode *tail = (AstListNode *) Sym(3);
        p -> AllocateBrackets(tail -> index + 1);
        AstListNode *root = tail;
        do
        {
            root = root -> next;
            p -> AddBrackets((AstBrackets *) root -> element);
        } while(root != tail);
        FreeCircularList(tail);
    }
    p -> right_parenthesis_token_opt = Token(4);
    p -> expression                  = (AstExpression *) Sym(5);
    Sym(1) = p;
}
./

CastExpression ::= '(' Expression ')' UnaryExpressionNotPlusMinus
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    //
    // Note that Expression must be a name - i.e., Sym(2) -> isName() == true
    // This check is not performed here and should be performed during
    // semantic processing.
    //
    AstCastExpression *p = ast_pool -> NewCastExpression();
    p -> left_parenthesis_token_opt  = Token(1);
    p -> type_opt                    = Sym(2);
    p -> right_parenthesis_token_opt = Token(3);
    p -> expression                  = (AstExpression *) Sym(4);
    Sym(1) = p;
}
./

CastExpression ::= '(' Name Dims ')' UnaryExpressionNotPlusMinus
\:$MakeCastExpression:\
/.$shared_function
//
// void MakeCastExpression(void);
//./

MultiplicativeExpression -> UnaryExpression
\:$NoAction:\
/.$shared_NoAction./

MultiplicativeExpression ::= MultiplicativeExpression '*' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::STAR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

MultiplicativeExpression ::= MultiplicativeExpression '/' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::SLASH);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

MultiplicativeExpression ::= MultiplicativeExpression '%' UnaryExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::MOD);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

AdditiveExpression -> MultiplicativeExpression
\:$NoAction:\
/.$shared_NoAction./

AdditiveExpression ::= AdditiveExpression '+' MultiplicativeExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::PLUS);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

AdditiveExpression ::= AdditiveExpression '-' MultiplicativeExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::MINUS);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ShiftExpression -> AdditiveExpression
\:$NoAction:\
/.$shared_NoAction./

ShiftExpression ::= ShiftExpression '<<'  AdditiveExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::LEFT_SHIFT);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ShiftExpression ::= ShiftExpression '>>'  AdditiveExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::RIGHT_SHIFT);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ShiftExpression ::= ShiftExpression '>>>' AdditiveExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::UNSIGNED_RIGHT_SHIFT);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

RelationalExpression -> ShiftExpression
\:$NoAction:\
/.$shared_NoAction./

RelationalExpression ::= RelationalExpression '<'  ShiftExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::LESS);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

RelationalExpression ::= RelationalExpression '>'  ShiftExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::GREATER);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

RelationalExpression ::= RelationalExpression '<=' ShiftExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::LESS_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

RelationalExpression ::= RelationalExpression '>=' ShiftExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::GREATER_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

RelationalExpression ::= RelationalExpression 'instanceof' ReferenceType
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::INSTANCEOF);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = ast_pool -> NewTypeExpression(Sym(3));
    Sym(1) = p;
}
./

EqualityExpression -> RelationalExpression
\:$NoAction:\
/.$shared_NoAction./

EqualityExpression ::= EqualityExpression '==' RelationalExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::EQUAL_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

EqualityExpression ::= EqualityExpression '!=' RelationalExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::NOT_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./


AndExpression -> EqualityExpression
\:$NoAction:\
/.$shared_NoAction./

AndExpression ::= AndExpression '&' EqualityExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::AND);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ExclusiveOrExpression -> AndExpression
\:$NoAction:\
/.$shared_NoAction./

ExclusiveOrExpression ::= ExclusiveOrExpression '^' AndExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::XOR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

InclusiveOrExpression -> ExclusiveOrExpression
\:$NoAction:\
/.$shared_NoAction./

InclusiveOrExpression ::= InclusiveOrExpression '|' ExclusiveOrExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::IOR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ConditionalAndExpression -> InclusiveOrExpression
\:$NoAction:\
/.$shared_NoAction./

ConditionalAndExpression ::= ConditionalAndExpression '&&' InclusiveOrExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::AND_AND);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ConditionalOrExpression -> ConditionalAndExpression
\:$NoAction:\
/.$shared_NoAction./

ConditionalOrExpression ::= ConditionalOrExpression '||' ConditionalAndExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::OR_OR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

ConditionalExpression -> ConditionalOrExpression
\:$NoAction:\
/.$shared_NoAction./

ConditionalExpression ::= ConditionalOrExpression '?' Expression ':' ConditionalExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstConditionalExpression *p = ast_pool -> NewConditionalExpression();
    p -> test_expression  = (AstExpression *) Sym(1);
    p -> question_token   = Token(2);
    p -> true_expression  = (AstExpression *) Sym(3);
    p -> colon_token      = Token(4);
    p -> false_expression = (AstExpression *) Sym(5);
    Sym(1) = p;
}
./

AssignmentExpression -> ConditionalExpression
\:$NoAction:\
/.$shared_NoAction./

AssignmentExpression -> Assignment
\:$NoAction:\
/.$shared_NoAction./

Assignment ::= LeftHandSide AssignmentOperator AssignmentExpression
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    AstAssignmentExpression *p = (AstAssignmentExpression *) Sym(2);
    p -> left_hand_side = (AstExpression *) Sym(1);
    p -> expression     = (AstExpression *) Sym(3);
    Sym(1) = p;
}
./

LeftHandSide -> Name
\:$NoAction:\
/.$shared_NoAction./

LeftHandSide -> FieldAccess
\:$NoAction:\
/.$shared_NoAction./

LeftHandSide -> ArrayAccess
\:$NoAction:\
/.$shared_NoAction./

AssignmentOperator ::= '='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::SIMPLE_EQUAL, Token(1));
}
./

AssignmentOperator ::= '*='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::STAR_EQUAL, Token(1));
}
./

AssignmentOperator ::= '/='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::SLASH_EQUAL, Token(1));
}
./

AssignmentOperator ::= '%='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::MOD_EQUAL, Token(1));
}
./

AssignmentOperator ::= '+='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::PLUS_EQUAL, Token(1));
}
./

AssignmentOperator ::= '-='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::MINUS_EQUAL, Token(1));
}
./

AssignmentOperator ::= '<<='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::LEFT_SHIFT_EQUAL, Token(1));
}
./

AssignmentOperator ::= '>>='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::RIGHT_SHIFT_EQUAL, Token(1));
}
./

AssignmentOperator ::= '>>>='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::UNSIGNED_RIGHT_SHIFT_EQUAL, Token(1));
}
./

AssignmentOperator ::= '&='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::AND_EQUAL, Token(1));
}
./

AssignmentOperator ::= '^='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::XOR_EQUAL, Token(1));
}
./

AssignmentOperator ::= '|='
\:$action:\
/.$location
void Parser::Act$rule_number(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::IOR_EQUAL, Token(1));
}
./

Expression -> AssignmentExpression
\:$NoAction:\
/.$shared_NoAction./

ConstantExpression -> Expression
\:$NoAction:\
/.$shared_NoAction./

---------------------------------------------------------------------------------------
--
-- The following rules are for optional nonterminals.
--
---------------------------------------------------------------------------------------

PackageDeclarationopt ::= $empty
\:$NullAction:\
/.$location
//
// Given a rule of the form A ::= x1 x2 ... xn
//
// Construct a NULL Ast for A.
//
void Parser::NullAction(void) { Sym(1) = NULL; }
./

PackageDeclarationopt -> PackageDeclaration
\:$NoAction:\
/.$shared_NoAction./

Superopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Superopt -> Super
\:$NoAction:\
/.$shared_NoAction./

Expressionopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Expressionopt -> Expression
\:$NoAction:\
/.$shared_NoAction./

--1.1 feature
ClassBodyopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

--1.1 feature
ClassBodyopt -> ClassBody
\:$NoAction:\
/.$shared_NoAction./

---------------------------------------------------------------------------------------
--
-- The rules below are for optional terminal symbols.  An optional comma,
-- is only used in the context of an array initializer - It is a
-- "syntactic sugar" that otherwise serves no other purpose. By contrast,
-- an optional identifier is used in the definition of a break and
-- continue statement. When the identifier does not appear, a NULL
-- is produced. When the identifier is present, the user should use the
-- corresponding Token(i) method. See break statement as an example.
--
---------------------------------------------------------------------------------------

,opt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

,opt -> ,
\:$NoAction:\
/.$shared_NoAction./

ImportDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ImportDeclarationsopt -> ImportDeclarations
\:$NoAction:\
/.$shared_NoAction./

TypeDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

TypeDeclarationsopt -> TypeDeclarations
\:$NoAction:\
/.$shared_NoAction./

ClassBodyDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ClassBodyDeclarationsopt -> ClassBodyDeclarations
\:$NoAction:\
/.$shared_NoAction./

Modifiersopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Modifiersopt -> Modifiers
\:$NoAction:\
/.$shared_NoAction./

BlockStatementsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

BlockStatementsopt -> BlockStatements
\:$NoAction:\
/.$shared_NoAction./

Dimsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Dimsopt -> Dims
\:$NoAction:\
/.$shared_NoAction./

ArgumentListopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ArgumentListopt -> ArgumentList
\:$NoAction:\
/.$shared_NoAction./

Throwsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Throwsopt -> Throws
\:$NoAction:\
/.$shared_NoAction./

FormalParameterListopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

FormalParameterListopt -> FormalParameterList
\:$NoAction:\
/.$shared_NoAction./

Interfacesopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Interfacesopt -> Interfaces
\:$NoAction:\
/.$shared_NoAction./

InterfaceMemberDeclarationsopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

InterfaceMemberDeclarationsopt -> InterfaceMemberDeclarations
\:$NoAction:\
/.$shared_NoAction./

ForInitopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ForInitopt -> ForInit
\:$NoAction:\
/.$shared_NoAction./

ForUpdateopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ForUpdateopt -> ForUpdate
\:$NoAction:\
/.$shared_NoAction./

ExtendsInterfacesopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

ExtendsInterfacesopt -> ExtendsInterfaces
\:$NoAction:\
/.$shared_NoAction./

Catchesopt ::= $empty
\:$NullAction:\
/.$shared_NullAction./

Catchesopt -> Catches
\:$NoAction:\
/.$shared_NoAction./

PackageHeaderMarker ::= $empty
\:$action:\
/.$location
//
// When this function is invoked, if the "parse_package_header_only" flag
// is turned on, we skip to the end-of-file token.
//
void Parser::Act$rule_number(void)
{
    if (parse_package_header_only)
        lex_stream -> Reset(lex_stream -> NumTokens() - 1); // point to the EOF token
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
void Parser::Act$rule_number(void)
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

---------------------------------------------------------------------------------------

\:
#ifndef HEADERS
    return;
}

#ifdef	HAVE_NAMESPACE
}			// Close namespace Jikes block
#endif

#endif
:\

$names

BodyMarker ::= '"class Identifier { ... MethodHeader "'

void ::= ResultType

PLUS_PLUS ::=    '++'
MINUS_MINUS ::=    '--'
EQUAL_EQUAL ::=    '=='
LESS_EQUAL ::=    '<='
GREATER_EQUAL ::=    '>='
NOT_EQUAL ::=    '!='
LEFT_SHIFT ::=    '<<'
RIGHT_SHIFT ::=    '>>'
UNSIGNED_RIGHT_SHIFT ::=    '>>>'
PLUS_EQUAL ::=    '+='
MINUS_EQUAL ::=    '-='
MULTIPLY_EQUAL ::=    '*='
DIVIDE_EQUAL ::=    '/='
AND_EQUAL ::=    '&='
OR_EQUAL ::=    '|='
XOR_EQUAL ::=    '^='
REMAINDER_EQUAL ::=    '%='
LEFT_SHIFT_EQUAL ::=    '<<='
RIGHT_SHIFT_EQUAL ::=    '>>='
UNSIGNED_RIGHT_SHIFT_EQUAL ::=    '>>>='
OR_OR ::=    '||'
AND_AND ::=    '&&'

PLUS ::=    '+'
MINUS ::=    '-'
NOT ::=    '!'
REMAINDER ::=    '%'
XOR ::=    '^'
AND ::=    '&'
MULTIPLY ::=    '*'
OR ::=    '|'
TWIDDLE ::=    '~'
DIVIDE ::=    '/'
GREATER ::=    '>'
LESS ::=    '<'
LPAREN ::=    '('
RPAREN ::=    ')'
LBRACE ::=    '{'
RBRACE ::=    '}'
LBRACKET ::=    '['
RBRACKET ::=    ']'
SEMICOLON ::=    ';'
QUESTION ::=    '?'
COLON ::=    ':'
COMMA ::=    ','
DOT ::=    '.'
EQUAL ::=    '='

$end
