#line 427 "java.g"
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

#ifdef	HAVE_NAMESPACES
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


//
// Rule 1:  Goal -> CompilationUnit
//
#line 459 "java.g"
//
// Given a rule of the form A ::= x1 x2 ... xn        n >= 1
//
// Do nothing - Whatever Ast was produced for x1 is inherited by A.
//
void Parser::BadAction(void) { assert(false); }
void Parser::NoAction(void) {}

//
// Rule 2:  Goal ::= BodyMarker ConstructorBody
//
#line 471 "java.g"
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
void Parser::Act2(void)
{
    Sym(1) = Sym(2);
}

//
// Rule 3:  Literal ::= IntegerLiteral
//
#line 496 "java.g"
void Parser::Act3(void)
{
    Sym(1) = ast_pool -> NewIntegerLiteral(Token(1));
}

//
// Rule 4:  Literal ::= LongLiteral
//
#line 505 "java.g"
void Parser::Act4(void)
{
    Sym(1) = ast_pool -> NewLongLiteral(Token(1));
}

//
// Rule 5:  Literal ::= FloatingPointLiteral
//
#line 514 "java.g"
void Parser::Act5(void)
{
    Sym(1) = ast_pool -> NewFloatingPointLiteral(Token(1));
}

//
// Rule 6:  Literal ::= DoubleLiteral
//
#line 523 "java.g"
void Parser::Act6(void)
{
    Sym(1) = ast_pool -> NewDoubleLiteral(Token(1));
}

//
// Rule 7:  Literal -> BooleanLiteral
//
// void NoAction(void);
//

//
// Rule 8:  Literal ::= CharacterLiteral
//
#line 536 "java.g"
void Parser::Act8(void)
{
    Sym(1) = ast_pool -> NewCharacterLiteral(Token(1));
}

//
// Rule 9:  Literal ::= StringLiteral
//
#line 545 "java.g"
void Parser::Act9(void)
{
    Sym(1) = ast_pool -> NewStringLiteral(Token(1));
}

//
// Rule 10:  Literal ::= null
//
#line 554 "java.g"
void Parser::Act10(void)
{
    Sym(1) = ast_pool -> NewNullLiteral(Token(1));
}

//
// Rule 11:  BooleanLiteral ::= true
//
#line 563 "java.g"
void Parser::Act11(void)
{
    Sym(1) = ast_pool -> NewTrueLiteral(Token(1));
}

//
// Rule 12:  BooleanLiteral ::= false
//
#line 572 "java.g"
void Parser::Act12(void)
{
    Sym(1) = ast_pool -> NewFalseLiteral(Token(1));
}

//
// Rule 13:  Type -> PrimitiveType
//
// void NoAction(void);
//

//
// Rule 14:  Type -> ReferenceType
//
// void NoAction(void);
//

//
// Rule 15:  PrimitiveType -> NumericType
//
// void NoAction(void);
//

//
// Rule 16:  PrimitiveType ::= boolean
//
#line 595 "java.g"
void Parser::Act16(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::BOOLEAN, Token(1));
}

//
// Rule 17:  NumericType -> IntegralType
//
// void NoAction(void);
//

//
// Rule 18:  NumericType -> FloatingPointType
//
// void NoAction(void);
//

//
// Rule 19:  IntegralType ::= byte
//
#line 612 "java.g"
void Parser::Act19(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::BYTE, Token(1));
}

//
// Rule 20:  IntegralType ::= short
//
#line 621 "java.g"
void Parser::Act20(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::SHORT, Token(1));
}

//
// Rule 21:  IntegralType ::= int
//
#line 630 "java.g"
void Parser::Act21(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::INT, Token(1));
}

//
// Rule 22:  IntegralType ::= long
//
#line 639 "java.g"
void Parser::Act22(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::LONG, Token(1));
}

//
// Rule 23:  IntegralType ::= char
//
#line 648 "java.g"
void Parser::Act23(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::CHAR, Token(1));
}

//
// Rule 24:  FloatingPointType ::= float
//
#line 657 "java.g"
void Parser::Act24(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::FLOAT, Token(1));
}

//
// Rule 25:  FloatingPointType ::= double
//
#line 666 "java.g"
void Parser::Act25(void)
{
    Sym(1) = ast_pool -> NewPrimitiveType(Ast::DOUBLE, Token(1));
}

//
// Rule 26:  ReferenceType -> ClassOrInterfaceType
//
// void NoAction(void);
//

//
// Rule 27:  ReferenceType -> ArrayType
//
// void NoAction(void);
//

//
// Rule 28:  ClassOrInterfaceType -> Name
//
// void NoAction(void);
//

//
// Rule 29:  ArrayType ::= PrimitiveType Dims
//
#line 695 "java.g"
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

//
// Rule 30:  ArrayType ::= Name Dims
//
// void MakeArrayType(void);
//

//
// Rule 31:  ClassType -> ClassOrInterfaceType
//
// void NoAction(void);
//

//
// Rule 32:  InterfaceType -> ClassOrInterfaceType
//
// void NoAction(void);
//

//
// Rule 33:  Name -> SimpleName
//
// void NoAction(void);
//

//
// Rule 34:  Name -> QualifiedName
//
// void NoAction(void);
//

//
// Rule 35:  SimpleName ::= Identifier
//
#line 745 "java.g"
void Parser::MakeSimpleName(void)
{
    Sym(1) = ast_pool -> NewSimpleName(Token(1));
}

//
// Rule 36:  QualifiedName ::= Name DOT Identifier
//
#line 754 "java.g"
void Parser::MakeFieldAccess(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess();
    p -> base = (AstExpression *) Sym(1);
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}

//
// Rule 37:  CompilationUnit ::= PackageDeclarationopt ImportDeclarationsopt TypeDeclarationsopt
//
#line 769 "java.g"
void Parser::Act37(void)
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

//
// Rule 38:  ImportDeclarations ::= ImportDeclaration
//
#line 804 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act38(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 39:  ImportDeclarations ::= ImportDeclarations ImportDeclaration
//
#line 821 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act39(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 40:  TypeDeclarations ::= TypeDeclaration
//
#line 842 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act40(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 41:  TypeDeclarations ::= TypeDeclarations TypeDeclaration
//
#line 859 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act41(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 42:  PackageDeclaration ::= package Name PackageHeaderMarker SEMICOLON
//
#line 880 "java.g"
void Parser::Act42(void)
{
    AstPackageDeclaration *p = ast_pool -> NewPackageDeclaration();
    p -> package_token   = Token(1);
    p -> name            = (AstExpression *) Sym(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}

//
// Rule 43:  ImportDeclaration -> SingleTypeImportDeclaration
//
// void NoAction(void);
//

//
// Rule 44:  ImportDeclaration -> TypeImportOnDemandDeclaration
//
// void NoAction(void);
//

//
// Rule 45:  SingleTypeImportDeclaration ::= import Name SEMICOLON
//
#line 901 "java.g"
void Parser::Act45(void)
{
    AstImportDeclaration *p = ast_pool -> NewImportDeclaration();
    p -> import_token    = Token(1);
    p -> name            = (AstExpression *) Sym(2);
    p -> star_token_opt  = 0;
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}

//
// Rule 46:  TypeImportOnDemandDeclaration ::= import Name DOT MULTIPLY SEMICOLON
//
#line 915 "java.g"
void Parser::Act46(void)
{
    AstImportDeclaration *p = ast_pool -> NewImportDeclaration();
    p -> import_token         = Token(1);
    p -> name                 = (AstExpression *) Sym(2);
    p -> star_token_opt       = Token(4);
    p -> semicolon_token      = Token(5);
    Sym(1) = p;
}

//
// Rule 47:  TypeDeclaration -> ClassDeclaration
//
// void NoAction(void);
//

//
// Rule 48:  TypeDeclaration -> InterfaceDeclaration
//
// void NoAction(void);
//

//
// Rule 49:  TypeDeclaration ::= SEMICOLON
//
#line 937 "java.g"
void Parser::Act49(void)
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}

//
// Rule 50:  Modifiers ::= Modifier
//
#line 948 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act50(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 51:  Modifiers ::= Modifiers Modifier
//
#line 965 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act51(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 52:  Modifier ::= public
//
#line 986 "java.g"
void Parser::Act52(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::PUBLIC, Token(1));
}

//
// Rule 53:  Modifier ::= protected
//
#line 995 "java.g"
void Parser::Act53(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::PROTECTED, Token(1));
}

//
// Rule 54:  Modifier ::= private
//
#line 1004 "java.g"
void Parser::Act54(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::PRIVATE, Token(1));
}

//
// Rule 55:  Modifier ::= static
//
#line 1013 "java.g"
void Parser::Act55(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::STATIC, Token(1));
}

//
// Rule 56:  Modifier ::= abstract
//
#line 1022 "java.g"
void Parser::Act56(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::ABSTRACT, Token(1));
}

//
// Rule 57:  Modifier ::= final
//
#line 1031 "java.g"
void Parser::Act57(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::FINAL, Token(1));
}

//
// Rule 58:  Modifier ::= native
//
#line 1040 "java.g"
void Parser::Act58(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::NATIVE, Token(1));
}

//
// Rule 59:  Modifier ::= strictfp
//
#line 1049 "java.g"
void Parser::Act59(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::STRICTFP, Token(1));
}

//
// Rule 60:  Modifier ::= synchronized
//
#line 1058 "java.g"
void Parser::Act60(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::SYNCHRONIZED, Token(1));
}

//
// Rule 61:  Modifier ::= transient
//
#line 1067 "java.g"
void Parser::Act61(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::TRANSIENT, Token(1));
}

//
// Rule 62:  Modifier ::= volatile
//
#line 1076 "java.g"
void Parser::Act62(void)
{
    Sym(1) = ast_pool -> NewModifier(Ast::VOLATILE, Token(1));
}

//
// Rule 63:  ClassDeclaration ::= Modifiersopt class Identifier Superopt Interfacesopt ClassBody
//
#line 1092 "java.g"
void Parser::Act63(void)
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

//
// Rule 64:  Super ::= extends ClassType
//
#line 1130 "java.g"
void Parser::SetSym1ToSym2(void) { Sym(1) = Sym(2); }

//
// Rule 65:  Interfaces ::= implements InterfaceTypeList
//
// void SetSym1ToSym2(void);
//

//
// Rule 66:  InterfaceTypeList ::= InterfaceType
//
#line 1143 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act66(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 67:  InterfaceTypeList ::= InterfaceTypeList COMMA InterfaceType
//
#line 1160 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act67(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 68:  ClassBody ::= LBRACE ClassBodyDeclarationsopt RBRACE
//
#line 1181 "java.g"
void Parser::Act68(void)
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

//
// Rule 69:  ClassBodyDeclarations ::= ClassBodyDeclaration
//
#line 1319 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act69(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 70:  ClassBodyDeclarations ::= ClassBodyDeclarations ClassBodyDeclaration
//
#line 1336 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act70(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 71:  ClassBodyDeclaration -> ClassMemberDeclaration
//
// void NoAction(void);
//

//
// Rule 72:  ClassBodyDeclaration -> StaticInitializer
//
// void NoAction(void);
//

//
// Rule 73:  ClassBodyDeclaration -> ConstructorDeclaration
//
// void NoAction(void);
//

//
// Rule 74:  ClassBodyDeclaration ::= MethodHeaderMarker Block
//
#line 1370 "java.g"
void Parser::Act74(void)
{
    Sym(1) = Sym(2);
}

//
// Rule 75:  ClassMemberDeclaration -> FieldDeclaration
//
// void NoAction(void);
//

//
// Rule 76:  ClassMemberDeclaration -> MethodDeclaration
//
// void NoAction(void);
//

//
// Rule 77:  ClassMemberDeclaration -> ClassDeclaration
//
// void NoAction(void);
//

//
// Rule 78:  ClassMemberDeclaration -> InterfaceDeclaration
//
// void NoAction(void);
//

//
// Rule 79:  ClassMemberDeclaration ::= SEMICOLON
//
#line 1404 "java.g"
void Parser::Act79(void)
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}

//
// Rule 80:  FieldDeclaration ::= Modifiersopt Type VariableDeclarators SEMICOLON
//
#line 1423 "java.g"
void Parser::Act80(void)
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

//
// Rule 81:  VariableDeclarators ::= VariableDeclarator
//
#line 1461 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act81(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 82:  VariableDeclarators ::= VariableDeclarators COMMA VariableDeclarator
//
#line 1478 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act82(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 83:  VariableDeclarator ::= VariableDeclaratorId
//
#line 1499 "java.g"
void Parser::Act83(void)
{
    AstVariableDeclarator *p = ast_pool -> NewVariableDeclarator();
    p -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(1);
    p -> variable_initializer_opt = NULL;
    Sym(1) = p;
}

//
// Rule 84:  VariableDeclarator ::= VariableDeclaratorId EQUAL VariableInitializer
//
#line 1511 "java.g"
void Parser::Act84(void)
{
    AstVariableDeclarator *p = ast_pool -> NewVariableDeclarator();
    p -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(1);
    p -> variable_initializer_opt = Sym(3);
    Sym(1) = p;
}

//
// Rule 85:  VariableDeclaratorId ::= Identifier Dimsopt
//
#line 1523 "java.g"
void Parser::Act85(void)
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

//
// Rule 86:  VariableInitializer -> Expression
//
// void NoAction(void);
//

//
// Rule 87:  VariableInitializer -> ArrayInitializer
//
// void NoAction(void);
//

//
// Rule 88:  MethodDeclaration ::= MethodHeader MethodHeaderMarker MethodBody
//
#line 1571 "java.g"
void Parser::Act88(void)
{
    ((AstMethodDeclaration *) Sym(1)) -> method_body = (AstStatement *) Sym(3);
}

//
// Rule 89:  MethodHeader ::= Modifiersopt Type MethodDeclarator Throwsopt
//
#line 1580 "java.g"
void Parser::Act89(void)
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

//
// Rule 90:  MethodHeader ::= Modifiersopt void MethodDeclarator Throwsopt
//
#line 1616 "java.g"
void Parser::Act90(void)
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

//
// Rule 91:  MethodDeclarator ::= Identifier LPAREN FormalParameterListopt RPAREN Dimsopt
//
#line 1652 "java.g"
void Parser::Act91(void)
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

//
// Rule 92:  FormalParameterList ::= FormalParameter
//
#line 1689 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act92(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 93:  FormalParameterList ::= FormalParameterList COMMA FormalParameter
//
#line 1706 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act93(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 94:  FormalParameter ::= Type VariableDeclaratorId
//
#line 1727 "java.g"
void Parser::Act94(void)
{
    AstFormalParameter *p = ast_pool -> NewFormalParameter();
    p -> type = Sym(1);

    AstVariableDeclarator *formal_declarator = ast_pool -> NewVariableDeclarator();
    formal_declarator -> variable_declarator_name = (AstVariableDeclaratorId *) Sym(2);
    formal_declarator -> variable_initializer_opt = NULL;

    p -> formal_declarator = formal_declarator;

    Sym(1) = p;
}

//
// Rule 95:  FormalParameter ::= Modifiers Type VariableDeclaratorId
//
#line 1746 "java.g"
void Parser::Act95(void)
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

//
// Rule 96:  Throws ::= throws ClassTypeList
//
// void SetSym1ToSym2(void);
//

//
// Rule 97:  ClassTypeList ::= ClassType
//
#line 1786 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act97(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 98:  ClassTypeList ::= ClassTypeList COMMA ClassType
//
#line 1803 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act98(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 99:  MethodBody -> Block
//
// void NoAction(void);
//

//
// Rule 100:  MethodBody ::= SEMICOLON
//
#line 1828 "java.g"
void Parser::MakeEmptyStatement(void)
{
    Sym(1) = ast_pool -> NewEmptyStatement(Token(1));
}

//
// Rule 101:  StaticInitializer ::= static MethodHeaderMarker Block
//
#line 1839 "java.g"
void Parser::Act101(void)
{
    AstStaticInitializer *p = ast_pool -> NewStaticInitializer();
    p -> static_token = Token(1);
    p -> block        = (AstBlock *) Sym(3);
    Sym(1) = p;
}

//
// Rule 102:  ConstructorDeclaration ::= Modifiersopt ConstructorDeclarator Throwsopt MethodHeaderMarker ConstructorBody
//
#line 1864 "java.g"
void Parser::Act102(void)
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

//
// Rule 103:  ConstructorDeclarator ::= Identifier LPAREN FormalParameterListopt RPAREN
//
#line 1920 "java.g"
void Parser::Act103(void)
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

//
// Rule 104:  ConstructorBody -> Block
//
// void NoAction(void);
//

//
// Rule 105:  ConstructorBody ::= LBRACE ExplicitConstructorInvocation BlockStatementsopt RBRACE
//
#line 1955 "java.g"
void Parser::Act105(void)
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

//
// Rule 106:  ExplicitConstructorInvocation ::= this LPAREN ArgumentListopt RPAREN SEMICOLON
//
#line 1990 "java.g"
void Parser::Act106(void)
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

//
// Rule 107:  ExplicitConstructorInvocation ::= super LPAREN ArgumentListopt RPAREN SEMICOLON
//
#line 2018 "java.g"
void Parser::Act107(void)
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

//
// Rule 108:  ExplicitConstructorInvocation ::= Primary DOT this LPAREN ArgumentListopt RPAREN SEMICOLON
//
#line 2047 "java.g"
void Parser::Act108(void)
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

//
// Rule 109:  ExplicitConstructorInvocation ::= Primary DOT super LPAREN ArgumentListopt RPAREN SEMICOLON
//
#line 2076 "java.g"
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

//
// Rule 110:  ExplicitConstructorInvocation ::= Name DOT super LPAREN ArgumentListopt RPAREN SEMICOLON
//
// void MakeQualifiedSuper(void);
//

//
// Rule 111:  InterfaceDeclaration ::= Modifiersopt interface Identifier ExtendsInterfacesopt InterfaceBody
//
#line 2119 "java.g"
void Parser::Act111(void)
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

//
// Rule 112:  ExtendsInterfaces ::= extends InterfaceTypeList
//
// void SetSym1ToSym2(void);
//

//
// Rule 113:  InterfaceBody ::= LBRACE InterfaceMemberDeclarationsopt RBRACE
//
#line 2162 "java.g"
void Parser::Act113(void)
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

//
// Rule 114:  InterfaceMemberDeclarations ::= InterfaceMemberDeclaration
//
#line 2254 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act114(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 115:  InterfaceMemberDeclarations ::= InterfaceMemberDeclarations InterfaceMemberDeclaration
//
#line 2271 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act115(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 116:  InterfaceMemberDeclaration -> ConstantDeclaration
//
// void NoAction(void);
//

//
// Rule 117:  InterfaceMemberDeclaration -> AbstractMethodDeclaration
//
// void NoAction(void);
//

//
// Rule 118:  InterfaceMemberDeclaration -> ClassDeclaration
//
// void NoAction(void);
//

//
// Rule 119:  InterfaceMemberDeclaration -> InterfaceDeclaration
//
// void NoAction(void);
//

//
// Rule 120:  InterfaceMemberDeclaration ::= SEMICOLON
//
#line 2317 "java.g"
void Parser::Act120(void)
{
    Sym(1) = ast_pool -> NewEmptyDeclaration(Token(1));
}

//
// Rule 121:  ConstantDeclaration -> FieldDeclaration
//
// void NoAction(void);
//

//
// Rule 122:  AbstractMethodDeclaration ::= MethodHeader SEMICOLON
//
#line 2330 "java.g"
void Parser::Act122(void)
{
    ((AstMethodDeclaration *) Sym(1)) -> method_body = ast_pool -> NewEmptyStatement(Token(2));
}

//
// Rule 123:  ArrayInitializer ::= LBRACE ,opt RBRACE
//
#line 2347 "java.g"
void Parser::Act123(void)
{
    AstArrayInitializer *p = ast_pool -> NewArrayInitializer();
    p -> left_brace_token      = Token(1);
    p -> right_brace_token     = Token(3);
    Sym(1) = p;
}

//
// Rule 124:  ArrayInitializer ::= LBRACE VariableInitializers RBRACE
//
#line 2359 "java.g"
void Parser::Act124(void)
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

//
// Rule 125:  ArrayInitializer ::= LBRACE VariableInitializers COMMA RBRACE
//
#line 2383 "java.g"
void Parser::Act125(void)
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

//
// Rule 126:  VariableInitializers ::= VariableInitializer
//
#line 2407 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act126(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 127:  VariableInitializers ::= VariableInitializers COMMA VariableInitializer
//
#line 2424 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act127(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 128:  Block ::= LBRACE BlockStatementsopt RBRACE
//
#line 2447 "java.g"
void Parser::Act128(void)
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

//
// Rule 129:  BlockStatements ::= BlockStatement
//
#line 2471 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act129(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 130:  BlockStatements ::= BlockStatements BlockStatement
//
#line 2488 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act130(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 131:  BlockStatement -> LocalVariableDeclarationStatement
//
// void NoAction(void);
//

//
// Rule 132:  BlockStatement -> Statement
//
// void NoAction(void);
//

//
// Rule 133:  BlockStatement -> ClassDeclaration
//
// void NoAction(void);
//

//
// Rule 134:  LocalVariableDeclarationStatement ::= LocalVariableDeclaration SEMICOLON
//
#line 2522 "java.g"
void Parser::Act134(void)
{
    ((AstLocalVariableDeclarationStatement *) Sym(1)) -> semicolon_token_opt = Token(2);
}

//
// Rule 135:  LocalVariableDeclaration ::= Type VariableDeclarators
//
#line 2531 "java.g"
void Parser::Act135(void)
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

//
// Rule 136:  LocalVariableDeclaration ::= Modifiers Type VariableDeclarators
//
#line 2558 "java.g"
void Parser::Act136(void)
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

//
// Rule 137:  Statement -> StatementWithoutTrailingSubstatement
//
// void NoAction(void);
//

//
// Rule 138:  Statement -> LabeledStatement
//
// void NoAction(void);
//

//
// Rule 139:  Statement -> IfThenStatement
//
// void NoAction(void);
//

//
// Rule 140:  Statement -> IfThenElseStatement
//
// void NoAction(void);
//

//
// Rule 141:  Statement -> WhileStatement
//
// void NoAction(void);
//

//
// Rule 142:  Statement -> ForStatement
//
// void NoAction(void);
//

//
// Rule 143:  StatementNoShortIf -> StatementWithoutTrailingSubstatement
//
// void NoAction(void);
//

//
// Rule 144:  StatementNoShortIf -> LabeledStatementNoShortIf
//
// void NoAction(void);
//

//
// Rule 145:  StatementNoShortIf -> IfThenElseStatementNoShortIf
//
// void NoAction(void);
//

//
// Rule 146:  StatementNoShortIf -> WhileStatementNoShortIf
//
// void NoAction(void);
//

//
// Rule 147:  StatementNoShortIf -> ForStatementNoShortIf
//
// void NoAction(void);
//

//
// Rule 148:  StatementWithoutTrailingSubstatement -> Block
//
// void NoAction(void);
//

//
// Rule 149:  StatementWithoutTrailingSubstatement -> EmptyStatement
//
// void NoAction(void);
//

//
// Rule 150:  StatementWithoutTrailingSubstatement -> ExpressionStatement
//
// void NoAction(void);
//

//
// Rule 151:  StatementWithoutTrailingSubstatement -> SwitchStatement
//
// void NoAction(void);
//

//
// Rule 152:  StatementWithoutTrailingSubstatement -> DoStatement
//
// void NoAction(void);
//

//
// Rule 153:  StatementWithoutTrailingSubstatement -> BreakStatement
//
// void NoAction(void);
//

//
// Rule 154:  StatementWithoutTrailingSubstatement -> ContinueStatement
//
// void NoAction(void);
//

//
// Rule 155:  StatementWithoutTrailingSubstatement -> ReturnStatement
//
// void NoAction(void);
//

//
// Rule 156:  StatementWithoutTrailingSubstatement -> SynchronizedStatement
//
// void NoAction(void);
//

//
// Rule 157:  StatementWithoutTrailingSubstatement -> ThrowStatement
//
// void NoAction(void);
//

//
// Rule 158:  StatementWithoutTrailingSubstatement -> TryStatement
//
// void NoAction(void);
//

//
// Rule 159:  EmptyStatement ::= SEMICOLON
//
// void MakeEmptyStatement(void);
//

//
// Rule 160:  LabeledStatement ::= Identifier COLON Statement
//
#line 2693 "java.g"
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

//
// Rule 161:  LabeledStatementNoShortIf ::= Identifier COLON StatementNoShortIf
//
// void MakeLabeledStatement(void);
//

//
// Rule 162:  ExpressionStatement ::= StatementExpression SEMICOLON
//
#line 2730 "java.g"
void Parser::Act162(void)
{
    ((AstExpressionStatement *) Sym(1)) -> semicolon_token_opt = Token(2);
}

//
// Rule 163:  StatementExpression ::= Assignment
//
#line 2739 "java.g"
void Parser::MakeExpressionStatement(void)
{
    AstExpressionStatement *p = ast_pool -> NewExpressionStatement();
    p -> expression          = (AstExpression *) Sym(1);
    p -> semicolon_token_opt = 0;
    Sym(1) = p;
}

//
// Rule 164:  StatementExpression ::= PreIncrementExpression
//
// void MakeExpressionStatement(void);
//

//
// Rule 165:  StatementExpression ::= PreDecrementExpression
//
// void MakeExpressionStatement(void);
//

//
// Rule 166:  StatementExpression ::= PostIncrementExpression
//
// void MakeExpressionStatement(void);
//

//
// Rule 167:  StatementExpression ::= PostDecrementExpression
//
// void MakeExpressionStatement(void);
//

//
// Rule 168:  StatementExpression ::= MethodInvocation
//
// void MakeExpressionStatement(void);
//

//
// Rule 169:  StatementExpression ::= ClassInstanceCreationExpression
//
// void MakeExpressionStatement(void);
//

//
// Rule 170:  IfThenStatement ::= if LPAREN Expression RPAREN Statement
//
#line 2793 "java.g"
void Parser::Act170(void)
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

//
// Rule 171:  IfThenElseStatement ::= if LPAREN Expression RPAREN StatementNoShortIf else Statement
//
#line 2817 "java.g"
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

//
// Rule 172:  IfThenElseStatementNoShortIf ::= if LPAREN Expression RPAREN StatementNoShortIf else StatementNoShortIf
//
// void MakeIfThenElseStatement(void);
//

//
// Rule 173:  SwitchStatement ::= switch LPAREN Expression RPAREN SwitchBlock
//
#line 2858 "java.g"
void Parser::Act173(void)
{
    AstSwitchStatement *p = (AstSwitchStatement *) Sym(5);
    p -> switch_token = Token(1);
    p -> expression   = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 174:  SwitchBlock ::= LBRACE RBRACE
//
#line 2870 "java.g"
void Parser::Act174(void)
{
    AstSwitchStatement *p = ast_pool -> NewSwitchStatement();

    AstBlock *block = ast_pool -> NewBlock();
    block -> left_brace_token  = Token(1);
    block -> right_brace_token = Token(2);

    p -> switch_block = block;

    Sym(1) = p;
}

//
// Rule 175:  SwitchBlock ::= LBRACE SwitchBlockStatements RBRACE
//
#line 2887 "java.g"
void Parser::Act175(void)
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

//
// Rule 176:  SwitchBlock ::= LBRACE SwitchLabels RBRACE
//
#line 2916 "java.g"
void Parser::Act176(void)
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

//
// Rule 177:  SwitchBlock ::= LBRACE SwitchBlockStatements SwitchLabels RBRACE
//
#line 2953 "java.g"
void Parser::Act177(void)
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

//
// Rule 178:  SwitchBlockStatements ::= SwitchBlockStatement
//
#line 3004 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act178(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 179:  SwitchBlockStatements ::= SwitchBlockStatements SwitchBlockStatement
//
#line 3021 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act179(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 180:  SwitchBlockStatement ::= SwitchLabels BlockStatements
//
#line 3042 "java.g"
void Parser::Act180(void)
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

//
// Rule 181:  SwitchLabels ::= SwitchLabel
//
#line 3081 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act181(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 182:  SwitchLabels ::= SwitchLabels SwitchLabel
//
#line 3098 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act182(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 183:  SwitchLabel ::= case ConstantExpression COLON
//
#line 3119 "java.g"
void Parser::Act183(void)
{
    AstCaseLabel *p = ast_pool -> NewCaseLabel();
    p -> case_token  = Token(1);
    p -> expression  = (AstExpression *) Sym(2);
    p -> colon_token = Token(3);
    Sym(1) = p;
}

//
// Rule 184:  SwitchLabel ::= default COLON
//
#line 3132 "java.g"
void Parser::Act184(void)
{
    AstDefaultLabel *p = ast_pool -> NewDefaultLabel();
    p -> default_token = Token(1);
    p -> colon_token   = Token(2);
    Sym(1) = p;
}

//
// Rule 185:  WhileStatement ::= while LPAREN Expression RPAREN Statement
//
#line 3144 "java.g"
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

//
// Rule 186:  WhileStatementNoShortIf ::= while LPAREN Expression RPAREN StatementNoShortIf
//
// void MakeWhileStatement(void);
//

//
// Rule 187:  DoStatement ::= do Statement while LPAREN Expression RPAREN SEMICOLON
//
#line 3171 "java.g"
void Parser::Act187(void)
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

//
// Rule 188:  ForStatement ::= for LPAREN ForInitopt SEMICOLON Expressionopt SEMICOLON ForUpdateopt RPAREN Statement
//
#line 3193 "java.g"
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

//
// Rule 189:  ForStatementNoShortIf ::= for LPAREN ForInitopt SEMICOLON Expressionopt SEMICOLON ForUpdateopt RPAREN...
//
// void MakeForStatement(void);
//

//
// Rule 190:  ForInit -> StatementExpressionList
//
// void NoAction(void);
//

//
// Rule 191:  ForInit ::= LocalVariableDeclaration
//
#line 3248 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act191(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 192:  ForUpdate -> StatementExpressionList
//
// void NoAction(void);
//

//
// Rule 193:  StatementExpressionList ::= StatementExpression
//
#line 3269 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act193(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 194:  StatementExpressionList ::= StatementExpressionList COMMA StatementExpression
//
#line 3286 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act194(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 195:  BreakStatement ::= break SEMICOLON
//
#line 3313 "java.g"
void Parser::Act195(void)
{
    AstBreakStatement *p = ast_pool -> NewBreakStatement();
    p -> break_token          = Token(1);
    p -> identifier_token_opt = 0;
    p -> semicolon_token      = Token(2);
    Sym(1) = p;
}

//
// Rule 196:  BreakStatement ::= break Identifier SEMICOLON
//
#line 3326 "java.g"
void Parser::Act196(void)
{
    AstBreakStatement *p = ast_pool -> NewBreakStatement();
    p -> break_token          = Token(1);
    p -> identifier_token_opt = Token(2);
    p -> semicolon_token      = Token(3);
    Sym(1) = p;
}

//
// Rule 197:  ContinueStatement ::= continue SEMICOLON
//
#line 3339 "java.g"
void Parser::Act197(void)
{
    AstContinueStatement *p = ast_pool -> NewContinueStatement();
    p -> continue_token       = Token(1);
    p -> identifier_token_opt = 0;
    p -> semicolon_token      = Token(2);
    Sym(1) = p;
}

//
// Rule 198:  ContinueStatement ::= continue Identifier SEMICOLON
//
#line 3352 "java.g"
void Parser::Act198(void)
{
    AstContinueStatement *p = ast_pool -> NewContinueStatement();
    p -> continue_token       = Token(1);
    p -> identifier_token_opt = Token(2);
    p -> semicolon_token      = Token(3);
    Sym(1) = p;
}

//
// Rule 199:  ReturnStatement ::= return Expressionopt SEMICOLON
//
#line 3365 "java.g"
void Parser::Act199(void)
{
    AstReturnStatement *p = ast_pool -> NewReturnStatement();
    p -> return_token    = Token(1);
    p -> expression_opt  = (AstExpression *) Sym(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}

//
// Rule 200:  ThrowStatement ::= throw Expression SEMICOLON
//
#line 3378 "java.g"
void Parser::Act200(void)
{
    AstThrowStatement *p = ast_pool -> NewThrowStatement();
    p -> throw_token     = Token(1);
    p -> expression      = (AstExpression *) Sym(2);
    p -> semicolon_token = Token(3);
    Sym(1) = p;
}

//
// Rule 201:  SynchronizedStatement ::= synchronized LPAREN Expression RPAREN Block
//
#line 3391 "java.g"
void Parser::Act201(void)
{
    AstSynchronizedStatement *p = ast_pool -> NewSynchronizedStatement();
    p -> synchronized_token = Token(1);
    p -> expression         = (AstExpression *) Sym(3);
    p -> block              = (AstBlock *) Sym(5);
    p -> block -> block_tag = AstBlock::SYNCHRONIZED;

    Sym(1) = p;
}

//
// Rule 202:  TryStatement ::= try Block Catches
//
#line 3406 "java.g"
void Parser::Act202(void)
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

//
// Rule 203:  TryStatement ::= try Block Catchesopt Finally
//
#line 3434 "java.g"
void Parser::Act203(void)
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

//
// Rule 204:  Catches ::= CatchClause
//
#line 3466 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act204(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 205:  Catches ::= Catches CatchClause
//
#line 3483 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act205(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 206:  CatchClause ::= catch LPAREN FormalParameter RPAREN Block
//
#line 3504 "java.g"
void Parser::Act206(void)
{
    AstCatchClause *p = ast_pool -> NewCatchClause();
    p -> catch_token      = Token(1);
    p -> formal_parameter = (AstFormalParameter *) Sym(3);
    p -> block            = (AstBlock *) Sym(5);

    Sym(1) = p;
}

//
// Rule 207:  Finally ::= finally Block
//
#line 3518 "java.g"
void Parser::Act207(void)
{
    AstFinallyClause *p     = ast_pool -> NewFinallyClause();
    p -> finally_token      = Token(1);
    p -> block              = (AstBlock *) Sym(2);
    p -> block -> block_tag = AstBlock::FINALLY;

    Sym(1) = p;
}

//
// Rule 208:  Primary -> PrimaryNoNewArray
//
// void NoAction(void);
//

//
// Rule 209:  Primary -> ArrayCreationExpression
//
// void NoAction(void);
//

//
// Rule 210:  PrimaryNoNewArray -> Literal
//
// void NoAction(void);
//

//
// Rule 211:  PrimaryNoNewArray ::= this
//
#line 3546 "java.g"
void Parser::Act211(void)
{
    Sym(1) = ast_pool -> NewThisExpression(Token(1));
}

//
// Rule 212:  PrimaryNoNewArray ::= LPAREN Expression RPAREN
//
#line 3555 "java.g"
void Parser::Act212(void)
{
    AstParenthesizedExpression *p = ast_pool -> NewParenthesizedExpression();
    p -> left_parenthesis_token = Token(1);
    p -> expression = (AstExpression *) Sym(2);
    p -> right_parenthesis_token = Token(3);
    Sym(1) = p;
}

//
// Rule 213:  PrimaryNoNewArray -> ClassInstanceCreationExpression
//
// void NoAction(void);
//

//
// Rule 214:  PrimaryNoNewArray -> FieldAccess
//
// void NoAction(void);
//

//
// Rule 215:  PrimaryNoNewArray ::= Name DOT this
//
#line 3577 "java.g"
void Parser::Act215(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess(AstFieldAccess::THIS_TAG);
    p -> base = (AstExpression *) Sym(1);
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}

//
// Rule 216:  PrimaryNoNewArray ::= Type DOT class
//
#line 3591 "java.g"
void Parser::Act216(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess(AstFieldAccess::CLASS_TAG);
    p -> base = ast_pool -> NewTypeExpression(Sym(1));
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}

//
// Rule 217:  PrimaryNoNewArray ::= void DOT class
//
#line 3605 "java.g"
void Parser::Act217(void)
{
    AstFieldAccess *p = ast_pool -> NewFieldAccess(AstFieldAccess::CLASS_TAG);
    p -> base = ast_pool -> NewTypeExpression(ast_pool -> NewPrimitiveType(Ast::VOID_TYPE, Token(1)));
    p -> dot_token = Token(2);
    p -> identifier_token = Token(3);
    Sym(1) = p;
}

//
// Rule 218:  PrimaryNoNewArray -> MethodInvocation
//
// void NoAction(void);
//

//
// Rule 219:  PrimaryNoNewArray -> ArrayAccess
//
// void NoAction(void);
//

//
// Rule 220:  ClassInstanceCreationExpression ::= new ClassType LPAREN ArgumentListopt RPAREN ClassBodyopt
//
#line 3631 "java.g"
void Parser::Act220(void)
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

//
// Rule 221:  ClassInstanceCreationExpression ::= Primary DOT new SimpleName LPAREN ArgumentListopt RPAREN ClassBodyopt
//
#line 3661 "java.g"
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

//
// Rule 222:  ClassInstanceCreationExpression ::= Name DOT new SimpleName LPAREN ArgumentListopt RPAREN ClassBodyopt
//
// void MakeQualifiedNew(void);
//

//
// Rule 223:  ArgumentList ::= Expression
//
#line 3698 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act223(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 224:  ArgumentList ::= ArgumentList COMMA Expression
//
#line 3715 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act224(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(3);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 225:  ArrayCreationExpression ::= new PrimitiveType DimExprs Dimsopt
//
#line 3736 "java.g"
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

//
// Rule 226:  ArrayCreationExpression ::= new ClassOrInterfaceType DimExprs Dimsopt
//
// void MakeArrayCreationExpression(void);
//

//
// Rule 227:  ArrayCreationExpression ::= new ArrayType ArrayInitializer
//
#line 3784 "java.g"
void Parser::Act227(void)
{
    AstArrayCreationExpression *p = ast_pool -> NewArrayCreationExpression();
    p -> new_token             = Token(1);
    p -> array_type            = Sym(2);
    p -> array_initializer_opt = (AstArrayInitializer *) Sym(3);
    Sym(1) = p;
}

//
// Rule 228:  DimExprs ::= DimExpr
//
#line 3797 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act228(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = Sym(1);
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 229:  DimExprs ::= DimExprs DimExpr
//
#line 3814 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act229(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = Sym(2);
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 230:  DimExpr ::= LBRACKET Expression RBRACKET
//
#line 3835 "java.g"
void Parser::Act230(void)
{
    AstDimExpr *p = ast_pool -> NewDimExpr();
    p -> left_bracket_token  = Token(1);
    p -> expression          = (AstExpression *) Sym(2);
    p -> right_bracket_token = Token(3);
    Sym(1) = p;
}

//
// Rule 231:  Dims ::= LBRACKET RBRACKET
//
#line 3848 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act231(void)
{
    AstListNode *p = AllocateListNode();
    p -> next = p;
    p -> element = ast_pool -> NewBrackets(Token(1), Token(2));
    p -> index = 0;

    Sym(1) = p;
}

//
// Rule 232:  Dims ::= Dims LBRACKET RBRACKET
//
#line 3865 "java.g"
//
// Note that the list is circular so as to preserve the order of the elements
//
void Parser::Act232(void)
{
    AstListNode *tail = (AstListNode *) Sym(1);

    AstListNode *p = AllocateListNode();
    p -> element = ast_pool -> NewBrackets(Token(2), Token(3));
    p -> index = tail -> index + 1;

    p -> next = tail -> next;
    tail -> next = p;

    Sym(1) = p;
}

//
// Rule 233:  FieldAccess ::= Primary DOT Identifier
//
// void MakeFieldAccess(void);
//

//
// Rule 234:  FieldAccess ::= super DOT Identifier
//
#line 3893 "java.g"
void Parser::MakeSuperFieldAccess(void)
{
    Sym(1) = ast_pool -> NewSuperExpression(Token(1));

    MakeFieldAccess();
}

//
// Rule 235:  FieldAccess ::= Name DOT super DOT Identifier
//
#line 3905 "java.g"
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

//
// Rule 236:  MethodInvocation ::= Name LPAREN ArgumentListopt RPAREN
//
#line 3925 "java.g"
void Parser::Act236(void)
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

//
// Rule 237:  MethodInvocation ::= Primary DOT Identifier LPAREN ArgumentListopt RPAREN
//
#line 3950 "java.g"
void Parser::Act237(void)
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

//
// Rule 238:  MethodInvocation ::= super DOT Identifier LPAREN ArgumentListopt RPAREN
//
#line 3977 "java.g"
void Parser::Act238(void)
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

//
// Rule 239:  MethodInvocation ::= Name DOT super DOT Identifier LPAREN ArgumentListopt RPAREN
//
#line 4005 "java.g"
void Parser::Act239(void)
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

//
// Rule 240:  ArrayAccess ::= Name LBRACKET Expression RBRACKET
//
#line 4032 "java.g"
void Parser::MakeArrayAccess(void)
{
    AstArrayAccess *p = ast_pool -> NewArrayAccess();
    p -> base                = (AstExpression *) Sym(1);
    p -> left_bracket_token  = Token(2);
    p -> expression          = (AstExpression *) Sym(3);
    p -> right_bracket_token = Token(4);
    Sym(1) = p;
}

//
// Rule 241:  ArrayAccess ::= PrimaryNoNewArray LBRACKET Expression RBRACKET
//
// void MakeArrayAccess(void);
//

//
// Rule 242:  PostfixExpression -> Primary
//
// void NoAction(void);
//

//
// Rule 243:  PostfixExpression -> Name
//
// void NoAction(void);
//

//
// Rule 244:  PostfixExpression -> PostIncrementExpression
//
// void NoAction(void);
//

//
// Rule 245:  PostfixExpression -> PostDecrementExpression
//
// void NoAction(void);
//

//
// Rule 246:  PostIncrementExpression ::= PostfixExpression PLUS_PLUS
//
#line 4069 "java.g"
void Parser::Act246(void)
{
    AstPostUnaryExpression *p = ast_pool -> NewPostUnaryExpression(AstPostUnaryExpression::PLUSPLUS);
    p -> expression          = (AstExpression *) Sym(1);
    p -> post_operator_token = Token(2);
    Sym(1) = p;
}

//
// Rule 247:  PostDecrementExpression ::= PostfixExpression MINUS_MINUS
//
#line 4081 "java.g"
void Parser::Act247(void)
{
    AstPostUnaryExpression *p = ast_pool -> NewPostUnaryExpression(AstPostUnaryExpression::MINUSMINUS);
    p -> expression          = (AstExpression *) Sym(1);
    p -> post_operator_token = Token(2);
    Sym(1) = p;
}

//
// Rule 248:  UnaryExpression -> PreIncrementExpression
//
// void NoAction(void);
//

//
// Rule 249:  UnaryExpression -> PreDecrementExpression
//
// void NoAction(void);
//

//
// Rule 250:  UnaryExpression ::= PLUS UnaryExpression
//
#line 4101 "java.g"
void Parser::Act250(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::PLUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}

//
// Rule 251:  UnaryExpression ::= MINUS UnaryExpression
//
#line 4113 "java.g"
void Parser::Act251(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::MINUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}

//
// Rule 252:  UnaryExpression -> UnaryExpressionNotPlusMinus
//
// void NoAction(void);
//

//
// Rule 253:  PreIncrementExpression ::= PLUS_PLUS UnaryExpression
//
#line 4129 "java.g"
void Parser::Act253(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::PLUSPLUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}

//
// Rule 254:  PreDecrementExpression ::= MINUS_MINUS UnaryExpression
//
#line 4141 "java.g"
void Parser::Act254(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::MINUSMINUS);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}

//
// Rule 255:  UnaryExpressionNotPlusMinus -> PostfixExpression
//
// void NoAction(void);
//

//
// Rule 256:  UnaryExpressionNotPlusMinus ::= TWIDDLE UnaryExpression
//
#line 4157 "java.g"
void Parser::Act256(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::TWIDDLE);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}

//
// Rule 257:  UnaryExpressionNotPlusMinus ::= NOT UnaryExpression
//
#line 4169 "java.g"
void Parser::Act257(void)
{
    AstPreUnaryExpression *p = ast_pool -> NewPreUnaryExpression(AstPreUnaryExpression::NOT);
    p -> pre_operator_token = Token(1);
    p -> expression         = (AstExpression *) Sym(2);
    Sym(1) = p;
}

//
// Rule 258:  UnaryExpressionNotPlusMinus -> CastExpression
//
// void NoAction(void);
//

//
// Rule 259:  CastExpression ::= LPAREN PrimitiveType Dimsopt RPAREN UnaryExpression
//
#line 4185 "java.g"
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

//
// Rule 260:  CastExpression ::= LPAREN Expression RPAREN UnaryExpressionNotPlusMinus
//
#line 4211 "java.g"
void Parser::Act260(void)
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

//
// Rule 261:  CastExpression ::= LPAREN Name Dims RPAREN UnaryExpressionNotPlusMinus
//
// void MakeCastExpression(void);
//

//
// Rule 262:  MultiplicativeExpression -> UnaryExpression
//
// void NoAction(void);
//

//
// Rule 263:  MultiplicativeExpression ::= MultiplicativeExpression MULTIPLY UnaryExpression
//
#line 4241 "java.g"
void Parser::Act263(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::STAR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 264:  MultiplicativeExpression ::= MultiplicativeExpression DIVIDE UnaryExpression
//
#line 4254 "java.g"
void Parser::Act264(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::SLASH);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 265:  MultiplicativeExpression ::= MultiplicativeExpression REMAINDER UnaryExpression
//
#line 4267 "java.g"
void Parser::Act265(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::MOD);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 266:  AdditiveExpression -> MultiplicativeExpression
//
// void NoAction(void);
//

//
// Rule 267:  AdditiveExpression ::= AdditiveExpression PLUS MultiplicativeExpression
//
#line 4284 "java.g"
void Parser::Act267(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::PLUS);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 268:  AdditiveExpression ::= AdditiveExpression MINUS MultiplicativeExpression
//
#line 4297 "java.g"
void Parser::Act268(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::MINUS);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 269:  ShiftExpression -> AdditiveExpression
//
// void NoAction(void);
//

//
// Rule 270:  ShiftExpression ::= ShiftExpression LEFT_SHIFT AdditiveExpression
//
#line 4314 "java.g"
void Parser::Act270(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::LEFT_SHIFT);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 271:  ShiftExpression ::= ShiftExpression RIGHT_SHIFT AdditiveExpression
//
#line 4327 "java.g"
void Parser::Act271(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::RIGHT_SHIFT);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 272:  ShiftExpression ::= ShiftExpression UNSIGNED_RIGHT_SHIFT AdditiveExpression
//
#line 4340 "java.g"
void Parser::Act272(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::UNSIGNED_RIGHT_SHIFT);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 273:  RelationalExpression -> ShiftExpression
//
// void NoAction(void);
//

//
// Rule 274:  RelationalExpression ::= RelationalExpression LESS ShiftExpression
//
#line 4357 "java.g"
void Parser::Act274(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::LESS);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 275:  RelationalExpression ::= RelationalExpression GREATER ShiftExpression
//
#line 4370 "java.g"
void Parser::Act275(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::GREATER);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 276:  RelationalExpression ::= RelationalExpression LESS_EQUAL ShiftExpression
//
#line 4383 "java.g"
void Parser::Act276(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::LESS_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 277:  RelationalExpression ::= RelationalExpression GREATER_EQUAL ShiftExpression
//
#line 4396 "java.g"
void Parser::Act277(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::GREATER_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 278:  RelationalExpression ::= RelationalExpression instanceof ReferenceType
//
#line 4409 "java.g"
void Parser::Act278(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::INSTANCEOF);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = ast_pool -> NewTypeExpression(Sym(3));
    Sym(1) = p;
}

//
// Rule 279:  EqualityExpression -> RelationalExpression
//
// void NoAction(void);
//

//
// Rule 280:  EqualityExpression ::= EqualityExpression EQUAL_EQUAL RelationalExpression
//
#line 4426 "java.g"
void Parser::Act280(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::EQUAL_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 281:  EqualityExpression ::= EqualityExpression NOT_EQUAL RelationalExpression
//
#line 4439 "java.g"
void Parser::Act281(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::NOT_EQUAL);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 282:  AndExpression -> EqualityExpression
//
// void NoAction(void);
//

//
// Rule 283:  AndExpression ::= AndExpression AND EqualityExpression
//
#line 4457 "java.g"
void Parser::Act283(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::AND);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 284:  ExclusiveOrExpression -> AndExpression
//
// void NoAction(void);
//

//
// Rule 285:  ExclusiveOrExpression ::= ExclusiveOrExpression XOR AndExpression
//
#line 4474 "java.g"
void Parser::Act285(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::XOR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 286:  InclusiveOrExpression -> ExclusiveOrExpression
//
// void NoAction(void);
//

//
// Rule 287:  InclusiveOrExpression ::= InclusiveOrExpression OR ExclusiveOrExpression
//
#line 4491 "java.g"
void Parser::Act287(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::IOR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 288:  ConditionalAndExpression -> InclusiveOrExpression
//
// void NoAction(void);
//

//
// Rule 289:  ConditionalAndExpression ::= ConditionalAndExpression AND_AND InclusiveOrExpression
//
#line 4508 "java.g"
void Parser::Act289(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::AND_AND);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 290:  ConditionalOrExpression -> ConditionalAndExpression
//
// void NoAction(void);
//

//
// Rule 291:  ConditionalOrExpression ::= ConditionalOrExpression OR_OR ConditionalAndExpression
//
#line 4525 "java.g"
void Parser::Act291(void)
{
    AstBinaryExpression *p = ast_pool -> NewBinaryExpression(AstBinaryExpression::OR_OR);
    p -> left_expression       = (AstExpression *) Sym(1);
    p -> binary_operator_token = Token(2);
    p -> right_expression      = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 292:  ConditionalExpression -> ConditionalOrExpression
//
// void NoAction(void);
//

//
// Rule 293:  ConditionalExpression ::= ConditionalOrExpression QUESTION Expression COLON ConditionalExpression
//
#line 4542 "java.g"
void Parser::Act293(void)
{
    AstConditionalExpression *p = ast_pool -> NewConditionalExpression();
    p -> test_expression  = (AstExpression *) Sym(1);
    p -> question_token   = Token(2);
    p -> true_expression  = (AstExpression *) Sym(3);
    p -> colon_token      = Token(4);
    p -> false_expression = (AstExpression *) Sym(5);
    Sym(1) = p;
}

//
// Rule 294:  AssignmentExpression -> ConditionalExpression
//
// void NoAction(void);
//

//
// Rule 295:  AssignmentExpression -> Assignment
//
// void NoAction(void);
//

//
// Rule 296:  Assignment ::= LeftHandSide AssignmentOperator AssignmentExpression
//
#line 4565 "java.g"
void Parser::Act296(void)
{
    AstAssignmentExpression *p = (AstAssignmentExpression *) Sym(2);
    p -> left_hand_side = (AstExpression *) Sym(1);
    p -> expression     = (AstExpression *) Sym(3);
    Sym(1) = p;
}

//
// Rule 297:  LeftHandSide -> Name
//
// void NoAction(void);
//

//
// Rule 298:  LeftHandSide -> FieldAccess
//
// void NoAction(void);
//

//
// Rule 299:  LeftHandSide -> ArrayAccess
//
// void NoAction(void);
//

//
// Rule 300:  AssignmentOperator ::= EQUAL
//
#line 4589 "java.g"
void Parser::Act300(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::SIMPLE_EQUAL, Token(1));
}

//
// Rule 301:  AssignmentOperator ::= MULTIPLY_EQUAL
//
#line 4598 "java.g"
void Parser::Act301(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::STAR_EQUAL, Token(1));
}

//
// Rule 302:  AssignmentOperator ::= DIVIDE_EQUAL
//
#line 4607 "java.g"
void Parser::Act302(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::SLASH_EQUAL, Token(1));
}

//
// Rule 303:  AssignmentOperator ::= REMAINDER_EQUAL
//
#line 4616 "java.g"
void Parser::Act303(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::MOD_EQUAL, Token(1));
}

//
// Rule 304:  AssignmentOperator ::= PLUS_EQUAL
//
#line 4625 "java.g"
void Parser::Act304(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::PLUS_EQUAL, Token(1));
}

//
// Rule 305:  AssignmentOperator ::= MINUS_EQUAL
//
#line 4634 "java.g"
void Parser::Act305(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::MINUS_EQUAL, Token(1));
}

//
// Rule 306:  AssignmentOperator ::= LEFT_SHIFT_EQUAL
//
#line 4643 "java.g"
void Parser::Act306(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::LEFT_SHIFT_EQUAL, Token(1));
}

//
// Rule 307:  AssignmentOperator ::= RIGHT_SHIFT_EQUAL
//
#line 4652 "java.g"
void Parser::Act307(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::RIGHT_SHIFT_EQUAL, Token(1));
}

//
// Rule 308:  AssignmentOperator ::= UNSIGNED_RIGHT_SHIFT_EQUAL
//
#line 4661 "java.g"
void Parser::Act308(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::UNSIGNED_RIGHT_SHIFT_EQUAL, Token(1));
}

//
// Rule 309:  AssignmentOperator ::= AND_EQUAL
//
#line 4670 "java.g"
void Parser::Act309(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::AND_EQUAL, Token(1));
}

//
// Rule 310:  AssignmentOperator ::= XOR_EQUAL
//
#line 4679 "java.g"
void Parser::Act310(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::XOR_EQUAL, Token(1));
}

//
// Rule 311:  AssignmentOperator ::= OR_EQUAL
//
#line 4688 "java.g"
void Parser::Act311(void)
{
    Sym(1) = ast_pool -> NewAssignmentExpression(AstAssignmentExpression::IOR_EQUAL, Token(1));
}

//
// Rule 312:  Expression -> AssignmentExpression
//
// void NoAction(void);
//

//
// Rule 313:  ConstantExpression -> Expression
//
// void NoAction(void);
//

//
// Rule 314:  PackageDeclarationopt ::=
//
#line 4711 "java.g"
//
// Given a rule of the form A ::= x1 x2 ... xn
//
// Construct a NULL Ast for A.
//
void Parser::NullAction(void) { Sym(1) = NULL; }

//
// Rule 315:  PackageDeclarationopt -> PackageDeclaration
//
// void NoAction(void);
//

//
// Rule 316:  Superopt ::=
//
// void NullAction(void);
//

//
// Rule 317:  Superopt -> Super
//
// void NoAction(void);
//

//
// Rule 318:  Expressionopt ::=
//
// void NullAction(void);
//

//
// Rule 319:  Expressionopt -> Expression
//
// void NoAction(void);
//

//
// Rule 320:  ClassBodyopt ::=
//
// void NullAction(void);
//

//
// Rule 321:  ClassBodyopt -> ClassBody
//
// void NoAction(void);
//

//
// Rule 322:  ,opt ::=
//
// void NullAction(void);
//

//
// Rule 323:  ,opt -> COMMA
//
// void NoAction(void);
//

//
// Rule 324:  ImportDeclarationsopt ::=
//
// void NullAction(void);
//

//
// Rule 325:  ImportDeclarationsopt -> ImportDeclarations
//
// void NoAction(void);
//

//
// Rule 326:  TypeDeclarationsopt ::=
//
// void NullAction(void);
//

//
// Rule 327:  TypeDeclarationsopt -> TypeDeclarations
//
// void NoAction(void);
//

//
// Rule 328:  ClassBodyDeclarationsopt ::=
//
// void NullAction(void);
//

//
// Rule 329:  ClassBodyDeclarationsopt -> ClassBodyDeclarations
//
// void NoAction(void);
//

//
// Rule 330:  Modifiersopt ::=
//
// void NullAction(void);
//

//
// Rule 331:  Modifiersopt -> Modifiers
//
// void NoAction(void);
//

//
// Rule 332:  BlockStatementsopt ::=
//
// void NullAction(void);
//

//
// Rule 333:  BlockStatementsopt -> BlockStatements
//
// void NoAction(void);
//

//
// Rule 334:  Dimsopt ::=
//
// void NullAction(void);
//

//
// Rule 335:  Dimsopt -> Dims
//
// void NoAction(void);
//

//
// Rule 336:  ArgumentListopt ::=
//
// void NullAction(void);
//

//
// Rule 337:  ArgumentListopt -> ArgumentList
//
// void NoAction(void);
//

//
// Rule 338:  Throwsopt ::=
//
// void NullAction(void);
//

//
// Rule 339:  Throwsopt -> Throws
//
// void NoAction(void);
//

//
// Rule 340:  FormalParameterListopt ::=
//
// void NullAction(void);
//

//
// Rule 341:  FormalParameterListopt -> FormalParameterList
//
// void NoAction(void);
//

//
// Rule 342:  Interfacesopt ::=
//
// void NullAction(void);
//

//
// Rule 343:  Interfacesopt -> Interfaces
//
// void NoAction(void);
//

//
// Rule 344:  InterfaceMemberDeclarationsopt ::=
//
// void NullAction(void);
//

//
// Rule 345:  InterfaceMemberDeclarationsopt -> InterfaceMemberDeclarations
//
// void NoAction(void);
//

//
// Rule 346:  ForInitopt ::=
//
// void NullAction(void);
//

//
// Rule 347:  ForInitopt -> ForInit
//
// void NoAction(void);
//

//
// Rule 348:  ForUpdateopt ::=
//
// void NullAction(void);
//

//
// Rule 349:  ForUpdateopt -> ForUpdate
//
// void NoAction(void);
//

//
// Rule 350:  ExtendsInterfacesopt ::=
//
// void NullAction(void);
//

//
// Rule 351:  ExtendsInterfacesopt -> ExtendsInterfaces
//
// void NoAction(void);
//

//
// Rule 352:  Catchesopt ::=
//
// void NullAction(void);
//

//
// Rule 353:  Catchesopt -> Catches
//
// void NoAction(void);
//

//
// Rule 354:  PackageHeaderMarker ::=
//
#line 4892 "java.g"
//
// When this function is invoked, if the "parse_package_header_only" flag
// is turned on, we skip to the end-of-file token.
//
void Parser::Act354(void)
{
    if (parse_package_header_only)
        lex_stream -> Reset(lex_stream -> NumTokens() - 1); // point to the EOF token
    Sym(1) = NULL;
}

//
// Rule 355:  MethodHeaderMarker ::=
//
#line 4907 "java.g"
//
// When this function is invoked, if the "parse_header_only" flag
// is turned on, the body of the method being parsed is skipped.
//
void Parser::Act355(void)
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
