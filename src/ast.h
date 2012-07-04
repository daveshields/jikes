// $Id: ast.h,v 1.65 2003/10/06 12:48:30 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002, 2003 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef ast_INCLUDED
#define ast_INCLUDED

#include "platform.h"
#include "stream.h"
#include "depend.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


class Parser;
class SemanticEnvironment;
class LiteralValue;
class Symbol;
class BlockSymbol;
class VariableSymbol;
class MethodSymbol;
class TypeSymbol;
class StoragePool;
struct CaseElement;

class VariableSymbolArray
{
    typedef VariableSymbol* T;

    T** base;
    size_t base_size;
    int top;
    int size;
    StoragePool* pool;
    unsigned short log_blksize;
    unsigned short base_increment;

    inline size_t Blksize() { return 1 << log_blksize; }

    //
    // Allocate another block of storage for the VariableSymbol array.
    //
    void AllocateMoreSpace();

public:
    //
    // This function is used to reset the size of a VariableSymbol array
    // without allocating or deallocting space. It may be invoked with an
    // integer argument n which indicates the new size or with no argument
    // which indicates that the size should be reset to 0.
    //
    void Reset(const int n = 0)
    {
        if (n < 0 || n > size)
            assert(false);
        top = n;
    }

    //
    // Return length of the VariableSymbol array.
    //
    unsigned Length() { return top; }

    //
    // Return a reference to the ith element of the VariableSymbol array.
    //
    // Note that no check is made here to ensure that 0 <= i < top.
    // Such a check might be useful for debugging and a range exception
    // should be thrown if it yields true.
    //
    T& operator[](const int i) { return base[i >> log_blksize][i]; }

    //
    // Add an element to the VariableSymbol array and return the top index.
    //
    int NextIndex()
    {
        int i = top++;
        if (i == size)
            AllocateMoreSpace();
        return i;
    }

    //
    // Add an element to the VariableSymbol array and return a reference to
    // that new element.
    //
    T& Next() { int i = NextIndex(); return base[i >> log_blksize][i]; }

    //
    // Constructor of a VariableSymbol array.
    //
    VariableSymbolArray(StoragePool*, unsigned);

    //
    // Destructor of an VariableSymbol array.
    //
    ~VariableSymbolArray() { assert(false); }
};


//***************************************************************************
//
// TODO: This documentation is a bit out of date...
//
// This file contains the definitions of the classes used to construct the
// AST representation of a Java program.
//
// The node Ast is a base class of all other classes. (The name of the other
// classes start with the prefix "Ast".) The nodes associated with executable
// statements (e.g., AstIfStatement) are subclasses of AstStatement and nodes
// associated with expressions (e.g., AstBinaryExpression) are subclasses of
// AstExpression.
//
// The information contained in the AST nodes is described by a grammar where
// each rule consists of a left-hand side nonterminal followed by "-->"
// followed by a right-hand side symbol or a sequence enclosed in the pair of
// symbols "<" and ">". In defining the symbols, the following notation is
// used:
//
// Symbols that are capitalized (e.g., Type) are nonterminals. Symbols that are
// in all upper case (e.g., PACKAGE) represent node kinds. Symbols that contain
// the substring "_token" represents tokens in the source file. The suffix
// "_opt" indicates that a symbol is optional. For example, if Super_opt
// appears in a rule, it indicates that either Super or null can be expected.
// When a symbol is plural (e.g., Modifiers), it indicates zero or more
// instances of such a symbol (a list to be precise) can be expected. Thus,
// when "Modifiers" is specified in the right-hand side of a rule either no
// Modifier or a sequence of them may appear.
//
// Implementation Notes:
//
//    A complete AST tree for a Java program always contains an
//    AstCompilationUnit root node. The kind of that node is
//    Ast::EMPTY_COMPILATION for a tree with no type declaration,
//    Ast::COMPILATION for a tree constructed from an otherwise valid program
//    and Ast::BAD_COMPILATION for a tree constructed from an invalid program.
//
//    All AST tree nodes belong to a StoragePool. When a new node must be
//    added, it is allocated from the same StoragePool. Thus, you should
//    never use operator new to construct an AST object, but instead use New*
//    or Gen* defined in StoragePool. Likewise, AST tree nodes never need
//    destruction - simply delete the pool to reclaim the entire tree.
//
//    When the preprocessor variable JIKES_DEBUG is defined the user may print
//    out an AST tree to standard output by calling the virtual function
//    "Print" for the root node of the tree.
//
//    DynamicArrays are used to implement lists. This representation has the
//    advantage of being very flexible and easy to use. However, it may be
//    slightly less time-efficient than a straightforward linked list. My
//    guess is no more than 10% which justifies this use, but that should be
//    checked at some point...
//
//***************************************************************************
//
// This is a complete list of all Ast nodes declared here to allow
// forward references.
//
class Ast;
template <typename T> class AstArray;
class AstListNode;
class AstDeclared;
class AstDeclaredType;
class AstStatement;
class AstExpression;
class AstType;

class AstBlock;
class AstName;
class AstPrimitiveType;
class AstBrackets;
class AstArrayType;
class AstTypeName;
class AstPackageDeclaration;
class AstImportDeclaration;
class AstCompilationUnit;
class AstModifiers;
class AstEmptyDeclaration;
class AstClassBody;
class AstClassDeclaration;
class AstArrayInitializer;
class AstVariableDeclaratorId;
class AstVariableDeclarator;
class AstFieldDeclaration;
class AstFormalParameter;
class AstMethodDeclarator;
class AstMethodBody;
class AstMethodDeclaration;
class AstInitializerDeclaration;
class AstArguments;
class AstThisCall;
class AstSuperCall;
class AstConstructorDeclaration;
class AstInterfaceDeclaration;
class AstLocalVariableDeclarationStatement;
class AstLocalClassDeclarationStatement;
class AstIfStatement;
class AstEmptyStatement;
class AstExpressionStatement;
class AstSwitchLabel;
class AstSwitchBlockStatement;
class AstSwitchStatement;
class AstWhileStatement;
class AstDoStatement;
class AstForStatement;
class AstBreakStatement;
class AstContinueStatement;
class AstReturnStatement;
class AstThrowStatement;
class AstSynchronizedStatement;
class AstAssertStatement;
class AstCatchClause;
class AstFinallyClause;
class AstTryStatement;
class AstIntegerLiteral;
class AstLongLiteral;
class AstFloatLiteral;
class AstDoubleLiteral;
class AstTrueLiteral;
class AstFalseLiteral;
class AstStringLiteral;
class AstCharacterLiteral;
class AstNullLiteral;
class AstClassLiteral;
class AstThisExpression;
class AstSuperExpression;
class AstParenthesizedExpression;
class AstClassInstanceCreationExpression;
class AstDimExpr;
class AstArrayCreationExpression;
class AstFieldAccess;
class AstMethodInvocation;
class AstArrayAccess;
class AstPostUnaryExpression;
class AstPreUnaryExpression;
class AstCastExpression;
class AstBinaryExpression;
class AstInstanceofExpression;
class AstConditionalExpression;
class AstAssignmentExpression;

//
// The Ast base node.
//
class Ast
{
public:
    //
    // These tags are used to identify nodes that can represent more than
    // one kind of objects.
    //
    enum AstTag
    {
        NO_TAG,
        PRIMITIVE_TYPE,
        STATEMENT,
        EXPRESSION,

        _num_tags = EXPRESSION,

        STATIC,
        UNPARSED
    };

    //
    // These are the different kinds for the Ast objects.
    //
    enum AstKind
    {
        AST, // must be first
        // Expressions
        NAME,
        DOT,
        INTEGER_LITERAL,
        LONG_LITERAL,
        FLOAT_LITERAL,
        DOUBLE_LITERAL,
        TRUE_LITERAL,
        FALSE_LITERAL,
        STRING_LITERAL,
        CHARACTER_LITERAL,
        NULL_LITERAL,
        CLASS_LITERAL,
        THIS_EXPRESSION,
        SUPER_EXPRESSION,
        PARENTHESIZED_EXPRESSION,
        ARRAY_ACCESS,
        CALL,
        CLASS_CREATION,
        ARRAY_CREATION,
        POST_UNARY,
        PRE_UNARY,
        CAST,
        BINARY,
        INSTANCEOF,
        CONDITIONAL,
        ASSIGNMENT,
        _num_expression_kinds,
        // Statements
        THIS_CALL,
        SUPER_CALL,
        BLOCK,
        IF,
        EMPTY_STATEMENT,
        EXPRESSION_STATEMENT,
        SWITCH,
        SWITCH_BLOCK,
        LOCAL_VARIABLE_DECLARATION,
        LOCAL_CLASS,
        WHILE,
        DO,
        FOR,
        BREAK,
        CONTINUE,
        RETURN,
        THROW,
        SYNCHRONIZED_STATEMENT,
        ASSERT,
        TRY,
        _num_expr_or_stmt_kinds,
        // All others
        ARGUMENTS = _num_expr_or_stmt_kinds,
        DIM,
        LIST_NODE,
        INT,
        DOUBLE,
        CHAR,
        LONG,
        FLOAT,
        BYTE,
        SHORT,
        BOOLEAN,
        VOID_TYPE,
        ARRAY,
        TYPE,
        COMPILATION,
        BAD_COMPILATION,
        EMPTY_COMPILATION,
        PACKAGE_COMPONENT,
        PACKAGE_NAME,
        PACKAGE,
        IMPORT,
        EMPTY_DECLARATION,
        CLASS,
        CLASS_BODY,
        MODIFIERS,
        FIELD,
        VARIABLE_DECLARATOR,
        VARIABLE_DECLARATOR_NAME,
        BRACKETS,
        METHOD,
        METHOD_DECLARATOR,
        PARAMETER,
        CONSTRUCTOR,
        INTERFACE,
        ARRAY_INITIALIZER,
        INITIALIZER,
        METHOD_BODY,
        SWITCH_LABEL,
        CATCH,
        FINALLY,

        _num_kinds
    };

#ifdef JIKES_DEBUG
    typedef AstKind Kind;
    typedef AstTag Tag;
#else
    typedef unsigned short Kind;
    typedef unsigned char Tag;
#endif

    Kind kind; // every node has a unique kind...
    // Some subsets of nodes are grouped together to form a class of nodes.
    Tag class_tag;

    //
    // "generated" is a boolean value that indicates whether or not a node
    // is associated with a construct in a source file or that is was generated
    // by the compiler. See functions "gen_ ..." and "new_ ..." below.
    //
    bool generated;

#ifdef JIKES_DEBUG
    unsigned id;
    static unsigned count;
    static bool debug_unparse;

    Ast() : id(++count)
    {}
#endif

    //
    // ASTs should not be destructed. Instead, delete the containing
    // StoragePool.
    //
    virtual ~Ast() { assert(false); }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&) = 0;
    virtual void Unparse(Ostream&, LexStream*) = 0;
#endif

    //
    // General queries.
    //
    bool IsLeftHandSide();
    bool IsExplicitConstructorInvocation();
    bool IsGenerated();

    //
    // The Conversion functions below are provided as a convenient way to
    // cast a generic Ast node into a specific node. Note that if one knows
    // the type of a node for sure, it is more efficient to use a specific
    // cast expression. For example, if one knows that a "Ast* p" pointer
    // dereferences a FieldDeclaration then a cast expression should be
    // used to cast p, as follows:
    //
    //       AstFieldDeclaration* fp = (FieldDeclaration*) p;
    //
    // However, if p points to a ClassBodyDeclaration which may be
    // either a FieldDeclaration, MethodDeclaration, ConstructorDeclaration,
    // InitializerDeclaration, ClassDeclaration, or an InterfaceDeclaration,
    // then the following sequence of code may be used:
    //
    //    AstFieldDeclaration* fp;
    //    AstMethodDeclaration* mp;
    //    AstConstructorDeclaration* cp;
    //    AstInitializerDeclaration* sp;
    //    AstClassDeclaration* Cp; // 1.1 only
    //    AstInterfaceDeclaration* Ip; // 1.1 only
    //
    //    if (fp = p -> FieldDeclarationCast())
    //        ...
    //    else if (mp = p -> MethodDeclarationCast())
    //        ...
    //    else if (cp = p -> ConstructorDeclarationCast())
    //        ...
    //    else if (sp = p -> InitializerDeclarationCast())
    //        ...
    //    else if (Cp = p -> ClassDeclarationCast())
    //        ...
    //    else if (Ip = p -> InterfaceDeclarationCast())
    //        ...
    //

    //
    // These cast functions are used for classes that represent more than
    // one kind of nodes.  The functions must be listed after the subclasses
    // have been defined.
    //
    inline AstStatement* StatementCast();
    inline AstExpression* ExpressionCast();
    inline AstPrimitiveType* PrimitiveTypeCast();
    inline AstFieldDeclaration* StaticFieldCast();
    inline AstInitializerDeclaration* StaticInitializerCast();
    inline AstClassBody* UnparsedClassBodyCast();
    inline AstCompilationUnit* BadCompilationUnitCast();
    inline AstCompilationUnit* EmptyCompilationUnitCast();

    //
    // These cast functions are used for classes that represent exactly
    // one kind of node.
    //
    inline AstListNode* ListNodeCast();
    inline AstBlock* BlockCast();
    inline AstName* NameCast();
    inline AstBrackets* BracketsCast();
    inline AstArrayType* ArrayTypeCast();
    inline AstTypeName* TypeNameCast();
    inline AstPackageDeclaration* PackageDeclarationCast();
    inline AstImportDeclaration* ImportDeclarationCast();
    inline AstCompilationUnit* CompilationUnitCast();
    inline AstModifiers* ModifiersCast();
    inline AstEmptyDeclaration* EmptyDeclarationCast();
    inline AstClassBody* ClassBodyCast();
    inline AstClassDeclaration* ClassDeclarationCast();
    inline AstArrayInitializer* ArrayInitializerCast();
    inline AstVariableDeclaratorId* VariableDeclaratorIdCast();
    inline AstVariableDeclarator* VariableDeclaratorCast();
    inline AstFieldDeclaration* FieldDeclarationCast();
    inline AstFormalParameter* FormalParameterCast();
    inline AstMethodDeclarator* MethodDeclaratorCast();
    inline AstMethodBody* MethodBodyCast();
    inline AstMethodDeclaration* MethodDeclarationCast();
    inline AstInitializerDeclaration* InitializerDeclarationCast();
    inline AstArguments* ArgumentsCast();
    inline AstThisCall* ThisCallCast();
    inline AstSuperCall* SuperCallCast();
    inline AstConstructorDeclaration* ConstructorDeclarationCast();
    inline AstInterfaceDeclaration* InterfaceDeclarationCast();
    inline AstLocalVariableDeclarationStatement*
    LocalVariableDeclarationStatementCast();
    inline AstLocalClassDeclarationStatement*
    LocalClassDeclarationStatementCast();
    inline AstIfStatement* IfStatementCast();
    inline AstEmptyStatement* EmptyStatementCast();
    inline AstExpressionStatement* ExpressionStatementCast();
    inline AstSwitchLabel* SwitchLabelCast();
    inline AstSwitchBlockStatement* SwitchBlockStatementCast();
    inline AstSwitchStatement* SwitchStatementCast();
    inline AstWhileStatement* WhileStatementCast();
    inline AstDoStatement* DoStatementCast();
    inline AstForStatement* ForStatementCast();
    inline AstBreakStatement* BreakStatementCast();
    inline AstContinueStatement* ContinueStatementCast();
    inline AstReturnStatement* ReturnStatementCast();
    inline AstThrowStatement* ThrowStatementCast();
    inline AstSynchronizedStatement* SynchronizedStatementCast();
    inline AstAssertStatement* AssertStatementCast();
    inline AstCatchClause* CatchClauseCast();
    inline AstFinallyClause* FinallyClauseCast();
    inline AstTryStatement* TryStatementCast();
    inline AstIntegerLiteral* IntegerLiteralCast();
    inline AstLongLiteral* LongLiteralCast();
    inline AstFloatLiteral* FloatLiteralCast();
    inline AstDoubleLiteral* DoubleLiteralCast();
    inline AstTrueLiteral* TrueLiteralCast();
    inline AstFalseLiteral* FalseLiteralCast();
    inline AstStringLiteral* StringLiteralCast();
    inline AstCharacterLiteral* CharacterLiteralCast();
    inline AstNullLiteral* NullLiteralCast();
    inline AstClassLiteral* ClassLiteralCast();
    inline AstThisExpression* ThisExpressionCast();
    inline AstSuperExpression* SuperExpressionCast();
    inline AstParenthesizedExpression* ParenthesizedExpressionCast();
    inline AstClassInstanceCreationExpression*
    ClassInstanceCreationExpressionCast();
    inline AstDimExpr* DimExprCast();
    inline AstArrayCreationExpression* ArrayCreationExpressionCast();
    inline AstFieldAccess* FieldAccessCast();
    inline AstMethodInvocation* MethodInvocationCast();
    inline AstArrayAccess* ArrayAccessCast();
    inline AstPostUnaryExpression* PostUnaryExpressionCast();
    inline AstPreUnaryExpression* PreUnaryExpressionCast();
    inline AstCastExpression* CastExpressionCast();
    inline AstBinaryExpression* BinaryExpressionCast();
    inline AstInstanceofExpression* InstanceofExpressionCast();
    inline AstConditionalExpression* ConditionalExpressionCast();
    inline AstAssignmentExpression* AssignmentExpressionCast();

    //
    // It would be nice if this could be covariant, as it would allow
    // less casting. But MSVC++ can't yet handle covariant return
    // types at all, and both GCC and HP's aCC croak with covariance during
    // multiple inheritance (bummer). So, there is a bunch of hideous casting
    // in ast.cpp that could otherwise be avoided if standards were followed.
    //
    // Clones are used for various things, such as pre-evaluating final
    // constant values.
    //
    virtual Ast* Clone(StoragePool*) = 0;

    //
    // These functions return the left and right tokens of this tree branch.
    //
    virtual LexStream::TokenIndex LeftToken() = 0;
    virtual LexStream::TokenIndex RightToken() = 0;
};


//
// This AstArray template class can be used to construct a dynamic
// array of arbitrary objects. The space for the array is allocated in
// blocks of size 2**LOG_BLKSIZE. In declaring a Ast array the user
// may specify a value for LOG_BLKSIZE which by default is 6. Also,
// as the array is implemented using a base+offset strategy, the user
// may also specify the number of "slots" to add to the base when the
// current base runs out of space. Each slot points to a block.
//
template <typename T>
class AstArray
{
    T** base;
    size_t base_size;
    int top;
    int size;
    StoragePool* pool;
    unsigned short log_blksize;
    unsigned short base_increment;

    inline size_t Blksize() { return 1 << log_blksize; }

    //
    // Allocate another block of storage for the Ast array.
    //
    void AllocateMoreSpace();

public:
    //
    // This function is used to reset the size of a Ast array without
    // allocating or deallocting space. It may be invoked with an integer
    // argument n which indicates the new size or with no argument which
    // indicates that the size should be reset to 0.
    //
    void Reset(const int n = 0)
    {
        if (n < 0 || n > size)
            assert(false);
        top = n;
    }

    //
    // Return length of the Ast array.
    //
    unsigned Length() { return top; }

    //
    // Return a reference to the ith element of the Ast array.
    //
    // Note that no check is made here to ensure that 0 <= i < top.
    // Such a check might be useful for debugging and a range exception
    // should be thrown if it yields true.
    //
    T& operator[](const int i) { return base[i >> log_blksize][i]; }

    //
    // Add an element to the Ast array and return the top index.
    //
    int NextIndex()
    {
        int i = top++;
        if (i == size)
            AllocateMoreSpace();
        return i;
    }

    //
    // Add an element to the Ast array and return a reference to
    // that new element.
    //
    T& Next() { int i = NextIndex(); return base[i >> log_blksize][i]; }

    inline void Push(T elt) { Next() = elt; }
    // Not "return (*this)[--top]" because that may violate an invariant
    // in operator[].
    inline T Pop()
    {
        assert(top!=0);
        top--;
        return base[top >> log_blksize][top];
    }
    inline T Top() { assert(top!=0); return (*this)[top-1]; }

    //
    // Constructor of a ast array.
    //
    AstArray(StoragePool*, unsigned);

    //
    // Destructor of an Ast array.
    //
    ~AstArray() { assert(false); }
};


//
// The Ast list node. This is a temporary object used in constructing lists
// while parsing the grammar; once constructed, the contents are extracted
// and this list is reclaimed. It is circular to make insertion easy while
// maintaining declaration order.
//
class AstListNode : public Ast
{
public:
    AstListNode* next;
    Ast* element;
    unsigned index;

    AstListNode()
    {
        kind = LIST_NODE;
        class_tag = NO_TAG;
        generated = false;
#ifdef JIKES_DEBUG
        --count; // don't count these nodes
#endif
    }

    ~AstListNode() {}

    //
    // These next three functions should never be called, since list nodes
    // only exist long enough to create the AST tree and then are reclaimed.
    //
    virtual Ast* Clone(StoragePool*) { assert(false); return NULL; }
#ifdef JIKES_DEBUG
    virtual void Print(LexStream&) { assert(false); }
    virtual void Unparse(Ostream&, LexStream*) { assert(false); }
#endif

    virtual LexStream::TokenIndex LeftToken()
    {
        return element -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return element -> RightToken();
    }
};


//
// This class adds some type safety. It represents all member declarations
// in types. See AstFieldDeclaration, AstMethodDeclaration,
// AstConstructorDeclaration, AstInitializerDeclaration, DeclaredType.
//
class AstDeclared : public Ast
{
public:
    AstModifiers* modifiers_opt;

    //
    // For efficiency, there is no constructor. Subclasses are expected to set
    // modifiers_opt to NULL in their constructor.
    //

    ~AstDeclared() {}
};


//
// This class adds some type safety. It represents all type declarations.
// See AstClassDeclaration, AstInterfaceDeclaration, AstEmptyDeclaration.
//
class AstDeclaredType : public AstDeclared
{
public:
    AstClassBody* class_body;

    //
    // For efficiency, there is no constructor. Subclasses are expected to set
    // class_body to NULL in their constructor.
    //

    ~AstDeclaredType() {}

    inline bool IsValid();
};


//
// This class represents statements.
//
class AstStatement : public Ast
{
public:
    bool is_reachable;
    bool can_complete_normally;

    //
    // Note that for efficiency reasons AstStatement does not have a
    // constructor. Therefore, subclasses that are derived from AstStatement
    // are expected to initialize the fields is_reachable and
    // can_complete_normally appropriately.
    //

    ~AstStatement() {}
};


class AstExpression : public Ast
{
public:
    LiteralValue* value;
    Symbol* symbol;

    //
    // Note that for efficiency reasons AstExpression does not have a
    // constructor. However, subclasses that are derived from AstExpression
    // are expected to initialize the fields value and symbol to NULL as
    // indicated below:
    //
    // AstExpression() : value(NULL),
    //                   symbol(NULL)
    // {}
    //

    ~AstExpression() {}

    bool IsConstant() { return value != NULL; }

    TypeSymbol* Type();
};


//
// This is the superclass of constructs which represent a type.
//
class AstType : public Ast
{
public:
    TypeSymbol* symbol;

    //
    // Note that for efficiency reasons AstType does not have a constructor.
    // Subclasses that are derived from AstType are expected to initialize
    // symbol to NULL.
    //

    ~AstType() {}

    virtual LexStream::TokenIndex IdentifierToken() = 0;
};


//
// Block --> <BLOCK, {_token, BlockStatements, }_token>
//
// BlockStatement --> LocalVariableDeclarationStatement
//                  | Statement
//                  | ClassDeclaration
//
class AstBlock : public AstStatement
{
protected:
    StoragePool* pool;

private:
    AstArray<AstStatement*>* block_statements;
    VariableSymbolArray* defined_variables;

public:
    enum BlockTag
    {
        NONE,
        TRY_CLAUSE_WITH_FINALLY,
        TRY_CLAUSE_WITH_CATCH,
        ABRUPT_TRY_FINALLY,
        FINALLY,
        SYNCHRONIZED,
        SWITCH
    };
    BlockTag block_tag;

    BlockSymbol* block_symbol;
    unsigned nesting_level;

    LexStream::TokenIndex label_opt;
    LexStream::TokenIndex left_brace_token;
    LexStream::TokenIndex right_brace_token;

    bool no_braces;

    AstBlock(StoragePool* p)
        : pool(p),
          block_statements(NULL),
          defined_variables(NULL),
          block_tag(NONE),
          block_symbol(NULL),
          nesting_level(0),
          label_opt(LexStream::BadToken()),
          no_braces(false)
    {
        kind = BLOCK;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstBlock() {}

    inline AstStatement*& Statement(unsigned i)
    {
        return (*block_statements)[i];
    }
    inline unsigned NumStatements()
    {
        return block_statements ? block_statements -> Length() : 0;
    }
    inline void AllocateStatements(unsigned estimate = 1);
    inline void AddStatement(AstStatement*);

    inline VariableSymbol*& LocallyDefinedVariable(unsigned i)
    {
        return (*defined_variables)[i];
    }
    inline unsigned NumLocallyDefinedVariables()
    {
        return defined_variables ? defined_variables -> Length() : 0;
    }
    inline void AllocateLocallyDefinedVariables(unsigned estimate = 1);
    inline void AddLocallyDefinedVariable(VariableSymbol*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return left_brace_token; }
    virtual LexStream::TokenIndex RightToken() { return right_brace_token; }

protected:
    void CloneBlock(StoragePool*, AstBlock*);
};


//
// Simple and qualified names.
//
class AstName : public AstExpression
{
public:
    AstName* base_opt;
    LexStream::TokenIndex identifier_token;

    //
    // When a name refers to a member in an enclosing scope, it is mapped
    // into an expression that creates a path to the member in question.
    //
    AstExpression* resolution_opt;

    AstName(LexStream::TokenIndex token)
        : base_opt(NULL),
          identifier_token(token),
          resolution_opt(NULL)
    {
        kind = NAME;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstName() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return base_opt ? base_opt -> LeftToken() : identifier_token;
    }
    virtual LexStream::TokenIndex RightToken() { return identifier_token; }
};


//
// Type --> PrimitiveType
//        | ReferenceType
//
// PrimitiveType --> <PrimitiveKind, PrimitiveName>
//
// PrimitiveKind --> BYTE | SHORT | INT | LONG | CHAR | FLOAT | DOUBLE |
//                   BOOLEAN | VOID
//
// PrimitiveName --> byte_token | short_token | int_token | long_token |
//                   char_token | float_token | double_token | boolean_token |
//                   void_token
//
class AstPrimitiveType : public AstType
{
public:
    LexStream::TokenIndex primitive_kind_token;

    AstPrimitiveType(Kind k, LexStream::TokenIndex token)
        : primitive_kind_token(token)
    {
        kind = k;
        class_tag = PRIMITIVE_TYPE;
        generated = false;
        symbol = NULL;
    }
    ~AstPrimitiveType() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return primitive_kind_token; }
    virtual LexStream::TokenIndex RightToken() { return primitive_kind_token; }
    virtual LexStream::TokenIndex IdentifierToken()
    {
        return primitive_kind_token;
    }
};


//
// Represents one or more pairs of '[' ']'.
//
class AstBrackets : public Ast
{
public:
    LexStream::TokenIndex left_bracket_token;
    LexStream::TokenIndex right_bracket_token;

    unsigned dims;

    AstBrackets(LexStream::TokenIndex l, LexStream::TokenIndex r)
        : left_bracket_token(l),
          right_bracket_token(r),
          dims(1)
    {
        kind = BRACKETS;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstBrackets() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return left_bracket_token; }
    virtual LexStream::TokenIndex RightToken() { return right_bracket_token; }
};


//
// ReferenceType --> ClassType
//                 | ArrayType
//
// ClassType --> Name
//
// ArrayType --> <ARRAY, ArrayKind, [_token, ]_token>
//
// ArrayKind --> PrimitiveType
//             | Name
//             | ArrayType
//
class AstArrayType : public AstType
{
public:
    AstType* type; // AstPrimitiveType, AstTypeName
    AstBrackets* brackets;

    AstArrayType(AstType* t, AstBrackets* b)
        : type(t),
          brackets(b)
    {
        kind = ARRAY;
        class_tag = NO_TAG;
        generated = false;
        symbol = NULL;
    }
    ~AstArrayType() {}

    inline unsigned NumBrackets() { return brackets -> dims; }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return type -> LeftToken(); }
    virtual LexStream::TokenIndex RightToken()
    {
        return brackets -> right_bracket_token;
    }
    virtual LexStream::TokenIndex IdentifierToken()
    {
        return type -> IdentifierToken();
    }
};


//
// Represents a type. Occurs in several contexts - imports; supertypes;
// throws clauses; parameter, field, and method return types; qualified this
// and super; class literals; casts.
//
class AstTypeName : public AstType
{
public:
    AstName* name;

    AstTypeName(StoragePool*, AstName* n)
        : name(n)
    {
        kind = TYPE;
        class_tag = NO_TAG;
        generated = false;
        symbol = NULL;
    }
    ~AstTypeName() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return name -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return name -> identifier_token;
    }
    virtual LexStream::TokenIndex IdentifierToken()
    {
        return name -> identifier_token;
    }
};


//
// PackageDeclaration --> <PACKAGE, package_token, Name, ;_token>
//
class AstPackageDeclaration : public Ast
{
public:
    LexStream::TokenIndex package_token;
    AstName* name;
    LexStream::TokenIndex semicolon_token;

    AstPackageDeclaration()
    {
        kind = PACKAGE;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstPackageDeclaration() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return package_token; }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ImportDeclaration --> <IMPORT, import_token, Name, *_token_opt, ;_token>
//
class AstImportDeclaration : public Ast
{
public:
    LexStream::TokenIndex import_token;
    AstName* name;
    LexStream::TokenIndex star_token_opt;
    LexStream::TokenIndex semicolon_token;

    AstImportDeclaration()
        : star_token_opt(LexStream::BadToken())
    {
        kind = IMPORT;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstImportDeclaration() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return import_token; }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// The root node for compilation.
//
class AstCompilationUnit : public Ast
{
    AstArray<AstImportDeclaration*>* import_declarations;
    AstArray<AstDeclaredType*>* type_declarations;

public:
    StoragePool* ast_pool;

    AstPackageDeclaration* package_declaration_opt;

    AstCompilationUnit(StoragePool* p)
        : import_declarations(NULL),
          type_declarations(NULL),
          ast_pool(p),
          package_declaration_opt(NULL)
    {
        kind = COMPILATION;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstCompilationUnit() {}

    void FreeAst();

    inline AstImportDeclaration*& ImportDeclaration(unsigned i)
    {
        return (*import_declarations)[i];
    }
    inline unsigned NumImportDeclarations()
    {
        return import_declarations ? import_declarations -> Length() : 0;
    }
    inline void AllocateImportDeclarations(unsigned estimate = 1);
    inline void AddImportDeclaration(AstImportDeclaration*);

    inline AstDeclaredType*& TypeDeclaration(unsigned i)
    {
        return (*type_declarations)[i];
    }
    inline unsigned NumTypeDeclarations()
    {
        return type_declarations ? type_declarations -> Length() : 0;
    }
    inline void AllocateTypeDeclarations(unsigned estimate = 1);
    inline void AddTypeDeclaration(AstDeclaredType*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);

    // special forms
    virtual void Unparse(LexStream*, const char* const directory);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        if (package_declaration_opt)
            return package_declaration_opt -> package_token;
        if (NumImportDeclarations())
            return ImportDeclaration(0) -> import_token;
        if (NumTypeDeclarations())
            return TypeDeclaration(0) -> LeftToken();
        return 0;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        if (NumTypeDeclarations())
            return TypeDeclaration(NumTypeDeclarations() - 1) -> RightToken();
        if (NumImportDeclarations())
            return ImportDeclaration(NumImportDeclarations() - 1) ->
                semicolon_token;
        if (package_declaration_opt)
            return package_declaration_opt -> semicolon_token;
        return 0;
    }
};


//
// Represents one or more modifier tokens ('public', 'protected', 'private',
// 'static', 'abstract', 'final', 'native', 'synchronized', 'transient',
// 'volatile', and 'strictfp').
//
class AstModifiers : public Ast
{
public:
    LexStream::TokenIndex left_modifier_token;
    LexStream::TokenIndex right_modifier_token;

    // Store the location of the first 'static' token encountered.
    LexStream::TokenIndex static_token_opt;

    AstModifiers(LexStream::TokenIndex token)
        : left_modifier_token(token),
          right_modifier_token(token),
          static_token_opt(LexStream::BadToken())
    {
        kind = MODIFIERS;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstModifiers() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return left_modifier_token; }
    virtual LexStream::TokenIndex RightToken() { return right_modifier_token; }
};


//
// EmptyDeclaration --> <EMPTY_DECLARATION, ;_token>
//
class AstEmptyDeclaration : public AstDeclaredType
{
public:
    LexStream::TokenIndex semicolon_token;

    AstEmptyDeclaration(LexStream::TokenIndex token)
        : semicolon_token(token)
    {
        kind = EMPTY_DECLARATION;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
        class_body = NULL;
    }
    ~AstEmptyDeclaration() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return semicolon_token; }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ClassBody --> <CLASS_BODY, {_token, ClassBodyDeclarations, }_token>
//
class AstClassBody : public Ast
{
    friend class Parser;

    StoragePool* pool;
    AstArray<AstDeclared*>* class_body_declarations;

    AstArray<AstFieldDeclaration*>* instance_variables;
    AstArray<AstFieldDeclaration*>* class_variables;
    AstArray<AstMethodDeclaration*>* methods;
    AstArray<AstConstructorDeclaration*>* constructors;
    AstArray<AstInitializerDeclaration*>* static_initializers;
    AstArray<AstInitializerDeclaration*>* instance_initializers;
    AstArray<AstClassDeclaration*>* inner_classes;
    AstArray<AstInterfaceDeclaration*>* inner_interfaces;
    AstArray<AstEmptyDeclaration*>* empty_declarations;

public:
    SemanticEnvironment* semantic_environment;
    AstConstructorDeclaration* default_constructor;

    //
    // Filled in by the owning AstClassDeclaration, AstInterfaceDeclaration,
    // or AstClassInstanceCreationExpression to allow nicer error messages.
    // Note that owner is null for anonymous classes.
    //
    AstDeclaredType* owner;
    LexStream::TokenIndex identifier_token;

    //
    // The actual delimiters of the class body.
    //
    LexStream::TokenIndex left_brace_token;
    LexStream::TokenIndex right_brace_token;

    AstClassBody(StoragePool* p)
        : pool(p),
          class_body_declarations(NULL),
          instance_variables(NULL),
          class_variables(NULL),
          methods(NULL),
          constructors(NULL),
          static_initializers(NULL),
          instance_initializers(NULL),
          inner_classes(NULL),
          inner_interfaces(NULL),
          empty_declarations(NULL),
          semantic_environment(NULL),
          default_constructor(NULL),
          identifier_token(LexStream::BadToken())
    {
        kind = CLASS_BODY;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstClassBody() {}

    inline void MarkUnparsed() { class_tag = UNPARSED; }
    inline void MarkParsed() { class_tag = NO_TAG; }

    inline AstDeclared*& ClassBodyDeclaration(unsigned i)
    {
        return (*class_body_declarations)[i];
    }
    inline unsigned NumClassBodyDeclarations()
    {
        return class_body_declarations
            ? class_body_declarations -> Length() : 0;
    }
    inline void AllocateClassBodyDeclarations(unsigned estimate = 1);
    inline void AddClassBodyDeclaration(AstDeclared*);
    inline void AddClassBodyDeclarationNicely(AstDeclared*);

    inline AstFieldDeclaration*& InstanceVariable(unsigned i)
    {
        return (*instance_variables)[i];
    }
    inline unsigned NumInstanceVariables()
    {
        return instance_variables ? instance_variables -> Length() : 0;
    }
    inline void AllocateInstanceVariables(unsigned estimate = 1);
    inline void AddInstanceVariable(AstFieldDeclaration*);

    inline AstFieldDeclaration*& ClassVariable(unsigned i)
    {
        return (*class_variables)[i];
    }
    inline unsigned NumClassVariables()
    {
        return class_variables ? class_variables -> Length() : 0;
    }
    inline void AllocateClassVariables(unsigned estimate = 1);
    inline void AddClassVariable(AstFieldDeclaration*);

    inline AstMethodDeclaration*& Method(unsigned i) { return (*methods)[i]; }
    inline unsigned NumMethods()
    {
        return methods ? methods -> Length() : 0;
    }
    inline void AllocateMethods(unsigned estimate = 1);
    inline void AddMethod(AstMethodDeclaration*);

    inline AstConstructorDeclaration*& Constructor(unsigned i)
    {
        return (*constructors)[i];
    }
    inline unsigned NumConstructors()
    {
        return constructors ? constructors -> Length() : 0;
    }
    inline void AllocateConstructors(unsigned estimate = 1);
    inline void AddConstructor(AstConstructorDeclaration*);

    inline AstInitializerDeclaration*& StaticInitializer(unsigned i)
    {
        return (*static_initializers)[i];
    }
    inline unsigned NumStaticInitializers()
    {
        return static_initializers ? static_initializers -> Length() : 0;
    }
    inline void AllocateStaticInitializers(unsigned estimate = 1);
    inline void AddStaticInitializer(AstInitializerDeclaration*);

    inline AstInitializerDeclaration*& InstanceInitializer(unsigned i)
    {
        return (*instance_initializers)[i];
    }
    inline unsigned NumInstanceInitializers()
    {
        return instance_initializers ? instance_initializers -> Length() : 0;
    }
    inline void AllocateInstanceInitializers(unsigned estimate = 1);
    inline void AddInstanceInitializer(AstInitializerDeclaration*);

    inline AstClassDeclaration*& NestedClass(unsigned i)
    {
        return (*inner_classes)[i];
    }
    inline unsigned NumNestedClasses()
    {
        return inner_classes ? inner_classes -> Length() : 0;
    }
    inline void AllocateNestedClasses(unsigned estimate = 1);
    inline void AddNestedClass(AstClassDeclaration*);

    inline AstInterfaceDeclaration*& NestedInterface(unsigned i)
    {
        return (*inner_interfaces)[i];
    }
    inline unsigned NumNestedInterfaces()
    {
        return inner_interfaces ? inner_interfaces -> Length() : 0;
    }
    inline void AllocateNestedInterfaces(unsigned estimate = 1);
    inline void AddNestedInterface(AstInterfaceDeclaration*);

    inline AstEmptyDeclaration*& EmptyDeclaration(unsigned i)
    {
        return (*empty_declarations)[i];
    }
    inline unsigned NumEmptyDeclarations()
    {
        return empty_declarations ? empty_declarations -> Length() : 0;
    }
    inline void AllocateEmptyDeclarations(unsigned estimate = 1);
    inline void AddEmptyDeclaration(AstEmptyDeclaration*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return left_brace_token; }
    virtual LexStream::TokenIndex RightToken() { return right_brace_token; }
};



//
// Represents a class declaration.
//
class AstClassDeclaration : public AstDeclaredType
{
    StoragePool* pool;
    AstArray<AstTypeName*>* interfaces;

public:
    LexStream::TokenIndex class_token;
    AstTypeName* super_opt;

    AstClassDeclaration(StoragePool* p)
        : pool(p),
          interfaces(NULL),
          super_opt(NULL)
    {
        kind = CLASS;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
    }
    ~AstClassDeclaration() {}

    inline AstTypeName*& Interface(unsigned i) { return (*interfaces)[i]; }
    inline unsigned NumInterfaces()
    {
        return interfaces ? interfaces -> Length() : 0;
    }
    inline void AllocateInterfaces(unsigned estimate = 1);
    inline void AddInterface(AstTypeName*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : class_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return class_body -> right_brace_token;
    }
};


//
// VariableInitializer --> Expression
//                       | ArrayInitializer
//
// ArrayInitializer --> <ARRAY_INITIALIZER, {_token, VariableInitializers,
// }_token>
//
class AstArrayInitializer : public Ast
{
    StoragePool* pool;
    AstArray<Ast*>* variable_initializers;

public:
    LexStream::TokenIndex left_brace_token;
    LexStream::TokenIndex right_brace_token;

    AstArrayInitializer(StoragePool* p)
        : pool(p),
          variable_initializers(NULL)
    {
        kind = ARRAY_INITIALIZER;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstArrayInitializer() {}

    inline Ast*& VariableInitializer(unsigned i)
    {
        return (*variable_initializers)[i];
    }
    inline unsigned NumVariableInitializers()
    {
        return variable_initializers ? variable_initializers -> Length() : 0;
    }
    inline void AllocateVariableInitializers(unsigned estimate = 1);
    inline void AddVariableInitializer(Ast*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return left_brace_token; }
    virtual LexStream::TokenIndex RightToken() { return right_brace_token; }
};


//
// VariableDeclaratorId --> <VARIABLE_DECLARATOR_NAME, identifier_token,
// Brackets>
//
class AstVariableDeclaratorId : public Ast
{
public:
    LexStream::TokenIndex identifier_token;
    AstBrackets* brackets_opt;

    AstVariableDeclaratorId()
        : brackets_opt(NULL)
    {
        kind = VARIABLE_DECLARATOR_NAME;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstVariableDeclaratorId() {}

    inline unsigned NumBrackets()
    {
        return brackets_opt ? brackets_opt -> dims : 0;
    }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return identifier_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return brackets_opt ? brackets_opt -> right_bracket_token
            : identifier_token;
    }
};


//
// VariableDeclarator --> <VARIABLE_DECLARATOR, VariableDeclaratorId,
//     VariableInitializer_opt>
//
// Technically, this is not a statement. But it is similar to local variable
// declarations, which are, and treating it as a statement makes compiling
// initializer blocks more uniform.
//
class AstVariableDeclarator : public AstStatement
{
public:
    VariableSymbol* symbol;

    // when true, this variable signals that the variable_initializer_opt
    // for this variable is currently being evaluated
    bool pending;

    AstVariableDeclaratorId* variable_declarator_name;
    Ast* variable_initializer_opt;

    AstVariableDeclarator()
        : symbol(NULL),
          pending(false),
          variable_declarator_name(NULL),
          variable_initializer_opt(NULL)
    {
        kind = VARIABLE_DECLARATOR;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = true;
        can_complete_normally = true;
    }
    ~AstVariableDeclarator() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return variable_declarator_name -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return variable_initializer_opt
            ? variable_initializer_opt -> RightToken()
            : variable_declarator_name -> RightToken();
    }
};


//
// FieldDeclaration --> <FIELD, VariableModifiers, Type, VariableDeclarators,
// ;_token>
//
// FieldModifier --> Modifier (PUBLIC, PROTECTED, PRIVATE, FINAL, STATIC,
// TRANSIENT or VOLATILE)
//
class AstFieldDeclaration : public AstDeclared
{
    StoragePool* pool;
    AstArray<AstVariableDeclarator*>* variable_declarators;

public:
    AstType* type;
    LexStream::TokenIndex semicolon_token;

    AstFieldDeclaration(StoragePool* p)
        : pool(p),
          variable_declarators(NULL)
    {
        kind = FIELD;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
    }
    ~AstFieldDeclaration() {}

    inline void MarkStatic() { class_tag = STATIC; }

    inline AstVariableDeclarator*& VariableDeclarator(unsigned i)
    {
        return (*variable_declarators)[i];
    }
    inline unsigned NumVariableDeclarators()
    {
        return variable_declarators ? variable_declarators -> Length() : 0;
    }
    inline void AllocateVariableDeclarators(unsigned estimate = 1);
    inline void AddVariableDeclarator(AstVariableDeclarator*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : type -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// FormalParameter --> <PARAMETER, Type, VariableDeclaratorId>
//
class AstFormalParameter : public Ast
{
public:
    AstModifiers* modifiers_opt;
    AstType* type;
    AstVariableDeclarator* formal_declarator;

    AstFormalParameter()
        : modifiers_opt(NULL)
    {
        kind = PARAMETER;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstFormalParameter() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : type -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return formal_declarator -> RightToken();
    }
};


//
// MethodDeclarator --> <METHOD_DECLARATOR, identifier_token, (_token,
// FormalParameters, )_token, Brackets>
//
class AstMethodDeclarator : public Ast
{
    StoragePool* pool;
    AstArray<AstFormalParameter*>* formal_parameters;

public:
    LexStream::TokenIndex identifier_token;
    LexStream::TokenIndex left_parenthesis_token;
    LexStream::TokenIndex right_parenthesis_token;
    AstBrackets* brackets_opt;

    AstMethodDeclarator(StoragePool* p)
        : pool(p),
          formal_parameters(NULL),
          brackets_opt(NULL)
    {
        kind = METHOD_DECLARATOR;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstMethodDeclarator() {}

    inline AstFormalParameter*& FormalParameter(unsigned i)
    {
        return (*formal_parameters)[i];
    }
    inline unsigned NumFormalParameters()
    {
        return formal_parameters ? formal_parameters -> Length() : 0;
    }
    inline void AllocateFormalParameters(unsigned estimate = 1);
    inline void AddFormalParameter(AstFormalParameter*);

    inline unsigned NumBrackets()
    {
        return brackets_opt ? brackets_opt -> dims : 0;
    }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return identifier_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return brackets_opt ? brackets_opt -> right_bracket_token
            : right_parenthesis_token;
    }
};


//
// This class represents a method body, for methods, constructors, and
// initializers. It is basically a block, with the addition of an explicit
// constructor invocation (used only in the context of constructors, NULL
// otherwise).
//
class AstMethodBody : public AstBlock
{
public:
    AstStatement* explicit_constructor_opt;

    AstMethodBody(StoragePool* p)
        : AstBlock(p),
          explicit_constructor_opt(NULL)
    {
        kind = METHOD_BODY;
        class_tag = STATEMENT;
        is_reachable = true;
        can_complete_normally = false;
        no_braces = true;
    }
    ~AstMethodBody() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);
    // Inherited LeftToken(), RightToken() are adequate.
};


//
// MethodDeclaration --> <METHOD, MethodModifiers, Type, MethodDeclarator,
//    Throws, MethodBody>
//
// MethodModifier --> Modifier (PUBLIC, PROTECTED, PRIVATE, STATIC, ABSTRACT,
//    FINAL, NATIVE or SYNCHRONIZED)
//
// Throws --> Names
//
// MethodBody --> Block
//              | EmptyStatement
//
class AstMethodDeclaration : public AstDeclared
{
    StoragePool* pool;
    AstArray<AstTypeName*>* throws;

public:
    MethodSymbol* method_symbol;

    AstType* type;
    AstMethodDeclarator* method_declarator;
    AstMethodBody* method_body_opt;

    AstMethodDeclaration(StoragePool* p)
        : pool(p),
          throws(NULL),
          method_symbol(NULL),
          method_body_opt(NULL)
    {
        kind = METHOD;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
    }
    ~AstMethodDeclaration() {}

    bool IsValid() { return method_symbol != NULL; }

    bool IsSignature() { return ! method_body_opt; }

    inline AstTypeName*& Throw(unsigned i) { return (*throws)[i]; }
    inline unsigned NumThrows() { return throws ? throws -> Length() : 0; }
    inline void AllocateThrows(unsigned estimate = 1);
    inline void AddThrow(AstTypeName*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : type -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return method_body_opt ? method_body_opt -> right_brace_token
            : method_declarator -> RightToken() + 1;
    }
};


//
// This class represents static and instance initializers. It also accepts
// other modifiers, to give a nicer error message.
//
class AstInitializerDeclaration : public AstDeclared
{
public:
    AstMethodBody* block;

    AstInitializerDeclaration()
    {
        kind = INITIALIZER;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
    }
    ~AstInitializerDeclaration() {}

    inline void MarkStatic() { class_tag = STATIC; }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : block -> left_brace_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return block -> right_brace_token;
    }
};


//
// Represents the arguments of AstThisCall, AstSuperCall, AstMethodInvocation,
// and AstClassInstanceCreationExpression. For convenience, the need to add
// null argument or pass shadow parameters is contained here, even though
// not all the calling instances can use these features.
//
class AstArguments : public Ast
{
    StoragePool* pool;
    AstArray<AstExpression*>* arguments;
    AstArray<AstName*>* shadow_arguments;
    bool null_argument;

public:
    LexStream::TokenIndex left_parenthesis_token;
    LexStream::TokenIndex right_parenthesis_token;

    AstArguments(StoragePool* p,
                 LexStream::TokenIndex l, LexStream::TokenIndex r)
        : pool(p),
          arguments(NULL),
          shadow_arguments(NULL),
          null_argument(false),
          left_parenthesis_token(l),
          right_parenthesis_token(r)
    {
        kind = ARGUMENTS;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstArguments() {}

    inline AstExpression*& Argument(unsigned i) { return (*arguments)[i]; }
    inline unsigned NumArguments()
    {
        return arguments ? arguments -> Length() : 0;
    }
    inline void AllocateArguments(unsigned estimate = 1);
    inline void AddArgument(AstExpression*);

    inline AstName*& LocalArgument(unsigned i)
    {
        return (*shadow_arguments)[i];
    }
    inline unsigned NumLocalArguments()
    {
        return shadow_arguments ? shadow_arguments -> Length() : 0;
    }
    inline void AllocateLocalArguments(unsigned estimate = 1);
    inline void AddLocalArgument(AstName*);

    inline void AddNullArgument() { null_argument = true; }
    inline bool NeedsExtraNullArgument() { return null_argument; }

    //
    // TODO: Add a helper method for converting all arguments to a string
    // for error message purposes.
    //

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return left_parenthesis_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return right_parenthesis_token;
    }
};


//
// Represents an explicit call to another constructor in this class.
//
class AstThisCall : public AstStatement
{
public:
    MethodSymbol* symbol;

    LexStream::TokenIndex this_token;
    AstArguments* arguments;
    LexStream::TokenIndex semicolon_token;

    AstThisCall()
        : symbol(NULL)
    {
        kind = THIS_CALL;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = true;
        can_complete_normally = true;
    }
    ~AstThisCall() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return this_token; }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// Represents an explicit call to a superconstructor.
//
class AstSuperCall : public AstStatement
{
public:
    MethodSymbol* symbol;

    AstExpression* base_opt;
    LexStream::TokenIndex super_token;
    AstArguments* arguments;
    LexStream::TokenIndex semicolon_token;

    AstSuperCall()
        : symbol(NULL),
          base_opt(NULL)
    {
        kind = SUPER_CALL;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = true;
        can_complete_normally = true;
    }
    ~AstSuperCall() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return base_opt ? base_opt -> LeftToken() : super_token;
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ConstructorDeclaration --> <CONSTRUCTOR, ConstructorModifiers,
//     MethodDeclarator, Throws, ConstructorBody>
//
// ConstructorBody --> <METHOD_BODY, {_token,
//     ExplicitConstructorInvocation, BlockStatements, }_token>
//
// ConstructorModifier --> Modifier (PUBLIC, PROTECTED or PRIVATE)
//
// ExplicitConstructorInvocation --> ThisCall
//                                 | SuperCall
//
// NOTE: We do not actually build ConstructorBodies. Instead, we have
// overloaded MethodBody to store the necessary information. This is
// because this() and super() are treated as Statements in the grammar;
// and in the bytecode, constructors are just methods with a special
// name.
//
class AstConstructorDeclaration : public AstDeclared
{
    StoragePool* pool;
    AstArray<AstTypeName*>* throws;

public:
    MethodSymbol* constructor_symbol;
    int index; // Used in depend.cpp to detect cycles.

    AstMethodDeclarator* constructor_declarator;
    AstMethodBody* constructor_body;

    AstConstructorDeclaration(StoragePool* p)
        : pool(p),
          throws(NULL),
          constructor_symbol(NULL),
          index(ConstructorCycleChecker::OMEGA)
    {
        kind = CONSTRUCTOR;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
    }
    ~AstConstructorDeclaration() {}

    bool IsValid() { return constructor_symbol != NULL; }

    inline AstTypeName*& Throw(unsigned i) { return (*throws)[i]; }
    inline unsigned NumThrows() { return throws ? throws -> Length() : 0; }
    inline void AllocateThrows(unsigned estimate = 1);
    inline void AddThrow(AstTypeName*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : constructor_declarator -> identifier_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return constructor_body -> right_brace_token;
    }
};


//
// InterfaceDeclaration --> <INTERFACE, Interfacemodifiers, interface_token,
// identifier_token, ExtendsInterfaces, {_token, InterfaceMemberDeclarations,
// }_token>
//
// InterfaceModifier --> Modifier (PUBLIC, ABSTRACT)
//
// ExtendsInterfaces --> Names
//
//
// InterfaceMemberDeclaration --> ConstantDeclaration
//                              | AbstractMethodDeclaration
//
// ConstantDeclaration --> FieldDeclaration (where the FieldModifierList is a
//         Constantmodifiers)
//
// ConstantModifier --> Modifier (PUBLIC, STATIC or FINAL)
//
// AbstractMethodDeclaration --> MethodDeclaration (where MethodModifierList
//         is a SignatureModifierList and the MethodBody is an EmptyStatement)
//
// SignatureModifier --> Modifier (PUBLIC or ABSTRACT)
//
class AstInterfaceDeclaration : public AstDeclaredType
{
    StoragePool* pool;
    AstArray<AstTypeName*>* interfaces;

public:
    LexStream::TokenIndex interface_token;

    AstInterfaceDeclaration(StoragePool* p)
        : pool(p),
          interfaces(NULL)
    {
        kind = INTERFACE;
        class_tag = NO_TAG;
        generated = false;
        modifiers_opt = NULL;
    }
    ~AstInterfaceDeclaration() {}

    inline AstTypeName*& Interface(unsigned i)
    {
        return (*interfaces)[i];
    }
    inline unsigned NumInterfaces()
    {
        return interfaces ? interfaces -> Length() : 0;
    }
    inline void AllocateInterfaces(unsigned estimate = 1);
    inline void AddInterface(AstTypeName*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : interface_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return class_body -> right_brace_token;
    }
};


//
// Represents a local variable declaration statement.
//
class AstLocalVariableDeclarationStatement : public AstStatement
{
    StoragePool* pool;
    AstArray<AstVariableDeclarator*>* variable_declarators;

public:
    AstModifiers* modifiers_opt;
    AstType* type;
    LexStream::TokenIndex semicolon_token_opt;

    AstLocalVariableDeclarationStatement(StoragePool* p)
        : pool(p),
          variable_declarators(NULL),
          modifiers_opt(NULL),
          semicolon_token_opt(LexStream::BadToken())
    {
        kind = LOCAL_VARIABLE_DECLARATION;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstLocalVariableDeclarationStatement() {}

    inline AstVariableDeclarator*& VariableDeclarator(unsigned i)
    {
        return (*variable_declarators)[i];
    }
    inline unsigned NumVariableDeclarators()
    {
        return variable_declarators ? variable_declarators -> Length() : 0;
    }
    inline void AllocateVariableDeclarators(unsigned estimate = 1);
    inline void AddVariableDeclarator(AstVariableDeclarator*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return modifiers_opt ? modifiers_opt -> left_modifier_token
            : type -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return semicolon_token_opt ? semicolon_token_opt
            : (VariableDeclarator(NumVariableDeclarators() - 1) ->
               RightToken());
    }
};


//
// Represents a local class declaration statement.
//
class AstLocalClassDeclarationStatement : public AstStatement
{
public:
    AstClassDeclaration* declaration;

    AstLocalClassDeclarationStatement(AstClassDeclaration* decl)
        : declaration(decl)
    {
        kind = LOCAL_CLASS;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = true;
    }
    ~AstLocalClassDeclarationStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return declaration -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return declaration -> class_body -> right_brace_token;
    }
};


//
// Statement --> IfStatement
//             | WhileStatement
//             | ForStatement
//             | Block
//             | EmptyStatement
//             | ExpressionStatement
//             | SwitchStatement
//             | DoStatement
//             | BreakStatement
//             | ContinueStatement
//             | ReturnStatement
//             | SynchronizedStatement
//             | ThrowStatement
//             | TryStatement
//
// Label --> identifier_token
//
// IfStatement --> <IF, if_token, Expression, Statement, Statement_opt>
// The parser always makes blocks for the enclosed statements, so we denote
// that here (even though any statement is legal).
//
class AstIfStatement : public AstStatement
{
public:
    LexStream::TokenIndex if_token;
    AstExpression* expression;
    AstBlock* true_statement;
    AstBlock* false_statement_opt;

    AstIfStatement()
        : false_statement_opt(NULL)
    {
        kind = IF;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstIfStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return if_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return false_statement_opt ? false_statement_opt -> RightToken()
            : true_statement -> RightToken();
    }
};


//
// EmptyStatement --> <EMPTY_STATEMENT, Label_opt, ;_token>
//
class AstEmptyStatement : public AstStatement
{
public:
    LexStream::TokenIndex semicolon_token;

    AstEmptyStatement(LexStream::TokenIndex token)
        : semicolon_token(token)
    {
        kind = EMPTY_STATEMENT;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstEmptyStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return semicolon_token; }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ExpressionStatement --> <EXPRESSION_STATEMENT, Label_opt, Expression,
// ;_token_opt>
//
class AstExpressionStatement : public AstStatement
{
public:
    AstExpression* expression;
    LexStream::TokenIndex semicolon_token_opt;

    AstExpressionStatement()
        : semicolon_token_opt(LexStream::BadToken())
    {
        kind = EXPRESSION_STATEMENT;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstExpressionStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return expression -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return semicolon_token_opt ? semicolon_token_opt
            : expression -> RightToken();
    }
};


//
// Represents "case <constant> :" and "default :".
//
class AstSwitchLabel : public Ast
{
public:
    LexStream::TokenIndex case_token;
    AstExpression* expression_opt;
    LexStream::TokenIndex colon_token;

    //
    // The sorted index of this label in the overall switch. Default cases
    // are set to NumCases().
    //
    unsigned map_index;

    AstSwitchLabel()
        : expression_opt(NULL)
    {
        kind = SWITCH_LABEL;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstSwitchLabel() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return case_token; }
    virtual LexStream::TokenIndex RightToken() { return colon_token; }
};


//
// SwitchBlockStatement --> <SWITCH_BLOCK, SwitchLabels, BlockStatements>
//
class AstSwitchBlockStatement : public AstBlock
{
    AstArray<AstSwitchLabel*>* switch_labels;

public:
    AstSwitchBlockStatement(StoragePool* p)
        : AstBlock(p),
          switch_labels(NULL)
    {
        kind = SWITCH_BLOCK;
        no_braces = true;
    }
    ~AstSwitchBlockStatement() {}

    inline AstSwitchLabel*& SwitchLabel(unsigned i)
    {
        return (*switch_labels)[i];
    }
    inline unsigned NumSwitchLabels()
    {
        return switch_labels ? switch_labels -> Length() : 0;
    }
    inline void AllocateSwitchLabels(unsigned estimate = 1);
    inline void AddSwitchLabel(AstSwitchLabel*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);
    virtual LexStream::TokenIndex LeftToken()
    {
        return SwitchLabel(0) -> case_token;
    }
    // Inherited RightToken() is adequate.
};


//
// This structure allows a switch statement to sort its case labels. It should
// be a plain-old-data type (POD) for efficient copying.
//
struct CaseElement
{
    unsigned block_index; // which SwitchBlockStatement
    unsigned case_index; // which case label within the block
    i4 value; // the value of the case's expression

    //
    // This keeps the sort stable, so that duplicates stay later in the list.
    //
    inline bool operator<(CaseElement& right)
    {
        return value < right.value ||
            (value == right.value &&
             (block_index < right.block_index ||
              (block_index == right.block_index &&
               case_index < right.case_index)));
    }
};


//
// SwitchStatement --> <SWITCH, Label_opt, switch_token, Expression, {_token,
// SwitchBlockStatements, SwitchLabels_opt, }_token>
//
class AstSwitchStatement : public AstStatement
{
    StoragePool* pool;
    //
    // The sorted list of case label values. Index 0 is reserved for the
    // default case.
    //
    AstArray<CaseElement*>* cases;

public:
    LexStream::TokenIndex switch_token;
    AstExpression* expression;
    AstBlock* switch_block;

    AstSwitchStatement(StoragePool* p)
        : pool(p),
          cases(NULL)
    {
        kind = SWITCH;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstSwitchStatement() {}

    inline CaseElement*& Case(unsigned i) { return (*cases)[i + 1]; }
    inline CaseElement*& DefaultCase() { return (*cases)[0]; }
    inline unsigned NumCases() { return cases -> Length() - 1; }
    inline void AllocateCases(unsigned estimate = 1);
    inline void AddCase(CaseElement*);

    inline AstSwitchBlockStatement* Block(unsigned i)
    {
        return (AstSwitchBlockStatement*) switch_block -> Statement(i);
    }
    inline unsigned NumBlocks() { return switch_block -> NumStatements(); }

    void SortCases();
    CaseElement* CaseForValue(i4 value);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return switch_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return switch_block -> right_brace_token;
    }
};


//
// WhileStatement --> <WHILE, Label_opt, while_token, Expression, Statement>
//
class AstWhileStatement : public AstStatement
{
public:
    LexStream::TokenIndex while_token;
    AstExpression* expression;
    AstBlock* statement;

    AstWhileStatement()
    {
        kind = WHILE;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstWhileStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return while_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return statement -> RightToken();
    }
};


//
// DoStatement --> <DO, Label_opt, do_token, Expression, Statement, ;_token>
//
class AstDoStatement : public AstStatement
{
public:
    LexStream::TokenIndex do_token;
    AstBlock* statement;
    LexStream::TokenIndex while_token;
    AstExpression* expression;
    LexStream::TokenIndex semicolon_token;

    AstDoStatement()
    {
        kind = DO;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstDoStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return do_token;
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ForStatement --> <FOR, Label_opt, for_token, ForInits, Expression_opt,
// ForUpdates, Statement>
//
// ForInit --> ExpressionStatement
//           | LocalVariableDeclarationStatement
//
// ForUpdate --> ExpressionStatement
//
class AstForStatement : public AstStatement
{
    StoragePool* pool;
    AstArray<AstStatement*>* for_init_statements;
    AstArray<AstExpressionStatement*>* for_update_statements;

public:
    LexStream::TokenIndex for_token;
    AstExpression* end_expression_opt;
    AstBlock* statement;

    AstForStatement(StoragePool* p)
        : pool(p),
          for_init_statements(NULL),
          for_update_statements(NULL),
          end_expression_opt(NULL)
    {
        kind = FOR;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstForStatement() {}

    inline AstStatement*& ForInitStatement(unsigned i)
    {
        return (*for_init_statements)[i];
    }
    inline unsigned NumForInitStatements()
    {
        return for_init_statements ? for_init_statements -> Length() : 0;
    }
    inline void AllocateForInitStatements(unsigned estimate = 1);
    inline void AddForInitStatement(AstStatement*);

    inline AstExpressionStatement*& ForUpdateStatement(unsigned i)
    {
        return (*for_update_statements)[i];
    }
    inline unsigned NumForUpdateStatements()
    {
        return for_update_statements ? for_update_statements -> Length() : 0;
    }
    inline void AllocateForUpdateStatements(unsigned estimate = 1);
    inline void AddForUpdateStatement(AstExpressionStatement*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return for_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return statement -> RightToken();
    }
};


//
// BreakStatement --> <BREAK, Label_opt, break_token, identifier_token_opt,
// ;_token>
//
class AstBreakStatement : public AstStatement
{
public:
    LexStream::TokenIndex break_token;
    LexStream::TokenIndex identifier_token_opt;
    LexStream::TokenIndex semicolon_token;
    unsigned nesting_level;

    AstBreakStatement()
        : identifier_token_opt(LexStream::BadToken()),
          nesting_level(0)
    {
        kind = BREAK;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstBreakStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return break_token;
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ContinueStatement --> <CONTINUE, Label_opt, continue_token, SimpleName_opt,
// ;_token>
//
class AstContinueStatement : public AstStatement
{
public:
    LexStream::TokenIndex continue_token;
    LexStream::TokenIndex identifier_token_opt;
    LexStream::TokenIndex semicolon_token;
    unsigned nesting_level;

    AstContinueStatement()
        : identifier_token_opt(LexStream::BadToken()),
          nesting_level(0)
    {
        kind = CONTINUE;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstContinueStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return continue_token;
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ReturnStatement --> <RETURN, Label_opt, return_token, Expression_opt,
// ;_token>
//
class AstReturnStatement : public AstStatement
{
public:
    LexStream::TokenIndex return_token;
    AstExpression* expression_opt;
    LexStream::TokenIndex semicolon_token;

    AstReturnStatement()
        : expression_opt(NULL)
    {
        kind = RETURN;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstReturnStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return return_token;
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// ThrowStatement --> <THROW, Label_opt, throw_token, Expression, ;_token>
//
class AstThrowStatement : public AstStatement
{
public:
    LexStream::TokenIndex throw_token;
    AstExpression* expression;
    LexStream::TokenIndex semicolon_token;

    AstThrowStatement()
    {
        kind = THROW;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstThrowStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return throw_token;
    }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// SynchronizedStatement --> <SYNCHRONIZED_STATEMENT, Label_opt,
// synchronized_token, Expression, Block>
//
class AstSynchronizedStatement : public AstStatement
{
public:
    LexStream::TokenIndex synchronized_token;
    AstExpression* expression;
    AstBlock* block;

    AstSynchronizedStatement()
    {
        kind = SYNCHRONIZED_STATEMENT;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstSynchronizedStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return synchronized_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return block -> right_brace_token;
    }
};


//
// AssertStatement --> <ASSERT, Label_opt, assert_token, Expression, ;_token>
//                 --> <ASSERT, Label_opt, assert_token, Expression, :_token,
// Expression, ;_token>
//
class AstAssertStatement : public AstStatement
{
public:
    LexStream::TokenIndex assert_token;
    LexStream::TokenIndex semicolon_token;
    AstExpression* condition;
    AstExpression* message_opt;

    VariableSymbol* assert_variable;

    AstAssertStatement()
        : message_opt(NULL),
          assert_variable(NULL)
    {
        kind = ASSERT;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstAssertStatement() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return assert_token; }
    virtual LexStream::TokenIndex RightToken() { return semicolon_token; }
};


//
// CatchClause --> <CATCH, catch_token, FormalParameter, Block>
//
class AstCatchClause : public Ast
{
public:
    VariableSymbol* parameter_symbol;

    LexStream::TokenIndex catch_token;
    AstFormalParameter* formal_parameter;
    AstBlock* block;

    AstCatchClause() : parameter_symbol(NULL)
    {
        kind = CATCH;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstCatchClause() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return catch_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return block -> right_brace_token;
    }
};


//
// FinallyClause --> <FINALLY, finally_token, Block>
//
class AstFinallyClause : public Ast
{
public:
    LexStream::TokenIndex finally_token;
    AstBlock* block;

    AstFinallyClause()
    {
        kind = FINALLY;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstFinallyClause() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return finally_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return block -> right_brace_token;
    }
};


//
// TryStatement --> <TRY, Label_opt, try-token, Block CatchClauses,
// FinallyClause_opt>
//
class AstTryStatement : public AstStatement
{
    StoragePool* pool;
    AstArray<AstCatchClause*>* catch_clauses;

public:
    LexStream::TokenIndex try_token;
    AstBlock* block;
    AstFinallyClause* finally_clause_opt;
    bool processing_try_block;

    AstTryStatement(StoragePool* p)
        : pool(p),
          catch_clauses(NULL),
          finally_clause_opt(NULL),
          processing_try_block(false)
    {
        kind = TRY;
        class_tag = STATEMENT;
        generated = false;
        is_reachable = false;
        can_complete_normally = false;
    }
    ~AstTryStatement() {}

    inline AstCatchClause*& CatchClause(unsigned i)
    {
        return (*catch_clauses)[i];
    }
    inline unsigned NumCatchClauses()
    {
        return catch_clauses ? catch_clauses -> Length() : 0;
    }
    inline void AllocateCatchClauses(unsigned estimate = 1);
    inline void AddCatchClause(AstCatchClause*);

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return try_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        //
        // when the Finally clause is null, there must be one or more catch
        // clauses
        //
        return finally_clause_opt ? finally_clause_opt -> RightToken()
            : CatchClause(NumCatchClauses() - 1) -> RightToken();
    }
};


//
// Expression --> Primary
//              | UnaryExpression
//              | BinaryExpression
//              | ConditionalExpression
//              | AssignmentExpression
//
// Primary --> Literal
//           | NullLiteral
//           | ThisExpression
//           | SuperExpression
//           | ParenthesizedExpression
//           | ClassInstanceCreationExpression
//           | ArrayCreationExpression
//           | FieldAccess
//           | MethodInvocation
//           | ArrayAccess
//
// Literal --> IntegerLiteral
//           | LongLiteral
//           | FloatLiteral
//           | DoubleLiteral
//           | BooleanLiteral
//           | StringLiteral
//           | CharacterLiteral
//
// BooleanLiteral --> TrueLiteral
//                  | FalseLiteral
//

//
// IntegerLiteral --> <INTEGER_LITERAL, integer_literal_token, value>
//
class AstIntegerLiteral : public AstExpression
{
public:
    LexStream::TokenIndex integer_literal_token;

    AstIntegerLiteral(LexStream::TokenIndex token)
        : integer_literal_token(token)
    {
        kind = INTEGER_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstIntegerLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return integer_literal_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return integer_literal_token;
    }
};


//
// LongLiteral --> <LONG_LITERAL, long_literal_token, value>
//
class AstLongLiteral : public AstExpression
{
public:
    LexStream::TokenIndex long_literal_token;

    AstLongLiteral(LexStream::TokenIndex token)
        : long_literal_token(token)
    {
        kind = LONG_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstLongLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return long_literal_token; }
    virtual LexStream::TokenIndex RightToken() { return long_literal_token; }
};


//
// FloatLiteral --> <FLOAT_LITERAL, Literal, value>
//
class AstFloatLiteral : public AstExpression
{
public:
    LexStream::TokenIndex float_literal_token;

    AstFloatLiteral(LexStream::TokenIndex token)
        : float_literal_token(token)
    {
        kind = FLOAT_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstFloatLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return float_literal_token; }
    virtual LexStream::TokenIndex RightToken() { return float_literal_token; }
};


//
// DoubleLiteral --> <DOUBLE_LITERAL, Literal, value>
//
class AstDoubleLiteral : public AstExpression
{
public:
    LexStream::TokenIndex double_literal_token;

    AstDoubleLiteral(LexStream::TokenIndex token)
        : double_literal_token(token)
    {
        kind = DOUBLE_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstDoubleLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return double_literal_token; }
    virtual LexStream::TokenIndex RightToken() { return double_literal_token; }
};


//
// TrueLiteral --> <TRUE_LITERAL, Literal, value>
//
class AstTrueLiteral : public AstExpression
{
public:
    LexStream::TokenIndex true_literal_token;

    AstTrueLiteral(LexStream::TokenIndex token)
        : true_literal_token(token)
    {
        kind = TRUE_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstTrueLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return true_literal_token; }
    virtual LexStream::TokenIndex RightToken() { return true_literal_token; }
};


//
// FalseLiteral --> <FALSE_LITERAL, Literal, value>
//
class AstFalseLiteral : public AstExpression
{
public:
    LexStream::TokenIndex false_literal_token;

    AstFalseLiteral(LexStream::TokenIndex token)
        : false_literal_token(token)
    {
        kind = FALSE_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstFalseLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return false_literal_token; }
    virtual LexStream::TokenIndex RightToken() { return false_literal_token; }
};


//
// StringLiteral --> <STRING_LITERAL, Literal, value>
//
class AstStringLiteral : public AstExpression
{
public:
    LexStream::TokenIndex string_literal_token;

    AstStringLiteral(LexStream::TokenIndex token)
        : string_literal_token(token)
    {
        kind = STRING_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstStringLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return string_literal_token; }
    virtual LexStream::TokenIndex RightToken() { return string_literal_token; }
};


//
// CharacterLiteral --> <CHARACTER_LITERAL, literal_token, value>
//
class AstCharacterLiteral : public AstExpression
{
public:
    LexStream::TokenIndex character_literal_token;

    AstCharacterLiteral(LexStream::TokenIndex token)
        : character_literal_token(token)
    {
        kind = CHARACTER_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstCharacterLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return character_literal_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return character_literal_token;
    }
};


//
// NullLiteral --> <NULL_EXPRESSION, null_token>
//
class AstNullLiteral : public AstExpression
{
public:
    LexStream::TokenIndex null_token;

    AstNullLiteral(LexStream::TokenIndex token)
        : null_token(token)
    {
        kind = NULL_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstNullLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return null_token; }
    virtual LexStream::TokenIndex RightToken() { return null_token; }
};


//
// Represents class literals.
//
class AstClassLiteral : public AstExpression
{
public:
    AstType* type;
    LexStream::TokenIndex class_token;

    //
    // If this expression requires a caching variable and a call to class$(),
    // the resolution holds the needed class$xxx or array$xxx cache.
    //
    AstExpression* resolution_opt;

    AstClassLiteral(LexStream::TokenIndex token)
        : class_token(token),
          resolution_opt(NULL)
    {
        kind = CLASS_LITERAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstClassLiteral() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return type -> LeftToken(); }
    virtual LexStream::TokenIndex RightToken() { return class_token; }
};


//
// Represents qualified and simple 'this'.
//
class AstThisExpression : public AstExpression
{
public:
    AstTypeName* base_opt;
    LexStream::TokenIndex this_token;

    //
    // If this expression accesses an enclosing instance, the resolution
    // holds the needed chain of "this$0" traversals.
    //
    AstExpression* resolution_opt;

    AstThisExpression(LexStream::TokenIndex token)
        : base_opt(NULL),
          this_token(token),
          resolution_opt(NULL)
    {
        kind = THIS_EXPRESSION;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstThisExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return base_opt ? base_opt -> LeftToken() : this_token;
    }
    virtual LexStream::TokenIndex RightToken() { return this_token; }
};


//
// Represents qualified and simple 'super'.
//
class AstSuperExpression : public AstExpression
{
public:
    AstTypeName* base_opt;
    LexStream::TokenIndex super_token;

    //
    // If this expression accesses an enclosing instance, the resolution
    // holds the needed chain of "this$0" traversals.
    //
    AstExpression* resolution_opt;

    AstSuperExpression(LexStream::TokenIndex token)
        : base_opt(NULL),
          super_token(token),
          resolution_opt(NULL)
    {
        kind = SUPER_EXPRESSION;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstSuperExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return base_opt ? base_opt -> LeftToken() : super_token;
    }
    virtual LexStream::TokenIndex RightToken() { return super_token; }
};


//
// ParenthesizedExpression --> <PARENTHESIZED_EXPRESSION, (_token, Expression,
// )_token>
//
class AstParenthesizedExpression : public AstExpression
{
public:
    LexStream::TokenIndex left_parenthesis_token;
    AstExpression* expression;
    LexStream::TokenIndex right_parenthesis_token;

    AstParenthesizedExpression()
    {
        kind = PARENTHESIZED_EXPRESSION;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstParenthesizedExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return left_parenthesis_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return right_parenthesis_token;
    }
};


//
// ClassInstanceCreationExpression --> <CLASS_CREATION, new_token, TypeName,
// (_token, Arguments, )_token>
//
// Sometimes, during semantic analysis an artificial base_opt expression is
// constructed. In such a case, the user can determine this condition by
// testing base_opt -> generated.
//
class AstClassInstanceCreationExpression : public AstExpression
{
public:
    AstExpression* base_opt;
    LexStream::TokenIndex new_token;
    AstTypeName* class_type;
    AstArguments* arguments;
    AstClassBody* class_body_opt;

    //
    // For anonymous classes, we resolve the original statement into a new
    // one that does not have a class_body_opt. This is necessary to get
    // the parameters called in the correct order.
    //
    AstClassInstanceCreationExpression* resolution_opt;

    AstClassInstanceCreationExpression()
        : base_opt(NULL),
          class_body_opt(NULL),
          resolution_opt(NULL)
    {
        kind = CLASS_CREATION;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstClassInstanceCreationExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return base_opt ? base_opt -> LeftToken() : new_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return class_body_opt ? class_body_opt -> right_brace_token
            : arguments -> right_parenthesis_token;
    }
};


//
// DimExpr --> <DIM, [_token, Expression, ]_token>
//
class AstDimExpr : public Ast
{
public:
    LexStream::TokenIndex left_bracket_token;
    AstExpression* expression;
    LexStream::TokenIndex right_bracket_token;

    AstDimExpr()
    {
        kind = DIM;
        class_tag = NO_TAG;
        generated = false;
    }
    ~AstDimExpr() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return left_bracket_token; }
    virtual LexStream::TokenIndex RightToken() { return right_bracket_token; }
};


//
// ArrayCreationExpression --> <ARRAY_CREATION, new_token, Type, DimExprs,
// Brackets>
//
class AstArrayCreationExpression : public AstExpression
{
    StoragePool* pool;
    AstArray<AstDimExpr*>* dim_exprs;

public:
    LexStream::TokenIndex new_token;
    AstType* array_type;
    AstBrackets* brackets_opt;
    AstArrayInitializer* array_initializer_opt;

    AstArrayCreationExpression(StoragePool* p)
        : pool(p),
          dim_exprs(NULL),
          brackets_opt(NULL),
          array_initializer_opt(NULL)
    {
        kind = ARRAY_CREATION;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstArrayCreationExpression() {}

    inline AstDimExpr*& DimExpr(unsigned i) { return (*dim_exprs)[i]; }
    inline unsigned NumDimExprs()
    {
        return dim_exprs ? dim_exprs -> Length() : 0;
    }
    inline void AllocateDimExprs(unsigned estimate = 1);
    inline void AddDimExpr(AstDimExpr*);

    inline unsigned NumBrackets()
    {
        return brackets_opt ? brackets_opt -> dims : 0;
    }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return new_token; }
    virtual LexStream::TokenIndex RightToken()
    {
        return array_initializer_opt
            ? array_initializer_opt -> right_brace_token
            : brackets_opt ? brackets_opt -> right_bracket_token
            : DimExpr(NumDimExprs() - 1) -> right_bracket_token;
    }
};


//
// FieldAccess --> <DOT, Primary, ._token, Identifier>
//
class AstFieldAccess : public AstExpression
{
public:
    AstExpression* base; // Not AstName.
    LexStream::TokenIndex identifier_token;

    //
    // If the base expression of FieldAccess expression is of the form
    // type.this.X, where X is a private variable that is a member of an
    // outer class, then we resolve it into a method call to the read_mehod
    // that gives access to X.
    //
    AstExpression* resolution_opt;

    AstFieldAccess()
        : resolution_opt(NULL)
    {
        kind = DOT;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstFieldAccess() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return base -> LeftToken(); }
    virtual LexStream::TokenIndex RightToken() { return identifier_token; }
};


//
// MethodInvocation --> <CALL, Method, (_token, Arguments, )_token>
//
// Method --> SimpleName
//          | FieldAccess
//
class AstMethodInvocation : public AstExpression
{
public:
    AstExpression* method; // AstName, AstFieldAccess
    AstArguments* arguments;

    //
    // When a method refers to a member in an enclosing scope,
    // it is mapped into a new expression that creates a path to
    // the member in question.
    //
    AstExpression* resolution_opt;

    AstMethodInvocation()
        : resolution_opt(NULL)
    {
        kind = CALL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstMethodInvocation() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return method -> LeftToken(); }
    virtual LexStream::TokenIndex RightToken()
    {
        return arguments -> right_parenthesis_token;
    }
};


//
// ArrayAccess --> <ARRAY_ACCESS, Base, [_token, Expression, ]_token>
//
class AstArrayAccess : public AstExpression
{
public:
    AstExpression* base;
    LexStream::TokenIndex left_bracket_token;
    AstExpression* expression;
    LexStream::TokenIndex right_bracket_token;

    AstArrayAccess()
    {
        kind = ARRAY_ACCESS;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstArrayAccess() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken() { return base -> LeftToken(); }
    virtual LexStream::TokenIndex RightToken() { return right_bracket_token; }
};


//
// UnaryExpression --> PreUnaryExpression
//                   | PostUnaryExpression
//                   | CastExpression
//
// PostUnaryExpression --> <POST_UNARY, PostUnaryTag, Expression, PostOperator>
//
// PostUnaryTag --> PLUSPLUS | MINUSMINUS
//
// PostOperator --> ++_token | --_token
//
class AstPostUnaryExpression : public AstExpression
{
public:
    enum PostUnaryExpressionTag
    {
        NONE,
        PLUSPLUS,
        MINUSMINUS,

        _num_kinds
    };

    PostUnaryExpressionTag post_unary_tag;
    AstExpression* expression;
    LexStream::TokenIndex post_operator_token;

    //
    // When the left-hand side of an assignment is a name that refers
    // to a private field in an enclosing scope, the access method
    // that gives write-permission to that field is recorded here.
    //
    MethodSymbol* write_method;

    AstPostUnaryExpression(PostUnaryExpressionTag tag)
        : post_unary_tag(tag),
          write_method(NULL)
    {
        kind = POST_UNARY;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstPostUnaryExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return expression -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return post_operator_token;
    }
};


//
// PreUnaryExpression -->  <PRE_UNARY, PreUnaryTag, PreOperator, Expression>
//
// PreUnaryTag --> PLUS | MINUS | TWIDDLE | NOT | PLUSPLUS | MINUSMINUS
//
// PreOperator --> +_token | -_token | ~_token | !_token | ++_token | --_token
//
class AstPreUnaryExpression : public AstExpression
{
public:
    enum PreUnaryExpressionTag
    {
        NONE,
        PLUSPLUS,
        MINUSMINUS,
        PLUS,
        MINUS,
        TWIDDLE,
        NOT,

        _num_kinds
    };

    PreUnaryExpressionTag pre_unary_tag;
    LexStream::TokenIndex pre_operator_token;
    AstExpression* expression;

    //
    // When the left-hand side of an assignment is a name that refers
    // to a private field in an enclosing scope, the access method
    // that gives write-permission to that field is recorded here.
    //
    MethodSymbol* write_method;

    AstPreUnaryExpression(PreUnaryExpressionTag tag)
        : pre_unary_tag(tag),
          write_method(NULL)
    {
        kind = PRE_UNARY;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstPreUnaryExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return pre_operator_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return expression -> RightToken();
    }
};


//
// CastExpression --> <castkind, (_token_opt, Type, )_token_opt, Expression>
//
// NOTE that the optional symbols above are absent only when the compiler
// inserts a CAST conversion node into the program.
//
class AstCastExpression : public AstExpression
{
public:
    LexStream::TokenIndex left_parenthesis_token;
    AstType* type;
    LexStream::TokenIndex right_parenthesis_token;
    AstExpression* expression;

    AstCastExpression()
        : type(NULL)
    {
        kind = CAST;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstCastExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return left_parenthesis_token;
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return expression -> RightToken();
    }
};


//
// BinaryExpression --> <BINARY, BinaryTag, Expression, BinaryOperator,
//                      Expression>
//
// BinaryTag --> STAR | SLASH | MOD | PLUS | MINUS | LEFT_SHIFT | RIGHT_SHIFT |
//               UNSIGNED_RIGHT_SHIFT | LESS | GREATER |
//               LESS_EQUAL | GREATER_EQUAL | EQUAL_EQUAL | NOT_EQUAL |
//               AND | XOR | IOR | AND_AND | OR_OR
//
// BinaryOperator --> *_token | /_token | %_token | +_token | -_token |
//                    <<_token | >>_token | >>>_token | <_token | >_token |
//                    <=_token | >=_token | ==_token | !=_token | &_token |
//                    ^_token | |_token | &&_token | ||_token
//
class AstBinaryExpression : public AstExpression
{
public:
    enum BinaryExpressionTag
    {
        NONE,
        STAR,
        SLASH,
        MOD,
        PLUS,
        MINUS,
        LEFT_SHIFT,
        RIGHT_SHIFT,
        UNSIGNED_RIGHT_SHIFT,
        LESS,
        GREATER,
        AND,
        XOR,
        IOR,
        AND_AND,
        OR_OR,

        LESS_EQUAL,
        GREATER_EQUAL,
        EQUAL_EQUAL,
        NOT_EQUAL,

        _num_kinds
    };

    BinaryExpressionTag binary_tag;
    AstExpression* left_expression;
    LexStream::TokenIndex binary_operator_token;
    AstExpression* right_expression;

    AstBinaryExpression(BinaryExpressionTag tag)
        : binary_tag(tag)
    {
        kind = BINARY;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstBinaryExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return left_expression -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return right_expression -> RightToken();
    }
};


//
// Represents instanceof expressions.
//
class AstInstanceofExpression : public AstExpression
{
public:
    AstExpression* expression;
    LexStream::TokenIndex instanceof_token;
    AstType* type; // AstArrayType, AstTypeName

    AstInstanceofExpression()
    {
        kind = INSTANCEOF;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstInstanceofExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return expression -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return type -> RightToken();
    }
};


//
// ConditionalExpression --> <CONDITIONAL, Expression, ?_token, Expression,
//                            :_token, Expression>
//
class AstConditionalExpression : public AstExpression
{
public:
    AstExpression* test_expression;
    LexStream::TokenIndex question_token;
    AstExpression* true_expression;
    LexStream::TokenIndex colon_token;
    AstExpression* false_expression;

    AstConditionalExpression()
    {
        kind = CONDITIONAL;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstConditionalExpression() {}

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return test_expression -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return false_expression -> RightToken();
    }
};


//
// Assignment --> <ASSIGNMENT, AssignmentTag, LeftHandSide, AssignmentOperator,
//                Expression>
//
// AssignmentTag --> EQUAL | STAR_EQUAL | SLASH_EQUAL | MOD_EQUAL |
//                   PLUS_EQUAL | MINUS_EQUAL | LEFT_SHIFT_EQUAL |
//                   RIGHT_SHIFT_EQUAL | UNSIGNED_RIGHT_SHIFT_EQUAL |
//                   AND_EQUAL | XOR_EQUAL | IOR_EQUAL
//
// LeftHandSide --> Name | FieldAccess | ArrayAccess | ParenthesizedExpression
//                  | CastExpression
//
// NOTE: that a LeftHandSide appears as a cast node only when the
// assignment_operator in question is of the form "op=" and the application
// of the operator requires a casting of the value of the left-hand side.
//
// AssignmentOperator --> =_token | *=_token | /=_token | %=_token | +=_token |
//                        -=_token | <<=_token | >>=_token | >>>=_token |
//                        &=_token | ^=_token | |=_token
//
class AstAssignmentExpression : public AstExpression
{
public:
    enum AssignmentExpressionTag
    {
        NONE,
        SIMPLE_EQUAL,
        STAR_EQUAL,
        SLASH_EQUAL,
        MOD_EQUAL,
        PLUS_EQUAL,
        MINUS_EQUAL,
        LEFT_SHIFT_EQUAL,
        RIGHT_SHIFT_EQUAL,
        UNSIGNED_RIGHT_SHIFT_EQUAL,

        AND_EQUAL,
        XOR_EQUAL,
        IOR_EQUAL,

        _num_kinds
    };

    //
    // When the left-hand side of an assignment is a name that refers
    // to a private field in an enclosing scope, the access method
    // that gives write-permission to that field is recorded here.
    //
    MethodSymbol* write_method;

    AssignmentExpressionTag assignment_tag;
    AstExpression* left_hand_side;
    LexStream::TokenIndex assignment_operator_token;
    AstExpression* expression;

    AstAssignmentExpression(AssignmentExpressionTag tag,
                            LexStream::TokenIndex token)
        : write_method(NULL),
          assignment_tag(tag),
          assignment_operator_token(token)
    {
        kind = ASSIGNMENT;
        class_tag = EXPRESSION;
        generated = false;
        value = NULL;
        symbol = NULL;
    }
    ~AstAssignmentExpression() {}

    inline bool SimpleAssignment() { return assignment_tag == SIMPLE_EQUAL; }

#ifdef JIKES_DEBUG
    virtual void Print(LexStream&);
    virtual void Unparse(Ostream&, LexStream*);
#endif

    virtual Ast* Clone(StoragePool*);

    virtual LexStream::TokenIndex LeftToken()
    {
        return left_hand_side -> LeftToken();
    }
    virtual LexStream::TokenIndex RightToken()
    {
        return expression -> RightToken();
    }
};


//
// Given an Ast tree, check whether it is a variable (not a value).
//
inline bool Ast::IsLeftHandSide()
{
    return kind == NAME || kind == DOT || kind == ARRAY_ACCESS;
}


//
// Given an Ast tree, check whether it is an explicit constructor invocation.
//
inline bool Ast::IsExplicitConstructorInvocation()
{
    return kind == THIS_CALL || kind == SUPER_CALL;
}


//
// Given an Ast tree, check whether or not it is generated.
//
inline bool Ast::IsGenerated()
{
    return generated;
}


//
// This Storage pool is modeled after the Dynamic arrays. The difference is
// that instead of a Next() function we have an alloc(size_t) function. The
// value of the size_t argument represents the size of the object to allocate.
//
// Since all AST nodes for a given parse are allocated from the same storage
// pool, you should never need to directly allocate an AST object (with
// operator new), and you should never need to destroy an AST object (with
// operator delete). To reclaim memory when processing is complete, simply
// delete the underlying storage pool.
//
class StoragePool
{
public:
    typedef void* Cell;

    inline size_t Blksize() { return 1 << log_blksize; }

private:
    Cell** base;
    size_t base_size;
    int top;
    int size;

    size_t log_blksize;
    size_t base_increment;

    //
    // Allocate another block of storage for the storage pool
    //
    void AllocateMoreSpace()
    {
        //
        // The variable size always indicates the maximum number of
        // cells that has been allocated for the storage pool.
        // Initially, it is set to 0 to indicate that the pool is empty.
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
        // If the base is overflowed, reallocate it and initialize the new
        // elements to NULL.
        //
        if (k == base_size)
        {
            int old_base_size = base_size;
            Cell** old_base = base;

            base_size += base_increment;
            base = new Cell*[base_size];

            if (old_base != NULL)
            {
                memcpy(base, old_base, old_base_size * sizeof(Cell*));
                delete [] old_base;
            }
            memset(&base[old_base_size], 0,
                   (base_size - old_base_size) * sizeof(Cell*));
        }

        //
        // If the slot "k" does not already contain a segment,
        // we allocate a new segment and place its adjusted address in
        // base[k]. The adjustment allows us to index the segment directly,
        // instead of having to perform a subtraction for each reference.
        // See operator[] below.
        //
        if (base[k] == NULL)
        {
            base[k] = new Cell[Blksize()];
            base[k] -= size;
        }

        //
        // Finally, we update SIZE.
        //
        size += Blksize();
    }

public:
    //
    // Constructor of a storage pool
    //
    StoragePool(size_t num_tokens)
    {
        //
        // Make a guess on the size that will be required for the ast
        // based on the number of tokens. The ratio for the bodies is
        // usually 40 to 1. We will double the base to add to account
        // for the headers
        // recall that each cell is a-byte word. So, 10 * 4 = 40
        //
        size_t estimate = num_tokens * 10;

        //
        // Find a block of size 2**log_blksize that is large enough
        // to satisfy our estimate.
        //
        for (log_blksize = 8;
             (((unsigned) 1 << log_blksize) < estimate) && (log_blksize < 31);
             log_blksize++)
            ;

        //
        // If the size of the block found is < 1k, allocate a block of size 1k
        // with one slot to spare, just in case.
        // If the size is less than 16k, then break it up into equal blocks
        // of size 1k. Otherwise, fragment it into pieces of size 16k.
        //
        if (log_blksize < 8)
        {
            base_increment = 1;
            log_blksize = 8;
        }
        else if (log_blksize < 13)
        {
            base_increment = (unsigned) 1 << (log_blksize - 8);
            log_blksize = 8;
        }
        else if (log_blksize < 17)
        {
            base_increment = (unsigned) 1 << (log_blksize - 10);
            log_blksize = 10;
        }
        else
        {
            // assume we won't be allocating more than this many blocks.
            base_increment = (unsigned) 1 << (log_blksize - 12);
            log_blksize = 12;
        }

        //
        // Double the size of the base in order to allocate extra space for
        // the headers and add a little margin for stuff like extra Cast node
        // and computation of static expressions that require cloning.
        //
        base_increment = (base_increment << 1) + 3;

        base_size = 0;
        size = 0;
        top = 0;
        base = NULL;
    }

    //
    // Destructor of a storage pool. This frees the memory of all of the AST
    // nodes allocated in this pool.
    //
    ~StoragePool()
    {
        for (int k = (size >> log_blksize) - 1; k >= 0; k--)
        {
            size -= Blksize();
            base[k] += size;
            delete [] base[k];
        }

        delete [] base;
    }

    //
    // alloc allocates an object of size n in the pool and
    // returns a pointer to it.
    //
    inline void* Alloc(size_t n)
    {
        size_t i = top,
               chunk_size = ((n + sizeof(Cell) - 1) / sizeof(Cell));
        top += chunk_size;
        if (top > size)
        {
            assert(chunk_size <= Blksize() &&
                   "allocating a chunk of storage larger than the block !");

            i = size;
            top = size + chunk_size;
            AllocateMoreSpace();
        }

        return (void*) &(base[i >> log_blksize] [i]);
    }

    //
    // Return length of the amount of storage that has been allocated so far.
    //
    inline size_t Length() { return top; }

    //
    // This function is used to reset the Storage pool. This action
    // automatically invalidates all objects that had been allocated in the
    // pool. At least, YOU should assume it does!!!
    //
    inline void Reset(const int n = 0)
    {
        if (n < 0 || n > size)
            assert(false);
        top = n;
    }

    //
    // This function frees up all dynamic space that
    // was allocated for this storage pool.
    //
    inline void Destroy()
    {
        for (int k = (size >> log_blksize) - 1; k >= 0; k--)
        {
            size -= Blksize();
            base[k] += size;
            delete [] base[k];
            base[k] = NULL;
        }

        delete [] base;
        base = NULL;
        base_size = 0;

        Reset();
    }

    // ********************************************************************

    inline VariableSymbolArray* NewVariableSymbolArray(unsigned size = 0)
    {
        return new (Alloc(sizeof(VariableSymbolArray)))
            VariableSymbolArray(this, size);
    }

    //
    // Older compilers do not support templatized member methods, hence we
    // have moved this to be a global method (yuck).
    //
    // template <typename T>
    // inline AstArray<T>* NewAstArray(unsigned estimate = 0)
    // {
    //     return ! estimate ? NULL
    //         : new (Alloc(sizeof(AstArray<T>))) AstArray<T>(this, estimate);
    // }

    inline AstListNode* NewListNode()
    {
        return new (Alloc(sizeof(AstListNode))) AstListNode();
    }

    inline AstBlock* NewBlock()
    {
        return new (Alloc(sizeof(AstBlock))) AstBlock(this);
    }

    inline AstName* NewName(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstName))) AstName(token);
    }

    inline AstPrimitiveType* NewPrimitiveType(Ast::Kind kind,
                                              LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstPrimitiveType)))
            AstPrimitiveType(kind, token);
    }

    inline AstBrackets* NewBrackets(LexStream::TokenIndex left,
                                    LexStream::TokenIndex right)
    {
        return new (Alloc(sizeof(AstBrackets))) AstBrackets(left, right);
    }

    inline AstArrayType* NewArrayType(AstType* type, AstBrackets* brackets)
    {
        return new (Alloc(sizeof(AstArrayType))) AstArrayType(type, brackets);
    }

    inline AstTypeName* NewTypeName(AstName* name)
    {
        return new (Alloc(sizeof(AstTypeName))) AstTypeName(this, name);
    }

    inline AstPackageDeclaration* NewPackageDeclaration()
    {
        return new (Alloc(sizeof(AstPackageDeclaration)))
            AstPackageDeclaration();
    }

    inline AstImportDeclaration* NewImportDeclaration()
    {
        return new (Alloc(sizeof(AstImportDeclaration)))
            AstImportDeclaration();
    }

    inline AstCompilationUnit* NewCompilationUnit()
    {
        return new (Alloc(sizeof(AstCompilationUnit)))
            AstCompilationUnit(this);
    }

    inline AstModifiers* NewModifier(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstModifiers))) AstModifiers(token);
    }

    inline AstEmptyDeclaration* NewEmptyDeclaration(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstEmptyDeclaration)))
            AstEmptyDeclaration(token);
    }

    inline AstClassBody* NewClassBody()
    {
        return new (Alloc(sizeof(AstClassBody))) AstClassBody(this);
    }

    inline AstClassDeclaration* NewClassDeclaration()
    {
        return new (Alloc(sizeof(AstClassDeclaration)))
            AstClassDeclaration(this);
    }

    inline AstArrayInitializer* NewArrayInitializer()
    {
        return new (Alloc(sizeof(AstArrayInitializer)))
            AstArrayInitializer(this);
    }

    inline AstVariableDeclaratorId* NewVariableDeclaratorId()
    {
        return new (Alloc(sizeof(AstVariableDeclaratorId)))
            AstVariableDeclaratorId();
    }

    inline AstVariableDeclarator* NewVariableDeclarator()
    {
        return new (Alloc(sizeof(AstVariableDeclarator)))
            AstVariableDeclarator();
    }

    inline AstFieldDeclaration* NewFieldDeclaration()
    {
        return new (Alloc(sizeof(AstFieldDeclaration)))
            AstFieldDeclaration(this);
    }

    inline AstFormalParameter* NewFormalParameter()
    {
        return new (Alloc(sizeof(AstFormalParameter))) AstFormalParameter();
    }

    inline AstMethodDeclarator* NewMethodDeclarator()
    {
        return new (Alloc(sizeof(AstMethodDeclarator)))
            AstMethodDeclarator(this);
    }

    inline AstMethodBody* NewMethodBody()
    {
        return new (Alloc(sizeof(AstMethodBody))) AstMethodBody(this);
    }

    inline AstMethodDeclaration* NewMethodDeclaration()
    {
        return new (Alloc(sizeof(AstMethodDeclaration)))
            AstMethodDeclaration(this);
    }

    inline AstInitializerDeclaration* NewInitializerDeclaration()
    {
        return new (Alloc(sizeof(AstInitializerDeclaration)))
            AstInitializerDeclaration();
    }

    inline AstArguments* NewArguments(LexStream::TokenIndex left,
                                      LexStream::TokenIndex right)
    {
        return new (Alloc(sizeof(AstArguments)))
            AstArguments(this, left, right);
    }

    inline AstThisCall* NewThisCall()
    {
        return new (Alloc(sizeof(AstThisCall))) AstThisCall();
    }

    inline AstSuperCall* NewSuperCall()
    {
        return new (Alloc(sizeof(AstSuperCall))) AstSuperCall();
    }

    inline AstConstructorDeclaration* NewConstructorDeclaration()
    {
        return new (Alloc(sizeof(AstConstructorDeclaration)))
            AstConstructorDeclaration(this);
    }

    inline AstInterfaceDeclaration* NewInterfaceDeclaration()
    {
        return new (Alloc(sizeof(AstInterfaceDeclaration)))
            AstInterfaceDeclaration(this);
    }

    inline AstLocalVariableDeclarationStatement* NewLocalVariableDeclarationStatement()
    {
        return new (Alloc(sizeof(AstLocalVariableDeclarationStatement)))
            AstLocalVariableDeclarationStatement(this);
    }

    inline AstLocalClassDeclarationStatement* NewLocalClassDeclarationStatement(AstClassDeclaration* decl)
    {
        return new (Alloc(sizeof(AstLocalClassDeclarationStatement)))
            AstLocalClassDeclarationStatement(decl);
    }

    inline AstIfStatement* NewIfStatement()
    {
        return new (Alloc(sizeof(AstIfStatement))) AstIfStatement();
    }

    inline AstEmptyStatement* NewEmptyStatement(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstEmptyStatement)))
            AstEmptyStatement(token);
    }

    inline AstExpressionStatement* NewExpressionStatement()
    {
        return new (Alloc(sizeof(AstExpressionStatement)))
            AstExpressionStatement();
    }

    inline AstSwitchLabel* NewSwitchLabel()
    {
        return new (Alloc(sizeof(AstSwitchLabel))) AstSwitchLabel();
    }

    inline AstSwitchBlockStatement* NewSwitchBlockStatement()
    {
        return new (Alloc(sizeof(AstSwitchBlockStatement)))
            AstSwitchBlockStatement(this);
    }

    inline AstSwitchStatement* NewSwitchStatement()
    {
        return new (Alloc(sizeof(AstSwitchStatement)))
            AstSwitchStatement(this);
    }

    inline AstWhileStatement* NewWhileStatement()
    {
        return new (Alloc(sizeof(AstWhileStatement))) AstWhileStatement();
    }

    inline AstDoStatement* NewDoStatement()
    {
        return new (Alloc(sizeof(AstDoStatement))) AstDoStatement();
    }

    inline AstForStatement* NewForStatement()
    {
        return new (Alloc(sizeof(AstForStatement))) AstForStatement(this);
    }

    inline AstBreakStatement* NewBreakStatement()
    {
        return new (Alloc(sizeof(AstBreakStatement))) AstBreakStatement();
    }

    inline AstContinueStatement* NewContinueStatement()
    {
        return new (Alloc(sizeof(AstContinueStatement)))
            AstContinueStatement();
    }

    inline AstReturnStatement* NewReturnStatement()
    {
        return new (Alloc(sizeof(AstReturnStatement)))
            AstReturnStatement();
    }

    inline AstThrowStatement* NewThrowStatement()
    {
        return new (Alloc(sizeof(AstThrowStatement))) AstThrowStatement();
    }

    inline AstSynchronizedStatement* NewSynchronizedStatement()
    {
        return new (Alloc(sizeof(AstSynchronizedStatement)))
            AstSynchronizedStatement();
    }

    inline AstAssertStatement* NewAssertStatement()
    {
        return new (Alloc(sizeof(AstAssertStatement)))
            AstAssertStatement();
    }

    inline AstCatchClause* NewCatchClause()
    {
        return new (Alloc(sizeof(AstCatchClause))) AstCatchClause();
    }

    inline AstFinallyClause* NewFinallyClause()
    {
        return new (Alloc(sizeof(AstFinallyClause))) AstFinallyClause();
    }

    inline AstTryStatement* NewTryStatement()
    {
        return new (Alloc(sizeof(AstTryStatement))) AstTryStatement(this);
    }

    inline AstIntegerLiteral* NewIntegerLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstIntegerLiteral))) AstIntegerLiteral(token);
    }

    inline AstLongLiteral* NewLongLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstLongLiteral))) AstLongLiteral(token);
    }

    inline AstFloatLiteral* NewFloatLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstFloatLiteral))) AstFloatLiteral(token);
    }

    inline AstDoubleLiteral* NewDoubleLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstDoubleLiteral))) AstDoubleLiteral(token);
    }

    inline AstTrueLiteral* NewTrueLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstTrueLiteral))) AstTrueLiteral(token);
    }

    inline AstFalseLiteral* NewFalseLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstFalseLiteral))) AstFalseLiteral(token);
    }

    inline AstStringLiteral* NewStringLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstStringLiteral))) AstStringLiteral(token);
    }

    inline AstCharacterLiteral* NewCharacterLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstCharacterLiteral)))
            AstCharacterLiteral(token);
    }

    inline AstNullLiteral* NewNullLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstNullLiteral))) AstNullLiteral(token);
    }

    inline AstClassLiteral* NewClassLiteral(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstClassLiteral))) AstClassLiteral(token);
    }

    inline AstThisExpression* NewThisExpression(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstThisExpression))) AstThisExpression(token);
    }

    inline AstSuperExpression* NewSuperExpression(LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstSuperExpression)))
            AstSuperExpression(token);
    }

    inline AstParenthesizedExpression* NewParenthesizedExpression()
    {
        return new (Alloc(sizeof(AstParenthesizedExpression)))
            AstParenthesizedExpression();
    }

    inline AstClassInstanceCreationExpression* NewClassInstanceCreationExpression()
    {
        return new (Alloc(sizeof(AstClassInstanceCreationExpression)))
            AstClassInstanceCreationExpression();
    }

    inline AstDimExpr* NewDimExpr()
    {
        return new (Alloc(sizeof(AstDimExpr))) AstDimExpr();
    }

    inline AstArrayCreationExpression* NewArrayCreationExpression()
    {
        return new (Alloc(sizeof(AstArrayCreationExpression)))
            AstArrayCreationExpression(this);
    }

    inline AstFieldAccess* NewFieldAccess()
    {
        return new (Alloc(sizeof(AstFieldAccess))) AstFieldAccess();
    }

    inline AstMethodInvocation* NewMethodInvocation()
    {
        return new (Alloc(sizeof(AstMethodInvocation))) AstMethodInvocation();
    }

    inline AstArrayAccess* NewArrayAccess()
    {
        return new (Alloc(sizeof(AstArrayAccess))) AstArrayAccess();
    }

    inline AstPostUnaryExpression* NewPostUnaryExpression(AstPostUnaryExpression::PostUnaryExpressionTag tag)
    {
        return new (Alloc(sizeof(AstPostUnaryExpression)))
            AstPostUnaryExpression(tag);
    }

    inline AstPreUnaryExpression* NewPreUnaryExpression(AstPreUnaryExpression::PreUnaryExpressionTag tag)
    {
        return new (Alloc(sizeof(AstPreUnaryExpression)))
            AstPreUnaryExpression(tag);
    }

    inline AstCastExpression* NewCastExpression()
    {
        return new (Alloc(sizeof(AstCastExpression))) AstCastExpression();
    }

    inline AstBinaryExpression* NewBinaryExpression(AstBinaryExpression::BinaryExpressionTag tag)
    {
        return new (Alloc(sizeof(AstBinaryExpression)))
            AstBinaryExpression(tag);
    }

    inline AstInstanceofExpression* NewInstanceofExpression()
    {
        return new (Alloc(sizeof(AstInstanceofExpression)))
            AstInstanceofExpression();
    }

    inline AstConditionalExpression* NewConditionalExpression()
    {
        return new (Alloc(sizeof(AstConditionalExpression)))
            AstConditionalExpression();
    }

    inline AstAssignmentExpression* NewAssignmentExpression(AstAssignmentExpression::AssignmentExpressionTag tag,
                                                            LexStream::TokenIndex token)
    {
        return new (Alloc(sizeof(AstAssignmentExpression)))
            AstAssignmentExpression(tag, token);
    }

    // *********************************************************************

    //
    // Note that CaseElement nodes are always generated. Since they are not
    // Ast nodes they do not need to be marked.
    //
    inline CaseElement* GenCaseElement(unsigned block_index,
                                       unsigned case_index)
    {
        CaseElement* p = new (Alloc(sizeof(CaseElement))) CaseElement();
        p -> block_index = block_index;
        p -> case_index = case_index;
        return p;
    }

    inline AstBlock* GenBlock()
    {
        AstBlock* p = NewBlock();
        p -> generated = true;
        p -> no_braces = true;
        return p;
    }

    inline AstName* GenName(LexStream::TokenIndex token)
    {
        AstName* p = NewName(token);
        p -> generated = true;
        return p;
    }

    inline AstPrimitiveType* GenPrimitiveType(Ast::Kind kind,
                                              LexStream::TokenIndex token)
    {
        AstPrimitiveType* p = NewPrimitiveType(kind, token);
        p -> generated = true;
        return p;
    }

    inline AstBrackets* GenBrackets(LexStream::TokenIndex left,
                                    LexStream::TokenIndex right)
    {
        AstBrackets* p = NewBrackets(left, right);
        p -> generated = true;
        return p;
    }

    inline AstArrayType* GenArrayType(AstType* type, AstBrackets* brackets)
    {
        AstArrayType* p = NewArrayType(type, brackets);
        p -> generated = true;
        return p;
    }

    inline AstTypeName* GenTypeName(AstName* type)
    {
        AstTypeName* p = NewTypeName(type);
        p -> generated = true;
        return p;
    }

    inline AstPackageDeclaration* GenPackageDeclaration()
    {
        AstPackageDeclaration* p = NewPackageDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstImportDeclaration* GenImportDeclaration()
    {
        AstImportDeclaration* p = NewImportDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstCompilationUnit* GenCompilationUnit()
    {
        AstCompilationUnit* p = NewCompilationUnit();
        p -> generated = true;
        return p;
    }

    inline AstModifiers* GenModifier(LexStream::TokenIndex token)
    {
        AstModifiers* p = NewModifier(token);
        p -> generated = true;
        return p;
    }

    inline AstEmptyDeclaration* GenEmptyDeclaration(LexStream::TokenIndex token)
    {
        AstEmptyDeclaration* p = NewEmptyDeclaration(token);
        p -> generated = true;
        return p;
    }

    inline AstClassBody* GenClassBody()
    {
        AstClassBody* p = NewClassBody();
        p -> generated = true;
        return p;
    }

    inline AstClassDeclaration* GenClassDeclaration()
    {
        AstClassDeclaration* p = NewClassDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstArrayInitializer* GenArrayInitializer()
    {
        AstArrayInitializer* p = NewArrayInitializer();
        p -> generated = true;
        return p;
    }

    inline AstVariableDeclaratorId* GenVariableDeclaratorId()
    {
        AstVariableDeclaratorId* p = NewVariableDeclaratorId();
        p -> generated = true;
        return p;
    }

    inline AstVariableDeclarator* GenVariableDeclarator()
    {
        AstVariableDeclarator* p = NewVariableDeclarator();
        p -> generated = true;
        return p;
    }

    inline AstFieldDeclaration* GenFieldDeclaration()
    {
        AstFieldDeclaration* p = NewFieldDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstFormalParameter* GenFormalParameter()
    {
        AstFormalParameter* p = NewFormalParameter();
        p -> generated = true;
        return p;
    }

    inline AstMethodDeclarator* GenMethodDeclarator()
    {
        AstMethodDeclarator* p = NewMethodDeclarator();
        p -> generated = true;
        return p;
    }

    inline AstMethodBody* GenMethodBody()
    {
        AstMethodBody* p = NewMethodBody();
        p -> generated = true;
        return p;
    }

    inline AstMethodDeclaration* GenMethodDeclaration()
    {
        AstMethodDeclaration* p = NewMethodDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstInitializerDeclaration* GenInitializerDeclaration()
    {
        AstInitializerDeclaration* p = NewInitializerDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstArguments* GenArguments(LexStream::TokenIndex left,
                                      LexStream::TokenIndex right)
    {
        AstArguments* p = NewArguments(left, right);
        p -> generated = true;
        return p;
    }

    inline AstThisCall* GenThisCall()
    {
        AstThisCall* p = NewThisCall();
        p -> generated = true;
        return p;
    }

    inline AstSuperCall* GenSuperCall()
    {
        AstSuperCall* p = NewSuperCall();
        p -> generated = true;
        return p;
    }

    inline AstConstructorDeclaration* GenConstructorDeclaration()
    {
        AstConstructorDeclaration* p = NewConstructorDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstInterfaceDeclaration* GenInterfaceDeclaration()
    {
        AstInterfaceDeclaration* p = NewInterfaceDeclaration();
        p -> generated = true;
        return p;
    }

    inline AstLocalVariableDeclarationStatement* GenLocalVariableDeclarationStatement()
    {
        AstLocalVariableDeclarationStatement* p =
            NewLocalVariableDeclarationStatement();
        p -> generated = true;
        return p;
    }

    inline AstLocalClassDeclarationStatement* GenLocalClassDeclarationStatement(AstClassDeclaration* decl)
    {
        AstLocalClassDeclarationStatement* p =
            NewLocalClassDeclarationStatement(decl);
        p -> generated = true;
        return p;
    }

    inline AstIfStatement* GenIfStatement()
    {
        AstIfStatement* p = NewIfStatement();
        p -> generated = true;
        return p;
    }

    inline AstEmptyStatement* GenEmptyStatement(LexStream::TokenIndex token)
    {
        AstEmptyStatement* p = NewEmptyStatement(token);
        p -> generated = true;
        return p;
    }

    inline AstExpressionStatement* GenExpressionStatement()
    {
        AstExpressionStatement* p = NewExpressionStatement();
        p -> generated = true;
        return p;
    }

    inline AstSwitchLabel* GenSwitchLabel()
    {
        AstSwitchLabel* p = NewSwitchLabel();
        p -> generated = true;
        return p;
    }

    inline AstSwitchBlockStatement* GenSwitchBlockStatement()
    {
        AstSwitchBlockStatement* p = NewSwitchBlockStatement();
        p -> generated = true;
        return p;
    }

    inline AstSwitchStatement* GenSwitchStatement()
    {
        AstSwitchStatement* p = NewSwitchStatement();
        p -> generated = true;
        return p;
    }

    inline AstWhileStatement* GenWhileStatement()
    {
        AstWhileStatement* p = NewWhileStatement();
        p -> generated = true;
        return p;
    }

    inline AstDoStatement* GenDoStatement()
    {
        AstDoStatement* p = NewDoStatement();
        p -> generated = true;
        return p;
    }

    inline AstForStatement* GenForStatement()
    {
        AstForStatement* p = NewForStatement();
        p -> generated = true;
        return p;
    }

    inline AstBreakStatement* GenBreakStatement()
    {
        AstBreakStatement* p = NewBreakStatement();
        p -> generated = true;
        return p;
    }

    inline AstContinueStatement* GenContinueStatement()
    {
        AstContinueStatement* p = NewContinueStatement();
        p -> generated = true;
        return p;
    }

    inline AstReturnStatement* GenReturnStatement()
    {
        AstReturnStatement* p = NewReturnStatement();
        p -> generated = true;
        return p;
    }

    inline AstThrowStatement* GenThrowStatement()
    {
        AstThrowStatement* p = NewThrowStatement();
        p -> generated = true;
        return p;
    }

    inline AstSynchronizedStatement* GenSynchronizedStatement()
    {
        AstSynchronizedStatement* p = NewSynchronizedStatement();
        p -> generated = true;
        return p;
    }

    inline AstAssertStatement* GenAssertStatement()
    {
        AstAssertStatement* p = NewAssertStatement();
        p -> generated = true;
        return p;
    }

    inline AstCatchClause* GenCatchClause()
    {
        AstCatchClause* p = NewCatchClause();
        p -> generated = true;
        return p;
    }

    inline AstFinallyClause* GenFinallyClause()
    {
        AstFinallyClause* p = NewFinallyClause();
        p -> generated = true;
        return p;
    }

    inline AstTryStatement* GenTryStatement()
    {
        AstTryStatement* p = NewTryStatement();
        p -> generated = true;
        return p;
    }

    inline AstIntegerLiteral* GenIntegerLiteral(LexStream::TokenIndex token)
    {
        AstIntegerLiteral* p = NewIntegerLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstLongLiteral* GenLongLiteral(LexStream::TokenIndex token)
    {
        AstLongLiteral* p = NewLongLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstFloatLiteral* GenFloatLiteral(LexStream::TokenIndex token)
    {
        AstFloatLiteral* p = NewFloatLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstDoubleLiteral* GenDoubleLiteral(LexStream::TokenIndex token)
    {
        AstDoubleLiteral* p = NewDoubleLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstTrueLiteral* GenTrueLiteral(LexStream::TokenIndex token)
    {
        AstTrueLiteral* p = NewTrueLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstFalseLiteral* GenFalseLiteral(LexStream::TokenIndex token)
    {
        AstFalseLiteral* p = NewFalseLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstStringLiteral* GenStringLiteral(LexStream::TokenIndex token)
    {
        AstStringLiteral* p = NewStringLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstCharacterLiteral* GenCharacterLiteral(LexStream::TokenIndex token)
    {
        AstCharacterLiteral* p = NewCharacterLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstNullLiteral* GenNullLiteral(LexStream::TokenIndex token)
    {
        AstNullLiteral* p = NewNullLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstClassLiteral* GenClassLiteral(LexStream::TokenIndex token)
    {
        AstClassLiteral* p = NewClassLiteral(token);
        p -> generated = true;
        return p;
    }

    inline AstThisExpression* GenThisExpression(LexStream::TokenIndex token)
    {
        AstThisExpression* p = NewThisExpression(token);
        p -> generated = true;
        return p;
    }

    inline AstSuperExpression* GenSuperExpression(LexStream::TokenIndex token)
    {
        AstSuperExpression* p = NewSuperExpression(token);
        p -> generated = true;
        return p;
    }

    inline AstParenthesizedExpression* GenParenthesizedExpression()
    {
        AstParenthesizedExpression* p = NewParenthesizedExpression();
        p -> generated = true;
        return p;
    }

    inline AstClassInstanceCreationExpression* GenClassInstanceCreationExpression()
    {
        AstClassInstanceCreationExpression* p =
            NewClassInstanceCreationExpression();
        p -> generated = true;
        return p;
    }

    inline AstDimExpr* GenDimExpr()
    {
        AstDimExpr* p = NewDimExpr();
        p -> generated = true;
        return p;
    }

    inline AstArrayCreationExpression* GenArrayCreationExpression()
    {
        AstArrayCreationExpression* p = NewArrayCreationExpression();
        p -> generated = true;
        return p;
    }

    inline AstFieldAccess* GenFieldAccess()
    {
        AstFieldAccess* p = NewFieldAccess();
        p -> generated = true;
        return p;
    }

    inline AstMethodInvocation* GenMethodInvocation()
    {
        AstMethodInvocation* p = NewMethodInvocation();
        p -> generated = true;
        return p;
    }

    inline AstArrayAccess* GenArrayAccess()
    {
        AstArrayAccess* p = NewArrayAccess();
        p -> generated = true;
        return p;
    }

    inline AstPostUnaryExpression* GenPostUnaryExpression(AstPostUnaryExpression::PostUnaryExpressionTag tag)
    {
        AstPostUnaryExpression* p = NewPostUnaryExpression(tag);
        p -> generated = true;
        return p;
    }

    inline AstPreUnaryExpression* GenPreUnaryExpression(AstPreUnaryExpression::PreUnaryExpressionTag tag)
    {
        AstPreUnaryExpression* p = NewPreUnaryExpression(tag);
        p -> generated = true;
        return p;
    }

    inline AstCastExpression* GenCastExpression()
    {
        AstCastExpression* p = NewCastExpression();
        p -> generated = true;
        return p;
    }

    inline AstBinaryExpression* GenBinaryExpression(AstBinaryExpression::BinaryExpressionTag tag)
    {
        AstBinaryExpression* p = NewBinaryExpression(tag);
        p -> generated = true;
        return p;
    }

    inline AstInstanceofExpression* GenInstanceofExpression()
    {
        AstInstanceofExpression* p = NewInstanceofExpression();
        p -> generated = true;
        return p;
    }

    inline AstConditionalExpression* GenConditionalExpression()
    {
        AstConditionalExpression* p = NewConditionalExpression();
        p -> generated = true;
        return p;
    }

    inline AstAssignmentExpression* GenAssignmentExpression(AstAssignmentExpression::AssignmentExpressionTag tag,
                                                            LexStream::TokenIndex token)
    {
        AstAssignmentExpression* p = NewAssignmentExpression(tag, token);
        p -> generated = true;
        return p;
    }

    // *********************************************************************

    //
    // Return the total size of temporary space allocated.
    //
    size_t SpaceAllocated(void)
    {
        return (base_size * sizeof(Cell**)) + (size * sizeof(Cell));
    }

    //
    // Return the total size of temporary space used.
    //
    size_t SpaceUsed(void)
    {
        return (((size >> log_blksize) * sizeof(Cell**)) +
                top * sizeof(Cell));
    }
};


//
// Older compilers do not support templatized member methods. If it were not
// for that fact, this method should be a member of StoragePool.
//
template <typename T>
inline AstArray<T>* NewAstArray(StoragePool* pool, unsigned estimate = 0)
{
    return ! estimate ? NULL
        : new (pool -> Alloc(sizeof(AstArray<T>))) AstArray<T>(pool, estimate);
}

//
// Cast conversions for Ast
//

inline AstStatement* Ast::StatementCast()
{
    return DYNAMIC_CAST<AstStatement*> (class_tag == STATEMENT ? this : NULL);
}

inline AstExpression* Ast::ExpressionCast()
{
    return DYNAMIC_CAST<AstExpression*>
        (class_tag == EXPRESSION ? this : NULL);
}

inline AstPrimitiveType* Ast::PrimitiveTypeCast()
{
    return DYNAMIC_CAST<AstPrimitiveType*>
        (class_tag == PRIMITIVE_TYPE ? this : NULL);
}

inline AstFieldDeclaration* Ast::StaticFieldCast()
{
    return DYNAMIC_CAST<AstFieldDeclaration*>
        (kind == FIELD && class_tag == STATIC ? this : NULL);
}

inline AstInitializerDeclaration* Ast::StaticInitializerCast()
{
    return DYNAMIC_CAST<AstInitializerDeclaration*>
        (kind == INITIALIZER && class_tag == STATIC ? this : NULL);
}

inline AstClassBody* Ast::UnparsedClassBodyCast()
{
    return DYNAMIC_CAST<AstClassBody*>
        (kind == CLASS_BODY && class_tag == UNPARSED ? this : NULL);
}

inline AstCompilationUnit* Ast::BadCompilationUnitCast()
{
    return DYNAMIC_CAST<AstCompilationUnit*>
        (kind == BAD_COMPILATION ? this : NULL);
}

inline AstCompilationUnit* Ast::EmptyCompilationUnitCast()
{
    return DYNAMIC_CAST<AstCompilationUnit*>
        (kind == EMPTY_COMPILATION ? this : NULL);
}

//
// These cast functions are used for classes that represent exactly
// one kind of node.
//

inline AstListNode* Ast::ListNodeCast()
{
    return DYNAMIC_CAST<AstListNode*> (kind == LIST_NODE ? this : NULL);
}

inline AstBlock* Ast::BlockCast()
{
    return DYNAMIC_CAST<AstBlock*>
        (kind == BLOCK || kind == METHOD_BODY || kind == SWITCH_BLOCK
         ? this : NULL);
}

inline AstName* Ast::NameCast()
{
    return DYNAMIC_CAST<AstName*> (kind == NAME ? this : NULL);
}

inline AstBrackets* Ast::BracketsCast()
{
    return DYNAMIC_CAST<AstBrackets*> (kind == BRACKETS ? this : NULL);
}

inline AstArrayType* Ast::ArrayTypeCast()
{
    return DYNAMIC_CAST<AstArrayType*> (kind == ARRAY ? this : NULL);
}

inline AstTypeName* Ast::TypeNameCast()
{
    return DYNAMIC_CAST<AstTypeName*> (kind == TYPE ? this : NULL);
}

inline AstPackageDeclaration* Ast::PackageDeclarationCast()
{
    return DYNAMIC_CAST<AstPackageDeclaration*>
        (kind == PACKAGE ? this : NULL);
}

inline AstImportDeclaration* Ast::ImportDeclarationCast()
{
    return DYNAMIC_CAST<AstImportDeclaration*> (kind == IMPORT ? this : NULL);
}

inline AstCompilationUnit* Ast::CompilationUnitCast()
{
    return DYNAMIC_CAST<AstCompilationUnit*>
        (kind == COMPILATION || kind == BAD_COMPILATION
            || kind == EMPTY_COMPILATION ? this : NULL);
}

inline AstModifiers* Ast::ModifiersCast()
{
    return DYNAMIC_CAST<AstModifiers*> (kind == MODIFIERS ? this : NULL);
}

inline AstEmptyDeclaration* Ast::EmptyDeclarationCast()
{
    return DYNAMIC_CAST<AstEmptyDeclaration*>
        (kind == EMPTY_DECLARATION ? this : NULL);
}

inline AstClassBody* Ast::ClassBodyCast()
{
    return DYNAMIC_CAST<AstClassBody*> (kind == CLASS_BODY ? this : NULL);
}

inline AstClassDeclaration* Ast::ClassDeclarationCast()
{
    return DYNAMIC_CAST<AstClassDeclaration*> (kind == CLASS ? this : NULL);
}

inline AstArrayInitializer* Ast::ArrayInitializerCast()
{
    return DYNAMIC_CAST<AstArrayInitializer*>
        (kind == ARRAY_INITIALIZER ? this : NULL);
}

inline AstVariableDeclaratorId* Ast::VariableDeclaratorIdCast()
{
    return DYNAMIC_CAST<AstVariableDeclaratorId*>
        (kind == VARIABLE_DECLARATOR_NAME ? this : NULL);
}

inline AstVariableDeclarator* Ast::VariableDeclaratorCast()
{
    return DYNAMIC_CAST<AstVariableDeclarator*>
        (kind == VARIABLE_DECLARATOR ? this : NULL);
}

inline AstFieldDeclaration* Ast::FieldDeclarationCast()
{
    return DYNAMIC_CAST<AstFieldDeclaration*> (kind == FIELD ? this : NULL);
}

inline AstFormalParameter* Ast::FormalParameterCast()
{
    return DYNAMIC_CAST<AstFormalParameter*> (kind == PARAMETER ? this : NULL);
}

inline AstMethodDeclarator* Ast::MethodDeclaratorCast()
{
    return DYNAMIC_CAST<AstMethodDeclarator*>
        (kind == METHOD_DECLARATOR ? this : NULL);
}

inline AstMethodBody* Ast::MethodBodyCast()
{
    return DYNAMIC_CAST<AstMethodBody*> (kind == METHOD_BODY ? this : NULL);
}

inline AstMethodDeclaration* Ast::MethodDeclarationCast()
{
    return DYNAMIC_CAST<AstMethodDeclaration*> (kind == METHOD ? this : NULL);
}

inline AstInitializerDeclaration* Ast::InitializerDeclarationCast()
{
    return DYNAMIC_CAST<AstInitializerDeclaration*>
        (kind == INITIALIZER ? this : NULL);
}

inline AstArguments* Ast::ArgumentsCast()
{
    return DYNAMIC_CAST<AstArguments*> (kind == ARGUMENTS ? this : NULL);
}

inline AstThisCall* Ast::ThisCallCast()
{
    return DYNAMIC_CAST<AstThisCall*> (kind == THIS_CALL ? this : NULL);
}

inline AstSuperCall* Ast::SuperCallCast()
{
    return DYNAMIC_CAST<AstSuperCall*> (kind == SUPER_CALL ? this : NULL);
}

inline AstConstructorDeclaration* Ast::ConstructorDeclarationCast()
{
    return DYNAMIC_CAST<AstConstructorDeclaration*>
        (kind == CONSTRUCTOR ? this : NULL);
}

inline AstInterfaceDeclaration* Ast::InterfaceDeclarationCast()
{
    return DYNAMIC_CAST<AstInterfaceDeclaration*>
        (kind == INTERFACE ? this : NULL);
}

inline AstLocalVariableDeclarationStatement* Ast::LocalVariableDeclarationStatementCast()
{
    return DYNAMIC_CAST<AstLocalVariableDeclarationStatement*>
        (kind == LOCAL_VARIABLE_DECLARATION ? this : NULL);
}

inline AstLocalClassDeclarationStatement* Ast::LocalClassDeclarationStatementCast()
{
    return DYNAMIC_CAST<AstLocalClassDeclarationStatement*>
        (kind == LOCAL_CLASS ? this : NULL);
}

inline AstIfStatement* Ast::IfStatementCast()
{
    return DYNAMIC_CAST<AstIfStatement*> (kind == IF ? this : NULL);
}

inline AstEmptyStatement* Ast::EmptyStatementCast()
{
    return DYNAMIC_CAST<AstEmptyStatement*>
        (kind == EMPTY_STATEMENT ? this : NULL);
}

inline AstExpressionStatement* Ast::ExpressionStatementCast()
{
    return DYNAMIC_CAST<AstExpressionStatement*>
        (kind == EXPRESSION_STATEMENT ? this : NULL);
}

inline AstSwitchLabel* Ast::SwitchLabelCast()
{
    return DYNAMIC_CAST<AstSwitchLabel*> (kind == SWITCH_LABEL ? this : NULL);
}

inline AstSwitchBlockStatement* Ast::SwitchBlockStatementCast()
{
    return DYNAMIC_CAST<AstSwitchBlockStatement*>
        (kind == SWITCH_BLOCK ? this : NULL);
}

inline AstSwitchStatement* Ast::SwitchStatementCast()
{
    return DYNAMIC_CAST<AstSwitchStatement*> (kind == SWITCH ? this : NULL);
}

inline AstWhileStatement* Ast::WhileStatementCast()
{
    return DYNAMIC_CAST<AstWhileStatement*> (kind == WHILE ? this : NULL);
}

inline AstDoStatement* Ast::DoStatementCast()
{
    return DYNAMIC_CAST<AstDoStatement*> (kind == DO ? this : NULL);
}

inline AstForStatement* Ast::ForStatementCast()
{
    return DYNAMIC_CAST<AstForStatement*> (kind == FOR ? this : NULL);
}

inline AstBreakStatement* Ast::BreakStatementCast()
{
    return DYNAMIC_CAST<AstBreakStatement*> (kind == BREAK ? this : NULL);
}

inline AstContinueStatement* Ast::ContinueStatementCast()
{
    return DYNAMIC_CAST<AstContinueStatement*>
        (kind == CONTINUE ? this : NULL);
}

inline AstReturnStatement* Ast::ReturnStatementCast()
{
    return DYNAMIC_CAST<AstReturnStatement*> (kind == RETURN ? this : NULL);
}

inline AstThrowStatement* Ast::ThrowStatementCast()
{
    return DYNAMIC_CAST<AstThrowStatement*> (kind == THROW ? this : NULL);
}

inline AstSynchronizedStatement* Ast::SynchronizedStatementCast()
{
    return DYNAMIC_CAST<AstSynchronizedStatement*>
        (kind == SYNCHRONIZED_STATEMENT ? this : NULL);
}

inline AstAssertStatement* Ast::AssertStatementCast()
{
    return DYNAMIC_CAST<AstAssertStatement*> (kind == ASSERT ? this : NULL);
}

inline AstCatchClause* Ast::CatchClauseCast()
{
    return DYNAMIC_CAST<AstCatchClause*> (kind == CATCH ? this : NULL);
}

inline AstFinallyClause* Ast::FinallyClauseCast()
{
    return DYNAMIC_CAST<AstFinallyClause*> (kind == FINALLY ? this : NULL);
}

inline AstTryStatement* Ast::TryStatementCast()
{
    return DYNAMIC_CAST<AstTryStatement*> (kind == TRY ? this : NULL);
}

inline AstIntegerLiteral* Ast::IntegerLiteralCast()
{
    return DYNAMIC_CAST<AstIntegerLiteral*>
        (kind == INTEGER_LITERAL ? this : NULL);
}

inline AstLongLiteral* Ast::LongLiteralCast()
{
    return DYNAMIC_CAST<AstLongLiteral*> (kind == LONG_LITERAL ? this : NULL);
}

inline AstFloatLiteral* Ast::FloatLiteralCast()
{
    return DYNAMIC_CAST<AstFloatLiteral*>
        (kind == FLOAT_LITERAL ? this : NULL);
}

inline AstDoubleLiteral* Ast::DoubleLiteralCast()
{
    return DYNAMIC_CAST<AstDoubleLiteral*>
        (kind == DOUBLE_LITERAL ? this : NULL);
}

inline AstTrueLiteral* Ast::TrueLiteralCast()
{
    return DYNAMIC_CAST<AstTrueLiteral*> (kind == TRUE_LITERAL ? this : NULL);
}

inline AstFalseLiteral* Ast::FalseLiteralCast()
{
    return DYNAMIC_CAST<AstFalseLiteral*>
        (kind == FALSE_LITERAL ? this : NULL);
}

inline AstStringLiteral* Ast::StringLiteralCast()
{
    return DYNAMIC_CAST<AstStringLiteral*>
        (kind == STRING_LITERAL ? this : NULL);
}

inline AstCharacterLiteral* Ast::CharacterLiteralCast()
{
    return DYNAMIC_CAST<AstCharacterLiteral*>
        (kind == CHARACTER_LITERAL ? this : NULL);
}

inline AstNullLiteral* Ast::NullLiteralCast()
{
    return DYNAMIC_CAST<AstNullLiteral*> (kind == NULL_LITERAL ? this : NULL);
}

inline AstClassLiteral* Ast::ClassLiteralCast()
{
    return DYNAMIC_CAST<AstClassLiteral*>
        (kind == CLASS_LITERAL ? this : NULL);
}

inline AstThisExpression* Ast::ThisExpressionCast()
{
    return DYNAMIC_CAST<AstThisExpression*>
        (kind == THIS_EXPRESSION ? this : NULL);
}

inline AstSuperExpression* Ast::SuperExpressionCast()
{
    return DYNAMIC_CAST<AstSuperExpression*>
        (kind == SUPER_EXPRESSION ? this : NULL);
}

inline AstParenthesizedExpression* Ast::ParenthesizedExpressionCast()
{
    return DYNAMIC_CAST<AstParenthesizedExpression*>
        (kind == PARENTHESIZED_EXPRESSION ? this : NULL);
}

inline AstClassInstanceCreationExpression* Ast::ClassInstanceCreationExpressionCast()
{
    return DYNAMIC_CAST<AstClassInstanceCreationExpression*>
        (kind == CLASS_CREATION ? this : NULL);
}

inline AstDimExpr* Ast::DimExprCast()
{
    return DYNAMIC_CAST<AstDimExpr*> (kind == DIM ? this : NULL);
}

inline AstArrayCreationExpression* Ast::ArrayCreationExpressionCast()
{
    return DYNAMIC_CAST<AstArrayCreationExpression*>
        (kind == ARRAY_CREATION ? this : NULL);
}

inline AstFieldAccess* Ast::FieldAccessCast()
{
    return DYNAMIC_CAST<AstFieldAccess*> (kind == DOT ? this : NULL);
}

inline AstMethodInvocation* Ast::MethodInvocationCast()
{
    return DYNAMIC_CAST<AstMethodInvocation*> (kind == CALL ? this : NULL);
}

inline AstArrayAccess* Ast::ArrayAccessCast()
{
    return DYNAMIC_CAST<AstArrayAccess*> (kind == ARRAY_ACCESS ? this : NULL);
}

inline AstPostUnaryExpression* Ast::PostUnaryExpressionCast()
{
    return DYNAMIC_CAST<AstPostUnaryExpression*>
        (kind == POST_UNARY ? this : NULL);
}

inline AstPreUnaryExpression* Ast::PreUnaryExpressionCast()
{
    return DYNAMIC_CAST<AstPreUnaryExpression*>
        (kind == PRE_UNARY ? this : NULL);
}

inline AstCastExpression* Ast::CastExpressionCast()
{
    return DYNAMIC_CAST<AstCastExpression*> (kind == CAST ? this : NULL);
}

inline AstBinaryExpression* Ast::BinaryExpressionCast()
{
    return DYNAMIC_CAST<AstBinaryExpression*> (kind == BINARY ? this : NULL);
}

inline AstInstanceofExpression* Ast::InstanceofExpressionCast()
{
    return DYNAMIC_CAST<AstInstanceofExpression*>
        (kind == INSTANCEOF ? this : NULL);
}

inline AstConditionalExpression* Ast::ConditionalExpressionCast()
{
    return DYNAMIC_CAST<AstConditionalExpression*>
        (kind == CONDITIONAL ? this : NULL);
}

inline AstAssignmentExpression* Ast::AssignmentExpressionCast()
{
    return DYNAMIC_CAST<AstAssignmentExpression*>
        (kind == ASSIGNMENT ? this : NULL);
}

// **********************************************

inline bool AstDeclaredType::IsValid()
{
    return class_body && class_body -> semantic_environment;
}

inline void AstBlock::AllocateStatements(unsigned estimate)
{
    assert(! block_statements);
    block_statements = NewAstArray<AstStatement*> (pool, estimate);
}

inline void AstBlock::AddStatement(AstStatement* statement)
{
    assert(block_statements);
    block_statements -> Next() = statement;
}

inline void AstBlock::AllocateLocallyDefinedVariables(unsigned estimate)
{
    if (! defined_variables)
        defined_variables = pool -> NewVariableSymbolArray(estimate);
}

inline void AstBlock::AddLocallyDefinedVariable(VariableSymbol* variable_symbol)
{
    if (! defined_variables)
        AllocateLocallyDefinedVariables(1);
    defined_variables -> Next() = variable_symbol;
}

inline void AstCompilationUnit::AllocateImportDeclarations(unsigned estimate)
{
    assert(! import_declarations);
    import_declarations =
        NewAstArray<AstImportDeclaration*> (ast_pool, estimate);
}

inline void AstCompilationUnit::AddImportDeclaration(AstImportDeclaration* import_declaration)
{
    assert(import_declarations);
    import_declarations -> Next() = import_declaration;
}

inline void AstCompilationUnit::AllocateTypeDeclarations(unsigned estimate)
{
    assert(! type_declarations);
    type_declarations = NewAstArray<AstDeclaredType*> (ast_pool, estimate);
}

inline void AstCompilationUnit::AddTypeDeclaration(AstDeclaredType* type_declaration)
{
    assert(type_declarations);
    type_declarations -> Next() = type_declaration;
}

inline void AstClassBody::AllocateClassBodyDeclarations(unsigned estimate)
{
    assert(! class_body_declarations);
    class_body_declarations = NewAstArray<AstDeclared*> (pool, estimate);
}

inline void AstClassBody::AddClassBodyDeclaration(AstDeclared* member)
{
    assert(class_body_declarations);
    class_body_declarations -> Next() = member;
}

inline void AstClassBody::AllocateInstanceVariables(unsigned estimate)
{
    assert(! instance_variables);
    instance_variables = NewAstArray<AstFieldDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddInstanceVariable(AstFieldDeclaration* field_declaration)
{
    assert(instance_variables);
    instance_variables -> Next() = field_declaration;
}

inline void AstClassBody::AllocateClassVariables(unsigned estimate)
{
    assert(! class_variables);
    class_variables = NewAstArray<AstFieldDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddClassVariable(AstFieldDeclaration* field_declaration)
{
    assert(class_variables);
    class_variables -> Next() = field_declaration;
}

inline void AstClassBody::AllocateMethods(unsigned estimate)
{
    assert(! methods);
    methods = NewAstArray<AstMethodDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddMethod(AstMethodDeclaration* method_declaration)
{
    assert(methods);
    methods -> Next() = method_declaration;
}

inline void AstClassBody::AllocateConstructors(unsigned estimate)
{
    assert(! constructors);
    constructors = NewAstArray<AstConstructorDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddConstructor(AstConstructorDeclaration* constructor_declaration)
{
    assert(constructors);
    constructors -> Next() = constructor_declaration;
}

inline void AstClassBody::AllocateStaticInitializers(unsigned estimate)
{
    assert(! static_initializers);
    static_initializers =
        NewAstArray<AstInitializerDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddStaticInitializer(AstInitializerDeclaration* initializer)
{
    assert(static_initializers);
    static_initializers -> Next() = initializer;
}

inline void AstClassBody::AllocateInstanceInitializers(unsigned estimate)
{
    assert(! instance_initializers);
    instance_initializers =
        NewAstArray<AstInitializerDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddInstanceInitializer(AstInitializerDeclaration* initializer)
{
    assert(instance_initializers);
    instance_initializers -> Next() = initializer;
}

inline void AstClassBody::AllocateNestedClasses(unsigned estimate)
{
    assert(! inner_classes);
    inner_classes = NewAstArray<AstClassDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddNestedClass(AstClassDeclaration* class_declaration)
{
    assert(inner_classes);
    inner_classes -> Next() = class_declaration;
}

inline void AstClassBody::AllocateNestedInterfaces(unsigned estimate)
{
    assert(! inner_interfaces);
    inner_interfaces = NewAstArray<AstInterfaceDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddNestedInterface(AstInterfaceDeclaration* interface_declaration)
{
    assert(inner_interfaces);
    inner_interfaces -> Next() = interface_declaration;
}

inline void AstClassBody::AllocateEmptyDeclarations(unsigned estimate)
{
    assert(! empty_declarations);
    empty_declarations = NewAstArray<AstEmptyDeclaration*> (pool, estimate);
}

inline void AstClassBody::AddEmptyDeclaration(AstEmptyDeclaration* empty_declaration)
{
    assert(empty_declarations);
    empty_declarations -> Next() = empty_declaration;
}

inline void AstClassDeclaration::AllocateInterfaces(unsigned estimate)
{
    assert(! interfaces);
    interfaces = NewAstArray<AstTypeName*> (pool, estimate);
}

inline void AstClassDeclaration::AddInterface(AstTypeName* interf)
{
    assert(interfaces);
    interfaces -> Next() = interf;
}

inline void AstArrayInitializer::AllocateVariableInitializers(unsigned estimate)
{
    assert(! variable_initializers);
    variable_initializers = NewAstArray<Ast*> (pool, estimate);
}

inline void AstArrayInitializer::AddVariableInitializer(Ast* initializer)
{
    assert(variable_initializers);
    variable_initializers -> Next() = initializer;
}

inline void AstFieldDeclaration::AllocateVariableDeclarators(unsigned estimate)
{
    assert(! variable_declarators);
    variable_declarators =
        NewAstArray<AstVariableDeclarator*> (pool, estimate);
}

inline void AstFieldDeclaration::AddVariableDeclarator(AstVariableDeclarator* variable_declarator)
{
    assert(variable_declarators);
    variable_declarators -> Next() = variable_declarator;
}

inline void AstMethodDeclarator::AllocateFormalParameters(unsigned estimate)
{
    assert(! formal_parameters);
    formal_parameters = NewAstArray<AstFormalParameter*> (pool, estimate);
}

inline void AstMethodDeclarator::AddFormalParameter(AstFormalParameter* formal_parameter)
{
    assert(formal_parameters);
    formal_parameters -> Next() = formal_parameter;
}

inline void AstMethodDeclaration::AllocateThrows(unsigned estimate)
{
    assert(! throws);
    throws = NewAstArray<AstTypeName*> (pool, estimate);
}

inline void AstMethodDeclaration::AddThrow(AstTypeName* exception)
{
    assert(throws);
    throws -> Next() = exception;
}

inline void AstArguments::AllocateArguments(unsigned estimate)
{
    assert(! arguments);
    arguments = NewAstArray<AstExpression*> (pool, estimate);
}

inline void AstArguments::AddArgument(AstExpression* argument)
{
    assert(arguments);
    arguments -> Next() = argument;
}

inline void AstArguments::AllocateLocalArguments(unsigned estimate)
{
    assert(! shadow_arguments);
    shadow_arguments = NewAstArray<AstName*> (pool, estimate);
}

inline void AstArguments::AddLocalArgument(AstName* argument)
{
    assert(shadow_arguments);
    shadow_arguments -> Next() = argument;
}

inline void AstConstructorDeclaration::AllocateThrows(unsigned estimate)
{
    assert(! throws);
    throws = NewAstArray<AstTypeName*> (pool, estimate);
}

inline void AstConstructorDeclaration::AddThrow(AstTypeName* exception)
{
    assert(throws);
    throws -> Next() = exception;
}

inline void AstInterfaceDeclaration::AllocateInterfaces(unsigned estimate)
{
    assert(! interfaces);
    interfaces = NewAstArray<AstTypeName*> (pool, estimate);
}

inline void AstInterfaceDeclaration::AddInterface(AstTypeName* interf)
{
    assert(interfaces);
    interfaces -> Next() = interf;
}

inline void AstLocalVariableDeclarationStatement::AllocateVariableDeclarators(unsigned estimate)
{
    assert(! variable_declarators);
    variable_declarators =
        NewAstArray<AstVariableDeclarator*> (pool, estimate);
}

inline void AstLocalVariableDeclarationStatement::AddVariableDeclarator(AstVariableDeclarator* variable_declarator)
{
    assert(variable_declarators);
    variable_declarators -> Next() = variable_declarator;
}

inline void AstSwitchBlockStatement::AllocateSwitchLabels(unsigned estimate)
{
    assert(! switch_labels);
    switch_labels = NewAstArray<AstSwitchLabel*> (pool, estimate);
}

inline void AstSwitchBlockStatement::AddSwitchLabel(AstSwitchLabel* case_label)
{
    assert(switch_labels);
    switch_labels -> Next() = case_label;
}

inline void AstSwitchStatement::AllocateCases(unsigned estimate)
{
    //
    // Reserve element 0 for the default case.
    //
    assert(! cases);
    cases = NewAstArray<CaseElement*> (pool, estimate + 1);
    cases -> Next() = NULL;
}

inline void AstSwitchStatement::AddCase(CaseElement* case_element)
{
    assert(cases);
    cases -> Next() = case_element;
}

inline void AstForStatement::AllocateForInitStatements(unsigned estimate)
{
    assert(! for_init_statements);
    for_init_statements = NewAstArray<AstStatement*> (pool, estimate);
}

inline void AstForStatement::AddForInitStatement(AstStatement* statement)
{
    assert(for_init_statements);
    for_init_statements -> Next() = statement;
}

inline void AstForStatement::AllocateForUpdateStatements(unsigned estimate)
{
    assert(! for_update_statements);
    for_update_statements =
        NewAstArray<AstExpressionStatement*> (pool, estimate);
}

inline void AstForStatement::AddForUpdateStatement(AstExpressionStatement* statement)
{
    assert(for_update_statements);
    for_update_statements -> Next() = statement;
}

inline void AstTryStatement::AllocateCatchClauses(unsigned estimate)
{
    assert(! catch_clauses);
    catch_clauses = NewAstArray<AstCatchClause*> (pool, estimate);
}

inline void AstTryStatement::AddCatchClause(AstCatchClause* catch_clause)
{
    assert(catch_clauses);
    catch_clauses -> Next() = catch_clause;
}

inline void AstArrayCreationExpression::AllocateDimExprs(unsigned estimate)
{
    assert(! dim_exprs);
    dim_exprs = NewAstArray<AstDimExpr*> (pool, estimate);
}

inline void AstArrayCreationExpression::AddDimExpr(AstDimExpr* dim_expr)
{
    assert(dim_exprs);
    dim_exprs -> Next() = dim_expr;
}

// ******************************************

//
// Allocate another block of storage for the ast array.
//
template <typename T>
void AstArray<T>::AllocateMoreSpace()
{
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
    size_t k = size >> log_blksize; /* which segment? */

    //
    // If the base is overflowed, reallocate it and initialize the new
    // elements to NULL.
    //
    if (k == base_size)
    {
        int old_base_size = base_size;
        T** old_base = base;
        base_size += base_increment;

        // There must be enough room to allocate base
        assert(base_size <= pool -> Blksize());
        base = (T**) pool -> Alloc(sizeof(T*) * base_size);

        if (old_base != NULL)
            memcpy(base, old_base, old_base_size * sizeof(T*));
        memset(&base[old_base_size], 0,
               (base_size - old_base_size) * sizeof(T*));
    }

    //
    // We allocate a new segment and place its adjusted address in
    // base[k]. The adjustment allows us to index the segment directly,
    // instead of having to perform a subtraction for each reference.
    // See operator[] below.
    //
    // There must be enough room to allocate block
    assert(Blksize() <= pool -> Blksize());

    base[k] = (T*) pool -> Alloc(sizeof(T) * Blksize());
    base[k] -= size;

    //
    // Finally, we update size.
    //
    size += Blksize();
}


//
// Constructor of a ast array.
//
template <typename T>
AstArray<T>::AstArray(StoragePool* p, unsigned estimate) : pool(p)
{
    // AstArray should only be used for arrays of pointers.
    assert(sizeof(T) == sizeof(StoragePool::Cell));
    // There must be enough space in the storage pool to move !!!
    assert(pool -> Blksize() >= 256);

    if (estimate == 0)
        log_blksize = 6; // take a guess
    else
    {
        for (log_blksize = 1;
             (((unsigned) 1 << log_blksize) < estimate) &&
                 (log_blksize < 31);
             log_blksize++)
            ;
    }

    //
    // Increment a base_increment size that is big enough not to have to
    // be reallocated. Find a block size that is smaller that the block
    // size of the pool.
    //
    base_increment = (Blksize() > pool -> Blksize()
                      ? Blksize() / pool -> Blksize() : 1) * 2;
    while (Blksize() >= pool -> Blksize())
        log_blksize--;

    base_size = 0;
    size = 0;
    top = 0;
    base = NULL;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // ast_INCLUDED

