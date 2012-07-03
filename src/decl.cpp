// $Id: decl.cpp,v 1.32 1999/10/09 15:38:31 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "config.h"
#include <sys/stat.h>
#include "semantic.h"
#include "control.h"
#include "depend.h"
#include "table.h"
#include "tuple.h"

//
// If this compilation unit contains a package declaration, make sure the package
// is associated with a directory and that the name of the package is not also
// associated with a type.
//
inline void Semantic::CheckPackage()
{
    if (compilation_unit -> package_declaration_opt)
    {
        //
        // Make sure that the package actually exists.
        //
        if (this_package -> directory.Length() == 0 && control.option.directory == NULL)
        {
            ReportSemError(SemanticError::PACKAGE_NOT_FOUND,
                           compilation_unit -> package_declaration_opt -> name -> LeftToken(),
                           compilation_unit -> package_declaration_opt -> name -> RightToken(),
                           this_package -> PackageName());
        }
        else
        {
            //
            // Make sure that the package or any of its parents does not match the name of a type.
            //
            for (PackageSymbol *subpackage = this_package, *package = subpackage -> owner;
                 package;
                 subpackage = package, package = package -> owner)
            {
                FileSymbol *file_symbol = Control::GetFile(control, package, subpackage -> Identity());
                if (file_symbol)
                {
                    char *file_name = file_symbol -> FileName();
                    int length = file_symbol -> FileNameLength();
                    wchar_t *error_name = new wchar_t[length + 1];
                    for (int i = 0; i < length; i++)
                        error_name[i] = file_name[i];
                    error_name[length] = U_NULL;

                    ReportSemError(SemanticError::PACKAGE_TYPE_CONFLICT,
                                   compilation_unit -> package_declaration_opt -> name -> LeftToken(),
                                   compilation_unit -> package_declaration_opt -> name -> RightToken(),
                                   package -> PackageName(),
                                   subpackage -> Name(),
                                   error_name);

                    delete [] error_name;
                }
            }
        }
    }

    return;
}


//
// Pass 1: Introduce the main package, the current package and all types specified into their proper scope
//
void Semantic::ProcessTypeNames()
{
    import_on_demand_packages.Next() = control.system_package;
    compilation_unit = source_file_symbol -> compilation_unit;

    //
    // If we are supposed to be verbose, report empty declarations...
    //
    if (control.option.pedantic)
    {
        if (compilation_unit -> EmptyCompilationUnitCast())
        {
            ReportSemError(SemanticError::NO_TYPES,
                           compilation_unit -> LeftToken(),
                           compilation_unit -> RightToken());
        }

        for (int i = 0; i < compilation_unit -> NumTypeDeclarations(); i++)
        {
            Ast *type_declaration = compilation_unit -> TypeDeclaration(i);
            if (type_declaration -> EmptyDeclarationCast())
            {
                ReportSemError(SemanticError::EMPTY_DECLARATION,
                               type_declaration -> LeftToken(),
                               type_declaration -> RightToken());
            }
        }
    }

    //
    // If we have a bad compilation unit insert its types as "bad types"
    //
    if (compilation_unit -> BadCompilationUnitCast())
    {
        for (int i = 0; i < lex_stream -> NumTypes(); i++)
        {
            LexStream::TokenIndex identifier_token = lex_stream -> Next(lex_stream -> Type(i));
            if (lex_stream -> Kind(identifier_token) == TK_Identifier)
            {
                NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);
                TypeSymbol *type = this_package -> FindTypeSymbol(name_symbol);

                assert(type);

                type -> MarkBad();
                type -> MarkSourceNoLongerPending();
                type -> supertypes_closure = new SymbolSet;
                type -> subtypes = new SymbolSet;
                type -> semantic_environment = new SemanticEnvironment((Semantic *) this, type, NULL);
                if (type != control.Object())
                    type -> super = (type == control.Throwable() ? control.Object() : control.Throwable());
                if (! type -> FindConstructorSymbol())
                    AddDefaultConstructor(type);
                source_file_symbol -> types.Next() = type;
            }
        }

        return;
    }

    //
    // Use this tuple to compute the list of valid types encountered in this
    // compilation unit.
    //
    Tuple<TypeSymbol *> type_list;

    //
    // Process each type in this compilation unit, in turn
    //
    for (int k = 0; k < compilation_unit -> NumTypeDeclarations(); k++)
    {
        LexStream::TokenIndex identifier_token;
        TypeSymbol *type = NULL;

        Ast *type_declaration = compilation_unit -> TypeDeclaration(k);
        switch(type_declaration -> kind)
        {
            case Ast::CLASS:
            {
                AstClassDeclaration *class_declaration = (AstClassDeclaration *) type_declaration;
                identifier_token = class_declaration -> identifier_token;
                NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);

                type = this_package -> FindTypeSymbol(name_symbol);
                if (type)
                {
                    if (! type -> SourcePending())
                    {
                        ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                                       identifier_token,
                                       identifier_token,
                                       name_symbol -> Name(),
                                       type -> FileLoc());
                        type = NULL;
                    }
                    else
                    {
                        if (type -> ContainingPackage() == control.unnamed_package)
                        {
                            TypeSymbol *old_type = (TypeSymbol *) control.unnamed_package_types.Image(name_symbol);
                            if (old_type != type)
                            {
                                ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                                               identifier_token,
                                               identifier_token,
                                               name_symbol -> Name(),
                                               old_type -> FileLoc());
                            }
                        }

                        type_list.Next() = type; // Save valid type for later processing. See below

                        type -> MarkSourceNoLongerPending();
                        type -> semantic_environment = new SemanticEnvironment((Semantic *) this, type, NULL);
                        type -> declaration = class_declaration;
                        type -> SetFlags(ProcessClassModifiers(class_declaration));
                        //
                        // Add 3 extra elements for padding. May need a default constructor and other support elements.
                        //
                        type -> SetSymbolTable(class_declaration -> class_body -> NumClassBodyDeclarations() + 3);
                        type -> SetLocation();

                        if (lex_stream -> IsDeprecated(lex_stream -> Previous(class_declaration -> LeftToken())))
                            type -> MarkDeprecated();

                        source_file_symbol -> types.Next() = type;
                        class_declaration -> semantic_environment = type -> semantic_environment; // save for processing bodies later.

                        CheckClassMembers(type, class_declaration -> class_body);
                    }
                }

                break;
            }
            case Ast::INTERFACE:
            {
                AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) type_declaration;
                identifier_token = interface_declaration -> identifier_token;
                NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);

                type = this_package -> FindTypeSymbol(name_symbol);
                if (type)
                {
                    if (! type -> SourcePending())
                    {
                        ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                                       identifier_token,
                                       identifier_token,
                                       name_symbol -> Name(),
                                       type -> FileLoc());
                        type = NULL;
                    }
                    else
                    {
                        if (type -> ContainingPackage() == control.unnamed_package)
                        {
                            TypeSymbol *old_type = (TypeSymbol *) control.unnamed_package_types.Image(name_symbol);
                            if (old_type != type)
                            {
                                ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                                               identifier_token,
                                               identifier_token,
                                               name_symbol -> Name(),
                                               old_type -> FileLoc());
                            }
                        }

                        type_list.Next() = type; // Save valid type for later processing. See below

                        type -> MarkSourceNoLongerPending();
                        type -> semantic_environment = new SemanticEnvironment((Semantic *) this, type, NULL);
                        type -> declaration = interface_declaration;
                        type -> file_symbol = source_file_symbol;
                        type -> SetFlags(ProcessInterfaceModifiers(interface_declaration));
                        type -> SetSymbolTable(interface_declaration -> NumInterfaceMemberDeclarations());
                        type -> SetLocation();

                        if (lex_stream -> IsDeprecated(lex_stream -> Previous(interface_declaration -> LeftToken())))
                            type -> MarkDeprecated();

                        source_file_symbol -> types.Next() = type;
                        interface_declaration -> semantic_environment = type -> semantic_environment;

                        CheckInterfaceMembers(type, interface_declaration);
                    }
                }
                break;
            }
        }

        //
        // If we successfully processed this type, check that
        //     . its name does not conflict with a subpackage
        //     . if it is contained in a file with a diffent name
        //       than its own name that there does not also exist a
        //       (java or class) file with its name.
        //
        if (type)
        {
            NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);
            for (int i = 0; i < this_package -> directory.Length(); i++)
            {
                if (this_package -> directory[i] -> FindDirectorySymbol(name_symbol))
                {
                    char *file_name = type -> file_symbol -> FileName();
                    int length = type -> file_symbol -> FileNameLength();
                    wchar_t *error_name = new wchar_t[length + 1];
                    for (int j = 0; j < length; j++)
                        error_name[j] = file_name[j];
                    error_name[length] = U_NULL;

                    ReportSemError(SemanticError::PACKAGE_TYPE_CONFLICT,
                                   identifier_token,
                                   identifier_token,
                                   this_package -> PackageName(),
                                   name_symbol -> Name(),
                                   error_name);

                    delete [] error_name;
                }
            }

            if (type -> Identity() != source_file_symbol -> Identity())
            {
                PackageSymbol *package = this_package;
                FileSymbol *file_symbol = Control::GetJavaFile(package, type -> Identity());

                if (file_symbol)
                {
                    ReportSemError(SemanticError::TYPE_IN_MULTIPLE_FILES,
                                   identifier_token,
                                   identifier_token,
                                   this_package -> PackageName(),
                                   source_file_symbol -> Name(),
                                   package -> PackageName(),
                                   type -> Name());
                }
            }
        }
    }

    CheckPackage();
    ProcessImports();

    //
    // Process outer type of superclasses and interfaces and make sure that compilation unit
    // contains exactly one public type.
    //
    TypeSymbol *public_type = NULL;
    for (int i = 0; i < type_list.Length(); i++)
    {
        TypeSymbol *type = type_list[i];

        AstClassDeclaration *class_declaration = type -> declaration -> ClassDeclarationCast();
        AstInterfaceDeclaration *interface_declaration = type -> declaration -> InterfaceDeclarationCast();
        if (class_declaration)
             ProcessSuperTypeDependences(class_declaration);
        else ProcessSuperTypeDependences(interface_declaration);

        if (type && type -> ACC_PUBLIC())
        {
            if (! public_type)
            {
                public_type = type;

                if  (source_file_symbol -> Identity() != public_type -> Identity())
                {
                    if (class_declaration)
                    {
                        ReportSemError(SemanticError::MISMATCHED_TYPE_AND_FILE_NAMES,
                                       class_declaration -> identifier_token,
                                       class_declaration -> identifier_token,
                                       public_type -> Name());
                    }
                    else
                    {
                        ReportSemError(SemanticError::MISMATCHED_TYPE_AND_FILE_NAMES,
                                       interface_declaration -> identifier_token,
                                       interface_declaration -> identifier_token,
                                       public_type -> Name());
                    }
                }
            }
            else
            {
                if (class_declaration)
                {
                    ReportSemError(SemanticError::MULTIPLE_PUBLIC_TYPES,
                                   class_declaration -> identifier_token,
                                   class_declaration -> identifier_token,
                                   type -> Name(),
                                   public_type -> Name());
                }
                else
                {
                    ReportSemError(SemanticError::MULTIPLE_PUBLIC_TYPES,
                                   interface_declaration -> identifier_token,
                                   interface_declaration -> identifier_token,
                                   type -> Name(),
                                   public_type -> Name());
                }
            }
        }
    }

    return;
}


void Semantic::CheckClassMembers(TypeSymbol *containing_type, AstClassBody *class_body)
{
    for (int i = 0; i < class_body -> NumNestedClasses(); i++)
    {
        AstClassDeclaration *class_declaration = class_body -> NestedClass(i);

        if (! control.option.one_one)
        {
             ReportSemError(SemanticError::ONE_ONE_FEATURE,
                            class_declaration -> LeftToken(),
                            class_declaration -> RightToken());
        }

        ProcessNestedClassName(containing_type, class_declaration);
    }

    for (int j = 0; j < class_body -> NumNestedInterfaces(); j++)
    {
        AstInterfaceDeclaration *interface_declaration = class_body -> NestedInterface(j);

        if (! control.option.one_one)
        {
            ReportSemError(SemanticError::ONE_ONE_FEATURE,
                           interface_declaration -> LeftToken(),
                           interface_declaration -> RightToken());
        }

        ProcessNestedInterfaceName(containing_type, interface_declaration);
    }

    for (int k = 0; k < class_body -> NumBlocks(); k++)
    {
        if (! control.option.one_one)
        {
            ReportSemError(SemanticError::ONE_ONE_FEATURE,
                           class_body -> Block(k) -> LeftToken(),
                           class_body -> Block(k) -> RightToken());
        }
    }

    for (int l = 0; l < class_body -> NumEmptyDeclarations(); l++)
    {
        if (control.option.pedantic)
        {
            ReportSemError(SemanticError::EMPTY_DECLARATION,
                           class_body -> EmptyDeclaration(l) -> LeftToken(),
                           class_body -> EmptyDeclaration(l) -> RightToken());
        }
    }

    return;
}


inline TypeSymbol *Semantic::FindTypeInShadow(TypeShadowSymbol *type_shadow_symbol, LexStream::TokenIndex identifier_token)
{
    //
    // Recall that even an inaccessible member x of a super class (or interface) S,
    // in addition to not been inherited by a subclass, hides all other occurrences of x that may
    // appear in a super class (or super interface) of S (see 8.3).
    //
    TypeSymbol *type_symbol = type_shadow_symbol -> type_symbol;

    for (int i = 0; i < type_shadow_symbol -> NumConflicts(); i++)
    {
        ReportSemError(SemanticError::AMBIGUOUS_TYPE,
                       identifier_token,
                       identifier_token,
                       type_symbol -> Name(),
                       type_symbol -> owner -> TypeCast() -> ContainingPackage() -> PackageName(),
                       type_symbol -> owner -> TypeCast() -> ExternalName(),
                       type_shadow_symbol -> Conflict(i) -> owner -> TypeCast() -> ContainingPackage() -> PackageName(),
                       type_shadow_symbol -> Conflict(i) -> owner -> TypeCast() -> ExternalName());
    }

    return type_symbol;
}


//
// Look for a type within an environment stack, without regard to inheritance !!!
//
TypeSymbol *Semantic::FindTypeInEnvironment(SemanticEnvironment *env_stack, NameSymbol *name_symbol)
{
    for (SemanticEnvironment *env = env_stack; env; env = env -> previous)
    {
        TypeSymbol *type = env -> symbol_table.FindTypeSymbol(name_symbol);
        if (type)
            return type;
        type = env -> Type() -> FindTypeSymbol(name_symbol);
        if (type)
            return type;
        if (name_symbol == env -> Type() -> Identity())
            return env -> Type();
    }

    return (TypeSymbol *) NULL;
}


void Semantic::CheckNestedTypeDuplication(SemanticEnvironment *env, LexStream::TokenIndex identifier_token)
{
    NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);

    //
    // First check to see if we have a duplication at the same level...
    //
    TypeSymbol *old_type = (env -> symbol_table.Size() > 0 ? env -> symbol_table.Top() -> FindTypeSymbol(name_symbol)
                                                           : env -> Type() -> FindTypeSymbol(name_symbol));
    if (old_type)
    {
        ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                       identifier_token,
                       identifier_token,
                       name_symbol -> Name(),
                       old_type -> FileLoc());
    }
    else
    {
        //
        // ... Then check the enclosing environments...
        //
        for (; env; env = env -> previous)
        {
            if (env -> Type() -> Identity() == name_symbol)
            {
                ReportSemError(SemanticError::DUPLICATE_INNER_TYPE_NAME,
                               identifier_token,
                               identifier_token,
                               name_symbol -> Name(),
                               env -> Type() -> FileLoc());
                break;
            }
        }
    }

    return;
}


TypeSymbol *Semantic::ProcessNestedClassName(TypeSymbol *containing_type, AstClassDeclaration *class_declaration)
{
    CheckNestedTypeDuplication(containing_type -> semantic_environment, class_declaration -> identifier_token);

    NameSymbol *name_symbol = lex_stream -> NameSymbol(class_declaration -> identifier_token);
    TypeSymbol *outermost_type = containing_type -> outermost_type;

    int length = containing_type -> ExternalNameLength() + 1 + name_symbol -> NameLength(); // +1 for $,... +1 for $
    wchar_t *external_name = new wchar_t[length + 1]; // +1 for '\0';
    wcscpy(external_name, containing_type -> ExternalName());
    wcscat(external_name, StringConstant::US__DS);
    wcscat(external_name, name_symbol -> Name());

    TypeSymbol *inner_type = containing_type -> InsertNestedTypeSymbol(name_symbol);
    inner_type -> outermost_type = outermost_type;
    inner_type -> supertypes_closure = new SymbolSet;
    inner_type -> subtypes = new SymbolSet;
    inner_type -> SetExternalIdentity(control.FindOrInsertName(external_name, length));
    inner_type -> semantic_environment = new SemanticEnvironment((Semantic *) this,
                                                                 inner_type,
                                                                 containing_type -> semantic_environment);
    inner_type -> declaration = class_declaration;
    inner_type -> file_symbol = source_file_symbol;
    inner_type -> SetFlags(containing_type -> ACC_INTERFACE()
                                            ? ProcessStaticNestedClassModifiers(class_declaration)
                                            : ProcessNestedClassModifiers(class_declaration));
    inner_type -> SetOwner(containing_type);
    //
    // Add 3 extra elements for padding. May need a default constructor and other support elements.
    //
    inner_type -> SetSymbolTable(class_declaration -> class_body -> NumClassBodyDeclarations() + 3);
    inner_type -> SetLocation();
    inner_type -> SetSignature(control);

    if (lex_stream -> IsDeprecated(lex_stream -> Previous(class_declaration -> LeftToken())))
        inner_type -> MarkDeprecated();

    //
    // If not a top-level type, then add pointer to enclosing type.
    //
    if (! inner_type -> ACC_STATIC())
        inner_type -> InsertThis(0);

    if (inner_type -> IsLocal())
    {
        if (! outermost_type -> local)
            outermost_type -> local = new SymbolSet;
        outermost_type -> local -> AddElement(inner_type);
    }
    else
    {
        if (! outermost_type -> non_local)
            outermost_type -> non_local = new SymbolSet;
        outermost_type -> non_local -> AddElement(inner_type);
    }

    class_declaration -> semantic_environment = inner_type -> semantic_environment; // save for processing bodies later.

    CheckClassMembers(inner_type, class_declaration -> class_body);

    delete [] external_name;

    return inner_type;
}


void Semantic::CheckInterfaceMembers(TypeSymbol *containing_type, AstInterfaceDeclaration *interface_declaration)
{
    for (int i = 0; i < interface_declaration -> NumNestedClasses(); i++)
    {
        AstClassDeclaration *class_declaration = interface_declaration -> NestedClass(i);

        if (! control.option.one_one)
        {
             ReportSemError(SemanticError::ONE_ONE_FEATURE,
                            class_declaration -> LeftToken(),
                            class_declaration -> RightToken());
        }

        ProcessNestedClassName(containing_type, class_declaration);
    }

    for (int j = 0; j < interface_declaration -> NumNestedInterfaces(); j++)
    {
        AstInterfaceDeclaration *inner_interface_declaration = interface_declaration -> NestedInterface(j);

        if (! control.option.one_one)
        {
            ReportSemError(SemanticError::ONE_ONE_FEATURE,
                           inner_interface_declaration -> LeftToken(),
                           inner_interface_declaration -> RightToken());
        }

        ProcessNestedInterfaceName(containing_type, inner_interface_declaration);
    }

    for (int l = 0; l < interface_declaration -> NumEmptyDeclarations(); l++)
    {
        if (control.option.pedantic)
        {
            ReportSemError(SemanticError::EMPTY_DECLARATION,
                           interface_declaration -> EmptyDeclaration(l) -> LeftToken(),
                           interface_declaration -> EmptyDeclaration(l) -> RightToken());
        }
    }

    return;
}


TypeSymbol *Semantic::ProcessNestedInterfaceName(TypeSymbol *containing_type, AstInterfaceDeclaration *interface_declaration)
{
    CheckNestedTypeDuplication(containing_type -> semantic_environment, interface_declaration -> identifier_token);

    NameSymbol *name_symbol = lex_stream -> NameSymbol(interface_declaration -> identifier_token);
    TypeSymbol *outermost_type = containing_type -> outermost_type;

    int length = containing_type -> ExternalNameLength() + 1 + name_symbol -> NameLength(); // +1 for $,... +1 for $
    wchar_t *external_name = new wchar_t[length + 1]; // +1 for '\0';
    wcscpy(external_name, containing_type -> ExternalName());
    wcscat(external_name, StringConstant::US__DS);
    wcscat(external_name, name_symbol -> Name());

    TypeSymbol *inner_type = containing_type -> InsertNestedTypeSymbol(name_symbol);
    inner_type -> outermost_type = outermost_type;
    inner_type -> supertypes_closure = new SymbolSet;
    inner_type -> subtypes = new SymbolSet;
    inner_type -> SetExternalIdentity(control.FindOrInsertName(external_name, length));
    inner_type -> semantic_environment = new SemanticEnvironment((Semantic *) this,
                                                                 inner_type,
                                                                 containing_type -> semantic_environment);
    inner_type -> declaration = interface_declaration;
    inner_type -> file_symbol = source_file_symbol;
    inner_type -> SetFlags(ProcessNestedInterfaceModifiers(interface_declaration));
    inner_type -> SetOwner(containing_type);
    inner_type -> SetSymbolTable(interface_declaration -> NumInterfaceMemberDeclarations());
    inner_type -> SetLocation();
    inner_type -> SetSignature(control);

    if (lex_stream -> IsDeprecated(lex_stream -> Previous(interface_declaration -> LeftToken())))
        inner_type -> MarkDeprecated();

    if (inner_type -> IsLocal())
    {
        if (! outermost_type -> local)
            outermost_type -> local = new SymbolSet;
        outermost_type -> local -> AddElement(inner_type);
    }
    else
    {
        if (! outermost_type -> non_local)
            outermost_type -> non_local = new SymbolSet;
        outermost_type -> non_local -> AddElement(inner_type);
    }

    interface_declaration -> semantic_environment = inner_type -> semantic_environment; // save for processing bodies later.

    CheckInterfaceMembers(inner_type, interface_declaration);

    delete [] external_name;

    return inner_type;
}


//
// Pass 1.2: Process all import statements
//
void Semantic::ProcessImports()
{
    for (int i = 0; i < compilation_unit -> NumImportDeclarations(); i++)
    {
        AstImportDeclaration *import_declaration = compilation_unit -> ImportDeclaration(i);

        if (import_declaration -> star_token_opt)
             ProcessTypeImportOnDemandDeclaration(import_declaration);
        else ProcessSingleTypeImportDeclaration(import_declaration);
    }

    return;
}


//
// Pass 1.3: Process outer types in "extends" and "implements" clauses associated with the types.
//
void Semantic::ProcessSuperTypeDependences(AstClassDeclaration *class_declaration)
{
    TypeSymbol *type = class_declaration -> semantic_environment -> Type();
    if (class_declaration -> super_opt)
    {
        TypeSymbol *super_type = FindFirstType(class_declaration -> super_opt) -> symbol -> TypeCast();
        if (super_type)
            super_type -> subtypes -> AddElement(type -> outermost_type);
    }

    for (int k = 0; k < class_declaration -> NumInterfaces(); k++)
    {
         TypeSymbol *super_type = FindFirstType(class_declaration -> Interface(k)) -> symbol -> TypeCast();
         if (super_type)
         {
             assert(super_type -> subtypes);

             super_type -> subtypes -> AddElement(type -> outermost_type);
         }
    }

    SetDefaultSuperType(class_declaration);

    AstClassBody *class_body = class_declaration -> class_body;
    for (int i = 0; i < class_body -> NumNestedClasses(); i++)
        ProcessSuperTypeDependences(class_body -> NestedClass(i));

    for (int j = 0; j < class_body -> NumNestedInterfaces(); j++)
        ProcessSuperTypeDependences(class_body -> NestedInterface(j));

    return;
}


void Semantic::ProcessSuperTypeDependences(AstInterfaceDeclaration *interface_declaration)
{
    TypeSymbol *type = interface_declaration -> semantic_environment -> Type();
    for (int k = 0; k < interface_declaration -> NumExtendsInterfaces(); k++)
    {
        TypeSymbol *super_type = FindFirstType(interface_declaration -> ExtendsInterface(k)) -> symbol -> TypeCast();
        if (super_type)
            super_type -> subtypes -> AddElement(type -> outermost_type);
    }

    SetDefaultSuperType(interface_declaration);

    for (int i = 0; i < interface_declaration -> NumNestedClasses(); i++)
        ProcessSuperTypeDependences(interface_declaration -> NestedClass(i));

    for (int j = 0; j < interface_declaration -> NumNestedInterfaces(); j++)
        ProcessSuperTypeDependences(interface_declaration -> NestedInterface(j));

    return;
}


void Semantic::SetDefaultSuperType(AstClassDeclaration *class_declaration)
{
    TypeSymbol *type = class_declaration -> semantic_environment -> Type();

    //
    // If a type has no super type, set it up properly in case
    // it is expanded prematurely by one of its dependents.
    //
    if (! class_declaration -> super_opt && class_declaration -> NumInterfaces() == 0)
    {
        if (type -> Identity() != control.object_name_symbol ||
            type -> ContainingPackage() != control.system_package || type -> IsNested())
            type -> super = control.Object();
    }

    return;
}


void Semantic::SetDefaultSuperType(AstInterfaceDeclaration *interface_declaration)
{
    TypeSymbol *type = interface_declaration -> semantic_environment -> Type();

    //
    // Set it up an interface properly in case it is
    // expanded prematurely by one of its dependents.
    //
    type -> super = control.Object();

    for (int i = 0; i < interface_declaration -> NumNestedClasses(); i++)
    {
        AstClassDeclaration *inner_class_declaration = interface_declaration -> NestedClass(i);
        if (inner_class_declaration -> semantic_environment)
            SetDefaultSuperType(inner_class_declaration);
    }

    for (int j = 0; j < interface_declaration -> NumNestedInterfaces(); j++)
    {
        AstInterfaceDeclaration *inner_interface_declaration = interface_declaration -> NestedInterface(j);
        if (inner_interface_declaration -> semantic_environment)
            SetDefaultSuperType(inner_interface_declaration);
    }

    return;
}


//
// Pass 2: Process "extends" and "implements" clauses associated with the types.
//
void Semantic::ProcessTypeHeader(AstClassDeclaration *class_declaration)
{
    TypeSymbol *this_type = class_declaration -> semantic_environment -> Type();

    assert(! this_type -> HeaderProcessed() || this_type -> Bad());

    //
    // If the class does not have a super type then ...
    //
    if (! class_declaration -> super_opt)
    {
        if (this_type -> Identity() != control.object_name_symbol ||
            this_package != control.system_package || this_type -> IsNested())
            SetObjectSuperType(this_type, class_declaration -> identifier_token);
    }
    else
    {
        TypeSymbol *super_type = MustFindType(class_declaration -> super_opt);

        assert(this_type -> subtypes_closure);
        assert(! super_type -> SourcePending());

        this_type -> super = super_type;

        if (this_type -> subtypes_closure -> IsElement(super_type)) // if there is a cycle, break it and issue an error message
        {
            this_type -> super = control.Object();
            this_type -> MarkCircular();
            ReportSemError(SemanticError::CIRCULAR_CLASS,
                           class_declaration -> identifier_token,
                           class_declaration -> super_opt -> RightToken(),
                           this_type -> ContainingPackage() -> PackageName(),
                           this_type -> ExternalName());
        }
        else if (this_type -> Identity() == control.object_name_symbol &&
                 this_package == control.system_package && (! this_type -> IsNested()))
        {
             ReportSemError(SemanticError::OBJECT_WITH_SUPER_TYPE,
                            class_declaration -> super_opt -> LeftToken(),
                            class_declaration -> super_opt -> RightToken(),
                            this_type -> ContainingPackage() -> PackageName(),
                            this_type -> ExternalName());
             this_type -> super = NULL;
        }
        else if (this_type -> super -> ACC_INTERFACE())
        {
            ReportSemError(SemanticError::NOT_A_CLASS,
                           class_declaration -> super_opt -> LeftToken(),
                           class_declaration -> super_opt -> RightToken(),
                           this_type -> super -> ContainingPackage() -> PackageName(),
                           this_type -> super -> ExternalName());

            SetObjectSuperType(this_type, class_declaration -> identifier_token);
        }
        else if (this_type -> super -> ACC_FINAL())
        {
             ReportSemError(SemanticError::SUPER_IS_FINAL,
                            class_declaration -> super_opt -> LeftToken(),
                            class_declaration -> super_opt -> RightToken(),
                            this_type -> super -> ContainingPackage() -> PackageName(),
                            this_type -> super -> ExternalName());
        }
    }

    for (int i = 0; i < class_declaration -> NumInterfaces(); i++)
        ProcessInterface(this_type, class_declaration -> Interface(i));

    this_type -> MarkHeaderProcessed();

    return;
}


void Semantic::ProcessTypeHeader(AstInterfaceDeclaration *interface_declaration)
{
    TypeSymbol *this_type = interface_declaration -> semantic_environment -> Type();

    assert(! this_type -> HeaderProcessed() || this_type -> Bad());

    SetObjectSuperType(this_type, interface_declaration -> identifier_token);
    for (int k = 0; k < interface_declaration -> NumExtendsInterfaces(); k++)
        ProcessInterface(this_type, interface_declaration -> ExtendsInterface(k));

    assert(this_type -> subtypes_closure);

    for (int i = 0; i < this_type -> NumInterfaces(); i++)
    {
        if (this_type -> subtypes_closure -> IsElement(this_type -> Interface(i)))
        {
            this_type -> ResetInterfaces(); // Remove all the interfaces if a loop is detected. The error will be reported later
            this_type -> MarkCircular();
            ReportSemError(SemanticError::CIRCULAR_INTERFACE,
                           interface_declaration -> identifier_token,
                           interface_declaration -> ExtendsInterface(interface_declaration -> NumExtendsInterfaces() - 1) -> RightToken(),
                           this_type -> ContainingPackage() -> PackageName(),
                           this_type -> ExternalName());
            break;
        }
    }

    this_type -> MarkHeaderProcessed();

    return;
}


//
// Marked type and all other types that are nested inside it "circular"
//
void Semantic::MarkCircularNest(TypeSymbol *type)
{
    if (type -> Circular())
        return;

    //
    // Mark the type as circular
    //
    type -> MarkCircular();
    type -> super = control.Object();
    type -> ResetInterfaces();

    //
    // Recursively, process any nested type...
    //
    AstClassDeclaration *class_declaration = type -> declaration -> ClassDeclarationCast();
    if (class_declaration)
    {
        AstClassBody *class_body = class_declaration -> class_body;
        for (int i = 0; i < class_body -> NumNestedClasses(); i++)
            MarkCircularNest(class_body -> NestedClass(i) -> semantic_environment -> Type());
        for (int k = 0; k < class_body -> NumNestedInterfaces(); k++)
            MarkCircularNest(class_body -> NestedInterface(k) -> semantic_environment -> Type());
    }
    else
    {
        AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) type -> declaration;
        for (int i = 0; i < interface_declaration -> NumNestedClasses(); i++)
            MarkCircularNest(interface_declaration -> NestedClass(i) -> semantic_environment -> Type());
        for (int k = 0; k < interface_declaration -> NumNestedInterfaces(); k++)
            MarkCircularNest(interface_declaration -> NestedInterface(k) -> semantic_environment -> Type());
    }

    return;
}


//
// Compute the set of super types associated with this outer-level type
// and check for circularity.
//
void Semantic::ProcessSuperTypesOfOuterType(TypeSymbol *type)
{
    assert((! type -> IsNested()) || type -> owner -> MethodCast());

    if (type -> super)
    {
        type -> supertypes_closure -> AddElement(type -> super -> outermost_type);
        type -> supertypes_closure -> Union(*type -> super -> outermost_type -> supertypes_closure);
    }

    for (int k = 0; k < type -> NumInterfaces(); k++)
    {
        type -> supertypes_closure -> AddElement(type -> Interface(k) -> outermost_type);
        type -> supertypes_closure -> Union(*type -> Interface(k) -> outermost_type -> supertypes_closure);
    }

    SymbolSet &inner_types = *(type -> innertypes_closure);
    for (TypeSymbol *inner_type = (TypeSymbol *) inner_types.FirstElement();
                     inner_type;
                     inner_type = (TypeSymbol *) inner_types.NextElement())
    {
        TypeSymbol *super_type = inner_type -> super;
        for (int k = 0; super_type;
                        super_type = (TypeSymbol *) (k < inner_type -> NumInterfaces() ? inner_type -> Interface(k++) : NULL))
        {
            if (super_type -> outermost_type != type)
            {
                type -> supertypes_closure -> AddElement(super_type -> outermost_type);
                type -> supertypes_closure -> Union(*super_type -> outermost_type -> supertypes_closure);
            }
        }
    }

    bool circular = type -> supertypes_closure -> IsElement(type) ||
                    type -> subtypes_closure -> Intersects(*type -> supertypes_closure);
    if (circular)
    {
        if (type -> Circular())        // If the type is already marked circular, an error message has already been issued
            type -> MarkNonCircular(); // Remove the circular mark, so that we can remark the whole "nest" ?
        else
        {
            if (type -> ACC_INTERFACE())
            {
                AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) type -> declaration;
                int right_token_index = interface_declaration -> NumExtendsInterfaces() - 1;

                ReportSemError(SemanticError::CIRCULAR_INTERFACE,
                               interface_declaration -> identifier_token,
                               (interface_declaration -> NumExtendsInterfaces() > 0
                                                       ? interface_declaration -> ExtendsInterface(right_token_index) -> RightToken()
                                                       : interface_declaration -> identifier_token),
                               type -> ContainingPackage() -> PackageName(),
                               type -> ExternalName());
            }
            else
            {
                AstClassDeclaration *class_declaration = (AstClassDeclaration *) type -> declaration;
                int right_token_index = class_declaration -> NumInterfaces() - 1;

                ReportSemError(SemanticError::CIRCULAR_CLASS,
                               class_declaration -> identifier_token,
                               (class_declaration -> NumInterfaces() > 0
                                                   ? class_declaration -> Interface(right_token_index) -> RightToken()
                                                   : (class_declaration -> super_opt
                                                                         ? class_declaration -> super_opt -> RightToken()
                                                                         : class_declaration -> identifier_token)),
                               type -> ContainingPackage() -> PackageName(),
                               type -> ExternalName());

                SetObjectSuperType(type, class_declaration -> identifier_token);

                assert(type -> Identity() != control.object_name_symbol || type -> ContainingPackage() != control.system_package);
            }
        }

        MarkCircularNest(type);
    }

    return;
}


//
// The array partially_ordered_types contains a list of inner types. For
// each of these types, compute the set of super types associated with it
// and check for circularity.
//
void Semantic::ProcessSuperTypesOfInnerType(TypeSymbol *type, Tuple<TypeSymbol *> &partially_ordered_types)
{
    for (int l = 0; l < partially_ordered_types.Length(); l++)
    {
        TypeSymbol *inner_type = partially_ordered_types[l];

        SymbolSet &nested_types = *(inner_type -> innertypes_closure);
        nested_types.AddElement(inner_type); // Compute reflexive transitive closure
        for (TypeSymbol *nested_type = (TypeSymbol *) nested_types.FirstElement();
                         nested_type;
                         nested_type = (TypeSymbol *) nested_types.NextElement())
        {
            TypeSymbol *super_type = nested_type -> super;
            for (int k = 0; super_type;
                            super_type = (TypeSymbol *) (k < nested_type -> NumInterfaces() ? nested_type -> Interface(k++) : NULL))
            {
                for ( ; super_type; super_type = super_type -> owner -> TypeCast())
                {
                    if (type -> innertypes_closure -> IsElement(super_type))
                        break;
                }

                if (super_type && super_type != inner_type)
                {
                    inner_type -> supertypes_closure -> AddElement(super_type);
                    inner_type -> supertypes_closure -> Union(*super_type -> supertypes_closure);
                }
            }
        }

        bool circular = inner_type -> supertypes_closure -> IsElement(inner_type) ||
                        inner_type -> subtypes_closure -> Intersects(*inner_type -> supertypes_closure);

        if (circular)
        {
            MarkCircularNest(inner_type);

            if (inner_type -> ACC_INTERFACE())
            {
                AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) inner_type -> declaration;
                ReportSemError(SemanticError::CIRCULAR_INTERFACE,
                               interface_declaration -> identifier_token,
                               (interface_declaration -> NumExtendsInterfaces() > 0
                                                       ? interface_declaration -> ExtendsInterface(interface_declaration -> NumExtendsInterfaces() - 1) -> RightToken()
                                                       : interface_declaration -> identifier_token),
                               inner_type -> ContainingPackage() -> PackageName(),
                               inner_type -> ExternalName());
            }
            else
            {
                AstClassDeclaration *class_declaration = (AstClassDeclaration *) inner_type -> declaration;
                ReportSemError(SemanticError::CIRCULAR_CLASS,
                               class_declaration -> identifier_token,
                               (class_declaration -> NumInterfaces() > 0
                                                   ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                                   : (class_declaration -> super_opt
                                                                         ? class_declaration -> super_opt -> RightToken()
                                                                         : class_declaration -> identifier_token)),
                               inner_type -> ContainingPackage() -> PackageName(),
                               inner_type -> ExternalName());
            }
        }
    }

    //
    // At this point the innertypes_closure set contains only the
    // immediate inner types.
    //
    if (partially_ordered_types.Length() > 1) // inner_types set has more than one element?
    {
        SymbolSet &inner_types = *(type -> innertypes_closure);

        assert(partially_ordered_types.Length() == inner_types.Size());

        TopologicalSort *topological_sorter = new TopologicalSort(inner_types, partially_ordered_types);
        topological_sorter -> Sort();
        delete topological_sorter;
    }

    //
    // Now, complete the closure set of inner types.
    //
    for (int i = 0; i < partially_ordered_types.Length(); i++)
    {
        TypeSymbol *inner_type = partially_ordered_types[i];
        type -> AddNestedType(inner_type);
        type -> innertypes_closure -> Union(*(inner_type -> innertypes_closure));
    }

    return;
}


void Semantic::ProcessTypeHeaders(AstClassDeclaration *class_declaration)
{
    TypeSymbol *this_type = class_declaration -> semantic_environment -> Type();

    assert(state_stack.Size() == 0 || this_type -> owner -> MethodCast()); // Not a nested type or a immediately local type.

    ProcessTypeHeader(class_declaration);
    ProcessNestedTypeHeaders(this_type, class_declaration -> class_body);
    ProcessSuperTypesOfOuterType(this_type);

    //
    // Note that if we are processing an outermost type, no environment is set before we
    // invoke ProcessTypeHeader to process its super types. Therefore, the dependence map
    // is not updated with the super type information. In that case, we do so here.
    //
    if (state_stack.Size() == 0)
    {
        if (this_type -> super)
        {
            AddDependence(this_type,
                          this_type -> super,
                          (class_declaration -> super_opt ? class_declaration -> super_opt -> LeftToken()
                                                          : class_declaration -> identifier_token));
        }

        for (int i = 0; i < class_declaration -> NumInterfaces(); i++)
        {
            if (class_declaration -> Interface(i) -> Type())
                AddDependence(this_type,
                              class_declaration -> Interface(i) -> Type(),
                              class_declaration -> Interface(i) -> LeftToken());
        }
    }

    return;
}


void Semantic::ProcessTypeHeaders(AstInterfaceDeclaration *interface_declaration)
{
    TypeSymbol *this_type = interface_declaration -> semantic_environment -> Type();

    assert(state_stack.Size() == 0); // Not a nested type

    ProcessTypeHeader(interface_declaration);
    ProcessNestedTypeHeaders(interface_declaration);
    ProcessSuperTypesOfOuterType(interface_declaration -> semantic_environment -> Type());

    //
    // Note that no environment is set before we invoke ProcessTypeHeader to process the
    // super types of this_type. As a result, the dependence map is not updated with tha
    // information. We do so here.
    //
    for (int i = 0; i < interface_declaration -> NumExtendsInterfaces(); i++)
    {
        if (interface_declaration -> ExtendsInterface(i) -> Type())
            AddDependence(this_type,
                          interface_declaration -> ExtendsInterface(i) -> Type(),
                          interface_declaration -> ExtendsInterface(i) -> LeftToken());
    }

    return;
}


void Semantic::ReportTypeInaccessible(LexStream::TokenIndex left_tok, LexStream::TokenIndex right_tok, TypeSymbol *type)
{
    ReportSemError(SemanticError::TYPE_NOT_ACCESSIBLE,
                   left_tok,
                   right_tok,
                   type -> ContainingPackage() -> PackageName(),
                   type -> ExternalName(),
                   (type -> ACC_PRIVATE() ? StringConstant::US_private : (type -> ACC_PROTECTED() ? StringConstant::US_protected : StringConstant::US_default)));

    return;
}


TypeSymbol *Semantic::FindNestedType(TypeSymbol *type, LexStream::TokenIndex identifier_token)
{
    if (type == control.null_type || type == control.no_type || type -> Primitive())
        return NULL;

    NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);

    if (! type -> expanded_type_table)
        ComputeTypesClosure(type, identifier_token);
    TypeShadowSymbol *type_shadow_symbol = type -> expanded_type_table -> FindTypeShadowSymbol(name_symbol);

    return (type_shadow_symbol ? FindTypeInShadow(type_shadow_symbol, identifier_token)
                               : type -> FindTypeSymbol(name_symbol));
}


TypeSymbol *Semantic::MustFindNestedType(TypeSymbol *type, Ast *name)
{
    AstSimpleName *simple_name = name -> SimpleNameCast();
    LexStream::TokenIndex identifier_token = (simple_name ? simple_name -> identifier_token
                                                          : ((AstFieldAccess *) name) -> identifier_token);

    TypeSymbol *inner_type = FindNestedType(type, identifier_token);
    if (inner_type)
         TypeAccessCheck(name, inner_type);
    else inner_type = GetBadNestedType(type, identifier_token);

    return inner_type;
}


//
// The Ast name is a qualified name (simple name or a field access). The function FindTypeInLayer
// searches for the first subname that is the name of a type contained in the set inner_types.
// If such a type is found, it is returned. Otherwise, the whole qualified name is resolved to
// a symbol that is returned.
//
TypeSymbol *Semantic::FindTypeInLayer(Ast *name, SymbolSet &inner_types)
{
    //
    // Unwind all the field accesses until we get to a base that is a simple name
    //
    Tuple<AstFieldAccess *> field;
    for (AstFieldAccess *field_access = name -> FieldAccessCast(); field_access; field_access = field_access -> base -> FieldAccessCast())
    {
        field.Next() = field_access;
        name = field_access -> base;
    }

    //
    // If the simple_name base is a type that is an element in the inner_types set
    // return it. Otherwise, assume it is a package name...
    //
    AstSimpleName *simple_name = name -> SimpleNameCast();

    assert(simple_name);

    PackageSymbol *package = NULL;
    TypeSymbol *type = FindType(simple_name -> identifier_token);
    if (type)
    {
        if (inner_types.IsElement(type))
            return type;
    }
    else // If the simple_name is not a type, assume it is a package
    {
        NameSymbol *name_symbol = lex_stream -> NameSymbol(simple_name -> identifier_token);
        package = control.external_table.FindPackageSymbol(name_symbol);
        if (! package)
            package = control.external_table.InsertPackageSymbol(name_symbol, NULL);
        control.FindPathsToDirectory(package);
    }

    //
    // We now go through the field access in order until we either encouter a type that is an element of inner_types,
    // in which case, we return the type. Otherwise, we return NULL.
    //
    //
    for (int i = field.Length() - 1; i >= 0; i--)
    {
        AstFieldAccess *field_access = field[i];

        if (type) // The base name is a type that is not contained in the inner_types set?
        {
            type = FindNestedType(type, field_access -> identifier_token); // resolve the next type...
            if (! type)
                break;
            if (inner_types.IsElement(type))
                return type;
        }
        else
        {
            NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
            type = package -> FindTypeSymbol(name_symbol);
            if (! type)
            {
                FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);
                if (file_symbol)
                    type = ReadType(file_symbol, package, name_symbol, field_access -> identifier_token);
            }
            else if (type -> SourcePending())
                 control.ProcessHeaders(type -> file_symbol);

            //
            //
            //
            if (type)
            {
                if (inner_types.IsElement(type))
                    return type;
            }
            else // If the field access was not resolved to a type assume it is a package
            {
                NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
                PackageSymbol *subpackage = package -> FindPackageSymbol(name_symbol);
                if (! subpackage)
                    subpackage = package -> InsertPackageSymbol(name_symbol);
                control.FindPathsToDirectory(subpackage);
                package = subpackage;
            }
        }
    }

    return NULL;
}


void Semantic::ProcessNestedSuperTypes(TypeSymbol *type)
{
    int num_inner_types = type -> innertypes_closure -> Size();

    if (num_inner_types > 0)
    {
        SymbolSet &inner_types = *(type -> innertypes_closure);

        for (TypeSymbol *inner_type = (TypeSymbol *) inner_types.FirstElement();
                         inner_type;
                         inner_type = (TypeSymbol *) inner_types.NextElement())
        {
            if (inner_type -> ACC_INTERFACE())
            {
                AstInterfaceDeclaration *inner_interface_declaration = (AstInterfaceDeclaration *) inner_type -> declaration;

                for (int l = 0; l < inner_interface_declaration -> NumExtendsInterfaces(); l++)
                {
                    AstExpression *interface_name = inner_interface_declaration -> ExtendsInterface(l);
                    TypeSymbol *super_type = FindTypeInLayer(interface_name, inner_types);
                    if (super_type)
                        super_type -> subtypes -> AddElement(inner_type);
                }
            }
            else
            {
                AstClassDeclaration *inner_class_declaration = (AstClassDeclaration *) inner_type -> declaration;

                if (inner_class_declaration -> super_opt)
                {
                    TypeSymbol *super_type = FindTypeInLayer(inner_class_declaration -> super_opt, inner_types);
                    if (super_type)
                        super_type -> subtypes -> AddElement(inner_type);
                }

                for (int l = 0; l < inner_class_declaration -> NumInterfaces(); l++)
                {
                    TypeSymbol *super_type = FindTypeInLayer(inner_class_declaration -> Interface(l), inner_types);
                    if (super_type)
                        super_type -> subtypes -> AddElement(inner_type);
                }
            }
        }

        //
        // Create a partial order or the inner types. If there are cycles,
        // then the order is arbitrary.
        //
        Tuple<TypeSymbol *> partially_ordered_types;

        if (num_inner_types > 0) // inner_types set is not empty?
        {
            TypeCycleChecker *cycle_checker = new TypeCycleChecker(partially_ordered_types);
            cycle_checker -> PartialOrder(inner_types);
            delete cycle_checker;
        }

        for (int k = 0; k < partially_ordered_types.Length(); k++)
        {
            TypeSymbol *inner_type = partially_ordered_types[k];
            if (inner_type -> ACC_INTERFACE())
            {
                AstInterfaceDeclaration *inner_interface_declaration = (AstInterfaceDeclaration *) inner_type -> declaration;
                ProcessTypeHeader(inner_interface_declaration);
                ProcessNestedTypeHeaders(inner_interface_declaration);
            }
            else
            {
                AstClassDeclaration *inner_class_declaration = (AstClassDeclaration *) inner_type -> declaration;
                ProcessTypeHeader(inner_class_declaration);
                ProcessNestedTypeHeaders(inner_class_declaration -> semantic_environment -> Type(),
                                        inner_class_declaration -> class_body);
            }
        }

        ProcessSuperTypesOfInnerType(type, partially_ordered_types);
    }

    return;
}


void Semantic::ProcessNestedTypeHeaders(TypeSymbol *type, AstClassBody *class_body)
{
    if (type -> expanded_type_table && (type -> super != control.Object() || type -> NumInterfaces() > 0))
    {
        delete type -> expanded_type_table;
        type  -> expanded_type_table = NULL;
    }

    if (! type -> expanded_type_table)
        ComputeTypesClosure(type, class_body -> left_brace_token);

    state_stack.Push(type -> semantic_environment);

    type -> innertypes_closure = new SymbolSet;

    for (int i = 0; i < class_body -> NumNestedClasses(); i++)
    {
        if (class_body -> NestedClass(i) -> semantic_environment)
            type -> innertypes_closure -> AddElement(class_body -> NestedClass(i) -> semantic_environment -> Type());
    }

    for (int j = 0; j < class_body -> NumNestedInterfaces(); j++)
    {
        if (class_body -> NestedInterface(j) -> semantic_environment)
            type -> innertypes_closure -> AddElement(class_body -> NestedInterface(j) -> semantic_environment -> Type());
    }

    ProcessNestedSuperTypes(type);

    state_stack.Pop();

    return;
}


void Semantic::ProcessNestedTypeHeaders(AstInterfaceDeclaration *interface_declaration)
{
    TypeSymbol *type = interface_declaration -> semantic_environment -> Type();
    if (type -> expanded_type_table && type -> NumInterfaces() > 0)
    {
        delete type -> expanded_type_table;
        type  -> expanded_type_table = NULL;
    }

    if (! type -> expanded_type_table)
        ComputeTypesClosure(type, interface_declaration -> identifier_token);

    state_stack.Push(interface_declaration -> semantic_environment);

    type -> innertypes_closure = new SymbolSet;

    for (int i = 0; i < interface_declaration -> NumNestedClasses(); i++)
    {
        if (interface_declaration -> NestedClass(i) -> semantic_environment)
            type -> innertypes_closure -> AddElement(interface_declaration -> NestedClass(i) -> semantic_environment -> Type());
    }

    for (int j = 0; j < interface_declaration -> NumNestedInterfaces(); j++)
    {
        if (interface_declaration -> NestedInterface(j) -> semantic_environment)
            type -> innertypes_closure -> AddElement(interface_declaration -> NestedInterface(j) -> semantic_environment -> Type());
    }

    ProcessNestedSuperTypes(type);

    state_stack.Pop();

    return;
}


//
// Pass 3: Process all method and constructor declarations within the compilation unit so that
//         any field initialization enclosed in the compilation unit can invoke any constructor or
//         method within the unit.
//
inline void Semantic::ProcessConstructorMembers(AstClassBody *class_body)
{
    TypeSymbol *this_type = ThisType();

    assert(this_type -> HeaderProcessed());

    //
    // If the class contains no constructor, ...
    //
    if (class_body -> NumConstructors() > 0)
    {
        for (int k = 0; k < class_body -> NumConstructors(); k++)
            ProcessConstructorDeclaration(class_body -> Constructor(k));
    }
    else if (! this_type -> Anonymous())
         AddDefaultConstructor(this_type);

    this_type -> MarkConstructorMembersProcessed();

    return;
}


inline void Semantic::ProcessMethodMembers(AstClassBody *class_body)
{
    assert(ThisType() -> HeaderProcessed());

    for (int k = 0; k < class_body -> NumMethods(); k++)
        ProcessMethodDeclaration(class_body -> Method(k));

    ThisType() -> MarkMethodMembersProcessed();

    return;
}


inline void Semantic::ProcessFieldMembers(AstClassBody *class_body)
{
    assert(ThisType() -> HeaderProcessed());

    for (int i = 0; i < class_body -> NumInstanceVariables(); i++)
        ProcessFieldDeclaration(class_body -> InstanceVariable(i));

    for (int k = 0; k < class_body -> NumClassVariables(); k++)
        ProcessFieldDeclaration(class_body -> ClassVariable(k));

    ThisType() -> MarkFieldMembersProcessed();

    return;
}


void Semantic::ProcessMembers(SemanticEnvironment *environment, AstClassBody *class_body)
{
    //
    //
    //
    state_stack.Push(environment);
    TypeSymbol *this_type = ThisType();

    assert(! this_type -> ConstructorMembersProcessed() || this_type -> Bad());
    assert(! this_type -> MethodMembersProcessed() || this_type -> Bad());
    assert(! this_type -> FieldMembersProcessed() || this_type -> Bad());

    ProcessConstructorMembers(class_body);
    ProcessMethodMembers(class_body);
    ProcessFieldMembers(class_body);

    delete this_type -> innertypes_closure; // save some space !!!
    this_type -> innertypes_closure = NULL;

    if (! this_type -> IsTopLevel())
    {
        for (int i = 0; i < class_body -> NumStaticInitializers(); i++)
        {
             ReportSemError(SemanticError::STATIC_INITIALIZER_IN_INNER_CLASS,
                            class_body -> StaticInitializer(i) -> LeftToken(),
                            class_body -> StaticInitializer(i) -> RightToken(),
                            this_type -> Name(),
                            this_type -> FileLoc());
        }
    }

    for (int i = 0; i < this_type -> NumNestedTypes(); i++)
    {
        TypeSymbol *inner_type = this_type -> NestedType(i);

        if (inner_type -> ACC_INTERFACE())
        {
            AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) inner_type -> declaration;

            ProcessMembers(interface_declaration);

            if (! this_type -> IsTopLevel())
            {
                //
                // TODO: 1.1 assumption
                //
                // As every field in an interface is static, we presume that all interfaces
                // should be treated as static entities
                //
                if (interface_declaration -> semantic_environment)
                {
                    ReportSemError(SemanticError::STATIC_TYPE_IN_INNER_CLASS,
                                   interface_declaration -> identifier_token,
                                   interface_declaration -> identifier_token,
                                   lex_stream -> NameString(interface_declaration -> identifier_token),
                                   this_type -> Name(),
                                   this_type -> FileLoc());
                }
            }
        }
        else
        {
            AstClassDeclaration *class_declaration = (AstClassDeclaration *) inner_type -> declaration;

            ProcessMembers(class_declaration -> semantic_environment, class_declaration -> class_body);

            if (! this_type -> IsTopLevel())
            {
                if (class_declaration -> semantic_environment && class_declaration -> semantic_environment -> Type() -> ACC_STATIC())
                {
                    ReportSemError(SemanticError::STATIC_TYPE_IN_INNER_CLASS,
                                   class_declaration -> identifier_token,
                                   class_declaration -> identifier_token,
                                   lex_stream -> NameString(class_declaration -> identifier_token),
                                   this_type -> Name(),
                                   this_type -> FileLoc());
                }
            }
        }
    }

    state_stack.Pop();

    return;
}


inline void Semantic::ProcessMethodMembers(AstInterfaceDeclaration *interface_declaration)
{
    assert(ThisType() -> HeaderProcessed());

    for (int k = 0; k < interface_declaration -> NumMethods(); k++)
        ProcessMethodDeclaration(interface_declaration -> Method(k));

    ThisType() -> MarkMethodMembersProcessed();

    return;
}


inline void Semantic::ProcessFieldMembers(AstInterfaceDeclaration *interface_declaration)
{
    assert(ThisType() -> HeaderProcessed());

    for (int k = 0; k < interface_declaration -> NumClassVariables(); k++)
        ProcessFieldDeclaration(interface_declaration -> ClassVariable(k));

    ThisType() -> MarkFieldMembersProcessed();

    return;
}


void Semantic::ProcessMembers(AstInterfaceDeclaration *interface_declaration)
{
    //
    //
    //
    state_stack.Push(interface_declaration -> semantic_environment);
    TypeSymbol *this_type = ThisType();

    assert(! this_type -> MethodMembersProcessed() || this_type -> Bad());
    assert(! this_type -> FieldMembersProcessed() || this_type -> Bad());

    ProcessMethodMembers(interface_declaration);
    ProcessFieldMembers(interface_declaration);

    delete this_type -> innertypes_closure; // save some space !!!
    this_type -> innertypes_closure = NULL;

    for (int i = 0; i < this_type -> NumNestedTypes(); i++)
    {
        TypeSymbol *inner_type = this_type -> NestedType(i);

        if (inner_type -> ACC_INTERFACE())
        {
            AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) inner_type -> declaration;
            ProcessMembers(interface_declaration);
        }
        else
        {
            AstClassDeclaration *class_declaration = (AstClassDeclaration *) inner_type -> declaration;
            ProcessMembers(class_declaration -> semantic_environment, class_declaration -> class_body);
        }
    }

    state_stack.Pop();

    return;
}


//
// Pass 4: Process the field declarations at the top level of the types
//
void Semantic::CompleteSymbolTable(SemanticEnvironment *environment, LexStream::TokenIndex identifier_token, AstClassBody *class_body)
{
    if (compilation_unit -> BadCompilationUnitCast())
        return;

    state_stack.Push(environment);
    TypeSymbol *this_type = ThisType();

    assert(this_type -> ConstructorMembersProcessed());
    assert(this_type -> MethodMembersProcessed());
    assert(this_type -> FieldMembersProcessed());

    //
    //
    //
    if (! this_type -> expanded_method_table)
        ComputeMethodsClosure(this_type, identifier_token);

    ExpandedMethodTable &expanded_table = *(this_type -> expanded_method_table);
    if (! this_type -> ACC_ABSTRACT())
    {
        //
        // Check that every abstract method that is inherited is overridden.
        //
        for (int i = 0; i < expanded_table.symbol_pool.Length(); i++)
        {
            MethodSymbol *method = expanded_table.symbol_pool[i] -> method_symbol;

            if (method -> ACC_ABSTRACT())
            {
                TypeSymbol *containing_type = method -> containing_type;
                if (containing_type != this_type)
                {
                    if (! method -> IsTyped())
                        method -> ProcessMethodSignature((Semantic *) this, identifier_token);

                    //
                    // If the method is contained in an abstract type read from a class file,
                    // then it is possible that the abstract method is just out-of-date and needs
                    // to be recompiled.
                    //
                    ReportSemError((! containing_type -> ACC_INTERFACE()) &&
                                   (containing_type -> file_symbol && containing_type -> file_symbol -> IsClass())
                                        ? SemanticError::NON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD_FROM_ABSTRACT_CLASS
                                        : SemanticError::NON_ABSTRACT_TYPE_INHERITS_ABSTRACT_METHOD,
                                   identifier_token,
                                   identifier_token,
                                   method -> Header(),
                                   containing_type -> ContainingPackage() -> PackageName(),
                                   containing_type -> ExternalName(),
                                   this_type -> ContainingPackage() -> PackageName(),
                                   this_type -> ExternalName());
                }
            }
        }

        //
        // If the super class of this_type is abstract and it is contained in a
        // different package, check to see if its members include abstract methods
        // with default access. If so, we must issue error messages for them also
        // as they cannot be overridden.
        //
        if (this_type != control.Object() && this_type -> super -> ACC_ABSTRACT() &&
            (this_type -> ContainingPackage() != this_type -> super -> ContainingPackage()))
        {
            ExpandedMethodTable &super_expanded_table = *(this_type -> super -> expanded_method_table);
            for (int i = 0; i < super_expanded_table.symbol_pool.Length(); i++)
            {
                MethodSymbol *method = super_expanded_table.symbol_pool[i] -> method_symbol;

                if (method -> ACC_ABSTRACT() &&
                    (! (method -> ACC_PUBLIC() || method -> ACC_PROTECTED() || method -> ACC_PRIVATE())))
                {
                    TypeSymbol *containing_type = method -> containing_type;

                    if (! method -> IsTyped())
                        method -> ProcessMethodSignature((Semantic *) this, identifier_token);

                    //
                    // If the method is contained in an abstract type read from a class file,
                    // then it is possible that the abstract method is just out-of-date and needs
                    // to be recompiled.
                    //
                    ReportSemError(SemanticError::NON_ABSTRACT_TYPE_CANNOT_OVERRIDE_DEFAULT_ABSTRACT_METHOD,
                                   identifier_token,
                                   identifier_token,
                                   method -> Header(),
                                   containing_type -> ContainingPackage() -> PackageName(),
                                   containing_type -> ExternalName(),
                                   this_type -> ContainingPackage() -> PackageName(),
                                   this_type -> ExternalName());
                }
            }
        }
    }

    for (int i = 0; i < expanded_table.symbol_pool.Length(); i++)
    {
        MethodShadowSymbol *method_shadow = expanded_table.symbol_pool[i];

        if (method_shadow -> NumConflicts() > 0)
        {
            MethodSymbol *method = method_shadow -> method_symbol;

            if (method -> containing_type == this_type)
            {
                AstMethodDeclaration *method_declaration = (AstMethodDeclaration *) method -> method_or_constructor_declaration;

                for (int k = 0; k < method_shadow -> NumConflicts(); k++)
                {
                    MethodSymbol *hidden_method = method_shadow -> Conflict(k);
                    if (method -> containing_type != hidden_method -> containing_type) // the methods are not in the same type
                        CheckMethodOverride(method_declaration, hidden_method);
                }
            }
            else
            {
                AstClassDeclaration *class_declaration = (AstClassDeclaration *) this_type -> declaration;

                for (int k = 0; k < method_shadow -> NumConflicts(); k++)
                {
                    MethodSymbol *hidden_method = method_shadow -> Conflict(k);
                    if (method -> containing_type != hidden_method -> containing_type) // the methods are not in the same type
                        CheckMethodOverride(class_declaration, method, hidden_method);
                }

                if (! method -> ACC_ABSTRACT())
                {
                    if (method -> ACC_STATIC())
                    {
                        if (! method -> IsTyped())
                            method -> ProcessMethodSignature((Semantic *) this, identifier_token);

                        if (! method_shadow -> Conflict(0) -> IsTyped())
                            method_shadow -> Conflict(0) -> ProcessMethodSignature((Semantic *) this, identifier_token);

                        ReportSemError(SemanticError::STATIC_OVERRIDE_ABSTRACT_EXTERNALLY,
                                       class_declaration -> identifier_token,
                                       (class_declaration -> NumInterfaces() > 0
                                             ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                             : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                               : class_declaration -> identifier_token)),
                                       lex_stream -> NameString(class_declaration -> identifier_token),
                                       method -> Header(),
                                       method -> containing_type -> ContainingPackage() -> PackageName(),
                                       method -> containing_type -> ExternalName(),
                                       method_shadow -> Conflict(0) -> Header(),
                                       method_shadow -> Conflict(0) -> containing_type -> ContainingPackage() -> PackageName(),
                                       method_shadow -> Conflict(0) -> containing_type -> ExternalName());
                    }
                }
            }

            method_shadow -> RemoveConflicts();
        }
    }

    ProcessStaticInitializers(class_body);

    ProcessBlockInitializers(class_body);

    //
    // Reset the this_variable and this_method may have been set in
    // ProcessStaticInitializers and/or ProcessBlockInitializers.
    // Indicate that there is no method being currently compiled
    // in this environment.
    //
    ThisVariable() = NULL;
    ThisMethod() = NULL;

    //
    // Recursively process all inner types
    //
    for (int l = 0; l < this_type -> NumNestedTypes(); l++)
    {
        TypeSymbol *inner_type = this_type -> NestedType(l);
        if (inner_type -> ACC_INTERFACE())
            CompleteSymbolTable((AstInterfaceDeclaration *) inner_type -> declaration);
        else
        {
            AstClassDeclaration *class_declaration = (AstClassDeclaration *) inner_type -> declaration;
            CompleteSymbolTable(class_declaration -> semantic_environment,
                                class_declaration -> identifier_token, class_declaration -> class_body);
        }
    }

    state_stack.Pop();

    return;
}


void Semantic::CompleteSymbolTable(AstInterfaceDeclaration *interface_declaration)
{
    if (compilation_unit -> BadCompilationUnitCast())
        return;

    state_stack.Push(interface_declaration -> semantic_environment);
    TypeSymbol *this_type = ThisType();

    assert(this_type -> MethodMembersProcessed());
    assert(this_type -> FieldMembersProcessed());

    //
    //
    //
    if (! this_type -> expanded_method_table)
        ComputeMethodsClosure(this_type, interface_declaration -> identifier_token);

    ExpandedMethodTable &expanded_table = *(this_type -> expanded_method_table);
    for (int i = 0; i < interface_declaration -> NumMethods(); i++)
    {
        AstMethodDeclaration *method_declaration = interface_declaration -> Method(i);
        MethodSymbol *method = method_declaration -> method_symbol;

        if (method)
        {
            MethodShadowSymbol *method_shadow = expanded_table.FindOverloadMethodShadow(method,
                                                                                        (Semantic *) this,
                                                                                        interface_declaration -> identifier_token);
            for (int k = 0; k < method_shadow -> NumConflicts(); k++)
            {
                if (method_shadow -> method_symbol -> Type() != method_shadow -> Conflict(k) -> Type())
                {
                    ReportSemError(SemanticError::MISMATCHED_INHERITED_METHOD,
                                   method_declaration -> method_declarator -> LeftToken(),
                                   method_declaration -> method_declarator -> RightToken(),
                                   method_shadow -> method_symbol -> Header(),
                                   method_shadow -> Conflict(k) -> Header(),
                                   method_shadow -> Conflict(k) -> containing_type -> ContainingPackage() -> PackageName(),
                                   method_shadow -> Conflict(k) -> containing_type -> ExternalName());
                }

                if (method_shadow -> method_symbol -> containing_type == this_type) // override ?
                    CheckInheritedMethodThrows(method_declaration, method_shadow -> Conflict(k));
            }
        }
    }

    //
    // Compute the set of final variables (all fields in an interface are final) in this type.
    //
    Tuple<VariableSymbol *> finals(this_type -> NumVariableSymbols());
    for (int j = 0; j < this_type -> NumVariableSymbols(); j++)
    {
        VariableSymbol *variable_symbol = this_type -> VariableSym(j);
        finals.Next() = variable_symbol;
    }

    //
    // Initialize each variable, in turn, and check to see if we need to declare a static initialization method: <clinit>.
    //
    MethodSymbol *init_method = NULL;
    for (int k = 0; k < interface_declaration -> NumClassVariables(); k++)
    {
        InitializeVariable(interface_declaration -> ClassVariable(k), finals);

        //
        // We need a static constructor-initializer if we encounter at least one class
        // variable that is declared with an initialization expression that is not a
        // constant expression.
        //
        if ((! init_method) && NeedsInitializationMethod(interface_declaration -> ClassVariable(k)))
        {
            MethodSymbol *init_method = this_type -> InsertMethodSymbol(control.clinit_name_symbol);

            init_method -> SetType(control.void_type);
            init_method -> SetACC_FINAL();
            init_method -> SetACC_STATIC();
            init_method -> SetContainingType(this_type);
            init_method -> SetBlockSymbol(new BlockSymbol(0)); // the symbol table associated with this block will contain no element
            init_method -> block_symbol -> max_variable_index = 0;
            init_method -> SetSignature(control);

            //
            //
            //
            init_method -> max_block_depth = 2; // TODO: Dave why is this the case? We need a legitimate comment here !!!
            init_method -> block_symbol -> CompressSpace(); // space optimization

            this_type -> static_initializer_method = init_method;
        }
    }

    //
    // Recursively process all inner types
    //
    for (int l = 0; l < this_type -> NumNestedTypes(); l++)
    {
        TypeSymbol *inner_type = this_type -> NestedType(l);
        if (inner_type -> ACC_INTERFACE())
            CompleteSymbolTable((AstInterfaceDeclaration *) inner_type -> declaration);
        else
        {
            AstClassDeclaration *class_declaration = (AstClassDeclaration *) inner_type -> declaration;
            CompleteSymbolTable(class_declaration -> semantic_environment,
                                class_declaration -> identifier_token, class_declaration -> class_body);
        }
    }

    state_stack.Pop();

    return;
}


//
// Pass 5: Free up unneeded space.
//
void Semantic::CleanUp()
{
    for (int i = 0; i < compilation_unit -> NumTypeDeclarations(); i++)
    {
        TypeSymbol *type = NULL;
        Ast *type_declaration = compilation_unit -> TypeDeclaration(i);
        switch(type_declaration -> kind)
        {
            case Ast::CLASS:
            {
                AstClassDeclaration *class_declaration = (AstClassDeclaration *) type_declaration;
                if (class_declaration -> semantic_environment)
                    type = class_declaration -> semantic_environment -> Type();
                break;
            }
            case Ast::INTERFACE:
            {
                AstInterfaceDeclaration *interface_declaration = (AstInterfaceDeclaration *) type_declaration;
                if (interface_declaration -> semantic_environment)
                    type = interface_declaration -> semantic_environment -> Type();
                break;
            }
        }

        if (type)
            CleanUpType(type);
    }

    return;
}


void Semantic::CleanUpType(TypeSymbol *type)
{
    type -> DeleteAnonymousTypes();
    for (int i = 0; i < type -> NumNestedTypes(); i++)
        CleanUpType(type -> NestedType(i));

    type -> CompressSpace(); // space optimization

    for (int j = 0; j < type -> NumMethodSymbols(); j++)
        type -> MethodSym(j) -> CleanUp();

    delete type -> local;
    type -> local = NULL;

    delete type -> non_local;
    type -> non_local = NULL;

    delete type -> semantic_environment;
    type -> semantic_environment = NULL;

    type -> declaration = NULL;

    return;
}


TypeSymbol *Semantic::ReadType(FileSymbol *file_symbol, PackageSymbol *package, NameSymbol *name_symbol, LexStream::TokenIndex tok)
{
    TypeSymbol *type;

    if (file_symbol && file_symbol -> IsJava())
    {
        if (! file_symbol -> semantic)
            control.ProcessHeaders(file_symbol);
        type = package -> FindTypeSymbol(name_symbol);
        if (! type)
        {
            type = package -> InsertOuterTypeSymbol(name_symbol);
            type -> MarkBad();
            type -> outermost_type = type;
            type -> supertypes_closure = new SymbolSet;
            type -> subtypes = new SymbolSet;
            type -> semantic_environment = new SemanticEnvironment((Semantic *) this, type, NULL);
            if (type != control.Object())
                type -> super = (type == control.Throwable() ? control.Object() : control.Throwable());
            type -> SetOwner(package);
            type -> SetSignature(control);
            AddDefaultConstructor(type);
            type -> file_symbol = file_symbol;
            file_symbol -> types.Next() = type;

            ReportSemError(SemanticError::TYPE_NOT_FOUND,
                           tok,
                           tok,
                           type -> ContainingPackage() -> PackageName(),
                           type -> ExternalName());
        }
    }
    else // Read class file.
    {
        type = package -> InsertOuterTypeSymbol(name_symbol);
        type -> outermost_type = type;
        type -> supertypes_closure = new SymbolSet;
        type -> subtypes = new SymbolSet;
        type -> SetOwner(package);
        type -> SetSignature(control);

        if (file_symbol)
        {
            type -> file_symbol = file_symbol;
            type -> SetLocation();

            file_symbol -> package = package;
            file_symbol -> types.Next() = type;

            ReadClassFile(type, tok);

            assert (! type -> IsNested());

            control.input_class_file_set.AddElement(file_symbol);
        }
        else
        {
            control.ProcessBadType(type);
            type -> MarkBad();
            if (type != control.Object())
                type -> super = (type == control.Throwable() ? control.Object() : control.Throwable());
            AddDefaultConstructor(type);

            ReportSemError(SemanticError::TYPE_NOT_FOUND,
                           tok,
                           tok,
                           type -> ContainingPackage() -> PackageName(),
                           type -> ExternalName());

            if (package == control.unnamed_package)
            {
                TypeSymbol *old_type = (TypeSymbol *) control.unnamed_package_types.Image(type -> Identity());
                if (! old_type)
                    control.unnamed_package_types.AddElement(type);
                else
                {
                    ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                                   tok,
                                   tok,
                                   type -> Name(),
                                   old_type -> FileLoc());
                }
            }
        }
    }

    return type;
}


TypeSymbol *Semantic::GetBadNestedType(TypeSymbol *type, LexStream::TokenIndex identifier_token)
{
    NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);

    TypeSymbol *outermost_type = type -> outermost_type;
    if (! outermost_type -> non_local)
        outermost_type -> non_local = new SymbolSet;
    if (! outermost_type -> local)
        outermost_type -> local = new SymbolSet;

    int length = type -> ExternalNameLength() + 1 + name_symbol -> NameLength(); // +1 for $,... +1 for $
    wchar_t *external_name = new wchar_t[length + 1]; // +1 for '\0';
    wcscpy(external_name, type -> ExternalName());
    wcscat(external_name, StringConstant::US__DS);
    wcscat(external_name, name_symbol -> Name());

    TypeSymbol *inner_type = type -> InsertNestedTypeSymbol(name_symbol);
    inner_type -> MarkBad();
    inner_type -> outermost_type = type -> outermost_type;
    inner_type -> supertypes_closure = new SymbolSet;
    inner_type -> subtypes = new SymbolSet;
    inner_type -> SetExternalIdentity(control.FindOrInsertName(external_name, length));
    inner_type -> semantic_environment = new SemanticEnvironment((Semantic *) this,
                                                                 inner_type,
                                                                 type -> semantic_environment);
    inner_type -> super = control.Object();
    inner_type -> SetOwner(type);
    inner_type -> SetSignature(control);
    inner_type -> InsertThis(0);
    AddDefaultConstructor(inner_type);

    ReportSemError(SemanticError::TYPE_NOT_FOUND,
                   identifier_token,
                   identifier_token,
                   inner_type -> ContainingPackage() -> PackageName(),
                   inner_type -> ExternalName());

    delete [] external_name;

    return inner_type;
}


void Semantic::ProcessImportQualifiedName(AstExpression *name)
{
    AstFieldAccess *field_access = name -> FieldAccessCast();
    if (field_access)
    {
        ProcessImportQualifiedName(field_access -> base);
        Symbol *symbol = field_access -> base -> symbol;

        TypeSymbol *type = symbol -> TypeCast();
        if (type) // The base name is a type
        {
            if (! type -> NestedTypesProcessed())
                type -> ProcessNestedTypeSignatures((Semantic *) this, field_access -> identifier_token);
            NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
            TypeSymbol *inner_type = type -> FindTypeSymbol(name_symbol);
            if (! inner_type)
                 inner_type = GetBadNestedType(type, field_access -> identifier_token);
            else if (! (inner_type -> ACC_PUBLIC() || inner_type -> ContainingPackage() == this_package))
                 ReportTypeInaccessible(field_access, inner_type);
            type = inner_type;
            field_access -> symbol = type; // save the type to which this expression was resolved for later use...
        }
        else
        {
            PackageSymbol *package = symbol -> PackageCast();
            NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
            type = package -> FindTypeSymbol(name_symbol);
            if (! type)
            {
                FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);
                if (file_symbol)
                    type = ReadType(file_symbol, package, name_symbol, field_access -> identifier_token);
            }
            else if (type -> SourcePending())
                 control.ProcessHeaders(type -> file_symbol);

            //
            // If the field_access was resolved to a type, save it later use.
            // Otherwise, assume the field_access is a package name.
            //
            if (type)
                 field_access -> symbol = type;
            else
            {
                NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
                PackageSymbol *subpackage = package -> FindPackageSymbol(name_symbol);
                if (! subpackage)
                    subpackage = package -> InsertPackageSymbol(name_symbol);
                control.FindPathsToDirectory(subpackage);
                field_access -> symbol = subpackage;
            }
        }
    }
    else
    {
        AstSimpleName *simple_name = name -> SimpleNameCast();

        assert(simple_name);

        //
        // From the 1.1 document:
        //
        //    Nested classes of all sorts (top-level or inner) can be imported by either kind of
        //    import statement. Class names in import statements must be fully package
        //    qualified, and be resolvable without reference to inheritance relations...
        //
        TypeSymbol *type;
        if (compilation_unit -> package_declaration_opt)
        {
            type = FindSimpleNameType(this_package, simple_name -> identifier_token);
            //
            // If the type was not found, look for it in the unnamed package.
            // The relevant passages that justify this lookup are:
            // 6.5.4.11, 6.7, 7.4.2, 7.5.1
            //
            if (! type)
                type = FindSimpleNameType(control.unnamed_package, simple_name -> identifier_token);
        }
        else type = FindSimpleNameType(control.unnamed_package, simple_name -> identifier_token);

        //
        // If the simple_name is a type, save it. Otherwise, assume it is a package
        //
        if (type)
        {
            if (! (type -> ACC_PUBLIC() || type -> ContainingPackage() == this_package))
                ReportTypeInaccessible(name, type);
            simple_name -> symbol = type;
        }
        else
        {
            NameSymbol *name_symbol = lex_stream -> NameSymbol(simple_name -> identifier_token);
            PackageSymbol *package = control.external_table.FindPackageSymbol(name_symbol);
            if (! package)
                package = control.external_table.InsertPackageSymbol(name_symbol, NULL);
            control.FindPathsToDirectory(package);
            simple_name -> symbol = package;
        }
    }

    return;
}


void Semantic::ProcessPackageOrType(AstExpression *name)
{
    AstFieldAccess *field_access = name -> FieldAccessCast();
    if (field_access)
    {
        ProcessPackageOrType(field_access -> base);
        Symbol *symbol = field_access -> base -> symbol;

        TypeSymbol *type = symbol -> TypeCast();
        if (type) // The base name is a type
        {
            type = MustFindNestedType(type, field_access);
            field_access -> symbol = type; // save the type to which this expression was resolved for later use...
        }
        else
        {
            PackageSymbol *package = symbol -> PackageCast();
            NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
            type = package -> FindTypeSymbol(name_symbol);
            if (! type)
            {
                FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);
                if (file_symbol)
                    type = ReadType(file_symbol, package, name_symbol, field_access -> identifier_token);
            }
            else if (type -> SourcePending())
                 control.ProcessHeaders(type -> file_symbol);

            //
            // If the field access was resolved into a type, then save it.
            // Otherwise, assume it is a package
            //
            if (type)
                 field_access -> symbol = type; // save the type to which this expression was resolved for later use...
            else
            {
                NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
                PackageSymbol *subpackage = package -> FindPackageSymbol(name_symbol);
                if (! subpackage)
                    subpackage = package -> InsertPackageSymbol(name_symbol);
                control.FindPathsToDirectory(subpackage);
                field_access -> symbol = subpackage;
            }
        }
    }
    else
    {
        AstSimpleName *simple_name = name -> SimpleNameCast();

        assert(simple_name);

        TypeSymbol *type = FindType(simple_name -> identifier_token);
        if (type)
        {
            TypeAccessCheck(simple_name, type);
            simple_name -> symbol = type;
        }
        else
        {
            NameSymbol *name_symbol = lex_stream -> NameSymbol(simple_name -> identifier_token);
            PackageSymbol *package = control.external_table.FindPackageSymbol(name_symbol);
            if (! package)
                package = control.external_table.InsertPackageSymbol(name_symbol, NULL);
            control.FindPathsToDirectory(package);
            simple_name -> symbol = package;
        }
    }

    return;
}


void Semantic::ProcessTypeImportOnDemandDeclaration(AstImportDeclaration *import_declaration)
{
    ProcessImportQualifiedName(import_declaration -> name);
    Symbol *symbol = import_declaration -> name -> symbol;

    PackageSymbol *package = symbol -> PackageCast();
    if (package && package -> directory.Length() == 0)
    {
        ReportSemError(SemanticError::PACKAGE_NOT_FOUND,
                       import_declaration -> name -> LeftToken(),
                       import_declaration -> name -> RightToken(),
                       package -> PackageName());
    }

    //
    // Two or more type-import-on-demand may name the same package; the effect is as if there
    // were only one such declaration.
    //
    for (int i = 0; i < import_on_demand_packages.Length(); i++)
    {
        if (symbol == import_on_demand_packages[i])
            return;
    }

    import_on_demand_packages.Next() = symbol;

    //
    //
    //
    TypeSymbol *type = symbol -> TypeCast();
    if (control.option.deprecation && type && type -> IsDeprecated() && type -> file_symbol != source_file_symbol)
    {
        ReportSemError(SemanticError::DEPRECATED_TYPE,
                       import_declaration -> name -> LeftToken(),
                       import_declaration -> name -> RightToken(),
                       type -> ContainingPackage() -> PackageName(),
                       type -> ExternalName());
    }

    return;
}


//
// The Ast name is a name expression (either a qualified name or a simplename)
// FindFirstType traverses the name tree and returns the first subtree that it
// finds that matches a type. As a side-effect, each subtree that matches a package
// or a type has that package or type recorded in its "symbol" field.
//
AstExpression *Semantic::FindFirstType(Ast *name)
{
    AstExpression *name_expression = NULL;

    AstFieldAccess *field_access = name -> FieldAccessCast();
    if (field_access)
    {
        AstExpression *expr = FindFirstType(field_access -> base);

        if (expr -> symbol -> TypeCast()) // A subexpression has been found, pass it up
            name_expression = expr;
        else
        {
            PackageSymbol *package = expr -> symbol -> PackageCast();

            assert(package);

            name_expression = field_access; // The relevant subexpression might be this field access...

            NameSymbol *name_symbol = lex_stream -> NameSymbol(field_access -> identifier_token);
            TypeSymbol *type = package -> FindTypeSymbol(name_symbol);
            if (type)
            {
                if (type -> SourcePending())
                    control.ProcessHeaders(type -> file_symbol);
                field_access -> symbol = type;
            }
            else
            {
                FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);
                if (file_symbol)
                    field_access -> symbol = ReadType(file_symbol, package, name_symbol, field_access -> identifier_token);
                else
                {
                    PackageSymbol *subpackage = package -> FindPackageSymbol(name_symbol);
                    if (! subpackage)
                        subpackage = package -> InsertPackageSymbol(name_symbol);
                    control.FindPathsToDirectory(subpackage);
                    field_access -> symbol = subpackage;
                }
            }
        }
    }
    else
    {
        AstSimpleName *simple_name = name -> SimpleNameCast();

        assert(simple_name);

        ProcessPackageOrType(simple_name);
        name_expression = simple_name;
    }

    return name_expression;
}


TypeSymbol *Semantic::FindSimpleNameType(PackageSymbol *package, LexStream::TokenIndex identifier_token)
{
    NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);
    TypeSymbol *type = package -> FindTypeSymbol(name_symbol);
    if (type)
    {
        if (type -> SourcePending())
             control.ProcessHeaders(type -> file_symbol);
    }
    else
    {
        //
        // Check whether or not the type was declared in another compilation unit
        // in the main package.
        //
        FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);
        if (file_symbol)
            type = ReadType(file_symbol, package, name_symbol, identifier_token);
    }

    return type;
}

void Semantic::ProcessSingleTypeImportDeclaration(AstImportDeclaration *import_declaration)
{
    ProcessImportQualifiedName(import_declaration -> name);
    Symbol *symbol = import_declaration -> name -> symbol;
    PackageSymbol *package = symbol -> PackageCast();
    if (package)
    {
        ReportSemError(SemanticError::UNKNOWN_QUALIFIED_NAME_BASE,
                       import_declaration -> name -> LeftToken(),
                       import_declaration -> name -> RightToken(),
                       package -> PackageName());
        return;
    }

    TypeSymbol *type = symbol -> TypeCast();

    //
    // If two single-type-import declarations in the same compilation unit attempt to
    // import types with the same simple name, then a compile-time error occurs, unless
    // the two types are the same type, in which case the duplicate declaration is ignored.
    //
    for (int i = 0; i < single_type_imports.Length(); i++)
    {
        if (type == single_type_imports[i])
            return;
    }

    TypeSymbol *old_type;
    int k;
    for (k = 0; k < compilation_unit -> NumTypeDeclarations(); k++)
    {
        AstClassDeclaration *class_declaration;
        AstInterfaceDeclaration *interface_declaration;

        if (class_declaration = compilation_unit -> TypeDeclaration(k) -> ClassDeclarationCast())
        {
            if (class_declaration -> semantic_environment)
            {
                old_type = class_declaration -> semantic_environment -> Type();
                if (old_type -> Identity() == type -> Identity())
                    break;
            }
        }
        else if (interface_declaration = compilation_unit -> TypeDeclaration(k) -> InterfaceDeclarationCast())
        {
            if (interface_declaration -> semantic_environment)
            {
                old_type = interface_declaration -> semantic_environment -> Type();
                if (old_type -> Identity() == type -> Identity())
                    break;
            }
        }
    }

    if (k < compilation_unit -> NumTypeDeclarations())
    {
        AstFieldAccess *field_access = import_declaration -> name -> FieldAccessCast();
        package = (field_access ? field_access -> base -> symbol -> PackageCast() : control.unnamed_package);

        //
        // It's ok to import a type that is being compiled...
        //
        if (type == old_type && package == this_package)
        {
            ReportSemError(SemanticError::UNNECESSARY_TYPE_IMPORT,
                           import_declaration -> name -> LeftToken(),
                           import_declaration -> name -> RightToken(),
                           lex_stream -> NameString(import_declaration -> name -> RightToken()),
                           old_type -> FileLoc());
        }
        else
        {
            ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                           import_declaration -> name -> LeftToken(),
                           import_declaration -> name -> RightToken(),
                           lex_stream -> NameString(import_declaration -> name -> RightToken()),
                           old_type -> FileLoc());
        }
    }
    else
    {
        int i = 0;
        for (i = 0; i < compilation_unit -> NumImportDeclarations(); i++)
        {
            TypeSymbol *other_type = compilation_unit -> ImportDeclaration(i) -> name -> Type();
            if ((compilation_unit -> ImportDeclaration(i) == import_declaration) ||
                (other_type && other_type -> Identity() == type -> Identity()))
                break;
        }

        assert(i < compilation_unit -> NumImportDeclarations());

        if (compilation_unit -> ImportDeclaration(i) == import_declaration) // No duplicate found
        {
            import_declaration -> name -> symbol = type;
            single_type_imports.Next() = type;
        }
        else
        {
            FileLocation file_location(lex_stream, compilation_unit -> ImportDeclaration(i) -> LeftToken());
            ReportSemError(SemanticError::DUPLICATE_TYPE_DECLARATION,
                           import_declaration -> name -> LeftToken(),
                           import_declaration -> name -> RightToken(),
                           lex_stream -> NameString(import_declaration -> name -> RightToken()),
                           file_location.location);
        }
    }

    if (! (type -> ACC_PUBLIC() || type -> ContainingPackage() == this_package))
        ReportTypeInaccessible(import_declaration -> name, type);

    //
    //
    //
    if (control.option.deprecation && type -> IsDeprecated() && type -> file_symbol != source_file_symbol)
    {
        ReportSemError(SemanticError::DEPRECATED_TYPE,
                       import_declaration -> name -> LeftToken(),
                       import_declaration -> name -> RightToken(),
                       type -> ContainingPackage() -> PackageName(),
                       type -> ExternalName());
    }

    return;
}


void Semantic::ProcessFieldDeclaration(AstFieldDeclaration *field_declaration)
{
    TypeSymbol *this_type = ThisType();
    AccessFlags access_flags = (this_type -> ACC_INTERFACE()
                                           ? ProcessConstantModifiers(field_declaration)
                                           : ProcessFieldModifiers(field_declaration));

    //
    // New feature in java 1.2 that is undocumented in the 1.1 document.
    // A field may be declared static iff it is final and not blank-final...
    //
    if (access_flags.ACC_STATIC() && (! access_flags.ACC_FINAL()) && this_type -> IsInner())
    {
        AstModifier *modifier = NULL;
        for (int i = 0; i < field_declaration -> NumVariableModifiers(); i++)
        {
            if (field_declaration -> VariableModifier(i) -> kind == Ast::STATIC)
                modifier = field_declaration -> VariableModifier(i);
        }

        assert(modifier);

        ReportSemError(SemanticError::STATIC_FIELD_IN_INNER_CLASS,
                       modifier -> modifier_kind_token,
                       modifier -> modifier_kind_token);
    }

    //
    //
    //
    bool deprecated_declarations = lex_stream -> IsDeprecated(lex_stream -> Previous(field_declaration -> LeftToken()));
    AstArrayType *array_type = field_declaration -> type -> ArrayTypeCast();
    Ast *actual_type = (array_type ? array_type -> type : field_declaration -> type);
    AstPrimitiveType *primitive_type = actual_type -> PrimitiveTypeCast();
    TypeSymbol *field_type = (primitive_type ? FindPrimitiveType(primitive_type) : MustFindType(actual_type));
    for (int i = 0; i < field_declaration -> NumVariableDeclarators(); i++)
    {
        AstVariableDeclarator *variable_declarator = field_declaration -> VariableDeclarator(i);
        AstVariableDeclaratorId *name = variable_declarator -> variable_declarator_name;
        NameSymbol *name_symbol = lex_stream -> NameSymbol(name -> identifier_token);

        if (this_type -> FindVariableSymbol(name_symbol))
        {
            ReportSemError(SemanticError::DUPLICATE_FIELD,
                           name -> identifier_token,
                           name -> identifier_token,
                           name_symbol -> Name(),
                           this_type -> Name());
        }
        else
        {
            VariableSymbol *variable = this_type -> InsertVariableSymbol(name_symbol);
            int num_dimensions = (array_type ? array_type -> NumBrackets() : 0) + name -> NumBrackets();
            if (num_dimensions == 0)
                 variable -> SetType(field_type);
            else variable -> SetType(field_type -> GetArrayType((Semantic *) this, num_dimensions));
            variable -> SetFlags(access_flags);
            variable -> SetOwner(this_type);
            variable -> declarator = variable_declarator;
            variable -> MarkIncomplete(); // the declaration of a field is not complete until its initializer
                                          // (if any) has been processed.
            variable_declarator -> symbol = variable;

            if (deprecated_declarations)
                variable -> MarkDeprecated();
        }
    }

    return;
}


void Semantic::GenerateLocalConstructor(MethodSymbol *constructor)
{
    TypeSymbol *local_type = constructor -> containing_type;

    //
    // Make up external name for constructor as we shall turn it into a regular method later.
    // Note that the method needs to be PRIVATE and FINAL so that:
    //
    //    1. virtual calls are not made to it
    //
    //    2. it might be inlined later.
    //
    // This resetting destroys the original access flags specified by the user. We shall
    // say more about this later...
    //
    IntToWstring value(local_type -> NumGeneratedConstructors());

    int length = 12 + value.Length(); // +12 for constructor$
    wchar_t *external_name = new wchar_t[length + 1]; // +1 for '\0';
    wcscpy(external_name, StringConstant::US__constructor_DOLLAR);
    wcscat(external_name, value.String());
    constructor -> SetExternalIdentity(control.FindOrInsertName(external_name, length)); // Turn the constructor into a method
    constructor -> ResetFlags();
    constructor -> SetACC_PRIVATE();
    constructor -> SetACC_FINAL();

    delete [] external_name;

    //
    // Make generated constructor symbol. The associated symbol table will not contain too many elements.
    //
    BlockSymbol *block_symbol = new BlockSymbol(local_type -> NumConstructorParameters() +
                                                constructor -> NumFormalParameters() + 3);
    block_symbol -> max_variable_index = 1; // All types need a spot for "this"

    //
    // Note that the local constructor does not inherit the access flags of the specified constructor.
    // This is because it acts more like a read_access method. The synthetic attribute that is associated
    // with the constructor allows the compiler to prevent an illegal access from an unauthorized environment.
    //
    MethodSymbol *local_constructor = local_type -> LocalConstructorOverload(constructor);
    local_constructor -> MarkSynthetic();
    local_constructor -> method_or_constructor_declaration = constructor -> method_or_constructor_declaration;
    local_constructor -> SetType(control.void_type);
    local_constructor -> SetContainingType(local_type);
    local_constructor -> SetBlockSymbol(block_symbol);
    for (int i = 0; i < constructor -> NumThrows(); i++)
        local_constructor -> AddThrows(constructor -> Throws(i));

    for (int j = 0; j < local_type -> NumConstructorParameters(); j++)
    {
        VariableSymbol *param = local_type -> ConstructorParameter(j),
                       *symbol = block_symbol -> InsertVariableSymbol(param -> Identity());
        symbol -> MarkSynthetic();
        symbol -> SetType(param -> Type());
        symbol -> SetOwner(local_constructor);
        symbol -> SetExternalIdentity(param -> ExternalIdentity());
        symbol -> SetLocalVariableIndex(block_symbol -> max_variable_index++);
        if (control.IsDoubleWordType(symbol -> Type()))
            block_symbol -> max_variable_index++;
        local_constructor -> AddFormalParameter(symbol);
    }

    //
    // Add all the parameters from the original constructor to the symbol
    // table of the local constructor. However, only mark them complete and
    // do not yet assign a number to them. This will be done after we know
    // how many extra "local" variable shadows are needed.
    //
    for (int k = 0; k < constructor -> NumFormalParameters(); k++)
    {
        VariableSymbol *param = constructor -> FormalParameter(k),
                       *symbol = block_symbol -> InsertVariableSymbol(param -> Identity());
        symbol -> MarkSynthetic();
        symbol -> MarkComplete();
        symbol -> SetType(param -> Type());
        symbol -> SetOwner(local_constructor);
    }

    local_type -> AddGeneratedConstructor(local_constructor);

    return;
}


void Semantic::ProcessConstructorDeclaration(AstConstructorDeclaration *constructor_declaration)
{
    TypeSymbol *this_type = ThisType();
    if (this_type -> Anonymous())
    {
        ReportSemError(SemanticError::CONSTRUCTOR_FOUND_IN_ANONYMOUS_CLASS,
                       constructor_declaration -> LeftToken(),
                       constructor_declaration -> RightToken());
        return;
    }

    AccessFlags access_flags = ProcessConstructorModifiers(constructor_declaration);

    AstMethodDeclarator *constructor_declarator = constructor_declaration -> constructor_declarator;
    NameSymbol *name_symbol = lex_stream -> NameSymbol(constructor_declarator -> identifier_token);
    wchar_t *constructor_name = lex_stream -> NameString(constructor_declarator -> identifier_token);

    if (lex_stream -> NameSymbol(constructor_declarator -> identifier_token) != this_type -> Identity())
    {
        ReportSemError(SemanticError::MISMATCHED_CONSTRUCTOR_NAME,
                       constructor_declarator -> identifier_token,
                       constructor_declarator -> identifier_token,
                       constructor_name,
                       this_type -> Name());
        constructor_name = this_type -> Name(); // assume the proper name !
    }

    //
    // As the body of the constructor may not have been parsed yet, we estimate a size
    // for its symbol table based on the number of lines in the body + a margin for one-liners.
    //
    AstConstructorBlock *block = constructor_declaration -> constructor_body -> ConstructorBlockCast();
    BlockSymbol *block_symbol = new BlockSymbol(constructor_declarator -> NumFormalParameters() + 3);
    block_symbol -> max_variable_index = 1; // All types need a spot for "this".

    ProcessFormalParameters(block_symbol, constructor_declarator);

    //
    // Note that constructors are always named "<init>"
    //
    MethodSymbol *constructor = this_type -> FindMethodSymbol(control.init_name_symbol);

    if (! constructor) // there exists a constructor already in type -> table.
         constructor = this_type -> InsertConstructorSymbol(control.init_name_symbol);
    else
    {
        if (this_type -> FindOverloadMethod(constructor, constructor_declarator))
        {
            ReportSemError(SemanticError::DUPLICATE_CONSTRUCTOR,
                           constructor_declarator -> LeftToken(),
                           constructor_declarator -> RightToken(),
                           this_type -> Name());
            delete block_symbol;
            return;
        }

        constructor = this_type -> Overload(constructor);
    }

    //
    // If the method is not static, leave a slot for the "this" pointer.
    //
    constructor -> SetType(control.void_type);
    constructor -> SetFlags(access_flags);
    constructor -> SetContainingType(this_type);
    constructor -> SetBlockSymbol(block_symbol);
    constructor -> method_or_constructor_declaration = constructor_declaration;

    VariableSymbol *this0_variable = NULL;
    if (this_type -> IsInner())
    {
        this0_variable = block_symbol -> InsertVariableSymbol(control.this0_name_symbol);
        this0_variable -> SetType(this_type -> ContainingType());
        this0_variable -> SetOwner(constructor);
        this0_variable -> SetLocalVariableIndex(block_symbol -> max_variable_index++);
    }

    for (int i = 0; i < constructor_declarator -> NumFormalParameters(); i++)
    {
        AstVariableDeclarator *formal_declarator = constructor_declarator -> FormalParameter(i) -> formal_declarator;
        VariableSymbol *symbol = formal_declarator -> symbol;

        symbol -> SetOwner(constructor);
        symbol -> SetLocalVariableIndex(block_symbol -> max_variable_index++);
        if (control.IsDoubleWordType(symbol -> Type()))
            block_symbol -> max_variable_index++;
        symbol -> declarator = formal_declarator;
        constructor -> AddFormalParameter(symbol);
    }

    constructor -> SetSignature(control, this0_variable);

    for (int k = 0; k < constructor_declaration -> NumThrows(); k++)
    {
        AstExpression *throw_expression = constructor_declaration -> Throw(k);
        TypeSymbol *throw_type = MustFindType(throw_expression);
        throw_expression -> symbol = throw_type;
        constructor -> AddThrows(throw_type);
    }

    constructor_declaration -> constructor_symbol = constructor; // save for processing bodies later.

    if (this_type -> IsLocal())
        GenerateLocalConstructor(constructor);

    if (lex_stream -> IsDeprecated(lex_stream -> Previous(constructor_declaration -> LeftToken())))
        constructor -> MarkDeprecated();

    return;
}


void Semantic::AddDefaultConstructor(TypeSymbol *type)
{
    MethodSymbol *constructor = type -> InsertConstructorSymbol(control.init_name_symbol);

    BlockSymbol *block_symbol = new BlockSymbol(1); // TODO: make sure this size is right !!!
    block_symbol -> max_variable_index = 1; // All types need a spot for "this"

    constructor -> SetType(control.void_type);
    constructor -> SetContainingType(type);
    constructor -> SetBlockSymbol(block_symbol);
    if (type -> ACC_PUBLIC())
        constructor -> SetACC_PUBLIC();

    VariableSymbol *this0_variable = NULL;
    if (type -> IsInner())
    {
        this0_variable = block_symbol -> InsertVariableSymbol(control.this0_name_symbol);
        this0_variable -> SetType(type -> ContainingType());
        this0_variable -> SetOwner(constructor);
        this0_variable -> SetLocalVariableIndex(block_symbol -> max_variable_index++);
    }

    constructor -> SetSignature(control, this0_variable);

    AstClassDeclaration *class_declaration = (type -> declaration ? type -> declaration -> ClassDeclarationCast()
                                                                  : (AstClassDeclaration *) NULL);
    if (class_declaration)
    {
        AstClassBody *class_body = class_declaration -> class_body;
        LexStream::TokenIndex left_loc  = class_declaration -> identifier_token,
                              right_loc = (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                          : class_declaration -> identifier_token);

        AstMethodDeclarator *method_declarator       = compilation_unit -> ast_pool -> GenMethodDeclarator();
        method_declarator -> identifier_token        = left_loc;
        method_declarator -> left_parenthesis_token  = left_loc;
        method_declarator -> right_parenthesis_token = right_loc;

        AstSuperCall *super_call = NULL;
        if (type != control.Object())
        {
            super_call                            = compilation_unit -> ast_pool -> GenSuperCall();
            super_call -> base_opt                = NULL;
            super_call -> dot_token_opt           = 0;
            super_call -> super_token             = left_loc;
            super_call -> left_parenthesis_token  = left_loc;
            super_call -> right_parenthesis_token = right_loc;
            super_call -> semicolon_token         = right_loc;
        }

        AstReturnStatement *return_statement = compilation_unit -> ast_pool -> GenReturnStatement();
        return_statement -> return_token = left_loc;
        return_statement -> expression_opt = NULL;
        return_statement -> semicolon_token = left_loc;
        return_statement -> is_reachable = true;

        AstBlock *block = compilation_unit -> ast_pool -> GenBlock();
        block -> AllocateBlockStatements(1); // this block contains one statement
        block -> left_brace_token  = left_loc;
        block -> right_brace_token = right_loc;

        block -> is_reachable = true;
        block -> can_complete_normally = false;
        block -> AddStatement(return_statement);

        AstConstructorBlock *constructor_block                   = compilation_unit -> ast_pool -> GenConstructorBlock();
        constructor_block -> left_brace_token                    = left_loc;
        constructor_block -> explicit_constructor_invocation_opt = super_call;
        constructor_block -> block                               = block;
        constructor_block -> right_brace_token                   = right_loc;

        AstConstructorDeclaration *constructor_declaration = compilation_unit -> ast_pool -> GenConstructorDeclaration();
        constructor_declaration -> constructor_declarator   = method_declarator;
        constructor_declaration -> constructor_body         = constructor_block;

        constructor_declaration -> constructor_symbol = constructor;
        constructor -> method_or_constructor_declaration = constructor_declaration;
        class_body -> default_constructor = constructor_declaration;

        if (type -> IsLocal())
            GenerateLocalConstructor(constructor);
    }

    return;
}


void Semantic::CheckInheritedMethodThrows(AstMethodDeclaration *method_declaration, MethodSymbol *method)
{
    if (method_declaration -> NumThrows() > 0)
    {
        if (! method -> IsTyped())
            method -> ProcessMethodSignature((Semantic *) this, method_declaration -> Throw(0) -> LeftToken());
        method -> ProcessMethodThrows((Semantic *) this, method_declaration -> Throw(0) -> LeftToken());

        for (int i = 0; i < method_declaration -> NumThrows(); i++)
        {
            AstExpression *name = method_declaration -> Throw(i);
            TypeSymbol *exception = (TypeSymbol *) name -> symbol;

            if (CheckedException(exception))
            {
                int k;
                for (k = method -> NumThrows() - 1; k >= 0; k--)
                {
                    if (exception -> IsSubclass(method -> Throws(k)))
                        break;
                }

                if (k < 0)
                {
                    ReportSemError(SemanticError::MISMATCHED_OVERRIDDEN_EXCEPTION,
                                   name -> LeftToken(),
                                   name -> RightToken(),
                                   exception -> Name(),
                                   method -> Header(),
                                   method -> containing_type -> ContainingPackage() -> PackageName(),
                                   method -> containing_type -> ExternalName());
                }
            }
        }
    }

    return;
}


void Semantic::CheckMethodOverride(AstMethodDeclaration *method_declaration, MethodSymbol *hidden_method)
{
    AstMethodDeclarator *method_declarator = method_declaration -> method_declarator;
    MethodSymbol *method = method_declaration -> method_symbol;
    LexStream::TokenIndex token_location = method_declaration -> method_declarator -> identifier_token;

    if (hidden_method -> Type() != method -> Type())
    {
        ReportSemError(SemanticError::MISMATCHED_INHERITED_METHOD,
                       method_declarator -> LeftToken(),
                       method_declarator -> RightToken(),
                       method -> Header(),
                       hidden_method -> Header(),
                       hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                       hidden_method -> containing_type -> ExternalName());
    }
    if (hidden_method -> IsFinal() || hidden_method -> ACC_PRIVATE()) // Merged because same kind of message. See error.cpp
        ReportSemError(hidden_method -> IsFinal() ? SemanticError::FINAL_METHOD_OVERRIDE : SemanticError::PRIVATE_METHOD_OVERRIDE,
                       method_declarator -> LeftToken(),
                       method_declarator -> RightToken(),
                       method -> Header(),
                       hidden_method -> Header(),
                       hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                       hidden_method -> containing_type -> ExternalName());

    if (method -> ACC_STATIC() != hidden_method -> ACC_STATIC())
    {
        if (method -> ACC_STATIC())
             ReportSemError(SemanticError::INSTANCE_METHOD_OVERRIDE,
                            method_declarator -> LeftToken(),
                            method_declarator -> RightToken(),
                            method -> Header(),
                            hidden_method -> Header(),
                            hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                            hidden_method -> containing_type -> ExternalName());
        else ReportSemError(SemanticError::CLASS_METHOD_OVERRIDE,
                            method_declarator -> LeftToken(),
                            method_declarator -> RightToken(),
                            method -> Header(),
                            hidden_method -> Header(),
                            hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                            hidden_method -> containing_type -> ExternalName());
    }

    if (hidden_method -> ACC_PUBLIC())
    {
        if (! method -> ACC_PUBLIC())
            ReportSemError(SemanticError::BAD_ACCESS_METHOD_OVERRIDE,
                           method_declarator -> LeftToken(),
                           method_declarator -> RightToken(),
                           method -> Header(),
                           (method -> ACC_PRIVATE() ? StringConstant::US_private : (method -> ACC_PROTECTED() ? StringConstant::US_protected : StringConstant::US_default)),
                           hidden_method -> Header(),
                           StringConstant::US_public,
                           hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                           hidden_method -> containing_type -> ExternalName());
    }
    else if (hidden_method -> ACC_PROTECTED())
    {
        if (! (method -> ACC_PROTECTED() || method -> ACC_PUBLIC()))
             ReportSemError(SemanticError::BAD_ACCESS_METHOD_OVERRIDE,
                            method_declarator -> LeftToken(),
                            method_declarator -> RightToken(),
                            method -> Header(),
                            StringConstant::US_default,
                            hidden_method -> Header(),
                            StringConstant::US_protected,
                            hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                            hidden_method -> containing_type -> ExternalName());
    }
    else if (method -> ACC_PRIVATE()) // The hidden_method method must have default access as it cannot be private...
    {
         ReportSemError(SemanticError::BAD_ACCESS_METHOD_OVERRIDE,
                        method_declarator -> LeftToken(),
                        method_declarator -> RightToken(),
                        method -> Header(),
                        StringConstant::US_private,
                        hidden_method -> Header(),
                        StringConstant::US_default,
                        hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                        hidden_method -> containing_type -> ExternalName());
    }

    CheckInheritedMethodThrows(method_declaration, hidden_method);

    return;
}


void Semantic::CheckInheritedMethodThrows(AstClassDeclaration *class_declaration, MethodSymbol *method, MethodSymbol *hidden_method)
{
    method -> ProcessMethodThrows((Semantic *) this, class_declaration -> identifier_token);
    hidden_method -> ProcessMethodThrows((Semantic *) this, class_declaration -> identifier_token);

    for (int i = method -> NumThrows() - 1; i >= 0; i--)
    {
        TypeSymbol *exception = method -> Throws(i);

        if (CheckedException(exception))
        {
            int k;
            for (k = hidden_method -> NumThrows() - 1; k >= 0; k--)
            {
                if (exception -> IsSubclass(hidden_method -> Throws(k)))
                    break;
            }

            if (k < 0)
            {
                if (! method -> IsTyped())
                    method -> ProcessMethodSignature((Semantic *) this, class_declaration -> identifier_token);

                if (! hidden_method -> IsTyped())
                    hidden_method -> ProcessMethodSignature((Semantic *) this, class_declaration -> identifier_token);

                ReportSemError(SemanticError::MISMATCHED_OVERRIDDEN_EXCEPTION_EXTERNALLY,
                               class_declaration -> identifier_token,
                               (class_declaration -> NumInterfaces() > 0
                                     ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                     : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                       : class_declaration -> identifier_token)),
                               lex_stream -> NameString(class_declaration -> identifier_token),
                               exception -> Name(),
                               method -> Header(),
                               hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                               hidden_method -> containing_type -> ExternalName(),
                               hidden_method -> Header(),
                               method -> containing_type -> ContainingPackage() -> PackageName(),
                               method -> containing_type -> ExternalName());
            }
        }
    }

    return;
}


void Semantic::CheckMethodOverride(AstClassDeclaration *class_declaration, MethodSymbol *method, MethodSymbol *hidden_method)
{
    if (hidden_method -> Type() != method -> Type())
        ReportSemError(SemanticError::MISMATCHED_INHERITED_METHOD_EXTERNALLY,
                       class_declaration -> identifier_token,
                       (class_declaration -> NumInterfaces() > 0
                                           ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                           : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                             : class_declaration -> identifier_token)),
                       lex_stream -> NameString(class_declaration -> identifier_token),
                       method -> Header(),
                       method -> containing_type -> ContainingPackage() -> PackageName(),
                       method -> containing_type -> ExternalName(),
                       hidden_method -> Header(),
                       hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                       hidden_method -> containing_type -> ExternalName());
    if (hidden_method -> IsFinal() || hidden_method -> ACC_PRIVATE()) // Merged because same kind of message. See error.cpp
        ReportSemError(hidden_method -> IsFinal() ? SemanticError::FINAL_METHOD_OVERRIDE_EXTERNALLY
                                                  : SemanticError::PRIVATE_METHOD_OVERRIDE_EXTERNALLY,
                       class_declaration -> identifier_token,
                       (class_declaration -> NumInterfaces() > 0
                                           ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                           : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                             : class_declaration -> identifier_token)),
                       lex_stream -> NameString(class_declaration -> identifier_token),
                       method -> Header(),
                       method -> containing_type -> ContainingPackage() -> PackageName(),
                       method -> containing_type -> ExternalName(),
                       hidden_method -> Header(),
                       hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                       hidden_method -> containing_type -> ExternalName());

    if (method -> ACC_STATIC() != hidden_method -> ACC_STATIC())
    {
        if (method -> ACC_STATIC())
             ReportSemError(SemanticError::INSTANCE_METHOD_OVERRIDE_EXTERNALLY,
                            class_declaration -> identifier_token,
                            (class_declaration -> NumInterfaces() > 0
                                                ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                                : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                                  : class_declaration -> identifier_token)),
                            lex_stream -> NameString(class_declaration -> identifier_token),
                            method -> Header(),
                            method -> containing_type -> ContainingPackage() -> PackageName(),
                            method -> containing_type -> ExternalName(),
                            hidden_method -> Header(),
                            hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                            hidden_method -> containing_type -> ExternalName());
        else ReportSemError(SemanticError::CLASS_METHOD_OVERRIDE_EXTERNALLY,
                            class_declaration -> identifier_token,
                            (class_declaration -> NumInterfaces() > 0
                                                ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                                : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                                  : class_declaration -> identifier_token)),
                            lex_stream -> NameString(class_declaration -> identifier_token),
                            method -> Header(),
                            method -> containing_type -> ContainingPackage() -> PackageName(),
                            method -> containing_type -> ExternalName(),
                            hidden_method -> Header(),
                            hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                            hidden_method -> containing_type -> ExternalName());
    }

    if (hidden_method -> ACC_PUBLIC())
    {
        if (! method -> ACC_PUBLIC())
            ReportSemError(SemanticError::BAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY,
                           class_declaration -> identifier_token,
                           (class_declaration -> NumInterfaces() > 0
                                               ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                               : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                                 : class_declaration -> identifier_token)),
                           lex_stream -> NameString(class_declaration -> identifier_token),
                           method -> Header(),
                           (method -> ACC_PRIVATE() ? StringConstant::US_private
                                                    : (method -> ACC_PROTECTED() ? StringConstant::US_protected
                                                                                 : StringConstant::US_default)),
                           method -> containing_type -> ContainingPackage() -> PackageName(),
                           method -> containing_type -> ExternalName(),
                           hidden_method -> Header(),
                           StringConstant::US_public,
                           hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                           hidden_method -> containing_type -> ExternalName());
    }
    else if (hidden_method -> ACC_PROTECTED())
    {
        if (! (method -> ACC_PROTECTED() || method -> ACC_PUBLIC()))
             ReportSemError(SemanticError::BAD_ACCESS_METHOD_OVERRIDE,
                            class_declaration -> identifier_token,
                            (class_declaration -> NumInterfaces() > 0
                                                ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                                : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                                  : class_declaration -> identifier_token)),
                            lex_stream -> NameString(class_declaration -> identifier_token),
                            method -> Header(),
                            StringConstant::US_default,
                            method -> containing_type -> ContainingPackage() -> PackageName(),
                            method -> containing_type -> ExternalName(),
                            hidden_method -> Header(),
                            StringConstant::US_protected,
                            hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                            hidden_method -> containing_type -> ExternalName());
    }
    else if (method -> ACC_PRIVATE()) // The hidden_method method must have default access as it cannot be private...
    {
         ReportSemError(SemanticError::BAD_ACCESS_METHOD_OVERRIDE_EXTERNALLY,
                        class_declaration -> identifier_token,
                        (class_declaration -> NumInterfaces() > 0
                                            ? class_declaration -> Interface(class_declaration -> NumInterfaces() - 1) -> RightToken()
                                            : (class_declaration -> super_opt ? class_declaration -> super_opt -> RightToken()
                                                                              : class_declaration -> identifier_token)),
                        lex_stream -> NameString(class_declaration -> identifier_token),
                        method -> Header(),
                        StringConstant::US_private,
                        method -> containing_type -> ContainingPackage() -> PackageName(),
                        method -> containing_type -> ExternalName(),
                        hidden_method -> Header(),
                        StringConstant::US_default,
                        hidden_method -> containing_type -> ContainingPackage() -> PackageName(),
                        hidden_method -> containing_type -> ExternalName());
    }

    CheckInheritedMethodThrows(class_declaration, method, hidden_method);

    return;
}


void Semantic::AddInheritedTypes(TypeSymbol *base_type, TypeSymbol *super_type)
{
    ExpandedTypeTable &base_expanded_table = *(base_type -> expanded_type_table),
                      &super_expanded_table = *(super_type -> expanded_type_table);

    for (int j = 0; j < super_expanded_table.symbol_pool.Length(); j++)
    {
        TypeShadowSymbol *type_shadow_symbol = super_expanded_table.symbol_pool[j];
        TypeSymbol *type_symbol = type_shadow_symbol -> type_symbol;

        //
        // Note that since all fields in an interface are implicitly public, all other fields
        // encountered here are enclosed in a type that is a super class of base_type.
        //
        if (type_symbol -> ACC_PUBLIC() ||
            type_symbol -> ACC_PROTECTED() ||
            ((! type_symbol -> ACC_PRIVATE()) &&
             (super_type -> ContainingPackage() == base_type -> ContainingPackage())))
        {
            NameSymbol *name_symbol = type_symbol -> Identity();
            TypeShadowSymbol *shadow = base_expanded_table.FindTypeShadowSymbol(name_symbol);

            if ((! shadow) || (shadow -> type_symbol -> owner != base_type))
            {
                if (! shadow)
                     shadow = base_expanded_table.InsertTypeShadowSymbol(type_symbol);
                else shadow -> AddConflict(type_symbol);

                if (type_symbol -> owner != super_type) // main type doesn't override all other fields? process conflicts.
                {
                    for (int k = 0; k < type_shadow_symbol -> NumConflicts(); k++)
                        shadow -> AddConflict(type_shadow_symbol -> Conflict(k));
                }
            }
            //
            // TODO: maybe? if base_type is a nested type check if a type with the same
            //       name appears in one of the enclosed lexical scopes. If so, add
            //       it to the shadow!
            //
        }
    }

    return;
}


void Semantic::AddInheritedFields(TypeSymbol *base_type, TypeSymbol *super_type)
{
    ExpandedFieldTable &base_expanded_table = *(base_type -> expanded_field_table),
                       &super_expanded_table = *(super_type -> expanded_field_table);

    for (int i = 0; i < super_expanded_table.symbol_pool.Length(); i++)
    {
        VariableShadowSymbol *variable_shadow_symbol = super_expanded_table.symbol_pool[i];
        VariableSymbol *variable_symbol = variable_shadow_symbol -> variable_symbol;
        //
        // Note that since all fields in an interface are implicitly public, all other fields
        // encountered here are enclosed in a type that is a super class of base_type.
        //
        if (variable_symbol -> ACC_PUBLIC() ||
            variable_symbol -> ACC_PROTECTED() ||
            ((! variable_symbol -> ACC_PRIVATE()) &&
             (super_type -> ContainingPackage() == base_type -> ContainingPackage())))
        {
            NameSymbol *name_symbol = variable_symbol -> Identity();
            VariableShadowSymbol *shadow = base_expanded_table.FindVariableShadowSymbol(name_symbol);

            if ((! shadow) || (shadow -> variable_symbol -> owner != base_type))
            {
                if (! shadow)
                     shadow = base_expanded_table.InsertVariableShadowSymbol(variable_symbol);
                else shadow -> AddConflict(variable_symbol);

                if (variable_symbol -> owner != super_type) // main variable doesn't override all other fields? process conflicts.
                {
                    for (int k = 0; k < variable_shadow_symbol -> NumConflicts(); k++)
                        shadow -> AddConflict(variable_shadow_symbol -> Conflict(k));
                }
            }
        }
    }

    return;
}


void Semantic::AddInheritedMethods(TypeSymbol *base_type, TypeSymbol *super_type, LexStream::TokenIndex tok)
{
    ExpandedMethodTable &base_expanded_table = *(base_type -> expanded_method_table),
                        &super_expanded_table = *(super_type -> expanded_method_table);

    for (int k = 0; k < super_expanded_table.symbol_pool.Length(); k++)
    {
        MethodShadowSymbol *method_shadow_symbol = super_expanded_table.symbol_pool[k];
        MethodSymbol *method = method_shadow_symbol -> method_symbol;

        //
        // Note that since all methods in an interface are implicitly
        // public, all other methods encountered here are enclosed in a
        // type that is a super class of base_type.
        //
        if (method -> ACC_PUBLIC() ||
            method -> ACC_PROTECTED() ||
            ((! method -> ACC_PRIVATE()) &&
             (super_type -> ContainingPackage() == base_type -> ContainingPackage())))
        {
            MethodShadowSymbol *base_method_shadow =
                  base_expanded_table.FindMethodShadowSymbol(method -> Identity());
            if (! base_method_shadow)
                 base_expanded_table.InsertMethodShadowSymbol(method);
            else
            {
                MethodShadowSymbol *shadow = base_expanded_table.FindOverloadMethodShadow(method, (Semantic *) this, tok);

                if (! shadow)
                     base_expanded_table.Overload(base_method_shadow, method);
                else
                {
                    shadow -> AddConflict(method);

                    //
                    // If main method in question does not override all other methods,
                    // add all other conflicting methods.
                    //
                    if (method -> containing_type != super_type)
                    {
                        for (int i = 0; i < method_shadow_symbol -> NumConflicts(); i++)
                            shadow -> AddConflict(method_shadow_symbol -> Conflict(i));
                    }
                }
            }
        }
        else if (! (method -> ACC_PRIVATE() || method -> IsSynthetic())) // Not a method with default access from another package?
        {
            MethodShadowSymbol *base_method_shadow = base_expanded_table.FindMethodShadowSymbol(method -> Identity());

            if (base_method_shadow)
            {
                MethodShadowSymbol *shadow = base_expanded_table.FindOverloadMethodShadow(method, (Semantic *) this, tok);

                if (shadow)
                {
                    LexStream::TokenIndex left_tok,
                                          right_tok;

                    if (ThisType() == base_type)
                    {
                        AstMethodDeclaration *method_declaration = (AstMethodDeclaration *)
                                                                    shadow -> method_symbol -> method_or_constructor_declaration;
                        AstMethodDeclarator *method_declarator = method_declaration -> method_declarator;

                        left_tok = method_declarator -> LeftToken();
                        right_tok = method_declarator -> RightToken();
                    }
                    else
                    {
                        AstInterfaceDeclaration *interface_declaration = ThisType() -> declaration -> InterfaceDeclarationCast();
                        AstClassDeclaration *class_declaration = ThisType() -> declaration -> ClassDeclarationCast();
                        if (interface_declaration)
                        {
                            left_tok = right_tok = interface_declaration -> identifier_token;
                        }
                        else if (class_declaration)
                        {
                            left_tok = right_tok = class_declaration -> identifier_token;
                        }
                        else
                        {
                            AstClassInstanceCreationExpression *class_creation = ThisType() -> declaration
                                                                                            -> ClassInstanceCreationExpressionCast();

                            assert(class_creation);

                            left_tok = class_creation -> class_type -> LeftToken();
                            right_tok = class_creation -> class_type -> RightToken();
                        }
                    }

                    if (! method -> IsTyped())
                        method -> ProcessMethodSignature((Semantic *) this, tok);

                    ReportSemError(SemanticError::DEFAULT_METHOD_NOT_OVERRIDDEN,
                                   left_tok,
                                   right_tok,
                                   method -> Header(),
                                   base_type -> ContainingPackage() -> PackageName(),
                                   base_type -> ExternalName(),
                                   super_type -> ContainingPackage() -> PackageName(),
                                   super_type -> ExternalName());
                }
            }
        }
    }

    return;
}


void Semantic::ComputeTypesClosure(TypeSymbol *type, LexStream::TokenIndex tok)
{
    type -> expanded_type_table = new ExpandedTypeTable();

    TypeSymbol *super_class = type -> super;
    if (super_class)
    {
        if (! super_class -> expanded_type_table)
            ComputeTypesClosure(super_class, tok);
    }

    for (int j = 0; j < type -> NumInterfaces(); j++)
    {
        TypeSymbol *interf = type -> Interface(j);
        if (! interf -> expanded_type_table)
            ComputeTypesClosure(interf, tok);
    }

    if (! type -> NestedTypesProcessed())
        type -> ProcessNestedTypeSignatures((Semantic *) this, tok);
    for (int i = 0; i < type -> NumTypeSymbols(); i++)
    {
        if (! type -> TypeSym(i) -> Bad())
            type -> expanded_type_table -> InsertTypeShadowSymbol(type -> TypeSym(i));
    }
    if (super_class)
        AddInheritedTypes(type, super_class);
    for (int k = 0; k < type -> NumInterfaces(); k++)
        AddInheritedTypes(type, type -> Interface(k));
    type -> expanded_type_table -> CompressSpace();

    return;
}


void Semantic::ComputeFieldsClosure(TypeSymbol *type, LexStream::TokenIndex tok)
{
    type -> expanded_field_table = new ExpandedFieldTable();

    TypeSymbol *super_class = type -> super;
    if (super_class)
    {
        if (! super_class -> expanded_field_table)
            ComputeFieldsClosure(super_class, tok);
    }

    for (int j = 0; j < type -> NumInterfaces(); j++)
    {
        TypeSymbol *interf = type -> Interface(j);
        if (! interf -> expanded_field_table)
            ComputeFieldsClosure(interf, tok);
    }

    assert(type -> FieldMembersProcessed());

    for (int i = 0; i < type -> NumVariableSymbols(); i++)
    {
        VariableSymbol *variable = type -> VariableSym(i);
        type -> expanded_field_table -> InsertVariableShadowSymbol(variable);
    }

    //
    // As the type Object which is the super type of all interfaces does
    // not contain any field declarations, we don't have to do any special
    // check here as we have to when computing method closures.
    //
    if (super_class)
        AddInheritedFields(type, super_class);
    for (int k = 0; k < type -> NumInterfaces(); k++)
        AddInheritedFields(type, type -> Interface(k));
    type -> expanded_field_table -> CompressSpace();

    return;
}


void Semantic::ComputeMethodsClosure(TypeSymbol *type, LexStream::TokenIndex tok)
{
    type -> expanded_method_table = new ExpandedMethodTable();

    TypeSymbol *super_class = type -> super;
    if (super_class)
    {
        if (! super_class -> expanded_method_table)
            ComputeMethodsClosure(super_class, tok);
    }

    for (int j = 0; j < type -> NumInterfaces(); j++)
    {
        TypeSymbol *interf = type -> Interface(j);

        if (! interf -> expanded_method_table)
            ComputeMethodsClosure(interf, tok);
    }

    assert(type -> MethodMembersProcessed());

    for (int i = 0; i < type -> NumMethodSymbols(); i++)
    {
        MethodSymbol *method = type -> MethodSym(i);
        //
        // If the method in question is neither a constructor nor an
        // initializer, then ...
        //        if (method -> Identity() != control.init_name_symbol &&
        //            method -> Identity() != control.block_init_name_symbol &&
        //            method -> Identity() != control.clinit_name_symbol)
        //
        if (*(method -> Name()) != U_LESS)
        {
            type -> expanded_method_table -> Overload(method);
        }
    }
    if (super_class && (! type -> ACC_INTERFACE()))
        AddInheritedMethods(type, super_class, tok);
    for (int k = 0; k < type -> NumInterfaces(); k++)
        AddInheritedMethods(type, type -> Interface(k), tok);
    if (type -> ACC_INTERFACE()) // the super class is Object
        AddInheritedMethods(type, control.Object(), tok);
    type -> expanded_method_table -> CompressSpace();

    return;
}


void Semantic::ProcessFormalParameters(BlockSymbol *block, AstMethodDeclarator *method_declarator)
{
    for (int i = 0; i < method_declarator -> NumFormalParameters(); i++)
    {
        AstFormalParameter *parameter = method_declarator -> FormalParameter(i);
        AstArrayType *array_type = parameter -> type -> ArrayTypeCast();
        Ast *actual_type = (array_type ? array_type -> type : parameter -> type);

        if ((! control.option.one_one) && parameter -> NumParameterModifiers() > 0)
        {
            ReportSemError(SemanticError::ONE_ONE_FEATURE,
                           parameter -> ParameterModifier(0) -> LeftToken(),
                           parameter -> ParameterModifier(0) -> RightToken());
        }
        AccessFlags access_flags = ProcessFormalModifiers(parameter);

        AstPrimitiveType *primitive_type = actual_type -> PrimitiveTypeCast();
        TypeSymbol *parm_type = (primitive_type ? FindPrimitiveType(primitive_type) : MustFindType(actual_type));

        AstVariableDeclaratorId *name = parameter -> formal_declarator -> variable_declarator_name;
        NameSymbol *name_symbol = lex_stream -> NameSymbol(name -> identifier_token);
        VariableSymbol *symbol = block -> FindVariableSymbol(name_symbol);
        if (symbol)
        {
            ReportSemError(SemanticError::DUPLICATE_FORMAL_PARAMETER,
                           name -> identifier_token,
                           name -> identifier_token,
                           name_symbol -> Name());
        }
        else symbol = block -> InsertVariableSymbol(name_symbol);

        int num_dimensions = (array_type ? array_type -> NumBrackets() : 0) + name -> NumBrackets();
        if (num_dimensions == 0)
             symbol -> SetType(parm_type);
        else symbol -> SetType(parm_type -> GetArrayType((Semantic *) this, num_dimensions));
        symbol -> SetFlags(access_flags);
        symbol -> MarkComplete();

        parameter -> formal_declarator -> symbol = symbol;
    }

    return;
}


void Semantic::ProcessMethodDeclaration(AstMethodDeclaration *method_declaration)
{
    TypeSymbol *this_type = ThisType();
    AccessFlags access_flags = (this_type -> ACC_INTERFACE()
                                           ? ProcessAbstractMethodModifiers(method_declaration)
                                           : ProcessMethodModifiers(method_declaration));

    //
    // TODO: File Query Sun on that one. We no longer explicitly mark such methods as final
    //       as it appears that some tools expect these methods to remain unmarked.
    //
    //
    // A private method and all methods declared in a final class are implicitly final.
    //
    // if (access_flags.ACC_PRIVATE() || this_type -> ACC_FINAL())
    //    access_flags.SetACC_FINAL();
    //

    //
    // A method enclosed in an inner type may not be declared static.
    //
    if (access_flags.ACC_STATIC() && this_type -> IsInner())
    {
        AstModifier *modifier = NULL;
        for (int i = 0; i < method_declaration -> NumMethodModifiers(); i++)
        {
            if (method_declaration -> MethodModifier(i) -> kind == Ast::STATIC)
                modifier = method_declaration -> MethodModifier(i);
        }

        assert(modifier);

        ReportSemError(SemanticError::STATIC_METHOD_IN_INNER_CLASS,
                       modifier -> modifier_kind_token,
                       modifier -> modifier_kind_token);
    }

    //
    //
    //
    AstArrayType *array_type = method_declaration -> type -> ArrayTypeCast();
    Ast *actual_type = (array_type ? array_type -> type : method_declaration -> type);
    AstPrimitiveType *primitive_type = actual_type -> PrimitiveTypeCast();
    TypeSymbol *method_type = (primitive_type ? FindPrimitiveType(primitive_type) : MustFindType(actual_type));

    AstMethodDeclarator *method_declarator = method_declaration -> method_declarator;
    if (method_declarator -> NumBrackets() > 0)
    {
        if (method_type == control.void_type)
             ReportSemError(SemanticError::VOID_ARRAY,
                            method_declaration -> type -> LeftToken(),
                            method_declarator -> RightToken());
        else ReportSemError(SemanticError::OBSOLESCENT_BRACKETS,
                            method_declarator -> LeftToken(),
                            method_declarator -> RightToken());
    }

    NameSymbol *name_symbol = lex_stream -> NameSymbol(method_declarator -> identifier_token);

    if (name_symbol == this_type -> Identity())
    {
        ReportSemError(SemanticError::METHOD_WITH_CONSTRUCTOR_NAME,
                       method_declaration -> type -> LeftToken(),
                       method_declarator -> identifier_token,
                       name_symbol -> Name());
    }

    //
    // As the body of the method may not have been parsed yet, we estimate a size
    // for its symbol table based on the number of lines in the body + a margin for one-liners.
    //
    AstBlock *block = method_declaration -> method_body -> BlockCast();
    BlockSymbol *block_symbol = new BlockSymbol(method_declarator -> NumFormalParameters());
    block_symbol -> max_variable_index = (access_flags.ACC_STATIC() ? 0 : 1);
    ProcessFormalParameters(block_symbol, method_declarator);

    MethodSymbol *method = this_type -> FindMethodSymbol(name_symbol);

    if (! method)
        method = this_type -> InsertMethodSymbol(name_symbol);
    else
    {
        if (this_type -> FindOverloadMethod(method, method_declarator))
        {
            ReportSemError(SemanticError::DUPLICATE_METHOD,
                           method_declarator -> LeftToken(),
                           method_declarator -> RightToken(),
                           name_symbol -> Name(),
                           this_type -> Name());
            delete block_symbol;
            return;
        }

        method = this_type -> Overload(method);
    }

    int num_dimensions = (method_type == control.void_type
                                       ? 0
                                       : (array_type ? array_type -> NumBrackets() : 0) + method_declarator -> NumBrackets());
    if (num_dimensions == 0)
         method -> SetType(method_type);
    else method -> SetType(method_type -> GetArrayType((Semantic *) this, num_dimensions));

    //
    // if the method is not static, leave a slot for the "this" pointer.
    //
    method -> SetFlags(access_flags);
    method -> SetContainingType(this_type);
    method -> SetBlockSymbol(block_symbol);
    method -> method_or_constructor_declaration = method_declaration;
    for (int i = 0; i < method_declarator -> NumFormalParameters(); i++)
    {
        AstVariableDeclarator *formal_declarator = method_declarator -> FormalParameter(i) -> formal_declarator;
        VariableSymbol *symbol = formal_declarator -> symbol;

        symbol -> SetOwner(method);
        symbol -> SetLocalVariableIndex(block_symbol -> max_variable_index++);
        if (control.IsDoubleWordType(symbol -> Type()))
            block_symbol -> max_variable_index++;
        symbol -> declarator = formal_declarator;
        method -> AddFormalParameter(symbol);
    }
    method -> SetSignature(control);

    for (int k = 0; k < method_declaration -> NumThrows(); k++)
    {
        AstExpression *throw_expression = method_declaration -> Throw(k);
        TypeSymbol *throw_type = MustFindType(throw_expression);
        throw_expression -> symbol = throw_type;
        method -> AddThrows(throw_type);
    }

    method_declaration -> method_symbol = method; // save for processing bodies later.

    if (method -> ACC_ABSTRACT() && (! this_type -> ACC_ABSTRACT()))
    {
        ReportSemError(SemanticError::NON_ABSTRACT_TYPE_CONTAINS_ABSTRACT_METHOD,
                       method_declaration -> LeftToken(),
                       method_declarator -> identifier_token,
                       name_symbol -> Name(),
                       this_type -> Name());
    }

    if (lex_stream -> IsDeprecated(lex_stream -> Previous(method_declaration -> LeftToken())))
        method -> MarkDeprecated();

    return;
}


//
// Search for simple identifier which is supposed to be a type.
// If it is not found, issue an error message.
//
TypeSymbol *Semantic::FindPrimitiveType(AstPrimitiveType *primitive_type)
{
    switch(primitive_type -> kind)
    {
        case Ast::INT:
             return control.int_type;
        case Ast::DOUBLE:
             return control.double_type;
        case Ast::CHAR:
             return control.char_type;
        case Ast::LONG:
             return control.long_type;
        case Ast::FLOAT:
             return control.float_type;
        case Ast::BYTE:
             return control.byte_type;
        case Ast::SHORT:
             return control.short_type;
        case Ast::BOOLEAN:
             return control.boolean_type;
        default:
             break;
    }

    return control.void_type;
}


TypeSymbol *Semantic::ImportType(LexStream::TokenIndex identifier_token, NameSymbol *name_symbol)
{
    TypeSymbol *type = NULL;
    PackageSymbol *package = NULL;
    FileSymbol *file_symbol = NULL;

    for (int i = 0; i < import_on_demand_packages.Length(); i++)
    {
        PackageSymbol *import_package = import_on_demand_packages[i] -> PackageCast();

        if (import_package)
        {
            FileSymbol *symbol = Control::GetFile(control, import_package, name_symbol);
            if (symbol)
            {
                if (! package)
                {
                    file_symbol = symbol;
                    package = import_package;
                }
                else
                {
                    ReportSemError(SemanticError::DUPLICATE_ON_DEMAND_IMPORT,
                                   identifier_token,
                                   identifier_token,
                                   name_symbol -> Name(),
                                   package -> PackageName(),
                                   import_package -> PackageName());
                }
            }
        }
        else
        {
            TypeSymbol *import_type = (TypeSymbol *) import_on_demand_packages[i];

            if (! import_type -> expanded_type_table)
                ComputeTypesClosure(import_type, identifier_token);
            TypeShadowSymbol *type_shadow_symbol = import_type -> expanded_type_table -> FindTypeShadowSymbol(name_symbol);
            if (type_shadow_symbol)
                type = FindTypeInShadow(type_shadow_symbol, identifier_token);
        }
    }

    if (! type)
    {
        if (package)
        {
            type = package -> FindTypeSymbol(name_symbol);
            if (! type)
                 type = ReadType(file_symbol, package, name_symbol, identifier_token);
            else if (type -> SourcePending())
                 control.ProcessHeaders(type -> file_symbol);
        }
    }

    if (type && (! (type -> ACC_PUBLIC() || type -> ContainingPackage() == this_package)))
        ReportTypeInaccessible(identifier_token, identifier_token, type);

    return type;
}


TypeSymbol *Semantic::FindType(LexStream::TokenIndex identifier_token)
{
    TypeSymbol *type;

    NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);

    SemanticEnvironment *env;
    for (env = state_stack.Top(); env; env = env -> previous)
    {
        type = env -> symbol_table.FindTypeSymbol(name_symbol);
        if (type)
            break;

        type = env -> Type();
        if (name_symbol == type -> Identity()) // Recall that a type may not have the same name as one of its enclosing types.
            break;

        if (! type -> expanded_type_table)
            ComputeTypesClosure(type, identifier_token);
        TypeShadowSymbol *type_shadow_symbol = type -> expanded_type_table -> FindTypeShadowSymbol(name_symbol);
        if (type_shadow_symbol)
        {
            type = FindTypeInShadow(type_shadow_symbol, identifier_token);
            break;
        }
    }

    if (env) // The type was found in some enclosing environment?
    {
        //
        // If the type is an inherited type, make sure that there is not a
        // type of the same name within an enclosing lexical scope.
        //
        if (type -> owner -> TypeCast() && type -> owner != env -> Type())
        {
            for (SemanticEnvironment *env2 = env -> previous; env2; env2 = env2 -> previous)
            {
                TypeSymbol *outer_type = env2 -> symbol_table.FindTypeSymbol(name_symbol); // check local type
                if (! outer_type) // if local type not found, check inner type...
                {
                    if (! env2 -> Type() -> expanded_type_table)
                        ComputeTypesClosure(env2 -> Type(), identifier_token);
                    TypeShadowSymbol *type_shadow_symbol = env2 -> Type() -> expanded_type_table
                                                                          -> FindTypeShadowSymbol(name_symbol);
                    if (type_shadow_symbol)
                        outer_type = FindTypeInShadow(type_shadow_symbol, identifier_token);
                }

                //
                // If a different type of the same name was found in an enclosing scope.
                //
                if (outer_type && outer_type != type)
                {
                    MethodSymbol *method = outer_type -> owner -> MethodCast();

                    if (method)
                    {
                        ReportSemError(SemanticError::INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_LOCAL,
                                       identifier_token,
                                       identifier_token,
                                       lex_stream -> NameString(identifier_token),
                                       env -> Type() -> ContainingPackage() -> PackageName(),
                                       env -> Type() -> ExternalName(),
                                       method -> Name());
                        break;
                    }
                    else
                    {
                        ReportSemError(SemanticError::INHERITANCE_AND_LEXICAL_SCOPING_CONFLICT_WITH_MEMBER,
                                       identifier_token,
                                       identifier_token,
                                       lex_stream -> NameString(identifier_token),
                                       env -> Type() -> ContainingPackage() -> PackageName(),
                                       env -> Type() -> ExternalName(),
                                       env2 -> Type() -> ContainingPackage() -> PackageName(),
                                       env2 -> Type() -> ExternalName());
                        break;
                    }
                }
            }
        }

        return type;
    }

    //
    // check whether or not the type is in the current compilation unit
    // either because it was declared as a class or interface or it was
    // imported by a single-type-import declaration.
    //
    for (int i = 0; i < single_type_imports.Length(); i++)
    {
        type = single_type_imports[i];
        if (name_symbol == type -> Identity())
            return type;
    }

    //
    // If a package was specified in this file (the one we are compiling),
    // we look for the type requested inside the package in question.
    // Otherwise, we look for the type requested in the unnamed package.
    // If we don't find the type or we find a bad type we check to see
    // if the type can be imported.
    //
    PackageSymbol *package = (compilation_unit -> package_declaration_opt ? this_package : control.unnamed_package);
    type = FindSimpleNameType(package, identifier_token);
    TypeSymbol *imported_type = ((! type ) || type -> Bad() ? ImportType(identifier_token, name_symbol) : (TypeSymbol *) NULL);

    //
    // If a valid type can be imported on demand, chose that type.
    // Otherwise, if a type was found at all, do some final checks on it.
    //
    // Note that a type T contained in a package P is always accessible to all other
    // types contained in P. I.e., we do not need to perform access check for type...
    //
    //
    if (imported_type)
        type = imported_type;
    else if (type)
    {
        //
        // If a type T was specified in a source file that is not called T.java
        // but X.java (where X != T) and we are not currently compiling file X,
        // issue a warning to alert the user that in some circumstances, this
        // may not be visible. (i.e., if the file X has not yet been compiled,
        // then T is invisile as the compiler will only look for T in T.java.)
        //
        FileSymbol *file_symbol = type -> file_symbol;
        if (file_symbol && (type -> Identity() != file_symbol -> Identity()) && (file_symbol != this -> source_file_symbol))
        {
            ReportSemError(SemanticError::REFERENCE_TO_TYPE_IN_MISMATCHED_FILE,
                           identifier_token,
                           identifier_token,
                           type -> Name(),
                           file_symbol -> Name());
        }
    }

    return type;
}


//
//
//
TypeSymbol *Semantic::MustFindType(Ast *name)
{
    TypeSymbol *type;
    LexStream::TokenIndex identifier_token;

    AstSimpleName *simple_name = name -> SimpleNameCast();
    if (simple_name)
    {
        identifier_token = simple_name -> identifier_token;
        type = FindType(identifier_token);

        //
        // If the type was not found, try to read it again to generate an appropriate error message.
        //
        if (! type)
        {
            NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);
            PackageSymbol *package = (compilation_unit -> package_declaration_opt ? this_package : control.unnamed_package);
            FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);

            //
            // If there is no file associated with the name of the type then the type does
            // not exist. Before invoking ReadType to issue a "type not found" error message,
            // check whether or not the user is not attempting to access an inaccessible
            // private type.
            //
            if ((! file_symbol) && state_stack.Size() > 0)
            {
                for (TypeSymbol *super_type = ThisType() -> super; super_type; super_type = super_type -> super)
                {
                    assert(super_type -> expanded_type_table);

                    TypeShadowSymbol *type_shadow_symbol = super_type -> expanded_type_table -> FindTypeShadowSymbol(name_symbol);
                    if (type_shadow_symbol)
                    {
                        type = FindTypeInShadow(type_shadow_symbol, identifier_token);
                        break;
                    }
                }
            }

            if (type)
                 ReportTypeInaccessible(name -> LeftToken(), name -> RightToken(), type);
            else type = ReadType(file_symbol, package, name_symbol, identifier_token);
        }
        else
        {
             TypeAccessCheck(name, type);
        }
    }
    else
    {
        AstFieldAccess *field_access = name -> FieldAccessCast();

        assert(field_access);

        identifier_token = field_access -> identifier_token;

        ProcessPackageOrType(field_access -> base);
        Symbol *symbol = field_access -> base -> symbol;

        type = symbol -> TypeCast();
        if (type)
        {
            TypeNestAccessCheck(field_access -> base);
            type = MustFindNestedType(type, field_access);
        }
        else
        {
            PackageSymbol *package = symbol -> PackageCast();
            if (package -> directory.Length() == 0)
            {
                ReportSemError(SemanticError::PACKAGE_NOT_FOUND,
                               field_access -> base -> LeftToken(),
                               field_access -> base -> RightToken(),
                               package -> PackageName());
            }
            NameSymbol *name_symbol = lex_stream -> NameSymbol(identifier_token);
            type = package -> FindTypeSymbol(name_symbol);
            if (! type)
            {
                FileSymbol *file_symbol = Control::GetFile(control, package, name_symbol);
                type = ReadType(file_symbol, package, name_symbol, identifier_token);
            }
            else
            {
                if (type -> SourcePending())
                    control.ProcessHeaders(type -> file_symbol);
                TypeAccessCheck(name, type);
            }
        }
    }

    //
    // Establish a dependence from the base_type to a type that it "must find".
    // Note that the only time an environment is not available is when were are
    // processing the type header of an outermost type.
    //
    if (state_stack.Size() > 0)
        AddDependence(ThisType(), type, identifier_token);

    //
    //
    //
    if (control.option.deprecation && type -> IsDeprecated() && type -> file_symbol != source_file_symbol)
    {
        ReportSemError(SemanticError::DEPRECATED_TYPE,
                       name -> LeftToken(),
                       name -> RightToken(),
                       type -> ContainingPackage() -> PackageName(),
                       type -> ExternalName());
    }

    return type;
}


void Semantic::ProcessInterface(TypeSymbol *base_type, AstExpression *name)
{
    TypeSymbol *interf = MustFindType(name);

    assert(! interf -> SourcePending());

    if (! interf -> ACC_INTERFACE())
    {
        if (! interf -> Bad())
        {
            ReportSemError(SemanticError::NOT_AN_INTERFACE,
                           name -> LeftToken(),
                           name -> RightToken(),
                           interf -> ContainingPackage() -> PackageName(),
                           interf -> ExternalName());
        }

        name -> symbol = NULL;
    }
    else
    {
        for (int k = 0; k < base_type -> NumInterfaces(); k++)
        {
            if (base_type -> Interface(k) == interf)
            {
                ReportSemError(SemanticError::DUPLICATE_INTERFACE,
                               name -> LeftToken(),
                               name -> RightToken(),
                               interf -> ContainingPackage() -> PackageName(),
                               interf -> ExternalName(),
                               base_type -> ExternalName());

                name -> symbol = NULL;

                return;
            }
        }

        name -> symbol = interf; // save type  name in ast.
        base_type -> AddInterface(interf);
    }

    return;
}


void Semantic::InitializeVariable(AstFieldDeclaration *field_declaration, Tuple<VariableSymbol *> &finals)
{
    ThisMethod() = NULL;

    for (int i = 0; i < field_declaration -> NumVariableDeclarators(); i++)
    {
        AstVariableDeclarator *variable_declarator = field_declaration -> VariableDeclarator(i);

        if (ThisVariable() = variable_declarator -> symbol)
        {
            if (variable_declarator -> variable_initializer_opt)
            {
                variable_declarator -> pending = true;

                int start_num_errors = NumErrors();

                ProcessVariableInitializer(variable_declarator);
                if (NumErrors() == start_num_errors)
                     DefiniteVariableInitializer(variable_declarator, finals);
                else variable_declarator -> symbol -> MarkDefinitelyAssigned(); // assume variable is assigned

                variable_declarator -> pending = false;
            }
            ThisVariable() -> MarkComplete();
        }
    }

    return;
}


inline void Semantic::ProcessInitializer(AstBlock *initializer_block, AstBlock *block_body,
                                         MethodSymbol *init_method, Tuple<VariableSymbol *> &finals)
{
    ThisVariable() = NULL;
    ThisMethod() = init_method;

    LocalBlockStack().Push(initializer_block);
    LocalSymbolTable().Push(init_method -> block_symbol -> Table());

    int start_num_errors = NumErrors();

    AstConstructorBlock *constructor_block = block_body -> ConstructorBlockCast();
    if (constructor_block)
    {
        assert(constructor_block -> explicit_constructor_invocation_opt);

        ReportSemError(SemanticError::MISPLACED_EXPLICIT_CONSTRUCTOR_INVOCATION,
                       constructor_block -> explicit_constructor_invocation_opt -> LeftToken(),
                       constructor_block -> explicit_constructor_invocation_opt -> RightToken());
    }

    ProcessBlock(block_body);

    if (NumErrors() == start_num_errors)
        DefiniteBlockInitializer(block_body, LocalBlockStack().max_size, finals);

    //
    // If an enclosed block has a higher max_variable_index than the current block,
    // update max_variable_index in the current_block, accordingly.
    //
    if (init_method -> block_symbol -> max_variable_index < LocalBlockStack().TopMaxEnclosedVariableIndex())
        init_method -> block_symbol -> max_variable_index = LocalBlockStack().TopMaxEnclosedVariableIndex();

    LocalBlockStack().Pop();
    LocalSymbolTable().Pop();

    return;
}


bool Semantic::NeedsInitializationMethod(AstFieldDeclaration *field_declaration)
{
    //
    // We need a static constructor-initializer if we encounter at least one class
    // variable that is declared with an initializer is not a constant expression.
    //
    for (int i = 0; i < field_declaration -> NumVariableDeclarators(); i++)
    {
        AstVariableDeclarator *variable_declarator = field_declaration -> VariableDeclarator(i);
        if (variable_declarator -> variable_initializer_opt)
        {
            AstExpression *init = variable_declarator -> variable_initializer_opt -> ExpressionCast();
            if (! (init && init -> IsConstant()))
                return true;

            //
            // TODO: there seems to be a contradiction between the language spec and the VM spec.
            // The language spec seems to require that a variable be initialized (in the class file)
            // with a "ConstantValue" only if it is static. The VM spec, on the other hand, states
            // that a static need not be final to be initialized with a ConstantValue.
            // As of now, we are following the language spec - ergo, this extra test.
            //
            if (variable_declarator -> symbol && (! variable_declarator -> symbol -> ACC_FINAL()))
                return true;
        }
    }

    return false;
}


void Semantic::ProcessStaticInitializers(AstClassBody *class_body)
{
    if (class_body -> NumStaticInitializers() > 0 || class_body -> NumClassVariables() > 0)
    {
        TypeSymbol *this_type = ThisType();

        BlockSymbol *block_symbol = new BlockSymbol(0); // the symbol table associated with this block will contain no element
        block_symbol -> max_variable_index = 0;         // if we need this, it will be associated with a static function.

        AstBlock *initializer_block = compilation_unit -> ast_pool -> GenBlock();
        initializer_block -> left_brace_token  = class_body -> left_brace_token;
        initializer_block -> right_brace_token = class_body -> left_brace_token;
        initializer_block -> block_symbol = block_symbol;
        initializer_block -> nesting_level = LocalBlockStack().Size();

        LocalBlockStack().max_size = 0;

        //
        // If the class being processed contains any static
        // initializer then, it needs a static initializer function?
        //
        MethodSymbol *init_method = NULL;
        if (class_body -> NumStaticInitializers() > 0)
        {
              init_method = this_type -> InsertMethodSymbol(control.clinit_name_symbol);

              init_method -> SetType(control.void_type);
              init_method -> SetACC_FINAL();
              init_method -> SetACC_STATIC();
              init_method -> SetContainingType(this_type);
              init_method -> SetBlockSymbol(block_symbol);
              init_method -> SetSignature(control);
        }

        //
        // Compute the set of final variables declared by the user in this type.
        //
        Tuple<VariableSymbol *> finals(this_type -> NumVariableSymbols());
        for (int i = 0; i < this_type -> NumVariableSymbols(); i++)
        {
            VariableSymbol *variable_symbol = this_type -> VariableSym(i);
            if (variable_symbol -> ACC_FINAL())
                finals.Next() = variable_symbol;
        }

        //
        // The static initializers and class variable initializers are executed in textual order... 8.5
        //
        int j,
            k;
        for (j = 0, k = 0; j < class_body -> NumClassVariables() && k < class_body -> NumStaticInitializers(); )
        {
            //
            // Note that since there cannot be any overlap in the
            // declarations, we can use either location position.
            // The RightToken of the field declaration is used because it
            // does not have to be computed (it is the terminating semicolon).
            // Similarly, the LeftToken of the static initializer is used
            // because it is the initial "static" keyword that marked the initializer.
            //
            //    if (class_body -> InstanceVariables(j) -> RightToken() < class_body -> Block(k) -> LeftToken())
            //
            if (class_body -> ClassVariable(j) -> semicolon_token < class_body -> StaticInitializer(k) -> static_token)
            {
                InitializeVariable(class_body -> ClassVariable(j), finals);
                j++;
            }
            else
            {
                AstBlock *block_body = class_body -> StaticInitializer(k) -> block;

                //
                // The first block that is the body of an initializer is reachable
                // A subsequent block is reachable iff its predecessor can complete
                // normally
                //
                block_body -> is_reachable = (k == 0
                                                 ? true
                                                 : class_body -> StaticInitializer(k - 1) -> block -> can_complete_normally);

                ProcessInitializer(initializer_block, block_body, init_method, finals);
                k++;
            }
        }
        for (; j < class_body -> NumClassVariables(); j++)
            InitializeVariable(class_body -> ClassVariable(j), finals);
        for (; k < class_body -> NumStaticInitializers(); k++)
        {
            AstBlock *block_body = class_body -> StaticInitializer(k) -> block;

            //
            // The first block that is the body of an initializer is reachable
            // A subsequent block is reachable iff its predecessor can complete
            // normally
            //
            block_body -> is_reachable = (k == 0
                                             ? true
                                             : class_body -> StaticInitializer(k - 1) -> block -> can_complete_normally);

            ProcessInitializer(initializer_block, block_body, init_method, finals);
        }

        //
        // Check that each static final variables has been initialized by now.
        // If not, issue an error and assume it is.
        //
        for (int l = 0; l < finals.Length(); l++)
        {
            if (finals[l] -> ACC_STATIC() && (! finals[l] -> IsDefinitelyAssigned()))
            {
                ReportSemError(SemanticError::UNINITIALIZED_STATIC_FINAL_VARIABLE,
                               finals[l] -> declarator -> LeftToken(),
                               finals[l] -> declarator -> RightToken());
                finals[l] -> MarkDefinitelyAssigned();
            }
        }

        //
        // If we had not yet defined a static initializer function and
        // there exists a static variable in the class that is not initialized
        // with a constant then define one now.
        //
        if (! init_method)
        {
            //
            // Check whether or not we need a static initializer method. We need a static
            // constructor-initializer iff:
            //
            //     . we have at least one static initializer (this case is processed above)
            //     . we have at least one static variable that is not initialized with a
            //       constant expression.
            //
            for (int i = 0; i < class_body -> NumClassVariables(); i++)
            {
                if (NeedsInitializationMethod(class_body -> ClassVariable(i)))
                {
                    init_method = this_type -> InsertMethodSymbol(control.clinit_name_symbol);

                    init_method -> SetType(control.void_type);
                    init_method -> SetACC_FINAL();
                    init_method -> SetACC_STATIC();
                    init_method -> SetContainingType(this_type);
                    init_method -> SetBlockSymbol(block_symbol);
                    init_method -> SetSignature(control);

                    break;
                }
            }
        }

        //
        // If an initialization method has been defined, update its max_block_depth.
        // Otherwise, deallocate the block symbol as nobody would be pointing to it
        // after this point.
        //
        if (init_method)
        {
             init_method -> max_block_depth = LocalBlockStack().max_size;
             this_type -> static_initializer_method = init_method;

             block_symbol -> CompressSpace(); // space optimization
        }
        else delete block_symbol;

// STG:
//        delete initializer_block;
    }

    return;
}


void Semantic::ProcessBlockInitializers(AstClassBody *class_body)
{
    TypeSymbol *this_type = ThisType();

    //
    // Compute the set of final variables declared by the user in this type.
    //
    Tuple<VariableSymbol *> finals(this_type -> NumVariableSymbols());
    for (int i = 0; i < this_type -> NumVariableSymbols(); i++)
    {
        VariableSymbol *variable_symbol = this_type -> VariableSym(i);
        if (variable_symbol -> ACC_FINAL())
            finals.Next() = variable_symbol;
    }

    //
    // Initialization code is executed by every constructor, just after the
    // superclass constructor is called, in textual order along with any
    // instance variable initializations.
    //
    if (class_body -> NumBlocks() == 0)
    {
        for (int j = 0; j < class_body -> NumInstanceVariables(); j++)
            InitializeVariable(class_body -> InstanceVariable(j), finals);
    }
    else
    {
        MethodSymbol *init_method = this_type -> InsertMethodSymbol(control.block_init_name_symbol);
        init_method -> SetType(control.void_type);
        init_method -> SetACC_FINAL();
        init_method -> SetACC_PRIVATE();
        init_method -> SetContainingType(this_type);
        init_method -> SetBlockSymbol(new BlockSymbol(0));  // the symbol table associated with this block will contain no element
        init_method -> block_symbol -> max_variable_index = 1;
        init_method -> SetSignature(control);

        //
        // Compute the set of constructors whose bodies do not start with
        // an explicit this call.
        //
        for (int i = 0; i < class_body -> NumConstructors(); i++)
        {
            MethodSymbol *method = class_body -> Constructor(i) -> constructor_symbol;
            if (method)
            {
                AstConstructorBlock *constructor_block = class_body -> Constructor(i) -> constructor_body;
                if (! (constructor_block -> explicit_constructor_invocation_opt &&
                       constructor_block -> explicit_constructor_invocation_opt -> ThisCallCast()))
                init_method -> AddInitializerConstructor(method);
            }
        }

        this_type -> block_initializer_method = init_method;

        AstBlock *initializer_block = compilation_unit -> ast_pool -> GenBlock();
        initializer_block -> left_brace_token  = class_body -> left_brace_token;
        initializer_block -> right_brace_token = class_body -> left_brace_token;
        initializer_block -> block_symbol = init_method -> block_symbol;
        initializer_block -> nesting_level = LocalBlockStack().Size();

        LocalBlockStack().max_size = 0;

        int j,
            k;
        for (j = 0, k = 0; j < class_body -> NumInstanceVariables() && k < class_body -> NumBlocks(); )
        {
            //
            // Note that since there cannot be any overlap in the
            // declarations, we can use either location position.
            // The RightToken of the field declaration is used because it
            // does not have to be computed (it is the terminating semicolon).
            // Similarly, the LeftToken of the block initializer is used
            // because it is the initial "{".
            //
            //    if (class_body -> InstanceVariable(j) -> RightToken() < class_body -> Blocks(k) -> LeftToken())
            //
            if (class_body -> InstanceVariable(j) -> semicolon_token < class_body -> Block(k) -> left_brace_token)
            {
                InitializeVariable(class_body -> InstanceVariable(j), finals);
                j++;
            }
            else
            {
                AstBlock *block_body = class_body -> Block(k);

                //
                // The first block that is the body of an initializer is reachable
                // A subsequent block is reachable iff its predecessor can complete
                // normally
                //
                block_body -> is_reachable = (k == 0 ? true : class_body -> Block(k - 1) -> can_complete_normally);

                ProcessInitializer(initializer_block, block_body, init_method, finals);
                k++;
            }
        }
        for (; j < class_body -> NumInstanceVariables(); j++)
            InitializeVariable(class_body -> InstanceVariable(j), finals);
        for (; k < class_body -> NumBlocks(); k++)
        {
            AstBlock *block_body = class_body -> Block(k);

            //
            // The first block that is the body of an initializer is reachable
            // A subsequent block is reachable iff its predecessor can complete
            // normally
            //
            block_body -> is_reachable = (k == 0 ? true : class_body -> Block(k - 1) -> can_complete_normally);

            ProcessInitializer(initializer_block, block_body, init_method, finals);
        }

        init_method -> max_block_depth = LocalBlockStack().max_size;
        init_method -> block_symbol -> CompressSpace(); // space optimization

        //
        // Note that unlike the case of static fields. We do not ensure here that
        // each final instance variable has been initialized at this point, because
        // the user may chose instead to initialize such a final variable in every
        // constructor instead. See body.cpp
        //

// STG:
//        delete initializer_block;
    }

    return;
}
