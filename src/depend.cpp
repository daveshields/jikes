// $Id: depend.cpp,v 1.18 2000/07/25 11:32:32 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#include "depend.h"
#include "control.h"
#include "ast.h"
#include "semantic.h"

#ifdef HAVE_TIME_H
#include <time.h>
#endif

#ifdef	HAVE_NAMESPACES
using namespace Jikes;
#endif

//
// Note that the types are ordered based on on the subtype relationship. We reverse
// the order here because the desired order for processing is the supertype relationship.
//
inline void TypeCycleChecker::ReverseTypeList()
{
    for (int head = 0, tail = type_list.Length() - 1; head < tail; head++, tail--)
    {
        TypeSymbol *temp = type_list[head];
        type_list[head] = type_list[tail];
        type_list[tail] = temp;
    }

    return;
}


void TypeCycleChecker::PartialOrder(Tuple<Semantic *> &semantic, int start)
{
    type_list.Reset();

    //
    // assert that the "index" of all types that should be checked is initially set to OMEGA
    //
    for (int i = start; i < semantic.Length(); i++)
    {
        Semantic *sem = semantic[i];

        for (int k = 0; k < sem -> compilation_unit -> NumTypeDeclarations(); k++)
        {
            AstClassDeclaration *class_declaration = sem -> compilation_unit -> TypeDeclaration(k) -> ClassDeclarationCast();
            AstInterfaceDeclaration *interface_declaration = sem -> compilation_unit -> TypeDeclaration(k) -> InterfaceDeclarationCast();
            SemanticEnvironment *env = (class_declaration ? class_declaration -> semantic_environment
                                                          : (interface_declaration ? interface_declaration -> semantic_environment
                                                                                   : (SemanticEnvironment *) NULL));
            if (env) // type was successfully compiled thus far?
            {
                TypeSymbol *type = env -> Type();
                if (type -> index == OMEGA)
                   ProcessSubtypes(type);
            }
        }
    }

    ReverseTypeList();

    return;
}


void TypeCycleChecker::PartialOrder(SymbolSet &types)
{
    //
    // assert that the "index" of all types that should be checked is initially set to OMEGA
    //
    for (TypeSymbol *type = (TypeSymbol *) types.FirstElement(); type; type = (TypeSymbol *) types.NextElement())
    {
        if (type -> index == OMEGA)
            ProcessSubtypes(type);
    }

    ReverseTypeList();

    return;
}


void TypeCycleChecker::ProcessSubtypes(TypeSymbol *type)
{
    stack.Push(type);
    int indx = stack.Size();
    type -> index = indx;

    type -> subtypes_closure = new SymbolSet;
    type -> subtypes_closure -> Union(*(type -> subtypes));
    for (TypeSymbol *subtype = (TypeSymbol *) type -> subtypes -> FirstElement();
                     subtype;
                     subtype = (TypeSymbol *) type -> subtypes -> NextElement())
    {
        if (subtype -> index == OMEGA)
             ProcessSubtypes(subtype);
        type -> index = Min(type -> index, subtype -> index);
        type -> subtypes_closure -> Union(*(subtype -> subtypes_closure));
    }

    if (type -> index == indx)
    {
        TypeSymbol *scc_subtype;
        do
        {
            scc_subtype = stack.Top();
            scc_subtype -> index = CYCLE_INFINITY;
            *(scc_subtype -> subtypes_closure) = *(type -> subtypes_closure);
            type_list.Next() = scc_subtype;
            stack.Pop();
        } while (scc_subtype != type);
    }

    return;
}


ConstructorCycleChecker::ConstructorCycleChecker(AstClassBody *class_body)
{
    for (int k = 0; k < class_body -> NumConstructors(); k++)
    {
        AstConstructorDeclaration *constructor_declaration = class_body -> Constructor(k);
        if (constructor_declaration -> index == OMEGA)
            CheckConstructorCycles(constructor_declaration);
    }

    return;
}


void ConstructorCycleChecker::CheckConstructorCycles(AstConstructorDeclaration *constructor_declaration)
{
    stack.Push(constructor_declaration);
    int indx = stack.Size();
    constructor_declaration -> index = indx;

    AstConstructorDeclaration *called_constructor_declaration = NULL;

    AstConstructorBlock *constructor_block = constructor_declaration -> constructor_body;
    if (constructor_block -> explicit_constructor_invocation_opt)
    {
        AstThisCall *this_call = constructor_block -> explicit_constructor_invocation_opt -> ThisCallCast();
        MethodSymbol *called_constructor = (MethodSymbol *) (this_call ? this_call -> symbol : NULL);

        if (called_constructor)
        {
            called_constructor_declaration = (AstConstructorDeclaration *) called_constructor -> method_or_constructor_declaration;

            if (called_constructor_declaration -> index == OMEGA)
                CheckConstructorCycles(called_constructor_declaration);
            constructor_declaration -> index = Min(constructor_declaration -> index, called_constructor_declaration -> index);
        }
    }

    if (constructor_declaration -> index == indx)
    {
        //
        // If the constructor_declaration is alone in its strongly connected component (SCC),
        // and it does not form a trivial cycle with itsself, pop it, mark it and return;
        //
        if (constructor_declaration == stack.Top() && constructor_declaration != called_constructor_declaration)
        {
            stack.Pop();
            constructor_declaration -> index = CYCLE_INFINITY;
        }
        //
        // Otherwise, all elements in the stack up to (and including) constructor_declaration form an SCC.
        // Pop them off the stack, in turn, mark them and issue the appropriate error message.
        //
        else
        {
            do
            {
                called_constructor_declaration = stack.Top();
                stack.Pop();
                called_constructor_declaration -> index = CYCLE_INFINITY;

                constructor_block = (AstConstructorBlock *) called_constructor_declaration -> constructor_body;
                AstMethodDeclarator *constructor_declarator = called_constructor_declaration -> constructor_declarator;

                Semantic *sem = called_constructor_declaration -> constructor_symbol
                                                               -> containing_type -> semantic_environment -> sem;
                sem -> ReportSemError(SemanticError::CIRCULAR_THIS_CALL,
                                       constructor_block -> explicit_constructor_invocation_opt -> LeftToken(),
                                       constructor_block -> explicit_constructor_invocation_opt -> RightToken(),
                                       sem -> lex_stream -> NameString(constructor_declarator -> identifier_token));
            } while (called_constructor_declaration != constructor_declaration);
        }
    }

    return;
}


//
// assert that the "index" of all types that should be checked is initially set to OMEGA
//
void TypeDependenceChecker::PartialOrder()
{
    for (FileSymbol *file_symbol = (FileSymbol *) file_set.FirstElement();
                     file_symbol;
                     file_symbol = (FileSymbol *) file_set.NextElement())
    {
        for (int j = 0; j < file_symbol -> types.Length(); j++)
        {
            TypeSymbol *type = file_symbol -> types[j];
            if (type -> incremental_index == OMEGA)
                ProcessType(type);
        }
    }

    for (int k = 0; k < type_trash_bin.Length(); k++)
    {
        TypeSymbol *type = type_trash_bin[k];
        if (type -> incremental_index == OMEGA)
            ProcessType(type);
    }

    return;
}


void TypeDependenceChecker::ProcessType(TypeSymbol *type)
{
    stack.Push(type);
    int indx = stack.Size();
    type -> incremental_index = indx;

    type -> dependents -> RemoveElement(type); // if dependents is reflexive make it non-reflexive - saves time !!!
    type -> dependents_closure = new SymbolSet;
    type -> dependents_closure -> AddElement(type); // compute reflexive transitive closure
    for (TypeSymbol *dependent = (TypeSymbol *) type -> dependents -> FirstElement();
                     dependent;
                     dependent = (TypeSymbol *) type -> dependents -> NextElement())
    {
        if (dependent -> incremental_index == OMEGA)
             ProcessType(dependent);
        type -> incremental_index = Min(type -> incremental_index, dependent -> incremental_index);
        type -> dependents_closure -> Union(*(dependent -> dependents_closure));
    }

    if (type -> incremental_index == indx)
    {
        TypeSymbol *scc_dependent;
        do
        {
            scc_dependent = stack.Top();
            scc_dependent -> incremental_index = CYCLE_INFINITY;
            *(scc_dependent -> dependents_closure) = *(type -> dependents_closure);
            type_list.Next() = scc_dependent;
            stack.Pop();
        } while (scc_dependent != type);
    }

    return;
}


void TypeDependenceChecker::OutputMake(FILE *outfile, char *output_name, Tuple<FileSymbol *> &file_list)
{
    assert(outfile);

    for (int i = 0; i < file_list.Length(); i++)
    {
        FileSymbol *file_symbol = file_list[i];
        char *name = file_symbol -> FileName();
        int length = file_symbol -> FileNameLength() - (file_symbol -> IsJava() ? FileSymbol::java_suffix_length
                                                                                : FileSymbol::class_suffix_length);

        char *class_name = new char[length + FileSymbol::class_suffix_length + 1],
             *java_name = new char[length + FileSymbol::java_suffix_length + 1];

        strncpy(class_name, name, length);
        strcpy(&class_name[length], FileSymbol::class_suffix);
        strncpy(java_name, name, length);
        strcpy(&java_name[length], FileSymbol::java_suffix);

        fprintf(outfile, "%s : %s\n", output_name, java_name);

        if (i > 0) // Not the first file in the list
        {
            fprintf(outfile, "%s : %s\n", output_name, class_name);
        }

        delete [] class_name;
        delete [] java_name;
    }

    return;
}


void TypeDependenceChecker::OutputMake(FileSymbol *file_symbol)
{
    //
    //
    //
    char *name;
    char *buf;
    int length;

    if (control -> option.directory == NULL)
    {
        name = file_symbol -> FileName();
        length = file_symbol -> FileNameLength() - (file_symbol -> IsJava() ? FileSymbol::java_suffix_length
                                                                                : FileSymbol::class_suffix_length);
    }
    else
    {
        name = file_symbol -> Utf8Name();
        length = strlen(name);

        DirectorySymbol *dir_symbol = file_symbol -> OutputDirectory();
        char *dir_name = dir_symbol -> DirectoryName();
        int dir_length = strlen(dir_name);
        
        buf = new char[length + FileSymbol::class_suffix_length + dir_length + 2];
        strcpy(buf, dir_name);

#ifdef UNIX_FILE_SYSTEM
        buf[dir_length] = (char)U_SLASH;
#elif defined(WIN32_FILE_SYSTEM)
        buf[dir_length] = (char)U_BACKSLASH;
#endif

        strcpy(&buf[dir_length+1], name);
        name = buf;
        length = dir_length + 1 + length;
    }



    char *output_name = new char[length + FileSymbol::class_suffix_length + 1],
         *u_name = new char[length + strlen(StringConstant::U8S__DO_u) + 1];

    strncpy(output_name, name, length);
    strncpy(u_name, name, length);
    strcpy(&output_name[length], FileSymbol::class_suffix);
    strcpy(&u_name[length], StringConstant::U8S__DO_u);

    //
    //
    //
    SymbolSet file_set;
    for (int i = 0; i < file_symbol -> types.Length(); i++)
    {
        TypeSymbol *type = file_symbol -> types[i];
        for (TypeSymbol *parent = (TypeSymbol *) type -> parents_closure -> FirstElement();
                         parent;
                         parent = (TypeSymbol *) type -> parents_closure -> NextElement())
        {
            FileSymbol *symbol = parent -> file_symbol;
            if (symbol && (! symbol -> IsZip()))
                file_set.AddElement(symbol);
        }
    }
    file_set.RemoveElement(file_symbol);

    //
    //
    //
    Tuple<FileSymbol *> file_list(file_set.Size());
    file_list.Next() = file_symbol;
    for (FileSymbol *symbol = (FileSymbol *) file_set.FirstElement(); symbol; symbol = (FileSymbol *) file_set.NextElement())
        file_list.Next() = symbol;

    FILE *outfile = ::SystemFopen(u_name, "w");
    if (outfile == NULL)
        Coutput << "*** Cannot open file " << u_name << "\n";
    else
    {
        OutputMake(outfile, output_name, file_list);
        fclose(outfile);
    }

    delete [] output_name;
    delete [] u_name;
    if (control -> option.directory)
        delete [] buf;

    return;
}


void TypeDependenceChecker::OutputDependences()
{
    SymbolSet new_file_set;

    for (int i = 0; i < type_list.Length(); i++)
    {
        TypeSymbol *type = type_list[i];
        type -> parents_closure = new SymbolSet;

        FileSymbol *file_symbol = type -> file_symbol;
        if (file_symbol && (! file_symbol -> IsZip()))
            new_file_set.AddElement(file_symbol);
    }

    for (int l = 0; l < type_list.Length(); l++)
    {
        TypeSymbol *parent = type_list[l];

        for (TypeSymbol *dependent = (TypeSymbol *) parent -> dependents_closure -> FirstElement();
                         dependent;
                         dependent = (TypeSymbol *) parent -> dependents_closure -> NextElement())
                dependent -> parents_closure -> AddElement(parent);
    }

    for (FileSymbol *symbol = (FileSymbol *) new_file_set.FirstElement(); symbol; symbol = (FileSymbol *) new_file_set.NextElement())
        OutputMake(symbol);

    for (int n = 0; n < type_list.Length(); n++)
    {
        TypeSymbol *type = type_list[n];
        delete type -> parents_closure;
        type -> parents_closure = NULL;
    }

    return;
}


void TopologicalSort::Process(TypeSymbol *type)
{
    pending -> AddElement(type);

    for (TypeSymbol *super_type = (TypeSymbol *) type -> supertypes_closure -> FirstElement();
                     super_type;
                     super_type = (TypeSymbol *) type -> supertypes_closure -> NextElement())
    {
        if (type_collection.IsElement(super_type))
        {
            if (! pending -> IsElement(super_type))
                Process(super_type);
        }
    }

    type_list.Next() = type;

    return;
}


void TopologicalSort::Sort()
{
    type_list.Reset();

    for (TypeSymbol *type = (TypeSymbol *) type_collection.FirstElement(); type; type = (TypeSymbol *) type_collection.NextElement())
    {
        if (! pending -> IsElement(type))
            Process(type);
    }

    pending -> SetEmpty();

    return;
}


TopologicalSort::TopologicalSort(SymbolSet &type_collection_, Tuple<TypeSymbol *> &type_list_) : type_collection(type_collection_),
                                                                                                 type_list(type_list_)
{
    pending = new SymbolSet(type_collection.Size());

    return;
}


TopologicalSort::~TopologicalSort()
{
    delete pending;
}
