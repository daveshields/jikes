// $Id: depend.h,v 1.5 1999/03/25 14:10:11 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef depend_INCLUDED
#define depend_INCLUDED

#include "tuple.h"
#include "config.h"
#include <limits.h>

class Semantic;
class TypeSymbol;
class AstClassBody;
class AstConstructorDeclaration;
class SymbolSet;

class CycleChecker
{
public:
    enum { OMEGA = -1, INFINITY = INT_MAX };

    inline int Min(int x, int y) { return (x < y ? x : y); }
};

class TypeCycleChecker : public CycleChecker
{
public:

    TypeCycleChecker(Tuple<TypeSymbol *> &type_list_) : type_list(type_list_)
    {}
    void PartialOrder(Tuple<Semantic *> &, int);
    void PartialOrder(SymbolSet &);

private:
    class Stack
    {
    public:
        void Push(TypeSymbol *type) { info.Next() = type; }
        void Pop()                  { if (info.Length() > 0) info.Reset(info.Length() - 1); }
        int Size()                  { return info.Length(); }
        TypeSymbol *Top()           { return (TypeSymbol *) (info.Length() > 0 ? info[info.Length() - 1] : NULL); }
    private:
        Tuple<TypeSymbol *> info;
    } stack;

    Tuple<TypeSymbol *> &type_list;

    void ProcessSubtypes(TypeSymbol *);
    void ReverseTypeList();
};


class ConstructorCycleChecker : public CycleChecker
{
public:

    ConstructorCycleChecker(AstClassBody *);

private:
    class Stack
    {
    public:
        void Push(AstConstructorDeclaration *constructor) { info.Next() = constructor; }
        void Pop()                                        { if (info.Length() > 0) info.Reset(info.Length() - 1); }
        int Size()                                        { return info.Length(); }
        AstConstructorDeclaration *Top()
           { return (AstConstructorDeclaration *) (info.Length() > 0 ? info[info.Length() - 1] : NULL);}
    private:
        Tuple<AstConstructorDeclaration *> info;
    } stack;

    void CheckConstructorCycles(AstConstructorDeclaration *);
};


class TypeDependenceChecker : public CycleChecker
{
public:
    TypeDependenceChecker(Control *control_, SymbolSet &file_set_, Tuple<TypeSymbol *> &type_trash_bin_)
        : control(control_),
          file_set(file_set_),
          type_trash_bin(type_trash_bin_)
    {}

    ~TypeDependenceChecker() {}

    void PartialOrder();
    void OutputDependences();

    Tuple<TypeSymbol *> &TypeList() { return type_list; }

    SymbolSet &file_set;

private:
    Control *control;
    Tuple<TypeSymbol *> &type_trash_bin;

    class Stack
    {
    public:
        void Push(TypeSymbol *type) { info.Next() = type; }
        void Pop()                  { if (info.Length() > 0) info.Reset(info.Length() - 1); }
        int Size()                  { return info.Length(); }
        TypeSymbol *Top()           { return (TypeSymbol *) (info.Length() > 0 ? info[info.Length() - 1] : NULL); }
    private:
        Tuple<TypeSymbol *> info;
    } stack;

    void OutputMake(FILE *, char *, Tuple<FileSymbol *> &);
    void OutputMake(FileSymbol *);

    Tuple<TypeSymbol *> type_list;

    void ProcessType(TypeSymbol *);
};


class TopologicalSort
{
public:
    TopologicalSort(SymbolSet &, Tuple<TypeSymbol *> &);
    ~TopologicalSort();

    void Sort();

private:
    void Process(TypeSymbol *);

    SymbolSet *pending;

    SymbolSet &type_collection;
    Tuple<TypeSymbol *> &type_list;
};

#endif /* cycle_INCLUDED */
