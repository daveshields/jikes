// $Id: jcl_access.h,v 1.1 1999/11/04 18:48:03 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_access_INCLUDED
#define jcl_access_INCLUDED

#include "jcl_bool.h"
#include "jcl_int.h"
#include <stream.h>

class AccessFlags
{
public:
    bool ACC_PUBLIC()       { return access_flags & 0x0001; }
    bool ACC_PRIVATE()      { return access_flags & 0x0002; }
    bool ACC_PROTECTED()    { return access_flags & 0x0004; }
    bool ACC_STATIC()       { return access_flags & 0x0008; }
    bool ACC_FINAL()        { return access_flags & 0x0010; }
    bool ACC_SYNCHRONIZED() { return access_flags & 0x0020; }
    bool ACC_VOLATILE()     { return access_flags & 0x0040; }
    bool ACC_TRANSIENT()    { return access_flags & 0x0080; }
    bool ACC_NATIVE()       { return access_flags & 0x0100; }
    bool ACC_INTERFACE()    { return access_flags & 0x0200; }
    bool ACC_ABSTRACT()     { return access_flags & 0x0400; }

    void set_ACC_PUBLIC()       { access_flags |= 0x0001; }
    void set_ACC_PRIVATE()      { access_flags |= 0x0002; }
    void set_ACC_PROTECTED()    { access_flags |= 0x0004; }
    void set_ACC_STATIC()       { access_flags |= 0x0008; }
    void set_ACC_FINAL()        { access_flags |= 0x0010; }
    void set_ACC_SYNCHRONIZED() { access_flags |= 0x0020; }
    void set_ACC_VOLATILE()     { access_flags |= 0x0040; }
    void set_ACC_TRANSIENT()    { access_flags |= 0x0080; }
    void set_ACC_NATIVE()       { access_flags |= 0x0100; }
    void set_ACC_INTERFACE()    { access_flags |= 0x0200; }
    void set_ACC_ABSTRACT()     { access_flags |= 0x0400; }

    u2 access_flags;

    AccessFlags() : access_flags(0) {}
    AccessFlags(u2& _access_flags) : access_flags(_access_flags) {}

#ifdef TEST
        void print() {
                cout << "access_flags: ";
                if (ACC_PUBLIC())       cout << " public";
                if (ACC_PRIVATE())      cout << " private";
                if (ACC_PROTECTED())    cout << " protected";
                if (ACC_STATIC())       cout << " static";
                if (ACC_FINAL())        cout << " final";
                if (ACC_SYNCHRONIZED()) cout << " synchronized_or_super";
                if (ACC_VOLATILE())     cout << " volatile";
                if (ACC_TRANSIENT())    cout << " transient";
                if (ACC_NATIVE())       cout << " native";
                if (ACC_INTERFACE())    cout << " interface";
                if (ACC_ABSTRACT())     cout << " abstract";
                cout << "\n";
        }
#endif
};

#endif
