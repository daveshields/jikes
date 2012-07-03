// $Id: access.h,v 1.11 2000/07/25 11:32:31 mdejong Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef access_INCLUDED
#define access_INCLUDED

#include "platform.h"

#ifdef	HAVE_NAMESPACES
namespace Jikes {	// Open namespace Jikes block
#endif


class AccessFlags
{
protected:
    u2 access_flags;

public:

    bool ACC_PUBLIC()       { return (access_flags & 0x0001) != 0; }
    bool ACC_PRIVATE()      { return (access_flags & 0x0002) != 0; }
    bool ACC_PROTECTED()    { return (access_flags & 0x0004) != 0; }
    bool ACC_STATIC()       { return (access_flags & 0x0008) != 0; }
    bool ACC_FINAL()        { return (access_flags & 0x0010) != 0; }
    bool ACC_SUPER()        { return (access_flags & 0x0020) != 0; }
    bool ACC_SYNCHRONIZED() { return (access_flags & 0x0020) != 0; }
    bool ACC_VOLATILE()     { return (access_flags & 0x0040) != 0; }
    bool ACC_TRANSIENT()    { return (access_flags & 0x0080) != 0; }
    bool ACC_NATIVE()       { return (access_flags & 0x0100) != 0; }
    bool ACC_INTERFACE()    { return (access_flags & 0x0200) != 0; }
    bool ACC_ABSTRACT()     { return (access_flags & 0x0400) != 0; }
    bool ACC_STRICTFP()     { return (access_flags & 0x0800) != 0; }

    void SetACC_PUBLIC()       { access_flags |= 0x0001; }
    void SetACC_PRIVATE()      { access_flags |= 0x0002; }
    void SetACC_PROTECTED()    { access_flags |= 0x0004; }
    void SetACC_STATIC()       { access_flags |= 0x0008; }
    void SetACC_FINAL()        { access_flags |= 0x0010; }
    void SetACC_SUPER()        { access_flags |= 0x0020; }
    void SetACC_SYNCHRONIZED() { access_flags |= 0x0020; }
    void SetACC_VOLATILE()     { access_flags |= 0x0040; }
    void SetACC_TRANSIENT()    { access_flags |= 0x0080; }
    void SetACC_NATIVE()       { access_flags |= 0x0100; }
    void SetACC_INTERFACE()    { access_flags |= 0x0200; }
    void SetACC_ABSTRACT()     { access_flags |= 0x0400; }
    void SetACC_STRICTFP()     { access_flags |= 0x0800; }

    void ResetACC_PUBLIC()       { access_flags &= (~ 0x0001); }
    void ResetACC_PRIVATE()      { access_flags &= (~ 0x0002); }
    void ResetACC_PROTECTED()    { access_flags &= (~ 0x0004); }
    void ResetACC_STATIC()       { access_flags &= (~ 0x0008); }
    void ResetACC_FINAL()        { access_flags &= (~ 0x0010); }
    void ResetACC_SUPER()        { access_flags &= (~ 0x0020); }
    void ResetACC_SYNCHRONIZED() { access_flags &= (~ 0x0020); }
    void ResetACC_VOLATILE()     { access_flags &= (~ 0x0040); }
    void ResetACC_TRANSIENT()    { access_flags &= (~ 0x0080); }
    void ResetACC_NATIVE()       { access_flags &= (~ 0x0100); }
    void ResetACC_INTERFACE()    { access_flags &= (~ 0x0200); }
    void ResetACC_ABSTRACT()     { access_flags &= (~ 0x0400); }
    void ResetACC_STRICTFP()     { access_flags &= (~ 0x0800); }

    void ResetFlags() { access_flags = 0; }
    void SetFlags(u2 access_flags_) { access_flags = access_flags_; }
    void SetFlags(AccessFlags af)  { this -> access_flags = af.access_flags; }
    u2 Flags() { return access_flags; }

    AccessFlags() : access_flags(0) {}
    AccessFlags(u2& _access_flags) : access_flags(_access_flags) {}

#ifdef JIKES_DEBUG
    void Print()
    {
        Coutput << " access_flags: ";
        if (ACC_PUBLIC())       Coutput << " public";
        if (ACC_PRIVATE())      Coutput << " private";
        if (ACC_PROTECTED())    Coutput << " protected";
        if (ACC_STATIC())       Coutput << " static";
        if (ACC_FINAL())        Coutput << " final";
        // super and synchronized use the same bit!
        if (ACC_SYNCHRONIZED()) Coutput << " super_or_synchronized";
        if (ACC_VOLATILE())     Coutput << " volatile";
        if (ACC_TRANSIENT())    Coutput << " transient";
        if (ACC_NATIVE())       Coutput << " native";
        if (ACC_INTERFACE())    Coutput << " interface";
        if (ACC_ABSTRACT())     Coutput << " abstract";
        if (ACC_STRICTFP())     Coutput << " strictfp";
        Coutput << "\n";
    }
#endif
};

#ifdef	HAVE_NAMESPACES
}			// Close namespace Jikes block
#endif

#endif

