// $Id: access.h,v 1.18 2002/11/02 15:54:26 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef access_INCLUDED
#define access_INCLUDED

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif


class AccessFlags
{
public:
    enum
    {
        ACCESS_PUBLIC = 0x0001,
        ACCESS_PRIVATE = 0x0002,
        ACCESS_PROTECTED = 0x0004,
        ACCESS_STATIC = 0x0008,
        ACCESS_FINAL = 0x0010,
        ACCESS_SUPER = 0x0020,
        ACCESS_SYNCHRONIZED = 0x0020,
        ACCESS_VOLATILE = 0x0040,
        ACCESS_TRANSIENT = 0x0080,
        ACCESS_NATIVE = 0x0100,
        ACCESS_INTERFACE = 0x0200,
        ACCESS_ABSTRACT = 0x0400,
        ACCESS_STRICTFP = 0x0800,
        ACCESS_ACCESS = ACCESS_PUBLIC | ACCESS_PROTECTED | ACCESS_PRIVATE
    };

protected:
    u2 access_flags;

public:
    AccessFlags(u2 flags = 0) : access_flags(flags) {}

    bool IsSet(u2 flag) const { return (access_flags & flag) != 0; }
    bool ACC_PUBLIC() const { return IsSet(ACCESS_PUBLIC); }
    bool ACC_PRIVATE() const { return IsSet(ACCESS_PRIVATE); }
    bool ACC_PROTECTED() const { return IsSet(ACCESS_PROTECTED); }
    bool ACC_STATIC() const { return IsSet(ACCESS_STATIC); }
    bool ACC_FINAL() const { return IsSet(ACCESS_FINAL); }
    bool ACC_SUPER() const { return IsSet(ACCESS_SUPER); }
    bool ACC_SYNCHRONIZED() const { return IsSet(ACCESS_SYNCHRONIZED); }
    bool ACC_VOLATILE() const { return IsSet(ACCESS_VOLATILE); }
    bool ACC_TRANSIENT() const { return IsSet(ACCESS_TRANSIENT); }
    bool ACC_NATIVE() const { return IsSet(ACCESS_NATIVE); }
    bool ACC_INTERFACE() const { return IsSet(ACCESS_INTERFACE); }
    bool ACC_ABSTRACT() const { return IsSet(ACCESS_ABSTRACT); }
    bool ACC_STRICTFP() const { return IsSet(ACCESS_STRICTFP); }

    void SetFlags(u2 flag) { access_flags |= flag; }
    void SetFlags(const AccessFlags& af)  { access_flags = af.access_flags; }
    void SetACC_PUBLIC() { SetFlags(ACCESS_PUBLIC); }
    void SetACC_PRIVATE() { SetFlags(ACCESS_PRIVATE); }
    void SetACC_PROTECTED() { SetFlags(ACCESS_PROTECTED); }
    void SetACC_STATIC() { SetFlags(ACCESS_STATIC); }
    void SetACC_FINAL() { SetFlags(ACCESS_FINAL); }
    void SetACC_SUPER() { SetFlags(ACCESS_SUPER); }
    void SetACC_SYNCHRONIZED() { SetFlags(ACCESS_SYNCHRONIZED); }
    void SetACC_VOLATILE() { SetFlags(ACCESS_VOLATILE); }
    void SetACC_TRANSIENT() { SetFlags(ACCESS_TRANSIENT); }
    void SetACC_NATIVE() { SetFlags(ACCESS_NATIVE); }
    void SetACC_INTERFACE() { SetFlags(ACCESS_INTERFACE); }
    void SetACC_ABSTRACT() { SetFlags(ACCESS_ABSTRACT); }
    void SetACC_STRICTFP() { SetFlags(ACCESS_STRICTFP); }

    void ResetFlags(u2 flag = 0xffff) { access_flags &= ~ flag; }
    void ResetACC_PUBLIC() { ResetFlags(ACCESS_PUBLIC); }
    void ResetACC_PRIVATE() { ResetFlags(ACCESS_PRIVATE); }
    void ResetACC_PROTECTED() { ResetFlags(ACCESS_PROTECTED); }
    void ResetACC_STATIC() { ResetFlags(ACCESS_STATIC); }
    void ResetACC_FINAL() { ResetFlags(ACCESS_FINAL); }
    void ResetACC_SUPER() { ResetFlags(ACCESS_SUPER); }
    void ResetACC_SYNCHRONIZED() { ResetFlags(ACCESS_SYNCHRONIZED); }
    void ResetACC_VOLATILE() { ResetFlags(ACCESS_VOLATILE); }
    void ResetACC_TRANSIENT() { ResetFlags(ACCESS_TRANSIENT); }
    void ResetACC_NATIVE() { ResetFlags(ACCESS_NATIVE); }
    void ResetACC_INTERFACE() { ResetFlags(ACCESS_INTERFACE); }
    void ResetACC_ABSTRACT() { ResetFlags(ACCESS_ABSTRACT); }
    void ResetACC_STRICTFP() { ResetFlags(ACCESS_STRICTFP); }

    u2 Flags() const { return access_flags; }

    inline const wchar_t* AccessString() const
    {
        return (ACC_PUBLIC() ? StringConstant::US_public
                : ACC_PROTECTED() ? StringConstant::US_protected
                : ACC_PRIVATE() ? StringConstant::US_private
                : StringConstant::US_default);
    }

    //
    // Return true if adding flag will not violate recommended ordering of
    // modifiers based on the modifiers already set.
    //
    bool RecommendedOrder(const u2 flag)
    {
        u2 later_flags = 0;
        switch (flag)
        {
        case ACCESS_PUBLIC:
        case ACCESS_PRIVATE:
        case ACCESS_PROTECTED:
            later_flags = ACCESS_ABSTRACT | ACCESS_STATIC | ACCESS_FINAL |
                ACCESS_SYNCHRONIZED | ACCESS_TRANSIENT | ACCESS_VOLATILE |
                ACCESS_NATIVE | ACCESS_STRICTFP;
            break;
        case ACCESS_ABSTRACT:
            later_flags = ACCESS_STATIC | ACCESS_FINAL | ACCESS_SYNCHRONIZED |
                ACCESS_TRANSIENT | ACCESS_VOLATILE | ACCESS_NATIVE |
                ACCESS_STRICTFP;
            break;
        case ACCESS_STATIC:
            later_flags = ACCESS_FINAL | ACCESS_SYNCHRONIZED |
                ACCESS_TRANSIENT | ACCESS_VOLATILE | ACCESS_NATIVE |
                ACCESS_STRICTFP;
            break;
        case ACCESS_FINAL:
            later_flags = ACCESS_SYNCHRONIZED | ACCESS_TRANSIENT |
                ACCESS_VOLATILE | ACCESS_NATIVE | ACCESS_STRICTFP;
            break;
        case ACCESS_SYNCHRONIZED:
            later_flags = ACCESS_TRANSIENT | ACCESS_VOLATILE | ACCESS_NATIVE |
                ACCESS_STRICTFP;
            break;
        case ACCESS_TRANSIENT:
            later_flags = ACCESS_VOLATILE | ACCESS_NATIVE | ACCESS_STRICTFP;
            break;
        case ACCESS_VOLATILE:
            later_flags = ACCESS_NATIVE | ACCESS_STRICTFP;
            break;
        case ACCESS_NATIVE:
            later_flags = ACCESS_STRICTFP;
            break;
        case ACCESS_STRICTFP:
            break;
        default:
            assert(false && "invalid modifier");
        }
        return ! IsSet(later_flags);
    }

#ifdef JIKES_DEBUG
    void Print() const
    {
        Coutput << " access_flags: ";
        if (ACC_PUBLIC())
            Coutput << " public";
        if (ACC_PRIVATE())
            Coutput << " private";
        if (ACC_PROTECTED())
            Coutput << " protected";
        if (ACC_STATIC())
            Coutput << " static";
        if (ACC_FINAL())
            Coutput << " final";
        // super and synchronized use the same bit!
        if (ACC_SYNCHRONIZED())
            Coutput << " super_or_synchronized";
        if (ACC_VOLATILE())
            Coutput << " volatile";
        if (ACC_TRANSIENT())
            Coutput << " transient";
        if (ACC_NATIVE())
            Coutput << " native";
        if (ACC_INTERFACE())
            Coutput << " interface";
        if (ACC_ABSTRACT())
            Coutput << " abstract";
        if (ACC_STRICTFP())
            Coutput << " strictfp";
        Coutput << endl;
    }
#endif // JIKES_DEBUG
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // access_INCLUDED

