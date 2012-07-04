dnl @synopsis AC_CXX_RTTI
dnl
dnl If the compiler supports Run-Time Type Identification (typeinfo
dnl header and typeid keyword), define HAVE_RTTI.
dnl
dnl @version $Id: ac_cxx_rtti.m4,v 1.4 2004/01/13 13:31:27 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_RTTI],
[AC_CACHE_CHECK([whether the compiler supports Run-Time Type Identification],
[ac_cv_cxx_rtti],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <typeinfo>
class Base { public :
             Base () {}
             virtual int f () { return 0; }
           };
class Derived : public Base { public :
                              Derived () {}
                              virtual int f () { return 1; }
                            };
],[Derived d;
Base *ptr = &d;
return typeid (*ptr) == typeid (Derived);
])],
 [ac_cv_cxx_rtti=yes], [ac_cv_cxx_rtti=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_rtti" = yes],
  [AC_DEFINE(HAVE_RTTI,,
         [define if the compiler supports Run-Time Type Identification])])])
