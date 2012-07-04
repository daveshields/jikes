dnl @synopsis AC_CXX_STATIC_CAST
dnl
dnl If the compiler supports static_cast<>, define HAVE_STATIC_CAST.
dnl
dnl @version $Id: ac_cxx_static_cast.m4,v 1.4 2004/01/13 13:31:27 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_STATIC_CAST],
[AC_CACHE_CHECK([whether the compiler supports static_cast<>],
[ac_cv_cxx_static_cast],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <typeinfo>
class Base { public : Base () {} virtual void f () = 0; };
class Derived : public Base { public : Derived () {} virtual void f () {} };
int g (Derived&) { return 0; }],[
Derived d; Base& b = d;
Derived& s = static_cast<Derived&> (b); return g (s);])],
 [ac_cv_cxx_static_cast=yes], [ac_cv_cxx_static_cast=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_static_cast" = yes],
  [AC_DEFINE(HAVE_STATIC_CAST,,
            [define if the compiler supports static_cast<>])])])
