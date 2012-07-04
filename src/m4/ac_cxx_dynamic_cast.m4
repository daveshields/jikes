dnl @synopsis AC_CXX_DYNAMIC_CAST
dnl
dnl If the compiler supports dynamic_cast<>, define HAVE_DYNAMIC_CAST.
dnl
dnl @version $Id: ac_cxx_dynamic_cast.m4,v 1.4 2004/01/13 13:31:26 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_DYNAMIC_CAST],
[AC_CACHE_CHECK([whether the compiler supports dynamic_cast<>],
[ac_cv_cxx_dynamic_cast],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <typeinfo>
class Base { public : Base () {} virtual void f () = 0;};
class Derived : public Base { public : Derived () {} virtual void f () {} };],
[Derived d; Base& b=d; return dynamic_cast<Derived*>(&b) ? 0 : 1;])],
 [ac_cv_cxx_dynamic_cast=yes], [ac_cv_cxx_dynamic_cast=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_dynamic_cast" = yes],
  [AC_DEFINE(HAVE_DYNAMIC_CAST,,
        [define if the compiler supports dynamic_cast<>])])])
