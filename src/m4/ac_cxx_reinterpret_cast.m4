dnl @synopsis AC_CXX_REINTERPRET_CAST
dnl
dnl If the compiler supports reinterpret_cast<>, define HAVE_REINTERPRET_CAST.
dnl
dnl @version $Id: ac_cxx_reinterpret_cast.m4,v 1.4 2004/01/13 13:31:27 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_REINTERPRET_CAST],
[AC_CACHE_CHECK([whether the compiler supports reinterpret_cast<>],
[ac_cv_cxx_reinterpret_cast],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include <typeinfo>
class Base { public : Base () {} virtual void f () = 0;};
class Derived : public Base { public : Derived () {} virtual void f () {} };
class Unrelated { public : Unrelated () {} };
int g (Unrelated&) { return 0; }],[
Derived d;Base& b=d;
Unrelated& e=reinterpret_cast<Unrelated&>(b);return g(e);])],
 [ac_cv_cxx_reinterpret_cast=yes], [ac_cv_cxx_reinterpret_cast=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_reinterpret_cast" = yes],
  [AC_DEFINE(HAVE_REINTERPRET_CAST,,
            [define if the compiler supports reinterpret_cast<>])])])
