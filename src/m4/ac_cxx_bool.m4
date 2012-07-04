dnl @synopsis AC_CXX_BOOL
dnl
dnl If the compiler recognizes bool as a separate built-in type,
dnl define HAVE_BOOL. Note that a typedef is not a separate
dnl type since you cannot overload a function such that it accepts either
dnl the basic type or the typedef.
dnl
dnl @version $Id: ac_cxx_bool.m4,v 1.7 2004/01/13 13:31:25 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_BOOL],
[AC_CACHE_CHECK([whether the compiler recognizes bool as a built-in type],
 [ac_cv_cxx_bool],
 [AC_LANG_PUSH([C++])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
int f(int  x){return x;}
int f(char x){return x;}
int f(bool x){return x;}
],[bool b = true; return f(b);])],
 [ac_cv_cxx_bool=yes], [ac_cv_cxx_bool=no])
  AC_LANG_POP([C++])])
AS_IF([test "$ac_cv_cxx_bool" = yes],
  [AC_DEFINE(HAVE_BOOL,,[defined if bool is a built-in C++ type])])])
