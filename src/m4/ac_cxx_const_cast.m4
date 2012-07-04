dnl @synopsis AC_CXX_CONST_CAST
dnl
dnl If the compiler supports const_cast<>, define HAVE_CONST_CAST.
dnl
dnl @version $Id: ac_cxx_const_cast.m4,v 1.4 2004/01/13 13:31:26 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_CONST_CAST],
[AC_CACHE_CHECK([whether the compiler supports const_cast<>],
[ac_cv_cxx_const_cast],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
  [int x = 0;const int& y = x;int& z = const_cast<int&>(y);return z;])],
 [ac_cv_cxx_const_cast=yes], [ac_cv_cxx_const_cast=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_const_cast" = yes],
  [AC_DEFINE([HAVE_CONST_CAST],,
        [define if the compiler supports const_cast<>])])])
