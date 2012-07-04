dnl @synopsis AC_CXX_EXPLICIT
dnl
dnl If the compiler can be asked to prevent using implicitly one argument
dnl constructors as converting constructors with the explicit
dnl keyword, define HAVE_EXPLICIT.
dnl
dnl @version $Id: ac_cxx_explicit.m4,v 1.4 2004/01/13 13:31:26 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_EXPLICIT],
[AC_CACHE_CHECK([whether the compiler supports the explicit keyword],
[ac_cv_cxx_explicit],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
                    [class A{public:explicit A(double){}};],
                    [double c = 5.0;A x(c);return 0;])],
     [ac_cv_cxx_explicit=yes], [ac_cv_cxx_explicit=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_explicit" = yes],
  [AC_DEFINE(HAVE_EXPLICIT,,
        [define if the compiler supports the explicit keyword])])])
