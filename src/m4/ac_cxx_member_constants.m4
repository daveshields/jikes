dnl @synopsis AC_CXX_MEMBER_CONSTANTS
dnl
dnl If the compiler supports member constants, define HAVE_MEMBER_CONSTANTS.
dnl
dnl @version $Id: ac_cxx_member_constants.m4,v 1.4 2004/01/13 13:31:26 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_MEMBER_CONSTANTS],
[AC_CACHE_CHECK([whether the compiler supports member constants],
[ac_cv_cxx_member_constants],
[AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
  [class C {public: static const int i = 0;}; const int C::i;],
  [return C::i;])],
 [ac_cv_cxx_member_constants=yes], [ac_cv_cxx_member_constants=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_member_constants" = yes],
  [AC_DEFINE(HAVE_MEMBER_CONSTANTS,,
        [define if the compiler supports member constants])])])
