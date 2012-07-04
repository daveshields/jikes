dnl @synopsis AC_CHECK_ERROR_DISCARD_CONST
dnl
dnl Check to see if calling a non const function with a const
dnl argument causes a compiler error. If it does then discarding
dnl a const qualifier is treated as an error by this compiler.
dnl
dnl @version $Id: ac_check_error_discard_const.m4,v 1.6 2004/01/13 13:31:25 ericb Exp $
dnl @author Mo DeJong <mdejong@cygnus.com>
dnl
AC_DEFUN([AC_CHECK_ERROR_DISCARD_CONST],
[AC_CACHE_CHECK([for compiler error on discard of const qualifier],
[ac_cv_error_discard_const],
[AC_COMPILE_IFELSE([AC_LANG_SOURCE([
char * foo(char * arg) { return arg; }
const char * const_foo(const char * arg) { return foo(arg); }
])],
 [ac_cv_error_discard_const=no], [ac_cv_error_discard_const=yes])
])
AS_IF([test "$ac_cv_error_discard_const" = yes],
  [AC_DEFINE([HAVE_ERROR_DISCARD_CONST], ,
    [compiler will generate an error when discarding a const qualifier])])])
