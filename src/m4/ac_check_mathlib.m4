dnl @synopsis AC_CHECK_MATHLIB
dnl
dnl This check will check for the math.h include file and see if the
dnl math library -lm needs to be added to the compiler LIBS.
dnl
dnl @version $Id: ac_check_mathlib.m4,v 1.5 2004/01/02 14:07:52 ericb Exp $
dnl @author Mo DeJong <mdejong@cygnus.com>
dnl
AC_DEFUN([AC_CHECK_MATHLIB],
[
AC_CHECK_HEADERS(math.h)
AC_SEARCH_LIBS(cos, m)
]
)

