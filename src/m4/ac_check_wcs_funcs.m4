dnl @synopsis AC_CHECK_WCS_FUNCS
dnl
dnl Check to see if the wcs family of wide string (unicode)
dnl is defined.
dnl @version $Id: ac_check_wcs_funcs.m4,v 1.8 2004/01/13 13:31:25 ericb Exp $
dnl @author Mo DeJong <mdejong@cygnus.com>
dnl
dnl This macro depends on AC_CHECK_ERROR_DISCARD_CONST
dnl This file also defines the macro AC_CHECK_EXTENDED_WCS_FUNCS
dnl which checks for even more wcs functions.
dnl
AC_DEFUN([AC_CHECK_WCS_FUNCS],
[dnl	Check for standard places where wide string functions are defined
AC_CHECK_HEADERS(wchar.h string.h strings.h memory.h)

dnl	Check for wcs functions in wchar.h or string.h
AC_CHECK_FUNCS(wcslen wcscpy wcsncpy wcscat wcscmp wcsncmp)

dnl Check to see if we get a compiler error when discarding a const qualifier
AC_REQUIRE([AC_CHECK_ERROR_DISCARD_CONST])

wcs_funcs_includes="
#ifdef HAVE_STRING_H
# if !STDC_HEADERS && HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#else
# ifdef HAVE_STRINGS_H
#  include <strings.h>
# endif
#endif
#ifdef HAVE_WCHAR_H
# include <wchar.h>
#endif
"

dnl	See if wint_t is defined in stddef.h or wchar.h
AC_CHECK_TYPE([wint_t], [ac_cv_type_wint_t=yes], [ac_cv_type_wint_t=no],
              [$wcs_funcs_includes])
AS_IF([test "$ac_cv_type_wint_t" = yes],
  [AC_DEFINE(HAVE_WINT_T, 1,
    [Defined when the wint_t type is supported])])

dnl	Make sure we can compile a simple call to wcslen
AC_CACHE_CHECK([call wcslen], [ac_cv_call_wcslen],
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$wcs_funcs_includes], [wcslen(NULL)])],
  [ac_cv_call_wcslen=yes],
  [ac_cv_call_wcslen=no])
])

dnl No additional wcslen testing is possible if regular call does not work.
dnl In this case, the application will need to provide an impl of wcslen.
AS_IF([test "$ac_cv_call_wcslen" = yes],
[dnl	Check for declaration of wcslen without const qualifier
 AC_CACHE_CHECK([for error calling wcslen with const argument],
  [ac_cv_error_call_wcslen_const],
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$wcs_funcs_includes], [
const wchar_t * s = NULL;
wcslen(s);
])],
 [ac_cv_error_call_wcslen_const=no], [ac_cv_error_call_wcslen_const=yes])
])
 AS_IF([test "$ac_cv_error_call_wcslen_const" = yes],
  [dnl define symbol so we know when to cast the argument to wcslen
  dnl from const wchar_t * to wchar_t *

  AC_DEFINE([HAVE_ERROR_CALL_WCSLEN_CONST], ,
    [Defined when the compiler would generate an error on a call to wcslen
with a non const argument. This is only known to happen on cygwin and mingwin])

  dnl Double check that our suggested fix works.
  AC_CACHE_CHECK([fix for calling wcslen with non const argument],
    [ac_cv_call_wcslen_non_const],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$wcs_funcs_includes], [
  const wchar_t * s = NULL;
  wcslen(
  #ifdef HAVE_ERROR_CALL_WCSLEN_CONST
           (wchar_t *)
  #endif
         s);
  ])],
    [ac_cv_call_wcslen_non_const=yes],
    [AC_MSG_ERROR([Could not compile with wcslen const cast fix])]
  )])])
])])

dnl	Check for extended wcs functions, these are not as commonly used
dnl	as the ones above so we do not want to always check for them.

AC_DEFUN([AC_CHECK_EXTENDED_WCS_FUNCS],
[
AC_CHECK_FUNCS([wcsncat wcsnlen wcscasecmp wcsncasecmp wcscoll wcsxfrm wcsdup])
AC_CHECK_FUNCS([wcschr wcsrchr wcscspn wcsspn wcspbrk wcsstr wcswcs wcstok])
AC_CHECK_FUNCS([wmemchr wmemcmp wmemcpy wmemmove wmemset btowc wctob])
AC_CHECK_FUNCS([mbsinit mbrtowc wcrtomb mbrlen mbsrtowcs wcsrtombs])
AC_CHECK_FUNCS([mbsnrtowcs wcsnrtombs wcwidth wcswidth wcstod wcstof wcstold])
AC_CHECK_FUNCS([wcstol wcstoul wcstoq wcstouq wcstoll wcstoull])
])
