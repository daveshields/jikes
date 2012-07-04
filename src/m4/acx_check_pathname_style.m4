dnl @synopsis ACX_CHECK_PATHNAME_STYLE_DOS
dnl
dnl Check if host OS uses DOS-style pathnames. This includes the use
dnl of drive letters and backslashes. Under DOS, Windows, and OS/2,
dnl defines HAVE_PATHNAME_STYLE_DOS and PATH_SEPARATOR to ';'.
dnl Otherwise, defines PATH_SEPARATOR to ':'.
dnl
dnl This macro depends on the AC_CANONICAL_HOST.
dnl
dnl Use for enabling code to handle drive letters, backslashes in
dnl filenames and semicolons in the PATH.
dnl
dnl @version $Id: acx_check_pathname_style.m4,v 1.8 2004/01/13 13:31:27 ericb Exp $
dnl @author Mark Elbrecht <snowball3@bigfoot.com>
dnl
AC_DEFUN([ACX_CHECK_PATHNAME_STYLE_DOS],
[AC_MSG_CHECKING(for Windows and DOS and OS/2 style pathnames)
AC_CACHE_VAL(acx_cv_pathname_style_dos,
[AC_REQUIRE([AC_CANONICAL_HOST])

acx_cv_pathname_style_dos="no"
case ${host_os} in
  *djgpp | *mingw32* | *windows32* | *emx*) acx_cv_pathname_style_dos=yes ;;
esac
])
AC_MSG_RESULT($acx_cv_pathname_style_dos)
AS_IF([test "$acx_cv_pathname_style_dos" = yes],
 [AC_DEFINE([HAVE_PATHNAME_STYLE_DOS],,
   [defined if running on a system with dos style paths])
  AC_DEFINE(PATH_SEPARATOR, ';', [used to seperate elements on the PATH])],
 [AC_DEFINE(PATH_SEPARATOR, ':', [used to seperate elements on the PATH])])])
