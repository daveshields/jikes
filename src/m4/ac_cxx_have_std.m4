dnl @synopsis AC_CXX_HAVE_STD
dnl
dnl If the compiler supports ISO C++ standard library (i.e., can include the
dnl files iostream, map, iomanip and cmath}), define HAVE_STD.
dnl
dnl @version $Id: ac_cxx_have_std.m4,v 1.6 2004/01/13 13:31:26 ericb Exp $
dnl @author Luc Maisonobe
dnl
AC_DEFUN([AC_CXX_HAVE_STD],
[AC_CACHE_CHECK(whether the compiler supports ISO C++ standard library,
ac_cv_cxx_have_std,
[AC_REQUIRE([AC_CXX_NAMESPACES])
 AC_LANG_PUSH([C++])
 AC_COMPILE_IFELSE([AC_LANG_SOURCE([#include <iostream>
#include <map>
#include <iomanip>
#include <cmath>
#ifdef HAVE_NAMESPACES
using namespace std;
#endif
])],
 [ac_cv_cxx_have_std=yes], [ac_cv_cxx_have_std=no])
 AC_LANG_POP([C++])
])
AS_IF([test "$ac_cv_cxx_have_std" = yes],
  [AC_DEFINE(HAVE_STD,,
        [define if the compiler supports ISO C++ standard library])])])
