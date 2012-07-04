dnl @synopsis AC_CXX_TYPE_EQUIVALENCE(TYPEA, TYPEB
dnl                                   [, INCLUDES = DEFAULT-INCLUDES])
dnl
dnl See if C++ treats the two types, TYPEA and TYPEB, as equivalent for
dnl method overloading purposes.  Use INCLUDES to define either type, if
dnl necessary.  If the two types are the same, then
dnl AC_DEFINE TYPE_<TYPEA>_IS_<TYPEB>.  This also sets the cache
dnl variable ac_cv_type_<typea>_is_<typeb> to yes or no as appropriate.
dnl
dnl For example, some compilers cannot tell `char' from `unsigned char'. So,
dnl in configure.ac, add:
dnl
dnl    AC_CXX_TYPE_EQUIVALENCE([char], [unsigned char])
dnl
dnl Then, in your code, when you want all possible overloads, you can avoid
dnl conflicting definitions with:
dnl
dnl    myfunc (char);
dnl    #ifndef TYPE_CHAR_IS_UNSIGNED_CHAR
dnl    myfunc (unsigned char);
dnl    #endif // TYPE_CHAR_IS_UNSIGNED_CHAR
dnl
dnl @version $Id: ac_cxx_type_equivalence.m4,v 1.2 2003/09/27 18:17:09 ericb Exp $
dnl @author Eric Blake <ebb9@byu.net>

dnl The style of this test was copied from
dnl /usr/share/autoconf/autoconf/types.m4, and uses undocumented constructs.
AC_DEFUN([AC_CXX_TYPE_EQUIVALENCE],
  [AS_VAR_PUSHDEF([ac_Types], [ac_cv_$1_is_$2])dnl
  AC_CACHE_CHECK([whether $1 and $2 are equivalent types], ac_Types,
    [AC_LANG_PUSH([C++])
    AC_COMPILE_IFELSE([
      AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT([$3])
extern void
foo ($1);
void (*bar) ($2) = &foo;])],
      [AS_VAR_SET(ac_Types, yes)],
      [AS_VAR_SET(ac_Types, no)])
    AC_LANG_POP([C++])])
  AS_IF([test AS_VAR_GET(ac_Types) = yes],
    [AC_DEFINE_UNQUOTED(AS_TR_CPP(TYPE_$1_IS_$2), 1,
      [Defined when $1 and $2 are equivalent types for method overloading.])])
  AS_VAR_POPDEF([ac_Types])dnl
])
