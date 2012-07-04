dnl @synopsis AC_FIND_ENCODING
dnl
dnl search through the list of possible encodings to find one that
dnl provides the right size and endianness to match the platform's
dnl wchar_t. We may need to alter the selection or order of the
dnl ones we test.
dnl @version $Id: ac_find_encoding.m4,v 1.4 2004/02/02 14:46:49 ericb Exp $
dnl @author Christopher Abbey <chris_abbey@yahoo.com>

AC_DEFUN([AC_FIND_ENCODING], [

dnl start the list with wchar_t... because if it
dnl  is there then we're made in the shade.
list=wchar_t

dnl could also test endianness and add the appropriate
dnl  endian specific encodings here
AS_IF([test $ac_cv_sizeof_wchar_t -eq 4],
  [list="$list ucs4 utf32 ucs-4 utf-32 UCS-4"],
  [list="$list ucs2 utf16 ucs-2 utf-16 UCS-2"])

AS_IF([test -z "$ac_cv_jikes_iconv_encoding"],
 [for enc in $list ; do
    AC_TRY_AN_ENCODING($enc)
    if test "$ac_cv_jikes_iconv_encoding" = "$enc" ; then
      AC_DEFINE_UNQUOTED([JIKES_ICONV_ENCODING], "$enc",
        [Define this to the name of the unicode encoding that is the same 
size as your wchar_t. See also the following entry for byte swapping.])
      break
    fi
  done],
 [AC_DEFINE_UNQUOTED([JIKES_ICONV_ENCODING], "$enc",
        [Define this to the name of the unicode encoding that is the same 
size as your wchar_t. See also the following entry for byte swapping.])])
dnl accept byte swapping as a second best option.
AS_IF([test -z "$ac_cv_jikes_iconv_encoding"],
 [for enc in $list ; do
    AC_TRY_AN_ENCODING($enc, yes)
    if test "$ac_cv_jikes_iconv_encoding" = "$enc" ; then
      AC_DEFINE_UNQUOTED([JIKES_ICONV_ENCODING], "$enc",
        [Define this to the name of the unicode encoding that is the same
size as your wchar_t. See also the following entry for byte swapping.])
      AC_DEFINE([JIKES_ICONV_NEEDS_BYTE_SWAP], 1, [Define this if the encoding
specified above does not match your platform's native endianness.])
      break
    fi
  done])
AS_IF([test -z "$ac_cv_jikes_iconv_encoding"],
  [AC_MSG_WARN([Configure was unable to locate a functional iconv encoding,
                even with byte swapping.])])
])
