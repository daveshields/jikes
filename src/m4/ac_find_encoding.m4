dnl @synopsis AC_FIND_ENCODING
dnl
dnl search through the list of possible encodings to find one that
dnl provides the right size and endianness to match the platform's
dnl wchar_t. We may need to alter the selection or order of the
dnl ones we test.
dnl @version $Id: ac_find_encoding.m4,v 1.3 2003/12/29 13:18:30 ericb Exp $
dnl @author Christopher Abbey <chris_abbey@yahoo.com>

AC_DEFUN([AC_FIND_ENCODING], [

dnl start the list with wchar_t... because if it
dnl  is there then we're made in the shade.
list=wchar_t

dnl could also test endianness and add the appropriate
dnl  endian specific encodings here

if test $ac_cv_sizeof_wchar_t -eq 4 ; then
  list="$list ucs4 utf32 ucs-4 utf-32 UCS-4"
else
  list="$list ucs2 utf16 ucs-2 utf-16 UCS-2"
fi

for enc in $list ; do
  AC_TRY_AN_ENCODING($enc)
  if test "$is_good" = "$enc" ; then
    AC_DEFINE_UNQUOTED(JIKES_ICONV_ENCODING, "$enc",
        [Define this to the name of the unicode encoding that is the same 
size as your wchar_t. See also the following entry for byte swapping.])
    break
  fi
done
dnl accept byte swapping as a second best option.
if test -z "$is_good" ; then
  for enc in $list ; do
    AC_TRY_AN_ENCODING($enc, yes)
    if test "$is_good" = "$enc" ; then
      AC_DEFINE_UNQUOTED(JIKES_ICONV_ENCODING, "$enc",
        [Define this to the name of the unicode encoding that is the same
size as your wchar_t. See also the following entry for byte swapping.])
      AC_DEFINE(JIKES_ICONV_NEEDS_BYTE_SWAP, 1, [Define this if the encoding
specified above does not match your platform's native endianness.])
      break
    fi
  done
fi
if test -z "$is_good" ; then
  AC_MSG_ERROR([Configure was unable to locate a functional iconv encoding, even with byte swapping. Iconv support should be DISABLED by setting ac_cv_header_iconv_h=no in your shell and re-running configure.])

  dnl I hoped this would unset the define that was previously set.
dnl AC_DEFINE(HAVE_ICONV_H, 0)
  dnl nope... :(  we'll have to punt on this 'cuz that doesn't work,
  dnl changed the above warning to an error, and gave directions to rerun.  
fi
])
