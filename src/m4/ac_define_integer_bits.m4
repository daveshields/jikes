dnl http://www.gnu.org/software/ac-archive/Miscellaneous/ac_define_integer_bits.html

dnl @synopsis AC_DEFINE_INTEGER_BITS (TYPE [, CANDIDATE-TYPE]...)
dnl
dnl Given a TYPE of the form "int##_t" or "uint##_t", see if the datatype
dnl TYPE is predefined.  If not, then define TYPE -- both with AC_DEFINE
dnl and as a shell variable -- to the first datatype of exactly ## bits in
dnl a list of CANDIDATE-TYPEs.  If none of the CANDIDATE-TYPEs contains
dnl exactly ## bits, then set the TYPE shell variable to "no".
dnl
dnl For example, the following ensures that uint64_t is defined as a
dnl 64-bit datatype:
dnl
dnl     AC_DEFINE_INTEGER_BITS(uint64_t, unsigned long long, unsigned __int64, long)
dnl     if test "$uint64_t" = no; then
dnl       AC_MSG_ERROR([unable to continue without a 64-bit datatype])
dnl     fi
dnl
dnl You should then put the following in your C code to ensure that all
dnl datatypes defined by AC_DEFINE_INTEGER_BITS are visible to your program:
dnl
dnl     #include "config.h"
dnl
dnl     #if HAVE_INTTYPES_H
dnl     # include <inttypes.h>
dnl     #else
dnl     # if HAVE_STDINT_H
dnl     #  include <stdint.h>
dnl     # endif
dnl     #endif
dnl
dnl @version $Id: ac_define_integer_bits.m4,v 1.1 2002/10/07 22:06:20 ericb Exp $
dnl @author Scott Pakin <pakin@uiuc.edu>
dnl

AC_DEFUN([AC_DEFINE_INTEGER_BITS],
[m4_define([ac_datatype_bits], [m4_translit($1, [a-zA-Z_])])
m4_define([ac_datatype_bytes], [m4_eval(ac_datatype_bits/8)])
AC_CHECK_TYPE($1, ,
 [
  AC_MSG_NOTICE([trying to find a suitable ]ac_datatype_bytes[-byte replacement for $1])
  $1=no
  find_$1 ()
  {
    _AC_DEFINE_INTEGER_BITS_HELPER($@)
    :
  }
  find_$1
  AC_DEFINE_UNQUOTED($1, $$1,
    [If not already defined, then define as a datatype of *exactly* ]ac_datatype_bits[ bits.])
 ])
])

dnl Iterate over arguments $2..$N, trying to find a good match for $1.
m4_define([_AC_DEFINE_INTEGER_BITS_HELPER],
[ifelse($2, , ,
 [m4_define([ac_datatype_bits], [m4_translit($1, [a-zA-Z_])])
  m4_define([ac_datatype_bytes], [m4_eval(ac_datatype_bits/8)])
  AC_CHECK_SIZEOF($2)
  if test "$AS_TR_SH(ac_cv_sizeof_$2)" -eq ac_datatype_bytes; then
    $1="$2"
    return
  fi
  _AC_DEFINE_INTEGER_BITS_HELPER($1, m4_shift(m4_shift($@)))
 ])
])
