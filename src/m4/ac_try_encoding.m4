dnl @synopsis AC_TRY_AN_ENCODING
dnl
dnl Try an encoding ($1) and possibly byte swapping it ($2)
dnl  I choose ISO-8859-1 as the source encoding because it
dnl  seems the safest... I could be wrong.
dnl @version $Id: ac_try_encoding.m4,v 1.8 2004/02/02 14:46:49 ericb Exp $
dnl @author Christopher Abbey <chris_abbey@yahoo.com>

AC_DEFUN([AC_TRY_AN_ENCODING], [
  checkmsg="$1 as an encoding"
  AS_IF([test "$2" = yes], [byte_swap=1
         checkmsg="$checkmsg with byteswapping"],
        [byte_swap=0])
  AC_MSG_CHECKING([$checkmsg])
  AC_RUN_IFELSE([AC_LANG_PROGRAM([
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
#ifdef HAVE_ICONV_H
# include <iconv.h>
#endif
], [[const char * src = "abc\0";
  size_t dstspace = 4*sizeof(wchar_t);
  size_t srcspace = strlen(src);
  char * dst = new char[dstspace];
  char * dst_ptr = dst;
  iconv_t d = iconv_open( "$1" , "ISO-8859-1");
  if (d == (iconv_t) -1) {
    d = iconv_open( "$1" , "ISO8859-1");
    if (d == (iconv_t) -1) {
      return 1;
    }
  }
  size_t r = iconv( d,
#ifdef HAVE_ERROR_CALL_ICONV_CONST
	 (char **)
#endif
	 &src, &srcspace, &dst_ptr, &dstspace);
  if (r == (size_t) -1)
    return 2;
  wchar_t * test = (wchar_t *) dst;
#if $byte_swap
  char tmp;
  for (int lcv=0; lcv<3; lcv++) {
    char *targ = (char *)&test[lcv];
# if SIZEOF_WCHAR_T == 2
    tmp = targ[0];
    targ[0]=targ[1];
    targ[1]=tmp;
# elif SIZEOF_WCHAR_T == 4
    tmp = targ[0];
    targ[0]=targ[3];
    targ[3]=tmp;
    tmp=targ[1];
    targ[1]=targ[2];
    targ[2]=tmp;
# else
    return 3;
# endif //sizeof(wchar_t)
  }
#endif //byte_swap
  if (test[0] == 'a' && test[1] == 'b' && test[2] == 'c')
    return 0;
  else
    return 4;
  ]])],
  [ac_cv_jikes_iconv_encoding=$1
   AC_MSG_RESULT($1)],
  [AC_MSG_RESULT(no)],
  [AC_MSG_RESULT([cross-compiling, you must supply correct answer in cache])])
])
