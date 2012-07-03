dnl If you want to make a new macro available for use
dnl in configure.in, add the macro file to the src/m4 subdirectory
dnl and add the name of the m4 macro file with a builtin().
dnl For example, add a FOO macro from foo.m4 like so:
dnl builtin(include,foo.m4)

dnl For lots of handy autoconf C++ macros, go to
dnl http://research.cys.de/autoconf-archive/

builtin(include,src/m4/ac_cxx_bool.m4)
builtin(include,src/m4/ac_cxx_exceptions.m4)
builtin(include,src/m4/ac_cxx_namespaces.m4)
builtin(include,src/m4/ac_cxx_have_std.m4)
builtin(include,src/m4/acx_check_pathname_style.m4)
builtin(include,src/m4/ac_check_mathlib.m4)
builtin(include,src/m4/ac_cxx_check_set_new_handler.m4)
builtin(include,src/m4/ac_check_error_discard_const.m4)
builtin(include,src/m4/ac_check_wcs_funcs.m4)

dnl builtin(include,src/m4/)

