// ---------------------------------------------------------------------
// This file is provided by Gimpel Software (www.gimpel.com) for use with
// its products PC-lint and FlexeLint.
//
// Redistribution and use of this file, with or without modification, is
// permitted provided that any such redistribution retains this notice.
// ---------------------------------------------------------------------

#ifndef CO_GCC_H_
#define CO_GCC_H_
/*lint -save -w1 */

#ifdef _lint /* Make sure no compiler comes this way */
#ifdef __cplusplus
extern "C" {
#endif

/*
   The headers included below must be generated; For C++, generate
   with:

   g++ [usual build options] -E -dM t.cpp >lint_cppmac.h

   For C, generate with:

   gcc [usual build options] -E -dM t.c >lint_cmac.h

   ...where "t.cpp" and "t.c" are empty source files.

   It's important to use the same compiler options used when compiling
   project code because they can affect the existence and precise
   definitions of certain predefined macros.  See gcc-readme.txt for
   details and a tutorial.
 */
#if defined(ARM)

#include <sys/cdefs.h>
#undef __GNUCLIKE_ATTRIBUTE_MODE_DI
#define __packed
#define __aligned(x)

#if defined(__cplusplus)
#       include "freebsd/lint_cppmac.h" // DO NOT COMMENT THIS OUT. DO NOT SUPPRESS ERROR 322. (If you see an error here, your Lint configuration is broken; check -i options and ensure that you have generated lint_cppmac.h as documented in gcc-readme.txt. Otherwise Gimpel Software cannot support your configuration.)
#else
#       include "freebsd/lint_cmac.h" // DO NOT COMMENT THIS OUT. DO NOT SUPPRESS ERROR 322. (If you see an error here, your Lint configuration is broken; check -i options and ensure that you have generated lint_cmac.h as documented in gcc-readme.txt. Otherwise Gimpel Software cannot support your configuration.)
#endif

#elif defined(PPC)

#if defined(__cplusplus)
#       include "linux/lint_cppmac.h" // DO NOT COMMENT THIS OUT. DO NOT SUPPRESS ERROR 322. (If you see an error here, your Lint configuration is broken; check -i options and ensure that you have generated lint_cppmac.h as documented in gcc-readme.txt. Otherwise Gimpel Software cannot support your configuration.)
#else
#       include "linux/lint_cmac.h" // DO NOT COMMENT THIS OUT. DO NOT SUPPRESS ERROR 322. (If you see an error here, your Lint configuration is broken; check -i options and ensure that you have generated lint_cmac.h as documented in gcc-readme.txt. Otherwise Gimpel Software cannot support your configuration.)
#endif

#else

#if defined(__cplusplus)
#       include "lint_cppmac.h" // DO NOT COMMENT THIS OUT. DO NOT SUPPRESS ERROR 322. (If you see an error here, your Lint configuration is broken; check -i options and ensure that you have generated lint_cppmac.h as documented in gcc-readme.txt. Otherwise Gimpel Software cannot support your configuration.)
#else
#       include "lint_cmac.h" // DO NOT COMMENT THIS OUT. DO NOT SUPPRESS ERROR 322. (If you see an error here, your Lint configuration is broken; check -i options and ensure that you have generated lint_cmac.h as documented in gcc-readme.txt. Otherwise Gimpel Software cannot support your configuration.)
#endif

#endif

/* If the macro set given by the generated macro files must be adjusted in
   order for Lint to cope, then you can make those adjustments here.
 */

#define LINT_CO_GCC_H_GCC_VERSION  ( __GNUC__     * 10000 +     \
                                     __GNUC_MINOR__ * 100 +     \
                                     __GNUC_PATCHLEVEL__ )

/* The following is a workaround for versions of GCC with bug 25717, in
   which the preprocessor does not dump a #define directive for __STDC__
   when -dM is given:
   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=25717

   We know the unconditional definition of __STDC__ was introduced no
   later than version 3.0; the preprocessor bug was fixed no later than
   version 4.1.0.
 */
#if ( LINT_CO_GCC_H_GCC_VERSION >= 30000 &&                     \
      LINT_CO_GCC_H_GCC_VERSION <  40100 )
#        define __STDC__ 1
#endif


#if LINT_CO_GCC_H_GCC_VERSION >= 40300
#        define __COUNTER__ __lint__COUNTER__
//lint +rw( *type_traits ) // Enable type traits support
#endif


#ifdef __cplusplus
} /* extern "C" */
#endif

#if _lint >= 909 // For 9.00i and later:
        //// __attribute__ is GCC's __attribute__:
        //
        //lint -rw_asgn(__attribute__,__gcc_attribute__)
        //lint -rw_asgn(__attribute,  __gcc_attribute__)
        //
        //// Prevent "__attribute__" from being defined as a macro:
        //
        //lint --u"__attribute__"
        //lint --u"__attribute"
        //
        //// Because an attribute-specifier is a form of
        //// declaration-modifier, and because it can appear at the
        //// beginning of a decl-specifier-seq, we must enable "Early
        //// Modifiers":
        //
        //lint +fem
#else // for 9.00h and earlier:
        //lint -d__attribute__()=
        //lint -d__attribute()= 
#endif

#endif /* _lint      */
/*lint -restore */
#endif /* CO_GCC_H_ */
