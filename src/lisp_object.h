#ifndef LISP_OBJECT_H
#define LISP_OBJECT_H

/* EMACS_INT - signed integer wide enough to hold an Emacs value
   EMACS_INT_MAX - maximum value of EMACS_INT; can be used in #if
   pI - printf length modifier for EMACS_INT
   EMACS_UINT - unsigned variant of EMACS_INT */
#ifndef EMACS_INT_MAX
# if LONG_MAX < LLONG_MAX && defined WIDE_EMACS_INT
typedef long long int EMACS_INT;
typedef unsigned long long int EMACS_UINT;
#  define EMACS_INT_MAX LLONG_MAX
#  define pI "ll"
# elif INT_MAX < LONG_MAX
typedef long int EMACS_INT;
typedef unsigned long int EMACS_UINT;
#  define EMACS_INT_MAX LONG_MAX
#  define pI "l"
# else
typedef int EMACS_INT;
typedef unsigned int EMACS_UINT;
#  define EMACS_INT_MAX INT_MAX
#  define pI ""
# endif
#endif


#ifdef CHECK_LISP_OBJECT_TYPE

typedef struct { EMACS_INT i; } Lisp_Object;

#define XLI(o) (o).i
LISP_INLINE Lisp_Object
XIL (EMACS_INT i)
{
  Lisp_Object o = { i };
  return o;
}

LISP_INLINE Lisp_Object
LISP_MAKE_RVALUE (Lisp_Object o)
{
    return o;
}

#define LISP_INITIALLY_ZERO {0}

#undef CHECK_LISP_OBJECT_TYPE
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = 1 };
#else /* CHECK_LISP_OBJECT_TYPE */

/* If a struct type is not wanted, define Lisp_Object as just a number.  */

typedef EMACS_INT Lisp_Object;
#define XLI(o) (o)
#define XIL(i) (i)
#define LISP_MAKE_RVALUE(o) (0 + (o))
#define LISP_INITIALLY_ZERO 0
enum CHECK_LISP_OBJECT_TYPE { CHECK_LISP_OBJECT_TYPE = 0 };
#endif /* CHECK_LISP_OBJECT_TYPE */


#endif /* LISP_OBJECT_H */
