#ifndef lpginput_INCLUDED
#define lpginput_INCLUDED

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

typedef TokenIndex TokenObject;
typedef TokenIndex Location;

inline Location Loc(TokenObject i) { return i; }

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#include "javasym.h" /* mapping of lexical symbols  */
#include "javadef.h" /* definition of parsing names */
#include "javaprs.h" /* parsing action functions    */

#endif // lpginput_INCLUDED
