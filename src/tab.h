#ifndef tab_INCLUDED
#define tab_INCLUDED

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class Tab
{
public:
    enum { DEFAULT_TAB_SIZE = 8 };

    inline static int TabSize() { return tab_size; }
    inline static void SetTabSize(int value) { tab_size = value; }

    static int Wcslen(wchar_t *line, int start, int end);

private:
    static int tab_size;
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // tab_INCLUDED

