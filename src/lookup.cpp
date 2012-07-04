// $Id: lookup.cpp,v 1.44 2002/06/21 04:30:29 cabbey Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "lookup.h"
#include "control.h"
#include "symbol.h"
#include "code.h"
#include "ast.h"
#include "case.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

PackageSymbol *Symbol::PackageCast()
{
    return DYNAMIC_CAST<PackageSymbol *, Symbol *>
        (_kind == PACKAGE ? this : NULL);
}

TypeSymbol *Symbol::TypeCast()
{
    return DYNAMIC_CAST<TypeSymbol *, Symbol *>
        (_kind == TYPE ? this : NULL);
}

MethodSymbol *Symbol::MethodCast()
{
    return DYNAMIC_CAST<MethodSymbol *, Symbol *>
        (_kind == METHOD ? this : NULL);
}

BlockSymbol *Symbol::BlockCast()
{
    return DYNAMIC_CAST<BlockSymbol *, Symbol *>
        (_kind == BLOCK ? this : NULL);
}

VariableSymbol *Symbol::VariableCast()
{
    return DYNAMIC_CAST<VariableSymbol *, Symbol *>
        (_kind == VARIABLE ? this : NULL);
}

LabelSymbol *Symbol::LabelCast()
{
    return DYNAMIC_CAST<LabelSymbol *, Symbol *>
        (_kind == LABEL ? this : NULL);
}

LiteralSymbol *Symbol::LiteralCast()
{
    return DYNAMIC_CAST<LiteralSymbol *, Symbol *>
        (_kind == LITERAL ? this : NULL);
}

NameSymbol *Symbol::NameCast()
{
    return DYNAMIC_CAST<NameSymbol *, Symbol *>
        (_kind == NAME ? this : NULL);
}

PathSymbol *Symbol::PathCast()
{
    return DYNAMIC_CAST<PathSymbol *, Symbol *>
        (_kind == PATH ? this : NULL);
}

DirectorySymbol *Symbol::DirectoryCast()
{
    return DYNAMIC_CAST<DirectorySymbol *, Symbol *>
        (_kind == _DIRECTORY ? this : NULL);
}

FileSymbol *Symbol::FileCast()
{
    return DYNAMIC_CAST<FileSymbol *, Symbol *>
        (_kind == _FILE ? this : NULL);
}

int SystemTable::primes[] = {DEFAULT_HASH_SIZE, 101, 401, MAX_HASH_SIZE};

SystemTable::SystemTable(int hash_size_) : directories(1024)
{
    hash_size = (hash_size_ <= 0 ? 1 : hash_size_);

    prime_index = -1;
    do
    {
        if (hash_size < primes[prime_index + 1])
            break;
        prime_index++;
    } while (primes[prime_index] < MAX_HASH_SIZE);

    base = (Element **) memset(new Element *[hash_size], 0,
                               hash_size * sizeof(Element *));
}

SystemTable::~SystemTable()
{
    for (int i = 0; i < directories.Length(); i++)
        delete directories[i];

    delete [] base;
}

void SystemTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (Element **) memset(new Element *[hash_size], 0,
                               hash_size * sizeof(Element *));

    for (int k = 0; k < directories.Length(); k++)
    {
        Element *element = directories[k];

        int i = hash(element -> device, element -> inode);
        element -> next = base[i];
        base[i] = element;
    }
}

DirectorySymbol *SystemTable::FindDirectorySymbol(dev_t device, ino_t inode)
{
    int k = hash(device, inode);

    for (Element *element = base[k]; element; element = element -> next)
    {
        if (element -> device == device && element -> inode == inode)
            return element -> directory_symbol;
    }

    return NULL;
}

void SystemTable::InsertDirectorySymbol(dev_t device, ino_t inode,
                                        DirectorySymbol *directory_symbol)
{
    int k = hash(device, inode);

    Element *element = new Element(device, inode, directory_symbol);
    directories.Next() = element;

    element -> next = base[k];
    base[k] = element;

    //
    // If the set is "adjustable" and the number of unique elements in it
    // exceeds 2 times the size of the base, and we have not yet reached the
    // maximum allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (directories.Length() > (hash_size << 1) &&
        hash_size < MAX_HASH_SIZE)
    {
        Rehash();
    }
}

int DirectoryTable::primes[] = {DEFAULT_HASH_SIZE, 2039, 4093, MAX_HASH_SIZE};

DirectoryTable::DirectoryTable(int estimate) : entry_pool(estimate),
                                               hash_size(primes[0]),
                                               prime_index(0)
{
    base = (DirectoryEntry **) memset(new DirectoryEntry *[hash_size], 0,
                                      hash_size * sizeof(DirectoryEntry *));
}

DirectoryTable::~DirectoryTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (Symbol *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the Name table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (Symbol *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/
    for (int i = 0; i < entry_pool.Length(); i++)
        delete entry_pool[i];
    delete [] base;
}


DirectoryEntry *DirectoryTable::FindEntry(char *str, int len)
{
    int k = Hash(str, len) % hash_size;
    DirectoryEntry *entry;
    for (entry = base[k]; entry; entry = entry -> next)
    {
        if (len == entry -> length && memcmp(entry -> name, str,
                                             len * sizeof(char)) == 0)
        {
            return (entry -> IsDummy() ? (DirectoryEntry *) NULL : entry);
        }
    }

    return NULL;
}


void DirectoryTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (DirectoryEntry **) memset(new DirectoryEntry *[hash_size], 0,
                                      hash_size * sizeof(DirectoryEntry *));

    for (int i = 0; i < entry_pool.Length(); i++)
    {
        DirectoryEntry *e = entry_pool[i];
        int k = Hash(e -> name, e -> length) % hash_size;
        e -> next = base[k];
        base[k] = e;
    }
}


DirectoryEntry *DirectoryTable::InsertEntry(DirectorySymbol *directory_symbol,
                                            char *str, int len)
{
    int k = Hash(str, len) % hash_size;
    DirectoryEntry *entry;
    for (entry = base[k]; entry; entry = entry -> next)
    {
        if (len == entry -> length && memcmp(entry -> name, str,
                                             len * sizeof(char)) == 0)
            return entry;
    }

    entry = new DirectoryEntry();
    entry_pool.Next() = entry;
    entry -> Initialize(directory_symbol, str, len);

    entry -> next = base[k];
    base[k] = entry;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (entry_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return entry;
}


#ifdef WIN32_FILE_SYSTEM
DirectoryEntry *DirectoryTable::FindCaseInsensitiveEntry(char *name,
                                                         int length)
{
    char *lower_name = new char[length + 1];
    for (int i = 0; i < length; i++)
        lower_name[i] = Case::ToAsciiLower(name[i]);
    lower_name[length] = U_NULL;

    DirectoryEntry *entry = FindEntry(lower_name, length);
    delete [] lower_name;

    return (entry ? entry -> Image() : entry);
}

void DirectoryTable::InsertCaseInsensitiveEntry(DirectoryEntry *image)
{
    int length = image -> length;
    char *lower_name = new char[length + 1];
    for (int i = 0; i < length; i++)
        lower_name[i] = Case::ToAsciiLower(image -> name[i]);
    lower_name[length] = U_NULL;

    int k = Hash(lower_name, length) % hash_size;
    DirectoryEntry *entry;
    for (entry = base[k]; entry; entry = entry -> next)
    {
        if (length == entry -> length && memcmp(entry -> name, lower_name,
                                                length * sizeof(char)) == 0)
            break;
    }

    if (! entry)
    {
        FoldedDirectoryEntry *folded_entry = new FoldedDirectoryEntry(image);
        entry_pool.Next() = folded_entry;
        folded_entry -> Initialize(image, lower_name, length);

        folded_entry -> next = base[k];
        base[k] = folded_entry;

        //
        // If the number of unique elements in the hash table exceeds 2 times
        // the size of the base, and we have not yet reached the maximum
        // allowable size for a base, reallocate a larger base and rehash
        // the elements.
        //
        if (entry_pool.Length() > (hash_size << 1) &&
            hash_size < MAX_HASH_SIZE)
        {
            Rehash();
        }
    }

    delete [] lower_name;
}
#endif


time_t DirectoryEntry::Mtime()
{
    if (mtime_ == 0)
    {
        char *dirname = this -> directory -> DirectoryName();
        int length = this -> directory -> DirectoryNameLength() +
            this -> length + 1; // +1 for '/'
        char *file_name = new char[length + 1];
        strcpy(file_name, dirname);
        if (dirname[this -> directory -> DirectoryNameLength() - 1] != U_SLASH)
            strcat(file_name, StringConstant::U8S_SL);
        strcat(file_name, this -> name);

        struct stat status;
        if (JikesAPI::getInstance()->stat(file_name, &status) == 0)
             mtime_ = status.st_mtime;
        else assert(false && "Cannot compute system time stamp\n");

        delete [] file_name;
    }

    return mtime_;
}


int NameLookupTable::primes[] = {
    DEFAULT_HASH_SIZE, 8191, 16411, MAX_HASH_SIZE
};

NameLookupTable::NameLookupTable(int estimate) : symbol_pool(estimate),
                                                 hash_size(primes[0]),
                                                 prime_index(0)
{
    base = (NameSymbol **) memset(new NameSymbol *[hash_size], 0,
                                  hash_size * sizeof(NameSymbol *));
}

NameLookupTable::~NameLookupTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (Symbol *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the Name table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (Symbol *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/
    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


void NameLookupTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (NameSymbol **) memset(new NameSymbol *[hash_size], 0,
                                  hash_size * sizeof(NameSymbol *));

    for (int i = 0; i < symbol_pool.Length(); i++)
    {
        NameSymbol *ns = symbol_pool[i];
        int k = ns -> hash_address % hash_size;
        ns -> next = base[k];
        base[k] = ns;
    }
}


NameSymbol *NameLookupTable::FindOrInsertName(wchar_t *str, size_t len)
{
    unsigned hash_address = Hash(str, len);
    int k = hash_address % hash_size;
    NameSymbol *symbol;
    for (symbol = base[k]; symbol; symbol = (NameSymbol *) symbol -> next)
    {
        if (len == symbol -> NameLength() &&
            memcmp(symbol -> Name(), str, len * sizeof(wchar_t)) == 0)
        {
            return symbol;
        }
    }

    int index = symbol_pool.Length(); // index of the next element
    symbol = new NameSymbol();
    symbol_pool.Next() = symbol;
    symbol -> Initialize(str, len, hash_address, index);

    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return symbol;
}


int TypeLookupTable::primes[] = {
    DEFAULT_HASH_SIZE, 8191, 16411, MAX_HASH_SIZE
};

TypeLookupTable::TypeLookupTable(int estimate) : symbol_pool(estimate),
                                                 hash_size(primes[0]),
                                                 prime_index(0)
{
    base = (TypeSymbol **) memset(new TypeSymbol *[hash_size], 0,
                                  hash_size * sizeof(TypeSymbol *));
}


TypeLookupTable::~TypeLookupTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (TypeSymbol *s = base[n]; s; s = s -> next_type)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the Type table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (TypeSymbol *s = base[n]; s; s = s -> next_type)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/

    delete [] base;
}


void TypeLookupTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (TypeSymbol **) memset(new TypeSymbol *[hash_size], 0,
                                  hash_size * sizeof(TypeSymbol *));

    for (int i = 0; i < symbol_pool.Length(); i++)
    {
        TypeSymbol *type = symbol_pool[i];
        int k = type -> hash_address % hash_size;
        type -> next_type = base[k];
        base[k] = type;
    }
}


TypeSymbol *TypeLookupTable::FindType(const char *str, int len)
{
    unsigned hash_address = Hash(str, len);
    int k = hash_address % hash_size;

    for (TypeSymbol *type = base[k]; type; type = type -> next_type)
    {
        assert(type -> fully_qualified_name);

        Utf8LiteralValue *fully_qualified_name = type -> fully_qualified_name;
        if (len == fully_qualified_name -> length &&
            memcmp(fully_qualified_name -> value, str,
                   len * sizeof(char)) == 0)
        {
            return type;
        }
    }

    return NULL;
}


void TypeLookupTable::InsertType(TypeSymbol *type)
{
    assert(type && type -> fully_qualified_name);

    unsigned hash_address = Hash(type -> fully_qualified_name -> value,
                                 type -> fully_qualified_name -> length);
    int k = hash_address % hash_size;

#ifdef JIKES_DEBUG
    for (TypeSymbol *t = base[k]; t; t = t -> next_type)
        assert (type != t && "Type was already entered in type table");
#endif

    symbol_pool.Next() = type;
    type -> hash_address = hash_address;

    type -> next_type = base[k];
    base[k] = type;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();
}


//
// Remove all elements from the table.
//
void TypeLookupTable::SetEmpty()
{
    symbol_pool.Reset();
    (void) memset(base, 0, hash_size * sizeof(TypeSymbol *));
}


int IntLiteralTable::int32_limit = 0x7FFFFFFF / 10;
int IntLiteralTable::primes[] = {
    DEFAULT_HASH_SIZE, 8191, 16411, MAX_HASH_SIZE
};

IntLiteralTable::IntLiteralTable(LiteralValue *bad_value_)
    : symbol_pool(16384),
      hash_size(primes[0]),
      prime_index(0),
      bad_value(bad_value_)
{
    base = (IntLiteralValue **) memset(new IntLiteralValue *[hash_size], 0,
                                       hash_size * sizeof(IntLiteralValue *));
    symbol_pool.Next() = NULL; // do not use the 0th element
}

IntLiteralTable::~IntLiteralTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the integer table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/

    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


LiteralValue *IntLiteralTable::FindOrInsertChar(LiteralSymbol *literal)
{
    wchar_t *name = literal -> Name();
    int len = literal -> NameLength();

    if (len == 0) // An isolated quote.
        return literal -> value = bad_value;
    if (len == 1) // A regular character.
        return literal -> value = FindOrInsert((int) name[0]);

    int value = -1;

    if (name[0] == U_BACKSLASH)
        switch (name[1])
        {
        case U_b:
            value = U_BACKSPACE;
            break;
        case U_f:
            value = U_FORM_FEED;
            break;
        case U_n:
            value = U_LINE_FEED;
            break;
        case U_r:
            value = U_CARRIAGE_RETURN;
            break;
        case U_t:
            value = U_HORIZONTAL_TAB;
            break;
        case U_DOUBLE_QUOTE:
        case U_SINGLE_QUOTE:
        case U_BACKSLASH:
            value = name[1];
            break;
        case U_0:
        case U_1:
        case U_2:
        case U_3:
        case U_4:
        case U_5:
        case U_6:
        case U_7:
            {
                value = 0;
                int i = 0;
                while (++i < len)
                    value = value * 8 + name[i] - U_0;
            }
        }

    return literal -> value = (value < 0 || value > 65535 ? bad_value
                               : FindOrInsert(value));
}


LiteralValue *IntLiteralTable::FindOrInsertHexInt(LiteralSymbol *literal)
{
    wchar_t *head = literal -> Name() + 1, // point to X
            *tail = &literal -> Name()[literal -> NameLength() - 1];

    u4 uvalue = 0;

    for (++head; tail > head && *head == U_0; head++)
        ; // skip leading zeroes
    head--;

    for (int i = 0; i < 32 && tail > head; i += 4, tail--)
    {
        u4 d = *tail - (Code::IsDigit(*tail) ? U_0 : (Code::IsLower(*tail)
                                                      ? U_a : U_A) - 10);
        uvalue |= (d << i);
    }

    return (tail > head ? bad_value : FindOrInsert((int) uvalue));
}


LiteralValue *IntLiteralTable::FindOrInsertOctalInt(LiteralSymbol *literal)
{
    wchar_t *head = literal -> Name(), // point to initial '0'
            *tail = &head[literal -> NameLength() - 1];

    u4 uvalue = 0;
    for (++head; tail > head && *head == U_0; head++) // skip leading zeroes
        ;
    head--;

    for (int i = 0; i < 30 && tail > head; i += 3, tail--)
    {
        u4 d = *tail - U_0;
        uvalue |= (d << i);
    }

    if (tail > head)
    {
        u4 d = *tail - U_0;

        if (d <= 3) // max number that can fit in 2 bits
        {
            tail--;
            uvalue |= (d << 30);
        }
    }

    return (tail > head ? bad_value : FindOrInsert((int) uvalue));
}


LiteralValue *IntLiteralTable::FindOrInsertInt(LiteralSymbol *literal)
{
    wchar_t *name = literal -> Name();

    if (name[0] == U_0)
        literal -> value = (name[1] == U_x || name[1] == U_X
                            ? FindOrInsertHexInt(literal)
                            : FindOrInsertOctalInt(literal));
    else
    {
        int value = 0;

        wchar_t *p;
        for (p = name; *p; p++)
        {
            int digit = *p - U_0;
            if (value > int32_limit || (value == int32_limit && digit > 7))
                break;
            value = value * 10 + digit;
        }

        literal -> value = (*p ? bad_value : FindOrInsert(value));
    }

    return literal -> value;
}


LiteralValue *IntLiteralTable::FindOrInsertNegativeInt(LiteralSymbol *literal)
{
    if (literal -> value && literal -> value != bad_value)
    {
        // A positive value already exists.
        IntLiteralValue *int_literal = (IntLiteralValue *) literal -> value;
        return FindOrInsert(- int_literal -> value);
    }

    wchar_t *name = literal -> Name();

    //
    // We can assert that the name of a literal contains at least two
    // characters: at least one digit and the terminating '\0'.
    //
    if (name[0] == U_0)
    {
        IntLiteralValue *int_literal =
            (IntLiteralValue *) (name[1] == U_x || name[1] == U_X
                                 ? FindOrInsertHexInt(literal)
                                 : FindOrInsertOctalInt(literal));
        return FindOrInsert(- int_literal -> value);
    }

    int value = 0;

    wchar_t *p;
    for (p = name; *p; p++)
    {
        int digit = *p - U_0;
        if (value > int32_limit || (value == int32_limit && digit > 8))
            break;
        value = value * 10 + digit;
    }

    return (*p ? bad_value : FindOrInsert(-value));
}


void IntLiteralTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (IntLiteralValue **) memset(new IntLiteralValue *[hash_size], 0,
                                       hash_size * sizeof(IntLiteralValue *));

    //
    // Recall that the 0th element is unused.
    //
    for (int i = 1; i < symbol_pool.Length(); i++)
    {
        IntLiteralValue *ilv = symbol_pool[i];
        // The unsigned casting turns the negative values into positive values.
        int k = ((unsigned) ilv -> value) % hash_size;
        ilv -> next = base[k];
        base[k] = ilv;
    }
}


IntLiteralValue *IntLiteralTable::Find(int value)
{
    // The unsigned casting turns the negative values into positive values.
    int k = ((unsigned) value) % hash_size;

    IntLiteralValue *lit = NULL;
    for (lit = base[k]; lit; lit = (IntLiteralValue *) lit -> next)
    {
        if (lit -> value == value)
            break;
    }

    return lit;
}


IntLiteralValue *IntLiteralTable::FindOrInsert(int value)
{
    // The unsigned casting turns the negative values into positive values.
    int k = ((unsigned) value) % hash_size;

    IntLiteralValue *lit;
    for (lit = base[k]; lit; lit = (IntLiteralValue *) lit -> next)
    {
        if (lit -> value == value)
            return lit;
    }

    lit = new IntLiteralValue();
    lit -> Initialize(value, symbol_pool.Length());
    symbol_pool.Next() = lit;

    lit -> next = base[k];
    base[k] = lit;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return lit;
}


LongInt LongLiteralTable::int64_limit = LongInt(0x7FFFFFFF, 0xFFFFFFFF) / 10;
int LongLiteralTable::primes[] = {
    DEFAULT_HASH_SIZE, 2039, 4093, MAX_HASH_SIZE
};

LongLiteralTable::LongLiteralTable(LiteralValue *bad_value_)
    : symbol_pool(16384),
      hash_size(primes[0]),
      prime_index(0),
      bad_value(bad_value_)
{
    base = (LongLiteralValue **) memset(new LongLiteralValue *[hash_size], 0,
                                        hash_size * sizeof(LongLiteralValue *));
    symbol_pool.Next() = NULL; // do not use the 0th element
}

LongLiteralTable::~LongLiteralTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the long table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/

    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


LiteralValue *LongLiteralTable::FindOrInsertHexLong(LiteralSymbol *literal)
{
    u4 high = 0,
       low = 0;

    wchar_t *head = literal -> Name() + 1, // point to X
        // -2 to skip the 'L' suffix
            *tail = &literal -> Name()[literal -> NameLength() - 2];

    for (++head; tail > head && *head == U_0; head++) // skip leading zeroes
        ;
    head--;

    for (int i = 0; i < 32 && tail > head; i += 4, tail--)
    {
        u4 d = *tail - (Code::IsDigit(*tail) ? U_0 : (Code::IsLower(*tail)
                                                      ? U_a : U_A) - 10);
        low |= (d << i);
    }

    for (int j = 0; j < 32 && tail > head; j += 4, tail--)
    {
        u4 d = *tail - (Code::IsDigit(*tail) ? U_0 : (Code::IsLower(*tail)
                                                      ? U_a : U_A) - 10);
        high |= (d << j);
    }

    return (tail > head ? bad_value : FindOrInsert(LongInt(high, low)));
}


LiteralValue *LongLiteralTable::FindOrInsertOctalLong(LiteralSymbol *literal)
{
    wchar_t *head = literal -> Name(), // point to initial '0'
        // -2 to skip the 'L' suffix
            *tail = &head[literal -> NameLength() - 2];

    ULongInt uvalue = 0;
    for (++head; tail > head && *head == U_0; head++) // skip leading zeroes
        ;
    head--;

    for (int i = 0; i < 63 && tail > head; i += 3, tail--)
    {
        ULongInt d = (u4) (*tail - U_0);
        uvalue |= (d << i);
    }

    if (tail > head)
    {
        u4 d = *tail - U_0;

        if (d <= 1) // max number that can fit in 1 bit
        {
            tail--;
            uvalue |= ULongInt((d << 31), 0);
        }
    }

    return (tail > head ? bad_value : FindOrInsert((LongInt) uvalue));
}


LiteralValue *LongLiteralTable::FindOrInsertLong(LiteralSymbol *literal)
{
    wchar_t *name = literal -> Name();

    //
    // We can assert that the name of a literal contains at least two
    // characters: at least one digit and the terminating '\0'.
    //
    if (name[0] == U_0)
        literal -> value = (name[1] == U_x || name[1] == U_X
                            ? FindOrInsertHexLong(literal)
                            : FindOrInsertOctalLong(literal));
    else
    {
        LongInt value = 0;

        wchar_t *p;
        for (p = name; *p != U_L && *p != U_l; p++)
        {
            u4 digit = *p - U_0;
            if (value > int64_limit || (value == int64_limit && digit > 7))
                break;
            value = value * 10 + digit;
        }

        literal -> value = (*p != U_L && *p != U_l ? bad_value
                            : FindOrInsert(value));
    }

    return literal -> value;
}


LiteralValue *LongLiteralTable::FindOrInsertNegativeLong(LiteralSymbol *literal)
{
    // A positive value already exists.
    if (literal -> value && literal -> value != bad_value)
    {
        LongLiteralValue *long_literal = (LongLiteralValue *) literal -> value;
        return FindOrInsert(- long_literal -> value);
    }

    wchar_t *name = literal -> Name();
    //
    // We can assert that the name of a literal contains at least two
    // characters: at least one digit and the terminating '\0'.
    //
    if (name[0] == U_0)
    {
        LongLiteralValue *long_literal =
            (LongLiteralValue *) (name[1] == U_x || name[1] == U_X
                                  ? FindOrInsertHexLong(literal)
                                  : FindOrInsertOctalLong(literal));
        return FindOrInsert(- long_literal -> value);
    }

    LongInt value = 0;

    wchar_t *p;
    for (p = name; *p != U_L && *p != U_l && value >= 0; p++)
    {
        u4 digit = *p - U_0;
        if (value > int64_limit || (value == int64_limit && digit > 8))
            break;
        value = value * 10 + digit;
    }

    return (*p != U_L && *p != U_l ? bad_value : FindOrInsert(-value));
}


void LongLiteralTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (LongLiteralValue **) memset(new LongLiteralValue *[hash_size], 0,
                                        hash_size * sizeof(LongLiteralValue *));

    //
    // Recall that the 0th element is unused.
    //
    for (int i = 1; i < symbol_pool.Length(); i++)
    {
        LongLiteralValue *llv = symbol_pool[i];
        // The hash function for LongInt values is cheap so we don't need to
        // save it.
        int k = Hash(llv -> value) % hash_size;
        llv -> next = base[k];
        base[k] = llv;
    }
}


LongLiteralValue *LongLiteralTable::FindOrInsert(LongInt value)
{
    int k = Hash(value) % hash_size;

    LongLiteralValue *lit;
    for (lit = base[k]; lit; lit = (LongLiteralValue *) lit -> next)
    {
        if (lit -> value == value)
            return lit;
    }

    lit = new LongLiteralValue();
    lit -> Initialize(value, symbol_pool.Length());
    symbol_pool.Next() = lit;

    lit -> next = base[k];
    base[k] = lit;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return lit;
}


int FloatLiteralTable::primes[] = {
    DEFAULT_HASH_SIZE, 2039, 4093, MAX_HASH_SIZE
};

FloatLiteralTable::FloatLiteralTable(LiteralValue *bad_value_)
    : symbol_pool(16384),
      hash_size(primes[0]),
      prime_index(0),
      bad_value(bad_value_)
{
    base = (FloatLiteralValue **) memset(new FloatLiteralValue *[hash_size], 0,
                                         hash_size * sizeof(FloatLiteralValue *));
    symbol_pool.Next() = NULL; // do not use the 0th element
}

FloatLiteralTable::~FloatLiteralTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the float table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/

    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


LiteralValue *FloatLiteralTable::FindOrInsertFloat(LiteralSymbol *literal)
{
    char *name = new char[literal -> NameLength() + 1];
    for (size_t i = 0; i < literal -> NameLength(); i++)
        name[i] = (char) literal -> Name()[i];
    name[literal -> NameLength()] = U_NULL;

    //
    // JLS 3.10.2 states it is an error for a literal to round to infinity or 0
    // Passing the second parameter tells the constructor to set value to NaN
    // if the literal is invalid.
    //
    IEEEfloat value = IEEEfloat(name, true);

    literal -> value = (value.IsNaN() ? bad_value : FindOrInsert(value));

    delete [] name;

    return literal -> value;
}


void FloatLiteralTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (FloatLiteralValue **) memset(new FloatLiteralValue *[hash_size], 0,
                                         hash_size * sizeof(FloatLiteralValue *));

    //
    // Recall that the 0th element is unused.
    //
    for (int i = 1; i < symbol_pool.Length(); i++)
    {
        FloatLiteralValue *flv = symbol_pool[i];
        // The hash function for float values is cheap so we don't need to
        // save it.
        int k = Hash(flv -> value) % hash_size;
        flv -> next = base[k];
        base[k] = flv;
    }
}


FloatLiteralValue *FloatLiteralTable::FindOrInsert(IEEEfloat value)
{
    int k = Hash(value) % hash_size;

    FloatLiteralValue *lit;
    for (lit = base[k]; lit; lit = (FloatLiteralValue *) lit -> next)
    {
        if (lit -> value.equals(value))
            return lit;
    }

    lit = new FloatLiteralValue();
    lit -> Initialize(value, symbol_pool.Length());
    symbol_pool.Next() = lit;

    lit -> next = base[k];
    base[k] = lit;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return lit;
}


int DoubleLiteralTable::primes[] = {
    DEFAULT_HASH_SIZE, 2039, 4093, MAX_HASH_SIZE
};

DoubleLiteralTable::DoubleLiteralTable(LiteralValue *bad_value_)
    : symbol_pool(16384),
      hash_size(primes[0]),
      prime_index(0),
      bad_value(bad_value_)
{
    base = (DoubleLiteralValue **) memset(new DoubleLiteralValue *[hash_size],
                                          0,
                                          hash_size * sizeof(DoubleLiteralValue *));
    symbol_pool.Next() = NULL; // do not use the 0th element
}

DoubleLiteralTable::~DoubleLiteralTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the double table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/
    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


LiteralValue *DoubleLiteralTable::FindOrInsertDouble(LiteralSymbol *literal)
{
    char *name = new char[literal -> NameLength() + 1];
    for (size_t i = 0; i < literal -> NameLength(); i++)
        name[i] = (char) literal -> Name()[i];
    name[literal -> NameLength()] = U_NULL;

    //
    // JLS 3.10.2 states it is an error for a literal to round to infinity or 0
    // Passing the second parameter tells the constructor to set value to NaN
    // if the literal is invalid.
    //
    IEEEdouble value = IEEEdouble(name, true);

    literal -> value = (value.IsNaN() ? bad_value : FindOrInsert(value));

    delete [] name;

    return literal -> value;
}


void DoubleLiteralTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (DoubleLiteralValue **) memset(new DoubleLiteralValue *[hash_size],
                                          0,
                                          hash_size * sizeof(DoubleLiteralValue *));

    //
    // Recall that the 0th element is unused.
    //
    for (int i = 1; i < symbol_pool.Length(); i++)
    {
        DoubleLiteralValue *dlv = symbol_pool[i];
        // The hash function for double values is cheap so we don't need to
        // save it.
        int k = Hash(dlv -> value) % hash_size;
        dlv -> next = base[k];
        base[k] = dlv;
    }
}


DoubleLiteralValue *DoubleLiteralTable::FindOrInsert(IEEEdouble value)
{
    int k = Hash(value) % hash_size;

    DoubleLiteralValue *lit;
    for (lit = base[k]; lit; lit = (DoubleLiteralValue *) lit -> next)
    {
        if (lit -> value.equals(value))
            return lit;
    }

    lit = new DoubleLiteralValue();
    lit -> Initialize(value, symbol_pool.Length());
    symbol_pool.Next() = lit;

    lit -> next = base[k];
    base[k] = lit;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return lit;
}


LiteralValue *Utf8LiteralTable::FindOrInsertString(LiteralSymbol *literal)
{
    wchar_t *name = literal -> Name();

    int literal_length = literal -> NameLength();

    // Big enough for the worst case: 3 bytes/char + \0.
    char *value = new char[literal_length * 3 + 1];
    int len = 0;
    int i = -1;

    while (++i < literal_length)
    {
        int ch = name[i];
        if (ch == U_BACKSLASH)
        {
            ch = 0;
            switch (name[++i])
            {
            case U_b:
                ch = U_BACKSPACE;
                break;
            case U_f:
                ch = U_FORM_FEED;
                break;
            case U_n:
                ch = U_LINE_FEED;
                break;
            case U_r:
                ch = U_CARRIAGE_RETURN;
                break;
            case U_t:
                ch = U_HORIZONTAL_TAB;
                break;
            case U_DOUBLE_QUOTE:
            case U_SINGLE_QUOTE:
            case U_BACKSLASH:
                ch = name[i];
                break;
            case U_0:
            case U_1:
            case U_2:
            case U_3:
                ch = name[i] - U_0;
                if (! Code::IsOctalDigit(name[i + 1]))
                    break;
                i++;
                // fallthrough
            case U_4:
            case U_5:
            case U_6:
            case U_7:
                ch = ch * 8 + name[i] - U_0;
                if (! Code::IsOctalDigit(name[i + 1]))
                    break;
                ch = ch * 8 + name[++i] - U_0;
                break;
            default:
                ch = -1;
            }
        }
        else if (Code::IsNewline(ch))
            ch = -1;

        if (ch < 0)
            break;
        else if (ch == 0)
        {
             value[len++] = (char) 0xC0;
             value[len++] = (char) 0x80;
        }
        else if (ch <= 0x007F)
             value[len++] = (char) ch;
        else if (ch <= 0x07FF)
        {
             value[len++] = (char) (0x0C0 | ((ch >> 6) & 0x01F));
             value[len++] = (char) (0x080 | (ch & 0x03F));
        }
        else
        {
             value[len++] = (char) (0x0E0 | ((ch >> 12) & 0x0F));
             value[len++] = (char) (0x080 | ((ch >> 6) & 0x03F));
             value[len++] = (char) (0x080 | (ch & 0x03F));
        }
    }

    value[len] = U_NULL;
    literal -> value = (i < literal_length ? bad_value
                        : FindOrInsert(value, len));

    delete [] value;
    return literal -> value;
}


Utf8LiteralValue *Utf8LiteralTable::FindOrInsert(wchar_t ch)
{
    int len = 0;
    char str[4];

    if (ch == 0)
    {
         str[len++] = (char) 0xC0;
         str[len++] = (char) 0x80;
    }
    else if (ch <= 0x007F)
         str[len++] = (char) ch;
    else if (ch <= 0x07FF)
    {
         str[len++] = (char) (0x0C0 | ((ch >> 6) & 0x01F));
         str[len++] = (char) (0x080 | (ch & 0x03F));
    }
    else
    {
         str[len++] = (char) (0x0E0 | (char) ((ch >> 12) & 0x0F));
         str[len++] = (char) (0x080 | (char) ((ch >> 6) & 0x03F));
         str[len++] = (char) (0x080 | (char) (ch & 0x03F));
    }

    str[len] = U_NULL;

    return FindOrInsert(str, len);
}


void Utf8LiteralTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (Utf8LiteralValue **) memset(new Utf8LiteralValue *[hash_size], 0,
                                        hash_size * sizeof(Utf8LiteralValue *));

    //
    // Recall that the 0th element is unused.
    //
    for (int i = 1; i < symbol_pool.Length(); i++)
    {
        Utf8LiteralValue *ulv = symbol_pool[i];
        int k = ulv -> hash_address % hash_size;
        ulv -> next = base[k];
        base[k] = ulv;
    }
}


int Utf8LiteralTable::primes[] = {
    DEFAULT_HASH_SIZE, 8191, 16411, MAX_HASH_SIZE
};

Utf8LiteralTable::Utf8LiteralTable(LiteralValue *bad_value_)
    : symbol_pool(16384),
      hash_size(primes[0]),
      prime_index(0),
      bad_value(bad_value_)
{
    base = (Utf8LiteralValue **) memset(new Utf8LiteralValue *[hash_size], 0,
                                        hash_size * sizeof(Utf8LiteralValue *));
    symbol_pool.Next() = NULL; // do not use the 0th element
}


Utf8LiteralTable::~Utf8LiteralTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the Utf8 table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (LiteralValue *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/
    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


Utf8LiteralValue *Utf8LiteralTable::FindOrInsert(const char *str, int len)
{
    unsigned hash_address = Hash(str, len);
    int k = hash_address % hash_size;

    Utf8LiteralValue *lit;
    for (lit = base[k]; lit; lit = (Utf8LiteralValue *) lit -> next)
    {
        if (len == lit -> length)
        {
            if (memcmp(lit -> value, str, len * sizeof(char)) == 0)
                 return lit;
        }
    }

    lit = new Utf8LiteralValue();
    lit -> Initialize(str, len, hash_address, symbol_pool.Length());
    symbol_pool.Next() = lit;

    lit -> next = base[k];
    base[k] = lit;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return lit;
}


//
// Collapses all known strings in an expression chain into the leftmost one;
// since the others in the chain have been set to "", this allows the emitter
// to use a single call to StringBuffer.append() for the entire chain.
//
void Utf8LiteralTable::CollectStrings()
{
    int count = utf8_literals -> Length();
    assert(count && leftmost_constant_expr);
    if (count == 1)
    {
        if (! leftmost_constant_expr -> NullLiteralCast())
            leftmost_constant_expr -> value = (*utf8_literals)[0];
    }
    else
    {
        int length = 0;
        for (int i = 0; i < count; i++)
            length += (*utf8_literals)[i] -> length;
        char *str = new char[length + 1]; // +1 for '\0'

        int index = 0;
        for (int k = 0; k < count; k++)
        {
            Utf8LiteralValue *literal = (*utf8_literals)[k];
            assert(literal -> value);

            memmove(&str[index], literal -> value,
                    literal -> length * sizeof(char));
            index += literal -> length;
        }
        str[length] = U_NULL;

        leftmost_constant_expr -> value = FindOrInsert(str, length);

        delete [] str;
    }
    utf8_literals -> Reset();
    leftmost_constant_expr = NULL;
}


//
// The return value is true iff leftmost_constant_expr != NULL; in other words,
// if the current expression ends in a known String value which can be chained
// to the next expression. As a side effect, if the expression is constant, it
// is in the growing tuple of known strings seen so far; and if the expression
// is not constant, all strings in the tuple are collected into the leftmost
// constant of the previous chain.
//
bool Utf8LiteralTable::EndsInKnownString(AstExpression *expression)
{
    if (expression -> IsConstant())
    {
        //
        // CollectStrings only works with Utf8LiteralValue* types, which
        // previous code in expr.cpp has already calculated. Here, we replace
        // constants with blank strings, and later we replace the left-most
        // constant with the concatenated version, so that expressions like
        // (nonconst + "a") + "b"; become (nonconst + "ab") + "";.  The
        // bytecode emitter is then smart enough to ignore the "".
        //
        Utf8LiteralValue *literal =
            DYNAMIC_CAST<Utf8LiteralValue *> (expression -> value);
        assert(literal -> value);

        utf8_literals -> Next() = literal;
        if (! leftmost_constant_expr)
            leftmost_constant_expr = expression;
        else
            expression -> value = FindOrInsert("", 0);
        return true;
    }

    AstBinaryExpression *binary_expr = expression -> BinaryExpressionCast();
    AstCastExpression *cast_expr = expression -> CastExpressionCast();
    AstParenthesizedExpression *paren_expr =
        expression -> ParenthesizedExpressionCast();
    AstNullLiteral *null_expr = expression -> NullLiteralCast();
    if (binary_expr)
    {
        //
        // If either subexpression is a constant but not a String, we have
        // already assigned it a Utf8LiteralValue.  But if a subexpression
        // is of type String, we don't know if it is constant yet.  Therefore,
        // we recurse to append the constant String for a primitive
        // expression, as well as to check if a String expression is constant.
        // This relies on the fact that this binary expression is of type
        // String. Remember that the null literal is not constant.
        //
        AstExpression *left  = binary_expr -> left_expression,
                      *right = binary_expr -> right_expression;
        if (left -> IsConstant() ||
            left -> Type() == expression -> Type())
        {
            EndsInKnownString(left);
        }
        if ((right -> IsConstant() ||
             right -> Type() == expression -> Type()) &&
            EndsInKnownString(right))
        {
            if (leftmost_constant_expr == left &&
                ! left -> NullLiteralCast() && ! right -> NullLiteralCast())
            {
                leftmost_constant_expr = binary_expr;
            }
            else
                right -> symbol = expression -> Type();
            return true;
        }
    }
    else if (cast_expr && EndsInKnownString(cast_expr -> expression))
    {
        //
        // If we get here, the subexpression is necessarily a constant String;
        // but this cast is constant only if it is to type String.
        //
        if (leftmost_constant_expr == cast_expr -> expression &&
            cast_expr -> expression -> Type() == cast_expr -> Type())
        {
            leftmost_constant_expr = cast_expr;
        }
        return true;
    }
    else if (paren_expr && EndsInKnownString(paren_expr -> expression))
    {
        if (leftmost_constant_expr == paren_expr -> expression &&
            ! leftmost_constant_expr -> NullLiteralCast())
        {
            leftmost_constant_expr = paren_expr;
        }
        return true;
    }
    else if (null_expr)
    {
        //
        // We are careful that null is never given a string value unless it is
        // part of a chain of strings, as it is not a compile-time constant.
        //
        utf8_literals -> Next() = FindOrInsert(StringConstant::U8S_null, 4);
        if (! leftmost_constant_expr)
            leftmost_constant_expr = expression;
        else
            expression -> value = FindOrInsert("", 0);
        return true;
    }

    if (leftmost_constant_expr)
        CollectStrings();
    return false; // Not a constant String expression
}


//
// This method flattens all known String expressions in the tree into a minimal
// number of utf8 literals. Note that it even flattens non-constant expressions
// (such as (Object)"ab", or null), when there are no side effects which could
// get in the way.  After this method, expression -> IsConstant() will return
// the correct value, but some intermediate subexpressions may return a
// harmless false negative.
//
void Utf8LiteralTable::CheckStringConstant(AstExpression *expression)
{
    utf8_literals = new Tuple<Utf8LiteralValue *>(256);
    leftmost_constant_expr = NULL;

    if (EndsInKnownString(expression))
        CollectStrings();

    delete utf8_literals;
}


int LiteralLookupTable::primes[] = {
    DEFAULT_HASH_SIZE, 2039, 4093, MAX_HASH_SIZE
};

LiteralLookupTable::LiteralLookupTable() : symbol_pool(16384),
                                           hash_size(primes[0]),
                                           prime_index(0)
{
    base = (LiteralSymbol **) memset(new LiteralSymbol *[hash_size], 0,
                                     hash_size * sizeof(LiteralSymbol *));
}

LiteralLookupTable::~LiteralLookupTable()
{
/*
int n;
int num_slots = 0;
int total = 0;
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (Symbol *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
{
num_slots++;
total += num;
}
}

if (num_slots > 0)
{
Coutput << "\nDestroying the Literal table with " << total
        << " elements and base size " << hash_size << " containing "
        << num_slots << " non-empty slots\n";
for (n = 0; n < hash_size; n++)
{
int num = 0;
for (Symbol *s = base[n]; s; s = s -> next)
    num++;
if (num > 0)
Coutput << "    slot " << n << " contains " << num << " element(s)\n";
}
}
if (hash_size < total)
    total = total;
*/
    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];
    delete [] base;
}


void LiteralLookupTable::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (LiteralSymbol **) memset(new LiteralSymbol *[hash_size], 0,
                                     hash_size * sizeof(LiteralSymbol *));

    for (int i = 0; i < symbol_pool.Length(); i++)
    {
        LiteralSymbol *ls = symbol_pool[i];
        int k = ls -> hash_address % hash_size;
        ls -> next = base[k];
        base[k] = ls;
    }
}


LiteralSymbol *LiteralLookupTable::FindOrInsertLiteral(wchar_t *str,
                                                       size_t len)
{
    unsigned hash_address = Hash(str, len);
    int k = hash_address % hash_size;
    LiteralSymbol *symbol;
    for (symbol = base[k]; symbol; symbol = (LiteralSymbol *) symbol -> next)
    {
        if (len == symbol -> NameLength() &&
            memcmp(symbol -> Name(), str, len * sizeof(wchar_t)) == 0)
        {
            return symbol;
        }
    }

    symbol = new LiteralSymbol();
    symbol_pool.Next() = symbol;
    symbol -> Initialize(str, hash_address, len);

    symbol -> next = base[k];
    base[k] = symbol;

    //
    // If the number of unique elements in the hash table exceeds 2 times
    // the size of the base, and we have not yet reached the maximum
    // allowable size for a base, reallocate a larger base and rehash
    // the elements.
    //
    if (symbol_pool.Length() > (hash_size << 1) && hash_size < MAX_HASH_SIZE)
        Rehash();

    return symbol;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

