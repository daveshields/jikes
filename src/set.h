// $Id: set.h,v 1.7 1999/09/01 15:04:29 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef set_INCLUDED
#define set_INCLUDED

#include "config.h"
#include "assert.h"
#include "symbol.h"

class ShadowSymbol
{
public:
    ShadowSymbol *next;
    Symbol *symbol;
    int pool_index;

    inline NameSymbol *Identity() { return symbol -> Identity(); }

    ShadowSymbol(Symbol *symbol_) : symbol(symbol_),
                                    conflict(NULL)
    {}

    ~ShadowSymbol() { delete conflict; }

    Symbol *Conflict(int i) { return (*conflict)[i]; }

    inline int NumConflicts()
    {
        return (conflict ? conflict -> Length() : 0);
    }

    inline void AddConflict(Symbol *conflict_symbol)
    {
        if ((symbol != conflict_symbol) && (! Find(conflict_symbol)))
            conflict -> Next() = conflict_symbol;
        return;
    }

    inline void RemoveConflict(int k)
    {
        int last_index = conflict -> Length() - 1;
        if (k < 0) // when k is negative, it identifies the main symbol
             symbol = (*conflict)[last_index];
        else (*conflict)[k] = (*conflict)[last_index];
        conflict -> Reset(last_index);
    }

private:
    Tuple<Symbol *> *conflict;

    bool Find(Symbol *conflict_symbol)
    {
        if (! conflict)
            conflict = new Tuple<Symbol *>(4);
        for (int k = 0; k < conflict -> Length(); k++)
            if ((*conflict)[k] == conflict_symbol)
                return true;
        return false;
    }
};


class SymbolSet
{
public:
    enum
    {
        DEFAULT_HASH_SIZE = 13,
        MAX_HASH_SIZE = 1021
    };

    SymbolSet(int hash_size_ = DEFAULT_HASH_SIZE) : symbol_pool(256),
                                                    main_index(0),
                                                    sub_index(0)
    {
        hash_size = (hash_size_ <= 0 ? 1 : hash_size_);

        prime_index = -1;
        do
        {
            if (hash_size < primes[prime_index + 1])
                break;
            prime_index++;
        } while (primes[prime_index] < MAX_HASH_SIZE);

        base = (ShadowSymbol **) memset(new ShadowSymbol *[hash_size], 0, hash_size * sizeof(ShadowSymbol *));
    }

    ~SymbolSet();

    //
    // Calculate the size of the set an return the value.
    //
    inline int Size()
    {
        int size = 0;

        for (int i = 0; i < symbol_pool.Length(); i++)
        {
            ShadowSymbol *shadow = symbol_pool[i];
            Symbol *symbol = shadow -> symbol;
            for (int k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
                size++;
        }

        return size;
    }

    //
    // Empty out the set in question - i.e., remove all its elements
    //
    inline void SetEmpty()
    {
        for (int i = 0; i < symbol_pool.Length(); i++)
            delete symbol_pool[i];
        symbol_pool.Reset();
        base = (ShadowSymbol **) memset(base, 0, hash_size * sizeof(ShadowSymbol *));
    }

    //
    // Empty out the set in question - i.e., remove all its elements
    //
    bool IsEmpty() { return symbol_pool.Length() == 0; }

    //
    // Assignment of a set to another.
    //
    SymbolSet &operator=(SymbolSet &rhs)
    {
        if (this != &rhs)
        {
            this -> SetEmpty();
            this -> Union(rhs);
        }

        return *this;
    }

    //
    // Equality comparison of two sets
    //
    bool operator==(SymbolSet &);

    //
    // NonEquality comparison of two sets
    //
    inline bool operator!=(SymbolSet &rhs)
    {
        return ! (*this == rhs);
    }

    //
    // Union the set in question with the set passed as argument: "set"
    //
    void Union(SymbolSet &);

    //
    // Intersect the set in question with the set passed as argument: "set"
    //
    void Intersection(SymbolSet &);

    //
    // Return a bolean value indicating whether or not the set in question intersects the set passed as argument: "set"
    // i.e., is there at least one element of set that is also an element of "this" set.
    //
    bool Intersects(SymbolSet &);

    //
    // How many elements with this name symbol do we have?
    //
    inline int NameCount(Symbol *element)
    {
        NameSymbol *name_symbol = element -> Identity();
        for (ShadowSymbol *shadow = base[name_symbol -> index % hash_size]; shadow; shadow = shadow -> next)
        {
            if (shadow -> Identity() == name_symbol)
                return shadow -> NumConflicts() + 1;
        }

        return 0;
    }

    //
    // Is "element" an element of the set in question ?
    //
    inline bool IsElement(Symbol *element)
    {
        assert(element);

        NameSymbol *name_symbol = element -> Identity();
        for (ShadowSymbol *shadow = base[name_symbol -> index % hash_size]; shadow; shadow = shadow -> next)
        {
            if (shadow -> Identity() == name_symbol)
            {
                Symbol *symbol = shadow -> symbol;
                for (int k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
                {
                    if (symbol == element)
                        return true;
                }

                return false;
            }
        }

        return false;
    }

    //
    // Add element to the set in question if was not already there.
    //
    inline void AddElement(Symbol *element)
    {
        NameSymbol *name_symbol = element -> Identity();
        int i = name_symbol -> index % hash_size;

        ShadowSymbol *shadow;
        for (shadow = base[i]; shadow; shadow = shadow -> next)
        {
            if (shadow -> Identity() == name_symbol)
            {
                shadow -> AddConflict(element);
                return;
            }
        }

        shadow = new ShadowSymbol(element);
        shadow -> pool_index = symbol_pool.Length();
        symbol_pool.Next() = shadow;

        shadow -> next = base[i];
        base[i] = shadow;

        //
        // If the set is "adjustable" and the number of unique elements in it exceeds
        // 2 times the size of the base, and we have not yet reached the maximum
        // allowable size for a base, reallocate a larger base and rehash the elements.
        //
        if ((symbol_pool.Length() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
            Rehash();

        return;
    }


    void RemoveElement(Symbol *);

    Symbol* FirstElement()
    {
        main_index = 0;
        sub_index = 0;
        return (main_index < symbol_pool.Length() ? symbol_pool[main_index] -> symbol : (Symbol *) NULL);
    }

    Symbol* NextElement()
    {
        Symbol *symbol = NULL;

        if (main_index < symbol_pool.Length())
        {
             if (sub_index < symbol_pool[main_index] -> NumConflicts())
                  symbol = symbol_pool[main_index] -> Conflict(sub_index++);
             else
             {
                 main_index++;
                 sub_index = 0;
                 symbol = (main_index < symbol_pool.Length() ? symbol_pool[main_index] -> symbol : (Symbol *) NULL);
             }
        }

        return symbol;
    }

protected:

    Tuple<ShadowSymbol *> symbol_pool;

    int main_index,
        sub_index;

    ShadowSymbol **base;
    int hash_size;

    static int primes[];
    int prime_index;

    void Rehash();
};


//
// Single-value Mapping from a name_symbol into a symbol with that name.
//
class NameSymbolMap : public SymbolSet
{
public:
    NameSymbolMap(int hash_size_ = DEFAULT_HASH_SIZE) : SymbolSet(hash_size_)
    {}

    //
    // Is there an element with this name in the map ?
    //
    inline Symbol *Image(NameSymbol *name_symbol)
    {
        assert(name_symbol);

        for (ShadowSymbol *shadow = base[name_symbol -> index % hash_size]; shadow; shadow = shadow -> next)
        {
            if (shadow -> Identity() == name_symbol)
                return shadow -> symbol;
        }

        return NULL;
    }

    //
    // Add element to the set in question if was not already there.
    //
    inline void AddElement(Symbol *element)
    {
        assert(element);

        ShadowSymbol *shadow = NULL;
        for (shadow = base[element -> Identity() -> index % hash_size]; shadow; shadow = shadow -> next)
        {
            if (shadow -> Identity() == element -> Identity())
                break;
        }

        //
        // If an element was already mapped into that name, replace it.
        // Otherwise, add the new element.
        //
        if (shadow)
             shadow -> symbol = element;
        else SymbolSet::AddElement(element);

        return;
    }
};


//
// Single-value Mapping from an arbitrary symbol into another arbitrary symbol.
//
class SymbolMap
{
public:
    enum
    {
        DEFAULT_HASH_SIZE = 13,
        MAX_HASH_SIZE = 1021
    };

    SymbolMap(int hash_size_ = DEFAULT_HASH_SIZE);
    ~SymbolMap();

    //
    // Has symbol been mapped to an image, yet? If so, return the image.
    //
    inline Symbol *Image(Symbol *symbol)
    {
        assert(symbol);

        int k = symbol -> Identity() -> index % hash_size;
        for (Element *element = base[k]; element; element = element -> next)
        {
            if (element -> domain_element == symbol)
                return element -> image;
        }

        return NULL;
    }

    //
    // Map or remap symbol to a given image.
    //
    void Map(Symbol *, Symbol *);

private:

    class Element
    {
    public:
        Element *next;
        Symbol  *domain_element,
                *image;
    };

    Tuple<Element *> symbol_pool;

    Element **base;
    int hash_size;

    static int primes[];
    int prime_index;

    void Rehash();
};


//
// This Bitset template class can be used to construct sets of
// integers. The template takes as argument the address of an integer
// variable, set_size, which indicates that the universe of the sets
// is: {0..*set_size}.
//
class BitSet
{
    typedef unsigned CELL;

    CELL *s;
    const int set_size;

public:

    enum { EMPTY, UNIVERSE, cell_size = sizeof(CELL) * CHAR_BIT };

    //
    // Produce the empty set.
    //
    void SetEmpty()
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] = 0;
    }

    //
    // Produce the universe set.
    //
    void SetUniverse()
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] = ~((CELL) 0);
    }

    //
    // This function takes as argument the size of a hash table, table_size.
    // It hashes a bitset into a location within the range <1..table_size-1>.
    //
    int Hash(int table_size)
    {
        unsigned long hash_address = 0;

        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            hash_address += s[i];

        return hash_address % table_size;
    }

    //
    // Assignment of a bitset to another.
    //
    BitSet& operator=(const BitSet& rhs)
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] = rhs.s[i];

        return *this;
    }

    //
    // Constructor of an uninitialized bitset.
    //
    BitSet(int set_size_) : set_size(set_size_)
    {
        //
        // Note that we comment out the -1 because some C++ compilers
        // do not know how to allocate an array of size 0. Note that
        // we assert that set_size >= 0.
        //
        s = new CELL[(set_size + cell_size /* - 1 */) / cell_size];
    }

    //
    // Constructor of an initialized bitset.
    //
    BitSet(int set_size_, int init) : set_size(set_size_)
    {
        //
        // Note that we comment out the -1 because some C++ compilers
        // do not know how to allocate an array of size 0. Note that
        // we assert that set_size >= 0.
        //
        s = new CELL[(set_size + cell_size /* - 1 */) / cell_size];
        if (init == UNIVERSE)
             SetUniverse();
        else SetEmpty();
    }

    //
    // Constructor to clone a bitset.
    //
    BitSet(const BitSet& rhs) : set_size(rhs.set_size)
    {
        //
        // Note that we comment out the -1 because some C++ compilers
        // do not know how to allocate an array of size 0. Note that
        // we assert that set_size >= 0.
        //
        s = new CELL[(set_size + cell_size /* - 1 */) / cell_size];
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] = rhs.s[i];
    }

    //
    // Destructor of a bitset.
    //
    ~BitSet() { delete [] s; }

    //
    // Return size of a bit set.
    //
    int Size() { return set_size; }

    //
    // Return a boolean value indicating whether or not the element i
    // is in the bitset in question.
    //
    int operator[](const int i)
    {
        assert(i >= 0 && i < set_size);

        //
        // Note that no check is made here to ensure that 0 <= i < set_size.
        // Such a check might be useful for debugging and a range exception
        // should be thrown if it yields TRUE.
        //
        return s[i / cell_size] &
               ((i + cell_size) % cell_size
                         ? (CELL) 1 << ((i + cell_size) % cell_size)
                         : (CELL) 1);
    }

    //
    // Insert an element i in the bitset in question.
    //
    void AddElement(int i)
    {
        assert(i >= 0 && i < set_size);

        s[i / cell_size] |= ((i + cell_size) % cell_size
                                             ? (CELL) 1 << ((i + cell_size) % cell_size)
                                             : (CELL) 1);
    }

    //
    // Remove an element i from the bitset in question.
    //
    void RemoveElement(int i)
    {
        assert(i >= 0 && i < set_size);

        s[i / cell_size] &= ~((i + cell_size) % cell_size
                                              ? (CELL) 1 << ((i + cell_size) % cell_size)
                                              : (CELL) 1);
    }

    //
    // Yield a boolean result indicating whether or not two sets are
    // identical.
    //
    int operator==(const BitSet& rhs)
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
        {
            if (s[i] != rhs.s[i])
                return 0;
        }

        return 1;
    }

    //
    // Yield a boolean result indicating whether or not two sets are
    // identical.
    //
    int operator!=(const BitSet& rhs)
    {
        return ! (*this == rhs);
    }

    //
    // Union of two bitsets.
    //
    BitSet operator+(const BitSet& rhs)
    {
        BitSet result(set_size);

        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            result.s[i] = s[i] | rhs.s[i];

        return result;
    }

    //
    // Union of an lvalue bitset and a rhs bitset.
    //
    BitSet& operator+=(const BitSet& rhs)
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] |= rhs.s[i];

        return *this;
    }

    //
    // Intersection of two bitsets.
    //
    BitSet operator*(const BitSet& rhs)
    {
        BitSet result(set_size);

        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            result.s[i] = s[i] & rhs.s[i];

        return result;
    }

    //
    // Intersection of an lvalue bitset and a rhs bitset.
    //
    BitSet& operator*=(const BitSet& rhs)
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] &= rhs.s[i];

        return *this;
    }

    //
    // Difference of two bitsets.
    //
    BitSet operator-(const BitSet& rhs)
    {
        BitSet result(set_size);

        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            result.s[i] = s[i] & (~ rhs.s[i]);

        return result;
    }

    //
    // Difference of an lvalue bitset and a rhs bitset.
    //
    BitSet& operator-=(const BitSet& rhs)
    {
        for (int i = (set_size - 1) / cell_size; i >= 0; i--)
            s[i] &= (~ rhs.s[i]);

        return *this;
    }
};

class DefiniteAssignmentSet
{
public:
    int set_size;

    BitSet true_set,
           false_set;

    DefiniteAssignmentSet(int set_size_) : set_size(set_size_),
                                           true_set(set_size),
                                           false_set(set_size)
    {}
};
#endif

