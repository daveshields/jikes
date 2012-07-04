// $Id: set.cpp,v 1.15 2001/09/14 05:31:34 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1998, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#include "set.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

int SymbolSet::primes[] = {DEFAULT_HASH_SIZE, 101, 401, MAX_HASH_SIZE};

void SymbolSet::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (ShadowSymbol **) memset(new ShadowSymbol *[hash_size], 0, hash_size * sizeof(ShadowSymbol *));

    for (int k = 0; k < symbol_pool.Length(); k++)
    {
        ShadowSymbol *shadow = symbol_pool[k];
        int i = shadow -> Identity() -> index % hash_size;
        shadow -> next = base[i];
        base[i] = shadow;
    }

    return;
}


SymbolSet::~SymbolSet()
{
    SetEmpty();
    delete [] base;
}


bool SymbolSet::operator==(SymbolSet& rhs)
{
    if (this != &rhs)
    {
        if (symbol_pool.Length() != rhs.symbol_pool.Length())
            return false;

        for (int i = 0; i < symbol_pool.Length(); i++)
        {
            ShadowSymbol *shadow = symbol_pool[i];
            Symbol *symbol = shadow -> symbol;
            for (int k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
            {
                if (! rhs.IsElement(symbol))
                    return false;
            }
        }
    }

    return true;
}


//
// Union the set in question with the set passed as argument: "set"
//
void SymbolSet::Union(SymbolSet &set)
{
    if (this != &set)
    {
        for (int i = 0; i < set.symbol_pool.Length(); i++)
        {
            ShadowSymbol *shadow = set.symbol_pool[i];
            Symbol *symbol = shadow -> symbol;
            for (int k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
                AddElement(symbol);
        }
    }

    return;
}


//
// Intersect the set in question with the set passed as argument: "set"
//
void SymbolSet::Intersection(SymbolSet &set)
{
    if (this != &set)
    {
        Tuple<Symbol *> old_symbol_pool(this -> symbol_pool.Length());
        for (int i = 0; i < this -> symbol_pool.Length(); i++)
        {
            ShadowSymbol *shadow = this -> symbol_pool[i];
            Symbol *symbol = shadow -> symbol;
            for (int k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
                old_symbol_pool.Next() = symbol;
        }

        this -> SetEmpty();

        for (int j = 0; j < old_symbol_pool.Length(); j++)
        {
            if (set.IsElement(old_symbol_pool[j]))
                AddElement(old_symbol_pool[j]);
        }
    }

    return;
}


//
// Return a bolean value indicating whether or not the set in question intersects the set passed as argument: "set"
// i.e., is there at least one element of set that is also an element of "this" set.
//
bool SymbolSet::Intersects(SymbolSet &set)
{
    for (int i = 0; i < set.symbol_pool.Length(); i++)
    {
        ShadowSymbol *shadow = set.symbol_pool[i];
        Symbol *symbol = shadow -> symbol;
        for (int k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
            if (IsElement(symbol))
                return true;
    }

    return false;
}


//
// Remove element from the set
//
void SymbolSet::RemoveElement(Symbol *element)
{
    NameSymbol *name_symbol = element -> Identity();
    int i = name_symbol -> index % hash_size;
    ShadowSymbol *previous = NULL,
                 *shadow;
    for (shadow = base[i]; shadow; previous = shadow, shadow = shadow -> next)
    {
        if (shadow -> Identity() == name_symbol)
        {
            Symbol *symbol = shadow -> symbol;
            int k;
            for (k = 0; symbol; symbol = (Symbol *) (k < shadow -> NumConflicts() ? shadow -> Conflict(k++) : NULL))
            {
                if (symbol == element)
                    break;
            }

            if (symbol)
            {
                if (shadow -> NumConflicts() == 0)
                    break;
                shadow -> RemoveConflict(k - 1);
            }

            return;
        }
    }

    if (shadow) // element is the only object contained in shadow
    {
        if (previous == NULL)
             base[i] = shadow -> next;
        else previous -> next = shadow -> next;

        int last_index = symbol_pool.Length() - 1;
        if (shadow -> pool_index != last_index)
        {// move last element to position previously occupied by element being deleted
            symbol_pool[last_index] -> pool_index = shadow -> pool_index;
            symbol_pool[shadow -> pool_index] = symbol_pool[last_index];
        }

        symbol_pool.Reset(last_index); // remove last slot in symbol_pool

        delete shadow;
    }

    return;
}


int SymbolMap::primes[] = {DEFAULT_HASH_SIZE, 101, 401, MAX_HASH_SIZE};

void SymbolMap::Rehash()
{
    hash_size = primes[++prime_index];

    delete [] base;
    base = (Element **) memset(new Element *[hash_size], 0, hash_size * sizeof(Element *));

    for (int i = 0; i < symbol_pool.Length(); i++)
    {
        Element *element = symbol_pool[i];
        int k = element -> domain_element -> Identity() -> index % hash_size;
        element -> next = base[k];
        base[k] = element;
    }

    return;
}


void SymbolMap::Map(Symbol *symbol, Symbol *image)
{
    assert(symbol);

    Element *element;
    int k = symbol -> Identity() -> index % hash_size;
    for (element = base[k]; element; element = element -> next)
    {
        if (element -> domain_element == symbol)
            break;
    }

    //
    // If this is a new element, add it to the map.
    //
    if (! element)
    {
        element = new Element();
        element -> domain_element = symbol;
        element -> next = base[k];
        base[k] = element;

        symbol_pool.Next() = element;

        //
        // If the number of unique elements in the map exceeds 2 times
        // the size of the base, and we have not yet reached the maximum
        // allowable size for a base, reallocate a larger base and rehash
         // the elements.
        //
        if ((symbol_pool.Length() > (hash_size << 1)) && (hash_size < MAX_HASH_SIZE))
            Rehash();
    }
    else
    {
        fprintf(stderr, "WARNING: This should not have happened !!!");
    }

    element -> image = image;

    return;
}


SymbolMap::SymbolMap(int hash_size_)
{
    hash_size = (hash_size_ <= 0 ? 1 : hash_size_);

    prime_index = -1;
    do
    {
        if (hash_size < primes[prime_index + 1])
            break;
        prime_index++;
    } while (primes[prime_index] < MAX_HASH_SIZE);

    base = (Element **) memset(new Element *[hash_size], 0, hash_size * sizeof(Element *));
}


SymbolMap::~SymbolMap()
{
    for (int i = 0; i < symbol_pool.Length(); i++)
        delete symbol_pool[i];

    delete [] base;

    return;
}

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

