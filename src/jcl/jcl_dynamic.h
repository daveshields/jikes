// $Id: jcl_dynamic.h,v 1.2 1999/12/09 18:02:26 lord Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef jcl_DYNAMIC_INCLUDED
#define jcl_DYNAMIC_INCLUDED

#include <stdlib.h>
#include <string.h>

//
// This DynamicArray template class can be used to construct a dynamic
// array of arbitrary objects. The space for the array is allocated in
// blocks of size 2**LOG_BLKSIZE. In declaring a dynamic array the user
// may specify a value for LOG_BLKSIZE which by default is 10. Also,
// as the array is implemented using a base+offset strategy, the user
// may also specify the number of "slots" to add to the base when the
// current base runs out of space. Each slot points to a block.
//
template <class T, int LOG_BLKSIZE = 10, int BASE_INCREMENT = 16>
class DynamicArray
{
    T **base;
    int base_size,
        top,
        size;

    //
    // Reallocate the base and initialize the new elements to NULL.
    //
    void reallocate_base();

    //
    // Allocate another block of storage for the dynamic array.
    //
    void allocate_more_space();

public:

    //
    // This function is invoked with an integer argument n. It ensures
    // that enough space is allocated for n elements in the dynamic array.
    // I.e., that the array will be indexable in the range  (0..n-1)
    //
    // Note that this function can be used as a garbage collector.  When
    // invoked with no argument(or 0), it frees up all dynamic space that
    // was allocated for the array.
    //
    void resize(const int n = 0);

    //
    // This function is used to reset the size of a dynamic array without
    // allocating or deallocting space. It may be invoked with an integer 
    // argument n which indicates the new size or with no argument which
    // indicates that the size should be reset to 0.
    //
    void reset(const int n = 0)
    {
        if (n < 0 || n >= size)
            /* throw range exception */ ;
        top = n;
    }

    //
    // Return length of the dynamic array.
    //
    int length() { return top; }

    //
    // Return a reference to the ith element of the dynamic array.
    //
    // Note that no check is made here to ensure that 0 <= i < top.
    // Such a check might be useful for debugging and a range exception
    // should be thrown if it yields true.
    //
    T& operator[](const int i) { return base[i >> LOG_BLKSIZE][i]; }

    //
    // Add an element to the dynamic array and return the top index.
    //
    int next_index()
    {
        int i = top++;
        if (i == size)
            allocate_more_space();
        return i;
    }

    //
    // Add an element to the dynamic array and return a reference to
    // that new element. 
    //
    T& next() { int i = next_index(); return base[i >> LOG_BLKSIZE][i]; }

    //
    // Assignment of a dynamic array to another.
    //
    DynamicArray<T, LOG_BLKSIZE, BASE_INCREMENT>&
        operator=(const DynamicArray<T, LOG_BLKSIZE, BASE_INCREMENT>& rhs)
    {
        if (this != &rhs)
        {
            resize(rhs.top);
            for (int i = 0; i < rhs.top; i++)
                base[i >> LOG_BLKSIZE][i] = rhs.base[i >> LOG_BLKSIZE][i];
        }

        return *this;
    }

    //
    // Constructor of a dynamic array.
    //
    DynamicArray(const int n = 0)
    {
        base_size = 0;
        size = 0;
        base = NULL;
        resize(n);
    }

    //
    // Initialization of a dynamic array.
    //
    DynamicArray(const DynamicArray<T, LOG_BLKSIZE, BASE_INCREMENT>& rhs)
    {
        base_size = 0;
        size = 0;
        base = NULL;
        *this = rhs;
    }

    //
    // Destructor of a dynamic array.
    //
    ~DynamicArray() { resize(0); }
};

//
// Reallocate the base and initialize the new elements to null.
//
template <class T, int LOG_BLKSIZE, int BASE_INCREMENT>
    void DynamicArray<T, LOG_BLKSIZE, BASE_INCREMENT>::reallocate_base()
    {
        int old_base_size = base_size;
        T **old_base = base;
    
        base_size += BASE_INCREMENT;
        base = new T*[base_size];
    
        if (old_base != NULL)
            memmove(base, old_base, old_base_size * sizeof(T *));
        memset(&base[old_base_size], 0, (base_size - old_base_size) * sizeof(T *));
    }
    
//
// Allocate another block of storage for the dynamic array.
//
template <class T, int LOG_BLKSIZE, int BASE_INCREMENT>
    void DynamicArray<T, LOG_BLKSIZE, BASE_INCREMENT>::allocate_more_space()
    {
        //
        // The variable size always indicates the maximum number of
        // elements that has been allocated for the array.
        // Initially, it is set to 0 to indicate that the array is empty.
        // The pool of available elements is divided into segments of size
        // 2**log_blksize each. Each segment is pointed to by a slot in
        // the array base.
        //
        // By dividing size by the size of the segment we obtain the
        // index for the next segment in base. If base is full, it is
        // reallocated.
        //
        //
        int blksize = 1 << LOG_BLKSIZE;
        int k = size >> LOG_BLKSIZE; /* which segment? */
    
        if (k == base_size)      // base overflowed? reallocate
            reallocate_base();
    
        //
        // We allocate a new segment and place its adjusted address in 
        // base[k]. The adjustment allows us to index the segment directly,
        // instead of having to perform a subtraction for each reference.
        // See operator[] below.
        //
        base[k] = new T[blksize];
        base[k] -= size;
    
        //
        // Finally, we update SIZE.
        //
        size += blksize;
    
        return;
    }

//
// This function is invoked with an integer argument n. It ensures
// that enough space is allocated for n elements in the dynamic array.
// I.e., that the array will be indexable in the range  (0..n-1)
//
// Note that this function can be used as a garbage collector.  When
// invoked with no argument(or 0), it frees up all dynamic space that
// was allocated for the array.
//
template <class T, int LOG_BLKSIZE, int BASE_INCREMENT>
    void DynamicArray<T, LOG_BLKSIZE, BASE_INCREMENT>::resize(const int n)
    {
        //
        // If array did not previously contain enough space, allocate
        // the necessary additional space. Otherwise, if the array had
        // more blocks than are needed, release the extra blocks.
        //
        if (n > size)
        {
            do
            {
                allocate_more_space();
            } while (n > size);
        }
        else if (n < size)
        {
            // slot is the index of the base element whose block
            // will contain the (n-1)th element.
            int blksize = 1 << LOG_BLKSIZE;
            int slot = (n <= 0 ? -1 : (n - 1) >> LOG_BLKSIZE);
    
            for (int k = (size >> LOG_BLKSIZE) - 1; k > slot; k--)
            {
                size -= blksize;
                base[k] += size;
                delete [] base[k];
                base[k] = NULL;
            }
    
            if (slot < 0)
            {
                delete [] base;
                base = NULL;
                base_size = 0;
            }
        }
    
        top  = n;
    }
    
#endif /* #ifndef DYNAMIC_INCLUDED */
