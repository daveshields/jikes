// $Id: tuple.h,v 1.6 1999/08/26 15:34:10 shields Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1998, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//
#ifndef TUPLE_INCLUDED
#define TUPLE_INCLUDED

#ifdef WIN32_FILE_SYSTEM
#include <windows.h>
#endif

#include "config.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

class OutputBuffer;

//
// This Tuple template class can be used to construct a dynamic
// array of arbitrary objects. The space for the array is allocated in
// blocks of size 2**LOG_BLKSIZE. In declaring a tuple the user
// may specify an estimate of how many elements he expects.
// Based on that estimate, suitable value will be calsulated log_blksize
// and base_increment. If these estimates are off, more space will be
// allocated.
//
template <class T>
class Tuple
{
protected:

    friend class OutputBuffer;

    enum { DEFAULT_LOG_BLKSIZE = 3, DEFAULT_BASE_INCREMENT = 4 };

    T **base;
    int base_size,
        top,
        size;

    int log_blksize,
        base_increment;

    inline int Blksize() { return (1 << log_blksize); }

    //
    // Allocate another block of storage for the dynamic array.
    //
    inline void AllocateMoreSpace()
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
        int k = size >> log_blksize; /* which segment? */

        //
        // If the base is overflowed, reallocate it and initialize the new elements to NULL.
        //
        if (k == base_size)
        {
            int old_base_size = base_size;
            T **old_base = base;

            base_size += base_increment;
            base = new T*[base_size];

            if (old_base != NULL)
            {
                memmove(base, old_base, old_base_size * sizeof(T *));
                delete [] old_base;
            }
            memset(&base[old_base_size], 0, (base_size - old_base_size) * sizeof(T *));
        }

        //
        // We allocate a new segment and place its adjusted address in
        // base[k]. The adjustment allows us to index the segment directly,
        // instead of having to perform a subtraction for each reference.
        // See operator[] below.
        //
        base[k] = new T[Blksize()];
        base[k] -= size;

        //
        // Finally, we update SIZE.
        //
        size += Blksize();

        return;
    }

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
    inline void Resize(const int n = 0)
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
                AllocateMoreSpace();
            } while (n > size);
        }
        else if (n < size)
        {
            // slot is the index of the base element whose block
            // will contain the (n-1)th element.
            int slot = (n <= 0 ? -1 : (n - 1) >> log_blksize);

            for (int k = (size >> log_blksize) - 1; k > slot; k--)
            {
                size -= Blksize();
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

    //
    // This function is used to reset the size of a dynamic array without
    // allocating or deallocting space. It may be invoked with an integer
    // argument n which indicates the new size or with no argument which
    // indicates that the size should be reset to 0.
    //
    inline void Reset(const int n = 0)
    {
        assert(n >= 0 && n <= size);

        top = n;
    }

    //
    // Return length of the dynamic array.
    //
    inline int Length() { return top; }

    //
    // Return a reference to the ith element of the dynamic array.
    //
    // Note that no check is made here to ensure that 0 <= i < top.
    // Such a check might be useful for debugging and a range exception
    // should be thrown if it yields true.
    //
    inline T& operator[](const int i)
    {
        assert(i >= 0 && i < top);

        return base[i >> log_blksize][i];
    }

    //
    // Add an element to the dynamic array and return the top index.
    //
    inline int NextIndex()
    {
        int i = top++;
        if (i == size)
            AllocateMoreSpace();
        return i;
    }

    //
    // Add an element to the dynamic array and return a reference to
    // that new element.
    //
    inline T& Next() { int i = NextIndex(); return base[i >> log_blksize][i]; }

    //
    // Assignment of a dynamic array to another.
    //
    inline Tuple<T>& operator=(const Tuple<T>& rhs)
    {
        if (this != &rhs)
        {
            Resize(rhs.top);
            for (int i = 0; i < rhs.top; i++)
                base[i >> log_blksize][i] = rhs.base[i >> log_blksize][i];
        }

        return *this;
    }

    //
    // Constructor of a Tuple
    //
    Tuple(unsigned estimate = 0)
    {
        if (estimate == 0)
        {
            log_blksize = DEFAULT_LOG_BLKSIZE;
            base_increment = DEFAULT_BASE_INCREMENT;
        }
        else
        {
            for (log_blksize = 1; (((unsigned) 1 << log_blksize) < estimate) && (log_blksize < 31); log_blksize++)
                ;
            if (log_blksize <= DEFAULT_LOG_BLKSIZE)
                base_increment = 1;
            else if (log_blksize < 13)
            {
                base_increment = (unsigned) 1 << (log_blksize - 4);
                log_blksize = 4;
            }
            else
            {
                base_increment = (unsigned) 1 << (log_blksize - 8);
                log_blksize = 8;
            }
            base_increment++; // add a little margin to avoid reallocating the base.
        }

        base_size = 0;
        size = 0;
        top = 0;
        base = NULL;
    }

    //
    // Constructor of a Tuple
    //
    Tuple(int log_blksize_, int base_increment_) : log_blksize(log_blksize_),
                                                   base_increment(base_increment_)
    {
        base_size = 0;
        size = 0;
        top = 0;
        base = NULL;
    }

    //
    // Initialization of a dynamic array.
    //
    Tuple(const Tuple<T>& rhs) : log_blksize(rhs.log_blksize),
                                 base_increment(rhs.base_increment)
    {
        base_size = 0;
        size = 0;
        base = NULL;
        *this = rhs;
    }

    //
    // Destructor of a dynamic array.
    //
    ~Tuple() { Resize(0); }

    // ********************************************************************************************** //

    //
    // Return the total size of temporary space allocated.
    //
    inline size_t SpaceAllocated(void)
    {
        return ((base_size * sizeof(T **)) + (size * sizeof(T)));
    }

    //
    // Return the total size of temporary space used.
    //
    inline size_t SpaceUsed(void)
    {
        return (((size >> log_blksize) * sizeof(T **)) + (top * sizeof(T)));
    }
};


//
//
//
template <class T>
class ConvertibleArray : public Tuple<T>
{
public:

    ConvertibleArray(int estimate = 0) : Tuple<T>(estimate),
                                         array(NULL)
    {}

    ConvertibleArray(int log_blksize, int base_increment) : Tuple<T>(log_blksize, base_increment),
                                                            array(NULL)
    {}

    ~ConvertibleArray() { delete [] array; }

    //
    // This function converts a tuple into a regular array and destroys the
    // original tuple.
    //
    inline T *&Array()
    {
        if ((! array) && Tuple<T>::top > 0)
        {
            array = new T[Tuple<T>::top];

            int i = 0,
                processed_size = 0,
                n = (Tuple<T>::top - 1) >> Tuple<T>::log_blksize; // the last non-empty slot!
            while (i < n)
            {
                memmove(&array[processed_size], Tuple<T>::base[i] + processed_size, Tuple<T>::Blksize() * sizeof(T));
                delete [] (Tuple<T>::base[i] + processed_size);
                i++;
                processed_size += Tuple<T>::Blksize();
            }
            memmove(&array[processed_size], Tuple<T>::base[n] + processed_size, (Tuple<T>::top - processed_size) * sizeof(T));
            delete [] (Tuple<T>::base[n] + processed_size);
            delete [] Tuple<T>::base;
            Tuple<T>::base = NULL;
            Tuple<T>::size = 0;
        }

        return array;
    }

    inline T& operator[](const int i)
    {
        assert(i >= 0 && i < Tuple<T>::top);

        return (array ? array[i] : Tuple<T>::base[i >> Tuple<T>::log_blksize][i]);
    }

    inline void Resize(const int n = 0) { assert(false); }
    inline void Reset(const int n = 0) { assert(false); }
    inline T& Next()
    {
        assert(! array);

        int i = Tuple<T>::NextIndex();
        return Tuple<T>::base[i >> Tuple<T>::log_blksize][i];
    }
    inline Tuple<T>& operator=(const Tuple<T>& rhs)
    {
        assert(false);
        return *this;
    }

private:

    T *array;
};


//
//
//
class OutputBuffer
{
public:

    OutputBuffer(int log_blksize = 13, int base_increment = 128) : buffer(log_blksize, base_increment) {}

    inline void PutB1(u1 u) { buffer.Next() = u; }

    inline void PutB2(u2 u)
    {
        buffer.Next() = u >> 8;
        buffer.Next() = u & 0xff;
    }

    inline void PutB4(u4 u)
    {
        buffer.Next() = u >> 24;
        buffer.Next() = (u >> 16) & 0xff;
        buffer.Next() = (u >> 8)  & 0xff;
        buffer.Next() = u & 0xff;
    }

    inline void put_n(u1 *u, int n)
    {
        for (int i = 0; i < n; i++)
            buffer.Next() = u[i];
    }

    inline bool WriteToFile(char *file_name)
    {
#ifdef UNIX_FILE_SYSTEM
        FILE *file = ::SystemFopen(file_name, "wb");
        if (file  ==  (FILE *) NULL)
            return false;

        int i = 0,
            size = 0,
            n = (buffer.top - 1) >> buffer.log_blksize; // the last non-empty slot!
        while (i < n)
        {
            fwrite(buffer.base[i] + size, sizeof(u1), buffer.Blksize(), file);
            i++;
            size += buffer.Blksize();
        }
        fwrite(buffer.base[n] + size, sizeof(u1), (buffer.top - size), file);

        fclose(file);
#elif defined(WIN32_FILE_SYSTEM)
        HANDLE file = CreateFile(file_name, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        HANDLE mapfile = (file == INVALID_HANDLE_VALUE
                                     ? file
                                     : CreateFileMapping(file, NULL, PAGE_READWRITE, 0, buffer.top, NULL));
        if (mapfile == INVALID_HANDLE_VALUE)
            return false;
        u1 *string_buffer = (u1 *) MapViewOfFile(mapfile, FILE_MAP_WRITE, 0, 0, buffer.top);

        assert(string_buffer);

        int i = 0,
            size = 0,
            n = (buffer.top - 1) >> buffer.log_blksize; // the last non-empty slot!
        while (i < n)
        {
            memmove(&string_buffer[size], buffer.base[i] + size, buffer.Blksize() * sizeof(u1));
            i++;
            size += buffer.Blksize();
        }
        memmove(&string_buffer[size], buffer.base[n] + size, (buffer.top - size) * sizeof(u1));

        UnmapViewOfFile(string_buffer);
        CloseHandle(mapfile);
        CloseHandle(file);
#endif

        return true;
    }

private:

    Tuple<u1> buffer;
};

#endif /* #ifndef TUPLE_INCLUDED */
