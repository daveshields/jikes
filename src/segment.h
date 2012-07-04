// $Id: segment.h,v 1.9 2002/08/05 23:56:28 ericb Exp $ -*- c++ -*-
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://ibm.com/developerworks/opensource/jikes.
// Copyright (C) 1996, 1999, 2000, 2001 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

#ifndef segment_INCLUDED
#define segment_INCLUDED

#include "platform.h"
#include "tuple.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

class SegmentPool;


class PairSegment
{
public:

    enum
    {
        LIST_LIMIT = 5,
        LOG_BLKSIZE = 8,
        BLKSIZE = 1 << LOG_BLKSIZE,
        MASK = ~ (BLKSIZE - 1)
    };

private:

    class TargetValuePair
    {
    public:
        int target;
        u2 value;
    };

    TargetValuePair list[LIST_LIMIT];
    int top;
    u2 *array;

public:

    PairSegment() : top(0),
                    array(NULL)
    {}

    ~PairSegment()
    {
        if (array)
        {
            unsigned offset = ((unsigned) list[0].target) & MASK;
            delete [] (array + offset);
        }
    }

    u2 &Image(int);
};


class Pair
{
public:

    Pair(SegmentPool &segment_pool_, int estimate = 0)
        : segment_pool(segment_pool_)
    {
        //
        // DO NOT PERFORM THESE INITIALIZATION IN THE METHOD DECLARATOR !!!
        // There appears to be a bug in the xlC compiler that causes base to
        // not be initialized properly !!!
        //
        base_size = (estimate > 0
                     ? (estimate >> PairSegment::LOG_BLKSIZE) + 1 : 0);
        base = (PairSegment **) (estimate > 0
                                 ? memset(new PairSegment*[base_size], 0,
                                          base_size * sizeof(PairSegment *))
                                 : NULL);
    }

    ~Pair()
    {
        delete [] base;
    }

    u2 &operator[](const int);

private:

    SegmentPool &segment_pool;

    PairSegment **base;
    int base_size;
};


class TripletSegment
{
public:

    enum
    {
        LIST_LIMIT = 5,
        LOG_BLKSIZE = 8,
        BLKSIZE = 1 << LOG_BLKSIZE,
        MASK = ~ (BLKSIZE - 1)
    };

private:

    class TargetValuePair
    {
    public:
        int target;
        Pair *value;
    };

    SegmentPool &segment_pool;

    TargetValuePair list[LIST_LIMIT];
    int top;
    Pair **array;

public:

    TripletSegment(SegmentPool &segment_pool_) : segment_pool(segment_pool_),
                                                 top(0),
                                                 array(NULL)
    {}

    ~TripletSegment()
    {
        if (array)
        {
            unsigned offset = ((unsigned) list[0].target) & MASK;
            delete [] (array + offset);
        }
    }

    Pair &Image(int);
};


class Triplet
{
public:

    Triplet(SegmentPool &segment_pool_, int estimate = 0)
        : segment_pool(segment_pool_)
    {
        //
        // DO NOT PERFORM THESE INITIALIZATION IN THE METHOD DECLARATOR !!!
        // There appears to be a bug in the xlC compiler that causes base to
        // not be initialized properly !!!
        //
        base_size = (estimate > 0
                     ? (estimate >> TripletSegment::LOG_BLKSIZE) + 1 : 0);
        base = (TripletSegment **)
            (estimate > 0
             ? memset(new TripletSegment*[base_size], 0,
                      base_size * sizeof(TripletSegment *))
             : NULL);
    }

    ~Triplet()
    {
        delete [] base;
    }

    u2 &Image(const int, const int);

private:

    SegmentPool &segment_pool;

    TripletSegment **base;
    int base_size;
};


class SegmentPool
{
    Tuple<TripletSegment *> triplet_segment_pool;
    Tuple<PairSegment *> pair_segment_pool;
    Tuple<Pair *> pair_pool;

public:
    SegmentPool();

    ~SegmentPool();

    Pair *AllocatePair(int estimate = 0)
    {
        return pair_pool.Next() = new Pair(*this, estimate);
    }
    PairSegment *AllocatePairSegment()
    {
        return pair_segment_pool.Next() = new PairSegment();
    }
    TripletSegment *AllocateTripletSegment()
    {
        return triplet_segment_pool.Next() = new TripletSegment(*this);
    }
};

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // segment_INCLUDED

