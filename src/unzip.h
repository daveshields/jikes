// $Id: unzip.h,v 1.9 2002/07/30 16:30:03 ericb Exp $

#ifndef unzip_INCLUDED
#define unzip_INCLUDED

#include "platform.h"

#ifdef HAVE_JIKES_NAMESPACE
namespace Jikes { // Open namespace Jikes block
#endif

//
// NOTE: Jikes incorporates compression code from the Info-ZIP
// group. There are no extra charges or costs due to the use of
// this code, and the original compression sources are freely
// available from http://www.cdrom/com/pub/infozip/ or
// ftp://ftp.cdrom.com/pub/infozip/ on the Internet.
// The sole use by Jikes of this compression code is contained in the
// files unzip.h and unzip.cpp, which are based on Info-ZIP's inflate.c and
// associated header files.
//

//
// You can do whatever you like with this source file, though I would
// prefer that if you modify it and redistribute it that you include
// comments to that effect with your name and the date.  Thank you.
// The abbreviated History list below includes the work of the
// following:
// M. Adler, G. Roelofs, J-l. Failly, J. Bush, C. Ghisler, A. Verheijen,
// P. Kienitz, C. Spieler, S. Maxwell, J. Altman
// Only the first and last entries from the original inflate.c are
// reproduced here.
//

//
// History:
// vers    date          who           what
// ----  ---------  --------------  ------------------------------------
//  a    ~~ Feb 92  M. Adler        used full (large, one-step) lookup table
//  ...
//  c16  20 Apr 97  J. Altman       added memzero(v[]) in huft_build()
//

#define DFUNZIP /* needed for unzip compilation*/

//
// inflate.c -- put in the public domain by Mark Adler
// version c15c, 28 March 1997
//

//
// Inflate deflated (PKZIP's method 8 compressed) data.  The compression
// method searches for as much of the current string of bytes (up to a
// length of 258) in the previous 32K bytes.  If it doesn't find any
// matches (of at least length 3), it codes the next byte.  Otherwise, it
// codes the length of the matched string and its distance backwards from
// the current position.  There is a single Huffman code that codes both
// single bytes (called "literals") and match lengths.  A second Huffman
// code codes the distance information, which follows a length code.  Each
// length or distance code actually represents a base value and a number
// of "extra" (sometimes zero) bits to get to add to the base value.  At
// the end of each deflated block is a special end-of-block (EOB) literal/
// length code.  The decoding process is basically: get a literal/length
// code; if EOB then done; if a literal, emit the decoded byte; if a
// length then get the distance and emit the referred-to bytes from the
// sliding window of previously emitted data.

// There are (currently) three kinds of inflate blocks: stored, fixed, and
// dynamic.  The compressor outputs a chunk of data at a time and decides
// which method to use on a chunk-by-chunk basis.  A chunk might typically
// be 32K to 64K, uncompressed.  If the chunk is uncompressible, then the
// "stored" method is used.  In this case, the bytes are simply stored as
// is, eight bits per byte, with none of the above coding.  The bytes are
// preceded by a count, since there is no longer an EOB code.

// If the data are compressible, then either the fixed or dynamic methods
// are used.  In the dynamic method, the compressed data are preceded by
// an encoding of the literal/length and distance Huffman codes that are
// to be used to decode this block.  The representation is itself Huffman
// coded, and so is preceded by a description of that code.  These code
// descriptions take up a little space, and so for small blocks, there is
// a predefined set of codes, called the fixed codes.  The fixed method is
// used if the block ends up smaller that way (usually for quite small
// chunks); otherwise the dynamic method is used.  In the latter case, the
// codes are customized to the probabilities in the current block and so
// can code it much better than the pre-determined fixed codes can.

// The Huffman codes themselves are decoded using a multi-level table
// lookup, in order to maximize the speed of decoding plus the speed of
// building the decoding tables.  See the comments below that precede the
// lbits and dbits tuning parameters.

// GRR:  return values(?)
//         0  OK
//         1  incomplete table
//         2  bad input
//         3  not enough memory
//

//
// If BMAX needs to be larger than 16, then h and x[] should be unsigned long.
//
#define BMAX 16         /* maximum bit length of any code (16 for explode) */
#define N_MAX 288       /* maximum number of codes in any set */


//
// Notes beyond the 1.93a appnote.txt:

// 1. Distance pointers never point before the beginning of the output
//    stream.
// 2. Distance pointers can point back across blocks, up to 32k away.
// 3. There is an implied maximum of 7 bits for the bit length table and
//    15 bits for the actual data.
// 4. If only one code exists, then it is encoded using one bit.  (Zero
//    would be more efficient, but perhaps a little confusing.)  If two
//    codes exist, they are coded using one bit each (0 and 1).
// 5. There is no way of sending zero distance codes--a dummy must be
//    sent if there are none.  (History: a pre 2.0 version of PKZIP would
//    store blocks with no distance codes, but this was discovered to be
//    too harsh a criterion.)  Valid only for 1.93a.  2.04c does allow
//    zero distance codes, which is sent as one code of zero bits in
//    length.
// 6. There are up to 286 literal/length codes.  Code 256 represents the
//    end-of-block.  Note however that the static length tree defines
//    288 codes just to fill out the Huffman codes.  Codes 286 and 287
//    cannot be used though, since there is no length base or extra bits
//    defined for them.  Similarily, there are up to 30 distance codes.
//    However, static trees define 32 codes (all 5 bits) to fill out the
//    Huffman codes, but the last two had better not show up in the data.
// 7. Unzip can check dynamic Huffman blocks for complete code sets.
//    The exception is that a single code would not be complete (see #4).
// 8. The five bits following the block type is really the number of
//    literal codes sent minus 257.
// 9. Length codes 8,16,16 are interpreted as 13 length codes of 8 bits
//    (1+6+6).  Therefore, to output three times the length, you output
//    three codes (1+1+1), whereas to output four times the same length,
//    you only need two codes (1+3).  Hmm.
//10. In the tree reconstruction algorithm, Code = Code + Increment
//    only if BitLength(i) is not zero.  (Pretty obvious.)
//11. Correction: 4 Bits: # of Bit Length codes - 4     (4 - 19)
//12. Note: length code 284 can represent 227-258, but length code 285
//    really is 258.  The last length deserves its own, short code
//    since it gets used a lot in very redundant files.  The length
//    258 is special since 258 - 3 (the min match length) is 255.
//13. The literal/length and distance code bit lengths are read as a
//    single stream of lengths.  It is possible (and advantageous) for
//    a repeat code (16, 17, or 18) to go across the boundary between
//    the two sets of lengths.
//

#define PKZIP_BUG_WORKAROUND    /* PKZIP 1.93a problem--live with it */

//
//  inflate.h must supply the unsigned char slide[WSIZE] array, the zvoid typedef
//  (void if (void *) is accepted, else char) and the NEXTBYTE,
//  FLUSH() and memzero macros.  If the window size is not 32K, it
//  should also define WSIZE.  If INFMOD is defined, it can include
//  compiled functions to support the NEXTBYTE and/or FLUSH() macros.
//  There are defaults for NEXTBYTE and FLUSH() below for use as
//  examples of what those functions need to do.  Normally, you would
//  also want FLUSH() to compute a crc on the data.

//  This module uses the external functions malloc() and free() (and
//  probably memset() or bzero() in the memzero() macro).  Their
//  prototypes are normally found in <string.h> and <stdlib.h>.
//

/* #define DEBUG */


#ifndef WSIZE           /* default is 32K */
#  define WSIZE 0x8000  /* window size--must be a power of two, and at least */
#endif                  /* 32K for zip's deflate method */

#  define wsize WSIZE       /* wsize is a constant */


#ifndef NEXTBYTE        /* default is to simply get a byte from stdin */
/* default for   define NEXTBYTE is  getchar() */
#ifdef UNIX_FILE_SYSTEM
    #define NEXTBYTE getc(global_file)
#elif defined(WIN32_FILE_SYSTEM)
    #define NEXTBYTE ((u1) (*global_file++))
#endif
#endif

#ifndef MESSAGE   /* only used twice, for fixed strings--NOT general-purpose */
#  define MESSAGE(str, len, flag)  fprintf(stderr, (char *) (str))
#endif

#ifndef FLUSH           /* default is to simply write the buffer to stdout */
/* default   define FLUSH(n) is fwrite(slide_buffer, 1, n, stdout)   return value not used */
#define FLUSH(n) memcpy(global_bufferp, slide_buffer, n); global_bufferp += n
#endif
/* Warning: the fwrite above might not work on 16-bit compilers, since
   0x8000 might be interpreted as -32768 by the library function. */

#ifndef Trace
#  ifdef DEBUG
#    define Trace(x) fprintf x
#  else
#    define Trace(x)
#  endif
#endif


/*---------------------------------------------------------------------------*/

// Macros for inflate() bit peeking and grabbing.
// The usage is:
//
//      NEEDBITS(j)
//      x = b & mask_bits[j];
//
//      DUMPBITS(j)
// where NEEDBITS makes sure that b has at least j bits in it, and
// DUMPBITS removes the bits from b.  The macros use the variable k
// for the number of bits in b.  Normally, b and k are register
// variables for speed and are initialized at the begining of a
// routine that uses these macros from a global bit buffer and count.
//
// In order to not ask for more bits than there are in the compressed
// stream, the Huffman tables are constructed to only ask for just
// enough bits to make up the end-of-block code (value 256).  Then no
// bytes need to be "returned" to the buffer at the end of the last
// block.  See the huft_build() routine.
//

#ifndef CHECK_EOF
#  define CHECK_EOF   /* default as of 5.13/5.2 */
#endif

#ifndef CHECK_EOF
#  define NEEDBITS(n) {while (k<(n)) { \
                         b |= ((unsigned long) NEXTBYTE) << k; \
                         k+=8;}}
#else
#  define NEEDBITS(n) {while (k<(n)) { \
                         int c = NEXTBYTE; \
                         if (c == EOF) return 1; \
                         b |= ((unsigned long) c) << k; \
                         k+=8;}}
#endif                      /* Piet Plomp:  change "return 1" to "break" */

#define DUMPBITS(n) {b>>=(n);k-=(n);}


//
// Huffman code lookup table entry--this entry is four bytes for machines
// that have 16-bit pointers (e.g. PC's in the small or medium model).
// Valid extra bits are 0..13.  e == 15 is EOB (end of block), e == 16
// means that v is a literal, 16 < e < 32 means that v is a pointer to
// the next table, which codes e - 16 bits, and lastly e == 99 indicates
// an unused code.  If a code with e == 99 is looked up, this implies an
// error in the data.
//
struct huft {
    unsigned char e;                /* number of extra bits or operation */
    unsigned char b;                /* number of bits in this code or subcode */
    union {
        unsigned short n;            /* literal, length base, or distance base */
        struct huft *t;   /* pointer to next level of table */
    } v;
};

//
// The inflate algorithm uses a sliding 32K byte window on the uncompressed
// stream to find repeated byte strings.  This is implemented here as a
// circular buffer.  The index is updated simply by incrementing and then
// and'ing with 0x7fff (32K-1). */
// It is left to other modules to supply the 32K area.  It is assumed
// to be usable as if it were declared "uch slide[32768];" or as just
// "uch *slide;" and then malloc'ed in the latter case.  The definition
// must be in unzip.h, included above.
//
class Unzip
{
public:
    static unsigned long global_bb;                         /* bit buffer */
    static unsigned global_bk;                    /* bits in bit buffer */

    static unsigned global_wp;  /* current position in slide */
    static unsigned global_hufts; /* huff memory usage */
    static unsigned char slide_buffer[];
    static struct huft *global_fixed_tl;    /* inflate static */
    static struct huft *global_fixed_td;    /* inflate static */
    static int global_fixed_bl,
               global_fixed_bd;
#ifdef UNIX_FILE_SYSTEM
    static FILE *global_file; /* file pointer for zip file */
#elif defined(WIN32_FILE_SYSTEM)
    static char *global_file;
#endif
    static char *global_bufferp; /* current position in output buffer */

    /* Tables for deflate from PKZIP's appnote.txt. */
    static unsigned border[];
    static unsigned short cplens[];
    static unsigned short cplext[]; /* Extra bits for literal codes 257..285 */
    static unsigned short cpdist[]; /* Copy offsets for distance codes 0..29 */
    static unsigned short cpdext[]; /* Extra bits for distance codes */

    /* moved to consts.h (included in unzip.c), resp. funzip.c */
    /* And'ing with mask_bits[n] masks the lower n bits */
    static unsigned short mask_bits[];

    //
    // Huffman code decoding is performed using a multi-level table lookup.
    // The fastest way to decode is to simply build a lookup table whose
    // size is determined by the longest code.  However, the time it takes
    // to build this table can also be a factor if the data being decoded
    // are not very long.  The most common codes are necessarily the
    // shortest codes, so those codes dominate the decoding time, and hence
    // the speed.  The idea is you can have a shorter table that decodes the
    // shorter, more probable codes, and then point to subsidiary tables for
    // the longer codes.  The time it costs to decode the longer codes is
    // then traded against the time it takes to make longer tables.
    //
    // This results of this trade are in the variables lbits and dbits
    // below.  lbits is the number of bits the first level table for literal/
    // length codes can decode in one step, and dbits is the same thing for
    // the distance codes.  Subsequent tables are also less than or equal to
    // those sizes.  These values may be adjusted either when all of the
    // codes are shorter than that, in which case the longest code length in
    // bits is used, or when the shortest code is *longer* than the requested
    // table size, in which case the length of the shortest code in bits is
    // used.
    //
    // There are two different values for the two tables, since they code a
    // different number of possibilities each.  The literal/length table
    // codes 286 possible values, or in a flat code, a little over eight
    // bits.  The distance table codes 30 possible values, or a little less
    // than five bits, flat.  The optimum values for speed end up being
    // about one bit more than those, so lbits is 8+1 and dbits is 5+1.
    // The optimum values may differ though from machine to machine, and
    // possibly even between compilers.  Your mileage may vary.
    //

    static int lbits;           /* bits in base literal/length lookup table */
    static int dbits;           /* bits in base distance lookup table */

    static int huft_build(unsigned *b, unsigned n, unsigned s,
                          unsigned short *d, unsigned short *e,
                          struct huft **t, int *m);
    static int huft_free(struct huft *);
    static int inflate_codes(struct huft *tl, struct huft * td,
                             int  bl, int bd);
    static int inflate_stored();
    static int inflate_fixed();
    static int inflate_dynamic();
    static int inflate_block(int *);
    static int inflate_free();

#ifdef UNIX_FILE_SYSTEM
    static int unzip8(FILE * zipfile, char *buffer);

    static int UncompressFile0(FILE *, char *, long);
    static int UncompressFile1(FILE *, char *, long);
    static int UncompressFile2(FILE *, char *, long);
    static int UncompressFile3(FILE *, char *, long);
    static int UncompressFile4(FILE *, char *, long);
    static int UncompressFile5(FILE *, char *, long);
    static int UncompressFile6(FILE *, char *, long);
    static int UncompressFile7(FILE *, char *, long);
    static int UncompressFile8(FILE *, char *, long);
    static int UncompressFile9(FILE *, char *, long);
#elif defined(WIN32_FILE_SYSTEM)
    static int unzip8(char *zipfile, char *buffer);

    static int UncompressFile0(char *, char *, long);
    static int UncompressFile1(char *, char *, long);
    static int UncompressFile2(char *, char *, long);
    static int UncompressFile3(char *, char *, long);
    static int UncompressFile4(char *, char *, long);
    static int UncompressFile5(char *, char *, long);
    static int UncompressFile6(char *, char *, long);
    static int UncompressFile7(char *, char *, long);
    static int UncompressFile8(char *, char *, long);
    static int UncompressFile9(char *, char *, long);
#endif
}; // end class unzip

#ifdef HAVE_JIKES_NAMESPACE
} // Close namespace Jikes block
#endif

#endif // unzip_INCLUDED

