// $Id: gencode.java,v 1.10 2001/09/14 05:31:33 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1999, 2000, 2001, International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

import java.io.*;
import java.util.Arrays;

/**
 * This helper class generates code.h and code.cpp, the lookup table used
 * by Jikes in deciding how to categorize Unicode input characters.  This
 * is written in Java in order to accurately track the Unicode rules as
 * implemented by the latest version of java.lang.Character.
 */
class gencode
{
    static final int NEWLINE_CODE      = 1; // \n, \r
    static final int SPACE_CODE        = 2; // \t, \f, ' '
    static final int BAD_CODE          = 3; // everything not covered by other codes ...
    static final int DIGIT_CODE        = 4; // '0'..'9'                    
    static final int OTHER_DIGIT_CODE  = 5; // all unicode digits
    static final int LOWER_CODE        = 6; // 'a'..'z'
    static final int UPPER_CODE        = 7; // 'A'..'Z'
    static final int OTHER_LETTER_CODE = 8; // '$', '_', all other unicode letters

    //
    // Must be 0 for flat listing, or a value between 2..13 for 2-level listing.
    // Larger values make smaller slots, allowing more duplication between slots,
    // at the expense of more pointer overhead.
    // For Unicode as of JDK 1.4-beta, manual experiments show best results with 9.
    //
    static final int LOG_BASE_SIZE       = 9;

    static final int LOG_COMPLEMENT_SIZE = (16 - LOG_BASE_SIZE);
    static final int BASE_SIZE           = (1 << LOG_BASE_SIZE);
    static final int SLOT_SIZE           = (1 << LOG_COMPLEMENT_SIZE);
    static final int SLOT_MASK           = (SLOT_SIZE - 1);

    static final int BaseIndex(int i) { return i >> LOG_COMPLEMENT_SIZE; }
    static final int DataIndex(int i) { return i & SLOT_MASK; }

    static public void main(String args[]) throws FileNotFoundException, IOException
    {
        int num_elements = 65536;
        int num_slots = BASE_SIZE;

        byte base[][] = new byte[BASE_SIZE][SLOT_SIZE];

        char a = '\u0000';
        for (int i = 0; i < 65536; i++, a++)
        {
            if (a == '\n' || a == '\r')
                base[BaseIndex(i)][DataIndex(i)] = NEWLINE_CODE;
            else if (a==' ' || a=='\t' || a=='\f')
                base[BaseIndex(i)][DataIndex(i)] = SPACE_CODE;
            else if (a < 128 && Character.isLowerCase(a)) // Ascii lower case
                base[BaseIndex(i)][DataIndex(i)] = LOWER_CODE;
            else if (a < 128 && Character.isUpperCase(a)) // Ascii upper case
                base[BaseIndex(i)][DataIndex(i)] = UPPER_CODE;
            else if (a < 128 && Character.isDigit(a)) // Ascii digit
                base[BaseIndex(i)][DataIndex(i)] = DIGIT_CODE;
            else if (Character.isJavaIdentifierStart(a))
                base[BaseIndex(i)][DataIndex(i)] = OTHER_LETTER_CODE;
            else if (Character.isJavaIdentifierPart(a))
                base[BaseIndex(i)][DataIndex(i)] = OTHER_DIGIT_CODE;
            else
            {
                base[BaseIndex(i)][DataIndex(i)] = BAD_CODE;
                num_elements--;
            }
        }

        //
        // Process the code.h file
        //
        PrintStream hfile = new PrintStream(new FileOutputStream("code.h"));
        printHeader(hfile, new String[] {"\"platform.h\""});

        //
        // Process the code.cpp file
        //
        PrintStream cfile = new PrintStream(new FileOutputStream("code.cpp"));
        printHeader(cfile, new String[] {"\"code.h\""});

        if (LOG_BASE_SIZE > 1 && LOG_BASE_SIZE < 14)
        {
            //
            // We exploit the fact that array elements are initialized to 0.
            //
            int base_index[] = new int[BASE_SIZE],
                slot_index[] = new int[BASE_SIZE],
                overlaps[] = new int[BASE_SIZE],
                offset = SLOT_SIZE,
                prior = 0;

            for (int i = 1; i < BASE_SIZE; i++)
                if (! Arrays.equals(base[0], base[i]))
                    slot_index[i] = i;

            //
            // Now, check for places to share entries.  While a more compact form
            // can be found, this one is relatively simple: match all duplicate
            // slots, then find largest overlap of a single category between
            // successive slots.
            //
            for (int i = 1; i < BASE_SIZE; i++)
            {
                if (slot_index[i] < i) // we've already matched this slot
                    continue;

                int overlap = 0;
                byte value = base[i][0];
                if (base[prior][SLOT_SIZE - overlap - 1] == value)
                {
                    //
                    // Find the number of repeated bytes "value" at end of prior
                    // slot and beginning of this slot
                    //
                    do overlap++;
                    while (base[prior][SLOT_SIZE - overlap - 1] == value &&
                           base[i][overlap] == value);
                    overlaps[i] = overlap;
                    offset -= overlap;
                }

                base_index[i] = offset;
                offset += SLOT_SIZE;
                prior = i;

                for (int k = i + 1; k < BASE_SIZE; k++)
                {
                    if (slot_index[k] < k) // we've already matched this slot
                        continue;
                    if (Arrays.equals(base[i], base[k])) // found new match
                    {
                        slot_index[k] = i;
                        base_index[k] = base_index[i];
                        num_slots--;
                    }
                }
            }

            hfile.println("#ifndef code_INCLUDED");
            hfile.println("#define code_INCLUDED");
            hfile.println();
            hfile.println("class Code");
            hfile.println("{");
            hfile.println("    //");
            hfile.println("    // To facilitate the scanning, the character set is partitioned into");
            hfile.println("    // 8 classes using the array CODE. The classes are described below");
            hfile.println("    // together with some self-explanatory functions defined on CODE.");
            hfile.println("    //");
            hfile.println("    enum {");

            hfile.println("             LOG_BASE_SIZE       = " + LOG_BASE_SIZE + ',');
            hfile.println("             LOG_COMPLEMENT_SIZE = " + LOG_COMPLEMENT_SIZE + ',');
            hfile.println("             BASE_SIZE           = " + BASE_SIZE + ',');
            hfile.println("             SLOT_SIZE           = " + SLOT_SIZE + ',');
            hfile.println("             SLOT_MASK           = " + SLOT_MASK + ',');
            hfile.println();
            hfile.println("             NEWLINE_CODE        = " + NEWLINE_CODE + ',');
            hfile.println("             SPACE_CODE          = " + SPACE_CODE + ',');
            hfile.println("             BAD_CODE            = " + BAD_CODE + ',');
            hfile.println("             DIGIT_CODE          = " + DIGIT_CODE + ',');
            hfile.println("             OTHER_DIGIT_CODE    = " + OTHER_DIGIT_CODE + ',');
            hfile.println("             LOWER_CODE          = " + LOWER_CODE + ',');
            hfile.println("             UPPER_CODE          = " + UPPER_CODE + ',');
            hfile.println("             OTHER_LETTER_CODE   = " + OTHER_LETTER_CODE);
            hfile.println("         };");
            hfile.println();
            hfile.println("    static char code[" + offset + "];");
            hfile.println("    static char *base[" +  BASE_SIZE + "];");
            hfile.println();
            hfile.println();
            hfile.println("public:");
            hfile.println();
            hfile.println("    static inline void SetBadCode(wchar_t c)");
            hfile.println("    {");
            hfile.println("        base[c >> LOG_COMPLEMENT_SIZE][c] = BAD_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline void CodeCheck(wchar_t c)");
            hfile.println("    {");
            hfile.println("        assert(c >> LOG_COMPLEMENT_SIZE < BASE_SIZE);");
            hfile.println("        assert(base[c >> LOG_COMPLEMENT_SIZE] + c >= (&code[0]));");
            hfile.println("        assert(base[c >> LOG_COMPLEMENT_SIZE] + c < (&code[" + offset + "]));");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool CodeCheck(void)");
            hfile.println("    {");
            hfile.println("        for (int i = 0; i <= 0xffff; i++)");
            hfile.println("            CodeCheck((wchar_t) i);");
            hfile.println("        return true;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsNewline(wchar_t c) // \\r characters are replaced by \\x0a in Stream::ProcessInput().");
            hfile.println("    {");
            hfile.println("        return c == '\\x0a';");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsSpaceButNotNewline(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] == SPACE_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsSpace(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] <= SPACE_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsDigit(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] == DIGIT_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsUpper(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] == UPPER_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsLower(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] == LOWER_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsAlpha(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] >= LOWER_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsAlnum(wchar_t c)");
            hfile.println("    {");
            hfile.println("        return base[c >> LOG_COMPLEMENT_SIZE][c] >= DIGIT_CODE;");
            hfile.println("    }");
            hfile.println();
            hfile.println();
            hfile.println("};");
            hfile.println();
            hfile.println("#endif // code_INCLUDED");

            cfile.println("char Code::code[" +  offset + "] =");
            cfile.println("{");

            for (int j = 0; j < BASE_SIZE; j++)
            {
                cfile.println("    //");
                cfile.print  ("    // Slot " + j  + ":");

                if (slot_index[j] < j)
                {
                    cfile.println(" matches Slot " + slot_index[j]);
                    cfile.println("    //");
                    cfile.println();
                    continue;
                }

                if (overlaps[j] > 0)
                    cfile.print(" overlaps prior slot by " + overlaps[j]);
                cfile.println();
                cfile.println("    //");

                byte slot[] = base[j];
                for (int k = overlaps[j]; k < SLOT_SIZE; k += 4)
                {
                    for (int l = 0; l < 4; l++)
                    {
                        if (k + l >= SLOT_SIZE)
                            break;

                        cfile.print(l == 0 ? "    " : " ");
                        switch(slot[k + l])
                        {
                        case NEWLINE_CODE:
                            cfile.print("NEWLINE_CODE,");
                            break;
                        case SPACE_CODE:
                            cfile.print("SPACE_CODE,");
                            break;
                        case BAD_CODE:
                            cfile.print("BAD_CODE,");
                            break;
                        case DIGIT_CODE:
                            cfile.print("DIGIT_CODE,");
                            break;
                        case OTHER_DIGIT_CODE:
                            cfile.print("OTHER_DIGIT_CODE,");
                            break;
                        case LOWER_CODE:
                            cfile.print("LOWER_CODE,");
                            break;
                        case UPPER_CODE:
                            cfile.print("UPPER_CODE,");
                            break;
                        default:
                            cfile.print("OTHER_LETTER_CODE,");
                            break;
                        }
                    }
                    cfile.println();
                }

                cfile.println();
            }

            cfile.println("};");

            cfile.println();
            cfile.println();
            cfile.println("//");
            cfile.println("// The Base vector:");
            cfile.println("//");
            cfile.println("char *Code::base[" + BASE_SIZE + "] =");
            cfile.println("{");
            for (int k = 0; k < BASE_SIZE; k += 4)
            {
                for (int i = 0; i < 4; i++)
                {
                    int j = k + i;
                    cfile.print(i == 0 ? "   " : " ");
                    cfile.print(" &code[" + base_index[j] + "] - " +
                                (j * SLOT_SIZE) + ",");
                }
                cfile.println();
            }
            cfile.println("};");

            //
            // Print Statistics
            //
            System.out.println(" The number of slots used is " + num_slots);
            System.out.println(" Total static storage utilization is " +
                               offset + " bytes for encoding plus " +
                               BASE_SIZE * 4 + " bytes for the base");
            System.out.println(" Savings of " + (65536 - offset - BASE_SIZE * 4) + " bytes over flat listing");
            System.out.println(" The number of unicode characters is " + num_elements);
        }
        else if (LOG_BASE_SIZE == 0)
        {
            hfile.println("#ifndef code_INCLUDED");
            hfile.println("#define code_INCLUDED");
            hfile.println();
            hfile.println("class Code");
            hfile.println("{");
            hfile.println("    //");
            hfile.println("    // To facilitate the scanning, the character set is partitioned into");
            hfile.println("    // 8 classes using the array CODE. The classes are described below");
            hfile.println("    // together with some self-explanatory functions defined on CODE.");
            hfile.println("    //");
            hfile.println("    enum {");
            hfile.println("             NEWLINE_CODE      = " + NEWLINE_CODE + ",");
            hfile.println("             SPACE_CODE        = " + SPACE_CODE + ",");
            hfile.println("             BAD_CODE          = " + BAD_CODE + ",");
            hfile.println("             DIGIT_CODE        = " + DIGIT_CODE + ",");
            hfile.println("             OTHER_DIGIT_CODE  = " + OTHER_DIGIT_CODE + ",");
            hfile.println("             LOWER_CODE        = " + LOWER_CODE + ",");
            hfile.println("             UPPER_CODE        = " + UPPER_CODE + ",");
            hfile.println("             OTHER_LETTER_CODE = " + OTHER_LETTER_CODE);
            hfile.println("         };");
            hfile.println();
            hfile.println("    static char code[65536];");
            hfile.println();
            hfile.println();
            hfile.println("public:");
            hfile.println();
            hfile.println("    static inline bool CodeCheck(void) { return true; }");
            hfile.println();
            hfile.println("    //");
            hfile.println("    // \\r characters are replaced by \\x0a in read_input.");
            hfile.println("    //");
            hfile.println("    static inline bool IsNewline(wchar_t c)            { return c == '\\x0a'; }");
            hfile.println("    static inline bool IsSpaceButNotNewline(wchar_t c) { return code[c] == SPACE_CODE; }");
            hfile.println("    static inline bool IsSpace(wchar_t c)              { return code[c] <= SPACE_CODE; }");
            hfile.println("    static inline bool IsDigit(wchar_t c)              { return code[c] == DIGIT_CODE; }");
            hfile.println("    static inline bool IsUpper(wchar_t c)              { return code[c] == UPPER_CODE; }");
            hfile.println("    static inline bool IsLower(wchar_t c)              { return code[c] == LOWER_CODE; }");
            hfile.println("    static inline bool IsAlpha(wchar_t c)              { return code[c] >= LOWER_CODE; }");
            hfile.println("    static inline bool IsAlnum(wchar_t c)              { return code[c] >= DIGIT_CODE; }");
            hfile.println();
            hfile.println("};");
            hfile.println();
            hfile.println("#endif // code_INCLUDED");

            cfile.println("char Code::code[65536] =");
            cfile.println("{");

            int k = 0;
            for (int i = 0; i < 65536; i += 256)
            {
                cfile.println("    //");
                cfile.println("    // Slot " + i + ":");
                cfile.println("    //");

                for (int j = 0; j < 256; j += 4)
                {
                    for (int l = 0; l < 4; l++)
                    {
                        byte b = base[BaseIndex(k)][DataIndex(k)];
                        k++;
                        cfile.print(l == 0 ? "    " : " ");
                        switch(b)
                        {
                            case NEWLINE_CODE:
                                 cfile.print("NEWLINE_CODE,");
                                 break;
                            case SPACE_CODE:
                                 cfile.print("SPACE_CODE,");
                                 break;
                            case BAD_CODE:
                                 cfile.print("BAD_CODE,");
                                 break;
                            case DIGIT_CODE:
                                 cfile.print("DIGIT_CODE,");
                                 break;
                            case OTHER_DIGIT_CODE:
                                 cfile.print("OTHER_DIGIT_CODE,");
                                 break;
                            case LOWER_CODE:
                                 cfile.print("LOWER_CODE,");
                                 break;
                            case UPPER_CODE:
                                 cfile.print("UPPER_CODE,");
                                 break;
                            default:
                                 cfile.print("OTHER_LETTER_CODE,");
                                 break;
                        }
                    }
                    cfile.println();
                }

                cfile.println();
            }

            cfile.println("};");

            //
            // Print Statistics
            //
            System.out.println(" The number of unicode letters is " + num_elements);
            System.out.println(" Total static storage utilization is 65536");
        }
        else throw new IllegalArgumentException("illegal LOG_BASE_SIZE");

        printFooter(hfile);
        printFooter(cfile);

        hfile.close();
        cfile.close();
    }

    static void printHeader(PrintStream file, String[] includes)
    {
        file.println("// $I" + /* CVS hack */ "d$ -*- c++ -*-");
        file.println("// DO NOT MODIFY THIS FILE - it is generated using gencode.java.");
        file.println("//");
        file.println("// This software is subject to the terms of the IBM Jikes Compiler");
        file.println("// License Agreement available at the following URL:");
        file.println("// http://www.ibm.com/research/jikes.");
        file.println("// Copyright (C) 1999, 2000, 2001, International Business");
        file.println("// Machines Corporation and others.  All Rights Reserved.");
        file.println("// You must accept the terms of that agreement to use this software.");
        file.println("//");
        file.println();

        for (int i = 0; i < includes.length; i++)
        {
            file.println("#include " + includes[i]);
        }

        file.println();
        file.println("#ifdef HAVE_JIKES_NAMESPACE");
        file.println("namespace Jikes { // Open namespace Jikes block");
        file.println("#endif");
        file.println();
    }

    static void printFooter(PrintStream file)
    {
        file.println();
        file.println("#ifdef HAVE_JIKES_NAMESPACE");
        file.println("} // Close namespace Jikes block");
        file.println("#endif");
        file.println();
    }

}
