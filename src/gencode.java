// $Id: gencode.java,v 1.12 2002/03/06 17:12:25 ericb Exp $
//
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1999, 2000, 2001, 2002 International Business
// Machines Corporation and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.
//

import java.io.*;
import java.util.*;

/**
 * This helper class generates code.h and code.cpp, the lookup table used
 * by Jikes in deciding how to categorize Unicode input characters.  This
 * is written in Java in order to accurately track the Unicode rules as
 * implemented by the latest version of java.lang.Character.
 */
class gencode
{
    static final int NEWLINE_CODE      = 0; // '\n', '\r'
    static final int SPACE_CODE        = 1; // '\t', '\f', ' '
    static final int BAD_CODE          = 2; // everything else ...
    static final int DIGIT_CODE        = 3; // '0'..'9'
    static final int OTHER_DIGIT_CODE  = 4; // Character.isJavaIdentifierPart
    static final int LOWER_CODE        = 5; // 'a'..'z'
    static final int UPPER_CODE        = 6; // 'A'..'Z'
    static final int OTHER_LETTER_CODE = 7; // Character.isJavaIdentifierStart

    static final String[] CODE_NAMES = {
        "NEWLINE_CODE", "SPACE_CODE", "BAD_CODE", "DIGIT_CODE",
        "OTHER_DIGIT_CODE", "LOWER_CODE", "UPPER_CODE", "OTHER_LETTER_CODE"
    };

//      /**
//       * The amount to shift a character by in the first level lookup.
//       */
//      static int LOG_BASE_SIZE       = 9;

//      static final int LOG_COMPLEMENT_SIZE = (16 - LOG_BASE_SIZE);
//      static final int BASE_SIZE           = (1 << LOG_BASE_SIZE);
//      static final int SLOT_SIZE           = (1 << LOG_COMPLEMENT_SIZE);
//      static final int SLOT_MASK           = (SLOT_SIZE - 1);

//      static final int BaseIndex(int i) { return i >> LOG_COMPLEMENT_SIZE; }
//      static final int DataIndex(int i) { return i & SLOT_MASK; }

    static public void main(String args[])
        throws FileNotFoundException, IOException
    {
        System.out.println("Gathering data...");
        // Use char[] for ease in building strings, despite only using 8 bits.
        int numElements = 65536;
        char[] info = new char[numElements];
        for (int i = 0; i < info.length; i++)
        {
            if (i == '\n' || i == '\r')
                info[i] = NEWLINE_CODE;
            else if (i == ' ' || i == '\t' || i == '\f')
                info[i] = SPACE_CODE;
            else if (i < 128 && Character.isLowerCase((char) i))
                info[i] = LOWER_CODE; // Ascii lower case
            else if (i < 128 && Character.isUpperCase((char) i))
                info[i] = UPPER_CODE; // Ascii upper case
            else if (i < 128 && Character.isDigit((char) i))
                info[i] = DIGIT_CODE; // Ascii digit
            else if (Character.isJavaIdentifierStart((char) i))
                info[i] = OTHER_LETTER_CODE;
            else if (Character.isJavaIdentifierPart((char) i))
                info[i] = OTHER_DIGIT_CODE;
            else
            {
                info[i] = BAD_CODE;
                numElements--;
            }
        }

        System.out.println("Compressing tables...");
        int bestShift = 0;
        int bestEst = info.length;
        String bestBlkStr = null;

        for (int i = 3; i < 11; i++)
        {
            int blkSize = 1 << i;
            Map blocks = new HashMap();
            List blkArray = new ArrayList();
            System.out.print("shift: " + i);

            for (int j = 0; j < info.length; j += blkSize)
            {
                String key = new String(info, j, blkSize);
                if (blocks.get(key) == null)
                {
                    blkArray.add(key);
                    blocks.put(key, new Integer(blkArray.size()));
                }
            }
            int blkNum = blkArray.size();
            int blockLen = blkNum * blkSize;
            System.out.print(" before " + blockLen);

            //
            // Try to pack blkArray, by finding successively smaller matches
            // between heads and tails of blocks.
            //
            for (int j = blkSize - 1; j > 0; j--)
            {
                Map tails = new HashMap();
                for (int k = 0; k < blkArray.size(); k++)
                {
                    String str = (String) blkArray.get(k);
                    if (str == null)
                        continue;
                    String tail = str.substring(str.length() - j);
                    List l = (List) tails.get(tail);
                    if (l == null)
                        tails.put(tail,
                                  new LinkedList(Collections
                                                 .singleton(new Integer(k))));
                    else l.add(new Integer(k));
                }

                //
                // Now calculate the heads, and merge overlapping blocks
                //
            block:
                for (int k = 0; k < blkArray.size(); k++)
                {
                    String tomerge = (String) blkArray.get(k);
                    if (tomerge == null)
                        continue;
                    while (true)
                    {
                        String head = tomerge.substring(0, j);
                        LinkedList entry = (LinkedList) tails.get(head);
                        if (entry == null)
                            continue block;
                        Integer other = (Integer) entry.removeFirst();
                        if (other.intValue() == k)
                        {
                            if (entry.size() > 0)
                            {
                                entry.add(other);
                                other = (Integer) entry.removeFirst();
                            }
                            else
                            {
                                entry.add(other);
                                continue block;
                            }
                        }
                        if (entry.size() == 0)
                            tails.remove(head);

                        //
                        // A match was found.
                        //
                        String merge = blkArray.get(other.intValue()) +
                            tomerge.substring(j);
                        blockLen -= j;
                        blkNum--;
                        if (other.intValue() < k)
                        {
                            blkArray.set(k, null);
                            blkArray.set(other.intValue(), merge);
                            String tail = merge.substring(merge.length() - j);
                            List l = (List) tails.get(tail);
                            Collections.replaceAll(l, new Integer(k), other);
                            continue block;
                        }
                        blkArray.set(k, merge);
                        blkArray.set(other.intValue(), null);
                        tomerge = merge;
                    }
                }
            }
            StringBuffer blockStr = new StringBuffer(blockLen);
            for (int k = 0; k < blkArray.size(); k++)
            {
                String str = (String) blkArray.get(k);
                if (str != null)
                    blockStr.append(str);
            }
            if (blockStr.length() != blockLen)
                throw new Error("Unexpected blockLen " + blockLen);
            int estimate = blockLen + (info.length >> (i - 1));
            System.out.println(" after merge " + blockLen + ": " + estimate +
                               " bytes");
            if (estimate < bestEst)
            {
                bestEst = estimate;
                bestShift = i;
                bestBlkStr = blockStr.toString();
            }
        }

        int blkSize = 1 << bestShift;
        char[] blocks = new char[info.length / blkSize];
        for (int j = 0; j < info.length; j += blkSize)
        {
            String key = new String(info, j, blkSize);
            int index = bestBlkStr.indexOf(key);
            if (index == -1)
                throw new Error("Unexpected index for " + j);
            blocks[j >> bestShift] = (char) (index - j);
        }

        //
        // Process the code.h file
        //
        System.out.println("Generating code.h with shift of " + bestShift);
        PrintStream hfile = new PrintStream(new FileOutputStream("code.h"));
        printHeader(hfile, new String[] {"\"platform.h\""});
        hfile.println("#ifndef code_INCLUDED");
        hfile.println("#define code_INCLUDED");
        hfile.println();
        hfile.println("class Code");
        hfile.println("{");
        hfile.println("    //");
        hfile.println("    // To facilitate the scanning, the character set is partitioned into");
        hfile.println("    // 8 categories using the array CODE. These are described below");
        hfile.println("    // together with some self-explanatory functions defined on CODE.");
        hfile.println("    //");
        hfile.println("    enum {");
        hfile.println("        SHIFT = " + bestShift + ",");
        hfile.println("        NEWLINE_CODE = " + NEWLINE_CODE + ',');
        hfile.println("        SPACE_CODE = " + SPACE_CODE + ',');
        hfile.println("        BAD_CODE = " + BAD_CODE + ',');
        hfile.println("        DIGIT_CODE = " + DIGIT_CODE + ',');
        hfile.println("        OTHER_DIGIT_CODE = " + OTHER_DIGIT_CODE + ',');
        hfile.println("        LOWER_CODE = " + LOWER_CODE + ',');
        hfile.println("        UPPER_CODE = " + UPPER_CODE + ',');
        hfile.println("        OTHER_LETTER_CODE = " + OTHER_LETTER_CODE);
        hfile.println("    };");
        hfile.println();
        hfile.println("    static char codes[" + bestBlkStr.length() + "];");
        hfile.println("    static u2 blocks[" +  blocks.length + "];");
        hfile.println();
        hfile.println();
        hfile.println("public:");
        hfile.println();
        hfile.println("    static inline void SetBadCode(wchar_t c)");
        hfile.println("    {");
        hfile.println("        codes[(u2) (blocks[c >> SHIFT] + c)] = BAD_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline void CodeCheck(wchar_t c)");
        hfile.println("    {");
        hfile.println("        assert((u2) (blocks[c >> SHIFT] + c) < " +
                      bestBlkStr.length() + ");");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool CodeCheck(void)");
        hfile.println("    {");
        hfile.println("        for (int i = 0; i <= 0xffff; i++)");
        hfile.println("            CodeCheck((wchar_t) i);");
        hfile.println("        return true;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    //");
        hfile.println("    // \\r characters are replaced by \\x0a in Stream::ProcessInput().");
        hfile.println("    //");
        hfile.println("    static inline bool IsNewline(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return c == '\\x0a';");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsSpaceButNotNewline(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] == SPACE_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsSpace(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] <= SPACE_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsDigit(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] == DIGIT_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsOctalDigit(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return c >= U_0 && c <= U_7;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsHexDigit(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return c <= U_f && (c >= U_a ||");
        hfile.println("                            (c >= U_A && c <= U_F) ||");
        hfile.println("                            (c >= U_0 && c <= U_9));");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsUpper(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] == UPPER_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsLower(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] == LOWER_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsAlpha(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] >= LOWER_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("    static inline bool IsAlnum(wchar_t c)");
        hfile.println("    {");
        hfile.println("        return codes[(u2) (blocks[c >> SHIFT] + c)] >= DIGIT_CODE;");
        hfile.println("    }");
        hfile.println();
        hfile.println("};");
        hfile.println();
        hfile.println("#endif // code_INCLUDED");
        printFooter(hfile);
        hfile.close();

        //
        // Process the code.cpp file
        //
        System.out.println("Generating code.cpp");
        PrintStream cfile = new PrintStream(new FileOutputStream("code.cpp"));
        printHeader(cfile, new String[] {"\"code.h\""});
        cfile.println("char Code::codes[" + bestBlkStr.length() + "] =");
        cfile.println("{");
        for (int j = 0; j < bestBlkStr.length(); j += 4)
        {
            for (int k = 0; k < 4; k++)
            {
                if (k + j >= bestBlkStr.length())
                    break;
                if (k == 0)
                    cfile.print("   ");
                cfile.print(" " + CODE_NAMES[bestBlkStr.charAt(k + j)] + ",");
            }
            cfile.println();
        }
        cfile.println("};");
        cfile.println();
        cfile.println();
        cfile.println("//");
        cfile.println("// The Blocks vector:");
        cfile.println("//");
        cfile.println("u2 Code::blocks[" + blocks.length + "] =");
        cfile.println("{");
        for (int k = 0; k < blocks.length; k += 9)
        {
            for (int i = 0; i < 9; i++)
            {
                if (k + i >= blocks.length)
                    break;
                if (i == 0)
                    cfile.print("   ");
                cfile.print(" 0x" + Integer.toHexString(blocks[k + i]) + ",");
            }
            cfile.println();
        }
        cfile.println("};");
        printFooter(cfile);
        cfile.close();

        //
        // Print statistics.
        //
        System.out.println("Total static storage utilization is " +
                           blocks.length * 2 + " bytes for block lookup");
        System.out.println("   plus " + bestBlkStr.length() +
                           " bytes for the encodings");
        System.out.println("The number of unicode characters legal in Java sourcecode is " +
                           numElements);
    }

    static void printHeader(PrintStream file, String[] includes)
    {
        file.println("// $I" + /* CVS hack */ "d$ -*- c++ -*-");
        file.println("// DO NOT MODIFY THIS FILE - it is generated using gencode.java.");
        file.println("//");
        file.println("// This software is subject to the terms of the IBM Jikes Compiler");
        file.println("// License Agreement available at the following URL:");
        file.println("// http://www.ibm.com/research/jikes.");
        file.println("// Copyright (C) 1999, 2000, 2001, 2002, International Business");
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
