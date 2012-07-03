import java.lang.Integer;
import java.lang.Character;
import java.io.PrintStream;
import java.io.FileOutputStream;

class gencode
{
    static final int NEWLINE_CODE      = 1; // \n, \r
    static final int SPACE_CODE        = 2; // \t, \v, \f, ' '
    static final int BAD_CODE          = 3; // everything not covered by other codes ...
    static final int DIGIT_CODE        = 4; // '0'..'9'                    
    static final int OTHER_DIGIT_CODE  = 5; // all unicode digits
    static final int LOWER_CODE        = 6; // 'a'..'z'
    static final int UPPER_CODE        = 7; // 'A'..'Z'
    static final int OTHER_LETTER_CODE = 8; // '$', '_', all other unicode letters

    static final int LOG_BASE_SIZE       = 9; // must be a value between 0..16
    static final int LOG_COMPLEMENT_SIZE = (16 - LOG_BASE_SIZE);
    static final int BASE_SIZE           = (1 << LOG_BASE_SIZE);
    static final int SLOT_SIZE           = (1 << LOG_COMPLEMENT_SIZE);
    static final int SLOT_MASK           = (SLOT_SIZE - 1);

    static final int BaseIndex(int i) { return i >> LOG_COMPLEMENT_SIZE; }
    static final int DataIndex(int i) { return i & SLOT_MASK; }

    static public void main(String args[]) throws java.io.FileNotFoundException, java.io.IOException
    {
        int num_elements = 65536,
            num_slots = BASE_SIZE;

        byte base[][] = new byte[BASE_SIZE + 1][SLOT_SIZE];
        if (LOG_BASE_SIZE > 0 && LOG_BASE_SIZE < 16)
        {
            for (int i = 0; i < SLOT_SIZE; i++)
                base[BASE_SIZE][i] = BAD_CODE;
            num_slots++;
        }

        for (int i = 0; i < 65536; i++)
        {
            char a = (char) i;

            if (a == '\n' || a == '\r')
                 base[BaseIndex(i)][DataIndex(i)] = NEWLINE_CODE;
            else if (Character.isWhitespace(a))
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

        for (int i = 0; i < BASE_SIZE; i++)
        {
            int k;
            for (k = 0; k < SLOT_SIZE; k++)
            {
                if (base[i][k] != BAD_CODE)
                    break;
            }

            if (k == SLOT_SIZE)
            {
                base[i] = base[BASE_SIZE];
                num_slots--;
            }
        }

        //
        // Process the code.h file
        //
        PrintStream hfile = new PrintStream(new FileOutputStream("code.h"));

        //
        // Process the code.cpp file
        //
        PrintStream cfile = new PrintStream(new FileOutputStream("code.cpp"));

        if (LOG_BASE_SIZE > 0 && LOG_BASE_SIZE < 16)
        {
            hfile.println("#ifndef code_INCLUDED");
            hfile.println("#define code_INCLUDED");
            hfile.println();
            hfile.println("#include \"config.h\"");
            hfile.println("#include <ctype.h>");
            hfile.println("#include <assert.h>");
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
            hfile.println("    static char code[" + num_slots * SLOT_SIZE + "];");
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
            hfile.println("         assert(c >> LOG_COMPLEMENT_SIZE < BASE_SIZE);");
            hfile.println("         assert(base[c >> LOG_COMPLEMENT_SIZE] + c >= (&code[0]));");
            hfile.println("         assert(base[c >> LOG_COMPLEMENT_SIZE] + c < (&code[" + num_slots * SLOT_SIZE + "]));");
            hfile.println("    }");
            hfile.println();
            hfile.println("    static inline bool IsNewline(wchar_t c) // \\r characters are replaced by \\x0a in read_input.");
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
            hfile.println("#endif");

            cfile.println("#include \"code.h\"");
            cfile.println();
            cfile.println("char Code::code[" +  num_slots * SLOT_SIZE + "] =");
            cfile.println("{");

            int base_index[] = new int[BASE_SIZE + 1],
                num = 0;

            for (int j = 0; j <= BASE_SIZE; j++)
            {
                cfile.println("    //");
                cfile.println("    // Slot " + j  + ":");
                cfile.println("    //");

                byte slot[] = base[j];
                if (j != BASE_SIZE && slot == base[BASE_SIZE])
                {
                    base_index[j] = -1;
                }
                else
                {
                    base_index[j] = num;
                    num += SLOT_SIZE;
                    for (int k = 0; k < SLOT_SIZE; k += 4)
                    {
                        for (int l = 0; l < 4; l++)
                        {
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
                    cfile.print(" &code[" + 
                                (base_index[j] >= 0 ? base_index[j] : base_index[BASE_SIZE]) +
                                "] - " +
                                (j * SLOT_SIZE) +
                                  ",");
                }
                cfile.println();
            }
            cfile.println("};");

            //
            // Print Statistics
            //
            System.out.println(" The number of slots used is " + num_slots);
            System.out.println(" Total static storage utilization is " +
                               num_slots * SLOT_SIZE + " bytes for encoding plus " +
                               BASE_SIZE * 4 + " bytes for the base");
            System.out.println(" The number of unicode characters is " + num_elements);
            System.out.println(" Total static storage utilization is 65536");
        }
        else
        {
            hfile.println("#ifndef code_INCLUDED");
            hfile.println("#define code_INCLUDED");
            hfile.println();
            hfile.println("#include \"config.h\"");
            hfile.println("#include <ctype.h>");
            hfile.println("#include \"bool.h\"");
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
            hfile.println("#endif");

            cfile.println("#include \"code.h\"");
            cfile.println();
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

        hfile.close();
        cfile.close();
    }
}
