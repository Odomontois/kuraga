def compressedString(word: String): String =
    def go(i: Int, k: Int, chr: Char, b: StringBuilder): String =
        inline def push() = b += (k + '0').toChar += chr
        i match
            case i if i == word.length         => push().result()
            case word(c) if c != chr || k == 9 => go(i + 1, 1, c, push())
            case _                             => go(i + 1, k + 1, chr, b)
    go(1, 1, word(0), StringBuilder(word.length))

compressedString("abcdef")
compressedString("aaaaaaaaaaaaaabb")
