namespace WHILE_Lexer
{
    public class StringOps
    {

        // operator |
        public static Alt verticalBarOP(string s, Rexp r)
        {
            return new Alt(Lexer.stringToRexp(s), r);
        }

        public static Alt verticalBarOP(string s, string r)
        {
            return new Alt(Lexer.stringToRexp(s), Lexer.stringToRexp(r));
        }

        // operator %
        public static Star percentOP(string s)
        {
            return new Star(Lexer.stringToRexp(s));
        }

        // operator ~
        public static Seq tildeOP(string s, Rexp r)
        {
            return new Seq(Lexer.stringToRexp(s), r);
        }

        public static Seq tildeOP(string s, string r)
        {
            return new Seq(Lexer.stringToRexp(s), Lexer.stringToRexp(r));
        }

        // operator $
        public static Recd dollarOP(string s, Rexp r)
        {
            return new Recd(s, r);
        }
    }
}