using System;
using System.Collections.Generic;

namespace WHILE_Lexer
{
    public class LexingRules
    {

        public static Seq PLUS(Rexp r)
        {
            return RexpOps.tildeOP(r, RexpOps.percentOP(r));
        }

        public static Rexp Range(List<char> s)
        {
            if ((s.Count == 0))
            {
                return new Zero();
            }
            else if ((s.Count == 1))
            {
                char ch = (char)s[0];
                s.RemoveAt(0);
                return new Char(ch);
            }
            else
            {
                char ch = (char)s[0];
                s.RemoveAt(0);
                return new Alt(new Char(ch), LexingRules.Range(s));
            }

        }

        public static Rexp RANGE(string s)
        {
            List<char> list = new List<char>();
            char[] chars = s.ToCharArray();
            foreach (char ch in chars)
            {
                list.Add(ch);
            }

            return LexingRules.Range(list);
        }

        public static Rexp LETTER()
        {
            return LexingRules.RANGE("ABCDEFGHIJKLMNOPQRSTUVXYZabcdefghijklmnopqrstuvwxyz_");
        }

        public static Rexp SYM()
        {
            return RexpOps.verticalBarOP(LETTER(), RANGE(" .><=;,:"));
        }

        public static Rexp DIGIT()
        {
            return LexingRules.RANGE("0123456789");
        }

        public static Rexp ID()
        {
            return RexpOps.tildeOP(LexingRules.LETTER(), RexpOps.percentOP(RexpOps.verticalBarOP(LexingRules.LETTER(), LexingRules.DIGIT())));
        }

        public static Rexp NUM()
        {
            return LexingRules.PLUS(LexingRules.DIGIT());
        }

        public static Rexp KEYWORD()
        {
            String[] str = new String[] {
                "skip",
                "while",
                "do",
                "if",
                "then",
                "else",
                "read",
                "write",
                "for",
                "to",
                "true",
                "false"};
            Rexp res = StringOps.verticalBarOP(str[0], str[1]);
            for (int i = 2; i < str.Length; i++)
            {
                res = StringOps.verticalBarOP(str[i], res);
            }

            return res;
        }

        public static Rexp SEMI = new Char(';'); //PP not sure

        public static Rexp OP()
        {
            String[] str = new String[] {
                ":=",
                "==",
                "-",
                "+",
                "*",
                "!=",
                "<",
                ">",
                "%",
                "&&",
                "||",
                "<=",
                ">="};
            Rexp res = StringOps.verticalBarOP(str[0], str[1]);
            for (int i = 2; (i < str.Length); i++)
            {
                res = StringOps.verticalBarOP(str[i], res);
            }

            return res;
        }

        public static Rexp WHITESPACE()
        {
            String[] str = new String[] {
                " ",
                "\n",
                "\t",
                "\r"
            };
            Rexp res = StringOps.verticalBarOP(str[0], str[1]);
            for (int i = 2; (i < str.Length); i++)
            {
                res = StringOps.verticalBarOP(str[i], res);
            }

            return LexingRules.PLUS(res);
        }


        public static Rexp RPAREN()
        {
            return StringOps.verticalBarOP("{", "(");
        }

        public static Rexp LPAREN()
        {
            return StringOps.verticalBarOP("}", ")");
        }

        //Check if order is correct
        public static Rexp STRING()
        {
            Rexp res = RexpOps.tildeOP(new Char('\"'), RexpOps.percentOP(LexingRules.LETTER()));
            return RexpOps.tildeOP(res, new Char('\"'));
        }

        public static Rexp COMMENT()
        {
            return RexpOps.tildeOP(RexpOps.powOP(new Char('/'),times: 2), RexpOps.percentOP(RexpOps.verticalBarOP(SYM(), DIGIT())));
        }

        public static Rexp WHILE_REGS()
        {
            Rexp rexp = StringOps.dollarOP("k", LexingRules.KEYWORD());
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("i", LexingRules.ID()));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("o", LexingRules.OP()));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("n", LexingRules.NUM()));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("s", LexingRules.SEMI));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("str", LexingRules.STRING()));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("p", RexpOps.verticalBarOP(LexingRules.LPAREN(), LexingRules.RPAREN())));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("w", LexingRules.WHITESPACE()));
            rexp = RexpOps.verticalBarOP(rexp, StringOps.dollarOP("c", LexingRules.COMMENT()));
            return RexpOps.percentOP(rexp);
        }
    }
}