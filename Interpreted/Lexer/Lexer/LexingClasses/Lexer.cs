using System;
using System.Collections.Generic;
using System.Linq;

namespace WHILE_Lexer
{
    public class Lexer
    {
        public static int pos = 0;
        public List<Token> tokens;

        public Lexer(List<Token> tokens)
        {
            this.tokens = tokens;
        }

        public Lexer()
        {
            tokens = new List<Token>();
        }

        public Lexer(List<Pair<string, string>> pairs)
        {
            tokens = new List<Token>();
            pairs.ForEach(x => tokens.Add(x));
        }

        public static Rexp charListToRexp(List<char> s)
        {
            if ((s.Count == 0))
            {
                return new One();
            }
            else if ((s.Count == 1))
            {
                char c = s[0];
                s.RemoveAt(0);
                return new Char(c);
            }

            char ch = s[0];
            s.RemoveAt(0);
            return new Seq(new Char(ch), Lexer.charListToRexp(s));
        }

        public List<Token> RemoveCommsAndWs()
        {
            List<Token> newTokens = new List<Token>();
            foreach (var token in tokens)
            {
                if (token.id != "c" && token.id != "w")
                {
                    newTokens.Add(token);
                }
            }
            return newTokens;
        }

        public void Display()
        {
            foreach (var token in tokens)
            {
                Console.WriteLine(getTokenByID(token.id) + " : " + token.value);
            }
        }

        public static string getTokenByID(string id)
        {
            switch (id)
            {
                case "k":
                    return "Keyword";
                case "i":
                    return "Id";
                case "o":
                    return "Operation";
                case "n":
                    return "Number";
                case "s":
                    return "Semicolon";
                case "str":
                    return "String";
                case "p":
                    return "Paren";
                case "w":
                    return "Whitespace";
                case "c":
                    return "Comment";
                default:
                    return "";
            }
        }

        public static Rexp stringToRexp(string s)
        {
            List<char> al = new List<char>();
            char[] chars = s.ToCharArray();
            foreach (char ch in chars)
            {
                al.Add(ch);
            }

            return Lexer.charListToRexp(al);
        }

        public static bool isNullable(Rexp r)
        {
            if ((r is Zero))
            {
                return false;
            }
            else if ((r is One))
            {
                return true;
            }
            else if ((r is Char))
            {
                return false;
            }
            else if ((r is Alt))
            {
                return (Lexer.isNullable(((Alt)(r)).r1) || Lexer.isNullable(((Alt)(r)).r2));
            }
            else if ((r is Seq))
            {
                return (Lexer.isNullable(((Seq)(r)).r1) && Lexer.isNullable(((Seq)(r)).r2));
            }
            else if ((r is Star))
            {
                return true;
            }
            else if ((r is Recd))
            {
                return Lexer.isNullable(((Recd)(r)).r);
            }
            else
            {
                return false;
            }

        }

        public static Rexp der(char c, Rexp r)
        {
            if ((r is Zero))
            {
                return new Zero();
            }
            else if ((r is One))
            {
                return new Zero();
            }
            else if ((r is Char))
            {
                if ((c == ((Char)(r)).c))
                {
                    return new One();
                }
                else
                {
                    return new Zero();
                }

            }
            else if ((r is Alt))
            {
                return new Alt(Lexer.der(c, ((Alt)(r)).r1), Lexer.der(c, ((Alt)(r)).r2));
            }
            else if ((r is Seq))
            {
                if (Lexer.isNullable(((Seq)(r)).r1))
                {
                    return new Alt(new Seq(Lexer.der(c, ((Seq)(r)).r1), ((Seq)(r)).r2), Lexer.der(c, ((Seq)(r)).r2));
                }
                else
                {
                    return new Seq(Lexer.der(c, ((Seq)(r)).r1), ((Seq)(r)).r2);
                }

            }
            else if ((r is Star))
            {
                return new Seq(Lexer.der(c, ((Star)(r)).r), new Star(((Star)(r)).r));
            }
            else
            {
                return Lexer.der(c, ((Recd)(r)).r);
            }

        }

        public static Rexp ders(Rexp r, List<char> list)
        {
            if ((list.Count == 0))
            {
                return r;
            }

            char ch = list[0];
            list.RemoveAt(0);
            return Lexer.ders(Lexer.der(ch, r), list);
        }

        public static string flatten(Val v)
        {
            if ((v is Empty))
            {
                return "";
            }
            else if ((v is Chr))
            {
                return char.ToString(((Chr)(v)).c);
            }
            else if ((v is Left))
            {
                return Lexer.flatten(((Left)(v)).v);
            }
            else if ((v is Right))
            {
                return Lexer.flatten(((Right)(v)).v);
            }
            else if ((v is Sequ))
            {
                return (Lexer.flatten(((Sequ)(v)).v1) + Lexer.flatten(((Sequ)(v)).v2));
            }
            else if ((v is Stars))
            {
                List<Val> inputList = new List<Val>();
                inputList.AddRange(((Stars)(v)).vs);
                string str = "";
                foreach (Val obj in inputList)
                {
                    str = (str + Lexer.flatten(obj));
                }

                return str;
            }
            else if ((v is Rec))
            {
                return Lexer.flatten(((Rec)(v)).v);
            }
            else
            {
                return "";
            }

        }

        public static List<Pair<string, string>> env(Val v)
        {
            if ((v is Empty))
            {
                return new List<Pair<string, string>>();
            }
            else if ((v is Chr))
            {
                return new List<Pair<string, string>>();
            }
            else if ((v is Left))
            {
                return Lexer.env(((Left)(v)).v);
            }
            else if ((v is Right))
            {
                return Lexer.env(((Right)(v)).v);
            }
            else if ((v is Sequ))
            {
                List<Pair<string, string>> list1 = Lexer.env(((Sequ)(v)).v1);
                List<Pair<string, string>> list2 = Lexer.env(((Sequ)(v)).v2);
                List<Pair<string, string>> res = new List<Pair<string, string>>();
                res.AddRange(list1);
                res.AddRange(list2);
                return res;
            }
            else if ((v is Stars))
            {
                List<Val> inp = ((Stars)(v)).vs;
                List<Pair<string, string>> res = new List<Pair<string, string>>();
                foreach (Val obj in inp)
                {
                    res.AddRange(Lexer.env(obj));
                }

                return res;
            }
            else if ((v is Rec))
            {
                List<Pair<string, string>> list = new List<Pair<string, string>>();
                list.AddRange(Lexer.env(((Rec)(v)).v));
                list.Insert(0, new Pair<string, string>(((Rec)(v)).x, Lexer.flatten(((Rec)(v)).v)));
                return list;
            }
            else
            {
                return new List<Pair<string, string>>();
            }

        }

        public static Val mkeps(Rexp r)
        {
            if ((r is One))
            {
                return new Empty();
            }
            else if ((r is Alt))
            {
                if (Lexer.isNullable(((Alt)(r)).r1))
                {
                    return new Left(Lexer.mkeps(((Alt)(r)).r1));
                }
                else
                {
                    return new Right(Lexer.mkeps(((Alt)(r)).r2));
                }

            }
            else if ((r is Seq))
            {
                return new Sequ(Lexer.mkeps(((Seq)(r)).r1), Lexer.mkeps(((Seq)(r)).r2));
            }
            else if ((r is Star))
            {
                return new Stars(new List<Val>());
            }
            else if ((r is Recd))
            {
                return new Rec(((Recd)(r)).x, Lexer.mkeps(((Recd)(r)).r));
            }
            else
            {
                return new Empty();
            }

        }

        public static Val inj(Rexp r, char c, Val v)
        {
            if (((r is Star) && (v is Sequ)))
            {
                if (((Sequ)(v)).v2 is Stars)
                {
                    List<Val> list = new List<Val>();
                    list.Add(Lexer.inj(((Star)(r)).r, c, ((Sequ)(v)).v1));
                    list.AddRange(((Stars)(((Sequ)(v)).v2)).vs);
                    return new Stars(list);
                }

            }

            if ((r is Seq) && (v is Sequ))
            {
                return new Sequ(Lexer.inj(((Seq)(r)).r1, c, ((Sequ)(v)).v1), ((Sequ)(v)).v2);
            }

            if ((r is Seq) && (v is Left))
            {
                if ((((Left)(v)).v is Sequ))
                {
                    return new Sequ(Lexer.inj(((Seq)(r)).r1, c, ((Sequ)(((Left)(v)).v)).v1), ((Sequ)(((Left)(v)).v)).v2);
                }

            }

            if ((r is Seq) && (v is Right))
            {
                return new Sequ(Lexer.mkeps(((Seq)(r)).r1), Lexer.inj(((Seq)(r)).r2, c, ((Right)(v)).v));
            }

            if ((r is Alt) && (v is Left))
            {
                return new Left(Lexer.inj(((Alt)(r)).r1, c, ((Left)(v)).v));
            }

            if ((r is Alt) && (v is Right))
            {
                return new Right(Lexer.inj(((Alt)(r)).r2, c, ((Right)(v)).v));
            }

            if ((r is Char) && (v is Empty))
            {
                return new Chr(c);
            }

            if ((r is Recd))
            {
                return new Rec(((Recd)(r)).x, Lexer.inj(((Recd)(r)).r, c, v));
            }

            return null;
        }

        public static Pair<Rexp, Func<Val, Val>> simp(Rexp r)
        {
            if ((r is Alt))
            {
                Pair<Rexp, Func<Val, Val>> pair1 = Lexer.simp(((Alt)(r)).r1);
                Pair<Rexp, Func<Val, Val>> pair2 = Lexer.simp(((Alt)(r)).r2);
                if (pair1.first is Zero)
                {
                    return new Pair<Rexp, Func<Val, Val>>(pair2.first, Rectification.F_RIGHT(pair2.second));
                }
                else if (pair2.first is Zero)
                {
                    return new Pair<Rexp, Func<Val, Val>>(pair1.first, Rectification.F_LEFT(pair1.second));
                }
                else
                {
                    //may throw exception in an unknown case
                    return new Pair<Rexp, Func<Val, Val>>(new Alt(pair1.first, pair2.first), Rectification.F_ALT(pair1.second, pair2.second));
                }

            }
            else if (r is Seq)
            {
                Pair<Rexp, Func<Val, Val>> pair1 = Lexer.simp(((Seq)(r)).r1);
                Pair<Rexp, Func<Val, Val>> pair2 = Lexer.simp(((Seq)(r)).r2);
                if (pair1.first is Zero)
                {
                    return new Pair<Rexp, Func<Val, Val>>(new Zero(), Rectification.F_ERROR());
                }
                else if (pair2.first is Zero)
                {
                    return new Pair<Rexp, Func<Val, Val>>(new Zero(), Rectification.F_ERROR());
                }
                else if (pair1.first is One)
                {
                    return new Pair<Rexp, Func<Val, Val>>(pair2.first, Rectification.F_SEQ_EMPTY1(pair1.second, pair2.second));
                }
                else if (pair2.first is One)
                {
                    return new Pair<Rexp, Func<Val, Val>>(pair1.first, Rectification.F_SEQ_EMPTY2(pair1.second, pair2.second));
                }
                else
                {
                    return new Pair<Rexp, Func<Val, Val>>(new Seq(pair1.first, pair2.first), Rectification.F_SEQ(pair1.second, pair2.second));
                }

            }
            else
            {
                return new Pair<Rexp, Func<Val, Val>>(r, Rectification.F_ID);
            }

        }

        public static Val lex_simp(Rexp r, List<char> s)
        {
            if ((s.Count == 0))
            {
                if (Lexer.isNullable(r))
                {
                    return Lexer.mkeps(r);
                }
                else
                {
                    throw new Exception($"lexing error occured in '{r}' type");
                }
            }
            else
            {
                char c = (char)s[0];
                s.RemoveAt(0);
                return inj(r, c, Lexer.lex_simp(Lexer.der(c, r), s));
            }

        }

        public static List<Pair<string, string>> lexing_simp(Rexp r, string s)
        {
            List<char> list = new List<char>();
            list.AddRange(s);

            return Lexer.env(Lexer.lex_simp(r, list));
        }

        public void Tokenizer(string line)
        {
            List<Pair<string, string>> pairs = Lexer.lexing_simp(LexingRules.WHILE_REGS(), line);
            this.tokens.AddRange(pairs.PairsToTokens());
        }
    }

}
