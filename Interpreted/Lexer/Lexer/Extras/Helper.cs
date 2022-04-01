using System;
using System.Collections.Generic;
using System.Linq;

namespace WHILE_Lexer
{
    public static class Helper
    {
        public static void Deconstruct<T>(this List<T> list, out T head, out List<T> tail)
        {
            head = list.FirstOrDefault();
            tail = new List<T>(list.Skip(1));
        }

        public static List<Token> PairsToTokens(this List<Pair<string, string>> pairs)
        {
            List<Token> tokens = new List<Token>();
            pairs.ForEach(x => tokens.Add(x));
            return tokens;
        }
    }
}
