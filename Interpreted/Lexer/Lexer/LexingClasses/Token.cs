using System;
using System.Collections.Generic;

namespace WHILE_Lexer
{
    public class Token
    {
        public string id { get; private set; }
        public string value { get; private set; }

        public Token(Pair<String,String> token)
        {
            id = token.first;
            value = token.second;
        }

        public Token(string id, string value)
        {
            this.id = id;
            this.value = value;
        }
        public static implicit operator Token(Pair<string, string> pair)
        {
            return new Token(pair);
        }
    }
}
