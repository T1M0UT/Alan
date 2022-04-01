using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace WHILE_Lexer
{
    class MainClass
    {
        private static string GetFilePath(string folder, string fileName) => $"../../../../../Examples/{folder}/{fileName}";

        public static void Main()
        {
            //Read file

            Lexer lexer = new Lexer();

            Console.Clear();
            Console.WriteLine("Enter the file name (e.g. fib.txt): ");
            string fileName = Console.ReadLine();

            string[] tag = fileName.Split('.');
            string folder = tag[0];
            try
            {
                IEnumerable<string> lines;
                string inputFilePath = GetFilePath(folder, fileName);
                using (StreamReader inputFile = new StreamReader(inputFilePath))
                {
                    lines = File.ReadLines(inputFilePath);
                }
                foreach (var line in lines)
                {
                    lexer.Tokenizer(line);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error occurred while reading: " + ex.Message);
            }
            finally
            {
                //Write lex tokens to file

                string tokensFileName = tag[0] + "Tokens." + tag[1];
                string tokensFilePath = GetFilePath(folder, tokensFileName);
                using (StreamWriter outputFile = new StreamWriter(tokensFilePath))
                {
                    foreach (Token token in lexer.tokens)
                    {
                        outputFile.WriteLine(Lexer.getTokenByID(token.id) + " : " + token.value);
                    }
                }

                //Write cleared from comments and whitespaces program to file
                string clearedStringFileName = tag[0] + "Cleared." + tag[1];
                string clearedStringFilePath = GetFilePath(folder, clearedStringFileName);

                Lexer clearedLex = new Lexer();
                clearedLex.tokens = lexer.RemoveCommsAndWs();
                using (StreamWriter outputFile = new StreamWriter(clearedStringFilePath))
                {
                    foreach (Token token in clearedLex.tokens)
                    {
                        outputFile.Write(token.value);
                    }
                }
            }


            //Write tokens to console

            lexer.Display();


        }
    }
}