using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace WHILE_Lexer
{
    class MainClass
    {
        //private static string GetFilePath(string folder, string fileName) => $"/Users/tymur/Desktop/Alan/ALAN/Compiled/Examples/{folder}/{fileName}";

        public static void Main(String[] args)
        {
            //Read file

            Lexer lexer = new Lexer();

            //Console.Clear();
            //onsole.WriteLine("Enter the file name (e.g. fib.txt): ");
            //string fileName = Console.ReadLine();
            if (args.Length != 1)
            {
                // Print usage
                Console.WriteLine("Usage: WHILE_Lexer <path_filename>");
                return;
            }

            var path_filename = args[0].Split('/');
            string path = String.Join('/', path_filename[0..^1]);
            //Console.WriteLine(path);
            string fileName = path_filename.Last();
            
            Console.WriteLine("Lexing...");
            
            string[] tag = fileName.Split('.');
            string folder = tag[0];
            try
            {
                IEnumerable<string> lines;
                string inputFilePath = $"{path}/{fileName}";
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
                // string tokensFilePath = GetFilePath(folder, tokensFileName);
                string tokensFilePath = $"{path}/{tokensFileName}";
                using (StreamWriter outputFile = new StreamWriter(tokensFilePath))
                {
                    foreach (Token token in lexer.tokens)
                    {
                        outputFile.WriteLine(Lexer.getTokenByID(token.id) + " : " + token.value);
                    }
                }

                //Write cleared from comments and whitespaces program to file
                //string clearedStringFileName = tag[0] + "Cleared." + tag[1];
                //Console.WriteLine(tag[0] + ", " + tag[1]);
                string clearedStringFileName = tag[0] + "." + tag[1] + ".cleared";
                string clearedStringFilePath = $"{path}/{clearedStringFileName}";

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

            //lexer.Display();
        }
    }
}