using System;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Transform
{
    public class Decoder
    {
        const string Part3MagicLine = "000000*£TC-PART3££££££££££££££££££££££££££££££££££££££££££££££££££££££££";
        const string Part4MagicLine = "000000*£TC-PART4££££££££££££££££££££££££££££££££££££££££££££££££££££££££";
        const int LineLength = 66;
        const int CommentPos = 6;

        /// <summary>
        /// 
        /// </summary>
        /// <param name="typeCobolFilePath"></param>
        /// <param name="cobol85FilePath"></param>
        /// <param name="outputFilePath"></param>
        /// <returns>true if the conactenation was successful, false otherwise</returns>
	    public static bool concatenateFiles(string typeCobolFilePath, string cobol85FilePath, string outputFilePath)
        {
            string[] typeCobolLines = File.ReadAllLines(typeCobolFilePath);
            string[] cobol85Lines = File.ReadAllLines(cobol85FilePath);
            Stream outputStream = File.OpenWrite(outputFilePath);
            var outputWriter = new StreamWriter(outputStream);
            try
            {
                int part2Start = 2;
                int part3Start = part2Start + cobol85Lines.Length;
                int part4Start = part3Start + typeCobolLines.Length;
                //string firstLine = string.Format("000000*£TC-PART1£PART2-{0:000000}£PART3-{1:000000}£PART4-{2:000000}£££££££££££££££££", 
                //                part2Start, part3Start, part4Start);
                //outputWriter.WriteLine(firstLine);
                outputWriter.WriteLine("000000*£TC-PART1£PART2-{0:000000}£PART3-{1:000000}£PART4-{2:000000}£££££££££££££££££",
                                part2Start, part3Start, part4Start);
                //Part 2 - Cobol 85 generated code
                outputWriter.WriteLine("000000*£TC-PART2££££££££££££££££££££££££££££££££££££££££££££££££££££££££");
                foreach (var cobol85Line in cobol85Lines)
                {
                    outputWriter.WriteLine(cobol85Line);
                }

                //Part 3 - TypeCobol without 7th column
                outputWriter.WriteLine(Part3MagicLine);
                System.Text.StringBuilder columns7 = new System.Text.StringBuilder(part4Start - part3Start);
                foreach (var typeCobolLine in typeCobolLines)
                {
                    if (typeCobolLine.Length >= CommentPos)
                    {
                        //TODO Check the length >= 8
                        try
                        {
                            if (typeCobolLine.Length > 7)                            
                                outputWriter.WriteLine("000000*" + typeCobolLine.Substring(7));
                            else
                                outputWriter.WriteLine("000000*");
                            columns7.Append(typeCobolLine[CommentPos]);
                        }
                        catch(Exception e)
                        {
                            System.Console.WriteLine(e.Message);
                            return false;
                        }
                    }
                    else
                    {
                        outputWriter.WriteLine("000000*");
                        columns7.Append(' ');
                    }
                }

                //Part 4 - 7th column of the TypeCobol part 3
                outputWriter.WriteLine(Part4MagicLine);
                String s_columns7 = columns7.ToString();                
                int c7Length = (LineLength - 1);
                int nSplit = (s_columns7.Length / c7Length) + ((s_columns7.Length % c7Length) == 0 ? 0 : 1);
                for (int i = 0, sPos = 0; i < nSplit; i++, sPos += (LineLength-1))
                {
                    outputWriter.Write("000000*");
                    outputWriter.WriteLine(s_columns7.Substring(sPos, Math.Min(c7Length, s_columns7.Length - sPos)));
                }
            }
            finally
            {
                outputWriter.Flush();
                outputStream.Close();
            }
            return true;
        }


        /// <summary>
        /// 
        /// </summary>
        /// <param name="typeCobolFilePath"></param>
        /// <param name="cobol85FilePath"></param>
        /// <param name="outputFilePath"></param>
        /// <returns>true if the decoding was successful, false otherwise</returns>
	    public static bool decode(string concatenatedFilePath, string typeCobolOutputFilePath)
        {
            Stream inputStream = File.OpenRead(concatenatedFilePath);
            StreamReader inputReader = new StreamReader(inputStream);
            Stream outputStream = File.OpenWrite(typeCobolOutputFilePath);
            var outputWriter = new StreamWriter(outputStream);
            try
            {
                string firstFline = inputReader.ReadLine();
                if (firstFline != null)
                {
                    String s_part3Len = firstFline.Substring(36, 6);
                    int part3Start = Convert.ToInt32(s_part3Len);
                    String s_part4Len = firstFline.Substring(49, 6);
                    int part4Start = Convert.ToInt32(s_part4Len);
                    for (int i = 0; i < (part3Start - 1); i++)
                    {
                        inputReader.ReadLine();
                    }

                    ////Read Part 3
                    // Check if magic line is present
                    string part3MagicLineRead = inputReader.ReadLine();
                    if (!Part3MagicLine.Equals(part3MagicLineRead))
                    {
                        Console.WriteLine("Magic line of part 3 not present: " + part3MagicLineRead);
                        //TODO
                        return false;
                    }
                    else
                    {
                        int part3Length = part4Start - part3Start;
                        IList<string> tcLines = new List<string>(part3Length);
                        System.Text.StringBuilder tcLinesCol7 = new System.Text.StringBuilder(part3Length + LineLength);
                        for (int i = 0; i < part3Length; i++)
                        {
                            string tcLineRead = inputReader.ReadLine();
                            if (tcLineRead.Length >= 7)
                            {
                                tcLines.Add(tcLineRead.Substring(7 /*, Math.Min(LineLength, rightLen)*/));                                
                            }
                            else
                            {
                                tcLines.Add("");
                            }
                        }

                        // Read Part 4
                        // Check if magic line is present
                        string part4MagicLineRead = inputReader.ReadLine();
                        if (Part4MagicLine.Equals(part4MagicLineRead))
                        {
                            string currLine = null;
                            while((currLine = inputReader.ReadLine()) != null)
                            {
                                String s = currLine.Substring(7).PadRight(LineLength-1);
                                tcLinesCol7.Append(s);
                            }
                        }
                        else
                        {
                            Console.WriteLine("Magic line of part 4 not present: " + part4MagicLineRead);
                            return false;
                        }

                        for (var i = 0; i < part3Length; i++)
                        {
                            if (i != (part3Length-1))
                                outputWriter.WriteLine(tcLinesCol7[i] + tcLines[i]);
                            else
                                outputWriter.Write(tcLinesCol7[i] + tcLines[i]);
                        }
                    }
                }
                return true;
            }
            finally
            {
                outputWriter.Flush();
                outputWriter.Close();
                inputStream.Close();                
            }
        }
    }
}


