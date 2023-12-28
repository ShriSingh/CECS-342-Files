// Importing the libararies
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;

namespace Export
{
    class Program
    {

        // Recursively enumerates all files in a given folder and its subfolders
        static IEnumerable<string> EnumerateFilesRecursively(string path)
        {
            var result = new List<string>();

            // Get all files in the current folder
            result.AddRange(Directory.GetFiles(path));

            // Recursively get files from each subfolder
            foreach (var directory in Directory.GetDirectories(path))
            {
                result.AddRange(EnumerateFilesRecursively(directory));
            }

            // Outputs the collected number of files
            return result;
        }
        
        // Calculates the size of the files enumerated
        static string FormatByteSize(long byteSize)
        {
            //1kB = 1000B
            //B, kB, MB, GB, TB, PB, EB, and ZB
            //numerical value should be greater or equal to 0, less than 1000, and rounded to 2 digits after the decimal point, e.g. "1.30kB"
            //sources used: mostly w3 schools for c# stuff
            if (byteSize < 1000) {
                return $"{byteSize}B";
            }
            if (byteSize < 1000000)
            {
              decimal byteSizeNew = byteSize/10;
              return $"{byteSizeNew/100}kB";
            }
            if (byteSize < 1000000000)
            {
              decimal byteSizeNew = byteSize/10000;
              return $"{byteSizeNew/100}MB";
            }
            if (byteSize < 1000000000000)
            {
              decimal byteSizeNew = byteSize/10000000;
              return $"{byteSizeNew/100}GB";
            }
            if (byteSize < 1000000000000000)
            {
              decimal byteSizeNew = byteSize/10000000000;
              return $"{byteSizeNew/100}TB";
            }
            if (byteSize < 1000000000000000000)
            {
              decimal byteSizeNew = byteSize/10000000000000;
              return $"{byteSizeNew/100}PB";
            }
            byteSize /= 1000;
            if ((byteSize) < 1000000000000000000)
            {
              decimal byteSizeNew = byteSize/10000000000000;
              return $"{byteSizeNew/100}EB";
            }
            byteSize  /= 1000;
            if ((byteSize) < 1000000000000000000)
            {
              decimal byteSizeNew = byteSize/10000000000000;
              return $"{byteSizeNew/100}ZB";
            }
            //i'm not sure if a long can even reach this level to be quite honest
            //from what i can tell the highest a long can go is 9.22 PB
            return "0B";
        } 

        // Creates an HTML/XML document to state the type, count, and the size of the files processed
        static XDocument CreateReport(IEnumerable<string> files)
        {
            //getting the info from each file in the path
            IEnumerable<FileInfo> fileInfos = files.Select(filePath => new FileInfo(filePath));

            //query to group and order files by type
            var queryResult = from fileI in fileInfos
                group fileI by fileI.Extension.ToLower() into fileGroup
                let totalSize = fileGroup.Sum(fileI => fileI.Length)
                orderby totalSize descending
                //selecting type, count and size
                    select new
                    {
                        Type = fileGroup.Key,
                        Count = fileGroup.Count(),
                        Size = FormatByteSize(totalSize)
                    };
                    
            //using xelement to create the table, setting the titles
            XElement table = new XElement("table",
                new XElement("tr",
                new XElement("th", "Type"),
                new XElement("th", "Count"),
                new XElement("th", "Size")
            )
            );

            //running thru the query to print it out into each row
            foreach (var fileInfo in queryResult)
            {
                XElement row = new XElement("tr",
                    new XElement("td", fileInfo.Type),
                    new XElement("td", fileInfo.Count),
                    new XElement("td", fileInfo.Size)
                );

            table.Add(row);
            }

            //putting it all together and finishing up the html
            XDocument document = new XDocument(
                new XElement("html",
                    new XElement("head"),
                    new XElement("style", new XAttribute("type", "text/css"), " th, td {border: 1px solid black;}"),
                    new XElement("body", table)
                )
            );

            return document;
        }      


         // Entry point of the program
        static void Main(string[] args)
        {
            // Check if the correct number of command-line arguments is provided
            if (args.Length != 2)
            {
                Console.WriteLine("You must run with arguments [inputFolder] [outputFile]");
                return;
            }

            // Extract input and output paths from command-line arguments
            string inputFolderPath = args[0];
            string outputFilePath = args[1];

            // Recursively get all files in the specified input folder
            IEnumerable<string> files = EnumerateFilesRecursively(inputFolderPath);

            // Create an HTML report and save it to the specified output file
            XDocument report = CreateReport(files);
            report.Save(outputFilePath);
            Console.WriteLine($"Report generated and saved to: {outputFilePath}");
        }
    }
}
