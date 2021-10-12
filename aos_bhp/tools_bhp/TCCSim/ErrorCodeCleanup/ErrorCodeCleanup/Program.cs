using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using System.IO;

namespace ErrorCodeCleanup
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                usage();
            }
            if (!File.Exists(args[0]))
            {
                usage();
            }
            clean(args[0], args[1]);
        }
        private static void clean(string in_file, string out_file)
        {
            XElement xdoc = XElement.Load(in_file);
            XElement clean = new XElement("Root"); ;
            foreach (XElement x in xdoc.Descendants("Row"))
            {
                XElement tx = new XElement("Row");
                tx.Add(x.Element("ST_Error_Code"));
                tx.Add(x.Element("Description"));
                clean.Add(tx);
            }
            clean.Save(out_file);
        }
        private static void usage()
        {
            Console.WriteLine(System.AppDomain.CurrentDomain.FriendlyName + " [in file] [out file]");
            Environment.Exit(-1);
        }
    }
}
