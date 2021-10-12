/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          Program.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:42 %
*
*  DESCRIPTION:    The main class that runs on start. It just runs AOSAnalyzer. 
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2011-07-26    Blomqvist   File created
*
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.IO;
using System.Xml.Serialization;
using System.Xml.Linq;
using System.Xml;

namespace AOSAnalyzer
{

    static class Program
    {

        [STAThread]
        static void Main(string[] args)
        {
            #region statictest

            /* Serialize test. */
            {
                //XmlSerializer ser = new XmlSerializer(typeof(List<TypeDescr>));
                //XDocument doc = new XDocument();
                //using (XmlWriter xw = doc.CreateWriter())
                //{
                //    ser.Serialize(xw, types);
                //    xw.Close();
                //}
                //Console.WriteLine(doc.Root);
            }

            /* Serialize test. */
            //{
            //    XmlSerializer ser = new XmlSerializer(typeof(List<Entry>));
            //    XDocument doc = new XDocument();
            //    using (XmlWriter xw = doc.CreateWriter())
            //    {
            //        ser.Serialize(xw, entries);
            //        xw.Close();
            //    }
            //    Console.WriteLine(doc.Root);
            //}
            /* Serialize test end. */
            #endregion 
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new AOSAnalyzer(args));
        }
    }
}
