/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AOSConnections.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:42 %
*
*  DESCRIPTION:    Used to keep track of and serialize the unitconnecions.
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
using System.IO;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;

namespace AOSAnalyzer
{
    //
    // A class to save unitconnections
    //
    public class AOSConnections
    {
        [XmlElement("Unitconnections")]
        public List<UnitConn> unitconnections { get; set; }

        public AOSConnections()
        {
            unitconnections = new List<UnitConn>();
        }

        // Checks if there is a unit with the same name in the list.
        public bool Exists(string name)
        {
            foreach (UnitConn u in unitconnections)
            {
                if (String.Compare(u.name.ToLower(), name.ToLower()) == 0)
                    return true;
                return false;
            }
            return false;
        }

        // Serialize this object
        public XElement Serialize()
        {
            XmlSerializer ser;
            try
            {
                ser = new XmlSerializer(typeof(AOSConnections));
            }
            catch (Exception)
            {
                return null;
            }

            XDocument doc = new XDocument();
            using (XmlWriter xw = doc.CreateWriter())
            {
                ser.Serialize(xw, this);
                xw.Close();
            }
            return doc.Root;
        }

        // Deserialize
        public static AOSConnections ReadConnections(string path)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(AOSConnections));
            TextReader textReader = new StreamReader(path);

            AOSConnections c = (AOSConnections)deserializer.Deserialize(textReader);
            textReader.Close();

            return c;
        }
    }
}
