/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          UnitDescr.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2012-08-02 12:47 %
*
*  DESCRIPTION:    Instance of an Unit. 
*                  Contains: Name, Version and Protocol version of the unit.
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
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;

namespace AOSAnalyzer
{
    public class UnitDescr
    {
        [XmlElement("Name")]
        public string Name { get; set; }
        [XmlElement("Version")]
        public string Version { get; set; }
        [XmlElement("Protocol")]
        public string Protocol { get; set; }

        public UnitDescr()
        {
            Name = "";
            Version = "";
            Protocol = "";
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="_Name">Name of this unit</param>
        /// <param name="_Version">Version of this unit</param>
        /// <param name="_Protocol">Protocolversion of this unit</param>
        public UnitDescr(string _Name, string _Version, string _Protocol)
        {
            Name = _Name;
            Version = _Version;
            Protocol = _Protocol;
        }
        /// <summary>
        /// Creates an object of class from a string containing description of data type from AOS.
        /// </summary>
        /// <param name="_line"></param>
        public UnitDescr(string _line)
        {
            ParseString(_line);
        }

        public static int Compare(UnitDescr u1, UnitDescr u2)
        {
            if (u1 == null)
            {
                if (u2 == null)
                {
                    return 0;
                }
                else
                {
                    return -1;
                }
            }
            else
            {
                if (u2 == null)
                {
                    return 1;
                }
                else
                {
                    return u1.Name.CompareTo(u2.Name);
                }
            }
        }
        
        private void ParseArray(string[] array)
        {
            switch (array[0])
            {
                case "Name":
                    Name = array[1];
                    break;
                case "Version":
                    Version = array[1];
                    break;
                case "ProtocolVer":
                    Protocol = array[1];
                    break;
                default:
                    break;
            }
        }
        /// <summary>
        /// Parses a line containing a type description to the object.
        /// </summary>
        /// <param name="line">String to parse</param>
        public void ParseString(string line)
        {
            string[] values = line.Split(new char[] { ' ' });
            ParseArray(values);
        }

        public XElement SerializeToXML()
        {
            XmlSerializer ser = new XmlSerializer(typeof(UnitDescr));
            XDocument doc = new XDocument();
            using (XmlWriter xw = doc.CreateWriter())
            {
                ser.Serialize(xw, this);
                xw.Close();
            }
            return doc.Root;
        }
    }
}
