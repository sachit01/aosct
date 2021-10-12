/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          UnitConn.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Instance of a connection to a unit.
*                  Contains: Name, IP, Port of the connection.
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
    public class UnitConn
    {
        [XmlElement("Name")]
        public string name { get; set; }
        [XmlElement("Ip")]
        public string ip { get; set; }
        [XmlElement("Port")]
        public string port { get; set; }
        [XmlElement("Graphsetup")]
        public string graphSetup { get; set; }

        public UnitConn()
        {
            name = "Default";
            ip = "";
            port = "";
            graphSetup = "";
        }
        /// <summary>
        /// Creates an UnitConn withgiven name and empty ip- and portfield
        /// </summary>
        /// <param name="_name">Name of the Unit</param>
        public UnitConn(string _name)
        {
            name = _name;
            ip = "";
            port = "";
            graphSetup = "";
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="_name">Name of the Unit</param>
        /// <param name="_ip">Ip-number to the Unit</param>
        /// <param name="_port">Port to the Unit</param>
        public UnitConn(string _name, string _ip, string _port)
        {
            name = _name;
            ip = _ip;
            port = _port;
            graphSetup = "";
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="_name">Name of the Unit</param>
        /// <param name="_ip">Ip-number to the Unit</param>
        /// <param name="_port">Port to the Unit</param>
        public UnitConn(string _name, string _ip, string _port, string _path)
        {
            name = _name;
            ip = _ip;
            port = _port;
            graphSetup = _path;
        }

        override public string ToString()
        {
            return name;
        }

        public XElement SerializeToXML()
        {
            XmlSerializer ser = new XmlSerializer(typeof(UnitConn));
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
