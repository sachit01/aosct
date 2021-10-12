/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          TypeDescr.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2012-08-02 12:47 %
*
*  DESCRIPTION:    Instance of an Type/Measurable. 
*                  Contains: Name, Description, Unit, Min and Max for the Type.
*                  Each type is a choosable curve in the program.
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
    public class TypeDescr
    {
        [XmlElement("Name")]
        public string Name { get; set; }

        [XmlElement("Description")]
        public string Description { get; set; }

        [XmlElement("Type")]
        public string Type { get; set; }

        [XmlElement("Unit")]
        public string Unit { get; set; }

        [XmlElement("Min")]
        public string Min { get; set; }

        [XmlElement("Max")]
        public string Max { get; set; }

         //  "Speed";"Current speed";WORD;"cm/s";0;1000
         //  "Acceleration";"Current acceleration";SWORD;"cm/s^2";0;1000
         //  "ATPMode";"ATPMode";DWORD;"mode";0;13
         //  "CeilingSpeed";"Current ceiling speed";WORD;"cm/s";0;1000
         //  "DistToTarget";"Distance to current target";SDWORD;"cm";-900000;900000
         //  "ATPState";"ATPState";DWORD;"state";0;10

        private object limitMin { get; set; }
        private object limitMax { get; set; }

        public TypeDescr()
        {
            Name = null;
            Description = null;
            Type = null;
            Unit = null;
            Min = null;
            Max = null;
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="_Name">Name of this type.</param>
        /// <param name="_Description">Description of this type.</param>
        /// <param name="_Type">Data type.</param>
        /// <param name="_Unit">Unit of this type.</param>
        /// <param name="_Min"></param>
        /// <param name="_Max"></param>
        public TypeDescr(string _Name, string _Description, string _Type, string _Unit, string _Min, string _Max)
        {
            Name = _Name.Replace("\"", "");
            Description = _Description.Replace("\"", "");
            Type = _Type.Replace("\"", "");
            Unit = _Unit.Replace("\"", "");
            Min = _Min.Replace("\"", "");
            Max = _Max.Replace("\"", "");
        }
        /// <summary>
        /// Creates an object of class from a string containing description of data type from AOS.
        /// </summary>
        /// <param name="_line"></param>
        public TypeDescr(string _line)
        {
            parseString(_line);
        }

        private void parseArray(string[] array)
        {
            Name = array[0].Replace("\"", "");
            Description = array[1].Replace("\"", "");
            Type = array[2].Replace("\"", "");
            Unit = array[3].Replace("\"", "");
            Min = array[4].Replace("\"", "");
            Max = array[5].Replace("\"", "");
        }

        /// <summary>
        /// Parses a line containing a type description to the object.
        /// </summary>
        /// <param name="line">String to convert. </param>
        public void parseString(string line)
        {
            string[] values = line.Split(new char[] {';'});
            parseArray(values);
        }

        //
        // Returns:
        //  1 if t1 > t2 
        //  0 if t1 = t2
        // -1 if t1 < t2
        //
        public static int Compare(TypeDescr t1, TypeDescr t2)
        {
            if (t1 == null)
            {
                if (t2 == null)
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
                if (t2 == null)
                {
                    return 1;
                }
                else
                {
                    return t1.Name.CompareTo(t2.Name);
                }
            }
        }

        /// <summary>
        /// Serializes this object to an XElement.
        /// </summary>
        /// <returns>XElement of this object.</returns>
        public XElement SerializeToXML()
        {
            XmlSerializer ser = new XmlSerializer(typeof(TypeDescr));
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
