/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AOSSettings.cs %
*
*  %version:       4 %
*
*  %created_by:    lantback %
*
*  %date_created:  2012-08-02 12:44 %
*
*  DESCRIPTION:    Contains and serializes some settings for the application.
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
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;

namespace AOSAnalyzer
{
    public class AOSSettings
    {
        [XmlElement("Latest connected unit")]
        public static String latestConnected;
        [XmlElement("Path to connections")]
        public static String ConnectionsPath;
        [XmlElement("ColorArray")]
        public static Color[] ColorArray = new Color[] {
            Color.Red,
            Color.Blue,
            Color.Green,
            Color.Pink,
            Color.Black,
            Color.Aquamarine,
            Color.Olive
        };

        public AOSSettings()
        {
            latestConnected = "";
            ConnectionsPath = "connections.xml";
        }

        public XElement Serialize()
        {
            XmlSerializer ser;
            try
            {
                ser = new XmlSerializer(typeof(AOSSettings));
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

        public static AOSSettings ReadConfig(string path)
        {
            XmlSerializer deserializer = new XmlSerializer(typeof(AOSSettings));
            TextReader textReader = new StreamReader(path);

            AOSSettings s = (AOSSettings)deserializer.Deserialize(textReader);
            textReader.Close();

            return s;
        }
    }

    [XmlRoot("dictionary")]
    public class SerializableDictionary<TKey, TValue>
        : Dictionary<TKey, TValue>, IXmlSerializable
    {
        #region IXmlSerializable Members
        public System.Xml.Schema.XmlSchema GetSchema()
        {
            return null;
        }

        public void ReadXml(System.Xml.XmlReader reader)
        {
            XmlSerializer keySerializer = new XmlSerializer(typeof(TKey));
            XmlSerializer valueSerializer = new XmlSerializer(typeof(TValue));

            bool wasEmpty = reader.IsEmptyElement;
            reader.Read();

            if (wasEmpty)
                return;

            while (reader.NodeType != System.Xml.XmlNodeType.EndElement)
            {
                reader.ReadStartElement("item");

                reader.ReadStartElement("key");
                TKey key = (TKey)keySerializer.Deserialize(reader);
                reader.ReadEndElement();

                reader.ReadStartElement("value");
                TValue value = (TValue)valueSerializer.Deserialize(reader);
                reader.ReadEndElement();

                this.Add(key, value);

                reader.ReadEndElement();
                reader.MoveToContent();
            }
            reader.ReadEndElement();
        }

        public void WriteXml(System.Xml.XmlWriter writer)
        {
            XmlSerializer keySerializer = new XmlSerializer(typeof(TKey));
            XmlSerializer valueSerializer = new XmlSerializer(typeof(TValue));

            foreach (TKey key in this.Keys)
            {
                writer.WriteStartElement("item");

                writer.WriteStartElement("key");
                keySerializer.Serialize(writer, key);
                writer.WriteEndElement();

                writer.WriteStartElement("value");
                TValue value = this[key];
                valueSerializer.Serialize(writer, value);
                writer.WriteEndElement();

                writer.WriteEndElement();
            }
        }
        #endregion
    }
}