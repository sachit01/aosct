using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;
using System.IO;
using System.Xml.Linq;
using System.Collections;

namespace TCCSim
{
    public class MsgDef
    {
        private static XDocument MsgDefs;
        private static XDocument FieldDefs;
        private static XDocument BlockDefs;
        private static XDocument ErrorDesc;
        private static string XmlDir;

        public MsgDef()
        {
            //MsgDefs = Path.GetDirectoryName(
#if DEBUG
            XmlDir = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"..\..\XML\");
#else
            XmlDir = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @".\XML\");
#endif
            MsgDefs = XDocument.Load(Path.Combine(XmlDir, "MessageDescriptions.xml"));
            BlockDefs = XDocument.Load(Path.Combine(XmlDir, "BlockDescriptions.xml"));
            FieldDefs = XDocument.Load(Path.Combine(XmlDir, "FieldDescriptions.xml"));
            ErrorDesc = XDocument.Load(Path.Combine(XmlDir, "ErrorCodes.xml"));
            
        }

        public Dictionary<string, FieldType> GetMessageTypes()
        {
            Dictionary<string, FieldType> mtypes = new Dictionary<string, FieldType>();

            foreach (XElement x in MsgDefs.Descendants("Message"))
            {
                FieldType mt = new FieldType(x.Attribute("type").Value);
                int id = int.Parse(x.Attribute("value").Value);
                bool filtered = bool.Parse(x.Element("Filtered").Value);
                mt.values.Add(filtered);
                mtypes.Add(mt.name, mt);
            }
            return mtypes;
        }

        // Returns a list of all message types that can be created on the stationary side.
        public IEnumerable<StringInt> GetStationaryTypes()
        {
            IEnumerable<StringInt> list = from El in MsgDefs.Descendants("Message")
                                          where El.Element("StationaryMessage").Value == "True"
                                          select new StringInt { Data = El.Attribute("type").Value, Number = int.Parse(El.Attribute("value").Value) };
            return list;
        }
        
        // Fetches a description of a message type from the name of the message type.
        // Returns an ArrayList of the field names, where each name is stored in a FieldType.
        // A XElement with all the fields and blocks of the message is also returned.
        public GetTypeEventArgs GetMessageType(string Type)
        {
            //try
            //{
                IEnumerable<XElement> Elem = from El in MsgDefs.Descendants("Message")
                                             where El.Attribute("type").Value == Type.ToString()
                                             select El;

                ArrayList Fields = new ArrayList();
                foreach (XElement l in Elem)
                {
                    foreach (var i in l.Elements())
                    {
                        if (i.Name == "FieldType")
                        {
                            IEnumerable<XElement> x = from El in FieldDefs.Descendants("FieldDescription")
                                                      where El.Attribute("type").Value == i.Attribute("Name").Value
                                                      select El;
                            FieldType ft = new FieldType(i.Attribute("Name").Value);
                            ft.values.Add(x.First());
                            Fields.Add(ft);
                            // Used to add message type specific description of field.
                            try
                            {
                                ft.values.Add(i.Element("Display").Value.ToString());
                            }
                            catch (System.NullReferenceException) { ft.values.Add(ft.name); }
                        }
                        if (i.Name == "BlockType")
                        {
                            IEnumerable<XElement> x = from El in FieldDefs.Descendants("FieldDescription")
                                                      where El.Attribute("type").Value == i.Attribute("Name").Value
                                                      select El;
                            BlockType bt = new BlockType(i.Attribute("Name").Value);
                            bt.values.Add(i);
                            try
                            {
                                bt.values.Add(i.Element("Display").Value.ToString());
                            }
                            catch (System.NullReferenceException) { bt.values.Add(bt.name); }
                            Fields.Add(bt);
                        }
                    }
                }
                GetTypeEventArgs e = new GetTypeEventArgs(Elem, Fields);
                return e;
            //}
            //catch (Exception) { return null; }
        }

        // Fetches all the fields from a block type.
        // BlockID is the name of the block to be fetched.
        // Returns a list of FieldTypes with the field names 
        // and a XElement of the block.
        public BlockFieldsEventArgs BlockFields(string BlockID)
        {
            try
            {
                IEnumerable<XElement> Elem;
                Elem = from El in BlockDefs.Descendants("Block")
                       where El.Attribute("type").Value == BlockID
                       select El;

                ArrayList Fields = new ArrayList();
                foreach (XElement l in Elem)
                {
                    foreach (var i in l.Elements())
                    {

                        if (i.Name == "FieldType")
                        {
                            IEnumerable<XElement> x = from El in FieldDefs.Descendants("FieldDescription")
                                                      where El.Attribute("type").Value == i.Attribute("Name").Value
                                                      select El;
                            FieldType ft = new FieldType(i.Attribute("Name").Value);
                            ft.values.Add(x.First());
                            Fields.Add(ft);
                            // Used to add message type specific description of field.
                            try
                            {
                                ft.values.Add(i.Element("Display").Value.ToString());
                            }
                            catch (System.NullReferenceException) { ft.values.Add(ft.name); }
                        }
                    }
                }
                BlockFieldsEventArgs eargs = new BlockFieldsEventArgs(Elem, Fields);
                return eargs;
            }
            catch (Exception) { return null; }
        }

        // Gets the format, UINT, INT, BITMASK and so on from
        // the name of the field.
        public static string GetFormatFromName(string name)
        {
            try
            {
                string ret = (from Format in FieldDefs.Descendants("FieldDescription")
                              where Format.Attribute("type").Value == name
                              select Format.Element("Format").Value).First();
                return ret;
            }
            catch (Exception) { return null; }
        }

        // Field name, element name.
        // Used to fetch the value of a specified element in a field type.
        // Eg. the Max value of a field type.
        public static string GetElementValue(string fname, string element)
        {
            try
            {
                string ret = (from FT in FieldDefs.Descendants("FieldDescription")
                              where FT.Attribute("type").Value == fname
                              select FT.Element(element).Value).First();
                return ret;
            }
            catch (Exception) { return null; }
        }

        // Gets a message description from value, where value is the numeric value of message type.
        public static XElement GetMessageDescription(string value)
        {
            try
            {
                return (from MD in MsgDefs.Descendants("Message")
                        where MD.Attribute("value").Value == value
                        select MD).First();
            }
            catch (Exception) { return null; }
        }

        // Gets at block description from btype, where btype is the numeric value of block type.
        public static XElement GetBlockDescription(string btype)
        {
            try
            {
                return (from BD in BlockDefs.Descendants("Block")
                        where BD.Attribute("value").Value == btype
                        select BD).First();
            }
            catch (Exception) { return null; }
        }

        // Fetches the description of a block type from the block name.
        public static XElement GetBlockDescFromName(string bname)
        {
            try
            {
                return (from BD in BlockDefs.Descendants("Block")
                        where BD.Name.ToString() == bname
                        select BD).First();
            }
            catch (Exception) { return null; }
        }

        // Gets a description of all information for a field type. Fields can only be identified by name.
        public static XElement GetFieldDescription(string fname)
        {
            try
            {
                return (from FD in FieldDefs.Descendants("FieldDescription")
                        where FD.Attribute("type").Value == fname
                        select FD).First();
            }
            catch (Exception) { return null; }
        }

        public static String GetErrorDescription(int Code)
        {
            try
            {
                return (from Errors in ErrorDesc.Descendants("Row")
                        where Errors.Element("ST_Error_Code").Value == Code.ToString()
                        select Errors.Element("Description").Value).First();
            }
            catch (Exception) { return null; }
        }

        // Returns a XElement that describes the message type requested by message type name in mt.
        public static XElement GetMsgDscrFromStr(string mt)
        {
            try
            {
                return (from MD in MsgDefs.Descendants("Message")
                        where MD.Attribute("type").Value == mt
                        select MD).First();
            }
            catch (Exception) { return null; }
        }

        // Returns the descrption of a block type that is used in a specific message type.
        // This since there can be multiple definitions of a block with the same id to create
        // errenous blocks for testing.
        public static XElement GetBlockDscWithMT(string mt, int bt)
        {
            try
            {
                XElement mess = (from MD in MsgDefs.Descendants("Message")
                                 where MD.Attribute("type").Value == mt
                                 select MD).First();
                string bname = (from MD in mess.Descendants("BlockType")
                                where MD.Attribute("Numeric").Value == bt.ToString()
                                select MD.Attribute("Name").Value).First();
                return (from BD in BlockDefs.Descendants("Block")
                        where BD.Attribute("type").Value == bname
                        select BD).First();
            }
            catch (Exception) { return null; }
        }

        public static bool SaveFilters(Dictionary<string, FieldType> Filter)
        {
            if (Filter == null)
                return false;

            foreach (String s in Filter.Keys)
            {
                SetMTFilter(s, (bool)Filter[s].values[0]);
            }
            try
            {
                MsgDefs.Save(Path.Combine(XmlDir, "MessageDescriptions.xml"));
            }
            catch (Exception) { return false; } // MessageDescriptions.xml write protected.
            return true;
        }

        private static bool SetMTFilter(string fname, bool fbool)
        {
            XElement query;
            try
            {
                query = (from mt in MsgDefs.Descendants("Message")
                                  where mt.Attribute("type").Value == fname
                                  select mt.Element("Filtered")).First();
            }
            catch (Exception) { return false; }
            query.Value = fbool.ToString();
            return true;
        }
    }
}
