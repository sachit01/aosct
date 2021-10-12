using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Xml.Linq;

namespace MessageTranslation
{
    class Program
    {
        #region Constants
        const int STX_FAIL = 1;
        #endregion

        #region Globals
        static bool PrintHeader;
        static string XmlDir = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                @"XML\");
        static XDocument MsgDefs = XDocument.Load(Path.Combine(XmlDir, "Message Descriptions.xml"));
        static XDocument BlockDefs = XDocument.Load(Path.Combine(XmlDir, "Block Descriptions.xml"));
        static XDocument FieldDefs = XDocument.Load(Path.Combine(XmlDir, "Field Descriptions.xml"));
        static XDocument ErrorDesc = XDocument.Load(Path.Combine(XmlDir, "ErrorCodes.xml"));
        #endregion

        static void Main(string[] args)
        {
            PrintHeader = false;
            for (int i = 0; i < args.Length; ++i)
            {
                if (args[i] == "-v")
                    PrintHeader = true;
                if (args[i] == "-h")
                {
                    Console.WriteLine("\n-v: Extra verbosity, include message headers when printing.\n-h: Print this help message.\n");
                    Console.WriteLine("Reads from message strings from STDIN where each byte is separatade by '.' or '-'");
                    Console.WriteLine("Example of usage: c:\\> type message_data.txt | " + System.AppDomain.CurrentDomain.FriendlyName);
                    return;
                }
            }
            string s;
            while ((s = Console.ReadLine()) != null)
            {
                // The indata is now expected to be a string where
                // each byte is separated by . or -
                string[] line = s.Split(new char[] { '.', '-' });
                ArrayList parse_line = new ArrayList();
                foreach (string b in line)
                {
                    parse_line.Add(Convert.ToByte(b));
                }
                ParseBytes((byte[])parse_line.ToArray(typeof(byte)));
                Console.WriteLine(); // Separate messages
            }
        }

        // Takes a message as an array of bytes and translates it into human readable representation.
        public static int ParseBytes(byte[] bytestr)
        {
            ArrayList message = new ArrayList();

            // Handle header 
            if (bytestr[0] != 2)
            {
                Console.WriteLine("<STX> Fail!");
                return STX_FAIL;
            }

            // Print header values if wanted.
            if (PrintHeader)
            {
                // Print <STX>
                Console.WriteLine("<STX>: " + bytestr[0]);

                // ID
                Console.WriteLine("ID: " + bytestr[1]);

                // Length
                byte[] b_header = TakeNrBytes(bytestr, 2, 2);
                ushort bytelen = BitConverter.ToUInt16(b_header, 0);
                Console.WriteLine("LEN: " + bytelen);

                // T_SENDER
                b_header = TakeNrBytes(bytestr, 4, 2);
                Console.WriteLine("T_SENDER: " + BitConverter.ToUInt16(b_header, 0));

                // T_REF
                b_header = TakeNrBytes(bytestr, 6, 2);
                Console.WriteLine("T_REF: " + BitConverter.ToUInt16(b_header, 0));

                // CRC
                b_header = TakeNrBytes(bytestr, bytestr.Length - 5, 4);
                Console.WriteLine("CRC: " + BitConverter.ToUInt32(b_header, 0));

                // BCC
                Console.WriteLine("BCC: " + bytestr[bytestr.Length - 1]);
            }

            // Parse static fields.
            int index = 9;
            XElement msgdesc = GetMessageDescription(bytestr[8].ToString());

            IEnumerator<XElement> mtypetree = msgdesc.Elements().GetEnumerator();
            mtypetree.MoveNext(); mtypetree.MoveNext(); // Skip first two tags since they are not relevant here.

            Console.WriteLine(string.Format("NID_MESSAGE_TYPE: {0} ({1})", msgdesc.Attribute("type").ToString(), bytestr[8]));
            ReadFields(mtypetree, ref index, bytestr);

            // Parse blocks if any.
            while (index < bytestr.Length - 5)
            {
                // Read block id, move index to next interesting byte.
                byte blocktype = bytestr[index++];

                // Found m_end_of_message, stop.
                if (blocktype == 0)
                {
                    break;
                }

                XElement blockdesc = GetBlockDescription(blocktype.ToString());

                string name = blockdesc.Attribute("type").Value;
                Console.WriteLine(name);

                IEnumerator<XElement> bd = blockdesc.Elements().GetEnumerator();

                // Block type EXTERNAL_DATA is a secial case that has to be handled by its own.
                if (name == "EXTERNAL_DATA")
                {
                    // Get NID_SYSTEM byte.
                    Console.WriteLine("NID_SYSTEM: " + bytestr[++index]);

                    // Get N_LENGTH.
                    byte[] n_length = new byte[2];
                    n_length[0] = bytestr[index++]; n_length[1] = bytestr[index++];
                    if (BitConverter.IsLittleEndian)
                    {
                        Array.Reverse(n_length);
                    }
                    ushort _n_length = BitConverter.ToUInt16(n_length, 0);
                    Console.WriteLine("N_LENGTH: " + _n_length);

                    // Get app data.
                    byte[] appdata = new byte[_n_length];
                    for (int i = 0; i < _n_length; ++i)
                    {
                        appdata[i] = bytestr[index + i];
                    }
                    index += _n_length;
                    Console.WriteLine(BitConverter.ToString(appdata));
                }
                else
                {
                    ReadFields(bd, ref index, bytestr);
                }
            }

            return 0;
        }
        private static void ReadFields(IEnumerator<XElement> Xfields, ref int index, byte[] bytestr)
        {
            while (Xfields.MoveNext())
            {
                XElement x = Xfields.Current;
                if (x.Name == "FieldType")
                {
                    string fname = x.Attribute("Name").Value;

                    // Special case.
                    if (fname == "M_END_OF_MESSAGE")
                    {
                        Console.WriteLine("M_END_OF_MESSAGE");
                        continue;
                    }

                    // Description of current field type
                    XElement fdesc = GetFieldDescription(fname);

                    // Get byte length of value to read.
                    int length = int.Parse(fdesc.Element("Length").Value);

                    // Read interesting bytes from large byte string.
                    byte[] bytes = new byte[length];
                    for (int i = 0; i < length; ++i)
                    {
                        bytes[i] = bytestr[index + i];
                    }
                    // Update index to read from.
                    index += length;

                    string Format = fdesc.Element("Format").Value;

                    Console.Write(fname + ": ");

                    // Signed values has to be handled more specificly.
                    // To parse a byte string to short the byte string read from
                    // must be at least 2 bytes long. Since this is a 8 bit signed
                    // integer the value must be read from the large bytestr
                    // with some over reading. The same goes for every type that 
                    // consists of an uneven number of bytes. Herp derp.
                    if (Format == "INT")
                    {
                        var value = new Int32();
                        if (BitConverter.IsLittleEndian)
                        {
                            Array.Reverse(bytes);
                        }

                        switch (length)
                        {
                            case 1:
                                sbyte int8 = (sbyte)bytestr[index - length];
                                value = int8;
                                break;
                            case 2:
                                value = BitConverter.ToInt16(bytes, 0);
                                break;
                            case 3:
                                // 24 bit long signed integer...
                                byte[] b24 = new byte[4];
                                // Byte string used must be 4 bytes long for ToInt32.
                                // Where to pad with zero depends on endianness.
                                if (BitConverter.IsLittleEndian)
                                {
                                    Array.Copy(bytes, 0, b24, 0, 3);
                                }
                                else
                                {
                                    Array.Copy(bytes, 0, b24, 1, 3);
                                }

                                int int24 = BitConverter.ToInt32(b24, 0);
                                // Take interesting bytes.
                                int24 &= 0xffffff;
                                // Test for negative.
                                if ((int24 & 0x800000) != 0)
                                {
                                    // Set negative
                                    unchecked { int24 |= (int)0xff000000; }
                                }
                                value = int24;
                                break;
                            case 4:
                                value = BitConverter.ToInt32(bytes, 0);
                                break;
                            default:
                                break;
                        }
                        Console.WriteLine(value);
                        continue;
                    }

                    // Handle integers and unsigned integers.
                    if (Format == "UINT")
                    {

                        // Values in bytestr is in network byte order.
                        // Change order if needed.
                        if (BitConverter.IsLittleEndian)
                        {
                            Array.Reverse(bytes);
                        }

                        uint value = PadConvert(bytes);

                        // Some integers has descriptions to each value.
                        // If so display description instead of value.
                        try
                        {
                            if (fdesc.Element("Special").Element("Fields").HasElements)
                            {
                                string desc = (from field in fdesc.Element("Special").Element("Fields").Descendants("Field")
                                               where field.Attribute("value").Value == (string)value.ToString()
                                               select field).First().Value;
                                Console.WriteLine(desc);
                                continue;
                            }
                        }
                        catch (NullReferenceException) { }

                        // Show value formated in base 16.
                        if (fdesc.Element("Min").Value.StartsWith("0x"))
                        {
                            Console.WriteLine(string.Format("0x{0:X}\r\n", value));
                            continue;
                        }

                        Console.WriteLine(value);
                        continue;
                    }
                    if (Format == "BITMASK")
                    {
                        Console.WriteLine();
                        if (BitConverter.IsLittleEndian)
                            Array.Reverse(bytes);

                        uint bits = PadConvert(bytes);

                        try
                        {
                            uint maxvalue = uint.Parse(fdesc.Element("Max").Value);
                        }
                        catch (Exception) { continue; }

                        try
                        {
                            // Check which bits are set, add description to output string..
                            for (int i = 0; i < length * 8; ++i)
                            {
                                int bit = 1 << i;
                                if ((bits & 1 << i) != 0)
                                {
                                    string bitdesc = (from field in fdesc.Element("Special").Element("Bits").Descendants("Bit")
                                                      where field.Attribute("value").Value == (string)i.ToString()
                                                      select field).First().Value;
                                    Console.WriteLine("\t" + bitdesc);
                                }
                            }
                        }
                        catch (Exception) { continue; }
                        continue;
                    }
                    if (Format == "STRING")
                    {
                        Console.WriteLine(System.Text.Encoding.ASCII.GetString(bytes, 0, bytes.Length));
                    }
                }
                else
                {
                    return;
                }
            }
        }
        // Reads number of bytes from byte string from index and returns
        // byte string in correct endian order for integers.
        private static byte[] TakeNrBytes(byte[] from_byte, int index, int nr_bytes)
        {
            byte[] b_ret = new byte[nr_bytes];
            for (int i = 0; i < nr_bytes; ++i)
            {
                b_ret[i] = from_byte[index + i];
            }
            if (BitConverter.IsLittleEndian)
            {
                Array.Reverse(b_ret);
            }
            return b_ret;
        }
        // Gets a message description from value, where value is the numeric value of message type.
        public static XElement GetMessageDescription(string value)
        {
            return (from MD in MsgDefs.Descendants("Message")
                    where MD.Attribute("value").Value == value
                    select MD).First();
        }
        // Gets at block description from btype, where btype is the numeric value of block type.
        public static XElement GetBlockDescription(string btype)
        {
            return (from BD in BlockDefs.Descendants("Block")
                    where BD.Attribute("value").Value == btype
                    select BD).First();
        }
        // Gets a description of all information for a field type. Fields can only be identified by name.
        public static XElement GetFieldDescription(string fname)
        {
            return (from FD in FieldDefs.Descendants("FieldDescription")
                    where FD.Attribute("type").Value == fname
                    select FD).First();
        }
        // Used to convert byte[<=4] to Int32
        private static UInt32 PadConvert(byte[] bytes)
        {
            byte[] uint32 = new byte[4];
            for (int i = 0; i < bytes.Length; ++i)
            {
                uint32[i] = bytes[i];
            }
            return BitConverter.ToUInt32(uint32, 0);
        }
    }
}
