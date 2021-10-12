using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;
using System.Collections;
using System.Xml.Linq;


namespace TCCSim
{
    static class Message
    {
        public static int GetTrainID(Byte[] Msg)
        {
            if (Msg.Length >= 2)
                return Msg[1];

            return -1;
        }

        /* 
         * Take data from message creation view in XML format
         * and create byte array.
         */
        public static byte[] XMLtoByte(XElement messageXML)
        {
            ArrayList bytestr = new ArrayList();

            // Add NID_MESSAGE_TYPE to bytestr.
            byte nid_mtype = byte.Parse(messageXML.Attribute("value").Value);
            bytestr.Add(nid_mtype);

            foreach (XElement x in messageXML.Elements())
            {
                string ftype = x.Name.ToString();

                if (ftype == "M_END_OF_MESSAGE")
                {
                    // Add end of message stuffs here.
                    bytestr.Add((byte)0x00);
                    continue;
                }

                // Handle special case of T_CLOCK.
                // This value shall always be set to system clock, so it has to be fetchen upon request.
                if (ftype == "T_CLOCK")
                {
                    TimeSpan t = (DateTime.Now - new DateTime(1970, 1, 1));
                    uint timestamp = (uint)t.TotalSeconds;
                    byte[] time = BitConverter.GetBytes(timestamp);
                    if (BitConverter.IsLittleEndian)
                    {
                        Array.Reverse(time);
                    }
                    for (int i = 0; i < time.Length; ++i)
                    {
                        bytestr.Add(time[i]);
                    }
                    Console.WriteLine("time: " + BitConverter.ToString(time));
                    continue;
                }

                // Start of block. Add block type id to bytestr.
                if (x.Name == "Block")
                {
                    try
                    {
                        byte block = byte.Parse(x.Attribute("value").Value);
                        bytestr.Add(block);
                        continue;
                    }
                    catch (Exception)
                    {
                        return null;
                    }
                }

                string mtype = MsgDef.GetFormatFromName(ftype);
                // Check if message type could be fetched.
                if (mtype == null)
                    return null;

                // Bitfields can also be handled here since it is just
                // numeric values with a description string.
                if (mtype == "UINT" || mtype == "INT" || mtype == "BITMASK")
                {
                    bool parse_bits = false;
                    // Check if bitmask has values stored as bits or as unsigned integer.
                    if (mtype == "BITMASK")
                    {
                        try
                        {
                            parse_bits = x.Element("Bit").HasAttributes;
                        }
                        catch (Exception) { }
                    }
                    if (parse_bits)
                    {
                        try
                        {
                            ulong bit_value = 0;
                            foreach (XElement bit in x.Elements())
                            {
                                short pot = short.Parse(bit.Attribute("value").Value, 0);
                                bit_value += (ulong)Math.Pow(2, pot);
                            }
                            byte[] bv = BitConverter.GetBytes(bit_value);

                            int len = int.Parse(MsgDef.GetElementValue(ftype, "Length"));
                            if (BitConverter.IsLittleEndian)
                            {
                                Array.Reverse(bv);
                                // Add interesting bytes to bytestr, use length to find out which bytes.
                                for (int i = 8 - len; i < 8; ++i)
                                {
                                    bytestr.Add(bv[i]);
                                }
                            }
                            else
                            {
                                for (int i = 0; i < len; ++i)
                                    bytestr.Add(bv[i]);
                            }
                            continue;
                        }
                        catch (Exception) { return null; }
                    }
                    long value = 0;
                    try
                    {
                        value = long.Parse(x.Value);
                    }
                    catch (Exception) { }
                    byte[] b = BitConverter.GetBytes(value);

                    byte length = byte.Parse(MsgDef.GetElementValue(ftype, "Length"));
                    if (BitConverter.IsLittleEndian)
                    {
                        Array.Reverse(b);
                        // Add interesting bytes to bytestr, use length to find out which bytes.
                        for (int i = 8 - length; i < 8; ++i)
                        {
                            bytestr.Add(b[i]);
                        }
                    }
                    else
                    {
                        for (int i = 0; i < length; ++i)
                            bytestr.Add(b[i]);
                    }
                    continue;
                }

                if (mtype == "STRING")
                {
                    // Strings can only have ASCII chars.
                    byte[] b = System.Text.Encoding.ASCII.GetBytes(x.Value);
                    int length = int.Parse(MsgDef.GetElementValue(ftype, "Length"));

                    for (int i = 0; i < b.Length && i < length; ++i)
                    {
                        bytestr.Add(b[i]);
                    }
                    // Pad unused bytes with null chars.
                    for (int i = 0; i < length - b.Length; ++i)
                    {
                        bytestr.Add((byte)0x00);
                    }
                    continue;
                }
            }
            return (byte[])bytestr.ToArray(typeof(byte));
        }

        public static string string_MTypeFromBytes(byte[] bytestr)
        {
            if (bytestr[0] != 2)
            {
                return "Unknown";
            }
            try
            {
                XElement msgdesc = MsgDef.GetMessageDescription(bytestr[8].ToString());
                return msgdesc.Attribute("type").Value;
            }
            catch (Exception) { return "Unknown"; }
        }

        public static int int_MTypeFromBytes(byte[] bytestr)
        {
            if (bytestr[0] != 2)
            {
                return 0;
            }
            try
            {
                XElement msgdesc = MsgDef.GetMessageDescription(bytestr[8].ToString());
                return int.Parse(msgdesc.Attribute("value").Value);
            }
            catch (Exception) { return 0; }
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

        #region pr stuff
        // Used to call ParseBytes without a known message type.
        public static ArrayList ParseBytes(byte[] bytestr)
        {
            return ParseBytes(bytestr, null);
        }

        // Parses all fields and blocks in a message byte string. message_type is 
        // used to be able to determine which message type a byte string is.
        // This since the nid_message_type in the byte string cannot be used to
        // certainly determine which message type that is to be parsed. This due
        // to that message types are identified by name to be able to create faulty
        // definitions of a message type.
        public static ArrayList ParseBytes(byte[] bytestr, string message_type)
        {
            ArrayList message = new ArrayList();

            // Handle header 
            if (bytestr[0] != 2)
            {
                FieldType ft = new FieldType("<STX> Fail");
                message.Add(ft);
                return message;
            }

            if (bytestr.Length < 9)
            {
                FieldType ft = new FieldType("Byte array to short to contain message!");
                message.Add(ft);
                return message;
            }

            FieldType header = new FieldType("<STX>");
            header.values.Add(bytestr[0]);
            message.Add(header);

            header = new FieldType("ID");
            header.values.Add(bytestr[1]);
            message.Add(header);

            header = new FieldType("LEN");
            byte[] b_header = TakeNrBytes(bytestr, 2, 2);
            ushort bytelen = BitConverter.ToUInt16(b_header, 0);
            header.values.Add(bytelen);
            message.Add(header);

            header = new FieldType("T_SENDER");
            b_header = TakeNrBytes(bytestr, 4, 2);
            header.values.Add(BitConverter.ToUInt16(b_header, 0));
            message.Add(header);

            header = new FieldType("T_REF");
            b_header = TakeNrBytes(bytestr, 6, 2);
            header.values.Add(BitConverter.ToUInt16(b_header, 0));
            message.Add(header);

            header = new FieldType("CRC");
            b_header = TakeNrBytes(bytestr, bytestr.Length - 5, 4);
            header.values.Add(BitConverter.ToUInt32(b_header, 0));
            message.Add(header);

            header = new FieldType("BCC");
            header.values.Add(bytestr[bytestr.Length - 1]);
            message.Add(header);

            // Parse static fields.
            int index = 9;
            XElement msgdesc;
            if (message_type != null) 
            {
                msgdesc = MsgDef.GetMsgDscrFromStr(message_type);
            }
            else
            {
                msgdesc = MsgDef.GetMessageDescription(int_MTypeFromBytes(bytestr).ToString());
            }

            if (msgdesc == null)
            {
                FieldType ft = new FieldType("Could not parse message type!");
                message.Add(ft);
                return message;
            }

            IEnumerator<XElement> mtypetree = msgdesc.Elements().GetEnumerator();

            // Verify the first two fields of the XElement.
            // Should be StationaryMessage and Filtered.
            // These are not used when parsing.
            XElement tmpx;
            mtypetree.MoveNext();
            tmpx = mtypetree.Current;
            if (tmpx.Name != "StationaryMessage")
            {
                FieldType fault = new FieldType("Error in XML tag " + msgdesc.Name);
                message.Add(fault);
                return message;
            }
            mtypetree.MoveNext();
            tmpx = mtypetree.Current;
            if (tmpx.Name != "Filtered")
            {
                FieldType fault = new FieldType("Error in XML tag " + msgdesc.Name);
                message.Add(fault);
                return message;
            }
            
            ReadFields(mtypetree, ref index, bytestr, message);

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

                XElement blockdesc;
                if (message_type != null) 
                {
                    blockdesc = MsgDef.GetBlockDscWithMT(message_type, blocktype);
                }
                else
                {
                    blockdesc = MsgDef.GetBlockDescription(blocktype.ToString());
                }
                if (blockdesc == null)
                {
                    BlockType failblock = new BlockType("Could not determine block type, cannot continue parsing!");
                    message.Add(failblock);
                    return message;
                }
                string name;
                try
                {
                    name = blockdesc.Attribute("type").Value;
                }
                catch (Exception) { name = "Unknown"; }

                BlockType b = new BlockType(name);
                message.Add(b);
                IEnumerator<XElement> bd = blockdesc.Elements().GetEnumerator();

                // Block type EXTERNAL_DATA is a secial case that has to be handled by its own.
                if (name == "EXTERNAL_DATA")
                {
                    // Get NID_SYSTEM byte.
                    FieldType tmp_f = new FieldType("NID_SYSTEM");
                    tmp_f.values.Add(bytestr[++index]);
                    b.values.Add(tmp_f);

                    // Get N_LENGTH.
                    byte[] n_length = new byte[2];
                    n_length[0] = bytestr[index++]; n_length[1] = bytestr[index++];
                    if (BitConverter.IsLittleEndian)
                    {
                        Array.Reverse(n_length);
                    }
                    ushort _n_length = BitConverter.ToUInt16(n_length, 0);
                    tmp_f = new FieldType("N_LENGTH");
                    tmp_f.values.Add(_n_length);
                    b.values.Add(tmp_f);

                    // Get app data.
                    byte[] appdata = new byte[_n_length];
                    for (int i = 0; i < _n_length; ++i)
                    {
                        appdata[i] = bytestr[index + i];
                    }
                    index += _n_length;
                    tmp_f.values.Add(appdata); // Just add after N_LENGTH value.
                }
                else
                {
                    ReadFields(bd, ref index, bytestr, b.values);
                }
            }

            return message;
        }
        private static void ReadFields(IEnumerator<XElement> Xfields, ref int index, byte[] bytestr, ArrayList list)
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
                        continue;
                    }

                    // Description of current field type
                    XElement fdesc = MsgDef.GetFieldDescription(fname);
                    if (fdesc == null)
                    {
                        FieldType ff = new FieldType("Could not find field type: " + fname);
                        list.Add(ff);
                        return;
                    }

                    // Get byte length of value to read.
                    int length;
                    try
                    {
                        length = int.Parse(fdesc.Element("Length").Value);
                    }
                    catch (Exception)
                    {
                        FieldType ff = new FieldType("Could not read length of " + fname);
                        list.Add(ff);
                        return;
                    }

                    // Read interesting bytes from large byte string.
                    byte[] bytes = new byte[length];
                    for (int i = 0; i < length; ++i)
                    {
                        bytes[i] = bytestr[index + i];
                    }
                    // Update index to read from.
                    index += length;

                    string Format;
                    try
                    {
                        Format = fdesc.Element("Format").Value;
                    }
                    catch (Exception) { Format = "STRING"; } // If the format is unreadable try handling the field as text.

                    FieldType ft_ptr = new FieldType(fname);
                    list.Add(ft_ptr);

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
                        ft_ptr.values.Add(value);
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
                                ft_ptr.values.Add(desc);
                                continue;

                            }
                        }
                        catch (Exception)
                        {
                            ft_ptr.values.Add(value + " - Unknown");
                            continue;
                        }

                        // Show value formated in base 16.
                        try
                        {
                            if (fdesc.Element("Min").Value.StartsWith("0x"))
                            {
                                ft_ptr.values.Add(string.Format("0x{0:X}\r\n", value));
                                continue;
                            }
                        }
                        catch (Exception) { }

                        ft_ptr.values.Add(value);
                        continue;
                    }
                    if (Format == "BITMASK")
                    {
                        if (BitConverter.IsLittleEndian)
                            Array.Reverse(bytes);

                        uint bits = PadConvert(bytes);

                        // Check that value of bitmask not exceeds max value.
                        uint maxvalue;
                        try
                        {
                            maxvalue = uint.Parse(fdesc.Element("max").Value);
                        }
                        catch (Exception) { continue; }

                        if (bits > maxvalue)
                            continue;

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
                                    ft_ptr.values.Add(bitdesc);
                                }
                            }
                        }
                        catch (Exception) { continue; }
                        continue;
                    }
                    if (Format == "STRING")
                    {
                        ft_ptr.values.Add(System.Text.Encoding.ASCII.GetString(bytes, 0, bytes.Length));
                        continue; // If more cases would be added.
                    }
                }
                else
                {
                    return;
                }
            }
        }
        #endregion

        // Parses a message byte string to XML.
        // Used in scripting.
        public static XElement BytesToXML(byte[] bstr)
        {
            // Here only a successfull result or no result is of interest.
            try
            {
                XElement parsed;
                int index = 9;

                XElement xmt = MsgDef.GetMessageDescription(int_MTypeFromBytes(bstr).ToString());

                parsed = new XElement(xmt.Attribute("type").Value, new XAttribute("value", int_MTypeFromBytes(bstr)));

                IEnumerator<XElement> mti = xmt.Elements().GetEnumerator();

                // Verify the first two fields of the XElement.
                // Should be StationaryMessage and Filtered.
                // These are not used when parsing.
                XElement tmpx;
                mti.MoveNext();
                tmpx = mti.Current;
                if (tmpx.Name != "StationaryMessage")
                {
                    return null;
                }
                mti.MoveNext();
                tmpx = mti.Current;
                if (tmpx.Name != "Filtered")
                {
                    return null;
                }

                SimpleParse(mti, ref index, bstr, parsed);

                while (index < bstr.Length - 5)
                {
                    byte blocktype = bstr[index++];
                    // Found m_end_of_message, stop.
                    if (blocktype == 0)
                    {
                        break;
                    }
                    parsed.Add(new XElement("Block", new XAttribute("value", blocktype)));

                    XElement blockdesc = MsgDef.GetBlockDescription(blocktype.ToString());
                    string name = blockdesc.Attribute("type").Value;

                    if (name == "EXTERNAL_DATA")
                    {
                        parsed.Add(new XElement("NID_SYSTEM", bstr[++index]));
                        // Get N_LENGTH.
                        byte[] n_length = new byte[2];
                        n_length[0] = bstr[index++]; n_length[1] = bstr[index++];
                        if (BitConverter.IsLittleEndian)
                        {
                            Array.Reverse(n_length);
                        }
                        ushort _n_length = BitConverter.ToUInt16(n_length, 0);
                        parsed.Add(new XElement("N_LENGTH", _n_length));
                        // Get app data.
                        byte[] appdata = new byte[_n_length];
                        for (int i = 0; i < _n_length; ++i)
                        {
                            appdata[i] = bstr[index + i];
                        }
                        index += _n_length;
                        parsed.Add(new XElement("EXTERNAL_DATA", appdata));
                    }
                    else
                    {
                        IEnumerator<XElement> bd = blockdesc.Elements().GetEnumerator();
                        SimpleParse(bd, ref index, bstr, parsed);
                    }
                }
                return parsed;
            }
            catch (Exception) { return null; }
        }
        // Used with scripting. So only the values are interesting, no need for translation to description of each value if there is one.
        private static void SimpleParse(IEnumerator<XElement> xf, ref int index, byte[] bstr, XElement xout)
        {
            while (xf.MoveNext())
            {
                XElement x = xf.Current;
                if (x.Name == "FieldType")
                {
                    string fname = x.Attribute("Name").Value;
                    if (fname == "M_END_OF_MESSAGE")
                    {
                        continue;
                    }
                    // Description of current field type
                    XElement fdesc = MsgDef.GetFieldDescription(fname);

                    // Get byte length of value to read.
                    int length = int.Parse(fdesc.Element("Length").Value);

                    // Read interesting bytes from large byte string.
                    byte[] bytes = new byte[length];
                    for (int i = 0; i < length; ++i)
                    {
                        bytes[i] = bstr[index + i];
                    }
                    // Update index to read from.
                    index += length;

                    string Format = fdesc.Element("Format").Value;

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
                                sbyte int8 = (sbyte)bstr[index - length];
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
                        xout.Add(new XElement(fname, value));
                        continue;
                    }
                    // A bitmask is really just an unsigned integer with a description for each bit.
                    // When scripting they can be handled equally.
                    if (Format == "UINT" || Format == "BITMASK")
                    {

                        // Values in bytestr is in network byte order.
                        // Change order if needed.
                        if (BitConverter.IsLittleEndian)
                        {
                            Array.Reverse(bytes);
                        }

                        uint value = PadConvert(bytes);
                        xout.Add(new XElement(fname, value));
                        continue;
                    }
                    if (Format == "STRING")
                    {
                        xout.Add(new XElement(fname, System.Text.Encoding.ASCII.GetString(bytes, 0, bytes.Length)));
                        continue;
                    }
                    xout.Add(new XElement(fname, bytes));
                }
            }
        }
    }
}
