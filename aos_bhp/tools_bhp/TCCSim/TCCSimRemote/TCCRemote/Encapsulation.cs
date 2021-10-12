using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Collections;

namespace TCCSim
{
    class Encapsulation
    {
    }

    // Used to create a pair of one string and on int to use with comboboxes.
    public class StringInt
    {
        public String Data {get;set;}
        public int Number {get;set;}

        public StringInt()
        {
            Data = null;
            Number = 0;
        }

        public StringInt(String Data, int Number)
        {
            this.Data = Data;
            this.Number = Number;
        }

        public override String ToString()
        {
            return Data;
        }
    }

    public class MessageFile : IComparable<MessageFile>
    {
        public String FilePath { get; set; }

        public MessageFile(String FilePath)
        {
            this.FilePath = FilePath;
        }

        public override string ToString()
        {
            return Path.GetFileName(FilePath);
        }

        public int CompareTo(MessageFile m)
        {
            return this.FilePath.CompareTo(m.FilePath);
        }

        public override bool Equals(object obj)
        {
            if (obj is MessageFile)
                return this.FilePath == ((MessageFile)obj).FilePath;
            return false;
        }
    }

    // Used to stor information about fields belonging to block in MsgView.
    public class BlockInfo 
    {
        public int MessageType { get; set; }
        public int StartingRow { get; set; } // Starting row of of block, including enabling Control.
        public int EndingRow { get; set; } // Last row occupied by block element.
        public int FieldsPerBlock { get; set; } // How many fields each block element has.
        public int InsertRow { get; set; } // Row to insert new block fields on.

        public BlockInfo() { }
        public BlockInfo(int StartingRow, int EndingRow, int FieldsPerBlock, int InsertRow)
        {
            this.StartingRow = StartingRow;
            this.EndingRow = EndingRow;
            this.FieldsPerBlock = FieldsPerBlock;
            this.InsertRow = InsertRow;
        }
    }

    public class FieldType
    {
        public string name { get; private set; }
        public ArrayList values;

        public FieldType(string name)
        {
            this.name = name;
            values = new ArrayList();
        }
    }

    public class BlockType
    {
        public string name { get; private set; }
        public ArrayList values;

        public BlockType(string name)
        {
            this.name = name;
            values = new ArrayList();
        }
    }
}
