﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;

namespace TCCCom
{
    public static class Safety
    {
        [DllImport("srprot.dll")]
        public static extern void crc32_init();
        [DllImport("srprot.dll")]
        public static extern uint crc32_update(uint seed, byte[] Data, ushort Length);

        public static Byte[] TSRef { set; get; }
        public static Byte[] LastTSSender { set; get; }
        public const Byte STX = 0x02;

        private static Byte[] BCCTable = // Copied from Srprot.c in StatSim
          { 0x00, 0x85, 0x8F, 0x0A, 0x9B, 0x1E, 0x14, 0x91, 0xB3, 0x36,
          0x3C, 0xB9, 0x28, 0xAD, 0xA7, 0x22, 0xE3, 0x66, 0x6C, 0xE9,
          0x78, 0xFD, 0xF7, 0x72, 0x50, 0xD5, 0xDF, 0x5A, 0xCB, 0x4E,
          0x44, 0xC1, 0x43, 0xC6, 0xCC, 0x49, 0xD8, 0x5D, 0x57, 0xD2,
          0xF0, 0x75, 0x7F, 0xFA, 0x6B, 0xEE, 0xE4, 0x61, 0xA0, 0x25,
          0x2F, 0xAA, 0x3B, 0xBE, 0xB4, 0x31, 0x13, 0x96, 0x9C, 0x19,
          0x88, 0x0D, 0x07, 0x82, 0x86, 0x03, 0x09, 0x8C, 0x1D, 0x98,
          0x92, 0x17, 0x35, 0xB0, 0xBA, 0x3F, 0xAE, 0x2B, 0x21, 0xA4,
          0x65, 0xE0, 0xEA, 0x6F, 0xFE, 0x7B, 0x71, 0xF4, 0xD6, 0x53,
          0x59, 0xDC, 0x4D, 0xC8, 0xC2, 0x47, 0xC5, 0x40, 0x4A, 0xCF,
          0x5E, 0xDB, 0xD1, 0x54, 0x76, 0xF3, 0xF9, 0x7C, 0xED, 0x68,
          0x62, 0xE7, 0x26, 0xA3, 0xA9, 0x2C, 0xBD, 0x38, 0x32, 0xB7,
          0x95, 0x10, 0x1A, 0x9F, 0x0E, 0x8B, 0x81, 0x04, 0x89, 0x0C,
          0x06, 0x83, 0x12, 0x97, 0x9D, 0x18, 0x3A, 0xBF, 0xB5, 0x30,
          0xA1, 0x24, 0x2E, 0xAB, 0x6A, 0xEF, 0xE5, 0x60, 0xF1, 0x74,
          0x7E, 0xFB, 0xD9, 0x5C, 0x56, 0xD3, 0x42, 0xC7, 0xCD, 0x48,
          0xCA, 0x4F, 0x45, 0xC0, 0x51, 0xD4, 0xDE, 0x5B, 0x79, 0xFC,
          0xF6, 0x73, 0xE2, 0x67, 0x6D, 0xE8, 0x29, 0xAC, 0xA6, 0x23,
          0xB2, 0x37, 0x3D, 0xB8, 0x9A, 0x1F, 0x15, 0x90, 0x01, 0x84,
          0x8E, 0x0B, 0x0F, 0x8A, 0x80, 0x05, 0x94, 0x11, 0x1B, 0x9E,
          0xBC, 0x39, 0x33, 0xB6, 0x27, 0xA2, 0xA8, 0x2D, 0xEC, 0x69,
          0x63, 0xE6, 0x77, 0xF2, 0xF8, 0x7D, 0x5F, 0xDA, 0xD0, 0x55,
          0xC4, 0x41, 0x4B, 0xCE, 0x4C, 0xC9, 0xC3, 0x46, 0xD7, 0x52,
          0x58, 0xDD, 0xFF, 0x7A, 0x70, 0xF5, 0x64, 0xE1, 0xEB, 0x6E,
          0xAF, 0x2A, 0x20, 0xA5, 0x34, 0xB1, 0xBB, 0x3E, 0x1C, 0x99,
          0x93, 0x16, 0x87, 0x02, 0x08, 0x8D };



        public static Byte[] AppendSafetyHeader(Byte[] Data, Byte ID, bool BCCFault, bool CRCFault, bool TSRefDecFault, bool TSRefIncFault, bool TSSenderDecFault, bool TSSenderReuse, bool IDFault)
        {
            Random Ran = new Random();
            Byte[] Complete = new Byte[7];

            if (IDFault)
                ID = (Byte)(ID + (Byte)Ran.Next(1, 256));
            Complete[0] = ID;

            // Length
            Byte[] Len = BitConverter.GetBytes((ushort)Data.Length);
            if (BitConverter.IsLittleEndian)
                Array.Reverse(Len);
            Array.Copy(Len, 0, Complete, 1, 2);


            // Timestamps
            if (!TSSenderReuse)
            {
                long TS = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                TS = TS >> 4;
                LastTSSender = BitConverter.GetBytes(TS);
            }
            else if (TSSenderDecFault)
            {
                LastTSSender = BitConverter.GetBytes(BitConverter.ToInt64(LastTSSender, 0) - 100);
            }

            Byte[] TSSenderOutput = new Byte[8];
            Byte[] TSRefOutput = new Byte[2];

            Array.Copy(LastTSSender, TSSenderOutput, 8);
            Array.Copy(TSRef, TSRefOutput, 2);

            if (TSRefDecFault)
            {
                TSRefOutput = BitConverter.GetBytes((UInt16)(BitConverter.ToUInt16(TSRefOutput, 0) - 100));
            }
            else if (TSRefIncFault)
            {
                TSRefOutput = BitConverter.GetBytes((UInt16)(BitConverter.ToUInt16(TSRefOutput, 0) + 100));
            }

            if (BitConverter.IsLittleEndian)
            {
                Array.Reverse(TSSenderOutput);
                Array.Reverse(TSRefOutput);
            }
            Array.Copy(TSSenderOutput, 6, Complete, 3, 2);

            Array.Copy(TSRefOutput, 0, Complete, 5, 2);

            Complete = Complete.Concat(Data).ToArray<Byte>();

            // CRC32
            uint Crc32 = CRC_Calc(Complete, (uint)0);
            if (CRCFault)
                Crc32 = (uint)(Crc32 + (uint)Ran.Next(1, Int32.MaxValue));
            Byte[] Crc32Bytes = BitConverter.GetBytes(Crc32);
            if (BitConverter.IsLittleEndian)
                Array.Reverse(Crc32Bytes);
            Complete = Complete.Concat(Crc32Bytes).ToArray<Byte>();

            Byte[] STX = { Safety.STX };
            Complete = STX.Concat(Complete).ToArray<Byte>();

            // BCC
            Byte BCC = BCC_Calc(Complete, 0);
            if (BCCFault)
                BCC = (Byte)(BCC + (Byte)Ran.Next(1, 256));
            Byte[] BCCArray = { BCC };
            Complete = Complete.Concat(BCCArray).ToArray<Byte>();

            return Complete;
        }

        public static uint CRC_Calc(Byte[] Data, uint Seed)
        {
            uint Crc32 = crc32_update(Seed, Data, (ushort)Data.Length); // External C call
            return Crc32;
        }

        public static Byte BCC_Calc(Byte[] Data, Byte Seed)
        {
            // There is no example of this in the
            // protocol paper so this has to be tested
            // in some way
            foreach (Byte b in Data)
                Seed = BCCTable[Seed ^ b];
            return Seed;
        }

        public static void Init()
        {
            crc32_init(); // External C call
            TSRef = new Byte[2];
            if (BitConverter.IsLittleEndian)
            {
                TSRef[0] = 0x65;
                TSRef[1] = 0x56;
            }
            else
            {
                TSRef[0] = 0x56;
                TSRef[1] = 0x65;
            }
            LastTSSender = new Byte[8];
        }

        public static void SetTSRef(Byte[] Ref)
        {
            TSRef = Ref;
        }
    }


}
