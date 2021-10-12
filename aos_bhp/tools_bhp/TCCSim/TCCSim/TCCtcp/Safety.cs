/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2014-07-18    Hidaji      Added site ID, adjusted the position of radio header elements 
*                           based on new radio protcol
* 2014-08-19    Bo H        Backward compatibility with previous protocol (no site ID)                          
* 2016-06-27    akushwah    upgraded the TCCSim for BHP Project
* 2017-03-22    marlundg    Updates related to new CRC64 calculation
*******************************************************************************/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using System.Threading;

namespace TCCtcp
{
    public static class Safety
    {
        //[DllImport("srprot.dll", CharSet=CharSet.Unicode, CallingConvention=CallingConvention.Cdecl)]
        //public static extern void crc32_init();
        //[DllImport("srprot.dll", CharSet=CharSet.Unicode, CallingConvention=CallingConvention.Cdecl)]
        //public static extern uint crc32_update(uint seed, byte[] Data, ushort Length);

        private const UInt64 CRC_INIT_VAL = 0x989626B1E8F2D6F0;
        private const UInt64 CRC_INIT_VAL_CENTRAL = 0xffffffffffffffffUL;
        
        // Length of messsage-data chunks
        public const UInt16 maxLengthOfMessageChunks = 4000;

        // Expected Header length includes STX,ID,Site ID,Region ID, Len, T_Sender & T_Ref's Size
        public const Byte expectedHeaderLength = 11;

        private static UInt64[] crcTable = new UInt64[256]
        {
            0x0000000000000000UL,0x843EED14D357E50DUL,
            0x8C43373D75F82F17UL,0x087DDA29A6AFCA1AUL,
            0x9CB8836E38A7BB23UL,0x18866E7AEBF05E2EUL,
            0x10FBB4534D5F9434UL,0x94C559479E087139UL,
            0xBD4FEBC8A218934BUL,0x397106DC714F7646UL,
            0x310CDCF5D7E0BC5CUL,0xB53231E104B75951UL,
            0x21F768A69ABF2868UL,0xA5C985B249E8CD65UL,
            0xADB45F9BEF47077FUL,0x298AB28F3C10E272UL,
            0xFEA13A859766C39BUL,0x7A9FD79144312696UL,
            0x72E20DB8E29EEC8CUL,0xF6DCE0AC31C90981UL,
            0x6219B9EBAFC178B8UL,0xE62754FF7C969DB5UL,
            0xEE5A8ED6DA3957AFUL,0x6A6463C2096EB2A2UL,
            0x43EED14D357E50D0UL,0xC7D03C59E629B5DDUL,
            0xCFADE67040867FC7UL,0x4B930B6493D19ACAUL,
            0xDF5652230DD9EBF3UL,0x5B68BF37DE8E0EFEUL,
            0x5315651E7821C4E4UL,0xD72B880AAB7621E9UL,
            0x797C981FFD9A623BUL,0xFD42750B2ECD8736UL,
            0xF53FAF2288624D2CUL,0x710142365B35A821UL,
            0xE5C41B71C53DD918UL,0x61FAF665166A3C15UL,
            0x69872C4CB0C5F60FUL,0xEDB9C15863921302UL,
            0xC43373D75F82F170UL,0x400D9EC38CD5147DUL,
            0x487044EA2A7ADE67UL,0xCC4EA9FEF92D3B6AUL,
            0x588BF0B967254A53UL,0xDCB51DADB472AF5EUL,
            0xD4C8C78412DD6544UL,0x50F62A90C18A8049UL,
            0x87DDA29A6AFCA1A0UL,0x03E34F8EB9AB44ADUL,
            0x0B9E95A71F048EB7UL,0x8FA078B3CC536BBAUL,
            0x1B6521F4525B1A83UL,0x9F5BCCE0810CFF8EUL,
            0x972616C927A33594UL,0x1318FBDDF4F4D099UL,
            0x3A924952C8E432EBUL,0xBEACA4461BB3D7E6UL,
            0xB6D17E6FBD1C1DFCUL,0x32EF937B6E4BF8F1UL,
            0xA62ACA3CF04389C8UL,0x2214272823146CC5UL,
            0x2A69FD0185BBA6DFUL,0xAE57101556EC43D2UL,
            0xF2F9303FFB34C476UL,0x76C7DD2B2863217BUL,
            0x7EBA07028ECCEB61UL,0xFA84EA165D9B0E6CUL,
            0x6E41B351C3937F55UL,0xEA7F5E4510C49A58UL,
            0xE202846CB66B5042UL,0x663C6978653CB54FUL,
            0x4FB6DBF7592C573DUL,0xCB8836E38A7BB230UL,
            0xC3F5ECCA2CD4782AUL,0x47CB01DEFF839D27UL,
            0xD30E5899618BEC1EUL,0x5730B58DB2DC0913UL,
            0x5F4D6FA41473C309UL,0xDB7382B0C7242604UL,
            0x0C580ABA6C5207EDUL,0x8866E7AEBF05E2E0UL,
            0x801B3D8719AA28FAUL,0x0425D093CAFDCDF7UL,
            0x90E089D454F5BCCEUL,0x14DE64C087A259C3UL,
            0x1CA3BEE9210D93D9UL,0x989D53FDF25A76D4UL,
            0xB117E172CE4A94A6UL,0x35290C661D1D71ABUL,
            0x3D54D64FBBB2BBB1UL,0xB96A3B5B68E55EBCUL,
            0x2DAF621CF6ED2F85UL,0xA9918F0825BACA88UL,
            0xA1EC552183150092UL,0x25D2B8355042E59FUL,
            0x8B85A82006AEA64DUL,0x0FBB4534D5F94340UL,
            0x07C69F1D7356895AUL,0x83F87209A0016C57UL,
            0x173D2B4E3E091D6EUL,0x9303C65AED5EF863UL,
            0x9B7E1C734BF13279UL,0x1F40F16798A6D774UL,
            0x36CA43E8A4B63506UL,0xB2F4AEFC77E1D00BUL,
            0xBA8974D5D14E1A11UL,0x3EB799C10219FF1CUL,
            0xAA72C0869C118E25UL,0x2E4C2D924F466B28UL,
            0x2631F7BBE9E9A132UL,0xA20F1AAF3ABE443FUL,
            0x752492A591C865D6UL,0xF11A7FB1429F80DBUL,
            0xF967A598E4304AC1UL,0x7D59488C3767AFCCUL,
            0xE99C11CBA96FDEF5UL,0x6DA2FCDF7A383BF8UL,
            0x65DF26F6DC97F1E2UL,0xE1E1CBE20FC014EFUL,
            0xC86B796D33D0F69DUL,0x4C559479E0871390UL,
            0x44284E504628D98AUL,0xC016A344957F3C87UL,
            0x54D3FA030B774DBEUL,0xD0ED1717D820A8B3UL,
            0xD890CD3E7E8F62A9UL,0x5CAE202AADD887A4UL,
            0x61CC8D6B253E6DE1UL,0xE5F2607FF66988ECUL,
            0xED8FBA5650C642F6UL,0x69B157428391A7FBUL,
            0xFD740E051D99D6C2UL,0x794AE311CECE33CFUL,
            0x713739386861F9D5UL,0xF509D42CBB361CD8UL,
            0xDC8366A38726FEAAUL,0x58BD8BB754711BA7UL,
            0x50C0519EF2DED1BDUL,0xD4FEBC8A218934B0UL,
            0x403BE5CDBF814589UL,0xC40508D96CD6A084UL,
            0xCC78D2F0CA796A9EUL,0x48463FE4192E8F93UL,
            0x9F6DB7EEB258AE7AUL,0x1B535AFA610F4B77UL,
            0x132E80D3C7A0816DUL,0x97106DC714F76460UL,
            0x03D534808AFF1559UL,0x87EBD99459A8F054UL,
            0x8F9603BDFF073A4EUL,0x0BA8EEA92C50DF43UL,
            0x22225C2610403D31UL,0xA61CB132C317D83CUL,
            0xAE616B1B65B81226UL,0x2A5F860FB6EFF72BUL,
            0xBE9ADF4828E78612UL,0x3AA4325CFBB0631FUL,
            0x32D9E8755D1FA905UL,0xB6E705618E484C08UL,
            0x18B01574D8A40FDAUL,0x9C8EF8600BF3EAD7UL,
            0x94F32249AD5C20CDUL,0x10CDCF5D7E0BC5C0UL,
            0x8408961AE003B4F9UL,0x00367B0E335451F4UL,
            0x084BA12795FB9BEEUL,0x8C754C3346AC7EE3UL,
            0xA5FFFEBC7ABC9C91UL,0x21C113A8A9EB799CUL,
            0x29BCC9810F44B386UL,0xAD822495DC13568BUL,
            0x39477DD2421B27B2UL,0xBD7990C6914CC2BFUL,
            0xB5044AEF37E308A5UL,0x313AA7FBE4B4EDA8UL,
            0xE6112FF14FC2CC41UL,0x622FC2E59C95294CUL,
            0x6A5218CC3A3AE356UL,0xEE6CF5D8E96D065BUL,
            0x7AA9AC9F77657762UL,0xFE97418BA432926FUL,
            0xF6EA9BA2029D5875UL,0x72D476B6D1CABD78UL,
            0x5B5EC439EDDA5F0AUL,0xDF60292D3E8DBA07UL,
            0xD71DF3049822701DUL,0x53231E104B759510UL,
            0xC7E64757D57DE429UL,0x43D8AA43062A0124UL,
            0x4BA5706AA085CB3EUL,0xCF9B9D7E73D22E33UL,
            0x9335BD54DE0AA997UL,0x170B50400D5D4C9AUL,
            0x1F768A69ABF28680UL,0x9B48677D78A5638DUL,
            0x0F8D3E3AE6AD12B4UL,0x8BB3D32E35FAF7B9UL,
            0x83CE090793553DA3UL,0x07F0E4134002D8AEUL,
            0x2E7A569C7C123ADCUL,0xAA44BB88AF45DFD1UL,
            0xA23961A109EA15CBUL,0x26078CB5DABDF0C6UL,
            0xB2C2D5F244B581FFUL,0x36FC38E697E264F2UL,
            0x3E81E2CF314DAEE8UL,0xBABF0FDBE21A4BE5UL,
            0x6D9487D1496C6A0CUL,0xE9AA6AC59A3B8F01UL,
            0xE1D7B0EC3C94451BUL,0x65E95DF8EFC3A016UL,
            0xF12C04BF71CBD12FUL,0x7512E9ABA29C3422UL,
            0x7D6F33820433FE38UL,0xF951DE96D7641B35UL,
            0xD0DB6C19EB74F947UL,0x54E5810D38231C4AUL,
            0x5C985B249E8CD650UL,0xD8A6B6304DDB335DUL,
            0x4C63EF77D3D34264UL,0xC85D02630084A769UL,
            0xC020D84AA62B6D73UL,0x441E355E757C887EUL,
            0xEA49254B2390CBACUL,0x6E77C85FF0C72EA1UL,
            0x660A12765668E4BBUL,0xE234FF62853F01B6UL,
            0x76F1A6251B37708FUL,0xF2CF4B31C8609582UL,
            0xFAB291186ECF5F98UL,0x7E8C7C0CBD98BA95UL,
            0x5706CE83818858E7UL,0xD338239752DFBDEAUL,
            0xDB45F9BEF47077F0UL,0x5F7B14AA272792FDUL,
            0xCBBE4DEDB92FE3C4UL,0x4F80A0F96A7806C9UL,
            0x47FD7AD0CCD7CCD3UL,0xC3C397C41F8029DEUL,
            0x14E81FCEB4F60837UL,0x90D6F2DA67A1ED3AUL,
            0x98AB28F3C10E2720UL,0x1C95C5E71259C22DUL,
            0x88509CA08C51B314UL,0x0C6E71B45F065619UL,
            0x0413AB9DF9A99C03UL,0x802D46892AFE790EUL,
            0xA9A7F40616EE9B7CUL,0x2D991912C5B97E71UL,
            0x25E4C33B6316B46BUL,0xA1DA2E2FB0415166UL,
            0x351F77682E49205FUL,0xB1219A7CFD1EC552UL,
            0xB95C40555BB10F48UL,0x3D62AD4188E6EA45UL,
        };

        /// <summary>
        /// Calculates a CRC64 for the passes data block and length
        /// </summary>
        /// <param name="dataBlk"></param>
        /// <param name="dataBlkSize"></param>
        public static ulong calculateCRC64(Byte[] dataBlk, UInt16 dataBlkSize)
        {
            UInt64 crc = CRC_INIT_VAL;
            UInt16 dataIndex = 0;

            while (dataIndex < dataBlkSize)
            {
                Int64 temp = dataBlk[dataIndex] ^ (Byte)(crc >> 56);
                crc = crcTable[dataBlk[dataIndex] ^ (Byte)(crc >> 56)] ^ (crc << 8);
                dataIndex++;
            }
            return crc;
        }

        private static UInt64[] crcTableCentral = new UInt64[256]
        {
            0x0000000000000000UL,0x42F0E1EBA9EA3693UL,
            0x85E1C3D753D46D26UL,0xC711223CFA3E5BB5UL,
            0x493366450E42ECDFUL,0x0BC387AEA7A8DA4CUL,
            0xCCD2A5925D9681F9UL,0x8E224479F47CB76AUL,
            0x9266CC8A1C85D9BEUL,0xD0962D61B56FEF2DUL,
            0x17870F5D4F51B498UL,0x5577EEB6E6BB820BUL,
            0xDB55AACF12C73561UL,0x99A54B24BB2D03F2UL,
            0x5EB4691841135847UL,0x1C4488F3E8F96ED4UL,
            0x663D78FF90E185EFUL,0x24CD9914390BB37CUL,
            0xE3DCBB28C335E8C9UL,0xA12C5AC36ADFDE5AUL,
            0x2F0E1EBA9EA36930UL,0x6DFEFF5137495FA3UL,
            0xAAEFDD6DCD770416UL,0xE81F3C86649D3285UL,
            0xF45BB4758C645C51UL,0xB6AB559E258E6AC2UL,
            0x71BA77A2DFB03177UL,0x334A9649765A07E4UL,
            0xBD68D2308226B08EUL,0xFF9833DB2BCC861DUL,
            0x388911E7D1F2DDA8UL,0x7A79F00C7818EB3BUL,
            0xCC7AF1FF21C30BDEUL,0x8E8A101488293D4DUL,
            0x499B3228721766F8UL,0x0B6BD3C3DBFD506BUL,
            0x854997BA2F81E701UL,0xC7B97651866BD192UL,
            0x00A8546D7C558A27UL,0x4258B586D5BFBCB4UL,
            0x5E1C3D753D46D260UL,0x1CECDC9E94ACE4F3UL,
            0xDBFDFEA26E92BF46UL,0x990D1F49C77889D5UL,
            0x172F5B3033043EBFUL,0x55DFBADB9AEE082CUL,
            0x92CE98E760D05399UL,0xD03E790CC93A650AUL,
            0xAA478900B1228E31UL,0xE8B768EB18C8B8A2UL,
            0x2FA64AD7E2F6E317UL,0x6D56AB3C4B1CD584UL,
            0xE374EF45BF6062EEUL,0xA1840EAE168A547DUL,
            0x66952C92ECB40FC8UL,0x2465CD79455E395BUL,
            0x3821458AADA7578FUL,0x7AD1A461044D611CUL,
            0xBDC0865DFE733AA9UL,0xFF3067B657990C3AUL,
            0x711223CFA3E5BB50UL,0x33E2C2240A0F8DC3UL,
            0xF4F3E018F031D676UL,0xB60301F359DBE0E5UL,
            0xDA050215EA6C212FUL,0x98F5E3FE438617BCUL,
            0x5FE4C1C2B9B84C09UL,0x1D14202910527A9AUL,
            0x93366450E42ECDF0UL,0xD1C685BB4DC4FB63UL,
            0x16D7A787B7FAA0D6UL,0x5427466C1E109645UL,
            0x4863CE9FF6E9F891UL,0x0A932F745F03CE02UL,
            0xCD820D48A53D95B7UL,0x8F72ECA30CD7A324UL,
            0x0150A8DAF8AB144EUL,0x43A04931514122DDUL,
            0x84B16B0DAB7F7968UL,0xC6418AE602954FFBUL,
            0xBC387AEA7A8DA4C0UL,0xFEC89B01D3679253UL,
            0x39D9B93D2959C9E6UL,0x7B2958D680B3FF75UL,
            0xF50B1CAF74CF481FUL,0xB7FBFD44DD257E8CUL,
            0x70EADF78271B2539UL,0x321A3E938EF113AAUL,
            0x2E5EB66066087D7EUL,0x6CAE578BCFE24BEDUL,
            0xABBF75B735DC1058UL,0xE94F945C9C3626CBUL,
            0x676DD025684A91A1UL,0x259D31CEC1A0A732UL,
            0xE28C13F23B9EFC87UL,0xA07CF2199274CA14UL,
            0x167FF3EACBAF2AF1UL,0x548F120162451C62UL,
            0x939E303D987B47D7UL,0xD16ED1D631917144UL,
            0x5F4C95AFC5EDC62EUL,0x1DBC74446C07F0BDUL,
            0xDAAD56789639AB08UL,0x985DB7933FD39D9BUL,
            0x84193F60D72AF34FUL,0xC6E9DE8B7EC0C5DCUL,
            0x01F8FCB784FE9E69UL,0x43081D5C2D14A8FAUL,
            0xCD2A5925D9681F90UL,0x8FDAB8CE70822903UL,
            0x48CB9AF28ABC72B6UL,0x0A3B7B1923564425UL,
            0x70428B155B4EAF1EUL,0x32B26AFEF2A4998DUL,
            0xF5A348C2089AC238UL,0xB753A929A170F4ABUL,
            0x3971ED50550C43C1UL,0x7B810CBBFCE67552UL,
            0xBC902E8706D82EE7UL,0xFE60CF6CAF321874UL,
            0xE224479F47CB76A0UL,0xA0D4A674EE214033UL,
            0x67C58448141F1B86UL,0x253565A3BDF52D15UL,
            0xAB1721DA49899A7FUL,0xE9E7C031E063ACECUL,
            0x2EF6E20D1A5DF759UL,0x6C0603E6B3B7C1CAUL,
            0xF6FAE5C07D3274CDUL,0xB40A042BD4D8425EUL,
            0x731B26172EE619EBUL,0x31EBC7FC870C2F78UL,
            0xBFC9838573709812UL,0xFD39626EDA9AAE81UL,
            0x3A28405220A4F534UL,0x78D8A1B9894EC3A7UL,
            0x649C294A61B7AD73UL,0x266CC8A1C85D9BE0UL,
            0xE17DEA9D3263C055UL,0xA38D0B769B89F6C6UL,
            0x2DAF4F0F6FF541ACUL,0x6F5FAEE4C61F773FUL,
            0xA84E8CD83C212C8AUL,0xEABE6D3395CB1A19UL,
            0x90C79D3FEDD3F122UL,0xD2377CD44439C7B1UL,
            0x15265EE8BE079C04UL,0x57D6BF0317EDAA97UL,
            0xD9F4FB7AE3911DFDUL,0x9B041A914A7B2B6EUL,
            0x5C1538ADB04570DBUL,0x1EE5D94619AF4648UL,
            0x02A151B5F156289CUL,0x4051B05E58BC1E0FUL,
            0x87409262A28245BAUL,0xC5B073890B687329UL,
            0x4B9237F0FF14C443UL,0x0962D61B56FEF2D0UL,
            0xCE73F427ACC0A965UL,0x8C8315CC052A9FF6UL,
            0x3A80143F5CF17F13UL,0x7870F5D4F51B4980UL,
            0xBF61D7E80F251235UL,0xFD913603A6CF24A6UL,
            0x73B3727A52B393CCUL,0x31439391FB59A55FUL,
            0xF652B1AD0167FEEAUL,0xB4A25046A88DC879UL,
            0xA8E6D8B54074A6ADUL,0xEA16395EE99E903EUL,
            0x2D071B6213A0CB8BUL,0x6FF7FA89BA4AFD18UL,
            0xE1D5BEF04E364A72UL,0xA3255F1BE7DC7CE1UL,
            0x64347D271DE22754UL,0x26C49CCCB40811C7UL,
            0x5CBD6CC0CC10FAFCUL,0x1E4D8D2B65FACC6FUL,
            0xD95CAF179FC497DAUL,0x9BAC4EFC362EA149UL,
            0x158E0A85C2521623UL,0x577EEB6E6BB820B0UL,
            0x906FC95291867B05UL,0xD29F28B9386C4D96UL,
            0xCEDBA04AD0952342UL,0x8C2B41A1797F15D1UL,
            0x4B3A639D83414E64UL,0x09CA82762AAB78F7UL,
            0x87E8C60FDED7CF9DUL,0xC51827E4773DF90EUL,
            0x020905D88D03A2BBUL,0x40F9E43324E99428UL,
            0x2CFFE7D5975E55E2UL,0x6E0F063E3EB46371UL,
            0xA91E2402C48A38C4UL,0xEBEEC5E96D600E57UL,
            0x65CC8190991CB93DUL,0x273C607B30F68FAEUL,
            0xE02D4247CAC8D41BUL,0xA2DDA3AC6322E288UL,
            0xBE992B5F8BDB8C5CUL,0xFC69CAB42231BACFUL,
            0x3B78E888D80FE17AUL,0x7988096371E5D7E9UL,
            0xF7AA4D1A85996083UL,0xB55AACF12C735610UL,
            0x724B8ECDD64D0DA5UL,0x30BB6F267FA73B36UL,
            0x4AC29F2A07BFD00DUL,0x08327EC1AE55E69EUL,
            0xCF235CFD546BBD2BUL,0x8DD3BD16FD818BB8UL,
            0x03F1F96F09FD3CD2UL,0x41011884A0170A41UL,
            0x86103AB85A2951F4UL,0xC4E0DB53F3C36767UL,
            0xD8A453A01B3A09B3UL,0x9A54B24BB2D03F20UL,
            0x5D45907748EE6495UL,0x1FB5719CE1045206UL,
            0x919735E51578E56CUL,0xD367D40EBC92D3FFUL,
            0x1476F63246AC884AUL,0x568617D9EF46BED9UL,
            0xE085162AB69D5E3CUL,0xA275F7C11F7768AFUL,
            0x6564D5FDE549331AUL,0x279434164CA30589UL,
            0xA9B6706FB8DFB2E3UL,0xEB46918411358470UL,
            0x2C57B3B8EB0BDFC5UL,0x6EA7525342E1E956UL,
            0x72E3DAA0AA188782UL,0x30133B4B03F2B111UL,
            0xF7021977F9CCEAA4UL,0xB5F2F89C5026DC37UL,
            0x3BD0BCE5A45A6B5DUL,0x79205D0E0DB05DCEUL,
            0xBE317F32F78E067BUL,0xFCC19ED95E6430E8UL,
            0x86B86ED5267CDBD3UL,0xC4488F3E8F96ED40UL,
            0x0359AD0275A8B6F5UL,0x41A94CE9DC428066UL,
            0xCF8B0890283E370CUL,0x8D7BE97B81D4019FUL,
            0x4A6ACB477BEA5A2AUL,0x089A2AACD2006CB9UL,
            0x14DEA25F3AF9026DUL,0x562E43B4931334FEUL,
            0x913F6188692D6F4BUL,0xD3CF8063C0C759D8UL,
            0x5DEDC41A34BBEEB2UL,0x1F1D25F19D51D821UL,
            0xD80C07CD676F8394UL,0x9AFCE626CE85B507UL
        };


        /// <summary>
        /// Calculates a CRC64 for central TCC for the passes data block and length
        /// </summary>
        /// <param name="dataBlk"></param>
        /// <param name="dataBlkSize"></param>
        public static ulong calculateCRC64Central(Byte[] dataBlk, UInt16 dataBlkSize)
        {
            UInt64 crc = CRC_INIT_VAL_CENTRAL;
            UInt16 dataIndex = 0;

            while (dataIndex < dataBlkSize)
            {
                Int64 temp = dataBlk[dataIndex] ^ (Byte)(crc >> 56);
                crc = crcTableCentral[dataBlk[dataIndex] ^ (Byte)(crc >> 56)] ^ (crc << 8);
                dataIndex++;
            }
            // final xor with -1
            crc = crc ^ 0xffffffffffffffffUL;
            return crc;
        }

        /// <summary>
        /// Verify the crc for string "123456789" with the specified value of FFFIS TCC-AOS section 4.2.2
        /// </summary>
        public static bool crcSelfTestCentral()
        {
            Byte[] dataBlk = new Byte[]  { 49, 50, 51, 52, 53, 54, 55, 56, 57 }; // "123456789"
            ulong crc = calculateCRC64Central(dataBlk, 9);
            ulong expect = 0x62EC59E3F1A4F00AUL;
            return (crc == expect);
        }

        // Stores the last timestamp received, timestamps are stored in host byte order
        public static Byte[] TSRef { set; get; }
        // Stores the last timestamp sent
        public static Byte[] LastTSSender { set; get; }
        
        public const Byte STX = 0x02;

        /// <summary>
        /// Takes a message and appends the safetyhead.
        /// Injects fault if told so.
        /// </summary>
        /// <param name="Data"></param>
        /// <param name="ID"></param>
        /// <param name="CRCFault"></param>
        /// <param name="TSRefDecFault"></param>
        /// <param name="TSRefIncFault"></param>
        /// <param name="TSSenderDecFault"></param>
        /// <param name="TSSenderReuse"></param>
        /// <param name="IDFault"></param>
        /// <returns></returns>
        public static Byte[] AppendSafetyHeader(Byte[] Data, Int16 ID, byte siteID, byte regionID, crcType crcToUse, bool CRCFault, bool TSRefDecFault, bool TSRefIncFault, bool TSSenderDecFault, bool TSSenderReuse, bool IDFault, bool siteIDFault, bool regionIDFault)
        {

            //No backward compatibilty 
            //if (siteID == 0)
            //    ExpectedHeaderLength = 8;   // Previous protocol without site ID

            if (Data == null || Data.Length == 0)
                return new Byte[0];
            if (TSRef == null)
                InitTSref();

            Random Ran = new Random();

            // Without STX
            Byte[] Complete = new Byte[expectedHeaderLength-1];

            // Sets ID
            if (siteID == 0)
            {                       // Previous protocol, Radio ID is byte
                if (IDFault)
                {
                    ID = (Byte)(ID + (Byte)Ran.Next(1, 256));
                }
                Complete[0] = (Byte)ID;
            }
            else
            {
                if (IDFault)
                {
                    ID = (Int16)(ID + (Int16)Ran.Next(1, 65536));
                }

                Byte[] trainID = BitConverter.GetBytes((ushort)ID);
                if (BitConverter.IsLittleEndian)
                    Array.Reverse(trainID);

                Array.Copy(trainID, 0, Complete, 0, 2);
            }

            if (siteID != 0)
            {
                // Sets site
                if (siteIDFault)
                    siteID = (Byte)(siteID + (Byte)Ran.Next(1, 256));
                Complete[2] = (byte)siteID;
            }

            if (regionID != 0)
            {
                // Sets region
                if (regionIDFault)
                    regionID = (Byte)(regionID + (Byte)Ran.Next(1, 256));
                Complete[3] = (byte)regionID;
            }

            // Calculates length and sets it
            Byte[] Len = BitConverter.GetBytes((ushort)Data.Length);
            if (BitConverter.IsLittleEndian)
                Array.Reverse(Len);
            Array.Copy(Len, 0, Complete, expectedHeaderLength - 7, 2);
          
            // Calculates new timestamp
            if (!TSSenderReuse)
            {
                long TS = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                TS = TS >> 4;
                LastTSSender = BitConverter.GetBytes(TS);
            }
            // Uses previous timestamp and decreases it
            else if (TSSenderDecFault)
            {
                LastTSSender = BitConverter.GetBytes(BitConverter.ToInt64(LastTSSender, 0) - 100);
            }
            // If none of the cases above matched, the previous timestamp will be reused.

            Byte[] TSSenderOutput = new Byte[8];
            Byte[] TSRefOutput = new Byte[2];

            Array.Copy(LastTSSender, TSSenderOutput, 8);
            Array.Copy(TSRef, TSRefOutput, 2);

            // Decrease previously received timestamp and uses it as reference timestamp
            if (TSRefDecFault)
            {
                TSRefOutput = BitConverter.GetBytes((UInt16)(BitConverter.ToUInt16(TSRefOutput, 0) - 100));
            }
            // Increase previously received timestamp and uses it as reference timestamp
            else if (TSRefIncFault)
            {
                TSRefOutput = BitConverter.GetBytes((UInt16)(BitConverter.ToUInt16(TSRefOutput, 0) + 100));
            }

            if (BitConverter.IsLittleEndian)
            {
                Array.Reverse(TSSenderOutput);
                Array.Reverse(TSRefOutput);
            }
            Array.Copy(TSSenderOutput, 6, Complete, expectedHeaderLength - 5, 2);

            Array.Copy(TSRefOutput, 0, Complete, expectedHeaderLength - 3, 2);

            // Concat rest of message
            Complete = Complete.Concat(Data).ToArray<Byte>();

            // Calculate the Message Chunks
            UInt16 messageChunks = (UInt16)(Data.Length / maxLengthOfMessageChunks);

            if (Data.Length % maxLengthOfMessageChunks != 0)
            {
                messageChunks += 1;
            }

            Byte[] CrcBeforeAppending = new Byte[0];

            for (int index = 0; index < messageChunks ; index++)
            {
                ulong currentCrc64 = 0;
                UInt16 crcLength = 0;
                UInt16 crcPosition = 0;

                if (index == 0U)
                {
                    // Start from position 0 (ID-field)
                    crcPosition = 0;

                    // Calculate length to use for CRC calculation, exclude 1 byte for STX that is not included in calculation.
                    crcLength =  (ushort)((expectedHeaderLength -1) + (((ushort)Data.Length < maxLengthOfMessageChunks) ? (ushort)Data.Length : maxLengthOfMessageChunks));
                }
                else 
                {
                    // Calculate start position in message to use for CRC calculation (Header-STX + number of already processed chunks).
                    crcPosition = (ushort)((expectedHeaderLength - 1) + (index * maxLengthOfMessageChunks));
                    
                    // Calculate length to use for CRC calculation, either rest of message or maxLengthOfMessageChunks.
                    crcLength = (ushort)(((index + 1U) == messageChunks) ? ((ushort)Data.Length - ((index * maxLengthOfMessageChunks))) : maxLengthOfMessageChunks);
                }

                // Calculate CRC64 for a certain portion of the message.
                if (crcToUse == crcType.crcTypeRegion)
                {
                    currentCrc64 = calculateCRC64(Complete.Skip(crcPosition).ToArray(), crcLength);
                }
                else
                {
                    currentCrc64 = calculateCRC64Central(Complete.Skip(crcPosition).ToArray(), crcLength);
                }

                if (CRCFault)
                    currentCrc64 = (uint)(currentCrc64 + (ulong)Ran.Next(1, Int32.MaxValue));
                Byte[] Crc64Bytes = BitConverter.GetBytes(currentCrc64);

                if (BitConverter.IsLittleEndian)
                    Array.Reverse(Crc64Bytes);
                CrcBeforeAppending = CrcBeforeAppending.Concat(Crc64Bytes).ToArray<Byte>();
            }

            // Concat rest of message with CRC
            Complete = Complete.Concat(CrcBeforeAppending).ToArray<Byte>();

            Byte[] STX = { Safety.STX };
            Complete = STX.Concat(Complete).ToArray<Byte>();

           return Complete;
        }

        /// <summary>
        /// Initiate the safety layer class
        /// </summary>
        public static void Init()
        {
            // First reference timestamp should be 0x5665 (network byte order)
            InitTSref();            
            LastTSSender = new Byte[8];
        }

        public static void InitTSref()
        {
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
        }

        public static void SetTSRef(Byte[] Ref)
        {
            TSRef = Ref;
        }
    }


}
