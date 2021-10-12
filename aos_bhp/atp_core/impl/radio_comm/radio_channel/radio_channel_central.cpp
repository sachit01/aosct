/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
*  DESCRIPTION:
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-11-10    keisele     Created
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include <vfw_identity.h>
#include <vfwChannel.h>
#include "atc_util.hpp"
#include "radio_channel_central.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_array.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/


/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/

namespace ATP
{
  namespace RadioCom
  {

    /**
    * crcTable for TCC Central 
    **/
    static const uint64_t crcTableCentral[256] =
    {
                                                    0x00000000UL, (static_cast<uint64_t>(0x42F0E1EBUL) << 32) + 0xA9EA3693UL,
      (static_cast<uint64_t>(0x85E1C3D7UL) << 32) + 0x53D46D26UL, (static_cast<uint64_t>(0xC711223CUL) << 32) + 0xFA3E5BB5UL,
      (static_cast<uint64_t>(0x49336645UL) << 32) + 0x0E42ECDFUL, (static_cast<uint64_t>(0x0BC387AEUL) << 32) + 0xA7A8DA4CUL,
      (static_cast<uint64_t>(0xCCD2A592UL) << 32) + 0x5D9681F9UL, (static_cast<uint64_t>(0x8E224479UL) << 32) + 0xF47CB76AUL,
      (static_cast<uint64_t>(0x9266CC8AUL) << 32) + 0x1C85D9BEUL, (static_cast<uint64_t>(0xD0962D61UL) << 32) + 0xB56FEF2DUL,
      (static_cast<uint64_t>(0x17870F5DUL) << 32) + 0x4F51B498UL, (static_cast<uint64_t>(0x5577EEB6UL) << 32) + 0xE6BB820BUL,
      (static_cast<uint64_t>(0xDB55AACFUL) << 32) + 0x12C73561UL, (static_cast<uint64_t>(0x99A54B24UL) << 32) + 0xBB2D03F2UL,
      (static_cast<uint64_t>(0x5EB46918UL) << 32) + 0x41135847UL, (static_cast<uint64_t>(0x1C4488F3UL) << 32) + 0xE8F96ED4UL,
      (static_cast<uint64_t>(0x663D78FFUL) << 32) + 0x90E185EFUL, (static_cast<uint64_t>(0x24CD9914UL) << 32) + 0x390BB37CUL,
      (static_cast<uint64_t>(0xE3DCBB28UL) << 32) + 0xC335E8C9UL, (static_cast<uint64_t>(0xA12C5AC3UL) << 32) + 0x6ADFDE5AUL,
      (static_cast<uint64_t>(0x2F0E1EBAUL) << 32) + 0x9EA36930UL, (static_cast<uint64_t>(0x6DFEFF51UL) << 32) + 0x37495FA3UL,
      (static_cast<uint64_t>(0xAAEFDD6DUL) << 32) + 0xCD770416UL, (static_cast<uint64_t>(0xE81F3C86UL) << 32) + 0x649D3285UL,
      (static_cast<uint64_t>(0xF45BB475UL) << 32) + 0x8C645C51UL, (static_cast<uint64_t>(0xB6AB559EUL) << 32) + 0x258E6AC2UL,
      (static_cast<uint64_t>(0x71BA77A2UL) << 32) + 0xDFB03177UL, (static_cast<uint64_t>(0x334A9649UL) << 32) + 0x765A07E4UL,
      (static_cast<uint64_t>(0xBD68D230UL) << 32) + 0x8226B08EUL, (static_cast<uint64_t>(0xFF9833DBUL) << 32) + 0x2BCC861DUL,
      (static_cast<uint64_t>(0x388911E7UL) << 32) + 0xD1F2DDA8UL, (static_cast<uint64_t>(0x7A79F00CUL) << 32) + 0x7818EB3BUL,
      (static_cast<uint64_t>(0xCC7AF1FFUL) << 32) + 0x21C30BDEUL, (static_cast<uint64_t>(0x8E8A1014UL) << 32) + 0x88293D4DUL,
      (static_cast<uint64_t>(0x499B3228UL) << 32) + 0x721766F8UL, (static_cast<uint64_t>(0x0B6BD3C3UL) << 32) + 0xDBFD506BUL,
      (static_cast<uint64_t>(0x854997BAUL) << 32) + 0x2F81E701UL, (static_cast<uint64_t>(0xC7B97651UL) << 32) + 0x866BD192UL,
      (static_cast<uint64_t>(0x00A8546DUL) << 32) + 0x7C558A27UL, (static_cast<uint64_t>(0x4258B586UL) << 32) + 0xD5BFBCB4UL,
      (static_cast<uint64_t>(0x5E1C3D75UL) << 32) + 0x3D46D260UL, (static_cast<uint64_t>(0x1CECDC9EUL) << 32) + 0x94ACE4F3UL,
      (static_cast<uint64_t>(0xDBFDFEA2UL) << 32) + 0x6E92BF46UL, (static_cast<uint64_t>(0x990D1F49UL) << 32) + 0xC77889D5UL,
      (static_cast<uint64_t>(0x172F5B30UL) << 32) + 0x33043EBFUL, (static_cast<uint64_t>(0x55DFBADBUL) << 32) + 0x9AEE082CUL,
      (static_cast<uint64_t>(0x92CE98E7UL) << 32) + 0x60D05399UL, (static_cast<uint64_t>(0xD03E790CUL) << 32) + 0xC93A650AUL,
      (static_cast<uint64_t>(0xAA478900UL) << 32) + 0xB1228E31UL, (static_cast<uint64_t>(0xE8B768EBUL) << 32) + 0x18C8B8A2UL,
      (static_cast<uint64_t>(0x2FA64AD7UL) << 32) + 0xE2F6E317UL, (static_cast<uint64_t>(0x6D56AB3CUL) << 32) + 0x4B1CD584UL,
      (static_cast<uint64_t>(0xE374EF45UL) << 32) + 0xBF6062EEUL, (static_cast<uint64_t>(0xA1840EAEUL) << 32) + 0x168A547DUL,
      (static_cast<uint64_t>(0x66952C92UL) << 32) + 0xECB40FC8UL, (static_cast<uint64_t>(0x2465CD79UL) << 32) + 0x455E395BUL,
      (static_cast<uint64_t>(0x3821458AUL) << 32) + 0xADA7578FUL, (static_cast<uint64_t>(0x7AD1A461UL) << 32) + 0x044D611CUL,
      (static_cast<uint64_t>(0xBDC0865DUL) << 32) + 0xFE733AA9UL, (static_cast<uint64_t>(0xFF3067B6UL) << 32) + 0x57990C3AUL,
      (static_cast<uint64_t>(0x711223CFUL) << 32) + 0xA3E5BB50UL, (static_cast<uint64_t>(0x33E2C224UL) << 32) + 0x0A0F8DC3UL,
      (static_cast<uint64_t>(0xF4F3E018UL) << 32) + 0xF031D676UL, (static_cast<uint64_t>(0xB60301F3UL) << 32) + 0x59DBE0E5UL,
      (static_cast<uint64_t>(0xDA050215UL) << 32) + 0xEA6C212FUL, (static_cast<uint64_t>(0x98F5E3FEUL) << 32) + 0x438617BCUL,
      (static_cast<uint64_t>(0x5FE4C1C2UL) << 32) + 0xB9B84C09UL, (static_cast<uint64_t>(0x1D142029UL) << 32) + 0x10527A9AUL,
      (static_cast<uint64_t>(0x93366450UL) << 32) + 0xE42ECDF0UL, (static_cast<uint64_t>(0xD1C685BBUL) << 32) + 0x4DC4FB63UL,
      (static_cast<uint64_t>(0x16D7A787UL) << 32) + 0xB7FAA0D6UL, (static_cast<uint64_t>(0x5427466CUL) << 32) + 0x1E109645UL,
      (static_cast<uint64_t>(0x4863CE9FUL) << 32) + 0xF6E9F891UL, (static_cast<uint64_t>(0x0A932F74UL) << 32) + 0x5F03CE02UL,
      (static_cast<uint64_t>(0xCD820D48UL) << 32) + 0xA53D95B7UL, (static_cast<uint64_t>(0x8F72ECA3UL) << 32) + 0x0CD7A324UL,
      (static_cast<uint64_t>(0x0150A8DAUL) << 32) + 0xF8AB144EUL, (static_cast<uint64_t>(0x43A04931UL) << 32) + 0x514122DDUL,
      (static_cast<uint64_t>(0x84B16B0DUL) << 32) + 0xAB7F7968UL, (static_cast<uint64_t>(0xC6418AE6UL) << 32) + 0x02954FFBUL,
      (static_cast<uint64_t>(0xBC387AEAUL) << 32) + 0x7A8DA4C0UL, (static_cast<uint64_t>(0xFEC89B01UL) << 32) + 0xD3679253UL,
      (static_cast<uint64_t>(0x39D9B93DUL) << 32) + 0x2959C9E6UL, (static_cast<uint64_t>(0x7B2958D6UL) << 32) + 0x80B3FF75UL,
      (static_cast<uint64_t>(0xF50B1CAFUL) << 32) + 0x74CF481FUL, (static_cast<uint64_t>(0xB7FBFD44UL) << 32) + 0xDD257E8CUL,
      (static_cast<uint64_t>(0x70EADF78UL) << 32) + 0x271B2539UL, (static_cast<uint64_t>(0x321A3E93UL) << 32) + 0x8EF113AAUL,
      (static_cast<uint64_t>(0x2E5EB660UL) << 32) + 0x66087D7EUL, (static_cast<uint64_t>(0x6CAE578BUL) << 32) + 0xCFE24BEDUL,
      (static_cast<uint64_t>(0xABBF75B7UL) << 32) + 0x35DC1058UL, (static_cast<uint64_t>(0xE94F945CUL) << 32) + 0x9C3626CBUL,
      (static_cast<uint64_t>(0x676DD025UL) << 32) + 0x684A91A1UL, (static_cast<uint64_t>(0x259D31CEUL) << 32) + 0xC1A0A732UL,
      (static_cast<uint64_t>(0xE28C13F2UL) << 32) + 0x3B9EFC87UL, (static_cast<uint64_t>(0xA07CF219UL) << 32) + 0x9274CA14UL,
      (static_cast<uint64_t>(0x167FF3EAUL) << 32) + 0xCBAF2AF1UL, (static_cast<uint64_t>(0x548F1201UL) << 32) + 0x62451C62UL,
      (static_cast<uint64_t>(0x939E303DUL) << 32) + 0x987B47D7UL, (static_cast<uint64_t>(0xD16ED1D6UL) << 32) + 0x31917144UL,
      (static_cast<uint64_t>(0x5F4C95AFUL) << 32) + 0xC5EDC62EUL, (static_cast<uint64_t>(0x1DBC7444UL) << 32) + 0x6C07F0BDUL,
      (static_cast<uint64_t>(0xDAAD5678UL) << 32) + 0x9639AB08UL, (static_cast<uint64_t>(0x985DB793UL) << 32) + 0x3FD39D9BUL,
      (static_cast<uint64_t>(0x84193F60UL) << 32) + 0xD72AF34FUL, (static_cast<uint64_t>(0xC6E9DE8BUL) << 32) + 0x7EC0C5DCUL,
      (static_cast<uint64_t>(0x01F8FCB7UL) << 32) + 0x84FE9E69UL, (static_cast<uint64_t>(0x43081D5CUL) << 32) + 0x2D14A8FAUL,
      (static_cast<uint64_t>(0xCD2A5925UL) << 32) + 0xD9681F90UL, (static_cast<uint64_t>(0x8FDAB8CEUL) << 32) + 0x70822903UL,
      (static_cast<uint64_t>(0x48CB9AF2UL) << 32) + 0x8ABC72B6UL, (static_cast<uint64_t>(0x0A3B7B19UL) << 32) + 0x23564425UL,
      (static_cast<uint64_t>(0x70428B15UL) << 32) + 0x5B4EAF1EUL, (static_cast<uint64_t>(0x32B26AFEUL) << 32) + 0xF2A4998DUL,
      (static_cast<uint64_t>(0xF5A348C2UL) << 32) + 0x089AC238UL, (static_cast<uint64_t>(0xB753A929UL) << 32) + 0xA170F4ABUL,
      (static_cast<uint64_t>(0x3971ED50UL) << 32) + 0x550C43C1UL, (static_cast<uint64_t>(0x7B810CBBUL) << 32) + 0xFCE67552UL,
      (static_cast<uint64_t>(0xBC902E87UL) << 32) + 0x06D82EE7UL, (static_cast<uint64_t>(0xFE60CF6CUL) << 32) + 0xAF321874UL,
      (static_cast<uint64_t>(0xE224479FUL) << 32) + 0x47CB76A0UL, (static_cast<uint64_t>(0xA0D4A674UL) << 32) + 0xEE214033UL,
      (static_cast<uint64_t>(0x67C58448UL) << 32) + 0x141F1B86UL, (static_cast<uint64_t>(0x253565A3UL) << 32) + 0xBDF52D15UL,
      (static_cast<uint64_t>(0xAB1721DAUL) << 32) + 0x49899A7FUL, (static_cast<uint64_t>(0xE9E7C031UL) << 32) + 0xE063ACECUL,
      (static_cast<uint64_t>(0x2EF6E20DUL) << 32) + 0x1A5DF759UL, (static_cast<uint64_t>(0x6C0603E6UL) << 32) + 0xB3B7C1CAUL,
      (static_cast<uint64_t>(0xF6FAE5C0UL) << 32) + 0x7D3274CDUL, (static_cast<uint64_t>(0xB40A042BUL) << 32) + 0xD4D8425EUL,
      (static_cast<uint64_t>(0x731B2617UL) << 32) + 0x2EE619EBUL, (static_cast<uint64_t>(0x31EBC7FCUL) << 32) + 0x870C2F78UL,
      (static_cast<uint64_t>(0xBFC98385UL) << 32) + 0x73709812UL, (static_cast<uint64_t>(0xFD39626EUL) << 32) + 0xDA9AAE81UL,
      (static_cast<uint64_t>(0x3A284052UL) << 32) + 0x20A4F534UL, (static_cast<uint64_t>(0x78D8A1B9UL) << 32) + 0x894EC3A7UL,
      (static_cast<uint64_t>(0x649C294AUL) << 32) + 0x61B7AD73UL, (static_cast<uint64_t>(0x266CC8A1UL) << 32) + 0xC85D9BE0UL,
      (static_cast<uint64_t>(0xE17DEA9DUL) << 32) + 0x3263C055UL, (static_cast<uint64_t>(0xA38D0B76UL) << 32) + 0x9B89F6C6UL,
      (static_cast<uint64_t>(0x2DAF4F0FUL) << 32) + 0x6FF541ACUL, (static_cast<uint64_t>(0x6F5FAEE4UL) << 32) + 0xC61F773FUL,
      (static_cast<uint64_t>(0xA84E8CD8UL) << 32) + 0x3C212C8AUL, (static_cast<uint64_t>(0xEABE6D33UL) << 32) + 0x95CB1A19UL,
      (static_cast<uint64_t>(0x90C79D3FUL) << 32) + 0xEDD3F122UL, (static_cast<uint64_t>(0xD2377CD4UL) << 32) + 0x4439C7B1UL,
      (static_cast<uint64_t>(0x15265EE8UL) << 32) + 0xBE079C04UL, (static_cast<uint64_t>(0x57D6BF03UL) << 32) + 0x17EDAA97UL,
      (static_cast<uint64_t>(0xD9F4FB7AUL) << 32) + 0xE3911DFDUL, (static_cast<uint64_t>(0x9B041A91UL) << 32) + 0x4A7B2B6EUL,
      (static_cast<uint64_t>(0x5C1538ADUL) << 32) + 0xB04570DBUL, (static_cast<uint64_t>(0x1EE5D946UL) << 32) + 0x19AF4648UL,
      (static_cast<uint64_t>(0x02A151B5UL) << 32) + 0xF156289CUL, (static_cast<uint64_t>(0x4051B05EUL) << 32) + 0x58BC1E0FUL,
      (static_cast<uint64_t>(0x87409262UL) << 32) + 0xA28245BAUL, (static_cast<uint64_t>(0xC5B07389UL) << 32) + 0x0B687329UL,
      (static_cast<uint64_t>(0x4B9237F0UL) << 32) + 0xFF14C443UL, (static_cast<uint64_t>(0x0962D61BUL) << 32) + 0x56FEF2D0UL,
      (static_cast<uint64_t>(0xCE73F427UL) << 32) + 0xACC0A965UL, (static_cast<uint64_t>(0x8C8315CCUL) << 32) + 0x052A9FF6UL,
      (static_cast<uint64_t>(0x3A80143FUL) << 32) + 0x5CF17F13UL, (static_cast<uint64_t>(0x7870F5D4UL) << 32) + 0xF51B4980UL,
      (static_cast<uint64_t>(0xBF61D7E8UL) << 32) + 0x0F251235UL, (static_cast<uint64_t>(0xFD913603UL) << 32) + 0xA6CF24A6UL,
      (static_cast<uint64_t>(0x73B3727AUL) << 32) + 0x52B393CCUL, (static_cast<uint64_t>(0x31439391UL) << 32) + 0xFB59A55FUL,
      (static_cast<uint64_t>(0xF652B1ADUL) << 32) + 0x0167FEEAUL, (static_cast<uint64_t>(0xB4A25046UL) << 32) + 0xA88DC879UL,
      (static_cast<uint64_t>(0xA8E6D8B5UL) << 32) + 0x4074A6ADUL, (static_cast<uint64_t>(0xEA16395EUL) << 32) + 0xE99E903EUL,
      (static_cast<uint64_t>(0x2D071B62UL) << 32) + 0x13A0CB8BUL, (static_cast<uint64_t>(0x6FF7FA89UL) << 32) + 0xBA4AFD18UL,
      (static_cast<uint64_t>(0xE1D5BEF0UL) << 32) + 0x4E364A72UL, (static_cast<uint64_t>(0xA3255F1BUL) << 32) + 0xE7DC7CE1UL,
      (static_cast<uint64_t>(0x64347D27UL) << 32) + 0x1DE22754UL, (static_cast<uint64_t>(0x26C49CCCUL) << 32) + 0xB40811C7UL,
      (static_cast<uint64_t>(0x5CBD6CC0UL) << 32) + 0xCC10FAFCUL, (static_cast<uint64_t>(0x1E4D8D2BUL) << 32) + 0x65FACC6FUL,
      (static_cast<uint64_t>(0xD95CAF17UL) << 32) + 0x9FC497DAUL, (static_cast<uint64_t>(0x9BAC4EFCUL) << 32) + 0x362EA149UL,
      (static_cast<uint64_t>(0x158E0A85UL) << 32) + 0xC2521623UL, (static_cast<uint64_t>(0x577EEB6EUL) << 32) + 0x6BB820B0UL,
      (static_cast<uint64_t>(0x906FC952UL) << 32) + 0x91867B05UL, (static_cast<uint64_t>(0xD29F28B9UL) << 32) + 0x386C4D96UL,
      (static_cast<uint64_t>(0xCEDBA04AUL) << 32) + 0xD0952342UL, (static_cast<uint64_t>(0x8C2B41A1UL) << 32) + 0x797F15D1UL,
      (static_cast<uint64_t>(0x4B3A639DUL) << 32) + 0x83414E64UL, (static_cast<uint64_t>(0x09CA8276UL) << 32) + 0x2AAB78F7UL,
      (static_cast<uint64_t>(0x87E8C60FUL) << 32) + 0xDED7CF9DUL, (static_cast<uint64_t>(0xC51827E4UL) << 32) + 0x773DF90EUL,
      (static_cast<uint64_t>(0x020905D8UL) << 32) + 0x8D03A2BBUL, (static_cast<uint64_t>(0x40F9E433UL) << 32) + 0x24E99428UL,
      (static_cast<uint64_t>(0x2CFFE7D5UL) << 32) + 0x975E55E2UL, (static_cast<uint64_t>(0x6E0F063EUL) << 32) + 0x3EB46371UL,
      (static_cast<uint64_t>(0xA91E2402UL) << 32) + 0xC48A38C4UL, (static_cast<uint64_t>(0xEBEEC5E9UL) << 32) + 0x6D600E57UL,
      (static_cast<uint64_t>(0x65CC8190UL) << 32) + 0x991CB93DUL, (static_cast<uint64_t>(0x273C607BUL) << 32) + 0x30F68FAEUL,
      (static_cast<uint64_t>(0xE02D4247UL) << 32) + 0xCAC8D41BUL, (static_cast<uint64_t>(0xA2DDA3ACUL) << 32) + 0x6322E288UL,
      (static_cast<uint64_t>(0xBE992B5FUL) << 32) + 0x8BDB8C5CUL, (static_cast<uint64_t>(0xFC69CAB4UL) << 32) + 0x2231BACFUL,
      (static_cast<uint64_t>(0x3B78E888UL) << 32) + 0xD80FE17AUL, (static_cast<uint64_t>(0x79880963UL) << 32) + 0x71E5D7E9UL,
      (static_cast<uint64_t>(0xF7AA4D1AUL) << 32) + 0x85996083UL, (static_cast<uint64_t>(0xB55AACF1UL) << 32) + 0x2C735610UL,
      (static_cast<uint64_t>(0x724B8ECDUL) << 32) + 0xD64D0DA5UL, (static_cast<uint64_t>(0x30BB6F26UL) << 32) + 0x7FA73B36UL,
      (static_cast<uint64_t>(0x4AC29F2AUL) << 32) + 0x07BFD00DUL, (static_cast<uint64_t>(0x08327EC1UL) << 32) + 0xAE55E69EUL,
      (static_cast<uint64_t>(0xCF235CFDUL) << 32) + 0x546BBD2BUL, (static_cast<uint64_t>(0x8DD3BD16UL) << 32) + 0xFD818BB8UL,
      (static_cast<uint64_t>(0x03F1F96FUL) << 32) + 0x09FD3CD2UL, (static_cast<uint64_t>(0x41011884UL) << 32) + 0xA0170A41UL,
      (static_cast<uint64_t>(0x86103AB8UL) << 32) + 0x5A2951F4UL, (static_cast<uint64_t>(0xC4E0DB53UL) << 32) + 0xF3C36767UL,
      (static_cast<uint64_t>(0xD8A453A0UL) << 32) + 0x1B3A09B3UL, (static_cast<uint64_t>(0x9A54B24BUL) << 32) + 0xB2D03F20UL,
      (static_cast<uint64_t>(0x5D459077UL) << 32) + 0x48EE6495UL, (static_cast<uint64_t>(0x1FB5719CUL) << 32) + 0xE1045206UL,
      (static_cast<uint64_t>(0x919735E5UL) << 32) + 0x1578E56CUL, (static_cast<uint64_t>(0xD367D40EUL) << 32) + 0xBC92D3FFUL,
      (static_cast<uint64_t>(0x1476F632UL) << 32) + 0x46AC884AUL, (static_cast<uint64_t>(0x568617D9UL) << 32) + 0xEF46BED9UL,
      (static_cast<uint64_t>(0xE085162AUL) << 32) + 0xB69D5E3CUL, (static_cast<uint64_t>(0xA275F7C1UL) << 32) + 0x1F7768AFUL,
      (static_cast<uint64_t>(0x6564D5FDUL) << 32) + 0xE549331AUL, (static_cast<uint64_t>(0x27943416UL) << 32) + 0x4CA30589UL,
      (static_cast<uint64_t>(0xA9B6706FUL) << 32) + 0xB8DFB2E3UL, (static_cast<uint64_t>(0xEB469184UL) << 32) + 0x11358470UL,
      (static_cast<uint64_t>(0x2C57B3B8UL) << 32) + 0xEB0BDFC5UL, (static_cast<uint64_t>(0x6EA75253UL) << 32) + 0x42E1E956UL,
      (static_cast<uint64_t>(0x72E3DAA0UL) << 32) + 0xAA188782UL, (static_cast<uint64_t>(0x30133B4BUL) << 32) + 0x03F2B111UL,
      (static_cast<uint64_t>(0xF7021977UL) << 32) + 0xF9CCEAA4UL, (static_cast<uint64_t>(0xB5F2F89CUL) << 32) + 0x5026DC37UL,
      (static_cast<uint64_t>(0x3BD0BCE5UL) << 32) + 0xA45A6B5DUL, (static_cast<uint64_t>(0x79205D0EUL) << 32) + 0x0DB05DCEUL,
      (static_cast<uint64_t>(0xBE317F32UL) << 32) + 0xF78E067BUL, (static_cast<uint64_t>(0xFCC19ED9UL) << 32) + 0x5E6430E8UL,
      (static_cast<uint64_t>(0x86B86ED5UL) << 32) + 0x267CDBD3UL, (static_cast<uint64_t>(0xC4488F3EUL) << 32) + 0x8F96ED40UL,
      (static_cast<uint64_t>(0x0359AD02UL) << 32) + 0x75A8B6F5UL, (static_cast<uint64_t>(0x41A94CE9UL) << 32) + 0xDC428066UL,
      (static_cast<uint64_t>(0xCF8B0890UL) << 32) + 0x283E370CUL, (static_cast<uint64_t>(0x8D7BE97BUL) << 32) + 0x81D4019FUL,
      (static_cast<uint64_t>(0x4A6ACB47UL) << 32) + 0x7BEA5A2AUL, (static_cast<uint64_t>(0x089A2AACUL) << 32) + 0xD2006CB9UL,
      (static_cast<uint64_t>(0x14DEA25FUL) << 32) + 0x3AF9026DUL, (static_cast<uint64_t>(0x562E43B4UL) << 32) + 0x931334FEUL,
      (static_cast<uint64_t>(0x913F6188UL) << 32) + 0x692D6F4BUL, (static_cast<uint64_t>(0xD3CF8063UL) << 32) + 0xC0C759D8UL,
      (static_cast<uint64_t>(0x5DEDC41AUL) << 32) + 0x34BBEEB2UL, (static_cast<uint64_t>(0x1F1D25F1UL) << 32) + 0x9D51D821UL,
      (static_cast<uint64_t>(0xD80C07CDUL) << 32) + 0x676F8394UL, (static_cast<uint64_t>(0x9AFCE626UL) << 32) + 0xCE85B507UL
    };

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    RadioChannelCentral::RadioChannelCentral(const char_t * const readChannelName, 
      const char_t * const writeChannelName, const uint16_t radChnlID, const char_t * const compShtName) :
      RadioChannel(readChannelName, writeChannelName, radChnlID,  compShtName)
    {
    }

    /******************************************************************************
    * calculateCRC
    ******************************************************************************/
    uint64_t RadioChannelCentral::calculateCRC(const uint8_t*  start, uint16_t const length)
    {
      uint64_t crc = crcInitValueCentral;
      uint16_t bytesLeft = length;

      if (NULL != start)
      {
        while (bytesLeft > 0U)
        {
          crc = crcTableCentral[*start ^ static_cast<uint8_t>(crc >> 56)] ^ (crc << 8);
          start++;
          bytesLeft--;
        }
      }
      else
      {
        ATC::aosHalt(__FILE__, __LINE__, "NULL pointer for start passed to calculateCRC()");
      }

      const uint64_t xorval = (static_cast<uint64_t>(0xffffffffUL) << 32) + 0xffffffffUL;
      crc = crc ^ xorval;
      return crc;
    }

    /**
    * Test the predefined output specified in FFFIS TCC-AOS 4.2.2 for string "123456789"
    */
    bool RadioChannelCentral::selfTest(void) 
    {
      uint8_t b[9] = 
      { 
        static_cast<uint8_t>('1'), 
        static_cast<uint8_t>('2'), 
        static_cast<uint8_t>('3'), 
        static_cast<uint8_t>('4'), 
        static_cast<uint8_t>('5'), 
        static_cast<uint8_t>('6'), 
        static_cast<uint8_t>('7'), 
        static_cast<uint8_t>('8'), 
        static_cast<uint8_t>('9') 
      };

      uint64_t crc = calculateCRC(&b[0], 9U);
      uint64_t val = (static_cast<uint64_t>(0x62EC59E3UL) << 32) + 0xF1A4F00AUL;

      return (crc == val);
    }

    /******************************************************************************
    * isCentral
    ******************************************************************************/
    bool RadioChannelCentral::isCentral() const
    {
      return true;
    }

    /******************************************************************************
    * initCrossCompare
    *-----------------------------------------------------------------------------
    * Registers internal persistent values for cross-compare
    *
    ******************************************************************************/
    void RadioChannelCentral::initCrossCompare() const
    {
      RadioChannel::initCrossCompare();

      //lint --e{586} 'new' is acceptable during initialization

      // Divide the table into 4 parts in order to get less data for each compare
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTableCentral[0U], 64U));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTableCentral[64U], 64U));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTableCentral[128U], 64U));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareArray<uint64_t>(&crcTableCentral[192U], 64U));
    }
  }
}


