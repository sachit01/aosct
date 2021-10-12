/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2015
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  Simulation of vfw-calls in the PC environment
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2015-12-22    bhermans    Created
* 2016-04-19    lantback    Removed Synergy strings
* 2016-05-03    akushwah    added vfwGetSide function
* 2016-05-24    akushwah    updated read, write vital framework buffer functions
* 2016-06-23    spandita    updated  buffer ,channel,time,sync and supported function
                            added comments
* 2016-07-04    spandita    updated with review comments
* 2016-07-05    spandita    updated with review comments (renamed to odoconfigResponseChannel_A)
* 2016-08-04    adgupta     Added null check and faking validity true for internal vfw operations
* 2016-08-04    adgupta     Added support for DMI communication
* 2016-09-20    akushwah    Added function vfwGetTimeOfDay
* 2016-09-23    bhermans    Corrected vfwGetReferenceTime. Shall return time in ms
* 2016-09-15    spandita    Added Code for OPC sim channel read and writes
* 2016-09-28    adgupta     Added support for BTM Handler.
* 2016-10-06    arastogi    Removed the include of ATP-BHP config file.
* 2016-10-06    adgupta     Updated write channel buffer to point to buffer base pointer
* 2016-10-07    akushwah    Added DMI Channel2 for Init and updated the vfwGetString() & vfwPutString()
* 2016-12-05    rquensel    Added support for Cross Compare
* 2016-01-03    spandita    Fixed the bug of vfwPutString
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "vfw_sim.hpp"
#include <vfw_buffer.h>
#include <vfw_checkpoints.h>
#include <vfw_crosscompare.h>
#include <vfw_init.h>
#include <vfw_string.h>
#include <vfw_sync.h>
#include <vfw_identity.h>
#include "vfw_sim.hpp"
#include <time.h>
#include <vfw_nvsh.h>
#include "atc_util.hpp"
#include "channel_config.hpp"
#include "abstract_basic_ip.hpp"
#include "basic_ip.hpp"
#include <vfw_crc.h>
#include <vfw_string.h>
#include <string.h>
#include "abstract_config_base.hpp"

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
void simVfwStartCycle();
#endif

#ifndef __GNUG__
#define __attribute__(A) /* If not GNU, do nothing. This must be defined before including vfw_halt.h */
#endif
#include <vfw_halt.h>
#include <vfw_version.h>

/**
* \brief Ip address for ATPCPU (local host)
*/
const char ipAddrA[20] = "127.0.0.1"; //!< local host for SIL operation 

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/
#ifdef WIN32
// Its is not possible to include <vfw_time.h> in visual studio
extern "C" int64_t vfwGetReferenceTime(void);
extern "C" int64_t vfwGetHighResolutionClock(void);
extern "C" void vfwGetTimeOfDay(struct timespec * const timespec_p, struct tm * const tm_p);
#else
#include <vfw_time.h>
#endif


/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

struct VFWReadBufferStruct
{
  uint8_t    readBuffer[ATC::maxRadioMessageSize]; // Buffer for incoming data 
  uint32_t   bytesRead;  // Bytes left to read from readBuffer
};

struct vfw_i_CrossCompare
{
  VFW_CCAddData*     addData;
  VFW_CCReceiveData* receiveData;
  uint32_t           compareSize;
};

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/

namespace
{
  void vfwValidateBuffer(const VFW_Buffer* const buffer);
  bool VfwCheckWrite(const VFW_Buffer* const buffer);
  void compareCrossCompareData(const VFW_Buffer* const ownBuffer, const VFW_Buffer* const otherBuffer);

  const uint32_t BUFFER_READ_MODE = 0U;
  const uint32_t BUFFER_WRITE_MODE = 1U;
  const char_t* module_C = __FILE__;

  bool vfwInitIsDone = false;
  bool vfwStartIsDone = false;

  // Buffers for incoming data to ATP
  static struct VFWReadBufferStruct vfwReadBuffer[ipConnectionMax];

  // 1500 * 100ms = 150sec, this reflects the estimated time until AOS on target is up and running. 
  int64_t cycleCounter = 1500;

  VFW_SyncTimerHandler* atpMainLoop = static_cast<VFW_SyncTimerHandler*>(NULL);
  VFW_SyncTimer atpMainLoopTimer = static_cast<VFW_SyncTimer>(NULL);

  /**
  * \brief Port Number for Simulation functionalities
  *
  */
  uint16_t portNrSimCodOdometryConfig = 0U;
  uint16_t portNrSimCodOdometryConfigResponse = 0U;
  uint16_t portNrSimCodOdometryMeas = 0U;
  uint16_t portNrSimVIOHSim = 0U;
  uint16_t portNrSpeedSim = 0U;
  uint16_t portNrRadiochannel1 = 0U;
  uint16_t portNrRadiochannel2 = 0U;
  uint16_t portNrRadiochannel3 = 0U;
  uint16_t portNrDMIChannel1 = 0U;
  uint16_t portNrDMIChannel2 = 0U;
  uint16_t portNrBTMHdlrOpcBtmTel = 0U;
  uint16_t portNrBTMHdlrOpcAppStatus = 0U;
  uint16_t portNrBTMHdlrTigrisOffset = 0U;
  uint16_t portNrBTMHdlrOpcClockSync = 0U;
  uint16_t portNrBTMHdlrCommand = 0U;
  uint16_t portNrLCS = 0U;
  uint16_t portNrOPCSim = 0U;
  uint16_t portNrOBRDSim = 0U;
  char_t ipAddrLCS[20];

  const uint64_t     crc64Init = 0U;
  const uint64_t     crc64Table[256] =
  {
    /*(static_cast<uint64_t>(0x00000000UL) << 32) +*/ 0x00000000UL, (static_cast<uint64_t>(0x42F0E1EBUL) << 32) + 0xA9EA3693UL,
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

#ifndef WIN32
  char* _strdup(const char* source)
  {
    return strdup(source);
  }

  void Sleep(uint32_t milliSec)
  {
    usleep(milliSec * 1000U);
  }

  uint32_t GetModuleFileNameA(void* /*module*/, char* filename, uint32_t size)
  {
    ATC::debugInfo("Warning: GetModuleFileNameA() isn't fully implemented for this platform\n");

    strncpy(filename, "test_atp.exe", size);
    filename[size - 1] = '\0';

    return strlen(filename);
  }

  uint32_t GetPrivateProfileIntA(const char* appName, const char* keyName, int32_t defaultValue, const char* filename)
  {
    ATC::debugInfo("Warning: GetPrivateProfileIntA() isn't fully implemented for this platform\n");

    return defaultValue;
  }

  uint32_t GetPrivateProfileStringA(const char* appName, const char* keyName, const char* defaultValue, char* returnedString, const uint32_t size, const char* filename)
  {
    ATC::debugInfo("Warning: GetPrivateProfileStringA() isn't fully implemented for this platform\n");

    strncpy(returnedString, defaultValue, size);
    returnedString[size - 1] = '\0';

    return strlen(returnedString);
  }

  bool WritePrivateProfileStringA(const char* appName, const char* keyName, const char* stringValue, const char* filename)
  {
    ATC::debugInfo("Warning: WritePrivateProfileStringA() isn't fully implemented for this platform\n");

    return true;
  }
#endif

  char_t simulationFilename[1000];
  char_t cfgDataFilename[1000];
  char_t rtDataFilename[1000];

  const char_t* const getIniFileName(const char_t* const fileName)
  {
    const char_t* iniFilename = static_cast<char_t *>(NULL);

    if (strcmp(fileName, "cfg_data.bin") == 0)
    {
      iniFilename = cfgDataFilename;
    }
    else if (strcmp(fileName, "rt_data.bin") == 0)
    {
      iniFilename = rtDataFilename;
    }
    else if (strcmp(fileName, "mnt_data.bin") == 0)
    {
      iniFilename = cfgDataFilename;
    }
    else if (strcmp(fileName, "type_data.bin") == 0)
    {
      iniFilename = cfgDataFilename;
    }
    else if (strcmp(fileName, "instance_data.bin") == 0)
    {
      iniFilename = cfgDataFilename;
    }
    else
    {
      iniFilename = simulationFilename;
    }

    return iniFilename;
  }

  void setupIniFilenames()
  {
    char_t iniFileName[1000];
    /* Prepare ini file name based on exe file name*/
    GetModuleFileNameA(NULL, iniFileName, sizeof(iniFileName));
    /* Patch .exe to .ini*/
    char_t *p = iniFileName;

    uint16_t i = 0U;

    while (*p != '\0')
    {
      ++p;
      ++i;
    }

    // Remove .exe from filename...
    i -= 4U;

    memcpy(simulationFilename, iniFileName, i);
    strcpy(simulationFilename + i, ".ini");

    memcpy(cfgDataFilename, iniFileName, i);
    strcpy(cfgDataFilename + i, "_cfg.ini");

    memcpy(rtDataFilename, iniFileName, i);
    strcpy(rtDataFilename + i, "_rt.ini");
  }

  void readSimulationParameters()
  {
    setupIniFilenames();

    const char_t section[] = "Simulation";

    const char_t* const filename = getIniFileName(section);

    portNrSimCodOdometryConfig = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrSimCodOdometryConfig", 30170, filename));
    portNrSimCodOdometryConfigResponse = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrSimCodOdometryConfigResponse", 30171, filename));
    portNrSimCodOdometryMeas = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrSimCodOdometryMeas", 30172, filename));
    portNrSimVIOHSim = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrSimVIOHSim", 30190, filename));
    portNrSpeedSim = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrSpeedSim", 30191, filename));
    portNrRadiochannel1 = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrRadiochannel1", 30132, filename));
    portNrRadiochannel2 = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrRadiochannel2", 30133, filename));
    portNrRadiochannel3 = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrRadiochannel3", 30134, filename));
    portNrDMIChannel1 = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrDMIChannel1", 30130, filename));
    portNrDMIChannel2 = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrDMIChannel2", 30136, filename));
    portNrBTMHdlrOpcBtmTel = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrBTMHdlrOpcBtmTel", 30140, filename));
    portNrBTMHdlrOpcAppStatus = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrBTMHdlrOpcAppStatus", 30144, filename));
    portNrBTMHdlrTigrisOffset = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrBTMHdlrTigrisOffset", 30147, filename));
    portNrBTMHdlrOpcClockSync = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrBTMHdlrOpcClockStatus", 30145, filename));
    portNrBTMHdlrCommand = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrBTMHdlrCommand", 30142, filename));
    portNrLCS = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrLCS", 30150, filename));
    portNrOPCSim = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrOPCSim", 30192, filename));
    portNrOBRDSim = static_cast<uint16_t>(GetPrivateProfileIntA(section,
      "portNrOBRDSim", 30151, filename));
    GetPrivateProfileStringA(section,
      "ipAddrLCS", &ipAddrA[0], &ipAddrLCS[0],sizeof(ipAddrLCS), filename);
  }

#ifdef _DEBUG
  void printSimulationParameters()
  {
    printf("//*************************************************************/\n");
    printf("//******************Read Simulated Parameter*******************/\n");

    printf("\nCodOdoConfig port:%12hu \n", portNrSimCodOdometryConfig);
    printf("CodOdoConfigResponse port:%4hu \n", portNrSimCodOdometryConfigResponse);
    printf("CodOdoConfigMeas port:%8hu \n", portNrSimCodOdometryMeas);
    printf("VIOHSim port:%17hu \n", portNrSimVIOHSim);
    printf("SpeedSim port:%16hu \n", portNrSpeedSim);
    printf("RadioChannel1 port:%11hu \n", portNrRadiochannel1);
    printf("RadioChannel2 port:%11hu \n", portNrRadiochannel2);
    printf("RadioChannel3 port:%11hu \n", portNrRadiochannel3);
    printf("DMIChannel1 port:%13hu \n", portNrDMIChannel1);
    printf("DMIChannel2 port:%13hu \n", portNrDMIChannel2);
    printf("BTHhandlerOpcBtmTelegram port:%3hu \n", portNrBTMHdlrOpcBtmTel);
    printf("BTMHdlrCommand port:%11hu\n", portNrBTMHdlrCommand);
    printf("LCS port:%21hu\n", portNrLCS);
    printf("OPCSim port:%16hu \n", portNrOPCSim);
    printf("OBRD Sim Port:%16hu \n", portNrOBRDSim);
    printf("LCS IP Address: %s", ipAddrLCS);

    printf("\n//**************************End********************************/\n");
  }
#endif

  /******************************************************************************
  * calcCrc64
  ******************************************************************************/
  uint64_t calcCrc64(const uint8_t* start, uint32_t const length)
  {
    uint64_t crc = crc64Init;
    uint32_t bytesLeft = length;
    
    if (NULL != start)
    {
      while (bytesLeft > 0U)
      {
        crc = crc64Table[*start ^ static_cast<uint8_t>(crc >> 56)] ^ (crc << 8);
        start++;
        bytesLeft--;
      }
    }
    else
    {
      VFW_HALT(("NULL pointer for start passed to calculateCRC()"));
    }
    
    const uint64_t xorval = (static_cast<uint64_t>(0xffffffffUL) << 32) + 0xffffffffUL;
    crc = crc ^ xorval;
    return crc;
  }

  /* Local helper functions
  */

  /******************************************************************************
  * unsigned minimum
  ******************************************************************************/
  uint32_t minimum(const uint32_t a, const uint32_t b)
  {
    return (a < b) ? a : b;
  }

  VFW_Buffer  *
  vfwBufferHead(VFW_Buffer * buffer)
  {
    VFW_Buffer         *head = buffer;

    if (buffer == NULL) {
      VFW_HALT(("Illegal parameter"));
    }
    else {
      while (head->p != NULL) {
        head = (VFW_Buffer *)head->p;
      }
    }

    return head;
  }

  void
  vfwUpdateBuffer(VFW_Buffer * buffer, int32_t relative_offset)
  {

    if (buffer == NULL) {
      VFW_HALT(("Illegal parameter"));
    }
    else {
      VFW_Buffer         *current;

      for (current = buffer; current != NULL; current = current->p) {
        current->o += relative_offset;

        if (current->b_s < current->o) {
          VFW_HALT(("buffer overflow sanity check"));
        }
      }
    }
  }

  void vfwValidateBuffer(const VFW_Buffer* const buffer)
  {
    if (buffer == NULL) {
      VFW_HALT(("Illegal parameter"));
    }
    else {
      if (buffer->b == NULL) {
        VFW_HALT(("Illegal parameter"));
      }

      if (buffer->b_s < buffer->o) {
        VFW_HALT(("Illegal parameter"));
      }

      switch (buffer->m) {
      case BUFFER_READ_MODE:
        if ((buffer->v < buffer->o)) {
          VFW_HALT(("Illegal parameter, offset greater than valid"));
        }
        break;

      case BUFFER_WRITE_MODE:
        break;

      default:
        VFW_HALT(("Illegal mode"));
        break;
      }
    }
  }



  /***************************************************************************
  * vfw buffer mode check functions
  *-----------------------------------------------------------------------------
  *
  * \brief  Used for validating the mode of buffer (Internal file used functions)
  * \return  Mode of buffer(True - Write /False -Read)
  ******************************************************************************/
  bool VfwCheckWrite(const VFW_Buffer* const buffer)
  {

    switch (buffer->m) {
    case BUFFER_READ_MODE:
      return false;
      break;
    case BUFFER_WRITE_MODE:
      if ((buffer->b_s <= buffer->o)) {
        return false;
      }
      else {
        return true;
      }
      break;
    default:
#ifdef DEBUG
      std::cout << "In default mode" << std::endl;
#endif
      return false;
      break;
    }
  }


  void compareCrossCompareData(const VFW_Buffer* const ownBuffer, const VFW_Buffer* const otherBuffer)
  {
    const uint32_t ownSize = vfwGetValidSize(ownBuffer);
    const uint32_t otherSize = vfwGetValidSize(otherBuffer);

    if (ownSize != otherSize)
    {
      // TODO: HALT
    }
    else
    {
      // Same length, get the actual byte buffers ...
      const uint8_t* ownByteBuffer = vfwGetStart(ownBuffer);
      const uint8_t* otherByteBuffer = vfwGetStart(otherBuffer);

      // Compare the char buffers received.
      const int32_t cmpResult = memcmp(ownByteBuffer, otherByteBuffer, ownSize);

      if (cmpResult != 0)
      {
        uint32_t i = 0U;

        while (i < ownSize)
        {
          if (*ownByteBuffer != *otherByteBuffer)
          {
            // TODO: HALT
            i = ownSize;
          }
          else
          {
            ++i;
            ++ownByteBuffer;
            ++otherByteBuffer;
          }
        }
      }
    }
  }

  /***************************************************************************
  * vfw buffer mode check functions
  *-----------------------------------------------------------------------------
  *
  * \brief  Used for validating the mode of buffer (Internal file used functions)
  * \return  Mode of buffer(True - Write /False -Read)
  ******************************************************************************/

  void
    VfwRecursivelyValidateBuffer(const VFW_Buffer * buffer)
  {
    VFW_Buffer         *current = (VFW_Buffer *)buffer;

    do {
      vfwValidateBuffer(current);
      current = current->p;
    } while (current != NULL);
  }

} // namespace


/********************************************************************************
* \brief  To get the current time
* \return  The simulated synchronized reference time, in milliseconds.
*********************************************************************************/

int64_t vfwGetReferenceTime(void)
{
  /*
  int64_t time_cal = 0;
  time_t tm;
  time_cal = static_cast<int64_t>(_time64(&tm));
  return time_cal;
  */
  // Do not make exact 100 ms steps
  static const int64_t timeDiffOffsets[13] = { -5, -4, -2, 1, 2, 4, 7, 9, 6, 3, 2, -1, -3 };

  return (cycleCounter * 100) + timeDiffOffsets[cycleCounter % 13];
}

/********************************************************************************
* \brief  Gets time in nanoseconds, since first called.
* \return  Time, in nanoseconds, since this function was first called.
*********************************************************************************/

int64_t vfwGetHighResolutionClock(void)
{
  static int64_t clockWhenFirstCalledMs = vfwGetReferenceTime();
  static bool firstCall = true;
  int64_t retVal;

  if (firstCall)
  {
    firstCall = false;
    retVal = 0;
  }
  else
  {
    retVal = (vfwGetReferenceTime() - clockWhenFirstCalledMs) * 1000000; // Simulate Nanoseconds
  }

  return retVal;
}

/********************************************************************************
* \brief  To get the calendar time
* \return  calculated time .
*********************************************************************************/

void vfwGetTimeOfDay(struct timespec *const   timespec_p, struct tm *const tm_p)
{
  // current date/time based on current system

#ifdef WIN32
  SYSTEMTIME systemTime;
  GetSystemTime(&systemTime);

  if (tm_p != NULL)
  {
    tm_p->tm_year = systemTime.wYear - 1900;
    tm_p->tm_mon  = systemTime.wMonth - 1;
    tm_p->tm_wday = systemTime.wDayOfWeek;
    tm_p->tm_mday = systemTime.wDay;
    tm_p->tm_hour = systemTime.wHour;
    tm_p->tm_min  = systemTime.wMinute;
    tm_p->tm_sec  = systemTime.wSecond;
  }

  if (timespec_p != NULL)
  {
    FILETIME fileTime;
    static_cast<void>(::SystemTimeToFileTime(&systemTime, &fileTime));

    ULARGE_INTEGER largeInt;
    largeInt.HighPart = fileTime.dwHighDateTime;
    largeInt.LowPart = fileTime.dwLowDateTime;

    const ULONGLONG EPOCH_DIFFERENCE = 11644473600ULL;
    const ULONGLONG TICKS_PER_SECOND = 10000000ULL;

    timespec_p->tv_sec = static_cast<time_t>((largeInt.QuadPart / TICKS_PER_SECOND) - EPOCH_DIFFERENCE);
    timespec_p->tv_nsec = static_cast<long>(systemTime.wMilliseconds) * 1000000L;
  }
#else
  time_t now = time(0);

  tm *ltm = localtime(&now);

  if (tm_p != NULL)
  {
    *tm_p = *ltm;
  }

  if (timespec_p != NULL)
  {
    timespec_p->tv_sec = now;
    timespec_p->tv_nsec = 0;
  }
#endif
}

/***************************************************************************
* vfw Buffer handling functions
*-----------------------------------------------------------------------------
*
* Simulate vfw behaviour
*
******************************************************************************/

/******************************************************************************
 * \brief       Initialize a raw buffer for VFW_Buffer usage.
 *
 *              The VFW buffer framework will take care of buffer overflow protection,
 *              current offset and alignment.
 *
 *******************************************************************************/
extern "C"
{
  void vfwInitBuffer(VFW_Buffer* buffer, uint8_t* raw_buffer, uint32_t buffer_size)
  {
    // Simulate vfw behaviour
    if (buffer != NULL) {           // Can't test for "valid buffer" here, since it shouldn't be valid before inited... Just check its not null.
      buffer->b = raw_buffer;     //pointing to raw buffer
      buffer->o = 0;              //offset set to 0 
      buffer->b_s = buffer_size;  //set the buffer size
      buffer->p = NULL;           //Pointer to parent buffer
      buffer->m = BUFFER_WRITE_MODE;    //to check for mode 
      buffer->v = 0;              //No valid data to start with
    }
    else {
    }
  }


  /**
  * \brief       Reserves part of an existing VFW_Buffer for indepenent usage from the
  *              existing source buffer.
  *
  *              The buffer inherits its mode from the parent buffer.
  */
  void
    vfwInitSubBuffer(VFW_Buffer * buffer, VFW_Buffer * source_buffer, uint32_t buffer_size)
  {
    if (buffer == NULL) {
      VFW_HALT(("Illegal parameter"));
    }
    else {
      VfwRecursivelyValidateBuffer(source_buffer);
      if (buffer_size > (source_buffer->b_s - source_buffer->o)) {
        VFW_HALT(("Trying to allocate outside buffer size"));
      }

      buffer->b = &source_buffer->b[source_buffer->o];
      buffer->b_s = buffer_size;
      buffer->o = 0;
      buffer->m = source_buffer->m;
      buffer->v = (buffer->m == BUFFER_READ_MODE ? buffer->b_s : 0);
      buffer->p = NULL;
      vfwUpdateBuffer(source_buffer, buffer_size);
    }
  }


  /*******************************************************************************
   * \brief           Sets the used memory area in buffer to zero and resets the
   *                  internal buffer offset to zero.
   * \param [in,out]  buffer buffer to be cleared.
   ********************************************************************************/

  void vfwClearBuffer(VFW_Buffer* buffer)
  {
    vfwValidateBuffer(buffer);

    buffer->v = 0;
    buffer->m = BUFFER_WRITE_MODE;
    memset(buffer->b, 0, static_cast<size_t>(buffer->o));
    buffer->o = 0;
  }

  /*******************************************************************************
   * \brief       Update the valid length of a write buffer and set it to read mode.
   *
   *              To be used when the buffer has been initialised with a raw_buffer
   *              that already contains data to be used by buffer functions.
  *******************************************************************************/

  void vfwSetReadBuffer(VFW_Buffer* buffer, uint32_t buffer_size)
  {
    vfwValidateBuffer(buffer);
    buffer->v = buffer_size;  //Set the Valid Size in Buffer
    buffer->o = 0;            //Reset offset
    buffer->m = BUFFER_READ_MODE;   //Change to read mode
  }

  /*******************************************************************************
   * \brief           Stop writing data to a buffer, reset offset and set read mode.
   *                  If the buffer is already in read mode it is unchanged.
   *
   * \param [in,out]  buffer Buffer to change offset and mode in.
  *******************************************************************************/

  void vfwSetFullBuffer(VFW_Buffer* buffer)
  {
    vfwValidateBuffer(buffer);

    if (buffer->m != BUFFER_READ_MODE) {
      buffer->v = buffer->o;
      buffer->m = BUFFER_READ_MODE;
      buffer->o = 0;
    }
  }

  /*******************************************************************************
   * \brief           Appends an uint64_t value to the buffer.
   * \param [in,out]  buffer Buffer to be appended.
   * \param [in]      value Value to add.
   *******************************************************************************/

  void vfwPutU64(VFW_Buffer* buffer, uint64_t value)
  {
    uint32_t hiValue = htonl((uint32_t) (value >> 32U));
    uint32_t loValue = htonl((uint32_t) value);

    vfwCpyFromRawBuffer(buffer, (uint8_t*) &hiValue, (int32_t) sizeof(hiValue));
    vfwCpyFromRawBuffer(buffer, (uint8_t*) &loValue, (int32_t) sizeof(loValue));
  }

  /***************************************************************************
   * \brief           Reads an uint64_t value from buffer.
   *
   * \param           buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned in host byte order.
   *
   ****************************************************************************/

  uint64_t vfwGetU64(VFW_Buffer* buffer)
  {
    uint32_t hiValue;
    uint32_t loValue;
    uint64_t host_value;

    vfwCpyToRawBuffer((uint8_t*) &hiValue, buffer, (int32_t) sizeof(hiValue));
    vfwCpyToRawBuffer((uint8_t*) &loValue, buffer, (int32_t) sizeof(loValue));

    host_value = (((uint64_t) ntohl(hiValue)) << 32U) + ((uint64_t) ntohl(loValue));

    return host_value;
  }

  /****************************************************************************
   * \brief           Appends an int64_t value to the buffer.
   *
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add.
   *****************************************************************************/

  void vfwPutI64(VFW_Buffer* buffer, const int64_t value)
  {
    vfwPutU64(buffer, (uint64_t) value);
  }

  /****************************************************************************
   * \brief           Reads an int64_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   *****************************************************************************/

  int64_t vfwGetI64(VFW_Buffer* buffer)
  {
    int64_t host_value = (int64_t) vfwGetU64(buffer);

    return host_value;
  }
  /*****************************************************************************
   * \brief           Appends an uint32_t value to the buffer.
   * \param [in,out]  buffer Buffer to be appended.
   * \param [in]      value Value to add.
   ****************************************************************************/

  void vfwPutU32(VFW_Buffer* buffer, uint32_t value)
  {
    uint32_t            network_value = htonl(value);
    vfwCpyFromRawBuffer(buffer, (uint8_t *)& network_value, (int32_t) sizeof(network_value));
  }
  /*****************************************************************************
   * \brief           Reads an uint32_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned in host byte order.
   ****************************************************************************/

  uint32_t vfwGetU32(VFW_Buffer* buffer)
  {
    uint32_t            value, host_value;
    vfwCpyToRawBuffer((uint8_t *)& value, buffer, (int32_t) sizeof(value));
    host_value = ntohl(value);

    return host_value;
  }

  /*****************************************************************************
   * \brief           Appends an int32_t value to the buffer.
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add.
   ****************************************************************************/
  void vfwPutI32(VFW_Buffer* buffer, int32_t value)
  {
    vfwPutU32(buffer, static_cast<uint32_t> (value));
  }
  /****************************************************************************
   * \brief           Reads an int32_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned in host byte order.
   ****************************************************************************/
  int32_t vfwGetI32(VFW_Buffer* buffer)
  {
    return static_cast<int32_t> (vfwGetU32(buffer));
  }
  /***************************************************************************
   * \brief           Appends an uint16_t value to the buffer.
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add.
   ****************************************************************************/
  void vfwPutU16(VFW_Buffer* buffer, uint16_t value)
  {
    uint16_t            network_value = htons(value);
    vfwCpyFromRawBuffer(buffer, (uint8_t *)& network_value, (int32_t) sizeof(network_value));
  }

  /***************************************************************************
   * \brief           Reads an uint16_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned in host byte order.
    ****************************************************************************/
  uint16_t vfwGetU16(VFW_Buffer* buffer)
  {
    uint16_t            value, host_value;
    vfwCpyToRawBuffer((uint8_t *)& value, buffer, (int32_t) sizeof(value));
    host_value = ntohs(value);

    return host_value;
  }
  /***************************************************************************
   * \brief           Appends an int16_t value to the buffer.
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add.
   ****************************************************************************/
  void vfwPutI16(VFW_Buffer* buffer, int16_t value)
  {
    vfwPutU16(buffer, static_cast<uint16_t>(value));
  }
  /***************************************************************************
   * \brief           Reads an int16_t value from the buffer.
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned in host byte order.
   ****************************************************************************/
  int16_t vfwGetI16(VFW_Buffer* buffer)
  {
    return static_cast<int16_t>(vfwGetU16(buffer));
  }

  /*****************************************************************************
   * \brief           Appends an uint8_t value to the buffer.
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add.
   ***************************************************************************/
  void vfwPutU8(VFW_Buffer* buffer, uint8_t value)
  {
    vfwCpyFromRawBuffer(buffer, (uint8_t *)& value, (int32_t) sizeof(value));
  }

  /***************************************************************************
   * \brief           Reads an uint8_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned.
    ***************************************************************************/
  uint8_t vfwGetU8(VFW_Buffer* buffer)
  {
    uint8_t             value;
    vfwCpyToRawBuffer(&value, buffer, (int32_t) sizeof(value));

    return value;
  }
  /***************************************************************************
   * \brief           Appends an int8_t value to the buffer.
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add
   ****************************************************************************/
  void vfwPutI8(VFW_Buffer* buffer, int8_t value)
  {
    vfwPutU8(buffer, static_cast<uint8_t>(value));
  }
  /***************************************************************************
   * \brief           Reads an int8_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned.

   ****************************************************************************/
  int8_t vfwGetI8(VFW_Buffer* buffer)
  {
    return static_cast<int8_t> (vfwGetU8(buffer));
  }
  /*****************************************************************************
   * \brief           Appends a bool_t value to the buffer.
   * \param [in,out]  buffer Buffer to append.
   * \param [in]      value Value to add.
   ****************************************************************************/
  void vfwPutBOOL(VFW_Buffer* buffer, bool_t value)
  {
    uint8_t  bool_value = value ? 1 : 0;

    vfwCpyFromRawBuffer(buffer, &bool_value, (int32_t) sizeof(bool_value));
  }
  /****************************************************************************
   * \brief           Reads a bool_t value from the buffer.
   *
   * \param [in,out]  buffer Buffer to read from.
   * \return          The value pointed to at the current offset is returned in host byte order.
   ****************************************************************************/
  bool_t vfwGetBOOL(VFW_Buffer* buffer)
  {
    bool_t              value;
    uint8_t             bool_value;

    vfwCpyToRawBuffer(&bool_value, buffer, (int32_t) sizeof(bool_value));
    value = (bool_value == 1);

    return value;
  }
  /*****************************************************************************
   * \brief           Appends a character string to the buffer and increments the offset in the buffer.
   *
   *                  The length of the string is placed before the string as an int32_t value.
   *
   * \param [in,out]  buffer      Buffer to append.
   * \param [in]      text_string String to add.
   ****************************************************************************/
  void vfwPutString(VFW_Buffer* buffer, const char * text_string)
  {
    if (text_string == NULL) {
      VFW_HALT(("Illegal parameter"));
    }
    else {
      const int32_t  text_length = (int32_t)strlen(text_string);
      vfwValidateBuffer(buffer);

      if (buffer->b_s < (buffer->o + text_length + 4)) {
        VFW_HALT(("Writing outside buffer"));
      }

      vfwPutI32(buffer, text_length);
      vfwCpyFromRawBuffer(buffer, (uint8_t *)text_string, text_length);
    }
  }

  /****************************************************************************
   * \brief           Reads a character string from the buffer.
   *
   *                  The string is always nul-terminated.
   *
   * \param [in,out]  buffer      Buffer to read from.
   *        [out]      text_string  String to be used for returning the read string.
   *        [in]      size        The maximum length of the string to read, including the nul character.
   * \return          The actual length of the read string, excluding the nul character.
   ****************************************************************************/
  uint32_t vfwGetString(VFW_Buffer* buffer, char * text_string, uint32_t size)
  {
    uint32_t retValue = 0U;

    vfwValidateBuffer(buffer);
    {
      const int32_t strLen = vfwGetI32(buffer);

      if (strLen < 0)
      {
        // Error?
      }
      else if (static_cast<uint32_t>(strLen) >= size)
      {
        // Error?
      }
      else
      {
        retValue = static_cast<uint32_t>(strLen);
        vfwCpyToRawBuffer(text_string, buffer, retValue);
        text_string[retValue] = '\0';
      }
    }

    return retValue;
  }

  /***************************************************************************
   * \brief           Copy from a VFW_Buffer buffer to a raw buffer.
   *
   *                  The data will be copied from the current offset in buffer.
   * \param [in, out] raw_buffer Raw buffer to copy to.
   * \param [in,out]  buffer Buffer to copy from.
   * \param [in]      size Number of bytes to copy.
   ****************************************************************************/
  void vfwCpyToRawBuffer(void *  raw_buffer, VFW_Buffer *  buffer, uint32_t  size)
  {
    if (raw_buffer == NULL) {

      VFW_HALT(("Illegal parameter"));
    }
    else {
      vfwValidateBuffer(buffer);
      if (buffer->m != BUFFER_READ_MODE) {
        VFW_HALT(("Illegal mode"));
      }
      else {
        if (buffer->v < (size + buffer->o)) {
          VFW_HALT(("Reading outside buffer"));
        }

        memmove(raw_buffer, &buffer->b[buffer->o], (size_t)size);
        vfwUpdateBuffer(buffer, size);
      }
    }
  }

  /***************************************************************************
   * \brief           Copy from a raw buffer to a buffer.
   *
   *                  The data will be appended at the current offset in the buffer.
   * \param [in,out]  buffer Buffer to copy to.
   * \param [in]      raw_buffer Buffer to copy from. May be NULL if buffer is fake buffer.
   * \param [in]      size Number of bytes to copy.
   ****************************************************************************/
  void vfwCpyFromRawBuffer(VFW_Buffer *  buffer, const void * raw_buffer, uint32_t  size)
  {
    if (raw_buffer == NULL) {
      VFW_HALT(("Illegal parameter"));
    }
    else {
      vfwValidateBuffer(buffer);
      if (buffer->m != BUFFER_WRITE_MODE) {
        VFW_HALT(("Illegal mode"));
      }
      else 
      {
        if (size > (buffer->b_s - buffer->o)) {
          VFW_HALT(("Writing outside buffer"));
        }

        memmove(&buffer->b[buffer->o], raw_buffer, (size_t)size);
        vfwUpdateBuffer(buffer, size);
      }
    }
  }


  /**
  * \brief           Copy from source_buffer to dest_buffer.
  *
  *                  The data will be
  *                  retrieved from the current offset in source_buffer and appended
  *                  at the current offset in the dest_buffer.
  *                  If dest_buffer is a fake buffer, source_buffer will not be copied,
  *                  however, dest_buffer's offset and size will be updated.
  *
  * \param [in,out]  dest_buffer Buffer to copy to.
  * \param [in,out]  source_buffer Buffer to copy from. May be NULL if dest_buffer is a fake buffer.
  * \param [in]      size Number of bytes to copy.
  * \pre             source_buffer and dest_buffer must have been initialized.
  * \pre             source_buffer and dest_buffer may not be part of the same parent/child tree.
  * \post            The data has been copied from source_buffer to dest_buffer and the
  *                  internal offset of each buffer has been incremented.
  * \exception       dest_buffer is invalid.
  * \exception       source_buffer is invalid.
  * \exception       source_buffer is fake and dest_buffer is not fake.
  * \exception       dest_buffer mode is read.
  * \exception       source_buffer mode is write.
  * \exception       dest_buffer trying to write outside dest_buffer.
  * \exception       source_buffer trying to read outside source_buffer.
  * \exception       source_buffer and dest_buffer origin from the same parent buffer.
  */

  void
  vfwCpyBuffer(VFW_Buffer * dest_buffer, VFW_Buffer * source_buffer, uint32_t size)
  {
    vfwValidateBuffer(dest_buffer);
    vfwValidateBuffer(source_buffer);

    if (dest_buffer->m != BUFFER_WRITE_MODE)
    {
      VFW_HALT(("Buffer mode error in destination buffer"));
    }

    if (source_buffer->m != BUFFER_READ_MODE)
    {
      VFW_HALT(("Buffer mode error in source buffer"));
    }

    if (vfwBufferHead(dest_buffer) == vfwBufferHead(source_buffer))
    {
      VFW_HALT(("Not allowed to copy within buffer this way"));
    }

    if (vfwGetFreeSpace(dest_buffer) < size)
    {
      VFW_HALT(("Buffer overflow in destination buffer"));
    }

    if (vfwGetValidSize(source_buffer) < size)
    {
      VFW_HALT(("Buffer overflow in source buffer"));
    }

    uint8_t* const destPointer = vfwGetPointer(dest_buffer);
    const uint8_t* const sourcePointer = vfwGetPointer(source_buffer);

    memmove(destPointer, sourcePointer, size);

    vfwUpdateBuffer(dest_buffer, size);
    vfwUpdateBuffer(source_buffer, size);
  }



  /***************************************************************************
   * \brief           In read mode, returns the number of bytes left to read.
   *                  In write mode, returns offset.
   * \param [in]      buffer Buffer to get valid size of.
   * \return          number of bytes left to read.
   ****************************************************************************/
  uint32_t vfwGetValidSize(const VFW_Buffer * buffer)
  {
    if (buffer->m == BUFFER_READ_MODE) {
      return (buffer->v - buffer->o);
    }
    else {
      return buffer->o;
    }
  }

  /***************************************************************************
  * vfw Channel handling functions
  *-----------------------------------------------------------------------------
  *
  * Simulate vfw behaviour
  *
  ******************************************************************************/
  /******************************************************************************
   * \brief        Open a communication channel in read mode.
   * \param [in]   channelName Channel name.
   * \param [in]   nMessages Maximum number of messages that the channel can hold.
   * \param [in]   sizeMessages Maximum size of a message.
   * \param [in]   notifyName Optional name of a notify file that can be used in a select system call, may be NULL.
   * \return       A VFW_ChannelDesc that should be used when reading from the channel.
   ******************************************************************************/

  VFW_ChannelDesc vfwChannelOpenRead(const char *channelName, uint32_t /*nMessages*/, uint32_t sizeMessages, const char* /*notifyName*/)
  {
    vfw_ChannelDesc *simulatedReadDesc = new vfw_ChannelDesc();
    bool  statusIP = false;
    simulatedReadDesc->descriptor = 0U; //To initialize with an incorrect value.

#ifdef _DISPATCHER
   //No VFW sim is intended to work in Dispatcher. There is Actual
    simulatedReadDesc->descriptor = 1U;   //added here to compile and link the code of dispacher
    statusIP = true; //Flyspray has been raised to fix this issue
#else
    if (0 == strncmp(channelName, ATC::radioChannel1DispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimTCC1;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        "0.0.0.0", portNrRadiochannel1, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::radioChannel2DispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimTCC2;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        "0.0.0.0", portNrRadiochannel2, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::radioChannel3DispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimTCC3;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        "0.0.0.0", portNrRadiochannel3, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::btmhChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimOPCSimBTMCommand;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrBTMHdlrCommand, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::opcClockSyncChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerClockSync;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrBTMHdlrOpcClockSync, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::odoConfigTelegramChannel_A, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimCODConfig;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrSimCodOdometryConfig, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::odoconfigResponseChannel_A, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimOdometryConfigResponse;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrSimCodOdometryConfigResponse, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::odoMeasureDataTeleChannel_A, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimOdometryMeas;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrSimCodOdometryMeas, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::speedChannelSim_A, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimCODSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrSpeedSim, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::opcChannelDispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimOPCSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrOPCSim, sizeMessages, sizeMessages);
    }
    //VIOH channel comparison
    else if (0 == strncmp(channelName, ATC::viohChannelDispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimVIOHSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrSimVIOHSim, sizeMessages, sizeMessages);
    }
    //DMI channel 1
    else if (0 == strncmp(channelName, ATC::dmiChannel1DispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimMMI1;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpClient,
        ipAddrA, portNrDMIChannel1, sizeMessages, sizeMessages);
    }
    //DMI channel 2
    else if (0 == strncmp(channelName, ATC::dmiChannel2DispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimMMI2;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpClient,
        ipAddrA, portNrDMIChannel2, sizeMessages, sizeMessages);
    }
    //BTM Handler
    else if (0 == strncmp(channelName, ATC::btmhChannelDispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerTelegram;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrBTMHdlrOpcBtmTel, sizeMessages, sizeMessages);
    }
    //BTM Handler Application status
    else if (0 == strncmp(channelName, ATC::btmhChannelDispAppDataToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerToOpcAppStatus;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrBTMHdlrOpcAppStatus, sizeMessages, sizeMessages);
    }
    //BTM Handler Tigris Offset
    else if (0 == strncmp(channelName, ATC::tigrisOffsetChannelNameA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerToOpcTigrisOffset;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrBTMHdlrTigrisOffset, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::lcsChannelDispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimLCS;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpClient,
        ipAddrLCS, portNrLCS, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::obrdChannelDispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedReadDesc->descriptor = ATP::BasicIP::connectionSimOBRDSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedReadDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        ipAddrA, portNrOBRDSim, sizeMessages, sizeMessages);
    }
    else
    {
      statusIP = false;
    }

#endif  

    return statusIP ? simulatedReadDesc : NULL;
  }
  /******************************************************************************
   * \brief        Open a communication channel in write mode.
   * \param [in]   channelName Channel name.
   * \param [in]   nMessages Maximum number of messages that the channel can hold.
   * \param [in]   sizeMessages Maximum size of a message.
   * \return       A VFW_ChannelDesc that should be used when writing to the channel.
   ******************************************************************************/

  VFW_ChannelDesc vfwChannelOpenWrite(const char *channelName, uint32_t /*nMessages*/, uint32_t sizeMessages)
  {
    vfw_ChannelDesc *simulatedWriteDesc = new vfw_ChannelDesc;

    simulatedWriteDesc->descriptor = 0U;  //To initialize with an incorrect value.
    bool  statusIP = false;

#ifdef _DISPATCHER
    //No VFW sim is intended to work in Dispatcher. There is Actual 
    simulatedWriteDesc->descriptor = 1U;//added here to run the code of dispacher
    statusIP = true;//Flyspray has been raised to fix/revisited this issue 
#else
    if (0 == strncmp(channelName, ATC::radioChannel1ATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimTCC1;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        ipAddrA, portNrRadiochannel1, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::radioChannel2ATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimTCC2;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        ipAddrA, portNrRadiochannel2, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::radioChannel3ATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimTCC3;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        ipAddrA, portNrRadiochannel3, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::btmhChannelDispToATPA, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimOPCSimBTMTelegram;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrBTMHdlrOpcBtmTel, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::btmhChannelDispAppDataToATPA, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerAppStatus;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrBTMHdlrOpcAppStatus, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::tigrisOffsetChannelNameA, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerTigrisOffset;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrBTMHdlrTigrisOffset, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::odoConfigTelegramChannel_A, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimOdometryConfig;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrSimCodOdometryConfig, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::odoconfigResponseChannel_A, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimCODConfigResponse;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrSimCodOdometryConfigResponse, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::odoMeasureDataTeleChannel_A, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimCODMeas;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrSimCodOdometryMeas, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::viohChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimVIOHSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrSimVIOHSim, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::opcChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimOPCSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpHost,
        ipAddrA, portNrOPCSim, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::dmiChannel1ATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimMMI1;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpClient,
        ipAddrA, portNrDMIChannel1, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::dmiChannel2ATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimMMI2;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpClient,
        ipAddrA, portNrDMIChannel2, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::btmhChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimBTMHandlerBTMCommand;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrBTMHdlrCommand, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::opcClockSyncChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimBTMhandlerClockSync;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeUdpClient,
        ipAddrA, portNrBTMHdlrOpcClockSync, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::lcsChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimLCS;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpClient,
        ipAddrLCS, portNrLCS, sizeMessages, sizeMessages);
    }
    else if (0 == strncmp(channelName, ATC::obrdChannelATPAToDisp, VFW_CH_NAME_MAX))
    {
      simulatedWriteDesc->descriptor = ATP::BasicIP::connectionSimOBRDSim;
      statusIP = ATC::AbstractBasicIP::corePtr()->initConnection(simulatedWriteDesc->descriptor, ATC::AbstractBasicIP::ConnectionTypeTcpHost,
        ipAddrA, portNrOBRDSim, sizeMessages, sizeMessages);
    }
    else
    {
      // No matcing channel name...
    }

    if (!statusIP)
    {
      delete simulatedWriteDesc;
      simulatedWriteDesc = static_cast<vfw_ChannelDesc*>(NULL);
    }
#endif

    return simulatedWriteDesc;
  }

  /******************************************************************************
   * \brief        Open a dual communication channel.
   * \param [in]   channelName Channel name.
   * \param [in]   sizeMessage Maximum size of a message.
   * \param [in]   notifyName Optional name of a notify file that can be used in a select system call, may be NULL.
   * \return       Descriptor for dual channel.

   ******************************************************************************/
  VFW_ChannelDesc vfwDualOpen(const char* /*channelName*/, uint32_t /*sizeMessage*/, const char* /*notifyName*/)
  {
    vfw_ChannelDesc *simulatedDualDesc = new vfw_ChannelDesc();
    //This feature will not be available to simulator
    return simulatedDualDesc;
  }

  /** ******************************************************************************
   * \brief        Write to a communication channel.
   * \param [in]   chDesc The channel descriptor from vfwChannelOpenWrite() or vfwDualOpen().
   * \param [in]   buf The buffer.
   * \param [in]   len Size of buf in bytes.
   * *******************************************************************************/

  void vfwChannelWrite(const VFW_ChannelDesc chDesc, const void *  buf, uint32_t  len)
  {
    // Post-Process the message (casting away const to be able to post-process data in simulation).
    vfwSimPostProcessOutgoingMessage(chDesc->descriptor, (uint8_t*)(buf), len);

    ATC::AbstractBasicIP::corePtr()->writeBuf(chDesc->descriptor, (uint8_t*)(buf), len);
  }

  /********************************************************************************
  * \brief        Read from a communication channel.
  * \param [in]   descriptor The channel descriptor.
  * \param [out]  buf The buffer.
  * \param [in]   lenBuf Length of buf in bytes.
  * \return       Number of bytes read.
  *******************************************************************************/
  static int32_t readFromChannel(const uint8_t descriptor, void* buf, uint32_t lenBuf)
  {
    uint32_t readLen = 0U; // Bytes written back and returned to caller

    VFWReadBufferStruct& readBuffer = vfwReadBuffer[descriptor];

    // Read more data (if there is room in the buffer)
    const uint32_t availableSize = sizeof(readBuffer.readBuffer) - readBuffer.bytesRead;
    if (availableSize > 0U)
    {
      const uint32_t readResult = ATC::AbstractBasicIP::corePtr()->readBuf(
        descriptor, &readBuffer.readBuffer[readBuffer.bytesRead], availableSize);

      if (readResult > 0)
      {
        readBuffer.bytesRead += readResult;
      }
    }
    else
    {
      ATC::aosHalt(__FILE__, __LINE__, "Receive buffer too small");
    }

    // If any bytes read from IP or left in readBuffer -> Process & copy to buf
    if (readBuffer.bytesRead > 0U)
    {
      // Preprocess message (if needed)
      const bool processed = vfwSimPreProcessIncomingMessage(
        descriptor, readBuffer.readBuffer, readBuffer.bytesRead, static_cast<uint8_t*>(buf), readLen);

      if (!processed)
      {
        // No preprocessing, just copy message to buf
        readLen = minimum(readBuffer.bytesRead, lenBuf);
        memcpy(buf, readBuffer.readBuffer, readLen);
        readBuffer.bytesRead -= readLen;

        // Move any remaining bytes to the start of the buffer
        if (readBuffer.bytesRead > 0U)
        {
          memmove(&readBuffer.readBuffer[0], &readBuffer.readBuffer[readLen], readBuffer.bytesRead);
        }
      }
    }

    return readLen;
  }

  /********************************************************************************
   * \brief        Read from a communication channel.
   * \param [in]   chDesc The channel descriptor from vfwChannelOpenRead() or vfwDualOpen().
   * \param [out]  buf The buffer.
   * \param [in]   lenBuf Length of buf in bytes.
   * \return       Number of bytes read.
   *******************************************************************************/
  int32_t vfwChannelRead(const VFW_ChannelDesc  chDesc, void *  buf, uint32_t  lenBuf)
  {
    return readFromChannel(chDesc->descriptor, buf, lenBuf);
  }

  /***************************************************************************
  * vfw Sync handling functions
  *-----------------------------------------------------------------------------
  *
  * Simulate vfw behaviour
  *
  ******************************************************************************/
  /********************************************************************************
   * \brief       Add a channel to be handled by the synchronizer.
   *
   * \param [in]  channel The channel handle returned by vfwChannelOpenRead() or vfwDualOpen()
   * \param [in]  synchronized Whether the channel shall be
   *              synchronized or not.
   * \simulator   As channel are limited therefore use different descriptor for different sync
   *******************************************************************************/
  VFW_SyncChannel vfwSyncAddChannel(const VFW_ChannelDesc channel, bool_t /*synchronized*/)
  {
    // Synchronize with diversified channel (A/B). 
    vfw_i_SyncChannel *simulatedSyncDesc = new vfw_i_SyncChannel();
    simulatedSyncDesc->descriptor = channel->descriptor;
    return simulatedSyncDesc;
  }

  /********************************************************************************
   * \brief       Deactivates a synchronized channel..
   *
   * \simulator  This feature will not be available for simulator as there will not
   be any event based triggered
   *******************************************************************************/
  void vfwSyncChannelDeactivate(VFW_SyncChannel /*channel*/)
  {
  }
  /*******************************************************************************
   * \brief       Read a single message from a channel
   *
   * \param [in]  channel The channel handle returned by vfwSyncAddChannel().
   * \param [out] buf A pointer to at least len bytes where the message will be
   *              stored.
   * \param [in]  len The maximum size of the message.
   * \simulator   Read from Basic IP readbuf
   ********************************************************************************/
  int32_t vfwSyncChannelRead(VFW_SyncChannel channel, void *buf, uint32_t len)
  {
    return readFromChannel(channel->descriptor, buf, len);
  }

  /*********************************************************************************
  * \brief       Read a single message from a channel.
  *
  *              Same as vfwSyncChannelRead() but this function also returns a
  *              data structure for sequence and timeliness control.
  *              If the check parameter is NULL the function behaves as
  *              vfwSyncChannelRead().
  *
  * \param [in]  channel The channel handle returned by vfwSyncAddChannel().
  * \param [out]  buf A pointer to at least len bytes where the message will be
  *              stored.
  * \param [in]  len The maximum size of the message.
  * \param [out] check Structure for sequence and timeliness control.
  *
  * \return      The size of the message, or zero if there are no more messages
  *              pending.
  *
  * \exception   vfwStart() has not been called.
  * \pre         channel is a valid VFW_SyncChannel.
  * \pre         buf is at least len bytes.
  * \pre         check is a valid VFW_ChannelCheck or NULL.
  * \note This function calls vfwChannelReadCheck() for the corresponding
  *       channel.
  ********************************************************************************/
  int32_t vfwSyncChannelReadCheck(VFW_SyncChannel channel, void *buf, uint32_t len, VFW_ChannelCheck *check)
  {
    check->error = VFW_ChannelErrorNone;
    check->timeSinceProduced = 99U;

    return readFromChannel(channel->descriptor, buf, len);
  }

  /*********************************************************************************
  * \brief        Read from a communication channel to a VFW_Buffer.
  * \param        chDesc The channel handle returned by vfwSyncAddChannel().
  * \param        The buffer.
  * \return       Number of bytes read.
  ********************************************************************************/
  int32_t vfwSyncChannelReadBuffer(VFW_SyncChannel channel, VFW_Buffer *buffer)
  {
    return readFromChannel(channel->descriptor, buffer->b, buffer->b_s);
  }

  /********************************************************************************
  * \brief       Read a single message from a channel.
  *
  *              Same as vfwSyncChannelReadBuffer() but this function also returns
  *              a data structure for sequence and timeliness control.
  *              If the check parameter is NULL the function behaves as
  *              vfwSyncChannelReadBuffer().
  *
  * \param [in]  channel The channel handle returned by vfwSyncAddChannel().
  * \param [out]  buffer The buffer in which the message will be stored.
  * \param [out] check Structure for sequence and timeliness control.
  * \return      The size of the message, or zero if there are no more messages
  *              pending.
  *
  * \exception   vfwStart() has not been called.
  * \pre         channel is a valid VFW_SyncChannel.
  * \pre         buffer is a valid VFW_Buffer.
  * \pre         check is a valid VFW_ChannelCheck or NULL.
  * \note This function calls vfwChannelReadBufferCheck() for the corresponding
  *       channel.
  ********************************************************************************/
  int32_t vfwSyncChannelReadBufferCheck(VFW_SyncChannel channel, VFW_Buffer *buffer, VFW_ChannelCheck *check)
  {
    check->error = VFW_ChannelErrorNone;
    check->timeSinceProduced = 99U;

    return vfwSyncChannelReadBuffer(channel, buffer);
  }

  /********************************************************************************
   * \brief       Returns the number of  character on a synchronized
   *              channel.
   *
   * \param [in]  channel The channel handle returned by vfwSyncAddChannel().
   * \return      The number of synchronized messages available on the channel.
   *
   ********************************************************************************/
  uint32_t vfwSyncChannelStat(const VFW_SyncChannel channel)
  {
    /*
    The simulated function will actually return the no of characters available instead of (number of messages) as the real vfw.
    That , will however be Ok but must be considered when the function is used in the ATP code.
    This function will return the status of data available on channel
    return true if data is available
    */
    uint32_t retVal;

    if (channel->descriptor != 0U)
    {
      if ((vfwReadBuffer[channel->descriptor].bytesRead > 0U) || (ATC::AbstractBasicIP::corePtr()->isPendingRead(channel->descriptor)))
      {
        retVal = 1U;  //If true return 1 do denote at least 1 character is available
      }
      else
      {
        retVal = 0U;
      }
    }
    else
    {
      retVal = 0U;
    }

    return retVal;
  }

  /***************************************************************************
  * vfw identity functions
  *-----------------------------------------------------------------------------
  *
  * Simulate vfw behaviour
  *
  ******************************************************************************/
  /********************************************************************************
   * \brief      Gets the "diverse" side of the current process instance.
   * \return     A value indicating that the process has the role of
   *             an A, B or C program.
   *******************************************************************************/

  VFW_Side vfwGetSide(void)
  {
    return VFW_A_SIDE; //Currently only A-Side is in Use
  }

  /*********************************************************************************
   * \brief        Read from a communication channel to a VFW_Buffer.
   * \param        chDesc The channel descriptor
   * \param        The buffer.
   * \return       Number of bytes read.
   ********************************************************************************/
  int32_t vfwChannelReadBuffer(const VFW_ChannelDesc chDesc, VFW_Buffer *buffer)
  {
    return readFromChannel(chDesc->descriptor, buffer->b, buffer->b_s);
  }

  /********************************************************************************
   * \brief        Write to a communication channel from a VFW_Buffer.
   * \param        chDesc The channel descriptor from vfwChannelOpenWrite() or vfwDualOpen().
   * \param        buffer The buffer.
   *********************************************************************************/

  void vfwChannelWriteBuffer(const VFW_ChannelDesc chDesc, VFW_Buffer *buffer)
  {
    uint8_t *buf = buffer->b;
    uint32_t len = vfwGetValidSize(buffer);

    // Post-Process the message
    vfwSimPostProcessOutgoingMessage(chDesc->descriptor, buf, len);

    const uint32_t lengthWritten = ATC::AbstractBasicIP::corePtr()->writeBuf(chDesc->descriptor, buf, len);
  }
  /***********************************************************************************
   * \brief        Get the channel status//no of characters available.
   * \param [in]   chDesc The channel descriptor .
   * \return       presence of message in channel
   **********************************************************************************/

  uint32_t vfwChannelStat(const VFW_ChannelDesc chDesc)
  {
    /*
    The simulated function will actually return the no of characters available instead of (number of messages) as the real vfw.
    That , will however be Ok but must be considered when the function is used in the ATP code.
    This function will return the status of data available on channel
    return true if data is available
    */
    return static_cast<uint32_t>(ATC::AbstractBasicIP::corePtr()->isPendingRead(chDesc->descriptor));
  }

  /***************************************************************************
  * vfw timer functions
  *-----------------------------------------------------------------------------
  *
  * Simulate vfw behaviour
  *
  ******************************************************************************/
  /******************************************************************************
   * \brief  Create a timer.
   *
   * \return A handle to be used in further calls to the vfwTimer API.
   ******************************************************************************/

  VFW_Timer vfwTimerAllocate(void)
  {
    VFW_ASSERT((vfwInitIsDone && (!vfwStartIsDone)), (("Called in wrong order")));

    vfw_i_Timer *VFW_Timer = new vfw_i_Timer();
    if (VFW_Timer == NULL) {
      return 0;
    }
    else {
      return VFW_Timer;
    }
  }
  /******************************************************************************
   * \brief        Set the timeout for a timer, in milliseconds.
   *
   * The timeout can be set when the timer is not running or if the
   * timer has expired.
   *
   * \param [in,out]  timer The timer handle returned by vfwTimerAllocate().
   * \param [in]      msTimeout The new timeout, in milliseconds.
   *
   *******************************************************************************/
  void vfwTimerSetTimeout(VFW_Timer  timer, int64_t  msTimeout)
  {
    timer->time_out = msTimeout;
  }

  /*******************************************************************************
   * \brief        Start a timer.
   *
   * When a timer is started, the frozen synchronized reference time
   * is stored as its start time.
   * The running unsynchronized time is stored too, to be used for timeout
   * tolerance evaluation.
   *
   * \param [in,out]  timer The timer handle returned by vfwTimerAllocate(). *
   ********************************************************************************/
  void vfwTimerStart(VFW_Timer  timer)
  {
    timer->start_time = vfwGetReferenceTime();
  }

  /*******************************************************************************
  * \brief  Calculate the time left until a timer will expire.
  *
  * This function returns the time remaining in the timer after the timer
  * was started.
  ********************************************************************************/
  int64_t vfwTimerTimeLeft(const VFW_Timer timer)
  {
    int64_t timeRemaining;
    int64_t timeElapsed;

    timeElapsed = (vfwGetReferenceTime()) - (timer->start_time);
    timeRemaining = timer->time_out - timeElapsed;

    return timeRemaining;
  }

  /********************************************************************************
   * \brief  Check if a timer has expired.
   *
   * If the timer's expiry time has been overshot by more than its
   * tolerance (if a tolerance was set for the timer), the system
   * enters halt state.
   * The tolerance calculation is based upon the running unsynchronized time.
   *
   * \param [in]   timer The timer handle returned by vfwTimerAllocate().
   * \return Whether the timer has expired or not.
   *********************************************************************************/

  bool_t vfwTimerCheck(const VFW_Timer  timer)
  {
    bool_t statusTime = ATC::trueVfw;
    int64_t inter_time = timer->start_time + timer->time_out;
    int64_t curTime = vfwGetReferenceTime();
    if ((inter_time > curTime) || (0 == timer->start_time)){
      statusTime = ATC::falseVfw;
    }

    return statusTime;
  }

  /********************************************************************************
  * \brief  Check if a timer is running.
  *
  *    A timer is considered running after it has been started,
  *    but before it has been stopped.
  *
  * \param [in]   timer The timer handle returned by vfwTimerAllocate().
  * \return       Whether the timer is running or not.
  *********************************************************************************/
  bool_t vfwTimerRunning(const VFW_Timer timer)
  {
    bool_t running = ATC::falseVfw;
    int64_t curTime = vfwGetReferenceTime();
    if ((timer->start_time != 0) && (curTime < (timer->start_time + timer->time_out)) && timer->time_out != 0)
    {
      running = ATC::trueVfw;
    }
    return running;
  }

  /***************************************************************************
  * \brief  Stop a timer.
  *
  * \param [in,out]  timer The timer handle returned by vfwTimerAllocate().
  ***************************************************************************/
  void vfwTimerStop(VFW_Timer timer)
  {
    timer->start_time = 0;
  }

  /***************************************************************************
  * vfw Get current pointer functions
  *-----------------------------------------------------------------------------
  *
  * \brief  Used to get the pointer to current offset
  * \return  pointer to current offset
  ******************************************************************************/
  uint8_t* vfwGetPointer(const VFW_Buffer* buffer)
  {
    return (buffer->b + buffer->o);
  }

  /***************************************************************************
  * vfw NVSH functions
  *-----------------------------------------------------------------------------
  *
  *
  ******************************************************************************/
  struct vfw_i_NvshDescriptor {
    char_t * f;               //filename,
    char_t * u;               //uniqueFileId,
    uint32_t ds;              //dataSize,
    VFW_NvshErrorHandler* eh; //errorHandler,
    VFW_NvshReadHandler* rh;  //readHandler,
    uint32_t to;              //msMessageResponseTimeout
    VFW_Buffer b;             //buffer associated with this handle 
    uint8_t * r_b;            //raw buffer to use in vfw_buffer
  };



  /********************************************************************************
  * \brief           Allocate an NVSH handle for a file.
  *
  *                  Allocates an NVSH handle that should be used when making
  *                  further references to the NVSH file.
  *
  * \param [in]      filename A string identifying the file associated with this
  *                  handle. No file path, just a filename.
  * \param [in]      uniqueFileId A string uniquely identifying the file and its
  *                  (or a compatible) version. The id must be changed if the
  *                  application's data format for storage is changed.
  * \param [in]      dataSize The maximum amount of data that can be read from
  *                  this NVSH file in bytes.
  * \param [in]      errorHandler A call-back which will be called if any error is
  *                  encountered.
  * \param [in]      readHandler A call-back which will be called when the file
  *                  has been read and data is delivered.
  * \param [in]      consistencyHandler  A call-back which will be called to report
  *                  the consistency state of the file.
  * \param [in]      msMessageResponseTimeout Timeout in milliseconds for read
  *                  responses. If the timeout expires, the
  *                  system halts.
  * \param [in]      msReadbackTimeout Timeout in milliseconds for readback
  *                  requests, i.e. how often a readback should be performed for
  *                  sthis handle
  * \return          An NVSH handle that should be used when making further
  *                  references to the NVSH file.
  *******************************************************************************/
  VFW_NvshHandle vfwNvshAllocate(const char* filename,
    const char *    uniqueFileId,
    const uint32_t  dataSize,
    VFW_NvshErrorHandler *const  errorHandler,
    VFW_NvshReadHandler *const   readHandler,
    VFW_NvshConsistencyHandler* const  /*consistencyHandler*/,
    const uint32_t  msMessageResponseTimeout,
    const uint32_t  /*msReadbackTimeout*/)
  {
    vfw_i_NvshDescriptor * handle = static_cast<vfw_i_NvshDescriptor*>(malloc(sizeof(vfw_i_NvshDescriptor)));

    handle->f = _strdup(filename);
    handle->u = _strdup(uniqueFileId);
    handle->ds = dataSize;
    handle->eh = errorHandler;
    handle->rh = readHandler;
    handle->to = msMessageResponseTimeout;
    handle->r_b = static_cast<uint8_t*>(malloc(dataSize * sizeof(uint8_t)));

    vfwInitBuffer(&(handle->b), handle->r_b, handle->ds);
    vfwSetReadBuffer(&(handle->b), handle->ds);

    return handle;
  }

  /********************************************************************************
   * \brief           Allocate an NVSH handle for a configuration file.
   *
   *                  Allocates an NVSH handle that should be used when making
   *                  further references to the NVSH file.
   *
   * \param [in]      filename A string identifying the file associated with this
   *                  handle. No file path, just a filename.
   * \param [in]      uniqueFileId A string uniquely identifying the file and its
   *                  (or a compatible) version. The id must be changed if the
   *                  application's data format for storage is changed.
   * \param [in]      dataSize The maximum amount of data that can be read from
   *                  this NVSH file in bytes.
   * \param [in]      errorHandler A call-back which will be called if any error is
   *                  encountered.
   * \param [in]      readHandler A call-back which will be called when the file
   *                  has been read and data is delivered.
   * \param [in]      msMessageResponseTimeout Timeout in milliseconds for read
   *                  responses. If the timeout expires, the
   *                  system halts.
   * \return          An NVSH handle that should be used when making further
   *                  references to the NVSH file.
   *******************************************************************************/
  VFW_NvshHandle vfwNvshAllocateConfiguration(const char_t * filename,
    const char_t * uniqueFileId,
    const uint32_t dataSize,
    VFW_NvshErrorHandler* const errorHandler,
    VFW_NvshReadHandler* const readHandler,
    const uint32_t msMessageResponseTimeout)
  {
    vfw_i_NvshDescriptor * handle = static_cast<vfw_i_NvshDescriptor*>(malloc(sizeof(vfw_i_NvshDescriptor)));

    handle->f = _strdup(filename);
    handle->u = _strdup(uniqueFileId);
    handle->ds = dataSize;
    handle->eh = errorHandler;
    handle->rh = readHandler;
    handle->to = msMessageResponseTimeout;
    handle->r_b = static_cast<uint8_t*>(malloc(dataSize * sizeof(uint8_t)));

    vfwInitBuffer(&(handle->b), handle->r_b, handle->ds);

    return handle;
  }
  /********************************************************************************
   * \brief           Read data from an NVSH file.
   *
   *                  Results will be received in the registered read handler function.
   *
   * \param [in]      handle The handle returned by vfwNvshAllocate() or vfwNvshAllocateConfiguration().
   *******************************************************************************/
  void vfwNvshReadRequest(const VFW_NvshHandle handle)
  {
    const char_t* const section = handle->f;
    //Ignore supplied filename, we use only one ini-file with same name as the executable
    //(only change .exe to .ini), and in same folder as the executable.
    //char_t * filename = vfwNvshGetFilename(handle);
    const char_t * itemName;
    ATC::BaseConfigItem::ItemDatatype itemType;
    ATC::ConfigFile * configFile;
    bool versionWritten = false;
    char_t   retbuf[100];
    uint32_t read;
    uint32_t retval;
    uint8_t ipVByte[25];

    const char_t* const filename = getIniFileName(section);

    // Go through all available slots for config items
    for (ATC::ConfigIdType id = ATC::firstConfigItem; id < ATC::maxConfigItems; ++id)
    {
      // Try to find an item and get the info needed
      bool foundItem = ATC::AbstractConfigBase::basePtr()->getItemInfoForVfwSim(id, itemName, itemType, configFile);

      // If an item is found with that id, and the item belong to the right config area...
      if (foundItem && (handle == configFile->getNvshHandle()))
      {
        if (!versionWritten)
        {
          // Write the version, before the first parameter
          vfwPutU8(&(handle->b), configFile->getExpectedMajorVersion());
          vfwPutU8(&(handle->b), configFile->getExpectedMinorVersion());
          versionWritten = true;
        }

        // Push the id to the buffer
        vfwPutU16(&(handle->b), id);

        // And depending on type of item, get the int or text value from ini file
        // and push it as the correct type to the buffer
        if (itemType == ATC::BaseConfigItem::ItemDatatypeU8)
        {
          ATC::Uint8ConfigItem* u8item = static_cast<ATC::Uint8ConfigItem*> (ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          uint8_t u8defVal = u8item->getDefault();
          retval = GetPrivateProfileIntA(section, itemName, u8defVal, filename);
          vfwPutU8(&(handle->b), static_cast<uint8_t>(retval));
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeI8)
        {
          ATC::Int8ConfigItem* i8item = static_cast<ATC::Int8ConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          int8_t i8defVal = i8item->getDefault();
          retval = GetPrivateProfileIntA(section, itemName, i8defVal, filename);
          vfwPutI8(&(handle->b), static_cast<int8_t>(retval));
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeU16)
        {
          ATC::Uint16ConfigItem* u16item = static_cast<ATC::Uint16ConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          uint16_t u16defVal = u16item->getDefault();
          retval = GetPrivateProfileIntA(section, itemName, u16defVal, filename);
          vfwPutU16(&(handle->b), static_cast<uint16_t>(retval));
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeI16)
        {
          ATC::Int16ConfigItem* i16item = static_cast<ATC::Int16ConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          int16_t i16defVal = i16item->getDefault();
          retval = GetPrivateProfileIntA(section, itemName, i16defVal, filename);
          vfwPutI16(&(handle->b), static_cast<int16_t>(retval));
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeU32)
        {
          ATC::Uint32ConfigItem* u32item = static_cast<ATC::Uint32ConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          uint32_t u32defVal = u32item->getDefault();
          retval = GetPrivateProfileIntA(section, itemName, u32defVal, filename);
          vfwPutU32(&(handle->b), static_cast<uint32_t>(retval));
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeI32)
        {
          ATC::Int32ConfigItem* i32item = static_cast<ATC::Int32ConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          int32_t i32defVal = i32item->getDefault();
          retval = GetPrivateProfileIntA(section, itemName, i32defVal, filename);
          vfwPutI32(&(handle->b), static_cast<int32_t>(retval));
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeU64)
        {
          ATC::aosHalt(__FILE__, __LINE__, "Trying to read unsupported parameter type");
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeI64)
        {
          ATC::aosHalt(__FILE__, __LINE__, "Trying to read unsupported parameter type");
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeText)
        {
          ATC::StringConfigItem* stringItem = static_cast<ATC::StringConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          const char_t* strDefVal = stringItem->getDefault();
          read = GetPrivateProfileStringA(section, itemName, strDefVal, retbuf, 100, filename);
          vfwPutString(&(handle->b), retbuf);
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeBool)
        {
          ATC::BoolConfigItem* boolItem = static_cast<ATC::BoolConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          bool boolDefVal = boolItem->getDefault();
          int defVal = (boolDefVal) ? 1 : 0;
          retval = GetPrivateProfileIntA(section, itemName, defVal, filename);
          if (retval == 0)
            vfwPutBOOL(&(handle->b), ATC::falseVfw);
          else
            vfwPutBOOL(&(handle->b), ATC::trueVfw);
        }
        else if (itemType == ATC::BaseConfigItem::ItemDatatypeIpaddress)
        {
          ATC::IPaddressConfigItem * stringItem = static_cast<ATC::IPaddressConfigItem*>(ATC::AbstractConfigBase::basePtr()->getConfigItem(id));
          const char_t* strDefVal = stringItem->getDefault();
          read = GetPrivateProfileStringA(section, itemName, strDefVal, retbuf, 100, filename);
          sscanf(retbuf, " %hhu.%hhu.%hhu.%hhu ", &ipVByte[0], &ipVByte[1], &ipVByte[2], &ipVByte[3]);
          vfwPutU8(&(handle->b), ipVByte[0]);
          vfwPutU8(&(handle->b), ipVByte[1]);
          vfwPutU8(&(handle->b), ipVByte[2]);
          vfwPutU8(&(handle->b), ipVByte[3]);
          memset(ipVByte, 0, 5);
        }
        else
        {
          // Can't get here, itemType can only be one of the above...
          ;
        }
      }
    }

    // When all items have been written to the buffer, mark the buffer as full...
    vfwSetFullBuffer(&(handle->b));
    // and call the read callback
    handle->rh(handle, &(handle->b));
  }

  /*********************************************************************************
  * \brief           Write data to an NVSH file.
  *
  *                  This call is non-blocking.
  *                  Results will be received in the registered read handler
  *                  function.
  * \param [in]      handle The handle returned by vfwNvshAllocate() or vfwNvshAllocateConfiguration().
  * \exception       handle does not exist.
  * \exception       vfwStart() has not been called.
  * \exception       handle A read request is pending.
  ********************************************************************************/
  void vfwNvshWrite(const VFW_NvshHandle handle, VFW_Buffer *buffer)
  {
    //get the major and Minor version
    static_cast<void>(vfwGetU8(buffer)); // Major
    static_cast<void>(vfwGetU8(buffer)); // Minor

    const char_t* const section = handle->f;

    const char_t * itemName;
    ATC::BaseConfigItem::ItemDatatype itemType;
    ATC::ConfigFile * configFile;

    const char_t* const filename = getIniFileName(section);

    while (0U < vfwGetValidSize(buffer))
    {
      uint16_t id = vfwGetU16(buffer);
      uint32_t curTime = vfwGetU32(buffer);
      char_t timeBuf[32];
      static_cast<void>(snprintf(timeBuf, sizeof(timeBuf), "%d", curTime));

      // Try to find an item and get the info needed
      bool foundItem = ATC::AbstractConfigBase::basePtr()->getItemInfoForVfwSim(id, itemName, itemType, configFile);

      // If an item is found with that id.
      if (foundItem)
      {
        //NVSH write
        static_cast<void>(WritePrivateProfileStringA(section, itemName, timeBuf, filename));
      }
    }

  }


  /********************************************************************************
   * \brief           Get the filename related to an NVSH handle.
   *
   *                  Help function to get the same filename as was
   *                  supplied when calling vfwNvshAllocate() or vfwNvshAllocateConfiguration().
   *
   * \param [in]      handle An NVSH handle as allocated by vfwNvshAllocate().
   * \returns         A filename without path.
   *******************************************************************************/
  char_t * vfwNvshGetFilename(const VFW_NvshHandle handle)
  {
    return handle->f;
  }




  /**
  * \brief Allocate a cross-compare point.
  *
  * Cross-compare points and rendezvous points shall be allocated
  * in the same order on both A and B sides.
  *
  * \return A cross-compare point handle.
  * \exception vfwInit() not yet called.
  * \exception vfwStart() already called.
  * \exception Memory allocation fails.
  */
  VFW_CrossCompare
    vfwAllocateCrossCompare(void)
  {
    static uint8_t crossCompIndex = 0U;
    const uint16_t MAX_CROSS_COMPARE_POINTS = 3U;

    static struct vfw_i_CrossCompare crossCompareCallbacks[MAX_CROSS_COMPARE_POINTS];

    VFW_CrossCompare allocateCrossCompare = static_cast<VFW_CrossCompare>(NULL);

    // Check that vfwInitIsDone is true, but vfwStartIsDone is false
    VFW_ASSERT((vfwInitIsDone && (!vfwStartIsDone)), (("Called in wrong order")));
    // Check that we are not allocating too many cross compare points
    VFW_ASSERT((crossCompIndex < MAX_CROSS_COMPARE_POINTS), (("Too many cross compare points allocated!")));

    allocateCrossCompare = &crossCompareCallbacks[crossCompIndex];
    ++crossCompIndex;

    return allocateCrossCompare;
  }

  /**
  * \brief Register callback functions to be called at a specific
  *        cross-compare point, or at the global cross-compare point.
  *
  * For each Cross-compare point and rendezvous point,
  * callback functions shall be registered, using this function,
  * in the same order on both sides.
  *
  * Callback functions will be executed in the same order as
  * they were registered.
  *
  * Callback functions for the global cross-compare point will be executed
  * before callback functions for any specific cross-compare points.
  *
  * \param [in]  ccpoint Handle returned by vfwAllocateCrossCompare().
  *                      NULL means register the functions on
  *                      the global cross-compare point.
  * \param [in]  addData A function that adds the data to the
  *                      cross-compare message.
  * \param [in]  receiveData A function that receives the data
  *                          from the message from the partner.
  *              If NULL, a plain memory comparison will be made, and any
  *              difference will result in the system going to halt state.
  * \param [in]  size Maximum number of bytes of data that can be added
  *                   by the addData function.
  *
  * \pre ccpoint shall be a valid cross-compare point, when not NULL.
  * \exception vfwInit() not yet called.
  * \exception vfwStart() already called.
  * \exception size is zero.
  * \exception Memory allocation fails.
  */
  void vfwCrossCompareRegisterFunctions(VFW_CrossCompare ccpoint, VFW_CCAddData *addData, VFW_CCReceiveData *receiveData, uint32_t size)
  {
    // Check that vfwInitIsDone is true, but vfwStartIsDone is false
    VFW_ASSERT((vfwInitIsDone && (!vfwStartIsDone)), (("Called in wrong order")));

    ccpoint->addData = addData;
    ccpoint->receiveData = receiveData;
    ccpoint->compareSize = size;

  }


  /**
  * \brief Perform cross-comparison for the given cross-compare point, and for
  *        the global cross-compare point.
  *
  * When this function returns it is guaranteed that both sides have
  * successfully performed the data cross-comparison.
  *
  * \param [in]  ccpoint Handle returned by vfwAllocateCrossCompare().
  *                      NULL means use only the global cross-compare point.
  *
  * \pre ccpoint shall be a valid cross-compare point, when not NULL.
  * \exception vfwStart() not yet called.
  * \exception callback an exception occured in any of the receiveData callbacks.
  * \exception memcmp NULL was provided as a receiveData callback to
  *            vfwCrossCompareRegisterFunctions() and the plain memory comparison
  *            failed.
  */
  void vfwCrossCompare(VFW_CrossCompare ccpoint)
  {
    if (ccpoint == static_cast<VFW_CrossCompare> (NULL))
    {
      // NULL means use only the global cross-compare point, not simulated...
    }
    else
    {
      const uint32_t COMPARE_BUFFER_SIZE = 11000U;

      VFW_Buffer localDummyCCBuffer;
      uint8_t  localDummyBuffer[COMPARE_BUFFER_SIZE];
      const uint32_t bufferSize = ccpoint->compareSize;

      VFW_ASSERT((bufferSize <= COMPARE_BUFFER_SIZE), (("Buffer Size too large")));

      memset(localDummyBuffer, 0, sizeof(localDummyBuffer));
      vfwInitBuffer(&localDummyCCBuffer, localDummyBuffer, bufferSize);

      ccpoint->addData(&localDummyCCBuffer);

      VFW_Buffer localDummyCCBuffer2;
      uint8_t  localDummyBuffer2[COMPARE_BUFFER_SIZE];

      memmove(localDummyBuffer2, localDummyBuffer, bufferSize);
      memmove(&localDummyCCBuffer2, &localDummyCCBuffer, sizeof(VFW_Buffer));

      localDummyCCBuffer2.b = localDummyBuffer2;

      vfwSetFullBuffer(&localDummyCCBuffer);
      vfwSetFullBuffer(&localDummyCCBuffer2);

      if (ccpoint->receiveData == NULL)
      {
        compareCrossCompareData(&localDummyCCBuffer, &localDummyCCBuffer2);
      }
      else
      {
        ccpoint->receiveData(&localDummyCCBuffer, &localDummyCCBuffer2);
      }
    }
  }


  /**
  * \brief           Returns the number of bytes left in the buffer, based on the
  *                  current offset.
  * \param [in]      buffer Buffer to get free space of.
  * \return          Remaining bytes in buffer.
  * \note            For a fake buffer UINT32_MAX - the current offset will be returned.
  * \pre             buffer must have been initialized.
  * \exception       buffer is invalid.
  */
  uint32_t
    vfwGetFreeSpace(const VFW_Buffer * buffer)
  {
    vfwValidateBuffer(buffer);

    return (buffer->b_s - buffer->o);
  }

  /**
  * \brief           Returns the size of the buffer.
  * \param [in]      buffer Buffer to get size of.
  * \return          Buffer size.
  * \note            For a fake buffer UINT32_MAX - the current offset will be returned.
  * \pre             buffer must have been initialized.
  * \exception       buffer is invalid.
  */
  uint32_t vfwGetMaxSize(const VFW_Buffer* buffer)
  {
    vfwValidateBuffer(buffer);

    return buffer->b_s;
  }

  /**
  * \brief           Return an uint8_t pointer to the start of the byte array in buffer.
  * \param [in]      buffer Buffer to get pointer from.
  * \return          uint8_t pointer to start of buffer.
  * \pre             buffer must have been initialized.
  * \exception       buffer is invalid.
  * \exception       buffer is fake.
  */
  uint8_t            *
    vfwGetStart(const VFW_Buffer * buffer)
  {
    vfwValidateBuffer(buffer);

    return buffer->b;
  }

  /**
  * \brief           Consume data in buffer.
  *
  * \param [in,out]  buffer Buffer to consume.
  * \param [in]      size Number of bytes to consume.
  *
  * \pre             buffer must have been initialized.
  * \pre             buffer must be in read mode.
  * \post            The internal offset in the buffer has been incremented by size.
  * \exception       buffer is invalid.
  * \exception       buffer mode is write.
  * \exception       buffer trying to move offset outside buffer.
  * \see             VFW_BufferStatus vfwConsumeBufferCheck()
  */
  void
    vfwConsumeBuffer(VFW_Buffer * buffer, uint32_t size)
  {
    vfwValidateBuffer(buffer);

    if (buffer->m != BUFFER_READ_MODE) {
      VFW_HALT(("Illegal mode"));
    }
    else {
      if (buffer->v < (size + buffer->o)) {
        VFW_HALT(("Consuming outside buffer"));
      }

      vfwUpdateBuffer(buffer, size);
    }
  }



  /**
  * \brief        Visit a checkpoint.
  * \param [in, out]  cp The checkpoint identity returned from the call.
  * \param [in]   name A unique string that defines the checkpoint.
  * \exception    vfwStart() has not been called.
  * \exception    vfwInitCheckPoints() has not been called.
  * \exception    name length is zero.
  * \exception    vfwInitCheckPoints() has allocated too few checkpoints.
  * \note         The cp value should be 0 at first call for each checkpoint.
  */
  void vfwVisitCheckPoint(uint32_t *cp, const char * /*name*/)
  {
    (*cp)++;
  }

  /**
  * \brief Concatenate strings.
  *
  *  Guarantees to NUL-terminate the destination string for all
  *  strings where the given size is non-zero.
  * \param [out]  dst The destination.
  * \param [in]   src The source.
  * \param [in]   siz The size of dst.
  * \return       The total length of the created string.
  * \exception    dst is too small.
  */
  size_t vfw_strlcat(char *dst, const char *src, size_t siz)
  {
    // Get the length that is feasible to copy. This is the string length which will be copied in cpyLen.

    const size_t lenSrc = strnlen(src, siz);
    const size_t lenDst = strnlen(dst, siz);  //Length already having data, not size; size is known to be = siz
    size_t cpyLen;

    if (lenDst < (siz - 1))
    {
      cpyLen = siz - lenDst - 1;//Length available in dst string
      if (lenSrc < cpyLen)
      {
        cpyLen = lenSrc;
      }

      //Appends the cpyLen characters of src to dst, plus a terminating NULL-character.
      strncat(dst, src, cpyLen);
    }
    else
    {
      //Destination already full can not copy
      cpyLen = 0;
      dst[siz] = '\0'; //Making sure the dst is NULL terminated as it may not be earlier
    }

    //Reusing cpyLen to return value
    cpyLen = strnlen(dst, siz);   //Now, strnlen() will return correct value as the string dst will always be NULL terminated.

    return cpyLen;
  }

  /**
  * \brief Copy a string.
  *
  *  Guarantees to NUL-terminate the destination string for all
  *  strings where the given size is non-zero.
  * \param [out]  dst The destination.
  * \param [in]   src The source.
  * \param [in]   siz The size of dst.
  * \return       The total length of the created string.
  * \exception    dst is too small.
  */
  size_t vfw_strlcpy(char *dst, const char *src, size_t siz)
  {
    // Get the length that is feasible to copy. This is the string length which will be copied.
    // This will be size, not including the NULL character.
    const size_t lenSrc = strnlen(src, siz);

    VFW_ASSERT((lenSrc < siz), ("vfw_strlcpy ERROR: lenSrc >= siz"));

    strncpy(dst, src, lenSrc);
    dst[lenSrc] = '\0';
    // Do not append zero's any further as vfw_strcpy 'does not' zero fill their destination strings other than compulsory NULL
    // to terminated the string. Refer vfw_string File reference.

    return (lenSrc);
  }

  /**
  * \brief  Prints information and halts the system.
  *
  * This function
  * \li formats and prints information including time, file information,
  *     line number, function name, and a custom message, using syslog
  * \li calls the registred callback (if vfwAtHalt() has been called)
  * \li halts the calling process and sends
  *     a SIGTERM signal to the process group.
  *
  * Time is printed in the format YYYY-MM-DD hh:mm:ss.sss (ssssss.sss)
  * i.e. local time and master time as returned from vfwGetUnsynchronizedTime().
  *
  * When compiled with -DVFW_TEST_SYSTEM the function prints to stderr.
  * If not compiled with -DVFW_TEST_SYSTEM the function prints to stderr and syslog.
  *
  * \param [in] sourceFileInfo Information about the caller, such as filename.
  * \param [in] lineNumber The line number from where this function was called.
  * \param [in] functionName The name of the calling function.
  * \param [in] format A printf style format specifier.
  *
  * \note  Use macro VFW_HALT() or VFW_ASSERT()
  *        rather than calling this function directly.
  */
  /*lint -esym(960,16.1) MISRA 2004 Required Rule 16.1, function has variable number of arguments
  * The benefit of having a flexible and simple way to report diagnostics when terminating outweighs
  * the problems associated with variable number of argument semantics. */
  void vfwHalt(const char* /*sourceFileInfo*/, const integer_t /*lineNumber*/, const char* /*functionName*/, const char* format, ...)
    /*lint +esym(960,16.1) */
  {
    // Please note that file name and line number is added by aosHalt()
    // (done because VFW_HALT shows the wrong line number)

    char_t text[250];

    va_list args;
    va_start(args, format);
    int ret = vsnprintf(text, sizeof(text), format, args);
    va_end(args);

    if (ret <= 0)
    {
      text[0] = '\0';
    }

    struct tm utcTime;
    vfwGetTimeOfDay(static_cast<timespec *>(NULL), &utcTime);

    FILE *aosHaltFile = fopen("aosHalt.log", "a");
    if (aosHaltFile != NULL)
    {
      fprintf(aosHaltFile, "%04d-%02d-%02d %02d:%02d:%02d HALT in aosHalt: %s\n",
        utcTime.tm_year + 1900, utcTime.tm_mon + 1, utcTime.tm_mday, utcTime.tm_hour, utcTime.tm_min, utcTime.tm_sec, text);
      fflush(aosHaltFile);
      fclose(aosHaltFile);
    }
    exit(1);
  }

  /**
  * \brief   Prints information and halts the system.
  *
  * \li formats and prints information including file information,
  *     line number, function name, failed condition and a custom message,
  *     using syslog
  * \li calls the registred callback (if vfwAtHalt() has been called)
  * \li halts the calling process and sends
  *     a SIGTERM signal to the process group.
  *
  * Time is printed in the format YYYY-MM-DD hh:mm:ss.sss (ssssss.sss)
  * i.e. local time and master time as returned from vfwGetUnsynchronizedTime().
  *
  * When compiled with -DVFW_TEST_SYSTEM the function prints to stderr.
  * If not compiled with -DVFW_TEST_SYSTEM the function prints to stderr and syslog.
  *
  * \param [in] condition The condition to assert.
  * \param [in] sourceFileInfo Information about the caller, such as filename.
  * \param [in] lineNumber The line number from where this function was called.
  * \param [in] functionName The name of the calling function.
  * \param [in] format A printf style format specifier.
  *
  * \note  Use macro VFW_ASSERT()
  *        rather than calling this function directly.
  */
  /*lint -esym(960,16.1) MISRA 2004 Required Rule 16.1, function has variable number of arguments
  * The benefit of having a flexible and simple way to report diagnostics when terminating outweighs
  * the problems associated with variable number of argument semantics. */
  void vfwAssert(const char* condition, const char* sourceFileInfo, const integer_t lineNumber, 
    const char* /*functionName*/, const char* /*format*/, ...)
    /*lint +esym(960,16.1) */
  {
    printf("Assertion '%s' failed, file %s, line %d\n", condition, sourceFileInfo, lineNumber);
    exit(1);
  }

  /**
  * \brief Format an error string
  *
  * The function is intended for internal use.
  *
  * \param [in] format A printf style format specifier.
  * \return The formatted error string.
  */
  /*lint -esym(960,16.1) MISRA 2004 Required Rule 16.1, function has variable number of arguments */
  char *vfwBuildErrStr(const char* format, ...)
    /*lint +esym(960,16.1) */
  {
    static char         errorString[1000];
    va_list             argp;

    va_start(argp, format);     /*lint !e530 concequence from variable arguments */

    (void)vsnprintf(errorString, sizeof errorString, format, argp);

    va_end(argp);               /*lint !e950 concequence from variable arguments */

    return errorString;
  }


  /**
  * \brief Start VFW.
  *
  * Should be called after allocating resources,
  * but before using crosscompare, rendezvous, synchronization etc.
  *
  * This finalises VFW internal allocation of resources based on the resources
  * allocated between vfwInit() and vfwStart().
  *
  * As part of this process the global cross-compare point is executed
  * to check that VFW's internal data on the A and B side matches.
  * This means that all cross-comparison registered by the application on
  * the global cross-compare point will also be executed.
  *
  * \exception vfwInit() has not been called.
  * \exception vfwStart() has been called.
  * \exception callback handler is not registered for all synchronized timers.
  * \exception cross-comparison error in a diversified appplication.
  * \exception lack of system resources.
  * \sa vfw_identity.h, vfwChannel.h, vfw_crosscompare.h
  */
  void vfwStart(void)
  {
    // Check that vfwInitIsDone == true
    VFW_ASSERT((vfwInitIsDone && (!vfwStartIsDone)), (("Called in wrong order")));
    vfwStartIsDone = true;
  }

  /**
  * \brief Initialize VFW.
  *
  * Should be called before allocating resources, such as timers, channels, etc.
  * \exception vfwInit() has been called.
  * \exception environment variable VFW_FIFO_DIR has not been defined.
  * \exception environment variable VFW_REDUNDANT_SIDE has not been defined
  *            to "LEFT" or "RIGHT".
  * \exception vfwSetSide() has not been called.
  * \exception vfwSetApplicationName() has not been called.
  * \exception vfwSetUniqId() has not been called.
  * \exception lack of system resources.
  * \sa getenv()
  */
  void vfwInit(void)
  {
    readSimulationParameters();
#ifdef _DEBUG
    printSimulationParameters();
#endif

    VFW_ASSERT(((!vfwInitIsDone) && (!vfwStartIsDone)), (("Called in wrong order")));

    vfwInitIsDone = true;
  }


  /**
  * \brief       Set the callback to be called when data is available on the
  *              channel.
  *
  * \param [in]  channel The channel handle returned by vfwSyncAddChannel().
  * \param [in]  handler The function to be called when data is available.
  * \param [in]  data A pointer which will be passed as the data parameter of the
  *              handler. May be NULL.
  *
  * \exception   vfwInit() has not been called.
  * \exception   vfwStart() has been called in a diversified process.
  * \exception   handler is set.
  * \pre         channel is a valid VFW_ChannelDesc.
  */
  void vfwSyncChannelSetHandler(VFW_SyncChannel /*channel*/, VFW_SyncChannelHandler* /*handler*/, void* /*data*/)
  {
    // TODO 
  }

  /**
  * \brief       Add a timer to be handled by the synchronizer.
  *
  *              The timer shall henceforth be accessed through the vfwSync API
  *              and referenced by the returned handle.
  *
  *              Timers added with the synchronized parameter set are
  *              matched up and cross-compared between the A and B side.
  *
  *              The application is expected to handle synchronized timers
  *              equally on the A and B side or the cross-comparison will fail.
  *
  *              vfwStart() checks the mandatory callback registration for
  *              all synchronized timers.
  *
  * \return      A handle to be used in later calls to the vfwSync API.
  *
  * \param [in]  timer The timer handle returned by vfwTimerAllocate()
  * \param [in]  synchronized Whether the timer is synchronized or not.
  *
  * \exception   vfwInit() has not been called.
  * \exception   vfwStart() has been called in a diversified process.
  * \exception   synchronized is set in a non-diversified process.
  * \pre         timer is a valid VFW_Timer.
  */
  VFW_SyncTimer vfwSyncAddTimer(VFW_Timer /*timer*/, bool_t /*synchronized*/)
  {
    vfw_i_SyncTimer * time = new vfw_i_SyncTimer();
    return time;
  }

  void vfwSyncTimerStart(VFW_SyncTimer /*timer*/)
  {

  }

  void vfwWatchdogKick(uint32_t /*timeout*/)
  {

  }

  int32_t             vfwApiVersionMajor(void)
  {
    return VFW_API_VERSION_MAJOR;
  }

  int32_t             vfwApiVersionMinor(void)
  {
    return VFW_API_VERSION_MINOR;
  }

  /**
  * \brief       Set the callback to be called when the timer expires.
  *
  * \param [in]  timer The timer handle returned by vfwSyncAddTimer().
  * \param [in]  handler The function to be called when the timer expires.
  * \param [in]  data A pointer which will be passed as the data parameter of  the
  *              handler. May be NULL.
  *
  * \exception   vfwInit() has not been called.
  * \exception   vfwStart() has been called in a diversified process.
  * \exception   handler is set.
  * \pre         timer is a valid VFW_SyncTimer.
  * \pre         handler is a valid VFW_SyncTimerHandler.
  */
  void
  vfwSyncTimerSetHandler(VFW_SyncTimer timer, VFW_SyncTimerHandler* handler,
      void* /*data*/)
  {
    atpMainLoop = handler;
    atpMainLoopTimer = timer;
  }
  /**
  * \brief       Set the timeout of the timer in milliseconds.
  *
  * \param [in]  timer The timer handle returned by vfwSyncAddTimer().
  * \param [in]  timeout The new timeout in milliseconds.
  *
  * \exception   vfwInit() has not been called.
  * \pre         timer is a valid VFW_SyncTimer.
  * \note This function calls vfwTimerSetTimeout() for the corresponding timer.
  *
  */
  void vfwSyncTimerSetTimeout(VFW_SyncTimer /*timer*/, int64_t /*timeout*/)
  {
    //TODO
  }

  /**
  * \brief  Set a tolerance for a timer's timeout.
  *
  * A timer's tolerance is taken into account when checking
  * the timer for expiry, or
  * when calculating the time left. If the timer is checked later than
  * the given tolerance after its expiry, the system will
  * enter halt state.
  * The tolerance will be effective immediately.
  *
  * \param  [in] timer The timer handle returned by vfwSyncAddTimer().
  * \param  msTolerance An absolute tolerance in milliseconds.
  *         The value zero means no tolerance check will be done.
  *
  * \exception vfwInit() has not been called.
  * \pre       timer is a valid VFW_SyncTimer.
  * \note This function calls vfwTimerSetTimeoutTolerance() for the corresponding timer.
  */
  void vfwSyncTimerSetTimeoutTolerance(VFW_SyncTimer /*timer*/, int32_t /*msTolerance*/)
  {
  }

  /**
  * \brief       Restart the timer when expired compensated for overscheduling.
  *
  * \param [in]  timer The timer handle returned by vfwSyncAddTimer().
  *
  * \exception   vfwStart() has not been called.
  * \pre         timer is a legal VFW_SyncTimer.
  * \note   This must be done within the callback, since if the timer is neither
  *         restarted or stopped within the callback it will be automatically
  *         stopped after the callback has returned.
  * \note This function calls vfwTimerReactivateCyclic() for the corresponding timer.
  */
  void vfwSyncTimerReactivateCyclic(VFW_SyncTimer /*timer*/)
  {
    //TODO
  }
  /**
  * \brief       Perform synchronization.
  *
  *              The vfwSync() function will block until there is data to read on
  *              either a channel or a file descriptor and/or a timer has
  *              expired. If an event has happened on a synchronized entity the
  *              function will unblock on both A and B.
  *              When the function returns it will do so guaranteeing that
  *              synchronized channels and timers will yield the same results in
  *              the local machine as on the partner
  *              if they are accessed through the vfwSync API.
  *
  *              If there is an event on an unsynchronized entity only, the
  *              function will unblock only on the side in which the event
  *              occurred.
  *
  *              Before the function returns callbacks associated with entities
  *              will be called as described under \b Event \b Dispatching.
  *
  *              In a non-diversified system no synchronization is done, but the
  *              event-dispatching
  *              part can still be used to implement an event driven system.
  *
  * \exception   vfwStart() has not been called.
  * \exception   vfwSetMaxTimeDiff() has not been called (only for diversified applications).
  */
  void vfwSync(void)
  {
    const uint64_t startTimeInMs = ATC::getUTCTimeInMs();

    simVfwStartCycle();

    VFW_ASSERT(((atpMainLoop)), (("No main loop set!")));

    atpMainLoop(atpMainLoopTimer, NULL);

    const uint64_t elapsedTime = ATC::getUTCTimeInMs() - startTimeInMs;

    if (elapsedTime < 100U)
    {
      const uint64_t sleepTime = 100U - elapsedTime;

      Sleep(static_cast<uint32_t>(sleepTime));
    }
  }

  /**
  * \brief      Sets the "diverse" side of the current process instance.
  * \param [in] side Indicates the side of the current process.
  * \exception  vfwSetSide() has already been called.
  * \exception  side is illegal.
  */
  void vfwSetSide(VFW_Side /*side*/)
  {
    //TODO
  }

  void vfwSetApplicationName(const char_t* /*baseName*/, int32_t /*instance*/)
  {
    //TODO
  }

  void vfwSetUniqId(const char* /*id*/)
  {
    //TODO
  }
  void vfwSetMaxTimeDiff(uint32_t /*maxTimeDiff*/)
  {
    //TODO
  }
  void vfwAtHalt(VFW_AtHalt* /*func*/)
  {
    //TODO
  }

  /**
  * \brief        Initialize the checkpoint internal buffers.
  *
  *               Visited checkpoints are saved in an internal log in case of cross-compare mismatch.
  *               The internal log must in this case be examined with a debugger.
  * \param [in]   maxCheckpoints The max number of checkpoints that can be used.
  * \param [in]   maxCheckpointsLog The max number of checkpoints saved in internal log.
  * \exception    vfwInit() has not been called.
  * \exception    vfwStart() has been called.
  * \exception    vfwInitCheckPoints() has already been called.
  * \exception    vfwGetSide() is neither VFW_A_SIDE nor VFW_B_SIDE.
  * \exception    maxCheckpoints can not be allocated.
  * \exception    maxCheckpointsLog can not be allocated.
  */
  void vfwInitCheckPoints(uint32_t /*maxCheckpoints*/, uint32_t /*maxCheckpointsLog*/)
  {

  }

  void vfwChannelSetOverwritable(const VFW_ChannelDesc /*chDesc*/)
  {
    //TODO
  }

  const char* vfwVersion(void)
  {
     return "1.3";
  }


  void vfwCalcBlockedCrc64(const VFW_Buffer* inbuf, VFW_Buffer* crcbuf, uint32_t blockSize)
  {
    uint32_t bytesLeftToCrc = vfwGetValidSize(inbuf);
    const bool isOK = (inbuf  != static_cast<VFW_Buffer*>(NULL)) && (bytesLeftToCrc > 0U)  && 
                      (crcbuf != static_cast<VFW_Buffer*>(NULL)) && (blockSize != 0U);

    VFW_ASSERT((isOK), (("vfwCalcBlockedCrc64: Illegal parameters")));

    const uint8_t* p = vfwGetPointer(inbuf);

    while (bytesLeftToCrc > 0U)
    {
      const uint32_t dataSizeForCrc = minimum(blockSize, bytesLeftToCrc);

      // Add the CRC to the cross compare buffer...
      vfwPutU64(crcbuf, calcCrc64(p, dataSizeForCrc));

      p += dataSizeForCrc;

      bytesLeftToCrc -= dataSizeForCrc;
    }
  }
} // extern "C"

void simVfwStartCycle()
{
  ++cycleCounter;
}


//-- Blank below...
