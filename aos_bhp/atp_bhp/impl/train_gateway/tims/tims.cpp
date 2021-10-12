/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2017
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
* DESCRIPTION:
* This file implements the methods of the TIMS class.

******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2017-05-29    akushwah    Created

*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "tims.hpp"
#include "tims_event_ids.hpp"
#include "abstract_tsetup.hpp"
#include "abstract_tracks.hpp"
#include "vehicle_com.hpp"
#include "loco_io.hpp"
#include "config.hpp"
#include "abstract_cross_compare.hpp"
#include "cross_compare_complex.hpp"
#include <vfw_checkpoints.h>
#include <stdio.h>

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace
{
  uint32_t convertPsiToPa(const uint8_t pressureInPsi)
  {
    return static_cast<uint32_t>(pressureInPsi) * 6895U; // [PSI] to [Pa]
  }
}

namespace ATP
{
  namespace TG
  {

    /******************************************************************************
    * Constructor
    ******************************************************************************/
    TIMS::TIMS() : AbstractTIMS(),
      invalidObrdEvent(ATC::Event::createLogEvent(
        atpTimsId, ATC::AdaptationContainer, eventIdObrdOutsideTrain, 0x0U, "OBRD reported position outside train extent: ", true)),
      obrdMessageHandler(
        (vfwGetSide() == VFW_A_SIDE) ? ATC::obrdChannelDispToATPA : ATC::obrdChannelDispToATPB,
        (vfwGetSide() == VFW_A_SIDE) ? ATC::obrdChannelATPAToDisp : ATC::obrdChannelATPBToDisp,
        AbstractTIMS::corePtr()->getTrace()),
      obrdStatusReportTime(0),
      validObrdReportReceived(false),
      obrdBrakePressureReceived(false),
      ecpbConfirmedTime(0U),
      ecpbStatusReportTime(0),
      ecpbReportReceived(false),
      ecpbIsUsed(false)
    {
    }

    /******************************************************************************
    * instance
    ******************************************************************************/
    TIMS& TIMS::instance()
    {
      static TIMS theOnlyTIMSInstance;
      return theOnlyTIMSInstance;
    }

    /******************************************************************************
    * init
    ******************************************************************************/
    bool TIMS::init()
    {
      const bool baseInit = AbstractTIMS::init();
      const bool obrdMessageHandlerInit = obrdMessageHandler.init();

      return baseInit && obrdMessageHandlerInit;
    }

    /******************************************************************************
    * preInit
    ******************************************************************************/
    void TIMS::preInit()
    {
      obrdMessageHandler.preInit();
    }

    /******************************************************************************
    * initCrossCompare
    ******************************************************************************/
    void TIMS::initCrossCompare() const
    {
      //lint --e{586} 'new' is acceptable during initialization

      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareComplex<ATC::Event>(&invalidObrdEvent));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint16(&obrdStatusReport.unitTrackAndPos.track));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint32(&obrdStatusReport.unitTrackAndPos.position));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint8(&obrdStatusReport.lastCarBrakePressure));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint64(&obrdStatusReport.timeOfMeasurement));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&obrdStatusReportTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&validObrdReportReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&obrdBrakePressureReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareUint64(&ecpbConfirmedTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareInt64(&ecpbStatusReportTime));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&ecpbReportReceived));
      Support::AbstractCrossCompare::corePtr()->addCrossCompareData(new Support::CrossCompareBool(&ecpbIsUsed));

      AbstractTIMS::initCrossCompare();
    }

    /******************************************************************************
    * runIn
    ******************************************************************************/
    void TIMS::runIn()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TIMS_runIn");

      // Read incoming OBRD messages
      obrdMessageHandler.runIn();
    }

    /******************************************************************************
    * run
    ******************************************************************************/
    void TIMS::run()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TIMS_run");

      checkECPBInput();
      checkOBRDInput();

      AbstractTIMS::run();
    }

    /******************************************************************************
    * runOut
    ******************************************************************************/
    void TIMS::runOut()
    {
      static uint32_t cp = 0U; // Must be initialized to 0
      vfwVisitCheckPoint(&cp, "TIMS_runOut");

      // Send outgoing OBRD messages
      obrdMessageHandler.runOut();
    }

    /******************************************************************************
    * updateTimsAvailable
    ******************************************************************************/
    void TIMS::updateTimsAvailable()
    {
      const DS::TrainSetup* trainSetup = DS::AbstractTSetup::corePtr()->getTrainSetup();
      if (trainSetup != NULL)
      {
        /*TIMS availability shall be as defined by TCC in the TrainSetup message, Q_TIMS_SUPERVISION.
         the value of TIMS availability will be timsSupNeeded
        */
        setTimsAvailable(trainSetup->timsSupNeeded);
      }
      /*if train setup is not available then check for train integrity status from ECPB */
      else if (ecpbReportReceived)
      {
        //TIMS is available when brake system in use is ECPB
        setTimsAvailable(true);
      }
      else
      {
        //TIMS is not available when brake system in use is Pneumatic
        setTimsAvailable(false);
      }
    }

    /******************************************************************************
    * updateTimsStatus
    ******************************************************************************/
    void TIMS::updateTimsStatus()
    {
      if (getTimsSupervision() != TIMSNotSupervised)
      {
        if (!checkBrakePressureLoco())
        {
          setTimsStatus(TIMSStatusBroken);
        }
        else if (ecpbIsUsed)
        {
          updateECBPStatus();
        }
        else // OBRD
        {
          updateOBRDStatus();
        }
      }

      AbstractTIMS::updateTimsStatus();
    }

    /******************************************************************************
    * getAutomatedReportTime
    ******************************************************************************/
    uint64_t TIMS::getAutomatedReportTime()
    {
      uint64_t time_;

      if (ecpbIsUsed && ecpbReportReceived)
      {
        time_ = ecpbConfirmedTime;
      }
      else if ((!ecpbIsUsed) && validObrdReportReceived)
      {
        time_ = obrdStatusReport.timeOfMeasurement;
      }
      else
      {
        time_ = 0U;
      }

      return time_;
    }

    /******************************************************************************
    * checkECPBInput
    ******************************************************************************/
    void TIMS::checkECPBInput()
    {
      ecpbIsUsed = false;
      ecpbReportReceived = false;

      EcpbOperatingModesType ecpbOperatingMode;
      if (VehicleCom::instance().getEcpbOperatingModes(ecpbOperatingMode))
      {
        if (ecpbOperatingMode == EcpbOperatingRunMode)
        {
          const LocoTypeAdap locoType = static_cast<LocoTypeAdap>(Config::instance().getLocoType());
          const BrakeSystemType brakeSystem = DS::AbstractTSetup::corePtr()->getBrakeSystemInUse();

          // Brake system type 2 if the stored brake system in use is ECPB
          ecpbIsUsed = (locoType == EMD) && (brakeSystem == BrakeSystemType2);
        }
      }

      TrainIntegrityStatusEcpbType ecpbIntegrityStatus;
      uint64_t timeStamp;
      if (VehicleCom::instance().getTrainIntegrityStatusEcpb(ecpbIntegrityStatus, timeStamp))
      {
        if (ecpbIntegrityStatus == TrainIntegrityConfirmed)
        {
          ecpbReportReceived = true;
          ecpbConfirmedTime = timeStamp;
          ecpbStatusReportTime = vfwGetReferenceTime();
        }
      }

      if (getTimsSupervision() == TIMSNotSupervised)
      {
        ecpbStatusReportTime = 0; // Disables the timeout
      }
    }

    /******************************************************************************
    * checkOBRDInput
    ******************************************************************************/
    void TIMS::checkOBRDInput()
    {
      validObrdReportReceived = false;
      obrdBrakePressureReceived = false;

      OBRDUnitStatusReport obrdReport;
      if (obrdMessageHandler.getStatusReport(obrdReport))
      {
        obrdStatusReport = obrdReport;
        obrdBrakePressureReceived = true;

        if (positionReport.posKnown)
        {
          bool obrdPositionWithinTrain = false;

          if (obrdReport.unitTrackAndPos.track != 0U)
          {
            OdoPosition obrdPos;
            OdoPosition largerPos;
            OdoPosition smallerPos;

            const bool ok = DS::AbstractTracks::corePtr()->getOdoPos(obrdReport.unitTrackAndPos, obrdPos);
            if (ok)
            {
              if (positionReport.frontPos > positionReport.rearPos)
              {
                largerPos = positionReport.frontPos;
                smallerPos = positionReport.rearPos;
              }
              else
              {
                largerPos = positionReport.rearPos;
                smallerPos = positionReport.frontPos;
              }

              OdoPosition obrdLengthMargin = static_cast<OdoPosition>(Config::instance().getOBRDLengthMargin()) * 100; // [cm]
              obrdPositionWithinTrain =
                ((largerPos + obrdLengthMargin) >= obrdPos) && (obrdPos >= (smallerPos - obrdLengthMargin));
            }
          }

          if (obrdPositionWithinTrain)
          {
            validObrdReportReceived = true;
            obrdStatusReportTime = vfwGetReferenceTime();
          }
          else
          {
            char_t buf[100];

            const int32_t ret = snprintf(&buf[0], sizeof(buf), "track %u, position %u",
              obrdReport.unitTrackAndPos.track, obrdReport.unitTrackAndPos.position);

            if ((ret <= 0) || (static_cast<size_t>(ret) >= sizeof(buf)))
            {
              buf[0] = '\0';
            }

            invalidObrdEvent.setDynamicText(&buf[0]);
            ATC::AbstractEventHandler::corePtr()->reportEvent(invalidObrdEvent, __FILE__, __LINE__);
          }
        }
      }

      if (getTimsSupervision() == TIMSNotSupervised)
      {
        obrdStatusReportTime = 0; // Disables the timeout
      }
    }

    /******************************************************************************
    * updateECBPStatus
    ******************************************************************************/
    void TIMS::updateECBPStatus()
    {
      if (ecpbReportReceived)
      {
        // Note: Brake loco pressure was confirmed earlier
        setTimsConfirmed(true);
      }
      else if (ecpbStatusReportTime != 0)
      {
        const int64_t timeNow = vfwGetReferenceTime();
        //lint -e{1960} Cast changes signedness - but we have to change sign here
        // (uint64_t is problematic on ARM - uint32_t should be large enough)
        const uint32_t timeSinceLastReport = static_cast<uint32_t>(timeNow - ecpbStatusReportTime);

        if ((timeSinceLastReport / 1000U) >= Config::instance().getTimsEcbpTimeOut())
        {
          // Report timed out
          setTimsStatus(TIMSStatusBroken);
        }
      }
      else
      {
        // No report received, so do nothing
      }
    }

    /******************************************************************************
    * updateOBRDStatus
    ******************************************************************************/
    void TIMS::updateOBRDStatus()
    {
      if (validObrdReportReceived)
      {
        bool positionValid = false;
        bool positionOk = false;
        checkObrdPosition(positionValid, positionOk);

        bool pressureValid = false;
        bool pressureOk = false;
        checkBrakePressureLastCar(pressureValid, pressureOk);

        if (positionValid && positionOk && pressureValid && pressureOk)
        {
          // Note: Brake loco pressure was confirmed earlier
          setTimsConfirmed(true);
        }

        if ((positionValid && (!positionOk)) || (pressureValid && (!pressureOk)))
        {
          setTimsStatus(TIMSStatusBroken);
        }
      }
      else if (obrdStatusReportTime != 0)
      {
        const int64_t timeNow = vfwGetReferenceTime();
        //lint -e{1960} Cast changes signedness - but we have to change sign here
        // (uint64_t is problematic on ARM - uint32_t should be large enough)
        const uint32_t timeSinceLastReport = static_cast<uint32_t>(timeNow - obrdStatusReportTime);

        if ((timeSinceLastReport / 1000U) >= Config::instance().getTimsEotTimeOut())
        {
          // Report timed out
          setTimsStatus(TIMSStatusBroken);
        }
      }
      else
      {
        // No report received, so do nothing
      }
    }

    /******************************************************************************
    * checkBrakePressureLoco
    ******************************************************************************/
    bool TIMS::checkBrakePressureLoco() const
    {
      bool brakePressureOk = true;

      const uint8_t pressureSensorsAvailable = Config::instance().getEbFbSignalStatus();

      if ((pressureSensorsAvailable & eBFeedbackSignalStatusFirstEbFbOnly) != 0U) // sensor 1 is in use
      {
        uint16_t brakePressure;
        bool success = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure1, brakePressure);
        if (success)
        {
          if (brakePressure < Config::instance().getTimsMinBPLoco()) // [kPa]
          {
            brakePressureOk = false;
          }
        }
        else
        {
          trace.write(ATC::briefTrace, "Invalid Brake pressure 1 values received");
        }
      }

      if ((pressureSensorsAvailable & eBFeedbackSignalStatusSecondEbFbOnly) != 0U) // sensor 2 is in use
      {
        uint16_t brakePressure;
        bool success = IO::LocoIO::instance().getAdapAnalogInputValue(IO::LocoIO::BrakePressure2, brakePressure);
        if (success)
        {
          if (brakePressure < Config::instance().getTimsMinBPLoco()) // [kPa]
          {
            brakePressureOk = false;
          }
        }
        else
        {
          trace.write(ATC::briefTrace, "Invalid Brake pressure 2 values received");
        }
      }

      return brakePressureOk;
    }

    /******************************************************************************
    * checkLastCarBPDrop
    ******************************************************************************/
    void TIMS::checkLastCarBPDrop(bool& pressureValid, bool& pressureDropped) const
    {     
      if (obrdBrakePressureReceived &&
        (obrdStatusReport.lastCarBrakePressure != lastCarBrakePressureNotAsserted))
      {
        const uint32_t lastCarBrakePressure = convertPsiToPa(obrdStatusReport.lastCarBrakePressure); // [Pa]
        pressureDropped = lastCarBrakePressure < (static_cast<uint32_t>(Config::instance().getOBRDBrakeTestPressure()) * 1000U); // [Pa]
        pressureValid = true;
      }
      else
      {
        pressureValid = false;
      }
    }

    /******************************************************************************
    * checkBrakePressureLastCar
    ******************************************************************************/
    void TIMS::checkBrakePressureLastCar(bool& pressureValid, bool& pressureOk) const
    {
      if (obrdBrakePressureReceived &&
        (obrdStatusReport.lastCarBrakePressure != lastCarBrakePressureNotAsserted))
      {
        const uint32_t lastCarBrakePressure = convertPsiToPa(obrdStatusReport.lastCarBrakePressure); // [Pa]
        pressureOk = lastCarBrakePressure >= (static_cast<uint32_t>(Config::instance().getTimsMinBPLastCar()) * 1000U); // [Pa]
        pressureValid = true;
      }
      else
      {
        pressureValid = false;
      }
    }

    /******************************************************************************
    * checkObrdPosition
    ******************************************************************************/
    void TIMS::checkObrdPosition(bool& positionValid, bool& positionOk) const
    {
      positionValid = false;

      PositionSample sample;
      const uint64_t timeInMillis = obrdStatusReport.timeOfMeasurement * 1000U;
      bool ok = getLastCarPositionAtTime(timeInMillis, sample);

      if (ok && sample.valid && (obrdStatusReport.unitTrackAndPos.track != 0U))
      {
        OdoPosition obrdPos;
        ok = DS::AbstractTracks::corePtr()->getOdoPos(obrdStatusReport.unitTrackAndPos, obrdPos);

        if (ok)
        {
          OdoPosition distanceBehindLastCar = sample.lastCarPosition - obrdPos; // [cm]
          OdoPosition lengthMargin = static_cast<OdoPosition>(Config::instance().getTimsLengthMargin()) * 100; // [cm]

          positionOk = distanceBehindLastCar <= lengthMargin;
          positionValid = true;
        }
        else
        {
          trace.write(ATC::briefTrace, "OBRD track/position not found");
        }
      }
    }
  }
}
