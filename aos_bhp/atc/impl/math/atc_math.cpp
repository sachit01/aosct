/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*  This file contains safe integer math functions
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-22    Hidaji      Created
* 2016-09-28    bhidaji     Added km/h conversion to cm/s and vise-versa
* 2017-01-11    nsyed       Refactored into class ATCMath
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_math.hpp"
#include "atc_util.hpp"
#include "dmi_atc_event_codes.hpp"
#include "atc_math_event_ids.hpp"

/******************************************************************************
* EXTERNAL REFERENCES
******************************************************************************/

/******************************************************************************
* LOCAL DECLARATIONS
******************************************************************************/

/******************************************************************************
* LOCAL FUNCTION PROTOTYPES
******************************************************************************/
namespace ATC
{

  /******************************************************************************
  * Constructor
  ******************************************************************************/
  ATCMath::ATCMath():
  // creating different set of objects for different type of events
  errorSignMul1(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrSignMul, NoEB,
    DMICom::atcMathInternalCalculationFailure, "Sign multiply overflow!")),
  errorSignMulOpACast(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrSignMulOpACast, NoEB,
    DMICom::atcMathInternalCalculationFailure, "Error in casting from unsigned to signed operand a!")),
  errorSignMulOpBCast(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrSignMulOpBCast, NoEB,
    DMICom::atcMathInternalCalculationFailure, "Error in casting from unsigned to signed operand B!")),
  errorUnsignedDivisionByZero(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrUnSignDivZero,
    NoEB, DMICom::atcMathInternalCalculationFailure, "Error divide by zero!")),
  errorSignDivDivisorOverflow(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrSignDivDivisorOverflow,
    NoEB, DMICom::atcMathInternalCalculationFailure, "Error divide divisor overflow!")),
  errorSignDivDividendOverflow(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrSignDivDividendOverflow,
    NoEB, DMICom::atcMathInternalCalculationFailure, "Error dividend divisor overflow!")),
  errorAbsOverflow(Event::createSafetyHaltEvent(atcMathId, CommonContainer, eventIdErrAbsOverflow,
    NoEB, DMICom::atcMathInternalCalculationFailure, "Error absolute function overflow!"))
  {
    // Nothing to be done here.
  }

  /******************************************************************************
  * instance
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::instance) May be used in other projects
  ATCMath& ATCMath::instance(void)
  {
    static ATCMath ATCMathInstance;

    return ATCMathInstance;
  }

  /******************************************************************************
  * sqrtr square root rounded
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::sqrtr) May be used in other projects
  uint32_t ATCMath::sqrtr(uint32_t op)
  {
    uint32_t res = 0U;
    uint32_t one = 1UL << 30; // The second-to-top bit is set: use 1u << 14 for uint16_t type; use 1uL<<30 for uint32_t type

                              // "one" starts at the highest power of four <= than the argument.
    while (one > op)
    {
      one >>= 2;
    }

    while (one != 0U)
    {
      if (op >= (res + one))
      {
        op = op - (res + one);
        res = res + (2U * one);
      }
      res >>= 1;
      one >>= 2;
    }

    // Do arithmetic rounding to nearest integer 
    if (op > res)
    {
      res++;
    }

    return res;
  }


  /******************************************************************************
  * sqrtr square root rounded for 64 bit
  ******************************************************************************/
  uint32_t ATCMath::sqrtr(uint64_t op)
  {
    uint64_t res = 0U;
    uint64_t one = 1U;
    one <<= 62; // The second-to-top bit is set: use 1ull << 62 
                                 // "one" starts at the highest power of four <= than the argument.
    while (one > op)
    {
      one >>= 2;
    }

    while (one != 0U)
    {
      if (op >= (res + one))
      {
        op = op - (res + one);
        res = res + (2U * one);
      }
      res >>= 1;
      one >>= 2;
    }

    // Do arithmetic rounding to nearest integer 
    if (op > res)
    {
      res++;
    }

    return static_cast<uint32_t>(res);
  }

  /******************************************************************************
  * signMul with signed inputs
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::signMul) May be used in other projects
  int64_t ATCMath::signMul(const int32_t a, const int32_t b, const char_t* const file, const int32_t line) const
  {
    const int64_t p = static_cast<int64_t>(a) * static_cast<int64_t>(b);

    if (a != 0)
    {
      const int32_t q = static_cast<int32_t>(p / a);
      if (q != b)
      {
        //Log event or trace!
        AbstractEventHandler::corePtr()->reportEvent(errorSignMul1, file, line);
      }
    }
    return p;
  }

  /******************************************************************************
  * signMul with unsigned inputs
  ******************************************************************************/
  int64_t ATCMath::signMul(const uint32_t a, const uint32_t b, const char_t* const file, const int32_t line) const
  {
    int32_t a_int = static_cast<int32_t>(a);
    int32_t b_int = static_cast<int32_t>(b);
    int64_t result = 0;

    if ((a_int < 0) || (b_int < 0))   //   or it can for instance be: if a is greater than INT32_MAX 
    {
      if (a_int < 0)
      {
        AbstractEventHandler::corePtr()->reportEvent(errorSignMulOpACast, file, line);
      }
      if (b_int < 0)
      {
        AbstractEventHandler::corePtr()->reportEvent(errorSignMulOpBCast, file, line);
      }
      result = 0;
    }
    else
    {
      result = signMul(a_int, b_int, file, line);
    }

    return result;
  }

  /******************************************************************************
  * signMul with unsigned and signed inputs
  ******************************************************************************/
  int64_t ATCMath::signMul(const uint32_t a, const int32_t b, const char_t* const file, const int32_t line) const
  {
    int32_t a_int = static_cast<int32_t>(a);

    int64_t result = 0;

    if (a_int < 0)
    {
      result = 0;
      AbstractEventHandler::corePtr()->reportEvent(errorSignMulOpACast, file, line);
    }
    else
    {
      result = signMul(a_int, b, file, line);
    }

    return result;
  }

  /******************************************************************************
  * signMul with signed and unsigned inputs
  ******************************************************************************/
  int64_t ATCMath::signMul(const int32_t a, const uint32_t b, const char_t* const file, const int32_t line) const
  {
    int32_t b_int = static_cast<int32_t>(b);

    int64_t result = 0;

    if (b_int < 0)
    {
      result = 0;
      AbstractEventHandler::corePtr()->reportEvent(errorSignMulOpBCast, file, line);
    }
    else
    {
      result = signMul(a, b_int, file, line);
    }

    return result;
  }

  /******************************************************************************
  * signDiv with signed inputs
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::signDiv) May be used in other projects
  int32_t ATCMath::signDiv(int32_t dividend, int32_t divisor, const char_t* const file, const int32_t line) const
  {
    bool negative = false;
    int32_t result = 0;
    if (dividend < 0)
    {
      dividend = -dividend;
      negative = true;
    }

    if (divisor < 0)
    {
      divisor = -divisor;
      if (true == negative)
      {
        negative = false;
      }
      else
      {
        negative = true;
      }
    }

    if ((divisor < 0) || (dividend < 0))
    {
      if (divisor < 0)
      {
        AbstractEventHandler::corePtr()->reportEvent(errorSignDivDivisorOverflow, file, line);
        result = 0;
      }
      if (dividend < 0)
      {
        AbstractEventHandler::corePtr()->reportEvent(errorSignDivDividendOverflow, file, line);
        result = 0;
      }
    }
    else
    {
      result = static_cast<int32_t>(unsignDiv(static_cast<uint32_t>(dividend), static_cast<uint32_t>(divisor), file, line));
    }
    if (true == negative)
    {
      result = -result;
    }
    return result;
  }

  /******************************************************************************
  * signDiv with 64 bit signed inputs
  ******************************************************************************/
  int32_t ATCMath::signDiv(int64_t dividend, int32_t divisor, const char_t* const file, const int32_t line) const
  {
    bool negative = false;
    int32_t result = 0;
    if (dividend < 0)
    {
      dividend = -dividend;
      negative = true;
    }

    if (divisor < 0)
    {
      divisor = -divisor;
      if (true == negative)
      {
        negative = false;
      }
      else
      {
        negative = true;
      }
    }

    if ((divisor < 0) || (dividend < 0))
    {
      if (divisor < 0)
      {
        AbstractEventHandler::corePtr()->reportEvent(errorSignDivDivisorOverflow, file, line);
        result = 0;
      }
      if (dividend < 0)
      {
        AbstractEventHandler::corePtr()->reportEvent(errorSignDivDividendOverflow, file, line);
        result = 0;
      }
    }
    else
    {
      result = static_cast<int32_t>(unsignDiv(static_cast<uint64_t>(dividend), static_cast<uint32_t>(divisor), file, line));
    }
    if (true == negative)
    {
      result = -result;
    }
    return result;
  }

  /******************************************************************************
  * unsignDiv with 32 bit inputs
  ******************************************************************************/
  uint32_t ATCMath::unsignDiv(const uint32_t dividend, const uint32_t divisor, const char_t* const file, const int32_t line) const
  {
    uint32_t quotient;
    uint32_t remainder;

    if (divisor == 0U)
    {
      // divide by zero error
      AbstractEventHandler::corePtr()->reportEvent(errorUnsignedDivisionByZero, file, line);
      quotient = 0U;
    }
    else
    {
      quotient = dividend / divisor;
      remainder = dividend % divisor;

      // if remainder * 2 is greater than or equal to the divisor it means that remainder is >= 0.5
      if ((remainder << 1) >= divisor)
      {
        //the result is rounded up
        ++quotient;
      }
    }

    return quotient;
  }

  /******************************************************************************
  * unsignDiv with 64 bit dividend
  ******************************************************************************/
  uint32_t ATCMath::unsignDiv(const uint64_t dividend, const uint32_t divisor, const char_t* const file, const int32_t line) const
  {
    uint32_t quotient;
    uint32_t remainder;

    if (divisor == 0U)
    {
      // divide by zero error
      AbstractEventHandler::corePtr()->reportEvent(errorUnsignedDivisionByZero, file, line);
      quotient = 0U;
    }
    else
    {
      quotient = static_cast<uint32_t>(dividend / divisor);
      remainder = static_cast<uint32_t>(dividend % divisor);

      // if remainder * 2 is greater than or equal to the divisor it means that remainder is >= 0.5
      if ((remainder << 1) >= divisor)
      {
        //the result is rounded up
        ++quotient;
      }
    }

    return quotient;
  }

  /******************************************************************************
  * unsignDivRoundUp
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::unsignDivRoundUp) May be used in other projects
  uint32_t ATCMath::unsignDivRoundUp(const uint32_t dividend, const uint32_t divisor, const char_t* const file, const int32_t line) const
  {
    uint32_t quotient;
    uint32_t remainder;

    if (divisor == 0U)
    {
      // divide by zero error
      AbstractEventHandler::corePtr()->reportEvent(errorUnsignedDivisionByZero, file, line);
      quotient = 0U;
    }
    else
    {
      quotient = dividend / divisor;
      remainder = dividend % divisor;

      if (remainder != 0U)
      {
        //the result is rounded up
        ++quotient;
      }
    }

    return quotient;
  }

  /******************************************************************************
  * signed maximum
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::maximum) May be used in other projects
  int32_t ATCMath::maximum(const int32_t a, const int32_t b)
  {
    return (a < b) ? b : a;
  }

  /******************************************************************************
  * unsigned maximum
  ******************************************************************************/
  uint32_t ATCMath::maximum(const uint32_t a, const uint32_t b)
  {
    return (a < b) ? b : a;
  }

  /******************************************************************************
  * signed minimum
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::minimum) May be used in other projects
  int32_t ATCMath::minimum(const int32_t a, const int32_t b)
  {
    return (a < b) ? a : b;
  }

  /******************************************************************************
  * unsigned minimum
  ******************************************************************************/
  uint32_t ATCMath::minimum(const uint32_t a, const uint32_t b)
  {
    return (a < b) ? a : b;
  }

  /******************************************************************************
  * unsigned maximum
  ******************************************************************************/
  uint16_t ATCMath::maximum(const uint16_t a, const uint16_t b)
  {
    return (a < b) ? b : a;
  }

  /******************************************************************************
  * unsigned minimum
  ******************************************************************************/
  uint16_t ATCMath::minimum(const uint16_t a, const uint16_t b)
  {
    return (a < b) ? a : b;
  }

  /******************************************************************************
  * unsigned minimum
  ******************************************************************************/
  uint8_t ATCMath::minimum(const uint8_t a, const uint8_t b)
  {
    return (a < b) ? a : b;
  }

  /******************************************************************************
  * absolute
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::absolute) May be used in other projects
  int32_t ATCMath::absolute(const int32_t a, const char_t* const file, const int32_t line) const
  {
    int32_t result = 0;
    if (a == int32Min)
    {
      AbstractEventHandler::corePtr()->reportEvent(errorAbsOverflow, file, line);
      result = 0;
    }
    else
    {
      result = (a < 0) ? -a : a;
    }
    return result;
  }

  /******************************************************************************
  * absDiff
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::absDiff) May be used in other projects
  uint16_t ATCMath::absDiff(const uint16_t a, const uint16_t b)
  {
    uint16_t result;

    if (a > b)
    {
      result = a - b;
    }
    else
    {
      result = b - a;
    }

    return result;
  }

  /******************************************************************************
  * absDiff
  ******************************************************************************/
  uint32_t ATCMath::absDiff(const uint32_t a, const uint32_t b)
  {
    uint32_t result;

    if (a > b)
    {
      result = a - b;
    }
    else
    {
      result = b - a;
    }

    return result;
  }

  /******************************************************************************
  * convKmphToCmps
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::convKmphToCmps) May be used in other projects
  uint32_t ATCMath::convKmphToCmps(const uint32_t speed)
  {
    const uint32_t result = ((speed * 1000U) + 18U) / 36U;
    return result;
  }

  /******************************************************************************
  * convCmpsToKmph Convert cm/s to km/h (rounding)
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::convCmpsToKmph) May be used in other projects
  uint32_t ATCMath::convCmpsToKmph(const uint32_t speed)
  {
    const uint32_t result = ((speed * 36U) + 500U) / 1000U;
    return result;
  }

  /******************************************************************************
  * convCmpsTo100mph Convert cm/s to x0.1 km/h (rounding)
  ******************************************************************************/
  //lint -esym(1714,ATC::ATCMath::convCmpsTo100mph) May be used in other projects
  uint32_t ATCMath::convCmpsTo100mph(const uint32_t speed)
  {
    const uint32_t result = ((speed * 36U) + 50U) / 100U;
    return result;
  }
}
