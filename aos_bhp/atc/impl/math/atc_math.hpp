#ifndef ATCMath_hpp
#define ATCMath_hpp
/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2016
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*
* DESCRIPTION:
* This is the header file for safe integer math functions
*
*****************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2016-09-22    bhidaji     Created
* 2016-09-26    bhidaji     removed commented code
* 2016-09-28    bhidaji     Added km/h conversion to cm/s and vise-versa
* 2017-01-11    nsyed       Refactored into class ATCMath
*
*******************************************************************************/

/******************************************************************************
* INCLUDE FILES
******************************************************************************/
#include "atc_types.hpp"
#include "abstract_event_handler.hpp"

/******************************************************************************
* DECLARATIONS
******************************************************************************/

namespace ATC
{

  /**
  * Constants for distances, time etc.
  * Instead of using the standard defines directly.
  */
  static const int32_t maxDistance = int32Max;
  static const uint32_t maxTimeValue = uint32Max;

  /**
  * The class ATCMath implements the math functions used by the ATP components
  */
  class ATCMath
  {
  public:

    /**
    * Singleton instance.
    * Only one instance of this class is allowed.
    *
    * @return the one and only one instance.
    */
    static ATCMath& instance(void);

    /** Calculate the square root rounded
    *
    * Implements the sqrtr() function.
    * The function calculates a fast square root algorithm, with rounding
    * The rounding of the result is arithmetic. That is, if the real answer
    * would have a fractional part of 0.5 or greater, the result is rounded up to
    * the next integer.
    *      - sqrtr(2) --> 1
    *      - sqrtr(3) --> 2
    *      - sqrtr(4) --> 2
    *      - sqrtr(6) --> 2
    *      - sqrtr(7) --> 3
    *      - sqrtr(8) --> 3
    *      - sqrtr(9) --> 3
    *
    * @param[in]  op               is the unsigned input value for the square root calculation
    * @return     unsigned square root rounded
    */
    static uint32_t sqrtr(uint32_t op);

    /** Calculate the square root rounded for 64 bit Number
    *
    * Implements the sqrtr() function.
    * The function calculates a fast square root algorithm, with rounding
    * The rounding of the result is arithmetic. That is, if the real answer
    * would have a fractional part of 0.5 or greater, the result is rounded up to
    * the next integer.
    *      - sqrtr(2) --> 1
    *      - sqrtr(3) --> 2
    *      - sqrtr(4) --> 2
    *      - sqrtr(6) --> 2
    *      - sqrtr(7) --> 3
    *      - sqrtr(8) --> 3
    *      - sqrtr(9) --> 3
    *
    * @param[in]  op               is the unsigned input value for the square root calculation in 64 bit
    * @return     unsigned square root rounded
    */
    static uint32_t sqrtr(uint64_t op);

    /**
    * Signed multiplication
    *
    * The function takes two operands a and b and multiplies them with the signed result
    * The function also checks for overflow
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @param[in] file   Pointer to the filename from where the event is reported.
    * @param[in] line   Line number from where the event is reported.
    * @return           result of multiplication
    *
    */
    int64_t signMul(const int32_t a, const int32_t b, const char_t* const file, const int32_t line) const;

    /**
    * Signed multiplication
    *
    * The function takes two operands a and b and multiplies them with the signed result
    * The function also checks for overflow
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @param[in] file   Pointer to the filename from where the event is reported.
    * @param[in] line   Line number from where the event is reported.
    * @return           result of multiplication
    *
    */
    int64_t signMul(const uint32_t a, const uint32_t b, const char_t* const file, const int32_t line) const;

    /**
    * Signed multiplication
    *
    * The function takes two operands a and b and multiplies them with the signed result
    * The function also checks for overflow
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @param[in] file   Pointer to the filename from where the event is reported.
    * @param[in] line   Line number from where the event is reported.
    * @return           result of multiplication
    *
    */
    int64_t signMul(const int32_t a, const uint32_t b, const char_t* const file, const int32_t line) const;

    /**
    * Signed multiplication
    *
    * The function takes two operands a and b and multiplies them with the signed result
    * The function also checks for overflow
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @param[in] file   Pointer to the filename from where the event is reported.
    * @param[in] line   Line number from where the event is reported.
    * @return           result of multiplication
    *
    */
    int64_t signMul(const uint32_t a, const int32_t b, const char_t* const file, const int32_t line) const;

    /**
    * unsigned division
    *
    * The function takes two unsigned operands dividend and divisor and divides dividend by divisor with an unsigned result
    * The function also checks for overflow
    *
    * @param[in] dividend    the dividend
    * @param[in] divisor     the divisor
    * @param[in] file        Pointer to the filename from where the event is reported.
    * @param[in] line        Line number from where the event is reported.
    *
    * @return result of division
    */
    uint32_t unsignDiv(const uint32_t dividend, const uint32_t divisor, const char_t* const file, const int32_t line) const;

    /**
    * unsigned division for 64 bit dividend
    *
    * The function takes two unsigned operands dividend and divisor and divides dividend by divisor with an unsigned result
    * The function also checks for overflow
    *
    * @param[in] dividend    the dividend
    * @param[in] divisor     the divisor
    * @param[in] file        Pointer to the filename from where the event is reported.
    * @param[in] line        Line number from where the event is reported.
    *
    * @return result of division
    */
    uint32_t unsignDiv(const uint64_t dividend, const uint32_t divisor, const char_t* const file, const int32_t line) const;

    /**
    * Performs integer division by rounding upwards.
    *
    * @param[in] dividend    the dividend
    * @param[in] divisor     the divisor
    * @param[in] file        Pointer to the filename from where the event is reported.
    * @param[in] line        Line number from where the event is reported.
    *
    * @return result of division, rounded upwards
    */
    uint32_t unsignDivRoundUp(const uint32_t dividend, const uint32_t divisor, const char_t* const file, const int32_t line) const;

    /**
    * signed division
    *
    * The function takes two signed operands dividend and divisor and divides dividend by divisor with a signed result
    * The function also checks for overflow
    *
    * @param[in] dividend    the dividend
    * @param[in] divisor     the divisor
    * @param[in] file        Pointer to the filename from where the event is reported.
    * @param[in] line        Line number from where the event is reported.
    * @return                result of division
    *
    */
    int32_t signDiv(const int32_t dividend, const int32_t divisor, const char_t* const file, const int32_t line) const;

    /**
    * signed division for 64 bit dividend
    *
    * The function takes two signed operands dividend and divisor and divides dividend by divisor with a signed result
    * The function also checks for overflow
    *
    * @param[in] dividend    the dividend 64 bit
    * @param[in] divisor     the divisor
    * @param[in] file        Pointer to the filename from where the event is reported.
    * @param[in] line        Line number from where the event is reported.
    * @return                result of division
    *
    */
    int32_t signDiv(const int64_t dividend, const int32_t divisor, const char_t* const file, const int32_t line) const;

    /**
    * signed maximum
    *
    * The function takes two signed operands a and b and returns the signed maximum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           maximum value
    *
    */
    static int32_t maximum(const int32_t a, const int32_t b);

    /**
    * unsigned maximum
    *
    * The function takes two unsigned operands a and b and returns the unsigned maximum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           maximum value
    *
    */
    static uint32_t maximum(const uint32_t a, const uint32_t b);

    /**
    * signed minimum
    *
    * The function takes two signed operands a and b and returns the signed minimum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           minimum value
    *
    */
    static int32_t minimum(const int32_t a, const int32_t b);

    /**
    * unsigned minimum
    *
    * The function takes two unsigned operands a and b and returns the unsigned minimum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           minimum value
    *
    */
    static uint32_t minimum(const uint32_t a, const uint32_t b);

    /**
    * unsigned maximum
    *
    * The function takes two unsigned operands a and b and returns the unsigned maximum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           maximum value
    *
    */
    static uint16_t maximum(const uint16_t a, const uint16_t b);

    /**
    * unsigned minimum
    *
    * The function takes two unsigned operands a and b and returns the unsigned minimum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           minimum value
    *
    */
    static uint16_t minimum(const uint16_t a, const uint16_t b);

    /**
    * unsigned minimum
    *
    * The function takes two unsigned operands a and b and returns the unsigned minimum
    *
    * @param[in] a      first operand
    * @param[in] b      second operand
    * @return           minimum value
    *
    */
    static uint8_t minimum(const uint8_t a, const uint8_t b);

    /**
    * signed absolute
    *
    * The function takes a signed operand a and returns it's signed absolute value
    * The function also checks for overflow
    *
    * @param[in] a      operand
    * @param[in] file   Pointer to the filename from where the event is reported.
    * @param[in] line   Line number from where the event is reported.
    * @return           absolute value of the operand
    *
    */
    int32_t absolute(const int32_t a, const char_t* const file, const int32_t line) const;

    /**
    * Calculates the absolute value of the difference between two unsigned 16-bit values.
    *
    * @param[in] a  the first value
    * @param[in] b  the second value
    *
    * @return The absolute value of the difference between a and b
    */
    static uint16_t absDiff(const uint16_t a, const uint16_t b);

    /**
    * Calculates the absolute value of the difference between two unsigned 32-bit values.
    *
    * @param[in] a  the first value
    * @param[in] b  the second value
    *
    * @return The absolute value of the difference between a and b
    */
    static uint32_t absDiff(const uint32_t a, const uint32_t b);

    /**
    * convert km/h to cm/s with rounding
    *
    * The function converts speed unit from km/h to cm/s
    *
    * @param[in] speed      speed in km/h
    * @return               speed in cm/s
    *
    */
    static uint32_t convKmphToCmps(const uint32_t speed);

    /**
    * convert cm/s to km/h with rounding
    *
    * The function converts speed unit from cm/s to km/h 
    *
    * @param[in] speed      speed in cm/s
    * @return               speed in km/h
    *
    */
    static uint32_t convCmpsToKmph(const uint32_t speed);
    
    /**
    * convert cm/s to 0.1 km/h with rounding
    *
    * The function converts speed unit from cm/s to 0.1 km/h
    *
    * @param[in] speed      speed in cm/s
    * @return               speed in 0.1 km/h
    *
    */
    static uint32_t convCmpsTo100mph(const uint32_t speed);

  protected:

    /**
    * Constructor for ATCMath
    */
    ATCMath();

  private:

    /**
    * Events to report errors occurring during math calculations
    */
    const Event errorSignMul1;
    const Event errorSignMulOpACast;
    const Event errorSignMulOpBCast;
    const Event errorUnsignedDivisionByZero;
    const Event errorSignDivDivisorOverflow;
    const Event errorSignDivDividendOverflow;
    const Event errorAbsOverflow;

    /**
    * Declare copy-constructor as private in order to prevent illegal use.
    */
    ATCMath(const ATCMath&);
  };
}

#endif
