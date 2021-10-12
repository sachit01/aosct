// sine_lookup_protoype.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#define _USE_MATH_DEFINES
#include <math.h>
#include <conio.h>

const __int64 FirstQuad   = 1570796327;
const __int64 SecQuad     = 3141592654;
const __int64 ThirdQuad   = 4712388980;
const __int64 FouthQuad   = 6283185307;

const int EarthRadie= 6371000;

const __int64 Factor =  1000000000;

const int TableSize = 1001;

typedef struct {
  __int64 radians;
  __int64 sinValue;
} sinValType;

sinValType sineTable[TableSize];


// Function to pre-generate a sine-table for first quadrant. 
void genSinTable()
{
  for (int i = 0; i < TableSize; i++)
  {
    double radians = (M_PI / 2 / (TableSize-1)) * i;
    double sinVal = sin(radians);

    sineTable[i].radians = radians * Factor;
    sineTable[i].sinValue = sinVal * Factor;
  }
}

__int64 getInterpolatedSine(__int64 rad)
{
  int i = 0;
  __int64 retVal = 0;

  while (sineTable[i].radians < rad && (i < TableSize))
  {
    ++i;
  }
    
  if (sineTable[i].radians == rad)
  {
    retVal = sineTable[i].sinValue;
  }
  else
  {
    __int64 tmp = ((sineTable[i].sinValue - sineTable[i - 1].sinValue) * Factor);
    tmp /= (sineTable[i].radians - sineTable[i - 1].radians);

    tmp *= (rad - sineTable[i - 1].radians);
    tmp /= Factor;

    retVal = tmp + sineTable[i - 1].sinValue;
  }

  return(retVal);
}

// Public function to lookup Sine
__int64 lookupSine(__int64 rad)
{
  __int64 retVal;

  if (rad <= FirstQuad)
  {
    retVal = getInterpolatedSine(rad);
  }
  else if (rad <= SecQuad)
  {
    retVal = getInterpolatedSine(SecQuad - rad);
  }
  else if (rad <= ThirdQuad)
  {
    retVal = -getInterpolatedSine(rad - SecQuad);
  }
  else
  {
    retVal = -getInterpolatedSine(FouthQuad - rad);
  }

  return retVal;
}

// Public function to lookup Cosine
__int64 lookupCosine(__int64 rad)
{
  if (rad > FouthQuad)
  {
    rad -= FouthQuad;
  }

  __int64 retVal = lookupSine(rad + FirstQuad);

  return retVal;
}

__int64 getInterpolatedArcSine(__int64 sinValue)
{
  int i = 0;
  __int64 retVal = 0;

  while (sineTable[i].sinValue < sinValue && (i < TableSize))
  {
    ++i;
  }

  if (sineTable[i].sinValue == sinValue)
  {
    retVal = sineTable[i].radians;
  }
  else
  {
    __int64 tmp = ((sineTable[i].radians - sineTable[i - 1].radians)  * Factor) / (sineTable[i].sinValue - sineTable[i - 1].sinValue);
    tmp *= (sinValue - sineTable[i - 1].sinValue);
    tmp /= Factor;

    retVal = tmp + sineTable[i - 1].radians;
  }

  return(retVal);
}

// Public function to lookup ArcSine
__int64 lookupArcSine(__int64 sineVal)
{
  __int64 retVal;

  if (sineVal > 0)
  {
    retVal = getInterpolatedArcSine(sineVal);
  }
  else
  {
    retVal = -getInterpolatedArcSine(sineVal);
  }

  return retVal;
}


// Public function to lookup ArcCosine
__int64 lookupArcCosine(__int64 cosVal)
{
  __int64 retVal = 0;
  
  if (cosVal > 0)
  {
    retVal = FirstQuad - lookupArcSine(cosVal);
  }
  else
  {
    retVal = FirstQuad + lookupArcSine(cosVal);
  }

  return retVal;
}

typedef struct
{
  __int64 latitude;
  __int64 longitude;

}gpsPos;

// Public function to get distance between 2 GPS Points
int getDistance(gpsPos gpsPos1, gpsPos gpsPos2)
{
  int retVal;

  __int64 term1 = (lookupSine(gpsPos1.latitude) * lookupSine(gpsPos2.latitude)) / Factor;
  __int64 temp = (lookupCosine(gpsPos1.latitude) * lookupCosine(gpsPos2.latitude))/ Factor;
  __int64 term2 = (temp * lookupCosine(gpsPos1.longitude - gpsPos2.longitude)) / Factor;

  __int64 factor1 = lookupArcCosine(term1 + term2);

  retVal = (EarthRadie * factor1) / Factor;

  return retVal;
}

int main()
{
  genSinTable();

  for (int i = 0; i < TableSize; i++)
  {
    printf("%lld %lld", sineTable[i].radians, sineTable[i].sinValue);
    printf("    Lookuptable: %lld and in between %lld\n", lookupSine(sineTable[i].radians), (lookupSine(sineTable[i].radians) + lookupSine(sineTable[i+1].radians)) / 2);
  }
  
  gpsPos gpsPos1;
  gpsPos gpsPos2;
  
  // Test 1376m
  gpsPos1.latitude = 0;
  gpsPos1.longitude = 0;
  gpsPos2.latitude = 0;
  gpsPos2.longitude = 215926; // Multiplied with Factor
  
  printf("Distance between pos 1 and 2 : %d meter", getDistance(gpsPos1, gpsPos2));

  _getch();

  return 0;
}