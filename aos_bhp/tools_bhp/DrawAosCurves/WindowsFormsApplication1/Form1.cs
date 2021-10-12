using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

using int32_t = System.Int32;
using uint32_t = System.UInt32;
using int64_t = System.Int64;
using uint64_t = System.UInt64;


namespace DrawCurves
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            System.Windows.Forms.DataVisualization.Charting.DataPoint dataPoint;
            int distance = 0;
            uint32_t speed;

            int32_t gradient = Convert.ToInt32(this.tbGradient.Text);
            int32_t deceleration = Convert.ToInt32(this.tbBreakability.Text) + gradient;

            if(gradient > 0)
            {
                gradient = 0;
            }

            chart1.Series["EB"].Points.Clear();
            chart1.Series["SB"].Points.Clear();
            chart1.Series["SW"].Points.Clear();
            chart1.Series["FW"].Points.Clear();
            chart1.Series["PS"].Points.Clear();
            chart1.Series["PS-FW"].Points.Clear();

            int ebTP = 200;
            int sbTP = ebTP + 100;

            uint32_t targetSp = Convert.ToUInt32(this.tbTargetSpeed.Text);
            uint32_t ebSpeed = 0;
            uint32_t sbSpeed = 0;
            uint32_t swSpeed = 0;

            int ebOdo = ebTP;
            int sbOdo = sbTP;
            int swOdo = sbOdo;
            int fwOdo = swOdo;

            if (targetSp != 0)
            {
                ebSpeed = targetSp + calcEBSpeedMargin(targetSp);
                sbSpeed = targetSp + calcSBSpeedMargin(targetSp);
                swSpeed = targetSp + calcSWSpeedMargin(targetSp);

                uint32_t ebDelay = (getEBCurveDelay() + 5U) / 10U;
                uint32_t sbDelay = (getSBCurveDelay() + 5U) / 10U;
                uint32_t swDelay = (get2ndwarnCurveDelay() + 5U) / 10U;
                uint32_t fwDelay = (get1stwarnCurveDelay() + 5U) / 10U;

                ebOdo = ebTP + calcSupTargOdoOffset(ebSpeed, ebDelay, gradient, deceleration);
                sbOdo = sbTP + calcSupTargOdoOffset(sbSpeed, sbDelay, gradient, deceleration);
                swOdo = sbTP + calcSupTargOdoOffset(swSpeed, swDelay, gradient, deceleration);
                fwOdo = sbTP + calcSupTargOdoOffset(targetSp, fwDelay, gradient, deceleration);
            }

            Console.WriteLine("Speed \t EB \t SB \t SW \t FW");

            for (int sp = Convert.ToInt32(this.tbStartSpeed.Text); sp >= Convert.ToUInt32(this.tbTargetSpeed.Text); sp--)
            {
                distance = 0;
                Console.Write("{0} \t", sp.ToString("D"));

                if (sp>=ebSpeed)
                {
                    distance = calcEBCurveDistance((uint32_t)sp, gradient, deceleration,
                        Convert.ToUInt32(this.tbTargetSpeed.Text), Convert.ToUInt32(this.tbCeilingSpeed.Text));
                    distance += ebOdo;
                    dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(distance, sp);
                    chart1.Series["EB"].Points.Add(dataPoint);                    
                }
                Console.Write("{0} \t", distance.ToString("D"));

                if (sp>=sbSpeed)
                {
                    distance = calcSBCurveDistance((uint32_t)sp, gradient, deceleration,
        Convert.ToUInt32(this.tbTargetSpeed.Text), Convert.ToUInt32(this.tbCeilingSpeed.Text));
                    distance += sbOdo;
                    dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(distance, sp);
                    chart1.Series["SB"].Points.Add(dataPoint);
                }
                Console.Write("{0} \t", distance.ToString("D"));

                if (sp>=swSpeed)
                {
                    distance = calcSecondWarningCurveDistance((uint32_t)sp, gradient, deceleration,
                                 Convert.ToUInt32(this.tbTargetSpeed.Text), Convert.ToUInt32(this.tbCeilingSpeed.Text));
                    distance += swOdo;
                    dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(distance, sp);
                    chart1.Series["SW"].Points.Add(dataPoint);
                }
                Console.Write("{0} \t", distance.ToString("D"));

                distance = calcFirstWarningCurveDistance((uint32_t)sp, gradient, deceleration,
    Convert.ToUInt32(this.tbTargetSpeed.Text));
                distance += fwOdo;
                dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(distance, sp);
                chart1.Series["FW"].Points.Add(dataPoint);
                Console.Write("{0} \n", distance.ToString("D"));

                speed = calcFirstWarningCurveSpeed(0, distance - fwOdo, 0, gradient, deceleration, Convert.ToUInt32(this.tbTargetSpeed.Text));
                dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(distance, speed);
                chart1.Series["PS"].Points.Add(dataPoint);

                dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(distance, speed-sp);
        chart1.Series["PS-FW"].Points.Add(dataPoint);
      }
            dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(0, ebSpeed);
            chart1.Series["EB"].Points.Add(dataPoint);

            dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(0, sbSpeed);
            chart1.Series["SB"].Points.Add(dataPoint);

            dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(0, swSpeed);
            chart1.Series["SW"].Points.Add(dataPoint);

            dataPoint = new System.Windows.Forms.DataVisualization.Charting.DataPoint(0, targetSp);
            chart1.Series["FW"].Points.Add(dataPoint);
        }

/******************************************************************************
* calcCurveSpeed
******************************************************************************/
        uint32_t calcCurveSpeed(int32_t distance, uint32_t delay, int32_t gradient, int32_t deceleration, uint32_t targetSp)
        {


            /********** Formula to calculate speed **********
            ** v1 = delay * gradient
            ** v2 = (delay * deceleration) - v1
            ** d1 = (delay * delay) * gradient
            ** w =  (targetSpeed * targetSpeed) - (v1 * v1) + 2 deceleration(distance + d1)
            ** speed = sqrt((v2 * v2) + w) - v2
            */

            //distance = distance * 10; // cm => mm
            //modTargetSp = modTargetSp * 10U;  // cm/s => mm/s to increase accuracy
            //deceleration = deceleration * 10; //cm/s^2 => mm/s^2 to increase accuracy
            //gradient = gradient * 10; //cm / s^2 => mm / s^2 to increase accuracy*/
            delay = (delay + 5U) / 10U; // 0.1s => s
            int32_t delay_sqr = (int32_t)signMul(delay, delay);

            if (gradient > 0)
            {
                gradient = 0;
            }

            if (targetSp == 0)
            {
                int32_t dist31 = (int32_t)(signMul(delay, gradient));
                int32_t dist32 = (int32_t)(signMul(dist31, (int32_t)delay)) / 2;
                dist31 = (int32_t)(signMul(dist31, dist31)) / (2 * deceleration);
                distance = distance - dist31 + dist32;
            }

            uint32_t speed = 0U;

            if(distance <= 0)
            {
                speed = 0U;
            }
            else
            {
                uint32_t modTargetSp = (uint32_t)(targetSp - (int32_t)signMul(delay, gradient));
                int32_t v1 = (int32_t)signMul(delay, gradient);
                int32_t v2 = (int32_t)signMul(delay, (deceleration - gradient));
                int64_t w1 = signMul(modTargetSp, modTargetSp) - signMul(v1, v1) + signMul(v2, v2);
                int64_t square = w1 + signMul((2 * deceleration), (distance + (int32_t)signMul(delay, targetSp)));

                if (square < 0)
                {
                    // The square cannot be negative.
                    MessageBox.Show("!!!!15");
                    speed = 0U;
                }
                else
                {
                    speed = sqrtr((uint64_t)(square)) - (uint32_t)(v2);
                }
            }

            return speed;
        }

/******************************************************************************
* calcFirstWarningCurveSpeed
******************************************************************************/
        uint32_t calcFirstWarningCurveSpeed(int32_t startPointOdo, int32_t targetSbOdo, int32_t confidenceMargin, //TravelDir tDir,
      int32_t gradient, int32_t deceleration, uint32_t targetSpeed)
    {
        // calculate delay
        uint32_t delay = get1stwarnCurveDelay();

        // calculate distance
        int32_t distance = 0;
      //if (tDir == DirForward)
      //{
        distance = (targetSbOdo - startPointOdo);
      //}
      //else if (tDir == DirReverse)
      //{
      //  distance = (startPointOdo - targetSbOdo);
      //}
      //else
      //{
      //  distance = 0;
      //}

      distance = distance - confidenceMargin;

      // If the target point odometer (in this case, with SB margin and confidence margin) is passed return target speed
      uint32_t speed = 0U;
      if (distance <= 0)
      {
        speed = targetSpeed;
      }
      else
      {
        speed = calcCurveSpeed(distance, delay, gradient, deceleration, targetSpeed);
                speed = Math.Max(speed, targetSpeed);
      }
      return speed;
    }

        /******************************************************************************
        * calcEBCurveDistance
        ******************************************************************************/
        int32_t calcEBCurveDistance(uint32_t currSpeed, int32_t gradient, int32_t deceleration,
      uint32_t targetSpeed, uint32_t ceilingSpeed)
        {
            uint32_t delayEB = getEBCurveDelay();
            uint32_t modTargetSp;

            // calculate modTargetSp
            if (0U == targetSpeed)
            {
                modTargetSp = targetSpeed;
            }
            else
            {
                modTargetSp = targetSpeed + calcEBSpeedMargin(targetSpeed);
            }
        return calcCurveDistance(currSpeed, delayEB, gradient, deceleration, modTargetSp);
        }

        /******************************************************************************
        * calcSBCurveDistance
        ******************************************************************************/
        int32_t calcSBCurveDistance(uint32_t currSpeed, int32_t gradient, int32_t deceleration,
      uint32_t targetSpeed, uint32_t ceilingSpeed)
        {
            uint32_t delaySB = getSBCurveDelay();

            // calculate modTargetSp
            uint32_t modTargetSp = 0U;
            if (0U == targetSpeed)
            {
                modTargetSp = targetSpeed;
            }
            else
            {
                modTargetSp = targetSpeed + calcSBSpeedMargin(targetSpeed);
            }

        return calcCurveDistance(currSpeed, delaySB, gradient, deceleration, modTargetSp);
        }

        /******************************************************************************
        * calcSecondWarningCurveDistance
        ******************************************************************************/
        int32_t calcSecondWarningCurveDistance(uint32_t currSpeed, int32_t gradient, int32_t deceleration,
                                  uint32_t targetSpeed, uint32_t ceilingSpeed)
        {
            uint32_t delaySecondWarning = get2ndwarnCurveDelay();

            // calculate modTargetSp
            uint32_t modTargetSp = 0U;
            if (0U == targetSpeed)
            {
                modTargetSp = targetSpeed;
            }
            else
            {
                modTargetSp = targetSpeed + calcSWSpeedMargin(targetSpeed);
            }
            return calcCurveDistance(currSpeed, delaySecondWarning, gradient, deceleration, modTargetSp);
        }

        /******************************************************************************
        * calcFirstWarningCurveDistance
        ******************************************************************************/
        int32_t calcFirstWarningCurveDistance(uint32_t currSpeed, int32_t gradient, int32_t deceleration,
          uint32_t targetSpeed)
        {

            uint32_t delayFirstWarning = get1stwarnCurveDelay();
            return calcCurveDistance(currSpeed, delayFirstWarning, gradient, deceleration, targetSpeed);
        }


        uint32_t calcEBSpeedMargin(uint32_t ceilingSpeed)
        {
            return calcSpeedLimitMargin(ceilingSpeed,200,139,333);
        }

        uint32_t calcSBSpeedMargin(uint32_t ceilingSpeed)
        {
            return calcSpeedLimitMargin(ceilingSpeed,100,83,139);
        }

        uint32_t calcSWSpeedMargin(uint32_t ceilingSpeed)
        {
            return calcSpeedLimitMargin(ceilingSpeed,50,17,83);
        }

        uint32_t calcSpeedLimitMargin(uint32_t ceilingSpeed, uint32_t permil, uint32_t minValue, uint32_t maxValue)
        {
            return Math.Min(Math.Max((ceilingSpeed * permil) / 1000U, minValue), maxValue);
        }

        /******************************************************************************
        * calcCurveDistance
        ******************************************************************************/
        int32_t calcCurveDistance(uint32_t currSpeed, uint32_t delay, int32_t gradient, int32_t deceleration, uint32_t targetSp)
        {
            //Formula to calculate distance
            /* vax = currSpeed - (delay * gradient)
               distance = ((vax * vax) - (targetSpeed * targetSpeed))/(2 * deceleration) + (currSpeed * delay) - ((delay * delay) * gradient /2)
            */

            //      Vc^2 - Vm^2                G
            //d = ------------- +  Vc t - t^2 ---
            //          2r                     2
            // Vc = v - t G

            //currSpeed = currSpeed * 10;  // cm/s => mm/s to increase accuracy 
            //modTargetSp = modTargetSp * 10;  // cm/s => mm/s to increase accuracy 
            //deceleration = deceleration * 10; //cm/s^2 => mm/s^2 to increase accuracy
            //gradient = gradient * 10; //cm / s^2 => mm /s^2 to increase accuracy
            delay = (delay + 5) / 10U; // convert 0.1s to s

            uint32_t modTargetSp = 0;

            if (targetSp >= currSpeed) { return 0; }
            if (gradient > 0)
            {
                gradient = 0;
            }

            int32_t dist3 = 0;
            if (targetSp == 0)
            {
                int32_t dist31 = (int32_t)(signMul(delay, gradient));
                int32_t dist32 = (int32_t)(signMul(dist31, (int32_t)delay)) / 2;
                dist31 = (int32_t)(signMul(dist31, dist31)) / (2 * deceleration);
                dist3 = dist31 - dist32;
            }

            modTargetSp = (uint32_t)((int32_t)targetSp - (int32_t)(signMul(delay, gradient)));
            int32_t vax = (int32_t)currSpeed - (int32_t)(signMul(delay, gradient)); // cast added by babak (int32_t)currSpeed  // 
            int64_t v_sqr = signMul(vax, vax) - signMul(modTargetSp, modTargetSp);
            int32_t distance = signDiv(v_sqr, (2 * deceleration));

            int32_t distance2 = (int32_t)(signMul(currSpeed, delay)) - (int32_t)(signMul(targetSp, delay)); //multiplication of delay and current speed will not exceed 32 bit
            distance = distance + distance2 + dist3;
            //            distance = distance / 10;  //mm => cm 

            return distance;
        }
    /******************************************************************************
    * getEBCurveDelay
    ******************************************************************************/
        uint32_t getEBCurveDelay()
        {
            uint32_t ebDelay = Convert.ToUInt32(this.tbEBDelay.Text);
            ebDelay += getBrakeResponseTime();
            return ebDelay;
        }

        /******************************************************************************
        * getSBCurveDelay
        ******************************************************************************/
        uint32_t getSBCurveDelay()
        {
            uint32_t sbDelay = Convert.ToUInt32(this.tbSBDelay.Text);
            sbDelay += (getEBCurveDelay() + getBrakeResponseTime());
            return sbDelay;
        }

        /******************************************************************************
        * get2ndwarnCurveDelay
        ******************************************************************************/
        uint32_t get2ndwarnCurveDelay()
        {
            uint32_t delay2ndWarn = Convert.ToUInt32(this.tbSWtoSBdelay.Text);
            delay2ndWarn += getSBCurveDelay();

            return delay2ndWarn;
        }

        /******************************************************************************
        * get1stwarnCurveDelay
        ******************************************************************************/
        uint32_t get1stwarnCurveDelay()
        {
            uint32_t delay1stWarn = Convert.ToUInt32(this.tbFWtoSwDealy.Text);
            delay1stWarn += get2ndwarnCurveDelay();

            return delay1stWarn;
        }

        /******************************************************************************
        * getBrakeResponseTime
        ******************************************************************************/
        uint32_t getBrakeResponseTime()
        {
            return ((Convert.ToUInt32(this.tbBRST.Text) + 50U) / 100U);
        }


        /******************************************************************************
        * signMul with signed inputs
        ******************************************************************************/
        int64_t signMul(int32_t a, int32_t b)
        {
            int64_t p = 0;
            int32_t q = 0;


            p = (int64_t)(a) * (int64_t)(b);

            if (a != 0)
            {
                q = (int32_t)(p / a);
                if (q != b)
                {
                    //Log event or trace!
                    MessageBox.Show("!!!!");
                }
            }
            return p;
        }

        /******************************************************************************
* signMul with unsigned and signed inputs
******************************************************************************/
        int64_t signMul(uint32_t a, int32_t b)
        {
            int32_t a_int = (int32_t)(a);

            int64_t result = 0;

            if (a_int < 0)
            {
                result = 0;
                MessageBox.Show("!!!!2");
            }
            else
            {
                result = signMul(a_int, b);
            }

            return result;
        }

        /******************************************************************************
* signMul with unsigned inputs
******************************************************************************/
        int64_t signMul(uint32_t a, uint32_t b)
        {
            int32_t a_int = (int32_t)(a);
            int32_t b_int = (int32_t)(b);
            int64_t result = 0;

            if ((a_int < 0) || (b_int < 0))   //   or it can for instance be: if a is greater than INT32_MAX 
            {
                if (a_int < 0)
                {
                    MessageBox.Show("!!!!3");
                }
                if (b_int < 0)
                {
                    MessageBox.Show("!!!!4");
                }
                result = 0;
            }
            else
            {
                result = signMul(a_int, b_int);
            }

            return result;
        }

        /******************************************************************************
* signDiv with 64 bit signed inputs
******************************************************************************/
        int32_t signDiv(int64_t dividend, int32_t divisor)
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
                    MessageBox.Show("!!!!8");
                    result = 0;
                }
                if (dividend < 0)
                {
                    MessageBox.Show("!!!!9");
                    result = 0;
                }
            }
            else
            {
                result = (int32_t)(unsignDiv((uint64_t)(dividend), (uint32_t)(divisor)));
            }
            if (true == negative)
            {
                result = -result;
            }
            return result;
        }

        /******************************************************************************
* unsignDiv with 64 bit signed inputs
******************************************************************************/
        uint32_t unsignDiv(uint64_t dividend, uint32_t divisor)
        {
            uint32_t quotient;
            uint32_t reminder;

            if (divisor == 0U)
            {
                // divide by zero error
                MessageBox.Show("!!!!10");
                quotient = 0U;
            }
            else
            {
                quotient = (uint32_t)(dividend / divisor);
                reminder = (uint32_t)(dividend % divisor);   // Cast added by babak
                if (reminder != 0U)
                {
                    // round up
                    // if reminder * 2 is greater than or equal to the deviser it means that remainder is >= 0.5 
                    if ((reminder << 1) >= divisor)
                    {
                        //the result is rounded up
                        ++quotient;
                    }
                }
            }
            return (quotient);
        }

        /******************************************************************************
 * signDiv with signed inputs
 ******************************************************************************/
        int32_t signDiv(int32_t dividend, int32_t divisor)
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
                    MessageBox.Show("!!!!5");
                    result = 0;
                }
                if (dividend < 0)
                {
                    MessageBox.Show("!!!!6");
                    result = 0;
                }
            }
            else
            {
                result = (int32_t)(unsignDiv((uint32_t)(dividend), (uint32_t)(divisor)));
            }
            if (true == negative)
            {
                result = -result;
            }
            return result;
        }

        /******************************************************************************
* unsignDiv with signed inputs
******************************************************************************/
        uint32_t unsignDiv(uint32_t dividend, uint32_t divisor)
        {
            uint32_t quotient;
            uint32_t reminder;

            if (divisor == 0U)
            {
                // divide by zero error
                MessageBox.Show("!!!!7");
                quotient = 0U;
            }
            else
            {
                quotient = (dividend / divisor);
                reminder = (dividend % divisor);
                if (reminder != 0U)
                {
                    // round up
                    // if reminder * 2 is greater than or equal to the deviser it means that remainder is >= 0.5 
                    if ((reminder << 1) >= divisor)
                    {
                        //the result is rounded up
                        ++quotient;
                    }
                }
            }
            return (quotient);
        }


        /******************************************************************************
* sqrtr square root rounded for 64 bit
******************************************************************************/
        uint32_t sqrtr(uint64_t op)
        {
            uint64_t res = 0U;
            uint64_t one = 1;
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

            return (uint32_t)(res);
        }

        /******************************************************************************
* calcSupTargOdoOffset
******************************************************************************/
        int32_t calcSupTargOdoOffset(uint32_t speedLimit, uint32_t delay, int32_t gradient, int32_t deceleration)
        {
      uint32_t v1 = (uint32_t)(speedLimit -signMul(gradient, (int)delay));

        int32_t delaySpeedDist = (int32_t)(signMul(speedLimit, delay));

        int32_t delayAccDeaccDist = (int32_t)(signDiv(
                    (int32_t)(signMul(v1, v1) - signMul(speedLimit, speedLimit)), 2 * deceleration));

        int32_t delayGradDist = (int32_t)(signMul((delay * delay), gradient) / 2);

      return (delaySpeedDist + delayAccDeaccDist - delayGradDist);
    }
}

 
}
