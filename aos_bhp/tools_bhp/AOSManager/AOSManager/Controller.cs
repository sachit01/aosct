/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation Sweden AB, 2018
*
* We reserve all rights in this file and in the information
* contained therein. Reproduction, use or disclosure to third
* parties without written authority is strictly forbidden.
*
*  DESCRIPTION:
*
*  later.
*
******************************************************************************/


/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2018-10-17    akushwah    Created
* 2018-11-04    akushwah    Added the log functionality 
* 2019-01-21    akushwah    updated after integration test with TE 
* 2019-03-19    akushwah    updated for Re-registration processing 
* 2019-06-13    akushwah    Added the minimize and restore functionality 
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Ini;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Net;
using System.Net.Sockets;
using System.ComponentModel;
using System.Diagnostics;
using System.Timers;
using System.Threading;

namespace AOSManager
{
   public delegate void SetStatus(int trainNumber, bool status);

   struct AOSPCStatus
   {
      public TcpClient aospcClient;
      public NetworkStream nwStream;
      public bool istrainRunning;
   };

   enum LogSource
   {
      T = 0,  // Test Environment
      A = 1,   // AOSPC
      I = 2   // Internal Log of AOSManager
   };

   class Controller
   {
      const int bufferSize = 256;
      const int waitTimeBeforeClosingAOSPC = 500;
      const int waitTimeAfterClosingAOSPC = 1000;
      const int timerInterval = 100; //msec
      const int offsetValue = 0;
      const int whiteSpacesCount = 2;
      const int indexValue = 0;

      static System.Timers.Timer aTimer;
      IniFile ini;
      public int trainCount;
      int testEnvPort;
      int remotePortForAOSPC;
      String aosRootPath = null;
      String logPath = null;
      bool isLogEnabled = false;
      TcpListener server = null;
      TcpClient tEClient = null;
      bool initDone = false;
      NetworkStream tEstream = null;
      public event SetStatus SetAppStatus = null;


      // Buffer for reading data
      Byte[] incomingTEbytes = new Byte[bufferSize];
      String IncomingTEdata = null;
      Byte[] OutgoingTEbytes = new Byte[bufferSize];
      String OutgoingTEdata = null;
      String tECommandFirstData = null;
      int trainNumberFetched;
      String CommandToSendAOSPC = null;
      AOSPCStatus[] aospcStatus;
      int bytesReadFromAOSPC;
      bool isStopReceived;

      /******************************************************************************
      * Function:     initialisation
      ******************************************************************************/
      public void initialisation()
      {
         string exeDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
         Directory.SetCurrentDirectory(exeDir);

         string aosManagerIniPath = exeDir + "\\AOSManager.ini";

         ini = new IniFile(@aosManagerIniPath);
         trainCount = Convert.ToInt32(ini.IniReadValue("General", "TrainCount"));
         aospcStatus = new AOSPCStatus[trainCount];

         aosRootPath = ini.IniReadValue("General", "Path");
         testEnvPort = Convert.ToInt32(ini.IniReadValue("TEPort", "TEPort"));
         isLogEnabled = Convert.ToBoolean(ini.IniReadValue("Log", "Enabled"));
         logPath = ini.IniReadValue("Log", "Path");

         // Set the TcpListener for TE
         server = new TcpListener(testEnvPort);

         // Start listening for client requests.
         server.Start();

         isStopReceived = false;
      }

      /******************************************************************************
      * Function:  Check the folder structure is OK or not
      ******************************************************************************/
      public bool checkFolderStructure()
      {
         bool retValue = true;

         var aospcExeExists = Directory.EnumerateFiles(aosRootPath, "AOSPC.exe", SearchOption.AllDirectories).FirstOrDefault();
         var atpExeExists = Directory.EnumerateFiles(aosRootPath, "test_atp.exe", SearchOption.AllDirectories).FirstOrDefault();

         if (aospcExeExists != null && atpExeExists != null)
         {
            for (int trainId = 1; trainId <= trainCount; trainId++)
            {
               var directoryToFind = "Train" + trainId;

               var result = Directory
                   .EnumerateDirectories(aosRootPath, directoryToFind, SearchOption.AllDirectories)
                   .FirstOrDefault();

               if (result == null)
               {
                  retValue = false;
               }
            }
         }
         else
         {
            retValue = false;
         }
         return retValue;
      }

      /******************************************************************************
      * Function:  read the Commands from Test Environment
      ******************************************************************************/
      public void readCommandFromTE()
      {
         if (tEstream == null)
         {
            // Accept requests.
            tEClient = server.AcceptTcpClient();
         }

         try
         {
            // Get a stream object for reading and writing
            tEstream = tEClient.GetStream();

            int noOfBytesRead = 0;

            if (tEstream.DataAvailable && tEstream.CanRead)
            {
               noOfBytesRead = tEstream.Read(incomingTEbytes, offsetValue, incomingTEbytes.Length);
               //check whether any data is available 
               if (noOfBytesRead != 0)
               {
                  // Translate data bytes to a ASCII string.
                  IncomingTEdata = System.Text.Encoding.ASCII.GetString(incomingTEbytes, indexValue, noOfBytesRead).ToLower().TrimEnd();

                  //Write the Log 
                  writeLog(LogSource.T, IncomingTEdata);

                  //Get the First word from the Command send from TE
                  tECommandFirstData = IncomingTEdata.Split(' ').Skip(0).FirstOrDefault();
                  //Get the subsystem Name from the command
                  string subSystemName = IncomingTEdata.Split(' ').Skip(1).FirstOrDefault();

                  if (subSystemName != "aos")
                  {
                     writeLog(LogSource.I, "Wrong name of subSystem! Expected aos");
                     return;
                  }

                  //Get the Train number from the command
                  trainNumberFetched = Convert.ToInt32(IncomingTEdata.Split(' ').Skip(2).FirstOrDefault());

                  if (IncomingTEdata.Count(Char.IsWhiteSpace) == whiteSpacesCount)
                  {
                     //Command to send to AOSPC
                     CommandToSendAOSPC = tECommandFirstData;
                  }
                  else
                  {
                     int count = 0;
                     string remainingData = null;
                     foreach (var str in IncomingTEdata)
                     {
                        String tempStr = IncomingTEdata.Split(' ').Skip(count).FirstOrDefault();

                        if ((tempStr != null))
                        {
                           //count = 1, means tempStr has subsystem name i.e AOS in IncomingTEdata
                           //count = 2, means tempStr has train number in IncomingTEdata
                           if ((count == 1) || (count == 2))
                           {
                              //Skip the substring while adding them in RemainingData
                           }
                           else
                           {
                              remainingData = remainingData + " " + tempStr;
                           }
                        }
                        else
                        {
                           break;
                        }
                        //Increment the counter to get next substring from command
                        count++;
                     }

                     CommandToSendAOSPC = remainingData.Trim();
                  }

                  if ((trainNumberFetched != 0) && (trainNumberFetched <= trainCount))
                  {
                     //Add the no of train to GUI display
                     if ((tECommandFirstData == "start") || (tECommandFirstData == "starttrain"))
                     {
                        // Check whether train is already running or not
                        if (aospcStatus[trainNumberFetched - 1].istrainRunning)
                        {
                           writeLog(LogSource.I, "Train number: " + trainNumberFetched + "is already running");
                           
                           //Stop the train if already running
                           aospcStatus[trainNumberFetched - 1].nwStream = aospcStatus[trainNumberFetched - 1].aospcClient.GetStream();
                           string stopCommand = "stop";
                           byte[] bytesToSend = ASCIIEncoding.ASCII.GetBytes(stopCommand);
                           aospcStatus[trainNumberFetched - 1].nwStream.Write(bytesToSend, offsetValue, bytesToSend.Length);
                           Thread.Sleep(waitTimeAfterClosingAOSPC);
                        }

                        startTrain();

                     }
                     else if (CommandToSendAOSPC == "stop")
                     {
                        if (SetAppStatus != null)
                           SetAppStatus(trainNumberFetched, false);

                        isStopReceived = true;
                     }
                  }
                  else
                  {
                     writeLog(LogSource.I, "Train number out of expected range, Train Number: " + trainNumberFetched);
                  }
                  //write Data to AOSPC Data Processing
                  writeCommandToAOSPC(trainNumberFetched - 1);
               }
            }
         }
         catch (Exception)
         {
         }
      }

      /******************************************************************************
      * Function:  write the Commands to AOSPC
      ******************************************************************************/
      public void writeCommandToAOSPC(int trainNoIndex)
      {
         try
         {
            if (aospcStatus[trainNoIndex].istrainRunning)
            {
               aospcStatus[trainNoIndex].nwStream = aospcStatus[trainNoIndex].aospcClient.GetStream();

               byte[] bytesToSend = ASCIIEncoding.ASCII.GetBytes(CommandToSendAOSPC);

               //write Data to AOSPC 
               aospcStatus[trainNoIndex].nwStream.Write(bytesToSend, offsetValue, bytesToSend.Length);
            }
         }
         catch (Exception)
         {
            //AOS PC not connected
         }
      }

      /******************************************************************************
      * Function:  read the response from the AOSPC
      ******************************************************************************/
      public void readResponsefromAOSPC()
      {
         try
         {
            for (int trainIndex = 0; trainIndex < trainCount; trainIndex++)
            {
               if (aospcStatus[trainIndex].istrainRunning)
               {
                  aospcStatus[trainIndex].nwStream = aospcStatus[trainIndex].aospcClient.GetStream();
                  //Read Data from AOSPC
                  if (aospcStatus[trainIndex].nwStream.CanRead)
                  {
                     if (aospcStatus[trainIndex].nwStream.DataAvailable)
                     {
                        bytesReadFromAOSPC = aospcStatus[trainIndex].nwStream.Read(OutgoingTEbytes, offsetValue, OutgoingTEbytes.Length);
                        //translate the value and add '\n' to the output result so that Test Environment can receive it.
                        OutgoingTEdata = System.Text.Encoding.ASCII.GetString(OutgoingTEbytes, indexValue, bytesReadFromAOSPC).ToLower().TrimEnd() + '\n';

                        //Write data to Test Environment
                        byte[] msgBytesToSendTE = System.Text.Encoding.ASCII.GetBytes(OutgoingTEdata);
                        tEstream.Write(msgBytesToSendTE, offsetValue, msgBytesToSendTE.Length);

                        //Write the Log 
                        writeLog(LogSource.A, OutgoingTEdata);
                     }
                  }
               }
            }
         }
         catch (Exception)
         {
            //AOS PC not connected
         }
      }


      /******************************************************************************
      * Function:  run 
      ******************************************************************************/
      private void run(object source, ElapsedEventArgs e)
      {
         if (initDone)
         {
            //Read the Test Environment Data Processing
            readCommandFromTE();

            //Read response from AOSPC
            readResponsefromAOSPC();

            //reset the values
            if (isStopReceived)
            {
               //reset the Stream
               try
               {
                  //Wait for some cycle before closing the aospcClient
                  Thread.Sleep(waitTimeBeforeClosingAOSPC);
                  aospcStatus[trainNumberFetched - 1].aospcClient.Close();
                  aospcStatus[trainNumberFetched - 1].istrainRunning = false;
                  aospcStatus[trainNumberFetched - 1].nwStream = null;
                  isStopReceived = false;
               }
               catch (Exception)
               {
                  isStopReceived = false;
                  writeLog(LogSource.I, "Train " + trainNumberFetched + " was not started, Please start it");
               }
            }
         }
      }

      /******************************************************************************
      * Constructor 
      ******************************************************************************/
      public Controller()
      {
         //Read the ini file and its processing
         initialisation();

         //Check the folder structure is OK or not
         if (checkFolderStructure())
         {
            initDone = true;
         }
         else
         {
            writeLog(LogSource.I, "Folder Structure Not Correct");
            Application.Exit();
         }

         // Create a timer.
         aTimer = new System.Timers.Timer(timerInterval);
         // Hook up the Elapsed event for the timer.
         aTimer.Elapsed += new ElapsedEventHandler(run);
         // Set the Interval
         aTimer.Interval = timerInterval;
         aTimer.Enabled = true;
      }


      /******************************************************************************
      * Function:     writeLog
      ******************************************************************************/
      public void writeLog(LogSource src, String valueToWrite)
      {
         if (isLogEnabled)
         {
            try
            {
               string logFileName = logPath + "\\" + "AOS_" + DateTime.Now.ToString("yyyyMMdd") + ".log";
               using (StreamWriter strWriter = File.AppendText(logFileName))
               {
                  strWriter.WriteLine("{0} {1} {2}", DateTime.Now.ToString("HH:mm:ss"), src, valueToWrite);
               }
            }
            catch (Exception)
            {
            }
         }
      }


      /******************************************************************************
        * Function:     startTrain
        ******************************************************************************/
      public void startTrain()
      {
         var iniFile = aosRootPath + "\\Train" + trainNumberFetched + "\\AOSPC.ini";
         //Start the connection to the AOSPC
         IniFile aospcIni = new IniFile(@iniFile);
         remotePortForAOSPC = Convert.ToInt32(aospcIni.IniReadValue("RemoteInterface", "Port"));

         //Open DMI exe 
         var dmiIniFile = aosRootPath + "\\Train" + trainNumberFetched + "\\DMI.ini";
         //Start the DMI exe's 
         Process myDMIProcess = new Process();
         try
         {
            myDMIProcess.StartInfo.UseShellExecute = false;
            myDMIProcess.StartInfo.FileName = aosRootPath + "\\DMI\\DMI.exe";
            myDMIProcess.StartInfo.CreateNoWindow = true;
            myDMIProcess = Process.Start(myDMIProcess.StartInfo.FileName, dmiIniFile);

            Thread.Sleep(2000);
         }
         catch (Exception)
         {
            writeLog(LogSource.I, "DMI process did not started correctly ");
         }

         //Start the AOSPC exe's 
         Process myAOSPCProcess = new Process();
         try
         {
            myAOSPCProcess.StartInfo.UseShellExecute = false;
            myAOSPCProcess.StartInfo.FileName = aosRootPath + "\\AOSPC\\AOSPC.exe";
            myAOSPCProcess.StartInfo.CreateNoWindow = true;
            myAOSPCProcess = Process.Start(myAOSPCProcess.StartInfo.FileName, iniFile);

            Thread.Sleep(1000);
            aospcStatus[trainNumberFetched - 1].aospcClient = new TcpClient();
            aospcStatus[trainNumberFetched - 1].istrainRunning = true;
            aospcStatus[trainNumberFetched - 1].aospcClient.Connect("127.0.0.1", remotePortForAOSPC);

            if (SetAppStatus != null)
               SetAppStatus(trainNumberFetched, true);
         }
         catch (Exception)
         {
            writeLog(LogSource.I, "Connection refused from port :" + remotePortForAOSPC);
         }
      }

      /******************************************************************************
      * Function:     setCommandToSendAOSPC
      ******************************************************************************/
      public void setCommandToSendAOSPC(String valueToSet)
      {
         CommandToSendAOSPC = valueToSet;
      }
   }
}
