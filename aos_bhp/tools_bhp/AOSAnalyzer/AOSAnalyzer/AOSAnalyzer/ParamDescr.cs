/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          ParamDescr.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2012-08-02 12:46 %
*
*  DESCRIPTION:   The file has two things:
*                 
*                 Instance of a ParameterChange. 
*                 Used to keep track of the changes made to the parameter.
*                 Contains: Struct with Name, old Value and new Value of the parameter.
*  
*                 Instance of an Parameter. 
*                 Contains: Name, Description, Type, Unit, Min and Max values.
*              
*
******************************************************************************/

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2011-07-26    Blomqvist   File created
*
*******************************************************************************/
using System;
using System.Collections.Generic;
using System.Text;

namespace AOSAnalyzer
{
    // Used to store the parameterchanges made
    public struct ParameterChange
    {
        public string parameterName;
        public string oldValue;
        public string newValue;

        public ParameterChange(string _parName, string _oldValue, string _newValue)
        {
            this.parameterName = _parName;
            this.oldValue = _oldValue;
            this.newValue = _newValue;
        }

        public string[] GetData()
        {
            string[] data = { parameterName, oldValue, newValue };
            return data;
        }
    }


    public class ParamDescr
    {
        public string Name { get; set; }

        public string Description { get; set; }

        public string Type { get; set; }

        public string Unit { get; set; }

        public string Min { get; set; }

        public string Max { get; set; }

        public string Value { get; set; }

        public ParamDescr()
        {
            Name = null;
            Description = null;
            Type = null;
            Unit = null;
            Min = null;
            Max = null;
            Value = null;
        }

        public ParamDescr(string _line)
        {
            parseString(_line);
        }
        
        override public string  ToString()
        {
            return Name;
        }

        private void parseArray(string[] array)
        {
            Name = array[0].Replace("\"", "");
            Description = array[1].Replace("\"", "");
            Type = array[2].Replace("\"", "");
            Unit = array[3].Replace("\"", "");
            Min = array[4].Replace("\"", "");
            Max = array[5].Replace("\"", "");
            Value = array[6].Replace("\"", "");
        }
                
        public void parseString(string line)
        {
            string[] values = line.Split(new char[] {';'});
            parseArray(values);
        }

        //
        // Returns:
        //  1 if p1 > p2 
        //  0 if p1 = p2
        // -1 if p1 < p2
        //
        public static int Compare(ParamDescr p1, ParamDescr p2)
        {
            if (p1 == null)
            {
                if (p2 == null)
                {
                    return 0;
                }
                else
                {
                    return -1;
                }
            }
            else
            {
                if (p2 == null)
                {
                    return 1;
                }
                else
                {
                    return p1.Name.CompareTo(p2.Name);
                }
            }
        }
    }
}
