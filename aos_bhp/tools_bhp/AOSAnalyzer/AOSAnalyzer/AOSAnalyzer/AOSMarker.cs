/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AOSMarker.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Instance of the marker.
*                  Inherits from the Zedgraphs LineObj so it can be added
*                  to the graphs.
*                  Contains: Name of the Marker.
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
using System.Linq;
using System.Text;
using ZedGraph;

namespace AOSAnalyzer
{
    public class AOSMarker : LineObj
    {
        public string name { get; private set; }

        public AOSMarker()
        {
            name = "Marker";
            Location.Width = 0; // this time width must be zero
            Location.X = 10;
            Location.Height = double.MaxValue;
            Location.Y = Location.Height / 2;
            Line.IsAntiAlias = true; // Get rid of the edges.
            Line.Style = System.Drawing.Drawing2D.DashStyle.Solid;
            IsClippedToChartRect = true; // when true, line isn't drawn outside the boundaries of the chart rectangle
            ZOrder = ZOrder.D_BehindAxis; // sets the order of the  in front of the curves, filling and gridlines but behind axis, border and legend. You can choose whatever you like of course.
        }

        public AOSMarker(double x)
        {
            name = "Marker";
            Location.Width = 0; // this time width must be zero
            Location.X = x;
            Location.Height = double.MaxValue;
            Location.Y = Location.Height / 2;
            Line.IsAntiAlias = true; // Get rid of the edges.
            Line.Style = System.Drawing.Drawing2D.DashStyle.Solid;
            IsClippedToChartRect = true; // when true, line isn't drawn outside the boundaries of the chart rectangle
            ZOrder = ZOrder.D_BehindAxis; // sets the order of the  in front of the curves, filling and gridlines but behind axis, border and legend. You can choose whatever you like of course.
        }

        public AOSMarker(string _name)
        {
            name = _name;
            Location.Width = 0; // this time width must be zero
            Location.X = 10;
            Location.Height = double.MaxValue;
            Location.Y = Location.Height/2;
            Line.IsAntiAlias = true; // Get rid of the edges.
            Line.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            IsClippedToChartRect = true; // when true, line isn't drawn outside the boundaries of the chart rectangle
            ZOrder = ZOrder.D_BehindAxis; // sets the order of the  in front of the curves, filling and gridlines but behind axis, border and legend. You can choose whatever you like of course.
        }

        public AOSMarker(string _name, double x)
        {
            name = _name;
            Location.X = x;

            Location.Width = 0; // this time width must be zero
            Location.Height = double.MaxValue;
            Location.Y = Location.Height / 2;
            Line.IsAntiAlias = true; // Get rid of the edges.
            Line.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            IsClippedToChartRect = true; // when true, line isn't drawn outside the boundaries of the chart rectangle
            ZOrder = ZOrder.D_BehindAxis; // sets the order of the  in front of the curves, filling and gridlines but behind axis, border and legend. You can choose whatever you like of course.
        }

        public override string ToString()
        {
            return name;
        }
    }
}
