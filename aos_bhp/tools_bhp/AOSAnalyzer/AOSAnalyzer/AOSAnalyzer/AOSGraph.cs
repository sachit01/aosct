/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          AOSGraph.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:42 %
*
*  DESCRIPTION:    Instance of the graph.
*                  Used to keep track of what the application want to know 
*                  about a graph.
*                  Contains: A graphpane, list of markers, list of curvenames,
*                  comment, autoscaling on/off, title, maxvalue and minvalue.
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
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;
using ZedGraph;

namespace AOSAnalyzer
{
    public class AOSGraph
    {
        GraphPane gp { get; set; }
        List<AOSMarker> markers { get; set; }

        // Saved in the graph to be able to recreate the graph from the saved values.
        [XmlElement("Curves")]
        public List<String> curveNames { get; set; }
        [XmlElement("Comment")]
        public string Comment { get; set; }
        [XmlElement("Autoscaling")]
        public bool IsAuto { get; set; }
        [XmlElement("Title")]
        private string title;
        public string Title 
        {
            get
            {
                return title;
            }
            set
            {
                title = value;
                if(gp != null)
                    gp.Title.Text = value;
            }
        }
        [XmlElement("GraphMax")]
        private int graphMax;
        public int GraphMax
        {
            get
            {
                return graphMax;
            }
            set
            {
                graphMax = value;
                if(gp != null)
                    gp.YAxis.Scale.Max = value;
            }
        }
        [XmlElement("GraphMin")]
        private int graphMin;
        public int GraphMin
        {
            get
            {
                return graphMin;
            }
            set
            {
                graphMin = value;
                if (gp != null) 
                    gp.YAxis.Scale.Min = value;
            }
        }
        /// <summary>
        /// Creates an autoscaling graph without a title.
        /// </summary>
        public AOSGraph()
        {
            curveNames = new List<string>();
            markers = new List<AOSMarker>();
            IsAuto = true;
            Title = "";
            GraphMax = 10;
            GraphMin = 0;
            gp = CreateGraphPane();
        }

        /// <summary>
        /// Creates an autoscaling graph with given title.
        /// </summary>
        /// <param name="_title">Title of the AOSGraph</param>
        public AOSGraph(string _title)
        {
            curveNames = new List<string>();
            markers = new List<AOSMarker>();
            IsAuto = true;
            Title = _title;
            GraphMax = 10;
            GraphMin = 0;
            gp = CreateGraphPane();
        }
        /// <summary>
        /// Creates a graph based on the given arguments.
        /// </summary>
        /// <param name="_title">Title of the AOSGraph</param>
        /// <param name="_graphMax">Y-Max of the AOSGraph</param>
        /// <param name="_graphMin">Y-Min of the AOSGraph</param>
        /// <param name="_graphheight">Height of the AOSGraph</param>
        public AOSGraph(string _title, int _graphMax, int _graphMin)
        {
            curveNames = new List<string>();
            markers = new List<AOSMarker>();
            IsAuto = false;
            Title = _title;
            GraphMax = _graphMax;
            GraphMin = _graphMin;
            gp = CreateGraphPane();
        }

        /// <summary>
        /// Creates a GraphPane with the values from this object.
        /// </summary>
        /// <returns>A GraphPane based on this graphs values</returns>
        private GraphPane CreateGraphPane()
        {
            // Create a graph with the chosen height and empty titles.
            GraphPane _gp = new GraphPane();
            _gp.Title.Text = Title;
            if (IsAuto)
            {
                _gp.YAxis.Scale.MaxAuto = true;
                _gp.YAxis.Scale.MinAuto = true;
            }
            else
            {
                _gp.YAxis.Scale.Min = GraphMin;
                _gp.YAxis.Scale.Max = GraphMax;
            }
            return _gp;
        }

        /// <summary>
        /// Sets this objects GraphPane to the given GraphPane.
        /// </summary>
        /// <param name="_gp"></param>
        public void SetGraphPane(GraphPane _gp)
        {
            gp = _gp;
        }

        /// <summary>
        /// Returns this objects GraphPane.
        /// </summary>
        /// <returns></returns>
        public GraphPane GetGraphPane()
        {
            return gp;
        }

        /// <summary>
        /// Adds the curves string to the curvename-list.
        /// Adds the curve to the GraphPane.
        /// </summary>
        /// <param name="name">Name of the curve</param>
        /// <param name="curve">Curve to add</param>
        public void AddCurve(string name, CurveItem curve)
        {
            if (!curveNames.Contains(name))
            {
                curveNames.Add(name);
            }

            if (curve.IsY2Axis)
            {
                gp.Y2Axis.IsVisible = true;
                gp.Y2Axis.Scale.MajorStep = 1;
                gp.Y2Axis.MinorTic.Size = 0;
            }

            gp.CurveList.Add(curve);
        }

        /// <summary>
        /// Clear curvenames and curves in the GraphPane.
        /// </summary>
        public void ClearCurves()
        {
            curveNames.Clear();
            gp.CurveList.Clear();
            gp.Y2Axis.IsVisible = false;
        }

        /// <summary>
        /// Checks if the curvename exists in the curvename-list.
        /// </summary>
        /// <param name="name">Name of the curve</param>
        /// <returns>True if the graph contains the curvename.</returns> 
        public bool CointainsCurve(string name)
        {
            return curveNames.Contains(name);
        }

        /// <summary>
        /// Adds the curves which name is in curvenames
        /// to the GraphPane.
        /// </summary>
        /// <param name="curves">List with the curves available to plot</param>
        public void UpdateCurves(List<LineItem> curves)
        {
            foreach (string curvename in curveNames)
            {                
                foreach (LineItem l in curves)
                {
                    if (l.Label.Text.Equals(curvename))
                    {
                        gp.CurveList.Add(l);
                    }
                }
            }
        }
        /// <summary>
        /// Adds a marker to the graph at given x-position.
        /// </summary>
        /// <param name="xposition">Where to insert the marker</param>
        public void AddMarker(double xposition)
        {
            AOSMarker marker = new AOSMarker();
            marker.Location.X = xposition;
            markers.Add(marker);
            gp.GraphObjList.Add(marker);
        }
        /// <summary>
        /// Adds a marker to the markerlist
        /// </summary>
        /// <param name="marker">The marker to add</param>
        public void AddMarker(AOSMarker marker)
        {
            markers.Add(marker);
            gp.GraphObjList.Add(marker);
        }
        /// <summary>
        /// Removes a marker to the markerlist
        /// </summary>
        /// <param name="marker">The marker to add</param>
        public void RemoveMarker(AOSMarker marker)
        {
            markers.Remove(marker);
            gp.GraphObjList.Remove(marker);
        }
        /// <summary>
        /// Stuff to do when a marker is changed. Empty right now.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        public void OnMarkerChanged(object sender, AOSMarkerEventArgs e)
        {
        }
        /// <summary>
        /// Called when a marker is added to/removed from the markerlist.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e">Contains the new list of markers</param>
        public void OnMarkerListChanged(object sender, AOSMarkerListEventArgs e)
        {
            markers.Clear();
            gp.GraphObjList.Clear();
            foreach (AOSMarker marker in e.markers)
            {
                marker.IsVisible = true;
                marker.ZOrder = ZOrder.A_InFront;
                markers.Add(marker);
                gp.GraphObjList.Add(marker);
            }
        }
        /// <summary>
        /// Change the values on the YAxis
        /// </summary>
        public void OnAutoChanged()
        {
            if (IsAuto)
            {
                gp.YAxis.Scale.MaxAuto = true;
                gp.YAxis.Scale.MinAuto = true;
            }
            else
            {
                gp.YAxis.Scale.Min = GraphMin;
                gp.YAxis.Scale.Max = GraphMax;
            }
        }
        /// <summary>
        /// Restore scales.
        /// </summary>
        public void RestoreZoomEvents()
        {
            gp.ZoomStack.Clear();
            gp.YAxis.Scale.Max = GraphMax;
            gp.YAxis.Scale.Min = GraphMin;
            gp.YAxis.Scale.MaxAuto = IsAuto;
            gp.YAxis.Scale.MinAuto = IsAuto;
        }

        /// <summary>
        /// Returns the title as the objects string.
        /// </summary>
        /// <returns></returns>
        public override string ToString()
        {
            return Title;
        }

        /// <summary>
        /// Serialize this graph to XML
        /// </summary>
        /// <returns>Xelement to write to file.</returns>
        public XElement SerializeToXML()
        {
            XmlSerializer ser = new XmlSerializer(typeof(AOSGraph));
            XDocument doc = new XDocument();
            using (XmlWriter xw = doc.CreateWriter())
            {
                ser.Serialize(xw, this);
                xw.Close();
            }
            return doc.Root;
        }
    }
}
