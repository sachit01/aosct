/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          Delegates_and_EventArgs.cs %
*
*  %version:       2 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:44 %
*
*  DESCRIPTION:    A file with the application's public delegates and Eventargs. 
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
    public delegate void AOSGraphListChanged(object sender, AOSGraphListEventArgs e);
    public delegate void AOSHeaderRead(object sender, EventArgs e);
    public delegate void AOSMarkerChanged(object sender, AOSMarkerEventArgs e);
    public delegate void AOSMarkerListChanged(object sender, AOSMarkerListEventArgs e);
    public delegate void AOSMeasuresChanged(object sender, AOSMeasureSliceEventArgs e);
    public delegate void AOSVisGraphAdded(object sender, EventArgs e);
    public delegate void DisconnectCallback(object sender, EventArgs e);
    public delegate void DisconnectedAOSEventHandler(object sender, EventArgs e);
    public delegate void GraphdataChanged();
    public delegate void NewAOSDataEventHandler(object sender, EventArgs e);
    public delegate void MeasuringStatusChanged(AOSMeasuringStatusChangedEventArgs e);

    public class AOSConnectEventArgs : EventArgs
    {
        public string ip { get; private set; }
        public int port { get; private set; }
        public string graphSetupPath { get; set; }

        public AOSConnectEventArgs(string _ip, int _port, string _path)
        {
            ip = _ip;
            port = _port;
            graphSetupPath = _path;
        }
    }

    public class AOSMeasuringStatusChangedEventArgs : EventArgs
    {
        public bool measuring { get; private set; }

        public AOSMeasuringStatusChangedEventArgs(bool _measuring)
        {
            measuring = _measuring;
        }
    }

    public class AOSVisGraphEventArgs : EventArgs
    {
        public AOSGraph graph { get; private set; }

        public AOSVisGraphEventArgs(AOSGraph _graph)
        {
            graph = _graph;
        }
    }

    public class AOSMeasureSliceEventArgs : EventArgs
    {
        public double start { get; private set; }
        public double end { get; private set; }

        public AOSMeasureSliceEventArgs(double _start, double _end)
        {
            this.start = _start;
            this.end = _end;
        }
    }

    public class AOSGraphListEventArgs : EventArgs
    {
        public List<AOSGraph> graphs { get; private set; }

        public AOSGraphListEventArgs(List<AOSGraph> _graphs)
        {
            graphs = _graphs;
        }
    }

    public class AOSMarkerEventArgs : EventArgs
    {
        public AOSMarker marker { get; private set; }

        public AOSMarkerEventArgs(AOSMarker _marker)
        {
            marker = _marker;
        }
    }
    
    public class AOSMarkerListEventArgs : EventArgs
    {
        public List<AOSMarker> markers { get; private set; }

        public AOSMarkerListEventArgs(List<AOSMarker> _markers)
        {
            markers = _markers;
        }
    }

    // Eventargs that contains data
    public class AOSDataEventArgs : EventArgs
    {
        public string Data { get; private set; }

        public AOSDataEventArgs(string _data)
        {
            Data = _data;
        }
    }
}
