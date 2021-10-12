using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace AOSAnalyzer
{
	public partial class SliceForm : Form
	{
        public event AOSMeasuresChanged measuresChanged;

        List<AOSMarker> markers;

		public SliceForm(List<AOSMarker> _markers)
		{
			InitializeComponent();
            this.markers = _markers;
            UpdateMarkers();
		}

        private void UpdateMarkers()
        {
            fromMarkerBox.Items.Clear();
            fromMarkerBox.Items.AddRange(markers.ToArray());

            toMarkerBox.Items.Clear();
            toMarkerBox.Items.AddRange(markers.ToArray());
        }

        private void valueSliceButton_Click(object sender, EventArgs e)
        {
            int from = Int32.Parse(fromValueBox.Text);
            int to = Int32.Parse(toValueBox.Text);
            if (from < to)
            {
                measuresChanged(sender, new AOSMeasureSliceEventArgs(from, to));
            }
            else
            {
                measuresChanged(sender, new AOSMeasureSliceEventArgs(to, from));
            }
        }

        private void markerSliceButton_Click(object sender, EventArgs e)
        {
            double from = ((AOSMarker)fromMarkerBox.SelectedItem).Location.X;
            double to = ((AOSMarker)toMarkerBox.SelectedItem).Location.X;
            if (from < to)
            {
                measuresChanged(sender, new AOSMeasureSliceEventArgs(from, to));
            }
            else
            {
                measuresChanged(sender, new AOSMeasureSliceEventArgs(to, from));
            }
        }

        public void MarkerListChanged(object sender, AOSMarkerListEventArgs e)
        {
            markers = e.markers;
            UpdateMarkers();
        }

        private void onValueKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                valueSliceButton.PerformClick();
            }
        }

        private void onMarkerKeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                markerSliceButton.PerformClick();
            }
        }
	}
}
