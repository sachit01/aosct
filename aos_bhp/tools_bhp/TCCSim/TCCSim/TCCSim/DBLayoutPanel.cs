using System.Windows.Forms;
using System.ComponentModel;

namespace TCCSim
{
    /// <summary>
    /// TableLayoutPanel with double buffering added to improve speed and reduce flickering.
    /// </summary>
    public partial class DBLayoutPanel : TableLayoutPanel
    {
        public DBLayoutPanel()
        {
            this.DoubleBuffered = true;/*
            SetStyle(ControlStyles.OptimizedDoubleBuffer 
                | ControlStyles.AllPaintingInWmPaint 
                | ControlStyles.UserPaint
                , true);*/
            /* The double buffering is now suspended to check if it is 
             * what is causing problems with other programs. */
        }
    }
}
