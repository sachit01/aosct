/*******************************************************************************
*
* (C) COPYRIGHT Bombardier Transportation (Signal) Sweden AB, 2012
* 
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without written authority is strictly forbidden.
*
*  %name:          UnitAboutForm.cs %
*
*  %version:       3 %
*
*  %created_by:    lantback %
*
*  %date_created:  2013-07-17 09:43 %
*
*  DESCRIPTION:    Presents the units name, version and protocol version for the user. 
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
using System.Windows.Forms;

namespace AOSAnalyzer
{
    public partial class UnitAboutForm : Form
    {
        public UnitAboutForm(UnitDescr unit)
        {
            InitializeComponent();
            SetData(unit);
        }

        private void SetData(UnitDescr unit)
        {
            nameLabel.Text = unit.Name;
            versionLabel.Text = unit.Version;
            protocolLabel.Text = unit.Protocol;
        }
    }
}
