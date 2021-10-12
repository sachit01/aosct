using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Xml.Linq;
using System.IO;

namespace TCCSim
{
    public partial class ButtonView : Form
    {
        public event ButtonSetsChanged buttonSetChanged;
        public delegate void ButtonSetsChanged(ButtonSetEventArgs e);

        public Dictionary<String, ButtonConfiguration[]> buttonSets;
        ButtonConfiguration[] activeButtonSet;
        String buttonSetPath;
        List<Label> buttonLabels;
        Label activeLabel;

        public ButtonView(List<ButtonConfiguration[]> _buttonSets, String _buttonSetPath)
        {
            InitializeComponent();
            buttonSets = new Dictionary<string, ButtonConfiguration[]>();
            buttonLabels = new List<Label>();
            for (int i = 0; i < ButtonLabelTablePanel.ColumnCount; i++)
            {
                Label l = new Label();
                l.AutoEllipsis = true;
                l.Dock = DockStyle.Fill;
                l.Click += new EventHandler(l_Click);
                buttonLabels.Add(l);
                ButtonLabelTablePanel.Controls.Add(l, i, 0);
            }

            this.buttonSetPath = _buttonSetPath;
            foreach (ButtonConfiguration[] set in _buttonSets)
            {
                buttonSets.Add(set[0].Name, set);
            }

            UpdateButtonSets();

            buttonOpenFileDialog.InitialDirectory = AppDomain.CurrentDomain.BaseDirectory;

            buttonOpenFileDialog.Filter =
                "XML file (*.xml)|*.xml|Python file (*.py)|*.py|All files (*.*)|*.*";
        }

        void l_Click(object sender, EventArgs e)
        {
            Label l = (Label)sender;
            if (activeLabel != null)
            {
                activeLabel.BackColor = l.BackColor;
            }
            activeLabel = l;
            l.BackColor = Color.ForestGreen;
            int index = buttonLabels.IndexOf(activeLabel);
            nameBox.Text = activeButtonSet[index + 1].Name;
            fileBox.Text = activeButtonSet[index + 1].Data;
            nameBox.Focus();
        }

        private void UpdateButtonSets()
        {
            String[] keys = buttonSets.Keys.ToArray<String>();
            int index = ButtonSetBox.SelectedIndex;
            ButtonSetBox.Items.Clear();
            for (int i = 0; i < buttonSets.Count; i++)
            {
                ButtonSetBox.Items.Add(keys[i]);
            }
            if (ButtonSetBox.Items.Count > 0 && index < ButtonSetBox.Items.Count)
            {
                ButtonSetBox.SelectedIndex = index < 0 ? 0 : index;
            }
        }

        private void AddButtonSet(String name)
        {
            ButtonConfiguration[] set = new ButtonConfiguration[10];
            set[0] = new ButtonConfiguration(name, "");
            for (int i = 1; i < set.Length; i++)
            {
                set[i] = new ButtonConfiguration();
            }
            buttonSets.Add(set[0].Name, set);

            UpdateButtonSets();
            ButtonSetBox.SelectedIndex = buttonSets.Count - 1;

            buttonSetChanged(new ButtonSetEventArgs(buttonSets.Values.ToList<ButtonConfiguration[]>()));
        }

        private void AddButtonSet(String name, ButtonConfiguration[] buttonSet)
        {
            ButtonConfiguration[] set = new ButtonConfiguration[10];
            set[0] = new ButtonConfiguration(name, "");
            for (int i = 1; i < set.Length; i++)
            {
                set[i] = buttonSet[i];
            }
            buttonSets.Add(set[0].Name, set);

            UpdateButtonSets();
            ButtonSetBox.SelectedIndex = buttonSets.Count - 1;

            buttonSetChanged(new ButtonSetEventArgs(buttonSets.Values.ToList<ButtonConfiguration[]>()));
        }

        private void RemoveButtonSet(ButtonConfiguration[] set)
        {
            buttonSets.Remove(set[0].Name);
            UpdateButtonSets();

            if (buttonSets.Count > 0)
            {
                int index = ButtonSetBox.SelectedIndex - 1;
                ButtonSetBox.SelectedIndex = index > 0 ? index : 0;
            }
            buttonSetChanged(new ButtonSetEventArgs(buttonSets.Values.ToList<ButtonConfiguration[]>()));
        }

        private void ButtonSetBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            activeButtonSet = buttonSets[ButtonSetBox.Text];
            nameBox.Text = "";
            fileBox.Text = "";
            for (int i = 1; i < activeButtonSet.Length; i++)
            {
                buttonLabels[i - 1].Text = activeButtonSet[i].Name;
                if (activeButtonSet[i].Data.EndsWith(".py"))
                {
                    buttonLabels[i - 1].BackColor = Color.FromArgb(194, 214, 154);
                }
                else
                {
                    buttonLabels[i - 1].BackColor = Form.DefaultBackColor;
                }
            }
            if (activeLabel != null)
            {
                activeLabel.BackColor = SystemColors.Control;
                activeLabel = null;
            }
        }

        private void SaveButton_Click(object sender, EventArgs e)
        {
            XElement ButtonSetXml = new XElement("ButtonSets");
            foreach (ButtonConfiguration[] buttonSet in buttonSets.Values)
            {
                XElement newSet = new XElement("ButtonSet");
                newSet.SetAttributeValue("Name", buttonSet[0]);
                for (int i = 1; i < buttonSet.Length; i++)
                {
                    XElement btn = new XElement("Button");
                    btn.SetAttributeValue("Name", buttonSet[i].Name);
                    btn.Value = buttonSet[i].Data;
                    newSet.Add(btn);
                }
                ButtonSetXml.Add(newSet);
            }
            try
            {
                ButtonSetXml.Save(buttonSetPath);
            }
            catch (Exception)
            {
                System.Windows.Forms.MessageBox.Show("Could not write file " + buttonSetPath +
                    ", maybe it's write protected?"
                    , "Save Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void RenameButton_Click(object sender, EventArgs e)
        {
            SimpleInputDialog sid = new SimpleInputDialog("Rename the set");
            if (sid.ShowDialog() == DialogResult.OK)
            {
                if (sid.inputMessage != "" && !buttonSets.ContainsKey(sid.inputMessage))
                {
                        int index = ButtonSetBox.SelectedIndex;
                        ButtonConfiguration[] set = activeButtonSet;
                        buttonSets.Remove(activeButtonSet[0].Name);
                        AddButtonSet(sid.inputMessage, activeButtonSet);
                        ButtonSetBox.SelectedIndex = index > 0 ? index : 0;
                }
                else
                {
                    MessageBox.Show("The name either is empty of already exists", "Name error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }

        private void NewButton_Click(object sender, EventArgs e)
        {
            SimpleInputDialog sid = new SimpleInputDialog("Name the set");
            if (sid.ShowDialog() == DialogResult.OK)
            {
                if (sid.inputMessage != "")
                    AddButtonSet(sid.inputMessage);
            }
        }

        private void DeleteButton_Click(object sender, EventArgs e)
        {
            if (activeButtonSet != null)
                RemoveButtonSet(activeButtonSet);
        }

        private void CopyButton_Click(object sender, EventArgs e)
        {
            SimpleInputDialog sid = new SimpleInputDialog("Name the set");
            if (sid.ShowDialog() == DialogResult.OK)
            {
                if (sid.inputMessage != "")
                    AddButtonSet(sid.inputMessage, activeButtonSet);
            }
        }

        private void ImportButton_Click(object sender, EventArgs e)
        {
            OpenFileDialog ofd = new OpenFileDialog();
            if (ofd.ShowDialog() == DialogResult.OK)
            {
                XElement xml = XElement.Load(ofd.FileName);

                foreach (XElement buttonset in xml.Descendants("ButtonSet"))
                {
                    ButtonConfiguration[] tmp = new ButtonConfiguration[10];
                    tmp[0] = new ButtonConfiguration(buttonset.FirstAttribute.Value, "");
                    int i = 1;
                    foreach (XElement button in buttonset.Descendants("Button"))
                    {
                        tmp[i++] = new ButtonConfiguration(button.Attribute("Name").Value, button.Value);
                    }
                    while (buttonSets.ContainsKey(tmp[0].Name))
                    {
                        tmp[0].Name = "Copy of " + tmp[0].Name;
                    }
                    buttonSets.Add(tmp[0].Name, tmp);

                }
                UpdateButtonSets();
                buttonSetChanged(new ButtonSetEventArgs(buttonSets.Values.ToList<ButtonConfiguration[]>()));
            }
        }

        private void CloseButton_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void ButtonView_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F1:
                    l_Click(buttonLabels[0], EventArgs.Empty);
                    break;
                case Keys.F2:
                    l_Click(buttonLabels[1], EventArgs.Empty);
                    break;
                case Keys.F3:
                    l_Click(buttonLabels[2], EventArgs.Empty);
                    break;
                case Keys.F4:
                    l_Click(buttonLabels[3], EventArgs.Empty);
                    break;
                case Keys.F5:
                    l_Click(buttonLabels[4], EventArgs.Empty);
                    break;
                case Keys.F6:
                    l_Click(buttonLabels[5], EventArgs.Empty);
                    break;
                case Keys.F7:
                    l_Click(buttonLabels[6], EventArgs.Empty);
                    break;
                case Keys.F8:
                    l_Click(buttonLabels[7], EventArgs.Empty);
                    break;
                case Keys.F9:
                    l_Click(buttonLabels[8], EventArgs.Empty);
                    break;
                case Keys.F12:
                    ButtonSetBox.SelectedIndex = (ButtonSetBox.SelectedIndex + 1) % ButtonSetBox.Items.Count;
                    break;
                default:
                    break;
            }
        }

        private void pickButton_Click(object sender, EventArgs e)
        {
            if (buttonOpenFileDialog.ShowDialog() == DialogResult.OK)
            {
                fileBox.Text = buttonOpenFileDialog.FileName;
            }
        }

        private void saveButtonButton_Click(object sender, EventArgs e)
        {
            int index = buttonLabels.IndexOf(activeLabel);
            if (nameBox.Text.Length > 0 && fileBox.Text.Length > 0)
            {
                activeButtonSet[index + 1].Name = nameBox.Text;
                try
                {
                    activeButtonSet[index + 1].Data = RelativePath(AppDomain.CurrentDomain.BaseDirectory, fileBox.Text);
                }
                catch (ArgumentException)
                {
                    activeButtonSet[index + 1].Data = Path.GetFullPath(fileBox.Text);
                }
                buttonSetChanged(new ButtonSetEventArgs(buttonSets.Values.ToList<ButtonConfiguration[]>()));
                UpdateButtonSets();
            }
        }

        public string RelativePath(string absPath, string relTo)
        {
            string[] absDirs = absPath.Split('\\');
            string[] relDirs = relTo.Split('\\');

            // Get the shortest of the two paths 
            int len = absDirs.Length < relDirs.Length ? absDirs.Length : relDirs.Length;

            // Use to determine where in the loop we exited i
            int lastCommonRoot = -1;
            int index;

            // Find common root 
            for (index = 0; index < len; index++)
            {
                if (absDirs[index].Equals(relDirs[index], StringComparison.OrdinalIgnoreCase))
                    lastCommonRoot = index;
                else break;
            }

            // If we didn't find a common prefix then throw 
            if (lastCommonRoot == -1)
            {
                throw new ArgumentException("Paths do not have a common base");
            }

            // Build up the relative path 
            StringBuilder relativePath = new StringBuilder();

            // Add on the .. 
            for (index = lastCommonRoot + 1; index < absDirs.Length; index++)
            {
                if (absDirs[index].Length > 0)
                    relativePath.Append("..\\");
            }

            // Add on the folders 
            for (index = lastCommonRoot + 1; index < relDirs.Length - 1; index++)
            {
                relativePath.Append(relDirs[index] + "\\");
            }
            relativePath.Append(relDirs[relDirs.Length - 1]);
            return relativePath.ToString();
        }
    }
}
