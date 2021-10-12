using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Xml.Linq;
using System.Collections;
using System.Globalization;
using System.Runtime.InteropServices;

/*******************************************************************************
*
* REVISION HISTORY :
*
* Date          Name        Changes
* ---------------------------------------------------------------------------
* 2012-05-29   Rickard      Added Show()/Hide() on the tablelayoutpanel.
*                           This is a test to see if it improves the speed
*                           of the rendering.
* 2013-06-26   Blomqvist    Speedup of the layout by calling SuspendLayout and 
*                           ResumeLayout for the Parent before self. 
* 2013-07-05   Blomqvist    When adding a new field the first textbox in it 
*                           will now be focused.
*
*******************************************************************************/

namespace TCCSim
{
  public partial class MsgView : Form
  {
    #region Globals
    private IEnumerable<StringInt> MessageList;
    private Model Mdl;
    private Dictionary<string, BlockInfo> BlockFields; // Info about fields of all currentley used blocks.
    private ArrayList AllControls; // Used to index tab.
    private string CurrentMessageType;
    private string CurrentMessagePath;
    private Dictionary<string, int> BlockMax; // Block type, max entries of block.
    private Dictionary<Control, string> ValueControls; // All controls that reads values
    private Dictionary<string, Control> BlockEnablers; // Controls that enables/disables a block type. Block type, Control.
    private Dictionary<string, int> BlockCounters; // Number of blocks of each type
    private String OriginalText;
    private bool XmlLoaded = false;
    #endregion

    #region Events
    public event MsgViewEvent MsgViewE;
    public delegate void MsgViewEvent(EventArgs e);
    #endregion

    #region Constants
    private const int labelColumn = 0;
    private const int widgetColumn = 1;
    private const int resColumn = 2;
    #endregion

    public MsgView(Model M, IEnumerable<StringInt> MessageList)
    {
      this.Mdl = M;
      this.Mdl.ME += new Model.MEvent(Mdl_ME);
      this.MessageList = MessageList;
      this.BlockFields = new Dictionary<string, BlockInfo>();
      this.AllControls = new ArrayList();
      this.CurrentMessageType = null;
      BlockMax = new Dictionary<string, int>();
      ValueControls = new Dictionary<Control, string>();
      BlockEnablers = new Dictionary<string, Control>();
      BlockCounters = new Dictionary<string, int>();

      // Restore position/size of window.
      for (int i = 0; i < Screen.AllScreens.Length; i++)
      {
        if (Screen.AllScreens[i].Bounds.Contains(Properties.Settings.Default.MCBounds.Location))
          this.StartPosition = FormStartPosition.Manual;
      }

      InitializeComponent();

      // Reset previous window size and position.
      this.UpdateBounds(Properties.Settings.Default.MCBounds.X,
              Properties.Settings.Default.MCBounds.Y,
              Properties.Settings.Default.MCBounds.Width,
              Properties.Settings.Default.MCBounds.Height);
      this.WindowState = Properties.Settings.Default.MCState;

      this.OriginalText = this.Text;
      DoubleBuffered = true;
      Init();
    }

    private const int WM_SETREDRAW = 0x000B;
    private const int WM_USER = 0x400;
    private const int EM_GETEVENTMASK = (WM_USER + 59);
    private const int EM_SETEVENTMASK = (WM_USER + 69);
    int drawStopCount = 0;
    [DllImport("user32", CharSet = CharSet.Auto)]
    private extern static IntPtr SendMessage(IntPtr hWnd, int msg, int wParam, IntPtr lParam);

    IntPtr eventMask = IntPtr.Zero;

    public void StopDrawing()
    {
      if (drawStopCount == 0)
      {
        // Stop redrawing:
        SendMessage(tablePanel.Handle, WM_SETREDRAW, 0, IntPtr.Zero);
        // Stop sending of events:
        eventMask = SendMessage(tablePanel.Handle, EM_GETEVENTMASK, 0, IntPtr.Zero);
      }
      ++drawStopCount;
    }

    public void StartDrawing()
    {
      --drawStopCount;
      if (drawStopCount == 0)
      {
        // turn on events
        SendMessage(tablePanel.Handle, EM_SETEVENTMASK, 0, eventMask);

        // turn on redrawing
        SendMessage(tablePanel.Handle, WM_SETREDRAW, 1, IntPtr.Zero);

        tablePanel.Invalidate();
        tablePanel.Refresh();
      }
    }

    public void Init()
    {
      // Populate message types list.
      foreach (StringInt line in MessageList)
      {
        comboBox1.Items.Add(line);
      }
      AllControls.Add(comboBox1);

      // Fix horizontal scrollbar.
      int vertScrollWidth = SystemInformation.VerticalScrollBarWidth;
      tablePanel.Padding = new Padding(0, 0, vertScrollWidth, 0);

      comboBox1.Select(); // Select topmost control.

      // Prevent unintended scrolling in message type selection.
      comboBox1.MouseWheel += new MouseEventHandler(ScrollForward);
    }

    // Event registrator and director.
    private void Mdl_ME(EventArgs ev)
    {
      // Message type selected
      // Create input controls for selected message type
      if (ev is GetTypeEventArgs)
      {
        StopDrawing();
        LoadMessageType(ev);
        StartDrawing();
      }
      if (ev is BlockFieldsEventArgs)
      {
        StopDrawing();
        LoadBlockType(ev);
        StartDrawing();
      }
      if (ev is LoadXMLEArgs)
      {
        StopDrawing();
        LoadXML(ev);
        StartDrawing();
      }
      if (ev is LoadDataEArgs)
      {
        StopDrawing();
        LoadData(ev);
        StartDrawing();
      }
      if (ev is XMLParseErrorEventArgs)
      {
        XMLParseErrorEventArgs ex = (XMLParseErrorEventArgs)ev;
        MessageBox.Show(ex.Text, "XML Parse error", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }

    // Functions that handles events fetched by event handler.
    #region Event handlers

    // Creates the view for a specific message type.
    private void LoadMessageType(EventArgs ev)
    {
      // First clear old rows and widgets.
      for (int i = tablePanel.RowCount - 1; i > 0; --i)
      {
        RemoveRow(i);
      }
      BlockFields.Clear();
      AllControls.Clear();
      BlockMax.Clear();
      ValueControls.Clear();
      BlockEnablers.Clear();
      BlockCounters.Clear();

      GetTypeEventArgs e = (GetTypeEventArgs)ev;
      var rows = e.Fields;
      foreach (var field in rows)
      {
        int row = tablePanel.RowCount;

        // Add new row to panel.
        tablePanel.RowCount++;
        tablePanel.RowStyles.Add(new RowStyle());

        // Check if entry describes field or block
        if (field is BlockType)
        {
          BlockType bt = (BlockType)field;
          XElement bx = (XElement)bt.values[0];
          int blockValue = int.Parse(bx.Attribute("Numeric").Value);

          // Label displaying name of block type.
          Label l = new Label();
          l.AutoEllipsis = true;
          l.Text = (string)bt.values[1];
          l.SetTip("Block: " + bt.name);
          l.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;

          // Handle block with maximum occurances.
          int bmax = 0;
          try
          {
            bmax = int.Parse(bx.Element("Max").Value);
          }
          catch (NullReferenceException eb) { Console.WriteLine("Error max value of block not set.\n" + eb.Message); }
          BlockMax.Add(bt.name, bmax);

          int bmin = 0;
          try
          {
            bmin = int.Parse(bx.Element("Min").Value);
          }
          catch (NullReferenceException) { };

          // Enable/disable block
          // Box layout.
          ComboBox b = new ComboBox();
          b.Tag = bt.name;

          // Some blocks is always enabled. Then don't add disable value.
          if (bmin == 0)
          {
            b.Items.Add(new StringInt("Off", -blockValue));
          }

          b.Items.Add(new StringInt("On", blockValue));
          b.SelectedIndex = 0;
          b.DropDownStyle = ComboBoxStyle.DropDownList;


          // Box event.
          b.SelectedIndexChanged += new System.EventHandler(BlockBoxChanged);

          // Set anchors of widgets and add to panel.
          l.SetAnchors();
          b.SetAnchors();
          tablePanel.Controls.Add(l, labelColumn, row);
          AddControlRow(b, row);
          tablePanel.Controls.Add(new Label(), resColumn, row);

          // Add combobox to BlockEnablers. This is used when loading a stored message.
          BlockEnablers.Add(bt.name, b);

          // Add counters for blockEnablers, reset to 0.
          BlockCounters.Add(bt.name, 0);


          // Block should be default enabled.
          if (bmin != 0)
          {
            GetBlockEventArgs bargs = new GetBlockEventArgs(bt.name);
            BlockBoxChanged(b, bargs);
            comboBox1.Select();
          }
        }
        else
        {
          FieldType ft = (FieldType)field;
          XElement fx = (XElement)ft.values[0];
          Label l = new Label();
          l.AutoEllipsis = true; // Cut long field names
          l.Text = (string)ft.values[1];
          l.Tag = ft.name;
          l.SetTip("Field: " + ft.name + "\n" + (string)fx.Element("Detail"));
          l.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
          l.SetAnchors();

          Label resolution = new Label();

          // Field type may not have set description.
          try
          {
            resolution.Text = (string)fx.Element("Resolution").Value;
            resolution.AutoSize = false;
            resolution.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
          }
          catch (NullReferenceException) { /* We don't want to do anything. */ }

          tablePanel.Controls.Add(l, labelColumn, row);
          Control input = FieldControl(ft.name, fx);
          AddControlRow(input, row);
          ValueControls.Add(input, null);
          tablePanel.Controls.Add(resolution, resColumn, row);
          input.SetAnchors();
        }
      }
    }

    // Adds fields from a block type to the view.
    private void LoadBlockType(EventArgs ev)
    {
      //Cast EventArgs to BlockFieldsEventArgs.
      BlockFieldsEventArgs e = (BlockFieldsEventArgs)ev;

      // Get NID_BLOCK_TYPE and number of fields for that block type.
      int BlockID = int.Parse(e.Block.First().Attribute("value").Value);
      string BlockT = e.Block.First().Attribute("type").Value;
      int fields = e.Fields.Count;

      // Set value of number of blocks in global dictionary for block fields.
      BlockFields[BlockT].FieldsPerBlock = fields;

      // Store starting row to later update blocks below this.
      int startingrow = BlockFields[BlockT].StartingRow;

      bool firstControl = true;
      foreach (var field in e.Fields)
      {
        FieldType ft = (FieldType)field;
        XElement fx = (XElement)ft.values[0];
        Label l = new Label();
        l.AutoEllipsis = true;
        l.Tag = ft.name;
        l.Text = "   » " + ft.values[1];
        l.SetTip("Field: " + ft.name + "\n" + (string)fx.Element("Detail"));
        l.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
        l.SetAnchors();
        Control inputControl = FieldControl(ft.name, fx);

        Label resolution = new Label();
        resolution.Text = fx.Element("Resolution").Value;
        resolution.AutoSize = false;
        resolution.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;

        // Add new row to panel.
        int inrow = ++BlockFields[BlockT].InsertRow;
        ++BlockFields[BlockT].EndingRow;
        tablePanel.SuspendLayout();
        tablePanel.InsertRow(inrow);

        //Set anchors and add to panel.
        l.SetAnchors();
        tablePanel.Controls.Add(l, labelColumn, inrow);
        AddControlRow(inputControl, inrow);
        ValueControls.Add(inputControl, null);
        tablePanel.Controls.Add(resolution, resColumn, inrow);

        tablePanel.ResumeLayout();
        // If control added is first line in new block entry select that control.
        if (firstControl)
        {
          firstControl = false;
          inputControl.Select();
        }

      }
      // If block type can be added multiple times, add add/remove-buttons.
      if (BlockMax[BlockT] != 1)
      {
        int inrow = ++BlockFields[BlockT].InsertRow; ++BlockFields[BlockT].EndingRow;
        Button addButton = new Button();
        Button removeButton = new Button();

        addButton.Text = "Add";
        removeButton.Text = "Remove";
        addButton.Tag = new StringInt { Data = BlockT, Number = BlockID };
        removeButton.Tag = new StringInt { Data = BlockT, Number = -BlockID };

        addButton.Click += new EventHandler(BlockAddRemove);
        removeButton.Click += new EventHandler(BlockAddRemove);

        // Create panel to be able to add two buttons to one cell in layout.
        DBLayoutPanel buttonsPanel = new DBLayoutPanel();
        buttonsPanel.ColumnCount = 2;
        buttonsPanel.AutoSize = true;

        // Add buttons to button panel and add panel to panel.
        addButton.Anchor = AnchorStyles.None | AnchorStyles.Top | AnchorStyles.Right;
        removeButton.Anchor = AnchorStyles.None | AnchorStyles.Top | AnchorStyles.Right;

        buttonsPanel.Anchor = AnchorStyles.None;
        buttonsPanel.Anchor = AnchorStyles.Top | AnchorStyles.Right;
        buttonsPanel.Controls.Add(addButton);
        buttonsPanel.Controls.Add(removeButton);

        tablePanel.InsertRow(inrow);
        tablePanel.Controls.Add(new Label(), labelColumn, inrow);
        AddControlRow(buttonsPanel, inrow);
        CheckBlockDeleteButton(BlockT);
      }
      // Update BlockInfo on all blocks below this block.
      foreach (BlockInfo block in BlockFields.Values)
      {
        if (block.StartingRow > startingrow)
        {
          if (BlockMax[BlockT] != 1)
          {
            block.StartingRow += fields + 1;
            block.EndingRow += fields + 1;
            block.InsertRow += fields + 1;
          }
          else
          {
            block.StartingRow += fields;
            block.EndingRow += fields;
            block.InsertRow += fields;
          }
        }
      }

    }

    // Populate fields with data from a stored message.
    private void LoadXML(EventArgs ev)
    {
      this.Text = OriginalText;
      this.CurrentMessagePath = "";
      LoadXMLEArgs xargs = (LoadXMLEArgs)ev;
      XElement message;
      try
      {
        message = XElement.Load(xargs.File);
      }
      catch (Exception) { /* No file selected */ return; }
      //StopDrawing();
      tablePanel.SuspendLayout(); // Improve speed.
      tablePanel.Hide();
      XMLtoFields(message);
      comboBox1.Select(); // Set scroll at top.
      tablePanel.Parent.ResumeLayout();
      tablePanel.ResumeLayout();
      tablePanel.AutoScroll = false;
      tablePanel.AutoScroll = true;
      //StartDrawing();
      this.Focus();
      tablePanel.Show();

      CurrentMessagePath = xargs.File;
      String path = Path.GetFullPath(xargs.File);
      this.Text += " - (" + path + ")";
      XmlLoaded = true;
    }

    private void LoadData(EventArgs ev)
    {
      LoadDataEArgs args = (LoadDataEArgs)ev;
      //StopDrawing();
      tablePanel.Parent.SuspendLayout();
      tablePanel.SuspendLayout(); // Improve speed.
      tablePanel.Hide();
      DatatoFields(args.Data);
      comboBox1.Select(); // Set scroll at top.
                          //StartDrawing();
      tablePanel.Parent.ResumeLayout();
      tablePanel.ResumeLayout();
      tablePanel.AutoScroll = false;
      tablePanel.AutoScroll = true;
      this.Focus();
      tablePanel.Show();
    }
    #endregion


    // Event handler for message type selector.
    private void comboBox1_SelectedIndexChanged(object sender, EventArgs e)
    {
      string selBox = ((StringInt)comboBox1.SelectedItem).Data;
      if (!XmlLoaded)
      {
        CurrentMessagePath = "";
        this.Text = this.OriginalText;
      }
      XmlLoaded = false;
      // Protect against accidental changes to same type.
      if (CurrentMessageType != selBox)
      {
        CurrentMessageType = selBox;
        GetMessageEventArgs eargs = new GetMessageEventArgs(selBox);
        tablePanel.Parent.SuspendLayout();
        tablePanel.SuspendLayout();
        tablePanel.Hide();
        MsgViewE(eargs);
        tablePanel.Parent.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
        tablePanel.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
        tablePanel.AutoScroll = false;
        tablePanel.AutoScroll = true;
        tablePanel.Show();
      }
    }

    // Event handler for enabler/disabler for block types.
    private void BlockBoxChanged(object sender, EventArgs e)
    {
      ComboBox b = (ComboBox)sender;
      StringInt selBox = (StringInt)b.SelectedItem;
      string BlockT = (string)b.Tag;

      // Block is already enabled.
      // Cannot be enabled twice.
      if (BlockFields.ContainsKey(BlockT) && selBox.Number > 0)
      {
        return;
      }

      // Fields shall be added.
      if (selBox.Number > 0 && !BlockFields.ContainsKey(BlockT))
      {
        int row = tablePanel.GetRow(b);
        BlockFields.Add(BlockT, new BlockInfo
        {
          StartingRow = row,
          EndingRow = row,
          FieldsPerBlock = -1,
          InsertRow = row
        });
        GetBlockEventArgs bargs = new GetBlockEventArgs(BlockT);
        tablePanel.AutoScroll = false; // Ensure new Controls are scrolled into view.
        tablePanel.Hide();
        tablePanel.Parent.SuspendLayout();
        tablePanel.SuspendLayout();
        MsgViewE(bargs);
        tablePanel.Parent.ResumeLayout();
        tablePanel.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
        tablePanel.AutoScroll = true;
        tablePanel.Show();
      }
      // All fields of block shall be removed.
      if (selBox.Number < 0 && BlockFields.ContainsKey(BlockT))
      {
        tablePanel.Parent.SuspendLayout();
        tablePanel.SuspendLayout();
        tablePanel.Hide();
        for (int i = BlockFields[BlockT].EndingRow; i > BlockFields[BlockT].StartingRow; --i)
        {
          RemoveRow(i);
        }
        tablePanel.Parent.ResumeLayout();
        tablePanel.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
        tablePanel.AutoScroll = false; // This fixes scrollbar when removing controls
        tablePanel.AutoScroll = true;
        tablePanel.Show();

        // Update BlockInfo on all blocks below this block.
        int lines = BlockFields[BlockT].EndingRow - BlockFields[BlockT].StartingRow;

        foreach (BlockInfo block in BlockFields.Values)
        {
          if (block.StartingRow > BlockFields[BlockT].EndingRow)
          {
            block.StartingRow -= lines;
            block.EndingRow -= lines;
            block.InsertRow -= lines;
          }
        }

        BlockFields.Remove(BlockT);
      }
    }

    // Handles adding and removing of block types.
    private void BlockAddRemove(object sender, EventArgs e)
    {
      Control control = (Control)sender;
      StringInt block = (StringInt)control.Tag;

      Control parent = ((Control)sender).Parent;

      // Add rows at row BlockID.
      if (block.Number > 0)
      {
        //tablePanel.Hide();
        tablePanel.Parent.SuspendLayout();
        tablePanel.SuspendLayout();
        BlockFields[block.Data].InsertRow = tablePanel.GetRow(parent);
        GetBlockEventArgs bargs = new GetBlockEventArgs(block.Data);
        MsgViewE(bargs);
        tablePanel.Parent.ResumeLayout();
        tablePanel.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
                                   //tablePanel.AutoScroll = false;
                                   //tablePanel.AutoScroll = true;
                                   //tablePanel.Show();

        int index = control.Parent.Controls.IndexOf(control);
        Control removeButton = control.Parent.Controls[index + 1];
        // Jumps to next tabindex after remove button instead of start of form.
        this.SelectNextControl(removeButton, true, true, true, true);
      }
      // Remove rows from BlockID.
      if (block.Number < 0)
      {
        // Store the value of the scrollbar
        int yValue = tablePanel.AutoScrollPosition.Y;
        // Jumps to next tabindex after remove button instead of start of form.
        this.SelectNextControl(control, true, true, true, true);

        // Update BlockInfo on all blocks below this block.
        foreach (BlockInfo binfo in BlockFields.Values)
        {
          if (binfo.StartingRow > BlockFields[block.Data].EndingRow)
          {
            // This can only happen for multi block so compensate for one row of add/remove controlers.
            binfo.StartingRow -= BlockFields[block.Data].FieldsPerBlock + 1;
            binfo.EndingRow -= BlockFields[block.Data].FieldsPerBlock + 1;
            binfo.InsertRow -= BlockFields[block.Data].FieldsPerBlock + 1;
          }
        }

        tablePanel.Parent.SuspendLayout();
        tablePanel.SuspendLayout();
        //tablePanel.Hide();
        BlockFields[block.Data].InsertRow = tablePanel.GetRow(parent);
        for (int i = BlockFields[block.Data].InsertRow; i >= BlockFields[block.Data].InsertRow - BlockFields[block.Data].FieldsPerBlock; --i)
        {
          RemoveRow(i);
          BlockFields[block.Data].EndingRow--;
        }
        tablePanel.Parent.ResumeLayout();
        tablePanel.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
        tablePanel.AutoScroll = false; // This fixes scrolling when removing controls.

        try
        {
          tablePanel.VerticalScroll.Value = -yValue;
        }
        catch { }

        tablePanel.AutoScroll = true;
        //tablePanel.Show();
      }
      CheckBlockDeleteButton(block.Data);
    }

    // Used to add controls that shall have tab index.
    // I.E. controls in right column
    private void AddControlRow(Control c, int inrow)
    {
      AllControls.Insert(inrow - 1, c);
      tablePanel.Controls.Add(c, widgetColumn, inrow);
      for (int i = inrow - 1; i < AllControls.Count; ++i)
      {
        ((Control)AllControls[i]).TabIndex = i;
      }

      // This will forward all scroll events from the Control
      // to the TableyLayoutPanel at all times. To make the 
      // MsgView scrollable at all times.
      c.MouseWheel += new MouseEventHandler(ScrollForward);
    }

    // Forward scroll events from Controls to TableLayoutPanel.
    private void ScrollForward(object sender, EventArgs e)
    {
      HandledMouseEventArgs ma = (HandledMouseEventArgs)e;
      // Prevent default actions from take place.
      ((HandledMouseEventArgs)e).Handled = true;

      if (ma.Delta > 0)
      {
        int new_scroll = tablePanel.VerticalScroll.Value - (tablePanel.VerticalScroll.SmallChange + tablePanel.VerticalScroll.LargeChange) / 2;
        if (new_scroll > tablePanel.VerticalScroll.Minimum)
          tablePanel.VerticalScroll.Value = new_scroll;
        else
          tablePanel.VerticalScroll.Value = tablePanel.VerticalScroll.Minimum;
      }
      if (ma.Delta < 0)
      {
        int new_scroll = tablePanel.VerticalScroll.Value + (tablePanel.VerticalScroll.SmallChange + tablePanel.VerticalScroll.LargeChange) / 2;
        if (new_scroll < tablePanel.VerticalScroll.Maximum)
          tablePanel.VerticalScroll.Value = new_scroll;
        else
          tablePanel.VerticalScroll.Value = tablePanel.VerticalScroll.Maximum;
      }
    }

    // Handles removing of a row in the TableLayoutPanel.
    private void RemoveRow(int row)
    {
      Control c1 = tablePanel.GetControlFromPosition(0, row);
      Control c2 = tablePanel.GetControlFromPosition(1, row);
      Control c3 = tablePanel.GetControlFromPosition(2, row);
      c1.Dispose();
      c2.Dispose();
      //c3.Dispose();
      ValueControls.Remove((Control)AllControls[row - 1]);
      AllControls.RemoveAt(row - 1);
      tablePanel.RemoveRow(row);
      for (int i = row - 1; i < AllControls.Count; ++i)
      {
        ((Control)AllControls[i]).TabIndex--;
      }
    }

    // Restore current message type to an empty message structure.
    private void Clear()
    {
      CurrentMessagePath = "";
      this.Text = OriginalText;
      StringInt selBox = (StringInt)comboBox1.SelectedItem;
      if (selBox == null)
        return;
      GetMessageEventArgs eargs = new GetMessageEventArgs(selBox.Data);
      tablePanel.Parent.SuspendLayout();
      tablePanel.SuspendLayout();
      tablePanel.Hide();
      MsgViewE(eargs);
      tablePanel.Parent.ResumeLayout();
      tablePanel.ResumeLayout(); // Suspend/resume resolves speed problem when adding/removing controls.
      tablePanel.AutoScroll = false; // This fixes scrolling when removing controls.
      tablePanel.AutoScroll = true;
      tablePanel.Show();
    }

    // Called to check if a delete button should be enabled or not.
    // In a block delete button should only be enabled if there is
    // more than one entry.
    private void CheckBlockDeleteButton(string BlockT)
    {
      BlockInfo b = BlockFields[BlockT];

      int rowWithPanel = b.StartingRow + b.FieldsPerBlock + 1; // Starting row is row of enabling/disabling control
                                                               // so increase with 1.
      DBLayoutPanel panel = (DBLayoutPanel)tablePanel.GetControlFromPosition(widgetColumn, rowWithPanel);
      Button del = (Button)panel.GetControlFromPosition(1, 0);
      del.Enabled = rowWithPanel != b.EndingRow;
    }

    // Make all data in an input control to be selected when a control is entered.
    private void EnterControlHandler(object sender, EventArgs e)
    {
      if (sender is NumericUpDown)
      {
        NumericUpDown n = (NumericUpDown)sender;
        n.Select(0, n.Text.Length);
      }

      if (sender is TextBox)
      {
        TextBox t = (TextBox)sender;
        t.Select(0, t.Text.Length);
      }
    }

    private void LeaveControlHandler(object sender, EventArgs e)
    {
      if (sender is NumericUpDown)
      {
        NumericUpDown n = (NumericUpDown)sender;
        if (n.Name == "T_CLOCK")
        {
          ulong min_T_CLOCK_value = this.Mdl.Get_T_CLOCK_Min();
          if (n.Value > 0 && n.Value < min_T_CLOCK_value)
          {
            n.Value = 0;
            ToolTip tt = new ToolTip();
            tt.Show("Value has to be either 0 or larger than " + min_T_CLOCK_value, n, 2000);
          }
        }
        n.Select(0, n.Text.Length);
      }
    }


    // Takes an XElement and returns a Control formated
    // for that type of field.
    private Control FieldControl(string s, XElement x)
    {
      string Format;

      // If format cannot be read, return blank label.
      try
      {
        Format = x.Element("Format").Value;
      }
      catch (Exception)
      {
        return new Label();
      }
      // End of message has no inputs.
      if (s == "M_END_OF_MESSAGE")
        return new Label();

      // Try to fetch defualt value.
      string default_value = "";
      try
      {
        default_value = x.Element("Default").Value;
      }
      catch (Exception) { }

      // These can be handled the same.
      if (Format == "UINT" || Format == "INT" || Format == "ULONG")
      {
        try
        {
          if (x.Element("Special").Element("Fields").HasElements)
          {
            ComboBox box = new ComboBox();
            box.DropDownStyle = ComboBoxStyle.DropDownList;

            int index = 0;
            int count = 0;
            foreach (var l in x.Element("Special").Element("Fields").Elements())
            {
              box.Items.Add(new StringInt(l.Value, int.Parse(l.Attribute("value").Value)));
              if (l.Attribute("value").Value == default_value)
                index = count;
              count++;
            }
            box.SelectedIndex = index;
            box.SetAnchors();
            return box;
          }
        }
        catch (NullReferenceException) { }

        NumericUpDown updown;

        // Check if UpDown should handle numbers in base 16 or not.
        bool base16 = false;

        try
        {
          base16 = x.Element("Max").Value.StartsWith("0x");
        }
        catch (Exception)
        {
          try
          {
            base16 = x.Element("Min").Value.StartsWith("0x");
          }
          catch (Exception) { }
        }

        if (base16)//x.Element("Max").Value.StartsWith("0x"))
        {
          updown = new HexNumericUpDown();
          try
          {
            updown.Minimum = Convert.ToInt32(x.Element("Min").Value, 16);
          }
          catch (Exception)
          {
            MessageBox.Show(x.Attribute("type").Value + " has some error with Min tag.\nNo minimum value set."
                , "Parse error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            updown.Minimum = long.MinValue;
          }
          try
          {
            updown.Maximum = Convert.ToInt32(x.Element("Max").Value, 16);
          }
          catch (Exception)
          {
            MessageBox.Show(x.Attribute("type").Value + " has some error with Max tag.\nNo maximum value set."
                , "Parse error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            updown.Maximum = long.MaxValue;
          }
          updown.Hexadecimal = true;
        }
        ////T_CLOCK field parsing.
        else if (s == "T_CLOCK") // Parse 64 bit unsigned integer
        {
          updown = new NumericUpDown();

          try
          {
            updown.Minimum = ulong.Parse(x.Element("Min").Value);
          }
          catch (Exception)
          {
            MessageBox.Show(x.Attribute("type").Value + " has some error with Min tag.\nNo minimum value set."
                , "Parse error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            updown.Minimum = ulong.MinValue;
          }
          try
          {
            updown.Maximum = ulong.Parse(x.Element("Max").Value);
          }
          catch (Exception)
          {
            MessageBox.Show(x.Attribute("type").Value + " has some error with Max tag.\nNo maximum value set."
                , "Parse error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            updown.Maximum = ulong.MaxValue;
          }
        }
        else
        {
          updown = new NumericUpDown();
          try
          {
            updown.Minimum = int.Parse(x.Element("Min").Value);
          }
          catch (Exception)
          {
            MessageBox.Show(x.Attribute("type").Value + " has some error with Min tag.\nNo minimum value set."
                , "Parse error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            updown.Minimum = long.MinValue;
          }
          try
          {
            updown.Maximum = long.Parse(x.Element("Max").Value);
          }
          catch (Exception)
          {
            MessageBox.Show(x.Attribute("type").Value + " has some error with Max tag.\nNo maximum value set."
                , "Parse error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            updown.Maximum = long.MaxValue;
          }
        }

        updown.Enter += new EventHandler(EnterControlHandler);
        updown.Leave += new EventHandler(LeaveControlHandler);
        updown.SetAnchors();

        // Check and set default value
        if (default_value != "")
        {
          updown.Value = long.Parse(default_value);
        }
        updown.Name = s;
        return updown;
      }
      if (Format == "BITMASK")
      {
        ArrayList bits = new ArrayList();
        CheckedListBox bitbox = new CheckedListBox();

        foreach (var bit in x.Element("Special").Elements("Bits").Elements())
        {
          int index = int.Parse(bit.Attribute("value").Value);
          bits.Insert(index, new StringInt(bit.Value, index));
        }
        bitbox.Items.AddRange(bits.ToArray());

        // Check and set default values.
        if (default_value != "")
        {
          int bit_value = int.Parse(default_value);
          for (int i = 0; i < bitbox.Items.Count; ++i)
          {
            if ((bit_value & (int)Math.Pow(2, i)) != 0)
              bitbox.SetItemChecked(i, true);
          }
        }

        int h = bitbox.ItemHeight * bitbox.Items.Count;
        bitbox.Height = h + bitbox.Height - bitbox.ClientSize.Height;
        bitbox.SetAnchors();

        bitbox.CheckOnClick = true;

        return bitbox;
      }
      if (Format == "STRING")
      {
        int length;
        int.TryParse(x.Element("Length").Value, out length);
        TextBox textbox = new TextBox();
        textbox.MaxLength = length;
        textbox.Enter += new EventHandler(EnterControlHandler);
        textbox.SetAnchors();
        textbox.Text = default_value;
        return textbox;
      }

      // Don't know what this is, return textbox
      return new TextBox();
    }

    /// <summary>
    /// Reads all fields an stores values in a XML structure.
    /// </summary>
    /// <returns>XElement of all fields.</returns>
    private XElement FieldsToXML()
    {
      // Make sure a message type is selected.
      if (comboBox1.SelectedItem == null)
        return null;

      // Create head tag, <Message type name value="NID_MESSAGE_TYPE">
      XElement msgtree = new XElement(comboBox1.Text, new XAttribute("value", ((StringInt)comboBox1.SelectedItem).Number));

      foreach (Control control in AllControls)
      {
        int row = tablePanel.GetRow(control);
        Label l = (Label)tablePanel.GetControlFromPosition(labelColumn, row);
        string type = (string)tablePanel.GetControlFromPosition(labelColumn, row).Tag;
        XElement tag = null;

        if (control is NumericUpDown)
        {
          tag = new XElement(type, ((NumericUpDown)control).Value);
        }
        else if (control is CheckedListBox)
        {

          if (((CheckedListBox)control).CheckedItems.Count > 0)
          {
            XElement tmp = new XElement(type);
            foreach (StringInt elem in ((CheckedListBox)control).CheckedItems)
            {
              tmp.Add(new XElement("Bit", new XAttribute("value", elem.Number)));
            }
            tag = tmp;
          }
          else
          {
            // No bits are set. Add notification of this in tag.
            tag = new XElement(type, "<!-- No bits set -->");
          }
        }
        else if (control is ComboBox)
        {
          ComboBox cb = (ComboBox)control;
          StringInt selected = (StringInt)cb.SelectedItem;

          if (ValueControls.ContainsKey(control))
          {
            tag = new XElement(type, selected.Number);
          }
          else
          {
            // This is a block enabler, check if block is enabled.
            if (selected.Number > 0)
            {
              tag = new XElement("Block");
              tag.Add(new XAttribute("type", cb.Tag));
              tag.Add(new XAttribute("value", selected.Number));
            }
          }
        }
        // This is a container for Add/remove buttons for block.
        // If the remove button is active, the block has more than
        // one entry and has to be entered in the XML,
        // unless it is the last row of block.
        else if (control is DBLayoutPanel)
        {
          DBLayoutPanel t = (DBLayoutPanel)control;
          Control c = t.GetControlFromPosition(1, 0);
          StringInt block = (StringInt)c.Tag;

          if (c.Enabled
              && tablePanel.GetRow(control) != BlockFields[block.Data].EndingRow)
          {
            XElement tmpx = new XElement("Block");
            tmpx.Add(new XAttribute("type", block.Data));
            tmpx.Add(new XAttribute("value", -block.Number));
            msgtree.Add(tmpx);
          }
        }
        // Just fetch text from control. Store in ascii encoding.
        else
        {
          tag = new XElement(type, System.Text.Encoding.GetEncoding(28591).GetString(System.Text.Encoding.GetEncoding(28591).GetBytes(control.Text)));
        }

        msgtree.Add(tag);
      }
      return msgtree;
    }

    /// <summary>
    /// Load a message from file/loaded XElement.
    /// </summary>
    /// <param name="Xmess">XElement of of a stored message.</param>
    private void XMLtoFields(XElement Xmess)
    {
      string messagetype = Xmess.Name.ToString();

      // Change form to the right message type. 
      try
      {
        // Change form to correct message type.
        if (CurrentMessageType != messagetype)
        {
          // Search for message type.
          for (int i = 0; i < comboBox1.Items.Count; ++i)
          {
            if (((StringInt)comboBox1.Items[i]).Data == messagetype)
            {
              comboBox1.SelectedIndex = i;
              // Searched index found, end search.
              i = comboBox1.Items.Count;
            }
          }
        }
        else
        {
          // Wanted message type already selected, ensure fields are cleared.
          GetMessageEventArgs eargs = new GetMessageEventArgs(Xmess.Name.ToString());
          MsgViewE(eargs);
        }
      }
      catch (Exception)
      {
        MessageBox.Show("Could not change to message type " + Xmess.Name.ToString()
            , "Form error"
            , MessageBoxButtons.OK
            , MessageBoxIcon.Error);
      }

      tablePanel.Parent.SuspendLayout();
      tablePanel.SuspendLayout();
      int row = 0;
      IEnumerator<XElement> xi = Xmess.Elements().GetEnumerator();
      while (xi.MoveNext())
      {
        XElement x = xi.Current;
        #region debug
        if (x.Name == "M_END_OF_MESSAGE")
          continue;
        #endregion
        // Check if tag is block id, then activate that block.
        if (x.Name == "Block")
        {
          string block = default(string);
          try
          {
            block = x.Attribute("type").Value;
          }
          catch (Exception)
          {
            MessageBox.Show("Block tag missing type attribute!\nCannot continue!"
                , "Attribute error"
                , MessageBoxButtons.OK
                , MessageBoxIcon.Error);
            Clear();
          }
          ComboBox b = (ComboBox)BlockEnablers[block];
          int blockCounter = (int)BlockCounters[block];

          // One more block is added
          BlockCounters[block] = ++blockCounter;

          // Enable block if not enabled.
          if (b.SelectedIndex == 0)
          {
            // Block may be default enabled, then skip. 
            if (b.Items.Count > 1)
            {
              b.SelectedIndex = 1;
            }
          }

          // Add more block fields if more than one block in total.
          if (blockCounter > 1)
          {
            GetBlockEventArgs bargs = new GetBlockEventArgs(block);
            MsgViewE(bargs);
          }

          continue;
        }

        Control control = (Control)AllControls[row];
        // Make sure control is not a block enabler or tablelayoutpanel for add/remove-buttons.
        // If so, load next control.
        while (control is DBLayoutPanel || BlockEnablers.ContainsValue(control))
        {
          control = (Control)AllControls[++row];
        }

        if (control is NumericUpDown)
        {
          try
          {
            ((NumericUpDown)control).Value = int.Parse(x.Value);
          }
          catch (Exception)
          {
            try
            {
              ((NumericUpDown)control).Value = ulong.Parse(x.Value);
            }
            catch (Exception)
            {
              MessageBox.Show(x.Value + " is not a legal value for " + x.Name.ToString() + "\nThis value has been set to a legal value!", "Illegal value", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
          }
        }
        else if (control is ComboBox)
        {
          string value = x.Value;
          foreach (StringInt si in ((ComboBox)control).Items)
          {
            if (si.Number == int.Parse(value))
              ((ComboBox)control).SelectedItem = si;
          }
        }
        else if (control is CheckedListBox)
        {
          CheckedListBox checkbox = (CheckedListBox)control;
          Dictionary<int, int> bits = new Dictionary<int, int>();
          foreach (XElement bit in x.Elements())
          {
            bits.Add(int.Parse(bit.Attribute("value").Value), 0);
          }
          for (int j = 0; j < checkbox.Items.Count; ++j)
          {
            if (bits.ContainsKey(((StringInt)checkbox.Items[j]).Number))
            {
              checkbox.SetItemChecked(j, true);
            }
          }
        }
        else
        {
          control.Text = x.Value;
        }
        row++;
      }
      tablePanel.Parent.ResumeLayout();
      tablePanel.ResumeLayout();
    }

    private void DatatoFields(Byte[] Data)
    {
      try
      {
        XElement xml = Message.BytesToXML(Data);
        XMLtoFields(xml);
      }
      catch (Exception)
      {
        MessageBox.Show("Could not parse the transfer data");
      }
    }

    #region buttons
    private void sendButton_Click(object sender, EventArgs e)
    {
      // Ensure a message type is selected.
      if (comboBox1.SelectedItem == null)
        return;
      SendXMLEventArgs sargs = new SendXMLEventArgs(FieldsToXML());
      MsgViewE(sargs);
    }

    private void cancelButton_Click(object sender, EventArgs e)
    {
      this.Hide();
    }

    // Alter closing behavior. Just hide when requested to clos.
    protected override void OnClosing(CancelEventArgs e)
    {
      e.Cancel = true;
      base.OnClosing(e);
      this.Hide();
    }

    private void saveButton_Click(object sender, EventArgs e)
    {
      XElement message = FieldsToXML();
      if (message == null)
      {
        MessageBox.Show("Could not read message fields!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        return;
      }

      // Check if a filename is selected.
      if (CurrentMessagePath != "")
        message.Save(CurrentMessagePath);
    }

    private void saveAsButton_Click(object sender, EventArgs e)
    {
      XElement message = FieldsToXML();
      if (message == null)
      {
        MessageBox.Show("Could not read message fields!", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        return;
      }

      SaveFileDialog sfd = new SaveFileDialog();
      sfd.InitialDirectory = AppDomain.CurrentDomain.BaseDirectory;
      sfd.Filter = "XML file (*.xml)|*.xml|All files (*.*)|*.*";
      sfd.Title = "Save a message to XML.";
      sfd.ShowDialog();

      // Check if a filename is selected.
      if (sfd.FileName != "")
      {
        message.Save(sfd.FileName);
        CurrentMessagePath = sfd.FileName;
        this.Text = OriginalText + " - (" + CurrentMessagePath + ")";
      }
    }

    private void loadButton_Click(object sender, EventArgs e)
    {
      OpenFileDialog ofd = new OpenFileDialog();
      ofd.InitialDirectory = AppDomain.CurrentDomain.BaseDirectory;
      ofd.Filter = "XML file (*.xml)|*.xml|All files (*.*)|*.*";
      ofd.Title = "Load a message from XML.";
      ofd.ShowDialog();

      XElement message;
      try
      {
        message = XElement.Load(ofd.FileName);
      }
      catch (Exception) { /* No file selected */ return; }

      CurrentMessagePath = ofd.FileName;
      String path = Path.GetFullPath(CurrentMessagePath);
      this.Text = OriginalText + " - (" + path + ")";

      tablePanel.Parent.SuspendLayout();
      tablePanel.SuspendLayout(); // Improve speed.
      tablePanel.Hide();
      XMLtoFields(message);
      comboBox1.Select(); // Set scroll at top.
      tablePanel.Parent.ResumeLayout();
      tablePanel.ResumeLayout();
      tablePanel.Show();
    }

    private void clearButton_Click(object sender, EventArgs e)
    {
      Clear();
    }
    #endregion

    // Alter closing behavior.
    private void MsgView_FormClosing(object sender, FormClosingEventArgs e)
    {
      Properties.Settings.Default.MCState = this.WindowState;
      if (this.WindowState == FormWindowState.Normal)
        Properties.Settings.Default.MCBounds = this.Bounds;
      Properties.Settings.Default.Save();
    }
  }

  #region Custom controls

  // Used to get a NumericUpDown control that shows that value is in hex.
  partial class HexNumericUpDown : NumericUpDown
  {
    protected override void UpdateEditText()
    {
      base.Text = string.Format(@"0x{0:X}", (int)base.Value);
    }
  }
  #endregion

  #region Extensions
  public static class HelperExtensions
  {
    public static void SetAnchors(this Control c)
    {
      c.Anchor = AnchorStyles.None;
      c.Anchor = AnchorStyles.Left | AnchorStyles.Right | AnchorStyles.Top;
    }
    public static void SetTip(this Control c, string tip)
    {
      ToolTip labelsToolTip = new ToolTip(new Container());
      labelsToolTip.SetToolTip(c, tip);
    }

  }

  // Extension of TableLayoutPanel to add function to easily remove row.
  public static class TableLayoutPanelExtensions
  {
    public static void RemoveRow(this TableLayoutPanel tableLayoutPanel, int rowNumber)
    {
      tableLayoutPanel.AutoScroll = false; // Fix sizing of panel
      for (int i = 0; i < 2; i++) // Must iterate over controls two times to remove all on one row????
      {
        foreach (Control control in tableLayoutPanel.Controls)
        {
          int row = tableLayoutPanel.GetRow(control);
          if (row == rowNumber)
          {
            tableLayoutPanel.Controls.Remove(control);
            control.Dispose();
          }
        }
      }
      tableLayoutPanel.RowStyles.RemoveAt(rowNumber);

      // Shift controls below removed row upwards.
      foreach (Control control in tableLayoutPanel.Controls)
      {
        int row = tableLayoutPanel.GetRow(control);
        if (row > rowNumber)
        {
          tableLayoutPanel.SetRow(control, row - 1);
        }
      }
      tableLayoutPanel.RowCount--;
      tableLayoutPanel.AutoScroll = true; // Fix sizing of panel
    }

    public static void InsertRow(this TableLayoutPanel tableLayoutPanel, int inrow)
    {
      tableLayoutPanel.RowCount++;
      tableLayoutPanel.RowStyles.Insert(inrow, new RowStyle());

      // Shift controls below removed row downwards.
      foreach (Control c in tableLayoutPanel.Controls)
      {
        int row = tableLayoutPanel.GetRow(c);
        if (row >= inrow)
        {
          tableLayoutPanel.SetRow(c, row + 1);
        }
      }
    }
  }
  #endregion
}
