namespace DrawCurves
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.DataVisualization.Charting.ChartArea chartArea1 = new System.Windows.Forms.DataVisualization.Charting.ChartArea();
            System.Windows.Forms.DataVisualization.Charting.ChartArea chartArea2 = new System.Windows.Forms.DataVisualization.Charting.ChartArea();
            System.Windows.Forms.DataVisualization.Charting.Legend legend1 = new System.Windows.Forms.DataVisualization.Charting.Legend();
            System.Windows.Forms.DataVisualization.Charting.Series series1 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series2 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series3 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series4 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series5 = new System.Windows.Forms.DataVisualization.Charting.Series();
            System.Windows.Forms.DataVisualization.Charting.Series series6 = new System.Windows.Forms.DataVisualization.Charting.Series();
            this.chart1 = new System.Windows.Forms.DataVisualization.Charting.Chart();
            this.button1 = new System.Windows.Forms.Button();
            this.tbCeilingSpeed = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.tbBRST = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.tbBreakability = new System.Windows.Forms.TextBox();
            this.label4 = new System.Windows.Forms.Label();
            this.tbGradient = new System.Windows.Forms.TextBox();
            this.label5 = new System.Windows.Forms.Label();
            this.tbTargetSpeed = new System.Windows.Forms.TextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.tbFWtoSwDealy = new System.Windows.Forms.TextBox();
            this.label8 = new System.Windows.Forms.Label();
            this.tbSWtoSBdelay = new System.Windows.Forms.TextBox();
            this.label9 = new System.Windows.Forms.Label();
            this.tbSBDelay = new System.Windows.Forms.TextBox();
            this.label10 = new System.Windows.Forms.Label();
            this.tbEBDelay = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.tbStartSpeed = new System.Windows.Forms.TextBox();
            ((System.ComponentModel.ISupportInitialize)(this.chart1)).BeginInit();
            this.SuspendLayout();
            // 
            // chart1
            // 
            this.chart1.BorderlineColor = System.Drawing.Color.Yellow;
            chartArea1.AxisX.IsReversed = true;
            chartArea1.Name = "ChartArea1";
            chartArea2.AxisX.IsReversed = true;
            chartArea2.Name = "ChartArea2";
            this.chart1.ChartAreas.Add(chartArea1);
            this.chart1.ChartAreas.Add(chartArea2);
            legend1.Name = "Legend1";
            this.chart1.Legends.Add(legend1);
            this.chart1.Location = new System.Drawing.Point(8, 141);
            this.chart1.Name = "chart1";
            series1.ChartArea = "ChartArea1";
            series1.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series1.Color = System.Drawing.Color.Red;
            series1.Legend = "Legend1";
            series1.Name = "EB";
            series2.ChartArea = "ChartArea1";
            series2.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series2.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(128)))));
            series2.Legend = "Legend1";
            series2.Name = "SB";
            series3.ChartArea = "ChartArea1";
            series3.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series3.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(128)))), ((int)(((byte)(0)))));
            series3.Legend = "Legend1";
            series3.Name = "SW";
            series4.ChartArea = "ChartArea1";
            series4.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series4.Color = System.Drawing.Color.Lime;
            series4.Legend = "Legend1";
            series4.Name = "FW";
            series5.ChartArea = "ChartArea1";
            series5.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series5.Legend = "Legend1";
            series5.Name = "PS";
            series6.ChartArea = "ChartArea2";
            series6.ChartType = System.Windows.Forms.DataVisualization.Charting.SeriesChartType.Line;
            series6.Color = System.Drawing.Color.Blue;
            series6.Legend = "Legend1";
            series6.Name = "PS-FW";
            this.chart1.Series.Add(series1);
            this.chart1.Series.Add(series2);
            this.chart1.Series.Add(series3);
            this.chart1.Series.Add(series4);
            this.chart1.Series.Add(series5);
            this.chart1.Series.Add(series6);
            this.chart1.Size = new System.Drawing.Size(949, 709);
            this.chart1.TabIndex = 0;
            this.chart1.Text = "chart1";
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(49, 15);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(75, 23);
            this.button1.TabIndex = 1;
            this.button1.Text = "Calculate";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // tbCeilingSpeed
            // 
            this.tbCeilingSpeed.Location = new System.Drawing.Point(324, 15);
            this.tbCeilingSpeed.Name = "tbCeilingSpeed";
            this.tbCeilingSpeed.Size = new System.Drawing.Size(100, 20);
            this.tbCeilingSpeed.TabIndex = 2;
            this.tbCeilingSpeed.Text = "1000";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(185, 18);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(105, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Ceiling Speed (cm/s)";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(458, 15);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(125, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "Brake response time (ms)";
            // 
            // tbBRST
            // 
            this.tbBRST.Location = new System.Drawing.Point(597, 8);
            this.tbBRST.Name = "tbBRST";
            this.tbBRST.Size = new System.Drawing.Size(100, 20);
            this.tbBRST.TabIndex = 4;
            this.tbBRST.Text = "15000";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(185, 122);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(100, 13);
            this.label3.TabIndex = 9;
            this.label3.Text = "Breakability (cm/s2)";
            // 
            // tbBreakability
            // 
            this.tbBreakability.Location = new System.Drawing.Point(324, 115);
            this.tbBreakability.Name = "tbBreakability";
            this.tbBreakability.Size = new System.Drawing.Size(100, 20);
            this.tbBreakability.TabIndex = 8;
            this.tbBreakability.Text = "25";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(185, 96);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(86, 13);
            this.label4.TabIndex = 7;
            this.label4.Text = "Gradient (cm/s2)";
            // 
            // tbGradient
            // 
            this.tbGradient.Location = new System.Drawing.Point(324, 93);
            this.tbGradient.Name = "tbGradient";
            this.tbGradient.Size = new System.Drawing.Size(100, 20);
            this.tbGradient.TabIndex = 6;
            this.tbGradient.Text = "0";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(185, 72);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(105, 13);
            this.label5.TabIndex = 11;
            this.label5.Text = "Target Speed (cm/s)";
            // 
            // tbTargetSpeed
            // 
            this.tbTargetSpeed.Location = new System.Drawing.Point(324, 65);
            this.tbTargetSpeed.Name = "tbTargetSpeed";
            this.tbTargetSpeed.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.tbTargetSpeed.Size = new System.Drawing.Size(100, 20);
            this.tbTargetSpeed.TabIndex = 10;
            this.tbTargetSpeed.Text = "0";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(458, 122);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(114, 13);
            this.label7.TabIndex = 19;
            this.label7.Text = "FW to SW delay (0.1s)";
            // 
            // tbFWtoSwDealy
            // 
            this.tbFWtoSwDealy.Location = new System.Drawing.Point(597, 115);
            this.tbFWtoSwDealy.Name = "tbFWtoSwDealy";
            this.tbFWtoSwDealy.Size = new System.Drawing.Size(100, 20);
            this.tbFWtoSwDealy.TabIndex = 18;
            this.tbFWtoSwDealy.Text = "20";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(458, 96);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(114, 13);
            this.label8.TabIndex = 17;
            this.label8.Text = "SW to SB delay (0.1 s)";
            // 
            // tbSWtoSBdelay
            // 
            this.tbSWtoSBdelay.Location = new System.Drawing.Point(597, 93);
            this.tbSWtoSBdelay.Name = "tbSWtoSBdelay";
            this.tbSWtoSBdelay.Size = new System.Drawing.Size(100, 20);
            this.tbSWtoSBdelay.TabIndex = 16;
            this.tbSWtoSBdelay.Text = "20";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(458, 68);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(81, 13);
            this.label9.TabIndex = 15;
            this.label9.Text = "SB delay (0.1 s)";
            // 
            // tbSBDelay
            // 
            this.tbSBDelay.Location = new System.Drawing.Point(597, 61);
            this.tbSBDelay.Name = "tbSBDelay";
            this.tbSBDelay.Size = new System.Drawing.Size(100, 20);
            this.tbSBDelay.TabIndex = 14;
            this.tbSBDelay.Text = "30";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(458, 42);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(115, 13);
            this.label10.TabIndex = 13;
            this.label10.Text = "EB + TCO delay (0.1 s)";
            // 
            // tbEBDelay
            // 
            this.tbEBDelay.Location = new System.Drawing.Point(597, 39);
            this.tbEBDelay.Name = "tbEBDelay";
            this.tbEBDelay.Size = new System.Drawing.Size(100, 20);
            this.tbEBDelay.TabIndex = 12;
            this.tbEBDelay.Text = "60";
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(185, 46);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(96, 13);
            this.label6.TabIndex = 21;
            this.label6.Text = "Start Speed (cm/s)";
            // 
            // tbStartSpeed
            // 
            this.tbStartSpeed.Location = new System.Drawing.Point(324, 39);
            this.tbStartSpeed.Name = "tbStartSpeed";
            this.tbStartSpeed.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.tbStartSpeed.Size = new System.Drawing.Size(100, 20);
            this.tbStartSpeed.TabIndex = 20;
            this.tbStartSpeed.Text = "833";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSize = true;
            this.ClientSize = new System.Drawing.Size(969, 742);
            this.Controls.Add(this.label6);
            this.Controls.Add(this.tbStartSpeed);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.tbFWtoSwDealy);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.tbSWtoSBdelay);
            this.Controls.Add(this.label9);
            this.Controls.Add(this.tbSBDelay);
            this.Controls.Add(this.label10);
            this.Controls.Add(this.tbEBDelay);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.tbTargetSpeed);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.tbBreakability);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.tbGradient);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.tbBRST);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.tbCeilingSpeed);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.chart1);
            this.Name = "Form1";
            this.Text = "Form1";
            ((System.ComponentModel.ISupportInitialize)(this.chart1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.DataVisualization.Charting.Chart chart1;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.TextBox tbCeilingSpeed;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox tbBRST;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TextBox tbBreakability;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox tbGradient;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.TextBox tbTargetSpeed;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.TextBox tbFWtoSwDealy;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.TextBox tbSWtoSBdelay;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.TextBox tbSBDelay;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.TextBox tbEBDelay;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.TextBox tbStartSpeed;
    }
}

