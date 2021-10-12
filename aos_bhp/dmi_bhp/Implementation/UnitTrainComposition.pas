unit UnitTrainComposition;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, StdCtrls, Buttons, ColorButton2, pngimage,
  ExtCtrls;

type
  TFormTrainComposition = class(TForm)
    DBGridTrainComp: TDBGrid;
    DataSourceTrainComp: TDataSource;
    DownArrow: TColorButton;
    UpArrow: TColorButton;
    BevelCarList: TBevel;
    BevelTrainInfo: TBevel;
    BitBtnClose: TBitBtn;
    LabelTrainLen: TLabel;
    EditTrainLen: TEdit;
    LabelTrainWeight: TLabel;
    EditTrainWeight: TEdit;
    LabelTrainName: TLabel;
    EditTrainName: TEdit;
    LabelTrainWeightUnit: TLabel;
    LabelTrainLenUnit: TLabel;
    procedure FormShow(Sender: TObject);
    procedure DownArrowClick(Sender: TObject);
    procedure UpArrowClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  NoOfRecordsToScroll = 15;

var
  FormTrainComposition: TFormTrainComposition;

implementation

{$R *.dfm}
uses UnitDMIDataModule;

procedure TFormTrainComposition.UpArrowClick(Sender: TObject);
var
 I: Integer;
begin

  for I := 1 to NoOfRecordsToScroll do
  begin
  DBGridTrainComp.DataSource.DataSet.Prior;
  end;

end;



procedure TFormTrainComposition.CloseButtonClick(Sender: TObject);
begin
FormTrainComposition.Close;
end;

procedure TFormTrainComposition.DownArrowClick(Sender: TObject);
var
 I: Integer;
begin
  for I := 1 to NoOfRecordsToScroll do
  begin
    DBGridTrainComp.DataSource.DataSet.Next;
  end;
end;

procedure TFormTrainComposition.FormShow(Sender: TObject);
var
  TrainLength : Word;
  TrainLengthChanged : Boolean;
  TrainName : String;
  TrainNameChanged : Boolean;
  TrainWeight : LongWord;
  TrainWeightChanged : Boolean;
begin

  {Fix to hide the scroll-bar}
  ShowScrollBar(DBGridTrainComp.Handle, SB_VERT, False);
  {Train Length}
   TrainLength := DataModuleDMI.GetTrainLength(TrainLengthChanged);
   if (TrainLength > 0) then
   begin
     EditTrainLen.Text := IntToStr(TrainLength);
     LabelTrainLenUnit.Caption := 'm';
   end
   else
     EditTrainLen.Text := '';

  {Train Name}
   TrainName := DataModuleDMI.GetTrainId(TrainNameChanged);
   if (TrainName <> '') then
     EditTrainName.Text :=  TrainName
   else
     EditTrainName.Text := '';

  {Train Weight}
   TrainWeight := DataModuleDMI.GetTrainWeight(TrainWeightChanged);
   if (TrainWeight > 0) then
   begin
     EditTrainWeight.Text := IntToStr(TrainWeight);
     LabelTrainWeightUnit.Caption := 'Mg';
   end
   else
   begin
     EditTrainWeight.Text := '';
   end;

   DBGridTrainComp.Columns[0].Alignment := taLeftJustify;
   DBGridTrainComp.Columns[1].Alignment := taLeftJustify;
   DBGridTrainComp.Columns[2].Alignment := taLeftJustify;
   DBGridTrainComp.Columns[3].Alignment := taLeftJustify;

end;

end.
