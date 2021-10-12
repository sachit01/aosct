unit UnitNoMAControlled;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UnitMainLayer3, GestureMgr, ImgList, Menus, DB, ExtCtrls, Buttons,
  StdCtrls, Grids, DBGrids, ColorButton2, MMIPanel, MMIareaC, MMIareaB;

type
  TFormNoMAControlled = class(TFormMainLayer3)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateDMI(Refresh : Boolean);
  end;

var
  FormNoMAControlled: TFormNoMAControlled;

implementation

uses UnitMMIFrame, UnitConfirmLoadedStatus, MMITypes, UnitDMIDataModule;

{$R *.dfm}

procedure TFormNoMAControlled.FormCreate(Sender: TObject);
begin

  Parent := FormMMIFrame.PanelMMIFrame;
  Align := alClient;

  inherited;
end;

{ *********************************************************
* Function:    UpdateDMI
* Description: Call this method to update this form
* Arguments: Update all when Refresh = True
********************************************************* }
procedure TFormNoMAControlled.FormShow(Sender: TObject);
begin
  inherited;
  UpdateDMI(true);

end;

procedure TFormNoMAControlled.UpdateDMI(Refresh : Boolean);
var
  AdditionalConfirmationInfo: Byte;
  AdditionalConfirmationInfoChanged: Boolean;
begin
  inherited;

  AdditionalConfirmationInfo := DataModuleDMI.GetAdditionalConfirmationInfo
    (AdditionalConfirmationInfoChanged);
  if (Refresh or AdditionalConfirmationInfoChanged) then
  begin
    if (ByteBool(AdditionalConfirmationInfo and
      CONFIRM_TRAIN_LOADED_STATUS_CHANGE)) then
      FormConfirmLoadedStatus.Show
    else
      FormConfirmLoadedStatus.Hide;
  end;
end;

end.
