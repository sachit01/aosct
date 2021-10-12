
unit UnitViewLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UnitMainArea, MMITypes, MMIStd, ComCtrls, Gestures, GestureMgr,
  ExtCtrls, ActnList, PlatformDefaultStyleActnCtrls, ActnMan, ToolWin, Buttons;

type
  TFormViewLog = class(TForm)
    Panel: TPanel;
    Memo: TMemo;
    GestureManager: TGestureManager;
    ActionManager: TActionManager;
    ActionTop: TAction;
    ActionBottom: TAction;
    BitBtnCancel: TBitBtn;
    ButtonBottom: TButton;
    ButtonTop: TButton;
    ButtonPrevPage: TButton;
    ActionPrevPage: TAction;
    ButtonNextPage: TButton;
    ActionNextPage: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure ActionTopExecute(Sender: TObject);
    procedure ActionBottomExecute(Sender: TObject);
    procedure ActionPrevPageExecute(Sender: TObject);
    procedure ActionNextPageExecute(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
    Procedure RefreshLinesFromLog;
    function  GetVisibleLineCount(Memo: TMemo): Integer;

  public
    { Public declarations }
    LogRoot : String ;
    LogPath : String;

    constructor Create(AOwner: TComponent); override;
    Procedure ScreenChange;
  end;

var
  FormViewLog: TFormViewLog;

implementation

uses UnitMMIFrame;

{$R *.DFM}

const
  ViewLogErrorCreate = 1;

var
  LinesPerPage : Integer; { Will be calculated in FormShow }

procedure TFormViewLog.FormCreate(Sender: TObject);
begin
  ScreenChange;
end;

procedure TFormViewLog.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if (EventInfo.GestureID = sgiRight) or (EventInfo.GestureID = sgiLeft) then
  begin
    Hide;
  end;

end;

function TFormViewLog.GetVisibleLineCount(Memo: TMemo): Integer;
var
  DC: HDC;
  SaveFont: HFONT;
  TextMetric: TTextMetric;
  EditRect: TRect;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Memo.Font.Handle);
  GetTextMetrics(DC, TextMetric);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);

  Memo.Perform(EM_GETRECT, 0, LPARAM(@EditRect));
  Result := (EditRect.Bottom - EditRect.Top) div TextMetric.tmHeight;
end;

procedure TFormViewLog.FormShow(Sender: TObject);
var
  LogFileName : String;

begin

  LogFileName := '\N-JRU_' + FormatDateTime('yyyy-mm-dd',Now) + '.Log';
  LogPath := LogRoot +  LogFileName;

  RefreshLinesFromLog();

end;

Procedure TFormViewLog.RefreshLinesFromLog;
Var
  ThisFile : TextFile;
  ThisLine : String;

Begin

  Memo.Clear;
  if FileExists(LogPath) then
  begin

    AssignFile(ThisFile, LogPath);
    Reset(ThisFile);

      { Do not use LoadFromFile because it fails sometimes }
    while not Eof(ThisFile) do
    begin
      ReadLn(ThisFile, ThisLine);
      Memo.Lines.Add(Copy(ThisLine,21,200));
    end;

    CloseFile(ThisFile);

    SendMessage(Memo.Handle, EM_LINESCROLL, 0,Memo.Lines.Count);

    LinesPerPage := GetVisibleLineCount(Memo);

  end
  else
  begin

    ThisLine := 'No logfile exists at:' + LogPath;
    Memo.Lines.Add(ThisLine);
    LinesPerPage := 0;

  end;

End;


Procedure TFormViewLog.ScreenChange;
Begin

  AdjustControl(Panel);
  AdjustControl(Memo);

End;

procedure TFormViewLog.ActionBottomExecute(Sender: TObject);
begin
  SendMessage(Memo.Handle, EM_LINESCROLL, 0,Memo.Lines.Count);

end;

procedure TFormViewLog.ActionNextPageExecute(Sender: TObject);
begin
   SendMessage(Memo.Handle, EM_LINESCROLL, 0,LinesPerPage);

end;

procedure TFormViewLog.ActionPrevPageExecute(Sender: TObject);
begin
   SendMessage(Memo.Handle, EM_LINESCROLL, 0,-LinesPerPage);
end;

procedure TFormViewLog.ActionTopExecute(Sender: TObject);
begin
   SendMessage(Memo.Handle, EM_LINESCROLL, 0,-Memo.Lines.Count);
end;

procedure TFormViewLog.BitBtnCancelClick(Sender: TObject);
begin
  Hide;
end;

constructor TFormViewLog.Create(AOwner: TComponent);
begin
  Try
    inherited Create(AOwner);
    Left := 0;
    Top := 0 ;
{    Width := Screen.Width;
    Height := Screen.Height;}

    Parent := FormMMIFrame.PanelMMIFrame;
    Align := alClient;

  Except
    on E: Exception do
    begin
      FormMMIFrame.WriteMMIStatus('VIEWLOG',MS_SW_ERROR,MS_COMM_LOST,ViewLogErrorCreate);
      if FormMMIFrame.TestMode then
        MessageBox(Handle,PWideChar('Error in Create:'+E.Message),PWideChar(ClassName),MB_OK or MB_ICONEXCLAMATION);
      Application.Terminate ;
    end;
 End;
end;
end.
