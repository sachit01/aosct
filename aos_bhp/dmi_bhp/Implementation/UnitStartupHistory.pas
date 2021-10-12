(**************************************************************************
           © COPYRIGHT Bombardier Transportation (Signal)  AB, SWEDEN 2011.
           ================================================================

    The copyright to the computer program herein is the property of
    Bombardier Transportation (Signal) AB, Sweden. All rights reserved.
    The program may be used and/or copied only with the written permission
    from Bombardier Transportation (Signal) AB and in accordance
    with the terms and conditions stipulated in the agreement/contract
    under which the program has been supplied.


    NAME :  UnitStartupHistory.pas

    PROJECT :  LKAB

    Ver    Author           Date    Reason
    ---    ------           ------  ------
          Bo H              111206  Limit no of lines

    DESCRIPTION :  Form that displays startup-history received from the ATP

    INTERFACE :
***************************************************************************)

unit UnitStartupHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ExtCtrls, MMITypes;

type
  TFormStartUpHistory = class(TForm)
    GridPanel: TGridPanel;
    ListView: TListView;
    BitBtnClose: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    MaxStartUpHistoryLines:Integer;

  public
    { Public declarations }
    Procedure AppendHistory(Data:PStartUpHistoryMT);
  end;

var
  FormStartUpHistory: TFormStartUpHistory;

implementation

uses UnitMainArea, UnitMMIFrame, MMIStd;

{$R *.dfm}

{*********************************************************
* Function:    AppendHistory
* Description: Appends the history-lines to the list-view
*********************************************************}

Procedure TFormStartUpHistory.AppendHistory(Data:PStartUpHistoryMT);
var
  Lines : TStringList;            // Define our string list variable
  Item  : TListItem;
  I     : Integer;
  TextLength : Integer;
  ItemCount : Integer;
begin
  Lines := TStringList.Create;

  TextLength := Swap(Data.TextLength);
                                  // Terminate text
  if TextLength < Sizeof(Data.Text) then
  begin
    Data.Text[TextLength + 1]:=Char(0);
  end;


  Lines.Text := String(Data.Text);  // Set the entire stringlist from textbuf
                                    // Assume each string terminated by LF / CRLF

                                    // Insert each string
  for I := 0 to (Lines.Count - 1) do
  begin

    Item:=ListView.Items.Insert(0);
                                    // Always format in a fixed way, 24-hour clock
    Item.Caption:=FormatDateTime('yyyy-mm-dd hh:mm:ss',Now);
    Item.SubItems.Add(Lines.Strings[I]);

  end;

  Lines.Free;

                                  // Limit no of lines in listview

  while (ListView.Items.Count > MaxStartUpHistoryLines) do
  begin                           // Delete old lines if too many lines
    ItemCount := ListView.Items.Count;
    ListView.Items.Delete(ItemCount-1);
  end;

end;
procedure TFormStartUpHistory.FormCreate(Sender: TObject);
begin
                                  // Init the max no of lines in the list-view.
  MaxStartUpHistoryLines := FormMMIFrame.MaxStartUpHistoryLines;

end;

procedure TFormStartUpHistory.FormShow(Sender: TObject);
begin
  Caption := InterfaceString(ifStartupHistory );
  BitBtnClose.Caption := InterfaceString(ifCloseBtn );
  ListView.Columns.Items[0].Caption := InterfaceString(ifTime );
  ListView.Columns.Items[1].Caption := InterfaceString(ifText );
end;

end.
