unit fFileUnlock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, uFileUnlock;

type

  { TfrmFileUnlock }

  TfrmFileUnlock = class(TForm)
    btnUnlockAll: TButton;
    btnUnlock: TButton;
    btnClose: TButton;
    btnTerminate: TButton;
    stgFileHandles: TStringGrid;
    procedure btnTerminateClick(Sender: TObject);
    procedure btnUnlockAllClick(Sender: TObject);
    procedure btnUnlockClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure stgFileHandlesDblClick(Sender: TObject);
    procedure stgFileHandlesSelection(Sender: TObject; aCol, aRow: Integer);
  private
    procedure ShowThread;
    procedure UnlockRow(Index: Integer);
  public

  end;

function ShowUnlockForm(ProcessInfo: TProcessInfoArray): Boolean;

implementation

{$R *.lfm}

uses
  Windows, Math, LCLStrConsts, fMain, uMyWindows, uLng;

function ShowUnlockForm(ProcessInfo: TProcessInfoArray): Boolean;
var
  Index: Integer;
  UnlockEnabled: Boolean = False;
begin
  with TfrmFileUnlock.Create(frmMain) do
  try
    stgFileHandles.RowCount:= Length(ProcessInfo) + 1;

    for Index:= 1 to stgFileHandles.RowCount - 1 do
    begin
      if (ProcessInfo[Index - 1].FileHandle <> 0) then begin
        UnlockEnabled:= True;
        stgFileHandles.Cells[0, Index]:= IntToStr(ProcessInfo[Index - 1].FileHandle);
      end;
      stgFileHandles.Cells[1, Index]:= IntToStr(ProcessInfo[Index - 1].ProcessId);
      stgFileHandles.Cells[2, Index]:= ProcessInfo[Index - 1].ExecutablePath;
    end;
    btnUnlockAll.Enabled:= UnlockEnabled;
    btnTerminate.Enabled:= Length(ProcessInfo) > 0;
    stgFileHandles.Row:= IfThen(UnlockEnabled, 1, 0);
    btnUnlock.Enabled:= UnlockEnabled and (Length(stgFileHandles.Cells[0, 1]) > 0);

    TThread.Synchronize(nil, @ShowThread);

    Result:= (ModalResult = mrOK);
  finally
    Free;
  end;
end;

{ TfrmFileUnlock }

procedure TfrmFileUnlock.stgFileHandlesSelection(Sender: TObject; aCol, aRow: Integer);
begin
  btnUnlock.Enabled:= (aRow > 0) and (Length(stgFileHandles.Cells[0, aRow]) > 0);
end;

procedure TfrmFileUnlock.btnUnlockClick(Sender: TObject);
begin
  UnlockRow(stgFileHandles.Row);
  if (stgFileHandles.RowCount = 1) then
  begin
    Close;
    ModalResult:= mrOK;
  end;
end;

procedure TfrmFileUnlock.FormShow(Sender: TObject);
var
  Index: Integer;
begin
  for Index:= 0 to stgFileHandles.Columns.Count - 1 do
  begin
    stgFileHandles.Columns[Index].Width:= stgFileHandles.Canvas.TextWidth(stgFileHandles.Columns[Index].Title.Caption + 'W');
  end;
end;

procedure TfrmFileUnlock.stgFileHandlesDblClick(Sender: TObject);
var
  AHandle: HWND;
  ProcessId: DWORD;
begin
  if (stgFileHandles.Row > 0) then
  begin
    ProcessId:= StrToDWord(stgFileHandles.Cells[1, stgFileHandles.Row]);
    AHandle:= FindMainWindow(ProcessId);
    if AHandle <> 0 then ShowWindowEx(AHandle);
  end;
end;

procedure TfrmFileUnlock.btnUnlockAllClick(Sender: TObject);
var
  Index: Integer;
begin
  for Index:= stgFileHandles.RowCount - 1 downto 1 do
  begin
    UnlockRow(Index);
  end;
  if (stgFileHandles.RowCount = 1) then
  begin
    Close;
    ModalResult:= mrOK;
  end;
end;

procedure TfrmFileUnlock.btnTerminateClick(Sender: TObject);
var
  Index: Integer;
  ProcessId: DWORD;
begin
  if (stgFileHandles.Row > 0) then
  begin
    if MessageBoxW(Handle, PWideChar(UTF8Decode(rsMsgTerminateProcess)), PWideChar(UTF8Decode(rsMtWarning)), MB_YESNO or MB_ICONWARNING) = IDYES then
    begin
      ProcessId:= StrToDWord(stgFileHandles.Cells[1, stgFileHandles.Row]);
      if uFileUnlock.TerminateProcess(ProcessId) then
      begin
        for Index:= stgFileHandles.RowCount - 1 downto 1 do
        begin
          if (ProcessId = StrToDWord(stgFileHandles.Cells[1, Index])) then
            stgFileHandles.DeleteRow(Index);
        end;
        if (stgFileHandles.RowCount = 1) then
        begin
          Close;
          ModalResult:= mrOK;
        end;
      end;
    end;
  end;
end;

procedure TfrmFileUnlock.ShowThread;
begin
  ShowModal;
end;

procedure TfrmFileUnlock.UnlockRow(Index: Integer);
var
  ProcessId: DWORD;
  FileHandle: HANDLE;
begin
  ProcessId:= StrToDWord(stgFileHandles.Cells[1, Index]);
  FileHandle:= StrToQWord(stgFileHandles.Cells[0, Index]);
  if FileUnlock(ProcessId, FileHandle) then stgFileHandles.DeleteRow(Index);
end;

end.

