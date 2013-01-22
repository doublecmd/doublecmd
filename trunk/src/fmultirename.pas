{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : Pavel Letko (letcuv@centrum.cz)

   Advanced multi rename tool

   contributors:

   Copyright (C) 2007-2011  Koblov Alexander (Alexx2000@mail.ru)
}

unit fMultiRename;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Forms, StdCtrls, Menus, SynRegExpr, Controls, LCLType,
  DCClassesUtf8, uClassesEx, uFile, uFileSource, StringHashList, Grids, ExtCtrls, DCXmlConfig;

type

  PMultiRenamePreset = ^TMultiRenamePreset;
  TMultiRenamePreset = record
    FileName: String;
    Extension: String;
    FileNameStyle: Integer;
    ExtensionStyle: Integer;
    Find: String;
    Replace: String;
    RegExp: Boolean;
    UseSubs: Boolean;
    Counter: String;
    Interval: String;
    Width: Integer;
    Log: Boolean;
    LogFile: String;
  end;

  { TfrmMultiRename }

  TfrmMultiRename = class(TForm)
    btnLoadPreset: TButton;
    btnSavePreset: TButton;
    btnDeletePreset: TButton;
    cbRegExp: TCheckBox;
    cbUseSubs: TCheckBox;
    cmbExtensionStyle: TComboBox;
    cbPresets: TComboBox;
    miDay2: TMenuItem;
    miHour1: TMenuItem;
    miMinute1: TMenuItem;
    miSecond1: TMenuItem;
    pnlOptions: TPanel;
    StringGrid: TStringGrid;
    gbPresets: TGroupBox;
    gbMaska: TGroupBox;
    lbName: TLabel;
    lbExt: TLabel;
    edName: TEdit;
    edExt: TEdit;
    btnNameMenu: TButton;
    btnExtMenu: TButton;
    gbFindReplace: TGroupBox;
    lbFind: TLabel;
    lbReplace: TLabel;
    edFind: TEdit;
    edReplace: TEdit;
    cmbNameStyle: TComboBox;
    gbCounter: TGroupBox;
    lbStNb: TLabel;
    lbInterval: TLabel;
    lbWidth: TLabel;
    edPoc: TEdit;
    edInterval: TEdit;
    cmbxWidth: TComboBox;
    btnRename: TButton;
    btnClose: TButton;
    gbLog: TGroupBox;
    edFile: TEdit;
    cbLog: TCheckBox;
    btnRestore: TButton;
    miPlugin: TMenuItem;
    N5: TMenuItem;
    miDay3: TMenuItem;
    miDay1: TMenuItem;
    miMonth3: TMenuItem;
    miMonth2: TMenuItem;
    miMonth1: TMenuItem;
    miYear1: TMenuItem;
    ppNameMenu: TPopupMenu;
    miNextName: TMenuItem;
    miName: TMenuItem;
    miNameX: TMenuItem;
    miNameXX: TMenuItem;
    N1: TMenuItem;
    miNextExtension: TMenuItem;
    Extension: TMenuItem;
    miExtensionX: TMenuItem;
    miExtensionXX: TMenuItem;
    N2: TMenuItem;
    miCounter: TMenuItem;
    N3: TMenuItem;
    miNext: TMenuItem;
    miYear: TMenuItem;
    miMonth: TMenuItem;
    miDay: TMenuItem;
    N4: TMenuItem;
    miHour: TMenuItem;
    miMinute: TMenuItem;
    miSecond: TMenuItem;
    procedure btnLoadPresetClick(Sender: TObject);
    procedure btnSavePresetClick(Sender: TObject);
    procedure btnDeletePresetClick(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure cmbNameStyleChange(Sender: TObject);
    procedure StringGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure StringGridTopLeftChanged(Sender: TObject);
    procedure edPocChange(Sender: TObject);
    procedure edIntervalChange(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnNameMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure miDay1Click(Sender: TObject);
    procedure miDay2Click(Sender: TObject);
    procedure miDay3Click(Sender: TObject);
    procedure miDayClick(Sender: TObject);
    procedure miHour1Click(Sender: TObject);
    procedure miHourClick(Sender: TObject);
    procedure miMinute1Click(Sender: TObject);
    procedure miMinuteClick(Sender: TObject);
    procedure miMonth1Click(Sender: TObject);
    procedure miMonth2Click(Sender: TObject);
    procedure miMonth3Click(Sender: TObject);
    procedure miMonthClick(Sender: TObject);
    procedure miSecond1Click(Sender: TObject);
    procedure miSecondClick(Sender: TObject);
    procedure miYear1Click(Sender: TObject);
    procedure miYearClick(Sender: TObject);
    procedure miPluginClick(Sender: TObject);
    procedure NameClick(Sender: TObject);
    procedure NameXClick(Sender: TObject);
    procedure NameXXClick(Sender: TObject);
    procedure ExtensionClick(Sender: TObject);
    procedure CounterClick(Sender: TObject);
    procedure btnExtMenuClick(Sender: TObject);
    procedure cbLogClick(Sender: TObject);
    procedure ExtensionXClick(Sender: TObject);
    procedure ExtensionXXClick(Sender: TObject);
 private
    IniPropStorage: TIniPropStorageEx;
    FLastPreset: String;
    FFileSource: IFileSource;
    FFiles: TFiles;
    FPresets: TStringHashList; // of PMultiRenamePreset
    FSourceRow: Integer;
    FMoveRow : Boolean;

    {Handles a single formatting string}
    function sHandleFormatString(const sFormatStr: string; ItemNr: Integer): string;
    {Function sReplace call sReplaceXX with parametres}
    function sReplace(sMask: string; ItemNr: Integer): string;
    {sReplaceXX doing N, Nx, Nx:y and E, Ex, Ex:y}
    function sReplaceXX(const sFormatStr, sOrig: string): string;
    {InsertMask is for write key symbols from buttons}
    procedure InsertMask(const Mask:string;edChoose:Tedit);
    procedure InsertMask(const Mask:string;editNr:PtrInt);
    {Get new file name for file with ItemIndex}
    function FreshText(ItemIndex: Integer): UTF8String;
    {Executes the main operation of renaming files}
    procedure RenameFiles;
    {Changes first char to uppercase and the rest to lowercase}
    function FirstCharToUppercaseUTF8(InputString: String): String;
    {Changes first char of first word to uppercase and the rest to lowercase}
    function FirstCharOfFirstWordToUppercaseUTF8(InputString: String): String;
    {Changes first char of every word to uppercase and the rest to lowercase}
    function FirstCharOfEveryWordToUppercaseUTF8(InputString: String): String;
    {Returns true if a byte represents a letter.}
    function IsLetter(AChar: AnsiChar): Boolean;
    {Applies style (uppercase, lowercase, etc.) to a string}
    function ApplyStyle(InputString: String; Style: Integer): String;
    {Load preset configuration}
    procedure LoadPresets;
    procedure LoadPresetsXml(AConfig: TXmlConfig);
    {Save preset configuration}
    procedure SavePresets;
    procedure SavePresetsXml(AConfig: TXmlConfig);
    {Loads specified preset from the configuration}
    procedure LoadPreset(PresetName: String);
    {Saves specified preset to the configuration}
    procedure SavePreset(PresetName: String);
    {Delete specified preset from configuration}
    procedure DeletePreset(PresetName: String);
    {Fills presets list with preset from configuration}
    procedure FillPresetsList;
    {Removes all presets from the presets list}
    procedure ClearPresetsList;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent; aFileSource: IFileSource; var aFiles: TFiles); reintroduce;
    destructor Destroy; override;

    // Temporary for switching configuration from INI to XML
    procedure PublicSavePresets;
    procedure LoadPresetsIni(IniFile: TIniFileEx);
    procedure SavePresetsIni(IniFile: TIniFileEx);
  end;

{initialization function}
  function ShowMultiRenameForm(aFileSource: IFileSource; var aFiles: TFiles):Boolean;

implementation

{$R *.lfm}

uses
  LCLProc, FileUtil, uDebug, uLng, uGlobs, uFileProcs, DCOSUtils, DCStrUtils,
  fSelectTextRange, uShowMsg, uFileSourceUtil, uFileProperty, uFileFunctions;

const
  sPresetsSection = 'MultiRenamePresets';

function ShowMultiRenameForm(aFileSource: IFileSource; var aFiles: TFiles):Boolean;
begin
  Result:= True;
  try
    with TfrmMultiRename.Create(Application, aFileSource, aFiles) do
    begin
      Show;
    end;
  except
    Result:= False;
  end;
end;

constructor TfrmMultiRename.Create(TheOwner: TComponent; aFileSource: IFileSource; var aFiles: TFiles);
begin
  FPresets := TStringHashList.Create(False);
  FFileSource := aFileSource;
  FFiles := aFiles;
  aFiles := nil;
  FSourceRow := -1;
  FMoveRow := False;
  inherited Create(TheOwner);
end;

destructor TfrmMultiRename.Destroy;
begin
  inherited Destroy;
  ClearPresetsList;
  FreeAndNil(FPresets);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
end;

procedure TfrmMultiRename.FormCreate(Sender: TObject);
begin
  // Localize File name style ComboBox
  ParseLineToList(rsMulRenFileNameStyleList, cmbNameStyle.Items);
  ParseLineToList(rsMulRenFileNameStyleList, cmbExtensionStyle.Items);

  // Set row count
  StringGrid.RowCount:= FFiles.Count + 1;
  StringGrid.FocusRectVisible := False;

  // Initialize property storage
  IniPropStorage:= InitPropStorage(Self);
  IniPropStorage.StoredValues.Add.DisplayName:= 'lsvwFile_Columns.Item0_Width';
  IniPropStorage.StoredValues.Add.DisplayName:= 'lsvwFile_Columns.Item1_Width';
  IniPropStorage.StoredValues.Add.DisplayName:= 'lsvwFile_Columns.Item2_Width';

  // Set default values for controls.
  btnRestoreClick(nil);

  // Initialize presets.
  LoadPresets;
  FillPresetsList;
  cbPresets.Text := FLastPreset;
  LoadPreset(FLastPreset);
end;

procedure TfrmMultiRename.FormShow(Sender: TObject);
begin
  with StringGrid.Columns do
  begin
    Items[0].Width:= StrToIntDef(IniPropStorage.StoredValue['lsvwFile_Columns.Item0_Width'], Items[0].Width);
    Items[1].Width:= StrToIntDef(IniPropStorage.StoredValue['lsvwFile_Columns.Item1_Width'], Items[1].Width);
    Items[2].Width:= StrToIntDef(IniPropStorage.StoredValue['lsvwFile_Columns.Item2_Width'], Items[2].Width);
  end;
end;

procedure TfrmMultiRename.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SavePresets;

  CloseAction:= caFree;
  with StringGrid.Columns do
  begin
    IniPropStorage.StoredValue['lsvwFile_Columns.Item0_Width']:= IntToStr(Items[0].Width);
    IniPropStorage.StoredValue['lsvwFile_Columns.Item1_Width']:= IntToStr(Items[1].Width);
    IniPropStorage.StoredValue['lsvwFile_Columns.Item2_Width']:= IntToStr(Items[2].Width);
  end;
end;

procedure TfrmMultiRename.miDay1Click(Sender: TObject);
begin
  InsertMask('[DD]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miDay2Click(Sender: TObject);
begin
  InsertMask('[DDD]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miDay3Click(Sender: TObject);
begin
  InsertMask('[DDDD]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miDayClick(Sender: TObject);
begin
  InsertMask('[D]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miHour1Click(Sender: TObject);
begin
  InsertMask('[hh]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miHourClick(Sender: TObject);
begin
  InsertMask('[h]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miMinute1Click(Sender: TObject);
begin
  InsertMask('[nn]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miMinuteClick(Sender: TObject);
begin
  InsertMask('[n]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miMonth1Click(Sender: TObject);
begin
  InsertMask('[MM]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miMonth2Click(Sender: TObject);
begin
  InsertMask('[MMM]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miMonth3Click(Sender: TObject);
begin
  InsertMask('[MMMM]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miMonthClick(Sender: TObject);
begin
  InsertMask('[M]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miSecond1Click(Sender: TObject);
begin
  InsertMask('[ss]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miSecondClick(Sender: TObject);
begin
  InsertMask('[s]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miYear1Click(Sender: TObject);
begin
  InsertMask('[YYYY]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.miYearClick(Sender: TObject);
begin
  InsertMask('[Y]',ppNameMenu.Tag);
end;

function TfrmMultiRename.FreshText(ItemIndex: Integer): UTF8String;
var
  bError: Boolean;
  sTmpName, sTmpExt: UTF8String;
begin
  bError:= False;

  // Use mask
  sTmpName:=sReplace(edName.Text, ItemIndex);
  sTmpExt:=sReplace(edExt.Text, ItemIndex);

  // Join
  Result := sTmpName;
  if sTmpExt <> '' then
    Result := Result + '.' + sTmpExt;

  // Find and replace
  if cbRegExp.Checked and (edFind.Text <> '') then
    try
      Result:= ReplaceRegExpr(edFind.Text, Result, edReplace.Text, cbUseSubs.Checked);
    except
      Result:= rsMsgErrRegExpSyntax;
      bError:= True;
    end
  else
    Result:=StringReplace(Result,edFind.Text,edReplace.Text,[rfReplaceAll,rfIgnoreCase]);

  // File name style
  sTmpName := ExtractOnlyFileName(Result);
  sTmpExt  := ExtractFileExt(Result);

  sTmpName := ApplyStyle(sTmpName, cmbNameStyle.ItemIndex);
  sTmpExt  := ApplyStyle(sTmpExt, cmbExtensionStyle.ItemIndex);

  Result := sTmpName + sTmpExt;

  btnRename.Enabled:= not bError;
  if bError then
    begin
      edFind.Color := clRed;
      edFind.Font.Color := clWhite;
    end
  else
    begin
      edFind.Color := clWindow;
      edFind.Font.Color := clWindowText;
    end;
end;

procedure TfrmMultiRename.cmbNameStyleChange(Sender: TObject);
begin
  StringGridTopLeftChanged(StringGrid);
end;

procedure TfrmMultiRename.StringGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  tmpFile: TFile;
  DestRow: Integer;
begin

  DestRow := StringGrid.Row;

  if (Shift = [ssShift]) then
  begin
    case Key of
      VK_UP: begin DestRow := StringGrid.Row - 1; end;
      VK_DOWN: begin DestRow := StringGrid.Row + 1 end;
    end;

    if (DestRow <> StringGrid.Row) and (0 < DestRow) and (DestRow < StringGrid.RowCount) then
      begin
        tmpFile := FFiles.Items[DestRow - 1];
        FFiles.Items[DestRow - 1] := FFiles.Items[StringGrid.Row - 1];
        FFiles.Items[StringGrid.Row - 1] := tmpFile;

        StringGridTopLeftChanged(StringGrid);
      end;
  end;
end;

procedure TfrmMultiRename.StringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  SourceCol: Integer;
begin
  if(Button = mbLeft) then
  begin
    StringGrid.MouseToCell(X, Y, SourceCol, FSourceRow);
    if (FSourceRow > 0) then
    begin
      FMoveRow := True;
    end;
  end;
end;

procedure TfrmMultiRename.StringGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMoveRow := False;
  end;
end;

procedure TfrmMultiRename.StringGridSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  tmpFile: TFile;
begin
  if FMoveRow and (aRow <> FSourceRow)then
    begin
      tmpFile := FFiles.Items[aRow-1];
      FFiles.Items[aRow-1] := FFiles.Items[FSourceRow-1];
      FFiles.Items[FSourceRow-1] := tmpFile;

      FSourceRow := aRow;
      StringGridTopLeftChanged(StringGrid);
    end;
end;

procedure TfrmMultiRename.StringGridTopLeftChanged(Sender: TObject);
var
  I, iRowCount: Integer;
begin
  iRowCount:= StringGrid.TopRow + StringGrid.VisibleRowCount;
  if iRowCount > FFiles.Count then iRowCount:= FFiles.Count;
  for I:= StringGrid.TopRow to iRowCount do
  begin
    StringGrid.Cells[0, I]:= FFiles[I - 1].Name;
    StringGrid.Cells[1, I]:= FreshText(I - 1);
    StringGrid.Cells[2, I]:= FFiles[I - 1].Path;
  end;
end;

procedure TfrmMultiRename.cbRegExpChange(Sender: TObject);
begin
  if cbRegExp.Checked then
    cbUseSubs.Checked:= Boolean(cbUseSubs.Tag)
  else
    begin
      cbUseSubs.Tag:= Integer(cbUseSubs.Checked);
      cbUseSubs.Checked:= False;
    end;
  cbUseSubs.Enabled:= cbRegExp.Checked;
  StringGridTopLeftChanged(StringGrid);
end;

procedure TfrmMultiRename.btnLoadPresetClick(Sender: TObject);
begin
  LoadPreset(cbPresets.Text);
end;

procedure TfrmMultiRename.btnSavePresetClick(Sender: TObject);
begin
  if cbPresets.Text <> '' then
  begin
    if FPresets.Find(cbPresets.Text) <> -1 then
    begin
      if msgYesNo(Format(rsMsgPresetAlreadyExists, [cbPresets.Text])) = False then
        Exit;
    end;

    SavePreset(cbPresets.Text);

    if cbPresets.Items.IndexOf(cbPresets.Text) = -1 then
      cbPresets.Items.Add(cbPresets.Text);
  end;
end;

procedure TfrmMultiRename.btnDeletePresetClick(Sender: TObject);
var
  Index: Integer;
begin
  if cbPresets.Text <> '' then
  begin
    DeletePreset(cbPresets.Text);

    Index := cbPresets.Items.IndexOf(cbPresets.Text);
    if Index <> -1 then
      cbPresets.Items.Delete(Index);

    cbPresets.Text := '';
  end;
end;

procedure TfrmMultiRename.edPocChange(Sender: TObject);
var
  c:integer;
begin
  c:=StrToIntDef(edPoc.Text,maxint);
  if c=MaxInt then
    with edPoc do              //editbox only for numbers
    begin
       Text:='1';
       SelectAll;
    end;
  StringGridTopLeftChanged(StringGrid);
end;

procedure TfrmMultiRename.edIntervalChange(Sender: TObject);
var
  c:integer;
begin
  c:=StrToIntDef(edInterval.Text,maxint);
  if c=MaxInt then
    with edInterval do         //editbox only for numbers
    begin
       Text:='1';
       SelectAll;
    end;
  StringGridTopLeftChanged(StringGrid);
end;

procedure TfrmMultiRename.InsertMask(const Mask:string;edChoose:Tedit);
var
  sTmp: String;
  I: Integer;
begin
  if edChoose.SelLength > 0 then
    edChoose.SelText:= Mask // Replace selected text
  else
    begin
      sTmp:= edChoose.Text;
      I:= edChoose.SelStart + 1;  // Insert on current position
      UTF8Insert(Mask, sTmp, I);
      Inc(I, UTF8Length(Mask));
      edChoose.Text:= sTmp;
      edChoose.SelStart:= I - 1;
    end;
end;

procedure TfrmMultiRename.InsertMask(const Mask:string;editNr:PtrInt);
begin
  if editNr = 0 then
    InsertMask(Mask, edName)
  else
    InsertMask(Mask, edExt);
end;

procedure TfrmMultiRename.btnRestoreClick(Sender: TObject);
begin
  edName.Text:='[N]';
  edName.SelStart:= UTF8Length(edName.Text);
  edExt.Text:='[E]';
  edExt.SelStart:= UTF8Length(edExt.Text);
  edFind.Text:='';
  edReplace.Text:='';
  cbRegExp.Checked:=False;
  cbUseSubs.Checked:=False;
  cmbNameStyle.ItemIndex:=0;
  cmbExtensionStyle.ItemIndex:=0;
  edPoc.Text:='1';
  edInterval.Text:='1';
  cmbxWidth.ItemIndex:=0;
  cbLog.Checked:=False;
  edFile.Enabled:=cbLog.Checked;
  if (FFiles.Count > 0) then
    edFile.Text:= FFiles[0].Path + 'default.log'
  else
    edFile.Text:= 'default.log';
  edFile.SelStart:= UTF8Length(edFile.Text);
  cbPresets.Text:='';
  FLastPreset:='';
end;

function TfrmMultiRename.sHandleFormatString(const sFormatStr: string; ItemNr: Integer): string;
var
  aFile: TFile;
  Counter: Integer;
begin
  Result := '';
  if Length(sFormatStr) > 0 then
  begin
    aFile := FFiles[ItemNr];
    case sFormatStr[1] of
      'N':
        begin
          Result := sReplaceXX(sFormatStr, aFile.NameNoExt);
        end;
      'E':
        begin
          Result := sReplaceXX(sFormatStr, aFile.Extension);
        end;
      'C':
        begin
          Counter := StrToIntDef(edPoc.Text, 1) +
                     StrToIntDef(edInterval.Text, 1) * ItemNr;
          Result := Format('%.' + cmbxWidth.Items[cmbxWidth.ItemIndex] + 'd', [Counter]);
        end;
      '=':
        begin
          Result:= FormatFileFunction(UTF8Copy(sFormatStr, 2, UTF8Length(sFormatStr) - 1), FFiles.Items[ItemNr], FFileSource, True);
        end;
      else
      begin
        // Assume it is date/time formatting string ([h][n][s][Y][M][D]).
        with FFiles.Items[ItemNr] do
          if fpModificationTime in SupportedProperties then
          try
            Result := SysToUTF8(FormatDateTime(sFormatStr, ModificationTime));
          except
            Result := sFormatStr;
          end;
      end;
    end;
  end;
end;

function TfrmMultiRename.sReplace(sMask: string; ItemNr: Integer): string;
var
  iStart, iEnd: Integer;
begin
  Result := '';
  while Length(sMask) > 0 do
  begin
    iStart := Pos('[', sMask);
    if iStart > 0 then
    begin
      iEnd := Pos(']', sMask);
      if iEnd > 0 then
      begin
        Result := Result + Copy(sMask, 1, iStart - 1) +
                  sHandleFormatString(Copy(sMask, iStart + 1, iEnd - iStart - 1), ItemNr);
        Delete(sMask, 1, iEnd);
      end
      else
        Break;
    end
    else
      Break;
  end;
  Result := Result + sMask;
end;

function TfrmMultiRename.sReplaceXX(const sFormatStr, sOrig: string): string;
var
  iFrom, iTo, iSemiColon: Integer;
begin
  if Length(sFormatStr) = 1 then
    Result := sOrig
  else
  begin
    iSemiColon := Pos(':', sFormatStr);
    if iSemiColon = 0 then
    begin
      iFrom := StrToIntDef(Copy(sFormatStr, 2, MaxInt), 1);
      iTo   := iFrom;
    end
    else
    begin
      iFrom := StrToIntDef(Copy(sFormatStr, 2, iSemiColon - 2), 1);
      iTo   := StrToIntDef(Copy(sFormatStr, iSemiColon + 1, MaxInt), MaxInt);
    end;
    Result := UTF8Copy(sOrig, iFrom, iTo - iFrom + 1);
  end;
end;

procedure TfrmMultiRename.btnNameMenuClick(Sender: TObject);
begin
  ppNameMenu.AutoPopup:= False;
  FillContentFieldMenu(miPlugin, @miPluginClick);
  ppNameMenu.Popup(gbMaska.Parent.Left+gbMaska.Left+
                    btnNameMenu.Left,gbMaska.Parent.Top+
                    gbMaska.Top+btnNameMenu.Top);
  ppNameMenu.Tag:= 0;
end;

procedure TfrmMultiRename.btnExtMenuClick(Sender: TObject);
begin
  ppNameMenu.AutoPopup:= False;
  FillContentFieldMenu(miPlugin, @miPluginClick);
  ppNameMenu.Popup(gbMaska.Parent.Left+gbMaska.Left+
                    btnExtMenu.Left,gbMaska.Parent.Top+
                    gbMaska.Top+btnExtMenu.Top);
  ppNameMenu.Tag:= 1;
end;

procedure TfrmMultiRename.NameClick(Sender: TObject);
begin
  InsertMask('[N]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.NameXClick(Sender: TObject);
var
  iSelStart, iSelFinish: LongInt;
begin
  if ShowSelectTextRangeDlg(Caption, FFiles[0].NameNoExt, iSelStart, iSelFinish) then
  begin
    InsertMask('[N' + IntToStr(iSelStart) + ']', ppNameMenu.Tag);
  end;
end;

procedure TfrmMultiRename.NameXXClick(Sender: TObject);
var
  iSelStart, iSelFinish: LongInt;
begin
  if ShowSelectTextRangeDlg(Caption, FFiles[0].NameNoExt, iSelStart, iSelFinish) then
  begin
    InsertMask('[N' + IntToStr(iSelStart) + ':' + IntToStr(iSelFinish) + ']', ppNameMenu.Tag);
  end;
end;

procedure TfrmMultiRename.ExtensionClick(Sender: TObject);
begin
  InsertMask('[E]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.ExtensionXClick(Sender: TObject);
var
  iSelStart, iSelFinish: LongInt;
begin
  if ShowSelectTextRangeDlg(Caption, FFiles[0].Extension, iSelStart, iSelFinish) then
  begin
    InsertMask('[E' + IntToStr(iSelStart) + ']', ppNameMenu.Tag);
  end;
end;

procedure TfrmMultiRename.ExtensionXXClick(Sender: TObject);
var
  iSelStart, iSelFinish: LongInt;
begin
  if ShowSelectTextRangeDlg(Caption, FFiles[0].Extension, iSelStart, iSelFinish) then
  begin
    InsertMask('[E' + IntToStr(iSelStart) + ':' + IntToStr(iSelFinish) + ']', ppNameMenu.Tag);
  end;
end;

procedure TfrmMultiRename.miPluginClick(Sender: TObject);
var
  sMask: String;
  MenuItem: TMenuItem absolute Sender;
begin
  case MenuItem.Tag of
    0:  begin
          sMask := '[=DC().' + MenuItem.Hint + '{}]';
        end;
    1: begin
          sMask := '[=Plugin(' + MenuItem.Parent.Caption + ').' + MenuItem.Caption + '{}]';
       end;
    2: begin
          sMask := '[=Plugin(' + MenuItem.Parent.Parent.Caption + ').' + MenuItem.Parent.Caption + '{' + MenuItem.Caption + '}]';
       end;
  end;
  InsertMask(sMask, ppNameMenu.Tag);
end;

procedure TfrmMultiRename.CounterClick(Sender: TObject);
begin
  InsertMask('[C]',ppNameMenu.Tag);
end;

procedure TfrmMultiRename.cbLogClick(Sender: TObject);
begin
  edFile.Enabled:=not edFile.Enabled;
end;

procedure TfrmMultiRename.btnRenameClick(Sender: TObject);
begin
  RenameFiles;
end;

procedure TfrmMultiRename.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMultiRename.RenameFiles;
var
  hFile: THandle;
  c: Integer;
  sNewName,
  sResult: UTF8String;
begin
  try
    if cbLog.Checked then
    begin
      if edFile.Text = EmptyStr then
        edFile.Text:= FFiles[0].Path + 'default.log';
      mbForceDirectory(ExtractFileDir(edFile.Text));

      if mbFileExists(edFile.Text) then
        begin
          hFile:= mbFileOpen(edFile.Text, fmOpenReadWrite);
          FileTruncate(hFile, 0);
        end
      else
        begin
          hFile:= mbFileCreate(edFile.Text);
        end;
    end;
    for c:= 0 to FFiles.Count - 1 do
    begin
      sResult:= FFiles[c].Name;
      sNewName:= FreshText(c);
      if RenameFile(FFileSource, FFiles[c], sNewName, True) = True then
        begin
          FFiles[c].Name := sNewName; // Write new name to the file object
          sResult := 'OK    ' + sResult + ' -> ' + sNewName;
        end
      else
        begin
          sResult := 'FAILED' + sResult + ' -> ' + sNewName;
        end;

      if cbLog.Checked then
        FileWriteLn(hFile, sResult);
    end;
  finally
    if cbLog.Checked then
      FileClose(hFile);
  end;

  StringGridTopLeftChanged(StringGrid);
end;

function TfrmMultiRename.FirstCharToUppercaseUTF8(InputString: String): String;
var
  FirstChar: String;
begin
  if UTF8Length(InputString) > 0 then
    begin
      Result := UTF8LowerCase(InputString);
      FirstChar := UTF8Copy(Result, 1, 1);
      UTF8Delete(Result, 1, 1);
      Result := UTF8UpperCase(FirstChar) + Result;
    end
  else
    Result := '';
end;

function TfrmMultiRename.FirstCharOfFirstWordToUppercaseUTF8(InputString: String): String;
var
  SeparatorPos: Integer;
begin
  InputString := UTF8LowerCase(InputString);
  Result := '';

  // Search for first letter.
  for SeparatorPos := 1 to Length(InputString) do
    if IsLetter(InputString[SeparatorPos]) then
      break;

  Result := Copy(InputString, 1, SeparatorPos - 1)
          + FirstCharToUppercaseUTF8(Copy(InputString, SeparatorPos, Length(InputString) - SeparatorPos + 1));
end;

function TfrmMultiRename.FirstCharOfEveryWordToUppercaseUTF8(InputString: String): String;
var
  SeparatorPos: Integer;
begin
  InputString := UTF8LowerCase(InputString);
  Result := '';

  while InputString <> '' do
  begin
    // Search for first non-letter (word separator).
    for SeparatorPos := 1 to Length(InputString) do
      if not IsLetter(InputString[SeparatorPos]) then
        break;

    Result := Result
            + FirstCharToUppercaseUTF8(Copy(InputString, 1, SeparatorPos));

    Delete(InputString, 1, SeparatorPos);
  end;
end;

function TfrmMultiRename.IsLetter(AChar: AnsiChar): Boolean;
begin
  Result :=  // Ascii letters
             ( (AChar < #128)
               and
               (((AChar >= 'a') and (AChar <= 'z')) or
                ((AChar >= 'A') and (AChar <= 'Z'))) )
         or
             // maybe Ansi or UTF8
             (AChar >= #128);
end;

function TfrmMultiRename.ApplyStyle(InputString: String; Style: Integer): String;
begin
  case Style of
    1: Result := UTF8UpperCase(InputString);
    2: Result := UTF8LowerCase(InputString);
    3: Result := FirstCharOfFirstWordToUppercaseUTF8(InputString);
    4: Result := FirstCharOfEveryWordToUppercaseUTF8(InputString);
    else
       Result := InputString;
  end;
end;

procedure TfrmMultiRename.LoadPresets;
begin
  if Assigned(gIni) then
    LoadPresetsIni(gIni)
  else
    LoadPresetsXml(gConfig);
end;

procedure TfrmMultiRename.LoadPresetsIni(IniFile: TIniFileEx);
var
  i: Integer;
  PresetIndex: Integer;
  PresetName: String;
  sPresetNr: String;
  PresetsCount: Integer;
begin
  ClearPresetsList;

  FLastPreset := IniFile.ReadString(sPresetsSection, 'LastPreset', '');
  PresetsCount := IniFile.ReadInteger(sPresetsSection, 'Presets', -1);

  for i := 0 to PresetsCount - 1 do
  begin
    sPresetNr := 'Preset' + IntToStr(I + 1);

    PresetName := IniFile.ReadString(sPresetsSection, sPresetNr + 'PresetName', '');
    if PresetName <> '' then
    begin
      PresetIndex := FPresets.Add(PresetName, New(PMultiRenamePreset));

      with PMultiRenamePreset(FPresets.List[PresetIndex]^.Data)^ do
      begin
        FileName := IniFile.ReadString(sPresetsSection, sPresetNr + 'Filename', '[N]');
        Extension := IniFile.ReadString(sPresetsSection, sPresetNr + 'Extension', '[E]');
        FileNameStyle := IniFile.ReadInteger(sPresetsSection, sPresetNr + 'FilenameStyle', 0);
        ExtensionStyle := IniFile.ReadInteger(sPresetsSection, sPresetNr + 'ExtensionStyle', 0);
        Find := IniFile.ReadString(sPresetsSection, sPresetNr + 'Find', '');
        Replace := IniFile.ReadString(sPresetsSection, sPresetNr + 'Replace', '');
        RegExp := IniFile.ReadBool(sPresetsSection, sPresetNr + 'RegExp', False);
        UseSubs := IniFile.ReadBool(sPresetsSection, sPresetNr + 'UseSubs', False);
        Counter := IniFile.ReadString(sPresetsSection, sPresetNr + 'Counter', '1');
        Interval := IniFile.ReadString(sPresetsSection, sPresetNr + 'Interval', '1');
        Width := IniFile.ReadInteger(sPresetsSection, sPresetNr + 'Width', 0);
        Log := IniFile.ReadBool(sPresetsSection, sPresetNr + 'Log', False);
        LogFile := IniFile.ReadString(sPresetsSection, sPresetNr + 'LogFile', '');
      end;
    end;
  end;
end;

procedure TfrmMultiRename.LoadPresetsXml(AConfig: TXmlConfig);
var
  PresetName: String;
  APreset: PMultiRenamePreset;
  ANode: TXmlNode;
begin
  ClearPresetsList;

  ANode := AConfig.FindNode(AConfig.RootNode, sPresetsSection);
  FLastPreset := AConfig.GetValue(ANode, 'LastPreset', '');

  ANode := AConfig.FindNode(ANode, 'Presets');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('Preset') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', PresetName) then
        begin
          APreset := New(PMultiRenamePreset);
          FPresets.Add(PresetName, APreset);
          with APreset^ do
          begin
            FileName := AConfig.GetValue(ANode, 'Filename', '[N]');
            Extension := AConfig.GetValue(ANode, 'Extension', '[E]');
            FileNameStyle := AConfig.GetValue(ANode, 'FilenameStyle', 0);
            ExtensionStyle := AConfig.GetValue(ANode, 'ExtensionStyle', 0);
            Find := AConfig.GetValue(ANode, 'Find', '');
            Replace := AConfig.GetValue(ANode, 'Replace', '');
            RegExp := AConfig.GetValue(ANode, 'RegExp', False);
            UseSubs := AConfig.GetValue(ANode, 'UseSubs', False);
            Counter := AConfig.GetValue(ANode, 'Counter', '1');
            Interval := AConfig.GetValue(ANode, 'Interval', '1');
            Width := AConfig.GetValue(ANode, 'Width', 0);
            Log := AConfig.GetValue(ANode, 'Log/Enabled', False);
            LogFile := AConfig.GetValue(ANode, 'Log/File', '');
          end;
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TfrmMultiRename.SavePresets;
begin
  if Assigned(gIni) then
    SavePresetsIni(gIni);
  SavePresetsXml(gConfig);
  gConfig.Save;
end;

procedure TfrmMultiRename.SavePresetsIni(IniFile: TIniFileEx);
var
  i: Integer;
  sPresetNr: String;
begin
  IniFile.EraseSection(sPresetsSection);
  IniFile.WriteString(sPresetsSection, 'LastPreset', FLastPreset);
  IniFile.WriteInteger(sPresetsSection, 'Presets', FPresets.Count);

  for i := 0 to FPresets.Count - 1 do
    with PMultiRenamePreset(FPresets.List[i]^.Data)^ do
    begin
      sPresetNr := 'Preset' + IntToStr(I + 1);

      IniFile.WriteString(sPresetsSection, sPresetNr + 'PresetName', FPresets.List[i]^.Key);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'Filename', FileName);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'Extension', Extension);
      IniFile.WriteInteger(sPresetsSection, sPresetNr + 'FilenameStyle', FileNameStyle);
      IniFile.WriteInteger(sPresetsSection, sPresetNr + 'ExtensionStyle', ExtensionStyle);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'Find', Find);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'Replace', Replace);
      IniFile.WriteBool(sPresetsSection, sPresetNr + 'RegExp', RegExp);
      IniFile.WriteBool(sPresetsSection, sPresetNr + 'UseSubs', UseSubs);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'Counter', Counter);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'Interval', Interval);
      IniFile.WriteInteger(sPresetsSection, sPresetNr + 'Width', Width);
      IniFile.WriteBool(sPresetsSection, sPresetNr + 'Log', Log);
      IniFile.WriteString(sPresetsSection, sPresetNr + 'LogFile', LogFile);
    end;
end;

procedure TfrmMultiRename.SavePresetsXml(AConfig: TXmlConfig);
var
  i: Integer;
  ANode, SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(AConfig.RootNode, sPresetsSection, True);
  AConfig.ClearNode(ANode);
  AConfig.SetValue(ANode, 'LastPreset', FLastPreset);

  ANode := AConfig.FindNode(ANode, 'Presets', True);
  for i := 0 to FPresets.Count - 1 do
  begin
    SubNode := AConfig.AddNode(ANode, 'Preset');
    with PMultiRenamePreset(FPresets.List[i]^.Data)^ do
    begin
      AConfig.AddValue(SubNode, 'Name', FPresets.List[i]^.Key);
      AConfig.AddValue(SubNode, 'Filename', FileName);
      AConfig.AddValue(SubNode, 'Extension', Extension);
      AConfig.AddValue(SubNode, 'FilenameStyle', FileNameStyle);
      AConfig.AddValue(SubNode, 'ExtensionStyle', ExtensionStyle);
      AConfig.AddValue(SubNode, 'Find', Find);
      AConfig.AddValue(SubNode, 'Replace', Replace);
      AConfig.AddValue(SubNode, 'RegExp', RegExp);
      AConfig.AddValue(SubNode, 'UseSubs', UseSubs);
      AConfig.AddValue(SubNode, 'Counter', Counter);
      AConfig.AddValue(SubNode, 'Interval', Interval);
      AConfig.AddValue(SubNode, 'Width', Width);
      AConfig.SetValue(SubNode, 'Log/Enabled', Log);
      AConfig.SetValue(SubNode, 'Log/File', LogFile);
    end;
  end;
end;

procedure TfrmMultiRename.LoadPreset(PresetName: String);
var
  PresetIndex: Integer;
begin
  if PresetName <> '' then
  begin
    PresetIndex := FPresets.Find(PresetName);
    if PresetIndex = -1 then
      PresetIndex := FPresets.Add(PresetName, New(PMultiRenamePreset));

    with PMultiRenamePreset(FPresets.List[PresetIndex]^.Data)^ do
    begin
      edName.Text := FileName;
      edExt.Text := Extension;
      cmbNameStyle.ItemIndex := FileNameStyle;
      cmbExtensionStyle.ItemIndex := ExtensionStyle;
      edFind.Text := Find;
      edReplace.Text := Replace;
      cbRegExp.Checked := RegExp;
      cbUseSubs.Checked := UseSubs;
      edPoc.Text := Counter;
      edInterval.Text := Interval;
      cmbxWidth.ItemIndex := Width;
      cbLog.Checked := Log;
      edFile.Text := LogFile;
    end;

    FLastPreset := PresetName;

    StringGridTopLeftChanged(StringGrid);
  end;
end;

procedure TfrmMultiRename.SavePreset(PresetName: String);
var
  PresetIndex: Integer;
begin
  if PresetName <> '' then
  begin
    PresetIndex := FPresets.Find(PresetName);
    if PresetIndex = -1 then
      PresetIndex := FPresets.Add(PresetName, New(PMultiRenamePreset));

    with PMultiRenamePreset(FPresets.List[PresetIndex]^.Data)^ do
    begin
      FileName := edName.Text;
      Extension := edExt.Text;
      FileNameStyle := cmbNameStyle.ItemIndex;
      ExtensionStyle := cmbExtensionStyle.ItemIndex;
      Find := edFind.Text;
      Replace := edReplace.Text;
      RegExp := cbRegExp.Checked;
      UseSubs := cbUseSubs.Checked;
      Counter := edPoc.Text;
      Interval := edInterval.Text;
      Width := cmbxWidth.ItemIndex;
      Log := cbLog.Checked;
      LogFile := edFile.Text;
    end;

    FLastPreset := PresetName;
    SavePresets;
  end;
end;

procedure TfrmMultiRename.DeletePreset(PresetName: String);
var
  PresetIndex: Integer;
begin
  if PresetName <> '' then
  begin
    PresetIndex := FPresets.Find(PresetName);
    if PresetIndex <> -1 then
    begin
      Dispose(PMultiRenamePreset(FPresets.List[PresetIndex]^.Data));
      FPresets.Remove(PresetName);
      FLastPreset := '';
      SavePresets;
    end;
  end;
end;

procedure TfrmMultiRename.FillPresetsList;
var
  i: Integer;
  PresetName: String;
begin
  cbPresets.Clear;

  for i := 0 to FPresets.Count - 1 do
  begin
    PresetName := FPresets.List[i]^.Key;
    if cbPresets.Items.IndexOf(PresetName) = -1 then
      cbPresets.Items.Add(PresetName);
  end;
end;

procedure TfrmMultiRename.ClearPresetsList;
var
  i: Integer;
begin
  for i := 0 to FPresets.Count - 1 do
    Dispose(PMultiRenamePreset(FPresets.List[i]^.Data));
  FPresets.Clear;
end;

// Temporary for switching configuration from INI to XML
procedure TfrmMultiRename.PublicSavePresets;
begin
  SavePresetsXml(gConfig);
end;

end.

