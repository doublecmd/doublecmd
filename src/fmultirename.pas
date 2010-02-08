{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : Pavel Letko (letcuv@centrum.cz)

   Advanced multi rename tool

   contributors:

   Copyright (C) 2007-2009  Koblov Alexander (Alexx2000@mail.ru)
}

unit fMultiRename;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, Buttons, SynRegExpr, LCLType,
  uClassesEx, uFile, uFileSource, StringHashList, uXmlConfig;

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
    gbPresets: TGroupBox;
    lsvwFile: TListView;
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
    procedure edPocChange(Sender: TObject);
    procedure edIntervalChange(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnNameMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure miDayClick(Sender: TObject);
    procedure miHourClick(Sender: TObject);
    procedure miMinuteClick(Sender: TObject);
    procedure miMonthClick(Sender: TObject);
    procedure miSecondClick(Sender: TObject);
    procedure miYearClick(Sender: TObject);
    procedure NameClick(Sender: TObject);
    procedure NameXClick(Sender: TObject);
    procedure NameXXClick(Sender: TObject);
    procedure ExtensionClick(Sender: TObject);
    procedure CounterClick(Sender: TObject);
    procedure btnExtMenuClick(Sender: TObject);
    procedure cbLogClick(Sender: TObject);
    procedure ExtensionXClick(Sender: TObject);
    procedure ExtensionXXClick(Sender: TObject);
    procedure ppNameMenuPopup(Sender: TObject);
 private
    IniPropStorage: TIniPropStorageEx;
    FLastPreset: String;
    FFileSource: IFileSource;
    FFiles: TFiles;
    FPresets: TStringHashList; // of PMultiRenamePreset

    {Function sReplace call sReplaceXX with parametres}
    function sReplace(sMask:string;count:integer):string;
    {sReplaceXX doing Nx,Nx:x and Ex,Ex:x}
    function sReplaceXX(sMask,sSymbol,sOrig:string):string;
    {sReplaceDateTime doing Y,M,D and h,m,s}
    function sReplaceDateTime(sMask: String; dtDateTime: TDateTime): String;
    {InsertMask is for write key symbols from buttons}
    procedure InsertMask(Mask:string;edChoose:Tedit);
    {Main function for write into lsvwFile}
    procedure FreshText;
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

uses
  LCLProc, FileUtil, uLng, uGlobs, uFileProcs, uDCUtils, uOSUtils, uShowMsg, uFileSourceUtil, uFileProperty;

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
  inherited Create(TheOwner);
end;

destructor TfrmMultiRename.Destroy;
begin
  inherited;
  ClearPresetsList;
  FreeAndNil(FPresets);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
end;

procedure TfrmMultiRename.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Localize File name style ComboBox
  ParseLineToList(rsMulRenFileNameStyleList, cmbNameStyle.Items);
  ParseLineToList(rsMulRenFileNameStyleList, cmbExtensionStyle.Items);

  // Initialize property storage
  IniPropStorage:= InitPropStorage(Self);
  IniPropStorage.StoredValues.Add.DisplayName:= 'lsvwFile_Columns.Item0_Width';
  IniPropStorage.StoredValues.Add.DisplayName:= 'lsvwFile_Columns.Item1_Width';
  IniPropStorage.StoredValues.Add.DisplayName:= 'lsvwFile_Columns.Item2_Width';

  // Fill the files list.
  for i := 0 to FFiles.Count - 1 do
  with lsvwFile.Items do
  begin
    Add;
    Item[i].Data:= FFiles.Items[i];
    Item[i].Caption := FFiles.Items[i].Name;
    Item[i].SubItems.Add('');
    Item[i].SubItems.Add(ExcludeTrailingBackslash(FFiles.Items[i].Path));
  end;

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
  with lsvwFile.Columns do
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
  with lsvwFile.Columns do
  begin
    IniPropStorage.StoredValue['lsvwFile_Columns.Item0_Width']:= IntToStr(Items[0].Width);
    IniPropStorage.StoredValue['lsvwFile_Columns.Item1_Width']:= IntToStr(Items[1].Width);
    IniPropStorage.StoredValue['lsvwFile_Columns.Item2_Width']:= IntToStr(Items[2].Width);
  end;
end;

procedure TfrmMultiRename.miDayClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[D]',edName)
  else
    InsertMask('[D]',edExt);
end;

procedure TfrmMultiRename.miHourClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[h]',edName)
  else
    InsertMask('[h]',edExt);
end;

procedure TfrmMultiRename.miMinuteClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[n]',edName)
  else
    InsertMask('[n]',edExt);
end;

procedure TfrmMultiRename.miMonthClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[M]',edName)
  else
    InsertMask('[M]',edExt);
end;

procedure TfrmMultiRename.miSecondClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[s]',edName)
  else
    InsertMask('[s]',edExt);
end;

procedure TfrmMultiRename.miYearClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[Y]',edName)
  else
    InsertMask('[Y]',edExt);
end;

procedure TfrmMultiRename.FreshText;
var
  bError: Boolean;
  c:integer;
  sTmpAll,sTmpName,sTmpExt:string;
begin
  bError:= False;
  for c:=0 to lsvwFile.Items.Count-1 do
  begin
    //use mask
    sTmpName:=sReplace(edName.Text,c);
    sTmpExt:=sReplace(edExt.Text,c);

    //join
    sTmpAll := sTmpName;
    if sTmpExt <> '' then
      sTmpAll := sTmpAll + '.' + sTmpExt;

    //find and replace
    if cbRegExp.Checked and (edFind.Text <> '') then
      try
        sTmpAll:= ReplaceRegExpr(edFind.Text, sTmpAll, edReplace.Text, cbUseSubs.Checked);
      except
        sTmpAll:= rsMsgErrRegExpSyntax;
        bError:= True;
      end
    else
      sTmpAll:=StringReplace(sTmpAll,edFind.Text,edReplace.Text,[rfReplaceAll,rfIgnoreCase]);

    //file name style
    sTmpName := ExtractOnlyFileName(sTmpAll);
    sTmpExt  := ExtractFileExt(sTmpAll);

    sTmpName := ApplyStyle(sTmpName, cmbNameStyle.ItemIndex);
    sTmpExt  := ApplyStyle(sTmpExt, cmbExtensionStyle.ItemIndex);

    sTmpAll := sTmpName + sTmpExt;

    //save new name file
    lsvwFile.Items[c].SubItems.Strings[0]:=sTmpAll;
  end;

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
  FreshText;
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
  FreshText;
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
  FreshText;
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
  FreshText;
end;

procedure TfrmMultiRename.InsertMask(Mask:string;edChoose:Tedit);
var
  sTmp:string;
  i:integer;
begin
  if edChoose.SelLength>0 then
    edChoose.Text:='';  //selected text clear
  sTmp:=edChoose.Text;
  i:=edChoose.SelStart+2;    //insert on current position
  System.Insert(Mask,sTmp,i);
  inc(i);
  edChoose.Text:=sTmp;
  edChoose.SelStart:=i;
end;

procedure TfrmMultiRename.btnRestoreClick(Sender: TObject);
begin
  edName.Text:='[N]';
  edName.SelStart:=length(edName.Text);
  edExt.Text:='[E]';
  edExt.SelStart:=length(edExt.Text);
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
  if (lsvwFile.Items.Count > 0) and (lsvwFile.Items[0].SubItems.Count > 1) then
    edFile.Text:=IncludeTrailingBackslash(lsvwFile.Items.Item[0].SubItems[1])+'default.log'
  else
    edFile.Text:='default.log';
  edFile.SelStart:=length(edFile.Text);
  cbPresets.Text:='';
  FLastPreset:='';
end;

function TfrmMultiRename.sReplace(sMask:string;count:integer):string;
var
  sNew,sTmp,sOrigName,sOrigExt:string;
  i:integer;
begin
  sOrigName:=ChangeFileExt(lsvwFile.Items[count].Caption,'');
  sOrigExt:=ExtractFileExt(lsvwFile.Items[count].Caption);
  delete(sOrigExt,1,1);
//type [E]
  sNew:=StringReplace(sMask,'[E]', sOrigExt,[rfReplaceAll]);
//type [N]
  sNew:=StringReplace(sNew,'[N]', sOrigName,[rfReplaceAll]);
//type [C]
  i:=StrToInt(edPoc.Text)+StrToInt(edInterval.Text)*count;
  sTmp:=format('%.'+
    cmbxWidth.Items[cmbxWidth.ItemIndex]+'d',[i]);
  sNew:=StringReplace(sNew,'[C]',
        sTmp,[rfReplaceAll,rfIgnoreCase]);
//type[Nxx]
  sNew:=sReplaceXX(sNew,'[N',sOrigName);
//type[Exx]
  sNew:=sReplaceXX(sNew,'[E',sOrigExt);
//type [h][n][s][Y][M][D]
  with FFiles.Items[count] do
  if (fpModificationTime in GetSupportedProperties) then
    sNew:= sReplaceDateTime(sNew, (Properties[fpModificationTime] as TFileDateTimeProperty).Value);
  Result:= sNew;
end;

function TfrmMultiRename.sReplaceXX(sMask,sSymbol,sOrig:string):string;
var
  p:array [0..2] of integer;
  sTmp,sTmp2:string;
  c,c1:integer;
Begin
  while Pos(sSymbol,UpperCase(sMask))>0 do
  begin
    p[0]:=Pos(sSymbol,UpperCase(sMask));
    p[1]:=Pos(':',sMask);
    p[2]:=Pos(']',sMask);
//incorect type
    if (p[2]=0)or(p[0]>p[2]) then
      break;
//type [Symbolx]
    if (p[1]=0)or(p[1]>p[2])or(p[1]<p[0]) then
    begin
      sTmp:=copy(sMask,p[0]+2,p[2]-(p[0]+2));
      c:=StrToIntDef(sTmp,0);
      if (c<1) then
        break;
      if (c<=length(sOrig)) then
        sMask:=StringReplace(sMask,copy(sMask,p[0],(p[2]+1)-(p[0])),
            sOrig[c],[rfIgnoreCase])
      else
        sMask:=StringReplace(sMask,copy(sMask,p[0],(p[2]+1)-(p[0])),
            '',[rfIgnoreCase]);
    end
//type [Symbolx:x]
    else
    begin
      sTmp:=copy(sMask,p[0]+2,p[1]-(p[0]+2));
      sTmp2:=copy(sMask,p[1]+1,p[2]-(p[1]+1));
      c:=StrToIntDef(sTmp,0);
      c1:=StrToIntDef(sTmp2,0);
      if (c>c1)or(c<1) then
        break;
      if (c1>length(sOrig))then
        c1:=length(sOrig);
      sMask:=StringReplace(sMask,copy(sMask,p[0],(p[2]+1)-(p[0])),
      copy(sOrig,c,(c1+1)-c),[rfIgnoreCase]);
    end;
  end;
  result:=sMask;
end;

function TfrmMultiRename.sReplaceDateTime(sMask: String; dtDateTime: TDateTime): String;
var
  iStart,
  iEnd: Integer;
  sTmp: String;
begin
  Result:= sMask;
  repeat
    iStart:= Pos('[', Result);
    iEnd:= Pos(']', Result);
    if (iStart = 0) or (iEnd = 0) then Exit;
    sTmp:= Copy(Result, iStart+1, iEnd-iStart-1);
    sTmp:= SysToUTF8(FormatDateTime(sTmp, dtDateTime));
    Delete(Result, iStart, iEnd-iStart+1);
    Insert(sTmp, Result, iStart);
  until False;
end;

procedure TfrmMultiRename.btnNameMenuClick(Sender: TObject);
begin
  ppNameMenu.AutoPopup:=false;
  ppNameMenu.Popup(gbMaska.Parent.Left+gbMaska.Left+
                    btnNameMenu.Left,gbMaska.Parent.Top+
                    gbMaska.Top+btnNameMenu.Top);
  ppNameMenu.Tag:=0;
end;

procedure TfrmMultiRename.btnExtMenuClick(Sender: TObject);
begin
  ppNameMenu.AutoPopup:=false;
  ppNameMenu.Popup(gbMaska.Parent.Left+gbMaska.Left+
                    btnExtMenu.Left,gbMaska.Parent.Top+
                    gbMaska.Top+btnExtMenu.Top);
  ppNameMenu.Tag:=1;
end;

procedure TfrmMultiRename.NameClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[N]',edName)
  else
    InsertMask('[N]',edExt);
end;

procedure TfrmMultiRename.NameXClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[N1]',edName)
  else
    InsertMask('[N1]',edExt);
end;

procedure TfrmMultiRename.NameXXClick(Sender: TObject);
var
  c,i:integer;
begin
  i:=0;
  for c:=0 to lsvwFile.Items.Count-1 do
    if i<length(ChangeFileExt(lsvwFile.Items[c].Caption,'')) then
      i:=length(ChangeFileExt(lsvwFile.Items[c].Caption,''));
  if ppNameMenu.Tag=0 then
    InsertMask('[N1:'+inttostr(i)+']',edName)
  else
    InsertMask('[N1:'+inttostr(i)+']',edExt);
end;

procedure TfrmMultiRename.ExtensionClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[E]',edName)
  else
    InsertMask('[E]',edExt);
end;

procedure TfrmMultiRename.ExtensionXClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[E1]',edName)
  else
    InsertMask('[E1]',edExt);
end;

procedure TfrmMultiRename.ExtensionXXClick(Sender: TObject);
var
  c,i:integer;
  sTmp:string;
begin
  i:=0;
  for c:=0 to lsvwFile.Items.Count-1 do
  begin
    sTmp:=ExtractFileExt(lsvwFile.Items[c].Caption);
    delete(sTmp,1,1);
    if i<length(sTmp) then
      i:=length(sTmp);
  end;
  if ppNameMenu.Tag=0 then
    InsertMask('[E1:'+inttostr(i)+']',edName)
  else
    InsertMask('[E1:'+inttostr(i)+']',edExt);
end;

procedure TfrmMultiRename.ppNameMenuPopup(Sender: TObject);
begin

end;

procedure TfrmMultiRename.CounterClick(Sender: TObject);
begin
  if ppNameMenu.Tag=0 then
    InsertMask('[C]',edName)
  else
    InsertMask('[C]',edExt);
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
  hFile: Integer;
  c: Integer;
  sResult: String;
begin
  try
    if cbLog.Checked then
    begin
      if edFile.Text='' then
        edFile.Text:=lsvwFile.Items.Item[0].SubItems[1]+ PathDelim+'default.log';
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
    for c:=0 to lsvwFile.Items.Count-1 do
      with lsvwFile.Items do
      begin
        if RenameFile(FFileSource, TFile(Item[c].Data), Item[c].SubItems[0], True) = True then
        begin
          Item[c].Caption          := Item[c].SubItems[0]; // write the new name to table
          TFile(Item[c].Data).Name := Item[c].SubItems[0]; // and to the file object
          sResult := 'OK    ';
        end
        else
        begin
          sResult := 'FAILED';
        end;

        if cbLog.Checked then
          FileWriteLn(hFile, sResult + ' ' + item[c].Caption+' -> '+Item[c].SubItems[0]);
      end;
  finally
    if cbLog.Checked then
      FileClose(hFile);
  end;

  FreshText;
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
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
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

    FreshText;
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

initialization
 {$I fmultirename.lrs}
end.

