{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : Pavel Letko (letcuv@centrum.cz)

   Advanced multi rename tool

   contributors:

   Copyright (C) 2007-2008  Koblov Alexander (Alexx2000@mail.ru)
}

unit fMultiRename;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, Buttons, SynRegExpr;

type

  { TfrmMultiRename }

  TfrmMultiRename = class(TForm)
    cbRegExp: TCheckBox;
    cbUseSubs: TCheckBox;
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
    gbFontStyle: TGroupBox;
    cmbxFont: TComboBox;
    gbCounter: TGroupBox;
    lbStNb: TLabel;
    lbInterval: TLabel;
    lbWidth: TLabel;
    edPoc: TEdit;
    edInterval: TEdit;
    cmbxWidth: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
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
    procedure cbRegExpChange(Sender: TObject);
    procedure cmbxFontChange(Sender: TObject);
    procedure edPocChange(Sender: TObject);
    procedure edIntervalChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnNameMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    {Function sReplace call sReplaceXX with parametres}
    function sReplace(sMask:string;count:integer):string;
    {sReplaceXX doing Nx,Nx:x and Ex,Ex:x}
    function sReplaceXX(sMask,sSymbol,sOrig:string):string;
    {InsertMask is for write key symbols from buttons}
    procedure InsertMask(Mask:string;edChoose:Tedit);
    {Main function for write into lsvwFile}
    procedure FreshText;
  public
    { Public declarations }
  end;

{initialization function}
  function ShowMultiRenameForm(Var lsInFiles: TStringList):Boolean;

implementation

uses
  uLng, uFileProcs, uOSUtils;

function ShowMultiRenameForm(Var lsInFiles: TStringList):Boolean;
var
  c:integer;
begin
  Result:= False;
  With TfrmMultiRename.Create(Application) do
  begin
    try
      for c:=0 to lsInFiles.Count-1 do
      with lsvwFile.Items do
      begin
        Add;
        Item[c].Caption:=ExtractFileName(lsInFiles[c]);
        item[c].SubItems.Add('');
        item[c].SubItems.Add(ExtractFileDir(lsInFiles[c]));
      end;
      btnRestoreClick(nil);
      Result:= (ShowModal = mrOK);
    finally
      Free;
    end;
  end;
end;

procedure TfrmMultiRename.FormCreate(Sender: TObject);
var
  I: Integer;
  s: String;
  xPos: Integer;
begin
  // Localize File name style ComboBox
  s:= rsMulRenFileNameStyleList;
  for I:= 0 to cmbxFont.Items.Count - 1 do
  begin
    xPos:= Pos(';',s);
    cmbxFont.Items[I]:= Copy(s,1,xPos - 1);
    Delete(s,1,xPos);
  end;
end;

procedure TfrmMultiRename.FreshText;
var
  c:integer;
  sTmpAll,sTmpName,sTmpExt:string;
begin
  for c:=0 to lsvwFile.Items.Count-1 do
  begin
    //use mask
    sTmpName:=sReplace(edName.Text,c);
    sTmpExt:=sReplace(edExt.Text,c);
    //join
    sTmpAll:=sTmpName+'.'+sTmpExt;
    //find and replace
    if cbRegExp.Checked and (edFind.Text <> '') then
      sTmpAll:= ReplaceRegExpr(edFind.Text, sTmpAll, edReplace.Text, cbUseSubs.Checked)
    else
      sTmpAll:=StringReplace(sTmpAll,edFind.Text,edReplace.Text,[rfReplaceAll,rfIgnoreCase]);
    //file name style
    case cmbxFont.ItemIndex of
      1: sTmpAll:=UpperCase(sTmpAll);
      2: sTmpAll:=LowerCase(sTmpAll);
      3: begin
           sTmpAll:=LowerCase(sTmpAll);
           if length(sTmpAll)>0 then
             sTmpAll[1]:=UpCase(stmpall[1]);
         end;
    end;
    //save new name file
    lsvwFile.Items[c].SubItems.Strings[0]:=sTmpAll;
  end;
end;

procedure TfrmMultiRename.cmbxFontChange(Sender: TObject);
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
  cmbxFont.ItemIndex:=0;
  edPoc.Text:='1';
  edInterval.Text:='1';
  cmbxWidth.ItemIndex:=0;
  cbLog.Checked:=False;
  edFile.Enabled:=cbLog.Checked;
  edFile.Text:=IncludeTrailingBackslash(lsvwFile.Items.Item[0].SubItems[1])+'default.log';
  edFile.SelStart:=length(edFile.Text);
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
  sNew:=StringReplace(sMask,'[E]',
        sOrigExt,[rfReplaceAll,rfIgnoreCase]);

{
//type [H][Mi][S][R][Me][D]
  sNew:=StringReplace(sNew,'[H..D]'-what symbol,
        -which replace,[rfReplaceAll,rfIgnoreCase]);
}

//type [N]
  sNew:=StringReplace(sNew,'[N]',
        sOrigName,[rfReplaceAll,rfIgnoreCase]);
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
  Result:=sNew;
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

procedure TfrmMultiRename.btnOKClick(Sender: TObject);
var
  hFile: Integer;
  c:integer;
begin
  try
    if cbLog.Checked then
    begin
      if edFile.Text='' then
        edFile.Text:=lsvwFile.Items.Item[0].SubItems[1]+ PathDelim+'default.log';
      ForceDirectory(ExtractFileDir(edFile.Text));

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
        mbRenameFile(Item[c].SubItems[1]+pathDelim+item[c].Caption,
            Item[c].SubItems[1]+pathdelim+Item[c].SubItems[0]);
        if cbLog.Checked then
          FileWriteLn(hFile,item[c].Caption+' -> '+Item[c].SubItems[0]);
      end;
  finally
    if cbLog.Checked then
      FileClose(hFile);
  end;
  ModalResult:=mrOK;
end;

initialization
 {$I fmultirename.lrs}
end.

