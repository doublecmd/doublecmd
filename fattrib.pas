unit fAttrib;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, uFileList, Buttons;

type
  TfrmAttrib = class(TfrmLng)
    lblFile: TLabel;
    lblFileName: TLabel;
    cbReadOwner: TCheckBox;
    cbWriteOwner: TCheckBox;
    cbExecOwner: TCheckBox;
    lblOwner: TLabel;
    lblGroup: TLabel;
    cbReadGroup: TCheckBox;
    cbWriteGroup: TCheckBox;
    cbExecGroup: TCheckBox;
    lblOther: TLabel;
    cbReadOther: TCheckBox;
    cbWriteOther: TCheckBox;
    cbExecOther: TCheckBox;
    lblRead: TLabel;
    lblWrite: TLabel;
    lblExec: TLabel;
    btnOK: TBitBtn;
    btnAll: TBitBtn;
    btnSkip: TBitBtn;
    btnCancel: TBitBtn;
    lblTextAttr: TLabel;
    lblAttr: TLabel;
    cbSuid: TCheckBox;
    cbSgid: TCheckBox;
    cbSticky: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
  private
    { Private declarations }
    iCurrent:Integer;
    ffileList:TFileList;
  public
    { Public declarations }
    Path:String;

    procedure LoadLng; override;
    procedure ShowFile(iIndex:Integer);
    procedure StoreData(FileList:TFileList);
    function FindNextSelected:Boolean;
    procedure ShowAttr(iMode:Integer);
    function GetModeFromForm:Integer;
    procedure ChangeMod;
  end;

  procedure ShowAttrForm(FileList:TFileList; const aPath:String);


implementation

uses
  uLng, uFileOp, BaseUnix;

procedure ShowAttrForm(FileList:TFileList; const aPath:String);
begin
  with TfrmAttrib.Create(Application) do
  begin
    try
      Path:=aPath;
      StoreData(FileList);
      if FindNextSelected then
      begin
        ShowFile(iCurrent);
        ShowModal;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmAttrib.ShowFile(iIndex:Integer);
begin
  with ffileList.GetItem(iIndex)^ do
  begin
    lblFileName.Caption:=sName;
    ShowAttr(iMode);
  end;
end;

procedure TfrmAttrib.StoreData(FileList:TFileList);
begin
  fFileList:=FileList;
  iCurrent:=0;
end;

function TfrmAttrib.FindNextSelected:Boolean;
var
  i:Integer;
begin
  for i:=iCurrent to FFileList.Count-1 do
  begin
    if FFileList.GetItem(i)^.bSelected then
    begin
      iCurrent:=i;
      Result:=True;
      Exit;
    end;
  end;
  Result:=False;
end;

procedure TfrmAttrib.ShowAttr(iMode:Integer);
begin
  cbReadOwner.Checked:= ((iMode AND S_IRUSR) = S_IRUSR);
  cbWriteOwner.Checked:= ((iMode AND S_IWUSR) = S_IWUSR);
  cbExecOwner.Checked:= ((iMode AND S_IXUSR) = S_IXUSR);
  cbReadGroup.Checked:= ((iMode AND S_IRGRP) = S_IRGRP);
  cbWriteGroup.Checked:= ((iMode AND S_IWGRP) = S_IWGRP);
  cbExecGroup.Checked:= ((iMode AND S_IXGRP) = S_IXGRP);
  cbReadOther.Checked:= ((iMode AND S_IROTH) = S_IROTH);
  cbWriteOther.Checked:= ((iMode AND S_IWOTH) = S_IWOTH);
  cbExecOther.Checked:= ((iMode AND S_IXOTH) = S_IXOTH);

  cbSuid.Checked:= ((iMode AND S_ISUID) = S_ISUID);
  cbSgid.Checked:= ((iMode AND S_ISGID) = S_ISGID);
  cbSticky.Checked:= ((iMode AND S_ISVTX) = S_ISVTX);

  lblAttr.Caption:=AttrToStr(iMode);
end;

procedure TfrmAttrib.LoadLng;
begin
// load strings
  Caption:=lngGetString(clngAttrChmod);
  
  lblFile.Caption:=lngGetString(clngLinkColumnNameFile);
  lblOwner.Caption:=lngGetString(clngAttrOwner);
  lblGroup.Caption:=lngGetString(clngAttrGroup);
  lblOther.Caption:=lngGetString(clngAttrOther);
  lblRead.Caption:=lngGetString(clngAttrRead);
  lblWrite.Caption:=lngGetString(clngAttrWrite);
  lblExec.Caption:=lngGetString(clngAttrExec);
  lblTextAttr.Caption:=lngGetString(clngAttrTextRep);

  btnCancel.Caption:=lngGetString(clngbutCancel);
  btnSkip.Caption:=lngGetString(clngbutSkip);
  btnAll.Caption:=lngGetString(clngbutAll);
end;

procedure TfrmAttrib.ChangeMod;
begin
  fpchmod(PChar(Path+ffileList.GetItem(iCurrent)^.sName),GetModeFromForm);
end;

procedure TfrmAttrib.btnOKClick(Sender: TObject);
begin
  ChangeMod;
  inc (iCurrent);
  if not FindNextSelected Then
    Close
  else
    ShowFile(iCurrent);
end;

function TfrmAttrib.GetModeFromForm:Integer;
begin
  Result:=0;
  if cbReadOwner.Checked then Result:=(Result OR S_IRUSR);
  if cbWriteOwner.Checked then Result:=(Result OR S_IWUSR);
  if cbExecOwner.Checked then Result:=(Result OR S_IXUSR);
  if cbReadGroup.Checked then Result:=(Result OR S_IRGRP);
  if cbWriteGroup.Checked then Result:=(Result OR S_IWGRP);
  if cbExecGroup.Checked then Result:=(Result OR S_IXGRP);
  if cbReadOther.Checked then Result:=(Result OR S_IROTH);
  if cbWriteOther.Checked then Result:=(Result OR S_IWOTH);
  if cbExecOther.Checked then Result:=(Result OR S_IXOTH);

  if cbSuid.Checked then Result:=(Result OR S_ISUID);
  if cbSgid.Checked then Result:=(Result OR S_ISGID);
  if cbSticky.Checked then Result:=(Result OR S_ISVTX);
end;

procedure TfrmAttrib.btnAllClick(Sender: TObject);
begin
  inherited;
  repeat
    ChangeMod;
    inc (iCurrent);
  until not FindNextSelected;
  Close;
end;

procedure TfrmAttrib.btnSkipClick(Sender: TObject);
begin
  inherited;
  inc (iCurrent);  
  if not FindNextSelected Then
    Close
  else
    ShowFile(iCurrent);
end;

initialization
 {$I fAttrib.lrs}

end.
