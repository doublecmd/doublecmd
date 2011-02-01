{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : Pavel Letko (letcuv@centrum.cz)

File split

contributors:
  Radek Cervinka
}

unit fSplitter;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Buttons;

type

  { TfrmSplitter }

  TfrmSplitter = class(TForm)
    teNumberParts: TEdit;
    grbxFile: TGroupBox;
    edFileSource: TEdit;
    lblNumberParts: TLabel;
    lbFileSource: TLabel;
    edDirTarget: TEdit;
    lbDirTarget: TLabel;
    btnFTChoice: TButton;
    grbxSize: TGroupBox;
    cmbxSize: TComboBox;
    grbxWatch: TGroupBox;
    memWatch: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    prgbrDoIt: TProgressBar;
    rbtnKiloB: TRadioButton;
    rbtnMegaB: TRadioButton;
    rbtnGigaB: TRadioButton;
    procedure btnFTChoiceClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cmbxSizeCloseUp(Sender: TObject);
    procedure cmbxSizeKeyPress(Sender: TObject; var Key: char);
    procedure cmbxSizeKeyUp(Sender: TObject; var Key: char; Shift: TShiftState);
    procedure rbtnKiloBChange(Sender: TObject);
    procedure SetNumberOfPart;
    procedure SetSizeOfPart;
    procedure teNumberPartsKeyPress(Sender: TObject; var Key: char);
    procedure teNumberPartsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function StrConvert(str:string):int64;
    //test for correct file size format;
  public
    { Public declarations }

  end;

  function ShowSplitterFileForm(const sFile: TStringList; const sTargetDir: String): Boolean;


implementation

{$R *.lfm}

uses
  LCLProc, uLng, uClassesEx, uOSUtils;

function ShowSplitterFileForm(const sFile: TStringList; const sTargetDir: String): Boolean;
begin
  with TfrmSplitter.Create(Application) do
  begin
    try
      edFileSource.Text:= sFile[0];
      edDirTarget.Text:= ExtractFileDir(sTargetDir);
      SetNumberOfPart;
      rbtnKiloB.Enabled:=false;
      rbtnMegaB.Enabled:=false;
      rbtnGigaB.Enabled:=false;
      Result:= (ShowModal = mrOK);
    finally
      Free;
    end;
  end;
end;

procedure TfrmSplitter.SetNumberOfPart;
begin
  if  not (cmbxSize.Text='') then
    begin
      if  mbFileSize(edFileSource.Text) mod StrConvert(cmbxSize.Text)>0 then
        teNumberParts.Text:= IntToStr(mbFileSize(edFileSource.Text)div StrConvert(cmbxSize.Text)+1)
      else
        teNumberParts.Text:= IntToStr(mbFileSize(edFileSource.Text)div StrConvert(cmbxSize.Text));
    end;
end;

procedure TfrmSplitter.SetSizeOfPart;
begin
  if not (teNumberParts.Text='') then
    begin
      if mbFileSize(edFileSource.Text) mod StrToInt64Def(teNumberParts.Text,0)>0 then
        cmbxSize.Text := IntToStr(mbFileSize(edFileSource.Text) div StrToInt64Def(teNumberParts.Text,0)+1)+'B'
      else
        cmbxSize.Text := IntToStr(mbFileSize(edFileSource.Text) div StrToInt64Def(teNumberParts.Text,0))+'B';
    end;
end;

procedure TfrmSplitter.teNumberPartsKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'.. '9', #8, #46]) then
    begin
      Key := #0;
      Exit;
    end;
end;

procedure TfrmSplitter.teNumberPartsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SetSizeOfPart;
end;

procedure TfrmSplitter.btnFTChoiceClick(Sender: TObject);
var
  sDir: string;
begin
  if SelectDirectory(rsSplitSelDir, '', sDir) then
  // Select directory:
  // must change on linux!!!
  begin
    edDirTarget.Text:= sDir;
  end;
end;

function TfrmSplitter.StrConvert(str:string):int64;
var iRet:int64;
    iPos,iMult:integer;
    sStr:string;
begin
  str:=UpperCase(str);
  iPos:=Pos('B',str);
  if iPos>1 then
  begin
    rbtnKiloB.Enabled:=false;
    rbtnMegaB.Enabled:=false;
    rbtnGigaB.Enabled:=false;
    rbtnKiloB.Checked:=false;
    rbtnMegaB.Checked:=false;
    rbtnGigaB.Checked:=false;
    dec(iPos);
    case str[iPos] of
      'K':iMult:=1024;       //Kilo
      'M':iMult:=1024*1024;    //Mega
      'G':iMult:=1024*1024*1024;//Giga
    else
      iMult:=1;
      inc(iPos);
    end;
    dec(iPos);
    sStr:=Copy(str,1,iPos);
    iRet:=StrToInt64Def(sStr,0)*iMult;
  end
  else
  begin
    rbtnKiloB.Enabled:=true;
    rbtnMegaB.Enabled:=true;
    rbtnGigaB.Enabled:=true;
    iMult:=1;
    if rbtnKiloB.Checked then iMult:=1024;          //Kilo
    if rbtnMegaB.Checked then iMult:=1024*1024;     //Mega
    if rbtnGigaB.Checked then iMult:=1024*1024*1024;//Giga
    iRet:=StrToInt64Def(Str,0)*iMult;
  end;
  Result:=iRet;
end;


procedure TfrmSplitter.btnOKClick(Sender: TObject);
var
  iFileSize:int64;
  i,num:integer;
  fSource: TFileStreamEx = nil;
  fDest: TFileStreamEx = nil;
begin
  memWatch.Clear;
  prgbrDoIt.Position:=0;
  iFileSize:=StrConvert(cmbxSize.Text);
  if iFileSize<=0 then
  begin
    memWatch.Append(rsSplitErrFileSize);
    //Incorrect file size format!
    exit;
  end;
  if not mbDirectoryExists(edDirTarget.Text) then
  begin
    if not mbCreateDir(edDirTarget.Text) then
    begin
      memWatch.Append(rsSplitErrDirectory);
      //Unable to create target directory!
      exit;
    end;
  end;
  if edDirTarget.Text[Length(edDirTarget.Text)]<>PathDelim then
     edDirTarget.Text:=edDirTarget.Text+PathDelim;

  try
    fSource:= TFileStreamEx.Create(edFileSource.Text,fmOpenRead);
  except
    on E: EFOpenError do
      begin
        MessageDlg(Caption, rsMsgErrEOpen + ': ' + E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    on E: EReadError do
      begin
        MessageDlg(Caption, rsMsgErrERead + ': ' + E.Message, mtError, [mbOK], 0);
        Exit;
      end;
  end;

  try
    if iFileSize >= fSource.Size then
    begin
      memWatch.Append(rsSplitErrSplitFile);
      //Unable to split the file!
      exit;
    end;
    prgbrDoIt.Max:=(fSource.Size div iFileSize);
    if prgbrDoIt.Max=0 then
    begin
      memWatch.Append(rsSplitErrSplitFile);
      //Unable to split the file!
      exit;
    end;
    if prgbrDoIt.Max > 100 then
      begin
        if MessageDlg(Caption, rsSplitMsgManyParts, mtWarning, mbYesNo, 0) <> mrYes then
          begin
            memWatch.Append(rsSplitErrTooManyParts);
            // Too many parts
            Exit;
          end;
      end;
    num:=0;
    i:=prgbrDoIt.Max;
    while i>=1 do
    begin
      i:=i div 10;
      inc(num);
    end;
    i:=0;
    while i<=prgbrDoIt.Max-1 do
    try
      fDest:=TFileStreamEx.Create(
        edDirTarget.Text+ExtractFileName(edFileSource.Text)+
        '.'+Format('%.*d',[num+1,i])+'.split'
        ,fmCreate);
      try
        fSource.Seek(iFileSize*i,soFromBeginning);
        fDest.CopyFrom(fSource,iFileSize);
        memWatch.Append(rsSplitMsgCreated+' '+
        edDirTarget.Text+ExtractFileName(edFileSource.Text)+
        '.'+Format('%.*d',[num+1,i])+'.split'+
        ' ... '+rsSplitMsgSize+' '+
        IntToStr(iFileSize)+'b');
        prgbrDoIt.Position:=prgbrDoIt.Position+1;
      finally
        FreeThenNil(fDest);
      end;
      inc(i);
    except
      on E: EFCreateError do
        begin
          MessageDlg(Caption, rsMsgErrECreate + ': ' + E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      on E: EWriteError do
        begin
          MessageDlg(Caption, rsMsgErrEWrite + ': ' + E.Message, mtError, [mbOK], 0);
          Exit;
        end;
    end;
    if (fSource.Position)<fSource.Size then
    try
      fDest:=TFileStreamEx.Create(
        edDirTarget.Text+ExtractFileName(edFileSource.Text)+
        '.'+Format('%.*d',[num+1,i])+'.split'
        ,fmCreate);
      try
        fDest.CopyFrom(fSource,fSource.Size-fSource.Position);
        memWatch.Append(rsSplitMsgCreated+' '+
        edDirTarget.Text+ExtractFileName(edFileSource.Text)+
        '.'+Format('%.*d',[num+1,i])+'.split ... '+
        rsSplitMsgSize+' '+
        IntToStr(fSource.Size-(iFileSize*i))+'b');
        prgbrDoIt.Position:=prgbrDoIt.Position+1;
      finally
        FreeThenNil(fDest);
      end;
    except
      on E: EFCreateError do
        begin
          MessageDlg(Caption, rsMsgErrECreate + ': ' + E.Message, mtError, [mbOK], 0);
          Exit;
        end;
      on E: EWriteError do
        begin
          MessageDlg(Caption, rsMsgErrEWrite + ': ' + E.Message, mtError, [mbOK], 0);
          Exit;
        end;
    end;
  finally
    FreeThenNil(fSource);
  end;
end;

procedure TfrmSplitter.cmbxSizeCloseUp(Sender: TObject);
begin
  SetNumberOfPart;
end;

procedure TfrmSplitter.cmbxSizeKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'.. '9', #8, #46,'G','g','M','m','K','k','B','b']) then
    begin
      Key := #0;
      Exit;
    end;
end;

procedure TfrmSplitter.cmbxSizeKeyUp(Sender: TObject; var Key: char;
  Shift: TShiftState);
begin
  if Key in ['0'.. '9','B','b'] then
    begin
      SetNumberOfPart;
      Exit;
    end;
  if Key in [#8, #46] then
    begin
      if (Pos ('M',UpperCase(cmbxSize.Text))>0) or
         (Pos ('K',UpperCase(cmbxSize.Text))>0) or
         (Pos ('G',UpperCase(cmbxSize.Text))>0) then Exit
      else SetNumberOfPart;
    end;
end;

procedure TfrmSplitter.rbtnKiloBChange(Sender: TObject);
begin
  SetNumberOfPart;
end;

end.
