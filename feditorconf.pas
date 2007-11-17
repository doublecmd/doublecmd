unit fEditorConf;

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEditHighlighter, SynEditStrConst,
  Grids, Buttons, ComCtrls, uGlobsPaths, SynEdit, ButtonPanel;

type
  TLittleAttr= Packed Record
     clFg:TColor;
     clBg:TColor;
     fntStyle:Integer;
  end;

const
  cCountSynAttrs=90;
//  cCountSynAttrs=1;
  csDefaultName='editor.col';
  
  // not all synedit highighter are ported
  cSynAttrNames: Array[0..cCountSynAttrs-1] of String =

  (SYNS_Untitled,
   SYNS_AttrAsm, SYNS_AttrAsmComment, SYNS_AttrAsmKey,
   //4
   SYNS_AttrAssembler, SYNS_AttrAttributeName, SYNS_AttrAttributeValue,
   SYNS_AttrBlock, SYNS_Untitled, SYNS_AttrBrackets,
   SYNS_AttrCDATASection, SYNS_AttrCharacter, SYNS_AttrClass,
   //13
   SYNS_AttrComment, SYNS_AttrCondition,
   SYNS_AttrDataType,  SYNS_AttrDefaultPackage,
   //16
   SYNS_AttrDir, SYNS_AttrDirective, SYNS_AttrDOCTYPESection,
   SYNS_AttrDocumentation, SYNS_AttrElementName, SYNS_AttrEmbedSQL,
   SYNS_AttrEmbedText, SYNS_AttrEntityReference, SYNS_AttrEscapeAmpersand,
   //25
   SYNS_AttrEvent, SYNS_AttrException, SYNS_AttrFloat, SYNS_AttrForm,
   SYNS_AttrFunction, SYNS_AttrHexadecimal, SYNS_AttrIcon,
   //32
   SYNS_AttrIdentifier, SYNS_AttrIllegalChar, SYNS_AttrInclude,
  { SYNS_AttrIndicator,}   SYNS_AttrIndirect, SYNS_AttrInvalidSymbol,
   SYNS_AttrInternalFunction, SYNS_AttrKey, SYNS_AttrLabel,
   //40
   SYNS_AttrMacro, SYNS_AttrMarker,
   SYNS_AttrMessage, SYNS_AttrMiscellaneous, SYNS_AttrNamespaceAttrName,
   SYNS_AttrNamespaceAttrValue, SYNS_AttrNonReservedKeyword,
   SYNS_AttrNull, SYNS_AttrNumber,
   SYNS_AttrOctal, SYNS_AttrOperator, SYNS_AttrPLSQL,
   //53

   SYNS_AttrPragma, SYNS_AttrPreprocessor,
   SYNS_AttrProcessingInstr, SYNS_AttrQualifier, SYNS_AttrRegister,
   SYNS_AttrReservedWord, SYNS_AttrRpl, SYNS_AttrRplKey,
   //60
   SYNS_AttrRplComment, SYNS_AttrSASM, SYNS_AttrSASMComment,
   SYNS_AttrSASMKey, SYNS_AttrSecondReservedWord,
   SYNS_AttrSection,{ SYNS_AttrSequence,}
   //66
   SYNS_AttrSpace,
   SYNS_AttrSpecialVariable, SYNS_AttrSQLKey, SYNS_AttrSQLPlus,
   //70
   SYNS_AttrString,{ SYNS_AttrSingleString,} SYNS_AttrSymbol,
   SYNS_AttrSyntaxError, SYNS_AttrSystem, SYNS_AttrSystemValue,
{   SYNS_AttrTagArea,} SYNS_AttrTableName, SYNS_AttrTerminator,
   //77
   SYNS_AttrText, SYNS_AttrUnknownWord, SYNS_AttrUser,
   SYNS_AttrUserFunction, SYNS_AttrValue, SYNS_AttrVariable,
   SYNS_AttrWhitespace, SYNS_AttrMathMode, SYNS_AttrTextMathMode,
   SYNS_AttrSquareBracket, SYNS_AttrRoundBracket, SYNS_AttrTeXCommand);

  cColorGrid:Array[0..15] of TColor=
  (clBlack, clMaroon, clGreen, clOlive, clNavy,
   clPurple, clTeal, clGray, clSilver, clRed,
   clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

var
  cAttrs:Array [0..cCountSynAttrs-1] of TLittleAttr;

type

  { TfrmEditorConf }

  TfrmEditorConf = class(TForm)
    grColor: TDrawGrid;
    lbNames: TListBox;
    lbSample: TLabel;
    cbBold: TCheckBox;
    cbUnderline: TCheckBox;
    cbStrikeOut: TCheckBox;
    cbItalic: TCheckBox;
    cmbPredefined: TComboBox;
    lbPredefined: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    edtSample: TSynEdit;
    procedure grColorDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure lbNamesClick(Sender: TObject);
    procedure grColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure cmbPredefinedChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbBoldClick(Sender: TObject);
  private
    { Private declarations }
    fbUpdatingBoxes:Boolean;
  public
    { Public declarations }
    procedure FillComboPred;
  end;
  procedure LoadAttrFromFile(const sFileName:String);
  procedure SaveAttrToFile(const sFileName:String);
  procedure SetupColorOfHighlighter(var h: TSynCustomHighlighter);


implementation


uses
  uShowMsg, uLng, uFindEx;

procedure SetupColorOfHighlighter(var h: TSynCustomHighlighter);
var
  i, j:Integer;
begin
  if not assigned(h) then Exit;
  for i:=0 to h.AttrCount-1 do
  begin
    with h.Attribute[i] do
    begin
      for j:=0 to cCountSynAttrs-1 do
      begin
        if h.Attribute[i].Name=cSynAttrNames[j] then
        begin
          Background:= cAttrs[j].clBg;
          Foreground:= cAttrs[j].clFg;
          //load style
          IntegerStyle:= cAttrs[j].fntStyle;
          Break;
        end;
      end;
    end;
  end;
{  // default attr
  if assigned(h.CommentAttribute) then
    with h.CommentAttribute do
    begin
      Background:= cAttrs[15].clBg;
      Foreground:= cAttrs[15].clFg;
      IntegerStyle:= cAttrs[15].fntStyle;
    end;
  if assigned(h.IdentifierAttribute) then
    with h.IdentifierAttribute do
    begin
      Background:= cAttrs[36].clBg;
      Foreground:= cAttrs[36].clFg;
      IntegerStyle:= cAttrs[36].fntStyle;
    end;
  if assigned(h.KeywordAttribute) then
    with h.KeywordAttribute do
    begin
      Background:= cAttrs[43].clBg;
      Foreground:= cAttrs[43].clFg;
      IntegerStyle:= cAttrs[43].fntStyle;
    end;
  if assigned(h.StringAttribute) then
    with h.StringAttribute do
    begin
      Background:= cAttrs[76].clBg;
      Foreground:= cAttrs[76].clFg;
      IntegerStyle:= cAttrs[76].fntStyle;
    end;
  if assigned(h.WhitespaceAttribute) then
    with h.WhitespaceAttribute do
    begin
      Background:= cAttrs[72].clBg;
      Foreground:= cAttrs[72].clFg;
      IntegerStyle:= cAttrs[72].fntStyle;
    end;}
end;


procedure LoadAttrFromFile(const sFileName:String);
var
  i:Integer;
  f:TextFile;
  s, sValue:String;
begin
  if Not FileExists(sFileName) Then Exit;
  assign(f,sFileName);
  reset(f);
  try
    while not eof(f) do
    begin
      readln(f,s);
      s:=Trim(s);
      if s='' then Continue;
      if s[1]='#' then Continue;
      if s[1]<>'[' then Continue;
      s:=Copy(s,2,length(s)-2);
      for i:=0 to cCountSynAttrs-1 do
      begin
        if s=cSynAttrNames[i] then
        begin
          // load bg
          readln(f,s);
          sValue:=Copy(s,Pos('=',s)+1,length(s));
          cAttrs[i].clBg:=StrToIntDef(sValue,0);

          // load fg
          readln(f,s);
          sValue:=Copy(s,Pos('=',s)+1,length(s));
          cAttrs[i].clFg:=StrToIntDef(sValue,0);

          //load style
          readln(f,s);
          sValue:=Copy(s,Pos('=',s)+1,length(s));
          cAttrs[i].fntStyle:=StrToIntDef(sValue,0);
        end;
      end;
    end;
  finally
    CloseFile(f);
  end;
end;

procedure SaveAttrToFile(const sFileName:String);
var
  i:Integer;
  f:TextFile;
begin
  assign(f,sFileName);
  rewrite(f);
  try
    writeln(f,'# color is $00bbggrr (in hex)');
    for i:=0 to cCountSynAttrs-1 do
    begin
      writeln(f,'[',cSynAttrNames[i],']');
      writeln(f,'bg=$',IntToHex(cAttrs[i].clBg,8));
      writeln(f,'fg=$',IntToHex(cAttrs[i].clFg,8));
      writeln(f,'style=$',cAttrs[i].fntStyle);
    end;
  finally
    CloseFile(f);
  end;
end;

procedure TfrmEditorConf.grColorDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with grColor.Canvas do
  begin
    Brush.Style:=bsSolid;
    Brush.Color:=cColorGrid[Acol*4+Arow];
    FillRect(Rect);
  end;
end;

procedure TfrmEditorConf.FormCreate(Sender: TObject);
var
  i:Integer;
begin
  grColor.ScrollBars:=ssNone;
  lbNames.Clear;
  for i:=0 to cCountSynAttrs-1 do
    lbNames.Items.Add(cSynAttrNames[i]);
  lbNames.ItemIndex:=0;
  FillComboPred;
  LoadAttrFromFile(gpIniDir+csDefaultName);
  lbNamesClick(Sender);
end;

procedure TfrmEditorConf.lbNamesClick(Sender: TObject);
begin
  edtSample.Font.Color:=cAttrs[lbNames.ItemIndex].clFg;
  edtSample.Color:=cAttrs[lbNames.ItemIndex].clBg;
  With edtSample.Font,cAttrs[lbNames.ItemIndex]   do
  begin
    if fntStyle and $1 = 0 then Style:= [] else Style:= [fsBold];
    if fntStyle and $2 = 2 then Style:= Style + [fsItalic];
    if fntStyle and $4 = 4 then Style:= Style + [fsUnderline];
    if fntStyle and $8 = 8 then Style:= Style + [fsStrikeout];
    fbUpdatingBoxes:=True;
    try
      cbBold.Checked:= (fntStyle and $1) <>0;
      cbItalic.Checked:=(fntStyle and $2) <> 0;
      cbUnderline.Checked:=(fntStyle and $4) <> 0;
      cbStrikeOut.Checked:=(fntStyle and $8) <> 0;
    finally
      fbUpdatingBoxes:=False;
    end;
  end;
end;

procedure TfrmEditorConf.grColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with grColor do
  begin
    if X>=DefaultColWidth*4 then Exit;
    if Y>=DefaultRowHeight*4 then Exit;

    if Button=mbRight then
      cAttrs[lbNames.ItemIndex].clBg:=cColorGrid[X div DefaultColWidth *4 + Y div DefaultRowHeight];
    if Button=mbLeft then
      cAttrs[lbNames.ItemIndex].clFg:=cColorGrid[X div DefaultColWidth *4 + Y div DefaultRowHeight];
    edtSample.Font.Color:=cAttrs[lbNames.ItemIndex].clFg;
    edtSample.Color:=cAttrs[lbNames.ItemIndex].clBg;
  end;
end;

procedure TfrmEditorConf.FillComboPred;
var
  fr:TSearchRec;
  iIndex:Integer;
begin
  cmbPredefined.Clear;
  if FindFirst(gpIniDir+'*.col', faAnyFile, fr)<>0 then
  begin
    FindClose(fr);
    Exit;
  end;
  repeat
    cmbPredefined.Items.Add(fr.Name);
  until FindNext(fr)<>0;
  FindClose(fr);

  if cmbPredefined.Items.Count>0 then
  begin
    iIndex:=cmbPredefined.Items.IndexOf(csDefaultName);
    if iIndex>=0 then
      cmbPredefined.ItemIndex:=iIndex;
  end;
end;

procedure TfrmEditorConf.btnOKClick(Sender: TObject);
begin
  SaveAttrToFile(gpIniDir+csDefaultName);
  Close;
end;

procedure TfrmEditorConf.cmbPredefinedChange(Sender: TObject);
begin
  LoadAttrFromFile(gpCfgDir+cmbPredefined.Text);
  lbNamesClick(Self);
//  MsgOk(Format(lngGetString(clngEditCfgLoadOK),[gpCfgDir+cmbPredefined.Text]));
end;

procedure TfrmEditorConf.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditorConf.cbBoldClick(Sender: TObject);
begin
  with edtSample.Font do
  begin
    if not cbBold.Checked then Style:= [] else Style:= [fsBold];
    if cbItalic.Checked then Style:= Style + [fsItalic];
    if cbUnderline.Checked then Style:= Style + [fsUnderline];
    if cbStrikeOut.Checked then Style:= Style + [fsStrikeout];
  end;
  if not fbUpdatingBoxes then
    cAttrs[lbNames.ItemIndex].fntStyle:= Ord(cbBold.Checked)+ 2*Ord(cbItalic.Checked)+4*ord(cbUnderline.Checked)+8*ord(cbStrikeOut.Checked);
end;

initialization
 {$I feditorconf.lrs}
end.
