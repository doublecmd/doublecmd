{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynUniHighlighter.pas, released 2003-01
All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.}

{
@abstract(Desginer for TSynUniSyn)
@authors(Vit [nevzorov@yahoo.com], Fantasist [walking_in_the_sky@yahoo.com])
@created(2003)
@lastmod(2003-02-01)
}

(******************************************************************************
Authors: Vit (Vitaly Nevzorov nevzorov@yahoo.com); 
         Fantasist (Kirill Burtsev walking_in_the_sky@yahoo.com)
Official Site: www.delphist.com
With all questions, please visit www.delphist.com/forum

Contributors:

Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
  Initially adapted for use with Lazarus and FPC - 2003-06-12
  Changes can be found by searching for: ////TL
  Issues that need review are flagged with one or more ! suffixes after TL

******************************************************************************)

unit SynUniDesigner;

{$mode objfpc}{$H+}

interface

uses
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Buttons,   ////TL Added for TButton
  Dialogs,
  Menus,
  GraphType, ////TL Added for TFontStyles
  LCLType,   ////TL Added for vk_* key declarations
  Classes, FileUtil, LazUTF8Classes,
  SysUtils,
  SynEdit,
  SynEditHighlighter,
  SynUniHighlighter;

type
  TNodeText = String;

  CClass=class of TControl;

  TSynUniDesigner=class(Tobject)
  Private
    F:TForm;
    pBottom, pTop, pLeft, pAttri, pDef, pNum, pDF, pDB, pNF, pNB, pProp, pMemo:TPanel;
    Lc, LDef, LNum:TLabel;
    cDb,cDi,CDu,cDs,cNb,cNi,CNu,cNs, cCaseSensitiveRt, cCaseSensitiveRg, cCloseOnWord, cCloseOnEOL,cAnyStart:TCheckBox;
    SampleMemo:TSynEdit;

    ////TL Changed from TPageControl to TNotebook... with fingers crossed!
    ////TL PageControl:TPageControl;
    PageControl:TNotebook;

    ////TL Changed from TTabsheet to TPage... with fingers crossed again!
    ////TL RootRange,Range,KeyWords:TTabsheet;
    RootRange,Range,KeyWords:TPage;
    Memo:TMemo;
    EFrom, ETo, EDelimitersRg, EDelimitersRt:TEdit;
    UpdatingControls:boolean;
    AddRange, AddRangeRt, AddKeyword,AddKeywordRt, bSort, bLowercase, bExchange:TButton;
    Tree:TTreeView;
    h:TSynUniSyn;
    ImageList: TImageList;
    TreeMenu: TPopupMenu;
    procedure SetControlAttributes(Node: TTreeNode);
    procedure SetControlKWAttributes(Node: TTreeNode);
    procedure KwListChange(Sender: TObject);
    procedure TotalUpdate;
    procedure AddingRange(Node: TTreeNode);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    ////TL replaced out prefix procedure CreateComponent(out Comp: TControl; CName: CClass; Parent: TWinControl; Align: TAlign);
    procedure CreateComponent(var Comp: TControl; CName: CClass; Parent: TWinControl; Align: TAlign);
    procedure DoAddRange(Sender: TObject);
    ////TL replaced out prefix procedure CreateCheckBox(out comp: TCheckBox; Parent: TWinControl; left, top, width, height: integer);
    procedure CreateCheckBox(var comp: TCheckBox; Parent: TWinControl; left, top, width, height: integer);
    procedure CreateLabel(Parent: TWinControl; left, top, width, height: integer; Caption: string);
    ////TL replaced out prefix procedure CreatePanel(out comp: TPanel; Parent: TWinControl; left, top, width, height: integer);
    procedure CreatePanel(var comp: TPanel; Parent: TWinControl; left, top, width, height: integer);
    function GetNodeType(Node: TTreeNode): TNodeType;
    procedure SetDefaultAttributes(Attr: TSynHighlighterAttributes);
    procedure SetDefaultRangeAttr(R: TSynRange);
    procedure DoAddKeyword(Sender: TObject);
    procedure AddingKeyWord(Node: TTreeNode);
    procedure PanelColorChange(Sender: TObject);
    procedure SetAttributes(Node: TTreeNode);
    procedure StatusChanged(Sender: TObject);
    procedure RangeChange(Sender: TObject);
    procedure RootRangeChange(Sender: TObject);
    function GetSample: string;
    procedure SetSample(const Value: string);
    procedure TreeEdited(Sender: TObject; Node: TTreeNode; var S: TNodeText);
    procedure FillTree;
    ////TL Dupe ID... changed to Range1
    procedure TreeAddRange(Node: TTreeNode; Range1: TSynRange);
    procedure TreeAddSymbol(Node: TTreeNode; kw: TSynSymbolGroup);
    procedure SampleMemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DeleteNode(Node: TTreeNode);
    procedure DelimDblClick(Sender: TObject);
    procedure SampleDblClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SortClick(Sender: TObject);
    procedure Case2Lower(Sender: TObject);
    procedure ChangeSpaces(Sender: TObject);
    procedure TreeMenuPopup(Sender: TObject);
    procedure LoadFromFileClick(Sender: TObject);
    procedure SetTitle(const Value: string);
    function GetTitle: string;
  Public
    Constructor Create(highl:TSynUniSyn);
    Destructor Destroy;override;
    function Execute: boolean;
    ////TL Removed default optional parm... will explicitly state in the calls
    ////TL Changed Title to Title1 to avoid dupe identifier error from fpc
    ////TL class function EditHighlighter(aHL: TSynUniSyn; Title:string=''): boolean;
    class function EditHighlighter(aHL: TSynUniSyn; Title1:string): boolean;
    Property Title:string read GetTitle write SetTitle;
    property Sample:string read GetSample write SetSample;
  End;

implementation

Function GetFontStyle(bold, italic, underline, strikeout:boolean):TFontStyles;
begin
  result:=[];
  if bold then result:=result+[fsBold];
  if italic then result:=result+[fsitalic];
  if underline then result:=result+[fsunderline];
  if strikeout then result:=result+[fsStrikeOut];
end;

{ TSynUniDesigner }

Procedure TSynUniDesigner.AddingKeyWord(Node:TTreeNode);
  var n:TTreeNode;
      kw:TSynSymbolGroup;
begin
  n:=Tree.Items.AddChild(Node, _NewRange);
  kw:=TSynSymbolGroup.Create('',TSynHighlighterAttributes.Create('unknown'));
  kw.Name:=_NewRange;
  n.Data:=kw;
  n.ImageIndex:=2;
  n.SelectedIndex:=2;
  TSynRange(Node.data).AddSymbolGroup(kw);
  n.Expand(false);
  n.Selected:=true;
  tree.SetFocus;
  n.EditText;
  SetDefaultAttributes(kw.Attribs);
  SetControlKWAttributes(n);
end;

procedure TSynUniDesigner.Case2Lower(Sender: TObject);
begin
  Memo.text:=Lowercase(memo.text);
end;

procedure TSynUniDesigner.ChangeSpaces(Sender: TObject);
begin
  Memo.text:=StringReplace(memo.text, ' ',#13#10,[rfReplaceAll]);
end;

constructor TSynUniDesigner.Create(highl: TSynUniSyn);
  var b:TBitmap;
begin
  inherited create;
  h:=highl;
  F:=TForm.create(nil);
  F.Position:=poScreenCenter;
  F.Width:=600;
  F.Height:=400;
  F.KeyPreview:=true;
  ////TL added 2 @ prefixes
  f.OnKeyPress:=@FormKeyPress;
{$WARNING TODO Fix UTF8BIDI issue}
  f.OnUTF8KeyPress:=nil;
  f.OnKeyDown:=@FormKeyDown;
  f.caption:='Unihighlighter Designer (c) Fantasist, Vit (2002)';

//  Menu:=TMainMenu.Create(f);


//  if FFileName<>'' then
//    f.caption := f.caption+' - '+ExtractFileName(FFileName);

  CreateComponent(TControl(pTop), TPanel, F, alTop);
  pTop.Height:=184;

  CreateComponent(TControl(pLeft), TPanel, pTop, alLeft);
  pLeft.Width:=200;

  ImageList:=TImageList.create(F);
  b:=TBitmap.create;
  b.Width:=16;
  b.Height:=16;
  b.canvas.Font.color:=clRed;
  b.canvas.Font.Size:=14;
  b.canvas.TextOut(1,1,#52);
  ImageList.AddMasked(b, clWhite);
  b.free;
  b:=TBitmap.create;
  b.Width:=16;
  b.Height:=16;
  b.canvas.Font.color:=clGreen;
  b.canvas.Font.Size:=14;
  b.canvas.TextOut(1,1,#52);
  ImageList.AddMasked(b, clWhite);
  b.free;
  b:=TBitmap.create;
  b.Width:=16;
  b.Height:=16;
  b.canvas.Font.Size:=14;
  b.canvas.Font.color:=clblue;
  b.canvas.TextOut(1,1,#104);
  ImageList.AddMasked(b, clWhite);
  b.free;


  Tree:=TTreeView.Create(pLeft);
  Tree.Align:=alClient;
  Tree.Parent:=pLeft;
  Tree.Images:=ImageList;
  Tree.HideSelection:=false;
  Tree.RightClickSelect:=True;
  ////TL added @ prefixes
  Tree.OnEdited:=@TreeEdited;
  Tree.OnKeyDown:=@TreeKeyDown;
  Tree.OnMouseUp:=@TreeMouseUp;

  CreateComponent(TControl(pAttri), TPanel, pTop, alRight);
  pAttri.Anchors:=[akRight];
  pAttri.Width:=200;
  pAttri.BevelInner := bvRaised;
  pAttri.BevelOuter := bvLowered;


  CreateComponent(TControl(Lc), TLabel, pAttri, alTop);
  Lc.Alignment := taCenter;
  Lc.Caption := 'Attributes';
  Lc.Color := 16776176;
  Lc.Font.Color := clGreen;
  Lc.Font.Style := [fsBold];

  CreateLabel(pAttri, 10, 43, 66, 13, 'Foreground');
  CreateLabel(pAttri, 10, 66, 66, 13, 'Background');
  CreateLabel(pAttri, 10, 90, 66, 13, 'Bold');
  CreateLabel(pAttri, 10, 113, 66, 13, 'Italic');
  CreateLabel(pAttri, 10, 137, 66, 13, 'Underline');
  CreateLabel(pAttri, 10, 160, 66, 13, 'StrikeOut');

  CreatePanel(pDef, pAttri, 79, 19, 58, 161);
  pDef.BevelInner := bvLowered;

  CreateComponent(TControl(LDef), TLabel, pDef, alTop);
  LDef.Alignment := taCenter;
  LDef.Caption := 'Default';
  LDef.Font.Color := clMaroon;
  LDef.Font.Style := [fsBold];

  CreatePanel(pDF, pDef, 8, 20, 42, 21);
  ////TL added @ prefixes to the onclick event assignments
  pDF.OnClick:=@PanelColorChange;
  CreatePanel(pDB, pDef, 8, 44, 42, 21);
  pDB.OnClick:=@PanelColorChange;

  CreateCheckBox(cDb,pDef, 21, 67, 17, 17);
  cDb.OnClick:=@StatusChanged;

  CreateCheckBox(cDi,pDef, 21, 91, 17, 17);
  cDi.OnClick:=@StatusChanged;

  CreateCheckBox(CDu,pDef, 21,114, 17, 17);
  cDu.OnClick:=@StatusChanged;

  CreateCheckBox(cDs,pDef, 21,138, 17, 17);
  cDs.OnClick:=@StatusChanged;

  CreatePanel(pNum, pAttri, 137, 19, 58, 161);
  pNum.BevelInner := bvLowered;

  CreateComponent(TControl(LNum), TLabel, pNum, alTop);
  LNum.Alignment := taCenter;
  LNum.Caption := 'Numbers';
  LNum.Font.Color := clMaroon;
  LNum.Font.Style := [fsBold];

  CreatePanel(pNF, pNum, 8, 20, 42, 21);
  pNF.OnClick:=@PanelColorChange;

  CreatePanel(pNB, pNum, 8, 44, 42, 21);
  pNB.OnClick:=@PanelColorChange;

  CreateCheckBox(cNb,pNum, 21, 67, 17, 17);
  cNb.OnClick:=@StatusChanged;

  CreateCheckBox(cNi,pNum, 21, 91, 17, 17);
  cNi.OnClick:=@StatusChanged;

  CreateCheckBox(CNu,pNum, 21,114, 17, 17);
  cNu.OnClick:=@StatusChanged;

  CreateCheckBox(cNs,pNum, 21,138, 17, 17);
  cNs.OnClick:=@StatusChanged;

  CreateComponent(TControl(pProp), TPanel, pTop, alClient);
  pProp.BevelOuter:= bvLowered;
  pProp.BevelInner:= bvLowered;

  CreateComponent(TControl(PageControl), TNotebook, pProp, alClient);
  //PageControl.ShowTabs:=false;

  PageControl.Pages.Add('');
  RootRange:=PageControl.Page[0];

  CreateCheckBox(cCaseSensitiveRt,RootRange, 10,10, 140, 17);
  cCaseSensitiveRt.Caption:='Case Sensitive';
  cCaseSensitiveRt.OnClick:=@RootRangeChange;

  CreateLabel(RootRange, 10, 102, 44, 17, 'Delimiters:');
  CreateComponent(TControl(EDelimitersRt), TEdit, RootRange, alNone);
  EDelimitersRt.Top:=98;
  EDelimitersRt.Left:=60;
  EDelimitersRt.Width:=115;
  EDelimitersRt.Anchors:=[akLeft,akRight, akTop];
  EDelimitersRt.font.Name:='FixedSys';
  EDelimitersRt.OnChange:=@RootRangeChange;
  ////TL! No OnDblClick available in TEdit... using OnClick instead
  ////TL! EDelimitersRt.OnDblClick:=DelimDblClick;
  EDelimitersRt.OnClick:=@DelimDblClick;

  CreateComponent(TControl(AddRangeRt), TButton, RootRange, alNone);
  AddRangeRt.Left:=10;
  AddRangeRt.Top:=130;
  AddRangeRt.Caption:='Add Range';
  AddRangeRt.OnClick:=@DoAddRange;

  CreateComponent(TControl(AddKeywordRt), TButton, RootRange, alNone);
  AddKeywordRt.Left:=100;
  AddKeywordRt.Top:=130;
  AddKeywordRt.Caption:='Add Keywords';
  AddKeywordRt.OnClick:=@DoAddKeyword;


  PageControl.Pages.Add('');
  Range:=PageControl.Page[1];
  CreateCheckBox(cCaseSensitiveRg,Range, 10,10, 140, 17);
  cCaseSensitiveRg.OnClick:=@RangeChange;
  cCaseSensitiveRg.Caption:='Case Sensitive';

  CreateLabel(Range, 10, 33, 44, 17, 'From');
  CreateComponent(TControl(EFrom), TEdit, Range, alNone);
  EFrom.Top:=30;
  EFrom.Left:=35;
  EFrom.Width:=60;
  EFrom.OnChange:=@RangeChange;

  CreateLabel(Range, 100, 33, 44, 17, 'To');
  CreateComponent(TControl(ETo), TEdit, Range, alNone);
  ETo.Top:=30;
  ETo.Left:=115;
  ETo.Width:=60;
  ETo.OnChange:=@RangeChange;
//56
  CreateCheckBox(cCloseOnWord,Range, 10,56, 140, 17);
  cCloseOnWord.OnClick:=@RangeChange;
  cCloseOnWord.Caption:='Close on delimiter';

  CreateCheckBox(cCloseOnEOL,Range, 10,77, 140, 17);
  cCloseOnEOL.Caption:='Close on end of line';
  cCloseOnEOL.OnClick:=@RangeChange;


  CreateCheckBox(cAnyStart ,Range, 10, 98, 160, 17);
  cAnyStart.Caption:='Open range tag is part of term';
  cAnyStart.OnClick:=@RangeChange;

  CreateLabel(Range, 10, 120, 44, 17, 'Delimiters:');
  CreateComponent(TControl(EDelimitersRg), TEdit, Range, alNone);
  EDelimitersRg.Top:=116;
  EDelimitersRg.Left:=60;
  EDelimitersRg.Width:=115;
  EDelimitersRg.Anchors:=[akLeft,akRight, akTop];
//  EDelimitersRg.font.Charset:=255;
  EDelimitersRg.OnChange:=@RangeChange;
  ////TL! No OnDblClick available in TEdit... using OnClick instead
  ////TL! EDelimitersRg.OnDblClick:=DelimDblClick;
  EDelimitersRg.OnClick:=@DelimDblClick;

  CreateComponent(TControl(AddRange), TButton, Range, alNone);
  AddRange.Left:=10;
  AddRange.Top:=142;
  AddRange.Caption:='Add Range';
  AddRange.OnClick:=@DoAddRange;

  CreateComponent(TControl(AddKeyword), TButton, Range, alNone);
  AddKeyword.Left:=100;
  AddKeyword.Top:=142;
  AddKeyword.Caption:='Add Keywords';
  AddKeyword.OnClick:=@DoAddKeyword;

  PageControl.Pages.Add('');
  KeyWords:=PageControl.Page[2];
  CreateComponent(TControl(pMemo), TPanel, KeyWords, alRight);
  pMemo.Anchors:=[akRight];
  pMemo.Width:=100;
  pMemo.BevelInner:=bvLowered;

  CreateComponent(TControl(bSort), TButton, pMemo, alNone);
  bSort.Left:=3;
  bSort.Top:=2;
  bSort.Width:=95;
  bSort.caption:='Sort';
  bSort.OnClick:=@SortClick;

  CreateComponent(TControl(bLowercase), TButton, pMemo, alNone);
  bLowercase.Left:=3;
  bLowercase.Top:=28;
  bLowercase.Width:=95;
  bLowercase.caption:='Lower Case';
  bLowercase.OnClick:=@Case2Lower;

  CreateComponent(TControl(bExchange), TButton, pMemo, alNone);
  bExchange.Left:=3;
  bExchange.Top:=54;
  bExchange.Width:=95;
  bExchange.caption:='Spaces->Breaks';
  bExchange.OnClick:=@ChangeSpaces;

  CreateComponent(TControl(Memo), TMemo, KeyWords, alClient);
  Memo.ScrollBars:=ssVertical;
  ////TL! this probably available... later: memo.BorderStyle:=bsNone;

  pBottom:=TPanel.Create(F);
  pBottom.Parent:=F;
  pBottom.Align:=alClient;
  pBottom.Visible:=true;

  CreateComponent(TControl(SampleMemo), TSynEdit, pBottom, alClient);
  SampleMemo.Highlighter:=h;
  SampleMemo.Lines.Text:=h.Info.Sample.Text;
  SampleMemo.OnKeyDown:=@SampleMemoKeyDown;
  SampleMemo.WantTabs:=true;
  SampleMemo.OnDblClick:=@SampleDblClick;

  TreeMenu := TPopupMenu.Create( F );
  TreeMenu.OnPopup := @TreeMenuPopup;
  TreeMenu.Items.Add( TMenuItem.Create( F ) );
  TreeMenu.Items.Items[0].Caption := 'Load from file ...';
  TreeMenu.Items.Items[0].OnClick := @LoadFromFileClick;
  Tree.PopupMenu := TreeMenu;

  FillTree;

  Tree.OnChange:=@TreeChange;
  ////TL! changed to PageIndex for TNotebook PageControl.ActivePageIndex:=1;
  PageControl.PageIndex:=1;
  Tree.Selected:=Tree.items[0];
  TreeChange(nil, Tree.Selected);
  Tree.items[0].Expand(false);
end;

Procedure TSynUniDesigner.CreateCheckBox(var comp:TCheckBox; Parent:TWinControl; left, top, width, height:integer);
begin
  CreateComponent(TControl(Comp), TCheckBox, Parent, alNone);
  Comp.Left := Left;
  Comp.Top := Top;
  Comp.Width := Width;
  Comp.Height := Height;
end;

Procedure TSynUniDesigner.CreateComponent(var Comp:TControl; CName:CClass; Parent:TWinControl; Align:TAlign);
begin
  Comp:=Cname.create(Parent);
  Comp.Parent:=Parent;
  Comp.Align:=Align;
  Comp.Visible:=true;
end;

Procedure TSynUniDesigner.CreateLabel(Parent:TWinControl; left, top, width, height:integer; Caption:string);
  var Comp:TLabel;
begin
  CreateComponent(TControl(Comp), TLabel, Parent, alNone);
  Comp.Left := Left;
  Comp.Top := Top;
  Comp.Width := Width;
  Comp.Height := Height;
  Comp.Caption := Caption;
  Comp.Alignment := taRightJustify;
end;

Procedure TSynUniDesigner.CreatePanel(var comp:TPanel; Parent:TWinControl; left, top, width, height:integer);
begin
  CreateComponent(TControl(Comp), TPanel, Parent, alNone);
  Comp.Left := Left;
  Comp.Top := Top;
  Comp.Width := Width;
  Comp.Height := Height;
end;

procedure TSynUniDesigner.DeleteNode(Node:TTreeNode);
  var i:integer;
begin
  for i:=0 to Node.Count-1 do DeleteNode(Node[i]);
  if TObject(Tree.Selected.data) is TSynRange then
    TSynRange(Tree.Selected.Parent.data).DeleteRange(TSynRange(Tree.Selected.data))
  else
    if TObject(Tree.Selected.data) is TSynSymbolGroup then TSynRange(Tree.Selected.Parent.data).DeleteSymbolGroup(TSynSymbolGroup(Tree.Selected.data));
  Node.Delete;
end;

Procedure TSynUniDesigner.DelimDblClick(Sender:TObject);
begin
  (Sender as TEdit).text:=set2string(DefaultTermSymbols);
end;

destructor TSynUniDesigner.Destroy;
begin
  SampleMemo.Highlighter:=nil;
  SampleMemo.Free;
  F.free;
  inherited;
end;

procedure TSynUniDesigner.DoAddKeyword(Sender: TObject);
begin
  AddingKeyWord(Tree.selected);
end;

////TL dupe identifier Title... changed to Title1
class function TSynUniDesigner.EditHighlighter(aHL: TSynUniSyn; Title1: string): boolean;
var
  d:TSynUniDesigner;
  iTempSyn: TSynUniSyn;
  iBuffer: TMemoryStream;
begin
  iBuffer := TMemoryStream.Create;
  try
    if aHL.MainRules.RangeCount > 0 then
      aHL.SaveToStream( iBuffer );
    iBuffer.Position := 0;
    iTempSyn := TSynUniSyn.Create( nil );
    try
      if iBuffer.Size <> 0 then
        iTempSyn.LoadFromStream( iBuffer );
      iBuffer.Clear;

      d := TSynUniDesigner.Create(iTempSyn);
      try
        if Title1<>'' then d.Title:=Title1;
        d.Sample:= aHL.Info.Sample.Text;
        Result := d.Execute;
        if Result and (iTempSyn.MainRules.RangeCount > 0) then
        begin
          iTempSyn.SaveToStream( iBuffer );
          iBuffer.Position := 0;
          aHL.LoadFromStream( iBuffer );
          aHL.Info.Sample.Text := d.Sample;
        end;
      finally
        d.free;
      end;
    finally
      iTempSyn.Free;
    end;
  finally
    iBuffer.Free;
  end;
end;

function TSynUniDesigner.Execute: boolean;
begin
  //Result := F.showmodal = mrOk;
  F.showmodal;
  Result := True;
end;

procedure TSynUniDesigner.FillTree;
begin
  TreeAddRange(nil, h.MainRules);
end;

procedure TSynUniDesigner.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (key=#27) and (not Tree.IsEditing) then F.close;
end;

procedure TSynUniDesigner.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key=vk_F1) then showmessage('UniHighlighter Component'+#13#10#13#10+'Copyright(c):'+#13#10+'Fantasist (walking_in_the_sky@yahoo.com)'+#13#10+'Vit (nevzorov@yahoo.com)'+#13#10#13#10+'Official Web Site: www.delphist.com'+#13#10#13#10+'New York-Chicago-2002');
end;

Function TSynUniDesigner.GetNodeType(Node:TTreeNode):TNodeType;
begin
  Result:=ntNone;
  if Node<>nil then
   if (TObject(Node.data) is TSynRange) and (Node.level=0) then Result:=ntRootRange else
     if TObject(Node.data) is TSynRange then Result:=ntRange else
       if TObject(Node.data) is TSynSymbolGroup then Result:=ntKeyWords;
end;

function TSynUniDesigner.GetSample: string;
begin
  Result:=SampleMemo.lines.Text;
end;

function TSynUniDesigner.GetTitle: string;
begin
  Result:=f.Caption;
end;

procedure TSynUniDesigner.PanelColorChange(Sender: TObject);
begin
  (Sender as TPanel).BevelOuter := bvLowered;
  With TColorDialog.Create(nil) do
    try
      ////TL!! Not there with TColorDialog: CustomColors.text:='ColorA='+inttohex((Sender as TPanel).color,6)+#13#10+'ColorB=FFFFEE'+#13#10+'ColorC=EEFFFF'+#13#10+'ColorD=EEFFEE'+#13#10+'ColorE=EEEEFF'+#13#10+'ColorF=FFEEEE'+#13#10+'ColorG=EEEEEE'+#13#10+'ColorH=FFEEAA'+#13#10+'ColorJ=FFAAEE'+#13#10+'ColorK=AAFFEE'+#13#10+'ColorI=AAEEFF'+#13#10+'ColorL=EEFFAA'+#13#10+'ColorM=EEAAFF'+#13#10+'ColorN=AAAAAA'+#13#10+'ColorO=DDDDDD'+#13#10+'ColorP=999999';
      Color:=(Sender as TPanel).color;
      ////TL!! Not there with TColorDialog: Options:=[cdFullOpen];
      if Execute then
        begin
          (Sender as TPanel).color:=color;
          SetAttributes(Tree.selected);
        end;
    finally
      free;
    end;
  (Sender as TPanel).BevelOuter := bvRaised;
end;

Procedure TSynUniDesigner.RangeChange(Sender: TObject);
  var i:integer;
begin
  if UpdatingControls then exit;
  if GetNodeType(Tree.selected) in [ntRange]  then
    begin
      TSynRange(Tree.selected.data).OpenSymbol.Symbol:=EFrom.text;
      TSynRange(Tree.selected.data).CloseSymbol.Symbol:=ETo.text;
      TSynRange(Tree.selected.data).CloseOnTerm:=cCloseOnWord.Checked;
      TSynRange(Tree.selected.data).CloseOnEol:=cCloseOnEOL.Checked;
      TSynRange(Tree.selected.data).CaseSensitive:=cCaseSensitiveRg.Checked;
      TSynRange(Tree.selected.data).TermSymbols:=String2Set(EDelimitersRg.text);
      { the apparently useless typecast to char is for CLX compatibility }
      for i:=1 to length(EDelimitersRg.text) do
        TSynRange(Tree.selected.data).TermSymbols :=
          TSynRange(Tree.selected.data).TermSymbols + [Char(EDelimitersRg.text[i])];
      if cAnyStart.Checked then
        TSynRange(Tree.selected.data).OpenSymbol.BrakeType:=btAny
      else
        TSynRange(Tree.selected.data).OpenSymbol.BrakeType:=btTerm;
    end;
  TotalUpdate;
end;

Procedure TSynUniDesigner.RootRangeChange(Sender: TObject);
  var i:integer;
begin
  if UpdatingControls then exit;
  if GetNodeType(Tree.selected) in [ntRootRange]  then
    begin
      TSynRange(Tree.selected.data).CaseSensitive:=cCaseSensitiveRt.Checked;
    end;
  TSynRange(Tree.selected.data).TermSymbols:=[];
  { the apparently useless typecast to char is for CLX compatibility }
  for i:=1 to length(EDelimitersRt.text) do TSynRange(Tree.selected.data).TermSymbols:=TSynRange(Tree.selected.data).TermSymbols+[Char(EDelimitersRt.text[i])];
  TotalUpdate;
end;

Procedure TSynUniDesigner.SetAttributes(Node: TTreeNode);
begin
  if UpdatingControls then exit;
  if (node<>nil) and (TObject(Node.data) is TSynRange) then
    with TSynRange(Node.data) do
      begin
        DefaultAttri.Foreground:=pDF.Color;
        DefaultAttri.Background:=pDB.Color;
        DefaultAttri.Style:=GetFontStyle(cDB.checked, cDI.checked, cDU.checked,  cDS.checked);

        NumberAttri.Foreground:=pNF.Color;
        NumberAttri.Background:=pNB.Color;
        NumberAttri.Style:=GetFontStyle(cNB.checked, cNI.checked, cNU.checked,  cNS.checked);
        TotalUpdate;
      end;
  if (node<>nil) and (TObject(Node.data) is TSynSymbolGroup) then
    with TSynSymbolGroup(Node.data) do
      begin
        Attribs.Foreground:=pDF.Color;
        Attribs.Background:=pDB.Color;
        Attribs.Style:=GetFontStyle(cDB.checked, cDI.checked, cDU.checked,  cDS.checked);
        TotalUpdate;
      end;
end;

procedure TSynUniDesigner.SampleMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ////TL! replaced ActivePage with ActivePageComponent
  if (key=vk_Return) and (ssCtrl in Shift) and (PageControl.Page[PageControl.PageIndex] = KeyWords) then
    begin
      memo.lines.add(samplememo.SelText);
      TotalUpdate;
    end;
end;

procedure TSynUniDesigner.StatusChanged(Sender: TObject);
begin
  SetAttributes(Tree.selected);
end;

Procedure TSynUniDesigner.TreeAddRange(Node:TTreeNode;Range1:TSynRange);
  var temp:TTreeNode;
      i:integer;
begin
  if Node=nil then
    begin
      Temp:=Tree.items.add(nil,Range1.name);
      temp.ImageIndex:=0;
      temp.SelectedIndex:=0;
    end
  else
    begin
      Temp:=Tree.items.addChild(Node,Range1.name);
      temp.ImageIndex:=1;
      temp.SelectedIndex:=1;
    end;
  Temp.Data:=Range1;
  For i:=0 to Range1.SymbolGroupCount-1 do TreeAddSymbol(Temp, Range1.SymbolGroups[i]);
  For i:=0 to Range1.RangeCount-1 do TreeAddRange(Temp, Range1.Ranges[i]);
end;

Procedure TSynUniDesigner.TreeAddSymbol(Node:TTreeNode;kw:TSynSymbolGroup);
  var temp:TTreeNode;
begin
  temp:=Tree.items.addChild(Node,kw.name);
  temp.ImageIndex:=2;
  temp.SelectedIndex:=2;
  temp.data:=kw;
end;

procedure TSynUniDesigner.TreeEdited(Sender: TObject; Node: TTreeNode;
  var S: TNodeText);
begin
  if Node.Data=nil then exit;
  if TObject(Node.Data) is TSynRange then  TSynRange(Node.Data).Name:=s;
  if TObject(Node.Data) is TSynSymbolGroup then  TSynSymbolGroup(Node.Data).Name:=s;
end;

procedure TSynUniDesigner.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ////TL! Added TSynRange typecast to Tree.Selected.Data
  if (key=vk_Delete) and (h.MainRules<>TSynRange(Tree.Selected.Data)) and (not Tree.IsEditing) then
    begin
      if MessageDlg('Are you sure you want to delete '+Tree.Selected.text,mtWarning,[mbYes,MbNo],0)=mrYes then
        begin
          DeleteNode(Tree.Selected);
          TotalUpdate;
        end;
    end;
  if Key=vk_F2 then Tree.Selected.EditText;  
end;

Procedure TSynUniDesigner.SetControlAttributes(Node: TTreeNode);
begin
  UpdatingControls:=true;
  if TObject(Node.data) is TSynRange then
    with TSynRange(Node.data) do
      begin
        pDF.Color:=DefaultAttri.Foreground;
        pDB.Color:=DefaultAttri.Background;
        cDb.checked:=fsBold in DefaultAttri.Style;
        cDi.checked:=fsItalic in DefaultAttri.Style;
        CDu.checked:=fsUnderline in DefaultAttri.Style;
        cDs.checked:=fsStrikeOut in DefaultAttri.Style;

        pNF.Color:=NumberAttri.Foreground;
        pNB.Color:=NumberAttri.Background;
        cNb.checked:=fsBold in NumberAttri.Style;
        cNi.checked:=fsItalic in NumberAttri.Style;
        CNu.checked:=fsUnderline in NumberAttri.Style;
        cNs.checked:=fsStrikeOut in NumberAttri.Style;
      end;
  UpdatingControls:=false;
end;

Procedure TSynUniDesigner.SetControlKWAttributes(Node: TTreeNode);
begin
  UpdatingControls:=true;
  if TObject(Node.data) is TSynSymbolGroup then
    with TSynSymbolGroup(Node.data) do
      begin
        pDF.Color:=Attribs.Foreground;
        pDB.Color:=Attribs.Background;
        cDb.checked:=fsBold in Attribs.Style;
        cDi.checked:=fsItalic in Attribs.Style;
        CDu.checked:=fsUnderline in Attribs.Style;
        cDs.checked:=fsStrikeOut in Attribs.Style;
      end;
  UpdatingControls:=false;
end;

procedure TSynUniDesigner.TotalUpdate;
begin
  h.Reset;
  h.MainRules.Reset;
  h.ResetRange;
  h.Prepare;
  SampleMemo.Highlighter:=nil;
  SampleMemo.Highlighter:=h;
  SampleMemo.Refresh;
end;

procedure TSynUniDesigner.KwListChange(Sender: TObject);
begin
  TSynSymbolGroup(tree.selected.data).KeywordsList.text:=memo.lines.text;
  TotalUpdate;
end;

procedure TSynUniDesigner.TreeChange(Sender: TObject; Node: TTreeNode);
begin
 UpdatingControls:=true;
 case GetNodeType(Node) of
   ntRange:begin
       EFrom.text:=TSynRange(Node.data).OpenSymbol.Symbol;
       ETo.text:=TSynRange(Node.data).CloseSymbol.Symbol;
       cCloseOnWord.Checked:=TSynRange(Node.data).CloseOnTerm;
       cCloseOnEOL.Checked:=TSynRange(Node.data).CloseOnEol;
       cCaseSensitiveRg.Checked:=TSynRange(Node.data).CaseSensitive;
       EDelimitersRg.text:=Set2String(TSynRange(Node.data).TermSymbols);
       cAnyStart.Checked:=TSynRange(Node.data).OpenSymbol.BrakeType=btAny;
       SetControlAttributes(Node);
       pNum.visible:=true;
       ////TL! replaced ActivePage with ActivePageComponent
       PageControl.PageIndex := PageControl.Pages.IndexOf(Range.Name);
     end;
   ntRootRange:begin
       SetControlAttributes(Node);
       EDelimitersRt.text:=Set2String(TSynRange(Node.data).TermSymbols);
       pNum.visible:=true;
       cCaseSensitiveRt.Checked:=TSynRange(Node.data).CaseSensitive;
       ////TL! replaced ActivePage with ActivePageComponent
       PageControl.PageIndex := PageControl.Pages.IndexOf(RootRange.Name);
     end;
   ntKeyWords: begin
       SetControlKWAttributes(node);
       pNum.visible:=false;
       Memo.OnChange:=nil;
       Memo.lines.assign(TSynSymbolGroup(node.Data).KeywordsList);
       ////TL added @
       Memo.OnChange:=@KwListChange;
       ////TL! replaced ActivePage with ActivePageComponent
       PageControl.PageIndex := PageControl.Pages.IndexOf(Keywords.Name);
     end;
 End;
 UpdatingControls:=false;
end;

Procedure  TSynUniDesigner.AddingRange(Node:TTreeNode);
  var n:TTreeNode;
      r:TSynRange;
begin
 if Node=nil then
    begin
      Tree.items.clear;
      n:=Tree.Items.Add(nil, _Root);
      h.MainRules.Name:=_Root;
      n.Data:=h.MainRules;
      n.ImageIndex:=0;
      n.SelectedIndex:=0;
    end
  else
    begin
      n:=Tree.Items.AddChild(Node, _NewRange);
      ////TL Added explicit parameters to create call
      r:=TSynRange.create('','');
      r.Name:=_NewRange;
      r.OpenSymbol.BrakeType:=btAny;
      n.Data:=r;
      TSynRange(Node.data).AddRange(r);
      n.Expand(false);
      n.Selected:=true;
      tree.SetFocus;
      n.EditText;
      n.ImageIndex:=1;
      n.SelectedIndex:=1;
    end;
  SetDefaultRangeAttr(TSynRange(n.Data));
  SetControlAttributes(n);
end;

Procedure TSynUniDesigner.DoAddRange(Sender:TObject);
Begin
  AddingRange(Tree.selected);
End;

Procedure TSynUniDesigner.SampleDblClick(Sender:TObject);
begin
  ////TL! replaced ActivePage with ActivePageComponent
  if (PageControl.Page[PageControl.PageIndex]=KeyWords) then
      memo.Lines.Add(SampleMemo.SelText);
end;

Procedure TSynUniDesigner.SetDefaultAttributes(Attr:TSynHighlighterAttributes);
begin
  Attr.Background:=clWhite;
  Attr.Foreground:=clBlack;
  Attr.Style:=[];
end;

Procedure TSynUniDesigner.SetDefaultRangeAttr(R:TSynRange);
begin
  SetDefaultAttributes(R.NumberAttri);
  SetDefaultAttributes(R.DefaultAttri);
end;

procedure TSynUniDesigner.SetSample(const Value: string);
begin
  SampleMemo.Text:=Value;
end;

procedure TSynUniDesigner.SetTitle(const Value: string);
begin
  f.Caption := Value;
end;

procedure TSynUniDesigner.SortClick(Sender: TObject);
var i:integer;
begin
  With TStringList.Create do
    try
      Sorted:=true;
      Duplicates:=dupIgnore;
      for i:=0 to Memo.Lines.Count-1 do if trim(Memo.lines[i])<>'' then add(trim(Memo.lines[i]));
      sort;
      Memo.text:=trim(text);
    finally
      free;
    end;
end;

////TL FPC wanted this global... moved from just inside the procedure below
resourcestring
  sUniFileDescription = 'UniHighlighter Syntax';

procedure TSynUniDesigner.LoadFromFileClick(Sender: TObject);
////TL resourcestring
////TL  sUniFileDescription = 'UniHighlighter Syntax';
var
  iDlg: TOpenDialog;
  iFile: TFileStream;
  iNode: TTreeNode;
  iRange: TSynRange;
  cSub: integer;
begin
  { TreeMenuPopup should handle this }
  Assert( TObject(Tree.Selected.Data) is TSynRange );
  iNode := Tree.Selected;
  iRange := TSynRange(iNode.Data);
  iDlg := TOpenDialog.Create( nil );
  try
    iDlg.DefaultExt := '.hgl';
    iDlg.Filter := sUniFileDescription + ' (*.hgl)|*.hgl';
    if not iDlg.Execute then
      Exit;
    iFile := TFileStreamUTF8.Create( iDlg.FileName, fmOpenRead or fmShareDenyWrite );
    try
      if iRange = h.MainRules then
      begin
        h.LoadFromStream( iFile );
        Tree.Items.Clear;
        FillTree;
        Sample := h.SampleSource;
        iNode := Tree.Items[0];
      end
      else begin
        iRange.LoadFromStream( iFile );
        iNode.DeleteChildren;
        for cSub := 0 to iRange.SymbolGroupCount -1 do
          TreeAddSymbol( iNode, iRange.SymbolGroups[cSub] );
        for cSub := 0 to iRange.RangeCount -1 do
          TreeAddRange( iNode, iRange.Ranges[cSub] );
      end;
      iNode.Expand( False );
      TotalUpdate;
    finally
      iFile.Free;
    end;
  finally
    iDlg.Free;
  end;
end;

procedure TSynUniDesigner.TreeMenuPopup(Sender: TObject);
begin
  TreeMenu.Items.Items[0].Visible := (Tree.Selected <> nil) and
    ( TObject(Tree.Selected.Data) is TSynRange );
end;

procedure TSynUniDesigner.TreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iNode: TTreeNode;
begin
  if Button <> mbRight then
    Exit;
  iNode := Tree.GetNodeAt( X, Y );
  if iNode <> nil then
    iNode.Selected := True;
end;

end.
