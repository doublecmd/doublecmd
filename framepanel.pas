{
   Seksi Commander
   ----------------------------
   Implementing of File Panel Components, created dynamically (replacing TFrame)

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)
   
   Copyright (C) 2008 Vitaly Zotov (vitalyzotov@mail.ru)
}

unit framePanel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LMessages,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, uFilePanel, Grids, uTypes,
  Buttons, uColumns,lcltype,Menus;

type
  TFilePanelSelect=(fpLeft, fpRight);
  {class cracer}
  THackDrawGrid = class(TCustomDrawGrid)
  end;
  { TFrameFilePanel }

  TFrameFilePanel = class (TWinControl)
  private
    fSearchDirect,
    fNext,
    fPrevious : Boolean;
    procedure edSearchKeyPress(Sender: TObject; var Key: Char);
    procedure edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    pnlFooter: TPanel;
    pnPanel: TPanel;
    lblLInfo: TLabel;
    pnlHeader: TPanel;
    lblLPath: TLabel;
    edtPath,
    edtRename: TEdit;
//---------------------
    dgPanel: TDrawGrid;
   // Colm:TPanelColumnsClass;
    ActiveColm:String;
//---------------------
    pnAltSearch: TPanel;
    edtSearch: TEdit;
    procedure SetColWidths;
    procedure edSearchChange(Sender: TObject);
    procedure edtPathKeyPress(Sender: TObject; var Key: Char);
    procedure edtRenameKeyPress(Sender: TObject; var Key: Char);
    procedure dgPanelDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dgPanelExit(Sender: TObject);
    procedure dgPanelDblClick(Sender: TObject);
    procedure dgPanelEnter(Sender: TObject);
    procedure dgPanelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dgPanelKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure dgPanelMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);

    procedure dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
                                               State: TDragState; var Accept: Boolean);
    procedure dgPanelHeaderClick(Sender: TObject;IsColumn: Boolean; index: Integer);
    procedure dgPanelKeyPress(Sender: TObject; var Key: Char);
    procedure dgPanelPrepareCanvas(sender: TObject; Col, Row: Integer; aState: TGridDrawState);
    procedure dgPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure lblLPathMouseEnter(Sender: TObject);
    procedure lblLPathMouseLeave(Sender: TObject);
    procedure pnlHeaderResize(Sender: TObject);

  private
    { Private declarations }
    FLastMark:String;
    FLastSelect:TGridRect;
    FLastAutoSelect: Boolean;
    FLastSelectionStartRow: Integer;
  protected

  public
    { Public declarations }
    pnlFile:TFilePanel;
    edtCmdLine:TComboBox;
    PanelSelect:TFilePanelSelect;
    constructor Create(AOwner :TWinControl; lblDriveInfo : TLabel; lblCommandPath:TLabel; cmbCommand:TComboBox);
    destructor Destroy; override;
    procedure LoadPanel;
    procedure SetFocus;
    procedure SelectFile(frp:PFileRecItem);
    procedure SelectFileIfNoSelected(frp:PFileRecItem);
    procedure UnSelectFileIfSelected(frp:PFileRecItem);
    procedure MakeVisible(iRow:Integer);
    procedure MakeSelectedVisible;
    procedure InvertAllFiles;
    procedure MarkAll;
    procedure RefreshPanel;
    procedure Init;
    procedure ClearCmdLine;
    procedure CloseAltPanel;
    procedure ShowAltPanel(Char : Char = #0);
    procedure UnMarkAll;
    procedure UpDatelblInfo;
    Function GetActiveDir:String;
    procedure MarkMinus;
    procedure MarkPlus;
    procedure MarkShiftPlus;
    procedure MarkShiftMinus;
    function AnySelected:Boolean;
    procedure ClearGridSelection;
    procedure RedrawGrid;
    function GetActiveItem:PFileRecItem;
    property ActiveDir:String read GetActiveDir;
  end;

implementation

uses
  LCLProc, Masks, uLng, uShowMsg, uGlobs, GraphType, uPixmapManager, uVFSUtil, uDCUtils, uOSUtils, math;


procedure TFrameFilePanel.LoadPanel;
begin
  if pnAltSearch.Visible then
    CloseAltPanel;
  pnlFile.LoadPanel;
end;

procedure TFrameFilePanel.SetFocus;
begin
  with FLastSelect do
  begin
    if top<0 then Top:=0;
    if Left<0 then Left:=0;
    if Right<0 then Right:=0;
    if Bottom<0 then Bottom:=0;
  end;
  if dgPanel.Row<0 then
    dgPanel.Selection:=FLastSelect;
  dgPanel.SetFocus;
  lblLPath.Color:=clHighlight;
  lblLPath.Font.Color:=clHighlightText;
  pnlFile.UpdatePrompt;
//  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.SelectFile(frp:PFileRecItem);
begin
  pnlFile.InvertFileSection(frp);
  UpDatelblInfo;
end;

procedure TFrameFilePanel.SelectFileIfNoSelected(frp:PFileRecItem);
var
  i:Integer;
begin
  FLastAutoSelect:= False;
  for i:=0 to pnlFile.FileList.Count-1 do
  begin
    if pnlFile.FileList.GetItem(i)^.bSelected then Exit;
  end;
  pnlFile.InvertFileSection(frp);
  UpDatelblInfo;
  FLastAutoSelect:= True;
end;

procedure TFrameFilePanel.UnSelectFileIfSelected(frp:PFileRecItem);
begin
  if FLastAutoSelect and (frp^.bSelected) then
    begin
      pnlFile.InvertFileSection(frp);
      UpDatelblInfo;
    end;
  FLastAutoSelect:= False;
end;

procedure TFrameFilePanel.InvertAllFiles;
begin
  pnlFile.InvertAllFiles;
  dgPanel.Invalidate;
  UpDatelblInfo;
end;

procedure TFrameFilePanel.RefreshPanel;
begin
  if dgPanel.Row>=0 then
  begin
    pnlFile.LastActive:=pnlFile.GetActiveItem^.sName;
  end;
  if pnlFile.PanelMode = pmDirectory then
    pnlFile.LoadPanel
  else // if in VFS
    begin
      if pnlFile.VFS.VFSmodule.VFSRefresh then
        begin
          pnlFile.VFS.VFSmodule.VFSList(ExtractDirLevel(pnlFile.VFS.ArcFullName, ActiveDir), pnlFile.FileList);
          if gShowIcons then
            pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);
          pnlFile.Sort; // and Update panel
          dgPanel.Invalidate;
        end;
    end;
  if pnAltSearch.Visible then
    CloseAltPanel;
  UpDatelblInfo;
//  dgPanel.SetFocus;
end;


procedure TFrameFilePanel.Init;
begin
  ClearCmdLine;
  UpDatelblInfo;
  FLastMark:='*.*';
  FLastAutoSelect:= False;
  dgPanel.DefaultRowHeight:=gIconsSize;
  with FLastSelect do
  begin
    Left:=0;
    Top:=0;
    Bottom:=0;
    Right:=dgPanel.ColCount-1;
  end;
end;

procedure TFrameFilePanel.ClearCmdLine;
begin
  edtCmdLine.Text:='';
//  dgPanel.SetFocus;
end;

procedure TFrameFilePanel.dgPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iRow, iCol : Integer;
  ARow, AFromRow, AToRow: Integer;
  frp: PFileRecItem;
begin
  dgPanel.MouseToCell(X, Y, iCol, iRow);
  
{  if (Button=mbRight) and (iRow < dgPanel.FixedRows ) then
    begin
      pmFrColumnMenu.PopUp(X,Y);
    end;}
  case Button of
    mbRight: begin
      if (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
      begin
        frp := pnlFile.GetReferenceItemPtr(iRow - dgPanel.FixedRows); // substract fixed rows (header)
        if Assigned(frp) then
        begin
          pnlFile.InvertFileSection(frp);
          dgPanel.Invalidate;
        end;
      end;
    end;
    
    mbLeft: begin
      if (dgPanel.Row < 0) or (dgPanel.Row >= dgPanel.RowCount) then
        begin
          dgPanel.Row := iRow;
        end
      else if gMouseSelectionEnabled then
      begin
        if ssCtrl in Shift then
          begin
            frp := pnlFile.GetReferenceItemPtr(iRow - dgPanel.FixedRows); // substract fixed rows (header)
            if Assigned(frp) then
              begin
                pnlFile.InvertFileSection(frp);
                dgPanel.Invalidate;
              end;
          end
        else if ssShift in Shift then
          begin
            if(FLastSelectionStartRow < 0) then
              begin
                AFromRow := Min(dgPanel.Row, iRow) - dgPanel.FixedRows;
                AToRow := Max(dgPanel.Row, iRow) - dgPanel.FixedRows;
                FLastSelectionStartRow := dgPanel.Row;
              end
            else
              begin
                AFromRow := Min(FLastSelectionStartRow, iRow) - dgPanel.FixedRows; // substract fixed rows (header)
                AToRow := Max(FLastSelectionStartRow, iRow) - dgPanel.FixedRows;
              end;

            pnlFile.MarkAllFiles(False);
            for ARow := AFromRow to AToRow do
            begin
              frp := pnlFile.GetReferenceItemPtr(ARow);
              if not Assigned(frp) then Continue;
              pnlFile.MarkFile(frp, True);
            end;
            dgPanel.Invalidate;
          end
        else if (gMouseSelectionButton = 0) then
          begin
            pnlFile.MarkAllFiles(False);
            dgPanel.Invalidate;
          end;
      end;//of mouse selection handler
    end;
  else
    dgPanel.Row := iRow;
    SetFocus;
    Exit;
  end;
  
  if iRow >= dgPanel.FixedRows then begin // if not column header
    dgPanel.BeginDrag(False);
  end;
end;

procedure TFrameFilePanel.dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
begin

end;

procedure TFrameFilePanel.dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  THackDrawGrid(dgPanel).FGridState := gsRowMoving;
end;

procedure TFrameFilePanel.dgPanelHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
begin
  if not IsColumn then Exit;
  pnlFile.SortDirection:= not pnlFile.SortDirection;
  pnlFile.SortByCol(Index);
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.dgPanelKeyPress(Sender: TObject; var Key: Char);
begin
  DebugLn('dgpanel:' + Key)
end;

procedure TFrameFilePanel.dgPanelPrepareCanvas(sender: TObject; Col,
  Row: Integer; aState: TGridDrawState);
var
  FS : TFontStyles;
begin
  if Row=0 then Exit;
  with dgPanel do
  begin
    Canvas.Brush.Style:=bsSolid;
    Canvas.Font.Name := gFontName;
    Canvas.Font.Size := gFontSize;
    Move(gFontWeight, FS, 1);
    Canvas.Font.Style := FS;

    if gdSelected in aState then
      Canvas.Brush.Color:= gCursorColor
    else
      Canvas.Brush.Color:=Color;
  end;
end;

procedure TFrameFilePanel.dgPanelMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:= True;
  case gScrollMode of
  1:
    dgPanel.Perform(LM_VSCROLL, SB_LINEUP, 0);
  2:
    dgPanel.Perform(LM_VSCROLL, SB_PAGEUP, 0);
  else
    Handled:= False;
  end;  
end;

procedure TFrameFilePanel.dgPanelMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:= True;
  case gScrollMode of
  1:
    dgPanel.Perform(LM_VSCROLL, SB_LINEDOWN, 0);
  2:
    dgPanel.Perform(LM_VSCROLL, SB_PAGEDOWN, 0);
  else
    Handled:= False;
  end;
end;

procedure TFrameFilePanel.edSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (key=#13) or (key=#27) then
  begin
    CloseAltPanel;
    SetFocus;
  end;
end;

procedure TFrameFilePanel.edSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 40 then // Down
    begin
      fSearchDirect := True;
      fNext := True;
      Key := 0;
      edSearchChange(Sender);
    end;
  if Key = 38 then // Up
    begin
      fSearchDirect := False;
      fPrevious := True;
      Key := 0;
      edSearchChange(Sender);
    end;
end;

procedure TFrameFilePanel.SetColWidths;
var x:integer;
begin
  //  setup column widths
 dgPanel.ColCount:=ColSet.GetColumnSet(ActiveColm).ColumnsCount;
 if ColSet.GetColumnSet(ActiveColm).ColumnsCount>0 then
  for x:=0 to ColSet.GetColumnSet(ActiveColm).ColumnsCount-1 do
    dgPanel.ColWidths[x]:=ColSet.GetColumnSet(ActiveColm).GetColumnWidth(x);
end;

procedure TFrameFilePanel.edSearchChange(Sender: TObject);
var
  I, iPos, iEnd : Integer;
  Result : Boolean;
  sSearchName,
  sSearchNameNoExt,
  sSearchExt : String;
begin
  if edtSearch.Text='' then Exit;
  //DebugLn('edSearchChange: '+ edSearch.Text);

  sSearchName := AnsiLowerCase(edtSearch.Text);

  if Pos('.', sSearchName) <> 0 then
    begin
      sSearchNameNoExt := ExtractOnlyFileName(sSearchName);
      sSearchExt := ExtractFileExt(sSearchName);
      if not gQuickSearchMatchBeginning then
        sSearchNameNoExt := '*' + sSearchNameNoExt;
      if not gQuickSearchMatchEnding then
        sSearchNameNoExt := sSearchNameNoExt + '*';
      sSearchName := sSearchNameNoExt + sSearchExt + '*';
    end
  else
    begin
      if not gQuickSearchMatchBeginning then
        sSearchName := '*' + sSearchName;
      sSearchName := sSearchName + '*';
    end;

  DebugLn('sSearchName = ', sSearchName);

  I := dgPanel.Row; // start search from current cursor position
  iPos := I;        // save cursor position
  if not (fNext or fPrevious) then fSearchDirect := True;
  if fSearchDirect then
    begin
      if fNext then
        I := edtSearch.Tag + 1; // begin search from next file
      iEnd := dgPanel.RowCount;
    end
  else
    begin
      if fPrevious then
        I := edtSearch.Tag - 1; // begin search from previous file
      iEnd := dgPanel.FixedRows;
    end;
  if I < 1 then I := 1;
  

  
  while I <> iEnd do
    begin
      Result := MatchesMask(AnsiLowerCase(pnlFile.GetReferenceItemPtr(I-1)^.sName), sSearchName);

      if Result then
        begin
          dgPanel.Row := I;
          MakeVisible(I);
          edtSearch.Tag := I;
          Exit;
        end;
      if fSearchDirect then
        Inc(I)
      else
        Dec(I);
      // if not Next or Previous then search from beginning of list
      // to cursor position
      if (not(fNext or fPrevious)) and (I = iEnd) then
        begin
          I := 1;
          iEnd := iPos;
		  iPos := 1;
        end;
    end; // while
  fNext := False;
  fPrevious := False;
end;

procedure TFrameFilePanel.CloseAltPanel;
begin
  pnAltSearch.Visible:=False;
  edtSearch.Text:='';
end;

procedure TFrameFilePanel.ShowAltPanel(Char : Char);
begin
  pnAltSearch.Top := dgPanel.Top + dgPanel.Height;
  pnAltSearch.Left := dgPanel.Left;
  pnAltSearch.Visible := True;
  edtSearch.SetFocus;
  edtSearch.Tag := 0; // save current search position
  fSearchDirect := True; // set search direction
  fNext := False;
  fPrevious := False;
  edtSearch.Text := Char;
  edtSearch.SelStart := Length(edtSearch.Text) + 1;
end;

procedure TFrameFilePanel.UnMarkAll;
begin
  pnlFile.MarkAllFiles(False);
  dgPanel.Invalidate;
  UpDatelblInfo;
end;

procedure TFrameFilePanel.UpDatelblInfo;
begin
  with pnlFile do
  begin
    UpdateCountStatus;
    lblLInfo.Caption:=Format(rsMsgSelected,
      [cnvFormatFileSize(SizeSelected), cnvFormatFileSize(SizeInDir) ,FilesSelected, FilesInDir ]);
  end;
end;


procedure TFrameFilePanel.MarkAll;
begin
  pnlFile.MarkAllFiles(True);
  dgPanel.Invalidate;
  UpDatelblInfo;
end;

Function TFrameFilePanel.GetActiveDir:String;
begin
  Result:=pnlFile.ActiveDir;
end;


procedure TFrameFilePanel.MarkPlus;
var
  s:String;
begin
  s:=FLastMark;
  if not ShowInputComboBox(rsMarkPlus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark:=s;
  pnlFile.MarkGroup(s,True);
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.MarkShiftPlus;
begin
  with GetActiveItem^ do
  begin
    pnlFile.MarkGroup('*.'+sExt, True);
    dgPanel.Invalidate;
  end;
end;

procedure TFrameFilePanel.MarkShiftMinus;
begin
  with GetActiveItem^ do
  begin
    pnlFile.MarkGroup('*.'+sExt ,False);
    dgPanel.Invalidate;
  end;
end;

procedure TFrameFilePanel.MarkMinus;
var
  s:String;
begin
  s:=FLastMark;
  if not ShowInputComboBox(rsMarkMinus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark:=s;
  pnlFile.MarkGroup(s,False);
  dgPanel.Invalidate;
//  pnlFile.UpdatePanel;
end;

procedure TFrameFilePanel.edtPathKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#27 then
  begin
    edtPath.Visible:=False;
    SetFocus;
  end;
  if Key=#13 then
  begin
    Key:=#0; // catch the enter
    //if DirectoryExists(edtPath.Text) then
      begin
        pnlFile.ActiveDir:=edtPath.Text;
        LoadPanel;
        edtPath.Visible:=False;
        RefreshPanel;
        SetFocus;
      end;
  end;
end;

procedure TFrameFilePanel.edtRenameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#27 then
  begin
    edtRename.Visible:=False;
    UnMarkAll;
    SetFocus;
  end;
  if Key=#13 then
  begin
    Key:=#0; // catch the enter
    RenameFile(edtRename.Hint, ExtractFilePath(edtRename.Hint)+edtRename.Text);
    edtRename.Visible:=False;
    pnlFile.LastActive:=edtRename.Text;
    RefreshPanel;
    SetFocus;
  end;
end;

(*procedure TFrameFilePanel.HeaderSectionClick(
  HeaderControl: TCustomHeaderControl; Section: TCustomHeaderSection);
begin
  pnlFile.SortDirection:= not pnlFile.SortDirection;
  pnlFile.SortByCol(Section.Index{ Column.Index});
  dgPanel.Invalidate;
end; *)

procedure TFrameFilePanel.dgPanelDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  tw, cw:Integer;
  newColor:TColor;
  s:String;
  frp:PFileRecItem;
  bmp:TBitmap;
  iTextTop : Integer;
begin
{  (Sender as TDrawGrid).Canvas.TextOut(Rect.Left, Rect.Top, IntToStr(ARow));
  Exit;}

  iTextTop := Rect.Top + (gIconsSize div 2) - (dgPanel.Canvas.TextHeight('Pp') div 2);

  if dgPanel.FixedRows <> Integer(gTabHeader) then
    dgPanel.FixedRows := Integer(gTabHeader);

  if (ARow = 0) and gTabHeader then
  begin
    // Draw fixed header
    if not (ACol in [0..ColSet.GetColumnSet(ActiveColm).ColumnsCount-1]) then Exit;
    with dgPanel do
    begin
      tw := 0;
      s := ColSet.GetColumnSet(ActiveColm).GetColumnTitle(ACol);
      if ACol = pnlFile.SortColumn then
        begin
          tw := 1;
          if pnlFile.SortDirection then
            s := s + ' <'
          else
            s := s + ' >';
        end;
      if gCutTextToColWidth then
        begin
          while Canvas.TextWidth(s)-(Rect.Right-Rect.Left)-4>0 do
            Delete(s,Length(s)-tw,1);
        end;
      Canvas.TextOut(Rect.Left + 4, iTextTop, s);
    end;
    Exit;
  end;

  if (ARow>=dgPanel.RowCount)or (ARow<0) then Exit;
  if (ACol>=dgPanel.ColCount)or (ACol<0) then Exit;
  frp:=pnlFile.GetReferenceItemPtr(ARow - dgPanel.FixedRows); // substract fixed rows (header)
  if not Assigned(frp) then Exit;

  with frp^, dgPanel do
  begin
    Canvas.Brush.Style:=bsSolid;
    if gdSelected in State then
      Canvas.Brush.Color:= gCursorColor
    else
      begin
        if (ARow mod 2) = 0 then
          Canvas.Brush.Color := gBackColor
        else
          Canvas.Brush.Color := gBackColor2;
      end;
    Canvas.FillRect(Rect);
    //Canvas.Font.Style:=[];
    newColor:=gColorExt.GetColorBy(sExt, sModeStr);
    if bSelected then
      Canvas.Font.Color:= gMarkColor
    else
    if (gdSelected in State) then
      Canvas.Font.Color:=gCursorText
    else
      Canvas.Font.Color:= NewColor;

    if ACol=0 then
      begin
        if (iIconID >= 0) and gShowIcons then
          begin
            PixMapManager.DrawBitmap(iIconID, Canvas, Rect);
          end;
          s:=ColSet.GetColumnSet(ActiveColm).GetColumnItemResultString(ACol,frp);
          if gCutTextToColWidth then
            begin
              while Canvas.TextWidth(s)-(Rect.Right-Rect.Left)-4>0 do
                Delete(s,Length(s),1);
            end;
          if gShowIcons then
            Canvas.TextOut(Rect.Left + gIconsSize + 2 ,iTextTop,s)
          else
            Canvas.TextOut(Rect.Left + 2 ,iTextTop,s);
      end
    else
      begin
        //------------------------------------------------------
        s:=ColSet.GetColumnSet(ActiveColm).GetColumnItemResultString(ACol,frp);
        if gCutTextToColWidth then
          begin
            while Canvas.TextWidth(s)-(Rect.Right-Rect.Left)-4>0 do
              Delete(s,Length(s),1);
          end;
        if ColSet.GetColumnSet(ActiveColm).GetColumnAlign(ACol) = taRightJustify then
          begin
            cw:=ColWidths[ACol];
            tw:=Canvas.TextWidth(s);
            Canvas.TextOut(Rect.Left+cw-tw,iTextTop,s);
          end
        else
          Canvas.TextOut(Rect.Left,iTextTop,s);
        //------------------------------------------------------
      end;
  end;   //with
end;

procedure TFrameFilePanel.MakeVisible(iRow:Integer);
{var
  iNewTopRow:Integer;}
begin
  with dgPanel do
  begin
    if iRow<TopRow then
      TopRow:=iRow;
    if iRow>TopRow+VisibleRowCount then
      TopRow:=iRow-VisibleRowCount;
  end;
end;

procedure TFrameFilePanel.dgPanelExit(Sender: TObject);
begin
//  DebugLn(Self.Name+'.dgPanelExit');
//  edtRename.OnExit(Sender);        // this is hack, because onExit is NOT called
{  if pnAltSearch.Visible then
    CloseAltPanel;}
  lblLPath.Color:=clBtnFace;
  lblLPath.Font.Color:=clBlack;
  ClearGridSelection;
end;

procedure TFrameFilePanel.MakeSelectedVisible;
begin
  if dgPanel.Row>=0 then
    MakeVisible(dgPanel.Row);
end;

function TFrameFilePanel.AnySelected:Boolean;
begin
  Result:=dgPanel.Row>=0;
end;

procedure TFrameFilePanel.ClearGridSelection;
var
  nilRect:TGridRect;
begin
  FLastSelect:=dgPanel.Selection;
  nilRect.Left:=-1;
  nilRect.Top:=-1;
  nilRect.Bottom:=-1;
  nilRect.Right:=-1;
  dgPanel.Selection:=nilRect;
end;

procedure TFrameFilePanel.dgPanelDblClick(Sender: TObject);
var
  Point : TPoint;
  iRow, iCol : Integer;
begin
  Point:= dgPanel.ScreenToClient(Mouse.CursorPos);
  dgPanel.MouseToCell(Point.X, Point.Y, iCol, iRow);
  if iRow < dgPanel.FixedRows then Exit;

  Screen.Cursor:=crHourGlass;
  try
    pnlFile.ChooseFile(pnlFile.GetActiveItem{(false)});
    UpDatelblInfo;
  finally
    dgPanel.Invalidate;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TFrameFilePanel.dgPanelEnter(Sender: TObject);
begin
//  DebugLn(Self.Name+'.OnEnter');
  CloseAltPanel;
//  edtRename.OnExit(Sender);        // this is hack, bacause onExit is NOT called
  SetFocus;
  UpDatelblInfo;
end;

procedure TFrameFilePanel.RedrawGrid;
begin
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.dgPanelKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SHIFT: begin
      FLastSelectionStartRow := -1;
    end;
  end;
end;

procedure TFrameFilePanel.dgPanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_Insert then
  begin
    SelectFile(GetActiveItem);
//    dgPanel.Invalidate(ListView.Items[ListView.Selected.Index].DisplayRect, True);
//    dgPanel.Invalidate;
    if dgPanel.Row<dgPanel.RowCount-1 then
      dgPanel.Row:=dgPanel.Row+1;
    MakeSelectedVisible;
    dgPanel.Invalidate;
    Exit;
  end;

  if Key=VK_MULTIPLY then
  begin
    InvertAllFiles;
    Exit;
  end;

  if Key=VK_ADD then
  begin
    if shift=[ssCtrl] then
      MarkAll;
    if shift=[] then
      MarkPlus;
    if shift=[ssShift] then
      MarkShiftPlus;
    Exit;
  end;

  if Key=VK_SUBTRACT then
  begin
    if shift=[ssCtrl] then
      UnMarkAll;

    if shift=[] then
      MarkMinus;
    if shift=[ssShift] then
      MarkShiftMinus;
    Exit;
  end;
end;

function TFrameFilePanel.GetActiveItem:PFileRecItem;
begin
  Result:=pnlFile.GetActiveItem;
end;

procedure TFrameFilePanel.lblLPathMouseEnter(Sender: TObject);
begin
  lblLPath.Font.Color:=clRed;
  lblLPath.Font.Style:=[fsUnderline];
end;

procedure TFrameFilePanel.lblLPathMouseLeave(Sender: TObject);
begin
  if lblLPath.Color=clHighlight then
    lblLPath.Font.Color:=clHighlightText
  else
    lblLPath.Font.Color:=clBlack;
  lblLPath.Font.Style:=[];
end;

procedure TFrameFilePanel.pnlHeaderResize(Sender: TObject);
begin
  lblLPath.Width:=pnlHeader.Width - 4;
end;



constructor TFrameFilePanel.Create(AOwner : TWinControl; lblDriveInfo : TLabel; lblCommandPath:TLabel; cmbCommand:TComboBox);
var
  x:Integer;
begin
  DebugLn('TFrameFilePanel.Create components');
  inherited Create(AOwner);
  Parent:=AOwner;
  Align:=alClient;
  OnKeyPress:=@dgPanelKeyPress;
  pnlHeader:=TPanel.Create(Self);
  pnlHeader.Parent:=Self;
  pnlHeader.Height:=24;
  pnlHeader.Align:=alTop;

//  pnlHeader.Width:=AOwner.Width;
  
  pnlHeader.BevelInner:=bvNone;
  pnlHeader.BevelOuter:=bvNone;

//  pnlHeader.Color:=clRed;

  lblLPath:=TLabel.Create(pnlHeader);
  lblLPath.Parent:=pnlHeader;
  lblLPath.Top := 2;
  lblLPath.AutoSize:=False;
  lblLPath.Width:=pnlHeader.Width - 4;
  lblLPath.Color:=clActiveCaption;

  edtPath:=TEdit.Create(lblLPath);
  edtPath.Parent:=pnlHeader;
  edtPath.Visible:=False;

  pnlFooter:=TPanel.Create(Self);
  pnlFooter.Parent:=Self;
  pnlFooter.Align:=alBottom;

  pnlFooter.Width:=AOwner.Width;
  pnlFooter.Anchors:=[akLeft, akRight, akBottom];
  pnlFooter.Height:=20;
  pnlFooter.Top:=Height-20;

  pnlFooter.BevelInner:=bvNone;
  pnlFooter.BevelOuter:=bvNone;;

  FLastSelectionStartRow:=-1;

  dgPanel:=TDrawGrid.Create(Self);
  dgPanel.Parent:=Self;
  dgPanel.FixedCols:=0;
  dgPanel.FixedRows:=0;
  dgPanel.DefaultDrawing:=True;
  dgPanel.Width:=Self.Width;


//  dgPanel.Height:=Self.Height - pnlHeader.Height - pnlFooter.Height;
//  DebugLn(Self.Height - pnlHeader.Height - pnlFooter.Height);
  dgPanel.Align:=alClient;
//  dgPanel.DefaultDrawing:=False;
//------------------------------------------------------
//  Colm:=TPanelColumnsClass.Create;
//  ColSet.GetColumnSet(ActiveColm).Load(gIni);
//  dgPanel.ColCount:=ColSet.GetColumnSet(ActiveColm).ColumnsCount;
//------------------------------------------------------
  dgPanel.Options:=[goFixedVertLine, goFixedHorzLine, goTabs, goRowSelect, goColSizing, goHeaderHotTracking, goHeaderPushedLook];
  dgPanel.TitleStyle := tsStandard;
  dgPanel.TabStop:=False;

  lblLInfo:=TLabel.Create(pnlFooter);
  lblLInfo.Parent:=pnlFooter;
  lblLInfo.Width:=250;//  pnlFooter.Width;
  lblLInfo.AutoSize:=True;

  edtRename:=TEdit.Create(dgPanel);
  edtRename.Parent:=dgPanel;
  edtRename.Visible:=False;

  // now create search panel
  pnAltSearch:=TPanel.Create(Self);
  pnAltSearch.Parent:=Self;
  pnAltSearch.Height:=20;
  pnAltSearch.Width:=185;
  pnAltSearch.Caption:='Find:'; //localize
  pnAltSearch.Alignment:=taLeftJustify;
  
  edtSearch:=TEdit.Create(pnAltSearch);
  edtSearch.Parent:=pnAltSearch;
  edtSearch.Width:=118;
  edtSearch.Left:=64;
  edtSearch.Top:=1;
  edtSearch.Height:=18;

  pnAltSearch.Visible := False;
  
  // ---
  dgPanel.OnMouseDown := @dgPanelMouseDown;
  dgPanel.OnStartDrag := @dgPanelStartDrag;
  dgPanel.OnDragOver := @dgPanelDragOver;
  dgPanel.OnDblClick:=@dgPanelDblClick;
  dgPanel.OnDrawCell:=@dgPanelDrawCell;
  dgPanel.OnEnter:=@dgPanelEnter;
  dgPanel.OnExit:=@dgPanelExit;
  dgPanel.OnKeyUp:=@dgPanelKeyUp;
  dgPanel.OnKeyDown:=@dgPanelKeyDown;
  dgPanel.OnKeyPress:=@dgPanelKeyPress;
  dgPanel.OnHeaderClick:=@dgPanelHeaderClick;
  dgPanel.OnPrepareCanvas:=@dgPanelPrepareCanvas;
  {Alexx2000}
  dgPanel.OnMouseWheelUp := @dgPanelMouseWheelUp;
  dgPanel.OnMouseWheelDown := @dgPanelMouseWheelDown;
  {/Alexx2000}
  edtSearch.OnChange:=@edSearchChange;
  edtSearch.OnKeyPress:=@edSearchKeyPress;
  edtSearch.OnKeyDown:=@edSearchKeyDown;
  edtPath.OnKeyPress:=@edtPathKeyPress;
  edtRename.OnKeyPress:=@edtRenameKeyPress;

  pnlHeader.OnResize := @pnlHeaderResize;

  lblLPath.OnMouseEnter:=@lblLPathMouseEnter;
  lblLPath.OnMouseLeave:=@lblLPathMouseLeave;

  
  pnlFile:=TFilePanel.Create(AOwner, dgPanel,lblLPath,lblCommandPath, lblDriveInfo, cmbCommand);
  
//  setup column widths
////  dgPanel.ColCount:=ColSet.GetColumnSet(ActiveColm).ColumnsCount;
// if ColSet.GetColumnSet(ActiveColm).ColumnsCount>0 then
//  for x:=0 to ColSet.GetColumnSet(ActiveColm).ColumnsCount-1 do
//    dgPanel.ColWidths[x]:=ColSet.GetColumnSet(ActiveColm).GetColumnWidth(x);
  SetColWidths;
end;

destructor TFrameFilePanel.Destroy;
begin
  if assigned(pnlFile) then
    FreeAndNil(pnlFile);
  //---------------------
//  if assigned(Colm) then
//     Colm.Free;
  //---------------------
    
  inherited Destroy;
end;


end.
