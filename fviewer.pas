{
Seksi Commander
----------------------------
Integrated viewer form

Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

contributors:
  Radek Polak


ported to lazarus:


 changes:
 23.7.
   - fixed: scroll bar had wrong max value until user pressed key (by Radek Polak)
   - fixed: wrong scrolling with scroll bar - now look at ScrollBarVertScroll (by Radek Polak) 
}

unit fViewer;
{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, fLngForm, Menus,
  viewercontrol, fFindView;

type
  TfrmViewer = class(TfrmLng)
    Image: TImage;
    miSeparator: TMenuItem;
    miSavePos: TMenuItem;
    nbPages: TNotebook;
    pgText: TPage;
    pgImage: TPage;
    ScrollBarVert: TScrollBar;
    ScrollBox: TScrollBox;
    Status: TStatusBar;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miPrev: TMenuItem;
    miNext: TMenuItem;
    miView: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    miImage: TMenuItem;
    miStretch: TMenuItem;
    miText: TMenuItem;
    miBin: TMenuItem;
    miHex: TMenuItem;
    miWrapText: TMenuItem;
    miAbout: TMenuItem;
    miAbout2: TMenuItem;
    miDiv1: TMenuItem;
    miSearch: TMenuItem;
    miDiv2: TMenuItem;
    miGraphics: TMenuItem;
    miEdit: TMenuItem;
    miSelectAll: TMenuItem;
    miCopyToClipboard: TMenuItem;
    ViewerControl: TViewerControl;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ViewerControlMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ViewerControlMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure frmViewerClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmViewerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure frmViewerKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miExitClick(Sender: TObject);
    procedure miNextClick(Sender: TObject);
    procedure miPrevClick(Sender: TObject);
    procedure miSavePosClick(Sender: TObject);
    procedure miStretchClick(Sender: TObject);
    procedure miTextClick(Sender: TObject);
    procedure miBinClick(Sender: TObject);
    procedure miHexClick(Sender: TObject);
    procedure miWrapTextClick(Sender: TObject);
    procedure miAbout2Click(Sender: TObject);
    procedure miSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miProcessClick(Sender: TObject);
    procedure miGraphicsClick(Sender: TObject);
    procedure miCopyToClipboardClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure ScrollBarVertScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    { Private declarations }
    sList:TStringList;
    iActiveFile:Integer;
    bImage:Boolean;
    FFindDialog:TfrmFindView;
    procedure UpDateScrollBar;
    Function CheckGraphics(const sFileName:String):Boolean;
    procedure LoadGraphics(const sFileName:String);
    procedure DoSearch;
  public
    procedure LoadLng; override;

    procedure LoadFile(iIndex:Integer);
    procedure ReMmapIfNeed;
  end;


procedure ShowViewer(sl:TStringList);

implementation

uses
  uLng, uShowMsg, uGlobs, lcltype, lazjpeg{$IFNDEF WIN32}, uFindMmap{$ENDIF} ;

procedure ShowViewer(sl:TStringList);
var
  x:Integer;
begin
//  writeln('ShowViewer - Using Internal');

  With TfrmViewer.Create(Application) do
  begin
//    writeln('ShowViewer - Using Internal - created');
    Left:=gViewerPos.Left;
    Top:=gViewerPos.Top;
    Width:=gViewerPos.Right;
    Height:=gViewerPos.Bottom;
    try
      sList:=TStringList.Create;
//      writeln('ShowViewer - Using Internal - before assign');
      for x:=0 to sl.Count-1 do
      begin
        writeln('Viewing:',sl.Strings[x]);
        sList.Add(sl.Strings[x]);
      end;
//      writeln('ShowViewer - Using Internal - after assign');
      

      ViewerControl.ViewerMode:=vmText;
//      miProcess.Checked:=False;
      LoadFile(0);
      Show;//Modal;
    finally
//      Free;
    end;
  end;
end;

procedure TfrmViewer.LoadLng;
begin
// load language
//  writeln('TfrmViewer.LoadLng');
//  ViewerControl.Font.Assign(gViewerFont);
  miFile.Caption:=   lngGetString(clngEditFile);
  miSavePos.Caption:= lngGetString(clngSavePosition);
  miPrev.Caption:=  lngGetString(clngEditPrev);
  miNext.Caption:=    lngGetString(clngEditNext);
  miExit.Caption:=   lngGetString(clngEditExit);
  miImage.Caption:= lngGetString(clngEditImage);
  miStretch.Caption:= lngGetString(clngEditStretch);

  miView.Caption:= lngGetString(clngViewView);
  miText.Caption:= lngGetString(clngViewText);
  miBin.Caption:= lngGetString(clngViewBin);
  miHex.Caption:= lngGetString(clngViewHex);
  miWrapText.Caption:= lngGetString(clngViewWrap);
  miAbout.Caption:= lngGetString(clngViewAbout);
  miAbout2.Caption:= lngGetString(clngViewAbout);
  miSearch.Caption:= lngGetString(clngViewSearch);
//  miProcess.Caption:=lngGetString(clngViewProcFile);
//  Image.Picture.RegisterFileFormat('jpeg', 'jpeg', TBitmap);
  miEdit.Caption:=lngGetString(clngEditEdit);
  miGraphics.Caption:=lngGetString(clngViewGraphics);
  miCopyToClipboard.Caption:=lngGetString(clngViewCpClip);
  miSelectAll.Caption:=lngGetString(clngViewSelectAll);
  ViewerControl.Color:=clWindow;
end;

procedure TfrmViewer.LoadFile(iIndex:Integer);
begin
//  writeln('Viewer: LoadFile:',iIndex);
  iActiveFile:=iIndex;
  Caption:=sList.Strings[iIndex];
  Screen.Cursor:=crHourGlass;
  try
//    writeln('View: BeforeCheckGraphics:',iIndex);
    if CheckGraphics(sList.Strings[iIndex]) then
    begin
//      writeln('View: LoadGraphics:',iIndex);
      LoadGraphics(sList.Strings[iIndex]);
    end
    else
    begin
//      writeln('View: LoadIntoViewer:',iIndex);

      miImage.Visible:=False;
      miEdit.Visible:=True;
      bImage:=False;
      nbPages.ActivePageComponent:=pgText;
      ViewerControl.UnMapFile; // if any mapped
//      miProcess.Click;
      ViewerControl.MapFile(sList.Strings[iIndex]);     //handled by miProcess.Click
      UpDateScrollBar;
    end;
    Status.Panels[0].Text:=sList.Strings[iIndex];
    Status.Panels[1].Text:=Format('%d/%d',[iIndex+1,slist.Count]);
  finally
    Screen.Cursor:=crDefault;
  end;
end;



procedure TfrmViewer.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (key='N') or (key='n') then
    miNextClick(Sender);

  if (key='P') or (key='p') then
    miPrevClick(Sender);


  if Key='1' then
    ViewerControl.ViewerMode:=vmText;
  if Key='2' then
    ViewerControl.ViewerMode:=vmBin;
  if Key='3' then
    ViewerControl.ViewerMode:=vmHex;
  if Key='4' then
    ViewerControl.ViewerMode:=vmWrap;

end;

procedure TfrmViewer.ViewerControlMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ViewerControl.DownBy(3);
  Handled:=True;
end;

procedure TfrmViewer.ViewerControlMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ViewerControl.UpBy(3);
  Handled:=True;
end;

procedure TfrmViewer.frmViewerClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TfrmViewer.frmViewerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=VK_Q) then Close;
  if bImage then Exit;
  // now handle shortcuts to viewer
  if Shift<>[] then Exit;
  if Key=VK_Down then
    ViewerControl.Down;
  if Key=VK_Up then
    ViewerControl.Up;
  if Key=VK_Home then
    ViewerControl.GoHome;
  if Key=VK_End then
    ViewerControl.GoEnd;
  if Key=VK_PRIOR then
    ViewerControl.PageUp;
  if Key=VK_NEXT then
    ViewerControl.PageDown;
    
  if (Key=VK_F3) or ((Key=VK_F) and (Shift=[ssCtrl])) then
    DoSearch;

  UpDateScrollBar;
end;

procedure TfrmViewer.frmViewerKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=27 then
  begin
    Key:=0;
    Close;
  end;
end;

procedure TfrmViewer.miExitClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfrmViewer.miNextClick(Sender: TObject);
begin
  inherited;
  if iActiveFile+1>=sList.Count then
    LoadFile(0)
  else
    LoadFile(iActiveFile+1);
end;

procedure TfrmViewer.miPrevClick(Sender: TObject);
begin
  inherited;
  if iActiveFile>0 then
    LoadFile(iActiveFile-1)
  else
    LoadFile(sList.Count-1);
end;

procedure TfrmViewer.miSavePosClick(Sender: TObject);
begin
  gViewerPos.Left:=Left;
  gViewerPos.Top:=Top;
  gViewerPos.Bottom:=Height;
  gViewerPos.Right:=Width;
  msgOK(lngGetString(clngPositionSaved));
end;

procedure TfrmViewer.miStretchClick(Sender: TObject);
begin
  miStretch.Checked:=not miStretch.Checked;
  Image.Stretch:=miStretch.Checked;
end;


procedure TfrmViewer.miTextClick(Sender: TObject);
begin
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmText;
end;

procedure TfrmViewer.miBinClick(Sender: TObject);
begin
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmBin;
end;

procedure TfrmViewer.miHexClick(Sender: TObject);
begin
  inherited;
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmHex;
end;

procedure TfrmViewer.miWrapTextClick(Sender: TObject);
begin
  inherited;
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmWrap;
end;

procedure TfrmViewer.miAbout2Click(Sender: TObject);
begin
  MsgOK(lngGetString(clngViewAboutText));
end;

procedure TfrmViewer.miSearchClick(Sender: TObject);
begin
  DoSearch;
end;

procedure TfrmViewer.FormCreate(Sender: TObject);
begin
//  writeln('TfrmViewer.FormCreate');
  inherited;
  FFindDialog:=nil; // dialog is created in first use
{  Status.Panels[0].Width:=50;
  Status.Panels[1].Width:=50;}

//  writeln('TfrmViewer.FormCreate done');
end;

procedure TfrmViewer.FormDestroy(Sender: TObject);
begin
  if assigned(FFindDialog) then
     FreeAndNil(FFindDialog);
  inherited;
end;

procedure TfrmViewer.miProcessClick(Sender: TObject);
var
  sViewCmd:String;
  sCurrName:String;
begin
{  writeln('TfrmViewer.miProcessClick');
  inherited;
  miEdit.Visible:=True;
  if not miProcess.Checked then
  begin
//    if ViewerControl.DataAccess=dtNothing then
    ViewerControl.MapFile(sList.Strings[iActiveFile]);
    miProcess.Checked:=not miProcess.Checked;
  end
  else
  begin
    sCurrName:=sList.Strings[iActiveFile];
    sViewCmd:=gExts.GetCommandText(lowercase(ExtractFileExt(sCurrName)),'view');
    if (sViewCmd='') then Exit;
    sViewCmd:=Copy(sViewCmd, pos('=',sViewCmd)+1, length(sViewCmd));
    // i known about range, but Copy correct this "bug"

    sViewCmd:=StringReplace(sViewCmd,'%f',ExtractFileName(sCurrName),[rfReplaceAll]);
    sViewCmd:=StringReplace(sViewCmd,'%d',ExtractFilePath(sCurrName),[rfReplaceAll]);
    sViewCmd:=Trim(StringReplace(sViewCmd,'%p',sCurrName,[rfReplaceAll]));

    ViewerControl.UnMapFile;
    Status.Panels[2].Text:=IntToStr(ViewerControl.FileSize);
    Status.Panels[3].Text:=sViewCmd;
    UpDateScrollBar;
    miProcess.Checked:=not miProcess.Checked;
  end;
}
end;

procedure TfrmViewer.ReMmapIfNeed;
begin
//  writeln('TfrmViewer.RemmapIfneed');
  if bImage then
  begin
    bImage:=False;
    ViewerControl.MapFile(sList.Strings[iActiveFile]);
    miImage.Visible:=False;
    miEdit.Visible:=True;
    bImage:=False;
    nbPages.ActivePageComponent:=pgText;
    image.Picture:=nil;
  end;
  Status.Panels[2].Text:=IntToStr(ViewerControl.FileSize);
  Status.Panels[3].Text:='';
  UpDateScrollBar;
end;

procedure TfrmViewer.UpDateScrollBar;
begin
//  writeln('TfrmViewer.Update scrollbar');
  if ScrollBarVert.Min<>0 then
    ScrollBarVert.Min:=0;
  if ScrollBarVert.Max<>ViewerControl.FileSize then
    ScrollBarVert.Max:=ViewerControl.FileSize;
  if ScrollBarVert.Position<> ViewerControl.Position then
    ScrollBarVert.Position:=ViewerControl.Position;
end;

procedure TfrmViewer.miGraphicsClick(Sender: TObject);
begin
  inherited;
  if CheckGraphics(sList.Strings[iActiveFile]) then
    LoadGraphics(sList.Strings[iActiveFile]);
end;

Function TfrmViewer.CheckGraphics(const sFileName:String):Boolean;
var
  sExt:String;
begin
//  writeln('TfrmViewer.CheckGraphics');
  sExt:=Lowercase(ExtractFileExt(sFileName));
  Result:=(sExt='.bmp') or (sExt='.xpm') or (sExt='.png') or
       (sExt='.jpg') or (sExt='.jpeg') or (sExt='.ico') or
       (sExt='.ddw') or (sExt='.tga');
end;

procedure TfrmViewer.LoadGraphics(const sFileName:String);
begin
//  writeln('TfrmViewer.Load graphics');
  Image.Stretch:=miStretch.Checked;
  Image.Picture.LoadFromFile(sFileName);
  if Image.Picture.Width<350 then
    Width:=350
  else
    Width:=Image.Picture.Width+10;
  if Image.Picture.Height<100 then
     Height:=100
  else
     Height:=Image.Picture.Height+Status.Height+10; // bulgarian constant
  nbPages.ActivePageComponent:=pgImage;
  miImage.Visible:=True;
  miEdit.Visible:=False;
//  miView.Visible:=False;// text modes
  bImage:=True;
end;

procedure TfrmViewer.DoSearch;
var
  PAdr:PChar;
  iSizeData:Integer;
begin
  inherited;
// fi
  PAdr:=ViewerControl.GetDataAdr; // begin of data in memory
  inc(PAdr,ViewerControl.Position); // move to current position
  iSizeData:=ViewerControl.FileSize - ViewerControl.Position;
  if iSizeData<=0 then Exit;
// in first use create dialog
  if not assigned(FFindDialog) then
     FFindDialog:=TfrmFindView.Create(Application);
  if FFindDialog.ShowModal <> mrOK then Exit;
  if FFindDialog.cbDataToFind.Text='' then Exit;
  {$IFNDEF WIN32} // Alexx2000 сделать позже поиск
  PAdr:=PosMem(PAdr, iSizeData, FFindDialog.cbDataToFind.Text, FFindDialog.cbCaseSens.Checked);
  {$ENDIF}
  if (Integer(PAdr)<>-1) then
  begin
// founded, set position to ViewerControl
    ViewerControl.Position:=Integer(PAdr)-Integer(ViewerControl.GetDataAdr);
//    ViewerControl.Up;
// position is property and have write method  (repaint widget)
     UpDateScrollBar;
  end;
  SetFocus;
end;


procedure TfrmViewer.miCopyToClipboardClick(Sender: TObject);
begin
  ViewerControl.CopyToClipboard;
end;

procedure TfrmViewer.miSelectAllClick(Sender: TObject);
begin
  inherited;
  ViewerControl.SelectAll;
end;

procedure TfrmViewer.ScrollBarVertScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  inherited;
  // RADEK
  case ScrollCode of
    scLineUp:
    begin
      ViewerControl.Up;
      ScrollPos := ViewerControl.Position;
    end;
    scLineDown:
    begin
      ViewerControl.Down;
      ScrollPos := ViewerControl.Position;
    end;
    scPageUp:
    begin
      ViewerControl.PageUp;
      ScrollPos := ViewerControl.Position;
    end;
    scPageDown:
    begin
      ViewerControl.PageDown;
      ScrollPos := ViewerControl.Position;
    end;
    scTop: ViewerControl.GoHome;
    scBottom: ViewerControl.GoEnd;
    scPosition:
    begin
      ViewerControl.Position := ScrollPos;
      if ViewerControl.UpLine then ViewerControl.DownLine;
      ScrollPos := ViewerControl.Position;
    end;
  end;
end;

initialization
 {$I fViewer.lrs}
end.
