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

   Dmitry Kolomiets
   15.03.08
   changes:
     - Added WLX api support (TC WLX api v 1.8)

}

unit fViewer;

{$mode objfpc}{$H+}

interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, LCLProc, Menus,
  viewercontrol, fFindView,uwlxmodule;

type

  { TfrmViewer }

  TfrmViewer = class(TForm)
    Image: TImage;
    pmiSelectAll: TMenuItem;
    miDiv5: TMenuItem;
    pmiCopy: TMenuItem;
    miDiv3: TMenuItem;
    miEncoding: TMenuItem;
    miPlugins: TMenuItem;
    miSeparator: TMenuItem;
    miSavePos: TMenuItem;
    nbPages: TNotebook;
    pnlLister: TPanel;
    pgText: TPage;
    pgImage: TPage;
    pmEditMenu: TPopupMenu;
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
    procedure FormCreate(Sender : TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure miPluginsClick(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure ViewerControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    procedure FormDestroy(Sender: TObject);
    procedure miProcessClick(Sender: TObject);
    procedure miGraphicsClick(Sender: TObject);
    procedure miCopyToClipboardClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miChangeEncodingClick(Sender:TObject);
    procedure ScrollBarVertScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
  private
    { Private declarations }
    FileList: TStringList;
    iActiveFile:Integer;
    bImage,
    bPlugin: Boolean;
    FFindDialog:TfrmFindView;
    FDeleteAfterView : Boolean;
    //---------------------
    WlxPlugins:TWLXModuleList;
    ActivePlugin:Integer;
    //---------------------
    function CheckPlugins(Index:integer; Force: boolean=false):boolean;
    procedure ExitPluginMode;
    procedure UpDateScrollBar;
    Function CheckGraphics(const sFileName:String):Boolean;
    procedure AdjustImageSize;
    procedure LoadGraphics(const sFileName:String);
    procedure DoSearch;
    procedure ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
  public
    procedure LoadFile(iIndex:Integer);
    procedure ReMmapIfNeed;
  end;


procedure ShowViewer(const FilesToView:TStringList; bDeleteAfterView : Boolean = False);

implementation

uses
  uLng, uShowMsg, uGlobs, LCLType, LConvEncoding, uClassesEx, uFindMmap, uDCUtils,
  uOSUtils;

procedure ShowViewer(const FilesToView:TStringList; bDeleteAfterView : Boolean = False);
var viewer: TfrmViewer;
begin
  //DebugLn('ShowViewer - Using Internal');
  viewer := TfrmViewer.Create(Application);
  gViewerPos.Restore(viewer);
  viewer.FileList.Assign(FilesToView); // Make a copy of the list
  viewer.LoadFile(0);
  viewer.FDeleteAfterView := bDeleteAfterView;
  viewer.Show;
end;

procedure TfrmViewer.LoadFile(iIndex:Integer);
begin
//  DebugLn('Viewer: LoadFile:' + iIndex);
  iActiveFile:=iIndex;
  Caption:=FileList.Strings[iIndex];
  Screen.Cursor:=crHourGlass;
  try
    bPlugin:= CheckPlugins(iIndex);
    if bPlugin then
      Status.Panels[2].Text:= WlxPlugins.GetWLxModule(ActivePlugin).Name
//      DebugLn('View: BeforeCheckGraphics:' + iIndex);
    else if CheckGraphics(FileList.Strings[iIndex]) then
      begin
//        DebugLn('View: LoadGraphics:' + iIndex);
        LoadGraphics(FileList.Strings[iIndex]);
      end
    else
      begin
//        DebugLn('View: LoadIntoViewer:' + iIndex);

        miImage.Visible:=False;
        miEdit.Visible:=True;
        miEncoding.Visible:= True;
        bImage:=False;
        nbPages.ActivePageComponent:=pgText;
        ViewerControl.UnMapFile; // if any mapped
//        miProcess.Click;
        ViewerControl.MapFile(FileList.Strings[iIndex]);     //handled by miProcess.Click
        UpDateScrollBar;
        ChooseEncoding(miEncoding, ViewerControl.Encoding);
      end;
    Status.Panels[0].Text:=FileList.Strings[iIndex];
    Status.Panels[1].Text:=Format('%d/%d',[iIndex+1,FileList.Count]);
    Status.Panels[3].Text:= cnvFormatFileSize(ViewerControl.FileSize) + ' (100 %)';
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

function TfrmViewer.CheckPlugins(Index:integer; Force:boolean=false):boolean;
var
  I: Integer;
begin
  I:= 0;
  DebugLn('WlXPlugins.Count = ' + IntToStr(WlxPlugins.Count));
  while (I < WlxPlugins.Count) do
   if WlxPlugins.GetWLxModule(I).FileParamVSDetectStr(FileList[Index]) then
     begin
       Result:= True;
       DebugLn('I = '+IntToStr(I));
       nbPages.Visible:= False;
       if not WlxPrepareContainer(pnlLister.Handle) then {TODO: ERROR and exit;};
       WlxPlugins.LoadModule(I);
       DebugLn('WlxModule.Name = ', WlxPlugins.GetWLxModule(I).Name);
       if WlxPlugins.GetWLxModule(I).CallListLoad(pnlLister.Handle,FileList[Index], {TODO: showFlags}0) = 0 then
         begin
           WlxPlugins.GetWLxModule(I).UnloadModule;
           Inc(I);
           Continue;
         end;
       ActivePlugin:= I;
       Exit;
     end
   else  I:= I + 1;
 // Plugin not found
 nbPages.Visible:= True;
 ActivePlugin:= -1;
 Result:= False;
end;

procedure TfrmViewer.ExitPluginMode;
begin
  WlxPrepareContainer(pnlLister.Handle,true);
  if (WlxPlugins.Count > 0) and (ActivePlugin >= 0) then
    begin
      WlxPlugins.GetWLxModule(ActivePlugin).CallListCloseWindow;
      WlxPlugins.GetWLxModule(ActivePlugin).UnloadModule;
    end;
//  pnlLister.Hide;
  nbPages.Show;
end;

procedure TfrmViewer.miPluginsClick(Sender: TObject);
begin
  ViewerControl.UnMapFile; // if any mapped
  bPlugin:= CheckPlugins(iActiveFile, True);
  if bPlugin then
    Status.Panels[2].Text:= WlxPlugins.GetWLxModule(ActivePlugin).Name
  else
    ViewerControl.MapFile(FileList.Strings[iActiveFile]);
end;

procedure TfrmViewer.ScrollBoxResize(Sender: TObject);
begin
  if bImage then AdjustImageSize;
end;

procedure TfrmViewer.ViewerControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    pmEditMenu.PopUp();
end;

procedure TfrmViewer.ViewerControlMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ViewerControl.DownBy(3);
  UpDateScrollBar;
  Handled:=True;
end;

procedure TfrmViewer.ViewerControlMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ViewerControl.UpBy(3);
  UpDateScrollBar;
  Handled:=True;
end;

procedure TfrmViewer.frmViewerClose(Sender: TObject;
                                    var CloseAction: TCloseAction);
var
  I, Count : Integer;
begin
  // TODO: may be better automtic save
  // (see also TfrmViewer.miSavePosClick)
  CloseAction:=caFree;
  if not bImage then gViewerPos.Save(Self);
  gViewerImageStretch:= miStretch.Checked;
  ViewerControl.UnMapFile;
  if FDeleteAfterView then
    begin
      Count := FileList.Count - 1;
      //DebugLN('DeleteFile == ' + FileList.Strings[0]);
      for I := 0 to Count do
        mbDeleteFile(FileList.Strings[I]);
    end;
  if Assigned(WlxPlugins) then
     begin
       ExitPluginMode;
       FreeAndNil(WlxPlugins);
     end;
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
  begin
    DoSearch;
    Key:=0;
  end;
// To prevent editor open on key F4 in viewer
  if (Key=VK_F4) then  Key:=0;

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
  if iActiveFile+1>=FileList.Count then
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
    LoadFile(FileList.Count-1);
end;

procedure TfrmViewer.miSavePosClick(Sender: TObject);
begin
  // TODO: It really need? may be better automtic save
  gViewerPos.Save(Self);
  msgOK(rsPositionSaved);
end;

procedure TfrmViewer.miStretchClick(Sender: TObject);
begin
  miStretch.Checked:= not miStretch.Checked;
  Image.Stretch:= miStretch.Checked;
  Image.AutoSize:= not Image.Stretch;
  Image.Proportional:= Image.Stretch;
  AdjustImageSize;
end;

procedure TfrmViewer.miTextClick(Sender: TObject);
begin
  ExitPluginMode;
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmText;
end;

procedure TfrmViewer.miBinClick(Sender: TObject);
begin
  ExitPluginMode;
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmBin;
end;

procedure TfrmViewer.miHexClick(Sender: TObject);
begin
  inherited;
  ExitPluginMode;
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmHex;
end;

procedure TfrmViewer.miWrapTextClick(Sender: TObject);
begin
  inherited;
  ExitPluginMode;
  ReMmapIfNeed;
  ViewerControl.ViewerMode:=vmWrap;
end;

procedure TfrmViewer.miAbout2Click(Sender: TObject);
begin
  MsgOK(rsViewAboutText);
end;

procedure TfrmViewer.miSearchClick(Sender: TObject);
begin
  DoSearch;
end;

procedure TfrmViewer.FormCreate(Sender: TObject);
var
  I: Integer;
  mi: TMenuItem;
  EncodingsList: TStringList;
begin
//  DebugLn('TfrmViewer.FormCreate');
  ViewerControl.Color:= clWindow;
  ViewerControl.Font.Name:= gViewerFontName;
  ViewerControl.Font.Size:= gViewerFontSize;
  ViewerControl.Font.Style:= gViewerFontStyle;
  FileList := TStringList.Create;

  WlxPlugins:=TWLXModuleList.Create;
  WlxPlugins.Load(gIni);
  DebugLn('WLX: Load - OK');

  FFindDialog:=nil; // dialog is created in first use
  
{  Status.Panels[0].Width:=50;
  Status.Panels[1].Width:=50;}
  miStretch.Checked:= gViewerImageStretch;
// update menu encoding
  miEncoding.Clear;
  EncodingsList:= TStringList.Create;
  GetSupportedEncodings(EncodingsList);
  for I:= 0 to EncodingsList.Count - 1 do
    begin
      mi:= TMenuItem.Create(miEncoding);
      mi.Caption:= EncodingsList[I];
      mi.AutoCheck:= True;
      mi.RadioItem:= True;
      mi.GroupIndex:= 1;
      mi.OnClick:= @miChangeEncodingClick;
      miEncoding.Add(mi);
    end;
  EncodingsList.Free;
 // DebugLn('TfrmViewer.FormCreate done');
end;

procedure TfrmViewer.FormDestroy(Sender: TObject);
begin
  if assigned(WlxPlugins) then
     begin
        FreeAndNil(WlxPlugins);
     end;
  FileList.Free;
  if assigned(FFindDialog) then
     FreeAndNil(FFindDialog);
  inherited;
end;

procedure TfrmViewer.miProcessClick(Sender: TObject);
{var
  sViewCmd:String;
  sCurrName:String;}
begin
{  DebugLn('TfrmViewer.miProcessClick');
  inherited;
  miEdit.Visible:=True;
  if not miProcess.Checked then
  begin
//    if ViewerControl.DataAccess=dtNothing then
    ViewerControl.MapFile(FileList.Strings[iActiveFile]);
    miProcess.Checked:=not miProcess.Checked;
  end
  else
  begin
    sCurrName:=FileList.Strings[iActiveFile];
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
//  DebugLn('TfrmViewer.ReMmapIfNeed');
  if bImage or bPlugin then
  begin
    bImage:=False;
    bPlugin:= False;
    ViewerControl.MapFile(FileList.Strings[iActiveFile]);
    miImage.Visible:=False;
    miEdit.Visible:=True;
    miEncoding.Visible:= True;
    bImage:=False;
    nbPages.ActivePageComponent:=pgText;
    ChooseEncoding(miEncoding, ViewerControl.Encoding);
    image.Picture:=nil;
    Status.Panels[2].Text:= '0 (0 %)';
  end;
  Status.Panels[3].Text:= cnvFormatFileSize(ViewerControl.FileSize) + ' (100 %)';
  UpDateScrollBar;
end;

procedure TfrmViewer.UpDateScrollBar;
var
  iPercent: Integer;
begin
//  DebugLn('TfrmViewer.UpDateScrollBar');
  if ScrollBarVert.Min <> 0 then
    ScrollBarVert.Min:= 0;
  if ScrollBarVert.Max <> 100 then
    ScrollBarVert.Max:= 100;

  if ViewerControl.FileSize > 0 then
    begin
      iPercent:= ViewerControl.Percent;
      if (ScrollBarVert.Position <> iPercent) then
        begin
          ScrollBarVert.Position:= iPercent;
          Status.Panels[2].Text:= cnvFormatFileSize(ViewerControl.Position)+' ('+IntToStr(iPercent)+' %)';
        end;
    end;
end;

procedure TfrmViewer.miGraphicsClick(Sender: TObject);
begin
  inherited;
  nbPages.Show;
  if CheckGraphics(FileList.Strings[iActiveFile]) then
    begin
      ViewerControl.UnMapFile; // if any mapped
      LoadGraphics(FileList.Strings[iActiveFile]);
    end;
end;

Function TfrmViewer.CheckGraphics(const sFileName:String):Boolean;
var
  sExt:String;
begin
//  DebugLn('TfrmViewer.CheckGraphics');
  sExt:=Lowercase(ExtractFileExt(sFileName));
  Result:=(sExt='.bmp') or (sExt='.xpm') or (sExt='.png') or
       (sExt='.jpg') or (sExt='.jpeg') or (sExt='.ico') or
       (sExt='.ddw') or (sExt='.tga');
end;

// Adjust Image size (width and height) to ScrollBox size
procedure TfrmViewer.AdjustImageSize;
const
  fmtImageInfo = '%s (%s %%)';
var
  sResolution: String;
  iScale: Integer;
begin
  if Image.Stretch then
    begin
      Image.Width:= ScrollBox.ClientWidth;
      Image.Height:= ScrollBox.ClientHeight;
      // show image resolution and scale
      sResolution:= IntToStr(Image.ClientWidth) + 'x' + IntToStr(Image.ClientHeight);
      iScale:= (Image.ClientWidth * 100) div Image.Picture.Width;
      Status.Panels[2].Text:= Format(fmtImageInfo, [sResolution, IntToStr(iScale)]);
      sResolution:= IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
      Status.Panels[3].Text:= Format(fmtImageInfo, [sResolution, '100']);
    end
  else
    begin
      // show image resolution and scale
      sResolution:= IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
      Status.Panels[2].Text:= Format(fmtImageInfo, [sResolution, '100']);
      Status.Panels[3].Text:= Status.Panels[2].Text;
    end;
end;

procedure TfrmViewer.LoadGraphics(const sFileName:String);
var
  sExt: String;
  fsFileStream: TFileStreamEx;  
begin
//  DebugLn('TfrmViewer.Load graphics');
  bImage:= True;
  sExt:= ExtractFileExt(sFilename);
  System.Delete(sExt, 1, 1); // delete a dot
  try
    fsFileStream:= TFileStreamEx.Create(sFileName, fmOpenRead);
    try
      Image.Picture.LoadFromStreamWithFileExt(fsFileStream, sExt);
    except
      FreeAndNil(fsFileStream);
      ReMmapIfNeed; // open as text
      Exit;
    end;
  finally
    if Assigned(fsFileStream) then
      fsFileStream.Free;
  end;
  miStretch.Checked:= not miStretch.Checked;
  miStretchClick(nil);
  nbPages.ActivePageComponent:= pgImage;
  miImage.Visible:= True;
  miEdit.Visible:= False;
  miEncoding.Visible:= False;
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
  // Load search history
  FFindDialog.cbDataToFind.Items.Assign(glsSearchHistory);
  if FFindDialog.ShowModal <> mrOK then Exit;
  if FFindDialog.cbDataToFind.Text = '' then Exit;
  // Save search history
  glsSearchHistory.Assign(FFindDialog.cbDataToFind.Items);

  PAdr:= PosMem(PAdr, iSizeData, FFindDialog.cbDataToFind.Text, FFindDialog.cbCaseSens.Checked);

  if (PtrInt(PAdr) <> -1) then
  begin
    // founded, set position to ViewerControl
    ViewerControl.Position:= PtrInt(PAdr)-PtrInt(ViewerControl.GetDataAdr);
    ViewerControl.Up;
    // position is property and have write method  (repaint widget)
    UpDateScrollBar;
  end;
  SetFocus;
end;

procedure TfrmViewer.ChooseEncoding(mnuMenuItem: TMenuItem; sEncoding: String);
var
  I: Integer;
begin
  sEncoding:= NormalizeEncoding(sEncoding);
  for I:= 0 to mnuMenuItem.Count - 1 do
    if SameText(NormalizeEncoding(mnuMenuItem.Items[I].Caption), sEncoding) then
      mnuMenuItem.Items[I].Checked:= True;
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

procedure TfrmViewer.miChangeEncodingClick(Sender: TObject);
begin
  ViewerControl.Encoding:= (Sender as TMenuItem).Caption;
  ViewerControl.Repaint;
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
      ScrollPos := ViewerControl.Percent;
    end;
    scLineDown:
    begin
      ViewerControl.Down;
      ScrollPos := ViewerControl.Percent;
    end;
    scPageUp:
    begin
      ViewerControl.PageUp;
      ScrollPos := ViewerControl.Percent;
    end;
    scPageDown:
    begin
      ViewerControl.PageDown;
      ScrollPos := ViewerControl.Percent;
    end;
    scTop: ViewerControl.GoHome;
    scBottom: ViewerControl.GoEnd;
    scPosition:
    begin
      if ScrollPos = 0 then
        ViewerControl.GoHome
      else if ScrollPos = 100 then
        ViewerControl.GoEnd
      else
        begin
          ViewerControl.Percent := ScrollPos;
          if ViewerControl.UpLine then ViewerControl.DownLine;
          ScrollPos := ViewerControl.Percent;
        end;
    end;
  end;
  Status.Panels[2].Text:= cnvFormatFileSize(ViewerControl.Position)+' ('+IntToStr(ScrollPos)+' %)';
end;

initialization
 {$I fviewer.lrs}
end.
