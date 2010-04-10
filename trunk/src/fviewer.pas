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
  ExtCtrls, ComCtrls, LCLProc, Menus, Dialogs, ExtDlgs, EditBtn,
  viewercontrol, fFindView, WLXPlugin, uWLXModule,
  uFileSource;

type

  { TfrmViewer }

  TfrmViewer = class(TForm)
    Image: TImage;
    miZoomOut: TMenuItem;
    miZoomIn: TMenuItem;
    miRotate: TMenuItem;
    miMirror: TMenuItem;
    mi270: TMenuItem;
    mi180: TMenuItem;
    mi90: TMenuItem;
    miSearchPrev: TMenuItem;
    miPrint: TMenuItem;
    miSearchNext: TMenuItem;
    pmiSelectAll: TMenuItem;
    miDiv5: TMenuItem;
    pmiCopy: TMenuItem;
    pnlImage: TPanel;
    pnlText: TPanel;
    miDiv3: TMenuItem;
    miEncoding: TMenuItem;
    miPlugins: TMenuItem;
    miSeparator: TMenuItem;
    pnlLister: TPanel;
    pmEditMenu: TPopupMenu;
    sboxImage: TScrollBox;
    Status: TStatusBar;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miPrev: TMenuItem;
    miNext: TMenuItem;
    miView: TMenuItem;
    miExit: TMenuItem;
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
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miPluginsClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure miSearchNextClick(Sender: TObject);
    procedure miSearchPrevClick(Sender: TObject);
    procedure miZoomClick(Sender: TObject);
    procedure pnlListerResize(Sender: TObject);
    procedure sboxImageResize(Sender: TObject);
    procedure ViewerControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure frmViewerClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure frmViewerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miExitClick(Sender: TObject);
    procedure miNextClick(Sender: TObject);
    procedure miPrevClick(Sender: TObject);
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
    procedure ViewerPositionChanged(Sender:TObject);
    procedure miRotateClick(Sender: TObject);
  private
    FileList: TStringList;
    iActiveFile,
    tmpX, tmpY:Integer;
    bImage,
    bPlugin,
    bQuickView,
    MDFlag: Boolean;
    FFindDialog:TfrmFindView;
    FFileSource: IFileSource;
    FLastSearchPos: PtrInt;
    //---------------------
    WlxPlugins:TWLXModuleList;
    ActivePlugin:Integer;
    //---------------------
    function CheckPlugins(const sFileName: UTF8String; Force: boolean=false):boolean;
    procedure ExitPluginMode;
    Function CheckGraphics(const sFileName:String):Boolean;
    procedure AdjustImageSize;
    procedure LoadGraphics(const sFileName:String);
    procedure DoSearch(bQuickSearch: Boolean; bSearchBackwards: Boolean);
    procedure MakeTextEncodingsMenu;
    procedure ActivatePanel(Panel: TPanel);
    procedure ReopenAsTextIfNeeded;

  public
    constructor Create(TheOwner: TComponent; aFileSource: IFileSource); reintroduce;
    destructor Destroy; override;
    procedure LoadFile(const aFileName: UTF8String);
    procedure LoadNextFile(const aFileName: UTF8String);
    procedure LoadFile(iIndex:Integer);
    property QuickView: Boolean read bQuickView write bQuickView;
  end;


procedure ShowViewer(const FilesToView:TStringList; const aFileSource: IFileSource = nil);

implementation

uses
  IntfGraphics, uLng, uShowMsg, uGlobs, LCLType, LConvEncoding, uClassesEx, uFindMmap, uDCUtils;

const
  // Status bar panels indexes.
  sbpFileName             = 0;
  sbpFileNr               = 1;
  // Text
  sbpPosition             = 2;
  sbpFileSize             = 3;
  sbpTextEncoding         = 4;
  // WLX
  sbpPluginName           = 2;
  // Graphics
  sbpCurrentResolution    = 2;
  sbpFullResolution       = 3;

procedure ShowViewer(const FilesToView:TStringList; const aFileSource: IFileSource);
var
  Viewer: TfrmViewer;
begin
  //DebugLn('ShowViewer - Using Internal');
  Viewer := TfrmViewer.Create(Application, aFileSource);
  Viewer.QuickView:= False;
  Viewer.FileList.Assign(FilesToView); // Make a copy of the list
  Viewer.LoadFile(0);
  Viewer.Show;
end;

constructor TfrmViewer.Create(TheOwner: TComponent; aFileSource: IFileSource);
begin
  inherited Create(TheOwner);
  FFileSource := aFileSource;
  FLastSearchPos := -1;
end;

destructor TfrmViewer.Destroy;
begin
  FreeThenNil(FileList);
  inherited Destroy;
  FFileSource := nil; // If this is temp file source, the files will be deleted.
end;

procedure TfrmViewer.LoadFile(const aFileName: UTF8String);
var
  i: Integer;
begin
  FLastSearchPos := -1;
  Caption := aFileName;

  // Clear text on status bar.
  for i := 0 to Status.Panels.Count - 1 do
    Status.Panels[i].Text := '';

  Screen.Cursor:=crHourGlass;
  try
    bPlugin:= CheckPlugins(aFileName);
    if bPlugin then
      begin
        Status.Panels[sbpPluginName].Text:= WlxPlugins.GetWLxModule(ActivePlugin).Name;
        ActivatePanel(pnlLister);
      end
    else if CheckGraphics(aFileName) then
      begin
        LoadGraphics(aFileName);
        ActivatePanel(pnlImage);
      end
    else
      begin
        ViewerControl.FileName := aFileName;     //handled by miProcess.Click
        ActivatePanel(pnlText);
//        miProcess.Click;
      end;

    Status.Panels[sbpFileName].Text:=aFileName;
    Status.Panels[sbpFileSize].Text:= cnvFormatFileSize(ViewerControl.FileSize) + ' (100 %)';
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmViewer.LoadNextFile(const aFileName: UTF8String);
begin
  if bPlugin then
    begin
      if WlxPlugins.GetWlxModule(ActivePlugin).CallListLoadNext(pnlLister.Handle, aFileName, 0) <> LISTPLUGIN_ERROR then
        Exit;
    end;

  LoadFile(aFileName);
end;

procedure TfrmViewer.LoadFile(iIndex: Integer);
begin
  iActiveFile := iIndex;
  LoadFile(FileList.Strings[iIndex]);
  Status.Panels[sbpFileNr].Text:=Format('%d/%d',[iIndex+1,FileList.Count]);
end;

procedure TfrmViewer.FormKeyPress(Sender: TObject; var Key: Char);
begin
  // The following keys work only in QuickView mode because there is no menu there.
  // Otherwise this function is never called for those keys
  // because the menu shortcuts are automatically used.
  if bQuickView then
    case Key of
      'N', 'n':
        begin
          miNextClick(Sender);
          Key := #0;
        end;
      'P', 'p':
        begin
          miPrevClick(Sender);
          Key := #0;
        end;
      '1':
        begin
          miTextClick(Sender);
          Key := #0;
        end;
      '2':
        begin
          miBinClick(Sender);
          Key := #0;
        end;
      '3':
        begin
          miHexClick(Sender);
          Key := #0;
        end;
      '4':
        begin
          miWrapTextClick(Sender);
          Key := #0;
        end;
    end;
end;

procedure TfrmViewer.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MDFlag := true;
  //sboxImage.AutoScroll:=false;
  //sboxImage.HorzScrollBar.Range:=Image.Width;
  //sboxImage.VertScrollBar.Range:=Image.Height;
  sboxImage.VertScrollBar.Position:=tmpY;
  sboxImage.HorzScrollBar.Position:=tmpX;
  tmpX:=x;
  tmpY:=y{-sboxImage.VertScrollBar.Position};
  Image.Cursor:=crHandPoint;
end;

procedure TfrmViewer.ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MDFlag then
    begin
      sboxImage.VertScrollBar.Position:=sboxImage.VertScrollBar.Position+tmpY-y;
      sboxImage.HorzScrollBar.Position:=sboxImage.HorzScrollBar.Position+tmpX-x;
    end;
end;

procedure TfrmViewer.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MDFlag:=false;
  tmpY:=sboxImage.VertScrollBar.Position;
  tmpX:=sboxImage.HorzScrollBar.Position;
  Image.Cursor:=crDefault;
end;

function TfrmViewer.CheckPlugins(const sFileName: UTF8String; Force:boolean=false):boolean;
var
  I: Integer;
begin
  I:= 0;
  DebugLn('WlXPlugins.Count = ' + IntToStr(WlxPlugins.Count));
  while (I < WlxPlugins.Count) do
   if WlxPlugins.GetWLxModule(I).FileParamVSDetectStr(sFileName) then
     begin
       Result:= True;
       DebugLn('I = '+IntToStr(I));
       {$PUSH}{$R-}
       if not WlxPrepareContainer(pnlLister.Handle) then {TODO: ERROR and exit;};
       {$POP}
       WlxPlugins.LoadModule(I);
       DebugLn('WlxModule.Name = ', WlxPlugins.GetWLxModule(I).Name);
       if WlxPlugins.GetWLxModule(I).CallListLoad(pnlLister.Handle, sFileName, {TODO: showFlags}0) = 0 then
         begin
           WlxPlugins.GetWLxModule(I).UnloadModule;
           Inc(I);
           Continue;
         end;
       ActivePlugin:= I;
       WlxPlugins.GetWlxModule(ActivePlugin).ResizeWindow(pnlLister.ClientRect);
       Exit;
     end
   else  I:= I + 1;
 // Plugin not found
 ActivePlugin:= -1;
 Result:= False;
end;

procedure TfrmViewer.ExitPluginMode;
begin
  {$PUSH}{$R-}
  WlxPrepareContainer(pnlLister.Handle,true);
  {$POP}
  if (WlxPlugins.Count > 0) and (ActivePlugin >= 0) then
    begin
      WlxPlugins.GetWLxModule(ActivePlugin).CallListCloseWindow;
      WlxPlugins.GetWLxModule(ActivePlugin).UnloadModule;
    end;
end;

procedure TfrmViewer.miPluginsClick(Sender: TObject);
begin
  bPlugin:= CheckPlugins(FileList.Strings[iActiveFile], True);
  if bPlugin then
  begin
    Status.Panels[sbpPluginName].Text:= WlxPlugins.GetWLxModule(ActivePlugin).Name;
    ActivatePanel(pnlLister);
  end
  else
    ViewerControl.FileName := FileList.Strings[iActiveFile];
end;

procedure TfrmViewer.miPrintClick(Sender: TObject);
var
  aRect: TRect;
begin
  if bPlugin then
    begin
      aRect:= pnlLister.ClientRect;
      WlxPlugins.GetWlxModule(ActivePlugin).CallListPrint(FileList[iActiveFile], EmptyStr, 0, aRect);
    end;
end;

procedure TfrmViewer.miSearchNextClick(Sender: TObject);
begin
  DoSearch(True, False);
end;

procedure TfrmViewer.miSearchPrevClick(Sender: TObject);
begin
  DoSearch(True, True);
end;

procedure TfrmViewer.miZoomClick(Sender: TObject);
begin
  miStretch.Checked := false;
  Image.Stretch:=true;
  Image.Proportional:=true;
  Image.AutoSize := false;
  if sender=miZoomIn
  then
    begin
      Image.Width:= Image.Width + round(0.118*Image.Picture.Width);  {Image.Width+Round(0.1*Image.Width)}
      Image.Height:= Image.Height + round(0.118*Image.Picture.Height);  {Image.Height+Round(Image.Height*0.1)}
    end
  else
    begin
     Image.Width:= Image.Width-Round(0.1333*Image.Width);
     Image.Height:= Image.Height-Round(0.133*Image.Height);
    end;
  //sboxImage.VertScrollBar.Position:= sboxImage.VertScrollBar.Position+Image.Height div 16;
  //sboxImage.HorzScrollBar.Position:= sboxImage.HorzScrollBar.Position+Image.Width div 16;
  AdjustImageSize
end;

procedure TfrmViewer.pnlListerResize(Sender: TObject);
begin
  if bPlugin then
    WlxPlugins.GetWlxModule(ActivePlugin).ResizeWindow(pnlLister.ClientRect);
end;

procedure TfrmViewer.sboxImageResize(Sender: TObject);
begin
  if bImage then AdjustImageSize;
end;

procedure TfrmViewer.ViewerControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    pmEditMenu.PopUp();
end;

procedure TfrmViewer.frmViewerClose(Sender: TObject;
                                    var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  gViewerImageStretch:= miStretch.Checked;
  if Assigned(WlxPlugins) then
     begin
       ExitPluginMode;
     end;
end;

procedure TfrmViewer.frmViewerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (not bQuickView) and (Key in [VK_Q, VK_ESCAPE]) then
  begin
    Key := 0;
    Close;
    Exit;
  end;

  if (not bImage) then
    case Key of
      VK_F:
        if Shift = [ssCtrl] then
        begin
          DoSearch(False, False);
          Key:= 0;
          Exit;
        end;

      VK_F3:
        if Shift - [ssShift] = [] then
        begin
          DoSearch(True, Shift = [ssShift]);
          Key:= 0;
          Exit;
        end;
    end;
end;

procedure TfrmViewer.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmViewer.miNextClick(Sender: TObject);
var
  I: Integer;
begin
  I:= iActiveFile + 1;
  if I >= FileList.Count then
    I:= 0;

  if bPlugin then
    begin
      if WlxPlugins.GetWlxModule(ActivePlugin).CallListLoadNext(pnlLister.Handle, FileList[I], 0) <> LISTPLUGIN_ERROR then
        Exit;
    end;

  LoadFile(I);
end;

procedure TfrmViewer.miPrevClick(Sender: TObject);
var
  I: Integer;
begin
  I:= iActiveFile - 1;
  if I < 0 then
    I:= FileList.Count - 1;

  if bPlugin then
    begin
      if WlxPlugins.GetWlxModule(ActivePlugin).CallListLoadNext(pnlLister.Handle, FileList[I], 0) <> LISTPLUGIN_ERROR then
        Exit;
    end;

  LoadFile(I);
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
  ReopenAsTextIfNeeded;
  ViewerControl.ViewerMode := vmText;
  miText.Checked := True;
end;

procedure TfrmViewer.miBinClick(Sender: TObject);
begin
  ExitPluginMode;
  ReopenAsTextIfNeeded;
  ViewerControl.ViewerMode := vmBin;
  miBin.Checked := True;
end;

procedure TfrmViewer.miHexClick(Sender: TObject);
begin
  ExitPluginMode;
  ReopenAsTextIfNeeded;
  ViewerControl.ViewerMode := vmHex;
  miHex.Checked := True;
end;

procedure TfrmViewer.miWrapTextClick(Sender: TObject);
begin
  ExitPluginMode;
  ReopenAsTextIfNeeded;
  ViewerControl.ViewerMode := vmWrap;
  miWrapText.Checked := True;
end;

procedure TfrmViewer.miAbout2Click(Sender: TObject);
begin
  MsgOK(rsViewAboutText);
end;

procedure TfrmViewer.miSearchClick(Sender: TObject);
begin
  FLastSearchPos := -1;
  DoSearch(False, False);
end;

procedure TfrmViewer.FormCreate(Sender: TObject);
begin
  InitPropStorage(Self);

  ViewerControl.Font.Name  := gViewerFontName;
  ViewerControl.Font.Size  := gViewerFontSize;
  ViewerControl.Font.Style := gViewerFontStyle;

  FileList := TStringList.Create;

  WlxPlugins:=TWLXModuleList.Create;
  WlxPlugins.Assign(gWLXPlugins);
  DebugLn('WLX: Load - OK');

  FFindDialog:=nil; // dialog is created in first use
  
  miStretch.Checked:= gViewerImageStretch;

  MakeTextEncodingsMenu;

  Status.Panels[sbpFileNr].Alignment := taRightJustify;
  Status.Panels[sbpPosition].Alignment := taRightJustify;
  Status.Panels[sbpFileSize].Alignment := taRightJustify;

  ViewerPositionChanged(Self);
end;

procedure TfrmViewer.FormDestroy(Sender: TObject);
begin
  if Assigned(WlxPlugins) then
     FreeAndNil(WlxPlugins);
  if Assigned(FFindDialog) then
     FreeAndNil(FFindDialog);
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
    miProcess.Checked:=not miProcess.Checked;
  end;
}
end;

procedure TfrmViewer.ReopenAsTextIfNeeded;
begin
  if bImage or bPlugin then
  begin
    Image.Picture := nil;
    ViewerControl.FileName := FileList.Strings[iActiveFile];
    ActivatePanel(pnlText);
  end;
end;

procedure TfrmViewer.miGraphicsClick(Sender: TObject);
begin
  if CheckGraphics(FileList.Strings[iActiveFile]) then
    begin
      ViewerControl.FileName := ''; // unload current file if any is loaded
      LoadGraphics(FileList.Strings[iActiveFile]);
    end;
end;

procedure TfrmViewer.miCopyToClipboardClick(Sender: TObject);
begin
  if bPlugin then
    WlxPlugins.GetWLxModule(ActivePlugin).CallListSendCommand(lc_copy, 0)
  else
    ViewerControl.CopyToClipboard;
end;

procedure TfrmViewer.miSelectAllClick(Sender: TObject);
begin
  if bPlugin then
    WlxPlugins.GetWLxModule(ActivePlugin).CallListSendCommand(lc_selectall, 0)
  else
    ViewerControl.SelectAll;
end;

procedure TfrmViewer.miChangeEncodingClick(Sender: TObject);
begin
  ViewerControl.EncodingName := (Sender as TMenuItem).Caption;
  Status.Panels[4].Text := rsViewEncoding + ': ' + ViewerControl.EncodingName;
end;

function TfrmViewer.CheckGraphics(const sFileName:String):Boolean;
var
  sExt:String;
begin
  sExt:=Lowercase(ExtractFileExt(sFileName));
  Result:=(sExt='.bmp') or (sExt='.xpm') or (sExt='.png') or
       (sExt='.jpg') or (sExt='.jpeg') or (sExt='.ico') or
       (sExt='.ddw') or (sExt='.tga');
end;

// Adjust Image size (width and height) to sboxImage size
procedure TfrmViewer.AdjustImageSize;
const
  fmtImageInfo = '%s (%s %%)';
var
  sResolution: String;
  iScale: Integer;
begin
  if miStretch.Checked then
     begin
       Image.Stretch:=true;
       Image.AutoSize := true;
       if (Image.Picture.Width > sboxImage.ClientWidth) or  (Image.Picture.Height > sboxImage.ClientHeight) then
         begin
           Image.Left:= 0;
           Image.Top:= 0;
           Image.AutoSize := false;
           Image.Width:= sboxImage.ClientWidth;
           Image.Height:= sboxImage.ClientHeight;
           sboxImage.HorzScrollBar.Visible:=false;
           sboxImage.VertScrollBar.Visible:=false;
           // show image resolution and scale
           if Image.Picture.Width < Image.Width then
             begin
             iScale:= 100*(Image.Picture.Width * Image.ClientHeight) div (Image.Picture.Width* Image.Picture.Height);
             sResolution:= IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Height);
             end;
           if Image.Picture.Height < Image.Height then
             begin
              iScale:= 100*(Image.ClientWidth * Image.Picture.Height) div (Image.Picture.Width* Image.Picture.Height);
              sResolution:= IntToStr(Image.Width) + 'x' + IntToStr(Image.Picture.Height);
             end;
           if (Image.Picture.Width >= Image.Width) and  (Image.Picture.Height >= Image.Height) then
             begin
              iScale:= 100*(Image.Width * Image.Height) div (Image.Picture.Width* Image.Picture.Height);
              sResolution:= IntToStr(Image.Width) + 'x' + IntToStr(Image.Height);
             end;
           Status.Panels[sbpCurrentResolution].Text:= Format(fmtImageInfo, [sResolution, IntToStr(iScale)]);
           sResolution:= IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
           Status.Panels[sbpFullResolution].Text:= Format(fmtImageInfo, [sResolution, '100']);
         end
       else
         begin
           Image.Left:= (sboxImage.ClientWidth-Image.Picture.Width) div 2;        //move image to center
           Image.Top:=  (sboxImage.ClientHeight-Image.Picture.Height) div 2;
           sResolution:= IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
           Status.Panels[sbpCurrentResolution].Text:= Format(fmtImageInfo, [sResolution, '100']);
           Status.Panels[sbpFullResolution].Text:= Status.Panels[2].Text;
         end;
     end
  else
    begin
      // show image resolution and scale
      Image.Left:= 0;
      Image.Top:= 0;
      iScale:= 100*(Image.Width * Image.Height) div (Image.Picture.Width* Image.Picture.Height);
      sResolution:= IntToStr(Image.Width) + 'x' + IntToStr(Image.Height);
      Status.Panels[sbpCurrentResolution].Text:= Format(fmtImageInfo, [sResolution, IntToStr(iScale)]);
      sResolution:= IntToStr(Image.Picture.Width) + 'x' + IntToStr(Image.Picture.Height);
      //Status.Panels[sbpCurrentResolution].Text:= Format(fmtImageInfo, [sResolution, '100']);
      Status.Panels[sbpFullResolution].Text:= Format(fmtImageInfo, [sResolution, '100']);
      //Status.Panels[sbpFullResolution].Text:= Status.Panels[2].Text;
    end;
end;

// Try to rotate image
procedure TfrmViewer.miRotateClick(Sender: TObject);
var
  x, y: Integer;
  xWidth,
  yHeight: Integer;
  SourceImg: TLazIntfImage = nil;
  TargetImg: TLazIntfImage = nil;
begin
  TargetImg:= TLazIntfImage.Create(0, 0);
  SourceImg:= Image.Picture.Bitmap.CreateIntfImage;
  TargetImg.DataDescription:= SourceImg.DataDescription; // use the same image format
  xWidth:= Image.Picture.Bitmap.Width - 1;
  yHeight:= Image.Picture.Bitmap.Height - 1;
  if Sender = mi180 then
      begin
        TargetImg.SetSize(xWidth + 1, yHeight + 1);
        for y:= 0 to yHeight do
        begin
          for x:= 0 to xWidth do
          begin
            TargetImg.Colors[x, y]:= SourceImg.Colors[xWidth - x, yHeight - y];
          end;
        end;
      end;
    if Sender = mi270 then
      begin
        TargetImg.SetSize(yHeight + 1, xWidth + 1);
        for y:= 0 to xWidth do
        begin
          for x:= 0 to yHeight do
          begin
            TargetImg.Colors[x, y]:= SourceImg.Colors[xWidth - y, x];
          end;
        end;
      end;
    if Sender = mi90 then
      begin
        TargetImg.SetSize(yHeight + 1, xWidth + 1);
        for y:= 0 to xWidth do
        begin
          for x:= 0 to yHeight do
          begin
            TargetImg.Colors[x, y]:= SourceImg.Colors[y, yHeight - x];
          end;
        end;
      end;
    if Sender = miMirror then
      begin
        TargetImg.SetSize(xWidth + 1, yHeight + 1);
        for y:= 0 to yHeight do
        begin
          for x:= 0 to xWidth do
          begin
            TargetImg.Colors[x, y]:= SourceImg.Colors[xWidth - x, y];
          end;
        end;
      end;
  Image.Picture.Bitmap.LoadFromIntfImage(TargetImg);
  FreeThenNil(SourceImg);
  FreeThenNil(TargetImg);
end;

procedure TfrmViewer.LoadGraphics(const sFileName:String);
var
  sExt: String;
  fsFileStream: TFileStreamEx = nil;
begin
  bImage:= True;
  sExt:= ExtractFileExt(sFilename);
  System.Delete(sExt, 1, 1); // delete a dot
  try
    fsFileStream:= TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone);
    try
      Image.Picture.LoadFromStreamWithFileExt(fsFileStream, sExt);
    except
      FreeAndNil(fsFileStream);
      ReopenAsTextIfNeeded; // open as text
      Exit;
    end;
  finally
    if Assigned(fsFileStream) then
      FreeAndNil(fsFileStream);
  end;

  miStretch.Checked:= not miStretch.Checked;
  miStretchClick(nil);
  ActivatePanel(pnlImage);
end;

procedure TfrmViewer.DoSearch(bQuickSearch: Boolean; bSearchBackwards: Boolean);
var
  PAdr: PChar;
  iSizeData: Integer;
  sSearchText: UTF8String;
begin
  // in first use create dialog
  if not Assigned(FFindDialog) then
     FFindDialog:= TfrmFindView.Create(Application);

  if (bQuickSearch and gFirstTextSearch) or not bQuickSearch then
    begin
      if bPlugin then
        begin
          // if plugin has specific search dialog
          if WlxPlugins.GetWLxModule(ActivePlugin).CallListSearchDialog(0) = LISTPLUGIN_OK then
            Exit;
        end;
      // Load search history
      FFindDialog.cbDataToFind.Items.Assign(glsSearchHistory);
      if FFindDialog.ShowModal <> mrOK then Exit;
      if FFindDialog.cbDataToFind.Text = '' then Exit;
      sSearchText:= FFindDialog.cbDataToFind.Text;
      // Save search history
      glsSearchHistory.Assign(FFindDialog.cbDataToFind.Items);
      gFirstTextSearch:= False;
    end
  else
    begin
      if bPlugin then
        begin
          // if plugin has specific search dialog
          if WlxPlugins.GetWLxModule(ActivePlugin).CallListSearchDialog(1) = LISTPLUGIN_OK then
            Exit;
        end;
      if glsSearchHistory.Count > 0 then
        sSearchText:= glsSearchHistory[0];
    end;

  if bPlugin then
    begin
      iSizeData:= 0;
      if FFindDialog.cbCaseSens.Checked then
        iSizeData:= lcs_matchcase;
      WlxPlugins.GetWLxModule(ActivePlugin).CallListSearchText(sSearchText, iSizeData);
    end
  else
    begin
      // Choose search start position.
      if not bSearchBackwards then
      begin
        if FLastSearchPos = -1 then
          FLastSearchPos := 0
        else if FLastSearchPos < ViewerControl.FileSize - 1 then
          FLastSearchPos := FLastSearchPos + 1;
      end
      else
      begin
        if FLastSearchPos = -1 then
          FLastSearchPos := ViewerControl.FileSize - 1
        else if FLastSearchPos > 0 then
          FLastSearchPos := FLastSearchPos - 1;
      end;

      PAdr := PosMem(ViewerControl.GetDataAdr, ViewerControl.FileSize,
                     FLastSearchPos, sSearchText, FFindDialog.cbCaseSens.Checked,
                     bSearchBackwards);

      if (PAdr <> Pointer(-1)) then
        begin
          FLastSearchPos := PAdr - ViewerControl.GetDataAdr;
          // text found, show it in ViewerControl if not visible
          ViewerControl.MakeVisible(FLastSearchPos);
          // Select found text.
          ViewerControl.SelectText(FLastSearchPos, FLastSearchPos + UTF8Length(sSearchText));
        end
      else
        begin
          msgOK(Format(rsViewNotFound, ['"' + sSearchText + '"']));
          FLastSearchPos := -1;
        end;
    end;
end;

procedure TfrmViewer.MakeTextEncodingsMenu;
var
  I: Integer;
  mi: TMenuItem;
  EncodingsList: TStringList;
begin
  miEncoding.Clear;
  EncodingsList := TStringList.Create;
  try
    ViewerControl.GetSupportedEncodings(EncodingsList);
    for I:= 0 to EncodingsList.Count - 1 do
      begin
        mi:= TMenuItem.Create(miEncoding);
        mi.Caption:= EncodingsList[I];
        mi.AutoCheck:= True;
        mi.RadioItem:= True;
        mi.GroupIndex:= 1;
        mi.OnClick:= @miChangeEncodingClick;
        if ViewerControl.EncodingName = EncodingsList[I] then
          mi.Checked := True;
        miEncoding.Add(mi);
      end;
  finally
    FreeAndNil(EncodingsList);
  end;
end;

procedure TfrmViewer.ViewerPositionChanged(Sender:TObject);
begin
  if ViewerControl.FileSize > 0 then
    begin
      Status.Panels[sbpPosition].Text :=
          cnvFormatFileSize(ViewerControl.Position) +
          ' (' + IntToStr(ViewerControl.Percent) + ' %)';
    end
  else
    Status.Panels[sbpPosition].Text:= cnvFormatFileSize(0) + ' (0 %)';
end;

procedure TfrmViewer.ActivatePanel(Panel: TPanel);
begin
  pnlLister.Hide;
  pnlImage.Hide;
  pnlText.Hide;

  Panel.Visible := True;

  bImage             := (Panel = pnlImage);
  bPlugin            := (Panel = pnlLister);
  miPlugins.Checked  := (Panel = pnlLister);
  miGraphics.Checked := (Panel = pnlImage);
  miImage.Visible    := (Panel = pnlImage);
  miEncoding.Visible := (Panel = pnlText);
  miEdit.Visible     := (Panel = pnlText) or (Panel = pnlLister);

  if Panel = pnlLister then
  begin
  end
  else if Panel = pnlText then
  begin
    if (not bQuickView) and CanFocus and ViewerControl.CanFocus then
      ViewerControl.SetFocus;

    case ViewerControl.ViewerMode of
      vmText: miText.Checked := True;
      vmWrap: miWrapText.Checked := True;
      vmBin:  miBin.Checked := True;
      vmHex:  miHex.Checked := True;
    end;

    Status.Panels[sbpFileSize].Text:= cnvFormatFileSize(ViewerControl.FileSize) + ' (100 %)';
    Status.Panels[sbpTextEncoding].Text := rsViewEncoding + ': ' + ViewerControl.EncodingName;
  end
  else if Panel = pnlImage then
  begin
  end;
end;

initialization
 {$I fviewer.lrs}

end.
