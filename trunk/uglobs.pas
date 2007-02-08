{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

Globals variables and some consts

contributors:

Alexander Koblov (Alexx2000@mail.ru)

}

unit uGlobs;

interface
uses
  Classes, uExts, uColorExt, Graphics, uIni;
const
  cTopBorder = 23; // px on top is title
  cLeftBorder = 6; // px on left

var
  gDirSortFirst:Boolean=True; // want to show dir first in panels
  gDirHistoryCount:Integer=30; // how many history we remember
  gShowSystemFiles:Boolean=True;
  gLng:String='english.lng';
  gTerm:String='/usr/X11R6/bin/xterm -e bash -i -c %s';
  gRunTerm:String='/usr/X11R6/bin/xterm';
  gCaseSensitiveSort:Boolean=True;
  gLynxLike:Boolean=True;
  gDirSelect:Boolean=True;
  glsHotDir:TStringList;
  glsDirHistory:TStringList;
  gColumnSize:Array[0..4] of Integer;

  gShortFileSizeFormat:Boolean=True;
  gSeparateExt:Boolean=False;    // draw filename and extension separate

  gUseExtEdit:Boolean=False;
  gUseExtView:Boolean=False;
  gUseExtDiff:Boolean=False;

  gExtEdit:String='emacs "%s"';
  gExtView:String='emacs "%s"';
  gExtDiff:String='gtk-diff "%s" "%s"';
  gExts:TExts;
  gColorExt:TColorExt;

  gFontName:String;
  gFontSize:Integer;
  gFontWeight : Integer;
  gEditorFontName:String;
  gEditorSize:Integer;
  gViewerFontName:String;
  gViewerSize:Integer;

  gViewerPos:TRect;
  gEditorPos:TRect;

procedure LoadGlobs;
procedure SaveGlobs;
function LoadStringsFromFile(var list:TStringList; const sFileName:String):boolean;

const
  cMaxStringItems=50;
  
var
  gIni:TIni =nil;

implementation
uses
   SysUtils, uGlobsPaths, uLng, uShowMsg;

procedure LoadRect(Var ARect:TRect; sPrefix:String);
begin
//  writeln('Load Rect',sPrefix);
  ARect.Left:=StrToIntDef(gIni.Value[sPrefix+'left'],50);
//  writeln(ARect.Left);
  ARect.Top:=StrToIntDef(gIni.Value[sPrefix+'top'],50);
//  writeln(ARect.Top);
// warning Bottom = height rigth = width, TRect is used only for simplification
  ARect.Bottom:=StrToIntDef(gIni.Value[sPrefix+'height'],300);
//  writeln(ARect.Bottom);
  ARect.Right:=StrToIntDef(gIni.Value[sPrefix+'width'],400);
//  writeln(ARect.Right);
end;

procedure SaveRect(ARect:TRect; sPrefix:String);
begin

  gIni.Value[sPrefix+'left']:=IntToStr(ARect.Left+cLeftBorder);
  gIni.Value[sPrefix+'top']:=IntToStr(ARect.Top+cTopBorder);
// warning Bottom = height rigth = width, TRect is used only for simplification
  gIni.Value[sPrefix+'height']:=IntToStr(ARect.Bottom);
  gIni.Value[sPrefix+'width']:=IntToStr(ARect.Right);
end;

procedure InitGlobs;
begin
  gIni:=TIni.Create(gpIniDir+'doublecmd.ini');
  gExts:=TExts.Create;
  gColorExt:=TColorExt.Create;
  glsHotDir:=TStringList.Create;
  glsDirHistory:=TStringList.Create;
end;

procedure DeInitGlobs;
begin
  if assigned(gColorExt) then
    FreeAndNil(gColorExt);
  if assigned(glsDirHistory) then
    FreeAndNil(glsDirHistory);
  if assigned(glsHotDir) then
    FreeAndNil(glsHotDir);
  if Assigned(gExts) then
    FreeAndNil(gExts);
  if Assigned(gIni) then
    FreeAndNil(gIni);
end;

function StrToBoolDef(const sString:String; bDef:Boolean):Boolean;
begin
  if sString='' then
    Result:=bDef
  else
    Result:=StrToBool(sString);
end;

procedure LoadGlobs;
begin
  writeln('Loading configuration...');
  InitGlobs;
  gShowSystemFiles:=StrToBoolDef(gIni.Value['ShowSystemFiles'], False);
  gLng:=gIni.Value['Language'];
  gTerm:=gIni.Value['Term'];
  gCaseSensitiveSort:=StrToBoolDef(gIni.Value['CaseSensitiveSort'], False);
  gLynxLike:=StrToBoolDef(gIni.Value['LynxLike'], True);
  gDirSelect:=StrToBoolDef(gIni.Value['DirSelect'], True);
  glsHotDir.CommaText:=gIni.Value['HotDir'];
  gShortFileSizeFormat:=StrToBoolDef(gIni.Value['ShortFileSizeFormat'], True);

  gUseExtEdit:=StrToBoolDef(gIni.Value['UseExtEdit'], False);
  gUseExtView:=StrToBoolDef(gIni.Value['UseExtView'], False);
  gUseExtDiff:=StrToBoolDef(gIni.Value['UseExtDiff'], False);
  gSeparateExt:=StrToBoolDef(gIni.Value['SeparateExt'], True);

  gExtEdit:=gIni.Value['ExtEdit'];
  gExtView:=gIni.Value['ExtView'];
  gExtDiff:=gIni.Value['ExtDiff'];
  gRunTerm:=gIni.Value['RunTerm'];

  gFontName:=gIni.Value['FontName'];
  gFontWeight := StrToIntDef(gIni.Value['FontWeight'],700);
  writeln('gFontName:',gFontName);
  gEditorFontName:=gIni.Value['FontEditorName'];
  writeln('gEditorFontName:',gEditorFontName);
  gViewerFontName:=gIni.Value['FontViewerName'];
  writeln('gViewerEditorFontName:',gViewerFontName);
  gFontSize:=StrToIntDef(gIni.Value['FontSize'],10);
  gEditorSize:=StrToIntDef(gIni.Value['EditorSize'],14);
  gViewerSize:=StrToIntDef(gIni.Value['ViewerSize'],14);


  gExts.LoadFromFile(gpCfgDir+'doublecmd.ext');
  LoadStringsFromFile(glsDirHistory,gpIniDir+'dirhistory.txt');
  gColorExt.LoadFromFile(gpCfgDir+'color.ext');

  // default column widths
  gColumnSize[0]:=StrToIntDef(gIni.Value['Col0'],133);
  gColumnSize[1]:=StrToIntDef(gIni.Value['Col1'],50);
  gColumnSize[2]:=StrToIntDef(gIni.Value['Col2'],64);
  gColumnSize[3]:=StrToIntDef(gIni.Value['Col3'],73);
  gColumnSize[4]:=StrToIntDef(gIni.Value['Col4'],59);

  writeln('Loading viewer position...');
  LoadRect(gViewerPos, 'Viewer.');
  writeln('Loading editor position...');
  LoadRect(gEditorPos, 'Editor.');
  DoLoadLng;
  msgLoadLng;
end;

function Bool2Str(b:Boolean):Char;
begin
  Result:=Chr(ord(b)+ord('0'));
end;

function LoadStringsFromFile(var list:TStringList; const sFileName:String):boolean;
var
  i:Integer;
begin
  Assert(list <> nil,'LoadStringsFromFile: list=nil');
  list.Clear;
  Result:=False;
  if not FileExists(sFileName) then Exit;
  list.LoadFromFile(sFileName);
  for i:=list.Count-1 downto 0 do
    if i>cMaxStringItems then
      list.Delete(i)
    else
      Break;
  Result:=True;
end;


procedure SaveGlobs;
var
  x:Integer;
begin
  glsDirHistory.SaveToFile(gpIniDir+'dirhistory.txt');

  gIni.Value['ShowSystemFiles']:=Bool2Str(gShowSystemFiles);
  gIni.Value['Language']:=gLng;
  gIni.Value['Term']:=gTerm;
  gIni.Value['CaseSensitiveSort']:=Bool2Str(gCaseSensitiveSort);
  gIni.Value['LynxLike']:=Bool2Str(gLynxLike);
  gIni.Value['DirSelect']:=Bool2Str(gDirSelect);
  gIni.Value['HotDir']:=glsHotDir.CommaText;
  gIni.Value['ShortFileSizeFormat']:=Bool2Str(gShortFileSizeFormat);


  gIni.Value['UseExtEdit']:=Bool2Str(gUseExtEdit);
  gIni.Value['UseExtView']:=Bool2Str(gUseExtView);
  gIni.Value['UseExtDiff']:=Bool2Str(gUseExtDiff);
  gIni.Value['SeparateExt']:=Bool2Str(gSeparateExt);

  gIni.Value['ExtEdit']:=gExtEdit;
  gIni.Value['ExtView']:=gExtView;
  gIni.Value['ExtDiff']:=gExtDiff;
  gIni.Value['RunTerm']:=gRunTerm;

  gIni.Value['FontName']:=gFontName;
  gIni.Value['FontWeight'] := IntToStr(gFontWeight);
  gIni.Value['FontEditorName']:=gEditorFontName;
  gIni.Value['FontViewerName']:=gViewerFontName;

  gIni.Value['FontSize']:=IntToStr(gFontSize);
  gIni.Value['EditorSize']:=IntToStr(gEditorSize);
  gIni.Value['ViewerSize']:=IntToStr(gViewerSize);

  for x:=0 to 4 do
    gIni.Value['Col'+IntToStr(x)]:= IntToStr(gColumnSize[x]);
    
  SaveRect(gViewerPos, 'Viewer.');
  SaveRect(gEditorPos, 'Editor.');

  gIni.Save;
end;

initialization

finalization
  SaveGlobs;
end.
