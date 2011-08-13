unit uwlxprototypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WlxPlugin;

{$IFDEF MSWINDOWS}{$CALLING STDCALL}{$ELSE}{$CALLING CDECL}{$ENDIF}

type
  { Mandatory }
  TListLoad = function (ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle;
  { Optional }
  TListLoadNext = function (ParentWin,PluginWin:thandle;FileToLoad:pchar;ShowFlags:integer):integer;
  TListCloseWindow = procedure (ListWin:thandle);
  TListGetDetectString = procedure (DetectString:pchar;maxlen:integer);
  TListSearchText = function (ListWin:thandle;SearchString:pchar; SearchParameter:integer):integer;
  TListSearchDialog = function (ListWin:thandle;FindNext:integer):integer;
  TListSendCommand = function (ListWin:thandle;Command,Parameter:integer):integer;
  TListPrint = function (ListWin:thandle;FileToPrint,DefPrinter:pchar; PrintFlags:integer;var Margins:trect):integer;
  TListNotificationReceived = function (ListWin:thandle;Message,wParam,lParam:integer):integer;
  TListSetDefaultParams = procedure (dps:pListDefaultParamStruct);
  TListGetPreviewBitmap = function (FileToLoad:pchar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap;
  { Unicode }
  TListLoadW = function (ParentWin:thandle;FileToLoad:pwidechar;ShowFlags:integer):thandle;
  TListLoadNextW = function (ParentWin,PluginWin:thandle;FileToLoad:pwidechar;ShowFlags:integer):integer;
  TListSearchTextW = function (ListWin:thandle;SearchString:pwidechar; SearchParameter:integer):integer;
  TListPrintW = function (ListWin:thandle;FileToPrint,DefPrinter:pwidechar; PrintFlags:integer;var Margins:trect):integer;
  TListGetPreviewBitmapW = function (FileToLoad:pwidechar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap;

{$CALLING DEFAULT}

implementation

end.

