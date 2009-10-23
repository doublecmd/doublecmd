unit uwlxprototypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WlxPlugin;

type
    { Mandatory }
    TListLoad = function (ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle; stdcall;
    { Optional }
    TListLoadNext = function (ParentWin,PluginWin:thandle;FileToLoad:pchar;ShowFlags:integer):integer; stdcall;
    TListCloseWindow = procedure (ListWin:thandle); stdcall;
    TListGetDetectString = procedure (DetectString:pchar;maxlen:integer); stdcall;
    TListSearchText = function (ListWin:thandle;SearchString:pchar; SearchParameter:integer):integer; stdcall;
    TListSearchDialog = function (ListWin:thandle;FindNext:integer):integer; stdcall;
    TListSendCommand = function (ListWin:thandle;Command,Parameter:integer):integer; stdcall;
    TListPrint = function (ListWin:thandle;FileToPrint,DefPrinter:pchar; PrintFlags:integer;var Margins:trect):integer; stdcall;
    TListNotificationReceived = function (ListWin:thandle;Message,wParam,lParam:integer):integer; stdcall;
    TListSetDefaultParams = procedure (dps:pListDefaultParamStruct); stdcall;
    TListGetPreviewBitmap = function (FileToLoad:pchar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap; stdcall;
    { Unicode }
    TListLoadW = function (ParentWin:thandle;FileToLoad:pwidechar;ShowFlags:integer):thandle; stdcall;
    TListLoadNextW = function (ParentWin,PluginWin:thandle;FileToLoad:pwidechar;ShowFlags:integer):integer; stdcall;
    TListSearchTextW = function (ListWin:thandle;SearchString:pwidechar; SearchParameter:integer):integer; stdcall;
    TListPrintW = function (ListWin:thandle;FileToPrint,DefPrinter:pwidechar; PrintFlags:integer;var Margins:trect):integer; stdcall;
    TListGetPreviewBitmapW = function (FileToLoad:pwidechar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap; stdcall;

implementation

end.

