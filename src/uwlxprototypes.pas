unit uwlxprototypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,WLXPlugin;

type
    TListLoad= function (ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle; stdcall;
    TListLoadNext=  function (ParentWin,PluginWin:thandle;FileToLoad:pchar;ShowFlags:integer):integer; stdcall;
    TListCloseWindow=  procedure (ListWin:thandle); stdcall;
    TListGetDetectString=  procedure (DetectString:pchar;maxlen:integer); stdcall;
    TListSearchText=  function (ListWin:thandle;SearchString:pchar; SearchParameter:integer):integer; stdcall;
    TListSearchDialog=  function (ListWin:thandle;FindNext:integer):integer; stdcall;
    TListSendCommand=  function (ListWin:thandle;Command,Parameter:integer):integer; stdcall;
    TListPrint=  function (ListWin:thandle;FileToPrint,DefPrinter:pchar; PrintFlags:integer;var Margins:trect):integer; stdcall;
    TListNotificationReceived=  function (ListWin:thandle;Message,wParam,lParam:integer):integer; stdcall;
    TListSetDefaultParams=  procedure (dps:pListDefaultParamStruct); stdcall;
    TListGetPreviewBitmap=  function (FileToLoad:pchar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap; stdcall;


implementation

end.

