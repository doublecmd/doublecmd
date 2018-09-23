unit uwlxprototypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WlxPlugin, LCLType;

{$IFDEF MSWINDOWS}{$CALLING STDCALL}{$ELSE}{$CALLING CDECL}{$ENDIF}

type
  { Mandatory }
  TListLoad = function (ParentWin: HWND; FileToLoad: PAnsiChar; ShowFlags: Integer): HWND;
  { Optional }
  TListLoadNext = function (ParentWin, PluginWin: HWND; FileToLoad: PAnsiChar; ShowFlags: Integer): Integer;
  TListCloseWindow = procedure (ListWin: HWND);
  TListGetDetectString = procedure (DetectString: PAnsiChar; MaxLen: Integer);
  TListSearchText = function (ListWin: HWND; SearchString: PAnsiChar; SearchParameter: Integer): Integer;
  TListSearchDialog = function (ListWin: HWND; FindNext: Integer): Integer;
  TListSendCommand = function (ListWin: HWND; Command, Parameter: Integer): Integer;
  TListPrint = function (ListWin: HWND; FileToPrint, DefPrinter: PAnsiChar; PrintFlags: Integer; var Margins: TRect): Integer;
  TListNotificationReceived = function (ListWin: HWND; Message, wParam, lParam: Integer): Integer;
  TListSetDefaultParams = procedure (dps: PListDefaultParamStruct);
  TListGetPreviewBitmap = function (FileToLoad: PAnsiChar; Width, Height: Integer; ContentBuf: PByte; ContentBufLen: Integer): HBITMAP;
  { Unicode }
  TListLoadW = function (ParentWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): HWND;
  TListLoadNextW = function (ParentWin, PluginWin: HWND; FileToLoad: PWideChar; ShowFlags: Integer): Integer;
  TListSearchTextW = function (ListWin: HWND; SearchString: PWideChar; SearchParameter: Integer): Integer;
  TListPrintW = function (ListWin: HWND; FileToPrint, DefPrinter: PWideChar; PrintFlags: Integer; var Margins: TRect): Integer;
  TListGetPreviewBitmapW = function (FileToLoad: PWideChar; Width, Height: Integer; ContentBuf: PByte; ContentBufLen: Integer): HBITMAP;

{$CALLING DEFAULT}

implementation

end.

