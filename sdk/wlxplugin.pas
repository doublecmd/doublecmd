// Lister API definitions.
// This unit is written by Christian Ghisler, it's from Total Commander
// Lister API Guide, which can be found at http://ghisler.com.
// Version: 2.0.

unit WlxPlugin;

interface

const
  lc_copy=1;
  lc_newparams=2;
  lc_selectall=3;
  lc_setpercent=4;
  lcp_wraptext=1;
  lcp_fittowindow=2;
  lcp_ansi=4;
  lcp_ascii=8;
  lcp_variable=12;
  lcp_forceshow=16;
  lcp_fitlargeronly=32;
  lcp_center=64;
  lcs_findfirst=1;
  lcs_matchcase=2;
  lcs_wholewords=4;
  lcs_backwards=8;
  itm_percent=$FFFE;
  itm_fontstyle=$FFFD;
  itm_wrap=$FFFC;
  itm_fit=$FFFB;
  itm_next=$FFFA;
  itm_center=$FFF9;
  LISTPLUGIN_OK=0;
  LISTPLUGIN_ERROR=1;

const
  MAX_PATH=32000;

type
  { Unsigned integer with pointer size }
  THandle = {$IFDEF CPU64}QWord{$ELSE}LongWord{$ENDIF};

const
  wlxInvalidHandle: THandle = THandle(-1);

type
  tListDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..MAX_PATH-1] of char;
  end;

  pListDefaultParamStruct=^tListDefaultParamStruct;

type tdateformat=record
       wYear,wMonth,wDay:word;
     end;
     pdateformat=^tdateformat;

type ttimeformat=record
       wHour,wMinute,wSecond:word;
     end;
     ptimeformat=^ttimeformat;

type HBITMAP = type THandle;


{ Function prototypes: Functions need to be defined exactly like this!}

(*

function ListLoad(ParentWin:thandle;FileToLoad:pchar;ShowFlags:integer):thandle; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListLoadW(ParentWin:thandle;FileToLoad:pwidechar;ShowFlags:integer):thandle; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListLoadNext(ParentWin,PluginWin:thandle;FileToLoad:pchar;ShowFlags:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListLoadNextW(ParentWin,PluginWin:thandle;FileToLoad:pwidechar;ShowFlags:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
procedure ListCloseWindow(ListWin:thandle); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
procedure ListGetDetectString(DetectString:pchar;maxlen:integer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListSearchText(ListWin:thandle;SearchString:pchar; SearchParameter:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListSearchTextW(ListWin:thandle;SearchString:pwidechar; SearchParameter:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListSearchDialog(ListWin:thandle;FindNext:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListSendCommand(ListWin:thandle;Command,Parameter:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListPrint(ListWin:thandle;FileToPrint,DefPrinter:pchar; PrintFlags:integer;var Margins:trect):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListPrintW(ListWin:thandle;FileToPrint,DefPrinter:pwidechar; PrintFlags:integer;var Margins:trect):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListNotificationReceived(ListWin:thandle;Message,wParam,lParam:integer):integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
procedure ListSetDefaultParams(dps:pListDefaultParamStruct); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListGetPreviewBitmap(FileToLoad:pchar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
function ListGetPreviewBitmapW(FileToLoad:pwidechar;width,height:integer; contentbuf:pchar;contentbuflen:integer):hbitmap; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

*)

implementation

end.
