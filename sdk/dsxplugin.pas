unit DsxPlugin;

interface

uses Sysutils;

type

TSearchAttrRecord=record
    rFileMask : Pchar;
    rAttributes: Cardinal;
    rAttribStr : Pchar;
    rCaseSens:Boolean;
    {Date search}
    rIsDateFrom,
    rIsDateTo : Boolean;
    rDateTimeFrom,
    rDateTimeTo : TDateTime;
    {Time search}
    rIsTimeFrom,
    rIsTimeTo : Boolean;
    (* File size search *)
    rIsFileSizeFrom,
    rIsFileSizeTo : Boolean;
    rFileSizeFrom,
    rFileSizeTo : Int64;
    (* Find text *)
    rIsNoThisText,
    rFindInFiles:Boolean;
    rFindData:Pchar;
    (* Replace text *)
    rReplaceInFiles : Boolean;
    rReplaceData : Pchar;
end;


  tDSXDefaultParamStruct=record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi:longint;
    DefaultIniName:array[0..MAX_PATH-1] of char;
  end;
  pDSXDefaultParamStruct=^tDSXDefaultParamStruct;

{Prototypes}
{Callbacks procs}
TSAddFileProc=procedure (PlugNr:integer; FoundFile:pchar); stdcall; //if FoundFile='' then searching is finished

TSUpdateStatusProc=procedure (PlugNr:integer; CurrentFile:pchar; FilesScaned:integer); stdcall;

{Mandatory (must be implemented)}
{
function Init(dps:pDSXDefaultParamStruct; pAddFileProc:TSAddFileProc; pUpdateStatus:TSUpdateStatusProc):integer; stdcall;
procedure StartSearch(FPluginNr:integer; StartPath:pchar; SearchAttrRec:TSearchAttrRecord); stdcall;
procedure StopSearch(FPluginNr:integer); stdcall;
procedure Finalize(FPluginNr:integer); stdcall;
}

implementation

end.
