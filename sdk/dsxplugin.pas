unit DsxPlugin;

interface

uses
  SysUtils;

type

  TDsxSearchRecord = record
    FileMask: array[0..1024] of AnsiChar;
    Attributes: Cardinal;
    AttribStr: array[0..32] of AnsiChar;
    CaseSensitive: Boolean;
    { Date/time search }
    IsDateFrom,
    IsDateTo,
    IsTimeFrom,
    IsTimeTo: Boolean;
    DateTimeFrom,
    DateTimeTo: TDateTime;
    { File size search }
    IsFileSizeFrom,
    IsFileSizeTo: Boolean;
    FileSizeFrom,
    FileSizeTo: Int64;
    { Find/replace text }
    IsFindText: Boolean;
    FindText: array[0..1024] of AnsiChar;
    IsReplaceText: Boolean;
    ReplaceText: array[0..1024] of AnsiChar;
    NotContainingText: Boolean;
  end;


  tDSXDefaultParamStruct = record
    size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: Longint;
    DefaultIniName: array[0..MAX_PATH - 1] of Char;
  end;
  pDSXDefaultParamStruct = ^tDSXDefaultParamStruct;

  {Prototypes}
  {Callbacks procs}
  TSAddFileProc = procedure(PlugNr: Integer; FoundFile: PChar); Stdcall;
  //if FoundFile='' then searching is finished

  TSUpdateStatusProc = procedure(PlugNr: Integer; CurrentFile: PChar;
    FilesScaned: Integer); Stdcall;

  {Mandatory (must be implemented)}
  {
  function Init(dps:pDSXDefaultParamStruct; pAddFileProc:TSAddFileProc; pUpdateStatus:TSUpdateStatusProc):integer; stdcall;
  procedure StartSearch(FPluginNr:integer; StartPath:pchar; SearchAttrRec:TSearchAttrRecord); stdcall;
  procedure StopSearch(FPluginNr:integer); stdcall;
  procedure Finalize(FPluginNr:integer); stdcall;
  }

implementation

end.

