unit DsxPlugin;

interface

uses
  SysUtils;

type

  PDsxSearchRecord = ^TDsxSearchRecord;
  TDsxSearchRecord = record
    StartPath: array[0..1024] of AnsiChar;
    FileMask: array[0..1024] of AnsiChar;
    Attributes: Cardinal;
    AttribStr: array[0..128] of AnsiChar;
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


  TDsxDefaultParamStruct = record
    Size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: Longint;
    DefaultIniName: array[0..MAX_PATH - 1] of Char;
  end;
  PDsxDefaultParamStruct = ^TDsxDefaultParamStruct;

  {Prototypes}
  {Callbacks procs}
  TSAddFileProc = procedure(PluginNr: Integer; FoundFile: PChar); stdcall;
  //if FoundFile='' then searching is finished

  TSUpdateStatusProc = procedure(PluginNr: Integer; CurrentFile: PChar;
    FilesScaned: Integer); stdcall;

  {Mandatory (must be implemented)}
  TSInit = function(dps: PDsxDefaultParamStruct; pAddFileProc: TSAddFileProc;
    pUpdateStatus: TSUpdateStatusProc): Integer; stdcall;
  TSStartSearch = procedure(PluginNr: Integer; pSearchRec: PDsxSearchRecord); stdcall;
  TSStopSearch = procedure(PluginNr: Integer); stdcall;
  TSFinalize = procedure(PluginNr: Integer); stdcall;

implementation

end.

