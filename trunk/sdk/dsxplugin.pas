unit DsxPlugin;

interface

uses
  SysUtils;

type

  PDsxSearchRecord = ^TDsxSearchRecord;
  TDsxSearchRecord = record
    StartPath: array[0..1023] of AnsiChar;
    FileMask: array[0..1023] of AnsiChar;
    Attributes: Cardinal;
    AttribStr: array[0..127] of AnsiChar;
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
    FindText: array[0..1023] of AnsiChar;
    IsReplaceText: Boolean;
    ReplaceText: array[0..1023] of AnsiChar;
    NotContainingText: Boolean;
  end;


  TDsxDefaultParamStruct = record
    Size,
    PluginInterfaceVersionLow,
    PluginInterfaceVersionHi: Longint;
    DefaultIniName: array[0..MAX_PATH - 1] of Char;
  end;
  PDsxDefaultParamStruct = ^TDsxDefaultParamStruct;

  { For compatibility with Delphi use $IFDEF's to set calling convention }

  {Prototypes}
  {Callbacks procs}
  TSAddFileProc = procedure(PluginNr: Integer; FoundFile: PChar); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  //if FoundFile='' then searching is finished

  TSUpdateStatusProc = procedure(PluginNr: Integer; CurrentFile: PChar;
    FilesScaned: Integer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  {Mandatory (must be implemented)}
  TSInit = function(dps: PDsxDefaultParamStruct; pAddFileProc: TSAddFileProc;
    pUpdateStatus: TSUpdateStatusProc): Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TSStartSearch = procedure(PluginNr: Integer; pSearchRec: PDsxSearchRecord); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TSStopSearch = procedure(PluginNr: Integer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  TSFinalize = procedure(PluginNr: Integer); {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

implementation

end.

