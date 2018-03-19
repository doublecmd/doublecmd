unit uMultiArchiveUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCConvertEncoding, uMultiArc, un_process, uFile,
  uMultiArchiveParser;

type

  { TOutputParser }

  TOutputParser = class
    FPassword: String;
    FExProcess: TExProcess;
    FMultiArcItem: TMultiArcItem;
    FParser: TMultiArchiveParser;
    FConvertEncoding: function (const Source: String): RawByteString;
  private
    FArchiveName: String;
    FStartParsing: boolean;
    function PrepareCommand: String;
    procedure SetOnGetArchiveItem(AValue: TOnGetArchiveItem);
  protected
    procedure OnProcessExit;
    procedure OnReadLn(str: string);
    procedure OnQueryString(str: string);
    function CheckOut(const SubStr, Str: string): boolean;
  public
    constructor Create(aMultiArcItem: TMultiArcItem; const anArchiveName: String);
    destructor Destroy; override;

    procedure Prepare;
    procedure Execute;

    property Password: String read FPassword write FPassword;
    property OnGetArchiveItem: TOnGetArchiveItem write SetOnGetArchiveItem;
  end;

function ExtractErrorLevel(var Command: String): LongInt;

function FormatArchiverCommand(const Archiver, sCmd, anArchiveName: String;
                               aFiles: TFiles = nil;
                               sFileName: String = '';
                               aDestPath: String = '';
                               sTempFile: String = '';
                               sPassword: String = '';
                               sVolumeSize: String = '';
                               sCustomParams: String = ''): string;

implementation

uses
  LazUTF8, DCClassesUtf8, uDCUtils, DCOSUtils, uOSUtils,
  uDebug, uShowMsg, uLng, uMultiArchiveDynamicParser;

function Utf8ToUtf8(const Source: String): RawByteString;
begin
  Result:= Source;
end;

{ TOutputParser }

function TOutputParser.PrepareCommand: String;
var
  Index: Integer;
begin
  Result:= FMultiArcItem.FList;
  Index:= Pos('%O', Result);
  FConvertEncoding:= @DCOSUtils.ConsoleToUTF8;
  if (Index > 0) and (Index + 2 <= Length(Result)) then
  begin
    case (Result[Index + 2]) of
      'A': FConvertEncoding:= CeSysToUtf8;
      'U': FConvertEncoding:= @Utf8ToUtf8;
    end;
    Delete(Result, Index, 3);
  end;
end;

procedure TOutputParser.SetOnGetArchiveItem(AValue: TOnGetArchiveItem);
begin
  FParser.OnGetArchiveItem:= AValue;
end;

procedure TOutputParser.OnProcessExit;
begin
  FParser.ParseLines;
end;

procedure TOutputParser.OnReadLn(str: string);
begin
  str:= FConvertEncoding(str);

  if FMultiArcItem.FDebug then
    DCDebug(str);

  if (str = EmptyStr) or (Trim(str) = EmptyStr) then Exit; // skip empty lines

  if not FStartParsing then
    FStartParsing := (FMultiArcItem.FStart = EmptyStr); // if not defined start line

  if FStartParsing and (FMultiArcItem.FEnd <> EmptyStr) and CheckOut(FMultiArcItem.FEnd, Str) then
  begin
    FExProcess.Stop;
    Exit;
  end;

  if FStartParsing then
  begin
    FParser.AddLine(Str);
  end
  else
  begin
    FStartParsing := (FMultiArcItem.FStart = EmptyStr) or CheckOut(FMultiArcItem.FStart, Str);
  end;
end;

procedure TOutputParser.OnQueryString(str: string);
var
  pcPassword: PAnsiChar;
begin
  str:= FConvertEncoding(str);
  if not ShowInputQuery(FMultiArcItem.FDescription, rsMsgPasswordEnter, True, FPassword) then
    FExProcess.Stop
  else begin
    pcPassword:= PAnsiChar(UTF8ToConsole(FPassword + LineEnding));
    FExProcess.Process.Input.Write(pcPassword^, Length(pcPassword));
  end;
end;

function TOutputParser.CheckOut(const SubStr, Str: string): boolean;
begin
  if SubStr[1] = '^' then
    Result := (Pos(PChar(SubStr) + 1, Str) = 1)
  else
    Result := (Pos(SubStr, Str) > 0);
end;

constructor TOutputParser.Create(aMultiArcItem: TMultiArcItem;
  const anArchiveName: String);
begin
  FArchiveName := anArchiveName;
  FMultiArcItem := aMultiArcItem;

  if TMultiArchiveDynamicParser.NeedDynamic(FMultiArcItem.FFormat) then
    FParser:= TMultiArchiveDynamicParser.Create(FMultiArcItem)
  else begin
    FParser:= TMultiArchiveStaticParser.Create(FMultiArcItem);
  end;
  DCDebug(FParser.ClassName, '.Create');
end;

destructor TOutputParser.Destroy;
begin
  FreeAndNil(FParser);
  FreeAndNil(FExProcess);
  inherited Destroy;
end;

procedure TOutputParser.Execute;
begin
  FParser.Prepare;
  // execute archiver
  FExProcess.Execute;
end;

procedure TOutputParser.Prepare;
var
  sCommandLine: String;
begin
  FStartParsing:= False;
  FreeAndNil(FExProcess);
  sCommandLine:= PrepareCommand;
  sCommandLine:= FormatArchiverCommand(FMultiArcItem.FArchiver,
                                       sCommandLine, FArchiveName,
                                       nil, '', '','', FPassword);
  if FMultiArcItem.FDebug then
    DCDebug(sCommandLine);

  FExProcess := TExProcess.Create(sCommandLine);
  FExProcess.OnReadLn := @OnReadLn;
  FExProcess.OnProcessExit:= @OnProcessExit;
  if Length(FMultiArcItem.FPasswordQuery) <> 0 then
  begin
    FExProcess.QueryString:= UTF8ToConsole(FMultiArcItem.FPasswordQuery);
    FExProcess.OnQueryString:= @OnQueryString;
  end;
end;

function ExtractErrorLevel(var Command: String): LongInt;
var
  I, J: Integer;
  sErrorLevel: String;
begin
  Result:= 0;
  I:= Pos('%E', Command);
  if I > 0 then
    begin
      J:= I + 2;
      while (J <= Length(Command)) and (Command[J] in ['0'..'9']) do
        Inc(J);
      sErrorLevel:= Copy(Command, I + 2, J - I - 2);
      Delete(Command, I, J - I);
      Result:= StrToIntDef(sErrorLevel, 0);
    end;
end;

function FormatArchiverCommand(const Archiver, sCmd, anArchiveName: String;
                               aFiles: TFiles;
                               sFileName: String;
                               aDestPath: String;
                               sTempFile: String;
                               sPassword: String;
                               sVolumeSize: String;
                               sCustomParams: String): string;
type
  TFunctType = (ftNone, ftArchiverLongName, ftArchiverShortName,
    ftArchiveLongName, ftArchiveShortName,
    ftFileListLongName, ftFileListShortName, ftFileName, ftTargetArchiveDir,
    ftVolumeSize, ftPassword, ftCustomParams);
  TStatePos = (spNone, spPercent, spFunction, spComplete);
  TFuncModifiers = set of (fmOnlyFiles, fmQuoteWithSpaces, fmQuoteAny, fmNameOnly,
    fmPathOnly, fmUTF8, fmAnsi);

  TState = record
    pos: TStatePos;
    functStartIndex,
    bracketStartIndex: integer;
    funct: TFunctType;
    FuncModifiers: TFuncModifiers;
    closeBracket: Boolean;
  end;

var
  index: integer;
  state: Tstate;
  sOutput: string = '';
  parseStartIndex: integer;

  function BuildName(const sFileName: String): String;
  begin
    Result := sFileName;
    if fmNameOnly in state.FuncModifiers then
      Result := ExtractFileName(Result);
    if fmPathOnly in state.FuncModifiers then
      Result := ExtractFilePath(Result);
    if (fmQuoteWithSpaces in state.FuncModifiers) and (Pos(#32, Result) <> 0) then
      Result := '"' + Result + '"';
    if (fmQuoteAny in state.FuncModifiers) then
      Result := '"' + Result + '"';
  end;

  function BuildFileList(bShort: boolean): String;
  var
    I: integer;
    FileName: String;
    FileList: TStringListEx;
  begin
    if not Assigned(aFiles) then Exit(EmptyStr);
    Result := sTempFile;
    FileList := TStringListEx.Create;
    for I := 0 to aFiles.Count - 1 do
    begin
      if aFiles[I].IsDirectory and (fmOnlyFiles in state.FuncModifiers) then
        Continue;
      if bShort then
        FileName := BuildName(mbFileNameToSysEnc(aFiles[I].FullPath))
      else begin
        FileName := BuildName(aFiles[I].FullPath);
      end;
      if (fmAnsi in state.FuncModifiers) then
        FileName := CeUtf8ToSys(FileName)
      else if not (fmUTF8 in state.FuncModifiers) then begin
        FileName := UTF8ToConsole(FileName);
      end;
      FileList.Add(FileName);
    end;
    try
      FileList.SaveToFile(Result);
    except
      Result := EmptyStr;
    end;
    FileList.Free;
  end;

  function BuildOutput: String;
  begin
    case state.funct of
      ftArchiverLongName:
        // TProcess arguments must be enclosed with double quotes and not escaped.
        Result := '"' + mbExpandFileName(Archiver) + '"';
      ftArchiverShortName:
        // TProcess arguments must be enclosed with double quotes and not escaped.
        Result := '"' + mbFileNameToSysEnc(mbExpandFileName(Archiver)) + '"';
      ftArchiveLongName:
        Result := BuildName(anArchiveName);
      ftArchiveShortName:
        Result := BuildName(mbFileNameToSysEnc(anArchiveName));
      ftFileListLongName:
        Result := BuildFileList(False);
      ftFileListShortName:
        Result := BuildFileList(True);
      ftFileName:
        Result:= BuildName(sFileName);
      ftTargetArchiveDir:
        Result := BuildName(aDestPath);
      ftVolumeSize:
        Result:= sVolumeSize;
      ftPassword:
        Result:= sPassword;
      ftCustomParams:
        Result:= sCustomParams;
      else
        Exit('');
    end;
  end;

  procedure ResetState(var aState: TState);
  begin
    with aState do
    begin
      pos := spNone;
      funct := ftNone;
      functStartIndex := 0;
      FuncModifiers := [];
      if closeBracket then
        begin
          closeBracket:= False;
          bracketStartIndex:= 0;
        end;
    end;
  end;

  procedure AddParsedText(limit: integer);
  begin
    // Copy [parseStartIndex .. limit - 1].
    if limit > parseStartIndex then
      sOutput := sOutput + Copy(sCmd, parseStartIndex, limit - parseStartIndex);
    parseStartIndex := index;
  end;

  procedure AddBrackedText(limit: integer);
  begin
    // Copy [state.bracketStartIndex + 1 .. limit - 1].
    if limit > state.bracketStartIndex then
      sOutput := sOutput + Copy(sCmd, state.bracketStartIndex + 1, limit - state.bracketStartIndex - 1);
  end;

  procedure DoFunction;
  var
    aOutput: String;
  begin
    aOutput:= BuildOutput;
    if (aOutput = EmptyStr) and (state.bracketStartIndex <> 0) then
      begin
        AddParsedText(state.bracketStartIndex);
      end
    else
      begin
        if (state.bracketStartIndex <> 0) then
          begin
            // add text before bracket
            AddParsedText(state.bracketStartIndex);
            //add text after bracket
            AddBrackedText(state.functStartIndex);
          end
        else
          AddParsedText(state.functStartIndex);
        sOutput := sOutput + aOutput;
      end;
    ResetState(state);
  end;

begin
  try
    index := 1;
    parseStartIndex := index;
    FillByte(state, SizeOf(state), 0);

    ResetState(state);

    while index <= Length(sCmd) do
    begin
      case state.pos of
        spNone:
          case sCmd[index] of
            '%':
            begin
              state.pos := spPercent;
              state.functStartIndex := index;
            end;
            '{':
            begin
              state.bracketStartIndex := index;
            end;
          end;

        spPercent:
          case sCmd[index] of
            'P':
            begin
              state.funct := ftArchiverLongName;
              state.pos := spFunction;
            end;
            'p':
            begin
              state.funct := ftArchiverShortName;
              state.pos := spFunction;
            end;
            'A':
            begin
              state.funct := ftArchiveLongName;
              state.pos := spFunction;
            end;
            'a':
            begin
              state.funct := ftArchiveShortName;
              state.pos := spFunction;
            end;
            'L':
            begin
              state.funct := ftFileListLongName;
              state.pos := spFunction;
            end;
            'l':
            begin
              state.funct := ftFileListShortName;
              state.pos := spFunction;
            end;
            'F':
            begin
              state.funct := ftFileName;
              state.pos := spFunction;
            end;
            'R':
            begin
              state.funct := ftTargetArchiveDir;
              state.pos := spFunction;
            end;
            'V':
            begin
              state.funct := ftVolumeSize;
              state.pos := spFunction;
            end;
            'W':
            begin
              state.funct := ftPassword;
              state.pos := spFunction;
            end;
            'S':
            begin
              state.funct := ftCustomParams;
              state.pos := spFunction;
            end;
            else
              state.pos := spFunction;
          end;

        spFunction:
          case sCmd[index] of
            'F':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmOnlyFiles];
              state.pos := spFunction;
            end;
            'Q':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmQuoteWithSpaces];
              state.pos := spFunction;
            end;
            'q':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmQuoteAny];
              state.pos := spFunction;
            end;
            'W':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmNameOnly];
              state.pos := spFunction;
            end;
            'P':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmPathOnly];
              state.pos := spFunction;
            end;
            'U':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmUTF8];
              state.pos := spFunction;
            end;
            'A':
            begin
              state.FuncModifiers := state.FuncModifiers + [fmAnsi];
              state.pos := spFunction;
            end;
            '}':
            begin
              state.closeBracket:= True;
            end
            else
              state.pos := spComplete;
          end;
      end;

      if state.pos <> spComplete then
        Inc(index) // check next character
      else
        // Process function and then check current character again after resetting state.
        DoFunction;
    end; // while

    // Finish current parse.
    if state.pos in [spFunction] then
      DoFunction
    else
      AddParsedText(index);

    Result := sOutput;

  finally

  end;
end;

end.

