unit uMultiArchiveUtil;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, uMultiArc, un_process, uFile, DCBasicTypes;

const
  MAF_UNIX_PATH        = 1; // Use Unix path delimiter (/)
  MAF_WIN_PATH         = 2; // Use Windows path delimiter (\)
  MAF_UNIX_ATTR        = 4; // Use Unix file attributes
  MAF_WIN_ATTR         = 8; // Use Windows file attributes

type

  TOnGetArchiveItem = procedure(ArchiveItem: TArchiveItem) of object;
  TGetFileAttr = function(sAttr: String): TFileAttrs;
  TGetFileName = function(const Str: String): UTF8String;

  TKeyPos = record
    Index,
    Start,
    Count: longint;
  end;

  { TOutputParser }

  TOutputParser = class
    FMultiArcItem: TMultiArcItem;
    FExProcess: TExProcess;
  private
    FNamePos,
    FUnpSizePos,
    FPackSizePos,
    FYearPos,
    FMonthPos,
    FMonthNamePos,
    FDayPos,
    FHourPos,
    FHourModifierPos,
    FMinPos,
    FSecPos,
    FAttrPos: TKeyPos;
  private
    FOnGetArchiveItem: TOnGetArchiveItem;
    FStartParsing: boolean;
    FFormatIndex: longint;
    FArchiveItem: TArchiveItem;
    FArchiveName: UTF8String;
    FGetFileAttr: TGetFileAttr;
    FGetFileName: TGetFileName;
  protected
    function FixPosition(const Str: String; Key: TKeyPos): LongInt;
    function KeyPos(Key: char; out Position: TKeyPos): boolean;
    function GetKeyValue(const str: String; Key: TKeyPos): UTF8String;
    procedure OnReadLn(str: string);
    function CheckOut(const SubStr, Str: string): boolean;
  public
    constructor Create(aMultiArcItem: TMultiArcItem; const anArchiveName: UTF8String);
    destructor Destroy; override;
    procedure Prepare;
    procedure Execute;
    property OnGetArchiveItem: TOnGetArchiveItem read FOnGetArchiveItem write FOnGetArchiveItem;
  end;

function ExtractErrorLevel(var Command: UTF8String): LongInt;

function FormatArchiverCommand(const Archiver, sCmd, anArchiveName: UTF8String;
                               aFiles: TFiles = nil;
                               sFileName: UTF8String = '';
                               aDestPath: UTF8String = '';
                               sTempFile: UTF8String = '';
                               sPassword: UTF8String = '';
                               sVolumeSize: UTF8String = '';
                               sCustomParams: UTF8String = ''): string;

implementation

uses
  LCLProc, FileUtil, StrUtils, DCClassesUtf8, uDCUtils, DCOSUtils, uOSUtils,
  DCDateTimeUtils, uDebug, DCFileAttributes;

function GetUnixFileName(const Str: String): UTF8String;
var
  I: Integer;
begin
  Result:= ConsoleToUTF8(Str);
  for I:= 1 to Length(Str) do
    if Result[I] = '/' then Result[I]:= PathDelim;
end;

function GetWinFileName(const Str: String): UTF8String;
var
  I: Integer;
begin
  Result:= ConsoleToUTF8(Str);
  for I:= 1 to Length(Str) do
    if Result[I] = '\' then Result[I]:= PathDelim;
end;

{ TOutputParser }

function TOutputParser.FixPosition(const Str: String; Key: TKeyPos): LongInt;
var
  I, K, U, C: LongInt;
  Format: String;
begin
  I:= 0;
  U:= 0;
  Result:= Key.Start;
  Format:= FMultiArcItem.FFormat[Key.Index];
  repeat
    C:= 0;
    I:= PosEx('*', Format, I + 1);
    if (I = 0) or (I >= Result) then Exit;
    if (I > 0) then
    begin
      I:= I + U;
      K:= I;
      while (K <= Length(Str)) and (Str[K] <> #32) do
        begin
          Inc(C);
          Inc(K);
        end;
      if C > 0 then
        U:= C - 1
      else
        U:= 0;
      Result:= Result + U;
    end;
  until I = 0;
end;

function TOutputParser.KeyPos(Key: char; out Position: TKeyPos): boolean;
var
  I: integer;
  Format: string;
begin
  Result := False;
  Position.Index := -1;
  for I := 0 to FMultiArcItem.FFormat.Count - 1 do
    with FMultiArcItem do
    begin
      Format := FFormat[I];
      Position.Start := Pos(Key, Format);
      if Position.Start = 0 then
        Continue;
      Position.Count:= Position.Start;
      while ((Position.Count <= Length(Format)) and (Format[Position.Count] = Key)) do
        Inc(Position.Count);
      Position.Count := Position.Count - Position.Start;
      Position.Index := I;
      {$IFDEF DEBUG}
      DCDebug('Key: ', Key, ' Format: ', IntToStr(I), ' Start: ', IntToStr(Position.Start), ' Count: ', IntToStr(Position.Count));
      {$ENDIF}
      Result := True;
      Break;
    end;
end;

function TOutputParser.GetKeyValue(const str: String; Key: TKeyPos): UTF8String;
begin
  Result:= Copy(str, FixPosition(str, Key), Key.Count);
end;

{ TOutputParser }

procedure TOutputParser.OnReadLn(str: string);
begin
  if FMultiArcItem.FDebug then
    DCDebug(str);

  if (str = EmptyStr) or (Trim(str) = EmptyStr) then Exit; // skip empty lines

  if not FStartParsing then
    FStartParsing:= (FMultiArcItem.FStart = EmptyStr); // if not defined start line

  if FStartParsing and (FMultiArcItem.FEnd <> EmptyStr) and CheckOut(FMultiArcItem.FEnd, Str) then
  begin
    FExProcess.Stop;
    Exit;
  end;

  if FStartParsing then
  begin
    // if next item
    if FFormatIndex = 0 then
      FArchiveItem := TArchiveItem.Create;
    // get all file properties
    if FNamePos.Index = FFormatIndex then
      FArchiveItem.FileName := FGetFileName(Trim(GetKeyValue(str, FNamePos)));
    if FUnpSizePos.Index = FFormatIndex then
      FArchiveItem.UnpSize := StrToIntDef(Trim(GetKeyValue(str, FUnpSizePos)), 0);
    if FPackSizePos.Index = FFormatIndex then
      FArchiveItem.PackSize := StrToIntDef(Trim(GetKeyValue(str, FPackSizePos)), 0);
    if FYearPos.Index = FFormatIndex then
      FArchiveItem.Year := YearShortToLong(StrToIntDef(Trim(GetKeyValue(str, FYearPos)), 0));
    if FMonthPos.Index = FFormatIndex then
      FArchiveItem.Month := StrToIntDef(Trim(GetKeyValue(str, FMonthPos)), 0);
    if FMonthNamePos.Index = FFormatIndex then
      FArchiveItem.Month := MonthToNumberDef(GetKeyValue(str, FMonthNamePos), 0);
    if FDayPos.Index = FFormatIndex then
      FArchiveItem.Day := StrToIntDef(Trim(GetKeyValue(str, FDayPos)), 0);
    if FHourPos.Index = FFormatIndex then
      FArchiveItem.Hour := StrToIntDef(Trim(GetKeyValue(str, FHourPos)), 0);
    if FHourModifierPos.Index = FFormatIndex then
      FArchiveItem.Hour := TwelveToTwentyFour(FArchiveItem.Hour, GetKeyValue(str, FHourModifierPos));
    if FMinPos.Index = FFormatIndex then
      FArchiveItem.Minute := StrToIntDef(Trim(GetKeyValue(str, FMinPos)), 0);
    if FSecPos.Index = FFormatIndex then
      FArchiveItem.Second := StrToIntDef(Trim(GetKeyValue(str, FSecPos)), 0);
    if FAttrPos.Index = FFormatIndex then
      FArchiveItem.Attributes := FGetFileAttr(GetKeyValue(str, FAttrPos));

    FFormatIndex := FFormatIndex + 1;
    if FFormatIndex >= FMultiArcItem.FFormat.Count then
    begin
      FFormatIndex := 0;
      {$IFDEF DEBUG}
      DCDebug('FileName: ', FArchiveItem.FileName);
      DCDebug('Size: ', IntToStr(FArchiveItem.UnpSize));
      DCDebug('Pack size: ', IntToStr(FArchiveItem.PackSize));
      DCDebug('Attributes: ', IntToStr(FArchiveItem.Attributes));
      DCDebug('-------------------------------------');
      {$ENDIF}
      if Assigned(FOnGetArchiveItem) then
        FOnGetArchiveItem(FArchiveItem);
    end;
  end
  else
  begin
    FStartParsing := (FMultiArcItem.FStart = EmptyStr) or CheckOut(FMultiArcItem.FStart, Str);
    if FStartParsing then
      FFormatIndex := 0;
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
  const anArchiveName: UTF8String);
begin
  FMultiArcItem := aMultiArcItem;
  FArchiveName:= anArchiveName;
  FExProcess := nil;

  with FMultiArcItem do
  begin
    // Setup function to process file attributes
    if (FFormMode and MAF_UNIX_ATTR) <> 0 then
      FGetFileAttr:= @UnixStrToFileAttr
    else if (FFormMode and MAF_WIN_ATTR) <> 0 then
      FGetFileAttr:= @WinStrToFileAttr
    else
      FGetFileAttr:= @StrToFileAttr;
    // Setup function to process file name
    if ((FFormMode and MAF_UNIX_PATH) <> 0) and (PathDelim <> '/') then
      FGetFileName:= @GetUnixFileName
    else if ((FFormMode and MAF_WIN_PATH) <> 0) and (PathDelim <> '\') then
      FGetFileName:= @GetWinFileName
    else
      FGetFileName:= @ConsoleToUTF8;
  end;
end;

destructor TOutputParser.Destroy;
begin
  FreeThenNil(FExProcess);
  inherited Destroy;
end;

procedure TOutputParser.Execute;
begin
  // get positions of all properties
  KeyPos('n', FNamePos);  // file name
  KeyPos('z', FUnpSizePos); // unpacked size
  KeyPos('p', FPackSizePos); // packed size
  KeyPos('y', FYearPos);
  KeyPos('t', FMonthPos);
  KeyPos('T', FMonthNamePos);
  KeyPos('d', FDayPos);
  KeyPos('h', FHourPos);
  KeyPos('H', FHourModifierPos);
  KeyPos('m', FMinPos);
  KeyPos('s', FSecPos);
  KeyPos('a', FAttrPos);
  // execute archiver
  FExProcess.Execute;
end;

procedure TOutputParser.Prepare;
var
  sCommandLine: UTF8String;
begin
  FStartParsing:= False;
  FFormatIndex:= 0;
  FreeThenNil(FExProcess);
  sCommandLine:= FormatArchiverCommand(FMultiArcItem.FArchiver,
                                       FMultiArcItem.FList, FArchiveName);
  if FMultiArcItem.FDebug then
    DCDebug(sCommandLine);

  FExProcess := TExProcess.Create(sCommandLine);
  FExProcess.OnReadLn := @OnReadLn;
end;

function ExtractErrorLevel(var Command: UTF8String): LongInt;
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

function FormatArchiverCommand(const Archiver, sCmd, anArchiveName: UTF8String;
                               aFiles: TFiles;
                               sFileName: UTF8String;
                               aDestPath: UTF8String;
                               sTempFile: UTF8String;
                               sPassword: UTF8String;
                               sVolumeSize: UTF8String;
                               sCustomParams: UTF8String): string;
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

  function BuildName(const sFileName: UTF8String): UTF8String;
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
    if (fmUTF8 in state.FuncModifiers) then
      Exit;
    if (fmAnsi in state.FuncModifiers) then
      Result := UTF8ToSys(Result)
    else
      Result := UTF8ToConsole(Result);
  end;

  function BuildFileList(bShort: boolean): UTF8String;
  var
    I: integer;
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
        FileList.Add(BuildName(mbFileNameToSysEnc(aFiles[I].FullPath)))
      else
        FileList.Add(BuildName(aFiles[I].FullPath));
    end;
    try
      FileList.SaveToFile(Result);
    except
      Result := EmptyStr;
    end;
    FileList.Free;
  end;

  function BuildOutput: UTF8String;
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
        Result:= UTF8ToConsole(sPassword);
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
    aOutput: UTF8String;
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

