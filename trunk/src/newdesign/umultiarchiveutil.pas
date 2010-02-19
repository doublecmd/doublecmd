unit uMultiArchiveUtil;

{$mode objfpc}{$H+}

{.$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, uMultiArc, un_process, uFile;

type

  TOnGetArchiveItem = procedure(ArchiveItem: TArchiveItem) of object;

  TKeyPos = record
    Index,
    Start,
    Finish: longint;
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
    FMinPos,
    FSecPos,
    FAttrPos: TKeyPos;
  private
    FOnGetArchiveItem: TOnGetArchiveItem;
    FStartParsing: boolean;
    FFormatIndex: longint;
    FArchiveItem: TArchiveItem;
    FArchiveName: UTF8String;
  protected
    function KeyPos(Key: char; out Position: TKeyPos): boolean;
    procedure OnReadLn(str: string);
    function CheckOut(const SubStr, Str: string): boolean;
  public
    constructor Create(aMultiArcItem: TMultiArcItem; const anArchiveName: UTF8String);
    destructor Destroy; override;
    procedure Prepare;
    procedure Execute;
    property OnGetArchiveItem: TOnGetArchiveItem read FOnGetArchiveItem write FOnGetArchiveItem;
  end;

function FormatArchiverCommand(const Archiver, sCmd, anArchiveName: UTF8String;
                               aFiles: TFiles = nil;
                               sFileName: UTF8String = '';
                               aDestPath: UTF8String = '';
                               sTempFile: UTF8String = ''): string;

implementation

uses
  LCLProc, FileUtil, StrUtils, uClassesEx, uOSUtils;

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
      Position.Finish:= Position.Start;
      while ((Position.Finish <= Length(Format)) and (Format[Position.Finish] = Key)) do
        Inc(Position.Finish);
      Position.Finish := Position.Finish - Position.Start;
      Position.Index := I;
      {$IFDEF DEBUG}
      DebugLn('Key: ', Key, ' Format: ', IntToStr(I), ' Start: ', IntToStr(Position.Start), ' Finish: ', IntToStr(Position.Finish));
      {$ENDIF}
      Result := True;
      Break;
    end;
end;

{ TOutputParser }

procedure TOutputParser.OnReadLn(str: string);
begin
  if FMultiArcItem.FDebug then
    DebugLn(str);

  if str = EmptyStr then Exit; // skip empty lines

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
      FArchiveItem.FileName := Copy(str, FNamePos.Start, FNamePos.Finish);
    if FUnpSizePos.Index = FFormatIndex then
      FArchiveItem.UnpSize := StrToIntDef(Trim(Copy(str, FUnpSizePos.Start, FUnpSizePos.Finish)), 0);
    if FPackSizePos.Index = FFormatIndex then
      FArchiveItem.PackSize := StrToIntDef(Trim(Copy(str, FPackSizePos.Start, FPackSizePos.Finish)), 0);
    if FYearPos.Index = FFormatIndex then
      FArchiveItem.Year := StrToIntDef(Trim(Copy(str, FYearPos.Start, FYearPos.Finish)), 0);
    if FMonthPos.Index = FFormatIndex then
      FArchiveItem.Month := StrToIntDef(Trim(Copy(str, FMonthPos.Start, FMonthPos.Finish)), 0);
    if FMonthNamePos.Index = FFormatIndex then
      FArchiveItem.MonthName := Copy(str, FMonthNamePos.Start, FMonthNamePos.Finish);
    if FDayPos.Index = FFormatIndex then
      FArchiveItem.Day := StrToIntDef(Trim(Copy(str, FDayPos.Start, FDayPos.Finish)), 0);
    if FHourPos.Index = FFormatIndex then
      FArchiveItem.Hour := StrToIntDef(Trim(Copy(str, FHourPos.Start, FHourPos.Finish)), 0);
    if FMinPos.Index = FFormatIndex then
      FArchiveItem.Minute := StrToIntDef(Trim(Copy(str, FMinPos.Start, FMinPos.Finish)), 0);
    if FSecPos.Index = FFormatIndex then
      FArchiveItem.Second := StrToIntDef(Trim(Copy(str, FSecPos.Start, FSecPos.Finish)), 0);
    if FAttrPos.Index = FFormatIndex then
      FArchiveItem.Attributes := StrToAttr(Copy(str, FAttrPos.Start, FAttrPos.Finish));

    FFormatIndex := FFormatIndex + 1;
    if FFormatIndex >= FMultiArcItem.FFormat.Count then
    begin
      FFormatIndex := 0;
      {$IFDEF DEBUG}
      DebugLn('FileName: ', FArchiveItem.FileName);
      DebugLn('Size: ', IntToStr(FArchiveItem.UnpSize));
      DebugLn('Pack size: ', IntToStr(FArchiveItem.PackSize));
      DebugLn('Attributes: ', IntToStr(FArchiveItem.Attributes));
      DebugLn('-------------------------------------');
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
    DebugLn(sCommandLine);

  FExProcess := TExProcess.Create(sCommandLine);
  FExProcess.OnReadLn := @OnReadLn;
end;

function FormatArchiverCommand(const Archiver, sCmd, anArchiveName: UTF8String;
                               aFiles: TFiles;
                               sFileName: UTF8String;
                               aDestPath: UTF8String;
                               sTempFile: UTF8String): string;
type
  TFunctType = (ftNone, ftArchiverLongName, ftArchiverShortName,
    ftArchiveLongName, ftArchiveShortName,
    ftFileListLongName, ftFileListShortName, ftFileName, ftTargetArchiveDir);
  TStatePos = (spNone, spPercent, spFunction, spComplete);
  TFuncModifiers = set of (fmQuoteWithSpaces, fmQuoteAny, fmNameOnly,
    fmPathOnly, fmAnsi);

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
    if not (fmAnsi in state.FuncModifiers) then
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
      if bShort then
        FileList.Add(mbFileNameToSysEnc(aFiles[I].FullPath))
      else
        FileList.Add(aFiles[I].FullPath);
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
        Result := BuildName(Archiver);
      ftArchiverShortName:
        Result := BuildName(mbFileNameToSysEnc(Archiver));
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
          AddParsedText(state.bracketStartIndex)
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
            else
              state.pos := spFunction;
          end;

        spFunction:
          case sCmd[index] of
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

