{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains some functions for open files in associated applications.

    Copyright (C) 2006-2019 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uShellExecute;

{$mode objfpc}{$H+}

interface

uses
  Classes, uFile, uFileView, fMain;

const
  ASCII_DLE = #16;

type
  TPrepareParameterOption = (ppoNormalizePathDelims, ppoReplaceTilde);
  TPrepareParameterOptions = set of TPrepareParameterOption;

function PrepareParameter(sParam: string; paramFile: TFile = nil; options: TPrepareParameterOptions = []; pbShowCommandLinePriorToExecute: PBoolean = nil; pbRunInTerminal: PBoolean = nil; pbKeepTerminalOpen: PBoolean = nil; pbAbortOperation: PBoolean = nil): string; overload;

{en
   Replace variable parameters that depend on files in panels.
}
function ReplaceVarParams(sSourceStr: string; paramFile: TFile = nil; pbShowCommandLinePriorToExecute: PBoolean = nil; pbRunInTerminal: PBoolean = nil; pbKeepTerminalOpen: PBoolean = nil; pbAbortOperation: PBoolean = nil): string; overload;
{en
   Replace variable parameters that depend on the file in active dir.
}

function ProcessExtCommandFork(sCmd: string; sParams: string = ''; sWorkPath: string = ''; paramFile: TFile = nil; bTerm: boolean = False; bKeepTerminalOpen: boolean = False): boolean;

function ShellExecuteEx(sActionName, sFileName, sActiveDir: string): boolean;

implementation

uses
  //Notes: StrUtils is here first, so because of visibility rules, if a called
  //       routine name is present both in "StrUtils" and one of the following
  //       it will be one of the following that will be used and not the one
  //       "StrUtils". Make sure to let "StrUtils" at the first position.
  //       "StrUtils" is here to have the "PosEx".
  //Lazarus, Free-Pascal, etc.
  StrUtils, Dialogs, SysUtils, Process, UTF8Process, LazUTF8, LConvEncoding,
  DCUnicodeUtils,

  //DC
  uShowMsg, uDCUtils, uLng, uFormCommands, fViewer, fEditor, uShowForm, uGlobs,
  uOSUtils, uFileSystemFileSource, DCOSUtils, DCStrUtils, DCClassesUtf8;

//Dialogs,
//LConvEncoding;


(*
  Functions (without parameters they give output for all selected files):

  Miscellaneous:
    %? - as first parameter only, it will show report to show command line prior to execute
    %% - to use one time the percent sign

  "File related":
  ------------------------------------------------------------------------------
    %f - only filename
    %d - only path of the file
    %z - last directory of path of the file
    %p - path + filename
    %o - only the filename with no extension
    %e - only the file extension
    %v - only relative path + filename

    %D - current path in active or chosen panel
    %Z - last directory of path of active or chosen panel
    %a - address + path + filename
    %A - current address in active or chosen panel
    %F - file list with file name only
    %L - file list with full file name
    %R - file list with relative path + file name
    %F, %L and %R - create a list file in the TEMP directory with the names of the selected
                    files and directories, and appends the name of the list file to the command line

  "Choosing encoding" for %F, $L and %R (if not given, system encoding used):
  ---------------------------------------------------------------------------
  %X[U|W|Q] - where X is function %F, %L or %R
    U - UTF-8,
    W - UTF-16 (with byte order marker),
    Q - quote file name by double quotes

  "Choosing panel" (if not given, active panel is used):
  ------------------------------------------------------------------------------
  %X[l|r|s|t] - where X is function (l - left, r - right, s - source, t - target)
    s - source or active panel (no matter if it's left or right)
    t - target or inactive panel (no matter if it's left or right)
    l - left panel
    r - right panel
    b - both panels, left first, right second
    p - both panels, active first, inactive second

  "Choosing selected files" (only for %f, %d, %p, %o and %e):
  ------------------------------------------------------------------------------
  %X[<nr>] - where X is function
    <nr> is 1..n, where n is number of selected files.
    Also <nr> can be 0, file under cursor is used in this case.
    If there are no selected files, currently active file is nr 1.
    If <nr> is invalid or there is no selected file by that number the result for the whole function will be empty string.

  "Adding prefix, postfix before or after the result string":
  ------------------------------------------------------------------------------
  %X[{<prefix>}][{<postfix>}]
    If applied to multiple files, each name is prefixed/postfixed.

  Control if %f, %d, etc... will return name between quotes or not
  ----------------------------------------------------------------
   %" - will set default. For DC legacy is quoted
   %"0 - will make the result unquoted
   %"1 - will make the result quoted

  Control if %D, %d etc... will return path name with the ending delimiter or not
  -------------------------------------------------------------------------------
   %/ - will set default. For DC legacy it was without ending delimited
   %/0 - will exclude the ending delimiter
   %/1 - will include the ending delimiter

   Prompt the user with a sentence, propose a default, and use what the user typed
   -------------------------------------------------------------------------------
   %[This required the \\DB-2010\ server to be online!] - if no default value is given, DC will simply shows the message, assuming it's simply to echo a message.
   %[Enter with required;1024] - This is an example. The text following the ";" indicates that default value is 1024
   %[First name;%o] - The text proposed in the parameter value may be parsed prior to be displayed to user. For example here, the %o will be substituted to the filename with no extension prior to be displayed to user.

   Control what will be the effective "%" char (for situation where we want the "%" to be the "#" sign instead
   -----------------------------------------------------------------------------------------------------------
   %# - Will set the percent-variable indicator to be the "#" character from now on when evaluating the line.
        Note that it will be evaluated -only- when the current percent-variable indicator is "%".
   #% - Will set the percent-variable indicator to be the "%" character from now on when evaluating the line.
        Note that it will be evaluated -only- when the current percent-variable indicator is "#".

   Control if it run in terminal, if it close it at the end or not
   ---------------------------------------------------------------
   %t - Will have it run in terminal for sure, close or not depend of the action requested
   %t0 - Will run in terminal AND request to close it at the end
   %t1 - Will run in terminal AND let it run at the end

  Above parameters can be combined together.
  ------------------------------------------------------------------------------
  Order of params:
  - %function
  - quoting and encoding (only for %F, %L and %R)
  - left or right or source or target panel (optional)
  - nr of file (optional)
  - prefix, postfix (optional)

  Examples:
    %f1       - first selected file in active panel
    %pr2      - full path of second selected file in right panel
    %fl       - only filenames from left panel
    %pr       - full filenames from right panel
    %Dl       - current path in left panel
    %f{-f }   - prepend each name with "-f "
                (ex.: -f <file_1> -f <file_2>)
    %f{"}{"}  - enclose each name in quotes
                (ex.: "<file_1>" "<file_2>")
    %f1{-first }%f2{ -second }
         - if only 1 file selected      : -first <file_1>
         - if 2 (or more) files selected: -first <file_1> -second <file_2>
*)
function ReplaceVarParams(sSourceStr: string; paramFile: TFile; pbShowCommandLinePriorToExecute: PBoolean; pbRunInTerminal: PBoolean; pbKeepTerminalOpen: PBoolean; pbAbortOperation: PBoolean = nil): string;
type
  TFunctType = (ftNone, ftName, ftDir, ftLastDir, ftPath, ftSingleDir, ftLastSingleDir, ftSource, ftSourcePath,
    ftFileFullList, ftFileNameList, ftRelativeFileNameList,
    ftNameNoExt, ftExtension, ftRelativePath,
    ftProcessPercentSignIndicator, ftJustSetTheShowFlag,
    ftSetTrailingDelimiter, ftSetQuoteOrNot, ftSetTerminalOptions,
    ftEchoSimpleMessage, ftPromptUserForParam, ftExecuteConsoleCommand);
  TFuncModifiers = set of (fmQuote, fmUTF8, fmUTF16);
  TStatePos = (spNone, spPercent, spFunction, spPrefix, spPostfix,
    spGotPrefix, spSide, spIndex, spUserInputOrEcho,
    spGotInputHintWaitEndDefaultVal, spGetExecuteConsoleCommand,
    spComplete);

  var
    leftFiles: TFiles = nil;
    rightFiles: TFiles = nil;
    singleFileFiles: TFiles = nil;
    leftFile: TFile;
    rightFile: TFile;
    activeFile: TFile;
    inactiveFile: TFile;
    activeFiles: TFiles;
    inactiveFiles: TFiles;
    activeDir: string;
    inactiveDir: string;
    activeAddress: string;
    inactiveAddress: string;
    bTrailingDelimiter: boolean = False;
    bQuoteOrNot: boolean = True;
    CurrentPercentIndicator: char = '%';
    bKeepProcessParameter:boolean = true;

  // There is a inside recursive function because of the %[ that could have a parameter that could be parsed using the same function.
  // It would have been possible to simply call again "ReplaceVarParams" without an inner function...
  // ...but there are a few things that would have not work as what the user would expect.
  // For example, if user would have wrote previously %"0 to have the following not include the quote, by simply recalling "ReplaceVarParams" itself, if he would have used then a %o in the default parameter value for the %[ , the filename would have been quoted again since it's the default when entering into the "ReplaceVarParams" function originally...
  // Same similar problem with the bTrailingDelimiter, etc.
  // So that's why there is an inner recursive functions where the kind of local-global flag like the ones mentionned above have to be global for the current parsed string.
  function InnerRecursiveReplaceVarParams(sSourceStr: string; paramFile: TFile; pbShowCommandLinePriorToExecute: PBoolean; pbRunInTerminal: PBoolean; pbKeepTerminalOpen: PBoolean; pbAbortOperation: PBoolean = nil): string;
  type
    Tstate = record
      pos: TStatePos;
      functStartIndex: integer;
      funct: TFunctType;
      functMod: TFuncModifiers;
      files: TFiles;
      otherfiles: TFiles;
      fil: TFile;
      otherfil: TFile;
      dir: string;
      address: string;
      sFileIndex: string;
      prefix, postfix: string; // a string to add before/after each output
      // (for functions giving output of multiple strings)
      sSubParam: string;
      sUserMessage: string;
    end;

  var
    index: integer;
    state: Tstate;
    sOutput: string = '';
    parseStartIndex: integer;

    function BuildName(aFile: TFile): string;
    begin
      //1. Processing according to function requested
      case state.funct of
        ftName, ftDir, ftLastDir, ftPath, ftNameNoExt, ftExtension, ftSingleDir, ftLastSingleDir, ftRelativePath, ftSource, ftSourcePath:
        begin
          case state.funct of
            ftName:
              Result := aFile.Name;

            ftDir:
              Result := aFile.Path;

            ftLastDir:
              Result := GetLastDir(aFile.Path);

            ftPath:
              Result := aFile.FullPath;

            ftNameNoExt:
              Result := aFile.NameNoExt;

            ftExtension:
              Result := aFile.Extension;

            ftRelativePath:
              Result := ExtractRelativepath(state.dir, aFile.FullPath);

            ftSingleDir:
              Result := state.dir;

            ftLastSingleDir:
              Result := GetLastDir(state.dir);

            ftSource:
              Result := state.address;

            ftSourcePath:
              Result := state.address + aFile.FullPath;
          end;
        end;
        else
          Exit('');
      end;

      //2. Processing the prefix/postfix requested
      Result := state.prefix + Result + state.postfix;

      //3. Processing the trailing path delimiter requested
      case state.funct of
        ftDir, ftLastDir, ftSingleDir, ftLastSingleDir:
        begin
          if bTrailingDelimiter then
            Result := IncludeTrailingPathDelimiter(Result)
          else
            Result := ExcludeBackPathDelimiter(Result);
        end;
      end;

      //4. Processing the quotes requested
      if bQuoteOrNot then
        Result := QuoteStr(Result);
    end;

    function BuildAllNames: string;
    var
      i: integer;
    begin
      Result := '';

      if Assigned(state.files) then
        for i := 0 to pred(state.files.Count) do
          Result := ConcatenateStrWithSpace(Result, BuildName(state.files[i]));

      if Assigned(state.otherfiles) then
        for i := 0 to pred(state.otherfiles.Count) do
          Result := ConcatenateStrWithSpace(Result, BuildName(state.otherfiles[i]));
    end;

    function BuildFile(aFile: TFile): string;
    begin
      case state.funct of
        ftFileFullList: Result := aFile.FullPath;
        ftFileNameList: Result := aFile.Name;
        ftRelativeFileNameList: Result := ExtractRelativepath(state.dir, aFile.FullPath);
        else
          Result := aFile.Name;
      end;

      if aFile.isDirectory then
      begin
        if bTrailingDelimiter then
          Result := IncludeTrailingPathDelimiter(Result)
        else
          Result := ExcludeBackPathDelimiter(Result);
      end;

      if (fmQuote in state.functMod) then
        Result := '"' + Result + '"';

      if (fmUTF16 in state.functMod) then
        Result := Utf8ToUtf16LE(Result)
      else if not (fmUTF8 in state.functMod) then
        Result := UTF8ToSys(Result);
    end;

    function BuildFileList: String;
    var
      I: integer;
      FileName: ansistring;
      FileList: TFileStreamEx;
      LineEndingA: ansistring = LineEnding;
    begin
      Result := GetTempName(GetTempFolderDeletableAtTheEnd + 'Filelist') + '.lst';
      try
        FileList := TFileStreamEx.Create(Result, fmCreate);
        try
          if fmUTF16 in state.functMod then
          begin
            FileName := UTF16LEBOM;
            LineEndingA := Utf8ToUtf16LE(LineEnding);
          end;

          if Assigned(state.files) then
          begin
            if state.files.Count > 0 then
            begin
              for I := 0 to state.files.Count - 1 do
                FileName += BuildFile(state.files[I]) + LineEndingA;
            end;
          end;

          if Assigned(state.otherfiles) then
          begin
            if state.otherfiles.Count > 0 then
            begin
              FileName += LineEndingA;

              for I := 0 to state.otherfiles.Count - 1 do
                FileName += BuildFile(state.otherfiles[I]) + LineEndingA;
            end;
          end;

          FileList.Write(FileName[1], Length(FileName));
        finally
          FileList.Free;
        end;
      except
        Result := EmptyStr;
      end;
    end;

    procedure ResetState(var aState: TState);
    begin
      with aState do
      begin
        pos := spNone;
        fil := activeFile;
        otherfil := nil;
        if paramFile <> nil then
          files := singleFileFiles
        else
          files := activeFiles;
        otherfiles := nil;
        dir := activeDir;
        address := activeAddress;
        sFileIndex := '';
        funct := ftNone;
        functMod := [];
        functStartIndex := 0;
        prefix := '';
        postfix := '';
        sSubParam := '';
        sUserMessage := '';
      end;
    end;

    procedure AddParsedText(limit: integer);
    begin
      // Copy [parseStartIndex .. limit - 1].
      if limit > parseStartIndex then
        sOutput := sOutput + Copy(sSourceStr, parseStartIndex, limit - parseStartIndex);
      parseStartIndex := index;
    end;

    procedure SetTrailingPathDelimiter;
    begin
      bTrailingDelimiter := state.sSubParam = '1';
      // Currently in the code, anything else than "0" will include the trailing delimiter.
      // BUT, officially, in the documentation, just state that 0 or 1 is required.
      // This could give room for future addition maybe.
    end;

    procedure SetQuoteOrNot;
    begin
      bQuoteOrNot := not (state.sSubParam = '0');
      // Currently in the code, anything else than "0" will indicate we want to quote
      // BUT, officially, in the documentation, just state that 0 or 1 is required.
      // This could give room for future addition maybe.
    end;

    procedure SetTerminalOptions;
    begin
      if pbRunInTerminal <> nil then
      begin
        pbRunInTerminal^ := True;
        if pbKeepTerminalOpen <> nil then
          pbKeepTerminalOpen^ := not (state.sSubParam = '0');
      end;
    end;

    procedure JustEchoTheMessage;
    begin
      state.sUserMessage := InnerRecursiveReplaceVarParams(state.sUserMessage, paramFile, pbShowCommandLinePriorToExecute, pbRunInTerminal, pbKeepTerminalOpen, pbAbortOperation);
      msgOK(state.sUserMessage);
    end;

    procedure AskUserParamAndReplace;
    begin
      state.sSubParam := InnerRecursiveReplaceVarParams(state.sSubParam, paramFile, pbShowCommandLinePriorToExecute, pbRunInTerminal, pbKeepTerminalOpen, pbAbortOperation);
      if ShowInputQuery(rsMsgCofirmUserParam, state.sUserMessage, state.sSubParam) then
      begin
        sOutput := sOutput + state.sSubParam;
      end
      else
      begin
        bKeepProcessParameter:=False;
      end;
    end;

    procedure ExecuteConsoleCommand;
    var
      sTmpFilename, sShellCmdLine: string;
      Process: TProcessUTF8;
    begin
      sTmpFilename := GetTempName(GetTempFolderDeletableAtTheEnd) + '.tmp';
      //sShellCmdLine := Copy(state.sSubParam, 3, length(state.sSubParam)-2) + ' > ' + QuoteStr(sTmpFilename);
      sShellCmdLine := state.sSubParam + ' > ' + QuoteStr(sTmpFilename);
      Process := TProcessUTF8.Create(nil);
      try
        Process.CommandLine := FormatShell(sShellCmdLine);
        Process.Options := [poNoConsole, poWaitOnExit];
        Process.Execute;
      finally
        Process.Free;
      end;
      sOutput := sOutput + sTmpFilename;
    end;

    procedure ProcessPercentSignIndicator;
    begin
      if CurrentPercentIndicator = state.sSubParam then
        sOutput := sOutput + state.sSubParam
      else
      if CurrentPercentIndicator = '%' then
        CurrentPercentIndicator := '#'
      else
        CurrentPercentIndicator := '%';
    end;

    procedure DoFunction;
    var
      fileIndex: integer = -2;
      OffsetFromStart: integer = 0;
    begin
      AddParsedText(state.functStartIndex);

      if state.sFileIndex <> '' then
        try
          fileIndex := StrToInt(state.sFileIndex);
          fileIndex := fileIndex - 1; // Files are counted from 0, but user enters 1..n.
        except
          on EConvertError do
            fileIndex := -2;
        end;

      if fileIndex = -1 then
      begin
        if Assigned(state.fil) then
          sOutput := sOutput + BuildName(state.fil);
        if Assigned(state.otherfil) then
          sOutput := ConcatenateStrWithSpace(sOutput, BuildName(state.otherfil));
      end
      else if fileIndex > -1 then
      begin
        if (fileIndex >= 0) and Assigned(state.files) then
        begin
          if fileIndex < state.files.Count then
            sOutput := sOutput + BuildName(state.files[fileIndex]);
          OffsetFromStart := state.files.Count;
        end;

        if ((fileIndex - OffsetFromStart) >= 0) and Assigned(state.otherfiles) then
          if (fileIndex - OffsetFromStart) < state.otherfiles.Count then
            sOutput := sOutput + BuildName(state.otherfiles[fileIndex - OffsetFromStart]);
      end
      else
      begin
        if state.funct in [ftName, ftPath, ftDir, ftLastDir, ftNameNoExt, ftSourcePath, ftExtension, ftRelativePath] then
          sOutput := sOutput + BuildAllNames
        else if state.funct in [ftSingleDir, ftLastSingleDir, ftSource] then // only single current dir
          sOutput := sOutput + BuildName(nil)
        else if state.funct in [ftFileFullList, ftFileNameList, ftRelativeFileNameList] then // for list of file
          sOutput := sOutput + BuildFileList
        else if state.funct in [ftProcessPercentSignIndicator] then // only add % sign
          ProcessPercentSignIndicator
        else if state.funct in [ftJustSetTheShowFlag] then //only set the flag to show the params prior to execute
        begin
          if pbShowCommandLinePriorToExecute <> nil then
            pbShowCommandLinePriorToExecute^ := True;
        end
        else if state.funct in [ftSetTrailingDelimiter] then //set the trailing path delimiter
          SetTrailingPathDelimiter
        else if state.funct in [ftSetQuoteOrNot] then
          SetQuoteOrNot
        else if state.funct in [ftEchoSimpleMessage] then
          JustEchoTheMessage
        else if state.funct in [ftPromptUserForParam] then
          AskUserParamAndReplace
        else if state.funct in [ftSetTerminalOptions] then
          SetTerminalOptions
        else if state.funct in [ftExecuteConsoleCommand] then
          ExecuteConsoleCommand;
      end;

      ResetState(state);
    end;

    procedure ProcessNumber;
    begin
      case state.funct of
        ftSingleDir, ftLastSingleDir: state.pos := spComplete; // Numbers not allowed for %D and %Z
        ftSetTrailingDelimiter, ftSetQuoteOrNot, ftSetTerminalOptions:
        begin
          state.sSubParam := state.sSubParam + sSourceStr[index];
          state.pos := spComplete;
          Inc(Index);
        end;
        else
        begin
          state.sFileIndex := state.sFileIndex + sSourceStr[index];
          state.pos := spIndex;
        end;
      end;
    end;

    procedure ProcessOpenBracket; // '{'
    begin
      if state.pos <> spGotPrefix then
        state.pos := spPrefix
      else
        state.pos := spPostfix;
    end;

  begin
    index := 1;
    parseStartIndex := index;

    ResetState(state);

    while (index <= Length(sSourceStr)) AND (bKeepProcessParameter) do
    begin
      case state.pos of
        spNone:
          if sSourceStr[index] = CurrentPercentIndicator then
          begin
            state.pos := spPercent;
            state.functStartIndex := index;
          end;

        spPercent:
          case sSourceStr[index] of
            '?':
            begin
              state.funct := ftJustSetTheShowFlag;
              state.pos := spComplete;
              Inc(Index);
            end;

            ASCII_DLE:
            begin
              AddParsedText(state.functStartIndex);
              parseStartIndex := Index + 1;
              Index := Length(sSourceStr) + 1;
              state.pos := spComplete;
              Break;
            end;

            '%', '#':
            begin
              state.funct := ftProcessPercentSignIndicator;
              state.sSubParam := sSourceStr[index];
              state.pos := spComplete;
              Inc(Index);
            end;

            'f', 'd', 'z', 'p', 'o', 'e', 'v', 'D', 'Z', 'A', 'a', 'n', 'h', '/', '"', 't':
            begin
              case sSourceStr[index] of
                'f': state.funct := ftName;
                'd': state.funct := ftDir;
                'z': state.funct := ftLastDir;
                'p': state.funct := ftPath;
                'o': state.funct := ftNameNoExt;
                'e': state.funct := ftExtension;
                'v': state.funct := ftRelativePath;
                'D': state.funct := ftSingleDir;
                'Z': state.funct := ftLastSingleDir;
                'A': state.funct := ftSource;
                'a': state.funct := ftSourcePath;
                '/': state.funct := ftSetTrailingDelimiter;
                '"': state.funct := ftSetQuoteOrNot;
                't': state.funct := ftSetTerminalOptions;
              end;
              state.pos := spFunction;
            end;

            'L', 'F', 'R':
            begin
              case sSourceStr[index] of
                'L': state.funct := ftFileFullList;
                'F': state.funct := ftFileNameList;
                'R': state.funct := ftRelativeFileNameList;
              end;
              state.pos := spFunction;
            end;

            '[':
            begin
              state.pos := spUserInputOrEcho;
            end;

            '<':
            begin
              state.pos := spGetExecuteConsoleCommand;
            end;
            else
              ResetState(state);
          end;

        spFunction:
          case sSourceStr[index] of
            'l', 'b':
            begin
              state.files := leftFiles;
              state.fil := leftFile;
              state.dir := frmMain.FrameLeft.CurrentPath;
              state.address := frmMain.FrameLeft.CurrentAddress;
              state.pos := spSide;
              if sSourceStr[index] = 'b' then
              begin
                state.otherfiles := rightFiles;
                state.otherfil := rightFile;
              end;
            end;

            'r':
            begin
              state.files := rightFiles;
              state.fil := rightFile;
              state.dir := frmMain.FrameRight.CurrentPath;
              state.address := frmMain.FrameRight.CurrentAddress;
              state.pos := spSide;
            end;

            's', 'p':
            begin
              state.files := activeFiles;
              state.fil := activeFile;
              state.dir := activeDir;
              state.address := activeAddress;
              state.pos := spSide;
              if sSourceStr[index] = 'p' then
              begin
                state.otherfil := inactiveFile;
                state.otherfiles := inactiveFiles;
              end;
            end;

            't':
            begin
              state.files := inactiveFiles;
              state.fil := inactiveFile;
              state.dir := inactiveDir;
              state.address := inactiveAddress;
              state.pos := spSide;
            end;

            'U':
            begin
              state.functMod += [fmUTF8];
              state.pos := spFunction;
            end;

            'W':
            begin
              state.functMod += [fmUTF16];
              state.pos := spFunction;
            end;

            'Q':
            begin
              state.functMod += [fmQuote];
              state.pos := spFunction;
            end;

            '0'..'9':
              ProcessNumber;

            '{':
              ProcessOpenBracket;

            else
              state.pos := spComplete;
          end;

        spSide:
          case sSourceStr[index] of
            '0'..'9':
              ProcessNumber;
            '{':
              ProcessOpenBracket;
            else
              state.pos := spComplete;
          end;

        spIndex:
          case sSourceStr[index] of
            '0'..'9':
              ProcessNumber;
            '{':
              ProcessOpenBracket;
            else
              state.pos := spComplete;
          end;

        spPrefix, spPostfix:
          case sSourceStr[index] of
            '}':
            begin
              if state.pos = spPostfix then
              begin
                Inc(index); // include closing bracket in the function
                state.pos := spComplete;
              end
              else
                state.pos := spGotPrefix;
            end;
            else
            begin
              case state.pos of
                spPrefix:
                  state.prefix := state.prefix + sSourceStr[index];
                spPostfix:
                  state.postfix := state.postfix + sSourceStr[index];
              end;
            end;
          end;

        spGotPrefix:
          case sSourceStr[index] of
            '{':
              ProcessOpenBracket;
            else
              state.pos := spComplete;
          end;

        spUserInputOrEcho:
        begin
          case sSourceStr[index] of
            ';':
            begin
              state.pos := spGotInputHintWaitEndDefaultVal;
            end;

            ']':
            begin
              state.funct := ftEchoSimpleMessage;
              state.pos := spComplete;
              Inc(Index);
            end;
            else
              State.sUserMessage := State.sUserMessage + sSourceStr[index];
          end;
        end;

        spGotInputHintWaitEndDefaultVal:
        begin
          case sSourceStr[index] of
            ']':
            begin
              state.funct := ftPromptUserForParam;
              state.pos := spComplete;
              Inc(Index);
            end;
            else
              State.sSubParam := State.sSubParam + sSourceStr[index];
          end;
        end;

        spGetExecuteConsoleCommand:
        begin
          case sSourceStr[index] of
            '>':
            begin
              state.funct := ftExecuteConsoleCommand;
              state.pos := spComplete;
              Inc(Index);
            end;
            else
              State.sSubParam := State.sSubParam + sSourceStr[index];
          end;
        end;
      end;

      if state.pos <> spComplete then
        Inc(index) // check next character
      else
        // Process function and then check current character again after resetting state.
        DoFunction;
    end;

    // Finish current parse.
    if bKeepProcessParameter then
    begin
      if state.pos in [spFunction, spSide, spIndex, spGotPrefix] then
        DoFunction
      else
        AddParsedText(index);
    end;

    if bKeepProcessParameter then
      Result := sOutput
    else
      if pbAbortOperation<>nil then
        pbAbortOperation^ := True;
  end;

begin
  result := '';
  try
    leftFiles := frmMain.FrameLeft.CloneSelectedOrActiveFiles;
    rightFiles := frmMain.FrameRight.CloneSelectedOrActiveFiles;
    if paramFile <> nil then
    begin
      singleFileFiles := TFiles.Create(paramFile.Path);
      singleFileFiles.Add(paramFile.Clone);
    end;

    leftFile:= frmMain.FrameLeft.CloneActiveFile;
    rightFile:= frmMain.FrameRight.CloneActiveFile;
    if Assigned(leftFile) and (not leftFile.IsNameValid) then
      FreeAndNil(leftFile);
    if Assigned(rightFile) and (not rightFile.IsNameValid) then
      FreeAndNil(rightFile);

    if frmMain.ActiveFrame = frmMain.FrameLeft then
    begin
      activeFiles := leftFiles;
      activeFile:= leftFile;
      inactiveFile:= rightFile;
      activeDir := frmMain.FrameLeft.CurrentPath;
      activeAddress := frmMain.FrameLeft.CurrentAddress;
      inactiveFiles := rightFiles;
      inactiveDir := frmMain.FrameRight.CurrentPath;
      inactiveAddress := frmMain.FrameRight.CurrentAddress;
    end
    else
    begin
      activeFiles := rightFiles;
      activeFile:= rightFile;
      inactiveFile:= leftFile;
      activeDir := frmMain.FrameRight.CurrentPath;
      activeAddress := frmMain.FrameRight.CurrentAddress;
      inactiveFiles := leftFiles;
      inactiveDir := frmMain.FrameLeft.CurrentPath;
      inactiveAddress := frmMain.FrameLeft.CurrentAddress;
    end;

    result:=InnerRecursiveReplaceVarParams(sSourceStr, paramFile, pbShowCommandLinePriorToExecute, pbRunInTerminal, pbKeepTerminalOpen, pbAbortOperation);

  finally
    FreeAndNil(leftFile);
    FreeAndNil(rightFile);
    FreeAndNil(leftFiles);
    FreeAndNil(rightFiles);
    FreeAndNil(singleFileFiles);
  end;
end;

{ PrepareParameter }
function PrepareParameter(sParam: string; paramFile: TFile; options: TPrepareParameterOptions; pbShowCommandLinePriorToExecute: PBoolean; pbRunInTerminal: PBoolean; pbKeepTerminalOpen: PBoolean; pbAbortOperation: PBoolean = nil): string; overload;
begin
  Result := sParam;

  if ppoNormalizePathDelims in Options then
    Result := NormalizePathDelimiters(Result);

  if ppoReplaceTilde in Options then
    Result := ReplaceTilde(Result);

  Result := ReplaceEnvVars(Result);

  Result := ReplaceVarParams(Result, paramFile, pbShowCommandLinePriorToExecute, pbRunInTerminal, pbKeepTerminalOpen,pbAbortOperation);

  Result := Trim(Result);
end;

{ ProcessExtCommandFork }
function ProcessExtCommandFork(sCmd, sParams, sWorkPath: string; paramFile: TFile; bTerm: boolean; bKeepTerminalOpen: boolean): boolean;
var
  sTmpFile, sShellCmdLine: string;
  iStart, iCount: integer;
  iLastConsoleCommandPos: integer = 0;
  Process: TProcessUTF8;
  sl: TStringList;
  bShowCommandLinePriorToExecute: boolean = False;
  bAbortOperationFlag: boolean = false;
begin
  Result := False;

  // 1. Parse the command, parameters and working directory for the percent-variable substitution.
  sCmd := PrepareParameter(sCmd, paramFile, [ppoReplaceTilde]);
  sParams := PrepareParameter(sParams, paramFile, [], @bShowCommandLinePriorToExecute, @bTerm, @bKeepTerminalOpen, @bAbortOperationFlag);
  if not bAbortOperationFlag then sWorkPath := PrepareParameter(sWorkPath, paramFile, [ppoNormalizePathDelims, ppoReplaceTilde]);

  // 2. If working directory has been specified, let's switch to it.
  if not bAbortOperationFlag then
  begin
    if sWorkPath <> '' then
      mbSetCurrentDir(sWorkPath);

    // 3. If user has command-line to execute and get the result to a file, let's execute it.
    // Check for <? ?> command.
    // This command is used to put output of some console program to a file so
    //   that the file can then be viewed. The command is between '<?' and '?>'.
    // The whole <?...?> expression is replaced with a path to the temporary file
    //   containing output of the command.
    // For example:
    // {!VIEWER} <?rpm -qivlp --scripts %p?>
    //  Show in Viewer information about RPM package
    repeat
      iStart := Posex('<?', sParams, (iLastConsoleCommandPos + 1)) + 2;
      iCount := Posex('?>', sParams, iStart) - iStart;
      if (iStart <> 0) and (iCount >= 0) then
      begin
        sTmpFile := GetTempName(GetTempFolderDeletableAtTheEnd) + '.tmp';
        sShellCmdLine := Copy(sParams, iStart, iCount) + ' > ' + QuoteStr(sTmpFile);
        Process := TProcessUTF8.Create(nil);
        try
          Process.CommandLine := FormatShell(sShellCmdLine);
          Process.Options := [poWaitOnExit];
          Process.ShowWindow := swoHide;
          Process.Execute;
        finally
          Process.Free;
        end;
        sParams := Copy(sParams, 1, iStart - 3) + sTmpFile + Copy(sParams, iStart + iCount + 2, MaxInt);
        iLastConsoleCommandPos := iStart;
      end;
    until ((iStart = 0) or (iCount < 0));

    //4. If user user wanted to execute an internal command, let's do it.
    if frmMain.Commands.Commands.ExecuteCommand(sCmd, [sParams]) = cfrSuccess then
    begin
      Result := True;
      exit;
    end;

    //5. From legacy, invoking shell seems to be similar to "run in terminal with stay open" with param as-is
    if Pos('{!SHELL}', sCmd) > 0 then
    begin
      sCmd := sParams;
      sParams := '';
      bTerm := True;
      bKeepTerminalOpen := True;
    end;

    //6. If user wants to process via terminal (and close at the end), let's flag it.
    if Pos('{!TERMANDCLOSE}', sCmd) > 0 then
    begin
      sCmd := RemoveQuotation(sParams);
      sParams := '';
      bTerm := True;
    end;

    //7. If user wants to process via terminal (and close at the end), let's flag it.
    if Pos('{!TERMSTAYOPEN}', sCmd) > 0 then
    begin
      sCmd := RemoveQuotation(sParams);
      sParams := '';
      bTerm := True;
      bKeepTerminalOpen := True;
    end;

    //8. If our end-job is to EDIT a file via what's configured as editor, let's do it.
    if Pos('{!EDITOR}', sCmd) > 0 then
    begin
      uShowForm.ShowEditorByGlob(RemoveQuotation(sParams));
      Result := True;
      Exit;
    end;

    //9. If our end-job is to EDIT a file via internal editor, let's do it.
    if Pos('{!DC-EDITOR}', sCmd) > 0 then
    begin
      fEditor.ShowEditor(RemoveQuotation(sParams));
      Result := True;
      Exit;
    end;

    //10. If our end-job is to VIEW a file via what's configured as viewer, let's do it.
    if Pos('{!VIEWER}', sCmd) > 0 then
    begin
      uShowForm.ShowViewerByGlob(RemoveQuotation(sParams));
      Result := True;
      Exit;
    end;

    //11. If our end-job is to VIEW a file or files via internal viewer, let's do it.
    if Pos('{!DC-VIEWER}', sCmd) > 0 then
    begin
      sl := TStringList.Create;
      try
        sl.Add(RemoveQuotation(sParams));
        fViewer.ShowViewer(sl);
        Result := True;
      finally
        FreeAndNil(sl);
      end;
      Exit;
    end;

    //12. Ok. If we're here now it's to execute something external so let's launch it!
    try
      Result := ExecCmdFork(sCmd, sParams, sWorkPath, bShowCommandLinePriorToExecute, bTerm, bKeepTerminalOpen);
    except
      on e: EInvalidCommandLine do
      begin
        MessageDlg(rsMsgInvalidCommandLine, rsMsgInvalidCommandLine + ': ' + e.Message, mtError, [mbOK], 0);
        Result := False;
      end;
    end;

  end; //if not bAbortOperationFlag
end;

function ShellExecuteEx(sActionName, sFileName, sActiveDir: string): boolean;
var
  aFile: TFile;
  sCmd, sParams, sStartPath: string;
begin
  Result := False;

  // Executing files directly only works for FileSystem.
  aFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
  try
    if gExts.GetExtActionCmd(aFile, sActionName, sCmd, sParams, sStartPath) then
    begin
      Result := ProcessExtCommandFork(sCmd, sParams, sStartPath, aFile);
    end;

    if not Result then
    begin
      mbSetCurrentDir(sActiveDir);
      Result := ShellExecute(sFileName);
    end;

  finally
    FreeAndNil(aFile);
  end;
end;



end.














