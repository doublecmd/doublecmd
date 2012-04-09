{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains some functions for open files in associated applications.

    Copyright (C) 2006-2011  Koblov Alexander (Alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uShellExecute;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFile, uFileView;

type
  TPrepareParameterOption = (ppoNormalizePathDelims, ppoReplaceTilde);
  TPrepareParameterOptions = set of TPrepareParameterOption;

function PrepareParameter(sParam: String;
                          leftPanel: TFileView;
                          rightPanel: TFileView;
                          activePanel: TFileView;
                          options: TPrepareParameterOptions = []): String; overload;
function PrepareParameter(sParam: String;
                          aFile: TFile;
                          options: TPrepareParameterOptions = []): String; overload;
{en
   Replace variable parameters that depend on files in panels.
}
function ReplaceVarParams(sSourceStr: String;
                          leftPanel: TFileView;
                          rightPanel: TFileView;
                          activePanel: TFileView): String; overload;
{en
   Replace variable parameters that depend on the file in active dir.
}
function ReplaceVarParams(sSourceStr: String; aFile: TFile): String; overload;
function ProcessExtCommand(sCmd:String; ActiveDir: String): Boolean;
function ShellExecuteEx(sCmd, sFileName, sActiveDir: String): Boolean;

implementation

uses
  Process, UTF8Process, uDCUtils, uShowForm, uGlobs, uOSUtils,
  uFileSystemFileSource, DCOSUtils, DCStrUtils;

function PrepareParameter(sParam: String;
                          leftPanel: TFileView;
                          rightPanel: TFileView;
                          activePanel: TFileView;
                          options: TPrepareParameterOptions = []): String;
begin
  Result := sParam;
  if ppoNormalizePathDelims in Options then
    Result := NormalizePathDelimiters(Result);
  if ppoReplaceTilde in Options then
    Result := ReplaceTilde(Result);
  Result := ReplaceEnvVars(Result);
  Result := ReplaceVarParams(Result, leftPanel, rightPanel, activePanel);
  Result := Trim(Result);
end;

function PrepareParameter(sParam: String; aFile: TFile; options: TPrepareParameterOptions = []): String;
begin
  Result := sParam;
  if ppoNormalizePathDelims in Options then
    Result := NormalizePathDelimiters(Result);
  if ppoReplaceTilde in Options then
    Result := ReplaceTilde(Result);
  Result := ReplaceEnvVars(Result);
  Result := ReplaceVarParams(Result, aFile);
  Result := Trim(Result);
end;

(*
  Functions (without parameters they give output for all selected files):
  %f - only filename
  %d - only path, without trailing delimiter
  %p - path+filename
  %D - current path in active or chosen panel

  Choosing panel (if not given, active panel is used):
    %X[l|r|s|t] - where X is function (l - left, r - right, s - source, t - target)

  Choosing selected files (only for %f, %d, %p):
    %X[<nr>] - where X is function
               <nr> is 1..n, where n is number of selected files.
               If there are no selected files, currently active file is nr 1.
               If <nr> is invalid or there is no selected file by that number
               the result for the whole function will be empty string.

  Adding prefix, postfix before or after the result string:
    %X[{<prefix>}][{<postfix>}]
      If applied to multiple files, each name is prefixed/postfixed.

  Above parameters can be combined together.
  Order of params:
  - %function
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
function ReplaceVarParams(sSourceStr: String;
                          leftPanel: TFileView;
                          rightPanel: TFileView;
                          activePanel: TFileView): String;
type
  TFunctType = (ftNone, ftName, ftDir, ftPath, ftSingleDir);
  TStatePos = (spNone, spPercent, spFunction, spPrefix, spPostfix,
               spGotPrefix, spSide, spIndex, spComplete);

  Tstate = record
    pos: TStatePos;
    functStartIndex: Integer;
    funct: TFunctType;
    files: TFiles;
    dir: String;
    sFileIndex: String;
    prefix, postfix: String; // a string to add before/after each output
                             // (for functions giving output of multiple strings)
  end;

var
  index: Integer;
  leftFiles: TFiles = nil;
  rightFiles: TFiles = nil;
  activeFiles: TFiles;
  inactiveFiles: TFiles;
  activeDir: String;
  inactiveDir: String;
  state: Tstate;
  sOutput: String = '';
  parseStartIndex: Integer;

  function BuildName(aFile: TFile): String;
  begin
    case state.funct of
      ftName:
        Result := aFile.Name;
      ftDir:
        Result := ExcludeTrailingPathDelimiter(aFile.Path);
      ftPath:
        Result := aFile.FullPath;
      ftSingleDir:
        Result := ExcludeTrailingPathDelimiter(state.dir);
      else
        Exit('');
    end;
    Result := state.prefix + QuoteStr(Result) + state.postfix;
  end;

  function BuildAllNames: String;
  var
    i: Integer;
  begin
    Result := '';
    if Assigned(state.files) then
    begin
      for i := 0 to state.files.Count - 1 do
      begin
        if i > 0 then
          Result := Result + ' ';
        Result := Result + BuildName(state.files[i]);
      end;
    end;
  end;

  procedure ResetState(var aState: TState);
  begin
    with aState do
    begin
      pos := spNone;
      files := activeFiles;
      dir := activeDir;
      sFileIndex := '';
      funct := ftNone;
      functStartIndex := 0;
      prefix := '';
      postfix := '';
    end;
  end;

  procedure AddParsedText(limit: Integer);
  begin
    // Copy [parseStartIndex .. limit - 1].
    if limit > parseStartIndex then
      sOutput := sOutput + Copy(sSourceStr, parseStartIndex, limit - parseStartIndex);
    parseStartIndex := index;
  end;

  procedure DoFunction;
  var
    fileIndex: Integer = -1;
  begin
    AddParsedText(state.functStartIndex);

    if state.sFileIndex <> '' then
    try
      fileIndex := StrToInt(state.sFileIndex);
      fileIndex := fileIndex - 1; // Files are counted from 0, but user enters 1..n.
    except
      on EConvertError do
        fileIndex := -1;
    end;

    if fileIndex <> -1 then
    begin
      if Assigned(state.files) and
         (fileIndex >= 0) and (fileIndex < state.files.Count) then
        sOutput := sOutput + BuildName(state.files[fileIndex]);
    end
    else
    begin
      if state.funct in [ftName, ftPath, ftDir] then
        sOutput := sOutput + BuildAllNames
      else if state.funct in [ftSingleDir] then // only single current dir
        sOutput := sOutput + BuildName(nil);
    end;

    ResetState(state);
  end;

  procedure ProcessNumber;
  begin
    if state.funct = ftSingleDir then
      // Numbers not allowed for %D
      state.pos := spComplete
    else
    begin
      state.sFileIndex := state.sFileIndex + sSourceStr[index];
      state.pos := spIndex;
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
  try
    leftFiles := leftPanel.CloneSelectedFiles;
    rightFiles := rightPanel.CloneSelectedFiles;

    if activePanel = leftPanel then
    begin
      activeFiles := leftFiles;
      activeDir := leftPanel.CurrentPath;
      inactiveFiles := rightFiles;
      inactiveDir := rightPanel.CurrentPath;
    end
    else
    begin
      activeFiles := rightFiles;
      activeDir := rightPanel.CurrentPath;
      inactiveFiles := leftFiles;
      inactiveDir := leftPanel.CurrentPath;
    end;

    index := 1;
    parseStartIndex := index;

    ResetState(state);

    while index <= Length(sSourceStr) do
    begin
      case state.pos of
        spNone:
          if sSourceStr[index] = '%' then
          begin
            state.pos := spPercent;
            state.functStartIndex := index;
          end;

        spPercent:
          case sSourceStr[index] of
            'f':
              begin
                state.funct := ftName;
                state.pos := spFunction;
              end;
            'd':
              begin
                state.funct := ftDir;
                state.pos := spFunction;
              end;
            'D':
              begin
                state.funct := ftSingleDir;
                state.pos := spFunction;
              end;
            'p':
              begin
                state.funct := ftPath;
                state.pos := spFunction;
              end;
            else
              ResetState(state);
          end;

        spFunction:
          case sSourceStr[index] of
            'l':
              begin
                state.files := leftFiles;
                state.dir := leftpanel.CurrentPath;
                state.pos := spSide;
              end;

            'r':
              begin
                state.files := rightFiles;
                state.dir := rightPanel.CurrentPath;
                state.pos := spSide;
              end;

            's':
              begin
                state.files := activeFiles;
                state.dir := activeDir;
                state.pos := spSide;
              end;

            't':
              begin
                state.files := inactiveFiles;
                state.dir := inactiveDir;
                state.pos := spSide;
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
      end;

      if state.pos <> spComplete then
        Inc(index) // check next character
      else
        // Process function and then check current character again after resetting state.
        DoFunction;
    end;

    // Finish current parse.
    if state.pos in [spFunction, spSide, spIndex, spGotPrefix] then
      DoFunction
    else
      AddParsedText(index);

    Result := sOutput;

  finally
    if Assigned(leftFiles) then
      FreeAndNil(leftFiles);
    if Assigned(rightFiles) then
      FreeAndNil(rightFiles);
  end;
end;

function ReplaceVarParams(sSourceStr: String; aFile: TFile): String;
begin
  Result := StringReplace(sSourceStr,'%f',QuoteStr(aFile.Name),[rfReplaceAll]);
  Result := StringReplace(Result    ,'%d',QuoteStr(aFile.Path),[rfReplaceAll]);
  Result := StringReplace(Result    ,'%p',QuoteStr(aFile.FullPath),[rfReplaceAll]);
end;

function ProcessExtCommand(sCmd:String; ActiveDir: String): Boolean;
var
  bTerm: Boolean;
  sTmpFile, sCmdLine: String;
  iStart,
  iCount: Integer;
  Process: TProcessUTF8;  
begin
  Result:= False;
  bTerm:= False;
  (*
    Check for <? ?> command.
    This command is used to put output of some console program to a file so
    that the file can then be viewed. The command is between '<?' and '?>'.
    The whole <?...?> expression is replaced with a path to the temporary file
    containing output of the command.
    For example:
    {!VIEWER} <?rpm -qivlp --scripts %p?>
    Show in Viewer information about RPM package
  *)
  if Pos('<?', sCmd) <> 0 then
    begin
      iStart:= Pos('<?', sCmd) + 2;
      iCount:= Pos('?>', sCmd) - iStart;
      sTmpFile := GetTempName(GetTempFolder) + '.tmp';
      sCmdLine := Copy(sCmd, iStart, iCount) + ' > ' + QuoteStr(sTmpFile);
      Process:= TProcessUTF8.Create(nil);
      try
        Process.CommandLine:= FormatShell(sCmdLine);
        Process.Options:= [poNoConsole, poWaitOnExit];
        Process.Execute;
      finally
        Process.Free;
      end;
      sCmd:= Copy(sCmd, 1, iStart-3) + sTmpFile + Copy(sCmd, iStart + iCount + 2, MaxInt);
    end;  
  if Pos('{!SHELL}', sCmd) > 0 then
  begin
    sCmd:= Trim(StringReplace(sCmd,'{!SHELL}','',[rfReplaceAll]));
    bTerm:= True;
  end;
  if Pos('{!EDITOR}',sCmd) > 0 then
  begin
    sCmd:= Trim(StringReplace(sCmd,'{!EDITOR}','',[rfReplaceAll]));
    uShowForm.ShowEditorByGlob(RemoveQuotation(sCmd));
    Result:= True;
    Exit;
  end;
  if Pos('{!VIEWER}',sCmd) > 0 then
  begin
    sCmd:= Trim(StringReplace(sCmd,'{!VIEWER}','',[rfReplaceAll]));
    uShowForm.ShowViewerByGlob(RemoveQuotation(sCmd));
    Result:= True;
    Exit;
  end;
  mbSetCurrentDir(ActiveDir);
  Result:= ExecCmdFork(sCmd, bTerm, gRunInTerm);
end;

function ShellExecuteEx(sCmd, sFileName, sActiveDir: String): Boolean;
var
  aFile: TFile;
  sCommand: String;
begin
  Result:= False;

  // Executing files directly only works for FileSystem.

  aFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
  try
    sCommand:= gExts.GetExtActionCmd(aFile, sCmd);
    if sCommand <> '' then
      begin
        sCommand := PrepareParameter(sCommand, aFile);
        Result:= ProcessExtCommand(sCommand, sActiveDir);
      end;

    if not Result then
      begin
        mbSetCurrentDir(sActiveDir);
        Result:= ShellExecute(sFileName);
      end;

  finally
    FreeAndNil(aFile);
  end;
end;

end.
