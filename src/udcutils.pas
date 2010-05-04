{
   Double Commander
   -------------------------------------------------------------------------
   Several useful functions
   
   Copyright (C) 2006-2010  Koblov Alexander (Alexx2000@mail.ru)

   contributors:
   
   Radek Cervinka  <radek.cervinka@centrum.cz>
   (cnvFormatFileSize and DivFileName functions)

   Tomas Bzatek <tbzatek@users.sourceforge.net>
   (QuoteStr, RemoveQuotation and SplitArgs functions)
   
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

unit uDCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StdCtrls, uFile
  {$IF DEFINED(UNIX)}
  , uTypes
  {$ENDIF}
  ;

const
{$IF DEFINED(UNIX)}
  NoQuotesSpecialChars     = [' ', '"', '''', '(', ')', ':', '&', '!', '$', '*', '?', '=', '`', '\', #10];
  DoubleQuotesSpecialChars = ['$', '\', '`', '"', #10];
{$ENDIF}
  EnvVarCommanderPath = '%commander_path%';

type
  TPathType = ( ptNone, ptRelative, ptAbsolute );

function GetCmdDirFromEnvVar(sPath : String) : String;
function SetCmdDirAsEnvVar(sPath : String) : String;
{en
   Replaces environment variables of form %<NAME>% with their values.
   Also replaces the internal "%commander_path%".
}
function ReplaceEnvVars(const sText: String): String;
{en
   Expands the file name with environment variables by replacing them by absolute path.
   @param(sFileName File name to expand.)
   @returns(Absolute file name.)
}
function mbExpandFileName(const sFileName: UTF8String): UTF8String;
function GetTempFolder: String;
{en
   Get last directory name in path
   @returns(Last directory name in path)
}
function GetLastDir(Path : String) : String;
{en
   Retrieves the root directory for a path.
   @param(sPath Absolute path to a directory or a file.)
   @returns(Root directory or an empty string if the path is not absolute.)
}
function GetRootDir(sPath : String) : String;
{en
   Retrieves parent directory for a path (removes the last subdirectory in the path).
   @param(sPath Absolute or relative path to a directory or a file.)
   @returns(Parent directory or an empty string
            if the path does not have a parent directory.)
}
function GetParentDir(sPath : String) : String;

{en
   Gets the deepest (longest) path that exist.
}
function GetDeepestExistingPath(const sPath : UTF8String) : UTF8String;

function GetSplitFileName(var sFileName, sPath : String) : String;
function MakeFileName(const sPath, sFileNameDef : String) : String;
{en
   Split path into list of directories
   @param(DirName Path)
   @param(Dirs List of directories names)
   @returns(The function returns the number of directories found, or -1
   if none were found.)
}
function GetDirs (DirName : String; var Dirs : TStringList) : Longint;
{en
   Get absolute file name from relative file name
   @param(sPath Current path)
   @param(sRelativeFileName Relative file name)
   @returns(Absolute file name)
}
function GetAbsoluteFileName(sPath, sRelativeFileName : String) : String;
{en
   Checks if a path to a directory or file is absolute or relative.
   @returns(ptNone if a path is just a directory or file name (MyDir)
            ptRelative if a path is relative                  (MyDir/MySubDir)
            ptAbsolute if a path is absolute)                 (/root/MyDir)
}
function GetPathType(const sPath : String): TPathType;
{en
   Get file name without path and extension
   @param(FileName File name)
   @returns(File name without path and extension)
}
function ExtractOnlyFileName(const FileName: string): string;
{en
   Get file extension without the '.' at the front.
}
function ExtractOnlyFileExt(const FileName: string): string;
{en
   Convert file size to string representation in floating format (Kb, Mb, Gb)
   @param(iSize File size)
   @param(ShortFormat If @true than short format is used,
                      otherwise long format (bytes) is used.)
   @returns(File size in string representation)
}
function cnvFormatFileSize(iSize: Int64; ShortFormat: Boolean): String;
function cnvFormatFileSize(iSize: Int64): String; inline;
{en
   Minimize file path
   @param(PathToMince File path)
   @param(Canvas Output canvas)
   @param(MaxLen Max length of path in pixels)
   @returns(Minimized file path)
}
function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                            MaxLen: Integer): String;
{en
   Expands an absolute file path by removing all relative references.
   Processes '/../' and '/./'.

   Example:  /home/user/files/../somedirectory/./file.txt
           = /home/user/somedirectory/file.txt

   @param(Path path to expand.)
}
function ExpandAbsolutePath(Path: String): String;

{en
  Checks if a file or directory belongs in the specified path.
  Only strings are compared, no file-system checks are done.

  @param(sBasePath
         Absolute path where the path to check should be in.)
  @param(sPathToCheck
         Absolute path to file or directory to check.)
  @param(AllowSubDirs
         If @true, allows the sPathToCheck to point to a file or directory in some subdirectory of sPath.
         If @false, only allows the sPathToCheck to point directly to a file or directory in sPath.
  @return(@true if sPathToCheck points to a directory or file in sBasePath.
          @false otherwise.)

  Examples:
    IsInPath('/home', '/home/somedir/somefile', True) = True
    IsInPath('/home', '/home/somedir/somefile', False) = False
    IsInPath('/home', '/home/somedir/', False) = True
}
function IsInPath(sBasePath : String; sPathToCheck : String; AllowSubDirs: Boolean) : Boolean;

{en
  Checks if a filename matches any filename in the filelist or
  if it could be in any directory of the file list or any of their subdirectories.
  @param(Files
         List of files to which the filename must be matched.)
  @param(FileName
         Path to a file that will be matched. This may be absolute, relative
         or contain no path at all (only filename).)
}
function MatchesFileList(const Files: TFiles; FileName: String): Boolean;

{en
  Checks if a file matches any mask in the masklist.
  @param(aFile
         File that will be matched.)
  @param(MaskList
         List of masks to which the file must be matched.)
}
function MatchesMaskListEx(const aFile: TFile; MaskList: TStringList): Boolean;

{en
  Changes all the files' paths making them relative to 'sNewRootPath'.
  It is done by removing 'sNewRootPath' prefix from the paths and setting
  the general path (Files.Path) to sNewRootPath.
  @param(sNewRootPath
         Path that specifies new 'root' directory for all filenames.)
  @param(Files
         Contains list of files to change.)
}
procedure ChangeFileListRoot(sNewRootPath: String; Files: TFiles);

{en
   Changes a path to be relative to some parent directory.

   @param(sPrefix
          Absolute path that is a parent of sPath.)
   @param(sPath
          Path to change. Must be a subpath of sPrefix, otherwise no change is made.)

   Examples:
     ExtractDirLevel('/home', '/home/somedir/somefile') = '/somedir/somefile'
}
function ExtractDirLevel(const sPrefix, sPath: String): String;

{en
   Removes a path delimiter at the beginning of the string, if it exists.
}
function ExcludeFrontPathDelimiter(s: String): String;
{en
   Removes a path delimiter at the ending of the string, if it exists.
}
function ExcludeBackPathDelimiter(const Path: UTF8String): UTF8String;

{en
   Return position of character in string begun from start position
   @param(C character)
   @param(S String)
   @param(StartPos Start position)
   @returns(Position of character in string)
}
function CharPos(C: Char; const S: string; StartPos: Integer = 1): Integer;
{en
   Split file name on name and extension
   @param(sFileName File name)
   @param(n Name)
   @param(e Extension)
}
procedure DivFileName(const sFileName:String; out n,e:String);
{en
   Replace executable extension by system specific executable extension
   @param(sFileName File name)
   @returns(Executable name with system specific executable extension)
}
function FixExeExt(const sFileName: UTF8String): UTF8String;
{en
   Get count of character in string
   @param(Char Character)
   @param(S String)
   @returns(Count of character)
}
function NumCountChars(const Char: Char; const S: String): Integer;
{en
   Delete quotes from string
   @param(Str String)
}
function TrimQuotes(const Str: String): String;
function QuoteStr(const Str: String): String;
{$IFDEF UNIX}
function QuoteSingle(const Str: String): String;
{$ENDIF}
function QuoteDouble(const Str: String): String;
{$IFDEF UNIX}
{en
   Escapes characters to be inserted between single quotes (')
   and passed to shell command line.
   The resulting string is not enclosed with '', only escaped.

   For example <cmd1> needs to be escaped with this function:
     sh -c '<cmd1>' "<cmd2>" <cmd3>
}
function EscapeSingleQuotes(const Str: String): String;
{en
   Escapes characters to be inserted between double quotes (")
   and passed to shell command line.
   The resulting string is not enclosed with "", only escaped.

   For example <cmd2> needs to be escaped with this function:
     sh -c '<cmd1>' "<cmd2>" <cmd3>
}
function EscapeDoubleQuotes(const Str: String): String;
{en
   Escapes characters to be passed to shell command line when no quoting is used.

   For example <cmd3> needs to be escaped with this function:
     sh -c '<cmd1>' "<cmd2>" <cmd3>
}
function EscapeNoQuotes(const Str: String): String;
{$ENDIF}
{en
   Delete quotation characters from string
   @param(Str String)
   @returns(String without quotation characters)
}
function RemoveQuotation(const Str: String): String;
{$IF DEFINED(UNIX)}
{en
   Split command line to command and a list of arguments.
   Each argument is unquoted.
   @param(sCmdLine Command line)
   @param(sCommand Command)
   @param(Args List of arguments)
}
procedure SplitCmdLine(sCmdLine: String; out sCommand: String; out Args: TOpenStringArray);
{$ELSEIF DEFINED(MSWINDOWS)}
{en
   Split command line to command and parameters
   @param(sCmdLine Command line)
   @param(sCmd Command)
   @param(sParams Parameters)
}
procedure SplitCmdLine(sCmdLine : String; var sCmd, sParams : String);
{$ENDIF}
{en
   Remove last line ending in text
   @param(sText Text)
   @param(TextLineBreakStyle Text line break style)
}
function TrimRightLineEnding(const sText: UTF8String; TextLineBreakStyle: TTextLineBreakStyle): UTF8String;

procedure ParseLineToList(sLine: String; ssItems: TStrings);
procedure InsertFirstItem(sLine: String; comboBox: TCustomComboBox);

function StrNewW(const mbString: UTF8String): PWideChar;
procedure StrDisposeW(var pStr : PWideChar);
function StrLCopyW(Dest, Source: PWideChar; MaxLen: SizeInt): PWideChar;
function StrPCopyW(Dest: PWideChar; const Source: WideString): PWideChar;
function StrPLCopyW(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;

{en
   Checks if a string begins with another string.
   @returns(@true if StringToCheck begins with StringToMatch.
            StringToCheck may be longer than StringToMatch.)
}
function StrBegins(const StringToCheck, StringToMatch: String): Boolean;
{en
   Checks if a string ends with another string.
   @returns(@true if StringToCheck ends with StringToMatch.
            StringToCheck may be longer than StringToMatch.)
}
function StrEnds(const StringToCheck, StringToMatch: String): Boolean;

{en
   Adds a string to another string. If the source string is not empty adds
   a separator before adding the string.
}
procedure AddStrWithSep(var SourceString: String; const StringToAdd: String; const Separator: Char = ' ');

{en
   Convert a number specified as an octal number to it's decimal value.
   @param(Value Octal number as string)
   @returns(Decimal number)
}
function OctToDec(Value: String): LongInt;
{en
   Convert a number specified as an decimal number to it's octal value.
   @param(Value Decimal number)
   @returns(Octal number as string)
}
function DecToOct(Value: LongInt): String;

function EstimateRemainingTime(StartValue, CurrentValue, EndValue: Int64;
                               StartTime: TDateTime; CurrentTime: TDateTime;
                               out SpeedPerSecond: Int64): TDateTime;

function ModColor(AColor: TColor; APercent: Byte) : TColor;

implementation

uses
   FileUtil, Masks, StrUtils, uOSUtils, uGlobs, uGlobsPaths;

function GetCmdDirFromEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(EnvVarCommanderPath, sPath) <> 0 then
    Result := StringReplace(sPath, EnvVarCommanderPath, ExcludeTrailingPathDelimiter(gpExePath), [rfIgnoreCase])
  else
    Result := sPath;
end;

function SetCmdDirAsEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(gpExePath, sPath) <> 0 then
    Result := StringReplace(sPath, ExcludeTrailingPathDelimiter(gpExePath), EnvVarCommanderPath, [rfIgnoreCase])
  else
    Result := sPath;
end;

function ReplaceEnvVars(const sText: String): String;
var
  I, X: Integer;
  EnvVarList: TStringList;
begin
  if sText = EmptyStr then Exit(EmptyStr);
  Result:= sText;
  X:= GetEnvironmentVariableCount;
  if X = 0 then Exit;
  EnvVarList:= TStringList.Create;
  for I:= 1 to X do
    begin
      EnvVarList.Add(mbGetEnvironmentString(I));
      Result:= StringReplace(Result, '%'+EnvVarList.Names[I-1]+'%', EnvVarList.ValueFromIndex[I-1], [rfReplaceAll, rfIgnoreCase]);
    end;
  FreeAndNil(EnvVarList);

  StringReplace(Result, EnvVarCommanderPath, ExcludeTrailingPathDelimiter(gpExePath), [rfReplaceAll, rfIgnoreCase]);
end;

function mbExpandFileName(const sFileName: UTF8String): UTF8String;
begin
  Result := ReplaceEnvVars(sFileName);
  if Pos(PathDelim, Result) <> 0 then
    Result:= ExpandFileName(Result);
end;

function GetTempFolder: String;
begin
  Result:= GetTempDir + '_dc';
  if not mbDirectoryExists(Result) then
    mbCreateDir(Result);
  Result:= Result + PathDelim;
end;

function GetLastDir(Path : String) : String;
begin
  Result:= ExtractFileName(ExcludeTrailingPathDelimiter(Path));
  if Result = '' then
    Result:= ExtractFileDrive(Path);
  if Result = '' then
    Result:= PathDelim;
end;

function GetRootDir(sPath : String) : String;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := ExtractFileDrive(sPath);
  if Result <> '' then
    Result := Result + PathDelim;
{$ELSEIF DEFINED(UNIX)}
  Result := PathDelim;  // Hardcoded
{$ELSE}
  Result := '';
{$ENDIF}
end;

function GetParentDir(sPath : String) : String;
var
  i : Integer;
begin
  Result := '';
  sPath := ExcludeTrailingPathDelimiter(sPath);
  // Start from one character before last.
  for i := length(sPath) - 1 downto 1 do
    if sPath[i] = DirectorySeparator then
    begin
      Result := Copy(sPath, 1, i);
      Break;
    end;
end;

function GetDeepestExistingPath(const sPath : UTF8String) : UTF8String;
begin
  Result := sPath;
  while Result <> EmptyStr do
  begin
    if not mbDirectoryExists(Result) then
      Result := GetParentDir(Result)
    else
      Break;
  end;
end;

function GetSplitFileName(var sFileName, sPath : String) : String;
begin
  if Pos(PathDelim, sFileName) <> 0 then
    begin
      Result := sFileName;
      sPath := ExtractFilePath(sFileName);
      sFileName := ExtractFileName(sFileName);
    end
  else
    Result := sPath + sFileName;
end;

function MakeFileName(const sPath, sFileNameDef : String) : String;
begin
  Result:= ExtractFileName(ExcludeTrailingBackslash(sPath));
  if Result = EmptyStr then
    Result:= sFileNameDef;
end;

function GetDirs (DirName : String; var Dirs : TStringList) : Longint;

var
  I : Longint;
  len : Integer;
  sDir : String;
begin
  I:= 1;
  Result:= -1;
  len := Length(DirName);
  while I <= len do
    begin
    if DirName[I]=PathDelim then
      begin
      Inc(Result);
      sDir := Copy(DirName, 1, len - (len - I + 1));
      if dirs.IndexOf(sDir) < 0 then
        dirs.Add(sDir);
      end;
    Inc(I);
    end;
  if Result > -1 then inc(Result);
end;

function GetAbsoluteFileName(sPath, sRelativeFileName : String) : String;
begin
  case GetPathType(sRelativeFileName) of
    ptNone:
      Result := sPath + sRelativeFileName;

    ptRelative:
      Result := ExpandAbsolutePath(sPath + sRelativeFileName);

    ptAbsolute:
      Result := sRelativeFileName;
  end;
end;

function GetPathType(const sPath : String): TPathType;
begin
  if sPath <> EmptyStr then
  begin
{$IFDEF MSWINDOWS}
    {check for drive/unc info}
    if ( Pos( '\\', sPath ) > 0 ) or ( Pos( DriveDelim, sPath ) > 0 ) then
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
    { UNIX absolute paths start with a slash }
    if (sPath[1] = PathDelim) then
{$ENDIF UNIX}
      Result := ptAbsolute
    else if ( Pos( PathDelim, sPath ) > 0 ) then
      Result := ptRelative
    else
      Result := ptNone;
  end
  else
    Result := ptNone;
end;

function ExtractOnlyFileName(const FileName: string): string;
var
 iDotIndex,
 I: longint;
begin
  (* Find a dot index *)
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['.', '/', '\', ':']) do Dec(I);
  if (I > 0) and (FileName[I] = '.') then
     iDotIndex := I
  else
     iDotIndex := MaxInt;
  (* Find file name index *)
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['/', '\', ':']) do Dec(I);
  Result := Copy(FileName, I + 1, iDotIndex - I - 1);
end;

function ExtractOnlyFileExt(const FileName: string): string;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
    Result := Copy(FileName, I + 1, MaxInt)
  else
    Result := '';
end;

function cnvFormatFileSize(iSize: Int64; ShortFormat: Boolean): String;
var
  d: Double;
begin
  if ShortFormat then
  begin
    if iSize div (1024*1024*1024) > 0 then
    begin
      Result:=FloatToStrF((iSize*16 div (1024*1024*1024))/16, ffFixed, 15, 1)+' G'
    end
    else
    if iSize div (1024*1024) >0 then
    begin
      Result:=FloatToStrF((iSize*10 div (1024*1024))/10, ffFixed, 15, 1)+' M'
    end
    else
    if iSize div 1024 >0 then
    begin
      Result:=FloatToStrF((iSize*10 div 1024)/10, ffFixed, 15, 1)+' K'
    end
    else
      Result:=IntToStr(iSize);
  end
  else
  begin
    d:=iSize;
    Result:=Format('%8.0n',[d]);
  end;
end;

function cnvFormatFileSize(iSize: Int64): String;
begin
  Result := cnvFormatFileSize(iSize, gShortFileSizeFormat);
end;

{
  This function based on code from http://www.delphirus.com.ru
}
   
{=========================================================}
function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                                      MaxLen: Integer): String;
{=========================================================}
// "C:\Program Files\Delphi\DDropTargetDemo\main.pas"
// "C:\Program Files\..\main.pas"
Var
  sl: TStringList;
  sHelp, sFile,
  sFirst: String;
  iPos: Integer;

Begin
  if MaxLen <= 0 then Exit;

  sHelp := PathToMince;
  iPos := Pos(PathDelim, sHelp);
  If iPos = 0 Then
  Begin
    Result := PathToMince;
  End
  Else
  Begin
    sl := TStringList.Create;
    // Decode string
    While iPos <> 0 Do
    Begin
      sl.Add(Copy(sHelp, 1, (iPos - 1)));
      sHelp := Copy(sHelp, (iPos + 1), Length(sHelp));
      iPos := Pos(PathDelim, sHelp);
    End;
    If sHelp <> '' Then
    Begin
      sl.Add(sHelp);
    End;
    // Encode string
    sFirst := sl[0];
    sFile := sl[sl.Count - 1];
    sl.Delete(sl.Count - 1);
    Result := '';
    MaxLen := MaxLen - Canvas.TextWidth('XXX');
    if (sl.Count <> 0) and (Canvas.TextWidth(Result + sl[0] + PathDelim + sFile) < MaxLen) then
      begin
        While (sl.Count <> 0) and (Canvas.TextWidth(Result + sl[0] + PathDelim + sFile) < MaxLen) Do
          Begin
            Result := Result + sl[0] + PathDelim;
            sl.Delete(0);
          End;
        If sl.Count = 0 Then
          Begin
            Result := Result + sFile;
          End
        Else
          Begin
            Result := Result + '..' + PathDelim + sFile;
          End;
      end
    else
      If sl.Count = 0 Then
          Begin
            Result := sFirst + PathDelim;
          End
        Else
          Begin
            Result := sFirst + PathDelim + '..' + PathDelim + sFile;
          End;
    sl.Free;
  End;
  //DebugLn('PathX ' , Result);
  if Canvas.TextWidth(Result) > MaxLen + Canvas.TextWidth('XXX') then
       begin
         while (Length(Result) > 0) and (Canvas.TextWidth(Result) > MaxLen) do
           begin
             Delete(Result, Length(Result), 1);
           end;
         Result := Copy(Result, 1, Length(Result) - 3) + '...';
       end;
End;

function ExpandAbsolutePath(Path: String): String;
var
  I, J: Integer;
begin
  {First remove all references to '\.\'}
  I := Pos (DirectorySeparator + '.' + DirectorySeparator, Path);
  while I <> 0 do
      begin
          Delete (Path, I, 2);
          I := Pos (DirectorySeparator + '.' + DirectorySeparator, Path);
      end;

  {Then remove all references to '\..\'}
  I := Pos (DirectorySeparator + '..', Path);
  while (I <> 0) do
      begin
          J := Pred (I);
          while (J > 0) and (Path [J] <> DirectorySeparator) do
              Dec (J);
          if (J = 0) then
              Delete (Path, I, 3)
          else
              Delete (Path, J, I - J + 3);
          I := Pos (DirectorySeparator + '..', Path);
      end;

  Result := Path;
end;

function IsInPath(sBasePath : String; sPathToCheck : String; AllowSubDirs: Boolean) : Boolean;
var
  BasePathLength, PathToCheckLength: Integer;
  DelimiterPos: Integer;
begin
  Result := False;

  if sBasePath = '' then Exit;

  sBasePath := IncludeTrailingPathDelimiter(sBasePath);

  BasePathLength := Length(sBasePath);
  PathToCheckLength := Length(sPathToCheck);

  if (PathToCheckLength >= BasePathLength) and
     (CompareStr(Copy(sPathToCheck, 1, BasePathLength), sBasePath) = 0) then
  begin
    if AllowSubDirs then
      Result := True
    else
    begin
      // Additionally check if the remaining path is a relative path.

      // Look for a path delimiter in the middle of the filepath.
      sPathToCheck := Copy(sPathToCheck, 1 + BasePathLength,
                           PathToCheckLength - BasePathLength);

      DelimiterPos := Pos(DirectorySeparator, sPathToCheck);

      // If no delimiter was found or it was found at then end (for example,
      // directories may end with it), then the 'sPathToCheck' is in 'sBasePath'.
      if (DelimiterPos = 0) or (DelimiterPos = PathToCheckLength - BasePathLength) then
        Result := True;
    end;
  end;
end;

function MatchesFileList(const Files: TFiles; FileName: String): Boolean;
var
  i: Integer;
  aFile: TFile;
begin
  Result := False;
  for i := 0 to Files.Count - 1 do
  begin
    aFile := Files[i];

    if aFile.IsDirectory then
    begin
      // Check if 'FileName' is in this directory or any of its subdirectories.
      if IsInPath(aFile.FullPath, FileName, True) then
        Result := True;
    end
    else
    begin
      // Item in the list is a file, only compare names.
      if aFile.FullPath = FileName then
        Result := True;
    end;
  end;
end;

function MatchesMaskListEx(const aFile: TFile; MaskList: TStringList): Boolean;
var
  I: Integer;
  sMask,
  sFileName: UTF8String;
begin
  Result:= False;
  for I:= 0 to MaskList.Count - 1 do
    begin
      sMask:= MaskList[I];
      case GetPathType(sMask) of
        ptAbsolute:
          sFileName:= aFile.FullPath;
        else
          sFileName:= aFile.Name;
      end;
      // When a mask is ended with a PathDelim, it will match only directories
      if (Length(sMask) > 1) and (sMask[Length(sMask)] = PathDelim) then
        begin
          if aFile.IsDirectory then
            sMask:= ExcludeTrailingPathDelimiter(sMask)
          else
            Continue;
        end;
      if MatchesMaskList(sFileName, sMask) then
        Exit(True);
    end;
end;

procedure ChangeFileListRoot(sNewRootPath: String; Files: TFiles);
var
  i: Integer;
  aFile: TFile;
begin
  if IsInPath(sNewRootPath, Files.Path, True) then
  begin
    // Current path is a subpath of new root path.

    for i := 0 to Files.Count - 1 do
    begin
      aFile := Files[i];
      aFile.Path := ExtractDirLevel(sNewRootPath, aFile.Path);
    end;

    Files.Path := ExtractDirLevel(sNewRootPath, Files.Path);
  end
  else
  begin
    // Current path has a different base than new root path.

    if sNewRootPath <> EmptyStr then
      sNewRootPath := IncludeTrailingPathDelimiter(sNewRootPath);

    for i := 0 to Files.Count - 1 do
    begin
      aFile := Files[i];
      aFile.Path := sNewRootPath + ExtractDirLevel(Files.Path, aFile.Path);
    end;

    Files.Path := sNewRootPath;
  end;
end;

function ExtractDirLevel(const sPrefix, sPath: String): String;
var
  PrefixLength: Integer;
begin
  if IsInPath(sPrefix, sPath, True) then
  begin
    PrefixLength := Length(sPrefix);
    Result := Copy(sPath, 1 + PrefixLength, Length(sPath) - PrefixLength)
  end
  else
    Result := sPath;
end;

function ExcludeFrontPathDelimiter(s: String): String;
begin
  if (Length(s) > 0) and (s[1] = PathDelim) then
    Result := Copy(s, 2, Length(s) - 1)
  else
    Result := s;
end;

function ExcludeBackPathDelimiter(const Path: UTF8String): UTF8String;
var
  L: Integer;
begin
  L:= Length(Path);
  if (L > 1) and (Path[L] in AllowDirectorySeparators) then
    Result:= Copy(Path, 1, L - 1)
  else
    Result:= Path;
end;

procedure DivFileName(const sFileName:String; out n,e:String);
var
  i:Integer;
begin
  for i:= length(sFileName) downto 1 do
    if sFileName[i]='.' then
    begin
//      if i>1 then // hidden files??
      e:=Copy(sFileName,i,Length(sFileName)-i+1);
      n:=Copy(sFileName,1,i-1);
      Exit;
    end;
  e:='';
  n:=sFileName;
end;


function CharPos(C: Char; const S: string; StartPos: Integer = 1): Integer;
var
 sNewStr : String;
begin
if StartPos <> 1 then
  begin
    sNewStr := Copy(S, StartPos, Length(S) - StartPos + 1);
    Result := Pos(C, sNewStr);
    if Result <> 0 then
      Result := Result + StartPos - 1;
  end
else
  Result := Pos(C, S);
end;

function FixExeExt(const sFileName: UTF8String): UTF8String;
var
  ExeExt: UTF8String;
begin
  Result:= sFileName;
  ExeExt:= GetExeExt;
  if not SameText(ExeExt, ExtractFileExt(sFileName)) then
    Result:= ChangeFileExt(sFileName, ExeExt);
end;

function NumCountChars(const Char: char; const S: String): Integer;
var
  I : Integer;
begin
  Result := 0;
  if Length(S) > 0 then
    for I := 1 to Length(S) do
      if S[I] = Char then Inc(Result);
end;

function TrimQuotes(const Str: String): String;
begin
  Result:= TrimSet(Str, ['"', '''']);
end;

function QuoteStr(const Str: String): String;
{$IF DEFINED(UNIX)}
begin
  // Default method is to escape every special char with backslash.
  Result := EscapeNoQuotes(Str);
end;
{$ELSE}
begin
  // On Windows only double quotes can be used for quoting.
  // The double quotes on Windows can be nested, e.g.,
  // "cmd /C "type "Some long file name""" or
  // "cmd /C ""some long file.exe" "long param1" "long param2""
  Result := QuoteDouble(Str);
end;
{$ENDIF}

{$IF DEFINED(UNIX)}
function QuoteSingle(const Str: String): String;
begin
  Result := '''' + EscapeSingleQuotes(Str) + '''';
end;
{$ENDIF}

function QuoteDouble(const Str: String): String;
begin
{$IF DEFINED(UNIX)}
  Result := '"' + EscapeDoubleQuotes(Str) + '"';
{$ELSEIF DEFINED(MSWINDOWS)}
  // Nothing needs to be escaped on Windows, because only double quote (") itself
  // would need to be escaped but there's no standard mechanism for escaping it.
  // It seems every application handles it on their own and CMD doesn't support it at all.
  // Also double quote is a forbidden character on FAT, NTFS.
  Result := '"' + Str + '"';
{$ENDIF}
end;

{$IF DEFINED(UNIX)}
function EscapeString(const Str: String; const EscapeChars: TCharSet; const EscapeWith: String): String;
var
  StartPos: Integer = 1;
  CurPos: Integer = 1;
begin
  Result := '';
  while CurPos <= Length(Str) do
  begin
    if Str[CurPos] in EscapeChars then
    begin
      Result := Result + Copy(Str, StartPos, CurPos - StartPos) + EscapeWith;
      // The character being quoted will be copied later.
      StartPos := CurPos;
    end;
    Inc(CurPos);
  end;
  Result := Result + Copy(Str, StartPos, CurPos - StartPos);
end;

function EscapeSingleQuotes(const Str: String): String;
begin
  // Single quotes are strong quotes - no special characters are recognized
  // inside those quotes, so only ' needs to be escaped.
  Result := EscapeString(Str, [''''], '''\''');
end;

function EscapeDoubleQuotes(const Str: String): String;
begin
  // Double quotes are weak quotes and a few special characters are allowed
  // which need to be escaped.
  Result := EscapeString(Str, DoubleQuotesSpecialChars, ShieldChar);
end;

function EscapeNoQuotes(const Str: String): String;
begin
  // When neither single nor double quotes are used several special characters
  // need to be escaped with backslash (single character quote).
  Result := EscapeString(Str, NoQuotesSpecialChars, ShieldChar);
end;

// Helper for RemoveQuotation and SplitCmdLine.
procedure RemoveQuotationOrSplitCmdLine(sCmdLine: String; out sCommand: String; out Args: TOpenStringArray; bSplitArgs: Boolean);
var
  I : Integer;
  QuoteChar : Char;
  CurrentArg: String = '';
  DoubleQuotesEscape: Boolean = False;

  procedure AddArgument;
  begin
    if bSplitArgs then
    begin
      if sCommand = '' then
        sCommand := CurrentArg
      else
      begin
        SetLength(Args, Length(Args) + 1);
        Args[Length(Args) - 1] := CurrentArg;
      end;
      CurrentArg := '';
    end;
  end;

begin
  sCommand := '';
  SetLength(Args, 0);
  QuoteChar := #0;
  for I := 1 to Length(sCmdLine) do
    case QuoteChar of
      '\':
        begin
          if sCmdLine[I] <> #10 then
          begin
            if not (sCmdLine[I] in NoQuotesSpecialChars) then
              CurrentArg := CurrentArg + '\';
            CurrentArg := CurrentArg + sCmdLine[I];
          end;
          QuoteChar := #0;
        end;
      '''':
        begin
          if sCmdLine[I] = '''' then
            QuoteChar := #0
          else
            CurrentArg := CurrentArg + sCmdLine[I];
        end;
      '"':
        begin
          if DoubleQuotesEscape then
          begin
            if not (sCmdLine[I] in DoubleQuotesSpecialChars) then
              CurrentArg := CurrentArg + '\';
            CurrentArg := CurrentArg + sCmdLine[I];
            DoubleQuotesEscape := False;
          end
          else
          begin
            case sCmdLine[I] of
              '\':
                DoubleQuotesEscape := True;
              '"':
                QuoteChar := #0;
              else
                CurrentArg := CurrentArg + sCmdLine[I];
            end;
          end;
        end;
      else
        begin
          case sCmdLine[I] of
            '\', '''', '"':
              QuoteChar := sCmdLine[I];
            ' ', #9:
              if CurrentArg <> '' then
                AddArgument;
            #10:
              AddArgument;
            else
              CurrentArg := CurrentArg + sCmdLine[I];
          end;
        end;
    end;
  if QuoteChar <> #0 then
    raise Exception.Create('Invalid quoting');
  if CurrentArg <> '' then
    AddArgument;
  if (not bSplitArgs) then
    sCommand := CurrentArg;
end;
{$ENDIF}

function RemoveQuotation(const Str: String): String;
{$IF DEFINED(MSWINDOWS)}
var
  TrimmedStr: String;
begin
  if Length(Str) = 0 then
    Result := EmptyStr
  else
  begin
    TrimmedStr := Trim(Str);
    if (TrimmedStr[1] = '"') and (TrimmedStr[Length(TrimmedStr)] = '"') then
      Result := Copy(TrimmedStr, 2, Length(TrimmedStr) - 2)
    else
      Result := Str;
  end;
end;
{$ELSEIF DEFINED(UNIX)}
var
  Args: TOpenStringArray;
begin
  RemoveQuotationOrSplitCmdLine(Str, Result, Args, False);
end;
{$ENDIF}

{$IF DEFINED(UNIX)}
procedure SplitCmdLine(sCmdLine: String; out sCommand: String; out Args: TOpenStringArray);
begin
  RemoveQuotationOrSplitCmdLine(sCmdLine, sCommand, Args, True);
end;
{$ELSEIF DEFINED(MSWINDOWS)}
procedure SplitCmdLine(sCmdLine : String; var sCmd, sParams : String);
var
  iPos : Integer;
begin
  if Pos('"', sCmdLine) = 1 then
    begin
      iPos := CharPos('"', sCmdLine, 2);
      sCmd := Copy(sCmdLine, 2, iPos - 2);
      sParams := Copy(sCmdLine, iPos + 2, Length(sCmdLine) - iPos + 1)
    end
  else
    begin
      iPos := Pos(#32, sCmdLine);
      if iPos <> 0 then
        begin
      	  sCmd := Copy(sCmdLine, 1, iPos - 1);
      	  sParams := Copy(sCmdLine, iPos + 1, Length(sCmdLine) - iPos + 1)
        end
      else
        begin
          sCmd := sCmdLine;
          sParams := '';
        end;
    end;
end;
{$ENDIF}

function TrimRightLineEnding(const sText: UTF8String;
                                    TextLineBreakStyle: TTextLineBreakStyle): UTF8String;
const
  TextLineBreakArray: array[TTextLineBreakStyle] of Integer = (1, 2, 1);
var
  I, L: Integer;
begin
  L:= Length(sText);
  I:= TextLineBreakArray[TextLineBreakStyle];
  Result:= Copy(sText, 1, L - I); // Copy without last line ending
end;

procedure ParseLineToList(sLine: String; ssItems: TStrings);
var
  xPos: Integer;
begin
  ssItems.Clear;
  while sLine <> '' do
    begin
      xPos:= Pos(';', sLine);
      if xPos > 0 then
        begin
          ssItems.Add(Copy(sLine, 1, xPos - 1));
          Delete(sLine, 1, xPos);
        end
      else
        begin
          ssItems.Add(sLine);
          Exit;
        end;
    end;
end;

procedure InsertFirstItem(sLine: String; comboBox: TCustomComboBox);
var
  I: Integer = 0;
begin
  if sLine = EmptyStr then Exit;
  with comboBox.Items do
  begin
    // Use case sensitive search
    while (I < Count) and (CompareStr(Strings[I], sLine) <> 0) do Inc(I);

    if (I < 0) or (I >= Count) then
      comboBox.Items.Insert(0, sLine)
    else
      begin
        comboBox.Items.Move(I, 0);
        // Reset selected item (and combobox text), because Move has destroyed it.
        comboBox.ItemIndex := 0;
      end;
  end;
end;

function StrNewW(const mbString: UTF8String): PWideChar;
var
  wsString: WideString;
  iLength: PtrInt;
begin
  Result:= nil;
  wsString:= UTF8Decode(mbString);
  iLength:= (Length(wsString) * SizeOf(WideChar)) + 1;
  Result:= GetMem(iLength);
  if Result <> nil then
    Move(PWideChar(wsString)^, Result^, iLength);
end;

procedure StrDisposeW(var pStr : PWideChar);
begin
  FreeMem(pStr);
  pStr := nil;
end;

function StrLCopyW(Dest, Source: PWideChar; MaxLen: SizeInt): PWideChar;
var
  I: SizeInt;
begin
  Result := Dest;
  for I:= 0 to MaxLen - 1 do
  begin
    if Source^ = #0 then Break;
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
  end;
  Dest^ := #0;
end;

function StrPCopyW(Dest: PWideChar; const Source: WideString): PWideChar;
begin
  Result := StrLCopyW(Dest, PWideChar(Source), Length(Source));
end;

function StrPLCopyW(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
begin
  Result := StrLCopyW(Dest, PWideChar(Source), MaxLen);
end;

function StrBegins(const StringToCheck, StringToMatch: String): Boolean;
begin
  Result := (Length(StringToCheck) >= Length(StringToMatch)) and
            (CompareChar(StringToCheck[1], StringToMatch[1], Length(StringToMatch)) = 0);
end;

function StrEnds(const StringToCheck, StringToMatch: String): Boolean;
begin
  Result := (Length(StringToCheck) >= Length(StringToMatch)) and
            (CompareChar(StringToCheck[1 + Length(StringToCheck) - Length(StringToMatch)],
                         StringToMatch[1], Length(StringToMatch)) = 0);
end;

procedure AddStrWithSep(var SourceString: String; const StringToAdd: String; const Separator: Char);
begin
  if Length(SourceString) > 0 then
    SourceString := SourceString + Separator;
  SourceString := SourceString + StringToAdd;
end;

function OctToDec(Value: String): LongInt;
var
  I: Integer;
begin
  Result:= 0;
  for I:= 1 to Length(Value) do
    Result:= Result * 8 + StrToInt(Copy(Value, I, 1));
end;

function DecToOct(Value: LongInt): String;
var
  iMod: Integer;
begin
  while Value >= 8 do
    begin
      iMod:= Value mod 8;
      Value:= Value div 8;
      Result:= IntToStr(iMod) + Result;
    end;
  Result:= IntToStr(Value) + Result;
end;

function EstimateRemainingTime(StartValue, CurrentValue, EndValue: Int64;
                               StartTime: TDateTime; CurrentTime: TDateTime;
                               out SpeedPerSecond: Int64): TDateTime;
var
  Speed: Double;
begin
  SpeedPerSecond := 0;
  Result := 0;

  if (CurrentValue > StartValue) and (CurrentTime > StartTime) then
    begin
      Speed := Double(CurrentValue - StartValue) / (CurrentTime - StartTime);
      Result := Double(EndValue - CurrentValue) / Speed;
      SpeedPerSecond := Trunc(Speed) div SecsPerDay;
    end;
end;

function ModColor(AColor: TColor; APercent: Byte) : TColor;
var
  R, G, B : Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  R := R * APercent div 100;
  G := G * APercent div 100;
  B := B * APercent div 100;
  Result := RGBToColor(R, G, B);
end;

end.

