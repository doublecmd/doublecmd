{
   Double Commander
   -------------------------------------------------------------------------
   Several useful functions
   
   Copyright (C) 2006-2024 Alexander Koblov (alexx2000@mail.ru)

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
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uDCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ColorBox,
  {$IF DEFINED(UNIX)}
  DCBasicTypes, uSysFolders,
  {$ENDIF}
  uFile, uTypes;

const
  TextLineBreakValue: array[TTextLineBreakStyle] of String = (#10, #13#10, #13);

{$IFDEF MSWINDOWS}
  VARDELIMITER='%';
  VARDELIMITER_END='%';
{$ENDIF}

{$IFDEF UNIX }
  VARDELIMITER='$';
  VARDELIMITER_END='';
{$ENDIF}

  EnvVarCommanderPath = '%COMMANDER_PATH%'; // Using '%' for backward compatibility
  EnvVarConfigPath    = '%DC_CONFIG_PATH%'; // Using '%' for backward compatibility
  EnvVarTodaysDate    = VARDELIMITER + 'DC_TODAYSDATE' + VARDELIMITER_END;

type
  TUsageOfSizeConversion = (uoscFile, uoscHeader, uoscFooter, uoscOperation);

function GetCmdDirFromEnvVar(const sPath : String) : String;
function SetCmdDirAsEnvVar(const sPath : String) : String;
{en
   Replaces environment variables of form %<NAME>% with their values.
   Also replaces the internal "%COMMANDER_PATH%".
}
function ReplaceEnvVars(const sText: String): String;
{en
   Replaces home directory at the beginning of the string with tilde ~.
}
function ReplaceHome(const Path: String): String;
{en
   Replaces tilde ~ at the beginning of the string with home directory.
}
function ReplaceTilde(const Path: String): String;
{en
   Expands the file name with environment variables by replacing them by absolute path.
   @param(sFileName File name to expand.)
   @returns(Absolute file name.)
}
function mbExpandFileName(const sFileName: String): String;
{en
  Convert Int64 to string with Thousand separators. We can't use FloatToStrF with ffNumber because of integer rounding to thousands
  @param(AValue Integer value)
  @returns(String represenation)
}
function IntToStrTS(const APositiveValue: Int64): String;
{en
   Convert file size to string representation in floating format (Kb, Mb, Gb)
   @param(iSize File size)
   @param(ShortFormat If @true than short format is used,
                      otherwise long format (bytes) is used.)
   @param(Number Number of digits after decimal)
   @returns(File size in string representation)
}
function cnvFormatFileSize(const iSize: Int64; FSF: TFileSizeFormat; const Number: Integer): String;
function cnvFormatFileSize(const iSize: Int64; const UsageOfSizeConversion: TUsageOfSizeConversion): String;
function cnvFormatFileSize(const iSize: Int64): String; inline;
{en
   Minimize file path replacing the folder name
     before the last PathDelim with '..'
     (if path ending with PathDelim it replaces the last folder name!!!)
   @param(PathToMince File path)
   @param(Canvas Output canvas)
   @param(MaxWidth Max width of the path in pixels)
   @returns(Minimized file path)
}
function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                          MaxWidth: Integer): String;
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
  Checks if a file or directory belongs in the specified path list.
  Only strings are compared, no file-system checks are done.

  @param(sBasePathList
         List of absolute paths where the path to check should be in.)
  @param(sPathToCheck
         Absolute path to file or directory to check.)
  @return(@true if sPathToCheck points to a directory or file in sBasePathList.
          @false otherwise.)
}
function IsInPathList(sBasePathList : String; sPathToCheck : String;
                      ASeparator: AnsiChar = ';') : Boolean;
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
   Replace executable extension by system specific executable extension
   @param(sFileName File name)
   @returns(Executable name with system specific executable extension)
}
function FixExeExt(const sFileName: String): String;
{en
   Delete quotes from string
   @param(Str String)
}
function TrimQuotes(const Str: String): String;
function QuoteStr(const Str: String): String;
function QuoteFilenameIfNecessary(const Str: String): String;
function ConcatenateStrWithSpace(const Str: String; const Addition: String):string;
{$IFDEF UNIX}
function QuoteSingle(const Str: String): String;
{$ENDIF}
function QuoteDouble(const Str: String): String;
{$IFDEF UNIX}
{en
   Split command line parameters into argument array
}
procedure SplitCommandArgs(const Params: String; out Args: TDynamicStringArray);
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
procedure SplitCmdLine(sCmdLine: String; out sCommand: String; out Args: TDynamicStringArray);
{$ELSEIF DEFINED(MSWINDOWS)}
{en
   Split command line to command and parameters
   @param(sCmdLine Command line)
   @param(sCmd Command)
   @param(sParams Parameters)
}
procedure SplitCmdLine(sCmdLine : String; var sCmd, sParams : String);
{$ENDIF}
function CompareStrings(const s1, s2: String; Natural: Boolean; Special: Boolean; CaseSensitivity: TCaseSensitivity): PtrInt;

procedure InsertFirstItem(sLine: String; comboBox: TCustomComboBox; AValue: UIntPtr = 0);
{en
   Compares two strings taking into account the numbers or special chararcters
}
function StrChunkCmp(const str1, str2: String; Natural: Boolean; Special: Boolean; CaseSensitivity: TCaseSensitivity): PtrInt;

function EstimateRemainingTime(StartValue, CurrentValue, EndValue: Int64;
                               StartTime: TDateTime; CurrentTime: TDateTime;
                               out SpeedPerSecond: Int64): TDateTime;
{en
   Check color lightness
   @returns(@true when color is light, @false otherwise)
}
function ColorIsLight(AColor: TColor): Boolean;
{en
   Modify color levels
}
function ModColor(AColor: TColor; APercent: Byte) : TColor;
{en
   Makes a color some darker
   @param(AColor Source color)
   @param(APercent The percentage of brightness decrease)
   @returns(New some darker color)
}
function DarkColor(AColor: TColor; APercent: Byte): TColor;
{en
   Makes a color some lighter
   @param(AColor Source color)
   @param(APercent The percentage of brightness increase)
   @returns(New some lighter color)
}
function LightColor(AColor: TColor; APercent: Byte): TColor;
function ContrastColor(Color: TColor; APercent: Byte): TColor;
procedure SetColorInColorBox(const lcbColorBox: TColorBox; const lColor: TColor);
procedure UpdateColor(Control: TControl; Checked: Boolean);
procedure EnableControl(Control:  TControl; Enabled: Boolean);
procedure SetComboWidthToLargestElement(AComboBox: TCustomComboBox; iExtraWidthToAdd: integer = 0);

procedure SplitCmdLineToCmdParams(sCmdLine : String; var sCmd, sParams : String);

function GuessLineBreakStyle(const S: String): TTextLineBreakStyle;
function GetTextRange(Strings: TStrings; Start, Finish: Integer): String;
function DCGetNewGUID: TGUID;
procedure DCPlaceCursorNearControlIfNecessary(AControl: TControl);

implementation

uses
  uLng, LCLProc, LCLType, uMasks, FileUtil, StrUtils, uOSUtils, uGlobs, uGlobsPaths,
  DCStrUtils, DCOSUtils, DCConvertEncoding, LazUTF8
{$IF DEFINED(MSWINDOWS)}
  , Windows
{$ENDIF}
  ;

var
  dtLastDateSubstitutionCheck:TDateTime=0;

function GetCmdDirFromEnvVar(const sPath: String): String;
begin
  Result := NormalizePathDelimiters(sPath);
  Result := StringReplace(Result, EnvVarCommanderPath, ExcludeTrailingPathDelimiter(gpExePath), [rfIgnoreCase]);
  Result := StringReplace(Result, EnvVarConfigPath, ExcludeTrailingPathDelimiter(gpCfgDir), [rfIgnoreCase]);
end;

function SetCmdDirAsEnvVar(const sPath: String): String;
begin
  Result := NormalizePathDelimiters(sPath);
  Result := StringReplace(Result, ExcludeTrailingPathDelimiter(gpExePath), EnvVarCommanderPath, []);
  Result := StringReplace(Result, ExcludeTrailingPathDelimiter(gpCfgDir), EnvVarConfigPath, []);
end;

function ReplaceEnvVars(const sText: String): String;
var
  I: Integer;
  MyYear, MyMonth, MyDay:word;
begin
  Result:= sText;

  //1st, if we have an empty string, get out of here, quick
  if sText = EmptyStr then Exit;

  //2th, let's check the "easy" substitution, there one related with Double Commander
  if pos('%', sText) > 0 then
  begin
    Result := StringReplace(Result, EnvVarCommanderPath, ExcludeTrailingPathDelimiter(gpExePath), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, EnvVarConfigPath, ExcludeTrailingPathDelimiter(gpCfgDir), [rfReplaceAll, rfIgnoreCase]);
  end;

  //3nd, if we don't have the variable indication (% in windows for example), get out of here here, quick
  if pos(VARDELIMITER, sText) = 0 then Exit;

  //4rd, let's check if date changed since last time we updated our dc_todaysdate variable
  if dtLastDateSubstitutionCheck<>Trunc(now) then
    begin
      //Date changed! Let's find where variable is and update it.
      //Don't worry for time consumed: this is done only once per day!
      I:=0;
      while (I<gSpecialDirList.Count) do
      begin
        if gSpecialDirList.SpecialDir[I].VariableName=ENVVARTODAYSDATE then
        begin
          //Variable name found! Let's assign the new date to path value
          DecodeDate(now,MyYear,MyMonth,MyDay);
          gSpecialDirList.SpecialDir[I].PathValue:=Format('%d-%2.2d-%2.2d',[MyYear,MyMonth,MyDay]);
          I:=gSpecialDirList.Count; //To make sure we will end the search loop
        end;
        inc(I);
      end;
      dtLastDateSubstitutionCheck:=Trunc(now); //So we won't re-check this while we're under the same day
    end;

  //5th, let's roll through the possible variable. We did that with a "while" instead of a constant "for-loop" to get out quickly as soon as we solved the variables
  I:=0;
  while (I<gSpecialDirList.Count) AND (pos(VARDELIMITER,sText)<>0) do
  begin
    if pos(gSpecialDirList.SpecialDir[I].VariableName,Result)<>0 then Result := StringReplace(Result, gSpecialDirList.SpecialDir[I].VariableName, ExcludeTrailingPathDelimiter(gSpecialDirList.SpecialDir[I].PathValue), [rfReplaceAll, rfIgnoreCase]);
    inc(I);
  end;

  //6th, if we don't have variable indication anymore, (% in windows for example), get out of here here, quick
  if pos(VARDELIMITER, sText) = 0 then Exit;

  //7th, if still we have variable there, let's scan through the environment variable.
  //     We got them in the "gSpecialDirList" but just in case some others were added on-the-fly
  //     between the moment the application started and the moment we might needed them
  Result:= mbExpandEnvironmentStrings(Result);
end;

function ReplaceHome(const Path: String): String;
{$IFDEF UNIX}
var
  Len: Integer;
  AHome: String;
{$ENDIF}
begin
{$IFDEF UNIX}
  AHome:= GetHomeDir;
  Len:= Length(AHome);
  if StrBegins(Path, AHome) and ((Length(Path) = Len) or (Path[Len + 1] = PathDelim)) then
    Result := '~' + Copy(Path, Len + 1, MaxInt)
  else
{$ENDIF}
    Result := Path;
end;

function ReplaceTilde(const Path: String): String;
begin
{$IFDEF UNIX}
  if StrBegins(Path, '~') and ((Length(Path) = 1) or (Path[2] = PathDelim)) then
    Result := GetHomeDir + Copy(Path, 2, MaxInt)
  else
{$ENDIF}
    Result := Path;
end;

function mbExpandFileName(const sFileName: String): String;
const
  PATH_DELIM_POS = {$IFDEF MSWINDOWS}1{$ELSE}0{$ENDIF};
begin
  if (Pos('://', sFileName) > 2) then
    Result:= sFileName
  else begin
    Result:= NormalizePathDelimiters(sFileName);
    Result:= ReplaceEnvVars(Result);

    if Pos(PathDelim, Result) > PATH_DELIM_POS then
    begin
{$IF DEFINED(MSWINDOWS)}
    if (Length(Result) > 1) and (Result[1] in ['A'..'Z', 'a'..'z']) and
       (Result[2] = DriveSeparator) and (GetDriveType(PAnsiChar(ExtractFileDrive(Result) + PathDelim)) = DRIVE_REMOTE) then
    begin
      Result:= ExpandAbsolutePath(Result)
    end
    else
{$ENDIF}
      Result:= ExpandFileName(Result);
    end;

{$IF DEFINED(MSWINDOWS)}
    // Remove double backslash '\\' after calling 'ExpandFileName'
    if (Pos(':\\', Result) = 2) and (Result[1] in ['A'..'Z', 'a'..'z'])  then
      Result:= Result.Remove(2, 1);
{$ENDIF}
  end;
end;

function IntToStrTS(const APositiveValue: Int64): String;
var
  // 25 is length of '9,223,372,036,854,775,807' (max Int64 with thousand separators)
  ShortStringResult: String[25];
  SmallI, BigI: Byte;
begin
  Str(APositiveValue, ShortStringResult);
  if (APositiveValue > 999) and (DefaultFormatSettings.ThousandSeparator <> #0) then
  begin
    SmallI := Length(ShortStringResult);
    BigI := SmallI + (SmallI - 1) div 3;
    SetLength(ShortStringResult, BigI);
    repeat
      ShortStringResult[BigI] := ShortStringResult[SmallI];
      Dec(BigI);
      Dec(SmallI);
      ShortStringResult[BigI] := ShortStringResult[SmallI];
      Dec(BigI);
      Dec(SmallI);
      ShortStringResult[BigI] := ShortStringResult[SmallI];
      Dec(BigI);
      Dec(SmallI);
      ShortStringResult[BigI] := DefaultFormatSettings.ThousandSeparator;
      Dec(BigI);
    until BigI = SmallI;
  end;
  Result := ShortStringResult;
end;

function cnvFormatFileSize(const iSize: int64; FSF: TFileSizeFormat; const Number: integer): string;
const
  DIVISORS: array[LOW(TFileSizeFormat) .. HIGH(TFileSizeFormat)] of uint64 = (1, 1, 1024, (1024*1024), (1024*1024*1024), (1024*1024*1024*1024), 1, 1, 1024, (1024*1024), (1024*1024*1024), (1024*1024*1024*1024));
var
  FloatSize: extended;
begin
  FloatSize := iSize;
  if FSF = fsfPersonalizedFloat then
  begin
    if iSize div (1024 * 1024 * 1024 * 1024) > 0 then FSF := fsfPersonalizedTera
    else if iSize div (1024 * 1024 * 1024) > 0 then FSF := fsfPersonalizedGiga
    else if iSize div (1024 * 1024) > 0 then FSF := fsfPersonalizedMega
    else if iSize div 1024 > 0 then FSF := fsfPersonalizedKilo
    else FSF := fsfPersonalizedByte;
  end
  else if FSF = fsfFloat then
  begin
    if iSize div (1024 * 1024 * 1024 * 1024) > 0 then FSF := fsfTera
    else if iSize div (1024 * 1024 * 1024) > 0 then FSF := fsfGiga
    else if iSize div (1024 * 1024) > 0 then FSF := fsfMega
    else if iSize div 1024 > 0 then FSF := fsfKilo
    else FSF := fsfByte;
  end;

  case FSF of
    fsfByte, fsfPersonalizedByte: Result := Format('%.0n%s', [FloatSize, gSizeDisplayUnits[FSF]]);
    else
      Result := FloatToStrF(FloatSize / DIVISORS[FSF], ffNumber, 15, Number) + gSizeDisplayUnits[FSF];
  end;
end;

function cnvFormatFileSize(const iSize: Int64; const UsageOfSizeConversion: TUsageOfSizeConversion): String;
begin
  case UsageOfSizeConversion of
    uoscOperation: //By legacy, it was simply adding a "B" to single size letter so we will do the samefor legacy mode.
      begin
        Result := cnvFormatFileSize(iSize, gOperationSizeFormat, gOperationSizeDigits);
        case gOperationSizeFormat of
          fsfFloat: if iSize div 1024 > 0 then Result := Result + rsLegacyOperationByteSuffixLetter else Result := Result + ' ' + rsLegacyOperationByteSuffixLetter;
          fsfByte: Result := Result + ' ' + rsLegacyOperationByteSuffixLetter;
          fsfKilo, fsfMega, fsfGiga, fsfTera: Result := Result + rsLegacyOperationByteSuffixLetter;
        end;
      end;
    uoscFile: Result := cnvFormatFileSize(iSize, gFileSizeFormat, gFileSizeDigits);
    uoscHeader: Result := cnvFormatFileSize(iSize, gHeaderSizeFormat, gHeaderDigits);
    uoscFooter: Result := cnvFormatFileSize(iSize, gFooterSizeFormat, gFooterDigits);
  end;
end;

function cnvFormatFileSize(const iSize: Int64): String;
begin
  Result := cnvFormatFileSize(iSize, gFileSizeFormat, gFileSizeDigits);
end;

{
  This function based on code from http://www.delphirus.com.ru
}
   
{=========================================================}
function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                                      MaxWidth: Integer): String;
{=========================================================}
// "C:\Program Files\Delphi\DDropTargetDemo\main.pas"
// "C:\Program Files\..\main.pas"
Var
  sl: TStringList;
  sHelp, sFile,
  sFirst: String;
  iPos: Integer;
  Len: Integer;
Begin
  if MaxWidth <= 0 then Exit;

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
    MaxWidth := MaxWidth - Canvas.TextWidth('XXX');
    if (sl.Count <> 0) and (Canvas.TextWidth(Result + sl[0] + PathDelim + sFile) < MaxWidth) then
      begin
        While (sl.Count <> 0) and (Canvas.TextWidth(Result + sl[0] + PathDelim + sFile) < MaxWidth) Do
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
  if Canvas.TextWidth(Result) > MaxWidth + Canvas.TextWidth('XXX') then
  begin
    Len:= UTF8Length(Result);
    while (Len >= 3) and (Canvas.TextWidth(Result) > MaxWidth) do
    begin
      UTF8Delete(Result, Len, 1);
      Dec(Len);
    end;
    Result := UTF8Copy(Result, 1, Len - 3) + '...';
  end;
End;


function MatchesFileList(const Files: TFiles; FileName: String): Boolean;
var
  i: Integer;
  aFile: TFile;
begin
  for i := 0 to Files.Count - 1 do
  begin
    aFile := Files[i];

    if aFile.IsDirectory then
    begin
      // Check if 'FileName' is in this directory or any of its subdirectories.
      if IsInPath(aFile.FullPath, FileName, True, True) then
        Exit(True);
    end
    else
    begin
      // Item in the list is a file, only compare names.
      if aFile.FullPath = FileName then
        Exit(True);
    end;
  end;
  Result := False;
end;

function MatchesMaskListEx(const aFile: TFile; MaskList: TStringList): Boolean;
var
  I: Integer;
  sMask,
  sFileName: String;
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

function IsInPathList(sBasePathList: String; sPathToCheck: String;
                      ASeparator: AnsiChar = ';'): Boolean;
var
  sBasePath: String;
begin
  sBasePathList := UTF8UpperCase(sBasePathList);
  sPathToCheck := UTF8UpperCase(sPathToCheck);
  repeat
    sBasePath := Copy2SymbDel(sBasePathList, ASeparator);
    if IsInPath(sBasePath, sPathToCheck, True, True) then
      Exit(True);
  until Length(sBasePathList) = 0;
  Result := False
end;

procedure ChangeFileListRoot(sNewRootPath: String; Files: TFiles);
var
  i: Integer;
  aFile: TFile;
begin
  if IsInPath(sNewRootPath, Files.Path, True, True) then
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

function FixExeExt(const sFileName: String): String;
var
  ExeExt: String;
begin
  Result:= sFileName;
  ExeExt:= GetExeExt;
  if not SameText(ExeExt, ExtractFileExt(sFileName)) then
    Result:= ChangeFileExt(sFileName, ExeExt);
end;

function TrimQuotes(const Str: String): String;
begin
  Result:= Str;
  if (Length(Str) > 0) then
  begin
    if (Str[1] in ['"', '''']) then
    begin
      if (Length(Str) = 1) then
        Result:= EmptyStr
      else if (Str[1] = Str[Length(Str)]) then
        Result:= Copy(Str, 2, Length(Str) - 2);
    end;
  end;
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

function QuoteFilenameIfNecessary(const Str: String): String;
{$IF DEFINED(UNIX)}
begin
  // Default method is to escape every special char with backslash.
  Result := EscapeNoQuotes(Str);
end;
{$ELSE}
begin
  if Pos(#32, Str) <> 0 then
    Result := QuoteDouble(Str)
  else
    Result := Str;
end;
{$ENDIF}

function ConcatenateStrWithSpace(const Str: String; const Addition: String):string;
begin
  result:=Str;
  if Addition <> EmptyStr then
    if result = EmptyStr then
      result := Addition
    else
      result := result + #32 + Addition;
end;

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
// Helper for RemoveQuotation and SplitCmdLine.
procedure RemoveQuotationOrSplitCmdLine(sCmdLine: String; out sCommand: String; out Args: TDynamicStringArray; bSplitArgs: Boolean; bNoCmd: Boolean = False);
var
  I : Integer;
  QuoteChar : Char;
  CurrentArg: String = '';
  DoubleQuotesEscape: Boolean = False;

  procedure AddArgument;
  begin
    if bSplitArgs then
    begin
      if (bNoCmd = False) and (sCommand = '') then
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
    raise EInvalidQuoting.Create;
  if CurrentArg <> '' then
    AddArgument;
  if (not bSplitArgs) then
    sCommand := CurrentArg;
end;

procedure SplitCommandArgs(const Params: String; out Args: TDynamicStringArray);
var
  Unused: String;
begin
  RemoveQuotationOrSplitCmdLine(Params, Unused, Args, True, True);
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
  Args: TDynamicStringArray;
begin
  RemoveQuotationOrSplitCmdLine(Str, Result, Args, False);
end;
{$ENDIF}

procedure SplitCmdLineToCmdParams(sCmdLine : String; var sCmd, sParams : String);
var
  iPos : Integer;
begin
  if Pos('"', sCmdLine) = 1 then
    begin
      iPos := CharPos('"', sCmdLine, 2);
      if iPos = 0 then
        raise EInvalidQuoting.Create;
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

{$IF DEFINED(UNIX)}
procedure SplitCmdLine(sCmdLine: String; out sCommand: String; out Args: TDynamicStringArray);
begin
  RemoveQuotationOrSplitCmdLine(sCmdLine, sCommand, Args, True);
end;
{$ELSEIF DEFINED(MSWINDOWS)}
procedure SplitCmdLine(sCmdLine : String; var sCmd, sParams : String);
begin
  SplitCmdLineToCmdParams(sCmdLine,sCmd,sParams);
end;
{$ENDIF}

function CompareStrings(const s1, s2: String; Natural: Boolean; Special: Boolean; CaseSensitivity: TCaseSensitivity): PtrInt; inline;
{$IF DEFINED(MSWINDOWS)}
var
  dwCmpFlags: DWORD;
  U1, U2: UnicodeString;
{$ENDIF}
begin
  if Natural or Special then
  begin
{$IF DEFINED(MSWINDOWS)}
    if (CaseSensitivity <> cstCharValue) and
       ((Win32MajorVersion > 6) or ((Win32MajorVersion = 6) and (Win32MinorVersion >= 1))) then
    begin
      dwCmpFlags := 0;
      U1 := CeUtf8ToUtf16(s1);
      U2 := CeUtf8ToUtf16(s2);
      if CaseSensitivity = cstNotSensitive then begin
        dwCmpFlags := dwCmpFlags or NORM_IGNORECASE;
      end;
      if Natural then begin
        dwCmpFlags := dwCmpFlags or SORT_DIGITSASNUMBERS;
      end;
      if Special then begin
        dwCmpFlags := dwCmpFlags or SORT_STRINGSORT;
      end;
      Result := CompareStringW(LOCALE_USER_DEFAULT, dwCmpFlags, PWideChar(U1), Length(U1), PWideChar(U2), Length(U2));
      if Result <> 0 then
        Result := Result - 2
      else begin
        Result := StrChunkCmp(s1, s2, Natural, Special, CaseSensitivity);
      end;
    end
    else
{$ENDIF}
    Result := StrChunkCmp(s1, s2, Natural, Special, CaseSensitivity)
  end
  else
    begin
      case CaseSensitivity of
        cstNotSensitive:
          Result := UnicodeCompareText(CeUtf8ToUtf16(s1), CeUtf8ToUtf16(s2));
        cstLocale:
          Result := UnicodeCompareStr(CeUtf8ToUtf16(s1), CeUtf8ToUtf16(s2));
        cstCharValue:
          Result := SysUtils.CompareStr(S1, S2);
        else
          raise Exception.Create('Invalid CaseSensitivity parameter');
      end;
    end;
end;

procedure InsertFirstItem(sLine: String; comboBox: TCustomComboBox; AValue: UIntPtr);
var
  I: Integer = 0;
begin
  if sLine = EmptyStr then Exit;
  with comboBox.Items do
  begin
    // Use case sensitive search
    while (I < Count) and (CompareStr(Strings[I], sLine) <> 0) do Inc(I);

    if (I < 0) or (I >= Count) then
      begin
        comboBox.Items.Insert(0, sLine);
        comboBox.ItemIndex := 0;
      end
    else if (I > 0) then
      begin
        comboBox.Items.Move(I, 0);
        // Reset selected item (and combobox text), because Move has destroyed it.
        comboBox.ItemIndex := 0;
      end;
    Objects[0]:= TObject(AValue);
  end;
end;

function UnicodeStrComp(const Str1, Str2 : UnicodeString): PtrInt;
 var
  counter: SizeInt = 0;
  pstr1, pstr2: PWideChar;
 Begin
   pstr1 := PWideChar(Str1);
   pstr2 := PWideChar(Str2);
   While pstr1[counter] = pstr2[counter] do
   Begin
     if (pstr2[counter] = #0) or (pstr1[counter] = #0) then
        break;
     Inc(counter);
   end;
   Result := ord(pstr1[counter]) - ord(pstr2[counter]);
 end;

function StrChunkCmp(const str1, str2: String; Natural: Boolean; Special: Boolean; CaseSensitivity: TCaseSensitivity): PtrInt;

type
  TCategory = (cNone, cNumber, cSpecial, cString);

  TChunk = record
    FullStr: String;
    Str: String;
    Category: TCategory;
    PosStart: Integer;
    PosEnd: Integer;
  end;

var
  Chunk1, Chunk2: TChunk;

  function Categorize(c: Char): TCategory; inline;
  begin
    if Natural and (c in ['0'..'9']) then
      Result := cNumber
    else if Special and (c in [' '..'/', ':'..'@', '['..'`', '{'..'~']) then
      Result := cSpecial
    else
      Result := cString;
  end;

  procedure NextChunkInit(var Chunk: TChunk); inline;
  begin
    Chunk.PosStart := Chunk.PosEnd;
    if Chunk.PosStart > Length(Chunk.FullStr) then
      Chunk.Category := cNone
    else
      Chunk.Category := Categorize(Chunk.FullStr[Chunk.PosStart]);
  end;

  procedure FindChunk(var Chunk: TChunk); inline;
  begin
    Chunk.PosEnd := Chunk.PosStart;
    repeat
      inc(Chunk.PosEnd);
    until (Chunk.PosEnd > Length(Chunk.FullStr)) or
          (Categorize(Chunk.FullStr[Chunk.PosEnd]) <> Chunk.Category);
  end;

  procedure FindSameCategoryChunks; inline;
  begin
    Chunk1.PosEnd := Chunk1.PosStart;
    Chunk2.PosEnd := Chunk2.PosStart;
    repeat
      inc(Chunk1.PosEnd);
      inc(Chunk2.PosEnd);
    until (Chunk1.PosEnd > Length(Chunk1.FullStr)) or
          (Chunk2.PosEnd > Length(Chunk2.FullStr)) or
          (Categorize(Chunk1.FullStr[Chunk1.PosEnd]) <> Chunk1.Category) or
          (Categorize(Chunk2.FullStr[Chunk2.PosEnd]) <> Chunk2.Category);
  end;

  procedure PrepareChunk(var Chunk: TChunk); inline;
  begin
    Chunk.Str := Copy(Chunk.FullStr, Chunk.PosStart, Chunk.PosEnd - Chunk.PosStart);
  end;

  procedure PrepareNumberChunk(var Chunk: TChunk); inline;
  begin
    while (Chunk.PosStart <= Length(Chunk.FullStr)) and
          (Chunk.FullStr[Chunk.PosStart] = '0') do
      inc(Chunk.PosStart);
    PrepareChunk(Chunk);
  end;

begin
  Chunk1.FullStr := str1;
  Chunk2.FullStr := str2;
  Chunk1.PosEnd := 1;
  Chunk2.PosEnd := 1;

  NextChunkInit(Chunk1);
  NextChunkInit(Chunk2);

  if (Chunk1.Category = cSpecial) and (Chunk2.Category <> cSpecial) then
    Exit(-1);
  if (Chunk2.Category = cSpecial) and (Chunk1.Category <> cSpecial) then
    Exit(1);

  if Chunk1.Category = cSpecial then
    FindSameCategoryChunks
  else
  begin
    FindChunk(Chunk1);
    FindChunk(Chunk2);
  end;

  if (Chunk1.Category = cNumber) xor (Chunk2.Category = cNumber) then // one of them is number
    Chunk1.Category := cString; // compare as strings to put numbers in a natural position

  while True do
  begin

    case Chunk1.Category of
      cString:
        begin
          PrepareChunk(Chunk1);
          PrepareChunk(Chunk2);
          case CaseSensitivity of
            cstNotSensitive: Result := UnicodeCompareText(CeUtf8ToUtf16(Chunk1.Str), CeUtf8ToUtf16(Chunk2.Str));
            cstLocale:       Result := UnicodeCompareStr(CeUtf8ToUtf16(Chunk1.Str), CeUtf8ToUtf16(Chunk2.Str));
            cstCharValue:    Result := UnicodeStrComp(CeUtf8ToUtf16(Chunk1.Str), CeUtf8ToUtf16(Chunk2.Str));
            else
              raise Exception.Create('Invalid CaseSensitivity parameter');
          end;
          if Result <> 0 then
            Exit;
        end;
      cNumber:
        begin
          PrepareNumberChunk(Chunk1);
          PrepareNumberChunk(Chunk2);
          Result := Length(Chunk1.Str) - Length(Chunk2.Str);
          if Result <> 0 then
            Exit;
          Result := CompareStr(Chunk1.Str, Chunk2.Str);
          if Result <> 0 then
            Exit;
        end;
      cSpecial:
        begin
          PrepareChunk(Chunk1);
          PrepareChunk(Chunk2);
          Result := CompareStr(Chunk1.Str, Chunk2.Str);
          if Result <> 0 then
            Exit;
        end;
      cNone:
        Exit(UnicodeStrComp(CeUtf8ToUtf16(str1), CeUtf8ToUtf16(str2)));
    end;

    NextChunkInit(Chunk1);
    NextChunkInit(Chunk2);

    if (Chunk1.Category = cNone) and (Chunk2.Category = cNone) then
      Exit;

    if Chunk1.Category <> Chunk2.Category then
      if Chunk1.Category < Chunk2.Category then
        Exit(-1)
      else
        Exit(1);

    if Chunk1.Category = cSpecial then
      FindSameCategoryChunks
    else
    begin
      FindChunk(Chunk1);
      FindChunk(Chunk2);
    end;

  end;
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

function ColorIsLight(AColor: TColor): Boolean;
var
  R, G, B: Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  Result:= ((0.299 * R + 0.587 * G + 0.114 * B) / 255) > 0.5;
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

function DarkColor(AColor: TColor; APercent: Byte): TColor;
var
  R, G, B: Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  R:= R - MulDiv(R, APercent, 100);
  G:= G - MulDiv(G, APercent, 100);
  B:= B - MulDiv(B, APercent, 100);
  Result:= RGBToColor(R, G, B);
end;

function LightColor(AColor: TColor; APercent: Byte): TColor;
var
  R, G, B: Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  R:= R + MulDiv(255 - R, APercent, 100);
  G:= G + MulDiv(255 - G, APercent, 100);
  B:= B + MulDiv(255 - B, APercent, 100);
  Result:= RGBToColor(R, G, B);
end;

function ContrastColor(Color: TColor; APercent: Byte): TColor;
begin
  if ColorIsLight(Color) then
    Result:= DarkColor(Color, APercent)
  else begin
    Result:= LightColor(Color, APercent);
  end;
end;

procedure SetColorInColorBox(const lcbColorBox: TColorBox; const lColor: TColor);
//< select in lcbColorBox lColor if lColor in lcbColorBox else
// add to lcbColorBox lColor and select him
var
  I: LongInt;
begin
  if (lcbColorBox = nil) then Exit; // if lcbColorBox not exist

  with lcbColorBox do
  begin
    // search lColor in colorbox colorlist
    for I:= 0 to Items.Count - 1 do
      if Colors[I] = lColor then // find color
      begin
        // select color
        Selected:= lColor;
        Exit;
      end;//  if for

    //add items to colorbox list
    Items.Objects[Items.Add('$'+HexStr(lColor, 8))]:= TObject(PtrInt(lColor));
    Selected:= lColor;
  end; // with
end;

procedure UpdateColor(Control: TControl; Checked: Boolean);
begin
  if Checked then
    Control.Color:= clDefault
  else
    Control.Color:= $FFFFFFFF8000000F;
end;

procedure EnableControl(Control: TControl; Enabled: Boolean);
begin
  Control.Enabled:= Enabled;
  {$IF DEFINED(LCLWIN32)}
  if Enabled then
    begin
      Control.Color:= clDefault;
      Control.Font.Color:= clDefault;
    end
  else
    begin
      Control.Color:= clBtnFace;
      Control.Font.Color:= clGrayText;
    end;
  {$ENDIF}
end;

{ SetComboWidthToLargestElement }
// Set the width of a TComboBox to fit to the largest element in it.
procedure SetComboWidthToLargestElement(AComboBox: TCustomComboBox; iExtraWidthToAdd: integer = 0);
var
  iElementIndex, iCurrentElementWidth, iLargestWidth: integer;
begin
  iLargestWidth := 0;
  iElementIndex := 0;
  while (iElementIndex < AComboBox.Items.Count) do
  begin
    iCurrentElementWidth := AComboBox.Canvas.TextWidth(AComboBox.Items.Strings[iElementIndex]);
    if iCurrentElementWidth > iLargestWidth then iLargestWidth := iCurrentElementWidth;
    inc(iElementIndex);
  end;
  if iLargestWidth > 0 then AComboBox.Width := (iLargestWidth + iExtraWidthToAdd);
end;

function GuessLineBreakStyle(const S: String): TTextLineBreakStyle;
var
  Start, Finish, Current: PAnsiChar;
begin
  Start:= PAnsiChar(S);
  Finish:= Start + Length(S);
  Current:= Start;
  while Current < Finish do
  begin
    case Current[0] of
      #10, #13:
        begin
          if (Current[0] = #13) then
          begin
            if (Current[1] = #10) then
              Result:= tlbsCRLF
            else
              Result:= tlbsCR;
          end
          else begin
            Result:= tlbsLF;
          end;
          Exit;
        end;
    end;
    Inc(Current);
  end;
  Result:= DefaultTextLineBreakStyle;
end;

function GetTextRange(Strings: TStrings; Start, Finish: Integer): String;
var
  P: PAnsiChar;
  S, NL: String;
  I, L, NLS: LongInt;
begin
  with Strings do
  begin
    L:= 0;
    NL:= TextLineBreakValue[TextLineBreakStyle];
    NLS:= Length(NL);
    for I:= Start to Finish do
      L:= L + Length(Strings[I]) + NLS;
    SetLength(Result, L);
    P:= Pointer(Result);
    for I:= Start to Finish do
    begin
      S:= Strings[I];
      L:= Length(S);
      if L <> 0 then
        System.Move(Pointer(S)^, P^, L);
      P:= P + L;
      for L:= 1 to NLS do
      begin
        P^:= NL[L];
        Inc(P);
      end;
    end;
  end;
end;

{ DCGetNewGUID }
function DCGetNewGUID: TGUID;
var
  iIndex: integer;
begin
  if CreateGuid(Result) <> 0 then
  begin
    Result.Data1 := random($233528DE);
    Result.Data2 := random($FFFF);
    Result.Data3 := random($FFFF);
    for iIndex := 0 to 7 do Result.Data4[iIndex] := random($FF);
  end;
end;

{ DCPlaceCursorNearControlIfNecessary }
//Note: Programmatically move cursor position near a control if it's not around it.
//      This is for those who would use the keyboard accelerator key for making popup at a logical place.
procedure DCPlaceCursorNearControlIfNecessary(AControl: TControl);
var
  ptControlCenter: TPoint;
begin
  ptControlCenter := AControl.ClientToScreen(Classes.Point(AControl.Width div 2, AControl.Height div 2));
  if (abs(Mouse.CursorPos.x - ptControlCenter.x) > (AControl.Width div 2)) or  (abs(Mouse.CursorPos.y - ptControlCenter.y) > (AControl.Height div 2)) then
    Mouse.CursorPos := Classes.Point((ptControlCenter.x + (AControl.width div 2)) - 10, ptControlCenter.y);
end;

end.

