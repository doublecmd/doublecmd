{
   Double Commander
   -------------------------------------------------------------------------
   Useful functions dealing with strings.
   
   Copyright (C) 2006-2020  Alexander Koblov (alexx2000@mail.ru)
   Copyright (C) 2012       Przemyslaw Nagay (cobines@gmail.com)

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

unit DCStrUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes,LazUtf8;

const
  NoQuotesSpecialChars     = [' ', '"', '''', '(', ')', ':', '&', '!', '$', '*', '?', '=', '`', '\', '|', ';', #10];
  DoubleQuotesSpecialChars = ['$', '\', '`', '"', #10];

type
  TPathType = (ptNone, ptRelative, ptAbsolute);

{en
   Checks if StringToCheck contains any of the single characters in
   PossibleCharacters. Only ASCII can be searched.
}
function ContainsOneOf(StringToCheck: String; PossibleCharacters: String): Boolean;
{en
   Convert known directory separators to the current directory separator.
}
function NormalizePathDelimiters(const Path: String): String;
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
function GetDeepestExistingPath(const sPath : String) : String;

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
function GetAbsoluteFileName(const sPath, sRelativeFileName : String) : String;
{en
   Checks if a path to a directory or file is absolute or relative.
   @returns(ptNone if a path is just a directory or file name (MyDir)
            ptRelative if a path is relative                  (MyDir/MySubDir)
            ptAbsolute if a path is absolute)                 (/root/MyDir)
}
function GetPathType(const sPath : String): TPathType;
function ExtractFileDirEx(const FileName: String): String;
function ExtractFilePathEx(const FileName: String): String;
function ExtractFileNameEx(const FileName: String): String;
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
   Remove file extension with the '.' from file name.
}
function RemoveFileExt(const FileName: String): String;
function ReplaceInvalidChars(const FileName: String): String;
function RemoveInvalidCharsFromFileName(const FileName: String): String;
function ContainsWildcards(const Path: String): Boolean;
{en
   Expands an absolute file path by removing all relative references.
   Processes '/../' and '/./'.

   Example:  /home/user/files/../somedirectory/./file.txt
           = /home/user/somedirectory/file.txt

   @param(Path path to expand.)
}
function ExpandAbsolutePath(const Path: String): String;
function HasPathInvalidCharacters(Path: String): Boolean;
{en
  Checks if a file or directory belongs in the specified path.
  Only strings are compared, no file-system checks are done.

  @param(sBasePath
         Absolute path where the path to check should be in.)
  @param(sPathToCheck
         Absolute path to file or directory to check.)
  @param(AllowSubDirs
         If @true, allows the sPathToCheck to point to a file or directory in some subdirectory of sBasePath.
         If @false, only allows the sPathToCheck to point directly to a file or directory in sBasePath.)
  @param(AllowSame
         If @true, returns @true if sBasePath = sPathToCheck.
         If @false, returns @false if sBasePath = sPathToCheck.)
  @return(@true if sPathToCheck points to a directory or file in sBasePath.
          @false otherwise.)

  Examples:
    IsInPath('/home', '/home/somedir/somefile', True, False) = True
    IsInPath('/home', '/home/somedir/somefile', False, False) = False
    IsInPath('/home', '/home/somedir/', False, False) = True
    IsInPath('/home', '/home', False, False) = False
    IsInPath('/home', '/home', False, True) = True
}
function IsInPath(sBasePath : String; sPathToCheck : String;
                  AllowSubDirs : Boolean; AllowSame : Boolean) : Boolean;

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
   Adds a path delimiter at the beginning of the string, if it not exists.
}
function IncludeFrontPathDelimiter(s: String): String;
{en
   Removes a path delimiter at the beginning of the string, if it exists.
}
function ExcludeFrontPathDelimiter(s: String): String;
{en
   Removes a path delimiter at the ending of the string, if it exists.
   Doesn't remove path delimiter if it is the only character in the path (root dir),
   so it is safer to use than ExcludeTrailingPathDelimiter, especially on Unix.
}
function ExcludeBackPathDelimiter(const Path: String): String;

{en
   Return position of character in string begun from start position
   @param(C character)
   @param(S String)
   @param(StartPos Start position)
   @returns(Position of character in string)
}
function CharPos(C: Char; const S: string; StartPos: Integer = 1): Integer;



{en
  Return position of any of tag-characters in string T in string S begun from start position
  @param(T set of characters)
  @param(S String)
  @param(StartPos Start position)
  @param(SearchBackward set @True if need search backwards)
  @returns(Position of character in string)

}
function TagPos(T: string; const S: string; StartPos: Integer;SearchBackward: boolean=False): Integer;


{en

}
function scopy(IndexBegin,IndexEnd:integer;str:string):string;


{en
   Split file name on name and extension
   @param(sFileName File name)
   @param(n Name)
   @param(e Extension)
}
procedure DivFileName(const sFileName:String; out n,e:String);
{en
   Split ';' separated path list to array
   @param(Path Path list to split)
   @returns(Path array)
}
function SplitPath(const Path: String): TDynamicStringArray;
{en
   Split file mask on name mask and extension mask
   @param(DestMask File mask)
   @param(DestNameMask Name mask)
   @param(DestExtMask Extension mask)
}
procedure SplitFileMask(const DestMask: String; out DestNameMask: String; out DestExtMask: String);
{en
   Apply name and extension mask to the file name
   @param(aFileName File name)
   @param(NameMask Name mask)
   @param(ExtMask Extension mask)
}
function ApplyRenameMask(aFileName: String; NameMask: String; ExtMask: String): String;
{en
   Get count of character in string
   @param(Char Character)
   @param(S String)
   @returns(Count of character)
}
function NumCountChars(const Char: Char; const S: String): Integer;
{en
   Trim the leading and ending spaces
}
function TrimPath(const Path: String): String;
{en
   Remove last line ending in text
   @param(sText Text)
   @param(TextLineBreakStyle Text line break style)
}
function TrimRightLineEnding(const sText: String; TextLineBreakStyle: TTextLineBreakStyle): String;
function mbCompareText(const s1, s2: String): PtrInt;

function StrNewW(const mbString: String): PWideChar;
procedure StrDisposeW(var pStr : PWideChar);
function StrLCopyW(Dest, Source: PWideChar; MaxLen: SizeInt): PWideChar;
function StrPCopyW(Dest: PWideChar; const Source: WideString): PWideChar;
function StrPLCopyW(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
function RPos(const Substr : UnicodeString; const Source : UnicodeString) : Integer; overload;

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
procedure AddStrWithSep(var SourceString: String; const StringToAdd: String; const Separator: String);
procedure ParseLineToList(sLine: String; ssItems: TStrings);
function ParseLineToFileFilter(sFilterPair: array of string): string;

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

procedure AddString(var anArray: TDynamicStringArray; const sToAdd: String);
{en
   Splits a string into different parts delimited by the specified delimiter character.
}
function SplitString(const S: String; Delimiter: AnsiChar): TDynamicStringArray;
{en
   Checks if the second array is the beginning of first.
   If BothWays is @true then also checks the other way around,
   if the first array is the beginning of second.
   For Array1=[1,2]     Array2=[1,2] returns @true.
   For Array1=[1,2,...] Array2=[1,2] returns @true.
   For Array1=[1,3,...] Array2=[1,2] returns @false.
   If BothWays = True then also
   For Array1=[1] Array2=[1,2] returns @true.
   For Array1=[1] Array2=[2] returns @false.
}
function ArrBegins(const Array1, Array2: array of String; BothWays: Boolean): Boolean;
function ArrayToString(const anArray: TDynamicStringArray; const Separator: Char = ' '): String;
{en
   Compares length and contents of the arrays.
   If lengths differ or individual elements differ returns @false, otherwise @true.
}
function Compare(const Array1, Array2: array of String): Boolean;
{en
   Copies open array to dynamic array.
}
function CopyArray(const anArray: array of String): TDynamicStringArray;
function ContainsOneOf(const ArrayToSearch, StringsToSearch: array of String): Boolean;
function Contains(const ArrayToSearch: array of String; const StringToSearch: String): Boolean;
procedure DeleteString(var anArray: TDynamicStringArray; const Index: Integer);
procedure DeleteString(var anArray: TDynamicStringArray; const sToDelete: String);
function GetArrayFromStrings(Strings: TStrings): TDynamicStringArray;
procedure SetStringsFromArray(Strings: TStrings; const anArray: TDynamicStringArray);
{en
   Replaces old value of Key or adds a new Key=NewValue string to the array.
}
procedure SetValue(var anArray: TDynamicStringArray; Key, NewValue: String);
procedure SetValue(var anArray: TDynamicStringArray; Key: String; NewValue: Boolean);
function ShortcutsToText(const Shortcuts: TDynamicStringArray): String;
function GetDateTimeInStrEZSortable(DateTime:TDateTime):string;
function WrapTextSimple(const S: String; MaxCol: Integer = 100): String;

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

implementation

uses
  DCOSUtils, StrUtils;

function NormalizePathDelimiters(const Path: String): String;
{$IFDEF UNIX}
begin
  Result:= Path;
end;
{$ELSE}
const
  AllowPathDelimiters : set of char = ['\','/'];
var
  I : LongInt;
begin
  Result:= Path;
  // If path is not URI
  if Pos('://', Result) = 0 then
  begin
    for I:= 1 to Length(Path) do
      if Path[I] in AllowPathDelimiters then
        Result[I]:= DirectorySeparator;
  end;
end;
{$ENDIF}

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

function GetDeepestExistingPath(const sPath : String) : String;
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

function GetAbsoluteFileName(const sPath, sRelativeFileName : String) : String;
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
    { Absolute path in Windows }
    if { X:\...  [Disk] ":" is reserved otherwise }
       ( Pos( DriveDelim, sPath ) > 0 ) or
       { \\...   [UNC]
         \...    [Root of current drive] }
       ( sPath[1] = PathDelim ) then
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
    { UNIX absolute paths start with a slash }
    if (sPath[1] = PathDelim) then
{$ENDIF UNIX}
      Result := ptAbsolute
    else if ( Pos( PathDelim, sPath ) > 0 ) then
      Result := ptRelative
    else if (sPath = '..') then
      Result := ptRelative
    else
      Result := ptNone;
  end
  else
    Result := ptNone;
end;

function ExtractFileDirEx(const FileName: String): String;
var
  i : longint;
begin
  I := Length(FileName);
  while (I > 0) and not CharInSet(FileName[I],AllowDirectorySeparators) do
    Dec(I);
  if (I > 1) and CharInSet(FileName[I],AllowDirectorySeparators) and
     not CharInSet(FileName[I - 1],AllowDirectorySeparators) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFilePathEx(const FileName: String): String;
var
  i : longint;
begin
  i := Length(FileName);
  while (i > 0) and not CharInSet(FileName[i],AllowDirectorySeparators) do
    Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function ExtractFileNameEx(const FileName: String): String;
var
  i : longint;
begin
  I := Length(FileName);
  while (I > 0) and not CharInSet(FileName[I],AllowDirectorySeparators) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractOnlyFileName(const FileName: string): string;
var
  SOF : Boolean;
  I, Index : LongInt;
  EndSep : Set of Char;
begin
  Index := MaxInt;
  // Find a dot index
  I := Length(FileName);
  EndSep:= AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
  begin
    SOF:= (I = 1) or (FileName[I - 1] in AllowDirectorySeparators);
    if (not SOF) or FirstDotAtFileNameStartIsExtension then
      Index := I
  end;
  // Find file name index
  EndSep := EndSep - [ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do Dec(I);
  Result := Copy(FileName, I + 1, Index - I - 1);
end;

function ExtractOnlyFileExt(const FileName: string): string;
var
  I : LongInt;
  SOF : Boolean;
  EndSep : Set of Char;
begin
  Result := EmptyStr;
  I := Length(FileName);
  EndSep:= AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
  begin
    SOF:= (I = 1) or (FileName[I - 1] in AllowDirectorySeparators);
    if (not SOF) or FirstDotAtFileNameStartIsExtension then
      Result := Copy(FileName, I + 1, MaxInt)
  end;
end;

function RemoveFileExt(const FileName: String): String;
var
  I : LongInt;
  SOF : Boolean;
  EndSep : Set of Char;
begin
  Result := FileName;
  I := Length(FileName);
  EndSep:= AllowDirectorySeparators + AllowDriveSeparators + [ExtensionSeparator];
  while (I > 0) and not (FileName[I] in EndSep) do Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
  begin
    SOF:= (I = 1) or (FileName[I - 1] in AllowDirectorySeparators);
    if (not SOF) or FirstDotAtFileNameStartIsExtension then
      Result := Copy(FileName, 1, I - 1)
  end;
end;

function ContainsWildcards(const Path: String): Boolean;
begin
  Result := ContainsOneOf(Path, '*?');
end;

function ReplaceInvalidChars(const FileName: String): String;
const
{$IFDEF MSWINDOWS}
  ForbiddenChars : set of char = ['<','>',':','"','/','|','?','*'];
{$ELSE}
  ForbiddenChars : set of char = [#0];
{$ENDIF}
var
  I : LongInt;
begin
  Result:= EmptyStr;
  for I:= 1 to Length(FileName) do
  begin
    if not (FileName[I] in ForbiddenChars) then
      Result:= Result + FileName[I]
    else
      Result+= '%' + HexStr(Ord(FileName[I]), 2);
  end;
end;

{ RemoveInvalidCharsFromFileName }
function RemoveInvalidCharsFromFileName(const FileName: String): String;
const
{$IFDEF MSWINDOWS}
  ForbiddenChars : set of char = ['<','>',':','"','/','\','|','?','*'];
{$ELSE}
  ForbiddenChars : set of char = ['/'];
{$ENDIF}
var
  I : LongInt;
begin
  Result:= '';
  for I:= 1 to Length(FileName) do
    if not (FileName[I] in ForbiddenChars) then
      Result:=Result+FileName[I];
end;

function ExpandAbsolutePath(const Path: String): String;
var
  I, J: Integer;
begin
  Result := Path;

  {First remove all references to '\.\'}
  I := Pos (DirectorySeparator + '.' + DirectorySeparator, Result);
  while I <> 0 do
    begin
      Delete (Result, I, 2);
      I := Pos (DirectorySeparator + '.' + DirectorySeparator, Result);
    end;
  if StrEnds(Result, DirectorySeparator + '.') then
    Delete (Result, Length(Result) - 1, 2);

  {Then remove all references to '\..\'}
  I := Pos (DirectorySeparator + '..', Result);
  while (I <> 0) do
    begin
      J := Pred (I);
      while (J > 0) and (Result [J] <> DirectorySeparator) do
        Dec (J);
      if (J = 0) then
        Delete (Result, I, 3)
      else
        Delete (Result, J, I - J + 3);
      I := Pos (DirectorySeparator + '..', Result);
    end;
end;

function HasPathInvalidCharacters(Path: String): Boolean;
begin
  Result := ContainsOneOf(Path, '*?');
end;

function IsInPath(sBasePath : String; sPathToCheck : String;
                  AllowSubDirs : Boolean; AllowSame : Boolean) : Boolean;
var
  BasePathLength, PathToCheckLength: Integer;
  DelimiterPos: Integer;
begin
  if sBasePath = '' then Exit(False);

  sBasePath := IncludeTrailingPathDelimiter(sBasePath);

  BasePathLength := Length(sBasePath);
  PathToCheckLength := Length(sPathToCheck);

  if PathToCheckLength > BasePathLength then
  begin
    if CompareStr(Copy(sPathToCheck, 1, BasePathLength), sBasePath) = 0 then
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

        // If no delimiter was found or it was found at then end (directories
        // may end with it), then the 'sPathToCheck' is in 'sBasePath'.
        Result := (DelimiterPos = 0) or (DelimiterPos = PathToCheckLength - BasePathLength);
      end;
    end
    else
      Result := False;
  end
  else
    Result := AllowSame and
      (((PathToCheckLength = BasePathLength) and
        (CompareStr(sPathToCheck, sBasePath) = 0)) or
       ((PathToCheckLength = BasePathLength - 1) and
        (CompareStr(Copy(sBasePath, 1, PathToCheckLength), sPathToCheck) = 0)));
end;

function ExtractDirLevel(const sPrefix, sPath: String): String;
var
  PrefixLength: Integer;
begin
  if IsInPath(sPrefix, sPath, True, True) then
  begin
    PrefixLength := Length(sPrefix);
    Result := Copy(sPath, 1 + PrefixLength, Length(sPath) - PrefixLength)
  end
  else
    Result := sPath;
end;

function IncludeFrontPathDelimiter(s: String): String;
begin
  if (Length(s) > 0) and (s[1] = PathDelim) then
    Result:= s
  else
    Result:= PathDelim + s;
end;

function ExcludeFrontPathDelimiter(s: String): String;
begin
  if (Length(s) > 0) and (s[1] = PathDelim) then
    Result := Copy(s, 2, Length(s) - 1)
  else
    Result := s;
end;

function ExcludeBackPathDelimiter(const Path: String): String;
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

function SplitPath(const Path: String): TDynamicStringArray;
const
  cDelta = {$IF DEFINED(UNIX)}1{$ELSE}2{$ENDIF};
  cDelimiter = {$IF DEFINED(UNIX)}'/'{$ELSE}':'{$ENDIF};
var
  L, F: Integer;
  S: Integer = 1;
begin
  L:= Length(Path);
  SetLength(Result, 0);
  for F:= 1 to L - cDelta do
  begin
    if (Path[F] = ';') and (Path[F + cDelta] = cDelimiter) then
    begin
      AddString(Result, Copy(Path, S, F - S));
      S:= F + 1;
    end;
  end;
  if S <= L then
  begin
    AddString(Result, Copy(Path, S, L - S + 1));
  end;
end;

procedure SplitFileMask(const DestMask: String; out DestNameMask: String; out DestExtMask: String);
var
  iPos: LongInt;
begin
  // Special case for mask that contains '*.*' ('*.*.old' for example)
  iPos:= Pos('*.*', DestMask);
  if (iPos = 0) then
    DivFileName(DestMask, DestNameMask, DestExtMask)
  else
    begin
      DestNameMask := Copy(DestMask, 1, iPos);
      DestExtMask := Copy(DestMask, iPos + 1, MaxInt);
    end;
  // Treat empty mask as '*.*'.
  if (DestNameMask = '') and (DestExtMask = '') then
  begin
    DestNameMask := '*';
    DestExtMask  := '.*';
  end;
end;

function ApplyRenameMask(aFileName: String; NameMask: String; ExtMask: String): String;

  function ApplyMask(const TargetString: String; Mask: String): String;
  var
    i:Integer;
  begin
    Result:='';
    for i:=1 to Length(Mask) do
    begin
      if Mask[i]= '?' then
        Result:=Result + TargetString[i]
      else
      if Mask[i]= '*' then
        Result:=Result + Copy(TargetString, i, Length(TargetString) - i + 1)
      else
        Result:=Result + Mask[i];
    end;
  end;

var
  sDstExt: String;
  sDstName: String;
begin
  if ((NameMask = '*') and (ExtMask = '.*')) then
    Result := aFileName
  else
    begin
      DivFileName(aFileName, sDstName, sDstExt);
      sDstName := ApplyMask(sDstName, NameMask);
      sDstExt  := ApplyMask(sDstExt, ExtMask);

      Result := sDstName;
      if sDstExt <> '.' then
        Result := Result + sDstExt;
    end;
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



function TagPos(T: string; const S: string; StartPos: Integer;
  SearchBackward: boolean): Integer;
// in future this function will moved to DCStrUtils
var
  i,cnt:integer;
  ch:char;
begin
  Result:=0;
  i:=StartPos;
  if i=0 then i:=1;

  cnt:=UTF8Length(S);

  if SearchBackward then
  begin
     while (i>0)do
     begin
       ch:=S[UTF8CharToByteIndex(PChar(S), length(S), i)];
       if Pos(ch,T)=0 then
          dec(i)
       else
          break;
     end;
  end
  else
     while (i<=cnt)do
     begin
       ch:=S[UTF8CharToByteIndex(PChar(S), length(S), i)];
       if Pos(ch,T)=0 then
          inc(i)
       else
          break;
     end;


  Result:=i;
end;


function scopy(IndexBegin,IndexEnd:integer;str:string):string;
begin
    if (IndexBegin<=IndexEnd) then
        Result:=copy(str,IndexBegin,(IndexEnd-IndexBegin+1))
    else
        Result:='';
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

function TrimPath(const Path: String): String;
const
  WhiteSpace = [#0..' '{$IFDEF MSWINDOWS},'.'{$ENDIF}];
var
  Index: Integer;
  S: TStringArray;
begin
  S:= TrimRightSet(Path, WhiteSpace).Split([PathDelim]);
  if Length(S) = 0 then
    Result:= EmptyStr
  else begin
    Result:= TrimRightSet(S[0], WhiteSpace);
    for Index := Low(S) + 1 to High(S) do
    begin
      Result+= PathDelim + TrimRightSet(S[Index], WhiteSpace);
    end;
  end;
end;

function TrimRightLineEnding(const sText: String; TextLineBreakStyle: TTextLineBreakStyle): String;
const
  TextLineBreakArray: array[TTextLineBreakStyle] of Integer = (1, 2, 1);
var
  I, L: Integer;
begin
  L:= Length(sText);
  I:= TextLineBreakArray[TextLineBreakStyle];
  Result:= Copy(sText, 1, L - I); // Copy without last line ending
end;

function mbCompareText(const s1, s2: String): PtrInt; inline;
begin
// From 0.9.31 LazUtils can be used but this package does not exist in 0.9.30.
//  Result := LazUTF8.UTF8CompareText(s1, s2);
  Result := WideCompareText(UTF8Decode(s1), UTF8Decode(s2));
end;

function StrNewW(const mbString: String): PWideChar;
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

function RPos(const Substr: UnicodeString; const Source: UnicodeString): Integer;
var
  c : WideChar;
  pc, pc2 : PWideChar;
  MaxLen, llen : Integer;
begin
  Result:= 0;
  llen:= Length(SubStr);
  maxlen:= Length(Source);
  if (llen > 0) and (maxlen > 0) and (llen <= maxlen) then
   begin
     pc:= @Source[maxlen];
     pc2:= @Source[llen - 1];
     c:= Substr[llen];
     while pc >= pc2 do
      begin
        if (c = pc^) and
           (CompareByte(Substr[1], PByte(pc - llen + 1)^, llen * SizeOf(WideChar)) = 0) then
         begin
           Result:= PWideChar(pc - llen + 1) - PWideChar(@source[1]) + 1;
           Exit;
         end;
        Dec(pc);
      end;
   end;
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
  if (Length(SourceString) > 0) and (Length(StringToAdd) > 0) then
    SourceString := SourceString + Separator;
  SourceString := SourceString + StringToAdd;
end;

procedure AddStrWithSep(var SourceString: String; const StringToAdd: String; const Separator: String);
begin
  if (Length(SourceString) > 0) and (Length(StringToAdd) > 0) then
    SourceString := SourceString + Separator;
  SourceString := SourceString + StringToAdd;
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

function ParseLineToFileFilter(sFilterPair: array of string): string;
var
  iPairIndex: integer;
begin
  result:='';
  for iPairIndex := 0 to pred(length(sFilterPair) div 2) do
    result := result + sFilterPair[iPairIndex*2] + '|' + sFilterPair[succ(iPairIndex*2)] + '|';
  if length(result)>0 then
    result := LeftStr(result, pred(length(result)));
end;

function ContainsOneOf(StringToCheck: String; PossibleCharacters: String): Boolean;
var
  i, j: SizeInt;
  pc : PChar;
begin
  pc := Pointer(StringToCheck);
  for i := 1 to Length(StringToCheck) do
  begin
    for j := 1 to Length(PossibleCharacters) do
      if pc^ = PossibleCharacters[j] then
        Exit(True);
    Inc(pc);
  end;
  Result := False;
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
  Result := '';
  while Value >= 8 do
    begin
      iMod:= Value mod 8;
      Value:= Value div 8;
      Result:= IntToStr(iMod) + Result;
    end;
  Result:= IntToStr(Value) + Result;
end;

procedure AddString(var anArray: TDynamicStringArray; const sToAdd: String);
var
  Len: Integer;
begin
  Len := Length(anArray);
  SetLength(anArray, Len + 1);
  anArray[Len] := sToAdd;
end;

function SplitString(const S: String; Delimiter: AnsiChar): TDynamicStringArray;
var
  Start: Integer = 1;
  Len, Finish: Integer;
begin
  Len:= Length(S);
  SetLength(Result, 0);
  for Finish:= 1 to Len do
  begin
    if S[Finish] = Delimiter then
    begin
      AddString(Result, Copy(S, Start, Finish - Start));
      Start:= Finish + 1;
    end;
  end;
  if Start <= Len then
  begin
    AddString(Result, Copy(S, Start, Len - Start + 1));
  end;
end;

function ArrBegins(const Array1, Array2: array of String; BothWays: Boolean): Boolean;
var
  Len1, Len2: Integer;
  i: Integer;
begin
  Len1 := Length(Array1);
  Len2 := Length(Array2);
  if not BothWays and (Len1 < Len2) then
    Result := False
  else
  begin
    if Len1 > Len2 then
      Len1 := Len2;
    for i := 0 to Len1 - 1 do
      if Array1[i] <> Array2[i] then
        Exit(False);
    Result := True;
  end;
end;

function ArrayToString(const anArray: TDynamicStringArray; const Separator: Char): String;
var
  i: Integer;
begin
  Result := '';
  for i := Low(anArray) to High(anArray) do
    AddStrWithSep(Result, anArray[i], Separator);
end;

function Compare(const Array1, Array2: array of String): Boolean;
var
  Len1, Len2: Integer;
  i: Integer;
begin
  Len1 := Length(Array1);
  Len2 := Length(Array2);
  if Len1 <> Len2 then
    Result := False
  else
  begin
    for i := 0 to Len1 - 1 do
      if Array1[i] <> Array2[i] then
        Exit(False);
    Result := True;
  end;
end;

function CopyArray(const anArray: array of String): TDynamicStringArray;
var
  i: Integer;
begin
  SetLength(Result, Length(anArray));
  for i := Low(anArray) to High(anArray) do
    Result[i] := anArray[i];
end;

function ContainsOneOf(const ArrayToSearch, StringsToSearch: array of String): Boolean;
var
  i: Integer;
begin
  for i := Low(StringsToSearch) to High(StringsToSearch) do
    if Contains(ArrayToSearch, StringsToSearch[i]) then
      Exit(True);
  Result := False;
end;

function Contains(const ArrayToSearch: array of String; const StringToSearch: String): Boolean;
var
  i: Integer;
begin
  for i := Low(ArrayToSearch) to High(ArrayToSearch) do
    if ArrayToSearch[i] = StringToSearch then
      Exit(True);
  Result := False;
end;

procedure DeleteString(var anArray: TDynamicStringArray; const Index: Integer);
var
  Len: Integer;
  i: Integer;
begin
  Len := Length(anArray);
  for i := Index + 1 to Len - 1 do
    anArray[i - 1] := anArray[i];
  SetLength(anArray, Len - 1);
end;

procedure DeleteString(var anArray: TDynamicStringArray; const sToDelete: String);
var
  i: Integer;
begin
  for i := Low(anArray) to High(anArray) do
    if anArray[i] = sToDelete then
    begin
      DeleteString(anArray, i);
      Exit;
    end;
end;

function GetArrayFromStrings(Strings: TStrings): TDynamicStringArray;
var
  LinesCount: Integer;
  i: Integer;
begin
  LinesCount := Strings.Count;
  if LinesCount > 0 then
  begin
    if Strings[LinesCount-1] = '' then
      Dec(LinesCount);
    SetLength(Result, LinesCount);
    for i := 0 to LinesCount - 1 do
      Result[i] := Strings[i];
  end;
end;

procedure SetStringsFromArray(Strings: TStrings; const anArray: TDynamicStringArray);
var
  s: String;
begin
  Strings.Clear;
  for s in anArray do
    Strings.Add(s);
end;

procedure SetValue(var anArray: TDynamicStringArray; Key, NewValue: String);
var
  i: Integer;
begin
  Key := Key + '=';
  for i := Low(anArray) to High(anArray) do
    if StrBegins(anArray[i], Key) then
    begin
      anArray[i] := Key + NewValue;
      Exit;
    end;
  AddString(anArray, Key + NewValue);
end;

procedure SetValue(var anArray: TDynamicStringArray; Key: String; NewValue: Boolean);
begin
  if NewValue then
    SetValue(anArray, Key, 'true')
  else
    SetValue(anArray, Key, 'false');
end;

function ShortcutsToText(const Shortcuts: TDynamicStringArray): String;
begin
  Result := ArrayToString(Shortcuts, ' ');
end;

{ GetDateTimeInStrEZSortable: Return the date and time in string format with YYYY-MM-DD@HH-MM-SS
  so it can be integrate in a filename. Also, because of the order of the terms, it make it
  useful when things are sorted BECAUSE it will also sort by date/time at the same time}
function GetDateTimeInStrEZSortable(DateTime:TDateTime):string;
var
  MyYear, MyMonth, MyDay, MyHour, MyMin, MySec, MyMilSec: word;
begin
  DecodeDate(DateTime, MyYear, MyMonth, MyDay);
  DecodeTime(DateTime, MyHour, MyMin, MySec, MyMilSec);
  result:=Format('%d-%2.2d-%2.2d@%2.2d-%2.2d-%2.2d', [MyYear, MyMonth, MyDay, MyHour, MyMin, MySec]);
end;

function WrapTextSimple(const S: String; MaxCol: Integer): String;
var
  Len, Index: Integer;
begin
  Index:= 1;
  Result:= EmptyStr;
  Len:= UTF8Length(S);
  while (Len > 0) do
  begin
    Result:= Result + UTF8Copy(S, Index, MaxCol) + LineEnding;
    Inc(Index, MaxCol); Dec(Len, MaxCol);
  end;
  SetLength(Result, Length(Result) - Length(LineEnding));
end;

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
  Result := EscapeString(Str, DoubleQuotesSpecialChars, '\');
end;

function EscapeNoQuotes(const Str: String): String;
begin
  // When neither single nor double quotes are used several special characters
  // need to be escaped with backslash (single character quote).
  Result := EscapeString(Str, NoQuotesSpecialChars, '\');
end;

end.

