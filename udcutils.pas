{
   Double Commander
   -------------------------------------------------------------------------
   Several useful functions
   
   Copyright (C) 2006-2008  Koblov Alexander (Alexx2000@mail.ru)

   contributors:
   
   Radek Cervinka  <radek.cervinka@centrum.cz>
   (cnvFormatFileSize and DivFileName functions)

   Tomas Bzatek <tbzatek@users.sourceforge.net>
   (TrimQuotes, QuoteStr, RemoveQuotation and SplitArgs functions)
   
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
  Classes, SysUtils, Graphics;

const
  QuotationCharacters = [' ', '"', '''', '(', ')', ':', '&'];

type
  TOpenStringArray = array of String;

function GetCmdDirFromEnvVar(sPath : String) : String;
function SetCmdDirAsEnvVar(sPath : String) : String;
function GetTempFolder: String;
{en
   Get last directory name in path
   @returns(Last directory name in path)
}
function GetLastDir(Path : String) : String;
function GetSplitFileName(var sFileName, sPath : String) : String;
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
   Get file name without path and extension
   @param(FileName File name)
   @returns(File name without path and extension)
}
function ExtractOnlyFileName(const FileName: string): string;
{en
   Split command line on command and parameters
   @param(sCmdLine Command line)
   @param(sCmd Command)
   @param(sParams Parameters)
   @returns(The function returns @true if successful, @false otherwise)
}
function SplitCmdLine(sCmdLine : String; var sCmd, sParams : String) : Boolean;
{en
   Convert file size to string representation in floating format (Kb, Mb, Gb)
   @param(iSize File size)
   @returns(File size in string representation)
}
function cnvFormatFileSize(iSize:Int64):String;
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
procedure DivFileName(const sFileName:String; var n,e:String);
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
procedure TrimQuotes(var s: String);
function QuoteStr(const Str: String): String;
{en
   Delete quotation characters [' ', '"', '''', '(', ')', ':', '&'] from string
   @param(Str String)
   @returns(String without quotation characters)
}
function RemoveQuotation(const Str: String): String;
{en
   Split command line on list of arguments
   @param(Args List of arguments)
   @param(CmdLine Command line)
}
procedure SplitArgs(var Args: TOpenStringArray; CmdLine: String);

procedure ParseLineToList(sLine: String; var ssItems: TStrings);

implementation
uses
   uOSUtils, uGlobs, uGlobsPaths, uVFSUtil;


function GetCmdDirFromEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos('%commander_path%', sPath) <> 0 then
    Result := StringReplace(sPath, '%commander_path%', ExcludeTrailingPathDelimiter(gpExePath), [rfIgnoreCase])
  else
    Result := sPath;
end;

function SetCmdDirAsEnvVar(sPath: String): String;
begin
  DoDirSeparators(sPath);
  if Pos(gpExePath, sPath) <> 0 then
    Result := StringReplace(sPath, ExcludeTrailingPathDelimiter(gpExePath), '%commander_path%', [rfIgnoreCase])
  else
    Result := sPath;
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
var
  iPos : Integer;
  sDir : String;
begin
  sDir := '';
  if (Pos(PathDelim, sRelativeFileName)  <> 0) and (Pos(DriveDelim, sRelativeFileName) = 0) then
    begin
      iPos := Pos('..' + PathDelim, sRelativeFileName);
      if iPos <> 0 then
        sDir := sPath;
      while iPos <> 0 do
        begin
          sDir := LowDirLevel(sDir);
          Delete(sRelativeFileName, iPos, 3);
          iPos := Pos('..' + PathDelim, sRelativeFileName);
        end;
      Result := sDir + sRelativeFileName;
    end
  else
    if Pos(DriveDelim, sRelativeFileName) = 0 then
      Result := sPath + sRelativeFileName
    else
      Result := sRelativeFileName;
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

function SplitCmdLine(sCmdLine : String; var sCmd, sParams : String) : Boolean;
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
  sParams:= RemoveQuotation(sParams);
  Result := (sCmd <>'');
end;

function cnvFormatFileSize(iSize:Int64):String;
var
  d:double;
begin
  //DebugLn(IntToStr(iSize));
  if gShortFileSizeFormat then
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
         while Canvas.TextWidth(Result) > MaxLen do
           begin
             Delete(Result, Length(Result), 1);
           end;
         Result := Copy(Result, 1, Length(Result) - 3) + '...';
       end;
End;

procedure DivFileName(const sFileName:String; var n,e:String);
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

function NumCountChars(const Char: char; const S: String): Integer;
var
  I : Integer;
begin
  Result := 0;
  if Length(S) > 0 then
    for I := 1 to Length(S) do
      if S[I] = Char then Inc(Result);
end;

procedure TrimQuotes(var s: String);
begin
  while (Length(s) > 0) and (s[1] in ['"', '''']) do Delete(s, 1, 1);
  while (Length(s) > 0) and (s[Length(s)] in ['"', '''']) do Delete(s, Length(s), 1);
end;

function QuoteStr(const Str: String): String;
var
  I : Integer;
begin
  Result := '';
  if Length(Str) > 0 then
    for I := 1 to Length(Str) do begin
      if Str[I] in QuotationCharacters then Result := Result + ShieldChar;
      Result := Result + Str[I];
    end;
end;

function RemoveQuotation(const Str: String): String;
var
  b : boolean;
  I : integer;
begin
  Result := Str;
  if Length(Result) < 2 then Exit;
  {b := True;
  for I := 2 to Length(Result) do
    if (Result[I] in QuotationCharacters) and (Result[I - 1] <> ShieldChar) then b := False;
  if b then} for I := Length(Result) downto 2 do
    if (Result[I] in QuotationCharacters) and (Result[I - 1] = ShieldChar) then Delete(Result, I - 1, 1);
end;

procedure SplitArgs(var Args: TOpenStringArray; CmdLine: String);
var
  InQuotes : Boolean;
  I, Start : Integer;
  QuoteChar : Char;
  s : string;
begin
  SetLength(Args, 0);
  InQuotes := False;
  CMDLine := Trim(CmdLine);
  if Length(CmdLine) = 0 then Exit;
  Start := 1;
  QuoteChar := #0;
  for I := 1 to Length(CmdLine) do
    case CmdLine[I] of
      ' ': if (not InQuotes) and ((I = 1) or (CMDLine[I - 1] <> ShieldChar)) then begin
             s := Trim(Copy(CmdLine, Start, I - Start));
             TrimQuotes(s);
             Start := I;
             if s = '' then Continue;
             SetLength(Args, Length(Args) + 1);
             Args[Length(Args) - 1] := s;
           end;
      '"', '''': if (I = 1) or (CmdLine[I - 1] <> ShieldChar) then
           if not InQuotes then begin
             InQuotes := True;
             QuoteChar := CmdLine[I];
//             Start := i;
           end else
           if CMDLine[I] = QuoteChar then begin
             InQuotes := False;
             s := Trim(Copy(CmdLine, Start, I + 1 - Start));
             TrimQuotes(s);
             Start := I;
             if s = '' then Continue;
//             if (Pos('"', s) > 1) and (Pos('"', s) < Length(s)) and (NumCountChars('"', s) mod 2 = 1) then s := s + '"';
//             if (Pos('''', s) > 1) and (Pos('''', s) < Length(s)) and (NumCountChars('''', s) mod 2 = 1) then s := s + '''';
             SetLength(Args, Length(Args) + 1);
             Args[Length(Args) - 1] := s;
           end;
    end;
  if (Start <> Length(CmdLine)) or (Start = 1) then begin
    SetLength(Args, Length(Args) + 1);
    Args[Length(Args) - 1] := Trim(Copy(CMDLine, Start, Length(CmdLine) + 1 - Start));
    TrimQuotes(Args[Length(Args) - 1]);
  end;
end;

procedure ParseLineToList(sLine: String; var ssItems: TStrings);
var
  I: Integer;
  xPos: Integer;
begin
  for I:= 0 to ssItems.Count - 1 do
    begin
      xPos:= Pos(';', sLine);
      ssItems[I]:= Copy(sLine, 1, xPos - 1);
      Delete(sLine, 1, xPos);
    end;
end;

end.

