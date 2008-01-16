{
   Double Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : Alexander Koblov (Alexx2000@mail.ru)

   Several useful functions

   contributors:
   
   Radek Cervinka  <radek.cervinka@centrum.cz>

}

unit uDCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function GetCmdDirFromEnvVar(sPath : String) : String;
function SetCmdDirAsEnvVar(sPath : String) : String;
function GetSplitFileName(var sFileName, sPath : String) : String;
function GetDirs (DirName : String; var Dirs : TStringList) : Longint;
function GetAbsoluteFileName(sPath, sRelativeFileName : String) : String;
function ExtractOnlyFileName(const FileName: string): string;
function SplitCmdLine(sCmdLine : String; var sCmd, sParams : String) : Boolean;
function cnvFormatFileSize(iSize:Int64):String;
function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                           MaxLen: Integer): String;
function CharPos(C: Char; const S: string; StartPos: Integer = 1): Integer;
procedure DivFileName(const sFileName:String; var n,e:String);

implementation
uses
   uGlobs, uGlobsPaths, uVFSUtil;


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

{
  DirName is split in a list of directory names,
  Dirs is an TStrings.
  The function returns the number of directories found, or -1
  if none were found.
  DirName must contain only PathDelim as Directory separator chars.
}

function GetDirs (DirName : String; var Dirs : TStringList) : Longint;

Var
  I : Longint;
  len : Integer;
  sDir : String;
begin
  I:= 1;
  Result:= -1;
  len := Length(DirName);
  While I <= len do
    begin
    If DirName[I]=PathDelim then
      begin
      Inc(Result);
      sDir := Copy(DirName, 1, len - (len - I + 1));
      if dirs.IndexOf(sDir) < 0 then
        dirs.Add(sDir);
      end;
    Inc(I);
    end;
  If Result > -1 then inc(Result);
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
  Result := (sCmd <>'');
end;

function cnvFormatFileSize(iSize:Int64):String;
var
  d:double;
begin
//   DebugLn( iSize);
  if gShortFileSizeFormat then
  begin
  // TODo  Giga
    if iSize div (1024*1024) >0 then
    begin
//      DebugLn( 'Div:',Trunc(iSize*10 /(1024*1024))/10);
      Result:=FloatToStrF((iSize*10 div (1024*1024))/10, ffFixed, 15, 1)+'M'
    end
    else
    if iSize div 1024 >0 then
    begin
      Result:=FloatToStrF((iSize*10 div 1024)/10, ffFixed, 15, 1)+'K'
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

end.

