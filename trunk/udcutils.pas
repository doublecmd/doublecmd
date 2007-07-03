{
Double Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : Alexander Koblov (Alexx2000@mail.ru)

Several useful functions

contributors:
Radek Cervinka  <radek.cervinka@centrum.cz>

Part of this code got from http://www.delphirus.com.ru

}

unit uDCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function GetCmdDirFromEnvVar(sPath : String) : String;
function SetCmdDirAsEnvVar(sPath : String) : String;
function GetDirs (DirName : String; var Dirs : TStringList) : Longint;
function GetAbsoluteFileName(sPath, sRelativeFileName : String) : String;
function ExtractOnlyFileName(const FileName: string): string;
procedure Split(const sFileNameWithParams : String; var sFileName, sParams : String);
Function cnvFormatFileSize(iSize:Int64):String;
Function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
                                           MaxLen: Integer): String;
function CharPos(C: Char; const S: string; StartPos: Integer = 1): Integer; overload;
function G_ValidateWildText(const S, Mask: string; bCaseSens : Boolean = False; MaskChar: Char = '?';
                             WildCard: Char = '*'): Boolean;
procedure DivFileName(const sFileName:String; var n,e:String);

implementation
uses
   uGlobs, uGlobsPaths, uVFSUtil;


function GetCmdDirFromEnvVar(sPath: String): String;
begin
  if Pos('%commander_path%', sPath) <> 0 then
    Result := StringReplace(sPath, '%commander_path%', ExcludeTrailingPathDelimiter(gpExePath), [rfIgnoreCase])
  else
    Result := sPath;
end;

function SetCmdDirAsEnvVar(sPath: String): String;
begin
  if Pos(gpExePath, sPath) <> 0 then
    Result := StringReplace(sPath, ExcludeTrailingPathDelimiter(gpExePath), '%commander_path%', [rfIgnoreCase])
  else
    Result := sPath;
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

procedure Split(const sFileNameWithParams : String; var sFileName, sParams : String);
var
  sr: TSearchRec;
  iSpacePos : Integer;
  sTempFileName : String;
  iLength : Integer;
  iSearchPos : Integer;
begin
  iSearchPos := 1;
  sFileName :=  sFileNameWithParams;
  iLength := Length(sFileNameWithParams);
  repeat
    iSpacePos := CharPos(' ', sFileNameWithParams, iSearchPos);
    iSearchPos := iSpacePos + 1;
    sFileName := Copy(sFileNameWithParams, 1, iSpacePos - 1 );
  until (FindFirst(sFileName, faAnyFile, sr) = 0)  or (iSpacePos = 0);
  if sFileName = '' then
    sFileName := sFileNameWithParams
  else
    sParams := Copy(sFileNameWithParams, iSpacePos + 1, iLength - iSpacePos);
end;

Function cnvFormatFileSize(iSize:Int64):String;
var
  d:double;
begin
//   WriteLN(output, iSize);
  if gShortFileSizeFormat then
  begin
  // TODo  Giga
    if iSize div (1024*1024) >0 then
    begin
//      WriteLN(output, 'Div:',Trunc(iSize*10 /(1024*1024))/10);
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
   
{=========================================================}
Function MinimizeFilePath(const PathToMince: String; Canvas: TCanvas;
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
  //WriteLN(output, 'PathX ' , Result);
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


function CharPos(C: Char; const S: string; StartPos: Integer = 1): Integer; overload;
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

{
  This function based on G_ValidateWildText from AcedUtils
  http://acedutils.narod.ru/AcedUtils.zip
}

function G_ValidateWildText(const S, Mask: string; bCaseSens : Boolean = False; MaskChar: Char = '?';
                             WildCard: Char = '*'): Boolean;
label
  99;
var
  L, X, X0, Q: Integer;
  P, P1, B: PChar;
  C: Char;
  sUpperS,
  sUpperMask : String;
begin
  if not bCaseSens then
    begin
    sUpperS := UpperCase(S);
    sUpperMask := UpperCase(Mask);
    end
  else
    begin
      sUpperS := S;
      sUpperMask := Mask;
    end;

  X := Pos(WildCard, sUpperMask);
  Result := False;
  if X = 0 then
  begin
    L := Length(sUpperMask);
    if (L > 0) and (L = Length(sUpperS)) then
    begin
      P := Pointer(sUpperS);
      B := Pointer(sUpperMask);
      repeat
        C := B^;
        if (C <> MaskChar) and (C <> P^) then
          Exit;
        Dec(L);
        Inc(B);
        Inc(P);
      until L = 0;
      Result := True;
    end;
    Exit;
  end;
  L := Length(sUpperS);
  P := Pointer(sUpperS);
  B := Pointer(sUpperMask);
  Q := X - 1;
  if L < Q then
    Exit;
  while Q > 0 do
  begin
    C := B^;
    if (C <> MaskChar) and (C <> P^) then
      Exit;
    Dec(Q);
    Inc(B);
    Inc(P);
  end;
  Dec(L, X - 1);
  repeat
    X0 := X;
    P1 := P;
    while sUpperMask[X0] = WildCard do
      Inc(X0);
    X := CharPos(WildCard, sUpperMask, X0);
    if X = 0 then
      Break;
  99:
    P := P1;
    B := @sUpperMask[X0];
    Q := X - X0;
    if L < Q then
      Exit;
    while Q > 0 do
    begin
      C := B^;
      if (C <> MaskChar) and (C <> P^) then
      begin
        Inc(P1);
        Dec(L);
        goto 99;
      end;
      Dec(Q);
      Inc(B);
      Inc(P);
    end;
    Dec(L, X - X0);
  until False;
  X := Length(sUpperMask);
  if L >= X - X0 + 1 then
  begin
    P := Pointer(sUpperS);
    Inc(P, Length(sUpperS) - 1);
    while X >= X0 do
    begin
      C := sUpperMask[X];
      if (C <> MaskChar) and (C <> P^) then
        Exit;
      Dec(X);
      Dec(P);
    end;
    Result := True;
  end;
end;



end.

