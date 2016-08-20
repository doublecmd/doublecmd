{
   Double commander
   -------------------------------------------------------------------------
   This unit contains class for working with file comments.

   Copyright (C) 2008-2016 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit uDescr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCClassesUtf8, uConvEncoding;

const
  DESCRIPT_ION = 'descript.ion';

type

  { TDescription }

  TDescription = class(TStringListEx)
  private
    FModified: Boolean;
    FLastDescrFile: String;
    FDestDescr: TDescription;
    FEncoding: TMacroEncoding;
    FNewEncoding: TMacroEncoding;
    procedure PrepareDescrFile(FileName: String);
    function GetDescription(Index: Integer): String;
    function GetDescription(const FileName: String): String;
    procedure SetDescription(Index: Integer; const AValue: String);
    procedure SetDescription(const FileName: String; const AValue: String);
    procedure SetEncoding(const AValue: TMacroEncoding);
  public
    {en
       Create TDescription class
       @param(UseSubDescr @true if need Copy/Move functions)
    }
    constructor Create(UseSubDescr: Boolean);
    {en
       Destroy TDescription class
    }
    destructor Destroy; override;
    {en
       Load data from description file
    }
    procedure LoadFromFile(const FileName: String); override;
    {en
       Save data to description file
    }
    procedure SaveToFile(const FileName: String); override;
    {en
       Add description for file
       @param(FileName File name)
       @param(Descr Description)
       @returns(Index of added item)
    }
    function AddDescription(FileName, Descr: String): Integer;
    {en
       Read description by file name
       @param(FileName File name)
       @returns(Description of file)
    }
    function ReadDescription(FileName: String): String;
    {en
       Write description for file
       @param(FileName File name)
       @param(Descr Description)
    }
    procedure WriteDescription(FileName: String; const Descr: String);
    {en
       Delete description by file name
       @param(FileName File name)
       @returns(The function returns @true if successful, @false otherwise)
    }
    function DeleteDescription(const FileName: String): Boolean;
    {en
       Copy description for file
       @param(FileNameFrom Source file name)
       @param(FileNameTo Destination file name)
       @returns(The function returns @true if successful, @false otherwise)
    }
    function CopyDescription(const FileNameFrom, FileNameTo: String): Boolean;
    {en
       Move description for file
       @param(FileNameFrom Source file name)
       @param(FileNameTo Destination file name)
       @returns(The function returns @true if successful, @false otherwise)
    }
    function MoveDescription(const FileNameFrom, FileNameTo: String): Boolean;
    {en
       Rename file in description
       @param(FileNameOld Old file name)
       @param(FileNameNew New file name)
       @returns(The function returns @true if successful, @false otherwise)
    }
    function Rename(const FileNameOld, FileNameNew: String): Boolean;
    {en
       Save all changes to description file
    }
    procedure SaveDescription;
    {en
       Reset last description file name
    }
    procedure Reset;

    function Find(const S: string; out Index: Integer): Boolean; override;
    {en
       File description encoding
    }
    property Encoding: TMacroEncoding read FEncoding write SetEncoding;
    {en
       Get description by file name
    }
    property DescrByFileName[const FileName: String]: String read GetDescription write SetDescription;
    {en
       Get description by file name index
    }
    property DescrByIndex[Index: Integer]: String read GetDescription write SetDescription;
  end;

implementation

uses
  LazUTF8, LConvEncoding, uDebug, DCOSUtils, DCConvertEncoding, DCUnicodeUtils,
  uGlobs;

{ TDescription }

procedure TDescription.PrepareDescrFile(FileName: String);
var
  sDescrFile: String;
begin
  sDescrFile:= ExtractFilePath(FileName) + DESCRIPT_ION;
  if sDescrFile <> FLastDescrFile then
    try
      // save previous decription file if need
      if FModified and (FLastDescrFile <> EmptyStr) and (Count > 0) then
        SaveToFile(FLastDescrFile);
      // load description file if exists
      FLastDescrFile:= sDescrFile;
      if not mbFileExists(FLastDescrFile) then
      begin
        Clear;
        FModified:= False;
        // use new encoding if new file
        FEncoding:= FNewEncoding;
      end
      else begin
        FEncoding:= gDescReadEncoding;
        LoadFromFile(FLastDescrFile);
        // set target encoding
        if Assigned(FDestDescr) then begin
          FDestDescr.FNewEncoding:= FEncoding;
        end;
      end;
    except
      on E: Exception do
        DCDebug('TDescription.PrepareDescrFile - ' + E.Message);
    end;
end;

function TDescription.Find(const S: string; out Index: Integer): Boolean;
var
  iIndex, iPosOfDivider, iLength, iFirstStringPos: Integer;
  sFileName, sIndexString: String;
  cSearchChar : Char;
begin
  Result:= False;
  sFileName:= ExtractFileName(S);
  //DCDebug('#########################');
  //DCDebug('sFileName:               '+ sFileName);
  for iIndex:= Count - 1 downto 0 do
  begin
    sIndexString := Self[iIndex];
    //DCDebug('Self[I]:                 '+ sIndexString);
    //DCDebug('iIndex:                  '+ IntToStr(iIndex));
    //DCDebug('Count:                   '+ IntToStr(Count));
    //DCDebug('Pos(sFileName, Self[I]): '+ IntToStr(Pos(sFileName, sIndexString)));

    // File comment length
    iLength := Length(sIndexString);
    if iLength = 0 then Continue;

    //at the first, look if first char a "
    if(sIndexString[1]='"')then
    begin // YES
      cSearchChar := '"';
      iFirstStringPos := 2;
    end
    else
    begin //NO
     cSearchChar := ' ';
     iFirstStringPos := 1;
    end;

    // find position of next cSearchChar in sIndexString
    iPosOfDivider:= 2;
    while (iPosOfDivider < iLength) do // don't look above the strings length
    begin
      // is at this position the cSearchChar?
      if (sIndexString[iPosOfDivider] = cSearchChar) then
      begin // YES
        // found the sFileName in the sIndexString (no more, no less)
        if mbCompareFileNames(sFileName, Copy(sIndexString, iFirstStringPos, iPosOfDivider-iFirstStringPos)) then
        begin // YES
          Index := iIndex;
          Exit(True);
        end
        else
        begin // NO
          Break;
        end;
      end;
      Inc(iPosOfDivider);
    end;
  end;
end;

function TDescription.GetDescription(Index: Integer): String;
var
  sLine: String;
  iDescrStart: Integer;
begin
  sLine:= Self[Index];
  if Pos(#34, sLine) <> 1 then
    begin
      iDescrStart:= Pos(#32, sLine);
      Result:= Copy(sLine, iDescrStart+1, Length(sLine) - iDescrStart);
    end
  else
    begin
      iDescrStart:= Pos(#34#32, sLine);
      Result:= Copy(sLine, iDescrStart+2, Length(sLine) - iDescrStart);
    end;
end;

function TDescription.GetDescription(const FileName: String): String;
var
  I: Integer;
begin
  if Find(FileName, I) then
    Result:= GetDescription(I)
  else begin
    Result:= EmptyStr;
  end;
end;

procedure TDescription.SetDescription(Index: Integer; const AValue: String);
var
  sLine,
  sFileName: String;
  iDescrStart: Integer;
begin
  FModified:= True;
  sLine:= Self[Index];
  if Pos('"', sLine) <> 1 then
    begin
      iDescrStart:= Pos(#32, sLine);
      sFileName:= Copy(sLine, 1, iDescrStart);
      Self[Index]:= sFileName + AValue;
    end
  else
    begin
      iDescrStart:= Pos(#34#32, sLine);
      sFileName:= Copy(sLine, 1, iDescrStart+1);
      Self[Index]:= sFileName + AValue;
    end;
end;

procedure TDescription.SetDescription(const FileName: String;
  const AValue: String);
var
  I: Integer;
begin
  if Find(FileName, I) then
    SetDescription(I, AValue)
  else
    AddDescription(FileName, AValue);
end;

procedure TDescription.SetEncoding(const AValue: TMacroEncoding);
begin
  if FEncoding <> AValue then
  begin
    FEncoding:= AValue;
    if mbFileExists(FLastDescrFile) then
      LoadFromFile(FLastDescrFile);
  end;
end;

constructor TDescription.Create(UseSubDescr: Boolean);
begin
  FModified:= False;
  FEncoding:= gDescReadEncoding;
  if gDescCreateUnicode then
    FNewEncoding:= gDescWriteEncoding
  else begin
    FNewEncoding:= gDescReadEncoding;
  end;
  if UseSubDescr then begin
    FDestDescr:= TDescription.Create(False)
  end;
  inherited Create;
end;

destructor TDescription.Destroy;
begin
  FreeAndNil(FDestDescr);
  inherited Destroy;
end;

procedure TDescription.LoadFromFile(const FileName: String);
var
  S: String;
  fsFileStream: TFileStreamEx;
begin
  FModified:= False;
  fsFileStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(S, fsFileStream.Size);
    fsFileStream.Read(S[1], Length(S));
  finally
    fsFileStream.Free;
  end;
  // Try to guess encoding
  FEncoding:= DetectEncoding(S, FEncoding, True);
  // If need convert encoding
  case FEncoding of
    meUTF8:    Text:= S;
    meOEM:     Text:= CeOemToUtf8(S);
    meANSI:    Text:= CeAnsiToUtf8(S);
    meUTF8BOM: Text:= Copy(S, 4, MaxInt);
    meUTF16LE: Text:= Utf16LEToUtf8(Copy(S, 3, MaxInt));
    meUTF16BE: Text:= Utf16BEToUtf8(Copy(S, 3, MaxInt));
  end;
end;

procedure TDescription.SaveToFile(const FileName: String);
const
  faSpecial = faHidden or faSysFile;
var
  S: String;
  Attr: Integer;
  fsFileStream: TFileStreamEx;
begin
  FModified:= False;
  case FEncoding of
    meUTF8:    S:= Text;
    meANSI:    S:= CeUtf8ToAnsi(Text);
    meOem:     S:= CeUtf8ToOem(Text);
    meUTF8BOM: S:= UTF8BOM + Text;
    meUTF16LE: S:= UTF16LEBOM + Utf8ToUtf16LE(Text);
    meUTF16BE: S:= UTF16BEBOM + Utf8ToUtf16BE(Text);
  end;
  Attr:= FileGetAttr(UTF8ToSys(FileName));
  // Remove hidden & system attributes
  if (Attr <> -1) and ((Attr and faSpecial) <> 0) then begin
    FileSetAttr(UTF8ToSys(FileName), faArchive);
  end;
  fsFileStream:= TFileStreamEx.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    fsFileStream.Write(S[1], Length(S));
  finally
    fsFileStream.Free;
  end;
  // Restore original attributes
  if (Attr <> -1) and ((Attr and faSpecial) <> 0) then begin
    FileSetAttr(UTF8ToSys(FileName), Attr);
  end;
end;

function TDescription.AddDescription(FileName, Descr: String): Integer;
begin
  FModified:= True;
  FileName:= ExtractFileName(FileName);
  if Pos(#32, FileName) <> 0 then
    Result := Add(#34+FileName+#34#32+Descr)
  else
    Result := Add(FileName+#32+Descr);
end;

function TDescription.ReadDescription(FileName: String): String;
begin
  PrepareDescrFile(FileName);
  Result:= GetDescription(FileName);
end;

procedure TDescription.WriteDescription(FileName: String; const Descr: String);
begin
  PrepareDescrFile(FileName);
  SetDescription(FileName, Descr);
end;

function TDescription.DeleteDescription(const FileName: String): Boolean;
var
  I: Integer;
begin
  Result:= False;
  PrepareDescrFile(FileName);
  if Find(FileName, I) then
  begin
    Delete(I);
    FModified:= True;
    Result:= True;
  end;
end;

function TDescription.CopyDescription(const FileNameFrom, FileNameTo: String): Boolean;
var
  I: Integer;
begin
  Result:= False;
  PrepareDescrFile(FileNameFrom);
  if Find(FileNameFrom, I) then
  begin
    DCDebug(FileNameFrom, '=', DescrByIndex[I]);
    FDestDescr.WriteDescription(FileNameTo, DescrByIndex[I]);
    Result:= True;
  end;
end;

function TDescription.MoveDescription(const FileNameFrom, FileNameTo: String): Boolean;
var
  I: Integer;
begin
  Result:= False;
  PrepareDescrFile(FileNameFrom);
  if Find(FileNameFrom, I) then
  begin
    DCDebug(FileNameFrom, '=', DescrByIndex[I]);
    FDestDescr.WriteDescription(FileNameTo, DescrByIndex[I]);
    Delete(I);
    FModified:= True;
    Result:= True;
  end;
end;

function TDescription.Rename(const FileNameOld, FileNameNew: String): Boolean;
var
  I: Integer;
  AValue: String;
begin
  Result:= False;
  PrepareDescrFile(FileNameOld);
  if Find(FileNameOld, I) then
  begin
    AValue:= GetDescription(I); Delete(I);
    AddDescription(FileNameNew, AValue);
    FModified:= True;
    Result:= True;
  end;
end;

procedure TDescription.SaveDescription;
begin
  try
    if FModified then
    begin
      if Count > 0 then
        SaveToFile(FLastDescrFile)
      else
        mbDeleteFile(FLastDescrFile);
    end;
    if Assigned(FDestDescr) then
      FDestDescr.SaveDescription;
  except
    on E: Exception do
      DCDebug('TDescription.SaveDescription - ' + E.Message);
  end;
end;

procedure TDescription.Reset;
begin
  FLastDescrFile:= EmptyStr;
end;

end.

