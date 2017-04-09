{
    Double Commander
    -------------------------------------------------------------------------
    Simple exif-wdx plugin.

    Copyright (C) 2016-2017 Alexander Koblov (alexx2000@mail.ru)

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uExifWdx;

{$mode delphi}

interface

uses
  Classes, SysUtils, WdxPlugin, uWDXModule, uExifReader;

type

  { TExifWdx }

  TExifWdx = class(TEmbeddedWDX)
  private
    FFileName: String;
    FExif: TExifReader;
    procedure GetData(const FileName: String);
  protected
    function GetAName: String; override;
    function GetADetectStr: String; override;
  public
    //---------------------
    constructor Create; override;
    destructor Destroy; override;
    //------------------------------------------------------
    procedure CallContentGetSupportedField; override;
    procedure CallContentSetDefaultParams; override;
    procedure CallContentStopGetValue(FileName: String); override;
    //---------------------
    function CallContentGetDefaultSortOrder(FieldIndex: Integer): Boolean; override;
    function CallContentGetDetectString: String; override;
    function CallContentGetValueV(FileName: String; FieldIndex, UnitIndex: Integer; flags: Integer): Variant; overload; override;
    function CallContentGetValue(FileName: String; FieldIndex, UnitIndex: Integer; flags: Integer): String; overload; override;
    function CallContentGetSupportedFieldFlags(FieldIndex: Integer): Integer; override;
    //------------------------------------------------------
  end;

implementation

{ TExifWdx }

procedure TExifWdx.GetData(const FileName: String);
begin
  if (FFileName <> FileName) then
  begin
    FFileName:= FileName;
    FExif.LoadFromFile(FileName);
  end;
end;

function TExifWdx.GetAName: String;
begin
  Result:= '<Exif>';
end;

function TExifWdx.GetADetectStr: String;
begin
  Result:= CallContentGetDetectString;
end;

constructor TExifWdx.Create;
begin
  inherited Create;
  FExif:= TExifReader.Create;
end;

destructor TExifWdx.Destroy;
begin
  FExif.Free;
  inherited Destroy;
end;

procedure TExifWdx.CallContentGetSupportedField;
begin
  AddField(rsMake, ft_string);
  AddField(rsModel, ft_string);
  AddField(rsImageWidth, ft_numeric_32);
  AddField(rsImageHeight, ft_numeric_32);
  AddField(rsOrientation, ft_numeric_32);
  AddField(rsDateTimeOriginal, ft_string);
end;

procedure TExifWdx.CallContentSetDefaultParams;
begin

end;

procedure TExifWdx.CallContentStopGetValue(FileName: String);
begin

end;

function TExifWdx.CallContentGetDefaultSortOrder(FieldIndex: Integer): Boolean;
begin
  Result:= False;
end;

function TExifWdx.CallContentGetDetectString: String;
begin
  Result:= '(EXT="JPG") | (EXT="JPEG")';
end;

function TExifWdx.CallContentGetValueV(FileName: String; FieldIndex,
  UnitIndex: Integer; flags: Integer): Variant;
begin
  Result:= Unassigned;
  EnterCriticalSection(FMutex);
  try
    GetData(FileName);
    case FieldIndex of
      0: if Length(FExif.Make) > 0 then Result:= FExif.Make;
      1: if Length(FExif.Model) > 0 then Result:= FExif.Model;
      2: if FExif.ImageWidth > 0 then Result:= FExif.ImageWidth;
      3: if FExif.ImageHeight > 0 then Result:= FExif.ImageHeight;
      4: if FExif.Orientation > 0 then Result:= FExif.Orientation;
      5: if Length(FExif.DateTimeOriginal) > 0 then Result:= FExif.DateTimeOriginal;
    end;
  finally
    LeaveCriticalSection(FMutex);
  end;
end;

function TExifWdx.CallContentGetValue(FileName: String; FieldIndex,
  UnitIndex: Integer; flags: Integer): String;
begin
  Result:= EmptyStr;
  EnterCriticalSection(FMutex);
  try
    GetData(FileName);
    case FieldIndex of
      0: Result:= FExif.Make;
      1: Result:= FExif.Model;
      2: if FExif.ImageWidth > 0 then Result:= IntToStr(FExif.ImageWidth);
      3: if FExif.ImageHeight > 0 then Result:= IntToStr(FExif.ImageHeight);
      4: if FExif.Orientation > 0 then Result:= IntToStr(FExif.Orientation);
      5: Result:= FExif.DateTimeOriginal;
    end;
  finally
    LeaveCriticalSection(FMutex);
  end;
end;

function TExifWdx.CallContentGetSupportedFieldFlags(FieldIndex: Integer): Integer;
begin
  Result:= 0;
end;

end.

