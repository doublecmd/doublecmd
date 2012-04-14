(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbSpanSt.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: TAbSpanStream Class                         *}
{*********************************************************}
{* Stream to handle spanning ZIP files to diskettes      *}
{*********************************************************}

{$I AbDefine.inc}

unit AbSpanSt;

interface

uses
  Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, AbArcTyp, AbUtils, AbExcept;

type
  TAbSpanMode = (smReading, smWriting);
  TAbMediaType = (mtRemoveable, mtLocal);


  TAbSpanStream = class(TStream)
  private
    function GetSpace: Int64;
    function FixSpanNumber(ImageNumber: Integer): Integer;
  protected {private}
    FSpanMode     : TAbSpanMode;  {Reading or Writing                }
    FMediaType    : TAbMediaType; {Local or Removeable               }
    FThreshold    : Int64;      {Max size that can be written      }
    FSpanNumber   : Integer;      {Contains sequence of curr. span   }
    FImageName    : string;       {Contains name of curr. image      }
    FCancelled    : Boolean;      {Determines whether to abort       }
    FBytesWritten : Int64;      {Contains the no. of bytes
                                   written to the surrent span       }
    FBytesRead    : Int64;

    FBytesAvail   : Int64;      {Contains the no. of available
                                   bytes on the current media        }
    FStr          : TStream;      {Internal file stream              }
    FFileMode     : Word;         {File open mode for internal stream}
    FIgnoreSpanning : Boolean;      { only work within current span }    {!!.01}
    FSpanStreamInCharge : Boolean; {Span stream in charge of floppies} {!!.02}

    {fired when new media required   }
    FOnRequestImage     : TAbRequestImageEvent;
    FOnArchiveProgress  : TAbProgressEvent;                            {!!.04}
    FArchiveTotalWritten : Int64;                                    {!!.04}
    FArchiveTotalSize : Int64;                                       {!!.04}


    function MediaIsValid(const FName : string) : Boolean;
    function DoRequestNewMedia{(const Prompt: string)}: Boolean;         {!!.01}
    function NextDefaultImageName : string;
    function ValidateImageName(const NewName : string) : Boolean;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(const FileName: string; Mode: Word;
                       MediaType : TAbMediaType; Threshold : Int64);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    procedure GotoNext;

    property SpanMode : TAbSpanMode read FSpanMode;
    property MediaType : TAbMediaType read FMediaType write FMediaType;
    property SpanNumber : Integer read FSpanNumber write FSpanNumber;   {!!.01}
    property Threshold : Int64
      read  FThreshold write FThreshold
      default 0;
    property OnRequestImage : TAbRequestImageEvent
      read FOnRequestImage write FOnRequestImage;
    property OnArchiveProgress  : TAbProgressEvent                     {!!.04}
      read FOnArchiveProgress write FOnArchiveProgress;                {!!.04}
    property ArchiveTotalSize : Int64                                {!!.04}
      read FArchiveTotalSize write FArchiveTotalSize;                  {!!.04}

    property FreeSpace : Int64 read  GetSpace;

    property IgnoreSpanning : boolean                                    {!!.01}
      read FIgnoreSpanning write FIgnoreSpanning;                        {!!.01}
    property SpanStreamInCharge : Boolean                            {!!.02}
      read FSpanStreamInCharge write FSpanStreamInCharge;            {!!.02}

  end;

implementation

uses
  DCOSUtils, DCClassesUtf8;

{!!.01 -- added}
function TAbSpanStream.FixSpanNumber(ImageNumber: Integer): Integer;
begin
  Result := ImageNumber;
  if MediaType = mtRemoveable then
    Result := Succ(ImageNumber);
end;
{!!.01 -- end added}


{------------------------------------------------------------------------------}
function TAbSpanStream.Read(var Buffer; Count: Longint): Longint;
var
  Valid : Boolean;
begin
  Result := 0;                                                           {!!.01}
  if FIgnoreSpanning then begin                                          {!!.01}
    Result := FStr.Read(Buffer, Count);                                  {!!.01}
  end                                                                    {!!.01}
  else begin
    if (Count > 0) and (FStr.Position = FStr.Size) then begin { need next span }
      if not Assigned(FOnRequestImage) then exit;
      FStr.Free;
      FStr := nil;
      Inc(FSpanNumber);
      FOnRequestImage(Self, FixSpanNumber(FSpanNumber),
        FImageName, FCancelled);                                         {!!.01}
      if FCancelled then
        raise EAbUserAbort.Create;                                       {!!.05}
      FSpanStreamInCharge := True;                                       {!!.02}
      Valid := MediaIsValid(FImageName);
      if Valid and not FCancelled then begin
        FStr := TFileStreamEx.Create(FImageName, FFileMode);
      end else begin
        if not Valid then
          raise EAbFileNotFound.Create;
      end;
    end else
      Result := 0;

    if Assigned(FStr) then
      Result := FStr.Read(Buffer, Count);
  end;                                                                   {!!.01}
end;
{------------------------------------------------------------------------------}
{!!.01 -- added}
function Least(const a : array of Integer) : Integer;
var
  i : Integer;
begin
  Result := a[0];
  for i := Low(a) + 1 to High(a) do
    if a[i] < Result then
      Result := a[i];
end;
{!!.01 -- end added}

{!!.01 -- re-written}
function TAbSpanStream.Write(const Buffer; Count: Longint): Longint;
var
  CurWritten, TotalWritten : LongInt;
  LocalBuff, LocalPtr : PAnsiChar;
  Abort : Boolean;                                                     {!!.04}
begin
  if FSpanMode = smReading then
    Result := 0
  else begin

    if FMediaType = mtLocal then begin                  { media not removeable }
      if (FThreshold = 0) then begin                    { not local span }
        TotalWritten := FStr.Write(Buffer, Count);      { write buffer }
      end

      else begin                                        { it's a local span }
        if GetSpace > Count then begin                  { there's room on }
                                                          { the local span }
          CurWritten := FStr.Write(Buffer, Count);      { write buffer }
          Inc(FBytesWritten, CurWritten);
          TotalWritten := CurWritten;
        end
        else begin                                      { not enough room }
          GetMem(LocalBuff, Count);
          Move(Buffer, LocalBuff^, Count);
          LocalPtr := LocalBuff;

          TotalWritten := FStr.Write(LocalPtr^, GetSpace); { write as much as }
                                                              { there's room for }
          Inc(LocalPtr, TotalWritten);

          while TotalWritten < Count do begin           { still data in Buffer }
            DoRequestNewMedia{('Media Full')};            { skip to next medium }

            CurWritten :=                               { write as much as }
              FStr.Write(LocalPtr^, Least([Count-TotalWritten, GetSpace])); { there's room for }
            Inc(LocalPtr, CurWritten);
            Inc(FBytesWritten, CurWritten);
            Inc(TotalWritten, CurWritten);
          end; {while}

          FreeMem(LocalBuff);
        end; {if FBytesAvail }
      end; {if GetSpace }

    end

    else begin { media IS removeable }
      if GetSpace > Count then begin                    { there's room on }
                                                          { removeable span }
        TotalWritten := FStr.Write(Buffer, Count);      { write buffer }
        Inc(FBytesWritten, TotalWritten);
      end
      else begin                                        { not enough room }
        GetMem(LocalBuff, Count);
        Move(Buffer, LocalBuff^, Count);
        LocalPtr := LocalBuff;

        TotalWritten := FStr.Write(LocalPtr^, GetSpace); { write as much as }
                                                          { there's room for }
        Inc(LocalPtr, TotalWritten);

        while TotalWritten < Count do begin           { still data in Buffer }
          DoRequestNewMedia{('Media Full')};            { skip to next medium }

          CurWritten :=                               { write as much as }
            FStr.Write(LocalPtr^, Least([Count-TotalWritten, GetSpace])); { there's room for }

            Inc(LocalPtr, CurWritten);
            Inc(FBytesWritten, CurWritten);
            Inc(TotalWritten, CurWritten);
          end; {while}
      end {if GetSpace}

    end; { if FMediaType }

{!!.04 - changed }
//    Result := TotalWritten
    Inc(FArchiveTotalWritten, TotalWritten);
    Abort := False;
    if Assigned(FOnArchiveProgress) then
      FOnArchiveProgress(AbPercentage(FArchiveTotalWritten, FArchiveTotalSize), Abort);
    if not Abort then
      Result := TotalWritten
    else
      Result := 0;
{!!.04 - changed end }

  end; {if FSpanMode }
end;
{!!.01 -- end re-written}

{------------------------------------------------------------------------------}
function TAbSpanStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  Valid : Boolean;
  NewPos : Int64;
begin
  { can only seek when reading }
  if FSpanMode = smWriting then
    Result := FStr.Position                                              {!!.01}
  else begin
    NewPos := FStr.Position;
    Result := NewPos;

    case Origin of
      soBeginning : NewPos := Offset;
      soEnd : begin
        { calc size }
        NewPos := FStr.Size + Offset;
      end;
      soCurrent : begin
        NewPos := FStr.Position + Offset;
      end;
    end;

{!!.01}
    if FIgnoreSpanning then begin
      if NewPos < 0 then
        NewPos := 0;
      if NewPos > FStr.Size then
        NewPos := FStr.Size;
    end;
{!!.01}

    if (NewPos < 0) then begin  { past beginning of current stream }

      { request previous media }
      if not Assigned(FOnRequestImage) then exit;
      Dec(FSpanNumber);
      FOnRequestImage(Self,FixSpanNumber(FSpanNumber), FImageName, FCancelled);             {!!.01}
      if FCancelled then                                               {!!.05}
        raise EAbUserAbort.Create;

      { reset internal stream }
      Valid := MediaIsValid(FImageName);
      if Valid and not FCancelled then begin
        FStr.Free;
        FStr := nil;
        FStr := TFileStreamEx.Create(FImageName, FFileMode);
      end else begin
        if not Valid then
          raise EAbFileNotFound.Create;
      end;

      { seek rest of way in new stream}
      Result := Result + FStr.Seek(NewPos, soEnd);
    end
    else
    if (NewPos > FStr.Size) then begin { past end of current stream }
      { request next media }
      if not Assigned(FOnRequestImage) then exit;
      Dec(FSpanNumber);
      FOnRequestImage(Self, FixSpanNumber(FSpanNumber), FImageName, FCancelled);             {!!.01}
      if FCancelled then
        raise EAbUserAbort.Create;
      { reset internal stream }
      Valid := MediaIsValid(FImageName);
      if Valid and not FCancelled then begin
        FStr.Free;
        FStr := nil;
        FStr := TFileStreamEx.Create(FImageName, FFileMode);
      end else begin
        if not Valid then
          raise EAbFileNotFound.Create;
      end;

      { seek rest of way in new stream}
      Result := Result + FStr.Seek(NewPos - FStr.Size, soBeginning);
    end
    else { offset is within current stream } begin
      Result := FStr.Seek(NewPos, soBeginning);
    end;
  end;
end;

{------------------------------------------------------------------------------}
constructor TAbSpanStream.Create(const FileName: string; Mode: Word;
                                 MediaType : TAbMediaType; Threshold : Int64);
begin
  inherited Create;

  if AbGetPathType(FileName) <> ptAbsolute then                          {!!.02}
    raise EAbException.Create('Full Path Required');                     {!!.02}

  if ((Mode and fmCreate) = fmCreate) or
     ((Mode and fmOpenWrite) = fmOpenWrite) then
    FSpanMode := smWriting
  else if ((Mode and fmOpenRead) = fmOpenRead) then
    FSpanMode := smReading
  else
    { error: can't support read/write at same time}
    raise EAbException.Create('File must be opened for Read OR Write');

  FArchiveTotalWritten := 0;                                           {!!.04}
  FArchiveTotalSize := 0;                                              {!!.04}
  FSpanStreamInCharge := False;
  FImageName := FileName;
  FThreshold := Threshold;
  FMediaType := MediaType;
  FFileMode := Mode;

  FOnRequestImage := nil;
  FOnArchiveProgress := nil;

  if MediaIsValid(FileName) or (FSpanMode = smReading) then              {!!.02}
    FStr := TFileStreamEx.Create(FileName, Mode)
  else
    raise EAbException.Create( 'Invalid Media' );
end;
{------------------------------------------------------------------------------}
destructor TAbSpanStream.Destroy;
begin
  if FStr <> nil then
    FStr.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TAbSpanStream.MediaIsValid(const FName : string) : Boolean;
{- Determines if media is valid / formatted}
{$IFDEF MSWINDOWS}
var
  DriveLetter : string;
{$ENDIF}  
begin
  {$IFDEF MSWINDOWS}
  Result := True;
  if Pos(':', FName) > -1 then
    DriveLetter := UpperCase(FName[1])
  else
    DriveLetter := UpperCase(GetCurrentDir[1]);

  FBytesAvail := AbGetDriveFreeSpace(FName);

  if FBytesAvail < -1 then FBytesAvail := High(FBytesAvail);
  if FBytesAvail = -1 then         {either no disk or disk not formatted}
    Result := False;

  {check that filename doesn't already exist}
  if not Result then begin
    Result := not mbFileExists(FName);                                     {!!.01}
  end;
  {$ENDIF}
  {$IFDEF LINUX}                                                         {!!.01}
    FBytesAvail := AbGetDriveFreeSpace(FName);                           {!!.01}
    Result := {Result and} (FBytesAvail > 0);                            {!!.01}
  {$ENDIF}                                                               {!!.01}
end;
{------------------------------------------------------------------------------}
function TAbSpanStream.DoRequestNewMedia{(const Prompt: string)}: Boolean; {!!.01}
{- Fires OnRequestImage when new media is required}
var
  NewName   : string;
  ValidName : Boolean;
  Mode      : Word;
  SpanNo    : Byte;                                                      {!!.01}
begin
  Result := true;
  FStr.Free;                                                             {!!.01}
  FStr := nil;                                                           {!!.01}
  if Assigned(FOnRequestImage) then begin
    if MediaType = mtLocal then begin                                    {!!.01}
      NewName := NextDefaultImageName;
      SpanNo := SpanNumber + 1;                                          {!!.01} 
    end
    else begin { it's a floppy span }                                    {!!.01}
      NewName := FImageName;                                             {!!.01}
      SpanNo := SpanNumber + 2; { floppy spans are 1 based }             {!!.01}
    end;                                                                 {!!.01}
    ValidName := False;
    while ((not ValidName) and (not FCancelled)) do begin

      FOnRequestImage(Self, FixSpanNumber(SpanNo), NewName, FCancelled); {!!.01}
      if FCancelled then
        raise EAbUserAbort.Create;
      if not FCancelled then begin
        if ValidateImageName(NewName) then begin
          Mode := FFileMode;
          ValidName := True;
          FImageName := NewName;
          Inc(FSpanNumber);
          FBytesWritten := 0;
          FStr := TFileStreamEx.Create(FImageName, fmCreate);              {!!.01}
          FStr.Free;                                                     {!!.01}
          FStr := TFileStreamEx.Create(FImageName, Mode);
        end
        else                                                             {!!.01}
          Result := False;                                               {!!.01}
      end { if Not FCancelled}
      else {!!.05 [ 783614 ]}
       begin
          result := false;
          exit;
       end;
    end; { While }
   end { if Assigned(FOnRequestImage) then begin }
   else Raise EAbZipstreamFull.Create; {!!.05 [ 753982 ],[714944] }
end;
{------------------------------------------------------------------------------}
function TAbSpanStream.NextDefaultImageName : string;
begin
  Result := FImageName;
  if pos('.', Result) > 0 then
    Delete(Result, Pos('.', Result), Length(Result) - Pos('.', Result) + 1);
  Result := Result + '.z';
  if (FSpanNumber + 2) < 10 then  {!!.05 change +1 to +2}
    Result := Result + '0' + IntToStr(FSpanNumber + 2) {!!.05 change +1 to +2}
  else if (FSpanNumber + 1) < 100 then
    Result := Result + IntToStr(FSpanNumber + 2) {!!.05 change +1 to +2}
  else
    Result := '';
end;
{------------------------------------------------------------------------------}
function TAbSpanStream.ValidateImageName(const NewName : string) : Boolean;
begin
  Result := MediaIsValid(NewName);
end;
{------------------------------------------------------------------------------}
{!!.01 -- Rewritten}
function TAbSpanStream.GetSpace: Int64;
{ Return space remaining in current span}
var
  EffectiveThreshold : Int64;
begin
  if FMediaType = mtRemoveable then begin
    EffectiveThreshold := FThreshold;
    if EffectiveThreshold = 0 then
      EffectiveThreshold := MaxLongInt;
    Result := Least([EffectiveThreshold, AbGetDriveFreeSpace(FImageName)]);
  end
  else begin { spanning locally }
    if FThreshold = 0 then
      Result := AbGetDriveFreeSpace(FImageName)
    else
      Result := FThreshold - FBytesWritten;
  end;
end;
{!!.01 -- End Rewritten}
{------------------------------------------------------------------------------}
{!!.01 -- Rewritten}
procedure TAbSpanStream.SetSize(const NewSize: Int64);
var
  CurSize, Remaining : Int64;
begin
  if Assigned(FStr) then begin
    if NewSize = 0 then
      FStr.Size := 0
    else begin
      CurSize := FStr.Size;
      Remaining := AbGetDriveFreeSpace(FImageName);
      if NewSize < (CurSize + Remaining) then
        FStr.Size := NewSize
      else
        FStr.Size := CurSize + Remaining;
    end;
  end;
  inherited SetSize(NewSize);
end;
{------------------------------------------------------------------------------}
{!!.01 -- End Rewritten}
procedure TAbSpanStream.GotoNext;
var
  GotNewMedia : Boolean;
begin
  { close current span}
  FStr.Free;
  FStr := nil;

  repeat
    { ask for new media, if needed}
    GotNewMedia := DoRequestNewMedia;

  { until new media or cancelled}
  until GotNewMedia or FCancelled;

  if not FCancelled then begin
    { open new stream on new media}
    if FSpanMode = smWriting then
      FStr := TFileStreamEx.Create(FImageName, FFileMode);

    if FSpanMode = smReading then begin
      if mbFileExists(FImageName) then
        FStr := TFileStreamEx.Create(FImageName, FFileMode)
      else
        raise EAbException.Create('Cannot open spanned file: ' + FImageName);
    end;

    Inc(FSpanNumber);                                                  {!!.04}
  end
  else begin
  {cancel and cleanup}
    FStr.Free;
    FStr := nil;
    raise EAbUserAbort.Create;                                           {!!.01}

  end;
end;
{------------------------------------------------------------------------------}
end.
