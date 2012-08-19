{
   The contents of this file are subject to the Mozilla Public License
   Version 1.1 (the "License"); you may not use this file except in compliance
   with the License. You may obtain a copy of the License at
   http://www.mozilla.org/MPL/

   Software distributed under the License is distributed on an "AS IS" basis,
   WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
   the specific language governing rights and limitations under the License.

   Alternatively, the contents of this file may be used under the terms of the
   GNU General Public License Version 2 or later (the "GPL"), in which case
   the provisions of the GPL are applicable instead of those above.
   If you wish to allow use of your version of this file only under the terms
   of the GPL and not to allow others to use your version of this file
   under the MPL, indicate your decision by deleting the provisions above and
   replace them with the notice and other provisions required by the GPL.
   If you do not delete the provisions above, a recipient may use your version
   of this file under either the MPL or the GPL.
}

unit uSynEditFiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEditLines, FPCAdds, DCClassesUtf8;

type

  { TSynEditFiler }

  TSynEditFiler = class(TObject)
  private
    FLineEndString: String;
    FLineEndLen: Integer;
    FLineEndType: TSynLinesFileLineEndType;
    procedure SetLineEndType(const AValue: TSynLinesFileLineEndType);
  protected
    fBuffer: PChar;
    fBufPtr: Cardinal;
    fBufSize: Cardinal;
    fFiler: TFileStreamEx;
    procedure Flush; virtual;
    procedure SetBufferSize(NewSize: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property LineEndType: TSynLinesFileLineEndType read FLineEndType write SetLineEndType;
  end;

  { TSynEditFileReader }

  TSynEditFileReader = class(TSynEditFiler)
  protected
    fEOF: Boolean;
    fFilePos: TStreamSeekType;
    fFileSize: TStreamSeekType;
    procedure FillBuffer;
  public
    constructor Create(const aFileName: UTF8String);
    function ReadLine: string;
    property EOF: Boolean read fEOF;
  end;

  { TSynEditFileWriter }

  TSynEditFileWriter = class(TSynEditFiler)
  protected
    procedure Flush; override;
  public
    constructor Create(const FileName: string);
    procedure Write(const S: string);
    procedure WriteLine(const S: string);
  end;

implementation

constructor TSynEditFiler.Create;
const
  kByte = 1024;
begin
  inherited Create;
  LineEndType := sfleSystem;
  SetBufferSize(16 * kByte);
  fBuffer[0] := #0;
end;

destructor TSynEditFiler.Destroy;
begin
  Flush;
  fFiler.Free;
  SetBufferSize(0);
  inherited Destroy;
end;

procedure TSynEditFiler.SetLineEndType(const AValue: TSynLinesFileLineEndType);
begin
  FLineEndType := AValue;
  case FLineEndType of
    sfleCrLf: FLineEndString := #13#10;
    sfleCr:   FLineEndString := #13;
    sfleLf:   FLineEndString := #10;
    else
      FLineEndString := LineEnding;
  end;
  FLineEndLen := length(FLineEndString);
end;

procedure TSynEditFiler.Flush;
begin
end;

procedure TSynEditFiler.SetBufferSize(NewSize: Cardinal);
begin
  if NewSize <> fBufSize then begin
    ReallocMem(fBuffer, NewSize);
    fBufSize := NewSize;
  end;
end;

constructor TSynEditFileReader.Create(const aFileName: UTF8String);
begin
  inherited Create;
  fEOF := False;
  fFiler := TFileStreamEx.Create(aFileName, fmOpenRead or fmShareDenyNone);
  fFileSize := fFiler.Size;
  fFiler.Seek(0, soFromBeginning);
end;

procedure TSynEditFileReader.FillBuffer;
var
  Count: Cardinal;
begin
  if fBufPtr >= fBufSize - 1 then
    fBufPtr := 0;
  Count := fFileSize - fFilePos;
  if Count >= fBufSize - fBufPtr then
    Count := fBufSize - fBufPtr - 1;
  fFiler.ReadBuffer(fBuffer[fBufPtr], Count);
  fBuffer[fBufPtr + Count] := #0;
  fFilePos := fFilePos + Count;
  fBufPtr := 0;
end;

function TSynEditFileReader.ReadLine: string;
var
  E, P, S: PChar;
begin
  Result := '';
  repeat
    if (fBuffer[fBufPtr] = #0) and (fFilePos >= fFileSize) then begin
      fEOF := True;
      Result := EmptyStr;
      Exit;
    end;
    S := PChar(@fBuffer[fBufPtr]);
    if S[0] = #0 then begin
      FillBuffer;
      S := PChar(@fBuffer[0]);
    end;
    E := PChar(@fBuffer[fBufSize]);
    P := S;
    while P + 2 < E do begin
      case P[0] of
        #10, #13:
          begin
            SetString(Result, S, P - S);
            // a single #13 is used in Mac OS files
            if (P[0] = #13) then begin
              if (P[1] = #10) then begin
                FLineEndType := sfleCrLf;
                inc(P);
              end
              else
                FLineEndType := sfleCr;
            end
            else
              FLineEndType := sfleLf;
            Inc(P);
            fBufPtr := P - fBuffer;
            Exit;
          end;
        #0:
          if fFilePos >= fFileSize then begin
            fEOF := True;
            fBufPtr := P - fBuffer;
            SetString(Result, S, P - S);
            Exit;
          end;
      end;
      Inc(P);
    end;
    // put the partial string to the start of the buffer, and refill the buffer
    Inc(P);
    if S > fBuffer then
      StrLCopy(fBuffer, S, P - S);
    fBufPtr := P - S;
    fBuffer[fBufPtr] := #0;
    // if line is longer than half the buffer then grow it first
    if 2 * Cardinal(P - S) > fBufSize then
      SetBufferSize(fBufSize + fBufSize);
  until FALSE;
end;

constructor TSynEditFileWriter.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStreamEx.Create(FileName, fmCreate);
  fFiler.Seek(0, soFromBeginning);
end;

procedure TSynEditFileWriter.Flush;
begin
  if fBufPtr > 0 then begin
    fFiler.WriteBuffer(fBuffer[0], fBufPtr);
    fBufPtr := 0;
  end;
end;

procedure TSynEditFileWriter.Write(const S: string);
var
  L: Cardinal;
begin
  L := Length(S);
  repeat
    if fBufPtr + L <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      Exit;
    end;
    Flush;
    if L > fBufSize then
      SetBufferSize(L);
  until FALSE;
end;

procedure TSynEditFileWriter.WriteLine(const S: string);
var
  L: Cardinal;
begin
  L := Length(S);
  repeat
    if fBufPtr + L + FLineEndLen <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      Move(FLineEndString[1], fBuffer[fBufPtr], FLineEndLen);
      Inc(fBufPtr, FLineEndLen);
      exit;
    end;
    Flush;
    if L + FLineEndLen > fBufSize then
      SetBufferSize(L + FLineEndLen);
  until FALSE;
end;

end.

