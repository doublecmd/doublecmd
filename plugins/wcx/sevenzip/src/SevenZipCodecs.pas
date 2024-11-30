{
   Double Commander
   -------------------------------------------------------------------------
   SevenZip archiver plugin

   Copyright (C) 2017-2023 Alexander Koblov (alexx2000@mail.ru)

   Based on Far Manager arclite plugin

   Copyright © 2000 Far Group

   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
   3. The name of the authors may not be used to endorse or promote products
      derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
   IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
   NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
   THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit SevenZipCodecs;

{$mode delphi}

interface

uses
  Classes, SysUtils, SevenZip, fgl, ActiveX, Windows, JclCompression;

const
  cmMaximum = PtrInt(Ord(High(TJclCompressionMethod)));

type

    { TLibraryInfo }

    TLibraryInfo = class
    public
      Handle: TLibHandle;
      CreateObject: TCreateObjectFunc;
      GetHandlerProperty2: TGetHandlerProperty2;
      GetHandlerProperty: TGetHandlerProperty;
      GetMethodProperty: TGetMethodProperty;
      GetNumberOfFormats: TGetNumberOfFormatsFunc;
      GetNumberOfMethods: TGetNumberOfMethodsFunc;
      SetLargePageMode: TSetLargePageMode;
      SetCodecs: function(compressCodecsInfo: ICompressCodecsInfo): HRESULT; winapi;
      CreateDecoder: function(Index: Cardinal; IID: PGUID; out Decoder): HRESULT; winapi;
      CreateEncoder: function(Index: Cardinal; IID: PGUID; out Coder): HRESULT; winapi;
    end;

    { TCodecInfo }

    TCodecInfo = class
      LibraryIndex: Integer;
      CodecIndex: Integer;
      EncoderIsAssigned: LongBool;
      DecoderIsAssigned: LongBool;
      Encoder: CLSID;
      Decoder: CLSID;
      ID: Cardinal;
      Name: UnicodeString;
    end;

    { TCompressCodecsInfo }

    TCompressCodecsInfo = class(TInterfacedObject, ICompressCodecsInfo, IUnknown)
    private
      FCodecs: TFPGObjectList<TCodecInfo>;
      FLibraries: TFPGObjectList<TLibraryInfo>;
    public
      constructor Create(ACodecs: TFPGObjectList<TCodecInfo>; ALibraries: TFPGObjectList<TLibraryInfo>);
    public
      function GetNumberOfMethods(NumMethods: PCardinal): HRESULT; winapi;
      function GetProperty(Index: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; winapi;
      function CreateDecoder(Index: Cardinal; IID: PGUID; out Decoder): HRESULT; winapi;
      function CreateEncoder(Index: Cardinal; IID: PGUID; out Coder): HRESULT; winapi;
    end;

procedure LoadLibraries;
function GetCodecName(AMethod: Cardinal): WideString;

var
  ACodecs: TFPGObjectList<TCodecInfo> = nil;

implementation

uses
  LazUTF8, FileUtil, DCOSUtils, SevenZipHlp;

{ TCompressCodecsInfo }

constructor TCompressCodecsInfo.Create(ACodecs: TFPGObjectList<TCodecInfo>;
  ALibraries: TFPGObjectList<TLibraryInfo>);
begin
  FCodecs:= ACodecs;
  FLibraries:= ALibraries;
end;

function TCompressCodecsInfo.GetNumberOfMethods(NumMethods: PCardinal): HRESULT; winapi;
begin
  NumMethods^:= FCodecs.Count;
  Result:= S_OK;
end;

function TCompressCodecsInfo.GetProperty(Index: Cardinal; PropID: TPropID; out
  Value: TPropVariant): HRESULT; winapi;
var
  ACodecInfo: TCodecInfo;
begin
  ACodecInfo:= FCodecs[Index];
  if (PropID = kDecoderIsAssigned) then
  begin
    Value.vt:= VT_BOOL;
    Value.bool:= ACodecInfo.DecoderIsAssigned;
    Exit(S_OK);
  end
  else if (PropID = kEncoderIsAssigned) then
  begin
    Value.vt:= VT_BOOL;
    Value.bool:= ACodecInfo.EncoderIsAssigned;
    Exit(S_OK);
  end;
  Result:= FLibraries[ACodecInfo.LibraryIndex].GetMethodProperty(ACodecInfo.CodecIndex, PropID, Value);
end;

function TCompressCodecsInfo.CreateDecoder(Index: Cardinal; IID: PGUID; out
  Decoder): HRESULT; winapi;
var
  ACodecInfo: TCodecInfo;
  ALibraryInfo: TLibraryInfo;
begin
  Result:= S_OK;
  ACodecInfo:= FCodecs[Index];
  if (ACodecInfo.DecoderIsAssigned) then
  begin
    ALibraryInfo:= FLibraries[ACodecInfo.LibraryIndex];
    if Assigned(ALibraryInfo.CreateDecoder) then
      Result:= ALibraryInfo.CreateDecoder(ACodecInfo.CodecIndex, IID, Decoder)
    else
      Result:= ALibraryInfo.CreateObject(@ACodecInfo.Decoder, IID, Decoder);
  end;
end;

function TCompressCodecsInfo.CreateEncoder(Index: Cardinal; IID: PGUID; out
  Coder): HRESULT; winapi;
var
  ACodecInfo: TCodecInfo;
  ALibraryInfo: TLibraryInfo;
begin
  Result:= S_OK;
  ACodecInfo:= FCodecs[Index];
  if (ACodecInfo.EncoderIsAssigned) then
  begin
    ALibraryInfo:= FLibraries[ACodecInfo.LibraryIndex];
    if Assigned(ALibraryInfo.CreateEncoder) then
      Result:= ALibraryInfo.CreateEncoder(ACodecInfo.CodecIndex, IID, Coder)
    else
      Result:= ALibraryInfo.CreateObject(@ACodecInfo.Encoder, IID, Coder);
  end;
end;

function GetCoderInfo(GetMethodProperty: TGetMethodProperty; Index: UInt32; var AInfo: TCodecInfo): Boolean;
var
  Value: TPropVariant;
begin
  Value.vt:= VT_EMPTY;
  if (GetMethodProperty(Index, kDecoder, Value) <> S_OK) then
    Exit(False);
  if (Value.vt <> VT_EMPTY) then
  begin
    if (Value.vt <> VT_BSTR) then Exit(False);
    try
      if (SysStringByteLen(Value.bstrVal) < SizeOf(CLSID)) then
      begin
        Exit(False);
      end;
      AInfo.Decoder:= PGUID(Value.bstrVal)^;
      AInfo.DecoderIsAssigned:= True;
    finally
      VarStringClear(Value);
    end;
  end;

  if (GetMethodProperty(Index, kEncoder, Value) <> S_OK) then
    Exit(False);
  if (Value.vt <> VT_EMPTY) then
  begin
    if (Value.vt <> VT_BSTR) then Exit(False);
    try
      if (SysStringByteLen(Value.bstrVal) < SizeOf(CLSID)) then
      begin
        Exit(False);
      end;
      AInfo.Encoder:= PGUID(Value.bstrVal)^;
      AInfo.EncoderIsAssigned:= True;
    finally
      VarStringClear(Value);
    end;
  end;

  if (GetMethodProperty(Index, kID, Value) <> S_OK) then
    Exit(False);
  if (Value.vt <> VT_UI8) then
    Exit(False);
  AInfo.ID:= Value.uhVal.QuadPart;

  Value.vt:= VT_EMPTY;
  if (GetMethodProperty(Index, kName, Value) <> S_OK) then
    Exit(False);
  if (Value.vt = VT_BSTR) then
  try
    AInfo.Name:= BinaryToUnicode(Value.bstrVal);
  finally
    VarStringClear(Value);
  end;

  Result:= AInfo.DecoderIsAssigned or AInfo.EncoderIsAssigned;
end;

var
  ALibraries: TFPGObjectList<TLibraryInfo> = nil;

procedure LoadCodecs;
var
  Handle: TLibHandle;
  Index, J: Integer;
  AFiles: TStringList;
  ACodecCount: Integer;
  NumMethods: UInt32 = 1;
  ACodecInfo: TCodecInfo;
  ALibraryInfo: TLibraryInfo;
  ACompressInfo: ICompressCodecsInfo;
begin
  AFiles:= FindAllFiles(ExtractFilePath(SevenzipLibraryName) + 'Codecs', '*.' + SharedSuffix);
  for Index:= 0 to AFiles.Count - 1 do
  begin
    Handle:= mbLoadLibrary(AFiles[Index]);
    if Handle <> 0 then
    begin
      ALibraryInfo:= TLibraryInfo.Create;

      ALibraryInfo.Handle:= Handle;
      ALibraryInfo.CreateObject:= GetProcAddress(Handle, 'CreateObject');
      ALibraryInfo.CreateDecoder:= GetProcAddress(Handle, 'CreateDecoder');
      ALibraryInfo.CreateEncoder:= GetProcAddress(Handle, 'CreateEncoder');
      ALibraryInfo.GetNumberOfMethods:= GetProcAddress(Handle, 'GetNumberOfMethods');
      ALibraryInfo.GetMethodProperty:= GetProcAddress(Handle, 'GetMethodProperty');

      if (Assigned(ALibraryInfo.CreateObject) or Assigned(ALibraryInfo.CreateDecoder) or
          Assigned(ALibraryInfo.CreateEncoder)) and Assigned(ALibraryInfo.GetMethodProperty) then
      begin
        ACodecCount:= ACodecs.Count;
        if Assigned(ALibraryInfo.GetNumberOfMethods) then
        begin
          if ALibraryInfo.GetNumberOfMethods(@NumMethods) = S_OK then
          begin
            for J := 0 to Int32(NumMethods) - 1 do
            begin
              ACodecInfo:= TCodecInfo.Create;
              ACodecInfo.LibraryIndex:= ALibraries.Count;
              ACodecInfo.CodecIndex:= J;
              if (GetCoderInfo(ALibraryInfo.GetMethodProperty, J, ACodecInfo)) then
                ACodecs.Add(ACodecInfo)
              else
                ACodecInfo.Free;
            end;
          end;
        end; // GetNumberOfMethods
        if (ACodecCount < ACodecs.Count) then
          ALibraries.Add(ALibraryInfo)
        else begin
          ALibraryInfo.Free;
          FreeLibrary(Handle);
        end;
      end;
    end;
  end;
  AFiles.Free;
  if (ACodecs.Count > 0) then
  begin
    ACompressInfo:= TCompressCodecsInfo.Create(ACodecs, ALibraries);
    for Index:= 0 to ALibraries.Count - 1 do
    begin
      if Assigned(ALibraries[Index].SetCodecs) then
        ALibraries[Index].SetCodecs(ACompressInfo);
    end;
  end;
end;

procedure LoadLibraries;
var
  ALibraryInfo: TLibraryInfo;
begin
  ACodecs:= TFPGObjectList<TCodecInfo>.Create;
  ALibraries:= TFPGObjectList<TLibraryInfo>.Create;

  // Add default library
  ALibraryInfo:= TLibraryInfo.Create;
  ALibraryInfo.Handle:= SevenzipLibraryHandle;
  ALibraryInfo.CreateObject:= SevenZip.CreateObject;
  ALibraryInfo.GetHandlerProperty2:= SevenZip.GetHandlerProperty2;
  ALibraryInfo.GetHandlerProperty:= SevenZip.GetHandlerProperty;
  ALibraryInfo.GetMethodProperty:= SevenZip.GetMethodProperty;
  ALibraryInfo.GetNumberOfFormats:= SevenZip.GetNumberOfFormats;
  ALibraryInfo.GetNumberOfMethods:= SevenZip.GetNumberOfMethods;
  ALibraryInfo.SetLargePageMode:= SevenZip.SetLargePageMode;
  ALibraryInfo.SetCodecs:= GetProcAddress(SevenzipLibraryHandle, 'SetCodecs');
  ALibraryInfo.CreateDecoder:= GetProcAddress(SevenzipLibraryHandle, 'CreateDecoder');
  ALibraryInfo.CreateEncoder:= GetProcAddress(SevenzipLibraryHandle, 'CreateEncoder');
  ALibraries.Add(ALibraryInfo);

  // Load external codecs
  LoadCodecs;
end;

function GetCodecName(AMethod: Cardinal): WideString;
var
  Index: Integer;
begin
  if Assigned(ACodecs) then begin
    for Index:= 0 to ACodecs.Count - 1 do
    begin
      if (ACodecs[Index].ID = AMethod) then
        Exit(ACodecs[Index].Name);
    end;
  end;
  Result:= EmptyWideStr;
end;

procedure Finish;
var
  Index: Integer;
begin
  if Assigned(ALibraries) then
  begin
    for Index:= 0 to ALibraries.Count - 1 do
    begin
      if Assigned(ALibraries[Index].SetCodecs) then
        ALibraries[Index].SetCodecs(nil);
      FreeLibrary(ALibraries[Index].Handle);
    end;
    ALibraries.Free;
  end;
  ACodecs.Free;
end;

finalization
  Finish;

end.

