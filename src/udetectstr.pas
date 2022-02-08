{
   Double Commander
   -------------------------------------------------------------------------
   Detect string parser.

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Copyright (C) 2009-2022  Alexander Koblov (alexx2000@mail.ru)

   Based on TMathControl by Vimil Saju
  
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

unit uDetectStr;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMasks, uFile;

type
  TMathType = (mtnil, mtoperator, mtlbracket, mtrbracket, mtoperand);

type
  TMathOperatorType = (monone, // NULL
                       moequ,  // =
                       moneq,  // != replaced with #
                       moles,  // <
                       momor,  // >
                       moand,  // &
                       moor,   // |
                       monot   // NOT
                      );

type
  PMathChar = ^TMathChar;
  TMathChar = record
    case mathtype: TMathType of
      mtoperand: (data: shortstring);
      mtoperator: (op: TMathOperatorType);
  end;

type

  { TParserControl }

  TParserControl = class
  private
    FData: TBytes;
    FForce: Boolean;
    FDataSize: Integer;
    FFileRead: Boolean;
    FDetectStr: String;
    FMathString: String;
    FInput, FOutput, FStack: array of TMathChar;
  private
    function FileRead(const FileName: String): Boolean;
    function Calculate(aFile: TFile; operand1, operand2, Aoperator: TMathChar): String;
    function GetOperator(C: AnsiChar): TMathOperatorType;
    function GetOperand(Mid: Integer; var Len: Integer): String;
    procedure ProcessString;
    procedure ConvertInfixToPostfix;
    function IsOperand(C: AnsiChar): Boolean;
    function IsOperator(C: AnsiChar): Boolean;
    function GetPrecedence(mop: TMathOperatorType): Integer;
    procedure SetDetectStr(const AValue: String);
    function BooleanToStr(X: Boolean): String;
    function StrToBoolean(S: String):Boolean;
  public
    function TestFileResult(const aFile: TFile): Boolean; overload;
    function TestFileResult(const aFileName: String): Boolean; overload;
  published
    property DetectStr: String read FDetectStr write SetDetectStr;
    property IsForce: Boolean read FForce write FForce;
  end;

implementation

uses
  DCStrUtils, DCClassesUtf8, uDebug, uFileProperty, uFileSystemFileSource,
  uFindMmap;

function TParserControl.FileRead(const FileName: String): Boolean;
begin
  FFileRead:= True;
  SetLength(FData, 8192);
  try
    with TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone) do
    try
      FDataSize:= Read(FData[0], Length(FData));
    finally
      Free;
    end;
    Result:= True;
  except
    FDataSize:= 0;
    Result:= False;
  end;
end;

function TParserControl.Calculate(aFile: TFile; operand1, operand2, Aoperator: TMathChar): String;
var
  ASize: Int64;
  AValue: Boolean;
  AChar, Index: Integer;
  tmp, data1, data2: String;
begin
  Result:= 'false';
  data1:= UpperCase(operand1.data);

  // NOT
  if (operand1.data = 'NOT') and ((operand2.data = 'true') or (operand2.data = 'false')) then
  begin
    Result:= BooleanToStr(not StrToBoolean(operand2.data));
  end;

  // & |
  if ((operand1.data = 'true') or (operand1.data = 'false')) and
     ((operand2.data = 'true') or (operand2.data = 'false')) then
  begin
    case Aoperator.op of
      moand: Result:= BooleanToStr((StrToBoolean(operand1.data)) and (StrToBoolean(operand2.data)));
      moor: Result:= BooleanToStr((StrToBoolean(operand1.data)) or (StrToBoolean(operand2.data)));
    end;
  end;

  // [X]= [X]!=
  if StrBegins(data1, '[') and StrEnds(data1, ']') then
  begin
    if FFileRead then
    begin
      if (FDataSize = 0) then Exit;
    end
    else begin
      if not FileRead(aFile.FullPath) then Exit;
    end;

    data2:= operand2.data;
    ASize:= Length(data1);
    Index:= StrToIntDef(Copy(data1, 2, ASize - 2), -1);

    if (Index >= 0) and (Index < FDataSize) then
    begin
      ASize:= Length(data2);

      if (ASize > 2) and (data2[1] = '"') and (data2[ASize] = '"') then
        AChar:= Ord(data2[2])
      else begin
        if not TryStrToInt(data2, AChar) then Exit;
      end;

      Result:= BooleanToStr(FData[Index] = AChar);
    end;
  end;

  // FIND FINDI
  if (data1 = 'FIND') or (data1 = 'FINDI') then
  begin
    if FFileRead then
    begin
      if (FDataSize = 0) then Exit;
    end
    else begin
      if not FileRead(aFile.FullPath) then Exit;
    end;
    data2:= operand2.data;
    ASize:= Length(data2);
    AValue:= (data1 = 'FIND');
    if (ASize > 2) and (data2[1] = '"') and (data2[ASize] = '"') then
    begin
      data2:= Copy(data2, 2, ASize - 2);
      Result:= BooleanToStr(PosMem(@FData[0], FDataSize, 0, data2, AValue, False) <> Pointer(-1));
    end;
  end;

  // EXT= EXT!=
  if (data1 = 'EXT') then
  begin
    tmp:= aFile.Extension;
    tmp:= UpperCase(tmp);
    tmp:= '"' + tmp + '"';
    case Aoperator.op of
      moequ: Result:= BooleanToStr(MatchesMask(tmp, operand2.data));
      moneq: Result:= BooleanToStr(not MatchesMask(tmp, operand2.data));
    end;
  end;

  // SIZE > < = !=
  if (data1 = 'SIZE') and (fpSize in aFile.SupportedProperties) then
  begin
    if TryStrToInt64(operand2.data, ASize) then
    begin
      case Aoperator.op of
        moequ: Result:= BooleanToStr(aFile.Size = ASize);
        moneq: Result:= BooleanToStr(aFile.Size <> ASize);
        moles: Result:= BooleanToStr(aFile.Size < ASize);
        momor: Result:= BooleanToStr(aFile.Size > ASize);
      end;
    end;
  end;
end;

function TParserControl.TestFileResult(const aFile: TFile): Boolean;
var
  I: Integer;
  tmp1, tmp2, tmp3: TMathChar;
begin
  if FMathString = '' then
  begin
    Result:= True;
    Exit;
  end;
  FFileRead:= False;
  SetLength(FStack, 0);
  for I:= 0 to Length(FOutput) - 1 do
  begin
    if FOutput[I].mathtype = mtoperand then
    begin
      SetLength(FStack, Length(FStack) + 1);
      FStack[Length(FStack) - 1]:= FOutput[I];
    end
    else if FOutput[I].mathtype = mtoperator then
    begin
      if Length(FStack) > 1 then
      begin
        tmp1:= FStack[Length(FStack) - 1];
        tmp2:= FStack[Length(FStack) - 2];
        SetLength(FStack, Length(FStack) - 2);
        tmp3.mathtype:= mtoperand;
        tmp3.data:= Calculate(aFile, tmp2, tmp1, FOutput[I]);
        SetLength(FStack, Length(FStack) + 1);
        FStack[Length(FStack) - 1]:= tmp3;
      end;
    end;
  end;
  Result:= (Length(FStack) > 0) and StrToBoolean(FStack[0].data);
  SetLength(FStack, 0);
end;

function TParserControl.TestFileResult(const aFileName: String): Boolean;
var
  aFile: TFile;
begin
  aFile:= TFileSystemFileSource.CreateFileFromFile(aFileName);
  try
    Result:= TestFileResult(aFile);
  finally
    aFile.Free;
  end;
end;

function TParserControl.GetOperator(C: AnsiChar): TMathOperatorType;
begin
  case C of
    '<': Result:= moles;
    '>': Result:= momor;
    '&': Result:= moand;
    '=': Result:= moequ;
    '#': Result:= moneq;
    '!': Result:= monot;
    '|': Result:= moor;
  else
         Result:= monone;
  end;
end;

function TParserControl.GetOperand(Mid: Integer; var Len: Integer): String;
var
  I: Integer;
begin
  Len:= High(FMathString);
  if (FMathString[Mid] = '"') then
  begin
    Result:= FMathString[Mid];
    for I:= Mid + 1 to Len do
    begin
      Result:= Result + FMathString[I];
      if FMathString[I] = '"' then Break;
    end;
  end
  else begin
    Result:= EmptyStr;
    for I:= Mid to Len do
    begin
      if IsOperand(FMathString[I]) then
        Result:= Result + FMathString[I]
      else
        Break;
    end;
  end;
  Len:= Length(Result);
end;

procedure TParserControl.ProcessString;
const
  Flags: TReplaceFlags = [rfReplaceAll, rfIgnoreCase];
var
  I: Integer;
  NumLen: Integer;
begin
  FMathString:= StringReplace(FMathString, 'FIND(', 'FIND=(', Flags);
  FMathString:= StringReplace(FMathString, '!=', '#', [rfReplaceAll]);
  FMathString:= StringReplace(FMathString, 'FINDI(', 'FINDI=(', Flags);
  FMathString:= StringReplace(FMathString, 'MULTIMEDIA', 'true', Flags);
  FMathString:= StringReplace(FMathString, 'FORCE', BooleanToStr(FForce), Flags);

  NumLen:= 1;
  while NumLen < Length(FMathString) do
  begin
    if (FMathString[NumLen] = '!') and (FMathString[NumLen + 1] <> '=') then
    begin
      I:= NumLen;
      Delete(FMathString, I, 1);
      Insert('NOT!', FMathString, I);
      Inc(NumLen, 4);
    end else Inc(NumLen);
  end;

  I:= 0;
  NumLen:= 0;
  SetLength(FInput, 0);
  SetLength(FStack, 0);
  SetLength(FOutput, 0);

  FMathString:= '(' + FMathString + ')';
  SetLength(FInput, Length(FMathString));

  while I <= Length(FMathString) - 1 do
  begin
    if FMathString[I + 1] = '(' then
    begin
      FInput[I].mathtype:= mtlbracket;
      Inc(I);
    end
    else if FMathString[I + 1] = ')' then
    begin
      FInput[I].mathtype:= mtrbracket;
      Inc(I);
    end
    else if IsOperator(FMathString[I + 1]) then
    begin
      FInput[I].mathtype:= mtoperator;
      FInput[I].op:= GetOperator(FMathString[I + 1]);
      Inc(I);
    end
    else if IsOperand(FMathString[I+1]) then
    begin
      FInput[I].mathtype:= mtoperand;
      FInput[I].data:= GetOperand(I + 1, NumLen);
      Inc(I, NumLen);
    end
    else {if FMathString[I + 1] = ' ' then} Inc(I);
  end;
end;

function TParserControl.IsOperator(C: AnsiChar): Boolean;
begin
  Result:= (C in ['=', '#', '!', '&', '<', '>', '|']);
end;

function TParserControl.IsOperand(C: AnsiChar): Boolean;
begin
  Result:= not (C in ['=', '#', '!', '&', '<', '>', '|', '(', ')', ' ']);
end;

function TParserControl.GetPrecedence(mop: TMathOperatorType): Integer;
begin
  case mop of
    moor:  Result:= 0;
    moand: Result:= 1;
    moequ: Result:= 2;
    moneq: Result:= 2;
    moles: Result:= 2;
    momor: Result:= 2;
    monot: Result:= 2;
  else
           Result:= -1;
  end;
end;

function TParserControl.BooleanToStr(X: Boolean): String;
begin
  if X then Result:= 'true' else Result:= 'false';
end;

procedure TParserControl.SetDetectStr(const AValue: String);
begin
  if FDetectStr <> AValue then
  begin
    FDetectStr:= AValue;
    FMathString:= AValue;
    ConvertInfixToPostfix;
  end;
end;

function TParserControl.StrToBoolean(S: String): Boolean;
begin
  if S = 'true' then Result:= True else Result:= False;
end;

procedure TParserControl.ConvertInfixToPostfix;
var
  i, j, prec: Integer;
begin
  ProcessString;
  for i:= 0 to Length(FInput) - 1 do
  begin
    if FInput[i].mathtype = mtoperand then
    begin
      SetLength(FOutput, Length(FOutput) + 1);
      FOutput[Length(FOutput) - 1]:= FInput[i];
    end
    else if FInput[i].mathtype = mtlbracket then
    begin
      SetLength(FStack, Length(FStack) + 1);
      FStack[Length(FStack) - 1]:= FInput[i];
    end
    else if FInput[i].mathtype = mtoperator then
    begin
      prec:= GetPrecedence(FInput[i].op);
      j:= Length(FStack) - 1;
      if j >= 0 then
      begin
        while (j >= 0) and (GetPrecedence(FStack[j].op) >= prec) do
        begin
          SetLength(FOutput, Length(FOutput) + 1);
          FOutput[Length(FOutput) - 1]:= FStack[j];
          Setlength(FStack, Length(FStack) - 1);
          j:= j - 1;
        end;
        SetLength(FStack, Length(FStack) + 1);
        FStack[Length(FStack) - 1]:= FInput[i];
      end;
    end
    else if FInput[i].mathtype = mtrbracket then
    begin
      j:= Length(FStack) - 1;
      if j >= 0 then
      begin
        while (j >= 0) and (FStack[j].mathtype <> mtlbracket) do
        begin
          SetLength(FOutput, Length(FOutput) + 1);
          FOutput[Length(FOutput) - 1]:= FStack[j];
          SetLength(FStack, Length(FStack) - 1);
          j:= j - 1;
        end;
        if j >= 0 then begin
          SetLength(FStack, Length(FStack) - 1);
        end;
      end;
    end;
  end;
end;

end.

