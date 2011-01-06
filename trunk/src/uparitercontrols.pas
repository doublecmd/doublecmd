{
   Copyright (C) 2004  Flavio Etrusco
   Copyright (C) 2011  Koblov Alexander (Alexx2000@mail.ru)

   All rights reserved.

   Redistribution and use in source and binary forms, with or without modification,
   are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
   ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
   ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit uPariterControls;

{$mode objfpc}{$H+}

interface

uses
  LCLType,
  Forms,
  Messages,
  Controls,
  SysUtils,
  Graphics,
  Classes,
  SynEditKeyCmds,
  SynEditTextBuffer,
  SynEditHighlighter,
  SynEdit,
  uSynDiffControls,
  uDiff;

const
  SynSpaceGlyph = Chr($B7);     //'·'
  SynTabGlyph = Chr($BB);       //'»'

type

  { TSynDiffHighlighter }

  TSynDiffHighlighter = class(TSynCustomHighlighter)
  private
    fWhitespaceAttribute: TSynHighlighterAttributes;
    fAddedAttribute: TSynHighlighterAttributes;
    fRemovedAttribute: TSynHighlighterAttributes;
    fModifiedAttribute: TSynHighlighterAttributes;
    fUnmodifiedAttribute: TSynHighlighterAttributes;
    fDiff:   TDiff;
    fTokens: TStringList;
    {}
    fRun:      Integer;
    fTokenPos: Integer;
    fTokenLen: Integer;
    fTokenKind: TChangeKind;
    {}
    fAddedAttriPointer:   TSynHighlighterAttributes;
    fDefaultAttriPointer: TSynHighlighterAttributes;
    function GetEditor: TSynDiffEdit;
    procedure ComputeTokens(const aOldLine, aNewLine: String);
  protected
    procedure UpdateColors;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
  public
    constructor Create(aOwner: TSynDiffEdit); reintroduce; overload;
    constructor Create(aOwner: TComponent); overload; override;
    destructor Destroy; override;

    procedure ResetRange; override;
    procedure SetLine(const aNewValue: String; aLineNumber: Integer); override;

    function GetEol: Boolean; override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function GetTokenPos: Integer; override; // 0-based
    procedure Next; override;
    property Editor: TSynDiffEdit read GetEditor;
  end;

implementation

uses
  SynEditTypes, uHash;

{ TSynDiffHighlighter }

procedure TSynDiffHighlighter.ComputeTokens(const aOldLine, aNewLine: String);

  procedure Tokenize(const aText: String; aList: TStrings);
  var
    vTokenStart: PChar;
    vRun: PChar;
    vToken: String;
  begin
    vTokenStart := PChar(aText);
    vRun := vTokenStart;
    while vRun^ <> #0 do
    begin
      while vRun^ in [#32,#9] do
        Inc(vRun);
      if vTokenStart <> vRun then
      begin
        SetString(vToken, vTokenStart, vRun - vTokenStart);
        aList.Add(vToken);
        vTokenStart := vRun;
      end;
      {}
      while vRun^ in TSynValidStringChars do
        Inc(vRun);
      if vTokenStart <> vRun then
      begin
        SetString(vToken, vTokenStart, vRun - vTokenStart);
        aList.Add(vToken);
        vTokenStart := vRun;
      end;
      {}
      if vRun^ in [#33..#255] - TSynValidStringChars then
      begin
        aList.Add(vRun^);
        Inc(vRun);
        vTokenStart := vRun;
      end;
    end;
  end;

var
  vOldTokens: TStringList;
  vArray1: array of Integer;
  vArray2: array of Integer;
  i: Integer;
  vChange: TCompareRec;
  cChange, cLine: Integer;
begin
  { Tokenize fOldLine }
  vOldTokens := TStringList.Create;
  try
    Tokenize(aOldLine, vOldTokens);
    SetLength(vArray1, vOldTokens.Count);
    for i := Length(vArray1) -1 downto 0 do
      vArray1[i] := PtrInt(HashString(vOldTokens[i], False, False));
  finally
    vOldTokens.Free;
  end;
  { Tokenize fNewLine }
  Tokenize(aNewLine, fTokens);
  SetLength(vArray2, fTokens.Count);
  for i := Length(vArray2) -1 downto 0 do
    vArray2[i] := PtrInt(HashString(fTokens[i], False, False));
  { Calculate diffs }
  fDiff.Execute( PInteger(@(vArray1[0])), PInteger(@(vArray2[0])),
    Length(vArray1), Length(vArray2) );
  for cChange := 0 to fDiff.Count -1 do
  begin
    vChange := fDiff.Compares[cChange];
    if vChange.Kind <> ckDelete then
      for cLine := vChange.oldIndex1 to vChange.oldIndex2 do
        fTokens.Objects[cLine] := TObject(PtrInt(vChange.Kind));
  end;
  fDiff.Clear;
end;

constructor TSynDiffHighlighter.Create(aOwner: TComponent);
begin
  Create(aOwner as TSynDiffEdit);
end;

constructor TSynDiffHighlighter.Create(aOwner: TSynDiffEdit);
begin
  inherited Create(aOwner);
  fDiff := TDiff.Create(Self);
  {}
  fWhitespaceAttribute := TSynHighlighterAttributes.Create('Whitespace');
  AddAttribute(fWhitespaceAttribute);
  {}
  fAddedAttribute := TSynHighlighterAttributes.Create('Added');
  fAddedAttribute.Style := [fsBold];
  AddAttribute(fAddedAttribute);
  {}
  fRemovedAttribute := TSynHighlighterAttributes.Create('Removed');
  fRemovedAttribute.Style := [fsBold];
  AddAttribute(fRemovedAttribute);
  {}
  fModifiedAttribute := TSynHighlighterAttributes.Create('Modified');
  fModifiedAttribute.Style := [fsBold];
  AddAttribute(fModifiedAttribute);
  {}
  fUnmodifiedAttribute := TSynHighlighterAttributes.Create('Unmodified');
  AddAttribute(fUnmodifiedAttribute);
  {}
  UpdateColors;
  SetAttributesOnChange(@DefHighlightChange);
  fTokens := TStringList.Create;
end;

destructor TSynDiffHighlighter.Destroy;
begin
  inherited Destroy;
  fTokens.Free;
end;

function TSynDiffHighlighter.GetEditor: TSynDiffEdit;
begin
  Result := TSynDiffEdit(inherited Owner);
end;

function TSynDiffHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  if Index = SYN_ATTR_WHITESPACE then
    Result := fDefaultAttriPointer
  else
    Result := nil;
end;

function TSynDiffHighlighter.GetEol: Boolean;
begin
  Result := (fTokenLen = 0);
end;

function TSynDiffHighlighter.GetToken: String;
var
  cChar: Integer;
begin
  Result := fTokens[fRun];
  if (Editor.PaintStyle = psForeground) and (fTokenKind <> ckNone) then
    for cChar := 1 to Length(Result) do
      if Result[cChar] = #32 then
        Result[cChar] := SynSpaceGlyph
      else if Result[cChar] = #9 then
        Result[cChar] := SynTabGlyph;
end;

function TSynDiffHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenKind of
    ckAdd:
      Result := fAddedAttriPointer;
    ckModify:
      Result := fModifiedAttribute;
    else
      Result := fDefaultAttriPointer;
  end;
end;

function TSynDiffHighlighter.GetTokenKind: integer;
begin
  Result := Ord(fTokenKind);
end;

procedure TSynDiffHighlighter.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
  TokenLength:= fTokenLen;
  if TokenLength > 0 then
    TokenStart:= PChar(fTokens[fRun])
  else
    TokenStart:= nil;
end;

function TSynDiffHighlighter.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynDiffHighlighter.Next;
begin
  Inc(fRun);
  if fRun = fTokens.Count then
  begin
    fTokenLen := 0;
    Exit;
  end;
  Inc(fTokenPos, fTokenLen);
  fTokenLen := Length(fTokens[fRun]);
  fTokenKind := TChangeKind(PtrInt(fTokens.Objects[fRun]));
end;

procedure TSynDiffHighlighter.ResetRange;
begin
  fDefaultAttriPointer := fWhitespaceAttribute;
end;

procedure TSynDiffHighlighter.SetLine(const aNewValue: String; aLineNumber: Integer);
var
  vOtherEdit: TSynDiffEdit;
  vOldLine: String;
  vNewLine: String;
begin
  fDiff.Clear;
  fTokens.Clear;
  fRun := -1;
  fTokenPos := 0;
  fTokenLen := 0;

  if Editor.OriginalFile <> nil then
    begin
      fAddedAttriPointer := fAddedAttribute;
      vOtherEdit := Editor.OriginalFile;
    end
  else
    begin
      fAddedAttriPointer := fRemovedAttribute;
      vOtherEdit := Editor.ModifiedFile;
    end;

  if Editor.DiffKind[aLineNumber] = ckModify then
    fDefaultAttriPointer := fUnmodifiedAttribute
  else
    fDefaultAttriPointer := fWhitespaceAttribute;

  if (vOtherEdit <> nil) and (aLineNumber < vOtherEdit.Lines.Count) then
    vOldLine := vOtherEdit.Lines[aLineNumber];
  vNewLine := aNewValue;
  if Length(vNewLine) <> 0 then
    begin
      if Length(vOldLine) <> 0 then
        ComputeTokens(vOldLine, vNewLine)
      else
        fTokens.Add(vNewLine);
    end;
  Next;
end;

procedure TSynDiffHighlighter.UpdateColors;
begin
  BeginUpdate;
  try
    if Editor.PaintStyle = psForeground then
    begin
      fAddedAttribute.Foreground := Editor.Colors.Added;
      fAddedAttribute.Background := clBtnFace;
      fRemovedAttribute.Foreground := Editor.Colors.Deleted;
      fRemovedAttribute.Background := clBtnFace;
      fModifiedAttribute.Foreground := Editor.Colors.Modified;
      fModifiedAttribute.Background := clBtnFace;
      fUnmodifiedAttribute.Foreground := clNone;
      fUnmodifiedAttribute.Background := clBtnFace;
    end
    else begin
      fAddedAttribute.Foreground := clNone;
      fAddedAttribute.Background := Editor.Colors.Added;
      fRemovedAttribute.Foreground := clNone;
      fRemovedAttribute.Background := Editor.Colors.Deleted;
      fModifiedAttribute.Foreground := clText;
      fModifiedAttribute.Background := Editor.Colors.Modified;
      fUnmodifiedAttribute.Foreground := clNone;
      fUnmodifiedAttribute.Background := Editor.Colors.Modified;
    end;
  finally
    EndUpdate;
  end;
end;

end.

