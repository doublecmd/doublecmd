{
    Double Commander
    -------------------------------------------------------------------------
    Implementation of configuration file in XML.

    Based on XmlConf from fcl-xml package.

    Copyright (C) 2010  Przemyslaw Nagay (cobines@gmail.com)

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
unit DCXmlConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite;

type
  // Define type aliases so we don't have to include DOM if we want to use config.
  TXmlNode = TDOMNode;
  TXmlPath = DOMString;

  { TXmlConfig }

  TXmlConfig = class
  private
    FFileName: UTF8String;
    FDoc: TXMLDocument;

    function GetRootNode: TXmlNode;
    procedure SplitPathToNodeAndAttr(const Path: DOMString; out NodePath: DOMString; out AttrName: DOMString);

  public
    constructor Create; virtual;
    constructor Create(const AFileName: UTF8String; AutoLoad: Boolean = False); virtual;
    destructor Destroy; override;

    procedure Clear;

    function AddNode(const RootNode: TDOMNode; const ValueName: DOMString): TDOMNode;
    procedure DeleteNode(const RootNode: TDOMNode; const Path: DOMString);
    procedure DeleteNode(const Node: TDOMNode);
    procedure ClearNode(const Node: TDOMNode);
    function FindNode(const RootNode: TDOMNode; const Path: DOMString; bCreate: Boolean = False): TDOMNode;
    function GetContent(const Node: TDOMNode): UTF8String;
    function IsEmpty: Boolean;
    procedure SetContent(const Node: TDOMNode; const AValue: UTF8String);

    // ------------------------------------------------------------------------

    function GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: UTF8String): UTF8String;
    function GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Boolean): Boolean;
    function GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Integer): Integer;
    function GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Int64): Int64;
    function GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Double): Double;
    function GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: UTF8String): UTF8String;
    function GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Boolean): Boolean;
    function GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Integer): Integer;
    function GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Int64): Int64;
    function GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Double): Double;

    // The Try... functions return True if the attribute/node was found and only then set AValue.
    function TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: UTF8String): Boolean;
    function TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Boolean): Boolean;
    function TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Integer): Boolean;
    function TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Int64): Boolean;
    function TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Double): Boolean;
    function TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: UTF8String): Boolean;
    function TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Boolean): Boolean;
    function TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Integer): Boolean;
    function TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Int64): Boolean;
    function TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Double): Boolean;

    // ------------------------------------------------------------------------

    // AddValue functions always add a new node.
    procedure AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: String);
    procedure AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Boolean);
    procedure AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Integer);
    procedure AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Int64);
    procedure AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Double);
    procedure AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: String);
    procedure AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Boolean);
    procedure AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Integer);
    procedure AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Int64);
    procedure AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Double);

    // SetValue functions can only set values for unique paths.
    procedure SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: UTF8String);
    procedure SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Boolean);
    procedure SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Integer);
    procedure SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Int64);
    procedure SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Double);
    procedure SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: String);
    procedure SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Boolean);
    procedure SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Integer);
    procedure SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Int64);
    procedure SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Double);

    // ------------------------------------------------------------------------

    procedure GetFont(const aNode: TXmlNode; Path: TXmlPath;
                      out Name: UTF8String; out Size: Integer; out Style: Integer;
                      const DefName: UTF8String; const DefSize: Integer; const DefStyle: Integer);

    procedure SetFont(const aNode: TXmlNode; Path: TXmlPath;
                      const Name: UTF8String; const Size: Integer; const Style: Integer);

    // ------------------------------------------------------------------------

    procedure ReadFromFile(const AFilename: UTF8String);
    procedure ReadFromStream(AStream: TStream);
    procedure WriteToFile(const AFilename: UTF8String);
    procedure WriteToStream(AStream: TStream);

    function Load: Boolean;
    function LoadBypassingErrors: Boolean;
    function Save: Boolean;

    {en
       Get path of form "<RootNodeName>/<Child1NodeName>/<Child2NodeName>...".
    }
    function GetPathFromNode(aNode: TDOMNode): String;

    property FileName: UTF8String read FFileName write FFileName;
    property RootNode: TXmlNode read GetRootNode;
  end;

  EFileEmpty = class(EFilerError);
  EFileNotFound = class(EFilerError);

implementation

uses
  LazUTF8, LazLogger, DCOSUtils, DCClassesUtf8, URIParser;

const
  BoolStrings: array[Boolean] of DOMString = ('False', 'True');

constructor TXmlConfig.Create;
begin
  Clear;
end;

constructor TXmlConfig.Create(const AFileName: UTF8String; AutoLoad: Boolean);
begin
  FFileName := AFileName;
  if not (AutoLoad and LoadBypassingErrors) then
    Clear;
end;

destructor TXmlConfig.Destroy;
begin
  FreeAndNil(FDoc);
  inherited Destroy;
end;

procedure TXmlConfig.Clear;
begin
  FreeAndNil(FDoc);
  FDoc := TXMLDocument.Create;
  FDoc.AppendChild(FDoc.CreateElement(ApplicationName));
end;

function TXmlConfig.GetRootNode: TXmlNode;
begin
  Result := FDoc.DocumentElement;
end;

// ------------------------------------------------------------------------

function TXmlConfig.GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: UTF8String): UTF8String;
begin
  if not TryGetAttr(RootNode, Path, Result) then
    Result := ADefault;
end;

function TXmlConfig.GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Boolean): Boolean;
begin
  if not TryGetAttr(RootNode, Path, Result) then
    Result := ADefault;
end;

function TXmlConfig.GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Integer): Integer;
begin
  if not TryGetAttr(RootNode, Path, Result) then
    Result := ADefault;
end;

function TXmlConfig.GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Int64): Int64;
begin
  if not TryGetAttr(RootNode, Path, Result) then
    Result := ADefault;
end;

function TXmlConfig.GetAttr(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Double): Double;
begin
  if not TryGetAttr(RootNode, Path, Result) then
    Result := ADefault;
end;

function TXmlConfig.TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: UTF8String): Boolean;
var
  Node: TDOMNode;
  Attr: TDOMAttr;
  NodePath, AttrName: DOMString;
begin
  SplitPathToNodeAndAttr(Path, NodePath, AttrName);
  if NodePath <> EmptyWideStr then
  begin
    Node := FindNode(RootNode, NodePath, False);
    if not Assigned(Node) then
      Exit(False);
  end
  else
    Node := RootNode;

  Attr := TDOMElement(Node).GetAttributeNode(AttrName);
  Result := Assigned(Attr);
  if Result then
    AValue := UTF16ToUTF8(Attr.Value);
end;

function TXmlConfig.TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Boolean): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetAttr(RootNode, Path, sValue);
  if Result then
  begin
    if SameText(sValue, 'TRUE') then
      AValue := True
    else if SameText(sValue, 'FALSE') then
      AValue := False
    else
      Result := False;  // If other text then return not found.
  end;
end;

function TXmlConfig.TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Integer): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetAttr(RootNode, Path, sValue) and TryStrToInt(sValue, AValue);
end;

function TXmlConfig.TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Int64): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetAttr(RootNode, Path, sValue) and TryStrToInt64(sValue, AValue);
end;

function TXmlConfig.TryGetAttr(const RootNode: TDOMNode; const Path: DOMString; out AValue: Double): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetAttr(RootNode, Path, sValue) and TryStrToFloat(sValue, AValue);
end;

function TXmlConfig.GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: UTF8String): UTF8String;
var
  Node: TDOMNode;
begin
  Node := FindNode(RootNode, Path, False);
  if Assigned(Node) then
    Result := UTF16ToUTF8(Node.TextContent)
  else
    Result := ADefault;
end;

function TXmlConfig.IsEmpty: Boolean;
begin
  Result := RootNode.ChildNodes.Count = 0;
end;

function TXmlConfig.GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Boolean): Boolean;
var
  sValue: UTF8String;
begin
  sValue := GetValue(RootNode, Path, '');
  if SameText(sValue, 'TRUE') then
    Result := True
  else if SameText(sValue, 'FALSE') then
    Result := False
  else
    Result := ADefault;
end;

function TXmlConfig.GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Integer): Integer;
begin
  Result := StrToIntDef(GetValue(RootNode, Path, ''), ADefault);
end;

function TXmlConfig.GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Int64): Int64;
begin
  Result := StrToInt64Def(GetValue(RootNode, Path, ''), ADefault);
end;

function TXmlConfig.GetValue(const RootNode: TDOMNode; const Path: DOMString; const ADefault: Double): Double;
begin
  Result := StrToFloatDef(GetValue(RootNode, Path, ''), ADefault);
end;

function TXmlConfig.TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: UTF8String): Boolean;
var
  Node: TDOMNode;
begin
  Node := FindNode(RootNode, Path, False);
  Result := Assigned(Node);
  if Result then
    AValue := UTF16ToUTF8(Node.TextContent);
end;

function TXmlConfig.TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Boolean): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetValue(RootNode, Path, sValue);
  if Result then
  begin
    if SameText(sValue, 'TRUE') then
      AValue := True
    else if SameText(sValue, 'FALSE') then
      AValue := False
    else
      Result := False;  // If other text then return not found.
  end;
end;

function TXmlConfig.TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Integer): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetValue(RootNode, Path, sValue) and TryStrToInt(sValue, AValue);
end;

function TXmlConfig.TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Int64): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetValue(RootNode, Path, sValue) and TryStrToInt64(sValue, AValue);
end;

function TXmlConfig.TryGetValue(const RootNode: TDOMNode; const Path: DOMString; out AValue: Double): Boolean;
var
  sValue: UTF8String;
begin
  Result := TryGetValue(RootNode, Path, sValue) and TryStrToFloat(sValue, AValue);
end;

// ----------------------------------------------------------------------------

procedure TXmlConfig.AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: String);
var
  Node: TDOMNode;
begin
  Node := RootNode.AppendChild(FDoc.CreateElement(ValueName));
  Node.TextContent := UTF8ToUTF16(AValue);
end;

procedure TXmlConfig.AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Boolean);
begin
  if AValue <> DefaultValue then
    AddValue(RootNode, ValueName, AValue);
end;

procedure TXmlConfig.AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Double);
begin
  if AValue <> DefaultValue then
    AddValue(RootNode, ValueName, AValue);
end;

procedure TXmlConfig.AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Int64);
begin
  if AValue <> DefaultValue then
    AddValue(RootNode, ValueName, AValue);
end;

procedure TXmlConfig.AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: Integer);
begin
  if AValue <> DefaultValue then
    AddValue(RootNode, ValueName, AValue);
end;

procedure TXmlConfig.AddValueDef(const RootNode: TDOMNode; const ValueName: DOMString; const AValue, DefaultValue: String);
begin
  if AValue <> DefaultValue then
    AddValue(RootNode, ValueName, AValue);
end;

procedure TXmlConfig.AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Boolean);
begin
  AddValue(RootNode, ValueName, BoolStrings[AValue]);
end;

procedure TXmlConfig.AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Integer);
begin
  AddValue(RootNode, ValueName, IntToStr(AValue));
end;

procedure TXmlConfig.AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Int64);
begin
  AddValue(RootNode, ValueName, IntToStr(AValue));
end;

procedure TXmlConfig.AddValue(const RootNode: TDOMNode; const ValueName: DOMString; const AValue: Double);
begin
  AddValue(RootNode, ValueName, FloatToStr(AValue));
end;

procedure TXmlConfig.SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: UTF8String);
var
  Node: TDOMNode;
  NodePath, AttrName: DOMString;
begin
  SplitPathToNodeAndAttr(Path, NodePath, AttrName);
  if NodePath <> EmptyWideStr then
  begin
    Node := FindNode(RootNode, NodePath, True);
    TDOMElement(Node)[AttrName] := UTF8ToUTF16(AValue);
  end
  else
    TDOMElement(RootNode)[AttrName] := UTF8ToUTF16(AValue);
end;

procedure TXmlConfig.SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Boolean);
begin
  SetAttr(RootNode, Path, BoolStrings[AValue]);
end;

procedure TXmlConfig.SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Integer);
begin
  SetAttr(RootNode, Path, IntToStr(AValue));
end;

procedure TXmlConfig.SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Int64);
begin
  SetAttr(RootNode, Path, IntToStr(AValue));
end;

procedure TXmlConfig.SetAttr(const RootNode: TDOMNode; const Path: DOMString; const AValue: Double);
begin
  SetAttr(RootNode, Path, FloatToStr(AValue));
end;

procedure TXmlConfig.SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: String);
var
  Node: TDOMNode;
begin
  Node := FindNode(RootNode, Path, True);
  Node.TextContent := UTF8ToUTF16(AValue);
end;

procedure TXmlConfig.SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Boolean);
begin
  SetValue(RootNode, Path, BoolStrings[AValue]);
end;

procedure TXmlConfig.SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Integer);
begin
  SetValue(RootNode, Path, IntToStr(AValue));
end;

procedure TXmlConfig.SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Int64);
begin
  SetValue(RootNode, Path, IntToStr(AValue));
end;

procedure TXmlConfig.SetValue(const RootNode: TDOMNode; const Path: DOMString; const AValue: Double);
begin
  SetValue(RootNode, Path, FloatToStr(AValue));
end;

// ----------------------------------------------------------------------------

procedure TXmlConfig.ReadFromFile(const AFilename: UTF8String);
var
  FileStream: TStream;
  TmpDoc: TXMLDocument;
begin
  FileStream := TFileStreamEx.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    if FileStream.Size = 0 then
      raise EFileEmpty.Create('');
    ReadXMLFile(TmpDoc, FileStream, FilenameToURI(AFilename));
    if TmpDoc.DocumentElement.NodeName <> ApplicationName then
      raise EXMLReadError.Create('Root element is not <' + ApplicationName + '>.');
    FDoc.Free;
    FDoc := TmpDoc;
  finally
    FileStream.Free;
  end;
end;

procedure TXmlConfig.ReadFromStream(AStream: TStream);
var
  TmpDoc: TXMLDocument;
begin
  if AStream.Size = 0 then
    raise EFileEmpty.Create('');
  ReadXMLFile(TmpDoc, AStream);
  FDoc.Free;
  FDoc := TmpDoc;
end;

procedure TXmlConfig.WriteToFile(const AFilename: UTF8String);
var
  FileStream: TStream;
begin
  FileStream := TFileStreamEx.Create(AFilename, fmCreate or fmShareDenyWrite);
  try
    WriteXMLFile(FDoc, FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TXmlConfig.WriteToStream(AStream: TStream);
begin
  WriteXMLFile(FDoc, AStream);
end;

function TXmlConfig.Load: Boolean;
begin
  Result := False;

  if FFileName = '' then
    Exit;

  if not mbFileExists(FileName) then
    raise EFileNotFound.Create('');
  if not mbFileAccess(FileName, fmOpenRead) then
    raise EFOpenError.Create(SysErrorMessage(GetLastOSError));

  ReadFromFile(FileName);
  Result := True;
end;

function TXmlConfig.LoadBypassingErrors: Boolean;
var
  ErrMsg: String;
begin
  try
    Result := Load;
  except
    on e: Exception do
    begin
      ErrMsg := 'Error loading configuration file ' + FileName;
      if e.Message <> EmptyStr then
        ErrMsg := ErrMsg + ': ' + e.Message;
      DebugLogger.DebugLn(ErrMsg);
      Result := False;
    end;
  end;
end;

function TXmlConfig.Save: Boolean;
var
  sTmpConfigFileName: String;
begin
  Result := False;

  if FFileName = '' then
    Exit;

  // Write to temporary file and if successfully written rename to proper name.
  if (not mbFileExists(FileName)) or mbFileAccess(FileName, fmOpenWrite) then
  begin
    sTmpConfigFileName := GetTempName(FileName);
    try
      WriteToFile(sTmpConfigFileName);
      if not mbRenameFile(sTmpConfigFileName, FileName) then
      begin
        mbDeleteFile(sTmpConfigFileName);
        DebugLogger.Debugln('Cannot save configuration file ', FileName);
      end
      else
        Result := True;
    except
      on e: EStreamError do
      begin
        mbDeleteFile(sTmpConfigFileName);
        DebugLogger.Debugln('Error saving configuration file ', FileName, ': ' + e.Message);
      end;
    end;
  end
  else
  begin
    DebugLogger.Debugln('Cannot save configuration file ', FileName, ' - check permissions');
  end;
end;

procedure TXmlConfig.SplitPathToNodeAndAttr(const Path: DOMString; out NodePath: DOMString; out AttrName: DOMString);
var
  AttrSepPos: Integer;
begin
  // Last part of the path is the attr name.
  AttrSepPos := Length(Path);
  while (AttrSepPos > 0) and (Path[AttrSepPos] <> '/') do
    Dec(AttrSepPos);

  if (AttrSepPos = 0) or (AttrSepPos = Length(Path)) then
  begin
    NodePath := EmptyWideStr;
    AttrName := Path;
  end
  else
  begin
    NodePath := Copy(Path, 1, AttrSepPos - 1);
    AttrName := Copy(Path, AttrSepPos + 1, Length(Path) - AttrSepPos);
  end;
end;

function TXmlConfig.AddNode(const RootNode: TDOMNode; const ValueName: DOMString): TDOMNode;
begin
  Result := RootNode.AppendChild(FDoc.CreateElement(ValueName));
end;

procedure TXmlConfig.DeleteNode(const RootNode: TDOMNode; const Path: DOMString);
begin
  DeleteNode(FindNode(RootNode, Path, False));
end;

procedure TXmlConfig.DeleteNode(const Node: TDOMNode);
begin
  if Assigned(Node) and Assigned(Node.ParentNode) then
    Node.ParentNode.DetachChild(Node);
end;

procedure TXmlConfig.ClearNode(const Node: TDOMNode);
var
  Attr: TDOMAttr;
begin
  while Assigned(Node.FirstChild) do
    Node.RemoveChild(Node.FirstChild);

  if Node.HasAttributes then
  begin
    Attr := TDOMAttr(Node.Attributes[0]);
    while Assigned(Attr) do
    begin
      TDOMElement(Node).RemoveAttributeNode(Attr);
      Attr := TDOMAttr(Attr.NextSibling);
    end;
  end;
end;

function TXmlConfig.FindNode(const RootNode: TDOMNode; const Path: DOMString; bCreate: Boolean = False): TDOMNode;
var
  StartPos, EndPos: Integer;
  PathLen: Integer;
  Child: TDOMNode;

  function CompareDOMStrings(const s1, s2: DOMPChar; l1, l2: integer): integer;
  var i: integer;
  begin
    Result:=l1-l2;
    i:=0;
    while (i<l1) and (Result=0) do begin
      Result:=ord(s1[i])-ord(s2[i]);
      inc(i);
    end;
  end;

begin
  Result := RootNode;

  PathLen := Length(Path);
  if PathLen = 0 then
    Exit;
  StartPos := 1;

  while Assigned(Result) do
  begin
    EndPos := StartPos;
    while (EndPos <= PathLen) and (Path[EndPos] <> '/') do
      Inc(EndPos);

    Child := Result.FirstChild;
    while Assigned(Child) and not ((Child.NodeType = ELEMENT_NODE)
      and (0 = CompareDOMStrings(DOMPChar(Child.NodeName), @Path[StartPos],
                                 Length(Child.NodeName), EndPos-StartPos))) do
        Child := Child.NextSibling;

    if not Assigned(Child) and bCreate then
    begin
      Child := FDoc.CreateElementBuf(@Path[StartPos], EndPos-StartPos);
      Result.AppendChild(Child);
    end;

    Result := Child;
    StartPos := EndPos + 1;
    if StartPos > PathLen then
      Break;
  end;
end;

function TXmlConfig.GetContent(const Node: TDOMNode): UTF8String;
begin
  Result := UTF16ToUTF8(Node.TextContent);
end;

procedure TXmlConfig.SetContent(const Node: TDOMNode; const AValue: UTF8String);
begin
  Node.TextContent := UTF8ToUTF16(AValue);
end;

function TXmlConfig.GetPathFromNode(aNode: TDOMNode): String;
begin
  Result := aNode.NodeName;
  aNode := aNode.ParentNode;
  while Assigned(aNode) and (aNode <> RootNode) do
  begin
    Result := aNode.NodeName + '/' + Result;
    aNode := aNode.ParentNode;
  end;
end;

procedure TXmlConfig.GetFont(const aNode: TXmlNode; Path: TXmlPath;
                             out Name: UTF8String; out Size: Integer; out Style: Integer;
                             const DefName: UTF8String; const DefSize: Integer; const DefStyle: Integer);
begin
  if Path <> '' then
    Path := Path + '/';
  Name := GetValue(aNode, Path + 'Name', DefName);
  Size := GetValue(aNode, Path + 'Size', DefSize);
  Style := GetValue(aNode, Path + 'Style', DefStyle);
end;

procedure TXmlConfig.SetFont(const aNode: TXmlNode; Path: TXmlPath;
                             const Name: UTF8String; const Size: Integer; const Style: Integer);
begin
  if Path <> '' then
    Path := Path + '/';
  SetValue(aNode, Path + 'Name', Name);
  SetValue(aNode, Path + 'Size', Size);
  SetValue(aNode, Path + 'Style', Style);
end;

end.

