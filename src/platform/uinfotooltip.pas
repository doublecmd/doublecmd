unit uInfoToolTip;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, uXmlConfig,
  uFile, uFileSource;

type
  { THintItem }

  THintItem = class
    Name: UTF8String;
    Mask: UTF8String;
    Hint: UTF8String;
    function Clone: THintItem;
  end;

  { THintItemList }

  THintItemList = specialize TFPGObjectList<THintItem>;

  { TFileInfoToolTip }

  TFileInfoToolTip = class(TPersistent)
  protected
    FHintItemList: THintItemList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure Clear;

    function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode);

    property  HintItemList: THintItemList read FHintItemList;
  end;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

implementation

uses
  LCLProc, StrUtils, uMasks, uDebug, uGlobs, uFileProperty, uFileFunctions,
  uSearchTemplate, uFileSourceProperty
{$IF DEFINED(MSWINDOWS)}
  , uShlObjAdditional
{$ENDIF}
  ;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

  function GetDefaultToolTip(const Hint: UTF8String): UTF8String;
  begin
    if fpModificationTime in aFile.SupportedProperties then
      with (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty) do
      Result:= IfThen(Hint = EmptyStr, EmptyStr, Hint + LineEnding) + GetDescription + #58#32 +  AsString;
    if fpSize in aFile.SupportedProperties then
      with (aFile.Properties[fpSize] as TFileSizeProperty) do
      Result:= IfThen(Hint = EmptyStr, EmptyStr, Hint + LineEnding) + GetDescription + #58#32 + AsString;
    if fpCompressedSize in aFile.SupportedProperties then
      with (aFile.Properties[fpCompressedSize] as TFileCompressedSizeProperty) do
      Result:= IfThen(Hint = EmptyStr, EmptyStr, Hint + LineEnding) + GetDescription + #58#32 + AsString;
  end;

begin
  Result:= EmptyStr;

  if fspDirectAccess in aFileSource.Properties then
    begin
      Result:= StringReplace(gFileInfoToolTip.GetFileInfoToolTip(aFileSource, aFile), '\n', LineEnding, [rfReplaceAll]);
      {$IF DEFINED(MSWINDOWS)}
      Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + SHGetInfoTip(aFile.Path, aFile.Name)
      {$ELSE}
      Result:= GetDefaultToolTip(Result);
      {$ENDIF}
    end
  else
    begin
      Result:= GetDefaultToolTip(Result);
    end;
end;

{ THintItem }

function THintItem.Clone: THintItem;
begin
  Result:= THintItem.Create;
  Result.Name:= Name;
  Result.Mask:= Mask;
  Result.Hint:= Hint;
end;

{ TFileInfoToolTip }

constructor TFileInfoToolTip.Create;
begin
  FHintItemList:= THintItemList.Create(True);
end;

destructor TFileInfoToolTip.Destroy;
begin
  FreeThenNil(FHintItemList);
  inherited Destroy;
end;

procedure TFileInfoToolTip.Clear;
begin
  begin
    while FHintItemList.Count > 0 do
      begin
        //FHintItemList[0].Free;
        FHintItemList.Delete(0);
      end;
  end;
end;

procedure TFileInfoToolTip.Assign(Source: TPersistent);
var
  I: LongInt;
  From: TFileInfoToolTip;
begin
  Clear;
  From:= Source as TFileInfoToolTip;
  for I:= 0 to From.FHintItemList.Count - 1 do
    FHintItemList.Add(From.FHintItemList[I].Clone);
end;

function TFileInfoToolTip.GetFileInfoToolTip(aFileSource: IFileSource;
  const aFile: TFile): UTF8String;
var
  I, J: Integer;
  HintItem: THintItem;
begin
 Result:= EmptyStr;

 for I:= 0 to FHintItemList.Count - 1 do
   begin
     HintItem:= FHintItemList[I];

     // Get hint by search template
     if IsMaskSearchTemplate(HintItem.Mask) then
       for J:= 0 to gSearchTemplateList.Count - 1 do
         with gSearchTemplateList do
         begin
           if (Templates[J].TemplateName = PChar(HintItem.Mask)+1) and
              Templates[J].CheckFile(AFile) then
             begin
               Result:= FormatFileFunctions(HintItem.Hint, aFile, aFileSource);
               Exit;
             end;
         end;

     // Get hint by file mask
     if MatchesMaskList(AFile.Name, HintItem.Mask) then
       begin
         Result:= FormatFileFunctions(HintItem.Hint, aFile, aFileSource);
         Exit;
       end;
   end;
end;

procedure TFileInfoToolTip.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sMask,
  sName,
  sHint: UTF8String;
  MaskItem: THintItem;
begin
  Clear;

  ANode := ANode.FindNode('CustomFields');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('CustomField') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', sName) and
           AConfig.TryGetValue(ANode, 'Mask', sMask) and
           AConfig.TryGetValue(ANode, 'Hint', sHint) then
        begin
          MaskItem:= THintItem.Create;
          MaskItem.Name := sName;
          MaskItem.Mask := sMask;
          MaskItem.Hint := sHint;
          FHintItemList.Add(MaskItem);
        end
        else
        begin
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
        end;
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TFileInfoToolTip.Save(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I : Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, 'CustomFields', True);
  AConfig.ClearNode(ANode);

  for I:=0 to FHintItemList.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'CustomField');
      AConfig.AddValue(SubNode, 'Name', FHintItemList[I].Name);
      AConfig.AddValue(SubNode, 'Mask', FHintItemList[I].Mask);
      AConfig.AddValue(SubNode, 'Hint', FHintItemList[I].Hint);
    end;
end;

end.

