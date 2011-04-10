unit uInfoToolTip;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, uXmlConfig,
  uFile, uFileSource;

type
  { TMaskItem }

  TMaskItem = class
    Name: UTF8String;
    Mask: UTF8String;
    Hint: UTF8String;
  end;

  { TMaskItemList }

  TMaskItemList = specialize TFPGObjectList<TMaskItem>;

  { TFileInfoToolTip }

  TFileInfoToolTip = class
  protected
    FMaskItemList: TMaskItemList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

    procedure Load(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure Save(AConfig: TXmlConfig; ANode: TXmlNode);

    property  MaskItemList: TMaskItemList read FMaskItemList;
  end;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;

implementation

uses
  LCLProc, StrUtils, uMasks, uDebug, uGlobs, uFileProperty, uFileFunctions,
  uFileSourceProperty
{$IF DEFINED(MSWINDOWS)}
  , uShlObjAdditional
{$ENDIF}
  ;

function GetFileInfoToolTip(aFileSource: IFileSource; const aFile: TFile): UTF8String;
begin
  Result:= EmptyStr;

  if fspDirectAccess in aFileSource.Properties then
    begin
      Result:= StringReplace(gFileInfoToolTip.GetFileInfoToolTip(aFileSource, aFile), '\n', LineEnding, [rfReplaceAll]);
      {$IF DEFINED(MSWINDOWS)}
      Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + SHGetInfoTip(aFile.Path, aFile.Name)
      {$ENDIF}
    end
  else
    begin
      if fpModificationTime in aFile.SupportedProperties then
        with (aFile.Properties[fpModificationTime] as TFileModificationDateTimeProperty) do
        Result:= GetDescription + #58#32 +  AsString;
      if fpSize in aFile.SupportedProperties then
        with (aFile.Properties[fpSize] as TFileSizeProperty) do
        Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 + AsString;
      if fpCompressedSize in aFile.SupportedProperties then
        with (aFile.Properties[fpCompressedSize] as TFileCompressedSizeProperty) do
        Result:= IfThen(Result = EmptyStr, EmptyStr, Result + LineEnding) + GetDescription + #58#32 + AsString;
    end;
end;

{ TFileInfoToolTip }

constructor TFileInfoToolTip.Create;
begin
  FMaskItemList:= TMaskItemList.Create(True);
end;

destructor TFileInfoToolTip.Destroy;
begin
  FreeThenNil(FMaskItemList);
  inherited Destroy;
end;

procedure TFileInfoToolTip.Clear;
begin
  begin
    while FMaskItemList.Count > 0 do
      begin
        FMaskItemList[0].Free;
        FMaskItemList.Delete(0);
      end;
  end;
end;

function TFileInfoToolTip.GetFileInfoToolTip(aFileSource: IFileSource;
  const aFile: TFile): UTF8String;
var
  I, J: Integer;
  MaskItem: TMaskItem;
begin
 Result:= EmptyStr;

 for I:= 0 to FMaskItemList.Count - 1 do
   begin
     MaskItem:= FMaskItemList[I];

     // Get hint by search template
     if MaskItem.Mask[1] = '>' then
       for J:= 0 to gSearchTemplateList.Count - 1 do
         with gSearchTemplateList do
         begin
           if (Templates[J].TemplateName = PChar(MaskItem.Mask)+1) and
              Templates[J].CheckFile(AFile) then
             begin
               Result:= FormatFileFunctions(MaskItem.Hint, aFile, aFileSource);
               Exit;
             end;
         end;

     // Get hint by file mask
     if MatchesMaskList(AFile.Name, MaskItem.Mask) then
       begin
         Result:= FormatFileFunctions(MaskItem.Hint, aFile, aFileSource);
         Exit;
       end;
   end;
end;

procedure TFileInfoToolTip.Load(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sMask,
  sName,
  sHint: UTF8String;
  MaskItem: TMaskItem;
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
          MaskItem:= TMaskItem.Create;
          MaskItem.Name := sName;
          MaskItem.Mask := sMask;
          MaskItem.Hint := sHint;
          FMaskItemList.Add(MaskItem);
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

  for I:=0 to FMaskItemList.Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'CustomField');
      AConfig.AddValue(SubNode, 'Name', FMaskItemList[I].Name);
      AConfig.AddValue(SubNode, 'Mask', FMaskItemList[I].Mask);
      AConfig.AddValue(SubNode, 'Hint', FMaskItemList[I].Hint);
    end;
end;

end.

