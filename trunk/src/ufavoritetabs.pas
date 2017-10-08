{
   Double Commander
   -------------------------------------------------------------------------
   Structure/Load/Save/Working With FavoriteTab and List of them

   Copyright (C) 2016-2017  Alexander Koblov (alexx2000@mail.ru)

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

unit ufavoritetabs;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, Menus, ExtCtrls, Controls, ComCtrls,

  //DC
  DCXmlConfig;
const
  FAVORITETABS_SEPARATORSTRING: string = '···························';

  // In "uMainCommands", the procedure "DoOnClickMenuJobFavoriteTabs" is called when a menu item of "FavoriteTabs" popup menu item is clicked.
  // Associted to the "tag" properties of the menuitem, these offset are added to "tag" to help for dispatching of action.
  TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING = $10000;
  TAGOFFSET_FAVTABS_SOMETHINGELSE = $20000;

  // These must match with the order of "TTabConfigLocation" afew lines below.
  gsConfigLocationName: array[0..5] of string = ('left', 'right', 'active', 'inactive', 'both', 'none');

type
  { TTabsConfigLocation }
  // These must match with the order of "gsConfigLocationName"
  // Note: NEVER CHANGE THE ORDER OF THESE CONSTANTS SINCE SETTINGS SAVED ASSUMED THIS ORDER FOREVER.
  TTabsConfigLocation = (tclLeft, tclRight, tclActive, tclInactive, tclBoth, tclNone);
  TTabsFlagsAlreadyDestroyed = set of (tfadLeft, tfadRight);

  { TKindOfFavoriteTabsEntry }
  TKindOfFavoriteTabsEntry = (fte_NULL, fte_ACTUALFAVTABS, fte_SEPARATOR, fte_STARTMENU, fte_ENDMENU);

  { TKindFavoriteTabsMenuPopulation }
  TKindFavoriteTabsMenuPopulation = (ftmp_JUSTFAVTABS, ftmp_FAVTABSWITHCONFIG);

  { TPositionWhereToAddFavoriteTabs }
  TPositionWhereToAddFavoriteTabs = (afte_First, afte_Last, afte_Alphabetical);

  { TProcedureWhenClickMenuItem}
  TProcedureWhenClickOnMenuItem = procedure(Sender: TObject) of object;

  { TFavoriteTabs }
  TFavoriteTabs = class
  private
    FDispatcher: TKindOfFavoriteTabsEntry;
    FFavoriteTabsName: string; // Friendly name, what the user decides, see, use, breath!
    FDestinationForSavedLeftTabs: TTabsConfigLocation; // Configured restoration destination side for what was saved from left panel.
    FDestinationForSavedRightTabs: TTabsConfigLocation; // Configured restoration destination side for what was saved from right panel.
    FExistingTabsToKeep: TTabsConfigLocation; // Useful when we restore tabs, to determine if we keep or not the existing ones.
    FSaveDirHistory: boolean; // Indicate if we save the dir history or not for that setup.
    FUniqueID: TGUID; // Key info! This is the unique number identifying the FactoriteTabs.
    FGroupNumber: integer; // We won't save in the XML. Just useful run-time with the tree.
  public
    constructor Create;
    procedure CopyToFavoriteTabs(DestinationFavoriteTabs: TFavoriteTabs; bExactCopyWanted: boolean = True);
    function GuidToXMLString: string;
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
    property Dispatcher: TKindOfFavoriteTabsEntry read FDispatcher write FDispatcher;
    property FavoriteTabsName: string read FFavoriteTabsName write FFavoriteTabsName;
    property DestinationForSavedLeftTabs: TTabsConfigLocation read FDestinationForSavedLeftTabs write FDestinationForSavedLeftTabs;
    property DestinationForSavedRightTabs: TTabsConfigLocation read FDestinationForSavedRightTabs write FDestinationForSavedRightTabs;
    property ExistingTabsToKeep: TTabsConfigLocation read FExistingTabsToKeep write FExistingTabsToKeep;
    property SaveDirHistory: boolean read FSaveDirHistory write FSaveDirHistory;
    property UniqueID: TGUID read FUniqueID write FUniqueID;
    property GroupNumber: integer read FGroupNumber write FGroupNumber;
  end;

  { TFavoriteTabsList }
  TFavoriteTabsList = class(TList)
  private
    FLastFavoriteTabsLoadedUniqueId: TGUID;
    FLastImportationStringUniqueId: TStringList;
    FAssociatedMainMenuItem: TMenuItem;
    function GetFavoriteTabs(Index: integer): TFavoriteTabs;
    function GetBestIndexForAlphabeticalNewFavoriteTabs(sFavoriteTabsNameToFindAPlaceFor: string): integer;
    function GetIndexForSuchFavoriteTabsName(sSearchedFavoriteTabsName: string): integer;
    procedure AddToListAndToXmlFileHeader(paramFavoriteTabs: TFavoriteTabs; AConfig: TXmlConfig; SpecifiedIndex: integer = -1);
    function ActualDumpFavoriteTabsListInXml(AConfig: TXmlConfig): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    function Add(FavoriteTabs: TFavoriteTabs): integer;
    procedure DeleteFavoriteTabs(Index: integer);
    procedure CopyFavoriteTabsListToFavoriteTabsList(var DestinationFavoriteTabsList: TFavoriteTabsList);
    function GetIndexLastFavoriteTabsLoaded: integer;
    function GetIndexPreviousLastFavoriteTabsLoaded: integer;
    function GetIndexNextLastFavoriteTabsLoaded: integer;
    function GetIndexForSuchUniqueID(SearchedGUID: TGUID): integer;
    function GetSuggestedParamsForFavoriteTabs(sAttemptedName: string; var SuggestedFavoriteTabsName: string): boolean;
    function ComputeSignature(Seed:dword=$00000000): dword;
    procedure LoadAllListFromXml;
    function LoadTabsFromXmlEntry(paramIndexToLoad: integer): boolean;
    function SaveNewEntryFavoriteTabs(paramFavoriteTabsEntryName: string): boolean;
    function ReSaveTabsToXMLEntry(paramIndexToSave: integer): boolean;
    function SaveCurrentFavoriteTabsIfAnyPriorToChange: boolean;
    function RefreshXmlFavoriteTabsListSection: boolean;
    procedure PopulateMenuWithFavoriteTabs(mncmpMenuComponentToPopulate: TComponent; ProcedureWhenFavoriteTabItemClicked: TProcedureWhenClickOnMenuItem; KindFavoriteTabMenuPopulation: TKindFavoriteTabsMenuPopulation);
    procedure RefreshAssociatedMainMenu;
    procedure LoadTTreeView(ParamTreeView: TTreeView);
    procedure RefreshFromTTreeView(ParamTreeView: TTreeView);
    function ImportFromLegacyTabsFile(paramFilename: string; SpecifiedIndex: integer = -1): boolean;
    function ExportToLegacyTabsFile(index: integer; OutputDirectory: string): boolean;
    property FavoriteTabs[Index: integer]: TFavoriteTabs read GetFavoriteTabs;
    property LastFavoriteTabsLoadedUniqueId: TGUID read FLastFavoriteTabsLoadedUniqueId write FLastFavoriteTabsLoadedUniqueId;
    property AssociatedMainMenuItem: TMenuItem read FAssociatedMainMenuItem write FAssociatedMainMenuItem;
    property LastImportationStringUniqueId: TStringList read FLastImportationStringUniqueId write FLastImportationStringUniqueId;
  end;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  LCLProc, crc, Graphics, Forms, lazutf8, Dialogs,

  //DC
  fMain, DCFileAttributes, uDebug, uDCUtils, uLng, DCOSUtils, uGlobs, uShowMsg,
  uFilePanelSelect, DCStrUtils;

{ GetSingleXmlFavoriteTabsFilename }
function GetSingleXmlFavoriteTabsFilename: string;
begin
  Result := mbExpandFileName(IncludeTrailingPathDelimiter(EnvVarConfigPath) + 'favoritetabs.xml');
end;

{ XmlStringToGuid }
function XmlStringToGuid(sXmlString: string): TGUID;
begin
  sXmlString := '{' + copy(sXmlString, 5, (length(sXmlString) - 4)) + '}';
  Result := StringToGuid(sXmlString);
end;


{ TFavoriteTabs }

{ TFavoriteTabs.Create }
constructor TFavoriteTabs.Create;
begin
  inherited Create;
  FDispatcher := fte_NULL;
  FFavoriteTabsName := '';
  FDestinationForSavedLeftTabs := tclLeft;
  FDestinationForSavedRightTabs := tclRight;
  FExistingTabsToKeep := tclNone;
  FSaveDirHistory := False;
  FUniqueID := DCGetNewGUID;
  FGroupNumber := 0;
end;

{ TFavoriteTabs.CopyToFavoriteTabs }
procedure TFavoriteTabs.CopyToFavoriteTabs(DestinationFavoriteTabs: TFavoriteTabs; bExactCopyWanted: boolean = True);
begin
  DestinationFavoriteTabs.Dispatcher := FDispatcher;
  DestinationFavoriteTabs.FavoriteTabsName := FFavoriteTabsName;
  DestinationFavoriteTabs.DestinationForSavedLeftTabs := FDestinationForSavedLeftTabs;
  DestinationFavoriteTabs.DestinationForSavedRightTabs := FDestinationForSavedRightTabs;
  DestinationFavoriteTabs.ExistingTabsToKeep := FExistingTabsToKeep;
  DestinationFavoriteTabs.SaveDirHistory := FSaveDirHistory;
  if bExactCopyWanted then DestinationFavoriteTabs.UniqueID := FUniqueId else DestinationFavoriteTabs.UniqueID := DCGetNewGUID;
  DestinationFavoriteTabs.GroupNumber := FGroupNumber;
end;

{TFavoriteTabs.GuidToXMLString }
function TFavoriteTabs.GuidToXMLString: string;
begin
  Result := GuidToString(FUniqueID);
  Result := 'GUID' + copy(Result, 2, (length(Result) - 2));
end;

{ TFavoriteTabs.SaveToXml }
procedure TFavoriteTabs.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  AConfig.SetAttr(ANode, 'UniqueID', GuidToString(FUniqueID));

  case Dispatcher of
    fte_NULL:
    begin
      AConfig.SetAttr(ANode, 'Name', '');
    end;

    fte_ACTUALFAVTABS:
    begin
      AConfig.SetAttr(ANode, 'Name', FFavoriteTabsName);
      AConfig.SetAttr(ANode, 'DestLeft', integer(FDestinationForSavedLeftTabs));
      AConfig.SetAttr(ANode, 'DestRight', integer(FDestinationForSavedRightTabs));
      AConfig.SetAttr(ANode, 'ExistingKeep', integer(FExistingTabsToKeep));
      AConfig.SetAttr(ANode, 'SaveDirHistory', FSaveDirHistory);
    end;

    fte_SEPARATOR:
    begin
      AConfig.SetAttr(ANode, 'Name', '-');
    end;

    fte_STARTMENU:
    begin
      AConfig.SetAttr(ANode, 'Name', '-' + FFavoriteTabsName);
    end;

    fte_ENDMENU:
    begin
      AConfig.SetAttr(ANode, 'Name', '--');
    end;
  end;
end;


{ TFavoriteTabsList }

{ TFavoriteTabsList.Create }
constructor TFavoriteTabsList.Create;
begin
  inherited Create;
  FLastFavoriteTabsLoadedUniqueId := DCGetNewGUID;
  FLastImportationStringUniqueId := TStringList.Create;
  FAssociatedMainMenuItem := nil;
end;

{ TFavoriteTabsList.Destroy }
destructor TFavoriteTabsList.Destroy;
begin
  if Assigned(FLastImportationStringUniqueId) then FreeAndNil(FLastImportationStringUniqueId);
  inherited;
end;

{ TFavoriteTabsList.Clear }
procedure TFavoriteTabsList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    FavoriteTabs[i].Free;
  inherited Clear;
end;

{ TFavoriteTabsList.Add }
function TFavoriteTabsList.Add(FavoriteTabs: TFavoriteTabs): integer;
begin
  Result := inherited Add(FavoriteTabs);
end;

{ TFavoriteTabsList.DeleteFavoriteTab }
procedure TFavoriteTabsList.DeleteFavoriteTabs(Index: integer);
begin
  FavoriteTabs[Index].Free;
  Delete(Index);
end;

{ TFavoriteTabsList.GetFavoriteTabs }
function TFavoriteTabsList.GetFavoriteTabs(Index: integer): TFavoriteTabs;
begin
  Result := TFavoriteTabs(Items[Index]);
end;

{ GenericCopierProcessNode }
// Will copy a given Xml structure from a certain node to another one.
// Maybe this function should be located elsewhere but for now it's here.
// WARNING: "GenericCopierProcessNode" is recursive and may call itself!
procedure GenericCopierProcessNode(paramInputNode: TXmlNode; paramOutputConfig: TXmlConfig; paramOutputNode: TXmlNode);
var
  InnerInputNode, InnerOutputNode: TXmlNode;
  iAttribute: integer;
begin
  if paramInputNode = nil then Exit; // Stoppe si on a atteint une feuille

  if paramInputNode.NodeName <> '#text' then
  begin
    if paramOutputNode <> nil then
      InnerOutputNode := paramOutputConfig.AddNode(paramOutputNode, paramInputNode.NodeName)
    else
      InnerOutputNode := paramOutputConfig.FindNode(paramOutputConfig.RootNode, paramInputNode.NodeName, True);
  end
  else
  begin
    paramOutputConfig.SetValue(paramOutputNode, '', UTF16ToUTF8(paramInputNode.NodeValue));
  end;

  // Ajoute un nœud à l'arbre s'il existe
  if paramInputNode.HasAttributes and (paramInputNode.Attributes.Length > 0) then
    for iAttribute := 0 to pred(paramInputNode.Attributes.Length) do
      paramOutputConfig.SetAttr(InnerOutputNode, paramInputNode.Attributes[iAttribute].NodeName, UTF16ToUTF8(paramInputNode.Attributes[iAttribute].NodeValue));

  // Va au nœud enfant
  InnerInputNode := paramInputNode.ChildNodes.Item[0];

  // Traite tous les nœuds enfants
  while InnerInputNode <> nil do
  begin
    GenericCopierProcessNode(InnerInputNode, paramOutputConfig, InnerOutputNode);
    InnerInputNode := InnerInputNode.NextSibling;
  end;
end;


{ TFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList }
procedure TFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(var DestinationFavoriteTabsList: TFavoriteTabsList);
var
  LocalFavoriteTabs: TFavoriteTabs;
  Index: longint;
begin
  // Let's delete possible previous list content
  for Index := pred(DestinationFavoriteTabsList.Count) downto 0 do
    DestinationFavoriteTabsList.DeleteFavoriteTabs(Index);
  DestinationFavoriteTabsList.Clear;

  // Now let's create entries and add them one by one to the destination list.
  for Index := 0 to pred(Count) do
  begin
    LocalFavoriteTabs := TFavoriteTabs.Create;
    FavoriteTabs[Index].CopyToFavoriteTabs(LocalFavoriteTabs);
    DestinationFavoriteTabsList.Add(LocalFavoriteTabs);
  end;

  // So when we go in the editor, we know which one was the latest one.
  DestinationFavoriteTabsList.LastFavoriteTabsLoadedUniqueId := FLastFavoriteTabsLoadedUniqueId;
end;

{ TFavoriteTabsList.GetIndexLastFavoriteTabsLoaded }
function TFavoriteTabsList.GetIndexLastFavoriteTabsLoaded: integer;
var
  Index: integer;
begin
  Result := -1;
  Index := 0;
  while (Index < Count) and (Result = -1) do
    if IsEqualGUID(FavoriteTabs[Index].UniqueID, FLastFavoriteTabsLoadedUniqueId) then
      Result := Index
    else
      Inc(Index);
end;

{ TFavoriteTabsList.GetIndexPreviousLastFavoriteTabsLoaded }
function TFavoriteTabsList.GetIndexPreviousLastFavoriteTabsLoaded: integer;
var
  SearchingIndex: integer;
begin
  Result := -1;

  SearchingIndex := GetIndexLastFavoriteTabsLoaded;
  if SearchingIndex > 0 then
  begin
    while (Result = -1) and (SearchingIndex > 0) do
    begin
      Dec(SearchingIndex);
      if FavoriteTabs[SearchingIndex].Dispatcher = fte_ACTUALFAVTABS then Result := SearchingIndex;
    end;
  end;
end;

{ TFavoriteTabsList.GetIndexNextLastFavoriteTabsLoaded }
function TFavoriteTabsList.GetIndexNextLastFavoriteTabsLoaded: integer;
var
  SearchingIndex: integer;
begin
  Result := -1;

  SearchingIndex := GetIndexLastFavoriteTabsLoaded;
  if SearchingIndex < pred(Count) then
  begin
    while (Result = -1) and (SearchingIndex < pred(Count)) do
    begin
      Inc(SearchingIndex);
      if FavoriteTabs[SearchingIndex].Dispatcher = fte_ACTUALFAVTABS then Result := SearchingIndex;
    end;
  end;
end;

{ TFavoriteTabsList.GetIndexForSuchUniqueID }
function TFavoriteTabsList.GetIndexForSuchUniqueID(SearchedGUID: TGUID): integer;
var
  iSearchedIndex: integer;
begin
  Result := -1;
  iSearchedIndex := 0;
  while (Result = -1) and (iSearchedIndex < Count) do
  begin
    if IsEqualGUID(SearchedGUID, FavoriteTabs[iSearchedIndex].UniqueID) then
      Result := iSearchedIndex
    else
      Inc(iSearchedIndex);
  end;
end;

{ TFavoriteTabsList.GetIndexForSuchFavoriteTabsName }
function TFavoriteTabsList.GetIndexForSuchFavoriteTabsName(sSearchedFavoriteTabsName: string): integer;
var
  iSearchedIndex: integer;
begin
  Result := -1;
  iSearchedIndex := 0;
  while (Result = -1) and (iSearchedIndex < Count) do
  begin
    if FavoriteTabs[iSearchedIndex].FavoriteTabsName = sSearchedFavoriteTabsName then
      Result := iSearchedIndex
    else
      Inc(iSearchedIndex);
  end;
end;

{ TFavoriteTabsList.GetSuggestedParamsForFavoriteTabs }
// It won't hurt anything here if user want to use more than once the SAME name...
// By adding the (1), (2), etc. between parenthesis, it's a kind of friendly indication the same name already exists.
// But we do not blocked this. If he ever decides to specifically rename with the same name. This was a kind of friendly reminder.
function TFavoriteTabsList.GetSuggestedParamsForFavoriteTabs(sAttemptedName: string; var SuggestedFavoriteTabsName: string): boolean;
var
  iIndexInCaseFound: integer;
begin
  Result := False;

  if length(sAttemptedName) > 0 then sAttemptedName := UTF8UpperCase(LeftStr(sAttemptedName, 1)) + RightStr(sAttemptedName, length(sAttemptedName) - 1);
  SuggestedFavoriteTabsName := sAttemptedName;
  iIndexInCaseFound := 1;
  while GetIndexForSuchFavoriteTabsName(SuggestedFavoriteTabsName) <> -1 do
  begin
    SuggestedFavoriteTabsName := Format('%s(%d)', [sAttemptedName, iIndexInCaseFound]);
    Inc(iIndexInCaseFound);
  end;

  if InputQuery(rsMsgFavoriteTabsEnterName, rsMsgFavoriteTabsEnterNameTitle, SuggestedFavoriteTabsName) then
    Result := (length(SuggestedFavoriteTabsName) > 0);
end;

{ TFavoriteTabsList.ComputeSignature }
// Routine tries to pickup all char chain from element of favorite tabs list to compute a unique CRC32.
// This CRC32 will be a kind of signature of the favorite tabs list.
function TFavoriteTabsList.ComputeSignature(Seed:dword): dword;
var
  Index: integer;
begin
  Result := Seed;
  for Index := 0 to pred(Count) do
  begin
    Result := crc32(Result, @FavoriteTabs[Index].Dispatcher, 1);
    if length(FavoriteTabs[Index].FavoriteTabsName) > 0 then Result := crc32(Result, @FavoriteTabs[Index].FavoriteTabsName[1], length(FavoriteTabs[Index].FavoriteTabsName));
    Result := crc32(Result, @FavoriteTabs[Index].DestinationForSavedLeftTabs, sizeof(TTabsConfigLocation));
    Result := crc32(Result, @FavoriteTabs[Index].DestinationForSavedRightTabs, sizeof(TTabsConfigLocation));
    Result := crc32(Result, @FavoriteTabs[Index].ExistingTabsToKeep, sizeof(TTabsConfigLocation));
  end;
end;

{ TFavoriteTabsList.LoadAllListFromXml}
procedure TFavoriteTabsList.LoadAllListFromXml;
var
  ANode: TXmlNode;
  AConfig: TXmlConfig;
  LocalFavoriteTabs: TFavoriteTabs;
  sName: string;
  CurrentMenuLevel: integer;
  FlagAvortInsertion: boolean;
begin
  Clear;
  CurrentMenuLevel := 0;

  // We don't add it to the list UNTIL a new entry has been added to the XML file.
  AConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);
  try
    ANode := AConfig.FindNode(AConfig.RootNode, 'FavoriteTabsList');
    if Assigned(ANode) then
    begin
      ANode := ANode.FirstChild;
      while Assigned(ANode) do
      begin
        if ANode.CompareName('FavoriteTabs') = 0 then
        begin
          if AConfig.TryGetAttr(ANode, 'Name', sName) then
          begin
            FlagAvortInsertion := False;
            LocalFavoriteTabs := TFavoriteTabs.Create;

            if sName = '-' then
            begin
              LocalFavoriteTabs.Dispatcher := fte_SEPARATOR;
            end
            else
            begin
              if sName = '--' then
              begin
                LocalFavoriteTabs.Dispatcher := fte_ENDMENU;
                if CurrentMenuLevel > 0 then
                  Dec(CurrentMenuLevel)
                else
                  FlagAvortInsertion := True; // Sanity correction in case we got corrupted from any ways
              end
              else
              begin
                if (UTF8Length(sName) > 1) then
                begin
                  if (sName[1] = '-') and (sName[2] <> '-') then
                  begin
                    Inc(CurrentMenuLevel);
                    LocalFavoriteTabs.Dispatcher := fte_STARTMENU;
                    LocalFavoriteTabs.FavoriteTabsName := UTF8RightStr(sName, UTF8Length(sName) - 1);
                  end;
                end;

                if LocalFavoriteTabs.Dispatcher = fte_NULL then
                begin
                  LocalFavoriteTabs.FavoriteTabsName := sName;
                  LocalFavoriteTabs.Dispatcher := fte_ACTUALFAVTABS;
                  LocalFavoriteTabs.DestinationForSavedLeftTabs := TTabsConfigLocation(AConfig.GetAttr(Anode, 'DestLeft', integer(tclLeft)));
                  LocalFavoriteTabs.DestinationForSavedRightTabs := TTabsConfigLocation(AConfig.GetAttr(Anode, 'DestRight', integer(tclRight)));
                  LocalFavoriteTabs.ExistingTabsToKeep := TTabsConfigLocation(AConfig.GetAttr(Anode, 'ExistingKeep', integer(tclNone)));
                  LocalFavoriteTabs.SaveDirHistory := AConfig.GetAttr(Anode, 'SaveDirHistory', False);
                  LocalFavoriteTabs.UniqueID := StringToGuid(AConfig.GetAttr(Anode, 'UniqueID', GuidToString(DCGetNewGUID)));
                end;
              end;
            end;

            if not FlagAvortInsertion then
            begin
              Add(LocalFavoriteTabs);
            end
            else
            begin
              LocalFavoriteTabs.Free;
            end;
          end;
        end;
        ANode := ANode.NextSibling;
      end;
      while CurrentMenuLevel > 0 do
      begin
        Dec(CurrentMenuLevel);
        LocalFavoriteTabs := TFavoriteTabs.Create;
        LocalFavoriteTabs.Dispatcher := fte_ENDMENU;
        Add(LocalFavoriteTabs);
      end;
    end;

  finally
    FreeAndNil(AConfig);
  end;

  RefreshAssociatedMainMenu;
end;

{ TFavoriteTabsList.LoadTabsFromXmlEntry }
function TFavoriteTabsList.LoadTabsFromXmlEntry(paramIndexToLoad: integer): boolean;
var
  AConfig: TXmlConfig;
  sActualTabSection: string;
  TestNode: TXmlNode; // Use just to validate there will be something to load from file.
  originalFilePanel: TFilePanelSelect;
  // Following variables are "pre-initialized" to default values AND are values used when not using the possibility to configure redirection of tabs in our setups.
  TargetDestinationForLeft: TTabsConfigLocation = tclLeft;
  TargetDestinationForRight: TTabsConfigLocation = tclRight;
  DestinationToKeep: TTabsConfigLocation = tclNone;
  TabsAlreadyDestroyedFlags: TTabsFlagsAlreadyDestroyed = [];
begin
  Result := False;

  if (paramIndexToLoad >= 0) and (paramIndexToLoad < Count) then
  begin
    if FavoriteTabs[paramIndexToLoad].Dispatcher = fte_ACTUALFAVTABS then
    begin
      // 1. We remember to restore later the current selected panel.
      originalFilePanel := frmMain.SelectedPanel;

      // 2. We set the section-path to our wanted setup. (Don't forget the trailing slash at the end because "LoadTheseTabsWithThisConfig" requires it for the "ABranch" parameter.
      sActualTabSection := 'ActualTabs/' + FavoriteTabs[paramIndexToLoad].GuidToXMLString + '/';

      // 3. We set the location where to restore tabs according to our setup IF we're configure for it.
      if gFavoriteTabsUseRestoreExtraOptions then
      begin
        TargetDestinationForLeft := FavoriteTabs[paramIndexToLoad].DestinationForSavedLeftTabs;
        TargetDestinationForRight := FavoriteTabs[paramIndexToLoad].DestinationForSavedRightTabs;
        DestinationToKeep := FavoriteTabs[paramIndexToLoad].ExistingTabsToKeep;
      end;

      // 4. We're ready to open the config files and actually load the stored tabs to the correct target side.
      AConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);
      if Assigned(AConfig) then
      begin
        try
          // 5.1. We load the left tabs (We check before to make sure there is a "Left" section to don't possibly destroy existing tabs and later realize there is no section to load (But it should not happen...)).
          TestNode := AConfig.FindNode(AConfig.RootNode, sActualTabSection + 'Left/Tab');
          if Assigned(TestNode) then
            frmMain.LoadTheseTabsWithThisConfig(AConfig, sActualTabSection, tclLeft, TargetDestinationForLeft, DestinationToKeep, TabsAlreadyDestroyedFlags);

          // 5.2. We load the right tabs (We check before to make sure there is a "Right" section to don't possibly destroy existing tabs and later realize there is no section to load (But it should not happen...)).
          TestNode := AConfig.FindNode(AConfig.RootNode, sActualTabSection + 'Right/Tab');
          if Assigned(TestNode) then
            frmMain.LoadTheseTabsWithThisConfig(AConfig, sActualTabSection, tclRight, TargetDestinationForRight, DestinationToKeep, TabsAlreadyDestroyedFlags);

          // 6. We've loaded a setup, let's remember it.
          FLastFavoriteTabsLoadedUniqueId := FavoriteTabs[paramIndexToLoad].FUniqueID;

          // 7. We need to refresh main menu so the check will be on the right setup. The "Reload" and "Resave" will be enabled also.
          RefreshAssociatedMainMenu;

          // 8. If we reach that point, we deserve to return True!
          Result := True;
        finally
          FreeAndNil(AConfig);
        end;
      end;

      // 9. We restore focuse to what was our active panel, ready to play in it.
      frmMain.SelectedPanel := originalFilePanel;
      frmMain.ActiveFrame.SetFocus;
    end;
  end;
end;

{ TFavoriteTabsList.GetBestIndexForAlphabeticalNewFavoriteTabs }
// We take the whole list of "FavoriteTabsName" seen as one single alphabetical sorted list.
// We find the exact place where our string to add could fit.
// We insert it right in front of the one its is closed to go.
// If we have the last one, we placed it last!
function TFavoriteTabsList.GetBestIndexForAlphabeticalNewFavoriteTabs(sFavoriteTabsNameToFindAPlaceFor: string): integer;
var
  I, iPosInserted: integer;
  localFavoriteTabsNameToFindAPlaceFor: string;
  MagickSortedList: TStringList;
begin
  Result := -1;
  localFavoriteTabsNameToFindAPlaceFor := UTF8LowerCase(sFavoriteTabsNameToFindAPlaceFor);

  MagickSortedList := TStringList.Create;
  try
    MagickSortedList.Sorted := True;
    MagickSortedList.Duplicates := dupAccept;

    // 1. We add in the list only the actual FavoriteTabsName.
    for I := 0 to pred(Count) do
      if FavoriteTabs[I].Dispatcher = fte_ACTUALFAVTABS then
        MagickSortedList.Add(UTF8LowerCase(FavoriteTabs[I].FavoriteTabsName) + DirectorySeparator + IntToStr(I));

    // 2. Add to list our string.
    iPosInserted := MagickSortedList.Add(localFavoriteTabsNameToFindAPlaceFor);

    // 3. We now know the best place to insert our entry (unless it's last one).
    if MagickSortedList.Count > 1 then
    begin
      if iPosInserted < pred(MagickSortedList.Count) then
      begin
        Result := StrToInt(GetLastDir(MagickSortedList.Strings[iPosInserted + 1]));
      end
      else
      begin
        Result := (StrToInt(GetLastDir(MagickSortedList.Strings[iPosInserted - 1]))) + 1;
        if Result >= Count then Result := -1;
      end;
    end;

  finally
    MagickSortedList.Free;
  end;
end;

{ TFavoriteTabsList.AddToListAndToXmlFileHeader }
procedure TFavoriteTabsList.AddToListAndToXmlFileHeader(paramFavoriteTabs: TFavoriteTabs; AConfig: TXmlConfig; SpecifiedIndex: integer = -1);
var
  SubNode, RootNode: TXmlNode;
  iIndexToInsert: integer;
begin
  if SpecifiedIndex = -1 then
  begin
    case gWhereToAddNewFavoriteTabs of
      afte_Last:
      begin
        // The simplest case: We add the node, we add it at the end of the list, that's it!
        RootNode := AConfig.FindNode(AConfig.RootNode, 'FavoriteTabsList', True);
        SubNode := AConfig.AddNode(RootNode, 'FavoriteTabs');
        paramFavoriteTabs.SaveToXml(AConfig, SubNode);
        Add(paramFavoriteTabs);
      end;

      afte_First:
      begin
        // A bit more complicated: we will "insert" at position 0 our Favorites Tabs in our list and then we'll recreate the index at the beginning of our Xml file.
        Insert(0, paramFavoriteTabs);
        ActualDumpFavoriteTabsListInXml(AConfig);
      end;

      afte_Alphabetical:
      begin
        //A medium bit more complicated: we will
        iIndexToInsert := GetBestIndexForAlphabeticalNewFavoriteTabs(paramFavoriteTabs.FavoriteTabsName);
        if (iIndexToInsert >= 0) and (Count > 0) then
          Insert(iIndexToInsert, paramFavoriteTabs)
        else
          Add(paramFavoriteTabs);
        ActualDumpFavoriteTabsListInXml(AConfig);
      end;
    end;
  end
  else
  begin
    if SpecifiedIndex < pred(Count) then
      Insert(SpecifiedIndex, paramFavoriteTabs)
    else
      Add(paramFavoriteTabs);
    ActualDumpFavoriteTabsListInXml(AConfig);
  end;
end;

{ TFavoriteTabsList.SaveNewEntryFavoriteTabs }
function TFavoriteTabsList.SaveNewEntryFavoriteTabs(paramFavoriteTabsEntryName: string): boolean;
var
  AConfig: TXmlConfig;
  sActualTabSection: string;
  LocalFavoriteTabs: TFavoriteTabs;
  bFlagToSaveHistoryOrNot: boolean;
begin
  try
    LocalFavoriteTabs := TFavoriteTabs.Create;
    LocalFavoriteTabs.Dispatcher := fte_ACTUALFAVTABS;
    LocalFavoriteTabs.FavoriteTabsName := paramFavoriteTabsEntryName;
    LocalFavoriteTabs.DestinationForSavedLeftTabs := gDefaultTargetPanelLeftSaved;
    LocalFavoriteTabs.DestinationForSavedRightTabs := gDefaultTargetPanelRightSaved;
    LocalFavoriteTabs.ExistingTabsToKeep := gDefaultExistingTabsToKeep;
    LocalFavoriteTabs.SaveDirHistory := gFavoriteTabsSaveDirHistory;
    //The UniqueID is not assigned here because it has already been set when "LocalFavoriteTabs" has been created.

    if gFavoriteTabsUseRestoreExtraOptions then
      bFlagToSaveHistoryOrNot := LocalFavoriteTabs.SaveDirHistory
    else
      bFlagToSaveHistoryOrNot := gFavoriteTabsSaveDirHistory;

    AConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);
    try
      AddToListAndToXmlFileHeader(LocalFavoriteTabs, AConfig);

      sActualTabSection := 'ActualTabs/' + LocalFavoriteTabs.GuidToXMLString;
      frmMain.SaveTabsXml(AConfig, sActualTabSection, frmMain.LeftTabs, bFlagToSaveHistoryOrNot);
      frmMain.SaveTabsXml(AConfig, sActualTabSection, frmMain.RightTabs, bFlagToSaveHistoryOrNot);

      AConfig.Save;

      FLastFavoriteTabsLoadedUniqueId := LocalFavoriteTabs.UniqueID;
      RefreshAssociatedMainMenu; // To possibly enable the action for the "ReLoad" and "ReSave".
    finally
      FreeAndNil(AConfig);
    end;

    Result := True; // If we get here, we'll assumed Favorite Tabs has been saved!
  except
    Result := False;
  end;
end;

{ TFavoriteTabsList.ReSaveTabsToXMLEntry }
// When "ReSaving", we must first find in the Xml the previous entry to write over it.
function TFavoriteTabsList.ReSaveTabsToXMLEntry(paramIndexToSave: integer): boolean;
var
  ANode: TXmlNode;
  AConfig: TXmlConfig;
  sActualTabSection, sGUIDTemp: string;
  bFlagSaved: boolean = False;
  bFlagToSaveHistoryOrNot: boolean;
begin
  Result := False;
  if (paramIndexToSave >= 0) and (paramIndexToSave < Count) then
  begin
    if FavoriteTabs[paramIndexToSave].Dispatcher = fte_ACTUALFAVTABS then
    begin
      AConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);
      try
        ANode := AConfig.FindNode(AConfig.RootNode, 'FavoriteTabsList');
        if Assigned(ANode) then
        begin
          ANode := ANode.FirstChild;
          while Assigned(ANode) and (not bFlagSaved) do
          begin
            if ANode.CompareName('FavoriteTabs') = 0 then
            begin
              if AConfig.TryGetAttr(ANode, 'UniqueID', sGUIDTemp) then
              begin
                if IsEqualGUID(FavoriteTabs[paramIndexToSave].UniqueID, StringToGuid(sGUIDTemp)) then
                begin
                  FavoriteTabs[paramIndexToSave].SaveToXml(AConfig, ANode);
                  bFlagSaved := True;
                  FLastFavoriteTabsLoadedUniqueId := FavoriteTabs[paramIndexToSave].UniqueID;
                end;
              end;
            end;
            ANode := ANode.NextSibling;
          end;
        end;

        if bFlagSaved then
        begin
          if gFavoriteTabsUseRestoreExtraOptions then
            bFlagToSaveHistoryOrNot := FavoriteTabs[paramIndexToSave].SaveDirHistory
          else
            bFlagToSaveHistoryOrNot := gFavoriteTabsSaveDirHistory;

          sActualTabSection := 'ActualTabs/' + FavoriteTabs[paramIndexToSave].GuidToXMLString;
          frmMain.SaveTabsXml(AConfig, sActualTabSection, frmMain.LeftTabs, bFlagToSaveHistoryOrNot);
          frmMain.SaveTabsXml(AConfig, sActualTabSection, frmMain.RightTabs, bFlagToSaveHistoryOrNot);
          AConfig.Save;

          Result := True;
        end;
      finally
        FreeAndNil(AConfig);
      end;
    end;
  end;
end;

{ TFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange }
function TFavoriteTabsList.SaveCurrentFavoriteTabsIfAnyPriorToChange: boolean;
var
  iIndex: integer;
  bFlagToSaveHistoryOrNot: boolean;
begin
  Result := True;
  iIndex := GetIndexLastFavoriteTabsLoaded;
  if iIndex <> -1 then
  begin
    if gFavoriteTabsUseRestoreExtraOptions then
      bFlagToSaveHistoryOrNot := FavoriteTabs[iIndex].SaveDirHistory
    else
      bFlagToSaveHistoryOrNot := gFavoriteTabsSaveDirHistory;

    if bFlagToSaveHistoryOrNot then
      Result := ReSaveTabsToXMLEntry(iIndex);
  end;
end;

{ TFavoriteTabsList.ActualDumpFavoriteTabsListInXml }
// Since we save everything, we will will flush the current "FavoriteTabsList" header section to restore it.
// We need to do in case we would delete entries.
// Also, certainly we could have done something like "AConfig.DeleteNode(ListNode)" and then rewrite completely our table after.
// BUT, doing that would place our list at the end. That's fine and functional. But it's nice for human to see it on top when debugging.
// So that's why we delete item one by one without deleting the initial node.
// Also, we need to do a kind of purge of the subsequent node regarding actual tab setup.
// To do that, we check each node to see if we have a correspondant entry in the header.
// If not, we flush that node!
function TFavoriteTabsList.ActualDumpFavoriteTabsListInXml(AConfig: TXmlConfig): boolean;
var
  SubNode, ListNode, ANode, BNode: TXmlNode;
  iIndex: integer;
begin
  ListNode := AConfig.FindNode(AConfig.RootNode, 'FavoriteTabsList', True);
  if Assigned(ListNode) then
  begin
    ANode := ListNode.FirstChild;
    while Assigned(ANode) do
    begin
      BNode := ANode.NextSibling;
      AConfig.DeleteNode(ANode);
      ANode := BNode;
    end;
  end;

  // 1. Write the kind of "Table" of Favorite Tabs
  iIndex := 0;
  while iIndex < Count do
  begin
    case FavoriteTabs[iIndex].Dispatcher of
      fte_ACTUALFAVTABS, fte_SEPARATOR, fte_STARTMENU, fte_ENDMENU:
      begin
        SubNode := AConfig.AddNode(ListNode, 'FavoriteTabs');
        FavoriteTabs[iIndex].SaveToXml(AConfig, SubNode);
      end;
    end;
    Inc(iIndex);
  end;

  // 2. Validate if bare data for actual tabs have a matching entry in our table.
  //    If not, flush it.
  ListNode := AConfig.FindNode(AConfig.RootNode, 'ActualTabs');
  if Assigned(ListNode) then
  begin
    ANode := ListNode.FirstChild;
    while Assigned(ANode) do
    begin
      BNode := ANode.NextSibling;
      if GetIndexForSuchUniqueID(XmlStringToGuid(ANode.NodeName)) = -1 then
        AConfig.DeleteNode(ANode);
      ANode := BNode;
    end;
  end;
end;

{ TFavoriteTabsList.RefreshXmlFavoriteTabsListSection }
function TFavoriteTabsList.RefreshXmlFavoriteTabsListSection: boolean;
var
  AConfig: TXmlConfig;
begin
  Result := False;
  try
    AConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);
    try
      ActualDumpFavoriteTabsListInXml(AConfig);
      AConfig.Save;
    finally
      FreeAndNil(AConfig);
    end;
    Result := True;
  except
    on E: Exception do
      msgError(E.Message);
  end;
end;

{ TFavoriteTabsList.PopulateMenuWithFavoriteTabs }
procedure TFavoriteTabsList.PopulateMenuWithFavoriteTabs(mncmpMenuComponentToPopulate: TComponent; ProcedureWhenFavoriteTabItemClicked: TProcedureWhenClickOnMenuItem; KindFavoriteTabMenuPopulation: TKindFavoriteTabsMenuPopulation);
var
  I: longint; //Same variable for main and local routine

  // WARNING: "CompleteMenu" is recursive and may call itself!
  function CompleteMenu(ParamMenuItem: TComponent; TagOffset: integer = 0): longint;

    // WARNING: "DoCheckedBackToTop" is recursive and may call itself!
    procedure DoCheckedBackToTop(paramTMenuItem: TComponent);
    begin
      if (mncmpMenuComponentToPopulate = paramTMenuItem) then Exit;
      if (paramTMenuItem.ClassType = TMenuItem) and (TMenuItem(paramTMenuItem).Caption <> rsMsgFavortieTabsSaveOverExisting) then
        if TMenuItem(paramTMenuItem).Parent <> nil then
          if (TMenuItem(paramTMenuItem).Parent.ClassType <> TMainMenu) then
          begin
            TMenuItem(paramTMenuItem).Checked := True;
            DoCheckedBackToTop(TMenuItem(paramTMenuItem).Parent);
          end;
    end;

  var
    localmi: TMenuItem;
    LocalLastAdditionIsASeparator: boolean;
  begin
    Result := 0;
    LocalLastAdditionIsASeparator := False;
    while I < Count do
    begin
      Inc(I);

      case FavoriteTabs[I - 1].Dispatcher of
        fte_ACTUALFAVTABS:
        begin
          localmi := TMenuItem.Create(ParamMenuItem);
          localmi.Caption := FavoriteTabs[I - 1].FFavoriteTabsName;
          localmi.tag := (I - 1) + TagOffset;
          localmi.OnClick := ProcedureWhenFavoriteTabItemClicked;
          localmi.Checked := IsEqualGUID(FavoriteTabs[I - 1].UniqueID, FLastFavoriteTabsLoadedUniqueId);
          if ParamMenuItem.ClassType = TPopupMenu then
            TPopupMenu(ParamMenuItem).Items.Add(localmi)
          else if ParamMenuItem.ClassType = TMenuItem then
          begin
            TMenuItem(ParamMenuItem).Add(localmi);
            if localmi.Checked then DoCheckedBackToTop(localmi);
          end;

          LocalLastAdditionIsASeparator := False;
          Inc(Result);
        end;

        fte_SEPARATOR:
        begin
          if (not LocalLastAdditionIsASeparator) then
          begin
            localmi := TMenuItem.Create(ParamMenuItem);
            localmi.Caption := '-';
            if ParamMenuItem.ClassType = TPopupMenu then
              TPopupMenu(ParamMenuItem).Items.Add(localmi)
            else if ParamMenuItem.ClassType = TMenuItem then
              TMenuItem(ParamMenuItem).Add(localmi);
            LocalLastAdditionIsASeparator := True;
            Inc(Result);
          end;
        end;

        fte_STARTMENU:
        begin
          localmi := TMenuItem.Create(ParamMenuItem);
          localmi.Caption := FavoriteTabs[I - 1].FavoriteTabsName;
          //if gIconsInMenus then
          //  localmi.ImageIndex := frmMain.miLoadFavoriteTabs.ImageIndex;
          if ParamMenuItem.ClassType = TPopupMenu then
            TPopupMenu(ParamMenuItem).Items.Add(localmi)
          else if ParamMenuItem.ClassType = TMenuItem then
            TMenuItem(ParamMenuItem).Add(localmi);
          CompleteMenu(localmi, TagOffset);
          if localmi.Count <> 0 then
          begin
            LocalLastAdditionIsASeparator := False;
            Inc(Result);
          end
          else
          begin
            localmi.Free;
          end;
        end;

        fte_ENDMENU:
        begin
          if LocalLastAdditionIsASeparator then
          begin
            if ParamMenuItem.ClassType = TPopupMenu then
              TPopupMenu(ParamMenuItem).Items[pred(TPopupMenu(ParamMenuItem).Items.Count)].Free
            else if ParamMenuItem.ClassType = TMenuItem then
              TMenuItem(ParamMenuItem).Items[pred(TMenuItem(ParamMenuItem).Count)].Free;
            Dec(Result);
          end;
          exit;
        end;
      end; //case FavoriteTabs[I-1].Dispatcher of
    end; //while I<Count do
  end;

var
  miMainTree: TMenuItem;
begin
  // 1. Let's clear possible previous items in the menu to create a fresh new one.
  if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
    TPopupMenu(mncmpMenuComponentToPopulate).Items.Clear;

  // 2. Add the Favorite Tabs shortcuts if we have any.
  if Count > 0 then
  begin
    I := 0;
    CompleteMenu(mncmpMenuComponentToPopulate, 0);
  end;

  // 3. Customize minimally our menu. If we wants some configuration and saving, let's add them.
  case KindFavoriteTabMenuPopulation of
    ftmp_FAVTABSWITHCONFIG:
    begin
      // 3.1. Add the reload/resave items.
      if (Count > 0) and (GetIndexLastFavoriteTabsLoaded <> -1) then
      begin
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := ('-');
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Action := frmMain.actReloadFavoriteTabs;
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Action := frmMain.actResaveFavoriteTabs;
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
      end;

      // 3.2. Add a delimiter, a simple line to separate.
      if Count > 0 then
      begin
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := '-';
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
      end;

      // 3.3 Now add "Add current tabs to Favorite Tabs".
      miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
      miMainTree.Action := frmMain.actSaveFavoriteTabs;
      if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
      else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

      // 3.4. If we have at least one entry, let's create the "Save Over Existing.." items.
      if Count > 0 then
      begin
        // 3.4.1. Add the "Save current tabs over existing Favorite Tabs entry" in a submenu.
        //        It's placed in a sub menu since when it's time to saved since it's less frequent then keeping accessing...
        //        ...and to avoid to click on it by accident and overwring existing stuff.
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := rsMsgFavortieTabsSaveOverExisting;
        miMainTree.ImageIndex := frmMain.actResaveFavoriteTabs.ImageIndex;
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        // 3.4.1. And then add our favorite tabs again BUT with the "TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING" offset in the tag.
        I := 0;
        CompleteMenu(miMainTree, TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING);

        // 3.4.2. Then another separator.
        //       Intentionnally there is no separator when user has no favorite tabs and there is one when there is at least one...
        //       It seems stupid to have a single separator when there is only two items...
        //       ... and when we have many items, it looks good to have the "Save's" enclose between separators.
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := '-';
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
      end;

      // 3.5 Now add "Configure Folder tab".
      miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
      miMainTree.Action := frmMain.actConfigFolderTabs;
      if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
      else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then  TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

      // 3.6 Now add "Configure Favorite Tabs".
      miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
      miMainTree.Action := frmMain.actConfigFavoriteTabs;
      if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
      else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then  TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
    end;
  end; //case KindFavoriteTabMenuPopulation of

  if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
    if TPopupMenu(mncmpMenuComponentToPopulate).Images = nil then
      TPopupMenu(mncmpMenuComponentToPopulate).Images := frmMain.imgLstActions;

  if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
    if TMenuItem(mncmpMenuComponentToPopulate).GetParentMenu.Images = nil then
      TMenuItem(mncmpMenuComponentToPopulate).GetParentMenu.Images := frmMain.imgLstActions;
end;

{ TFavoriteTabsList.RefreshAssociatedMainMenu }
procedure TFavoriteTabsList.RefreshAssociatedMainMenu;
var
  iIndex: integer;
  miMainTree: TMenuItem;
begin
  if FAssociatedMainMenuItem <> nil then
  begin
    if FAssociatedMainMenuItem <> nil then
    begin
      if FAssociatedMainMenuItem.Count > 4 then
      begin
        iIndex := pred(FAssociatedMainMenuItem.Count);
        while iIndex > 3 do
        begin
          FAssociatedMainMenuItem.Delete(iIndex);
          Dec(iIndex);
        end;
      end;

      if Count > 0 then
      begin
        miMainTree := TMenuItem.Create(FAssociatedMainMenuItem);
        miMainTree.Caption := '-';
        FAssociatedMainMenuItem.Add(miMainTree);
      end;

      if GetIndexLastFavoriteTabsLoaded = -1 then
      begin
        frmMain.actReloadFavoriteTabs.Enabled := False;
        frmMain.actResaveFavoriteTabs.Enabled := False;
      end
      else
      begin
        frmMain.actReloadFavoriteTabs.Enabled := True;
        frmMain.actResaveFavoriteTabs.Enabled := True;
      end;
    end;

    PopulateMenuWithFavoriteTabs(FAssociatedMainMenuItem, @frmMain.Commands.DoOnClickMenuJobFavoriteTabs, ftmp_JUSTFAVTABS);
  end;
end;

{ TFavoriteTabsList.LoadTTreeView }
// We'll try to restore what was selected prior the reload for the situation where it's pertinent.
procedure TFavoriteTabsList.LoadTTreeView(ParamTreeView: TTreeView);
var
  Index: longint;

  procedure RecursivAddElements(WorkingNode: TTreeNode);
  var
    FlagGetOut: boolean = False;
    LocalNode: TTreeNode;
  begin
    while (FlagGetOut = False) and (Index < Count) do
    begin
      case FavoriteTabs[Index].Dispatcher of
        fte_STARTMENU:
        begin
          LocalNode := ParamTreeView.Items.AddChildObject(WorkingNode, FavoriteTabs[Index].FavoriteTabsName, FavoriteTabs[Index]);
          LocalNode.Data := FavoriteTabs[Index];
          Inc(Index);
          RecursivAddElements(LocalNode);
        end;

        fte_ENDMENU:
        begin
          FlagGetOut := True;
          Inc(Index);
        end;

        fte_SEPARATOR:
        begin
          LocalNode := ParamTreeView.Items.AddChildObject(WorkingNode, FAVORITETABS_SEPARATORSTRING, FavoriteTabs[Index]);
          LocalNode.Data := FavoriteTabs[Index];
          Inc(Index);
        end

        else // ...but should not happened.
        begin
          LocalNode := ParamTreeView.Items.AddChildObject(WorkingNode, FavoriteTabs[Index].FavoriteTabsName, FavoriteTabs[Index]);
          Inc(Index);
        end;
      end;
    end;
  end;

begin
  ParamTreeView.Items.Clear;
  Index := 0;
  RecursivAddElements(nil);
end;

{ TFavoriteTabsList.RefreshFromTTreeView }
// The routine will recreate the complete TFavoriteTabsList from a TTreeView.
// It cannot erase or replace immediately the current list because the TTreeView refer to it!
// So it create it into the "TransitFavoriteTabsList" and then, it will copy it to self one.
// It will remember what was selected before based on the unique ID and then restore what is possible to restored after.
procedure TFavoriteTabsList.RefreshFromTTreeView(ParamTreeView: TTreeView);
var
  TransitFavoriteTabsList: TFavoriteTabsList;
  iIndex: integer;
  slRememberCurrentSelections: TStringList;

  procedure RecursiveEncapsulateSubMenu(WorkingTreeNode: TTreeNode);
  var
    MaybeChildNode: TTreeNode;
    WorkingFavoriteTabEntry: TFavoriteTabs;
  begin
    while WorkingTreeNode <> nil do
    begin
      MaybeChildNode := WorkingTreeNode.GetFirstChild;
      if MaybeChildNode <> nil then
      begin
        WorkingFavoriteTabEntry := TFavoriteTabs.Create;
        TFavoriteTabs(WorkingTreeNode.Data).CopyToFavoriteTabs(WorkingFavoriteTabEntry);
        WorkingFavoriteTabEntry.Dispatcher := fte_STARTMENU; //Probably not necessary, but let's make sure it will start a menu
        TransitFavoriteTabsList.Add(WorkingFavoriteTabEntry);

        RecursiveEncapsulateSubMenu(MaybeChildNode);

        WorkingFavoriteTabEntry := TFavoriteTabs.Create;
        WorkingFavoriteTabEntry.Dispatcher := fte_ENDMENU;
        TransitFavoriteTabsList.Add(WorkingFavoriteTabEntry);
      end
      else
      begin
        //We won't copy EMPTY submenu so that's why we check for "fte_STARTMENU". And the check for "fte_ENDMENU" is simply probably unecessary protection
        if (TFavoriteTabs(WorkingTreeNode.Data).Dispatcher <> fte_STARTMENU) and (TFavoriteTabs(WorkingTreeNode.Data).Dispatcher <> fte_ENDMENU) then
        begin
          WorkingFavoriteTabEntry := TFavoriteTabs.Create;
          TFavoriteTabs(WorkingTreeNode.Data).CopyToFavoriteTabs(WorkingFavoriteTabEntry);
          TransitFavoriteTabsList.Add(WorkingFavoriteTabEntry);
        end;
      end;
      WorkingTreeNode := WorkingTreeNode.GetNextSibling;
    end;
  end;

begin
  if ParamTreeView.Items.Count > 0 then
  begin
    slRememberCurrentSelections := TStringList.Create;
    TransitFavoriteTabsList := TFavoriteTabsList.Create;
    try
      //Saving a trace of what is selected right now.
      for iIndex := 0 to pred(ParamTreeView.Items.Count) do
        if ParamTreeView.Items[iIndex].Selected then
          if TFavoriteTabs(ParamTreeView.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
            slRememberCurrentSelections.Add(GUIDtoString(TFavoriteTabs(ParamTreeView.Items[iIndex].Data).UniqueID));

      TransitFavoriteTabsList.LastFavoriteTabsLoadedUniqueId := FLastFavoriteTabsLoadedUniqueId;
      RecursiveEncapsulateSubMenu(ParamTreeView.Items.Item[0]);
      TransitFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(self);
      LoadTTreeView(ParamTreeView);

      // Restoring what was selected.
      ParamTreeView.ClearSelection(False);
      for iIndex := 0 to pred(ParamTreeView.Items.Count) do
        if TFavoriteTabs(ParamTreeView.Items[iIndex].Data).Dispatcher = fte_ACTUALFAVTABS then
          ParamTreeView.Items[iIndex].Selected := (slRememberCurrentSelections.IndexOf(GUIDtoString(TFavoriteTabs(ParamTreeView.Items[iIndex].Data).UniqueID)) <> -1);
    finally
      TransitFavoriteTabsList.Clear;
      TransitFavoriteTabsList.Free;
      FreeAndNil(slRememberCurrentSelections);
    end;
  end
  else
  begin
    Self.Clear;
    mbDeleteFile(GetSingleXmlFavoriteTabsFilename);
  end;
end;

{ TFavoriteTabsList.ImportFromLegacyTabsFile }
function TFavoriteTabsList.ImportFromLegacyTabsFile(paramFilename: string; SpecifiedIndex: integer = -1): boolean;
var
  iNode, oNode: TXmlNode;
  InputXmlConfig: TXmlConfig;
  OutputXmlConfig: TXmlConfig;
  LocalFavoriteTabs: TFavoriteTabs;

begin
  Result := False;
  try
    LocalFavoriteTabs := TFavoriteTabs.Create;
    LocalFavoriteTabs.Dispatcher := fte_ACTUALFAVTABS;
    LocalFavoriteTabs.FavoriteTabsName := ExtractOnlyFileName(paramFilename);
    LocalFavoriteTabs.DestinationForSavedLeftTabs := tclLeft;
    LocalFavoriteTabs.DestinationForSavedRightTabs := tclRight;
    LocalFavoriteTabs.ExistingTabsToKeep := tclNone;
    //The UniqueID is not assigned here because it has already been set when "LocalFavoriteTabs" has been created.

    InputXmlConfig := TXmlConfig.Create(paramFilename, True);
    OutputXmlConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);

    try
      AddToListAndToXmlFileHeader(LocalFavoriteTabs, OutputXmlConfig, SpecifiedIndex);

      iNode := InputXmlConfig.FindNode(InputXmlConfig.RootNode, 'Tabs/OpenedTabs/Left');
      oNode := OutputXmlConfig.FindNode(OutputXmlConfig.RootNode, 'ActualTabs/' + LocalFavoriteTabs.GuidToXMLString, True);

      while iNode <> nil do
      begin
        GenericCopierProcessNode(iNode, OutputXmlConfig, oNode); // Procédure récursive
        iNode := iNode.NextSibling;
      end;

      OutputXmlConfig.Save;
      gFavoriteTabsList.LastImportationStringUniqueId.Add(GUIDToString(LocalFavoriteTabs.UniqueID));
      Result := True;
    finally
      FreeAndNil(OutputXmlConfig);
      FreeAndNil(InputXmlConfig);
    end;
  except
    on E: Exception do
      msgError(E.Message);
  end;
end;

{ TFavoriteTabsList.ExportToLegacyTabsFile }
function TFavoriteTabsList.ExportToLegacyTabsFile(index: integer; OutputDirectory: string): boolean;
var
  iNode, oNode: TXmlNode;
  InputXmlConfig: TXmlConfig;
  OutputXmlConfig: TXmlConfig;
  sBasicOutputFilename, sConfigFilename: string;
  iAttempt: integer;
begin
  Result := False;
  try
    // 1. Let's try to give an exported filename based of the Favorite Tabs friendly name.
    //    If a filename like that already exists, add "(x)" to the name and increase "x" until the file does not already exists!
    sBasicOutputFilename := RemoveInvalidCharsFromFileName(FavoriteTabs[index].FavoriteTabsName);
    if sBasicOutputFilename = '' then sBasicOutputFilename := 'TabsExported';
    sBasicOutputFilename := IncludeTrailingPathDelimiter(OutputDirectory) + sBasicOutputFilename;
    sConfigFilename := sBasicOutputFilename + '.tab';
    iAttempt := 1;
    while FileExists(sConfigFilename) do
    begin
      sConfigFilename := sBasicOutputFilename + '(' + IntToStr(iAttempt) + ').tab';
      Inc(iAttempt);
    end;

    // 2. Ok. Let's start our exportatation. Basically we start from the section of the source setup and write it to a new single isolated one.
    InputXmlConfig := TXmlConfig.Create(GetSingleXmlFavoriteTabsFilename, True);
    OutputXmlConfig := TXmlConfig.Create(sConfigFilename);

    try
      iNode := InputXmlConfig.FindNode(InputXmlConfig.RootNode, 'ActualTabs/' + FavoriteTabs[index].GuidToXMLString + '/Left');
      if iNode <> nil then
      begin
        oNode := OutputXmlConfig.FindNode(OutputXmlConfig.RootNode, 'Tabs/OpenedTabs', True);
        while iNode <> nil do
        begin
          GenericCopierProcessNode(iNode, OutputXmlConfig, oNode); // Procédure récursive
          iNode := iNode.NextSibling;
        end;

        OutputXmlConfig.Save;
        gFavoriteTabsList.LastImportationStringUniqueId.Add(sConfigFilename);
        Result := True;
      end;
    finally
      FreeAndNil(OutputXmlConfig);
      FreeAndNil(InputXmlConfig);
    end;
  except
    on E: Exception do
      msgError(E.Message);
  end;
end;

end.
