{
   Double Commander
   -------------------------------------------------------------------------
   Structure/Load/Save/Working With FavoriteTab and List of them

   Copyright (C) 2016  Alexander Koblov (alexx2000@mail.ru)

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

   -This unit has been added in 2016, inspired a lot from "uhotdir".
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
  cSectionOfFavoriteTab = 'FavoriteTabsList';

  // These has to match with the "ICONINDEXNAME" sequence in "uHotDir.pas" since loaded in "imgLstDirectoryHotlist" in "fMain".
  // (Did not want to createa new image list just for this :-/ )
  ICONINDEX_NEWADDEDFAVTABS = 4;
  ICONINDEX_SUBMENUFAVTABS = 5;

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
    FFavoriteTabsSavedFilename: string;
    FDestinationForSavedLeftTabs: TTabsConfigLocation;
    FDestinationForSavedRightTabs: TTabsConfigLocation;
    FExistingTabsToKeep: TTabsConfigLocation;
    FUniqueID: TGUID;
    FGroupNumber: integer; // We won't save in the XML. Just useful run-time with the tree.
  public
    constructor Create;
    procedure CopyToFavoriteTabs(DestinationFavoriteTabs: TFavoriteTabs; bExactCopyWanted: boolean = True);
    property Dispatcher: TKindOfFavoriteTabsEntry read FDispatcher write FDispatcher;
    property FavoriteTabsName: string read FFavoriteTabsName write FFavoriteTabsName;
    property FavoriteTabsSavedFilename: string read FFavoriteTabsSavedFilename write FFavoriteTabsSavedFilename;
    property DestinationForSavedLeftTabs: TTabsConfigLocation read FDestinationForSavedLeftTabs write FDestinationForSavedLeftTabs;
    property DestinationForSavedRightTabs: TTabsConfigLocation read FDestinationForSavedRightTabs write FDestinationForSavedRightTabs;
    property ExistingTabsToKeep: TTabsConfigLocation read FExistingTabsToKeep write FExistingTabsToKeep;
    property UniqueID: TGUID read FUniqueID write FUniqueID;
    property GroupNumber: integer read FGroupNumber write FGroupNumber;
  end;

  { TFavoriteTabsList }
  TFavoriteTabsList = class(TList)
  private
    FLastFavoriteTabsLoadedUniqueId: TGUID;
    function GetFavoriteTabs(Index: integer): TFavoriteTabs;
  public
    constructor Create;
    procedure Clear; override;
    function Add(FavoriteTabs: TFavoriteTabs): integer;
    procedure DeleteFavoriteTabs(Index: integer);
    procedure CopyFavoriteTabsListToFavoriteTabsList(var DestinationFavoriteTabsList: TFavoriteTabsList);
    function ComputeSignature: dword;
    function GetIndexLastFavoriteTabsLoaded: integer;
    function GetIndexPreviousLastFavoriteTabsLoaded: integer;
    function GetIndexNextLastFavoriteTabsLoaded: integer;
    function GetSuggestedParamsForFavoriteTabs(sAttemptedName: string; var SuggestedFavoriteTabsName: string; var SuggestedFavoriteTabsSavedFilename: string): boolean;
    function GetIndexForSuchFavoriteTabsName(sSearchedFavoriteTabsName: string): integer;
    procedure PopulateMenuWithFavoriteTabs(mncmpMenuComponentToPopulate: TComponent; ProcedureWhenFavoriteTabItemClicked: TProcedureWhenClickOnMenuItem; KindFavoriteTabMenuPopulation: TKindFavoriteTabsMenuPopulation);
    function LoadTTreeView(ParamTreeView: TTreeView; FavoriteTabsIndexToSelectIfAny: longint): TTreeNode;
    procedure RefreshFromTTreeView(ParamTreeView: TTreeView);
    procedure LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode; FlagEraseOriginalOnes: boolean);
    property FavoriteTabs[Index: integer]: TFavoriteTabs read GetFavoriteTabs;
    property LastFavoriteTabsLoadedUniqueId: TGUID read FLastFavoriteTabsLoadedUniqueId write FLastFavoriteTabsLoadedUniqueId;
  end;

{ GetNewUniqueID }
function GetNewUniqueID: TGUID;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  crc, Graphics, Forms, lazutf8, Dialogs,

  //DC
  fMain, DCFileAttributes, uDebug, uDCUtils, uLng, DCOSUtils, uGlobs,
  uFileProcs, DCStrUtils;

{ GetNewUniqueID }
function GetNewUniqueID: TGUID;
var
  iIndex: integer;
begin
  if CreateGuid(Result) <> 0 then
  begin
    Result.Data1 := random($233528DE);
    Result.Data2 := random($FFFF);
    Result.Data3 := random($FFFF);
    for iIndex := 0 to 7 do Result.Data4[iIndex] := random($FF);
  end;
end;

{ TFavoriteTabs.Create }
constructor TFavoriteTabs.Create;
begin
  inherited Create;
  FDispatcher := fte_NULL;
  FFavoriteTabsName := '';
  FFavoriteTabsSavedFilename := '';
  FDestinationForSavedLeftTabs := tclLeft;
  FDestinationForSavedRightTabs := tclRight;
  FExistingTabsToKeep := tclNone;
  FUniqueID := GetNewUniqueID;
  FGroupNumber := 0;
end;

{ TFavoriteTabs.CopyToFavoriteTabs }
procedure TFavoriteTabs.CopyToFavoriteTabs(DestinationFavoriteTabs: TFavoriteTabs; bExactCopyWanted: boolean = True);
begin
  DestinationFavoriteTabs.Dispatcher := FDispatcher;
  DestinationFavoriteTabs.FavoriteTabsName := FFavoriteTabsName;
  DestinationFavoriteTabs.FavoriteTabsSavedFilename := FFavoriteTabsSavedFilename;
  DestinationFavoriteTabs.DestinationForSavedLeftTabs := FDestinationForSavedLeftTabs;
  DestinationFavoriteTabs.DestinationForSavedRightTabs := FDestinationForSavedRightTabs;
  DestinationFavoriteTabs.ExistingTabsToKeep := FExistingTabsToKeep;
  if bExactCopyWanted then DestinationFavoriteTabs.UniqueID := FUniqueId else DestinationFavoriteTabs.UniqueID := GetNewUniqueID;
  DestinationFavoriteTabs.GroupNumber := FGroupNumber;
end;

{ TFavoriteTabsList.Create }
constructor TFavoriteTabsList.Create;
begin
  inherited Create;
  LastFavoriteTabsLoadedUniqueId := GetNewUniqueID;
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

  // Next line is useful for when calling "cm_ConfigFavoriteTabs-SubmitToAddOrConfigToFavoriteTabsDlg with ftaaocJustShowFavoriteTabsConfig" so the last favorite tabs setup loaded will be selected in the tree when we enter in the config screen.
  DestinationFavoriteTabsList.LastFavoriteTabsLoadedUniqueId := FLastFavoriteTabsLoadedUniqueId;
end;

{ TFavoriteTabsList.ComputeSignature }
// Routine tries to pickup all char chain from element of favorite tabs list to compute a unique CRC32.
// This CRC32 will be a kind of signature of the favorite tabs list.
// We compute the CRC32 at the start of edition (TfrmOptionsFavoriteTabs.Load) and
// at the end (TfrmOptionsFavoriteTabs.CanWeClose).
// If they are different, it's a sign that favorite tabs list have been modified.
// It's not "perfect" since it might happen that two different combinaisons will
// give the same CRC32 but odds are very good that it will be a different one.
function TFavoriteTabsList.ComputeSignature: dword;
var
  Index: integer;
begin
  Result := $000000002;
  for Index := 0 to pred(Count) do
  begin
    Result := crc32(Result, @FavoriteTabs[Index].Dispatcher, 1);
    if length(FavoriteTabs[Index].FavoriteTabsName) > 0 then
      Result := crc32(Result, @FavoriteTabs[Index].FavoriteTabsName[1], length(FavoriteTabs[Index].FavoriteTabsName));
    if length(FavoriteTabs[Index].FavoriteTabsSavedFilename) > 0 then
      Result := crc32(Result, @FavoriteTabs[Index].FavoriteTabsSavedFilename[1], length(FavoriteTabs[Index].FavoriteTabsSavedFilename));
    Result := crc32(Result, @FavoriteTabs[Index].DestinationForSavedLeftTabs, sizeof(TTabsConfigLocation));
    Result := crc32(Result, @FavoriteTabs[Index].DestinationForSavedRightTabs, sizeof(TTabsConfigLocation));
    Result := crc32(Result, @FavoriteTabs[Index].ExistingTabsToKeep, sizeof(TTabsConfigLocation));
  end;
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

{ TFavoriteTabsList.GetSuggestedParamsForFavoriteTabs }
// It won't hurt anything here if user want to use more than once the SAME name...
// By adding the (1), (2), etc. between parenthesis, it's a kind of friendly indication the same name already exists.
// But we do not blocked this. If he ever decides to specifically rename with the same name. This was a kind of friendly reminder.
// But regarding the filename, obviously we attempt to force a valid one.
function TFavoriteTabsList.GetSuggestedParamsForFavoriteTabs(sAttemptedName: string; var SuggestedFavoriteTabsName: string; var SuggestedFavoriteTabsSavedFilename: string): boolean;
var
  iIndexInCaseFound: integer;
  FavoriteTabsSavedDirectory, sBaseFilename: string;
begin
  Result := False;

  sAttemptedName := RemoveInvalidCharsFromFileName(sAttemptedName);
  SuggestedFavoriteTabsName := sAttemptedName;
  iIndexInCaseFound := 1;
  while GetIndexForSuchFavoriteTabsName(SuggestedFavoriteTabsName) <> -1 do
  begin
    SuggestedFavoriteTabsName := Format('%s(%d)', [sAttemptedName, iIndexInCaseFound]);
    Inc(iIndexInCaseFound);
  end;

  if InputQuery(rsMsgFavoriteTabsEnterName, rsMsgFavoriteTabsEnterNameTitle, SuggestedFavoriteTabsName) then
  begin
    sAttemptedName := RemoveInvalidCharsFromFileName(sAttemptedName);
    if length(SuggestedFavoriteTabsName) > 0 then
    begin
      FavoriteTabsSavedDirectory := IncludeTrailingPathDelimiter(EnvVarConfigPath) + 'FavoriteTabs';
      if mbForceDirectory(mbExpandFileName(FavoriteTabsSavedDirectory)) then
      begin
        sBaseFilename:=RemoveInvalidCharsFromFileName(SuggestedFavoriteTabsName);
        if length(sBaseFilename)=0 then sBaseFilename:='GenericTabFile';
        SuggestedFavoriteTabsSavedFilename := FavoriteTabsSavedDirectory + DirectorySeparator + sBaseFilename + '.tab';
        iIndexInCaseFound := 1;
        while mbFileExists(mbExpandFileName(SuggestedFavoriteTabsSavedFilename)) do
        begin
          SuggestedFavoriteTabsSavedFilename := FavoriteTabsSavedDirectory + DirectorySeparator + sBaseFilename + '(' + IntToStr(iIndexInCaseFound) + ').tab';
          Inc(iIndexInCaseFound);
        end;

        Result := True;
      end;
    end;
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

{ TFavoriteTabsList.PopulateMenuWithFavoriteTabs }
procedure TFavoriteTabsList.PopulateMenuWithFavoriteTabs(mncmpMenuComponentToPopulate: TComponent; ProcedureWhenFavoriteTabItemClicked: TProcedureWhenClickOnMenuItem; KindFavoriteTabMenuPopulation: TKindFavoriteTabsMenuPopulation);
var
  I: longint; //Same variable for main and local routine
  LocalIndexOfLast: integer;

  // WARNING: "CompleteMenu" is recursive and calls itself!
  function CompleteMenu(ParamMenuItem: TComponent; TagOffset: integer = 0): longint;
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
            TMenuItem(ParamMenuItem).Add(localmi);
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
          if gIconsInMenus then
            localmi.ImageIndex := frmMain.miLoadFavoriteTabs.ImageIndex;
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
  miMainTree, miSubElement: TMenuItem;
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
      // 3.1. Add a delimiter, a simple line to separate.
      if Count > 0 then
      begin
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := '-';
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
      end;

      // 3.2 Now add "Add current tabs to Favorite Tabs".
      miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
      miMainTree.Action := frmMain.actSaveFavoriteTabs;
      if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
      else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

      // 3.3. If we have at least one entry, let's create the "Save Over Existing.." items.
      if Count > 0 then
      begin
        // 3.3.1. Add the "Save current tabs over existing Favorite Tabs entry" in a submenu.
        //        It's placed in a sub menu since when it's time to saved since it's less frequent then keeping accessing...
        //        ...and to avoid to click on it by accident and overwring existing stuff.
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := rsMsgFavortieTabsSaveOverExisting;
        miMainTree.ImageIndex := frmMain.actSaveFavoriteTabs.ImageIndex;
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
          TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        // 3.3.3. Add "Save over existing entry in Favorite Tabs (%s)", name of *last loaded*
        //        The loast loaded one has a special treatment to maybe speed up user's eyes to view it and click on it.
        LocalIndexOfLast := GetIndexLastFavoriteTabsLoaded; //Let's have a temp local value so we won't have "LastFavoriteTabsIndexLoaded2015" who search 3 times...
        if LocalIndexOfLast <> -1 then
        begin
          miSubElement := TMenuItem.Create(mncmpMenuComponentToPopulate);
          miSubElement.Caption := Format(rsMsgFavoriteTabsSaveLastLoadOver, [FavoriteTabs[LocalIndexOfLast].FavoriteTabsName]);;
          miSubElement.Tag := LocalIndexOfLast + TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING;
          miSubElement.OnClick := ProcedureWhenFavoriteTabItemClicked;
          miMainTree.Add(miSubElement);

          miSubElement := TMenuItem.Create(mncmpMenuComponentToPopulate);
          miSubElement.Caption := '-';
          miMainTree.Add(miSubElement);
        end;

        // 3.3.4. And then add our favorite tabs again BUT with the "TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING" offset in the tag.
        I := 0;
        CompleteMenu(miMainTree, TAGOFFSET_FAVTABS_FORSAVEOVEREXISTING);

        // 3.3.5. Then another separator.
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

      // 3.4 Now add "Configure Favorite Tabs".
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

{ TFavoriteTabsList.LoadTTreeView }
function TFavoriteTabsList.LoadTTreeView(ParamTreeView: TTreeView; FavoriteTabsIndexToSelectIfAny: longint): TTreeNode;
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
          if FavoriteTabsIndexToSelectIfAny = Index then
            Result := LocalNode;
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
          if FavoriteTabsIndexToSelectIfAny = Index then
            Result := LocalNode;
          Inc(Index);
        end

        else // ...but should not happened.
        begin
          LocalNode := ParamTreeView.Items.AddChildObject(WorkingNode, FavoriteTabs[Index].FavoriteTabsName, FavoriteTabs[Index]);
          if FavoriteTabsIndexToSelectIfAny = Index then
            Result := LocalNode;
          Inc(Index);
        end;
      end;
    end;
  end;

begin
  Result := nil;
  ParamTreeView.Items.Clear;
  Index := 0;
  RecursivAddElements(nil);
end;

{ TFavoriteTabsList.RefreshFromTTreeView }
// The routine will recreate the complete TFavoriteTabsList from a TTreeView.
// It cannot erase or replace immediately the current list because the TTreeView refer to it!
// So it create it into the "TransitFavoriteTabsList" and then, it will copy it to self one.

procedure TFavoriteTabsList.RefreshFromTTreeView(ParamTreeView: TTreeView);
var
  TransitFavoriteTabsList: TFavoriteTabsList;
  IndexToTryToRestore: longint = -1;
  MaybeTTreeNodeToSelect: TTreeNode;

  procedure RecursiveEncapsulateSubMenu(WorkingTreeNode: TTreeNode);
  var
    MaybeChildNode: TTreeNode;
    WorkingFavoriteTabEntry: TFavoriteTabs;
  begin
    while WorkingTreeNode <> nil do
    begin
      if WorkingTreeNode = ParamTreeView.Selected then
        IndexToTryToRestore := TransitFavoriteTabsList.Count;

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
    TransitFavoriteTabsList := TFavoriteTabsList.Create;
    try
      TransitFavoriteTabsList.LastFavoriteTabsLoadedUniqueId := FLastFavoriteTabsLoadedUniqueId;
      RecursiveEncapsulateSubMenu(ParamTreeView.Items.Item[0]);
      TransitFavoriteTabsList.CopyFavoriteTabsListToFavoriteTabsList(self);
      MaybeTTreeNodeToSelect := LoadTTreeView(ParamTreeView, IndexToTryToRestore);
      if MaybeTTreeNodeToSelect <> nil then
        MaybeTTreeNodeToSelect.Selected := True
      else if ParamTreeView.Items.Count > 0 then
        ParamTreeView.Items.Item[0].Selected := True;
    finally
      TransitFavoriteTabsList.Clear;
      TransitFavoriteTabsList.Free;
    end;
  end
  else
  begin
    Self.Clear;
  end;
end;

{ TFavoriteTabsList.LoadFromXml }
// Information are stored in similar way to "hotdir", which was similar to TC.
// It's enclosed in try/except to keep the flow *after* in an unexpected error happened.
procedure TFavoriteTabsList.LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sName, sMaybeId: string;
  LocalFavoriteTabs: TFavoriteTabs;
  CurrentMenuLevel: integer;
  FlagAvortInsertion: boolean;
begin
  try
    Clear;
    CurrentMenuLevel := 0;

    ANode := ANode.FindNode(cSectionOfFavoriteTab);
    if Assigned(ANode) then
    begin
      // First item is the last Favorite Tabs ID that was loaded on previous session, let's attempt load it!
      ANode := ANode.FirstChild;
      if Assigned(ANode) then
      begin
        if ANode.CompareName('FavoriteTabs') = 0 then
        begin
          if AConfig.TryGetAttr(ANode, 'LastUniqueID', sMaybeId) then
          begin
            LastFavoriteTabsLoadedUniqueId := StringToGuid(sMaybeId);
          end;
        end;
        ANode := ANode.NextSibling;
      end;

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
                  LocalFavoriteTabs.FavoriteTabsSavedFilename := AConfig.GetAttr(Anode, 'Filename', '');
                  LocalFavoriteTabs.Dispatcher := fte_ACTUALFAVTABS;
                  LocalFavoriteTabs.DestinationForSavedLeftTabs := TTabsConfigLocation(AConfig.GetAttr(Anode, 'DestLeft', integer(tclLeft)));
                  LocalFavoriteTabs.DestinationForSavedRightTabs := TTabsConfigLocation(AConfig.GetAttr(Anode, 'DestRight', integer(tclRight)));
                  LocalFavoriteTabs.ExistingTabsToKeep := TTabsConfigLocation(AConfig.GetAttr(Anode, 'ExistingKeep', integer(tclNone)));
                  LocalFavoriteTabs.UniqueID := StringToGuid(AConfig.GetAttr(Anode, 'UniqueID', GuidToString(GetNewUniqueID)));
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

      //Try to fix possible problem if the LAST MENU is not ending correctly...
      while CurrentMenuLevel > 0 do
      begin
        Dec(CurrentMenuLevel);
        LocalFavoriteTabs := TFavoriteTabs.Create;
        LocalFavoriteTabs.Dispatcher := fte_ENDMENU;
        Add(LocalFavoriteTabs);
      end;

    end;
  except
    MessageDlg(rsMsgFavoriteTabsErrorLoading, mtError, [mbOK], 0);
  end;
end;

{ TFavoriteTabsList.SaveToXml }
// Information are stored in similar way to "hotdir", which was similar to TC.}
// It's enclosed in try/except to keep the flow *after* in an unexpected error happened.
procedure TFavoriteTabsList.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode; FlagEraseOriginalOnes: boolean);
var
  Index: integer;
  SubNode: TXmlNode;
begin
  try
    ANode := AConfig.FindNode(ANode, cSectionOfFavoriteTab, True);
    if FlagEraseOriginalOnes then
      AConfig.ClearNode(ANode);

    SubNode := AConfig.AddNode(ANode, 'FavoriteTabs');
    AConfig.SetAttr(SubNode, 'LastUniqueID', GuidToString(LastFavoriteTabsLoadedUniqueId));

    for Index := 0 to pred(Count) do
    begin
      SubNode := AConfig.AddNode(ANode, 'FavoriteTabs');

      case TFavoriteTabs(FavoriteTabs[Index]).Dispatcher of
        fte_NULL:
        begin
          AConfig.SetAttr(SubNode, 'Name', '');
          AConfig.SetAttr(SubNode, 'Filename', '');
        end;

        fte_ACTUALFAVTABS:
        begin
          AConfig.SetAttr(SubNode, 'Name', FavoriteTabs[Index].FavoriteTabsName);
          AConfig.SetAttr(SubNode, 'Filename', FavoriteTabs[Index].FavoriteTabsSavedFilename);
          AConfig.SetAttr(SubNode, 'DestLeft', integer(FavoriteTabs[Index].DestinationForSavedLeftTabs));
          AConfig.SetAttr(SubNode, 'DestRight', integer(FavoriteTabs[Index].DestinationForSavedRightTabs));
          AConfig.SetAttr(SubNode, 'ExistingKeep', integer(FavoriteTabs[Index].ExistingTabsToKeep));
          AConfig.SetAttr(SubNode, 'UniqueID', GuidToString(FavoriteTabs[Index].UniqueID));
        end;

        fte_SEPARATOR:
        begin
          AConfig.SetAttr(SubNode, 'Name', '-');
        end;

        fte_STARTMENU:
        begin
          AConfig.SetAttr(SubNode, 'Name', '-' + FavoriteTabs[Index].FavoriteTabsName);
        end;

        fte_ENDMENU:
        begin
          AConfig.SetAttr(SubNode, 'Name', '--');
        end;
      end;
    end;
  except
    MessageDlg(rsMsgFavoriteTabsErrorSaving, mtError, [mbOK], 0);
  end;
end;

{ TFavoriteTabsList.GetFavoriteTabs }
function TFavoriteTabsList.GetFavoriteTabs(Index: integer): TFavoriteTabs;
begin
  Result := TFavoriteTabs(Items[Index]);
end;

end.
