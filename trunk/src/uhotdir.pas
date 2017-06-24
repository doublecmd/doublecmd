{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save/WorkingWith HotDir

   Copyright (C) 2014-2016  Alexander Koblov (alexx2000@mail.ru)

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

unit uHotDir;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Classes, SysUtils, Menus, ExtCtrls, Controls, ComCtrls,

  //DC
  DCClassesUtf8, DCXmlConfig;
const
  cSectionOfHotDir = 'DirectoryHotList';

  ACTION_INVALID = 0;
  ACTION_ADDTOHOTLIST = 1;
  ACTION_ADDJUSTSOURCETOHOTLIST = 2;
  ACTION_ADDBOTHTOHOTLIST = 3;
  ACTION_CONFIGTOHOTLIST = 4;
  ACTION_JUSTSHOWCONFIGHOTLIST = 5;
  ACTION_ADDSELECTEDDIR = 6;
  ACTION_DIRECTLYCONFIGENTRY = 7;

  HOTLISTMAGICWORDS:array[1..7] of string =('add','addsrconly','addboth','config','show','addsel','directconfig');

  TAGOFFSET_FORCHANGETOSPECIALDIR = $10000;

  ICONINDEX_SUBMENU = 0;
  ICONINDEX_DIRECTORYNOTPRESENTHERE = 1;
  ICONINDEX_SUBMENUWITHMISSING = 2;
  ICONINDEX_NEWADDEDDIRECTORY = 3;
  ICONINDEXNAME:array[0..3] of string = ('submenu','dirmissing','submenuwithmissing','newaddition');

  HOTLIST_SEPARATORSTRING:string='···························';
  TERMINATORNOTPRESENT = ':-<#/?*+*?\#>-:';

  STR_ACTIVEFRAME: string = 'panel=active';
  STR_NOTACTIVEFRAME: string = 'panel=inactive';
  STR_LEFTFRAME: string = 'panel=left';
  STR_RIGHTFRAME: string = 'panel=right';
  STR_NAME: string = 'column=name';
  STR_EXTENSION: string = 'column=ext';
  STR_SIZE: string = 'column=size';
  STR_MODIFICATIONDATETIME: string = 'column=datetime';
  STR_ASCENDING : string = 'order=ascending';
  STR_DESCENDING : string = 'order=descending';


type
  { TKindOfHotDirEntry }
  TKindOfHotDirEntry = (hd_NULL, hd_CHANGEPATH, hd_SEPARATOR, hd_STARTMENU, hd_ENDMENU, hd_COMMAND);

  { TKindHotDirMenuPopulation }
  TKindHotDirMenuPopulation = (mpJUSTHOTDIRS, mpHOTDIRSWITHCONFIG, mpPATHHELPER);

  { TPositionWhereToAddHotDir }
  TPositionWhereToAddHotDir = (ahdFirst, ahdLast, ahdSmart);

  { TExistingState }
  TExistingState = (DirExistUnknown, DirExist, DirNotExist);

  { TProcedureWhenClickMenuItem}
  TProcedureWhenClickOnMenuItem = procedure(Sender: TObject) of object;

  { THotDir }
  THotDir = class
  private
    FDispatcher: TKindOfHotDirEntry;
    FHotDirName: string;
    FHotDirPath: string;
    FHotDirPathSort: longint;
    FHotDirTarget: string;
    FHotDirTargetSort: longint;
    FHotDirExistingState: TExistingState;
    FGroupNumber : integer;
  public
    constructor Create;
    procedure CopyToHotDir(var DestinationHotDir: THotDir);
    property Dispatcher: TKindOfHotDirEntry read FDispatcher write FDispatcher;
    property HotDirName: string read FHotDirName write FHotDirName;
    property HotDirPath: string read FHotDirPath write FHotDirPath;
    property HotDirPathSort: longint read FHotDirPathSort write FHotDirPathSort;
    property HotDirTarget: string read FHotDirTarget write FHotDirTarget;
    property HotDirTargetSort: longint read FHotDirTargetSort write FHotDirTargetSort;
    property HotDirExisting: TExistingState read FHotDirExistingState write FHotDirExistingState;
    property GroupNumber: integer read FGroupNumber write FGroupNumber;
  end;

  { TDirectoryHotlist }
  TDirectoryHotlist = class(TList)
  private
    function GetHotDir(Index: integer): THotDir;
  public
    constructor Create;
    procedure Clear; override;
    function Add(HotDir: THotDir): integer;
    procedure DeleteHotDir(Index: integer);
    procedure CopyDirectoryHotlistToDirectoryHotlist(var DestinationDirectoryHotlist: TDirectoryHotlist);
    procedure LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode; FlagEraseOriginalOnes: boolean);
    procedure ImportDoubleCommander(DoubleCommanderFilename: String);
    function ExportDoubleCommander(DoubleCommanderFilename: String; FlagEraseOriginalOnes: boolean): boolean;
    procedure PopulateMenuWithHotDir(mncmpMenuComponentToPopulate: TComponent; ProcedureWhenHotDirItemClicked, ProcedureWhenHotDirAddOrConfigClicked: TProcedureWhenClickOnMenuItem; KindHotDirMenuPopulation: TKindHotDirMenuPopulation; TagOffset: longint);
    function LoadTTreeView(ParamTreeView:TTreeView; DirectoryHotlistIndexToSelectIfAny:longint):TTreeNode;
    procedure RefreshFromTTreeView(ParamTreeView:TTreeView);
    function AddFromAnotherTTreeViewTheSelected(ParamWorkingTreeView, ParamTreeViewToImport:TTreeView; FlagAddThemAll: boolean): longint;
    function ComputeSignature(Seed:dword=$00000000):dword;
    property HotDir[Index: integer]: THotDir read GetHotDir;
    {$IFDEF MSWINDOWS}
    function ImportTotalCommander(TotalCommanderFilename: String): integer;
    function ExportTotalCommander(TotalCommanderFilename: String; FlagEraseOriginalOnes: boolean): boolean;
    {$ENDIF}
  end;

  { TCheckDrivePresenceThread }
  TCheckDrivePresenceThread = class(TThread)
  private
    FDriveToSearchFor: string;
    FListOfNonExistingDrive: TStringList;
    FThreadCountPoint: ^longint;
    procedure ReportNotPresentInTheThread;
    procedure ReportPresentInTheThread;
  protected
    procedure Execute; override;
  public
    constructor Create(sDrive: string; ParamListOfNonExistingDrive: TStringList; var ThreadCount: longint);
    destructor Destroy; override;
  end;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  crc, Graphics, Forms, lazutf8,

  //DC
  DCFileAttributes, uDebug, uDCUtils, fMain, uFile,  uLng, DCOSUtils, uGlobs,
  uSpecialDir
{$IFDEF MSWINDOWS}
  ,uTotalCommander
{$ENDIF}
  ;

{ THotDir.Create }
constructor THotDir.Create;
begin
  inherited Create;
  FDispatcher := hd_NULL;
  FHotDirName := '';
  FHotDirPath := '';
  FHotDirPathSort := 0;
  FHotDirTarget := '';
  FHotDirTargetSort := 0;
  FHotDirExistingState := DirExistUnknown;
  FGroupNumber := 0;
end;

{ THotDir.CopyToHotDir }
procedure THotDir.CopyToHotDir(var DestinationHotDir: THotDir);
begin
  DestinationHotDir.Dispatcher := FDispatcher;
  DestinationHotDir.HotDirName := FHotDirName;
  DestinationHotDir.HotDirPath := FHotDirPath;
  DestinationHotDir.HotDirPathSort := FHotDirPathSort;
  DestinationHotDir.HotDirTarget := FHotDirTarget;
  DestinationHotDir.HotDirTargetSort := FHotDirTargetSort;
  DestinationHotDir.HotDirExisting := FHotDirExistingState;
  DestinationHotDir.GroupNumber := FGroupNumber;
end;

{ TDirectoryHotlist.Create }
constructor TDirectoryHotlist.Create;
begin
  inherited Create;
end;

{ TDirectoryHotlist.Clear }
procedure TDirectoryHotlist.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do HotDir[i].Free;
  inherited Clear;
end;

{ TDirectoryHotlist.Add }
function TDirectoryHotlist.Add(HotDir: THotDir): integer;
begin
  Result := inherited Add(HotDir);
end;

{ TDirectoryHotlist.DeleteHotDir }
procedure TDirectoryHotlist.DeleteHotDir(Index: integer);
begin
  HotDir[Index].Free;
  Delete(Index);
end;

{ TDirectoryHotlist.CopyDirectoryHotlistToDirectoryHotlist }
procedure TDirectoryHotlist.CopyDirectoryHotlistToDirectoryHotlist(var DestinationDirectoryHotlist: TDirectoryHotlist);
var
  LocalHotDir: THotDir;
  Index: longint;
begin
  //Let's delete possible previous list content
  for Index := pred(DestinationDirectoryHotlist.Count) downto 0 do DestinationDirectoryHotlist.DeleteHotDir(Index);
  DestinationDirectoryHotlist.Clear;

  //Now let's create entries and add them one by one to the destination list
  for Index := 0 to pred(Count) do
  begin
    LocalHotDir := THotDir.Create;
    LocalHotDir.Dispatcher := HotDir[Index].Dispatcher;
    LocalHotDir.HotDirName := HotDir[Index].HotDirName;
    LocalHotDir.HotDirPath := HotDir[Index].HotDirPath;
    LocalHotDir.HotDirPathSort := HotDir[Index].HotDirPathSort;
    LocalHotDir.HotDirTarget := HotDir[Index].HotDirTarget;
    LocalHotDir.HotDirTargetSort := HotDir[Index].HotDirTargetSort;
    LocalHotDir.FHotDirExistingState := HotDir[Index].HotDirExisting;
    LocalHotDir.FGroupNumber := HotDir[Index].GroupNumber;
    DestinationDirectoryHotlist.Add(LocalHotDir);
  end;
end;

{ TDirectoryHotlist.LoadTTreeView }
//For each node, the "ImageIndex" field is recuperated to be an index of which
//item in the directory list it represent. Because of the fact that the
//"hd_ENDMENU's" don't have their direct element in the tree, the field
//"absoluteindex" cannot be used for that since as soon as there is a subment,
//we lost the linearity of the matching of absoluteindex vs index of hotdir in
//the list.
function TDirectoryHotlist.LoadTTreeView(ParamTreeView:TTreeView; DirectoryHotlistIndexToSelectIfAny:longint):TTreeNode;
var
  Index: longint;

  procedure RecursivAddElements(WorkingNode: TTreeNode);
  var
    FlagGetOut: boolean = False;
    LocalNode: TTreeNode;
  begin
    while (FlagGetOut = False) and (Index < Count) do
    begin
      case HotDir[Index].Dispatcher of
        hd_STARTMENU:
        begin
          LocalNode := ParamTreeView.Items.AddChildObject(WorkingNode, HotDir[Index].HotDirName,HotDir[Index]);
          if HotDir[Index].FHotDirExistingState=DirNotExist then
          begin
            LocalNode.ImageIndex:=ICONINDEX_SUBMENUWITHMISSING;
            LocalNode.SelectedIndex:=ICONINDEX_SUBMENUWITHMISSING;
            LocalNode.StateIndex:=ICONINDEX_SUBMENUWITHMISSING;
          end
          else
          begin
            LocalNode.ImageIndex:=ICONINDEX_SUBMENU;
            LocalNode.SelectedIndex:=ICONINDEX_SUBMENU;
            LocalNode.StateIndex:=ICONINDEX_SUBMENU;
          end;
          LocalNode.Data:=HotDir[Index];
          if DirectoryHotlistIndexToSelectIfAny=Index then result:=LocalNode;
          Inc(Index);
          RecursivAddElements(LocalNode);
        end;

        hd_ENDMENU:
        begin
          FlagGetOut := True;
          Inc(Index);
        end;

        hd_SEPARATOR:
        begin
          LocalNode:=ParamTreeView.Items.AddChildObject(WorkingNode, HOTLIST_SEPARATORSTRING ,HotDir[Index]);
          LocalNode.Data:=HotDir[Index];
          if DirectoryHotlistIndexToSelectIfAny=Index then result:=LocalNode;
          Inc(Index);
        end

        else
        begin
          LocalNode:=ParamTreeView.Items.AddChildObject(WorkingNode, HotDir[Index].HotDirName,HotDir[Index]);
          if HotDir[Index].FHotDirExistingState=DirNotExist then
          begin
            LocalNode.ImageIndex:=ICONINDEX_DIRECTORYNOTPRESENTHERE;
            LocalNode.SelectedIndex:=ICONINDEX_DIRECTORYNOTPRESENTHERE;
            LocalNode.StateIndex:=ICONINDEX_DIRECTORYNOTPRESENTHERE;
          end;
          LocalNode.Data:=HotDir[Index];
          if DirectoryHotlistIndexToSelectIfAny=Index then result:=LocalNode;
          Inc(Index);
        end;
      end;
    end;
  end;

  begin
    result:=nil;
    ParamTreeView.Items.Clear;
    Index := 0;
    RecursivAddElements(nil);
  end;


{ TDirectoryHotlist.PopulateMenuWithHotDir }
procedure TDirectoryHotlist.PopulateMenuWithHotDir(mncmpMenuComponentToPopulate: TComponent; ProcedureWhenHotDirItemClicked, ProcedureWhenHotDirAddOrConfigClicked: TProcedureWhenClickOnMenuItem; KindHotDirMenuPopulation: TKindHotDirMenuPopulation; TagOffset: longint);
var
  I: longint; //Same variable for main and local routine
  FlagCurrentPathAlreadyInMenu, FlagSelectedPathAlreadyInMenu: boolean;
  CurrentPathToSearch, SelectedPathToSearch: string;
  MaybeActiveOrSelectedDirectories: TFiles;

  //Warning: "CompleteMenu" is recursive and call itself.
  function CompleteMenu(ParamMenuItem: TMenuItem): longint;
  var
    localmi: TMenuItem;
    LocalLastAdditionIsASeparator: boolean;
  begin
    Result := 0;
    LocalLastAdditionIsASeparator := False;
    while I < Count do
    begin
      Inc(I);

      case HotDir[I - 1].Dispatcher of
        hd_CHANGEPATH:
        begin
          case HotDir[I - 1].HotDirExisting of
            DirExistUnknown, DirExist:
            begin
              localmi := TMenuItem.Create(ParamMenuItem);
              localmi.Caption := GetMenuCaptionAccordingToOptions(HotDir[I - 1].HotDirName,HotDir[I - 1].HotDirPath);
              localmi.tag := (I - 1) + TagOffset;
              localmi.OnClick := ProcedureWhenHotDirItemClicked;
              ParamMenuItem.Add(localmi);
              if CurrentPathToSearch = UpperCase(mbExpandFileName(HotDir[I - 1].FHotDirPath)) then FlagCurrentPathAlreadyInMenu := True;
              if SelectedPathToSearch = UpperCase(mbExpandFileName(HotDir[I - 1].FHotDirPath)) then FlagSelectedPathAlreadyInMenu := True;
              LocalLastAdditionIsASeparator := False;
              Inc(Result);
            end;
          end;
        end;

        hd_NULL, hd_COMMAND:
        begin
          if KindHotDirMenuPopulation <> mpPATHHELPER then
          begin
            localmi := TMenuItem.Create(ParamMenuItem);
            localmi.Caption := HotDir[I - 1].HotDirName;
            localmi.tag := (I - 1) + TagOffset;
            localmi.OnClick := ProcedureWhenHotDirItemClicked;
            ParamMenuItem.Add(localmi);
            LocalLastAdditionIsASeparator := False;
            Inc(Result);
          end;
        end;

        hd_SEPARATOR:
        begin
          if (ParamMenuItem.Count > 0) and (not LocalLastAdditionIsASeparator) then
          begin
            localmi := TMenuItem.Create(ParamMenuItem);
            localmi.Caption := '-';
            ParamMenuItem.Add(localmi);
            LocalLastAdditionIsASeparator := True;
            Inc(Result);
          end;
        end;

        hd_STARTMENU:
        begin
          localmi := TMenuItem.Create(ParamMenuItem);
          localmi.Caption := HotDir[I - 1].HotDirName;
          if gIconsInMenus then localmi.ImageIndex:=ICONINDEX_SUBMENU;
          ParamMenuItem.Add(localmi);
          CompleteMenu(localmi);
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

        hd_ENDMENU:
        begin
          if LocalLastAdditionIsASeparator then
          begin
            ParamMenuItem.Items[pred(ParamMenuItem.Count)].Free;
            Dec(Result);
          end;
          exit;
        end;
      end; //case HotDir[I-1].Dispatcher of
    end; //while I<Count do
  end;
var
  miMainTree: TMenuItem;
  LastAdditionIsASeparator: boolean;
  NumberOfElementsSoFar, InitialNumberOfItems: longint;
begin
  MaybeActiveOrSelectedDirectories:=frmMain.ActiveFrame.CloneSelectedOrActiveDirectories;
  try
    // Create All popup menu
    CurrentPathToSearch := UpperCase(mbExpandFileName(frmMain.ActiveFrame.CurrentLocation));
    if MaybeActiveOrSelectedDirectories.Count=1 then SelectedPathToSearch := UpperCase(IncludeTrailingPathDelimiter(mbExpandFileName(MaybeActiveOrSelectedDirectories.Items[0].FullPath))) else SelectedPathToSearch := TERMINATORNOTPRESENT;

    FlagCurrentPathAlreadyInMenu := False;
    FlagSelectedPathAlreadyInMenu := FALSE;
    LastAdditionIsASeparator := False;

    case KindHotDirMenuPopulation of
      mpJUSTHOTDIRS, mpHOTDIRSWITHCONFIG:
      begin
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
        begin
          TPopupMenu(mncmpMenuComponentToPopulate).Items.Clear;
          InitialNumberOfItems := mncmpMenuComponentToPopulate.ComponentCount;
        end;
      end;
    end;

    I := 0;
    while I < Count do
    begin
      Inc(I);

      case HotDir[I - 1].Dispatcher of
        hd_CHANGEPATH:
        begin
          case HotDir[I - 1].HotDirExisting of
            DirExistUnknown, DirExist:
            begin
              miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
              miMainTree.Caption := GetMenuCaptionAccordingToOptions(HotDir[I - 1].HotDirName,HotDir[I - 1].HotDirPath);
              miMainTree.tag := (I - 1) + TagOffset;
              miMainTree.OnClick := ProcedureWhenHotDirItemClicked;

              if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
              else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

              if CurrentPathToSearch = UpperCase(mbExpandFileName(HotDir[I - 1].FHotDirPath)) then FlagCurrentPathAlreadyInMenu := True;
              if SelectedPathToSearch = UpperCase(mbExpandFileName(HotDir[I - 1].FHotDirPath)) then FlagSelectedPathAlreadyInMenu := True;
              LastAdditionIsASeparator := False;
            end;
          end;
        end;

        hd_NULL, hd_COMMAND:
        begin
          miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
          miMainTree.Caption := HotDir[I - 1].HotDirName;
          miMainTree.tag := (I - 1) + TagOffset;
          miMainTree.OnClick := ProcedureWhenHotDirItemClicked;

          if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
          else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

          LastAdditionIsASeparator := False;
        end;

        hd_SEPARATOR:
        begin
          if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then NumberOfElementsSoFar := TPopupMenu(mncmpMenuComponentToPopulate).Items.Count
          else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then NumberOfElementsSoFar := TMenuItem(mncmpMenuComponentToPopulate).Count;
          if (NumberOfElementsSoFar > 0) and (not LastAdditionIsASeparator) then
          begin
            miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
            miMainTree.Caption := '-';
            if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
            else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
            LastAdditionIsASeparator := True;
          end;
        end;

        hd_STARTMENU:
        begin
          miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
          miMainTree.Caption := HotDir[I - 1].HotDirName;
          if gIconsInMenus then miMainTree.ImageIndex := ICONINDEX_SUBMENU;
          if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
            TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
          else
          if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
            TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
          CompleteMenu(miMainTree);
          if miMainTree.Count <> 0 then
          begin
            LastAdditionIsASeparator := False;
          end
          else
          begin
            miMainTree.Free;
          end;
        end;

        hd_ENDMENU:
        begin
          if LastAdditionIsASeparator then
          begin
            if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items[pred(TPopupMenu(mncmpMenuComponentToPopulate).Items.Count)].Free
            else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Items[pred(TMenuItem(mncmpMenuComponentToPopulate).Count)].Free;
          end;
        end;
      end;
    end;

    //2014-08-25:If last item added is a separator, we need to remove it so it will not look bad with another separator added at the end
    if LastAdditionIsASeparator then
    begin
      if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items[pred(TPopupMenu(mncmpMenuComponentToPopulate).Items.Count)].Free
      else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Items[pred(TMenuItem(mncmpMenuComponentToPopulate).Count)].Free;
    end;

    case KindHotDirMenuPopulation of
      mpHOTDIRSWITHCONFIG:
      begin
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
        begin
          if mncmpMenuComponentToPopulate.ComponentCount>InitialNumberOfItems then
          begin
            miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
            miMainTree.Caption := '-';
            if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
            else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
          end;
        end;

        // Let's add the "Special path" in a context of change directory
        gSpecialDirList.PopulateMenuWithSpecialDir(mncmpMenuComponentToPopulate, mp_CHANGEDIR, ProcedureWhenHotDirItemClicked);

        // now add delimiter
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := '-';
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        //now add the "selected path", if any, if it's the case
        if MaybeActiveOrSelectedDirectories.Count>0 then
        begin
          miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
          case MaybeActiveOrSelectedDirectories.Count of
            1: with Application.MainForm as TForm do if not FlagSelectedPathAlreadyInMenu then miMainTree.Caption := rsMsgHotDirAddSelectedDirectory + MinimizeFilePath(MaybeActiveOrSelectedDirectories.Items[0].FullPath, Canvas, 250) else miMainTree.Caption := rsMsgHotDirReAddSelectedDirectory + MinimizeFilePath(MaybeActiveOrSelectedDirectories.Items[0].FullPath, Canvas, 250);
            else miMainTree.Caption := Format(rsMsgHotDirAddSelectedDirectories,[MaybeActiveOrSelectedDirectories.Count]);
          end;
          miMainTree.Tag := ACTION_ADDSELECTEDDIR;
          miMainTree.OnClick := ProcedureWhenHotDirAddOrConfigClicked;
          if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
          else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
        end;

        // now allow to add or re-add the "current path"
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        with Application.MainForm as TForm do if not FlagCurrentPathAlreadyInMenu then miMainTree.Caption := rsMsgHotDirAddThisDirectory + MinimizeFilePath(frmMain.ActiveFrame.CurrentPath, Canvas, 250) else miMainTree.Caption := rsMsgHotDirReAddThisDirectory + MinimizeFilePath(frmMain.ActiveFrame.CurrentPath, Canvas, 250);
        miMainTree.Tag := ACTION_ADDTOHOTLIST;
        miMainTree.OnClick := ProcedureWhenHotDirAddOrConfigClicked;
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        // now add configure item
        miMainTree := TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption := rsMsgHotDirConfigHotlist;
        miMainTree.Tag := ACTION_CONFIGTOHOTLIST;
        miMainTree.ShortCut := frmMain.mnuCmdConfigDirHotlist.ShortCut;
        miMainTree.OnClick := ProcedureWhenHotDirAddOrConfigClicked;
        if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
        else if mncmpMenuComponentToPopulate.ClassType = TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
      end;
    end; //case KindHotDirMenuPopulation of
  finally
    FreeAndNil(MaybeActiveOrSelectedDirectories);
  end;

  if mncmpMenuComponentToPopulate.ClassType = TPopupMenu then
    if TPopupMenu(mncmpMenuComponentToPopulate).Images=nil then
      TPopupMenu(mncmpMenuComponentToPopulate).Images:= frmMain.imgLstDirectoryHotlist;

  if mncmpMenuComponentToPopulate.ClassType = TMenuItem then
    if TMenuItem(mncmpMenuComponentToPopulate).GetParentMenu.Images=nil then
      TMenuItem(mncmpMenuComponentToPopulate).GetParentMenu.Images:= frmMain.imgLstDirectoryHotlist;
end;

{ TDirectoryHotlist.LoadFromXml }
{ Information are stored like originally DC was storing them WITH addition of menu related info in a simular way TC. }
procedure TDirectoryHotlist.LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sName, sPath: string;
  LocalHotDir: THotDir;
  CurrentMenuLevel: integer;
  FlagAvortInsertion: boolean;
begin
  Clear;
  CurrentMenuLevel := 0;

  ANode := ANode.FindNode(cSectionOfHotDir);
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('HotDir') = 0 then
      begin
        if AConfig.TryGetAttr(ANode, 'Name', sName) and AConfig.TryGetAttr(ANode, 'Path', sPath) then
        begin
          FlagAvortInsertion := False;
          LocalHotDir := THotDir.Create;

          if sName = '-' then
          begin
            LocalHotDir.Dispatcher := hd_SEPARATOR;
          end
          else
          begin
            if sName = '--' then
            begin
              LocalHotDir.Dispatcher := hd_ENDMENU;
              if CurrentMenuLevel > 0 then Dec(CurrentMenuLevel) else FlagAvortInsertion := True; //Sanity correction in case we got corrupted from any ways
            end
            else
            begin
              if (UTF8Length(sName) > 1) then
              begin
                if (sName[1] = '-') and (sName[2] <> '-') then
                begin
                  Inc(CurrentMenuLevel);
                  LocalHotDir.Dispatcher := hd_STARTMENU;
                  LocalHotDir.HotDirName := UTF8RightStr(sName, UTF8Length(sName) - 1);
                end;
              end;

              if LocalHotDir.Dispatcher = hd_NULL then
              begin
                LocalHotDir.HotDirName := sName;
                LocalHotDir.HotDirPath := sPath;
                if UTF8Pos('cm_', UTF8LowerCase(sPath)) = 0 then
                begin
                  LocalHotDir.HotDirPathSort := AConfig.GetAttr(Anode, 'PathSort', 0);
                  LocalHotDir.HotDirTarget := AConfig.GetAttr(ANode, 'Target', '');
                  LocalHotDir.HotDirTargetSort := AConfig.GetAttr(Anode, 'TargetSort', 0);
                  if LocalHotDir.HotDirPath<>'' then LocalHotDir.HotDirPath:=IncludeTrailingPathDelimiter(LocalHotDir.HotDirPath);
                  if LocalHotDir.HotDirTarget<>'' then LocalHotDir.HotDirTarget:=IncludeTrailingPathDelimiter(LocalHotDir.HotDirTarget);
                  LocalHotDir.Dispatcher := hd_CHANGEPATH;
                end
                else
                begin
                  LocalHotDir.Dispatcher := hd_COMMAND;
                end;
              end;
            end;
          end;

          if not FlagAvortInsertion then
            begin
              Add(LocalHotDir);
            end
          else
            begin
              LocalHotDir.Free;
            end;
        end;
      end;

      ANode := ANode.NextSibling;
    end;

    //Try to fix possible problem if the LAST MENU is not ending correctly...
    while CurrentMenuLevel > 0 do
    begin
      Dec(CurrentMenuLevel);
      LocalHotDir := THotDir.Create;
      LocalHotDir.Dispatcher := hd_ENDMENU;
      Add(LocalHotDir);
    end;

  end;
end;

{ TDirectoryHotlist.SaveToXml }
// Information are stored like originally DC was storing them WITH addition of menu related info in a simular way TC.
// When the parameter has the same value as it would have when loaded with no value (so with the default value), the parameter is not saved...
// ...this way, it makes the overall filelenth smaller. When running on a portable mode from a usb key, everything thing counts! :-)
// ..."Name" and "Path" always must be present for backward compatibility reason in case someone would go backward.
// ...Not saving the value that are correctly initialized anyway as default on startup, with a setup of 386 entries for example saved 6642 bytes (5.3% of original 126005 bytes file)
//
procedure TDirectoryHotlist.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode; FlagEraseOriginalOnes: boolean);
var
  Index: integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, cSectionOfHotDir, True);
  if FlagEraseOriginalOnes then AConfig.ClearNode(ANode);
  for Index := 0 to pred(Count) do
  begin
    SubNode := AConfig.AddNode(ANode, 'HotDir');

    case THotDir(HotDir[Index]).Dispatcher of
      hd_NULL:
      begin
        AConfig.SetAttr(SubNode, 'Name', '');
        AConfig.SetAttr(SubNode, 'Path', '');
      end;

      hd_CHANGEPATH:
      begin
        AConfig.SetAttr(SubNode, 'Name', HotDir[Index].HotDirName);
        AConfig.SetAttr(SubNode, 'Path', HotDir[Index].HotDirPath);
        if HotDir[Index].HotDirTarget <> '' then AConfig.SetAttr(SubNode, 'Target', HotDir[Index].HotDirTarget);
        if HotDir[Index].HotDirPathSort <> 0 then AConfig.SetAttr(SubNode, 'PathSort', HotDir[Index].HotDirPathSort);
        if HotDir[Index].HotDirTargetSort <> 0 then AConfig.SetAttr(SubNode, 'TargetSort', HotDir[Index].HotDirTargetSort);
      end;

      hd_SEPARATOR:
      begin
        AConfig.SetAttr(SubNode, 'Name', '-');
        AConfig.SetAttr(SubNode, 'Path', '');
      end;

      hd_STARTMENU:
      begin
        AConfig.SetAttr(SubNode, 'Name', '-' + THotDir(HotDir[Index]).HotDirName);
        AConfig.SetAttr(SubNode, 'Path', '');
      end;

      hd_ENDMENU:
      begin
        AConfig.SetAttr(SubNode, 'Name', '--');
        AConfig.SetAttr(SubNode, 'Path', '');
      end;

      hd_COMMAND:
      begin
        AConfig.SetAttr(SubNode, 'Name', HotDir[Index].HotDirName);
        AConfig.SetAttr(SubNode, 'Path', HotDir[Index].HotDirPath);
        if HotDir[Index].HotDirTarget <> '' then AConfig.SetAttr(SubNode, 'Target', HotDir[Index].HotDirTarget);
      end;
    end;
  end;
end;

{ TDirectoryHotlist.ImportDoubleCommander }
procedure TDirectoryHotlist.ImportDoubleCommander(DoubleCommanderFilename: String);
var
  DoubleCommanderXMLToImport: TXmlConfig;
  Root: TXmlNode;
begin
  DoubleCommanderXMLToImport := TXmlConfig.Create(DoubleCommanderFilename);
  try
    if DoubleCommanderXMLToImport.Load then
    begin
      Root := DoubleCommanderXMLToImport.RootNode;
      LoadFromXML(DoubleCommanderXMLToImport, Root);
    end;
  finally
    FreeAndNil(DoubleCommanderXMLToImport);
  end;
end;

{ TDirectoryHotlist.ExportDoubleCommander }
function TDirectoryHotlist.ExportDoubleCommander(DoubleCommanderFilename: String; FlagEraseOriginalOnes: boolean): boolean;
var
  DoubleCommanderXMLToImport: TXmlConfig;
  Root: TXmlNode;
  FlagKeepGoing: boolean;
begin
  Result := False; //Unless we reach correctly the end, the result is negative
  FlagKeepGoing := True;

  DoubleCommanderXMLToImport := TXmlConfig.Create(DoubleCommanderFilename);
  try
    //Just in case we're requested to add or update content of a .XML file will already other data in it, first we load the structure
    if FileExists(DoubleCommanderFilename) then FlagKeepGoing := DoubleCommanderXMLToImport.Load;

    if FlagKeepGoing then
    begin
      Root := DoubleCommanderXMLToImport.RootNode;
      SaveToXml(DoubleCommanderXMLToImport, Root, FlagEraseOriginalOnes);
      Result := DoubleCommanderXMLToImport.Save;
    end;
  finally
    FreeAndNil(DoubleCommanderXMLToImport);
  end;
end;

{ TDirectoryHotlist.AddFromAnotherTTreeViewTheSelected }
//It looks the ".selected" field only gives us the kind of "itemindex" of current selection in the TTREE.
//So, the apparent way to detect the current selected node is to check the ".Selection" fields.
//So, we'll set the "GroupNumber" of pointed HotDir to 1 to indicate the one to import.
//
function TDirectoryHotlist.AddFromAnotherTTreeViewTheSelected(ParamWorkingTreeView, ParamTreeViewToImport:TTreeView; FlagAddThemAll: boolean): longint;

  procedure RecursiveAddTheOnesWithGroupNumberOne(WorkingTreeNode:TTreeNode; InsertionNodePlace:TTreeNode);
  var
    MaybeChildNode:TTreeNode;
    WorkingHotDirEntry:THotDir;
    NewTreeNode:TTreeNode;
  begin
    while WorkingTreeNode<>nil do
    begin
      MaybeChildNode:=WorkingTreeNode.GetFirstChild;

      if MaybeChildNode<>nil then
      begin
        if (THotDir(WorkingTreeNode.Data).GroupNumber = 1) OR (FlagAddThemAll) then
        begin
          WorkingHotDirEntry:=THotDir.Create;
          THotDir(WorkingTreeNode.Data).CopyToHotDir(WorkingHotDirEntry);
          WorkingHotDirEntry.Dispatcher:=hd_STARTMENU; //Probably not necessary, but let's make sure it will start a menu
          Add(WorkingHotDirEntry);
          if ParamWorkingTreeView<>nil then
          begin
            NewTreeNode := ParamWorkingTreeView.Items.AddChildObject(InsertionNodePlace, WorkingHotDirEntry.HotDirName,HotDir[count-1]);
            NewTreeNode.ImageIndex:=ICONINDEX_SUBMENU;
            NewTreeNode.SelectedIndex:=ICONINDEX_SUBMENU;
            NewTreeNode.StateIndex:=ICONINDEX_SUBMENU;
          end;
          inc(result);
        end;

        RecursiveAddTheOnesWithGroupNumberOne(MaybeChildNode,NewTreeNode);

        if (THotDir(WorkingTreeNode.Data).GroupNumber=1) OR (FlagAddThemAll) then
        begin
          WorkingHotDirEntry:=THotDir.Create;
          WorkingHotDirEntry.Dispatcher:=hd_ENDMENU;
          Add(WorkingHotDirEntry);
        end;
      end
      else
      begin
        if (THotDir(WorkingTreeNode.Data).GroupNumber=1) OR (FlagAddThemAll) then
        begin
          WorkingHotDirEntry:=THotDir.Create;
          THotDir(WorkingTreeNode.Data).CopyToHotDir(WorkingHotDirEntry);
          Add(WorkingHotDirEntry);
          if ParamWorkingTreeView<>nil then
          begin
            case WorkingHotDirEntry.Dispatcher of
              hd_Separator: NewTreeNode := ParamWorkingTreeView.Items.AddChildObject(InsertionNodePlace, HOTLIST_SEPARATORSTRING, HotDir[count-1]);
              else NewTreeNode := ParamWorkingTreeView.Items.AddChildObject(InsertionNodePlace, WorkingHotDirEntry.HotDirName, HotDir[count-1]);
            end;
          end;
          inc(result);
        end;
      end;

      WorkingTreeNode:=WorkingTreeNode.GetNextSibling;
    end;
  end;

  procedure RecursiveSetGroupNumberToOne(WorkingTreeNode:TTreeNode; FlagTakeAlsoSibling:boolean);
  begin
    repeat
      if WorkingTreeNode.GetFirstChild=nil then
      begin
        if (THotDir(WorkingTreeNode.Data).Dispatcher<>hd_STARTMENU) AND (THotDir(WorkingTreeNode.Data).Dispatcher<>hd_ENDMENU) then
        begin
          THotDir(WorkingTreeNode.Data).GroupNumber:=1;
        end;
      end
      else
      begin
        THotDir(WorkingTreeNode.Data).GroupNumber:=1;
        RecursiveSetGroupNumberToOne(WorkingTreeNode.GetFirstChild,TRUE);
      end;

      if FlagTakeAlsoSibling then WorkingTreeNode:=WorkingTreeNode.GetNextSibling;
    until (FlagTakeAlsoSibling=FALSE) OR (WorkingTreeNode=nil);
  end;

var
  OutsideIndex:integer;
begin
  result:=0;

  //First, make sure no one is marked
  for OutsideIndex:=0 to pred(ParamTreeViewToImport.Items.Count) do THotDir(ParamTreeViewToImport.Items.Item[OutsideIndex].Data).GroupNumber:=0;

  //Then, set the "GroupNumber" to 1 to the ones to import
  if ParamTreeViewToImport.SelectionCount>0 then for OutsideIndex:=0 to pred(ParamTreeViewToImport.SelectionCount) do RecursiveSetGroupNumberToOne(ParamTreeViewToImport.Selections[OutsideIndex],FALSE);

  //Finally now collect the one with the "GroupNumber" set to 1.
  RecursiveAddTheOnesWithGroupNumberOne(ParamTreeViewToImport.Items.Item[0],nil);
end;

{ TDirectoryHotlist.ComputeSignature }
// Routine tries to pickup all char chain from element of directory hotlist and compute a unique CRC32.
// This CRC32 will bea kind of signature of the directory hotlist.
function TDirectoryHotlist.ComputeSignature(Seed:dword):dword;
var
  Index:integer;
begin
  result:=Seed;
  for Index := 0 to pred(Count) do
  begin
    Result := crc32(Result,@HotDir[Index].Dispatcher,1);
    if length(HotDir[Index].HotDirName)>0 then Result := crc32(Result,@HotDir[Index].HotDirName[1],length(HotDir[Index].HotDirName));
    if length(HotDir[Index].HotDirPath)>0 then Result := crc32(Result,@HotDir[Index].HotDirPath[1],length(HotDir[Index].HotDirPath));
    Result := crc32(Result,@HotDir[Index].HotDirPathSort,4);
    if length(HotDir[Index].HotDirTarget)>0 then Result := crc32(Result,@HotDir[Index].HotDirTarget[1],length(HotDir[Index].HotDirTarget));
    Result := crc32(Result,@HotDir[Index].HotDirTargetSort,4);
    Result := crc32(Result,@HotDir[Index].HotDirExisting,1);
    Result := crc32(Result,@HotDir[Index].GroupNumber,4);
  end;
end;

{ TDirectoryHotlist.GetHotDir }
function TDirectoryHotlist.GetHotDir(Index: integer): THotDir;
begin
  Result := THotDir(Items[Index]);
end;

{ TDirectoryHotlist.RefreshFromTTreeView }
//The routine will recreate the complete TDirectoryHotlist from a TTreeView.
//It cannot erase or replace immediately the current list because the TTreeView refer to it!
//So it create it into the "TransitDirectoryHotlist" and then, it will copy it to self one.
//
procedure TDirectoryHotlist.RefreshFromTTreeView(ParamTreeView:TTreeView);
var
  TransitDirectoryHotlist:TDirectoryHotlist;
  IndexToTryToRestore:longint=-1;
  MaybeTTreeNodeToSelect:TTreeNode;

  procedure RecursiveEncapsulateSubMenu(WorkingTreeNode:TTreeNode);
  var
    MaybeChildNode:TTreeNode;
    WorkingHotDirEntry:THotDir;
  begin
    while WorkingTreeNode<>nil do
    begin
      if WorkingTreeNode=ParamTreeView.Selected then IndexToTryToRestore:=TransitDirectoryHotlist.Count;

      MaybeChildNode:=WorkingTreeNode.GetFirstChild;
      if MaybeChildNode<>nil then
      begin
        WorkingHotDirEntry:=THotDir.Create;
        THotDir(WorkingTreeNode.Data).CopyToHotDir(WorkingHotDirEntry);
        WorkingHotDirEntry.Dispatcher:=hd_STARTMENU; //Probably not necessary, but let's make sure it will start a menu
        TransitDirectoryHotlist.Add(WorkingHotDirEntry);

        RecursiveEncapsulateSubMenu(MaybeChildNode);

        WorkingHotDirEntry:=THotDir.Create;
        WorkingHotDirEntry.Dispatcher:=hd_ENDMENU;
        TransitDirectoryHotlist.Add(WorkingHotDirEntry);
      end
      else
      begin
        //We won't copy EMPTY submenu so that's why we check for "hd_STARTMENU". And the check for "hd_ENDMENU" is simply probably unecessary protection
        if (THotDir(WorkingTreeNode.Data).Dispatcher<>hd_STARTMENU) AND (THotDir(WorkingTreeNode.Data).Dispatcher<>hd_ENDMENU) then
        begin
          WorkingHotDirEntry:=THotDir.Create;
          THotDir(WorkingTreeNode.Data).CopyToHotDir(WorkingHotDirEntry);
          TransitDirectoryHotlist.Add(WorkingHotDirEntry);
        end;
      end;
      WorkingTreeNode:=WorkingTreeNode.GetNextSibling;
    end;
  end;

begin
  if ParamTreeView.Items.count>0 then
  begin
    TransitDirectoryHotlist:=TDirectoryHotlist.Create;
    try
      RecursiveEncapsulateSubMenu(ParamTreeView.Items.Item[0]);
      TransitDirectoryHotlist.CopyDirectoryHotlistToDirectoryHotlist(self);
      MaybeTTreeNodeToSelect:=LoadTTreeView(ParamTreeView,IndexToTryToRestore);
      if MaybeTTreeNodeToSelect<>nil then MaybeTTreeNodeToSelect.Selected:=TRUE else if ParamTreeView.Items.count>0 then ParamTreeView.Items.Item[0].Selected:=TRUE;
    finally
      TransitDirectoryHotlist.Clear;
      TransitDirectoryHotlist.Free;
    end;
  end
  else
  begin
    Self.Clear;
  end;
end;

{ TCheckDrivePresenceThread.Create }
constructor TCheckDrivePresenceThread.Create(sDrive: string; ParamListOfNonExistingDrive: TStringList; var ThreadCount: longint);
begin
  FListOfNonExistingDrive := ParamListOfNonExistingDrive;
  FDriveToSearchFor := sDrive;
  FThreadCountPoint := addr(ThreadCount);
  FreeOnTerminate := True;
  inherited Create(False);
end;

{ TCheckDrivePresenceThread.Destroy }
destructor TCheckDrivePresenceThread.Destroy;
begin
  inherited Destroy;
end;

{TCheckDrivePresenceThread.Execute}
procedure TCheckDrivePresenceThread.Execute;
begin
  if FDriveToSearchFor = '' then
  begin
    Synchronize(@Self.ReportPresentInTheThread);
  end
  else
  begin
    if mbDirectoryExists(FDriveToSearchFor) then
    begin
      Synchronize(@Self.ReportPresentInTheThread);
    end
    else
    begin
      Synchronize(@Self.ReportNotPresentInTheThread);
    end;
  end;
  Terminate;
end;

{ TCheckDrivePresenceThread.ReportPresentInTheThread }
procedure TCheckDrivePresenceThread.ReportPresentInTheThread;
begin
  Dec(FThreadCountPoint^);
end;

{ TCheckDrivePresenceThread.ReportNotPresentInTheThread }
procedure TCheckDrivePresenceThread.ReportNotPresentInTheThread;
begin
  FListOfNonExistingDrive.Add(FDriveToSearchFor);
  Dec(FThreadCountPoint^);
end;

{$IFDEF MSWINDOWS}
{ TDirectoryHotlist.ImportTotalCommander }
function TDirectoryHotlist.ImportTotalCommander(TotalCommanderFilename: String): integer;
const
  CONFIGFILE_SECTIONNAME = 'DirMenu';
  CONFIGFILE_NAMEPREFIX = 'menu';
  CONFIGFILE_PATHPREFIX = 'cmd';
  CONFIGFILE_TARGETPREFIX = 'path';
var
  LocalHotDir: THotDir;
  ConfigFile: TIniFileEx;
  sName, sPath, sTarget: string;
  Index, CurrentMenuLevel, InitialNumberOfElement: longint;
  FlagAvortInsertion: boolean;
begin
  InitialNumberOfElement := Count;
  Index := 1;
  CurrentMenuLevel := 0;

  ConfigFile := TIniFileEx.Create(TotalCommanderFilename);

  try
    repeat
      sName := ConvertTCStringToString(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(Index), TERMINATORNOTPRESENT));

      if sName <> TERMINATORNOTPRESENT then
      begin
        sName := StringReplace(sName, '&', '', [rfReplaceAll, rfIgnoreCase]); //Let's remove the amperstand

        FlagAvortInsertion := False;
        LocalHotDir := THotDir.Create;

        if sName = '-' then //Was it a separator?
        begin
          LocalHotDir.Dispatcher := hd_SEPARATOR;
        end
        else
        begin
          if sName = '--' then //Was is a end of menu?
          begin
            LocalHotDir.Dispatcher := hd_ENDMENU;
            if CurrentMenuLevel > 0 then Dec(CurrentMenuLevel) else FlagAvortInsertion := True; //Sanity correction since Total Commande may contains extra end of menu...
          end
          else
          begin
            if (UTF8Length(sName) > 1) then //Was it a menu start?
            begin
              if (sName[1] = '-') and (sName[2] <> '-') then
              begin
                Inc(CurrentMenuLevel);
                LocalHotDir.Dispatcher := hd_STARTMENU;
                LocalHotDir.HotDirName := UTF8RightStr(sName, UTF8Length(sName) - 1);
              end;
            end;

            if LocalHotDir.Dispatcher = hd_NULL then
            begin
              LocalHotDir.HotDirName := sName;

              sPath := ReplaceTCEnvVars(ConvertTCStringToString(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME, CONFIGFILE_PATHPREFIX + IntToStr(Index), '')));
              if UTF8Length(sPath) > 3 then if UTF8Pos('cd ', UTF8LowerCase(sPath)) = 1 then sPath := UTF8Copy(sPath, 4, UTF8Length(sPath) - 3);

              if UTF8Pos('cm_', UTF8LowerCase(sPath)) = 0 then //Make sure it's not a command
              begin
                if sPath <> '' then sPath := IncludeTrailingPathDelimiter(sPath); //Not an obligation but DC convention seems to like a backslash at the end

                sTarget := ReplaceTCEnvVars(ConvertTCStringToString(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME, CONFIGFILE_TARGETPREFIX + IntToStr(Index), '')));
                if UTF8Length(sTarget) > 3 then if UTF8Pos('cd ', UTF8LowerCase(sTarget)) = 1 then sTarget := UTF8Copy(sTarget, 4, UTF8Length(sTarget) - 3);
                if sTarget <> '' then sTarget := IncludeTrailingPathDelimiter(sTarget); //Not an obligation but DC convention seems to like a backslash at the end

                LocalHotDir.Dispatcher := hd_CHANGEPATH;
                LocalHotDir.HotDirPath := sPath;
                LocalHotDir.HotDirTarget := sTarget;
              end
              else
              begin //If it's command, store it as a command
                LocalHotDir.Dispatcher := hd_COMMAND;
                LocalHotDir.HotDirPath := sPath;
              end;
            end;
          end;
        end;

        if not FlagAvortInsertion then Add(LocalHotDir) else LocalHotDir.Free;
        Inc(Index);
      end;
    until sName = TERMINATORNOTPRESENT;

    //Try to fix possible problem if the LAST MENU is not ending correctly...
    while CurrentMenuLevel > 0 do
    begin
      Dec(CurrentMenuLevel);
      LocalHotDir := THotDir.Create;
      LocalHotDir.Dispatcher := hd_ENDMENU;
      Add(LocalHotDir);
    end;
  finally
    ConfigFile.Free;
  end;
  Result := Count - InitialNumberOfElement;
end;

{ TDirectoryHotlist.ExportTotalCommander }
function TDirectoryHotlist.ExportTotalCommander(TotalCommanderFilename: String; FlagEraseOriginalOnes: boolean): boolean;
const
  CONFIGFILE_SECTIONNAME = 'DirMenu';
  CONFIGFILE_NAMEPREFIX = 'menu';
  CONFIGFILE_PATHPREFIX = 'cmd';
  CONFIGFILE_TARGETPREFIX = 'path';
  TERMINATORNOTPRESENT = ':-<#/?*+*?\#>-:';
var
  ConfigFile: TIniFileEx;
  Index, OffsetForOnesAlreadyThere: integer;
  sName: string;
  RememberCursor: TCursor;
begin
  Result := True;
  OffsetForOnesAlreadyThere := 0;

  try
    RememberCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      ConfigFile := TIniFileEx.Create(TotalCommanderFilename);
      try
        with ConfigFile do
        begin
          if FlagEraseOriginalOnes then
          begin
            EraseSection(CONFIGFILE_SECTIONNAME);
          end
          else
          begin
            Index := 1;
            repeat
              sName := ConfigFile.ReadString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(Index), TERMINATORNOTPRESENT);
              if sName <> TERMINATORNOTPRESENT then Inc(OffsetForOnesAlreadyThere);
              Inc(Index);
            until sName = TERMINATORNOTPRESENT;
          end;

          for Index := 0 to pred(Count) do
          begin
            case THotDir(HotDir[Index]).Dispatcher of
              hd_NULL:
              begin
              end;

              hd_CHANGEPATH:
              begin
                WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString(THotDir(HotDir[Index]).HotDirName));
                if THotDir(HotDir[Index]).HotDirPath <> '' then WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_PATHPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString('cd ' + ReplaceDCEnvVars(THotDir(HotDir[Index]).HotDirPath)));
                if THotDir(HotDir[Index]).HotDirTarget <> '' then WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_TARGETPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString(ReplaceDCEnvVars(THotDir(HotDir[Index]).HotDirTarget)));
              end;

              hd_SEPARATOR:
              begin
                WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), '-');
              end;

              hd_STARTMENU:
              begin
                //See the position of the '-'. It *must* be inside the parameter for calling "ConvertStringToTCString" because the expected utf8 signature of TC must be before the '-'.
                WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString('-' + THotDir(HotDir[Index]).HotDirName));
              end;

              hd_ENDMENU:
              begin
                WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), '--');
              end;

              hd_COMMAND:
              begin
                WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_NAMEPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString(THotDir(HotDir[Index]).HotDirName));
                if THotDir(HotDir[Index]).HotDirPath <> '' then WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_PATHPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString(THotDir(HotDir[Index]).HotDirPath));
                if THotDir(HotDir[Index]).HotDirTarget <> '' then WriteString(CONFIGFILE_SECTIONNAME, CONFIGFILE_TARGETPREFIX + IntToStr(OffsetForOnesAlreadyThere + Index + 1), ConvertStringToTCString(THotDir(HotDir[Index]).HotDirTarget));
              end;
            end;
          end;
        end;
        ConfigFile.UpdateFile;
      finally
        ConfigFile.Free;
      end;

    except
      Result := False;
    end;

  finally
    Screen.Cursor := RememberCursor;
  end;
end;
{$ENDIF}

end.

