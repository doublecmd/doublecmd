{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save/WorkingWith HotDir

   Copyright (C) 2009-2014  Alexander Koblov (alexx2000@mail.ru)

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

   -This unit has been added in 2014.
   -Inspired a lot from "usearchtemplate"
}

unit uHotDir;

{$mode objfpc}{$H+}

interface

uses
 //Lazarus, Free-Pascal, etc.
 Classes, SysUtils, Menus, extctrls, Controls, stdctrls,

 //DC
 DCClassesUtf8, DCXmlConfig, uFile, uFindFiles;

const
  cSectionOfHotDir = 'DirectoryHotList';

  ACTION_ADDTOHOTLIST = 1;
  ACTION_CONFIGTOHOTLIST = 2;

  TAGOFFSET_FORCHANGETOSPECIALDIR = $10000;

type
  { TKindOfHotDirEntry }
  TKindOfHotDirEntry = (hd_NULL, hd_CHANGEPATH, hd_SEPARATOR, hd_STARTMENU, hd_ENDMENU, hd_COMMAND);

  { TKindHotDirMenuPopulation }
  TKindHotDirMenuPopulation = (mpJUSTHOTDIRS,mpHOTDIRSWITHCONFIG,mpPATHHELPER);

  { TExistingState }
  TExistingState = (DirExistUnknown,DirExist,DirNotExist);

  { TProcedureWhenClickMenuItem}
  TProcedureWhenClickOnMenuItem = procedure(Sender: TObject) of object;

  { THotDir }
  THotDir = class
  private
    FDispatcher: TKindOfHotDirEntry;
    FMenuLevel: integer;
    FHotDirName: string;
    FHotDirPath: string;
    FHotDirPathSort: longint;
    FHotDirTarget: string;
    FHotDirTargetSort: longint;
    FHotDirExistingState: TExistingState;
  public
    constructor Create;
    property Dispatcher: TKindOfHotDirEntry read FDispatcher write FDispatcher;
    property MenuLevel: integer read FMenuLevel write FMenuLevel;
    property HotDirName: string read FHotDirName write FHotDirName;
    property HotDirPath: string read FHotDirPath write FHotDirPath;
    property HotDirPathSort: longint read FHotDirPathSort write FHotDirPathSort;
    property HotDirTarget: string read FHotDirTarget write FHotDirTarget;
    property HotDirTargetSort: longint read FHotDirTargetSort write FHotDirTargetSort;
    property HotDirExisting: TExistingState read FHotDirExistingState write FHotDirExistingState;
  end;

  { THotDirList }
  THotDirList = class(TList)
  private
    function GetHotDir(Index: Integer): THotDir;
  public
    FlagModified:boolean;
    constructor Create;
    procedure Clear; override;
    function Add(HotDir: THotDir): Integer;
    procedure DeleteHotDir(Index: Integer);
    procedure DeleteHotDirMenuDelimiters(Index,DeleteDispatcher:Integer);
    procedure LoadToStringList(StringList: TStrings);
    function TryToGetCloserHotDir(APath:string; paramActionDispatcher:integer):longint;
    function GetIndexSubMenuEnd(SearchIndex:longint):longint;
    procedure Move(CurIndex, NewIndex: Integer);
    function MoveHotDirMenu(CurIndex, NewIndex: Integer):longint;
    procedure PopulateMenuWithHotDir(mncmpMenuComponentToPopulate:TComponent; ProcedureWhenHotDirItemClicked,ProcedureWhenHotDirAddOrConfigClicked:TProcedureWhenClickOnMenuItem; CurrentPath:string; KindHotDirMenuPopulation:TKindHotDirMenuPopulation; TagOffset:longint);
    procedure LoadFromIni(IniFile: TIniFileEx);
    procedure SaveToIni(IniFile: TIniFileEx);
    procedure LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode; FlagEraseOriginalOnes:boolean);
    procedure ImportTotalCommander(TotalCommanderFilename:utf8string);
    function ExportTotalCommander(TotalCommanderFilename:utf8string; FlagEraseOriginalOnes:boolean):boolean;
    procedure ImportDoubleCommander(DoubleCommanderFilename:utf8string);
    function ExportDoubleCommander(DoubleCommanderFilename:utf8string; FlagEraseOriginalOnes:boolean):boolean;
    procedure RefreshExistingProperty(lsShowingIt:TListBox;lbleditShowingIt:TLabeledEdit; ScanMode:integer);
    procedure lsImportationHotDirClick(Sender: TObject);
    function AddFromAnotherListTheSelected(ImportantedHotDirList:THotDirList;lsHoldingSelection:TListBox):longint;
    function EliminateTheNonSelectedInList(lsHoldingSelection:TListBox):longint;
    procedure lsHotDirDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure CopyListToHotDirList(var DestinationHotDirList:THotDirList);
    property HotDir[Index: Integer]: THotDir read GetHotDir;
  end;

  { TCheckDrivePresenceThread }
  TCheckDrivePresenceThread = class(TThread)
  private
    FThread: TThread;
    FDriveToSearchFor:string;
    FListOfNonExistingDrive: TStringList;
    FThreadCountPoint: ^longint;
    procedure ReportNotPresentInTheThread;
    procedure ReportPresentInTheThread;
  protected
    procedure Execute; override;
  public
    constructor Create(sDrive:string; ParamListOfNonExistingDrive: TStringList; var ThreadCount:longint);
    destructor Destroy; override;
  end;

implementation

uses
  //Lazarus, Free-Pascal, etc.
 Graphics, Forms,

 //DC
 Dialogs, DCFileAttributes, DCBasicTypes, uDebug, DCStrUtils, uDCUtils,
 uLng, DCOSUtils, uGlobs, uSpecialDir;

{ THotDir.Create }
constructor THotDir.Create;
begin
  inherited Create;
  FDispatcher:=hd_NULL;
  FMenuLevel:=0;
  FHotDirName:='';
  FHotDirPath:='';
  FHotDirPathSort:=0;
  FHotDirTarget:='';
  FHotDirTargetSort:=0;
  FHotDirExistingState:=DirExistUnknown;
end;

{ THotDirList.Create }
constructor THotDirList.Create;
begin
  inherited Create;
  FlagModified:=FALSE;
end;

{ THotDirList.Clear }
procedure THotDirList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do HotDir[i].Free;
  inherited Clear;
end;

{ THotDirList.Add }
function THotDirList.Add(HotDir: THotDir): Integer;
begin
  Result:= inherited Add(HotDir);
  FlagModified:=TRUE;
end;

{ THotDirList.DeleteHotDir }
procedure THotDirList.DeleteHotDir(Index: Integer);
begin
  HotDir[Index].Free;
  Delete(Index);
  FlagModified:=TRUE;
end;

{ THotDirList.DeleteHotDirMenuDelimiters }
procedure THotDirList.DeleteHotDirMenuDelimiters(Index,DeleteDispatcher:Integer);
var
  EndIndex,OtherItemIndex,Answer:integer;
begin
  if HotDir[Index].Dispatcher=hd_STARTMENU then
  begin
    EndIndex:=GetIndexSubMenuEnd(Index);

    case DeleteDispatcher of
      1: Answer:=MessageDlg(rsMsgHotDirWhatToDelete, mtConfirmation,[mbYes,mbNo,mbCancel],0);
      2: Answer:=mrNo;
      3: Answer:=mrYes;
      else Answer:=mrCancel; //Should not happen, but just in case
    end;

    case Answer of
      mrYes:
        begin
          for OtherItemIndex:=EndIndex downto Index do
            begin
              HotDir[OtherItemIndex].Free;
              Delete(OtherItemIndex);
            end;
          FlagModified:=TRUE;
        end;

      mrNo:
        begin
          for OtherItemIndex:=(Index+1) to (EndIndex-1) do HotDir[OtherItemIndex].MenuLevel:=HotDir[OtherItemIndex].MenuLevel-1;
          HotDir[EndIndex].Free;
          Delete(EndIndex);
          HotDir[Index].Free;
          Delete(Index);
          FlagModified:=TRUE;
        end;

      mrCancel:
        begin
        end;
    end; //case Answer of
  end; //if HotDir[Index].Dispatcher=hd_STARTMENU then
end;

{ THotDirList.LoadToStringList }
procedure THotDirList.LoadToStringList(StringList: TStrings);
var
  I: Integer;
begin
  StringList.Clear;
  for I:= 0 to Count - 1 do StringList.Add('');
end;

{ THotDirList.TryToGetCloserHotDir }
function THotDirList.TryToGetCloserHotDir(APath:string; paramActionDispatcher:integer):longint;
var
  PerfectMatchIndex,I,SecondAlternative,SecondAlternativeLengthRecord:Integer;
  InitialPath,AttemptingPath:string;
begin
  InitialPath:=lowercase(IncludeTrailingPathDelimiter(APath));
  APath:=InitialPath;
  SecondAlternative:=-1;
  SecondAlternativeLengthRecord:=0;

  PerfectMatchIndex:=-1;

  repeat
    I:=0;

    while (I < Count) and (PerfectMatchIndex=-1) do
    begin
      if HotDir[I].Dispatcher=hd_CHANGEPATH then
      begin
        AttemptingPath:=lowercase(ExpandFileName(ReplaceEnvVars(HotDir[I].HotDirPath)));

        if APath=AttemptingPath then
        begin
          case paramActionDispatcher of
            ACTION_CONFIGTOHOTLIST: PerfectMatchIndex:=I;
            ACTION_ADDTOHOTLIST: PerfectMatchIndex:=I+1;
          end;
        end
        else
        begin
          if (pos(APath,AttemptingPath)=1) and (length(APath)>=SecondAlternativeLengthRecord) then
          begin
            SecondAlternativeLengthRecord:=length(APath);

            if SecondAlternative=-1 then
            begin
              if (InitialPath<AttemptingPath) OR (paramActionDispatcher=ACTION_CONFIGTOHOTLIST) then
              begin
                SecondAlternative:=I;
              end
              else
              begin
                if (I+1)<Count then SecondAlternative:=(I+1);
              end;
            end
            else
            begin
              if (InitialPath>AttemptingPath) AND (paramActionDispatcher=ACTION_ADDTOHOTLIST) then
              begin
                if (I+1)<Count then SecondAlternative:=(I+1);
              end;
            end;
          end;
        end;
      end;
      inc(I);
     end; //while (I < Count) and (PerfectMatchIndex=-1) do

    if PerfectMatchIndex=-1 then APath:=ExcludeTrailingPathDelimiter(GetParentDir(APath));
  until (PerfectMatchIndex<>-1) OR (APath='');

  if PerfectMatchIndex<>-1 then result:=PerfectMatchIndex else if SecondAlternative<>-1 then result:=SecondAlternative else result:=-1;
end;

{ THotDirList.GetIndexSubMenuEnd }
{ Given the start of a menu, it will return the index of the end of the same menu}
function THotDirList.GetIndexSubMenuEnd(SearchIndex:longint):longint;
var
  InnerMenuCount:longint;
begin
  result:=-1;
  if HotDir[SearchIndex].Dispatcher=hd_STARTMENU then
  begin
    InnerMenuCount:=1;
    while (SearchIndex<pred(count)) and (InnerMenuCount<>0) do
    begin
      inc(SearchIndex);

      if HotDir[SearchIndex].Dispatcher=hd_STARTMENU then
        inc(InnerMenuCount)
      else
        if HotDir[SearchIndex].Dispatcher=hd_ENDMENU then dec(InnerMenuCount);
    end;

    if InnerMenuCount=0 then result:=SearchIndex;
  end;
end;

{ THotDirList.Move }
procedure THotDirList.Move(CurIndex, NewIndex: Integer);
begin
  inherited Move(CurIndex, NewIndex);

  if NewIndex=0 then
  begin
    HotDir[NewIndex].MenuLevel:=0;
  end
  else
  begin
    case HotDir[NewIndex-1].Dispatcher of
      hd_STARTMENU:
        begin
          case HotDir[NewIndex].Dispatcher of
            hd_ENDMENU: HotDir[NewIndex].MenuLevel:=HotDir[NewIndex-1].MenuLevel;
            else HotDir[NewIndex].MenuLevel:=HotDir[NewIndex-1].MenuLevel+1;
          end;
        end;
      else
        begin
          case HotDir[NewIndex].Dispatcher of
            hd_ENDMENU: HotDir[NewIndex].MenuLevel:=HotDir[NewIndex-1].MenuLevel-1;
            else HotDir[NewIndex].MenuLevel:=HotDir[NewIndex-1].MenuLevel;
          end;
        end;
    end;
  end;
  FlagModified:=TRUE;
end;

{ MoveHotDirMenu.MoveHotDirMenu }
function THotDirList.MoveHotDirMenu(CurIndex, NewIndex: Integer):Integer;
var
  HotDirSubMenuEndIndex,OffsetIndex:longint;
begin
  result:=-1;
  HotDirSubMenuEndIndex:=GetIndexSubMenuEnd(CurIndex);
  if HotDirSubMenuEndIndex<>-1 then
  begin
    if (NewIndex<CurIndex) OR (NewIndex>HotDirSubMenuEndIndex) then
    begin
      if NewIndex<CurIndex then
      begin
        for OffsetIndex:=0 to (HotDirSubMenuEndIndex-CurIndex) do Move(CurIndex+OffsetIndex, NewIndex+OffsetIndex);
      end
      else
      begin
        for OffsetIndex:=0 to (HotDirSubMenuEndIndex-CurIndex) do Move(CurIndex, NewIndex);
      end;
      FlagModified:=TRUE;
    end;
  end;
end;

{ THotDirList.PopulateMenuWithHotDir }
procedure THotDirList.PopulateMenuWithHotDir(mncmpMenuComponentToPopulate:TComponent; ProcedureWhenHotDirItemClicked,ProcedureWhenHotDirAddOrConfigClicked:TProcedureWhenClickOnMenuItem; CurrentPath:string; KindHotDirMenuPopulation:TKindHotDirMenuPopulation; TagOffset:longint);
var
  I:longint; //Same variable for main and local routine
  FlagPathAlreadyInMenu:boolean;
  CurrentPathToSearch:string;

  //Warning: "CompleteMenu" is recursive and call itself.
  function CompleteMenu(ParamMenuItem:TMenuItem):longint;
    var
      localmi:TMenuItem;
      LocalLastAdditionIsASeparator:boolean;
    begin
      result:=0;
      LocalLastAdditionIsASeparator:=FALSE;
      while I<Count do
      begin
        inc(I);

        case HotDir[I-1].Dispatcher of
          hd_CHANGEPATH:
            begin
              case HotDir[I-1].HotDirExisting of
                DirExistUnknown,DirExist:
                  begin
                    localmi:= TMenuItem.Create(ParamMenuItem);
                    localmi.Caption:=HotDir[I-1].HotDirName;
                    localmi.tag:= (I-1)+TagOffset;
                    localmi.OnClick:=ProcedureWhenHotDirItemClicked;
                    ParamMenuItem.Add(localmi);
                    if CurrentPathToSearch=UpperCase(mbExpandFileName(HotDir[I-1].FHotDirPath)) then FlagPathAlreadyInMenu:=TRUE;
                    LocalLastAdditionIsASeparator:=FALSE;
                    inc(result);
                  end;
              end;
            end;

          hd_NULL,hd_COMMAND:
            begin
              if KindHotDirMenuPopulation<>mpPATHHELPER then
              begin
                localmi:= TMenuItem.Create(ParamMenuItem);
                localmi.Caption:=HotDir[I-1].HotDirName;
                localmi.tag:= (I-1)+TagOffset;
                localmi.OnClick:=ProcedureWhenHotDirItemClicked;
                ParamMenuItem.Add(localmi);
                LocalLastAdditionIsASeparator:=FALSE;
                inc(result);
              end;
            end;

          hd_SEPARATOR:
            begin
              if (ParamMenuItem.Count>0) AND (not LocalLastAdditionIsASeparator) then
              begin
                localmi:= TMenuItem.Create(ParamMenuItem);
                localmi.Caption:='-';
                ParamMenuItem.Add(localmi);
                LocalLastAdditionIsASeparator:=TRUE;
                inc(result);
              end;
            end;

          hd_STARTMENU:
            begin
              localmi:= TMenuItem.Create(ParamMenuItem);
              localmi.Caption:=HotDir[I-1].HotDirName;
              ParamMenuItem.Add(localmi);
              CompleteMenu(localmi);
              if localmi.Count<>0 then
              begin
                LocalLastAdditionIsASeparator:=FALSE;
                inc(result);
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
                  dec(result);
                end;
              exit;
            end;
        end; //case HotDir[I-1].Dispatcher of
      end; //while I<Count do
    end;
var
  miMainTree: TMenuItem;
  LastAdditionIsASeparator:boolean;
  NumberOfElementsSoFar:longint;
begin
  // Create All popup menu
  CurrentPathToSearch:=UpperCase(mbExpandFileName(CurrentPath));
  FlagPathAlreadyInMenu:=FALSE;
  LastAdditionIsASeparator:=FALSE;

  case KindHotDirMenuPopulation of
    mpJUSTHOTDIRS,mpHOTDIRSWITHCONFIG: if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Clear;
  end;

  I:=0;
  while I<Count do
  begin
    inc(I);

    case HotDir[I-1].Dispatcher of
      hd_CHANGEPATH:
        begin
          case HotDir[I-1].HotDirExisting of
            DirExistUnknown,DirExist:
              begin
                miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
                miMainTree.Caption:=HotDir[I-1].HotDirName;
                miMainTree.tag:= (I-1)+TagOffset;
                miMainTree.OnClick:=ProcedureWhenHotDirItemClicked;

                if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
                  else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

                if CurrentPathToSearch=UpperCase(mbExpandFileName(HotDir[I-1].FHotDirPath)) then FlagPathAlreadyInMenu:=TRUE;
                LastAdditionIsASeparator:=FALSE;
              end;
          end;
        end;

      hd_NULL,hd_COMMAND:
        begin
          miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
          miMainTree.Caption:=HotDir[I-1].HotDirName;
          miMainTree.tag:= (I-1)+TagOffset;
          miMainTree.OnClick:=ProcedureWhenHotDirItemClicked;

          if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
            else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

          LastAdditionIsASeparator:=FALSE;
        end;

      hd_SEPARATOR:
        begin
          if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then NumberOfElementsSoFar:=TPopupMenu(mncmpMenuComponentToPopulate).Items.Count
            else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then NumberOfElementsSoFar:=TMenuItem(mncmpMenuComponentToPopulate).Count;
          if (NumberOfElementsSoFar>0) AND (not LastAdditionIsASeparator) then
          begin
            miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
            miMainTree.Caption:='-';
            if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
              else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
            LastAdditionIsASeparator:=TRUE;
          end;
        end;

      hd_STARTMENU:
        begin
          miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
          miMainTree.Caption:=HotDir[I-1].HotDirName;
          if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
            else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
          CompleteMenu(miMainTree);
          if miMainTree.Count<>0 then
          begin
            LastAdditionIsASeparator:=TRUE;
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
              if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items[pred(TPopupMenu(mncmpMenuComponentToPopulate).Items.Count)].Free
                else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Items[pred(TPopupMenu(mncmpMenuComponentToPopulate).Items.Count)].Free;
            end;
        end;
    end;
  end;

  case KindHotDirMenuPopulation of
    mpHOTDIRSWITHCONFIG:
      begin
        // now add delimiter
        miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption:= '-';
        if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
          else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        // Let's add the "Special path" in a context of change directory
        gSpecialDirList.PopulateMenuWithSpecialDir(mncmpMenuComponentToPopulate,mp_CHANGEDIR,ProcedureWhenHotDirItemClicked);

        // now add delimiter
        miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption:= '-';
        if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
          else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);

        // now add the "add current path" (id not already present)
        if not FlagPathAlreadyInMenu then
        begin
          miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
          with Application.MainForm as TForm do miMainTree.Caption:=rsMsgHotDirAddThisDirectory+MinimizeFilePath(CurrentPath,Canvas,250);
          miMainTree.Tag:=ACTION_ADDTOHOTLIST;
          miMainTree.OnClick:=ProcedureWhenHotDirAddOrConfigClicked;
          if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
            else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
        end;

        // now add configure item
        miMainTree:= TMenuItem.Create(mncmpMenuComponentToPopulate);
        miMainTree.Caption:= rsMsgHotDirConfigHotlist;
        miMainTree.Tag:=ACTION_CONFIGTOHOTLIST;
        miMainTree.OnClick:= ProcedureWhenHotDirAddOrConfigClicked;
        if mncmpMenuComponentToPopulate.ClassType=TPopupMenu then TPopupMenu(mncmpMenuComponentToPopulate).Items.Add(miMainTree)
          else if mncmpMenuComponentToPopulate.ClassType=TMenuItem then TMenuItem(mncmpMenuComponentToPopulate).Add(miMainTree);
      end;
  end; //case KindHotDirMenuPopulation of
end;

{ THotDirList.LoadFromIni }
procedure THotDirList.LoadFromIni(IniFile: TIniFileEx);
begin
  Clear;
end;

{ THotDirList.SaveToIni }
procedure THotDirList.SaveToIni(IniFile: TIniFileEx);
begin
  IniFile.EraseSection(cSectionOfHotDir);
end;

{ THotDirList.LoadFromXml }
{ Information are stored like originally DC was storing them WITH addition of menu related info in a simular way TC. }
procedure THotDirList.LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  sName, sPath: String;
  LocalHotDir: THotDir;
  CurrentMenuLevel:integer;
  FlagAvortInsertion:boolean;
begin
  Clear;

  CurrentMenuLevel:=0;

  ANode := ANode.FindNode(cSectionOfHotDir);
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('HotDir') = 0 then
      begin
        if AConfig.TryGetAttr(ANode, 'Name', sName) and
           AConfig.TryGetAttr(ANode, 'Path', sPath) then
        begin
          FlagAvortInsertion:=FALSE;
          LocalHotDir:=THotDir.Create;

          if sName='-' then
          begin
            LocalHotDir.Dispatcher:=hd_SEPARATOR;
            LocalHotDir.MenuLevel:=CurrentMenuLevel;
          end
          else
          begin
            if sName='--' then
            begin
              LocalHotDir.Dispatcher:=hd_ENDMENU;
              if CurrentMenuLevel>0 then dec(CurrentMenuLevel) else FlagAvortInsertion:=TRUE; //Sanity correction in case we got correcupted from any ways
              LocalHotDir.MenuLevel:=CurrentMenuLevel;
            end
            else
            begin
              if (length(sName)>1) then
              begin
                if (sName[1]='-') AND (sName[2]<>'-') then
                begin
                  LocalHotDir.MenuLevel:=CurrentMenuLevel;
                  inc(CurrentMenuLevel);
                  LocalHotDir.Dispatcher:=hd_STARTMENU;
                  LocalHotDir.HotDirName:=rightstr(sName,length(sName)-1);
                end;
              end;

              if LocalHotDir.Dispatcher=hd_NULL then
              begin
                LocalHotDir.HotDirName:=sName;
                LocalHotDir.HotDirPath:=sPath;
                if pos('cm_',lowercase(sPath))=0 then
                begin
                  LocalHotDir.HotDirPathSort:=AConfig.GetAttr(Anode, 'PathSort',0);
                  LocalHotDir.HotDirTarget:=AConfig.GetAttr(ANode, 'Target', '');
                  LocalHotDir.HotDirTargetSort:=AConfig.GetAttr(Anode, 'TargetSort',0);
                  LocalHotDir.Dispatcher:=hd_CHANGEPATH;
                end
                else
                begin
                  LocalHotDir.Dispatcher:=hd_COMMAND;
                end;
                LocalHotDir.MenuLevel:=CurrentMenuLevel;
              end;
            end;
          end;

          if not FlagAvortInsertion then Add(LocalHotDir) else LocalHotDir.Free;
        end
        else
        begin
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
        end;
      end;

      ANode := ANode.NextSibling;
    end;

    //Try to fix possible problem if the LAST MENU is not ending correctly...
    while CurrentMenuLevel>0 do
      begin
        dec(CurrentMenuLevel);
        LocalHotDir:=THotDir.Create;
        LocalHotDir.Dispatcher:=hd_ENDMENU;
        LocalHotDir.MenuLevel:=CurrentMenuLevel;
        Add(LocalHotDir);
      end;

  end;
end;

{ THotDirList.SaveToXml }
{ Information are stored like originally DC was storing them WITH addition of menu related info in a simular way TC. }
procedure THotDirList.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode; FlagEraseOriginalOnes:boolean);
var
  Index: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, cSectionOfHotDir, True);
  if FlagEraseOriginalOnes then AConfig.ClearNode(ANode);
  for Index:= 0 to pred(Count) do
    begin
      SubNode := AConfig.AddNode(ANode, 'HotDir');

      case THotDir(HotDir[Index]).Dispatcher of
        hd_NULL:
          begin
            AConfig.SetAttr(SubNode, 'Name', '');
            AConfig.SetAttr(SubNode, 'Path', '');
            AConfig.SetAttr(SubNode, 'Target', '');
          end;

        hd_CHANGEPATH:
          begin
            AConfig.SetAttr(SubNode, 'Name', HotDir[Index].HotDirName);
            if THotDir(HotDir[Index]).HotDirPath<>'' then AConfig.SetAttr(SubNode, 'Path', HotDir[Index].HotDirPath) else AConfig.SetAttr(SubNode, 'Path', '');
            AConfig.SetAttr(SubNode, 'PathSort', HotDir[Index].HotDirPathSort);
            if THotDir(HotDir[Index]).HotDirTarget<>'' then AConfig.SetAttr(SubNode, 'Target', HotDir[Index].HotDirTarget) else AConfig.SetAttr(SubNode, 'Target', '');
            AConfig.SetAttr(SubNode, 'TargetSort', HotDir[Index].HotDirTargetSort);
          end;

        hd_SEPARATOR:
          begin
            AConfig.SetAttr(SubNode, 'Name', '-');
            AConfig.SetAttr(SubNode, 'Path', '');
            AConfig.SetAttr(SubNode, 'Target', '');
          end;

        hd_STARTMENU:
          begin
            AConfig.SetAttr(SubNode, 'Name', '-'+THotDir(HotDir[Index]).HotDirName);
            AConfig.SetAttr(SubNode, 'Path', '');
            AConfig.SetAttr(SubNode, 'Target', '');
          end;

        hd_ENDMENU:
          begin
            AConfig.SetAttr(SubNode, 'Name', '--');
            AConfig.SetAttr(SubNode, 'Path', '');
            AConfig.SetAttr(SubNode, 'Target', '');
          end;

        hd_COMMAND:
          begin
            AConfig.SetAttr(SubNode, 'Name', HotDir[Index].HotDirName);
            if THotDir(HotDir[Index]).HotDirPath<>'' then AConfig.SetAttr(SubNode, 'Path', HotDir[Index].HotDirPath) else AConfig.SetAttr(SubNode, 'Path', '');
            if THotDir(HotDir[Index]).HotDirTarget<>'' then AConfig.SetAttr(SubNode, 'Target', HotDir[Index].HotDirTarget) else AConfig.SetAttr(SubNode, 'Target', '');
          end;
      end;
    end;
end;

{ THotDirList.ImportTotalCommander }
procedure THotDirList.ImportTotalCommander(TotalCommanderFilename:utf8string);
const
  CONFIGFILE_SECTIONNAME = 'DirMenu';
  CONFIGFILE_NAMEPREFIX = 'menu';
  CONFIGFILE_PATHPREFIX = 'cmd';
  CONFIGFILE_TARGETPREFIX = 'path';
  TERMINATORNOTPRESENT = ':-<#/?*+*?\#>-:';
var
  LocalHotDir:THotDir;
  ConfigFile:TIniFileEx;
  sName, sPath, sTarget:UTF8string;
  Index,CurrentMenuLevel:longint;
  FlagAvortInsertion:boolean;
begin
  Index:=1;
  CurrentMenuLevel:=0;

  ConfigFile:=TIniFileEx.Create(TotalCommanderFilename);

  try
    repeat
      sName:=AnsiToUTF8(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(Index),TERMINATORNOTPRESENT));
      if sName<>TERMINATORNOTPRESENT then
      begin
        sName:=StringReplace(sName,'&','',[rfReplaceAll, rfIgnoreCase]); //Let's remove the amperstand

        FlagAvortInsertion:=FALSE;
        LocalHotDir:=THotDir.Create;

        if sName='-' then //Was it a separator?
        begin
          LocalHotDir.Dispatcher:=hd_SEPARATOR;
          LocalHotDir.MenuLevel:=CurrentMenuLevel;
        end
        else
        begin
          if sName='--' then //Was is a end of menu?
          begin
            LocalHotDir.Dispatcher:=hd_ENDMENU;
            if CurrentMenuLevel>0 then dec(CurrentMenuLevel) else FlagAvortInsertion:=TRUE; //Sanity correction since Total Commande may contains extra end of menu...
            LocalHotDir.MenuLevel:=CurrentMenuLevel;
          end
          else
          begin
            if (length(sName)>1) then //Was it a menu start?
            begin
              if (sName[1]='-') AND (sName[2]<>'-') then
              begin
                LocalHotDir.MenuLevel:=CurrentMenuLevel;
                inc(CurrentMenuLevel);
                LocalHotDir.Dispatcher:=hd_STARTMENU;
                LocalHotDir.HotDirName:=rightstr(sName,length(sName)-1);
              end;
            end;

            if LocalHotDir.Dispatcher=hd_NULL then
            begin
              LocalHotDir.MenuLevel:=CurrentMenuLevel;
              LocalHotDir.HotDirName:=sName;

              sPath:=AnsiToUTF8(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME,CONFIGFILE_PATHPREFIX+IntToStr(Index),''));
              if length(sPath)>3 then if pos('cd ',lowercase(sPath))=1 then sPath:=copy(sPath,4,length(sPath)-3);
              sPath:=StringReplace(sPath,'%COMMANDER_PATH%','%commander_path%',[rfReplaceAll, rfIgnoreCase]);

              if pos('cm_',LowerCase(sPath))=0 then //Make sure it's not a command
              begin
                if sPath<>'' then sPath:=IncludeTrailingPathDelimiter(sPath); //Not an obligation but DC convention seems to like a backslash at the end

                sTarget:=AnsiToUTF8(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME,CONFIGFILE_TARGETPREFIX+IntToStr(Index),''));
                if length(sTarget)>3 then if pos('cd ',lowercase(sTarget))=1 then sTarget:=copy(sTarget,4,length(sTarget)-3);
                sTarget:=StringReplace(sTarget,'%COMMANDER_PATH%','%commander_path%',[rfReplaceAll, rfIgnoreCase]);
                if sTarget<>'' then sTarget:=IncludeTrailingPathDelimiter(sTarget); //Not an obligation but DC convention seems to like a backslash at the end

                LocalHotDir.Dispatcher:=hd_CHANGEPATH;
                LocalHotDir.HotDirPath:=sPath;
                LocalHotDir.HotDirTarget:=sTarget;
              end
              else
              begin //If it's command, store it as a command
                LocalHotDir.Dispatcher:=hd_COMMAND;
                LocalHotDir.HotDirPath:=sPath;
              end;
            end;
          end;
        end;

        if not FlagAvortInsertion then Add(LocalHotDir) else LocalHotDir.Free;
        inc(Index);
      end;
    until sName=TERMINATORNOTPRESENT;

    //Try to fix possible problem if the LAST MENU is not ending correctly...
    while CurrentMenuLevel>0 do
    begin
      dec(CurrentMenuLevel);
      LocalHotDir:=THotDir.Create;
      LocalHotDir.Dispatcher:=hd_ENDMENU;
      LocalHotDir.MenuLevel:=CurrentMenuLevel;
      Add(LocalHotDir);
    end;
  finally
    ConfigFile.Free;
  end;
end;

{ THotDirList.ExportTotalCommander }
function THotDirList.ExportTotalCommander(TotalCommanderFilename:utf8string; FlagEraseOriginalOnes:boolean):boolean;
const
  CONFIGFILE_SECTIONNAME = 'DirMenu';
  CONFIGFILE_NAMEPREFIX = 'menu';
  CONFIGFILE_PATHPREFIX = 'cmd';
  CONFIGFILE_TARGETPREFIX = 'path';
  TERMINATORNOTPRESENT = ':-<#/?*+*?\#>-:';
var
  ConfigFile: TIniFileEx;
  Index, OffsetForOnesAlreadyThere: integer;
  sName:string;
  RememberCursor:TCursor;
begin
  result:=TRUE;
  OffsetForOnesAlreadyThere:=0;

  try
    RememberCursor:=Screen.Cursor;
    Screen.Cursor:=crHourGlass;
    try
      ConfigFile:=TIniFileEx.Create(TotalCommanderFilename);
      try
        with ConfigFile do
          begin
            if FlagEraseOriginalOnes then
            begin
              EraseSection(CONFIGFILE_SECTIONNAME);
            end
            else
            begin
              Index:=1;
              repeat
                sName:=AnsiToUTF8(ConfigFile.ReadString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(Index),TERMINATORNOTPRESENT));
                if sName<>TERMINATORNOTPRESENT then inc(OffsetForOnesAlreadyThere);
                inc(Index);
              until sName=TERMINATORNOTPRESENT;
            end;

            for Index:=0 to pred(count) do
              begin
                case THotDir(HotDir[Index]).Dispatcher of
                  hd_NULL:
                    begin
                    end;

                  hd_CHANGEPATH:
                    begin
                      WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),UTF8toAnsi(THotDir(HotDir[Index]).HotDirName));
                      if THotDir(HotDir[Index]).HotDirPath<>'' then WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_PATHPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),UTF8toAnsi('cd '+THotDir(HotDir[Index]).HotDirPath));
                      if THotDir(HotDir[Index]).HotDirTarget<>'' then WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_TARGETPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),UTF8toAnsi(THotDir(HotDir[Index]).HotDirTarget));
                    end;

                  hd_SEPARATOR:
                    begin
                      WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),'-');
                    end;

                  hd_STARTMENU:
                    begin
                      WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),'-'+UTF8toAnsi(THotDir(HotDir[Index]).HotDirName));
                    end;

                  hd_ENDMENU:
                    begin
                      WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),'--');
                    end;

                  hd_COMMAND:
                    begin
                      WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_NAMEPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),UTF8toAnsi(THotDir(HotDir[Index]).HotDirName));
                      if THotDir(HotDir[Index]).HotDirPath<>'' then WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_PATHPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),UTF8toAnsi(THotDir(HotDir[Index]).HotDirPath));
                      if THotDir(HotDir[Index]).HotDirTarget<>'' then WriteString(CONFIGFILE_SECTIONNAME,CONFIGFILE_TARGETPREFIX+IntToStr(OffsetForOnesAlreadyThere+Index+1),UTF8toAnsi(THotDir(HotDir[Index]).HotDirTarget));
                    end;
                end;
              end;
          end;
      finally
        ConfigFile.Free;
      end;

    except
      result:=FALSE;
    end;

  finally
    Screen.Cursor:=RememberCursor;
  end;
end;

{ THotDirList.ImportDoubleCommander }
procedure THotDirList.ImportDoubleCommander(DoubleCommanderFilename:utf8string);
var
  DoubleCommanderXMLToImport:TXmlConfig;
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

{ THotDirList.ExportDoubleCommander }
function THotDirList.ExportDoubleCommander(DoubleCommanderFilename:utf8string; FlagEraseOriginalOnes:boolean):boolean;
var
  DoubleCommanderXMLToImport:TXmlConfig;
  Root: TXmlNode;
  FlagKeepGoing:boolean;
begin
  result:=FALSE; //Unless we reach correctly the end, the result is negative
  FlagKeepGoing:=TRUE;

  DoubleCommanderXMLToImport := TXmlConfig.Create(DoubleCommanderFilename);
  try
    //Just in case we're requested to add or update content of a .XML file will already other data in it, first we load the structure
    if FileExists(DoubleCommanderFilename) then FlagKeepGoing:=DoubleCommanderXMLToImport.Load;

    if FlagKeepGoing then
    begin
      Root := DoubleCommanderXMLToImport.RootNode;
      SaveToXml(DoubleCommanderXMLToImport, Root, FlagEraseOriginalOnes);
      result:=DoubleCommanderXMLToImport.Save;
    end;
  finally
    FreeAndNil(DoubleCommanderXMLToImport);
  end;
end;

{ THotDirList.RefreshExistingProperty }
procedure THotDirList.RefreshExistingProperty(lsShowingIt:TListBox; lbleditShowingIt:TLabeledEdit; ScanMode:integer);
var
  Index,LocalThreadCount:longint;
  ListOfAlreadyCheckDrive,ListOfNonExistingDrive:TStringList;
  RememberCursor:TCursor;

  procedure StartThreadToSeeIfThisDriveExists(const sDrive:string);
  var
    CheckDrivePresenceThread: TCheckDrivePresenceThread;
  begin
    CheckDrivePresenceThread:= TCheckDrivePresenceThread.Create(sDrive,ListOfNonExistingDrive,LocalThreadCount);
  end;

  //Since we do that for both "Path" and "Target", it was useful to place in a routine so we can call two times the same routine
  procedure ScanForThisDir(DirToScan:string);
  var
    localPath,localDrive:string;
  begin
    localPath:=ExcludeTrailingPathDelimiter(mbExpandFileName(DirToScan));
    localDrive:=UpperCase(ExtractFileDrive(localPath));

    if ListOfAlreadyCheckDrive.IndexOf(localDrive) = -1 then
    begin
      inc(LocalThreadCount);
      StartThreadToSeeIfThisDriveExists(localDrive);
      ListOfAlreadyCheckDrive.Add(localDrive);
    end;
  end;

  //Since we do that for both "Path" and "Target", it was useful to place in a routine so we can call two times the same routine
  function CheckIfThisDirectoryExists(RequestedDirectoryToCheck:string):boolean;
  var
    localPath,localDrive:string;
  begin
    if RequestedDirectoryToCheck<>'' then
    begin
      result:=FALSE;
      localPath:=ExcludeTrailingPathDelimiter(mbExpandFileName(RequestedDirectoryToCheck));
      localDrive:=UpperCase(ExtractFileDrive(localPath));
      lbleditShowingIt.text:=localPath;
      Application.ProcessMessages;

      if ListOfNonExistingDrive.IndexOf(localDrive) = -1 then
      begin
        result:=mbDirectoryExists(localPath);
      end;

      if not result then
      begin
        FlagModified:=FlagModified OR (HotDir[Index].FHotDirExistingState<>DirNotExist);
        HotDir[Index].FHotDirExistingState:=DirNotExist;
      end;
    end
    else
    begin
      result:=TRUE;
    end;
  end;

begin
  RememberCursor:=Screen.Cursor;

  try
    Screen.Cursor:=crHourGlass;

    ListOfAlreadyCheckDrive:=TStringList.Create;
    ListOfAlreadyCheckDrive.Sorted:=FALSE;
    ListOfAlreadyCheckDrive.Clear;

    ListOfNonExistingDrive:=TStringList.Create;
    ListOfNonExistingDrive.Sorted:=FALSE;
    ListOfNonExistingDrive.Clear;

    try
      LocalThreadCount:=0;

      //First, let's build a list of the "\\ServerName" that exists and let's check them in MultiThread
      //We scan only once each drive and "\\ServerName"
      //"\\ServerName" have a long timeout so that's why we check them this way
      for Index:=0 to pred(count) do
      begin
        case HotDir[Index].FDispatcher of
          hd_CHANGEPATH:
            begin
              ScanForThisDir(HotDir[Index].FHotDirPath);
              if ScanMode=2 then ScanForThisDir(HotDir[Index].FHotDirTarget);
            end;
        end;
      end;

      //Let's wait all the threads to complete
      //TODO:Maybe a little timeout or something would be better here!
      while (LocalThreadCount<>0) do
        begin
          lbleditShowingIt.text:=IntToStr(LocalThreadCount);
          Application.ProcessMessages;
          if LocalThreadCount=0 then Sleep(100);
        end;

      //Second, now let's scan if the director exists!
      for Index:=0 to pred(count) do
      begin
        if lsShowingIt<>nil then lsShowingIt.ItemIndex:=Index;
        case HotDir[Index].FDispatcher of
          hd_CHANGEPATH:
            begin
              if CheckIfThisDirectoryExists(HotDir[Index].FHotDirPath) then
              begin
                case ScanMode of
                  1:
                    begin
                      FlagModified:=FlagModified OR (HotDir[Index].FHotDirExistingState<>DirExist);
                      HotDir[Index].FHotDirExistingState:=DirExist;
                    end;

                  2:
                    begin
                      if CheckIfThisDirectoryExists(HotDir[Index].FHotDirTarget) then
                      begin
                        FlagModified:=FlagModified OR (HotDir[Index].FHotDirExistingState<>DirExist);
                        HotDir[Index].FHotDirExistingState:=DirExist;
                      end;
                    end;
                end; //case ScanMode
              end;
            end; //hd_CHANGEPATH:
        end; //case HotDir[Index].FDispatcher of
      end;
    finally
      ListOfAlreadyCheckDrive.Free;
      ListOfNonExistingDrive.Free;
    end;

  finally
    Screen.Cursor:=RememberCursor;
  end;
end;

{ THotDirList.lsImportationHotDirClick }
procedure THotDirList.lsImportationHotDirClick(Sender: TObject);
var
  Index, SubMenuEndIndex,IndexSel:longint;
begin
  with Sender as TListBox do
  begin
    Index:=ItemIndex;
    case HotDir[Index].Dispatcher of
      hd_NULL:
        begin
        end;

      hd_CHANGEPATH:
        begin
        end;

      hd_SEPARATOR:
        begin
        end;

      hd_STARTMENU:
        begin
          SubMenuEndIndex:=GetIndexSubMenuEnd(Index);
          if SubMenuEndIndex<>-1 then
          begin
            for IndexSel:=Index+1 to SubMenuEndIndex do Selected[IndexSel]:=Selected[Index];
          end;
        end;

      hd_ENDMENU:
        begin
          Selected[Index]:=not Selected[Index];
        end;

      hd_COMMAND:
        begin
        end;
    end;
  end;
end;

{ THotDirList.AddFromAnotherListTheSelected }
function THotDirList.AddFromAnotherListTheSelected(ImportantedHotDirList:THotDirList;lsHoldingSelection:TListBox):longint;
var
  LocalHotDir:THotDir;
  Index,CurrentMenuLevel:longint;
begin
  result:=0;

  CurrentMenuLevel:=0;
  Index:=0;
  while (Index<lsHoldingSelection.Items.Count) do
  begin
    if lsHoldingSelection.Selected[Index] then
    begin
      LocalHotDir:=THotDir.Create;
      LocalHotDir.Dispatcher:=ImportantedHotDirList.HotDir[Index].Dispatcher;
      LocalHotDir.HotDirName:=ImportantedHotDirList.HotDir[Index].HotDirName;
      LocalHotDir.HotDirPath:=ImportantedHotDirList.HotDir[Index].HotDirPath;
      LocalHotDir.HotDirPathSort:=ImportantedHotDirList.HotDir[Index].HotDirPathSort;
      LocalHotDir.HotDirTarget:=ImportantedHotDirList.HotDir[Index].HotDirTarget;
      LocalHotDir.HotDirTargetSort:=ImportantedHotDirList.HotDir[Index].HotDirTargetSort;

      case LocalHotDir.Dispatcher of
        hd_STARTMENU:
          begin
            LocalHotDir.MenuLevel:=CurrentMenuLevel;
            inc(CurrentMenuLevel);
          end;

        hd_ENDMENU:
          begin
            dec(CurrentMenuLevel);
            LocalHotDir.MenuLevel:=CurrentMenuLevel;
          end;

        else
          begin
            LocalHotDir.MenuLevel:=CurrentMenuLevel;
          end;
      end;

      Add(LocalHotDir);
      inc(result);
    end; ////if lsHoldingSelection.Selected[Index] then

    inc(Index);
  end;
end;

{ THotDirList.EliminateTheNonSelectedInList }
function THotDirList.EliminateTheNonSelectedInList(lsHoldingSelection:TListBox):longint;
var
  Index:longint;
begin
  Index:=pred(lsHoldingSelection.Items.Count);
  while (Index>=0) do
  begin
    if not lsHoldingSelection.Selected[Index] then DeleteHotDir(Index);
    dec(Index);
  end;
  result:=count;
end;

{ THotDirList.lsHotDirDrawItem }
procedure THotDirList.lsHotDirDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  function GetSpacesRequire(NbSpace:longint):string;
    var
      i:integer;
    begin
      result:='';
      for i:=1 to NbSpace do result:=result+' ';
      result:=result+result+result+result;
    end;

begin
  with Control as TListBox do
    begin
      Canvas.FillRect(ARect);

      case HotDir[Index].Dispatcher of
        hd_NULL:
          begin
          end;

        hd_CHANGEPATH:
          begin
            Canvas.Font.Style:=[];
            Canvas.TextOut(ARect.Left+2,ARect.Top,GetSpacesRequire(HotDir[Index].MenuLevel)+HotDir[Index].HotDirName);
          end;

        hd_SEPARATOR:
          begin
          end;

        hd_STARTMENU:
          begin
            Canvas.Font.Style:=[fsBold];
            Canvas.TextOut(ARect.Left+2,ARect.Top,GetSpacesRequire(HotDir[Index].MenuLevel)+HotDir[Index].HotDirName);
          end;

        hd_ENDMENU:
          begin
            ARect.Bottom:=ARect.Top+3;
            //Canvas.Brush.Color:=clYellow;
            //Canvas.FillRect(Rect);
            Canvas.TextOut(ARect.Left+2,ARect.Top,GetSpacesRequire(HotDir[Index].MenuLevel)+'--');
          end;

        hd_COMMAND:
          begin
            Canvas.Font.Style:=[];
            Canvas.TextOut(ARect.Left+2,ARect.Top,GetSpacesRequire(HotDir[Index].MenuLevel)+HotDir[Index].HotDirName);
          end;
      end;
    end;
end;

{ THotDirList.CopyListToHotDirList }
procedure THotDirList.CopyListToHotDirList(var DestinationHotDirList:THotDirList);
var
  LocalHotDir:THotDir;
  Index:longint;
begin
  //Let's delete possible previous list content
  for Index:=pred(DestinationHotDirList.Count) downto 0 do DestinationHotDirList.DeleteHotDir(Index);
  DestinationHotDirList.Clear;

  //Now let's create entries and add them one by one to the destination list
  for Index:=0 to pred(Count) do
  begin
    LocalHotDir:=THotDir.Create;
    LocalHotDir.Dispatcher:=HotDir[Index].Dispatcher;
    LocalHotDir.MenuLevel:=HotDir[Index].MenuLevel;
    LocalHotDir.HotDirName:=HotDir[Index].HotDirName;
    LocalHotDir.HotDirPath:=HotDir[Index].HotDirPath;
    LocalHotDir.HotDirPathSort:=HotDir[Index].HotDirPathSort;
    LocalHotDir.HotDirTarget:=HotDir[Index].HotDirTarget;
    LocalHotDir.HotDirTargetSort:=HotDir[Index].HotDirTargetSort;
    LocalHotDir.FHotDirExistingState:=HotDir[Index].HotDirExisting;
    DestinationHotDirList.Add(LocalHotDir);
  end;
  DestinationHotDirList.FlagModified:=TRUE;
end;

{ THotDirList.GetHotDir }
function THotDirList.GetHotDir(Index: Integer): THotDir;
begin
  Result:= THotDir(Items[Index]);
end;

{ TCheckDrivePresenceThread.Create }
constructor TCheckDrivePresenceThread.Create(sDrive:string; ParamListOfNonExistingDrive: TStringList; var ThreadCount:longint);
begin
  FListOfNonExistingDrive:=ParamListOfNonExistingDrive;
  FDriveToSearchFor:=sDrive;
  FThreadCountPoint:=addr(ThreadCount);
  FreeOnTerminate:=True;
  inherited Create(FALSE);
end;

{ TCheckDrivePresenceThread.Destroy }
destructor TCheckDrivePresenceThread.Destroy;
begin
  inherited Destroy;
end;

{TCheckDrivePresenceThread.Execute}
procedure TCheckDrivePresenceThread.Execute;
begin
  if FDriveToSearchFor='' then
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
  dec(FThreadCountPoint^);
end;

{ TCheckDrivePresenceThread.ReportNotPresentInTheThread }
procedure TCheckDrivePresenceThread.ReportNotPresentInTheThread;
begin
  FListOfNonExistingDrive.Add(FDriveToSearchFor);
  dec(FThreadCountPoint^);
end;

end.

