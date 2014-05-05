{
   Double Commander
   -------------------------------------------------------------------------
   Load/Save HotDir

   The text here will need to be written by someone better than me...

   But at least for now I can write it has been inspired directly from "uSearchTemplate.pas" on 2014-03-12

}

unit uHotDir;

{$mode objfpc}{$H+}

interface

uses
 //Lazarus
 Classes, SysUtils, Menus,

 //DC
 DCClassesUtf8, uFile, DCXmlConfig, uFindFiles;

const
  ACTION_ADDTOHOTLIST = 1;
  ACTION_CONFIGTOHOTLIST = 2;

  POPUPMENU_JUSTDIRECTORIES = 1;
  POPUPMENU_WITHADDANDCONFIG = 2;

type
  { TProcedureWhenClickOnHotDirMenuItem}
  TProcedureWhenClickOnHotDirMenuItem = procedure(Sender: TObject) of object;

  { THotDir }
  THotDir = class
  private
    FHotDirName: string;
    FHotDirPath: string;
    FHotDirTarget: string;
  public
    constructor Create;
    property HotDirName: string read FHotDirName write FHotDirName;
    property HotDirPath: string read FHotDirPath write FHotDirPath;
    property HotDirTarget: string read FHotDirTarget write FHotDirTarget;
  end;

  { THotDirList }
  THotDirList = class(TList)
  private
    function GetHotDir(Index: Integer): THotDir;
    function GetHotDir(const AName: string): THotDir;
  public
    FlagModified:boolean;
    constructor Create;
    procedure Clear; override;
    function Add(HotDir: THotDir): Integer;
    procedure DeleteHotDir(Index: Integer);
    procedure DeleteHotDirMenuDelimiters(Index:Integer);
    procedure LoadToStringList(StringList: TStrings);
    procedure LoadFromIni(IniFile: TIniFileEx);
    procedure LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
    procedure SaveToIni(IniFile: TIniFileEx);
    procedure SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
    function TryToGetCloserHotDir(APath:string):longint;
    function GetIndexSubMenuEnd(SearchIndex:longint):longint;
    procedure Move(CurIndex, NewIndex: Integer);
    function MoveHotDirMenu(CurIndex, NewIndex: Integer):longint;
    property HotDirByName[const AName: string]: THotDir read GetHotDir;
    property HotDir[Index: Integer]: THotDir read GetHotDir;
    procedure CreatePopUpHotDir(pmHotDir:TPopupMenu; Dispatcher:longint; ProcedureWhenHotDirItemClicked,ProcedureWhenHotDirAddOrConfigClicked:TProcedureWhenClickOnHotDirMenuItem; CurrentPath:string);
  end;

  procedure CopyHotDirList(SourceHotDirList:THotDirList; var DestinationHotDirList:THotDirList);

  function isHotDirSubMenuStart(HotDirString:string):boolean;
  function isHotDirSubMenuEnd(HotDirString:string):boolean;
  function isHotDirSeparator(HotDirString:string):boolean;

implementation

uses
 DCFileAttributes, DCBasicTypes, uDebug, DCStrUtils, uDCUtils, uLng;

{ THotDir }
constructor THotDir.Create;
begin
  inherited Create;
end;

{ THotDirList }

constructor THotDirList.Create;
begin
  inherited Create;
  FlagModified:=FALSE;
end;


procedure THotDirList.Move(CurIndex, NewIndex: Integer);
begin
  inherited Move(CurIndex, NewIndex);
  FlagModified:=TRUE;
end;

function THotDirList.GetHotDir(Index: Integer): THotDir;
begin
  Result:= THotDir(Items[Index]);
end;

function THotDirList.TryToGetCloserHotDir(APath:string):longint;
var
  I,SecondAlternative:Integer;
  FlagFound:boolean;
begin
  APath:=lowercase(APath);
  SecondAlternative:=-1;

  repeat
    I:=0;
    FlagFound:=FALSE;

    while (I < Count) and (not FlagFound) do
      begin
        if APath=lowercase(ReplaceEnvVars(THotDir(Items[I]).HotDirPath)) then
          begin
            FlagFound:=TRUE;
          end
        else
          begin
            if (SecondAlternative=-1) and (pos(APath,lowercase(ReplaceEnvVars(THotDir(Items[I]).HotDirPath)))=1) then
              begin
                SecondAlternative:=I;
              end;

            inc(I);
          end;
      end;

    if not FlagFound then APath:=DCStrUtils.GetParentDir(APath);

  until (FlagFound OR (APath=''));

  if FlagFound then result:=I else if SecondAlternative<>-1 then result:=SecondAlternative else result:=-1;
end;

{Given the start of a menu, it will return the index of the end of the same menu}
function THotDirList.GetIndexSubMenuEnd(SearchIndex:longint):longint;
var
  InnerMenuCount:longint;
begin
  result:=-1;
  if isHotDirSubMenuStart(THotDir(Items[SearchIndex]).HotDirName) then
    begin
      InnerMenuCount:=1;
      while (SearchIndex<pred(count)) and (InnerMenuCount<>0) do
        begin
          inc(SearchIndex);
          if isHotDirSubMenuStart(THotDir(Items[SearchIndex]).HotDirName) then
            inc(InnerMenuCount)
          else
            if isHotDirSubMenuEnd(THotDir(Items[SearchIndex]).HotDirName) then dec(InnerMenuCount);
        end;

      if InnerMenuCount=0 then result:=SearchIndex;
    end;
end;

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
              for OffsetIndex:=0 to (HotDirSubMenuEndIndex-CurIndex) do
                Move(CurIndex+OffsetIndex, NewIndex+OffsetIndex);
            end
          else
            begin
              for OffsetIndex:=0 to (HotDirSubMenuEndIndex-CurIndex) do
                Move(CurIndex, NewIndex);
            end;
          FlagModified:=TRUE;
        end;
    end;
end;

function THotDirList.GetHotDir(const AName: string): THotDir;
var
  I: Integer;
begin
  Result:= nil;

  for I:= 0 to Count - 1 do
    if SameText(THotDir(Items[I]).HotDirName, AName) then
      begin
        Result:= THotDir(Items[I]);
        Exit;
      end;
end;

procedure THotDirList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    HotDir[i].Free;
  inherited Clear;
end;

function THotDirList.Add(HotDir: THotDir): Integer;
begin
  Result:= inherited Add(HotDir);
  FlagModified:=TRUE;
end;

procedure THotDirList.DeleteHotDir(Index: Integer);
begin
  HotDir[Index].Free;
  Delete(Index);
  FlagModified:=TRUE;
end;

procedure THotDirList.DeleteHotDirMenuDelimiters(Index:Integer);
var
  EndIndex:longint;
begin
  if isHotDirSubMenuStart(HotDir[Index].HotDirName) then
    begin
      EndIndex:=GetIndexSubMenuEnd(Index);

      if (EndIndex<>-1) AND (EndIndex<>Index) then
        begin
          HotDir[EndIndex].Free;
          Delete(EndIndex);
        end;

      HotDir[Index].Free;
      Delete(Index);
      FlagModified:=TRUE;
    end;
end;

{
  When loading the structure to a TStrings, let's do that:
  -Initial submenu-like entry will have a "-" in front of the initial name.
  -Following ones are indented by 2 spaces.
  -Ending sub menu will show the "--" in the list.
  This duplicates the TC way of showing things.
}
procedure THotDirList.LoadToStringList(StringList: TStrings);
var
  I: Integer;
  MaybeDirName,Prefix:string;
begin
  StringList.Clear;
  Prefix:='';

  for I:= 0 to Count - 1 do
    begin
      MaybeDirName:=HotDir[I].HotDirName;

      if not isHotDirSubMenuEnd(HotDir[I].HotDirName) then
        begin
          StringList.Add(Prefix+MaybeDirName); //Let's show the "-" infront of HotDir name like TC
          if isHotDirSubMenuStart(HotDir[I].HotDirName) then Prefix:=Prefix+'    ';
        end
      else
        begin
          if length(Prefix)>1 then Prefix:=leftstr(Prefix,length(Prefix)-4);
          StringList.Add(Prefix+MaybeDirName); //Let's show the "--" like TC
        end;
    end;

  //TODO: could check if "prefix" is empty at the end. If not, and "--" until the end is reached! Not only in TStrings but also in structure!
end;

const
  cSection = 'DirectoryHotList';

procedure THotDirList.LoadFromIni(IniFile: TIniFileEx);
begin
  Clear;
end;

procedure THotDirList.LoadFromXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  Name, Path: String;
  LocalHotDir: THotDir;
begin
  Clear;

  ANode := ANode.FindNode(cSection);
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('HotDir') = 0 then
      begin
        if AConfig.TryGetAttr(ANode, 'Name', Name) and
           AConfig.TryGetAttr(ANode, 'Path', Path) then
        begin
          LocalHotDir:=THotDir.Create;
          LocalHotDir.HotDirName:=Name;
          LocalHotDir.HotDirPath:=Path;
          LocalHotDir.HotDirTarget:=AConfig.GetAttr(ANode, 'Target', '');
          Add(LocalHotDir);
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

procedure THotDirList.SaveToIni(IniFile: TIniFileEx);
begin
  IniFile.EraseSection(cSection);
end;

procedure THotDirList.SaveToXml(AConfig: TXmlConfig; ANode: TXmlNode);
var
  I: Integer;
  SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(ANode, cSection, True);
  AConfig.ClearNode(ANode);
  for I:= 0 to Count - 1 do
    begin
      SubNode := AConfig.AddNode(ANode, 'HotDir');
      AConfig.SetAttr(SubNode, 'Name', HotDir[I].HotDirName);
      AConfig.SetAttr(SubNode, 'Path', HotDir[I].HotDirPath);
      AConfig.SetAttr(SubNode, 'Target', HotDir[I].HotDirTarget);
    end;
end;

procedure THotDirList.CreatePopUpHotDir(pmHotDir:TPopupMenu; Dispatcher:longint; ProcedureWhenHotDirItemClicked,ProcedureWhenHotDirAddOrConfigClicked:TProcedureWhenClickOnHotDirMenuItem; CurrentPath:string);
var
  mi: TMenuItem;
  I: Integer;
  sName:string;

  procedure CompleteMenu(ParamMenuItem:TMenuItem);
    var
      localmi:TMenuItem;
      localsName:string;
    begin
      while I<Count do
        begin
          localsName:=HotDir[I].HotDirName;
          inc(I);
          if isHotDirSubMenuStart(localsName) then
            begin
              localmi:=TMenuItem.Create(ParamMenuItem);
              localmi.Caption:=copy(localsName,2,length(localsName)-1);
              ParamMenuItem.Add(localmi);
              CompleteMenu(localmi);
            end
          else
            begin
              if isHotDirSubMenuEnd(localsName) then
                begin
                  exit;
                end
              else
                begin
                  localmi:=TMenuItem.Create(ParamMenuItem);
                  localmi.Caption:=localsName;
                  localmi.tag:=I-1;
                  localmi.OnClick:=ProcedureWhenHotDirItemClicked;
                  ParamMenuItem.Add(localmi);
                end;
            end;
        end;
    end;

begin
  // Create All popup menu
  pmHotDir.Items.Clear;
  I:=0;
  while I<Count do
  begin
    sName:=HotDir[I].HotDirName;
    inc(I);

    if isHotDirSubMenuStart(sName) then
      begin
        mi:= TMenuItem.Create(pmHotDir);
        mi.Caption:=copy(sName,2,length(sName)-1);
        pmHotDir.Items.Add(mi);
        CompleteMenu(mi);
      end
    else
      begin
        if not isHotDirSubMenuEnd(sName) then
          begin
            mi:= TMenuItem.Create(pmHotDir);
            mi.Caption:=sName;
            mi.tag:= I-1;
            mi.OnClick:=ProcedureWhenHotDirItemClicked;
            pmHotDir.Items.Add(mi);
          end;
      end;
  end;

  case Dispatcher of
    POPUPMENU_WITHADDANDCONFIG:
      begin
        // now add delimiter
        mi:= TMenuItem.Create(pmHotDir);
        mi.Caption:= '-';
        pmHotDir.Items.Add(mi);

        // now add the "add current path"
        mi:= TMenuItem.Create(pmHotDir);
        mi.Caption:= Format(rsMsgPopUpHotAdd,[CurrentPath]);
        mi.Tag:=ACTION_ADDTOHOTLIST;
        mi.OnClick:=ProcedureWhenHotDirAddOrConfigClicked;
        pmHotDir.Items.Add(mi);

        // now add configure item
        mi:= TMenuItem.Create(pmHotDir);
        mi.Caption:= rsMsgPopUpHotCnf;
        mi.Tag:=ACTION_CONFIGTOHOTLIST;
        mi.OnClick:= ProcedureWhenHotDirAddOrConfigClicked;
        pmHotDir.Items.Add(mi);
      end;
  end;

end;




function isHotDirSubMenuStart(HotDirString:string):boolean;
  begin
    result:=FALSE;
    if length(HotDirString)>1 then
      begin
        result:=((HotDirString[1]='-') AND (HotDirString[2]<>'-'));
      end;
  end;

function isHotDirSubMenuEnd(HotDirString:string):boolean;
begin
  result:=FALSE;
  if length(HotDirString)>1 then
    begin
      result:=(copy(HotDirString,1,2)='--');
    end;
end;

function isHotDirSeparator(HotDirString:string):boolean;
begin
  result:=(HotDirString='-');
end;

procedure CopyHotDirList(SourceHotDirList:THotDirList; var DestinationHotDirList:THotDirList);
var
  LocalHotDir:THotDir;
  Index:longint;
begin
  for Index:=pred(DestinationHotDirList.Count) downto 0 do
    begin
      DestinationHotDirList.DeleteHotDir(Index);
    end;

  DestinationHotDirList.Clear;
  for Index:=0 to pred(SourceHotDirList.Count) do
    begin
      LocalHotDir:=THotDir.Create;
      LocalHotDir.HotDirName:=SourceHotDirList.HotDir[Index].HotDirName;
      LocalHotDir.HotDirPath:=SourceHotDirList.HotDir[Index].HotDirPath;
      LocalHotDir.HotDirTarget:=SourceHotDirList.HotDir[Index].HotDirTarget;
      DestinationHotDirList.Add(LocalHotDir);
    end;
end;

end.

