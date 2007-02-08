{
Double Commander
----------------------------
Implementation of Virtual File System

Licence  : GNU GPL v 2.0
Author   : Alexander Koblov (Alexx2000@mail.ru)

contributors:

Radek Cervinka, radek.cervinka@centrum.cz

}

unit uVFS_;


interface
uses
  Classes, uFileList, uVFSutil, uTypes, uWCXmodule;
type

  { TVFS }

  TVFS = class
  protected
    FPlugins : TStringList;
    FCurrentPlugin : String;
    sLastArchive:String;
    FArcFileList : TList;
    lsCache:TFileList;
    FWCXModule : TWCXmodule;
    function ExtractDirLevel(const sPrefix, sPath:String):String;
    function LowDirLevel(sPath : String) : String;
    function IncludeFileInList(sPath : String; var sFileName : String) : Boolean;
    procedure AddUpLevel (sUpPath : String; var ls:TFileList); // add virtually ..
  public
    Constructor Create;
    Destructor Destroy; override;
    function OpenArchive(var flist: TFileList) : Boolean;
    function ChangeDirLevel(frp:PFileRecItem; var flist: TFileList; cdUpLevel : Boolean) : Boolean;
    function FindModule(const sFileName:String):Boolean;
    property WCXmodule : TWCXmodule read FWCXModule;
    property ArcFullName : String read sLastArchive;
  end; //class TVFS

implementation

uses
  SysUtils, uGlobsPaths, FindEx, IniFiles, uOSUtils;

{ TVFS }

constructor TVFS.Create;
var
  Ini : TIniFile;
  //lsModules:TStrings;
begin
  //lsModules:=TStrings.Create; // list of modules
  FPlugins := TStringList.Create;
  Ini := TIniFile.Create(gpIniDir+'doublecmd.ini');
  Ini.ReadSectionRaw('PackerPlugins', FPlugins);
  Ini.Free;
  //FPlugins.AddStrings(lsModules);
  //lsModules.Free;
  sLastArchive:='';  // nothing
  lsCache:=TFileList.Create;
  //FArcFileList := TList.Create;
end;

destructor TVFS.Destroy;
var
  i:Integer;
begin
if Assigned(FWCXModule) then
   FWCXModule.Destroy;
FWCXModule := nil;
FreeAndNil(FPlugins);
FreeAndNil(FArcFileList);
FreeAndNil(lsCache);
inherited
end;

function TVFS.LowDirLevel(sPath : String) : String;
var
Index, I : Integer;
tmp : String;
begin
Index := Length(sPath);
if sPath[Index] = DirectorySeparator then
  Delete(sPath, Index, 1);
tmp := sPath;
while True do
  begin
    Index := Pos(DirectorySeparator, tmp);
    if Index > 0 then
      begin
        Delete(tmp, Index, 1);
        I := Index;
      end
    else
      Break;
  end;
Result := Copy(sPath, 1, I);

end;

function TVFS.ExtractDirLevel(const sPrefix, sPath: String): String;
begin
  Result:=Copy(sPath, length(sPrefix)+1, length(sPath)-length(sPrefix)+1); // remove prefix
  if Result='' then Exit;
  if Result[1]=DirectorySeparator then
    Delete(Result,1,1);
  if pos(DirectorySeparator,Result)>0 then // remove next level of dir
    Result:=Copy(Result,1,Pos(DirectorySeparator,Result)-1);
end;


function TVFS.IncludeFileInList(sPath : String; var sFileName : String) : Boolean;
var
Index : Integer;
begin
//WriteLN('Folder = ', SPath);
Result := False;
Index := Pos(SPath, sFileName);
if Index > 0 then
  begin
    Delete(sFileName, 1, Index + Length(SPath) - 1);
    if Pos(DirectorySeparator, sFileName) = 0 then
      Result := True;
  end;
end;

procedure TVFS.AddUpLevel(sUpPath : String; var ls:TFileList); // add virtually ..
var
  fi:TFileRecItem;
begin
  fi.sName:='..';
  fi.iSize:=0;
  fi.sExt:='';
  fi.sNameNoExt:=fi.sName;
  fi.bSelected:=False;
  fi.bExecutable:=False;
  fi.sModeStr:='drwxr-xr-x';
  fi.iMode:=ModeStr2Mode(fi.sModeStr); //!!!!
  fi.iDirSize:=0;
  fi.sPath := sUpPath;
  ls.AddItem(@fi);
end;


function TVFS.ChangeDirLevel(frp:PFileRecItem; var flist: TFileList; cdUpLevel : Boolean) : Boolean;
var
  Count,
  I:Integer;
  Folder, UpFolder : String;
  InFolder : PFileRecItem;
  CurrFileName : String;  // Current file name
begin
Result := False;
//WriteLN('Folder = ', frp^.sName);
if cdUpLevel then
  begin
    if frp^.sPath = '' then  // Exit from archive
      Exit;
    frp^.sName := frp^.sPath;
    frp^.sPath := LowDirLevel(frp^.sPath);
  end;
Folder := frp^.sName;
if not cdUpLevel then
  begin
    if (Folder[1] = DirectorySeparator) then
      Delete(Folder, 1, 1);
  Folder := frp^.sPath + Folder;
  end;
if (Folder[Length(Folder)] <> DirectorySeparator) then
Folder := Folder + DirectorySeparator;
UpFolder := frp^.sPath;
flist.Clear;
AddUpLevel(UpFolder, flist);
Count := FArcFileList.Count;
  for I := 0 to Count -1 do
  begin
  CurrFileName := TWCXItem(FArcFileList.Items[I]^).FileRecItem.sName;
 
  if IncludeFileInList(Folder, CurrFileName) then
    begin
      New(InFolder);
      InFolder^ := TWCXItem(FArcFileList.Items[I]^).FileRecItem;
      InFolder^.sName := CurrFileName;
      with InFolder^ do
        begin
          if FPS_ISDIR(iMode) then
            sExt:=''
          else
            sExt:=ExtractFileExt(sName);
        sNameNoExt:=Copy(sName,1,length(sName)-length(sExt));
        sPath := Folder;
       end; //with
       WriteLN('InFolder = ', InFolder^.sPath);
      flist.AddItem(InFolder);
    end;
  end;
  Result := True;
end;

function TVFS.OpenArchive(var flist: TFileList) : Boolean;
var
Count, I : Integer;
InFolder : PFileRecItem;
CurrFileName : String;  // Current file name
begin
FWCXModule := TWCXmodule.Create;
Result := FWCXModule.LoadModule(FCurrentPlugin);
WriteLN(BoolToStr(Result));
if Result then
  begin
  FArcFileList := FWCXModule.WCXOpen(sLastArchive);
  flist.Clear;
  AddUpLevel('', flist);
  Count := FArcFileList.Count;
  WriteLN('FArcFileList.Count = ', FArcFileList.Count);
  for I := 0 to Count -1 do
  begin
  CurrFileName := TWCXItem(FArcFileList.Items[I]^).FileRecItem.sName;
  if IncludeFileInList(DirectorySeparator, CurrFileName) then
     begin
      New(InFolder);
      InFolder^ := TWCXItem(FArcFileList.Items[I]^).FileRecItem;
      InFolder^.sName := CurrFileName;
      with InFolder^ do
        begin
          if FPS_ISDIR(iMode) then
            sExt:=''
          else
            sExt:=ExtractFileExt(sName);
        sNameNoExt:=Copy(sName,1,length(sName)-length(sExt));
       end; //with
      flist.AddItem(InFolder);
      end;
    end;
     WriteLN(TWCXItem(FArcFileList.Items[I]^).FileRecItem.sName);
  end;
end;

function TVFS.FindModule(const sFileName:String):Boolean;
var
  Count, i:Integer;
  sExt, tmp:String;
  Index : Integer;
begin
  Result := False;
  tmp := '';
  sExt := LowerCase(ExtractFileExt(sFileName));
  sExt := copy(sExt,2,length(sExt));
  WriteLN('sExt = ', sExt);
  tmp := FPlugins.Values[sExt];
  //**************** Debug
     //WriteLN(FPlugins.Text);
     for i:=0 to FPlugins.Count -1 do
     WriteLN(FPlugins.ValueFromIndex[i]);
  //***************
  WriteLN('tmp = ', tmp);
  if tmp <> '' then
    begin
      Index := Pos(',', tmp) + 1;
      FCurrentPlugin := Copy(tmp, Index, Length(tmp));
      WriteLN('FCurrentPlugin = ', FCurrentPlugin);
      sLastArchive := sFileName;
      WriteLN('sLastArchive = ', sLastArchive);
      Result := True;
    end;
end;


initialization

end.
