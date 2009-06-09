{
   Seksi Commander
   ----------------------------

   Class for storing list of files

   Licence   : GNU GPL 2
   Copyright : (2003)Radek Cervinka, Peter Cernoch
   Contact   : pcernoch@volny.cz
              radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2006-2007 Alexander Koblov (Alexx2000@mail.ru)

   TODO:
   maybe protect Sort with TCriticalSection, because
   in multithreaded program global variable
   bSortNegative can be rewriten by other thread,
   but Sort is Called only from main thread and
   it is safe
}

unit uFileList;

{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, uTypes, uColumns;

Type

  TSortDirection = (sdNone, sdAscending, sdDescending);

  TFileListSorting = class;

  {en
     Class for storing list of files
  }
  TFileList = class
  private
    fDir : String;
    function GetCount:Integer;
  protected
    {en
       Internal TList class for storing pointers
       to TFileRecItem strucrures
    }
    fList: TList;
    CurrentSorting : TFileListSorting;
  public
    {en
       Create TFileList
    }
    Constructor Create;
    {en
       Destroy TFileList
    }
    Destructor  Destroy; override;
    {en
       Clear file list
    }
    procedure   Clear;
    {en
       Add new item to file list
       @param(fi Pointer to TFileRecItem strucrure)
       @returns(Index of added item)
    }
    function  AddItem(fi: PFileRecItem):Integer;
    {en
       Delete item from file list
       @param(iIndex Index of deleting item)
    }
    procedure DeleteItem(iIndex: Integer);
    {en
       Return item by index
       @param(iIndex Item index)
       @returns(Pointer to TFileRecItem strucrure)
    }
    procedure LoadFromFileNames(const FileNamesList: TStringList);
    {en
       Clears the filelist and fills it with file records using
       a list of filenames with full paths. It is used generally
       to convert a list of file paths from external applications.
       @param(FileNamesList List of filenames with full paths)
    }
    function  GetItem(iIndex: Integer) : PFileRecItem;
    {en
       Return full file name of item by index
       @param(iIndex Item index)
       @returns(File name)
    }
    function  GetFileName(iIndex: Integer): String;
    {en
       Return item index by file name
       @param(sFileName File name)
       @returns(Item index if item found, -1 otherwise)
    }
    function  CheckFileName(const sFileName:String):Integer;
    {en
       Update icon index information
       @param(PanelMode Current panel mode)
    }
    procedure  UpdateFileInformation(PanelMode: TPanelMode);
    {en
       Sort file list
       @param(Sorting object)
    }
    procedure Sort(Sorting : TFileListSorting; ColumnsClass: TPanelColumnsClass); overload;
    {en
       Indicates the number of items in the file list
    }
    property Count      : Integer read GetCount;
    {en
       Contain current file list directory
    }
    property CurrentDirectory : String read fDir write fDir;
  end;

  PFileList = ^TFileList;

  TFileListSortingColumn = record
    iField : Integer;
    SortDirection : TSortDirection;
    CompareFunction : TCompareFunction;
    CompiledSorting : TCompiledFunctionSorting;
  end;

  TFileListSortFields = array of Integer;

  PFileListSortingColumn = ^TFileListSortingColumn;

  TFileListSorting = class(TList)
  public
    Destructor Destroy; override;
    procedure AddSorting(iField : Integer; SortDirection : TSortDirection);
    procedure Clear; override;
    function GetSortingDirection(iField : Integer) : TSortDirection;
    function GetSortFields: TFileListSortFields;
  end;

  PFileListSorting = ^TFileListSorting;

{ this function couldn't be a method > type of parametr TList.Sort
  is function and not a method
}
  function ICompareByDirectory(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByName(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByNameNoExt(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByExt (item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareBySize(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByDate(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByAttr(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;

  function IMulticolumnCompare(item1, item2:Pointer):Integer;

  procedure CopyListSelectedExpandNames(srcFileList, dstFileList: TFileList; sPath: String;
                                        bFullName: Boolean = True; bSkipFolder: Boolean = False);

  function ReverseSortDirection(SortDirection: TSortDirection): TSortDirection;

implementation

uses
  LCLProc, uGlobs, uPixmapManager, uDCUtils, uOSUtils, uFileOp;

{
class constructor
}
Constructor TFileList.Create;
begin
  fList := TList.Create;
  CurrentSorting := TFileListSorting.Create;
end;


Destructor TFileList.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  FreeAndNil(CurrentSorting);
  inherited;
end;


procedure TFileList.Clear;
var
  i:Integer;
begin
  if (Assigned(fList)) then
  begin
    for i:=fList.Count-1 downto 0 do
      DeleteItem(i);
    fList.Clear;
  end;
end;

Function TFileList.CheckFileName(const sFileName:String):Integer;
var
  i:Integer;
begin
  Result:=-1;
  for i:=0 to fList.Count-1 do
    if (GetItem(i)^.sName=sFileName) then
    begin
      Result:=i;
      Exit;
    end;
    DebugLN('GetItem(i)^.sName = ', GetItem(i)^.sName);
end;

procedure TFileList.DeleteItem(iIndex: Integer);
begin
  if (iIndex > (fList.Count - 1)) then exit;
  //delete items count
  dispose(PFileRecItem(fList.Items[iIndex]));
  fList.Delete(iIndex);
end;

{
add new item to file list
}
function TFileList.AddItem(fi : PFileRecItem): Integer;
var
  p: PFileRecItem;
begin
  new(p);

  p^.bIsLink := fi^.bIsLink;
  p^.bLinkIsDir:=fi^.bLinkIsDir;
  p^.sLinkTo := fi^.sLinkTo;
  p^.sName := fi^.sName;
  p^.sNameNoExt:=fi^.sNameNoExt;
  p^.sPath:= fi^.sPath;
  p^.sExt := fi^.sExt;
  p^.iSize := fi^.iSize;
  p^.fTimeI := fi^.fTimeI;
  p^.sTime := fi^.sTime;
  p^.iMode := fi^.iMode;
  p^.bSysFile := fi^.bSysFile;
  p^.bExecutable := fi^.bExecutable;
  p^.sModeStr := fi^.sModeStr;
  p^.iIconID:= fi^.iIconID;
  p^.bSelected:= fi^.bSelected;
  p^.sOwner:=fi^.sOwner;
  p^.sGroup:=fi^.sGroup;
  p^.iOwner:=fi^.iOwner; //[mate]
  p^.iGroup:=fi^.iGroup; //[mate]
  p^.iDirSize:= fi^.iDirSize;
  p^.pContainer := Self;
  Result := fList.Add(p);
end;

procedure TFileList.LoadFromFileNames(const FileNamesList: TStringList);
var
  fr: TFileRecItem;
  i: Integer;
begin
  fList.Clear;

  if not Assigned(FileNamesList) or (FileNamesList.Count <= 0) then Exit;

  // TODO: File names can be from different directories.
  //       Maybe set individual sPath's instead.
  CurrentDirectory := ExtractFilePath(FileNamesList[0]);

  for i := 0 to FileNamesList.Count-1 do
    begin
      fr:= LoadFilebyName(FileNamesList[i]);
      fr.sName:= FileNamesList[i];
      fr.sNameNoExt:= ExtractFileName(FileNamesList[i]);
      AddItem(@fr);
    end;
end;

{
return item with index iIndex
}
function TFileList.GetItem(iIndex: Integer) : PFileRecItem;
begin
  if ((iIndex + 1) > fList.Count) then
    Raise Exception.Create('Bad index in GetItem');
  Result := fList.items[iIndex];
end;


{
return full file name of item with index iIndex;
 -> index starts from 0
}
function TFileList.GetFileName(iIndex: Integer): String;
var
  p : PFileRecItem;
begin
  if (iIndex >= Count) or (iIndex < 0) then
    Raise Exception.Create('Bad index GetFileName');
  p := fList.Items[iIndex];
  Result:=p^.sName;
end;

{
Sort files by multicolumn sorting.
}
procedure TFileList.Sort(Sorting : TFileListSorting; ColumnsClass: TPanelColumnsClass);
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
  Column: TPanelColumn;
begin
  if fList.Count = 0 then Exit;

  CurrentSorting.Clear;

  for i := 0 to Sorting.Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(Sorting[i]);
    CurrentSorting.AddSorting(pSortingColumn^.iField, pSortingColumn^.SortDirection);
  end;

  for i := 0 to CurrentSorting.Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(CurrentSorting[i]);

    if (pSortingColumn^.iField >= 0) and
       (pSortingColumn^.iField < ColumnsClass.ColumnsCount) then
    begin
      Column := ColumnsClass.GetColumnItem(pSortingColumn^.iField);
      pSortingColumn^.CompiledSorting := Column.CompileFunctionSorting;
      pSortingColumn^.CompareFunction := @Column.Compare;
    end
    else
      Raise Exception.Create('Unknow sort parametr - fix me');
  end;
  fList.Sort(@IMulticolumnCompare);
end;

function TFileList.GetCount:Integer;
begin
  Result:=flist.Count;
end;

{ Return Values for ICompareByxxxx function

> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2
}

{
  This function is simples support of sorting
  directory (handle uglobs.gDirSortFirst)

  Result is 0 if both parametres is directory and equal
  or not a directory (both).

  Else return +/- as ICompare****
  > 0 (positive)   Item1 is less than Item2
  < 0 (negative)  Item1 is greater than Item2
}

function IMulticolumnCompare(item1, item2:Pointer):Integer;
var
  CurrentFileList : TFileList;
  Sorting : TFileListSorting;
  iSortingCount : Integer;
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
  bNegative: Boolean;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result := 0;
  if item1 = item2 then Exit;

  // Put directories first.
  Result := ICompareByDirectory(item1, item2, False); // Ascending
  if Result <> 0 then Exit;

  CurrentFileList := TFileList(PFileRecItem(item1)^.pContainer);
  Sorting := CurrentFileList.CurrentSorting;
  iSortingCount := Sorting.Count - 1;
  for i := 0 to iSortingCount do
  begin
    pSortingColumn := PFileListSortingColumn(Sorting[i]);
    bNegative := (pSortingColumn^.SortDirection = sdDescending);

    Result := pSortingColumn^.CompareFunction(pSortingColumn^.CompiledSorting,
                                              item1, item2, bNegative);

    if Result <> 0 then Exit;
  end;
end;

function ICompareByDirectory(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  if (not (FPS_ISDIR(item1^.iMode) or item1^.bLinkIsDir)) and  (not (FPS_ISDIR(item2^.iMode) or item2^.bLinkIsDir)) then  Exit;
  if (not (FPS_ISDIR(item1^.iMode) or item1^.bLinkIsDir)) and (FPS_ISDIR(item2^.iMode) or item2^.bLinkIsDir) then
  begin
    Result:=+1;
    Exit;
  end;
  if (FPS_ISDIR(item1^.iMode) or item1^.bLinkIsDir) and (not (FPS_ISDIR(item2^.iMode) or item2^.bLinkIsDir)) then
  begin
    Result:=-1;
    Exit;
  end;
// both is directory, compare it
//  if item1.fName=item2.fName then Exit;
  // handle .. first
  if item1^.sName='..' then
  begin
    Result:=-1;
    Exit;
  end;
  if item2^.sName='..' then
  begin
    Result:=+1;
    Exit;
  end;
end;

function ICompareByName(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result := 0;

  if gCaseSensitiveSort then
    Result := StrComp(PChar(item1^.sName), PChar(item2^.sName))
  else
    Result := mbCompareText(item1^.sName, item2^.sName);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByNameNoExt(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
var
  name1, name2: string;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result := 0;

  name1 := ExtractOnlyFileName(item1^.sName);
  name2 := ExtractOnlyFileName(item2^.sName);

  if gCaseSensitiveSort then
    Result := StrComp(PChar(name1), PChar(name2))
  else
    Result := mbCompareText(name1, name2);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByExt(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  if item1^.sExt = item2^.sExt then
    Exit;

  if gCaseSensitiveSort then
    Result := StrComp(PChar(item1^.sExt), PChar(item2^.sExt))
  else
    Result := mbCompareText(item1^.sExt, item2^.sExt);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByDate(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  if item1^.fTimeI = item2^.fTimeI then
    Exit;

  if item1^.fTimeI < item2^.fTimeI then
    Result := -1
  else
    Result := +1;

  if bSortNegative then
    Result := -Result;
end;

function ICompareByAttr(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  if item1^.iMode = item2^.iMode then
    Exit;

  if item1^.iMode > item2^.iMode then
    Result := -1
  else
    Result := +1;

  if bSortNegative then
    Result := -Result;
end;

function ICompareBySize(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;

  function GetSize(pFile: PFileRecItem): Cardinal;
  begin
    if FPS_ISDIR(pFile^.iMode) or pFile^.bLinkIsDir then
    begin
      if pFile^.iDirSize <> 0 then
        Result := pFile^.iDirSize
      else
        Result := 0;
    end
    else
      Result := pFile^.iSize;
  end;

var
  iSize1 : Cardinal;
  iSize2 : Cardinal;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result := 0;

  iSize1 := GetSize(item1);
  iSize2 := GetSize(item2);

  if iSize1 = iSize2 then
    Exit;

  if iSize1 < iSize2 then
    Result := -1
  else
    Result := +1;

  if bSortNegative then
    Result := -Result;
end;

procedure TFileList.UpdateFileInformation(PanelMode: TPanelMode);
var
  i:Integer;
  frp:PFileRecItem;
begin
  for i:=0 to fList.Count-1 do
  begin
    frp:=PFileRecItem(Flist.Items[i]);
    frp^.iIconID:=PixMapManager.GetIconByFile(frp, PanelMode);
  end;
end;

procedure CopyListSelectedExpandNames(srcFileList, dstFileList: TFileList; sPath: String;
                                      bFullName: Boolean = True; bSkipFolder: Boolean = False);
var
  xIndex:Integer;
  p:TFileRecItem;
begin
  Assert(srcFileList <> nil,'CopyListExpandNames: srcFileList=nil');
  Assert(dstFileList <> nil,'CopyListExpandNames: dstFileList=nil');
  dstFileList.Clear;
  dstFileList.CurrentDirectory := sPath;
  for xIndex:=0 to srcFileList.Count-1 do
  begin
    p:=srcFileList.GetItem(xIndex)^;
    if (not p.bSelected) or (p.sName = '..') then Continue;
    if bSkipFolder and FPS_ISDIR(p.iMode) then Continue;
    if bFullName then
      begin
        p.sNameNoExt:=p.sName; //dstname
        p.sName := GetSplitFileName(p.sNameNoExt, sPath);
        p.sPath:='';
      end
    else
      begin
        GetSplitFileName(p.sName, sPath);
        p.sPath := sPath;
      end;
    DebugLN(p.sName);
    dstFileList.AddItem(@p);
  end;
end;

procedure TFileListSorting.AddSorting(iField : Integer; SortDirection : TSortDirection);
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    if pSortingColumn^.iField = iField then
    begin
      pSortingColumn^.SortDirection := ReverseSortDirection(pSortingColumn^.SortDirection);
      Exit;
    end;
    dec(i);
  end;

  new(pSortingColumn);
  pSortingColumn^.iField := iField;
  pSortingColumn^.SortDirection := SortDirection;
  Add(pSortingColumn);
end;

Destructor TFileListSorting.Destroy;
begin
  Clear;
  inherited;
end;

procedure TFileListSorting.Clear;
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    dispose(pSortingColumn);
    dec(i);
  end;

  Inherited Clear;
end;

function TFileListSorting.GetSortingDirection(iField : Integer) : TSortDirection;
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  Result := sdNone;

  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    if pSortingColumn^.iField = iField then
    begin
      Result := pSortingColumn^.SortDirection;
      break;
    end;
    dec(i);
  end;
end;

function TFileListSorting.GetSortFields: TFileListSortFields;
begin
  SetLength(Result, 0);
end;

function ReverseSortDirection(SortDirection: TSortDirection): TSortDirection;
begin
  case SortDirection of
    sdAscending:
      Result := sdDescending;
    sdDescending:
      Result := sdAscending;
  end;
end;

end.

