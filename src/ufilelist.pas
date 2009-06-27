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
  SysUtils, Classes, uTypes, uColumns, uFileSorting;

Type

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
  end;

  PFileListSortingColumn = ^TFileListSortingColumn;

  TFileListSorting = class(TList)
  public
    Destructor Destroy; override;
    procedure AddSorting(iField : Integer; SortDirection : TSortDirection);
    procedure Clear; override;
    function GetSortingDirection(iField : Integer) : TSortDirection;
  end;

  PFileListSorting = ^TFileListSorting;

  procedure CopyListSelectedExpandNames(srcFileList, dstFileList: TFileList; sPath: String;
                                        bFullName: Boolean = True; bSkipFolder: Boolean = False);

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
  bSortedByName: Boolean;
  bSortedByExtension: Boolean;
  FileSortings: TFileSortings;
  FileListSorter: TListSorter = nil;
begin
  if (fList.Count = 0) or (ColumnsClass.ColumnsCount = 0) then Exit;

  CurrentSorting.Clear;

  for i := 0 to Sorting.Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(Sorting[i]);
    CurrentSorting.AddSorting(pSortingColumn^.iField, pSortingColumn^.SortDirection);
  end;

  bSortedByName := False;
  bSortedByExtension := False;

  SetLength(FileSortings, CurrentSorting.Count);

  for i := 0 to CurrentSorting.Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(CurrentSorting[i]);

    if (pSortingColumn^.iField >= 0) and
       (pSortingColumn^.iField < ColumnsClass.ColumnsCount) then
    begin
      Column := ColumnsClass.GetColumnItem(pSortingColumn^.iField);
      FileSortings[i].SortFunctions := Column.GetColumnFunctions;
      FileSortings[i].SortDirection := pSortingColumn^.SortDirection;

      if HasSortFunction(FileSortings[i].SortFunctions, fsfName) then
      begin
        bSortedByName := True;
        bSortedByExtension := True;
      end
      else if HasSortFunction(FileSortings[i].SortFunctions, fsfNameNoExtension)
      then
      begin
        bSortedByName := True;
      end
      else if HasSortFunction(FileSortings[i].SortFunctions, fsfExtension)
      then
      begin
        bSortedByExtension := True;
      end;
    end
    else
      Raise Exception.Create('Invalid column number in sorting - fix me');
  end;

  // Add automatic sorting by name and/or extension if there wasn't any.

  if not bSortedByName then
  begin
    if not bSortedByExtension then
      AddSorting(FileSortings, fsfName, sdAscending)
    else
      AddSorting(FileSortings, fsfNameNoExtension, sdAscending);
  end
  else
  begin
    if not bSortedByExtension then
      AddSorting(FileSortings, fsfExtension, sdAscending);
    // else
    //   There is already a sorting by filename and extension.
  end;

  // Sort.
  FileListSorter := TListSorter.Create(fList, FileSortings);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

function TFileList.GetCount:Integer;
begin
  Result:=flist.Count;
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

end.

