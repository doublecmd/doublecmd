unit uFileList;
{
Seksi Commander
----------------------------

  Class for storing list of files

  Licence   : GNU GPL 2
  Copyright : (2003)Radek Cervinka, Peter Cernoch 
  Contact   : pcernoch@volny.cz
              radek.cervinka@centrum.cz

TODO:
maybe protect Sort with TCriticalSection, because
in multithreaded program global variable
bSortNegative can be rewriten by other thread,
but Sort is Called only from main thread and
it is safe

}

{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, uTypes;

const

  //sort by specific field
  //ToDo:
  //  in future may be using enumerated values {sf_ByName, sf_ByExt,...} ?
  SF_BYNAME       = 0;
  SF_BYEXT        = 1;
  SF_BYSIZE       = 2;
  SF_BYDATE       = 3;
  SF_BYATTRIB     = 4;

Type
  TFileList = class
  private
    sortIn          : Integer;      //column for sorting
    negatSort       : Boolean;
    function GetCount:Integer;
  protected
    fList: TList;                   //store for file items
  public
    Constructor Create;
    Destructor  Destroy; override;

    procedure   Clear;

    function  AddItem(fi: PFileRecItem):Integer;
    procedure DeleteItem(iIndex: Integer);
    function  GetItem(iIndex: Integer) : PFileRecItem;
    function  GetFileName(iIndex: Integer): String;
    Function  CheckFileName(const sFileName:String):Integer;
    procedure UpdateFileInformation;
    procedure Sort(SortBy:Integer; bDirection:Boolean); overload;

    property Count      : Integer read GetCount;
  end;

{ this function couldn't be a method > type of parametr TList.Sort
  is function and not a method
}
  function ICompareByName(item1, item2:Pointer):Integer;
  function ICompareByExt (item1, item2:Pointer):Integer;
  function ICompareBySize(item1, item2:Pointer):Integer;
  function ICompareByDate(item1, item2:Pointer):Integer;
  function ICompareByAttr(item1, item2:Pointer):Integer;

  procedure CopyListSelectedExpandNames(srcFileList, dstFileList:TFileList; const sPath:String);

implementation

uses
  uGlobs, uPixmapManager, uOSUtils;

var
  bSortNegative:Boolean; // because implementation of TList.Sort

{
class constructor
}
Constructor TFileList.Create;
begin
  fList:=TList.Create;
  sortIn      := SF_BYNAME;
  negatSort   := FALSE;
end;


Destructor TFileList.Destroy;
begin
  Clear;
  FreeAndNil(fList);
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
    WriteLN('GetItem(i)^.sName = ', GetItem(i)^.sName);
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
Sort files by the default value in SortCol (e.g. SortIn) variable.
}
procedure TFileList.Sort(SortBy:Integer; bDirection:Boolean);
begin
  bSortNegative:=bDirection;
  if fList.Count=0 then Exit;
  case SortBy of
    SF_BYNAME:   fList.Sort(@ICompareByName);
    SF_BYEXT:    fList.Sort(@ICompareByExt);
    SF_BYSIZE:   fList.Sort(@ICompareBySize);
    SF_BYDATE:   fList.Sort(@ICompareByDate);
    SF_BYATTRIB: fList.Sort(@ICompareByAttr);
  else
    Raise Exception.Create('Unknow sort parametr - fix me');
  end;
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

function ICompareCheckDir(item1, item2: PFileRecItem; bCompareByName:Boolean=True):Integer;
begin
  Result:=0;
  if item1=item2 then Exit;

  if (not FPS_ISDIR(item1^.iMode)) and  (not FPS_ISDIR(item2^.iMode)) then  Exit;
  if (not FPS_ISDIR(item1^.iMode)) and FPS_ISDIR(item2^.iMode) then
  begin
    Result:=+1;
    Exit;
  end;
  if FPS_ISDIR(item1^.iMode) and (not FPS_ISDIR(item2^.iMode)) then
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

  if not bCompareByName then
  begin
    Result:=0; // used in by Attr, or Date
    Exit;
  end;
  Result:=StrComp(PChar(item1^.sName), PChar(item2^.sName));
  if bSortNegative then
    Result:=-Result;
end;

function ICompareByName(item1, item2:Pointer):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result:=0;
  if item1=item2 then Exit;;
  Result:= ICompareCheckDir(PFileRecItem(item1),PFileRecItem(item2));
  if Result<>0 then Exit;

  Result:=StrComp(PChar(PFileRecItem(item1)^.sName),PChar(PFileRecItem(item2)^.sName));

{  if FileRecPtr(item1)^.fName = FileRecPtr(item2)^.fName then
    Exit;

  if FileRecPtr(item1)^.fName > FileRecPtr(item2)^.fName then
    Result:=-1
  else
    Result:=+1;
}
  if bSortNegative then
    Result:=-Result;

end;

function ICompareByExt(item1, item2:Pointer):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result:=0;
  if item1=item2 then Exit;

  Result:= ICompareCheckDir(PFileRecItem(item1),PFileRecItem(item2));
  if Result<>0 then Exit;

  if PFileRecItem(item1)^.sExt = PFileRecItem(item2)^.sExt then
    Exit;

  if PFileRecItem(item1)^.sExt > PFileRecItem(item2)^.sExt then
    Result:=-1
  else
    Result:=+1;

  if bSortNegative then
    Result:=-Result;

end;

function ICompareByDate(item1, item2:Pointer):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result:=0;
  if item1=item2 then Exit;
  Result:= ICompareCheckDir(PFileRecItem(item1),PFileRecItem(item2), False);
  if Result<>0 then Exit;

  if PFileRecItem(item1)^.fTimeI = PFileRecItem(item2)^.fTimeI then
    Exit;

  if PFileRecItem(item1)^.fTimeI > PFileRecItem(item2)^.fTimeI then
    Result:=-1
  else
    Result:=+1;
  if bSortNegative then
    Result:=-Result;
end;

function ICompareByAttr(item1, item2:Pointer):Integer;
begin
  Result:=0;
  if item1=item2 then Exit;
  Result:= ICompareCheckDir(PFileRecItem(item1),PFileRecItem(item2), False);
  if Result<>0 then Exit;

  if PFileRecItem(item1)^.iMode = PFileRecItem(item2)^.iMode then
    Exit;

  if PFileRecItem(item1)^.iMode > PFileRecItem(item2)^.iMode then
    Result:=-1
  else
    Result:=+1;
  if bSortNegative then
    Result:=-Result;
end;

function ICompareBySize(item1, item2:Pointer):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result:=0;
  if item1=item2 then Exit;
  Result:= ICompareCheckDir(PFileRecItem(item1),PFileRecItem(item2), False);
  if Result<>0 then Exit;

  if PFileRecItem(item1)^.iSize = PFileRecItem(item2)^.iSize then
    Exit;

  if PFileRecItem(item1)^.iSize > PFileRecItem(item2)^.iSize then
    Result:=-1
  else
    Result:=+1;

  if bSortNegative then
    Result:=-Result;

end;

procedure TFileList.UpdateFileInformation;
var
  i:Integer;
  frp:PFileRecItem;
begin
  for i:=0 to fList.Count-1 do
  begin
    frp:=PFileRecItem(Flist.Items[i]);
    frp^.iIconID:=PixMapManager.GetIconByFile(frp);
  end;
end;

procedure CopyListSelectedExpandNames(srcFileList, dstFileList:TFileList; const sPath:String);
var
  xIndex:Integer;
  p:TFileRecItem;
begin
  Assert(srcFileList <> nil,'CopyListExpandNames: srcFileList=nil');
  Assert(dstFileList <> nil,'CopyListExpandNames: dstFileList=nil');
  dstFileList.Clear;
  for xIndex:=0 to srcFileList.Count-1 do
  begin
    p:=srcFileList.GetItem(xIndex)^;
    if (not p.bSelected) or (p.sName = '..') then Continue;
    p.sNameNoExt:=p.sName; //dstname
    p.sName:=sPath+p.sName;
    p.sPath:='';
    writeln(p.sName);
    dstFileList.AddItem(@p);
  end;
end;

end.

