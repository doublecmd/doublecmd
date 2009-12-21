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
  SysUtils, Classes, uTypes;

Type

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
       Indicates the number of items in the file list
    }
    property Count      : Integer read GetCount;
    {en
       Contain current file list directory
    }
    property CurrentDirectory : String read fDir write fDir;
  end;

  PFileList = ^TFileList;

implementation

uses
  LCLProc;

{
class constructor
}
Constructor TFileList.Create;
begin
  fList := TList.Create;
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

function TFileList.GetCount:Integer;
begin
  Result:=flist.Count;
end;

end.

