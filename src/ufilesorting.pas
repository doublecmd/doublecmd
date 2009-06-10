unit uFileSorting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uTypes, uColumns;

type

  TSortDirection = (sdNone, sdAscending, sdDescending);

  TFileSorting = record
    SortFunctions: TFileFunctions;
    SortDirection: TSortDirection;
  end;

  TFileSortings = array of TFileSorting;

  {en
     Compares two file records using file functions.
     @param(FileSorting
            Sorting which will be used to sort file records.)
     @param(ptr1
            First file)
     @param(ptr2
            Second file)
     @returns(-1 lesser
          @br  0 equal
          @br  1 greater)
  }
  function Compare(FileSorting: TFileSorting; ptr1, ptr2: PFileRecItem): Integer;

  {en
    Returns true if the file functions will sort by the given sort function.
  }
  function HasSortFunction(FileFunctions: TFileFunctions;
                           SortFunction: TFileFunction): Boolean;
  {en
     Adds a function to the given list of functions.
  }
  procedure AddSortFunction(var FileFunctions: TFileFunctions;
                            SortFunction: TFileFunction);

  {en
     Adds sorting by a function with a given sorting direction to a file sortings.
  }
  procedure AddSorting(var FileSortings: TFileSortings;
                       SortFunction: TFileFunction; SortDirection: TSortDirection);

  function IMulticolumnCompare(item1, item2:Pointer):Integer;

  function ICompareByDirectory(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByName(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByNameNoExt(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByExt (item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareBySize(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByDate(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;
  function ICompareByAttr(item1, item2:PFileRecItem; bSortNegative: Boolean):Integer;

  function ReverseSortDirection(SortDirection: TSortDirection): TSortDirection;


  var
    CurrentSorting: TFileSortings;

implementation

uses
  uOSUtils, uGlobs, uDCUtils;


function Compare(FileSorting: TFileSorting; ptr1, ptr2: PFileRecItem): Integer;
var
  i: Integer;
  bNegative: Boolean;
begin
  case FileSorting.SortDirection of
    sdAscending:
      bNegative := False;

    sdDescending:
      bNegative := True;

    else
      Exit;
  end;

  if Length(FileSorting.SortFunctions) > 0 then
  begin
    Result := 0;

    for i := 0 to Length(FileSorting.SortFunctions) - 1 do
    begin
      //------------------------------------------------------
      // Only DC internal functions supported.
      case FileSorting.SortFunctions[i] of
        fsfName:
          Result := ICompareByName(ptr1, ptr2, bNegative);
        fsfExtension:
          Result := ICompareByExt(ptr1, ptr2, bNegative);
        fsfSize:
          Result := ICompareBySize(ptr1, ptr2, bNegative);
        fsfAttr:
          Result := ICompareByAttr(ptr1, ptr2, bNegative);
        fsfPath:
          begin
             Result := mbCompareText(ptr1^.sPath, ptr2^.sPath);
             if bNegative then
               Result := -Result;
           end;
        fsfGroup:
          begin
             Result := mbCompareText(ptr1^.sGroup, ptr2^.sGroup);
             if bNegative then
               Result := -Result;
           end;
        fsfOwner:
          begin
             Result := mbCompareText(ptr1^.sOwner, ptr2^.sOwner);
             if bNegative then
               Result := -Result;
           end;
        fsfTime:
          Result := ICompareByDate(ptr1, ptr2, bNegative);
        fsfLinkTo:
          begin
             Result := mbCompareText(ptr1^.sLinkTo, ptr2^.sLinkTo);
             if bNegative then
               Result := -Result;
           end;
        fsfNameNoExtension:
          Result := ICompareByNameNoExt(ptr1, ptr2, bNegative);
      end;

      if Result <> 0 then
        Exit;
    end;
  end
  else
   Result := -1;
end;

function HasSortFunction(FileFunctions: TFileFunctions;
                         SortFunction: TFileFunction): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(FileFunctions) - 1 do
  begin
    if SortFunction = FileFunctions[i] then
      Exit(True);
  end;
  Result := False;
end;

procedure AddSortFunction(var FileFunctions: TFileFunctions;
                          SortFunction: TFileFunction);
begin
  SetLength(FileFunctions, Length(FileFunctions) + 1);
  FileFunctions[Length(FileFunctions) - 1] := SortFunction;
end;

procedure AddSorting(var FileSortings: TFileSortings;
                     SortFunction: TFileFunction; SortDirection: TSortDirection);
begin
  SetLength(FileSortings, Length(FileSortings) + 1);

  SetLength(FileSortings[Length(FileSortings) - 1].SortFunctions, 0);
  AddSortFunction(FileSortings[Length(FileSortings) - 1].SortFunctions, SortFunction);
  FileSortings[Length(FileSortings) - 1].SortDirection := SortDirection;
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
  i : Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result := 0;
  if item1 = item2 then Exit;

  // Put directories first.
  if gDirSortFirst then
  begin
    Result := ICompareByDirectory(item1, item2, False); // Ascending
    if Result <> 0 then Exit;
  end;

  for i := 0 to Length(CurrentSorting) - 1 do
  begin
    Result := uFileSorting.Compare(CurrentSorting[i], item1, item2);
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

  // Don't sort directories only by name.
  if FPS_ISDIR(item1^.iMode) or (FPS_ISLNK(item1^.iMode) and item1^.bLinkIsDir) or
     FPS_ISDIR(item2^.iMode) or (FPS_ISLNK(item2^.iMode) and item1^.bLinkIsDir) then
  begin
    // Sort by full name.
    Result := ICompareByName(item1, item2, bSortNegative);
  end
  else
  begin
    name1 := ExtractOnlyFileName(item1^.sName);
    name2 := ExtractOnlyFileName(item2^.sName);

    if gCaseSensitiveSort then
      Result := StrComp(PChar(name1), PChar(name2))
    else
      Result := mbCompareText(name1, name2);

    if bSortNegative then
      Result := -Result;
  end;
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

