unit uFileSorting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uColumns, uFile;

type

  TSortDirection = (sdNone, sdAscending, sdDescending);

  TFileSorting = record
    SortFunctions: TFileFunctions;
    SortDirection: TSortDirection;
  end;

  TFileSortings = array of TFileSorting;

  { TListSorter }

  TListSorter = class
    private
      FSortList: TFPList;
      FSortings: TFileSortings;

      function MultiCompare(item1, item2: Pointer):Integer;

      {en
         Compares two file records using file functions.
         @param(ptr1
                First file)
         @param(ptr2
                Second file)
         @returns(-1 lesser
              @br  0 equal
              @br  1 greater)
      }
      function Compare(FileSorting: TFileSorting; File1, File2: TFile): Integer;

      Procedure QuickSort(FList: PPointerList; L, R : Longint);

    public
      {en
         Creates the sorter.
         @param(List
                List to be sorted.)
         @param(FileSorting
                Sorting which will be used to sort file records.)
      }
      constructor Create(List: TFPList; Sortings: TFileSortings);

      procedure Sort;
  end;

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

  function ICompareByDirectory(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByName(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByNameNoExt(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByExt (item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareBySize(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByDate(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByAttr(item1, item2: TFile; bSortNegative: Boolean):Integer;

  function ReverseSortDirection(SortDirection: TSortDirection): TSortDirection;

implementation

uses
  uTypes, uOSUtils, uGlobs, uFileProperty;


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


function ICompareByDirectory(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  IsDir1, IsDir2: Boolean;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  IsDir1 := item1.IsDirectory or item1.IsLinkToDirectory;
  IsDir2 := item2.IsDirectory or item2.IsLinkToDirectory;

  if (not IsDir1) and (not IsDir2) then
    Exit
  else if (not IsDir1) and IsDir2 then
  begin
    Result:=+1;
  end
  else if IsDir1 and (not IsDir2) then
  begin
    Result:=-1;
  end
  // handle .. first
  else if item1.Name='..' then
  begin
    Result:=-1;
  end
  else if item2.Name='..' then
  begin
    Result:=+1;
  end;
end;

function ICompareByName(item1, item2: TFile; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result := 0;

  if gCaseSensitiveSort then
    Result := StrComp(PChar(item1.Name), PChar(item2.Name))
  else
    Result := mbCompareText(item1.Name, item2.Name);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByNameNoExt(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  name1, name2: string;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}
  Result := 0;

  // Don't sort directories only by name.
  if item1.IsDirectory or item1.IsLinkToDirectory or
     item2.IsDirectory or item2.IsLinkToDirectory then
  begin
    // Sort by full name.
    Result := ICompareByName(item1, item2, bSortNegative);
  end
  else
  begin
    name1 := item1.NameNoExt;
    name2 := item2.NameNoExt;

    if gCaseSensitiveSort then
      Result := StrComp(PChar(name1), PChar(name2))
    else
      Result := mbCompareText(name1, name2);

    if bSortNegative then
      Result := -Result;
  end;
end;

function ICompareByExt(item1, item2: TFile; bSortNegative: Boolean):Integer;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  if item1.Extension = item2.Extension then
    Exit;

  if gCaseSensitiveSort then
    Result := StrComp(PChar(item1.Extension), PChar(item2.Extension))
  else
    Result := mbCompareText(item1.Extension, item2.Extension);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByDate(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  Time1, Time2: TDateTime;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  // move this check before sorting starts?
  // then don't add sorting by date if not supported.
  if (not (fpDateTime in item1.SupportedProperties)) or
     (not (fpDateTime in item2.SupportedProperties)) then Exit;

  Time1 := (item1.Properties[fpDateTime] as TFileDateTimeProperty).Value;
  Time2 := (item2.Properties[fpDateTime] as TFileDateTimeProperty).Value;

  if Time1 = Time2 then Exit;

  if Time1 < Time2 then
    Result := -1
  else
    Result := +1;

  if bSortNegative then
    Result := -Result;
end;

function ICompareByAttr(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  Attr1, Attr2: TFileAttrs;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result:=0;

  if (not (fpAttributes in item1.SupportedProperties)) or
     (not (fpAttributes in item2.SupportedProperties)) then Exit;

  Attr1 := (item1.Properties[fpAttributes] as TFileAttributesProperty).Value;
  Attr2 := (item2.Properties[fpAttributes] as TFileAttributesProperty).Value;

  if Attr1 = Attr2 then
    Exit;

  if Attr1 > Attr2 then
    Result := -1
  else
    Result := +1;

  if bSortNegative then
    Result := -Result;
end;

function ICompareBySize(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  iSize1 : Int64;
  iSize2 : Int64;
begin
{> 0 (positive)   Item1 is less than Item2
  0              Item1 is equal to Item2
< 0 (negative)  Item1 is greater than Item2}

  Result := 0;

  if (not (fpSize in item1.SupportedProperties)) or
     (not (fpSize in item2.SupportedProperties)) then Exit;

  iSize1 := (item1.Properties[fpSize] as TFileSizeProperty).Value;
  iSize2 := (item2.Properties[fpSize] as TFileSizeProperty).Value;

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

{ TListSorter }

constructor TListSorter.Create(List: TFPList; Sortings: TFileSortings);
begin
  FSortList := List;
  FSortings := Sortings;

  inherited Create;
end;

procedure TListSorter.Sort;
begin
  if Assigned(FSortList) and Assigned(FSortList.List) and
     (FSortList.Count > 1) then
  begin
    QuickSort(FSortList.List, 0, FSortList.Count-1);
  end;
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

function TListSorter.MultiCompare(item1, item2: Pointer):Integer;
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
    Result := ICompareByDirectory(TFile(item1), TFile(item2), False); // Ascending
    if Result <> 0 then Exit;
  end;

  for i := 0 to Length(FSortings) - 1 do
  begin
    Result := Compare(FSortings[i], TFile(item1), TFile(item2));
    if Result <> 0 then Exit;
  end;
end;

function TListSorter.Compare(FileSorting: TFileSorting; File1, File2: TFile): Integer;
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
          Result := ICompareByName(File1, File2, bNegative);
        fsfExtension:
          Result := ICompareByExt(File1, File2, bNegative);
        fsfSize:
          Result := ICompareBySize(File1, File2, bNegative);
        fsfAttr:
          Result := ICompareByAttr(File1, File2, bNegative);
        fsfPath:
          begin
             Result := mbCompareText(File1.Path, File2.Path);
             if bNegative then
               Result := -Result;
           end;
{
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
}
        fsfTime:
          Result := ICompareByDate(File1, File2, bNegative);
{
        fsfLinkTo:
          begin
             Result := mbCompareText(ptr1^.sLinkTo, ptr2^.sLinkTo);
             if bNegative then
               Result := -Result;
           end;
}
        fsfNameNoExtension:
          Result := ICompareByNameNoExt(File1, File2, bNegative);
      end;

      if Result <> 0 then
        Exit;
    end;
  end
  else
   Result := -1;
end;

// From FPC: lists.inc.
Procedure TListSorter.QuickSort(FList: PPointerList; L, R : Longint);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while MultiCompare(P, FList^[i]) > 0 do
       I := I + 1;
     while MultiCompare(P, FList^[J]) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FList^[I];
       Flist^[I] := FList^[J];
       FList^[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   if L < J then
     QuickSort(FList, L, J);
   L := I;
 until I >= R;
end;

end.

