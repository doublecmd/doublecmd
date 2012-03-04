unit uFileSorting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFileFunctions, uFile, uFileProperty, uDisplayFile;

type

  TSortDirection = (sdNone, sdAscending, sdDescending);

  TFileSorting = record
    SortFunctions: TFileFunctions;
    SortDirection: TSortDirection;
  end;

  TFileSortings = array of TFileSorting;

  { TFileSorter }

  TFileSorter = class
    private
      FSortList: TFiles;
      FDisplaySortList: TDisplayFiles;
      FFilesToInsert: TDisplayFiles;
      FSortings: TFileSortings;

      function MultiCompare(item1, item2: Pointer):Integer;
      function MultiCompareDisplay(item1, item2: Pointer):Integer;

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
      Procedure QuickSortDisplay(FList: PPointerList; L, R : Longint);
      procedure InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles);

      {en
         Checks the files list for supported properties and removes
         not supported sortings. Currently treats files as if they
         all had the same properties.
      }
      procedure CheckSupportedProperties(SupportedFileProperties: TFilePropertiesTypes);

    public
      {en
         Creates the sorter.
         @param(Files
                List of files to be sorted.)
         @param(FileSorting
                Sorting which will be used to sort file records.)
      }
      constructor Create(Files: TFiles; Sortings: TFileSortings);
      constructor Create(Files: TDisplayFiles; Sortings: TFileSortings);
      constructor Create(FilesToInsert, AlreadySortedFiles: TDisplayFiles;
                         Sortings: TFileSortings);

      procedure Sort;

      {en
         Sorts files in FilesToSort using ASorting.
      }
      class procedure Sort(FilesToSort: TFiles; const ASortings: TFileSortings);
      class procedure Sort(FilesToSort: TDisplayFiles; const ASortings: TFileSortings);
      class procedure InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles;
                                 const ASortings: TFileSortings);
  end;

  {en
    Returns true if the file functions will sort by the given sort function.
  }
  function HasSortFunction(FileFunctions: TFileFunctions;
                           SortFunction: TFileFunction): Boolean;
  function HasSortFunction(FileSortings: TFileSortings;
                           SortFunction: TFileFunction): Boolean;
  function GetSortDirection(FileSortings: TFileSortings;
                            SortFunction: TFileFunction): TSortDirection;
  {en
     Adds a function to the given list of functions.
  }
  procedure AddSortFunction(var FileFunctions: TFileFunctions;
                            SortFunction: TFileFunction);

  {en
     Adds sorting by functions with a given sorting direction to existing sorting.
  }
  procedure AddSorting(var Sortings: TFileSortings;
                       SortFunctions: TFileFunctions;
                       SortDirection: TSortDirection);
  {en
     Checks if there is a sorting by Name, NameNoExtension or Extension
     and adds such sortings if there isn't.
  }
  procedure AddSortingByNameIfNeeded(var FileSortings: TFileSortings);

  {en
     Creates a deep copy of sortings.
  }
  function CloneSortings(const Sortings: TFileSortings): TFileSortings;

  function ICompareByDirectory(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByName(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByNameNoExt(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByExt (item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareBySize(item1, item2: TFile; bSortNegative: Boolean):Integer;
  function ICompareByDate(date1, date2: TDateTime; bSortNegative: Boolean):Integer;
  function ICompareByAttr(item1, item2: TFile; bSortNegative: Boolean):Integer;

  function ReverseSortDirection(SortDirection: TSortDirection): TSortDirection;
  function ReverseSortDirection(Sortings: TFileSortings): TFileSortings;

implementation

uses
  uTypes, uGlobs, uDCUtils
  {$IFDEF fileSortingTime}
  , uDebug
  {$ENDIF}
  ;

{$IFDEF fileSortingTime}
var
  fileSortingTimer: TDateTime;
{$ENDIF}

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

function HasSortFunction(FileSortings: TFileSortings;
                         SortFunction: TFileFunction): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(FileSortings) - 1 do
  begin
    if HasSortFunction(FileSortings[i].SortFunctions, SortFunction) then
      Exit(True);
  end;
  Result := False;
end;

function GetSortDirection(FileSortings: TFileSortings;
                          SortFunction: TFileFunction): TSortDirection;
var
  i, j: Integer;
begin
  for i := 0 to Length(FileSortings) - 1 do
  begin
    for j := 0 to Length(FileSortings[i].SortFunctions) - 1 do
    begin
      if FileSortings[i].SortFunctions[j] = SortFunction then
        Exit(FileSortings[i].SortDirection);
    end;
  end;
  Result := sdNone;
end;

procedure AddSortFunction(var FileFunctions: TFileFunctions;
                          SortFunction: TFileFunction);
begin
  SetLength(FileFunctions, Length(FileFunctions) + 1);
  FileFunctions[Length(FileFunctions) - 1] := SortFunction;
end;

procedure AddSorting(var Sortings: TFileSortings;
                     SortFunctions: TFileFunctions;
                     SortDirection: TSortDirection);
var
  SortingIndex: Integer;
begin
  SortingIndex := Length(Sortings);
  SetLength(Sortings, SortingIndex + 1);
  Sortings[SortingIndex].SortFunctions := SortFunctions;
  Sortings[SortingIndex].SortDirection := SortDirection;
end;

procedure AddSorting(var FileSortings: TFileSortings;
                     SortFunction: TFileFunction; SortDirection: TSortDirection);
begin
  SetLength(FileSortings, Length(FileSortings) + 1);
  SetLength(FileSortings[Length(FileSortings) - 1].SortFunctions, 0);
  AddSortFunction(FileSortings[Length(FileSortings) - 1].SortFunctions, SortFunction);
  FileSortings[Length(FileSortings) - 1].SortDirection := SortDirection;
end;

procedure AddSortingByNameIfNeeded(var FileSortings: TFileSortings);
var
  bSortedByName: Boolean = False;
  bSortedByExtension: Boolean = False;
  i: Integer;
begin
  for i := 0 to Length(FileSortings) - 1 do
  begin
    if HasSortFunction(FileSortings[i].SortFunctions, fsfName) then
    begin
      bSortedByName := True;
      bSortedByExtension := True;
      Exit;
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
  end;

  if not bSortedByName then
  begin
    if not bSortedByExtension then
      AddSorting(FileSortings, fsfName, sdAscending)
    else
      AddSorting(FileSortings, fsfNameNoExtension, sdAscending);
  end
  else if not bSortedByExtension then
    AddSorting(FileSortings, fsfExtension, sdAscending);
  // else
  //   There is already a sorting by filename and extension.
end;

function CloneSortings(const Sortings: TFileSortings): TFileSortings;
var
  i, j: Integer;
begin
  SetLength(Result, Length(Sortings));
  for i := 0 to Length(Sortings) - 1 do
  begin
    SetLength(Result[i].SortFunctions, Length(Sortings[i].SortFunctions));
    for j := 0 to Length(Sortings[i].SortFunctions) - 1 do
      Result[i].SortFunctions[j] := Sortings[i].SortFunctions[j];
    Result[i].SortDirection := Sortings[i].SortDirection;
  end;
end;

function CloneAndAddNameSortingIfNeeded(const Sortings: TFileSortings): TFileSortings;
begin
  Result := CloneSortings(Sortings);

  // Add automatic sorting by name and/or extension if there wasn't any.
  AddSortingByNameIfNeeded(Result);
end;

function ICompareByDirectory(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  IsDir1, IsDir2: Boolean;
begin
  IsDir1 := item1.IsDirectory or item1.IsLinkToDirectory;
  IsDir2 := item2.IsDirectory or item2.IsLinkToDirectory;

  if (not IsDir1) and (not IsDir2) then
    Result := 0
  else if (not IsDir1) and IsDir2 then
    Result := 1
  else if IsDir1 and (not IsDir2) then
    Result := -1
  // Put '..' first.
  else if item1.Name = '..' then
    Result := -1
  else if item2.Name = '..' then
    Result := 1
  else if (gSortFolderMode <> sfmSortNameShowFirst) then
    Result := 0
  else
    Result := CompareStrings(item1.Name, item2.Name, gSortNatural, gSortCaseSensitivity);
end;

function ICompareByName(item1, item2: TFile; bSortNegative: Boolean):Integer;
begin
  Result := CompareStrings(item1.Name, item2.Name, gSortNatural, gSortCaseSensitivity);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByNameNoExt(item1, item2: TFile; bSortNegative: Boolean):Integer;
begin
  // Don't sort directories only by name.
  if item1.IsDirectory or item1.IsLinkToDirectory or
     item2.IsDirectory or item2.IsLinkToDirectory then
  begin
    // Sort by full name.
    Result := ICompareByName(item1, item2, bSortNegative);
  end
  else
  begin
    Result := CompareStrings(item1.NameNoExt, item2.NameNoExt, gSortNatural, gSortCaseSensitivity);

    if bSortNegative then
      Result := -Result;
  end;
end;

function ICompareByExt(item1, item2: TFile; bSortNegative: Boolean):Integer;
begin
  Result := CompareStrings(item1.Extension, item2.Extension, gSortNatural, gSortCaseSensitivity);

  if bSortNegative then
    Result := -Result;
end;

function ICompareByDate(date1, date2: TDateTime; bSortNegative: Boolean):Integer;
begin
  if date1 = date2 then
    Result := 0
  else
  begin
    if date1 < date2 then
      Result := -1
    else
      Result := +1;

    if bSortNegative then
      Result := -Result;
  end;
end;

function ICompareByAttr(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  Attr1, Attr2: TFileAttrs;
begin
  Attr1 := item1.Attributes;
  Attr2 := item2.Attributes;

  if Attr1 = Attr2 then
    Result := 0
  else
  begin
    if Attr1 > Attr2 then
      Result := -1
    else
      Result := +1;

    if bSortNegative then
      Result := -Result;
  end;
end;

function ICompareBySize(item1, item2: TFile; bSortNegative: Boolean):Integer;
var
  iSize1 : Int64;
  iSize2 : Int64;
begin
  iSize1 := item1.Size;
  iSize2 := item2.Size;

  if iSize1 = iSize2 then
    Result := 0
  else
  begin
    if iSize1 < iSize2 then
      Result := -1
    else
      Result := +1;

    if bSortNegative then
      Result := -Result;
  end;
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

function ReverseSortDirection(Sortings: TFileSortings): TFileSortings;
var
  i: Integer;
begin
  Result := CloneSortings(Sortings);
  for i := 0 to Length(Result) - 1 do
    Result[i].SortDirection := ReverseSortDirection(Result[i].SortDirection);
end;

{ TFileSorter }

constructor TFileSorter.Create(Files: TFiles; Sortings: TFileSortings);
begin
  FSortList := Files;
  FSortings := Sortings;

  if Assigned(FSortList) and (FSortList.Count > 0) then
    CheckSupportedProperties(FSortList.Items[0].SupportedProperties);

  inherited Create;
end;

constructor TFileSorter.Create(Files: TDisplayFiles; Sortings: TFileSortings);
begin
  FDisplaySortList := Files;
  FSortings := Sortings;

  if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 0) then
    CheckSupportedProperties(FDisplaySortList[0].FSFile.SupportedProperties);

  inherited Create;
end;

constructor TFileSorter.Create(FilesToInsert, AlreadySortedFiles: TDisplayFiles; Sortings: TFileSortings);
begin
  FFilesToInsert := FilesToInsert;
  FDisplaySortList := AlreadySortedFiles;
  FSortings := Sortings;

  if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 0) and
     Assigned(FFilesToInsert) and (FFilesToInsert.Count > 0) then
  begin
    CheckSupportedProperties(FDisplaySortList[0].FSFile.SupportedProperties);
    CheckSupportedProperties(FFilesToInsert[0].FSFile.SupportedProperties);
  end;

  inherited Create;
end;

procedure TFileSorter.CheckSupportedProperties(SupportedFileProperties: TFilePropertiesTypes);
var
  SortingIndex: Integer;
  FunctionIndex: Integer;
  i: Integer;
begin
  // Check if each sort function is supported.
  SortingIndex := 0;
  while SortingIndex < Length(FSortings) do
  begin
    FunctionIndex := 0;
    while FunctionIndex < Length(FSortings[SortingIndex].SortFunctions) do
    begin
      if not (TFileFunctionToProperty[FSortings[SortingIndex].SortFunctions[FunctionIndex]] <= SupportedFileProperties) then
      begin
        for i := FunctionIndex to Length(FSortings[SortingIndex].SortFunctions) - 2 do
          FSortings[SortingIndex].SortFunctions[i] := FSortings[SortingIndex].SortFunctions[i+1];
        SetLength(FSortings[SortingIndex].SortFunctions, Length(FSortings[SortingIndex].SortFunctions) - 1);
      end
      else
        Inc(FunctionIndex);
    end;

    if Length(FSortings[SortingIndex].SortFunctions) = 0 then
    begin
      for i := SortingIndex to Length(FSortings) - 2 do
        FSortings[i] := FSortings[i+1];
      SetLength(FSortings, Length(FSortings) - 1);
    end
    else
      Inc(SortingIndex);
  end;
end;

procedure TFileSorter.Sort;
begin
  {$IFDEF fileSortingTime}
  fileSortingTimer := Now;
  {$ENDIF}

  if Length(FSortings) > 0 then
  begin
    if Assigned(FFilesToInsert) and Assigned(FDisplaySortList) then
    begin
      InsertSort(FFilesToInsert, FDisplaySortList);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Insert sort time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end
    else if Assigned(FSortList) and (FSortList.Count > 1) then
    begin
      QuickSort(FSortList.List.List, 0, FSortList.List.Count-1);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Sorting FSFiles time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end
    else if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 1) then
    begin
      QuickSortDisplay(FDisplaySortList.List.List, 0, FDisplaySortList.List.Count-1);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Sorting DisplayFiles time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end;
  end;
end;

class procedure TFileSorter.Sort(FilesToSort: TFiles; const ASortings: TFileSortings);
var
  FileListSorter: TFileSorter;
begin
  FileListSorter := TFileSorter.Create(FilesToSort, CloneAndAddNameSortingIfNeeded(ASortings));
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

class procedure TFileSorter.Sort(FilesToSort: TDisplayFiles; const ASortings: TFileSortings);
var
  FileListSorter: TFileSorter;
begin
  FileListSorter := TFileSorter.Create(FilesToSort, CloneAndAddNameSortingIfNeeded(ASortings));
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

class procedure TFileSorter.InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles; const ASortings: TFileSortings);
var
  FileListSorter: TFileSorter;
begin
  FileListSorter := TFileSorter.Create(FilesToInsert, AlreadySortedFiles, CloneAndAddNameSortingIfNeeded(ASortings));
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

{ Return Values for ICompareByxxxx function

> 0 (positive)   Item1 is greater than Item2
  0              Item1 is equal to Item2
< 0 (negative)   Item1 is less than Item2
}

{
  This function is simples support of sorting
  directory (handle uglobs.gDirSortFirst)

  Result is 0 if both parametres is directory and equal
  or not a directory (both).

  Else return +/- as ICompare****
}

function TFileSorter.MultiCompare(item1, item2: Pointer):Integer;
var
  i : Integer;
begin
  Result := 0;
  if item1 = item2 then Exit;

  // Put directories first.
  if (gSortFolderMode <> sfmSortLikeFile) then
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

function TFileSorter.MultiCompareDisplay(item1, item2: Pointer): Integer;
var
  i : Integer;
begin
  Result := 0;
  if item1 = item2 then Exit;

  // Put directories first.
  if (gSortFolderMode <> sfmSortLikeFile) then
  begin
    Result := ICompareByDirectory(TDisplayFile(item1).FSFile, TDisplayFile(item2).FSFile, False); // Ascending
    if Result <> 0 then Exit;
  end;

  for i := 0 to Length(FSortings) - 1 do
  begin
    Result := Compare(FSortings[i], TDisplayFile(item1).FSFile, TDisplayFile(item2).FSFile);
    if Result <> 0 then Exit;
  end;
end;

function TFileSorter.Compare(FileSorting: TFileSorting; File1, File2: TFile): Integer;
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
        fsfGroup:
          begin
            Result := mbCompareText(File1.OwnerProperty.GroupStr,
                                    File2.OwnerProperty.GroupStr);
            if bNegative then
              Result := -Result;
          end;
        fsfOwner:
          begin
            Result := mbCompareText(File1.OwnerProperty.OwnerStr,
                                    File2.OwnerProperty.OwnerStr);
            if bNegative then
              Result := -Result;
          end;
        fsfModificationTime:
          Result := ICompareByDate(File1.ModificationTime,
                                   File2.ModificationTime,
                                   bNegative);
        fsfCreationTime:
          Result := ICompareByDate(File1.CreationTime,
                                   File2.CreationTime,
                                   bNegative);
        fsfLastAccessTime:
          Result := ICompareByDate(File1.LastAccessTime,
                                   File2.LastAccessTime,
                                   bNegative);
        fsfLinkTo:
          begin
            Result := mbCompareText(File1.LinkProperty.LinkTo,
                                    File2.LinkProperty.LinkTo);
            if bNegative then
              Result := -Result;
          end;
        fsfNameNoExtension:
          Result := ICompareByNameNoExt(File1, File2, bNegative);
        fsfType:
          Result := mbCompareText(File1.TypeProperty.Value,
                                  File2.TypeProperty.Value);
      end;

      if Result <> 0 then
        Exit;
    end;
  end
  else
   Result := -1;
end;

// From FPC: lists.inc.
Procedure TFileSorter.QuickSort(FList: PPointerList; L, R : Longint);
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

procedure TFileSorter.QuickSortDisplay(FList: PPointerList; L, R: Longint);
var
  I, J : Longint;
  P, Q : Pointer;
begin
 repeat
   I := L;
   J := R;
   P := FList^[ (L + R) div 2 ];
   repeat
     while MultiCompareDisplay(P, FList^[i]) > 0 do
       I := I + 1;
     while MultiCompareDisplay(P, FList^[J]) < 0 do
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
     QuickSortDisplay(FList, L, J);
   L := I;
 until I >= R;
end;

procedure TFileSorter.InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles);
var
  i, j, SortedIndex: PtrInt;
  Psrc, Pdst: PPointerList;
  Cnt: Integer;
begin
  if FFilesToInsert.Count > 0 then
  begin
    if FFilesToInsert.Count > 1 then
    begin
      // First sort the files to insert of which there should be only a small number.
      QuickSortDisplay(FilesToInsert.List.List, 0, FilesToInsert.List.Count-1);
    end;
    Psrc := FilesToInsert.List.List;
    Cnt := AlreadySortedFiles.List.Count;

    SortedIndex := 0;
    for i := 0 to FilesToInsert.Count - 1 do
    begin
       // This pointer might change when adding items due to reallocating memory,
       // so read it on every turn.
       Pdst := AlreadySortedFiles.List.List;

       while (SortedIndex < Cnt) and
             (MultiCompareDisplay(Psrc^[i], Pdst^[SortedIndex]) >= 0) do
         Inc(SortedIndex);

       if SortedIndex >= Cnt then
       begin
         // Add remaining files at the end.
         for j := i to FilesToInsert.Count - 1 do
           AlreadySortedFiles.List.Add(Psrc^[j]);
         Break;
       end
       else
       begin
         AlreadySortedFiles.List.Insert(SortedIndex, Psrc^[i]);
         Inc(SortedIndex);
         Inc(Cnt);
       end;
    end;
  end;
end;

end.

