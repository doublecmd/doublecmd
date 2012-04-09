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

  { TBaseSorter }

  TBaseSorter = class
    private
      FSortings: TFileSortings;

      {en
         Checks the files list for supported properties and removes
         not supported sortings. Currently treats files as if they
         all had the same properties.
      }
      procedure CheckSupportedProperties(SupportedFileProperties: TFilePropertiesTypes);

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
      class function Compare(const FileSorting: TFileSorting; File1, File2: TFile): Integer;

    public
      constructor Create(const Sortings: TFileSortings); reintroduce;
  end;

  { TFileSorter }

  TFileSorter = class(TBaseSorter)
    private
      FSortList: TFiles;

      function MultiCompare(item1, item2: Pointer):Integer;
      procedure QuickSort(FList: PPointerList; L, R : Longint);

    public
      {en
         Creates the sorter.
         @param(Files
                List of files to be sorted.)
         @param(FileSorting
                Sorting which will be used to sort file records.)
      }
      constructor Create(Files: TFiles; Sortings: TFileSortings);

      procedure Sort;

      {en
         Sorts files in FilesToSort using ASorting.
      }
      class procedure Sort(FilesToSort: TFiles; const ASortings: TFileSortings);
  end;

  { TDisplayFileSorter }

  TDisplayFileSorter = class(TBaseSorter)
    private
      FDisplaySortList: TDisplayFiles;
      FFileToInsert: TDisplayFile;
      FFilesToInsert: TDisplayFiles;
      FFileIndexToResort: Integer;
      FResortSingle: Boolean;
      FSequentialSearch: Boolean; // Use sequential search instead of binary

    protected
      procedure BinaryInsertSingle(FileToInsert: TDisplayFile; List: TFPList; L, R: Longint);
      procedure BinaryResortSingle(UnsortedIndex: Integer; PList: PPointerList; L, R : Longint);
      function BinarySearch(DisplayFile: Pointer; PList: PPointerList; L, R: Longint; out FoundIndex: Longint): Integer;
      procedure InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles);
      procedure InsertSort(FileToInsert: TDisplayFile; AlreadySortedFiles: TDisplayFiles);
      function MultiCompare(item1, item2: Pointer):Integer;
      procedure QuickSort(FList: PPointerList; L, R : Longint);
      {en
         The single file at index IndexToResort should be repositioned in the
         SortedFiles list. All other elements, except for the element at IndexToResort,
         must be already sorted.
      }
      procedure ResortSingle(IndexToResort: Integer; SortedFiles: TDisplayFiles);
      procedure SequentialInsertSingle(FileToInsert: TDisplayFile; List: TFPList);

    public
      constructor Create(Files: TDisplayFiles; Sortings: TFileSortings);
      constructor Create(FilesToInsert, AlreadySortedFiles: TDisplayFiles;
                         const Sortings: TFileSortings);
      constructor Create(FileToInsert: TDisplayFile; AlreadySortedFiles: TDisplayFiles;
                         const Sortings: TFileSortings; ASequentialSearch: Boolean = False);
      constructor Create(IndexToResort: Integer; SortedFiles: TDisplayFiles;
                         const Sortings: TFileSortings);

      procedure Sort;

      class procedure InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles;
                                 const ASortings: TFileSortings);
      class procedure InsertSort(FileToInsert: TDisplayFile; AlreadySortedFiles: TDisplayFiles;
                                 const ASortings: TFileSortings; ASequentialSearch: Boolean = False);
      class procedure ResortSingle(IndexToResort: Integer; SortedFiles: TDisplayFiles;
                                   const ASortings: TFileSortings);
      class procedure Sort(FilesToSort: TDisplayFiles; const ASortings: TFileSortings);
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

  function CloneAndAddSortByNameIfNeeded(const Sortings: TFileSortings): TFileSortings;
  function ReverseSortDirection(SortDirection: TSortDirection): TSortDirection;
  function ReverseSortDirection(Sortings: TFileSortings): TFileSortings;

implementation

uses
  DCBasicTypes, uGlobs, DCStrUtils, uDCUtils
  {$IFDEF fileSortingTime}
  , uDebug
  {$ENDIF}
  ;

{$IFDEF fileSortingTime}
var
  fileSortingTimer: TDateTime;
{$ENDIF}

procedure TFPListFastMove(CurIndex, NewIndex: Integer; PList: PPointerList);
var
  Temp: Pointer;
begin
  Temp := PList^[CurIndex];
  if NewIndex > CurIndex then
    System.Move(PList^[CurIndex+1], PList^[CurIndex], (NewIndex - CurIndex) * SizeOf(Pointer))
  else
    System.Move(PList^[NewIndex], PList^[NewIndex+1], (CurIndex - NewIndex) * SizeOf(Pointer));
  PList^[NewIndex] := Temp;
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

function CloneAndAddSortByNameIfNeeded(const Sortings: TFileSortings): TFileSortings;
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

{ TBaseSorter }

constructor TBaseSorter.Create(const Sortings: TFileSortings);
begin
  FSortings := Sortings;
  inherited Create;
end;

procedure TBaseSorter.CheckSupportedProperties(SupportedFileProperties: TFilePropertiesTypes);
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

class function TBaseSorter.Compare(const FileSorting: TFileSorting; File1, File2: TFile): Integer;
var
  i: Integer;
  bNegative: Boolean;
begin
  Result := 0;

  case FileSorting.SortDirection of
    sdAscending:
      bNegative := False;

    sdDescending:
      bNegative := True;

    else
      Exit;
  end;

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
end;

{ TDisplayFileSorter }

constructor TDisplayFileSorter.Create(Files: TDisplayFiles; Sortings: TFileSortings);
begin
  inherited Create(Sortings);
  FDisplaySortList := Files;

  if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 0) then
    CheckSupportedProperties(FDisplaySortList[0].FSFile.SupportedProperties);
end;

constructor TDisplayFileSorter.Create(FilesToInsert, AlreadySortedFiles: TDisplayFiles; const Sortings: TFileSortings);
begin
  inherited Create(Sortings);
  FFilesToInsert := FilesToInsert;
  FDisplaySortList := AlreadySortedFiles;

  if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 0) and
     Assigned(FFilesToInsert) and (FFilesToInsert.Count > 0) then
  begin
    CheckSupportedProperties(FDisplaySortList[0].FSFile.SupportedProperties);
    CheckSupportedProperties(FFilesToInsert[0].FSFile.SupportedProperties);
  end;
end;

constructor TDisplayFileSorter.Create(FileToInsert: TDisplayFile; AlreadySortedFiles: TDisplayFiles; const Sortings: TFileSortings; ASequentialSearch: Boolean);
begin
  inherited Create(Sortings);
  FFileToInsert := FileToInsert;
  FDisplaySortList := AlreadySortedFiles;
  FSequentialSearch := ASequentialSearch;

  if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 0) and
     Assigned(FFileToInsert) then
  begin
    CheckSupportedProperties(FDisplaySortList[0].FSFile.SupportedProperties);
    CheckSupportedProperties(FFileToInsert.FSFile.SupportedProperties);
  end;
end;

constructor TDisplayFileSorter.Create(IndexToResort: Integer; SortedFiles: TDisplayFiles; const Sortings: TFileSortings);
begin
  inherited Create(Sortings);
  FFileIndexToResort := IndexToResort;
  FResortSingle := True;
  FDisplaySortList := SortedFiles;

  if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 0) then
    CheckSupportedProperties(FDisplaySortList[0].FSFile.SupportedProperties);
end;

procedure TDisplayFileSorter.Sort;
begin
  {$IFDEF fileSortingTime}
  fileSortingTimer := Now;
  {$ENDIF}

  // Restore this check when independent SortFunctions are implemented and sorting
  // by directory condition (gSortFolderMode <> sfmSortLikeFile) is removed from
  // the sorter and moved into Sortings.
  //if Length(FSortings) > 0 then
  begin
    if FResortSingle and Assigned(FDisplaySortList) then
    begin
      ResortSingle(FFileIndexToResort, FDisplaySortList);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Resort time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end
    else if Assigned(FFileToInsert) and Assigned(FDisplaySortList) then
    begin
      InsertSort(FFileToInsert, FDisplaySortList);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Insert sort time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end
    else if Assigned(FFilesToInsert) and Assigned(FDisplaySortList) then
    begin
      InsertSort(FFilesToInsert, FDisplaySortList);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Insert sort time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end
    else if Assigned(FDisplaySortList) and (FDisplaySortList.Count > 1) then
    begin
      QuickSort(FDisplaySortList.List.List, 0, FDisplaySortList.List.Count-1);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Sorting DisplayFiles time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end;
  end;
end;

class procedure TDisplayFileSorter.InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles; const ASortings: TFileSortings);
var
  FileListSorter: TDisplayFileSorter;
begin
  FileListSorter := TDisplayFileSorter.Create(FilesToInsert, AlreadySortedFiles, ASortings);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

class procedure TDisplayFileSorter.InsertSort(FileToInsert: TDisplayFile; AlreadySortedFiles: TDisplayFiles; const ASortings: TFileSortings; ASequentialSearch: Boolean);
var
  FileListSorter: TDisplayFileSorter;
begin
  FileListSorter := TDisplayFileSorter.Create(FileToInsert, AlreadySortedFiles, ASortings, ASequentialSearch);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

class procedure TDisplayFileSorter.ResortSingle(IndexToResort: Integer; SortedFiles: TDisplayFiles; const ASortings: TFileSortings);
var
  FileListSorter: TDisplayFileSorter;
begin
  FileListSorter := TDisplayFileSorter.Create(IndexToResort, SortedFiles, ASortings);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

class procedure TDisplayFileSorter.Sort(FilesToSort: TDisplayFiles; const ASortings: TFileSortings);
var
  FileListSorter: TDisplayFileSorter;
begin
  FileListSorter := TDisplayFileSorter.Create(FilesToSort, ASortings);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

procedure TDisplayFileSorter.BinaryInsertSingle(FileToInsert: TDisplayFile; List: TFPList; L, R: Longint);
var
  CompareRes: Integer;
  FoundIndex: Longint;
begin
  if List.Count = 0 then
    FoundIndex := 0
  else
  begin
    CompareRes := BinarySearch(FileToInsert, List.List, L, R, FoundIndex);
    if CompareRes > 0 then
      Inc(FoundIndex); // Insert after because it's greater than FoundIndex item.
  end;
  List.Insert(FoundIndex, FileToInsert);
end;

procedure TDisplayFileSorter.BinaryResortSingle(UnsortedIndex: Integer; PList: PPointerList; L, R: Longint);
var
  CompareRes: Integer;
  FoundIndex: Longint;
begin
  CompareRes := BinarySearch(PList^[UnsortedIndex], PList, L, R, FoundIndex);
  if CompareRes = 0 then
    TFPListFastMove(UnsortedIndex, FoundIndex, PList)
  else
  begin
    if UnsortedIndex < FoundIndex then
    begin
      if CompareRes < 0 then
        Dec(FoundIndex);
    end
    else
    begin
      if CompareRes > 0 then
        Inc(FoundIndex);
    end;
    TFPListFastMove(UnsortedIndex, FoundIndex, PList);
  end;
end;

function TDisplayFileSorter.BinarySearch(
  DisplayFile: Pointer;
  PList: PPointerList;
  L, R: Longint;
  out FoundIndex: Longint): Integer;
var
  I, J, K : Longint;
begin
  I := L;
  J := R;
  repeat
    K := (I + J) div 2;
    Result := MultiCompare(DisplayFile, PList^[K]);
    if Result < 0 then
      J := K - 1
    else if Result > 0 then
      I := K + 1
    else
      Break;
  until I > J;
  FoundIndex := K;
end;

procedure TDisplayFileSorter.InsertSort(FilesToInsert, AlreadySortedFiles: TDisplayFiles);
var
  i, j: PtrInt;
  L, R, FoundIndex: Longint;
  Psrc: PPointerList;
  Pcur: Pointer;
  SearchResult: Integer;
  DestList: TFPList;
begin
  if FFilesToInsert.Count > 0 then
  begin
    if FFilesToInsert.Count = 1 then
    begin
      InsertSort(FFilesToInsert[0], AlreadySortedFiles);
      Exit;
    end
    else
    begin
      // First sort the files to insert of which there should be only a small number.
      QuickSort(FilesToInsert.List.List, 0, FilesToInsert.List.Count-1);
    end;

    Psrc := FilesToInsert.List.List;
    DestList := AlreadySortedFiles.List;
    L := 0;
    R := DestList.Count - 1;
    if R < 0 then
    begin
      // Add remaining files at the end.
      for j := 0 to FilesToInsert.Count - 1 do
        DestList.Add(Psrc^[j]);
    end
    else
    begin
      FoundIndex := 0;
      for i := 0 to FilesToInsert.Count - 1 do
      begin
        Pcur := Psrc^[i];
        SearchResult := BinarySearch(Pcur, DestList.List, L, R, FoundIndex);
        // Insert Pcur after FoundIndex if it was greater.
        if SearchResult > 0 then
          Inc(FoundIndex);

        if FoundIndex > R then
        begin
          // Add remaining files at the end.
          for j := i to FilesToInsert.Count - 1 do
            DestList.Add(Psrc^[j]);
          Break;
        end;

        DestList.Insert(FoundIndex, Pcur);
        L := FoundIndex + 1; // Next time start searching from the next element after the one just inserted.
        Inc(R); // Number of elements has increased so also increase right boundary.
      end;
    end;
  end;
end;

procedure TDisplayFileSorter.InsertSort(FileToInsert: TDisplayFile; AlreadySortedFiles: TDisplayFiles);
begin
  if FSequentialSearch then
    SequentialInsertSingle(FileToInsert, AlreadySortedFiles.List)
  else
    BinaryInsertSingle(FileToInsert, AlreadySortedFiles.List, 0, AlreadySortedFiles.Count - 1);
end;

function TDisplayFileSorter.MultiCompare(item1, item2: Pointer): Integer;
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

procedure TDisplayFileSorter.QuickSort(FList: PPointerList; L, R: Longint);
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

procedure TDisplayFileSorter.ResortSingle(IndexToResort: Integer; SortedFiles: TDisplayFiles);
var
  PUnsorted: Pointer;
  PSorted: PPointerList;
begin
  PSorted   := SortedFiles.List.List;
  PUnsorted := PSorted^[IndexToResort];
  // The element at IndexToResort must either be moved left or right,
  // or should stay where it is.
  if (IndexToResort > 0) and
     (MultiCompare(PUnsorted, PSorted^[IndexToResort - 1]) < 0) then
  begin
    if IndexToResort = 1 then
      SortedFiles.List.Exchange(IndexToResort, IndexToResort - 1)
    else
      BinaryResortSingle(IndexToResort, PSorted, 0, IndexToResort - 1);
  end
  else if (IndexToResort < SortedFiles.List.Count - 1) and
          (MultiCompare(PUnsorted, PSorted^[IndexToResort + 1]) > 0) then
  begin
    if IndexToResort = SortedFiles.List.Count - 2 then
      SortedFiles.List.Exchange(IndexToResort, IndexToResort + 1)
    else
      BinaryResortSingle(IndexToResort, PSorted, IndexToResort + 1, SortedFiles.List.Count - 1);
  end;
end;

procedure TDisplayFileSorter.SequentialInsertSingle(FileToInsert: TDisplayFile; List: TFPList);
var
  SortedIndex: PtrInt;
  Pdst: PPointerList;
begin
  SortedIndex := 0;
  Pdst := List.List;

  while (SortedIndex < List.Count) and
        (MultiCompare(FileToInsert, Pdst^[SortedIndex]) > 0) do
    Inc(SortedIndex);

  List.Insert(SortedIndex, FileToInsert);
end;

{ TFileSorter }

constructor TFileSorter.Create(Files: TFiles; Sortings: TFileSortings);
begin
  inherited Create(Sortings);
  FSortList := Files;

  if Assigned(FSortList) and (FSortList.Count > 0) then
    CheckSupportedProperties(FSortList.Items[0].SupportedProperties);
end;

procedure TFileSorter.Sort;
begin
  {$IFDEF fileSortingTime}
  fileSortingTimer := Now;
  {$ENDIF}

  // Restore this check when independent SortFunctions are implemented and sorting
  // by directory condition (gSortFolderMode <> sfmSortLikeFile) is removed from
  // the sorter and moved into Sortings.
  //if Length(FSortings) > 0 then
  begin
    if Assigned(FSortList) and (FSortList.Count > 1) then
    begin
      QuickSort(FSortList.List.List, 0, FSortList.List.Count-1);
      {$IFDEF fileSortingTime}
      DCDebug('FileSorter: Sorting FSFiles time: ', IntToStr(DateTimeToTimeStamp(Now - fileSortingTimer).Time));
      {$ENDIF}
    end;
  end;
end;

class procedure TFileSorter.Sort(FilesToSort: TFiles; const ASortings: TFileSortings);
var
  FileListSorter: TFileSorter;
begin
  FileListSorter := TFileSorter.Create(FilesToSort, ASortings);
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

// From FPC: lists.inc.
procedure TFileSorter.QuickSort(FList: PPointerList; L, R : Longint);
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

