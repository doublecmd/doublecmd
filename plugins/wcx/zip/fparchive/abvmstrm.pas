(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbVMStrm.pas 3.04                           *}
{*********************************************************}
{* ABBREVIA: Virtual Memory Stream                       *}
{*********************************************************}

{$I AbDefine.inc}

unit AbVMStrm;

interface

uses
  Types,
  Classes;

const
  AB_VMSPageSize = 4096; {must be a power of two}
  AB_VMSMaxPages = 256;  {makes 1MB with the above value}

type
  PvmsPage = ^TvmsPage;
  TvmsPage = packed record
    vpStmOfs : Longint;  {value will be multiple of AB_VMSPageSize}
    vpLRU    : integer;  {'time' page was last accessed}
    vpDirty  : integer;  {has the page been changed?}
    vpData   : array [0..pred(AB_VMSPageSize)] of byte; {stream data}
  end;

type
  TAbVirtualMemoryStream = class(TStream)
    protected {private}
      vmsCachePage    : PvmsPage;   {the latest page used}
      vmsLRU          : Longint;    {'tick' value}
      vmsMaxMemToUse  : Longint;    {maximum memory to use for data}
      vmsMaxPages     : integer;    {maximum data pages}
      vmsPageList     : TList;      {page array, sorted by offset}
      vmsPosition     : Longint;    {position of stream}
      vmsSize         : Longint;    {size of stream}
      vmsSwapFileDir  : string;     {swap file directory}
      vmsSwapFileName : string;     {swap file name}
      vmsSwapFileSize : Longint;    {size of swap file}
      vmsSwapHandle   : integer;    {swap file handle}
    protected
      procedure vmsSetMaxMemToUse(aNewMem : Longint);

      function vmsAlterPageList(aNewMem : Longint) : Longint;
      procedure vmsFindOldestPage(var OldestInx : integer;
                                  var OldestPage: PvmsPage);
      function vmsGetNextLRU : Longint;
      function vmsGetPageForOffset(aOffset : Longint) : PvmsPage;

      procedure vmsSwapFileCreate;
      procedure vmsSwapFileDestroy;
      procedure vmsSwapFileRead(aPage : PvmsPage);
      procedure vmsSwapFileWrite(aPage : PvmsPage);
    public
      constructor Create;
        {-create the virtual memory stream}
      destructor Destroy; override;
        {-destroy the virtual memory stream}

      function Read(var Buffer; Count : Longint) : Longint; override;
        {-read from the stream into a buffer}
      function Write(const Buffer; Count : Longint) : Longint; override;
        {-write to the stream from a buffer}
      function Seek(Offset : Longint; Origin : Word) : Longint; override;
        {-seek to a particular point in the stream}

      procedure SetSize(NewSize : Longint); {$IFDEF VERSION3} override; {$ENDIF}  
        {-set the stream size}

      property MaxMemToUse : Longint
         read vmsMaxMemToUse write vmsSetMaxMemToUse;
        {-maximum memory to use for data before swapping to disk}
      property SwapFileDirectory : string
        read vmsSwapFileDir write vmsSwapFileDir;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
//  Libc,
  {$ENDIF}
  SysUtils,
  AbConst,
  AbExcept,                                                            
  AbUtils,
  AbArcTyp;

const
  LastLRUValue = $7FFFFFFF;

{===TAbVirtualMemoryStream===========================================}
constructor TAbVirtualMemoryStream.Create;
var
  Page : PvmsPage;
begin
  inherited Create;
  {create the page array}
  vmsPageList := TList.Create;
  {create the first page}
  New(Page);
  with Page^ do begin
    vpStmOfs := 0;
    vpLRU := vmsGetNextLRU;
    vpDirty := 0;
    FillChar(vpData, AB_VMSPageSize, 0);
  end;
  vmsPageList.Insert(0, pointer(Page));
  {prime the cache, from now on the cache will never be nil}
  vmsCachePage := Page;
  {assume we shall use at least 100K for data, we're already using 4K}
  MaxMemToUse := 100 * 1024;
end;
{--------}
destructor TAbVirtualMemoryStream.Destroy;
var
  Inx : integer;
  P : PvmsPage;
  
begin
  {destroy the swap file}
  vmsSwapFileDestroy;
  {throw away all pages in the list}
  if (vmsPageList <> nil) then begin
    for Inx := 0 to pred(vmsPageList.Count) do
      begin
      P:=PvmsPage(vmsPageList[Inx]);
      Dispose(P);
      end;
    vmsPageList.Destroy;
  end;
  {let our ancestor clean up}
  inherited Destroy;
end;
{--------}
function TAbVirtualMemoryStream.Read(var Buffer; Count : Longint) : Longint;
var
  BufAsBytes  : TByteArray absolute Buffer;
  BufInx      : Longint;
  Page        : PvmsPage;
  PageDataInx : integer;
  Posn        : Longint;
  BytesToGo   : Longint;
  BytesToRead : integer;
  StartOfs    : Longint;
begin
  {reading is complicated by the fact we can only read in chunks of
   AB_VMSPageSize: we need to partition out the overall read into a read
   from a partial page, zero or more reads from complete pages and
   then a possible read from a partial page}

  {initialise some variables, note that the complex calc in the
   expression for PageDataInx is the offset of the start of the page
   where Posn is found.}
  BufInx := 0;
  Posn := vmsPosition;
  PageDataInx := Posn - (Posn and (not pred(AB_VMSPageSize)));
  BytesToRead := AB_VMSPageSize - PageDataInx;
  {calculate the actual number of bytes to read - this depends on the
   current position and size of the stream}
  BytesToGo := Count;
  if (vmsSize < (vmsPosition + Count)) then
    BytesToGo := vmsSize - vmsPosition;
  if (BytesToGo < 0) then
    BytesToGo := 0;
  Result := BytesToGo;

  {while we have bytes to read, read them}
  while (BytesToGo <> 0) do begin
    if (BytesToRead > BytesToGo) then
      BytesToRead := BytesToGo;
    StartOfs := Posn and (not pred(AB_VMSPageSize));
    if (vmsCachePage^.vpStmOfs = StartOfs) then
      Page := vmsCachePage
    else
      Page := vmsGetPageForOffset(StartOfs);
    Move(Page^.vpData[PageDataInx], BufAsBytes[BufInx], BytesToRead);
    dec(BytesToGo, BytesToRead);
    inc(Posn, BytesToRead);
    inc(BufInx, BytesToRead);
    PageDataInx := 0;
    BytesToRead := AB_VMSPageSize;
  end;
  {remember our new position}
  vmsPosition := Posn;
end;
{--------}
function TAbVirtualMemoryStream.Seek( Offset : Longint;
                                      Origin : Word) : Longint;
begin
  case Origin of
    soFromBeginning : vmsPosition := Offset;
    soFromCurrent   : inc(vmsPosition, Offset);
    soFromEnd       : vmsPosition := vmsSize + Offset;
  else
    raise EAbVMSInvalidOrigin.Create( Origin, 0 );              
  end;
  Result := vmsPosition;
end;
{--------}
procedure TAbVirtualMemoryStream.SetSize(NewSize : Longint);
var
  Page : PvmsPage;
  Inx  : integer;
  NewFileSize : Longint;
begin
  if (NewSize < vmsSize) then begin
    {go through the page list discarding pages whose offset is greater
     than our new size; don't bother saving any data from them since
     it be beyond the end of the stream anyway}
    for Inx := pred(vmsPageList.Count) downto 0 do begin
      Page := PvmsPage(vmsPageList[Inx]);
      if (Page^.vpStmOfs >= NewSize) then
        Dispose(Page)
      else if (Page^.vpStmOfs < NewSize) then begin
        if (succ(Inx) < vmsPageList.Count) then
          vmsPageList.Count := succ(Inx);
        Break;
      end;
    end;
    {force the swap file file size in range, it'll be a multiple of
     AB_VMSPageSize}
    NewFileSize := pred(NewSize + AB_VMSPageSize) and
                   (not pred(AB_VMSPageSize));
    if (NewFileSize < vmsSwapFileSize) then
      vmsSwapFileSize := NewFileSize;
    {ignore the swap file itself}
  end;
  vmsSize := NewSize;
  if (vmsPosition > NewSize) then
    vmsPosition := NewSize;
end;
{--------}
function TAbVirtualMemoryStream.vmsAlterPageList(aNewMem : Longint) : Longint;
var
  NumPages : Longint;
  Page     : PvmsPage;
  i        : integer;
  OldestPageNum : integer;
begin
  {calculate the max number of pages required}
  NumPages := pred(aNewMem + AB_VMSPageSize) div AB_VMSPageSize;
  if (NumPages > AB_VMSMaxPages) then
    NumPages := AB_VMSMaxPages;
  {if the maximum number of pages means we have to shrink the current
   list, do so, tossing out the oldest pages first}
  if (NumPages < vmsPageList.Count) then
    for i := 1 to (vmsPageList.Count - NumPages) do begin
      {find the oldest page}
      vmsFindOldestPage(OldestPageNum, Page);
      {if it is dirty, write it out to the swap file}
      if (Page^.vpDirty <> 0) then begin
        vmsSwapFileWrite(Page);
      end;
      {remove it from the page list}
      vmsPageList.Delete(OldestPageNum);
      {free the page memory}
      Dispose(Page);
    end;
  {remember our new max number of pages}
  vmsMaxPages := NumPages;
  Result := NumPages * AB_VMSPageSize;
end;
{--------}
procedure TAbVirtualMemoryStream.vmsFindOldestPage(var OldestInx : integer;
                                                   var OldestPage: PvmsPage);
var
  OldestLRU : Longint;
  Inx       : integer;
  Page      : PvmsPage;
begin
  OldestInx := -1;
  OldestLRU := LastLRUValue;
  for Inx := 0 to pred(vmsMaxPages) do begin
    Page := PvmsPage(vmsPageList[Inx]);
    if (Page^.vpLRU < OldestLRU) then begin
      OldestInx := Inx;
      OldestLRU := Page^.vpLRU;
      OldestPage := Page;
    end;
  end;
end;
{--------}
function TAbVirtualMemoryStream.vmsGetNextLRU : Longint;
var
  Inx : integer;
begin
  if (vmsLRU = LastLRUValue) then begin
    {reset all LRUs in list}
    for Inx := 0 to pred(vmsPageList.Count) do
      PvmsPage(vmsPageList[Inx])^.vpLRU := 0;
    vmsLRU := 0;
  end;
  inc(vmsLRU);
  Result := vmsLRU;
end;
{--------}
function TAbVirtualMemoryStream.vmsGetPageForOffset(aOffset : Longint) : PvmsPage;
var
  Page     : PvmsPage;
  PageOfs  : Longint;
  L, M, R  : integer;
  OldestPageNum : integer;
  CreatedNewPage: boolean;
begin
  {using a sequential or a binary search (depending on the number of
   pages), try to find the page in the cache; we'll do a sequential
   search if the number of pages is very small, eg less than 4}
  if (vmsPageList.Count < 4) then begin
    L := vmsPageList.Count;
    for M := 0 to pred(vmsPageList.Count) do begin
      Page := PvmsPage(vmsPageList[M]);
      PageOfs := Page^.vpStmOfs;
      if (aOffset < PageOfs) then begin
        L := M;
        Break;
      end;
      if (aOffset = PageOfs) then begin
        Page^.vpLRU := vmsGetNextLRU;
        vmsCachePage := Page;
        Result := Page;
        Exit;
      end;
    end;
  end
  else {we need to do a binary search} begin
    L := 0;
    R := pred(vmsPageList.Count);
    repeat
      M := (L + R) div 2;
      Page := PvmsPage(vmsPageList[M]);
      PageOfs := Page^.vpStmOfs;
      if (aOffset < PageOfs) then
        R := pred(M)
      else if (aOffset > PageOfs) then
        L := succ(M)
      else {aOffset = PageOfs} begin
        Page^.vpLRU := vmsGetNextLRU;
        vmsCachePage := Page;
        Result := Page;
        Exit;
      end;
    until (L > R);
  end;
  {if we get here the page for the offset is not present in the page
   list, and once created/loaded, the page should be inserted at L}

  {enter a try..except block so that if a new page is created and an
   exception occurs, the page is freed}
  CreatedNewPage := false;
  Result := nil;
  try
    {if there is room to insert a new page, create one ready}
    if (vmsPageList.Count < vmsMaxPages) then begin
      New(Page);
      CreatedNewPage := true;
    end
    {otherwise there is no room for the insertion, so find the oldest
     page in the list and discard it}
    else {vmsMaxPages <= vmsPageList.Count} begin
      {find the oldest page}
      vmsFindOldestPage(OldestPageNum, Page);
      {if it is dirty, write it out to the swap file}
      if (Page^.vpDirty <> 0) then begin
        vmsSwapFileWrite(Page);
      end;
      {remove it from the page list}
      vmsPageList.Delete(OldestPageNum);
      {patch up the insertion point, in case the page just deleted was
       before it}
      if (OldestPageNum < L) then
        dec(L);
    end;
    {set all the page fields}
    with Page^ do begin
      vpStmOfs := aOffset;
      vpLRU := vmsGetNextLRU;
      vpDirty := 0;
      vmsSwapFileRead(Page);
    end;
    {insert the page into the correct spot}
    vmsPageList.Insert(L, pointer(Page));
    {return the page, remembering to save it in the cache}
    vmsCachePage := Page;
    Result := Page;
  except
    if CreatedNewPage then
      Dispose(Page);
  end;{try..except}
end;
{--------}
procedure TAbVirtualMemoryStream.vmsSetMaxMemToUse(aNewMem : Longint);
begin
  vmsMaxMemToUse := vmsAlterPageList(aNewMem);
end;
{--------}
procedure TAbVirtualMemoryStream.vmsSwapFileCreate;
begin
  if (vmsSwapHandle = 0) then begin
    vmsSwapFileName := AbCreateTempFile(vmsSwapFileDir);
    vmsSwapHandle := FileOpen(vmsSwapFileName, fmOpenReadWrite);
    if (vmsSwapHandle <= 0) then begin
      vmsSwapHandle := 0;
      DeleteFile(vmsSwapFileName);
      raise EAbVMSErrorOpenSwap.Create( vmsSwapFileName );             
    end;
    vmsSwapFileSize := 0;
  end;
end;
{--------}
procedure TAbVirtualMemoryStream.vmsSwapFileDestroy;
begin
  if (vmsSwapHandle <> 0) then begin
    FileClose(vmsSwapHandle);
    DeleteFile(vmsSwapFileName);
    vmsSwapHandle := 0;
  end;
end;
{--------}
procedure TAbVirtualMemoryStream.vmsSwapFileRead(aPage : PvmsPage);
var
  BytesRead : Longint;
  SeekResult: Longint;
begin
  if (vmsSwapHandle = 0) or (aPage^.vpStmOfs >= vmsSwapFileSize) then begin
    {there is nothing to be read from the disk (either the swap file
     doesn't exist or it's too small) so zero out the page data}
    FillChar(aPage^.vpData, AB_VMSPageSize, 0)
  end
  else {there is something to be read from the swap file} begin
    SeekResult := FileSeek(vmsSwapHandle, aPage^.vpStmOfs, 0);
    if (SeekResult = -1) then
      raise EAbVMSSeekFail.Create( vmsSwapFileName );                  
    BytesRead := FileRead(vmsSwapHandle, aPage^.vpData, AB_VMSPageSize);
    if (BytesRead <> AB_VMSPageSize) then
      raise EAbVMSReadFail.Create( AB_VMSPageSize, vmsSwapFileName );  
  end;
end;
{--------}
procedure TAbVirtualMemoryStream.vmsSwapFileWrite(aPage : PvmsPage);
var
  NewPos : Longint;
  BytesWrit : Longint;
  SeekResult: Longint;
begin
  if (vmsSwapHandle = 0) then
    vmsSwapFileCreate;
  SeekResult := FileSeek(vmsSwapHandle, aPage^.vpStmOfs, 0);
  if (SeekResult = -1) then
    raise EAbVMSSeekFail.Create( vmsSwapFileName );                    
  BytesWrit := FileWrite(vmsSwapHandle, aPage^.vpData, AB_VMSPageSize);
  if BytesWrit <> AB_VMSPageSize then
    raise EAbVMSWriteFail.Create( AB_VMSPageSize, vmsSwapFileName );   
  NewPos := aPage^.vpStmOfs + AB_VMSPageSize;
  if (NewPos > vmsSwapFileSize) then
    vmsSwapFileSize := NewPos;
end;
{--------}
function TAbVirtualMemoryStream.Write(const Buffer; Count : Longint) : Longint;
var
  BufAsBytes  : TByteArray absolute Buffer;
  BufInx      : Longint;
  Page        : PvmsPage;
  PageDataInx : integer;
  Posn        : Longint;
  BytesToGo   : Longint;
  BytesToWrite: integer;
  StartOfs    : Longint;
begin
  {writing is complicated by the fact we can only write in chunks of
   AB_VMSPageSize: we need to partition out the overall write into a
   write to a partial page, zero or more writes to complete pages and
   then a possible write to a partial page}

  {initialise some variables, note that the complex calc in the
   expression for PageDataInx is the offset of the start of the page
   where Posn is found.}
  BufInx := 0;
  Posn := vmsPosition;
  PageDataInx := Posn - (Posn and (not pred(AB_VMSPageSize)));
  BytesToWrite := AB_VMSPageSize - PageDataInx;
  {calculate the actual number of bytes to write}
  BytesToGo := Count;
  Result := BytesToGo;

  {while we have bytes to write, write them}
  while (BytesToGo <> 0) do begin
    if (BytesToWrite > BytesToGo) then
      BytesToWrite := BytesToGo;
    StartOfs := Posn and (not pred(AB_VMSPageSize));
    if (vmsCachePage^.vpStmOfs = StartOfs) then
      Page := vmsCachePage
    else
      Page := vmsGetPageForOffset(StartOfs);
    Move(BufAsBytes[BufInx], Page^.vpData[PageDataInx], BytesToWrite);
    Page^.vpDirty := 1;
    dec(BytesToGo, BytesToWrite);
    inc(Posn, BytesToWrite);
    inc(BufInx, BytesToWrite);
    PageDataInx := 0;
    BytesToWrite := AB_VMSPageSize;
  end;
  {remember our new position}
  vmsPosition := Posn;
  {if we've grown the stream, make a note of it}
  if (vmsPosition > vmsSize) then
    vmsSize := vmsPosition;
end;
{====================================================================}

end.
