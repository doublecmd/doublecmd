{
   DRAGDROP.PAS -- simple realization of OLE drag and drop.

   Author: Jim Mischel

   Last modification date: 30/05/97

   Add some changes for compatibility with FPC/Lazarus

   Copyright (C) 2009 Alexander Koblov (Alexx2000@mail.ru)

   Some inspiration for drag-and-drop using CF_FILEGROUPDESCRIPTORW and CFU_FILECONTENTS:
     -http://msdn.microsoft.com/en-us/library/windows/desktop/bb776904%28v=vs.85%29.aspx#filecontents
     -http://www.unitoops.com/uoole/examples/outlooktest.htm
}

unit uOleDragDrop;

{$mode delphi}{$H+}

interface

uses
  DCBasicTypes, Windows, ActiveX, Classes, Controls, ShlObj, uDragDropEx;

type

  { IEnumFormatEtc }
  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FIndex: Integer;
  public
    constructor Create(Index: Integer = 0);
    function Next(celt: LongWord; out elt: FormatEtc; pceltFetched: pULong): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enum: IEnumFormatEtc): HResult; stdcall;
  end;

  { TDragDropInfo }
  TDragDropInfo = class(TObject)
  private
    FFileList: TStringList;
    FPreferredWinDropEffect: DWORD;
    function CreateHDrop(bUnicode: Boolean): HGlobal;
    function CreateFileNames(bUnicode: Boolean): HGlobal;
    function CreateURIs(bUnicode: Boolean): HGlobal;
    function CreateShellIdListArray: HGlobal;
    function MakeHGlobal(ptr: Pointer; Size: LongWord): HGlobal;
  public
    constructor Create(PreferredWinDropEffect: DWORD);
    destructor Destroy; override;
    procedure Add(const s: string);
    function MakeDataInFormat(const formatEtc: TFormatEtc): HGlobal;
    function CreatePreferredDropEffect(WinDropEffect: DWORD): HGlobal;
    property Files: TStringList Read FFileList;
  end;

  TDragDropTargetWindows = class; // forward declaration

  { TFileDropTarget знает, как принимать сброшенные файлы }
  TFileDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FHandle: HWND;
    FReleased: Boolean;
    FDragDropTarget: TDragDropTargetWindows;
  public
    constructor Create(DragDropTarget: TDragDropTargetWindows);
    {en
       Unregisters drag&drop target and releases the object (it is destroyed).
       This is the function that should be called to cleanup the object instead
       of Free. Do not use the object after calling it.
    }
    procedure FinalRelease;

    function DragEnter(const {%H-}dataObj: IDataObject; grfKeyState: LongWord;
      pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

    function DragOver(grfKeyState: LongWord; pt: TPoint;
      var dwEffect: LongWord): HResult; stdcall;

    function DragLeave: HResult; stdcall;

    function Drop(const dataObj: IDataObject; grfKeyState: LongWord;
      pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

    {en
       Retrieves the filenames from the HDROP format
       as a list of UTF-8 strings.
       @returns(List of filenames or nil in case of an error.)
    }
    class function GetDropFilenames(hDropData: HDROP): TStringList;

    {en
       Retrieves the filenames from the CFU_FILEGROUPDESCRIPTORW/CFU_FILEGROUPDESCRIPTOR format
       as a list of UTF-8 strings.
       @returns(List of filenames or nil in case of an error.)
    }
    function GetDropFileGroupFilenames(const dataObj: IDataObject; var Medium: TSTGMedium; Format: TFormatETC): TStringList;
    function SaveCfuContentToFile(const dataObj:IDataObject; Index:Integer; WantedFilename:String; FileInfo: PFileDescriptorW):boolean;

    {en
       Retrieves the text from the CF_UNICODETEXT/CF_TEXT format, will store this in a single file
       return filename as a list of a single UTF-8 string.
       @returns(List of filenames or nil in case of an error.)
    }
    function GetDropTextCreatedFilenames(var Medium: TSTGMedium; Format: TFormatETC): TStringList;
  end;

  { TFileDropSource - источник для перетаскивания файлов }
  TFileDropSource = class(TInterfacedObject, IDropSource)
    constructor Create;
    {$IF FPC_FULLVERSION < 020601}
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: longint): HResult; stdcall;
    {$ELSE}
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: DWORD): HResult; stdcall;
    {$ENDIF}

    {$IF FPC_FULLVERSION < 020601}
    function GiveFeedback(dwEffect: longint): HResult; stdcall;
    {$ELSE}
    function GiveFeedback(dwEffect: DWORD): HResult; stdcall;
    {$ENDIF}
  end;

  { THDropDataObject - объект данных с информацией о перетаскиваемых файлах }
  THDropDataObject = class(TInterfacedObject, IDataObject)
  private
    FDropInfo: TDragDropInfo;
  public
    constructor Create(PreferredWinDropEffect: DWORD);
    destructor Destroy; override;
    procedure Add(const s: string);
    { из IDataObject }
    function GetData(const formatetcIn: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc;
      {$IF FPC_FULLVERSION < 30200}const{$ELSE}var{$ENDIF} medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: LongWord;
      out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: LongWord;
      const advSink: IAdviseSink; out dwConnection: LongWord): HResult; stdcall;
    function DUnadvise(dwConnection: LongWord): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult; stdcall;
  end;

  TDragDropSourceWindows = class(TDragDropSource)
  public
    function  RegisterEvents(DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                             RequestDataEvent: uDragDropEx.TRequestDataEvent;// not handled in Windows
                             DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean; override;

    function DoDragDrop(const FileNamesList: TStringList;
                        MouseButton: TMouseButton;
                        ScreenStartPoint: TPoint
                       ): Boolean; override;
  end;

  TDragDropTargetWindows = class(TDragDropTarget)
  public
    constructor Create(Control: TWinControl); override;
    destructor  Destroy; override;

    function  RegisterEvents(DragEnterEvent: uDragDropEx.TDragEnterEvent;
                             DragOverEvent : uDragDropEx.TDragOverEvent;
                             DropEvent     : uDragDropEx.TDropEvent;
                             DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean; override;

    procedure UnregisterEvents; override;

  private
    FDragDropTarget: TFileDropTarget;
  end;

  function GetEffectByKeyState(grfKeyState: LongWord) : Integer;

  { These functions convert Windows-specific effect value to
  { TDropEffect values and vice-versa. }
  function WinEffectToDropEffect(dwEffect: LongWord): TDropEffect;
  function DropEffectToWinEffect(DropEffect: TDropEffect): LongWord;

  { Query DROPFILES structure for [BOOL fWide] parameter }
  function DragQueryWide( hGlobalDropInfo: HDROP ): boolean;

implementation

uses
  //Lazarus, Free-Pascal, etc.
  LazUTF8, SysUtils, ShellAPI, LCLIntf, ComObj,
  DCDateTimeUtils, Forms, DCConvertEncoding,

  //DC
  uOSUtils, fOptionsDragDrop, uShowMsg, UGlobs, DCStrUtils, DCOSUtils,
  uClipboard, uLng, uDebug, uShlObjAdditional, uOSForms;

var
  // Supported formats by the source.
  DataFormats: TList = nil;  // of TFormatEtc

procedure InitDataFormats;

  procedure AddFormat(FormatId: Word);
  var
    FormatEtc: PFormatEtc;
  begin
    if FormatId > 0 then
    begin
      New(FormatEtc);
      if Assigned(FormatEtc) then
      begin
        DataFormats.Add(FormatEtc);

        with FormatEtc^ do
        begin
          CfFormat := FormatId;
          Ptd := nil;
          dwAspect := DVASPECT_CONTENT;
          lindex := -1;
          tymed := TYMED_HGLOBAL;
        end;
      end;
    end;
  end;

begin
  DataFormats := TList.Create;

  AddFormat(CF_HDROP);
  AddFormat(CFU_PREFERRED_DROPEFFECT);
  AddFormat(CFU_FILENAME);
  AddFormat(CFU_FILENAMEW);
  // URIs disabled for now. This implementation does not work correct.
  // See bug http://doublecmd.sourceforge.net/mantisbt/view.php?id=692
  {
  AddFormat(CFU_UNIFORM_RESOURCE_LOCATOR);
  AddFormat(CFU_UNIFORM_RESOURCE_LOCATORW);
  }
  AddFormat(CFU_SHELL_IDLIST_ARRAY);
end;

procedure DestroyDataFormats;
var
  i : Integer;
begin
  if Assigned(DataFormats) then
  begin
    for i := 0 to DataFormats.Count - 1 do
      if Assigned(DataFormats.Items[i]) then
        Dispose(PFormatEtc(DataFormats.Items[i]));

    FreeAndNil(DataFormats);
  end;
end;


{ TEnumFormatEtc.Create }

constructor TEnumFormatEtc.Create(Index: Integer);
begin
  inherited Create;

  FIndex := Index;
end;

{ TEnumFormatEtc.Next извлекает заданное количество структур TFormatEtc в передаваемый массив elt.
  Извлекается celt элементов, начиная с текущей позиции в списке. }
function TEnumFormatEtc.Next(celt: LongWord; out elt: FormatEtc;
  pceltFetched: pULong): HResult;
var
  i: Integer;
  eltout: PFormatEtc;
begin
  // Support returning only 1 format at a time.
  if celt > 1 then celt := 1;
  eltout := @elt;
  i := 0;

  while (i < celt) and (FIndex < DataFormats.Count) do
  begin
    (eltout + i)^ := PFormatEtc(DataFormats.Items[FIndex])^;
    Inc(FIndex);
    Inc(i);
  end;

  if (pceltFetched <> nil) then pceltFetched^ := i;

  if (I = celt) then
    Result := S_OK
  else
    Result := S_FALSE;
end;

{ TEnumFormatEtc.Skip пропускает celt элементов списка, устанавливая текущую позицию
   на (CurrentPointer + celt) или на конец списка в случае переполнения. }
function TEnumFormatEtc.Skip(celt: LongWord): HResult;
begin
  if (celt <= DataFormats.Count - FIndex) then
  begin
    FIndex := FIndex + celt;
    Result := S_OK;
  end
  else
  begin
    FIndex := DataFormats.Count;
    Result := S_FALSE;
  end;
end;

{ TEnumFormatEtc.Reset устанавливает указатель текущей позиции на начало списка }
function TEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

{ TEnumFormatEtc.Clone копирует список структур }
function TEnumFormatEtc.Clone(out enum: IEnumFormatEtc): HResult;
begin
  enum := TEnumFormatEtc.Create(FIndex);
  Result := S_OK;
end;

{ TDragDropInfo.Create }
constructor TDragDropInfo.Create(PreferredWinDropEffect: DWORD);
begin
  inherited Create;
  FFileList := TStringList.Create;
  FPreferredWinDropEffect := PreferredWinDropEffect;
end;

{ TDragDropInfo.Destroy }
destructor TDragDropInfo.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

{ TDragDropInfo.Add }
procedure TDragDropInfo.Add(const s: string);
begin
  Files.Add(s);
end;

{ TDragDropInfo.MakeDataInFormat }
function TDragDropInfo.MakeDataInFormat(const formatEtc: TFormatEtc): HGlobal;
begin
  Result := 0;

  if (formatEtc.tymed = DWORD(-1)) or  // Transport medium not specified.
     (Boolean(formatEtc.tymed and TYMED_HGLOBAL)) // Support only HGLOBAL medium.
  then
  begin
    if formatEtc.CfFormat = CF_HDROP then
      begin
        Result := CreateHDrop(Win32Platform = VER_PLATFORM_WIN32_NT)
      end
    else if formatEtc.CfFormat = CFU_PREFERRED_DROPEFFECT then
      begin
        Result := CreatePreferredDropEffect(FPreferredWinDropEffect);
      end
    else if (formatEtc.CfFormat = CFU_FILENAME) then
      begin
        Result := CreateFileNames(False);
      end
    else if (formatEtc.CfFormat = CFU_FILENAMEW) then
      begin
        Result := CreateFileNames(True);
      end

    // URIs disabled for now. This implementation does not work correct.
    // See bug http://doublecmd.sourceforge.net/mantisbt/view.php?id=692
    {
    else if (formatEtc.CfFormat = CFU_UNIFORM_RESOURCE_LOCATOR) then
      begin
        Result := CreateURIs(False);
      end
    else if (formatEtc.CfFormat = CFU_UNIFORM_RESOURCE_LOCATORW) then
      begin
        Result := CreateURIs(True);
      end
    }

    else if (formatEtc.CfFormat = CFU_SHELL_IDLIST_ARRAY) then
      begin
        Result := CreateShellIdListArray;
      end;
  end;
end;

{ TDragDropInfo.CreateFileNames }
function TDragDropInfo.CreateFileNames(bUnicode: Boolean): HGlobal;
var
  FileList: AnsiString;
  wsFileList: WideString;
begin
  if Files.Count = 0 then Exit;
  if bUnicode then
    begin
      wsFileList := UTF8Decode(Self.Files[0]) + #0;
      Result := MakeHGlobal(PWideChar(wsFileList),
                            Length(wsFileList) * SizeOf(WideChar));
    end
    else
    begin
      FileList := CeUtf8ToAnsi(Self.Files[0]) + #0;
      Result := MakeHGlobal(PAnsiChar(FileList),
                            Length(FileList) * SizeOf(AnsiChar));
    end;
end;

{ TDragDropInfo.CreateURIs }
function TDragDropInfo.CreateURIs(bUnicode: Boolean): HGlobal;
var
  UriList: AnsiString;
  wsUriList: WideString;
  I: Integer;
begin
  wsUriList := '';
  for I := 0 to Self.Files.Count - 1 do
  begin
    if I > 0 then wsUriList := wsUriList + LineEnding;

    wsUriList := wsUriList
               + fileScheme + '//'  { don't put hostname }
               + UTF8Decode(URIEncode(StringReplace(Files[I], '\', '/', [rfReplaceAll] )));
  end;

  wsUriList := wsUriList + #0;

  if bUnicode then
      Result := MakeHGlobal(PWideChar(wsUriList),
                            Length(wsUriList) * SizeOf(WideChar))
    else
    begin
      // Wide to Ansi
      UriList := CeUtf8ToAnsi(UTF16ToUTF8(wsUriList));

      Result := MakeHGlobal(PAnsiChar(UriList),
                            Length(UriList) * SizeOf(AnsiChar));
    end;
end;

{ TDragDropInfo.CreateShellIdListArray }
function TDragDropInfo.CreateShellIdListArray: HGlobal;
var
  pidl: LPITEMIDLIST;
  pidlSize: Integer;
  pIdA: LPIDA = nil; // ShellIdListArray structure
  ShellDesktop: IShellFolder = nil;
  CurPosition: UINT;
  dwTotalSizeToAllocate: DWORD;
  I: Integer;

  function GetPidlFromPath(ShellFolder: IShellFolder; Path: WideString): LPITEMIDLIST;
  var
    chEaten: ULONG = 0;
    dwAttributes: ULONG = 0;
  begin
    if ShellFolder.ParseDisplayName(0, nil, PWideChar(Path), chEaten,
                                    Result, dwAttributes) <> S_OK then
    begin
      Result := nil;
    end;
  end;

  function GetPidlSize(Pidl: LPITEMIDLIST): Integer;
  var
    pidlTmp: LPITEMIDLIST;
  begin
    Result := 0;
    pidlTmp := pidl;

    while pidlTmp^.mkid.cb <> 0 do
    begin
      Result := Result + pidlTmp^.mkid.cb;
      pidlTmp := LPITEMIDLIST(LPBYTE(pidlTmp) + PtrInt(pidlTmp^.mkid.cb)); // Next Item.
    end;

    Inc(Result, SizeOf(BYTE) * 2); // PIDL ends with two zeros.
  end;

begin
  Result := 0;

  // Get Desktop shell interface.
  if SHGetDesktopFolder(ShellDesktop) = S_OK then
  begin
    // Get Desktop PIDL, which will be the root PIDL for the files' PIDLs.
    if SHGetFolderLocation(0, CSIDL_DESKTOP, 0, 0, pidl) = S_OK then
    begin
      pidlSize := GetPidlSize(pidl);

      // How much memory to allocate for the whole structure.
      // We don't know how much memory each PIDL takes yet
      // (estimate using desktop pidl size).
      dwTotalSizeToAllocate := SizeOf(_IDA.cidl)
                             + SizeOf(UINT) * (Files.Count + 1)  // PIDLs' offsets
                             + pidlSize     * (Files.Count + 1); // PIDLs

      pIda := AllocMem(dwTotalSizeToAllocate);

      // Number of files PIDLs (without root).
      pIdA^.cidl := Files.Count;

      // Calculate offset for the first pidl (root).
      CurPosition := SizeOf(_IDA.cidl) + SizeOf(UINT) * (Files.Count + 1);

      // Write first PIDL.
      pIdA^.aoffset[0] := CurPosition;
      CopyMemory(LPBYTE(pIda) + PtrInt(CurPosition), pidl, pidlSize);
      Inc(CurPosition, pidlSize);

      CoTaskMemFree(pidl);

      for I := 0 to Self.Files.Count - 1 do
      begin
        // Get PIDL for each file (if Desktop is the root, then
        // absolute paths are acceptable).
        pidl := GetPidlFromPath(ShellDesktop, UTF8Decode(Files[i]));

        if pidl <> nil then
        begin
          pidlSize := GetPidlSize(pidl);

          // If not enough memory then reallocate.
          if dwTotalSizeToAllocate < CurPosition + pidlSize then
          begin
            // Estimate using current PIDL's size.
            Inc(dwTotalSizeToAllocate, (Files.Count - i) * pidlSize);

            pIdA := ReAllocMem(pIda, dwTotalSizeToAllocate);

            if not Assigned(pIda) then
              Break;
          end;

          // Write PIDL.
{$R-}
          pIdA^.aoffset[i + 1] := CurPosition;
{$R+}

          CopyMemory(LPBYTE(pIdA) + PtrInt(CurPosition), pidl, pidlSize);
          Inc(CurPosition, pidlSize);

          CoTaskMemFree(pidl);
        end;
      end;

      if Assigned(pIda) then
      begin
        // Current position it at the end of the structure.
        Result := MakeHGlobal(pIdA, CurPosition);
        Freemem(pIda);
      end;
    end; // SHGetSpecialFolderLocation

    ShellDesktop := nil;
  end; // SHGetDesktopFolder
end;

{ TDragDropInfo.CreatePreferredDropEffect }
function TDragDropInfo.CreatePreferredDropEffect(WinDropEffect: DWORD) : HGlobal;
begin
  Result := MakeHGlobal(@WinDropEffect, SizeOf(WinDropEffect));
end;

{ TDragDropInfo.MakeHGlobal }
function TDragDropInfo.MakeHGlobal(ptr: Pointer; Size: LongWord): HGlobal;
var
  DataPointer : Pointer;
  DataHandle  : HGLOBAL;
begin
  Result := 0;

  if Assigned(ptr) then
  begin
    DataHandle := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, Size);

    if (DataHandle <> 0) then
    begin
      DataPointer := GlobalLock(DataHandle);

      if Assigned(DataPointer) then
      begin
        CopyMemory(DataPointer, ptr, Size);
        GlobalUnlock(DataHandle);
        Result := DataHandle;
      end
      else
      begin
        GlobalFree(DataHandle);
      end;
    end;
  end;
end;

{ TDragDropInfo.CreateHDrop }
function TDragDropInfo.CreateHDrop(bUnicode: Boolean): HGlobal;
var
  RequiredSize: Integer;
  I: Integer;
  hGlobalDropInfo: HGlobal;
  DropFiles: PDropFiles;
  FileList: AnsiString = '';
  wsFileList: WideString = '';
begin
  { Построим структуру TDropFiles в памяти, выделенной через
    GlobalAlloc. Область памяти сделаем глобальной и совместной,
    поскольку она, вероятно, будет передаваться другому процессу.

    Bring the filenames in a form,
    separated by #0 and ending with a double #0#0 }
  if bUnicode then
  begin
    for I := 0 to Self.Files.Count - 1 do
      wsFileList := wsFileList + UTF8Decode(Self.Files[I]) + #0;
    wsFileList := wsFileList + #0;
    { Определяем необходимый размер структуры }
    RequiredSize := SizeOf(TDropFiles) + Length(wsFileList) * SizeOf(WChar);
  end
  else
  begin
    for I := 0 to Self.Files.Count - 1 do
      FileList := FileList + CeUtf8ToAnsi(Self.Files[I]) + #0;

    FileList := FileList + #0;
    { Определяем необходимый размер структуры }
    RequiredSize := SizeOf(TDropFiles) + Length(FileList) * SizeOf(AnsiChar);
  end;


  hGlobalDropInfo := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, RequiredSize);
  if (hGlobalDropInfo <> 0) then
  begin
  { Заблокируем область памяти, чтобы к ней можно было обратиться }
    DropFiles := GlobalLock(hGlobalDropInfo);

  { Заполним поля структуры DropFiles
    pFiles -- смещение от начала структуры до первого байта массива с именами файлов. }
    DropFiles.pFiles := SizeOf(TDropFiles);

    if Windows.GetCursorPos(@DropFiles.pt) = False then
    begin
      DropFiles.pt.x := 0;

      DropFiles.pt.y := 0;
    end;

    DropFiles.fNC := True;  // Pass cursor coordinates as screen coords

    DropFiles.fWide := bUnicode;

  { Копируем имена файлов в буфер.
    Буфер начинается со смещения
    DropFiles + DropFiles.pFiles,
    то есть после последнего поля структуры.

    The pointer should be aligned nicely,
    because the TDropFiles record is not packed. }
    DropFiles := Pointer(DropFiles) + DropFiles.pFiles;

    if bUnicode then
      CopyMemory(DropFiles, PWideChar(wsFileList), Length(wsFileList) * SizeOf(WChar))
    else
      CopyMemory(DropFiles, PAnsiChar(FileList), Length(FileList) * SizeOf(AnsiChar));

    { Снимаем блокировку }
    GlobalUnlock(hGlobalDropInfo);
  end;

  Result := hGlobalDropInfo;
end;


{ TFileDropTarget.Create }
constructor TFileDropTarget.Create(DragDropTarget: TDragDropTargetWindows);
begin
  inherited Create;

  // Here RefCount is 1 - as set in TInterfacedObject.NewInstance,
  // but it's decremented back in TInterfacedObject.AfterConstruction
  // (when this constructor finishes). So we must manually again increase it.
  _AddRef;
  FReleased := False;
  FDragDropTarget := DragDropTarget;

  // Increases RefCount.
  ActiveX.CoLockObjectExternal(Self, True, False);

  // Increases RefCount.
  FHandle:= GetControlHandle(DragDropTarget.GetControl);
  if ActiveX.RegisterDragDrop(FHandle, Self) <> S_OK then
    FHandle := 0;
end;

{ TFileDropTarget.FinalRelease }
procedure TFileDropTarget.FinalRelease;
begin
  if not FReleased then
  begin
    FReleased := True;

    // Decreases reference count.
    ActiveX.CoLockObjectExternal(Self, False, True);

    // Check if window was not already destroyed.
    if (FHandle <> 0) and (IsWindow(FHandle)) then
    begin
      // Decreases reference count.
      ActiveX.RevokeDragDrop(FHandle);
      FHandle := 0;
    end
    else
      _Release; // Cannot revoke - just release reference.

    _Release; // For _AddRef in Create.
  end;
end;

function TFileDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: LongWord; pt: TPoint; var dwEffect: LongWord): HResult; stdcall;
var
  DropEffect: TDropEffect;
begin
  // dwEffect parameter states which effects are allowed by the source.
  dwEffect := dwEffect and GetEffectByKeyState(grfKeyState);

  if Assigned(FDragDropTarget.GetDragEnterEvent) then
  begin
      DropEffect := WinEffectToDropEffect(dwEffect);

      if FDragDropTarget.GetDragEnterEvent()(DropEffect, pt) = True then
      begin
        dwEffect := DropEffectToWinEffect(DropEffect);
        Result := S_OK
      end
      else
        Result := S_FALSE;
  end
  else
      Result := S_OK;
end;

{ TFileDropTarget.DragOver }
function TFileDropTarget.DragOver(grfKeyState: LongWord; pt: TPoint; var dwEffect: LongWord): HResult; stdcall;
var
  DropEffect: TDropEffect;
begin
  // dwEffect parameter states which effects are allowed by the source.
  dwEffect := dwEffect and GetEffectByKeyState(grfKeyState);

  if Assigned(FDragDropTarget.GetDragOverEvent) then
  begin
      DropEffect := WinEffectToDropEffect(dwEffect);

      if FDragDropTarget.GetDragOverEvent()(DropEffect, pt) = True then
      begin
        dwEffect := DropEffectToWinEffect(DropEffect);
        Result := S_OK
      end
      else
        Result := S_FALSE;
  end
  else
      Result := S_OK;
end;

{ TFileDropTarget.DragLeave }
function TFileDropTarget.DragLeave: HResult; stdcall;
begin
  if Assigned(FDragDropTarget.GetDragLeaveEvent) then
  begin
      if FDragDropTarget.GetDragLeaveEvent() = True then
        Result := S_OK
      else
        Result := S_FALSE;
  end
  else
      Result := S_OK;
end;

{ Обработка сброшенных данных. }
{ TFileDropTarget.Drop }
function TFileDropTarget.Drop(const dataObj: IDataObject; grfKeyState: LongWord;
  pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

var
  Medium: TSTGMedium;
  CyclingThroughFormat, ChosenFormat: TFormatETC;
  i: Integer;
  DropInfo: TDragDropInfo;
  FileNames, DragTextModeOfferedList: TStringList;
  SelectedFormatName:String;
  DropEffect: TDropEffect;
  Enum: IEnumFormatEtc;
  DragAndDropSupportedFormatList:TStringList;
  UnusedInteger : integer;

begin
  DragAndDropSupportedFormatList:=TStringList.Create;
  try
    FileNames:=nil;
    UnusedInteger:=0;

    dataObj._AddRef;

    { Получаем данные.
      Структура TFormatETC сообщает dataObj.GetData, как получить данные и в каком формате они должны храниться
      (эта информация содержится в структуре TSTGMedium). }

    //1. Let's build as quick list of the supported formats of what we've just been dropped.
    // We scan through all because sometimes the best one is not the first compatible one.
    OleCheck(DataObj.EnumFormatEtc(DATADIR_GET, Enum));
    while Enum.Next(1, CyclingThroughFormat, nil) = S_OK do
      DragAndDropSupportedFormatList.Add(IntToStr(CyclingThroughFormat.CfFormat));

    //2. Let's determine our best guess.
    // The order for this will be:
    // 1st) CF_HDROP (for legacy purpose, since DC was using it first).
    // 2nd) CFU_FILEGROUPDESCRIPTORW + CFU_FILECONTENTS (Outlook 2010 / Windows Live Mail, etc.)
    // 3rd) CFU_FILEGROUPDESCRIPTOR + CFU_FILECONTENTS (Outlook 2010 / Windows Live Mail, etc.)
    // 4th) We'll see if user would like to create a new text file from possible selected text dropped on the panel
    // CF_UNICODETEXT (Notepad++ / Wordpad / Firefox)
    // CF_TEXT (Notepad / Wordpad / Firefox)
    // CFU_HTML (Firefox)
    // Rich Text (Wordpad / Microsoft Word)
    ChosenFormat.CfFormat:=0;
    if DragAndDropSupportedFormatList.IndexOf(IntToStr(CF_HDROP))<>-1 then ChosenFormat.CfFormat:=CF_HDROP;
    if (ChosenFormat.CfFormat=0) AND (DragAndDropSupportedFormatList.IndexOf(IntToStr(CFU_FILEGROUPDESCRIPTORW))<>-1) AND (DragAndDropSupportedFormatList.IndexOf(IntToStr(CFU_FILECONTENTS))<>-1) then ChosenFormat.CfFormat:=CFU_FILEGROUPDESCRIPTORW;
    if (ChosenFormat.CfFormat=0) AND (DragAndDropSupportedFormatList.IndexOf(IntToStr(CFU_FILEGROUPDESCRIPTOR))<>-1) AND (DragAndDropSupportedFormatList.IndexOf(IntToStr(CFU_FILECONTENTS))<>-1) then ChosenFormat.CfFormat:=CFU_FILEGROUPDESCRIPTOR;

    // If we have no chosen format yet, let's attempt for text ones...
    if ChosenFormat.CfFormat=0 then
    begin
      DragTextModeOfferedList:=TStringList.Create;
      try
        if (DragAndDropSupportedFormatList.IndexOf(IntToStr(CFU_RICHTEXT))<>-1) then DragTextModeOfferedList.Add(gDragAndDropDesiredTextFormat[DropTextRichText_Index].Name);
        if (DragAndDropSupportedFormatList.IndexOf(IntToStr(CFU_HTML))<>-1) then DragTextModeOfferedList.Add(gDragAndDropDesiredTextFormat[DropTextHtml_Index].Name);
        if (DragAndDropSupportedFormatList.IndexOf(IntToStr(CF_UNICODETEXT))<>-1) then DragTextModeOfferedList.Add(gDragAndDropDesiredTextFormat[DropTextUnicode_Index].Name);
        if (DragAndDropSupportedFormatList.IndexOf(IntToStr(CF_TEXT))<>-1) then DragTextModeOfferedList.Add(gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].Name);
        SortThisListAccordingToDragAndDropDesiredFormat(DragTextModeOfferedList);

        if DragTextModeOfferedList.Count>0 then SelectedFormatName:=DragTextModeOfferedList.Strings[0] else SelectedFormatName:='';
        if (DragTextModeOfferedList.Count>1) AND (gDragAndDropAskFormatEachTime) then if not ShowInputListBox(rsCaptionForTextFormatToImport,rsMsgForTextFormatToImport,DragTextModeOfferedList,SelectedFormatName,UnusedInteger) then SelectedFormatName:='';
        if SelectedFormatName<>'' then
          begin
            if SelectedFormatName=gDragAndDropDesiredTextFormat[DropTextRichText_Index].Name then ChosenFormat.CfFormat:=CFU_RICHTEXT;
            if SelectedFormatName=gDragAndDropDesiredTextFormat[DropTextHtml_Index].Name then ChosenFormat.CfFormat:=CFU_HTML;
            if SelectedFormatName=gDragAndDropDesiredTextFormat[DropTextUnicode_Index].Name then ChosenFormat.CfFormat:=CF_UNICODETEXT;
            if SelectedFormatName=gDragAndDropDesiredTextFormat[DropTextSimpleText_Index].Name then ChosenFormat.CfFormat:=CF_TEXT;
          end;
        finally
          DragTextModeOfferedList.Free;
        end;
      end;

    //3. According to our best guess, let's store to "FileNames" list, the list of files we got (...or that we'll create!)
    if ChosenFormat.CfFormat<>0 then
    begin
      ChosenFormat.ptd := nil;
      ChosenFormat.dwAspect := DVASPECT_CONTENT;
      ChosenFormat.lindex := -1;
      ChosenFormat.tymed := TYMED_HGLOBAL;

      { Заносим данные в структуру Medium }
      Result:=dataObj.GetData(ChosenFormat, Medium);

      { Если все прошло успешно, далее действуем, как при операции файлового перетаскивания FMDD. }
      if Result = S_OK then
      begin
        if Medium.Tymed=TYMED_HGLOBAL then
        begin
          case ChosenFormat.CfFormat of
            CF_HDROP: FileNames := GetDropFilenames(Medium.hGlobal);
            CF_UNICODETEXT, CF_TEXT: FileNames := GetDropTextCreatedFilenames(Medium, ChosenFormat);
            else
              begin
                if ChosenFormat.CfFormat=CFU_FILEGROUPDESCRIPTORW then FileNames := GetDropFileGroupFilenames(dataObj, Medium, ChosenFormat);
                if ChosenFormat.CfFormat=CFU_FILEGROUPDESCRIPTOR  then FileNames := GetDropFileGroupFilenames(dataObj, Medium, ChosenFormat);
                if ChosenFormat.CfFormat=CFU_HTML then FileNames := GetDropTextCreatedFilenames(Medium, ChosenFormat);
                if ChosenFormat.CfFormat=CFU_RICHTEXT then FileNames := GetDropTextCreatedFilenames(Medium, ChosenFormat);
              end;
          end;
        end;
      end;
    end;

    //4. If we have some filenames in our list, continue to process the actual "Drop" of files
    if (Result = S_OK) then
    begin
      { Создаем объект TDragDropInfo }
      DropInfo := TDragDropInfo.Create(dwEffect);

      if Assigned(FileNames) then
      begin
        for i := 0 to FileNames.Count - 1 do DropInfo.Add(FileNames[i]);
        FreeAndNil(FileNames);
      end;

      { Если указан обработчик, вызываем его }
      if (Assigned(FDragDropTarget.GetDropEvent)) then
      begin
        // Set default effect by examining keyboard keys, taking into
        // consideration effects allowed by the source (dwEffect parameter).
        dwEffect := dwEffect and GetEffectByKeyState(grfKeyState);
        DropEffect := WinEffectToDropEffect(dwEffect);

        FDragDropTarget.GetDropEvent()(DropInfo.Files, DropEffect, pt);
        dwEffect := DropEffectToWinEffect(DropEffect);
      end;
      DropInfo.Free;

      if (Medium.PUnkForRelease = nil) then
        // Drop target must release the medium allocated by GetData.
        // This does the same as DragFinish(Medium.hGlobal) in this case,
        // but can support other media.
        ReleaseStgMedium(@Medium)
      else
        // Drop source is responsible for releasing medium via this object.
        IUnknown(Medium.PUnkForRelease)._Release;
    end;

    dataObj._Release;

  finally
    DragAndDropSupportedFormatList.Free;
  end;
end;

{ TFileDropTarget.GetDropFilenames }
class function TFileDropTarget.GetDropFilenames(hDropData: HDROP): TStringList;
var
  NumFiles: Integer;
  i: Integer;
  wszFilename: PWideChar;
  FileName: WideString;
  RequiredSize: Cardinal;

begin
  Result := nil;

  if hDropData <> 0 then
  begin
    Result := TStringList.Create;

    try
      NumFiles := DragQueryFileW(hDropData, $FFFFFFFF, nil, 0);

      for i := 0 to NumFiles - 1 do
      begin
        RequiredSize := DragQueryFileW(hDropData, i, nil, 0) + 1; // + 1 = terminating zero

        wszFilename := GetMem(RequiredSize * SizeOf(WideChar));
        if Assigned(wszFilename) then
        try
          if DragQueryFileW(hDropData, i, wszFilename, RequiredSize) > 0 then
          begin
             FileName := wszFilename;

             // Windows inserts '?' character where Wide->Ansi conversion
             // of a character was not possible, in which case filename is invalid.
             // This may happen if a non-Unicode application was the source.
             if Pos('?', FileName) = 0 then
               Result.Add(UTF16ToUTF8(FileName))
             else
               raise Exception.Create(rsMsgInvalidFilename + ': ' + LineEnding +
                                      UTF16ToUTF8(FileName));
          end;

        finally
          FreeMem(wszFilename);
        end;
      end;

    except
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

{ TFileDropTarget.SaveCfuContentToFile }
function TFileDropTarget.SaveCfuContentToFile(const dataObj: IDataObject;
  Index: Integer; WantedFilename: String; FileInfo: PFileDescriptorW): boolean;
const
  TEMPFILENAME='CfuContentFile.bin';
var
  Format  : TFORMATETC;
  Medium : TSTGMedium;
  Ifile, iStg : IStorage;
  tIID : PGuid;
  hFile: THandle;
  pvStrm: IStream;
  statstg: TStatStg;
  dwSize:     LongInt;
  AnyPointer: PAnsiChar;
  InnerFilename: String;
  StgDocFile: WideString;
  msStream:   TMemoryStream;
  i64Size, i64Move: {$IF FPC_FULLVERSION < 030002}Int64{$ELSE}QWord{$ENDIF};
begin
  result:=FALSE;
  InnerFilename:= ExtractFilepath(WantedFilename) + TEMPFILENAME;
  Format.cfFormat := CFU_FILECONTENTS;
  Format.dwAspect := DVASPECT_CONTENT;
  Format.lindex := Index;
  Format.ptd := nil;
  Format.TYMED := TYMED_ISTREAM OR TYMED_ISTORAGE or TYMED_HGLOBAL;

  if dataObj.GetData(Format, Medium) = S_OK then
  begin
    if Medium.TYMED = TYMED_ISTORAGE then
    begin
      iStg := IStorage(Medium.pstg);
      StgDocFile := UTF8Decode(InnerFilename);
      StgCreateDocfile(PWideChar(StgDocFile), STGM_CREATE Or STGM_READWRITE Or STGM_SHARE_EXCLUSIVE, 0, iFile);
      tIID:=nil;
      iStg.CopyTo(0, tIID, nil, iFile);
      iFile.Commit(0);
      iFile := nil;
      iStg := nil;
    end
    else if Medium.Tymed = TYMED_HGLOBAL then
    begin
      AnyPointer := GlobalLock(Medium.HGLOBAL);
      try
        hFile := mbFileCreate(InnerFilename);
        if hFile <> feInvalidHandle then
        begin
          FileWrite(hFile, AnyPointer^, GlobalSize(Medium.HGLOBAL));
          FileClose(hFile);
        end;
      finally
        GlobalUnlock(Medium.HGLOBAL);
      end;
      if Medium.PUnkForRelease = nil then GlobalFree(Medium.HGLOBAL);
    end
    else
    begin
      pvStrm:= IStream(Medium.pstm);
      // Figure out how large the data is
      if (FileInfo^.dwFlags and FD_FILESIZE <> 0) then
        i64Size:= Int64(FileInfo.nFileSizeLow) or (Int64(FileInfo.nFileSizeHigh) shl 32)
      else if (pvStrm.Stat(statstg, STATFLAG_DEFAULT) = S_OK) then
        i64Size:= statstg.cbSize
      else if (pvStrm.Seek(0, STREAM_SEEK_END, i64Size) = S_OK) then
        // Seek back to start of stream
        pvStrm.Seek(0, STREAM_SEEK_SET, i64Move)
      else begin
        Exit;
      end;

      // Create memory stream to convert to
      msStream:= TMemoryStream.Create;
      // Allocate size
      msStream.Size:= i64Size;
      // Read from the IStream into the memory for the TMemoryStream
      if pvStrm.Read(msStream.Memory, i64Size, @dwSize) = S_OK then
        msStream.Size:= dwSize
      else
        msStream.Size:= 0;
      // Release interface
      pvStrm:=nil;

      msStream.Position:=0;
      msStream.SaveToFile(UTF8ToSys(InnerFilename));
      msStream.Free;
    end;
  end;

  if mbFileExists(InnerFilename) then
  begin
    if mbRenameFile(InnerFilename, WantedFilename) then
    begin
      if (FileInfo^.dwFlags and FD_CREATETIME = 0) then TWinFileTime(FileInfo^.ftCreationTime):= 0;
      if (FileInfo^.dwFlags and FD_WRITESTIME = 0) then TWinFileTime(FileInfo^.ftLastWriteTime):= 0;
      if (FileInfo^.dwFlags and FD_ACCESSTIME = 0) then TWinFileTime(FileInfo^.ftLastAccessTime):= 0;
      Result:= mbFileSetTime(WantedFilename, TWinFileTime(FileInfo^.ftLastWriteTime),
                             TWinFileTime(FileInfo^.ftCreationTime), TWinFileTime(FileInfo^.ftLastAccessTime));
    end;
  end;
end;

{ TFileDropTarget.GetDropFileGroupFilenames }
function TFileDropTarget.GetDropFileGroupFilenames(const dataObj: IDataObject; var Medium: TSTGMedium; Format: TFormatETC): TStringList;
var
  SuffixStr: String;
  AnyPointer: Pointer;
  DC_FileDescriptorW: PFILEDESCRIPTORW;
  ActualFilename, DroppedTextFilename: String;
  NumberOfFiles, CopyNumber, IndexFile: Integer;
  DC_FileDescriptorA: PFILEDESCRIPTORA absolute DC_FileDescriptorW;
  DC_FileGroupeDescriptorW: PFILEGROUPDESCRIPTORW absolute AnyPointer;
begin
  Result := nil;

  AnyPointer := GlobalLock(Medium.HGLOBAL);
  try
    NumberOfFiles:= DC_FileGroupeDescriptorW.cItems;

    // Return the number of messages
    if NumberOfFiles > 0 then
    begin
      DC_FileDescriptorW:= AnyPointer + SizeOf(FILEGROUPDESCRIPTORW.cItems);

      Result:= TStringList.Create;

      for IndexFile:= 0 to Pred(NumberOfFiles) do
      begin
        if Format.CfFormat = CFU_FILEGROUPDESCRIPTORW then
          ActualFilename:= UTF16ToUTF8(UnicodeString(DC_FileDescriptorW^.cFileName))
        else begin
          ActualFilename:= CeSysToUTF8(AnsiString(DC_FileDescriptorA^.cFileName));
        end;

        DroppedTextFilename := GetTempFolderDeletableAtTheEnd + ActualFilename;

        if Result.IndexOf(DroppedTextFilename) <> -1 then
        begin
          CopyNumber := 2;
          repeat
            case gTypeOfDuplicatedRename of
              drLikeWindows7: SuffixStr:=' ('+IntToStr(CopyNumber)+')';
              drLikeTC: SuffixStr:='('+IntToStr(CopyNumber)+')';
            end;

            case gTypeOfDuplicatedRename of
              drLegacyWithCopy: DroppedTextFilename := GetTempFolderDeletableAtTheEnd+SysUtils.Format(rsCopyNameTemplate, [CopyNumber, ActualFilename]);
              drLikeWindows7, drLikeTC: DroppedTextFilename := GetTempFolderDeletableAtTheEnd+RemoveFileExt(ActualFilename) + SuffixStr + ExtractFileExt(ActualFilename);
            end;
            Inc(CopyNumber);
          until Result.IndexOf(DroppedTextFilename) = -1;
        end;

        if SaveCfuContentToFile(dataObj, IndexFile, DroppedTextFilename, DC_FileDescriptorW) then Result.Add(DroppedTextFilename);

        if Format.CfFormat = CFU_FILEGROUPDESCRIPTORW then
          Inc(DC_FileDescriptorW)
        else begin
          Inc(DC_FileDescriptorA);
        end;
      end;
    end;
  finally
    // Release the pointer
    GlobalUnlock(Medium.HGLOBAL);
  end;
end;

{ TFileDropTarget.GetDropTextCreatedFilenames }
function TFileDropTarget.GetDropTextCreatedFilenames(var Medium: TSTGMedium; Format: TFormatETC): TStringList;
var
  FlagKeepGoing:boolean;
  AnyPointer: Pointer;
  UnicodeCharPointer: PUnicodeChar;
  hFile: THandle;
  DroppedTextFilename: String;
  MyUnicodeString: UnicodeString;

  procedure SetDefaultFilename;
  begin
    DroppedTextFilename:=GetDateTimeInStrEZSortable(now)+rsDefaultSuffixDroppedText+'.txt';
    if Format.CfFormat=CFU_RICHTEXT then DroppedTextFilename:=GetDateTimeInStrEZSortable(now)+rsDefaultSuffixDroppedTextRichtextFilename+'.rtf';
    if Format.CfFormat=CFU_HTML then DroppedTextFilename:=GetDateTimeInStrEZSortable(now)+rsDefaultSuffixDroppedTextHTMLFilename+'.html';
    if (Format.CfFormat=CF_UNICODETEXT) AND not gDragAndDropSaveUnicodeTextInUFT8 then DroppedTextFilename:=GetDateTimeInStrEZSortable(now)+rsDefaultSuffixDroppedTextUnicodeUTF16Filename+'.txt';
    if (Format.CfFormat=CF_UNICODETEXT) AND gDragAndDropSaveUnicodeTextInUFT8 then DroppedTextFilename:=GetDateTimeInStrEZSortable(now)+rsDefaultSuffixDroppedTextUnicodeUTF8Filename+'.txt';
    if Format.CfFormat=CF_TEXT then DroppedTextFilename:=GetDateTimeInStrEZSortable(now)+rsDefaultSuffixDroppedTextSimpleFilename+'.txt';
  end;

begin
  result:=nil;
  FlagKeepGoing:=TRUE;
  SetDefaultFilename;
  if not gDragAndDropTextAutoFilename then FlagKeepGoing:=ShowInputQuery(rsCaptionForAskingFilename, rsMsgPromptAskingFilename, DroppedTextFilename);

  if FlagKeepGoing then
  begin
    if DroppedTextFilename='' then SetDefaultFilename; //Just minimal idot-proof...
    DroppedTextFilename:=GetTempFolderDeletableAtTheEnd+DroppedTextFilename;

    AnyPointer := GlobalLock(Medium.hGlobal);
    try
      hFile:= mbFileCreate(DroppedTextFilename);
      try
        case Format.CfFormat of
          CF_TEXT:
            begin
              FileWrite(hFile, PAnsiChar(AnyPointer)^, UTF8Length(PAnsiChar(AnyPointer)));
            end;

          CF_UNICODETEXT:
            begin
              if gDragAndDropSaveUnicodeTextInUFT8 then
              begin
                UnicodeCharPointer:=AnyPointer;
                MyUnicodeString:='';
                while UnicodeCharPointer^<>#$0000 do
                begin
                  MyUnicodeString:=MyUnicodeString+UnicodeCharPointer^;
                  inc(UnicodeCharPointer);
                end;

                FileWrite(hFile, PChar(#$EF+#$BB+#$BF)[0], 3); //Adding Byte Order Mask for UTF8.
                FileWrite(hFile, UTF16toUTF8(MyUnicodeString)[1], Length(UTF16toUTF8(MyUnicodeString)));
              end
              else
              begin
                FileWrite(hFile, PChar(#$FF+#$FE)[0], 2); //Adding Byte Order Mask for UTF16, Little-Endian first.
                FileWrite(hFile, PUnicodeChar(AnyPointer)^, Length(PUnicodeChar(AnyPointer))*2);
              end;
            end;

          else
            begin
              if Format.CfFormat=CFU_HTML then FileWrite(hFile, PAnsiChar(AnyPointer)^, UTF8Length(PAnsiChar(AnyPointer)));
              if Format.CfFormat=CFU_RICHTEXT then FileWrite(hFile, PAnsiChar(AnyPointer)^, UTF8Length(PAnsiChar(AnyPointer)));
            end;
        end;
      finally
        FileClose(hFile);
      end;

      result:=TStringList.Create;
      result.Add(DroppedTextFilename);
    finally
      GlobalUnlock(Medium.hGlobal);
    end;
  end;
end;

{ TFileDropSource.Create }
constructor TFileDropSource.Create;
begin
  inherited Create;
  _AddRef;
end;


{ TFileDropSource.QueryContinueDrag }
{$IF FPC_FULLVERSION < 020601}
function TFileDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: longint): HResult;
{$ELSE}
function TFileDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: DWORD): HResult;
{$ENDIF}
var
  Point:TPoint;
begin
  if (fEscapePressed) then
  begin
    Result := DRAGDROP_S_CANCEL;

    // Set flag to notify that dragging was canceled by the user.
    uDragDropEx.TransformDragging := False;
  end
  else if ((grfKeyState and (MK_LBUTTON or MK_MBUTTON or MK_RBUTTON)) = 0) then
  begin
    Result := DRAGDROP_S_DROP;
  end
  else
  begin
    if uDragDropEx.AllowTransformToInternal then
    begin
      GetCursorPos(Point);

      // Call LCL function, not the Windows one.
      // LCL version will return 0 if mouse is over a window belonging to another process.
      if LCLIntf.WindowFromPoint(Point) <> 0 then

      begin
        // Mouse cursor has been moved back into the application window.
        // Cancel external dragging.
        Result := DRAGDROP_S_CANCEL;

        // Set flag to notify that dragging has not finished,
        // but rather it is to be transformed into internal dragging.
        uDragDropEx.TransformDragging := True;
      end
      else
        Result := S_OK;  // Continue dragging
    end
    else
      Result := S_OK;  // Continue dragging
  end;
end;

{$IF FPC_FULLVERSION < 020601}
function TFileDropSource.GiveFeedback(dwEffect: longint): HResult;
{$ELSE}
function TFileDropSource.GiveFeedback(dwEffect: DWORD): HResult;
{$ENDIF}
begin
  case LongWord(dwEffect) of
    DROPEFFECT_NONE,
    DROPEFFECT_COPY,
    DROPEFFECT_MOVE,
    DROPEFFECT_LINK,
    DROPEFFECT_SCROLL:
      Result := DRAGDROP_S_USEDEFAULTCURSORS;
    else
      Result := S_OK;
  end;
end;


{ THDropDataObject.Create }
constructor THDropDataObject.Create(PreferredWinDropEffect: DWORD);
begin
  inherited Create;
  _AddRef;
  FDropInfo := TDragDropInfo.Create(PreferredWinDropEffect);
end;

{ THDropDataObject.Destroy }
destructor THDropDataObject.Destroy;
begin
  if (FDropInfo <> nil) then FDropInfo.Free;
  inherited Destroy;
end;

{ THDropDataObject.Add }
procedure THDropDataObject.Add(const s: string);
begin
  FDropInfo.Add(s);
end;

{ THDropDataObject.GetData }
function THDropDataObject.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC;
  { Необходимо обнулить все поля medium на случай ошибки }
  medium.tymed := 0;
  medium.hGlobal := 0;
  medium.PUnkForRelease := nil;

  { Если формат поддерживается, создаем и возвращаем данные }
  if (QueryGetData(formatetcIn) = S_OK) then
  begin
    if (FDropInfo <> nil) then
    begin
      { Create data in specified format. }
      { The hGlobal will be released by the caller of GetData. }
      medium.hGlobal := FDropInfo.MakeDataInFormat(formatetcIn);

      if medium.hGlobal <> 0 then
      begin
        medium.tymed := TYMED_HGLOBAL;
        Result := S_OK;
      end;
    end;
  end;
end;

{ THDropDataObject.GetDataHere }
function THDropDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  Result := DV_E_FORMATETC; { К сожалению, не поддерживается }
end;

{ THDropDataObject.QueryGetData }
function THDropDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
var
  i:Integer;

begin
  with formatetc do
    if dwAspect = DVASPECT_CONTENT then
    begin
      Result := DV_E_FORMATETC; // begin with 'format not supported'

      // See if the queried format is supported.
      for i := 0 to DataFormats.Count - 1 do
      begin
        if Assigned(DataFormats[i]) then
        begin
          if cfFormat = PFormatEtc(DataFormats[i])^.CfFormat then
          begin
            // Format found, see if transport medium is supported.
            if (tymed = DWORD(-1)) or
               (Boolean(tymed and PFormatEtc(DataFormats[i])^.tymed)) then
            begin
              Result := S_OK;
            end
            else
              Result := DV_E_TYMED;   // transport medium not supported

            Exit; // exit if format found (regardless of transport medium)
          end
        end
      end
    end
    else
      Result := DV_E_DVASPECT;  // aspect not supported
end;

{ THDropDataObject.GetCanonicalFormatEtc }
function THDropDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

{ THDropDataObject.SetData }
function THDropDataObject.SetData(const formatetc: TFormatEtc;
  {$IF FPC_FULLVERSION < 30200}const{$ELSE}var{$ENDIF} medium: TStgMedium;
  fRelease: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;


{ THDropDataObject.EnumFormatEtc возвращает список поддерживаемых форматов}
function THDropDataObject.EnumFormatEtc(dwDirection: LongWord;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  { Поддерживается только Get. Задать содержимое данных нельзя }
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := TEnumFormatEtc.Create;
    Result := S_OK;
  end
  else
  begin
    enumFormatEtc := nil;
    Result := E_NOTIMPL;
  end;
end;

{ THDropDataObject.DAdviseDAdvise не поддерживаются}
function THDropDataObject.DAdvise(const formatetc: TFormatEtc;
  advf: LongWord; const advSink: IAdviseSink; out dwConnection: LongWord): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

{ THDropDataObject.DUnadvise }
function THDropDataObject.DUnadvise(dwConnection: LongWord): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

{ THDropDataObject.EnumDAdvise }
function THDropDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;


function GetEffectByKeyState(grfKeyState: LongWord): Integer;
begin
  Result := DROPEFFECT_COPY; { default effect }

  if (grfKeyState and MK_CONTROL) > 0 then
  begin
    if (grfKeyState and MK_SHIFT) > 0 then
      Result := DROPEFFECT_LINK
    else
      Result := DROPEFFECT_COPY;
  end
  else if (grfKeyState and MK_SHIFT) > 0 then
    Result := DROPEFFECT_MOVE;
end;

function WinEffectToDropEffect(dwEffect: LongWord): TDropEffect;
begin
  case dwEffect of
    DROPEFFECT_COPY: Result := DropCopyEffect;
    DROPEFFECT_MOVE: Result := DropMoveEffect;
    DROPEFFECT_LINK: Result := DropLinkEffect;
    else             Result := DropNoEffect;
  end;
end;

function DropEffectToWinEffect(DropEffect: TDropEffect): LongWord;
begin
  case DropEffect of
    DropCopyEffect: Result := DROPEFFECT_COPY;
    DropMoveEffect: Result := DROPEFFECT_MOVE;
    DropLinkEffect: Result := DROPEFFECT_LINK;
    else            Result := DROPEFFECT_NONE;
  end;
end;

function DragQueryWide( hGlobalDropInfo: HDROP ): boolean;
var DropFiles: PDropFiles;
begin
  DropFiles := GlobalLock( hGlobalDropInfo );
  Result := DropFiles^.fWide;
  GlobalUnlock( hGlobalDropInfo );
end;

{ ---------------------------------------------------------}
{ TDragDropSourceWindows }
function TDragDropSourceWindows.RegisterEvents(
                         DragBeginEvent  : uDragDropEx.TDragBeginEvent;
                         RequestDataEvent: uDragDropEx.TRequestDataEvent; // not Handled in Windows
                         DragEndEvent    : uDragDropEx.TDragEndEvent): Boolean;
begin
  inherited;

  // RequestDataEvent is not handled, because the system has control of all data transfer.

  Result := True; // confirm that events are registered
end;

function TDragDropSourceWindows.DoDragDrop(const FileNamesList: TStringList;
                                           MouseButton: TMouseButton;
                                           ScreenStartPoint: TPoint): Boolean;
var
  DropSource: TFileDropSource;
  DropData: THDropDataObject;
  Rslt: HRESULT;
  dwEffect: LongWord;
  I: Integer;
begin

    // Simulate drag-begin event.
    if Assigned(GetDragBeginEvent) then
    begin
      Result := GetDragBeginEvent()();
      if Result = False then Exit;
    end;

    // Create source-object
    DropSource:= TFileDropSource.Create;

    // and data object
    DropData:= THDropDataObject.Create(DROPEFFECT_COPY { default effect } );

    for I:= 0 to FileNamesList.Count - 1 do
      DropData.Add (FileNamesList[i]);

    // Start OLE Drag&Drop
    Rslt:= ActiveX.DoDragDrop(DropData, DropSource,
                      DROPEFFECT_MOVE or DROPEFFECT_COPY or DROPEFFECT_LINK, // Allowed effects
                      @dwEffect);

    case Rslt of
      DRAGDROP_S_DROP:
        begin
          FLastStatus := DragDropSuccessful;
          Result := True;
        end;

      DRAGDROP_S_CANCEL:
        begin
          FLastStatus := DragDropAborted;
          Result := False;
        end;

      else
        begin
          MessageBox(0, PAnsiChar(SysErrorMessage(Rslt)), nil, MB_OK or MB_ICONERROR);
          FLastStatus := DragDropError;
          Result := False;
        end;
    end;

    // Simulate drag-end event. This must be called here,
    // after DoDragDrop returns from the system.
    if Assigned(GetDragEndEvent) then
    begin
      if Result = True then
        Result := GetDragEndEvent()()
      else
        GetDragEndEvent()()
    end;

    // Release created objects.
    DropSource._Release;
    DropData._Release;
end;


{ ---------------------------------------------------------}
{ TDragDropTargetWindows }

constructor TDragDropTargetWindows.Create(Control: TWinControl);
begin
  FDragDropTarget := nil;
  inherited Create(Control);
end;

destructor TDragDropTargetWindows.Destroy;
begin
  inherited Destroy;
  if Assigned(FDragDropTarget) then
  begin
    FDragDropTarget.FinalRelease;
    FDragDropTarget := nil;
  end;
end;

function TDragDropTargetWindows.RegisterEvents(
                                DragEnterEvent: uDragDropEx.TDragEnterEvent;
                                DragOverEvent : uDragDropEx.TDragOverEvent;
                                DropEvent     : uDragDropEx.TDropEvent;
                                DragLeaveEvent: uDragDropEx.TDragLeaveEvent): Boolean;
begin
  // Unregister if registered before.
  UnregisterEvents;

  inherited; // Call inherited Register now.

  GetControl.HandleNeeded; // force creation of the handle
  if GetControl.HandleAllocated = True then
  begin
    FDragDropTarget := TFileDropTarget.Create(Self);
    Result := True;
  end;
end;

procedure TDragDropTargetWindows.UnregisterEvents;
begin
  inherited;
  if Assigned(FDragDropTarget) then
  begin
    FDragDropTarget.FinalRelease; // Releasing will unregister events
    FDragDropTarget := nil;
  end;
end;


initialization
  OleInitialize(nil);
  InitDataFormats;

finalization
  OleUninitialize;
  DestroyDataFormats;

end.

