{

   DRAGDROP.PAS -- simple realization of OLE drag and drop.

   Author: Jim Mischel

   Last modification date: 30/05/97

   Add some changes for compatibility with FPC/Lazarus

   Copyright (C) 2009 Alexander Koblov (Alexx2000@mail.ru)

}

unit uOleDragDrop;

{$mode delphi}{$H+}

interface

uses
  Windows, ActiveX, Classes, Controls, uDragDropEx;

type

  { IEnumFormatEtc }

  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)

  private

    FIndex: Integer;

  public

    constructor Create(Index: Integer = 0);

    function Next(celt: LongWord; out elt: FormatEtc;
      pceltFetched: pULong): HResult; stdcall;

    function Skip(celt: LongWord): HResult; stdcall;

    function Reset: HResult; stdcall;

    function Clone(out enum: IEnumFormatEtc): HResult; stdcall;

  end;

  { TDragDropInfo }

  TDragDropInfo = class(TObject)

  private

    FInClientArea: boolean;

    FDropPoint: TPoint;

    FFileList: TStringList;

    FPreferredWinDropEffect: DWORD;


    function CreateHDrop(bUnicode: Boolean): HGlobal;
    function CreateFileNames(bUnicode: Boolean): HGlobal;
    function CreateURIs(bUnicode: Boolean): HGlobal;
    function CreateShellIdListArray: HGlobal;
    function MakeHGlobal(ptr: Pointer; Size: LongWord): HGlobal;

  public

    constructor Create(ADropPoint: TPoint; AInClient: boolean;
                       PreferredWinDropEffect: DWORD);

    destructor Destroy; override;

    procedure Add(const s: string);

    function MakeDataInFormat(const formatEtc: TFormatEtc): HGlobal;

    function CreatePreferredDropEffect(WinDropEffect: DWORD): HGlobal;

    property InClientArea: boolean Read FInClientArea;

    property DropPoint: TPoint Read FDropPoint;

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


    { из IDropTarget }

    function DragEnter(const dataObj: IDataObject; grfKeyState: LongWord;
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
  end;

  { TFileDropSource - источник

  для перетаскивания файлов }

  TFileDropSource = class(TInterfacedObject, IDropSource)

    constructor Create;

    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: longint): HResult; stdcall;

    function GiveFeedback(dwEffect: longint): HResult; stdcall;

  end;



  { THDropDataObject - объект данных с

  информацией о перетаскиваемых файлах }

  THDropDataObject = class(TInterfacedObject, IDataObject)

  private

    FDropInfo: TDragDropInfo;

  public

    constructor Create(ADropPoint: TPoint; AInClient: boolean;
                       PreferredWinDropEffect: DWORD);

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

    function SetData(const formatetc: TFormatEtc; const medium: TStgMedium;
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
  SysUtils, ShellAPI, ShlObj, LCLIntf, Win32Proc, uClipboard, uLng;

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
  AddFormat(CFU_UNIFORM_RESOURCE_LOCATOR);
  AddFormat(CFU_UNIFORM_RESOURCE_LOCATORW);
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


{ TEnumFormatEtc }

constructor TEnumFormatEtc.Create(Index: Integer);

begin

  inherited Create;

  FIndex := Index;

end;

{

  Next извлекает заданное количество

  структур TFormatEtc

  в передаваемый массив elt.

  Извлекается celt элементов, начиная с

  текущей позиции в списке.

}

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



  if (pceltFetched <> nil) then

    pceltFetched^ := i;



  if (I = celt) then

    Result := S_OK

  else

    Result := S_FALSE;

end;

{

  Skip пропускает celt элементов списка,

  устанавливая текущую позицию

  на (CurrentPointer + celt) или на конец

  списка в случае переполнения.

}

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

{ Reset устанавливает указатель текущей

позиции на начало списка }

function TEnumFormatEtc.Reset: HResult;

begin

  FIndex := 0;

  Result := S_OK;

end;

{ Clone копирует список структур }

function TEnumFormatEtc.Clone(out enum: IEnumFormatEtc): HResult;

begin

  enum := TEnumFormatEtc.Create(FIndex);

  Result := S_OK;

end;


{ TDragDropInfo }

constructor TDragDropInfo.Create(ADropPoint: TPoint; AInClient: boolean;
                                 PreferredWinDropEffect: DWORD);

begin

  inherited Create;

  FFileList := TStringList.Create;

  FDropPoint := ADropPoint;

  FInClientArea := AInClient;

  FPreferredWinDropEffect := PreferredWinDropEffect;

end;

destructor TDragDropInfo.Destroy;

begin

  FFileList.Free;

  inherited Destroy;

end;

procedure TDragDropInfo.Add(const s: string);

begin

  Files.Add(s);

end;

function TDragDropInfo.MakeDataInFormat(const formatEtc: TFormatEtc): HGlobal;
begin

  Result := 0;

  if (formatEtc.tymed = DWORD(-1)) or  // Transport medium not specified.
     (Boolean(formatEtc.tymed and TYMED_HGLOBAL)) // Support only HGLOBAL medium.
  then

  begin

    if formatEtc.CfFormat = CF_HDROP then
      begin
        Result := CreateHDrop(Win32Proc.UnicodeEnabledOS)
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

    // URIs disabled for now. It may be enough to just report that URL format
    // is supported, but not actually format data this way.
    {else if (formatEtc.CfFormat = CFU_UNIFORM_RESOURCE_LOCATOR) then

      begin
        Result := CreateURIs(False);
      end

    else if (formatEtc.CfFormat = CFU_UNIFORM_RESOURCE_LOCATORW) then

      begin
        Result := CreateURIs(True);
      end}

    else if (formatEtc.CfFormat = CFU_SHELL_IDLIST_ARRAY) then

      begin
        Result := CreateShellIdListArray;
      end;

  end;

end;

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

      FileList := FileList + Utf8ToAnsi(Self.Files[0]) + #0;

      Result := MakeHGlobal(PAnsiChar(FileList),
                            Length(FileList) * SizeOf(AnsiChar));
    end;
end;

function TDragDropInfo.CreateURIs(bUnicode: Boolean): HGlobal;

var

  UriList: AnsiString;
  wsUriList: WideString;
  I: Integer;

begin

  wsUriList := '';

  for I := 0 to Self.Files.Count - 1 do
  begin
    if I > 0 then
      wsUriList := wsUriList + LineEnding;

    wsUriList := wsUriList
               + fileScheme + '//'  { don't put hostname }
               + URIEncode(UTF8Decode(
                   StringReplace(Files[I], '\', '/', [rfReplaceAll] )));
  end;

  wsUriList := wsUriList + #0;

  if bUnicode then

      Result := MakeHGlobal(PWideChar(wsUriList),
                            Length(wsUriList) * SizeOf(WideChar))
    else

    begin

      // Wide to Ansi
      UriList := Utf8ToAnsi(UTF8Encode(wsUriList));

      Result := MakeHGlobal(PAnsiChar(UriList),
                            Length(UriList) * SizeOf(AnsiChar));

    end;

end;

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
    if SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, pidl) = S_OK then
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

    ShellDesktop._Release;

  end; // SHGetDesktopFolder

end;

function TDragDropInfo.CreatePreferredDropEffect(WinDropEffect: DWORD) : HGlobal;
begin
  Result := MakeHGlobal(@WinDropEffect, SizeOf(WinDropEffect));
end;

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

function TDragDropInfo.CreateHDrop(bUnicode: Boolean): HGlobal;

var

  RequiredSize: Integer;

  I: Integer;

  hGlobalDropInfo: HGlobal;

  DropFiles: PDropFiles;

  FileList: AnsiString;

  wsFileList: WideString;

begin

  {

    Построим структуру TDropFiles в памяти,

    выделенной через

    GlobalAlloc. Область памяти сделаем глобальной

    и совместной,

    поскольку она, вероятно, будет передаваться

    другому процессу.

  }

  {
    Bring the filenames in a form,
    separated by #0 and ending with a double #0#0
  }

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
      FileList := FileList + Utf8ToAnsi(Self.Files[I]) + #0;

    FileList := FileList + #0;

    { Определяем необходимый размер структуры }

    RequiredSize := SizeOf(TDropFiles) + Length(FileList) * SizeOf(AnsiChar);

  end;


  hGlobalDropInfo := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, RequiredSize);

  if (hGlobalDropInfo <> 0) then

  begin
    { Заблокируем область памяти, чтобы к ней

      можно было обратиться

    }

    DropFiles := GlobalLock(hGlobalDropInfo);


    { Заполним поля структуры DropFiles }

    {

      pFiles -- смещение от начала

      структуры до первого байта массива

      с именами файлов.

    }

    DropFiles.pFiles := SizeOf(TDropFiles);

    DropFiles.pt := Self.FDropPoint;

    DropFiles.fNC := Self.InClientArea;

    DropFiles.fWide := bUnicode;

    {

      Копируем имена файлов в буфер.

      Буфер начинается со смещения

      DropFiles + DropFiles.pFiles,

      то есть после последнего поля структуры.

    }

    { The pointer should be aligned nicely,
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


{ TFileDropTarget }

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
  if ActiveX.RegisterDragDrop(DragDropTarget.GetControl.Handle, Self) = S_OK then

    FHandle := DragDropTarget.GetControl.Handle

  else

    FHandle := 0;

end;


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

function TFileDropTarget.DragOver

  (grfKeyState: LongWord; pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

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

{

  Обработка сброшенных данных.

}

function TFileDropTarget.Drop(const dataObj: IDataObject; grfKeyState: LongWord;
  pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

var

  Medium: TSTGMedium;

  Format: TFormatETC;

  i: Integer;

  DropInfo: TDragDropInfo;

  FileNames: TStringList;

  InClient: boolean;

  DropPoint: TPoint;

  DropEffect: TDropEffect;

begin

  dataObj._AddRef;

  {

    Получаем данные.  Структура TFormatETC

    сообщает

    dataObj.GetData, как получить данные

    и в каком формате

    они должны храниться (эта информация

    содержится в

    структуре TSTGMedium).

  }

  Format.cfFormat := CF_HDROP;

  Format.ptd := nil;

  Format.dwAspect := DVASPECT_CONTENT;

  Format.lindex := -1;

  Format.tymed := TYMED_HGLOBAL;



  { Заносим данные в структуру Medium }

  Result := dataObj.GetData(Format, Medium);



  {

    Если все прошло успешно, далее

    действуем, как при операции файлового

    перетаскивания FMDD.

  }

  if (Result = S_OK) then

  begin

    case Medium.Tymed of

    TYMED_HGLOBAL:

      begin

        InClient := DragQueryPoint(Medium.hGlobal, @DropPoint);

        if (DropPoint.X = 0) and (DropPoint.Y = 0) then
          DropPoint := pt;

        { Создаем объект TDragDropInfo }

        DropInfo := TDragDropInfo.Create(DropPoint, InClient, dwEffect);


        { Retrieve file names }

        FileNames := GetDropFilenames(Medium.hGlobal);

        if Assigned(FileNames) then

          for i := 0 to FileNames.Count - 1 do

          begin

            DropInfo.Add(FileNames[i]);

          end;


        { Если указан обработчик, вызываем его }

        if (Assigned(FDragDropTarget.GetDropEvent)) then

        begin

          // Set default effect by examining keyboard keys, taking into
          // consideration effects allowed by the source (dwEffect parameter).
          dwEffect := dwEffect and GetEffectByKeyState(grfKeyState);

          DropEffect := WinEffectToDropEffect(dwEffect);

          if FDragDropTarget.GetDropEvent()(DropInfo.Files, DropEffect, DropInfo.DropPoint) = False then

            ;

          dwEffect := DropEffectToWinEffect(DropEffect);

        end;

        DropInfo.Free;

      end; // TYMED_HGLOBAL

    end; // case


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

end;

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
               Result.Add(UTF8Encode(FileName))
             else
               raise Exception.Create(rsMsgInvalidFilename + ': ' + LineEnding +
                                      UTF8Encode(FileName));
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

{ TFileDropSource }

constructor TFileDropSource.Create;

begin

  inherited Create;

  _AddRef;

end;


{

QueryContinueDrag определяет необходимые действия.

}

function TFileDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: longint): HResult;

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

function TFileDropSource.GiveFeedback(dwEffect: longint): HResult;

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


{ THDropDataObject }

constructor THDropDataObject.Create(ADropPoint: TPoint; AInClient: boolean;
                                    PreferredWinDropEffect: DWORD);

begin

  inherited Create;

  _AddRef;

  FDropInfo := TDragDropInfo.Create(ADropPoint, AInClient, PreferredWinDropEffect);

end;

destructor THDropDataObject.Destroy;

begin

  if (FDropInfo <> nil) then

    FDropInfo.Free;

  inherited Destroy;

end;

procedure THDropDataObject.Add(const s: string);

begin

  FDropInfo.Add(s);

end;

function THDropDataObject.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;

begin

  Result := DV_E_FORMATETC;

  { Необходимо обнулить все поля medium

  на случай ошибки}

  medium.tymed := 0;

  medium.hGlobal := 0;

  medium.PUnkForRelease := nil;



  { Если формат поддерживается, создаем

  и возвращаем данные }

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

function THDropDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;

begin

  Result := DV_E_FORMATETC;  { К сожалению,

  не поддерживается }

end;

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

function THDropDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;

begin

  formatetcOut.ptd := nil;

  Result := E_NOTIMPL;

end;

function THDropDataObject.SetData(const formatetc: TFormatEtc;
  const medium: TStgMedium; fRelease: BOOL): HResult;

begin

  Result := E_NOTIMPL;

end;


{ EnumFormatEtc возвращает список поддерживаемых форматов }

function THDropDataObject.EnumFormatEtc(dwDirection: LongWord;
  out enumFormatEtc: IEnumFormatEtc): HResult;

begin

  { Поддерживается только Get. Задать

  содержимое данных нельзя }

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

{ Функции Advise не поддерживаются }

function THDropDataObject.DAdvise(const formatetc: TFormatEtc;
  advf: LongWord; const advSink: IAdviseSink; out dwConnection: LongWord): HResult;

begin

  Result := OLE_E_ADVISENOTSUPPORTED;

end;

function THDropDataObject.DUnadvise(dwConnection: LongWord): HResult;

begin

  Result := OLE_E_ADVISENOTSUPPORTED;

end;

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
    DropData:= THDropDataObject.Create(ScreenStartPoint, False,
                                       DROPEFFECT_COPY { default effect } );

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

      E_OUTOFMEMORY:
        begin
          MessageBox(0, 'Out of memory', 'Error!', 16);
          FLastStatus := DragDropError;
          Result := False;
        end;

      else
        begin
          MessageBox(0, 'Something bad happened', 'Error!', 16);
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

