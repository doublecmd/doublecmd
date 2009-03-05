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
  { TFormatList -- массив записей TFormatEtc }

  PFormatList = ^TFormatList;

  TFormatList = array[0..1] of TFormatEtc;

  { IEnumFormatEtc }

  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)

  private

    FFormatList: PFormatList;

    FFormatCount: Integer;

    FIndex: Integer;

  public

    constructor Create(FormatList: PFormatList; FormatCount, Index: Integer);

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

  public

    constructor Create(ADropPoint: TPoint; AInClient: boolean);

    destructor Destroy; override;

    procedure Add(const s: string);

    function CreateHDrop: HGlobal;

    property InClientArea: boolean Read FInClientArea;

    property DropPoint: TPoint Read FDropPoint;

    property Files: TStringList Read FFileList;

  end;


  TDragDropTargetWindows = class; // forward declaration

  { TFileDropTarget знает, как принимать сброшенные файлы }

  TFileDropTarget = class(TInterfacedObject, IDropTarget)

  private

    FHandle: HWND;

    FDragDropTarget: TDragDropTargetWindows;

  public

    constructor Create(DragDropTarget: TDragDropTargetWindows);

    destructor Destroy; override;


    { из IDropTarget }

    function DragEnter(const dataObj: IDataObject; grfKeyState: LongWord;
      pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

    function DragOver(grfKeyState: LongWord; pt: TPoint;
      var dwEffect: LongWord): HResult; stdcall;

    function DragLeave: HResult; stdcall;

    function Drop(const dataObj: IDataObject; grfKeyState: LongWord;
      pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

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

    constructor Create(ADropPoint: TPoint; AInClient: boolean);

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
  SysUtils, ShellAPI, ShlObj, LCLIntf;

{ TEnumFormatEtc }

constructor TEnumFormatEtc.Create(FormatList: PFormatList; FormatCount, Index: Integer);

begin

  inherited Create;

  FFormatList := FormatList;

  FFormatCount := FormatCount;

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

  eltout: TFormatList absolute elt;

begin

  i := 0;



  while (i < celt) and (FIndex < FFormatCount) do

  begin

    eltout[i] := FFormatList[FIndex];

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

  if (celt <= FFormatCount - FIndex) then

  begin

    FIndex := FIndex + celt;

    Result := S_OK;

  end
  else

  begin

    FIndex := FFormatCount;

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

  enum := TEnumFormatEtc.Create(FFormatList, FFormatCount, FIndex);

  Result := S_OK;

end;


{ TDragDropInfo }

constructor TDragDropInfo.Create(ADropPoint: TPoint; AInClient: boolean);

begin

  inherited Create;

  FFileList := TStringList.Create;

  FDropPoint := ADropPoint;

  FInClientArea := AInClient;

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

function TDragDropInfo.CreateHDrop: HGlobal;

var

  RequiredSize: Integer;

  I: Integer;

  hGlobalDropInfo: HGlobal;

  DropFiles: PDropFiles;

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

  for I := 0 to Self.Files.Count - 1 do

    begin

      wsFileList:= wsFileList + UTF8Decode(Self.Files[I]) + #0;

    end;

  wsFileList:= wsFileList + #0;

  { Определяем необходимый размер структуры }

  RequiredSize := SizeOf(TDropFiles) + Length(wsFileList) * 2;



  hGlobalDropInfo := GlobalAlloc((GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT),
    RequiredSize);

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

    DropFiles.fWide := True;



    {

      Копируем имена файлов в буфер.

      Буфер начинается со смещения

      DropFiles + DropFiles.pFiles,

      то есть после последнего поля структуры.

    }

    DropFiles := Pointer(DropFiles) + DropFiles.pFiles;

    CopyMemory(DropFiles, PWideChar(wsFileList), Length(wsFileList) * 2);



    { Снимаем блокировку }

    GlobalUnlock(hGlobalDropInfo);

  end;



  Result := hGlobalDropInfo;

end;


{ TFileDropTarget }

constructor TFileDropTarget.Create(DragDropTarget: TDragDropTargetWindows);
begin

  inherited Create;

  _AddRef;

  FDragDropTarget := DragDropTarget;

  ActiveX.CoLockObjectExternal(Self,

    True, False);

  ActiveX.RegisterDragDrop(DragDropTarget.GetControl.Handle, Self);

end;

{ Destroy снимает блокировку с объекта

и разрывает связь с ним }

destructor TFileDropTarget.Destroy;

var

  WorkHandle: HWND;

begin

  {

    Если значение FHandle не равно 0,

    значит, связь с окном все

    еще существует. Обратите внимание

    на то, что FHandle необходимо

    прежде всего присвоить 0, потому

    что CoLockObjectExternal и

    RevokeDragDrop вызывают Release,

    что, в свою очередь, может

    привести к вызову Free и зацикливанию

    программы.

    Подозреваю, что этот фрагмент не

    совсем надежен. Если объект будет

    освобожден до того, как

    счетчик ссылок упадет до 0,

    может возникнуть исключение.

  }

  if (FHandle <> 0) then

  begin

    WorkHandle := FHandle;

    FHandle := 0;

    ActiveX.CoLockObjectExternal

    (Self, False, True);

    ActiveX.RevokeDragDrop(WorkHandle);

  end;



  inherited Destroy;

end;

function TFileDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: LongWord; pt: TPoint; var dwEffect: LongWord): HResult; stdcall;

var
  DropEffect: TDropEffect;

begin
  dwEffect := GetEffectByKeyState(grfKeyState);

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
  dwEffect := GetEffectByKeyState(grfKeyState);

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

  NumFiles: Integer;

  i: Integer;

  DropInfo: TDragDropInfo;

  szFilename: array [0..MAX_PATH] of char;

  InClient: boolean;

  DropPoint: TPoint;

  bWideStrings: boolean;

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

    { Получаем количество файлов и

    прочие сведения }

    NumFiles := DragQueryFile(Medium.hGlobal, $FFFFFFFF, nil, 0);

    InClient := DragQueryPoint(Medium.hGlobal, @DropPoint);

    if (DropPoint.X = 0) and (DropPoint.Y = 0) then
      DropPoint := pt;

    bWideStrings := DragQueryWide( Medium.hGlobal );


    { Создаем объект TDragDropInfo }

    DropInfo := TDragDropInfo.Create(DropPoint, InClient);



    { Заносим все файлы в список }

    for i := 0 to NumFiles - 1 do

    begin

      DragQueryFile(Medium.hGlobal, i,

        szFilename,

        sizeof(szFilename));

      // If Wide strings, then do Wide to UTF-8 transform
      if( bWideStrings ) then
        DropInfo.Add( UTF8Encode( szFileName ) )
      else
        DropInfo.Add(szFilename);

    end;

    { Если указан обработчик, вызываем его }

    if (Assigned(FDragDropTarget.GetDropEvent)) then

    begin

      // Set default effect by examining keyboard keys.
      dwEffect := GetEffectByKeyState(grfKeyState);

      DropEffect := WinEffectToDropEffect(dwEffect);

      if FDragDropTarget.GetDropEvent()(DropInfo.Files, DropEffect, DropInfo.DropPoint) = False then

        ;

      dwEffect := DropEffectToWinEffect(DropEffect);

    end;



    DropInfo.Free;


    { Release memory allocated on DoDragDrop }
    DragFinish( Medium.hGlobal );

    if (Medium.PUnkForRelease = nil) then

      ReleaseStgMedium(@Medium);

  end;


  dataObj._Release;

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

  case dwEffect of

    DROPEFFECT_NONE,

    DROPEFFECT_COPY,

    DROPEFFECT_MOVE,

    DROPEFFECT_LINK,

    DROPEFFECT_SCROLL: Result :=

        DRAGDROP_S_USEDEFAULTCURSORS;

    else

      Result := S_OK;

  end;

end;


{ THDropDataObject }

constructor THDropDataObject.Create(ADropPoint: TPoint; AInClient: boolean);

begin

  inherited Create;

  _AddRef;

  FDropInfo := TDragDropInfo.Create(ADropPoint, AInClient);

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

      medium.tymed := TYMED_HGLOBAL;

      { За освобождение отвечает

      вызывающая сторона! }

      medium.hGlobal := FDropInfo.CreateHDrop;

      Result := S_OK;

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

begin

  Result := DV_E_FORMATETC;

  with formatetc do

    if dwAspect = DVASPECT_CONTENT then

      if (cfFormat = CF_HDROP) and (tymed = TYMED_HGLOBAL) then

        Result := S_OK;

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

const

  DataFormats: array [0..0] of TFormatEtc =

    (

    (

    cfFormat: CF_HDROP;

    ptd: nil;

    dwAspect: DVASPECT_CONTENT;

    lindex: -1;

    tymed: TYMED_HGLOBAL;

    )

    );

  DataFormatCount = 1;

begin

  { Поддерживается только Get. Задать

  содержимое данных нельзя }

  if dwDirection = DATADIR_GET then

  begin

    enumFormatEtc := TEnumFormatEtc.Create(@DataFormats, DataFormatCount, 0);

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
    DropData:= THDropDataObject.Create(ScreenStartPoint, True);

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

    { РћСЃРІРѕР±РѕР¶РґР°РµРј РёСЃРїРѕР»СЊР·РѕРІР°РЅРЅС‹Рµ СЂРµСЃСѓСЂСЃС‹

    РїРѕСЃР»Рµ Р·Р°РІРµСЂС€РµРЅРёСЏ СЂР°Р±РѕС‚С‹ }

   { DropSource.Free;

    DropData.Free; }
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
  if FDragDropTarget <> nil then
    FreeAndNil(FDragDropTarget);
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
    FDragDropTarget := uOleDragDrop.TFileDropTarget.Create(Self);
    Result := True;
  end;
end;

procedure TDragDropTargetWindows.UnregisterEvents;
begin
  inherited;
  if Assigned(FDragDropTarget) then
    FreeAndNil(FDragDropTarget); // Freeing will unregister events
end;


initialization

  OleInitialize(nil);


finalization

  OleUninitialize;

end.

