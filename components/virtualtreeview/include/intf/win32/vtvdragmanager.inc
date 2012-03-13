

//----------------------------------------------------------------------------------------------------------------------

// OLE drag and drop support classes
// This is quite heavy stuff (compared with the VCL implementation) but is much better suited to fit the needs
// of DD'ing various kinds of virtual data and works also between applications.

//----------------- TEnumFormatEtc -------------------------------------------------------------------------------------

constructor TEnumFormatEtc.Create(Tree: TBaseVirtualTree; AFormatEtcArray: TFormatEtcArray);

var
  I: Integer;

begin
  inherited Create;

  FTree := Tree;
  // Make a local copy of the format data.
  SetLength(FFormatEtcArray, Length(AFormatEtcArray));
  for I := 0 to High(AFormatEtcArray) do
    FFormatEtcArray[I] := AFormatEtcArray[I];
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;

var
  AClone: TEnumFormatEtc;

begin
  Result := S_OK;
  try
    AClone := TEnumFormatEtc.Create(nil, FFormatEtcArray);
    AClone.FCurrentIndex := FCurrentIndex;
    Enum := AClone as IEnumFormatEtc;
  except
    Result := E_FAIL;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Next(celt: LongWord; out elt: FormatEtc; pceltFetched:pULong=nil): HResult;
var
  CopyCount: LongWord;

begin
  Result := S_FALSE;
  CopyCount := Length(FFormatEtcArray) - FCurrentIndex;
  if celt < CopyCount then
    CopyCount := celt;
  if CopyCount > 0 then
  begin
    Move(FFormatEtcArray[FCurrentIndex], elt, CopyCount * SizeOf(TFormatEtc));
    Inc(FCurrentIndex, CopyCount);
    Result := S_OK;
  end;
  if pceltFetched <> nil then
    pceltFetched^ := CopyCount;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Reset: HResult;

begin
  FCurrentIndex := 0;
  Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Skip(celt: LongWord): HResult;

begin
  if FCurrentIndex + celt < High(FFormatEtcArray) then
  begin
    Inc(FCurrentIndex, celt);
    Result := S_Ok;
  end
  else
    Result := S_FALSE;
end;



//----------------- TVTDataObject --------------------------------------------------------------------------------------

constructor TVTDataObject.Create(AOwner: TBaseVirtualTree; ForClipboard: Boolean);

begin
  inherited Create;

  FOwner := AOwner;
  FForClipboard := ForClipboard;
  FOwner.GetNativeClipboardFormats(FFormatEtcArray);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDataObject.Destroy;

var
  I: Integer;
  StgMedium: PStgMedium;

begin
  // Cancel a pending clipboard operation if this data object was created for the clipboard and
  // is freed because something else is placed there.
  if FForClipboard and not (tsClipboardFlushing in FOwner.FStates) then
    FOwner.CancelCutOrCopy;

  // Release any internal clipboard formats
  for I := 0 to High(FormatEtcArray) do
  begin
    StgMedium := FindInternalStgMedium(FormatEtcArray[I].cfFormat);
    if Assigned(StgMedium) then
      ReleaseStgMedium(StgMedium);
  end;

  FormatEtcArray := nil;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.CanonicalIUnknown(TestUnknown: IUnknown): IUnknown;

// Uses COM object identity: An explicit call to the IUnknown::QueryInterface method, requesting the IUnknown
// interface, will always return the same pointer.

begin
  if Assigned(TestUnknown) then
  begin
    if TestUnknown.QueryInterface(IUnknown, Result) = 0 then
      Result._Release // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end
  else
    Result := TestUnknown
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;

begin
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and (FormatEtc1.ptd = FormatEtc2.ptd) and
    (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and (FormatEtc1.lindex = FormatEtc2.lindex) and
    (FormatEtc1.tymed and FormatEtc2.tymed <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.FindFormatEtc(TestFormatEtc: TFormatEtc; const FormatEtcArray: TFormatEtcArray): integer;

var
  I: integer;

begin
  Result := -1;
  for I := 0 to High(FormatEtcArray) do
  begin
    if EqualFormatEtc(TestFormatEtc, FormatEtcArray[I]) then
    begin
      Result := I;
      Break;
    end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.FindInternalStgMedium(Format: TClipFormat): PStgMedium;

var
  I: integer;
begin
  Result := nil;
  for I := 0 to High(InternalStgMediumArray) do
  begin
    if Format = InternalStgMediumArray[I].Format then
    begin
      Result := @InternalStgMediumArray[I].Medium;
      Break;
    end
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.HGlobalClone(HGlobal: THandle): THandle;

// Returns a global memory block that is a copy of the passed memory block.

var
  Size: Cardinal;
  Data,
  NewData: PByte;

begin
  Size := GlobalSize(HGlobal);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(hGlobal);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data^, NewData^, Size);
    finally
      GlobalUnLock(Result);
    end
  finally
    GlobalUnLock(hGlobal);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.RenderInternalOLEData(const FormatEtcIn: TFormatEtc; var Medium: TStgMedium;
  var OLEResult: HResult): Boolean;

// Tries to render one of the formats which have been stored via the SetData method.
// Since this data is already there it is just copied or its reference count is increased (depending on storage medium).

var
  InternalMedium: PStgMedium;

begin
  Result := True;
  InternalMedium := FindInternalStgMedium(FormatEtcIn.cfFormat);
  if Assigned(InternalMedium) then
    OLEResult := StgMediumIncRef(InternalMedium^, Medium, False, Self as IDataObject)
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
  CopyInMedium: Boolean; DataObject: IDataObject): HRESULT;

// InStgMedium is the data that is requested, OutStgMedium is the data that we are to return either a copy of or
// increase the IDataObject's reference and send ourselves back as the data (unkForRelease). The InStgMedium is usually
// the result of a call to find a particular FormatEtc that has been stored locally through a call to SetData.
// If CopyInMedium is not true we already have a local copy of the data when the SetData function was called (during
// that call the CopyInMedium must be true). Then as the caller asks for the data through GetData we do not have to make
// copy of the data for the caller only to have them destroy it then need us to copy it again if necessary.
// This way we increase the reference count to ourselves and pass the STGMEDIUM structure initially stored in SetData.
// This way when the caller frees the structure it sees the unkForRelease is not nil and calls Release on the object
// instead of destroying the actual data.

var
  Len: Integer;

begin
  Result := S_OK;

  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  // The data handled here always results from a call of SetData we got. This ensures only one storage format
  // is indicated and hence the case statement below is safe (IDataObject.GetData can optionally use several
  // storage formats).
  case InStgMedium.tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.hGlobal := HGlobalClone(InStgMedium.hGlobal);
          if OutStgMedium.hGlobal = 0 then
            Result := E_OUTOFMEMORY
        end
        else
          // Don't generate a copy just use ourselves and the copy previously saved.
          OutStgMedium.PunkForRelease := Pointer(DataObject); // Does not increase RefCount.
      end;
    TYMED_FILE:
      begin
        //todo_lcl_check
        Len := Length(WideString(InStgMedium.lpszFileName)) + 1; // Don't forget the terminating null character.
        OutStgMedium.lpszFileName := CoTaskMemAlloc(2 * Len);
        Move(InStgMedium.lpszFileName^, OutStgMedium.lpszFileName^, 2 * Len);
      end;
    TYMED_ISTREAM:
      IUnknown(OutStgMedium.Pstm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown(OutStgMedium.Pstg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.PunkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy GDI objects right now.
    TYMED_MFPICT:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.PunkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy MetaFile objects right now.
    TYMED_ENHMF:
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.PunkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED; // Don't know how to copy enhanced metafiles objects right now.
  else
    Result := DV_E_TYMED;
  end;

  if (Result = S_OK) and Assigned(OutStgMedium.PunkForRelease) then
    IUnknown(OutStgMedium.PunkForRelease)._AddRef;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.DAdvise(const FormatEtc: TFormatEtc; advf: DWord; const advSink: IAdviseSink;
  out dwConnection: DWord): HResult;

// Advise sink management is greatly simplified by the IDataAdviseHolder interface.
// We use this interface and forward all concerning calls to it.

begin
  Result := S_OK;
  if FAdviseHolder = nil then
    Result := CreateDataAdviseHolder(FAdviseHolder);
  if Result = S_OK then
    Result := FAdviseHolder.Advise(Self as IDataObject, FormatEtc, advf, advSink, dwConnection);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.DUnadvise(dwConnection: DWord): HResult;

begin
  if FAdviseHolder = nil then
    Result := E_NOTIMPL
  else
    Result := FAdviseHolder.Unadvise(dwConnection);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EnumDAdvise(Out enumAdvise : IEnumStatData):HResult;

begin
  if FAdviseHolder = nil then
    Result := OLE_E_ADVISENOTSUPPORTED
  else
    Result := FAdviseHolder.EnumAdvise(enumAdvise);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EnumFormatEtc(Direction: DWord; out EnumFormatEtc: IEnumFormatEtc): HResult;

var
  NewList: TEnumFormatEtc;

begin
  Result := E_FAIL;
  if Direction = DATADIR_GET then
  begin
    NewList := TEnumFormatEtc.Create(FOwner, FormatEtcArray);
    EnumFormatEtc := NewList as IEnumFormatEtc;
    Result := S_OK;
  end
  else
    EnumFormatEtc := nil;
  if EnumFormatEtc = nil then
    Result := OLE_S_USEREG;
end;

//----------------------------------------------------------------------------------------------------------------------

Function TVTDataObject.GetCanonicalFormatEtc(const pformatetcIn : FORMATETC;Out pformatetcOut : FORMATETC):HResult;

begin
  Result := DATA_S_SAMEFORMATETC;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HResult;

// Data is requested by clipboard or drop target. This method dispatchs the call
// depending on the data being requested.

var
  I: Integer;
  Data: PVTReference;

begin
  // The tree reference format is always supported and returned from here.
  if FormatEtcIn.cfFormat = CF_VTREFERENCE then
  begin
    // Note: this format is not used while flushing the clipboard to avoid a dangling reference
    //       when the owner tree is destroyed before the clipboard data is replaced with something else.
    if tsClipboardFlushing in FOwner.FStates then
      Result := E_FAIL
    else
    begin
      Medium.hGlobal := GlobalAlloc(GHND or GMEM_SHARE, SizeOf(TVTReference));
      Data := GlobalLock(Medium.hGlobal);
      Data.Process := GetCurrentProcessID;
      Data.Tree := FOwner;
      GlobalUnlock(Medium.hGlobal);
      Medium.tymed := TYMED_HGLOBAL;
      Medium.PunkForRelease := nil;
      Result := S_OK;
    end;
  end
  else
  begin
    try
      // See if we accept this type and if not get the correct return value.
      Result := QueryGetData(FormatEtcIn);
      if Result = S_OK then
      begin
        for I := 0 to High(FormatEtcArray) do
        begin
          if EqualFormatEtc(FormatEtcIn, FormatEtcArray[I]) then
          begin
            if not RenderInternalOLEData(FormatEtcIn, Medium, Result) then
              Result := FOwner.RenderOLEData(FormatEtcIn, Medium, FForClipboard);
            Break;
          end;
        end
      end
    except
      FillChar(Medium, SizeOf(Medium), #0);
      Result := E_FAIL;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HResult;

begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.QueryGetData(const FormatEtc: TFormatEtc): HResult;

var
  I: Integer;

begin
  Result := DV_E_CLIPFORMAT;
  for I := 0 to High(FFormatEtcArray) do
  begin
    if FormatEtc.cfFormat = FFormatEtcArray[I].cfFormat then
    begin
      if (FormatEtc.tymed and FFormatEtcArray[I].tymed) <> 0 then
      begin
        if FormatEtc.dwAspect = FFormatEtcArray[I].dwAspect then
        begin
          if FormatEtc.lindex = FFormatEtcArray[I].lindex then
          begin
            Result := S_OK;
            Break;
          end
          else
            Result := DV_E_LINDEX;
        end
        else
          Result := DV_E_DVASPECT;
      end
      else
        Result := DV_E_TYMED;
    end;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.SetData(const FormatEtc: TFormatEtc; {$ifdef VER2_0}var{$else}const{$endif} Medium: TStgMedium; DoRelease: BOOL): HResult;

// Allows dynamic adding to the IDataObject during its existance. Most noteably it is used to implement
// IDropSourceHelper and allows to set a special format for optimized moves during a shell transfer.

var
  Index: Integer;
  LocalStgMedium: PStgMedium;

begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc, FormatEtcArray);
  if Index > - 1 then
  begin
    // Just use the TFormatEct in the array after releasing the data.
    LocalStgMedium := FindInternalStgMedium(FormatEtcArray[Index].cfFormat);
    if Assigned(LocalStgMedium) then
    begin
      ReleaseStgMedium(LocalStgMedium);
      FillChar(LocalStgMedium^, SizeOf(LocalStgMedium^), #0);
    end;
  end
  else
  begin
    // It is a new format so create a new TFormatCollectionItem, copy the
    // FormatEtc parameter into the new object and and put it in the list.
    SetLength(FFormatEtcArray, Length(FormatEtcArray) + 1);
    FormatEtcArray[High(FormatEtcArray)] := FormatEtc;

    // Create a new InternalStgMedium and initialize it and associate it with the format.
    SetLength(FInternalStgMediumArray, Length(InternalStgMediumArray) + 1);
    InternalStgMediumArray[High(InternalStgMediumArray)].Format := FormatEtc.cfFormat;
    LocalStgMedium := @InternalStgMediumArray[High(InternalStgMediumArray)].Medium;
    FillChar(LocalStgMedium^, SizeOf(LocalStgMedium^), #0);
  end;

  if DoRelease then
  begin
    // We are simply being given the data and we take control of it.
    LocalStgMedium^ := Medium;
    Result := S_OK
  end
  else
  begin
    // We need to reference count or copy the data and keep our own references to it.
    Result := StgMediumIncRef(Medium, LocalStgMedium^, True, Self as IDataObject);

    // Can get a circular reference if the client calls GetData then calls SetData with the same StgMedium.
    // Because the unkForRelease for the IDataObject can be marshalled it is necessary to get pointers that
    // can be correctly compared. See the IDragSourceHelper article by Raymond Chen at MSDN.
    if Assigned(LocalStgMedium.PunkForRelease) then
    begin
      if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(LocalStgMedium.PunkForRelease)) then
        IUnknown(LocalStgMedium.PunkForRelease) := nil; // release the interface
    end;
  end;

  // Tell all registered advice sinks about the data change.
  if Assigned(FAdviseHolder) then
    FAdviseHolder.SendOnDataChange(Self as IDataObject, 0, 0);
end;

//----------------- TVTDragManager -------------------------------------------------------------------------------------

constructor TVTDragManager.Create(AOwner: TBaseVirtualTree);

begin
  inherited Create;
  FOwner := AOwner;

  // Create an instance  of the drop target helper interface. This will fail but not harm on systems which do
  // not support this interface (everything below Windows 2000);
  CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IID_IDropTargetHelper, FDropTargetHelper);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDragManager.Destroy;

begin
  // Set the owner's reference to us to nil otherwise it will access an invalid pointer
  // after our desctruction is complete.
  Pointer(FOwner.FDragManager) := nil;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDataObject: IDataObject;

begin
  // When the owner tree starts a drag operation then it gets a data object here to pass it to the OLE subsystem.
  // In this case there is no local reference to a data object and one is created (but not stored).
  // If there is a local reference then the owner tree is currently the drop target and the stored interface is
  // that of the drag initiator.
  if Assigned(FDataObject) then
    Result := FDataObject
  else
  begin
    Result := FOwner.DoCreateDataObject;
    if Result = nil then
      Result := TVTDataObject.Create(FOwner, False) as IDataObject;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDragSource: TBaseVirtualTree;

begin
  Result := FDragSource;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDropTargetHelperSupported: Boolean;

begin
  Result := Assigned(FDropTargetHelper);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetIsDropTarget: Boolean;

begin
  Result := FIsDropTarget;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragEnter(const DataObject: IDataObject; KeyState: LongWord; Pt: TPoint;
  var Effect: LongWord): HResult;

begin
  FDataObject := DataObject;
  FIsDropTarget := True;

  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, @FFullDragging, 0);
  // If full dragging of window contents is disabled in the system then our tree windows will be locked
  // and cannot be updated during a drag operation. With the following call painting is again enabled.
  if not FFullDragging then
    LockWindowUpdate(0);
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragEnter(FOwner.Handle, DataObject, Pt, Effect);

  FDragSource := FOwner.GetTreeFromDataObject(DataObject);
  Result := FOwner.DragEnter(KeyState, Pt, Effect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragLeave: HResult;

begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragLeave;

  FOwner.DragLeave;
  FIsDropTarget := False;
  FDragSource := nil;
  FDataObject := nil;
  Result := NOERROR;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragOver(KeyState: LongWord; Pt: TPoint; var Effect: LongWord): HResult;

begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragOver(Pt, Effect);

  Result := FOwner.DragOver(FDragSource, KeyState, dsDragMove, Pt, Effect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.Drop(const DataObject: IDataObject; KeyState: LongWord; Pt: TPoint;
  var Effect: LongWord): HResult;

begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.Drop(DataObject, Pt, Effect);

  Result := FOwner.DragDrop(DataObject, KeyState, Pt, Effect);
  FIsDropTarget := False;
  FDataObject := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTDragManager.ForceDragLeave;

// Some drop targets, e.g. Internet Explorer leave a drag image on screen instead removing it when they receive
// a drop action. This method calls the drop target helper's DragLeave method to ensure it removes the drag image from
// screen. Unfortunately, sometimes not even this does help (e.g. when dragging text from VT to a text field in IE).

begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragLeave;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GiveFeedback(Effect: Integer): HResult;

begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.QueryContinueDrag(EscapePressed: BOOL; KeyState: Integer): HResult;

var
  RButton,
  LButton: Boolean;

begin
  LButton := (KeyState and MK_LBUTTON) <> 0;
  RButton := (KeyState and MK_RBUTTON) <> 0;

  // Drag'n drop canceled by pressing both mouse buttons or Esc?
  if (LButton and RButton) or EscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
    // Drag'n drop finished?
    if not (LButton or RButton) then
      Result := DRAGDROP_S_DROP
    else
      Result := S_OK;
end;


