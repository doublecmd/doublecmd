var
  viewerImageItemsIdentifiers: TStringArray = (
    'ViewerForm.ZoomIn',
    'ViewerForm.ZoomOut',
    'NSToolbarFlexibleSpaceItem',
    'ViewerForm.Rotate270',
    'ViewerForm.Rotate90',
    'ViewerForm.Mirror',
    'ViewerForm.Resize',
    'NSToolbarFlexibleSpaceItem',

    'ViewerForm.GifMove',
    'ViewerForm.GifPrev',
    'ViewerForm.GifNext',
    'ViewerForm.GifExport',
    'NSToolbarFlexibleSpaceItem',

    'ViewerForm.HighLight',
    'ViewerForm.Crop',
    'ViewerForm.RedEyes',
    'NSToolbarFlexibleSpaceItem',

    'ViewerForm.Paint',
    'ViewerForm.PenMode',
    'ViewerForm.PenWidth',
    'ViewerForm.PenColor'
  );

type

  { TCocoaViewerFormHandler }

  TCocoaViewerFormHandler = class( TViewerFormHandler )
    procedure onShowModeChanged(
      const viewer: TfrmViewer;
      const mode: TViewerShowMode ); override;
    procedure onGifStateChanged(
      const viewer: TfrmViewer;
      const states: TViewerGifStates ); override;
    procedure onImageEditStateChanged(
      const viewer: TfrmViewer ); override;
    procedure onSlideStateChanged(
      const viewer: TfrmViewer ); override;

    procedure onPenWidthChanged( Sender: TObject );
    procedure onSlideStart( Sender: TObject );
  end;

function getCurrentViewerForm: TfrmViewer;
var
  form: TCustomForm;
begin
  Result:= nil;
  form:= Screen.ActiveCustomForm;
  if form is TfrmViewer then
    Result:= TfrmViewer(form);
end;

function isImageToolBarAttached( const form: TCustomForm ): Boolean;
var
  toolBar: NSToolBar;
  index: Integer;
begin
  Result:= False;

  toolBar:= TCocoaToolBarUtils.getToolBar( form );
  if toolBar = nil then
    Exit;

  index:= TCocoaToolBarUtils.findItemIndexByIdentifier( toolBar, viewerImageItemsIdentifiers[0] );
  Result:= (index>=0);
end;

procedure viewerAttachImageToolBar( const form: TCustomForm );
var
  toolBar: NSToolBar;
  i: Integer;
begin
  toolBar:= TCocoaToolBarUtils.getToolBar( form );
  if toolBar = nil then
    Exit;

  if isImageToolBarAttached(form) then
    Exit;

  for i:= 0 to Length(viewerImageItemsIdentifiers)-1 do
    TCocoaToolBarUtils.addItemWithIdentifier( toolBar, viewerImageItemsIdentifiers[i] );
end;

procedure viewerDeattachImageToolBar( const form: TCustomForm );
var
  toolBar: NSToolBar;
  index: Integer;
  i: Integer;
begin
  toolBar:= TCocoaToolBarUtils.getToolBar( form );
  if toolBar = nil then
    Exit;

  index:= TCocoaToolBarUtils.findItemIndexByIdentifier( toolBar, viewerImageItemsIdentifiers[0] );
  if index < 0 then
    Exit;

  for i:= toolBar.items.count-1 downto index do
    TCocoaToolBarUtils.removeItemByIndex( toolBar, i );
end;

procedure viewerReloadAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_Reload( [] );
end;

procedure viewerPrevAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_LoadPrevFile( [] );
end;

procedure viewerNextAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_LoadNextFile( [] );
end;

procedure viewerCopyAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_CopyFile( [] );
end;

procedure viewerMoveAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_MoveFile( [] );
end;

procedure viewerDeleteAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_DeleteFile( [] );
end;

procedure viewerZoomInAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_ZoomIn( [] );
end;

procedure viewerZoomOutAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_ZoomOut( [] );
end;

procedure viewerRotate270Action( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_Rotate270( [] );
end;

procedure viewerRotate90Action( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_Rotate90( [] );
end;

procedure viewerMirrorAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.cm_MirrorHorz( [] );
end;

procedure viewerResizeAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnResizeClick( nil );
end;

procedure viewerGifMoveAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnGifMoveClick( nil );
end;

procedure viewerGifPrevAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnPrevGifFrameClick( nil );
end;

procedure viewerGifNextAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnNextGifFrameClick( nil );
end;

procedure viewerGifExportAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnGifToBmpClick( nil );
end;

procedure viewerHighlightAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then begin
    form.btnHightlight.Down:= NOT form.btnHightlight.Down;
    form.btnPaintHightlight( form.btnHightlight );
  end;
end;

procedure viewerCropAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnCutTuImageClick( nil );
end;

procedure viewerRedEyesAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnRedEyeClick( nil );
end;

procedure viewerPaintAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then begin
    form.btnPaint.Down:= NOT form.btnPaint.Down;
    form.btnPaintHightlight( form.btnPaint );
  end;
end;

procedure viewerPenModeAction( const Sender: id );
var
  form: TfrmViewer;
  menuItem: TMenuItem = nil;
  penModeItem: NSToolBarItemGroup absolute Sender;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then begin
    case penModeItem.selectedIndex of
      0: menuItem:= form.miPen;
      1: menuItem:= form.miRect;
      2: menuItem:= form.miEllipse;
    end;

    if Assigned(menuItem) then
      form.miPenClick( menuItem );
  end;
end;

procedure viewerPenColorAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnPenColor.Click;
end;

procedure viewerSlideAction( const Sender: id );
var
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then
    form.btnSlideShowClick( nil );
end;

procedure onViewerGetPenWidthMenu( const menu: TMenu );
var
  i: Integer;
  item: TMenuItem;
begin
  for i:= 1 to 25 do begin
    item:= TMenuItem.Create( menu );
    item.Caption:= i.ToString;
    item.Tag:= i;
    item.OnClick:= @TCocoaViewerFormHandler(viewerFormHandler).onPenWidthChanged;
    menu.Items.Add( item );
  end;
end;

procedure onViewerGetSlideMenu( const menu: TMenu );
var
  i: Integer;
  item: TMenuItem;
begin
  for i:= 1 to 25 do begin
    item:= TMenuItem.Create( menu );
    item.Caption:= i.ToString;
    item.Tag:= i;
    item.OnClick:= @TCocoaViewerFormHandler(viewerFormHandler).onSlideStart;
    menu.Items.Add( item );
  end;
end;

const
  viewerReloadItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Reload';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'arrow.clockwise';
    title: 'Reload';
    tips: 'Reload';
    bordered: True;
    onAction: @viewerReloadAction;
  );

  viewerPrevItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Prev';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'chevron.left';
    title: 'Prev';
    tips: 'Prev';
    bordered: True;
    onAction: @viewerPrevAction;
  );

  viewerNextItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Next';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'chevron.right';
    title: 'Next';
    tips: 'Next';
    bordered: True;
    onAction: @viewerNextAction;
  );

  viewerCopyFileItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.CopyFile';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'doc.on.doc';
    title: 'CopyFile';
    tips: 'CopyFile';
    bordered: True;
    onAction: @viewerCopyAction;
  );

  viewerMoveFileItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.MoveFile';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'scissors';
    title: 'MoveFile';
    tips: 'MoveFile';
    bordered: True;
    onAction: @viewerMoveAction;
  );

  viewerDeleteFileItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.DeleteFile';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'trash';
    title: 'DeleteFile';
    tips: 'DeleteFile';
    bordered: True;
    onAction: @viewerDeleteAction;
  );

  viewerSlideItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'ViewerForm.Slide';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'play';
    title: 'Slide Show';
    tips: 'Slide Show';
    bordered: True;
    alternateIconName: 'stop';
    onAction: @viewerSlideAction;

    showsIndicator: True;
    menu: nil;
    onGetMenu: @onViewerGetSlideMenu;
  );


  viewerZoomInItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.ZoomIn';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'plus.magnifyingglass';
    title: 'Zoom In';
    tips: 'Zoom In';
    bordered: True;
    onAction: @viewerZoomInAction;
  );

  viewerZoomOutItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.ZoomOut';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'minus.magnifyingglass';
    title: 'Zoom Out';
    tips: 'Zoom Out';
    bordered: True;
    onAction: @viewerZoomOutAction;
  );

  viewerRotate270ItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Rotate270';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'rotate.left';
    title: 'Rotate -90 degree';
    tips: 'Rotate -90 degree';
    bordered: True;
    onAction: @viewerRotate270Action;
  );

  viewerRotate90ItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Rotate90';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'rotate.right';
    title: 'Rotate +90 degree';
    tips: 'Rotate +90 degree';
    bordered: True;
    onAction: @viewerRotate90Action;
  );

  viewerMirrorItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Mirror';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'arrow.left.and.right.righttriangle.left.righttriangle.right';
    title: 'Mirror';
    tips: 'Mirror';
    bordered: True;
    onAction: @viewerMirrorAction;
  );

  viewerResizeItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Resize';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'rectangle.on.rectangle';
    title: 'Resize';
    tips: 'Resize';
    bordered: True;
    onAction: @viewerResizeAction;
  );


  viewerGifMoveItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.GifMove';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'play.fill';
    title: '';
    tips: '';
    bordered: True;
    alternateIconName: 'pause.fill';

    onAction: @viewerGifMoveAction;
  );

  viewerGifPrevItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.GifPrev';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'arrow.left';
    title: 'GifPrev';
    tips: 'GifPrev';
    bordered: True;
    onAction: @viewerGifPrevAction;
  );

  viewerGifNextItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.GifNext';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'arrow.right';
    title: 'GifNext';
    tips: 'GifNext';
    bordered: True;
    onAction: @viewerGifNextAction;
  );

  viewerGifExportItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.GifExport';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'arrow.down.circle.fill';
    title: 'GifExport';
    tips: 'GifExport';
    bordered: True;
    onAction: @viewerGifExportAction;
  );

  viewerHighLightItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.HighLight';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'paintbrush.pointed';
    title: 'Highlight';
    tips: 'Highlight';
    bordered: True;
    alternateIconName: '#paintbrush.pointed.fill';

    onAction: @viewerHighlightAction;
  );

  viewerCropItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Crop';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'crop';
    title: 'Crop';
    tips: 'Crop';
    bordered: True;
    onAction: @viewerCropAction;
  );

  viewerRedEyesItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.RedEyes';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'eye';
    title: 'Red Eyes';
    tips: 'Red Eyes';
    bordered: True;
    onAction: @viewerRedEyesAction;
  );

  viewerPaintItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.Paint';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'pencil.tip';
    title: 'Paint';
    tips: 'Paint';
    bordered: True;
    alternateIconName: '#pencil.tip';

    onAction: @viewerPaintAction;
  );


  viewerPenModePenItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.PenMode.Pen';
    iconName: 'scribble';
    title: 'Pen';
    tips: 'Pen';
    bordered: True;
    onAction: nil;
  );

  viewerPenModeRectItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.PenMode.Rect';
    iconName: 'rectangle';
    title: 'Rect';
    tips: 'Rect';
    bordered: True;
    onAction: nil;
  );

  viewerPenModeEllipseItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.PenMode.Ellipse';
    iconName: 'oval';
    title: 'Ellipse';
    tips: 'Ellipse';
    bordered: True;
    onAction: nil;
  );

  viewerPenModeItemConfig: TCocoaConfigToolBarItemGroup = (
    identifier: 'ViewerForm.PenMode';
    priority: NSToolbarItemVisibilityPriorityLow;
    navigational: False;
    appValidates: True;
    iconName: '';
    title: '';
    tips: '';
    bordered: True;
    onAction: @viewerPenModeAction;

    representation: NSToolbarItemGroupControlRepresentationCollapsed;
    selectionMode: NSToolbarItemGroupSelectionModeSelectOne;
    selectedIndex: 0;
    subitems: (
    );
  );

  viewerPenWidthItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'ViewerForm.PenWidth';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'lineweight';
    title: 'Width';
    tips: '';
    bordered: True;
    onAction: nil;

    showsIndicator: False;
    menu: nil;
    onGetMenu: @onViewerGetPenWidthMenu;
  );

  viewerPenColorItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'ViewerForm.PenColor';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'eyedropper';
    title: 'Color';
    tips: '';
    bordered: True;
    onAction: @viewerPenColorAction;
  );

  viewerFormConfig: TCocoaConfigForm = (
    name: '';
    className: 'TfrmViewer';
    isMainForm: False;

    titleBar: (
      transparent: False;
      separatorStyle: NSTitlebarSeparatorStyleAutomatic;
    );

    toolBar: (
      identifier: 'ViewerForm.ToolBar';
      style: NSWindowToolbarStyleUnifiedCompact;
      displayMode: NSToolbarDisplayModeIconOnly;
      autoHideInFullScreen: True;

      allowsUserCustomization: False;
      autosavesConfiguration: False;

      items: (
      );
      defaultItemsIdentifiers: (
        'ViewerForm.Reload',
        'ViewerForm.Prev',
        'ViewerForm.Next',
        'ViewerForm.CopyFile',
        'ViewerForm.MoveFile',
        'ViewerForm.DeleteFile',
        'ViewerForm.Slide'
      );
      allowedItemsIdentifiers: (
        'ViewerForm.Reload',
        'ViewerForm.Prev',
        'ViewerForm.Next',
        'ViewerForm.CopyFile',
        'ViewerForm.MoveFile',
        'ViewerForm.DeleteFile',
        'ViewerForm.Slide',

        'ViewerForm.ZoomIn',
        'ViewerForm.ZoomOut',
        'ViewerForm.Rotate270',
        'ViewerForm.Rotate90',
        'ViewerForm.Mirror',
        'ViewerForm.Resize',

        'ViewerForm.GifMove',
        'ViewerForm.GifPrev',
        'ViewerForm.GifNext',
        'ViewerForm.GifExport',

        'ViewerForm.HighLight',
        'ViewerForm.Crop',
        'ViewerForm.RedEyes',
        'ViewerForm.Paint',
        'ViewerForm.PenMode',
        'ViewerForm.PenWidth',
        'ViewerForm.PenColor'
      );
      itemCreator: nil;      // default item Creator
    );
  );

procedure doInitViewerFormConfig;
begin
  viewerPenModeItemConfig.subitems:= [
    TCocoaToolBarUtils.toClass(viewerPenModePenItemConfig),
    TCocoaToolBarUtils.toClass(viewerPenModeRectItemConfig),
    TCocoaToolBarUtils.toClass(viewerPenModeEllipseItemConfig)
  ];

  viewerFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(viewerReloadItemConfig),
    TCocoaToolBarUtils.toClass(viewerPrevItemConfig),
    TCocoaToolBarUtils.toClass(viewerNextItemConfig),
    TCocoaToolBarUtils.toClass(viewerCopyFileItemConfig),
    TCocoaToolBarUtils.toClass(viewerMoveFileItemConfig),
    TCocoaToolBarUtils.toClass(viewerDeleteFileItemConfig),
    TCocoaToolBarUtils.toClass(viewerSlideItemConfig),

    TCocoaToolBarUtils.toClass(viewerZoomInItemConfig),
    TCocoaToolBarUtils.toClass(viewerZoomOutItemConfig),
    TCocoaToolBarUtils.toClass(viewerRotate270ItemConfig),
    TCocoaToolBarUtils.toClass(viewerRotate90ItemConfig),
    TCocoaToolBarUtils.toClass(viewerMirrorItemConfig),
    TCocoaToolBarUtils.toClass(viewerResizeItemConfig),

    TCocoaToolBarUtils.toClass(viewerGifMoveItemConfig),
    TCocoaToolBarUtils.toClass(viewerGifPrevItemConfig),
    TCocoaToolBarUtils.toClass(viewerGifNextItemConfig),
    TCocoaToolBarUtils.toClass(viewerGifExportItemConfig),

    TCocoaToolBarUtils.toClass(viewerHighLightItemConfig),
    TCocoaToolBarUtils.toClass(viewerCropItemConfig),
    TCocoaToolBarUtils.toClass(viewerRedEyesItemConfig),

    TCocoaToolBarUtils.toClass(viewerPaintItemConfig),
    TCocoaToolBarUtils.toClass(viewerPenModeItemConfig),
    TCocoaToolBarUtils.toClass(viewerPenWidthItemConfig),
    TCocoaToolBarUtils.toClass(viewerPenColorItemConfig)
  ];

  FreeAndNil( viewerFormHandler );
  viewerFormHandler:= TCocoaViewerFormHandler.Create;
end;

{ TCocoaViewerFormHandler }

procedure TCocoaViewerFormHandler.onShowModeChanged(
  const viewer: TfrmViewer;
  const mode: TViewerShowMode );
begin
  if mode = vsmImage then
    viewerAttachImageToolBar( viewer )
  else
    viewerDeattachImageToolBar( viewer )
end;

procedure TCocoaViewerFormHandler.onGifStateChanged(
  const viewer: TfrmViewer;
  const states: TViewerGifStates);
var
  moveEnabled: Boolean;
  prevEnabled: Boolean;
  nextEnabled: Boolean;
  exportEnabled: Boolean;

  toolBar: NSToolBar;
  item: NSToolBarItem;
begin
  if vgsIsGif in states then begin
    moveEnabled:= True;
    prevEnabled:= vgsPrevFrame in states;
    nextEnabled:= vgsNextFrame in states;
    exportEnabled:= True;
  end else begin
    moveEnabled:= False;
    prevEnabled:= False;
    nextEnabled:= False;
    exportEnabled:= False;
  end;

  viewerAttachImageToolBar( viewer );
  toolBar:= TCocoaToolBarUtils.getToolBar( viewer );
  if toolBar = nil then
    Exit;

  item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.GifMove');
  item.setEnabled( moveEnabled );
  TCocoaToolBarItem(item).lclSetState( vgsPlaying in states );

  item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.GifPrev');
  item.setEnabled( prevEnabled );
  item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.GifNext');
  item.setEnabled( nextEnabled );
  item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.GifExport');
  item.setEnabled( exportEnabled );
end;

procedure TCocoaViewerFormHandler.onImageEditStateChanged(
  const viewer: TfrmViewer );
var
  toolBar: NSToolBar;
  item: NSToolBarItem;

  procedure updateRotateResizeState;
  begin
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.Rotate270');
    item.setEnabled( viewer.btn270.Enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.Rotate90');
    item.setEnabled( viewer.btn90.Enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.Mirror');
    item.setEnabled( viewer.btnMirror.Enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.Resize');
    item.setEnabled( viewer.btnResize.Enabled );
  end;

  procedure updateHighlightState;
  var
    highlightButton: TToolButton;
  begin
    highlightButton:= viewer.btnHightlight;
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.HighLight');
    item.setEnabled( highlightButton.Enabled );
    TCocoaToolBarItem(item).lclSetState( highlightButton.Down );

    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.Crop');
    item.setEnabled( highlightButton.Down );
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.RedEyes');
    item.setEnabled( highlightButton.Down );
  end;

  procedure updatePaintState;
  var
    paintButton: TToolButton;
  begin
    paintButton:= viewer.btnPaint;
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.Paint');
    item.setEnabled( paintButton.Enabled );
    TCocoaToolBarItem(item).lclSetState( paintButton.Down );

    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.PenMode');
    item.setEnabled( paintButton.Down );
    NSToolbarItemGroup(item).setSelected_atIndex( True, viewer.btnPenMode.Tag );

    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.PenWidth');
    item.setEnabled( paintButton.Down );
    item:= TCocoaToolBarUtils.findItemByIdentifier( toolBar , 'ViewerForm.PenColor');
    item.setEnabled( paintButton.Down );
  end;

begin
  toolBar:= TCocoaToolBarUtils.getToolBar( viewer );
  if toolBar = nil then
    Exit;

  if NOT isImageToolBarAttached(viewer) then
    Exit;

  updateRotateResizeState;
  updateHighlightState;
  updatePaintState;
end;

procedure TCocoaViewerFormHandler.onSlideStateChanged(const viewer: TfrmViewer);
var
  item: NSToolBarItem;
begin
  item:= TCocoaToolBarUtils.findItemByIdentifier( viewer, 'ViewerForm.Slide' );
  if item = nil then
    Exit;

  TCocoaToolBarItem(item).lclSetState( viewer.btnSlideShow.Down );

  viewer.TimerViewer.Enabled:= viewer.btnSlideShow.Down;
end;

procedure TCocoaViewerFormHandler.onPenWidthChanged( Sender: TObject );
var
  toolBarMenuItem: TMenuItem absolute Sender;
  viewerMenuItem: TMenuItem;
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then begin
    viewerMenuItem:= form.pmPenWidth.Items[toolBarMenuItem.Tag-1];
    form.miPaintClick( viewerMenuItem );
  end;
end;

procedure TCocoaViewerFormHandler.onSlideStart( Sender: TObject );
var
  toolBarMenuItem: TMenuItem absolute Sender;
  viewerMenuItem: TMenuItem;
  form: TfrmViewer;
begin
  form:= getCurrentViewerForm;
  if Assigned(form) then begin
    viewerMenuItem:= form.pmTimeShow.Items[toolBarMenuItem.Tag-1];
    form.miPaintClick( viewerMenuItem );
    form.btnSlideShowSetState( True );
  end;
end;

