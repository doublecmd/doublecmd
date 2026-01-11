function getCurrentDifferForm: TfrmDiffer;
var
  form: TCustomForm;
begin
  Result:= nil;
  form:= Screen.ActiveCustomForm;
  if form is TfrmDiffer then
    Result:= TfrmDiffer(form);
end;

procedure differReloadAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_Reload([]);
end;

procedure differSaveAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.actSaveExecute( nil );
end;

procedure differSaveAsAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.actSaveAsExecute( nil );
end;

procedure differCompareAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.actStartCompareExecute( nil );
end;

procedure differCancelAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.actCancelCompareExecute( nil );
end;

procedure differNextDifferenceAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_NextDifference([]);
end;

procedure differPrevDifferenceAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_PrevDifference([]);
end;

procedure differLastDifferenceAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_LastDifference([]);
end;

procedure differFirstDifferenceAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_FirstDifference([]);
end;

procedure differCopyRightToLeftAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_CopyRightToLeft([]);
end;

procedure differCopyLeftToRightAction( const Sender: id );
var
  form: TfrmDiffer;
begin
  form:= getCurrentDifferForm;
  if Assigned(form) then
    form.cm_CopyLeftToRight([]);
end;

const
  differReloadItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.Reload';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrow.clockwise';
    title: 'Reload';
    tips: 'Reload';
    bordered: True;
    onAction: @differReloadAction;
  );

  differSaveItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.Save';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'square.and.arrow.down';
    title: 'Save';
    tips: 'Save';
    bordered: True;
    onAction: @differSaveAction;
  );

  differSaveAsItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.SaveAs';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'square.and.arrow.down.on.square';
    title: 'Save as';
    tips: 'Save as...';
    bordered: True;
    onAction: @differSaveAsAction;
  );

  differCompareItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.Compare';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'play.fill';
    title: 'Compare';
    tips: 'Compare';
    bordered: True;
    onAction: @differCompareAction;
  );

  differCancelItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.Cancel';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'stop.circle.fill';
    title: 'Cancel';
    tips: 'Cancel';
    bordered: True;
    onAction: @differCancelAction;
  );

  differNextDifferenceItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.NextDifference';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrow.down';
    title: 'Next Difference';
    tips: 'Next Difference';
    bordered: True;
    onAction: @differNextDifferenceAction;
  );

  differPrevDifferenceItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.PrevDifference';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrow.up';
    title: 'Previous Difference';
    tips: 'Previous Difference';
    bordered: True;
    onAction: @differPrevDifferenceAction;
  );

  differLastDifferenceItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.LastDifference';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrow.down.to.line';
    title: 'Last Difference';
    tips: 'Last Difference';
    bordered: True;
    onAction: @differLastDifferenceAction;
  );

  differFirstDifferenceItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.FirstDifference';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrow.up.to.line';
    title: 'First Difference';
    tips: 'First Difference';
    bordered: True;
    onAction: @differFirstDifferenceAction;
  );

  differCopyRightToLeftItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.CopyRightToLeft';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrowshape.turn.up.left';
    title: 'Copy Block Left';
    tips: 'Copy Block Left';
    bordered: True;
    onAction: @differCopyRightToLeftAction;
  );

  differCopyLeftToRightItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'DifferForm.CopyLeftToRight';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrowshape.turn.up.right';
    title: 'Copy Block Right';
    tips: 'Copy Block Right';
    bordered: True;
    onAction: @differCopyLeftToRightAction;
  );


  differFormConfig: TCocoaConfigForm = (
    name: '';
    className: 'TfrmDiffer';
    isMainForm: False;

    titleBar: (
      transparent: False;
      separatorStyle: NSTitlebarSeparatorStyleAutomatic;
    );

    toolBar: (
      identifier: 'DifferForm.ToolBar';
      style: NSWindowToolbarStyleAutomatic;
      displayMode: NSToolbarDisplayModeIconOnly;

      allowsUserCustomization: True;
      autosavesConfiguration: False;

      items: (
      );
      defaultItemsIdentifiers: (
        'DifferForm.Reload',
        'DifferForm.Save',
        'DifferForm.SaveAs',
        'NSToolbarFlexibleSpaceItem',
        'DifferForm.Compare',
        'DifferForm.Cancel',
        'NSToolbarFlexibleSpaceItem',
        'DifferForm.NextDifference',
        'DifferForm.PrevDifference',
        'DifferForm.LastDifference',
        'DifferForm.FirstDifference',
        'NSToolbarFlexibleSpaceItem',
        'DifferForm.CopyRightToLeft',
        'DifferForm.CopyLeftToRight'
      );
      allowedItemsIdentifiers: (
        'DifferForm.Reload',
        'DifferForm.Save',
        'DifferForm.SaveAs',
        'DifferForm.Compare',
        'DifferForm.Cancel',
        'DifferForm.NextDifference',
        'DifferForm.PrevDifference',
        'DifferForm.LastDifference',
        'DifferForm.FirstDifference',
        'DifferForm.CopyRightToLeft',
        'DifferForm.CopyLeftToRight'
      );
      itemCreator: nil;      // default item Creator
    );
  );


procedure translateDifferFormToolbarItem;
var
  poFile: TPOFile;
begin
  if NOT (LRSTranslator is TTranslator) then
    Exit;

  poFile:= TTranslator(LRSTranslator).POFile;
  translateItemConfig( poFile, 'tfrmdiffer.actreload.hint', differReloadItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actsave.hint', differSaveItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actsaveas.hint', differSaveAsItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actstartcompare.hint', differCompareItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actcancelcompare.hint', differCancelItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actnextdifference.hint', differNextDifferenceItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actprevdifference.hint', differPrevDifferenceItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actlastdifference.hint', differLastDifferenceItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actfirstdifference.hint', differFirstDifferenceItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actcopyrighttoleft.hint', differCopyRightToLeftItemConfig );
  translateItemConfig( poFile, 'tfrmdiffer.actcopylefttoright.hint', differCopyLeftToRightItemConfig );
end;

procedure addDifferFormShortCutTips;
begin
  addShortCutTipsToItemConfig( 'Differ', 'cm_Reload', differReloadItemConfig );
  addShortCutTipsToItemConfig( 'Differ', 'cm_NextDifference', differNextDifferenceItemConfig );
  addShortCutTipsToItemConfig( 'Differ', 'cm_PrevDifference', differPrevDifferenceItemConfig );
  addShortCutTipsToItemConfig( 'Differ', 'cm_LastDifference', differLastDifferenceItemConfig );
  addShortCutTipsToItemConfig( 'Differ', 'cm_FirstDifference', differFirstDifferenceItemConfig );
  addShortCutTipsToItemConfig( 'Differ', 'cm_CopyRightToLeft', differCopyRightToLeftItemConfig );
  addShortCutTipsToItemConfig( 'Differ', 'cm_CopyLeftToRight', differCopyLeftToRightItemConfig );
end;

procedure doInitDifferFormConfig;
begin
  translateDifferFormToolbarItem;
  addDifferFormShortCutTips;

  differFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(differReloadItemConfig),
    TCocoaToolBarUtils.toClass(differSaveItemConfig),
    TCocoaToolBarUtils.toClass(differSaveAsItemConfig),
    TCocoaToolBarUtils.toClass(differCompareItemConfig),
    TCocoaToolBarUtils.toClass(differCancelItemConfig),
    TCocoaToolBarUtils.toClass(differNextDifferenceItemConfig),
    TCocoaToolBarUtils.toClass(differPrevDifferenceItemConfig),
    TCocoaToolBarUtils.toClass(differLastDifferenceItemConfig),
    TCocoaToolBarUtils.toClass(differFirstDifferenceItemConfig),
    TCocoaToolBarUtils.toClass(differCopyRightToLeftItemConfig),
    TCocoaToolBarUtils.toClass(differCopyLeftToRightItemConfig)
  ];
end;

