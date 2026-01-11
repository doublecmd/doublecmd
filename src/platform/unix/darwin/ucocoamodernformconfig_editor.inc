function getCurrentEditorForm: TfrmEditor;
var
  form: TCustomForm;
begin
  Result:= nil;
  form:= Screen.ActiveCustomForm;
  if form is TfrmEditor then
    Result:= TfrmEditor(form);
end;

procedure editorNewAction( const Sender: id );
var
  form: TfrmEditor;
begin
  form:= getCurrentEditorForm;
  if Assigned(form) then
    form.cm_FileNew([]);
end;

procedure editorOpenAction( const Sender: id );
var
  form: TfrmEditor;
begin
  form:= getCurrentEditorForm;
  if Assigned(form) then
    form.cm_FileOpen([]);
end;

procedure editorSaveAction( const Sender: id );
var
  form: TfrmEditor;
begin
  form:= getCurrentEditorForm;
  if Assigned(form) then
    form.cm_FileSave([]);
end;

procedure editorFindAction( const Sender: id );
var
  form: TfrmEditor;
begin
  form:= getCurrentEditorForm;
  if Assigned(form) then
    form.cm_EditFind([]);
end;

procedure editorReplaceAction( const Sender: id );
var
  form: TfrmEditor;
begin
  form:= getCurrentEditorForm;
  if Assigned(form) then
    form.cm_EditRplc([]);
end;

procedure editorConfigurationAction( const Sender: id );
var
  form: TfrmEditor;
begin
  form:= getCurrentEditorForm;
  if Assigned(form) then
    form.cm_ConfHigh([]);
end;


const
  editorNewItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'EditorForm.New';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'doc';
    title: 'New';
    tips: 'New';
    bordered: True;
    onAction: @editorNewAction;
  );

  editorOpenItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'EditorForm.Open';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'folder';
    title: 'Open';
    tips: 'Open';
    bordered: True;
    onAction: @editorOpenAction;
  );

  editorSaveItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'EditorForm.Save';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'square.and.arrow.down';
    title: 'Save';
    tips: 'Save';
    bordered: True;
    onAction: @editorSaveAction;
  );

  editorFindItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'EditorForm.Find';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'magnifyingglass';
    title: 'Find';
    tips: 'Find';
    bordered: True;
    onAction: @editorFindAction;
  );

  editorReplaceItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'EditorForm.Replace';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'doc.text.magnifyingglass';
    title: 'Replace';
    tips: 'Replace';
    bordered: True;
    onAction: @editorReplaceAction;
  );

  editorConfigurationItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'EditorForm.Configuration';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'paperplane';
    title: 'Configuration';
    tips: 'Configuration';
    bordered: True;
    onAction: @editorConfigurationAction;
  );

  editorFormConfig: TCocoaConfigForm = (
    name: '';
    className: 'TfrmEditor';
    isMainForm: False;

    titleBar: (
      transparent: False;
      separatorStyle: NSTitlebarSeparatorStyleAutomatic;
    );

    toolBar: (
      identifier: 'EditorForm.ToolBar';
      style: NSWindowToolbarStyleAutomatic;
      displayMode: NSToolbarDisplayModeIconOnly;

      allowsUserCustomization: True;
      autosavesConfiguration: False;

      items: (
      );
      defaultItemsIdentifiers: (
        'EditorForm.New',
        'EditorForm.Open',
        'EditorForm.Save',
        'NSToolbarFlexibleSpaceItem',
        'EditorForm.Find',
        'EditorForm.Replace',
        'NSToolbarFlexibleSpaceItem',
        'EditorForm.Configuration'
      );
      allowedItemsIdentifiers: (
        'EditorForm.New',
        'EditorForm.Open',
        'EditorForm.Save',
        'EditorForm.Find',
        'EditorForm.Replace',
        'EditorForm.Configuration'
      );
      itemCreator: nil;      // default item Creator
    );
  );


procedure translateItemConfig(
  const poFile: TPOFile;
  const identifier: String;
  var item: TCocoaConfigToolBarItemWithUI );
var
  s: String;
begin
  s:= poFile.Translate( identifier, item.tips );
  item.title:= s;
  item.tips:= s;
end;

procedure translateEditorFormToolbarItem;
var
  poFile: TPOFile;
begin
  if NOT (LRSTranslator is TTranslator) then
    Exit;

  poFile:= TTranslator(LRSTranslator).POFile;
  translateItemConfig( poFile, 'tfrmeditor.actfilenew.hint', editorNewItemConfig );
  translateItemConfig( poFile, 'tfrmeditor.actfileopen.hint', editorOpenItemConfig );
  translateItemConfig( poFile, 'tfrmeditor.actfilesave.hint', editorSaveItemConfig );
  translateItemConfig( poFile, 'tfrmeditor.acteditfind.hint', editorFindItemConfig );
  translateItemConfig( poFile, 'tfrmeditor.acteditrplc.hint', editorReplaceItemConfig );
  translateItemConfig( poFile, 'tfrmeditor.actconfhigh.hint', editorConfigurationItemConfig );
end;

procedure addEditorFormShortCutTips;
begin
  addShortCutTipsToItemConfig( 'Editor', 'cm_FileNew', editorNewItemConfig );
  addShortCutTipsToItemConfig( 'Editor', 'cm_FileOpen', editorOpenItemConfig );
  addShortCutTipsToItemConfig( 'Editor', 'cm_FileSave', editorSaveItemConfig );
  addShortCutTipsToItemConfig( 'Editor', 'cm_EditFind', editorFindItemConfig );
  addShortCutTipsToItemConfig( 'Editor', 'cm_EditRplc', editorReplaceItemConfig );
  addShortCutTipsToItemConfig( 'Editor', 'cm_ConfHigh', editorConfigurationItemConfig );
end;

procedure doInitEditorFormConfig;
begin
  translateEditorFormToolbarItem;
  addEditorFormShortCutTips;

  editorFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(editorNewItemConfig),
    TCocoaToolBarUtils.toClass(editorOpenItemConfig),
    TCocoaToolBarUtils.toClass(editorSaveItemConfig),
    TCocoaToolBarUtils.toClass(editorFindItemConfig),
    TCocoaToolBarUtils.toClass(editorReplaceItemConfig),
    TCocoaToolBarUtils.toClass(editorConfigurationItemConfig)
  ];
end;

