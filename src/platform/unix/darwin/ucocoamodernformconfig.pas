unit uCocoaModernFormConfig;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  Forms,
  fMain,
  CocoaAll, CocoaConfig, CocoaToolBar, Cocoa_Extra, CocoaUtils;

implementation

procedure toggleTreeViewAction( const Sender: id );
begin
  frmMain.Commands.cm_TreeView([]);
end;

procedure toggleHorzSplitAction( const Sender: id );
begin
  frmMain.Commands.cm_HorizontalFilePanels([]);
end;

const
  treeViewItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.TreeView';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'sidebar.left';
    title: 'Tree View';
    tips: 'Tree View Panel';
    bordered: True;
    onAction: @toggleTreeViewAction;
  );

  horzSplitItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.HorzSplit';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'rectangle.split.1x2';
    title: 'HorzSplit';
    tips: 'Horizontal Panels Mode';
    bordered: True;
    onAction: @toggleHorzSplitAction;
  );

  mainFormConfig: TCocoaConfigForm = (
    name: 'frmMain';
    className: '';
    isMainForm: False;

    titleBar: (
      transparent: False;
      separatorStyle: NSTitlebarSeparatorStyleAutomatic;
    );

    toolBar: (
      identifier: 'MainForm.ToolBar';
      style: NSWindowToolbarStyleAutomatic;
      displayMode: NSToolbarDisplayModeIconOnly;

      allowsUserCustomization: True;
      autosavesConfiguration: False;

      items: (
      );
      defaultItemsIdentifiers: (
        'MainForm.TreeView',
        'MainForm.HorzSplit'
      );
      allowedItemsIdentifiers: (
      'MainForm.TreeView',
      'MainForm.HorzSplit'
      );
      itemCreator: nil;      // default item Creator
    );
  );

procedure initCocoaModernFormConfig;
begin
  mainFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(treeViewItemConfig),
    TCocoaToolBarUtils.toClass(horzSplitItemConfig)
  ];

  CocoaConfigForms:= [ mainFormConfig ];
end;

initialization
  if NSAppKitVersionNumber >= NSAppKitVersionNumber11_0 then
    initCocoaModernFormConfig;

end.

