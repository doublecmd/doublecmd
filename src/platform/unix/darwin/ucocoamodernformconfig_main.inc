procedure toggleTreeViewAction( const Sender: id );
begin
  frmMain.Commands.cm_TreeView([]);
end;

procedure toggleHorzSplitAction( const Sender: id );
begin
  frmMain.Commands.cm_HorizontalFilePanels([]);
end;

procedure swapPanelsAction( const Sender: id );
begin
  frmMain.Commands.cm_Exchange([]);
end;

procedure goBackwardAction( const Sender: id );
begin
  frmMain.Commands.cm_ViewHistoryPrev([]);
end;

procedure goForwardAction( const Sender: id );
begin
  frmMain.Commands.cm_ViewHistoryNext([]);
end;

procedure showModeAction( const Sender: id );
var
  showModeItem: NSToolBarItemGroup absolute Sender;
begin
  case showModeItem.selectedIndex of
    0: frmMain.Commands.cm_BriefView([]);
    1: frmMain.Commands.cm_ColumnsView([]);
    2: frmMain.Commands.cm_ThumbnailsView([]);
  end;
end;

procedure onFileViewUpdated( const fileView: TFileView );

  procedure updateShowMode;
  var
    itemGroup: NSToolbarItemGroup;
    itemGroupWrapper: TCocoaToolBarItemGroupWrapper;
  begin
    itemGroup:= NSToolbarItemGroup( TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.ShowMode' ) );
    if NOT Assigned(itemGroup) then
      Exit;
    itemGroupWrapper:= TCocoaToolBarItemGroupWrapper( itemGroup.target );

    if fileView is TColumnsFileView then
      itemGroupWrapper.lclSetSelectedIndex( 1 )
    else if fileView is TBriefFileView then
      itemGroupWrapper.lclSetSelectedIndex( 0 )
    else if fileView is TThumbFileView then
      itemGroupWrapper.lclSetSelectedIndex( 2 );
  end;

  procedure updateBackward;
  var
    item: NSToolbarItem;
  begin
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.GoBackward' );
    if NOT Assigned(item) then
      Exit;
    item.setEnabled( fileView.hasPrevHistory );
  end;

  procedure updateForward;
  var
    item: NSToolbarItem;
  begin
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.GoForward' );
    if NOT Assigned(item) then
      Exit;
    item.setEnabled( fileView.hasNextHistory );
  end;

  procedure updateRelyOnFileSystem;
  var
    enabled: Boolean;
    item: NSToolbarItem;
  begin
    enabled:= fileView.FileSource.IsClass( TFileSystemFileSource );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.Share' );
    item.setEnabled( enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.AirDrop' );
    item.setEnabled( enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.QuickLook' );
    item.setEnabled( enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.Tag' );
    item.setEnabled( enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.FinderReveal' );
    item.setEnabled( enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.FinderInfo' );
    item.setEnabled( enabled );
    item:= TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.Terminal' );
    item.setEnabled( enabled );
  end;

begin
  updateShowMode;
  updateBackward;
  updateForward;
  updateRelyOnFileSystem;
end;

function onGetSharingItems( item: NSToolBarItem ): TStringArray;
begin
  Result:= frmMain.NSServiceMenuGetFilenames();
end;

procedure airdropAction( const Sender: id );
begin
  TDarwinPanelUtil.showAirDrop;
end;

procedure quickLookAction( const Sender: id );
begin
  TDarwinPanelUtil.showQuickLook;
end;

procedure editTagAction( const Sender: id );
begin
  TDarwinPanelUtil.showEditFinderTags( Sender, frmMain );
end;

procedure finderRevealAction( const Sender: id );
begin
  TDarwinApplicationUtil.performService( 'Finder/Reveal' );
end;

procedure finderInfoAction( const Sender: id );
begin
  TDarwinApplicationUtil.performService( 'Finder/Show Info' );
end;

procedure compareAction( const Sender: id );
begin
  frmMain.Commands.cm_CompareContents([]);
end;

procedure syncAction( const Sender: id );
begin
  frmMain.Commands.cm_SyncDirs([]);
end;

procedure editAction( const Sender: id );
begin
  frmMain.Commands.cm_Edit([]);
end;

const
  COMMON_FOLDERS: TStringArray = (
    '~/Documents',
    '~/Desktop',
    '~/Downloads',
    '~',
    '~/Pictures',
    '~/Movies',
    '~/Music',
    '/Applications/Utilities',
    '~/Library',
    '/Applications',

    '~/Library/Saved Searches',

    '~/.Trash'
  );

type

  { TToolBarMenuHandler }

  TToolBarMenuHandler = class
  public
    procedure showHotlist( Sender: TObject );
    procedure showFavoriteTabs( Sender: TObject );
    procedure showQuickLookPanel( Sender: TObject );
    procedure showEditTagPanel( Sender: TObject );
    procedure goToFolder( Sender: TObject );

    procedure showFinderTagSelectorPanel( Sender: TObject );
    procedure onFinderTagSelectorPanelClose( const cancel: Boolean; const tagNames: NSArray );
    procedure searchOneFinderTagAction( Sender: TObject );
  end;

var
  toolBarMenuHandler: TToolBarMenuHandler;

procedure TToolBarMenuHandler.showHotlist( Sender: TObject );
begin
  frmMain.Commands.cm_DirHotList(['position=cursor']);
end;

procedure TToolBarMenuHandler.showFavoriteTabs( Sender: TObject );
begin
  frmMain.Commands.cm_LoadFavoriteTabs(['position=cursor']);
end;

procedure TToolBarMenuHandler.showQuickLookPanel( Sender: TObject );
begin
  quickLookAction( nil );
end;

procedure TToolBarMenuHandler.showEditTagPanel( Sender: TObject );
begin
  editTagAction( nil );
end;

procedure TToolBarMenuHandler.goToFolder(Sender: TObject);
var
  menuItem: TMenuItem absolute Sender;
  path: String;
begin
  path:= uDCUtils.ReplaceTilde( COMMON_FOLDERS[menuItem.Tag] );
  frmMain.Commands.cm_ChangeDir( [path] );
end;

procedure TToolBarMenuHandler.showFinderTagSelectorPanel(Sender: TObject);
begin
  TDarwinFinderUtil.popoverTagsSelector(
    rsMFSTBISearchCombinedTags,
    @self.onFinderTagSelectorPanelClose,
    NSView(frmMain.Handle) , NSMaxYEdge );
end;

procedure TToolBarMenuHandler.onFinderTagSelectorPanelClose(const cancel: Boolean;
  const tagNames: NSArray);
begin
  if cancel then
    Exit;
  if tagNames.count = 0 then
    Exit;
  TDarwinFinderModelUtil.searchFilesForTagNames( tagNames, @darwinSearchResultHandler.onSearchFinderTagComplete );
end;

procedure TToolBarMenuHandler.searchOneFinderTagAction( Sender: TObject );
var
  menuItem: TMenuItem absolute Sender;
  tagName: NSString;
begin
  tagName:= StringToNSString( menuItem.Caption );
  TDarwinFinderModelUtil.searchFilesForTagName( tagName, @darwinSearchResultHandler.onSearchFinderTagComplete );
end;

procedure onGetFolderMenu( const menu: TMenu );
var
  items: TMenuItem;
  tag: PtrInt = 0;

  procedure newItem( imageName: String );
  var
    folderName: String;
    lclItem: TMenuItem;
    cocoaItem: NSMenuItem;
    cocoaImage: NSImage;
  begin
    folderName:= TDarwinFileUtil.getDisplayName( COMMON_FOLDERS[tag] );
    lclItem:= TMenuItem.Create( menu );
    lclItem.Caption:= folderName;
    lclItem.onClick:= @toolBarMenuHandler.goToFolder;
    lclItem.Tag:= tag;
    items.Add( lclItem );

    cocoaImage:= NSImage.imageWithSystemSymbolName_accessibilityDescription(
      NSSTR(imageName), nil );
    cocoaItem:= NSMenuItem( lclItem.Handle );
    cocoaItem.setImage( cocoaImage );

    inc( tag );
  end;

begin
  items:= menu.Items;
  newItem('doc');
  newItem('menubar.dock.rectangle');
  newItem('arrow.down.circle');
  newItem('house');
  newItem('photo');
  newItem('film');
  newItem('music.quarternote.3');
  newItem('wrench.and.screwdriver.fill');
  newItem('l.circle');
  newItem('a.circle');
  items.AddSeparator;
  newItem('folder.fill.badge.gearshape');
  items.AddSeparator;
  newItem('trash');
end;

function copyMenuItem( owner: TComponent; source: TMenuItem ): TMenuItem;
begin
  Result:= TMenuItem.Create( owner );
  Result.Caption:= source.Caption;
  Result.Action:= source.Action;
end;

procedure onGetCommandMenu( const menu: TMenu );
  function toItem( source: TMenuItem ): TMenuItem;
  begin
    Result:= copyMenuItem( menu, source );
  end;

  function createShowHotlistMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMFSTBICommandMenuDirectoryHotlist;
    Result.OnClick:= @toolBarMenuHandler.showHotlist;
  end;

  function createShowFavoriteMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMFSTBICommandMenuFavoriteTabs;
    Result.OnClick:= @toolBarMenuHandler.showFavoriteTabs;
  end;

  function createQuickLookMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMFSTBICommandMenuQuickLook;
    Result.OnClick:= @toolBarMenuHandler.showQuickLookPanel;
    Result.ShortCut:= ShortCut( Word(' '), [ssAlt] );
  end;

  function createTagMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMenuMacOSEditFinderTags;
    Result.OnClick:= @toolBarMenuHandler.showEditTagPanel;
  end;

var
  items: TMenuItem;

begin
  items:= menu.Items;
  items.Add( toItem(frmMain.miMultiRename) );
  items.Add( toItem(frmMain.mnuFilesCmpCnt) );
  items.Add( toItem(frmMain.mnuCmdSyncDirs) );
  items.AddSeparator;
  items.Add( toItem(frmMain.mnuCmdSearch) );
  items.Add( toItem(frmMain.mnuCmdAddNewSearch) );
  items.Add( toItem(frmMain.mnuCmdViewSearches) );
  items.AddSeparator;
  items.Add( createShowHotlistMenuItem );
  items.Add( toItem(frmMain.mnuCmdConfigDirHotlist) );
  items.AddSeparator;
  items.Add( createShowFavoriteMenuItem );
  items.Add( toItem(frmMain.mnuConfigFavoriteTabs) );
  items.AddSeparator;
  items.Add( createQuickLookMenuItem );
  items.Add( toItem(frmMain.mnuQuickView) );
  items.Add( toItem(frmMain.mnuFilesShwSysFiles) );
  items.Add( toItem(frmMain.mnuShowOperations) );
  items.AddSeparator;
  items.Add( toItem(frmMain.miEditComment) );
  items.Add( createTagMenuItem );
  items.AddSeparator;
  items.Add( toItem(frmMain.mnuFilesSymLink) );
  items.Add( toItem(frmMain.mnuFilesHardLink) );
  items.AddSeparator;
  items.Add( toItem(frmMain.mnuSetFileProperties) );
  items.Add( toItem(frmMain.mnuFilesProperties) );
  items.AddSeparator;
  items.Add( toItem(frmMain.mnuCheckSumCalc) );
  items.Add( toItem(frmMain.mnuCheckSumVerify) );
end;

procedure onGetSearchMenu( const menu: TMenu );
var
  items: TMenuItem;

  procedure createSidebarTagMenuItem;
  var
    tagNames: NSArray;
    tagName: NSString;
    menuItem: TMenuItem;
  begin
    tagNames:= TDarwinFinderModelUtil.getSidebarTagNames;
    for tagName in tagNames do begin
      menuItem:= TMenuItem.Create( menu );
      menuItem.Caption:= tagName.UTF8String;
      menuItem.OnClick:= @toolBarMenuHandler.searchOneFinderTagAction;
      items.Add( menuItem );
    end;
    TDarwinFinderUtil.attachSearchForTagsMenu( menu );
  end;

  procedure createCombinedTagMenuItem;
  var
    menuItem: TMenuItem;
  begin
    menuItem:= TMenuItem.Create( menu );
    menuItem.Caption:= rsMFSTBISearchCombinedTags;
    menuItem.OnClick:= @toolBarMenuHandler.showFinderTagSelectorPanel;
    items.Add( menuItem );
  end;

  procedure createSearchFileMenuItem;
  var
    menuItem: TMenuItem;
  begin
    menuItem:= copyMenuItem( menu, frmMain.mnuCmdSearch );
    items.Add( menuItem );
  end;

begin
  items:= menu.Items;
  createSidebarTagMenuItem;
  items.AddSeparator;
  createCombinedTagMenuItem;
  items.AddSeparator;
  createSearchFileMenuItem;
end;

procedure iCloudAction( const Sender: id );
begin
  TDarwinFileViewUtil.addiCloudDrivePage;
end;

procedure networkAction( const Sender: id );
begin
  frmMain.Commands.DoOpenVirtualFileSystemList( frmMain.ActiveFrame );
end;

procedure terminalAction( const Sender: id );
begin
  frmMain.Commands.cm_RunTerm([]);
end;

procedure refreshAction( const Sender: id );
begin
  frmMain.Commands.cm_Refresh([]);
end;

procedure searchFilesAction( const Sender: id );
begin
  frmMain.Commands.cm_Search([]);
end;

procedure privilegeAction( const Sender: id );
begin
  TDarwinApplicationUtil.openSystemSecurityPreferences_PrivacyAllFiles;
end;

const
  treeViewItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.TreeView';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'sidebar.left';
    title: rsMFSTBITreeViewTitle;
    tips: rsMFSTBITreeViewTips;
    bordered: True;
    onAction: @toggleTreeViewAction;
  );

  horzSplitItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.HorzSplit';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'rectangle.split.1x2';
    title: rsMFSTBIHorzSplitTitle;
    tips: rsMFSTBIHorzSplitTips;
    bordered: True;
    onAction: @toggleHorzSplitAction;
  );

  swapPanelsItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.SwapPanels';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'arrow.left.arrow.right.square';
    title: rsMFSTBISwapPanelsTitle;
    tips: rsMFSTBISwapPanelsTips;
    bordered: True;
    onAction: @swapPanelsAction;
  );


  goBackwardItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.GoBackward';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'chevron.backward';
    title: rsMFSTBIGoBackwardTitle;
    tips: rsMFSTBIGoBackwardTips;
    bordered: True;
    onAction: @goBackwardAction;
  );

  goForwardItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.GoForward';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'chevron.forward';
    title: rsMFSTBIGoForwardTitle;
    tips: rsMFSTBIGoForwardTips;
    bordered: True;
    onAction: @goForwardAction;
  );

  iCloudItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.iCloud';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'cloud';
    title: rsMFSTBIiCloudDriveTitle;
    tips: rsMFSTBIiCloudDriveTips;
    bordered: True;
    onAction: @iCloudAction;
  );

  networkItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.network';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'network';
    title: rsMFSTBINetworkTitle;
    tips: rsMFSTBINetworkTips;
    bordered: True;
    onAction: @networkAction;
  );


  showBriefItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.ShowMode.Brief';
    iconName: 'rectangle.split.3x1';
    title: rsMFSTBIShowBriefTitle;
    tips: '';
    bordered: True;
    onAction: nil;
  );

  showFullItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.ShowMode.Full';
    iconName: 'list.bullet';
    title: rsMFSTBIShowFullTitle;
    tips: '';
    bordered: True;
    onAction: nil;
  );

  showThumbnailsItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.ShowMode.Thumbnails';
    iconName: 'square.grid.2x2';
    title: rsMFSTBIShowThumbnailsTitle;
    tips: '';
    bordered: True;
    onAction: nil;
  );

  showModeItemConfig: TCocoaConfigToolBarItemGroup = (
    identifier: 'MainForm.ShowMode';
    priority: NSToolbarItemVisibilityPriorityHigh;
    iconName: '';
    title: rsMFSTBIShowModeTitle;
    tips: rsMFSTBIShowModeTips;
    bordered: True;
    onAction: @showModeAction;

    representation: NSToolbarItemGroupControlRepresentationAutomatic;
    selectionMode: NSToolbarItemGroupSelectionModeSelectOne;
    selectedIndex: 0;
    subitems: (
    );
  );


  shareItemConfig: TCocoaConfigToolBarItemSharing = (
    identifier: 'MainForm.Share';
    priority: NSToolbarItemVisibilityPriorityUser;
    appValidates: True;
    iconName: '';
    title: rsMFSTBIShareTitle;
    tips: '';
    bordered: True;

    onGetItems: @onGetSharingItems;
  );

  airdropItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.AirDrop';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'airplayaudio';
    title: rsMFSTBIAirDropTitle;
    tips: rsMFSTBIAirDropTips;
    bordered: True;
    onAction: @airdropAction;
  );


  quickLookItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.QuickLook';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'eye';
    title: rsMFSTBIQuickLookTitle;
    tips: rsMFSTBIQuickLookTips;
    bordered: True;
    onAction: @quickLookAction;
  );

  tagItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Tag';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'tag';
    title: rsMFSTBIEditFinderTagTitle;
    tips: rsMFSTBIEditFinderTagTips;
    bordered: True;
    onAction: @editTagAction;
  );

  goItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'MainForm.Go';
    iconName: 'folder';
    title: rsMFSTBIGoTitle;
    tips: rsMFSTBIGoTips;
    bordered: True;
    onAction: nil;

    showsIndicator: False;
    menu: nil;
    onGetMenu: @onGetFolderMenu;
  );

  finderRevealItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.FinderReveal';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     appValidates: True;
     iconName: 'faceid';
     title: rsMFSTBIFinderRevealTitle;
     tips: rsMFSTBIFinderRevealTips;
     bordered: True;
     onAction: @finderRevealAction;
   );

  finderInfoItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.FinderInfo';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     appValidates: True;
     iconName: 'info.circle';
     title: rsMFSTBIShowInfoTitle;
     tips: rsMFSTBIShowInfoTips;
     bordered: True;
     onAction: @finderInfoAction;
   );


  commandItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'MainForm.Command';
    iconName: 'ellipsis.circle';
    title: rsMFSTBICommandTitle;
    tips: '';
    bordered: True;
    onAction: nil;

    showsIndicator: True;
    menu: nil;
    onGetMenu: @onGetCommandMenu;
  );


  terminalItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Terminal';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    appValidates: True;
    iconName: 'terminal';
    title: rsMFSTBITerminalTitle;
    tips: rsMFSTBITerminalTips;
    bordered: True;
    onAction: @terminalAction;
  );

  searchItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'MainForm.Search';
    iconName: 'magnifyingglass';
    title: rsMFSTBISearchTitle;
    tips: rsMFSTBISearchTips;
    bordered: True;
    onAction: nil;

    showsIndicator: False;
    dynamic: True;
    menu: nil;
    onGetMenu: @onGetSearchMenu;
  );

  privilegeItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Privilege';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'lock.shield';
    title: rsMFSTBIPrivilegeTitle;
    tips: rsMFSTBIPrivilegeTips;
    bordered: True;
    onAction: @privilegeAction;
  );


  refreshItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.Refresh';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     iconName: 'arrow.clockwise';
     title: rsMFSTBIRefreshTitle;
     tips: rsMFSTBIRefreshTips;
     bordered: True;
     onAction: @refreshAction;
   );

  compareItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Compare';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'rectangle.on.rectangle';
    title: rsMFSTBICompareTitle;
    tips: rsMFSTBICompareTips;
    bordered: True;
    onAction: @compareAction;
  );

  syncItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Sync';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'arrow.triangle.2.circlepath';
    title: rsMFSTBISyncTitle;
    tips: rsMFSTBISyncTips;
    bordered: True;
    onAction: @syncAction;
  );

  editItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Edit';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'square.and.pencil';
    title: rsMFSTBIEditTitle;
    tips: rsMFSTBIEditTips;
    bordered: True;
    onAction: @editAction;
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
        'MainForm.HorzSplit',
        'MainForm.SwapPanels',

        'MainForm.GoBackward',
        'MainForm.GoForward',
        'NSToolbarFlexibleSpaceItem',

        'MainForm.iCloud',
        'MainForm.network',
        'NSToolbarFlexibleSpaceItem',

        'MainForm.ShowMode',
        'NSToolbarFlexibleSpaceItem',

        'MainForm.Share',
        'MainForm.AirDrop',
        'NSToolbarFlexibleSpaceItem',

        'MainForm.QuickLook',
        'MainForm.Tag',
        'MainForm.Go',
        'MainForm.FinderReveal',
        'MainForm.FinderInfo',
        'NSToolbarFlexibleSpaceItem',
        'MainForm.Command',
        'NSToolbarFlexibleSpaceItem',
        'MainForm.Search',
        'MainForm.Terminal',
        'MainForm.Privilege'
      );
      allowedItemsIdentifiers: (
        'MainForm.TreeView',
        'MainForm.HorzSplit',
        'MainForm.SwapPanels',
        'MainForm.GoBackward',
        'MainForm.GoForward',

        'MainForm.iCloud',
        'MainForm.network',
        'MainForm.ShowMode',
        'MainForm.Share',
        'MainForm.AirDrop',
        'MainForm.QuickLook',
        'MainForm.Tag',
        'MainForm.Go',
        'MainForm.FinderReveal',
        'MainForm.FinderInfo',
        'MainForm.Command',

        'MainForm.Search',
        'MainForm.Terminal',
        'MainForm.Privilege',

        'MainForm.Refresh',
        'MainForm.Compare',
        'MainForm.Sync',
        'MainForm.Edit'
      );
      itemCreator: nil;      // default item Creator
    );
  );

procedure addMainFormShortCutTips;
begin
  addShortCutTipsToItemConfig( 'Main', 'cm_ViewHistoryPrev', goBackwardItemConfig );
  addShortCutTipsToItemConfig( 'Main', 'cm_ViewHistoryNext', goForwardItemConfig );
end;

procedure doInitMainFormConfig;
begin
  addMainFormShortCutTips;

  showModeItemConfig.subitems:= [
    TCocoaToolBarUtils.toClass(showBriefItemConfig),
    TCocoaToolBarUtils.toClass(showFullItemConfig),
    TCocoaToolBarUtils.toClass(showThumbnailsItemConfig)
  ];

  mainFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(treeViewItemConfig),
    TCocoaToolBarUtils.toClass(horzSplitItemConfig),
    TCocoaToolBarUtils.toClass(swapPanelsItemConfig),
    TCocoaToolBarUtils.toClass(goBackwardItemConfig),
    TCocoaToolBarUtils.toClass(goForwardItemConfig),

    TCocoaToolBarUtils.toClass(showModeItemConfig),
    TCocoaToolBarUtils.toClass(shareItemConfig),
    TCocoaToolBarUtils.toClass(airdropItemConfig),
    TCocoaToolBarUtils.toClass(commandItemConfig),
    TCocoaToolBarUtils.toClass(quickLookItemConfig),
    TCocoaToolBarUtils.toClass(tagItemConfig),
    TCocoaToolBarUtils.toClass(goItemConfig),
    TCocoaToolBarUtils.toClass(finderRevealItemConfig),
    TCocoaToolBarUtils.toClass(finderInfoItemConfig),
    TCocoaToolBarUtils.toClass(terminalItemConfig),
    TCocoaToolBarUtils.toClass(searchItemConfig),
    TCocoaToolBarUtils.toClass(privilegeItemConfig),

    TCocoaToolBarUtils.toClass(refreshItemConfig),
    TCocoaToolBarUtils.toClass(compareItemConfig),
    TCocoaToolBarUtils.toClass(syncItemConfig),
    TCocoaToolBarUtils.toClass(editItemConfig),

    TCocoaToolBarUtils.toClass(iCloudItemConfig),
    TCocoaToolBarUtils.toClass(networkItemConfig)
  ];
end;

