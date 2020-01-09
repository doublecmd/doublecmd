{
   Double Commander
   -------------------------------------------------------------------------
   MultiRename dialog window

   Copyright (C) 2007-2020 Alexander Koblov (alexx2000@mail.ru)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.


   Original comment:
   ----------------------------
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : Pavel Letko (letcuv@centrum.cz)

   Advanced multi rename tool

   contributors:

   Copyright (C) 2007-2018 Alexander Koblov (alexx2000@mail.ru)
}

unit fMultiRename;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  LazUtf8, SysUtils, Classes, Graphics, Forms, StdCtrls, Menus, Controls,
  LCLType, StringHashList, Grids, ExtCtrls, Buttons, ActnList, EditBtn,

  //DC
  DCXmlConfig, uOSForms, uRegExprW, uFileProperty, uFormCommands,
  uFileSourceSetFilePropertyOperation, DCStringHashListUtf8, uClassesEx, uFile,
  uFileSource, DCClassesUtf8, uHotkeyManager;

const
  HotkeysCategoryMultiRename = 'MultiRename';

type
  { TMultiRenamePreset }
  TMultiRenamePreset = class(TObject)
  private
    FPresetName: string;
    FFileName: string;
    FExtension: string;
    FFileNameStyle: integer;
    FExtensionStyle: integer;
    FFind: string;
    FReplace: string;
    FRegExp: boolean;
    FUseSubs: boolean;
    FCounter: string;
    FInterval: string;
    FWidth: integer;
    FLog: boolean;
    FLogFile: string;
    FLogAppend: boolean;
  public
    property PresetName: string read FPresetName write FPresetName;
    property FileName: string read FFileName write FFileName;
    property Extension: string read FExtension write FExtension;
    property FileNameStyle: integer read FFileNameStyle write FFileNameStyle;
    property ExtensionStyle: integer read FExtensionStyle write FExtensionStyle;
    property Find: string read FFind write FFind;
    property Replace: string read FReplace write FReplace;
    property RegExp: boolean read FRegExp write FRegExp;
    property UseSubs: boolean read FUseSubs write FUseSubs;
    property Counter: string read FCounter write FCounter;
    property Interval: string read FInterval write FInterval;
    property Width: integer read FWidth write FWidth;
    property Log: boolean read FLog write FLog;
    property LogFile: string read FLogFile write FLogFile;
    property LogAppend: boolean read FLogAppend write FLogAppend;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMultiRenamePresetList }
  TMultiRenamePresetList = class(TList)
  private
    function GetMultiRenamePreset(Index: integer): TMultiRenamePreset;
  public
    property MultiRenamePreset[Index: integer]: TMultiRenamePreset read GetMultiRenamePreset;
    procedure Delete(Index: integer);
    procedure Clear; override;
    function Find(sPresetName: string): integer;
  end;

  { tTargetForMask }
  //Used to indicate of a mask is used for the "Filename" or the "Extension".
  tTargetForMask = (tfmFilename, tfmExtension);

  { tRenameMaskToUse }
  //Used as a parameter type to indicate the kind of field the mask is related to.
  tRenameMaskToUse = (rmtuFilename, rmtuExtension, rmtuCounter, rmtuDate, rmtuTime, rmtuPlugins);

  { tSourceOfInformation }
  tSourceOfInformation = (soiFilename, soiExtension, soiCounter, soiGUID, soiVariable, soiDate, soiTime, soiPlugins, soiFullName, soiPath);

  { tMenuActionStyle }
  //Used to help to group common or similar action done for each mask.
  tMenuActionStyle = (masStraight, masXCharacters, masXYCharacters, masAskVariable, masDirectorySelector);

  { TfrmMultiRename }
  TfrmMultiRename = class(TAloneForm, IFormCommands)
    StringGrid: TStringGrid;
    pnlOptions: TPanel;
    pnlOptionsLeft: TPanel;
    gbMaska: TGroupBox;
    lbName: TLabel;
    edName: TEdit;
    btnAnyNameMask: TBitBtn;
    cbNameMaskStyle: TComboBox;
    lbExt: TLabel;
    edExt: TEdit;
    btnAnyExtMask: TBitBtn;
    cmbExtensionStyle: TComboBox;
    gbPresets: TGroupBox;
    cbPresets: TComboBox;
    btnPresets: TBitBtn;
    spltMainSplitter: TSplitter;
    pnlOptionsRight: TPanel;
    gbFindReplace: TGroupBox;
    lbFind: TLabel;
    edFind: TEdit;
    lbReplace: TLabel;
    edReplace: TEdit;
    cbRegExp: TCheckBox;
    cbUseSubs: TCheckBox;
    gbCounter: TGroupBox;
    lbStNb: TLabel;
    edPoc: TEdit;
    lbInterval: TLabel;
    edInterval: TEdit;
    lbWidth: TLabel;
    cmbxWidth: TComboBox;
    btnRestore: TBitBtn;
    btnRename: TBitBtn;
    btnConfig: TBitBtn;
    btnEditor: TBitBtn;
    btnClose: TBitBtn;
    cbLog: TCheckBox;
    cbLogAppend: TCheckBox;
    fneRenameLogFileFilename: TFileNameEdit;
    btnRelativeRenameLogFile: TSpeedButton;
    btnViewRenameLogFile: TSpeedButton;
    mmMainMenu: TMainMenu;
    miActions: TMenuItem;
    miResetAll: TMenuItem;
    miEditor: TMenuItem;
    miLoadNamesFromFile: TMenuItem;
    miEditNames: TMenuItem;
    miEditNewNames: TMenuItem;
    miSeparator1: TMenuItem;
    miConfiguration: TMenuItem;
    miSeparator2: TMenuItem;
    miRename: TMenuItem;
    miClose: TMenuItem;
    pmPresets: TPopupMenu;
    pmFloatingMainMaskMenu: TPopupMenu;
    pmDynamicMasks: TPopupMenu;
    pmEditDirect: TPopupMenu;
    mnuLoadFromFile: TMenuItem;
    mnuEditNames: TMenuItem;
    mnuEditNewNames: TMenuItem;
    pmPathToBeRelativeToHelper: TPopupMenu;
    actList: TActionList;
    actResetAll: TAction;
    actInvokeEditor: TAction;
    actLoadNamesFromFile: TAction;
    actEditNames: TAction;
    actEditNewNames: TAction;
    actConfig: TAction;
    actRename: TAction;
    actClose: TAction;
    actShowPresetsMenu: TAction;
    actDropDownPresetList: TAction;
    actLoadLastPreset: TAction;
    actLoadPreset: TAction;
    actLoadPreset1: TAction;
    actLoadPreset2: TAction;
    actLoadPreset3: TAction;
    actLoadPreset4: TAction;
    actLoadPreset5: TAction;
    actLoadPreset6: TAction;
    actLoadPreset7: TAction;
    actLoadPreset8: TAction;
    actLoadPreset9: TAction;
    actSavePreset: TAction;
    actSavePresetAs: TAction;
    actRenamePreset: TAction;
    actDeletePreset: TAction;
    actSortPresets: TAction;
    actAnyNameMask: TAction;
    actNameNameMask: TAction;
    actExtNameMask: TAction;
    actDateNameMask: TAction;
    actTimeNameMask: TAction;
    actCtrNameMask: TAction;
    actPlgnNameMask: TAction;
    actClearNameMask: TAction;
    actAnyExtMask: TAction;
    actNameExtMask: TAction;
    actExtExtMask: TAction;
    actDateExtMask: TAction;
    actTimeExtMask: TAction;
    actCtrExtMask: TAction;
    actPlgnExtMask: TAction;
    actClearExtMask: TAction;
    actInvokeRelativePath: TAction;
    actViewRenameLogFile: TAction;
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormCloseQuery({%H-}Sender: TObject; var CanClose: boolean);
    procedure FormClose({%H-}Sender: TObject; var CloseAction: TCloseAction);
    procedure StringGridKeyDown({%H-}Sender: TObject; var Key: word; Shift: TShiftState);
    procedure StringGridMouseDown({%H-}Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: integer);
    procedure StringGridMouseUp({%H-}Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure StringGridSelection({%H-}Sender: TObject; {%H-}aCol, aRow: integer);
    procedure StringGridTopLeftChanged({%H-}Sender: TObject);
    procedure cbNameStyleChange({%H-}Sender: TObject);
    procedure cbPresetsChange({%H-}Sender: TObject);
    procedure cbPresetsCloseUp({%H-}Sender: TObject);
    procedure edFindChange({%H-}Sender: TObject);
    procedure edReplaceChange({%H-}Sender: TObject);
    procedure cbRegExpChange({%H-}Sender: TObject);
    procedure edPocChange({%H-}Sender: TObject);
    procedure edIntervalChange({%H-}Sender: TObject);
    procedure cbLogClick({%H-}Sender: TObject);
    procedure actExecute(Sender: TObject);
    procedure actInvokeRelativePathExecute(Sender: TObject);
  private
    IniPropStorage: TIniPropStorageEx;
    FCommands: TFormCommands;
    FActuallyRenamingFile: boolean;
    FSourceRow: integer;
    FMoveRow: boolean;
    FFileSource: IFileSource;
    FFiles: TFiles;
    FNewNames: TStringHashListUtf8;
    FOldNames: TStringHashListUtf8;
    FNames: TStringList;
    FslVariableNames, FslVariableValues, FslVariableSuggestionName, FslVariableSuggestionValue: TStringList;
    FRegExp: TRegExprW;
    FFindText: TStringList;
    FReplaceText: TStringList;
    FPluginDispatcher: tTargetForMask;
    FMultiRenamePresetList: TMultiRenamePresetList;
    FParamPresetToLoadOnStart: string;
    FLastPreset: string;
    FbRememberLog, FbRememberAppend: boolean;
    FsRememberRenameLogFilename: string;
    FLog: TStringListEx;
    property Commands: TFormCommands read FCommands implements IFormCommands;
    procedure RestoreProperties(Sender: TObject);
    procedure SetConfigurationState(bConfigurationSaved: boolean);
    function GetPresetNameForCommand(const Params: array of string): string;
    procedure LoadPresetsXml(AConfig: TXmlConfig);
    function isOkToLosePresetModification: boolean;
    procedure SavePreset(PresetName: string);
    procedure SavePresetsXml(AConfig: TXmlConfig);
    procedure SavePresets;
    procedure DeletePreset(PresetName: string);
    procedure FillPresetsList(const WantedSelectedPresetName: string = '');
    procedure RefreshActivePresetCommands;
    procedure InitializeMaskHelper;
    procedure PopulateMainMenu;
    procedure PopulateFilenameMenu(AMenuSomething: TComponent);
    procedure PopulateExtensionMenu(AMenuSomething: TComponent);
    procedure BuildMaskMenu(AMenuSomething: TComponent; iTarget: tTargetForMask; iMenuTypeMask: tRenameMaskToUse);
    procedure BuildPresetsMenu(AMenuSomething: TComponent);
    procedure BuildMenuAndPopup(iTarget: tTargetForMask; iMenuTypeMask: tRenameMaskToUse);
    function GetMaskCategoryName(aRenameMaskToUse: tRenameMaskToUse): string;
    function GetImageIndexCategoryName(aRenameMaskToUse: tRenameMaskToUse): integer;
    function GetCategoryAction(TargetForMask: tTargetForMask; aRenameMask: tRenameMaskToUse): TAction;
    function AppendSubMenuToThisMenu(ATargetMenu: TMenuItem; sCaption: string; iImageIndex: integer): TMenuItem;
    function AppendActionMenuToThisMenu(ATargetMenu: TMenuItem; paramAction: TAction): TMenuItem;
    procedure MenuItemXCharactersMaskClick(Sender: TObject);
    procedure MenuItemVariableMaskClick(Sender: TObject);
    procedure MenuItemStraightMaskClick(Sender: TObject);
    procedure MenuItemDirectorySelectorMaskClick(Sender: TObject);
    procedure PopupDynamicMenuAtThisControl(APopUpMenu: TPopupMenu; AControl: TControl);
    procedure miPluginClick(Sender: TObject);
    procedure InsertMask(const Mask: string; edChoose: TEdit);
    procedure InsertMask(const Mask: string; TargetForMask: tTargetForMask);
    function sReplace(sMask: string; ItemNr: integer): string;
    function sReplaceXX(const sFormatStr, sOrig: string): string;
    function sReplaceVariable(const sFormatStr: string): string;
    function sReplaceBadChars(const sPath: string): string;
    function IsLetter(AChar: AnsiChar): boolean;
    function ApplyStyle(InputString: string; Style: integer): string;
    function FirstCharToUppercaseUTF8(InputString: string): string;
    function FirstCharOfFirstWordToUppercaseUTF8(InputString: string): string;
    function FirstCharOfEveryWordToUppercaseUTF8(InputString: string): string;
    procedure LoadNamesFromFile(const AFileName: string);
    function FreshText(ItemIndex: integer): string;
    function sHandleFormatString(const sFormatStr: string; ItemNr: integer): string;
    procedure SetFilePropertyResult(Index: integer; aFile: TFile; aTemplate: TFileProperty; Result: TSetFilePropertyResult);
    procedure SetOutputGlobalRenameLogFilename;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override; //Not used for actual renaming file. Present there just for the "TfrmOptionsHotkeys.FillCommandList" function who need to create the form in memory to extract internal commands from it.
    constructor Create(TheOwner: TComponent; aFileSource: IFileSource; var aFiles: TFiles; const paramPreset: string); reintroduce;
    destructor Destroy; override;
  published
    procedure cm_ResetAll(const Params: array of string);
    procedure cm_InvokeEditor(const {%H-}Params: array of string);
    procedure cm_LoadNamesFromFile(const {%H-}Params: array of string);
    procedure cm_EditNames(const {%H-}Params: array of string);
    procedure cm_EditNewNames(const {%H-}Params: array of string);
    procedure cm_Config(const {%H-}Params: array of string);
    procedure cm_Rename(const {%H-}Params: array of string);
    procedure cm_Close(const {%H-}Params: array of string);
    procedure cm_ShowPresetsMenu(const {%H-}Params: array of string);
    procedure cm_DropDownPresetList(const {%H-}Params: array of string);
    procedure cm_LoadPreset(const Params: array of string);
    procedure cm_LoadLastPreset(const {%H-}Params: array of string);
    procedure cm_LoadPreset1(const {%H-}Params: array of string);
    procedure cm_LoadPreset2(const {%H-}Params: array of string);
    procedure cm_LoadPreset3(const {%H-}Params: array of string);
    procedure cm_LoadPreset4(const {%H-}Params: array of string);
    procedure cm_LoadPreset5(const {%H-}Params: array of string);
    procedure cm_LoadPreset6(const {%H-}Params: array of string);
    procedure cm_LoadPreset7(const {%H-}Params: array of string);
    procedure cm_LoadPreset8(const {%H-}Params: array of string);
    procedure cm_LoadPreset9(const {%H-}Params: array of string);
    procedure cm_SavePreset(const Params: array of string);
    procedure cm_SavePresetAs(const Params: array of string);
    procedure cm_RenamePreset(const Params: array of string);
    procedure cm_DeletePreset(const Params: array of string);
    procedure cm_SortPresets(const Params: array of string);
    procedure cm_AnyNameMask(const {%H-}Params: array of string);
    procedure cm_NameNameMask(const {%H-}Params: array of string);
    procedure cm_ExtNameMask(const {%H-}Params: array of string);
    procedure cm_CtrNameMask(const {%H-}Params: array of string);
    procedure cm_DateNameMask(const {%H-}Params: array of string);
    procedure cm_TimeNameMask(const {%H-}Params: array of string);
    procedure cm_PlgnNameMask(const {%H-}Params: array of string);
    procedure cm_ClearNameMask(const {%H-}Params: array of string);
    procedure cm_AnyExtMask(const {%H-}Params: array of string);
    procedure cm_NameExtMask(const {%H-}Params: array of string);
    procedure cm_ExtExtMask(const {%H-}Params: array of string);
    procedure cm_CtrExtMask(const {%H-}Params: array of string);
    procedure cm_DateExtMask(const {%H-}Params: array of string);
    procedure cm_TimeExtMask(const {%H-}Params: array of string);
    procedure cm_PlgnExtMask(const {%H-}Params: array of string);
    procedure cm_ClearExtMask(const {%H-}Params: array of string);
    procedure cm_ViewRenameLogFile(const {%H-}Params: array of string);
  end;

{initialization function}
function ShowMultiRenameForm(aFileSource: IFileSource; var aFiles: TFiles; const PresetToLoad: string = ''): boolean;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Dialogs, Math,

  //DC
  fMain, uFileSourceOperation, uOperationsManager, uOSUtils, uDCUtils, uDebug,
  DCOSUtils, DCStrUtils, uLng, uGlobs, uSpecialDir, uFileProcs, uShowForm,
  fSelectTextRange, fSelectPathRange, uShowMsg, uFileFunctions, dmCommonData,
  fMultiRenameWait, fSortAnything;

type
  tMaskHelper = record
    sMenuItem: string;
    sKeyword: string;
    MenuActionStyle: tMenuActionStyle;
    iMenuType: tRenameMaskToUse;
    iSourceOfInformation: tSourceOfInformation;
  end;

const
  sPresetsSection = 'MultiRenamePresets';
  sLASTPRESET = '{BC322BF1-2185-47F6-9F99-D27ED1E23E53}';
  sFRESHMASKS = '{40422152-9D05-469E-9B81-791AF8C369D8}';
  iTARGETMASK = $00000001;

  sREFRESHCOMMANDS = 'refreshcommands';
  sDEFAULTLOGFILENAME = 'default.log';

  CONFIG_NOTSAVED = False;
  CONFIG_SAVED = True;

  NBMAXHELPERS = 30;

var
  //Sequence of operation to add a new mask:
  // 1. Add its entry below in the "MaskHelpers" array.
  // 2. Go immediately set its translatable string for the user in the function "InitializeMaskHelper" and the text in unit "uLng".
  // 3. When editing "InitializeMaskHelper", make sure to update the TWO columns of indexes.
  // 4. In the procedure "BuildMaskMenu", there is good chance you need to associated to the "AMenuItem.OnClick" the correct function based on "MaskHelpers[iSeekIndex].MenuActionStyle".
  // 5. If it's a NEW procedure, you'll need to write it. You may check "MenuItemXCharactersMaskClick" for inspiration.
  // 6. There is good chance you need to edit "sHandleFormatString" to add your new mask and action to do with it.

  MaskHelpers: array[0..pred(NBMAXHELPERS)] of tMaskHelper =
    (
    (sMenuItem: ''; sKeyword: '[N]'; MenuActionStyle: masStraight; iMenuType: rmtuFilename; iSourceOfInformation: soiFilename),
    (sMenuItem: ''; sKeyword: '[Nx]'; MenuActionStyle: masXCharacters; iMenuType: rmtuFilename; iSourceOfInformation: soiFilename),
    (sMenuItem: ''; sKeyword: '[Nx:y]'; MenuActionStyle: masXYCharacters; iMenuType: rmtuFilename; iSourceOfInformation: soiFilename),
    (sMenuItem: ''; sKeyword: '[A]'; MenuActionStyle: masStraight; iMenuType: rmtuFilename; iSourceOfInformation: soiFullName),
    (sMenuItem: ''; sKeyword: '[Ax:y]'; MenuActionStyle: masXYCharacters; iMenuType: rmtuFilename; iSourceOfInformation: soiFullName),
    (sMenuItem: ''; sKeyword: '[P]'; MenuActionStyle: masDirectorySelector; iMenuType: rmtuFilename; iSourceOfInformation: soiPath),
    (sMenuItem: ''; sKeyword: '[E]'; MenuActionStyle: masStraight; iMenuType: rmtuExtension; iSourceOfInformation: soiExtension),
    (sMenuItem: ''; sKeyword: '[Ex]'; MenuActionStyle: masXCharacters; iMenuType: rmtuExtension; iSourceOfInformation: soiExtension),
    (sMenuItem: ''; sKeyword: '[Ex:y]'; MenuActionStyle: masXYCharacters; iMenuType: rmtuExtension; iSourceOfInformation: soiExtension),
    (sMenuItem: ''; sKeyword: '[C]'; MenuActionStyle: masStraight; iMenuType: rmtuCounter; iSourceOfInformation: soiCounter),
    (sMenuItem: ''; sKeyword: '[G]'; MenuActionStyle: masStraight; iMenuType: rmtuCounter; iSourceOfInformation: soiGUID),
    (sMenuItem: ''; sKeyword: '[V:x]'; MenuActionStyle: masAskVariable; iMenuType: rmtuCounter; iSourceOfInformation: soiVariable),
    (sMenuItem: ''; sKeyword: '[Y]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[YYYY]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[M]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[MM]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[MMM]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[MMMM]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[D]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[DD]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[DDD]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[DDDD]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[YYYY]-[MM]-[DD]'; MenuActionStyle: masStraight; iMenuType: rmtuDate; iSourceOfInformation: soiDate),
    (sMenuItem: ''; sKeyword: '[h]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime),
    (sMenuItem: ''; sKeyword: '[hh]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime),
    (sMenuItem: ''; sKeyword: '[n]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime),
    (sMenuItem: ''; sKeyword: '[nn]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime),
    (sMenuItem: ''; sKeyword: '[s]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime),
    (sMenuItem: ''; sKeyword: '[ss]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime),
    (sMenuItem: ''; sKeyword: '[hh]-[nn]-[ss]'; MenuActionStyle: masStraight; iMenuType: rmtuTime; iSourceOfInformation: soiTime)
    );

{ TMultiRenamePreset.Create }
constructor TMultiRenamePreset.Create;
begin
  FPresetName := '';
  FFileName := '[N]';
  FExtension := '[E]';
  FFileNameStyle := 0;
  FExtensionStyle := 0;
  FFind := '';
  FReplace := '';
  FRegExp := False;
  FUseSubs := False;
  FCounter := '1';
  FInterval := '1';
  FWidth := 0;
  FLog := False;
  FLogFile := '';
  FLogAppend := False;
end;

{ TMultiRenamePreset.Destory }
// Not so necessary, but useful with a breakpoint to validate object is really free from memory when deleting an element from the list of clearing that list.
destructor TMultiRenamePreset.Destroy;
begin
  inherited Destroy;
end;

{ TMultiRenamePresetList.GetMultiRenamePreset }
function TMultiRenamePresetList.GetMultiRenamePreset(Index: integer): TMultiRenamePreset;
begin
  Result := TMultiRenamePreset(Items[Index]);
end;

{ TMultiRenamePresetList.Delete }
procedure TMultiRenamePresetList.Delete(Index: integer);
begin
  TMultiRenamePreset(Items[Index]).Free;
  inherited Delete(Index);
end;

{ TMultiRenamePresetList.Clear }
procedure TMultiRenamePresetList.Clear;
var
  Index: integer;
begin
  for Index := pred(Count) downto 0 do
    TMultiRenamePreset(Items[Index]).Free;
  inherited Clear;
end;

{ TMultiRenamePresetList.Find }
function TMultiRenamePresetList.Find(sPresetName: string): integer;
var
  iSeeker: integer = 0;
begin
  Result := -1;
  while (Result = -1) and (iSeeker < Count) do
    if SameText(sPresetName, MultiRenamePreset[iSeeker].PresetName) then
      Result := iSeeker
    else
      Inc(iSeeker);
end;

{ TfrmMultiRename.Create }
//Not used for actual renaming file.
//Present there just for the "TfrmOptionsHotkeys.FillCommandList" function who need to create the form in memory to extract internal commands from it.
constructor TfrmMultiRename.Create(TheOwner: TComponent);
var
  FDummyFiles: TFiles;
begin
  FDummyFiles := TFiles.Create(''); //Will be self destroyed by the "TfrmMultiRename" object itself.
  Create(TheOwner, nil, FDummyFiles, '');
end;

{ TfrmMultiRename.Create }
constructor TfrmMultiRename.Create(TheOwner: TComponent; aFileSource: IFileSource; var aFiles: TFiles; const paramPreset: string);
begin
  FActuallyRenamingFile := False;
  FRegExp := TRegExprW.Create;
  FNames := TStringList.Create;
  FFindText := TStringList.Create;
  FFindText.StrictDelimiter := True;
  FFindText.Delimiter := '|';
  FReplaceText := TStringList.Create;
  FReplaceText.StrictDelimiter := True;
  FReplaceText.Delimiter := '|';
  FMultiRenamePresetList := TMultiRenamePresetList.Create;
  FNewNames := TStringHashListUtf8.Create(FileNameCaseSensitive);
  FOldNames := TStringHashListUtf8.Create(FileNameCaseSensitive);
  FslVariableNames := TStringList.Create;
  FslVariableValues := TStringList.Create;
  FslVariableSuggestionName := TStringList.Create;
  FslVariableSuggestionValue := TStringList.Create;
  FFileSource := aFileSource;
  FFiles := aFiles;
  aFiles := nil;
  FSourceRow := -1;
  FMoveRow := False;
  FParamPresetToLoadOnStart := paramPreset;
  inherited Create(TheOwner);

  FCommands := TFormCommands.Create(Self, actList);
end;

{ TfrmMultiRename.Destroy }
destructor TfrmMultiRename.Destroy;
begin
  inherited Destroy;
  FMultiRenamePresetList.Clear;
  FreeAndNil(FMultiRenamePresetList);
  FreeAndNil(FNewNames);
  FreeAndNil(FOldNames);
  FreeAndNil(FslVariableNames);
  FreeAndNil(FslVariableValues);
  FreeAndNil(FslVariableSuggestionName);
  FreeAndNil(FslVariableSuggestionValue);
  FreeAndNil(FFiles);
  FreeAndNil(FNames);
  FreeAndNil(FRegExp);
  FreeAndNil(FFindText);
  FreeAndNil(FReplaceText);
end;

{ TfrmMultiRename.FormCreate }
procedure TfrmMultiRename.FormCreate({%H-}Sender: TObject);
var
  HMMultiRename: THMForm;
begin
  // Localize File name style ComboBox
  ParseLineToList(rsMulRenFileNameStyleList, cbNameMaskStyle.Items);
  ParseLineToList(rsMulRenFileNameStyleList, cmbExtensionStyle.Items);
  InitializeMaskHelper;

  // Set row count
  StringGrid.RowCount := FFiles.Count + 1;
  StringGrid.FocusRectVisible := False;

  // Initialize property storage
  IniPropStorage := InitPropStorage(Self);
  IniPropStorage.OnRestoreProperties := @RestoreProperties;
  IniPropStorage.StoredValues.Add.DisplayName := 'lsvwFile_Columns.Item0_Width';
  IniPropStorage.StoredValues.Add.DisplayName := 'lsvwFile_Columns.Item1_Width';
  IniPropStorage.StoredValues.Add.DisplayName := 'lsvwFile_Columns.Item2_Width';

  if gMulRenShowMenuBarOnTop then
    Menu := mmMainMenu
  else
    Menu := nil;

  if not gIconsInMenus then
  begin
    mmMainMenu.Images := nil;
    pmDynamicMasks.Images := nil;
    pmEditDirect.Images := nil;
    pmPresets.Images := nil;
  end;

  HMMultiRename := HotMan.Register(Self, HotkeysCategoryMultiRename);
  HMMultiRename.RegisterActionList(actList);

  // Set default values for controls.
  cm_ResetAll([sREFRESHCOMMANDS + '=0']);

  // Initialize presets.
  LoadPresetsXml(gConfig);

  if (FParamPresetToLoadOnStart <> '') and (FMultiRenamePresetList.Find(FParamPresetToLoadOnStart) <> -1) then
  begin
    FillPresetsList(FParamPresetToLoadOnStart);
  end
  else
  begin
    case gMulRenLaunchBehavior of
      mrlbLastMaskUnderLastOne: FillPresetsList(sLASTPRESET);
      mrlbLastPreset: FillPresetsList(FLastPreset);
      mrlbFreshNew: FillPresetsList(sFRESHMASKS);
    end;
  end;

  PopulateMainMenu;
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathToBeRelativeToHelper, mp_PATHHELPER, nil);
  FPluginDispatcher := tfmFilename;
end;

{ TfrmMultiRename.FormCloseQuery }
procedure TfrmMultiRename.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if not isOkToLosePresetModification then
    CanClose := False;
end;

{ TfrmMultiRename.FormClose }
procedure TfrmMultiRename.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SavePreset(sLASTPRESET);

  CloseAction := caFree;
  with StringGrid.Columns do
  begin
    IniPropStorage.StoredValue['lsvwFile_Columns.Item0_Width'] := IntToStr(Items[0].Width);
    IniPropStorage.StoredValue['lsvwFile_Columns.Item1_Width'] := IntToStr(Items[1].Width);
    IniPropStorage.StoredValue['lsvwFile_Columns.Item2_Width'] := IntToStr(Items[2].Width);
  end;
end;

{ TfrmMultiRename.StringGridKeyDown }
procedure TfrmMultiRename.StringGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  tmpFile: TFile;
  DestRow: integer;
begin
  DestRow := StringGrid.Row;

  if (Shift = [ssShift]) then
  begin
    case Key of
      VK_UP:
      begin
        DestRow := StringGrid.Row - 1;
      end;
      VK_DOWN:
      begin
        DestRow := StringGrid.Row + 1;
      end;
    end;

    if (DestRow <> StringGrid.Row) and (0 < DestRow) and (DestRow < StringGrid.RowCount) then
    begin
      tmpFile := FFiles.Items[DestRow - 1];
      FFiles.Items[DestRow - 1] := FFiles.Items[StringGrid.Row - 1];
      FFiles.Items[StringGrid.Row - 1] := tmpFile;

      StringGridTopLeftChanged(StringGrid);
    end;
  end;
end;

{ TfrmMultiRename.StringGridMouseDown }
procedure TfrmMultiRename.StringGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  SourceCol: integer = 0;
begin
  if (Button = mbLeft) then
  begin
    StringGrid.MouseToCell(X, Y, SourceCol, FSourceRow);
    if (FSourceRow > 0) then
    begin
      FMoveRow := True;
    end;
  end;
end;

{ TfrmMultiRename.StringGridMouseUp }
procedure TfrmMultiRename.StringGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FMoveRow := False;
  end;
end;

{ TfrmMultiRename.StringGridSelection }
procedure TfrmMultiRename.StringGridSelection(Sender: TObject; aCol, aRow: integer);
var
  tmpFile: TFile;
begin
  if FMoveRow and (aRow <> FSourceRow) then
  begin
    tmpFile := FFiles.Items[aRow - 1];
    FFiles.Items[aRow - 1] := FFiles.Items[FSourceRow - 1];
    FFiles.Items[FSourceRow - 1] := tmpFile;

    FSourceRow := aRow;
    StringGridTopLeftChanged(StringGrid);
  end;
end;

{ TfrmMultiRename.StringGridTopLeftChanged }
procedure TfrmMultiRename.StringGridTopLeftChanged(Sender: TObject);
var
  I, iRowCount: integer;
begin
  iRowCount := StringGrid.TopRow + StringGrid.VisibleRowCount;
  if iRowCount > FFiles.Count then
    iRowCount := FFiles.Count;
  for I := StringGrid.TopRow to iRowCount do
  begin
    StringGrid.Cells[0, I] := FFiles[I - 1].Name;
    StringGrid.Cells[1, I] := FreshText(I - 1);
    StringGrid.Cells[2, I] := FFiles[I - 1].Path;
  end;
end;

{ TfrmMultiRename.cbNameStyleChange }
procedure TfrmMultiRename.cbNameStyleChange(Sender: TObject);
begin
  StringGridTopLeftChanged(StringGrid);
  if ActiveControl <> cbPresets then
    SetConfigurationState(CONFIG_NOTSAVED);
end;

{ TfrmMultiRename.cbPresetsChange }
procedure TfrmMultiRename.cbPresetsChange(Sender: TObject);
begin
  if cbPresets.ItemIndex <> 0 then
    cm_LoadPreset(['name=' + cbPresets.Items.Strings[cbPresets.ItemIndex]])
  else
    cm_LoadPreset(['name=' + sLASTPRESET]);
  RefreshActivePresetCommands;
end;

{ TfrmMultiRename.cbPresetsCloseUp }
procedure TfrmMultiRename.cbPresetsCloseUp(Sender: TObject);
begin
  if edName.Enabled and gbMaska.Enabled then ActiveControl := edName;
  edName.SelStart := UTF8Length(edName.Text);
end;

{ TfrmMultiRename.edFindChange }
procedure TfrmMultiRename.edFindChange(Sender: TObject);
begin
  if cbRegExp.Checked then
    FRegExp.Expression := UTF8Decode(edFind.Text)
  else
  begin
    FFindText.DelimitedText := edFind.Text;
  end;
  SetConfigurationState(CONFIG_NOTSAVED);
  StringGridTopLeftChanged(StringGrid);
end;

{ TfrmMultiRename.edReplaceChange }
procedure TfrmMultiRename.edReplaceChange(Sender: TObject);
begin
  if not cbRegExp.Checked then
  begin
    FReplaceText.DelimitedText := edReplace.Text;
  end;
  SetConfigurationState(CONFIG_NOTSAVED);
  StringGridTopLeftChanged(StringGrid);
end;

{ TfrmMultiRename.cbRegExpChange }
procedure TfrmMultiRename.cbRegExpChange(Sender: TObject);
begin
  if cbRegExp.Checked then
    cbUseSubs.Checked := boolean(cbUseSubs.Tag)
  else
  begin
    cbUseSubs.Tag := integer(cbUseSubs.Checked);
    cbUseSubs.Checked := False;
  end;
  cbUseSubs.Enabled := cbRegExp.Checked;
  edFindChange(edFind);
end;

{ TfrmMultiRename.edPocChange }
procedure TfrmMultiRename.edPocChange(Sender: TObject);
var
  c: integer;
begin
  c := StrToIntDef(edPoc.Text, maxint);
  if c = MaxInt then
    with edPoc do //editbox only for numbers
    begin
      Text := '1';
      SelectAll;
    end;
  SetConfigurationState(CONFIG_NOTSAVED);
  StringGridTopLeftChanged(StringGrid);
end;

{ TfrmMultiRename.edIntervalChange }
procedure TfrmMultiRename.edIntervalChange(Sender: TObject);
var
  c: integer;
begin
  c := StrToIntDef(edInterval.Text, maxint);
  if c = MaxInt then
    with edInterval do //editbox only for numbers
    begin
      Text := '1';
      SelectAll;
    end;
  SetConfigurationState(CONFIG_NOTSAVED);
  StringGridTopLeftChanged(StringGrid);
end;

{ TfrmMultiRename.cbLogClick }
procedure TfrmMultiRename.cbLogClick(Sender: TObject);
begin
  fneRenameLogFileFilename.Enabled := cbLog.Checked;
  actInvokeRelativePath.Enabled := cbLog.Checked;
  actViewRenameLogFile.Enabled := cbLog.Checked;
  cbLogAppend.Enabled := cbLog.Checked;
  SetConfigurationState(CONFIG_NOTSAVED);
end;

{ TfrmMultiRename.actExecute }
procedure TfrmMultiRename.actExecute(Sender: TObject);
var
  cmd: string;
begin
  cmd := (Sender as TAction).Name;
  cmd := 'cm_' + Copy(cmd, 4, Length(cmd) - 3);
  Commands.ExecuteCommand(cmd, []);
end;

{ TfrmMultiRename.actInvokeRelativePathExecute }
procedure TfrmMultiRename.actInvokeRelativePathExecute(Sender: TObject);
begin
  fneRenameLogFileFilename.SetFocus;
  gSpecialDirList.SetSpecialDirRecipientAndItsType(fneRenameLogFileFilename, pfFILE);
  pmPathToBeRelativeToHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

{ TfrmMultiRename.RestoreProperties }
procedure TfrmMultiRename.RestoreProperties(Sender: TObject);
begin
  with StringGrid.Columns do
  begin
    Items[0].Width := StrToIntDef(IniPropStorage.StoredValue['lsvwFile_Columns.Item0_Width'], Items[0].Width);
    Items[1].Width := StrToIntDef(IniPropStorage.StoredValue['lsvwFile_Columns.Item1_Width'], Items[1].Width);
    Items[2].Width := StrToIntDef(IniPropStorage.StoredValue['lsvwFile_Columns.Item2_Width'], Items[2].Width);
  end;
end;

{ TfrmMultiRename.SetConfigurationState }
procedure TfrmMultiRename.SetConfigurationState(bConfigurationSaved: boolean);
begin
  if not cbPresets.DroppedDown then
  begin
    if bConfigurationSaved or (cbPresets.ItemIndex <> 0) then
    begin
      if cbPresets.Enabled <> bConfigurationSaved then
      begin
        cbPresets.Enabled := bConfigurationSaved;
      end;
    end;
  end;
end;

{ TfrmMultiRename.GetPresetNameForCommand }
// Wanted preset may be given via "name=presetname" or via "index=indexno".
function TfrmMultiRename.GetPresetNameForCommand(const Params: array of string): string;
var
  Param, sValue: string;
  iIndex: integer;
begin
  Result := '';

  for Param in Params do
  begin
    if GetParamValue(Param, 'name', sValue) then
      Result := sValue
    else
    if GetParamValue(Param, 'index', sValue) then
    begin
      iIndex := StrToIntDef(sValue, -1);
      if (iIndex >= 0) and (iIndex < cbPresets.items.Count) then
        if iIndex = 0 then
          Result := sLASTPRESET
        else
          Result := cbPresets.Items.Strings[iIndex];
    end;
  end;
end;

{ TfrmMultiRename.LoadPresetsXml }
procedure TfrmMultiRename.LoadPresetsXml(AConfig: TXmlConfig);
var
  PresetName: string;
  AMultiRenamePreset: TMultiRenamePreset;
  ANode: TXmlNode;
  PresetIndex: integer;
begin
  FMultiRenamePresetList.Clear;

  ANode := AConfig.FindNode(AConfig.RootNode, sPresetsSection);
  FLastPreset := AConfig.GetValue(ANode, 'LastPreset', sLASTPRESET);

  ANode := AConfig.FindNode(ANode, 'Presets');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('Preset') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Name', PresetName) then
        begin
          if FMultiRenamePresetList.Find(PresetName) = -1 then //Make sure we don't load preset with the same name.
          begin
            AMultiRenamePreset := TMultiRenamePreset.Create;
            AMultiRenamePreset.PresetName := PresetName;
            FMultiRenamePresetList.Add(AMultiRenamePreset);

            AMultiRenamePreset.FileName := AConfig.GetValue(ANode, 'Filename', '[N]');
            AMultiRenamePreset.Extension := AConfig.GetValue(ANode, 'Extension', '[E]');
            AMultiRenamePreset.FileNameStyle := AConfig.GetValue(ANode, 'FilenameStyle', 0);
            AMultiRenamePreset.ExtensionStyle := AConfig.GetValue(ANode, 'ExtensionStyle', 0);
            AMultiRenamePreset.Find := AConfig.GetValue(ANode, 'Find', '');
            AMultiRenamePreset.Replace := AConfig.GetValue(ANode, 'Replace', '');
            AMultiRenamePreset.RegExp := AConfig.GetValue(ANode, 'RegExp', False);
            AMultiRenamePreset.UseSubs := AConfig.GetValue(ANode, 'UseSubs', False);
            AMultiRenamePreset.Counter := AConfig.GetValue(ANode, 'Counter', '1');
            AMultiRenamePreset.Interval := AConfig.GetValue(ANode, 'Interval', '1');
            AMultiRenamePreset.Width := AConfig.GetValue(ANode, 'Width', 0);
            AMultiRenamePreset.Log := AConfig.GetValue(ANode, 'Log/Enabled', False);
            AMultiRenamePreset.LogAppend := AConfig.GetValue(ANode, 'Log/Append', False);
            AMultiRenamePreset.LogFile := AConfig.GetValue(ANode, 'Log/File', '');
          end;
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;

  //Make sure the "sLASTPRESET" is at position 0.
  PresetIndex := FMultiRenamePresetList.Find(sLASTPRESET);
  if PresetIndex <> 0 then
  begin
    if PresetIndex <> -1 then
    begin
      //If it's present but not at zero, move it to 0.
      FMultiRenamePresetList.Move(PresetIndex, 0);
    end
    else
    begin
      AMultiRenamePreset := TMultiRenamePreset.Create;
      AMultiRenamePreset.PresetName := sLASTPRESET;
      FMultiRenamePresetList.Insert(0, AMultiRenamePreset);
    end;
  end;
end;

{ TfrmMultiRename.isOkToLosePresetModification }
function TfrmMultiRename.isOkToLosePresetModification: boolean;
var
  MyMsgResult: TMyMsgResult;
begin
  Result := False;

  if (cbPresets.ItemIndex <= 0) or (cbPresets.Enabled) or (not Visible) then
    Result := True
  else
  begin
    case gMulRenExitModifiedPreset of
      mrempIgnoreSaveLast:
      begin
        Result := True;
      end;

      mrempSaveAutomatically:
      begin
        if cbPresets.ItemIndex > 0 then
          cm_SavePreset(['name=' + cbPresets.Items.Strings[cbPresets.ItemIndex]]);
        Result := True;
      end;

      mrempPromptUser:
      begin
        MyMsgResult := msgYesNoCancel(Format(rsMulRenSaveModifiedPreset, [cbPresets.Items.Strings[cbPresets.ItemIndex]]), msmbCancel);
        case MyMsgResult of
          mmrYes:
          begin
            cm_SavePreset([]);
            Result := True;
          end;
          mmrNo: Result := True;
          mmrCancel: ;
        end;
      end;
    end;
  end;
end;

{ TfrmMultiRename.SavePreset }
procedure TfrmMultiRename.SavePreset(PresetName: string);
var
  PresetIndex: integer;
  AMultiRenamePresetObject: TMultiRenamePreset;
begin
  if PresetName <> '' then
  begin
    PresetIndex := FMultiRenamePresetList.Find(PresetName);
    if PresetIndex = -1 then
    begin
      AMultiRenamePresetObject := TMultiRenamePreset.Create;
      AMultiRenamePresetObject.PresetName := PresetName;
      PresetIndex := FMultiRenamePresetList.Add(AMultiRenamePresetObject);
    end;

    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].FileName := edName.Text;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Extension := edExt.Text;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].FileNameStyle := cbNameMaskStyle.ItemIndex;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].ExtensionStyle := cmbExtensionStyle.ItemIndex;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Find := edFind.Text;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Replace := edReplace.Text;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].RegExp := cbRegExp.Checked;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].UseSubs := cbUseSubs.Checked;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Counter := edPoc.Text;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Interval := edInterval.Text;
    FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Width := cmbxWidth.ItemIndex;

    case gMulRenSaveRenamingLog of
      mrsrlPerPreset:
      begin
        FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Log := cbLog.Checked;
        FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogFile := fneRenameLogFileFilename.FileName;
        FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogAppend := cbLogAppend.Checked;
      end;

      mrsrlAppendSameLog:
      begin
        FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Log := FbRememberLog;
        FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogAppend := FbRememberAppend;
        FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogFile := FsRememberRenameLogFilename;
      end;
    end;

    SavePresets;
  end;
end;

{ TfrmMultiRename.SavePresetsXml }
procedure TfrmMultiRename.SavePresetsXml(AConfig: TXmlConfig);
var
  i: integer;
  ANode, SubNode: TXmlNode;
begin
  ANode := AConfig.FindNode(AConfig.RootNode, sPresetsSection, True);
  AConfig.ClearNode(ANode);

  if cbPresets.ItemIndex = 0 then
    AConfig.SetValue(ANode, 'LastPreset', sLASTPRESET)
  else
    AConfig.SetValue(ANode, 'LastPreset', cbPresets.Items.Strings[cbPresets.ItemIndex]);

  ANode := AConfig.FindNode(ANode, 'Presets', True);

  for i := 0 to pred(FMultiRenamePresetList.Count) do
  begin
    SubNode := AConfig.AddNode(ANode, 'Preset');
    AConfig.AddValue(SubNode, 'Name', FMultiRenamePresetList.MultiRenamePreset[i].PresetName);
    AConfig.AddValue(SubNode, 'Filename', FMultiRenamePresetList.MultiRenamePreset[i].FileName);
    AConfig.AddValue(SubNode, 'Extension', FMultiRenamePresetList.MultiRenamePreset[i].Extension);
    AConfig.AddValue(SubNode, 'FilenameStyle', FMultiRenamePresetList.MultiRenamePreset[i].FileNameStyle);
    AConfig.AddValue(SubNode, 'ExtensionStyle', FMultiRenamePresetList.MultiRenamePreset[i].ExtensionStyle);
    AConfig.AddValue(SubNode, 'Find', FMultiRenamePresetList.MultiRenamePreset[i].Find);
    AConfig.AddValue(SubNode, 'Replace', FMultiRenamePresetList.MultiRenamePreset[i].Replace);
    AConfig.AddValue(SubNode, 'RegExp', FMultiRenamePresetList.MultiRenamePreset[i].RegExp);
    AConfig.AddValue(SubNode, 'UseSubs', FMultiRenamePresetList.MultiRenamePreset[i].UseSubs);
    AConfig.AddValue(SubNode, 'Counter', FMultiRenamePresetList.MultiRenamePreset[i].Counter);
    AConfig.AddValue(SubNode, 'Interval', FMultiRenamePresetList.MultiRenamePreset[i].Interval);
    AConfig.AddValue(SubNode, 'Width', FMultiRenamePresetList.MultiRenamePreset[i].Width);
    AConfig.SetValue(SubNode, 'Log/Enabled', FMultiRenamePresetList.MultiRenamePreset[i].Log);
    AConfig.SetValue(SubNode, 'Log/Append', FMultiRenamePresetList.MultiRenamePreset[i].LogAppend);
    AConfig.SetValue(SubNode, 'Log/File', FMultiRenamePresetList.MultiRenamePreset[i].LogFile);
  end;
end;

{ TfrmMultiRename.SavePresets }
procedure TfrmMultiRename.SavePresets;
begin
  SavePresetsXml(gConfig);
  gConfig.Save;
end;

{ TfrmMultiRename.DeletePreset }
procedure TfrmMultiRename.DeletePreset(PresetName: string);
var
  PresetIndex: integer;
begin
  if PresetName <> '' then
  begin
    PresetIndex := FMultiRenamePresetList.Find(PresetName);
    if PresetIndex <> -1 then
    begin
      FMultiRenamePresetList.Delete(PresetIndex);
      SavePresets;
    end;
  end;
end;

{ TfrmMultiRename.FillPresetsList }
//We fill the preset drop list with the element in memory.
//If it's specified when called, will attempt to load the specified preset in parameter.
//If it's not specified, will attempt to re-select the one that was initially selected.
//If nothing is still selected, we'll select the [Last One].
procedure TfrmMultiRename.FillPresetsList(const WantedSelectedPresetName: string = '');
var
  i: integer;
  sRememberSelection, PresetName: string;

begin
  sRememberSelection := '';

  if WantedSelectedPresetName <> '' then
    sRememberSelection := WantedSelectedPresetName;

  if sRememberSelection = '' then
    if cbPresets.ItemIndex <> -1 then
      if cbPresets.ItemIndex < cbPresets.Items.Count then
        sRememberSelection := cbPresets.Items.Strings[cbPresets.ItemIndex];

  cbPresets.Clear;
  cbPresets.Items.Add(rsMulRenLastPreset);

  for i := 0 to pred(FMultiRenamePresetList.Count) do
  begin
    PresetName := FMultiRenamePresetList.MultiRenamePreset[i].PresetName;
    if (PresetName <> sLASTPRESET) then
      if cbPresets.Items.IndexOf(PresetName) = -1 then
        cbPresets.Items.Add(PresetName);
  end;

  if (WantedSelectedPresetName = sLASTPRESET) or (WantedSelectedPresetName = sFRESHMASKS) then
    cbPresets.ItemIndex := 0
  else
  if sRememberSelection <> '' then
    if cbPresets.Items.IndexOf(sRememberSelection) <> -1 then
      cbPresets.ItemIndex := cbPresets.Items.IndexOf(sRememberSelection);

  if cbPresets.ItemIndex = -1 then
    if cbPresets.Items.Count > 0 then
      cbPresets.ItemIndex := 0;

  if WantedSelectedPresetName <> sFRESHMASKS then
  begin
    cbPresetsChange(cbPresets);
    RefreshActivePresetCommands;
  end;
end;

{ TfrmMultiRename.RefreshActivePresetCommands }
procedure TfrmMultiRename.RefreshActivePresetCommands;
begin
  //"Load last preset" is always available since it's the [Last One].
  actLoadPreset1.Enabled := (cbPresets.Items.Count > 1) and (cbPresets.Enabled);
  actLoadPreset2.Enabled := (cbPresets.Items.Count > 2) and (cbPresets.Enabled);
  actLoadPreset3.Enabled := (cbPresets.Items.Count > 3) and (cbPresets.Enabled);
  actLoadPreset4.Enabled := (cbPresets.Items.Count > 4) and (cbPresets.Enabled);
  actLoadPreset5.Enabled := (cbPresets.Items.Count > 5) and (cbPresets.Enabled);
  actLoadPreset6.Enabled := (cbPresets.Items.Count > 6) and (cbPresets.Enabled);
  actLoadPreset7.Enabled := (cbPresets.Items.Count > 7) and (cbPresets.Enabled);
  actLoadPreset8.Enabled := (cbPresets.Items.Count > 8) and (cbPresets.Enabled);
  actLoadPreset9.Enabled := (cbPresets.Items.Count > 9) and (cbPresets.Enabled);
  actSavePreset.Enabled := (cbPresets.ItemIndex > 0);
  //"Save as is always available so we may save the [Last One]
  actRenamePreset.Enabled := (cbPresets.ItemIndex > 0);
  actDeletePreset.Enabled := (cbPresets.ItemIndex > 0);
end;

{ TfrmMultiRename.InitializeMaskHelper }
procedure TfrmMultiRename.InitializeMaskHelper;
begin
  if MaskHelpers[00].sMenuItem = '' then //"MaskHelpers" are no tin the object but generic, so we just need to initialize once.
  begin
    MaskHelpers[00].sMenuItem := MaskHelpers[00].sKeyword + ' ' + rsMulRenMaskName;
    MaskHelpers[01].sMenuItem := MaskHelpers[01].sKeyword + ' ' + rsMulRenMaskCharAtPosX;
    MaskHelpers[02].sMenuItem := MaskHelpers[02].sKeyword + ' ' + rsMulRenMaskCharAtPosXtoY;
    MaskHelpers[03].sMenuItem := MaskHelpers[03].sKeyword + ' ' + rsMulRenMaskFullName;
    MaskHelpers[04].sMenuItem := MaskHelpers[04].sKeyword + ' ' + rsMulRenMaskFullNameCharAtPosXtoY;
    MaskHelpers[05].sMenuItem := MaskHelpers[05].sKeyword + ' ' + rsMulRenMaskParent;
    MaskHelpers[06].sMenuItem := MaskHelpers[06].sKeyword + ' ' + rsMulRenMaskExtension;
    MaskHelpers[07].sMenuItem := MaskHelpers[07].sKeyword + ' ' + rsMulRenMaskCharAtPosX;
    MaskHelpers[08].sMenuItem := MaskHelpers[08].sKeyword + ' ' + rsMulRenMaskCharAtPosXtoY;
    MaskHelpers[09].sMenuItem := MaskHelpers[09].sKeyword + ' ' + rsMulRenMaskCounter;
    MaskHelpers[10].sMenuItem := MaskHelpers[10].sKeyword + ' ' + rsMulRenMaskGUID;
    MaskHelpers[11].sMenuItem := MaskHelpers[11].sKeyword + ' ' + rsMulRenMaskVarOnTheFly;
    MaskHelpers[12].sMenuItem := MaskHelpers[12].sKeyword + ' ' + rsMulRenMaskYear2Digits;
    MaskHelpers[13].sMenuItem := MaskHelpers[13].sKeyword + ' ' + rsMulRenMaskYear4Digits;
    MaskHelpers[14].sMenuItem := MaskHelpers[14].sKeyword + ' ' + rsMulRenMaskMonth;
    MaskHelpers[15].sMenuItem := MaskHelpers[15].sKeyword + ' ' + rsMulRenMaskMonth2Digits;
    MaskHelpers[16].sMenuItem := MaskHelpers[16].sKeyword + ' ' + rsMulRenMaskMonthAbrev;
    MaskHelpers[17].sMenuItem := MaskHelpers[17].sKeyword + ' ' + rsMulRenMaskMonthComplete;
    MaskHelpers[18].sMenuItem := MaskHelpers[18].sKeyword + ' ' + rsMulRenMaskDay;
    MaskHelpers[19].sMenuItem := MaskHelpers[19].sKeyword + ' ' + rsMulRenMaskDay2Digits;
    MaskHelpers[20].sMenuItem := MaskHelpers[20].sKeyword + ' ' + rsMulRenMaskDOWAbrev;
    MaskHelpers[21].sMenuItem := MaskHelpers[21].sKeyword + ' ' + rsMulRenMaskDOWComplete;
    MaskHelpers[22].sMenuItem := MaskHelpers[22].sKeyword + ' ' + rsMulRenMaskCompleteDate;
    MaskHelpers[23].sMenuItem := MaskHelpers[23].sKeyword + ' ' + rsMulRenMaskHour;
    MaskHelpers[24].sMenuItem := MaskHelpers[24].sKeyword + ' ' + rsMulRenMaskHour2Digits;
    MaskHelpers[25].sMenuItem := MaskHelpers[25].sKeyword + ' ' + rsMulRenMaskMin;
    MaskHelpers[26].sMenuItem := MaskHelpers[26].sKeyword + ' ' + rsMulRenMaskMin2Digits;
    MaskHelpers[27].sMenuItem := MaskHelpers[27].sKeyword + ' ' + rsMulRenMaskSec;
    MaskHelpers[28].sMenuItem := MaskHelpers[28].sKeyword + ' ' + rsMulRenMaskSec2Digits;
    MaskHelpers[29].sMenuItem := MaskHelpers[29].sKeyword + ' ' + rsMulRenMaskCompleteTime;
  end;
end;

{ TfrmMultiRename.PopulateMainMenu }
// This main menu is not essential.
// But it does not occupy a lot of pixels and may benefit to user to help to both discover and remember the keyboard shortcut by visualizing them.
// Also, we populate it run-time to save work to valuable translators so they won't have to re-translate the same strings or to validate copies.
procedure TfrmMultiRename.PopulateMainMenu;
var
  miPresets, miMasks, miSubMasks: TMenuItem;
begin
  btnAnyNameMask.Action := actAnyNameMask;
  btnAnyNameMask.Caption := '...';
  btnAnyNameMask.Glyph.Clear;
  btnAnyNameMask.Width := fneRenameLogFileFilename.ButtonWidth;;
  btnAnyExtMask.Action := actAnyExtMask;
  btnAnyExtMask.Caption := '...';
  btnAnyExtMask.Glyph.Clear;
  btnAnyExtMask.Width := fneRenameLogFileFilename.ButtonWidth;;;
  btnRelativeRenameLogFile.Action := actInvokeRelativePath;
  btnRelativeRenameLogFile.Caption := '';
  btnRelativeRenameLogFile.Width := fneRenameLogFileFilename.ButtonWidth;
  btnRelativeRenameLogFile.Hint := actInvokeRelativePath.Caption;
  btnViewRenameLogFile.Action := actViewRenameLogFile;
  btnViewRenameLogFile.Caption := '';
  btnViewRenameLogFile.Width := fneRenameLogFileFilename.ButtonWidth;
  btnViewRenameLogFile.Hint := actViewRenameLogFile.Caption;
  btnPresets.Action := actShowPresetsMenu;
  btnPresets.Caption := '';
  btnPresets.Hint := actShowPresetsMenu.Caption;
  btnPresets.Width := fneRenameLogFileFilename.ButtonWidth;;;

  miPresets := TMenuItem.Create(mmMainMenu);
  miPresets.Caption := gbPresets.Caption;
  mmMainMenu.Items.Add(miPresets);
  BuildPresetsMenu(miPresets);
  BuildPresetsMenu(pmPresets);

  miMasks := TMenuItem.Create(mmMainMenu);
  miMasks.Caption := gbMaska.Caption;
  mmMainMenu.Items.Add(miMasks);
  //We add the sub-menu for the filename masks
  miSubMasks := TMenuItem.Create(miMasks);
  miSubMasks.Caption := lbName.Caption;
  miSubMasks.ImageIndex := GetImageIndexCategoryName(rmtuFilename);
  miMasks.Add(miSubMasks);
  PopulateFilenameMenu(miSubMasks);

  //We add the sub-menu for the filename masks
  miSubMasks := TMenuItem.Create(miMasks);
  miSubMasks.Caption := lbExt.Caption;
  miSubMasks.ImageIndex := GetImageIndexCategoryName(rmtuExtension);
  miMasks.Add(miSubMasks);
  PopulateExtensionMenu(miSubMasks);
end;

{ TfrmMultiRename.PopulateFilenameMenu }
procedure TfrmMultiRename.PopulateFilenameMenu(AMenuSomething: TComponent);
var
  localMenuItem, miSubMenu, miMenuItem: TMenuItem;
begin
  if AMenuSomething.ClassType = TPopupMenu then
    localMenuItem := TPopupMenu(AMenuSomething).Items
  else if AMenuSomething.ClassType = TMenuItem then
  begin
    localMenuItem := TMenuItem(AMenuSomething);

    miMenuItem := TMenuItem.Create(localMenuItem);
    miMenuItem.Action := actAnyNameMask;
    localMenuItem.Add(miMenuItem);
    miMenuItem := TMenuItem.Create(localMenuItem);
    miMenuItem.Caption := '-';
    localMenuItem.Add(miMenuItem);
  end
  else
    exit;

  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuFilename), GetImageIndexCategoryName(rmtuFilename));
  BuildMaskMenu(miSubMenu, tfmFilename, rmtuFilename);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuExtension), GetImageIndexCategoryName(rmtuExtension));
  BuildMaskMenu(miSubMenu, tfmFilename, rmtuExtension);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuCounter), GetImageIndexCategoryName(rmtuCounter));
  BuildMaskMenu(miSubMenu, tfmFilename, rmtuCounter);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuDate), GetImageIndexCategoryName(rmtuDate));
  BuildMaskMenu(miSubMenu, tfmFilename, rmtuDate);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuTime), GetImageIndexCategoryName(rmtuTime));
  BuildMaskMenu(miSubMenu, tfmFilename, rmtuTime);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuPlugins), GetImageIndexCategoryName(rmtuPlugins));
  BuildMaskMenu(miSubMenu, tfmFilename, rmtuPlugins);
  AppendSubMenuToThisMenu(localMenuItem, '-', -1);
  AppendActionMenuToThisMenu(localMenuItem, actClearNameMask);
  AppendActionMenuToThisMenu(localMenuItem, actResetAll);
end;

{ TfrmMultiRename.PopulateExtensionMenu }
procedure TfrmMultiRename.PopulateExtensionMenu(AMenuSomething: TComponent);
var
  localMenuItem, miSubMenu, miMenuItem: TMenuItem;
begin
  if AMenuSomething.ClassType = TPopupMenu then
    localMenuItem := TPopupMenu(AMenuSomething).Items
  else if AMenuSomething.ClassType = TMenuItem then
  begin
    localMenuItem := TMenuItem(AMenuSomething);

    miMenuItem := TMenuItem.Create(localMenuItem);
    miMenuItem.Action := actAnyExtMask;
    localMenuItem.Add(miMenuItem);
    miMenuItem := TMenuItem.Create(localMenuItem);
    miMenuItem.Caption := '-';
    localMenuItem.Add(miMenuItem);
  end
  else
    exit;

  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuFilename), GetImageIndexCategoryName(rmtuFilename));
  BuildMaskMenu(miSubMenu, tfmExtension, rmtuFilename);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuExtension), GetImageIndexCategoryName(rmtuExtension));
  BuildMaskMenu(miSubMenu, tfmExtension, rmtuExtension);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuCounter), GetImageIndexCategoryName(rmtuCounter));
  BuildMaskMenu(miSubMenu, tfmExtension, rmtuCounter);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuDate), GetImageIndexCategoryName(rmtuDate));
  BuildMaskMenu(miSubMenu, tfmExtension, rmtuDate);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuTime), GetImageIndexCategoryName(rmtuTime));
  BuildMaskMenu(miSubMenu, tfmExtension, rmtuTime);
  miSubMenu := AppendSubMenuToThisMenu(localMenuItem, GetMaskCategoryName(rmtuPlugins), GetImageIndexCategoryName(rmtuPlugins));
  BuildMaskMenu(miSubMenu, tfmExtension, rmtuPlugins);
  AppendSubMenuToThisMenu(localMenuItem, '-', -1);
  AppendActionMenuToThisMenu(localMenuItem, actClearExtMask);
  AppendActionMenuToThisMenu(localMenuItem, actResetAll);
end;

{ TfrmMultiRename.BuildMaskMenu }
procedure TfrmMultiRename.BuildMaskMenu(AMenuSomething: TComponent; iTarget: tTargetForMask; iMenuTypeMask: tRenameMaskToUse);
var
  iSeekIndex: integer;
  AMenuItem, localMenuItem: TMenuItem;
  actCategoryActionToAdd: TAction = nil;
begin
  if AMenuSomething.ClassType = TPopupMenu then
    localMenuItem := TPopupMenu(AMenuSomething).Items
  else if AMenuSomething.ClassType = TMenuItem then
    localMenuItem := TMenuItem(AMenuSomething)
  else
    exit;

  localMenuItem.Clear;

  if AMenuSomething.ClassType = TMenuItem then
  begin
    actCategoryActionToAdd := GetCategoryAction(iTarget, iMenuTypeMask);
    if actCategoryActionToAdd <> nil then
    begin
      AMenuItem := TMenuItem.Create(AMenuSomething);
      AMenuItem.Action := actCategoryActionToAdd;
      localMenuItem.Add(AMenuItem);

      AMenuItem := TMenuItem.Create(AMenuSomething);
      AMenuItem.Caption := '-';
      localMenuItem.Add(AMenuItem);
    end;
  end;

  for iSeekIndex := 0 to pred(NBMAXHELPERS) do
  begin
    if MaskHelpers[iSeekIndex].iMenuType = iMenuTypeMask then
    begin
      AMenuItem := TMenuItem.Create(AMenuSomething);
      AMenuItem.Caption := MaskHelpers[iSeekIndex].sMenuItem;
      AMenuItem.Tag := (iSeekIndex shl 16) or Ord(iTarget);
      AMenuItem.Hint := MaskHelpers[iSeekIndex].sKeyword;
      AMenuItem.ImageIndex := GetImageIndexCategoryName(MaskHelpers[iSeekIndex].iMenuType);

      case MaskHelpers[iSeekIndex].MenuActionStyle of
        masStraight: AMenuItem.OnClick := @MenuItemStraightMaskClick;
        masXCharacters, masXYCharacters: AMenuItem.OnClick := @MenuItemXCharactersMaskClick;
        masAskVariable: AMenuItem.OnClick := @MenuItemVariableMaskClick;
        masDirectorySelector: AMenuItem.OnClick := @MenuItemDirectorySelectorMaskClick;
      end;

      localMenuItem.Add(AMenuItem);
    end;
  end;

  if rmtuPlugins = iMenuTypeMask then
  begin
    FPluginDispatcher := iTarget;
    FillContentFieldMenu(AMenuSomething, @miPluginClick); //No need to clear "pmDynamicMasks" because "FillContentFieldMenu" do it itself.

    if AMenuSomething.ClassType = TMenuItem then
    begin
      //We need to add the mask category menu item at the end since "FillContentFieldMenu" clears our "pmDynamicMasks".
      AMenuItem := TMenuItem.Create(AMenuSomething);
      AMenuItem.Caption := '-';
      localMenuItem.Insert(0, AMenuItem);

      AMenuItem := TMenuItem.Create(AMenuSomething);
      AMenuItem.Action := actCategoryActionToAdd;
      localMenuItem.Insert(0, AMenuItem);
    end;
  end;
end;

{ TfrmMultiRename.BuildPresetsMenu }
procedure TfrmMultiRename.BuildPresetsMenu(AMenuSomething: TComponent);
var
  localMenuItem: TMenuItem;
begin
  if AMenuSomething.ClassType = TPopupMenu then
    localMenuItem := TPopupMenu(AMenuSomething).Items
  else if AMenuSomething.ClassType = TMenuItem then
  begin
    localMenuItem := TMenuItem(AMenuSomething);
    AppendActionMenuToThisMenu(localMenuItem, actShowPresetsMenu);
    AppendSubMenuToThisMenu(localMenuItem, '-', -1);
  end
  else
    exit;

  AppendActionMenuToThisMenu(localMenuItem, actDropDownPresetList);
  AppendSubMenuToThisMenu(localMenuItem, '-', -1);
  AppendActionMenuToThisMenu(localMenuItem, actLoadLastPreset);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset1);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset2);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset3);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset4);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset5);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset6);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset7);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset8);
  AppendActionMenuToThisMenu(localMenuItem, actLoadPreset9);
  AppendSubMenuToThisMenu(localMenuItem, '-', -1);
  AppendActionMenuToThisMenu(localMenuItem, actSavePreset);
  AppendActionMenuToThisMenu(localMenuItem, actSavePresetAs);
  AppendActionMenuToThisMenu(localMenuItem, actRenamePreset);
  AppendActionMenuToThisMenu(localMenuItem, actDeletePreset);
  AppendActionMenuToThisMenu(localMenuItem, actSortPresets);
end;

{ TfrmMultiRename.BuildMenuAndPopup }
procedure TfrmMultiRename.BuildMenuAndPopup(iTarget: tTargetForMask; iMenuTypeMask: tRenameMaskToUse);
begin
  BuildMaskMenu(pmDynamicMasks, iTarget, iMenuTypeMask);
  case iTarget of
    tfmFilename: PopupDynamicMenuAtThisControl(pmDynamicMasks, edName);
    tfmExtension: PopupDynamicMenuAtThisControl(pmDynamicMasks, edExt);
  end;
end;

{ TfrmMultiRename.GetMaskCategoryName }
function TfrmMultiRename.GetMaskCategoryName(aRenameMaskToUse: tRenameMaskToUse): string;
begin
  Result := '';
  case aRenameMaskToUse of
    rmtuFilename: Result := rsMulRenFilename;
    rmtuExtension: Result := rsMulRenExtension;
    rmtuCounter: Result := rsMulRenCounter;
    rmtuDate: Result := rsMulRenDate;
    rmtuTime: Result := rsMulRenTime;
    rmtuPlugins: Result := rsMulRenPlugins;
  end;
end;

{ TfrmMultiRename.GetImageIndexCategoryName }
function TfrmMultiRename.GetImageIndexCategoryName(aRenameMaskToUse: tRenameMaskToUse): integer;
begin
  Result := -1;
  case aRenameMaskToUse of
    rmtuFilename: Result := 20;
    rmtuExtension: Result := 21;
    rmtuCounter: Result := 22;
    rmtuDate: Result := 23;
    rmtuTime: Result := 24;
    rmtuPlugins: Result := 25;
  end;
end;

{ TfrmMultiRename.GetCategoryAction }
function TfrmMultiRename.GetCategoryAction(TargetForMask: tTargetForMask; aRenameMask: tRenameMaskToUse): TAction;
begin
  Result := nil;
  case TargetForMask of
    tfmFilename:
    begin
      case aRenameMask of
        rmtuFilename: Result := actNameNameMask;
        rmtuExtension: Result := actExtNameMask;
        rmtuCounter: Result := actCtrNameMask;
        rmtuDate: Result := actDateNameMask;
        rmtuTime: Result := actTimeNameMask;
        rmtuPlugins: Result := actPlgnNameMask;
      end;
    end;

    tfmExtension:
    begin
      case aRenameMask of
        rmtuFilename: Result := actNameExtMask;
        rmtuExtension: Result := actExtExtMask;
        rmtuCounter: Result := actCtrExtMask;
        rmtuDate: Result := actDateExtMask;
        rmtuTime: Result := actTimeExtMask;
        rmtuPlugins: Result := actPlgnExtMask;
      end;
    end;
  end;
end;

{ TfrmMultiRename.AppendSubMenuToThisMenu }
function TfrmMultiRename.AppendSubMenuToThisMenu(ATargetMenu: TMenuItem; sCaption: string; iImageIndex: integer): TMenuItem;
begin
  Result := TMenuItem.Create(ATargetMenu);
  Result.ImageIndex := iImageIndex;
  if sCaption <> '' then
    Result.Caption := sCaption;
  ATargetMenu.Add(Result);
end;

{ TfrmMultiRename.AppendActionMenuToThisMenu }
function TfrmMultiRename.AppendActionMenuToThisMenu(ATargetMenu: TMenuItem; paramAction: TAction): TMenuItem;
begin
  Result := TMenuItem.Create(ATargetMenu);
  Result.Action := paramAction;
  ATargetMenu.Add(Result);
end;

{ TfrmMultiRename.MenuItemXCharactersMaskClick }
procedure TfrmMultiRename.MenuItemXCharactersMaskClick(Sender: TObject);
var
  sSourceToSelectFromText, sPrefix: string;
  sResultingMaskValue: string = '';
  iMaskHelperIndex: integer;
begin
  iMaskHelperIndex := TMenuItem(Sender).Tag shr 16;

  if iMaskHelperIndex < length(MaskHelpers) then
  begin
    sSourceToSelectFromText := '';
    case MaskHelpers[iMaskHelperIndex].iSourceOfInformation of
      soiFilename:
      begin
        sSourceToSelectFromText := FFiles[pred(StringGrid.Row)].NameNoExt;
        sPrefix := 'N';
      end;

      soiExtension:
      begin
        sSourceToSelectFromText := FFiles[pred(StringGrid.Row)].Extension;
        sPrefix := 'E';
      end;

      soiFullName:
      begin
        sSourceToSelectFromText := FFiles[pred(StringGrid.Row)].FullPath;
        sPrefix := 'A';
      end;
    end;

    if ShowSelectTextRangeDlg(Self, Caption, sSourceToSelectFromText, sPrefix, sResultingMaskValue) then
      InsertMask(sResultingMaskValue, tTargetForMask(TMenuItem(Sender).Tag and iTARGETMASK));
  end;
end;

{ TfrmMultiRename.MenuItemDirectorySelectorMaskClick }
procedure TfrmMultiRename.MenuItemDirectorySelectorMaskClick(Sender: TObject);
var
  sSourceToSelectFromText, sPrefix: string;
  sResultingMaskValue: string = '';
  iMaskHelperIndex: integer;
begin
  iMaskHelperIndex := TMenuItem(Sender).Tag shr 16;

  if iMaskHelperIndex < length(MaskHelpers) then
  begin
    sSourceToSelectFromText := '';
    case MaskHelpers[iMaskHelperIndex].iSourceOfInformation of
      soiPath:
      begin
        sSourceToSelectFromText := FFiles[pred(StringGrid.Row)].Path;
        sPrefix := 'P';
      end;
    end;

    if ShowSelectPathRangeDlg(Self, Caption, sSourceToSelectFromText, sPrefix, sResultingMaskValue) then
      InsertMask(sResultingMaskValue, tTargetForMask(TMenuItem(Sender).Tag and iTARGETMASK));
  end;
end;

{ TfrmMultiRename.MenuItemVariableMaskClick }
procedure TfrmMultiRename.MenuItemVariableMaskClick(Sender: TObject);
var
  sVariableName: string;
begin
  sVariableName := rsSimpleWordVariable;
  if InputQuery(rsMulRenDefineVariableName, rsMulRenEnterNameForVar, sVariableName) then
  begin
    if sVariableName = '' then
      sVariableName := rsSimpleWordVariable;
    InsertMask('[V:' + sVariableName + ']', tTargetForMask(TMenuItem(Sender).Tag and iTARGETMASK));
  end;
end;

{ TfrmMultiRename.MenuItemStraightMaskClick }
procedure TfrmMultiRename.MenuItemStraightMaskClick(Sender: TObject);
var
  sMaks: string;
begin
  sMaks := TMenuItem(Sender).Hint;
  case tTargetForMask(TMenuItem(Sender).Tag and iTARGETMASK) of
    tfmFilename:
    begin
      InsertMask(sMaks, edName);
      edName.SetFocus;
    end;
    tfmExtension:
    begin
      InsertMask(sMaks, edExt);
      edExt.SetFocus;
    end;
  end;
end;

{ TfrmMultiRename.PopupDynamicMenuAtThisControl }
procedure TfrmMultiRename.PopupDynamicMenuAtThisControl(APopUpMenu: TPopupMenu; AControl: TControl);
var
  PopupPoint: TPoint;
begin
  PopupPoint := AControl.Parent.ClientToScreen(Point(AControl.Left + AControl.Width - 5, AControl.Top + AControl.Height - 5));
  APopUpMenu.PopUp(PopupPoint.X, PopupPoint.Y);
end;

{ TfrmMultiRename.miPluginClick }
procedure TfrmMultiRename.miPluginClick(Sender: TObject);
var
  sMask: string;
  MenuItem: TMenuItem absolute Sender;
begin
  case MenuItem.Tag of
    0:
    begin
      sMask := '[=DC().' + MenuItem.Hint + '{}]';
    end;
    1:
    begin
      sMask := '[=Plugin(' + MenuItem.Parent.Caption + ').' + MenuItem.Hint + '{}]';
    end;
    2:
    begin
      sMask := '[=Plugin(' + MenuItem.Parent.Parent.Caption + ').' + MenuItem.Parent.Hint + '{' + MenuItem.Hint + '}]';
    end;
    3:
    begin
      sMask := '[=DC().' + MenuItem.Parent.Hint + '{' + MenuItem.Hint + '}]';
    end;
  end;

  case FPluginDispatcher of
    tfmFilename:
    begin
      InsertMask(sMask, edName);
      edName.SetFocus;
    end;
    tfmExtension:
    begin
      InsertMask(sMask, edExt);
      edExt.SetFocus;
    end;
  end;
end;

{ TfrmMultiRename.InsertMask }
procedure TfrmMultiRename.InsertMask(const Mask: string; edChoose: TEdit);
var
  sTmp, sInitialString: string;
  I: integer;
begin
  sInitialString := edChoose.Text;
  if edChoose.SelLength > 0 then
    edChoose.SelText := Mask // Replace selected text
  else
  begin
    sTmp := edChoose.Text;
    I := edChoose.SelStart + 1;  // Insert on current position
    UTF8Insert(Mask, sTmp, I);
    Inc(I, UTF8Length(Mask));
    edChoose.Text := sTmp;
    edChoose.SelStart := I - 1;
  end;
  if sInitialString <> edChoose.Text then
    cbNameStyleChange(edChoose);
end;

{ TfrmMultiRename.InsertMask }
procedure TfrmMultiRename.InsertMask(const Mask: string; TargetForMask: tTargetForMask);
begin
  case TargetForMask of
    tfmFilename:
    begin
      InsertMask(Mask, edName);
      edName.SetFocus;
    end;

    tfmExtension:
    begin
      InsertMask(Mask, edExt);
      edExt.SetFocus;
    end;
  end;
end;

{TfrmMultiRename.sReplace }
function TfrmMultiRename.sReplace(sMask: string; ItemNr: integer): string;
var
  iStart, iEnd: integer;
begin
  Result := '';
  while Length(sMask) > 0 do
  begin
    iStart := Pos('[', sMask);
    if iStart > 0 then
    begin
      iEnd := Pos(']', sMask);
      if iEnd > 0 then
      begin
        Result := Result + Copy(sMask, 1, iStart - 1) +
          sHandleFormatString(Copy(sMask, iStart + 1, iEnd - iStart - 1), ItemNr);
        Delete(sMask, 1, iEnd);
      end
      else
        Break;
    end
    else
      Break;
  end;
  Result := Result + sMask;
end;

{ TfrmMultiRename.sReplaceXX }
function TfrmMultiRename.sReplaceXX(const sFormatStr, sOrig: string): string;
var
  iFrom, iTo, iDelim: integer;
begin
  if Length(sFormatStr) = 1 then
    Result := sOrig
  else
  begin
    iDelim := Pos(':', sFormatStr);
    if iDelim = 0 then
    begin
      iDelim := Pos(',', sFormatStr);
      // Not found
      if iDelim = 0 then
      begin
        iFrom := StrToIntDef(Copy(sFormatStr, 2, MaxInt), 1);
        if iFrom < 0 then
          iFrom := sOrig.Length + iFrom + 1;
        iTo := iFrom;
      end
      // Range e.g. N1,3 (from 1, 3 symbols)
      else
      begin
        iFrom := StrToIntDef(Copy(sFormatStr, 2, iDelim - 2), 1);
        iDelim := Abs(StrToIntDef(Copy(sFormatStr, iDelim + 1, MaxSmallint), MaxSmallint));
        if iFrom >= 0 then
          iTo := iDelim + iFrom - 1
        else
        begin
          iTo := sOrig.Length + iFrom + 1;
          iFrom := Max(iTo - iDelim + 1, 1);
        end;
      end;
    end
    // Range e.g. N1:2 (from 1 to 2)
    else
    begin
      iFrom := StrToIntDef(Copy(sFormatStr, 2, iDelim - 2), 1);
      if iFrom < 0 then
        iFrom := sOrig.Length + iFrom + 1;
      iTo := StrToIntDef(Copy(sFormatStr, iDelim + 1, MaxSmallint), MaxSmallint);
      if iTo < 0 then
        iTo := sOrig.Length + iTo + 1;
      ;
      if iTo < iFrom then
      begin
        iDelim := iTo;
        iTo := iFrom;
        iFrom := iDelim;
      end;
    end;
    Result := UTF8Copy(sOrig, iFrom, iTo - iFrom + 1);
  end;
end;

{ TfrmMultiRename.sReplaceVariable }
function TfrmMultiRename.sReplaceVariable(const sFormatStr: string): string;
var
  iDelim, iVariableIndex, iVariableSuggestionIndex: integer;
  sVariableName: string = '';
  sVariableValue: string = '';
begin
  Result := '';

  iDelim := Pos(':', sFormatStr);
  if iDelim <> 0 then
    sVariableName := copy(sFormatStr, succ(iDelim), length(sFormatStr) - iDelim)
  else
    sVariableName := rsSimpleWordVariable;

  iVariableIndex := FslVariableNames.IndexOf(sVariableName);
  if iVariableIndex = -1 then
  begin
    iVariableSuggestionIndex := FslVariableSuggestionName.IndexOf(sVariableName);
    if iVariableSuggestionIndex <> -1 then
      sVariableValue := FslVariableSuggestionValue.Strings[iVariableSuggestionIndex]
    else
      sVariableValue := sVariableName;

    if InputQuery(rsMulRenDefineVariableValue, Format(rsMulRenEnterValueForVar, [sVariableName]), sVariableValue) then
    begin
      FslVariableNames.Add(sVariableName);
      iVariableIndex := FslVariableValues.Add(sVariableValue);
      if iVariableSuggestionIndex = -1 then
      begin
        FslVariableSuggestionName.Add(sVariableName);
        FslVariableSuggestionValue.Add(sVariableValue);
      end;
    end
    else
    begin
      FActuallyRenamingFile := False;
      exit;
    end;
  end;
  Result := FslVariableValues.Strings[iVariableIndex];
end;

{ TfrmMultiRename.sReplaceBadChars }//Replace bad path chars in string
function TfrmMultiRename.sReplaceBadChars(const sPath: string): string;
const
{$IFDEF MSWINDOWS}
  ForbiddenChars: set of char = ['<', '>', ':', '"', '/', '\', '|', '?', '*'];
{$ELSE}
  ForbiddenChars: set of char = ['/'];
{$ENDIF}
var
  Index: integer;
begin
  Result := '';
  for Index := 1 to Length(sPath) do
    if not (sPath[Index] in ForbiddenChars) then
      Result += sPath[Index]
    else
      Result += gMulRenInvalidCharReplacement;
end;

{ TfrmMultiRename.IsLetter }
function TfrmMultiRename.IsLetter(AChar: AnsiChar): boolean;
begin
  Result :=  // Ascii letters
    ((AChar < #128) and
    (((AChar >= 'a') and (AChar <= 'z')) or
    ((AChar >= 'A') and (AChar <= 'Z')))) or
    // maybe Ansi or UTF8
    (AChar >= #128);
end;

{ TfrmMultiRename.ApplyStyle }
// Applies style (uppercase, lowercase, etc.) to a string.
function TfrmMultiRename.ApplyStyle(InputString: string; Style: integer): string;
begin
  case Style of
    1: Result := UTF8UpperCase(InputString);
    2: Result := UTF8LowerCase(InputString);
    3: Result := FirstCharOfFirstWordToUppercaseUTF8(InputString);
    4: Result := FirstCharOfEveryWordToUppercaseUTF8(InputString);
    else
      Result := InputString;
  end;
end;

{ TfrmMultiRename.FirstCharToUppercaseUTF8 }
// Changes first char to uppercase and the rest to lowercase
function TfrmMultiRename.FirstCharToUppercaseUTF8(InputString: string): string;
var
  FirstChar: string;
begin
  if UTF8Length(InputString) > 0 then
  begin
    Result := UTF8LowerCase(InputString);
    FirstChar := UTF8Copy(Result, 1, 1);
    UTF8Delete(Result, 1, 1);
    Result := UTF8UpperCase(FirstChar) + Result;
  end
  else
    Result := '';
end;

{ TfrmMultiRename.FirstCharOfFirstWordToUppercaseUTF8 }
// Changes first char of first word to uppercase and the rest to lowercase
function TfrmMultiRename.FirstCharOfFirstWordToUppercaseUTF8(InputString: string): string;
var
  SeparatorPos: integer;
begin
  InputString := UTF8LowerCase(InputString);
  Result := '';

  // Search for first letter.
  for SeparatorPos := 1 to Length(InputString) do
    if IsLetter(InputString[SeparatorPos]) then
      break;

  Result := Copy(InputString, 1, SeparatorPos - 1) + FirstCharToUppercaseUTF8(Copy(InputString, SeparatorPos, Length(InputString) - SeparatorPos + 1));
end;

{ TfrmMultiRename.FirstCharOfEveryWordToUppercaseUTF8 }
// Changes first char of every word to uppercase and the rest to lowercase
function TfrmMultiRename.FirstCharOfEveryWordToUppercaseUTF8(InputString: string): string;
var
  SeparatorPos: integer;
begin
  InputString := UTF8LowerCase(InputString);
  Result := '';

  while InputString <> '' do
  begin
    // Search for first non-letter (word separator).
    for SeparatorPos := 1 to Length(InputString) do
      if not IsLetter(InputString[SeparatorPos]) then
        break;

    Result := Result + FirstCharToUppercaseUTF8(Copy(InputString, 1, SeparatorPos));

    Delete(InputString, 1, SeparatorPos);
  end;
end;

{ TfrmMultiRename.LoadNamesFromFile }
procedure TfrmMultiRename.LoadNamesFromFile(const AFileName: string);
var
  AFileList: TStringListEx;
begin
  AFileList := TStringListEx.Create;
  try
    AFileList.LoadFromFile(AFileName);
    if AFileList.Count <> FFiles.Count then
    begin
      msgError(Format(rsMulRenWrongLinesNumber, [AFileList.Count, FFiles.Count]));
    end
    else
    begin
      FNames.Assign(AFileList);


      gbMaska.Enabled := False;
      gbPresets.Enabled := False;
      gbCounter.Enabled := False;

      StringGridTopLeftChanged(StringGrid);
    end;
  except
    on E: Exception do
      msgError(E.Message);
  end;
  AFileList.Free;
end;

{ TfrmMultiRename.FreshText }
function TfrmMultiRename.FreshText(ItemIndex: integer): string;
var
  I: integer;
  bError: boolean;
  sTmpName, sTmpExt: string;
begin
  bError := False;

  if FNames.Count > 0 then
    Result := FNames[ItemIndex]
  else
  begin
    // Use mask
    sTmpName := sReplace(edName.Text, ItemIndex);
    sTmpExt := sReplace(edExt.Text, ItemIndex);

    // Join
    Result := sTmpName;
    if sTmpExt <> '' then
      Result := Result + '.' + sTmpExt;
  end;

  // Find and replace
  if (edFind.Text <> '') then
  begin
    if cbRegExp.Checked then
      try
        Result := UTF16ToUTF8(FRegExp.Replace(UTF8Decode(Result), UTF8Decode(edReplace.Text), cbUseSubs.Checked));
      except
        Result := rsMsgErrRegExpSyntax;
        bError := True;
      end
    else
    begin
      // Many at once, split find and replace by |
      if (FReplaceText.Count = 0) then
        FReplaceText.Add('');
      for I := 0 to FFindText.Count - 1 do
        Result := StringReplace(Result, FFindText[I], FReplaceText[Min(I, FReplaceText.Count - 1)], [rfReplaceAll, rfIgnoreCase]);
    end;
  end;

  // File name style
  sTmpExt := ExtractFileExt(Result);
  sTmpName := Copy(Result, 1, Length(Result) - Length(sTmpExt));

  sTmpName := ApplyStyle(sTmpName, cbNameMaskStyle.ItemIndex);
  sTmpExt := ApplyStyle(sTmpExt, cmbExtensionStyle.ItemIndex);

  Result := sTmpName + sTmpExt;

  actRename.Enabled := not bError;
  if bError then
  begin
    edFind.Color := clRed;
    edFind.Font.Color := clWhite;
  end
  else
  begin
    edFind.Color := clWindow;
    edFind.Font.Color := clWindowText;
  end;
end;

{ TfrmMultiRename.sHandleFormatString }
function TfrmMultiRename.sHandleFormatString(const sFormatStr: string; ItemNr: integer): string;
var
  aFile: TFile;
  Index: int64;
  Counter: int64;
  Dirs: TStringArray;
begin
  Result := '';
  if Length(sFormatStr) > 0 then
  begin
    aFile := FFiles[ItemNr];
    case sFormatStr[1] of
      '[', ']':
      begin
        Result := sFormatStr;
      end;

      'N':
      begin
        Result := sReplaceXX(sFormatStr, aFile.NameNoExt);
      end;

      'E':
      begin
        Result := sReplaceXX(sFormatStr, aFile.Extension);
      end;

      'A':
      begin
        Result := sReplaceBadChars(sReplaceXX(sFormatStr, aFile.FullPath));
      end;

      'G':
      begin
        Result := GuidToString(DCGetNewGUID);
      end;

      'V':
      begin
        if FActuallyRenamingFile then
          Result := sReplaceVariable(sFormatStr)
        else
          Result := '[' + sFormatStr + ']';
      end;

      'C':
      begin
        // Check for start value after C, e.g. C12
        if not TryStrToInt64(Copy(sFormatStr, 2, MaxInt), Index) then
          Index := StrToInt64Def(edPoc.Text, 1);
        Counter := Index + StrToInt64Def(edInterval.Text, 1) * ItemNr;
        Result := Format('%.' + cmbxWidth.Items[cmbxWidth.ItemIndex] + 'd', [Counter]);
      end;

      'P':  // sub path index
      begin
        Index := StrToIntDef(Copy(sFormatStr, 2, MaxInt), 0);
        Dirs := (aFile.Path + ' ').Split([PathDelim]);
        Dirs[High(Dirs)] := EmptyStr;
        if Index < 0 then
          Result := Dirs[Max(0, High(Dirs) + Index)]
        else
          Result := Dirs[Min(Index, High(Dirs))];
      end;

      '=':
      begin
        Result := sReplaceBadChars(FormatFileFunction(UTF8Copy(sFormatStr, 2, UTF8Length(sFormatStr) - 1), FFiles.Items[ItemNr], FFileSource, True));
      end;

      else
      begin
        // Assume it is date/time formatting string ([h][n][s][Y][M][D]).
        with FFiles.Items[ItemNr] do
          if fpModificationTime in SupportedProperties then
            try
              Result := SysToUTF8(FormatDateTime(sFormatStr, ModificationTime));
            except
              Result := sFormatStr;
            end;
      end;
    end;
  end;
end;

{ TfrmMultiRename.SetFilePropertyResult }
procedure TfrmMultiRename.SetFilePropertyResult(Index: integer; aFile: TFile; aTemplate: TFileProperty; Result: TSetFilePropertyResult);
var
  sFilenameForLog, S: string;
begin
  with TFileNameProperty(aTemplate) do
  begin
    if cbLog.Checked then
      if gMulRenFilenameWithFullPathInLog then
        sFilenameForLog := aFile.FullPath
      else
        sFilenameForLog := aFile.Name;

    case Result of
      sfprSuccess:
      begin
        S := 'OK      ' + sFilenameForLog + ' -> ' + Value;
        if Index < FFiles.Count then
          FFiles[Index].Name := Value // Write new name to the file object
        else
        begin
          Index := StrToInt(aFile.Extension);
          FFiles[Index].Name := Value; // Write new name to the file object
        end;
      end;
      sfprError: S := 'FAILED  ' + sFilenameForLog + ' -> ' + Value;
      sfprSkipped: S := 'SKIPPED ' + sFilenameForLog + ' -> ' + Value;
    end;
  end;
  if cbLog.Checked then
    FLog.Add(S);
end;

{ TfrmMultiRename.SetOutputGlobalRenameLogFilename }
procedure TfrmMultiRename.SetOutputGlobalRenameLogFilename;
begin
  if gMultRenDailyIndividualDirLog then
    fneRenameLogFileFilename.FileName := mbExpandFileName(ExtractFilePath(gMulRenLogFilename) + IncludeTrailingPathDelimiter(EnvVarTodaysDate) + ExtractFilename(gMulRenLogFilename))
  else
    fneRenameLogFileFilename.FileName := gMulRenLogFilename;
end;

{ TfrmMultiRename.cm_ResetAll }
procedure TfrmMultiRename.cm_ResetAll(const Params: array of string);
var
  Param: string;
  bNeedRefreshActivePresetCommands: boolean = True;
begin
  for Param in Params do
    GetParamBoolValue(Param, sREFRESHCOMMANDS, bNeedRefreshActivePresetCommands);

  edName.Text := '[N]';
  edName.SelStart := UTF8Length(edName.Text);
  edExt.Text := '[E]';
  edExt.SelStart := UTF8Length(edExt.Text);
  edFind.Text := '';
  edReplace.Text := '';
  cbRegExp.Checked := False;
  cbUseSubs.Checked := False;
  cbNameMaskStyle.ItemIndex := 0;
  cmbExtensionStyle.ItemIndex := 0;
  edPoc.Text := '1';
  edInterval.Text := '1';
  cmbxWidth.ItemIndex := 0;

  case gMulRenSaveRenamingLog of
    mrsrlPerPreset:
    begin
      cbLog.Checked := False;
      cbLog.Enabled := True;
      cbLogAppend.Checked := False;
      fneRenameLogFileFilename.Enabled := cbLog.Checked;
      actInvokeRelativePath.Enabled := cbLog.Checked;
      actViewRenameLogFile.Enabled := cbLog.Checked;
      cbLogAppend.Enabled := cbLog.Checked;
      if (FFiles.Count > 0) then
        fneRenameLogFileFilename.FileName := FFiles[0].Path + sDEFAULTLOGFILENAME
      else
        fneRenameLogFileFilename.FileName := sDEFAULTLOGFILENAME;
    end;

    mrsrlAppendSameLog:
    begin
      cbLog.Checked := True;
      cbLog.Enabled := False;
      cbLogAppend.Checked := True;
      cbLogAppend.Enabled := False;
      fneRenameLogFileFilename.Enabled := False;
      SetOutputGlobalRenameLogFilename;
      actInvokeRelativePath.Enabled := False;
      actViewRenameLogFile.Enabled := cbLog.Checked;
    end;
  end;

  cbPresets.Text := '';
  FNames.Clear;
  gbMaska.Enabled := True;
  gbPresets.Enabled := True;
  cbPresets.ItemIndex := 0;
  gbCounter.Enabled := True;
  StringGridTopLeftChanged(StringGrid);
  if bNeedRefreshActivePresetCommands then
    RefreshActivePresetCommands;
end;

{ TfrmMultiRename.cm_InvokeEditor }
procedure TfrmMultiRename.cm_InvokeEditor(const {%H-}Params: array of string);
begin
  DCPlaceCursorNearControlIfNecessary(btnEditor);
  pmEditDirect.PopUp;
end;

{ TfrmMultiRename.cm_LoadNamesFromFile }
procedure TfrmMultiRename.cm_LoadNamesFromFile(const {%H-}Params: array of string);
begin
  dmComData.OpenDialog.FileName := EmptyStr;
  dmComData.OpenDialog.Filter := AllFilesMask;
  if dmComData.OpenDialog.Execute then
    LoadNamesFromFile(dmComData.OpenDialog.FileName);
end;

{ TfrmMultiRename.cm_EditNames }
procedure TfrmMultiRename.cm_EditNames(const {%H-}Params: array of string);
var
  I: integer;
  AFileName: string;
  AFileList: TStringListEx;
begin
  AFileList := TStringListEx.Create;
  AFileName := GetTempFolderDeletableAtTheEnd;
  AFileName := GetTempName(AFileName) + '.txt';
  if FNames.Count > 0 then
    AFileList.Assign(FNames)
  else
  begin
    for I := 0 to FFiles.Count - 1 do
      AFileList.Add(FFiles[I].Name);
  end;
  try
    AFileList.SaveToFile(AFileName);
    try
      if ShowMultiRenameWaitForm(AFileName, Self) then
        LoadNamesFromFile(AFileName);
    finally
      mbDeleteFile(AFileName);
    end;
  except
    on E: Exception do
      msgError(E.Message);
  end;
  AFileList.Free;
end;

{ TfrmMultiRename.cm_EditNewNames }
procedure TfrmMultiRename.cm_EditNewNames(const {%H-}Params: array of string);
var
  sFileName: string;
  iIndexFile: integer;
  AFileList: TStringListEx;
begin
  AFileList := TStringListEx.Create;
  try
    for iIndexFile := 0 to pred(FFiles.Count) do
      AFileList.Add(FreshText(iIndexFile));
    sFileName := GetTempName(GetTempFolderDeletableAtTheEnd) + '.txt';
    try
      AFileList.SaveToFile(sFileName);
      try
        if ShowMultiRenameWaitForm(sFileName, Self) then
          LoadNamesFromFile(sFileName);
      finally
        mbDeleteFile(sFileName);
      end;
    except
      on E: Exception do
        msgError(E.Message);
    end;
  finally
    AFileList.Free;
  end;
end;

{ TfrmMultiRename.cm_Config }
procedure TfrmMultiRename.cm_Config(const {%H-}Params: array of string);
begin
  frmMain.Commands.cm_Options(['TfrmOptionsMultiRename']);
end;

{ TfrmMultiRename.cm_Rename }
procedure TfrmMultiRename.cm_Rename(const {%H-}Params: array of string);
var
  AFile: TFile;
  NewName: string;
  I, J, K: integer;
  TempFiles: TStringList;
  OldFiles, NewFiles: TFiles;
  AutoRename: boolean = False;
  Operation: TFileSourceOperation;
  theNewProperties: TFileProperties;
  LogFileStream: TFileStream;
begin
  FActuallyRenamingFile := True;
  try
    if cbLog.Checked then
    begin
      if fneRenameLogFileFilename.FileName = EmptyStr then
        fneRenameLogFileFilename.FileName := FFiles[0].Path + sDEFAULTLOGFILENAME;
      mbForceDirectory(ExtractFileDir(mbExpandFileName(fneRenameLogFileFilename.FileName)));
      FLog := TStringListEx.Create;
      if cbLogAppend.Checked then
        FLog.Add(';' + DateTimeToStr(Now) + ' - ' + rsMulRenLogStart);
    end;

    OldFiles := FFiles.Clone;
    TempFiles := TStringList.Create;
    NewFiles := TFiles.Create(EmptyStr);
    FslVariableNames.Clear;
    FslVariableValues.Clear; //We don't clear the "Suggestion" parts because we may re-use them as their original values if we ever re-do rename pass witht he same instance.

    // OldNames
    FOldNames.Clear;
    for I := 0 to OldFiles.Count - 1 do
      FOldNames.Add(OldFiles[I].Name, Pointer(PtrInt(I)));

    try
      FNewNames.Clear;
      for I := 0 to FFiles.Count - 1 do
      begin
        AFile := TFile.Create(EmptyStr);
        AFile.Name := FreshText(I);

        //In "FreshText", if there was a "Variable on the fly / [V:Hint]" and the user aborted it, the "FActuallyRenamingFile" will be cleared and so we abort the actual renaming process.
        if not FActuallyRenamingFile then
          Exit;

        // Checking duplicates
        NewName := FFiles[I].Path + AFile.Name;
        J := FNewNames.Find(NewName);
        if J < 0 then
          FNewNames.Add(NewName)
        else
        begin
          if not AutoRename then
          begin
            if MessageDlg(rsMulRenWarningDuplicate + LineEnding +
              NewName + LineEnding + LineEnding + rsMulRenAutoRename,
              mtWarning, [mbYes, mbAbort], 0, mbAbort) <> mrYes then
              Exit;
            AutoRename := True;
          end;
          K := 1;
          while J >= 0 do
          begin
            NewName := FFiles[I].Path + AFile.NameNoExt + ' (' + IntToStr(K) + ')';
            if AFile.Extension <> '' then
              NewName := NewName + ExtensionSeparator + AFile.Extension;
            J := FNewNames.Find(NewName);
            Inc(K);
          end;
          FNewNames.Add(NewName);
          AFile.Name := ExtractFileName(NewName);
        end;

        // Avoid collisions with OldNames
        J := FOldNames.Find(AFile.Name);
        if (J >= 0) and (PtrUInt(FOldNames.List[J]^.Data) <> I) then
        begin
          NewName := AFile.Name;
          // Generate temp file name, save file index as extension
          AFile.FullPath := GetTempName(FFiles[I].Path) + ExtensionSeparator + IntToStr(I);
          TempFiles.AddObject(NewName, AFile.Clone);
        end;

        NewFiles.Add(AFile);
      end;

      // Rename temp files back
      if TempFiles.Count > 0 then
      begin
        for I := 0 to TempFiles.Count - 1 do
        begin
          // Temp file name
          OldFiles.Add(TFile(TempFiles.Objects[I]));
          // Real new file name
          AFile := TFile.Create(EmptyStr);
          AFile.Name := TempFiles[I];
          NewFiles.Add(AFile);
        end;
      end;

      // Rename files
      FillChar({%H-}theNewProperties, SizeOf(TFileProperties), 0);
      Operation := FFileSource.CreateSetFilePropertyOperation(OldFiles, theNewProperties);
      if Assigned(Operation) then
      begin
        with Operation as TFileSourceSetFilePropertyOperation do
        begin
          SetTemplateFiles(NewFiles);
          OnSetFilePropertyResult := @SetFilePropertyResult;
        end;
        OperationsManager.AddOperationModal(Operation);
      end;
    finally
      if cbLog.Checked then
      begin
        try
          if (cbLogAppend.Checked) and (FileExists(mbExpandFileName(fneRenameLogFileFilename.FileName))) then
          begin
            LogFileStream := TFileStream.Create(mbExpandFileName(fneRenameLogFileFilename.FileName), fmOpenWrite);
            try
              LogFileStream.Seek(0, soEnd);
              FLog.SaveToStream(LogFileStream);
            finally
              LogFileStream.Free;
            end;
          end
          else
          begin
            FLog.SaveToFile(mbExpandFileName(fneRenameLogFileFilename.FileName));
          end;
        except
          on E: Exception do
            msgError(E.Message);
        end;
        FLog.Free;
      end;
      OldFiles.Free;
      NewFiles.Free;
      TempFiles.Free;
    end;
  finally
    FActuallyRenamingFile := False;
  end;

  StringGridTopLeftChanged(StringGrid);
end;

{ TfrmMultiRename.cm_Close }
procedure TfrmMultiRename.cm_Close(const {%H-}Params: array of string);
begin
  Close;
end;

{ TfrmMultiRename.cm_ShowPresetsMenu }
procedure TfrmMultiRename.cm_ShowPresetsMenu(const {%H-}Params: array of string);
begin
  PopupDynamicMenuAtThisControl(pmPresets, btnPresets);
end;

{ TfrmMultiRename.cm_DropDownPresetList }
procedure TfrmMultiRename.cm_DropDownPresetList(const {%H-}Params: array of string);
begin
  if (not cbPresets.CanFocus) and (not cbPresets.Enabled) then
    if isOkToLosePresetModification = True then
      cbPresets.Enabled := True;

  if cbPresets.CanFocus then
  begin
    cbPresets.SetFocus;
    cbPresets.DroppedDown := True;
  end;
end;

{ TfrmMultiRename.cm_LoadPreset }
procedure TfrmMultiRename.cm_LoadPreset(const Params: array of string);
var
  sPresetName: string;
  PresetIndex: integer;
begin
  if isOkToLosePresetModification then
  begin
    //1.Get the preset name from the parameters.
    sPresetName := GetPresetNameForCommand(Params);

    //2.Make sure we got something.
    if sPresetName <> '' then
    begin
      //3.Make sure it is in our list.
      PresetIndex := FMultiRenamePresetList.Find(sPresetName);
      if PresetIndex <> -1 then
      begin
        edName.Text := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].FileName;
        edName.SelStart := UTF8Length(edName.Text);
        edExt.Text := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Extension;
        edExt.SelStart := UTF8Length(edExt.Text);
        cbNameMaskStyle.ItemIndex := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].FileNameStyle;
        cmbExtensionStyle.ItemIndex := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].ExtensionStyle;
        edFind.Text := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Find;
        edReplace.Text := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Replace;
        cbRegExp.Checked := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].RegExp;
        cbUseSubs.Checked := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].UseSubs;
        edPoc.Text := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Counter;
        edInterval.Text := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Interval;
        cmbxWidth.ItemIndex := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Width;

        case gMulRenSaveRenamingLog of
          mrsrlPerPreset:
          begin
            cbLog.Checked := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Log;
            cbLogAppend.Checked := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogAppend;
            fneRenameLogFileFilename.FileName := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogFile;
          end;

          mrsrlAppendSameLog:
          begin
            FbRememberLog := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].Log;
            FbRememberAppend := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogAppend;
            FsRememberRenameLogFilename := FMultiRenamePresetList.MultiRenamePreset[PresetIndex].LogFile;
            SetOutputGlobalRenameLogFilename;
          end;
        end;

        //4.Preserved the last loaded setup.
        FLastPreset := sPresetName;

        //5.Refresh the whole thing.
        edFindChange(edFind);

        //6.We might come here with parameter "index=x" so make sure we switch also the preset combo box to the same index.
        if PresetIndex >= cbPresets.Items.Count then
          PresetIndex := 0;
        if cbPresets.ItemIndex <> PresetIndex then
          cbPresets.ItemIndex := PresetIndex;

        //7.Since we've load the setup, activate things so we may change setup if necessary.
        SetConfigurationState(CONFIG_SAVED);

        //8. If we're from anything else the preset droplist itself, let's go to focus on the name ready to edit it if necessary..
        if (ActiveControl <> cbPresets) and (ActiveControl <> edName) and (edName.Enabled and gbMaska.Enabled) then
        begin
          ActiveControl := edName;
          edName.SelStart := UTF8Length(edName.Text);
        end;
      end;
    end;
  end;
end;

{ TfrmMultiRename.cm_LoadLastPreset }
procedure TfrmMultiRename.cm_LoadLastPreset(const Params: array of string);
begin
  cm_LoadPreset(['index=0']);
end;

{ TfrmMultiRename.cm_LoadPreset1 }
procedure TfrmMultiRename.cm_LoadPreset1(const Params: array of string);
begin
  cm_LoadPreset(['index=1']);
end;

{ TfrmMultiRename.cm_LoadPreset2 }
procedure TfrmMultiRename.cm_LoadPreset2(const Params: array of string);
begin
  cm_LoadPreset(['index=2']);
end;

{ TfrmMultiRename.cm_LoadPreset3 }
procedure TfrmMultiRename.cm_LoadPreset3(const Params: array of string);
begin
  cm_LoadPreset(['index=3']);
end;

{ TfrmMultiRename.cm_LoadPreset4 }
procedure TfrmMultiRename.cm_LoadPreset4(const Params: array of string);
begin
  cm_LoadPreset(['index=4']);
end;

{ TfrmMultiRename.cm_LoadPreset5 }
procedure TfrmMultiRename.cm_LoadPreset5(const Params: array of string);
begin
  cm_LoadPreset(['index=5']);
end;

{ TfrmMultiRename.cm_LoadPreset6 }
procedure TfrmMultiRename.cm_LoadPreset6(const Params: array of string);
begin
  cm_LoadPreset(['index=6']);
end;

{ TfrmMultiRename.cm_LoadPreset7 }
procedure TfrmMultiRename.cm_LoadPreset7(const Params: array of string);
begin
  cm_LoadPreset(['index=7']);
end;

{ TfrmMultiRename.cm_LoadPreset8 }
procedure TfrmMultiRename.cm_LoadPreset8(const Params: array of string);
begin
  cm_LoadPreset(['index=8']);
end;

{ TfrmMultiRename.cm_LoadPreset9 }
procedure TfrmMultiRename.cm_LoadPreset9(const Params: array of string);
begin
  cm_LoadPreset(['index=9']);
end;

{ TfrmMultiRename.cm_SavePreset }
procedure TfrmMultiRename.cm_SavePreset(const Params: array of string);
begin
  if cbPresets.ItemIndex > 0 then
  begin
    SavePreset(cbPresets.Items.Strings[cbPresets.ItemIndex]);
    SetConfigurationState(CONFIG_SAVED);
  end;
end;

{ TfrmMultiRename.cm_SavePresetAs }
procedure TfrmMultiRename.cm_SavePresetAs(const Params: array of string);
var
  sNameForPreset: string;
  bKeepGoing: boolean;
begin
  sNameForPreset := GetPresetNameForCommand(Params);
  if sNameForPreset <> '' then
  begin
    bKeepGoing := True;
  end
  else
  begin
    if (FLastPreset = '') or (FLastPreset = sLASTPRESET) then
      sNameForPreset := rsMulRenDefaultPresetName
    else
      sNameForPreset := FLastPreset;
    bKeepGoing := InputQuery(Caption, rsMulRenPromptForSavedPresetName, sNameForPreset);
    if bKeepGoing then bKeepGoing := (sNameForPreset <> '');
  end;

  if bKeepGoing and (sNameForPreset <> FLastPreset) then
    if FMultiRenamePresetList.Find(sNameForPreset) <> -1 then
      if not msgYesNo(Format(rsMsgPresetAlreadyExists, [sNameForPreset]), msmbNo) then
        bKeepGoing := False;

  if bKeepGoing then
  begin
    SavePreset(sNameForPreset);

    if cbPresets.Items.IndexOf(sNameForPreset) = -1 then
    begin
      cbPresets.Items.Add(sNameForPreset);
    end;

    if cbPresets.ItemIndex <> cbPresets.Items.IndexOf(sNameForPreset) then
      cbPresets.ItemIndex := cbPresets.Items.IndexOf(sNameForPreset);

    SetConfigurationState(CONFIG_SAVED);
    RefreshActivePresetCommands;
  end;
end;

{ TfrmMultiRename.cm_RenamePreset }
// It also allow the at the same time to rename for changing case like "audio files" to "Audio Files".
procedure TfrmMultiRename.cm_RenamePreset(const Params: array of string);
var
  sCurrentName, sNewName: string;
  PresetIndex: integer;
  bKeepGoing: boolean;
begin
  sCurrentName := cbPresets.Items.Strings[cbPresets.ItemIndex];
  sNewName := sCurrentName;
  bKeepGoing := InputQuery(Caption, rsMulRenPromptNewPresetName, sNewName);
  if bKeepGoing and (sNewName <> '') and (sCurrentName <> sNewName) then
  begin
    PresetIndex := FMultiRenamePresetList.Find(sNewName);
    if (PresetIndex = -1) or (SameText(sCurrentName, sNewName)) then
    begin
      if SameText(FMultiRenamePresetList.MultiRenamePreset[cbPresets.ItemIndex].PresetName, cbPresets.Items.Strings[cbPresets.ItemIndex]) then
      begin
        FMultiRenamePresetList.MultiRenamePreset[cbPresets.ItemIndex].PresetName := sNewName;
        cbPresets.Items.Strings[cbPresets.ItemIndex] := sNewName;
      end;
    end
    else
    begin
      if msgYesNo(rsMulRenPromptNewNameExists, msmbNo) then
      begin
        if SameText(FMultiRenamePresetList.MultiRenamePreset[PresetIndex].PresetName, cbPresets.Items.Strings[PresetIndex]) and SameText(FMultiRenamePresetList.MultiRenamePreset[cbPresets.ItemIndex].PresetName, cbPresets.Items.Strings[cbPresets.ItemIndex]) then
        begin
          FMultiRenamePresetList.MultiRenamePreset[cbPresets.ItemIndex].PresetName := sNewName;
          cbPresets.Items.Strings[cbPresets.ItemIndex] := sNewName;

          cbPresets.Items.Delete(PresetIndex);
          FMultiRenamePresetList.Delete(PresetIndex);
        end;
      end;
    end;
  end;
end;

{ TfrmMultiRename.cm_DeletePreset }
procedure TfrmMultiRename.cm_DeletePreset(const Params: array of string);
var
  Index: integer;
  sPresetName: string;
begin
  sPresetName := GetPresetNameForCommand(Params);

  if sPresetName = '' then
    if cbPresets.ItemIndex > 0 then
      sPresetName := cbPresets.Items.Strings[cbPresets.ItemIndex];

  if sPresetName <> '' then
  begin
    if msgYesNo(Format(rsMsgPresetConfigDelete, [sPresetName]), msmbNo) then
    begin
      DeletePreset(sPresetName);
      Index := cbPresets.Items.IndexOf(sPresetName);
      if Index = cbPresets.ItemIndex then
        cbPresets.ItemIndex := 0;
      if Index <> -1 then
        cbPresets.Items.Delete(Index);
      FillPresetsList;
    end;
  end;
end;

{ TfrmMultiRename.cm_SortPresets }
procedure TfrmMultiRename.cm_SortPresets(const Params: array of string);
var
  slLocalPresets: TStringList;
  iSeeker, iPresetIndex: integer;
begin
  if isOkToLosePresetModification then
  begin
    if FMultiRenamePresetList.Count > 1 then
    begin
      slLocalPresets := TStringList.Create;
      try
        for iSeeker := 1 to pred(FMultiRenamePresetList.Count) do
          slLocalPresets.Add(FMultiRenamePresetList.MultiRenamePreset[iSeeker].PresetName);

        if HaveUserSortThisList(Self, rsMulRenSortingPresets, slLocalPresets) = mrOk then
        begin
          for iSeeker := 0 to pred(slLocalPresets.Count) do
          begin
            iPresetIndex := FMultiRenamePresetList.Find(slLocalPresets.Strings[iSeeker]);
            if succ(iSeeker) <> iPresetIndex then
              FMultiRenamePresetList.Move(iPresetIndex, succ(iSeeker));
          end;
          FillPresetsList(cbPresets.Items.Strings[cbPresets.ItemIndex]);
        end;
      finally
        slLocalPresets.Free;
      end;
    end;
  end;
end;

{ TfrmMultiRename.cm_AnyNameMask }
procedure TfrmMultiRename.cm_AnyNameMask(const {%H-}Params: array of string);
begin
  pmFloatingMainMaskMenu.Items.Clear;
  PopulateFilenameMenu(pmFloatingMainMaskMenu);
  PopupDynamicMenuAtThisControl(pmFloatingMainMaskMenu, btnAnyNameMask);
end;

{ TfrmMultiRename.cm_NameNameMask }
procedure TfrmMultiRename.cm_NameNameMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmFilename, rmtuFilename);
end;

{ TfrmMultiRename.cm_ExtNameMask }
procedure TfrmMultiRename.cm_ExtNameMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmFilename, rmtuExtension);
end;

{ TfrmMultiRename.cm_CtrNameMask }
procedure TfrmMultiRename.cm_CtrNameMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmFilename, rmtuCounter);
end;

{ TfrmMultiRename.cm_DateNameMask }
procedure TfrmMultiRename.cm_DateNameMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmFilename, rmtuDate);
end;

{ TfrmMultiRename.cm_TimeNameMask }
procedure TfrmMultiRename.cm_TimeNameMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmFilename, rmtuTime);
end;

{ TfrmMultiRename.cm_PlgnNameMask }
procedure TfrmMultiRename.cm_PlgnNameMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmFilename, rmtuPlugins);
end;

{ TfrmMultiRename.cm_ClearNameMask }
procedure TfrmMultiRename.cm_ClearNameMask(const {%H-}Params: array of string);
begin
  edName.Text := '';
  cbNameStyleChange(edExt);
  if edName.CanFocus then
    edName.SetFocus;
end;

{ TfrmMultiRename.cm_AnyExtMask }
procedure TfrmMultiRename.cm_AnyExtMask(const {%H-}Params: array of string);
begin
  pmFloatingMainMaskMenu.Items.Clear;
  PopulateExtensionMenu(pmFloatingMainMaskMenu);
  PopupDynamicMenuAtThisControl(pmFloatingMainMaskMenu, btnAnyExtMask);
end;

{ TfrmMultiRename.cm_NameExtMask }
procedure TfrmMultiRename.cm_NameExtMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmExtension, rmtuFilename);
end;

{ TfrmMultiRename.cm_ExtExtMask }
procedure TfrmMultiRename.cm_ExtExtMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmExtension, rmtuExtension);
end;

{ TfrmMultiRename.cm_CtrExtMask }
procedure TfrmMultiRename.cm_CtrExtMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmExtension, rmtuCounter);
end;

{ TfrmMultiRename.cm_DateExtMask }
procedure TfrmMultiRename.cm_DateExtMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmExtension, rmtuDate);
end;

{ TfrmMultiRename.cm_TimeExtMask }
procedure TfrmMultiRename.cm_TimeExtMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmExtension, rmtuTime);
end;

{ TfrmMultiRename.cm_PlgnExtMask }
procedure TfrmMultiRename.cm_PlgnExtMask(const {%H-}Params: array of string);
begin
  BuildMenuAndPopup(tfmExtension, rmtuPlugins);
end;

{ TfrmMultiRename.cm_ClearExtMask }
procedure TfrmMultiRename.cm_ClearExtMask(const {%H-}Params: array of string);
begin
  edExt.Text := '';
  cbNameStyleChange(edExt);
  if edExt.CanFocus then edExt.SetFocus;
end;

{ TfrmMultiRename.cm_ViewRenameLogFile }
procedure TfrmMultiRename.cm_ViewRenameLogFile(const {%H-}Params: array of string);
var
  sRenameLogFilename: string;
begin
  sRenameLogFilename := mbExpandFileName(fneRenameLogFileFilename.FileName);
  if FileExists(sRenameLogFilename) then
    ShowViewerByGlob(sRenameLogFilename)
  else
    MsgError(Format(rsMsgFileNotFound, [sRenameLogFilename]));
end;

{ ShowMultiRenameForm }
// Will be in fact the lone function called externally to launch a MultiRename dialog.
function ShowMultiRenameForm(aFileSource: IFileSource; var aFiles: TFiles; const PresetToLoad: string = ''): boolean;
begin
  Result := True;
  try
    with TfrmMultiRename.Create(Application, aFileSource, aFiles, PresetToLoad) do
    begin
      Show;
    end;
  except
    Result := False;
  end;
end;

initialization
  TFormCommands.RegisterCommandsForm(TfrmMultiRename, HotkeysCategoryMultiRename, @rsHotkeyCategoryMultiRename);

end.





