unit fQuickSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, LCLType, LCLProc,
  ExtCtrls, Buttons;

type
  TQuickSearchMode = (qsSearch, qsFilter);
  TQuickSearchDirection = (qsdNone, qsdFirst, qsdLast, qsdNext, qsdPrevious);
  TQuickSearchMatch = (qsmBeginning, qsmEnding);
  TQuickSearchCase = (qscSensitive, qscInsensitive);
  TQuickSearchItems = (qsiFiles, qsiDirectories, qsiFilesAndDirectories);

  TQuickSearchOptions = record
    Match: set of TQuickSearchMatch;
    SearchCase: TQuickSearchCase;
    Items: TQuickSearchItems;
  end;

  TOnChangeSearch = procedure(Sender: TObject; ASearchText: UTF8String; ASearchOptions: TQuickSearchOptions; Direction: TQuickSearchDirection = qsdNone) of Object;
  TOnChangeFilter = procedure(Sender: TObject; AFilterText: UTF8String; AFilterOptions: TQuickSearchOptions) of Object;
  TOnExecute = procedure(Sender: TObject) of Object;
  TOnHide = procedure(Sender: TObject) of Object;

  { TfrmQuickSearch }

  TfrmQuickSearch = class(TFrame)
    btnCancel: TButton;
    edtSearch: TEdit;
    lblSearch: TLabel;
    pnlOptions: TPanel;
    sbMatchBeginning: TSpeedButton;
    sbMatchEnding: TSpeedButton;
    sbCaseSensitive: TSpeedButton;
    sbFiles: TSpeedButton;
    sbDirectories: TSpeedButton;
    tglFilter: TToggleBox;
    tglOptions: TToggleBox;
    procedure btnCancelClick(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FrameExit(Sender: TObject);
    procedure sbCaseSensitiveClick(Sender: TObject);
    procedure sbFilesAndDirectoriesClick(Sender: TObject);
    procedure sbMatchBeginningClick(Sender: TObject);
    procedure sbMatchEndingClick(Sender: TObject);
    procedure tglFilterChange(Sender: TObject);
    procedure tglOptionsChange(Sender: TObject);
  private
    Options: TQuickSearchOptions;
    Mode: TQuickSearchMode;
    Active: Boolean;
    FilterOptions: TQuickSearchOptions;
    FilterText: String;
    Finalizing: Boolean;
    {en
       Loads control states from options values
    }
    procedure LoadControlStates;
    procedure PushFilter;
    procedure PopFilter;
    procedure ClearFilter;
    procedure CancelFilter;
  public
    OnChangeSearch: TOnChangeSearch;
    OnChangeFilter: TOnChangeFilter;
    OnExecute: TOnExecute;
    OnHide: TOnHide;
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure Initialize(SearchMode: TQuickSearchMode; Char: TUTF8Char = #0);
    procedure Finalize;
    procedure ToggleOption(ParamOption: String);
    function CheckSearchOrFilter(var Key: Word): Boolean; overload;
    function CheckSearchOrFilter(var UTF8Key: TUTF8Char): Boolean; overload;
  end;

{en
   Allows to compare TQuickSearchOptions structures
}
  operator = (qsOptions1, qsOptions2: TQuickSearchOptions) CompareResult: Boolean;

implementation

uses
  uKeyboard,
  uGlobs;

const
  // parameters for quick search / filter actions
  PARAMETER_TOGGLE_FILTER = 'togglefilter';
  PARAMETER_MATCH_BEGINNING = 'matchbeginning';
  PARAMETER_MATCH_ENDING = 'matchending';
  PARAMETER_CASE_SENSITIVE = 'casesensitivity';
  PARAMETER_FILES_DIRECTORIES = 'filesdirectories';

{$R *.lfm}

operator = (qsOptions1, qsOptions2: TQuickSearchOptions) CompareResult: Boolean;
begin
  Result := True;

  if qsOptions1.Match <> qsOptions2.Match then
    Result := False;

  if qsOptions1.Items <> qsOptions2.Items then
    Result := False;

  if qsOptions1.SearchCase <> qsOptions2.SearchCase then
    Result := False;
end;

{ TfrmQuickSearch }

constructor TfrmQuickSearch.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);

  Self.Parent := TheOwner;

  // load default options
  Options := gQuickSearchOptions;
  LoadControlStates;

  FilterOptions := gQuickSearchOptions;
  FilterText := EmptyStr;
  Finalizing := False;

  HotMan.Register(Self.edtSearch, 'Quick Search');
end;

destructor TfrmQuickSearch.Destroy;
begin
  if Assigned(HotMan) then
    HotMan.UnRegister(Self.edtSearch);

  inherited Destroy;
end;

procedure TfrmQuickSearch.Initialize(SearchMode: TQuickSearchMode; Char: TUTF8Char = #0);
begin
  tglFilter.Checked := SearchMode = qsFilter;

  Self.Visible := True;
  edtSearch.SetFocus;

  if Char = #0 then
    edtSearch.SelectAll
  else
    edtSearch.SelText := Char;

  Self.Active := True;
end;

procedure TfrmQuickSearch.Finalize;
begin
  PopFilter;

  Self.Visible := False;
end;

procedure TfrmQuickSearch.ToggleOption(ParamOption: String);
begin
  ParamOption := LowerCase(ParamOption);

  if ParamOption = PARAMETER_TOGGLE_FILTER then
  begin
    tglFilter.Checked := not tglFilter.Checked;
  end
  else if ParamOption = PARAMETER_MATCH_BEGINNING then
  begin
    sbMatchBeginning.Down := not sbMatchBeginning.Down;

    sbMatchBeginningClick(nil);
  end
  else if ParamOption = PARAMETER_MATCH_ENDING then
  begin
    sbMatchEnding.Down := not sbMatchEnding.Down;

    sbMatchEndingClick(nil);
  end
  else if ParamOption = PARAMETER_CASE_SENSITIVE then
  begin
    sbCaseSensitive.Down := not sbCaseSensitive.Down;

    sbCaseSensitiveClick(nil);
  end
  else if ParamOption = PARAMETER_FILES_DIRECTORIES then
  begin
    if sbFiles.Down and sbDirectories.Down then
      sbDirectories.Down := False
    else if sbFiles.Down then
    begin
      sbDirectories.Down := True;
      sbFiles.Down := False;
    end
    else if sbDirectories.Down then
      sbFiles.Down := True;

    sbFilesAndDirectoriesClick(nil);
  end;
end;

function TfrmQuickSearch.CheckSearchOrFilter(var Key: Word): Boolean;
var
  ModifierKeys: TShiftState;
  SearchOrFilterModifiers: TShiftState;
  SearchMode: TQuickSearchMode;
  UTF8Char: TUTF8Char;
  KeyTypingModifier: TKeyTypingModifier;
begin
  Result := False;

  ModifierKeys := GetKeyShiftStateEx;

  for KeyTypingModifier in TKeyTypingModifier do
  begin
    if gKeyTyping[KeyTypingModifier] in [ktaQuickSearch, ktaQuickFilter] then
    begin
      SearchOrFilterModifiers := TKeyTypingModifierToShift[KeyTypingModifier];
      if ((SearchOrFilterModifiers <> []) and
         (ModifierKeys * KeyModifiersShortcutNoText = SearchOrFilterModifiers))
{$IFDEF MSWINDOWS}
      // Entering international characters with Ctrl+Alt on Windows.
      or ((SearchOrFilterModifiers = []) and
         (ModifierKeys * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]))
{$ENDIF}
      then
      begin
        UTF8Char := VirtualKeyToUTF8Char(Key, ModifierKeys - SearchOrFilterModifiers);
        Result := (UTF8Char <> '') and
                  (not ((Length(UTF8Char) = 1) and (UTF8Char[1] in [#0..#31])));

        if Result then
        begin
          Key := 0;
          case gKeyTyping[KeyTypingModifier] of
            ktaQuickSearch:
              SearchMode := qsSearch;
            ktaQuickFilter:
              SearchMode := qsFilter;
          end;
          Self.Initialize(SearchMode, UTF8Char);
        end;

        Exit;
      end;
    end;
  end;
end;

function TfrmQuickSearch.CheckSearchOrFilter(var UTF8Key: TUTF8Char): Boolean;
var
  ModifierKeys: TShiftState;
  SearchMode: TQuickSearchMode;
begin
  Result := False;

  // Check for certain Ascii keys.
  if (Length(UTF8Key) = 1) and (UTF8Key[1] in [#0..#32,'+','-','*']) then
    Exit;

  ModifierKeys := GetKeyShiftStateEx;
  if gKeyTyping[ktmNone] in [ktaQuickSearch, ktaQuickFilter] then
  begin
    if ModifierKeys * KeyModifiersShortcutNoText = TKeyTypingModifierToShift[ktmNone] then
      begin
        // Make upper case if either caps-lock is toggled or shift pressed.
        if (ssCaps in ModifierKeys) xor (ssShift in ModifierKeys) then
          UTF8Key := UTF8UpperCase(UTF8Key)
        else
          UTF8Key := UTF8LowerCase(UTF8Key);

        case gKeyTyping[ktmNone] of
          ktaQuickSearch:
            SearchMode := qsSearch;
          ktaQuickFilter:
            SearchMode := qsFilter;
        end;

        Self.Initialize(SearchMode, UTF8Key);
        UTF8Key := '';

        Result := True;

        Exit;
      end;
  end;
end;

procedure TfrmQuickSearch.LoadControlStates;
begin
  sbDirectories.Down := (Options.Items = qsiDirectories) or (Options.Items = qsiFilesAndDirectories);
  sbFiles.Down := (Options.Items = qsiFiles) or (Options.Items = qsiFilesAndDirectories);
  sbCaseSensitive.Down := Options.SearchCase = qscSensitive;
  sbMatchBeginning.Down := qsmBeginning in Options.Match;
  sbMatchEnding.Down := qsmEnding in Options.Match;

  pnlOptions.Visible := tglOptions.Checked;
end;

procedure TfrmQuickSearch.PushFilter;
begin
  FilterText := edtSearch.Text;
  FilterOptions := Options;
end;

procedure TfrmQuickSearch.PopFilter;
begin
  edtSearch.Text := FilterText;

  // there was no filter saved, do not continue loading
  if FilterText = EmptyStr then
    Exit;

  Options := FilterOptions;
  LoadControlStates;

  FilterText := EmptyStr;

  tglFilter.Checked := True;
end;

procedure TfrmQuickSearch.ClearFilter;
begin
  FilterText := EmptyStr;
  FilterOptions := Options;

  if Assigned(Self.OnChangeFilter) then
    Self.OnChangeFilter(Self, EmptyStr, FilterOptions);
end;

procedure TfrmQuickSearch.CancelFilter;
begin
  Finalize;
  {$IFDEF LCLGTK2}
  // On GTK2 OnExit for frame is not called when it is hidden,
  // but only when a control from outside of frame gains focus.
  FrameExit(nil);
  {$ENDIF}
end;

procedure TfrmQuickSearch.edtSearchChange(Sender: TObject);
begin
  case Self.Mode of
    qsSearch:
      if Assigned(Self.OnChangeSearch) then
        Self.OnChangeSearch(Self, edtSearch.Text, Options);
    qsFilter:
      if Assigned(Self.OnChangeFilter) then
        Self.OnChangeFilter(Self, edtSearch.Text, Options);
  end;
end;

procedure TfrmQuickSearch.btnCancelClick(Sender: TObject);
begin
  CancelFilter;
end;

procedure TfrmQuickSearch.edtSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CheckSearchOrFilter(Key) then
    Exit;

  case Key of
    VK_DOWN:
    begin
      Key := 0;

      if Assigned(Self.OnChangeSearch) then
        Self.OnChangeSearch(Self, edtSearch.Text, Options, qsdNext);
    end;

    VK_UP:
    begin
      Key := 0;

      if Assigned(Self.OnChangeSearch) then
        Self.OnChangeSearch(Self, edtSearch.Text, Options, qsdPrevious);
    end;

(*  // disabled as they can conflict with trying to get to start/end position
    // of edtSearch
    VK_HOME:
    begin
      Key := 0;

      if Assigned(Self.OnChangeSearch) then
        Self.OnChangeSearch(Self, edtSearch.Text, Options, qsdFirst);
    end;

    VK_END:
    begin
      Key := 0;

      if Assigned(Self.OnChangeSearch) then
        Self.OnChangeSearch(Self, edtSearch.Text, Options, qsdLast);
    end;
*)
    VK_RETURN, VK_SELECT:
    begin
      Key := 0;

      if Assigned(Self.OnExecute) then
        Self.OnExecute(Self);

      CancelFilter;
    end;

    VK_TAB:
    begin
      Key := 0;

      FrameExit(nil);
    end;

    VK_ESCAPE:
    begin
      Key := 0;

      CancelFilter;
    end;
  end;
end;

procedure TfrmQuickSearch.FrameExit(Sender: TObject);
begin
  if not Finalizing then
  begin
    Finalizing := True;

    Self.Active := False;

    if (Mode = qsFilter) and (edtSearch.Text <> EmptyStr) then
      Self.Visible := not gQuickFilterAutoHide
    else
      Finalize;

    if Assigned(Self.OnHide) then
      Self.OnHide(Self);

    Finalizing := False;
  end;
end;

procedure TfrmQuickSearch.sbCaseSensitiveClick(Sender: TObject);
begin
  if sbCaseSensitive.Down then
    Options.SearchCase := qscSensitive
  else
    Options.SearchCase := qscInsensitive;

  edtSearchChange(nil);
end;

procedure TfrmQuickSearch.sbFilesAndDirectoriesClick(Sender: TObject);
begin
  if sbFiles.Down and sbDirectories.Down then
    Options.Items := qsiFilesAndDirectories
  else if sbFiles.Down then
    Options.Items := qsiFiles
  else if sbDirectories.Down then
    Options.Items := qsiDirectories
  else
  begin
    // unchecking both should not be possible, so recheck last unchecked
    case Options.Items of
      qsiFiles:
        sbFiles.Down := True;
      qsiDirectories:
        sbDirectories.Down := True;
    end;

    Exit;
  end;

  edtSearchChange(nil);
end;

procedure TfrmQuickSearch.sbMatchBeginningClick(Sender: TObject);
begin
  if sbMatchBeginning.Down then
    Include(Options.Match, qsmBeginning)
  else
    Exclude(Options.Match, qsmBeginning);

  edtSearchChange(nil);
end;

procedure TfrmQuickSearch.sbMatchEndingClick(Sender: TObject);
begin
  if sbMatchEnding.Down then
    Include(Options.Match, qsmEnding)
  else
    Exclude(Options.Match, qsmEnding);

  edtSearchChange(nil);
end;

procedure TfrmQuickSearch.tglFilterChange(Sender: TObject);
begin
  if tglFilter.Checked then
    Mode := qsFilter
  else
    Mode := qsSearch;

  // if a filter was set in background and a search is opened, the filter
  // will get pushed staying active. Otherwise the filter wil be converted
  // in a search
  if not Active and (Mode = qsSearch) then
    PushFilter
  else if Active then
    ClearFilter;

  edtSearchChange(nil);
end;

procedure TfrmQuickSearch.tglOptionsChange(Sender: TObject);
begin
  pnlOptions.Visible := tglOptions.Checked;
end;

end.
