unit fQuickSearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, LCLType,
  ExtCtrls, Buttons;

type
  TQuickSearchMode = (qsSearch, qsFilter, qsNone);
  TQuickSearchDirection = (qsdNone, qsdFirst, qsdLast, qsdNext, qsdPrevious);
  TQuickSearchMatch = (qsmBeginning, qsmEnding);
  TQuickSearchCase = (qscSensitive, qscInsensitive);
  TQuickSearchItems = (qsiFiles, qsiDirectories, qsiFilesAndDirectories);
  TQuickSearchCancelMode = (qscmNode, qscmAtLeastOneThenCancelIfNoFound, qscmCancelIfNoFound);

  TQuickSearchOptions = record
    Match: set of TQuickSearchMatch;
    SearchCase: TQuickSearchCase;
    Items: TQuickSearchItems;
    Direction: TQuickSearchDirection;
    LastSearchMode: TQuickSearchMode;
    CancelSearchMode: TQuickSearchCancelMode;
  end;

  TOnChangeSearch = procedure(Sender: TObject; ASearchText: String; const ASearchOptions: TQuickSearchOptions; InvertSelection: Boolean = False) of Object;
  TOnChangeFilter = procedure(Sender: TObject; AFilterText: String; const AFilterOptions: TQuickSearchOptions) of Object;
  TOnExecute = procedure(Sender: TObject) of Object;
  TOnHide = procedure(Sender: TObject) of Object;

  { TfrmQuickSearch }

  TfrmQuickSearch = class(TFrame)
    btnCancel: TButton;
    edtSearch: TEdit;
    pnlOptions: TPanel;
    sbMatchBeginning: TSpeedButton;
    sbMatchEnding: TSpeedButton;
    sbCaseSensitive: TSpeedButton;
    sbFiles: TSpeedButton;
    sbDirectories: TSpeedButton;
    tglFilter: TToggleBox;
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
    procedure btnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnCancelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    Options: TQuickSearchOptions;
    Mode: TQuickSearchMode;
    Active: Boolean;
    FilterOptions: TQuickSearchOptions;
    FilterText: String;
    Finalizing: Boolean;
    FUpdateCount: Integer;
    FNeedsChangeSearch: Boolean;
    procedure BeginUpdate;
    procedure CheckFilesOrDirectoriesDown;
    procedure EndUpdate;
    procedure DoHide;
    procedure DoOnChangeSearch;
    {en
       Loads control states from options values
    }
    procedure LoadControlStates;
    procedure PushFilter;
    procedure PopFilter;
    procedure ClearFilter;
    procedure CancelFilter;
    procedure ProcessParams(const SearchMode: TQuickSearchMode; const Params: array of String);
  public
    OnChangeSearch: TOnChangeSearch;
    OnChangeFilter: TOnChangeFilter;
    OnExecute: TOnExecute;
    OnHide: TOnHide;
    constructor Create(TheOwner: TWinControl); reintroduce;
    destructor Destroy; override;
    procedure CloneTo(AQuickSearch: TfrmQuickSearch);
    procedure Execute(SearchMode: TQuickSearchMode; const Params: array of String; Char: TUTF8Char = #0);
    procedure Finalize;
    function CheckSearchOrFilter(var Key: Word): Boolean; overload;
    function CheckSearchOrFilter(var UTF8Key: TUTF8Char): Boolean; overload;
  end;

{en
   Allows to compare TQuickSearchOptions structures
}
  operator = (qsOptions1, qsOptions2: TQuickSearchOptions) CompareResult: Boolean;

implementation

uses
  LazUTF8,
  uKeyboard,
  uGlobs,
  uFormCommands
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  , uFileView
{$ENDIF}
  ;

const
{
  Parameters:

  "filter"           - set filtering (on/off/toggle)
  "search"           - set searching (on/off/cycle)
  "matchbeginning"   - set match beginning option (on/off/toggle)
  "matchending"      - set match ending option (on/off/toggle)
  "casesensitive"    - set case sensitive searching (on/off/toggle)
  "files"            - set filtering files (on/off/toggle)
  "directories"      - set filtering directories (on/off/toggle)
  "filesdirectories" - toggle between files, directories and both (no value)
  "text"="<...>"     - set <...> as new text to search/filter (string)

  'toggle' switches between on and off
  'cycle' goto to next, next, next and so one
}
  // parameters for quick search / filter actions
  PARAMETER_FILTER                 = 'filter';
  PARAMETER_SEARCH                 = 'search';
  PARAMETER_DIRECTION              = 'direction';
  PARAMETER_MATCH_BEGINNING        = 'matchbeginning';
  PARAMETER_MATCH_ENDING           = 'matchending';
  PARAMETER_CASE_SENSITIVE         = 'casesensitive';
  PARAMETER_FILES                  = 'files';
  PARAMETER_DIRECTORIES            = 'directories';
  PARAMETER_FILES_DIRECTORIES      = 'filesdirectories';
  PARAMETER_TEXT                   = 'text';

  TOGGLE_VALUE = 'toggle';
  CYCLE_VALUE = 'cycle';
  FIRST_VALUE = 'first';
  LAST_VALUE = 'last';
  NEXT_VALUE = 'next';

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

function GetBoolState(const Value: String; OldState: Boolean): Boolean;
begin
  if Value = TOGGLE_VALUE then
    Result := not OldState
  else if not GetBoolValue(Value, Result) then
    Result := OldState;
end;

{ TfrmQuickSearch }

constructor TfrmQuickSearch.Create(TheOwner: TWinControl);
begin
  inherited Create(TheOwner);

  Self.Parent := TheOwner;

  // load default options
  Options := gQuickSearchOptions;
  Options.LastSearchMode := qsNone;
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

procedure TfrmQuickSearch.CloneTo(AQuickSearch: TfrmQuickSearch);
var
  TempEvent: TNotifyEvent;
begin
  AQuickSearch.Active := Self.Active;
  AQuickSearch.Mode := Self.Mode;
  AQuickSearch.Options := Self.Options;
  AQuickSearch.LoadControlStates;
  AQuickSearch.FilterOptions := Self.FilterOptions;
  AQuickSearch.FilterText := Self.FilterText;
  TempEvent := AQuickSearch.edtSearch.OnChange;
  AQuickSearch.edtSearch.OnChange := nil;
  AQuickSearch.edtSearch.Text := Self.edtSearch.Text;
  AQuickSearch.edtSearch.SelStart := Self.edtSearch.SelStart;
  AQuickSearch.edtSearch.SelLength := Self.edtSearch.SelLength;
  AQuickSearch.edtSearch.OnChange := TempEvent;
  TempEvent := AQuickSearch.tglFilter.OnChange;
  AQuickSearch.tglFilter.OnChange := nil;
  AQuickSearch.tglFilter.Checked := Self.tglFilter.Checked;
  AQuickSearch.tglFilter.OnChange := TempEvent;
end;

procedure TfrmQuickSearch.DoOnChangeSearch;
begin
  if FUpdateCount > 0 then
    FNeedsChangeSearch := True
  else
  begin
    Options.LastSearchMode:=Self.Mode;
    case Self.Mode of
      qsSearch:
        if Assigned(Self.OnChangeSearch) then
          Self.OnChangeSearch(Self, edtSearch.Text, Options);
      qsFilter:
        if Assigned(Self.OnChangeFilter) then
          Self.OnChangeFilter(Self, edtSearch.Text, Options);
    end;
    FNeedsChangeSearch := False;
  end;
end;

procedure TfrmQuickSearch.Execute(SearchMode: TQuickSearchMode; const Params: array of String; Char: TUTF8Char = #0);
begin
  Self.Visible := True;

  if not edtSearch.Focused then
  begin
    edtSearch.SetFocus;
    if Char = #0 then
      edtSearch.SelectAll;
  end;

  if Char <> #0 then
    edtSearch.SelText := Char;

  Self.Active := True;

  ProcessParams(SearchMode, Params);
end;

procedure TfrmQuickSearch.Finalize;
begin
  PopFilter;
  Self.Visible := False;

  Options.LastSearchMode := qsNone;
  Options.Direction := qsdNone;
  Options.CancelSearchMode:=qscmNode;
end;

{ TfrmQuickSearch.ProcessParams }
procedure TfrmQuickSearch.ProcessParams(const SearchMode: TQuickSearchMode; const Params: array of String);
var
  Param: String;
  Value: String;
  bWeGotMainParam: boolean = False;
  bLegacyBehavior: boolean = False;
begin
  BeginUpdate;
  try
    Options.Direction:=qsdNone;

    for Param in Params do
    begin
      if (SearchMode=qsFilter) AND (GetParamValue(Param, PARAMETER_FILTER, Value)) then
      begin
        if (Value <> TOGGLE_VALUE) then
          tglFilter.Checked := GetBoolState(Value, tglFilter.Checked)
        else
          tglFilter.Checked := (not tglFilter.Checked) OR (Options.LastSearchMode<>qsFilter); //With "toggle", if mode was not previously, we activate filter mode.
        bWeGotMainParam := True;
      end
      else if (SearchMode=qsSearch) AND (GetParamValue(Param, PARAMETER_FILTER, Value)) then //Legacy
      begin
        tglFilter.Checked := GetBoolState(Value, tglFilter.Checked);
        bWeGotMainParam := True;
        bLegacyBehavior:= True;
      end
      else if (SearchMode=qsSearch) AND (GetParamValue(Param, PARAMETER_SEARCH, Value)) then
      begin
        if (Value <> CYCLE_VALUE) then
        begin
          Options.CancelSearchMode:=qscmNode;
          if (Value <> TOGGLE_VALUE) then
            tglFilter.Checked := not (GetBoolState(Value, tglFilter.Checked))
          else
            tglFilter.Checked := not((tglFilter.Checked) OR (Options.LastSearchMode<>qsSearch)); //With "toggle", if mode was not previously, we activate search mode.
        end
        else
        begin
          tglFilter.Checked:=FALSE;
          if Options.LastSearchMode<>qsSearch then
          begin
            Options.Direction:=qsdFirst; //With "cycle", if mode was not previously, we activate search mode AND do to first item
            Options.CancelSearchMode:=qscmAtLeastOneThenCancelIfNoFound;
          end
          else
          begin
            Options.Direction:=qsdNext;
            Options.CancelSearchMode:=qscmCancelIfNoFound;
          end;
        end;
        bWeGotMainParam := True;
      end
      else if (SearchMode=qsSearch) AND GetParamValue(Param, PARAMETER_DIRECTION, Value) then
      begin
        if Value = FIRST_VALUE then Options.Direction:=qsdFirst;
        if Value = LAST_VALUE then Options.Direction:=qsdLast;
        if Value = NEXT_VALUE then Options.Direction:=qsdNext;
      end
      else if GetParamValue(Param, PARAMETER_MATCH_BEGINNING, Value) then
      begin
        sbMatchBeginning.Down := GetBoolState(Value, sbMatchBeginning.Down);

        sbMatchBeginningClick(nil);
      end
      else if GetParamValue(Param, PARAMETER_MATCH_ENDING, Value) then
      begin
        sbMatchEnding.Down := GetBoolState(Value, sbMatchEnding.Down);

        sbMatchEndingClick(nil);
      end
      else if GetParamValue(Param, PARAMETER_CASE_SENSITIVE, Value) then
      begin
        sbCaseSensitive.Down := GetBoolState(Value, sbCaseSensitive.Down);

        sbCaseSensitiveClick(nil);
      end
      else if GetParamValue(Param, PARAMETER_FILES, Value) then
      begin
        sbFiles.Down := GetBoolState(Value, sbFiles.Down);

        sbFilesAndDirectoriesClick(nil);
      end
      else if GetParamValue(Param, PARAMETER_DIRECTORIES, Value) then
      begin
        sbDirectories.Down := GetBoolState(Value, sbDirectories.Down);

        sbFilesAndDirectoriesClick(nil);
      end
      else if Param = PARAMETER_FILES_DIRECTORIES then
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
      end
      else if GetParamValue(Param, PARAMETER_TEXT, Value) then
      begin
        edtSearch.Text := Value;
        edtSearch.SelectAll;
      end;
    end;

    CheckFilesOrDirectoriesDown;

    //If search or filter was called with no parameter...
    case SearchMode of
      qsSearch: if not bWeGotMainParam then tglFilter.Checked:=False;
      qsFilter: if not bWeGotMainParam then tglFilter.Checked:=True;
    end;

    if not bLegacyBehavior then
    begin
      case SearchMode of
        qsSearch: if tglFilter.Checked then CancelFilter;
        qsFilter: if not tglFilter.Checked then CancelFilter;
      end;
    end;

  finally
    EndUpdate;
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
      or (HasKeyboardAltGrKey and (SearchOrFilterModifiers = []) and
          (ModifierKeys * KeyModifiersShortcutNoText = [ssCtrl, ssAlt]))
{$ENDIF}
      then
      begin
        if (Key <> VK_SPACE) or (edtSearch.Text <> '') then
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
            Self.Execute(SearchMode, [], UTF8Char);
          end;
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

        Self.Execute(SearchMode, [], UTF8Key);
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
  DoHide;
end;

procedure TfrmQuickSearch.CheckFilesOrDirectoriesDown;
begin
  if not (sbFiles.Down or sbDirectories.Down) then
  begin
    // unchecking both should not be possible, so recheck last unchecked
    case Options.Items of
      qsiFiles:
        sbFiles.Down := True;
      qsiDirectories:
        sbDirectories.Down := True;
    end;
  end;
end;

procedure TfrmQuickSearch.edtSearchChange(Sender: TObject);
begin
  Options.Direction := qsdNone;
  DoOnChangeSearch;
end;

procedure TfrmQuickSearch.BeginUpdate;
begin
  Inc(FUpdateCount);
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
      begin
        Options.Direction:=qsdNext;
        Self.OnChangeSearch(Self, edtSearch.Text, Options, ssShift in Shift);
      end;
    end;

    VK_UP:
    begin
      Key := 0;

      if Assigned(Self.OnChangeSearch) then
      begin
        Options.Direction:=qsdPrevious;
        Self.OnChangeSearch(Self, edtSearch.Text, Options, ssShift in Shift);
      end;
    end;

    // Request to have CTRL pressed at the same time.
    // VK_HOME alone reserved to get to start position of edtSearch.
    VK_HOME:
    begin
      if ssCtrl in Shift then
      begin
        Key := 0;

        if Assigned(Self.OnChangeSearch) then
        begin
          Options.Direction := qsdFirst;
          Self.OnChangeSearch(Self, edtSearch.Text, Options, ssShift in Shift);
        end;
      end;
    end;

    // Request to have CTRL pressed at the same time.
    // VK_END alone reserved to get to end position of edtSearch.
    VK_END:
    begin
      if ssCtrl in Shift then
      begin
        Key := 0;

        if Assigned(Self.OnChangeSearch) then
        begin
          Options.Direction := qsdLast;
          Self.OnChangeSearch(Self, edtSearch.Text, Options, ssShift in Shift);
        end;
      end;
    end;

    VK_INSERT:
    begin
      if Shift = [] then // no modifiers pressed, to not capture Ctrl+Insert and Shift+Insert
      begin
        Key := 0;

        if Assigned(Self.OnChangeSearch) then
        begin
          Options.Direction := qsdNext;
          Self.OnChangeSearch(Self, edtSearch.Text, Options, True);
        end;
      end;
    end;

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

      DoHide;
    end;

    VK_ESCAPE:
    begin
      Key := 0;

      CancelFilter;
    end;
  end;
end;

procedure TfrmQuickSearch.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    if FNeedsChangeSearch then
      DoOnChangeSearch;
  end;
end;

procedure TfrmQuickSearch.DoHide;
begin
  if Assigned(Self.OnHide) then
    Self.OnHide(Self);
end;

procedure TfrmQuickSearch.FrameExit(Sender: TObject);
begin
{$IF DEFINED(LCLQT) or DEFINED(LCLQT5)}
  // Workaround: QuickSearch frame lose focus on SpeedButton click
  if Screen.ActiveControl is TFileView then
    edtSearch.SetFocus
  else
{$ENDIF}
  if not Finalizing then
  begin
    Finalizing := True;

    Self.Active := False;

    if (Mode = qsFilter) and (edtSearch.Text <> EmptyStr) then
      Self.Visible := not gQuickFilterAutoHide
    else
      Finalize;

    Finalizing := False;
  end;
end;

procedure TfrmQuickSearch.sbCaseSensitiveClick(Sender: TObject);
begin
  if sbCaseSensitive.Down then
    Options.SearchCase := qscSensitive
  else
    Options.SearchCase := qscInsensitive;

  if gQuickFilterSaveSessionModifications then gQuickSearchOptions.SearchCase := Options.SearchCase;

  Options.Direction := qsdNone;

  DoOnChangeSearch;
end;

procedure TfrmQuickSearch.sbFilesAndDirectoriesClick(Sender: TObject);
begin
  if sbFiles.Down and sbDirectories.Down then
    Options.Items := qsiFilesAndDirectories
  else if sbFiles.Down then
    Options.Items := qsiFiles
  else if sbDirectories.Down then
    Options.Items := qsiDirectories
  else if FUpdateCount = 0 then
  begin
    CheckFilesOrDirectoriesDown;
    Exit;
  end;

  if gQuickFilterSaveSessionModifications then gQuickSearchOptions.Items := Options.Items;

  Options.Direction := qsdNone;

  DoOnChangeSearch;
end;

procedure TfrmQuickSearch.sbMatchBeginningClick(Sender: TObject);
begin
  if sbMatchBeginning.Down then
    Include(Options.Match, qsmBeginning)
  else
    Exclude(Options.Match, qsmBeginning);

  if gQuickFilterSaveSessionModifications then gQuickSearchOptions.Match := Options.Match;

  Options.Direction := qsdNone;

  DoOnChangeSearch;
end;

procedure TfrmQuickSearch.sbMatchEndingClick(Sender: TObject);
begin
  if sbMatchEnding.Down then
    Include(Options.Match, qsmEnding)
  else
    Exclude(Options.Match, qsmEnding);

  if gQuickFilterSaveSessionModifications then gQuickSearchOptions.Match := Options.Match;

  Options.Direction := qsdNone;

  DoOnChangeSearch;
end;

procedure TfrmQuickSearch.tglFilterChange(Sender: TObject);
begin
  Options.LastSearchMode := qsNone;
  if tglFilter.Checked then
    Mode := qsFilter
  else
    Mode := qsSearch;

  // if a filter was set in background and a search is opened, the filter
  // will get pushed staying active. Otherwise the filter will be converted
  // in a search
  if not Active and (Mode = qsSearch) then
    PushFilter
  else if Active then
    ClearFilter;

  Options.Direction := qsdNone;

  DoOnChangeSearch;
end;

procedure TfrmQuickSearch.btnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  edtSearch.SetFocus;
end;

procedure TfrmQuickSearch.btnCancelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Self.Visible then
    edtSearch.SetFocus;
end;

end.

