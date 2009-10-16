unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  uFile, uFileSource, uMethodsList;

type

  TOnBeforeChangeDirectory = function (FileView: TCustomPage; const NewDir : String): Boolean of object;
  TOnAfterChangeDirectory = procedure (FileView: TCustomPage; const NewDir : String) of object;
  TOnChangeFileSource = procedure (FileView: TCustomPage) of object;

  {en
     Base class for any view of a file or files.
     There should always be at least one file displayed on the view.
  }
  TFileView = class(TWinControl)
  private
    {en
       The file sources hierarchy associated with this view.
       Last element is the file source that is currently being viewed,
       parent file source is (index-1) and so on up to zero (first file source).

       For now they all live as long as TFileView lives,
       don't know if this should be changed or not.
    }
    FFileSources: TFileSources;

    FMethods: TMethodsList;

    FOnBeforeChangeDirectory : TOnBeforeChangeDirectory;
    FOnAfterChangeDirectory : TOnAfterChangeDirectory;
    FOnChangeFileSource : TOnChangeFileSource;

    function GetCurrentAddress: String;

    function GetNotebookPage: TCustomPage;

    function GetLastFileSource: TFileSource;
    function GetFileSource(Index: Integer): TFileSource;
    function GetFileSourcesCount: Integer;

  protected
    function GetCurrentPath: String; virtual;
    procedure SetCurrentPath(NewPath: String); virtual;
    function GetActiveFile: TFile; virtual;
    function GetDisplayedFiles: TFiles; virtual abstract;
    function GetSelectedFiles: TFiles; virtual abstract;

  public
    constructor Create(AOwner: TWinControl;
                       FileSource: TFileSource); virtual reintroduce;

    destructor Destroy; override;

    function Clone(NewParent: TWinControl): TFileView; virtual;
    procedure CloneTo(FileView: TFileView); virtual;

    procedure AddFileSource(aFileSource: TFileSource); virtual;
    procedure RemoveLastFileSource; virtual;
    {en
       Sets the list of file sources so that each file source is a clone
       of the file source from 'otherFileSources'.
    }
    procedure AssignFileSources(otherFileSources: TFileSources); virtual;

    // Retrieves files from file source again and displays the new list of files.
    procedure Reload; virtual abstract;

    // For now we use here the knowledge that there are tabs.
    // Config should be independent of that in the future.
    procedure LoadConfiguration(Section: String; TabIndex: Integer); virtual abstract;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); virtual abstract;

    procedure UpdateView; virtual abstract;

    procedure ExecuteCommand(CommandName: String; Parameter: String = ''); virtual;

    {en
       Returns @true if at least one file is somehow selected.
       What "selected" means depends on the concrete file view implementation.
       (Usually it will be a different method of selecting than ActiveFile.)
    }
    function HasSelectedFiles: Boolean; virtual abstract;

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property CurrentAddress: String read GetCurrentAddress;
    property FileSource: TFileSource read GetLastFileSource;
    property FileSources[Index: Integer]: TFileSource read GetFileSource;
    property FileSourcesList: TFileSources read FFileSources;
    property FileSourcesCount: Integer read GetFileSourcesCount;

    {en
       Currently active file.
       There should always be at least one file in the view at any time, but
       what 'active' means depends on the specific view, so ActiveFile may
       return 'nil' if there is no file active. Usually it is the file pointed
       to by a cursor or some other indicator.
    }
    property ActiveFile: TFile read GetActiveFile;
    {en
       A list of currently displayed files.
       Caller is responsible for freeing the list.
    }
    property Files: TFiles read GetDisplayedFiles;
    {en
       A list of files selected by the user
       (this should be a subset of displayed files list returned by Files).
       Caller is responsible for freeing the list.
    }
    property SelectedFiles: TFiles read GetSelectedFiles;

    property NotebookPage: TCustomPage read GetNotebookPage;
    property OnBeforeChangeDirectory : TOnBeforeChangeDirectory read FOnBeforeChangeDirectory write FOnBeforeChangeDirectory;
    property OnAfterChangeDirectory : TOnAfterChangeDirectory read FOnAfterChangeDirectory write FOnAfterChangeDirectory;
    property OnChangeFileSource : TOnChangeFileSource read FOnChangeFileSource write FOnChangeFileSource;
  end;

implementation

uses
  uActs, LCLProc;

constructor TFileView.Create(AOwner: TWinControl; FileSource: TFileSource);
begin
  FOnBeforeChangeDirectory := nil;
  FOnAfterChangeDirectory := nil;
  FOnChangeFileSource := nil;

  FFileSources := TFileSources.Create(True); // True = Free objects on destroy.
  FFileSources.Add(FileSource);
  FMethods := TMethodsList.Create(Self);

  inherited Create(AOwner);
end;

destructor TFileView.Destroy;
begin
  inherited;
  FreeAndNil(FMethods);
  FreeAndNil(FFileSources);
end;

function TFileView.Clone(NewParent: TWinControl): TFileView;
begin
  raise Exception.Create('Cannot create object of abstract class');
end;

procedure TFileView.CloneTo(FileView: TFileView);
var
  i: Integer;
begin
  if Assigned(FileView) then
  begin
    // FFileSource should have been passed to FileView constructor already.
    // FMethods are created in FileView constructor.
    FileView.OnBeforeChangeDirectory := OnBeforeChangeDirectory;
    FileView.OnAfterChangeDirectory := OnAfterChangeDirectory;

    for i := 0 to FFileSources.Count - 1 do
    begin
      FileView.FFileSources.Add(FileSources[i].Clone);
    end;
  end;
end;

function TFileView.GetNotebookPage: TCustomPage;
begin
  Result := Parent as TCustomPage;
end;

function TFileView.GetCurrentAddress: String;
begin
  Result := FileSource.CurrentAddress;
end;

function TFileView.GetCurrentPath: String;
begin
  Result := FileSource.CurrentPath;
end;

procedure TFileView.SetCurrentPath(NewPath: String);
begin
  FileSource.CurrentPath := NewPath;
end;

function TFileView.GetActiveFile: TFile;
begin
  Result := nil;
end;

procedure TFileView.ExecuteCommand(CommandName: String; Parameter: String);
var
  Method: TMethod;
begin
  Method := FMethods.GetMethod(CommandName);
  if Assigned(Method.Code) then
  begin
    // Command is supported - execute it.
    TCommandFunc(Method)(Parameter);
  end;
end;

procedure TFileView.AddFileSource(aFileSource: TFileSource);
begin
  FFileSources.Add(aFileSource);
  Reload;
  UpdateView;
end;

procedure TFileView.RemoveLastFileSource;
begin
  FFileSources.Delete(FFileSources.Count - 1);
  Reload;
  UpdateView;
end;

procedure TFileView.AssignFileSources(otherFileSources: TFileSources);
var
  i: Integer;
begin
  FFileSources.Clear;
  for i := 0 to otherFileSources.Count - 1 do
    FFileSources.Add(otherFileSources.Items[i].Clone);
  Reload;
  UpdateView;
end;

function TFileView.GetLastFileSource: TFileSource;
begin
  if FFileSources.Count > 0 then
    Result := FFileSources.Last as TFileSource
  else
    Result := nil;
end;

function TFileView.GetFileSource(Index: Integer): TFileSource;
begin
  Result := FFileSources.Items[Index] as TFileSource;
end;

function TFileView.GetFileSourcesCount: Integer;
begin
  Result := FFileSources.Count;
end;

end.

