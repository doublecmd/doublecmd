unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  uFile, uFileSource, uFilePanelSelect, uMethodsList;

type

  {en
     Base class for any view of a file or files.
     There should always be at least one file displayed on the view.
  }
  TFileView = class(TWinControl)
  private
    {en
       The file source associated with this view.

       For now it lives as long as TFileView lives (it is freed in destructor).
       Don't know if this should be changed or not.
    }
    FFileSource: TFileSource;

    // It should be independent of left/right side in the future.
    FPanelSelect: TFilePanelSelect;

    FMethods: TMethodsList;

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

    // Retrieves files from file source again and displays the new list of files.
    procedure Reload; virtual abstract;

    // For now we use here the knowledge that there are tabs.
    // Config should be independent of that in the future.
    procedure LoadConfiguration(Section: String; TabIndex: Integer); virtual abstract;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); virtual abstract;

    procedure UpdateView; virtual abstract;

    procedure ExecuteCommand(CommandName: String; Parameter: String); virtual;

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property FileSource: TFileSource read FFileSource write FFileSource;
    property PanelSelect: TFilePanelSelect read FPanelSelect write FPanelSelect;
    {en
       Currently active file.
       There should always be at least one file in the view at any time, but
       what 'active' means depends on the specific view, so ActiveFile may
       return 'nil' if there is no file active.
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
  end;

implementation

uses
  uOSUtils, uActs, LCLProc;

constructor TFileView.Create(AOwner: TWinControl; FileSource: TFileSource);
begin
  FFileSource := FileSource;
  FMethods := TMethodsList.Create(Self);
  inherited Create(AOwner);
end;

destructor TFileView.Destroy;
begin
  inherited;
  FreeAndNil(FFileSource);
  FreeAndNil(FMethods);
end;

function TFileView.GetCurrentPath: String;
begin
  Result := IncludeTrailingPathDelimiter(  // trailing path delim needed?
              FFileSource.CurrentPath);
end;

procedure TFileView.SetCurrentPath(NewPath: String);
begin
  FFileSource.CurrentPath := NewPath;
end;

function TFileView.GetActiveFile: TFile;
begin
  Result := nil;
end;

procedure TFileView.ExecuteCommand(CommandName: String; Parameter: String);
var
  Method: TMethod;
  Result: Integer;
begin
  Method := FMethods.GetMethod(CommandName);
  if Assigned(Method.Code) then
  begin
    // Command is supported - execute it.
    TCommandFunc(Method)(Parameter);
  end;
end;

end.

