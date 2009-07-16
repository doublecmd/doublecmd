unit uFileView; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls,
  uFile, uFileSource, uFilePanelSelect, uMethodsList;

type

  {en
     Base class for any view of the files.
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

    // I'm not sure CurrentPath property should be allowed for abstract TFileView.
    // We have no guarantee that the FileSource associated with this view even
    // has something like a current path?
    property CurrentPath: String read GetCurrentPath write SetCurrentPath;

    property FileSource: TFileSource read FFileSource write FFileSource;
    property PanelSelect: TFilePanelSelect read FPanelSelect write FPanelSelect;
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

