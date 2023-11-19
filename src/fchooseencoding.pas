unit fChooseEncoding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;

type

  { TfrmChooseEncoding }

  TfrmChooseEncoding = class(TForm)
    ButtonPanel: TButtonPanel;
    ScrollBox: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
  private
    FList: TStrings;
  public
    constructor Create(TheOwner: TComponent; AList: TStrings); reintroduce;
    destructor Destroy; override;
  end;

function ChooseEncoding(TheOwner: TComponent; AList: TStrings): Boolean;

implementation

uses
  uConvEncoding;

function ChooseEncoding(TheOwner: TComponent; AList: TStrings): Boolean;
begin
  with TfrmChooseEncoding.Create(TheOwner, AList) do
  try
    Result:= (ShowModal = mrOK);
    if Result then AList.Assign(FList);
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmChooseEncoding }

procedure TfrmChooseEncoding.CheckBoxChange(Sender: TObject);
begin
  with TCheckBox(Sender) do
  begin
    FList.Objects[Tag]:= TObject(PtrInt(Checked));
  end;
end;

constructor TfrmChooseEncoding.Create(TheOwner: TComponent; AList: TStrings);
begin
  inherited Create(TheOwner);
  FList:= TStringList.Create;
  FList.Assign(AList);
end;

destructor TfrmChooseEncoding.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

procedure TfrmChooseEncoding.FormCreate(Sender: TObject);
var
  Index: Integer;
  CheckBox: TCheckBox;
begin
  for Index:= 0 to FList.Count - 1 do
  begin
    CheckBox:= TCheckBox.Create(Self);
    CheckBox.Parent:= ScrollBox;
    CheckBox.Caption:= FList[Index];
    CheckBox.Tag:= Index;
    CheckBox.OnChange:= @CheckBoxChange;
    CheckBox.Checked:= Boolean(PtrInt(FList.Objects[Index]));
  end;
end;

end.

