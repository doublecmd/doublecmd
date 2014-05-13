unit GifAnimDsgn;

{$mode objfpc}{$H+}

interface

uses
  LazIDEIntf, PropEdits;

Type
  TGifFileNamePropertyEditor = class(TFileNamePropertyEditor)
  protected
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;

procedure Register;

implementation

uses
  SysUtils, GifAnim;

function TGifFileNamePropertyEditor.GetFilter: String;
begin
  Result := 'GIF|*.gif';
end;

function TGifFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(String), TGifAnim,
                         'FileName', TGifFileNamePropertyEditor);
end;

end.

