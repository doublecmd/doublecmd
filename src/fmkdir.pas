unit fMkDir;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type

  { TfrmMkDir }

  TfrmMkDir = class(TForm)
    cbMkDir: TComboBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    lblMakeDir: TLabel;
    procedure ButtonsClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
   function ShowMkDir(var sPath:string):Boolean;  // 21.05.2009 - перенес в public
  end;

var  frmMkDir: TfrmMkDir;  // 21.05.2009  - создаем из файла проекта

implementation
//uses      // 21.05.2009 - закомментировал неиспользуемый
//  uLng;

procedure TfrmMkDir.FormKeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  if Key=#27 then
  begin
    ModalResult:=mrCancel;
    Key := #0;
  end
  else if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

procedure TfrmMkDir.ButtonsClick(Sender: TObject);
begin
 cbMkDir.SetFocus; // 21.05.2009 - если нажимаем на кнопки мышью, то возвращаем фокус в поле ввода.
end;

// 21.05.2009 - функция переписана по подобию тотала.
Function TfrmMkDir.ShowMkDir(var sPath:string):Boolean;
 var i : byte;
begin
{
  Теперь форма создается при запуске программы и уничтожается при закрытии.
  Нужно для сохранения значений в списке.
}
//  with TfrmMkDir.Create(Application) do
 try
  If (sPath <> '..') then cbMkDir.Text := sPath
  else cbMkDir.Text := '';      // передаем имя из фокуса панели в строку ввода, если не фокус не в '..'
  Result:= (ShowModal = mrOK);  // отображение окна
  sPath:=Trim(cbMkDir.Text);
{
  Добавляем созданный каталог в список.
  Просматриваем список - если такое имя уже было, то не добавляем.
  Пока сделал просмотр вручную, наверно должно быть свойство!
}
  If Result then                       // если mrOK
   begin
    i:=0;
    while (i <= cbMkDir.Items.Count - 1) and (cbMkDir.Items.Strings[i] <> cbMkDir.Text) do
     Inc(i);                           // то просмотр списка и поиск в нем такого элемента
    if (i > cbMkDir.Items.Count - 1) then
     cbMkDir.Items.Add(cbMkDir.Text);  // если такой элемент не найден, то добавляем его.
   end;
  finally
//    Free;  -- перенес в TActs.cm_Exit
  end;
end;

initialization
 {$I fmkdir.lrs}

end.
