{ Этот файл был автоматически создан Lazarus. Не редактировать!
Исходный код используется только для компиляции и установки пакета.
 }

unit KASComp; 

interface

uses
  KASEdit, KAStoolBar, KASBarMenu, KASBarFiles, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('KASEdit', @KASEdit.Register); 
  RegisterUnit('KAStoolBar', @KAStoolBar.Register); 
  RegisterUnit('KASBarMenu', @KASBarMenu.Register); 
end; 

initialization
  RegisterPackage('KASComp', @Register); 
end.
