{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit cmdbox; 

interface

uses
  uCmdBox, LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('uCmdBox', @uCmdBox.Register); 
end; 

initialization
  RegisterPackage('cmdbox', @Register); 
end.
