{
Seksi Commander
----------------------------
Licence  : GNU GPL v 2.0
Author   : radek.cervinka@centrum.cz

thread for counting files a dir (if Space pressed)

contributors:

}


unit uSpaceThread;

interface

uses
  Classes, uFileOpThread;

type
  TSpaceThread = class(TFileOpThread)
  private
    { Private declarations }
  protected
    procedure MainExecute; override;
  public
    function UseForm:Boolean; override;
    function FreeAtEnd:Boolean; override;
    property DirCount:Integer read FDirCount;
    property FilesCount:Integer read FFilesCount;
    property FilesSize: Int64 read FFilesSize;
  end;

implementation

{ TSpaceThread }

procedure TSpaceThread.MainExecute;
begin
  { Place thread code here }
end;

function TSpaceThread.UseForm:Boolean;
begin
  Result:=False;
end;

function TSpaceThread.FreeAtEnd:Boolean;
begin
  Result:=False;
end;


end.
