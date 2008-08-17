{
   Seksi Commander
   ----------------------------
   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   thread for counting files a dir (if Space pressed)

   contributors:

   Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)
}


unit uSpaceThread;

interface

uses
  Classes, uFileOpThread, uFileList;

type
  TSpaceThread = class(TFileOpThread)
  private
    FDisplayMessage: Boolean;
  protected
    procedure MainExecute; override;
  public
    constructor Create(aFileList:TFileList; bDisplayMessage: Boolean);
    function UseForm:Boolean; override;
    function FreeAtEnd:Boolean; override;
    property DirCount:Integer read FDirCount;
    property FilesCount:Integer read FFilesCount;
    property FilesSize: Int64 read FFilesSize;
  end;

implementation
uses
  SysUtils, uLng, uShowMsg;

{ TSpaceThread }

procedure TSpaceThread.MainExecute;
begin
  if FDisplayMessage then
    msgOK(Self, Format(rsSpaceMsg,[FilesCount, DirCount, FilesSize]));
end;

constructor TSpaceThread.Create(aFileList: TFileList; bDisplayMessage: Boolean);
begin
  FDisplayMessage:= bDisplayMessage;
  inherited Create(aFileList);
end;

function TSpaceThread.UseForm:Boolean;
begin
  Result:= False;
end;

function TSpaceThread.FreeAtEnd:Boolean;
begin
  Result:= FDisplayMessage;
end;

end.
