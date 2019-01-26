{
   Double commander
   -------------------------------------------------------------------------
   Executes file operations with administrator privileges

   Copyright (C) 2016-2019 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uAdministrator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, uFileSystemFileSource, uFile,
  uFileSystemDeleteOperation;

type
  EAccessDenied = class(Exception);

procedure ExecuteOperation(const FileList: String);

procedure Delete(Helper: TFileSystemDeleteOperation; Files: TFiles; Index: Integer);

implementation

uses
  uFileSource, uFileSourceOperationMessageBoxesUI, dmCommonData, uOSUtils,
  uGlobs, uGlobsPaths, uOperationsManager, uFileSourceOperation, DCXmlConfig,
  uFileSourceOperationOptions, uSpecialDir, DCOSUtils,
  uDCUtils;

type

  { TDummy }

  TDummy = class
    procedure Finish(Operation: TFileSourceOperation; State: TFileSourceOperationState);
  end;

var
  Dummy: TDummy;
  FFileSourceOperationMessageBoxesUI: TFileSourceOperationMessageBoxesUI;

procedure Initialize;
begin
  LoadPaths;
  LoadWindowsSpecialDir;
  InitGlobs;
  Dummy:= TDummy.Create;
  Application.CreateForm(TdmComData, dmComData); // common data
  FFileSourceOperationMessageBoxesUI := TFileSourceOperationMessageBoxesUI.Create;
end;

procedure Delete(Xml: TXmlConfig);
var
  Files: TFiles;
  ANode: TXmlNode;
  FileSource: IFileSource;
  Operation: TFileSystemDeleteOperation;
begin
  ANode:= Xml.FindNode(Xml.RootNode, 'FileList');

  if Assigned(ANode) then
  begin
    Files:= TFiles.Create(Xml.GetAttr(aNode, 'Path', EmptyStr));
    ANode:= ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('Item') = 0 then
      begin
        Files.Add(TFileSystemFileSource.CreateFileFromFile(Xml.GetAttr(aNode, 'Name', EmptyStr)));
      end;
      ANode:= ANode.NextSibling;
    end;

    FileSource:= TFileSystemFileSource.GetFileSource;
    Operation:= FileSource.CreateDeleteOperation(Files) as TFileSystemDeleteOperation;

    Operation.SkipErrors:= False;
    Operation.Recycle:= Xml.GetValue(Xml.RootNode, 'Recycle', False);
    Operation.SymLinkOption:= TFileSourceOperationOptionSymLink(Xml.GetValue(Xml.RootNode, 'SymLinkOption', Integer(fsooslNone)));
    Operation.DeleteReadOnly:= TFileSourceOperationOptionGeneral(Xml.GetValue(Xml.RootNode, 'DeleteReadOnly', Integer(fsoogNone)));

    Operation.AddUserInterface(FFileSourceOperationMessageBoxesUI);
    Operation.AddStateChangedListener([fsosStopped], @Dummy.Finish);
    OperationsManager.AddOperation(Operation, True);
  end;
end;

procedure Delete(Helper: TFileSystemDeleteOperation; Files: TFiles; Index: Integer);
var
  I: Integer;
  Xml: TXmlConfig;
  ANode: TXmlNode;
  SubNode: TXmlNode;
  FileName: String;
begin
  FileName:= GetTempFileName;
  Xml:= TXmlConfig.Create(FileName);
  Xml.SetValue(Xml.RootNode, 'Operation', 0);
  Xml.SetValue(Xml.RootNode, 'Recycle', Helper.Recycle);
  Xml.SetValue(Xml.RootNode, 'DeleteReadOnly', Integer(Helper.DeleteReadOnly));
  Xml.SetValue(Xml.RootNode, 'SymLinkOption', Integer(Helper.SymLinkOption));

  ANode := Xml.AddNode(Xml.RootNode, 'FileList');
  Xml.SetAttr(ANode, 'Path', Files.Path);
  for I:= Index to Files.Count - 1 do
  begin
    SubNode := Xml.AddNode(ANode, 'Item');
    Xml.SetAttr(SubNode, 'Name', Files[I].FullPath);
  end;
  Xml.Save;
  Xml.Free;

  ExecCmdAdmin(ParamStrU(0), '--config-dir=' + QuoteStr(gpCfgDir) +
                             ' ' +
                             '--operation=' + QuoteStr(FileName));
end;

procedure ExecuteOperation(const FileList: String);
var
  Xml: TXmlConfig;
begin
  try
    Initialize;

    Xml:= TXmlConfig.Create(FileList, True);
    try
      case Xml.GetValue(Xml.RootNode, 'Operation', -1) of
        0: Delete(Xml);
      end;
    finally
      Xml.Free;
      mbDeleteFile(FileList);
    end;

    Application.Run;

    Halt(0);
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
      Halt(1);
    end;
  end;
end;

{ TDummy }

procedure TDummy.Finish(Operation: TFileSourceOperation; State: TFileSourceOperationState);
begin
  Application.Terminate;
end;

end.
