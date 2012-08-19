unit vtlogger;

{$mode objfpc}{$H+}

interface

uses
  multiloglcl, multilog;

const
  //lc stands for LogClass
  //it's possible to define the constants to suit any need
  lcAll = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31];
  lcDebug = 0;
  lcError = 1;
  lcInfo = 2;
  lcWarning = 3;
  
  lcEvents = 4;
  lcPaint = 5;
  lcPaintHeader = 6;
  lcDummyFunctions = 7;
  lcMessages = 8;
  lcPaintSelection = 9;
  lcSetCursor = 10;//it generates a lot of messages. so it will be debugged alone
  lcPaintBitmap = 11;
  lcScroll = 12;
  lcPaintDetails = 13;
  lcCheck = 14;
  lcEditLink = 15;
  lcEraseBkgnd = 16;
  lcColumnPosition = 17;
  lcTimer = 18;
  lcDrag = 19;
  lcOle = 20;
  lcPanning = 21;
  lcHeaderOffset = 22;
  lcSelection = 23;
  lcAlphaBlend = 24;
  lcHint = 25;
  lcMouseEvent = 26;
  
var
  Logger: TLCLLogger;


  function GetSelectedNodes(Sender: TLogger; Data: Pointer; var DoSend: Boolean): String;
  
implementation

uses
  VirtualTrees, sysutils;
  
type
  TNodeData = record
    Title: String;
  end;
  PNodeData = ^TNodeData;

  function GetSelectedNodes(Sender: TLogger; Data: Pointer; var DoSend: Boolean): String;
  var
    i: Integer;
    TempNode: PVirtualNode;
  begin
    with TBaseVirtualTree(Data) do
    begin
      Result:='SelectedCount: '+IntToStr(SelectedCount)+LineEnding;
      TempNode:=GetFirstSelected;
      if TempNode = nil then exit;
      Result:=Result+PNodeData(GetNodeData(TempNode))^.Title+LineEnding;
      for i:= 1 to SelectedCount -1 do
      begin
        TempNode:=GetNextSelected(TempNode);
        Result:=Result+PNodeData(GetNodeData(TempNode))^.Title+LineEnding;
      end;
    end;
  end;


initialization
  Logger:=TLCLLogger.Create;
finalization
  Logger.Free;
end.

