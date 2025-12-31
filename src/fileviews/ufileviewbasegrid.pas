unit uFileViewBaseGrid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSource, uFileView, uFileViewWithMainCtrl,
  uSmoothScrollingGrid;

type

  { TFileViewBaseGrid }

  TFileViewBaseGrid = class( TSmoothScrollingGrid )
  protected
    _onDrawCell: TFileViewOnDrawCell;
    property OnDrawCell: TFileViewOnDrawCell read _onDrawCell write _onDrawCell;
  protected
    procedure doCellEnhancedDraw( var params: TFileSourceUIParams );
    function doCellClick( const Shift: TShiftState; const X, Y: Integer ): Boolean;
  protected
    function getFileView: TFileView; virtual; abstract;
    function isMultiColumns: Boolean; virtual;
    function MouseOnGrid(X, Y: LongInt): Boolean;
    procedure MouseToCellWithoutOutbound(X, Y: Integer; out ACol, ARow: Longint);
    function ConvertToDecorationRect(const drawingRect: TRect): TRect; virtual;
    function CellToIndex(ACol, ARow: Integer): Integer; virtual; abstract;
  public
    function calcTextHeight: Integer; inline;
  end;

implementation

{ TFileViewBaseGrid }

function TFileViewBaseGrid.doCellClick( const Shift: TShiftState; const X, Y: Integer ): Boolean;
var
  fileView: TFileView;
  handler: TFileSourceUIHandler;
  params: TFileSourceUIParams;
  index: Integer;
begin
  Result:= False;
  fileView:= self.getFileView;

  params:= Default( TFileSourceUIParams );
  params.sender:= fileView;
  params.fs:= fileView.FileSource;
  params.multiColumns:= self.isMultiColumns;

  handler:= params.fs.GetUIHandler;
  if handler = nil then
    Exit;

  params.shift:= Shift;
  params.x:= X;
  params.y:= Y;
  self.MouseToCellWithoutOutbound( X, Y, params.col, params.row );
  index:= self.CellToIndex( params.col, params.row );
  if index < 0 then
    Exit;

  self.ColRowToOffset(True, True, params.col, params.drawingRect.Left, params.drawingRect.Right );
  self.ColRowToOffset(False, True, params.row, params.drawingRect.Top, params.drawingRect.Bottom );
  params.decorationRect:= self.ConvertToDecorationRect( params.drawingRect );

  params.displayFile:= fileView.DisplayFiles[index];
  Result:= handler.click( params );
end;

procedure TFileViewBaseGrid.doCellEnhancedDraw( var params: TFileSourceUIParams );

  procedure doFileSourceDrawCell( var params: TFileSourceUIParams );
  var
    handler: TFileSourceUIHandler;
  begin
    handler:= params.fs.GetUIHandler;
    if handler = nil then
      Exit;

    handler.draw( params );
  end;

  procedure doOnDrawCell( var params: TFileSourceUIParams );
  begin
    if Assigned(_onDrawCell) and not(CsDesigning in self.ComponentState) then
      _onDrawCell( params );
  end;

var
  fileView: TFileView;
begin
  fileView:= self.getFileView;
  params.sender:= fileView;
  params.fs:= fileView.FileSource;
  params.multiColumns:= self.isMultiColumns;
  params.decorationRect:= self.ConvertToDecorationRect( params.drawingRect );
  doFileSourceDrawCell( params );
  doOnDrawCell( params );
end;

function TFileViewBaseGrid.MouseOnGrid(X, Y: LongInt): Boolean;
var
  bTemp: Boolean;
  iRow, iCol: LongInt;
begin
  bTemp:= AllowOutboundEvents;
  AllowOutboundEvents:= False;
  MouseToCell(X, Y, iCol, iRow);
  AllowOutboundEvents:= bTemp;
  Result:= not (CellToIndex(iCol, iRow) < 0);
end;

procedure TFileViewBaseGrid.MouseToCellWithoutOutbound(X, Y: Integer; out ACol,
  ARow: Longint);
var
  bTemp: Boolean;
begin
  bTemp:= AllowOutboundEvents;
  AllowOutboundEvents:= False;
  MouseToCell(X, Y, ACol, ARow);
  AllowOutboundEvents:= bTemp;
end;

function TFileViewBaseGrid.ConvertToDecorationRect(const drawingRect: TRect ): TRect;
begin
  Result:= drawingRect;
end;

function TFileViewBaseGrid.calcTextHeight: Integer;
begin
  Result:= self.Canvas.TextHeight('Wg');
end;

function TFileViewBaseGrid.isMultiColumns: Boolean;
begin
  Result:= False;
end;

end.

