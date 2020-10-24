unit fBenchmark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Grids, Contnrs, ButtonPanel, StdCtrls, uFile, uFileSourceOperation, uOSForms,
  uFileSourceCalcChecksumOperation;

type

  { TfrmBenchmark }

  TfrmBenchmark = class(TAloneForm)
    ButtonPanel: TButtonPanel;
    lblBenchmarkSize: TLabel;
    stgResult: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

  { TBenchmarkResult }

  TBenchmarkResult = class
    Hash: String;
    Time: QWord;
    Speed: Double;
  end;

  { TBenchmarkOperation }

  TBenchmarkOperation = class(TFileSourceCalcChecksumOperation)
  private
    FFiles: TFiles;
    FBuffer: TBytes;
    FOwner: TCustomForm;
    FSpeedResult: TObjectList;
    FStatistics: TFileSourceCalcChecksumOperationStatistics;
  protected
    procedure MainExecute; override;
    procedure OnBenchmarkStateChanged(Operation: TFileSourceOperation;
                                      AState: TFileSourceOperationState);
  public
    constructor Create(TheOwner: TCustomForm); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  ISAAC, DCOSUtils, uFileSystemFileSource, uHash, uGlobs, uDCUtils;

const
  cSize = 1024 * 1024 * 256;

function CompareFunc(Item1, Item2: Pointer): Integer;
begin
  if TBenchmarkResult(Item1).Time = TBenchmarkResult(Item2).Time then
    Result:=  0
  else if TBenchmarkResult(Item1).Time < TBenchmarkResult(Item2).Time then
    Result:= -1
  else begin
    Result:= +1;
  end;
end;

{ TfrmBenchmark }

procedure TfrmBenchmark.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

{ TBenchmarkOperation }

procedure TBenchmarkOperation.MainExecute;
var
  ASize: Int64;
  AHash: String;
  ARandom: isaac_ctx;
  ABufferSize: Integer;
  Context: THashContext;
  Index: THashAlgorithm;
  AStart, AFinish: QWord;
  AResult: TBenchmarkResult;
begin
  ABufferSize := gHashBlockSize;
  SetLength(FBuffer, ABufferSize);
  isaac_init(ARandom, Int32(GetTickCount64));
  isaac_read(ARandom, @FBuffer[0], ABufferSize);
  ASize:= (cSize div ABufferSize) * ABufferSize;
  FStatistics.TotalFiles := (Length(HashName) - 1);
  FStatistics.TotalBytes:= ASize * FStatistics.TotalFiles;

  for Index := Low(THashAlgorithm) to Pred(High(THashAlgorithm)) do
  begin
    if Index = HASH_SFV then Continue;

    with FStatistics do
    begin
      CurrentFile := HashName[Index];
      CurrentFileTotalBytes := ASize;
      CurrentFileDoneBytes := 0;
    end;

    UpdateStatistics(FStatistics);

    AStart:= GetTickCountEx;
    HashInit(Context, Index);

    while FStatistics.CurrentFileDoneBytes < ASize do
    begin
      HashUpdate(Context, FBuffer[0], ABufferSize);

      with FStatistics do
      begin
        CurrentFileDoneBytes := CurrentFileDoneBytes + ABufferSize;
        DoneBytes := DoneBytes + ABufferSize;

        UpdateStatistics(FStatistics);
      end;

      CheckOperationState; // check pause and stop
    end;

    HashFinal(Context, AHash);
    AFinish:= GetTickCountEx - AStart;

    Inc(FStatistics.DoneFiles);
    UpdateStatistics(FStatistics);

    AResult:= TBenchmarkResult.Create;

    AResult.Hash:= HashName[Index];
    AResult.Time:= AFinish;
    AResult.Speed:= (cSize / (1024 * 1024)) / (AFinish / 1000);

    FSpeedResult.Add(AResult);
  end;
  FSpeedResult.Sort(@CompareFunc);
end;

procedure TBenchmarkOperation.OnBenchmarkStateChanged(
  Operation: TFileSourceOperation; AState: TFileSourceOperationState);
var
  Index: Integer;
  AValue: TBenchmarkResult;
begin
  if (AState = fsosStopped) and (Operation.Result = fsorFinished) then
  begin
    with TfrmBenchmark.Create(FOwner) do
    begin
      stgResult.BeginUpdate;
      stgResult.RowCount:= FSpeedResult.Count + 1;
      try
        for Index:= 0 to FSpeedResult.Count - 1 do
        begin
          AValue:= TBenchmarkResult(FSpeedResult[Index]);
          stgResult.Cells[0, Index + 1]:= AValue.Hash;
          stgResult.Cells[1, Index + 1]:= IntToStr(AValue.Time);
          stgResult.Cells[2, Index + 1]:= FloatToStrF(AValue.Speed, ffFixed, 15, 3);
        end;
        FreeAndNil(FSpeedResult);
        lblBenchmarkSize.Caption:= Format(lblBenchmarkSize.Caption, [cSize div (1024 * 1024)]);
      finally
        stgResult.EndUpdate();
      end;
      Show;
    end;
  end;
end;

constructor TBenchmarkOperation.Create(TheOwner: TCustomForm);
begin
  FOwner:= TheOwner;
  inherited Create(TFileSystemFileSource.GetFileSource, FFiles, EmptyStr, EmptyStr);
  AddStateChangedListener([fsosStopped], @OnBenchmarkStateChanged);
  FSpeedResult:= TObjectList.Create;
  Mode:= checksum_calc;
end;

destructor TBenchmarkOperation.Destroy;
begin
  FSpeedResult.Free;
  inherited Destroy;
end;

{$R *.lfm}

end.

