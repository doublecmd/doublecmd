unit uSampleForConfigListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource,
  uSampleForConfigFileSource;
type

  TSampleForConfigListOperation = class(TFileSourceListOperation)
  private
    FFileSource: ISampleForConfigFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: string); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uFile;
constructor TSampleForConfigListOperation.Create(aFileSource: IFileSource; aPath: string);
begin
  FFiles := TFiles.Create(aPath);
  FFileSource := aFileSource as ISampleForConfigFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TSampleForConfigListOperation.MainExecute;
var
  FakeFile: TFile;
  IndexFile: integer;
const
  BaseName: array[0..11] of string = ('config', 'Step', 'Prog', 'setup', 'Report', 'Skip', 'Closer', 'Face', 'Win', 'Unix', 'App', 'Klopp');
  SuffixName: array[0..11] of string = ('red', 'new', 'fst', 'South', 'slow', 'Cheap', 'dc', 'config', 'stop', 'Batch', 'Bash', 'rgctvcvt');
  ExtName: array[0..11] of string = ('bin', 'exe', 'txt', 's19', 'Rar', 'zip', 'xlsx', 'pdf', 'cpp', 'pas', 'DPR', 'tmp');

begin
  FFiles.Clear;

  randseed:=Trunc(now); // Random from a day to another, but not during the day. So during the day, user will do refresh and always the same thing is re-shown.

  for Indexfile := 1 to 30 do
  begin
    FakeFile := TSampleForConfigFileSource.CreateFile(SAMPLE_PATH);
    FakeFile.Name := BaseName[random(12)] + SuffixName[random(12)] + '.' + ExtName[random(12)];
    FakeFile.Size := 5000 + random(1000000);
    FFiles.Add(FakeFile);
  end;
end;



end.




