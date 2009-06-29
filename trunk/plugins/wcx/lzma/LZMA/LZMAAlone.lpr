program LZMAAlone;

{$MODE Delphi}

uses
  UCRC in 'UCRC.pas',
  ULZBinTree in 'compression\LZ\ULZBinTree.pas',
  ULZInWindow in 'compression\LZ\ULZInWindow.pas',
  ULZOutWindow in 'compression\LZ\ULZOutWindow.pas',
  ULZMABase in 'compression\LZMA\ULZMABase.pas',
  ULZMACommon in 'compression\LZMA\ULZMACommon.pas',
  ULZMADecoder in 'compression\LZMA\ULZMADecoder.pas',
  ULZMAEncoder in 'compression\LZMA\ULZMAEncoder.pas',
  UBitTreeDecoder in 'compression\RangeCoder\UBitTreeDecoder.pas',
  UBitTreeEncoder in 'compression\RangeCoder\UBitTreeEncoder.pas',
  URangeDecoder in 'compression\RangeCoder\URangeDecoder.pas',
  URangeEncoder in 'compression\RangeCoder\URangeEncoder.pas',
  UBufferedFS in 'UBufferedFS.pas',
  ULZMAAlone in 'ULZMAAlone.pas',
  ULZMABench in 'ULZMABench.pas',SysUtils;

var lz:TLZMAAlone;

{$IFDEF MSWINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

begin
try
   lz:=TLZMAAlone.Create;
   lz.Main;
   lz.Free;
   except on e:exception do
          writeln(e.message);
          end;
end.
