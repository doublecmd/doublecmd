unit ULZMAAlone;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses ULZMABench,ULZMAEncoder,ULZMADecoder,UBufferedFS,ULZMACommon,Classes;

const kEncode=0;
      kDecode=1;
      kBenchmark=2;

type TCommandLine=class
       public
         command:integer;
         NumBenchMarkPasses:integer;
         DictionarySize:integer;
         DictionarySizeIsDefined:boolean;
         Lc:integer;
         Lp:integer;
         Pb:integer;

         Fb:integer;
         FbIsDefined:boolean;

         Eos:boolean;

         Algorithm:integer;
         MatchFinder:integer;

         InFile:string;
         OutFile:string;

         constructor Create;
         function ParseSwitch(const s:string):boolean;
         function Parse:boolean;
       end;

     TLZMAAlone=class
       public
         procedure PrintHelp;
         procedure Main;
       end;

implementation

uses SysUtils;

constructor TCommandLine.Create;
begin
Command:=-1;
NumBenchMarkPasses:=10;
DictionarySize:=1 shl 23;
DictionarySizeIsDefined:= false;
Lc:= 3;
Lp:= 0;
Pb:= 2;

Fb:= 128;
FbIsDefined:= false;

Eos:= false;

Algorithm:= 2;
MatchFinder:= 1;
end;

function GetStr(const str:string;const offset:integer):string;
var i:integer;
begin
result:='';
for i:=offset to length(str) do
    result:=result+str[i];
end;

function GetInt(const str:string;const offset:integer):integer;
var s:string;
begin
s:=GetStr(s,offset);
result:=strtoint(s);
end;

function TCommandLine.ParseSwitch(const s:string):boolean;
var l:integer;
    mfs:string;
begin
result:=false;
l:=length(s);
if l=0 then exit;
case s[1] of
     'd': begin
          DictionarySize := 1 shl GetInt(s,2);
          DictionarySizeIsDefined := true;
          result:=true;
          end;
     'f': begin
          if (l>=2)and(s[2]='b') then begin
             fb:=GetInt(s,3);
             FbIsDefined := true;
             result:=true;
             end;
          end;
     'a': begin
          Algorithm := GetInt(s,2);
          result:=true;
          end;
     'l': begin
          if (l>=2) then begin
             if s[2]='c' then begin
                Lc:=GetInt(s,3);
                result:=true;
                end;
             if s[2]='p' then begin
                Lp:=GetInt(s,3);
                result:=true;
                end;
             end;
          end;
     'p': begin
          if (l>=2)and(s[2]='b') then begin
             Pb:=GetInt(s,3);
             result:=true;
             end;
          end;
     'e': begin
          if (l>=3)and(s[2]='o')and(s[3]='s') then begin
             eos:=true;
             result:=true;
             end;
          end;
     'm': begin
          if (l>=2)and(s[2]='f') then begin
             mfs:=GetStr(s,3);
             if mfs='bt2' then
                MatchFinder:=0
             else if mfs='bt4' then
                  MatchFinder:=1
             else if mfs='bt4b' then
                  MatchFinder:=2
             else begin
                  result:=false;
                  exit;
                  end;
             end;
          end;
      else
        result:=false;
      end;
end;

function TCommandLine.Parse:boolean;
var pos:integer;
    switchMode:boolean;
    i,l:integer;
    s,sw:string;
begin
pos := 1;
switchMode := true;
l:=ParamCount;
for i := 1 to l do begin
    s := ParamStr(i);
    if length(s) = 0 then begin
       result:=false;
       exit;
       end;
    if switchMode then begin
       if comparestr(s,'--')= 0 then begin
          switchMode := false;
          continue;
          end;
       if s[1]='-' then begin
          sw := AnsiLowerCase(GetStr(s,2));
          if length(sw) = 0 then begin
             result:=false;
             exit;
             end;
          try
             if not ParseSwitch(sw) then begin
                result:=false;
                exit;
                end;
             except on e:EConvertError do begin
                    result:=false;
                    exit;
                    end;
             end;
          continue;
          end;
       end;
       if pos = 1 then begin
          if comparetext(s,'e')=0 then
             Command := kEncode
          else if comparetext(s,'d')=0 then
             Command := kDecode
          else if comparetext(s,'b')=0 then
             Command := kBenchmark
          else begin
               result:=false;
               exit;
               end;
          end
        else if pos = 2 then begin
             if Command = kBenchmark then begin
                try
                   NumBenchmarkPasses := strtoint(s);
                   if NumBenchmarkPasses < 1 then begin
                      result:=false;
                      exit;
                      end;
                   except on e:EConvertError do begin
                          result:=false;
                          exit;
                          end;
                   end;
                end
             else InFile := s;
             end
        else if pos = 3 then
             OutFile := s
        else begin
             result:=false;
             exit;
             end;
    inc(pos);
    continue;
    end;
result:=true;
exit;
end;

procedure TLZMAAlone.PrintHelp;
begin
writeln(
        #10'Usage:  LZMA <e|d> [<switches>...] inputFile outputFile'#10 +
        '  e: encode file'#10 +
        '  d: decode file'#10 +
        '  b: Benchmark'#10 +
        '<Switches>'#10 +
        // '  -a{N}:  set compression mode - [0, 1], default: 1 (max)\n' +
        '  -d{N}:  set dictionary - [0,28], default: 23 (8MB)'#10 +
        '  -fb{N}: set number of fast bytes - [5, 273], default: 128'#10 +
        '  -lc{N}: set number of literal context bits - [0, 8], default: 3'#10 +
        '  -lp{N}: set number of literal pos bits - [0, 4], default: 0'#10 +
        '  -pb{N}: set number of pos bits - [0, 4], default: 2'#10 +
        '  -mf{MF_ID}: set Match Finder: [bt2, bt4], default: bt4'#10 +
        '  -eos:   write End Of Stream marker'#10
        );
end;

procedure TLZMAAlone.Main;
var params:TCommandLine;
    dictionary:integer;
    lzmaBench:tlzmabench;
    inStream:TBufferedFS;
    outStream:TBufferedFS;
    eos:boolean;
    encoder:TLZMAEncoder;
    filesize:int64;
    i:integer;
    properties:array[0..4] of byte;
    decoder:TLZMADecoder;
    outSize:int64;
    v:byte;
const propertiessize=5;
begin                          
writeln(#10'LZMA (Pascal) 4.42 Copyright (c) 1999-2006 Igor Pavlov  2006-05-15'#10);
if paramcount<1 then begin
   PrintHelp;
   exit;
   end;
params:=TCommandLine.Create;
if not params.Parse then begin
   writeln(#10'Incorrect command');
   exit;
   end;
if params.command=kBenchmark then begin
   dictionary:=1 shl 21;
   if params.DictionarySizeIsDefined then
      dictionary:=params.DictionarySize;
   if params.MatchFinder>1 then
      raise Exception.Create('Unsupported match finder');
   lzmaBench:=TLZMABench.Create;
   lzmaBench.LzmaBenchmark(params.NumBenchMarkPasses,dictionary);
   lzmaBench.Free;
   end
   else if (params.command=kEncode)or(params.command=kDecode) then begin
        inStream:=TBufferedFS.Create(params.InFile,fmOpenRead or fmsharedenynone);
        outStream:=TBufferedFS.Create(params.OutFile,fmcreate);

        eos := false;
        if params.Eos then
           eos := true;
        if params.Command = kEncode then begin
           encoder:=TLZMAEncoder.Create;
           if not encoder.SetAlgorithm(params.Algorithm) then
              raise Exception.Create('Incorrect compression mode');
           if not encoder.SetDictionarySize(params.DictionarySize) then
              raise Exception.Create('Incorrect dictionary size');
           if not encoder.SeNumFastBytes(params.Fb) then
              raise Exception.Create('Incorrect -fb value');
           if not encoder.SetMatchFinder(params.MatchFinder) then
              raise Exception.Create('Incorrect -mf value');
           if not encoder.SetLcLpPb(params.Lc, params.Lp, params.Pb) then
              raise Exception.Create('Incorrect -lc or -lp or -pb value');
           encoder.SetEndMarkerMode(eos);
           encoder.WriteCoderProperties(outStream);
        if eos then fileSize := -1
           else fileSize := inStream.Size;
        for i := 0 to 7 do
            WriteByte(outStream,(fileSize shr (8 * i)) and $FF);
        encoder.Code(inStream, outStream, -1, -1);
        encoder.free;
        end
        else begin
             if inStream.read(properties, propertiesSize) <> propertiesSize then
                raise Exception.Create('input .lzma file is too short');
             decoder := TLZMADecoder.Create;
             if not decoder.SetDecoderProperties(properties) then
                raise Exception.Create('Incorrect stream properties');
             outSize := 0;
             for i := 0 to 7 do begin
                 v := {shortint}(ReadByte(inStream));
                 if v < 0 then
                    raise Exception.Create('Can''t read stream size');
                 outSize := outSize or v shl (8 * i);
                 end;
             if not decoder.Code(inStream, outStream, outSize) then
                raise Exception.Create('Error in data stream');
             decoder.Free;
             end;
        outStream.Free;
        inStream.Free;
        end
        else raise Exception.Create('Incorrect command');
params.Free;
end;

end.
