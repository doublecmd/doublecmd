unit UCRC;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

type TCRC=class
       public
         Value:integer;
         constructor Create;
         procedure Init;
         procedure Update(const data: array of byte;const offset,size:integer);overload;
         procedure Update(const data: array of byte);overload;
         procedure UpdateByte(const b:integer);
         function GetDigest:integer;
       end;

implementation

var Table: array [0..255] of integer;

constructor TCRC.Create;
begin
Value:=-1;
end;

procedure TCRC.Init;
begin
Value:=-1;
end;

procedure TCRC.Update(const data: array of byte;const offset,size:integer);
var i:integer;
begin
for i := 0 to size-1 do
    value := Table[(value xor data[offset + i]) and $FF] xor (value shr 8);
end;

procedure TCRC.Update(const data: array of byte);
var size:integer;
    i:integer;
begin
size := length(data);
for i := 0 to size - 1 do
    value := Table[(value xor data[i]) and $FF] xor (value shr 8);
end;

procedure TCRC.UpdateByte(const b:integer);
begin
value := Table[(value xor b) and $FF] xor (value shr 8);
end;

function TCRC.GetDigest:integer;
begin
result:=value xor (-1);
end;

procedure InitCRC;
var i,j,r:integer;
begin
for i := 0 to 255 do begin
    r := i;
    for j := 0 to 7 do begin
        if ((r and 1) <> 0) then
           r := (r shr 1) xor integer($EDB88320)
        else r := r shr 1;
        end;
    Table[i] := r;
    end;
end;

initialization
InitCRC;

end.
