{
   Double Commander
   -------------------------------------------------------------------------
   Detect string parser.

   Copyright (C) 2008  Dmitry Kolomiets (B4rr4cuda@rambler.ru)
   Based on TMathControl by Vimil Saju
  
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uDetectStr;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  uMasks, uFile;

type
  TMathtype=(mtnil,mtoperator,mtlbracket,mtrbracket,mtoperand);

type
  TMathOperatortype=(monone, // NULL
                     moequ,  // =
                     moneq,  // != replaced with #
                     moles,  // <
                     momor,  // >
                     moand,  // &
                     moor,   // |
                     monot   // NOT
                    );

type
  pmathchar = ^Tmathchar;
  TMathChar = record
    case mathtype: Tmathtype of
      mtoperand:(data:shortstring);
      mtoperator:(op:TMathOperatortype);
  end;

type

   { TParserControl }

   TParserControl = class
   public
    function TestFileResult(const aFile: TFile): boolean; overload;
    function TestFileResult(const aFileName: UTF8String): boolean; overload;
   private
    input,output,stack:array of tmathchar;
    fmathstring:string;
    fforce:boolean;
    function getresult(const aFile: TFile):boolean;
    function calculate(aFile: TFile; operand1,operand2,Aoperator:Tmathchar):string;
    function getoperator(c:UTF8String):TMathOperatortype;
    function getoperand(mid:integer;var len:integer):string;
    procedure processstring;
    procedure convertinfixtopostfix;
    function isdigit(c:UTF8String):boolean;
    function isoperator(c:UTF8String):boolean;
    function getprecedence(mop:TMathOperatortype):integer;
    function BooleanToStr(x:boolean):string;
    function StrToBoolean(s:string):boolean;
   public
     destructor Destroy; override;
   published
    property DetectStr:string read fmathstring write fmathstring;
    property IsForce:boolean read fforce write fforce;
   end;

implementation

uses
  uDebug, uFileProperty, uFileSystemFileSource;
  
function TParserControl.calculate(aFile: TFile; operand1,operand2,Aoperator:Tmathchar):string;
var tmp:string;
begin
 result:='false';

 //Not
 if (operand1.data='NOT') and ((operand2.data='true') or (operand2.data='false')) then
  begin
    Result:=BooleanToStr(Not StrToBoolean(operand2.data));
  end;

  // & |
  if ((operand1.data='true') or (operand1.data='false')) and
     ((operand2.data='true') or (operand2.data='false')) then
    begin
      case Aoperator.op of
        moand: result:= BooleanToStr((StrToBoolean(operand1.data)) and (StrToBoolean(operand2.data)));
        moor: result:=BooleanToStr((StrToBoolean(operand1.data)) or (StrToBoolean(operand2.data)));
      end;
    end;

   //EXT= EXT!=
   if (operand1.data='EXT') then
     begin
       //---------------------
       tmp:= aFile.Extension;
       tmp:= UpperCase(tmp);
       tmp:= '"'+tmp+'"';
       //---------------------
       case Aoperator.op of
        moequ: Result:=BooleanToStr(MatchesMask(tmp,operand2.data));
        moneq: Result:=BooleanToStr(not MatchesMask(tmp,operand2.data));
       end;
     end;

   //SIZE > < = !=
   if (operand1.data='SIZE') and (fpSize in aFile.SupportedProperties) then
     begin
       tmp:= IntToStr(aFile.Size);
        case Aoperator.op of
           moequ: Result:= BooleanToStr(strtoint(tmp)=strtoint(operand2.data));
           moneq: Result:= BooleanToStr(strtoint(tmp)<>strtoint(operand2.data));
           moles: Result:= BooleanToStr(strtoint(tmp)<strtoint(operand2.data));
           momor: Result:= BooleanToStr(strtoint(tmp)>strtoint(operand2.data));
        end;
     end;


 
{ case Aoperator.op of
   moneq: ;
   moequ: ;
   moles: ;
   momor: ;
   moor: ;
   moand: ;
   monot: ;
 end;}
end;

function TParserControl.TestFileResult(const aFile: TFile):boolean;
begin
  Result:= getresult(aFile);
end;


function TParserControl.TestFileResult(const aFileName: UTF8String): boolean;
var
  aFile: TFile;
begin
  aFile:= TFileSystemFileSource.CreateFileFromFile(aFileName);
  try
    DCDebug('aFile.Extension = ' + aFile.Extension);
    Result:= getresult(aFile);
  finally
    aFile.Free;
  end;
end;

function TParserControl.getresult(const aFile: TFile):boolean;
var
 i:integer;
 tmp1,tmp2,tmp3:tmathchar;
begin
 if fmathstring='' then
   begin
     Result:=true;
     exit;
   end;
 convertinfixtopostfix;
 setlength(stack,0);
 for i:=0 to length(output)-1 do
   begin
    if output[i].mathtype=mtoperand then
     begin
      setlength(stack,length(stack)+1);
      stack[length(stack)-1]:=output[i];
     end
    else if output[i].mathtype=mtoperator then
     begin
       tmp1:=stack[length(stack)-1];
       tmp2:=stack[length(stack)-2];
       setlength(stack,length(stack)-2);
       tmp3.mathtype:=mtoperand;
       tmp3.data:=calculate(aFile, tmp2,tmp1,output[i]);
       setlength(stack,length(stack)+1);
       stack[length(stack)-1]:=tmp3;
     end;
   end;
 result:=strToBoolean(stack[0].data);
 setlength(stack,0);
 setlength(input,0);
 setlength(output,0);
 end;

 function TParserControl.getoperator(c:UTF8String):TMathOperatortype;
 begin
 result:=monone;
      if c='<' then
   result:=moles
 else if c='>' then
   result:=momor
 else if c='&' then
   result:=moand
 else if c='=' then
   result:=moequ
 else if c='#' then
   result:=moneq
 else if c='|' then
   result:=moor
else if c='!' then
   result:=monot;
 end;

 function TParserControl.getoperand(mid:integer;var len:integer):string;
 var
 i,j:integer;
 begin
 Result := '';
 j:=1;
 for i:=mid to length(fmathstring)-1 do
   begin
    if isdigit(fmathstring[i]) then
     begin
      if j<=20 then
       Result:=Result+fmathstring[i];
      j:=j+1;
     end
    else
     break;
   end;
 len:=length(Result);
 end;

procedure TParserControl.processstring;
 var
 i:integer;
 numlen:integer;
 begin
 //---------------------
 while pos('!=',fmathstring)>0 do
   begin
     i:=pos('!=',fmathstring);
     delete(fmathstring,i,2);
     insert('#',fmathstring,i);
   end;
 //---------------------
 while pos('FORCE',fmathstring)>0 do
   begin
     i:=pos('FORCE',fmathstring);
     delete(fmathstring,i,length('FORCE'));
     insert(BooleanToStr(fforce),fmathstring,i);
   end;
 //---------------------
 numlen:=1;
 while numlen < length(fmathstring) do
 if (fmathstring[numlen]='!') and (fmathstring[numlen+1]<>'=') then
   begin
     i:=numlen;
     delete(fmathstring,i,1);
     insert('NOT!',fmathstring,i);
     inc(numlen,4);
   end else inc(numlen);
 //---------------------

 i:=0;
 numlen:=0;
 setlength(output,0);
 setlength(input,0);
 setlength(stack,0);

 fmathstring:='('+fmathstring+')';
 setlength(input,length(fmathstring));
 while i<=length(fmathstring)-1 do
   begin
    if fmathstring[i+1]='(' then
     begin
      input[i].mathtype:=mtlbracket;
      i:=i+1;
     end
    else if fmathstring[i+1]=')' then
     begin
      input[i].mathtype:=mtrbracket;
      i:=i+1;
     end
    else if isoperator(fmathstring[i+1]) then
     begin
      input[i].mathtype:=mtoperator;
      input[i].op:=getoperator(fmathstring[i+1]);
      i:=i+1;
     end
    else if isdigit(fmathstring[i+1]) then
     begin
      input[i].mathtype:=mtoperand;
      input[i].data:=getoperand(i+1,numlen);
      i:=i+numlen;
     end else {if fmathstring[i+1]=' ' then} inc(i);
   end;
 end;


 function TParserControl.isoperator(c:UTF8String):boolean;
 begin
 result:=false;
 if (c='=')
 or (c='#')
 or (c='!')
 or (c='&')
 or (c='<')
 or (c='>')
 or (c='|') then
   result:=true;
 end;

function TParserControl.isdigit(c:UTF8String):boolean;
 begin
 result:=false;
 if pos(c,'=#!&<>|()')<=0 then
   result:=true;
 end;

 function TParserControl.getprecedence(mop:TMathOperatortype):integer;
 begin
 result:=-1;
 case mop of
   moor:result:=1;
   moand:result:=1;
   moequ:result:=2;
   moneq:result:=2;
   moles:result:=2;
   momor:result:=2;
   monot:result:=2;
 end;
 end;

function TParserControl.BooleanToStr(x: boolean): string;
 begin
   if x then
     Result:='true'
   else
     Result:='false';
 end;

function TParserControl.StrToBoolean(s: string): boolean;
 begin
  if s='true' then Result:=true else
   Result:=false;
 end;

destructor TParserControl.Destroy;
begin
  inherited Destroy;
end;

 procedure TParserControl.convertinfixtopostfix;
 var
 i,j,prec:integer;
 begin
 processstring;
 for i:=0 to length(input)-1 do
   begin
    if input[i].mathtype=mtoperand then
     begin
      setlength(output,length(output)+1);
      output[length(output)-1]:=input[i];
     end
    else if input[i].mathtype=mtlbracket then
     begin
      setlength(stack,length(stack)+1);
      stack[length(stack)-1]:=input[i];
     end
    else if input[i].mathtype=mtoperator then
     begin
      prec:=getprecedence(input[i].op);
      j:=length(stack)-1;
      if j>=0 then
       begin
        while(getprecedence(stack[j].op)>=prec) and (j>=0) do
         begin
          setlength(output,length(output)+1);
          output[length(output)-1]:=stack[j];
          setlength(stack,length(stack)-1);
          j:=j-1;
         end;
        setlength(stack,length(stack)+1);
        stack[length(stack)-1]:=input[i];
       end;
     end
    else if input[i].mathtype=mtrbracket then
     begin
      j:=length(stack)-1;
      if j>=0 then
       begin
        while(stack[j].mathtype<>mtlbracket) and (j>=0) do
         begin
          setlength(output,length(output)+1);
          output[length(output)-1]:=stack[j];
          setlength(stack,length(stack)-1);
          j:=j-1;
         end;
        if j>=0 then
         setlength(stack,length(stack)-1);
       end;
     end;
   end;
 end;

 end.
