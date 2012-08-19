unit NetUtils;

interface

uses
  Windows, Classes;

Type
  {$H+}
  PNetRes = ^TNetRes;

  TNetRes = Record
    dwScope:    Integer;
    dwType:     Integer;
    dwDisplayType: Integer;
    dwUsage:    Integer;
    LocalName:  String;
    RemoteName: String;
    Comment:    String;
    Provider:   String;
  End;

{H-}

function GetContainerList(ListRoot: PNetResource): TList;
procedure FreeNetResList(var NetResList: TList);

implementation

uses
  SysUtils;

type
  PnetResourceArr = ^TNetResource; {TNetResource - это запись,
                      эквивалентная TNetRes, за исключением того, что
                      вместо типов string там типы PChar. }

function GetContainerList(ListRoot: PNetResource): TList;
{возвращает список сетевых имён с подуровня ListRoot, каждый
элемент списка TList - это PNetRes, где поле RemoteName определяет
соответственно сетевое имя элемента списка. Если ListRoot=nil, то
возвращается самый верхний уровень типа:
1. Microsoft Windows Network
2. Novell Netware Network
Чтобы получить список доменов/рабочих групп сети Microsoft, нужно
вызвать эту функцию второй раз, передав ей в качестве параметра,
соответствующий элемент списка, полученного при первом её вызове.
Чтобы получить список компьютеров домена - вызвать третий раз...}
  {Единственное, я не знаю как узнать имя текущего домена.}
Var
  TempRec: PNetRes;
  Buf: Pointer;
  Count, BufSize, Res: DWORD;
  lphEnum: THandle;
  p: PNetResourceArr;
  i: SmallInt;
  NetworkList: TList;
Begin
  NetworkList := TList.Create;
  Result := nil;
  BufSize := 8192;
  GetMem(Buf, BufSize);
  Try
    Res := WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, ListRoot, lphEnum);
    {в результате получаем ссылку lphEnum}
    If Res <> 0 Then
      Raise Exception(Res);
    Count := $FFFFFFFF; {требуем выдать столько записей в
список, сколько есть}
    Res := WNetEnumResource(lphEnum, Count, Buf, BufSize);
                 {в буфере Buf - списочек
                  в виде массива указателей на структуры типа TNetResourceArr
                  а в Count - число этих структур}
    If Res = ERROR_NO_MORE_ITEMS Then
      Exit;
    If (Res <> 0) Then
      Raise Exception(Res);
    P := PNetResourceArr(Buf);
    For I := 0 To Count - 1 Do
    Begin           //Требуется копирование из буфера, так как он
      New(TempRec); //действителен только до следующего  вызова функций группы WNet
      TempRec^.dwScope := P^.dwScope;
      TempRec^.dwType := P^.dwType;
      TempRec^.dwDisplayType := P^.dwDisplayType;
      TempRec^.dwUsage := P^.dwUsage;
      TempRec^.LocalName := StrPas(P^.lpLocalName);   {имеются  ввиду вот эти указатели}
      TempRec^.RemoteName := StrPas(P^.lpRemoteName); {в смысле  - строки PChar}
      TempRec^.Comment := StrPas(P^.lpComment);
      TempRec^.Provider := StrPas(P^.lpProvider);
      NetworkList.Add(TempRec);
      Inc(P);
    End;
    Res := WNetCloseEnum(lphEnum);
    {а следующий вызов - вот он!}
    If Res <> 0 Then
      Raise Exception(Res);
    Result := NetWorkList;
  Finally
    FreeMem(Buf);
  End;
End;

procedure FreeNetResList(var NetResList: TList);
var
  I: Integer;
begin
  for I := NetResList.Count - 1 downto 0 do
  begin
    Dispose(PNetRes(NetResList[I]));
    NetResList.Delete(I);
  end;
  FreeAndNil(NetResList);
end;

end.

