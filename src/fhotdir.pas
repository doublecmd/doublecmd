{
   Double Commander
   -------------------------------------------------------------------------
   Configuration of HotDir

   Copyright (C) 2009-2014  Alexander Koblov (alexx2000@mail.ru)

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

unit fHotDir;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, EditBtn, ExtCtrls,
  Menus, Dialogs, KASPathEdit, uHotDir, types;

type
  TProcedureWhenClickingAMenuItem = procedure(Sender: TObject) of object;
  { TfrmHotDir }
  TfrmHotDir = class(TForm)
    btnTestMenu: TBitBtn;
    btnCancel: TBitBtn;
    btnGoToDir: TBitBtn;
    bynOk: TBitBtn;
    btnRelativePath: TSpeedButton;
    btnRelativeTarget: TSpeedButton;
    cbSortHotDirPath: TComboBox;
    cbSortHotDirTarget: TComboBox;
    lblDirectoryHotlist: TLabel;
    lbleditHotDirName: TLabeledEdit;
    lbleditHotDirPath: TLabeledEdit;
    lbleditHotDirTarget: TLabeledEdit;
    lsHotDir: TListBox;
    MenuItem2: TMenuItem;
    miAddTarget: TMenuItem;
    miAddCommand: TMenuItem;
    miAddCommand2: TMenuItem;
    miDetectIfPathTargetExist: TMenuItem;
    miAddCopyOfSelected2: TMenuItem;
    miDeleteSelectedEntry2: TMenuItem;
    miDetectIfPathExist: TMenuItem;
    miImport: TMenuItem;
    miExport: TMenuItem;
    miExportToTotalCommanderk: TMenuItem;
    miExportToTotalCommandernk: TMenuItem;
    miSeparator5: TMenuItem;
    miExportToDoubleCommanderk: TMenuItem;
    miExportToDoubleCommandernk: TMenuItem;
    miSeparator6: TMenuItem;
    miExportToHotlistFile: TMenuItem;
    miTestResultingMenu: TMenuItem;
    miTools: TMenuItem;
    miSeparator4: TMenuItem;
    miSeparator3: TMenuItem;
    miDeleteAllHotDirs: TMenuItem;
    miImportFromHotlistFile: TMenuItem;
    miImportTotalCommander: TMenuItem;
    miImportDoubleCommander: TMenuItem;
    mnuMainfHotDir: TMainMenu;
    MenuItem1: TMenuItem;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miBrowseToDirectory: TMenuItem;
    miTypeTheDirectory: TMenuItem;
    miActiveFrameDirectory: TMenuItem;
    miActiveInactiveFrameDirectory: TMenuItem;
    miAddCopyOfSelected: TMenuItem;
    miSeparator1: TMenuItem;
    miAddSeparator: TMenuItem;
    miAddSubmenu: TMenuItem;
    miDeleteSelectedEntry: TMenuItem;
    miSeparator2: TMenuItem;
    miDeleteJustSubMenu: TMenuItem;
    miDeleteCompleteSubMenu: TMenuItem;
    miBrowseToDirectory2: TMenuItem;
    miTypeTheDirectory2: TMenuItem;
    miActiveFrameDirectory2: TMenuItem;
    miActiveInactiveFrameDirectory2: TMenuItem;
    MenuItem7: TMenuItem;
    miAddSeparator2: TMenuItem;
    miAddSubmenu2: TMenuItem;
    OpenDialog: TOpenDialog;
    pmHotDirTestMenu: TPopupMenu;
    pmAddHotDirMenu: TPopupMenu;
    SaveDialog: TSaveDialog;
    TimerDragMove: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    function ActualAddDirectories(ParamDispatcher:TKindOfHotDirEntry; sName, sPath, sTarget:string; PositionOfInsertion:longint):longint;
    procedure FormShow(Sender: TObject);
    procedure lsHotDirDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure lsHotDirEnter(Sender: TObject);
    procedure lsHotDirExit(Sender: TObject);
    procedure lsHotDirClick(Sender: TObject);
    procedure lsHotDirKeyPress(Sender: TObject; var Key: char);
    procedure lsHotDirDblClick(Sender: TObject);
    procedure lsHotDirDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lsHotDirDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lsHotDirMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lsHotDirSetNumberOfItemsPerColumnAndItemHeight;
    procedure btnGenericEnter(Sender: TObject);
    procedure btnGenericExit(Sender: TObject);
    procedure btnTestMenuClick(Sender: TObject);
    procedure cbSortHotDirPathChange(Sender: TObject);
    procedure cbSortHotDirTargetChange(Sender: TObject);
    procedure miAddTargetClick(Sender: TObject);
    procedure PopulatePopupMenuWithCommands(pmMenuToPopulate:TPopupMenu);
    procedure miDeleteAllHotDirsClick(Sender: TObject);
    procedure miDeleteSelectedEntryClick(Sender: TObject);
    procedure miAddHotDirClick(Sender: TObject);
    procedure miDetectIfPathExistClick(Sender: TObject);
    procedure miImportFromAnythingClick(Sender: TObject);
    procedure miShowWhereItWouldGo(Sender: TObject);
    procedure miSimplyCopyCaption(Sender: TObject);
    procedure anyRelativeAbsolutePathClick(Sender: TObject);
    procedure lbleditHotDirExit(Sender: TObject);
    procedure lbleditHotDirEnter(Sender: TObject);
    procedure lbleditHotDirKeyPress(Sender: TObject; var Key: char);
    procedure lbleditHotDirMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerDragMoveTimer(Sender: TObject);
  private
    { Private declarations }
    pmPathHelper:TPopupMenu;
    pmCommandHelper:TPopupMenu;
    HotDirListTemp: THotDirList;
    lsHotDirNbOfItemsPerColumn:longint;
    lsHotDirItemHeight:longint; //It looks like with Free Pascal/Lazarus, once the listbox style is set to OwnerDraw, the itemheight is returned to be zero instead of appropriate heigt... So we'll backup it prior to set style to ownerdraw...
    FlagAlreadyScrolled:boolean;
  public
    { Public declarations }
    ActiveFramePath,NotActiveFramePath:string;
    procedure Refresh_lsHotDir(IndexToSelect:longint; FlagCenterSelection:boolean);
    procedure SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher:longint; paramActiveFramePath,paramNotActiveFramePath:string);
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, LCLType,

  //DC
  DCClassesUtf8, DCStrUtils, uGlobs, uLng, uDCUtils, uDebug, uGlobsPaths,
  uSpecialDir, fhotdirimportexport, fmain, uFormCommands;

const
  CENTER_SELECTION = TRUE;
  NOTOUCH_SELECTION = FALSE;

var
  Last_lsHotDirItemHeight:longint = 12;

{ TfrmHotDir.FormCreate }
procedure TfrmHotDir.FormCreate(Sender: TObject);
begin
  // Initialize property storage
  InitPropStorage(Self);
  ActiveFramePath:='';
  NotActiveFramePath:='';

  pmPathHelper:=TPopupMenu.Create(Self);
  gSpecialDirList.PopulateMenuWithSpecialDir(pmPathHelper,mp_PATHHELPER,nil);

  pmCommandHelper:=TPopupMenu.Create(Self);
  PopulatePopupMenuWithCommands(pmCommandHelper);

  btnRelativePath.Hint:=rsMsgHotDirTipSpecialDirBut;
  btnRelativeTarget.Hint:=rsMsgHotDirTipSpecialDirBut;
  cbSortHotDirPath.Hint := rsMsgHotDirTipOrderPath;
  cbSortHotDirTarget.Hint := rsMsgHotDirTipOrderTarget;
  miAddTarget.Checked := gHotDirAddTargetOrNot;

  {$IFNDEF MSWINDOWS}
  miExportToTotalCommanderk.Free;
  miExportToTotalCommandernk.Free;
  miSeparator5.Free;
  miImportTotalCommander.Free;
  {$ENDIF}
end;

{ TfrmHotDir.FormActivate }
procedure TfrmHotDir.FormActivate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  //If when show up for the first time the element selected is 0, it looks like there is a problem with drawing routine
  //somewhere because it does not higlight the first element. By selecting the second one and then the first one, it make
  //the highlight of the first to work. Hum...
  if ((lsHotDir.ItemIndex=0) and (lsHotDir.Items.Count>1)) then
  begin
    lsHotDir.ItemIndex:=1;
    lsHotDir.ItemIndex:=0;
  end;
{$ENDIF}
end;

{ TfrmHotDir.FormResize}
procedure TfrmHotDir.FormResize(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  NumberOfColumn:integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  NumberOfColumn:=lsHotDir.Width div 300;
  if NumberOfColumn<1 then NumberOfColumn:=1;
  lsHotDir.Columns:=NumberOfColumn;
{$ENDIF}
  lsHotDirSetNumberOfItemsPerColumnAndItemHeight;
end;

{ TfrmHotDir.FormCloseQuery }
procedure TfrmHotDir.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Answer:integer;
begin
  if (modalResult<>mrOk) AND (modalResult<>mrAll) AND (HotDirListTemp.FlagModified) then
    begin
      if modalResult<>mrIgnore then //Don't bother user if he voluntary hit CANCEL. It's clear he doesn't want to save!
        begin
          Answer:=MessageDlg(rsMsgHotDirModifiedWantToSave,mtConfirmation,[mbYes,mbNo,mbCancel],0);
          CanClose:=((Answer=mrYes) OR (Answer=mrNo));
          if Answer=mrYes then modalResult:=mrOk;
        end;
    end;
end;

{ TfrmHotDir.FormClose }
procedure TfrmHotDir.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (modalResult=mrOk) or (modalResult=mrAll) then HotDirListTemp.CopyListToHotDirList(gHotDirList);
  pmPathHelper.Free;
  pmCommandHelper.Free;
end;

{ TfrmHotDir.FormKeyPress }
procedure TfrmHotDir.FormKeyPress(Sender: TObject; var Key: char);
begin
  case ord(Key) of
    $1B: //Escape? Let's quit, simply
      begin
        if (not lbleditHotDirName.Focused) AND (not lbleditHotDirPath.Focused) AND (not lbleditHotDirTarget.Focused) then
        begin
          close;
          Key:=#$00;
        end;
      end;
  end;
end;

{ TfrmHotDir.ActualAddDirectories }
function TfrmHotDir.ActualAddDirectories(ParamDispatcher:TKindOfHotDirEntry; sName, sPath, sTarget:string; PositionOfInsertion:longint):longint;
  var
    LocalHotDir:THotDir;
  begin
    LocalHotDir:=THotDir.Create;
    LocalHotDir.Dispatcher:=ParamDispatcher;
    LocalHotDir.MenuLevel:=0;
    LocalHotDir.HotDirName:=sName;
    LocalHotDir.HotDirPath:=IncludeTrailingPathDelimiter(sPath);
    if sTarget<>'' then LocalHotDir.HotDirTarget:=IncludeTrailingPathDelimiter(sTarget);
    if (PositionOfInsertion>=0) AND (PositionOfInsertion<HotDirListTemp.Count) then
    begin
      if PositionOfInsertion>0 then
      begin
        case HotDirListTemp.HotDir[PositionOfInsertion-1].Dispatcher of
          hd_STARTMENU: LocalHotDir.MenuLevel:=HotDirListTemp.HotDir[PositionOfInsertion-1].MenuLevel+1;
          else LocalHotDir.MenuLevel:=HotDirListTemp.HotDir[PositionOfInsertion-1].MenuLevel;
        end;
      end;
      HotDirListTemp.Insert(PositionOfInsertion,LocalHotDir);
      HotDirListTemp.FlagModified:=TRUE;
      result:=PositionOfInsertion;
    end
    else
    begin
      HotDirListTemp.Add(LocalHotDir);
      HotDirListTemp.FlagModified:=TRUE;
      result:=pred(HotDirListTemp.Count);
    end;
  end;

{ TfrmHotDir.FormShow }
procedure TfrmHotDir.FormShow(Sender: TObject);
begin
  Refresh_lsHotDir(lsHotDir.ItemIndex,CENTER_SELECTION);
end;

{ TfrmHotDir.lsHotDirDrawItem }
procedure TfrmHotDir.lsHotDirDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
  function GetSpacesRequire(NbSpace:longint):string;
    var
      i:integer;
    begin
      result:='';
      for i:=1 to NbSpace do result:=result+' ';
      result:=result+result+result+result;
    end;

begin
  with Control as TListBox do
    begin
      Last_lsHotDirItemHeight:=ARect.Bottom-ARect.Top;
      Canvas.FillRect(ARect);
      Canvas.ClipRect:=ARect;

      case HotDirListTemp.HotDir[Index].Dispatcher of
        hd_NULL:
          begin
          end;

        hd_CHANGEPATH:
          begin
            if not (odSelected in State) then
            begin
              case HotDirListTemp.HotDir[Index].HotDirExisting of
                DirExistUnknown: Canvas.Font.Color:=clDefault;
                DirExist: Canvas.Font.Color:=clGreen;
                DirNotExist: Canvas.Font.Color:=clRed;
              end;
            end;

            Canvas.Font.Style:=[];
            Canvas.TextRect(ARect,ARect.Left+2,ARect.Top,GetSpacesRequire(HotDirListTemp.HotDir[Index].MenuLevel)+HotDirListTemp.HotDir[Index].HotDirName);
          end;

        hd_SEPARATOR:
          begin
          end;

        hd_STARTMENU:
          begin
            Canvas.Font.Style:=[fsBold];
            Canvas.TextRect(ARect,ARect.Left+2,ARect.Top,GetSpacesRequire(HotDirListTemp.HotDir[Index].MenuLevel)+HotDirListTemp.HotDir[Index].HotDirName);
          end;

        hd_ENDMENU:
          begin
            Canvas.TextRect(ARect,ARect.Left+2,ARect.Top,GetSpacesRequire(HotDirListTemp.HotDir[Index].MenuLevel)+'--');
          end;

        hd_COMMAND:
          begin
            Canvas.Font.Style:=[];
            Canvas.TextRect(ARect,ARect.Left+2,ARect.Top,GetSpacesRequire(HotDirListTemp.HotDir[Index].MenuLevel)+HotDirListTemp.HotDir[Index].HotDirName);
          end;
      end;
    end;
end;

{ TfrmHotDir.lsHotDirEnter }
procedure TfrmHotDir.lsHotDirEnter(Sender: TObject);
begin
  lblDirectoryHotlist.Font.Style:=[fsBold];
end;

{ TfrmHotDir. }
procedure TfrmHotDir.lsHotDirExit(Sender: TObject);
begin
    lblDirectoryHotlist.Font.Style:=[];
end;

{ TfrmHotDir.lsHotDirClick }
procedure TfrmHotDir.lsHotDirClick(Sender: TObject);
begin
  if lsHotDir.ItemIndex<>-1 then
  begin
    case HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher of
      hd_NULL:
        begin
        end;

      hd_CHANGEPATH:
        begin
          lbleditHotDirName.EditLabel.Caption:=rsMsgHotDirSimpleName;
          lbleditHotDirName.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName;
          lbleditHotDirName.Enabled:=TRUE;

          lbleditHotDirPath.EditLabel.Caption:=rsMsgHotDirJustPath;
          lbleditHotDirPath.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath;
          lbleditHotDirPath.Hint:=mbExpandFileName(lbleditHotDirPath.Text);
          cbSortHotDirPath.ItemIndex:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPathSort;
          lbleditHotDirPath.Visible:=TRUE;
          btnRelativePath.Tag:=2;

          lbleditHotDirTarget.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget;
          lbleditHotDirTarget.Hint:=mbExpandFileName(lbleditHotDirTarget.Text);
          cbSortHotDirTarget.ItemIndex:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTargetSort;
          lbleditHotDirTarget.Visible:=TRUE;
        end;

      hd_COMMAND:
        begin
          lbleditHotDirName.EditLabel.Caption:=rsMsgHotDirSimpleName;
          lbleditHotDirName.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName;
          lbleditHotDirName.Enabled:=TRUE;

          lbleditHotDirPath.EditLabel.Caption:=rsMsgHotDirSimpleCommand;
          lbleditHotDirPath.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath;
          lbleditHotDirPath.Hint:='';
          lbleditHotDirPath.Visible:=TRUE;
          btnRelativePath.Tag:=4;

          lbleditHotDirTarget.Visible:=FALSE;
        end;

      hd_SEPARATOR:
        begin
          lbleditHotDirName.EditLabel.Caption:='';
          lbleditHotDirName.Text:=rsMsgHotDirSimpleSeparator;
          lbleditHotDirName.Enabled:=FALSE;

          lbleditHotDirPath.Visible:=FALSE;
          lbleditHotDirTarget.Visible:=FALSE;
        end;

      hd_STARTMENU:
        begin
          lbleditHotDirName.EditLabel.Caption:=rsMsgHotDirSimpleMenu;
          lbleditHotDirName.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName;
          lbleditHotDirName.Enabled:=TRUE;

          lbleditHotDirPath.Visible:=FALSE;
          lbleditHotDirTarget.Visible:=FALSE;
        end;

      hd_ENDMENU:
        begin
          lbleditHotDirName.EditLabel.Caption:='';
          lbleditHotDirName.Text:=rsMsgHotDirSimpleEndOfMenu;
          lbleditHotDirName.Enabled:=FALSE;

          lbleditHotDirPath.Visible:=FALSE;
          lbleditHotDirTarget.Visible:=FALSE;
        end;
    end;

    btnRelativePath.Visible:=lbleditHotDirPath.Visible;
    cbSortHotDirPath.Visible:=lbleditHotDirPath.Visible AND (HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher<>hd_COMMAND);
    btnRelativeTarget.Visible:=lbleditHotDirTarget.Visible;
    cbSortHotDirTarget.Visible:=lbleditHotDirTarget.Visible;
    btnGoToDir.Enabled:=lbleditHotDirPath.Visible AND (HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher<>hd_COMMAND);
    miDeleteJustSubMenu.Enabled:=(HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_STARTMENU);
    miDeleteCompleteSubMenu.Enabled:=(HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_STARTMENU);
    miAddCopyOfSelected.Enabled:=((HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher<>hd_STARTMENU) AND (HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher<>hd_ENDMENU));
    miAddCopyOfSelected2.Enabled:=miAddCopyOfSelected.Enabled;
    miDeleteSelectedEntry.Enabled:=(HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher<>hd_ENDMENU);
    miDeleteSelectedEntry2.Enabled:=miDeleteSelectedEntry.Enabled;
  end;
end;

{ TfrmHotDir.lsHotDirKeyPress }
procedure TfrmHotDir.lsHotDirKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    #13:
      begin
        Key:=#0;
        if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
      end;
  end;
end;

{ TfrmHotDir.lsHotDirDblClick }
procedure TfrmHotDir.lsHotDirDblClick(Sender: TObject);
begin
  if lsHotDir.ItemIndex>-1 then
    begin
      if (HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath<>'-') AND (HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath<>'--') then
        begin
          ModalResult:=mrAll;
        end;
    end;
end;

{ TfrmHotDir.lsHotDirDragOver }
procedure TfrmHotDir.lsHotDirDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
const
  MOVEABLEZONEAREA=25;
var
  NbItemsPerColumn,TestNN:longint;
begin
  if lsHotDir.Columns<2 then
  begin
    if Y<MOVEABLEZONEAREA then
    begin
      lsHotDir.TopIndex:=lsHotDir.TopIndex-1; //No need to valite if on top already, code is doing it for us!
    end
    else
    begin
      if (Y+MOVEABLEZONEAREA)>lsHotDir.Height then
      begin
        lsHotDir.TopIndex:=lsHotDir.TopIndex+1;  //No need to valite if at bottom already, code is doing it for us!
      end
      else
      begin
        if (Source = lsHotDir) and (lsHotDir.ItemIndex<>-1) then
        begin
          Accept:=not(HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_ENDMENU); //We don't want "MenuEnd" to be moveable.
        end
        else
        begin
          Accept:=FALSE;
        end;
      end;
    end;
  end
  else
  begin
    //Since we have more than one columns, the scrollbar is horizontal, so we want user to drag item out of the list via the right side, not bottom like in 1 column.
    //After trying many various attempts in different ways, this is the closest way I fond workable.
    if (X+MOVEABLEZONEAREA)>lsHotDir.Width then
    begin
      if not FlagAlreadyScrolled then
      begin
        FlagAlreadyScrolled:=TRUE;
        lsHotDir.TopIndex:=(lsHotDir.TopIndex+lsHotDirNbOfItemsPerColumn);
        TimerDragMove.Enabled:=TRUE;
      end;
    end
    else
    begin
      if X < MOVEABLEZONEAREA then
      begin
        if not FlagAlreadyScrolled then
        begin
          FlagAlreadyScrolled:=TRUE;
          lsHotDir.TopIndex:=(lsHotDir.TopIndex-lsHotDirNbOfItemsPerColumn);
          TimerDragMove.Enabled:=TRUE;
        end;
      end
      else
      begin
        FlagAlreadyScrolled:=FALSE;
        if (Source = lsHotDir) and (lsHotDir.ItemIndex<>-1) then
        begin
          Accept:=not(HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_ENDMENU); //We don't want "MenuEnd" to be moveable.
        end
        else
        begin
          Accept:=FALSE;
        end;
      end;
    end;
  end;
end;

{ TfrmHotDir.lsHotDirDragDrop }
procedure TfrmHotDir.lsHotDirDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  CurIndex, NewIndex: Integer;
begin
  CurIndex := lsHotDir.ItemIndex;
  if CurIndex = -1 then Exit;
  NewIndex := lsHotDir.ItemAtPos(Point(X,Y),TRUE);
  if (NewIndex < 0) or (NewIndex >= lsHotDir.Count) then NewIndex := lsHotDir.Count - 1;

  case HotDirListTemp.HotDir[CurIndex].Dispatcher of
    hd_STARTMENU:
      begin
        HotDirListTemp.MoveHotDirMenu(CurIndex, NewIndex);
        Refresh_lsHotDir(NewIndex,NOTOUCH_SELECTION);
      end;

    hd_CHANGEPATH, hd_SEPARATOR, hd_COMMAND:
      begin
        HotDirListTemp.Move(CurIndex, NewIndex);
        Refresh_lsHotDir(NewIndex,NOTOUCH_SELECTION);
      end;
  end;
end;

{ TfrmHotDir.lsHotDirMouseDown }
procedure TfrmHotDir.lsHotDirMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{$IFDEF MSWINDOWS}
  lsHotDirClick(lsHotDir); //Might looks stupid an unnecessary but it is to avoid the fact that "OnClick" is not triggered in some circonstances.
                           //Example? Suppose the focus is on element #n. Suppose we press the down arrow to select n+1, if with the mouse we then click on element #n,
                           //the element is really selected BUT the event "OnClick" is not triggered (at least on Windows Vista) BUT OnMouseDown is triggered.
{$ENDIF}
end;

{ TfrmHotDir.lsHotDirSetNumberOfItemsPerColumnAndItemHeight }
procedure TfrmHotDir.lsHotDirSetNumberOfItemsPerColumnAndItemHeight;
var
  FirstItemOfColumn, LastItemOfColumn, SearchingY:longint;
begin
  LastItemOfColumn:=-1;
  FirstItemOfColumn:=lsHotDir.ItemAtPos(Point(4,4),TRUE);
  if FirstItemOfColumn<>-1 then
  begin
    SearchingY:=lsHotDir.Height-1;
    repeat
      LastItemOfColumn:=lsHotDir.ItemAtPos(Point(1,SearchingY),TRUE);
      dec(SearchingY);
    until (LastItemOfColumn<>-1) AND (SearchingY>0);
  end;

  if (FirstItemOfColumn<>-1) AND (LastItemOfColumn<>-1) then
  begin
    lsHotDirNbOfItemsPerColumn:=((LastItemOfColumn-FirstItemOfColumn)+1);
    lsHotDirItemHeight:=(lsHotDir.Height div lsHotDirNbOfItemsPerColumn);
  end
  else
  begin
    //We hope to don't fall here but if we ever fall here, at least variable will have a value!
    lsHotDirNbOfItemsPerColumn:=10;
    lsHotDirItemHeight:=Last_lsHotDirItemHeight;
  end;
end;

{ TfrmHotDir.btnGenericEnter }
procedure TfrmHotDir.btnGenericEnter(Sender: TObject);
begin
  with sender as TBitBtn do Font.Style:=[fsBold];
end;

{ TfrmHotDir.btnGenericExit }
procedure TfrmHotDir.btnGenericExit(Sender: TObject);
begin
  with sender as TBitBtn do Font.Style:=[];
end;

{ TfrmHotDir.btnTestMenuClick }
procedure TfrmHotDir.btnTestMenuClick(Sender: TObject);
var
  p:TPoint;
begin
  HotDirListTemp.PopulateMenuWithHotDir(pmHotDirTestMenu,@miShowWhereItWouldGo,nil,'',mpJUSTHOTDIRS,0);
  p:=lsHotDir.ClientToScreen(Classes.Point(0,0));
  pmHotDirTestMenu.PopUp(p.X,p.Y);
end;

{ TfrmHotDir.cbSortHotDirPathChange }
procedure TfrmHotDir.cbSortHotDirPathChange(Sender: TObject);
begin
  HotDirListTemp.FlagModified:=TRUE;
  HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPathSort:=cbSortHotDirPath.ItemIndex;
end;

{ TfrmHotDir.cbSortHotDirTargetChange }
procedure TfrmHotDir.cbSortHotDirTargetChange(Sender: TObject);
begin
  HotDirListTemp.FlagModified:=TRUE;
  HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTargetSort:=cbSortHotDirTarget.ItemIndex;
end;

procedure TfrmHotDir.miAddTargetClick(Sender: TObject);
begin
  gHotDirAddTargetOrNot := miAddTarget.Checked;
end;

{ TfrmHotDir.PopulatePopupMenuWithCommands }
procedure TfrmHotDir.PopulatePopupMenuWithCommands(pmMenuToPopulate:TPopupMenu);
  var
    FFormCommands: IFormCommands;
    LocalDummyComboBox:TComboBox;
    miMainTree:TMenuItem;
    IndexCommand:longint;
  procedure LocalPopulateUntil(ParamMenuItem:TMenuItem; LetterUpTo:Char);
    var
      LocalMenuItem:TMenuItem;
      MaybeItemName:string;
    begin
      MaybeItemName:='0000';
      while (IndexCommand<LocalDummyComboBox.Items.Count) AND (MaybeItemName[4]<>LetterUpTo) do
      begin
        MaybeItemName:=LocalDummyComboBox.Items.Strings[IndexCommand];
        if MaybeItemName[4]<>LetterUpTo then
        begin
          LocalMenuItem:=TMenuItem.Create(ParamMenuItem);
          LocalMenuItem.Caption:=MaybeItemName;
          LocalMenuItem.OnClick:=@miSimplyCopyCaption;
          ParamMenuItem.Add(LocalMenuItem);
          inc(IndexCommand);
        end;
      end;
    end;

  begin
    LocalDummyComboBox:=TComboBox.Create(Self);
    try
      LocalDummyComboBox.Clear;
      FFormCommands := frmMain as IFormCommands;
      FFormCommands.GetCommandsList(LocalDummyComboBox.Items);
      LocalDummyComboBox.Sorted := True;

      IndexCommand:=0;

      miMainTree:=TMenuItem.Create(pmMenuToPopulate);
      miMainTree.Caption:='cm_A..cm_C';
      pmMenuToPopulate.Items.Add(miMainTree);
      LocalPopulateUntil(miMainTree,'D');

      miMainTree:=TMenuItem.Create(pmMenuToPopulate);
      miMainTree.Caption:='cm_D..cm_L';
      pmMenuToPopulate.Items.Add(miMainTree);
      LocalPopulateUntil(miMainTree,'M');

      miMainTree:=TMenuItem.Create(pmMenuToPopulate);
      miMainTree.Caption:='cm_M..cm_Z';
      pmMenuToPopulate.Items.Add(miMainTree);
      LocalPopulateUntil(miMainTree,'A');

    finally
      LocalDummyComboBox.Free;
    end;
  end;

{ TfrmHotDir.miDeleteAllHotDirsClick }
procedure TfrmHotDir.miDeleteAllHotDirsClick(Sender: TObject);
var
  Index:longint;
begin
  if MessageDlg(rsMsgHotDirDeleteAllEntries, mtConfirmation, [mbYes,mbNo], 0)=mrYes then
  begin
    for Index:=pred(HotDirListTemp.Count) downto 0 do HotDirListTemp.DeleteHotDir(Index);
    Refresh_lsHotDir(lsHotDir.ItemIndex,NOTOUCH_SELECTION);
    lbleditHotDirName.Text:='';
    lbleditHotDirPath.Text:='';
    lbleditHotDirTarget.Text:='';
  end;
end;

{ TfrmHotDir.miDeleteSelectedEntryClick }
procedure TfrmHotDir.miDeleteSelectedEntryClick(Sender: TObject);
var
  DeleteDispatcher:integer;
begin
  if (lsHotDir.ItemIndex=-1) then Exit;
  with Sender as TComponent do DeleteDispatcher:=tag;

  case HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher of
    hd_NULL, hd_CHANGEPATH, hd_SEPARATOR, hd_COMMAND:
      begin
        HotDirListTemp.DeleteHotDir(lsHotDir.ItemIndex);
        Refresh_lsHotDir(lsHotDir.ItemIndex,NOTOUCH_SELECTION);
      end;

    hd_STARTMENU:
      begin
        HotDirListTemp.DeleteHotDirMenuDelimiters(lsHotDir.ItemIndex,DeleteDispatcher);
        Refresh_lsHotDir(lsHotDir.ItemIndex,NOTOUCH_SELECTION);
      end;

    hd_ENDMENU:
      begin
      end;
  end;
end;

{ TfrmHotDir.miAddHotDirClick }
procedure TfrmHotDir.miAddHotDirClick(Sender: TObject);
var
  sName, sPath, initialPath: String;
  Dispatcher,PositionOfInsertion:longint;
  FlagNeedRefresh:boolean;
begin
  with Sender as TComponent do Dispatcher:=tag;
  PositionOfInsertion:=lsHotDir.ItemIndex+1; //It's a ADD, not an INSERT so we ADD a-f-t-e-r! If're on last item, don't worry, "ActualAddDirectories" will return correct point of insertion
  sName:='';
  sPath:='';
  FlagNeedRefresh:=FALSE;

  case Dispatcher of
    1: //Directory I will browse to
      begin
        initialPath:='';
        if (lsHotDir.ItemIndex<>-1) then
        begin
          if HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_CHANGEPATH then initialPath:=mbExpandFileName(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath);
        end;
        if initialPath='' then initialPath:=ActiveFramePath;

        if SelectDirectory(rsSelectDir, initialPath, sPath, False) then
          begin
            PositionOfInsertion:=ActualAddDirectories(hd_CHANGEPATH,GetLastDir(sPath),sPath,'',PositionOfInsertion);
            FlagNeedRefresh:=TRUE;
          end;
      end;

    2: //Directory I will type
      begin
        PositionOfInsertion:=ActualAddDirectories(hd_CHANGEPATH, rsMsgHotDirName, rsMsgHotDirPath, rsMsgHotDirTarget, PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;

    3: //Directory of the active frame
      begin
        PositionOfInsertion:=ActualAddDirectories(hd_CHANGEPATH,GetLastDir(ActiveFramePath),ActiveFramePath,'',PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;

    4: //Directory of the active AND inactive frames
      begin
        PositionOfInsertion:=ActualAddDirectories(hd_CHANGEPATH,GetLastDir(ActiveFramePath),ActiveFramePath,NotActiveFramePath,PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;

    5:
      begin
        PositionOfInsertion:=ActualAddDirectories(hd_SEPARATOR,'','','',PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;

    6:
      begin
        PositionOfInsertion:=ActualAddDirectories(hd_STARTMENU,rsMsgHotDirSubMenuName,'','',PositionOfInsertion);
        inc(PositionOfInsertion);
        PositionOfInsertion:=ActualAddDirectories(hd_ENDMENU,'','','',PositionOfInsertion);
        dec(PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;

    7:
      begin
        with HotDirListTemp.HotDir[lsHotDir.ItemIndex] as THotDir do PositionOfInsertion:=ActualAddDirectories(Dispatcher,HotDirName,HotDirPath,HotDirTarget,PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;

    8: //A command
      begin
        PositionOfInsertion:=ActualAddDirectories(hd_COMMAND, rsMsgHotDirCommandName, rsMsgHotDirCommandSample, '', PositionOfInsertion);
        FlagNeedRefresh:=TRUE;
      end;
  end;

  if FlagNeedRefresh then
  begin
    Refresh_lsHotDir(PositionOfInsertion,NOTOUCH_SELECTION);
    if lbleditHotDirName.CanFocus then lbleditHotDirName.SetFocus;
  end;
end;

{ TfrmHotDir.miDetectIfPathExistClick }
procedure TfrmHotDir.miDetectIfPathExistClick(Sender: TObject);
var
  RememberSelected:longint;
begin
  RememberSelected:=lsHotDir.ItemIndex;
  try
    lbleditHotDirName.Text:='';
    lbleditHotDirName.Enabled:=FALSE;
    lbleditHotDirPath.Visible:=FALSE;
    cbSortHotDirPath.Visible:=FALSE;
    cbSortHotDirTarget.Visible:=FALSE;
    lbleditHotDirTarget.Visible:=FALSE;
    btnRelativePath.Visible:=FALSE;
    btnRelativeTarget.Visible:=FALSE;
    Application.ProcessMessages;
    with Sender as TComponent do HotDirListTemp.RefreshExistingProperty(lsHotDir, lbleditHotDirName, tag);
  finally
    lsHotDir.ItemIndex:=RememberSelected;
    Refresh_lsHotDir(lsHotDir.ItemIndex,NOTOUCH_SELECTION);
  end;
end;

{ TfrmHotDir.miImportFromAnythingClick }
procedure TfrmHotDir.miImportFromAnythingClick(Sender: TObject);
const
  ACTION_WITH_TOTALCOMMANDER = $00;
  ACTION_WITH_DOUBLECOMMANDER = $01;
  ACTION_WITH_SIMPLEFILE = $02;
  ACTION_IMPORT = $00;
  ACTION_EXPORT = $04;
  ACTION_ERASE_EXISTING = $10;
var
  WorkingHotDirList:THotDirList;
  Answer, NbOfAdditional, ActionDispatcher:longint;
  FilenameToWorkWith:string;
  FlagKeepGoing:boolean;
begin
  with Sender as TComponent do ActionDispatcher:=tag;

  case (ActionDispatcher AND $03) of
    ACTION_WITH_TOTALCOMMANDER:
      begin
        OpenDialog.DefaultExt:='*.ini';
        OpenDialog.FilterIndex:=1;
        OpenDialog.Title:=rsMsgHotDirLocateTC;
        FlagKeepGoing:=OpenDialog.Execute;
      end;

    ACTION_WITH_DOUBLECOMMANDER:
      begin
        OpenDialog.DefaultExt:='*.xml';
        OpenDialog.FilterIndex:=2;
        OpenDialog.Title:=rsMsgHotDirLocateDC;
        FlagKeepGoing:=OpenDialog.Execute;
      end;

    ACTION_WITH_SIMPLEFILE:
      begin
        case (ActionDispatcher AND $0C) of
          ACTION_IMPORT:
            begin
              OpenDialog.FilterIndex:=3;
              OpenDialog.Title:=rsMsgHotDirLocatePreviousSave;
              FlagKeepGoing:=OpenDialog.Execute;
            end;

          ACTION_EXPORT:
            begin
              SaveDialog.FilterIndex:=1;
              SaveDialog.FileName:='New directory hotlist';
              SaveDialog.Title:=rsMsgHotDirWhereToSave;
              FlagKeepGoing:=SaveDialog.Execute;
            end;
        end;
      end;
  end;

  if FlagKeepGoing then
  begin
    WorkingHotDirList:=THotDirList.Create;
    try
      case (ActionDispatcher AND $03) of
        ACTION_WITH_TOTALCOMMANDER: WorkingHotDirList.ImportTotalCommander(utf8string(OpenDialog.Filename));
        ACTION_WITH_DOUBLECOMMANDER: WorkingHotDirList.ImportDoubleCommander(utf8string(OpenDialog.Filename));
        ACTION_WITH_SIMPLEFILE: if (ActionDispatcher AND $C0)=ACTION_IMPORT then WorkingHotDirList.ImportDoubleCommander(utf8string(OpenDialog.Filename));
      end;

      with Tfrmhotdirimportexport.Create(Application) do
      begin
        try
          case (ActionDispatcher AND $0C) of
            ACTION_IMPORT:
              begin
                WorkingHotDirList.LoadToStringList(lsImportedHotDir.Items);
                lsImportedHotDir.OnDrawItem:=@WorkingHotDirList.lsHotDirDrawItem;
                lsImportedHotDir.OnClick:=@WorkingHotDirList.lsImportationHotDirClick;
                btnSelectAll.Caption:=rsMsgHotDirImportall;
                btnSelectionDone.Caption:=rsMsgHotDirImportSel;
                Caption:=rsMsgHotDirImportHotlist;
              end;

            ACTION_EXPORT:
              begin
                HotDirListTemp.CopyListToHotDirList(WorkingHotDirList);
                WorkingHotDirList.LoadToStringList(lsImportedHotDir.Items);
                lsImportedHotDir.OnDrawItem:=@WorkingHotDirList.lsHotDirDrawItem;
                lsImportedHotDir.OnClick:=@WorkingHotDirList.lsImportationHotDirClick;
                btnSelectAll.Caption:=rsMsgHotDirExportall;
                btnSelectionDone.Caption:=rsMsgHotDirExportSel;
                Caption:=rsMsgHotDirExportHotlist;
              end;
          end;

          Answer:=ShowModal;
          if Answer=mrAll then lsImportedHotDir.SelectAll;
          if ((Answer=mrOk) or (Answer=mrAll)) AND (lsImportedHotDir.SelCount>0) then
          begin
            case (ActionDispatcher AND $0C) of
              ACTION_IMPORT:
                begin
                  NbOfAdditional:=HotDirListTemp.AddFromAnotherListTheSelected(WorkingHotDirList,lsImportedHotDir);
                  if NbOfAdditional>0 then
                  begin
                    Refresh_lsHotDir(lsHotDir.ItemIndex,CENTER_SELECTION);
                    miDeleteSelectedEntry.Enabled:= (lsHotDir.Items.Count > 0);
                    MessageDlg(rsMsgHotDirNbNewEntries+IntToStr(NbOfAdditional),mtInformation,[mbOk],0);
                  end;
                end;

              ACTION_EXPORT:
                begin
                  WorkingHotDirList.EliminateTheNonSelectedInList(lsImportedHotDir);
                  if WorkingHotDirList.Count>0 then
                  begin
                    case (ActionDispatcher AND $03) of
                      ACTION_WITH_TOTALCOMMANDER: if WorkingHotDirList.ExportTotalCommander(OpenDialog.FileName,((ActionDispatcher AND $10)=$10)) then ShowMessage(rsMsgHotDirTotalExported+IntToStr(WorkingHotDirList.count)) else ShowMessage(rsMsgHotDirErrorExporting);
                      ACTION_WITH_DOUBLECOMMANDER:  if WorkingHotDirList.ExportDoubleCommander(OpenDialog.FileName,((ActionDispatcher AND $10)=$10)) then ShowMessage(rsMsgHotDirTotalExported+IntToStr(WorkingHotDirList.count)) else ShowMessage(rsMsgHotDirErrorExporting);
                      ACTION_WITH_SIMPLEFILE: if WorkingHotDirList.ExportDoubleCommander(SaveDialog.FileName,TRUE) then ShowMessage(rsMsgHotDirTotalExported+IntToStr(WorkingHotDirList.count)) else ShowMessage(rsMsgHotDirErrorExporting);
                    end;
                  end
                  else
                  begin
                    ShowMessage(rsMsgHotDirNothingToExport);
                  end;
                end; //ACTION_EXPORT:
            end; //case (ActionDispatcher AND $0C) of
          end; //If user confirmed OK and have selected something...
        finally
          Free;
        end;
      end;
    finally
      WorkingHotDirList.Free;
    end;
  end;
end;

{ TfrmHotDir.miShowWhereItWouldGo }
procedure TfrmHotDir.miShowWhereItWouldGo(Sender: TObject);
var
  StringToShow:string;
begin
  with Sender as TComponent do
    begin
      lsHotDir.ItemIndex:=tag;
      lsHotDir.TopIndex:=tag-((lsHotDir.Height div lsHotDirItemHeight) div 2);

      StringToShow:=rsMsgHotDirDemoName+'"'+HotDirListTemp.HotDir[tag].HotDirName+'"';

      case HotDirListTemp.HotDir[tag].Dispatcher of
        hd_CHANGEPATH:
          begin
            StringToShow:=StringToShow+#$0D+#$0A+#$0D+#$0A+rsMsgHotDirDemoPath;
            StringToShow:=StringToShow+#$0D+#$0A+mbExpandFileName(HotDirListTemp.HotDir[tag].HotDirPath);

            if HotDirListTemp.HotDir[tag].HotDirTarget<>'' then
              begin
                StringToShow:=StringToShow+#$0D+#$0A+#$0D+#$0A+rsMsgHotDirDemoTarget;
                StringToShow:=StringToShow+#$0D+#$0A+mbExpandFileName(HotDirListTemp.HotDir[tag].HotDirTarget);
              end;
          end;

        hd_COMMAND:
          begin
            StringToShow:=StringToShow+#$0D+#$0A+#$0D+#$0A+rsMsgHotDirDemoCommand;
            StringToShow:=StringToShow+#$0D+#$0A+mbExpandFileName(HotDirListTemp.HotDir[tag].HotDirPath);
          end;
      end;

      MessageDlg(StringToShow,mtInformation,[mbOk],0);
    end;
end;

{ TfrmHotDir.miSimplyCopyCaption }
procedure TfrmHotDir.miSimplyCopyCaption(Sender: TObject);
begin
  with Sender as TMenuItem do
    begin
      if lbleditHotDirPath.text='' then
        lbleditHotDirPath.Text:=Caption
      else
        lbleditHotDirPath.text:=Caption+' '+lbleditHotDirPath.text;
    end;
end;

{ TfrmHotDir.anyRelativeAbsolutePathClick }
procedure TfrmHotDir.anyRelativeAbsolutePathClick(Sender: TObject);
begin
  with Sender as TComponent do
  begin
    case tag of
      2:
        begin
          lbleditHotDirPath.SetFocus;
          gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirPath,pfPATH);
          pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          if HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath<>lbleditHotDirPath.Text then
          begin
            HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath:=lbleditHotDirPath.Text;
            HotDirListTemp.FlagModified:=TRUE;
          end;
        end;

      4:
        begin
          lbleditHotDirPath.SetFocus;
          gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirPath,pfPATH);
          pmCommandHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;

      3:
        begin
          lbleditHotDirTarget.SetFocus;
          gSpecialDirList.SetSpecialDirRecipientAndItsType(lbleditHotDirTarget,pfPATH);
          pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
        end;
    end;
  end;
end;

{ TfrmHotDir.lbleditHotDirExit }
procedure TfrmHotDir.lbleditHotDirExit(Sender: TObject);
var
  FlagWillNeedARefresh:boolean;
begin
  FlagWillNeedARefresh:=FALSE;

  with sender as TLabeledEdit do
  begin
    pmPathHelper.Tag:=0;
    Font.Style:=[];
    EditLabel.Font.Style:=[]; //Text not in bold anymore

    case tag of
      1: //Hot dir name
        begin
          try
            if (Text<>'') AND (Text[1]<>'-') then //Make sure we actually have something, not an attempf of submenu or end of menu
            begin
              if HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName<>Text then //Make sure it's different than what it was
              begin
                HotDirListTemp.FlagModified:=TRUE;
                HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName:=Text;
                FlagWillNeedARefresh:=TRUE;
              end;
            end;
          except
            //Just in case the "Text" is empty to don't show error with Text[1] check.
          end;
        end;

      2: //Hot dir path
        begin
          if (Text<>'') AND (HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_CHANGEPATH) then Text:=IncludeTrailingPathDelimiter(Text);
          if HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath<>Text then //Make sure it's different than what it was
          begin
            HotDirListTemp.FlagModified:=TRUE;
            HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath:=Text;
            HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirExisting:=DirExistUnknown;
          end;
        end;

      3: //Hot dir target
        begin
          if (Text<>'') AND (HotDirListTemp.HotDir[lsHotDir.ItemIndex].Dispatcher=hd_CHANGEPATH) then Text:=IncludeTrailingPathDelimiter(Text);
          if HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget<>Text then //Make sure it's different than what it was
          begin
            HotDirListTemp.FlagModified:=TRUE;
            HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget:=Text;
          end;
        end;
    end;

    if FlagWillNeedARefresh then Refresh_lsHotDir(lsHotDir.ItemIndex,NOTOUCH_SELECTION);
  end;
end;

{ TfrmHotDir.lbleditHotDirEnter }
procedure TfrmHotDir.lbleditHotDirEnter(Sender: TObject);
begin
  with sender as TLabeledEdit do
  begin
    pmPathHelper.Tag:=tag;
    Font.Style:=[fsBold];
    EditLabel.Font.Style:=[fsBold];
  end;
end;

{ TfrmHotDir.lbleditHotDirKeyPress }
procedure TfrmHotDir.lbleditHotDirKeyPress(Sender: TObject; var Key: char);
begin
  case ord(Key) of
    $0D: //Enter? Let's save the field and go to next one
      begin
        Key:=#00;
        with Sender as TLabeledEdit do
        begin
          case tag of
            1: //HotDirName
              begin
                if lbleditHotDirPath.CanFocus then lbleditHotDirPath.SetFocus else if lsHotDir.CanFocus then lsHotDir.SetFocus;
              end;
            2: //HotDirPath
              begin
                if lbleditHotDirTarget.CanFocus then lbleditHotDirTarget.SetFocus else if lsHotDir.CanFocus then lsHotDir.SetFocus;;
              end;
            3: //HotDirTarget
              begin
                if lsHotDir.CanFocus then lsHotDir.SetFocus else if lsHotDir.CanFocus then lsHotDir.SetFocus;
              end;
          end;
        end;
      end;

    $1B: //Escape? Place back the fields like they were
      begin
        Key:=#00;
        with Sender as TLabeledEdit do
        begin
          case tag of
            1: lsHotDirClick(lsHotDir);
            2: Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath;
            3: Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget;
          end;
        end;
        lsHotDir.SetFocus;
      end;
  end;
end;

{ TfrmHotDir.lbleditHotDirMouseDown }
procedure TfrmHotDir.lbleditHotDirMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with Sender as TComponent do pmPathHelper.Tag:=tag;
end;

{ TfrmHotDir.TimerDragMoveTimer }
procedure TfrmHotDir.TimerDragMoveTimer(Sender: TObject);
begin
  TimerDragMove.Enabled:=FALSE;
  FlagAlreadyScrolled:=FALSE;
end;

{ TfrmHotDir.Refresh_lsHotDir }
procedure TfrmHotDir.Refresh_lsHotDir(IndexToSelect:longint; FlagCenterSelection:boolean);
  var
    RememberFirstInList,MaybeTopPosition:Integer;
  begin
    if lsHotDir.Count>0 then RememberFirstInList:=lsHotDir.TopIndex else RememberFirstInList:=-1;

    lsHotDir.Clear;
    HotDirListTemp.LoadToStringList(lsHotDir.Items);
    lsHotDirSetNumberOfItemsPerColumnAndItemHeight;

    if FlagCenterSelection=NOTOUCH_SELECTION then
    begin
      if (RememberFirstInList<>-1) AND (RememberFirstInList<lsHotDir.Count) then lsHotDir.TopIndex:=RememberFirstInList;
    end;

    if (IndexToSelect<>-1) then
    begin
      if IndexToSelect<HotDirListTemp.Count then
        lsHotDir.ItemIndex:=IndexToSelect
      else
        lsHotDir.ItemIndex:=pred(HotDirListTemp.Count);
    end
    else
    begin
      if lsHotDir.Items.Count > 0 then lsHotDir.ItemIndex:= 0;
    end;

    //To help user to see the current selection, we could be ask to try to center it visually in the displayed list.
    if FlagCenterSelection=CENTER_SELECTION then
    begin
      if lsHotDir.Columns<2 then
      begin
        MaybeTopPosition:=lsHotDir.ItemIndex-((lsHotDir.Height div lsHotDirItemHeight) div 2);
      end
      else
      begin
        //In multi-column list, we don't have a control resolution of "1" which item starts the visual list.
        //So let's try at least to try to have the slected item in the column closer the middle.
        MaybeTopPosition:=0;
        while (MaybeTopPosition+lsHotDirNbOfItemsPerColumn) < lsHotDir.ItemIndex do  MaybeTopPosition:=(MaybeTopPosition+lsHotDirNbOfItemsPerColumn);
        MaybeTopPosition:=MaybeTopPosition-(((lsHotDir.Columns-1) div 2) * lsHotDirNbOfItemsPerColumn);
      end;

      if MaybeTopPosition<0 then MaybeTopPosition:=0;
      lsHotDir.TopIndex:=MaybeTopPosition;
    end;

   lsHotDirClick(lsHotDir);
  end;

{ TfrmHotDir.SubmitToAddOrConfigToHotDirDlg }
procedure TfrmHotDir.SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher:longint; paramActiveFramePath,paramNotActiveFramePath:string);
var
  CloserIndex:longint;
  sTempo:string;
begin
  paramActiveFramePath:=ExcludeTrailingPathDelimiter(paramActiveFramePath);
  paramNotActiveFramePath:=ExcludeTrailingPathDelimiter(paramNotActiveFramePath);
  ActiveFramePath:=paramActiveFramePath;
  NotActiveFramePath:=paramNotActiveFramePath;
//2014-05-19:Following lines removed because sometimes the path is very long and it makes the popup menu too wide for no reason or for reason not often useful.
//miAddActiveFramePath.Caption:='Set active frame path ('+paramActiveFramePath+')';
//miAddNotActiveFramePath.Caption:='Set not active frame path ('+paramNotActiveFramePath+')';

  HotDirListTemp:=THotDirList.Create;
  gHotDirList.CopyListToHotDirList(HotDirListTemp);

  case paramActionDispatcher of
    ACTION_ADDTOHOTLIST:
      begin
        if miAddTarget.Checked then sTempo:=paramNotActiveFramePath else sTempo:='';
        CloserIndex:=ActualAddDirectories(hd_CHANGEPATH,GetLastDir(paramActiveFramePath),paramActiveFramePath,sTempo,HotDirListTemp.TryToGetCloserHotDir(paramActiveFramePath,paramActionDispatcher));
        lbleditHotDirName.TabOrder:=0;
        lbleditHotDirPath.TabOrder:=1;
        lbleditHotDirTarget.TabOrder:=2;
        lsHotDir.TabOrder:=3;
      end;

    ACTION_CONFIGTOHOTLIST:
      begin
        CloserIndex:=HotDirListTemp.TryToGetCloserHotDir(paramActiveFramePath,paramActionDispatcher);
        HotDirListTemp.FlagModified:=FALSE;
        lsHotDir.TabOrder:=0;
        lbleditHotDirName.TabOrder:=1;
        lbleditHotDirPath.TabOrder:=2;
        lbleditHotDirTarget.TabOrder:=3;
      end
    else
      begin
        CloserIndex:=0;
      end;
  end;

  Refresh_lsHotDir(CloserIndex,CENTER_SELECTION);
end;

end.

