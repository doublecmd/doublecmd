unit fHotDir;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, EditBtn, ExtCtrls,
  Menus, Dialogs, KASPathEdit, uHotDir;

type
  { TfrmHotDir }
  TfrmHotDir = class(TForm)
    bynOk: TBitBtn;
    btnGoToDir: TBitBtn;
    btnCancel: TBitBtn;
    btnTestMenu: TBitBtn;
    btnRelativePath: TButton;
    btnRelativeTarget: TButton;
    Label1: TLabel;
    lbleditHotDirName: TLabeledEdit;
    lbleditHotDirPath: TLabeledEdit;
    lbleditHotDirTarget: TLabeledEdit;
    lsHotDir: TListBox;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miAddActiveFramePath: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    miAddNotActiveFramePath: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miMakeRelativeDC: TMenuItem;
    miMakeAbsolute: TMenuItem;
    OpenDialog1: TOpenDialog;
    pmPathHelper: TPopupMenu;
    pmHotDirTestMenu: TPopupMenu;
    pmAddHotDirMenu: TPopupMenu;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddHotDirClick(Sender: TObject);
    procedure btnGoToDirClick(Sender: TObject);
    procedure miImportTotalCommanderClick(Sender: TObject);
    procedure btnRelativeClick(Sender: TObject);
    procedure btnTestMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure lbleditHotDirEditingDone(Sender: TObject);
    procedure lbleditHotDirEnter(Sender: TObject);
    procedure lbleditHotDirExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbleditHotDirKeyPress(Sender: TObject; var Key: char);
    procedure lsHotDirClick(Sender: TObject);
    procedure lsHotDirDblClick(Sender: TObject);
    procedure lsHotDirDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lsHotDirDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lsHotDirMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miPlayPathClick(Sender: TObject);
    procedure miShowWhereItWouldGo(Sender: TObject);
    function ActualAddDirectories(sName, sPath, sTarget:string; PositionOfInsertion:longint):longint;
  private
    { Private declarations }
    HotDirListTemp: THotDirList;
  public
    { Public declarations }
    ActiveFramePath,NotActiveFramePath:string;
    procedure Refresh_lsHotDir(IndexToSelect:longint);
    procedure SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher:longint; paramActiveFramePath,paramNotActiveFramePath:string);
  end;


implementation

{$R *.lfm}

uses
  //Lazarus
  Graphics,

  //Component
  DCClassesUtf8,

  //Double Commander
  DCStrUtils, uGlobs, uLng, uDCUtils, uDebug;

procedure TfrmHotDir.SubmitToAddOrConfigToHotDirDlg(paramActionDispatcher:longint; paramActiveFramePath,paramNotActiveFramePath:string);
var
  CloserIndex:longint;
begin
  ActiveFramePath:=paramActiveFramePath;
  NotActiveFramePath:=paramNotActiveFramePath;
  miAddActiveFramePath.Caption:='Set active frame path ('+paramActiveFramePath+')';
  miAddNotActiveFramePath.Caption:='Set not active frame path ('+paramNotActiveFramePath+')';

  HotDirListTemp:=THotDirList.Create;
  CopyHotDirList(gHotDirList,HotDirListTemp);

  case paramActionDispatcher of
    ACTION_ADDTOHOTLIST:
      begin
        CloserIndex:=ActualAddDirectories(GetLastDir(paramActiveFramePath),paramActiveFramePath,paramNotActiveFramePath,HotDirListTemp.TryToGetCloserHotDir(paramActiveFramePath));
        lbleditHotDirName.TabOrder:=0;
        lbleditHotDirPath.TabOrder:=1;
        lbleditHotDirTarget.TabOrder:=2;
        lsHotDir.TabOrder:=3;
      end;

    ACTION_CONFIGTOHOTLIST:
      begin
        CloserIndex:=HotDirListTemp.TryToGetCloserHotDir(paramActiveFramePath);
        HotDirListTemp.FlagModified:=FALSE;
        lsHotDir.TabOrder:=0;
        lbleditHotDirName.TabOrder:=1;
        lbleditHotDirPath.TabOrder:=2;
        lbleditHotDirTarget.TabOrder:=3;
      end
    else
      begin
        DCDebug('ERROR: "SubmitToAddOrConfigToHotDirDlg" called with incorrect "paramActionDispatcher"');
        CloserIndex:=0;
      end;
  end;

  Refresh_lsHotDir(CloserIndex);
end;

procedure TfrmHotDir.Refresh_lsHotDir(IndexToSelect:longint);
var
  RememberFirstInList:Integer;
begin
  if lsHotDir.Count>0 then RememberFirstInList:=lsHotDir.TopIndex else RememberFirstInList:=-1;
  lsHotDir.Clear;

  HotDirListTemp.LoadToStringList(lsHotDir.Items);
  if (RememberFirstInList<>-1) AND (RememberFirstInList<lsHotDir.Count) then lsHotDir.TopIndex:=RememberFirstInList;

  btnDelete.Enabled:= (lsHotDir.Items.Count > 0);

  if (IndexToSelect<>-1) AND (IndexToSelect<HotDirListTemp.Count) then
    begin
      lsHotDir.ItemIndex:=IndexToSelect;
    end
  else
    begin
      if lsHotDir.Items.Count > 0 then lsHotDir.ItemIndex:= 0;
    end;

  lsHotDirClick(lsHotDir);
end;

procedure TfrmHotDir.btnDeleteClick(Sender: TObject);
begin
  if lsHotDir.ItemIndex=-1 then Exit;

  if isHotDirSubMenuStart(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName) then
    begin
      HotDirListTemp.DeleteHotDirMenuDelimiters(lsHotDir.ItemIndex);
    end
  else
    begin
      if not isHotDirSubMenuEnd(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName) then //for security, juste make sure to not delete a end menu
        begin
          HotDirListTemp.DeleteHotDir(lsHotDir.ItemIndex);
          if lsHotDir.ItemIndex>=HotDirListTemp.Count then lsHotDir.ItemIndex:=lsHotDir.ItemIndex-1;
        end;
    end;

  Refresh_lsHotDir(lsHotDir.ItemIndex);
end;

procedure TfrmHotDir.btnAddClick(Sender: TObject);
begin
  pmAddHotDirMenu.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

function TfrmHotDir.ActualAddDirectories(sName, sPath, sTarget:string; PositionOfInsertion:longint):longint;
var
  LocalHotDir:THotDir;

begin
  if sName<>'' then
    begin
      LocalHotDir:=THotDir.Create;
      LocalHotDir.HotDirName:=sName;
      LocalHotDir.HotDirPath:=sPath;
      LocalHotDir.HotDirTarget:=sTarget;
      if (PositionOfInsertion=-1) OR (PositionOfInsertion>=pred(HotDirListTemp.Count)) then
        begin
          HotDirListTemp.Add(LocalHotDir);
          result:=pred(HotDirListTemp.Count);
        end
      else
        begin
          HotDirListTemp.Insert(PositionOfInsertion,LocalHotDir);
          result:=PositionOfInsertion;
        end;
    end;
end;

procedure TfrmHotDir.btnAddHotDirClick(Sender: TObject);
var
  sName, sPath: String;
  Dispatcher,PositionOfInsertion:longint;
begin
  with Sender as TComponent do Dispatcher:=tag;
  PositionOfInsertion:=lsHotDir.ItemIndex+1; //It's a ADD, not an INSERT so we ADD a-f-t-e-r! If're on last item, don't worry, "ActualAddDirectories" will return correct point of insertion
  sName:='';
  sPath:='';

  case Dispatcher of
    1:
      begin
        if SelectDirectory(rsSelectDir, ActiveFramePath, sPath, False) then PositionOfInsertion:=ActualAddDirectories(GetLastDir(sPath),sPath,'',PositionOfInsertion);
      end;

    2:
      begin
        PositionOfInsertion:=ActualAddDirectories('Hot dir name','Hot dir path','Hot dir target',PositionOfInsertion);
      end;

    3:
      begin
        PositionOfInsertion:=ActualAddDirectories(GetLastDir(ActiveFramePath),ActiveFramePath,'',PositionOfInsertion);
      end;

    4:
      begin
        PositionOfInsertion:=ActualAddDirectories(GetLastDir(ActiveFramePath),ActiveFramePath,NotActiveFramePath,PositionOfInsertion);
      end;

    5:
      begin
        PositionOfInsertion:=ActualAddDirectories('-','','',PositionOfInsertion);
      end;

    6:
      begin
        PositionOfInsertion:=ActualAddDirectories('-Submenu name','','',PositionOfInsertion);
        inc(PositionOfInsertion);
        PositionOfInsertion:=ActualAddDirectories('Hot dir name','Hot dir path','Hot dir target',PositionOfInsertion);
        inc(PositionOfInsertion);
        PositionOfInsertion:=ActualAddDirectories('--','','',PositionOfInsertion);
        dec(PositionOfInsertion);
      end;
  end;

  Refresh_lsHotDir(PositionOfInsertion);
end;

procedure TfrmHotDir.btnGoToDirClick(Sender: TObject);
begin

end;

procedure TfrmHotDir.miImportTotalCommanderClick(Sender: TObject);
const
  TC_SECTIONNAME:string='DirMenu';
  DEFAULTNOTPRESENT:string='Mylene233528DE';
var
  LocalHotDir:THotDir;
  ConfigFile:TIniFileEx;
  sName, sPath, sTarget:UTF8String;
  InitialNumberOfItems,IndexMenu:longint;
begin
  OpenDialog1.Title:='Locate TC "wincmd.ini" file';
  if OpenDialog1.Execute then
    begin
      InitialNumberOfItems:=HotDirListTemp.Count;
      IndexMenu:=1;
      ConfigFile:=TIniFileEx.Create(OpenDialog1.Filename);

      try
        repeat
          sName:=ConfigFile.ReadString(TC_SECTIONNAME,'menu'+IntToStr(IndexMenu),DEFAULTNOTPRESENT);
          if (sName<>DEFAULTNOTPRESENT) then
            begin
              sPath:=ConfigFile.ReadString(TC_SECTIONNAME,'cmd'+IntToStr(IndexMenu),'');
              if length(sPath)>3 then if pos('cd ',lowercase(sPath))=1 then sPath:=copy(sPath,4,length(sPath)-3);
              sTarget:=ConfigFile.ReadString(TC_SECTIONNAME,'path'+IntToStr(IndexMenu),'');
              if length(sTarget)>3 then if pos('cd ',lowercase(sTarget))=1 then sTarget:=copy(sTarget,4,length(sTarget)-3);

              sName:=AnsiToUTF8(sName);
              sName:=StringReplace(sName,'&','',[rfReplaceAll, rfIgnoreCase]);
              sPath:=AnsiToUTF8(sPath);
              sPath:=StringReplace(sPath,'%COMMANDER_PATH%','%commander_path%',[rfReplaceAll, rfIgnoreCase]);
              if length(sPath)>1 then if sPath[length(sPath)]<>'\' then sPath:=sPath+PathDelim; //Not an obligation but DC convention seems to like a backslash at the end
              sTarget:=AnsiToUTF8(sTarget);
              sTarget:=StringReplace(sTarget,'%COMMANDER_PATH%','%commander_path%',[rfReplaceAll, rfIgnoreCase]);
              if length(sTarget)>1 then if sTarget[length(sTarget)]<>'\' then sTarget:=sTarget+PathDelim; //Not an obligation but DC convention seems to like a backslash at the end

              LocalHotDir:=THotDir.Create;
              LocalHotDir.HotDirName:=sName;
              LocalHotDir.HotDirPath:=sPath;
              LocalHotDir.HotDirTarget:=sTarget;
              HotDirListTemp.Add(LocalHotDir);
            end;
          inc(IndexMenu);
        until sName=DEFAULTNOTPRESENT;
      finally
        FreeAndNil(ConfigFile);
      end;

      Refresh_lsHotDir(lsHotDir.ItemIndex);
      btnDelete.Enabled:= (lsHotDir.Items.Count > 0);

      MessageDlg('Directory entries added: '+IntToStr(HotDirListTemp.Count-InitialNumberOfItems),mtInformation,[mbOk],0);
    end;
end;

procedure TfrmHotDir.btnRelativeClick(Sender: TObject);
begin
  with Sender as TButton do pmPathHelper.Tag:=tag;
  pmPathHelper.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TfrmHotDir.btnTestMenuClick(Sender: TObject);
var
  p:TPoint;
begin
  HotDirListTemp.CreatePopUpHotDir(pmHotDirTestMenu,POPUPMENU_JUSTDIRECTORIES,@miShowWhereItWouldGo,nil,'');
  p:=lsHotDir.ClientToScreen(Classes.Point(0,0));
  pmHotDirTestMenu.PopUp(p.X,p.Y);
end;

procedure TfrmHotDir.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if (modalResult=mrOk) or (modalResult=mrAll) then
    begin
      CopyHotDirList(HotDirListTemp,gHotDirList);
    end;
end;

procedure TfrmHotDir.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Answer:integer;
begin
  if (modalResult<>mrOk) AND (modalResult<>mrAll) AND (HotDirListTemp.FlagModified) then
    begin
      if modalResult<>mrIgnore then //Don't bother user if he voluntary hit CANCEL. It's clear he doesn't want to save!
        begin
          Answer:=MessageDlg('Directory hotlist has been modified.'+#$0A+'Do you want to save before to exit?',mtConfirmation,[mbYes,mbNo,mbCancel],0);
          CanClose:=((Answer=mrYes) OR (Answer=mrNo));
          if Answer=mrYes then modalResult:=mrOk;
        end;
    end;
end;

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

procedure TfrmHotDir.lbleditHotDirEditingDone(Sender: TObject);
begin
  with Sender as TLabeledEdit do
    begin
      case tag of
        1: if isHotDirSubMenuStart(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName) then HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName:='-'+Text else  HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName:=text;
        2: HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath:=Text;
        3: HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget:=Text;
      end;

      if tag=1 then Refresh_lsHotDir(lsHotDir.ItemIndex);
      HotDirListTemp.FlagModified:=TRUE;
    end;
end;

procedure TfrmHotDir.lbleditHotDirEnter(Sender: TObject);
begin
  with sender as TLabeledEdit do
  begin
    pmPathHelper.Tag:=tag;
    Font.Style:=[fsBold];
    EditLabel.Font.Style:=[fsBold];
  end;
end;

procedure TfrmHotDir.lbleditHotDirExit(Sender: TObject);
begin
  with sender as TLabeledEdit do
  begin
    pmPathHelper.Tag:=0;
    Font.Style:=[];
    EditLabel.Font.Style:=[];
  end;
end;

procedure TfrmHotDir.FormCreate(Sender: TObject);
begin
  // Initialize property storage
  InitPropStorage(Self);
  ActiveFramePath:='';
  NotActiveFramePath:='';
end;

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
                //if isHotDirSubMenuStart(gHotDirList.HotDir[lsHotDir.ItemIndex].HotDirName) then gHotDirList.HotDir[lsHotDir.ItemIndex].HotDirName:='-'+Text else  gHotDirList.HotDir[lsHotDir.ItemIndex].HotDirName:=text;
                //lsHotDir.Items.Strings[lsHotDir.ItemIndex]:=Text;
                //LoadFromGlob(lsHotDir.ItemIndex);
                if lbleditHotDirPath.CanFocus then lbleditHotDirPath.SetFocus;
              end;
            2: //HotDirPath
              begin
                //gHotDirList.HotDir[lsHotDir.ItemIndex].HotDirPath:=Text;
                if lbleditHotDirTarget.CanFocus then lbleditHotDirTarget.SetFocus;
              end;
            3: //HotDirTarget
              begin
                //gHotDirList.HotDir[lsHotDir.ItemIndex].HotDirTarget:=Text;
                if lsHotDir.CanFocus then lsHotDir.SetFocus;
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

procedure TfrmHotDir.lsHotDirClick(Sender: TObject);
begin
  if lsHotDir.ItemIndex<>-1 then
  begin
    if isHotDirSubMenuStart(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName) then
      begin
        lbleditHotDirName.EditLabel.Caption:='Menu:';
        lbleditHotDirName.Text:=rightstr(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName,length(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName)-1);
        lbleditHotDirName.Enabled:=TRUE;
        lbleditHotDirPath.Visible:=FALSE;
        lbleditHotDirTarget.Visible:=FALSE;
        btnGoToDir.Enabled:=FALSE;
        btnDelete.Enabled:=TRUE;
      end
    else
      begin
        if isHotDirSubMenuEnd(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName) then
          begin
            lbleditHotDirName.EditLabel.Caption:='';
            lbleditHotDirName.Text:='(end of sub menu)';
            lbleditHotDirName.Enabled:=FALSE;
            lbleditHotDirPath.Visible:=FALSE;
            lbleditHotDirTarget.Visible:=FALSE;
            btnGoToDir.Enabled:=FALSE;
            btnDelete.Enabled:=FALSE;
          end
        else
          begin
            if isHotDirSeparator(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName) then
              begin
                lbleditHotDirName.EditLabel.Caption:='';
                lbleditHotDirName.Text:='(separator)';
                lbleditHotDirName.Enabled:=FALSE;
                lbleditHotDirPath.Visible:=FALSE;
                lbleditHotDirTarget.Visible:=FALSE;
                btnGoToDir.Enabled:=FALSE;
                btnDelete.Enabled:=TRUE;
              end
            else
              begin
                lbleditHotDirName.EditLabel.Caption:='Name:';
                lbleditHotDirName.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName;
                lbleditHotDirName.Enabled:=TRUE;
                lbleditHotDirPath.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath;
                lbleditHotDirTarget.Text:=HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget;
                lbleditHotDirPath.Visible:=TRUE;
                lbleditHotDirTarget.Visible:=TRUE;
                btnGoToDir.Enabled:=TRUE;
                btnDelete.Enabled:=TRUE;
              end;
          end;
      end;

    btnRelativePath.Visible:=lbleditHotDirPath.Visible;
    btnRelativeTarget.Visible:=lbleditHotDirTarget.Visible;
  end;
end;

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

procedure TfrmHotDir.lsHotDirDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  CurIndex, NewIndex: Integer;
begin
  CurIndex := lsHotDir.ItemIndex;
  if CurIndex = -1 then Exit;
  NewIndex := lsHotDir.GetIndexAtY(Y);
  if (NewIndex < 0) or (NewIndex >= lsHotDir.Count) then
    NewIndex := lsHotDir.Count - 1;

  if not isHotDirSubMenuStart(HotDirListTemp.HotDir[CurIndex].HotDirName) then
    begin
      HotDirListTemp.Move(CurIndex, NewIndex);
    end
  else
    begin
      HotDirListTemp.MoveHotDirMenu(CurIndex, NewIndex);
    end;

  Refresh_lsHotDir(NewIndex);
end;

procedure TfrmHotDir.lsHotDirDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Y<20 then
  begin
    lsHotDir.TopIndex:=lsHotDir.TopIndex-1; //No need to valite if on top already, code is doing it for us!
  end
  else
  begin
    if (Y+20)>lsHotDir.Height then
    begin
      lsHotDir.TopIndex:=lsHotDir.TopIndex+1;  //No need to valite if at bottom already, code is doing it for us!
    end
    else
    begin
      if (Source = lsHotDir) and (lsHotDir.ItemIndex<>-1) then
        begin
          Accept:=not isHotDirSubMenuEnd(HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirName); //We don't want "MenuEnd" to be moveable.
        end
      else
        begin
          Accept:=FALSE;
        end;
    end;
  end;
end;

procedure TfrmHotDir.lsHotDirMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  lsHotDirClick(lsHotDir); //Might looks stupid an unnecessary but it is to avoid the fact that "OnClick" is not triggered in some circonstances.
                           //Example? Suppose the focus is on element #n. Suppose we press the down arrow to select n+1, if with the mouse we then click on element #n,
                           //the element is really selected BUT the event "OnClick" is not triggered (at least on Windows Vista) BUT OnMouseDown is triggered.
end;

procedure TfrmHotDir.miShowWhereItWouldGo(Sender: TObject);
var
  StringToShow:string;
begin
  with Sender as TComponent do
    begin
      lsHotDir.ItemIndex:=tag;
      lsHotDir.TopIndex:=tag-((lsHotDir.Height div lsHotDir.ItemHeight) div 2);

      StringToShow:='This is hot dir named '+HotDirListTemp.HotDir[tag].HotDirName;
      StringToShow:=StringToShow+#$0D+#$0A+#$0D+#$0A+'This will change active frame to the following path:';
      StringToShow:=StringToShow+#$0D+#$0A+mbExpandFileName(HotDirListTemp.HotDir[tag].HotDirPath);

      if HotDirListTemp.HotDir[tag].HotDirTarget<>'' then
        begin
          StringToShow:=StringToShow+#$0D+#$0A+#$0D+#$0A+'And inactive frame would change to the following path:';
          StringToShow:=StringToShow+#$0D+#$0A+mbExpandFileName(HotDirListTemp.HotDir[tag].HotDirTarget);
        end;

      MessageDlg(StringToShow,mtInformation,[mbOk],0);
    end;
end;

procedure TfrmHotDir.miPlayPathClick(Sender: TObject);
var
  WorkingPath:string;
  DispatcherAction:longint;
begin
  with Sender as TComponent do DispatcherAction:=tag;

  case DispatcherAction of
    1: // Make path relative to %commander_path%
      begin
        case pmPathHelper.tag of
          2: //HotDir Path
            begin
              WorkingPath:=ExtractRelativePath((ReplaceEnvVars('%commander_path%')+PathDelim),lbleditHotDirPath.Text);
              if WorkingPath<>lbleditHotDirPath.Text then lbleditHotDirPath.Text:='%commander_path%'+PathDelim+WorkingPath;
            end;
          3: //HotDir Target
            begin
              WorkingPath:=ExtractRelativePath((ReplaceEnvVars('%commander_path%')+PathDelim),lbleditHotDirTarget.Text);
              if WorkingPath<>lbleditHotDirTarget.Text then lbleditHotDirTarget.Text:='%commander_path%'+PathDelim+WorkingPath;
            end;
        end;
      end;

    2: // Make path absolute
      begin
        case pmPathHelper.tag of
          2: lbleditHotDirPath.Text:=mbExpandFileName(lbleditHotDirPath.Text); //HotDir Path
          3: lbleditHotDirTarget.Text:=mbExpandFileName(lbleditHotDirTarget.Text); //HotDir Target
        end;
      end;

    3: //Add path from active frame
      begin
        case pmPathHelper.tag of
          2: lbleditHotDirPath.Text:=ActiveFramePath; //HotDir Path
          3: lbleditHotDirTarget.Text:=ActiveFramePath; //HotDir Target
        end;
      end;

    4: //Add path from not active frame
      begin
        case pmPathHelper.tag of
          2: lbleditHotDirPath.Text:=NotActiveFramePath; //HotDir Path
          3: lbleditHotDirTarget.Text:=NotActiveFramePath; //HotDir Target
        end;
      end;

    5: //Browse and add selected directory
      begin
        case pmPathHelper.tag of
          2: if SelectDirectory(rsSelectDir, ActiveFramePath, WorkingPath, False) then lbleditHotDirPath.Text:=WorkingPath;
          3: if SelectDirectory(rsSelectDir, NotActiveFramePath, WorkingPath, False) then lbleditHotDirTarget.Text:=WorkingPath;
        end;
      end;
  end;

  case pmPathHelper.tag of
    2: if HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirPath<>lbleditHotDirPath.Text then lbleditHotDirEditingDone(lbleditHotDirPath);
    3: if HotDirListTemp.HotDir[lsHotDir.ItemIndex].HotDirTarget<>lbleditHotDirTarget.Text then lbleditHotDirEditingDone(lbleditHotDirTarget);
  end;
end;

end.

