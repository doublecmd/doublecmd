unit fSelectDuplicates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, KASComboBox, uFileView;

type

  { TfrmSelectDuplicates }

  TfrmSelectDuplicates = class(TForm)
    btnApply: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnIncludeMask: TSpeedButton;
    btnExcludeMask: TSpeedButton;
    cmbFirstMethod: TComboBoxAutoWidth;
    cmbIncludeMask: TComboBox;
    cmbExcludeMask: TComboBox;
    chkLeaveUnselected: TCheckBox;
    cmbSecondMethod: TComboBoxAutoWidth;
    lblIncludeMask: TLabel;
    lblExcludeMask: TLabel;
    lblFirstMethod: TLabel;
    lblSecondMethod: TLabel;
    pnlMethods: TPanel;
    pnlButtons: TPanel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnIncludeMaskClick(Sender: TObject);
    procedure cmbFirstMethodChange(Sender: TObject);
  private
    FFileView: TFileView;
  private
    procedure ButtonsAutosize;
  end;

procedure ShowSelectDuplicates(TheOwner: TCustomForm; AFileView: TFileView);

implementation

{$R *.lfm}

uses
  uFile, uFileSorting, uFileFunctions, uDisplayFile, uFileProperty, uTypes,
  uGlobs, fMaskInputDlg, uLng, uSearchTemplate, DCStrUtils;

procedure ShowSelectDuplicates(TheOwner: TCustomForm; AFileView: TFileView);
begin
  with TfrmSelectDuplicates.Create(TheOwner) do
  begin
    ButtonsAutosize;
    FFileView:= AFileView;
    cmbFirstMethod.ItemIndex:= 0;
    cmbSecondMethod.ItemIndex:= 2;
    cmbIncludeMask.Items.Assign(glsMaskHistory);
    cmbExcludeMask.Items.Assign(glsMaskHistory);

    // Localize methods
    ParseLineToList(rsSelectDuplicateMethod, cmbFirstMethod.Items);
    ParseLineToList(rsSelectDuplicateMethod, cmbSecondMethod.Items);
    cmbSecondMethod.Items.Delete(cmbSecondMethod.Items.Count - 1);
    cmbSecondMethod.Items.Delete(cmbSecondMethod.Items.Count - 1);

    if ShowModal = mrOK then
    begin
      btnApplyClick(btnApply);
    end;
    Free;
  end;
end;

{ TfrmSelectDuplicates }

procedure TfrmSelectDuplicates.cmbFirstMethodChange(Sender: TObject);
begin
  cmbSecondMethod.Enabled:= cmbFirstMethod.ItemIndex < 4;
end;

procedure TfrmSelectDuplicates.btnApplyClick(Sender: TObject);
var
  ARange: TRange;
  Index, J: Integer;
  ASelected: Integer;
  AFiles: TDisplayFiles;
  AGroup, AValue: Variant;
  NewSorting: TFileSortings = nil;
begin
  FFileView.MarkGroup(cmbIncludeMask.Text, True);
  if Length(cmbExcludeMask.Text) > 0 then begin
    FFileView.MarkGroup(cmbExcludeMask.Text, False);
  end;

  if not chkLeaveUnselected.Checked then Exit;

  AFiles:= FFileView.DisplayFiles;

  // First sort by group
  SetLength(NewSorting, 1);
  SetLength(NewSorting[0].SortFunctions, 1);
  NewSorting[0].SortFunctions[0] := fsfVariant;
  NewSorting[0].SortDirection := sdAscending;

  case cmbFirstMethod.ItemIndex of
    0, 1: // Newest/Oldest
      begin
        SetLength(NewSorting, 2);
        SetLength(NewSorting[1].SortFunctions, 1);
        NewSorting[1].SortFunctions[0] := fsfModificationTime;
        if (cmbFirstMethod.ItemIndex = 0) then
          // First item will be Oldest
          NewSorting[1].SortDirection := sdAscending
        else begin
          // First item will be Newest
          NewSorting[1].SortDirection := sdDescending;
        end;
      end;
    2, 3: // Largest/Smallest
      begin
        SetLength(NewSorting, 2);
        SetLength(NewSorting[1].SortFunctions, 1);
        NewSorting[1].SortFunctions[0] := fsfSize;
        if (cmbFirstMethod.ItemIndex = 2) then
          // First item will be Largest
          NewSorting[1].SortDirection := sdAscending
        else begin
          // First item will be Smallest
          NewSorting[1].SortDirection := sdDescending;
        end;
      end;
  end;

  if cmbSecondMethod.Enabled then
  begin
    case cmbSecondMethod.ItemIndex of
      0, 1:
        begin
          SetLength(NewSorting, 3);
          SetLength(NewSorting[2].SortFunctions, 1);
          NewSorting[2].SortFunctions[0] := fsfModificationTime;
          if (cmbSecondMethod.ItemIndex = 1) then
            NewSorting[2].SortDirection := sdAscending
          else begin
            NewSorting[2].SortDirection := sdDescending;
          end;
        end;
      2, 3:
        begin
          SetLength(NewSorting, 3);
          SetLength(NewSorting[2].SortFunctions, 1);
          NewSorting[2].SortFunctions[0] := fsfSize;
          if (cmbSecondMethod.ItemIndex = 3) then
            NewSorting[2].SortDirection := sdAscending
          else begin
            NewSorting[2].SortDirection := sdDescending;
          end;
        end;
    end;
  end;

  FFileView.Sorting:= NewSorting;

  // Skip '..' item
  if AFiles[0].FSFile.IsNameValid then
    ARange.First:= 0
  else begin
    ARange.First:= 1;
  end;

  AGroup:= TFileVariantProperty(AFiles[ARange.First].FSFile.Properties[fpVariant]).Value;
  for Index:= ARange.First + 1 to AFiles.Count - 1 do
  begin
    AValue:= TFileVariantProperty(AFiles[Index].FSFile.Properties[fpVariant]).Value;
    if (AValue <> AGroup) then
    begin
      ASelected:= 0;
      ARange.Last:= Index - 1;
      for J:= ARange.First to ARange.Last do
      begin
        if AFiles[J].Selected then Inc(ASelected);
      end;
      // Selected all files in the group
      if ASelected = (Index - ARange.First) then
      begin
        if cmbFirstMethod.ItemIndex = 5 then
          AFiles[ARange.Last].Selected:= False
        else begin
          AFiles[ARange.First].Selected:= False;
        end;
      end;
      AGroup:= AValue;
      ARange.First:= Index;
    end;
  end;
end;

procedure TfrmSelectDuplicates.btnIncludeMaskClick(Sender: TObject);
var
  sMask: String;
  bTemplate: Boolean;
  AComboBox: TComboBox;
begin
  if Sender = btnIncludeMask then
    AComboBox:= cmbIncludeMask
  else begin
    AComboBox:= cmbExcludeMask;
  end;
  sMask:= AComboBox.Text;
  if ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, sMask) then
  begin
    bTemplate:= IsMaskSearchTemplate(sMask);
    AComboBox.Enabled:= not bTemplate;
    AComboBox.Text:= sMask;
  end;
end;

procedure TfrmSelectDuplicates.ButtonsAutosize;
var
  Index: Integer;
  AControl: TControl;
  AMaxWidth, AMaxHeight: Integer;
begin
  AMaxWidth:= 0;
  AMaxHeight:= 0;
  for Index:= 0 to pnlButtons.ControlCount - 1 do
  begin
    AControl:= pnlButtons.Controls[Index];
    if AControl.Width > AMaxWidth then AMaxWidth:= AControl.Width;
    if AControl.Height > AMaxHeight then AMaxHeight:= AControl.Height;
  end;
  for Index:= 0 to pnlButtons.ControlCount - 1 do
  begin
    AControl:= pnlButtons.Controls[Index];
    AControl.Constraints.MinWidth:= AMaxWidth;
    AControl.Constraints.MinHeight:= AMaxHeight;
  end;
end;

end.

