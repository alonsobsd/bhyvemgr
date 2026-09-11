unit form_storage_snapshots;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Grids, ComCtrls;

type

  { TFormStorageSnapshot }

  TFormStorageSnapshot = class(TForm)
    BitBtnClose: TBitBtn;
    BitBtnCreateSnapshot: TBitBtn;
    BitBtnRemoveSnapshot: TBitBtn;
    BitBtnRestoreSnapshot: TBitBtn;
    EditSnapshotSuffix: TEdit;
    EditVirtualMachine: TEdit;
    GroupBox9: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    StatusBarStorageSnapshot: TStatusBar;
    StringGridSnapshots: TStringGrid;
    procedure BitBtnCreateSnapshotClick(Sender: TObject);
    procedure BitBtnRemoveSnapshotClick(Sender: TObject);
    procedure EditSnapshotSuffixEditingDone(Sender: TObject);
    procedure StringGridSnapshotsClick(Sender: TObject);
    procedure StringGridSnapshotsSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
  private
    RowNumber : Integer;
  public
    property SnapshotRowNumber: Integer read RowNumber;
    procedure ClearStringGrid(SnapshotGrid: TStringGrid);
    procedure LoadDefaultValues();
    procedure LoadSnapshots();
  end;

var
  FormStorageSnapshot: TFormStorageSnapshot;

implementation

{$R *.lfm}

uses
  RegExpr, StrUtils, unit_component, unit_util, unit_helper_client, unit_language;

{ TFormStorageSnapshot }

procedure TFormStorageSnapshot.BitBtnCreateSnapshotClick(Sender: TObject);
var
  VmName : String;
  SnapshotSuffix : String;
begin
  VmName:=EditVirtualMachine.Text;
  SnapshotSuffix:=EditSnapshotSuffix.Text;

  StatusBarStorageSnapshot.SimpleText:=EmptyStr;

  if not SnapshotSuffix.IsEmpty then
  begin
    if not CheckSnapshotSuffix(SnapshotSuffix) then
    begin
      StatusBarStorageSnapshot.Font.Color:=clRed;
      StatusBarStorageSnapshot.SimpleText:=Format(storage_snapshot_check_suffix, [SnapshotSuffix]);
      Exit;
    end;
  end;

  if CheckVmRunning(VmName) > 0 then
  begin
    StatusBarStorageSnapshot.Font.Color:=clRed;
    StatusBarStorageSnapshot.SimpleText:=Format(storage_snapshot_not_create, [VmName]);
    Exit;
  end;

  if MessageDialog(mtConfirmation, Format(storage_snapshot_create_confirmation, [VmName])) = mrYes then
  begin
    if ZfsCreateSnapshotHelper(VmName, SnapshotSuffix) then
    begin
      StatusBarStorageSnapshot.Font.Color:=clTeal;
      StatusBarStorageSnapshot.SimpleText:=storage_snapshot_create_success;

      EditSnapshotSuffix.Clear;
      ClearStringGrid(StringGridSnapshots);
      LoadSnapshots();
    end;
  end;
end;

procedure TFormStorageSnapshot.BitBtnRemoveSnapshotClick(Sender: TObject);
var
  VmName : String;
  Snapshot : String;
begin
  VmName:=EditVirtualMachine.Text;
  Snapshot:=EmptyStr;

  StatusBarStorageSnapshot.SimpleText:=EmptyStr;

  if RowNumber > 0 then
  begin
    Snapshot:=ExtractDelimited(2, StringGridSnapshots.Cells[0, RowNumber], ['@']);

    if MessageDialog(mtConfirmation, Format(storage_snapshot_remove_confirmation, [VmName+'@'+Snapshot])) = mrYes then
    begin
      if ZfsDestroySnapshotHelper(VmName, Snapshot) then
      begin
        StringGridSnapshots.DeleteRow(RowNumber);
        BitBtnRemoveSnapshot.Enabled:=False;

        StatusBarStorageSnapshot.Font.Color:=clTeal;
        StatusBarStorageSnapshot.SimpleText:=storage_snapshot_remove_success;
      end;
    end;
  end;
end;

procedure TFormStorageSnapshot.EditSnapshotSuffixEditingDone(Sender: TObject);
begin
  EditSnapshotSuffix.Text:=StringReplace(EditSnapshotSuffix.Text, ' ', '_', [rfReplaceAll]);
end;

procedure TFormStorageSnapshot.StringGridSnapshotsClick(Sender: TObject);
begin
  if RowNumber > 0 then
  begin
    BitBtnRemoveSnapshot.Enabled:=True;
    BitBtnRestoreSnapshot.Enabled:=True;
  end;
end;

procedure TFormStorageSnapshot.StringGridSnapshotsSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
begin
  RowNumber:=aRow;
end;

procedure TFormStorageSnapshot.LoadSnapshots();
var
  Snapshots : TStringList;
  RegexObj: TRegExpr;
begin
  Snapshots:=TStringList.Create();

  Snapshots.Text:=GetStorageSnapshotList(EditVirtualMachine.Text);

  RegexObj := TRegExpr.Create('(\S+)\s+(\S+)\s+(\S+)');

  if RegexObj.Exec(Snapshots.Text) then
  begin
    repeat
      StringGridSnapshots.InsertRowWithValues(StringGridSnapshots.RowCount, [RegexObj.Match[1], RegexObj.Match[3]]);
    until not RegexObj.ExecNext;
  end;

  RegexObj.Free;
  Snapshots.Free;
end;

procedure TFormStorageSnapshot.ClearStringGrid(SnapshotGrid: TStringGrid);
var
  i : Integer;
begin
  for i:=1 to SnapshotGrid.RowCount-1 do
  begin
    SnapshotGrid.DeleteRow(SnapshotGrid.RowCount-1);
  end;
end;

procedure TFormStorageSnapshot.LoadDefaultValues();
begin
  BitBtnRestoreSnapshot.Enabled:=False;
  BitBtnRemoveSnapshot.Enabled:=False;

  EditSnapshotSuffix.Clear;

  ClearStringGrid(StringGridSnapshots);
  LoadSnapshots();

  StatusBarStorageSnapshot.Font.Color:=clTeal;
  StatusBarStorageSnapshot.SimpleText:=virtual_machine+' : '+EditVirtualMachine.Text;
end;

end.

