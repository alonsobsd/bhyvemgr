{ BSD 3-Clause License

Copyright (c) 2024-2025, Alonso Cárdenas <acardenas@bsd-peru.org>

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit form_vm_create;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  Buttons, ComCtrls, LCLType, ExtCtrls, SpinEx, Clipbrd ,unit_thread;

type

  { TFormVmCreate }

  TFormVmCreate = class(TForm)
    BitBtnDownloadPaste: TBitBtn;
    BitBtnDownload: TBitBtn;
    BitBtnCreateVm: TBitBtn;
    BitBtnSshPaste: TBitBtn;
    CheckBoxImageUseSudo: TCheckBox;
    CheckBoxImageMinimal: TCheckBox;
    CheckBoxImageFiles: TCheckBox;
    CheckBoxFramebuffer: TCheckBox;
    CheckBoxIpv6Address: TCheckBox;
    CheckBoxOnlyLocalhost: TCheckBox;
    CheckBoxUEFIBootvars: TCheckBox;
    CheckBoxUseMedia: TCheckBox;
    CheckBoxWaitVNC: TCheckBox;
    ComboBoxSystemType: TComboBox;
    ComboBoxSystemVersion: TComboBox;
    ComboBoxVirtualDeviceType: TComboBox;
    ComboBoxVirtualStorageType: TComboBox;
    EditUrlImage: TEdit;
    EditUsername: TEdit;
    EditSshPubKey: TEdit;
    EditVmDescription: TEdit;
    EditVmFolderPath: TEdit;
    EditVmName: TEdit;
    FileNameEditBootMedia: TFileNameEdit;
    FileNameEditImageFile: TFileNameEdit;
    FileNameEditUserData: TFileNameEdit;
    FileNameEditMetaData: TFileNameEdit;
    FileNameEditNetworkConfig: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBoxDownloadImage: TGroupBox;
    GroupBoxSelectImage: TGroupBox;
    GroupBoxNewVirtualDisk: TGroupBox;
    GroupBoxMinimalConfiguration: TGroupBox;
    GroupBoxFileConfig: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    LabelDownloadStatus: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControlVmCreate: TPageControl;
    ProgressBarImage: TProgressBar;
    RadioButtonDiskFromImage: TRadioButton;
    RadioButtonNewDisk: TRadioButton;
    RadioButtonNotDisk: TRadioButton;
    SpinEditExDiskSize: TSpinEditEx;
    SpinEditExMemory: TSpinEditEx;
    StatusBarVmCreate: TStatusBar;
    TabSheetGeneral: TTabSheet;
    TabSheetImage: TTabSheet;
    procedure BitBtnDownloadClick(Sender: TObject);
    procedure BitBtnDownloadPasteClick(Sender: TObject);
    procedure BitBtnSshPasteClick(Sender: TObject);
    procedure CheckBoxFramebufferChange(Sender: TObject);
    procedure CheckBoxImageFilesChange(Sender: TObject);
    procedure CheckBoxImageMinimalChange(Sender: TObject);
    procedure CheckBoxOnlyLocalhostChange(Sender: TObject);
    procedure CheckBoxUseMediaChange(Sender: TObject);
    procedure CheckBoxWaitVNCChange(Sender: TObject);
    procedure ComboBoxSystemTypeChange(Sender: TObject);
    procedure EditVmNameEditingDone(Sender: TObject);
    procedure EditVmNameExit(Sender: TObject);
    procedure FileNameEditImageFileButtonClick(Sender: TObject);
    procedure FileNameEditImageFileChange(Sender: TObject);
    procedure FileNameEditMetaDataButtonClick(Sender: TObject);
    procedure FileNameEditNetworkConfigButtonClick(Sender: TObject);
    procedure FileNameEditUserDataButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioButtonDiskFromImageClick(Sender: TObject);
    procedure RadioButtonNewDiskChange(Sender: TObject);
    procedure RadioButtonNewDiskClick(Sender: TObject);
    procedure RadioButtonNotDiskChange(Sender: TObject);
    procedure RadioButtonNotDiskClick(Sender: TObject);
  private
    MyAppThread: AppProgressBarThread;
    function ConvertFileToRaw(ImageFile : String):Boolean;
    procedure ShowStatus(Status: Integer);
    procedure EndStatus(Status: Integer; AppName : String);
  public
    function FormValidate():Boolean;
  end;

var
  FormVmCreate: TFormVmCreate;
  RemoteFile, OldRemoteFile : String;

implementation

{$R *.lfm}

uses
  unit_component, unit_global, unit_util;

{ TFormVmCreate }

procedure TFormVmCreate.FormShow(Sender: TObject);
begin
  PageControlVmCreate.ActivePage:=TabSheetGeneral;
  TabSheetImage.TabVisible:=False;

  EditVmName.SetFocus;

  EditVmName.Clear;
  EditVmFolderPath.Clear;

  ComboBoxSystemType.Clear;
  ComboBoxSystemVersion.Clear;

  FillComboSystemType(ComboBoxSystemType);
  EditVmDescription.Clear;

  SpinEditExMemory.Text:='256';

  RadioButtonNotDisk.Checked:=False;
  RadioButtonNewDisk.Checked:=True;

  CheckBoxFramebuffer.Checked:=True;
  CheckBoxWaitVNC.Checked:=True;
  CheckBoxOnlyLocalhost.Checked:=True;
  CheckBoxUEFIBootvars.Checked:=True;
  CheckBoxIpv6Address.Checked:=False;

  if UseIpv6 = 'yes' then
    CheckBoxIpv6Address.Enabled:=True
  else
    CheckBoxIpv6Address.Enabled:=False;

  {$ifdef CPUAARCH64}
  CheckBoxFramebuffer.Enabled:=False;
  CheckBoxWaitVNC.Enabled:=False;
  CheckBoxOnlyLocalhost.Enabled:=False;
  CheckBoxUEFIBootvars.Enabled:=False;
  {$endif}

  ComboBoxVirtualDeviceType.Clear;
  FillComboVirtualDeviceType(ComboBoxVirtualDeviceType);
  ComboBoxVirtualDeviceType.Items.Delete(ComboBoxVirtualDeviceType.Items.IndexOf('ahci-cd'));

  ComboBoxVirtualStorageType.Clear;
  FillComboVirtualStorageType(ComboBoxVirtualStorageType);
  ComboBoxVirtualStorageType.ItemIndex:=-1;
  ComboBoxVirtualStorageType.Enabled:=True;

  SpinEditExDiskSize.Text:='20';

  CheckBoxUseMedia.Checked:=False;
  FileNameEditBootMedia.Clear;

  StatusBarVmCreate.SimpleText:=EmptyStr;
  GroupBoxNewVirtualDisk.Visible:=True;

  EditUrlImage.Clear;
  ProgressBarImage.Position:=0;
  LabelDownloadStatus.Visible:=False;
  LabelDownloadStatus.Caption:=EmptyStr;

  FileNameEditImageFile.Clear;

  CheckBoxImageMinimal.Checked:=False;
  CheckBoxImageFiles.Checked:=False;

  EditUsername.Clear;
  EditSshPubKey.Clear;
  CheckBoxImageUseSudo.Checked:=False;

  FileNameEditUserData.Clear;
  FileNameEditMetaData.Clear;
  FileNameEditNetworkConfig.Clear;

  FormVmCreate.BitBtnCreateVm.Enabled:=True;
end;

procedure TFormVmCreate.RadioButtonDiskFromImageClick(Sender: TObject);
begin
  if RadioButtonDiskFromImage.Checked then
  begin
    TabSheetImage.TabVisible:=True;
    GroupBoxNewVirtualDisk.Enabled:=True;
    GroupBoxNewVirtualDisk.Visible:=True;
    ComboBoxVirtualStorageType.ItemIndex:=0;
    ComboBoxVirtualStorageType.Enabled:=False;
  end
  else
    TabSheetImage.TabVisible:=False;
end;

procedure TFormVmCreate.RadioButtonNewDiskChange(Sender: TObject);
begin
  if RadioButtonNewDisk.Checked then
  begin
    GroupBoxNewVirtualDisk.Enabled:=True;
    GroupBoxNewVirtualDisk.Visible:=True;
  end;
end;

procedure TFormVmCreate.RadioButtonNewDiskClick(Sender: TObject);
begin
  if RadioButtonNewDisk.Checked then
  begin
    TabSheetImage.TabVisible:=False;
    ComboBoxVirtualStorageType.ItemIndex:=-1;
    ComboBoxVirtualStorageType.Enabled:=True;
  end;
end;

procedure TFormVmCreate.RadioButtonNotDiskChange(Sender: TObject);
begin
  if RadioButtonNotDisk.Checked then
  begin
    GroupBoxNewVirtualDisk.Enabled:=False;
    GroupBoxNewVirtualDisk.Visible:=False;
  end;
end;

procedure TFormVmCreate.RadioButtonNotDiskClick(Sender: TObject);
begin
  if RadioButtonNotDisk.Checked then
    TabSheetImage.TabVisible:=False
end;

function TFormVmCreate.ConvertFileToRaw(ImageFile: String): Boolean;
var
  extractType : String;
  NewRemoteFile : String;
begin
  ChDir(CloudVmImagesPath);

  case CheckFileExtension(ImageFile) of
    '.qcow2':
      begin
        if FileExists(OldRemoteFile) then
          RemoveFile(OldRemoteFile);

        OldRemoteFile:=ImageFile;
        NewRemoteFile:=StringReplace(ImageFile, '.qcow2', EmptyStr, [rfIgnoreCase, rfReplaceAll])+'.raw';

        extractType:='qcow2';

        ProgressBarImage.Position:=0;
        ProgressBarImage.Max:=ConvertFileSize(GetExtractSize(ImageFile, extractType), 'M');

        if ProgressBarImage.Max > 0 then
        begin
          MyAppThread := AppProgressBarThread.Create(QemuImgCmd, ['convert', '-O', 'raw', '-S', '0', ImageFile, NewRemoteFile]);

          LabelDownloadStatus.Visible:=True;
          LabelDownloadStatus.Caption:='Extracting...';

          RemoteFile:=NewRemoteFile;

          MyAppThread.OnShowStatus := @ShowStatus;
          MyAppThread.OnEndStatus:= @EndStatus;
          MyAppThread.Start;
        end
        else
        begin
          StatusBarVmCreate.Font.Color:=clRed;
          StatusBarVmCreate.SimpleText:=ExtractFileName(QemuImgCmd) + ' can not retrieve '+ImageFile+' file size.';
        end;
      end;
    '.img',
    '.raw':
      begin
        if FileExists(OldRemoteFile) then
          RemoveFile(OldRemoteFile);

        if FileExists(CloudVmImagesPath+'/'+RemoteFile) and not (CheckFileType(CloudVmImagesPath+'/'+RemoteFile) = 'dos/mbr') then
        begin
          StatusBarVmCreate.Font.Color:=clTeal;
          StatusBarVmCreate.SimpleText:=RemoteFile + ' could not be compatible with bhyve.';
        end;

        SpinEditExDiskSize.Text:=GetFileSize(CloudVmImagesPath+'/'+RemoteFile,'G').ToString;
        FileNameEditImageFile.Text:=CloudVmImagesPath+'/'+RemoteFile;
      end;
    '.qcow2.xz',
    '.raw.xz',
    '.img.xz':
      begin
        OldRemoteFile:=ImageFile;
        NewRemoteFile:=StringReplace(ImageFile, '.xz', EmptyStr, [rfIgnoreCase, rfReplaceAll]);

        extractType:='xz';

        ProgressBarImage.Position:=0;
        ProgressBarImage.Max:=ConvertFileSize(GetExtractSize(ImageFile, extractType), 'M');

        if ProgressBarImage.Max > 0 then
        begin
          MyAppThread := AppProgressBarThread.Create(XzCmd, ['-dk', ImageFile]);

          LabelDownloadStatus.Visible:=True;
          LabelDownloadStatus.Caption:='Extracting...';

          RemoteFile:=NewRemoteFile;

          MyAppThread.OnShowStatus := @ShowStatus;
          MyAppThread.OnEndStatus:= @EndStatus;
          MyAppThread.Start;
        end
        else
        begin
          StatusBarVmCreate.Font.Color:=clRed;
          StatusBarVmCreate.SimpleText:=ExtractFileName(XzCmd) + ' can not retrieve '+ImageFile+' file size.';
        end;
      end;
  end;

  Result:=True;
end;

procedure TFormVmCreate.ShowStatus(Status: Integer);
var
  total : Int64;
begin
  total:=ConvertFileSize(GetFileSize(RemoteFile), 'M');
  ProgressBarImage.Position:=total;
end;

procedure TFormVmCreate.EndStatus(Status: Integer; AppName : String);
var
  total : Int64;
begin
  if (Status = 0) then
  begin
    total:=ConvertFileSize(GetFileSize(RemoteFile), 'M');
    ProgressBarImage.Position:=total;

    LabelDownloadStatus.Caption:='Done';

    Sleep(100);

    ConvertFileToRaw(RemoteFile);
  end
  else if (Status > 0) then
  begin
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:=AppName+' process generated an error: '+Status.ToString;
  end;
end;

function TFormVmCreate.FormValidate(): Boolean;
begin
  Result:=True;

  if (ComboBoxSystemType.ItemIndex=-1) then
  begin
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:='You must select an operating system type. e.g, BSD, Linux, etc.';

    Result:=False;
    Exit;
  end;
  if (ComboBoxSystemVersion.ItemIndex=-1) then
  begin
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:='You must select an operating system version. e.g, FreeBSD 15.x, etc.';

    Result:=False;
    Exit;
  end;

  if RadioButtonNewDisk.Checked or RadioButtonDiskFromImage.Checked then
  begin
    if (ComboBoxVirtualDeviceType.ItemIndex=-1) then
    begin
      StatusBarVmCreate.Font.Color:=clRed;
      StatusBarVmCreate.SimpleText:='You must select a virtual device type. e.g, ahci-hd, nvme, etc.';

      Result:=False;
      Exit;
    end;
    if (ComboBoxVirtualStorageType.ItemIndex=-1) then
    begin
      StatusBarVmCreate.Font.Color:=clRed;
      StatusBarVmCreate.SimpleText:='You must select a virtual storage type. e.g, image file, etc.';

      Result:=False;
      Exit;
    end;
  end;

  if RadioButtonDiskFromImage.Checked then
  begin
    if SpinEditExDiskSize.Value < GetFileSize(FileNameEditImageFile.FileName ,'G') then
    begin
      StatusBarVmCreate.Font.Color:=clRed;
      StatusBarVmCreate.SimpleText:='The virtual disk size can not be less than '+GetFileSize(FileNameEditImageFile.FileName ,'G').ToString+'G';

      Result:=False;
      Exit;
    end;

    if not (FileExists(FileNameEditImageFile.FileName)) then
    begin
      StatusBarVmCreate.Font.Color:=clRed;
      StatusBarVmCreate.SimpleText:='The Cloud/VM image does not exist.';

      Result:=False;
      Exit;
    end;

    if CheckBoxImageMinimal.Checked then
    begin
      if not CheckUserName(EditUsername.Text) and (Trim(EditUsername.Text) = EmptyStr)  then
      begin
        StatusBarVmCreate.Font.Color:=clRed;
        StatusBarVmCreate.SimpleText:='Username is not valid.';

        Result:=False;
        Exit;
      end;
      if (Trim(EditSshPubKey.Text) = EmptyStr)  then
      begin
        StatusBarVmCreate.Font.Color:=clRed;
        StatusBarVmCreate.SimpleText:='SSH key can not be empty.';

        Result:=False;
        Exit;
      end;
      if not FileExists(DatadirPath+'templates/user-data') or not FileExists(DatadirPath+'templates/meta-data') then
      begin
        StatusBarVmCreate.Font.Color:=clRed;
        StatusBarVmCreate.SimpleText:='user-data or meta-data are not available at templates directory.';

        Result:=False;
        Exit;
      end;
    end;

    if CheckBoxImageFiles.Checked then
    begin
      if not FileExists(FileNameEditUserData.FileName)  then
      begin
        StatusBarVmCreate.Font.Color:=clRed;
        StatusBarVmCreate.SimpleText:='user-data file does not exist.';

        Result:=False;
        Exit;
      end;
      if not FileExists(FileNameEditMetaData.FileName)  then
      begin
        StatusBarVmCreate.Font.Color:=clRed;
        StatusBarVmCreate.SimpleText:='meta-data file does not exist.';

        Result:=False;
        Exit;
      end;
    end;
  end;

  if CheckBoxUseMedia.Checked then
  begin
    if not FileExists(FileNameEditBootMedia.FileName) then
    begin
      StatusBarVmCreate.Font.Color:=clRed;
      StatusBarVmCreate.SimpleText:='CD/DVD media does not exist.';

      Result:=False;
      Exit;
    end;
  end;
end;

procedure TFormVmCreate.ComboBoxSystemTypeChange(Sender: TObject);
begin
  ComboBoxSystemVersion.Clear;
  FillComboSystemVersion(ComboBoxSystemVersion, ComboBoxSystemType.Text);
end;

procedure TFormVmCreate.CheckBoxOnlyLocalhostChange(Sender: TObject);
begin
  if CheckBoxOnlyLocalhost.Checked then
  begin
    CheckBoxFramebuffer.Checked:=True;
  end;
end;

procedure TFormVmCreate.CheckBoxUseMediaChange(Sender: TObject);
begin
  if CheckBoxUseMedia.Checked then
    FileNameEditBootMedia.Enabled:=True
  else
    FileNameEditBootMedia.Enabled:=False;
end;

procedure TFormVmCreate.CheckBoxFramebufferChange(Sender: TObject);
begin
  if CheckBoxFramebuffer.Checked then
  begin
    CheckBoxOnlyLocalhost.Checked:=True;
  end
  else
  begin
    CheckBoxOnlyLocalhost.Checked:=False;
    CheckBoxWaitVNC.Checked:=False;
  end;
end;

procedure TFormVmCreate.BitBtnDownloadClick(Sender: TObject);
var
  RawFileName : String;
begin
  RemoteFile:=EmptyStr;
  OldRemoteFile:=EmptyStr;

  FileNameEditImageFile.Clear;
  StatusBarVmCreate.SimpleText:=EmptyStr;

  if not FileExists(CloudVmImagesPath) then
    CreateDirectory(CloudVmImagesPath, GetCurrentUserName());

  case CheckFileExtension(EditUrlImage.Text) of
    '.qcow2.xz',
    '.raw.xz',
    '.img.xz',
    '.qcow2',
    '.raw',
    '.img':
      begin
        ProgressBarImage.Position:=0;

        RemoteFile:=ExtractFileName(EditUrlImage.Text);

        RawFileName:=StringReplace(RemoteFile, CheckFileExtension(RemoteFile), EmptyStr, [rfIgnoreCase, rfReplaceAll])+'.raw';

        ChDir(CloudVmImagesPath);

        if FileExists(FetchCmd) and not FileExists(CloudVmImagesPath+'/'+RemoteFile) and not FileExists(CloudVmImagesPath+'/'+RawFileName) then
        begin
          ProgressBarImage.Max:=ConvertFileSize(GetRemoteSize(EditUrlImage.Text), 'M');

          if ProgressBarImage.Max > 0 then
          begin
            MyAppThread := AppProgressBarThread.Create(FetchCmd, ['-T', '3', EditUrlImage.Text]);
            MyAppThread.OnShowStatus := @ShowStatus;
            MyAppThread.OnEndStatus:= @EndStatus;
            MyAppThread.Start;

            LabelDownloadStatus.Visible:=True;
            LabelDownloadStatus.Caption:='Downloading...';
          end
          else
          begin
            StatusBarVmCreate.Font.Color:=clRed;
            StatusBarVmCreate.SimpleText:=ExtractFileName(FetchCmd) + ' can not retrieve '+ExtractFileName(EditUrlImage.Text)+' image file size.';
          end;
        end
        else
        begin
          if FileExists(CloudVmImagesPath+'/'+RawFileName) then
          begin
            RemoteFile:=RawFileName;

            LabelDownloadStatus.Visible:=True;
            LabelDownloadStatus.Caption:='Done';
          end;
          ConvertFileToRaw(RemoteFile);
        end;
      end;
  else
    LabelDownloadStatus.Visible:=True;
    LabelDownloadStatus.Caption:='Nothing to do';

    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:=ExtractFileName(EditUrlImage.Text) + ' file is not supported.';
  end;
end;

procedure TFormVmCreate.BitBtnDownloadPasteClick(Sender: TObject);
begin
  EditUrlImage.Text:=Clipboard.AsText;
end;

procedure TFormVmCreate.BitBtnSshPasteClick(Sender: TObject);
begin
  EditSshPubKey.Text:=Clipboard.AsText;
end;

procedure TFormVmCreate.CheckBoxImageFilesChange(Sender: TObject);
begin
  if CheckBoxImageFiles.Checked then
  begin
    CheckBoxImageMinimal.Checked:=False;
    GroupBoxMinimalConfiguration.Enabled:=False;
    GroupBoxFileConfig.Enabled:=True;
    CheckBoxUseMedia.Checked:=False;
    CheckBoxUseMedia.Enabled:=False;
  end
  else
  begin
    GroupBoxFileConfig.Enabled:=False;
    CheckBoxUseMedia.Enabled:=True;
  end;
end;

procedure TFormVmCreate.CheckBoxImageMinimalChange(Sender: TObject);
begin
  if CheckBoxImageMinimal.Checked then
  begin
    CheckBoxImageFiles.Checked:=False;
    GroupBoxMinimalConfiguration.Enabled:=True;
    GroupBoxFileConfig.Enabled:=False;
    CheckBoxUseMedia.Checked:=False;
    CheckBoxUseMedia.Enabled:=False;
  end
  else
  begin
    GroupBoxMinimalConfiguration.Enabled:=False;
    CheckBoxUseMedia.Enabled:=True;
  end;
end;

procedure TFormVmCreate.CheckBoxWaitVNCChange(Sender: TObject);
begin
  if CheckBoxWaitVNC.Checked then
  begin
    CheckBoxFramebuffer.Checked:=True;
  end;
end;

procedure TFormVmCreate.EditVmNameEditingDone(Sender: TObject);
begin
    EditVmFolderPath.Text:= VmPath + '/' + EditVmName.Text
end;

procedure TFormVmCreate.EditVmNameExit(Sender: TObject);
begin
  if not CheckVmName(EditVmName.Text) then
  begin
    PageControlVmCreate.ActivePageIndex:=0;
    EditVmName.SetFocus;
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:='Virtual machine name is not valid. Valid characters are [a-z][-_][0-9]';
  end;
  if not (GetNewVmName(EditVmName.Text)) then
  begin
    PageControlVmCreate.ActivePageIndex:=0;
    EditVmName.SetFocus;
    StatusBarVmCreate.Font.Color:=clRed;
    StatusBarVmCreate.SimpleText:=EditVmName.Text + ' virtual machine name is not available';
  end
end;

procedure TFormVmCreate.FileNameEditImageFileButtonClick(Sender: TObject);
begin
  FileNameEditImageFile.InitialDir:=CloudVmImagesPath;
end;

procedure TFormVmCreate.FileNameEditImageFileChange(Sender: TObject);
begin
  if FileExists(FileNameEditImageFile.FileName) then
  begin
    if not (CheckFileType(FileNameEditImageFile.FileName) = 'dos/mbr') then
    begin
      StatusBarVmCreate.Font.Color:=clTeal;
      StatusBarVmCreate.SimpleText:=ExtractFileName(FileNameEditImageFile.FileName) + ' image could not be compatible with bhyve.';
    end
    else
      SpinEditExDiskSize.Value:=GetFileSize(FileNameEditImageFile.FileName,'G');
  end;

end;

procedure TFormVmCreate.FileNameEditMetaDataButtonClick(Sender: TObject);
begin
  FileNameEditMetaData.InitialDir:=GetUserDir;
end;

procedure TFormVmCreate.FileNameEditNetworkConfigButtonClick(Sender: TObject);
begin
  FileNameEditNetworkConfig.InitialDir:=GetUserDir;
end;

procedure TFormVmCreate.FileNameEditUserDataButtonClick(Sender: TObject);
begin
  FileNameEditUserData.InitialDir:=GetUserDir;
end;

end.

