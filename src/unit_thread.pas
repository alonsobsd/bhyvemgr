{ BSD 3-Clause License

Copyright (c) 2024, Alonso Cárdenas <acardenas@bsd-peru.org>

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

unit unit_thread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Dialogs, SysUtils;

type
  TExitStatusEvent = procedure(Status: Integer; Message : String; VmName : String) of Object;

  { VmThread }

  VmThread = class(TThread)
  private
    AppName : string;
    AppVmName : string;
    AppVmPath : string;
    AppResult : String;
    AppParams: TStringArray;
    ExitMessage : String;
    ExitStatus : Integer;
    FOnExitStatus: TExitStatusEvent;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(VmName : String);
    property OnExitStatus: TExitStatusEvent read FOnExitStatus write FOnExitStatus;
  end;

  { AppThread }

  AppThread = class(TThread)
  private
    AppName : string;
    AppResult : Boolean;
    AppParams: TStringArray;
    ExitMessage : String;
    ExitStatus : Integer;
    procedure ShowAppStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(AppPath: String; Params: TStringArray);
  end;

implementation

uses
  unit_global, process;

{ VmThread }

procedure VmThread.ShowStatus;
begin
  if Assigned(FOnExitStatus) then
  begin
    FOnExitStatus(ExitStatus, ExitMessage, AppVmName);
  end;
end;

procedure VmThread.Execute;
var
  AppProcess: TProcess;
  I: Integer;
begin
  AppProcess := TProcess.Create(nil);

  try
    AppProcess.InheritHandles := False;
    AppProcess.Options := [poWaitOnExit];
    AppProcess.ShowWindow := swoShow;
    for I := 1 to GetEnvironmentVariableCount do
      AppProcess.Environment.Add(GetEnvironmentString(I));
    AppProcess.Executable:= AppName;

    for I:=0 to Length(AppParams)-1 do
    begin
      AppProcess.Parameters.Add(AppParams[I]);
    end;

    try
      AppProcess.Execute;

      ExitStatus:=AppProcess.ExitStatus;

      if (ExitStatus = -1) then
      begin
        AppResult:='True';
      end
      else
      begin
        if ExitStatus = 0 then
        begin
          ExitMessage:=AppVmName+' VM is rebooting';
          Synchronize(@Showstatus);
        end
        else if ExitStatus = 1 then
        begin
          ExitMessage:=AppVmName+' VM has been powered off';
          Synchronize(@Showstatus);
        end
        else if ExitStatus = 2 then
        begin
          ExitMessage:=AppVmName+' VM is halted';
          Synchronize(@Showstatus);
        end
        else if ExitStatus = 3 then
        begin
          ExitMessage:=AppVmName+' VM is triple fault';
          Synchronize(@Showstatus);
        end
        else if ExitStatus = 4 then
        begin
          ExitMessage:=AppVmName+' VM exited due to an error';
          Synchronize(@Showstatus);
        end
        else
        begin
          ExitStatus:=6;
          ExitMessage:=AppVmName+' VM exited';
          Synchronize(@Showstatus);
        end;

        AppProcess.Terminate(1);
      end
    except
      on E: Exception do
      begin
        ExitStatus:=5;
        ExitMessage:='An exception was raised: ' + E.Message;
        Synchronize(@Showstatus);
      end;
    end;
  finally
    AppProcess.Free;
  end;
end;

constructor VmThread.Create(VmName : String);
begin
  if UseSudo = 'no' then
    AppName:=DoasCmd
  else
    AppName:=SudoCmd;

  AppVmName:=VmName;
  AppVmPath:=VmPath;
  AppParams:=[BhyveCmd, '-k', VmPath+'/'+VmName+'/bhyve_config.conf'];

  if FileExists(AppName) and FileExists(BhyveCmd) then
  begin
    inherited Create(True);
    FreeOnTerminate := true;
  end;
end;

{ AppThread }

procedure AppThread.ShowAppStatus;
begin
  MessageDlg('Error message', ExitMessage, mtError, [mbOK], 0);
end;

procedure AppThread.Execute;
var
  AppProcess: TProcess;
  I: Integer;
begin
  AppProcess := TProcess.Create(nil);

  try
    AppProcess.InheritHandles := False;
    AppProcess.Options := [poWaitOnExit];
    AppProcess.ShowWindow := swoShow;
    for I := 1 to GetEnvironmentVariableCount do
      AppProcess.Environment.Add(GetEnvironmentString(I));
    AppProcess.Executable:= AppName;

    for I:=0 to Length(AppParams)-1 do
    begin
      AppProcess.Parameters.Add(AppParams[I]);
    end;

    try
      AppProcess.Execute;
      ExitStatus:=AppProcess.ExitStatus;

      if (ExitStatus = -1) then
      begin
        AppResult:=True;
      end
      else
      begin
        AppProcess.Terminate(1);
      end;

    except
      on E: Exception do
      begin
        ExitStatus:=5;
        ExitMessage:='An exception was raised: ' + E.Message;
        Synchronize(@ShowAppStatus);
      end;
    end;
  finally
    AppProcess.Free;
  end;
end;

constructor AppThread.Create(AppPath: String; Params: TStringArray);
begin
  AppName:=AppPath;
  AppParams:=Params;

  if FileExists(AppName) then
  begin
    inherited Create(True);
    FreeOnTerminate := true;
  end;
end;

end.

