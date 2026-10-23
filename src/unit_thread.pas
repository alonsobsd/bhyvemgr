{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2024-2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit unit_thread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Dialogs, SysUtils;

type
  TShowStatusEvent = procedure(Status: Integer; AppPid: Integer) of Object;
  TEndStatusEvent = procedure(Status: Integer; AppName : String; AppPid: Integer) of Object;

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

  { AppProgressBarThread }

  AppProgressBarThread = class(TThread)
  private
    AppName : string;
    AppResult : Boolean;
    AppPid : Integer;
    AppParams: TStringArray;
    ExitMessage : String;
    ExitStatus : Integer;
    FOnShowStatus: TShowStatusEvent;
    FOnEndStatus: TEndStatusEvent;
    procedure ShowAppStatus;
    procedure EndAppStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(AppPath: String; Params: TStringArray);
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
    property OnEndStatus: TEndStatusEvent read FOnEndStatus write FOnEndStatus;
  end;

implementation

uses
  unit_global, unit_language, unit_component, process, LazLogger;

{ AppThread }

procedure AppThread.ShowAppStatus;
begin
  MessageDialog(mtError, ExitMessage);
end;

procedure AppThread.Execute;
var
  AppProcess: TProcess;
  AppProcessOutput: TStringList;
  I: Integer;
begin
  AppProcess := TProcess.Create(nil);
  AppProcessOutput:= TStringList.Create;

  try
    AppProcess.InheritHandles := False;
    AppProcess.Options := [poWaitOnExit, poUsePipes];
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
      AppProcessOutput.LoadFromStream(AppProcess.Stderr);

      if (ExitStatus = 0) then
      begin
        AppResult:=True;
        AppProcess.Terminate(1);
      end
      else
      begin
        AppResult:=False;
        DebugLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : AppThread : '+AppName+' : '+sLineBreak+AppProcessOutput.Text);
        AppProcess.Terminate(1);
      end;

    except
      on E: Exception do
      begin
        ExitStatus:=5;
        ExitMessage:=Format(exception_status, [E.Message]);
        Synchronize(@ShowAppStatus);
      end;
    end;
  finally
    AppProcessOutput.Free;
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

{ AppProgressBarThread }

procedure AppProgressBarThread.ShowAppStatus;
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(ExitStatus, AppPid);
  end;
end;

procedure AppProgressBarThread.EndAppStatus;
begin
  if Assigned(FOnEndStatus) then
  begin
    FOnEndStatus(ExitStatus, AppName, AppPid);
  end;
end;

procedure AppProgressBarThread.Execute;
var
  AppProcess: TProcess;
  I: Integer;
begin
  AppProcess := TProcess.Create(nil);

  try
    AppProcess.InheritHandles := False;
    AppProcess.Options := [poUsePipes, poStderrToOutPut];
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

      AppPid:=AppProcess.ProcessID;
      Synchronize(@ShowAppStatus);

      I:=0;

      while (AppProcess.Running) do
      begin
        Inc(I);
        if I = 30 then
        begin
          Synchronize(@ShowAppStatus);
          I:=0;
        end;

        Sleep(100);
      end;

      Synchronize(@ShowAppStatus);

      ExitStatus:=AppProcess.ExitStatus;

      if (ExitStatus = 0) then
      begin
        AppResult:=True;
        Synchronize(@EndAppStatus);

        AppProcess.Terminate(1);
      end
      else
      begin
        AppResult:=False;
        Synchronize(@EndAppStatus);

        AppProcess.Terminate(1);
      end;

    except
      on E: Exception do
      begin
        ExitStatus:=5;
        ExitMessage:=Format(exception_status, [E.Message]);
        Synchronize(@EndAppStatus);
      end;
    end;
  finally
    AppProcess.Free;
  end;
end;

constructor AppProgressBarThread.Create(AppPath: String; Params: TStringArray);
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

