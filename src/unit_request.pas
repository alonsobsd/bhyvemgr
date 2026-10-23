{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit unit_request;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fgl, jsonparser, fpjson;

type
  { TPendingRequest }
  TPendingRequest = class
  private
    FID: String;
    FResponse: String;
    FEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override;

    property ID: String read FID write FID;
    property Response: String read FResponse write FResponse;
    property Event: TEvent read FEvent;
  end;

  TRequestMap = specialize TFPGMap<String, TPendingRequest>;

  { TRequestManager }
  TRequestManager = class
  private
    FRequests: TRequestMap;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function RegisterRequest: TPendingRequest;
    procedure Complete(const Response: String);
    procedure Remove(const RequestID: String);
  end;


implementation

uses
  LazLogger;

{ TPendingRequest }
constructor TPendingRequest.Create;
begin
  inherited Create;
  FResponse := EmptyStr;

  FEvent := TEvent.Create(nil, False, False, EmptyStr);
end;

destructor TPendingRequest.Destroy;
begin
  FEvent.Free;
  inherited Destroy;
end;

{ TRequestManager }
constructor TRequestManager.Create;
begin
  inherited Create;

  FRequests := TRequestMap.Create;

  FRequests.Sorted := True;

  InitCriticalSection(FLock);
end;

destructor TRequestManager.Destroy;
var
  I: Integer;
begin
  EnterCriticalSection(FLock);

  try
    for I := 0 to FRequests.Count - 1 do
      FRequests.Data[I].Free;

    FRequests.Free;
  finally
    LeaveCriticalSection(FLock);
  end;

  DoneCriticalSection(FLock);
  inherited Destroy;
end;

function TRequestManager.RegisterRequest: TPendingRequest;
var
  GUID: TGUID;
begin
  Result := TPendingRequest.Create;

  CreateGUID(GUID);

  Result.ID := GUIDToString(GUID);

  EnterCriticalSection(FLock);
  try
    FRequests.Add(Result.ID, Result);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TRequestManager.Complete(const Response: String);
var
  Index: Integer;
  Request: TPendingRequest;
  JSON: TJSONObject;
  RequestID: String;
begin
  JSON := GetJSON(Response) as TJSONObject;

  try
    RequestID := JSON.Get('id', EmptyStr);

    if RequestID = EmptyStr then
      Exit;

    EnterCriticalSection(FLock);

    try
      Index := FRequests.IndexOf(RequestID);

      if Index >= 0 then
      begin
        Request := FRequests.Data[Index];
        Request.Response := Response;
        Request.Event.SetEvent;
      end
    finally
      LeaveCriticalSection(FLock);
    end;
  finally
    JSON.Free;
  end;
end;

procedure TRequestManager.Remove(const RequestID: String);
var
  Index: Integer;
  Request: TPendingRequest;
begin
  EnterCriticalSection(FLock);
  try
    Index := FRequests.IndexOf(RequestID);

    if Index >= 0 then
    begin
      Request := FRequests.Data[Index];
      FRequests.Delete(Index);
      Request.Free;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

end.
