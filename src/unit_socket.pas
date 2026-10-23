{ SPDX-License-Identifier: BSD-3-Clause

Copyright (c) 2026, J. Alonso Cárdenas <acardenas@bsd-peru.org>

}

unit unit_socket;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, Sockets;

type
  TProcessMessage = procedure(Message : String) of Object;
  TProcessResponse = procedure(const Message : String) of Object;
  TOnDisconnected = procedure of object;

  { TSocketThread }
  TSocketThread = class(TThread)
  private
    FSocket : LongInt;
    FOutputBuffer : String;
    FInputBuffer  : String;
    { Shared with GUI and socket thread }
    FPendingBuffer : String;
    FLock : TRTLCriticalSection;
    { Used by main thread callbacks }
    FOnProcessMessage: TProcessMessage;
    FOnProcessResponse: TProcessResponse;
    FOnDisconnected: TOnDisconnected;
    procedure MovePendingToOutput;
    procedure FlushOutput;
    procedure ReceiveData;
    procedure QueueProcessMessage(const Msg: String);
    procedure HandleDisconnect;
    procedure DoDisconnect;
    function GetNextMessage(out Msg:String):Boolean;
    function CreateRequestID:String;
  protected
    procedure Execute; override;
  public
    constructor Create(ASocket:LongInt);
    destructor Destroy; override;
    procedure SendJSON(const JSON:String);
    property OnProcessMessage: TProcessMessage read FOnProcessMessage write FOnProcessMessage;
    property OnProcessResponse: TProcessResponse read FOnProcessResponse write FOnProcessResponse;
    property OnDisconnected: TOnDisconnected read FOnDisconnected write FOnDisconnected;
  end;

  { TQueuedSocketMessage }
  TQueuedSocketMessage = class
  private
    FSocketThread: TSocketThread;
    FMessage: String;
  public
    procedure Execute;
  end;


implementation

{ TPendingRequest }

constructor TSocketThread.Create(ASocket:LongInt);
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FSocket := ASocket;
  FOutputBuffer := EmptyStr;
  FInputBuffer := EmptyStr;
  FPendingBuffer := EmptyStr;

  InitCriticalSection(FLock);
end;

destructor TSocketThread.Destroy;
begin
  if FSocket >= 0 then
    fpClose(FSocket);

  DoneCriticalSection(FLock);
  inherited Destroy;
end;

procedure TSocketThread.QueueProcessMessage(const Msg: String);
var
  Item: TQueuedSocketMessage;
begin
  Item := TQueuedSocketMessage.Create;

  Item.FSocketThread := Self;
  Item.FMessage := Msg;

  TThread.Queue(Self, @Item.Execute);
end;

procedure TSocketThread.HandleDisconnect;
begin
  TThread.Queue(Self, @DoDisconnect);
end;

procedure TSocketThread.DoDisconnect;
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected();
end;

procedure TSocketThread.SendJSON(const JSON:String);
begin
  EnterCriticalSection(FLock);
  try
    FPendingBuffer := FPendingBuffer + JSON + LineEnding;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TSocketThread.MovePendingToOutput;
var
  Data:String;
begin
  EnterCriticalSection(FLock);

  try
    Data := FPendingBuffer;
    FPendingBuffer := EmptyStr;
  finally
    LeaveCriticalSection(FLock);
  end;

  if Data <> EmptyStr then
    FOutputBuffer := FOutputBuffer + Data;
end;

procedure TSocketThread.FlushOutput;
var
  Sent : LongInt;
begin
  if FOutputBuffer = EmptyStr then
    Exit;

  Sent := fpSend(FSocket, @FOutputBuffer[1], Length(FOutputBuffer), MSG_DONTWAIT or MSG_NOSIGNAL);

  if Sent > 0 then
  begin
    Delete(FOutputBuffer, 1, Sent);
  end;
end;

procedure TSocketThread.ReceiveData;
var
  Buffer : array[0..1023] of Char;
  Len : LongInt;
  Data : String;
  Msg : String;
begin
  Len := fpRecv(FSocket, @Buffer, SizeOf(Buffer), 0);

  if Len = 0 then
  begin
    HandleDisconnect;
    Terminate;
    Exit;
  end;

  if Len < 0 then
  begin
    if fpgeterrno = ESysEINTR then
      Exit;

    HandleDisconnect;
    Terminate;
    Exit;
  end;

  SetString(Data, Buffer, Len);

  FInputBuffer := FInputBuffer + Data;

  while GetNextMessage(Msg) do
  begin
    {$IFDEF DEBUG}
    WriteLn('JSON received:');
    WriteLn('['+FormatDateTime('DD-MM-YYYY HH:NN:SS', Now)+'] : '+Msg);
    {$ENDIF DEBUG}

    if Msg.Contains('"type" : "task"') then
    begin
      if Assigned(FOnProcessResponse) then
          FOnProcessResponse(Msg);
    end
    else
      if Msg.Contains('"type" : "event"') or Msg.Contains('"type" : "snapshot"')  then
        QueueProcessMessage(Msg);
  end;
end;

function TSocketThread.GetNextMessage(out Msg:String):Boolean;
var
  P : Integer;
begin
  Result := False;

  Msg := EmptyStr;

  P := Pos(LineEnding, FInputBuffer);

  if P = 0 then
    Exit;

  Msg := Copy(FInputBuffer, 1, P-1);

  Delete(FInputBuffer, 1, P + Length(LineEnding)-1);

  Result := True;
end;

function TSocketThread.CreateRequestID: String;
var
  G:TGUID;
begin
  CreateGUID(G);
  Result := GUIDToString(G);
end;

procedure TSocketThread.Execute;
var
  ReadSet  : TFDSet;
  WriteSet : TFDSet;
  Timeout : TTimeVal;
  Ret : Integer;
begin
  while not Terminated do
  begin

    { Pending messages from GUI to socket thread }
    MovePendingToOutput;

    fpFD_ZERO(ReadSet);
    fpFD_ZERO(WriteSet);

    fpFD_SET(FSocket, ReadSet);

    if FOutputBuffer <> EmptyStr then
      fpFD_SET(FSocket, WriteSet);

    Timeout.tv_sec := 0;
    Timeout.tv_usec := 5000;

    Ret := fpSelect(FSocket + 1, @ReadSet, @WriteSet, nil, @Timeout);

    if Ret <= 0 then
      Continue;

    if fpFD_ISSET(FSocket,WriteSet) <> 0 then
      FlushOutput;

    if fpFD_ISSET(FSocket,ReadSet) <> 0 then
      ReceiveData;
  end;
end;

{ TQueuedSocketMessage }

procedure TQueuedSocketMessage.Execute;
begin
  if Assigned(FSocketThread.FOnProcessMessage) then
    FSocketThread.FOnProcessMessage(FMessage);

  Free;
end;

end.
