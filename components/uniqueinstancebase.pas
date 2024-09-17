unit UniqueInstanceBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc;

const
  ParamsSeparator = #13;

var
  FIPCServer: TSimpleIPCServer;

procedure InitializeUniqueServer(const ServerId: String);

function GetFormattedParams: String;

function GetServerId(const Identifier: String): String;

implementation

uses
  LazUTF8;

const
  BaseServerId = 'tuniqueinstance_';

procedure InitializeUniqueServer(const ServerId: String);
begin
  //It's the first instance. Init the server
  if FIPCServer = nil then
  begin
    FIPCServer := TSimpleIPCServer.Create(nil);
    FIPCServer.ServerID := ServerId;
    FIPCServer.Global := True;
    FIPCServer.StartServer;
  end;
end;

function GetFormattedParams: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    Result := Result + ParamStrUTF8(i) + ParamsSeparator;
end;

function GetServerId(const Identifier: String): String;
begin
  if Identifier <> '' then
    Result := BaseServerId + Identifier
  else
    Result := BaseServerId + ExtractFileName(ParamStrUTF8(0));
end;

finalization
  FIPCServer.Free;

end.

