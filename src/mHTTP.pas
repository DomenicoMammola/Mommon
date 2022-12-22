// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mHTTP;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type
  THTTPHeader = record
    key : string;
    value : string;
  end;

  THTTPHeaders = array of THTTPHeader;

function PostJSONData (const aURI, aJSONMessage : String; out aResponseBody, aResponseText : String; out aErrorMessage : string) : boolean;
function PostJSONDataWithBasicAuthentication (const aURI, aJSONMessage : String; const aUsername, aPassword:  String; out aResponseBody, aResponseText : String; out aErrorMessage : string) : boolean;

function GetHTMLPage (const aURI : String; out aResponseBody: String; out aErrorMessage: String): boolean; overload;
function GetHTMLPage (const aURI : String; const aHeaders : THTTPHeaders; out aResponseBody: String; out aErrorMessage: String): boolean; overload;

implementation

uses
  Classes, sysutils, fphttpclient, {$IFDEF LINUX}FileUtil,{$ENDIF}
  IdHTTP, IdGlobal, IdSSLOpenSSLHeaders;

function PostJSONData(const aURI, aJSONMessage: String; out aResponseBody, aResponseText: String; out aErrorMessage: string): boolean;
begin
  Result := PostJSONDataWithBasicAuthentication(aURI, aJSONMessage, '', '', aResponseBody, aResponseText, aErrorMessage);
end;

function PostJSONDataWithBasicAuthentication(const aURI, aJSONMessage: String; const aUsername, aPassword: String; out aResponseBody, aResponseText: String; out aErrorMessage: string): boolean;
var
  HTTP: TIdHTTP;
  RequestBody: TStream;
begin
  Result := true;
  aErrorMessage := '';
  HTTP := TIdHTTP.Create;
  try
    try
      RequestBody := TStringStream.Create(UTF8Encode(aJSONMessage));
      try
        HTTP.Request.Accept := 'application/json';
        HTTP.Request.ContentType := 'application/json';

        if (aUsername <> '') or (aPassword <> '') then
        begin
          HTTP.Request.BasicAuthentication:=True;
          if aUsername <> '' then
            HTTP.Request.Username:= aUsername;
          if aPassword <> '' then
            HTTP.Request.Password:= aPassword;
        end;

        aResponseBody := HTTP.Post(aURI, RequestBody);
        aResponseText := HTTP.ResponseText;
      finally
        RequestBody.Free;
      end;
    except
      on E: EIdHTTPProtocolException do
      begin
        aErrorMessage:= E.Message + sLineBreak + E.ErrorMessage;
        Result := false;
      end;
      on E: Exception do
      begin
        aErrorMessage:= E.Message;
        Result := false;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

function GetHTMLPageIndy(const aURI: String; const aHeaders: THTTPHeaders; out aResponseBody: String; out aErrorMessage: String) : boolean;
var
  HTTP: TIdHTTP;
  i : integer;
begin
  Result := true;

  HTTP := TIdHTTP.Create;
  try
    try
      {$IFDEF LINUX}
      // https://synaptica.info/2021/01/12/delphi-10-4-1-indy-ssl-on-ubuntu-20-04/
      IdOpenSSLSetLibPath(ProgramDirectory);
      {$ENDIF}
      if  Length(aHeaders) > 0 then
      begin
        for i := Low(aHeaders) to High(aHeaders) do
          HTTP.Request.CustomHeaders.AddValue(aHeaders[i].key, aHeaders[i].value);
      end;

      aResponseBody := HTTP.Get(aURI, IndyTextEncoding(encUTF8));
    except
      on E: EIdHTTPProtocolException do
      begin
        aErrorMessage:= aURI + sLineBreak + E.Message + sLineBreak + E.ErrorMessage;
        Result := false;
      end;
      on E: Exception do
      begin
        aErrorMessage:= aURI + sLineBreak + E.Message + sLineBreak + 'OpenSSL error:' + WhichFailedToLoad;
        Result := false;
      end;
    end;
  finally
    HTTP.Free;
  end;
end;

function GetHTMLPageFPC(const aURI: String; const aHeaders: THTTPHeaders; out aResponseBody: String; out aErrorMessage: String) : boolean;
var
  i : integer;
begin
  Result := false;
  with TFPHTTPClient.Create(nil) do
  try
    try
      if  Length(aHeaders) > 0 then
      begin
        for i := Low(aHeaders) to High(aHeaders) do
          AddHeader(aHeaders[i].key, aHeaders[i].value);
      end;
      aResponseBody:= Get(aURI);
      Result := true;
    except
      on E: Exception do
      begin
        aErrorMessage:= aURI + sLineBreak + E.Message + sLineBreak + e.Message;
        Result := false;
      end;
    end;

  finally
    Free;
  end;
end;

function GetHTMLPage(const aURI: String; out aResponseBody: String; out aErrorMessage: String) : boolean;
begin
  Result := GetHTMLPage(aURI, nil, aResponseBody, aErrorMessage);
end;

function GetHTMLPage(const aURI: String; const aHeaders: THTTPHeaders; out aResponseBody: String; out aErrorMessage: String): boolean;
begin
  Result := GetHTMLPageFPC(aURI, aHeaders, aResponseBody, aErrorMessage);
  if not Result then
    Result := GetHTMLPageIndy(aURI, aHeaders, aResponseBody, aErrorMessage);
end;

end.
