// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mSendMail;

interface

uses
  Classes;

type
  { TSendMail }

  TSendMail = class
  strict private
    FHost: String;
    FPort: Integer;
    FUserName: String;
    FPassword: String;
    FSenderName: String;
    FSenderMailAddress: String;
    FRecipients: String;
    FSubject: String;
    FImageFiles : TStringList;
    FImageReferences : TStringList;
    FHTML: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function SetHost(const aHostName: String): TSendMail;
    function SetPort(const aPortNumber: integer): TSendMail;
    function SetUserName(const aUserName: String): TSendMail;
    function SetPassword(const aPassword: String): TSendMail;
    function SetSenderName(const aSenderName: String): TSendMail;
    function SetSenderMailAddress(const aMailAddress: String): TSendMail;
    function SetRecipients(const aRecipients: String): TSendMail;
    function SetSubject(const aSubject: String): TSendMail;
    function AddImageFile(const aFileName, aReferenceName: String): TSendMail;
    function AddHTML(const aStrings: TStrings): TSendMail;
    function ClearHTML: TSendMail;
    function Send(out aErrorMessage: String): boolean;
  end;


implementation

uses
  sysutils,
  IdSMTP, IdMessage, IdMessageBuilder;

{ TSendMail }

constructor TSendMail.Create;
begin
  FHost:= '127.0.0.1';
  FPort:= 25;
  FSenderName:= '';
  FSenderMailAddress:= '@noreply';
  FRecipients:= '';
  FSubject:= '';
  FUserName:= '';
  FPassword:= '';
  FImageFiles:= TStringList.Create;
  FImageReferences:= TStringList.Create;
  FHTML:= TStringList.Create;
end;

destructor TSendMail.Destroy;
begin
  FImageFiles.Free;
  FImageReferences.Free;
  FHTML.Free;
  inherited Destroy;
end;

function TSendMail.SetHost(const aHostName: String): TSendMail;
begin
  FHost:= aHostName;
  Result:= Self;
end;

function TSendMail.SetPort(const aPortNumber: integer): TSendMail;
begin
  FPort:= aPortNumber;
  Result:= Self;
end;

function TSendMail.SetUserName(const aUserName: String): TSendMail;
begin
  FUserName:= aUserName;
  Result:= Self;
end;

function TSendMail.SetPassword(const aPassword: String): TSendMail;
begin
  FPassword:= aPassword;
  Result:= Self;
end;

function TSendMail.SetSenderName(const aSenderName: String): TSendMail;
begin
  FSenderName:= aSenderName;
  Result:= Self;
end;

function TSendMail.SetSenderMailAddress(const aMailAddress: String): TSendMail;
begin
  FSenderMailAddress:= aMailAddress;
  Result:= Self;
end;

function TSendMail.SetRecipients(const aRecipients: String): TSendMail;
begin
  FRecipients:= aRecipients;
  Result:= Self;
end;

function TSendMail.SetSubject(const aSubject: String): TSendMail;
begin
  FSubject:= aSubject;
  Result:= Self;
end;

function TSendMail.AddImageFile(const aFileName, aReferenceName: String): TSendMail;
begin
  if not FileExists(aFileName) then
    raise Exception.Create('File ' + aFileName + ' not found');
  FImageFiles.Add(aFileName);
  FImageReferences.Add(aReferenceName);
  Result:= Self;
end;

function TSendMail.AddHTML(const aStrings: TStrings): TSendMail;
begin
  FHTML.AddStrings(aStrings);
  Result:= Self;
end;

function TSendMail.ClearHTML: TSendMail;
begin
  FHTML.Clear;
  Result:= Self;
end;

function TSendMail.Send(out aErrorMessage: String): boolean;
var
  tmpMessage: TIdMessage;
  tmpSMTP: TIdSMTP;
  error: Boolean;
  htmlMessageBuilder : TIdMessageBuilderHtml;
  i : integer;
begin
  // https://www.indyproject.org/2008/01/16/new-html-message-builder-class/
  // https://www.indyproject.org/2005/08/17/html-messages/

  htmlMessageBuilder := TIdMessageBuilderHtml.Create;
  tmpMessage := TIdMessage.Create(nil);
  try
    tmpMessage.From.Name:= FSenderName;
    tmpMessage.From.Address:= FSenderMailAddress;
    tmpMessage.Recipients.EMailAddresses:= FRecipients;
    tmpMessage.Subject:= FSubject;

    htmlMessageBuilder.Html.AddStrings(FHTML);
    for i := 0 to FImageFiles.Count - 1 do
      htmlMessageBuilder.HtmlFiles.Add(FImageFiles.Strings[i], FImageReferences.Strings[i]);
    htmlMessageBuilder.FillMessage(tmpMessage);

    tmpSMTP := TIdSMTP.Create(nil);
    try
      tmpSMTP.Host:= FHost;
      tmpSMTP.Port:= FPort;
      tmpSMTP.ConnectTimeout:= 5000;
      if FUserName <> '' then
        tmpSMTP.Username:= FUserName;
      if FPassword <> '' then
        tmpSMTP.Password:= FPassword;
      error:= false;
      try
        tmpSMTP.Connect;
        tmpSMTP.Send(tmpMessage);
      except
        on e:Exception do
        begin
          aErrorMessage := e.Message;
          error := true;
        end;
      end;

      try
        if tmpSMTP.Connected then
          tmpSMTP.Disconnect;
      except
        on e:Exception do
        begin
          if not error then
          begin
            aErrorMessage:= e.Message;
            error := true;
          end;
        end;
      end;

    finally
      tmpSMTP.Free;
    end;

  finally
    tmpMessage.Free;
    htmlMessageBuilder.Free;
  end;

  Result := not error;
end;

end.
