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
  Classes, contnrs;

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
    FHTMLImages : TObjectList;
    FAttachments : TObjectList;
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

    function AddHTMLImageFile(const aFileName, aReferenceName: String): TSendMail;
    function AddHTMLImage(const aData : TStream; const aMIMEType: String; const aReferenceName : String): TSendMail;
    function AddHTMLJPEGImage(const aData : TStream; const aReferenceName : String): TSendMail;
    function AddHTMLPNGImage(const aData : TStream; const aReferenceName: String): TSendMail;

    function AttachFile(const aFileName : String; const aReferenceName : String = ''): TSendMail;
    function AttachData(const aData : TStream; const aMIMEType: String; const aReferenceName: String = ''): TSendMail;
    function AttachExcelData(const aData : TStream; const aReferenceName: String = ''): TSendMail;
    function AttachPDFData(const aData : TStream; const aReferenceName: String = ''): TSendMail;
    function AttachTXTData(const aData : TStream; const aReferenceName: String = ''): TSendMail;
    function AttachXMLData(const aData : TStream; const aReferenceName: String = ''): TSendMail;
    function AttachZIPData(const aData : TStream; const aReferenceName: String = ''): TSendMail;
    function AttachWordData(const aData : TStream; const aReferenceName: String = ''): TSendMail;

    function AddHTML(const aStrings: TStrings): TSendMail;
    function ClearHTML: TSendMail;
    function Send(out aErrorMessage: String): boolean;
  end;


implementation

uses
  sysutils,
  IdSMTP, IdMessage, IdMessageBuilder;

type
  TAttachedFileType = (ftFile, ftStream);

  TAttachedFile = class
  private
    FileType : TAttachedFileType;
    MIMEType : String;
    FileName : String;
    Reference : String;
    Data : TStream;
  end;



{ TSendMail }

function TSendMail.AddHTMLImage(const aData: TStream; const aMIMEType: String; const aReferenceName: String): TSendMail;
var
  tmp : TAttachedFile;
begin
  tmp := TAttachedFile.Create;
  FHTMLImages.Add(tmp);
  tmp.FileType:= ftStream;
  tmp.Data:= aData;
  tmp.Reference:= aReferenceName;
  tmp.MIMEType:= aMIMEType;
  Result:= Self;
end;

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
  FHTMLImages:= TObjectList.Create(true);
  FAttachments:= TObjectList.Create(true);
  FHTML:= TStringList.Create;
end;

destructor TSendMail.Destroy;
begin
  FHTMLImages.Free;
  FAttachments.Free;
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


function TSendMail.AddHTMLImageFile(const aFileName, aReferenceName: String): TSendMail;
var
  tmp : TAttachedFile;
begin
  if not FileExists(aFileName) then
    raise Exception.Create('File ' + aFileName + ' not found');
  tmp := TAttachedFile.Create;
  FHTMLImages.Add(tmp);
  tmp.FileType:= ftFile;
  tmp.FileName:= aFileName;
  tmp.Reference:= aReferenceName;
  Result:= Self;
end;

function TSendMail.AddHTMLJPEGImage(const aData: TStream;
  const aReferenceName: String): TSendMail;
begin
  Result := Self.AddHTMLImage(aData, 'image/jpeg', aReferenceName);
end;

function TSendMail.AddHTMLPNGImage(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result := Self.AddHTMLImage(aData, 'image/x-png', aReferenceName);
end;

function TSendMail.AttachFile(const aFileName: String; const aReferenceName: String): TSendMail;
var
  tmp : TAttachedFile;
begin
  if not FileExists(aFileName) then
    raise Exception.Create('File ' + aFileName + ' not found');
  tmp := TAttachedFile.Create;
  FAttachments.Add(tmp);
  tmp.FileType:= ftFile;
  tmp.FileName:= aFileName;
  tmp.Reference:= aReferenceName;
  Result:= Self;
end;

function TSendMail.AttachExcelData(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/x-msexcel', aReferenceName);
end;

function TSendMail.AttachPDFData(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/pdf', aReferenceName);
end;

function TSendMail.AttachTXTData(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'text/plain', aReferenceName);
end;

function TSendMail.AttachXMLData(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'text/xml', aReferenceName);
end;

function TSendMail.AttachZIPData(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/x-zip-compressed', aReferenceName);
end;

function TSendMail.AttachWordData(const aData: TStream; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/msword', aReferenceName);
end;

function TSendMail.AttachData(const aData: TStream; const aMIMEType: String; const aReferenceName: String = ''): TSendMail;
var
  tmp : TAttachedFile;
begin
  tmp := TAttachedFile.Create;
  FAttachments.Add(tmp);
  tmp.FileType:= ftStream;
  tmp.Data:= aData;
  tmp.Reference:= aReferenceName;
  tmp.MIMEType:= aMIMEType;
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
  f : TAttachedFile;
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
    for i := 0 to FHTMLImages.Count - 1 do
    begin
      f := FHTMLImages.Items[i] as TAttachedFile;
      if f.FileType = ftFile then
        htmlMessageBuilder.HtmlFiles.Add(f.FileName, f.Reference)
      else if f.FileType = ftStream then
        htmlMessageBuilder.HtmlFiles.Add(f.Data, f.MIMEType, f.Reference);
    end;
    for i := 0 to FAttachments.Count - 1 do
    begin
      f := FAttachments.Items[i] as TAttachedFile;
      if f.FileType = ftFile then
        htmlMessageBuilder.Attachments.Add(f.FileName, f.Reference)
      else if f.FileType = ftStream then
        htmlMessageBuilder.Attachments.Add(f.Data, f.MIMEType, f.Reference);
    end;
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
