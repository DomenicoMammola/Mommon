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
  Classes, contnrs,
  IdSMTP;

//
// helpful info taken from:
//
// https://mikejustin.wordpress.com/2014/07/27/send-secured-smtp-email-from-delphi-applications/
//

const
  SMTP_PORT_EXPLICIT_TLS = 587;

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
    FCCRecipients : String;
    FBCCRecipients : String;
    FSubject: String;
    FHTMLImages : TObjectList;
    FAttachments : TObjectList;
    FHTML: TStringList;
    FPlainText : TStringList;
    FSSLConnection : boolean;
    FTLSConnection : boolean;
  strict private
    procedure AddSSLHandler(aSMTP : TIdSMTP);
    procedure InitSASL(aSMTP : TIdSMTP; const aUserName, aPassword : String);
    function ExtractDomain(const aMailAddress: String): String;
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
    function SetCCRecipients(const aRecipients: String): TSendMail;
    function SetBCCRecipients(const aRecipients: String): TSendMail;
    function SetSubject(const aSubject: String): TSendMail;
    function SetSSLConnection : TSendMail;
    function SetTLSConnection : TSendMail;

    function AddHTMLImageFile(const aFileName, aReferenceName: String): TSendMail;
    function AddHTMLImage(const aData : TStream; const aMIMEType: String; const aReferenceName : String): TSendMail;
    function AddHTMLJPEGImage(const aData : TStream; const aReferenceName : String): TSendMail;
    function AddHTMLPNGImage(const aData : TStream; const aReferenceName: String): TSendMail;

    function AttachFile(const aFileName : String; const aReferenceName : String = ''): TSendMail;
    function AttachData(const aData : TStream; const aMIMEType: String; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachExcelData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachExcelOOXML(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachExcelXLSX(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachPDFData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachTXTData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachXMLData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachZIPData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;
    function AttachWordData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TSendMail;

    function AddHTML(const aStrings: TStrings): TSendMail;
    function AddPlainText (const aStrings : TStrings) : TSendMail;
    function ClearHTML: TSendMail;
    function ClearPlainText : TSendMail;
    function Send(out aErrorMessage: String): boolean;
  end;


implementation

uses
  sysutils,
  IdMessage, IdMessageBuilder,
  IdComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,
  IdMessageClient, IdSMTPBase, IdBaseComponent, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdSASLLogin,
  IdSASL_CRAM_SHA1, IdSASL, IdSASLUserPass, IdSASL_CRAMBase, IdSASL_CRAM_MD5,
  IdSASLSKey, IdSASLPlain, IdSASLOTP, IdSASLExternal, IdSASLDigest,
  IdSASLAnonymous, IdUserPassProvider,
  mLog, mUtility;


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

var
  logger : TmLog;

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

procedure TSendMail.AddSSLHandler(aSMTP : TIdSMTP);
var
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(aSMTP);
  // SSL/TLS handshake determines the highest available SSL/TLS version dynamically
  SSLHandler.SSLOptions.Method := sslvSSLv23;
  SSLHandler.SSLOptions.Mode := sslmClient;
  SSLHandler.SSLOptions.VerifyMode := [];
  SSLHandler.SSLOptions.VerifyDepth := 0;
  aSMTP.IOHandler := SSLHandler;
end;

procedure TSendMail.InitSASL(aSMTP : TIdSMTP; const aUserName, aPassword : String);
var
  IdUserPassProvider: TIdUserPassProvider;
  IdSASLCRAMMD5: TIdSASLCRAMMD5;
  IdSASLCRAMSHA1: TIdSASLCRAMSHA1;
  IdSASLPlain: TIdSASLPlain;
  IdSASLLogin: TIdSASLLogin;
  IdSASLSKey: TIdSASLSKey;
  IdSASLOTP: TIdSASLOTP;
  IdSASLAnonymous: TIdSASLAnonymous;
  IdSASLExternal: TIdSASLExternal;
begin
  IdUserPassProvider := TIdUserPassProvider.Create(aSMTP);
  IdUserPassProvider.Username := aUserName;
  IdUserPassProvider.Password:= aPassword;

  IdSASLCRAMSHA1 := TIdSASLCRAMSHA1.Create(aSMTP);
  IdSASLCRAMSHA1.UserPassProvider := IdUserPassProvider;
  IdSASLCRAMMD5 := TIdSASLCRAMMD5.Create(aSMTP);
  IdSASLCRAMMD5.UserPassProvider := IdUserPassProvider;
  IdSASLSKey := TIdSASLSKey.Create(aSMTP);
  IdSASLSKey.UserPassProvider := IdUserPassProvider;
  IdSASLOTP := TIdSASLOTP.Create(aSMTP);
  IdSASLOTP.UserPassProvider := IdUserPassProvider;
  IdSASLAnonymous := TIdSASLAnonymous.Create(aSMTP);
  IdSASLExternal := TIdSASLExternal.Create(aSMTP);
  IdSASLLogin := TIdSASLLogin.Create(aSMTP);
  IdSASLLogin.UserPassProvider := IdUserPassProvider;
  IdSASLPlain := TIdSASLPlain.Create(aSMTP);
  IdSASLPlain.UserPassProvider := IdUserPassProvider;

  aSMTP.SASLMechanisms.Add.SASL := IdSASLCRAMSHA1;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLCRAMMD5;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLSKey;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLOTP;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLAnonymous;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLExternal;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLLogin;
  aSMTP.SASLMechanisms.Add.SASL := IdSASLPlain;
end;

function TSendMail.ExtractDomain(const aMailAddress: String): String;
var
  i : integer;
begin
  Result := '';
  i := Pos('@', aMailAddress);
  if i > 0 then
    Result := Copy(aMailAddress, i + 1, MaxInt);
end;

constructor TSendMail.Create;
begin
  FHost:= '127.0.0.1';
  FPort:= 25;
  FSenderName:= '';
  FSenderMailAddress:= '@noreply';
  FRecipients:= '';
  FCCRecipients:= '';
  FBCCRecipients:= '';
  FSubject:= '';
  FUserName:= '';
  FPassword:= '';
  FHTMLImages:= TObjectList.Create(true);
  FAttachments:= TObjectList.Create(true);
  FHTML:= TStringList.Create;
  FPlainText:= TStringList.Create;
  FSSLConnection:= false;
  FTLSConnection:= false;
end;

destructor TSendMail.Destroy;
begin
  FHTMLImages.Free;
  FAttachments.Free;
  FHTML.Free;
  FPlainText.Free;
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

function TSendMail.SetCCRecipients(const aRecipients: String): TSendMail;
begin
  FCCRecipients:= aRecipients;
  Result:= Self;
end;

function TSendMail.SetBCCRecipients(const aRecipients: String): TSendMail;
begin
  FBCCRecipients:= aRecipients;
  Result:= Self;
end;

function TSendMail.SetSubject(const aSubject: String): TSendMail;
begin
  FSubject:= aSubject;
  Result:= Self;
end;

function TSendMail.SetSSLConnection: TSendMail;
begin
  FSSLConnection:= true;
  Result := Self;
end;

function TSendMail.SetTLSConnection: TSendMail;
begin
  FTLSConnection:= true;
  Result := Self;
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

function TSendMail.AttachExcelData(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/x-msexcel', aFileName, aReferenceName);
end;

function TSendMail.AttachExcelOOXML(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', aFileName, aReferenceName);
end;

function TSendMail.AttachExcelXLSX(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', aFileName, aReferenceName);
end;

function TSendMail.AttachPDFData(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/pdf', aFileName, aReferenceName);
end;

function TSendMail.AttachTXTData(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'text/plain', aFileName, aReferenceName);
end;

function TSendMail.AttachXMLData(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'text/xml', aFileName, aReferenceName);
end;

function TSendMail.AttachZIPData(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/x-zip-compressed', aFileName, aReferenceName);
end;

function TSendMail.AttachWordData(const aData: TStream; const aFileName: String; const aReferenceName: String): TSendMail;
begin
  Result:= Self.AttachData(aData, 'application/msword', aFileName, aReferenceName);
end;

function TSendMail.AttachData(const aData: TStream; const aMIMEType: String; const aFileName: String; const aReferenceName: String = ''): TSendMail;
var
  tmp : TAttachedFile;
begin
  tmp := TAttachedFile.Create;
  FAttachments.Add(tmp);
  tmp.FileName:= aFileName;
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

function TSendMail.AddPlainText(const aStrings: TStrings): TSendMail;
begin
  FPlainText.AddStrings(aStrings);
  Result := Self;
end;

function TSendMail.ClearHTML: TSendMail;
begin
  FHTML.Clear;
  Result:= Self;
end;

function TSendMail.ClearPlainText: TSendMail;
begin
  FPlainText.Clear;
  Result := Self;
end;

function TSendMail.Send(out aErrorMessage: String): boolean;
var
  tmpMessage: TIdMessage;
  tmpSMTP: TIdSMTP;
  error: Boolean;
  htmlMessageBuilder : TIdMessageBuilderHtml;
  i : integer;
  f : TAttachedFile;
  tmpAttachment : TIdMessageBuilderAttachment;
begin
  // https://www.indyproject.org/2008/01/16/new-html-message-builder-class/
  // https://www.indyproject.org/2005/08/17/html-messages/
  // https://stackoverflow.com/questions/18541577/using-indy-10-with-exchange-smtp-server

  // IdOpenSSLSetCanLoadSymLinks(False);

  htmlMessageBuilder := TIdMessageBuilderHtml.Create;
  tmpMessage := TIdMessage.Create(nil);
  try
    htmlMessageBuilder.PlainTextCharSet:= 'utf-8';
    htmlMessageBuilder.HtmlCharSet:= 'utf-8';
    tmpMessage.From.Name:= FSenderName;
    tmpMessage.From.Address:= FSenderMailAddress;
    logger.Debug('Name: ' + tmpMessage.From.Name + ' Sender mail address: ' + tmpMessage.From.Address);
    tmpMessage.Recipients.EMailAddresses:= FRecipients;
    if FCCRecipients <> '' then
      tmpMessage.CCList.EMailAddresses:= FCCRecipients;
    if FBCCRecipients <> '' then
      tmpMessage.BccList.EMailAddresses:= FBCCRecipients;
    tmpMessage.Subject:= FSubject;
    tmpMessage.MsgId:= '<' + GenerateRandomIdString(30) + '@' + ExtractDomain(FSenderMailAddress) + '>';
    tmpMessage.UID:= GenerateRandomIdString(30);

    logger.Debug('To: ' + tmpMessage.Recipients.EMailAddresses);
    logger.Debug('CC: ' + tmpMessage.CCList.EMailAddresses);
    logger.Debug('BCC: ' + tmpMessage.BccList.EMailAddresses);

    if FHTML.Count > 0 then
      htmlMessageBuilder.Html.AddStrings(FHTML);
    if FPlainText.Count > 0 then
      htmlMessageBuilder.PlainText.AddStrings(FPlainText);

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
      begin
        tmpAttachment := htmlMessageBuilder.Attachments.Add(f.Data, f.MIMEType, f.Reference);
        if f.FileName <> '' then
          tmpAttachment.FileName:= f.FileName;
      end;
    end;
    htmlMessageBuilder.FillMessage(tmpMessage);

    tmpSMTP := TIdSMTP.Create(nil);
    try
      tmpSMTP.Host:= FHost;
      tmpSMTP.Port:= FPort;

      if FSSLConnection then
      begin
        AddSSLHandler(tmpSMTP);

        if tmpSMTP.Port = SMTP_PORT_EXPLICIT_TLS then
          tmpSMTP.UseTLS := utUseExplicitTLS
        else
          if FTLSConnection then
            tmpSMTP.UseTLS := utUseExplicitTLS
          else
            tmpSMTP.UseTLS := utUseImplicitTLS;
      end
      else if FTLSConnection then
      begin
        AddSSLHandler(tmpSMTP);

        tmpSMTP.UseTLS := utUseExplicitTLS;
      end;

      tmpSMTP.ConnectTimeout:= 5000;
      if (FUserName<>'') or (FPassword<>'') then
      begin
        tmpSMTP.AuthType := satSASL;
        InitSASL(tmpSMTP, FUserName, FPassword);
        if FUserName <> '' then
          tmpSMTP.Username:= FUserName;
        if FPassword <> '' then
          tmpSMTP.Password:= FPassword;
      end
      else
        tmpSMTP.AuthType := satNone;

      tmpSMTP.UseEHLO := true;

      error:= false;
      try
        logger.Debug('Connecting...');
        tmpSMTP.Connect;
        logger.Debug('Sending message...');
        tmpSMTP.Send(tmpMessage);
        logger.Debug('Disconnecting...');
        tmpSMTP.Disconnect(true);
      except
        on e:Exception do
        begin
          aErrorMessage := e.Message;
          error := true;
          logger.Error(e.Message);
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
            logger.Error(e.Message);
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

initialization
  logger := logManager.AddLog('mSendMail');

end.
