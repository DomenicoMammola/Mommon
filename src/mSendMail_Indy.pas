unit mSendMail_Indy;

interface

uses
  Classes, contnrs,
  IdSMTP,
  mSendMail;

  //
  // helpful info taken from:
  //
  // https://mikejustin.wordpress.com/2014/07/27/send-secured-smtp-email-from-delphi-applications/
  //

type
  TIndySendMail = class (TAbstractSendMail)
  strict private
    procedure AddSSLHandler(aSMTP : TIdSMTP);
    procedure InitSASL(aSMTP : TIdSMTP; const aUserName, aPassword : String);
  public
    function Send(out aErrorMessage: String): boolean; override;
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


var
  logger : TmLog;

{ TIndySendMail }



procedure TIndySendMail.AddSSLHandler(aSMTP : TIdSMTP);
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

procedure TIndySendMail.InitSASL(aSMTP : TIdSMTP; const aUserName, aPassword : String);
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



function TIndySendMail.Send(out aErrorMessage: String): boolean;
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
  logger := logManager.AddLog('mSendMail_Indy');


end.
