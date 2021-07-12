// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGetMail;

interface

{$DEFINE MLOG_AVAILABLE}

uses
  Classes, contnrs;

type

  { TReceivedMailAttachment }

  TReceivedMailAttachment = class
  strict private
    FFileName : String;
    FFileData : TMemoryStream;
  private
    procedure Analyze;
  public
    constructor Create;
    destructor Destroy; override;

    property FileName : String read FFileName write FFileName;
    property FileData : TMemoryStream read FFileData;
  end;


  TReceivedMail = class
  strict private
    FSubject : String;
    FRecipients : TStringList;
    FCCRecipients : TStringList;
    FBCCRecipients : TStringList;
    FBody : TStringList;
    FHTMLBody : TStringList;
    FReceivedDate : TDateTime;
    FSender : String;
    FAttachments : TObjectList;
    function GetHTMLReportHeader : String;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAttachment (const aIndex: integer) : TReceivedMailAttachment;
    function AttachmentsCount: integer;
    function AddAttachment : TReceivedMailAttachment;
    procedure SaveToFolder (const aFolder : String);
    function GetHTMLReport : String;
    function GetTXTReport : String;
    function GetTXTReportHeader : String;

    property Subject : String read FSubject write FSubject;
    property Recipients : TStringList read FRecipients;
    property CCRecipients : TStringList read FCCRecipients;
    property BCCRecipients : TStringList read FBCCRecipients;
    property Body : TStringList read FBody;
    property HTMLBody : TStringList read FHTMLBody;
    property ReceivedDate : TDateTime read FReceivedDate write FReceivedDate;
    property Sender : String read FSender write FSender;
  end;

  { TGetMailPop3 }

  TGetMailPop3 = class
  strict private
    FHost: String;
    FPort: Integer;
    FUserName: String;
    FPassword: String;
    FAcceptOnlyMailFromSpecificDomain : boolean;
    FAllowedSenderDomain : String;
    FSSLConnection : boolean;
    FTLSConnection : boolean;
    FReceivedMails : TObjectList;
    function AddReceivedMail : TReceivedMail;
  public
    constructor Create;
    destructor Destroy; override;

    function CheckMail(out aErrorMessage: String) : boolean;
    function MailsCount : integer;
    function GetMail (const aIndex : integer): TReceivedMail;

    function SetHost(const aHostName: String): TGetMailPop3;
    function SetPort(const aPortNumber: integer): TGetMailPop3;
    function SetUserName(const aUserName: String): TGetMailPop3;
    function SetPassword(const aPassword: String): TGetMailPop3;
    function SetSSLConnection : TGetMailPop3;
    function SetTLSConnection : TGetMailPop3;
    function SetAcceptOnlyMailFromSpecificDomain : TGetMailPop3;
    function SetAllowedSenderDomain(const aAllowedSenderDomain: String): TGetMailPop3;
  end;

implementation

uses
  sysutils,
  IdPOP3,
  IdMessage, IdMessageBuilder, IdAttachment, IdText, IdStrings,
  IdComponent, IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase,
  IdBaseComponent, IdIOHandler,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdSASLLogin,
  IdSASL_CRAM_SHA1, IdSASL, IdSASLUserPass, IdSASL_CRAM_MD5,
  IdSASLSKey, IdSASLPlain, IdSASLOTP, IdSASLExternal,
  IdSASLAnonymous, IdUserPassProvider,
  mUtility{$IFDEF MLOG_AVAILABLE}, mLog{$ENDIF};

{$IFDEF MLOG_AVAILABLE}
var
  logger : TmLog;
{$ENDIF}

procedure ImportMessage(const aIdMessage : TIdMessage; aReceivedMail : TReceivedMail);
var
  i : integer;
  newAttachment : TReceivedMailAttachment;
begin
  aReceivedMail.Sender := aIdMessage.From.Address;
  for i := 0 to aIdMessage.Recipients.Count - 1 do
    aReceivedMail.Recipients.Add(aIdMessage.Recipients.Items[i].Address);
  for i := 0 to aIdMessage.CCList.Count - 1 do
    aReceivedMail.CCRecipients.Add(aIdMessage.CCList.Items[i].Address);
  for i := 0 to aIdMessage.BccList.Count - 1 do
    aReceivedMail.BCCRecipients.Add(aIdMessage.BccList.Items[i].Address);
  aReceivedMail.Subject := aIdMessage.Subject;
  aReceivedMail.ReceivedDate:= aIdMessage.Date;
  if not aIdMessage.IsBodyEmpty then
    aReceivedMail.Body.AddStrings(aIdMessage.Body);
  aIdMessage.MessageParts.CountParts;
  for i := 0 to aIdMessage.MessageParts.Count - 1 do
  begin
    {$IFDEF MLOG_AVAILABLE}
    logger.Debug(aIdMessage.MessageParts.Items[i].ClassName);
    {$ENDIF}

    if aIdMessage.MessageParts.Items[i] is TIdAttachment then
    begin
      newAttachment := aReceivedMail.AddAttachment;
      TIdAttachment(aIdMessage.MessageParts.Items[i]).SaveToStream(newAttachment.FileData);
      newAttachment.FileName := TIdAttachment(aIdMessage.MessageParts.Items[i]).FileName;
      newAttachment.Analyze;
    end
    else if aIdMessage.MessageParts.Items[i] is TIdText then
    begin
      if (CompareText(aIdMessage.MessageParts.Items[i].ContentType, 'text/plain') = 0) then
      begin
        if aIdMessage.IsBodyEmpty then
          aReceivedMail.Body.AddStrings((aIdMessage.MessageParts.Items[i] as TIdText).Body);
      end
      else if (CompareText(aIdMessage.MessageParts.Items[i].ContentType, 'text/html') = 0) then
      begin
        aReceivedMail.HTMLBody.AddStrings((aIdMessage.MessageParts.Items[i] as TIdText).Body);
      end;
      {$IFDEF MLOG_AVAILABLE}
      logger.Debug('Part ' + IntToStr(i) +  ' - content type:' + (aIdMessage.MessageParts.Items[i] as TIdText).ContentType);
      logger.Debug('Part ' + IntToStr(i) +  ' - content description:' + (aIdMessage.MessageParts.Items[i] as TIdText).ContentDescription);
      logger.Debug('Part ' + IntToStr(i) +  ':' + (aIdMessage.MessageParts.Items[i] as TIdText).Body.Text);
      {$ENDIF}
    end;
  end;
end;


{ TReceivedMail }

function TReceivedMail.GetHTMLReportHeader: String;
var
  i : integer;
  sep : String;
begin
  Result := '';
  Result := Result + '<table style="font-family: Times New Roman; font-size: 12pt;">';
  Result := Result + '<tr style="height: 18px; vertical-align: top; "><td style="font-weight: bold; white-space:nowrap; ">From:</td><td>' + Self.Sender + '</td></tr>';
  Result := Result + '<tr style="height: 18px; vertical-align: top; "><td style="font-weight: bold; white-space:nowrap;">Sent on:</td><td>' + DateTimeToStr(Self.ReceivedDate) + '<br></td></tr>';
  Result := Result + '<tr style="height: 18px; vertical-align: top; "><td style="font-weight: bold; white-space:nowrap; ">To:</td><td>';
  sep := '';
  for i := 0 to Self.Recipients.Count - 1 do
  begin
    Result := Result + sep + Self.Recipients.Strings[i];
    sep := '; ';
  end;
  Result := Result + '</td></tr>';
  Result := Result + '<tr style="height: 18px; vertical-align: top; "><td style="font-weight: bold; white-space:nowrap; ">CC:</td><td>';
  sep := '';
  for i := 0 to Self.CCRecipients.Count - 1 do
  begin
    Result := Result + sep + Self.CCRecipients.Strings[i];
    sep := '; ';
  end;
  Result := Result + '</td></tr>';
  Result := Result + '<tr style="height: 18px; vertical-align: top; "><td style="font-weight: bold; white-space:nowrap;">Subject:</td><td>' + StrHtmlEncode(Self.Subject) + '<br></td></tr>';
  if Self.AttachmentsCount > 0 then
  begin
    Result := Result + '<tr style="height: 18px; vertical-align: top; "><td style="font-weight: bold; white-space:nowrap; ">Attachments:</td><td>';
    sep := '';
    for i := 0 to Self.AttachmentsCount - 1 do
    begin
      Result := Result + sep + StrHtmlEncode(Self.GetAttachment(i).FileName);
      sep := ', ';
    end;
    Result := Result + '</td></tr>';
  end;

  Result := Result + '<tr style="height: 18px; vertical-align: top; "><td>&nbsp;</td><td>&nbsp;</td></tr>';
  Result := Result + '</table><br>';
end;

function TReceivedMail.GetTXTReportHeader: String;
var
  i : integer;
  sep : String;
begin
  Result := '';
  Result := Result + 'From: ' + Self.Sender + sLineBreak;
  Result := Result + 'Sent on: ' + DateTimeToStr(Self.ReceivedDate) + sLineBreak;
  Result := Result + 'To: ';
  sep := '';
  for i := 0 to Self.Recipients.Count - 1 do
  begin
    Result := Result + sep + Self.Recipients.Strings[i];
    sep := '; ';
  end;
  Result := Result + sLineBreak;
  Result := Result + 'CC: ';
  sep := '';
  for i := 0 to Self.CCRecipients.Count - 1 do
  begin
    Result := Result + sep + Self.CCRecipients.Strings[i];
    sep := '; ';
  end;
  Result := Result + sLineBreak;
  Result := Result + 'Subject: ' + Self.Subject + sLineBreak;
  if Self.AttachmentsCount > 0 then
  begin
    Result := Result + 'Attachments: ';
    sep := '';
    for i := 0 to Self.AttachmentsCount - 1 do
    begin
      Result := Result + sep + Self.GetAttachment(i).FileName;
      sep := ', ';
    end;
    Result := Result + sLineBreak;
  end;
  Result := Result + sLineBreak;
end;

constructor TReceivedMail.Create;
begin
  FSubject := '';
  FRecipients := TStringList.Create;
  FCCRecipients := TStringList.Create;
  FBCCRecipients := TStringList.Create;
  FAttachments := TObjectList.Create(true);
  FBody := TStringList.Create;
  FHTMLBody := TStringList.Create;
  FReceivedDate := 0;
  FSender := '';
end;

destructor TReceivedMail.Destroy;
begin
  FRecipients.Free;
  FCCRecipients.Free;
  FBCCRecipients.Free;
  FAttachments.Free;
  FBody.Free;
  FHTMLBody.Free;
  inherited Destroy;
end;

function TReceivedMail.GetAttachment(const aIndex: integer): TReceivedMailAttachment;
begin
  Result := FAttachments.Items[aIndex] as TReceivedMailAttachment;
end;

function TReceivedMail.AttachmentsCount: integer;
begin
  Result := FAttachments.Count;
end;

function TReceivedMail.AddAttachment: TReceivedMailAttachment;
begin
  Result := TReceivedMailAttachment.Create;
  FAttachments.Add(Result);
end;

function CheckExistingFile(const aFileName: String) : String;
var
  tmp : String;
  k : integer;
begin
  if FileExists(aFileName) then
  begin
    k := 2;
    tmp := mUtility.AddNumberToFileName(aFileName, k);
    while FileExists(tmp) do
    begin
      inc(k);
      tmp := mUtility.AddNumberToFileName(aFileName, k);
    end;
    Result := tmp;
  end
  else
    Result := aFileName;
end;

procedure TReceivedMail.SaveToFolder(const aFolder: String);
var
  i: integer;
  ff: String;
begin
  if FBody.Count > 0 then
    FBody.SaveToFile(CheckExistingFile(IncludeTrailingPathDelimiter(aFolder) + 'body.txt'));
  if FHTMLBody.Count > 0 then
    FHTMLBody.SaveToFile(CheckExistingFile(IncludeTrailingPathDelimiter(aFolder) + 'body.htm'));

  for i := 0 to AttachmentsCount - 1 do
  begin
    if GetAttachment(i).FileName <> '' then
      ff := GetAttachment(i).FileName
    else
      ff := 'undefined';
    GetAttachment(i).FileData.SaveToFile(CheckExistingFile(IncludeTrailingPathDelimiter(aFolder) + ff));
  end;
end;

function TReceivedMail.GetHTMLReport: String;
var
  i, p, le : integer;
  toAdd, allLower : String;
begin
  Result := '';

  if FHTMLBody.Count > 0 then
  begin
    toAdd := FHTMLBody.Text;
    allLower := LowerCase(toAdd);
    p := Pos('<body', allLower);
    if p > 0 then
    begin
      Result := Copy(toAdd, 1, p);
      toAdd := Copy(toAdd, p + 1, MaxInt);
      p := 1;
      le := Length(toAdd);
      while (p <= le) and (toAdd[p] <> '>') do
        inc(p);
      Result := Result + Copy(toAdd, 1, p);
      toAdd := Copy(toAdd, p + 1, MaxInt);
    end;
    Result := Result + GetHTMLReportHeader + toAdd;
  end
  else
  begin
    Result := '<html><body>' + GetHTMLReportHeader + '<span>';
    for i := 0 to FBody.Count - 1 do
      Result := Result + FBody.Strings[i] + '<br>';
    Result := Result + '</span></body></html>';
  end;
  logger.Debug('GetHTMLReport:' + Result);
end;

function TReceivedMail.GetTXTReport: String;
begin
  Result := GetTXTReportHeader + FBody.Text;
end;

{ TReceivedMailAttachment }

procedure TReceivedMailAttachment.Analyze;
var
  list : TStringList;
  i : integer;
  fromFound, toFound, subjectFound : boolean;
  subject, curLine : String;
begin
  if FFileName = '' then
  begin

    // https://www.w3.org/Protocols/rfc822/
    // trying to read as eml message...

    list := TStringList.Create;
    try
      try
        FFileData.Position:= 0;
        list.LoadFromStream(FFileData);
        FFileData.Position:= 0;
        fromFound := false;
        toFound := false;
        subjectFound := false;
        subject := '';

        for i := 0 to list.Count - 1 do
        begin
          curLine := Trim(list.Strings[i]);
          fromFound := fromFound or (CompareText(LeftStr(curLine, 5), 'From:') = 0);
          toFound := toFound or (CompareText(LeftStr(curLine, 3), 'To:') = 0);
          if (not subjectFound) and (CompareText(LeftStr(curLine, 8), 'Subject:') = 0) then
          begin
            subjectFound:= true;
            subject:= Trim(Copy(curLine, 9, 9999));
          end;

          if fromFound and toFound and subjectFound then
          begin
            if subject <> '' then
              FFileName:= ChangeFileExt(SanitizeSubstringForFileName(subject), '.eml')
            else
              FFileName := 'no_subject.eml';
          end;
        end;
      except
        on e: Exception do
        begin
          FFileName:= 'undefined';
          exit;
        end;
      end;
    finally
      list.Free;
    end;
  end;
end;

constructor TReceivedMailAttachment.Create;
begin
  FFileData := TMemoryStream.Create;
  FFileName:= '';
end;

destructor TReceivedMailAttachment.Destroy;
begin
  FFileData.Free;
  inherited Destroy;
end;

{ TGetMailPop3 }

procedure AddSSLHandler(aPop3: TIdPOP3);
var
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(aPop3);
  // SSL/TLS handshake determines the highest available SSL/TLS version dynamically
  SSLHandler.SSLOptions.Method := sslvSSLv23;
  SSLHandler.SSLOptions.Mode := sslmClient;
  SSLHandler.SSLOptions.VerifyMode := [];
  SSLHandler.SSLOptions.VerifyDepth := 0;
  aPop3.IOHandler := SSLHandler;
end;

procedure InitSASL(aPop3: TIdPOP3; const aUserName, aPassword: String);
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
  IdUserPassProvider := TIdUserPassProvider.Create(aPop3);
  IdUserPassProvider.Username := aUserName;
  IdUserPassProvider.Password:= aPassword;

  IdSASLCRAMSHA1 := TIdSASLCRAMSHA1.Create(aPop3);
  IdSASLCRAMSHA1.UserPassProvider := IdUserPassProvider;
  IdSASLCRAMMD5 := TIdSASLCRAMMD5.Create(aPop3);
  IdSASLCRAMMD5.UserPassProvider := IdUserPassProvider;
  IdSASLSKey := TIdSASLSKey.Create(aPop3);
  IdSASLSKey.UserPassProvider := IdUserPassProvider;
  IdSASLOTP := TIdSASLOTP.Create(aPop3);
  IdSASLOTP.UserPassProvider := IdUserPassProvider;
  IdSASLAnonymous := TIdSASLAnonymous.Create(aPop3);
  IdSASLExternal := TIdSASLExternal.Create(aPop3);
  IdSASLLogin := TIdSASLLogin.Create(aPop3);
  IdSASLLogin.UserPassProvider := IdUserPassProvider;
  IdSASLPlain := TIdSASLPlain.Create(aPop3);
  IdSASLPlain.UserPassProvider := IdUserPassProvider;

  aPop3.SASLMechanisms.Add.SASL := IdSASLCRAMSHA1;
  aPop3.SASLMechanisms.Add.SASL := IdSASLCRAMMD5;
  aPop3.SASLMechanisms.Add.SASL := IdSASLSKey;
  aPop3.SASLMechanisms.Add.SASL := IdSASLOTP;
  aPop3.SASLMechanisms.Add.SASL := IdSASLAnonymous;
  aPop3.SASLMechanisms.Add.SASL := IdSASLExternal;
  aPop3.SASLMechanisms.Add.SASL := IdSASLLogin;
  aPop3.SASLMechanisms.Add.SASL := IdSASLPlain;
end;

function TGetMailPop3.AddReceivedMail: TReceivedMail;
begin
  Result := TReceivedMail.Create;
  FReceivedMails.Add(Result);
end;

constructor TGetMailPop3.Create;
begin
  FHost:= '127.0.0.1';
  FPort:= 995;
  FUserName:= '';
  FPassword:= '';
  FSSLConnection:= false;
  FTLSConnection:= false;
  FReceivedMails := TObjectList.Create(true);
end;

destructor TGetMailPop3.Destroy;
begin
  FReceivedMails.Free;
  inherited Destroy;
end;

function TGetMailPop3.CheckMail(out aErrorMessage: String): boolean;
var
  tmpPop3 : TIdPOP3;
  error : boolean;
  i, numMessages : integer;
  msg : TIdMessage;
begin
  Result := false;
  error := false;
  aErrorMessage:= '';
  FReceivedMails.Clear;

  tmpPop3 := TIdPOP3.Create(nil);
  try
    tmpPop3.Host:= FHost;
    tmpPop3.Port:= FPort;

    if FSSLConnection then
    begin
      AddSSLHandler(tmpPop3);

      if FTLSConnection then
        tmpPop3.UseTLS := utUseExplicitTLS
      else
        tmpPop3.UseTLS := utUseImplicitTLS;
    end
    else if FTLSConnection then
    begin
      AddSSLHandler(tmpPop3);

      tmpPop3.UseTLS := utUseExplicitTLS;
    end;

    tmpPop3.ConnectTimeout:= 5000;

    tmpPop3.AuthType := patUserPass;
    if FUserName <> '' then
      tmpPop3.Username:= FUserName;
    if FPassword <> '' then
      tmpPop3.Password:= FPassword;
//      InitSASL(tmpPop3, FUserName, FPassword);
//      tmpPop3.AuthType:= patSASL;

    error:= false;
    try
      tmpPop3.Connect;
      try
        numMessages := tmpPop3.CheckMessages;
        For i := 1 to numMessages do
        begin
          msg := TIdMessage.Create;
          try
            tmpPop3.Retrieve(i, msg);
            if (not FAcceptOnlyMailFromSpecificDomain) or (CompareText(msg.From.Domain, FAllowedSenderDomain) = 0) then
              ImportMessage(msg, AddReceivedMail)
              {$IFDEF MLOG_AVAILABLE}
            else
              logger.Info('Discarded mail from ' + msg.From.Address)
              {$ENDIF}
            ;
            tmpPop3.Delete(i);
          finally
            msg.Free;
          end;
        end;
      finally
        tmpPop3.Disconnect(true);
      end;
    except
      on e:Exception do
      begin
        aErrorMessage := e.Message;
        error := true;
      end;
    end;
  finally
    tmpPop3.Free;
  end;
  Result := not error;
end;

function TGetMailPop3.MailsCount: integer;
begin
  Result := FReceivedMails.Count;
end;

function TGetMailPop3.GetMail(const aIndex: integer): TReceivedMail;
begin
  Result := FReceivedMails.Items[aIndex] as TReceivedMail;
end;

function TGetMailPop3.SetHost(const aHostName: String): TGetMailPop3;
begin
  FHost := aHostName;
  Result := Self;
end;

function TGetMailPop3.SetPort(const aPortNumber: integer): TGetMailPop3;
begin
  FPort:= aPortNumber;
  Result := Self;
end;

function TGetMailPop3.SetUserName(const aUserName: String): TGetMailPop3;
begin
  FUserName:= aUserName;
  Result := Self;
end;

function TGetMailPop3.SetPassword(const aPassword: String): TGetMailPop3;
begin
  FPassword:= aPassword;
  Result := Self;
end;

function TGetMailPop3.SetSSLConnection: TGetMailPop3;
begin
  FSSLConnection:= true;
  Result := Self;
end;

function TGetMailPop3.SetTLSConnection: TGetMailPop3;
begin
  FTLSConnection:=true;
  Result := Self;
end;

function TGetMailPop3.SetAcceptOnlyMailFromSpecificDomain: TGetMailPop3;
begin
  FAcceptOnlyMailFromSpecificDomain:= true;
  Result := Self;
end;

function TGetMailPop3.SetAllowedSenderDomain(const aAllowedSenderDomain: String): TGetMailPop3;
begin
  FAllowedSenderDomain:= aAllowedSenderDomain;
  Result := Self;
end;

{$IFDEF MLOG_AVAILABLE}
initialization
  logger := logManager.AddLog('mGetMail');
{$ENDIF}

end.
