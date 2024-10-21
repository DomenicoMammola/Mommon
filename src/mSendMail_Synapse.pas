unit mSendMail_Synapse;

interface

uses
  Classes, contnrs,
  smtpsend,
  mSendMail;

type

  { TSynapseSendMail }

  TSynapseSendMail = class (TAbstractSendMail)
  strict private
    FSynBCCRecipients : TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Send(out aErrorMessage: String): boolean; override;
  end;

implementation

uses
  SysUtils,
  mimemess, mimepart, synachar, ssl_openssl3, mimeinln,
  mLog, mUtility;

var
  logger : TmLog;

{ TSynapseSendMail }

constructor TSynapseSendMail.Create;
begin
  inherited Create;
  FSynBCCRecipients := TStringList.Create;
end;

destructor TSynapseSendMail.Destroy;
begin
  FSynBCCRecipients.Free;
  inherited Destroy;
end;

function TSynapseSendMail.Send(out aErrorMessage: String): boolean;
var
  smtp: TSMTPSend;
  MimeMsg: TMimeMess;
  RootPart, HTMLImagesPart, HTMLImagePart, HTMLPart: TMimePart;
  tmpList : TStringList;
  i : integer;
  curAttachment : TAttachedFile;
  s, dest : String;
begin
  Result := false;

  MimeMsg := TMimeMess.Create;
  smtp := TSMTPSend.Create;
  try


    MimeMsg.Header.From := FSenderName + ' <' + FSenderMailAddress + '>';

    tmpList := TStringList.Create;
    try
      logger.Debug('Recipients: ' + FRecipients);
      tmpList.Delimiter:= ';';
      tmpList.DelimitedText:= FRecipients;

      for i := 0 to tmpList.Count - 1 do
      begin
        dest := trim(tmpList.Strings[i]);
        if dest <> '' then
        begin
          logger.Debug('TO: ' + dest);
          MimeMsg.Header.ToList.Add(dest);
        end;
      end;
    finally
      tmpList.Free;
    end;

    MimeMsg.Header.Subject := FSubject;
    case FMessagePriority of
      mpLow : MimeMsg.Header.Priority := MP_low;
      mpHigh : MimeMsg.Header.Priority:= MP_high;
      mpNormal : MimeMsg.Header.Priority:= MP_normal;
    end;

    MimeMsg.Header.Date := Now;
    MimeMsg.Header.CharsetCode := UTF_8;
    MimeMsg.Header.ReplyTo := FSenderName + ' <' + FSenderMailAddress + '>';
    MimeMsg.Header.MessageID := '<' + GenerateRandomIdString(30) + '@' + ExtractDomain(FSenderMailAddress) + '>';
    MimeMsg.Header.XMailer:= FXMailer;
    if FReceiveReadConfirmation then
      MimeMsg.Header.CustomHeaders.Add('Disposition-Notification-To: ' + MimeMsg.Header.From); //read confirmation;


    tmpList := TStringList.Create;
    try
      tmpList.Delimiter:= ';';
      tmpList.DelimitedText:= FCCRecipients;
      logger.Debug('CC Recipients: ' + FCCRecipients);

      for i := 0 to tmpList.Count - 1 do
      begin
        dest := trim(tmpList.Strings[i]);
        if dest <> '' then
        begin
          logger.Debug('CC: ' + dest);
          MimeMsg.Header.CCList.Add(dest);
        end;
      end;
    finally
      tmpList.Free;
    end;

    FSynBCCRecipients.Clear;

    tmpList := TStringList.Create;
    try
      tmpList.Delimiter:= ';';
      tmpList.DelimitedText:= FBCCRecipients;
      logger.Debug('BCC Recipients: ' + FBCCRecipients);

      s := '';
      for i := 0 to tmpList.Count - 1 do
      begin
        dest := trim(tmpList.Strings[i]);
        if dest <> '' then
        begin
          FSynBCCRecipients.Add(dest);
          if s = '' then
            s := InlineEmailEx(dest, UTF_8)
          else
            s := s + ', ' + InlineEmailEx(dest, UTF_8);
        end;
        if s <> '' then
        begin
          MimeMsg.Header.CustomHeaders.Add('BCC: ' + s);
          logger.Debug('Custom header BCC: ' + s);
        end;
      end;
    finally
      tmpList.Free;
    end;

    if FHTML.Count = 0 then
    begin
      RootPart := MimeMsg.AddPartTextEx(FPlainText, nil, UTF_8, True, ME_8BIT);
      RootPart.CharsetCode := UTF_8;
      RootPart.EncodingCode := ME_QUOTED_PRINTABLE;
    end
    else
    begin
      if FHTMLImages.Count > 0 then
      begin
        // https://stackoverflow.com/questions/20973354/how-to-send-a-email-with-a-base64-embedded-image

        RootPart := MimeMsg.AddPartMultipart('mixed',nil);
        HTMLImagesPart := MimeMsg.AddPartMultipart('related', RootPart);

        HTMLPart := MimeMsg.AddPart(RootPart);
        HTMLPart.ConvertCharset := False;
        with HTMLPart do
        begin
          FHTML.SaveToStream(DecodedLines);
          Primary := 'text';
          Secondary := 'html';
          Description := 'HTML text';
          Disposition := 'inline';
          CharsetCode := UTF_8;
          EncodingCode := ME_QUOTED_PRINTABLE;
          EncodePart;
          EncodePartHeader;
        end;

        for i := 0 to FHTMLImages.Count - 1 do
        begin
          curAttachment := FHTMLImages.Items[i] as TAttachedFile;
          if curAttachment.FileType = ftFile then
            HTMLImagePart := MimeMsg.AddPartHTMLBinaryFromFile(curAttachment.FileName, curAttachment.Reference, HTMLImagesPart)
          else
            HTMLImagePart := MimeMsg.AddPartHTMLBinary(curAttachment.Data, curAttachment.FileName, curAttachment.Reference, HTMLImagesPart);
          HTMLImagesPart.AddSubPart;
          HTMLImagesPart.AssignSubParts(HTMLImagePart);
        end;
      end
      else
      begin
        RootPart := MimeMsg.AddPart(nil);
        RootPart.ConvertCharset := False;
        with RootPart do
        begin
          FHTML.SaveToStream(DecodedLines);
          Primary := 'text';
          Secondary := 'html';
          Description := 'HTML text';
          Disposition := 'inline';
          CharsetCode := UTF_8;
          EncodingCode := ME_QUOTED_PRINTABLE;
          EncodePart;
          EncodePartHeader;
        end;
      end;
    end;

    for i := 0 to FAttachments.Count - 1 do
    begin
      curAttachment := FAttachments.Items[I] as TAttachedFile;
      if curAttachment.FileType = ftFile then
        MimeMsg.AddPartBinaryFromFile(curAttachment.FileName, RootPart)
      else
        MimeMsg.AddPartBinary(curAttachment.Data, curAttachment.FileName, RootPart);
    end;

    MimeMsg.EncodeMessage;

    smtp.UserName := FUserName;
    smtp.Password := FPassword;
    smtp.TargetHost := FHost;
    smtp.TargetPort := IntToStr(FPort);
    smtp.AutoTLS := true;

    if FSSLConnection then
      smtp.FullSSL:= true;

    if FTLSConnection then
    begin
      logger.Debug('SMTP StartTLS');
      if not smtp.StartTLS() then
      begin
        aErrorMessage := 'SMTP ERROR: StartTLS:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
        exit;
      end;
    end;

    logger.Debug('SMTP Login');
    if not smtp.Login() then
    begin
      aErrorMessage := 'SMTP ERROR: Login:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
      exit;
    end;


    if not smtp.AuthDone then
    begin
      aErrorMessage:= 'SMTP ERROR: AuthDone:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
      exit;
    end;

    logger.Debug('SMTP Mail');
    if not smtp.MailFrom(FSenderMailAddress, Length(FSenderMailAddress)) then
    begin
      aErrorMessage := 'SMTP ERROR: MailFrom:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
      exit;
    end;

    for i := 0 to MimeMsg.Header.ToList.Count - 1 do
    begin
      if not smtp.MailTo(MimeMsg.Header.ToList.Strings[i]) then
      begin
        aErrorMessage := 'SMTP ERROR: MailTo:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
        exit;
      end;
    end;
    for i := 0 to MimeMsg.Header.CCList.Count - 1 do
    begin
      if not smtp.MailTo(MimeMsg.Header.CCList.Strings[i]) then
      begin
        aErrorMessage := 'SMTP ERROR: MailTo:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
        exit;
      end;
    end;
    for i := 0 to FSynBCCRecipients.Count - 1 do
    begin
      if not smtp.MailTo(FSynBCCRecipients.Strings[i]) then
      begin
        aErrorMessage := 'SMTP ERROR: MailTo:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
        exit;
      end;
    end;

    if not smtp.MailData(MimeMSg.Lines) then
    begin
      aErrorMessage := 'SMTP ERROR: MailData:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
      exit;
    end;

    logger.Debug('SMTP Logout');
    if not smtp.Logout() then
    begin
      aErrorMessage := 'SMTP ERROR: Logout:' + smtp.EnhCodeString + ' ' + smtp.ResultString;
      exit;
    end;
    logger.Debug('Sent!');
  finally
    smtp.Free;
    MimeMsg.Free;
  end;
  Result := true;
end;


initialization
  logger := logManager.AddLog('mSendMail_Synapse');

end.
