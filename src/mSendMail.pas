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

const
  SMTP_PORT_EXPLICIT_TLS = 587;

type

  TAttachedFileType = (ftFile, ftStream);

  TAttachedFile = class
  public
    FileType : TAttachedFileType;
    MIMEType : String;
    FileName : String;
    Reference : String;
    Data : TStream;
  end;

  TMessagePriority = (mpNone, mpLow, mpNormal, mpHigh);


  { TAbstractSendMail }

  TAbstractSendMail = class abstract
  protected
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
    FXMailer : String;
    FReceiveReadConfirmation : boolean;
    FMessagePriority : TMessagePriority;
  protected
    function ExtractDomain(const aMailAddress: String): String;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function SetHost(const aHostName: String): TAbstractSendMail;
    function SetPort(const aPortNumber: integer): TAbstractSendMail;
    function SetUserName(const aUserName: String): TAbstractSendMail;
    function SetPassword(const aPassword: String): TAbstractSendMail;
    function SetSenderName(const aSenderName: String): TAbstractSendMail;
    function SetSenderMailAddress(const aMailAddress: String): TAbstractSendMail;
    function SetRecipients(const aRecipients: String): TAbstractSendMail;
    function SetCCRecipients(const aRecipients: String): TAbstractSendMail;
    function SetBCCRecipients(const aRecipients: String): TAbstractSendMail;
    function SetSubject(const aSubject: String): TAbstractSendMail;
    function SetSSLConnection : TAbstractSendMail;
    function SetTLSConnection : TAbstractSendMail;
    function SetXMailer(const aXMailer : String): TAbstractSendMail;
    function SetReceiveReadConfirmation(const aReceiveReadConfirmation: boolean): TAbstractSendMail;
    function SetMessagePriority(const aMessagePriority: TMessagePriority): TAbstractSendMail;

    function AddHTMLImageFile(const aFileName, aReferenceName: String): TAbstractSendMail;
    function AddHTMLImage(const aData : TStream; const aMIMEType: String; const aReferenceName : String): TAbstractSendMail;
    function AddHTMLJPEGImage(const aData : TStream; const aReferenceName : String): TAbstractSendMail;
    function AddHTMLPNGImage(const aData : TStream; const aReferenceName: String): TAbstractSendMail;

    function AttachFile(const aFileName : String; const aReferenceName : String = ''): TAbstractSendMail;
    function AttachData(const aData : TStream; const aMIMEType: String; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachExcelData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachExcelOOXML(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachExcelXLSX(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachPDFData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachTXTData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachXMLData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachZIPData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
    function AttachWordData(const aData : TStream; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;

    function AddHTML(const aStrings: TStrings): TAbstractSendMail;
    function AddPlainText (const aStrings : TStrings) : TAbstractSendMail;
    function ClearHTML: TAbstractSendMail;
    function ClearPlainText : TAbstractSendMail;
    function Send(out aErrorMessage: String): boolean; virtual; abstract;
  end;


implementation

uses
  SysUtils;


function TAbstractSendMail.ExtractDomain(const aMailAddress: String): String;
var
  i : integer;
begin
  Result := '';
  i := Pos('@', aMailAddress);
  if i > 0 then
    Result := Copy(aMailAddress, i + 1, MaxInt);
end;

constructor TAbstractSendMail.Create;
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
  FXMailer:= 'SendMail';
  FReceiveReadConfirmation:= false;
  FMessagePriority:= mpNone;
end;

destructor TAbstractSendMail.Destroy;
begin
  FHTMLImages.Free;
  FAttachments.Free;
  FHTML.Free;
  FPlainText.Free;
  inherited Destroy;
end;


function TAbstractSendMail.SetHost(const aHostName: String): TAbstractSendMail;
begin
  FHost:= aHostName;
  Result:= Self;
end;

function TAbstractSendMail.SetPort(const aPortNumber: integer): TAbstractSendMail;
begin
  FPort:= aPortNumber;
  Result:= Self;
end;

function TAbstractSendMail.SetUserName(const aUserName: String): TAbstractSendMail;
begin
  FUserName:= aUserName;
  Result:= Self;
end;

function TAbstractSendMail.SetPassword(const aPassword: String): TAbstractSendMail;
begin
  FPassword:= aPassword;
  Result:= Self;
end;

function TAbstractSendMail.SetSenderName(const aSenderName: String): TAbstractSendMail;
begin
  FSenderName:= aSenderName;
  Result:= Self;
end;

function TAbstractSendMail.SetSenderMailAddress(const aMailAddress: String): TAbstractSendMail;
begin
  FSenderMailAddress:= aMailAddress;
  Result:= Self;
end;

function TAbstractSendMail.SetRecipients(const aRecipients: String): TAbstractSendMail;
begin
  FRecipients:= aRecipients;
  Result:= Self;
end;

function TAbstractSendMail.SetCCRecipients(const aRecipients: String): TAbstractSendMail;
begin
  FCCRecipients:= aRecipients;
  Result:= Self;
end;

function TAbstractSendMail.SetBCCRecipients(const aRecipients: String): TAbstractSendMail;
begin
  FBCCRecipients:= aRecipients;
  Result:= Self;
end;

function TAbstractSendMail.SetSubject(const aSubject: String): TAbstractSendMail;
begin
  FSubject:= aSubject;
  Result:= Self;
end;

function TAbstractSendMail.SetSSLConnection: TAbstractSendMail;
begin
  FSSLConnection:= true;
  Result := Self;
end;

function TAbstractSendMail.SetTLSConnection: TAbstractSendMail;
begin
  FTLSConnection:= true;
  Result := Self;
end;

function TAbstractSendMail.SetXMailer(const aXMailer: String): TAbstractSendMail;
begin
  FXMailer:= aXMailer;
  Result := Self;
end;

function TAbstractSendMail.SetReceiveReadConfirmation(const aReceiveReadConfirmation: boolean): TAbstractSendMail;
begin
  FReceiveReadConfirmation:= aReceiveReadConfirmation;
  Result := Self;
end;

function TAbstractSendMail.SetMessagePriority(const aMessagePriority: TMessagePriority): TAbstractSendMail;
begin
  FMessagePriority:= aMessagePriority;
  Result := Self;
end;


function TAbstractSendMail.AddHTMLImage(const aData: TStream; const aMIMEType: String; const aReferenceName: String): TAbstractSendMail;
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


function TAbstractSendMail.AddHTMLImageFile(const aFileName, aReferenceName: String): TAbstractSendMail;
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

function TAbstractSendMail.AddHTMLJPEGImage(const aData: TStream; const aReferenceName: String): TAbstractSendMail;
begin
  Result := Self.AddHTMLImage(aData, 'image/jpeg', aReferenceName);
end;

function TAbstractSendMail.AddHTMLPNGImage(const aData: TStream; const aReferenceName: String): TAbstractSendMail;
begin
  Result := Self.AddHTMLImage(aData, 'image/x-png', aReferenceName);
end;

function TAbstractSendMail.AttachFile(const aFileName: String; const aReferenceName: String): TAbstractSendMail;
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

function TAbstractSendMail.AttachExcelData(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'application/x-msexcel', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachExcelOOXML(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachExcelXLSX(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachPDFData(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'application/pdf', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachTXTData(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'text/plain', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachXMLData(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'text/xml', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachZIPData(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'application/x-zip-compressed', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachWordData(const aData: TStream; const aFileName: String; const aReferenceName: String): TAbstractSendMail;
begin
  Result:= Self.AttachData(aData, 'application/msword', aFileName, aReferenceName);
end;

function TAbstractSendMail.AttachData(const aData: TStream; const aMIMEType: String; const aFileName: String; const aReferenceName: String = ''): TAbstractSendMail;
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

function TAbstractSendMail.AddHTML(const aStrings: TStrings): TAbstractSendMail;
begin
  FHTML.AddStrings(aStrings);
  Result:= Self;
end;

function TAbstractSendMail.AddPlainText(const aStrings: TStrings): TAbstractSendMail;
begin
  FPlainText.AddStrings(aStrings);
  Result := Self;
end;

function TAbstractSendMail.ClearHTML: TAbstractSendMail;
begin
  FHTML.Clear;
  Result:= Self;
end;

function TAbstractSendMail.ClearPlainText: TAbstractSendMail;
begin
  FPlainText.Clear;
  Result := Self;
end;


end.
