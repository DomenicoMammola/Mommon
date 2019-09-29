// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mOutlookMail;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes;

type

  { TmOutlookMailFactory }

  // https://docs.microsoft.com/en-us/office/vba/api/outlook.mailitem
  TmOutlookMailFactory = class
  strict private
    FSubject : String;
    FRecipients : TStringList;
    FCCRecipients : TStringList;
    FBCCRecipients : TStringList;
    FAttachments : TStringList;
    FBody : TStringList;
    FHTMLBody : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CreateMail(const aShowModal : boolean);
    function SetSubject(const aValue : String): TmOutlookMailFactory;
    function AddRecipient(const aValue : String): TmOutlookMailFactory;
    function AddCCRecipient(const aValue : String): TmOutlookMailFactory;
    function AddBCCRecipient(const aValue : String): TmOutlookMailFactory;
    function AddAttachment(const aValue : String): TmOutlookMailFactory;
    function AppendBodyLine(const aValue : String): TmOutlookMailFactory;
    function AppendHTMLBodyLine(const aValue : String): TmOutlookMailFactory;

    class function OutlookAvailable (out aVersion: String): boolean;
  end;


implementation

uses
{$ifdef windows}
  comobj;
{$else}
  SysUtils;
{$endif}

{ TmOutlookMailFactory }

constructor TmOutlookMailFactory.Create;
begin
 FSubject := '';
 FRecipients := TStringList.Create;
 FCCRecipients := TStringList.Create;
 FBCCRecipients := TStringList.Create;
 FAttachments := TStringList.Create;
 FBody := TStringList.Create;
 FHTMLBody := TStringList.Create;
end;

destructor TmOutlookMailFactory.Destroy;
begin
  FRecipients.Free;
  FCCRecipients.Free;
  FBCCRecipients.Free;
  FAttachments.Free;
  FBody.Free;
  FHTMLBody.Free;
  inherited Destroy;
end;

procedure TmOutlookMailFactory.CreateMail(const aShowModal : boolean);
{$ifdef windows}
const
  olMailItem = $00000000;
var
  Outlook: OLEVariant;
  MailItem, MailInspector: Variant;
  i : integer;
  s : WideString;
  tmp, sep : String;
begin
  // https://docs.microsoft.com/en-us/office/vba/api/outlook.mailitem
  try
    Outlook:= GetActiveOleObject('Outlook.Application') ;
  except
    Outlook:= CreateOleObject('Outlook.Application') ;
  end;
  try
    MailItem := Outlook.CreateItem(olMailItem);
    s := UTF8Decode(FSubject);
    MailItem.Subject := s;
    for i := 0 to FRecipients.Count - 1 do
    begin
      s := UTF8Decode(FRecipients.Strings[i]);
      MailItem.Recipients.Add(s);
    end;
    tmp := '';
    sep := '';
    for i := 0 to FCCRecipients.Count -  1 do
    begin
      tmp := tmp + sep + FCCRecipients.Strings[i];
      sep := '; ';
    end;
    if tmp <> '' then
    begin
      s := UTF8Decode(tmp);
      MailItem.CC := s;
    end;
    tmp := '';
    sep := '';
    for i := 0 to FBCCRecipients.Count -  1 do
    begin
      tmp := tmp + sep + FBCCRecipients.Strings[i];
      sep := '; ';
    end;
    if tmp <> '' then
    begin
      s := UTF8Decode(tmp);
      MailItem.BCC := s;
    end;
    for i := 0 to FAttachments.Count - 1 do
    begin
      s := UTF8Decode(FAttachments.Strings[i]);
      MailItem.Attachments.Add(s);
    end;
    if FBody.Count > 0 then
    begin
      s := UTF8Decode(FBody.Text);
      MailItem.Body := s;
    end;
    if FHTMLBody.Count > 0 then
    begin
      s := UTF8Decode(FHTMLBody.Text);
      MailItem.HTMLBody := s;
    end;
    MailInspector := MailItem.GetInspector;
    MailInspector.display(aShowModal); //true means modal
 finally
    Outlook := Unassigned; // VarNull
  end;
end;
{$else}
begin
  raise Exception.Create ('Not supported in this system. Available only under Windows.');
end;
{$endif}

function TmOutlookMailFactory.SetSubject(const aValue: String): TmOutlookMailFactory;
begin
  FSubject:= aValue;
  Result := Self;
end;

function TmOutlookMailFactory.AddRecipient(const aValue: String): TmOutlookMailFactory;
begin
  FRecipients.Add(aValue);
  Result := Self;
end;

function TmOutlookMailFactory.AddCCRecipient(const aValue: String): TmOutlookMailFactory;
begin
 FCCRecipients.Add(aValue);
 Result := Self;
end;

function TmOutlookMailFactory.AddBCCRecipient(const aValue: String): TmOutlookMailFactory;
begin
 FBCCRecipients.Add(aValue);
 Result := Self;
end;

function TmOutlookMailFactory.AddAttachment(const aValue: String): TmOutlookMailFactory;
begin
  FAttachments.Add(aValue);
  Result := Self;
end;

function TmOutlookMailFactory.AppendBodyLine(const aValue: String): TmOutlookMailFactory;
begin
  FBody.Append(aValue);
  Result := Self;
end;

function TmOutlookMailFactory.AppendHTMLBodyLine(const aValue: String): TmOutlookMailFactory;
begin
 FHTMLBody.Append(aValue);
 Result := Self;
end;

class function TmOutlookMailFactory.OutlookAvailable(out aVersion: String): boolean;
{$ifdef windows}
var
  Outlook: OLEVariant;
begin
  Result := false;
  aVersion := '';
  try
    Outlook:= GetActiveOleObject('Outlook.Application') ;
    Result := true;
    aVersion := Outlook.Version;
  except
    Result := false;
  end;
  if not Result then
  begin
    try
      Outlook:= CreateOleObject('Outlook.Application') ;
      Result := true;
      aVersion := Outlook.Version;
    except
      Result := false;
    end;
  end;
  if Result then
    Outlook := Unassigned;
end;
{$else}
begin
  Result := false;
end;
{$endif}

end.
