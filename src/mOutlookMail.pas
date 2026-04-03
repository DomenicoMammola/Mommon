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
    procedure UpdateRecipients(const aValue : String; aRecipients : TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    procedure CreateMail(const aShowModal : boolean);
    function SendReplyInConversation(const aSubject : string; const aReplyAll, aOverrideSubject, aShowModal : boolean) : boolean;
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
  StrUtils, SysUtils, Variants
{$ifdef windows}
  ,comobj
{$endif}
  ;

{ TmOutlookMailFactory }

procedure TmOutlookMailFactory.UpdateRecipients(const aValue: String; aRecipients: TStringList);
var
  tmpList : TStringList;
  i : integer;
begin
  if ContainsStr(aValue, ';') then
  begin
    tmpList := TStringList.Create;
    try
      tmpList.Delimiter:=';';
      tmpList.DelimitedText:=aValue;
      for i := 0 to tmpList.Count - 1 do
      begin
        if trim(tmpList.Strings[i]) <> '' then
          aRecipients.Add(tmpList.Strings[i]);
      end;
    finally
       tmpList.Free;
    end;
  end
  else
    aRecipients.Add(aValue);
end;

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
  MailItem, MailInspector, MailRecipient: Variant;
  i : integer;
  s, signature : WideString;
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
      MailRecipient := MailItem.Recipients.Add(s);
      MailRecipient.Resolve;
    end;
    tmp := '';
    sep := '';
    for i := 0 to FCCRecipients.Count -  1 do
    begin
      tmp := tmp + sep + FCCRecipients.Strings[i];
      sep := ';';
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
      sep := ';';
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
    MailInspector := MailItem.GetInspector;
    if FBody.Count > 0 then
    begin
      s := UTF8Decode(FBody.Text);
      MailItem.Body := s;
    end;
    if FHTMLBody.Count > 0 then
    begin
      s := UTF8Decode(FHTMLBody.Text);
      signature := MailItem.HTMLBody;
      s := s + signature;
      MailItem.HTMLBody := s;
    end;
    MailItem.Recipients.ResolveAll;
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

function TmOutlookMailFactory.SendReplyInConversation(const aSubject: string; const aReplyAll, aOverrideSubject, aShowModal: boolean) : boolean;
{$ifdef windows}
var
  Outlook: OLEVariant;
  Namespace: OLEVariant;
  Store: OleVariant;
  OriginalMail, MailInspector, Mail, MailRecipient: Variant;
  i: Integer;
  s, signature : WideString;
  tmp, sep : String;
  idDeletedItems, idDrafts, idJunk : string;

  procedure InitExclusions(aNameSpace : OLEVariant);
  begin
    idDeletedItems := aNameSpace.GetDefaultFolder(3).EntryID;
    idDrafts := aNameSpace.GetDefaultFolder(16).EntryID;
    idJunk := aNameSpace.GetDefaultFolder(23).EntryID;
  end;

  procedure ProcessFolder(aFolder : Variant);
  var
    i: Integer;
    SubFolder: OleVariant;
    Item: OleVariant;
    Items: Variant;
    currentID: String;
  begin
    currentID := aFolder.EntryID;
    if not (currentID = idDeletedItems) or (currentID = idDrafts) or (currentID = idJunk) then
    begin
      if (aFolder.DefaultItemType = 0) then
      begin
        Items := aFolder.Items;
        Items.IncludeRecurrences := True;
        Items.Sort('[ReceivedTime]', True);
        for i := 1 to Items.Count do
        begin
          Item := Items.Item(i);
          if (Item.MessageClass = 'IPM.Note') and
             (Pos(aSubject, Item.Subject) > 0) then
          begin
            OriginalMail := Item;
            Exit;
          end;
        end;
      end;
    end;

    for i := 1 to aFolder.Folders.Count do
    begin
      SubFolder := aFolder.Folders.Item(i);
      ProcessFolder(SubFolder);
      if not VarIsEmpty(OriginalMail) then
        Exit;
    end;
  end;
begin
  Result := false;
  try
    Outlook:= GetActiveOleObject('Outlook.Application') ;
  except
    Outlook:= CreateOleObject('Outlook.Application') ;
  end;
  try
    Namespace := Outlook.GetNamespace('MAPI');
    InitExclusions(Namespace);
    Store := Namespace.GetDefaultFolder(6).Parent;

    OriginalMail := Unassigned;
    ProcessFolder(Store);

    if not VarIsEmpty(OriginalMail) then
    begin
      Mail := OriginalMail.ReplyAll;

      if aOverrideSubject then
      begin
        s := UTF8Decode(FSubject);
        Mail.Subject := s;
      end;

      if not aReplyAll then
      begin
        while Mail.Recipients.Count > 0 do
          Mail.Recipients.Remove(1);

        tmp := '';
        sep := '';
        for i := 0 to FRecipients.Count - 1 do
        begin
          s := UTF8Decode(FRecipients.Strings[i]);
          MailRecipient := Mail.Recipients.Add(s);
          MailRecipient.Resolve;
        end;
        tmp := '';
        sep := '';
        for i := 0 to FCCRecipients.Count -  1 do
        begin
          tmp := tmp + sep + FCCRecipients.Strings[i];
          sep := ';';
        end;
        if tmp <> '' then
        begin
          s := UTF8Decode(tmp);
          Mail.CC := s;
        end;
        tmp := '';
        sep := '';
        for i := 0 to FBCCRecipients.Count -  1 do
        begin
          tmp := tmp + sep + FBCCRecipients.Strings[i];
          sep := ';';
        end;
        if tmp <> '' then
        begin
          s := UTF8Decode(tmp);
          Mail.BCC := s;
        end;
      end;

      for i := 0 to FAttachments.Count - 1 do
      begin
        s := UTF8Decode(FAttachments.Strings[i]);
        Mail.Attachments.Add(s);
      end;
      MailInspector := Mail.GetInspector;
      if FBody.Count > 0 then
      begin
        s := UTF8Decode(FBody.Text);
        Mail.Body := s;
      end;
      if FHTMLBody.Count > 0 then
      begin
        s := UTF8Decode(FHTMLBody.Text);
        signature := Mail.HTMLBody;
        s := s + signature;
        Mail.HTMLBody := s;
      end;
      Mail.Recipients.ResolveAll;
      MailInspector.display(aShowModal);
      Result := true;
    end;
  finally
    Outlook := Unassigned;
    Mail := Unassigned;
    OriginalMail := Unassigned;
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
  UpdateRecipients(aValue, FRecipients);
  Result := Self;
end;

function TmOutlookMailFactory.AddCCRecipient(const aValue: String): TmOutlookMailFactory;
begin
 UpdateRecipients(aValue, FCCRecipients);
 Result := Self;
end;

function TmOutlookMailFactory.AddBCCRecipient(const aValue: String): TmOutlookMailFactory;
begin
 UpdateRecipients(aValue, FBCCRecipients);
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
