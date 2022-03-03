// This is part of the Obo Component Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mExceptionLogForm;

{$mode objfpc}{$H+}

interface
{$I mDefines.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLIntf, ExtCtrls, Clipbrd;

resourcestring
  SWrongEmailMessage = 'Email is non valid.';
  SDoCtrlVTitle = 'Send mail';
  SDoCtrlVMessage = 'Now a new mail message will be created.' + sLineBreak + sLineBreak +'PLEASE CLICK CTRL-V to copy the trace log in the message body before sending it.';

type

  { TExceptionLogForm }
  TExceptionLogForm = class(TForm)
    BtnCancel: TButton;
    BtnHalt: TButton;
    CBSendByMail: TCheckBox;
    EditSendToMailAddresses: TEdit;
    MemoReport: TMemo;
    PanelBottom: TPanel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHaltClick(Sender: TObject);
    procedure CBSendByMailChange(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FUserWantsToShutDown : boolean;
    FReport : String;
    function SendReportByMail : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init (const aReport: String);

    property UserWantsToShutDown: boolean read FUserWantsToShutDown write FUserWantsToShutDown;
  end;

implementation

{$R *.lfm}

uses
  mExceptionLog, mUtility, mGraphicsUtility;


{ TExceptionLogForm }

procedure TExceptionLogForm.FormShow(Sender: TObject);
begin
  if ExceptionLogConfiguration.SendTraceLogByMail and (ExceptionLogConfiguration.TraceLogMailDestination <> '') then
  begin
    CBSendByMail.Checked:= true;
    EditSendToMailAddresses.Text:= ExceptionLogConfiguration.TraceLogMailDestination;
  end
  else
    CBSendByMail.Checked:= false;
end;

function TExceptionLogForm.SendReportByMail: boolean;
var
  s : String;
begin
  Result := true;
  if CBSendByMail.Checked and  (EditSendToMailAddresses.Text <> '') then
  begin
    if ValidEmails(EditSendToMailAddresses.Text, s) then
    begin
      CopyTextToClipboard(FReport);
      MessageDlg(SDoCtrlVTitle, SDoCtrlVMessage, mtWarning, [mbOk], 0);
      OpenURL('mailto:' + s + '?subject=Application trace log&body=Click CTRL-V');
    end
    else
    begin
      ShowMessage(SWrongEmailMessage);
      Result := false;
    end;
  end;
end;

procedure TExceptionLogForm.FormHide(Sender: TObject);
begin
end;

procedure TExceptionLogForm.BtnCancelClick(Sender: TObject);
begin
  FUserWantsToShutDown:= false;
  if SendReportByMail then
    Self.ModalResult:= mrOk;
end;

procedure TExceptionLogForm.BtnHaltClick(Sender: TObject);
begin
  FUserWantsToShutDown:=true;
  if SendReportByMail then
    Self.ModalResult:= mrOk;
end;

procedure TExceptionLogForm.CBSendByMailChange(Sender: TObject);
begin
  EditSendToMailAddresses.Enabled:= (Sender as TCheckBox).Checked;
end;

constructor TExceptionLogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Caption := 'DON''T PANIC';
end;

destructor TExceptionLogForm.Destroy;
begin
  inherited Destroy;
end;

procedure TExceptionLogForm.Init(const aReport: String);
begin
  MemoReport.Text:= aReport;
  FReport:= aReport;
end;

procedure ShowLogForm(const aReport: String; out aWantsToShutDown: boolean);
var
  Dlg : TExceptionLogForm;
begin
  Dlg := TExceptionLogForm.Create(nil);
  try
    Dlg.Init(aReport);
    Dlg.ShowModal;
    aWantsToShutDown:= Dlg.UserWantsToShutDown;
  finally
    Dlg.Free;
  end;
end;

initialization

RegisterExceptionLogPublisher(@ShowLogForm);

{$IFNDEF GUI}
** this unit should not be compiled in a console application **
{$ENDIF}

end.

