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

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLIntf, ExtCtrls, windows, contnrs;

type

  { TExceptionLogForm }
  TExceptionLogForm = class(TForm)
    BtnCancel: TButton;
    BtnHalt: TButton;
    CBSendByMail: TCheckBox;
    MemoReport: TMemo;
    PanelBottom: TPanel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnHaltClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
    FSendTraceLogByMail: boolean;
    FUserWantsToShutDown : boolean;
    FReport : String;
    procedure SendReportByMail;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init (const aReport: String);

    property UserWantsToShutDown: boolean read FUserWantsToShutDown write FUserWantsToShutDown;
  end;

implementation

{$R *.lfm}

uses
  mExceptionLog;

{ TExceptionLogForm }

procedure TExceptionLogForm.FormShow(Sender: TObject);
begin
  if ExceptionLogConfiguration.SendTraceLogByMail and (ExceptionLogConfiguration.TraceLogMailDestination <> '') then
    CBSendByMail.Checked:= true
  else
    CBSendByMail.Checked:= false;
end;

procedure TExceptionLogForm.SendReportByMail;
begin
  OpenURL('mailto:' + ExceptionLogConfiguration.TraceLogMailDestination + '?subject=Application trace log&body=' + StringReplace(FReport, sLineBreak, '%0D%0A', [rfReplaceAll]));
end;

procedure TExceptionLogForm.FormHide(Sender: TObject);
begin
end;

procedure TExceptionLogForm.BtnCancelClick(Sender: TObject);
begin
  FUserWantsToShutDown:= false;
  if ExceptionLogConfiguration.SendTraceLogByMail and (ExceptionLogConfiguration.TraceLogMailDestination <> '') then
    SendReportByMail;
  Self.ModalResult:= mrOk;
end;

procedure TExceptionLogForm.BtnHaltClick(Sender: TObject);
begin
  FUserWantsToShutDown:=true;
  if ExceptionLogConfiguration.SendTraceLogByMail and (ExceptionLogConfiguration.TraceLogMailDestination <> '') then
    SendReportByMail;
  Self.ModalResult:= mrOk;
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

end.

