// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
//
// ***********************************
// This code was originally taken from:
// https://wiki.freepascal.org/Executing_External_Programs
// ***********************************
//
unit mProcess;

interface

{$mode delphi}
uses Classes, SysUtils, process, dateutils;

function RunCommandTimeout(const exename: TProcessString; const commands: array of TProcessString;
  out outputstring: string; Options: TProcessOptions = []; SWOptions: TShowWindowOptions = swoNone; timeout: integer = 60): boolean;

implementation

type
  { TProcessTimeout }
  TProcessTimeout = class(TProcess)
  public
    timeoutperiod: TTime;
    timedout: boolean;
    started: TDateTime;
    procedure LocalnIdleSleep(Sender, Context: TObject;
      status: TRunCommandEventCode; const message: string);
  end;

procedure TProcessTimeout.LocalnIdleSleep(Sender, Context: TObject;
  status: TRunCommandEventCode; const message: string);
begin
  if status = RunCommandIdle then
  begin
    if (now - started) > timeoutperiod then
    begin
      timedout := True;
      Terminate(255);
      exit;
    end;
    sleep(RunCommandSleepTime);
  end;
end;

function RunCommandTimeout(const exename: TProcessString;
  const commands: array of TProcessString;
  out outputstring: string; Options: TProcessOptions = [];
  SWOptions: TShowWindowOptions = swoNone; timeout: integer = 60): boolean;
var
  p: TProcessTimeout;
  i, exitstatus: integer;
  ErrorString: string;
begin
  p := TProcessTimeout.Create(nil);
  p.OnRunCommandEvent := p.LocalnIdleSleep;
  p.timeoutperiod := timeout / SecsPerDay;
  if Options <> [] then
    P.Options := Options - [poRunSuspended, poWaitOnExit];
  p.options := p.options + [poRunIdle];
  // needed to run the RUNIDLE event. See User Changes 3.2.0

  P.ShowWindow := SwOptions;
  p.Executable := exename;
  if high(commands) >= 0 then
    for i := low(commands) to high(commands) do
      p.Parameters.add(commands[i]);
  p.timedout := False;
  p.started := now;
  try
    // the core loop of runcommand() variants, originally based on the "large output" scenario in the wiki, but continously expanded over 5 years.
    Result := p.RunCommandLoop(outputstring, errorstring, exitstatus) = 0;
    if p.timedout then
      Result := False;
  finally
    p.Free;
  end;
  if exitstatus <> 0 then Result := False;
end;

end.
