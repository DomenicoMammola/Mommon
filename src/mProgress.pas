// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mProgress;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

type

  ImProgress = interface
  ['{CECDFA83-F26F-45FD-9847-BCE6514A5FB5}']
    procedure Notify(const aMessage: string);
  end;

  { TmFakeProgress }

  TmFakeProgress = class (ImProgress)
  public
    procedure Notify(const aMessage: string);
  end;

  function FakeProgress : TmFakeProgress;

implementation

uses
  SysUtils;

var
  _FakeProgress : TmFakeProgress;

function FakeProgress: TmFakeProgress;
begin
  if not Assigned(_FakeProgress) then
    _FakeProgress := TmFakeProgress.Create;
  Result := _FakeProgress;
end;

{ TmFakeProgress }

procedure TmFakeProgress.Notify(const aMessage: string);
begin
  // do nothing..
end;

initialization
  _FakeProgress := nil;

finalization
  FreeAndNil(_FakeProgress);

end.
