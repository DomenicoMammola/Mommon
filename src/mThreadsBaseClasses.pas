// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mThreadsBaseClasses;

{$mode objfpc}{$H+}

interface

{$I mDefines.inc}

uses
  Classes, sysutils;

type

  { TmThread }

  TmThread = class (TThread)
  protected
    FExceptionCallStack : TStringList;
    FLastException : Exception;
    procedure DumpExceptionBackTrace;
    procedure ReRaiseLastException;
  public
    destructor Destroy; override;
    function GetDebugInfo: string; virtual; abstract;
    procedure GetCallStack(aList : TStringList); virtual;
  end;


implementation
{$IFDEF GUI}
uses
  Forms;
{$ENDIF}

{ TmThread }

procedure TmThread.DumpExceptionBackTrace;
var
  i : integer;
  Frames: PPointer;
begin
  if not Assigned(FExceptionCallStack) then
    FExceptionCallStack := TStringList.Create
  else
    FExceptionCallStack.Clear;
  FExceptionCallStack.Add(BackTraceStrFunc(ExceptAddr));
  Frames := ExceptFrames;
  for i := 0 to ExceptFrameCount - 1 do
    FExceptionCallStack.Add(BackTraceStrFunc(Frames[i]));
end;

procedure TmThread.ReRaiseLastException;
begin
  if Assigned(FLastException) then
  begin
    {$IFDEF GUI}
    if Assigned(Application.OnException) then
      Application.OnException(Self, FLastException)
    else
      Application.ShowException(FLastException);
    {$ELSE}
    Sysutils.ShowException(FLastException,ExceptAddr);
    {$ENDIF}
  end;
end;

destructor TmThread.Destroy;
begin
  FreeAndNil(FExceptionCallStack);
  inherited Destroy;
end;

procedure TmThread.GetCallStack(aList: TStringList);
begin
  aList.Clear;
  if Assigned(FExceptionCallStack) then
    aList.AddStrings(FExceptionCallStack);
end;

end.
