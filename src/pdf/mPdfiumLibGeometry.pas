// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (www.mammola.net)
//
unit mPdfiumLibGeometry;

interface

uses
  Classes, PdfiumCore;

function ContainingRect(const aRects : TPdfRectArray): TPdfRect;
function RectIsOnNextRow(const aRect1, aRect2 : TPdfRect; const aRowsSpacing : double = 6) : boolean;
function RectIsOnSameRow(const aRect1, aRect2 : TPdfRect; const aTolerance : double = 1) : boolean;
function RectIsOnSameColumn(const aRect1, aRect2: TPdfRect; const aTextAlignment : TAlignment; const aTolerance : double = 1.1) : boolean;
function RectIsFollowingOnTheSameRow(const aRect1, aRect2 : TPdfRect; const aSpaceWidth : double = 8.5) : boolean;


implementation

uses
  Math;

function ContainingRect(const aRects: TPdfRectArray): TPdfRect;
var
  i : integer;
  l, r, t, b : double;
begin
  if Length(aRects) > 0 then
  begin
    l := aRects[0].Left;
    r := aRects[0].Right;
    t := aRects[0].Top;
    b := aRects[0].Bottom;

    for i := 1 to Length(aRects) -1 do
    begin
      l := Min(l, aRects[i].Left);
      r := Max(r, aRects[i].Right);
      t := Max(t, aRects[i].Top);
      b := Min(b, aRects[i].Bottom);
    end;
    Result := TPdfRect.New(l, t, r, b);
  end
  else
    Result := TPdfRect.Empty;
end;

function RectIsOnNextRow(const aRect1, aRect2: TPdfRect; const aRowsSpacing : double): boolean;
begin
  Result := (aRect1.Bottom - aRect2.Top) < aRowsSpacing;
end;

function RectIsOnSameRow(const aRect1, aRect2: TPdfRect; const aTolerance : double): boolean;
begin
  Result := Abs((aRect1.Bottom + ((aRect1.Top - aRect1.Bottom) / 2)) - (aRect2.Bottom + ((aRect2.Top - aRect2.Bottom) / 2))) <= aTolerance;
  if not Result then
    Result := ((aRect1.Bottom >= aRect2.Bottom) and (aRect1.Top <= aRect2.Top)) or ((aRect2.Bottom >= aRect1.Bottom) and (aRect2.Top <= aRect1.Top));
end;

function RectIsOnSameColumn(const aRect1, aRect2: TPdfRect; const aTextAlignment: TAlignment; const aTolerance: double): boolean;
begin
  if aTextAlignment = taLeftJustify then
  begin
    Result := Abs(aRect1.Left - aRect2.Left) <= aTolerance;
  end
  else if aTextAlignment = taRightJustify then
  begin
    Result := Abs(aRect1.Right - aRect2.Right) <= aTolerance;
  end
  else // taCenter;
  begin
    Result := Abs((aRect1.Right + ((aRect1.Right - aRect1.Left) / 2) - (aRect2.Right + (aRect2.Right - aRect2.Left) / 2))) <= aTolerance;
    if not Result  then
      Result := ((aRect1.Right >= aRect2.Right) and (aRect1.Left <= aRect2.Left)) or ((aRect2.Right >= aRect1.Right) and (aRect2.Left <= aRect1.Left));
  end;
end;

function RectIsFollowingOnTheSameRow(const aRect1, aRect2: TPdfRect; const aSpaceWidth: double): boolean;
begin
  Result := RectIsOnSameRow(aRect1, aRect2) and ((aRect2.Left - aRect1.Right) <= aSpaceWidth);
end;

end.
