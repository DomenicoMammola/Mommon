// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mFilterOperators;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type
  TmFilterOperator = (foUnknown, foEq, foGtOrEq, foLtOrEq, foGt, foLt, foLike, foNotEq, foStartWith, foEndWith, foIn, foBetween);

  TmFilterOperatorsSet = set of TmFilterOperator;

  function TmFilterOperatorToString (const aValue: TmFilterOperator) : String;

implementation

function TmFilterOperatorToString(const aValue: TmFilterOperator): String;
begin
  Result := '';

  if aValue = foUnknown then
    Result := ''
  else if aValue = foEq then
    Result := '='
  else if aValue = foGtOrEq then
    Result := '>='
  else if aValue = foGt then
    Result := '>'
  else if aValue = foLtOrEq then
    Result := '<='
  else if aValue = foLt then
    Result := '<'
  else if aValue = foLike then
    Result := 'contains'
  else if aValue = foNotEq then
    Result := '<>'
  else if aValue = foStartWith then
    Result := 'starts'
  else if aValue = foEndWith then
    Result := 'ends'
  else if aValue = foIn then
    Result := 'in'
  else if aValue = foBetween then
    Result := 'between';
end;

end.
