// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mDataManagerClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

type

  TmDataManagerTransaction = class abstract
  public
    procedure Connect; virtual; abstract;
    procedure Close; virtual; abstract;

    procedure StartTransaction; virtual; abstract;
    procedure EndTransaction; virtual; abstract;
    procedure Commit; virtual; abstract;
    procedure Rollback; virtual; abstract;
    function Connected: boolean; virtual; abstract;
  end;

implementation

end.
