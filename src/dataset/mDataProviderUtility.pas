// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)

unit mDataProviderUtility;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  mDataProviderInterfaces, mSummary;

procedure RefreshSummaryPanel(aSummaryManager: ISummaryDatasetManager; aSummaryPanel : ISummaryPanel);

implementation

procedure RefreshSummaryPanel(aSummaryManager: ISummaryDatasetManager; aSummaryPanel: ISummaryPanel);
var
  i : integer;
  tempList : TmSummaryScreenValues;
begin
  if Assigned(aSummaryManager) then
  begin
    if Assigned(aSummaryPanel) then
    begin
      if aSummaryManager.GetSummaryValues.Count > 0 then
      begin
        tempList := TmSummaryScreenValues.Create;
        try
          for i := 0 to aSummaryManager.GetSummaryValues.Count - 1 do
          begin
            with tempList.Add do
            begin
              FormattedValue:= aSummaryManager.GetSummaryValues.Get(i).FormattedValue;
              RawValue:= aSummaryManager.GetSummaryValues.Get(i).ValueAsVariant;
              DataType:= aSummaryManager.GetSummaryValues.Get(i).DataType;
              Definition.Assign(aSummaryManager.GetSummaryValues.Get(i).Definition);
            end;
          end;
          aSummaryPanel.SetSummaryValues(tempList);
          aSummaryPanel.Show;
        finally
          tempList.Free;
        end;
      end
      else
      begin
        aSummaryPanel.Hide;
      end;
    end;
  end
  else
    if Assigned(aSummaryPanel) then
      aSummaryPanel.Hide;
end;

end.
