// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (mimmo71@gmail.com - www.mammola.net)
unit mGeographicsUtility;


interface

function IsEUCountry(const aCountryCode: String; const aDate : TDate) : boolean;

implementation
uses
  sysutils,
  mMaps;

function IsEUCountry(const aCountryCode: String; const aDate : TDate): boolean;
var
  EUCountries : TmStringDictionary;
begin
  Result := false;

  if aDate >= EncodeDate(2004,5,1) then
  begin
    EUCountries := TmStringDictionary.Create(false);
    try
      EUCountries.Add('AT', EUCountries);
      EUCountries.Add('BE', EUCountries);
      EUCountries.Add('BG', EUCountries);
      EUCountries.Add('CY', EUCountries);
      EUCountries.Add('HR', EUCountries);
      EUCountries.Add('DK', EUCountries);
      EUCountries.Add('EE', EUCountries);
      EUCountries.Add('FI', EUCountries);
      EUCountries.Add('FR', EUCountries);
      EUCountries.Add('DE', EUCountries);
      EUCountries.Add('GR', EUCountries);
      EUCountries.Add('IE', EUCountries);
      EUCountries.Add('IT', EUCountries);
      EUCountries.Add('LV', EUCountries);
      EUCountries.Add('LT', EUCountries);
      EUCountries.Add('LU', EUCountries);
      EUCountries.Add('MT', EUCountries);
      EUCountries.Add('NL', EUCountries);
      EUCountries.Add('PL', EUCountries);
      EUCountries.Add('PT', EUCountries);
      // Both the EU and UK Parliaments ratified the Withdrawal Agreement, which allowed the UK to leave the bloc at 11 pm GMT on 31 January 2020.
      // https://en.wikipedia.org/wiki/United_Kingdom_membership_of_the_European_Union
      if aDate <= EncodeDate(2020, 1, 31) then
        EUCountries.Add('GB', EUCountries);
      EUCountries.Add('CZ', EUCountries);
      EUCountries.Add('RO', EUCountries);
      EUCountries.Add('SK', EUCountries);
      EUCountries.Add('SI', EUCountries);
      EUCountries.Add('ES', EUCountries);
      EUCountries.Add('SE', EUCountries);
      EUCountries.Add('HU', EUCountries);

      Result := EUCountries.Contains(aCountryCode);
    finally
      EUCountries.Free;
    end;
  end
  else
    raise Exception.Create('[IsEUCountry] Missing data for date ' + DateToStr(aDate));
end;

end.
