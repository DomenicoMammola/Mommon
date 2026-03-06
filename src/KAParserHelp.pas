// This is part of the Mommon Library
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// This software is distributed without any warranty.
//
// @author Domenico Mammola (svil@mammola.net)
unit KAParserHelp;


{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  KAParser;

type

  TKAParserSintaxTokenHelp = record
    category : String;
    sintax : string;
    numOfParameters : string;
    description : string;
  end;

  { TKAParserEditorHelp }

  TKAParserEditorHelp = class
  public
    const CATEGORY_CONSTANT = 'constant';
    const CATEGORY_MATHEMATICAL = 'matematical';
    const CATEGORY_LOGICAL = 'logical';
    const CATEGORY_TEXT = 'text';
    const CATEGORY_GENERAL = 'general';
    const CATEGORY_DATETIME = 'datetime';
  public
    class function GetHelp(const aToken : String): TKAParserSintaxTokenHelp;
  end;

implementation

{ TKAParserEditorHelp }

class function TKAParserEditorHelp.GetHelp(const aToken: String): TKAParserSintaxTokenHelp;
var
  tmp : TKAParserSintaxTokenHelp;
begin
  tmp.category:= '';
  tmp.description:= '';
  tmp.sintax := aToken;
  if aToken = mpConst_now then
  begin
    tmp.category := CATEGORY_CONSTANT;
    tmp.numOfParameters:= '0';
    tmp.description := 'Returns current date/time';
  end
  else if aToken = mpConst_today then
  begin
    tmp.category := CATEGORY_CONSTANT;
    tmp.numOfParameters:= '0';
    tmp.description := 'Returns current date';
  end
  else if aToken = mpConst_true then
  begin
    tmp.category := CATEGORY_CONSTANT;
    tmp.numOfParameters:= '0';
    tmp.description := 'Returns value TRUE';
  end
  else if aToken = mpConst_false then
  begin
    tmp.category := CATEGORY_CONSTANT;
    tmp.numOfParameters:= '0';
    tmp.description := 'Returns value FALSE';
  end
  else if aToken = mpConst_pi then
  begin
    tmp.category := CATEGORY_CONSTANT;
    tmp.numOfParameters:= '0';
    tmp.description := 'Returns value PI';
  end
  else if aToken = mpFunction_trunc then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the integer part of a double';
  end
  else if aToken = mpFunction_sin then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Calculates the sin of the argument';
  end
  else if aToken = mpFunction_cos then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Calculates the cosine of the argument';
  end
  else if aToken = mpFunction_tan then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Calculates the tangent of the argument';
  end
  else if aToken = mpFunction_frac then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the fractional part of the argument';
  end
  else if aToken = mpFunction_int then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the integer part of the argument';
  end
  else if aToken = mpFunction_sqrt then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Calculates the square root of the argument';
  end
  else if aToken = mpFunction_if then
  begin
    tmp.category := CATEGORY_LOGICAL;
    tmp.numOfParameters:= '3';
    tmp.description := 'if the condition (first argument) is true returns the second argument otherwise the third';
  end
  else if aToken = mpFunction_empty then
  begin
    tmp.category := CATEGORY_GENERAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns 1 if the argument has null value otherwise 0';
  end
  else if aToken = mpFunction_len then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the length in chars of the argument';
  end
  else if aToken = mpFunction_and then
  begin
    tmp.category := CATEGORY_LOGICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Computes a logical AND between the arguments';
  end
  else if aToken = mpFunction_or then
  begin
    tmp.category := CATEGORY_LOGICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Computes a logical OR between the arguments';
  end
  else if aToken = mpFunction_safediv then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2';
    tmp.description := 'Divides the first argument by the second but returns 0 if the second is 0';
  end
  else if aToken = mpFunction_between then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '3';
    tmp.description := 'Checks if the value of the first argument is between the second argument and the third';
  end
  else if aToken = mpFunction_concatenate then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Concatenates the string values of the arguments';
  end
  else if aToken = mpFunction_concat then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Concatenates the string values of the arguments';
  end
  else if aToken = mpFunction_repl then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '3';
    tmp.description := 'Replicates the second argument a number of times as the third argument and appends to the first argument';
  end
  else if aToken = mpFunction_left then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2';
    tmp.description := 'Returns a number of chars from the first argument equal to the second argument starting from left';
  end
  else if aToken = mpFunction_right then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2';
    tmp.description := 'Returns a number of chars from the first argument equal to the second argument starting from right';
  end
  else if aToken = mpFunction_substr then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '3';
    tmp.description := 'Extracts a substring from the first argument starting from the position equal to the second argument to the position equal to the third argument';
  end
  else if aToken = mpFunction_tostr then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Converts a numeric value to its string representation';
  end
  else if aToken = mpFunction_pos then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2';
    tmp.description := 'Finds the position of the second argument in the first argument';
  end
  else if aToken = mpFunction_trim then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Removes starting and ending spaces';
  end
  else if aToken = mpFunction_ltrim then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Removes starting spaces';
  end
  else if aToken = mpFunction_rtrim then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Removes ending spaces';
  end
  else if aToken = mpFunction_uppercase then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Converts to uppercase';
  end
  else if aToken = mpFunction_lowercase then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '1';
    tmp.description := 'Converts to lowercase';
  end
  else if aToken = mpFunction_compare then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2';
    tmp.description := 'Returns 0 if the string arguments are equal, 1 if the first argument is major than the second, -1 otherwise (case sensitive)';
  end
  else if aToken = mpFunction_comparestr then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2';
    tmp.description := 'Returns 0 if the string arguments are equal, 1 if the first argument is major than the second, -1 otherwise (case sensitive)';
  end
  else if aToken = mpFunction_comparetext then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '2';
    tmp.description := 'Returns 0 if the string arguments are equal, 1 if the first argument is major than the second, -1 otherwise  (case unsensitive)';
  end
  else if aToken = mpFunction_replacestr then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '3';
    tmp.description := 'Replaces in the first argument all the occurances of the second argument (case sensitive)';
  end
  else if aToken = mpFunction_replacetext then
  begin
    tmp.category := CATEGORY_TEXT;
    tmp.numOfParameters:= '3';
    tmp.description := 'Replaces in the first argument all the occurances of the second argument (case unsensitive)';
  end
  else if aToken = mpFunction_round then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2';
    tmp.description := 'Rounds to a double value with a specific number of decimal digits';
  end
  else if aToken = mpFunction_ceil then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the lowest integer number greater than or equal to the argument';
  end
  else if aToken = mpFunction_floor then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the largest integer smaller than or equal to the argument';
  end
  else if aToken = mpFunction_not then
  begin
    tmp.category := CATEGORY_LOGICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Logical not';
  end
  else if aToken = mpFunction_sum then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Returns the sum of all the arguments';
  end
  else if aToken = mpFunction_max then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Returns the max value of all the arguments';
  end
  else if aToken = mpFunction_min then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Returns the min value of all the arguments';
  end
  else if aToken = mpFunction_avg then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Returns the average value of all the arguments';
  end
  else if aToken = mpFunction_count then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '2+';
    tmp.description := 'Returns number of arguments';
  end
  else if aToken = mpFunction_now then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the distance between the argument and the current datetime (1 = 1 day)';
  end
  else if aToken = mpFunction_getday then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the day of a date';
  end
  else if aToken = mpFunction_getweek then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the week of a date';
  end
  else if aToken = mpFunction_getmonth then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the month of a date';
  end
  else if aToken = mpFunction_getyear then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the year of a date';
  end
  else if aToken = mpFunction_startoftheweek then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the start of the week of the date';
  end
  else if aToken = mpFunction_startofthemonth then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the start of the month of the date';
  end
  else if aToken = mpFunction_endofthemonth then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the end of the month of the date';
  end
  else if aToken = mpFunction_todate then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '3';
    tmp.description := 'Convert the arguments to a date (year, month, day)';
  end
  else if aToken = mpFunction_todatetime then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '6';
    tmp.description := 'Converts the arguments to a date (year, month, day, hours, minutes, seconds)';
  end
  else if aToken = mpFunction_today then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Returns the distance between the argument and the current date (1 = 1 day)';
  end
  else if aToken = mpFunction_stringtodatetime then
  begin
    tmp.category := CATEGORY_DATETIME;
    tmp.numOfParameters:= '1';
    tmp.description := 'Converts a string to a datetime value';
  end
  else if aToken = mpFunction_todouble then
  begin
    tmp.category := CATEGORY_MATHEMATICAL;
    tmp.numOfParameters:= '1';
    tmp.description := 'Converts a string to a numeric value';
  end;
  Result := tmp;
end;

end.
