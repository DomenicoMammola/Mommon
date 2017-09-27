unit mProgress;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$interfaces corba}
{$ENDIF}

interface

type

  ImProgress = interface
  ['{CECDFA83-F26F-45FD-9847-BCE6514A5FB5}']
    procedure SetBouncing(AValue: boolean);
    procedure SetCaption(AValue: string);
    procedure SetCurrentStep(AValue: integer);
  end;

implementation

end.
