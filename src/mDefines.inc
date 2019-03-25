{$IFDEF FPC}
  {$IFDEF LCL}
    {$DEFINE GUI}
  {$ELSE}
    {$DEFINE NOGUI}
  {$ENDIF}
{$ELSE}
  {$IFDEF CONSOLE}
    {$DEFINE NOGUI}
  {$ELSE}
    {$DEFINE GUI}
  {$ENDIF}
{$ENDIF}
