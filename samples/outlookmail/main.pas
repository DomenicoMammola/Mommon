unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mOutlookMail;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  factory : TmOutlookMailFactory;
begin
  factory := TmOutlookMailFactory.Create;
  try
    factory.SetSubject('Subject 123456789 àèìòù').AddRecipient('pippo@topolinia.it').AddRecipient('paperino@paperopoli.it');
    factory.AddCCRecipient('paperoga@paperopoli.it');
    factory.AddCCRecipient('clarabella@topolina.it');
    factory.AddBCCRecipient('archimede_pitagorico@paperopoli.it');
    factory.AppendBodyLine('This is the body. && àèìòù').CreateMail;
  finally
    factory.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  factory : TmOutlookMailFactory;
begin
  factory := TmOutlookMailFactory.Create;
  try
    factory.SetSubject('Subject 123456789 àèìòù').AddRecipient('pippo@topolinia.it').AddRecipient('paperino@topolinia.it');
    factory.AppendHTMLBodyLine('<h1>Title</h1><br>').AppendHTMLBodyLine('First line').AppendHTMLBodyLine('<i>Signature àèìòù</i>').CreateMail;
  finally
    factory.Free;
  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
var
  factory : TmOutlookMailFactory;
  s : String;
begin
  factory := TmOutlookMailFactory.Create;
  try
    factory.SetSubject('Subject 123456789 àèìòù').AddRecipient('pippo@topolinia.it').AddRecipient('paperino@topolinia.it');
    s := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'gonzo.png';
    factory.AddAttachment(s);
    s := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'kermit.jpg';
    factory.AddAttachment(s);
    factory.AppendBodyLine('This is the body. && àèìòù').CreateMail;
  finally
    factory.Free;
  end;

end;

procedure TForm1.Button4Click(Sender: TObject);
var
  tmp : String;
begin
  if TmOutlookMailFactory.OutlookAvailable(tmp) then
    Label1.Caption:= 'Outlook version ' + tmp
  else
    Label1.Caption:= 'Outlook unavailable';
end;

end.

