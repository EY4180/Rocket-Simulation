program Main;

{$Mode delphi}
{$R *.res}

uses
  Forms,
  Interfaces,
  Data;

var
  MainMenu: TMainMenu;

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Title := 'Rocket Simulation Program';
  Application.Initialize;
  Application.CreateForm(TMainMenu, MainMenu);
  Application.Run;
end.
