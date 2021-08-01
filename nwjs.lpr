program nwjs;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, Unit1
  { you can add units after this };

{$R *.res}

{$IFDEF MSWINDOWS}
var
  IconFile: String;
{$ENDIF}

begin
  {$IFDEF MSWINDOWS}
  IconFile := ExtractFilePath(Application.ExeName) + '\app.ico';
  if FileExists(IconFile) then
    Application.Icon.LoadFromFile(IconFile);
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.ShowMainForm := False;
  Application.Initialize;
  Application.CreateForm(TLauncherForm, LauncherForm);
  Application.Run;
end.

