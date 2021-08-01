unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, fphttpclient, Process;

type

  { TLauncherForm }

  TLauncherForm = class(TForm)
    Button1: TButton;
    ButtonCancel: TButton;
    Message: TLabel;
    ProgressBar: TProgressBar;
    Timer: TTimer;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    function DownloadNodeWebkit(AFrom, ATo: String): Boolean;
    procedure RunNodeWebkit();
    procedure DataReceived(Sender : TObject; const ContentLength, CurrentPos : Int64);
  public

  end;

var
  LauncherForm: TLauncherForm;
  HTTPClient: TFPHTTPClient;

implementation

{$R *.lfm}

{ TLauncherForm }

procedure TLauncherForm.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  Application.ProcessMessages;

  ProgressBar.Max := ContentLength;
  ProgressBar.Position := CurrentPos;
end;

procedure TLauncherForm.RunNodeWebkit();
var
  FilePath: String;
  AProcess: TProcess;

begin
  FilePath := ExtractFilePath(ParamStr(0));

  if FileExists(FilePath + '/node-webkit') then
    begin
      AProcess := TProcess.Create(nil);
      try
        AProcess.ShowWindow  := swoShowNormal;
        AProcess.CommandLine := FilePath + '/node-webkit';
        AProcess.Execute;
      finally
        AProcess.Free;
    end;
  end;

  Application.Terminate;
end;

function TLauncherForm.DownloadNodeWebkit(AFrom, ATo: String): Boolean;
begin
  Result := False;
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.OnDataReceived := @DataReceived;
    HTTPClient.AllowRedirect := True;
    HTTPClient.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    try
      HTTPClient.Get(AFrom, ATo);
      Result := True;
    except
      on E: Exception do
        ShowMessage(E.Message)
    end;
  finally
     HTTPClient.Free;
  end;
end;

procedure TLauncherForm.FormCreate(Sender: TObject);
begin
  //{$IFFDEF UNIX}
    {$IFDEF DARWIN}
      if not FileExists('/Library/nwjs/0.54.1-sdk/nwjs.app/Contents/MacOS/nwjs') then
        begin
          Timer.Enabled := True;
          LauncherForm.Visible := True;
        end
      else
        begin
          RunNodeWebkit();
        end
    // {ELSE}
    {$ENDIF}
  //{$ENDIF}
end;

procedure TLauncherForm.TimerTimer(Sender: TObject);
var
  AProcess: TProcess;
  Result : AnsiString;

begin
  Application.ProcessMessages;
  Timer.Enabled := False;

  //{$IFFDEF UNIX}
    {$IFDEF DARWIN}
      if DownloadNodeWebkit('http://downloads.oleksandrsovenko.com/nwjs/macos/nwjs-sdk-v0.54.1-osx-x64.pkg', '/tmp/nwjs-sdk-v0.54.1-osx-x64.pkg') then
        begin
          if not FileExists('/Library/nwjs/0.54.1-sdk/nwjs.app/Contents/MacOS/nwjs') then
            RunCommand('open', ['/tmp/nwjs-sdk-v0.54.1-osx-x64.pkg'], Result);

          RunNodeWebkit();
        end;
    // {ELSE}
    {$ENDIF}
  //{$ENDIF}
end;

procedure TLauncherForm.ButtonCancelClick(Sender: TObject);
begin
  HTTPClient.Terminate;
  Application.Terminate;
end;

procedure TLauncherForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  HTTPClient.Terminate;
  Application.Terminate;
end;


end.

