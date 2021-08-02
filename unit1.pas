unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, fphttpclient, Process, Registry;

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
    {$IFDEF MSWINDOWS}
    function GetPathNodeWebkit: String;
    {$ENDIF}
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

{$IFDEF MSWINDOWS}
function TLauncherForm.GetPathNodeWebkit: String;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg    := TRegistry.Create;

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly('SOFTWARE\StimiInc\nwjs-sdk-v0.54.1-win-x64') then
      begin
       if Reg.ValueExists('Path') then
         begin
           Result := Reg.ReadString('Path');
         end
      end;
  finally
    Reg.Free;
  end;
end;
{$ENDIF}

procedure TLauncherForm.DataReceived(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  Application.ProcessMessages;

  ProgressBar.Max := ContentLength;
  ProgressBar.Position := CurrentPos;
end;

procedure TLauncherForm.RunNodeWebkit();
var
  AProcess  : TProcess;
  NodeWebkit: String;

begin
  Application.ProcessMessages;

  {$IFDEF MSWINDOWS}
  NodeWebkit := GetPathNodeWebkit() + ' ' + ExtractFilePath(Application.ExeName)
  {$ENDIF}

  {$IFDEF DARWIN}
  NodeWebkit := ExtractFilePath(Application.ExeName) + '/node-webkit';
  if not FileExists(NodeWebkit) then
    NodeWebkit := '';
  {$ENDIF}

  if not (NodeWebkit = '') then
    begin
      AProcess := TProcess.Create(nil);
      try
        AProcess.ShowWindow  := swoShowNormal;
        AProcess.CommandLine := NodeWebkit;
        AProcess.Execute;
      finally
        AProcess.Free;
      end;
    end;

  Application.Terminate;
end;

function TLauncherForm.DownloadNodeWebkit(AFrom, ATo: String): Boolean;
begin
  try
    HTTPClient.OnDataReceived := @DataReceived;
    HTTPClient.AllowRedirect := True;
    HTTPClient.AddHeader('User-Agent', 'Launcher NW.js');
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
  HTTPClient := TFPHTTPClient.Create(nil);

  {$IFDEF MSWINDOWS}
  if GetPathNodeWebkit() = '' then
  {$ENDIF}

  {$IFDEF DARWIN}
  if not FileExists('/Library/nwjs/0.54.1-sdk/nwjs.app/Contents/MacOS/nwjs') then
  {$ENDIF}
    begin
      Timer.Enabled := True;
      LauncherForm.Visible := True;
    end
      else
    begin
      RunNodeWebkit();
    end
end;

procedure TLauncherForm.TimerTimer(Sender: TObject);
var
  Result    : AnsiString;
  TempDir   : String;
  URL       : String;
  NodeWebkit: String;

begin
  Application.ProcessMessages;
  Timer.Enabled := False;
  TempDir := GetTempDir();

  {$IFDEF MSWINDOWS}
  URL := 'http://downloads.oleksandrsovenko.com/nwjs/windows/nwjs-sdk-v0.54.1-win-x64.exe';
  {$ENDIF}

  {$IFDEF DARWIN}
  URL := 'http://downloads.oleksandrsovenko.com/nwjs/macos/nwjs-sdk-v0.54.1-osx-x64.pkg';
  {$ENDIF}

  NodeWebkit := TempDir + '/' + ExtractFileName(URL);

  if DownloadNodeWebkit(URL, NodeWebkit) then
    begin
      if FileExists(NodeWebkit) then
        begin
          {$IFDEF MSWINDOWS}
          RunCommand(NodeWebkit, [], Result);
          {$ENDIF}

          {$IFDEF DARWIN}
          RunCommand('open', [NodeWebkit], Result);
          {$ENDIF}
        end;

      RunNodeWebkit();
    end;
end;

procedure TLauncherForm.ButtonCancelClick(Sender: TObject);
begin
  if HTTPClient <> nil then
    HTTPClient.Terminate;

  Application.Terminate;
end;

procedure TLauncherForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if HTTPClient <> nil then
    HTTPClient.Terminate;

  Application.Terminate;
end;


end.


