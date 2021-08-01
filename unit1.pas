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
  {$IFDEF MSWINDOWS}
  Result: AnsiString;
  {$ENDIF}

  FilePath: String;
  AProcess: TProcess;

begin
  Application.ProcessMessages;

  {$IFDEF MSWINDOWS}
  AProcess := TProcess.Create(nil);
  try
    AProcess.ShowWindow  := swoShowNormal;
    AProcess.CommandLine := GetPathNodeWebkit() + ' ' + ExtractFilePath(Application.ExeName);
    AProcess.Execute;
  finally
   AProcess.Free;
  end;
  {$ENDIF}

  {$IFDEF DARWIN}
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
  {$ENDIF}

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
  AProcess: TProcess;
  Result  : AnsiString;
  TempDir : String;
  URL     : String;
  FileName: String;

begin
  Application.ProcessMessages;
  Timer.Enabled := False;
  TempDir := GetTempDir();

  {$IFDEF MSWINDOWS}
  URL := 'http://downloads.oleksandrsovenko.com/nwjs/windows/nwjs-sdk-v0.54.1-win-x64.exe';
  FileName := TempDir + '/nwjs-sdk-v0.54.1-win-x64.exe';
  {$ENDIF}

  {$IFDEF DARWIN}
  URL := 'http://downloads.oleksandrsovenko.com/nwjs/macos/nwjs-sdk-v0.54.1-osx-x64.pkg';
  FileName := TempDir + '/nwjs-sdk-v0.54.1-osx-x64.pkg';
  {$ENDIF}

  if DownloadNodeWebkit(URL, FileName) then
    begin
      if FileExists(FileName) then
        begin
          {$IFDEF MSWINDOWS}
          RunCommand(FileName, [], Result);
          {$ENDIF}

          {$IFDEF DARWIN}
          RunCommand('open', [FileName], Result);
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

