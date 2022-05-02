unit emSetupIPAddress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, MaskEdit, ExtCtrls, emGlobal;

type

  { TSetupIPForm }

  TSetupIPForm = class(TForm)
    BtnDetectIP: TButton;
    ButtonPanel1: TButtonPanel;
    CbDefaultAuth: TCheckBox;
    EdIPAddress: TEdit;
    EdPassword: TEdit;
    EdUserName: TEdit;
    GroupBox1: TGroupBox;
    GbAuthentication: TGroupBox;
    LblPassword: TLabel;
    LblUserName: TLabel;
    MainPanel: TPanel;
    procedure BtnDetectIPClick(Sender: TObject);
    procedure CbDefaultAuthChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FNetworkParams: TNetworkParams;
    FDeviceParams: TDeviceParams;
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
  public
    { public declarations }
    procedure GuiToParams(var ANetworkParams: TNetworkParams; 
      var AAuthParams: TAuthenticationParams; var ADeviceParams: TDeviceParams);
    procedure ParamsToGui(const ANetworkParams: TNetworkParams; 
      const AAuthParams: TAuthenticationParams; const ADeviceParams: TDeviceParams);
  end;

var
  SetupIPForm: TSetupIPForm;

implementation

{$R *.lfm}

uses
  synaip,
  emDetectIPAddress;

{ TSetupIPForm }

procedure TSetupIPForm.BtnDetectIPClick(Sender: TObject);
var
  F: TDetectIPForm;
begin
  F := TDetectIPForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.ParamsToGui(FNetworkParams);
    if F.ShowModal = mrOK then
    begin
      F.GuiToParams(FNetworkParams);
      EdIPAddress.Text := FNetworkParams.DeviceAddress;
    end;
  finally
    F.Free;
  end;
end;


procedure TSetupIPForm.CbDefaultAuthChange(Sender: TObject);
begin
  EdUserName.Enabled := not CbDefaultAuth.Checked;
  EdPassword.Enabled := not CbDefaultAuth.Checked;
  LblUserName.Enabled := EdUserName.Enabled;
  LblPassword.Enabled := EdPassword.Enabled;
  
  if CbDefaultAuth.Checked then
  begin
    EdUserName.Text := DefaultAuthParams.UserName;
    EdPassword.Text := DefaultAuthParams.Password;
  end;
end;


procedure TSetupIPForm.GuiToParams(var ANetworkParams: TNetworkParams;
  var AAuthParams: TAuthenticationParams; var ADeviceParams: TDeviceParams);
begin
  ANetworkParams := FNetworkParams;
  with ANetworkParams do
    DeviceAddress := EdIPAddress.Text;

  with AAuthParams do
    if CbDefaultAuth.Checked then begin
      UserName := DefaultAuthParams.UserName;
      Password := DefaultAuthParams.Password;
    end else begin
      UserName := EdUserName.Text;
      Password := EdPassword.Text;
    end;
    
  ADeviceParams := FDeviceParams;
  ADeviceParams.IPAddress := FNetworkParams.DeviceAddress;
end;


procedure TSetupIPForm.OKButtonClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
begin
  if not ValidData(msg, C) then
  begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;


procedure TSetupIPForm.ParamsToGui(const ANetworkParams: TNetworkParams;
  const AAuthParams: TAuthenticationParams; const ADeviceParams: TDeviceParams);
begin
  FNetworkParams := ANetworkParams;
  with ANetworkParams do
    EdIPAddress.Text := DeviceAddress;
  
  with AAuthParams do
  begin
    CbDefaultAuth.Checked := 
      ((Username = '') or (Username = DefaultAuthParams.UserName)) and 
      ((Password = '') or (Password = DefaultAuthParams.Password));
    EdUserName.Text := UserName;
    EdPassword.Text := Password;
  end;
  
  FDeviceParams := ADeviceParams;
end;


function TSetupIPForm.ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
begin
  Result := false;

  if not IsIP(EdIPAddress.Text) then
  begin
    AMsg := 'No valid IP address.';
    AControl := EdIPAddress;
    exit;
  end;
  
  Result := true;
end;

end.

