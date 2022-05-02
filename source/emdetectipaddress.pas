unit emDetectIPAddress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls,
  emGlobal;

type
  
  { TDetectIPForm }

  TDetectIPForm = class(TForm)
    BtnDetect: TButton;
    ButtonPanel: TButtonPanel;
    EdNetworkAddress: TEdit;
    EdSubnetMask: TEdit;
    InfoIPAddress: TLabel;
    LblIPAddress: TLabel;
    LblNetworkIPAddress: TLabel;
    LblSubnetMask: TLabel;
    MainPanel: TPanel;
    procedure BtnDetectClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean;

  public
    procedure GuiToParams(var ANetworkParams: TNetworkParams);
    procedure ParamsToGui(const ANetworkParams: TNetworkParams);

  end;

var
  DetectIPForm: TDetectIPForm;

  
implementation

{$R *.lfm}

uses
  synaip,
  emUtils;

procedure TDetectIPForm.BtnDetectClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
  broadcastAddr: String;
  devParams: TDeviceParams;
begin
  if not ValidData(msg, C) then
  begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    exit;
  end;
  
  devParams := EmptyDeviceParams;
  broadcastAddr := GetBroadcastAddr(EdNetworkAddress.Text, EdSubnetMask.Text);
  if Edi_FindParams(broadcastAddr, devParams) then
    InfoIPAddress.Caption := devParams.IPAddress
  else
    InfoIPAddress.Caption := '(not found)';
end;

procedure TDetectIPForm.OKButtonClick(Sender: TObject);
begin
  if not IsIP(InfoIPAddress.Caption) then
  begin
    MessageDlg('IP address not found.', mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TDetectIPForm.GuiToParams(var ANetworkParams: TNetworkParams);
begin
  ANetworkParams.NetworkAddress := EdNetworkAddress.Text;
  ANetworkParams.SubnetMask := EdSubnetMask.Text;
  ANetworkParams.DeviceAddress := InfoIPAddress.Caption;
end;
  
  
procedure TDetectIPForm.ParamsToGui(const ANetworkParams: TNetworkParams);
begin
  EdNetworkAddress.Text := ANetworkParams.NetworkAddress;
  EdSubnetMask.Text := ANetworkParams.SubnetMask;
  InfoIPAddress.Caption := ''; 
end;


function TDetectIPForm.ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
var
  ip: DWord;
  i: Integer;
  mask: DWord;
begin
  Result := false;
  
  if not IsIP(EdNetworkAddress.Text) then
  begin
    AMsg := 'No valid IP address.';
    AControl := EdNetworkAddress;
    exit;
  end;
      
  if not IsIP(EdSubnetMask.Text) then
  begin
    AMsg := 'No valid subnet mask.';
    AControl := EdSubnetMask;
    exit;
  end;
             
  ip := DWord(StrToIP(EdNetworkAddress.Text));
  mask := $FF000000;
  for i:=1 to 4 do
  begin
    if (ip and mask = mask) or (ip and mask = 0) then
    begin
      AMsg := 'No IP address.';
      AControl := EdNetworkAddress;
      exit;
    end;
    mask := mask shr 8;
  end;
  
  Result := true;
end;

end.

