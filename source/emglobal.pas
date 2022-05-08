unit emGlobal;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  TTimeUnits = (tuSeconds, tuMinutes, tuHours, tuDays);

const
  EDI_URL_MASK = 'http://%s:%s@%s:%d/smartplug.cgi';
                 // <user>:<password>@<url>:<port>/smalrplug.cgi
  TIMEFORMAT: array[TTimeUnits] of string = (
    's.zzz', 'nn:ss', 'h:nn', '%.1f');
  TIMEFORMAT_LONG: array[TTimeUnits] of string = (
    's.zzz', 'nn:ss', 'h:nn:ss', 'd h:nn:ss');
  TIMEUNITS: array[TTimeUnits] of string = (
    'seconds', 'min:sec', 'hours:min', 'days'
  );

  ONE_DAY = 1.0;
  ONE_HOUR = ONE_DAY / 24;
  ONE_MINUTE = ONE_DAY / (24 * 60);
  
  BACKUP_INTERVAL = 1 * ONE_MINUTE;  // minutes
  BACKUP_FILENAME = 'measurement.csv';
  
  TAB = #9;

type
  TEdiRequest = (erOn, erOff, erStatus, erPower, erCurrent, erPowerCurrent, erHistory);
  TEdiState = (esUnknown, esOff, esOn);
  TEdiData = (edPower, edCurrent, edPowerAndCurrent);

  TNetworkParams = record
    NetworkAddress: String;
    SubnetMask: String;
    DeviceAddress: String;
    function BroadcastAddress: String;
  end;

  TAuthenticationParams = record
    UserName: String;
    Password: String;
  end;

  TDeviceParams = record
    DisplayName: String;
    Model: String;
    IPAddress: String;
    Port: Word;
    MacAddress: String;
    SoftwareVersion: String;
    Manufacturer: String;
  end;

type
  TMeasQuant = (mqPower, mqCurrent, mqEnergy, mqNone);
  TMeasValues = array[TMeasQuant] of Double;
  
const
  MEASQUANT_NAMES: array[TMeasQuant] of String = (
    'Power (W)', 'Current (A)', 'Energy (Wh)', ''
  );
  
type  
  TData = record
    Time: TDateTime;      // units: days
    Values: TMeasValues;  // units: Watts, Amperes, Wh
  end;

  TDataArray = array of TData;

  TMacAddress = packed array[0..5] of byte;

var
  NetworkParams: TNetworkParams = (
    NetworkAddress: '192.168.178.1';
    SubnetMask: '255.255.255.0';
    DeviceAddress: '';
  );

const
  EmptyDeviceParams: TDeviceParams = (
    DisplayName: '';
    Model: '';
    IPAddress: '';
    Port: 10000;
    MacAddress: '';
    SoftwareVersion: '';
    Manufacturer: '';
  );
  
  DefaultAuthParams: TAuthenticationParams = (
    UserName: 'admin';
    Password: '1234';
  );
  
var
  DeviceParams: TDeviceParams;
  AuthParams: TAuthenticationParams;

  Voltage: Double = 240;
  PointFormatSettings: TFormatSettings;


implementation

uses
  synaip;

function TNetworkParams.BroadcastAddress: String;
var
  dhcpAddr: DWord;
  subnetMaskAddr: DWord;
  ip: DWord;
begin
  dhcpAddr := DWord(StrToIP(NetworkAddress));
  subnetMaskAddr := DWord(StrToIP(SubnetMask));
  ip := (dhcpAddr and subnetMaskAddr) or (not subnetMaskAddr);
  Result := IPToStr(Integer(ip));
end;
  

initialization
  PointFormatSettings := DefaultFormatSettings;
  PointFormatSettings.DecimalSeparator := '.';

  DeviceParams := EmptyDeviceParams;
  AuthParams := DefaultAuthParams;
  
end.

