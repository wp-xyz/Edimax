unit emUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_DOM, inifiles, emGlobal;

function Edi_BuildXML(ARequest: TEdiRequest; AHistoryFrom: TDateTime = 0;
  AHistoryTo: TDateTime = 0): String;

function Edi_IsAvail(UseCache: Boolean): Boolean;
function Edi_FindParams(ABroadcastIP: String; var ADeviceParams: TDeviceParams): Boolean;
function Edi_GetState: TEdiState;
function Edi_Measure(AData: TEdiData): TMeasValues;
function Edi_ON: Boolean;
function Edi_OFF: Boolean;
function Edi_Send(AStream: TStream; ARequest: TEdiRequest;
  AStartDate: TDateTime = 0; AEnddate: TDateTime = 0): Boolean;
function Edi_SetState(AState: TEdiState): TEdiState;

function XML_GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
function XML_GetEdiValue(ARootNode: TDOMNode; ANodeName: String): String;
function XML_IsValidEdiXML(ARootNode: TDOMNode): Boolean;

function CreateIni: TCustomIniFile;
function GetBackupDir: String;
function GetBroadcastAddr(const ANetworkAddr, ASubnetMask: String): String;
function MacAddrToString(AMacAddr: TMacAddress): String;

function  ConvertTimeUnits(AValue: TDateTime; AOldUnits, ANewUnits: TTimeUnits): TDateTime;
procedure GetMeasQuantNameAndUnits(const s: String; var AQuantName, AUnits: String);
function  StrToTimeUnits(const s: String): TTimeUnits;

function  EdiFloatToStr(ANumFormat: String; AValue: Double): String;


implementation

uses
  Math, laz2_XMLRead, Process, 
  httpsend, synautil, synaip, blcksock;

var
  EDI_AVAIL: Boolean = false;

//https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-macos-xlinux-do-a-network-ping-from-within-your-appliccations/
function Ping(URL: String; ATimeout: Integer): Boolean;
var
  msg: String;
  time_out: String;
  ping_count: String;
begin
  ping_count := '2';
  time_out := IntToStr(ATimeOut);
  
 {$IFDEF MSWINDOWS}
  Result := RunCommand('ping.exe', ['-n', ping_count, '-w', time_out, url], msg, [], swoHide);
 {$ENDIF}

 {$IFDEF DARWIN}
  Result := RunCommand('/sbin/ping', ['-c', ping_count, '-t', time_out, '-q', url], msg);  
 {$ENDIF}
 
 {$IFDEF LINUX}
  Result := RunCommand('/bin/ping', ['-c', ping_count, '-w', time_out, '-q', url], msg);
 {$ENDIF}
end;

(*     // does not work in Linux...
function Ping(URL: String; ATimeout: Integer): Boolean;
var
  p: Integer;
begin
  p := pos('http://', url);
  if p <> 0 then Delete(url, 1, length('http://'));
  p := pos('/', url);
  if p > 0 then
    Delete(url, p-1, MaxInt);
  p := pos('@', url);
  if p > 0 then
    Delete(url, 1, p);
  p := pos(':', url);
  if p > 0 then
    Delete(url, p, MaxInt);

  with TPINGSend.Create do
    try
      Timeout := ATimeout;
      Result := Ping(URL);
     finally
       Free;
     end;
end;

function HttpPostURL(const URL, URLData: string; const Data: TStream;
  TimeOut: Integer = -1): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    if TimeOut > -1 then
      HTTP.ConnectionTimeOut := TimeOut;
    WriteStrToStream(HTTP.Document, URLData);
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    Result := HTTP.HTTPMethod('POST', URL);
    if Result then
      Data.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
end;       *)

function Edi_IsAvail(UseCache: Boolean): Boolean;
begin
  if UseCache then
    Result := EDI_AVAIL
  else begin
    Result := Ping(DeviceParams.IPAddress, 100);
    EDI_AVAIL := Result;
  end;
end;

function Edi_BuildXML(ARequest: TEdiRequest;
  AHistoryFrom: TDateTime = 0; AHistoryTo: TDateTime = 0): String;
var
  cmd: String;
begin
  case ARequest of
    erOn:
      cmd := '<CMD id="setup"><Device.System.Power.State>ON</Device.System.Power.State></CMD>';
    erOff:
      cmd := '<CMD id="setup"><Device.System.Power.State>OFF</Device.System.Power.State></CMD>';
    erStatus:
      cmd := '<CMD id="get"><Device.System.Power.State>OFF</Device.System.Power.State></CMD>';
    erPower:
      cmd := '<CMD id="get"><NOW_POWER><Device.System.Power.NowPower/></NOW_POWER></CMD>';
    erCurrent:
      cmd := '<CMD id="get"><NOW_POWER><Device.System.Power.NowCurrent/></NOW_POWER></CMD>';
    erPowerCurrent:
      cmd := '<CMD id="get"><NOW_POWER><Device.System.Power.NowCurrent/><Device.System.Power.NowPower/></NOW_POWER></CMD>';
    erHistory:
      cmd := Format('<CMD id="get"><NOW_POWER></NOW_POWER>'+
               '<POWER_HISTORY>'+
                 '<Device.System.Power.History.Energy unit="HOUR" date="%s-%s">'+
                 '</Device.System.Power.History.Energy>'+
               '</POWER_HISTORY>'+
             '</CMD>', 
             [
               FormatDateTime('yyyymmdd00', AHistoryFrom),
               FormatDateTime('yyyymmddhh', AHistoryTo)
             ]);
  end;
  Result := '<?xml version="1.0" encoding="utf-8"?>'+
    '<SMARTPLUG id="edimax">' +
      cmd +
    '</SMARTPLUG>';
end;

{ In order to find the Smartplugs within the network the Android app sends a
  UDP datagram broadcast. The payload (sniffed by WireShark) looks like this

  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x45, 0x44, 0x49, 0x4d, 0x41,
  0x58, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa1, 0xff, 0x5e

  Then the identified Smartplugs respond with a 186 byte message. This message
  contains
    at offset  6: the word "EDIMAX"
    at offset 22: the model name
    at offset 36: the software version
    at offset 44: the name given to the smartplug. 
    
  See also: 
    https://blog.guntram.de/?p=45 }
function Edi_FindParams(ABroadcastIP: String; var ADeviceParams: TDeviceParams): Boolean;
type
  TDataRec = packed record
    MacAddr: TMacAddress;  // array[0..5] of byte;
    Manufacturer: packed array[0..11] of ansichar;
    Unknown: LongInt;
    Model: packed array[0..13] of ansiChar;
    Version: packed array[0..7] of ansichar;
    DisplayName: packed array[0..127] of ansichar;
    Port: Word;
    IPAddr: packed array[0..3] of byte;
    Unknown2: LongInt;
    DstAddr: DWord;
  end;
var
  socket: TUDPBlockSocket;
  DataRec: TDataRec;
const
  PAYLOAD = #$FF#$FF#$FF#$FF#$FF#$FF#$45#$44#$49#$4D#$41#$58#$00#$00#$00#$00#$00#$00#$00#$A1#$FF#$5E;
  TIMEOUT = 500;
  PORT = '20560';
begin
  Result := false;
  socket := TUDPBlockSocket.Create;
  try
    socket.EnableBroadcast(true);    
    socket.Connect(ABroadcastIP, PORT);
    socket.SendBuffer(@PAYLOAD[1], Length(PAYLOAD));
    if socket.LastError <> 0 then
      exit;
    socket.Listen;
    socket.RecvBufferEx(@dataRec, SizeOf(DataRec), TIMEOUT);
    if socket.LastError <> 0 then
      exit;
    ADeviceParams.IPAddress := Format('%d.%d.%d.%d', [
      DataRec.IpAddr[0], DataRec.IpAddr[1], DataRec.IpAddr[2], DataRec.IpAddr[3]
    ]);
    ADeviceParams.DisplayName := DataRec.DisplayName;
    ADeviceParams.Port := DataRec.Port;
    ADeviceParams.Model := DataRec.Model;
    ADeviceParams.SoftwareVersion := DataRec.Version;
    ADeviceParams.Manufacturer := DataRec.Manufacturer;
    ADeviceParams.MacAddress := MacAddrToString(DataRec.MacAddr);
    Result := True;
  finally
    socket.Free;
  end;
end;

function Edi_GetState: TEdiState;
var
  stream: TMemoryStream;
  doc: TXMLDocument;
  oK: Boolean;
  s: String;
begin
  Result := esUnknown;

  doc := nil;
  stream := TMemoryStream.Create;
  try
    ok := Edi_Send(stream, erStatus);
    if not ok then
      exit;

    stream.Position := 0;
    ReadXMLFile(Doc, stream);
    if not XML_IsValidEdiXML(Doc.DocumentElement) then
      exit;
    s := XML_GetEdiValue(Doc.DocumentElement, 'Device.System.Power.State');
    if s = 'ON' then
      Result := esON
    else
    if s = 'OFF' then
      Result := esOFF;
  finally
    stream.Free;
    doc.Free;
  end;
end;

function Edi_Measure(AData: TEdiData): TMeasValues;
var
  s: String;
  stream: TMemoryStream;
  doc: TXMLDocument;
  topnode: TDomNode;
  ok: Boolean;
begin
  Result[mqPower] := NaN;
  Result[mqCurrent] := NaN;
  Result[mqEnergy] := NaN;

  doc := nil;
  stream := TMemoryStream.Create;
  try
    ok := Edi_Send(stream, erPowerCurrent);
    if not ok then exit;

    stream.Position := 0;
    ReadXMLFile(Doc, stream);

    topnode := Doc.DocumentElement; //.FirstChild;
    if not XML_IsValidEdiXML(topnode) then
      exit;

    // Read value from node
    if (AData in [edPower, edPowerAndCurrent]) then
    begin
      s := XML_GetEdiValue(topnode, 'Device.System.Power.NowPower');
      Result[mqPower] := StrToFloat(s, PointFormatSettings);
    end;
    if (AData in [edCurrent, edPowerAndCurrent]) then
    begin
      s := XML_GetEdiValue(topnode, 'Device.System.Power.NowCurrent');
      Result[mqCurrent] := StrToFloat(s, PointFormatSettings);
    end;
  finally
    stream.Free;
    doc.Free;
  end;
end;

function Edi_ON: Boolean;
begin
  Result := Edi_SetState(esON) = esON;
end;

function Edi_OFF: Boolean;
begin
  Result := Edi_SetState(esOFF) = esOFF;
end;

function Edi_SetState(AState: TEdiState): TEdiState;
var
  stream: TMemoryStream;
  doc: TXMLDocument;
  oK: Boolean;
  s: String;
begin
  Result := esUnknown;

  doc := nil;
  stream := TMemoryStream.Create;
  try
    if AState = esON then
      ok := Edi_Send(stream, erON)
    else if AState = esOFF then
      ok := Edi_Send(stream, erOFF)
    else
      raise Exception.Create('Edi_SetState: Can be called only for state esON or esOFF.');
    if not ok then
      exit;

    stream.Position := 0;
    ReadXMLFile(Doc, stream);
    if not XML_IsValidEdiXML(Doc.DocumentElement) then
      exit;
    s := Doc.DocumentElement.FirstChild.FirstChild.NodeValue;
    if s = 'OK' then
      Result := AState;
  finally
    stream.Free;
    if doc <> nil then doc.Free;
  end;
end;

function Edi_Send(AStream: TStream; ARequest: TEdiRequest;
  AStartDate: TDateTime = 0; AEnddate: TDateTime = 0): Boolean;
var
  xml: String;
  url: String;
  usr, pwd: String;
begin
  Result := false;
  if not Edi_IsAvail(true) then
    exit;

  if (ARequest = erHistory) and (AStartDate = 0) or (AEndDate = 0) then
  begin
    AStartDate := Now - 14;
    AEndDate := Now;
  end;
  if (AuthParams.UserName = '') and (AuthParams.Password = '') then begin
    usr := DefaultAuthParams.Username;
    pwd := DefaultAuthParams.Password;
  end else begin
    usr := AuthParams.UserName;
    pwd := AuthParams.Password;
  end;
  url := Format(EDI_URL_MASK, [
    usr,
    pwd,
    DeviceParams.IPAddress,
    DeviceParams.Port
  ]);
  xml := Edi_BuildXML(ARequest, AStartDate, AEndDate);

  Result := HttpPostURL(url, xml, AStream);
end;


{ XML Utilities }

{ Gets value for the specified attribute of the given node.
  Returns empty string if attribute is not found. }
function XML_GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
var
  i: LongWord;
  Found: Boolean;
begin
  Result := '';
  if (ANode = nil) or (ANode.Attributes = nil) then
    exit;

  Found := false;
  i := 0;
  while not Found and (i < ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName = AAttrName then begin
      Found := true;
      Result := ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;

function XML_GetEdiValue(ARootNode: TDOMNode; ANodeName: String): String;

  function FindNode(ANode: TDOMNode): TDOMNode;
  var
    node: TDOMNode;
    nodeName: String;
  begin
    Result := ANode.FirstChild;
    while Result <> nil do begin
      nodeName := Result.NodeName;
      if nodeName = ANodeName then
        exit;
      if Result.HasChildNodes then begin
        node := FindNode(Result);
        if node <> nil then begin
          Result := node;
          exit;
        end;
      end;
      Result := Result.NextSibling;
    end;
  end;

var
  node: TDOMNode;
begin
  node := FindNode(ARootNode);
  if node <> nil then
  begin
    node := node.FirstChild;
    if node <> nil then Result := node.NodeValue;
  end;
end;

function XML_IsValidEdiXML(ARootNode: TDOMNode): Boolean;
var
  nodeName: String;
  s: String;
begin
  Result := False;
  if ARootNode = nil then
    exit;
  nodeName := ARootNode.NodeName;
  if nodeName <> 'SMARTPLUG' then
    exit;
  if not ARootNode.HasAttributes then
    exit;
  s := XML_GetAttrValue(ARootNode, 'id');
  if s <> 'edimax' then
    exit;
  Result := true;
end;

function CreateIni: TCustomIniFile;
var
  cfgDir : string;
begin
  cfgDir := GetAppConfigDir(false);
  if not DirectoryExists(cfgDir) then
    CreateDir(cfgDir);
  result := TMemIniFile.Create(cfgDir + 'edimax.cfg');
end;

function GetBackupDir: String;
begin
  Result := GetAppConfigDir(false);
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

function GetBroadcastAddr(const ANetworkAddr, ASubnetMask: String): String;
var
  dhcpAddr: DWord;
  subnetMaskAddr: DWord;
  ip: DWord;
begin
  dhcpAddr := DWord(StrToIP(ANetworkAddr));
  subnetMaskAddr := DWord(StrToIP(ASubnetMask));
  ip := (dhcpAddr and subnetMaskAddr) or (not subnetMaskAddr);
  Result := IPToStr(Integer(ip));
end;

function MacAddrToString(AMacAddr: TMacAddress): String;
var
  p: PByte;
  i, n: Integer;
begin
  Result := '';
  n := SizeOf(AMacAddr);
  p := PByte(AMacAddr);
  for i := 0 to n - 1 do
  begin
    Result := Result + IntToHex(p^, 2) + ':';
    Inc(p);
  end;
  SetLength(Result, Length(Result) - 1);
end;

function ConvertTimeUnits(AValue: TDateTime; AOldUnits, ANewUnits: TTimeUnits): TDateTime;
const
  SECONDS_PER_DAY = 24*60*60;
  MINUTES_PER_DAY = 24*60;
  HOURS_PER_DAY = 24;
var
  value_days: TDateTime;
begin
  if AOldUnits = ANewUnits then
  begin
    Result := AValue;
    exit;
  end;
  
  // Convert old units to "days"
  case AOldUnits of
    tuSeconds:
      value_days := AValue / SECONDS_PER_DAY;
    tuMinutes:
      value_days := AValue / MINUTES_PER_DAY;
    tuHours:
      value_days := AValue / HOURS_PER_DAY;
    else
      value_days := AValue;
  end;
  
  // Convert "days" to new units
  case ANewUnits of
    tuSeconds:
      Result := value_days * SECONDS_PER_DAY;
    tuMinutes:
      Result := value_days * MINUTES_PER_DAY;
    tuHours:
      Result := value_days * HOURS_PER_DAY;
    else
      Result := value_days;
  end;
end;

function StrToTimeUnits(const s: String): TTimeUnits;
begin
  case Lowercase(s) of
    's', 'sec', 'secs', 'seconds':
      Result := tuSeconds;
    'm', 'min', 'mins', 'minutes':
      Result := tuMinutes;
    'h', 'hr', 'hrs', 'hours':
      Result := tuHours;
    'd', 'days':
      Result := tuDays;
    else
      raise Exception.Create('Unknown time units identifier.');
  end;
end;

procedure GetMeasQuantNameAndUnits(const s: String; var AQuantName, AUnits: String);
var
  p: Integer;
begin
  p := pos('(', s);
  if p > 0 then
  begin
    AQuantName := trim(Copy(s, 1, p-1));
    AUnits := trim(Copy(s, p+1));
    if (AUnits <> '') and (AUnits[Length(AUnits)] = ')') then
      Delete(AUnits, Length(AUnits), 1);
    exit;
  end;
  
  p := pos('[', s);
  if p > 0 then
  begin
    AQuantName := trim(Copy(s, 1, p-1));
    AUnits := trim(Copy(s, p+1));
    if (AUnits <> '') and (AUnits[Length(AUnits)] = ']') then
      Delete(AUnits, Length(AUnits), 1);
    exit;
  end;  
  
  p := pos(',', s);
  if p > 0 then
  begin
    AQuantName := trim(Copy(s, 1, p-1));
    AUnits := trim(Copy(s, p+1));
    exit;
  end;

  AQuantName := s;
  AUnits := '';
end;

function EdiFloatToStr(ANumFormat: String; AValue: Double): String;
begin
  if IsNaN(AValue) then
    Result := ''
  else
    Result := FormatFloat(ANumFormat, AValue);
end;

end.

