unit emMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Grids, Buttons, ActnList, 
  SynEdit, SynHighlighterXML,
  LedNumber, 
  TAGraph, TASeries, TACustomSource, TASources, TAIntervalSources, TAChartListbox, 
  emGlobal;

type

  { TMainForm }

  TMainForm = class(TForm)
    acSetup: TAction;
    acInfo: TAction;
    acStatus: TAction;
    acOn: TAction;
    acOff: TAction;
    acPower: TAction;
    acCurrent: TAction;
    acHistory: TAction;
    acStart: TAction;
    acStop: TAction;
    acAmperes: TAction;
    acLoad: TAction;
    acSave: TAction;
    acExpand: TAction;
    acExit: TAction;
    acWattHours: TAction;
    acWatts: TAction;
    ActionList: TActionList;
    Chart: TChart;
    ChartListbox1: TChartListbox;
    CoolBar: TCoolBar;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    Grid: TDrawGrid;
    Notebook: TNotebook;
    PgMeasurement: TPage;
    PgDebug: TPage;
    StandardToolBar: TToolBar;
    InternalToolbar: TToolBar;
    ToolbarImages: TImageList;
    LED_Images: TImageList;
    LineSeries: TLineSeries;
    OpenDialog: TOpenDialog;
    LeftPanel: TPanel;
    pbStatusLED: TPaintBox;
    Panel2: TPanel;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    RightSplitter: TSplitter;
    LeftSplitter: TSplitter;
    SynEdit: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    Timer: TTimer;
    ChartSource: TUserDefinedChartSource;
    TbSetup: TToolButton;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    tbExpand: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    XMLTree: TTreeView;
    procedure acEdiExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acExpandExecute(Sender: TObject);
    procedure acInfoExecute(Sender: TObject);
    procedure acLoadExecute(Sender: TObject);
    procedure acMeasQuantExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetupExecute(Sender: TObject);
    procedure acStartExecute(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure ChartListbox1ItemClick({%H-}ASender: TObject; AIndex: Integer);
    procedure ChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; ARect: TRect;
      {%H-}AState: TGridDrawState);
    procedure pbStatusLEDPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure StandardToolBarResize(Sender: TObject);

  private
    { private declarations }
    FData: array of TDataArray;
    FChartSources: array of TUserDefinedChartSource;
    FStartTime: TDateTime;
    FTimeUnits: TTimeUnits;
    FEdiState: TEdiState;
    FMeasQuant: TMeasQuant;
    FGridDataIdx: Integer;
    FLedNumber: TLedNumber;
    procedure ExecuteMeasurement;
    procedure LoadData(const AFileName: String);
    procedure SaveDataAs(const AFileName: String);
    procedure SetDebugView(const AEnable: Boolean);

    procedure UpdateCaption;
    procedure UpdateDeviceParams(IP: String = '');
    procedure UpdateStatusDisplay;
    procedure UpdateTimeAxis(ALastTime: TDateTime);
    procedure UpdateValueAxis;
    procedure UpdateValueDisplay(AValue: Double; IsPower: Boolean);

    procedure ReadFromIni;
    procedure WriteToIni;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  StrUtils, Math, IniFiles, LclIntf,
  laz2_DOM, laz2_XMLRead,
  TAChartUtils,
  emUtils, emSetupIPAddress, emXmlUtils;

const
  SERIES_COLORS: array[0..6] of TColor = (
    clBlue, clRed, clGreen, clBlack, clFuchsia, clLime, clGray
  );

  
{ TMainForm }

procedure TMainForm.acEdiExecute(Sender: TObject);
var
  request: TEdiRequest;
  stream: TMemoryStream;
  doc: TXMLDocument;
  value: String;
  topnode: TDOMNode;
  s: String = '';
begin
  if not Edi_IsAvail(true) then
    exit;
  
  Notebook.PageIndex := 1;  // PgDebug

  if Sender = acOn then
    request := erOn
  else if Sender = acOff then
    request := erOff
  else if Sender = acStatus then
    request := erStatus
  else if Sender = acPower then
    request := erPower
  else if Sender = acCurrent then
    request := erCurrent
  else if Sender = acHistory then
    request := erHistory
  else
    exit;

  stream := TMemoryStream.Create;
  try
    if not Edi_Send(stream, request, Date()-14, Date() ) then
      exit;

    stream.Position := 0;
    SetLength(s, stream.Size);
    stream.ReadBuffer(s[1], Length(s));
    BeautifyXml(s, SynEdit.Lines);

    stream.Position := 0;
    doc := nil;
    try
      ReadXMLFile(Doc, stream);
      XML2Tree(Doc, XMLTree);

      topnode := Doc.DocumentElement;
      if not XML_IsValidEdiXML(topnode) then
        exit;

      if request in [erPower, erCurrent] then
      begin
        case request of
          erPower:
            value := XML_GetEdiValue(topnode, 'Device.System.Power.NowPower');
          erCurrent:
            value := XML_GetEdiValue(topnode, 'Device.System.Power.NowCurrent');
        end;
        UpdateValueDisplay(StrToFloat(value, PointFormatSettings), request=erPower);
      end;
      UpdateStatusDisplay;

    finally
      if Assigned(doc) then doc.Free;
    end;
  finally
    stream.Free;
  end;
end;

procedure TMainForm.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acExpandExecute(Sender: TObject);
begin
  SetDebugView(not Coolbar.Bands[1].Visible);
end;


procedure TMainForm.acInfoExecute(Sender: TObject);
var
  s: String;
begin
  UpdateDeviceParams(DeviceParams.IPAddress);
  UpdateCaption;

  s := Format(
    'Displayname: %s' + LineEnding +
    'Model: %s' + LineEnding +
    'Manufacturer: %s' + LineEnding +
    'Software version: %s' + LineEnding +
    'IP address: %s' + LineEnding +
    'Port: %d' + LineEnding +
    'Mac address: %s',
  [
    DeviceParams.DisplayName,
    DeviceParams.Model,
    Deviceparams.Manufacturer,
    DeviceParams.SoftwareVersion,
    DeviceParams.IPAddress,
    DeviceParams.Port,
    DeviceParams.MacAddress
  ]);
  
  MessageDlg(s, mtInformation, [mbOK], 0);
end;


procedure TMainForm.acLoadExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
  begin
    if not SameText(ExtractFileExt(OpenDialog.FileName), OpenDialog.DefaultExt) then
      OpenDialog.FileName := ChangeFileExt(OpenDialog.FileName, OpenDialog.DefaultExt);
    LoadData(OpenDialog.FileName);
  end;
end;


procedure TMainForm.acMeasQuantExecute(Sender: TObject);
var
  i: Integer;
  ser: TLineSeries;
begin
  if acWatts.Checked then
    FMeasQuant := mqPower
  else if acAmperes.Checked then
    FMeasQuant := mqCurrent
  else if acWattHours.Checked then
    FMeasQuant := mqEnergy;

  UpdateValueAxis;
  for i := 0 to Chart.SeriesCount-1 do 
    if Chart.Series[i] is TLineSeries then
    begin
      ser := TLineSeries(Chart.Series[i]);
      if (ser.Source is TUserDefinedChartSource) then
        TUserDefinedChartSource(ser.Source).Reset;
    end;
  Chart.Invalidate;
end;


procedure TMainForm.acSaveExecute(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
  SaveDialog.FileName := '';
  if SaveDialog.Execute then begin
    if not SameText(ExtractFileExt(SaveDialog.FileName), SaveDialog.DefaultExt) then
      SaveDialog.FileName := ChangeFileExt(SaveDialog.Filename, SaveDialog.DefaultExt);
    SaveDataAs(SaveDialog.Filename);
  end;
end;


procedure TMainForm.acSetupExecute(Sender: TObject);
var
  F: TSetupIPForm;
begin
  F := TSetupIPForm.Create(nil);
  try
    F.ParamsToGui(NetworkParams, AuthParams, DeviceParams);
    if F.ShowModal = mrOK then
    begin
      F.GuiToParams(NetworkParams, AuthParams, DeviceParams);
      UpdateCaption;
      UpdateDeviceParams;
      UpdateStatusDisplay;
      Edi_IsAvail(false);
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acStartExecute(Sender: TObject);
begin
  if not Edi_IsAvail(true) then
    exit;
  
  SetDebugView(false);
  Application.ProcessMessages;
  
  SetLength(FData[0], 0);
  FChartSources[0].PointsNumber := 0;
  FChartSources[0].Reset;
  FGridDataIdx := 0;
  Grid.RowCount := 1;

  acStart.Enabled := false;
  acStop.Enabled := true;
  acSave.Enabled := false;
  acLoad.Enabled := false;
 
  FTimeUnits := tuMinutes;
  Chart.BottomAxis.Marks.Source := DateTimeIntervalChartSource;
  Chart.BottomAxis.Marks.Style := smsLabel;
  DateTimeIntervalChartSource.DateTimeFormat := TIMEFORMAT[FTimeUnits];
  
  UpdateValueAxis;
  Grid.Columns[0].Title.Caption := 'Time' + LineEnding + TIMEUNITS[FTimeUnits];
  
  if not edi_ON then begin
    MessageDlg('The SmartPlug cannot be switched on.', mtError, [mbOK], 0);
    exit;
  end;
  FStartTime := Now();
  ExecuteMeasurement;
  
  Timer.Enabled := true;
end;


procedure TMainForm.acStopExecute(Sender: TObject);
begin
  Timer.Enabled := false;
  acStart.Enabled := true;
  acStop.Enabled := false;
  acLoad.Enabled := true;
  acSave.Enabled := true;
end;


procedure TMainForm.ChartListbox1ItemClick(ASender: TObject; AIndex: Integer);
var
  ser: TLineSeries;
begin
  ser := TLineSeries(ChartListbox1.Series[AIndex]);
  if ser = nil then
    exit;
  FGridDataIdx := ser.Source.Tag;
  Grid.RowCount := Length(FData[FGridDataIdx]) + Grid.FixedRows;
end;


procedure TMainForm.ChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  ChartSourceIndex: Integer;
begin
  ChartSourceIndex := ASource.Tag;
  AItem.X := FData[ChartSourceIndex, AIndex].Time;
  AItem.Y := FData[ChartSourceIndex, AIndex].Values[FMeasQuant];
end;


procedure TMainForm.ExecuteMeasurement;
var
  currTime, prevTime: TDateTime;
  values: TMeasValues;
  accumEnergy: Double;
  n: Integer;
begin
  // Get measurement value
  values := Edi_Measure(edPowerAndCurrent);
  UpdateStatusDisplay;
  
  if IsNaN(values[mqPower]) and IsNaN(values[mqCurrent]) then
    exit;

  // Add new value to data array
  currTime := Now - FStartTime;
  n := Length(FData[0]);
  SetLength(FData[0], n+1);
  FData[0, n].Time := currTime;
  FData[0, n].Values := values;
  if not IsNaN(values[mqPower]) then
  begin
    if n = 0 then
    begin
      prevTime := 0;
      accumEnergy := 0;
    end else
    begin
      prevTime := FData[0, n-1].Time;
      accumEnergy := FData[0, n-1].Values[mqEnergy];
    end;
    FData[0, n].Values[mqEnergy] := accumEnergy + values[mqPower] * (currTime - prevTime) * 24;  // Wh
  end;

  // Update visual controls
  if FMeasQuant = mqCurrent then  // Current display
    UpdateValueDisplay(values[mqCurrent], false)
  else
    UpdateValueDisplay(values[mqPower], true);

  // Update chart
  UpdateTimeAxis(currTime);
  FChartSources[0].PointsNumber := n;
  Chart.Invalidate;

  // Update grid
  if FGridDataIdx = 0 then
  begin
    if Length(FData[0]) + Grid.FixedRows > Grid.RowCount then
      Grid.RowCount := Length(FData[0]) + Grid.FixedRows;
    Grid.Invalidate;
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CanClose then
    try
      WriteToIni;
    except
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PointFormatSettings := DefaultFormatSettings;
  PointFormatSettings.DecimalSeparator := '.';
  FTimeUnits := tuMinutes;

  FLedNumber := TLedNumber.Create(self);
  with FLedNumber do 
  begin
    Parent := LeftPanel;
    AnchorSideLeft.Control := LeftPanel;
    AnchorSideLeft.Side := asrLeft;
    AnchorSideTop.Control := LeftPanel;
    Caption := '';
    Columns := 6;
    Size := 3;
    Slanted := True;
    AutoSize := true;
  end;
    
  pbStatusLED.AnchorSideTop.Control := FLedNumber;
  pbStatusLED.AnchorSideTop.Side := asrTop;
  pbStatusLED.AnchorSideLeft.Control := FLedNumber;
  pbStatusLED.AnchorSideLeft.Side := asrRight;
  
  Notebook.PageIndex := 0;
  Chart.BottomAxis.Title.Caption := 'Time (' + TIMEUNITS[FTimeUnits] + ')';

  Grid.AnchorSideTop.Control := FLedNumber;
  Grid.AnchorSideTop.Side := asrBottom;
  Grid.RowHeights[0] := 2*Grid.DefaultRowHeight;
  
//  tbExpand.Align := alRight;

  // FData[0] and FChartSources[0] are reserved for measurement data
  SetLength(FData, 1);
  SetLength(FData[0], 0);
  SetLength(FChartSources, 1);
  FChartSources[0] := ChartSource;

  ReadFromIni;
  if not Edi_IsAvail(false) then
  begin     
    if (DeviceParams.IPAddress = '') then
      acSetupExecute(nil)
    else     
      MessageDlg('Edimax plug is not found', mtError, [mbOK], 0);
  end;

  UpdateCaption;
  UpdateStatusDisplay;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LeftPanel.Constraints.MinWidth := FLEDNumber.Left + FLEDNumber.Width;
end;

procedure TMainForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: String;
  ts: TTextStyle;
begin
  s := '';
  with Grid.Canvas do begin
    if ARow = 0 then
      exit;
    ts := TextStyle;
    if ACol = 0 then begin
      s := IntToStr(ARow);
      ts.Alignment := taRightJustify;
    end else
    if ARow <= Length(FData[FGridDataIdx]) then begin
      case ACol of
        1: s := FormatDateTime(TIMEFORMAT_LONG[FTimeUnits], FData[FGridDataIdx, ARow-1].Time);
        2: s := EdiFloatToStr('0.00', FData[FGridDataIdx, ARow-1].Values[mqPower]);
        3: s := EdiFloatToStr('0.000', FData[FGridDataIdx, ARow-1].Values[mqCurrent]);
        4: s := EdiFloatToStr('0.000', FData[FGridDataIdx, ARow-1].Values[mqEnergy]);
      end;
    end;
    InflateRect(ARect, -constCellPadding, -constCellPadding);
    TextRect(ARect, ARect.Left, ARect.Top, s, ts);
  end;
end;

procedure TMainForm.LoadData(const AFileName: String);
const
  BLOCK_SIZE = 1000;
var
  F: TextFile;
  s: String;
  sa: TStringArray;
  data: TDataArray = nil;
  quantName: String = '';
  quantUnits: String = '';
  timeUnits: TTimeUnits;
  n, m: Integer;
  ser: TLineSeries;
  mq: TMeasQuant;
  mq1: TMeasQuant = mqNone;
  mq2: TMeasQuant = mqNone;
  timestep: Double;
  energy: Double;
  accumEnergy: Double;
begin
  if not FileExists(AFileName) then
  begin
    MessageDlg(Format('File "%s" not found.', [AFileName]), mtError, [mbOK], 0);
    exit;
  end;
  
  n := 0;
  SetLength(data, BLOCK_SIZE);
  
  AssignFile(F, AFileName);
  Reset(F);
  while not EoF(F) do
  begin
    ReadLn(F, s);
    if s = '' then
      Continue;
    sa := s.Split(#9);
    if s[1] = '#' then
    begin
      GetMeasQuantNameAndUnits(sa[0], quantName, quantUnits);   // variables are dummies here.
      if quantUnits = '' then
        Continue;
      timeUnits := StrToTimeUnits(quantUnits);
      if Length(sa) >= 2 then
      begin
        GetMeasQuantNameAndUnits(sa[1], quantName, quantUnits);
        if quantUnits = 'W' then
          mq1 := mqPower
        else if quantUnits = 'A' then
          mq1 := mqCurrent;
      end;
      if Length(sa) >= 3 then
      begin
        GetMeasQuantNameAndUnits(sa[2], quantName, quantUnits);
        if quantUnits = 'W' then
          mq2 := mqPower
        else if quantUnits = 'A' then
          mq2 := mqCurrent;
      end;
    end else
    begin
      // Initialize values
      for mq in TMeasQuant do data[n].Values[mq] := NaN;
      // Get time
      data[n].Time := StrToFloat(sa[0], PointFormatSettings);
      if timeUnits <> tuDays then
        data[n].Time := ConvertTimeUnits(data[n].Time, timeUnits, tuDays);
      // Get value in 1st file column
      if mq1 <> mqNone then
        data[n].Values[mq1] := StrToFloat(sa[1], PointFormatSettings);
      // Get value in 2nd file column
      if mq2 <> mqNone then
        data[n].Values[mq2] := StrToFloat(sa[2], PointFormatSettings);
      // Calculate power value from current (if not contained in file)
      if IsNaN(data[n].Values[mqPower]) and not IsNaN(data[n].Values[mqCurrent]) then
        data[n].Values[mqPower] := Voltage * data[n].Values[mqCurrent];
      // Calculate current value from power (if not contained in file)
      if IsNaN(data[n].Values[mqCurrent]) and not IsNaN(data[n].Values[mqPower]) then
        data[n].Values[mqCurrent] := data[n].Values[mqPower] / Voltage;

      // Calculate accumlated energy, in Wh
      if not IsNaN(data[n].Values[mqPower]) then
      begin
        if n = 0 then
        begin
          timeStep := data[n].Time;
          accumEnergy := 0;
        end else
        begin
          timeStep := data[n].Time - data[n-1].Time;
          accumEnergy := data[n-1].Values[mqEnergy];
        end;
        energy := data[n].Values[mqPower] * timeStep * 24;  // Wh
        data[n].Values[mqEnergy] := accumEnergy + energy;
      end;
          
      // Increment counter of data values
      inc(n);      
      // Redim array if counter goes beyond current array limits.
      if n mod BLOCK_SIZE = 0 then
        SetLength(data, n + BLOCK_SIZE);
    end;
  end;
  CloseFile(F);
  
  // Trim data array to size occupied.
  SetLength(data, n);
 
  // Fix time units
  UpdateTimeAxis(data[n-1].Time);
  
  // Move data found to global data array
  SetLength(FData, Length(FData)+1);
  FData[High(FData)] := data;
  
  // Add chart source for the loaded data
  m := Length(FChartSources);
  SetLength(FChartSources, m + 1);
  FChartSources[m] := TUserDefinedChartSource.Create(self);
  FChartSources[m].Tag := m;
  FChartSources[m].OnGetChartDataItem := @ChartSourceGetChartDataItem;
  FChartSources[m].PointsNumber := n;
  
  // Add series for the loaded data
  ser := TLineSeries.Create(Chart);
  ser.Source := FChartSources[m];
  ser.Title := ChangeFileExt(ExtractFileName(AFileName), '');
  ser.SeriesColor := SERIES_COLORS[m mod Length(SERIES_COLORS)];
  Chart.AddSeries(ser);
  
  // Show loaded data in grid
  FGridDataIdx := m;
  Grid.RowCount := n + grid.FixedRows;
  Grid.Invalidate;
end;


procedure TMainForm.pbStatusLEDPaint(Sender: TObject);
var
  ppi: Integer;
  idx: Integer;
  f: Double;
begin
  ppi := Font.PixelsPerInch;
  idx := ord(FEdiState);
  f := GetCanvasScaleFactor;
  LED_Images.DrawForPPI(pbStatusLED.Canvas, 0, 0, idx, LED_Images.Width, ppi, f);
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    with NetworkParams do begin
      DeviceAddress := ini.ReadString('NetworkParams', 'IPAddress', DeviceAddress);
      NetworkAddress := ini.ReadString('NetworkParams', 'NetworkAddress', NetworkAddress);
      SubnetMask := ini.ReadString('NetworkParams', 'SubnetMask', SubnetMask);
    end;
    with AuthParams do begin
      UserName := ini.ReadString('Authentication', 'UserName', UserName);
      Password := ini.ReadString('Authentication', 'Password', Password);
    end;
    with DeviceParams do begin
      DisplayName := ini.ReadString('DeviceParams', 'DisplayName', DisplayName);
      Model := ini.ReadString('DeviceParams', 'Model', Model);
      IPAddress := ini.ReadString('DeviceParams', 'IPAddress', IPAddress);
      Port := ini.ReadInteger('DeviceParams', 'Port', Port);
      MacAddress := ini.ReadString('DeviceParams', 'MacAddress', MacAddress);
      Manufacturer := ini.ReadString('DeviceParams', 'Manufacturer', Manufacturer);
      SoftwareVersion := ini.ReadString('DeviceParams', 'SoftwareVersion', SoftwareVersion);
    end;

  finally
    ini.Free;
  end;
end;


procedure TMainForm.SaveDataAs(const AFileName: String);
var
  F: TextFile;
  i: integer;
  sT, sP, sI: String;
begin
  AssignFile(F, AFileName);
  Rewrite(F);
  try
    // add "#" for compatibility with gnuplot
    WriteLn(F, '#Measurement data');
    WriteLn(F, '#', DateTimeToStr(FStartTime));
    WriteLn(F);
    Write(F, '#', 'Time (days)', TAB, 'Power (W)', TAB, 'Current (A)');
    WriteLn(F);
    for i:=0 to High(FData[0]) do begin
      sT := Format('%.6f', [FData[0, i].Time], PointFormatSettings);
      sP := Format('%.6f', [FData[0, i].Values[mqPower]], PointFormatSettings);
      sI := Format('%.6f', [FData[0, i].Values[mqCurrent]], PointFormatSettings);
      WriteLn(F, sT, TAB, sP, TAB, sI);
    end;
  finally
    CloseFile(F);
  end;
end;


procedure TMainForm.SetDebugView(const AEnable: Boolean);
begin
  Coolbar.Bands[1].Visible := AEnable;
  if AEnable then
  begin
    Notebook.PageIndex := 1;  // PgDebug
    acExpand.ImageIndex := 7;
    acExpand.Hint := 'Hide toolbar with internal functions';
  end else 
  begin
    Notebook.PageIndex := 0;  // PgMeasurement
    acExpand.ImageIndex := 6;
    acExpand.Hint := 'Show toolbar with internal functions';
  end;
end;


procedure TMainForm.TimerTimer(Sender: TObject);
begin
  ExecuteMeasurement;
end;

procedure TMainForm.StandardToolBarResize(Sender: TObject);
begin
  //tbExpand.Left := StandardToolBar.ClientWidth - tbExpand.Width;
end;


procedure TMainForm.UpdateCaption;
begin
  if Edi_IsAvail(true) then
    Caption := 'Edimax ' + DeviceParams.Model + ' - [' + DeviceParams.DisplayName +']'
  else
    Caption := 'Edimax - not available';
end;


procedure TMainForm.UpdateDeviceParams(IP: String = '');
begin
  // In Linux in my VM, the broadcast does not work unless the correct IP address
  // is specified.
  if IP = '' then
    IP := GetBroadcastAddr(NetworkParams.NetworkAddress, NetworkParams.SubnetMask);
  if not Edi_FindParams(IP, DeviceParams) then
  begin
    DeviceParams := EmptyDeviceParams;
    // On Linux detection does not always work. 
    // Make sure that manually entered IP is used.
    DeviceParams.IPAddress := NetworkParams.DeviceAddress;
  end;
end;


procedure TMainForm.UpdateStatusDisplay;
begin
  FEdiState := Edi_GetState;
  pbStatusLED.Invalidate;
  acOn.Enabled := FEdiState <> esOn;
  acOff.Enabled := FEdiState = esOn;
end;


procedure TMainForm.UpdateTimeAxis(ALastTime: TDateTime);
begin
  if ALastTime > ONE_DAY then
    FTimeUnits := tuDays
  else 
  if ALastTime > ONE_HOUR then
    FTimeUnits := tuHours
  else
    FTimeUnits := tuMinutes;
  
  if FTimeUnits = tuDays then begin
    Chart.BottomAxis.Marks.Source := nil;
    Chart.BottomAxis.Marks.Style := smsValue;
    Chart.BottomAxis.Marks.Format := TIMEFORMAT[tuDays];
  end else begin
    DatetimeIntervalChartSource.DateTimeFormat := TIMEFORMAT[FTimeUnits];
    Chart.BottomAxis.Marks.Source := DateTimeIntervalChartSource;
    Chart.BottomAxis.Marks.Style := smsLabel;
  end;
  Chart.BottomAxis.Title.Caption := 'Time (' + TIMEUNITS[FTimeUnits] + ')';
end;


procedure TMainForm.UpdateValueAxis;
begin
  Chart.LeftAxis.Title.Caption := MEASQUANT_NAMES[FMeasQuant];
end;


procedure TMainForm.UpdateValueDisplay(AValue: Double; IsPower: Boolean);
const
  FMT: array[boolean] of String = ('%.3fA', '%.1fW');
begin
  FLEDNumber.Caption := PadLeft(Format(FMT[IsPower], [AValue]), FLEDNumber.Columns+1);
    // +1 because of decimal separator
end;


procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
begin
  ini := CreateIni;
  try
    with NetworkParams do begin
      ini.WriteString('NetworkParams', 'DHCPServer', NetworkAddress);
      ini.WriteString('NetworkParams', 'SubnetMask', SubnetMask);
      ini.WriteString('NetworkParams', 'IPAddress', DeviceAddress);
    end;
    with AuthParams do begin
      ini.WriteString('Authentication', 'UserName', UserName);
      ini.WriteString('Authentication', 'Password', Password);
    end;
    with DeviceParams do begin
      ini.WriteString('DeviceParams', 'DisplayName', DisplayName);
      ini.WriteString('DeviceParams', 'Model', Model);
      ini.WriteString('DeviceParams', 'IPAddress', IPAddress);
      ini.WriteInteger('DeviceParams', 'Port', Port);
      ini.WriteString('DeviceParams', 'MacAddress', MacAddress);
      ini.WriteString('DeviceParams', 'Manufacturer', Manufacturer);
      ini.WriteString('DeviceParams', 'SoftwareVersion', SoftwareVersion);
    end;
  finally
    ini.Free;
  end;
end;

end.

