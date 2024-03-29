unit emMain;

{$mode objfpc}{$H+}

interface

uses
  // FCL, LazUtils, LCL
  LCLType, LCLIntf,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Grids, Buttons, ActnList, StdCtrls, Spin, 
  // SynEdit
  SynEdit, SynHighlighterXML, 
  // third-party
  LedNumber, 
  // TAChart
  TATools,   // Needed for built-in chart toolset
  TAGraph, TASeries, TACustomSource, TASources, TAIntervalSources, 
  TAChartListbox, TAChartLiveView, 
  // project
  emGlobal, TALegend, TACustomSeries;

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
    ChartListbox: TChartListbox;
    CbLiveViewActive: TCheckBox;
    ChartLiveView: TChartLiveView;
    CoolBar: TCoolBar;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    Grid: TDrawGrid;
    Label1: TLabel;
    lblMeasTitle: TLabel;
    lblTotalEnergy: TLabel;
    Notebook: TNotebook;
    ChartPanel: TPanel;
    PgMeasurement: TPage;
    PgDebug: TPage;
    seLiveMinutes: TSpinEdit;
    StandardToolBar: TToolBar;
    InternalToolbar: TToolBar;
    ToolbarImages: TImageList;
    LED_Images: TImageList;
    LineSeries: TLineSeries;
    OpenDialog: TOpenDialog;
    LeftPanel: TPanel;
    pbStatusLED: TPaintBox;
    MainPanel: TPanel;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    RightSplitter: TSplitter;
    LeftSplitter: TSplitter;
    SynEdit: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    Timer: TTimer;
    ChartSource: TUserDefinedChartSource;
    TbSetup: TToolButton;
    TbInfo: TToolButton;
    TbStart: TToolButton;
    TbStop: TToolButton;
    TbWatts: TToolButton;
    TbAmps: TToolButton;
    TbWh: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    TbSave: TToolButton;
    TbLoad: TToolButton;
    TbExpand: TToolButton;
    ToolButton19: TToolButton;
    TbStatus: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    TbExit: TToolButton;
    ToolButton3: TToolButton;
    TbON: TToolButton;
    TbOFF: TToolButton;
    TbPower: TToolButton;
    TbCurrent: TToolButton;
    TbHistory: TToolButton;
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
    procedure CbLiveViewActiveChange(Sender: TObject);
    procedure ChartListboxAddSeries(ASender: TChartListbox; 
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListboxItemClick({%H-}ASender: TObject; AIndex: Integer);
    procedure ChartSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; ARect: TRect;
      {%H-}AState: TGridDrawState);
    procedure pbStatusLEDPaint(Sender: TObject);
    procedure seLiveMinutesChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
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
    FBackupFileName: String;
    FNextBackupTime: TDateTime;
    FActivated: Boolean;
    procedure ExecuteMeasurement;
    function IsMeasuring: Boolean;
    procedure LoadData(const AFileName: String);
    procedure SaveDataAs(const AFileName: String);
    procedure SetDebugView(const AEnable: Boolean);

    procedure UpdateCaption;
    procedure UpdateDeviceParams(IP: String = '');
    procedure UpdateStatusDisplay;
    procedure UpdateTimeAxis(ALastTime: TDateTime);
    procedure UpdateTotalEnergyDisplay;
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
  StrUtils, Math, IniFiles,
  laz2_DOM, laz2_XMLRead,
  TAChartUtils,
  emUtils, emSetup, emXmlUtils;

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
var
  i: Integer;
  fn: String;
begin
  OpenDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
  begin
    CbLiveViewActive.Checked := false;
    for i := 0 to OpenDialog.Files.Count-1 do
    begin
      fn := OpenDialog.Files[i];
      if not SameText(ExtractFileExt(fn), OpenDialog.DefaultExt) then
        fn := ChangeFileExt(fn, OpenDialog.DefaultExt);
      LoadData(fn);
    end;
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
  F: TSetupForm;
begin
  F := TSetupForm.Create(nil);
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

  FData[0].TotalEnergy := 0.0;
  SetLength(FData[0].Data, 0);
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
  FNextBackupTime := FStartTime + BACKUP_INTERVAL;
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


procedure TMainForm.CbLiveViewActiveChange(Sender: TObject);
begin
  ChartLiveView.Active := CbLiveViewActive.Checked;
  if not ChartLiveView.Active then
    Chart.ZoomFull;
end;


procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox; 
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
begin
  ASkip := (ASeries = LineSeries) and LineSeries.IsEmpty;
end;


procedure TMainForm.ChartListboxItemClick(ASender: TObject; AIndex: Integer);
var
  ser: TLineSeries;
begin
  ser := TLineSeries(ChartListbox.Series[AIndex]);
  if ser = nil then
    exit;
  FGridDataIdx := ser.Source.Tag;
  Grid.RowCount := Length(FData[FGridDataIdx].Data) + Grid.FixedRows;
  UpdateTotalEnergyDisplay;
end;


procedure TMainForm.ChartSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
var
  ChartSourceIndex: Integer;
begin
  ChartSourceIndex := ASource.Tag;
  AItem.X := FData[ChartSourceIndex].Data[AIndex].Time;
  AItem.Y := FData[ChartSourceIndex].Data[AIndex].Values[FMeasQuant];
end;


procedure TMainForm.ExecuteMeasurement;
var
  timeNow, currTime, prevTime: TDateTime;
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
  timeNow := Now();
  currTime := timeNow - FStartTime;
  n := Length(FData[0].Data);
  SetLength(FData[0].Data, n+1);
  FData[0].Data[n].Time := currTime;
  FData[0].Data[n].Values := values;
  if not IsNaN(values[mqPower]) then
  begin
    if n = 0 then
    begin
      prevTime := 0;
      accumEnergy := 0;
    end else
    begin
      prevTime := FData[0].Data[n-1].Time;
      accumEnergy := FData[0].Data[n-1].Values[mqEnergy];
    end;
    FData[0].Data[n].Values[mqEnergy] := accumEnergy + values[mqPower] * (currTime - prevTime) * 24;  // Wh
    FData[0].TotalEnergy := FData[0].Data[n].Values[mqEnergy];
    if timeNow > FNextBackupTime then
    begin
      SaveDataAs(FBackupFileName);
      FNextBackupTime := timeNow + BACKUP_INTERVAL;
    end;
  end;

  // Update power/current LED display 
  if FMeasQuant = mqCurrent then 
    UpdateValueDisplay(values[mqCurrent], false)
  else
    UpdateValueDisplay(values[mqPower], true);

  // Update chart
  UpdateTimeAxis(currTime);
  FChartSources[0].PointsNumber := n;
  Chart.Invalidate;

  if (ChartListbox.FindSeriesIndex(TCustomChartSeries(Chart.Series[0])) = -1) then
    ChartListbox.Populate;

  // Update grid
  if FGridDataIdx = 0 then
  begin
    if Length(FData[0].Data) + Grid.FixedRows > Grid.RowCount then
      Grid.RowCount := Length(FData[0].Data) + Grid.FixedRows;
    Grid.Row := Grid.RowCount-1;
  end;

  // Update total energy
  UpdateTotalEnergyDisplay
end;


procedure TMainForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    ReadFromIni;
    Grid.RowHeights[0] := 2*Grid.DefaultRowHeight;
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
  FBackupFileName := GetBackupDir + BACKUP_FILENAME;

  // To avoid installation of the TLedNumber create it at runtime.
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

  lblMeasTitle.AnchorSideTop.Control := FLedNumber;
  lblMeasTitle.AnchorSideTop.Side := asrBottom;

  Grid.AnchorSideTop.Control := lblMeasTitle;
  Grid.AnchorSideTop.Side := asrBottom;

  // FData[0] and FChartSources[0] are reserved for measurement data
  SetLength(FData, 1);
  SetLength(FData[0].Data, 0);
  FData[0].TotalEnergy := 0.0;
  SetLength(FChartSources, 1);
  FChartSources[0] := ChartSource;

  if not Edi_IsAvail(false) then
  begin     
    if (DeviceParams.IPAddress = '') then
      acSetupExecute(nil)
    else     
      MessageDlg('Edimax plug is not found', mtError, [mbOK], 0);
  end;

  SetDebugView(false);
  
  UpdateCaption;
  UpdateTotalEnergyDisplay;
  UpdateStatusDisplay;
              
  ChartLiveView.ViewportSize := seLiveMinutes.Value * ONE_MINUTE;
  ChartLiveView.Active := CbLiveViewActive.Checked;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  LeftPanel.Constraints.MinWidth := pbStatusLED.Left + pbStatusLED.Width;
  StandardToolbar.Constraints.MinWidth := TbExit.Left + TbExit.Width + 8;
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
    if ARow <= Length(FData[FGridDataIdx].Data) then begin
      case ACol of
        1: s := FormatDateTime(TIMEFORMAT_LONG[FTimeUnits], FData[FGridDataIdx].Data[ARow-1].Time);
        2: s := EdiFloatToStr('0.00', FData[FGridDataIdx].Data[ARow-1].Values[mqPower]);
        3: s := EdiFloatToStr('0.000', FData[FGridDataIdx].Data[ARow-1].Values[mqCurrent]);
        4: s := EdiFloatToStr('0.000', FData[FGridDataIdx].Data[ARow-1].Values[mqEnergy]);
      end;
    end;
    InflateRect(ARect, -constCellPadding, -constCellPadding);
    TextRect(ARect, ARect.Left, ARect.Top, s, ts);
  end;
end;


function TMainForm.IsMeasuring: Boolean;
begin
  Result := acStop.Enabled;
end;


procedure TMainForm.LoadData(const AFileName: String);
const
  BLOCK_SIZE = 1000;
var
  F: TextFile;
  s: String;
  sa: TStringArray;
  data: TDataArray;
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
  data.Data := nil;
  SetLength(data.Data, BLOCK_SIZE);
  
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
      for mq in TMeasQuant do data.Data[n].Values[mq] := NaN;
      // Get time
      data.Data[n].Time := StrToFloat(sa[0], PointFormatSettings);
      if timeUnits <> tuDays then
        data.Data[n].Time := ConvertTimeUnits(data.Data[n].Time, timeUnits, tuDays);
      // Get value in 1st file column
      if mq1 <> mqNone then
        data.Data[n].Values[mq1] := StrToFloat(sa[1], PointFormatSettings);
      // Get value in 2nd file column
      if mq2 <> mqNone then
        data.Data[n].Values[mq2] := StrToFloat(sa[2], PointFormatSettings);
      // Calculate power value from current (if not contained in file)
      if IsNaN(data.Data[n].Values[mqPower]) and not IsNaN(data.Data[n].Values[mqCurrent]) then
        data.Data[n].Values[mqPower] := Voltage * data.Data[n].Values[mqCurrent];
      // Calculate current value from power (if not contained in file)
      if IsNaN(data.Data[n].Values[mqCurrent]) and not IsNaN(data.Data[n].Values[mqPower]) then
        data.Data[n].Values[mqCurrent] := data.Data[n].Values[mqPower] / Voltage;

      // Calculate accumlated energy, in Wh
      if not IsNaN(data.Data[n].Values[mqPower]) then
      begin
        if n = 0 then
        begin
          timeStep := data.Data[n].Time;
          accumEnergy := 0;
        end else
        begin
          timeStep := data.Data[n].Time - data.Data[n-1].Time;
          accumEnergy := data.Data[n-1].Values[mqEnergy];
        end;
        energy := data.Data[n].Values[mqPower] * timeStep * 24;  // Wh
        data.Data[n].Values[mqEnergy] := accumEnergy + energy;
      end;
          
      // Increment counter of data values
      inc(n);      
      // Redim array if counter goes beyond current array limits.
      if n mod BLOCK_SIZE = 0 then
        SetLength(data.Data, n + BLOCK_SIZE);
    end;
  end;
  CloseFile(F);
  
  // Trim data array to size occupied.
  SetLength(data.Data, n);
  data.TotalEnergy := data.Data[High(data.Data)].Values[mqEnergy];
 
  // Fix time units
  UpdateTimeAxis(data.Data[n-1].Time);

  // Move data found to global data array
  m := Length(FData);
  SetLength(FData, m + 1);
  FData[m] := data;
  
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

  // Update caption for total energy
  UpdateTotalEnergyDisplay;
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
  L, T, W, H: Integer;
  R: TRect;
begin
  ini := CreateIni;
  try
    T := ini.ReadInteger('MainForm', 'Top', Top);
    L := ini.ReadInteger('MainForm', 'Left', Left);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);
    WindowState := wsNormal;
    Application.ProcessMessages;
    WindowState := TWindowState(ini.ReadInteger('MainForm', 'WindowState', 0));

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
    WriteLn(F, '#', FormatDateTime('yyyy-mm-dd hh:nn:ss', FStartTime));
    WriteLn(F);
    Write(F, '#', 'Time (days)', TAB, 'Power (W)', TAB, 'Current (A)');
    WriteLn(F);
    for i:=0 to High(FData[0].Data) do begin
      sT := Format('%.8f', [FData[0].Data[i].Time], PointFormatSettings);
      sP := Format('%.6f', [FData[0].Data[i].Values[mqPower]], PointFormatSettings);
      sI := Format('%.6f', [FData[0].Data[i].Values[mqCurrent]], PointFormatSettings);
      WriteLn(F, sT, TAB, sP, TAB, sI);
    end;
  finally
    CloseFile(F);
  end;
end;


procedure TMainForm.seLiveMinutesChange(Sender: TObject);
begin
  ChartLiveView.ViewportSize := seLiveMinutes.Value * ONE_MINUTE;
end;


procedure TMainForm.SetDebugView(const AEnable: Boolean);
begin
  if Coolbar.Bands.Count > 1 then
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
  if (cbLiveViewActive.Checked) then
  begin
    if seLiveMinutes.Value > 60 then
      FTimeUnits := tuHours
    else
      FTimeUnits := tuMinutes
  end 
  else
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


procedure TMainForm.UpdateTotalEnergyDisplay;
var
  measTitle: String;
  i: Integer;
begin
  if (FGridDataIdx > -1) and (Length(FData[FGridDataIdx].Data) > 0) then
  begin
    // Find title of selected measurement curve
    measTitle := '';
    for i := 0 to Chart.SeriesCount-1 do
      if (Chart.Series[i] is TChartSeries) and (TChartSeries(Chart.Series[i]).Source.Tag = FGridDataIdx) then
      begin
        measTitle := TChartSeries(Chart.Series[i]).Title;
        break;
      end;
    lblMeasTitle.Caption := measTitle;
    lblTotalEnergy.Caption := Format('Accumulated energy: %.1f Wh', [FData[FGridDataIdx].TotalEnergy]);
  end else
  begin
    lblMeasTitle.Caption := '';
    lblTotalEnergy.Caption := '';
  end;
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
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);
    ini.WriteInteger('MainForm', 'WindowState', Integer(WindowState));

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

