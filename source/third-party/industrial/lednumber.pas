{*********************************************************}
{*                 VPLEDLABEL.PAS 1.03  -> LEDNumber.PAS *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{* Modified by Jurassic Pork for include in industrial Stuff Lazarus package  *}
{* 05/2013                                                                    *}
{* ***** END LICENSE BLOCK *****                                              *}

unit LedNumber;

{$mode objfpc}{$H+}

interface

uses
  LMessages, Classes, Controls, Graphics;

type
  TSegmentSize = 2..10;
  TLedNumberBorderStyle = (lnbNone, lnbSingle, lnbSunken, lnbRaised);

  { TCustomLEDNumber }

  TCustomLEDNumber = class(TGraphicControl)
  private
    FBorderStyle: TLedNumberBorderStyle;
    FTransparent: boolean;
    FScaleFactor: Double;
    procedure SetBorderStyle(AValue: TLedNumberBorderStyle);
    procedure SetTransparent(AValue: boolean);
    procedure SetZeroToO(AValue: Boolean);
  protected{private}
    FBgColor   : TColor;
    FOffColor  : TColor;
    FOnColor   : TColor;
    FColumns   : Integer;
    FRows      : Integer;
    FZeroToO   : Boolean;
    FSize      : TSegmentSize;
    FSlantAngle: Integer;
    FSlanted   : Boolean;
    lbDrawBmp  : TBitmap;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure Initialize(out Points: array of TPoint);
    procedure Loaded; override;
    function  NewOffset(xOry: char; OldOffset: Integer): Integer;
    procedure Paint; override;
    procedure PaintSegment(Segment: Integer; TheColor: TColor;
                           Points: array of TPoint; OffsetX, OffsetY: Integer);
    procedure ProcessCaption(Points: array of TPoint);
    procedure ResizeControl(ARows, ACols, ASize: Integer);
    procedure SetParent(NewParent: TWinControl); override;
    procedure SlantPoint(var P: TPoint; RowHeight: Integer; tanAlpha: Double);

    function  GetAbout: string;
    function  GetSlantAngle: Double;
    procedure SetAbout(const {%H-}Value: string);
    procedure SetAutoSize(AValue: Boolean); override;
    procedure SetSize(Value: TSegmentSize);
    procedure SetSlantAngle(Value: Integer);
    procedure SetSlanted(Value: Boolean);
    procedure SetOnColor(Value: TColor);
    procedure SetOffColor(Value: TColor);
    procedure SetRows(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetBGColor(Value: TColor);
    procedure SelectSegments(Segment: Word; Points: array of TPoint;
                             OffsetX, OffsetY: Integer);
  protected
    {properties}
    property Version: string  read GetAbout write SetAbout stored False;
    property BorderStyle: TLedNumberBorderStyle read FBorderStyle write SetBorderStyle default lnbNone; {Draws border around segments.}
    property Columns: Integer read FColumns write SetColumns default 10;
    property Rows: Integer    read FRows write SetRows default 1;
    property BgColor: TColor  read FbgColor write SetbgColor default clBlack;
    property OffColor: TColor read FOffColor write SetOffColor default $000E3432;
    property OnColor: TColor  read FOnColor write SetOnColor default clLime;
    property Size: TSegmentSize read FSize write SetSize default 2;
    property SlantAngle: Integer read FSlantAngle write SetSlantAngle default 8;
    property Slanted: Boolean read FSlanted write SetSlanted default false;
    property Transparent: boolean read FTransparent write SetTransparent default false; {Draws segments with transparent background.BgColor is used as mask color.}
    property ZeroToO: Boolean read FZeroToO write SetZeroToO default false;

  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  end;

  TLEDNumber = class(TCustomLEDNumber)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Version;
    property BorderSpacing;
    property BorderStyle;
    property Caption;
    property Columns;
    property Rows;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property BgColor;
    property OffColor;
    property OnColor;
    property ParentShowHint;
    property PopupMenu;
    property Size;
    property ShowHint;
    property SlantAngle;
    property Slanted;
    property Transparent;
    property Visible;
    property ZeroToO;
  end;


implementation

uses
  Math;

{        LED Segment Map            }
{                                   }
{ ------------------------          }
{ |          1           |          }
{ ------------------------          }
{ |  | \    |  |    / |  |          }
{ |  |  \   |  |   /  |  |          }
{ |  |   \  |  |  /   |  |          }
{ |2 |\ 3 \ |4 | / 5 /|6 |          }
{ |  | \   \|  |/   / |  |          }
{ |  |  \   |  |   /  |  |          }
{ -----------  -----------          }
{ |   7      \/   8      |          }
{ -----------/\-----------          }
{ |  |  /   |  |   \  |  |          }
{ |  | /   /|  |\   \ |  |          }
{ |9 |/10 / |11| \12 \|13|          }
{ |  |   /  |  |  \   |  |          }
{ |  |  /   |  |   \  |  |          }
{ |  | /    |  |    \ |  |          }
{ ------------------------ |-----|  }
{ |          14          | |  *  |  }
{ ------------------------ |-----|  }
{                                   }
{ * Period and comma are drawn here }
{ Colon is drawn in the center of   }
{ segments 4 and 11                 }

{ Each segment is made up of 6 points.  The segments that don't need 6 points, }
{ such as the period and colon dots, return to the coordinates of the initial  }
{ point for the remaining unused points.                                       }

const
{LED SEGMENT ARRAYS}
  MAX_POINTS = 107;

  DigitPoints: array[0..MAX_POINTS] of TPoint =
  {Segment 1}
  ((X:2;Y:2),(X:3;Y:1),(X:11;Y:1),(X:12;Y:2),(X:11;Y:3),(X:3;Y:3),
  {Segment 2}
  (X:2;Y:3),(X:3;Y:4),(X:3;Y:12),(X:2;Y:13),(X:1;Y:12),(X:1;Y:4),
  {Segment 3}
  (X:3;Y:3),(X:6;Y:9),(X:6;Y:13),(X:3;Y:7),(X:3;Y:3),(X:3;Y:3),
  {Segment 4}
  (X:7;Y:3),(X:8;Y:4),(X:8;Y:12),(X:7;Y:13),(X:6;Y:12),(X:6;Y:4),
  {Segment 5}
  (X:11;Y:3),(X:11;Y:7),(X:8;Y:13),(X:8;Y:9),(X:11;Y:3),(X:11;Y:3),
  {Segment 6}
  (X:12;Y:3),(X:13;Y:4),(X:13;Y:12),(X:12;Y:13),(X:11;Y:12),(X:11;Y:4),
  {Segment 7}
  (X:2;Y:14),(X:3;Y:13),(X:6;Y:13),(X:7;Y:14),(X:6;Y:15),(X:3;Y:15),
  {Segment 8}
  (X:7;Y:14),(X:8;Y:13),(X:11;Y:13),(X:12;Y:14),(X:11;Y:15),(X:8;Y:15),
  {Segment 9}
  (X:2;Y:15),(X:3;Y:16),(X:3;Y:24),(X:2;Y:25),(X:1;Y:24),(X:1;Y:16),
  {Segment 10}
  (X:6;Y:15),(X:6;Y:19),(X:3;Y:25),(X:3;Y:21),(X:6;Y:15),(X:6;Y:15),
  {Segment 11}
  (X:7;Y:15),(X:8;Y:16),(X:8;Y:24),(X:7;Y:25),(X:6;Y:24),(X:6;Y:16),
  {Segment 12}
  (X:8;Y:15),(X:11;Y:21),(X:11;Y:25),(X:8;Y:19),(X:8;Y:15),(X:8;Y:15),
  {Segment 13}
  (X:12;Y:15),(X:13;Y:16),(X:13;Y:24),(X:12;Y:25),(X:11;Y:24),(X:11;Y:16),
  {Segment 14}
  (X:2;Y:26),(X:3;Y:25),(X:11;Y:25),(X:12;Y:26),(X:11;Y:27),(X:3;Y:27),
  {Period    }
  (X:14;Y:25),(X:16;Y:25),(X:16;Y:27),(X:14;Y:27),(X:14;Y:25),(X:14;Y:25),
  {Comma     }
  (X:14;Y:25),(X:16;Y:25),(X:16;Y:27),(X:13;Y:30),(X:14;Y:27),(X:14;Y:25),
  {Colon Top }
  (X:5;Y:7),(X:9;Y:7),(X:9;Y:10),(X:5;Y:10),(X:5;Y:7),(X:5;Y:7),
  {Colon Btm }
  (X:5;Y:20),(X:9;Y:20),(X:9;Y:23),(X:5;Y:23),(X:5;Y:20),(X:5;Y:20));

  Characters: Array[0..72] of Word =
  ($0000,$3B70,$1320,$0001,$0300,$0002,$0840,$CCCC,$1020,$8784,
  { ' '    *      +    ,     -     .     /      0     1     2  }
   $870C,$4708,$C30C,$C38C,$8408,$C78C,$C70C,$0810,$2040,$C788,
  {   3     4     5     6     7     8     9     <     >     A  }
   $952C,$C084,$942C,$C384,$C380,$C18C,$4788,$9024,$048C,$4A90,
  {   B     C     D     E     F     G     H     I     J     K  }
   $4084,$6C88,$6498,$C48C,$C780,$C49E,$C790,$C214,$9020,$448C,
  {   L     M     N     O     P     Q     R     S     T     U  }
   $48C0,$44D8,$2850,$2820,$8844,$2010,$C788,$952C,$C084,$942C,
  {   V     W     X     Y     Z     /     a     b     c     d  }
   $C384,$C380,$C18C,$4788,$9024,$048C,$4A90,$4084,$6C88,$6498,
  {   e     f     g     h     i     j     k     l     m     n  }
   $C48C,$C780,$C49E,$C790,$C214,$9020,$448C,$48C0,$44D8,$2850,
  {   o     p     q     r     s     t     u     v     w     x  }
   $2820,$8844,$FFFF);
  {   y     z     : }

  CharacterNDX: Array[1..122] of integer =
  (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
   10, 11, 12, 13, 14, 15, 16, 72, 0, 17, 0, 18, 0, 0, 19, 20, 21, 22, 23, 24,
   25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43,
   44, 0, 45, 0, 0, 0, 0, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
   32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44);

const
  BASE_WIDTH = 17;
  BASE_HEIGHT = 30;


{===== TCustomLEDNumber ============================================}

constructor TCustomLEDNumber.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FScaleFactor := Font.PixelsPerInch / 96;
  FTransparent := False;
  FBorderStyle := lnbNone;
  ControlStyle := [csCaptureMouse,
                   csOpaque,
                   csSetCaption,
                   csClickEvents,
                   csDoubleClicks];
  FRows := 1;
  FColumns := 10;
  FSize := 2;
  FOnColor := clLime;
  FOffColor := $000E3432;
  FBgColor := clBlack;
  FSlantAngle := 8;
  Caption := 'LED-LABEL';
  lbDrawBmp := TBitmap.Create;

  ResizeControl(FRows, FColumns, FSize);
end;
{=====}

destructor TCustomLEDNumber.Destroy;
begin
  lbDrawBmp.Free;
  lbDrawBmp := nil;
  inherited Destroy;
end;
{=====}

function TCustomLEDNumber.GetAbout : string;
begin
  Result := ''; //VpVersionStr;
end;
{=====}

procedure TCustomLEDNumber.SetAbout(const Value : string);
begin
  {Leave empty}
end;

procedure TCustomLEDNumber.SetAutoSize(AValue: Boolean);
begin
  if AValue = inherited AutoSize then
    exit;
  inherited SetAutoSize(AValue);
  if AValue then
    ResizeControl(FRows, FColumns, FSize);
end;

procedure TCustomLEDNumber.SetTransparent(AValue: boolean);
begin
  if FTransparent=AValue then Exit;
  FTransparent:=AValue;
  lbDrawBmp.Transparent := FTransparent;
  lbDrawBmp.TransparentColor := FBgColor;
  Invalidate;
end;

procedure TCustomLEDNumber.SetZeroToO(AValue: Boolean);
begin
  if FZeroToO = AValue then Exit;
  FZeroToO := AValue;
  Invalidate;
end;

procedure TCustomLEDNumber.SetBorderStyle(AValue: TLedNumberBorderStyle);
begin
  if FBorderStyle=AValue then Exit;
  FBorderStyle:=AValue;
  Invalidate;
end;

procedure TCustomLEDNumber.CMTextChanged(var Message: TLMessage);
begin
  inherited;
  Invalidate;
end;
{=====}

procedure TCustomLEDNumber.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  inherited;
  FScaleFactor := Font.PixelsPerInch / 96;
  ResizeControl(FRows, FColumns, FSize);
end;

procedure TCustomLEDNumber.Initialize(out Points: array of TPoint);
var
  I: Integer;
  tanAlpha: Double;
  rowHeight: Integer;
begin
  for I := 0 to MAX_POINTS do begin
    Points[i].X := round(FScaleFactor * (DigitPoints[i].X * (FSize - 1)));
    Points[i].Y := round(FScaleFactor * (DigitPoints[i].Y * (FSize - 1)));
  end;

  if FSlanted and (FSlantAngle <> 0) then
  begin
    rowHeight := round(FScaleFactor * BASE_HEIGHT * (FSize - 1));
    tanAlpha := tan(GetSlantAngle);
    for i := 0 to MAX_POINTS do
      SlantPoint(Points[i], rowHeight, tanAlpha);
  end;
end;
{=====}

procedure TCustomLEDNumber.Loaded;
begin
  inherited;
  ResizeControl(FRows, FColumns, FSize);
end;

function TCustomLEDNumber.NewOffset(xOry: char; OldOffset: Integer): Integer;
begin
  if (xOry = 'x')then
    Result := oldOffset + round(FScaleFactor * BASE_WIDTH * (FSize - 1))
  else
    Result := oldOffset + round(FScaleFactor * BASE_HEIGHT * (FSize - 1));
end;
{=====}

procedure TCustomLEDNumber.Paint;
var
  Points: array[0..MAX_POINTS] of TPoint;
  ARect: TRect;
  AADraw: Boolean;  // anti-aliased drawing flag
  savedScaleFactor: Double;
begin
  AADraw := (Font.PixelsPerInch <> 96) and (FSize = 2);
  if AADraw then begin
    savedScaleFactor := FScaleFactor;
    FScaleFactor := 2*FScaleFactor;
    lbDrawBMP.Width := 2*Width;
    lbDrawBMP.Height := 2*Height;
  end else
  begin
    lbDrawBMP.Width := Width;
    lbDrawBMP.Height := Height;
  end;

  Initialize(Points);
  lbDrawBMP.Canvas.Brush.Color := FBgColor;
  lbDrawBMP.Canvas.FillRect(ClientRect);
  ProcessCaption(Points);

  Canvas.CopyMode := cmSrcCopy;
  if (BorderStyle <> lnbNone) then
  begin
    ARect := ClientRect;
    case BorderStyle of
      lnbSingle:
      begin
        Canvas.Pen.Color := cl3DDkShadow;
        Canvas.Frame(ARect);
      end;
      lnbSunken: Canvas.Frame3D(ARect, cl3DDkShadow, clBtnHiLight, 1);
      lnbRaised: Canvas.Frame3D(ARect, clBtnHiLight, cl3DDkShadow, 1);
    end;
    inc(ARect.Left, 1);
    inc(ARect.Top, 1);
    inc(ARect.Right, 1);
    inc(ARect.Bottom, 1);
    Canvas.StretchDraw(ARect, lbDrawBMP);
  end else
  if AADraw then begin
    ARect := ClientRect;
    Canvas.StretchDraw(ARect, lbDrawBMP);
    FScaleFactor := savedScaleFactor;
  end else
    Canvas.Draw(0, 0, lbDrawBMP);
end;
{=====}

procedure TCustomLEDNumber.PaintSegment(Segment: Integer; TheColor: TColor;
  Points: array of TPoint; OffsetX, OffsetY: Integer);
var
  I: Integer;
  DrawPts: array[0..5] of TPoint;
begin
  Dec(Segment);
  lbDrawBMP.Canvas.Pen.Style := psClear;
  lbDrawBMP.Canvas.Brush.Color := TheColor;
  for i := 0 to 5 do begin
    DrawPts[i].X := offsetX + Points[Segment * 6 + i].X;
    DrawPts[i].Y := offsetY + Points[Segment * 6 + i].Y;
  end;
  lbDrawBMP.Canvas.Polygon(DrawPts);
end;
{=====}

procedure TCustomLEDNumber.SelectSegments(Segment: Word;
  Points: array of TPoint; OffsetX, OffsetY: Integer);
var
  I     : integer;
  Bit   : word;
  MyColor : TColor;
  Skip  : Boolean;
begin
  if (Segment and $FFFF) = $FFFF then begin
    MyColor := FOnColor;
    PaintSegment(BASE_WIDTH, MyColor, Points, OffsetX, OffsetY);
    PaintSegment(BASE_WIDTH+1, MyColor, Points, OffsetX, OffsetY);
  end
  else begin
    Bit := $8000;
    for I := 1 to 16 do begin
      Skip := False;
      if (Segment and Bit) = Bit then
        MyColor := FOnColor
      else begin
        if (i = 15) or (i = 16) then
          Skip := True;
        MyColor := FOffColor;
      end;
      if (not Skip) and (MyColor <> FBgColor) then
        PaintSegment(I, MyColor,  Points, OffsetX,  OffsetY);
      Bit := Bit shr 1;
    end;
  end;
end;

procedure TCustomLEDNumber.SlantPoint(var P: TPoint;
  RowHeight: Integer; tanAlpha: Double);
begin
  inc(P.X, round(tanAlpha * (RowHeight - P.Y)));
end;

procedure TCustomLEDNumber.ProcessCaption(Points: array of TPoint);
var
  Next           : Char;
  Last           : Char;
  I, X           : Integer;
  Row, ColsPerRow: Integer;
  Tmp            : Integer;
  OffsetX        : Integer;
  OffsetY        : Integer;
  DisplayStr     : string;
begin
  Last := #0;
  OffsetX  := FSize;
  OffsetY  := 0;
  DisplayStr := Caption;

  if Length(DisplayStr) > 0 then
    if (DisplayStr[1] = ',') or (DisplayStr[1] = '.') then
      DisplayStr := ' ' + DisplayStr;

  Row := 1;
  ColsPerRow := 0;
  for I := 1 to Length(Caption) do begin
    Next := Caption[I];
    case Ord(Next) of
      42..58,60,62,65..90,92,97..122: begin
        if ColsPerRow = FColumns  then begin
          Row := Row + 1;
          if Row > FRows then
            exit;
          offsetY := newOffset('y',offsetY);
          offsetX := FSize;
          ColsPerRow := 0
        end;
        if (Next = '.') or (Next = ',') then
          if (Last = '.') or (Last = ',') then begin
            Tmp := Characters[CharacterNDX[ord(Next)]];
            SelectSegments(Tmp, Points, OffsetX, OffsetY);
            OffsetX := NewOffset('x', OffsetX);
          end
          else begin
            OffsetX := OffsetX - round(FScaleFactor * BASE_WIDTH * (FSize - 1));
            Tmp := (Characters[CharacterNDX[Ord(Next)]] or Characters[CharacterNDX[Ord(Last)]]);
            SelectSegments(Tmp, Points, OffsetX,  OffsetY);
            OffsetX := NewOffset('x', OffsetX);
          end
        else begin
          if FZeroToO and (Next = '0') then Next := 'O';
          Tmp := Characters[CharacterNDX[Ord(Next)]];
          SelectSegments(Tmp, Points, OffsetX, OffsetY);
          OffsetX := NewOffset('x', OffsetX);
          ColsPerRow := ColsPerRow + 1;
        end;
        end;
      10: begin {eat linefeed}
        end;
      13: begin
        if ColsPerRow < FColumns then
          for x := 1 to (FColumns - ColsPerRow) do begin
            SelectSegments(Characters[CharacterNDX[1]], Points, OffsetX,  OffsetY);
            OffsetX := NewOffset('x', OffsetX);
          end;
          Row := Row + 1;
          if Row > FRows then
             exit;
          OffsetY := NewOffset('y', OffsetY);
          OffsetX := FSize;
          ColsPerRow := 0;
        end;
        else begin
          if ColsPerRow = FColumns  then begin
            Row := Row + 1;
            if Row > FRows then
              Exit;
            OffsetY := NewOffset('y', OffsetY);
            OffsetX := FSize;
            ColsPerRow := 0;
          end;
          SelectSegments(Characters[CharacterNDX[1]], Points, OffsetX, OffsetY);
          OffsetX := newOffset('x', OffsetX);
          ColsPerRow := ColsPerRow + 1;
        end;
    end;
    Last := Next;
  end;
  for x := 1 to (FColumns - ColsPerRow) do begin
    SelectSegments(Characters[CharacterNDX[1]], Points, OffsetX, OffsetY);
    OffsetX := NewOffset('x', OffsetX);
  end;
  if (FColumns * FRows) > Length(caption) then begin
    for X := Row + 1 to FRows do begin
      OffsetX := FSize;
      OffsetY := NewOffset('y', OffsetY);
      for I := 1 to FColumns do begin
        SelectSegments(Characters[CharacterNDX[1]], Points, OffsetX, OffsetY);
        OffsetX := NewOffset('x', OffsetX);
      end;
    end;
  end;
end;
{=====}

procedure TCustomLEDNumber.ResizeControl(ARows, ACols, ASize: Integer);
var
  w, h: Integer;
begin
  FRows := ARows;
  FColumns := ACols;
  FSize := ASize;
  h := round(FScaleFactor * FRows * BASE_HEIGHT * (FSize - 1));
  w := round(FScaleFactor * (FColumns * BASE_WIDTH * (FSize - 1)));
  if FSlanted and (FSlantAngle <> 0) then
    inc(w, round(h * tan(GetSlantAngle)));
  SetBounds(Left, Top, w, h);
  Invalidate;
end;
{=====}

procedure TCustomLEDNumber.SetBGColor(Value: TColor);
begin
  if FBGColor <> Value then begin
    FBGColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TCustomLEDNumber.SetOnColor(Value:TColor);
begin
  if FOnColor <> Value then begin
    FOnColor := Value;
    Invalidate;
  end;
end;
{=====}

procedure TCustomLEDNumber.SetOffColor(Value:TColor);
begin
  if FOffColor <> Value then begin
    FOffColor := Value;
    Invalidate;
  end;
end;

procedure TCustomLEDNumber.SetParent(NewParent: TWinControl);
begin
  inherited;
  Resizecontrol(FRows, FColumns, FSize);
end;

procedure TCustomLEDNumber.SetRows(Value : Integer);
begin
  if FRows <> Value then begin
    if Value < 1 then
      Value := 1;
    ResizeControl(Value, FColumns, FSize);
  end;
end;
{=====}

procedure TCustomLEDNumber.SetColumns(Value : Integer);
begin
  if FColumns <> Value then begin
    if Value < 1 then
      Value := 1;
    ResizeControl(FRows,  Value,  FSize);
  end;
end;
{=====}

procedure TCustomLEDNumber.SetSize(Value : TSegmentSize);
begin
  if FSize <> Value then begin
    //if Value < 2 then  <- unreachable
    //  Value := 2;
    //if Value > 10 then
    //  Value := 10;
    ResizeControl(FRows, FColumns, Value);
  end;
end;
{=====}

function TCustomLEDNumber.GetSlantAngle: Double;
begin
  if FSlanted then
    Result := DegToRad(FSlantAngle)
  else
    Result := 0;
end;

procedure TCustomLEDNumber.SetSlantAngle(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FSlantAngle <> Value then begin
    FSlantAngle := Value;
    ResizeControl(FRows, FColumns, FSize);
  end;
end;

procedure TCustomLEDNumber.SetSlanted(Value: Boolean);
begin
  if FSlanted <> Value then begin
    FSlanted := Value;
    ResizeControl(FRows, FColumns, FSize);
  end;
end;

end.
