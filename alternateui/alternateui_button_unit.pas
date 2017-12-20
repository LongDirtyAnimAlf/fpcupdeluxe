{ Custom component Button Control with various states and images placement.

  Initially Designed for fpcudeluxery

  Functionality:
  - Image placement to left or centerered on top of button
  - States (normal, hover, clicked, enabled)

  Copyright (C) 2017 Josh

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit alternateui_button_unit;
{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}windows,{$endif}Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls,lresources;

Const AlternateUiButton_Version='1.0.9';

Type

  TState = (MouseIn, MouseOut, Pressed);

  TButtonColorParams=Record
    BackgroundColor:TColor;
    BorderColor:TColor;
    BorderWidth:Integer;
    FontColor:TColor;
    ArrowColor:TColor;
    UseGradient:Boolean;
  end;

  TAlternateUiButton = class(TGraphicControl)
  private
    fButtonImage:TBitmap;
    fimageLeft:Boolean;
    fimagemargin:Integer;
 //   fgrayed:boolean;
    FNormalColor:TColor;
    FNormalBorderColor:TColor;
    FNormalBorderWidth:Integer;
    FNormalFontColor:TColor;

    FHoverColor:TColor;
    FHoverBorderColor:TColor;
    FHoverBorderWidth:Integer;
    FHoverFontColor:TColor;

    FDownColor:TColor;
    FDownBorderColor:TColor;
    FDownBorderWidth:Integer;
    FDownFontColor:TColor;

    FAlignCenter:Boolean;
    FAlignBottom:Boolean;
    FDown:Boolean;
    FState: TState;
    FDropDownArrow:Boolean;
    FDropDownArrowColor:TColor;
    FCaption: String;
    FOwner: TComponent;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMLBUTTONDOWN(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMLBUTTONUP(var Message: TMessage); message WM_LBUTTONUP;
    procedure SetCaption(const aValue: String);
    procedure SetNormalColor(const AValue:TColor);
    procedure SetNormalBorderColor(const AValue:TColor);
    procedure SetNormalBorderWidth(const AValue:Integer);
    procedure SetNormalFontColor(const AValue:TColor);

    procedure SetHoverColor(const AValue:TColor);
    procedure SetHoverBorderColor(const aValue:TColor);
    procedure SetHoverBorderWidth(const aValue:Integer);
    procedure SetHoverFontColor(const aValue:TColor);

    procedure SetDownColor(const aValue:TColor);
    procedure SetDownBorderColor(const aValue:TColor);
    procedure SetDownBorderWidth(const aValue:Integer);
    procedure SetDownFontColor(const aValue:TColor);

    procedure SetAlignCenter(const aValue:Boolean);
    procedure SetAlignBottom(const aValue:Boolean);
    procedure SetDown(const aValue:Boolean);
    procedure SetDropDownArrow(const aValue:Boolean);
    procedure SetDropDownArrowColor(const aValue:TColor);
    procedure SetButtonImage(const aValue:TBitmap);

    procedure SetImageLeft(const aValue:Boolean);
    procedure SetImageMargin(const aValue:Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;

    procedure SetEnabled(Value: boolean); override;

 //   procedure SetGrayed(const aValue:Boolean);

  protected
    procedure Paint; override;
  public
    Normal,Hover,Clicked,Disabled:TButtonColorParams;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  published
    property Caption;
    property Font;
    property NormalColor: TColor read FNormalColor Write SetNormalColor;
    property NormalBorderColor: TColor read FNormalBorderColor Write SetNormalBorderColor;
    property NormalBorderWidth: Integer read FNormalBorderWidth Write SetNormalBorderWidth;
    property NormalFontColor: TColor read FNormalFontColor Write SetNormalFontColor;

    property HoverColor: TColor read FHoverColor Write SetHoverColor;
    property HoverBorderColor: TColor read FHoverBorderColor Write SetHoverBorderColor;
    property HoverBorderWidth: Integer read FHoverBorderWidth Write SetHoverBorderWidth;
    property HoverFontColor: TColor read FHoverFontColor Write SetHoverFontColor;

    property DownColor: TColor read FDownColor Write SetDownColor;
    property DownBorderColor: TColor read FDownBorderColor Write SetDownBorderColor;
    property DownBorderWidth: Integer read FDownBorderWidth Write SetDownBorderWidth;
    property DownFontColor: TColor read FDownFontColor Write SetDownFontColor;
    property AlignCenter:Boolean read FAlignCenter write SetAlignCenter;
    property AlignBottom:Boolean read FAlignBottom write SetAlignBottom;
    property Down:Boolean read FDown write SetDown;
    property DropDownArrow:Boolean read FDropDownarrow write SetDropDownArrow;
    property DropDownArrowColor:TColor read FDropDownarrowColor write SetDropDownArrowColor;
    property ButtonImage:TBitmap read FButtonImage write SetButtonImage;
    property ImageLeft:Boolean read FImageLeft write SetImageLeft;
    property ImageMargin:Integer read FImageMargin write SetImageMargin;

   // property grayed:Boolean read Fgrayed write Setgrayed;

    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;

    property Enabled;

    property Visible;

  End;

procedure Register;

implementation

Constructor TAlternateUiButton.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FState := MouseOut;
  Width := 200;
  Height := 200;
  fButtonImage:= TBitmap.Create;
end;

Destructor TAlternateUiButton.Destroy;
begin
  inherited Destroy;
  fButtonImage.Free;
end;




procedure TAlternateUiButton.Paint;

Function LightenUp(MyColor: TColor; Factor: Double): TColor;
var Red, Green, Blue: Integer;
var ChangeAmount,f: Double;
    cm:integer;
begin
  // get the color components
  Red := MyColor and $FF;
  Green := (MyColor shr 8) and $FF;
  Blue := (MyColor shr 16) and $FF;

  // calculate the new color
  If Factor>0 then F:=1+(Factor/100) else F:=1-(abs(Factor)/100);
  Cm :=trunc(Red * F);
  if cm<0 then cm:=0;
  if cm>255 then cm:=255;
  Red := cm;
  Cm :=trunc(green * F);
  if cm<0 then cm:=0;
  if cm>255 then cm:=255;
  green := cm;
  Cm :=trunc(blue * F);
  if cm<0 then cm:=0;
  if cm>255 then cm:=255;
  blue := cm;
// and return it as a TColor
  Result := Red or (Green shl 8) or (Blue shl 16);
end;

var
  round_corner:integer=0;
  bmp: TBitmap;
  DestRect: TRect;
  L_Caption: String;
  TextStyle: TTextStyle;
  arrow_size:integer=10;
  arrow_border:integer=6;
  image_lower_margin:integer=10;
  text_left_margin:integer;
  Max_Height:Integer;
  Max_Width:Integer;
  Image_Ratio:Real;
  New_H,New_W,New_L:Integer;
  has_image:boolean;
  working_color:TButtonColorParams;
  h,w,ha:integer;

begin
  h:=self.Height;
  w:=self.Width;
  has_image:=((FButtonImage.Width>1) and (FButtonImage.Height>1));
  TextStyle := Canvas.TextStyle;
  TextStyle.Wordbreak := False;
  if FalignCenter then textstyle.Alignment:=taCenter
  else textstyle.Alignment:=taLeftJustify;
  TextStyle.SingleLine:=false;
  if FAlignBottom then textstyle.Layout:=tlBottom
  else textstyle.Layout:=tlCenter;
  Bmp := TBitmap.Create;
  // Create transparent background
  bmp.Canvas.AntialiasingMode:=aMOn;
  Bmp.Height := self.Height;
  Bmp.Width := self.Width;
  bmp.Canvas.Pen.Style:=psClear;
 { bmp.Transparent := True;
  bmp.TransparentColor := clFuchsia;
  Bmp.Canvas.Brush.Color :=clFuchsia;  }
  working_color:=normal;
 // bmp.canvas.FillRect(Rect(0,0,self.Width,self.Height));
  if ((Fstate=MouseOut) and (fDown=False)) Then  working_color:=normal
  else
  if ((Fstate=MouseIn) and (fDown=False)) Then working_color:=hover
  else
  if ((Fstate=Pressed) or (fDown=True)) Then working_color:=clicked;

  if enabled=false then working_color:=disabled;
  bmp.Canvas.Font.Assign(font);
  bmp.Canvas.Font.Color:=working_color.FontColor;
  bmp.Canvas.TextStyle:=textstyle;
  if working_color.BorderWidth<1 then bmp.Canvas.Pen.Style:=psClear
  else bmp.Canvas.Pen.Style:=psSolid;
  bmp.Canvas.Pen.Width:=working_color.BorderWidth;
  bmp.Canvas.Pen.Color:=working_color.BorderColor;
  Bmp.Canvas.Brush.Color :=working_color.BackgroundColor;
  //Bmp.Canvas.roundrect(Rect(0,0,self.Width,self.Height),ROund_Corner,ROund_Corner);
  //Bmp.Canvas.rectangle(Rect(0,0,self.Width,self.Height));//,ROund_Corner,ROund_Corner);

  // was using rectangle but this has issues on carbon as single line border was always drawn even when pen.style=pscler

  if working_color.BorderWidth>0 then
  begin
    bmp.Canvas.Brush.Color:=working_color.BorderColor;   // create filled rectangle of border color
    bmp.Canvas.fillrect(rect(0,0,self.Width,self.height));
  end;
  // now fill the inner button

  bmp.Canvas.Brush.Color:=working_color.BackgroundColor;

  If Working_Color.UseGradient then
  begin
    bmp.Canvas.Pen.Style:=psSolid;
    ha:=(h-working_color.BorderWidth-working_color.BorderWidth) div 2;
    bmp.Canvas.GradientFill(rect(working_color.BorderWidth,working_color.BorderWidth+1,w-working_color.BorderWidth-1,ha),LightenUp(working_color.BackgroundColor,40),working_color.BackgroundColor,gdVertical);
    bmp.Canvas.GradientFill(rect(working_color.BorderWidth,ha,w-working_color.BorderWidth-1,h-working_color.BorderWidth),working_color.BackgroundColor,LightenUp(working_color.BackgroundColor,60),gdVertical);
  end
  else
  begin
    bmp.Canvas.Pen.Style:=psClear;
    bmp.Canvas.fillrect(rect(working_color.BorderWidth,working_color.BorderWidth,w-working_color.BorderWidth,h-working_color.BorderWidth));
  end;

//  bmp.Canvas.Brush.Color:=working_color.BackgroundColor;   // create filled rectangle of button color
//  bmp.Canvas.fillrect(rect(working_color.BorderWidth,working_color.BorderWidth,self.Width-working_color.BorderWidth,self.height-working_color.BorderWidth));

//  bmp.Canvas.GradientFill(rect(working_color.BorderWidth,working_color.BorderWidth,w-working_color.BorderWidth,ha),clred,clblue,gdVertical);
//  bmp.Canvas.GradientFill(rect(working_color.BorderWidth,ha,w-working_color.BorderWidth,h-working_color.BorderWidth),clblue,clred,gdVertical);

  if FDropDownArrow then
  begin
    // draw triangle
    bmp.Canvas.Pen.Style:=psClear;
    bmp.Canvas.Brush.Color :=working_color.ArrowColor;
    bmp.Canvas.Polygon([Point(self.Width-arrow_size-arrow_border,(self.height-arrow_size) div 2),Point(self.width-arrow_border,(self.height-arrow_size) div 2),Point(self.Width-arrow_border-(arrow_size div 2),((self.height-arrow_size) div 2)+arrow_size)]);
  end;

//  canvas.Draw(0,0,bmp);
  text_left_margin:=0;
  If ((has_image) and (Assigned(FButtonImage))) Then
  Begin
    // calculate ratio
    if ((fbuttonimage.Width>0) and (fbuttonimage.Height>0)) then
    begin
      If FImageLeft then
      begin
        // Image appears on left like a glyph.
        Max_Height:=self.height-FImageMargin-FImageMargin;
        New_H:=Max_Height;
        New_W:=Max_Height;
        New_L:=0;
        text_left_margin:=New_W+FImageMargin;
      end
      else
      begin
        // Image Centers Across top of Button
        Max_Height:=self.height-image_lower_margin-FImageMargin-FImageMargin;
        Max_Width:=self.width-FImageMargin-FImageMargin;
        if FDropDownArrow then Max_Width:=self.width-arrow_size-arrow_border;
        Image_Ratio:=fButtonImage.width/fbuttonimage.height;
        New_H:=Max_Height;
        New_W:=trunc(new_H*Image_Ratio);
        If New_W>Max_Width then New_W:=Max_Width;
        New_L:=0;
        If New_W<Max_Width then New_L:=(Max_Width-New_W) div 2;
      end;
      //canvas.stretchDraw(rect(New_L+FImageMargin,FImageMargin,New_W+New_L,New_H),fButtonImage);
      bmp.Canvas.stretchDraw(rect(New_L+FImageMargin,FImageMargin,New_W+New_L,New_H),fButtonImage);
    end;
  end;
  Canvas.Brush.Style := bsClear;
  if FDropDownArrow then DestRect:= Rect(text_left_margin,0,self.width-arrow_size-arrow_border,self.Height)
  else DestRect := Rect(text_left_margin,0,self.width,self.Height-2);
  L_Caption := Caption;
  bmp.canvas.textrect(DestRect,0,0,L_Caption,textstyle);
  canvas.Draw(0,0,bmp);
  bmp.Free;
end;

procedure TAlternateUiButton.SetImageLeft(const aValue: Boolean);
begin
  if FImageLeft<> aValue then
  begin
    FImageLeft:= aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetImageMargin(const aValue: Integer);
begin
  if FImageMargin<> aValue then
  begin
    FImageMargin:= aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetAlignCenter(const aValue: Boolean);
begin
  if FAlignCenter<> aValue then
  begin
    FAlignCenter:= aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetDown(const aValue: Boolean);
begin
  if FDown<> aValue then
  begin
    FDown:= aValue;
    if fdown=false then Fstate:=MouseOut;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetDropDownArrow(const aValue: Boolean);
begin
  if FDropDownArrow<>aValue then
  begin
    FDropDownArrow:= aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetDropDownArrowColor(const aValue: TColor);
begin
  if FDropDownArrowColor<>aValue then
  begin
    FDropDownArrowColor:= aValue;
    Invalidate;
  end;
end;


procedure TAlternateUiButton.SetAlignBottom(const aValue: Boolean);
begin
  If FAlignBottom<>aValue then
  begin
    FAlignBottom:= aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetNormalColor(const aValue: TColor);
begin
  if FNormalColor<> aValue then
  begin
    FNormalColor := aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetNormalBorderColor(const aValue: TColor);
begin
  if FNormalBorderColor <> aValue then
  begin
    FNormalBorderColor := aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetNormalBorderWidth(const aValue: Integer);
begin
  if FNormalBorderWidth<> aValue then
  begin
    FNormalBorderWidth := aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.SetNormalFontColor(const aValue: TColor);
begin
  if FNormalFontColor<>aValue then
  begin
    FNormalFontColor := aValue;
    Invalidate;
  end;
end;


procedure TAlternateUiButton.SetHoverColor(const aValue: TColor);
begin
  FHoverColor := aValue;
  Invalidate;
end;

procedure TAlternateUiButton.SetHoverBorderColor(const aValue: TColor);
begin
  FHoverBorderColor := aValue;
  Invalidate;
end;

procedure TAlternateUiButton.SetHoverBorderWidth(const aValue: Integer);
begin
  FHoverBorderWidth := aValue;
  Invalidate;
end;

procedure TAlternateUiButton.SetHoverFontColor(const aValue: TColor);
begin
  FHoverFontColor := aValue;
  Invalidate;
end;


procedure TAlternateUiButton.SetDownColor(const aValue: TColor);
begin
  FDownColor := aValue;
  Invalidate;
end;

procedure TAlternateUiButton.SetDownBorderColor(const aValue: TColor);
begin
  FDownBorderColor := aValue;
  Invalidate;
end;

procedure TAlternateUiButton.SetDownBorderWidth(const aValue: Integer);
begin
  FDownBorderWidth := aValue;
  Invalidate;
end;

procedure TAlternateUiButton.SetDownFontColor(const aValue: TColor);
begin
  FDownFontColor := aValue;
  Invalidate;
end;


procedure TAlternateUiButton.SetButtonImage(const aValue: TBitmap);
begin
  FButtonImage.assign(aValue);
  Invalidate;
end;



procedure TAlternateUiButton.SetCaption(const aValue: String);
begin
  if FCaption <> aValue then
  begin
    FCaption := aValue;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.WMLBUTTONDOWN(var Message: TMessage);
begin
  inherited;
  FState := Pressed;
  Invalidate;
end;

procedure TAlternateUiButton.WMLBUTTONUP(var Message: TMessage);
begin
  inherited;
  FState := MouseIn;
  Invalidate;;
end;

procedure TAlternateUiButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

Procedure TAlternateUiButton.CMMouseEnter(var Message: TMessage);
Begin
  inherited;
  if FState <> MouseIn then
  begin
    FState := MouseIn;
    Invalidate;
  end;
end;

Procedure TAlternateUiButton.CMMouseLeave(var Message: TMessage);
Begin
  inherited;
  if FState <> MouseOut then
  begin
    FState := MouseOut;
    Invalidate;
  end;
end;

procedure TAlternateUiButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TAlternateUiButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
end;

Procedure TAlternateUiButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);;
end;
procedure TAlternateUiButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TAlternateUiButton.MouseLeave;
begin
  inherited MouseLeave;
end;
procedure TAlternateUiButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

procedure TAlternateUiButton.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Own', [TAlternateUiButton])
end;

end.

