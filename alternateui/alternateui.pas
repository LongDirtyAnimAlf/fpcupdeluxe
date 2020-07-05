(*
  Alternate User Interface for FPCUPDELUXE.

  It creates the controls on original fpcupdeluxe form
  There is no New form used, as the code creates all the
  relevent controls etc.

  A few hooks are added into fpcupdeluxery.lpi
  using ifdef ie

        {$ifdef usealternateui}
        // This must only be called once.
        If Not Alternate_ui_created then alternateui_Create_Controls;
        {$endif}

  Copyright (C) 2017 Josh

  Contributor(s):
    DonAlfredo

*)

unit alternateui;
{$mode objfpc}
{$H+}
interface
uses
  Classes, SysUtils{$ifdef windows},windows{$endif}{$ifdef usealternateui},Controls, Dialogs,Graphics, Forms, StdCtrls,ExtCtrls,alternateui_button_unit,lresources{$endif};

{$ifdef usealternateui}

Const alternateui_Version='AUI v1.1.9';

Var Alternate_ui_created:boolean=False;

Procedure alternateui_Create_Controls;
procedure alteranteui_LeaveHandler(Sender:TObject);
procedure alteranteui_EnterHandler(Sender:TObject);
procedure alteranteui_ClickHandler(Sender:TObject);
procedure alternateui_update_interface_buttons;
procedure alternateui_AddMessage(const aMessage:string; const UpdateStatus:boolean=false; const aMessageColor:TColor=clBlack);
{$endif}

implementation

{$ifdef usealternateui}
uses
  fpcupdeluxemainform,RGBCIEUtils;

{$R alternateui.res}

const
  name_of_button_for_Form1='alternate_ui_activate_button';
  Master_Panel_Border_Color=$00FFFFFF;
  Master_Panel_One_Button_Border_Color=$000000EC;
  Master_Panel_Custom_Border_Color=$001d911a;
  Master_Panel_Components_Border_Color=$0000E6E6;
  Master_Panel_Cross_Compilers_Border_Color=$00dfdf00;
  Master_Panel_Title_Background_Color=$00000000;
  Master_Panel_Title_Font_Color=$00F9F9F9;
  master_Panel_Info_display_BackGround_Color=$000B0B0B;
  master_Panel_Info_display_BackGround_Color1=$00060606;
  master_Panel_Info_display_Font_Color=$00BBBBBB;
  master_Panel_Info_display_Border_Color=$00BBBBBB;
  icon_size=30;
  drop_arrow_true=True;
  drop_arrow_False=False;
  button_uses_leave_and_enter_true=True;
  button_uses_leave_and_enter_False=False;
  form_BackGround_color=$00080808;
  form_memo_summary_BackGround_color=$00000000;
  form_memo_summary_font_color=$00FFC6C6;
  form_instalDirEdit_BackGround_Color=$00000000;
  form_instalDirEdit_font_Color=$0000D2D2;
  form_statusmessage_BackGround_color=$00000000;
  form_statusmessage_font_color=$00FFC6C6;
  form_realfpc_BackGround_color=$00000000;
  form_realfpc_font_color=$00FFC6C6;
  form_reallaz_BackGround_color=$00000000;
  form_reallaz_font_color=$00FFC6C6;
  button_panel_color=$0044120D;
  shape_border_green=$0000D700;
  shape_border_yellow=$0000D2D2;
  shape_fill_color=$00040000;
  shape_fill_opacity=128;
  shape_border_rounding_size=3;
  select_shape_fill_color=$00221906;
  button_rounding_size=6;
  button_normal_color=$00994022;
  button_normal_font_color=$00FFC6C6;
  button_normal_border_color=$00000000;
  button_normal_border_width=0;
  button_normal_border_use=False;
  button_hover_color=$00B47A66;
  button_hover_font_color=$00010101;
  button_hover_border_color=$00000000;
  button_hover_border_width=0;
  button_hover_border_use=False;
  button_clicked_color=$00003300;
  button_clicked_font_color=$00FFC6C6;
  button_clicked_border_color=$0000D700;
  button_clicked_border_width=2;
  button_clicked_border_use=true;
  button_panel_title_font_color=$00FFFFF;
  button_cursor=crHandPoint;
  Initial_Visibility=true;
  but_gap=4;
  Buttons_Left_margin=8;
  Close_Button_Size=20;
  Number_Of_Languages=2;
  Max_List_Controls=50;

  Shape_Divider_Vertical_Offset=20;

  Control_One_Button_Install='alternateOneButtonInstallerBtn';
  Control_Settings_Button='alternateSettingsBtn';
  Control_FPC_Select_Button='alternateFPCSelectBtn';
  Control_LAZ_Select_Button='alternateLAZSelectBtn';
  Control_FPC_Install_Button='alternateFPCInstallBtn';
  Control_LAZ_Install_Button='alternateLazarusInstallBtn';
  Control_FPC_And_LAZ_Install_Button='alternateFPCandLazarusInstallBtn';
  Control_Cross_Compiler_Select_Button='alternateCrossCompilerSelectBtn';
  Control_Cross_Compiler_Install_Button='alternateCrossCompilerInstallBtn';
  Control_Cross_Compiler_Update_Button='alternateCrossCompilerUpdateBtn';
  Control_Components_Select_Button='alternateComponentsSelectBtn';
  Control_Components_Install_Button='alternateComponentsInstallBtn';
  Control_Components_UnInstall_Button='alternateComponentsUnInstallBtn';
  Control_Install_Directory_Button='alternateInstallDirBtn';
  Control_Auto_Clear_Button='alternateAutoClearBtn';
  Control_Clear_Log_Button='alternateClearLogBtn';
  Control_Display_Help_Text='alternateUIMaster_Info_Display_Text';

  alternateui_max_controls_with_images=20;

  alternateui_MessageDelay=1200;


type
  Control_Type=record
                   //Name:Array[0..127] of AnsiChar;
                   Caption:Array[0..1023] of AnsiChar;
                   Hint:Array[0..1023] of AnsiChar;
                   Info:Array[0..1023] of AnsiChar;
                 end;
  alternateui_Language_Type=Record
                                Code:Array[0..1] of AnsiChar;
                                Flag:Array[0..63] of AnsiChar;
                                hint:Array[0..31] of AnsiChar;
                                Default_Info_Text:Array[0..1023] of AnsiChar;
                                One_Button_Title_Text:Array[0..127] of AnsiChar;
                                Custom_Title_Text:Array[0..127] of AnsiChar;
                                Component_Title_Text:Array[0..127] of AnsiChar;
                                Compiler_Title_Text:Array[0..127] of AnsiChar;
                                Controls:Array[0..Max_List_Controls] of Control_Type;
                              end;

var
    alternateui_Languages:Array[0..Number_Of_Languages] of alternateui_Language_Type;
    alternateui_inuse:boolean=False;
    Info_Display_Default_Text,Settings_Help_Text,OnButtonInstaller_Help_Text,NewUI_Help_Text,SelectFPC_Help_Text,SelectLazarus_Help_Text,InstallFPC_Help_Text:AnsiString;
    InstallLazarus_Help_Text,InstallFPCAndLazarus_Help_Text,SelectCrossCompiler_Help_Text,UpdateCrossCompiler_Help_Text,SelectComponents_Help_Text,SetInstallDireectory_Help_Text:AnsiString;
    AUtoClear_Help_Text,ClearLog_Help_Text,Lang_fpc_target,lang_laz_target:ansistring;

    Pan_List:Array[0..4] of AnsiString=('FPCTarget_','LazTarget_','ComponentSelect_','OneButtonSelect_','alternateUICrossCompiler_');
    But_List:Array[0..4] of Ansistring=(Control_FPC_Select_Button,Control_LAZ_Select_Button,Control_Components_Select_Button,Control_One_Button_Install,Control_Cross_Compiler_Select_Button);


    alternateui_Title:AnsiString='';
    alternateui_Form1_title:AnsiString='';

    alternateui_label_font_size:real=9.0;
    alternateui_button_font_size:real=8.0;

    alternateui_bars_bottom:Integer=556;
    alternateui_bars_name:Array[0..9] of AnsiString=('alternateui_Bar_fpcbootstrap','alternateui_Bar_compiling','alternateui_Bar_linking','alternateui_Bar_make','alternateui_Bar_install','Executing','fpcsrc','Extracted','Other','');
    alternateui_bars_values:Array[0..9] of integer=(0,0,0,0,0,0,0,0,0,0);
    alternateui_bars_left:Array[0..9] of integer=(0,0,0,0,0,0,0,0,0,0);
    alternateui_bars_color:Array[0..9] of TColor=(clRed,TColor($00A5FF),TColor($FF8C00),clAqua,clYellow,clLime,clNavy,TColor($AF10FF),clMaroon,clBlack);

    alternateui_bars_max_height:Integer=190;
    alternateui_bars_increment:integer=4;
    alternateui_bars_start_left:Integer=12;
    alternateui_bars_width:integer=18;
    alternateui_bars_gap:integer=2;

    alternateui_Normal_Button_Color,alternateui_Hover_Button_Color,alternateui_Clicked_Button_Color,alternateui_Disabled_Button_Color:TButtonColorParams;



    {$ifdef unix}           // should be ok for linux and darwin
    {$ifdef darwin}
    alternateui_font_ratio:Real=0.9;
    {$else}
    alternateui_font_ratio:Real=1.0;
    {$endif}
    {$else}
    alternateui_font_ratio:Real=1.0;
    {$endif}

procedure alternateui_set_language(LA:AnsiString);Forward;

function  alternateui_Get_Png_from_resource(AObj:TObject;Res_Name:ShortString):boolean;
var
png : TPortableNetworkGraphic;
vStream : TResourceStream;
begin
  result:=false;
  try
    png:=TPortableNetworkGraphic.Create;
    {$ifdef windows}
    vStream := TResourceStream.Create(HINSTANCE,Res_Name,windows.RT_RCDATA);
    {$else}
    vStream := TResourceStream.Create(HINSTANCE,Res_Name,RT_RCDATA);
    {$endif}
    Png.LoadFromStream(vStream);
    If AObj is TImage then
    begin
      try
        TImage(AObj).Picture.Assign(png)
      finally
        vStream.Free;
      end;
    end
    else
    If AObj is TAlternateUiButton then
    begin
      try
        TAlternateUiButton(AObj).ButtonImage.Assign(png);
      finally
        vStream.Free;
      end;
    end;
    result:=true;
  finally
    Png.Free;
  end;
end;


Function alternateui_IntToString(AInt:Integer):ShortString;
begin
  if AInt<10 then result:='00'+inttostr(aInt)
  else if AInt<100 then result:='0'+inttostr(aInt)
  else result:=inttostr(aint);
end;

function alternateui_animate_panel(na:shortstring;fade_up:boolean):boolean;
begin
  (Form1.FindComponent(na+'Panel') as TPanel).Visible:=fade_up;
  if fade_up then
  begin
    (Form1.FindComponent(na+'Panel') as TPanel).BringToFront;
  end;
  application.processmessages;
end;

procedure alternateui_set_FPCtarget_btn;
begin
  if (Form1.ListBoxFPCTarget.ItemIndex<>-1) then TAlternateUiButton(Form1.FindComponent(Control_FPC_Select_Button)).Caption:=Lang_FPC_target+slinebreak+Form1.ListBoxFPCTarget.Items.Strings[Form1.ListBoxFPCTarget.ItemIndex];
end;

procedure alternateui_set_LAZtarget_btn;
begin
  if (Form1.ListBoxLazarusTarget.ItemIndex<>-1) then TAlternateUiButton(Form1.FindComponent(Control_LAZ_Select_Button)).Caption:=Lang_laz_target+slinebreak+Form1.ListBoxLazarusTarget.Items.Strings[Form1.ListBoxLazarusTarget.ItemIndex];
end;

procedure alternateui_set_OSTarget_btn;
var i:integer=0;
begin
  while Form1.FindComponent('OSTarget_btn'+alternateui_IntToString(i))<>nil do
  begin
    TAlternateUiButton(Form1.FindComponent('OSTarget_btn'+alternateui_IntToString(i))).Down:=i=Form1.radgrpOS.ItemIndex;
    TAlternateUiButton(Form1.FindComponent('OSTarget_btn'+alternateui_IntToString(i))).Enabled:=Form1.radgrpOS.Enabled;
    inc(i);
  end
end;

procedure alternateui_set_CPUTarget_btn;
Var i:Integer=0;
begin
  while Form1.FindComponent('CPUTarget_btn'+alternateui_IntToString(i))<>nil do
  begin
    TAlternateUiButton(Form1.FindComponent('CPUTarget_btn'+alternateui_IntToString(i))).Down:=i=Form1.radgrpCPU.ItemIndex;
    TAlternateUiButton(Form1.FindComponent('CPUTarget_btn'+alternateui_IntToString(i))).Enabled:=Form1.radgrpCPU.Enabled;
    inc(i);
  end;
end;


procedure alternateui_set_Selected_Components(AKeep:Boolean);
Var i:Integer=0;
begin
  while Form1.FindComponent('ComponentSelect_btn'+alternateui_IntToString(i))<>nil do
  begin
    if Akeep=false then Form1.ListModules.Selected[i]:=false;
    TAlternateUiButton(Form1.FindComponent('ComponentSelect_btn'+alternateui_IntToString(i))).Down:=Form1.ListModules.Selected[i];
    inc(i);
  end;
end;

procedure alternateui_update_laz_fpc_popup_options;
var
  i:integer;
begin
  i:=0;
  while form1.findcomponent('FPCTarget_btn'+alternateui_IntToString(i)) <> nil do
  begin
    with form1.findcomponent('FPCTarget_btn'+alternateui_IntToString(i)) as TAlternateUiButton do
    begin
      if Form1.ListBoxFPCTarget.ItemIndex<>-1 then down:=caption=Form1.ListBoxFPCTarget.Items.Strings[Form1.ListBoxFPCTarget.ItemIndex];
    end;
    inc(i);
  end;
  i:=0;
  while form1.findcomponent('LAZTarget_btn'+alternateui_IntToString(i)) <> nil do
  begin
    with form1.findcomponent('LAZTarget_btn'+alternateui_IntToString(i)) as TAlternateUiButton do
    begin
      if Form1.ListBoxLazarusTarget.ItemIndex<>-1 then down:=caption=Form1.ListBoxLazarusTarget.Items.Strings[Form1.ListBoxLazarusTarget.ItemIndex];
    end;
    inc(i);
  end;
end;

procedure alternateui_update_interface_buttons;
begin
  if alternateui_inuse then
  begin
    // set the cross compiler options
    alternateui_set_OSTarget_btn;
    alternateui_set_CPUTarget_btn;
    alternateui_set_FPCtarget_btn;
    alternateui_set_Laztarget_btn;
    alternateui_set_Selected_Components(True);

    alternateui_update_laz_fpc_popup_options;
  end;
end;

procedure alternateui_toggle_new_ui;
var disp_control:boolean;
    i:integer;
begin
  alternateui_inuse:=not alternateui_inuse;
  disp_control:=not alternateui_inuse;
  if alternateui_inuse then
  begin
    Form1.Caption:=alternateui_title;
    Form1.listModules.MultiSelect:=true;
    Form1.Color:=form_BackGround_color;
    Form1.memoSummary.Color:=form_memo_summary_BackGround_color;
    Form1.memoSummary.Font.Color:=form_memo_summary_font_color;
    Form1.Constraints.MinWidth:=1052;
    Form1.Constraints.MinHeight:=631;
    Form1.DoubleBuffered:=true;
    Form1.InstallDirEdit.Color:=form_instalDirEdit_BackGround_Color;
    Form1.InstallDirEdit.Font.Color:=form_instalDirEdit_font_Color;
    Form1.StatusMessage.Color:=form_instalDirEdit_BackGround_Color;
    Form1.StatusMessage.font.Color:=form_instalDirEdit_font_Color;
    Form1.RealFPCURL.Color:=form_realfpc_BackGround_color;
    Form1.RealFPCURL.Font.Color:=form_realfpc_font_color;
    Form1.RealLAZURL.Color:=form_reallaz_BackGround_color;
    Form1.RealLAZURL.Font.Color:=form_reallaz_font_color;
    Form1.CheckAutoClear.AnchorSideRight.Control:=(Form1.FindComponent(Control_Clear_Log_Button) as TAlternateUiButton);
    Form1.StatusMessage.AnchorSideRight.Control:=(Form1.FindComponent(Control_Auto_Clear_Button) as TAlternateUiButton);
    Form1.InstallDirEdit.AnchorSideRight.Control:=(Form1.FindComponent(Control_Install_Directory_Button) as TAlternateUiButton);
    with (Form1.FindComponent(Control_Auto_Clear_Button) as TAlternateUiButton) do
    begin
      down:=Form1.CheckAutoClear.Checked;
      //adjust glyph
      if Down then alternateui_Get_Png_from_resource(Form1.FindComponent(Control_Auto_Clear_Button) as TAlternateUiButton,'AUI_GREEN_TICK_SMALL')
      else alternateui_Get_Png_from_resource(Form1.FindComponent(Control_Auto_Clear_Button) as TAlternateUiButton,'AUI_RED_CROSS_SMALL');
    end;
    // set the cross compiler options
    alternateui_update_interface_buttons;
{    alternateui_set_OSTarget_btn;
    alternateui_set_CPUTarget_btn;
    alternateui_set_FPCtarget_btn;
    alternateui_set_Laztarget_btn;
    alternateui_set_Selected_Components(True);}
  end
  else
  begin
    Form1.Caption:=alternateui_Form1_title;
    Form1.listModules.MultiSelect:=False;
    Form1.Color:=clDefault;
    Form1.memoSummary.Color:=clDefault;
    Form1.memoSummary.Font.Color:=clDefault;
    Form1.Constraints.MinWidth:=0;
    Form1.Constraints.MinHeight:=0;
    Form1.InstallDirEdit.Color:=clDefault;
    Form1.InstallDirEdit.Font.Color:=clRed;
    Form1.StatusMessage.Color:=clDefault;
    Form1.StatusMessage.font.Color:=clDefault;
    Form1.RealFPCURL.Color:=clDefault;
    Form1.RealFPCURL.Font.Color:=clDefault;
    Form1.RealLAZURL.Color:=clDefault;
    Form1.RealLAZURL.Font.Color:=clDefault;
    Form1.CheckAutoClear.AnchorSideRight.Control:=Form1.btnClearLog;
    Form1.StatusMessage.AnchorSideRight.Control:=Form1.CheckAutoClear;
    Form1.InstallDirEdit.AnchorSideRight.Control:=Form1.btnInstallDirSelect;
    // hide all panels
    for i:=0 to 4 do
    begin
      alternateui_animate_panel(pan_list[i],False);
      application.ProcessMessages;
    end;
    for i:=0 to 4 do TAlternateUiButton(Form1.FindComponent(but_list[i])).Down:=False;

  end;
  if not alternateui_inuse then alternateui_animate_panel('alternateUIMaster_',alternateui_inuse);

  (Form1.FindComponent(Control_Install_Directory_Button) as TAlternateUiButton).Visible:=Not Disp_control;
  (Form1.FindComponent(Control_Clear_Log_Button) as TAlternateUiButton).Visible:=Not Disp_control;
  (Form1.FindComponent(Control_Auto_Clear_Button) as TAlternateUiButton).Visible:=Not Disp_control;

  Form1.btnInstallDirSelect.Visible:=disp_control;
  Form1.btnClearLog.Visible:=disp_control;
  Form1.CheckAutoClear.Visible:=disp_control;
  Form1.Panel1.Visible:=disp_control;
  Form1.PageControl1.Visible:=disp_control;
  Form1.btnSetupPlus.Visible:=disp_control;
  if alternateui_inuse then alternateui_animate_panel('alternateUIMaster_',alternateui_inuse);
  Form1.Invalidate;
  application.ProcessMessages;
end;

procedure alteranteui_EnterHandler(Sender:TObject);
  var na:shortstring;
begin
  if Sender is TAlternateUiButton then
  begin
    (form1.FindComponent(Control_Display_Help_Text) as TLabel).font.Size:=round(alternateui_label_font_size*alternateui_font_ratio);
    na:=TAlternateUiButton(Sender).Name;
    with Form1.FindComponent(Control_Display_Help_Text) as TLabel do
    begin
      case na of
        Control_One_Button_Install    :Caption:=OnButtonInstaller_Help_Text;
        Control_Settings_Button              :Caption:=Settings_Help_Text;
        Control_FPC_Select_Button             :Caption:=SelectFPC_Help_Text;
        Control_LAZ_Select_Button             :Caption:=SelectLazarus_Help_Text;
        Control_FPC_Install_Button            :Caption:=InstallFPC_Help_Text;
        Control_LAZ_Install_Button        :Caption:=InstallLazarus_Help_Text;
        Control_FPC_And_LAZ_Install_Button  :Caption:=InstallFPCAndLazarus_Help_Text;
        Control_Cross_Compiler_Select_Button   :Caption:=SelectCrossCompiler_Help_Text;
        Control_Cross_Compiler_Update_Button   :Caption:=UpdateCrossCompiler_Help_Text;
        Control_Components_Select_Button      :Caption:=SelectComponents_Help_Text;
        Control_Install_Directory_Button            :Caption:=SetInstallDireectory_Help_Text;
        Control_Auto_Clear_Button             :Caption:=AUtoClear_Help_Text;
        Control_Clear_Log_Button              :Caption:=ClearLog_Help_Text;
      end;
    end;
  end;
  If Sender is TImage then
  Begin
    na:=TImage(Sender).name;
    if na=name_of_button_for_Form1 then
    begin
      (Form1.FindComponent(Control_Display_Help_Text) as TLabel).Caption:=NewUI_Help_Text;
      alternateui_Get_Png_from_resource(Form1.FindComponent(name_of_button_for_Form1) as TImage,'AUI_NEW_UI_HOVER');
    end;
  end;
end;

procedure alteranteui_LeaveHandler(Sender:TObject);
Begin
   If Sender is TAlternateUiButton then (Form1.FindComponent(Control_Display_Help_Text) as TLabel).Caption:=Info_Display_Default_Text;
   If Sender is TIMage then
   begin
     if TImage(sender).name=name_of_button_for_Form1 then
     begin
       (Form1.FindComponent(Control_Display_Help_Text) as TLabel).Caption:=Info_Display_Default_Text;
       alternateui_Get_Png_from_resource(Form1.FindComponent(name_of_button_for_Form1) as TImage,'AUI_NEW_UI');
     end;
   end;
end;

Procedure alternateui_Display_Hide_Panels(pn:shortstring;disp_pan:boolean);
Var I:Integer;
Begin
  for i:=0 to 4 do
  begin
    if pan_list[i]<>'' then
    begin
      if pan_list[i]=pn then
      begin
        alternateui_animate_panel(pan_list[i],true);
        TAlternateUiButton(Form1.FindComponent(but_list[i])).Down:=disp_pan;
      end
    end;
  end;
  for i:=0 to 4 do
  begin
    if pan_list[i]<>'' then
    begin
      if pan_list[i]<>pn then
      begin
        alternateui_animate_panel(pan_list[i],False);
        TAlternateUiButton(Form1.FindComponent(but_list[i])).Down:=False;
      end
    end;
  end;
  if Form1.FindComponent(pn+'Panel')<> nil then
  begin
  with (Form1.FindComponent(pn+'Panel')) as tpanel do
  begin
    Visible:=disp_pan;
    if disp_pan then bringtofront;
  end;
end;
end;

procedure alternateui_update_bar(Avalue:Integer);
var
  aBarComponent:TComponent;
begin
  aBarComponent:=Form1.FindComponent(alternateui_bars_name[Avalue]);
  if NOT Assigned(aBarComponent) then exit;
  inc(alternateui_bars_values[Avalue],alternateui_bars_increment);
  if alternateui_bars_values[Avalue]>alternateui_bars_max_height then alternateui_bars_values[Avalue]:=0;
  tshape(aBarComponent).SetBounds(alternateui_bars_left[aValue],alternateui_bars_bottom-(alternateui_bars_max_height div 2)-(alternateui_bars_values[aValue] div 2),alternateui_bars_width,alternateui_bars_values[aValue]);
end;

procedure alternateui_show_progress_bars(Vis:Boolean);
var i:integer;
begin
  for i:=0 to 9 do
  begin
    alternateui_bars_values[i]:=0;
    if alternateui_bars_name[i]<>'' then
    begin
      tshape(form1.FindComponent(alternateui_bars_name[i])).Visible:=Vis;
      if vis then
      begin
        tshape(form1.FindComponent(alternateui_bars_name[i])).BringToFront;
        alternateui_update_bar(i);
      end;
    end;
  end;
end;

procedure alternateui_ShowMessage(MyMessage:AnsiString);
begin
  with (form1.FindComponent(Control_Display_Help_Text) as TLabel) do
  begin
    font.Size:=round((alternateui_label_font_size*alternateui_font_ratio)*1.5);
    caption:=MyMessage;
    application.ProcessMessages;
    (form1.FindComponent('alternateuiHalt') as TImage).Visible:=False;
    alternateui_show_progress_bars(False);
  end;
end;

procedure alteranteui_ClickHandler(Sender:TObject);
var co:integer=0;
    na:shortstring;
    sender_name:shortstring;
    itm:integer;
    i:integer;
begin
  If Sender is TAlternateUiButton then
  begin
    sender_Name:=TAlternateUiButton(sender).Name;
    na:=copy(sender_name,1,length(sender_name)-3);
    if ((na='FPCTarget_btn') or (na='LazTarget_btn')) then
    begin
      while Form1.FindComponent(na+alternateui_IntToString(co))<>nil do
      begin
        TAlternateUiButton(Form1.FindComponent(na+alternateui_IntToString(co))).down:=False;
        inc(co);
      end;
      with sender as TAlternateUiButton do
      begin
        down:=true;
        if na='FPCTarget_btn' then
        begin
          itm:=strtoint(copy(TAlternateUiButton(sender).Name,length(TAlternateUiButton(sender).Name)-2,3));
          Form1.FPCTarget:=caption;
          Form1.ListBoxFPCTarget.ItemIndex:=itm;
          alternateui_set_FPCtarget_btn;
        end;
        if na='LazTarget_btn' then
        begin
          itm:=strtoint(copy(TAlternateUiButton(sender).Name,length(TAlternateUiButton(sender).Name)-2,3));
          Form1.LazarusTarget:=caption;
          Form1.ListBoxLazarusTarget.ItemIndex:=itm;
          alternateui_set_Laztarget_btn;
     end;
      end;
    end
    else
    if na='CPUTarget_btn' then
    begin
      itm:=strtoint(copy(TAlternateUiButton(sender).Name,length(TAlternateUiButton(sender).Name)-2,3));
      Form1.radgrpCPU.ItemIndex:=itm;
      application.ProcessMessages;
      alternateui_set_CPUTarget_btn;
      application.ProcessMessages;
      alternateui_set_OSTarget_btn
    end
    else
    if na='OSTarget_btn' then
    begin
      itm:=strtoint(copy(TAlternateUiButton(sender).Name,length(TAlternateUiButton(sender).Name)-2,3));
      Form1.radgrpOS.ItemIndex:=itm;
      application.ProcessMessages;
      alternateui_set_OSTarget_btn;
      application.ProcessMessages;
      alternateui_set_CPUTarget_btn
    end
    else
    if na='OneButtonSelect_btn' then
    begin
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      itm:=strtoint(copy(TAlternateUiButton(sender).Name,length(TAlternateUiButton(sender).Name)-2,3));
      TAlternateUiButton(sender).Down:=true;
      alternateui_Display_Hide_Panels('Blank',false);
      case itm of
        0:Form1.QuickBtnClick(Form1.TrunkBtn);
        1:Form1.QuickBtnClick(Form1.FixesBtn);
        2:Form1.QuickBtnClick(Form1.StableBtn);
        3:Form1.QuickBtnClick(Form1.OldBtn);
        4:Form1.QuickBtnClick(Form1.mORMotBtn);
        5:Form1.BitBtnHaltClick(Form1.BitBtnHalt);
      end;
      (Form1.FindComponent(Control_One_Button_Install) as TAlternateUiButton).Down:=False;
      TAlternateUiButton(sender).Down:=False;
      (Form1.FindComponent('OneButtonSelect_Panel') as TPanel).Visible:=False;
      alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_Settings_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      application.ProcessMessages;
      Form1.btnSetupPlusClick(Form1.btnSetupPlus);
      TAlternateUiButton(sender).Down:=False;
    end
    else
    if sender_name=Control_Install_Directory_Button then
    begin
      Form1.btnInstallDirSelectClick(Form1.btnInstallDirSelect);
    end
    else
    if sender_name=Control_Clear_Log_Button then
    begin
      Form1.btnClearLogClick(Form1.btnClearLog);
    end
    else
    if sender_name=Control_Auto_Clear_Button then
    begin
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      // now adjust the glyph
      if TAlternateUiButton(sender).Down then alternateui_Get_Png_from_resource(Form1.FindComponent(sender_name),'AUI_GREEN_TICK_SMALL')
      else alternateui_Get_Png_from_resource(Form1.FindComponent(sender_name),'AUI_RED_CROSS_SMALL');
      Form1.CheckAutoClear.Checked:=TAlternateUiButton(sender).Down;
    end
    else
    if sender_name=Control_One_Button_Install then
    begin
      // Display or remove One Button Installer menu
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      alternateui_Display_Hide_Panels('OneButtonSelect_',TAlternateUiButton(sender).Down);
    end
    else
    if sender_name=Control_FPC_Select_Button then
    begin
      // Display or remove FPCTARGETR menu
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      alternateui_Display_Hide_Panels('FPCTarget_',TAlternateUiButton(sender).Down);
    end
    else
    if sender_name=Control_LAZ_Select_Button then
    begin
      // Display or remove LAZTARGET menu
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      alternateui_Display_Hide_Panels('LazTarget_',TAlternateUiButton(sender).Down);
    end
    else
    if sender_name=Control_Components_Select_Button then
    begin
      // Display or remove LAZTARGET menu
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      alternateui_Display_Hide_Panels('ComponentSelect_',TAlternateUiButton(sender).Down);
    end
    else
    if sender_name=Control_FPC_Install_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      application.ProcessMessages;
      Form1.InstallClick(Form1.BitBtnFPCOnly);
      TAlternateUiButton(sender).Down:=False;
      alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_LAZ_Install_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      application.ProcessMessages;
      Form1.InstallClick(Form1.BitBtnLazarusOnly);
      TAlternateUiButton(sender).Down:=False;
      alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_FPC_And_LAZ_Install_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      application.ProcessMessages;
      Form1.InstallClick(Form1.BitBtnFPCandLazarus);
      TAlternateUiButton(sender).Down:=False;
      alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_Components_Install_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      (Form1.FindComponent('ComponentSelect_Panel') as TPanel).Visible:=False;
      alternateui_Display_Hide_Panels('Blank',false);
      application.ProcessMessages;
      Form1.btnInstallModuleClick(Form1.btnInstallModule);
      TAlternateUiButton(sender).Down:=False;
      // now remove any selected buttons
       alternateui_set_Selected_Components(False);
       alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_Components_UnInstall_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      (Form1.FindComponent('ComponentSelect_Panel') as TPanel).Visible:=False;
      alternateui_Display_Hide_Panels('Blank',false);
      application.ProcessMessages;
      Form1.btnInstallModuleClick(Form1.btnUninstallModule);
      TAlternateUiButton(sender).Down:=False;
      // now remove any selected buttons
       alternateui_set_Selected_Components(False);
       alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_Cross_Compiler_Select_Button then
    begin
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      alternateui_Display_Hide_Panels('alternateUICrossCompiler_',TAlternateUiButton(sender).Down);
    end
    else
    if sender_name=Control_Cross_Compiler_Install_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      alternateui_Display_Hide_Panels('Blank',false);
      application.ProcessMessages;
      Form1.ButtonProcessCrossCompiler(Form1.ButtonInstallCrossCompiler);
      TAlternateUiButton(sender).Down:=false;
      alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    if sender_name=Control_Cross_Compiler_Update_Button then
    begin
      TAlternateUiButton(sender).Down:=true;
      (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_show_progress_bars(True);
      application.ProcessMessages;
      Form1.ButtonAutoUpdateCrossCompiler(Form1.AutoCrossUpdate);
      TAlternateUiButton(sender).Down:=False;
      alternateui_ShowMessage(form1.StatusMessage.Text);
    end
    else
    begin
      //multi-select toggle
      TAlternateUiButton(sender).Down:=Not TAlternateUiButton(sender).Down;
      if na='ComponentSelect_btn' then
      begin
        itm:=strtoint(copy(TAlternateUiButton(sender).Name,length(TAlternateUiButton(sender).Name)-2,3));
        Form1.listmodules.Selected[itm]:=TAlternateUiButton(sender).Down;
      end;
    end;
  end
  else
  begin
    If Sender is TImage then
    begin
      sender_Name:=TImage(sender).Name;
      // check to see if its a close button
      if upcase(copy(sender_name,length(sender_name)-4,5))='CLOSE' then
      begin
        na:=copy(sender_name,1,pos('_',sender_name));
        alternateui_animate_panel(na,False);
        (Form1.FindComponent(na+'panel') as tpanel).Visible:=False;
        // also set the activating button to up
        for i:=0 to 4 do
        begin
          if pan_list[i]=na then TAlternateUiButton(Form1.FindComponent(but_list[i])).Down:=False;
        end;
      end
      else
      begin
        If sender_name=name_of_button_for_Form1 then alternateui_toggle_new_ui;
        if sender_name='alternateuiHalt' then Form1.BitBtnHaltClick(Form1.BitBtnHalt);
        case sender_name of
          'alternateuiLangEN':alternateui_set_language('EN');
          'alternateuiLangRU':alternateui_set_language('RU');
          'alternateuiLangFR':alternateui_set_language('FR');
        end;
      end;
    end;
  end;
  (Form1.FindComponent('alternateuiHalt') as TImage).Visible:=False;
  alternateui_show_progress_bars(False);
end;


Procedure alternateui_create_color_shape(Ctrl_name:ansistring;L,T,W,H:Integer;BackColor:TColor;Panel_name:ansistring;shape_fill_opacity:Integer);
begin
  // Create the Shape / Border
  with tshape.Create(Form1) do
  begin
    setbounds(L,T,W,H);
  //  Angle:=45;
    pen.Style:=psClear;
    pen.Width:=1;
    Shape:=stRectangle;
    brush.Color:=BackColor;
    brush.Style:=bsSolid;
    name:=Ctrl_name;
    parent:=(form1.FindComponent(Panel_name) as TPanel);
    visible:=false;
    BringToFront;
  end;

end;

procedure alternateui_Create_Button_Container(base_name:string;base_left,base_top,base_width,base_height:integer;base_title,base_Parent:shortstring;base_close:boolean;Panel_Color,Shape_color,Shape_Border_Color,Shape_Border_Width,Label_Color,Label_Font_Color,Label_Border_Color:TColor;Label_Border_Width:Integer;label_bold,Label_Clear_Color:Boolean);
begin
  with tpanel.Create(Form1) do
  begin
    SetBounds(base_Left,base_top,base_width,base_height);
    color:=Panel_Color;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderWidth:=0;
    BorderStyle:=bsNone;
    Name:=base_name+'Panel';
    doublebuffered:=true;
    if Form1.FindComponent(base_Parent) is TPanel then
    begin
      Parent:=(Form1.FindComponent(base_Parent) as TPanel);
      Visible:=true;
    end
    else
    begin
      Parent:=Form1;
      Visible:=Not Initial_Visibility;
    end;
    caption:='';
    bringtofront;
  end;
  if base_title<>'' then
  begin
    // create the title
    with TLabel.Create(Form1) do
    begin
      SetBounds(Label_Border_Width,shape_border_rounding_size,base_width-(Label_Border_Width*2),Close_Button_Size);
      color:=Label_Color;
      AutoSize:=False;
      //enabled:=False;
      if label_bold then Font.Style:=[fsbold]
      else Font.Style:=[];
      Font.Quality:=fqCleartypeNatural;
      Font.Color:=Label_Font_Color;
      Alignment:=taCenter;
      Font.Size:=round(alternateui_label_font_size*alternateui_font_ratio);
      Name:=base_name+'Title';
      Parent:=(Form1.FindComponent(base_name+'Panel') as TPanel);
      caption:=base_title;
      Visible:=true;
      transparent:=true;
      bringtofront;
    end;
  end;
  If Label_Border_Width>0 then
  begin
    // create a shape to act as the divider
    with Tshape.Create(Form1) do
    begin
      SetBounds(0,Shape_Divider_Vertical_Offset,base_width,Label_Border_Width);
      pen.Color:=Shape_Border_Color;
      pen.style:=psSolid;
      pen.Width:=Shape_Border_Width;
      Shape:=stRectangle;
      brush.Color:=Shape_Border_Width;
      brush.Style:=bsSolid;
      Name:=base_name+'Divider';
      Parent:=(Form1.FindComponent(base_name+'Panel') as TPanel);
      Visible:=true;
      sendtoback;
    end;
  end;
  // Create the Shape / Border
  with Tshape.Create(Form1) do
  begin
    Align:=alClient;
    //SetBounds(0,0,base_width,base_height);
    pen.Color:=Shape_Border_Color;
    pen.style:=psSolid;
    pen.Width:=Shape_Border_Width;
    Shape:=stRectangle;
    brush.Color:=Shape_color;
    brush.Style:=bsSolid;
    Name:=base_name+'Shape';
    Parent:=(Form1.FindComponent(base_name+'Panel') as TPanel);
    Visible:=true;
    sendtoback;
  end;
  if base_close then
  begin
    // Create the Close Button
    with TImage.Create(Form1) do
    begin
     // picture.Bitmap.LoadFromLazarusResource('CLOSE_BUTTON');
      Parent:=(Form1.FindComponent(base_name+'Panel') as TPanel);
      AntiAliasingMode:=amOn;
      Stretch:=True;
      Cursor:=button_Cursor;
      OnClick:=@Form1.alternateuibutClick;
      Name:=base_name+'Close';
      SetBounds(base_Width-Close_Button_Size-shape_border_rounding_size,shape_border_rounding_size,Close_Button_Size,Close_Button_Size);
    end;
    alternateui_Get_Png_from_resource(Form1.FindComponent(base_name+'Close') as TImage,'AUI_CLOSE_BUTTON');
  end;
end;

procedure alternateui_create_a_button(but_name:string;but_left,but_top,but_width,but_height:integer;but_caption,but_Parent:shortstring;but_drop_arrow,but_use_mouse_enter:boolean;but_glyph,but_add_image:ansistring;but_add_image_width,but_add_image_height:integer);
begin
  with TAlternateUiButton.Create(Form1) do
  begin
    SetBounds(But_Left,but_top,but_width,but_height);
    down:=false;
    normal:=alternateui_Normal_Button_Color;
    hover:=alternateui_Hover_Button_Color;
    if but_name=Control_One_Button_Install then
    begin
      normal.BackgroundColor:=$002e6200;
      hover.BackgroundColor:=$005e9200;
      //normal.FontColor:=clLime;
    end;
    clicked:=alternateui_Clicked_Button_Color;
    disabled:=alternateui_Disabled_Button_Color;

    Font.Quality:=fqCleartypeNatural;
    Font.Size:=round(alternateui_button_font_size*alternateui_font_ratio);
    Font.Style:=[];
    DropDownArrow:=but_drop_arrow;
    if DropDownArrow then dropdownarrowcolor:=clwhite;
    AlignCenter:=true;
    AlignBottom:=False;

    if Form1.FindComponent(but_Parent) is TPanel then Parent:=(Form1.FindComponent(but_Parent) as TPanel)
    else Parent:=Form1;
    caption:=but_caption;
    Name:=but_name;
    OnClick:=@Form1.alternateuibutClick;//(self);
    if but_use_mouse_enter then
    begin
      OnMouseEnter:=@Form1.alternateuibutEnter;
      OnMouseLeave:=@Form1.alternateuibutLeave;
    end;
    Cursor:=button_Cursor;
    Visible:=true;
    if but_glyph<>'' then
    begin
      imageLeft:=true;
      imagemargin:=4;
    end;
    if but_add_image<>'' then
    begin
      imageLeft:=false;
      imagemargin:=2;
      AlignCenter:=true;
      AlignBottom:=true;
    end;
  end;
  if but_glyph<>'' then alternateui_Get_Png_from_resource(Form1.FindComponent(but_name),but_glyph);
  if but_add_image<>'' then alternateui_Get_Png_from_resource(Form1.FindComponent(but_name),but_add_image);
end;

procedure alternateui_Create_Buttons_for_Container(base_btn:shortstring;btn_caps_list:TListBox;btn_caps_list_from_radio:TRadioGroup;title_caption:shortstring;How_Many_Across,Height_Of_Buttons,Width_Of_Buttons:Integer);

var control_base_name:shortstring='';
    ALoop:Integer;
    sy:Integer;
    hm:Integer;
    pan_width,pan_height:integer;
    but_count:integer=0;
    itm_count:integer;
    b_panel:string='';
    need_close:boolean=true;
    Panel_border:tcolor;
begin
  panel_border:=Master_Panel_Components_Border_Color;
  control_base_Name:=copy(base_btn,1,pos('_',base_btn));
  if ((control_base_name='CPUTarget_') or (control_base_name='OSTarget_')) then
  begin
    b_panel:='alternateUICrossCompiler_Panel';
    need_close:=False;
    panel_border:=Master_Panel_Cross_Compilers_Border_Color;
  end
  else b_panel:=control_base_name;
  alternateui_Create_Button_Container(control_base_name,150,60,200,100,title_caption,b_panel,need_close,button_panel_color,shape_fill_color,panel_border,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Components_Border_Color,0,False,true);
  // Create the Actual Button
  but_count:=0;
  itm_count:=0;
  b_panel:=control_base_name+'Panel';//control_base_name+'Panel';
  if btn_caps_list=nil then itm_count:=btn_caps_list_from_radio.Items.Count
  else itm_count:=btn_caps_list.Count;
  If itm_count<>0 then
  begin
    sy:=shape_border_rounding_size+Close_Button_Size+2;//buttons_top_margin;
    hm:=itm_count-1;
    if base_btn='OneButtonSelect_btn' then hm:=7;
    for aloop:=0 to hm do
    begin
     // with tbcbutton.Create(Form1) do
      begin
        if but_count=How_many_Across then
        begin
          sy:=sy+Height_Of_Buttons+but_gap;
          but_count:=0;
        end;
        if btn_caps_list=nil then alternateui_create_a_button(base_btn+alternateui_IntToString(aloop),Buttons_Left_margin+((Width_Of_Buttons+but_gap)*but_count),sy,Width_Of_Buttons,Height_Of_Buttons,btn_caps_list_from_radio.Items.Strings[aloop],b_panel,False,False,'','',0,0)
        else alternateui_create_a_button(base_btn+alternateui_IntToString(aloop),Buttons_Left_margin+((Width_Of_Buttons+but_gap)*but_count),sy,Width_Of_Buttons,Height_Of_Buttons,btn_caps_list.Items[aloop],b_panel,False,False,'','',0,0);
        inc(but_count);
        with  Form1.FindComponent(base_btn+alternateui_IntToString(aloop)) as TAlternateUiButton do
        begin
          if btn_caps_list=nil then Down :=btn_caps_list_from_radio.ItemIndex=aloop
          else Down :=btn_caps_list.Selected[aloop];
        end;
     end;
    end;
   //adjust the panel dimensions;
    with  Form1.FindComponent(control_base_name+'Panel') as TPanel do
    begin
      pan_height:=sy+Height_Of_Buttons+4+but_gap;//height;
      pan_width:=Buttons_Left_margin+((Width_Of_Buttons+but_gap)*How_Many_Across)+Buttons_Left_margin-but_gap;//width;
      SetBounds(left,top,pan_width,pan_height);
    end;
    //assign image to close button
    if Form1.FindComponent(control_base_name+'Close')<>nil then
    begin
      with  Form1.FindComponent(control_base_name+'Close') as TImage do
      begin
        SetBounds(Pan_Width-Close_Button_Size-shape_border_rounding_size,shape_border_rounding_size,Close_Button_Size,Close_Button_Size);
        bringtofront;
      end;
    end;
    // adjust posution based on panel dimensions
    if Form1.FindComponent(control_base_name+'Title')<>nil then
    begin
      with  Form1.FindComponent(control_base_name+'Title') as TLabel do
      begin
        SetBounds(Close_Button_Size+2,shape_border_rounding_size,pan_width-((close_button_size+2)*2),close_button_size);
      end;
    end;
    with  Form1.FindComponent(control_base_name+'Shape') as TShape do
    begin
      SetBounds(0,0,pan_width,pan_height);
      sendtoback;
    end;
  end;
end;

procedure alternateui_AddMessage(const aMessage:string; const UpdateStatus:boolean=false; const aMessageColor:TColor=clBlack);
var
  i,j,olddist,newdist,r,g,b : byte;
  l1, a1, b1: double;
  l2, a2, b2: double;
  DeltaE,OldDeltaE:double;
begin
  if Length(amessage)=0 then exit;

  RGBToLab(aMessageColor, l1, a1, b1);

  j:=0;
  OldDeltaE:=MaxInt;
  for i:=0 to 9 do
  begin
    if aMessageColor=alternateui_bars_color[i] then
    begin
      j:=i;
      break;
    end;
    RGBToLab(alternateui_bars_color[i], l2, a2, b2);
    DeltaE:=CalcDeltaE(l1, a1, b1, l2, a2, b2);
    if DeltaE<OldDeltaE then
    begin
      j:=i;
      OldDeltaE:=DeltaE;
    end;
  end;
  if (Alternate_ui_created) then alternateui_update_bar(j);
  {
  if Alternate_ui_created then
  begin
    if amessage<>'' then
    begin
      if Pos('pcbootstrap',amessage)>0 then alternateui_update_bar(0)
      else
        if Pos('ompiling',amessage)>0 then alternateui_update_bar(1)
        else
          if Pos('inking',amessage)>0 then alternateui_update_bar(2)
          else
            if Pos('ake',amessage)>0 then alternateui_update_bar(3)
            else
              if Pos('nstall',amessage)>0 then alternateui_update_bar(4)
              else
                if Pos('xecuting',amessage)>0 then alternateui_update_bar(5)
                else
                  if Pos('pcsrc',amessage)>0 then alternateui_update_bar(6)
                  else
                    if Pos('xtracted',amessage)>0 then alternateui_update_bar(7)
                    else
                      alternateui_update_bar(8);



    end;
  end;}
end;

procedure alternateui_add_title_and_glyph(bm:ansistring;gl:ansistring;ca:ansistring;hi:ansistring);
begin
  with (Form1.FindComponent(bm) as TAlternateUiButton) do
  begin
    caption:=ca;
    hint:=hi;
    if hi<>'' then showhint:=true;
    if gl='' then ButtonImage:=nil
    else
    begin
      imageLeft:=true;
      if gl='AUI_GO' then imagemargin:=4
      else imagemargin:=1;
      alternateui_Get_Png_from_resource(Form1.FindComponent(bm) as TAlternateUiButton,gl);

    end;
  end;
end;

procedure alternateui_set_text_variables;
begin
  alternateui_Languages[0].Code:='EN';
  alternateui_Languages[0].hint:='English';
  alternateui_Languages[0].Flag:='AUI_FLAG_EN';
  alternateui_Languages[0].One_Button_Title_Text:='Quick Installer';
  alternateui_Languages[0].Custom_Title_Text:='Custom Installation';
  alternateui_Languages[0].Component_Title_Text:='Install Additional Modules/Components';
  alternateui_Languages[0].Compiler_Title_Text:='Install Cross Compilers';
  alternateui_Languages[0].Default_Info_Text:='Welcome to FPCUPDeluxery'+slinebreak+'==============='+slinebreak+'This allows you to install sandboxed versions of Free Pascal/Lazarus; this allows you to have multiple different version of Lazarus and Free Pascal on your system'+slinebreak+slinebreak+'You can also install cross compilers to allow easy cross compiling to various different CPU and OS Targets'+slinebreak+slinebreak+'You can also install various additional components into your Lazarus Installations';
  with alternateui_Languages[0] do
  begin
    //controls[0].Name:=Control_One_Button_Install;
    controls[0].Hint:='';
    controls[0].Caption:='One Button Installer';
    controls[0].Info:='One Button Installer'+slinebreak+'================'+slinebreak+'Allows you to quickly install a Sandboxed version of Free Pascal / Lazarus';
    //controls[1].Name:=Control_Settings_Button;
    controls[1].Hint:='';
    controls[1].Caption:='Settings';
    controls[1].Info:='Settings'+slinebreak+'================'+slinebreak+'This will display a form that will allow you adjust the Configuration of FPCUPDeluxe';
    //controls[2].Name:=Control_FPC_Select_Button;
    controls[2].Hint:='';
    controls[2].Caption:='FPC Version';
    controls[2].Info:='Select the Free Pascal Compiler you wish to Install';
    //controls[3].Name:=Control_LAZ_Select_Button;
    controls[3].Hint:='';
    controls[3].Caption:='Lazarus Version';
    controls[3].Info:='Select the Lazarus Version you wish to Install';
    //controls[4].Name:=Control_FPC_Install_Button;
    controls[4].Hint:='';
    controls[4].Caption:='Install FPC';
    controls[4].Info:='Install the Selected version of Free Pascal';
    //controls[5].Name:=Control_LAZ_Install_Button;
    controls[5].Hint:='';
    controls[5].Caption:='Install Lazarus';
    controls[5].Info:='Install the Selected version of Lazarus';
    //controls[6].Name:=Control_FPC_And_LAZ_Install_Button;
    controls[6].Hint:='';
    controls[6].Caption:='Install FPC & Lazarus';
    controls[6].Info:='Install Both the Selected Version of Free Pascal and Lazarus';
    //controls[7].Name:=Control_Cross_Compiler_Select_Button;
    controls[7].Hint:='';
    controls[7].Caption:='Cross Compiler';
    controls[7].Info:='Select and Install the Cross Compiler you wish to Install';
    //controls[8].Name:=Control_Cross_Compiler_Update_Button;
    controls[8].Hint:='';
    controls[8].Caption:='Update Compilers';
    controls[8].Info:='Auto Update any Cross Compilers you have previously Installed';
    //controls[9].Name:=Control_Components_Select_Button;
    controls[9].Hint:='';
    controls[9].Caption:='Select Components';
    controls[9].Info:='Select and Install Modules and Components to add to your Installation';
    //controls[10].Name:=Control_Install_Directory_Button;
    controls[10].Hint:='';
    controls[10].Caption:='Set InstDir';
    controls[10].Info:='Set the Directory that FPCUPDELUXE Will use to Install/Update your Installatiions';
    //controls[11].Name:=Control_Auto_Clear_Button;
    controls[11].Hint:='';
    controls[11].Caption:='Auto Clear';
    controls[11].Info:='Clear the Log Screen when New Commands are Started';
    //controls[12].Name:=Control_Clear_Log_Button;
    controls[12].Hint:='';
    controls[12].Caption:='Clear Log';
    controls[12].Info:='Clear the Log Screen Now';
    //controls[13].Name:=Control_Clear_Log_Button;
    controls[13].Hint:='';
    controls[13].Caption:='Clear Log';
    controls[13].Info:='Switches between Classic and New Interface';
  end;
  // French
  alternateui_Languages[1].Code:='FR';
  alternateui_Languages[1].hint:='French';
  alternateui_Languages[1].Flag:='AUI_FLAG_FR';
  alternateui_Languages[1].One_Button_Title_Text:='Installateur rapide';
  alternateui_Languages[1].Custom_Title_Text:='Installation customisée';
  alternateui_Languages[1].Component_Title_Text:='Installer des modules / composants supplémentaires';
  alternateui_Languages[1].Compiler_Title_Text:='Installer des compilateurs croisés';
  alternateui_Languages[1].Default_Info_Text:='Bienvenue sur FPCUPDeluxery'+slinebreak+'==============='+slinebreak+'Cela vous permet d'+chr(39)+'installer des versions en bac à sable de Pascal libre / Lazarus; cela vous permet d'+chr(39)+'avoir plusieurs version différente de Lazarus et Free Pascal sur votre système.'+slinebreak+slinebreak+'Vous pouvez également installer des compilateurs croisés pour faciliter la compilation à divers différents CPU et OS cibles.'+slinebreak+slinebreak+'Vous pouvez également installer divers composants supplémentaires dans votre Installations Lazarus.';
  with alternateui_Languages[1] do
  begin
    //controls[0].Name:=Control_One_Button_Install;
    controls[0].Hint:='';
    controls[0].Caption:='Un bouton d'+chr(39)+'installation';
    controls[0].Info:='Un bouton d'+chr(39)+'installation'+slinebreak+'================'+slinebreak+'Vous permet d'+chr(39)+'installer rapidement une version Sandboxed de Free Pascal / Lazarus';
    //controls[1].Name:=Control_Settings_Button;
    controls[1].Hint:='';
    controls[1].Caption:='Paramètres';
    controls[1].Info:='Paramètres'+slinebreak+'================'+'Cela affichera un formulaire qui vous permettra d'+chr(39)+'ajuster la configuration de FPCUPDeluxe';
    //controls[2].Name:=Control_FPC_Select_Button;
    controls[2].Hint:='';
    controls[2].Caption:='Version FPC';
    controls[2].Info:='Sélectionnez le compilateur Free Pascal que vous souhaitez installer';
    //controls[3].Name:=Control_LAZ_Select_Button;
    controls[3].Hint:='';
    controls[3].Caption:='Lazarus Version';
    controls[3].Info:='Sélectionnez la version de Lazarus que vous souhaitez installer';
    //controls[4].Name:=Control_FPC_Install_Button;
    controls[4].Hint:='';
    controls[4].Caption:='Installer FPC';
    controls[4].Info:='Installez la version sélectionnée de Free Pascal';
    //controls[5].Name:=Control_LAZ_Install_Button;
    controls[5].Hint:='';
    controls[5].Caption:='Installer Lazarus';
    controls[5].Info:='Installez la version sélectionnée de Lazarus';
    //controls[6].Name:=Control_FPC_And_LAZ_Install_Button;
    controls[6].Hint:='';
    controls[6].Caption:='Installez FPC & Lazarus';
    controls[6].Info:='Installer à la fois la version sélectionnée de Free Pascal et Lazarus';
    //controls[7].Name:=Control_Cross_Compiler_Select_Button;
    controls[7].Hint:='';
    controls[7].Caption:='Cross Compiler';
    controls[7].Info:='Sélectionnez et installez le compilateur croisé que vous souhaitez installer';
    //controls[8].Name:=Control_Cross_Compiler_Update_Button;
    controls[8].Hint:='';
    controls[8].Caption:='Update Compilers';
    controls[8].Info:='Mise à jour automatique de tous les compilateurs croisés que vous avez déjà installés';
    //controls[9].Name:=Control_Components_Select_Button;
    controls[9].Hint:='';
    controls[9].Caption:='Sélectionner des composants';
    controls[9].Info:='Sélectionnez et installez les modules et les composants à ajouter à votre installation';
    //controls[10].Name:=Control_Install_Directory_Button;
    controls[10].Hint:='';
    controls[10].Caption:='Set InstDir';
    controls[10].Info:='Définir le répertoire que FPCUPDELUXE utilisera pour installer / mettre à jour vos installations';
    //controls[11].Name:=Control_Auto_Clear_Button;
    controls[11].Hint:='';
    controls[11].Caption:='Auto Clear';
    controls[11].Info:='Effacer l'+chr(39)+'écran du journal lorsque de nouvelles commandes sont démarrées';
    //controls[12].Name:=Control_Clear_Log_Button;
    controls[12].Hint:='';
    controls[12].Caption:='Clear Log';
    controls[12].Info:='Effacer l'+chr(39)+'écran du journal maintenant';
    //controls[13].Name:=Control_Clear_Log_Button;
    controls[13].Hint:='';
    controls[13].Caption:='Clear Log';
    controls[13].Info:='Bascule entre Classic et New Interface';
  end;
  // Russian
  alternateui_Languages[2].Code:='RU';
  alternateui_Languages[2].hint:='Russian';
  alternateui_Languages[2].Flag:='AUI_FLAG_RU';
  alternateui_Languages[2].One_Button_Title_Text:='Быстрый установщик';
  alternateui_Languages[2].Custom_Title_Text:='Выборочная установка';
  alternateui_Languages[2].Component_Title_Text:='Установка дополнительных модулей / компонентов';
  alternateui_Languages[2].Compiler_Title_Text:='Установка кросс-компиляторов';
  alternateui_Languages[2].Default_Info_Text:='Добро пожаловать в FPCUPDeluxery'+slinebreak+'==============='+slinebreak+'Это позволяет вам устанавливать изолированные Свободный Паскаль / Лазарь; это позволяет вам иметь несколько другая версия Lazarus и Free Pascal в вашей системе.'+slinebreak+slinebreak+'Вы также можете установить кросс-компиляторы, чтобы упростить крест компиляция для различных целей ЦП и ОС.'+slinebreak+slinebreak+'Вы также можете установить в Установки Лазаря.';
  with alternateui_Languages[2] do
  begin
    //controls[0].Name:=Control_One_Button_Install;
    controls[0].Hint:='';
    controls[0].Caption:='1 Установщик кнопок';
    controls[0].Info:='Установщик одной кнопки'+slinebreak+'================'+slinebreak+'Позволяет быстро установить версию Sandboxed Free Pascal / Lazarus.';
    //controls[1].Name:=Control_Settings_Button;
    controls[1].Hint:='';
    controls[1].Caption:='настройки';
    controls[1].Info:='настройки'+slinebreak+'================'+slinebreak+'Это отобразит форму, которая позволит вам'+slinebreak+'настройте конфигурацию FPCUPDeluxe.';
    //controls[2].Name:=Control_FPC_Select_Button;
    controls[2].Hint:='';
    controls[2].Caption:='Версия FPC';
    controls[2].Info:='Выберите компилятор Free Pascal, который вы хотите установить.';
    //controls[3].Name:=Control_LAZ_Select_Button;
    controls[3].Hint:='';
    controls[3].Caption:='Версия Lazarus';
    controls[3].Info:='Выберите версию Lazarus, которую вы хотите установить.';
   // controls[4].Name:=Control_FPC_Install_Button;
    controls[4].Hint:='';
    controls[4].Caption:='Установка FPC';
    controls[4].Info:='Установите выбранную версию Free Pascal';
    //controls[5].Name:=Control_LAZ_Install_Button;
    controls[5].Hint:='';
    controls[5].Caption:='Установить Lazarus';
    controls[5].Info:='Установите выбранную версию Lazarus';
    //controls[6].Name:=Control_FPC_And_LAZ_Install_Button;
    controls[6].Hint:='';
    controls[6].Caption:='Установка FPC & Lazarus';
    controls[6].Info:='Установите как выбранную версию Free Pascal, так и Lazarus';
    //controls[7].Name:=Control_Cross_Compiler_Select_Button;
    controls[7].Hint:='';
    controls[7].Caption:='Кросс-компилятор';
    controls[7].Info:='Выберите и установите кросс-компилятор, который вы хотите установить.';
    //controls[8].Name:=Control_Cross_Compiler_Update_Button;
    controls[8].Hint:='';
    controls[8].Caption:='Обновить компиляторы';
    controls[8].Info:='Автоматическое обновление любых кросс-компиляторов, которые вы ранее установили';
    //controls[9].Name:=Control_Components_Select_Button;
    controls[9].Hint:='';
    controls[9].Caption:='Выберите компоненты';
    controls[9].Info:='Выбор и установка модулей и компонентов для добавления к вашей установке';
    //controls[10].Name:=Control_Install_Directory_Button;
    controls[10].Hint:='';
    controls[10].Caption:='Set InstDir';
    controls[10].Info:='Установите каталог, который FPCUPDELUXE будет использовать для установки / обновления настроек';
    //controls[11].Name:=Control_Auto_Clear_Button;
    controls[11].Hint:='';
    controls[11].Caption:='Auto Clear';
    controls[11].Info:='Очистить экран журнала при запуске новых команд';
    //controls[12].Name:=Control_Clear_Log_Button;
    controls[12].Hint:='';
    controls[12].Caption:='Clear Log';
    controls[12].Info:='Очистить экран журнала';
    //controls[13].Name:=Control_Clear_Log_Button;
    controls[13].Hint:='';
    controls[13].Caption:='';
    controls[13].Info:='Переключение между классическим и новым интерфейсом';
  end;

end;

Procedure alternateui_set_Button_Values(Bm:ShorTstRING;lang_num,idx:Integer);
begin
  with Form1.FindComponent(bm) as TAlternateUiButton do
  begin
    caption:=alternateui_Languages[lang_num].Controls[idx].Caption;
    hint:=alternateui_Languages[lang_num].Controls[idx].hint;
  end;
end;

procedure alternateui_set_language(LA:AnsiString);
var itm:Integer=-1;
    i:Integer;
begin
  for i:=0 to Number_Of_Languages do if alternateui_Languages[i].Code=LA then itm:=i;
  if itm<>-1 then
  begin
    Lang_laz_target:=alternateui_Languages[itm].Controls[3].Caption;
    Lang_fpc_target:=alternateui_Languages[itm].Controls[2].Caption;
    with alternateui_Languages[itm] do
    begin
      Info_Display_Default_Text:=Default_Info_Text;
      OnButtonInstaller_Help_Text:=controls[0].Info;
      alternateui_Set_Button_Values(Control_One_Button_Install,itm,0);
      Settings_Help_Text:=controls[1].Info;
      alternateui_Set_Button_Values(Control_Settings_Button,itm,1);
      SelectFPC_Help_Text:=controls[2].Info;
      alternateui_Set_Button_Values(Control_FPC_Select_Button,itm,2);
      SelectLazarus_Help_Text:=controls[3].Info;
      alternateui_Set_Button_Values(Control_LAZ_Select_Button,itm,3);
      InstallFPC_Help_Text:=controls[4].Info;
      alternateui_Set_Button_Values(Control_FPC_Install_Button,itm,4);
      InstallLazarus_Help_Text:=controls[5].Info;
      alternateui_Set_Button_Values(Control_LAZ_Install_Button,itm,5);
      InstallFPCAndLazarus_Help_Text:=controls[6].Info;
      alternateui_Set_Button_Values(Control_FPC_And_LAZ_Install_Button,itm,6);
      SelectCrossCompiler_Help_Text:=controls[7].Info;
      alternateui_Set_Button_Values(Control_Cross_Compiler_Select_Button,itm,7);
      UpdateCrossCompiler_Help_Text:=controls[8].Info;
      alternateui_Set_Button_Values(Control_Cross_Compiler_Update_Button,itm,8);
      SelectComponents_Help_Text:=controls[9].Info;
      alternateui_Set_Button_Values(Control_Components_Select_Button,itm,9);
      SetInstallDireectory_Help_Text:=controls[10].Info;
      alternateui_Set_Button_Values(Control_Install_Directory_Button,itm,10);
      AUtoClear_Help_Text:=controls[11].Info;
      alternateui_Set_Button_Values(Control_Auto_Clear_Button,itm,11);
      ClearLog_Help_Text:=controls[12].Info;
      alternateui_Set_Button_Values(Control_Clear_Log_Button,itm,12);
      NewUI_Help_Text:=controls[13].Info;
      // adjust button title captions
      TLabel(Form1.FindComponent('alternateUIOneButBox_Title')).Caption:=One_Button_Title_Text;
      TLabel(Form1.FindComponent('alternateUICustomInstallationBox_Title')).Caption:=Custom_Title_Text;
      TLabel(Form1.FindComponent('alternateUIComponentInstallationBox_Title')).Caption:=Component_Title_Text;
      TLabel(Form1.FindComponent('alternateUICrossCompilerBox_Title')).Caption:=Compiler_Title_Text;
      IF La='EN' then alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangEN') as TImage,'AUI_LANG_EN_ON') else alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangEN') as TImage,'AUI_LANG_EN');
      IF La='RU' then alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangRU') as TImage,'AUI_LANG_RU_ON') else alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangRU') as TImage,'AUI_LANG_RU');
      IF La='FR' then alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangFR') as TImage,'AUI_LANG_FR_ON') else alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangFR') as TImage,'AUI_LANG_FR');
    end;
    (Form1.FindComponent(Control_Display_Help_Text) as TLabel).Caption:=Info_Display_Default_Text;
    alternateui_set_FPCtarget_btn;
    alternateui_set_Laztarget_btn;
    application.ProcessMessages;
  end;
end;

Procedure alternateui_Create_Controls;
Const  lm=10;
       wi=352;
var    flagt:integer;
       flagl:integer;
       le,i:integer;

begin
  // Create my Default Button Color Scheme
  alternateui_Normal_Button_Color.BackgroundColor:=button_normal_color;
  alternateui_Normal_Button_Color.BorderColor:=button_normal_border_color;
  alternateui_Normal_Button_Color.BorderWidth:=button_normal_border_width;
  alternateui_Normal_Button_Color.FontColor:=button_normal_font_color;
  alternateui_Normal_Button_Color.ArrowColor:=button_normal_font_color;
  alternateui_Normal_Button_Color.UseGradient:=true;

  alternateui_Hover_Button_Color.BackgroundColor:=button_hover_color;
  alternateui_Hover_Button_Color.BorderColor:=button_hover_border_color;
  alternateui_Hover_Button_Color.BorderWidth:=button_hover_border_width;
  alternateui_Hover_Button_Color.FontColor:=button_hover_font_color;
  alternateui_Hover_Button_Color.ArrowColor:=button_hover_font_color;
  alternateui_Hover_Button_Color.UseGradient:=true;

  alternateui_Clicked_Button_Color.BackgroundColor:=button_clicked_color;
  alternateui_Clicked_Button_Color.BorderColor:=button_clicked_border_color;
  alternateui_Clicked_Button_Color.BorderWidth:=button_clicked_border_width;
  alternateui_Clicked_Button_Color.FontColor:=button_clicked_font_color;
  alternateui_Clicked_Button_Color.ArrowColor:=button_clicked_font_color;
  alternateui_Clicked_Button_Color.UseGradient:=true;

  alternateui_Disabled_Button_Color.BackgroundColor:=$007a7a7a;
  alternateui_Disabled_Button_Color.BorderColor:=$008b8b8b;
  alternateui_Disabled_Button_Color.BorderWidth:=0;
  alternateui_Disabled_Button_Color.FontColor:=$00a4a4a4;
  alternateui_Disabled_Button_Color.ArrowColor:=$00a4a4a4;
  alternateui_Disabled_Button_Color.UseGradient:=False;

  // Memorize Title
  alternateui_Form1_title:=Form1.Caption;
  if Pos('base',alternateui_form1_title)>0 then alternateui_title:=StringReplace(alternateui_form1_title,'base','('+AlternateUi_Version+' [B v'+AlternateUiButton_Version+']) base',[rfReplaceAll])
  else alternateui_title:=StringReplace(alternateui_form1_title,'for','('+AlternateUi_Version+' [B v'+AlternateUiButton_Version+']) for',[rfReplaceAll]);

  // create a button on Form1 to activate interface
  with TImage.Create(Form1) do
  begin
    Parent:=Form1;//(Form1.FindComponent(control_base_name+'Panel') as TPanel);
    AntiAliasingMode:=amOn;
    Stretch:=True;
    Cursor:=button_Cursor;
    OnClick:=@Form1.alternateuibutClick;
    OnMouseEnter:=@Form1.alternateuibutEnter;
    OnMouseLeave:=@Form1.alternateuibutLeave;
    Name:=name_of_button_for_Form1;//
    SetBounds(Form1.btnInstallDirSelect.Left+Form1.btnInstallDirSelect.Width+12,4,trunc(Close_Button_Size*1.5),trunc(Close_Button_Size*1.5));
    showhint:=true;
    hint:='Switch between New/Classic User Interface.'
  end;
  alternateui_Get_Png_from_resource(Form1.FindComponent(name_of_button_for_Form1) as TImage,'AUI_NEW_UI');

   // create the buttons, panels, etc required for the popup menus.
  alternateui_Create_Buttons_for_Container('FPCTarget_btn',Form1.ListBoxFPCtarget,nil,'Please select the Free Pascal Compiler you wish to Install',7,18,120);
  alternateui_Create_Buttons_for_Container('LazTarget_btn',Form1.ListBoxLazarusTarget,nil,'Please select the Lazarus you wish to Install',7,18,120);
  alternateui_Create_Buttons_for_Container('ComponentSelect_btn',Form1.ListModules,nil,'Please select the Component(s) you wish to install.',7,18,120);
  alternateui_Create_Buttons_for_Container('OneButtonSelect_btn',Form1.ListModules,nil,'',8,34,120);

  // adjust panel positions and adjust any colours required
  with  Form1.FindComponent('OneButtonSelect_Panel') as TPanel do setbounds(14,116,width,height);
  with  Form1.FindComponent('OneButtonSelect_Shape') as TShape do
  begin
    pen.Color:=button_clicked_border_color;
    brush.Color:=select_shape_fill_color;
  end;

  with  Form1.FindComponent('FPCTarget_Panel') as TPanel do setbounds(14,182,width,height);
  with  Form1.FindComponent('FPCTarget_Shape') as TShape do
  begin
    pen.Color:=button_clicked_border_color;
    brush.Color:=select_shape_fill_color;
  end;

  with  Form1.FindComponent('LAZTarget_Panel') as TPanel do setbounds(14,182,width,height);
  with  Form1.FindComponent('LAZTarget_Shape') as TShape do
  begin
    pen.Color:=button_clicked_border_color;
    brush.Color:=select_shape_fill_color;
  end;

  with  Form1.FindComponent('ComponentSelect_Shape') as TShape do
  begin
    pen.Color:=button_clicked_border_color;
    brush.Color:=select_shape_fill_color;
  end;

  // Add Titles to OneButton
  alternateui_add_title_and_glyph('OneButtonSelect_btn000','AUI_TRUNK',Form1.trunkbtn.Caption,Form1.TrunkBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn001','AUI_FIXES',Form1.FixesBtn.Caption,Form1.FixesBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn002','AUI_STABLE',Form1.StableBtn.Caption,Form1.StableBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn003','AUI_OLD',Form1.OldBtn.Caption,Form1.OldBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn004','AUI_MORMOT',Form1.mORMotBtn.Caption,Form1.mORMotBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn005','AUI_HALT',Form1.BitBtnHalt.Caption,Form1.BitBtnHalt.Hint);

  //Make room for UIMaster Panel
  Form1.RealFPCURL.Left:=Form1.PageControl1.Left+360+8;

  //Create UIMaster Panel that is the parent to other Box Panels
  alternateui_Create_Button_Container('alternateUIMaster_',Form1.PageControl1.Left,Form1.CommandOutputScreen.Top,360,Form1.CommandOutputScreen.Height,'','',False,button_panel_color,shape_fill_color,Master_Panel_Border_Color,2,master_Panel_Info_display_BackGround_Color,Master_Panel_Title_Font_Color,master_Panel_Info_display_Border_Color,0,False,False);
  with (Form1.FindComponent('alternateUIMaster_Panel') as TPanel) do
  begin
    AnchorSideLeft.Control := Form1.InstallDirEdit;
    AnchorSideTop.Control := Form1.InstallDirEdit;
    AnchorSideTop.Side := asrBottom;
    //AnchorSideBottom.Control := Form1.CommandOutputScreen;
    AnchorSideBottom.Control := Form1.memoSummary;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akTop, akLeft, akBottom];
    BorderSpacing.Top := 20;
  end;

  //Create Panels to Hold Functional Buttons
  alternateui_Create_Button_Container('alternateUIOneButBox_',110,2,140,80,'Quick Installer','alternateUIMaster_Panel',False,button_panel_color,shape_fill_color,Master_Panel_One_Button_Border_Color,4,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_One_Button_Border_Color,2,False,False);
  alternateui_Create_Button_Container('alternateUICustomInstallationBox_',4,(tpanel(Form1.FindComponent('alternateUIOneButBox_Panel')).top)+(tpanel(Form1.FindComponent('alternateUIOneButBox_Panel')).height)+4,wi,114,'Custom Installation','alternateUIMaster_Panel',False,button_panel_color,shape_fill_color,Master_Panel_Custom_Border_Color,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Custom_Border_Color,1,False,False);
  alternateui_Create_Button_Container('alternateUIComponentInstallationBox_',4,
  (tpanel(form1.FindComponent('alternateUICustomInstallationBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUICustomInstallationBox_Panel')).height)+4,
  wi,76,'Install Additional Components','alternateUIMaster_Panel',false,button_panel_color,shape_fill_color,Master_Panel_Components_Border_Color,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Components_Border_Color,1,false,false);
  alternateui_Create_Button_Container('alternateUICrossCompilerBox_',4,(tpanel(Form1.FindComponent('alternateUIComponentInstallationBox_Panel')).top)+(tpanel(Form1.FindComponent('alternateUIComponentInstallationBox_Panel')).height)+4,wi,76,'Custom Cross Compilers','alternateUIMaster_Panel',False,button_panel_color,shape_fill_color,Master_Panel_Cross_Compilers_Border_Color,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Cross_Compilers_Border_Color,1,False,False);

  // Create Panels for Cross Compiler
  alternateui_Create_Button_Container('alternateUICrossCompiler_',370,40,600,564,'Select and Install Cross Compilers','',true,button_panel_color,shape_fill_color,Master_Panel_Border_Color,1,master_Panel_Info_display_BackGround_Color,Master_Panel_Title_Font_Color,master_Panel_Info_display_Border_Color,0,False,true);
  alternateui_Create_Buttons_for_Container('CPUTarget_btn',nil,Form1.radgrpCPU,'Select CPU',1,18,120);
  alternateui_Create_Buttons_for_Container('OSTarget_btn',nil,Form1.radgrpOS,'Select OS',1,18,120);
  with  Form1.FindComponent('alternateUICrossCompiler_Shape') as TShape do
  begin
    pen.Color:=button_clicked_border_color;
    brush.Color:=select_shape_fill_color;
    pen.Width:=2;
  end;
  with Form1.FindComponent('CPUTarget_Panel') as TPanel do setbounds(20,30,width,height);
  with Form1.FindComponent('OSTarget_Panel') as TPanel do setbounds((20+TPanel(Form1.FindComponent('CPUTarget_Panel')).Width+20),30,width,height);
  with Form1.FindComponent('alternateUICrossCompiler_Panel') as TPanel do
  begin
    left:=138;
    top:=12;
    width:=TPanel(Form1.FindComponent('OSTarget_Panel')).Left+TPanel(Form1.FindComponent('OSTarget_Panel')).Width+20;
    //check to see if os or cpu panel is taller and set position panel accordingly.
    if TPanel(Form1.FindComponent('CPUTarget_Panel')).Height>TPanel(Form1.FindComponent('OSTarget_Panel')).Height then height:=TPanel(Form1.FindComponent('CPUTarget_Panel')).Height+80
    else height:=TPanel(Form1.FindComponent('OSTarget_Panel')).Height+80;
    TShape(Form1.FindComponent('alternateUICrossCompiler_Shape')).Height:=height;
    TShape(Form1.FindComponent('alternateUICrossCompiler_Shape')).Width:=Width;
    TLabel(Form1.FindComponent('alternateUICrossCompiler_Title')).Width:=Width;
    Timage(Form1.FindComponent('alternateUICrossCompiler_Close')).left:=Width-Close_Button_Size-shape_border_rounding_size
  end;

  // Centralize the Components Panel
  //with  form1.FindComponent('ComponentSelect_Panel') as TPanel do setbounds(138,((tpanel(form1.FindComponent('alternateUIMaster_Panel')).Height-Height) div 2),width,height);
  with  form1.FindComponent('ComponentSelect_Panel') as TPanel do setbounds(138,12,width,height);

  // Create the Lower Notes Label Control
  with TLabel.Create(Form1) do
  begin
    SetBounds(4,
    (tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+4,
    wi,
    (tpanel(Form1.FindComponent('alternateUIMaster_Panel')).height)-((tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+4)-4);
    Color:=master_Panel_Info_display_BackGround_Color1;
    AutoSize:=False;
    enabled:=False;
    wordwrap:=true;
    Font.Style:=[];
    Font.Size:=round(alternateui_label_font_size*alternateui_font_ratio);
    Font.Quality:=fqCleartypeNatural;
    Font.Color:=master_Panel_Info_display_Font_Color;
    Alignment:=taCenter;
    Name:='alternateUIMaster_Info_Display';
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);
    caption:='';//Info_Display_Default_Text;
    Visible:=true;
    transparent:=true;
    bringtofront;
  end;
  with TLabel.Create(Form1) do
  begin
    SetBounds(8,
    (tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+8,
    wi-8,
    (tpanel(Form1.FindComponent('alternateUIMaster_Panel')).height)-((tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(Form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+4)-12);
    Color:=master_Panel_Info_display_BackGround_Color;
    AutoSize:=False;
    enabled:=False;
    Wordwrap:=True;
    Font.size:=round(alternateui_label_font_size*alternateui_font_ratio);
    Font.Style:=[];
    Font.Quality:=fqCleartypeNatural;
    Font.Color:=master_Panel_Info_display_Font_Color;
    Alignment:=taCenter;
    Name:=Control_Display_Help_Text;
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);
    caption:=Info_Display_Default_Text;
    Visible:=true;
    transparent:=true;
    bringtofront;
  end;

  // Now add buttons to Containers
  // One Button Panel and Settings Button
  alternateui_create_a_button(Control_Settings_Button,wi-96-2,32,96,48,'Settings','alternateUIMaster_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_SETTINGS',icon_size,icon_size);
  alternateui_create_a_button(Control_One_Button_Install,10,26,120,48,'One Button Installer','alternateUIOneButBox_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_QUICK',icon_size,icon_size);

  // Custom Installation Section
  alternateui_create_a_button(Control_FPC_Select_Button,10,24,120,32,'FPC Version','alternateUICustomInstallationBox_Panel',drop_arrow_True,button_uses_leave_and_enter_true,'','',0,0);
  alternateui_create_a_button(Control_LAZ_Select_Button,wi-120-lm,24,120,32,'Lazarus Version','alternateUICustomInstallationBox_Panel',drop_arrow_True,button_uses_leave_and_enter_true,'','',0,0);
  alternateui_create_a_button(Control_FPC_Install_Button,10,62,70,48,'Install FPC','alternateUICustomInstallationBox_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_FPC',icon_size,icon_size);
  alternateui_create_a_button(Control_LAZ_Install_Button,wi-70-lm,62,70,48,'Install Lazarus','alternateUICustomInstallationBox_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_LAZ',icon_size,icon_size);
  alternateui_create_a_button(Control_FPC_And_LAZ_Install_Button,(wi div 2)-(120 div 2),62,120,48,'Install FPC & Lazarus','alternateUICustomInstallationBox_Panel',False,button_uses_leave_and_enter_true,'','AUI_FPC_LAZ',110,icon_size);
  with (Form1.FindComponent(Control_FPC_Select_Button) as TAlternateUiButton) do
  begin
    Caption:='FPC Version'+slinebreak+'';
  end;
  with (Form1.FindComponent(Control_LAZ_Select_Button) as TAlternateUiButton) do
  begin
    Caption:='Lazarus Version'+slinebreak+'';
  end;

  // Components Section
  alternateui_create_a_button(Control_Components_Select_Button,(wi div 2)-(120 div 2),24,120,48,'Select Components','alternateUIComponentInstallationBox_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_MODULES',icon_size,icon_size);

  alternateui_create_a_button(Control_Components_Install_Button,((tpanel(Form1.FindComponent('ComponentSelect_Panel')).width) div 2)-144,tpanel(Form1.FindComponent('ComponentSelect_Panel')).height,140,40,'Install Modules','ComponentSelect_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','',0,0);
  with (Form1.FindComponent(Control_Components_Install_Button) as TAlternateUiButton) do
  begin
    Caption:='Install'+slinebreak+'Modules';
  end;
  alternateui_add_title_and_glyph(Control_Components_Install_Button,'AUI_GO','Install Modules',Form1.btnInstallModule.Hint);

  alternateui_create_a_button(Control_Components_UnInstall_Button,((tpanel(Form1.FindComponent('ComponentSelect_Panel')).width) div 2) +4,tpanel(Form1.FindComponent('ComponentSelect_Panel')).height,140,40,'UnInstall Modules','ComponentSelect_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','',0,0);
  with (Form1.FindComponent(Control_Components_UnInstall_Button) as TAlternateUiButton) do
  begin
    Caption:='UnInstall'+slinebreak+'Modules';
  end;
  alternateui_add_title_and_glyph(Control_Components_UnInstall_Button,'AUI_UNINSTALL','UnInstall Modules',Form1.btnUninstallModule.Hint);

  tpanel(Form1.FindComponent('ComponentSelect_Panel')).height:=tpanel(Form1.FindComponent('ComponentSelect_Panel')).height+48;
  TShape(Form1.FindComponent('ComponentSelect_Shape')).height:=TShape(Form1.FindComponent('ComponentSelect_Shape')).height+48;

  // Cross Compilers
  alternateui_create_a_button(Control_Cross_Compiler_Select_Button,10,24,120,48,'Cross Compiler','alternateUICrossCompilerBox_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_CROSS_COMPILER',110,icon_size);
  alternateui_create_a_button(Control_Cross_Compiler_Install_Button,((tpanel(Form1.FindComponent('alternateUICrossCompiler_Panel')).Width)-140) div 2,tpanel(Form1.FindComponent('alternateUICrossCompiler_Panel')).Height-46,140,40,'Install Compiler','alternateUICrossCompiler_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','',0,0);
  alternateui_add_title_and_glyph(Control_Cross_Compiler_Install_Button,'AUI_GO','Install Compiler',Form1.ButtonInstallCrossCompiler.Hint);
  alternateui_create_a_button(Control_Cross_Compiler_Update_Button,wi-120-lm,24,120,48,'Update Compilers','alternateUICrossCompilerBox_Panel',drop_arrow_False,button_uses_leave_and_enter_true,'','AUI_UPDATE',icon_size,icon_size);
  Alternate_ui_created:=true;

  // create buttons to replace normal buttons
  alternateui_create_a_button(Control_Install_Directory_Button,Form1.btnInstallDirSelect.Left,Form1.btnInstallDirSelect.Top,Form1.btnInstallDirSelect.Width,Form1.btnInstallDirSelect.Height,'Set InstDir','',drop_arrow_False,button_uses_leave_and_enter_true,'AUI_EXCLAMATION','',0,0);
  (Form1.FindComponent(Control_Install_Directory_Button) as TAlternateUiButton).Visible:=False;
  alternateui_create_a_button(Control_Clear_Log_Button,Form1.btnClearLog.Left,Form1.btnClearLog.Top,Form1.btnClearLog.Width,Form1.btnClearLog.Height,'Clear Log','',drop_arrow_False,button_uses_leave_and_enter_true,'AUI_CLEAR','',0,0);

  with (Form1.FindComponent(Control_Clear_Log_Button) as TAlternateUiButton) do
  begin
    Visible:=False;
    //AnchorSideRight.Control:=Owner;
    AnchorSideRight.Control:=Form1;
    AnchorSideRight.Side := asrBottom;
    BorderSpacing.Right:=12;
    Anchors := [akRight];
  end;

  alternateui_create_a_button(Control_Auto_Clear_Button,Form1.CheckAutoClear.Left,Form1.btnClearLog.Top,Form1.CheckAutoClear.Width+4,Form1.btnClearLog.Height,'Auto Clear','',drop_arrow_False,button_uses_leave_and_enter_true,'AUI_GREEN_TICK_SMALL','',0,0);

  with (Form1.FindComponent(Control_Auto_Clear_Button) as TAlternateUiButton) do
  begin
    Visible:=False;
    Down:=True;
    AnchorSideRight.Control:=(Form1.FindComponent(Control_Clear_Log_Button) as TAlternateUiButton);
    AnchorSideLeft.Side := asrBottom;
    BorderSpacing.Right:=12;
    Anchors := [akRight];
  end;

  // create image for logo
  with TImage.Create(Form1) do
  begin
    SetBounds(10,00,90,90);
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(Form1.FindComponent(control_base_name+'Panel') as TPanel);
    Name:='alternateuiLogo';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    Visible:=true;
  end;
  alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLogo') as TImage,'AUI_GLOBE');

  // create image for HALT
  with TImage.Create(Form1) do
  begin
    SetBounds(110,390,140,140);
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(Form1.FindComponent(control_base_name+'Panel') as TPanel);
    Name:='alternateuiHalt';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    Visible:=False;
    Cursor:=button_Cursor;
    OnClick:=@Form1.alternateuibutClick;
  end;
  alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiHalt') as TImage,'AUI_STOP');

  // create image for FLAGS
  flagt:=4;
  flagl:=wi-96-2;
  with TImage.Create(Form1) do
  begin
    SetBounds(flagl,flagt,24,16);
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(Form1.FindComponent(control_base_name+'Panel') as TPanel);
    Name:='alternateuiLangEN';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    Visible:=true;
    Cursor:=button_Cursor;
    OnClick:=@Form1.alternateuibutClick;
    showhint:=true;
    hint:='English';
  end;
  alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangEN') as TImage,'AUI_LANG_EN');
  flagl:=flagl+24;
  with TImage.Create(Form1) do
  begin
    SetBounds(flagl,flagt,24,16);
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(Form1.FindComponent(control_base_name+'Panel') as TPanel);
    Name:='alternateuiLangRU';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    Visible:=true;
    Cursor:=button_Cursor;
    OnClick:=@Form1.alternateuibutClick;
    showhint:=true;
    hint:='русский';
  end;
  alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangRU') as TImage,'AUI_LANG_RU');
  flagl:=flagl+24;
  with TImage.Create(Form1) do
  begin
    SetBounds(flagl,flagt,24,16);
    Parent:=(Form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(Form1.FindComponent(control_base_name+'Panel') as TPanel);
    Name:='alternateuiLangFR';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    Visible:=true;
    Cursor:=button_Cursor;
    OnClick:=@Form1.alternateuibutClick;
    showhint:=true;
    hint:='Français';
  end;
  alternateui_Get_Png_from_resource(Form1.FindComponent('alternateuiLangFR') as TImage,'AUI_LANG_FR');

  //Set Help Text;
  alternateui_set_text_variables;
  alternateui_set_language('EN');

  //Add some bars for progress      in     alternateUIMaster_Info_Display
  le:=10;
  for i:=0 to 9 do
  begin
    if alternateui_bars_name[i]<>'' then
    begin
      alternateui_bars_left[i]:=le;
      alternateui_create_color_shape(alternateui_bars_name[i],le,10,alternateui_bars_width,0,alternateui_bars_color[i],'alternateUIMaster_Panel',192);
      le:=le+alternateui_bars_width+alternateui_bars_gap;
      if i=4 then le:=le+140;
    end;
  end;
end;
{$endif}
end.

