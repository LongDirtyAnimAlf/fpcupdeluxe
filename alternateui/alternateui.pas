unit alternateui;
{$mode objfpc}
{$H+}
interface
uses
  Classes, SysUtils{$ifdef windows},windows{$endif}{$ifdef usealternateui},Controls, Dialogs,Graphics, Forms, StdCtrls,ExtCtrls, BCLabel, BCTypes, BCButton, BGRAShape,BGRABitmapTypes,lresources{$endif};

{$ifdef usealternateui}
Var Alternate_ui_created:boolean=false;

Procedure alternateui_Create_Controls;
procedure alteranteui_LeaveHandler(Sender:TObject);
procedure alteranteui_EnterHandler(Sender:TObject);
procedure alteranteui_ClickHandler(Sender:TObject);
procedure alternateui_resize;
procedure alternateui_make_sure_images_on_buttons_are_not_enabled;
{$endif}

implementation
{$ifdef usealternateui}
uses fpcupdeluxemainform;
{$R alternateui.res}
Const
  name_of_button_for_form1='alternate_ui_activate_button';
  Master_Panel_Border_Color=$00FFFFFF;
  Master_Panel_One_Button_Border_Color=$000000EC;
  Master_Panel_Custom_Border_Color=$001D911A;
  Master_Panel_Components_Border_Color=$0000E6E6;
  Master_Panel_Cross_Compilers_Border_Color=$00DFDF00;
  Master_Panel_Title_Background_Color=$00000000;
  Master_Panel_Title_Font_Color=$00F9F9F9;
  master_Panel_Info_display_BackGround_Color=$000B0B0B;
  master_Panel_Info_display_BackGround_Color1=$00060606;
  master_Panel_Info_display_Font_Color=$00BBBBBB;
  master_Panel_Info_display_Border_Color=$00BBBBBB;
  icon_size=30;
  drop_arrow_true=True;
  drop_arrow_false=False;
  button_uses_leave_and_enter_true=True;
  button_uses_leave_and_enter_False=False;
  form_background_color=$00080808;//$002F0000;
  form_memo_summary_background_color=$00000000;
  form_memo_summary_font_color=$00FFC6C6;
  form_instalDirEdit_BackGround_Color=$00000000;
  form_instalDirEdit_font_Color=$0000D2D2;
  form_statusmessage_background_color=$00000000;
  form_statusmessage_font_color=$00FFC6C6;
  form_realfpc_background_color=$00000000;
  form_realfpc_font_color=$00FFC6C6;
  form_reallaz_background_color=$00000000;
  form_reallaz_font_color=$00FFC6C6;
  button_panel_color=$0044120D;
  shape_border_green=$0000D700;
  shape_border_yellow=$0000D2D2;
  shape_fill_color=$00040000;//$003E0E00;
  shape_fill_opacity=128;
  shape_border_rounding_size=1;
  select_shape_fill_color=$00002000;
  button_rounding_size=6;
  button_normal_color=$00994022;
  button_normal_font_color=$00FFC6C6;
  button_normal_border_color=$00000000;
  button_normal_border_width=1;
  button_normal_border_use=False;
  button_hover_color=$00B47A66;
  button_hover_font_color=$00FFC6C6;
  button_hover_border_color=$00000000;
  button_hover_border_width=1;
  button_hover_border_use=False;
  button_clicked_color=$00003300;
  button_clicked_font_color=$00FFC6C6;
  button_clicked_border_color=$0000D700;
  button_clicked_border_width=2;
  button_clicked_border_use=true;
  button_panel_title_font_color=$00FFFFF;
  button_cursor=crHandPoint;
  Initial_Visibility=false;
  but_gap=4;
  Buttons_Left_margin=8;
  Close_Button_Size=20;
  Number_Of_Languages=2;
  Max_List_Controls=50;

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
  Control_Install_Directory_Button='alternateInstallDirBtn';
  Control_Auto_Clear_Button='alternateAutoClearBtn';
  Control_Clear_Log_Button='alternateClearLogBtn';
  Control_Display_Help_Text='alternateUIMaster_Info_Display_Text';

  alternateui_max_controls_with_images=20;


Type
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

Var alternateui_Languages:Array[0..Number_Of_Languages] of alternateui_Language_Type;
    alternateui_inuse:boolean=false;
    Info_Display_Default_Text,Settings_Help_Text,OnButtonInstaller_Help_Text,NewUI_Help_Text,SelectFPC_Help_Text,SelectLazarus_Help_Text,InstallFPC_Help_Text:AnsiString;
    InstallLazarus_Help_Text,InstallFPCAndLazarus_Help_Text,SelectCrossCompiler_Help_Text,UpdateCrossCompiler_Help_Text,SelectComponents_Help_Text,SetInstallDireectory_Help_Text:AnsiString;
    AUtoClear_Help_Text,ClearLog_Help_Text,Lang_fpc_target,lang_laz_target:ansistring;

    Pan_List:Array[0..4] of AnsiString=('FPCTarget_','LazTarget_','ComponentSelect_','OneButtonSelect_','alternateUICrossCompiler_');
    But_List:Array[0..4] of Ansistring=(Control_FPC_Select_Button,Control_LAZ_Select_Button,Control_Components_Select_Button,Control_One_Button_Install,Control_Cross_Compiler_Select_Button);

    alternateui_number_of_images_on_buttons:Integer=0;
    buttons_with_images_on:array[0..alternateui_max_controls_with_images] of AnsiString;

    alternateui_Version:AnsiString='AUI V 1.0.4';
    alternateui_Title:AnsiString='';
    alternateui_form1_title:AnsiString='';

    alternateui_label_font_size:real=12.0;
    alternateui_button_font_size:real=11.0;


    {$ifdef unix}           // should be ok for linux and darwin
    alternateui_font_ratio:Real=0.9;
    {$else}
    alternateui_font_ratio:Real=1.0;
    {$endif}

procedure alternateui_set_language(LA:AnsiString);Forward;

procedure alternateui_make_sure_images_on_buttons_are_not_enabled;
var i:integer;
begin
  // need to make sure that after fpupdeluxe disEnable routine that the image placed on buttons are not enabled.
  for i:=0 to alternateui_number_of_images_on_buttons do
  begin
    if buttons_with_images_on[i]<>'' then timage(form1.FindComponent(buttons_with_images_on[i])).enabled:=false;
  end;
end;

function  alternateui_Get_Png_from_resource(AObj:TObject;Res_Name:ShortString):boolean;
var
png : TPortableNetworkGraphic;
vStream : TResourceStream;
begin

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
    If AObj is TBCButton then
    begin
      try
        TBCButton(AObj).Glyph.Assign(png);
      finally
        vStream.Free;
      end;
    end;
  finally
    Png.Free;
  end;
  alternateui_Get_Png_from_resource:=true;
end;

Function alternateui_IntToString(AInt:Integer):ShortString;
begin
  if AInt<10 then result:='00'+inttostr(aInt)
  else if AInt<100 then result:='0'+inttostr(aInt)
  else result:=inttostr(aint);
end;

procedure alternateui_animate_panel(na:shortstring;fade_up:boolean);
const fade_interval=20;
      sleep_interval=10;
begin
  if sleep_interval<>0 then
  begin
    if form1.FindComponent(na+'Shape')<>nil then
    begin
      with form1.FindComponent(na+'Shape') as tbgrashape do
      begin
        if fade_up then
        begin
          bringtofront;
          FillOpacity:=255;
          application.ProcessMessages;
          (form1.FindComponent(na+'Panel') as TPanel).Visible:=true;
          while fillopacity>128 do
          begin
            fillopacity:=fillopacity-fade_interval;
            application.processmessages;
            sleep(sleep_interval);
          end;
        end
        else
        begin
          if (form1.FindComponent(na+'Panel') as TPanel).Visible=true then
          begin
            bringtofront;
            FillOpacity:=128;
            while fillopacity<245 do
            begin
              fillopacity:=fillopacity+fade_interval;
              application.processmessages;
              sleep(sleep_interval);
            end;
            (form1.FindComponent(na+'Panel') as TPanel).Visible:=False;
          end;
        end;
        fillopacity:=shape_fill_opacity;
        sendtoback;
      end;
    end;
  end
  else
  begin
    (form1.FindComponent(na+'Panel') as TPanel).Visible:=fade_up;
  end;
end;

procedure alternateui_set_FPCtarget_btn;
begin
  tbcbutton(form1.FindComponent(Control_FPC_Select_Button)).Caption:=Lang_FPC_target+slinebreak+form1.ListBoxFPCTarget.Items.Strings[form1.ListBoxFPCTarget.ItemIndex];
end;

procedure alternateui_set_LAZtarget_btn;
begin
  tbcbutton(form1.FindComponent(Control_LAZ_Select_Button)).Caption:=Lang_laz_target+slinebreak+form1.ListBoxLazarusTarget.Items.Strings[form1.ListBoxLazarusTarget.ItemIndex];
end;

procedure alternateui_set_OSTarget_btn;
var i:integer=0;
begin
  while form1.findcomponent('OSTarget_btn'+alternateui_IntToString(i))<>nil do
  begin
    tbcbutton(form1.findcomponent('OSTarget_btn'+alternateui_IntToString(i))).Down:=i=form1.radgrpOS.ItemIndex;
    tbcbutton(form1.findcomponent('OSTarget_btn'+alternateui_IntToString(i))).Enabled:=form1.radgrpOS.Enabled;
    inc(i);
  end
end;

procedure alternateui_set_CPUTarget_btn;
Var i:Integer=0;
begin
  while form1.findcomponent('CPUTarget_btn'+alternateui_IntToString(i))<>nil do
  begin
    tbcbutton(form1.findcomponent('CPUTarget_btn'+alternateui_IntToString(i))).Down:=i=form1.radgrpCPU.ItemIndex;
    tbcbutton(form1.findcomponent('CPUTarget_btn'+alternateui_IntToString(i))).Enabled:=form1.radgrpCPU.Enabled;
    inc(i);
  end;
end;


procedure alternateui_set_Selected_Components(AKeep:Boolean);
Var i:Integer=0;
begin
  while form1.findcomponent('ComponentSelect_btn'+alternateui_IntToString(i))<>nil do
  begin
    if Akeep=false then Form1.ListModules.Selected[i]:=false;
    tbcbutton(form1.findcomponent('ComponentSelect_btn'+alternateui_IntToString(i))).Down:=Form1.ListModules.Selected[i];
    inc(i);
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
    form1.Caption:=alternateui_title;
    form1.listModules.MultiSelect:=true;
    form1.Color:=form_background_color;
    form1.memoSummary.Color:=form_memo_summary_background_color;
    form1.memoSummary.Font.Color:=form_memo_summary_font_color;
    form1.Constraints.MinWidth:=1052;
    form1.Constraints.MinHeight:=631;
    form1.DoubleBuffered:=true;
    form1.InstallDirEdit.Color:=form_instalDirEdit_BackGround_Color;
    form1.InstallDirEdit.Font.Color:=form_instalDirEdit_font_Color;
    form1.StatusMessage.Color:=form_instalDirEdit_BackGround_Color;
    form1.StatusMessage.font.Color:=form_instalDirEdit_font_Color;
    form1.RealFPCURL.Color:=form_realfpc_background_color;
    form1.RealFPCURL.Font.Color:=form_realfpc_font_color;
    form1.RealLAZURL.Color:=form_reallaz_background_color;
    form1.RealLAZURL.Font.Color:=form_reallaz_font_color;
    form1.CheckAutoClear.AnchorSideRight.Control:=(form1.FindComponent(Control_Clear_Log_Button) as TBCButton);
    form1.StatusMessage.AnchorSideRight.Control:=(form1.FindComponent(Control_Auto_Clear_Button) as TBCButton);
    with (form1.FindComponent(Control_Auto_Clear_Button) as tbcbutton) do
    begin
      down:=form1.CheckAutoClear.Checked;
      //adjust glyph
      if Down then alternateui_Get_Png_from_resource(form1.findcomponent(Control_Auto_Clear_Button) as tbcbutton,'AUI_GREEN_TICK_SMALL')
      else alternateui_Get_Png_from_resource(form1.findcomponent(Control_Auto_Clear_Button) as tbcbutton,'AUI_RED_CROSS_SMALL');
    end;
    // set the cross compiler options
    alternateui_set_OSTarget_btn;
    alternateui_set_CPUTarget_btn;
    alternateui_set_FPCtarget_btn;
    alternateui_set_Laztarget_btn;
    alternateui_set_Selected_Components(True);
  end
  else
  begin
    form1.Caption:=alternateui_form1_title;
    form1.listModules.MultiSelect:=false;
    form1.Color:=clDefault;
    form1.memoSummary.Color:=clDefault;
    form1.memoSummary.Font.Color:=clDefault;
    form1.Constraints.MinWidth:=0;
    form1.Constraints.MinHeight:=0;
    form1.InstallDirEdit.Color:=clDefault;
    form1.InstallDirEdit.Font.Color:=clRed;
    form1.StatusMessage.Color:=clDefault;
    form1.StatusMessage.font.Color:=clDefault;
    form1.RealFPCURL.Color:=clDefault;
    form1.RealFPCURL.Font.Color:=clDefault;
    form1.RealLAZURL.Color:=clDefault;
    form1.RealLAZURL.Font.Color:=clDefault;
    form1.CheckAutoClear.AnchorSideRight.Control:=form1.btnClearLog;
    form1.StatusMessage.AnchorSideRight.Control:=form1.CheckAutoClear;
    // hide all panels
    for i:=0 to 4 do alternateui_animate_panel(pan_list[i],false);
    for i:=0 to 4 do tbcbutton(form1.FindComponent(but_list[i])).Down:=false;

  end;
  if not alternateui_inuse then alternateui_animate_panel('alternateUIMaster_',alternateui_inuse);
  form1.ListBoxFPCTarget.Visible:=disp_control;
  form1.ListBoxLazarusTarget.Visible:=disp_control;
  form1.BitBtnFPCOnly.Visible:=disp_control;
  form1.BitBtnLazarusOnly.Visible:=disp_control;
  form1.btnSetupPlus.Visible:=disp_control;
  form1.BitBtnFPCandLazarus.Visible:=disp_control;
  form1.listModules.Visible:=disp_control;
  (form1.FindComponent(Control_Install_Directory_Button) as TBCButton).Visible:=Not Disp_control;
  (form1.FindComponent(Control_Clear_Log_Button) as TBCButton).Visible:=Not Disp_control;
  (form1.FindComponent(Control_Auto_Clear_Button) as TBCButton).Visible:=Not Disp_control;
  form1.btnInstallDirSelect.Visible:=disp_control;
  form1.btnClearLog.Visible:=disp_control;
  form1.CheckAutoClear.Visible:=disp_control;
  form1.ButtonInstallCrossCompiler.Visible:=disp_control;
  form1.AutoCrossUpdate.Visible:=disp_control;
  form1.listModules.Visible:=disp_control;
  form1.btnInstallModule.Visible:=disp_control;
  form1.FPCVersionLabel.Visible:=disp_control;
  form1.LazarusVersionLabel.Visible:=disp_control;
  form1.radgrpCPU.Visible:=disp_control;
  form1.radgrpOS.Visible:=disp_control;
  form1.Panel1.Visible:=disp_control;
  if alternateui_inuse then alternateui_animate_panel('alternateUIMaster_',alternateui_inuse);
  form1.Invalidate;
  application.ProcessMessages;
end;

procedure alteranteui_EnterHandler(Sender:TObject);
  var na:shortstring;
begin
  if Sender is TBCBUtton then
  begin
    na:=tbcbutton(Sender).Name;
    with form1.FindComponent(Control_Display_Help_Text) as TBCLabel do
    begin
      case na of
        Control_One_Button_Install            :Caption:=OnButtonInstaller_Help_Text;
        Control_Settings_Button               :Caption:=Settings_Help_Text;
        Control_FPC_Select_Button             :Caption:=SelectFPC_Help_Text;
        Control_LAZ_Select_Button             :Caption:=SelectLazarus_Help_Text;
        Control_FPC_Install_Button            :Caption:=InstallFPC_Help_Text;
        Control_LAZ_Install_Button            :Caption:=InstallLazarus_Help_Text;
        Control_FPC_And_LAZ_Install_Button    :Caption:=InstallFPCAndLazarus_Help_Text;
        Control_Cross_Compiler_Select_Button  :Caption:=SelectCrossCompiler_Help_Text;
        Control_Cross_Compiler_Update_Button  :Caption:=UpdateCrossCompiler_Help_Text;
        Control_Components_Select_Button      :Caption:=SelectComponents_Help_Text;
        Control_Install_Directory_Button      :Caption:=SetInstallDireectory_Help_Text;
        Control_Auto_Clear_Button             :Caption:=AUtoClear_Help_Text;
        Control_Clear_Log_Button              :Caption:=ClearLog_Help_Text;
      end;
    end;
  end;
  If Sender is TImage then
  Begin
    na:=TImage(Sender).name;
    if na=name_of_button_for_form1 then
    begin
      (form1.FindComponent(Control_Display_Help_Text) as TBCLabel).Caption:=NewUI_Help_Text;
      alternateui_Get_Png_from_resource(form1.findcomponent(name_of_button_for_form1) as timage,'AUI_NEW_UI_HOVER');
    end;
  end;
end;

procedure alteranteui_LeaveHandler(Sender:TObject);
Begin
   If Sender is TBCButton then (form1.FindComponent(Control_Display_Help_Text) as TBCLabel).Caption:=Info_Display_Default_Text;
   If Sender is TIMage then
   begin
     if timage(sender).name=name_of_button_for_form1 then
     begin
       (form1.FindComponent(Control_Display_Help_Text) as TBCLabel).Caption:=Info_Display_Default_Text;
       alternateui_Get_Png_from_resource(form1.findcomponent(name_of_button_for_form1) as timage,'AUI_NEW_UI');
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
        tbcbutton(form1.findcomponent(but_list[i])).Down:=disp_pan;
      end
    end;
  end;
  for i:=0 to 4 do
  begin
    if pan_list[i]<>'' then
    begin
      if pan_list[i]<>pn then
      begin
        alternateui_animate_panel(pan_list[i],false);
        tbcbutton(form1.findcomponent(but_list[i])).Down:=false;
      end
    end;
  end;
  if form1.FindComponent(pn+'Panel')<> nil then
  begin
    with (form1.findcomponent(pn+'Panel')) as tpanel do
    begin
      Visible:=disp_pan;
      if disp_pan then bringtofront;
    end;
  end;
end;

procedure alteranteui_ClickHandler(Sender:TObject);
var co:integer=0;
    na:shortstring;
    sender_name:shortstring;
    itm:integer;
    i:integer;
begin
  If Sender is tbcbutton then
  begin
    sender_name:=tbcbutton(sender).Name;
    na:=copy(sender_name,1,length(sender_name)-3);
    if ((na='FPCTarget_btn') or (na='LazTarget_btn')) then
    begin
      while form1.findcomponent(na+alternateui_IntToString(co))<>nil do
      begin
        tbcbutton(form1.findcomponent(na+alternateui_IntToString(co))).down:=false;
        inc(co);
      end;
      with sender as tbcbutton do
      begin
        down:=true;
        if na='FPCTarget_btn' then
        begin
          itm:=strtoint(copy(tbutton(sender).Name,length(tbutton(sender).Name)-2,3));
          form1.FPCTarget:=caption;
          form1.ListBoxFPCTarget.ItemIndex:=itm;
          alternateui_set_FPCtarget_btn;
        end;
        if na='LazTarget_btn' then
        begin
          itm:=strtoint(copy(tbutton(sender).Name,length(tbutton(sender).Name)-2,3));
          Form1.LazarusTarget:=caption;
          form1.ListBoxLazarusTarget.ItemIndex:=itm;
          alternateui_set_Laztarget_btn;
        end;
      end;
    end
    else
    if na='CPUTarget_btn' then
    begin
      itm:=strtoint(copy(tbutton(sender).Name,length(tbutton(sender).Name)-2,3));
      form1.radgrpCPU.ItemIndex:=itm;
      application.ProcessMessages;
      alternateui_set_CPUTarget_btn;
      application.ProcessMessages;
      alternateui_set_OSTarget_btn
    end
    else
    if na='OSTarget_btn' then
    begin
      itm:=strtoint(copy(tbutton(sender).Name,length(tbutton(sender).Name)-2,3));
      form1.radgrpOS.ItemIndex:=itm;
      application.ProcessMessages;
      alternateui_set_OSTarget_btn;
      application.ProcessMessages;
      alternateui_set_CPUTarget_btn
    end
    else
    if na='OneButtonSelect_btn' then
    begin
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      itm:=strtoint(copy(tbutton(sender).Name,length(tbutton(sender).Name)-2,3));
      tbcbutton(sender).Down:=true;
      alternateui_Display_Hide_Panels('Blank',false);
      case itm of
        0:form1.QuickBtnClick(form1.TrunkBtn);
        1:form1.QuickBtnClick(form1.NPBtn);
        2:form1.QuickBtnClick(form1.FixesBtn);
        3:form1.QuickBtnClick(form1.StableBtn);
        4:form1.QuickBtnClick(form1.OldBtn);
        5:form1.QuickBtnClick(form1.DinoBtn);
        6:form1.QuickBtnClick(form1.mORMotBtn);
        7:form1.BitBtnHaltClick(form1.BitBtnHalt);
      end;
      (form1.FindComponent(Control_One_Button_Install) as TBCButton).Down:=False;
      tbcbutton(sender).Down:=false;
      (form1.FindComponent('OneButtonSelect_Panel') as TPanel).Visible:=False;
    end
    else
    if sender_name=Control_Settings_Button then
    begin
      tbcbutton(sender).Down:=true;
      application.ProcessMessages;
      form1.btnSetupPlusClick(form1.btnSetupPlus);
      tbcbutton(sender).Down:=false;
    end
    else
    if sender_name=Control_Install_Directory_Button then
    begin
      form1.btnInstallDirSelectClick(form1.btnInstallDirSelect);
    end
    else
    if sender_name=Control_Clear_Log_Button then
    begin
      form1.btnClearLogClick(form1.btnClearLog);
    end
    else
    if sender_name=Control_Auto_Clear_Button then
    begin
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      // now adjust the glyph
      if tbcbutton(sender).Down then alternateui_Get_Png_from_resource(form1.findcomponent(sender_name),'AUI_GREEN_TICK_SMALL')
      else alternateui_Get_Png_from_resource(form1.findcomponent(sender_name),'AUI_RED_CROSS_SMALL');
      form1.CheckAutoClear.Checked:=tbcbutton(sender).Down;
    end
    else
    if sender_name=Control_One_Button_Install then
    begin
      // Display or remove One Button Installer menu
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      alternateui_Display_Hide_Panels('OneButtonSelect_',tbcbutton(sender).Down);
    end
    else
    if sender_name=Control_FPC_Select_Button then
    begin
      // Display or remove FPCTARGETR menu
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      alternateui_Display_Hide_Panels('FPCTarget_',tbcbutton(sender).Down);
    end
    else
    if sender_name=Control_LAZ_Select_Button then
    begin
      // Display or remove LAZTARGET menu
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      alternateui_Display_Hide_Panels('LazTarget_',tbcbutton(sender).Down);
    end
    else
    if sender_name=Control_Components_Select_Button then
    begin
      // Display or remove LAZTARGET menu
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      alternateui_Display_Hide_Panels('ComponentSelect_',tbcbutton(sender).Down);
    end
    else
    if sender_name=Control_FPC_Install_Button then
    begin
      tbcbutton(sender).Down:=true;
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      application.ProcessMessages;
      form1.FPCOnlyClick(form1.BitBtnFPCOnly);
      tbcbutton(sender).Down:=false;
    end
    else
    if sender_name=Control_LAZ_Install_Button then
    begin
      tbcbutton(sender).Down:=true;
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      application.ProcessMessages;
      form1.LazarusOnlyClick(form1.BitBtnLazarusOnly);
      tbcbutton(sender).Down:=false;
    end
    else
    if sender_name=Control_FPC_And_LAZ_Install_Button then
    begin
      tbcbutton(sender).Down:=true;
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      application.ProcessMessages;
      form1.BitBtnFPCandLazarusClick(form1.BitBtnFPCandLazarus);
      tbcbutton(sender).Down:=false;
    end
    else
    if sender_name=Control_Components_Install_Button then
    begin
      tbcbutton(sender).Down:=true;
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      (form1.FindComponent('ComponentSelect_Panel') as TPanel).Visible:=False;
      alternateui_Display_Hide_Panels('Blank',false);
      application.ProcessMessages;
      form1.btnInstallModuleClick(form1.btnInstallModule);
      tbcbutton(sender).Down:=false;
      // now remove any selected buttons
       alternateui_set_Selected_Components(False);
    end
    else
    if sender_name=Control_Cross_Compiler_Select_Button then
    begin
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      alternateui_Display_Hide_Panels('alternateUICrossCompiler_',tbcbutton(sender).Down);
    end
    else
    if sender_name=Control_Cross_Compiler_Install_Button then
    begin
      tbcbutton(sender).Down:=true;
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      alternateui_Display_Hide_Panels('Blank',false);
      application.ProcessMessages;
      form1.ButtonInstallCrossCompilerClick(form1.ButtonInstallCrossCompiler);
      tbcbutton(sender).Down:=false;
    end
    else
    if sender_name=Control_Cross_Compiler_Update_Button then
    begin
      tbcbutton(sender).Down:=true;
      (form1.FindComponent('alternateuiHalt') as TImage).Visible:=True;
      application.ProcessMessages;
      form1.ButtonAutoUpdateCrossCompiler(form1.AutoCrossUpdate);
      tbcbutton(sender).Down:=false;
    end
    else
    begin
      //multi-select toggle
      tbcbutton(sender).Down:=Not tbcbutton(sender).Down;
      if na='ComponentSelect_btn' then
      begin
        itm:=strtoint(copy(tbutton(sender).Name,length(tbutton(sender).Name)-2,3));
        form1.listmodules.Selected[itm]:=tbcbutton(sender).Down;
      end;
    end;
  end
  else
  begin
    If Sender is timage then
    begin
      sender_name:=timage(sender).Name;
      // check to see if its a close button
      if upcase(copy(sender_name,length(sender_name)-4,5))='CLOSE' then
      begin
        na:=copy(sender_name,1,pos('_',sender_name));
        alternateui_animate_panel(na,false);
        (form1.FindComponent(na+'panel') as tpanel).Visible:=false;
        // also set the activating button to up
        for i:=0 to 4 do
        begin
          if pan_list[i]=na then tbcbutton(form1.findcomponent(but_list[i])).Down:=False;
        end;
      end
      else
      begin
        If sender_name=name_of_button_for_form1 then alternateui_toggle_new_ui;
        if sender_name='alternateuiHalt' then form1.BitBtnHaltClick(form1.BitBtnHalt);
        case sender_name of
          'alternateuiLangEN':alternateui_set_language('EN');
          'alternateuiLangRU':alternateui_set_language('RU');
          'alternateuiLangFR':alternateui_set_language('FR');
        end;
      end;
    end;
  end;
  (form1.FindComponent('alternateuiHalt') as TImage).Visible:=False;
end;

procedure alternateui_Create_Button_Container(base_name:string;base_left,base_top,base_width,base_height:integer;base_title,base_parent:shortstring;base_close:boolean;Panel_Color,Shape_color,Shape_Border_Color,Shape_Border_Width,Label_Color,Label_Font_Color,Label_Border_Color:TColor;Label_Border_Width:Integer;label_bold,Label_Clear_Color:Boolean);
begin
  with tpanel.Create(Form1) do
  begin
    setbounds(base_Left,base_top,base_width,base_height);
    color:=Panel_Color;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderWidth:=0;
    BorderStyle:=bsNone;
    name:=base_name+'Panel';
    doublebuffered:=true;
    if form1.findcomponent(base_parent) is TPanel then
    begin
      parent:=(form1.FindComponent(base_parent) as TPanel);
      visible:=true;
    end
    else
    begin
      parent:=Form1;
      visible:=Initial_Visibility;
    end;
    caption:='';
    bringtofront;
  end;
  if base_title<>'' then
  begin
    // create the title
    with tbclabel.Create(Form1) do
    begin
      setbounds(Label_Border_Width,shape_border_rounding_size,base_width-(Label_Border_Width*2),Close_Button_Size);
      background.color:=Label_Color;
      if Label_Clear_Color then background.style:=bbsClear
      else background.style:=bbsColor;
      border.Color:=Label_Border_Color;
      autosize:=false;
      if label_border_width=0 then border.Style:=bboNone
      else
      begin
        border.Style:=bboSolid;
        border.Width:=Label_Border_Width;
      end;
      enabled:=false;
      fontex.Shadow:=False;
      if label_bold then fontex.Style:=[fsbold]
      else fontex.Style:=[];
      fontex.FontQuality:=fqFineAntialiasing;
      fontex.Color:=Label_Font_Color;
      fontex.TextAlignment:=bcaCenter;
      fontex.Height:=round(alternateui_label_font_size*alternateui_font_ratio);
      name:=base_name+'Title';
      parent:=(form1.FindComponent(base_name+'Panel') as TPanel);
      caption:=base_title;
      visible:=true;
      bringtofront;
    end;
  end;
  // Create the Shape / Border
  with TBGRAShape.Create(Form1) do
  begin
    setbounds(0,0,base_width,base_height);
    Angle:=45;
    bordercolor:=Shape_Border_Color;
    borderstyle:=psSolid;
    borderwidth:=Shape_Border_Width;
    RoundRadius:=shape_border_rounding_size;
    ShapeType:=stRegularPolygon;
    SideCount:=4;
    UseBorderGradient:=false;
    UseFillGradient:=False;
    fillcolor:=Shape_color;
    FillOpacity:=shape_fill_opacity;
    name:=base_name+'Shape';
    parent:=(form1.FindComponent(base_name+'Panel') as TPanel);
    visible:=true;
    sendtoback;
  end;
  if base_close then
  begin
    // Create the Close Button
    with timage.Create(form1) do
    begin
     // picture.Bitmap.LoadFromLazarusResource('CLOSE_BUTTON');
      parent:=(form1.FindComponent(base_name+'Panel') as TPanel);
      AntiAliasingMode:=amOn;
      Stretch:=True;
      cursor:=button_Cursor;
      onclick:=@form1.alternateuibutClick;
      name:=base_name+'Close';
      setbounds(base_Width-Close_Button_Size-shape_border_rounding_size,shape_border_rounding_size,Close_Button_Size,Close_Button_Size);
    end;
    alternateui_Get_Png_from_resource(form1.findcomponent(base_name+'Close') as timage,'AUI_CLOSE_BUTTON');
  end;
end;

procedure alternateui_create_a_button(but_name:string;but_left,but_top,but_width,but_height:integer;but_caption,but_parent:shortstring;but_drop_arrow,but_use_mouse_enter:boolean;but_glyph,but_add_image:ansistring;but_add_image_width,but_add_image_height:integer);
begin
  with tbcbutton.Create(form1) do
  begin
    setbounds(But_Left,but_top,but_width,but_height);
    down:=false;
    statenormal.Background.Color:=button_normal_color;
    statenormal.Background.style:=bbsColor;
    statenormal.Border.Color:=button_normal_border_color;
    statenormal.FontEx.Color:=button_normal_font_color;
    statenormal.FontEx.FontQuality:=fqFineAntialiasing;
    statenormal.FontEx.Shadow:=False;
    statenormal.FontEx.Height:=round(alternateui_button_font_size*alternateui_font_ratio);
    statenormal.FontEx.Style:=[];
    statenormal.Border.Width:=button_normal_border_width;
    if button_normal_border_use then statenormal.Border.Style:=bboSolid
    else statenormal.Border.Style:=bboNone;
    stateHover.Background.Color:=button_hover_color;
    stateHover.Background.style:=bbsColor;
    stateHover.Border.Color:=button_hover_border_color;
    statehover.FontEx.Color:=button_hover_font_color;
    statehover.FontEx.FontQuality:=fqFineAntialiasing;
    statehover.FontEx.Shadow:=False;
    statehover.FontEx.Height:=round(alternateui_button_font_size*alternateui_font_ratio);
    statehover.FontEx.Style:=[];
    stateHover.Border.Width:=button_hover_border_width;
    if button_Hover_border_use then statehover.Border.Style:=bboSolid
    else stateHover.Border.Style:=bboNone;
    stateClicked.Background.Color:=button_clicked_color;
    stateClicked.Background.style:=bbsColor;
    stateClicked.Border.Color:=button_clicked_border_color;
    stateClicked.FontEx.Color:=button_clicked_font_color;
    stateClicked.FontEx.FontQuality:=fqFineAntialiasing;
    stateClicked.FontEx.Shadow:=False;
    stateclicked.FontEx.Height:=round(alternateui_button_font_size*alternateui_font_ratio);
    stateClicked.FontEx.Style:=[];
    stateClicked.Border.Width:=button_clicked_border_width;
    if button_clicked_border_use then stateClicked.Border.Style:=bboSolid
    else stateClicked.Border.Style:=bboNone;
    If but_drop_arrow Then
    Begin
      DropDownArrow:=True;
    End;
    Rounding.RoundX:=button_rounding_size;
    Rounding.RoundY:=button_rounding_size;
    if form1.findcomponent(but_parent) is TPanel then parent:=(form1.FindComponent(but_parent) as TPanel)
    else parent:=Form1;
    caption:=but_caption;
    name:=but_name;
    onclick:=@form1.alternateuibutClick;//(self);
    if but_use_mouse_enter then
    begin
      OnMouseEnter:=@form1.alternateuibutEnter;
      OnMouseLeave:=@form1.alternateuibutLeave;
    end;
    cursor:=button_Cursor;
    visible:=true;
  end;
  if but_glyph<>'' then
  begin
    alternateui_Get_Png_from_resource(form1.findcomponent(but_name),but_glyph);
  end;
  if but_add_image<>'' then
  begin
    // add an image to location and change text position
    with timage.Create(form1) do
    begin
      setbounds(But_Left+((but_width div 2)-(but_add_image_width div 2)),but_top+4,but_add_image_width,but_add_image_height);
      if form1.findcomponent(but_parent) is TPanel then parent:=(form1.FindComponent(but_parent) as TPanel)
      else parent:=Form1;
      name:=but_name+'img';
      AntiAliasingMode:=amOn;
      Stretch:=True;
      cursor:=button_Cursor;
      enabled:=false;
      onclick:=@form1.alternateuibutClick;//(self);
      if but_use_mouse_enter then
      begin
        OnMouseEnter:=@form1.alternateuibutEnter;
        OnMouseLeave:=@form1.alternateuibutLeave;
      end;
    end;
    alternateui_Get_Png_from_resource(form1.findcomponent(but_name+'img') as timage,but_add_image);
    // now change text poistion of button
    with  form1.FindComponent(but_name) as TBCButton do
    begin
      statenormal.FontEx.TextAlignment:=bcaCenterBottom;
      stateHover.FontEx.TextAlignment:=bcaCenterBottom;
      stateClicked.FontEx.TextAlignment:=bcaCenterBottom;
    end;
    // add this to button list; so that it can be quickly disabled if enabled via fpcupdeluxe;
    buttons_with_images_on[alternateui_number_of_images_on_buttons]:=but_name+'img';
    inc(alternateui_number_of_images_on_buttons);
  end;
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
  control_base_name:=copy(base_btn,1,pos('_',base_btn));
  if ((control_base_name='CPUTarget_') or (control_base_name='OSTarget_')) then
  begin
    b_panel:='alternateUICrossCompiler_Panel';
    need_close:=false;
    panel_border:=Master_Panel_Cross_Compilers_Border_Color;
  end
  else b_panel:=control_base_name;
  alternateui_Create_Button_Container(control_base_name,150,60,200,100,title_caption,b_panel,need_close,button_panel_color,shape_fill_color,panel_border,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Components_Border_Color,0,false,true);
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
     // with tbcbutton.Create(form1) do
      begin
        if but_count=How_many_Across then
        begin
          sy:=sy+Height_Of_Buttons+but_gap;
          but_count:=0;
        end;
        if btn_caps_list=nil then alternateui_create_a_button(base_btn+alternateui_IntToString(aloop),Buttons_Left_margin+((Width_Of_Buttons+but_gap)*but_count),sy,Width_Of_Buttons,Height_Of_Buttons,btn_caps_list_from_radio.Items.Strings[aloop],b_panel,false,false,'','',0,0)
        else alternateui_create_a_button(base_btn+alternateui_IntToString(aloop),Buttons_Left_margin+((Width_Of_Buttons+but_gap)*but_count),sy,Width_Of_Buttons,Height_Of_Buttons,btn_caps_list.Items[aloop],b_panel,false,false,'','',0,0);
        inc(but_count);
        with  form1.FindComponent(base_btn+alternateui_IntToString(aloop)) as tbcbutton do
        begin
          if btn_caps_list=nil then Down :=btn_caps_list_from_radio.ItemIndex=aloop
          else Down :=btn_caps_list.Selected[aloop];
        end;
     end;
    end;
   //adjust the panel dimensions;
    with  form1.FindComponent(control_base_name+'Panel') as TPanel do
    begin
      pan_height:=sy+Height_Of_Buttons+4+but_gap;//height;
      pan_width:=Buttons_Left_margin+((Width_Of_Buttons+but_gap)*How_Many_Across)+Buttons_Left_margin-but_gap;//width;
      setbounds(left,top,pan_width,pan_height);
    end;
    //assign image to close button
    if form1.FindComponent(control_base_name+'Close')<>nil then
    begin
      with  form1.FindComponent(control_base_name+'Close') as TImage do
      begin
        setbounds(Pan_Width-Close_Button_Size-shape_border_rounding_size,shape_border_rounding_size,Close_Button_Size,Close_Button_Size);
        bringtofront;
      end;
    end;
    // adjust posution based on panel dimensions
    if form1.FindComponent(control_base_name+'Title')<>nil then
    begin
      with  form1.FindComponent(control_base_name+'Title') as TBCLabel do
      begin
        setbounds(Close_Button_Size+2,shape_border_rounding_size,pan_width-((close_button_size+2)*2),close_button_size);
      end;
    end;
    with  form1.FindComponent(control_base_name+'Shape') as TBGRAShape do
    begin
      setbounds(0,0,pan_width,pan_height);
      sendtoback;
    end;
  end;
end;

procedure alternateui_resize;
begin
  if Alternate_ui_created then
  begin
    if alternateui_inuse then
    begin
      tbcbutton(form1.FindComponent(Control_Clear_Log_Button)).Left:=form1.width-84;//.btnClearLog.Left;
      tbcbutton(form1.FindComponent(Control_Auto_Clear_Button)).Left:=form1.width-170;//CheckAutoClear.Left;
    end;
  end;
end;

procedure alternateui_add_title_and_glyph(bm:ansistring;gl:ansistring;ca:ansistring;hi:ansistring);
begin
  with (form1.FindComponent(bm) as tbcbutton) do
  begin
    caption:=ca;
    hint:=hi;
    if hi<>'' then showhint:=true;
    if gl='' then glyph:=nil
    else alternateui_Get_Png_from_resource(form1.findcomponent(bm) as tbcbutton,gl);
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
  alternateui_Languages[0].Default_Info_Text:='Welcome to FPCUPDeluxe'+slinebreak+'==============='+slinebreak+'This allows you to install sandboxed versions of Free Pascal/Lazarus; this allows you to have multiple different version of Lazarus and Free Pascal on your system'+slinebreak+slinebreak+'You can also install cross compilers to allow easy cross compiling to various different CPU and OS Targets'+slinebreak+slinebreak+'You can also install various additional components into your Lazarus Installations';
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
  alternateui_Languages[1].Default_Info_Text:='Bienvenue sur FPCUPDeluxe'+slinebreak+'==============='+slinebreak+'Cela vous permet d'+chr(39)+'installer des versions en bac à sable de Pascal libre / Lazarus; cela vous permet d'+chr(39)+'avoir plusieurs version différente de Lazarus et Free Pascal sur votre système.'+slinebreak+slinebreak+'Vous pouvez également installer des compilateurs croisés pour faciliter la compilation à divers différents CPU et OS cibles.'+slinebreak+slinebreak+'Vous pouvez également installer divers composants supplémentaires dans votre Installations Lazarus.';
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
  alternateui_Languages[2].Default_Info_Text:='Добро пожаловать в FPCUPDeluxe'+slinebreak+'==============='+slinebreak+'Это позволяет вам устанавливать изолированные Свободный Паскаль / Лазарь; это позволяет вам иметь несколько другая версия Lazarus и Free Pascal в вашей системе.'+slinebreak+slinebreak+'Вы также можете установить кросс-компиляторы, чтобы упростить крест компиляция для различных целей ЦП и ОС.'+slinebreak+slinebreak+'Вы также можете установить в Установки Лазаря.';
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
  with form1.FindComponent(bm) as tbcbutton do
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
      TBCLabel(form1.findcomponent('alternateUIOneButBox_Title')).Caption:=One_Button_Title_Text;
      TBCLabel(form1.findcomponent('alternateUICustomInstallationBox_Title')).Caption:=Custom_Title_Text;
      TBCLabel(form1.findcomponent('alternateUIComponentInstallationBox_Title')).Caption:=Component_Title_Text;
      TBCLabel(form1.findcomponent('alternateUICrossCompilerBox_Title')).Caption:=Compiler_Title_Text;
      IF La='EN' then alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangEN') as timage,'AUI_LANG_EN_ON') else alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangEN') as timage,'AUI_LANG_EN');
      IF La='RU' then alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangRU') as timage,'AUI_LANG_RU_ON') else alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangRU') as timage,'AUI_LANG_RU');
      IF La='FR' then alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangFR') as timage,'AUI_LANG_FR_ON') else alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangFR') as timage,'AUI_LANG_FR');
    end;
    (form1.FindComponent(Control_Display_Help_Text) as TBCLabel).Caption:=Info_Display_Default_Text;
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

begin
  // Memorize Title
  alternateui_form1_title:=form1.Caption;

  alternateui_title:=StringReplace(alternateui_form1_title,'base','('+AlternateUi_Version+') base',[rfReplaceAll]);


  // create a button on form1 to activate interface
  with timage.Create(form1) do
  begin
    parent:=Form1;//(form1.FindComponent(control_base_name+'Panel') as TPanel);
    AntiAliasingMode:=amOn;
    Stretch:=True;
    cursor:=button_Cursor;
    onclick:=@form1.alternateuibutClick;
    OnMouseEnter:=@form1.alternateuibutEnter;
    OnMouseLeave:=@form1.alternateuibutLeave;
    name:=name_of_button_for_form1;//
    setbounds(form1.btnInstallDirSelect.Left+form1.btnInstallDirSelect.Width+12,4,trunc(Close_Button_Size*1.5),trunc(Close_Button_Size*1.5));
  end;
  alternateui_Get_Png_from_resource(form1.findcomponent(name_of_button_for_form1) as timage,'AUI_NEW_UI');

   // create the buttons, panels, etc required for the popup menus.
  alternateui_Create_Buttons_for_Container('FPCTarget_btn',Form1.ListBoxFPCtarget,nil,'Please select the Free Pascal Compiler you wish to Install',7,18,120);
  alternateui_Create_Buttons_for_Container('LazTarget_btn',Form1.ListBoxLazarusTarget,nil,'Please select the Lazarus you wish to Install',7,18,120);
  alternateui_Create_Buttons_for_Container('ComponentSelect_btn',Form1.ListModules,nil,'Please select the Component(s) you wish to install.',7,18,120);
  alternateui_Create_Buttons_for_Container('OneButtonSelect_btn',Form1.ListModules,nil,'',8,34,120);
  with  form1.FindComponent('OneButtonSelect_Panel') as TPanel do
  begin
    left:=14;
    top:=116;
  end;
  with  form1.FindComponent('OneButtonSelect_Shape') as TBGRAShape do
  begin
    bordercolor:=button_clicked_border_color;
    fillcolor:=select_shape_fill_color;
  end;
  with  form1.FindComponent('FPCTarget_Panel') as TPanel do
  begin
    left:=14;
    top:=182;
  end;
  with  form1.FindComponent('FPCTarget_Shape') as TBGRAShape do
  begin
    bordercolor:=button_clicked_border_color;
    fillcolor:=select_shape_fill_color;
  end;
  with  form1.FindComponent('LAZTarget_Panel') as TPanel do
  begin
    left:=14;
    top:=182;
  end;
  with  form1.FindComponent('LAZTarget_Shape') as TBGRAShape do
  begin
    bordercolor:=button_clicked_border_color;
    fillcolor:=select_shape_fill_color;
  end;
  with  form1.FindComponent('ComponentSelect_Panel') as TPanel do
  begin
    left:=138;
    top:=172;
  end;
  with  form1.FindComponent('ComponentSelect_Shape') as TBGRAShape do
  begin
    bordercolor:=button_clicked_border_color;
    fillcolor:=select_shape_fill_color;
  end;

  // Add Titles to OneButton
  alternateui_add_title_and_glyph('OneButtonSelect_btn000','AUI_TRUNK',form1.trunkbtn.Caption,form1.TrunkBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn001','AUI_NEW_PASCAL',form1.NPBtn.Caption,form1.NPBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn002','AUI_FIXES',form1.FixesBtn.Caption,form1.FixesBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn003','AUI_STABLE',form1.StableBtn.Caption,form1.StableBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn004','AUI_OLD',form1.OldBtn.Caption,form1.OldBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn005','AUI_DINO',form1.DinoBtn.Caption,form1.DinoBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn006','AUI_MORMOT',form1.mORMotBtn.Caption,form1.mORMotBtn.Hint);
  alternateui_add_title_and_glyph('OneButtonSelect_btn007','AUI_HALT',form1.BitBtnHalt.Caption,form1.BitBtnHalt.Hint);

  //Create Panels to Hold Functional Buttons
  alternateui_Create_Button_Container('alternateUIMaster_',4,40,360,564,'','',false,button_panel_color,shape_fill_color,Master_Panel_Border_Color,2,master_Panel_Info_display_BackGround_Color,Master_Panel_Title_Font_Color,master_Panel_Info_display_Border_Color,1,false,false);
  alternateui_Create_Button_Container('alternateUIOneButBox_',110,2,140,80,'Quick Installer','alternateUIMaster_Panel',false,button_panel_color,shape_fill_color,Master_Panel_One_Button_Border_Color,4,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_One_Button_Border_Color,2,false,false);
  alternateui_Create_Button_Container('alternateUICustomInstallationBox_',4,(tpanel(form1.FindComponent('alternateUIOneButBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUIOneButBox_Panel')).height)+4,wi,114,'Custom Installation','alternateUIMaster_Panel',false,button_panel_color,shape_fill_color,Master_Panel_Custom_Border_Color,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Custom_Border_Color,1,false,false);
  alternateui_Create_Button_Container('alternateUIComponentInstallationBox_',4,(tpanel(form1.FindComponent('alternateUICustomInstallationBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUICustomInstallationBox_Panel')).height)+4,wi,76,'Install Additional Components','alternateUIMaster_Panel',false,button_panel_color,shape_fill_color,Master_Panel_Components_Border_Color,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Components_Border_Color,1,false,false);
  alternateui_Create_Button_Container('alternateUICrossCompilerBox_',4,(tpanel(form1.FindComponent('alternateUIComponentInstallationBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUIComponentInstallationBox_Panel')).height)+4,wi,76,'Custom Cross Compilers','alternateUIMaster_Panel',false,button_panel_color,shape_fill_color,Master_Panel_Cross_Compilers_Border_Color,2,Master_Panel_Title_Background_Color,Master_Panel_Title_Font_Color,Master_Panel_Cross_Compilers_Border_Color,1,false,false);

  // Create the Lower Notes Label Control
  with tbclabel.Create(Form1) do
  begin
    setbounds(4,
    (tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+4,
    wi,
    (tpanel(form1.FindComponent('alternateUIMaster_Panel')).height)-((tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+4)-4);
    background.Color:=master_Panel_Info_display_BackGround_Color1;
    background.style:=bbsColor;
    autosize:=false;
    border.Style:=bboSolid;
    border.Color:=master_Panel_Info_display_Border_Color;
    enabled:=false;
    fontex.SingleLine:=False;
    fontex.WordBreak:=True;
    fontex.Shadow:=False;
    fontex.Style:=[];
    FontEx.Height:=round(alternateui_label_font_size*alternateui_font_ratio);
    fontex.FontQuality:=fqFineAntialiasing;
    fontex.Color:=master_Panel_Info_display_Font_Color;
    fontex.TextAlignment:=bcaCenterTop;
    name:='alternateUIMaster_Info_Display';
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);
    caption:='';//Info_Display_Default_Text;
    visible:=true;
    bringtofront;
  end;
  with tbclabel.Create(Form1) do
  begin
    setbounds(8,
    (tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+8,
    wi-8,
    (tpanel(form1.FindComponent('alternateUIMaster_Panel')).height)-((tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).top)+(tpanel(form1.FindComponent('alternateUICrossCompilerBox_Panel')).height)+4)-12);
    background.Color:=master_Panel_Info_display_BackGround_Color;
    background.style:=bbsColor;
    autosize:=false;
    border.Style:=bboNone;
    border.Color:=master_Panel_Info_display_Border_Color;
    enabled:=false;
    fontex.SingleLine:=False;
    fontex.WordBreak:=True;
    fontex.Shadow:=False;
    FontEx.Height:=round(alternateui_label_font_size*alternateui_font_ratio);
    fontex.Style:=[];
    fontex.FontQuality:=fqFineAntialiasing;
    fontex.Color:=master_Panel_Info_display_Font_Color;
    fontex.TextAlignment:=bcaCenterTop;
    name:=Control_Display_Help_Text;
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);
    caption:=Info_Display_Default_Text;
    visible:=true;
    bringtofront;
  end;

  // Create Panels for Cross Compiler
  alternateui_Create_Button_Container('alternateUICrossCompiler_',370,40,600,564,'Install Cross Compilers','',true,button_panel_color,shape_fill_color,Master_Panel_Border_Color,1,master_Panel_Info_display_BackGround_Color,Master_Panel_Title_Font_Color,master_Panel_Info_display_Border_Color,0,false,true);
  alternateui_Create_Buttons_for_Container('CPUTarget_btn',nil,form1.radgrpCPU,'Select CPU',1,18,120);
  alternateui_Create_Buttons_for_Container('OSTarget_btn',nil,form1.radgrpOS,'Select OS',1,18,120);
  with  form1.FindComponent('alternateUICrossCompiler_Shape') as TBGRAShape do
  begin
    bordercolor:=button_clicked_border_color;
    fillcolor:=select_shape_fill_color;
    borderwidth:=2;
  end;
  with form1.FindComponent('CPUTarget_Panel') as TPanel do
  begin
    left:=20;
    top:=30
  end;
  with form1.FindComponent('OSTarget_Panel') as TPanel do
  begin
    left:=20+TPanel(form1.FindComponent('CPUTarget_Panel')).Width+20;
    top:=30;
  end;
  with form1.FindComponent('alternateUICrossCompiler_Panel') as TPanel do
  begin
    left:=138;
    top:=192;
    width:=TPanel(form1.FindComponent('OSTarget_Panel')).Left+TPanel(form1.FindComponent('OSTarget_Panel')).Width+20;
    if TPanel(form1.FindComponent('CPUTarget_Panel')).Height>TPanel(form1.FindComponent('OSTarget_Panel')).Height then height:=TPanel(form1.FindComponent('CPUTarget_Panel')).Height+80
    else height:=TPanel(form1.FindComponent('OSTarget_Panel')).Height+80;
    tbgrashape(form1.FindComponent('alternateUICrossCompiler_Shape')).Height:=height;
    tbgrashape(form1.FindComponent('alternateUICrossCompiler_Shape')).Width:=Width;
    tbclabel(form1.FindComponent('alternateUICrossCompiler_Title')).Width:=Width;
    Timage(form1.FindComponent('alternateUICrossCompiler_Close')).left:=Width-Close_Button_Size-shape_border_rounding_size
  end;

  // Now add buttons to Containers
  // One Button Panel and Settings Button
  alternateui_create_a_button(Control_Settings_Button,wi-96-2,32,96,48,'Settings','alternateUIMaster_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_SETTINGS',icon_size,icon_size);
  alternateui_create_a_button(Control_One_Button_Install,10,26,120,48,'One Button Installer','alternateUIOneButBox_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_QUICK',icon_size,icon_size);

  // Custom Installation Section
  alternateui_create_a_button(Control_FPC_Select_Button,10,24,120,32,'FPC Version','alternateUICustomInstallationBox_Panel',drop_arrow_True,button_uses_leave_and_enter_true,'','',0,0);
  alternateui_create_a_button(Control_LAZ_Select_Button,wi-120-lm,24,120,32,'Lazarus Version','alternateUICustomInstallationBox_Panel',drop_arrow_True,button_uses_leave_and_enter_true,'','',0,0);
  alternateui_create_a_button(Control_FPC_Install_Button,10,62,70,48,'Install FPC','alternateUICustomInstallationBox_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_FPC',icon_size,icon_size);
  alternateui_create_a_button(Control_LAZ_Install_Button,wi-70-lm,62,70,48,'Install Lazarus','alternateUICustomInstallationBox_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_LAZ',icon_size,icon_size);
  alternateui_create_a_button(Control_FPC_And_LAZ_Install_Button,(wi div 2)-(120 div 2),62,120,48,'Install FPC & Lazarus','alternateUICustomInstallationBox_Panel',false,button_uses_leave_and_enter_true,'','AUI_FPC_LAZ',110,icon_size);
  with (form1.FindComponent(Control_FPC_Select_Button) as tbcbutton) do
  begin
    statenormal.FontEx.SingleLine:=False;
    statenormal.FontEx.WordBreak:=True;
    statehover.FontEx.SingleLine:=False;
    statehover.FontEx.WordBreak:=True;
    stateClicked.FontEx.SingleLine:=False;
    stateClicked.FontEx.WordBreak:=True;
    Caption:='FPC Version'+slinebreak+'';
  end;
  with (form1.FindComponent(Control_LAZ_Select_Button) as tbcbutton) do
  begin
    statenormal.FontEx.SingleLine:=False;
    statenormal.FontEx.WordBreak:=True;
    statehover.FontEx.SingleLine:=False;
    statehover.FontEx.WordBreak:=True;
    stateClicked.FontEx.SingleLine:=False;
    stateClicked.FontEx.WordBreak:=True;
    Caption:='Lazarus Version'+slinebreak+'';
  end;

  // Components Section
  alternateui_create_a_button(Control_Components_Select_Button,(wi div 2)-(120 div 2),24,120,48,'Select Components','alternateUIComponentInstallationBox_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_MODULES',icon_size,icon_size);
  alternateui_create_a_button(Control_Components_Install_Button,((tpanel(form1.FindComponent('ComponentSelect_Panel')).width)-140) div 2,tpanel(form1.FindComponent('ComponentSelect_Panel')).height,140,40,'Install Modules','ComponentSelect_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','',0,0);
  with (form1.FindComponent(Control_Components_Install_Button) as tbcbutton) do
  begin
    statenormal.FontEx.SingleLine:=False;
    statenormal.FontEx.WordBreak:=True;
    statehover.FontEx.SingleLine:=False;
    statehover.FontEx.WordBreak:=True;
    stateClicked.FontEx.SingleLine:=False;
    stateClicked.FontEx.WordBreak:=True;
    Caption:='Install'+slinebreak+'Modules';
  end;
  alternateui_add_title_and_glyph(Control_Components_Install_Button,'AUI_GO','Install Components',form1.btnInstallModule.Hint);
  tpanel(form1.FindComponent('ComponentSelect_Panel')).height:=tpanel(form1.FindComponent('ComponentSelect_Panel')).height+48;
  tbgrashape(form1.FindComponent('ComponentSelect_Shape')).height:=tbgrashape(form1.FindComponent('ComponentSelect_Shape')).height+48;

  // Cross Compilers
  alternateui_create_a_button(Control_Cross_Compiler_Select_Button,10,24,120,48,'Cross Compiler','alternateUICrossCompilerBox_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_CROSS_COMPILER',110,icon_size);
  alternateui_create_a_button(Control_Cross_Compiler_Install_Button,((tpanel(form1.FindComponent('alternateUICrossCompiler_Panel')).Width)-140) div 2,tpanel(form1.FindComponent('alternateUICrossCompiler_Panel')).Height-46,140,40,'Install Compiler','alternateUICrossCompiler_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','',0,0);
  alternateui_add_title_and_glyph(Control_Cross_Compiler_Install_Button,'AUI_GO','Install Compiler',form1.ButtonInstallCrossCompiler.Hint);
  alternateui_create_a_button(Control_Cross_Compiler_Update_Button,wi-120-lm,24,120,48,'Update Compilers','alternateUICrossCompilerBox_Panel',drop_arrow_false,button_uses_leave_and_enter_true,'','AUI_UPDATE',icon_size,icon_size);
  Alternate_ui_created:=true;
  alternateui_resize;

  // create buttons to replace normal buttons
  alternateui_create_a_button(Control_Install_Directory_Button,form1.btnInstallDirSelect.Left,form1.btnInstallDirSelect.Top,form1.btnInstallDirSelect.Width,form1.btnInstallDirSelect.Height,'Set InstDir','',drop_arrow_false,button_uses_leave_and_enter_true,'AUI_EXCLAMATION','',0,0);
  (form1.FindComponent(Control_Install_Directory_Button) as TBCButton).Visible:=False;
  alternateui_create_a_button(Control_Clear_Log_Button,form1.btnClearLog.Left,form1.btnClearLog.Top,form1.btnClearLog.Width,form1.btnClearLog.Height,'Clear Log','',drop_arrow_false,button_uses_leave_and_enter_true,'AUI_CLEAR','',0,0);
  (form1.FindComponent(Control_Clear_Log_Button) as TBCButton).Visible:=False;
  alternateui_create_a_button(Control_Auto_Clear_Button,form1.CheckAutoClear.Left,form1.btnClearLog.Top,form1.CheckAutoClear.Width+4,form1.btnClearLog.Height,'Auto Clear','',drop_arrow_false,button_uses_leave_and_enter_true,'AUI_GREEN_TICK_SMALL','',0,0);
  (form1.FindComponent(Control_Auto_Clear_Button) as TBCButton).Visible:=False;
  (form1.FindComponent(Control_Auto_Clear_Button) as TBCButton).Down:=True;

  // create image for logo
  with timage.Create(form1) do
  begin
    setbounds(10,00,90,90);
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(form1.FindComponent(control_base_name+'Panel') as TPanel);
    name:='alternateuiLogo';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    visible:=true;
  end;
  alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLogo') as timage,'AUI_GLOBE');

  // create image for HALT
  with timage.Create(form1) do
  begin
    setbounds(110,390,140,140);
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(form1.FindComponent(control_base_name+'Panel') as TPanel);
    name:='alternateuiHalt';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    visible:=false;
    cursor:=button_Cursor;
    onclick:=@form1.alternateuibutClick;
  end;
  alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiHalt') as timage,'AUI_STOP');

  // create image for FLAGS
  flagt:=4;
  flagl:=wi-96-2;
  with timage.Create(form1) do
  begin
    setbounds(flagl,flagt,24,16);
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(form1.FindComponent(control_base_name+'Panel') as TPanel);
    name:='alternateuiLangEN';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    visible:=true;
    cursor:=button_Cursor;
    onclick:=@form1.alternateuibutClick;
  end;
  alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangEN') as timage,'AUI_LANG_EN');
  flagl:=flagl+24;
  with timage.Create(form1) do
  begin
    setbounds(flagl,flagt,24,16);
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(form1.FindComponent(control_base_name+'Panel') as TPanel);
    name:='alternateuiLangRU';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    visible:=true;
    cursor:=button_Cursor;
    onclick:=@form1.alternateuibutClick;
  end;
  alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangRU') as timage,'AUI_LANG_RU');
  flagl:=flagl+24;
  with timage.Create(form1) do
  begin
    setbounds(flagl,flagt,24,16);
    parent:=(form1.FindComponent('alternateUIMaster_Panel') as TPanel);//(form1.FindComponent(control_base_name+'Panel') as TPanel);
    name:='alternateuiLangFR';
    AntiAliasingMode:=amOn;
    Stretch:=True;
    visible:=true;
    cursor:=button_Cursor;
    onclick:=@form1.alternateuibutClick;
  end;
  alternateui_Get_Png_from_resource(form1.findcomponent('alternateuiLangFR') as timage,'AUI_LANG_FR');

  //Set Help Text;
  alternateui_set_text_variables;
  alternateui_set_language('EN');
end;
{$endif}
end.
