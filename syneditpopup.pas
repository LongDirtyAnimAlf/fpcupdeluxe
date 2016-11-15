unit SynEditPopup;

interface

uses
 ActnList,
 Menus,
 Classes,
 SynEdit;

type
  TSynEdit = class(SynEdit.TSynEdit)
  private
    FActnList: TActionList;
    FPopupMenu : TPopupMenu;
    procedure CreateActns;
    procedure FillPopupMenu(APopupMenu : TPopupMenu);
    procedure CopyExecute(Sender: TObject);
    procedure CopyUpdate(Sender: TObject);
    procedure SelectAllExecute(Sender: TObject);
    procedure SelectAllUpdate(Sender: TObject);
    procedure SetPopupMenu_(const Value: TPopupMenu);
    function  GetPopupMenu_: TPopupMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PopupMenu: TPopupMenu read GetPopupMenu_ write SetPopupMenu_;
  end;

implementation

uses
 SysUtils;

const
 MenuName='uSynEditPopupMenu';

procedure TSynEdit.CopyExecute(Sender: TObject);
begin
  Self.CopyToClipboard;
end;

procedure TSynEdit.CopyUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled :=Self.SelAvail;
end;

procedure TSynEdit.SelectAllExecute(Sender: TObject);
begin
 Self.SelectAll;
end;

procedure TSynEdit.SelectAllUpdate(Sender: TObject);
begin
 TAction(Sender).Enabled :=Self.Lines.Text<>'';
end;

constructor TSynEdit.Create(AOwner: TComponent);
begin
  inherited;
  FActnList:=TActionList.Create(Self);
  FPopupMenu:=TPopupMenu.Create(Self);
  FPopupMenu.Name:=MenuName;
  CreateActns;
  FillPopupMenu(FPopupMenu);
  PopupMenu:=FPopupMenu;
end;

procedure TSynEdit.CreateActns;

 procedure AddActItem(const AText:string;AShortCut : TShortCut;AEnabled:Boolean;OnExecute,OnUpdate:TNotifyEvent);
 Var
    ActionItem  : TAction;
  begin
    ActionItem:=TAction.Create(FActnList);
    ActionItem.ActionList:=FActnList;
    ActionItem.Caption:=AText;
    ActionItem.ShortCut:=AShortCut;
    ActionItem.Enabled:=AEnabled;
    ActionItem.OnExecute:=OnExecute;
    ActionItem.OnUpdate:=OnUpdate;
  end;

begin
  AddActItem('&Copy',Menus.ShortCut(Word('C'), [ssCtrl]),False, @CopyExecute, @CopyUpdate);
  AddActItem('-',0,False,nil,nil);
  AddActItem('Select &All',Menus.ShortCut(Word('A'), [ssCtrl]),False, @SelectAllExecute, @SelectAllUpdate);
end;

procedure TSynEdit.SetPopupMenu_(const Value: TPopupMenu);
Var
  MenuItem : TMenuItem;
begin
  SynEdit.TSynEdit(Self).PopupMenu:=Value;
  if CompareText(MenuName,Value.Name)<>0 then
  begin
   MenuItem:=TMenuItem.Create(Value);
   MenuItem.Caption:='-';
   Value.Items.Add(MenuItem);
   FillPopupMenu(Value);
  end;
end;

function TSynEdit.GetPopupMenu_: TPopupMenu;
begin
  Result:=SynEdit.TSynEdit(Self).PopupMenu;
end;

destructor TSynEdit.Destroy;
begin
  FPopupMenu.Free;
  FActnList.Free;
  inherited;
end;

procedure TSynEdit.FillPopupMenu(APopupMenu : TPopupMenu);
var
  i        : integer;
  MenuItem : TMenuItem;
begin
  if Assigned(FActnList) then
  for i := 0 to FActnList.ActionCount-1 do
  begin
    MenuItem:=TMenuItem.Create(APopupMenu);
    MenuItem.Action  :=FActnList.Actions[i];
    APopupMenu.Items.Add(MenuItem);
  end;
end;

end.
