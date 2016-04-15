--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015  Frank J Jorgensen
--
--      This program is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 3 of the License, or
--      (at your option) any later version.
--
--      This program is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Gtk.Window;       use Gtk.Window;
with Gtk.Table;        use Gtk.Table;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Button;       use Gtk.Button;
with Gtk.Text_View;    use Gtk.Text_View;
with Gtk.Text_Buffer;  use Gtk.Text_Buffer;
with Gtk.Box;
with Gtk.Toolbar;
with Gtk.Tool_Button;  use Gtk.Tool_Button;
with Ada;
with Gdk;
with Ada.Strings.Unbounded;
with Gdk.Pixbuf;
with Glib;
with Gtk.Label;        use Gtk.Label;
with Gtk.Dialog;
with Gtk.GEntry;
with Gtk.Spin_Button;
with Gtk.Adjustment;
with Gtk.Combo_Box_Text;
with Hexagon.Client_Map;
with Piece;
with Piece.Client_Piece;
with Goods;

package Tubastga_Window_Pkg is

   type Type_Tower_Id_List is array (1 .. 3) of Piece.Type_Piece_Id;
   type Type_Goods_Id_List is array (1 .. 3) of Goods.Type_Goods;

   type Type_Client_Piece is new Piece.Client_Piece.Type_Client_Piece with record
      Action_Point : Integer;
      Storage      : Goods.Type_Storage (3);
      Stops        : Type_Tower_Id_List := (others => Piece.Undefined_Piece_Id);
      Load, Unload : Type_Goods_Id_List := (others => Goods.None);
      Captain      : Boolean;
   end record;

   type Type_Client_Access_Class is access all Type_Client_Piece'Class;

   type Type_Graphic_Data is record
      Filename                  : Ada.Strings.Unbounded.Unbounded_String;
      Image_Data                : Gdk.Pixbuf.Gdk_Pixbuf;
      Image_Height, Image_Width : Glib.Gint;
   end record;

   type Type_Image_Names is
     (Outside_View_Invisible_On_Minimap,
      Outside_View_Visible_On_Minimap,
      Inside_View_Invisible_On_Minimap,
      Inside_View_Visible_On_Minimap,
      Invisible,
      Reachable,
      Attackable,
      Selected_Patch,
      Selected_Area,
      Grass,
      Mountain,
      Water,
      Forest,

      None,
      Red,
      Green,
      Blue,
      Selected_Piece,
      Chest,
      Wall,
      Sentry_p1,
      Sentry_p2,
      Sentry_p3,
      Knight_p1,
      Knight_p2,
      Knight_p3,
      Bowman_p1,
      Bowman_p2,
      Bowman_p3,
      Ship_p1,
      Ship_p2,
      Ship_p3,
      Farm_p1,
      Farm_p2,
      Farm_p3,
      Tower_p1,
      Tower_p2,
      Tower_p3,
      Lumberjack_p1,
      Lumberjack_p2,
      Lumberjack_p3,
      Stonecutter_p1,
      Stonecutter_p2,
      Stonecutter_p3,
      Carrier_p1,
      Carrier_p2,
      Carrier_p3,

      Wall1,
      Wall2,
      Wall3,
      Wall4,
      Wall5,
      Wall6,

      Minimap_Grass,
      Minimap_Mountain,
      Minimap_Water,
      Minimap_Forest);

   type Type_Images is array (Type_Image_Names) of Type_Graphic_Data;
   type Type_Colors is (Black, White, Grey, Red, Blue, Green, Orange, Yellow);
   type Type_All_Gdk_GC is array (Type_Colors) of Gdk.Gdk_GC;

   type Dlg_MainMenu_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Layout_Table : Gtk.Table.Gtk_Table;

      Btn_Connect_Server,
      Btn_Refresh,
      Btn_Create_Game,
      Btn_Save_Game,
      Btn_Load_Game,
      Btn_Join_Game,
      Btn_Leave_Game,
      Btn_Disconnect_Server,
      Btn_Close : Gtk.Button.Gtk_Button;
      --
      Box_Connect                : Gtk.Box.Gtk_Hbox;
      TCPIPLabel, TCPIPPortLabel : Gtk.Label.Gtk_Label;
      Adj_TCPIPOctet1,
      Adj_TCPIPOctet2,
      Adj_TCPIPOctet3,
      Adj_TCPIPOctet4,
      Adj_TCPIPPort : Gtk.Adjustment.Gtk_Adjustment;
      Spn_TCPIPOctet1,
      Spn_TCPIPOctet2,
      Spn_TCPIPOctet3,
      Spn_TCPIPOctet4,
      Spn_TCPIPPort : Gtk.Spin_Button.Gtk_Spin_Button;
      --
      Box_Create_Game                                              : Gtk.Box.Gtk_Hbox;
      Lbl_Create_Game_Player_Name_1, Lbl_Create_Game_Player_Name_2, Lbl_Create_Game_Player_Name_3 : Gtk.Label.Gtk_Label;
      En_Create_Game_Player_Name_1, En_Create_Game_Player_Name_2, En_Create_Game_Player_Name_3   : Gtk.GEntry.Gtk_Entry;
      Lbl_Create_Game_Chose_Map                                    : Gtk.Label.Gtk_Label;
      Cmb_Create_Game_Chose_Map : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      --
      Box_Save_Game      : Gtk.Box.Gtk_Hbox;
      Lbl_Save_Game_Name : Gtk.Label.Gtk_Label;
      En_Save_Game_Name  : Gtk.GEntry.Gtk_Entry;
      --
      Box_Load_Game      : Gtk.Box.Gtk_Hbox;
      Lbl_Load_Game_Name : Gtk.Label.Gtk_Label;
      Cmb_Load_Game_Name : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      --
      Box_Join_Game        : Gtk.Box.Gtk_Hbox;
      Lbl_Join_Player_Name : Gtk.Label.Gtk_Label;
      Cmb_Join_Player_Name : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
      Lbl_Join_Game        : Gtk.Label.Gtk_Label;

   end record;

   type Dlg_MainMenu_Access is access all Dlg_MainMenu_Record'Class;

   procedure Gtk_New (dlg_MainMenu : out Dlg_MainMenu_Access);
   procedure Initialize (dlg_MainMenu : access Dlg_MainMenu_Record'Class);

   --
   type Wnd_Performing_Piece_Record is new Gtk_Window_Record with record

      Layout_Table : Gtk.Table.Gtk_Table;

      Piece_Info_Box                  : Gtk.Box.Gtk_Hbox;
      Construction_Box                : Gtk.Box.Gtk_Hbox;
      Fighting_Piece_Box              : Gtk.Box.Gtk_Hbox;
      Path_Box                        : Gtk.Box.Gtk_Hbox;
      View_Performing_Piece_Patch_Box : Gtk.Box.Gtk_Hbox;

      Lbl_Piece_Name, Lbl_Piece_Type : Gtk_Label;
      En_Piece_Name                  : Gtk.GEntry.Gtk_Entry;

      --
      -- Production Center
      Btn_Wall1, Btn_Wall2, Btn_Wall3, Btn_Wall4, Btn_Wall5, Btn_Wall6 : Gtk_Button;
      -- Fighting_Piece
      Btn_Attack, Btn_Move, Btn_Search, Btn_Promote, Btn_Demote : Gtk_Button;
      --
      Selected_Piece : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
      --
      View_Performing_Piece_Patch_Info   : Gtk_Text_View;
      Buffer_Performing_Piece_Patch_Info : Gtk_Text_Buffer;
      --
      Btn_Create_Path : Gtk_Button;
      Btn_Remove_Path : Gtk_Button;
   end record;

   type Wnd_Piece_Access is access all Wnd_Performing_Piece_Record'Class;

   procedure Gtk_New (Wnd_Performing_Piece : out Wnd_Piece_Access);
   procedure Initialize (Wnd_Performing_Piece : access Wnd_Performing_Piece_Record'Class);

   type Wnd_Target_Record is new Gtk_Window_Record with record

      Layout_Table : Gtk.Table.Gtk_Table;

      Piece_Info_Box                 : Gtk.Box.Gtk_Hbox;
      Fighting_Piece_Box             : Gtk.Box.Gtk_Hbox;
      Lbl_Piece_Name, Lbl_Piece_Type : Gtk_Label;
      En_Piece_Name                  : Gtk.GEntry.Gtk_Entry;

      View_Target_Piece_Patch_Box : Gtk.Box.Gtk_Hbox;

      Selected_Piece : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      View_Target_Piece_Patch_Info   : Gtk_Text_View;
      Buffer_Target_Piece_Patch_Info : Gtk_Text_Buffer;

   end record;

   type Wnd_Target_Access is access all Wnd_Target_Record'Class;

   procedure Gtk_New (Wnd_Target : out Wnd_Target_Access);
   procedure Initialize (Wnd_Target : access Wnd_Target_Record'Class);

   type Wnd_Main_Record is new Gtk_Window_Record with record
      Box_Toolbar_Game                   : Gtk.Box.Gtk_Vbox;
      Toolbar_Main                       : Gtk.Toolbar.Gtk_Toolbar;
      Layout_Table                       : Gtk_Table;
      Area_Map                           : Gtk_Drawing_Area;
      Area_Player_1_Timer, Area_Player_2_Timer, Area_Player_3_Timer : Gtk_Drawing_Area;
      Lbl_Player_1_Name, Lbl_Player_2_Name, Lbl_Player_3_Name     : Gtk_Label;

      View_Activity_Report   : Gtk_Text_View;
      Buffer_Activity_Report : Gtk_Text_Buffer;

      View_Hover_Info   : Gtk_Text_View;
      Buffer_Hover_Info : Gtk_Text_Buffer;

      View_Resources_Info   : Gtk_Text_View;
      Buffer_Resources_Info : Gtk_Text_Buffer;

      Btn_End_Turn          : Gtk_Tool_Button;
      Btn_Place_Sentry      : Gtk_Tool_Button;
      Btn_Place_Ship        : Gtk_Tool_Button;
      Btn_Place_Farm        : Gtk_Tool_Button;
      Btn_Place_Tower       : Gtk_Tool_Button;
      Btn_Place_Lumberjack  : Gtk_Tool_Button;
      Btn_Place_Bowman      : Gtk_Tool_Button;
      Btn_Place_Stonecutter : Gtk_Tool_Button;
      Btn_Place_Knight      : Gtk_Tool_Button;
      Btn_Place_Carrier     : Gtk_Tool_Button;
      --
      --
      Btn_Game : Gtk_Tool_Button;
      --
      Map_Image  : Type_Graphic_Data;
      All_Images : Type_Images;
      All_GC     : Type_All_Gdk_GC;
      --
      Wnd_Performing_Piece : Wnd_Piece_Access;
      Wnd_Target           : Wnd_Target_Access;
      dlgMainMenu          : Dlg_MainMenu_Access;
   end record;

   type Wnd_Main_Access is access all Wnd_Main_Record'Class;

   procedure Gtk_New (wndMain : out Wnd_Main_Access);
   procedure Initialize (wndMain : access Wnd_Main_Record'Class);

end Tubastga_Window_Pkg;
