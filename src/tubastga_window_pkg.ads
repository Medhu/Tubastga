--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015-2021  Frank J Jorgensen
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

with Gtk.Window;
with Gtk.Table;
with Gtk.Drawing_Area;
with Gtk.Button;
with Gtk.Text_View;
with Gtk.Text_Buffer;
with Gtk.List_Store;
with Gtk.Box;
with Gtk.Toolbar;
with Gtk.Tool_Button;
with Gtk.Label;
with Gtk.Dialog;
with Gtk.GEntry;
with Gtk.Spin_Button;
with Gtk.Adjustment;
with Gtk.Combo_Box_Text;
with Gtk.Tree_View;
with Hexagon.Client_Map;
with Piece;
with Piece.Client_Piece;
with Goods;
with Ada.Containers.Vectors;
with Ada.Strings;
with Gdk;
with Ada.Strings.Unbounded;
with Gdk.Pixbuf;
with Glib;

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

   type Type_Dlg_Main_Menu_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
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
      Box_Create_Game : Gtk.Box.Gtk_Hbox;
      Lbl_Create_Game_Player_Name_1,
      Lbl_Create_Game_Player_Name_2,
      Lbl_Create_Game_Player_Name_3 : Gtk.Label.Gtk_Label;
      En_Create_Game_Player_Name_1,
      En_Create_Game_Player_Name_2,
      En_Create_Game_Player_Name_3   : Gtk.GEntry.Gtk_Entry;
      Lbl_Create_Game_Chose_Scenario : Gtk.Label.Gtk_Label;
      Cmb_Create_Game_Chose_Scenario : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
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

   type Type_Dlg_Main_Menu_Access is access all Type_Dlg_Main_Menu_Record'Class;

   procedure Gtk_New (P_Dlg_Main_Menu : out Type_Dlg_Main_Menu_Access);
   procedure Initialize (P_Dlg_Main_Menu : access Type_Dlg_Main_Menu_Record'Class);
   --

   type Type_HBox_Performing_Piece is new Gtk.Box.Gtk_Hbox_Record with record
      A_Piece_Id : Piece.Type_Piece_Id;
      --
      Piece_Patch_Area                   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      View_Performing_Piece_Patch_Info   : Gtk.Text_View.Gtk_Text_View;
      Buffer_Performing_Piece_Patch_Info : Gtk.Text_Buffer.Gtk_Text_Buffer;
   end record;

   type Type_HBox_Performing_Piece_Access is access all Type_HBox_Performing_Piece;

   package Performing_Piece_Pkg is new Ada.Containers.Vectors (Positive, Type_HBox_Performing_Piece);
   --
   type Type_Wnd_Action_Record is new Gtk.Window.Gtk_Window_Record with record

      Performing_Content_HBox                 : Gtk.Box.Gtk_Hbox;
      Performing_Scroll_VBox                  : Gtk.Box.Gtk_Vbox;
      Performing_Pieces_List_Store : Gtk.List_Store.Gtk_List_Store;
      Perform_Pieces_Tree_View     : Gtk.Tree_View.Gtk_Tree_View;

      Fighting_Piece_Action1_VBox : Gtk.Box.Gtk_Vbox;
      Fighting_Piece_Action2_VBox : Gtk.Box.Gtk_Vbox;
      House_Piece_Action1_VBox    : Gtk.Box.Gtk_Vbox;
      House_Piece_Action2_VBox    : Gtk.Box.Gtk_Vbox;

      -- Production Center
      Btn_Place_Wall1,
      Btn_Place_Wall2,
      Btn_Place_Wall3,
      Btn_Place_Wall4,
      Btn_Place_Wall5,
      Btn_Place_Wall6,
      --
      Btn_Remove_Wall1,
      Btn_Remove_Wall2,
      Btn_Remove_Wall3,
      Btn_Remove_Wall4,
      Btn_Remove_Wall5,
      Btn_Remove_Wall6 : Gtk.Button.Gtk_Button;
      -- Fighting_Piece
      Btn_Attack, Btn_Ranged_Attack, Btn_Move, Btn_Search, Btn_Promote, Btn_Demote : Gtk.Button.Gtk_Button;
      --
      Btn_Create_Path : Gtk.Button.Gtk_Button;
      Btn_Remove_Path : Gtk.Button.Gtk_Button;
      --
      --
      Target_Content_HBox                 : Gtk.Box.Gtk_Hbox;
      Target_Scroll_VBox                  : Gtk.Box.Gtk_Vbox;

      Target_Pieces_List_Store : Gtk.List_Store.Gtk_List_Store;
      Target_Pieces_Tree_View     : Gtk.Tree_View.Gtk_Tree_View;

      -- Actions
      Btn_Card_1, Btn_Card_2 : Gtk.Button.Gtk_Button;

      Cards_Content_HBox : Gtk.Box.Gtk_Hbox;
      --
      Content_HBox : Gtk.Box.Gtk_Hbox;
   end record;

   type Type_Wnd_Action_Access is access all Type_Wnd_Action_Record'Class;

   procedure Gtk_New (P_Wnd_Action : out Type_Wnd_Action_Access);
   procedure Initialize (P_Wnd_Action : access Type_Wnd_Action_Record'Class);

   type Type_Map_Data is record
      Filename                  : Ada.Strings.Unbounded.Unbounded_String;
      Image_Data                : Gdk.Pixbuf.Gdk_Pixbuf;
      Image_Height, Image_Width : Glib.Gint;
   end record;

   type Type_Wnd_Main_Record is new Gtk.Window.Gtk_Window_Record with record
      Box_Toolbar_Game                                              : Gtk.Box.Gtk_Vbox;
      Toolbar_Main                                                  : Gtk.Toolbar.Gtk_Toolbar;
      Layout_Table                                                  : Gtk.Table.Gtk_Table;
      Area_Map                                                      : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Area_Player_1_Timer, Area_Player_2_Timer, Area_Player_3_Timer : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Lbl_Player_1_Name, Lbl_Player_2_Name, Lbl_Player_3_Name       : Gtk.Label.Gtk_Label;

      View_Activity_Report   : Gtk.Text_View.Gtk_Text_View;
      Buffer_Activity_Report : Gtk.Text_Buffer.Gtk_Text_Buffer;

      View_Hover_Info   : Gtk.Text_View.Gtk_Text_View;
      Buffer_Hover_Info : Gtk.Text_Buffer.Gtk_Text_Buffer;

      View_Resources_Info   : Gtk.Text_View.Gtk_Text_View;
      Buffer_Resources_Info : Gtk.Text_Buffer.Gtk_Text_Buffer;

      Btn_Place_Sentry      : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Ship        : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Farm        : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Tower       : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Lumberjack  : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Bowman      : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Stonecutter : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Knight      : Gtk.Tool_Button.Gtk_Tool_Button;
      Btn_Place_Carrier     : Gtk.Tool_Button.Gtk_Tool_Button;
      --
      --
      Btn_Game : Gtk.Tool_Button.Gtk_Tool_Button;
      --
      Map_Image : Type_Map_Data;

      Wnd_Performing_Patch : Type_Wnd_Action_Access;

      Dlg_Main_Menu          : Type_Dlg_Main_Menu_Access;
   end record;

   type Type_Wnd_Main_Access is access all Type_Wnd_Main_Record'Class;

   procedure Gtk_New (P_Wnd_Main : out Type_Wnd_Main_Access);
   procedure Initialize (P_Wnd_Main : access Type_Wnd_Main_Record'Class);

end Tubastga_Window_Pkg;
