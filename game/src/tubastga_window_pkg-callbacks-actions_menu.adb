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
with Text_IO;
with Landscape;
with Glib;
with Utilities;
with Tubastga_Window_Pkg.FullsizeView;
with Gtk.Tree_Selection;
with Gtk.Tree_Model;
with Gtk.Enums;
with Gtkada.Dialogs;
with Action;
with Tubastga_Game;
with Tubastga_UI_Aux;
with Effect;
with Hexagon.Area;
with Tubastga_Window_Pkg.Images;

package body Tubastga_Window_Pkg.Callbacks.Actions_Menu is

   Verbose : constant Boolean := False;

   procedure Activate_Action_Buttons
     (P_Window : in out Type_Wnd_Action_Access; P_Type_Category : in Piece.Type_Category)
   is
      use Piece;
   begin
      if P_Type_Category = Piece.Fighting_Piece then
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.House_Piece_Action1_VBox), False);
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.House_Piece_Action2_VBox), False);
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.Fighting_Piece_Action1_VBox), True);
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.Fighting_Piece_Action2_VBox), True);
      elsif P_Type_Category = Piece.House_Piece then
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.House_Piece_Action1_VBox), True);
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.House_Piece_Action2_VBox), True);
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.Fighting_Piece_Action1_VBox), False);
         Gtk.Widget.Set_Sensitive
           (Gtk.Widget.Gtk_Widget (P_Window.all.Fighting_Piece_Action2_VBox), False);
      end if;
   end Activate_Action_Buttons;

   procedure Set_Selected_Performing_Patch_Window
     (P_Window : in out Type_Wnd_Action_Access;
      P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
      Trav       : Landscape.Pieces_Here_List.Cursor;
      A_Piece_Id : Piece.Type_Piece_Id;
      A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      List_Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

      Trav_Effect : Effect.Effect_List.Cursor;

      use Gtk.Tree_Selection;
      use Gtk.Tree_Model;
      use Hexagon.Client_Map;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.Set_Selected_Performing_Patch_Window - enter");
      end if;

      if P_Patch = null then
         return;
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (P_Window.all.Performing_Pieces_Tree_View);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      if Selected_Model /= Gtk.Tree_Model.Null_Gtk_Tree_Model and
        Selected_Iter /= Gtk.Tree_Model.Null_Iter
      then
         A_Path := Gtk.Tree_Model.Get_Path (Selected_Model, Selected_Iter);
      end if;

      Gtk.List_Store.Clear (P_Window.all.Performing_Pieces_List_Store);
      Trav := Landscape.Pieces_Here_List.First (P_Patch.all.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav) loop
         A_Piece_Id := Landscape.Pieces_Here_List.Element (Trav);
         A_Piece    := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

         Tubastga_Window_Pkg.Callbacks.Actions_Menu.Activate_Action_Buttons
           (P_Window, A_Piece.all.Category);

         Gtk.List_Store.Append (P_Window.Performing_Pieces_List_Store, List_Store_Iter);
         Gtk.List_Store.Set
           (P_Window.Performing_Pieces_List_Store, List_Store_Iter, 0, Glib.Gint (A_Piece.all.Id));
         Gtk.List_Store.Set
           (P_Window.Performing_Pieces_List_Store, List_Store_Iter, 1,
            Tubastga_Window_Pkg.Images.Get_Image
              (Tubastga_Window_Pkg.Images.All_Images,
               Tubastga_Window_Pkg.Images.Find_Piece_Image
                 (Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all)))
              .Image_Data);
         Gtk.List_Store.Set
           (P_Window.Performing_Pieces_List_Store, List_Store_Iter, 2,
            Utilities.RemoteString.To_String (A_Piece.all.Name));

         Trav := Landscape.Pieces_Here_List.Next (Trav);
      end loop;

      if Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces) /=
        Piece.Undefined_Piece_Id
      then

         Selected_Record := Gtk.Tree_View.Get_Selection (P_Window.all.Performing_Pieces_Tree_View);

         if A_Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
            Gtk.Tree_Selection.Select_Path (Selected_Record, A_Path);
         end if;

      end if;

      --
      --
      if Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces) /=
        Piece.Undefined_Piece_Id
      then
         declare
            A_Piece_Id : Piece.Type_Piece_Id;
            A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
            An_Effect  : Effect.Type_Effect;

            use Piece.Client_Piece;
         begin
            A_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
            A_Piece    := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

            if A_Piece /= null then

               Gtk.List_Store.Clear (P_Window.all.Performing_Piece_Effects_List_Store);
               Trav_Effect := Effect.Effect_List.First (A_Piece.all.Effects_On_Piece);

               while Effect.Effect_List.Has_Element (Trav_Effect) loop

                  An_Effect := Effect.Effect_List.Element (Trav_Effect);

                  Gtk.List_Store.Append
                    (P_Window.Performing_Piece_Effects_List_Store, List_Store_Iter);
                  Gtk.List_Store.Set
                    (P_Window.Performing_Piece_Effects_List_Store, List_Store_Iter, 0,
                     Glib.Gint (An_Effect.Effect_Name));
                  Gtk.List_Store.Set
                    (P_Window.Performing_Piece_Effects_List_Store, List_Store_Iter, 1,
                     Tubastga_Window_Pkg.Images.Get_Image
                       (Tubastga_Window_Pkg.Images.All_Images,
                        Tubastga_Window_Pkg.Images.Find_Piece_Image
                          (Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all)))
                       .Image_Data);
                  Gtk.List_Store.Set
                    (P_Window.Performing_Piece_Effects_List_Store, List_Store_Iter, 2,
                     Utilities.RemoteString.To_String
                       (Tubastga_Game.Effect_Type_Info_List (An_Effect.Effect_Name).Type_Name) &
                     " (" & An_Effect.Aux'Img & ")");

                  Trav_Effect := Effect.Effect_List.Next (Trav_Effect);
               end loop;

            end if;

         exception
            when others =>
               Text_IO.Put_Line ("Effect Exception ");
               Text_IO.Put_Line ("A_Piece_Id:" & A_Piece_Id'Img);
               raise;
         end;

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.Set_Selected_Performing_Patch_Window - exit");
      end if;
   end Set_Selected_Performing_Patch_Window;

   procedure Set_Selected_Target_Patch_Window
     (P_Window : in out Type_Wnd_Action_Access;
      P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
      Trav       : Landscape.Pieces_Here_List.Cursor;
      A_Piece_Id : Piece.Type_Piece_Id;
      A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      List_Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

      Trav_Effect : Effect.Effect_List.Cursor;

      use Gtk.Tree_Selection;
      use Gtk.Tree_Model;
      use Hexagon.Client_Map;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.Set_Selected_Target_Patch_Window - enter");
      end if;

      if P_Patch = null then
         return;
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (P_Window.all.Target_Pieces_Tree_View);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      if Selected_Model /= Gtk.Tree_Model.Null_Gtk_Tree_Model and
        Selected_Iter /= Gtk.Tree_Model.Null_Iter
      then
         A_Path := Gtk.Tree_Model.Get_Path (Selected_Model, Selected_Iter);
      end if;

      Gtk.List_Store.Clear (P_Window.all.Target_Pieces_List_Store);
      Trav := Landscape.Pieces_Here_List.First (P_Patch.all.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav) loop
         A_Piece_Id := Landscape.Pieces_Here_List.Element (Trav);
         A_Piece    := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

         Gtk.List_Store.Append (P_Window.all.Target_Pieces_List_Store, List_Store_Iter);
         Gtk.List_Store.Set
           (P_Window.all.Target_Pieces_List_Store, List_Store_Iter, 0, Glib.Gint (A_Piece.all.Id));
         Gtk.List_Store.Set
           (P_Window.all.Target_Pieces_List_Store, List_Store_Iter, 1,
            Tubastga_Window_Pkg.Images.Get_Image
              (Tubastga_Window_Pkg.Images.All_Images,
               Tubastga_Window_Pkg.Images.Find_Piece_Image
                 (Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all)))
              .Image_Data);
         Gtk.List_Store.Set
           (P_Window.all.Target_Pieces_List_Store, List_Store_Iter, 2,
            Utilities.RemoteString.To_String (A_Piece.all.Name));

         Trav := Landscape.Pieces_Here_List.Next (Trav);
      end loop;

      if Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (RB_Selected_Pieces) /=
        Piece.Undefined_Piece_Id
      then

         Selected_Record := Gtk.Tree_View.Get_Selection (P_Window.all.Target_Pieces_Tree_View);

         if A_Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
            Gtk.Tree_Selection.Select_Path (Selected_Record, A_Path);
         end if;

      end if;

      if Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (RB_Selected_Pieces) /=
        Piece.Undefined_Piece_Id
      then
         declare
            A_Piece_Id : Piece.Type_Piece_Id;
            A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
            An_Effect  : Effect.Type_Effect;
         begin
            A_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (RB_Selected_Pieces);
            A_Piece    := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

            Gtk.List_Store.Clear (P_Window.all.Target_Piece_Effects_List_Store);
            Trav_Effect := Effect.Effect_List.First (A_Piece.all.Effects_On_Piece);

            while Effect.Effect_List.Has_Element (Trav_Effect) loop

               An_Effect := Effect.Effect_List.Element (Trav_Effect);

               Gtk.List_Store.Append (P_Window.Target_Piece_Effects_List_Store, List_Store_Iter);
               Gtk.List_Store.Set
                 (P_Window.Target_Piece_Effects_List_Store, List_Store_Iter, 0,
                  Glib.Gint (An_Effect.Effect_Name));
               Gtk.List_Store.Set
                 (P_Window.Target_Piece_Effects_List_Store, List_Store_Iter, 1,
                  Tubastga_Window_Pkg.Images.Get_Image
                    (Tubastga_Window_Pkg.Images.All_Images,
                     Tubastga_Window_Pkg.Images.Find_Piece_Image
                       (Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all)))
                    .Image_Data);
               Gtk.List_Store.Set
                 (P_Window.Target_Piece_Effects_List_Store, List_Store_Iter, 2,
                  Utilities.RemoteString.To_String
                    (Tubastga_Game.Effect_Type_Info_List (An_Effect.Effect_Name).Type_Name) &
                  " (" & An_Effect.Aux'Img & ")");

               Trav_Effect := Effect.Effect_List.Next (Trav_Effect);
            end loop;
         end;

      end if;
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.Set_Selected_Target_Patch_Window - exit");
      end if;
   end Set_Selected_Target_Patch_Window;

   procedure On_Performing_Patch_Tree_View
     (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class)
   is
      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Performing_Patch_Tree_View - enter");
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (Object);

      Gtk.Tree_Selection.Set_Mode (Selected_Record, Gtk.Enums.Selection_Single);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      A_Piece_Id := Piece.Type_Piece_Id (Gtk.Tree_Model.Get_Int (Selected_Model, Selected_Iter, 0));

      Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece
        (Tubastga_Window_Pkg.Callbacks.LB_Selected_Pieces, A_Piece_Id, False);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Performing_Patch_Tree_View - exit ");
      end if;
   end On_Performing_Patch_Tree_View;

   procedure On_Performing_Patch_Piece_Effects_Tree_View
     (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class)
   is
      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Performing_Patch_Piece_Effects_Tree_View - enter");
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (Object);

      Gtk.Tree_Selection.Set_Mode (Selected_Record, Gtk.Enums.Selection_Single);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      A_Piece_Id := Piece.Type_Piece_Id (Gtk.Tree_Model.Get_Int (Selected_Model, Selected_Iter, 0));

      Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece
        (Tubastga_Window_Pkg.Callbacks.LB_Selected_Pieces, A_Piece_Id, False);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Performing_Patch_Piece_Effects_Tree_View - exit ");
      end if;
   end On_Performing_Patch_Piece_Effects_Tree_View;

   procedure On_Target_Patch_Piece_Effects_Tree_View
     (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class)
   is
      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Target_Patch_Piece_Effects_Tree_View - enter");
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (Object);

      Gtk.Tree_Selection.Set_Mode (Selected_Record, Gtk.Enums.Selection_Single);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      A_Piece_Id := Piece.Type_Piece_Id (Gtk.Tree_Model.Get_Int (Selected_Model, Selected_Iter, 0));

      Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece
        (Tubastga_Window_Pkg.Callbacks.RB_Selected_Pieces, A_Piece_Id, False);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Target_Patch_Piece_Effects_Tree_View - exit ");
      end if;
   end On_Target_Patch_Piece_Effects_Tree_View;

   function Validate_Search (P_Searching_Piece_Id : in Piece.Type_Piece_Id) return Boolean is
      Tmp : Gtkada.Dialogs.Message_Dialog_Buttons;
      Ret : Boolean := True;

      use Piece;
   begin

      if P_Searching_Piece_Id = Piece.Undefined_Piece_Id then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose a piece to perform the searching");
         Ret := False;

      end if;

      return Ret;
   end Validate_Search;

   function Validate_Promote_Demote (P_Promoting_Piece_Id : in Piece.Type_Piece_Id) return Boolean
   is
      Tmp : Gtkada.Dialogs.Message_Dialog_Buttons;
      Ret : Boolean := True;

      use Piece;
   begin

      if P_Promoting_Piece_Id = Piece.Undefined_Piece_Id then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose a piece to perform the construction");
         Ret := False;

      end if;

      return Ret;
   end Validate_Promote_Demote;

   function Validate_Construct (P_Constructing_Piece_Id : in Piece.Type_Piece_Id) return Boolean is
      Tmp : Gtkada.Dialogs.Message_Dialog_Buttons;
      Ret : Boolean := True;

      use Piece;
   begin

      if P_Constructing_Piece_Id = Piece.Undefined_Piece_Id then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose a piece to perform the construction");
         Ret := False;

      end if;

      return Ret;
   end Validate_Construct;

   function Validate_Move (P_Moving_Piece_Id : in Piece.Type_Piece_Id) return Boolean is
      Tmp : Gtkada.Dialogs.Message_Dialog_Buttons;
      Ret : Boolean := True;

      use Piece;
   begin

      if P_Moving_Piece_Id = Piece.Undefined_Piece_Id then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose a piece to move");
         Ret := False;

      end if;

      return Ret;
   end Validate_Move;

   function Validate_Attack
     (P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id) return Boolean
   is
      Tmp : Gtkada.Dialogs.Message_Dialog_Buttons;
      Ret : Boolean := True;

      use Piece;
   begin

      if P_Attacking_Piece_Id = Piece.Undefined_Piece_Id and
        P_Attacked_Piece_Id = Piece.Undefined_Piece_Id
      then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose an attacker and a target");
         Ret := False;

      elsif P_Attacking_Piece_Id = Piece.Undefined_Piece_Id and
        P_Attacked_Piece_Id /= Piece.Undefined_Piece_Id
      then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose an attacker");
         Ret := False;

      elsif P_Attacking_Piece_Id /= Piece.Undefined_Piece_Id and
        P_Attacked_Piece_Id = Piece.Undefined_Piece_Id
      then
         Tmp := Gtkada.Dialogs.Message_Dialog ("Please choose a target");
         Ret := False;

      end if;

      return Ret;
   end Validate_Attack;

   procedure On_Button_Place_Wall1 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Place_Wall1 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Wall1, 0),
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall1;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Place_Wall1 - exception");
         raise;
   end On_Button_Place_Wall1;

   procedure On_Button_Place_Wall2 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Place_Wall2 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Wall2, 0),
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall2;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Place_Wall2 - exception");
         raise;
   end On_Button_Place_Wall2;

   procedure On_Button_Place_Wall3 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Place_Wall3 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Wall3, 0),
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall3;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Place_Wall3 - exception");
         raise;
   end On_Button_Place_Wall3;

   procedure On_Button_Place_Wall4 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Place_Wall4 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Wall4, 0),
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall4;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Place_Wall4 - exception");
         raise;
   end On_Button_Place_Wall4;

   procedure On_Button_Place_Wall5 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Place_Wall5 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Wall5, 0),
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall5;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Place_Wall5 - exception");
         raise;
   end On_Button_Place_Wall5;

   procedure On_Button_Place_Wall6 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Place_Wall6 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Wall6, 0),
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall6;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Place_Wall6 - exception");
         raise;
   end On_Button_Place_Wall6;
   --
   --
   -- Remove_WallN
   procedure On_Button_Remove_Wall1 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Wall1 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Revoke_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Wall1,
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Remove_Wall1;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Remove_Wall1 - exception");
         raise;
   end On_Button_Remove_Wall1;

   procedure On_Button_Remove_Wall2 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Wall2 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Revoke_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Wall2,
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Remove_Wall2;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Remove_Wall2 - exception");
         raise;
   end On_Button_Remove_Wall2;

   procedure On_Button_Remove_Wall3 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Wall3 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Revoke_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Wall3,
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Remove_Wall3;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Remove_Wall3 - exception");
         raise;
   end On_Button_Remove_Wall3;

   procedure On_Button_Remove_Wall4 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Wall4 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Revoke_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Wall4,
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Remove_Wall4;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Remove_Wall4 - exception");
         raise;
   end On_Button_Remove_Wall4;

   procedure On_Button_Remove_Wall5 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Wall5 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Revoke_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Wall5,
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Remove_Wall5;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Remove_Wall5 - exception");
         raise;
   end On_Button_Remove_Wall5;

   procedure On_Button_Remove_Wall6 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Wall6 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Construct (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);

         Piece.Client_Piece.Revoke_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Wall6,
            Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Pos));

         Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Remove_Wall6;
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Remove_Wall6 - exception");
         raise;
   end On_Button_Remove_Wall6;

   procedure On_Button_Move (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Move - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Move (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);
         Selected_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

         Piece.Client_Piece.Perform_Move
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Landscape.Type_Patch (Selected_Patch.all));

      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Move - exception");
         raise;
   end On_Button_Move;

   procedure On_Button_Attack (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece_LB    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id_LB : Piece.Type_Piece_Id;
      Selected_Patch_LB    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos_LB      : Hexagon.Type_Hexagon_Position;

      Selected_Piece_RB    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id_RB : Piece.Type_Piece_Id;
      Selected_Patch_RB    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos_RB      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Attack - clicked");
      end if;

      Selected_Piece_Id_LB :=
        Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
      Selected_Piece_Id_RB :=
        Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (RB_Selected_Pieces);

      if Validate_Attack (Selected_Piece_Id_LB, Selected_Piece_Id_RB) then

         Selected_Piece_LB := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id_LB);
         Selected_Pos_LB   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
         Selected_Patch_LB :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos_LB.A, Selected_Pos_LB.B);

         Selected_Piece_RB := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id_RB);
         Selected_Pos_RB   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);
         Selected_Patch_RB :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos_RB.A, Selected_Pos_RB.B);

         Piece.Client_Piece.Perform_Attack
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece_LB.all),
            Piece.Type_Piece (Selected_Piece_RB.all));

      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Attack - exception");
         raise;
   end On_Button_Attack;

   procedure On_Button_Ranged_Attack (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece_LB    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id_LB : Piece.Type_Piece_Id;
      Selected_Patch_LB    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos_LB      : Hexagon.Type_Hexagon_Position;

      Selected_Piece_RB    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id_RB : Piece.Type_Piece_Id;
      Selected_Patch_RB    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos_RB      : Hexagon.Type_Hexagon_Position;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Ranged_Attack - clicked");
      end if;

      Selected_Piece_Id_LB :=
        Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
      Selected_Piece_Id_RB :=
        Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (RB_Selected_Pieces);

      if Validate_Attack (Selected_Piece_Id_LB, Selected_Piece_Id_RB) then

         Selected_Piece_LB := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id_LB);
         Selected_Pos_LB   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
         Selected_Patch_LB :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos_LB.A, Selected_Pos_LB.B);

         Selected_Piece_RB := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id_RB);
         Selected_Pos_RB   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (RB_Selected_Pos);
         Selected_Patch_RB :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos_RB.A, Selected_Pos_RB.B);

         Piece.Client_Piece.Perform_Ranged_Attack
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece_LB.all),
            Piece.Type_Piece (Selected_Piece_RB.all));

      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Ranged_Attack - exception");
         raise;
   end On_Button_Ranged_Attack;

   procedure On_Button_Promote (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Promote - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Promote_Demote (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
         Selected_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

         Piece.Client_Piece.Grant_Piece_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'(Tubastga_Game.Effect_Captain, 1));
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Promote - exception");
         raise;
   end On_Button_Promote;

   procedure On_Button_Demote (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Demote - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Promote_Demote (Selected_Piece_Id) then
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
         Selected_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

         Piece.Client_Piece.Revoke_Piece_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Tubastga_Game.Effect_Captain);
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Demote - exception");
         raise;
   end On_Button_Demote;

   procedure On_Button_Search (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

      Effect_Cursor : Effect.Effect_List.Cursor;
      An_Effect     : Effect.Type_Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Search - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

      if Validate_Search (Selected_Piece_Id) then

         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
         Selected_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

         Effect_Cursor :=
           Effect.Effect_List.Find (Selected_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure);
         if Effect.Effect_List.Has_Element (Effect_Cursor) then
            An_Effect := Effect.Effect_List.Element (Effect_Cursor);

            Piece.Client_Piece.Perform_Patch_Effect
              (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
               An_Effect.Effect_Name,
               Hexagon.Area.Type_Action_Capabilities_A'(1 => Selected_Patch.all.Pos));
         end if;

      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Search - exception");
         raise;
   end On_Button_Search;

   procedure On_Button_Create_Path (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Create_Path - clicked");
      end if;

      declare
         The_Area : Hexagon.Area.Type_Action_Capabilities_A
           (1 .. Integer (Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Length (RB_Selected_Pos)));
         Trav_Pos   : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Cursor;
         Area_Index : Integer;
         A_Pos      : Hexagon.Type_Hexagon_Position;
      begin

         Area_Index := 1;
         Trav_Pos   := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (RB_Selected_Pos);
         while Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Has_Element (Trav_Pos) loop
            A_Pos                 := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav_Pos);
            The_Area (Area_Index) := Hexagon.Type_Hexagon_Position'(True, A_Pos.A, A_Pos.B);
            Trav_Pos              := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Next (Trav_Pos);
            Area_Index            := Area_Index + 1;
         end loop;
         --
         Selected_Piece_Id :=
           Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
         Selected_Piece := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
         Selected_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
         Selected_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
            Effect.Type_Effect'
              (Tubastga_Game.Effect_Path,
               Integer (Me_Player_Id) * 1_000_000 + Integer (Selected_Piece.all.Id) * 10 + 0),
            The_Area);
      end;

   end On_Button_Create_Path;

   procedure On_Button_Remove_Path (Object : access Gtk.Button.Gtk_Button_Record'Class) is

      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

      The_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1);

      use Hexagon.Client_Map;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Remove_Path - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
      Selected_Piece    := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
      Selected_Pos      := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
      Selected_Patch    :=
        Hexagon.Client_Map.Get_Patch_Adress_From_AB (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

      The_Area (1) :=
        Hexagon.Type_Hexagon_Position'(True, Selected_Patch.all.Pos.A, Selected_Patch.all.Pos.B);

      Piece.Client_Piece.Grant_Patch_Effect
        (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
         Effect.Type_Effect'
           (Tubastga_Game.Effect_Path,
            (Integer (Me_Player_Id) * 1_000_000 + Integer (Selected_Piece.all.Id) * 10 + 1)),
         The_Area);

   end On_Button_Remove_Path;

   procedure On_Button_Card_1 (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Selected_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Selected_Piece_Id : Piece.Type_Piece_Id;
      Selected_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Selected_Pos      : Hexagon.Type_Hexagon_Position;

      --The_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1);

      use Hexagon.Client_Map;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Card_1 - clicked");
      end if;

      Selected_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
      Selected_Piece    := Piece.Client_Piece.Find_Piece_In_List (Selected_Piece_Id);
      Selected_Pos      := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
      Selected_Patch    :=
        Hexagon.Client_Map.Get_Patch_Adress_From_AB (A_Client_Map, Selected_Pos.A, Selected_Pos.B);

      Piece.Client_Piece.Grant_Piece_Effect
        (Me_Player_Id, Action.Type_Action_Type (1), Piece.Type_Piece (Selected_Piece.all),
         Effect.Type_Effect'(Tubastga_Game.Effect_Card_1, 1));

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Card_1 - exception");
         raise;
   end On_Button_Card_1;

   procedure On_Button_Card_2 (Object : access Gtk.Button.Gtk_Button_Record'Class) is

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Button_Card_2 - clicked");
      end if;

   exception
      when others =>
         Text_IO.Put_Line ("On_Button_Card_2 - exception");
         raise;
   end On_Button_Card_2;

   procedure On_Target_Patch_Tree_View (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class) is
      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Target_Patch_Tree_View - enter");
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (Object);

      Gtk.Tree_Selection.Set_Mode (Selected_Record, Gtk.Enums.Selection_Single);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      A_Piece_Id := Piece.Type_Piece_Id (Gtk.Tree_Model.Get_Int (Selected_Model, Selected_Iter, 0));

      Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece (RB_Selected_Pieces, A_Piece_Id, False);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Actions_Menu.On_Target_Patch_Tree_View - exit ");
      end if;
   end On_Target_Patch_Tree_View;

end Tubastga_Window_Pkg.Callbacks.Actions_Menu;
