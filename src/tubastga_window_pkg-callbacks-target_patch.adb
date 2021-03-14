--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015-2017  Frank J Jorgensen
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
with Tubastga_Window_Pkg.FullsizeView;
with Utilities;
with Gtk.Tree_Selection;
with Gtk.Tree_Model;
with Gtk.Enums;
with Tubastga_Window_Pkg.Images;

package body Tubastga_Window_Pkg.Callbacks.Target_Patch is

   Verbose : constant Boolean := False;

   procedure On_Target_Patch_Tree_View (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class) is
      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Target_Patch.On_Target_Patch_Tree_View - enter");
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (Object);

      Gtk.Tree_Selection.Set_Mode (Selected_Record, Gtk.Enums.Selection_Single);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      A_Piece_Id := Piece.Type_Piece_Id (Gtk.Tree_Model.Get_Int (Selected_Model, Selected_Iter, 0));

      Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece (RB_Selected_Pieces, A_Piece_Id, False);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Target_Patch.On_Target_Patch_Tree_View - exit ");
      end if;
   end On_Target_Patch_Tree_View;

   procedure Set_Target_Patch_Window (P_Window : in out Type_Wnd_Action_Access;
      P_Patch                                  : in     Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
      Trav       : Landscape.Pieces_Here_List.Cursor;
      A_Piece_Id : Piece.Type_Piece_Id;
      A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      Selected_Record : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected_Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Selected_Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;

      A_Path          : Gtk.Tree_Model.Gtk_Tree_Path;
      List_Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

      use Gtk.Tree_Selection;
      use Gtk.Tree_Model;
      use Hexagon.Client_Map;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Target_Patch.Set_Target_Patch_Window - enter");
      end if;

      if P_Patch = null then
         return;
      end if;

      Selected_Record := Gtk.Tree_View.Get_Selection (P_Window.all.Target_Pieces_Tree_View);

      Gtk.Tree_Selection.Get_Selected (Selected_Record, Selected_Model, Selected_Iter);

      if Selected_Model /= Gtk.Tree_Model.Null_Gtk_Tree_Model and
        Selected_Iter /= Gtk.Tree_Model.Null_Iter then
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
        Piece.Undefined_Piece_Id then

         Selected_Record := Gtk.Tree_View.Get_Selection (P_Window.all.Target_Pieces_Tree_View);

         if A_Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
            Gtk.Tree_Selection.Select_Path (Selected_Record, A_Path);
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Target_Patch.Set_Target_Patch_Window - exit");
      end if;
   end Set_Target_Patch_Window;

end Tubastga_Window_Pkg.Callbacks.Target_Patch;
