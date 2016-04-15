--
--
--      Tubastga Game - A turn based strategy game
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

with Effect;
with Gtk.Text_Iter;
with Tubastga_Piece;
with Observation;
with Piece.Client_Piece;
with Goods;
with Utilities;
with Tubastga_Piece.Carrier;
with Text_IO;

package body Tubastga_Window_Pkg.Effects is
   Verbose : constant Boolean := False;

   function Format_Effect (P_Effect : in Effect.Type_Effect) return String is
   begin
      return Utilities.RemoteString.To_String
          (Tubastga_Piece.Effect_Type_Info_List (P_Effect.Effect_Name).Type_Name);

   end Format_Effect;

   procedure Update_Client_Piece is
      Trav_Pieces  : Piece.Client_Piece.Pieces_Client_List.Cursor;
      Trav_Effects : Effect.Effect_List.Cursor;
      An_Effect    : Effect.Type_Effect;
      A_Piece      : Tubastga_Window_Pkg.Type_Client_Access_Class;

      use Effect;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_window_Pkg.effects.Update_Client_Piece - enter");
      end if;

      Trav_Pieces :=
        Piece.Client_Piece.Pieces_Client_List.First (Piece.Client_Piece.Client_Pieces_In_Game);
      while Piece.Client_Piece.Pieces_Client_List.Has_Element (Trav_Pieces) loop

         A_Piece :=
           Tubastga_Window_Pkg.Type_Client_Access_Class
             (Piece.Client_Piece.Pieces_Client_List.Element (Trav_Pieces));

         A_Piece.all.Action_Point  := 0;
         A_Piece.all.Storage.Slots := (others => Goods.Type_Goods_Info'(Goods.None, 0));
         A_Piece.all.Captain       := False;

         Trav_Effects := Effect.Effect_List.First (A_Piece.all.Effects_On_Piece);
         while Effect.Effect_List.Has_Element (Trav_Effects) loop
            An_Effect := Effect.Effect_List.Element (Trav_Effects);

            if An_Effect.Effect_Name = Tubastga_Piece.Effect_Action_Point then
               A_Piece.all.Action_Point := An_Effect.Aux;

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Slot_1 then
               A_Piece.all.Storage.Slots (1) := Goods.Aux_To_Goods_Info (An_Effect.Aux);

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Slot_2 then
               A_Piece.all.Storage.Slots (2) := Goods.Aux_To_Goods_Info (An_Effect.Aux);

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Slot_3 then
               A_Piece.all.Storage.Slots (3) := Goods.Aux_To_Goods_Info (An_Effect.Aux);

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Stops then

               A_Piece.all.Stops (1) :=
                 Piece.Type_Piece_Id (Tubastga_Piece.Carrier.Get_Tower_Id (1, An_Effect.Aux));
               A_Piece.all.Stops (2) :=
                 Piece.Type_Piece_Id (Tubastga_Piece.Carrier.Get_Tower_Id (2, An_Effect.Aux));
               A_Piece.all.Stops (3) :=
                 Piece.Type_Piece_Id (Tubastga_Piece.Carrier.Get_Tower_Id (3, An_Effect.Aux));

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Load then
               A_Piece.all.Load (1) := Tubastga_Piece.Carrier.Get_Tower_Goods_Id (1, An_Effect.Aux);
               A_Piece.all.Load (2) := Tubastga_Piece.Carrier.Get_Tower_Goods_Id (2, An_Effect.Aux);
               A_Piece.all.Load (3) := Tubastga_Piece.Carrier.Get_Tower_Goods_Id (3, An_Effect.Aux);

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Unload then
               A_Piece.all.Unload (1) :=
                 Tubastga_Piece.Carrier.Get_Tower_Goods_Id (1, An_Effect.Aux);
               A_Piece.all.Unload (2) :=
                 Tubastga_Piece.Carrier.Get_Tower_Goods_Id (2, An_Effect.Aux);
               A_Piece.all.Unload (3) :=
                 Tubastga_Piece.Carrier.Get_Tower_Goods_Id (3, An_Effect.Aux);

            elsif An_Effect.Effect_Name = Tubastga_Piece.Effect_Captain then
               A_Piece.all.Captain := True;

            end if;

            Trav_Effects := Effect.Effect_List.Next (Trav_Effects);
         end loop;

         Trav_Pieces := Piece.Client_Piece.Pieces_Client_List.Next (Trav_Pieces);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_window_Pkg.Effects.Update_Client_Piece - exit");
      end if;

   end Update_Client_Piece;

   procedure Format_Piece_Effects
     (P_Buffer : in out Gtk_Text_Buffer;
      P_Piece  : in     Tubastga_Window_Pkg.Type_Client_Piece)
   is

      Msg_End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;

      use Piece;
   begin
      Get_End_Iter (P_Buffer, Msg_End_Iter);

      Gtk.Text_Buffer.Insert
        (P_Buffer,
         Msg_End_Iter,
         "Action Points " & P_Piece.Action_Point'Img & ASCII.LF);

      if P_Piece.Captain then
         Gtk.Text_Buffer.Insert (P_Buffer, Msg_End_Iter, "Captain" & ASCII.LF);
      end if;

      for Trav_Slot in P_Piece.Storage.Slots'First .. P_Piece.Storage.Slots'Last loop
         Get_End_Iter (P_Buffer, Msg_End_Iter);

         Gtk.Text_Buffer.Insert
           (P_Buffer,
            Msg_End_Iter,
            "Storing " &
            P_Piece.Storage.Slots (Trav_Slot).Quantity'Img &
            " of " &
            P_Piece.Storage.Slots (Trav_Slot).The_Goods'Img &
            ASCII.LF);
      end loop;

      for Trav_Stops in P_Piece.Stops'First .. P_Piece.Stops'Last loop
         Get_End_Iter (P_Buffer, Msg_End_Iter);

         Gtk.Text_Buffer.Insert
           (P_Buffer,
            Msg_End_Iter,
            "Stopping at " &
            P_Piece.Stops (Trav_Stops)'Img &
            " to load " &
            P_Piece.Load (Trav_Stops)'Img &
            " to unload " &
            P_Piece.Unload (Trav_Stops)'Img &
            ASCII.LF);

      end loop;

   end Format_Piece_Effects;

   procedure Format_Patch_Effects
     (P_Buffer : in out Gtk_Text_Buffer;
      P_Patch  :        Hexagon.Client_Map.Type_Client_Patch)
   is
      Trav_Patch_Effects : Effect.Effect_List.Cursor;
      Msg_End_Iter       : Gtk.Text_Iter.Gtk_Text_Iter;

      use Effect;
   begin
      Trav_Patch_Effects := Effect.Effect_List.First (P_Patch.Effects_Here);
      while Effect.Effect_List.Has_Element (Trav_Patch_Effects) loop

         Gtk.Text_Buffer.Get_End_Iter (P_Buffer, Msg_End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Buffer,
            Msg_End_Iter,
            Format_Effect (Effect.Effect_List.Element (Trav_Patch_Effects)) &
            " " &
            Effect.Effect_List.Element (Trav_Patch_Effects).Aux'Img &
            ASCII.LF);

         Trav_Patch_Effects := Effect.Effect_List.Next (Trav_Patch_Effects);
      end loop;

   end Format_Patch_Effects;
end Tubastga_Window_Pkg.Effects;
