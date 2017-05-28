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

with Tubastga_Window_Pkg.FullsizeView;

package body Tubastga_Window_Pkg.ScrolledView is

   procedure Scroll_Map
     (P_Client_Map       : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Scroll_Direction : in     Tubastga_Window_Pkg.Callbacks.Main_Window.Type_Scroll_Direction)
   is
      A, B : Hexagon.Type_Hexagon_Numbers;

      use Tubastga_Window_Pkg.Callbacks.Main_Window;
      use Hexagon;
   begin
      A := P_Client_Map.Origo_Patch.Pos.A;
      B := P_Client_Map.Origo_Patch.Pos.B;
      --
      if P_Scroll_Direction = Tubastga_Window_Pkg.Callbacks.Main_Window.Up and P_Client_Map.Origo_Patch.Pos.B < 100 then
         B := P_Client_Map.Origo_Patch.Pos.B + 1;
      elsif P_Scroll_Direction = Tubastga_Window_Pkg.Callbacks.Main_Window.Down and P_Client_Map.Origo_Patch.Pos.B > 1 then
         B := P_Client_Map.Origo_Patch.Pos.B - 1;
      elsif P_Scroll_Direction = Tubastga_Window_Pkg.Callbacks.Main_Window.Right and P_Client_Map.Origo_Patch.Pos.A < 100 then
         A := P_Client_Map.Origo_Patch.Pos.A + 1;
      elsif P_Scroll_Direction = Tubastga_Window_Pkg.Callbacks.Main_Window.Left and P_Client_Map.Origo_Patch.Pos.A > 1 then
         A := P_Client_Map.Origo_Patch.Pos.A - 1;
      end if;

      Hexagon.Client_Map.Set_Origo_Patch (P_Client_Map, A, B);

   end Scroll_Map;

   function Selected_Patch
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Scrolledsizeview_X,
      P_Scrolledsizeview_Y : Glib.Gdouble)
      return Hexagon.Client_Map.Type_Client_Patch_Adress
   is
   begin
      return Tubastga_Window_Pkg.FullsizeView.Selected_Patch
          (P_Client_Map,
           P_Scrolledsizeview_X,
           P_Scrolledsizeview_Y);
   end Selected_Patch;

   procedure Scroll_To_Patch
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Zoomed_Pos      : in  Hexagon.Type_Hexagon_Position)
   is
      A_Min, B_Min, A_Max, B_Max, Origo_A, Origo_B, A, B : Integer;

   begin
      --
      A := Integer (P_Zoomed_Pos.A);
      B := Integer (P_Zoomed_Pos.B);

      A_Min := A - 3;

      B_Min := B - 3;
      A_Max := A + 3;
      B_Max := B + 3;

      Origo_A := Integer (P_Client_Map.Origo_Patch.all.Pos.A);
      Origo_B := Integer (P_Client_Map.Origo_Patch.all.Pos.B);

      if A_Min < Integer (P_Client_Map.Origo_Patch.all.Pos.A) then
         Origo_A := A_Min;
      end if;

      if B_Min < Integer (P_Client_Map.Origo_Patch.all.Pos.B) then
         Origo_B := B_Min;
      end if;

      if A_Max > Integer (P_Client_Map.Origo_Patch.all.Pos.A) + 20 then
         Origo_A := Integer (P_Client_Map.Origo_Patch.all.Pos.A) + A - 3;
      end if;

      if B_Max > Integer (P_Client_Map.Origo_Patch.all.Pos.B) + 15 then
         Origo_B := Integer (P_Client_Map.Origo_Patch.all.Pos.B) + B - 3;
      end if;

      if Origo_A < 1 then
         Origo_A := 1;
      end if;
      if Origo_B < 1 then
         Origo_B := 1;
      end if;

      Hexagon.Client_Map.Set_Origo_Patch
        (P_Client_Map,
         Hexagon.Type_Hexagon_Numbers (Origo_A),
         Hexagon.Type_Hexagon_Numbers (Origo_B));

   end Scroll_To_Patch;

end Tubastga_Window_Pkg.ScrolledView;
