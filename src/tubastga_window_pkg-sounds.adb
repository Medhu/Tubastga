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

with mcc;
with mcc.Sounds;

package body Tubastga_Window_Pkg.Sounds is

   procedure Play_Placed_Piece is
   begin
      mcc.Sounds.Play_Continuous ("resources\click.wav");
   end Play_Placed_Piece;

   procedure Play_Set_Performing_Piece_Window is
   begin
      mcc.Sounds.Play_Continuous ("resources\click.wav");
   end Play_Set_Performing_Piece_Window;

   procedure Play_Set_Target_Piece_Window is
   begin
      mcc.Sounds.Play_Continuous ("resources\click.wav");
   end Play_Set_Target_Piece_Window;

end Tubastga_Window_Pkg.Sounds;
