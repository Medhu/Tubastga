--
--
--      Tubastga Game
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

package Tubastga_Game.Server_Logic.Move_Logic is
   procedure Move_Energy (P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
                          P_To_Patch : in Landscape.Type_Patch; P_Move_Status : out Boolean);

   procedure Rest_Energy (P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece);
end Tubastga_Game.Server_Logic.Move_Logic;
