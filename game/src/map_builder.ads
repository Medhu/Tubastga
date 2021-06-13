--
--
--      Tubastga - a turn based strategy game
--      Copyright (C) 2013  Frank J Jorgensen
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
with Landscape;
with Hexagon;
with Hexagon.Client_Map;

package Map_Builder is
   Invalid_Area : exception;

   procedure Fill_Area (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
                        P_From_A, P_To_A, P_From_B, P_To_B : in Hexagon.Type_Hexagon_Numbers;
                        P_Landscape : in Landscape.Type_Landscape);



end Map_Builder;
