--
--
--      Tubastga - a turn based strategy game - Editor
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

--with Hexagon.Server_Map;
with Landscape;
with Text_IO;
with Ada.Numerics.Discrete_Random;
with Hexagon.Client_Map;

package body Map_Builder is

   Verbose : constant Boolean := True;
   type Type_Direction is range 1 .. 6;

   package Random_Hexagon_Numbers is new Ada.Numerics.Discrete_Random
     (Hexagon.Type_Hexagon_Numbers);
   --package Random_Hexagon_Delta is new Ada.Numerics.Discrete_Random (Hexagon.Area.Type_Hexagon_Delta_Numbers);
   package Random_Direction is new Ada.Numerics.Discrete_Random
     (Type_Direction);
   Random_Numbers_Gen   : Random_Hexagon_Numbers.Generator;
   Random_Direction_Gen : Random_Direction.Generator;

   procedure Fill_Area
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_From_A, P_To_A, P_From_B, P_To_B : in Hexagon.Type_Hexagon_Numbers;
      P_Landscape                        : in Landscape.Type_Landscape)
   is
      This_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line("Map_Builder.Fill_Area - enter P_From_A=" & P_From_A'Img & " P_To_A=" & P_To_A'Img);
      end if;

      for A in P_From_A .. P_To_A loop
         for B in P_From_B .. P_To_B loop
            This_Patch :=
              Hexagon.Client_Map.Get_Patch_Adress_From_AB
                (P_Client_Map,
                 Hexagon.Type_Hexagon_Numbers (A),
                 Hexagon.Type_Hexagon_Numbers (B));

            This_Patch.all.Landscape_Here := P_Landscape;
         end loop;
      end loop;

      if Verbose then
         Text_IO.Put_Line("Map_Builder.Fill_Area - exit");
      end if;
   end Fill_Area;


end Map_Builder;
