--
--
--      Tubastga - Scenario Editor
--      Copyright (C) 2021  Frank J Jorgensen
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

package TubastgaEditor_UI_Aux is
   UI_Problem : exception;

   type Type_UI_State is
     (Scroll,
      Place_Landscape,
      Place_FillAllLandscape);


   type Type_Pencil_Width is (Width1, Width2, Width3);

   UI_State : Type_UI_State;
   UI_Paint_Landscape : Positive;

   UI_Pencil_Width : Type_Pencil_Width := Width1;
   UI_Selecting : Boolean := False;

end TubastgaEditor_UI_Aux;
