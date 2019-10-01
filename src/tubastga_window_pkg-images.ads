--
--
--      Tubastga Game - A turn based strategy game
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

with Gdk.Pixbuf;
with Glib;
with Ada.Strings.Unbounded;

package Tubastga_Window_Pkg.Images is
   type Type_Frame is
      record
         Frame_Name                : Ada.Strings.Unbounded.Unbounded_String;
         Image_Data                : Gdk.Pixbuf.Gdk_Pixbuf;
         Image_Height, Image_Width : Glib.Gint;
         Dest_X, Dest_Y,
         Offset_X, Offset_Y        : Glib.Gint;
      end record;
   type Type_Frame_Access is access all Type_Frame;

   package Frames_List_Pkg is new Ada.Containers.Vectors
     (Positive, Type_Frame_Access);

   type Type_Action is
      record
         Action_Name : Ada.Strings.Unbounded.Unbounded_String;
         Frames : Frames_List_Pkg.Vector;
      end record;
   type Type_Action_Access is access all Type_Action;

   package Actions_List_Pkg is new Ada.Containers.Vectors
     (Positive, Type_Action_Access);

   type Type_Creature is
      record
         Creature_Name : Ada.Strings.Unbounded.Unbounded_String;
         Actions : Actions_List_Pkg.Vector;
      end record;
   type Type_Creature_Access is access all Type_Creature;

   package Creatures_List_Pkg is new Ada.Containers.Vectors
     (Positive, Type_Creature_Access);

   type Type_Race is
      record
         Folder    : Ada.Strings.Unbounded.Unbounded_String;
         Race_Name : Ada.Strings.Unbounded.Unbounded_String;
         Creatures : Creatures_List_Pkg.Vector;
      end record;
   type Type_Race_Access is access all Type_Race;

   package Races_List_Pkg is new Ada.Containers.Vectors
     (Positive, Type_Race_Access);

   procedure Initialize (P_Creatures : in out Creatures_List_Pkg.Vector);

   function Get_Image (P_Creatures_List : in out Creatures_List_Pkg.Vector;
                       P_Piece_Image : Tubastga_UI_Resources.Type_Image_Names)
                       return Type_Frame_Access;

   All_Creatures : Creatures_List_Pkg.Vector;

   All_Races : Races_List_Pkg.Vector;

   procedure Print_Frame_List (P_Frame_List : in Frames_List_Pkg.Vector);

   procedure Print_Action_List (P_Action_List : in Actions_List_Pkg.Vector);

   procedure Print_Creature_List (P_Creature_List : in Creatures_List_Pkg.Vector);

   procedure Print_Creature (P_Creature : in Type_Creature);

   procedure Print_Action (P_Action : in Type_Action);

   procedure Print_Frame (P_Frame : in Type_Frame);

end Tubastga_Window_Pkg.Images;
