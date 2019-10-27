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

with Text_IO;
with Glib.Error;
with Ada.Directories;

package body Tubastga_Window_Pkg.Images is

   Verbose : constant Boolean := True;

   procedure Load_Image (P_Creature : in out Type_Creature_Access) is
      Error : Glib.Error.GError;

      use Glib.Error;
   begin

      Text_IO.Put_Line("Load_Image:"
                      & Ada.Strings.Unbounded.To_String (P_Creature.all.Frame.all.Frame_Name));
      Gdk.Pixbuf.Gdk_New_From_File
        (P_Creature.all.Frame.all.Image_Data,
         Ada.Strings.Unbounded.To_String (P_Creature.all.Frame.all.Frame_Name), Error);

      if Error = null then
         P_Creature.all.Frame.all.Image_Width :=
           Gdk.Pixbuf.Get_Width (P_Creature.all.Frame.all.Image_Data);
         P_Creature.all.Frame.all.Image_Height :=
           Gdk.Pixbuf.Get_Height (P_Creature.all.Frame.all.Image_Data);
         --
         Text_IO.Put_Line
           (" Width:" & P_Creature.all.Frame.all.Image_Width'Img & " Height:" &
            P_Creature.all.Frame.all.Image_Height'Img);
      else
         Text_IO.Put_Line ("Error: " & Glib.Error.Get_Message (Error));
         Glib.Error.Error_Free (Error);
         P_Creature.all.Frame.all.Image_Width  := 72;
         P_Creature.all.Frame.all.Image_Height := 72;

         P_Creature.all.Frame.all.Image_Data :=
           Gdk.Pixbuf.Gdk_New
             (Bits_Per_Sample => 24, Width => P_Creature.all.Frame.all.Image_Width,
              Height          => P_Creature.all.Frame.all.Image_Height);

         Gdk.Pixbuf.Fill (P_Creature.all.Frame.all.Image_Data, 16#0000FF00#);
      end if;

   end Load_Image;

   procedure Initialize (P_Races : in out Races_List_Pkg.Vector) is
      A_Creature     : Type_Creature_Access;
      A_Race     : Type_Race_Access;
      A_Frame : Type_Frame_Access;

      use Glib;
   begin

      declare
         A_Search              : Ada.Directories.Search_Type;
         Races_Directory_Entry : Ada.Directories.Directory_Entry_Type;
      begin
         Text_IO.Put_Line ("Directory:");

         Ada.Directories.Start_Search
           (A_Search, "D:\Ada\Git\Tubastga\tubastga_client\resources\ny", "*");
         while Ada.Directories.More_Entries (A_Search) loop
            Ada.Directories.Get_Next_Entry (A_Search, Races_Directory_Entry);

            if Ada.Directories.Simple_Name (Races_Directory_Entry) /= "." and
              Ada.Directories.Simple_Name (Races_Directory_Entry) /= ".." then

               A_Race :=
                 new Type_Race'
                   (Ada.Strings.Unbounded.To_Unbounded_String
                      (Ada.Directories.Simple_Name (Races_Directory_Entry)),
                    Creatures_List_Pkg.Empty_Vector);

               declare
                  A_Search                  : Ada.Directories.Search_Type;
                  Creatures_Directory_Entry : Ada.Directories.Directory_Entry_Type;
               begin

                  Ada.Directories.Start_Search
                    (A_Search, --"D:\Ada\Git\Tubastga\tubastga_client\resources\"
                     Ada.Directories.Full_Name (Races_Directory_Entry), "*");
                  while Ada.Directories.More_Entries (A_Search) loop
                     Ada.Directories.Get_Next_Entry (A_Search, Creatures_Directory_Entry);

                     if Ada.Directories.Simple_Name (Creatures_Directory_Entry) /= "." and
                       Ada.Directories.Simple_Name (Creatures_Directory_Entry) /= ".." then

                        A_Frame :=
                          new Type_Frame'
                            (Ada.Strings.Unbounded.To_Unbounded_String
                               (Ada.Directories.Full_Name (Creatures_Directory_Entry)),
                             null, 0, 0, 0, 0, 0, 0);

                        A_Creature :=
                          new Type_Creature'
                             (Ada.Strings.Unbounded.To_Unbounded_String
                                (Ada.Directories.Simple_Name (Creatures_Directory_Entry)),
                              A_Frame);

                        Load_Image (A_Creature);

                        Creatures_List_Pkg.Append
                          (A_Race.all.Creatures, A_Creature);

                     end if;

                  end loop;
               end;

               Races_List_Pkg.Append (All_Races, A_Race);
            end if;

         end loop;

      end;

   end Initialize;

   function Get_Image (P_Races_List : in out Races_List_Pkg.Vector;
                       P_Image_Key : in Type_Image_Key)
                       return Type_Frame_Access
   is
      A_Race     : Type_Race_Access;
      A_Creature : Type_Creature_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Get_Image - enter");
      end if;

      A_Race := Races_List_Pkg.Element (P_Races_List, 3);
      A_Creature := Creatures_List_Pkg.Element (A_Race.all.Creatures, 2);

      Tubastga_Window_Pkg.Images.Print_Creature (A_Creature.all);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Get_Image - exit");
      end if;

      return A_Creature.all.Frame;
   end Get_Image;

   procedure Print_Creature (P_Creature : in Type_Creature) is
   begin
      Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (P_Creature.Creature_Name));
   end Print_Creature;

   procedure Print_Frame (P_Frame : in Type_Frame) is
   begin
      Text_IO.Put_Line ("    " & Ada.Strings.Unbounded.To_String (P_Frame.Frame_Name));
   end Print_Frame;

   procedure Print_Creature_List (P_Creature_List : in Creatures_List_Pkg.Vector) is
      Trav_Creatures : Creatures_List_Pkg.Cursor;

      A_Creature : Type_Creature_Access;
   begin
      Trav_Creatures := Creatures_List_Pkg.First (P_Creature_List);
      while Creatures_List_Pkg.Has_Element (Trav_Creatures) loop
         A_Creature := Creatures_List_Pkg.Element (Trav_Creatures);
         Text_IO.Put ("Creature: " & Creatures_List_Pkg.To_Index (Trav_Creatures)'Img & " ");
         Print_Creature (A_Creature.all);

         Trav_Creatures := Creatures_List_Pkg.Next (Trav_Creatures);
      end loop;

   end Print_Creature_List;

   procedure Print_Races_List (P_Race_List : in Races_List_Pkg.Vector) is
      Trav_Races : Races_List_Pkg.Cursor;

      A_Race : Type_Race_Access;
   begin
      Trav_Races := Races_List_Pkg.First (P_Race_List);
      while Races_List_Pkg.Has_Element (Trav_Races) loop

         A_Race := Races_List_Pkg.Element (Trav_Races);
         Text_IO.Put_Line ("Race:" & Ada.Strings.Unbounded.To_String (A_Race.all.Race_Name));
         Print_Creature_List (A_Race.all.Creatures);

         Trav_Races := Races_List_Pkg.Next (Trav_Races);
      end loop;

   end Print_Races_List;

begin
   Initialize (All_Races);

   Print_Races_List (All_Races);
end Tubastga_Window_Pkg.Images;
