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

   procedure Load_Images (P_Creatures_List : in out Creatures_List_Pkg.Vector) is
      Error          : Glib.Error.GError;
      Trav_Creatures : Creatures_List_Pkg.Cursor;
      A_Creature     : Type_Creature_Access;
      Trav_Action    : Actions_List_Pkg.Cursor;
      An_Action      : Type_Action_Access;
      Trav_Frames    : Frames_List_Pkg.Cursor;
      A_Frame        : Type_Frame_Access;

      use Glib.Error;
   begin
      Trav_Creatures := Creatures_List_Pkg.First (P_Creatures_List);
      while Creatures_List_Pkg.Has_Element (Trav_Creatures) loop

         A_Creature := Creatures_List_Pkg.Element (Trav_Creatures);
         Text_IO.Put_Line
           ("Loading Creature: " & Ada.Strings.Unbounded.To_String (A_Creature.all.Creature_Name));
         Trav_Action := Actions_List_Pkg.First (A_Creature.all.Actions);
         while Actions_List_Pkg.Has_Element (Trav_Action) loop

            An_Action := Actions_List_Pkg.Element (Trav_Action);
            Text_IO.Put_Line
              (" Action: " & Ada.Strings.Unbounded.To_String (An_Action.all.Action_Name));
            Trav_Frames := Frames_List_Pkg.First (An_Action.all.Frames);
            while Frames_List_Pkg.Has_Element (Trav_Frames) loop

               A_Frame := Frames_List_Pkg.Element (Trav_Frames);
               Text_IO.Put ("  Frame: " & Ada.Strings.Unbounded.To_String (A_Frame.all.Frame_Name));

               Gdk.Pixbuf.Gdk_New_From_File
                 (A_Frame.all.Image_Data, Ada.Strings.Unbounded.To_String (A_Frame.all.Frame_Name),
                  Error);

               if Error = null then
                  A_Frame.all.Image_Width  := Gdk.Pixbuf.Get_Width (A_Frame.all.Image_Data);
                  A_Frame.all.Image_Height := Gdk.Pixbuf.Get_Height (A_Frame.all.Image_Data);
                  --
                  Text_IO.Put_Line
                    (" Width:" & A_Frame.all.Image_Width'Img & " Height:" &
                     A_Frame.all.Image_Height'Img);
               else
                  Text_IO.Put_Line ("Error: " & Glib.Error.Get_Message (Error));
                  Glib.Error.Error_Free (Error);
                  A_Frame.all.Image_Width  := 72;
                  A_Frame.all.Image_Height := 72;

                  A_Frame.all.Image_Data :=
                    Gdk.Pixbuf.Gdk_New
                      (Bits_Per_Sample => 24, Width => A_Frame.all.Image_Width,
                       Height          => A_Frame.all.Image_Height);

                  Gdk.Pixbuf.Fill (A_Frame.all.Image_Data, 16#0000FF00#);
               end if;

               Trav_Frames := Frames_List_Pkg.Next (Trav_Frames);
            end loop;

            Trav_Action := Actions_List_Pkg.Next (Trav_Action);
         end loop;

         Trav_Creatures := Creatures_List_Pkg.Next (Trav_Creatures);
      end loop;
   end Load_Images;

   procedure Creatures (P_Creatures_List : in out Creatures_List_Pkg.Vector) is
   begin
      null;
   end Creatures;

   procedure Race (P_Races : in out Type_Race_Access) is
      A_Search          : Ada.Directories.Search_Type;
      A_Directory_Entry : Ada.Directories.Directory_Entry_Type;

   begin
      Ada.Directories.Start_Search
        (A_Search, Ada.Strings.Unbounded.To_String(P_Races.all.Folder), "*");
      while Ada.Directories.More_Entries (A_Search) loop
         Ada.Directories.Get_Next_Entry (A_Search, A_Directory_Entry);

         Text_IO.Put_Line
           ("Races:" & Ada.Directories.Full_Name (A_Directory_Entry) & "=" &
            Ada.Directories.Simple_Name (A_Directory_Entry));

      end loop;
   end Race;

   procedure Traverse (P_Races_List : in out Races_List_Pkg.Vector) is
      A_Search          : Ada.Directories.Search_Type;
      A_Directory_Entry : Ada.Directories.Directory_Entry_Type;

      A_Race : Type_Race_Access;
   begin
      Text_IO.Put_Line ("Directory:");
--      Ada.Directories.Set_Directory ("D:\Ada\Git\Tubastga\tubastga_client\resources\ny");
      Ada.Directories.Start_Search
        (A_Search, "D:\Ada\Git\Tubastga\tubastga_client\resources\ny", "*");
      while Ada.Directories.More_Entries (A_Search) loop
         Ada.Directories.Get_Next_Entry (A_Search, A_Directory_Entry);

         Text_IO.Put_Line
           ("Folder:" & Ada.Directories.Full_Name (A_Directory_Entry) & "=" &
            Ada.Directories.Simple_Name (A_Directory_Entry));

         A_Race :=
           new Type_Race'
             (Ada.Strings.Unbounded.To_Unbounded_String
                (Ada.Directories.Full_Name (A_Directory_Entry)),
              Ada.Strings.Unbounded.To_Unbounded_String
                (Ada.Directories.Simple_Name (A_Directory_Entry)),
              Creatures_List_Pkg.Empty_Vector);

         Races_List_Pkg.Append (P_Races_List, A_Race);
         Race (A_Race);

      end loop;
   end Traverse;

   procedure Initialize (P_Creatures : in out Creatures_List_Pkg.Vector) is
      A_Creature     : Type_Creature_Access;
      Creatures_List : Creatures_List_Pkg.Vector;
      An_Action      : Type_Action_Access;
      Actions_List   : Actions_List_Pkg.Vector;
      A_Frame        : Type_Frame_Access;
      Frames_List    : Frames_List_Pkg.Vector;

      use Glib;
   begin
      --
      --
      Traverse(All_Races);
--      declare
--         A_Search          : Ada.Directories.Search_Type;
--         A_Directory_Entry : Ada.Directories.Directory_Entry_Type;
--      begin
--         Text_IO.Put_Line ("Directory:");
--         Ada.Directories.Set_Directory ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units");
--         Ada.Directories.Start_Search
--           (A_Search, "D:\Ada\wesnoth\wesnoth-master\data\core\images\units", "*");
--         while Ada.Directories.More_Entries (A_Search) loop
--            Ada.Directories.Get_Next_Entry (A_Search, A_Directory_Entry);

--            Text_IO.Put_Line
--              ("=" & Ada.Directories.Full_Name (A_Directory_Entry) & "=" &
--               Ada.Directories.Simple_Name (A_Directory_Entry));
--         end loop;

--      end;
      --
      --

      -- Units in wesnoth.
      -- https://units.wesnoth.org/1.14/mainline/en_US/mainline.html
      -- ...\wesnoth-master\data\core\images\units
      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter.png"),
           null, 0, 0, 0, 0, 0, 0);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      An_Action :=
        new Type_Action'(Ada.Strings.Unbounded.To_Unbounded_String ("Passive"), Frames_List);

      Actions_List_Pkg.Append (Actions_List, An_Action);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-s-4.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-s-5.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-s-6.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      An_Action :=
        new Type_Action'(Ada.Strings.Unbounded.To_Unbounded_String ("Blade-s"), Frames_List);

      Actions_List_Pkg.Append (Actions_List, An_Action);
      --
      Frames_List_Pkg.Clear (Frames_List);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-se-1.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-se-2.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-se-3.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-se-4.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-se-5.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\drakes\arbiter-blade-se-6.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      An_Action :=
        new Type_Action'(Ada.Strings.Unbounded.To_Unbounded_String ("Blade-se"), Frames_List);

      Actions_List_Pkg.Append (Actions_List, An_Action);
      --

      A_Creature :=
        new Type_Creature'(Ada.Strings.Unbounded.To_Unbounded_String ("drakes"), Actions_List);

      Creatures_List_Pkg.Append (P_Creatures, A_Creature);
      --
      --
      Actions_List_Pkg.Clear (Actions_List);
      Frames_List_Pkg.Clear (Frames_List);

      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\goblins\direwolver.png"),
           null, 0, 0, 0, 0, 0, 0);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      An_Action :=
        new Type_Action'(Ada.Strings.Unbounded.To_Unbounded_String ("Passive"), Frames_List);

      Actions_List_Pkg.Append (Actions_List, An_Action);

      Frames_List_Pkg.Clear (Frames_List);
      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\goblins\direwolver-attack.png"),
           null, 0, 0, 0, -50, 0, -50);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      An_Action :=
        new Type_Action'
          (Ada.Strings.Unbounded.To_Unbounded_String ("Direwolver-attack"), Frames_List);

      Actions_List_Pkg.Append (Actions_List, An_Action);

      --

      A_Creature :=
        new Type_Creature'(Ada.Strings.Unbounded.To_Unbounded_String ("goblins"), Actions_List);

      Creatures_List_Pkg.Append (P_Creatures, A_Creature);

      --
      Actions_List_Pkg.Clear (Actions_List);
      Frames_List_Pkg.Clear (Frames_List);
      A_Frame :=
        new Type_Frame'
          (Ada.Strings.Unbounded.To_Unbounded_String
             ("D:\Ada\wesnoth\wesnoth-master\data\core\images\units\human-loyalists\bowman.png"),
           null, 0, 0, 0, 0, 0, 0);

      Frames_List_Pkg.Append (Frames_List, A_Frame);

      An_Action :=
        new Type_Action'(Ada.Strings.Unbounded.To_Unbounded_String ("Passive"), Frames_List);

      Actions_List_Pkg.Append (Actions_List, An_Action);

      A_Creature :=
        new Type_Creature'
          (Ada.Strings.Unbounded.To_Unbounded_String ("human-loyalist"), Actions_List);

      Creatures_List_Pkg.Append (P_Creatures, A_Creature);

      Load_Images (P_Creatures);
   end Initialize;

   function Get_Image (P_Creatures_List : in out Creatures_List_Pkg.Vector;
      P_Piece_Image : in     Tubastga_UI_Resources.Type_Image_Names) return Type_Frame_Access
   is
      A_Creature : Type_Creature_Access;
      An_Action  : Type_Action_Access;
      A_Frame    : Type_Frame_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Get_Image - enter");
      end if;

      A_Creature := Creatures_List_Pkg.Element (P_Creatures_List, 3);
      An_Action  := Actions_List_Pkg.Element (A_Creature.Actions, 1);
      A_Frame    := Frames_List_Pkg.Element (An_Action.Frames, 1);

      Tubastga_Window_Pkg.Images.Print_Creature (A_Creature.all);
      Tubastga_Window_Pkg.Images.Print_Action (An_Action.all);
      Tubastga_Window_Pkg.Images.Print_Frame (A_Frame.all);
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Get_Image - exit");
      end if;

      return A_Frame;
   end Get_Image;

   procedure Print_Creature (P_Creature : in Type_Creature) is
   begin
      Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (P_Creature.Creature_Name));
      Print_Action_List (P_Creature.Actions);
   end Print_Creature;

   procedure Print_Action (P_Action : in Type_Action) is
   begin
      Text_IO.Put_Line ("  " & Ada.Strings.Unbounded.To_String (P_Action.Action_Name));
      Print_Frame_List (P_Action.Frames);
   end Print_Action;

   procedure Print_Frame (P_Frame : in Type_Frame) is
   begin
      Text_IO.Put_Line ("    " & Ada.Strings.Unbounded.To_String (P_Frame.Frame_Name));
   end Print_Frame;

   procedure Print_Frame_List (P_Frame_List : in Frames_List_Pkg.Vector) is
      Trav_Frames : Frames_List_Pkg.Cursor;

      A_Frame : Type_Frame_Access;
   begin
      Trav_Frames := Frames_List_Pkg.First (P_Frame_List);
      while Frames_List_Pkg.Has_Element (Trav_Frames) loop
         A_Frame := Frames_List_Pkg.Element (Trav_Frames);
         Text_IO.Put ("Frames: " & Frames_List_Pkg.To_Index (Trav_Frames)'Img & " ");
         Print_Frame (A_Frame.all);

         Trav_Frames := Frames_List_Pkg.Next (Trav_Frames);
      end loop;

   end Print_Frame_List;

   procedure Print_Action_List (P_Action_List : in Actions_List_Pkg.Vector) is
      Trav_Actions : Actions_List_Pkg.Cursor;

      An_Action : Type_Action_Access;
   begin
      Trav_Actions := Actions_List_Pkg.First (P_Action_List);
      while Actions_List_Pkg.Has_Element (Trav_Actions) loop
         An_Action := Actions_List_Pkg.Element (Trav_Actions);
         Text_IO.Put ("Action: " & Actions_List_Pkg.To_Index (Trav_Actions)'Img & " ");
         Print_Action (An_Action.all);

         Trav_Actions := Actions_List_Pkg.Next (Trav_Actions);
      end loop;

   end Print_Action_List;

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

begin
   Initialize (All_Creatures);

   Print_Creature_List (All_Creatures);
end Tubastga_Window_Pkg.Images;
