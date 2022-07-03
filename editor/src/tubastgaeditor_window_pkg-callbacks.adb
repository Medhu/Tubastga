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

with Glib;       use Glib;
with Gdk.Event;  use Gdk.Event;
with Gdk.Types;  use Gdk.Types;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.File_Filter;
with Gtk.Widget; use Gtk.Widget;
with Cairo;
with Gdk.Cairo;
with Gtk.Main;
with Gtk.File_Chooser_Dialog;
with Gtk.Check_Button;
--
with Text_IO;
with Gdk.Pixbuf;
with Glib.Error;
use type Glib.Error.GError;
with Gdk.Pixbuf;
with Gdk.Window; use Gdk.Window;

with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Ada.Strings.Unbounded;
with TubastgaEditor_UI_Aux;
with Landscape;
with Hexagon;
with Hexagon.Area;
with Hexagon.Client_Map;
with Hexagon.Server_Map;
with Hexagon.Server_Navigation;
with Hexagon.Server_Navigation.Modify;
with Map_Builder;
with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO;
with Gtk.Text_Iter;
with Gtk.Tree_Model;
with Glib.Values;
with Tubastga_Window_Pkg.Images;
with Tubastga_Window_Pkg.ScrolledView;
with Tubastga_Window_Pkg.FullsizeView;
with Tubastga_Window_Pkg.ZoomedView;
with Tubastga_Game;
with Tubastga_Game.Server_Logic;
with Action;
with Status;
with Landscape.Server;
with Gtk.Toggle_Button;
with Ada.Directories;

package body TubastgaEditor_Window_Pkg.Callbacks is
   Verbose : constant Boolean := False;

   Left_Mouse_Button  : constant Glib.Guint := 1;
   Right_Mouse_Button : constant Glib.Guint := 3;

   type Type_Patches_List is array (1 .. 19) of Hexagon.Type_Hexagon_Position;

   use Gtk.Arguments;

   All_Pix, Scale_Pix : Gdk.Pixbuf.Gdk_Pixbuf;

   All_Constructions_On_Patch, All_Effects_On_Patch,
   All_Landscape_On_Patch : Gdk.Pixbuf.Gdk_Pixbuf;

   A_Land_Navigation : Hexagon.Server_Navigation.Type_Navigation;
   A_Sea_Navigation  : Hexagon.Server_Navigation.Type_Navigation;

   A_Client_Map           : Hexagon.Client_Map.Type_Client_Map_Info;
   Server_Map_Origo_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

   Game_Area_Origo_X : constant Integer   := 50;
   Game_Area_Origo_Y : constant Integer   := 1050;
   Png_Width         : constant Glib.Gint := 72;
   Png_Height        : constant Glib.Gint := 72;

   Scenario_Map                  : Ada.Strings.Unbounded.Unbounded_String;
   Scenario_Land_Navigation_Path : Ada.Strings.Unbounded.Unbounded_String;
   Scenario_Sea_Navigation_Path  : Ada.Strings.Unbounded.Unbounded_String;

   type Type_Navigation_Info is record
      Navigation_Delta_Position : Hexagon.Area.Type_Hexagon_Delta_Position;
      From_Pos, To_Pos          : Hexagon.Type_Hexagon_Position;
   end record;

   Navigation_TAB : Type_Navigation_Info;

   Map_Scale                  : Float := 0.50;
   Left_Button_Pressed_Patch  : Hexagon.Client_Map.Type_Client_Patch_Adress;
   Right_Button_Pressed_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   A_Path                     : Hexagon.Server_Navigation.Path_Pkg.Vector;

   package Neighbour_List_Pkg is new Ada.Containers.Vectors (Positive,
      Hexagon.Area.Type_Hexagon_Delta_Position, Hexagon.Area."=");
   Template_Neighbours : Neighbour_List_Pkg.Vector;

   The_Window : Window1_Access;

   procedure Draw_Map
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in out Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
   begin
      if Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB
          (A_Client_Map, P_Patch.all) in
          0 .. 2460 and
        Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB
          (A_Client_Map, P_Patch.all) in
          0 .. 1050
      then

--         tubastgaeditor_window 1>res.txt 2>&1

         Gdk.Pixbuf.Fill (All_Landscape_On_Patch, Glib.Guint32 (0));
         Tubastga_Window_Pkg.FullsizeView.Draw_Landscapes
           (All_Landscape_On_Patch, P_Patch.all.Landscape_Here);
         Tubastga_Window_Pkg.FullsizeView.Draw_All_Patch
           (A_Client_Map, P_Patch.all, All_Pix, All_Constructions_On_Patch,
            All_Effects_On_Patch, All_Landscape_On_Patch);
      end if;

   end Draw_Map;

   function Get_Paint_Patches
     (P_Center_Patch : in Hexagon.Server_Map.Type_Server_Patch;
      P_Width        : in TubastgaEditor_UI_Aux.Type_Pencil_Width)
      return Type_Patches_List
   is
      Ret : Type_Patches_List :=
        (others => Hexagon.Type_Hexagon_Position'(P_Valid => False));

      use TubastgaEditor_UI_Aux;
      use Hexagon;
   begin

      if P_Width = TubastgaEditor_UI_Aux.Width1 then
         -- group I
         Ret (1) := P_Center_Patch.Pos;

      elsif P_Width = TubastgaEditor_UI_Aux.Width2 then
         -- group I
         Ret (1) := P_Center_Patch.Pos;
         Ret (2) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B + 1);
         Ret (3) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 1, P_Center_Patch.Pos.B);
         Ret (4) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 1, P_Center_Patch.Pos.B - 1);
         Ret (5) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B - 1);
         Ret (6) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 1, P_Center_Patch.Pos.B);
         Ret (7) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 1, P_Center_Patch.Pos.B + 1);

      elsif P_Width = TubastgaEditor_UI_Aux.Width3 then
         Ret (1) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B);
         Ret (2) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B + 1);
         Ret (3) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 1, P_Center_Patch.Pos.B);
         Ret (4) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 1, P_Center_Patch.Pos.B - 1);
         Ret (5) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B - 1);
         Ret (6) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 1, P_Center_Patch.Pos.B);
         Ret (7) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 1, P_Center_Patch.Pos.B + 1);

         Ret (8) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B + 2);
         Ret (9) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 1, P_Center_Patch.Pos.B + 1);
         Ret (10) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 2, P_Center_Patch.Pos.B);
         Ret (11) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 2, P_Center_Patch.Pos.B - 1);
         Ret (12) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 2, P_Center_Patch.Pos.B - 2);
         Ret (13) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A + 1, P_Center_Patch.Pos.B - 2);
         Ret (14) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A, P_Center_Patch.Pos.B - 2);
         Ret (15) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 1, P_Center_Patch.Pos.B - 1);
         Ret (16) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 2, P_Center_Patch.Pos.B);
         Ret (17) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 2, P_Center_Patch.Pos.B + 1);
         Ret (18) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 2, P_Center_Patch.Pos.B + 2);
         Ret (19) :=
           Hexagon.Type_Hexagon_Position'
             (True, P_Center_Patch.Pos.A - 1, P_Center_Patch.Pos.B + 2);

      end if;

      return Ret;
   end Get_Paint_Patches;

   procedure Validate_Paths
     (P_Scenario_Path : in Ada.Strings.Unbounded.Unbounded_String;
      P_Scenario_Land_Navigation_Path : in Ada.Strings.Unbounded
        .Unbounded_String;
      P_Scenario_Sea_Navigation_Path : in Ada.Strings.Unbounded
        .Unbounded_String;
      P_Ret : out Boolean)
   is
   begin
      Text_IO.Put_Line
        ("Scenario_Path:" & Ada.Strings.Unbounded.To_String (P_Scenario_Path));

      Text_IO.Put_Line
        ("Scenario_Land_Navigation:" &
         Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_Path));
      Text_IO.Put_Line
        ("Scenario_Sea_Navigation:" &
         Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_Path));

      P_Ret := True;
      if Ada.Directories.Exists
          (Ada.Strings.Unbounded.To_String (P_Scenario_Path)) then
         Gtk.Label.Set_Label
           (The_Window.all.lblMapPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Path) & " Ok");

      else
         Gtk.Label.Set_Label
           (The_Window.all.lblMapPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Path) & " Not Found");
         P_Ret := False;
      end if;
      if Ada.Directories.Exists
          (Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_Path))
      then
         Gtk.Label.Set_Label
           (The_Window.all.lblLandNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_Path) &
            " Ok");
      else
         Gtk.Label.Set_Label
           (The_Window.all.lblLandNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_Path) &
            " Not Found");
         P_Ret := False;
      end if;
      if Ada.Directories.Exists
          (Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_Path))
      then
         Gtk.Label.Set_Label
           (The_Window.all.lblSeaNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_Path) &
            " Ok");
      else
         Gtk.Label.Set_Label
           (The_Window.all.lblSeaNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_Path) &
            " Not Found");
         P_Ret := False;
      end if;

   end Validate_Paths;

   ----------------------------------------
   -- On_Map_Area_Button_Press_Event --
   ----------------------------------------

   procedure On_Button_Save (Object : access Gtk_Button_Record'Class) is
      Response           : Gtk.Dialog.Gtk_Response_Type;
      Scenario_Full_Path : Ada.Strings.Unbounded.Unbounded_String;
      Scenario_Name      : Ada.Strings.Unbounded.Unbounded_String;
      Scenario_Path      : Ada.Strings.Unbounded.Unbounded_String;

      Ret : Boolean;
      use Gtk.Dialog;
      use Ada.Strings.Unbounded;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Save - clicked");
      end if;

      Response :=
        Gtk.Dialog.Run (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgFileSave));
      Gtk.Dialog.Hide (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgFileSave));

      Scenario_Full_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Gtk.File_Chooser_Dialog.Get_Filename (The_Window.all.dlgFileSave));

      Text_IO.Put_Line
        ("Save Scenario_Full_Path:" &
         Ada.Strings.Unbounded.To_String (Scenario_Full_Path));

      Scenario_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Directories.Containing_Directory
             (Ada.Strings.Unbounded.To_String (Scenario_Full_Path)));

      Scenario_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Directories.Simple_Name
             (Ada.Strings.Unbounded.To_String (Scenario_Full_Path)));

      --text_io.put_line("===> " & Ada.Directories.Kind(Ada.Strings.Unbounded.To_String(Scenario_Path & "\..\land_navigation\"))'Img);

      Scenario_Land_Navigation_Path := Scenario_Path & "\..\land_navigation\";
      Scenario_Sea_Navigation_Path  := Scenario_Path & "\..\water_navigation\";

      Validate_Paths
        (Scenario_Path, Scenario_Land_Navigation_Path,
         Scenario_Sea_Navigation_Path, Ret);

      if Ret then
         if Response = Gtk.Dialog.Gtk_Response_OK then

            Hexagon.Server_Map.Save_Map
              (Scenario_Path & "\" & Scenario_Name, Hexagon.Server_Map.A_Map);
            null;
            Hexagon.Server_Navigation.Save_Navigation
              (Scenario_Land_Navigation_Path & "\" & Scenario_Name,
               A_Land_Navigation);
            Hexagon.Server_Navigation.Save_Navigation
              (Scenario_Sea_Navigation_Path & "\" & Scenario_Name,
               A_Sea_Navigation);

         end if;
      end if;

   end On_Button_Save;

   procedure Validate_Files
     (P_Scenario_File : in Ada.Strings.Unbounded.Unbounded_String;
      P_Scenario_Land_Navigation_File : in Ada.Strings.Unbounded
        .Unbounded_String;
      P_Scenario_Sea_Navigation_File : in Ada.Strings.Unbounded
        .Unbounded_String;
      P_Ret : out Boolean)
   is
   begin
      Text_IO.Put_Line
        ("Scenario_Path:" & Ada.Strings.Unbounded.To_String (P_Scenario_File));

      Text_IO.Put_Line
        ("Scenario_Land_Navigation:" &
         Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_File));

      Text_IO.Put_Line
        ("Scenario_Sea_Navigation:" &
         Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_File));

      P_Ret := True;
      if Ada.Directories.Exists
          (Ada.Strings.Unbounded.To_String (P_Scenario_File)) then
         Gtk.Label.Set_Label
           (The_Window.all.lblMapPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_File) & " Ok");

      else
         Gtk.Label.Set_Label
           (The_Window.all.lblMapPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_File) & " Not Found");
         P_Ret := False;
      end if;
      if Ada.Directories.Exists
          (Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_File))
      then
         Gtk.Label.Set_Label
           (The_Window.all.lblLandNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_File) &
            " Ok");
      else
         Gtk.Label.Set_Label
           (The_Window.all.lblLandNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Land_Navigation_File) &
            " Not Found");
         P_Ret := False;
      end if;
      if Ada.Directories.Exists
          (Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_File))
      then
         Gtk.Label.Set_Label
           (The_Window.all.lblSeaNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_File) &
            " Ok");
      else
         Gtk.Label.Set_Label
           (The_Window.all.lblSeaNavigationPath,
            Ada.Strings.Unbounded.To_String (P_Scenario_Sea_Navigation_File) &
            " Not Found");
         P_Ret := False;
      end if;

   end Validate_Files;

   procedure On_Button_Load (Object : access Gtk_Button_Record'Class) is
      Response                      : Gtk.Dialog.Gtk_Response_Type;
      Scenario_Full_Path            : Ada.Strings.Unbounded.Unbounded_String;
      Scenario_Name                 : Ada.Strings.Unbounded.Unbounded_String;
      Scenario_Path                 : Ada.Strings.Unbounded.Unbounded_String;
      Scenario_Land_Navigation_File : Ada.Strings.Unbounded.Unbounded_String;
      Scenario_Sea_Navigation_File  : Ada.Strings.Unbounded.Unbounded_String;

      Ret : Boolean;

      use Gtk.Dialog;
      use Ada.Strings.Unbounded;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Load - clicked");
      end if;

      Response :=
        Gtk.Dialog.Run (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgFileOpen));
      Gtk.Dialog.Hide (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgFileOpen));

      Scenario_Full_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Gtk.File_Chooser_Dialog.Get_Filename (The_Window.all.dlgFileOpen));

      Text_IO.Put_Line
        ("Scenario_Full_Path:" &
         Ada.Strings.Unbounded.To_String (Scenario_Full_Path));

      Scenario_Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Directories.Containing_Directory
             (Ada.Strings.Unbounded.To_String (Scenario_Full_Path)));
      Scenario_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Ada.Directories.Simple_Name
             (Ada.Strings.Unbounded.To_String (Scenario_Full_Path)));

      Scenario_Map                  := Scenario_Path & "\" & Scenario_Name;
      Scenario_Land_Navigation_Path := Scenario_Path & "\..\land_navigation\";
      Scenario_Sea_Navigation_Path  := Scenario_Path & "\..\water_navigation\";

      Scenario_Land_Navigation_File :=
        Scenario_Land_Navigation_Path & Scenario_Name;
      Scenario_Sea_Navigation_File :=
        Scenario_Sea_Navigation_Path & Scenario_Name;

      Validate_Files
        (Scenario_Map, Scenario_Land_Navigation_File,
         Scenario_Sea_Navigation_File, Ret);

      if Ret then
         if Response = Gtk.Dialog.Gtk_Response_OK then
            Hexagon.Server_Map.Load_Map
              (Scenario_Map, Hexagon.Server_Map.A_Map);

            Hexagon.Client_Map.Init_Client_Map (A_Client_Map);
            for X in 1 .. 100 loop
               for Y in 1 .. 100 loop
                  A_Client_Map.Map (X, Y).all.Pos :=
                    Hexagon.Server_Map.A_Map (X, Y).all.Pos;
                  A_Client_Map.Map (X, Y).all.Landscape_Here :=
                    Hexagon.Server_Map.A_Map (X, Y).all.Landscape_Here;

                  Hexagon.Client_Map.Set_Origo_Patch (A_Client_Map, 1, 1);
               end loop;
            end loop;

            Hexagon.Server_Navigation.Load_Navigation
              (Scenario_Land_Navigation_File, A_Land_Navigation);
            Hexagon.Server_Navigation.Load_Navigation
              (Scenario_Sea_Navigation_File, A_Sea_Navigation);
         end if;

      end if;

      Queue_Draw (The_Window);

   end On_Button_Load;

   procedure On_Button_Check_Path (Object : access Gtk_Button_Record'Class) is

      use Gtk.Dialog;
   begin
--      if Verbose then
      Text_IO.Put_Line
        ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Check_Path - clicked");
--      end if;

      if Gtk.Toggle_Button.Get_Active
          (Gtk.Toggle_Button.Gtk_Toggle_Button (Object)) then
         Text_IO.Put_Line ("Active True");
      else
         Text_IO.Put_Line ("Active False");
      end if;

   end On_Button_Check_Path;

   procedure On_Button_Check_Navigation
     (Object : access Gtk_Button_Record'Class)
   is

      use Gtk.Dialog;
   begin
--      if Verbose then
      Text_IO.Put_Line
        ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Check_Navigation - clicked");
--      end if;

   end On_Button_Check_Navigation;

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Object);
      Gtk.Main.Main_Quit;
   end Exit_Main;

   procedure On_Map_Area_Show (Object : access Gtk_Drawing_Area_Record'Class)
   is

   begin
      if Verbose then
         Text_IO.Put_Line ("On_Map_Area_Show - enter");
      end if;

      The_Window :=
        TubastgaEditor_Window_Pkg.Window1_Access (Get_Toplevel (Object));

      The_Window :=
        TubastgaEditor_Window_Pkg.Window1_Access
          (Gtk.Drawing_Area.Get_Toplevel (Object));

      All_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True, Width => 2600,
           Height    => Glib.Gint (Game_Area_Origo_Y) + 80);

      Scale_Pix :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 2600, Height => 730);

      All_Landscape_On_Patch :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);
      Gdk.Pixbuf.Fill (All_Landscape_On_Patch, Guint32 (0));

      All_Constructions_On_Patch :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);
      Gdk.Pixbuf.Fill (All_Constructions_On_Patch, Guint32 (0));

      All_Effects_On_Patch :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);
      Gdk.Pixbuf.Fill (All_Effects_On_Patch, Guint32 (0));

      All_Landscape_On_Patch :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);

      Server_Map_Origo_Patch := Hexagon.Server_Map.A_Map (1, 1);

      if Verbose then
         Text_IO.Put_Line ("On_Map_Area_Show - exit");
      end if;

   end On_Map_Area_Show;

   procedure Draw_Path
     (P_Navigation : in Hexagon.Server_Navigation.Type_Navigation;
      P_From, P_To : in Hexagon.Type_Hexagon_Position)
   is
      A_Piece : Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece_Access_Class;
      Ret_Status         : Status.Type_Status;
      Trav_Path          : Hexagon.Server_Navigation.Path_Pkg.Cursor;
      A_Prev_Patch       : Hexagon.Client_Map.Type_Client_Patch_Adress;
      A_Curr_Patch       : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Prev_Pos, Curr_Pos : Hexagon.Type_Hexagon_Position;
   begin
      A_Piece := new Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;

      Hexagon.Server_Navigation.Path_Pkg.Clear (A_Path);
      Hexagon.Server_Navigation.Find_Path
        (P_Navigation, Player.Undefined_Player_Id, Action.Type_Action_Type (1),
         A_Piece.all, P_From, P_To, Ret_Status, A_Path);

      Trav_Path := Hexagon.Server_Navigation.Path_Pkg.First (A_Path);
      Prev_Pos  :=
        Hexagon.Server_Navigation.Path_Pkg.Element (Trav_Path).all.Pos;
      Trav_Path := Hexagon.Server_Navigation.Path_Pkg.Next (Trav_Path);
      while Hexagon.Server_Navigation.Path_Pkg.Has_Element (Trav_Path) loop
         Curr_Pos :=
           Hexagon.Server_Navigation.Path_Pkg.Element (Trav_Path).all.Pos;

         A_Prev_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Prev_Pos.A, Prev_Pos.B);

         A_Curr_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_AB
             (A_Client_Map, Curr_Pos.A, Curr_Pos.B);

         if Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB
             (A_Client_Map, A_Prev_Patch.all) in
             0 .. 2460 and
           Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB
             (A_Client_Map, A_Prev_Patch.all) in
             0 .. 1050
            --
             and
           Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB
             (A_Client_Map, A_Curr_Patch.all) in
             0 .. 2460 and
           Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB
             (A_Client_Map, A_Curr_Patch.all) in
             0 .. 1050
         then
            Tubastga_Window_Pkg.FullsizeView.Draw_Arrow
              (A_Client_Map,
               Hexagon.Client_Map.Get_Patch_Adress_From_AB
                 (A_Client_Map, Prev_Pos.A, Prev_Pos.B).all,
               Hexagon.Client_Map.Get_Patch_Adress_From_AB
                 (A_Client_Map, Curr_Pos.A, Curr_Pos.B).all,
               All_Pix);
         end if;

         Prev_Pos := Curr_Pos;

         Trav_Path := Hexagon.Server_Navigation.Path_Pkg.Next (Trav_Path);
      end loop;

   end Draw_Path;

   procedure Draw_Navigation
     (P_Navigation      : in Hexagon.Server_Navigation.Type_Navigation;
      P_Navigation_Node : in Hexagon.Server_Navigation.Type_Navigation_Node)
   is
      Current_TAB : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Trav_Neighbour : Neighbour_List_Pkg.Cursor;

      use Hexagon.Area;
   begin
      Text_IO.Put_Line ("Draw_Navigation - enter");

      Trav_Neighbour := Neighbour_List_Pkg.First (Template_Neighbours);
      while Neighbour_List_Pkg.Has_Element (Trav_Neighbour) loop
         Current_TAB :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (Hexagon.Type_Hexagon_Numbers
                (Integer (P_Navigation_Node.Pos.A) +
                 Integer (Neighbour_List_Pkg.Element (Trav_Neighbour).A)),
              Hexagon.Type_Hexagon_Numbers
                (Integer (P_Navigation_Node.Pos.B) +
                 Integer (Neighbour_List_Pkg.Element (Trav_Neighbour).B)));

         if Navigation_TAB.Navigation_Delta_Position =
           Neighbour_List_Pkg.Element (Trav_Neighbour) then

            Tubastga_Window_Pkg.FullsizeView.Draw_Arrow
              (A_Client_Map,
               Hexagon.Client_Map.Get_Patch_Adress_From_AB
                 (A_Client_Map, P_Navigation_Node.Pos.A,
                  P_Navigation_Node.Pos.B).all,
               Hexagon.Client_Map.Get_Patch_Adress_From_AB
                 (A_Client_Map, Current_TAB.all.Pos.A,
                  Current_TAB.all.Pos.B).all,
               All_Pix);
         end if;

         Trav_Neighbour := Neighbour_List_Pkg.Next (Trav_Neighbour);
      end loop;

      Text_IO.Put_Line ("Draw_Navigation - exit");
   end Draw_Navigation;

   procedure Draw_Navigation_List (P_Draw : Cairo.Cairo_Context) is
      Trav         : Neighbour_List_Pkg.Cursor;
      Is_Neighbour : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg
        .Cursor;

      From_Navigation_Node : Hexagon.Server_Navigation
        .Type_Navigation_Node_Access;
      To_Navigation_Node : Hexagon.Server_Navigation
        .Type_Navigation_Node_Access;

      use Hexagon.Area;
      use Hexagon.Server_Navigation;
   begin
--      Text_IO.Put_Line ("Draw_Navigation_List - enter");

      Cairo.Set_Font_Size (P_Draw, Gdouble (20));
      Cairo.Set_Line_Width (P_Draw, Gdouble (1));

      From_Navigation_Node :=
        Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
          (A_Land_Navigation, Navigation_TAB.From_Pos);

      if From_Navigation_Node /= null then
         Trav := Neighbour_List_Pkg.First (Template_Neighbours);
         while Neighbour_List_Pkg.Has_Element (Trav) loop

            To_Navigation_Node :=
              Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
                (A_Land_Navigation, Navigation_TAB.To_Pos);

            Is_Neighbour :=
              Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Find
                (From_Navigation_Node.all.Neighbours,
                 To_Navigation_Node.all.Id);

            if Navigation_TAB.Navigation_Delta_Position /=
              Neighbour_List_Pkg.Element (Trav) then
               Cairo.Set_Source_Rgb
                 (P_Draw, Gdouble (0), Gdouble (0), Gdouble (0));

               Cairo.Move_To
                 (P_Draw, Gdouble (300),
                  Gdouble (400 + Neighbour_List_Pkg.To_Index (Trav) * 20));
               Cairo.Show_Text
                 (P_Draw,
                  UTF8_String'
                    (Hexagon.Area.To_String
                       (Neighbour_List_Pkg.Element (Trav))));

            elsif Navigation_TAB.Navigation_Delta_Position =
              Neighbour_List_Pkg.Element (Trav) then

               if Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg
                   .Has_Element
                   (Is_Neighbour) then
                  Cairo.Set_Source_Rgb
                    (P_Draw, Gdouble (0), Gdouble (1), Gdouble (0));
               else
                  Cairo.Set_Source_Rgb
                    (P_Draw, Gdouble (1), Gdouble (0), Gdouble (0));
               end if;

               Cairo.Move_To
                 (P_Draw, Gdouble (300),
                  Gdouble (400 + Neighbour_List_Pkg.To_Index (Trav) * 20));
               Cairo.Show_Text
                 (P_Draw,
                  UTF8_String'
                    (Hexagon.To_String (Navigation_TAB.From_Pos) & " to " &
                     Hexagon.Area.To_String
                       (Neighbour_List_Pkg.Element (Trav)) &
                     " " & Hexagon.To_String (Navigation_TAB.To_Pos) &
                     " + To Add or - To Remove"));

            end if;

            Trav := Neighbour_List_Pkg.Next (Trav);
         end loop;
      end if;

--      Text_IO.Put_Line ("Draw_Navigation_List - exit");
   end Draw_Navigation_List;

   function On_Map_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Map_Area_Expose_Event - enter");
      end if;

      -- remove picture from previous frame
      Gdk.Pixbuf.Fill (All_Pix, Glib.Guint32 (0));
      Gdk.Pixbuf.Fill (Scale_Pix, Guint32 (0));

      Hexagon.Client_Map.Reset_Visit;

      Hexagon.Client_Map.Traverse
        (A_Client_Map, A_Client_Map.Origo_Patch, Draw_Map'Access);

      declare
         Land_Navigation : Hexagon.Server_Navigation
           .Type_Navigation_Node_Access;

         Water_Navigation : Hexagon.Server_Navigation
           .Type_Navigation_Node_Access;

         use Hexagon.Client_Map;
         use Hexagon.Server_Navigation;
      begin
         Land_Navigation  := null;
         Water_Navigation := null;

         if Gtk.Toggle_Button.Get_Active
             (Gtk.Toggle_Button.Gtk_Toggle_Button
                (The_Window.all.chkNavigation))
         then
            Text_IO.Put_Line ("Active True:");
            if Left_Button_Pressed_Patch /= null then
               Text_IO.Put_Line ("Naboer:");
               Land_Navigation :=
                 Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
                   (A_Land_Navigation,
                    Hexagon.Type_Hexagon_Position'
                      (Left_Button_Pressed_Patch.all.Pos));

               Water_Navigation :=
                 Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
                   (A_Sea_Navigation,
                    Hexagon.Type_Hexagon_Position'
                      (Left_Button_Pressed_Patch.all.Pos));

               if Land_Navigation /= null then
                  TubastgaEditor_Window_Pkg.Callbacks.Draw_Navigation
                    (A_Land_Navigation, Land_Navigation.all);
               elsif Water_Navigation /= null then
                  TubastgaEditor_Window_Pkg.Callbacks.Draw_Navigation
                    (A_Sea_Navigation, Water_Navigation.all);

               end if;

            end if;

            if Right_Button_Pressed_Patch /= null then
               Text_IO.Put_Line ("Naboer:");
               Land_Navigation :=
                 Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
                   (A_Land_Navigation,
                    Hexagon.Type_Hexagon_Position'
                      (Right_Button_Pressed_Patch.all.Pos));

               Water_Navigation :=
                 Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
                   (A_Sea_Navigation,
                    Hexagon.Type_Hexagon_Position'
                      (Right_Button_Pressed_Patch.all.Pos));

               if Land_Navigation /= null then
                  TubastgaEditor_Window_Pkg.Callbacks.Draw_Navigation
                    (A_Land_Navigation, Land_Navigation.all);
               elsif Water_Navigation /= null then
                  TubastgaEditor_Window_Pkg.Callbacks.Draw_Navigation
                    (A_Sea_Navigation, Water_Navigation.all);

               end if;

            end if;
         end if;

         if Gtk.Toggle_Button.Get_Active
             (Gtk.Toggle_Button.Gtk_Toggle_Button (The_Window.all.chkPath))
         then

            if Left_Button_Pressed_Patch /= null and
              Right_Button_Pressed_Patch /= null then

               Draw_Path
                 (A_Land_Navigation, Left_Button_Pressed_Patch.all.Pos,
                  Right_Button_Pressed_Patch.all.Pos);

            end if;
         end if;

      end;

      Hexagon.Client_Map.Reset_Visit;

      Gdk.Pixbuf.Fill (Scale_Pix, Glib.Guint32 (0));
      Gdk.Pixbuf.Composite
        (All_Pix, Scale_Pix, Glib.Gint (0.0), Glib.Gint (0.0),
         Glib.Gint (1000.0), Glib.Gint (730.0), Glib.Gdouble (0),
         Glib.Gdouble (0), Glib.Gdouble (Map_Scale), Glib.Gdouble (Map_Scale),
         Gdk.Pixbuf.Interp_Nearest, 255);

      Gdk.Cairo.Set_Source_Pixbuf
        (P_Draw, Scale_Pix, Glib.Gdouble (0), Glib.Gdouble (0));

      Cairo.Paint (P_Draw);

      Draw_Navigation_List (P_Draw);

      Cairo.Stroke (P_Draw);

      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Map_Area_Expose_Event - exit");
      end if;

      return True;

   end On_Map_Area_Expose_Event;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1                             : Gdk_Event := To_Event (Params, 1);
      X                                : Gdouble;
      Y                                : Gdouble;
      Left_Button_Server_Pressed_Patch : Hexagon.Server_Map
        .Type_Server_Patch_Adress;
      Right_Button_Server_Pressed_Patch : Hexagon.Server_Map
        .Type_Server_Patch_Adress;

      Paint_Patches : Type_Patches_List;

      use Hexagon.Server_Map;
      use TubastgaEditor_UI_Aux;
   begin
      --if Verbose then
      Text_IO.Put_Line
        ("TubastgaEditor_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - enter");
      --end if;

      Get_Coords (Arg1, X, Y);
      --
      --

      if Gdk.Event.Get_Button (Arg1) = Left_Mouse_Button then
         Left_Button_Pressed_Patch :=
           Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);
         Hexagon.Client_Map.Put (Left_Button_Pressed_Patch.all);

         Left_Button_Server_Pressed_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (Left_Button_Pressed_Patch.Pos.A,
              Left_Button_Pressed_Patch.Pos.B);
         Hexagon.Client_Map.Put (Left_Button_Pressed_Patch.all);

         if TubastgaEditor_UI_Aux.UI_State =
           TubastgaEditor_UI_Aux.Place_Landscape then

            Paint_Patches :=
              Get_Paint_Patches
                (Left_Button_Server_Pressed_Patch.all,
                 TubastgaEditor_UI_Aux.UI_Pencil_Width);

            for Trav in Paint_Patches'First .. Paint_Patches'Last loop

               if Paint_Patches (Trav).P_Valid then

                  A_Client_Map.Map
                    (Integer (Paint_Patches (Trav).A),
                     Integer (Paint_Patches (Trav).B)).all
                    .Landscape_Here :=
                    Landscape.Type_Landscape
                      (TubastgaEditor_UI_Aux.UI_Paint_Landscape);

               end if;

            end loop;

         end if;

         if TubastgaEditor_UI_Aux.UI_State =
           TubastgaEditor_UI_Aux.Place_FillAllLandscape then
            --
            Map_Builder.Fill_Area
              (A_Client_Map,
               Hexagon.Type_Hexagon_Numbers
                 (Hexagon.Server_Map.A_Map'First (1)),
               Hexagon.Type_Hexagon_Numbers
                 (Hexagon.Server_Map.A_Map'Last (1)),
               Hexagon.Type_Hexagon_Numbers
                 (Hexagon.Server_Map.A_Map'First (2)),
               Hexagon.Type_Hexagon_Numbers
                 (Hexagon.Server_Map.A_Map'Last (2)),
               Landscape.Type_Landscape
                 (TubastgaEditor_UI_Aux.UI_Paint_Landscape));

         end if;

         Queue_Draw (The_Window);

      elsif Gdk.Event.Get_Button (Arg1) = Right_Mouse_Button then
         Right_Button_Pressed_Patch :=
           Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);
         Hexagon.Client_Map.Put (Right_Button_Pressed_Patch.all);

         Right_Button_Server_Pressed_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (Right_Button_Pressed_Patch.Pos.A,
              Right_Button_Pressed_Patch.Pos.B);
         Hexagon.Client_Map.Put (Right_Button_Pressed_Patch.all);

         Queue_Draw (The_Window);
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - exit");
      end if;

      return True;
   end On_Map_Area_Button_Press_Event;

   function On_Map_Area_Motion_Notify_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
      X    : Gdouble;
      Y    : Gdouble;

   begin
      Get_Coords (Arg1, X, Y);

      return True;
   end On_Map_Area_Motion_Notify_Event;

   procedure On_Button_Landscape_Grass
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Grass - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_State := TubastgaEditor_UI_Aux.Place_Landscape;

      TubastgaEditor_UI_Aux.UI_Paint_Landscape :=
        Tubastga_Game.Landscape_Grass;

      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Grass - exit");
      end if;
   end On_Button_Landscape_Grass;

   procedure On_Button_Landscape_Forest
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Forest - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_State := TubastgaEditor_UI_Aux.Place_Landscape;

      TubastgaEditor_UI_Aux.UI_Paint_Landscape :=
        Tubastga_Game.Landscape_Forest;

      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Forrest - exit");
      end if;
   end On_Button_Landscape_Forest;

   procedure On_Button_Landscape_Water
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Water - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_State := TubastgaEditor_UI_Aux.Place_Landscape;

      TubastgaEditor_UI_Aux.UI_Paint_Landscape :=
        Tubastga_Game.Landscape_Water;

      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Water - exit");
      end if;
   end On_Button_Landscape_Water;

   procedure On_Button_Landscape_Mountain
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Mountain - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_State := TubastgaEditor_UI_Aux.Place_Landscape;

      TubastgaEditor_UI_Aux.UI_Paint_Landscape :=
        Tubastga_Game.Landscape_Mountain;

      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Landscape_Mountain - exit");
      end if;
   end On_Button_Landscape_Mountain;

   procedure On_Button_FillAllLandscape
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_FillAllLandscape - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_State :=
        TubastgaEditor_UI_Aux.Place_FillAllLandscape;

   end On_Button_FillAllLandscape;

   procedure On_Button_Width1 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Width1 - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_Pencil_Width := TubastgaEditor_UI_Aux.Width1;
   end On_Button_Width1;

   procedure On_Button_Width2 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.callbacks.On_Button_Width2 - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_Pencil_Width := TubastgaEditor_UI_Aux.Width2;
   end On_Button_Width2;

   procedure On_Button_Width3 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("TubastgaEditor_Window_Pkg.Callbacks.On_Button_Width3 - clicked");
      end if;

      TubastgaEditor_UI_Aux.UI_Pencil_Width := TubastgaEditor_UI_Aux.Width3;
   end On_Button_Width3;

   function On_Keyboard_Key_Press (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);

      use Hexagon.Client_Map;
   begin

      Text_IO.Put ("Key Pressed:");

      case Get_Key_Val (Arg1) is

         when GDK_Home =>
            Text_IO.Put_Line ("LEFT-UP");

         when GDK_Page_Up =>
            Text_IO.Put_Line ("RIGHT-UP");

         when GDK_End =>
            Text_IO.Put_Line ("LEFT-DOWN");

         when GDK_Page_Down =>
            Text_IO.Put_Line ("RIGHT-DOWN");

         when GDK_space =>
            Text_IO.Put_Line ("SPACE");

         when GDK_uparrow | GDK_Up =>
            Text_IO.Put_Line ("Uparrow");
            Tubastga_Window_Pkg.ScrolledView.Scroll_Map
              (A_Client_Map, Tubastga_Window_Pkg.ScrolledView.Up);

         when GDK_downarrow | GDK_Down =>
            Text_IO.Put_Line ("Downarrow");
            Tubastga_Window_Pkg.ScrolledView.Scroll_Map
              (A_Client_Map, Tubastga_Window_Pkg.ScrolledView.Down);

         when GDK_leftarrow | GDK_Left =>
            Text_IO.Put_Line ("Leftarrow");
            Tubastga_Window_Pkg.ScrolledView.Scroll_Map
              (A_Client_Map, Tubastga_Window_Pkg.ScrolledView.Left);

         when GDK_rightarrow | GDK_Right =>
            Text_IO.Put_Line ("Rightarrow");
            Tubastga_Window_Pkg.ScrolledView.Scroll_Map
              (A_Client_Map, Tubastga_Window_Pkg.ScrolledView.Right);

         when GDK_Shift_L =>
            Text_IO.Put_Line ("Shift Left");
            TubastgaEditor_UI_Aux.UI_Selecting := True;

         when GDK_Tab =>
            if Left_Button_Pressed_Patch /= null then
               declare
                  Current : Neighbour_List_Pkg.Cursor;
               begin
                  Text_IO.Put_Line
                    ("GDK_Tab -enter  old TAB:" &
                     Navigation_TAB.Navigation_Delta_Position.A'Img & ", " &
                     Navigation_TAB.Navigation_Delta_Position.B'Img);

                  Navigation_TAB.From_Pos := Left_Button_Pressed_Patch.all.Pos;

                  Current :=
                    Neighbour_List_Pkg.Find
                      (Template_Neighbours,
                       Navigation_TAB.Navigation_Delta_Position);
                  Current := Neighbour_List_Pkg.Next (Current);

                  if not Neighbour_List_Pkg.Has_Element (Current) then
                     Current := Neighbour_List_Pkg.First (Template_Neighbours);
                  end if;

                  Navigation_TAB.Navigation_Delta_Position :=
                    Neighbour_List_Pkg.Element (Current);
                  Navigation_TAB.To_Pos.A :=
                    (Hexagon.Type_Hexagon_Numbers
                       (Integer (Left_Button_Pressed_Patch.all.Pos.A) +
                        Integer (Neighbour_List_Pkg.Element (Current).A)));

                  Navigation_TAB.To_Pos.B :=
                    (Hexagon.Type_Hexagon_Numbers
                       (Integer (Left_Button_Pressed_Patch.all.Pos.B) +
                        Integer (Neighbour_List_Pkg.Element (Current).B)));

                  Text_IO.Put_Line
                    ("GDK Tab - exit new TAB:" &
                     Navigation_TAB.Navigation_Delta_Position.A'Img & ", " &
                     Navigation_TAB.Navigation_Delta_Position.B'Img);
               end;
            end if;

         when GDK_minus =>
            Text_IO.Put_Line ("GDK Minus");
            declare
            begin
               if Gtk.Toggle_Button.Get_Active
                   (Gtk.Toggle_Button.Gtk_Toggle_Button
                      (The_Window.all.chkNavigation))
               then

                  Text_IO.Put_Line ("Remove before");
                  Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
                    (A_Land_Navigation, Navigation_TAB.From_Pos,
                     Navigation_TAB.To_Pos);
                  Text_IO.Put_Line ("Remove after");
               end if;
            end;

         when GDK_plus =>
            Text_IO.Put_Line ("GDK Plus");
            declare
            begin
               if Gtk.Toggle_Button.Get_Active
                   (Gtk.Toggle_Button.Gtk_Toggle_Button
                      (The_Window.all.chkNavigation))
               then

                  Text_IO.Put_Line ("Add before");
                  Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
                    (A_Land_Navigation, Navigation_TAB.From_Pos,
                     Navigation_TAB.To_Pos);
                  Text_IO.Put_Line ("Add after");
               end if;
            end;

         when GDK_Escape =>
            Text_IO.Put_Line ("Escape");

         when others =>
            null;
      end case;

      Queue_Draw (The_Window);

      return True;
   end On_Keyboard_Key_Press;

   function On_Keyboard_Key_Released (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);

   begin

      Text_IO.Put ("Key Released:");

      case Get_Key_Val (Arg1) is

         when GDK_Shift_L =>
            Text_IO.Put_Line ("Shift Left");
            TubastgaEditor_UI_Aux.UI_Selecting := False;

         when others =>
            null;
      end case;

      return True;
   end On_Keyboard_Key_Released;

begin
   Hexagon.Client_Map.Init_Client_Map (A_Client_Map);
   Hexagon.Client_Map.Set_Origo_Patch (A_Client_Map, 1, 1);

   Hexagon.Server_Map.Init (Hexagon.Server_Map.A_Map);

   for Trav_X in
     Hexagon.Server_Map.A_Map'First (1) .. Hexagon.Server_Map.A_Map'Last (1)
   loop
      for Trav_Y in
        Hexagon.Server_Map.A_Map'First (2) .. Hexagon.Server_Map.A_Map'Last (2)
      loop
         A_Client_Map.Map (Trav_X, Trav_Y).Pos :=
           Hexagon.Server_Map.A_Map (Trav_X, Trav_Y).Pos;
      end loop;
   end loop;

   Landscape.Server.Init (Tubastga_Game.Landscapes_Type_Info_List);

   --
   Neighbour_List_Pkg.Append
     (Template_Neighbours,
      Hexagon.Area.Type_Hexagon_Delta_Position'
        (True, Hexagon.Area.Type_Hexagon_Delta_Numbers (-1),
         Hexagon.Area.Type_Hexagon_Delta_Numbers (0)));
   Neighbour_List_Pkg.Append
     (Template_Neighbours,
      Hexagon.Area.Type_Hexagon_Delta_Position'
        (True, Hexagon.Area.Type_Hexagon_Delta_Numbers (-1),
         Hexagon.Area.Type_Hexagon_Delta_Numbers (1)));
   Neighbour_List_Pkg.Append
     (Template_Neighbours,
      Hexagon.Area.Type_Hexagon_Delta_Position'
        (True, Hexagon.Area.Type_Hexagon_Delta_Numbers (-1),
         Hexagon.Area.Type_Hexagon_Delta_Numbers (-1)));
   Neighbour_List_Pkg.Append
     (Template_Neighbours,
      Hexagon.Area.Type_Hexagon_Delta_Position'
        (True, Hexagon.Area.Type_Hexagon_Delta_Numbers (1),
         Hexagon.Area.Type_Hexagon_Delta_Numbers (0)));
   Neighbour_List_Pkg.Append
     (Template_Neighbours,
      Hexagon.Area.Type_Hexagon_Delta_Position'
        (True, Hexagon.Area.Type_Hexagon_Delta_Numbers (-1),
         Hexagon.Area.Type_Hexagon_Delta_Numbers (2)));

   Navigation_TAB.Navigation_Delta_Position :=
     Neighbour_List_Pkg.Element
       (Neighbour_List_Pkg.First (Template_Neighbours));
   Navigation_TAB.From_Pos := Hexagon.Type_Hexagon_Position'(True, 1, 1);
   Navigation_TAB.To_Pos   := Hexagon.Type_Hexagon_Position'(True, 1, 1);

end TubastgaEditor_Window_Pkg.Callbacks;
