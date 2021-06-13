---------------------------------------------------------------
----------
--
--    Package : Mcc.Sounds.Play_List
--
--    Author  : C3C Jason Bradford Head
--
--    Date Last Modified : 11 May 1999
--
--    Location : United States Air Force Academy
--               Colorado Springs, CO  80840
--
-----------------------------------------------------------------------
--Context:

with System;
with Ada.Unchecked_Deallocation;

-----------------------------------------------------------------------
--Mcc.Sounds.Play_List Implementation:
-----------------------------------------------------------------------

package body mcc.Sounds.Play_List is

   --------------------------------------------------------------------
   --Internal Implementation:
   --------------------------------------------------------------------

   pragma Linker_Options ("c:\usr\lib\libwinmm.a");

   type String_Pointer is access all String;

   --------------------------------------------------------------------

   procedure First_File
     (Play_List         : in out String;
      Next_Sound        : out String;
      Next_Sound_Length : out Integer;
      Is_Space          : out Boolean)
   is

      File_Length : Integer := 1;

   begin
      if Play_List'Length = 0 then
         Next_Sound_Length := 0;

      else
         while (Play_List'Length >= File_Length and then Play_List (File_Length) /= ',') loop
            Next_Sound (File_Length) := Play_List (File_Length);
            File_Length              := File_Length + 1;
         end loop;

         Next_Sound_Length := File_Length - 1;

         if Play_List'Length > File_Length then
            if Play_List (File_Length + 1) = ' ' then
               File_Length := File_Length + 2;
               Is_Space    := True;
            else
               File_Length := File_Length + 1;
               Is_Space    := False;
            end if;
         end if;

         Play_List (1 .. (Play_List'Length - File_Length + 1))   :=
           Play_List (File_Length .. Play_List'Length);

      end if;
   end First_File;

   --------------------------------------------------------------------

   procedure Play (Sound : in System.Address; Module : in System.Address; Soundc : in Integer);
   pragma Import (Stdcall, Play, "PlaySoundA");

   --------------------------------------------------------------------

   task type Play_Task is
      entry Start_Running (Sound : String_Pointer);
   end Play_Task;

   task body Play_Task is

      My_Sound          : String_Pointer;
      Next_Sound        : String (1 .. 255);
      Next_Sound_Length : Integer;
      Is_Space          : Boolean;

      procedure Dispose is new Ada.Unchecked_Deallocation (
         Object => String,
         Name   => String_Pointer);

   begin
      accept Start_Running (Sound : String_Pointer) do
         My_Sound := Sound;
      end Start_Running;

      declare
         Play_List        : String  := My_Sound.all;
         Play_List_Length : Integer := My_Sound.all'Length;
      begin

         while Play_List_Length > 0 loop
            First_File
              (Play_List (1 .. Play_List_Length),
               Next_Sound,
               Next_Sound_Length,
               Is_Space);

            if Next_Sound_Length /= 0 then
               if Is_Space then
                  Play_List_Length := Play_List_Length - Next_Sound_Length - 2;
               else
                  Play_List_Length := Play_List_Length - Next_Sound_Length - 1;
               end if;
            end if;

            declare
               This_Sound : String_Pointer :=
                 new String'(Next_Sound (1 .. Next_Sound_Length) & Character'First);
            begin

               Play
                 (Sound  => This_Sound.all'Address,
                  Module => System.Null_Address,
                  Soundc => 16#20000#);
               Dispose (This_Sound);
            end;

         end loop;

      end;

      Dispose (My_Sound);

   end Play_Task;

   --------------------------------------------------------------------

   type Play_Ptr is access Play_Task;

   Playing : Boolean := False;

   --------------------------------------------------------------------

   task type Back_Task is
      entry Start_Background (Sound : String_Pointer);
   end Back_Task;

   task body Back_Task is

      My_Sound          : String_Pointer;
      Next_Sound        : String (1 .. 255);
      Next_Sound_Length : Integer;
      Is_Space          : Boolean;

      procedure Dispose is new Ada.Unchecked_Deallocation (
         Object => String,
         Name   => String_Pointer);

   begin
      accept Start_Background (Sound : String_Pointer) do
         My_Sound := Sound;
         Playing  := True;
      end Start_Background;

      while Playing loop

         declare
            Play_List        : String  := My_Sound.all;
            Play_List_Length : Integer := My_Sound.all'Length;
         begin

            while Play_List_Length > 0 loop
               First_File
                 (Play_List (1 .. Play_List_Length),
                  Next_Sound,
                  Next_Sound_Length,
                  Is_Space);

               if Next_Sound_Length /= 0 then
                  if Is_Space then
                     Play_List_Length := Play_List_Length - Next_Sound_Length - 2;
                  else
                     Play_List_Length := Play_List_Length - Next_Sound_Length - 1;
                  end if;
               end if;

               declare
                  This_Sound : String_Pointer :=
                    new String'(Next_Sound (1 .. Next_Sound_Length) & Character'First);
               begin
                  Play
                    (Sound  => This_Sound.all'Address,
                     Module => System.Null_Address,
                     Soundc => 16#20000#);
                  Dispose (This_Sound);
               end;

            end loop;
         end;

         delay 0.5;
      end loop;
      Dispose (My_Sound);
   end Back_Task;

   type Background_Ptr is access Back_Task;

   --------------------------------------------------------------------
   --Exported Operations:
   --------------------------------------------------------------------

   procedure Play_Sound_List (List : in String) is

      Play_List         : String := List;
      Play_List_Length  : Integer;
      Next_Sound        : String (1 .. 255);
      Next_Sound_Length : Integer;
      Is_Space          : Boolean;

      procedure Dispose is new Ada.Unchecked_Deallocation (
         Object => String,
         Name   => String_Pointer);

   begin

      Play_List_Length := Play_List'Length;

      while Play_List_Length > 0 loop
         First_File (Play_List (1 .. Play_List_Length), Next_Sound, Next_Sound_Length, Is_Space);

         if Next_Sound_Length /= 0 then
            if Is_Space then
               Play_List_Length := Play_List_Length - Next_Sound_Length - 2;
            else
               Play_List_Length := Play_List_Length - Next_Sound_Length - 1;
            end if;
         end if;

         declare
            This_Sound : String_Pointer :=
              new String'(Next_Sound (1 .. Next_Sound_Length) & Character'First);
         begin
            Play
              (Sound  => This_Sound.all'Address,
               Module => System.Null_Address,
               Soundc => 16#20000#);
            Dispose (This_Sound);
         end;

      end loop;

   end Play_Sound_List;

   --------------------------------------------------------------------

   procedure Play_Continuous_List (List : in String) is

      Play_Wav  : Play_Ptr;
      Wave_Name : String_Pointer := new String'(List);

   begin
      Play_Wav := new Play_Task;
      Play_Wav.all.Start_Running (Wave_Name);
   end Play_Continuous_List;

   --------------------------------------------------------------------

   procedure Stop_Sound_List is

   begin
      Play (System.Null_Address, System.Null_Address, 16#20000#);
   end Stop_Sound_List;

   --------------------------------------------------------------------

   procedure Play_Background_List (List : in String) is

      Play_Back : Background_Ptr;
      Wave_Name : String_Pointer := new String'(List);

   begin
      Play_Back := new Back_Task;
      Play_Back.all.Start_Background (Wave_Name);
   end Play_Background_List;

   --------------------------------------------------------------------

   procedure Stop_Background_List is

   begin
      Playing := False;
      Stop_Sound;
   end Stop_Background_List;

   --------------------------------------------------------------------

end mcc.Sounds.Play_List;
