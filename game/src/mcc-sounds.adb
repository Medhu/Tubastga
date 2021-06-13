------------------------------------------------------
-------------------
--
--    Package : Mcc.Sounds
--
--    Authors : Code written by Dr. Martin Carlisle and James Davis
--              Abstracted to package form by Capt. Patrick Maes
--
--    Date Last Modified : 1 Oct 98 (new fiscal year programming money)
--
--    Location : United States Air Force Academy
--               Colorado Springs, CO  80840
--
--    This package makes the system calls to Windows95 to play .wav
--    files supplied by the programmer.  The two procedures, Play_Sound
--    and Play_Continuous, are supplied with the name of the physical
--    file on the disk.  The two procedures differ only in that
--    Play_Continuous passes control immediately back to the program
--    as the .wav file is being played.  Play_Sound will delay
--    execution of the running program until the file is played in its
--    entirety.
--
-- This is free software; you can redistribute it and/or
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-----------------------------------------------------------------------

with System;
with Ada.Unchecked_Deallocation;

package body mcc.Sounds is

   pragma Linker_Options ("-lwinmm");

   type String_Pointer is access all String;

   task type Play_Task is
      entry Start_Running (Sound : String_Pointer);
   end Play_Task;

   procedure Play (Sound : in System.Address; Module : in System.Address; Soundc : in Integer);
   pragma Import (Stdcall, Play, "PlaySoundA");

   task body Play_Task is

      My_Sound : String_Pointer;
      procedure Dispose is new Ada.Unchecked_Deallocation (
         Object => String,
         Name   => String_Pointer);

   begin
      accept Start_Running (Sound : String_Pointer) do
         My_Sound := Sound;
      end Start_Running;
      Play (Sound => My_Sound.all'Address, Module => System.Null_Address, Soundc => 16#20000#);
      Dispose (My_Sound);
   end Play_Task;

   type Play_Ptr is access Play_Task;

   Playing : Boolean := False;

   task type Back_Task is
      entry Start_Background (Sound : String_Pointer);
   end Back_Task;

   task body Back_Task is
      My_Sound : String_Pointer;

      procedure Dispose is new Ada.Unchecked_Deallocation (
         Object => String,
         Name   => String_Pointer);

   begin
      accept Start_Background (Sound : String_Pointer) do
         My_Sound := Sound;
         Playing  := True;
      end Start_Background;

      while Playing loop
         Play (Sound => My_Sound.all'Address, Module => System.Null_Address, Soundc => 16#20000#);
         delay 0.5;
      end loop;
      Dispose (My_Sound);
   end Back_Task;

   type Background_Ptr is access Back_Task;

   --------------------------------------------------------------------
   --
   --    Procedure : Play_Sound
   --
   --    Purpose : This procedure will play the supplied .wav file by
   --    making the appropriate Windows95 system calls.  Execution of the
   --    running program will be delayed until the .wav file has completely
   --    finished.  This delay is not noticable on sounds less than 1
   --    second.
   --
   --    If the file supplied in Name does not exist, no exception will be
   --    raised, and no sound will be played.  If there is already a sound
   --    file being played by the system, it will be cancelled and the
   --    supplied file will be executed.
   --
   --------------------------------------------------------------------
   procedure Play_Sound (Name : in String) is

      File_Name : String := Name & Character'First;

   begin
      Play (File_Name (File_Name'First)'Address, System.Null_Address, 16#20000#);
   end Play_Sound;

   --------------------------------------------------------------------
   --
   --    Procedure : Stop_Sound
   --
   --    Purpose : This procedure stops a sound started by
   --    Play_Continuous.
   --------------------------------------------------------------------
   procedure Stop_Sound is
   begin
      Play (System.Null_Address, System.Null_Address, 16#20000#);
   end Stop_Sound;

   --------------------------------------------------------------------
   --
   --    Procedure : Play_Continuous
   --
   --    Purpose : This procedure will play the supplied .wav file by
   --    making the appropriate Windows95 system calls.  Execution of the
   --    running program will NOT be delayed and control will pass directly
   --    back to the executing program, and the .wav file will be played
   --    via a called task.
   --
   --    If the file supplied in Name does not exist, no exception will be
   --    raised, and no sound will be played.  If there is already a sound
   --    file being played by the system, it will be cancelled and the
   --    supplied .wav file will be executed.
   --
   --------------------------------------------------------------------
   procedure Play_Continuous (Name : in String) is

      Play_Wav  : Play_Ptr;
      Wave_Name : String_Pointer := new String'(Name & Character'First);

   begin
      Play_Wav := new Play_Task;
      Play_Wav.all.Start_Running (Wave_Name);
   end Play_Continuous;

   --------------------------------------------------------------------
   --
   --    Procedure : Play_Background
   --
   --    Purpose : This procedure will play the supplied .wav file by
   --    making the appropriate Windows95 system calls.  Execution of the
   --    running program will NOT be delayed and control will pass directly
   --    back to the executing program, and the .wav file will be played
   --    via a called task.  It will then loop and continue playing until
   --    Stop_Background is called.  If the program terminates PRIOR to
   --    Stop_Background being called, task deadlock will result and the
   --    program will not terminate.
   --
   --    If the file supplied in Name does not exist, no exception will be
   --    raised, and no sound will be played.  If there is already a sound
   --    file being played by the system, it will be cancelled and the
   --    supplied .wav file will be executed.
   --
   --------------------------------------------------------------------
   procedure Play_Background (Name : in String) is

      Play_Back : Background_Ptr;
      Wave_Name : String_Pointer := new String'(Name & Character'First);

   begin
      Play_Back := new Back_Task;
      Play_Back.all.Start_Background (Wave_Name);
   end Play_Background;

   --------------------------------------------------------------------
   --
   --    Procedure : Stop_Background
   --
   --    Purpose : This procedure stops the background sound loop.
   --    If the program terminates PRIOR to Stop_Background being
   --    called, task deadlock will result and the program will not
   --    terminate.
   --
   --    If the file supplied in Name does not exist, no exception will be
   --    raised, and no sound will be played.  If there is already a sound
   --    file being played by the system, it will be cancelled and the
   --    supplied .wav file will be executed.
   --
   --------------------------------------------------------------------
   procedure Stop_Background is

   begin
      Playing := False;
      Stop_Sound;
   end Stop_Background;

end mcc.Sounds;
