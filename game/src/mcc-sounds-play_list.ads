------------------------------------------------------
-------------------
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
--
--    This package makes the system calls to Windows95 to play a list
--    of .wav files supplied by the programmer.  Each .wav fils cannot
--    be longer that 255 characters and must be separated by a ','
--    in the play list.  The play list has no restriction on the number
--    of files or characters.
--
-- This is free software; you can redistribute it and/or
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Thank you.
--
-----------------------------------------------------------------------

-----------------------------------------------------------------------
--Mcc.Sounds.Play_List Specification:
-----------------------------------------------------------------------

package mcc.Sounds.Play_List is

   --------------------------------------------------------------------
   --
   --Play_Sound_List : This procedure will play each of the supplied
   --    .wav file by making the appropriate Windows95 system calls.
   --    Execution of the  running program will be delayed until the
   --    .wav file has completely finished.  This delay is not
   --    noticeable on sounds less than 1 second.  Each .wav file in the
   --    list cannot be longer than 255 characters and must be
   --    separated by a ',' -- spaces are optional.
   --    If any file supplied in List does not exist, no exception will
   --    be raised, and no sound will be played.  If there is already a
   --    sound file being played by the system, it will be cancelled
   --    and the supplied file will be executed.
   --
   --------------------------------------------------------------------
   procedure Play_Sound_List (List : in String);

   --------------------------------------------------------------------
   --
   --Play_Continuous_List : This procedure will play the supplied .wav
   --    files by making the appropriate Windows95 system calls.
   --    Execution of the running program will NOT be delayed and
   --    control will pass directly back to the executing program, and
   --    the .wav file will be played via a called task.  Each .wav
   --    file in the list cannot be longer than 255 characters and
   --    must be  separated by a ',' -- spaces are optional.
   --    If any file supplied in List does not exist, no exception will
   --    be raised, and no sound will be played.  If there is already a
   --    sound file being played by the system, it will be cancelled
   --    and the supplied file will be executed.
   --
   --------------------------------------------------------------------
   procedure Play_Continuous_List (List : in String);

   --------------------------------------------------------------------
   --
   --Stop_Sound_List : This procedure stops a sound started by
   --    Play_Continuous_List.
   --
   --------------------------------------------------------------------
   procedure Stop_Sound_List;

   --------------------------------------------------------------------
   --
   --Play_Background_List : This procedure will play the supplied .wav
   --    files by making the appropriate Windows95 system calls.
   --    Execution of the running program will NOT be delayed and
   --    control will pass directly back to the executing program, and
   --    the .wav file will play via a called task.  It will then loop
   --    and continue playing until Stop_Background_List is called.  If
   --    the program terminates PRIOR to Stop_Background_List being
   --    called, task deadlock will result and the program will not
   --    terminate.  Each .wav file in the list cannot be longer
   --    than 255 characters and must be  separated by a ',' -- spaces
   --    are optional.
   --    If any file supplied in List does not exist, no exception will
   --    be raised, and no sound will be played.  If there is already a
   --    sound file being played by the system, it will be cancelled
   --    and the supplied .wav file will be executed.
   --
   --------------------------------------------------------------------
   procedure Play_Background_List (List : in String);

   --------------------------------------------------------------------
   --
   --Stop_Background_List : This procedure stops the background sound
   --    loop. If the program terminates PRIOR to Stop_Background_List
   --    being called, task deadlock will result and the program will
   --    not terminate.
   --
   --------------------------------------------------------------------
   procedure Stop_Background_List;

   --------------------------------------------------------------------

end mcc.Sounds.Play_List;
