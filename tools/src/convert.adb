with Text_IO;
with Hexagon;
with Hexagon.Server_Map;
with Ada.Strings.Unbounded;
with Hexagon.Navigation;
with Landscape;
with Ada.Command_Line;

--
-- gnatmake -Ad:\Ada\Git\Sisyfos\src convert.adb
--

procedure Convert is
   A_Navigation                   : Hexagon.Navigation.Type_Navigation;
   A_Navigation_Node : Hexagon.Navigation.Type_Navigation_Node_Access;
   A_Navigation_Node_Id           : Hexagon.Navigation.Type_Navigation_Node_Id;
   A_Neighbour_Navigation_Node_Id : Hexagon.Navigation.Type_Navigation_Node_Id;

   use Ada.Strings.Unbounded;
   use Hexagon;
   use Hexagon.Server_Map;
   use Landscape;
   use Hexagon.Navigation;
begin
   Text_IO.Put_Line ("Convert map ");

   Hexagon.Server_Map.Init (Hexagon.Server_Map.A_Map);
   Hexagon.Server_Map.Load_Map
     (Ada.Strings.Unbounded.To_Unbounded_String
        (Ada.Command_Line.Argument (2)),
      Hexagon.Server_Map.A_Map);

   for A in
     Hexagon.Server_Map.A_Map'First (1) .. Hexagon.Server_Map.A_Map'Last (1)
   loop
      for B in
        Hexagon.Server_Map.A_Map'First (2) .. Hexagon.Server_Map.A_Map'Last (2)
      loop

         if
           (Ada.Strings.Unbounded.To_Unbounded_String
              (Ada.Command_Line.Argument (1)) =
            "land" and
            (Hexagon.Server_Map.A_Map (A, B).all.Landscape_Here = 100 or
             Hexagon.Server_Map.A_Map (A, B).all.Landscape_Here = 101 or
             Hexagon.Server_Map.A_Map (A, B).all.Landscape_Here = 102))
            --
             or
           (Ada.Strings.Unbounded.To_Unbounded_String
              (Ada.Command_Line.Argument (1)) =
            "water" and
            (Hexagon.Server_Map.A_Map (A, B).all.Landscape_Here = 103))
         then

            A_Navigation_Node_Id :=
              Hexagon.Navigation.Type_Navigation_Node_Id
                (Integer (Hexagon.Server_Map.A_Map (A, B).all.Pos.A) * 1000 +
                 Integer (Hexagon.Server_Map.A_Map (A, B).all.Pos.B));

            A_Navigation_Node := new Hexagon.Navigation.Type_Navigation_Node;
            A_Navigation_Node.all.Id     := A_Navigation_Node_Id;
            A_Navigation_Node.all.Center :=
              Hexagon.Server_Map.A_Map (A, B).Pos;

            for Trav in
              Hexagon.Server_Map.A_Map (A, B).all.Neighbours'First ..
                Hexagon.Server_Map.A_Map (A, B).Neighbours'Last
            loop

               if Hexagon.Server_Map.A_Map (A, B).Neighbours (Trav) /= null
               then
                  if
                    (Ada.Strings.Unbounded.To_Unbounded_String
                       (Ada.Command_Line.Argument (1)) =
                     "land" and
                     (Hexagon.Server_Map.A_Map (A, B).Neighbours (Trav).all
                        .Landscape_Here =
                      100 or
                      Hexagon.Server_Map.A_Map (A, B).Neighbours (Trav).all
                          .Landscape_Here =
                        101 or
                      Hexagon.Server_Map.A_Map (A, B).Neighbours (Trav).all
                          .Landscape_Here =
                        102))
                     --
                      or
                    (Ada.Strings.Unbounded.To_Unbounded_String
                       (Ada.Command_Line.Argument (1)) =
                     "water" and
                     (Hexagon.Server_Map.A_Map (A, B).Neighbours (Trav).all
                        .Landscape_Here =
                      103))

                  then
                     A_Neighbour_Navigation_Node_Id :=
                       Hexagon.Navigation.Type_Navigation_Node_Id
                         (Integer
                            (Hexagon.Server_Map.A_Map (A, B).Neighbours
                               (Trav).all
                               .Pos
                               .A) *
                          1000 +
                          Integer
                            (Hexagon.Server_Map.A_Map (A, B).Neighbours
                               (Trav).all
                               .Pos
                               .B));

                     Hexagon.Navigation.Navigation_Neighbours_List_Pkg.Include
                       (A_Navigation_Node.all.Neighbours,
                        A_Neighbour_Navigation_Node_Id);
                  end if;

               end if;

            end loop;

            Hexagon.Navigation.Navigation_List_Pkg.Append
              (A_Navigation.Navigation_List, A_Navigation_Node);

         end if;

      end loop;
   end loop;

   Hexagon.Navigation.Save_Navigation
     (Ada.Strings.Unbounded.To_Unbounded_String
        ("nav_" & Ada.Command_Line.Argument (2)),
      A_Navigation);

end Convert;
