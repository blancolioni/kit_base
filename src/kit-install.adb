with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

package body Kit.Install is

   Kit_Library_Path : Ada.Strings.Unbounded.Unbounded_String;

   procedure Check_Library_Path;

   function User_Home return String;

   ------------------------
   -- Check_Library_Path --
   ------------------------

   procedure Check_Library_Path is
      use Ada.Strings.Unbounded;

      function Try (Path : String) return Boolean;

      ---------
      -- Try --
      ---------

      function Try (Path : String) return Boolean is
         use Ada.Directories;
      begin
         if Path /= ""
           and then Exists (Path)
           and then Kind (Path) = Directory
         then
            Kit_Library_Path := To_Unbounded_String (Path);
            return True;
         end if;
         return False;
      end Try;

   begin
      if Try (".kit") then
         return;
      end if;

      declare
         Path : constant String :=
                  Ada.Directories.Compose
                    (Ada.Directories.Compose
                       (User_Home, ".config"),
                     "kit");
      begin
         if Try (Path) then
            return;
         end if;
      end;

      declare
         Path : constant String :=
           Ada.Environment_Variables.Value ("KIT_LIBRARY_PATH", "");
      begin
         if Try (Path) then
            return;
         end if;
      end;

      raise Constraint_Error with
        "unable to find kit library path";

   end Check_Library_Path;

   ------------------
   -- Library_Path --
   ------------------

   function Library_Path return String is
      use Ada.Strings.Unbounded;
   begin
      Check_Library_Path;
      return To_String (Kit_Library_Path);
   end Library_Path;

   function User_Home return String is
      User_Profile : constant String :=
                       Ada.Environment_Variables.Value
                         ("USERPROFILE", "");
      Home         : constant String :=
                       Ada.Environment_Variables.Value
                         ("HOME", "");
   begin
      if User_Profile /= "" then
         return User_Profile;
      elsif Home /= "" then
         return Home;
      else
         return "";
      end if;
   end User_Home;

end Kit.Install;
