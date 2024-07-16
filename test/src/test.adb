--
--  Copyright 2024 (C) Jeremy Grosser
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Net_Server;

procedure Test is
begin
   Net_Server.Initialize;

   loop
      Net_Server.Poll;
   end loop;
end Test;
