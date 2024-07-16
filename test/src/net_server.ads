--
--  Copyright 2024 (C) Jeremy Grosser
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Net.DHCP;

package Net_Server is

   DHCP_Client : aliased Net.DHCP.Client;

   procedure Initialize;

   procedure Poll;

end Net_Server;
