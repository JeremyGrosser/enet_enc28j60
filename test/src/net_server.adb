--
--  Copyright 2024 (C) Jeremy Grosser
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Adafruit_Metro_RP2040.Pins;
with Adafruit_Metro_RP2040.GPIO;
with Adafruit_Metro_RP2040.SPI;
with Adafruit_Metro_RP2040;
with HAL;

with Net.Interfaces.ENC28J60;
with Net.Protos.Arp;
with Net;

package body Net_Server is
   package MRP renames Adafruit_Metro_RP2040;

   --  ENC_INT     : constant := MRP.Pins.A0;
   --  ENC_WOL     : constant := MRP.Pins.A1;
   ENC_CS      : constant := MRP.Pins.A2;
   ENC_RESET   : constant := MRP.Pins.A3;

   procedure ENC_Transfer
      (Data : in out Net.Uint8)
   is
      D : HAL.UInt8_Array (1 .. 1) := (1 => HAL.UInt8 (Data));
   begin
      MRP.SPI.Transfer (D);
      Data := Net.Uint8 (D (1));
   end ENC_Transfer;

   procedure ENC_Chip_Select
      (High : Boolean)
   is
   begin
      MRP.GPIO.Digital_Write (ENC_CS, High);
   end ENC_Chip_Select;

   package ENC is new Net.Interfaces.ENC28J60
      (SPI_Transfer  => ENC_Transfer,
       Chip_Select   => ENC_Chip_Select);

   Ethernet_0 : aliased ENC.ENC28J60_Ifnet;

   Link_Up : Boolean := False;
   Next_Link_State : Boolean := False;

   procedure Initialize is
      use MRP.GPIO;
   begin
      --  Hold ENC_RESET low until we're done configuring pins
      Pin_Mode (ENC_RESET, Output => True, Pull_Up => True);
      Digital_Write (ENC_RESET, False);

      Pin_Mode (ENC_CS, Output => True, Pull_Up => True);
      Digital_Write (ENC_CS, True);

      MRP.SPI.Initialize;
      MRP.SPI.Set_Speed (10_000_000);

      Digital_Write (ENC_RESET, True);
   end Initialize;

   procedure Poll is
   begin
      ENC.Get_Link_Status (Ethernet_0, Next_Link_State);
      if Next_Link_State /= Link_Up then
         --  Link state changed
         Link_Up := Next_Link_State;
         if Link_Up then
            --  Link changed to UP
            Net.DHCP.Initialize (DHCP_Client, Ethernet_0'Access);
            Net.DHCP.Discover (DHCP_Client);
         else
            --  Link changed to DOWN
            null;
         end if;
      end if;

      Net.Protos.Arp.Timeout (Ethernet_0);
   end Poll;

end Net_Server;
