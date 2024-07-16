--
--  Copyright 2024 (C) Jeremy Grosser
--
--  SPDX-License-Identifier: BSD-3-Clause
--
generic
   with procedure SPI_Transfer (Data : in out Net.Uint8);
   with procedure Chip_Select (High : Boolean);
package Net.Interfaces.ENC28J60 is

   type ENC28J60_Ifnet is new Ifnet_Type with private;

   procedure Initialize
      (Ifnet : in out ENC28J60_Ifnet);

   procedure Get_Link_Status
      (Ifnet : in out ENC28J60_Ifnet;
       Up    : out Boolean);

   procedure Send
      (Ifnet : in out ENC28J60_Ifnet;
       Buf   : in out Net.Buffers.Buffer_Type);

   procedure Receive
      (Ifnet : in out ENC28J60_Ifnet;
       Buf   : in out Net.Buffers.Buffer_Type);

private

   type ENC28J60_Ifnet is new Ifnet_Type with record
      Bank            : Uint8 := 0;
      Next_Packet_Ptr : Uint16 := 0;
      Init_Done       : Boolean := False;
   end record;

end Net.Interfaces.ENC28J60;
