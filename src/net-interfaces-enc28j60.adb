--
--  Copyright (C) 2024 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Inspired by https://github.com/bprayudha/avr-enc28j60
--
pragma Style_Checks ("M120");
pragma Warnings (Off, "constant ""*"" is not referenced");

package body Net.Interfaces.ENC28J60 is

   ADDR_MASK : constant Uint8 := 16#1F#;
   BANK_MASK : constant Uint8 := 16#60#;

   --  All Banks Registers
   EIE   : constant Uint8 := 16#1B#;
   EIR   : constant Uint8 := 16#1C#;
   ESTAT : constant Uint8 := 16#1D#;
   ECON2 : constant Uint8 := 16#1E#;
   ECON1 : constant Uint8 := 16#1F#;

   --  Bank 0 Registers
   ERDPTL   : constant Uint8 := 16#00#;
   ERDPTH   : constant Uint8 := 16#01#;
   EWRPTL   : constant Uint8 := 16#02#;
   EWRPTH   : constant Uint8 := 16#03#;
   ETXSTL   : constant Uint8 := 16#04#;
   ETXSTH   : constant Uint8 := 16#05#;
   ETXNDL   : constant Uint8 := 16#06#;
   ETXNDH   : constant Uint8 := 16#07#;
   ERXSTL   : constant Uint8 := 16#08#;
   ERXSTH   : constant Uint8 := 16#09#;
   ERXNDL   : constant Uint8 := 16#0A#;
   ERXNDH   : constant Uint8 := 16#0B#;
   ERXRDPTL : constant Uint8 := 16#0C#;
   ERXRDPTH : constant Uint8 := 16#0D#;
   ERXWRPTL : constant Uint8 := 16#0E#;
   ERXWRPTH : constant Uint8 := 16#0F#;
   EDMASTL  : constant Uint8 := 16#10#;
   EDMASTH  : constant Uint8 := 16#11#;
   EDMANDL  : constant Uint8 := 16#12#;
   EDMANDH  : constant Uint8 := 16#13#;
   EDMADSTL : constant Uint8 := 16#14#;
   EDMADSTH : constant Uint8 := 16#15#;
   EDMACSL  : constant Uint8 := 16#16#;
   EDMACSH  : constant Uint8 := 16#17#;

   --  Bank 1 Registers
   EHT0     : constant Uint8 := 16#20#;
   EHT1     : constant Uint8 := 16#21#;
   EHT2     : constant Uint8 := 16#22#;
   EHT3     : constant Uint8 := 16#23#;
   EHT4     : constant Uint8 := 16#24#;
   EHT5     : constant Uint8 := 16#25#;
   EHT6     : constant Uint8 := 16#26#;
   EHT7     : constant Uint8 := 16#27#;
   EPMM0    : constant Uint8 := 16#28#;
   EPMM1    : constant Uint8 := 16#29#;
   EPMM2    : constant Uint8 := 16#2A#;
   EPMM3    : constant Uint8 := 16#2B#;
   EPMM4    : constant Uint8 := 16#2C#;
   EPMM5    : constant Uint8 := 16#2D#;
   EPMM6    : constant Uint8 := 16#2E#;
   EPMM7    : constant Uint8 := 16#2F#;
   EPMCSL   : constant Uint8 := 16#30#;
   EPMCSH   : constant Uint8 := 16#31#;
   EPMOL    : constant Uint8 := 16#34#;
   EPMOH    : constant Uint8 := 16#35#;
   EWOLIE   : constant Uint8 := 16#36#;
   EWOLIR   : constant Uint8 := 16#37#;
   ERXFCON  : constant Uint8 := 16#38#;
   EPKTCNT  : constant Uint8 := 16#39#;

   --  Bank 2 Register
   MACON1   : constant Uint8 := 16#C0#;
   MACON2   : constant Uint8 := 16#C1#;
   MACON3   : constant Uint8 := 16#C2#;
   MACON4   : constant Uint8 := 16#C3#;
   MABBIPG  : constant Uint8 := 16#C4#;
   MAIPGL   : constant Uint8 := 16#C6#;
   MAIPGH   : constant Uint8 := 16#C7#;
   MACLCON1 : constant Uint8 := 16#C8#;
   MACLCON2 : constant Uint8 := 16#C9#;
   MAMXFLL  : constant Uint8 := 16#CA#;
   MAMXFLH  : constant Uint8 := 16#CB#;
   MAPHSUP  : constant Uint8 := 16#CD#;
   MICON    : constant Uint8 := 16#D1#;
   MICMD    : constant Uint8 := 16#D2#;
   MIREGADR : constant Uint8 := 16#D4#;
   MIWRL    : constant Uint8 := 16#D6#;
   MIWRH    : constant Uint8 := 16#D7#;
   MIRDL    : constant Uint8 := 16#D8#;
   MIRDH    : constant Uint8 := 16#D9#;

   --  Bank 3 Registers
   MAADR1   : constant Uint8 := 16#E0#;
   MAADR0   : constant Uint8 := 16#E1#;
   MAADR3   : constant Uint8 := 16#E2#;
   MAADR2   : constant Uint8 := 16#E3#;
   MAADR5   : constant Uint8 := 16#E4#;
   MAADR4   : constant Uint8 := 16#E5#;
   EBSTSD   : constant Uint8 := 16#66#;
   EBSTCON  : constant Uint8 := 16#67#;
   EBSTCSL  : constant Uint8 := 16#68#;
   EBSTCSH  : constant Uint8 := 16#69#;
   MISTAT   : constant Uint8 := 16#EA#;
   EREVID   : constant Uint8 := 16#72#;
   ECOCON   : constant Uint8 := 16#75#;
   EFLOCON  : constant Uint8 := 16#77#;
   EPAUSL   : constant Uint8 := 16#78#;
   EPAUSH   : constant Uint8 := 16#79#;

   --  PHY Registers
   PHCON1    : constant Uint8 := 16#00#;
   PHSTAT1   : constant Uint8 := 16#01#;
   PHHID1    : constant Uint8 := 16#02#;
   PHHID2    : constant Uint8 := 16#03#;
   PHCON2    : constant Uint8 := 16#10#;
   PHSTAT2   : constant Uint8 := 16#11#;
   PHIE      : constant Uint8 := 16#12#;
   PHIR      : constant Uint8 := 16#13#;
   PHLCON    : constant Uint8 := 16#14#;

   --  ERXFCON bit definitions
   UCEN      : constant Uint8 := 16#80#;
   ANDOR     : constant Uint8 := 16#40#;
   CRCEN     : constant Uint8 := 16#20#;
   PMEN      : constant Uint8 := 16#10#;
   MPEN      : constant Uint8 := 16#08#;
   HTEN      : constant Uint8 := 16#04#;
   MCEN      : constant Uint8 := 16#02#;
   BCEN      : constant Uint8 := 16#01#;

   --  EIE bit definitions
   INTIE     : constant Uint8 := 16#80#;
   PKTIE     : constant Uint8 := 16#40#;
   DMAIE     : constant Uint8 := 16#20#;
   LINKIE    : constant Uint8 := 16#10#;
   TXIE      : constant Uint8 := 16#08#;
   WOLIE     : constant Uint8 := 16#04#;
   TXERIE    : constant Uint8 := 16#02#;
   RXERIE    : constant Uint8 := 16#01#;

   --  EIR bit definitions
   PKTIF     : constant Uint8 := 16#40#;
   DMAIF     : constant Uint8 := 16#20#;
   LINKIF    : constant Uint8 := 16#10#;
   TXIF      : constant Uint8 := 16#08#;
   WOLIF     : constant Uint8 := 16#04#;
   TXERIF    : constant Uint8 := 16#02#;
   RXERIF    : constant Uint8 := 16#01#;

   --  ESTAT bit definitions
   INT       : constant Uint8 := 16#80#;
   LATECOL   : constant Uint8 := 16#10#;
   RXBUSY    : constant Uint8 := 16#04#;
   TXABRT    : constant Uint8 := 16#02#;
   CLKRDY    : constant Uint8 := 16#01#;

   --  ECON2 bit definitions
   AUTOINC   : constant Uint8 := 16#80#;
   PKTDEC    : constant Uint8 := 16#40#;
   PWRSV     : constant Uint8 := 16#20#;
   VRPS      : constant Uint8 := 16#08#;

   --  ECON1 bit definitions
   TXRST     : constant Uint8 := 16#80#;
   RXRST     : constant Uint8 := 16#40#;
   DMAST     : constant Uint8 := 16#20#;
   CSUMEN    : constant Uint8 := 16#10#;
   TXRTS     : constant Uint8 := 16#08#;
   RXEN      : constant Uint8 := 16#04#;
   BSEL1     : constant Uint8 := 16#02#;
   BSEL0     : constant Uint8 := 16#01#;

   --  MACON1 bit definitions
   LOOPBK    : constant Uint8 := 16#10#;
   TXPAUS    : constant Uint8 := 16#08#;
   RXPAUS    : constant Uint8 := 16#04#;
   PASSALL   : constant Uint8 := 16#02#;
   MARXEN    : constant Uint8 := 16#01#;

   --  MACON2 bit definitions
   MARST     : constant Uint8 := 16#80#;
   RNDRST    : constant Uint8 := 16#40#;
   MARXRST   : constant Uint8 := 16#08#;
   RFUNRST   : constant Uint8 := 16#04#;
   MATXRST   : constant Uint8 := 16#02#;
   TFUNRST   : constant Uint8 := 16#01#;

   --  MACON3 bit definitions
   PADCFG2   : constant Uint8 := 16#80#;
   PADCFG1   : constant Uint8 := 16#40#;
   PADCFG0   : constant Uint8 := 16#20#;
   TXCRCEN   : constant Uint8 := 16#10#;
   PHDRLEN   : constant Uint8 := 16#08#;
   HFRMLEN   : constant Uint8 := 16#04#;
   FRMLNEN   : constant Uint8 := 16#02#;
   FULDPX    : constant Uint8 := 16#01#;

   --  MICMD bit definitions
   MIISCAN   : constant Uint8 := 16#02#;
   MIIRD     : constant Uint8 := 16#01#;

   --  EBSTCON bit definitions
   BISTST    : constant Uint8 := 16#01#;

   --  MISTAT bit definitions
   NVALID    : constant Uint8 := 16#04#;
   SCAN      : constant Uint8 := 16#02#;
   BUSY      : constant Uint8 := 16#01#;

   --  PHCON1 bit definitions
   PRST      : constant Uint16 := 16#8000#;
   PLOOPBK   : constant Uint16 := 16#4000#;
   PPWRSV    : constant Uint16 := 16#0800#;
   PDPXMD    : constant Uint16 := 16#0100#;

   --  PHSTAT1 bit definitions
   PFDPX     : constant Uint16 := 16#1000#;
   PHDPX     : constant Uint16 := 16#0800#;
   LLSTAT    : constant Uint16 := 16#0004#;
   JBSTAT    : constant Uint16 := 16#0002#;

   --  PHCON2 bit definitions
   FRCLINK   : constant Uint16 := 16#4000#;
   TXDIS     : constant Uint16 := 16#2000#;
   JABBER    : constant Uint16 := 16#0400#;
   HDLDIS    : constant Uint16 := 16#0100#;

   --  Packet Control bit Definitions
   PHUGEEN   : constant Uint8 := 16#08#;
   PPADEN    : constant Uint8 := 16#04#;
   PCRCEN    : constant Uint8 := 16#02#;
   POVERRIDE : constant Uint8 := 16#01#;

   --  SPI ops
   RCR   : constant Uint8 := 16#00#;   --  Read Control Register
   RBM   : constant Uint8 := 16#3A#;   --  Read Buffer Memory
   WCR   : constant Uint8 := 16#40#;   --  Write Control Register
   WBM   : constant Uint8 := 16#7A#;   --  Write Buffer Memory
   BFS   : constant Uint8 := 16#80#;   --  Bit Field Set
   BFC   : constant Uint8 := 16#A0#;   --  Bit Field Clear
   SC    : constant Uint8 := 16#FF#;   --  Soft Reset

   RXSTART_INIT   : constant Uint16 := 16#0000#;
   RXSTOP_INIT    : constant Uint16 := 16#1FFF# - 16#0600# - 1;
   TXSTART_INIT   : constant Uint16 := 16#1FFF# - 16#0600#;
   TXSTOP_INIT    : constant Uint16 := 16#1FFF#;

   MAX_FRAMELEN   : constant := 1500;

   procedure SPI_Write
      (Data : Uint8)
   is
      D : Uint8 := Data;
   begin
      SPI_Transfer (D);
      pragma Unreferenced (D);
   end SPI_Write;

   procedure Read_Op
      (Op    : Uint8;
       Addr  : Uint8;
       Data  : out Uint8)
   is
   begin
      Chip_Select (False);

      Data := Op or (Addr and ADDR_MASK);
      SPI_Write (Data);

      Data := 0;
      SPI_Transfer (Data);

      if (Addr and 16#80#) /= 0 then
         Data := 0;
         SPI_Transfer (Data);
      end if;

      Chip_Select (True);
   end Read_Op;

   procedure Write_Op
      (Op    : Uint8;
       Addr  : Uint8;
       Data  : Uint8)
   is
      D : Uint8;
   begin
      Chip_Select (False);
      D := Op or (Addr and ADDR_MASK);
      SPI_Write (D);
      D := Data;
      SPI_Write (D);
      Chip_Select (True);
   end Write_Op;

   procedure Set_Bank
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8)
   is
   begin
      if (Addr and BANK_MASK) /= Ifnet.Bank then
         Write_Op (BFC, ECON1, BSEL1 or BSEL0);
         Write_Op (BFS, ECON1, (Addr and BANK_MASK) / 32);
         Ifnet.Bank := Addr and BANK_MASK;
      end if;
   end Set_Bank;

   procedure Write
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8;
       Data  : Uint8)
   is
   begin
      Set_Bank (Ifnet, Addr);
      Write_Op (WCR, Addr, Data);
   end Write;

   procedure Write_16
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8;
       Data  : Uint16)
   is
   begin
      Write (Ifnet, Addr, Uint8 (Data and 16#FF#));
      Write (Ifnet, Addr + 1, Uint8 (Data / 256));
   end Write_16;

   procedure Read
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8;
       Data  : out Uint8)
   is
   begin
      Set_Bank (Ifnet, Addr);
      Read_Op (RCR, Addr, Data);
   end Read;

   procedure Read_16
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8;
       Data  : out Uint16)
   is
      X : Uint8;
   begin
      Read (Ifnet, Addr, X);
      Data := Uint16 (X);
      Read (Ifnet, Addr + 1, X);
      Data := Data or (Uint16 (X) * 256);
   end Read_16;

   procedure Phy_Write
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8;
       Data  : Uint16)
   is
      Status : Uint8;
   begin
      Write (Ifnet, MIREGADR, Addr);
      Write_16 (Ifnet, MIWRL, Data);
      loop
         Read (Ifnet, MISTAT, Status);
         exit when (Status and BUSY) = 0;
      end loop;
   end Phy_Write;

   procedure Phy_Read
      (Ifnet : in out ENC28J60_Ifnet;
       Addr  : Uint8;
       Data  : out Uint16)
   is
      Status : Uint8;
   begin
      Write (Ifnet, MIREGADR, Addr);
      Write (Ifnet, MICMD, MIIRD);
      loop
         Read (Ifnet, MISTAT, Status);
         exit when (Status and BUSY) = 0;
      end loop;
      Write (Ifnet, MICMD, 16#00#);
      Read_16 (Ifnet, MIRDH, Data);
   end Phy_Read;

   procedure Write_Buffer
      (Buf : in out Net.Buffers.Buffer_Type)
   is
   begin
      Chip_Select (False);
      SPI_Write (WBM);
      for I in 1 .. Buf.Get_Length loop
         SPI_Write (Buf.Get_Uint8);
      end loop;
      Chip_Select (True);
   end Write_Buffer;

   procedure Read_Buffer
      (Buf : in out Net.Buffers.Buffer_Type)
   is
      Data : Uint8;
   begin
      Chip_Select (False);
      SPI_Write (RBM);
      for I in 1 .. Buf.Get_Length loop
         Data := 0;
         SPI_Transfer (Data);
         Buf.Put_Uint8 (Data);
      end loop;
      Chip_Select (True);
   end Read_Buffer;

   procedure Self_Test
      (Ifnet : in out ENC28J60_Ifnet)
   is
      Status : Uint8;
      DMA_Checksum, BST_Checksum : Uint16;
   begin
      Write_16 (Ifnet, EDMASTL, 16#0000#);
      Write_16 (Ifnet, EDMANDL, 16#1FFF#);
      Write_16 (Ifnet, ERXNDL, 16#1FFF#);
      Write_Op (BFS, ECON1, CSUMEN);
      Write_Op (BFS, EBSTCON, BISTST);
      Write_Op (BFS, ECON1, DMAST);
      loop
         Read_Op (RCR, ECON1, Status);
         exit when (Status and DMAST) = 0;
      end loop;
      Read_16 (Ifnet, EDMACSL, DMA_Checksum);
      Read_16 (Ifnet, EBSTCSL, BST_Checksum);
      if DMA_Checksum /= BST_Checksum then
         raise Program_Error with "ENC28J60 Self Test Failed";
      end if;
   end Self_Test;

   overriding
   procedure Initialize
      (Ifnet : in out ENC28J60_Ifnet)
   is
   begin
      --  Soft reset is unneeded if the hardware reset line was toggled
      --  Write_Op (SC, 0, SC);
      --  delay 0.001; --  Errata 1
      Self_Test (Ifnet);
      Ifnet.Next_Packet_Ptr := RXSTART_INIT;
      Write_16 (Ifnet, ERXSTL, RXSTART_INIT);
      Write_16 (Ifnet, ERXRDPTL, RXSTART_INIT);
      Write_16 (Ifnet, ERXNDL, RXSTOP_INIT);
      Write_16 (Ifnet, ETXSTL, TXSTART_INIT);
      Write_16 (Ifnet, ETXNDL, TXSTOP_INIT);
      Write (Ifnet, ERXFCON, UCEN or CRCEN or PMEN);
      Write (Ifnet, EPMM0, 16#3F#);
      Write (Ifnet, EPMM1, 16#30#);
      Write_16 (Ifnet, EPMCSL, 16#F7F9#);
      Write (Ifnet, MACON1, MARXEN or TXPAUS or RXPAUS);
      Write_Op (BFS, MACON3, PADCFG0 or TXCRCEN or FRMLNEN);
      Write_16 (Ifnet, MAIPGL, 16#0C12#);
      Write (Ifnet, MABBIPG, 16#12#);
      Write_16 (Ifnet, MAMXFLL, MAX_FRAMELEN);
      Write (Ifnet, MAADR5, Ifnet.Mac (6));
      Write (Ifnet, MAADR4, Ifnet.Mac (5));
      Write (Ifnet, MAADR3, Ifnet.Mac (4));
      Write (Ifnet, MAADR2, Ifnet.Mac (3));
      Write (Ifnet, MAADR1, Ifnet.Mac (2));
      Write (Ifnet, MAADR0, Ifnet.Mac (1));
      Phy_Write (Ifnet, PHCON2, HDLDIS);
      Set_Bank (Ifnet, ECON1);
      Write_Op (BFS, EIE, INTIE or PKTIE);
      Write_Op (BFS, ECON1, RXEN);
      Ifnet.Init_Done := True;
   end Initialize;

   overriding
   procedure Send
      (Ifnet : in out ENC28J60_Ifnet;
       Buf   : in out Net.Buffers.Buffer_Type)
   is
      Status : Uint8;
   begin
      loop
         Read_Op (RCR, ECON1, Status);
         exit when (Status and TXRTS) = 0;
         Read (Ifnet, EIR, Status);
         if (Status and TXERIF) /= 0 then
            Write_Op (BFS, ECON1, TXRST);
            Write_Op (BFC, ECON1, TXRST);
         end if;
      end loop;

      Write_16 (Ifnet, EWRPTL, TXSTART_INIT);
      Write_16 (Ifnet, ETXNDL, TXSTART_INIT + Buf.Get_Length);
      Write_Op (WBM, 0, 0);
      Write_Buffer (Buf);
      Write_Op (BFS, ECON1, TXRTS);
   end Send;

   overriding
   procedure Receive
      (Ifnet : in out ENC28J60_Ifnet;
       Buf   : in out Net.Buffers.Buffer_Type)
   is
      RXStat : Uint16;
      Len    : Uint16;
      X      : Uint8;
   begin
      Read (Ifnet, EPKTCNT, X);
      if X = 0 then
         Buf.Set_Length (0);
         return;
      end if;

      Write_16 (Ifnet, ERDPTL, Ifnet.Next_Packet_Ptr);
      Read_Op (RBM, 0, X);
      Ifnet.Next_Packet_Ptr := Uint16 (X);
      Read_Op (RBM, 0, X);
      Ifnet.Next_Packet_Ptr := Ifnet.Next_Packet_Ptr or (Uint16 (X) * 256);

      Read_Op (RBM, 0, X);
      Len := Uint16 (X);
      Read_Op (RBM, 0, X);
      Len := Len or (Uint16 (X) * 256);
      Len := Len - 4;

      Read_Op (RBM, 0, X);
      RXStat := Uint16 (X);
      Read_Op (RBM, 0, X);
      RXStat := RXStat or (Uint16 (X) * 256);

      if Len < Buf.Available then
         Buf.Set_Length (Len);
      else
         Buf.Set_Length (Buf.Available);
      end if;

      if (RXStat and 16#80#) = 0 then
         Buf.Set_Length (0);
      else
         Read_Buffer (Buf);
      end if;

      Write_16 (Ifnet, ERXRDPTL, Ifnet.Next_Packet_Ptr);

      --  if (Ifnet.Next_Packet_Ptr - 1) not in RXSTART_INIT .. RXSTOP_INIT then
      --     Write_16 (Ifnet, ERXRDPTL, RXSTOP_INIT);
      --  else
      Write_16 (Ifnet, ERXRDPTL, Ifnet.Next_Packet_Ptr - 1);

      Write_Op (BFS, ECON2, PKTDEC);
   end Receive;

   procedure Get_Link_Status
      (Ifnet : in out ENC28J60_Ifnet;
       Up    : out Boolean)
   is
      X : Uint16;
   begin
      Phy_Read (Ifnet, PHSTAT2, X);
      Up := (X and 4) /= 0;
   end Get_Link_Status;

end Net.Interfaces.ENC28J60;
