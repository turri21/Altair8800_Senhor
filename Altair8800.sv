//============================================================================
//  Altair 8800
// 
//	 By Fred VanEijk and Cyril Venditti
//  Created on 11/12/2018
//  Updates: 13SEP24
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================


module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [48:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	//if VIDEO_ARX[12] or VIDEO_ARY[12] is set then [11:0] contains scaled size instead of aspect ratio.
	output [12:0] VIDEO_ARX,
	output [12:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,
	output        VGA_SCALER, // Force VGA scaler
	output        VGA_DISABLE, // analog out is off

	input  [11:0] HDMI_WIDTH,
	input  [11:0] HDMI_HEIGHT,
	output        HDMI_FREEZE,
	output        HDMI_BLACKOUT,

`ifdef MISTER_FB
	// Use framebuffer in DDRAM
	// FB_FORMAT:
	//    [2:0] : 011=8bpp(palette) 100=16bpp 101=24bpp 110=32bpp
	//    [3]   : 0=16bits 565 1=16bits 1555
	//    [4]   : 0=RGB  1=BGR (for 16/24/32 modes)
	//
	// FB_STRIDE either 0 (rounded to 256 bytes) or multiple of pixel size (in bytes)
	output        FB_EN,
	output  [4:0] FB_FORMAT,
	output [11:0] FB_WIDTH,
	output [11:0] FB_HEIGHT,
	output [31:0] FB_BASE,
	output [13:0] FB_STRIDE,
	input         FB_VBL,
	input         FB_LL,
	output        FB_FORCE_BLANK,

`ifdef MISTER_FB_PALETTE
	// Palette control for 8bit modes.
	// Ignored for other video modes.
	output        FB_PAL_CLK,
	output  [7:0] FB_PAL_ADDR,
	output [23:0] FB_PAL_DOUT,
	input  [23:0] FB_PAL_DIN,
	output        FB_PAL_WR,
`endif
`endif

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,

	input         CLK_AUDIO, // 24.576 MHz
	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

	//ADC
	inout   [3:0] ADC_BUS,

	//SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

`ifdef MISTER_DUAL_SDRAM
	//Secondary SDRAM
	//Set all output SDRAM_* signals to Z ASAP if SDRAM2_EN is 0
	input         SDRAM2_EN,
	output        SDRAM2_CLK,
	output [12:0] SDRAM2_A,
	output  [1:0] SDRAM2_BA,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_nCS,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nWE,
`endif

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	input   [6:0] USER_IN,
	output  [6:0] USER_OUT,

	input         OSD_STATUS
);

`include "rtl/display/common.sv"

///////// Default values for ports not used in this core /////////

assign ADC_BUS  = 'Z;
assign {UART_RTS, UART_TXD, UART_DTR} = 0;
assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = '0;  

assign VGA_SL = 0;
assign VGA_F1 = 0;
assign VGA_SCALER  = 0;
assign VGA_DISABLE = 0;
assign HDMI_FREEZE = 0;
assign HDMI_BLACKOUT = 0;

assign AUDIO_S = 0;
assign AUDIO_L = 0;
assign AUDIO_R = 0;
assign AUDIO_MIX = 0;

assign LED_DISK = 0;
assign LED_POWER = 0;
assign BUTTONS = 0;


//
// Pin | USB Name | Signal
// ----+----------+--------------
//  3  | D+       | I   RX (serial in)
//  2  | D-       | O   TX (serial out)
//  8  | TX-      | I   none
//  4  | GND_d    | I   none
//  6  | RX+      | I   none
//  5  | RX-      | I   none
//  9  | TX+      | -   none
//

// this needs testing no sure if [0] or [1] same for mapping in machine
// assign USER_OUT[1] = 0; // TX should be high Z
assign USER_OUT[6] = 1'b1;
assign USER_OUT[5] = 1'b1;
assign USER_OUT[4] = 1'b1;
assign USER_OUT[3] = 1'b1;
assign USER_OUT[2] = 1'b1;
//assign USER_OUT[1] = 1'b1;
assign USER_OUT[0] = 1'b1;

//////////////////////////////////////////////////////////////////

// must run in 16 by 9 aspect ratio so Altair front panel is correct
assign VIDEO_ARX = 16;
assign VIDEO_ARY = 9;

`include "build_id.v"
localparam CONF_STR = {
	"Altair8800;;",
	"-;",
	"O79,Select Program,Empty,zeroToseven,KillBits,SIOEcho,StatusLights,Basic4k32;",
	"T6,Load Program;",
	"OA,Enable TurnMon,No,Yes;",
	"T0,Reset;",
	"-;",
	"V,v2.0.",`BUILD_DATE
};

////////////////////   MACHINE   ///////////////////

//wire rx; // serial rcv
//wire tx; // serial xmt
wire sync; // cpu sync
wire interrupt_ack; // cpu
wire n_memWR; // cpu
wire io_stack; // cpu
wire halt_ack; // cpu
wire ioWR; // cpu
wire m1; // cpu
wire ioRD; // cpu
wire memRD; // cpu
wire inte_led; // cpu
wire [7:0] debugLED;
wire pauseModeSW; // run/stop
wire stepPB; // single step
wire [7:0] dataLEDs;  // display on data LEDs shows input data to processor
wire [15:0] addrLEDs; // display on addr LEDS
wire [7:0] dataOraddrIn;    // input switches for data bus and low addr bus
wire [7:0] addrOrSenseIn;   // input switches for high address bus and Sense SWitches
wire examinePB;       // show data on data LEDs for addrIn - momentary pos edge
wire examine_nextPB;   // show data on data LEDs for addrIn = addrIn + 1 - momentary pos edge
wire depositPB;       // write data selected on dataOraddrIn Switches to address on addrOut LEDS - momentary pos edge
wire deposit_nextPB;    // write data selected on dataOraddrIn Switches to address+1 on addrOut LEDS - momentary pos edge
wire resetPB;           // set PC to 0
wire hold_in = 1'b0;
wire ready_in = 1'b1;

altair machine
(
 .clk(CLK_50M & ~on_off),
 .reset(reset_machine_delayed),
 .rx(USER_IN[0]),
 .tx(USER_OUT[1]),
 .sync(sync),
 .interrupt_ack(interrupt_ack),
 .n_memWR(n_memWR),
 .io_stack(io_stack),
 .halt_ack(halt_ack),
 .ioWR(ioWR),
 .m1(m1),
 .ioRD(ioRD),
 .memRD(memRD),
 .inte_o(inte_led),
 .hlda_o(hold_ack_led),
 .wait_o(wait_led),
 .debugLED(debugLED),
 .pauseModeSW(pauseModeSW),
 .stepPB(stepPB),
 .dataLEDs(dataLEDs),
 .addrLEDs(addrLEDs),
 .dataOraddrIn(dataOraddrIn),
 .addrOrSenseIn(addrOrSenseIn),
 .examinePB(examinePB),
 .examine_nextPB(examine_nextPB),
 .depositPB(depositPB),
 .deposit_nextPB(deposit_nextPB),
 .resetPB(resetPB),
 .hold_in(hold_in),
 .ready_in(ready_in),
 .prg_sel(prg_sel),
 .enable_turn_mon(enable_turn_mon)
);

////////////////////   CLOCKS   ///////////////////

wire locked;

pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(CLK_VIDEO),
	.locked(locked)
);


//////////////////   HPS I/O   ///////////////////
wire  [1:0] buttons;
wire [127:0] status;

wire [10:0] ps2_key;

wire forced_scandoubler;

hps_io #(.CONF_STR(CONF_STR)) hps_io
(
	.clk_sys(CLK_VIDEO),
	.HPS_BUS(HPS_BUS),
	.EXT_BUS(),
	.gamma_bus(),

	.forced_scandoubler(forced_scandoubler),

	.buttons(buttons),
	.status(status),
	.status_menumask({status[5]}),
	
	.ps2_key(ps2_key)
);

/////////////////  RESET  /////////////////////////

wire reset = RESET | status[0] | buttons[1];

wire reset_machine;
wire reset_machine_delayed;

pulse_gen reset_pulse
(
  .clk(CLK_50M),
  .trigger_in( reset | on_off | prg_load),
  .pulse_out(reset_machine)
);

delay
#(
  .DEPTH(4),
  .DATA_WIDTH(1)
)  reset_machine_delay
(
  .clk(clk),
  .reset(reset),
  .data_in(reset_machine),
  .data_out(reset_machine_delayed)
);

/////////////////  MENU  /////////////////////////
reg [2:0] prg_sel;
wire prg_load = status[6];
wire enable_turn_mon = status[10];

always @(posedge prg_load) begin
  prg_sel = status[9:7];
end

/////////////////  FRONT PANEL  //////////////////////////////////
// 0 to 9 Status
// 10 to 17 Data
// 18 to 19 Wait HLDA
// 20 to 35 Address
reg [0:35] leds_status;
reg [1:0]  switches_status[0:24];
reg aux1;
reg aux2;
reg on_off;
reg protect;
reg unprotect;
reg clear;
reg prot_led;
reg wait_led;
reg hold_ack_led;

assign prot_led = 1'b0;

front_panel_mapping	front_panel_mapping	
(
	.reset(reset),
	.clk(CLK_50M),
	.leds_status(leds_status),
	.switches_status(switches_status),
		
	// map altair front panel LEDS from machine
	.data(dataLEDs),
	.addr(addrLEDs),
	.INTE(inte_led),
	.PROT(prot_led),
	.MEMR(memRD),
	.INP(ioRD),
	.M1(m1),
	.OUT(ioWR),
	.HLTA(halt_ack),
	.STACK(io_stack),
	.WO(~n_memWR),
	.INT(interrupt_ack),
	.WAIT(wait_led),
	.HLDA(hold_ack_led),
	
	// map altair front panel SWITCHES to machine
	.sense_addr_sw(addrOrSenseIn),
	.data_addr_sw(dataOraddrIn),
	.on_off_sw(on_off),
	.stop_run_sw(pauseModeSW),
	.step_sw(stepPB),
	.examine_sw(examinePB),
	.examine_next_sw(examine_nextPB),
	.deposit_sw(depositPB),
	.deposit_next_sw(deposit_nextPB),
	.reset_sw(resetPB),
	.clear_sw(clear),
	.protect_sw(protect),
	.unprotect_sw(unprotect),
	.aux1_sw(aux1),
	.aux2_sw(aux2)
);


front_panel front_panel	
(
	.reset(reset),
	.clk(CLK_VIDEO),
	.leds_status_in(leds_status),
	.switches_status(switches_status),
	.ps2_key(ps2_key),
	.vga_r(R),
	.vga_g(G),
	.vga_b(B),
	.vga_hs(HS),
	.vga_vs(VS),
	.vga_h_blank(HBlank),
	.vga_v_blank(VBlank)
);


assign CE_PIXEL = 1;
wire HBlank, VBlank;

wire [7:0] R,G,B;
wire HS,VS;

video_cleaner video_cleaner
(
	.clk_vid(CLK_VIDEO),
	.ce_pix(CE_PIXEL),
	.R(R),
	.G(G),
	.B(B),
	.HSync(HS),
	.VSync(VS),
	.HBlank(HBlank),
	.VBlank(VBlank),
	.VGA_R(VGA_R),
	.VGA_G(VGA_G),
	.VGA_B(VGA_B),
	.VGA_VS(VGA_VS),
	.VGA_HS(VGA_HS),
	.VGA_DE(VGA_DE)
);

endmodule
