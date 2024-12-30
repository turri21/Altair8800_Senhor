module samples_mem_hex_io(
    input clk,
    input [ADDR_WIDTH-1:0] addr,
    input [DATA_WIDTH-1:0] data_in,
    input rd,
    input we,
    output reg [DATA_WIDTH-1:0] data_out,

    // HPS interface
    input        ioctl_download,
    input        ioctl_wr,
    input [24:0] ioctl_addr,
    input  [7:0] ioctl_data,
    input        rom_loaded
);

    parameter integer ADDR_WIDTH = 13;
    parameter integer DATA_WIDTH = 8;
    localparam RAM_SIZE = 4096;  // 4K size

    // Intel HEX parsing states
    reg [3:0] hex_state;
    reg [7:0] hex_byte_count;
    reg [15:0] hex_address;
    reg [7:0] hex_record_type;
    reg [7:0] hex_checksum;
    reg [7:0] hex_data_byte;
    reg [7:0] ascii_byte;
    reg [3:0] hex_digit;
    
    localparam 
        HEX_WAIT_COLON = 0,
        HEX_COUNT_HI = 1,
        HEX_COUNT_LO = 2,
        HEX_ADDR_HI = 3,
        HEX_ADDR_LO = 4,
        HEX_ADDR_LO2 = 5,
        HEX_ADDR_LO3 = 6,
        HEX_RECORD_HI = 7,
        HEX_RECORD_LO = 8,
        HEX_DATA_HI = 9,
        HEX_DATA_LO = 10,
        HEX_CHECKSUM_HI = 11,
        HEX_CHECKSUM_LO = 12;

    // Memory write control signals
    reg hex_write_enable;
    reg [ADDR_WIDTH-1:0] hex_write_addr;
    reg [DATA_WIDTH-1:0] hex_write_data;
    wire [7:0] ram_q;

    // Memory write arbitration
    wire [12:0] write_addr = hex_write_enable ? hex_write_addr : addr;
    wire [7:0] write_data = hex_write_enable ? hex_write_data : data_in;
    wire write_enable = hex_write_enable | we;

    // ASCII to hex conversion function
    function [3:0] ascii_to_hex;
        input [7:0] ascii;
        begin
            case (ascii)
                "0": ascii_to_hex = 4'h0;
                "1": ascii_to_hex = 4'h1;
                "2": ascii_to_hex = 4'h2;
                "3": ascii_to_hex = 4'h3;
                "4": ascii_to_hex = 4'h4;
                "5": ascii_to_hex = 4'h5;
                "6": ascii_to_hex = 4'h6;
                "7": ascii_to_hex = 4'h7;
                "8": ascii_to_hex = 4'h8;
                "9": ascii_to_hex = 4'h9;
                "A","a": ascii_to_hex = 4'hA;
                "B","b": ascii_to_hex = 4'hB;
                "C","c": ascii_to_hex = 4'hC;
                "D","d": ascii_to_hex = 4'hD;
                "E","e": ascii_to_hex = 4'hE;
                "F","f": ascii_to_hex = 4'hF;
                default: ascii_to_hex = 4'h0;
            endcase
        end
    endfunction

    // Intel HEX format parser
    always @(posedge clk) begin
        hex_write_enable <= 0;  // Default to no write

        if (!ioctl_download) begin
            hex_state <= HEX_WAIT_COLON;
        end
        else if (ioctl_wr) begin
            ascii_byte <= ioctl_data;
            hex_digit <= ascii_to_hex(ioctl_data);

            case (hex_state)
                HEX_WAIT_COLON: begin
                    if (ioctl_data == ":") begin
                        hex_state <= HEX_COUNT_HI;
                        hex_checksum <= 0;
                    end
                end

                HEX_COUNT_HI: begin
                    hex_byte_count[7:4] <= hex_digit;
                    hex_state <= HEX_COUNT_LO;
                end

                HEX_COUNT_LO: begin
                    hex_byte_count[3:0] <= hex_digit;
                    hex_checksum <= hex_byte_count;
                    hex_state <= HEX_ADDR_HI;
                end

                HEX_ADDR_HI: begin
                    hex_address[15:12] <= hex_digit;
                    hex_state <= HEX_ADDR_LO;
                end

                HEX_ADDR_LO: begin
                    hex_address[11:8] <= hex_digit;
                    hex_state <= HEX_ADDR_LO2;
                end

                HEX_ADDR_LO2: begin
                    hex_address[7:4] <= hex_digit;
                    hex_state <= HEX_ADDR_LO3;
                end

                HEX_ADDR_LO3: begin
                    hex_address[3:0] <= hex_digit;
                    hex_checksum <= hex_checksum + hex_address[15:8] + hex_address[7:0];
                    hex_state <= HEX_RECORD_HI;
                end

                HEX_RECORD_HI: begin
                    hex_record_type[7:4] <= hex_digit;
                    hex_state <= HEX_RECORD_LO;
                end

                HEX_RECORD_LO: begin
                    hex_record_type[3:0] <= hex_digit;
                    hex_checksum <= hex_checksum + hex_record_type;
                    if (hex_byte_count == 0)
                        hex_state <= HEX_CHECKSUM_HI;
                    else
                        hex_state <= HEX_DATA_HI;
                end

                HEX_DATA_HI: begin
                    hex_data_byte[7:4] <= hex_digit;
                    hex_state <= HEX_DATA_LO;
                end

                HEX_DATA_LO: begin
                    hex_data_byte[3:0] <= hex_digit;
                    // Store data byte if this is a data record (type 00)
                    if (hex_record_type == 8'h00) begin
                        hex_write_enable <= 1;
                        hex_write_addr <= hex_address[ADDR_WIDTH-1:0];
                        hex_write_data <= {hex_data_byte[7:4], hex_digit};
                    end
                    hex_checksum <= hex_checksum + hex_data_byte;
                    hex_address <= hex_address + 1;
                    hex_byte_count <= hex_byte_count - 1;
                    if (hex_byte_count == 1)
                        hex_state <= HEX_CHECKSUM_HI;
                    else
                        hex_state <= HEX_DATA_HI;
                end

                HEX_CHECKSUM_HI: begin
                    hex_state <= HEX_CHECKSUM_LO;
                end

                HEX_CHECKSUM_LO: begin
                    // After checksum, look for next record
                    hex_state <= HEX_WAIT_COLON;
                end
            endcase
        end
    end

    // altsyncram instance
    altsyncram altsyncram_component (
        .address_a (write_addr),
        .clock0 (clk),
        .data_a (write_data),
        .wren_a (write_enable),
        .q_a (ram_q),
        .aclr0 (1'b0),
        .aclr1 (1'b0),
        .address_b (1'b1),
        .addressstall_a (1'b0),
        .addressstall_b (1'b0),
        .byteena_a (1'b1),
        .byteena_b (1'b1),
        .clock1 (1'b1),
        .clocken0 (1'b1),
        .clocken1 (1'b1),
        .clocken2 (1'b1),
        .clocken3 (1'b1),
        .data_b (1'b1),
        .eccstatus (),
        .q_b (),
        .rden_a (1'b1),
        .rden_b (1'b1),
        .wren_b (1'b0));
    defparam
        altsyncram_component.clock_enable_input_a = "BYPASS",
        altsyncram_component.clock_enable_output_a = "BYPASS",
        altsyncram_component.init_file = "zeros.mif",
        altsyncram_component.intended_device_family = "Cyclone V",
        altsyncram_component.lpm_hint = "ENABLE_RUNTIME_MOD=NO",
        altsyncram_component.lpm_type = "altsyncram",
        altsyncram_component.numwords_a = RAM_SIZE,
        altsyncram_component.operation_mode = "SINGLE_PORT",
        altsyncram_component.outdata_aclr_a = "NONE",
        altsyncram_component.outdata_reg_a = "UNREGISTERED",
        altsyncram_component.power_up_uninitialized = "FALSE",
        altsyncram_component.read_during_write_mode_port_a = "NEW_DATA_NO_NBE_READ",
        altsyncram_component.widthad_a = ADDR_WIDTH,
        altsyncram_component.width_a = DATA_WIDTH,
        altsyncram_component.width_byteena_a = 1;

    // Output register
    always @(posedge clk) begin
        if (rd) begin
            data_out <= rom_loaded ? ram_q : 8'h00;
        end
    end

endmodule