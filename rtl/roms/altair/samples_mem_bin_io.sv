module samples_mem_bin_io(
    input clk,
    input [ADDR_WIDTH-1:0] addr,
    input [DATA_WIDTH-1:0] data_in,
    input rd,
    input we,
    output reg [DATA_WIDTH-1:0] data_out,

    input        ioctl_download,
    input        ioctl_wr,
    input [24:0] ioctl_addr,
    input  [7:0] ioctl_data,
    output reg   ioctl_wait,  // Add this signal    
    input        rom_loaded
);

    parameter integer ADDR_WIDTH = 13;  // Large enough for biggest program 8k
    parameter integer DATA_WIDTH = 8;
    // Explicitly define memory size for synthesis
    //localparam RAM_SIZE = 4096;  // 4K size

    // Program RAM with synthesis attribute for BRAM inference
    (* ram_style = "block" *) reg [DATA_WIDTH-1:0] ram[0:(2 ** ADDR_WIDTH)-1];

    // Initialize memory to 0
    initial begin
        integer i;
        for (i = 0; i < 4096; i = i + 1)
            ram[i] = 8'h00;
        for (i = 4096; i < (2 ** ADDR_WIDTH); i = i + 1)
            ram[i] = 8'h00;
    end

    // Handle writes (both ROM loading and regular writes)
    always @(posedge clk) begin
        ioctl_wait <= 0; // Default state
        
        if (ioctl_download && ioctl_wr) begin
            ram[ioctl_addr] <= ioctl_data;
            ioctl_wait <= 1; // Signal we're busy for one cycle
        end
        else if (we) begin
            ram[addr] <= data_in;
        end
    end

    // Handle reads
    always @(posedge clk) begin
        if (rd) begin
            data_out <= rom_loaded ? ram[addr] : 8'h00;
        end
    end

endmodule