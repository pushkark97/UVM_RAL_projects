`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 11/06/2024 02:24:37 PM
// Design Name: 
// Module Name: register
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module register(
input [3:0] din,
input wr,addr,
input clk, rst,
output [3:0] dout
    );

    reg [7:0] tempin;
    reg [7:0] tempout;

    assign dout = tempout;

    always @(posedge clk) begin
     if (rst) begin
         tempin <= 4'h5;
         tempout <= 4'h0;
     end
     else begin
         if (wr) begin
            if (!addr) begin
              tempin <= din;
            end
          end
          else begin
          if (!addr) begin
            tempout <= tempin;
          end
         end        
     end
    end

    
endmodule : register

//////////////////////////////////////////

interface register_if ;
  logic clk,rst;
  logic wr,addr;
  logic [3:0] din, dout;

endinterface
