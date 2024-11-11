`timescale 1ns / 1ns
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: Pushkar K
// 
// Create Date: 11/06/2024 03:14:33 PM
// Design Name: 
// Module Name: RAL_model_for_reg
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
import uvm_pkg::*;
`include "uvm_macros.svh"

class transaction extends uvm_sequence_item;

  bit [3:0] din,dout;
  bit wr, addr; 

`uvm_object_utils_begin(transaction)
`uvm_field_int(din,UVM_ALL_ON);
`uvm_field_int(wr,UVM_ALL_ON);
`uvm_field_int(addr,UVM_ALL_ON);
`uvm_field_int(dout,UVM_ALL_ON);
`uvm_object_utils_end


  function new(string name = "transaction") ;
    super.new(name);
  endfunction

endclass : transaction


//////////////////////////////////////////////////////////////////////////////////

class driver extends uvm_driver#(transaction);

transaction tr;
virtual register_if vif;

`uvm_component_utils(driver)

  function new (string name = "driver", uvm_component parent = null);
   super.new(name,parent);
  endfunction

  virtual function void build_phase (uvm_phase phase);
    super.build_phase(phase);
    if (!(uvm_config_db#(virtual register_if)::get(this, "", "vif",vif)))
      `uvm_error("DRV","Failed to connect to the Virtual interface");
    
    tr = transaction::type_id::create("tr");

  endfunction : build_phase

  task reset_dut(); // dont reset the input here check its value upon reset
    @(posedge vif.clk)
    vif.rst <= 'b1 ;
    vif.wr <= 'b1;
    vif.addr <= 'b0;
    repeat(2) @(posedge vif.clk) ;
    `uvm_info("DRV", $sformatf("Reset of the system is done, din = %0d",vif.din),UVM_NONE)
    vif.rst <= 'b0;
  endtask : reset_dut

  task drive_dut();
    @(posedge vif.clk);
    vif.rst <= 'b0 ; // make sure reset is deasserted
    vif.wr  <= tr.wr;
    vif.addr  <= tr.addr;
   
    if (tr.wr == 'b1 && tr.addr == 'b0) begin
       vif.din <= tr.din;
       repeat(2) @(posedge vif.clk) ;
       `uvm_info("DRV",$sformatf("Write data stimulus found wdata -> din = %0d", vif.din), UVM_NONE);
    end
    else begin 
      repeat(2) @(posedge vif.clk);
      tr.dout = vif.dout; // note that it is blocking assignment since we can't assign DUT interface values to objects of transaction class
       `uvm_info("DRV",$sformatf("Read data stimulus found rdata -> dout = %0d", vif.dout), UVM_NONE);
    end
  
  endtask : drive_dut

  ////////// task to RUN the driver

  virtual task run_phase (uvm_phase phase);
    forever begin //never ending stimulus to DUT from driver
      reset.dut();
      seq_item_port.get_next_item(tr);
       drive_dut();
      seq_item_port.item_done();
    end

  endtask : run_phase

endclass : driver

//////////////////////////////////////////////////////////////////////////////////

class agent extends uvm_agent; // everything here belongs to component_utils {driver + seqr}
`uvm_component_utils(agent)
  driver drv;
  uvm_sequencer#(transaction) seqr;
  
  function new(string name = "agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  function void build_phase (uvm_phase phase);
    super.build_phase(phase);
    drv  =  driver::type_id::create("drv",this);
    seqr = uvm_sequencer#(transaction)::type_id::create("seqr",this);
  endfunction : build_phase

  function void connect_phase (uvm_phase phase);
    super.connect_phase(phase);
    drv.seq_item_port.connect(seqr.seq_item_export);
  endfunction : connect_phase

endclass : agent


//////////////////////////////////REG+REG_BLOCK+Sequence of ADAPTER////////////////////////////////////////////////

class temp_reg extends uvm_reg;
  `uvm_object_utils(temp_reg)

  rand uvm_reg_field temp;

  function new(string name = "temp_reg");
    super.new(name, 4 , UVM_NO_COVERAGE);
  endfunction

  function void build;
    temp = uvm_reg_field::type_id::create("temp");
    temp.configure( .parent(this),
                        .lsb_pos(0),
                        .size(4),
                        .access("RW"),
                        .volatile(0),
                        .reset(4'h4),
                        .has_reset(1),
                        .is_rand(1),
                        .individually_accessible(1)
                       );

  endfunction: build

endclass : temp_reg

//////////////////////////////////////////////////////////////////////////////////

class top_reg_block extends uvm_reg_block; // general skeleton is followed everywhere for this class
  `uvm_object_utils(top_reg_block)

  rand temp_reg temp_reg_inst;

  function new (string name = "top_reg_block");
    super.new(name,build_coverage(UVM_NO_COVERAGE));
  endfunction 

  function void build;
    add_hdl_path("dut","RTL"); // dut is instance of design
    temp_reg_inst = temp_reg::type_id::create("temp_reg_inst");
    temp_reg_inst.build();
    temp_reg_inst.configure(this);
    temp_reg_inst.add_hdl_path_slice("tempin", 0, 4); // for backdoor access to the tempin reg in RTL
    
    default_map = create_map("default_map",'h0,1,UVM_LITTLE_ENDIAN); //name of map, base_addr, no_of_bytes_of_reg,endianness
    default_map.add_reg(temp_reg_inst,'h0,"RW"); // reg_instance, offset where it starts,access->can be overridden in temp_reg class itself
    
    default_map.set_auto_predict(1); //implicit predictor direct commn b/w reg_block & seqr of transactions

    lock_model();
  endfunction

endclass : top_reg_block


//////////////////////////////////////////////////////////////////////////////////

class top_reg_seq extends uvm_sequence;
  `uvm_object_utils(top_reg_seq)

  function new (string name = "top_reg_seq");
    super.new(name);
  endfunction

  top_reg_block top_reg_block_inst;

   task body; 
    uvm_status_e status;
    bit [3:0] rdata,rdata_m;

    bit [3:0] dout_t; // an empty temp reg used to accumulate the data read from the frontdoor or backdoor

    top_reg_block_inst.temp_reg_inst.write(status,4'h5,UVM_FRONTDOOR);
    rdata = top_reg_block_inst.temp_reg_inst.get();
    rdata_m = top_reg_block_inst.temp_reg_inst.get_mirrored_value(); // gets the value of the non-updated value of reg in DUT at present
    `uvm_info("REG_SEQ",$sformatf("Write value rdata = %0d, Mirror = %0d",rdata,rdata_m),UVM_NONE);

    
    top_reg_block_inst.temp_reg_inst.read(status,dout_t,UVM_FRONTDOOR);
    rdata = top_reg_block_inst.temp_reg_inst.get();
    rdata_m = top_reg_block_inst.temp_reg_inst.get_mirrored_value(); // gets the value of the non-updated value of reg in DUT at present
    `uvm_info("REG_SEQ",$sformatf("Read value rdata = %0d, Mirror = %0d Temp_value = %0d",rdata,rdata_m,dout_t),UVM_NONE);

       ///////////////////BACKDOOR WRITE///////////////////////////////////
    top_reg_block_inst.temp_reg_inst.write(status,'h7, UVM_BACKDOOR); //doesnt affect the din input , only affects the tempin reg
    rdata = top_reg_block_inst.temp_reg_inst.get();
    rdata_m = top_reg_block_inst.temp_reg_inst.get_mirrored_value();
    `uvm_info("REG_SEQ", $sformatf("Backdoor write rdata = %0d, Mirror= %0d , Temp_value = %0d", rdata, rdata_m), UVM_NONE);

    ///////////////////BACKDOOR READ///////////////////////////////////

    top_reg_block_inst.temp_reg_inst.read(status,dout_t, UVM_BACKDOOR);
    `uvm_info("REG_SEQ", $sformatf("Backdoor read temp_value = %0d", dout_t), UVM_NONE);
 
     
   endtask: body

endclass : top_reg_seq


/////////////////////////////CONVERTER OF REG2BUS & BUS2REG////////////////////////////

class top_adapter extends uvm_reg_adapter;
`uvm_object_utils(top_adapter)

function new (string name = "top_adapter");
  super.new(name);
endfunction

virtual function uvm_sequence_item reg2bus (const ref uvm_reg_bus_op rw); // this rw has parameters like kind, status, addr, data, byte_en,
  
  transaction tr;
  tr = transaction :: type_id:: create("tr");
  tr.wr = rw.kind == UVM_WRITE ? 'b1 : 'b0 ;
  tr.addr =  rw.addr ; 

  if (tr.wr == 'b1) tr.din = rw.data;
  
  return tr;
endfunction : reg2bus

virtual function void bus2reg (uvm_sequence_item bus_item, ref uvm_reg_bus_op rw);

  
  transaction tr;
  
  assert ($cast(tr,bus_item)) else `uvm_fatal("ADAPTER","Unable to convert transaction to bus_item")

  rw.kind = tr.wr ? UVM_WRITE : UVM_READ ;
  rw.addr = tr.addr ;
  rw.data = tr.dout ;
  rw.status = UVM_IS_OK;

endfunction : bus2reg

endclass : top_adapter


//////////////////////////////////////////////////////////////////////////////////

class env extends uvm_env; // connect top_reg_block inst to adapter & seqr here 
  `uvm_component_utils(env)

  function new (string name = "env", uvm_component parent);
    super.new(name, parent);
  endfunction

  
  agent agent_inst;
  top_reg_block top_reg_block_inst;
  top_adapter top_adapter_inst;


  virtual function void build_phase (uvm_phase phase);
     super.build_phase(phase);
     agent_inst = agent::type_id::create("agent_inst",this);
     top_reg_block_inst = top_reg_block::type_id::create("top_reg_block");
     top_reg_block_inst.build();

     top_adapter_inst = top_adapter::type_id::create("top_adapter_inst");

  endfunction : build_phase

  virtual function void connect_phase (uvm_phase phase);
    super.connect_phase(phase);
    top_reg_block_inst.default_map.set_sequencer(.adapter(top_adapter_inst), .sequencer(agent_inst.seqr));
    top_reg_block_inst.default_map.set_base_addr(0);

  endfunction : connect_phase

endclass : env

//////////////////////////////////////////////////////////////////////////////////
// connect top_reg_block instance present inside top_reg_seq and env , so that they match
//And start the seqr using the top_reg_seq_inst

class test extends uvm_test;   
  `uvm_component_utils(test)
  
  env env_inst;
  top_reg_seq top_reg_seq_inst;

  function new (string name = "test", uvm_component parent);
      super.new(name,parent);
  endfunction

  virtual function void build_phase (uvm_phase phase);
    super.build_phase(phase);
    env_inst = env :: type_id:: create("env_inst",this);
    top_reg_seq_inst = top_reg_seq::type_id::create("top_reg_seq_inst");

  endfunction : build_phase

  virtual task run_phase(uvm_phase phase);
   phase.raise_objection(this);
    top_reg_seq_inst.top_reg_block_inst = env_inst.top_reg_block_inst;
    top_reg_seq_inst.start(env_inst.agent_inst.seqr);
   phase.drop_objection(this);
  
   phase.phase_done.set_drain_time(this,200);

  endtask : run_phase

endclass : test

//////////////////////////////////////////////////////////////////////////////////

module RAL_model_for_reg(

    );

    register_if vif();

    register dut (.clk(vif.clk), 
             .rst(vif.rst),
             .wr(vif.wr) ,
             .addr(vif.addr),
             .din(vif.din) ,
             .dout(vif.dout));
    
    initial vif.clk  <= 'b0;
    
    always #10 vif.clk = ~vif.clk;

    initial begin
      uvm_config_db#(virtual register_if) :: set(null, "*" , "vif", vif);
      uvm_config_db#(int) :: set(null , "*", "include_coverage" , 0);
      run_test("test");

    end

endmodule : RAL_model_for_reg
