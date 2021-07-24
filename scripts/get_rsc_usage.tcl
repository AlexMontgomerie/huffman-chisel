######################################################################################
# vivado -mode batch -notrace -source scripts/get_rsc_usage.tcl -tclargs (module name)
######################################################################################

# get variables
set PART        xc7z020clg484-1
set BOARD       xilinx.com:zc702:part0:1.4
set FREQ        100

set PATH $::env(PWD)
set dir_name "test_prj"

# Create Vivado Project
create_project -force project_1 $PATH/$dir_name -part $PART
# Set FPGA device/board
set_property board_part $BOARD [current_project]

set module [ lindex $argv 0 ]

# add verilog file to project
add_files -norecurse impl/${module}.v
add_files -fileset constrs_1 clock.xdc

# set top module
set_property top $module [current_fileset]

update_compile_order -fileset sources_1
synth_design -mode out_of_context -top $module -part $PART

report_utilization
#report_timing_summary -delay_type min_max -report_unconstrained -check_timing_verbose -max_paths 10 -input_pins -routable_nets -name timing_1
report_timing_summary
