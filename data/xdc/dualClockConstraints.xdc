create_clock -period 2.000 -waveform {0.000 1.000} [get_ports ddrClk_clk]
create_clock -period 4.000 -waveform {0.000 2.000} [get_ports clk]

# slow 2 fast
set_multicycle_path -setup -end -from clk -to ddrClk_clk 2
set_multicycle_path -hold -end -from clk -to ddrClk_clk 1

# fast 2 slow
set_multicycle_path -setup -start -from ddrClk_clk -to clk 2
set_multicycle_path -hold -start -from ddrClk_clk -to clk 1






