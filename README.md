Calculating maximum masses of neutron stars (Tolman–Oppenheimer–Volkoff equations)
Reading in tables that provide equations of state (EoS) for neutron star matter
run from neutron_stars/build with: cmake .. && make && ./exec
Gnuplot scripts in /plots
Code in /src
units can be choosen in src/main.f90
table with input data starts from outside to inside of star (can be changed in src/eos_tpp.f90 (module: read_table)
-> pressure_array = (p_init, ..., p = 0) 
-> eos_array = (eps_init, ..., eps = 0)
-> array starts at r = 0
-> change do 'i = n_arr, 1, -1' to 'do i = 1, n_arr'
