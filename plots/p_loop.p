set terminal pdf enhanced color font 'sedna,8'
set terminal pdf 
set output 'new_mass_radius_relation.pdf'
set key at 14, 2 
#set title "Masse/Radius"
set multiplot
unset ytics
unset xtics
set xtics format " "
set ytics format " "
#set ytics add ("0.5" 0.5, "1" 1, "1.5" 1.5, "1.68" 1.68, "1.81" Fval)
#set xtics ("6" Xval1, "8" Xval2, "10" Xval3,"12.36" 12.36, "12.86" Xval, "14" Xval4)
set ytics ("0.5" 0.5, "1" 1, "1.5" 1.5, "2.07" 2.07, "2.3" 2.3)
set xtics ("8" 8, "10" 10,"11.63" 11.63, "14" 14)


#set ytics add ("0.5" 0.5, "1" 1, "1.5" 1.5, "1.75" 1.75, "2" Xval2)
#set xtics add ("6" Xval1, "8" Xval2, "10" Xval3,"12.89" 12.89, "14" Xval4)


set autoscale y
set autoscale x 
set termoption dash
set linestyle 1 lc rgb '#0025ad' lt 1 lw 1
set linestyle 2 dt 2 lw 1 lc rgb '#d95319'
set linestyle 3 dt 3 lw 1 lc rgb '#0060ad'

set arrow from 7,2.07 to 11.63, 2.07 nohead dt 3
set arrow from 11.63,0 to 11.63, 2.07 nohead dt 3
#set arrow from 7,1.78 to 12.88, 1.78 nohead dt 3
#set arrow from 12.88,0 to 12.88, 1.78 nohead dt 3



#set arrow from 7,1.68 to 12.36, 1.68 nohead dt 3
#set arrow from 12.36,0 to 12.36, 1.68 nohead dt 3


#set arrow from 7,2.07 to 11.44, 2.07 nohead dt 3
#set arrow from 11.44,0 to 11.44, 2.07 nohead dt 3

set yrange [0:2.4]
set xrange [7:14]
set xlabel 'radius [km]'
#set xlabel 'p_c [MeV/fm^3]'
set ylabel 'mass [M_s]'
#set xrange [8:12.5]
#set yrange [0.3:2.7] 
set ytics nomirror
set key left bottom
set termoption dash

plot \
'p_tabelle.csv' using 1:2 with lines ls 2 title 'Mass'
#'p_tabelle.csv' using 3:2 with lines ls 2 title 'M(p_c)'
#'p_tabelle_skyrme.csv' using 1:2 lw 1 linetype 1 with lines title 'Skyrme parametrization',\
