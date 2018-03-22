set terminal pdf enhanced color font 'sedna,8'
set style fill transparent solid 0.1
set output 'equation_state.pdf'
set key bottom right 
set multiplot
set y2tics
set ytics nomirror
set y2label ' [MeV/fm^2]'
#set xrange [0:1000]
set ylabel 'mass [M_s]'
set y2label 'e [MeV/fm^3]'
set xlabel 'p [MeV/fm^3]'
set termoption dash
set linestyle 1 lc rgb '#0025ad' lt 1 lw 1
set linestyle 2 dt 2 lw 1 lc rgb '#d95319'
set linestyle 3 dt 3 lw 1 lc rgb '#0060ad'

#set xrange [0:2000]
#set yrange [0:2]
#set y2range [1400:3000]
plot "table.csv" using 2:1 every 2 with dots dt 2 axes x1y2 title 'EoS', \
"p_tabelle.csv" using 3:2 every 2 with lines ls 3 title 'M_{max}', \
