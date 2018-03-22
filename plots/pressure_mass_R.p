set terminal pdf enhanced color font 'sedna,7'
set output 'plotxx.pdf'
set multiplot

set autoscale y
set autoscale x
set autoscale y2

set xlabel "ra [km]"
set y2label "m [M/M_S]"
#set y2range [0:1]
set ylabel "p [dyne/cm^2]"

#set xrange [0:13]
set ytics nomirror
set y2tics
set termoption dash
set format y "%2.1t{/Symbol \327}10^{%L}"



#set logscale y 10
set key right center

plot \
"tabelle_rel.csv" using 1:3 every 10 axis x1y2 with lines title 'mass', \
"tabelle_rel.csv" using 1:2 every 10 linewidth 1 with lines title 'pressure', \
#"tabelle_rel_skyrme.csv" using 1:2 every 10 with lines title 'p skyrme', \
#"tabelle_rel_skyrme.csv" using 1:3 every 10 axis x1y2 with lines title 'm skyrme', \
