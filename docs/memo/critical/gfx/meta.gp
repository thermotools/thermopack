set term epslatex size 4.2,3.02
set output 'meta.eps'
set grid ytics
set grid xtics
set xlabel "$T$ (K)" font "Helvetica-Bold,20"
set ylabel "$P$ (bar)" font "Helvetica-Bold,20"
set key left top
set xrange [150:220]

# set terminal png nocrop enhanced font arial 16
# set output 'meta.png'
# set grid ytics
# set grid xtics
# set ylabel "P (bar)" font "arial,20"
# set xlabel "T (K)" font "arial,20"
# set key left top
# set xrange [150:220]

set style line 1 linetype 1 linewidth 2 linecolor rgb "red"
set style line 2 linetype 1 linewidth 2 linecolor rgb "green"
set style line 6 lc rgb "black" pt 7 ps 1   # circle
#!grep "#Critical" envelope.dat | sed "s/#Critical (T K,P Bar)://" > Critical.dat

plot 'envelope.dat' using ($1):($2) title 'Envelope' with line ls 2, \
     'meta.dat' using ($1):($2) title 'Stab. limit' with line ls 1, \
     'Critical.dat' using ($1):($2) title 'Crit. point' with points ls 6
     