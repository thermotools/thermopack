set term epslatex size 4.2,3.02
set output 'meta_init.eps'
set grid ytics
set grid xtics
set ylabel "$\\lambda$ (-)" font "Helvetica-Bold,20"
set xlabel "$T$ (K)" font "Helvetica-Bold,20"
set key left center
set xrange [110:240]

# set terminal png nocrop enhanced font arial 16
# set output 'meta_init.png'
# set grid ytics
# set grid xtics
# set ylabel "lambda (-)" font "arial,20"
# set xlabel "T (K)" font "arial,20"
# set key left center
# #set yrange [1.0915e7:1.0925e7]
# set xrange [110:240]
# #set xrange [185:200]

set style line 1 linetype 1 linewidth 2 linecolor rgb "red"
set style line 2 linetype 1 linewidth 2 linecolor rgb "blue"
set style line 6 lc rgb "black" pt 7 ps 1   # circle
set style line 7 lc rgb "blue" pt 7 ps 1   # circle
set style line 8 lc rgb "green" pt 7 ps 1   # circle

#181.65293942483763
plot 'meta_init.dat' using ($1):($2) title 'P' with line ls 1, \
     'meta_init_TV.dat' using ($1):($2) title 'V' with line ls 2
     