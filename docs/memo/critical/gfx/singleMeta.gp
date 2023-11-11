set term epslatex size 4.2,3.02
set output 'singleMeta.eps'
set grid ytics
set grid xtics
set ylabel "$P$ (bar)" font "arial,20"
set xlabel "$T$ (K)" font "arial,20"
set key left top
set xrange [200:325]
set yrange [0:100]

set style line 1 linetype 1 linewidth 2 linecolor rgb "black"
set style line 2 linetype 1 linewidth 2 linecolor rgb "blue"
set style line 3 linetype 1 linewidth 2 linecolor rgb "red"
set style line 6 lc rgb "black" pt 7 ps 1   # circle
set style line 7 lc rgb "blue" pt 7 ps 1   # circle
set style line 8 lc rgb "green" pt 7 ps 1   # circle


plot 'saturation.dat' using ($1):($2) title 'Saturation' with lines ls 1, \
     'single.dat' using ($1):($2) title 'Liqiud Meta' with lines ls 2, \
     'single.dat' using ($4):($5) title 'Gas Meta' with lines ls 3
