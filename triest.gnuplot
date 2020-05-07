set term png size 1024,1024
set output 'triest.png'

set multiplot layout 2,1 title 'Triest' font ",18"
set yrange [0:]
set xrange [0:]

set ylabel "Global Triangles"
set xlabel "Memory"
set style line 1 lc rgb "blue" lw 3
set grid ytics lt 0 lw 1
set grid xtics lt 0 lw 1

plot filename with linespoints ls 1 title '#triangles'

unset multiplot
