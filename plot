gnuplot -p -e 'unset xtics; unset ytics; set size ratio -1; set key off; plot "< make | tail -n+2" with line'
