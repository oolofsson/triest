#!/bin/bash
gnuplot -e "filename='$1'" triest.gnuplot

open triest.png
