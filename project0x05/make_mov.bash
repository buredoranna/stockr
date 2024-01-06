#!/bin/bash
rm *.jpg *.avi
Rscript foo.R
avconv -r 10 -i img%05d.jpg -crf 18 -c:v rawvideo movie.avi
rm *.jpg
