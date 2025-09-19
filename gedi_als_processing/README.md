# README #


These are the instructions and scripts to create GEDI waveforms and derived metrics from ALS data over the calibration plots.


It relies upon the programs and scripts in gediRat:

https://bitbucket.org/umdgedi/gedisimulator


This is intended to be placed in:

% ~/src/gediRat

and for HOME to be declared as "~"

% setenv HOME ~


If it is placed eslewhere all .csh files will need "bin" modifiying to allow them to find their awk scripts.

Compile and install "gediRat", "gediMetric" and "mapLidar" using make.

Copy all .csh files to somewhere they can be called from "eg ~/bin/csh".


Questions to svenhancock@gmail.com
