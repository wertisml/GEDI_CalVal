#!/usr/bin/env python
"""
Script to batch combine simulated footprint data with field data
"""

from __future__ import print_function

import sys
import os
import string
import fileinput
import time


"""
If project name(s) passed as an argument, combine for that project(s).
Otherwise, combine for every project.
"""
if len(sys.argv) > 1:
    #print(len(sys.argv))
    #print(str(sys.argv))
    GEDI_CALVAL_PROJECTS = sys.argv[1:]
else:
    #print(len(sys.argv))
    #print(str(sys.argv))
    GEDI_CALVAL_PROJECTS = next(os.walk('./data'))[1]

GEDI_CALVAL_ROOT = os.getenv('GEDI_CALVAL_ROOT')


timestr = time.strftime("%Y%m%d")


if __name__ == "__main__":
    
    for project in GEDI_CALVAL_PROJECTS:
            
        try:
            
            # R script to create TEX document
            print(project)
            cmd = 'Rscript scripts/combineGEDICalValFootprintData.R -p "%s" -i "%s_r01" -o "%s_r03"' % (project,timestr,timestr)
            
            os.system(cmd)
       
        except Exception:
            pass
        
