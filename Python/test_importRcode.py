#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov 20 10:14:40 2017

@author: simon
"""

import rpy2
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

devtools = importr('devtools')

MRIOtools = devtools.install_github(repo = "simonpedrobolivar/masterthesis/MRIOtools")


from rpy2.rinterface import R_VERSION_BUILD
print(R_VERSION_BUILD)