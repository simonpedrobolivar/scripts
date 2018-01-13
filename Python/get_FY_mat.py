#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 30 09:50:30 2017

@author: simon
"""

import os
import scipy.io
import scipy
import csv


for i in range(1995, 2012):
    if os.path.exists("/media/simon/Elements1/EXIOBASEv3_txt/Extrapolations/FY_matrix/FY_" + str(i) + ".csv") == False:
        Filestring_Matlab_in      = '/media/simon/Elements1/EXIOBASEv3_txt/EX3_IO_constant/' + str(i) + "_ITC.mat"   
        print('Reading ' + str(i) + ' model from ' + Filestring_Matlab_in)    
        MRIO_FY = scipy.io.loadmat(Filestring_Matlab_in)['EB3_FinalDemand_Emissions']
        MRIO_FY = MRIO_FY[:,35:39]
        path = "/media/simon/Elements1/EXIOBASEv3_txt/Extrapolations/FY_matrix/"
        os.chdir(path)
        FY_csv = open("FY_" + str(i) + ".csv", "w")
        c = csv.writer(FY_csv)
        for row in range(0, 1330):
            c.writerow(MRIO_FY[row,:])
        FY_csv.close()