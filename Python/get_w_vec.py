#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 30 10:25:10 2017

@author: simon
"""

import os  
import scipy.io
import scipy
import csv
import numpy as np

path = "/media/simon/Elements1/EXIOBASEv3_txt/Extrapolations/w_vector/"
os.chdir(path)

for i in range(1995, 2012):
    if os.path.exists("/media/simon/Elements1/EXIOBASEv3_txt/Extrapolations/w_" + str(i) + ".csv") == False:
        Filestring_Matlab_in      = '/media/simon/Elements1/EXIOBASEv3_txt/EX3_IO_constant/' + str(i) + "_ITC.mat"  
        print('Reading ' + str(i) + ' model from ' + Filestring_Matlab_in)    
        MRIO_A = scipy.io.loadmat(Filestring_Matlab_in)['EB3_A_ITC']
        MRIO_w = 1 - MRIO_A.sum(axis = 0) # calculating relative value added
        print(MRIO_w.shape)
        np.savetxt("w_" + str(i) + ".csv", MRIO_w, delimiter = ",")
        """
        w_csv = open("w_" + str(i) + ".csv", "w")
        c = csv.writer(w_csv)
        c.writerow(MRIO_w[1,:])
        w_csv.close()
"""