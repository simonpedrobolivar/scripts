#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 28 12:11:07 2017

@author: simon
"""

import os
import scipy.io
# import string as string
# import scipy.sparse as sparse
# import scipy.sparse.linalg as slinalg
import scipy
import csv
import numpy as np
#import pandas as pd

for i in range(1995, 2012):
    if os.path.exists("/home/simon/Dokumente/Master/master_thesis/data/EB3/Scenario_4/S_" + str(i) + ".csv") == False:
        #Filestring_Matlab_in      = ProjectSpecs_DataPath1  + MRIO_Name + '_' + ScriptConfig['Datestamp'] + '_' + ScriptConfig['Construct'] + '.mat' 
        Filestring_Matlab_in      = '/media/simon/Elements1/EXIOBASEv3_txt/EX3_IO_constant/' + str(i) + "_ITC.mat"   
        #Mylog.info('Reading '+ MRIO_Name + '_' + ScriptConfig['Datestamp'] + '_' + ScriptConfig['Construct'] + ' model from ' + Filestring_Matlab_in)
        MRIO_S = scipy.io.loadmat(Filestring_Matlab_in)['EB3_S_ITC']
        path = "/home/simon/Dokumente/Master/master_thesis/data/EB3/Scenario_4/"
        os.chdir(path)
        S_csv = open("S_" + str(i) + ".csv", "w")
        c = csv.writer(S_csv)
        #print("year" + str(i + 2000))
        GWP100_indices = np.array([27 , 28 , 29 , 71 , 72 , 73 , 74 , 75 , 76 , 77 , 78,  96,  97, 427, 428, 429, 430, 431, 433, 439, 441, 442]) - 1
        for row in GWP100_indices:
            c.writerow(MRIO_S[row,:])
            #y_workbook.save(path + "Y_" + str(i) + '.xls') 
        S_csv.close()