#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Nov 28 15:11:56 2017

@author: simon
"""

import os
import scipy.io
# import string as string
# import scipy.sparse as sparse
# import scipy.sparse.linalg as slinalg
import scipy
import csv
#import pandas as pd

for i in range(1995, 1996):
    #Filestring_Matlab_in      = ProjectSpecs_DataPath1  + MRIO_Name + '_' + ScriptConfig['Datestamp'] + '_' + ScriptConfig['Construct'] + '.mat' 
    Filestring_Matlab_in      = '/media/simon/Elements1/EXIOBASEv3_txt/EX3_IO_constant/' + str(i) + "_ITC.mat"   
    #Mylog.info('Reading '+ MRIO_Name + '_' + ScriptConfig['Datestamp'] + '_' + ScriptConfig['Construct'] + ' model from ' + Filestring_Matlab_in)
    MRIO_Cf = scipy.io.loadmat(Filestring_Matlab_in)['EB3_CharacterisationFactors']
    path = "/home/simon/Dokumente/Master/master_thesis/data/EB3/"
    os.chdir(path)
    Cf_csv = open("Cf_" + str(i) + ".csv", "w")
    c = csv.writer(Cf_csv)
    #print("year" + str(i + 2000))
    for row in range(0,26):
        c.writerow(MRIO_Cf[row,:])
        #y_workbook.save(path + "Y_" + str(i) + '.xls') 
    Cf_csv.close()