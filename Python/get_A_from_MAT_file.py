#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 23 16:29:02 2017

@author: simon
"""
import os  
import scipy.io
import scipy
import csv


for i in range(1995, 2012):
    if os.path.exists("/media/simon/Elements1/EXIOBASEv3_txt/Extrapolations/A_" + str(i) + ".csv") == False:
        Filestring_Matlab_in      = '/media/simon/Elements1/EXIOBASEv3_txt/EX3_IO_constant/' + str(i) + "_ITC.mat"  
        print('Reading ' + str(i) + ' model from ' + Filestring_Matlab_in)    
        MRIO_A = scipy.io.loadmat(Filestring_Matlab_in)['EB3_A_ITC']
        path = "/media/simon/Elements1/EXIOBASEv3_txt/Extrapolations/"
        os.chdir(path)
        A_csv = open("A_" + str(i) + ".csv", "w")
        c = csv.writer(A_csv)
            #print("year" + str(i + 2000))
        for row in range(0,7987):
            c.writerow(MRIO_A[row,:])
            #y_workbook.save(path + "Y_" + str(i) + '.xls') 
        A_csv.close()






"""    
A_mat_txt = open(A_mat_txt, 'w')

for m in range(0,7987):
    for n in range(0, 7987):
        A_mat_txt.write(MRIO_A[m,n] + '\t')
    A_mat_txt.write("\n")  

A_mat_txt.close() 





import os
import xlrd, xlwt
import csv


path = "/home/simon/Dokumente/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables/"
os.chdir(path)

myfont = xlwt.Font()
myfont.bold = True
mystyle = xlwt.XFStyle()
mystyle.font = myfont


for i in range(2010, 2012):
    io_file  = xlrd.open_workbook(path + "wiot_" + str(i) + ".xls")
    io_sheet = io_file.sheet_by_index(0)
    #io_sheet = io_file.sheet_by_name('USE_DEU')
    #y_workbook  = xlwt.Workbook(encoding = 'ascii') 
    #y_worksheet = y_workbook.add_sheet('Y_matrix')
    A_csv = open("wiot_" + str(i) + ".csv", "w")
    c = csv.writer(A_csv)
    #print("year" + str(i + 2000))
    for row in range(0, 1449):
        c.writerow(io_sheet.row_values(row))
    #y_workbook.save(path + "Y_" + str(i) + '.xls') 
    A_csv.close()



"""








