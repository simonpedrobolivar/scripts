#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Dec  5 15:15:14 2017

@author: simon
"""


import os  
import sys
import logging
import xlrd, xlwt
import numpy as np
import time
import datetime
import scipy.io
# import string as string
# import scipy.sparse as sparse
# import scipy.sparse.linalg as slinalg
import scipy
#import pandas as pd
import imp
#import csv
import shutil   
import uuid
import matplotlib.pyplot as plt   
import pylab
from sys import platform
import os.path
from itertools import chain




WEO_raw  = xlrd.open_workbook("/home/simon/Dokumente/Master/master_thesis/data/WEO2017_AnnexA.xlsx")

Project_Configsheet = Project_Configfile.sheet_by_name('Config')

