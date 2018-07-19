# This project uses the virtual environment called Gen
# type the following command to activate:
# C:\Users\hazobou\Software\Python3.7\Venvs\Gen\Scripts\activate.bat

import os, os.path
import re
import datetime as dt
import csv
#  pip install csv

# This script is entended to collect all the macro names in a given folder
# WARNING: If a macro is in a director, it will be ignored

folder = "C:/Users/hazobou/PythonProjects/MacroDoc/EORTC macros/" # LABTOX/
folder = "K:/SAS/EORTC macros/"

def macroList1(folder):
    mlist = {}
    for f in os.listdir(folder):
        path = os.path.join(folder,f)
        if os.path.isfile(path):
            if f.lower().endswith(".sas"):
                mname = re.sub(".sas","",f)
                mlist[mname] = mname
        else:
            print(f)
            macroList(path)
    return mlist

def macroList(folder):
    mlist = []
    with open('listMACROS.csv', 'w', newline='') as t:
        macrowriter = csv.writer(t,delimiter=',',quotechar='|', quoting=csv.QUOTE_MINIMAL)
        macrowriter.writerow(['DATEMODIF','HMODIF','SIZE','NAME'])
        for f in os.listdir(folder):
            path = os.path.join(folder,f)
            # print(path)
            if os.path.isfile(path) and f.lower().endswith(".sas"):
                info = os.stat(path)
                date = dt.datetime.fromtimestamp(info.st_mtime)
                # print( date.date().strftime('%d/%m/%Y') )
                # print( date.time().strftime('%H:%M') )
                # print( info.st_size )
                mname = f # re.sub(".sas|.SAS","",f)
                mlist.append(f)
                macrowriter.writerow([date.date().strftime('%d/%m/%Y'),date.time().strftime('%H:%M'),info.st_size ,mname])
    return mlist

a = macroList(folder)
print(a)
print(len(a))
