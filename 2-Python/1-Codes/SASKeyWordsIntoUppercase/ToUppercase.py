# At Work: type C:\Users\hazobou\Software\Python3.7\Venvs\Gen\Scripts\activate.bat
# in the terminal to activate virtual environment call Gen


import re, os, os.path
import sasPatternDict as kw

def toUppercase(file, patternDict):
    # Read contents from file as a single string
    if os.path.isfile(file):
        print("File found. processing....")
        f_handle = open(file, 'r')
        f_string = f_handle.read()
        f_handle.close()
        # Use RE package to allow for replacement (also allowing for (multiline) REGEX)
        # Loop through the list
        for pattern, substr in patternDict.items():
            f_string = re.sub(pattern, substr.upper(), f_string, flags=re.IGNORECASE)

        # Write contents to file.
        # Using mode 'w' truncates the file.
        f_handle = open(file, 'w')
        f_handle.write(f_string)
        f_handle.close()
    else:
        print("file not found!")

def upperSasCode(string,patternList):
    for pattern in patternList:
        string = re.sub(pattern,pattern.upper(),string, flags=re.IGNORECASE)
    return string
file = os.path.join(os.getcwd(),'test.sas')

# file = "C:/Users/User/Google Drive/1_Exchange/Compliance/MyCompCheck/CreateTW_H.sas"
# file = "//mike.eortc.be/hazobou$/My Documents/EORTC/Tasks/4-1416_Study/1-Compliance/MyCompCheck/CreateTW_H.sas"
# toUppercase(file,kw.sasPatternDict)

file = "//mike.eortc.be/hazobou$/My Documents/EORTC/Tasks/4-1416_Study/1-Compliance/MyCompCheck/Compliance_H.sas"
file = "\\mike.eortc.be\hazobou$\My Documents\EORTC\Tasks\5-Macro_Development\1-Swimmer_plot\validator_H.sas"
file = os.path.join("//mike.eortc.be/hazobou$/My Documents/EORTC/Tasks/5-Macro_Development/1-Swimmer_plot","validator_H.sas")
toUppercase(file,kw.sasPatternDict)
