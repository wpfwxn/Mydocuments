import re, os, os.path
import sasPatternDict as kw

def toUppercase(file, patternDict):
    # Read contents from file as a single string
    if os.path.isfile(file):
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

SASKeyWords = ["proc print data=","proc sort data=","proc format","run;","noobs","datalines","libname ","value ",
               "proc import out=","datafile","dbms","replace","scantime","usedate","run;","data ","set ","then ",
               "if ","else ","eof ", "output","end","format ","nodupkey","by "," var "," label ","merge ","drop ","where ",
               "title ","do ","to ","sum","missing","array","where=","drop ","drop=","rename ","rename=","year","month",
               "proc freq data=","tables ","noprint ","retrain ","mdy","table ","proc sql ","select ","from ","ods ","rtf ",
               "proc sgplot","xaxis","yaxis","close","proc logistic","odsfile","filename","file ","dm ","dim","first.","last.",
               "_n_","proc transpose","descending","max","min","round","rannor","rand","ranks ","proc template ","proc contents ",
               "informat ","length ","length","varnum ","proc tabulate","proc report","column ","define ","put ","otherwise",
               "%include", "%let","%macro ","%mend ","%if ", "%then ","%else ","%do","%end","%put","%goto","%scan","%str",
               "intnx","compress","scan","index","substr","library","proc print","proc export","outfile",
               "fileexist","_temporary_","pad ","truncover ","dsd ","missover ","while "]
def upperSasCode(string,patternList):
    for pattern in patternList:
        string = re.sub(pattern,pattern.upper(),string, flags=re.IGNORECASE)
    return string
file = os.path.join(os.getcwd(),'test.sas')

file = "C:/Users/User/Google Drive/1_Exchange/Compliance/MyCompCheck/CreateTW_H.sas"
toUppercase(file,kw.sasPatternDict)
