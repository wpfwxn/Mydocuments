import re, os, os.path

def toUppercase(file, patternList):
    # Read contents from file as a single string
    if os.path.isfile(file):
        f_handle = open(file, 'r')
        f_string = f_handle.read()
        f_handle.close()
        # Use RE package to allow for replacement (also allowing for (multiline) REGEX)
        # Loop through the list
        for pattern in patternList:
            f_string = re.sub(pattern, pattern.upper(), f_string, flags=re.IGNORECASE)

        # Write contents to file.
        # Using mode 'w' truncates the file.
        f_handle = open(file, 'w')
        f_handle.write(f_string)
        f_handle.close()

sasPatternDict = {"proc +print +data *=":"proc print data=",
                  "proc +sort +data *=":"proc sort data=",
                  "proc +format":"proc format",
                  "proc +import +out *=":"proc import out=",
                  "run *;":"run;",
                  " noobs":" noobs",
                  "datalines":"datalines",
                  "libname ":"libname ",
                  "value ":"value ",
                  "datafile":"datafile",
                  "dbms *=":"dbms=",
                  "replace":"replace",
                  "scantime":"scantime",
                  "usedate":"usedate",
                  "data ":"data ",
                  "set ":"set ",
                  " then ":" then ",
                  "if ":"if ",
                  "else ":"else ",
                  "eof ":"eof ",
                  "output":"output",
                  "end":"end",
                  "format ":"format ",
                  "nodupkey":"nodupkey",
                  "\bby\b":"by",
                  "\bvar\b ":"var ",
                  "\blabel\b":"label",
                  "\bmerge\b":"merge",
                  "drop ":"drop",
                  "where ":"where ",
                  "title ":"title ",
                  "\bdo\b":"do",
                  "\bto\b":"to",
                  "sum\(":"sum(",
                  "\bmissing\b":"missing",
                  "array":"array",
                  "where *=":"where=",
                  "\bdrop\b":"drop",
                  "drop *=":"drop=",
                  "\brename\b":"rename",
                  "rename *=":"rename=",
                  "year\(":"sum(",
                  "month\(":"month(",
                  "proc *freq *data *=":"proc freq data=",
                  ,"\btables\b":"tables",
                  "\bnoprint\b":"noprint",
                  "\bretrain\b":"noprint",
                  "mdy\(":"mdy(",
                  "\btable\b":"table",
                  "proc *sql":"proc sql",
                  "\bselect\b":"select",
                  "\bfrom\b":"from",
                  "\bods\b":"ods",
                  "\brtf\b":"rtf",
                  "proc sgplot",
                  "xaxis",
                  "yaxis",
                  "close",
                  "proc logistic",
                  "odsfile",
                  "filename",
                  "file ",
                  "dm ",
                  "dim(",
                  "first.",
                  "last.",
                  "_n_",
                  "proc transpose",
                  "descending",
                  "max(",
                  "min(",
                  "round(",
                  "rannor(",
                  "rand(",
                  "ranks ",
                  "proc template ",
                  "proc contents ","informat ",
                  "length ",
                  "length(",
                  "varnum ",
                  "proc tabulate",
                  "proc report",
                  "column ",
                  "define ",
                  "\bput\b":"put",
                  "\botherwise\b":"otherwise",
                  "%include":"%include",
                  "%let":"%let",
                  "%macro +":"%macro ",
                  "%mend +":"%mend ",
                  "%if +":"%if ",
                  "%then +":"%then ",
                  "%else +":"%else ",
                  "%do *;":"%do;",
                  "%end *;":"%end;",
                  "%put +":"%put ",
                  "%goto +":"%goto",
                  "%scan":"%scan",
                  "%str":"%str",
                  "intnx":"intnx",
                  "compress\(":"compress(",
                  "scan\(":"scan(",
                  "index\(":"index(",
                  "substr\(":"substr(",
                  "fileexist":"fileexist",
                  "_temporary_":"_temporary_",
                  "\bpad\b":"pad",
                  "\btruncover\b":"truncover",
                  "\bdsd\b":"dsd",
                  "\bmissover\b":"missover",
                  "\bwhile\b":"while",
                  }

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

file = "C:/Users/User/Google Drive/1_Exchange/Compliance/MyCompCheck/CreateTimeWindow_H.sas"
toUppercase(file,SASKeyWords)
