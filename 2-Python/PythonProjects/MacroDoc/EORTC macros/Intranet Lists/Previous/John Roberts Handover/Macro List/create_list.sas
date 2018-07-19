**** CREATE_LIST.SAS ****;

* John Roberts;
* 21 March 2001;




libname ml 'c:\sas\eortc macros\macro list';
libname library 'c:\sas\eortc macros\macro list';



** !!!!!!!!!! DO NOT RUN AGAIN !!!!!!!!!!!!!!! **;
/* data ml.mac?????rolist;
   attrib macro   length=$32  label='Macro'
          current length=$1   label='Currently used'
          sas6    length=$1   label='Works with SAS 6' 
          sas8    length=$1   label='Works with SAS 8' 
          type    length=$2   label='Category'
          desc_sh length=$45  label='Description'
          desc_ln length=$200 label='Description continued'
          notes   length=$200 label='Notes'
                                     format=$100;
          basedon length=$20  label='Based on'
          origdate            label='Original date'     
                  informat=ddmmyy.   format=ddmmyy10.
          moddate             label='Last modified'     
                  informat=ddmmyy.   format=ddmmyy10.         
          author1 author2 author3 length=$3 label='Author'
          testdate            label='Last test date'     
                  informat=ddmmyy.   format=ddmmyy10.
          testver length=$4   label='Last test SAS version';
run;*/


proc format library=library;
   value $ftype 'U' ='Utility'
                'MA'='Meta-analysis'
                'G' ='Graph'
                'P' ='Program'
                'A' ='Analysis'
                'GM'='G Macro'
                'R' ='Report'
                'QL'='Quality of Life';
   value $fauthor 'LC' ='Laurence Collette'
                  'LB' ='Luc Bijnens'
                  'GHB'='Guido Hoctin Boes'
                  'JR' ='John Roberts'
                  'AI' ='A Ivanov'
                  'DC' ='Desmond Curran'
                  'KVS'='Kristel Van Steen'
                  'BBA'='Benoit Baron';
run;

data ml.macrolist;
   set ml.macrolist;
   format type ftype.
          author1 author2 author3 fauthor.;
run;

proc contents data=ml.macrolist position;
run;



    
