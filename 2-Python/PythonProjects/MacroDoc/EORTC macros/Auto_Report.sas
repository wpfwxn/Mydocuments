/******************************************************************************************************
* PROGRAM NAME.....:  Auto_report.sas                                 						  
*                                                                             
* FUNCTION.........:  Performs automatic reporting (from SAS output to word)         											  
* DATE.............:  22/11/2017	                                                      
* AUTHOR...........:  Gaelle Isaac                                                        
* REVISED :                                                                   
*                                                                             
* INPUT............: Output from SAS (rtf, png, pdf, ...) + Excel file with output_path and bookmarks                                           
* OUTPUT...........: 										                  
* REMARKS..........:                                                          
*                                                                             
******************************************************************************************************/


/*import excel file*/
proc import datafile="&location\&excelfile."
		    out=test1
			dbms=xlsx replace;
			getnames=yes;
			
run;

*Extract picture or image;
Data test2;
set test1;
if INDEX(output_path,'.rtf')=0 ;
no_ext=SUBSTR(output_path,1,(length(output_path)-4));
run;

*Convert all images to rtf file to include them in the report*;
data _NULL_;
set test2;
call symput (COMPRESS("adress"||_N_),TRIM(LEFT(no_ext)));
call symput ("i",trim(left(_N_)));
call symput (COMPRESS("original"||_N_),output_path);
run;
%put &adress1 with;
%put &i;

%macro convert ;
	%do a=1 %to &i.;
		option nodate nonumber;
		ods rtf file = "&&adress&a...rtf" nogtitle nogfoot;
			ods escapechar='~';

			/* Import the image and output into the RTF */
			ods text='~S={preimage="&&original&a"}';
	%end;
		ods rtf close;
%mend;
%convert;

*Change the extension of the file in the original excel file;
data test1;
set test1;
if INDEX(output_path,'.rtf')=0  then do;
	substr(output_path,length(output_path)-2,3)="rtf";
end;
run;


data _NULL_;
set test1 end=eof;
file "&location.\&OutputReport..vbs" lrecl=2048;
*Initialization – Open the template CSR file;
if _N_=1 then do;
put "%str(Set objword = CreateObject%()" @ ;
put '"Word.Application"' @; put "%str(%))";
put "%str(objword.Visible = True)";
put "%str(strComputer = %".%")";
put "%str(Set objDoc = objWord.Documents.Open(%"&location.\&TemplateReport.%"))";
put "%str(Set objSelection = objWord.Selection)";
end;
*Insertion of rtf files using bookmark; 
put "%str(return=objSelection.GoTo%(-1,,,%")" bookmark "%str(%"%))"; 
put "%str(returnsel=objSelection.InsertFile%(%")" output_path "%str(%")" "%str(,%"%"%", False, True, False%))";

*Save the document and quit MS Word; 
if eof=1 then do;
put "%bquote(objDoc.SaveAs("&location.\&OutputReport..docx"))"; 
put "objword.Quit";
end;
run;

X "%str(cscript.exe %"&location.\&OutputReport..vbs %")";



