PROC PRINTTO; RUN;
DM 'LOG; CLEAR;';
PROC SCAPROC;
	RECORD 'H:/My Documents/scaproc.txt';
RUN;

OPTIONS MPRINT MEXECNOTE;

%MACRO Sort(Data=,ByVars=,out=);
	%IF ( &Data= ) %THEN %DO;
		%PUT ERROR: The Data set is required;
		%GOTO EXIT;
	%END;
	%IF ( &ByVArs= ) %THEN %DO;
		%PUT ERROR: The ByVar parameter is required;
		%GOTO EXIT;
	%END;
	PROC SORT DATA=&Data 
		%IF ( &out ^= ) %THEN OUT=&OUT;;
		BY &ByVars;
	RUN;
%EXIT: %MEND Sort;

DATA Test;
	LENGTH Name City $20;
	INPUT Name $ City $ Age;
	DATALINES;
Hervé Louvain-la-neuve 30
Martinez Brussels 32
James Wavre 26
Mary Barcelona 28
;
RUN;

%Sort(Data=Test); /*,ByVars=Name,Out=TestSorted*/

PROC PRINT DATA=Test MEAN;
	BY Name;
RUN; 
/*
PROC OPTIONS;
RUN;
*/


PROC SCAPROC; 
	WRITE;
RUN;

FILENAME logf 'H:/My Documents/log.txt';
DM 'LOG; FILE logf REPLACE';
