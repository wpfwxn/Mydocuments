
DATA TRT;
	INFILE "C:\Users\hazobou\MyDocuments\1-SAS\1-SAS_Code\ProcSGPLOT\timetoresponse.csv" DSD FIRSTOBS=2;
	Subjn = _N_;
	INPUT crstart1 crstart2 prstart1 prstart2 end1 end2 line_end symbol $ dstage adrfl $;
	LABEL dstage="Disease Stage";
RUN;

PROC SORT DATA=TRT;
	BY DESCENDING Line_end;
RUN;

PROC SGPLOT DATA=TRT;
	HBARPARM CATEGORY=Subjn RESPONSE=Line_end / GROUP=dstage BARWIDTH=.5;
	YAXIS TYPE=DISCRETE DISPLAY=(NOVALUES NOTICKS) LABEL="Subjects who received drug";
	XAXIS TYPE=LINEAR LABEL="Months" VALUES=(-1 TO 20 BY 1);
RUN;

DATA Final1;
	SET TRT;
	Subjn = _N_;
	IF 

