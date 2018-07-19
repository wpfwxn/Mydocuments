
DATA Test1;
	INPUT Color $ @@;
	DATALINES;
Blue Yellow Brow Black
;
RUN;

PROC PRINT DATA=Test1;
RUN;
