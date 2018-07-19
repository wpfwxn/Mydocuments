FILENAME logf 'H:/My Documents/log.txt';

/* PRXPARSE("/^\d+ +$/"); */

%INCLUDE "\\mike.eortc.be\hazobou$\My Documents\MyFiles\Useful\SAS_Docs\Data.sas";

DATA Log CleanLog sascode(KEEP=Num Line);
	RETAIN Nun 0;
	INFILE logf TRUNCOVER;
	INPUT @1 Line $500. @;
	OUTPUT Log;
	Pos_no= PRXMATCH("/^\d+$/",COMPRESS(Line));
	Pos_num = PRXMATCH("/^\d+/",COMPRESS(Line)); /* The compressed line contains only numbers */
	IF (Pos_no OR Pos_num) THEN DO;
		Num = Num + 1;
		OUTPUT sascode;
	END;
	Pos_blank = PRXMATCH("/^$/",COMPRESS(Line)); /* The compressed line contains nothing */
	Pos_note = PRXMATCH("/^NOTE:|cputime|realtime/",COMPRESS(Line)); /* The line Starts with NOTE: or real time or cpu time*/
	RETAIN N_er 0;
	Pos_error = PRXMATCH("/^ERROR/",COMPRESS(Line));
	IF (Pos_error > 0) THEN N_er = N_er + 1;
	IF NOT (Pos_no OR Pos_blank OR Pos_note) THEN OUTPUT CleanLog;
RUN;
