
DATA TRT;
	INFILE "C:\Users\hazobou\MyDocuments\1-SAS\1-SAS_Code\ProcSGPLOT\timetoresponse.csv" DSD FIRSTOBS=2;
	*INFILE "/folders/myshortcuts/MyWorks/Mydocuments/1-SAS/1-SAS_Code/ProcSGPLOT/timetoresponse.csv" DSD FIRSTOBS=2;
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

PROC FORMAT;
	VALUE Stage 1="Stage 1" 2="Stage 2" 3="Stage 3" 4="Stage 4";
RUN;

DATA Final1;
	SET TRT;
	Subjn = _N_;
	IF adrfl = 'Y' THEN Durresp = -.25;
	FORMAT Dstage Stage.;
RUN;

/* Create annotate dataset */

DATA Anno1;
	LENGTH Function LineColor $25;
	RETAIN Drawspace "datavalue" LineColor "black" LineThickness 3;
	SET Final1(KEEP=Subjn Line_end symbol WHERE=(symbol="a"));
	Function = "ARROW";
	X1 = Line_end+0.05;
	X2 = Line_end+0.8;
	Y1 = Subjn;
	Y2 = Subjn;
	Shape = "FILLED";
RUN;

/* Customized legend with symbol and text */
DATA Anno2;
	LENGTH Label $100 Function Fillcolor LineColor $25;
	RETAIN Drawspace "DATAVALUE" LineTickness 3 Display "ALL" WidthUnit "data"
		   HeightUnit "data" Anchor "left" ;
	/* Draw red triangle indicating complete response start time */
	Function="POLYGON"; X1=15.20; Y1=14.10; LineColor='red'; FillColor='red'; OUTPUT;
	Function="POLYCONT"; X1=15.30; Y1=13.90; LineColor='red'; OUTPUT;
	Function="POLYCONT"; X1=15.10; Y1=13.90; LineColor='red'; OUTPUT;
	/*Function="POLYCONT"; X1=15.2; Y1=4.1; LineColor='red'; OUTPUT;/*
	
	/* Draw red triangle indicating complete response start time */
	Function="POLYGON"; X1=15.20;  Y1=13.10; LineColor='blue'; FillColor='blue'; OUTPUT;
	Function="POLYCONT"; X1=15.30; Y1=12.90; LineColor='blue'; OUTPUT;
	Function="POLYCONT"; X1=15.10; Y1=12.90; LineColor='blue'; OUTPUT;
	/*Function="POLYCONT"; X1=15.2; Y1=3.1; LineColor='blue'; OUTPUT;/*
	
	/* Draw the circle indicatinf the response and time using the OVAL function */
	Function="OVAL"; X1=15.125; Y1=8; LineColor='black'; FillColor="black"; Height=0.25;
		Width=0.2; OUTPUT;
	
	/* Draw arrow indicating continuing response */
	Function="ARROW"; X1=15; X2=15.8; Y1=9; Y2=9; LineColor='black'; Shape='FILLED'; OUTPUT;
	
	/* Draw a square indicating durable responder */
	Function='RECTANGLE'; X1=15.125; Y1=10; LineColor='black'; FillColor='black';
			Height=.25; width=.2;	OUTPUT;
	
	/* Create text labels for the legend */
	Function='TEXT'; X1=16; Y1=6; Label="Complete response start"; width=7; Shape=""; FillColor=''; OUTPUT;
	Function='TEXT'; X1=16; Y1=7; Label="Partial response start"; width=7; Shape=""; FillColor=''; OUTPUT;
	Function='TEXT'; X1=16; Y1=8; Label="Response episode end"; width=7; Shape=""; FillColor=''; OUTPUT;
	Function='TEXT'; X1=16; Y1=9; Label="Continued response"; width=7; Shape=""; FillColor=''; OUTPUT;
	Function='TEXT'; X1=16; Y1=10; Label="Durable responder"; width=7; Shape=""; FillColor=''; OUTPUT;
RUN;

/* Set both annotate datasets together (concatenate) to specify in PROC SGPLOT */
DATA Anno;
	SET Anno1 Anno2;
RUN;


/* Attribute map for assigning colors for disease stage in GROUP= option */
DATA Attrmap;
	LENGTH FillColor $25;
	ID='A'; Value=1; FillColor='cx7C95CA'; LineColor='cx7C95CA'; OUTPUT;
	ID='A'; Value=2; FillColor='cxDE7E6F'; LineColor='cxDE7E6F'; OUTPUT;
	ID='A'; Value=3; FillColor='cx66A5A0'; LineColor='cx66A5A0'; OUTPUT;
	ID='A'; Value=4; FillColor='cxA9865B'; LineColor='cxA9865B'; OUTPUT;
RUN;

/* Finally, the swimmer plot */
PROC SGPLOT DATA=Final1 SGANNO=Anno DATTRMAP=Attrmap;
	HBARPARM CATEGORY=Subjn RESPONSE=Line_end / GROUP=Dstage ATTRID=A BARWIDTH=.5;
	YAXIS TYPE=DISCRETE DISPLAY=(NOVALUES NOTICKS) LABEL="Subjects who received drug";
	XAXIS TYPE=LINEAR LABEL="Months" VALUES=(-1 TO 20 BY 1);
	
	SCATTER X=Durresp Y=Subjn /MARKERATTRS=(SYMBOL=SQUAREFILLED SIZE=9 COLOR=BLACK);
	SCATTER X=Crstart1 Y=Subjn /MARKERATTRS=(SYMBOL=TRIANGLEFILLED SIZE=9 COLOR=RED);
	SCATTER X=Crstart2 Y=Subjn /MARKERATTRS=(SYMBOL=TRIANGLEFILLED SIZE=9 COLOR=RED);
	SCATTER X=Prstart1 Y=Subjn /MARKERATTRS=(SYMBOL=TRIANGLEFILLED SIZE=9 COLOR=BLUE);
	SCATTER X=Prstart2 Y=Subjn /MARKERATTRS=(SYMBOL=TRIANGLEFILLED SIZE=9 COLOR=BLUE);
	SCATTER X=End1 Y=Subjn /MARKERATTRS=(SYMBOL=CIRCLEFILLED SIZE=9 COLOR=BLACK);
	SCATTER X=End2 Y=Subjn /MARKERATTRS=(SYMBOL=CIRCLEFILLED SIZE=9 COLOR=BLACK);
RUN;
