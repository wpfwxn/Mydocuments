/************************************************************************************
*
* CLEAN_TABLE.SAS
* ***********
*
* This macro let the user to clean all his RTF tables (TABLES2 tables)
* It removes all "continuous section breaks" between two tables and removes also 
* the "hidden text" in the header of tables {tc Dataset... } instructions.
*
* Version date    : 30 October 2006
* Software        : SAS version 9.3
* Original author : Jérôme RAPION
*
*************************************************************************************
* PARAMETERS
*
*    path      = Path of RTF file to clean (with extension)
*
*************************************************************************************
* MODIFICATIONS
*
* Modified : 20/03/2007  Add of compbl function to supress useless blank space inside a table
*                       JRA
* Modified : 12/04/2010 Revove of compbl function because in version SAS9.2, tabulation are replced by blank spaces.
*                       JRA
*
*************************************************************************************
* EXEMPLE
*
* If you want to clean your file c:\temp\TABLE1.RTF, apply
* %clean_table(path=c:\temp\TABLE1.RTF);
*
************************************************************************************/

%MACRO clean_table(path=);
	filename path "&path.";

	data t;
		infile path LRECL=35000 firstobs=1 TRUNCOVER; 
		input word $ 1-32000;
		if index(word,"sectd")>0 then a=1;
		if index(word,"{\tc") then a=1;
		if a=1 then delete;
		word=tranwrd(word,"%%","%");

		** JRA, 30MAR2010, deletion of this line ;
	*	word=compbl(word);

		drop a;
	run;

	data _null_;
	set t;
	file path;
	put word; 
	run;

   proc datasets library=work nolist;
   delete t;
   quit;

%MEND;

*%clean_table(path=H:\My Documents\EORTC\SAS\STYLES\OUTPUT2\TEST_B9.RTF);
*%clean_table(path=H:\My Documents\EORTC\SAS\STYLES\OUTPUT3\TEST_HEAD1.RTF);
*%clean_table(path=H:\My Documents\EORTC\SAS\STYLES\OUTPUT3\TEST_HEAD2.RTF);
*%clean_table(path=H:\My Documents\EORTC\SAS\STYLES\OUTPUT3\TEST_HEAD3.RTF);
*%clean_table(path=H:\My Documents\EORTC\SAS\STYLES\OUTPUT3\TEST_HEAD4.RTF);
*%clean_table(path=H:\My Documents\EORTC\SAS\STYLES\OUTPUT3\TEST_HEAD5.RTF);

