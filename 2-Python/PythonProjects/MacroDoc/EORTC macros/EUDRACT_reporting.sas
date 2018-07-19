

/******************************************************************************
* PROGRAM NAME.....:   EUDRACT_reporting.sas                                						  
*                      This macro is called by %AE_Table2                                                       
* FUNCTION.........:   Prepare an excel file that summarizes the adverse events for EUDRACT reporting           											  
* DATE.............:   Nov 2017	                                                      
* AUTHOR...........:   Sandra Collette                                                        
* REVISED :            Gaëlle ISAAC                                                       
*                                                                             
* INPUT............: VISTA DATABASE                                           
* OUTPUT...........: Excel file 
* REMARKS..........:                                                          
*                                                                             
******************************************************************************/

/*JAN 2018, NOTE from GIS:
In order to use the macro %EUDRACT_reporting (called by the %AE_table2 some additional variables 
should be added in the patient dataset AND in the adverse event dataset (AE_for_eudract).

The variable corresponding to the &POPVAR. parameter should be added in BOTH datasets (&dataPatient and &Data_AE)
The variable corresponding to the &SS. parameter can be in the &dataPatient only
The variable corresponding to the &PERIODVAR parameter can be in the &Data_AE only
The variable corresponding to the &TRT parameter should be in BOTH datasets (&dataPatient and &Data_AE).
The treatment value must be >0 (trt=1 and trt=2 and NOT trt=0 and trt=1)
The variable SOC_code should be in the excel file provided by the PVU. Otherwise create it.
*/

%macro eudract; 

/*------------------------------------------------------------------------
  STEP 0 :  FOOL PROOF TESTING OF ALL PARAMETERS
  --------------------------------------------------------------------------*/

%include "C:\SAS\Extra EORTC Macros\validator_v2.sas";

**popvar on datapatient and data_ae**;
%validator (type=VAR, parm=&popvar, parm_name=POPVAR, acceptable=, data_validator=&data_AE, var=&popvar, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
%validator (type=VAR, parm=&popvar, parm_name=POPVAR, acceptable=, data_validator=&dataPatient, var=&popvar, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**periodvar in data_ae**;
%validator (type=VAR, parm=&periodvar, parm_name=PERIODVAR, acceptable=, data_validator=&data_AE, var=&periodvar, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**trt in both datasets**;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=, data_validator=&data_AE, var=&trt, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=, data_validator=&dataPatient, var=&trt, required=, YN=N);
	%if &_STOPMACRO=Y %then %goto stop_exec;
**format for trt**;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=, data_validator=&data_AE, var=&trt, required=, YN=N, format=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;
%validator (type=VAR, parm=&trt, parm_name=TRT, acceptable=, data_validator=&dataPatient, var=&trt, required=, YN=N, format=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;

**SS must be present in datapatient*;
%validator (type=VAR, parm=&ss, parm_name=SS, acceptable=, data_validator=&dataPatient, var=&ss, required=, YN=N, format=Y);
	%if &_STOPMACRO=Y %then %goto stop_exec;


/*-------------------------------------------
  STEP 1: DATA PREPARATION STEP
--------------------------------------------*/


/*Import data from PVU
-----------------------*/

	PROC IMPORT OUT=__data_sae 
	            DATAFILE="&PVUFile."
	            DBMS=  XLSX   REPLACE;
	RUN;

		
	*Rename seqid=patid*;
	data __data_sae;
	set __data_sae;
	PATID=SeqId*1;
	SOC=UPCASE(SOC);
	PT=upcase(PT);
	SourceTermId=SOC_code*1;
	if UPCASE(SOC)="PRODUCT ISSUES" and UPCASE(PT)="DEVICE OCCLUSION" then do;
	SOC="INFECTIONS AND INFESTATIONS";
	SourceTermId=10021881;
	end;
	put "SAE corresponding to SOC='PRODUCT ISSUES' and PT='DEVICE OCCLUSION' was classified in the SOC='INFECTIONS AND INFESTATIONS' with SourceTermId=10021881.
	     Please check that it is still correct";
	run;

/*Other AE
-----------*/
/* Import from C:\SAS in case of laptop and on the K:\ in case of fixed computer*/

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\CTCAEv&CTC..xls))) > 0 %then %do;
	**Import CTCAE dictionnary**;
	PROC IMPORT OUT=CTCAE_EUDRACT 
	            DATAFILE="K:\SAS\EORTC macros\CTCAEv&CTC..xls"
	            DBMS=  EXCELCS   REPLACE;
	            SHEET='CTCAE'; 
	RUN;

	libname MEDDRA "K:\SAS\EORTC macros";

%end;

%if %sysfunc (fileexist(%STR(K:\SAS\EORTC macros\CTCAEv&CTC..xls))) = 0 %then %do;
	**Import CTCAE dictionnary**;
	PROC IMPORT OUT=CTCAE_EUDRACT 
	            DATAFILE="C:\SAS\EORTC macros\CTCAEv&CTC..xls"
	            DBMS=  EXCELCS   REPLACE;
	            SHEET='CTCAE'; 
	RUN;

	libname MEDDRA "C:\SAS\EORTC macros";

%end;


	*Import dictionnary of CTC*;
	data CTCAE_EUDRACT (keep=SOC2 PT SOC_short);
	length SOC2 $90.;
	set CTCAE_EUDRACT;
	SOC2=upcase(SOC_Name);
	PT=upcase(Term);
	run;
	
	proc sort data=CTCAE_EUDRACT out=allsoc(keep=SOC2 SOC_short) nodupkey;
	by SOC_short;
	run;


	proc sort data=ALLSOC out=list_SOC;by SOC_short; run;

	
	**Create patient**;
	%sort(data=&DataPatient. ,var=patid) ;
	%sort(data=__formAEmerge,var=patid) ;

	data __patient_EUDRACT;
	merge &DataPatient. (in=a) __formAEmerge(in=b);
	by patid;
	if a;
	AE=1;
	if a and not b then AE=0;
	run;

	proc sort data=__patient_EUDRACT (keep=patid &trt. &ss. &popvar. AEMODIFY SOC AETOXGR AE SOC_short)   ; by patid;run;

	**Recode SOC into categories accepted by eudract**;
	data __patient_EUDRACT;
	set __patient_EUDRACT;
	PT=UPCASE(AEMODIFY);
	run;

	%sort(data=__patient_EUDRACT,var=SOC_Short) ;
	%sort(data=list_SOC,var=SOC_Short) ;

	data Form_ae_final_EUDRACT;
	merge __patient_EUDRACT list_SOC; 
	by SOC_short;
	PT=UPCASE(AEMODIFY);
	grade_=AETOXGR;
	GPcode=&trt.;
	run;

	proc sort data=Form_ae_final_EUDRACT; by patid;run;

	
**Recode with CTCversion 4.0 with help of Carmela Caballero**;
**Select all ae except ae at baseline**;
data Form_ae_final_EUDRACT;
length SOC $90.;
set Form_ae_final_EUDRACT;
if SOC2="BLOOD/BONE MARROW" or SOC2="LYMPHATICS" or SOC2="COAGULATION" then SOC="BLOOD AND LYMPHATIC SYSTEM DISORDERS";
if SOC2="CARDIAC ARRHYTHMIA" or SOC2="CARDIAC GENERAL" or SOC_short="CAR OTH" then SOC="CARDIAC DISORDERS";
if SOC2="AUDITORY/EAR" then SOC="EAR AND LABYRINTH DISORDERS";
if SOC2="ENDOCRINE" then SOC="ENDOCRINE DISORDERS";
if SOC2="GROWTH AND DEVELOPMENT" and INDEX(UPCASE(PT), 'BONE')=0 then SOC="ENDOCRINE DISORDERS";
if SOC2="OCULAR/VISUAL" then SOC="EYE DISORDERS";
if SOC2="GASTROINTESTINAL" then SOC="GASTROINTESTINAL DISORDERS";
if SOC2="CONSTITUTIONAL SYMPTOMS" or SOC2="SYNDROMES" or SOC2="SECONDARY MALIGNANCY" or SOC2="DEATH" 
	then SOC="GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS";
if SOC2="HEPATOBILIARY/PANCREAS" or INDEX(UPCASE(PT), "HEPAT")>0 then SOC="HEPATOBILIARY DISORDERS";
if SOC2="ALLERGY/IMMUNOLOGY" then SOC="IMMUNE SYSTEM DISORDERS";
if SOC2="INFECTION" then SOC="INFECTIONS AND INFESTATIONS";
if SOC2="METABOLIC/LABORATORY" then SOC="METABOLISM AND NUTRITION DISORDERS";
if SOC2="MUSCULOSKELETAL/SOFT TISSUE" then SOC="MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS";
if SOC2="GROWTH AND DEVELOPMENT" and INDEX(UPCASE(PT), 'BONE')>0 then SOC="MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS";
if SOC2="NEUROLOGY" then SOC="NERVOUS SYSTEM DISORDERS";
if SOC2="RENAL/GENITOURINARY" then SOC="RENAL AND URINARY DISORDERS";
if SOC2="SEXUAL/REPRODUCTIVE FUNCTION"	then SOC="REPRODUCTIVE SYSTEM AND BREAST DISORDERS";
if SOC2="PULMONARY/UPPER RESPIRATORY" then SOC="RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS";
if SOC2="DERMATOLOGY/SKIN" then SOC="SKIN AND SUBCUTANEOUS TISSUE DISORDERS";
if SOC2="SURGERY/INTRA-OPERATIVE INJURY" then SOC="SURGICAL AND MEDICAL PROCEDURES";
if SOC2="VASCULAR" then SOC="VASCULAR DISORDERS";

*Recode manually all the pain and HEM*;
if SOC2="PAIN" then do;
	if TRIM(LEFT(PT))="GASTROINTESTINAL PAIN" or "ABDOMEN" or PT="ABDOMINAL" or PT="HEMORRHOIDS" or PT="INTESTINE" or PT="PERISTOMAL" or 
	   PT="PHARYNX" or PT="STOMACH" or PT="DENTAL/TEETH/PERIDONTAL" or PT="RECTUM" or PT="PROCTITIS" or PT="PERITONEUM"
		then SOC="GASTROINTESTINAL DISORDERS";
	if PT="AGITATION" or PT="PAIN NOS" or PT="OTHER ADVERSE EVENT" or PT="PELVIS" or PT="PELVIC" or PT="RADIATION" or PT="TUMOR PAIN" 
		or PT="ABDOMEN NOS" then SOC="GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS";
	if PT="BLADDER" or PT="CYSTITIS" or PT="KIDNEY" then SOC="RENAL AND URINARY DISORDERS";
	if PT="BUTTOCK" or PT="EXTREMITY-LIMB" or PT="MUSCLE" or PT="TENDON" 
		then SOC="MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS";
	if PT="CHEST/THORAX NOS" or PT="THORAX" then SOC="RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS";
	if PT="NEURALGIA/PERIPHERAL NERVE" then SOC="NERVOUS SYSTEM DISORDERS";
	if PT="HYPERTENSION" then SOC="VASCULAR DISORDERS";
	if PT="SCROTUM" or PT="TESTICLE" or PT="TESTIS" or PT="VAGINA" or PT="PERINEUM" or PT="URETHRA"
		then SOC="REPRODUCTIVE SYSTEM AND BREAST DISORDERS";
end;
if SOC2="HEMORRHAGE/BLEEDING" then do;
	if INDEX(PT, "ABDOMEN")>0 or INDEX(PT, "ANUS")>0 or INDEX(PT, "LOWER GI")>0 or INDEX(PT, "PERITONEAL CAVITY")>0 or 
       INDEX(PT, "RECTUM")>0 or INDEX(PT, "STOMA")>0 or INDEX(PT, "RECTAL")>0 or INDEX(PT, "MUCOSA")>0 or INDEX(PT, "ULCERATION")>0 
		then SOC="GASTROINTESTINAL DISORDERS";
	if PT="HEMATOMA"  or PT="OTHER ADVERSE EVENT" then SOC="BLOOD AND LYMPHATIC SYSTEM DISORDERS";
	if INDEX(PT, "URINARY")>0 then SOC="RENAL AND URINARY DISORDERS";
	if PT="PERINEUM" or PT="URETHRA" then SOC="REPRODUCTIVE SYSTEM AND BREAST DISORDERS";
end;
if SOC2="" then do;
	if PT="PROCTITIS" then SOC="GASTROINTESTINAL DISORDERS";
	if PT="URETER" then SOC="REPRODUCTIVE SYSTEM AND BREAST DISORDERS";
end;
*Some important preferred terms (anorexia, dehydration) were classified as GAS in the CTCv3 
 and are now classified as METABOLIC in CTCv4 => recode them to be as correct as possible*;
IF INDEX(PT,"ANOREXIA")>0 or INDEX(PT, "DEHYDRATION") then SOC="METABOLISM AND NUTRITION DISORDERS";
if INDEX(PT, "WEIGHT LOSS")>0 then SOC="INVESTIGATIONS";

**All those categories do not have an equivalent**;
*if SOC2="" then	SOC="CONGENITAL, FAMILIAL AND GENETIC DISORDERS";
*if SOC2="" then	SOC="SOCIAL CIRCUMSTANCES";
*if SOC2="" then	SOC="PREGNANCY, PUERPERIUM AND PERINATAL CONDITIONS";
*if SOC2="" then	SOC="PSYCHIATRIC DISORDERS";
*if SOC2=""	then SOC="NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS)";
*if SOC2="" then SOC=INJURY, POISONING AND PROCEDURAL COMPLICATIONS;

if SOC="" then SOC="OTHER ADVERSE EVENT";
if PT="" then PT=TRIM(LEFT("OTHER" ||" "||SOC));
run;


/*-------------------------------------------
  STEP 2: CREATION OF EXCEL FILE
--------------------------------------------*/

data meddra_identifiers;
set MEDDRA.meddra_identifiers;
run;

/* identification number of treatment group */
	proc sql;
	create table trtgroup as
	select count(distinct &trt.) as trtgp
	from &DataPatient.
	where &trt.>.U;
	quit;
	data _null_;
	set trtgroup;
	call symput('nbgroup',compress(trtgp));
	run;

/* selection of patients' information */
	%sort(data= &DataPatient.,var=patid) ;
	data ___patient;
	set &DataPatient.;
	keep patid &trt. &popvar. &ss.;
	run;

/* creation of AdverseEvents dataset */
	%sort(data= Form_ae_final_EUDRACT,var=patid) ;

	data Form_ae_final_EUDRACT;
	set Form_ae_final_EUDRACT;
	by patid;
	*if &grade_ae.>=1 then AE=1; * else AE=0;
	if AEdesc="" then AEdesc="";
	SOC=upcase(SOC);
	run;

/* creation of SeriousAdverseEvents dataset */
	%sort(data= __data_sae,var=patid) ;

	data SeriousAdverseEvents;
	length SOC $90.;
	merge __data_sae &DataPatient.;
	by patid;
	if AER_Number^="" then SAE=1; else SAE=0;
	if AEdesc="" then AEdesc="";
	SOC=upcase(SOC);
	run;


/* SOC recoding (SOC identifiers) */

	%sort(data=MEDDRA_IDENTIFIERS,var=SOC) ;
	*%sort(data= Form_ae_final_EUDRACT,var=SOC) ;
	proc sort data=Form_ae_final_EUDRACT out=AdverseEvents (where=(AE=1)); by SOC; run;

	data AdverseEvents;
	length SOC $90.;
	merge AdverseEvents (in=a) MEDDRA_IDENTIFIERS;
	by SOC;
	if a;
	run;

	proc sort data=SeriousAdverseEvents (where=(SAE=1)); by SOC; run;
	data SeriousAdverseEvents;
	length SOC $90.;
	merge SeriousAdverseEvents (in=a) MEDDRA_IDENTIFIERS;
	by SOC;
	if a;
	run;



/* Subjects exposed per group*/
	proc sql;
	create table gpexpo as
	select &trt. as GPcode, count(distinct patid) as GPexpo
	from &DataPatient.
	where &popvar.=&popval.
	group by &trt.;
	quit;


/* Subjects affected by SAE per group*/
	proc sql;
	create table GPaffSAE as
	select &trt. as GPcode, count(distinct patid) as GPaffSAE
	from SeriousAdverseEvents
	where sae=1 and &popvar.=&popval.
	group by &trt.;
	quit;

/* Subjects affected by non-SAE (or in our case “all AEs”) per group*/
	proc sql;
	create table GPaffnSAE as
	select &trt. as GPcode, count(distinct patid) as GPaffnSAE
	from AdverseEvents
	where AE=1 and &popvar.=&popval.
	group by &trt.;
	quit;

/* Total number of deaths (all causes) per group*/
	proc sql;
	create table GPdeath as
	select &trt. as GPcode, count(distinct patid) as GPdeath
	from &DataPatient.
	where &ss.=1 and &popvar.=&popval.
	group by &trt.;
	quit;

/* Total number of deaths resulting from SAE per group*/
	proc sql;
	create table GPdeathSAE as
	select &trt. as GPcode, count(distinct patid) as GPdeathSAE
	from SeriousAdverseEvents
	where sae=1 and &popvar.=&popval.
	and Outcome="Fatal" and (Relationship="Not assessable" or Relationship="Likely" or Relationship="Related") 
	group by &trt.;
	quit;


%sort(data=gpexpo,var=gpcode) ;
%sort(data=GPaffSAE,var=gpcode) ;
%sort(data=GPaffnSAE,var=gpcode) ;
%sort(data=GPdeath,var=gpcode) ;
%sort(data=GPdeathSAE,var=gpcode) ;

data reportingGP;
merge gpexpo GPaffSAE GPaffnSAE GPdeath GPdeathSAE;
by gpcode;
	if gpexpo<=.U then gpexpo=0;
	if GPaffSAE<=.U then GPaffSAE=0;
	if GPaffnSAE<=.U then GPaffnSAE=0;
	if GPdeath<=.U then GPdeath=0;
	if GPdeathSAE<=.U then GPdeathSAE=0;
	if gpcode<=.U then delete;
run;


/***** SAE item *****/


/* All the different preferred term observed by group */
proc sql;
create table allsocptsae as
select soc, SourceTermId, pt, AEdesc, count(distinct patid) as nb
from SeriousAdverseEvents
where sae=1 and &popvar.=&popval.
group by soc, SourceTermId, pt, AEdesc ;
quit;

/* Subjects affected number per group*/
proc sql;
create table subaffsae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(distinct patid) as subaff
from SeriousAdverseEvents
where sae=1 and &popvar.=&popval.
group by &trt., soc, SourceTermId, pt, AEdesc;
quit;

/* Occurrences number per group*/
proc sql;
create table alloccsae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(distinct AER_Number) as allocc
from SeriousAdverseEvents
where sae=1 and &popvar.=&popval.
group by &trt., soc, SourceTermId, pt, AEdesc;
quit;

/* Occurrences causally related to treatment number per group*/
proc sql;
create table allreloccsae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(distinct AER_Number) as allrelocc
from SeriousAdverseEvents
where sae=1 and &popvar.=&popval.
and (Relationship="Not assessable" or Relationship="Likely" or Relationship="Related") 
group by &trt., soc, SourceTermId, pt, AEdesc;
quit;

/* Fatalities number per group*/
proc sql;
create table fatalitiessae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(distinct patid) as fatalities
from SeriousAdverseEvents
where sae=1 and &popvar.=&popval. and Outcome="Fatal" 
group by &trt., soc, SourceTermId, pt, AEdesc;
quit;

/* Fatalities causally related to treatment number per group*/
proc sql;
create table relfatalitiessae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(distinct patid) as relfatalities
from SeriousAdverseEvents
where sae=1 and &popvar.=&popval.
and Outcome="Fatal" and (Relationship="Not assessable" or Relationship="Likely" or Relationship="Related") 
group by &trt., soc, SourceTermId, pt, AEdesc;
quit;

%sort(data=subaffsae,var=gpcode soc pt AEdesc) ;
%sort(data=alloccsae,var=gpcode soc pt AEdesc) ;
%sort(data=allreloccsae,var=gpcode soc pt AEdesc) ;
%sort(data=fatalitiessae,var=gpcode soc pt AEdesc) ;
%sort(data=relfatalitiessae,var=gpcode soc pt AEdesc) ;

data eudractsae;
merge subaffsae alloccsae allreloccsae fatalitiessae relfatalitiessae;
by gpcode soc pt AEdesc;
run;

data allptsaegp;
set allsocptsae;
	do i=1 to &nbgroup.;
		gpcode=i;
		output;
	end;
drop nb i;
run;

%sort(data=allptsaegp,var=soc pt AEdesc gpcode) ;
%sort(data=eudractsae,var=soc pt AEdesc gpcode) ;

data eudractsae;
merge eudractsae allptsaegp;
by soc pt AEdesc gpcode;
if subaff<=.U then subaff=0;
if allocc<=.U then allocc=0;
if allrelocc<=.U then allrelocc=0;
if fatalities<=.U then fatalities=0;
if relfatalities<=.U then relfatalities=0;
run;

%sort(data=eudractsae,var=gpcode soc pt AEdesc) ;
%sort(data=gpexpo,var=gpcode) ;

data eudractsae;
retain GPcode SOC SourceTermId PT AEdesc subaff subexpo allocc allrelocc fatalities relfatalities;
merge eudractsae gpexpo(rename=(gpexpo=subexpo));
by gpcode;
if subexpo<=.U then subexpo=0;
if gpcode<=.U then delete;
run;



/*****  All AE item *****/

/* All the different preferred term observed by group */
proc sql;
create table allsocptae as
select soc, SourceTermId, pt, AEdesc, count(distinct patid) as nb
from AdverseEvents
where ae=1 and &popvar.=&popval.
group by soc, SourceTermId, pt, AEdesc;
quit;

/* Subjects affected number per group*/
proc sql;
create table subaffae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(distinct patid) as subaff
from AdverseEvents
where ae=1 and &popvar.=&popval.
group by  &trt., soc, SourceTermId, pt, AEdesc;
quit;

/* Occurrences number per group*/
proc sql;
create table alloccae as
select &trt. as gpcode, soc, SourceTermId, pt, AEdesc, count(ae) as allocc
from AdverseEvents
where ae=1 and &popvar.=&popval.
group by  &trt., soc, SourceTermId, pt, AEdesc;
quit;

%sort(data=subaffae,var=gpcode soc pt AEdesc) ;
%sort(data=alloccae,var=gpcode soc pt AEdesc) ;

data eudractae;
merge subaffae alloccae ;
by gpcode soc pt AEdesc;
run;


data allptaegp;
set allsocptae;
	do i=1 to &nbgroup.;
		gpcode=i;
		output;
	end;
drop nb i;
run;

%sort(data=allptaegp,var=soc pt AEdesc gpcode) ;
%sort(data=eudractae,var=soc pt AEdesc gpcode) ;

data eudractae;
merge eudractae allptaegp ;
by soc pt AEdesc gpcode;
if subaff<=.U then subaff=0;
if allocc<=.U then allocc=0;
run;


%sort(data=eudractae,var=gpcode soc pt) ;
%sort(data=gpexpo,var=gpcode) ;
data eudractother;
merge eudractae gpexpo(rename=(gpexpo=subexpo));
retain GPcode SOC SourceTermId PT AEdesc subaff subexpo allocc;
by gpcode;
if SourceTermId<=.U then output;
run;
proc print data=eudractother;
run;

data eudractae;
retain GPcode SOC SourceTermId PT AEdesc subaff subexpo allocc;
merge eudractae gpexpo(rename=(gpexpo=subexpo));
by gpcode;
if subexpo<=.U then subexpo=0;
if gpcode<=.U then delete;
if SourceTermId<=.U then delete;
run;


/***** export in XLSX file *******/
%sort(data=reportingGP,var=gpcode) ;
PROC EXPORT OUTFILE="&outputEUDRACT." 
            DATA=reportingGP DBMS=XLSX REPLACE; 
			SHEET="GP" ; 
RUN;
%sort(data=eudractsae,var=soc pt gpcode) ;
PROC EXPORT OUTFILE="&outputEUDRACT." 
            DATA=eudractsae DBMS=XLSX REPLACE;
            SHEET="SAE";
RUN;
%sort(data=eudractae,var=soc pt gpcode) ;
PROC EXPORT OUTFILE="&outputEUDRACT." 
            DATA=eudractae DBMS=XLSX REPLACE;
            SHEET="AE";
RUN;

%stop_exec: 
run;
quit;

%mend eudract;




