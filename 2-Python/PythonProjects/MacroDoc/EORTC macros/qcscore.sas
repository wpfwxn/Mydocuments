
/***********************************************************/
/*                                                         */
/* SAS program for scoring the EORTC qLq-C30               */
/*     and associated modules.                             */
/*                                                         */
/* Kristel Van Steen and Desmond Curran, 30 November, 1998 */
/*                                                         */
/***********************************************************/

/*Modified : 31 May 2002  To ensure unknown (.U) values are handled correctly. KM.*/


/*********************/
/* Cleaning the Data */
/*********************/

%macro clean(data,version,qp);

proc format;
value item2_ 1='No'
             2='Yes';
value item4_ 1='Not at All'
             2='A Little'
             3='Quite a Bit'
             4='Very Much';
value item7_ 1='Very Poor'
             7='Excellent';

data &data;
        set &data;
        %if (&version=1 or &version=33 or &version=2) %then %do;
           %do I=1 %to 5;
              if (&qp.&i<1 or &qp.&i>2) then &qp.&i=.;
              format &qp.&i item2_.;
           %end;
        %end;
        %if (&version=3) %then %do;
          %do I=1 %to 5;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=.;
            format &qp.&i item4_.;
          %end;
        %end;
        %if (&version=1 or &version=33) %then %do;
          %do I=6 %to 7;
            if (&qp.&i<1 or &qp.&i>2) then &qp.&i=.;
            format &qp.&i item2_.;
          %end;
        %end;
        %if (&version=2 or &version=3) %then %do;
          %do I=6 %to 7;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=.;
            format &qp.&i item4_.;
          %end;
        %end;
        %do I=8 %to 28;
          if (&qp.&i<1 or &qp.&i>4) then &qp.&i=.;
          format &qp.&i item4_.;
        %end;
        %if (&version=1 or &version=2 or &version=3) %then %do;
          %do I=29 %to 30;
            if (&qp.&i<1 or &qp.&i>7) then &qp.&i=.;
            format &qp.&i item7_.;
          %end;
        %end;
        %if (&version=33) %then %do;
          %do I=29 %to 30;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=.;
            format &qp.&i item4_.;
          %end;
        %end;
        %if (&version=33) %then %do;
          %do I=31 %to 33;
            if (&qp.&i<1 or &qp.&i>7) then &qp.i=.;
            format &qp.&i item7_.;
          %end;
        %end;
run;

%mend clean;


/*************************************/
/* Definitions for Score Calculation */
/*************************************/

%macro score(data,type,scale,items);
%let i=1;
  %do %while(%length(%scan(&items,&i)) ^= 0);
      %let i=%eval(&i+1);
  %end;
%let i=%eval(&i-1) ;

data &data;
     set &data;
	   XMEAN = MEAN(OF &items);
       XNUM    = N (of &items);
       %if (%upcase(&type)=F) %then %do ;
         If (XNUM*2 GE &i) THEN &scale = ((1-(XMEAN-1)/3)*100);
       %end ;
       %if (%upcase(&type)=G) %then %do ;
         If (XNUM*2 GE &i) THEN &scale = (((XMEAN-1)/6)*100);
       %end ;
       %if (%upcase(&type)=S) %then %do ;
          %if (&items= hn31 or &items= hn32 or &items= hn33 or &items= hn34 or &items= hn35) %then %do ;
             If (XNUM*2 GE &i) THEN &scale = ((XMEAN-1)*100);
          %end ;
          %else %do ;
             If (XNUM*2 GE &i) THEN &scale = (((XMEAN-1)/3)*100);
          %end ;
       %end ;

       %if (%upcase(&scale)=PF) %then %do ;
         if ((&version=1 or &version=33 or &version=2) and (XNUM*2 GE &i)) THEN &scale = ((1-(XMEAN-1))*100);
       %end;
       %if (%upcase(&scale)=RF) %then %do ;
         if ((&version=1 or &version=33) and (XNUM*2 GE &i)) THEN &scale = ((1-(XMEAN-1))*100);
       %end;

run;

%mend score;


/**************************************************************/
/* Returning a Data Set with Calculated Scores for Each Scale:*/
/* Basic Macro                                                */
/**************************************************************/

%macro qcscore(data,version,qp);

        %clean(&data,&version,&qp) ;

        %score(data=&data , type=f, scale=pf, items= &qp.1 &qp.2 &qp.3 &qp.4 &qp.5);
        %score(data=&data , type=f, scale=rf, items= &qp.6 &qp.7);
        %if (&version=33) %then %do;
          %score(data=&data , type=f, scale=rf2, items= &qp.26 &qp.27);
        %end;
        %score(data=&data , type=f, scale=ef, items= &qp.21 &qp.22 &qp.23 &qp.24);
        %score(data=&data , type=f, scale=cf, items= &qp.20 &qp.25);
        %if (&version=1 or &version=2 or &version=3) %then %do;
          %score(data=&data , type=f, scale=sf, items= &qp.26 &qp.27);
        %end;
        %if (&version=33) %then %do;
          %score(data=&data , type=f, scale=sf, items= &qp.28 &qp.29);
        %end;
        %if (&version=1 or &version=2 or &version=3) %then %do;
          %score(data=&data , type=g, scale=ql, items= &qp.29 &qp.30);
        %end;
        %if (&version=33) %then %do;
          %score(data=&data , type=g, scale=ql, items= &qp.31 &qp.33);
          %score(data=&data , type=g, scale=ql2, items= &qp.32 &qp.33);
        %end;
        %score(data=&data , type=s, scale=fa, items= &qp.10 &qp.12 &qp.18);
        %score(data=&data , type=s, scale=nv, items= &qp.14 &qp.15);
        %score(data=&data , type=s, scale=pa, items= &qp.9 &qp.19);
        %score(data=&data , type=s, scale=dy, items= &qp.8);
        %score(data=&data , type=s, scale=sl, items= &qp.11);
        %score(data=&data , type=s, scale=ap, items= &qp.13);
        %score(data=&data , type=s, scale=co, items= &qp.16);
        %score(data=&data , type=s, scale=di, items= &qp.17);
        %if (&version=1 or &version=2 or &version=3) %then %do;
          %score(data=&data , type=s, scale=fi, items= &qp.28);
        %end;
        %if (&version=33) %then %do;
          %score(data=&data , type=s, scale=fi, items= &qp.30);
        %end;


        Data &data;
        set &data ;
        LABEL &qp.1='Q1: STRENOUS ACTIVITIES';
        LABEL &qp.2='Q2: LONG WALK';
        LABEL &qp.3='Q3: SHORT WALK';
        LABEL &qp.4='Q4: STAY IN BED/CHAIR';
        LABEL &qp.5='Q5: HELPING IN EATING/DRESSING';

        if  &version=1 or &version=33 then do;
          LABEL &qp.6='Q6: LIMITED IN WORK/JOBS';
          LABEL &qp.7='Q7: LIMITED IN HOBBIES/LEISURE';
        end;
        if  &version=2 or &version=3 then do;
          LABEL &qp.6='Q6: LIMITED IN WORK/DAILY ACTIVITIES';
          LABEL &qp.7='Q7: LIMITED IN HOBBIES/LEISURE';
        end;
        LABEL &qp.8='Q8: SHORT OF BREATH';
        LABEL &qp.9='Q9: PAIN';
        LABEL &qp.10='Q10: NEED A REST';
        LABEL &qp.11='Q11: TROUBLE SLEEPING';
        LABEL &qp.12='Q12: FELT WEAK';
        LABEL &qp.13='Q13: LACK OF APPETITE';
        LABEL &qp.14='Q14: NAUSEA';
        LABEL &qp.15='Q15: VOMITING';
        LABEL &qp.16='Q16: CONSTIPATION';
        LABEL &qp.17='Q17: DIARRHEA';
        LABEL &qp.18='Q18: TIRED';
        LABEL &qp.19='Q19: PAIN INTERFERING';
        LABEL &qp.20='Q20: DIFFICULT CONCENTRATING';
        LABEL &qp.21='Q21: FEEL TENSE';
        LABEL &qp.22='Q22: WORRY';
        LABEL &qp.23='Q23: FEEL IRRITABLE';
        LABEL &qp.24='Q24: FEEL DEPRESSED';
        LABEL &qp.25='Q25: DIFFICULT REMEMBERING';
        if (&version=1) then do;
          LABEL &qp.26='Q26: FAMILY LIFE';
          LABEL &qp.27='Q27: SOCIAL ACTIVITIES';
          LABEL &qp.28='Q28: FINANCIAL DIFFICULTIES';
          LABEL &qp.29='Q29: PHYSICAL CONDITION';
          LABEL &qp.30='Q30: QUALITY OF LIFE';
        end;
        if (&version=33) then do;
          LABEL &qp.26='Q26: LIMITED ACTIVITIES';
          LABEL &qp.27='Q27: LIMITED HOBBIES';
          LABEL &qp.28='Q28: FAMILY LIFE';
          LABEL &qp.29='Q29: SOCIAL ACTIVITIES';
          LABEL &qp.30='Q30: FINANCIAL DIFFICULTIES';
          LABEL &qp.31='Q31: PHYSICAL CONDITION';
          LABEL &qp.32='Q32: OVERALL HEALTH';
          LABEL &qp.33='Q33: QUALITY OF LIFE';
        end;
        if (&version=2 or &version=3) then do;
          LABEL &qp.26='Q26: FAMILY LIFE';
          LABEL &qp.27='Q27: SOCIAL LIFE';
          LABEL &qp.28='Q28: FINANCIAL DIFFICULTIES';
          LABEL &qp.29='Q29: OVERALL HEALTH';
          LABEL &qp.30='Q30: QUALITY OF LIFE';
        end;

        LABEL pf='Physical Functioning';
        LABEL rf='Role Functioning';
        LABEL rf2='Role Functioning';
        LABEL ef='Emotional Functioning';
        LABEL cf='Cognitive Functioning';
        LABEL sf='Social Functioning';
        LABEL ql='Global health status / QoL';
        LABEL ql2='Global health status / QoL';
        LABEL fa='Fatigue';
        LABEL nv='Nausea / Vomiting';
        LABEL pa='Pain';
        LABEL dy='Dyspnoea';
        LABEL sl='Insomnia';
        LABEL ap='Appetite loss';
        LABEL co='Constipation';
        LABEL di='Diarhoea';
        LABEL fi='Financial Problems';

        drop xnum xmean ;
        run ;

%mend qcscore;


/**********************/
/* Lung Cancer module */
/**********************/


%macro LCscore(data,qp);

        data &data ;
          set &data ;
          %do i=1 %to 12 ;
             if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
             format &qp.&i item4_. ;
          %end;
        run;

        %score(data=&data,type=S,scale=lcco,items = &qp.1) ;
        %score(data=&data,type=S,scale=lcha,items = &qp.2) ;
        %score(data=&data,type=S,scale=lcsm,items = &qp.6) ;
        %score(data=&data,type=S,scale=lcds,items = &qp.7) ;
        %score(data=&data,type=S,scale=lcpn,items = &qp.8) ;
        %score(data=&data,type=S,scale=lchr,items = &qp.9) ;
        %score(data=&data,type=S,scale=lcpc,items = &qp.10) ;
        %score(data=&data,type=S,scale=lcpa,items = &qp.11) ;
        %score(data=&data,type=S,scale=lcpo,items = &qp.12) ;

        data &data ;
          set &data ;
          if (&qp.3>.Z and &qp.4>.Z and &qp.5>.Z) then do;
              xmean = mean(of &qp.3 &qp.4 &qp.5);
              lcdy= (((xmean-1)/3)*100);
          end;

          *Symptom scales;
          label lcdy= 'LC Dyspnoea' ;
          label lcco= 'LC Coughing' ;
          label lcha= 'LC Haemoptysis' ;
          label lcsm= 'LC Sore Mouth' ;
          label lcds= 'LC Dysphagia' ;
          label lcpn= 'LC Peripheral Neuropathy' ;
          label lchr= 'LC Alopecia' ;
          label lcpc= 'LC Pain in chest' ;
          label lcpa= 'LC Pain in arm or shoulder' ;
          label lcpo= 'LC Pain in other parts' ;

          *Additional variables;
          label &qp.1='LC HOW MUCH DID YOU COUGH' ;
          label &qp.2='LC DID YOU COUGH BLOOD' ;
          label &qp.3='LC SHORT OF BREATH WHEN RESTED' ;
          label &qp.4='LC SHORT OF BREATH WHEN WALKED' ;
          label &qp.5='LC SHORT OF BREATH WHEN CLIMBED' ;
          label &qp.6='LC SORE MOUTH OR TONGUE' ;
          label &qp.7='LC TROUBLE SWALLOWING' ;
          label &qp.8='LC TINGLING HANDS OR FEET' ;
          label &qp.9='LC HAVE YOU HAD HAIR LOSS' ;
          label &qp.10='LC PAIN IN YOUR CHEST' ;
          label &qp.11='LC PAIN IN ARM OR SHOULDER' ;
          label &qp.12='LC PAIN IN OTHER PARTS OF BODY' ;

          drop xnum xmean ;
        run ;

%mend lcscore ;


%macro lcscore30(data,qp);

        data &data ;
          set &data ;
          %do i=31 %to 42 ;
             if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
             format &qp.&i item4_. ;
          %end;
        run;

        %score(data=&data,type=S,scale=lcco,items = &qp.31) ;
        %score(data=&data,type=S,scale=lcha,items = &qp.32) ;
        %score(data=&data,type=S,scale=lcsm,items = &qp.36) ;
        %score(data=&data,type=S,scale=lcds,items = &qp.37) ;
        %score(data=&data,type=S,scale=lcpn,items = &qp.38) ;
        %score(data=&data,type=S,scale=lchr,items = &qp.39) ;
        %score(data=&data,type=S,scale=lcpc,items = &qp.40) ;
        %score(data=&data,type=S,scale=lcpa,items = &qp.41) ;
        %score(data=&data,type=S,scale=lcpo,items = &qp.42) ;

        data &data ;
          set &data ;
          if (&qp.33>.Z and &qp.34>.Z and &qp.35>.Z) then do;
              xmean = mean(of &qp.33 &qp.34 &qp.35);
              lcdy= (((xmean-1)/3)*100);
          end;

          *Symptom scales;
          label lcdy= 'LC Dyspnoea' ;
          label lcco= 'LC Coughing' ;
          label lcha= 'LC Haemoptysis' ;
          label lcsm= 'LC Sore Mouth' ;
          label lcds= 'LC Dysphagia' ;
          label lcpn= 'LC Peripheral Neuropathy' ;
          label lchr= 'LC Alopecia' ;
          label lcpc= 'LC Pain in chest' ;
          label lcpa= 'LC Pain in arm or shoulder' ;
          label lcpo= 'LC Pain in other parts' ;

          *Additional variables;
          label &qp.31='LC HOW MUCH DID YOU COUGH' ;
          label &qp.32='LC DID YOU COUGH BLOOD' ;
          label &qp.33='LC SHORT OF BREATH WHEN RESTED' ;
          label &qp.34='LC SHORT OF BREATH WHEN WALKED' ;
          label &qp.35='LC SHORT OF BREATH WHEN CLIMBED' ;
          label &qp.36='LC SORE MOUTH OR TONGUE' ;
          label &qp.37='LC TROUBLE SWALLOWING' ;
          label &qp.38='LC TINGLING HANDS OR FEET' ;
          label &qp.39='LC HAVE YOU HAD HAIR LOSS' ;
          label &qp.40='LC PAIN IN YOUR CHEST' ;
          label &qp.41='LC PAIN IN ARM OR SHOULDER' ;
          label &qp.42='LC PAIN IN OTHER PARTS OF BODY' ;

          drop xnum xmean ;
        run ;

%mend lcscore30;



/***********************/
/* Breast Cancer module */
/***********************/


%macro BRscore(data,qp);

        data &data ;
          set &data ;
          %do i=1 %to 23 ;
             if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
             format &qp.&i item4_. ;
          %end;
        run;

        %score(data=&data,type=F,scale=brbi,items = &qp.9 &qp.10 &qp.11 &qp.12) ;
        %score(data=&data,type=F,scale=brfu,items = &qp.13) ;

        %score(data=&data,type=S,scale=brsef,items = &qp.14 &qp.15) ;
        %score(data=&data,type=S,scale=brst,items = &qp.1 &qp.2 &qp.3 &qp.4 &qp.6 &qp.7 &qp.8) ;
        %score(data=&data,type=S,scale=brbs,items = &qp.20 &qp.21 &qp.22 &qp.23) ;
        %score(data=&data,type=S,scale=bras,items = &qp.17 &qp.18 &qp.19 ) ;
		%score(data=&data,type=S,scale=brsee,items = &qp.16) ;
		%score(data=&data,type=S,scale=brhl,items = &qp.5) ;


        data &data ;
          set &data ;
          if (&qp.15 = 1) then brsee=.;	
		  if (&qp.4 = 1) then brhl = .;
         run ;

        data &data ;
          set &data ;

          *Function scales;
          label brbi=  'BR BODY IMAGE' ;
          label brsef= 'BR SEXUAL FUNCTIONING' ;
          label brsee= 'BR SEXUAL ENJOYMENT' ;

          *Symptom scales;
          label brst= 'BR SYSTEMATIC THERAPY SIDE EFFECTS' ;
          label brbs= 'BR BREAST SYMPTOMS' ;
          label bras= 'BR ARM SYMPTOMS' ;
          label brhl= 'BR UPSET BY HAIR LOSS' ;
          label brfu= 'BR FUTURE PERSPECTIVE' ;

          *Additional variables;
          label &qp.9= 'BR PHYSICALLY LESS ATTRACTIVE' ;
          label &qp.10='BR FEELING LESS FEMININE' ;
          label &qp.11='BR DIFF TO LOOK AT YOURSELF NAKED' ;
          label &qp.12='BR DISSATISFIED WITH YOUR BODY' ;
          label &qp.14='BR INTERESTED IN SEX' ;
          label &qp.15='BR SEXUAL ACTIVE' ;
          label &qp.16='BR WAS SEX ENJOYABLE FOR YOU' ;
          label &qp.1= 'BR DRY MOUTH' ;
          label &qp.2= 'BR FOOD, DRINK TASTE DIFFERENT' ;
          label &qp.3= 'BR EYES PAINFUL, IRRITATED' ;
          label &qp.4= 'BR HAVE YOU LOST ANY HAIR' ;
          label &qp.6= 'BR DID YOU FEEL ILL OR UNWELL' ;
          label &qp.7= 'BR DID YOU HAVE HOT FLUSHES' ;
          label &qp.8= 'BR DID YOU HAVE HEADACHE' ;
          label &qp.20='BR PAIN AREA OF AFFECTED &qp.EAST' ;
          label &qp.21='BR AREA OF AFFECT. &qp.EAST SWOLLEN' ;
          label &qp.22='BR AFFECTED &qp.EAST OVERSENSITIVE' ;
          label &qp.23='BR SKIN PROBLEMS AFFECT. &qp.EAST' ;
          label &qp.17='BR PAIN IN ARM OR SHOULDER' ;
          label &qp.18='BR SWOLLEN ARM OR HAND' ;
          label &qp.19='BR DIFFICULTY TO RAISE YOUR ARM' ;
          label &qp.5= 'BR UPSET/HAIR LOSS' ;
          label &qp.13='BR WORRIED ABOUT HEALTH IN FUTURE' ;

          drop xnum xmean ;
        run ;

%mend BRscore ;

%macro BRscore30(data,qp);

        data &data ;
          set &data ;
          %do i=31 %to 53 ;
             if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
             format &qp.&i item4_. ;
          %end;
        run;

        %score(data=&data,type=F,scale=brbi,items = &qp.39 &qp.40 &qp.41 &qp.42) ;
        %score(data=&data,type=F,scale=brfu,items = &qp.43) ;

        %score(data=&data,type=S,scale=brsef,items = &qp.44 &qp.45) ;
        %score(data=&data,type=S,scale=brst,items = &qp.31 &qp.32 &qp.33 &qp.34 &qp.36 &qp.37 &qp.38) ;
        %score(data=&data,type=S,scale=brbs,items = &qp.50 &qp.51 &qp.52 &qp.53) ;
        %score(data=&data,type=S,scale=bras,items = &qp.47 &qp.48 &qp.49 ) ;
		%score(data=&data,type=S,scale=brsee,items = &qp.46) ;
		%score(data=&data,type=S,scale=brhl,items = &qp.35) ;


        data &data ;
          set &data ;
          if (&qp.45 = 1) then brsee=.;	
		  if (&qp.34 = 1) then brhl = .;
         run ;

        data &data ;
          set &data ;

          *Function scales;
          label brbi=  'BR BODY IMAGE' ;
          label brsef= 'BR SEXUAL FUNCTIONING' ;
          label brsee= 'BR SEXUAL ENJOYMENT' ;

          *Symptom scales;
          label brst= 'BR SYSTEMATIC THERAPY SIDE EFFECTS' ;
          label brbs= 'BR BREAST SYMPTOMS' ;
          label bras= 'BR ARM SYMPTOMS' ;
          label brhl= 'BR UPSET BY HAIR LOSS' ;
          label brfu= 'BR FUTURE PERSPECTIVE' ;

          *Additional variables;
          label &qp.39='BR PHYSICALLY LESS ATTRACTIVE' ;
          label &qp.40='BR FEELING LESS FEMININE' ;
          label &qp.41='BR DIFF TO LOOK AT YOURSELF NAKED' ;
          label &qp.42='BR DISSATISFIED WITH YOUR BODY' ;
          label &qp.44='BR INTERESTED IN SEX' ;
          label &qp.45='BR SEXUAL ACTIVE' ;
          label &qp.46='BR WAS SEX ENJOYABLE FOR YOU' ;
          label &qp.31='BR DRY MOUTH' ;
          label &qp.32='BR FOOD, DRINK TASTE DIFFERENT' ;
          label &qp.33='BR EYES PAINFUL, IRRITATED' ;
          label &qp.34='BR HAVE YOU LOST ANY HAIR' ;
          label &qp.36='BR DID YOU FEEL ILL OR UNWELL' ;
          label &qp.37='BR DID YOU HAVE HOT FLUSHES' ;
          label &qp.38='BR DID YOU HAVE HEADACHE' ;
          label &qp.50='BR PAIN AREA OF AFFECTED BREAST' ;
          label &qp.51='BR AREA OF AFFECT. BREAST SWOLLEN' ;
          label &qp.52='BR AFFECTED BREAST OVERSENSITIVE' ;
          label &qp.53='BR SKIN PROBLEMS AFFECT. &qp.EAST' ;
          label &qp.47='BR PAIN IN ARM OR SHOULDER' ;
          label &qp.48='BR SWOLLEN ARM OR HAND' ;
          label &qp.49='BR DIFFICULTY TO RAISE YOUR ARM' ;
          label &qp.35='BR UPSET/HAIR LOSS' ;
          label &qp.43='BR WORRIED ABOUT HEALTH IN FUTURE' ;

          drop xnum xmean ;
        run ;

%mend BRscore30 ;


/*******************************/
/* Head and Neck Cancer module */
/*******************************/

%macro hnscore(data,qp);

        data &data ;
          set &data ;
          %do i=1 %to 30 ;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
          %end;
          %do j=31 %to 35 ;
            if (&qp.&j<1 or &qp.&j>2) then &qp.&j=. ;
            format &qp.&j item2_. ;
          %end;
        run;

        %score(data=&data,type=S,scale=hnpa,items = &qp.1 &qp.2 &qp.3 &qp.4) ;
        %score(data=&data,type=S,scale=hnsw,items = &qp.5 &qp.6 &qp.7 &qp.8) ;
        %score(data=&data,type=S,scale=hnse,items = &qp.13 &qp.14) ;
        %score(data=&data,type=S,scale=hnsp,items = &qp.16 &qp.23 &qp.24) ;
        %score(data=&data,type=S,scale=hnso,items = &qp.19 &qp.20 &qp.21 &qp.22) ;
        %score(data=&data,type=S,scale=hnsc,items = &qp.18 &qp.25 &qp.26 &qp.27 &qp.28) ;
        %score(data=&data,type=S,scale=hnsx,items = &qp.29 &qp.30) ;

        %score(data=&data,type=S,scale=hnte,items = &qp.9) ;
        %score(data=&data,type=S,scale=hnom,items = &qp.10) ;
        %score(data=&data,type=S,scale=hndr,items = &qp.11) ;
        %score(data=&data,type=S,scale=hnss,items = &qp.12) ;
        %score(data=&data,type=S,scale=hnco,items = &qp.15) ;
        %score(data=&data,type=S,scale=hnfi,items = &qp.17) ;

        %score(data=&data,type=S,scale=hnpk,items = &qp.31) ;
        %score(data=&data,type=S,scale=hnnu,items = &qp.32) ;
        %score(data=&data,type=S,scale=hnfe,items = &qp.33) ;
        %score(data=&data,type=S,scale=hnwl,items = &qp.34) ;
        %score(data=&data,type=S,scale=hnwg,items = &qp.35) ;


        data &data ;
           set &data ;

           *Symptom scales;
           label hnpa= 'HN PAIN' ;
           label hnsw= 'HN SWALLOWING' ;
           label hnse= 'HN SENSES PROBLEMS' ;
           label hnsp= 'HN SPEECH PROBLEMS' ;
           label hnso= 'HN TROUBLE WITH SOCIAL EATING' ;
           label hnsc= 'HN TROUBLE WITH SOCIAL CONTACT' ;
           label hnsx= 'HN LESS SEXUALITY' ;
           label hnte= 'HN TEETH' ;
           label hnom= 'HN OPENING MOUTH' ;
           label hndr= 'HN DRY MOUTH' ;
           label hnss= 'HN STICKY SALIVA' ;
           label hnco= 'HN COUGHED' ;
           label hnfi= 'HN FELT ILL' ;
           label hnpk= 'HN PAIN KILLERS' ;
           label hnnu= 'HN NUTRITIONAL SUPPLEMENTS' ;
           label hnfe= 'HN FEEDING TUBE' ;
           label hnwl= 'HN WEIGHT LOSS'  ;
           label hnwg= 'HN WEIGHT GAIN'  ;

           *Additional variables;
           label &qp.1=  'HN PAIN IN YOUR MOUTH' ;
           label &qp.2=  'HN PAIN IN YOUR JAW' ;
           label &qp.3=  'HN SORENESS IN YOUR MOUTH' ;
           label &qp.4=  'HN PAINFUL THROAT' ;
           label &qp.5=  'HN PROBELMS SWALLOWING LI&qp.UIDS' ;
           label &qp.6=  'HN PROBLEMS SWALLOWING PUREED FOOD' ;
           label &qp.7=  'HN PROBLEMS SWALLOWING SOLID FOOD' ;
           label &qp.8=  'HN CHOKED WHEN SWALLOWING' ;
           label &qp.13= 'HN PROBLEMS WITH SENSE OF SMELL' ;
           label &qp.14= 'HN PROBLEMS WITH SENSE OF TASTE' ;
           label &qp.16= 'HN HAVE YOU BEEN HOARSE' ;
           label &qp.23= 'HN TROUBLE TALKING WITH OTHERS' ;
           label &qp.24= 'HN TROUBLE TALKING ON PHONE' ;
           label &qp.19= 'HN TROUBLE EATING' ;
           label &qp.20= 'HN TROUBLE EATING WITH FAMILY' ;
           label &qp.21= 'HN TROUBLE EATING WITH OTHERS' ;
           label &qp.22= 'HN TROUBLE ENJOYING YOUR MEAL' ;
           label &qp.18= 'HN BOTHERED WITH APPEARANCE' ;
           label &qp.25= 'HN TROUBLE CONTACT WITH FAMILY' ;
           label &qp.26= 'HN TROUBLE CONTACT WITH FRIENDS' ;
           label &qp.27= 'HN TROUBLE GOING OUT IN PUBLIC';
           label &qp.28= 'HN TROUBLE PHYSICAL CONTACT' ;
           label &qp.29= 'HN LESS INTEREST IN SEX' ;
           label &qp.30= 'HN LESS SEXUAL ENJOYMENT' ;
           label &qp.9=  'HN PROBLEMS WITH YOUR TEETH' ;
           label &qp.10= 'HN PROBLEMS OPENING MOUTH WIDE' ;
           label &qp.11= 'HN HAVE YOU HAD A DRY MOUTH' ;
           label &qp.12= 'HN HAVE YOU HAD STICKY SALIVA' ;
           label &qp.15= 'HN HAVE YOU COUGHED' ;
           label &qp.17= 'HN HAVE YOU FELT ILL' ;
           label &qp.31= 'HN USED PAIN-KILLER' ;
           label &qp.32= 'HN NUTRITIONAL SUPPLEMENTS' ;
           label &qp.33= 'HN USED A FEEDING TUBE' ;
           label &qp.34= 'HN HAVE YOU LOST WEIGHT' ;
           label &qp.35= 'HN HAVE YOU GAINED WEIGHT' ;

           drop xnum xmean ;
        run ;

%mend hnscore ;


%macro hnscore30(data,qp);

        data &data ;
          set &data ;
          %do i=31 %to 60 ;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
          %end;
          %do j=61 %to 65 ;
            if (&qp.&j<1 or &qp.&j>2) then &qp.&j=. ;
            format &qp.&j item2_. ;
          %end;
        run;

        %score(data=&data,type=S,scale=hnpa,items = &qp.31 &qp.32 &qp.33 &qp.34) ;
        %score(data=&data,type=S,scale=hnsw,items = &qp.35 &qp.36 &qp.37 &qp.38) ;
        %score(data=&data,type=S,scale=hnse,items = &qp.43 &qp.44) ;
        %score(data=&data,type=S,scale=hnsp,items = &qp.46 &qp.53 &qp.54) ;
        %score(data=&data,type=S,scale=hnso,items = &qp.49 &qp.50 &qp.51 &qp.52) ;
        %score(data=&data,type=S,scale=hnsc,items = &qp.48 &qp.55 &qp.56 &qp.57 &qp.58) ;
        %score(data=&data,type=S,scale=hnsx,items = &qp.59 &qp.60) ;

        %score(data=&data,type=S,scale=hnte,items = &qp.39) ;
        %score(data=&data,type=S,scale=hnom,items = &qp.40) ;
        %score(data=&data,type=S,scale=hndr,items = &qp.41) ;
        %score(data=&data,type=S,scale=hnss,items = &qp.42) ;
        %score(data=&data,type=S,scale=hnco,items = &qp.45) ;
        %score(data=&data,type=S,scale=hnfi,items = &qp.47) ;

        %score(data=&data,type=S,scale=hnpk,items = &qp.61) ;
        %score(data=&data,type=S,scale=hnnu,items = &qp.62) ;
        %score(data=&data,type=S,scale=hnfe,items = &qp.63) ;
        %score(data=&data,type=S,scale=hnwl,items = &qp.64) ;
        %score(data=&data,type=S,scale=hnwg,items = &qp.65) ;


        data &data ;
           set &data ;

           *Symptom scales;
           label hnpa= 'HN PAIN' ;
           label hnsw= 'HN SWALLOWING' ;
           label hnse= 'HN SENSES PROBLEMS' ;
           label hnsp= 'HN SPEECH PROBLEMS' ;
           label hnso= 'HN TROUBLE WITH SOCIAL EATING' ;
           label hnsc= 'HN TROUBLE WITH SOCIAL CONTACT' ;
           label hnsx= 'HN LESS SEXUALITY' ;
           label hnte= 'HN TEETH' ;
           label hnom= 'HN OPENING MOUTH' ;
           label hndr= 'HN DRY MOUTH' ;
           label hnss= 'HN STICKY SALIVA' ;
           label hnco= 'HN COUGHED' ;
           label hnfi= 'HN FELT ILL' ;
           label hnpk= 'HN PAIN KILLERS' ;
           label hnnu= 'HN NUTRITIONAL SUPPLEMENTS' ;
           label hnfe= 'HN FEEDING TUBE' ;
           label hnwl= 'HN WEIGHT LOSS'  ;
           label hnwg= 'HN WEIGHT GAIN'  ;

           *Additional variables;
           label &qp.31=  'HN PAIN IN YOUR MOUTH' ;
           label &qp.32=  'HN PAIN IN YOUR JAW' ;
           label &qp.33=  'HN SORENESS IN YOUR MOUTH' ;
           label &qp.34=  'HN PAINFUL THROAT' ;
           label &qp.35=  'HN PROBELMS SWALLOWING LI&qp.UIDS' ;
           label &qp.36=  'HN PROBLEMS SWALLOWING PUREED FOOD' ;
           label &qp.37=  'HN PROBLEMS SWALLOWING SOLID FOOD' ;
           label &qp.38=  'HN CHOKED WHEN SWALLOWING' ;
           label &qp.43= 'HN PROBLEMS WITH SENSE OF SMELL' ;
           label &qp.44= 'HN PROBLEMS WITH SENSE OF TASTE' ;
           label &qp.46= 'HN HAVE YOU BEEN HOARSE' ;
           label &qp.53= 'HN TROUBLE TALKING WITH OTHERS' ;
           label &qp.54= 'HN TROUBLE TALKING ON PHONE' ;
           label &qp.49= 'HN TROUBLE EATING' ;
           label &qp.50= 'HN TROUBLE EATING WITH FAMILY' ;
           label &qp.51= 'HN TROUBLE EATING WITH OTHERS' ;
           label &qp.52= 'HN TROUBLE ENJOYING YOUR MEAL' ;
           label &qp.48= 'HN BOTHERED WITH APPEARANCE' ;
           label &qp.55= 'HN TROUBLE CONTACT WITH FAMILY' ;
           label &qp.56= 'HN TROUBLE CONTACT WITH FRIENDS' ;
           label &qp.57= 'HN TROUBLE GOING OUT IN PUBLIC';
           label &qp.58= 'HN TROUBLE PHYSICAL CONTACT' ;
           label &qp.59= 'HN LESS INTEREST IN SEX' ;
           label &qp.60= 'HN LESS SEXUAL ENJOYMENT' ;
           label &qp.39=  'HN PROBLEMS WITH YOUR TEETH' ;
           label &qp.40= 'HN PROBLEMS OPENING MOUTH WIDE' ;
           label &qp.41= 'HN HAVE YOU HAD A DRY MOUTH' ;
           label &qp.42= 'HN HAVE YOU HAD STICKY SALIVA' ;
           label &qp.45= 'HN HAVE YOU COUGHED' ;
           label &qp.47= 'HN HAVE YOU FELT ILL' ;
           label &qp.61= 'HN USED PAIN-KILLER' ;
           label &qp.62= 'HN NUTRITIONAL SUPPLEMENTS' ;
           label &qp.63= 'HN USED A FEEDING TUBE' ;
           label &qp.64= 'HN HAVE YOU LOST WEIGHT' ;
           label &qp.65= 'HN HAVE YOU GAINED WEIGHT' ;

           drop xnum xmean ;
        run ;

%mend hnscore30 ;



/*******************************/
/* Brain Cancer module */
/*******************************/

%macro BNscore(data,qp);

		  data &data ;
          set &data ;
          %do i=1 %to 20 ;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
          %end;

        %score(data=&data , type=s, scale=Bfu, items= &qp.1 &qp.2 &qp.3 &qp.5);
        %score(data=&data , type=s, scale=Bvd, items= &qp.6 &qp.7 &qp.8);
        %score(data=&data , type=s, scale=Bmd, items= &qp.10 &qp.15 &qp.19);
        %score(data=&data , type=s, scale=Bcd, items= &qp.11 &qp.12 &qp.13);
        %score(data=&data , type=s, scale=Bha, items= &qp.4);
        %score(data=&data , type=s, scale=Bse, items= &qp.9);
        %score(data=&data , type=s, scale=Bdr, items= &qp.14);
        %score(data=&data , type=s, scale=Bhl, items= &qp.16);
        %score(data=&data , type=s, scale=Bis, items= &qp.17);
        %score(data=&data , type=s, scale=Bwl, items= &qp.18);
        %score(data=&data , type=s, scale=Bbc, items= &qp.20);


        Data &data;
        set &data ;
        LABEL Bfu='Future uncertainty';
        LABEL Bvd='Visual disorder';
        LABEL Bmd='Motor dysfunction';
        LABEL Bcd='Comm. deficit';
        LABEL Bha='Headaches';
        LABEL Bse='Seizures';
        LABEL Bdr='Drowsiness';
        LABEL Bhl='Hair loss';
        LABEL Bis='Itchy skin';
        LABEL Bwl='Weakness legs';
        LABEL Bbc='Bladder control';

		LABEL &qp.1='Did you feel uncertain about the future?'; 
		LABEL &qp.2='Did you feel you had setbacks in your condition?';
		LABEL &qp.3='Were you concerned about disruption of family life?';
		LABEL &qp.4='Did you have headaches?';
		LABEL &qp.5='Did your outlook on the future worsen?';
		LABEL &qp.6='Did you have double vision?';
		LABEL &qp.7='Was your vision blurred?';
		LABEL &qp.8='Did you have difficulty reading because of your vision?';
		LABEL &qp.9='Did you have seizures? ';
		LABEL &qp.10='Did you have weakness on one side of your body?';
		LABEL &qp.11='Did you have trouble finding the right words to express yourself?';
		LABEL &qp.12='Did you have difficulty speaking?';
		LABEL &qp.13='Did you have trouble communicating your thoughts?';
		LABEL &qp.14='Did you feel drowsy during the daytime?';
		LABEL &qp.15='Did you have trouble with your coordination?';
		LABEL &qp.16='Did hair loss bother you?';
		LABEL &qp.17='Did itching of your skin bother you?';
		LABEL &qp.18='Did you have weakness of both legs?';
		LABEL &qp.19='Did you feel unsteady on your feet?';
		LABEL &qp.20='Did you have trouble controlling your bladder?';

        drop xnum xmean ;
        run ;

%mend BNscore;

%macro BNscore30(data,qp);

		  data &data ;
          set &data ;
          %do i=31 %to 50 ;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
          %end;

        %score(data=&data , type=s, scale=Bfu, items= &qp.31 &qp.32 &qp.33 &qp.35);
        %score(data=&data , type=s, scale=Bvd, items= &qp.36 &qp.37 &qp.38);
        %score(data=&data , type=s, scale=Bmd, items= &qp.40 &qp.45 &qp.49);
        %score(data=&data , type=s, scale=Bcd, items= &qp.41 &qp.42 &qp.43);
        %score(data=&data , type=s, scale=Bha, items= &qp.34);
        %score(data=&data , type=s, scale=Bse, items= &qp.39);
        %score(data=&data , type=s, scale=Bdr, items= &qp.44);
        %score(data=&data , type=s, scale=Bhl, items= &qp.46);
        %score(data=&data , type=s, scale=Bis, items= &qp.47);
        %score(data=&data , type=s, scale=Bwl, items= &qp.48);
        %score(data=&data , type=s, scale=Bbc, items= &qp.50);


        Data &data;
        set &data ;
        LABEL Bfu='Future uncertainty';
        LABEL Bvd='Visual disorder';
        LABEL Bmd='Motor dysfunction';
        LABEL Bcd='Comm. deficit';
        LABEL Bha='Headaches';
        LABEL Bse='Seizures';
        LABEL Bdr='Drowsiness';
        LABEL Bhl='Hair loss';
        LABEL Bis='Itchy skin';
        LABEL Bwl='Weakness legs';
        LABEL Bbc='Bladder control';

		LABEL &qp.31='Did you feel uncertain about the future?'; 
		LABEL &qp.32='Did you feel you had setbacks in your condition?';
		LABEL &qp.33='Were you concerned about disruption of family life?';
		LABEL &qp.34='Did you have headaches?';
		LABEL &qp.35='Did your outlook on the future worsen?';
		LABEL &qp.36='Did you have double vision?';
		LABEL &qp.37='Was your vision blurred?';
		LABEL &qp.38='Did you have difficulty reading because of your vision?';
		LABEL &qp.39='Did you have seizures? ';
		LABEL &qp.40='Did you have weakness on one side of your body?';
		LABEL &qp.41='Did you have trouble finding the right words to express yourself?';
		LABEL &qp.42='Did you have difficulty speaking?';
		LABEL &qp.43='Did you have trouble communicating your thoughts?';
		LABEL &qp.44='Did you feel drowsy during the daytime?';
		LABEL &qp.45='Did you have trouble with your coordination?';
		LABEL &qp.46='Did hair loss bother you?';
		LABEL &qp.47='Did itching of your skin bother you?';
		LABEL &qp.48='Did you have weakness of both legs?';
		LABEL &qp.49='Did you feel unsteady on your feet?';
		LABEL &qp.50='Did you have trouble controlling your bladder?';

        drop xnum xmean ;
        run ;

%mend BNscore30;

/*******************************/
/* Ovarian Cancer module */
/*******************************/

%macro ovscore(data,qp);

		  data &data ;
          set &data ;
          %do i=1 %to 28 ;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
		  %end;
		  if &qp.8=1 then &qp.9=1; 

        %score(data=&data , type=F, scale=OVBI, items= &qp.20 &qp.21);
        %score(data=&data , type=F, scale=OVAT, items= &qp.22 &qp.23 &qp.24);
		%score(data=&data , type=F, scale=OVSE1, items= &qp.25 &qp.26);
		%score(data=&data , type=F, scale=OVSE2, items= &qp.25 &qp.26 &qp.27 &qp.28);

        %score(data=&data , type=S, scale=OVAG, items= &qp.1 &qp.2 &qp.3 &qp.4 &qp.5 &qp.6 &qp.7);
        %score(data=&data , type=S, scale=OVPN, items= &qp.11 &qp.12 &qp.13);
        %score(data=&data , type=S, scale=OVHM, items= &qp.18  &qp.19);
        %score(data=&data , type=S, scale=OVCH, items= &qp.10 &qp.14 &qp.15 &qp.16 &qp.17);
		%score(data=&data , type=S, scale=OVHL, items= &qp.8 &qp.9);


        Data &data;
        set &data ;
		OVSE=.;
		if &qp.26<=1 then OVSE=OVSE1;
		if &qp.26>=2 then OVSE=OVSE2;
        LABEL OVBI='OV Body image';
        LABEL OVAT='OV Attitude to dis./treatment';
        LABEL OVSE='OV Sexuality';
        LABEL OVAG='OV Abdominal/GI sympoms';
        LABEL OVPN='OV Peripheral Neuropathy';
        LABEL OVHM='OV Hormonal/menopausal symptoms';
        LABEL OVCH='OV Other chemo side-effects';
        LABEL OVHL='OV Hair loss';

		LABEL &qp.1='Did you have abdominal pain?';
		LABEL &qp.2='Did you have a bloated feeling in your abdomen / stomach?';
		LABEL &qp.3='Did you have problems with your clothes feeling too tight? ';
		LABEL &qp.4='Did you experience any change in bowel habit as a result of your disease or treatment? ';
		LABEL &qp.5='Were you troubled by passing wind / gas / flatulence? ';
		LABEL &qp.6='Have you felt full too quickly after beginning to eat? ';
		LABEL &qp.7='Have you had indigestion or heartburn? ';
		LABEL &qp.8='Have you lost any hair? ';
		LABEL &qp.9='Answer this question only if you had any hair loss: Were you upset by the loss of your hair? ';
		LABEL &qp.10='Did food and drink taste different from usual? ';
		LABEL &qp.11='Have you had tingling hands or feet? ';
		LABEL &qp.12='Have you had numbness in your fingers or toes? ';
		LABEL &qp.13='Have you felt weak in your arms or legs? ';
		LABEL &qp.14='Did you have aches or pains in your muscles or joints? ';
		LABEL &qp.15='Did you have problems with hearing? ';
		LABEL &qp.16='Did you urinate frequently? ';
		LABEL &qp.17='Have you had skin problems (e.g. itchy, dry)? ';
		LABEL &qp.18='Did you have hot flushes? ';
		LABEL &qp.19='Did you have night sweats? ';
		LABEL &qp.20='Have you felt physically less attractive as a result of your disease or treatment?';
		LABEL &qp.21='Have you been dissatisfied with your body? ';
		LABEL &qp.22='How much has your disease been a burden to you? ';
		LABEL &qp.23='How much has your treatment been a burden to you? ';
		LABEL &qp.24='Were you worried about your future health? ';
		LABEL &qp.25='To what extent were you interested in sex?';
		LABEL &qp.26='To what extent were you sexually active?';
		LABEL &qp.27='To what extent was sex enjoyable for you? ';
		LABEL &qp.28='Did you have a dry vagina during sexual activity? ';

        drop xnum xmean ovse1 ovse2;
        run ;

%mend ovscore;


%macro ovscore30(data,qp);

		  data &data ;
          set &data ;
          %do i=31 %to 58 ;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
		  %end;
		  if &qp.38=1 then &qp.39=1; 

        %score(data=&data , type=F, scale=OVBI, items= &qp.50 &qp.51);
        %score(data=&data , type=F, scale=OVAT, items= &qp.52 &qp.53 &qp.54);
		%score(data=&data , type=F, scale=OVSE1, items= &qp.55 &qp.56);
		%score(data=&data , type=F, scale=OVSE2, items= &qp.55 &qp.56 &qp.57 &qp.58);

        %score(data=&data , type=S, scale=OVAG, items= &qp.31 &qp.32 &qp.33 &qp.34 &qp.35 &qp.36 &qp.37);
        %score(data=&data , type=S, scale=OVPN, items= &qp.41 &qp.42 &qp.43);
        %score(data=&data , type=S, scale=OVHM, items= &qp.48  &qp.49);
        %score(data=&data , type=S, scale=OVCH, items= &qp.40 &qp.44 &qp.45 &qp.46 &qp.47);
		%score(data=&data , type=S, scale=OVHL, items= &qp.38 &qp.39);


        Data &data;
        set &data ;
		OVSE=.;
		if &qp.56<=1 then OVSE=OVSE1;
		if &qp.56>=2 then OVSE=OVSE2;
        LABEL OVBI='OV Body image';
        LABEL OVAT='OV Attitude to dis./treatment';
        LABEL OVSE='OV Sexuality';
        LABEL OVAG='OV Abdominal/GI sympoms';
        LABEL OVPN='OV Peripheral Neuropathy';
        LABEL OVHM='OV Hormonal/menopausal symptoms';
        LABEL OVCH='OV Other chemo side-effects';
        LABEL OVHL='OV Hair loss';

		LABEL &qp.31='Did you have abdominal pain?';
		LABEL &qp.32='Did you have a bloated feeling in your abdomen / stomach?';
		LABEL &qp.33='Did you have problems with your clothes feeling too tight? ';
		LABEL &qp.34='Did you experience any change in bowel habit as a result of your disease or treatment? ';
		LABEL &qp.35='Were you troubled by passing wind / gas / flatulence? ';
		LABEL &qp.36='Have you felt full too quickly after beginning to eat? ';
		LABEL &qp.37='Have you had indigestion or heartburn? ';
		LABEL &qp.38='Have you lost any hair? ';
		LABEL &qp.39='Answer this question only if you had any hair loss: Were you upset by the loss of your hair? ';
		LABEL &qp.40='Did food and drink taste different from usual? ';
		LABEL &qp.41='Have you had tingling hands or feet? ';
		LABEL &qp.42='Have you had numbness in your fingers or toes? ';
		LABEL &qp.43='Have you felt weak in your arms or legs? ';
		LABEL &qp.44='Did you have aches or pains in your muscles or joints? ';
		LABEL &qp.45='Did you have problems with hearing? ';
		LABEL &qp.46='Did you urinate frequently? ';
		LABEL &qp.47='Have you had skin problems (e.g. itchy, dry)? ';
		LABEL &qp.48='Did you have hot flushes? ';
		LABEL &qp.49='Did you have night sweats? ';
		LABEL &qp.50='Have you felt physically less attractive as a result of your disease or treatment?';
		LABEL &qp.51='Have you been dissatisfied with your body? ';
		LABEL &qp.52='How much has your disease been a burden to you? ';
		LABEL &qp.53='How much has your treatment been a burden to you? ';
		LABEL &qp.54='Were you worried about your future health? ';
		LABEL &qp.55='To what extent were you interested in sex?';
		LABEL &qp.56='To what extent were you sexually active?';
		LABEL &qp.57='To what extent was sex enjoyable for you? ';
		LABEL &qp.58='Did you have a dry vagina during sexual activity? ';

        drop xnum xmean ovse1 ovse2;
        run ;

%mend ovscore30;


/*******************************/
/* Prostate Cancer module */
/*******************************/

%macro prscore(data,qp);

		  data &data ;
          set &data ;
          %do i=1 %to 25;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
		  %end;
		  &qp.20_r=5-&qp.20;
		  &qp.21_r=5-&qp.21;
		  &qp.22_r=5-&qp.22;
		  run;

		%score(data=&data , type=F, scale=PRSAC, items= &qp.20_r &qp.21_r);
        %score(data=&data , type=F, scale=PRSFU, items= &qp.22_r &qp.23 &qp.24 &qp.25);

        %score(data=&data , type=S, scale=PRURI, items= &qp.1 &qp.2 &qp.3 &qp.4 &qp.5 &qp.6 &qp.7 &qp.9);
        %score(data=&data , type=S, scale=PRBOW, items= &qp.10 &qp.11 &qp.12 &qp.13);
        %score(data=&data , type=S, scale=PRHTR, items= &qp.14 &qp.15 &qp.16 &qp.17 &qp.18  &qp.19);
        %score(data=&data , type=S, scale=PRAID, items= &qp.8);

        Data &data;
        set &data ;
		PRSFU2=PRSFU; if &qp.20=1 then PRSFU2=0;
		LABEL PRSAC="Sexual activity";
		LABEL PRSFU="Sexual functioning";
		LABEL PRSFU2="Sexual functioning";
		LABEL PRURI="Urinary symptoms";	
		LABEL PRBOW="Bowel symptoms";	
		LABEL PRHTR="Hormonal symptoms";	
		LABEL PRAID="Incontinence aid";

		LABEL &qp.1="PR urinate frequently during the day";
		LABEL &qp.2="PR urinate frequently at night";
		LABEL &qp.3="PR hurry to get to the toilet";
		LABEL &qp.4="PR enough sleep because frequent urinating";
		LABEL &qp.5="PR difficulty going out of the house ";
		LABEL &qp.6="PR unintentional release of urine";
		LABEL &qp.7="PR pain when urinating";
		LABEL &qp.8="PR wearing an incontinence aid been a problem";
		LABEL &qp.9="PR daily activities limited by urinary problems";
		LABEL &qp.10="PR daily activities limited by bowel problems";
		LABEL &qp.11="PR unintentional release of stools";
		LABEL &qp.12="PR blood in your stools";
		LABEL &qp.13="PR bloated feeling in your abdomen";
		LABEL &qp.14="PR hot flushes";
		LABEL &qp.15="PR enlarged nipples or breasts";
		LABEL &qp.16="PR swelling in legs or ankles";
		LABEL &qp.17="PR weight loss";
		LABEL &qp.18="PR weight gain";
		LABEL &qp.19="PR felt less masculine";
		LABEL &qp.20="PR interested in sex";
		LABEL &qp.21="PR sexually active";
		LABEL &qp.22="PR sex enjoyable";
		LABEL &qp.23="PR difficulty getting/maintaining an erection";
		LABEL &qp.24="PR ejaculation problems";
		LABEL &qp.25="PR uncomfortable about sexually intimate";
        drop xnum xmean;
        run ;

%mend prscore;

%macro prscore30(data,qp);

		  data &data ;
          set &data ;
          %do i=31 %to 55;
            if (&qp.&i<1 or &qp.&i>4) then &qp.&i=. ;
            format &qp.&i item4_. ;
		  %end;
		  &qp.50_r=5-&qp.50;
		  &qp.51_r=5-&qp.51;
		  &qp.52_r=5-&qp.52;
		  run;

		%score(data=&data , type=F, scale=PRSAC, items= &qp.50_r &qp.51_r);
        %score(data=&data , type=F, scale=PRSFU, items= &qp.52_r &qp.53 &qp.54 &qp.55);

        %score(data=&data , type=S, scale=PRURI, items= &qp.31 &qp.32 &qp.33 &qp.34 &qp.35 &qp.36 &qp.37 &qp.39);
        %score(data=&data , type=S, scale=PRBOW, items= &qp.40 &qp.41 &qp.42 &qp.43);
        %score(data=&data , type=S, scale=PRHTR, items= &qp.44 &qp.45 &qp.46 &qp.47 &qp.48  &qp.49);
        %score(data=&data , type=S, scale=PRAID, items= &qp.38);
		
        Data &data;
        set &data ;
		PRSFU2=PRSFU; if &qp.50=1 then PRSFU2=0;
		LABEL PRSAC="Sexual activity";
		LABEL PRSFU="Sexual functioning";
		LABEL PRSFU2="Sexual functioning";
		LABEL PRURI="Urinary symptoms";	
		LABEL PRBOW="Bowel symptoms";	
		LABEL PRHTR="Hormonal symptoms";	
		LABEL PRAID="Incontinence aid";

		LABEL &qp.31="PR urinate frequently during the day";
		LABEL &qp.32="PR urinate frequently at night";
		LABEL &qp.33="PR hurry to get to the toilet";
		LABEL &qp.34="PR enough sleep because frequent urinating";
		LABEL &qp.35="PR difficulty going out of the house ";
		LABEL &qp.36="PR unintentional release of urine";
		LABEL &qp.37="PR pain when urinating";
		LABEL &qp.38="PR wearing an incontinence aid been a problem";
		LABEL &qp.39="PR daily activities limited by urinary problems";
		LABEL &qp.40="PR daily activities limited by bowel problems";
		LABEL &qp.41="PR unintentional release of stools";
		LABEL &qp.42="PR blood in your stools";
		LABEL &qp.43="PR bloated feeling in your abdomen";
		LABEL &qp.44="PR hot flushes";
		LABEL &qp.45="PR enlarged nipples or breasts";
		LABEL &qp.46="PR swelling in legs or ankles";
		LABEL &qp.47="PR weight loss";
		LABEL &qp.48="PR weight gain";
		LABEL &qp.49="PR felt less masculine";
		LABEL &qp.50="PR interested in sex";
		LABEL &qp.51="PR sexually active";
		LABEL &qp.52="PR sex enjoyable";
		LABEL &qp.53="PR difficulty getting/maintaining an erection";
		LABEL &qp.54="PR ejaculation problems";
		LABEL &qp.55="PR uncomfortable about sexually intimate";
        drop xnum xmean;
        run ;

%mend prscore30;




