/************************************************************************************
*
* BRYADAY.SAS
* *************
*
* Macro Program for Single stage and 2-stages Bryant-Day Phase II design
*
* Reference: Incorporating toxicity considerations into the design of two stages phase II clinical trials
* Biometrics 51:1372-83, december 1995. (1)
* 
* Version date    : December 2007
* Software        : SAS version 9.1
* Original author : Jérôme RAPION 
*                   based on proc IML program from Jan Bogaerts. (2006)
*
*************************************************************************************
*
* HISTORY
*
*   Based on: a program with proc IML procedure from Jan Bogaerts (2006)
*
*************************************************************************************
*
* PARAMETERS
*
*    alprnom    : type I error on response rate tests
*    alptnom    : type I error on toxicity rates tests
*    betanom    : Power 
*    pt0        : pt0 s.th. 1-pt0 = min tox rate above which the trial must be stopped (0.3)
*    pt1        : pt1 s.th. 1-pt1 = max tox rate below which it is safe to continue (0.2 )
*    pr0        : max response rate which does not warrant continuing 
*    pr1        : min response rate which does warrant continuing 
*    nmax       : max number of patients available
*    outfile    : full path of result file, see EXAMPLE section (Default: C:\temp\results.rtf);
*    
*************************************************************************************
*
* NOTES
*
* Output given is:
* 
* NBEST= total sample size [N2 in (1)]
* N1BEST=sample size for stage 1 [N1 in (1)]
* RT1BEST=Number such that if at stage I, #patients without toxicity <=RT1BEST, stop [CT1 in (1)]
* RR1BEST=Number such that if at stage I, #patients with response <=RR1BEST, stop [CR1 in (1)]
* RTBEST=Number such that if #pts without toxicity <=RTBEST, consider the trial negative [CT2 in (1)]
* RRBEST=Number such that if #patients with response <=RRBEST, consider the trial negative [CR2 in (1)]
* 
* ENBEST=E[N]
* PETBEST=Probability of early termination
* ABEST=Probability of considering the trial positive
* These values are given in matrix form
* The number are as follows:
*  in the top left corner corresponds to the case of #non toxicity <=pT0 (unacceptable toxicity) 
*  and response rate<=pr0 (unacceptable response)
*  in the top right corner corresponds to #non toxicity >=pT1 (acceptable toxicity) and 
*  response rate <=pr0 (unacceptable response) = alpha resp
*  in the lower left corner corresponds to #non toxicity<=pT0 (unacceptable toxicity) 
*  and response rate>=pr1 (acceptable response) = alpha tox
*  in the lower right corner corresponds to the #non toxicity>=pT1 (acceptable toxicity) 
*  and response rate >=pr1 (acceptable response) = beta
*
* *****************************
*  It produces 2 SAS datasets :
* *****************************
*
* - work.parameter which contains PT0, PT1, PR0, PR1, ALPRNOM, ALPRNOM, BETANOM
* - work.DESIGN which contains NBEST, N1BEST, RTBEST, RRBEST, RT1BEST, RR1BEST, CRITBEST [max {E01, E10} in (1)]
* - work.PET which contains PETij (Probability of early termination)
* 
*************************************************************************************
*
* EXAMPLE :
*
* %BRYADAY(pt0=0.6,pt1=0.8,pr0=0.05,pr1=0.25,alprnom=0.10,alptnom=0.10,betanom=0.10,nmax=45);
* %BRYADAY(pt0=0.6,pt1=0.8,pr0=0.05,pr1=0.25,alprnom=0.10,alptnom=0.10,betanom=0.10,nmax=45,outfile=h:\desktop\test.rtf);
*
************************************************************************************/


%macro BRYADAY(pt0=,pt1=,pr0=,pr1=,alprnom=,alptnom=,betanom=,nmax=,outfile=);

OPTIONS NODATE NONUMBER ORIENTATION=PORTRAIT;

%if %length(&outfile.)=0 %then %let outfile=C:\temp\results.rtf;

ods rtf file="&outfile." style=EORTCStyle1 BODYTITLE;

title "Bryant and Day design";

ods output  PT0_PT1_PR0_PR1_ALPRNOM_ALPTNOM_ = parameter;

proc iml;

**********************start setup module************************;
start setup2(prob, cum, pet , nmax ,p);

prob=j(nmax,nmax+1,0);
cum=j(nmax,nmax+1,0);
prob[1,1]=1-p;
prob[1,2]=p;
cum[1,]=cusum(prob[1,]);
do k=2 to nmax;
   shift=0||prob[k-1,1:nmax];
   prob[k,]=(1-p)*prob[k-1,]+p*shift;
   cum[k,]=cusum(prob[k,]);
end;

pet=j(nmax,1,0)||cum;    *Pr{Y1<=r1|p0}, n1=1,2,...,nmax,
                                             r=-1,0,1,...,nmax;

finish setup2;
**********************finish setup module************************;
**********************start module to compute minimal one stage solution*****;
start onestage(n1stage , rt1stage , rr1stage , a1stage , alprnom , alptnom , betanom ,
        cumt0 ,cumt1 ,cumr0 , cumr1 );
n1stage=.;
rt1stage=.;
rr1stage=.;
a1stage=j(2,2,.);

at0=1-cumt0;
at1=1-cumt1;
ar0=1-cumr0;
ar1=1-cumr1;

nmax=nrow(at0);

feasr=( ar0<=(alprnom/(1-betanom)) )#(ar1>=(1-betanom)) ;
feasr=feasr[,<>];
nr=feasr[<:>];
feasr=feasr[nr];

feast=( at0<=(alptnom/(1-betanom)) )#(at1>=(1-betanom)) ;
feast=feast[,<>];
nt=feast[<:>];
feast=feast[nt];

*print feasr feast nr nt;
feasboth=0;
if (feasr#feast=1) then do;
    n1stage=max(nt,nr)-1;
    do while( (feasboth=0) & (n1stage<nmax) );
       n1stage=n1stage+1;
       feas1=(at1[n1stage,]`*ar0[n1stage,])<=alprnom;
       feas2=(at0[n1stage,]`*ar1[n1stage,])<=alptnom;
       feas3=(at1[n1stage,]`*ar1[n1stage,])>=(1-betanom);
       feas=feas1#feas2#feas3;
       rowmax=feas[,<>];
       bestcol=feas[,<:>];
       feasboth=rowmax[<>];
       bestrow=rowmax[<:>];
       bestcol=bestcol[bestrow];
       *print n1stage feasboth;
    end;
end;
if feasboth=0 then do;
      print
      "WARNING: nmax must be increased to determine min req'd 1-stage n";
      n1stage=.;
end;
else do;
       rr1stage=bestcol-1;
       rt1stage=bestrow-1;
       alphat=at0[n1stage,rt1stage+1]||at1[n1stage,rt1stage+1];
       alphar=ar0[n1stage,rr1stage+1]//ar1[n1stage,rr1stage+1];
       a1stage=alphar*alphat;
end;
finish onestage;
**********************finish onestage**********************************;
*********************start module to compute alpha matrices***;
start alpha(alpha,r,n,prob,cum);

*for fixed n and r (n>=2, r>=0, r<n), this module finds the (n-1)x(m+2) alpha
 matrix whose (n1,r1) element is Pr{Recommend Tx}, n1=1,2,...,n-1,
 r1=-1,0,1,...,m=min(n-2,r-1);

m=min(n-2,r-1);
alpha=j(n-1,m+2,0);

r1=-1;
alpha[,1]=cum[n,r+1]*j(n-1,1,1);   *for r1=-1, n1=1,2,...,n-2,
                                  Pr{accept tx}=Pr{Y<=r|n};

do r1=0 to m;
   alpha[,r1+2]=alpha[,r1+1]+prob[1:n-1,r1+1]#(1-cum[n-1:1,r-r1+1]);
end;

alpha=1-alpha;

finish alpha;
*********************finish module to optimize over (N1,R1), given (N,R)***;
*********************start module fixnr2***********************************;
start fixnr2(critbest,rt1best,rr1best,n1best,alpbest,petbest,enbest,
             n,rt,rr,alprnom,alptnom,betanom,at0,at1,ar0,ar1,pett0,
             pett1,petr0,petr1);

critbest=10**6;
rt1best=.;
rr1best=.;
n1best=.;
alpbest=j(2,2,.);
petbest=j(2,2,.);
enbest=j(2,2,.);

mt=min(rt-1,n-2);
allrt1=(1:mt+2);
mr=min(rr-1,n-2);
allrr1=(1:mr+2);
alln1=(1:n-1);
alln2=n-alln1;

got1=1-pett1[alln1,allrt1];
got0=1-pett0[alln1,allrt1];
gor0=1-petr0[alln1,allrr1];
gor1=1-petr1[alln1,allrr1];

*print n rt rr alprnom alptnom betanom got0 got1 gor0 gor1;
betafeas=1;
rr1=-1;
rt1=-1;
if (mr<mt) then do;
   ones=j(1,mt+2,1);
   do while( (rr1<=mr) & (betafeas=1) );
       *print "begin  the rr1 loop   rr1 ="  rr1;
       alpha1 =at1#(ar0[,rr1+2]*ones);
       alpha2 =at0#(ar1[,rr1+2]*ones);
       alpha3 =at1#(ar1[,rr1+2]*ones);

       feas1=(alpha1<=alprnom);
       feas2=(alpha2<=alptnom);
       feas3=(alpha3>=(1-betanom));
       betafeas=feas3[<>,<>];
       feas=feas1#feas2#feas3;
       *print feas1 feas2 feas3 feas;

       gor0temp=gor0[,rr1+2];
       gor1temp=gor1[,rr1+2];

       c1=got1#(gor0temp*ones);
       c2=got0#(gor1temp*ones);

       criter1=(alln1`*ones) + (alln2`*ones)#c1;
       criter2=(alln1`*ones) + (alln2`*ones)#c2;
       criter=(criter1<>criter2);

       criter=criter#feas+(10**9)*(1-feas);
       *print  gor0temp gor1temp c1 c2 criter1 criter2 criter;
       rowmins=criter[,><];
       bestcol=criter[,>:<];
       bestcrit=rowmins[><];
       bestrow=rowmins[>:<];
       bestcol=bestcol[bestrow];
       criter=criter[bestrow,bestcol];
       *print bestrow bestcol criter;
       if (criter<critbest) then do;
           critbest=criter;
           n1best=bestrow;
           rt1best=bestcol-2;
           rr1best=rr1;
       end;
       rr1=rr1+1;
   end;
end;
else do;
   ones=j(1,mr+2,1);
   do while( (rt1<=mt) & (betafeas=1) );
       alpha1 =ar0#(at1[,rt1+2]*ones);
       alpha2 =ar1#(at0[,rt1+2]*ones);
       alpha3 =ar1#(at1[,rt1+2]*ones);

       feas1=(alpha1<=alprnom);
       feas2=(alpha2<=alptnom);
       feas3=(alpha3>=(1-betanom));
       betafeas=feas3[<>,<>];
       feas=feas1#feas2#feas3;

       got0temp=got0[,rt1+2];
       got1temp=got1[,rt1+2];

       c1=gor0#(got1temp*ones);
       c2=gor1#(got0temp*ones);

       criter1=(alln1`*ones) + (alln2`*ones)#c1;
       criter2=(alln1`*ones) + (alln2`*ones)#c2;
       criter=(criter1<>criter2);

       criter=criter#feas+(10**9)*(1-feas);
       rowmins=criter[,><];
       bestcol=criter[,>:<];
       bestcrit=rowmins[><];
       bestrow=rowmins[>:<];
       bestcol=bestcol[bestrow];
       criter=criter[bestrow,bestcol];

       if (criter<critbest) then do;
           critbest=criter;
           n1best=bestrow;
           rr1best=bestcol-2;
           rt1best=rt1;
       end;
       rt1=rt1+1;
   end;
end;

*print critbest;
critbest =choose(critbest>10**5,.,critbest);
*print critbest;
if critbest ^=. then do;
       *print "beginning the finishup loop";
       *print critbest;
       *print n1best rt1best rr1best;
       alphat =at0[n1best,rt1best+2]||at1[n1best,rt1best+2];
       alphar =ar0[n1best,rr1best+2]//ar1[n1best,rr1best+2];
       alpbest=alphar*alphat;
       pett=1-(pett0[n1best,rt1best+2]||pett1[n1best,rt1best+2]);
       petr=1-(petr0[n1best,rr1best+2]//petr1[n1best,rr1best+2]);
       petbest=1-petr*pett;
       enbest=n-(n-n1best)*petbest;
end;


finish fixnr2;
*******************************finish fixnr2*********************************;
*******************************start bivariate simon optimizer***************;
start simonop2(pt0,pt1,pr0,pr1,alprnom,alptnom,betanom,nmax);

run setup2(probt0,cumt0,pett0,nmax,pt0);
run setup2(probt1,cumt1,pett1,nmax,pt1);
run setup2(probr0,cumr0,petr0,nmax,pr0);
run setup2(probr1,cumr1,petr1,nmax,pr1);

/*
print      pt0 probt0 cumt0 pett0
           pt1 probt1 cumt1 pett1
           pr0 probr0 cumr0 petr0
           pr1 probr1 cumr1 petr1;
*/

run onestage(n1stage,rt1stage,rr1stage,a1stage,alprnom,alptnom,betanom,
        cumt0,cumt1,cumr0,cumr1);

print pt0 pt1 pr0 pr1 alprnom alptnom betanom;
print "Best one-stage solution";
print n1stage rt1stage rr1stage,a1stage;

nstart=floor(.9*n1stage);
nstop=ceil(1.25*n1stage);
nstop=min(nstop,nmax);

nrows=nstop-nstart+1;
ns=(nstart:nstop)`;
n1best=j(nrows,1,.);
rtbest=j(nrows,1,.);
rrbest=j(nrows,1,.);
rt1best=j(nrows,1,.);
rr1best=j(nrows,1,.);
critbest=j(nrows,1,.);
enbest=j(nrows,4,.);
petbest=j(nrows,4,.);
abest=j(nrows,4,.);
rtstart=j(nrows,1,.);
rrstart=j(nrows,1,.);
rtstop=j(nrows,1,.);
rrstop=j(nrows,1,.);
lastrt=j(nrows,1,.);
lastrr=j(nrows,1,.);

do n=nstart to nstop;
  upbound=9999;
  row=n-nstart+1;

  rtstart[row]=floor(n*pt0); *compute bounds on the range of rt s to search;
  if (row >=2) then lastrt[row]=rtbest[row-1];
  *rtstart[row]=max(rtstart[row],lastrt[row]-2,0);
  rtstop2=cumt1[n,]<=betanom;
  rtstop[row]=rtstop2[+]-1;
  *rtstop[row]=min(rtstop[row],lastrt[row]+5);

  rrstart[row]=floor(n*pr0); *compute bounds on the range of rr s to search;
  if (row >=2) then lastrr[row]=rrbest[row-1];
  *rrstart[row]=max(rrstart[row],lastrr[row]-2,0);
  rrstop2=cumr1[n,]<=betanom;
  rrstop[row]=rrstop2[+]-1;
  *rrstop[row]=min(rrstop[row],lastrr[row]+5);

  rt=rtstart[row];
  do while (rt<=min(rtstop[row],n-1));
     run alpha(at0,rt,n,probt0,cumt0);
     run alpha(at1,rt,n,probt1,cumt1);
     rr=rrstart[row];
     do while (rr<=min(rrstop[row],n-1));
        run alpha(ar0,rr,n,probr0,cumr0);
        run alpha(ar1,rr,n,probr1,cumr1);

        run fixnr2(crit,rt1,rr1,n1,alp,pet,en,
             n,rt,rr,alprnom,alptnom,betanom,at0,at1,ar0,ar1,pett0,
             pett1,petr0,petr1);

        *print critbest rt1best rr1best n1best, alpbest petbest enbest;

        if (crit ^= .) & (crit < upbound) then do;

             upbound=crit;
             critbest[row,]=crit;
             abest[row,]=shape(alp,1,4);
             enbest[row,]=shape(en,1,4);;
             petbest[row,]=shape(pet,1,4);
             n1best[row,]=n1;
             rtbest[row,]=rt;
             rrbest[row,]=rr;
             rt1best[row,]=rt1;
             rr1best[row,]=rr1;
         end;
         rr=rr+1;
      end;
      rt=rt+1;
  end;
end;

print ns lastrt rtstart rtstop rtbest critbest[format=4.1];
print ns lastrr rrstart rrstop rrbest critbest[format=4.1];
print ns n1best rtbest rrbest rt1best rr1best critbest[format=4.1];
print ns enbest[format=4.1] petbest[format=4.3] critbest[format=4.1];
print ns abest[format=4.3] critbest[format=4.1];

bestrow=critbest[>:<];
nbest=ns[bestrow];
n1best=n1best[bestrow];
rtbest=rtbest[bestrow];
rrbest=rrbest[bestrow];
rt1best=rt1best[bestrow];
rr1best=rr1best[bestrow];
critbest=critbest[bestrow];
enbest=enbest[bestrow,];
petbest=petbest[bestrow,];
abest=abest[bestrow,];

enbest=shape(enbest,2,2);
petbest=shape(petbest,2,2);
abest=shape(abest,2,2);

create design var{nbest n1best rtbest rrbest rt1best rr1best critbest};
append;

varnames='PETBEST0':'PETBEST1';
create PET from petbest[colname=varnames];
append from petbest;

print nbest n1best rtbest rrbest rt1best rr1best critbest[format=4.1];

print enbest[format=4.1] petbest[format=4.3] abest[format=4.3];

finish simonop2;
**************finish simonop2**********************************************;

alprnom=&alprnom. ; *** type I error on response rate tests ;
alptnom=&alptnom.;  *** type I error on toxicity rates tests ;
betanom=&betanom.;  *** Power ;
pt0=&pt0.;       *** pt0 s.th. 1-pt0 = min tox rate above which the trial must be stopped (0.3) ;
pt1=&pt1.;       *** pt1 s.th. 1-pt1 = max tox rate below which it is safe to continue (0.2 );
pr0=&pr0.;      *** max response rate which does not warrant continuing ;
pr1=&pr1.;      *** min response rate which does warrant continuing ;
nmax=&nmax.;      *** max number of patients available ;

run simonop2(pt0,pt1,pr0,pr1,alprnom,alptnom,betanom,nmax);  

ods rtf close;
ods output close;

quit;


%MEND BRYADAY;


