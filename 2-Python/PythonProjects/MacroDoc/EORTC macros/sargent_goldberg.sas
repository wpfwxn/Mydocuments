***** Purpose of the macro: simulate the probabilities of the Sargent and Goldberg Phase II design
*****                Sargent and Goldberg, Statistics in Medicine 2001, 20:1051-1060
***** Corrections in Lui, Letter to the editor, SIM 2002, 21:625-627 
***** Other reference: presentation by LCO and MR Mattiaci in 2003 at stats club
***** Brief overview: Screening design with r arms (NO control arm).
*****   The idea is to pick an arm for Phase III that is not too bad.
*****   The criterion is that if one arm beats all the others by more than a rate d (e.g. d=0.05)
*****   for the primary endpoint (e.g. RR), then it will be selected (case 1).
*****   Otherwise one can choose among the best arms that are within d of each other by other methods (case 2). 
*****   The design controls the probability of picking the best arm at a level lambda, for the case where 
*****   the best arm has a true rate pi_a, and all the others have a rate pi_b (which is worse).
*****   Methods of estimating the probability of selecting the best:
*****    - count only the case 1 (very strict)
*****    - count case 1 + rho*case 2, where rho is between 0 and 1/r. 1/r then reflects some choice method, which is 
*****      conservatively seen as selecting at random
*****    - count case 1 + calculate for each type of case 2 how many arms would be within d of each other, and what
*****      would be the probability that a random choice out of the remaining arms, selects the good arm
*****      this is more complicated, but closest to what will happen. eg. if there are 3 arms, with observed rates 41% 39% 35%
*****      the choice will only be between the first two (if d=5%). So in such a case there is 0.5 (not 1/3) chance of selecting
*****      the first (or the second). 
*****      Note: if r=2, the second and third method are the same.
***** Input variables:
*****     r: number of arms
*****     n: number of patients per arm
*****     pi_a: success probability (eg. true response rate) in the one good arm
*****     pi_b: success probability in all the other arms 
*****     d: if one arm is >d better than all others, it will be selected (e.g. 0.05) 
*****     rho: the proportion of ambiguous cases that is included in the rate of successfull trials (where the good arm
*****          is selected)
*****     n_sim: number of simulations
***** Output: 1 page with
*****     Decision: 3 probabilities, in order: to select the good arm, to be in ambiguous situation, to select another arm
*****     Estimated_lambda: lambda estimated by using the value of rho: straight selection + rho * ambiguous cases
*****     Selected_p: lambda estimated by the most refined method, as described above (3rd dash)
*****                 this is what Sargent and Goldberg call pairwise comparison
***** Method to use: you need to search through the n-space to find a good value
***** Note: as for many discrete methods, results are not monotonous !
***** Written: 29 june 2005 
***** Author: J. Bogaerts
***** Validation: all tables in Sargent and Goldberg, 
                  and corrections in letter to the editor by Lui of SIM 2002 p.625
                  were reproduced 
***** Example calls:  %sargent_sim(r=3,n=25,pi_a=.90,pi_b=.70,d=0.09,rho=0.3333,n_sim=10000)
*****                 %sargent_sim(r=3,n=25,pi_a=.70,pi_b=.90,d=0.09,rho=0.3333,n_sim=10000)
*****                  example from table III in SIM 2002, p.628
*****                 %sargent_sim(r=3,n=68,pi_a=.40,pi_b=.25,d=0.05,rho=0.3333,n_sim=30000)
**********************************************************************************************************************;

%macro sargent_sim(r=,n=,pi_a=,pi_b=,d=,rho=,n_sim=);

title1"&r. treatment arms, &n. patients per arm, pi in best arm: &pi_a., pi in other arms: &pi_b.";
title2"rho=&rho., number of simulations: &n_sim. ";
title3"Select arm if observed rate is > &d. better than all others";
title4"Otherwise choose among those that are less than &d. worse than the best observed";
title6"Decision vector: simulations where best arm is chosen, where undecided, where another arm is chosen";
title7"Estimated lambda: calculation using rho value (choose from 0 to 1/r)";
title8"Selected-p: refined calculation taking into account";
title9"that arms drop out in undecided cases (because >= &d. worse)";

proc iml;

n=&n.;
r=&r.;
pi_a=&pi_a.;
pi_b=&pi_b.;
diff=&d.*n;
sim_vec=j(1,r,0);
sim_rank=j(1,r,0);
decision=j(1,3,0);
selection=j(1,3,0);
selected_p=0;

do sim=1 to &n_sim.;
   sim_vec[1]=ranbin(0,n,pi_a);
   do i=2 to r;
     sim_vec[i]=ranbin(0,n,pi_b);
	 end;
   sim_rank=r+1-rank(sim_vec);
   if ((sim_rank[1]=1) & ((sim_vec[1]-sim_vec[loc(sim_rank=2)]) > diff)) then decision[1]=decision[1]+1;
   else if ((sim_vec[loc(sim_rank=1)]-sim_vec[loc(sim_rank=2)]) <= diff) then decision[2]=decision[2]+1;
   else decision[3]=decision[3]+1;
   selection=((sim_vec+diff)>sim_vec[loc(sim_rank=1)]);
   selected_p=selected_p+(selection[1]/selection[+]);
   end;

decision=decision/&n_sim.;
estimated_lambda=decision[1]+&rho.*decision[2];
selected_p=selected_p/&n_sim.;
print decision estimated_lambda;
print selected_p;

quit iml;
run;

title;

%mend;


