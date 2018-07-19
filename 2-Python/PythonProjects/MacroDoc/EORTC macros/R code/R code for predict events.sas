SUBMIT / R;

attach(Hazard_rates)
upper <- time_from[2:length(time_from)] 
N_rates <- length(time_from)
maxevents <- maxevents[1]
dropout <- dropout[1]
lambdas <- lambda
detach(Hazard_rates)

attach(data_timevar)

events <- length(which( X_event_ny == 1))
timevar_sub <- timevar[ which( X_event_ny == 0 & dropped != 1) ]
N <- length(timevar)
remaining_sub <- remaining_till_today[ which( X_event_ny == 0 & dropped != 1) ]
Npatid <- length(timevar_sub)
allow <- allow[1]


event_t <- function(extrat,backlog)
{

Nextra <- 0
remaining_sub2 <- remaining_sub
timevar_sub2 <- timevar_sub

if (ACCRUALFINAL[1]=="N")
	{
	Nextra <- min(round(extrat*accrualrate,digits=0),TOTALSAMPLESIZE-N)
	if (Nextra >=1)
		{
		remaining_sub2 <- matrix(0,Npatid+Nextra,1)
		remaining_sub2[1:Npatid] <- remaining_sub
		remaining_sub2[(Npatid+1):(Npatid+Nextra)] <- -(1:Nextra)/accrualrate
		timevar_sub2 <- matrix(0,Npatid+Nextra,1)
		timevar_sub2[1:Npatid] <- timevar_sub
		timevar_sub2[(Npatid+1):(Npatid+Nextra)] <- 0
		}
	}

event_n <- matrix(0,Npatid+Nextra,1) 
if (N_rates > 1)
	{
	for (j in 1:(Npatid+Nextra))
    	{
		t2 <- max(remaining_sub2[j] + extrat - backlog , 0)
    	exp.part <- 1
    	time.prev <- 0
	    for (i in 1:(N_rates-1))
        	{
	    	event_n[j] <- event_n[j] + exp.part*(1- exp(-(lambdas[i]+dropout)*(  min( (t2-time.prev), max(0,upper[i]-timevar_sub2[j]-time.prev) )    )))*( lambdas[i]/(lambdas[i]+dropout) )  
			exp.part <- exp.part*(exp(-(lambdas[i]+dropout)*(  min( (t2-time.prev), max(0,upper[i]-timevar_sub2[j] -time.prev) ) )))
			time.prev <- time.prev + min( (t2-time.prev), max(0,upper[i]-timevar_sub2[j] -time.prev) )
			}
		event_n[j] <- event_n[j] + exp.part*(1- exp(-(lambdas[N_rates]+dropout)*( (t2-time.prev)    )))*( lambdas[N_rates]/(lambdas[N_rates]+dropout) ) 
		}
	}
if (N_rates == 1)
	{
	for (j in 1:(Npatid+Nextra))
    	{
		t2 <- max(remaining_sub2[j] + extrat - backlog , 0)
		event_n[j] <- (1- exp(-(lambdas[1]+dropout)*t2) )*(lambdas[1]/(lambdas[1]+dropout))
		}
	}
total <- sum(event_n)+events - maxevents
return(total)
}

extra_time_maxevent <- uniroot(event_t,interval=c(0,10000),backlog=0)$root
extra_time_maxevent2 <- uniroot(event_t,interval=c(0,10000),backlog=allow)$root

#print(paste("Extra time (in UNIT) post-today until ",maxevents," events are observed:",extra_time_maxevent))
#print(paste("Extra time (in UNIT) post-today until ",maxevents," events are observed, including backlog:",extra_time_maxevent2))

x.plot <- seq(from=0,to=(extra_time_maxevent2+(extra_time_maxevent2)/5),length.out=100)
y.plot <- matrix(NA,100,1)
y.plot.b <- matrix(NA,100,1)

for (i in (1:100))
	{
	y.plot[i] <- event_t(x.plot[i],backlog=0)+ maxevents
	y.plot.b[i] <- event_t(x.plot[i],backlog=allow)+ maxevents
	}
maxevents_solution <- data.frame(maxevent_extra_t=extra_time_maxevent,maxevent_extra_tb=extra_time_maxevent2,extra.time.plot=x.plot,total.events=y.plot,total.events.b=y.plot.b)

ENDSUBMIT;


