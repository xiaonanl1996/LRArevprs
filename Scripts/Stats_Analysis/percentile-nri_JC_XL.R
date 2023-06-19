# https://github.com/smckearnan/percentile-nri/blob/master/percentile-nri.R
# https://academic.oup.com/aje/article/187/6/1327/4786118

## Percentile-based NRI: bin data based on evenly-spaced quantiles of risk
library(boot)
library(survMisc)

## Function for computing net reclassification improvement, taking censoring into account
## Due to Pencina (2011)
## risks.1 and risks.2 are predicted risks for the models being compared
## T = time to event, C = censored/uncensored (vectors of length equal to risks.1 and risks.2)
## t = length of time period under consideration (single numeric quantity)
compute.cNRI <- function(risks.1,risks.2,event_time, censoring, fu_time) {
  n <- length(event_time)
  
  up.class <- (risks.2>risks.1)
  down.class <- (risks.1>risks.2)
  n.u <- sum(up.class) ## Number up-classified
  n.d <- sum(down.class) ## Number down-classified
  
  KM <- survfit(Surv(event_time, censoring)~1) ## overall
  p.KM <- 1 - KM$surv[max(which(KM$time<=fu_time))] ## P(event)
  
  if(n.u==0) {
    p.KM.u <- 1
  } else {
    KM.u <- survfit(Surv(event_time, censoring)~1,subset=up.class) ## up-classified
    p.KM.u <- 1 - KM.u$surv[max(which(KM.u$time<=fu_time))] ## P(event|up)
  }
  if(n.d==0) {
    p.KM.d <- 1
  } else {
    KM.d <- survfit(Surv(event_time, censoring)~1,subset=down.class) ## down-classified
    p.KM.d <- 1 - KM.d$surv[max(which(KM.d$time<=fu_time))] ## P(event|down)
  }
  
  nri.e <- (n.u*p.KM.u - n.d*p.KM.d)/(n*p.KM)
  nri.ne <- (n.d*(1-p.KM.d) - n.u*(1-p.KM.u))/(n*(1-p.KM))
  
  c(cNRI.events=nri.e,cNRI.nonevents=nri.ne,cNRI=nri.e+nri.ne)
}

compute.NRI <- function(risks.1,risks.2, outcome) {
  # Methodology from https://github.com/cran/nricens
  n <- length(outcome)
  
  up.class <- (risks.2>risks.1)
  down.class <- (risks.1>risks.2)
  n.u <- sum(up.class) ## Number up-classified
  n.d <- sum(down.class) ## Number down-classified
  
  pr.upp.case = mean(up.class[outcome == 1])
  pr.dwn.case = mean(down.class[outcome == 1])
  pr.dwn.ctrl = mean(down.class[outcome == 0])
  pr.upp.ctrl = mean(up.class[outcome == 0])
  
  nri.case = pr.upp.case - pr.dwn.case
  nri.ctrl = pr.dwn.ctrl - pr.upp.ctrl
  nri      = nri.case + nri.ctrl
  
  return(c(cNRI.events=nri.case, cNRI.nonevents=nri.ctrl, cNRI=nri))
}

## Function for computing net reclassification improvement specific for bootstrap sampling
## Used in the following function for bootstrapping confidence intervals for the NRI
compute.cNRI.boot <- function(data, ind) {
  ## Use only data indexed by bootstrap sampling
  risks.1 <- data[ind,]$risks.1
  risks.2 <- data[ind,]$risks.2
  event_time <- data[ind,]$event_time
  censoring <- data[ind,]$censoring
  fu_time <- data[1,]$fu_time
  ## Compute censored NRI as above 
  compute.cNRI(risks.1, risks.2, event_time, censoring, fu_time)
}

## Function for computing net reclassification improvement specific for bootstrap sampling
## Used in the following function for bootstrapping confidence intervals for the NRI
compute.NRI.boot <- function(data, ind) {
  ## Use only data indexed by bootstrap sampling
  risks.1 <- data[ind,]$risks.1
  risks.2 <- data[ind,]$risks.2
  outcome <- data[ind,]$outcome
  ## Compute  NRI as above 
  compute.NRI(risks.1, risks.2, outcome)
}

## Function to return bootstrap confidence intervals for event, non-event, and overall NRI
ci.nri <- function(risks.1, risks.2, outcome=NULL, event_time=NULL, censoring=NULL, fu_time=NULL){
  ## Set number of cores accordingly for parallel computing
  if (!is.null(outcome)){
    booted <- boot(data = data.frame(risks.1,risks.2, outcome), 
                 statistic = compute.NRI.boot, R = 1000,
                 parallel="multicore", ncpus=20)
  } else if (!is.null(event_time) & !is.null(censoring) & !is.null(fu_time)) {
    booted <- boot(data = data.frame(risks.1,risks.2, event_time, censoring, futime), 
                 statistic = compute.cNRI.boot, R = 1000,
                 parallel="multicore", ncpus=20)
  } else { print("Wrong combination of parameters entered")}
  
  ## Compute confidence intervals for event, non-event, and overall NRI
  ci.event <- boot.ci(boot.out = booted, conf=0.95, type = "perc")
  ci.nonevent <- boot.ci(boot.out = booted, conf=0.95, type = "perc", index=2)
  ci.overall <- boot.ci(boot.out = booted, conf=0.95, type = "perc", index=3)
  ## Output lower and upper bounds 
  ci <- cbind(ci.event$perc[4],ci.event$perc[5],ci.nonevent$perc[4],ci.nonevent$perc[5],
              ci.overall$perc[4],ci.overall$perc[5])
  colnames(ci) <- c("Event NRI LB","Event NRI UB","Non-event NRI LB",
                    "Non-event NRI UB","Overall NRI UB","Overall NRI LB")
  ci
}

## Function to compute percentile NRI for estimates from model 2 vs. estimates from model 1
## Bin predicted risks based on evenly-spaced percentiles of risk prior to use in the previously
## stated censored-NRI formula
pct.nri <- function(risks.1,risks.2, outcome=NULL, event_time=NULL, censoring=NULL, fu_time=NULL, num.cat=5, confint=TRUE){
  # Create empty data frame with space for number of categories,
  # event, non-event, and overall NRI with corresponding 95% confidence intervals
  results <- data.frame("num.cat"=as.numeric(),"event"=as.numeric(),"nonevent" = as.numeric(), 
                        "overall"=as.numeric(), "e.lb"=as.numeric(), "e.ub"=as.numeric(),
                        "ne.lb"=as.numeric(), "ne.ub"=as.numeric(),
                        "ov.lb"=as.numeric(),"ov.ub"=as.numeric())
  ## Apply percentile-based NRI to a varied number of categories chosen by the user 
  for (i in 1:length(num.cat)){
    # Sort estimates into categories based on quantiles of risk
    risks1.bin <- cut(risks.1, breaks=quantile(risks.1,seq(0,1,length.out=num.cat[i]+1)),include.lowest = TRUE,labels=FALSE)
    risks2.bin <- cut(risks.2, breaks=quantile(risks.2,seq(0,1,length.out=num.cat[i]+1)),include.lowest = TRUE,labels=FALSE)
    ## Calculate NRI
    ci <- c(0,0,0,0,0,0)
    if(!is.null(outcome)){
      nri <- compute.NRI(risks1.bin, risks2.bin, outcome)
      if(confint){
        ## Calculate 95% bootstrap confidence intervals
        ci <- ci.nri(risks1.bin, risks2.bin, outcome)
      }
    } else {
      nri <- compute.cNRI(risks1.bin, risks2.bin, event_time, censoring, fu_time)
      if(confint){
        ## Calculate 95% bootstrap confidence intervals
        ci <- ci.cnri(risks1.bin, risks2.bin, event_time, censoring, fu_time)
      }
    } 
    # Store results
    results[i,] <- c(num.cat[i],  nri,ci)
  }
  results
}

# XL modified version of above
# The aim is to allow splitting uneuqal percentiles of risk scores
# I replace the num.cat with prob for specifying the probability cut-off points
pct.nri_XL <- function(risks.1,risks.2, outcome=NULL, event_time=NULL, censoring=NULL, 
                    fu_time=NULL, probs, confint=TRUE){
  # Create empty data frame with space for number of categories,
  # event, non-event, and overall NRI with corresponding 95% confidence intervals
  results <- data.frame("num.cat"=as.numeric(),"event"=as.numeric(),"nonevent" = as.numeric(), 
                        "overall"=as.numeric(), "e.lb"=as.numeric(), "e.ub"=as.numeric(),
                        "ne.lb"=as.numeric(), "ne.ub"=as.numeric(),
                        "ov.lb"=as.numeric(),"ov.ub"=as.numeric())
  ## Apply percentile-based NRI to a varied number of categories chosen by the user 
    # Sort estimates into categories based on specified probabilities 
    risks1.bin <- cut(risks.1, breaks=quantile(risks.1,probs=probs,na.rm=TRUE),include.lowest = TRUE,labels=FALSE,right=FALSE)
    risks2.bin <- cut(risks.2, breaks=quantile(risks.2,probs=probs,na.rm=TRUE),include.lowest = TRUE,labels=FALSE,right=FALSE)
    ## Calculate NRI
    ci <- c(0,0,0,0,0,0)
    if(!is.null(outcome)){
      nri <- compute.NRI(risks1.bin, risks2.bin, outcome)
      if(confint){
        ## Calculate 95% bootstrap confidence intervals
        ci <- ci.nri(risks1.bin, risks2.bin, outcome)
      }
    } else {
      nri <- compute.cNRI(risks1.bin, risks2.bin, event_time, censoring, fu_time)
      if(confint){
        ## Calculate 95% bootstrap confidence intervals
        ci <- ci.cnri(risks1.bin, risks2.bin, event_time, censoring, fu_time)
      }
    } 
    # Store results
    results[1,] <- c(length(probs)-1,nri,ci)
  results
}


## XL modified reclassification function from PredictABEL package
#https://github.com/cran/PredictABEL/blob/master/R/PredictABEL.R
# The aim is to produce only reclassification table and enable pretty label of table
# Also modify the text to our context
reclassification_table <-
  function(data,cOutcome,predrisk1,predrisk2, cutoff,cutoff_label) {
    
    c1 <- cut(predrisk1,breaks = cutoff ,include.lowest=TRUE,right= FALSE)
    c2 <- cut(predrisk2,breaks = cutoff ,include.lowest=TRUE,right= FALSE)
    
    # Relabelling 
    #levels <- sapply(2:length(cutoff)-1,function(i) paste0("[",cutoff[i],",",cutoff[i+1],")"))
    c1 <- factor(c1,labels = cutoff_label)
    c2 <- factor(c2,labels = cutoff_label)
      
    tabReclas <- table("Leicester risk score"=c1, "New risk score"=c2)
    cat(" _________________________________________\n")
    cat(" \n     Reclassification table    \n")
    cat(" _________________________________________\n")
    
    ta<- table(c1, c2, data[,cOutcome])
    
    cat ("\n Control (no T2D) \n  \n" )
    TabAbs <- ta[,,1]
    tab1 <- cbind(TabAbs, " % reclassified"= round((rowSums(TabAbs)-diag(TabAbs))/rowSums(TabAbs),2)*100)
    names(dimnames(tab1)) <- c("Leicester risk score", "New risk score")
    print(tab1)
    
    cat ("\n \n Cases (T2D) \n  \n" )
    TabPre <- ta[,,2]
    tab2 <- cbind(TabPre, " % reclassified"= round((rowSums(TabPre)-diag(TabPre))/rowSums(TabPre),2)*100)
    names(dimnames(tab2)) <- c("Leicester risk score", "New risk score")
    print(tab2)
    
    cat ("\n \n Combined Data \n  \n" )
    Tab <- tabReclas
    tab <- cbind(Tab, " % reclassified"= round((rowSums(Tab)-diag(Tab))/rowSums(Tab),2)*100)
    names(dimnames(tab)) <- c("Leicester risk score", "New risk score")
    print(tab)
    cat(" _________________________________________\n")
    
    # c11 <-factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
    # c22 <-factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))
    # 
    # x<-improveProb(x1=as.numeric(c11)*(1/(length(levels(c11)))),
    #                x2=as.numeric(c22)*(1/(length(levels(c22)))), y=data[,cOutcome])
    # 
    # 
    # y<-improveProb(x1=predrisk1, x2=predrisk2, y=data[,cOutcome])
    # 
    # cat("\n NRI(Categorical) [95% CI]:", round(x$nri,4),"[",round(x$nri-1.96*x$se.nri,4),"-",
    #     round(x$nri+1.96*x$se.nri,4), "]", "; p-value:", round(2*pnorm(-abs(x$z.nri)),5), "\n" )
    # 
    # cat(" NRI(Continuous) [95% CI]:", round(y$nri,4),"[",round(y$nri-1.96*y$se.nri,4),"-",
    #     round(y$nri+1.96*y$se.nri,4), "]", "; p-value:", round(2*pnorm(-abs(y$z.nri)),5), "\n" )
    # 
    # cat(" IDI [95% CI]:", round(y$idi,4),"[",round(y$idi-1.96*y$se.idi,4),"-",
    #     round(y$idi+1.96*y$se.idi,4), "]","; p-value:", round(2*pnorm(-abs(y$z.idi)),5), "\n")
  }



