library(stringr)
options("scipen"=100)

# Run chunks of R code in arbitrary working directory
# From Hadley Wickham
# https://github.com/yihui/knitr/issues/38
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))
  
  force(code)
}

Percentile <- function(x,right=FALSE,
                       probs=c(0,0.01,0.05,0.1,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1),
                       labels=c("<1%","1-5%","5-10%","10-20%","20-40%","40-60%","60-80%","80-90%","90-95%","95-99%",">99%")){
  cut(x,breaks=c(quantile(x,probs=probs,na.rm=TRUE)),
      include.lowest=TRUE,
      right=right,
      labels=labels)
  
}


pretty_dp <- function(x, dp=0, pct=FALSE, comma=FALSE){
  if(pct){x <- 100*x}
  if(comma){
    format(round(x, dp), digits=dp, nsmall=dp, big.mark=",") %>% trimws
  } else {
    format(round(x, dp), digits=dp, nsmall=dp) %>% trimws
  }
}

pretty_confint <- function(lci, uci, dp, pct=FALSE){
  paste0("(", pretty_dp(x=lci, dp=dp, pct=pct), ", ", pretty_dp(x=uci, dp=dp, pct=pct), ")")
}

pretty_pval <- function(p, cutoff=0.001, string="<0.001", dp=3){
  ifelse(p<cutoff, string, pretty_dp(p, dp))
}

lower <- function(x){
  paste0(tolower(substring(x, 1,1)), substring(x, 2))
}

upper <- function(x){
  paste0(toupper(substring(x, 1,1)), substring(x, 2))
}

prettyfunc <- function(x, pnames=list(), upper=FALSE, bold=FALSE, flist=c()){
  out <- x
  if(x %in% names(pnames)){
    out <- pnames[[x]]
    if(upper){
      out <- upper(out)
    }
    if(bold){
      out <- paste0("**", out, "**")
    }
  }
  if(x %in% flist){
    if(!exists("footnote_no")){
      footnote_no <<- 1
    }
    out <- paste0(out, "^", footnote_no, "^")
    footnote_no <<- footnote_no + 1
  }
  return(out)
}

diagcollist <- function(colstring, sep="", ncols) {
  x <- 0:ncols
  colstr <- paste0("`",paste0(colstring, sep, x, collapse="`, `"),"`")
  return(colstr)
}

# Print numbers and proportions for factors, median and IQR or mean and 95% CI for continuous variables
# Optionally provide p-values from chi-squared (categorical) and t-test (continuous)
descriptivetable <- function(df, varlist, contavg='mean', assocvar=NULL, pretty_names=list(), footnote_list=c()){
  if(!exists("footnote_no")){footnote_no <<- 1} # Note use of <<- instead of <- to assign this globally
  outtable <- c()
  for(var in varlist){
    if(is.factor(df[[var]])){ # Categorical variables (factors) need a row per level, n's and %'s
      n <- table(df[[var]], useNA='ifany')
      pct <- pretty_dp(prop.table(n), dp=1, pct=TRUE)
      variable <- c(prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list), rep(NA, dim(n)-1))
      levels <- names(n)
      if(!is.null(assocvar)){
        tab <- table(df[[assocvar]], df[[var]])
        chi <- chisq.test(tab)
        pval <- c(ifelse(chi$p.value<0.001, "<0.001", round(chi$p.value,3)))
        outtable <- rbind(outtable, 
                          c(var, paste0("**", variable, "**"), "", "", pval), 
                          cbind(paste0(var, levels), levels, n, pct, ""))
      } else{
        outtable<- rbind(outtable, 
                         c(var, paste0("**", variable, "**"), "", ""), 
                         cbind(paste0(var, levels), levels, n, pct))
      }
    } else { # Continuous variables need the mean (and SD) or median (and IQR)
      if(contavg=="mean"){
        n <- pretty_dp(mean(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- pretty_dp(sd(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        variable <- paste0("Mean ", prettyfunc(var, pretty_names, upper=FALSE, flist=footnote_list), " (SD)")
      } else if (contavg=="median"){
        n <- pretty_dp(median(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        IQR <- pretty_dp(quantile(df[[var]], na.rm=TRUE), dp=1, comma=TRUE)
        pct <- paste0("(", IQR[2], "-", IQR[4], ")")
        variable <- paste0("Median ", prettyfunc(var, pnames=pretty_names, upper=FALSE, flist=footnote_list), " (IQR)")
      } else if(contavg=="n"){
        n <- nrow(df[!is.na(df[[var]]),])
        pct <- NA
        variable <- prettyfunc(var, pnames=pretty_names, upper=TRUE, flist=footnote_list)
      }
      if(!is.null(assocvar)){
        tt <- t.test(df[[var]][df[[assocvar]]==TRUE], df[[var]][df[[assocvar]]==FALSE])
        pval <- pretty_pval(tt$p.value)
        outtable <- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct, pval))
      } else {
        outtable<- rbind(outtable, cbind(var, paste0("**", variable, "**"), n, pct))
      }
    }
  }
  rownames(outtable) <- c()
  if(!is.null(assocvar)){
    colnames(outtable) <- c("IDcol", "Variable", "n", "%", "p")
  } else {
    colnames(outtable) <- c("IDcol", "Variable", "n", "%")
    }
  outdf <- as.data.frame(outtable, stringsAsFactors=FALSE)
  return(outdf)
}

# Prettyprint the results from a Cox model
# To use this, 
# model <- coxph(Surv(time_to_dementia, dementia_status) ~ age, data=data)
# kable(printcoxresults(model), caption="")
printcoxresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE,forplot=FALSE,print_beta=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  conf <- summ$conf.int
  
  regression <- data.frame(
    IDcol=rownames(coeff),
    beta=pretty_dp(coeff[,1], dp=2),
    HR=pretty_dp(coeff[,2], dp=2), # HR
    HR_num=coeff[,2],
    LCI=conf[,3],
    UCI=conf[,4],
    CI=pretty_confint(conf[,3], conf[,4], dp=2), # 95% CI
    beta_CI=pretty_confint(coeff[,1]-(1.96*coeff[,3]),
                           coeff[,1]+(1.96*coeff[,3]),
                           dp=2),
    p=pretty_pval(coeff[,5]), # p-value
    stringsAsFactors=FALSE
  )
  
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$beta[is.na(results$beta) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "Reference"
    results$HR[is.na(results$HR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "Reference"
    results$HR_num[is.na(results$HR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$beta[is.na(results$beta) & !is.na(results$Coefficient)] <- "Reference"
    results$HR[is.na(results$HR) & !is.na(results$Coefficient)] <- "Reference"
    results$HR_num[is.na(results$HR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    if(print_beta==FALSE){
    results <- results[,c(coeffcols, "HR", "CI", "p")]
    names(results) <- c(coeffcols, "HR", "95% CI", "p")
    } else {
      results <- results[,c(coeffcols, "beta", "beta_CI", "HR", "CI", "p")]
      names(results) <- c(coeffcols, "Beta", "95% CI (Beta)","HR", "95% CI (HR)", "p")
    }
  } else {
    results <- results[,c(coeffcols, "HR_num", "LCI", "UCI")]
  }
  
  
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
}


# Pretty print the results from a logistic regression model
printlogresults <- function(df, varlist, modeloutput, pretty_names=list(), onecol=FALSE, IDcol=FALSE,forplot=FALSE,print_beta=FALSE){
  require(dplyr)
  
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names, onecol=onecol)
  }
  coeffnames <- do.call(rbind, coefflist)
  
  summ <- summary(modeloutput)
  coeff <- summ$coefficients
  
  regression <- data.frame(
    IDcol=rownames(coeff),
    beta=pretty_dp(coeff[,1], dp=2),
    OR=pretty_dp(exp(coeff[,1]), dp=2), # OR
    OR_num=exp(coeff[,1]),
    LCI=exp(coeff[,1]-(1.96*coeff[,2])),
    UCI=exp(coeff[,1]+(1.96*coeff[,2])),
    beta_CI=pretty_confint(coeff[,1]-(1.96*coeff[,2]),
                           coeff[,1]+(1.96*coeff[,2]),
                           dp=2),
    CI=pretty_confint(exp(coeff[,1]-(1.96*coeff[,2])),
                      exp(coeff[,1]+(1.96*coeff[,2])),
                      dp=2), # 95% CI
    p=pretty_pval(coeff[,4]), # p-value
    stringsAsFactors=FALSE
  )
  
  results <- left_join(coeffnames, regression, by="IDcol")
  if(onecol){
    results$beta[is.na(results$beta) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "Reference"
    results$OR[is.na(results$OR) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- "1"
    results$OR_num[is.na(results$OR_num) & (results$IDcol != results$Coefficient & !is.na(results$Coefficient))] <- 1
  } else {
    results$beta[is.na(results$beta) & !is.na(results$Coefficient)] <- "Reference"
    results$OR[is.na(results$OR) & !is.na(results$Coefficient)] <- "1"
    results$OR_num[is.na(results$OR_num) & !is.na(results$Coefficient)] <- 1
  }
  
  coeffcols <- colnames(coeffnames)
  if(IDcol==FALSE){
    coeffcols <- coeffcols[coeffcols != "IDcol"]
  }
  
  if(forplot==FALSE){
    if(print_beta==FALSE){
      results <- results[,c(coeffcols, "OR", "CI", "p")]
      names(results) <- c(coeffcols, "OR", "95% CI", "p")
    }else{
      results <- results[,c(coeffcols, "beta", "beta_CI", "p")]
      names(results) <- c(coeffcols, "Beta", "95% CI", "p")
    }
    
  } else {
    results <- results[,c(coeffcols, "OR_num", "LCI", "UCI")]
  }
  
  
  
  rownames(results) <- NULL
  # https://www.r-bloggers.com/regression-on-categorical-variables/
  return(results)
  
}


# Round to the nearest m
mround <- function(x, base){
  base*round(x/base)
}


# Make a pretty proportion table
# To use this,
# tab <- table(data$VIhyp, data$prevHBP, useNA='ifany')
# kable(propped(tab), caption="")
propped <- function(table, margin=NULL) {
  prop <- round(100*prop.table(table, margin=margin),2)
  tabsums <- addmargins(table)
  dimt <- dim(table)
  for(i in c(1:dimt[1])) {
    if(length(dimt)>1){
      for(j in c(1:dimt[2])) {
        tabsums[i,j] <- paste0(table[i,j], " (", prop[i,j], "%)")
      }
    }
    else{
      tabsums[i] <- paste0(table[i], " (", prop[i], "%)") 
    }
  }
  return(tabsums)
}

preparecoefflist_1col <- function(df, varname, pretty_names=list()){
  # Detect whether has 2-way interaction first
  if(str_detect(varname,":")){
    
    var1=strsplit(varname,":")[[1]][1];var2=strsplit(varname,":")[[1]][2]
    
    pretty_varname1 <- prettyfunc(var1, pnames=pretty_names, bold=FALSE, upper=TRUE)
    pretty_varname2 <- prettyfunc(var2, pnames=pretty_names, bold=FALSE, upper=TRUE)
    
    int_name <-paste0("**",pretty_varname1,"*",pretty_varname2,"**")
    # detect variable type before and after :
    
    if(is.factor(df[[var1]])&is.factor(df[[var2]])){
      # Levels would be combinations of levels of each var (except ref level)
      
      ref_level<-paste(levels(df[[var1]])[1],levels(df[[var2]])[1])
      rest_level<-expand.grid(levels(df[[var1]])[-1],levels(df[[var2]])[-1])
      
      levels<-c(ref_level,paste(rest_level$Var1,rest_level$Var2))  
      variable<-c(int_name,levels)
      
      
      coeffname<-c(int_name,paste0(var1,c(levels(df[[var1]])[1],as.character(rest_level$Var1)),
                                   ":",var2,c(levels(df[[var2]])[1],as.character(rest_level$Var2))))
      
    }else if(is.numeric(df[[var1]])&is.numeric(df[[var2]])){
      variable<-int_name
      coeffname<-varname
      
    }else if(is.factor(df[[var1]])&is.numeric(df[[var2]])){
      levels<-levels(df[[var1]])
      variable <- c(int_name, levels)
      coeffname <- c(int_name, paste0(var1, levels,":",var2))
      
    }else if(is.numeric(df[[var1]])&is.factor(df[[var2]])){
      levels<-levels(df[[var2]])
      variable <- c(int_name, levels)
      coeffname <- c(int_name, paste0(var1,":",var2,levels))
      
    }else{warning("Please check type of variables in interaction")}
    
  }else{
    pretty_varname <- prettyfunc(varname, pnames=pretty_names, bold=TRUE, upper=TRUE)
    if(is.factor(df[[varname]])){
      if(is.ordered(df[[varname]])){
        poly <- length(levels(df[[varname]]))
        levels <- c("Ref", ".L", ".Q", ".C")
        if(poly > 4){
          powers <- c(4:(poly-1))
          levels <- c(levels, paste0("^", powers))
        }
        levels <- levels[1:poly]
      } else {
        levels <- levels(df[[varname]])
      }
      variable <- c(pretty_varname, levels)
      coeffname <- c(pretty_varname, paste0(varname, levels))
    } else {
      variable <- pretty_varname
      coeffname <- varname
    }
    
  }
  output <- data.frame(coeffname, variable, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient")
  rownames(output) <- NULL
  
  return(output)
}

preparecoefflist_2col <- function(df, varname, pretty_names=list()){
  # Detect whether has 2-way interaction first
  if(str_detect(varname,":")){
    
    var1=strsplit(varname,":")[[1]][1];var2=strsplit(varname,":")[[1]][2]
    
    pretty_varname1 <- prettyfunc(var1, pnames=pretty_names, bold=FALSE, upper=TRUE)
    pretty_varname2 <- prettyfunc(var2, pnames=pretty_names, bold=FALSE, upper=TRUE)
    
    int_name <-paste0(pretty_varname1,"*",pretty_varname2)
    # detect variable type before and after :
    
    if(is.factor(df[[var1]])&is.factor(df[[var2]])){
      # Levels would be combinations of levels of each var (except ref level)
      
      ref_level<-paste(levels(df[[var1]])[1],levels(df[[var2]])[1])
      rest_level<-expand.grid(levels(df[[var1]])[-1],levels(df[[var2]])[-1])
      
      levels<-c(ref_level,paste(rest_level$Var1,rest_level$Var2))  
      variable<-c(int_name,rep(NA,length(levels)-1))
      
      
      coeffname<-paste0(var1,c(levels(df[[var1]])[1],as.character(rest_level$Var1)),
                        ":",var2,c(levels(df[[var2]])[1],as.character(rest_level$Var2)))
      
    }else if(is.numeric(df[[var1]])&is.numeric(df[[var2]])){
      levels<-NA
      variable<-int_name
      coeffname<-varname
      
    }else if(is.factor(df[[var1]])&is.numeric(df[[var2]])){
      levels<-levels(df[[var1]])
      variable <- c(int_name, rep(NA,length(levels)-1))
      coeffname <- paste0(var1, levels,":",var2)
      
    }else if(is.numeric(df[[var1]])&is.factor(df[[var2]])){
      levels<-levels(df[[var2]])
      variable <- c(int_name, rep(NA,length(levels)-1))
      coeffname <- paste0(var1,":",var2,levels)
      
    }else{warning("Please check type of variables in interaction")}
    
  }else{
    pretty_varname <- prettyfunc(varname, pnames=pretty_names, upper=TRUE)
    if(is.factor(df[[varname]])){
      if(is.ordered(df[[varname]])){
        poly <- length(levels(df[[varname]]))
        levels <- c("Ref", ".L", ".Q", ".C")
        if(poly > 4){
          powers <- c(4:(poly-1))
          levels <- c(levels, paste0("^", powers))
        }
        levels <- levels[1:poly]
      } else {
        levels <- levels(df[[varname]])
      }
      variable <- c(pretty_varname,
                    rep(NA, length(levels)-1))
      coeffname <- paste0(varname,levels)
    } else {
      levels <- NA
      variable <- pretty_varname
      coeffname <- varname
    }
  }
  
  output <- data.frame(coeffname, variable, levels, stringsAsFactors=FALSE)
  colnames(output) <- c("IDcol", "Coefficient", "Levels")
  rownames(output) <- NULL
  return(output)
}



preparecoefflist <- function(onecol=FALSE, ...){
  if(onecol) {
    preparecoefflist_1col(...)
  } else {
    preparecoefflist_2col(...)
  }
}


regressiontable <- function(df, outcome, varlist, regresstype, adjvarlist=c("agegrp", "gender"), pretty_names=list(), IDcol=TRUE){
  coefflist <- list()
  # Prepare the list of coefficients - variables and levels for factors or blanks for continuous
  for(var in varlist){
    coefflist[[var]] <- preparecoefflist(df=df, varname=var, pretty_names=pretty_names)
  }
  
  if(regresstype=="univariable"){
    modellist <- list()
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    # Vertically concatenate all the pretty outputs into one output table
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="adjusted"){
    modellist <- list()
    
    # Run the regressions for age and gender separately to go on top of the table
    for(adjvar in adjvarlist){
      coeffnames <- preparecoefflist(df=df, varname=adjvar)
      
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"))
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[adjvar]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    
    # Putting age or gender in the regression twice would confuse it, so make sure they're not in the varlist
    varlist <- varlist[!varlist %in% adjvarlist]
    
    for(var in varlist){
      coeffnames <- coefflist[[var]]
      
      # Prepare the formula and pass it to the model
      formula <- paste0(outcome, " ~ ", paste(adjvarlist, collapse="+"), "+", var)
      model <- glm(formula, data=df, family="binomial")
      
      # Add the pretty-formatted outputs to the list
      modellist[[var]] <- printlogresults(model, coeffnames, IDcol=IDcol)
    }
    outdf <- do.call(rbind, modellist)
    
  } else if (regresstype=="multivariable"){
    coeffnames <- do.call(rbind, coefflist)
    formula <- paste0(outcome, " ~ ", paste(varlist, collapse=" + "))
    model <- glm(formula, data=df, family="binomial")
    outdf <- printlogresults(model, coeffnames, IDcol=IDcol)
  }
  
  rownames(outdf) <- NULL
  return(outdf)
}


# Convert output of etmCIF to ggplot acceptable input
#https://stackoverflow.com/questions/56784714/write-a-summary-to-as-data-frame-for-use-in-ggplot-r
etm_to_df <- function(object, ci.fun = "cloglog", level = 0.95, ...) {
  l.X <- ncol(object$X)
  l.trans <- nrow(object[[1]]$trans)
  res <- list()
  for (i in seq_len(l.X)) {
    temp <- summary(object[[i]], ci.fun = ci.fun, level = level)
    res[[i]] <- data.table::rbindlist(
      temp[object$failcode + 1], idcol = "CIF"
    )[, CIF := names(object)[i]]
  }
  do.call(rbind, res)
}

# Plot cumulative incidence function (with competing risk)
cuminc_plot<-function(data,group,start="TEU_BaC_AgeAtRec",end="TEU_BrCa_age",cr_status="cr_status",status="TEU_BrCa_status",failcode=1,smooth=FALSE,
                      title,legend.title,xlim=c(40,79),xlab="Age in years", ylim,
                      caption="Note: Age was truncated at 79 years old;\n CR cases stands for non-BrCa death cases"){
  
  #' Create cumulative incidence plot with competing risk (can adapt left-truncation, smoothing)
  #' Basically a wrapper of etm::etmCIF
  #' https://cran.r-project.org/web/packages/etm/vignettes/etmCIF_tutorial.pdf
  #'
  #' @param data The data
  #' @param group The group of interest, specify 1 if not interested plots by groups
  #' @param start Entry time if left-truncation exists, specify NULL if no left-truncation
  #' @param end Follow up time 
  #' @param cr_status competing risk event indicator (0=censored,1=event of interest,2=competing risk event)
  #' @param status follow up status (i.e.no competing risk indicator) (0=censored,1=event of interest)
  #' @param failcode Indicates the failure type of interest for plotting (default 1)
  #' @param title Title of the plot
  #' @param legend.title Legend title
  #' @param xlim limit of x
  #' @param xlab Label of x-axis
  #' @param ylim limit of y
  #' @param smooth Show smoothed curve using cubic spline technique with 10 knots (default FALSE)
  #' 
  #' @return Cumulative incidence plot
  
  
  # Start time: TEU_BaC_AgeAtRec; Stop time: TEU_BrCa_age; What counts as events: cr_status!=0
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", cr_status,"!= 0)~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", cr_status,"!= 0)~",group))}
  
  
  # etype specifies the type of events; failcode=1 means we are only interested in plotting cr_status==1 which is BrCa incident case
  fit<-etmCIF(formula,data=data,etype = cr_status,failcode=failcode)
  
  # Transform to ggplot acceptable format
  res<-etm_to_df(fit)%>%mutate(CIF=factor(CIF,levels=paste0(group,"=",levels(data[[group]])),labels = levels(data[[group]])))
  
  # Number of brca cases and non-brca dth cases within each PRS category
  labels=data%>%group_by(!!sym(group))%>%summarise(n=sum(!!sym(status)),nonbr=(sum(!!sym(cr_status))-sum(!!sym(status)))/2)%>%na.omit()
  
  if(smooth==FALSE){
    plot<-ggplot(res, aes(time, P)) +
      #geom_ribbon(aes(ymin = lower, ymax = upper, fill = CIF), alpha = 0.2) +
      geom_line(aes(color = CIF),linewidth=1) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      labs(x=xlab,y="Cumulative incidence",title = title,caption = caption)+
      scale_color_discrete(name=legend.title,
                           breaks=unique(res$CIF),
                           labels=paste(labels[[group]],"(",labels$n,"events,", labels$nonbr,"CR cases)"))+
      theme_bw()
    
  }else{
    
    # Smoothing using spline
    plot<-ggplot(res)+geom_spline(aes(x=time,y=P,colour=CIF),nknots = 10) +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim) +
      labs(x=xlab,y="Cumulative incidence",title = title,caption = caption)+
      scale_color_discrete(name=legend.title,
                           breaks=unique(res$CIF),
                           labels=paste(labels[[group]],"(",labels$n,"events,", labels$nonbr,"CR cases)"))+
      theme_bw()
    
  }
  
  return(plot)
}

# Plot cumulative incidence function (without competing risk)
cuminc_nocr_plot<-function(data,start,end,status,group,title,legend.title,xlab="Age in years",xlim=c(40,80)){
  
  if(!is.null(start)){
    formula<-as.formula(paste0("Surv(",start,",",end,",", status,")~",group))
  }else{formula<-as.formula(paste0("Surv(",end,",", status,")~",group))}
  
  
  fit<-surv_fit(formula,data=data)
  
  labels=data%>%group_by(!!sym(group))%>%summarise(n=sum(!!sym(status)))%>%na.omit()
  
  ggsurv<-ggsurvplot(fit,data=data,fun="event",title=title,
                     xlab=xlab,ylab="Cumulative incidence",#risk.table = TRUE,
                     xlim=xlim,break.x.by=5,
                     censor.size=1,size=1,ggtheme = theme_bw(),
                     legend.labs=paste(labels[[group]],"(",labels$n,"events)"),legend.title=legend.title)
  
  plot=ggsurv$plot+theme(legend.position="left")
  
  return(plot)
}


# Process analysis table of PRSonDiab for pretty presentation
# The main thing is to pivot the original printlogresults with PRS (Q1,..Q5) as the header
diab_tab<-function(df=data,varlist,modeloutput,interaction_var=NULL,analysis_name,pvalue){
  
  if(is.null(interaction_var)){
    # If the model doesn't have interaction term, we use our internal function printxxxresults
    if(class(modeloutput)[1]=="coxph"){
      tab<-printcoxresults(df,varlist ,pretty_names = pretty_names,modeloutput)
      # Hacky: Temporarily rename the column HR to OR to avoid breaking the code below
      tab=tab%>%rename(OR=HR)
      
    }else{
      # After model fitting, get the pretty table out
      tab<-printlogresults(df,varlist ,pretty_names = pretty_names,modeloutput)
    }
    
  }else{
    # If the model has interaction term, we use Publish package
    int_tab<-publish(modeloutput,print = FALSE,ci.format="(l,u)")
    
    if(class(modeloutput)[1]=="coxph"){
      # Hacky: Temporarily rename the column HR to OR to avoid breaking the code below
      int_tab$regressionTable=int_tab$regressionTable%>%rename(OddsRatio=HazardRatio)
    }
    
    # Process table output to match our printxxxresults format
    
    tab<-int_tab$regressionTable%>%
      select(!Units)%>%
      # Only keep the comparisons of interest (eg. interaction_var=TEU_BSM_BMI)
      # (e.g. HR of PRS within each category of BMI)
      filter(grepl(paste0('^',interaction_var),Variable))%>%
      # Separate out the Variable column
      # https://tidyr.tidyverse.org/reference/separate.html
      separate(Variable,c("Strata","Levels"),":",extra = "merge")%>%
      # Keep only content within the bracket
      mutate_at(c("Strata","Levels"),~gsub("[\\(\\)]", "", regmatches(., gregexpr("\\(.*?\\)", .))))%>%
      # Remove "vs Q1: lowest"
      mutate(Levels=str_remove(Levels," vs Q1: lowest"))%>%
      rename(OR=OddsRatio,`95% CI`=CI.95)
    
  }

  
  # Some processing
  tab_aft<-tab%>%
    mutate(`95% CI`=replace_na(`95% CI`," "))%>%
    mutate(OR_CI=paste0(OR," ",`95% CI`))%>%
    select(any_of(c("Strata","Levels","OR_CI")))%>%
    # transpose
    spread(Levels,OR_CI)%>%
    # Add analysis name at the beginning
    add_column(Analysis=analysis_name,.before = 1)%>%
    add_column(p=pvalue)
  
  if(is.null(interaction_var)){
    tab_aft<-tab_aft%>%
      add_column(Strata="",.after = "Analysis")
  }else{ # If there is interaction term 
    tab_aft<-tab_aft%>%
      add_column(`Q1: lowest`="1",.after="Strata")
    
    # Display interaction according to levels in the data
    tab_aft=tab_aft[match(levels(df[[interaction_var]]),tab_aft$Strata),]
  }
  
  return(tab_aft)
  
}

# Leicester risk score system
# https://pubmed.ncbi.nlm.nih.gov/20653746/
var_to_score <- function(x) {
  recode(x,
         "40-49" = 0,
         "50-59" = 5,
         "60-69" = 9,
         "Male"  = 1,
         "Female"= 0,
         "White"=0,
         "Other"=6,
         "No family history of Diab" = 0,
         "Family history of Diab" = 5,
         "<90" = 0,
         "90-99" = 4,
         "100-109" = 6,
         ">=110" = 9,
         "<25" = 0,
         "25-29" = 3,
         "30-34" = 5,
         ">=35" = 8,
         "No" = 0,
         "Yes" = 5,
         .default = NaN
  )
}

# Leicester score betas to compute probability
var_to_beta <- function(x) {
  recode(x,
         "40-49" = 0,
         "50-59" = 0.5346095,
         "60-69" = 0.9403614,
         "Male"  = 0,
         "Female"= 0.0883173,
         "White"=0,
         "Other"=0.569434,
         "No family history of Diab" = 0,
         "Family history of Diab" = 0.465857,
         "<90" = 0,
         "90-99" = 0.4261962,
         "100-109" = 0.5632965,
         ">=110" = 0.8590326,
         "<25" = 0,
         "25-29" = 0.260582,
         "30-34" = 0.4528574,
         ">=35" = 0.7528332,
         "No" = 0,
         "Yes" = 0.458373,
         .default = NaN
  )
}

Sens_Spec<-function(threshold,data=rs_train,risk_score,name,outcome="TEU_T2DM_status"){
  
  df_total=data.frame()
  
  for (i in threshold) {
    
    predicted_values<-ifelse(data[[risk_score]]>=i,1,0)
    actual_values<-data[[outcome]]
    conf_matrix<-table(predicted_values,actual_values)
    
    Sens<-sensitivity(conf_matrix,positive = "1")
    Spec<-specificity(conf_matrix,negative="0")
    
    df<-data.frame(score=name,threshold=paste0(">= ",i),sensitivity=pretty_dp(Sens,2),specificity=pretty_dp(Spec,2))
    
    df_total <- rbind(df_total,df)
    
  }
  
  return(df_total)
}

AUC_CI<-function(df,status="TEU_T2DM_status",score,method="delong"){
  
  roccurve<-pROC::roc(df[[status]]~df[[score]],quiet=TRUE)
  
  AUCs_train<-pROC::ci.auc(roccurve,quiet=TRUE,method=method)
  
  result<-paste0(pretty_dp(AUCs_train[2],3),pretty_confint(AUCs_train[1],AUCs_train[3],dp=3))
  
  return(result)
  
}

Reclassification_bar<-function(data,risk_cat=diab_rs_binary,name="Leicester risk score"){
  
  tab<-data%>%
    mutate(`Risk category`=factor(ifelse({{risk_cat}}==1,"Low","High"),levels = c("Low","High")))%>%
    group_by(`Risk category`)%>%
    summarise(n_cases=sum(TEU_T2DM_status),N=n())%>%
    mutate(perc_cases=(n_cases/N)*100,
           n_controls=N-n_cases,
           perc_controls=(n_controls/N)*100)%>%
    mutate(score=name)
  
  return(tab)
}


# Produce dataframe for calibration plot
Calibration_df<-function(data=rs_train,outcome="Surv(TEU_T2DM_time,TEU_T2DM_status)",predRisk="Leicrs_prob",groups=10,times=10,format="wide"){
  
  #' @param data The data
  #' @param predRisk The column name that records the predicted probability
  #' @param groups Categorise ppl into how many groups (usually 10 because decile)
  #' @param times Observed risk at what time (default is 10-year risk) We use survival outcome here
  #' @param format Produce output table in wide or long format. Use wide to get the standard calibration plot. Use long to plot RP's format. 
  
  # Categorise participants into different decile groups
  # For internal consistency, Do not use cut2 function.
  data<-data%>%mutate(
    pred_deciles=Percentile((!!sym(predRisk)),probs=seq(0, 1, 1/groups),labels = c(1:groups),right=TRUE))
  
  # If we use the actual probability from the logistic regression model
  pred<-data%>%group_by(pred_deciles)%>%summarise(value=mean((!!sym(predRisk))))%>%
    mutate(group=as.numeric(pred_deciles),variable="Pred")%>%
    select(group, variable, value)
  
  # Observed risk in each decile
  fit<-survfit(as.formula(paste0(outcome,"~ pred_deciles")),data=data)
  
  sum10<-summary(fit,times = times)
  
  sum10_df <- data.frame(group=sum10$strata,
                         surv = sum10$surv
  )
  
  obs<-sum10_df%>%
    mutate(group=as.numeric(str_remove(group,"pred_deciles=")),
           value=1-surv,
           variable="Obs")%>%
    select(group,variable,value)
  
  if(format=="wide"){
    pred_obs_wide=inner_join(pred,obs,by="group",suffix=c(".pred",".obs"))
    return(pred_obs_wide)
    
  }else{
    
    pred_obs_long<-rbind(pred,obs)
    return(pred_obs_long)
  }
}
  
  
  
  
  
  
  
  
  
  
  

