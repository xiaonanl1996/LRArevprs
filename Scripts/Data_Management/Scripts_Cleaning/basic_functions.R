# Jennifer Collister
# 30/03/2020
# Create functions 
library(data.table)
library(tools)
library(readxl)
library(qs)


# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}

read.csv_kdrive <- function(filename, maxErrors=2, sleep=20) {
  attempts <- 0
  retval <- NULL
  
  while(is.null(retval) && attempts < maxErrors) {
    if(attempts > 0) {Sys.sleep(sleep)}
    attempts <- attempts + 1
    retval <- tryCatch(suppressWarnings(
      read.csv(filename)),
      error = function(cond) {
        message(paste0("Error message: ", cond))
        if(attempts < maxErrors) {message(paste0("Trying again in ", sleep, " seconds..."))}
        return(NULL)
      }
    )
  }
  if(is.null(retval)){
    message("Failed to read file. Either it does not exist (typo?) or you are currently disconnected from the network drives")
  } else if (attempts>1) {
    message("Succeeded!")
  }
  
  return(retval)
}

read.xlsx_kdrive <- function(filename,col_types=NULL, sheet=NULL, maxErrors=2, sleep=20) {
  attempts <- 0
  retval <- NULL
  
  while(is.null(retval) && attempts < maxErrors) {
    if(attempts > 0) {Sys.sleep(sleep)}
    attempts <- attempts + 1
    retval <- tryCatch(suppressWarnings(
      read_excel(filename,col_types = col_types, sheet = sheet)),
      error = function(cond) {
        message(paste0("Error message: ", cond))
        if(attempts < maxErrors) {message(paste0("Trying again in ", sleep, " seconds..."))}
        return(NULL)
      }
    )
  }
  if(is.null(retval)){
    message("Failed to read file. Either it does not exist (typo?) or you are currently disconnected from the network drives")
  } else if (attempts>1) {
    message("Succeeded!")
  }
  
  return(retval)
}

cache.file <- function(filename, FUN, ...) {
  file <- evalWithMemoization(
    FUN(filename, ...),
    key = c(filename, file.info(filename)$mtime)
  )
}
# Formatting of existing UKB variables

FN_id <- function(x){x}

FN_unorder <- function(x){factor(x, ordered=FALSE)}

FN_factor <- function(levelorder, ordered=FALSE){
  function(x){
    factor(x, levels=levelorder, ordered=ordered)
  }
}

FN_toNumeric <- function(x) {
  as.numeric(x)
}

FN_toDate <- function(x){
  as.Date(x, origin=as.Date("1970-01-01"))
}

# Derived variables

FN_labelfactor <- function(levels, labels, recodeNA=NULL){
  function(x){
    y <- factor(x, levels=levels, labels=labels)
    if(!is.null(recodeNA)){
      levels(y) <- unique(c(levels(y), recodeNA))
      y[is.na(y)] <- recodeNA
    }
    return(y)
  }
}

FN_average <- function(colnames, na.rm=TRUE){
  function(data){
    rowMeans(data[,colnames], na.rm)
  }
}

FN_MYtoDate <- function(day, monthField, yearField, format="%d%B%Y"){
  function(data){
    as.Date(paste0(as.character(day), as.character(data[[monthField]]), as.character(data[[yearField]])), format)
  }
}


FN_buckets <- function(breaks, labels=NULL, right=TRUE){
  function(x){
    cut(x, breaks=breaks, labels=labels, right=right)
  }
}

FN_quantiles <- function(quant=4, labels=NULL, na.rm=TRUE){
  function(x){
    # if(anyNA(x)){warning("This vector contains NA values")}
    quantiles <- quantile(x, probs=seq(0, 1, 1/quant), na.rm=na.rm)
    if(is.null(labels)){
      labels <- c("Q1: lowest", paste0("Q", seq(2, quant-1)), paste0("Q", quant, ": highest"))
      }
    test <- cut(x, breaks=quantiles, labels=labels, right=TRUE, include.lowest=TRUE)
  }
}

FN_FamHist <- function(conditions, label){
  function(data){
    y <- apply(data[,c(grep("FaH_FatherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_MotherIll.0.", colnames(data), fixed=TRUE),
                       grep("FaH_SibIll.0.", colnames(data), fixed=TRUE)
    )
    ], 1, function(x) any(x %in% conditions)
    )
    y <- factor(as.numeric(y), levels=c(0,1), 
                labels=c(paste0("No family history of ", label), paste0("Family history of ", label)))
    return(y)
  }
}

FN_HMHmeds_any <- function(data){
  # Combine the first medication field across males and females
  medcombine <- coalesce(data[["HMH_MedCholBPDiabHorm.0.0"]], data[["HMH_MedCholBPDiab.0.0"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  medlist <- c("Cholesterol lowering medication",
               "Blood pressure medication",
               "Oral contraceptive pill or minipill",
               "Hormone replacement therapy",
               "Insulin"
               )
  y <- dplyr::case_when(
    is.na(medcombine) ~ "Unanswered",
    medcombine == "None of the above" ~ "No",
    medcombine == "Do not know" ~ "Do not know",
    medcombine == "Prefer not to answer" ~ "Prefer not to answer",
    medcombine %in% medlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer", "Unanswered"))
  return(y)
}

# XL add: 17/11/2020
FN_HMHmeds_any_raw <- function(data){
  # Combine the first medication field across males and females
  medcombine <- coalesce(data[["HMH_MedCholBPDiabHorm.0.0"]], data[["HMH_MedCholBPDiab.0.0"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  medlist <- c("Cholesterol lowering medication",
               "Blood pressure medication",
               "Oral contraceptive pill or minipill",
               "Hormone replacement therapy",
               "Insulin"
  )
  y <- dplyr::case_when(
    is.na(medcombine) ~ NA_character_,
    medcombine == "None of the above" ~ "No",
    medcombine == "Do not know" ~ "Do not know",
    medcombine == "Prefer not to answer" ~ "Prefer not to answer",
    medcombine %in% medlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer"))
  return(y)
}


FN_HMHmeds_type <- function(medtype, string){
  function(data){
    x <- FN_HMHmeds_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested medication across the columns
    y <- apply(data[,c(grep("HMH_MedCholBPDiab.0.", colnames(data), fixed=TRUE),
                       grep("HMH_MedCholBPDiabHorm.0.", colnames(data), fixed=TRUE))], 1, function(x) any(x==medtype))
    # And incorporate the info on whether this participant is taking any other medication
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }
}

FN_Vascular_any <- function(data) {
  # Combine the vascular condition columns
  vcon <- coalesce(data[["HMH_HeartProbs.0.0"]], data[["HMH_HeartProbs.0.1"]], 
                   data[["HMH_HeartProbs.0.2"]], data[["HMH_HeartProbs.0.3"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
  y <- dplyr::case_when(
    is.na(vcon) ~ "Unanswered",
    vcon == "None of the above" ~ "No",
    vcon == "Do not know" ~ "Do not know",
    vcon == "Prefer not to answer" ~ "Prefer not to answer",
    vcon %in% condlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer", "Unanswered"))
  return(y)
}

# XL add: Same as above except not assigning 'Unanswered' to NA
FN_Vascular_any_raw <- function(data) {
  # Combine the vascular condition columns
  vcon <- coalesce(data[["HMH_HeartProbs.0.0"]], data[["HMH_HeartProbs.0.1"]], 
                   data[["HMH_HeartProbs.0.2"]], data[["HMH_HeartProbs.0.3"]])
  # Create a new medication variable: yes/no/do not know/prefer not to answer/NA
  condlist <- c("High blood pressure", "Stroke", "Angina", "Heart attack")
  y <- dplyr::case_when(
    is.na(vcon) ~ NA_character_,
    vcon == "None of the above" ~ "No",
    vcon == "Do not know" ~ "Do not know",
    vcon == "Prefer not to answer" ~ "Prefer not to answer",
    vcon %in% condlist ~ "Yes",
    TRUE ~ "Unexpected answer"
  )
  y <- factor(y, levels = c("Yes", "No", "Do not know", "Prefer not to answer"))
  return(y)
}


FN_Vascular_condition <- function(conditions, string) {
  function(data){
    x <- FN_Vascular_any(data)
    if(anyNA(x)){
      stop("Unexpected value in source data for self-reported medication.")
    }
    
    # Now check for the requested condition across the columns
    y <- apply(data[,c("HMH_HeartProbs.0.0", "HMH_HeartProbs.0.1", "HMH_HeartProbs.0.2", "HMH_HeartProbs.0.3")],
               1, function(x) any(x %in% conditions))
    # And incorporate the info on whether this participant reported any condition
    y[x %in% c("Prefer not to answer", "Do not know", "Unanswered")] <- "Unanswered"
    y[is.na(y)] <- "FALSE"
    y <- factor(y, levels=c("TRUE", "FALSE", "Unanswered"), 
                labels=c(paste0("Self-reported ", string), paste0("Did not report ", string), "Unanswered"))
    return(y)
  }
}

FN_MissingCategory <- function(missingvals, categ_name){
  function(x){
    # XL add: Need to assign variable as factor format first 
    x<-factor(x)
    # Categorise missing data - change levels so specified levels and NA are both "Unanswered"
    labels <- c(levels(x)[-which(levels(x) %in% missingvals)], categ_name)
    y <- as.character(x)
    y[y %in% missingvals] <- categ_name
    y[is.na(y)] <- categ_name
    y <- factor(y, levels=labels, ordered=FALSE)
    return(y)
  }
}

FN_JoinPRS <- function(filepath, colname) {
  function(x) {
    if(file_ext(filepath)=="rds"){
      prs <- readRDS(filepath) %>%
        rename(ID = id)
      
    } else if(file_ext(filepath)=="sscore"){
      prs <- read.delim(filepath, header=TRUE) %>%
        rename(ID = IID)
    } else {
      warning("Unidentified file type for PRS")
    }
    y <- prs[[colname]][match(x, prs$ID)]
    return(y)
  }
}


FN_VItoLong <- function(data, colname, instance, mapper) {
  
  if(strsplit(mapper, "\\.")[[1]][2]=="csv"){
    mapper_file <- read.csv_kdrive(mapper)
  } else if(strsplit(mapper, "\\.")[[1]][2]=="xlsx") {
    mapper_file <- read.xlsx_kdrive(mapper)
  }
  
  if (is.null(instance)) {
    pattern <- paste0(colname, "(.*)\\.(.*)")
  } else {
    pattern <- paste0(colname, "(.*)\\.", instance, "\\.(.*)")
  }

  long <- evalWithMemoization(
    data %>% pivot_longer(
      cols = starts_with(colname),
      names_to = c(".value", "measure"),
      names_pattern = pattern
    ) %>%
      drop_na(starts_with(colname)) %>%
      mutate(Code = as.numeric(Code)) %>%
      left_join(x = ., y = mapper_file, by= "Code"),
    key = c(data, colname, instance, mapper_file)
  )
}

# XL: Below is for filtering VI diagnoses codes
FN_VI_filtercodes <- function(dx_codes, colname, instance = 0, return_label = "dx", mapper) {
  function(data) {
    
    if(strsplit(mapper, "\\.")[[1]][2]=="csv"){
      mapper_file <- read.csv_kdrive(mapper)
    } else if(strsplit(mapper, "\\.")[[1]][2]=="xlsx") {
      mapper_file <- read.xlsx_kdrive(mapper)
    }
    
    
    long_dx <- evalWithMemoization(
      FN_VItoLong(
        data,
        colname = colname,
        instance = instance,
        mapper = mapper
      ) %>%
        filter(Code %in% dx_codes) %>%
        group_by(ID) %>%
        arrange(Year) %>%
        slice_head %>%
        mutate(dx = factor(meaning),
               Year = ifelse(Year %in% c(-1, -3), NA, Year),
               Year_date = date_decimal(Year),
               duration = as.numeric(round(
                 difftime(Rec_DateAssess, Year_date, unit = "days") / 365.25,
                 digits = 2
               )))%>%
        mutate_at(if("Age" %in% names(.)) "Age" else integer(0),~ifelse(.x %in% c(-1, -3), NA, .x)),
      key = c(data, dx_codes, instance, mapper_file)
    )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    return(y)
  }
}

#XL add: 19/11/2020 Below is for filtering VI medication codes
# Can produce meds taken status and number of meds taken in that category
FN_VImed_filtercodes <- function(med_codes, med_name= 'statin', colname, instance = 0, return_label, mapper) {
  function(data) {
    
    mapper_file <- read.csv_kdrive(mapper)

    long_med <- evalWithMemoization(
      FN_VItoLong(
        data,
        colname = colname,
        instance = instance,
        mapper = mapper
      ) %>%
        filter(Code %in% med_codes)%>%
        # Meds taken status
        mutate(!!med_name:=1)%>%
        # Number of meds taken in that category
        add_count(ID)%>%
        rename(!!paste0(med_name,'_num'):=n)%>%
        #remove duplicate ID
        distinct(ID,.keep_all = TRUE)
      ,
      key = c(med_codes, instance, mapper_file)
    )
    
    y <- long_med[[return_label]][match(data$ID, long_med$ID)]
    y[is.na(y)] <- 0
    
    # If want to return med status, transfer to factor with level No/Yes; If want to return number of meds, transfer to factor format
    if(return_label==med_name){
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    } else if(return_label==paste0(med_name,'_num')){
      y<-factor(y)
    }
    
    return(y)
  }
}

# XL add: based on returned mapping list xlsx filter VI code
FN_VI_comorb<-function(condition,returned_mapping){
  function(data){
    
    # Coding list of interest
    dx_codes<-returned_mapping[which(returned_mapping$Conditions==condition),]$Code
    y<- FN_VI_filtercodes(dx_codes = dx_codes,
                          colname = "VeI_NonCancer",
                          instance = 0,
                          return_label = "dx",
                          mapper = file.path(config$cleaning$coding,"coding6_flat_NonCancerIllness.csv"))(data)
    # If not blank, assign yes
    y<- factor(ifelse(is.na(y), 0, 1), levels = c(0,1), labels = c('No','Yes'))
    return(y)
  }
  
}


FN_HES_episodes <- function(episodePath=file.path(config$data$portal$HES, "hesin.txt")) {
                        
  episodes <- evalWithMemoization(
    fread(episodePath,na.strings = "") %>%
      select(eid, ins_index, dsource, epistart, admidate) %>%
      rename(ID = eid) %>%
      mutate(DateFirst = as.Date(coalesce(epistart, admidate), format="%d/%m/%Y")) %>%
      select(-epistart, -admidate),
    key = episodePath
  )
  
  return(episodes)
}

FN_HES_diagnoses <- function(icd=10, HESPath=config$data$portal$HES) {
  
  episodePath=file.path(HESPath, "hesin.txt")
  diagnosisPath=file.path(HESPath, "hesin_diag.txt")
  
  # Not going to spend ages building in loads of tolerance but here's a simple check!
  if(is.character(icd)) {
    icd <- as.numeric(str_remove(tolower(icd), "icd"))
  }
  stopifnot(icd %in% c(9, 10))
  
  codecol <- glue("diag_icd{icd}")
  
  diagnoses <- evalWithMemoization(
    fread(diagnosisPath,na.strings = "") %>%
      rename(ID = eid,
             Code = !!codecol) %>%
      select(ID, ins_index, arr_index, level, Code) %>%
      filter(!is.na(Code)) %>%
      inner_join(FN_HES_episodes(episodePath), by=c("ID", "ins_index")),
    key = c(episodePath, diagnosisPath, icd)
  )
  return(diagnoses)
}

FN_HES_operations <- function(opcs=4, HESPath=config$data$portal$HES) {
  
  episodePath=file.path(HESPath, "hesin.txt")
  operationPath=file.path(HESPath, "hesin_oper.txt")
  
  # Not going to spend ages building in loads of tolerance but here's a simple check!
  if(is.character(opcs)) {
    opcs <- as.numeric(str_remove(tolower(opcs), "opcs"))
  }
  stopifnot(opcs %in% c(3, 4))
  
  codecol <- glue("oper{opcs}")
  
  operations <- evalWithMemoization(
    fread(operationPath,na.strings = "") %>%
      rename(ID = eid,
             Code = !!codecol) %>%
      select(ID, ins_index, arr_index, level, Code) %>%
      filter(!is.na(Code)) %>%
      inner_join(FN_HES_episodes(episodePath), by=c("ID", "ins_index")),
    key = c(episodePath, operationPath, opcs)
  )
  return(operations)
}

FN_Death_registry <- function(deathPath=config$data$portal$deaths) {
  
  recordPath <- file.path(deathPath, "death.txt")
  causePath <- file.path(deathPath, "death_cause.txt")
  
  # A few individuals have multiple differing date records, when 
  # a second death certificate was issued after a post mortem
  # In these cases, we want to take the updated cause of death
  # So we are interested in the greatest instance index per individual
  # See K:/TEU/UKB33952_Data/Data_Portal/Death_Registry/DeathLinkage.pdf
  # Section 2. Data Cleaning
  deaths <- evalWithMemoization(
    fread(recordPath,na.strings = "") %>%
      select(eid, ins_index, dsource, date_of_death) %>%
      rename(ID = eid) %>%
      group_by(ID) %>% 
      top_n(1, ins_index) %>%
      mutate(Dth_Date = as.Date(date_of_death, format="%d/%m/%Y")) %>%
      select(-date_of_death),
    key = recordPath
  )
  
  causes <- evalWithMemoization(
    fread(causePath,na.strings = "") %>% 
      rename(ID = eid,
            Dth_ICD10All = cause_icd10) %>%
      inner_join(deaths, by=c("ID", "ins_index")),
    key = c(causePath, deaths)
  )
  
  return(causes)
}

FN_Death_registry_primary <- function(deathPath=config$data$portal$deaths) {
  
  primary <- FN_Death_registry() %>%
    filter(arr_index==0) %>%
    rename(Dth_ICD10Underlying = Dth_ICD10All)
  
  return(primary)
}
FN_Death_registry_secondary <- function(deathPath=config$data$portal$deaths) {
  
  secondary <- FN_Death_registry() %>%
    filter(arr_index>0) %>%
    rename(Dth_ICD10Secondary = Dth_ICD10All)

  return(secondary)
}

FN_HEStoLong <- function(data, colname, removeNAfrom) {
  
  pattern <- paste0(colname, "(.*)\\.0\\.(.*)")
  
  long <- evalWithMemoization({
    data <- data %>% pivot_longer(
      cols = starts_with(colname),
      names_to = c(".value", "measure"),
      names_pattern = pattern
    ) 
    data %>%
      drop_na(!!removeNAfrom) %>%
      select(-measure)
  },
  key = c(data, colname, removeNAfrom))
}


# XL add: 10/12/2020

# First occurrence dataset within each HES source (ICD9 or ICD10 or OPCS4)
# data should contain ID, RecDate and corresponding HES dx and date fields
# Return dataset with ID, RecDate, Dx, DateFirst for first occurrence of condition from each HES source
FN_eachHES_First<-function(data,HES_xlsx,condition='MACE',colname,removeNAfrom, record_level=FALSE){
  
  # Produce code of interest from analysis codings xlsx
  #HES_codes=HES_xlsx[which(HES_xlsx$Conditions==condition),]$Code
  HES_file <- read.xlsx_kdrive(HES_xlsx, col_types=c('text'))
  
  # First occurrence of condition
  sub_long <- evalWithMemoization({
    if(record_level==FALSE){
      HESlong <- FN_HEStoLong(data = data,
                  colname = colname,
                  removeNAfrom = removeNAfrom)
    } else {
      HESlong <- switch(colname, 
                        "HES_ICD9" = FN_HES_diagnoses(icd=9),
                        "HES_ICD10" = FN_HES_diagnoses(icd=10),
                        "HES_OPCS4" = FN_HES_operations(opcs=4)) %>%
        left_join(., data, by="ID")
    }
    HESlong %>%
      rename_at(vars(starts_with('Diag')), ~ paste0('Code')) %>%
      # Left join to get 'Conditions' and 'ConditionsType' column
      left_join(x = ., y = HES_file, by = "Code")%>%
      # Filter based on condition of interest
      filter(Conditions%in%condition)%>%
      group_by(ID) %>%
      # Select the first occurrence
      slice(which.min(DateFirst)) #%>%
      # Keep essential columns only
      #select(ID, Rec_DateAssess, ConditionsType_TL, ConditionsType, DateFirst)
  },
  key = c(data, colname, removeNAfrom, HES_file, condition, record_level))
}

# First occurrence dataset among all HES source (ICD9 + ICD10 + OPCS4)
# Note: One can specify which source they do not wish to use as NULL (e.g. ICD9_xlsx=NULL)
# * return_label='baseline': condition status at baseline
# * return_label='baseline_comp': condition subtype at baseline
# * return_label='followup': condition status at followup 
# * return_label='followup_date': Date of condition at follow up (used for deriving outcome)
# * return_label='followup_comp': condition subtype at follow up
FN_HES_First<-function(ICD9_xlsx=NULL,ICD10_xlsx=NULL,OPCS4_xlsx=NULL,condition='MACE',return_label='baseline', record_level=FALSE){
  function(data){
    
    HES_total<-NULL
    
    # Get first occurrence dataset from each HES source and rbind together
    if (!is.null(ICD9_xlsx)){
      HES_total<-FN_eachHES_First(data%>%select(ID,'Rec_DateAssess', 
                                                contains("HES_ICD9Diag.0."), 
                                                contains("HES_ICD9DateFirst.0.")),
                                  HES_xlsx = ICD9_xlsx,
                                  condition = condition,
                                  colname = 'HES_ICD9',
                                  removeNAfrom = c('Diag','DateFirst'),
                                  record_level = record_level)
    }
    
    if (!is.null(ICD10_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_ICD10Diag.0."),
                                       contains("HES_ICD10DateFirst.0.")),
                         HES_xlsx = ICD10_xlsx,
                         condition = condition,
                         colname = 'HES_ICD10',
                         removeNAfrom = c('Diag','DateFirst'),
                         record_level = record_level))
    }
    
    if (!is.null(OPCS4_xlsx)){
      HES_total<-HES_total%>%bind_rows(
        FN_eachHES_First(data%>%select(ID,'Rec_DateAssess',
                                       contains("HES_OPCS4Code.0."),
                                       contains("HES_OPCS4DateFirst.0.")),
                         HES_xlsx = OPCS4_xlsx,
                         condition = condition,
                         colname = 'HES_OPCS4',
                         removeNAfrom = c('Code','DateFirst'),
                         record_level = record_level))
    }
    
    # Select first occurrence among all HES source
    long_dx=HES_total%>%
      group_by(ID)%>%
      # Select the first occurrence
      slice(which.min(DateFirst))%>%
      
      mutate(
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(DateFirst<=Rec_DateAssess,1,0),
        # Subtype column (prior to or at baseline)
        baseline_comp=ifelse(DateFirst<=Rec_DateAssess,ConditionsType,NA),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(DateFirst>Rec_DateAssess,1,0),
        # Date of condition (Follow-up)
        followup_date=if_else(DateFirst>Rec_DateAssess,DateFirst,as.Date(NA)),
        # Subtype column (Follow-up)
        followup_comp=ifelse(DateFirst>Rec_DateAssess,ConditionsType,NA)
      )
    
    y <- long_dx[[return_label]][match(data$ID, long_dx$ID)]
    
    if(return_label%in%c('baseline','followup')){
      y[is.na(y)]=0
      y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    return(y)
    
  }
}

# General cleaning of primary care data was performed in a separate Rmd (due to the complexity) so
# I didn't add FN_HES_diagnoses alike function for primary care

# Filter read2, read3 based on primary medical events data only (i.e. not prescription data yet)
FN_PCevent_filtercodes <- function(PCPath=config$data$portal$primarycare,
                                   read2_codes,read3_codes,return_label='baseline'){
  function(data){
    
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir=file.path(PCPath, "primary_care_processed.db"), 
                             read_only=TRUE)
    
    gp_clinical<-tbl(con,"clinical")
    
    df<-gp_clinical%>%
      # Keep read codes
      filter((!is.na(read_2) & read_2 %in% read2_codes) | (!is.na(read_3) & read_3 %in% read3_codes))%>%
      # Extract the first dx of participant
      group_by(eid)%>%
      slice_min(event_dt)%>%
      collect()
    
    duckdb::dbDisconnect(con, shutdown=TRUE)
    
    # Compare with baseline date to see whether it's prevalent or not
    df<-left_join(df,data,by=c("eid"="ID"))%>%
      mutate( 
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(event_dt<=Rec_DateAssess,1,0),
        duration=as.numeric(round((Rec_DateAssess-event_dt)/365.25,2)),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(event_dt>Rec_DateAssess,1,0),
        # Only keep incident event date
        followup_date=if_else(event_dt>Rec_DateAssess,event_dt,as.Date(NA))
      )
    
    y <- df[[return_label]][match(data$ID, df$eid)]
    
    if(return_label%in%c('baseline','followup')){
      # Assign NA to 0 because NA doesn't necessarily mean this person doesn't have PC data
      y[is.na(y)]=0
      #y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    
    return(y)
    
  }
}

# Filter condition based on biomarkers values in GP data
FN_PCevent_filtervalues <- function(PCPath="K:/TEU/UKB33952_Data/Data_Portal/Primary_Care/20220801",
                                   lower_threshold,upper_threshold,return_label='baseline'){
  function(data){
    
    gp_values<-readRDS(file.path(PCPath,"Processed/gp_HbA1c.rds"))
    
    df<-gp_values%>%
      # filter based on the thresholds
      filter(!is.na(value_mean) & value_mean>=lower_threshold & value_mean<upper_threshold)%>%
      # Extract the first dx of participant
      group_by(eid)%>%
      slice_min(event_dt)
    
    # Compare with baseline date to see whether it's prevalent or not
    df<-left_join(df,data,by=c("eid"="ID"))%>%
      mutate( 
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(event_dt<=Rec_DateAssess,1,0),
        duration=as.numeric(round((Rec_DateAssess-event_dt)/365.25,2)),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(event_dt>Rec_DateAssess,1,0),
        # Only keep incident event date
        followup_date=if_else(event_dt>Rec_DateAssess,event_dt,as.Date(NA))
      )
    
    y <- df[[return_label]][match(data$ID, df$eid)]
    
    if(return_label%in%c('baseline','followup')){
      # Assign NA to 0 because NA doesn't necessarily mean this person doesn't have PC data
      y[is.na(y)]=0
      #y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    
    return(y)
    
  }
}

# Filter read2, read3 based on primary prescription data only
FN_PCmeds_filtercodes <- function(PCPath=config$data$portal$primarycare,
                                  read2_codes,BNF_codes,DMD_codes,return_label='baseline'){
  function(data){
    
    con <- duckdb::dbConnect(duckdb::duckdb(), dbdir=file.path(PCPath, "primary_care_processed.db"), 
                             read_only=TRUE)
    
    gp_scripts<-tbl(con,"scripts")
    
    df<-gp_scripts%>%
      # Keep meds codes
      filter((!is.na(read_2) & read_2 %in% read2_codes) | (!is.na(bnf_code) & bnf_code %in% BNF_codes)|
               (!is.na(dmd_code) & dmd_code %in% DMD_codes))%>%
      # Extract the first issue date of participant
      group_by(eid)%>%
      slice_min(issue_date)%>%
      collect()
    
    duckdb::dbDisconnect(con, shutdown=TRUE)
    
    # Compare with baseline date to see whether it's prevalent or not
    df<-left_join(df,data,by=c("eid"="ID"))%>%
      mutate( 
        # Indicator column for previous dx (prior to or at baseline)
        baseline=ifelse(issue_date<=Rec_DateAssess,1,0),
        #duration=as.numeric(round((Rec_DateAssess-issue_date)/365.25,2)),
        # Indicator column for dx (Follow-Up)
        followup=ifelse(issue_date>Rec_DateAssess,1,0)
      )
    
    y <- df[[return_label]][match(data$ID, df$eid)]
    
    if(return_label%in%c('baseline','followup')){
      # Assign NA to 0 because NA doesn't necessarily mean this person doesn't have PC data
      y[is.na(y)]=0
      #y<-factor(y,levels = c(0,1),labels = c('No','Yes'))
    }
    
    return(y)
    
  }
}


# Filter death registry data based on ICD10 codes
# Atm this function doesn't incorporate filtering on both primary + secondary dth (primary=FALSE) using Showcase data (record_level=FALSE)
# primary=TRUE means primary cause of dth only
# * return_label='dth': return 'yes' for ones with primary cause of death by specified ICD10 codes  
# * return_label='dth_date': return death date by specified ICD10 codes 
FN_Dth_filtercodes <- function(ICD10_codes, return_label = "dth", record_level=TRUE, primary=TRUE) {
  function(data) {
    
    if(primary){
      
      if(record_level){
        ID <- data
        deaths <- FN_Death_registry_primary()%>%
          rename(Dth_ICD10 = Dth_ICD10All)
        
      } else {
        ID <- data$ID
        deaths <- data %>%
          mutate(Dth_ICD10 = coalesce(Dth_ICD10Underlying.1.0, Dth_ICD10Underlying.0.0),
                 Dth_Date = coalesce(Dth_Date.1.0, Dth_Date.0.0))
      }
    }else{
      
      if(record_level){
        ID <- data
        deaths <- FN_Death_registry()%>%
          rename(Dth_ICD10 = Dth_ICD10All)
      }
    }
    
    subdata <- deaths %>% filter(Dth_ICD10 %in% ICD10_codes) %>%
      mutate(dth = 'Yes',
             dth_date = Dth_Date)

    y <- subdata[[return_label]][match(ID, subdata$ID)]
    return(y)
  }
}

# Derivation of diabetes from Eastwood2016 
# Algorithm 1

FN_Diabetes<-function(data){
  
  # Renaming cols
  data <- data%>%
    rename(
      # Touchscreen
      TQ_gest=TEU_HMH_gest_diabetes,
      TQ_med=TEU_HMH_Meds_Diab,
      TQ_med1Yr=HMH_Insulin1Yr,
      
      # Verbal Interview
      VeI_Diab=Estw_VeI_diab,
      VeI_Diab_age=Estw_VeI_diab_age,
      VeI_T1_med=TEU_VeI_T1D_exclmeds,
      VeI_T2_med=TEU_VeI_T2DM_meds,
      VeI_gest=TEU_VeI_gest,
      VeI_gest_age=TEU_VeI_gest_age,
      VeI_T1=TEU_VeI_T1DM,
      VeI_T2=TEU_VeI_T2DM)%>%
    # Re-categorise all missing categories and NA into "Unanswered"
    mutate(
      TQ_gest=fct_explicit_na(TQ_gest,"Unanswered"),
      TQ_med1Yr=fct_explicit_na(TQ_med1Yr,"Unanswered"),
      VeI_Diab=fct_explicit_na(VeI_Diab,"No diabetes"),
      VeI_gest=fct_explicit_na(VeI_gest,"No gestational diabetes"),
      VeI_T1=fct_explicit_na(VeI_T1,"No T1 diabetes"),
      VeI_T2=fct_explicit_na(VeI_T2,"No T2 diabetes")
      )
    
   ID <- data$ID 
  
  # 1st left box: 
  left_1<-(
    ## No self-reported diab (of any types) from VI
    (data$VeI_Diab=="No diabetes") & 
    ## No gestational diab from TQ
    (data$TQ_gest!="Yes") &
    ## No self-report diab med (TQ+VI)
    (data$TQ_med!="Self-reported Insulin" & data$VeI_T1_med=="No" & data$VeI_T2_med=="No")
     )
  
  # What we really care about is the ID and their status of diabetes
  result<-data[left_1,"ID"]%>%
    mutate(y="Diabetes unlikely")
  
  # continue with subset
   data <- data[!left_1,]
 
  # 2nd left box
   left_2<-(
     ## TQ gestational diab + no diab med (TQ/VI) + no concurrent T1/T2 diab (VI)
     (data$TQ_gest=="Yes" & 
        data$TQ_med!="Self-reported Insulin" & data$VeI_T1_med=="No" & data$VeI_T2_med=="No" &
         data$VeI_T1=="No T1 diabetes" & data$VeI_T2=="No T2 diabetes") |
      ## VI gestational diab + age at dx < 50years + no diab med (TQ/VI) + no concurrent T1/T2 diab (VI)
     (data$VeI_gest=="gestational diabetes" & !is.na(data$VeI_gest_age) & data$VeI_gest_age<50 &
        data$TQ_med!="Self-reported Insulin" & data$VeI_T1_med=="No" & data$VeI_T2_med=="No" &
        data$VeI_T1=="No T1 diabetes" & data$VeI_T2=="No T2 diabetes") 
       
   ) 
   
   
   result <- rbind(result,data[left_2,"ID"]%>%mutate(y="Possible gestational diabetes"))
     
   data <- data[!left_2,]
   
   # Below is a draft and we don't really care about because all we want is to exclude ppl had prevalent T1D or T2D
   # 3rd and 4th left box
   
   left_3_4<-(
     ## Current non-metformin oral diab users
     (data$TEU_VeI_nonmet_meds=="Yes")|
       ## Age at dx>36 years with european origin or age at dx>30 with South asian or African Caribbean origin
       (data$TEU_VeI_nonmet_meds=="No" & data$TEU_ethnicgrp=="White" & !is.na(data$VeI_Diab_age) & data$VeI_Diab_age>36)|
       (data$TEU_VeI_nonmet_meds=="No" & data$TEU_ethnicgrp%in%c("S. Asian","Black") & !is.na(data$VeI_Diab_age) & data$VeI_Diab_age>30)
   )
   
   result <- rbind(result,data[left_3_4,"ID"]%>%mutate(y="Possible type 2 diabetes"))
   
   data <- data[!left_3_4,]
   
   # 5th box
   
   left_5<-(
     ## self report current insulin (TQ/VI)
     (data$TQ_med!="Self-reported Insulin" & data$VeI_T1_med=="No") & 
       ## self report insulin <12 months post dx
       data$TQ_med1Yr!="Yes" &
       data$VeI_T1=="No T1 diabetes"
   )
   
   result <- rbind(result,data[left_5,"ID"]%>%mutate(y="Possible type 2 diabetes"))
  
   # The rest
   
   result <- rbind(result,data[!left_5,"ID"]%>%mutate(y="Possible type 1 diabetes"))
   
   # Return the diab types of each participant
   y <- result[["y"]][match(ID, result$ID)]
  
  return(y)
}


























