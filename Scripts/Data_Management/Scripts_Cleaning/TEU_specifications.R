# Jennifer Collister
# 30/09/20

# Load the project config file for filepaths etc
if (!exists("config")) {
  library(yaml)
  config = yaml.load_file("config.yml")
}


specs <- function() {
  
  # If you're here to write a new spec, you can run this source line interactively
  # to load all the variable derivation objects into your working environment
  # so you get autocomplete when typing them!
  source(file.path(config$scripts$cleaning, "derivation_objects.R"),
         local = if (sys.nframe() == 0L) {
           FALSE
           } else {
             TEUmaps <- new.env()
             }
         )
  if (exists("TEUmaps")) {
    attach(TEUmaps)
    on.exit(detach(TEUmaps))
  }
  
  # Dataset specifications
  
  TEUvars_common <- list(
    ID,
    BaC_Sex,
    TEU_BaC_DateOfBirth,
    Rec_DateAssess,
    TEU_BaC_AgeAtRec,
    TEU_ethnicgrp,
    TEU_Rec_AssessCentre,
    TEU_Rec_Country
  )
  
  TEUvars_BP <- list(
    TEU_BlP_SBP.0.0,
    TEU_BlP_SBP.0.1,
    TEU_BlP_DBP.0.0,
    TEU_BlP_DBP.0.1,
    TEU_BlP_nSBP,
    TEU_BlP_nDBP,
    TEU_BlP_SBP.avg,
    TEU_BlP_DBP.avg
  )
  
  HTN_control_comorb <- list(
    TEU_VeI_CVD,
    TEU_VeI_diab,
    TEU_VeI_arrhy,
    TEU_VeI_osteo,
    TEU_VeI_joint,
    TEU_VeI_epil,
    TEU_VeI_mig,
    TEU_VeI_anx,
    TEU_VeI_dep,
    TEU_VeI_asthCOPD
    
    
  )
 
  # TEUvars_raw added by XL 
  # This block of variables are for exploring how we handle categories such as 'Prefer not to answer' and 'Do not know'
  TEUvars_raw <-list(
    ID,
    Eth_Ethnicity,
    Edu_HighestQual,
    Alc_Status,
    Smo_Status,
    TEU_HoH_PreTaxInc,
    HMH_BowelSc,
    HMH_Diabetes,
    HMH_IllDisab,
    HMH_VascCond,
    HMH_Meds_any,
    HMH_HTNAge
  )
  
  UKB_genetic <- list(
    ID,
    GeP_UsedInPCA, # Identifies participants which met UKB QC for inclusion in PCA
    GeP_Outliers, # Identifies participants who are outliers for missingness and heterozygosity
    GeP_ethnic, # Identifies participants with genetic White British ancestry
    GeP_Array, # We should adjust our PRS analyses by array
    GeP_Batch, # We may wish to adjust for batch effect
    GeP_Plate, # We may wish to adjust for plate effect
    GeP_PC(pc=1),
    GeP_PC(pc=2),
    GeP_PC(pc=3),
    GeP_PC(pc=4),
    GeP_PC(pc=5),
    GeP_PC(pc=6),
    GeP_PC(pc=7),
    GeP_PC(pc=8),
    GeP_PC(pc=9),
    GeP_PC(pc=10), # Genetic Principal Components of ancestry
    GeP_Sex, # Used to check for sex discordance
    BaC_Sex, # Used to check for sex discordance,
    GPLC_PC(pc=0),
    GPLC_PC(pc=1),
    GPLC_PC(pc=2),
    GPLC_PC(pc=3)
  )

  MACE_summary <- c(
    TEUvars_common,
    list(
      # MACE at baseline
      TEU_HES_MACE_prev(record_level=FALSE),
      TEU_VeI_MACE_nonc,
      TEU_VeI_MACE_op,
      TEU_HMH_MACE_prev,
      TEU_MACE_prev,
      # MACE outcome
      TEU_HES_MACE_fudate(record_level=FALSE),
      TEU_Dth_MACE_dthdate(record_level=FALSE),
      TEU_MACE_eventdate,
      TEU_Dth_NotMACE_dthdate(record_level=FALSE),
      Admin_CensorDate(record_level=FALSE),
      BaC_LostFUDate,
      TEU_MACE_censordate,
      TEU_MACE_status,
      TEU_MACE_time,
      TEU_MACE_time_yrs,
      # MACE subtypes 
      TEU_HES_MACE_fucomp(record_level=FALSE),
      TEU_MACE_fucomp
    )
    
  )
  
  MACE_recordlevel <- c(
    TEUvars_common,
    list(
      # MACE at baseline
      TEU_HES_MACE_prev(record_level=TRUE),
      TEU_VeI_MACE_nonc,
      TEU_VeI_MACE_op,
      TEU_HMH_MACE_prev,
      TEU_MACE_prev,
      # MACE outcome
      TEU_HES_MACE_fudate(record_level=TRUE),
      TEU_Dth_MACE_dthdate(record_level=TRUE),
      TEU_MACE_eventdate,
      TEU_Dth_NotMACE_dthdate(record_level=TRUE),
      Admin_CensorDate(record_level=TRUE),
      BaC_LostFUDate,
      TEU_MACE_censordate,
      TEU_MACE_status,
      TEU_MACE_time,
      TEU_MACE_time_yrs,
      # MACE subtypes 
      TEU_HES_MACE_fucomp(record_level=TRUE),
      TEU_MACE_fucomp
    )
  )
  
  HTN_control <- c(
    TEUvars_common,
    TEUvars_BP,
    TEU_VeI_HTN_prevalent(),
    list(
      TEU_selfrepHTN_dx,
      TEU_VeI_HTNmeds_rubric,
      TEU_selfrepHTN_meds,
      VeI_PregnantNow,
      TEU_BaC_AgeCat,
      TEU_BlP_measuredHTN,
      TEU_evidenceHTN,
      TEU_awareHTN,
      TEU_treatedHTN,
      TEU_HMH_BowelCancerScreen,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_Emp_CurrStat,
      TEU_Emp_JobCode_v2,
      TEU_HTN_Emp_category,
      TEU_HoH_PreTaxInc,
      TEU_Emp_CurrStat,
      TEU_Emp_JobCode_v2,
      TEU_HTN_Emp_category,
      TEU_TownsendDepInd_Quint,
      TEU_HMH_Meds_BP,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_Binge,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_BSM_BMIcat,
      TEU_BSM_WaistCircCat,
      TEU_SBP_PRS,
      TEU_DBP_PRS,
      TEU_BP_PRS,
      TEU_BP_PRS_quintiles,
      TEU_HMH_VascCond,
      TEU_HMH_prevHTN,
      TEU_HMH_prevstroke,
      TEU_HMH_prevCVD,
      HMH_IllDisab,
      HMH_Diabetes,
      HMH_HTNAge,
      TEU_BlP_HTNseverity
      
    )
  )
  
  
  test <- c(
    TEUvars_common,
    TEU_VeI_T2DM_meds(),
    TEU_VeI_T2DM_prevalent(),
    Estw_VeI_diab_prevalent(),
    TEU_VeI_T1DM_prevalent(),
    TEU_VeI_gest_prevalent(),
    list(
      TEU_BaC_AgeCat,
      # Self-report
      TEU_HMH_Meds_Diab,
      TEU_HMH_gest_diabetes,
      HMH_Diabetes,
      HMH_DiabetesAge,
      HMH_Insulin1Yr,
      
      # prevalent 
      #TEU_T2DM_prevalent,
      Estw_diab_prevalent,
      TEU_HES_diab_excl(record_level=TRUE),
      TEU_diab_excl,
      
      # Primary care data
      TEU_PC_diab_prev,
      TEU_PC_T2D_Age
    )
  )
  
  HTN_control_PRS <- c(
    UKB_genetic,
    HTN_control
  )
  
  Cholstrl_control<-c(
    MACE_recordlevel,
    TEU_VeI_statin(),
    HTN_control_comorb,
    list(
      # Exclusion criteria
      TEU_VeI_seriouscomb,
      TEU_VeI_cancer,
      VeI_PregnantNow,
      
      # Covariates
      TEU_BaC_AgeCat,
      BSM_BMI,
      TEU_BSM_BMIcat,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_WeeklyCat,
      PhA_METsWkAllAct,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_HMH_BowelCancerScreen,
      HTN_comorb_num,
      HTN_comorb_numcat,
      TownsendDepInd,
      TEU_TownsendDepInd_Quint,
      TEU_HoH_PreTaxInc,
      TEU_Emp_CurrStat,
      TEU_Emp_JobCode_v2,
      TEU_HTN_Emp_category,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_CountryIncome,
      
      ## PRS
      TEU_LDL_C_PRS,
      
      #GeP_Batch,
      GeP_ethnic,
      GeP_PC(pc=1),
      GeP_PC(pc=2),
      GeP_PC(pc=3),
      GeP_PC(pc=4),
      GeP_PC(pc=5),
      GeP_PC(pc=6),
      GeP_PC(pc=7),
      GeP_PC(pc=8),
      GeP_PC(pc=9),
      GeP_PC(pc=10),
      
      # cholesterol related
      BBC_CHOL_Result,
      BBC_HDL_Result,
      BBC_LDL_Result,
      TEU_LDLctrl_v1
      
    )
  )
  
 

                   
  Cholesterol_PRS <- c(
    TEUvars_common,
    TEU_VeI_CVD_operation(dx_codes = c(1069, 1070, 1095, 1105)),
    list(
      TEU_BaC_AgeCat,
      TEU_HMH_BowelCancerScreen,
      TEU_Edu_HighestQual,
      TEU_Edu_ISCED,
      TEU_HoH_PreTaxInc,
      TEU_TownsendDepInd_Quint,
      TEU_HMH_Meds_Chol,
      TEU_Smo_Status,
      TEU_Alc_Status,
      TEU_Alc_WeeklyAlcUnits,
      TEU_Alc_Binge,
      TEU_Pha_METsover1200,
      TEU_FaH_CVD,
      TEU_BSM_BMIcat,
      TEU_BSM_WaistCircCat,
      TEU_LDL_C_PRS,
      TEU_LDL_C_PRS_deciles,
      GeP_Batch,
      TEU_HMH_VascCond,
      TEU_HMH_prevHTN,
      TEU_HMH_prevstroke,
      TEU_HMH_prevCVD,
      HMH_IllDisab,
      HMH_Diabetes,
      BBC_CHOL_Result,
      BBC_HDL_Result,
      BBC_LDL_Result,
      GeP_PC(pc=1),
      GeP_PC(pc=2),
      GeP_PC(pc=3),
      GeP_PC(pc=4),
      ADO_DateFirstMI,
      ADO_DateFirstIStroke
    )
  )
  
  # Eastwood2016 for derivation of diabetes
  Eastwood2016 <- c(
    TEUvars_common,
    TEU_VeI_T2DM_meds(),
    TEU_VeI_T2DM_prevalent(),
    Estw_VeI_diab_prevalent(),
    TEU_VeI_T1DM_prevalent(),
    TEU_VeI_gest_prevalent(),
    list(
      # Self-report
      TEU_HMH_Meds_Diab,
      TEU_HMH_gest_diabetes,
      HMH_Diabetes,
      HMH_DiabetesAge,
      HMH_Insulin1Yr,
      Estw_diab_prevalent
    )
  )
  

  T2DM_PRS <- c(
    TEUvars_common,
    UKB_genetic,
    TEU_VeI_T2DM_meds(),
    TEU_VeI_T2DM_prevalent(),
    Estw_VeI_diab_prevalent(),
    TEU_VeI_T1DM_prevalent(),
    TEU_VeI_gest_prevalent(),
    list(
      TEU_BaC_AgeCat,
      # Self-report
      TEU_HMH_Meds_Diab,
      TEU_HMH_gest_diabetes,
      HMH_Diabetes,
      HMH_DiabetesAge,
      HMH_Insulin1Yr,
      
      TEU_PC_ind,
      #TEU_HES_T2DM_base(record_level=TRUE),
      #TEU_HES_T2DM_excl(record_level=FALSE),
      
      # prevalent 
      #TEU_T2DM_prevalent,
      Estw_diab_prevalent,
      TEU_HES_diab_excl(record_level=TRUE),
      TEU_diab_excl,
      
      # incident
      TEU_HES_T2DM_fudate(record_level=TRUE),
      TEU_Dth_T2DM_dthdate(record_level=TRUE,primary=FALSE),
      TEU_T2DM_eventdate,
      # censoring
      TEU_Dth_NotT2DM_dthdate,
      Admin_PC_CensorDate,
      Admin_HES_CensorDate(record_level=TRUE),
      Admin_Dth_CensorDate(record_level=TRUE),
      Admin_CensorDate_T2DM(sources=c("HES", "Dth")),
      BaC_LostFUDate,
      TEU_T2DM_censordate,
      TEU_T2DM_status,
      TEU_T2DM_time,
      
      # PRS
      TEU_T2DM_582_PRS,
      #TEU_T2DM_582_PRS_quintiles,
      TEU_T2DM_180k_PRS,
      #TEU_T2DM_180k_PRS_quintiles,
      GPLC_std_T2DM_PRS,
      GPLC_enh_T2DM_PRS,
      
      # Blood assay
      BBC_HBA1C_Result,
      BBC_HBA1C_ResultFU,
      
      # RFs
      TEU_FaH_diab,
      BSM_BMI,
      TEU_BSM_BMIcat,
      
      # Variables in Leicester risk score
      BSM_WaistCirc,
      TEU_HMH_prevHTN,
      TEU_HMH_Meds_BP,
      
      # Primary care data
      TEU_PC_diab_prev,
      TEU_PCmeds_diab_prev,
      TEU_PC_T2D_inc,
      TEU_PC_T2D_date,
      
      # prediabetes
      PCdx_prediab_prev,
      PCdx_prediab_incdate,
      HbA1c_preundiab_prev,
      HbA1c_prediab_incdate,
      TEU_prediab_eventdate
    )
  )
  
  Prediab <- c(
    TEUvars_common,
    list(
      PCdx_prediab_prev,
      PCdx_prediab_incdate,
      HbA1c_preundiab_prev,
      HbA1c_prediab_incdate,
      TEU_prediab_eventdate
    )
  )
  

  return(environment())
}

TEU_SPECS <- specs()
