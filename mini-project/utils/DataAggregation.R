# import and data column definitions --------------------------------------
library(rstatix)
library(googlesheets4)
library(gsheet)
library(stringr)
library(tidyverse)
source("utils/data_structures.R")
dataset <- tibble::as_tibble(read.csv("data/dataREanonymized.csv"))

# set variable sets


ctrl_cols <- c("gender", "stroke_type", "prenotification", "imaging_done", "thrombolysis", "hospital_stroke", "first_hospital")

Agg_cond_cols <- c("stroke_type", "thrombolysis", "thrombectomy", "hospital_stroke", "first_hospital", "post_acute_care", "discharge_destination")

subgroupVars <- c("prenotification", "imaging_done", "stroke_type", "gender")



# convert all empty strings to NA
dataset[dataset == ""] <- NA

# convert all columns based on boolean/logical values from char to logical
to.conv <- sapply(dataset, function(x) all(x %in% c("False", "True", NA)))
dataset[, to.conv] <- sapply(dataset[, to.conv], as.logical)


# set fantasy hospital names
hospital_Names <- c("Progress", "Paradise", "Angelvale", "Memorial", "Rose", "General", "Mercy", "Hope", "Samaritan")
dataset$site_name <- as.factor(dataset$site_name)
levels(dataset$site_name) <- hospital_Names

# set fantasy country names
country_names <- c("Far away", "Neverland", "Over rainbow")
dataset$site_country <- as.factor(dataset$site_country)
levels(dataset$site_country) <- country_names


# setting some convenience names for analysis/readability, still compatible with older Code in the dashboard
dataset <- dataset %>%
  mutate(patient_id = subject_id, hospital = site_id, h_name = site_name, h_country = site_country, year = discharge_year, quarter = discharge_quarter) %>%
  mutate(YQ = paste(year, quarter)) %>%
  relocate(all_of(key_cols), .before = where(is.character)) %>%
  # subset for debugging
  # filter (h_name=="General", year==2018, quarter=="Q1") %>%
  #       relocate(c(year,quarter,YQ), .after = country)
  filter(discharge_year > 2000 & discharge_year <= as.integer(format(Sys.Date(), "%Y")))

key_cols <- c(
  key_cols, "patient_id", "hospital", "h_name",
  "h_country", "year", "quarter"
)

# keep_cols <- c(key_cols, "door_to_needle", "gender", "stroke_type", "prenotification", "imaging_done", "thrombolysis", "hospital_stroke", "first_hospital", "dnt_missing", "dnt_leq_60", "dnt_leq_45")
keep_cols <- c(key_cols, "door_to_needle", "dnt_leq_60", "dnt_leq_45", "dnt_grt_45", "dgt_leq_120", "dgt_leq_90", "dgt_grt_45", "rec_total_is", "isp_dis_antiplat", "mri_first_hosp", "af_p_dis_anticoag", "sp_hosp_stroke_unit_ICU", "dysphagia_screening", "gender", "stroke_type", "prenotification", "imaging_done", "thrombolysis", "hospital_stroke", "first_hospital", "dnt_missing")

allCols<- unique(c(key_cols,ctrl_cols,subgroupVars,Agg_cond_cols,keep_cols))

# computations need to be verified with ICRC
# code needs refactoring pulling the condition from the angel_conditions - this seems to currently replicate the same materias
# dataset <- dataset %>%
#   mutate(
#     dnt_missing = ifelse(stroke_type == "ischemic" & thrombolysis == T & hospital_stroke != T & first_hospital == T, ifelse(!is.na(door_to_needle), 0, 1), NA),
#     dnt_leq_60 = ifelse(stroke_type == "ischemic" & thrombolysis == T & hospital_stroke != T & first_hospital == T, ifelse(door_to_needle > 60, 0, 1), NA),
#     dnt_leq_45 = ifelse(stroke_type == "ischemic" & thrombolysis == T & hospital_stroke != T & first_hospital == T, ifelse(door_to_needle > 45, 0, 1), NA),
#     dnt_grt_45 = ifelse(stroke_type == "ischemic" & thrombolysis == T & hospital_stroke != T & first_hospital == T, ifelse(door_to_needle > 45, 1, 0), NA),
#     dgt_leq_120 = ifelse(stroke_type == "ischemic" & thrombectomy == T & hospital_stroke != T & first_hospital == T, ifelse(door_to_groin > 120, 0, 1), NA),
#     dgt_leq_90 = ifelse(stroke_type == "ischemic" & thrombectomy == T & hospital_stroke != T & first_hospital == T, ifelse(door_to_groin > 90, 0, 1), NA),
#     dgt_grt_45 = ifelse(stroke_type == "ischemic" & thrombectomy == T & hospital_stroke != T & first_hospital == T, ifelse(door_to_groin > 45, 1, 0), NA),
#     rec_total_is = ifelse(stroke_type == "ischemic", ifelse(thrombolysis == T, 1, 0), NA),
#     mri_first_hosp = ifelse(first_hospital == T, ifelse(imaging_done == T, 1, 0), NA),
#     isp_dis_antiplat = ifelse(discharge_destination != "dead", ifelse(discharge_any_antiplatelet == T, 1, 0), NA),
#     af_p_dis_anticoag = ifelse(discharge_destination != "dead", ifelse(discharge_any_anticoagulant == T, 1, 0), NA),
#     sp_hosp_stroke_unit_ICU = ifelse(hospitalized_in == "ICU/stroke unit", 1, 0),
#     dysphagia_screening = ifelse(post_acute_care == "yes" & stroke_type %in% c("ischemic", "transient ischemic", "intracerebral hemorrhage", "undetermined"), ifelse(dysphagia_screening_done == "yes", 1, 0), NA),
#   )

# further KPIs
# onset_to_door,
# door_to_imaging,
# derived percentages
# % of patients with door-to-imaging time above 30 minutes

# sub-analysis
# Patients treated with DNT <= 45 minutes broken down by stroke type
# Door-to-needle time (DNT) of pre-notified patients vs. not pre-notified
# DNT medians for each gender
# DNT of patients who did not require imaging
# Door-to-imaging time by gender, pre-conditions, pre-notification, risk-factors (before onset drugs), age,

# mRS score (at discharge) for patients imaged outside hospital
# % of patient with mRS above 3 (at discharge)
# prestroke_mrs, discharge_mrs, three_m_mrs

# WHY KPI?
# % of patients above 60 or above median
# % of patients with additional risk factor

# # simplify for students to have fewer columns to work with
# numVars <- intersect(keep_cols, numVars)
# catVars <- intersect(keep_cols, catVars)
# pctCols <- intersect(keep_cols, pctCols)
# dataset <- dataset %>% select((keep_cols))


# Relevant columns from the dataset for the QI's HARDCODED. There might be a smarter way to do this.
numVars_cols <- c(key_cols, numVars)
catVars_cols <- c(key_cols, catVars)

# NA conditions for missing data
angel_conds <- angel_awards %>%
  select(QI, incInAgg, aggCond, nameOfAggr, isAngelKPI) %>%
  unique() #%>%
# mutate(QI=nameOfAggr)

# is this needed? HK
# agg_conds <- angel_conds %>%
#   select(QI, incInAgg, aggCond) %>%
#   unique() %>%
#   mutate(nameOfAggr = QI)
# ? is rbind part of pipe?
# rbind(angel_conds)

eval_vec <- Vectorize(eval.parent, vectorize.args = "expr")

# merge data with conditions and derive variables -> takes a few minutes e.g. 3 minutes

numKPIcols <- c('door_to_needle','door_to_groin')
charKPIcols <- c('thrombolysis','imaging_done','discharge_any_antiplatelet','discharge_any_anticoagulant','dysphagia_screening_type', 'hospitalized_in')
doubleDutyCols <- c('imaging_done_dup','thrombolysis_dup','hospitalized_in_dup') 
# we're missing 'dysphagia_screening_type' and 'hospitalized_in' there's some problem with the eval statement'

# ---- prepare all categorical KPIs for aggregation
df <- dataset[,] %>%
  select(all_of(unique(c(key_cols,charKPIcols, ctrl_cols, Agg_cond_cols,cond_cols)))) %>% 
  mutate_if(is.logical, as.character) %>%
  mutate(imaging_done_dup = imaging_done,
         thrombolysis_dup=thrombolysis,
         hospitalized_in_dup = hospitalized_in) %>% 
  pivot_longer(-c(key_cols, ctrl_cols, Agg_cond_cols,cond_cols), names_to = "QI", values_to = "Value") %>%
  mutate(QI = str_replace(QI, "imaging_done_dup", "imaging_done"),
         QI = str_replace(QI, "thrombolysis_dup", "thrombolysis"),
         QI = str_replace(QI, "hospitalized_in_dup", "hospitalized_in")) %>%
  left_join(angel_conds, multiple = "all") %>%
  filter(!is.na(isAngelKPI)) %>% 
  mutate(#allConds=strsplit(incInAgg, "&"),
    qual4agg = NA,
    qual4agg = ifelse(QI!="hospitalized_in",qual4agg, TRUE),
    qual4agg = ifelse(QI!="imaging_done",qual4agg, ifelse(first_hospital==TRUE,TRUE,FALSE)),
    qual4agg = ifelse(QI!="thrombolysis",qual4agg, ifelse(stroke_type=='ischemic',TRUE,FALSE)),
    qual4agg = ifelse(QI!="dysphagia_screening_type",qual4agg, ifelse(post_acute_care == 'yes' & stroke_type %in% c('ischemic','transient ischemic','intracerebral hemorrhage', 'undetermined'),TRUE,FALSE)),
    qual4agg = ifelse(QI!="discharge_any_antiplatelet",qual4agg,ifelse(discharge_destination!='dead',TRUE,FALSE)),
    qual4agg = ifelse(QI!="discharge_any_anticoagulant",qual4agg,ifelse(discharge_destination!='dead',TRUE,FALSE)),
    isMissingData = ifelse(!is.na(Value), 0, ifelse(!qual4agg,0,1 )),
    aggFunc = ifelse(isAngelKPI, "pct", "median"), 
    evaltext=sprintf("'%s' %s", Value, aggCond),
    evalVecTF=eval_vec(parse(text = evaltext)),
    Val2agg = ifelse(!qual4agg, NA, 
                     ifelse(isAngelKPI == FALSE, Value,
                            ifelse(evalVecTF,1,0)))) 

# ---- prepare all numerical KPIs for aggregation
df <- 
  dataset[, unique(c(key_cols,numKPIcols, ctrl_cols, Agg_cond_cols,cond_cols))] %>% 
  mutate_if(is.logical, as.character) %>%
  pivot_longer(-c(key_cols, ctrl_cols, Agg_cond_cols,cond_cols), names_to = "QI", values_to = "Value") %>%
  left_join(angel_conds, multiple = "all") %>%
  filter(!is.na(isAngelKPI)) %>%
  mutate(qual4agg = NA,
         qual4agg = ifelse(QI!="door_to_needle",qual4agg, ifelse(stroke_type=='ischemic' & thrombolysis == TRUE & hospital_stroke != TRUE & first_hospital == TRUE, TRUE,FALSE)),
         qual4agg = ifelse(QI!="door_to_groin",qual4agg, ifelse(stroke_type=='ischemic' & thrombectomy == TRUE & hospital_stroke != TRUE & first_hospital == TRUE, TRUE,FALSE)),         
         # qual4agg = Reduce(`&`, lapply(allConds, function(x) eval(parse(text = x), envir = .))),  
         isMissingData = ifelse(!is.na(Value), 0, ifelse(!qual4agg,0,1 )),
         aggFunc = ifelse(isAngelKPI, "pct", "median"), 
         evaltext=sprintf("'%s' %s", Value, aggCond),
         evalVecTF=eval_vec(parse(text = evaltext)),
         Val2agg = ifelse(!qual4agg, NA, 
                          ifelse(isAngelKPI == FALSE, Value,
                                 ifelse(evalVecTF,1,0))))%>%
  rbind(df)


# aggregation -------------------------------------------------------------
# create quarterly hospital aggregates of numVars
agg_dataNum <- df %>%
  group_by(QI, nameOfAggr, h_country, h_name, year, quarter, YQ, isAngelKPI, aggFunc) %>%
  summarise(
    Value = ifelse(first(isAngelKPI) == FALSE,
                   median(Val2agg, na.rm = TRUE),
                   ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
    ),
    sd = sd(Val2agg, na.rm = TRUE),
    data_Pts = sum(!is.na(Val2agg)),
    data_missing = sum(isMissingData)
  )
# %>%
#  # pivot_longer(cols = median:coverage_pct, names_to = "agg_function", values_to = "Value")
# createAggsT <- function(df, colName){
#   df %>% select(QI, nameOfAggr, colName) %>% dplyr::group_by(nameOfAggr, colName)%>% rename( gg=colName) %>% mutate(xyz=colName) %>% View()
# }


# add yearly hospital aggregates of numVars
# function_name <- function(arg_1, arg_2, ...) {
# Function body
# }

addYearlyHospAggregates <- function(df, angel_conds, agg_dataNum) {
  agg_dataNum <- df %>%
    left_join(angel_conds) %>%
    group_by(QI, nameOfAggr, h_country, h_name, year, isAngelKPI, aggFunc) %>%
    summarise(
      Value = ifelse(first(isAngelKPI) == FALSE,
                     median(Val2agg, na.rm = TRUE),
                     ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
      ),
      sd = sd(Val2agg, na.rm = TRUE),
      data_Pts = sum(!is.na(Val2agg)),
      data_missing = sum(isMissingData)
    ) %>%
    mutate(
      quarter = "all",
      YQ = as.character(year)
    ) %>%
    rbind(agg_dataNum)
  return(agg_dataNum)
}

agg_dataNum <- addYearlyHospAggregates(df, angel_conds, agg_dataNum)

# ---- add quarterly country aggregates of numVars
agg_dataNum <- df %>%
  left_join(angel_conds) %>%
  group_by(QI, nameOfAggr, h_country, year, quarter, isAngelKPI, aggFunc) %>%
  summarise(
    Value = ifelse(first(isAngelKPI) == FALSE,
                   median(Val2agg, na.rm = TRUE),
                   ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
    ),
    sd = sd(Val2agg, na.rm = TRUE),
    data_Pts = sum(!is.na(Val2agg)),
    data_missing = sum(isMissingData)
  ) %>%
  mutate(
    h_name = "all",
    YQ = paste(year, quarter)
  ) %>%
  rbind(agg_dataNum)

# ---- add yearly country aggregates of numVars
agg_dataNum <- df %>%
  left_join(angel_conds) %>%
  group_by(QI, nameOfAggr, h_country, year, isAngelKPI, aggFunc) %>%
  summarise(
    Value = ifelse(first(isAngelKPI) == FALSE,
                   median(Val2agg, na.rm = TRUE),
                   ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
    ),
    sd = sd(Val2agg, na.rm = TRUE),
    data_Pts = sum(!is.na(Val2agg)),
    data_missing = sum(isMissingData)
  ) %>%
  mutate(
    quarter = "all",
    h_name = "all",
    YQ = as.character(year)
  ) %>%
  rbind(agg_dataNum)

agg_dataNum <- agg_dataNum %>%
  mutate(
    isKPI = ifelse(QI %in% KPIs, TRUE, FALSE),
    pct_missing = ifelse(data_Pts == 0, 0, round(data_missing / (data_missing + data_Pts) * 100, 1)),
    subGroupVal = NA,
    subGroupVal = as.character(subGroupVal),
    subGroup = NA
  )

# function to return p-values and eta-squared for summarizing sub-group variable values
calc_eta_sq_pval <- function(df, var1, var2) {
  lm_res <- tryCatch(lm(paste(var1, "~", var2, collapse = " "), data = df),
                     error = function(e) NULL)
  if (is.null(lm_res)) {
    eta_sq <- NA
    p_val <- NA
  } else {
    ss_total <- sum((df[[var1]] - mean(df[[var1]]))^2)
    ss_residual <- sum(lm_res$residuals^2)
    eta_sq <- 1 - ss_residual/ss_total
    p_val <- summary(lm_res)$coefficients[2, 4]
  }
  list(eta_sq = eta_sq, p_val = p_val)
}

# sub-analysis function --------------------------------------------------

createAggs <- function(df, colName) {
  # add quarterly hospital aggregates of numVars
  agg_data <- df %>%
   group_by(QI, nameOfAggr, h_country, h_name, year, quarter, YQ, isAngelKPI, aggFunc, !!sym(colName)) %>%
   summarise(
     Value = ifelse(first(isAngelKPI) == FALSE,
       median(Val2agg, na.rm = TRUE),
       ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
     ),
     # eta_sq_p_val = list(calc_eta_sq_pval(., var1 = Val2agg, var2 = colName)),
     data_Pts = sum(!is.na(Val2agg)),
     data_missing = sum(isMissingData),
     sd = sd(Val2agg, na.rm = TRUE)
   ) %>%
   # unnest(eta_sq_p_val) %>%
   rename(subGroupVal = colName) %>%
   mutate(subGroup = colName)
  
  # add yearly hospital aggregates of numVars
  agg_data <- df %>%
    left_join(angel_conds) %>%
    group_by(QI, nameOfAggr, h_country, h_name, year, isAngelKPI, aggFunc, !!sym(colName)) %>%
    summarise(
      Value = ifelse(first(isAngelKPI) == FALSE,
                     median(Val2agg, na.rm = TRUE),
                     ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
                     # eff_size = kruskal_effsize(Val2agg ~ !!sym(colName)),
      ),
      data_Pts = sum(!is.na(Val2agg)),
      data_missing = sum(isMissingData),
      sd = sd(Val2agg, na.rm = TRUE)
    ) %>%
    mutate(
      quarter = "all",
      YQ = as.character(year)
    ) %>%
    rename(subGroupVal = colName) %>%
    mutate(subGroup = colName) %>%
    rbind(agg_data)
  
  # add quarterly country aggregates of numVars
  agg_data <- df %>%
    left_join(angel_conds) %>%
    group_by(QI, nameOfAggr, h_country, year, quarter, isAngelKPI, aggFunc, !!sym(colName)) %>%
    summarise(
      Value = ifelse(first(isAngelKPI) == FALSE,
                     median(Val2agg, na.rm = TRUE),
                     ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))#,
      ),
      # SD_value = sd(Val2agg, na.rm = TRUE)
      data_Pts = sum(!is.na(Val2agg)),
      data_missing = sum(isMissingData),
      sd = sd(Val2agg, na.rm = TRUE)
    ) %>%
    mutate(
      h_name = "all",
      YQ = paste(year, quarter)
    ) %>%
    rename(subGroupVal = colName) %>%
    mutate(subGroup = colName) %>%
    rbind(agg_data)
  
  # add yearly country aggregates of numVars
  agg_data <- df %>%
    left_join(angel_conds) %>%
    group_by(QI, nameOfAggr, h_country, year, isAngelKPI, aggFunc, !!sym(colName)) %>%
    summarise(
      Value = ifelse(first(isAngelKPI) == FALSE,
                     median(Val2agg, na.rm = TRUE),
                     ifelse(is.nan(mean(Val2agg, na.rm = TRUE)), NA, round(mean(Val2agg, na.rm = TRUE) * 100, 1))
      ),
      # SD_value = sd(Val2agg, na.rm = TRUE),
      # median = median(Value, na.rm = TRUE),
      data_Pts = sum(!is.na(Val2agg)),
      data_missing = sum(isMissingData),
      sd = sd(Val2agg, na.rm = TRUE)
    ) %>%
    mutate(
      quarter = "all",
      h_name = "all",
      YQ = as.character(year)
    ) %>%
    rename(subGroupVal = colName) %>%
    mutate(subGroup = colName) %>%
    rbind(agg_data)
  
  agg_data <- agg_data %>%
    mutate(
      isKPI = ifelse(QI %in% KPIs, TRUE, FALSE),
      pct_missing = ifelse(data_Pts == 0, 0, round(data_missing / (data_missing + data_Pts) * 100, 1))
    )
  
  return(agg_data)
}

createAggsLapply <- function(df, vars) {
  lapply(vars, function(v) createAggs(df, v))
}

# add aggregates for all subgroup variables
agg_dataNum <- rbind(agg_dataNum, do.call(rbind, createAggsLapply(df, subgroupVars)))

# agg_dataNum <- rbind(agg_dataNum, createAggs(df, "first_hospital"))
# agg_dataNum <- rbind(agg_dataNum, createAggs(df, "prenotification"))
# agg_dataNum <- rbind(agg_dataNum, createAggs(df, "imaging_done"))
#
# # agg_dataNum$subGroupVal <- as.character(agg_dataNum$subGroupVal)
#
# agg_dataNum <- rbind(agg_dataNum, createAggs(df, "stroke_type"))
# agg_dataNum <- rbind(agg_dataNum, createAggs(df, "gender"))
#

# set up the grid so we know all the data that should exist and might be missing from the aggregates
timeGrid <- as_tibble(unique(agg_dataNum[, c("year", "quarter")]))
hospitalGrid <- as_tibble(unique(agg_dataNum[, c("h_country", "h_name")]))
NumMeasureGrid <- as_tibble(unique(agg_dataNum[, c("nameOfAggr")]))
# NumMeasureGrid<-as_tibble(unique(agg_dataNum[,c('QI','agg_function')]))
full.grid <- timeGrid %>%
  merge(hospitalGrid) %>%
  merge(NumMeasureGrid) %>%
  mutate(YQ = ifelse(quarter == "all", as.character(year), paste(year, quarter))) %>%
  unique()

# creating full grid that includes all measures reported and those missing with NA
agg_dataNum <- left_join(full.grid, agg_dataNum)

# merge in the angel awards thresholds and mark YearAggregates
agg_dataNum <- angel_awards %>%
  filter(isAngelKPI) %>%
  select(nameOfAggr, gold, platinum, diamond) %>%
  # rename(QI = nameOfAggr) %>%
  right_join(agg_dataNum) %>%
  mutate(
    isYearAgg = ifelse(quarter == "all", TRUE, FALSE),
    isCountryAgg = ifelse(h_name == "all", TRUE, FALSE)
  ) %>%
  arrange(h_country, h_name, year, quarter, QI)

# adding awards to aggregation data
agg_dataNum <- agg_dataNum %>%
  filter(isAngelKPI) %>%
  mutate(angelAwardLevel = awardHandler_v(Value, gold, platinum, diamond)) %>%
  right_join(agg_dataNum)

agg_dataNum <- agg_dataNum %>%
  pivot_wider(
    names_from = angelAwardLevel,
    names_glue = "qual4{angelAwardLevel}",
    values_from = angelAwardLevel,
    values_fn = list(angelAwardLevel = ~1), values_fill = list(angelAwardLevel = 0)
  ) %>%
  select(-c(gold, platinum, diamond))

# adding national comparisons to aggregation data
agg_dataNum <- agg_dataNum %>%
  filter(isCountryAgg == T, isKPI == T) %>%
  select(h_country, nameOfAggr, year, quarter, subGroup, subGroupVal, Value) %>%
  rename("C_Value" = Value) %>%
  right_join(agg_dataNum) %>%
  mutate(
    MedianGeg_C = ifelse(Value >= C_Value, 1, 0),
    diffFromC = Value - C_Value
  )

# remove duplicate rows
agg_dataNum <- unique(agg_dataNum) %>%
  arrange(h_country, h_name, year, quarter, QI)

# add temporally derived data - first time above thresholds
agg_dataNum <- agg_dataNum %>%
  arrange(h_country, h_name, year, quarter, QI) %>%
  group_by(h_country, h_name, year, quarter, QI) %>%
  mutate(
    CSoAboveCountry = cumsum(ifelse(is.na(MedianGeg_C), 0, MedianGeg_C)),
    CSoAboveDiamond = cumsum(qual4Diamond),
    CSoAbovePlatinum = cumsum(qual4Platinum) + CSoAboveDiamond,
    CSoAboveGold = cumsum(qual4Gold) + CSoAbovePlatinum,
    is1stgeqC = ifelse(CSoAboveCountry == 1 & MedianGeg_C == 1, 1, 0),
    is1stGold = ifelse(CSoAboveGold == 1 & qual4Gold == 1, 1, 0),
    is1stPlat = ifelse(CSoAbovePlatinum == 1 & qual4Platinum == 1, 1, 0),
    is1stDiam = ifelse(CSoAboveDiamond == 1 & qual4Diamond == 1, 1, 0),
  )

# add the changes in QI from previous quarter holding data
agg_dataNum <- 
  agg_dataNum %>%
  filter(!isYearAgg,!is.na(Value)) %>% 
  arrange(h_country, h_name, nameOfAggr, subGroup, subGroupVal, YQ) %>%
  dplyr::group_by(h_country, h_name,  nameOfAggr, subGroup, subGroupVal)  %>% 
  mutate(diffFromPrevQwithData = Value - lag(Value,default = NA))%>%
  select(h_country, h_name,nameOfAggr,subGroup, subGroupVal, YQ,Value, diffFromPrevQwithData) %>%
  right_join(agg_dataNum)

# add the changes in QI from the same quarter year to year  (if no data available go back until you find some holding data)
agg_dataNum <- 
  agg_dataNum %>%
  filter(!isYearAgg,!is.na(Value)) %>% 
  arrange(h_country, h_name, nameOfAggr, subGroup, subGroupVal, quarter, year) %>%
  dplyr::group_by(h_country, h_name,  nameOfAggr, subGroup, subGroupVal, quarter)  %>% 
  mutate(diffFromQprevYearwithData = Value - lag(Value,default = NA))%>%
  select(h_country, h_name,nameOfAggr,subGroup, subGroupVal, quarter, year, Value, diffFromQprevYearwithData) %>%
  right_join(agg_dataNum)

agg_dataNum <- 
  agg_dataNum %>%
  left_join(angel_awards[,c("nameOfAggr","isBetter")]) %>%
  mutate(
    isAsGoodOrBetterThanCountry=eval_vec(parse(text = sprintf("%s %s %s", Value, isBetter, C_Value))),
    isAsGoodOrBetterThanLastYearSameQuarter=eval_vec(parse(text = sprintf("%s %s %s", diffFromQprevYearwithData, isBetter, "0" ))),
    isAsGoodOrBetterThanLastQuarter=eval_vec(parse(text = sprintf("%s %s %s", diffFromPrevQwithData, isBetter, "0")))
  ) 

agg_DataAcrossSubGroups <- agg_dataNum %>%
  filter(aggFunc=="median") %>%
  dplyr::group_by(QI, nameOfAggr, h_country, h_name, year, quarter, YQ, isAngelKPI, aggFunc,subGroup) %>%
  dplyr::summarize(SubGroupRange=ifelse(all(is.na(Value)),NA, max(Value,na.rm=TRUE)-min(Value,na.rm=TRUE))) %>%
  dplyr::rename(subGroupForRange=subGroup) 

agg_dataNum<-agg_dataNum %>% 
  filter(is.na(subGroup),aggFunc=="median") %>%
  right_join(agg_DataAcrossSubGroups,multiple = "all") %>% 
  mutate(sd = ifelse(aggFunc=="pct",NA,sd),
    subGrpRangeInSDs = ifelse(is.na(sd) | is.na(SubGroupRange),NA,
                                   ifelse(SubGroupRange==0,0,
                                          ifelse(sd==0, 10 ,SubGroupRange/sd)))) %>% filter(!is.na(Value)) %>% 
  rbind(agg_dataNum) %>%
  arrange(h_country, h_name, nameOfAggr, subGroup, subGroupVal, YQ) 

agg_dataNum <- agg_dataNum %>% 
  mutate(sd = ifelse(aggFunc=="pct",NA,sd),
    subGroup = ifelse(is.na(subGroupForRange),subGroup,subGroupForRange))



agg_dataNum <- 
  agg_dataNum %>%
  select(h_country,
         h_name,
         isCountryAgg,
         nameOfAggr,
         QI,
         isKPI,
         isAngelKPI,
         subGroup,
         subGroupVal,
         quarter,
         isYearAgg,
         year,
         YQ,
         aggFunc,
         Value,
         sd,
         data_Pts,
         data_missing,
         pct_missing,
         subGroupForRange,
         subGrpRangeInSDs,
         isBetter,
         isAsGoodOrBetterThanLastYearSameQuarter,
         isAsGoodOrBetterThanLastQuarter,
         isAsGoodOrBetterThanCountry,
         C_Value,
         diffFromC,
         is1stgeqC,
         CSoAboveCountry,
         qual4Gold,
         is1stGold,
         CSoAboveGold,
         qual4Platinum,
         is1stPlat,
         CSoAbovePlatinum,
         qual4Diamond,
         is1stDiam,
         CSoAboveDiamond
  )


options("scipen" = 999)

# EXAMPLE GGPLOT  ---------------------------------------------------------

# preparing data for plotting

dfx <- agg_dataNum

dfx <- agg_dataNum %>%
  filter(
    isYearAgg == FALSE, isAngelKPI == TRUE,
    h_name == "General", nameOfAggr == "dnt_leq_60",
    is.na(subGroup), !is.na(YQ), year == "2019"
  ) %>%
  mutate(condColor = ifelse(is1stDiam > 0, "#7cd461", "#4299f5")) %>%
  ggplot(aes(x = YQ, y = Value, group = 1)) +
  geom_line(color = "#42aaf5") +
  geom_line(aes(y = C_Value), color = "grey") +
  geom_point(aes(size = data_Pts, color = condColor)) +
  scale_color_identity() +
  theme_minimal() +
  ylab("dnt less or equal to 60, in %.")


agg_dataNum %>%
  filter(
    isYearAgg == FALSE, isAngelKPI == TRUE,
    h_name == "General", nameOfAggr == "dnt_leq_60",
    subGroup == "gender", !is.na(YQ), year == "2019"
  ) %>%
  mutate(gender = factor(subGroupVal)) %>%
  ggplot(aes(x = YQ, y = Value, group = gender, colour = gender)) +
  geom_line(position = position_dodge(.2)) +
  geom_point(aes(size = data_Pts), position = position_dodge(.2)) +
  theme_minimal() +
  ylab("dnt less or equal to 60, in %.")
