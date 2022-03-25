## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_NET_NEW Adjustments by Aaron Chafetz (USAID)
##      TX_NET_NEW Adjustment FXN by Andrea Stewart (CDC)
##      TX_ML & RTT Disaggregations edits for MER 2.6 by Femi Akinmade (CDC)
## CREATION DATE: 6/8/2020
## UPDATE: 02/23/2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)
library(readxl)

rm(list=ls())

source("https://raw.githubusercontent.com/qlx6/Continuity_in_TX/main/MSD_TSD_FXNS_V5_mer2.6.R.R")
#source("C:/r_archive/r_projects/Waterfall_Dataset_Generation/waterfall R scripts_mer2.6/MSD_TSD_FXNS_V5_mer2.6.R")

#options("install.lock"=FALSE)
## ==================== MAIN ====================

#setwd("C:/r_archive/r_projects/Waterfall_Dataset_Generation/data_waterfall/data") # Folder 
work_dir <- "C:/Users/qlx6/Downloads/MSD"
setwd(work_dir)
# setwd(choose.dir())

period <- "PostClean_FY22Q1"
date <- Sys.Date()

# Reading in all txt from repository (Ensure one download in repo)
ou_list <- list.files(pattern = ".*.txt")

#setwd("C:/Users/qlx6/Downloads/lesothoos")
#ou_list <- list.files(pattern = )
# ========================================================================================================
# Reading in individual downloads
# ou_list <- list.files(pattern = "MER_Structured_Datasets_Site_IM_FY19-22_20210917_v2_1_Uganda.txt")
# ========================================================================================================

ptm <- proc.time()
for (ou in ou_list) {
  
  ## Import
  ou_df <- msd_df(ou)
  
  ## Adjusted TX Generate
  df0 <- txs_adj_generate(ou_df, # msd_txt
                          "2019_qtr4", # prevR
                          "2020_qtr1", # currR
                          "2020_targets") # currT
  
  df <- txs_adj_generate(ou_df, 
                         "2020_qtr1", 
                         "2020_qtr2", 
                         "2020_targets") 
  
  df1 <- txs_adj_generate(ou_df,
                          "2020_qtr2",
                          "2020_qtr3",
                          "2020_targets")
  
  df2 <- txs_adj_generate(ou_df,
                          "2020_qtr3",
                          "2020_qtr4",
                          "2020_targets")
  
  df3 <- txs_adj_generate(ou_df,
                          "2020_qtr4",
                          "2021_qtr1",
                          "2021_targets")
  
  df4 <- txs_adj_generate(ou_df,
                          "2021_qtr1",
                          "2021_qtr2",
                          "2021_targets")
  
  df5 <- txs_adj_generate(ou_df,
                          "2021_qtr2",
                          "2021_qtr3",
                          "2021_targets")
  
  df6 <- txs_adj_generate(ou_df,
                          "2021_qtr3",
                          "2021_qtr4",
                          "2021_targets")
  
  df7 <- txs_adj_generate(ou_df,
                          "2021_qtr4",
                          "2022_qtr1",
                          "2022_targets")
  
  ## Waterfall Generate
#  df0a <- txs_generate(ou_df, # msd_txt
#                       "2019_qtr4", # prevR
#                       "2020_qtr1", # currR
#                       "2020_targets") # currT
  
#  dfa <- txs_generate(ou_df, 
#                      "2020_qtr1", 
#                      "2020_qtr2", 
#                      "2020_targets") 
  
#  df1a <- txs_generate(ou_df,
#                       "2020_qtr2",
#                       "2020_qtr3",
#                       "2020_targets")
  
#  df2a <- txs_generate(ou_df,
#                       "2020_qtr3",
#                       "2020_qtr4",
#                       "2020_targets")
  
  df3a <- txs_generate(ou_df,
                       "2020_qtr4",
                       "2021_qtr1",
                       "2021_targets")
  
  df4a <- txs_generate(ou_df,
                       "2021_qtr1",
                       "2021_qtr2",
                       "2021_targets")
  
  df5a <- txs_generate(ou_df,
                       "2021_qtr2",
                       "2021_qtr3",
                       "2021_targets")
  
  df6a <- txs_generate(ou_df,
                       "2021_qtr3",
                       "2021_qtr4",
                       "2021_targets")
  
  df7a <- txs_generate(ou_df,
                       "2021_qtr4",
                       "2022_qtr1",
                       "2022_targets")
  
  ## Adjusted TX Column Order
  ou_ou <- bind_rows(list(df3, df4, df5, df6, df7)) %>% 
    mutate(period = paste0("FY",substr(period,3,4),"Q",substr(period,9,9)))
  
  
  shell_df <- c("operatingunit",
                "countryname",
                "snu1",
                "snuprioritization",
                "psnu",
                "psnuuid",
                "sitetype",
                "sitename",
                "orgunituid",
                "fundingagency",
                "primepartner",
                "mech_name",
                "mech_code",
                "facility",
                "facilityprioritization",
                "period",
                "TX_CURR_Now_R",
                "tx_curr",
                "tx_net_new",
                "tx_curr_lag_site",
                "tx_net_new_adj",
                "tx_net_new_adj_plus",
                "tx_xfer",
                "flag_loneobs",
                "flag_multimech_site",
                "last_obs_site",
                "last_obs_sitexmech",
                "flag_end_sitexmech",
                "end_type",
                "agency_exiting",
                "agency_inheriting",
                "method")
  
  missing <- setdiff(shell_df, names(ou_ou))
  ou_ou[missing] <- NA
  ou_ou <- ou_ou[shell_df] # Column Order
  
  
  
  
  ## Waterfall Column Order

  
  ou_ou2 <- bind_rows(list(df3a, df4a, df5a, df6a, df7a)) %>% 
    mutate(period = paste0("FY",substr(period,3,4),"Q",substr(period,9,9))) %>% 
    dplyr::rename(#TX_ML_Now_R = TX_ML_Now_R,
                  TX_ML_Died_Now_R = `TX_ML_No Contact Outcome - Died_Now_R`,
                  `TX_ML_Interruption <3 Months Treatment_Now_R` = `TX_ML_No Contact Outcome - Interruption in Treatment <3 Months Treatment_Now_R`,
                  `TX_ML_Interruption 3+ Months Treatment_Now_R` = `TX_ML_No Contact Outcome - Interruption in Treatment 3+ Months Treatment_Now_R`,
                  `TX_ML_Interruption 3-5 Months Treatment_R` = `TX_ML_No Contact Outcome - Interruption in Treatment 3-5 Months Treatment_Now_R`,
                  `TX_ML_Interruption 6+ Months Treatment_R` = `TX_ML_No Contact Outcome - Interruption In Treatment 6+ Months Treatment_Now_R`,
                  `TX_ML_Refused Stopped Treatment_Now_R` = `TX_ML_No Contact Outcome - Refused Stopped Treatment_Now_R`,
                  `TX_ML_Transferred Out_Now_R` = `TX_ML_No Contact Outcome - Transferred Out_Now_R`,
                  TX_RTT_Now_R = TX_RTT_NA_Now_R,
                  # Commented RTT disaggs for Namibia because they are not in the Other Disaggregates column
                  `TX_RTT_ <3 Months Interruption` = `TX_RTT_No Contact Outcome - Interruption in Treatment <3 Months Interruption_Now_R`,
                  `TX_RTT_3-5 Months Interruption` = `TX_RTT_No Contact Outcome - Interruption in Treatment 3-5 Months Interruption_Now_R`,
                  `TX_RTT_6+ Months Interruption` = `TX_RTT_No Contact Outcome - Interruption In Treatment 6+ Months Interruption_Now_R`
                  )
  
  shell_df1 <- c("operatingunit",                                                         
                 "countryname",                                                           
                 "snu1",                                                                  
                 "snuprioritization",                                                     
                 "psnu",                                                                  
                 "psnuuid",                                                               
                 "sitetype",                                                              
                 "sitename",                                                              
                 "orgunituid",                                                            
                 "fundingagency",                                                         
                 "primepartner",                                                          
                 "mech_name",                                                             
                 "mech_code",
                 "facility",
                 "facilityprioritization",                                                
                 "age_type",                                                                   
                 "age",                                                                   
                 "sex", 
                 "indicatortype",
                 "period",
                 "TX_CURR_Prev_R",
                 "TX_CURR_Now_R",
                 "TX_CURR_Now_T",
                 "TX_NEW_Prev_R",
                 "TX_NEW_Now_R",
                 "TX_NEW_Now_T",
                 #"TX_ML_Now_R",
                 "TX_ML_Interruption <3 Months Treatment_Now_R",
                 "TX_ML_Interruption 3+ Months Treatment_Now_R",
                 "TX_ML_Interruption 3-5 Months Treatment_R",
                 "TX_ML_Interruption 6+ Months Treatment_R",
                 "TX_ML_Died_Now_R",
                 "TX_ML_Refused Stopped Treatment_Now_R",
                 "TX_ML_Transferred Out_Now_R",
                 "TX_RTT_Now_R",
                 "TX_RTT_ <3 Months Interruption",
                 "TX_RTT_3-5 Months Interruption",
                 "TX_RTT_6+ Months Interruption")

  
  

  missing1 <- setdiff(shell_df1, names(ou_ou2))
  ou_ou2[missing1] <- NA
  ou_ou2 <- ou_ou2[shell_df1] # Column Order
  
  ## Export
  ou_name <- unique(ou_ou$operatingunit)
  

  openxlsx::write.xlsx(ou_ou2, 
                       file=paste("C:/Users/qlx6/Downloads/PostClean_2022_Q1_V1_Datasets/COT_WF", 
                                  ou_name, period, date,"_1900hrs.xlsx", sep = "_"), 
                       keepNA = FALSE, asTable = TRUE) 
    
}

proc.time() - ptm


