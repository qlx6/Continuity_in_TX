## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_ML & RTT disaggregation inclusion for MER 2.6 by Femi Akinmade (CDC)
## CREATION DATE: 4/30/2021
## UPDATE: 02/23/2022
## UPDATE: For Global - all OU PSNUxIM
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)

rm(list=ls())

source("https://raw.githubusercontent.com/qlx6/Continuity_in_TX/main/MSD_TSD_FXNS_V5_PSNU_mer2.6.R")
#source("C:/r_archive/r_projects/Waterfall_Dataset_Generation/waterfall R scripts_mer2.6/MSD_TSD_FXNS_V5_PSNU_mer2.6.R")

## ==================== MAIN ====================
setwd("C:/Users/qlx6/Downloads/MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1") # Folder 



period <- "Clean_FY22Q1"
date <- Sys.Date()

ou_list <- list.files(pattern = ".*.txt")


ptm <- proc.time()
for (ou in ou_list) {
  
  ## Import
  ou_df <- msd_df(ou)
  
  ## Waterfall Generate
  
#  dfa1 <- txs_generate(ou_df,
#                     "2019_qtr1",
#                     "2019_qtr2",
#                     "2019_targets")
  
#  dfa2 <- txs_generate(ou_df,
#                       "2019_qtr2",
#                       "2019_qtr3",
#                       "2019_targets")
  
#  dfa3 <- txs_generate(ou_df,
#                       "2019_qtr3",
#                       "2019_qtr4",
#                       "2019_targets")
  
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
#  
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
  
#dfa1, dfa2, dfa3, # removed from the bind below  
  
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
      TX_RTT_Now_R = TX_RTT_NA_Now_R,#,
      `TX_RTT_<3 Months Interruption` = `TX_RTT_No Contact Outcome - Interruption in Treatment <3 Months Interruption_Now_R`,
      `TX_RTT_3-5 Months Interruption` = `TX_RTT_No Contact Outcome - Interruption in Treatment 3-5 Months Interruption_Now_R`,
      `TX_RTT_6+ Months Interruption` = `TX_RTT_No Contact Outcome - Interruption In Treatment 6+ Months Interruption_Now_R`) #%>% 
  
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
                 #"standardizeddisaggregate", # added
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
                 "TX_RTT_<3 Months Interruption",
                 "TX_RTT_3-5 Months Interruption",
                 "TX_RTT_6+ Months Interruption"
                 )

  
  missing1 <- setdiff(shell_df1, names(ou_ou2))
  ou_ou2[missing1] <- NA
  ou_ou2 <- ou_ou2[shell_df1] # Column Order
  
  ## Export
  ou_name <- "PSNUxIM"
  
  #openxlsx::write.xlsx(ou_ou2, file=paste("C:/Users/qlx6/PEPFAR/ICPI - Treatment Continuity Dashboard", ou_name, period,"_V1.xlsx", sep = ""), 
  #                     keepNA = FALSE, asTable = TRUE)
  
  
  #openxlsx::write.xlsx(ou_ou2, file=paste("C:/Users/qlx6/OneDrive - CDC/TSD - Yee, Randy (CDC_DDPHSIS_CGH_DGHT)'s files/Waterfall", ou_name, period,"_V1.xlsx", sep = ""), 
  #                     keepNA = FALSE, asTable = TRUE)

  openxlsx::write.xlsx(ou_ou2, 
                       file = paste("C:/Users/qlx6/Downloads/PreClean_2022_Q1_V1_Datasets/WF_Global", 
                                    ou_name, period, date,"_0400PM.xlsx", sep = "_"), 
                       keepNA = FALSE, asTable = TRUE) 
  
}

proc.time() - ptm


