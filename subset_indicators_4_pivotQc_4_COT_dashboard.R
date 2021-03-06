## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: Aux Script to subset MSD into TX_CURR, TX_NEW, TX_ML & TX_RTT
## AUTHOR: Femi Akinmade (qlx6@cdc.gov)
## DESCRIPTION: MSD into TX_CURR, TX_NEW, TX_ML & TX_RTT 
## CREATION DATE: 
## UPDATE: 02/23/2022
## DATASET: PSNUxIM
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(openxlsx)
library(readxl)
library(dplyr)

rm(list=ls())
period <- "2022Q1"
date <- Sys.Date()

# ------- Read-in PSNUxIM MSD ------- #
msd <- read_tsv(file.choose(), 
               trim_ws = TRUE,
               col_types = cols(.default = col_character(), 
                                targets = col_double(),
                                qtr1 = col_double(),
                                qtr2 = col_double(),
                                qtr3 = col_double(),
                                qtr4 = col_double(),
                                cumulative = col_double()))

TX_CURR_msd <- msd %>% 
  filter(indicator %in% c("TX_CURR"))

TX_NEW_msd <- msd %>% 
  filter(indicator %in% c("TX_NEW"))

TX_NET_NEW_msd <- msd %>% 
  filter(indicator %in% c("TX_NET_NEW"))

TX_ML_msd <- msd %>% 
  filter(indicator %in% c("TX_ML"))

TX_RTT_msd <- msd %>% 
  filter(indicator %in% c("TX_RTT"))

TX_RTT_msd <- msd %>% 
  filter(indicator %in% c("TX_RTT"))

HTS_INDEX_msd <- msd %>% 
  filter(indicator %in% c("HTS_INDEX"))






indicators <- unique(msd$indicator)
for (i in 1:length(msd)){ 
  temp <- msd[msd$indicator==indicator[i],]
  #more things to do with temp
}
=


inds <- as.character(unique(msd$indicator))

    for (i in 1:length(inds)) {
    assign(inds[i], inds[[i]])
    }




setwd("C:/Users/qlx6/Downloads/PreClean_2022_Q1_V1_Datasets/QC")
write.csv(TX_CURR_msd, file = "TX_CURR_msd_test.csv")
write_csv(TX_NEW_msd, file = "TX_NEW_msd_test.csv")
write_csv(TX_RTT_msd, file = "TX_RTT_msd_test.csv")
write_csv(TX_ML_msd, file = "TX_ML_msd_test.csv") 

write_csv(ind, file = "indicators.csv") 





asia_ml <- msd %>% 
  filter(operatingunit %in% c("Asia Region") & indicator %in% c("TX_ML"))


# - Extract Indonesia & Mozambique from msd MSD ------- #
# - Both do not require the Age Aggregate standardized disag
# - for TX_CURR & TX_NEW

# - OU Subsets - #
ind <- msd %>% 
  filter(countryname %in% c("Indonesia"))

ind_curr_new <- ind %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"))


ind_less_tx <- ind %>% 
  filter(indicator %in% c("TX_ML", "TX_RTT"))




moz <- msd %>% 
  filter(countryname %in% c("Mozambique"))

moz_curr_new <- ind %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"))

ind_less_tx <- ind %>% 
  filter(indicator %in% c("TX_ML", "TX_RTT"))




less_ind_moz_msd <- msd %>% 
  filter(countryname %in% c("Angola",
                            "Botswana",
                            "Burundi",
                            "Cameroon",
                            "Cote d'Ivoire",
                            "Democratic Republic of the Congo",
                            "Dominican Republic",
                            "Eswatini",
                            "Ethiopia",
                            "Haiti",
                            "Kenya",
                            "Lesotho",
                            "Malawi",
                            "Namibia",
                            "Nigeria",
                            "Rwanda",
                            "South Africa",
                            "South Sudan",
                            "Tanzania",
                            "Uganda",
                            "Ukraine",
                            "Vietnam",
                            "Zambia",
                            "Zimbabwe",
                            "Benin",
                            "Burkina Faso",
                            "Ghana",
                            "Liberia",
                            "Mali",
                            "Senegal",
                            "Sierra Leone",
                            "Togo",
                            "Burma",
                            "India",
                            "Kazakhstan",
                            "Kyrgyzstan",
                            "Laos",
                            "Nepal",
                            "Papua New Guinea",
                            "Philippines",
                            "Tajikistan",
                            "Thailand",
                            "Barbados",
                            "Brazil",
                            "El Salvador",
                            "Guatemala",
                            "Guyana",
                            "Honduras",
                            "Jamaica",
                            "Nicaragua",
                            "Panama",
                            "Trinidad and Tobago"))

# - Write out PSNU ~ Indonesia & Mozambique subset -------- #
# --------------------------------------------------------- #
write.table(ind, 
            file = "C:/Users/qlx6/Downloads/MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1/ind_msd.txt", 
            sep = "\t",
            row.names = TRUE, col.names = NA)

write.table(moz, 
            file = "C:/Users/qlx6/Downloads/MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1/ind_msd.txt", 
            sep = "\t",
            row.names = TRUE, col.names = NA)

write.table(less_ind_moz_msd, 
            file = "C:/Users/qlx6/Downloads/MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1/other_msd.txt", 
            sep = "\t",
            row.names = TRUE, col.names = NA)

# =================================================================== #
# =================================================================== #


# ------ Bind Global WF without SA wc SA WF ------ #
# ------------------------------------------------ #
setwd("C:/Users/qlx6/Downloads/PreClean_2022_Q1_V1_Datasets")
glo_wf <-   read_excel("Global_COT_PSNUxIM__FY22Q1_2022-02-24__0520PM.xlsx")

less_ind_moz_wf <- glo_wf %>%
  filter(countryname %in% c("Angola",
                            "Botswana",
                            "Burundi",
                            "Cameroon",
                            "Cote d'Ivoire",
                            "Democratic Republic of the Congo",
                            "Dominican Republic",
                            "Eswatini",
                            "Ethiopia",
                            "Haiti",
                            "Kenya",
                            "Lesotho",
                            "Malawi",
                            "Namibia",
                            "Nigeria",
                            "Rwanda",
                            "South Africa",
                            "South Sudan",
                            "Tanzania",
                            "Uganda",
                            "Ukraine",
                            "Vietnam",
                            "Zambia",
                            "Zimbabwe",
                            "Benin",
                            "Burkina Faso",
                            "Ghana",
                            "Liberia",
                            "Mali",
                            "Senegal",
                            "Sierra Leone",
                            "Togo",
                            "Burma",
                            "India",
                            "Kazakhstan",
                            "Kyrgyzstan",
                            "Laos",
                            "Nepal",
                            "Papua New Guinea",
                            "Philippines",
                            "Tajikistan",
                            "Thailand",
                            "Barbados",
                            "Brazil",
                            "El Salvador",
                            "Guatemala",
                            "Guyana",
                            "Honduras",
                            "Jamaica",
                            "Nicaragua",
                            "Panama",
                            "Trinidad and Tobago"))
# ------------------------------------------------ #
         
setwd("C:/Users/qlx6/Downloads/PreClean_2022_Q1_V1_Datasets/b")         
ar <- read_excel("COT_Asia Region__FY22Q1_2022-03-03__1030AM.xlsx")
mz <- read_excel("COT_Mozambique__FY22Q1_2022-03-03__1030AM.xlsx")

global <- bind_rows(sa_less_1, ar, mz)

openxlsx::write.xlsx(global, 
                     file = paste("C:/Users/qlx6/Downloads/PreClean_2022_Q1_V1_Datasets/WF_Global_Final", 
                                  period, date,"_0200PM.xlsx", sep = "_"), 
                     keepNA = FALSE, asTable = TRUE) 















# =================================================================== #
# Subset PSNUxIM ~ Country Names
# =================================================================== #

library(purrr)


setwd("C:/Users/qlx6/Downloads/MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1") 


ptm <- proc.time()

psnuXim <- read_tsv("MER_Structured_Datasets_PSNU_IM_FY20-22_20220211_v1_1.txt")

ou_list <- split(psnuXim, psnuXim$operatingunit)
ou_list

n <- names(ou_list)

lapply(seq_along(ou_list), 
       function(i,x) {assign(paste0("a",i),x[[i]], envir=.GlobalEnv)},
       x=ou_list)

proc.time() - ptm






