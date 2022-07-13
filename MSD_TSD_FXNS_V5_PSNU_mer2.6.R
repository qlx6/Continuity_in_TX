## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MSD Functions for TXS
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_ML & RTT disaggregation inclusion for MER 2.6 by Femi Akinmade (CDC)
## CREATION DATE: 4/30/2021
## UPDATE: 02/23/2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`%ni%` <- Negate(`%in%`) 


## ==================== MSD IMPORT ====================
msd_import <- function(msd_txt){
  
  df <- read_delim(msd_txt, 
                   "\t", 
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols(.default = col_character(), 
                                    targets = col_double(),
                                    qtr1 = col_double(),
                                    qtr2 = col_double(),
                                    qtr3 = col_double(),
                                    qtr4 = col_double(),
                                    cumulative = col_double()
                   ) 
  ) %>%
    
    filter(indicator %in% c(#"HTS_TST_POS", #a1
      #"HTS_TST", #a2
      "TX_CURR", #b #qtr
      "TX_ML", #b #qtr
      "TX_RTT", #b #qtr
      "TX_NEW",#, #b #qtr
      "TX_PVLS" #b
    ))
}


## ==================== MSD CONVERT LONG ====================
msd_convert_long <- function(msd){
  
  df <- pivot_longer(msd,
                     targets:cumulative,
                     names_to = "period",
                     values_to = "value")
  
  df <- unite(df, 
              "period", 
              c("fiscal_year", "period"),
              sep = "_", 
              remove = T)
}


c ## ==================== TXS RECODE PERIOD & NEW TX INDICATOR LABELS ==================== 
recode_period_txdisagg <- function(msd_converted_long, prev_r, curr_r, curr_t){
  df <- msd_converted_long %>%
    mutate(period = case_when(period == prev_r ~ "prev_result",
                              period == curr_r ~ "curr_result",
                              period == curr_t ~ "curr_target",
                              TRUE ~"remove")) %>%
    mutate(indicator = ifelse(indicator %in% c("TX_RTT", "TX_ML"),
                              paste(indicator, otherdisaggregate, sep="_"),
                              indicator)) %>%
    filter(period != "remove") %>%
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup() %>%
    mutate(period_range = #paste0(prev_r, "-", curr_r) 
             curr_r
    )
}  


## ==================== TXS ADD SINGLE AGE COLUMN ====================
collapse_age <- function(recoded_prioritizations){
  
  df <- recoded_prioritizations %>%
    select(-c(ageasentered, age_2019)) %>%
    rename("trendsfine"="age_2018") %>%
    pivot_longer(cols=c("trendsfine", "trendscoarse"), 
                 names_to = "age_type", 
                 values_to = "age") %>%
    filter(!is.na(value))
}


## ==================== TXS REDO INDICATOR NAMES FOR WIDE PIVOT ====================
redo_indicator_name <- function(reformatted_age_sex){
  df <- reformatted_age_sex %>% 
    mutate(var_suffix = 
             case_when(
               period == "curr_result"  ~ "Now_R",
               period == "curr_target"  ~ "Now_T",
               period == "prev_result"  ~ "Prev_R")
    ) %>% 
    filter(!is.na(var_suffix)) %>%
    unite("varname", 
          c(indicator, var_suffix),
          sep = "_", 
          remove = T) %>%
    group_by_if(is.character) %>%
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TXS REMOVE EXTRANEOUS COLUMNS & CLEAN ====================
txs_clean <- function(redone_indicator_name){
  df <- redone_indicator_name %>%
    filter(standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age Aggregated/Sex/HIVStatus", 
                                           "ARTNoContactReasonIIT/HIVStatus",
                                           "Age/Sex/ARTNoContactReason/HIVStatus"
                                           
                                           )

    ) %>%
    select(operatingunit,
           country,
           snu1,
           snuprioritization,
           psnu,
           psnuuid,
           funding_agency,
           prime_partner_name,
           mech_name,
           mech_code,
           indicatortype,
           #standardizeddisaggregate, # added
           age_type,
           age,
           sex,
           period_range,
           varname,
           value) %>%
    group_by_if(is_character) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TXS CONVERT WIDE ====================
txs_convert_wide <- function(txs_cleaned){
  df <- pivot_wider(txs_cleaned,
                    names_from = "varname",
                    values_from = "value") %>%
    na_if(0)
  
  df$SUMCOL <- rowSums(df[sapply(df, is.numeric)], na.rm = TRUE)
  
  df <- df %>%
    rename(period = period_range) %>%
    filter(SUMCOL != 0) %>%
    select(-SUMCOL)
  
}

## ==================== COMPOSED FUNCTION ====================
msd_df <- function(msd_txt){
  
  df <- msd_import(msd_txt)
  
  df <- msd_convert_long(df)
  
}


txs_generate <- function(msd_df, prevR, currR, currT){
  
  df <- recode_period_txdisagg(msd_df, prevR, currR, currT)
  
  df <- collapse_age(df)
  
  df <- redo_indicator_name(df)
  
  df <- txs_clean(df)
  
  df <- txs_convert_wide(df)
  
}