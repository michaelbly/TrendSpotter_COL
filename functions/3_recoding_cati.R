recoding_cari <- function(r) {
  r <- cati_response
  
  ###############################################################
  # FOOD CONSUMPTION SCORE CSI ADJUSTED
  ###############################################################
  # PUNTAJE FCS Y CSI
  r <- r %>% 
    # FCS
    mutate(fcs = (as.numeric(r$FCSStap)*2) + (as.numeric(r$FCSPulse)*3) + (as.numeric(r$FCSDairy)*4) + (as.numeric(r$FCSPr)*4) +
             as.numeric(r$FCSVeg) + as.numeric(r$FCSFruit) + (as.numeric(r$FCSFat)*0.5) + (as.numeric(r$FCSSugar)*0.5)) %>%
    # PUNTAJE CSI
    mutate(csi_score = (as.numeric(r$rCSILessQlty)*1) + (as.numeric(r$rCSIBorrow)*2) +
             (as.numeric(r$rCSIMealSize)*1) + (as.numeric(r$rCSIMealAdult)*3) +
             (as.numeric(r$rCSIMealNb)*1)) 
  
  
  
  # % de hogares con {poor, borderline, acceptable} Food Consumption Score
  # % de hogares por puntaje CSI
  r <- r %>%
    mutate(sa1_poor = ifelse(r$fcs <= 28,1,0), 
           sa1_borderline = ifelse(r$fcs > 28 & r$fcs <=42,1,0), 
           sa1_acceptable = ifelse(r$fcs > 42,1,0),
           sa1_poor_or_borderline = ifelse(r$fcs <=42,1,0),
           sa2_i = ifelse(r$csi_score <= 4,1,0), 
           sa2_ii = ifelse(r$csi_score > 4 & r$csi_score <=18,1,0), 
           sa2_iii = ifelse(r$csi_score > 18,1,0),
           fcs_category = case_when(
             r$fcs <= 28 ~ "Pobre",
             r$fcs > 28 & r$fcs <= 42 ~ "Limitrofe",
             r$fcs > 42 ~ "Aceptable"))
  

  
  
  # % de hogares por puntaje FCS-CSI ADJUSTED
  r <- r %>% 
    mutate(sa3_i = ifelse(r$sa1_acceptable == 1 & r$sa2_i == 1,1,0), 
           sa3_ii = ifelse(r$sa1_acceptable == 1 & (r$sa2_ii == 1 | r$sa2_iii == 1),1,0), 
           sa3_iii = ifelse(r$sa1_borderline == 1,1,0), 
           sa3_iv = ifelse(r$sa1_poor == 1,1,0))
  
  r <- r %>%
    mutate(fcs_ajuste_cari = case_when(
      r$sa3_i == 1 ~ 1,
      r$sa3_ii == 1 ~ 2,
      r$sa3_iii == 1 ~ 3,
      r$sa3_iv == 1 ~ 4))
  
  
  
  
  ###############################################################
  # ECONOMIC VULNERABILITY
  ###############################################################
  r <- r %>% mutate(hh_trend_categories = case_when(
    r$HHIncFirstChType == "THE INCOME DECREASED" ~ 2,
    r$HHIncFirstChType %in% c("THE INCOME INCREASED") ~ 3,
    r$HHIncFirstCh == "NO" ~ 3, 
    r$HHIncFirstChType == "NO ONE WORKS AT HOME SINCE THE PANDEMIC" ~ 1)) %>%
    
    mutate(hh_inc_categories = case_when(
      r$HHIncFirst %in% c("BUSINESS/SELF-EMPLOYED [FORMAL]", 
                          "SALARIED WORK WITH REGULAR INCOME IN THE PUBLIC OR PRIVATE SECTOR") ~ 1,
      r$HHIncFirst %in% c("CLEANING/CARE WORK AT OTHER PEOPLE'S HOME", "INFORMAL DAILY/CASUAL LABOUR",
                          "INFORMAL TRADE/STREET SALES", 
                          "MIGRANT REMITTANCES OR HELP FROM FAMILY/FRIENDS") ~ 2,
      r$HHIncFirst %in% c("GOVERNMENT ASSISTANCE/SOCIAL PROTECTION PROGRAMMES/PENSIONS",
                          "NO INCOME SOURCE", "SUPPORT FROM UN/NGO/CHARITY") ~ 4))
  
  r <- r %>%
    mutate(ve_cari = case_when(
      r$hh_trend_categories == 1 & r$hh_inc_categories == 1 ~ 1,
      r$hh_trend_categories == 1 & r$hh_inc_categories == 2 ~ 2,
      r$hh_trend_categories == 1 & r$hh_inc_categories == 4 ~ 4,
      
      r$hh_trend_categories == 2 & r$hh_inc_categories == 1 ~ 2,
      r$hh_trend_categories == 2 & r$hh_inc_categories == 2 ~ 3,
      r$hh_trend_categories == 2 & r$hh_inc_categories == 4 ~ 4,
      
      r$hh_trend_categories == 3 & r$hh_inc_categories == 1 ~ 1,
      r$hh_trend_categories == 3 & r$hh_inc_categories == 2 ~ 2,
      r$hh_trend_categories == 3 & r$hh_inc_categories == 4 ~ 4))
  
  
  
  
  ###############################################################
  # LIVELIHOOD COPING STRATEGIES
  ###############################################################
  # % de hogares que recurren a estrategias de stress/crisis/emergency para hacer frente a la falta de alimentos o de dinero para comprarlos
  r <- r %>% 
    mutate(lcs_savings = ifelse(
      r$LhCSISaving %in% c("No. Because I have already carried out this activity during the last 12 months and I cannot continue to do so", 
                           "Yes"), 1,0),
      lcs_assets = ifelse(
        r$LhCSIDomAsset %in% c("No. Because I have already carried out this activity during the last 12 months and I cannot continue to do so", 
                               "Yes"), 1,0),
      lcs_health = ifelse(
        r$LhCSIHealth %in% c("No. Because I have already carried out this activity during the last 12 months and I cannot continue to do so", 
                             "Yes"), 1,0)) %>%
    
    mutate(lcs = lcs_savings + lcs_assets + lcs_health)
  
  
  r <- r %>%         
    mutate(lcs_cari = case_when(
      r$lcs == 0 ~ 1,
      r$lcs == 1 ~ 2,
      r$lcs == 2 ~ 3,
      r$lcs == 3 ~ 4))
  
  
  
  return(r)
}
