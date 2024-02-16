# creat input data
data_EW[,5:238] <- sapply(data_EW[,5:238], function(x) as.numeric(x))
data_EW <- split(data_EW, list(data_EW$Location, data_EW$Place))
for(i in c(1:length(data_EW))) {
  data_EW[[i]]$rcs <- input[[i]][,1]
}
data_EW <- as.data.frame(do.call(rbind, data_EW))


# add preyear columns
era5$Jan_mAT_preyear <- lag(era5$Jan_mAT)
test <- cbind(era5$Jan_mAT_preyear, era5$Jan_mAT)
era5$Feb_mAT_preyear <- lag(era5$Feb_mAT)
era5$Mar_mAT_preyear <- lag(era5$Mar_mAT)
era5$Apr_mAT_preyear <- lag(era5$Apr_mAT)
era5$May_mAT_preyear <- lag(era5$May_mAT)
era5$Jun_mAT_preyear <- lag(era5$Jun_mAT)
era5$Jul_mAT_preyear <- lag(era5$Jul_mAT)
era5$Aug_mAT_preyear <- lag(era5$Aug_mAT)
era5$Sep_mAT_preyear <- lag(era5$Sep_mAT)
era5$Oct_mAT_preyear <- lag(era5$Oct_mAT)
era5$Nov_mAT_preyear <- lag(era5$Nov_mAT)
era5$Dec_mAT_preyear <- lag(era5$Dec_mAT)

era5$Jan_dailyPrec_preyear <- lag(era5$Jan_dailyPrec)
era5$Feb_dailyPrec_preyear <- lag(era5$Feb_dailyPrec)
era5$Mar_dailyPrec_preyear <- lag(era5$Mar_dailyPrec)
era5$Apr_dailyPrec_preyear <- lag(era5$Apr_dailyPrec)
era5$May_dailyPrec_preyear <- lag(era5$May_dailyPrec)
era5$Jun_dailyPrec_preyear <- lag(era5$Jun_dailyPrec)
era5$Jul_dailyPrec_preyear <- lag(era5$Jul_dailyPrec)
era5$Aug_dailyPrec_preyear <- lag(era5$Aug_dailyPrec)
era5$Sep_dailyPrec_preyear <- lag(era5$Sep_dailyPrec)
era5$Oct_dailyPrec_preyear <- lag(era5$Oct_dailyPrec)
era5$Nov_dailyPrec_preyear <- lag(era5$Nov_dailyPrec)
era5$Dec_dailyPrec_preyear <- lag(era5$Dec_dailyPrec)

era5$Jan_Evap_preyear <- lag(era5$Jan_Evap)
era5$Feb_Evap_preyear <- lag(era5$Feb_Evap)
era5$Mar_Evap_preyear <- lag(era5$Mar_Evap)
era5$Apr_Evap_preyear <- lag(era5$Apr_Evap)
era5$May_Evap_preyear <- lag(era5$May_Evap)
era5$Jun_Evap_preyear <- lag(era5$Jun_Evap)
era5$Jul_Evap_preyear <- lag(era5$Jul_Evap)
era5$Aug_Evap_preyear <- lag(era5$Aug_Evap)
era5$Sep_Evap_preyear <- lag(era5$Sep_Evap)
era5$Oct_Evap_preyear <- lag(era5$Oct_Evap)
era5$Nov_Evap_preyear <- lag(era5$Nov_Evap)
era5$Dec_Evap_preyear <- lag(era5$Dec_Evap)

era5$Jan_nSTR_preyear <- lag(era5$Jan_nSTR)
era5$Feb_nSTR_preyear <- lag(era5$Feb_nSTR)
era5$Mar_nSTR_preyear <- lag(era5$Mar_nSTR)
era5$Apr_nSTR_preyear <- lag(era5$Apr_nSTR)
era5$May_nSTR_preyear <- lag(era5$May_nSTR)
era5$Jun_nSTR_preyear <- lag(era5$Jun_nSTR)
era5$Jul_nSTR_preyear <- lag(era5$Jul_nSTR)
era5$Aug_nSTR_preyear <- lag(era5$Aug_nSTR)
era5$Sep_nSTR_preyear <- lag(era5$Sep_nSTR)
era5$Oct_nSTR_preyear <- lag(era5$Oct_nSTR)
era5$Nov_nSTR_preyear <- lag(era5$Nov_nSTR)
era5$Dec_nSTR_preyear <- lag(era5$Dec_nSTR)

# subset
data_era5_1 <- subset(era5, era5$year > 1971 & era5$year < 2018)
data_era5_2 <- subset(era5, era5$year > 1984 & era5$year < 2018)
data_era5_3 <- subset(era5, era5$year > 1975 & era5$year < 2018)
data_era5_4 <- subset(era5, era5$year > 1984 & era5$year < 2018)
data_era5_5 <- subset(era5, era5$year > 1984 & era5$year < 2018)
data_era5_6 <- subset(era5, era5$year > 2002 & era5$year < 2018)
data_era5_7 <- subset(era5, era5$year > 1985 & era5$year < 2018)
data_era5_8 <- subset(era5, era5$year > 1991 & era5$year < 2018)


data_era5 <- as.data.frame(rbind(data_era5_1, data_era5_2, data_era5_3, data_era5_4,
                                 data_era5_5, data_era5_6, data_era5_7, data_era5_8))

data <- data.frame(year = data_EW$Time,
                   region = data_EW$Place,
                   position = data_EW$Location,
                   RingWidth = data_EW$rcs,
                 #  P_annual = data_EW$`Annual precipitation`,
                 #  P_annual_preyear = data_EW$`Annual precipitation_last_year`,
                 # AT_mean_annual = data_EW$`Mean air temperature (year)`,
                 #  AT_mean_annual_preyear = data_EW$`Mean air temperature (year)_last_year`,
                #   AT_max_annual = data_EW$`Maximum air temperature (year)`,
               #    AT_max_annual_preyear = data_EW$`Maximum air temperature (year)_last_year`,
                  # AT_min_annual = data_EW$`Minimum air temperature (year)`,
                #   AT_min_annual_preyear = data_EW$`Minimum air temperature (year)_last_year`,
                   
                   AT_mean_winter = rowMeans(cbind(data_EW$`Mean air temperature (dec)_last_year`, data_EW$`Mean air temperature (jan)_current_year`, data_EW$`Mean air temperature (feb)_current_year`, na.rm = TRUE)),
                   AT_mean_spring = rowMeans(cbind(data_EW$`Mean air temperature (mar)_current_year`, data_EW$`Mean air temperature (apr)_current_year`, data_EW$`Mean air temperature (mai)_current_year`, na.rm = TRUE)),
                   AT_mean_summer = rowMeans(cbind(data_EW$`Mean air temperature (jun)_current_year`, data_EW$`Mean air temperature (jul)_current_year`, data_EW$`Mean air temperature (aug)_current_year`, na.rm = TRUE)),
                   AT_mean_summer_preyear = rowMeans(cbind(data_EW$`Mean air temperature (jun)_last_year`, data_EW$`Mean air temperature (jul)_last_year`, data_EW$`Mean air temperature (aug)_last_year`, na.rm = TRUE)),
                   AT_mean_autumn_preyear = rowMeans(cbind(data_EW$`Mean air temperature (sep)_last_year`, data_EW$`Mean air temperature (oct)_last_year`, data_EW$`Mean air temperature (nov)_last_year`, na.rm = TRUE)),
                   
             #      AT_max_winter = rowMeans(cbind(data_EW$`Maximum air temperature (dec)_last_year`, data_EW$`Maximum air temperature (jan)_current_year`, data_EW$`Maximum air temperature (feb)_current_year`, na.rm = TRUE)),
             #      AT_max_spring = rowMeans(cbind(data_EW$`Maximum air temperature (mar)_current_year`, data_EW$`Maximum air temperature (apr)_current_year`, data_EW$`Maximum air temperature (mai)_current_year`, na.rm = TRUE)),
             #      AT_max_summer = rowMeans(cbind(data_EW$`Maximum air temperature (jun)_current_year`, data_EW$`Maximum air temperature (jul)_current_year`, data_EW$`Maximum air temperature (aug)_current_year`, na.rm = TRUE)),
            #       AT_max_summer_preyear = rowMeans(cbind(data_EW$`Maximum air temperature (jun)_last_year`, data_EW$`Maximum air temperature (jul)_last_year`, data_EW$`Maximum air temperature (aug)_last_year`, na.rm = TRUE)),
           #        AT_max_autumn_preyear = rowMeans(cbind(data_EW$`Maximum air temperature (sep)_last_year`, data_EW$`Maximum air temperature (oct)_last_year`, data_EW$`Maximum air temperature (nov)_last_year`, na.rm = TRUE)),
                   
              #     AT_min_winter = rowMeans(cbind(data_EW$`Minimum air temperature (dec)_last_year`, data_EW$`Minimum air temperature (jan)_current_year`, data_EW$`Minimum air temperature (feb)_current_year`, na.rm = TRUE)),
               #    AT_min_spring = rowMeans(cbind(data_EW$`Minimum air temperature (mar)_current_year`, data_EW$`Minimum air temperature (apr)_current_year`, data_EW$`Minimum air temperature (mai)_current_year`, na.rm = TRUE)),
                #   AT_min_summer = rowMeans(cbind(data_EW$`Minimum air temperature (jun)_current_year`, data_EW$`Minimum air temperature (jul)_current_year`, data_EW$`Minimum air temperature (aug)_current_year`, na.rm = TRUE)),
                #   AT_min_summer_preyear = rowMeans(cbind(data_EW$`Minimum air temperature (jun)_last_year`, data_EW$`Minimum air temperature (jul)_last_year`, data_EW$`Minimum air temperature (aug)_last_year`, na.rm = TRUE)),
                #   AT_min_autumn_preyear = rowMeans(cbind(data_EW$`Minimum air temperature (sep)_last_year`, data_EW$`Minimum air temperature (oct)_last_year`, data_EW$`Minimum air temperature (nov)_last_year`, na.rm = TRUE)),
                   
                   P_winter = rowMeans(cbind(data_EW$`Precipitation (dec)_last_year`, data_EW$`Precipitation (jan)_current_year`, data_EW$`Precipitation (feb)_current_year`, na.rm = TRUE)),
                   P_spring = rowMeans(cbind(data_EW$`Precipitation (mar)_current_year`, data_EW$`Precipitation (apr)_current_year`, data_EW$`Precipitation (mai)_current_year`, na.rm = TRUE)),
                   P_summer = rowMeans(cbind(data_EW$`Precipitation (jun)_current_year`, data_EW$`Precipitation (jul)_current_year`, data_EW$`Precipitation (aug)_current_year`, na.rm = TRUE)),
                   P_summer_preyear = rowMeans(cbind(data_EW$`Precipitation (jun)_last_year`, data_EW$`Precipitation (jul)_last_year`, data_EW$`Precipitation (aug)_last_year`, na.rm = TRUE)),
                   P_autumn_preyear = rowMeans(cbind(data_EW$`Precipitation (sep)_last_year`, data_EW$`Precipitation (oct)_last_year`, data_EW$`Precipitation (nov)_last_year`, na.rm = TRUE)),
                  
                   AO_annual = data_EW$AO_AV_current_year,
                   AMO_annual = data_EW$AMO_KaplanSST_AV_current_year,
                   NAO_Annual = data_EW$NAO_AV_current_year,
                   
             #      AO_annual_preyear = data_EW$AO_AV_last_year,
            #       AMO_annual_preyear = data_EW$AMO_KaplanSST_AV_last_year,
            #       NAO_Annual_preyear = data_EW$NAO_AV_last_year,
                   
                   AO_winter = rowMeans(cbind(data_EW$AO_Dec_last_year, data_EW$AO_Jan_current_year, data_EW$AO_Feb_current_year, na.rm = TRUE)),
                   AO_spring = rowMeans(cbind(data_EW$AO_Mar_current_year, data_EW$AO_Apr_current_year, data_EW$AO_May_current_year, na.rm = TRUE)),
                   AO_summer = rowMeans(cbind(data_EW$AO_Jun_current_year, data_EW$A0_Jul_current_year...115, data_EW$AO_Aug_current_year, na.rm = TRUE)),
                   AO_summer_preyear = rowMeans(cbind(data_EW$AO_Jun_last_year, data_EW$A1_Jul_last_year...180, data_EW$AO_Aug_last_year, na.rm = TRUE)),
                   AO_autumn_preyear = rowMeans(cbind(data_EW$AO_Sep_last_year, data_EW$AO_Oct_last_year, data_EW$AO_Nov_last_year, na.rm = TRUE)),
                   
                   AMO_winter = rowMeans(cbind(data_EW$AMO_KaplanSST_Dec_last_year, data_EW$AMO_KaplanSST_Jan_current_year, data_EW$AMO_KaplanSST_Feb_current_year, na.rm = TRUE)),
                   AMO_spring = rowMeans(cbind(data_EW$AMO_KaplanSST_Mar_current_year, data_EW$AMO_KaplanSST_Apr_current_year, data_EW$AMO_KaplanSST_May_current_year, na.rm = TRUE)),
                   AMO_summer = rowMeans(cbind(data_EW$AMO_KaplanSST_Jun_current_year, data_EW$AMO_KaplanSST_Jul_current_year, data_EW$AMO_KaplanSST_Aug_current_year, na.rm = TRUE)),
                   AMO_summer_preyear = rowMeans(cbind(data_EW$AMO_KaplanSST_Jun_last_year, data_EW$AMO_KaplanSST_Jul_last_year, data_EW$AMO_KaplanSST_Aug_last_year, na.rm = TRUE)),
                   AMO_autumn_preyear = rowMeans(cbind(data_EW$AMO_KaplanSST_Sep_last_year, data_EW$AMO_KaplanSST_Oct_last_year, data_EW$AMO_KaplanSST_Nov_last_year, na.rm = TRUE)),
                   
                   NAO_winter = rowMeans(cbind(data_EW$NAO_Dec_last_year, data_EW$NAO_Jan_current_year, data_EW$NAO_Feb_current_year, na.rm = TRUE)),
                   NAO_spring = rowMeans(cbind(data_EW$NAO_Mar_current_year, data_EW$NAO_Apr_current_year, data_EW$NAO_May_current_year, na.rm = TRUE)),
                   NAO_summer = rowMeans(cbind(data_EW$NAO_Jun_current_year, data_EW$A0_Jul_current_year...63, data_EW$NAO_Aug_current_year, na.rm = TRUE)),
                   NAO_summer_preyear = rowMeans(cbind(data_EW$NAO_Jun_last_year, data_EW$A1_Jul_last_year...232, data_EW$NAO_Aug_last_year, na.rm = TRUE)),
                   NAO_autumn_preyear = rowMeans(cbind(data_EW$NAO_Sep_current_year, data_EW$NAO_Oct_current_year, data_EW$NAO_Nov_current_year, na.rm = TRUE)),  
                   
                   AT_mean_era5_winter = rowMeans(cbind(data_era5$Jan_mAT, data_era5$Feb_mAT, na.rm = TRUE)),
                   AT_mean_era5_spring = rowMeans(cbind(data_era5$Mar_mAT, data_era5$Apr_mAT, data_era5$May_mAT , na.rm = TRUE)),
                   AT_mean_era5_summer = rowMeans(cbind(data_era5$Jun_mAT, data_era5$Jul_mAT, data_era5$Aug_mAT, na.rm = TRUE)),
                   
              #     AT_max_era5_winter = rowMeans(cbind(data_era5$Jan_maxAT, data_era5$Feb_maxAT, na.rm = TRUE)),
             #      AT_max_era5_spring = rowMeans(cbind(data_era5$Mar_maxAT, data_era5$Apr_maxAT, data_era5$May_maxAT , na.rm = TRUE)),
            #       AT_max_era5_summer = rowMeans(cbind(data_era5$Jun_maxAT, data_era5$Jul_maxAT, data_era5$Aug_maxAT, na.rm = TRUE)),
                   
                   P_era5_winter = rowMeans(cbind(data_era5$Jan_dailyPrec, data_era5$Feb_dailyPrec, na.rm = TRUE)),
                   P_era5_spring = rowMeans(cbind(data_era5$Mar_dailyPrec, data_era5$Apr_dailyPrec, data_era5$May_dailyPrec , na.rm = TRUE)),
                   P_era5_summer = rowMeans(cbind(data_era5$Jun_dailyPrec, data_era5$Jul_dailyPrec, data_era5$Aug_dailyPrec, na.rm = TRUE)),
                   
                   Evap_era5_winter = rowMeans(cbind(data_era5$Jan_Evap, data_era5$Feb_Evap, na.rm = TRUE)),
                   Evap_era5_spring = rowMeans(cbind(data_era5$Mar_Evap, data_era5$Apr_Evap, data_era5$May_Evap, na.rm = TRUE)),
                   Evap_era5_summer = rowMeans(cbind(data_era5$Jun_Evap, data_era5$Jul_Evap, data_era5$Aug_Evap, na.rm = TRUE)),
                   
                   nSTR_era5_winter = rowMeans(cbind(data_era5$Jan_nSTR, data_era5$Feb_nSTR, na.rm = TRUE)),
                   nSTR_era5_spring = rowMeans(cbind(data_era5$Mar_nSTR, data_era5$Apr_nSTR, data_era5$May_nSTR, na.rm = TRUE)),
                   nSTR_era5_summer = rowMeans(cbind(data_era5$Jun_nSTR, data_era5$Jul_nSTR, data_era5$Aug_nSTR, na.rm = TRUE)),
                   
                   SWE_era5_winter = rowMeans(cbind(data_era5$Jan_SWE, data_era5$Feb_SWE, na.rm = TRUE)),
                   SWE_era5_spring = rowMeans(cbind(data_era5$Mar_SWE, data_era5$Apr_SWE, data_era5$May_SWE, na.rm = TRUE)),
                   SWE_era5_summer = rowMeans(cbind(data_era5$Jun_SWE, data_era5$Jul_SWE, data_era5$Aug_SWE, na.rm = TRUE))
                   )


data_era5_seasons <- data.frame(year = data_EW$Time,
                   region = data_EW$Place,
                   position = data_EW$Location,
                   RingWidth = data_EW$rcs,
                
                   AT_mean_era5_winter = rowMeans(cbind(data_era5$Jan_mAT, data_era5$Feb_mAT, na.rm = TRUE)),
                   AT_mean_era5_spring = rowMeans(cbind(data_era5$Apr_mAT, data_era5$May_mAT , na.rm = TRUE)),
                   AT_mean_era5_summer = rowMeans(cbind(data_era5$Jun_mAT, data_era5$Jul_mAT, data_era5$Aug_mAT, data_era5$Sep_mAT, na.rm = TRUE)),
                 
                   P_era5_winter = rowMeans(cbind(data_era5$Jan_dailyPrec, data_era5$Feb_dailyPrec, na.rm = TRUE)),
                   P_era5_spring = rowMeans(cbind(data_era5$Apr_dailyPrec, data_era5$May_dailyPrec , na.rm = TRUE)),
                   P_era5_summer = rowMeans(cbind(data_era5$Jun_dailyPrec, data_era5$Jul_dailyPrec, data_era5$Aug_dailyPrec, data_era5$Sep_dailyPrec, na.rm = TRUE)),
                   
                   Evap_era5_winter = rowMeans(cbind(data_era5$Jan_Evap, data_era5$Feb_Evap, na.rm = TRUE)),
                   Evap_era5_spring = rowMeans(cbind(data_era5$Apr_Evap, data_era5$May_Evap, na.rm = TRUE)),
                   Evap_era5_summer = rowMeans(cbind(data_era5$Jun_Evap, data_era5$Jul_Evap, data_era5$Aug_Evap ,data_era5$Sep_Evap, na.rm = TRUE)),
                   
                   nSTR_era5_winter = rowMeans(cbind(data_era5$Jan_nSTR, data_era5$Feb_nSTR, na.rm = TRUE)),
                   nSTR_era5_spring = rowMeans(cbind(data_era5$Apr_nSTR, data_era5$May_nSTR, na.rm = TRUE)),
                   nSTR_era5_summer = rowMeans(cbind(data_era5$Jun_nSTR, data_era5$Jul_nSTR, data_era5$Aug_nSTR, data_era5$Sep_nSTR, na.rm = TRUE))
)

data_era5_months <- data.frame(year = data_EW$Time,
                        region = data_EW$Place,
                        position = data_EW$Location,
                        RingWidth = data_EW$rcs,
                        
                        AT_JAN = data_era5$Jan_mAT,
                        AT_FEB = data_era5$Feb_mAT,
                        AT_MAR = data_era5$Mar_mAT,
                        AT_APR = data_era5$Apr_mAT,
                        AT_MAY = data_era5$May_mAT,
                        AT_JUN = data_era5$Jun_mAT,
                        AT_JUL = data_era5$Jul_mAT,
                        AT_AUG = data_era5$Aug_mAT,
                        AT_SEP = data_era5$Sep_mAT,
                        
                        AT_Jun = data_era5$Jun_mAT_preyear,
                        AT_Jul = data_era5$Jul_mAT_preyear,
                        AT_Aug = data_era5$Aug_mAT_preyear,
                        AT_Sep = data_era5$Sep_mAT_preyear,
                        AT_Oct = data_era5$Oct_mAT_preyear,
                        AT_Nov = data_era5$Nov_mAT_preyear,
                        AT_Dec = data_era5$Dec_mAT_preyear,
                        
                        P_JAN = data_era5$Jan_dailyPrec,
                        P_FEB = data_era5$Feb_dailyPrec,
                        P_MAR = data_era5$Mar_dailyPrec,
                        P_APR = data_era5$Apr_dailyPrec,
                        P_MAY = data_era5$May_dailyPrec,
                        P_JUN = data_era5$Jun_dailyPrec,
                        P_JUL = data_era5$Jul_dailyPrec,
                        P_AUG = data_era5$Aug_dailyPrec,
                        P_SEP = data_era5$Sep_dailyPrec,
                        
                        P_Jun = data_era5$Jun_dailyPrec_preyear,
                        P_Jul = data_era5$Jul_dailyPrec_preyear,
                        P_Aug = data_era5$Aug_dailyPrec_preyear,
                        P_Sep = data_era5$Sep_dailyPrec_preyear,
                        P_Oct = data_era5$Oct_dailyPrec_preyear,
                        P_Nov = data_era5$Nov_dailyPrec_preyear,
                        P_Dec = data_era5$Dec_dailyPrec_preyear,
                        
                        Evap_JAN = data_era5$Jan_Evap,
                        Evap_FEB = data_era5$Feb_Evap,
                        Evap_MAR = data_era5$Mar_Evap,
                        Evap_APR = data_era5$Apr_Evap,
                        Evap_MAY = data_era5$May_Evap,
                        Evap_JUN = data_era5$Jun_Evap,
                        Evap_JUL = data_era5$Jul_Evap,
                        Evap_AUG = data_era5$Aug_Evap,
                        Evap_SEP = data_era5$Sep_Evap,
                        
                        Evap_Jun = data_era5$Jun_Evap_preyear,
                        Evap_Jul = data_era5$Jul_Evap_preyear,
                        Evap_Aug = data_era5$Aug_Evap_preyear,
                        Evap_Sep = data_era5$Sep_Evap_preyear,
                        Evap_Oct = data_era5$Oct_Evap_preyear,
                        Evap_Nov = data_era5$Nov_Evap_preyear,
                        Evap_Dec = data_era5$Dec_Evap_preyear
                        )

data_oscelations <- data.frame(year = data_EW$Time,
                           region = data_EW$Place,
                           position = data_EW$Location,
                           RingWidth = data_EW$rcs,
                          
                           AO_winter = rowMeans(cbind(data_EW$AO_Oct_last_year, data_EW$AO_Nov_last_year, data_EW$AO_Dec_last_year, data_EW$AO_Jan_last_year, data_EW$AO_Feb_last_year, na.rm = TRUE)),
                           AO_growing = rowMeans(cbind(data_EW$AO_Jun_current_year, data_EW$A0_Jul_current_year...63, data_EW$AO_Aug_current_year, data_EW$AO_Sep_current_year, na.rm = TRUE)),
                           AO_spring = rowMeans(cbind(data_EW$AO_Apr_current_year, data_EW$AO_May_current_year, na.rm = TRUE)),
                           AO_growing_preyear = rowMeans(cbind(data_EW$AO_Jun_last_year, data_EW$A1_Jul_last_year...180, data_EW$AO_Aug_last_year, data_EW$AO_Sep_last_year, na.rm = TRUE)),
                           
                           AMO_winter = rowMeans(cbind(data_EW$AMO_KaplanSST_Oct_last_year, data_EW$AMO_KaplanSST_Nov_last_year, data_EW$AMO_KaplanSST_Dec_last_year, data_EW$AMO_KaplanSST_Jan_last_year, data_EW$AMO_KaplanSST_Feb_last_year, na.rm = TRUE)),
                           AMO_growing = rowMeans(cbind(data_EW$AMO_KaplanSST_Jun_current_year, data_EW$AMO_KaplanSST_Jul_current_year, data_EW$AMO_KaplanSST_Aug_current_year, data_EW$AMO_KaplanSST_Sep_current_year, na.rm = TRUE)),
                           AMO_spring = rowMeans(cbind(data_EW$AMO_KaplanSST_Apr_current_year, data_EW$AMO_KaplanSST_May_current_year, na.rm = TRUE)),
                           AMO_growing_preyear = rowMeans(cbind(data_EW$AMO_KaplanSST_Jun_last_year, data_EW$AMO_KaplanSST_Jul_last_year, data_EW$AMO_KaplanSST_Aug_last_year, data_EW$AMO_KaplanSST_Sep_last_year, na.rm = TRUE)),
                           
                           NAO_winter = rowMeans(cbind(data_EW$NAO_Oct_last_year, data_EW$NAO_Nov_last_year, data_EW$NAO_Dec_last_year, data_EW$NAO_Jan_last_year, data_EW$NAO_Feb_last_year, na.rm = TRUE)),
                           NAO_growing = rowMeans(cbind(data_EW$NAO_Jun_current_year, data_EW$A0_Jul_current_year...115, data_EW$NAO_Aug_current_year, data_EW$NAO_Sep_current_year, na.rm = TRUE)),
                           NAO_spring = rowMeans(cbind(data_EW$NAO_Apr_current_year, data_EW$NAO_May_current_year, na.rm = TRUE)),
                           NAO_growing_preyear = rowMeans(cbind(data_EW$NAO_Jun_last_year, data_EW$A1_Jul_last_year...232, data_EW$NAO_Aug_last_year, data_EW$NAO_Sep_last_year, na.rm = TRUE))
                          )


data_oscelations2 <- data.frame(year = data_EW$Time,
                               region = data_EW$Place,
                               position = data_EW$Location,
                               RingWidth = data_EW$rcs,
                               
                               AO_Jun = data_EW$AO_Jun_last_year,
                               AO_Jul = data_EW$A1_Jul_last_year...180,
                               AO_Aug = data_EW$AO_Aug_last_year,
                               AO_Sep = data_EW$AO_Sep_last_year,
                               AO_Oct = data_EW$AO_Oct_last_year,
                               AO_Nov = data_EW$AO_Nov_last_year,
                               AO_Dec = data_EW$AO_Dec_last_year,
                               AO_JAN = data_EW$AO_Jan_current_year,
                               AO_FEB = data_EW$AO_Feb_current_year,
                               AO_MAR = data_EW$AO_Mar_current_year,
                               AO_APR = data_EW$AO_Apr_current_year,
                               AO_MAY = data_EW$AO_May_current_year,
                               AO_JUN = data_EW$AO_Jun_current_year,
                               AO_JUL = data_EW$A0_Jul_current_year...63,
                               AO_AUG = data_EW$AO_Aug_current_year,
                               AO_SEP = data_EW$AO_Sep_current_year,
                              
                               AMO_Jun = data_EW$AMO_KaplanSST_Jul_last_year,
                               AMO_Jul = data_EW$AMO_KaplanSST_Jul_last_year,
                               AMO_Aug = data_EW$AMO_KaplanSST_Aug_last_year,
                               AMO_Sep = data_EW$AMO_KaplanSST_Sep_last_year,
                               AMO_Oct = data_EW$AMO_KaplanSST_Oct_last_year,
                               AMO_Nov = data_EW$AMO_KaplanSST_Nov_last_year,
                               AMO_Dec = data_EW$AMO_KaplanSST_Dec_last_year,
                               AMO_JAN = data_EW$AMO_KaplanSST_Jan_current_year,
                               AMO_FEB = data_EW$AMO_KaplanSST_Feb_current_year,
                               AMO_MAR = data_EW$AMO_KaplanSST_Mar_current_year,
                               AMO_APR = data_EW$AMO_KaplanSST_Apr_current_year,
                               AMO_MAY = data_EW$AMO_KaplanSST_May_current_year,
                               AMO_JUN = data_EW$AMO_KaplanSST_Jun_current_year,
                               AMO_JUL = data_EW$AMO_KaplanSST_Jul_current_year,
                               AMO_AUG = data_EW$AMO_KaplanSST_Aug_current_year,
                               AMO_SEP = data_EW$AMO_KaplanSST_Sep_current_year,
                               
                               NAO_Jun = data_EW$NAO_Jun_last_year,
                               NAO_Jul = data_EW$A1_Jul_last_year...232,
                               NAO_Aug = data_EW$NAO_Aug_last_year,
                               NAO_Sep = data_EW$NAO_Sep_last_year,
                               NAO_Oct = data_EW$NAO_Oct_last_year,
                               NAO_Nov = data_EW$NAO_Nov_last_year,
                               NAO_Dec = data_EW$NAO_Dec_last_year,
                               NAO_JAN = data_EW$NAO_Jan_current_year,
                               NAO_FEB = data_EW$NAO_Feb_current_year,
                               NAO_MAR = data_EW$NAO_Mar_current_year,
                               NAO_APR = data_EW$NAO_Apr_current_year,
                               NAO_MAY = data_EW$NAO_May_current_year,
                               NAO_JUN = data_EW$NAO_Jun_current_year,
                               NAO_JUL = data_EW$A0_Jul_current_year...115,
                               NAO_AUG = data_EW$NAO_Aug_current_year,
                               NAO_SEP = data_EW$NAO_Sep_current_year
                               
)

data_regional <- data.frame(year = data_EW$Time,
                           region = data_EW$Place,
                           position = data_EW$Location,
                           RingWidth = data_EW$rcs,
                            
                           AT_JAN = data_EW$`Mean air temperature (jan)_current_year`,
                           AT_FEB = data_EW$`Mean air temperature (feb)_current_year`,
                           AT_MAR = data_EW$`Mean air temperature (mar)_current_year`,
                           AT_APR = data_EW$`Mean air temperature (apr)_current_year`,
                           AT_MAY = data_EW$`Mean air temperature (mai)_current_year`,
                           AT_JUN = data_EW$`Mean air temperature (jun)_current_year`,
                           AT_JUL = data_EW$`Mean air temperature (jul)_current_year`,
                           AT_AUG = data_EW$`Mean air temperature (aug)_current_year`,
                           AT_SEP = data_EW$`Mean air temperature (sep)_current_year`,
                           
                           AT_Jun = data_EW$`Mean air temperature (jun)_last_year`,
                           AT_Jul = data_EW$`Mean air temperature (jul)_last_year`,
                           AT_Aug = data_EW$`Mean air temperature (aug)_last_year`,
                           AT_Sep = data_EW$`Mean air temperature (sep)_last_year`,
                           AT_Oct = data_EW$`Mean air temperature (oct)_last_year`,
                           AT_Nov = data_EW$`Mean air temperature (nov)_last_year`,
                           AT_Dec = data_EW$`Mean air temperature (dec)_last_year`,
                           
                           P_JAN = data_EW$`Precipitation (jan)_current_year`,
                           P_FEB = data_EW$`Precipitation (feb)_current_year`,
                           P_MAR = data_EW$`Precipitation (mar)_current_year`,
                           P_APR = data_EW$`Precipitation (apr)_current_year`,
                           P_MAY = data_EW$`Precipitation (mai)_current_year`,
                           P_JUN = data_EW$`Precipitation (jun)_current_year`,
                           P_JUL = data_EW$`Precipitation (jul)_current_year`,
                           P_AUG = data_EW$`Precipitation (aug)_current_year`,
                           P_SEP = data_EW$`Precipitation (sep)_current_year`,
                           
                           P_Jun = data_EW$`Precipitation (jun)_last_year`,
                           P_Jul = data_EW$`Precipitation (jul)_last_year`,
                           P_Aug = data_EW$`Precipitation (aug)_last_year`,
                           P_Sep = data_EW$`Precipitation (sep)_last_year`,
                           P_Oct = data_EW$`Precipitation (oct)_last_year`,
                           P_Nov = data_EW$`Precipitation (nov)_last_year`,
                           P_Dec = data_EW$`Precipitation (dec)_last_year`
                )
data_at <- data.frame(year = data_EW$Time,
                            region = data_EW$Place,
                            position = data_EW$Location,
                            RingWidth = data_EW$rcs,
                            
                            AT_Jan = data_EW$`Mean air temperature (jan)_current_year`,
                            AT_Feb = data_EW$`Mean air temperature (feb)_current_year`,
                            AT_Mar = data_EW$`Mean air temperature (mar)_current_year`,
                            AT_Apr = data_EW$`Mean air temperature (apr)_current_year`,
                            AT_May = data_EW$`Mean air temperature (mai)_current_year`,
                            AT_Jun = data_EW$`Mean air temperature (jun)_current_year`,
                            AT_Jul = data_EW$`Mean air temperature (jul)_current_year`,
                            AT_Aug = data_EW$`Mean air temperature (aug)_current_year`,
                            AT_Sep = data_EW$`Mean air temperature (sep)_current_year`,
                            
                            AT_Jun_preyear = data_EW$`Mean air temperature (jun)_last_year`,
                            AT_Jul_preyear = data_EW$`Mean air temperature (jul)_last_year`,
                            AT_Aug_preyear = data_EW$`Mean air temperature (aug)_last_year`,
                            AT_Sep_preyear = data_EW$`Mean air temperature (sep)_last_year`,
                            AT_Oct_preyear = data_EW$`Mean air temperature (oct)_last_year`,
                            AT_Nov_preyear = data_EW$`Mean air temperature (nov)_last_year`,
                            AT_Dec_preyear = data_EW$`Mean air temperature (dec)_last_year`
                            
)

data_p <- data.frame(year = data_EW$Time,
                            region = data_EW$Place,
                            position = data_EW$Location,
                            RingWidth = data_EW$rcs,
                            
                            P_Jan = data_EW$`Precipitation (jan)_current_year`,
                            P_Feb = data_EW$`Precipitation (feb)_current_year`,
                            P_Mar = data_EW$`Precipitation (mar)_current_year`,
                            P_Apr = data_EW$`Precipitation (apr)_current_year`,
                            P_May = data_EW$`Precipitation (mai)_current_year`,
                            P_Jun = data_EW$`Precipitation (jun)_current_year`,
                            P_Jul = data_EW$`Precipitation (jul)_current_year`,
                            P_Aug = data_EW$`Precipitation (aug)_current_year`,
                            P_Sep = data_EW$`Precipitation (sep)_current_year`,
                            
                            P_Jun_preyear = data_EW$`Precipitation (jun)_last_year`,
                            P_Jul_preyear = data_EW$`Precipitation (jul)_last_year`,
                            P_Aug_preyear = data_EW$`Precipitation (aug)_last_year`,
                            P_Sep_preyear = data_EW$`Precipitation (sep)_last_year`,
                            P_Oct_preyear = data_EW$`Precipitation (oct)_last_year`,
                            P_Nov_preyear = data_EW$`Precipitation (nov)_last_year`,
                            P_Dec_preyear = data_EW$`Precipitation (dec)_last_year`
)

# input data
# data: all data
# data_reduced: data by growing season, winter, spring
# data_regional: regional data only, by month
# data_at: monthly air temperature
# data_p: monthly p
# data_era5: ERA5 data
# data_oscelations

data_all <- list(data,
                 data_regional,
                 data_oscelations,
                 data_era5,
                 data_at,
                 data_p)
