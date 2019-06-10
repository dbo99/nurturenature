{
  #rm(list = ls()) 
  
  #setwd("~/R/proj/natvnat/threshold")
  Sys.setenv(TZ="America/Los_Angeles")
  source("libs.r")
  
  source("datafun_defs.r")
  source("flowplot_defs.r")
  source("stageplot_defs.r")
  #source("vanduzenridges_df_create.r")
  #Sys.getenv("TZ")
  #source("fun_defs_redo.r")
  
  
  
  dtrmnstc <- read_csv("data/brgc1_20160113_tidy_dmstc.csv") %>%  #all official am deterministics 15th to 23rd 
    transmute(fcast_i_gmt = forecast_issuance, fcast_t_gmt = forecast_time, kcfs) %>% 
    mutate(fcast_i_gmt = mdy_hm(fcast_i_gmt, tz = 
                                  "Europe/London"), fcast_t_gmt = mdy_hm(fcast_t_gmt, tz = "Europe/London")) %>% 
    mutate(kcfs = ifelse(kcfs == -9999, NA, kcfs), 
           fcast_t_pst = with_tz(fcast_t_gmt, "America/Los_Angeles"),
           fcast_i_pst = with_tz(fcast_i_gmt, "America/Los_Angeles"),
           cfs = 1000*kcfs)
  fcasts <- unique(dtrmnstc$fcast_i_pst)
  fcasts_firstam <- fcasts [c(1,4,7,10,12)] #just first am forecasts for now, simplyfying..
  fcasts <- fcasts_firstam  #redo this eventually..
  dtrmnstc <- dtrmnstc %>% filter(fcast_i_pst %in% fcasts_firstam) %>% mutate(fcast_i_pst_date = date(fcast_i_pst))
  dtrmnstc <- dtrmnstc %>% na.omit()
  rating <- read_csv("data/brgc1_rating_1.6.2016_table410024.csv")
  
  dtrmnstc$feet <- approx(x = rating$flow_cfs, y = rating$stage_ft, xout = dtrmnstc$cfs)$y
  dtrmnstc <- dtrmnstc %>% mutate(mefp_yr = "dtrmnstc")
  dtrmnstc <- dtrmnstc %>% mutate(feet = ifelse(cfs <= min(rating$flow_cfs), min(rating$stage_ft), feet))
  
  usgs_obs <- read_csv("data/vanduz_obs2.csv") %>% mutate(pst  = ymd_hms(pst, tz="America/Los_Angeles")) %>%
    mutate(gmt = with_tz(pst, "Europe/London"), cfs = as.numeric(cfs), kcfs = cfs/1000, feet = as.numeric(stage_ft)) %>% select(-"stage_ft")
  
  usgs_obs_hrly <- usgs_obs %>% filter(minute(pst) ==0) #%>% filter(pst) minute == 0 | filter(pst) == 15 | minute(pst) == 30 | minute(pst) == 45 )
  
  {
    
    
    csv13jan16.12z_hefs <- read_csv("data/2016011312_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pst = fcasts[1], fcast_i_pst_date = date(fcast_i_pst) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
    csv14jan16.12z_hefs <- read_csv("data/2016011412_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pst = fcasts[2],fcast_i_pst_date = date(fcast_i_pst) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
    csv15jan16.12z_hefs <- read_csv("data/2016011512_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pst = fcasts[3],fcast_i_pst_date = date(fcast_i_pst) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
    csv16jan16.12z_hefs <- read_csv("data/2016011612_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pst = fcasts[4], fcast_i_pst_date = date(fcast_i_pst) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
    csv17jan16.12z_hefs <- read_csv("data/2016011712_brgc1.csv") %>% mutate(fcast_t_gmt = mdy_hm(GMT)) %>% select(-"GMT")  %>% mutate(fcast_i_pst = fcasts[5], fcast_i_pst_date = date(fcast_i_pst) ) #%>% filter(fcast_t_gmt <= "2016-01-25 08:00")
    
    
  }
  
  
  csv13jan16.12z_hefs  %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
  csv14jan16.12z_hefs  %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
  csv15jan16.12z_hefs  %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
  csv16jan16.12z_hefs  %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs) 
  csv17jan16.12z_hefs  %<>%  gather(key = mefp_yr, value = "kcfs", 1:59) %>% mutate(mefp_yr = as.integer(substring(mefp_yr,2)),cfs = 1000*kcfs)
  
  
  df_hefs <-  mget(ls(pattern="^csv.*")) %>% bind_rows() # recognizes above new csv# data.frames and combines into one
  df_hefs <- df_hefs %>% mutate(fcast_t_pst = with_tz(fcast_t_gmt, "America/Los_Angeles"), 
                                fcast_i_gmt = with_tz(fcast_i_pst, "Europe/London"))           
  
  df_hefs$feet <- approx(x = rating$flow_cfs, y = rating$stage_ft, xout = df_hefs$cfs)$y
  df_hefs <- df_hefs %>% mutate(feet = ifelse(cfs <= min(rating$flow_cfs), min(rating$stage_ft), feet))
  
  notify_levs <- data.frame(value = c(13.0, 17.0, 21.0, 32.4), unit = c(rep("feet",2), c(rep("kcfs",2))), type = c(rep(c("monitor stage","flood stage"), 2)),
                            color = c(rep(c("orange","red"), 2)), value_t = c("13.0", "17.0", "21.0", "32.4")) 
  
  monitorstage_kcfs <- c(21.0)
  floodstage_kcfs <- c(32.4)
  monitorstage_feet <- c(13.0)
  floodstage_feet <- c(17.0)
  
  
  
  rm(list = ls()[grep("^csv", ls())])  #releases single csvs from memory - deletes everything starting with `csv`!)
  
}


#ggplotly(ggplot(usgs_obs, aes(pst, feet)) + geom_line())

# Jan 12th ##


