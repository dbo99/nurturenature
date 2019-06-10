###################### one off slides ##############################
#am_fcast <- ymd_hms(fcasts_firstam)
#fcasts_firstam_date <- date(fcasts_firstam)
## plot inputs
#am_fcast <- c("2016-01-13 07:37:00 PST") #fcasts[4]
#p1 <- ggplot(data = filter(dtrmnstc, fcast_i_pst == fcasts[5]), aes(x = fcast_t_pst, y  = kcfs)) + geom_line()
#ggplotly(p1)
rm(list = ls()) 
{
  setwd("~/R/proj/natvnat/threshold")
  source("vanduzen_ahps_natvnatjan16_12.19.r")
#am_fcast <- fcasts[1]
#fcast_start_date <- mdy(c("01-15-2016"))
#fcast_end_date <- mdy(c("01-17-2016"))
#fcaststartdate <- fcast_start_date
#fcastenddate <- fcast_end_date
#q_type <- 6
#obs_start_date <- mdy(c("10-23-2014"))
}

plot_s_5daypeakstage_web<- function(df_hefs, dtrmnstc, am_fcast, q_type) {

  dtrmnstc      <- dtrmnstc  %>% filter (fcast_i_pst == am_fcast)
firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
lastdtmrnstc  <- max(dtrmnstc$fcast_t_pst)

df_hefs       <- df_hefs %>% filter (fcast_i_pst == am_fcast)
df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
df_hefs       <- df_hefs %>% filter(fcast_t_pst <= lastdtmrnstc)
midpoint <- as.POSIXct((as.numeric(firstdtrmnstc) + 
                          as.numeric(lastdtmrnstc)) / 2, origin = '1970-01-01')  %>% round_date("hours")

df_qntls_p <- create_permax_quants_hourly(df_hefs, firstdtrmnstc, lastdtmrnstc, "mefp_yr", "feet", q_type) 


p1 <- ggplot() + 
  
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `90%`, ymax = `max`,fill='<10%')) +
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `75%`, ymax = `90%`,fill='10-25%')) + 
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `50%`, ymax = `75%`,fill='25-50%')) +  
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `25%`, ymax = `50%`,fill='50-75%')) +  
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `10%`, ymax = `25%`,fill='75-90%')) +  
  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `min`, ymax = `10%`,fill='>=90%')) +   
  
  geom_vline(xintercept = firstdtrmnstc, linetype = "dashed", color = "grey40", show.legend = F, size = 0.75) + 
  
  geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "gray", show.legend = F, size = 1.5) +   #deeppink
  geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "gray", show.legend = F, size = 1.5) +    #deeppink4
  
 
  geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "Forecast stage\n(deterministic)" ), size  = 1) +
  geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F, alpha = 0.4) +
  scale_x_datetime(expand = c(0.015, 0.015),
                   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
  
  scale_fill_manual(name = "Exceedance Probability\n(ensemble)",
                    
                    values = c(
                      
                      '>=90%' = 'red',
                      '75-90%' = 'yellow',
                      '50-75%' = 'chartreuse4',
                      '25-50%' = 'dodgerblue',
                      '10-25%' = 'blue2', 
                      '<10%' = 'purple'),
                    
                    breaks = c(
                      '<10%' , 
                      '10-25%' ,
                      '25-50%' ,
                      '50-75%' ,
                      '75-90%' ,
                      '>=90%' )) +
  
  scale_color_manual(name = NULL,
                     
                     values= c(
                       "Forecast stage\n(deterministic)" = 'black'),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid"),
                       shape = c( NA)))) + 
  
  scale_y_continuous(name = "feet") +
                     #sec.axis = dup_axis(name = NULL)) +   
  labs( x = NULL) + ggtitle("5-Day Maximum Peak Stage Probabilities")

p1
}


{
pday5 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[1], 6) + ggtitle("5 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
pday5
ggsave("stage_thres_5.jpg", dpi = 300, width = 7.5, height = 4, units = "in")

pday5 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[2], 6) + ggtitle("4 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
pday5
ggsave("stage_thres_4.jpg", dpi = 300, width = 7.5, height = 4, units = "in")

pday5 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[3], 6) + ggtitle("3 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
pday5
ggsave("stage_thres_3.jpg", dpi = 300, width = 7.5, height = 4, units = "in")

pday5 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[4], 6) + ggtitle("2 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
pday5
ggsave("stage_thres_2.jpg", dpi = 300, width = 7.5, height = 4, units = "in")

pday5 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[5], 6) + ggtitle("0-1 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
pday5
ggsave("stage_thres_1.jpg", dpi = 300, width = 7.5, height = 4, units = "in")

}

##################################################################################################
####################################### Flow #####################################################
##################################################################################################

plot_s_5daypeakflow_web<- function(df_hefs, dtrmnstc, am_fcast, q_type) {
  
  dtrmnstc      <- dtrmnstc  %>% filter (fcast_i_pst == am_fcast)
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  lastdtmrnstc  <- max(dtrmnstc$fcast_t_pst)
  
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == am_fcast)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst <= lastdtmrnstc)
  midpoint <- as.POSIXct((as.numeric(firstdtrmnstc) + 
                            as.numeric(lastdtmrnstc)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  df_qntls_p <- create_permax_quants_hourly(df_hefs, firstdtrmnstc, lastdtmrnstc, "mefp_yr", "kcfs", q_type) 
  
  
  p1 <- ggplot() + 
    
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `90%`, ymax = `max`,fill='<10%')) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `75%`, ymax = `90%`,fill='10-25%')) + 
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `50%`, ymax = `75%`,fill='25-50%')) +  
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `25%`, ymax = `50%`,fill='50-75%')) +  
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `10%`, ymax = `25%`,fill='75-90%')) +  
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `min`, ymax = `10%`,fill='>=90%')) +   
    
    geom_vline(xintercept = firstdtrmnstc, linetype = "dashed", color = "grey40", show.legend = F, size = 0.75) + 
    
    geom_hline(yintercept = monitorstage_kcfs, linetype = "dashed", color = "gray", show.legend = F, size = 1.5) + 
    geom_hline(yintercept = floodstage_kcfs, linetype = "dashed", color = "gray", show.legend = F, size = 1.5) + 
    
    
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = kcfs,color =  "Forecast flow\n(deterministic)" ), size  = 1) +
    geom_label(data = filter(notify_levs, unit == "kcfs"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F, alpha = 0.4) +
    scale_x_datetime(expand = c(0.015, 0.015),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    scale_fill_manual(name = "Exceedance Probability\n(ensemble)",
                      
                      values = c(
                        
                        '>=90%' = 'red',
                        '75-90%' = 'yellow',
                        '50-75%' = 'chartreuse4',
                        '25-50%' = 'dodgerblue',
                        '10-25%' = 'blue2', 
                        '<10%' = 'purple'),
                      
                      breaks = c(
                        '<10%' , 
                        '10-25%' ,
                        '25-50%' ,
                        '50-75%' ,
                        '75-90%' ,
                        '>=90%' )) +
    
    scale_color_manual(name = NULL,
                       
                       values= c(
                         "Forecast flow\n(deterministic)" = 'black'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( NA)))) + 
    
    scale_y_continuous(name = "kcfs") + #, sec.axis = dup_axis(name = NULL)) +  
    labs( x = NULL) + ggtitle("5-Day Maximum Peak Flow Probabilities")
  
  p1
}


{
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[1], 6) + ggtitle("5 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_5.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[2], 6) + ggtitle("4 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_4.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[3], 6) + ggtitle("3 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_3.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[4], 6) + ggtitle("2 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_2.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[5], 6) + ggtitle("0-1 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_1.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
}



#########################################
######### Build step by step ############
#########################################



########  just deterministic #########

df_hefs_edu <- df_hefs %>% filter(fcast_i_pst == fcasts[3], fcast_t_pst >= ymd("2016-01-14"), fcast_t_pst <= ymd("2016-01-19"))
dtrmnstc_edu <- dtrmnstc %>% filter(fcast_i_pst == fcasts[3], fcast_t_pst >= ymd("2016-01-14"), fcast_t_pst <= ymd("2016-01-19"))

p_dtrm <- ggplot(dtrmnstc_edu, aes(x = fcast_t_pst, y = feet)) + geom_line()
p_dtrm
ggsave("x.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
p_hefs <- ggplot(df_hefs_edu, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = factor(mefp_yr))) + geom_line() + 
  scale_y_continuous(limits = c(0,25))
p_hefs   
ggsave("y.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
f_hefs_edu_peak_ft <- df_hefs_edu %>% group_by(mefp_yr) %>% mutate(peak = ifelse(feet ==max(feet), feet, NA))

p_hefs_peak <-  ggplot(df_hefs_edu_peak_ft, aes(x = fcast_t_pst, y = peak, group = mefp_yr, color = factor(mefp_yr))) + geom_point() +
                scale_y_continuous(limits = c(0,25))
p_hefs_peak 
ggsave("z.jpg", dpi = 300, width = 7.5, height = 4, units = "in")


df_qntls_p <- df_hefs_edu_peak_ft_sum  %>% summarize(
  #`95%`=quantile(peak, probs=0.05), 
  `90%`=quantile(peak, probs=0.10),
  `75%`=quantile(peak, probs=0.25),
  `50%`=quantile(peak, probs=0.50),
  `25%`=quantile(peak, probs=0.75),
  `10%`=quantile(peak, probs=0.9))
  # `5%`=quantile(peak, probs=0.95))


p_hefs_peak_ribs <- p_hefs_peak +
 
  geom_hline(yintercept = 5.65) + 
  geom_hline(yintercept = 8.76) + 
  geom_hline(yintercept = 11.37) + 
  geom_hline(yintercept = 14.28) + 
  geom_hline(yintercept = 18.41) 

  
  
p_hefs_peak_ribs
ggsave("z2.jpg", dpi = 300, width = 7.5, height = 4, units = "in")




p_dtrm2 <- ggplot(dtrmnstc_edu, aes(x = fcast_t_pst, y = feet)) + geom_line() +
  geom_hline(yintercept = 5.65) + 
  geom_hline(yintercept = 8.76) + 
  geom_hline(yintercept = 11.37) + 
  geom_hline(yintercept = 14.28) + 
  geom_hline(yintercept = 18.41) + scale_y_continuous(limits = c(0,25))
p_dtrm2
ggsave("z3.jpg", dpi = 300, width = 7.5, height = 4, units = "in")



























