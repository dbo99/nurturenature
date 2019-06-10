###################### one off slides ##############################
#am_fcast <- ymd_hms(fcasts_firstam)
#fcasts_firstam_date <- date(fcasts_firstam)
## plot inputs
rm(list = ls()) 

#dtrmnstc_test <- dtrmnstc %>% filter(fcast_i_pst == fcasts[5])
setwd("~/R/proj/natvnat/middle")
source("vanduzen_ahps_middleroad_dfcreate.r")


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
    
    #geom_vline(xintercept = firstdtrmnstc, linetype = "dashed", color = "grey40", show.legend = F, size = 0.75) + 
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "gray", show.legend = F, size = 2.000) +   #deeppink
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "gray", show.legend = F, size = 2.000) +    #deeppink4
    
    #geom_hline(yintercept = 1, linetype = "dashed", color = "gray", show.legend = F, size = 1.5) + 
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color =  "Forecast stage\n(deterministic)" ), size  = 1) +
    #geom_label(data = filter(lowflow_levs, unit == "feet"), aes(x = midpoint, y = value, label = "action stage (1 ft)"), show.legend = F, alpha = 0.6) +
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F, alpha = 0.4) +
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
  pday5 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[2], 6) + ggtitle("5 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday5
  ggsave("stage_thres_5.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday4 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[3], 6) + ggtitle("4 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday4
  ggsave("stage_thres_4.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday3 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[4], 6) + ggtitle("3 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday3
  ggsave("stage_thres_3.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday2 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[5], 6) + ggtitle("2 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday2
  ggsave("stage_thres_2.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday1 <- plot_s_5daypeakstage_web(df_hefs, dtrmnstc, fcasts[6], 6) + ggtitle("0-1 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday1
  ggsave("stage_thres_1.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
}


#### double peak split  #####
{
pk1start <- ymd_hm("2019-02-02 00:00", tz="America/Los_Angeles")
pk1end   <- ymd_hm("2019-02-03 00:00", tz="America/Los_Angeles")
#pk2start <- ymd_hm("2019-02-03 13:00", tz="America/Los_Angeles")
#pk2end   <- ymd_hm("2019-02-04 18:00", tz="America/Los_Angeles")

leg <- get_legend(pday4)
pday3_noleg <- pday3 + theme(legend.position = "none") +
           geom_vline(xintercept = pk1start, linetype = "dashed", color = "grey25") +
           geom_vline(xintercept = pk1end, linetype = "dashed", color = "grey25") +
           #geom_vline(xintercept = pk2start, linetype = "dashed", color = "gray") + 
           #geom_vline(xintercept = pk2end, linetype = "dashed", color = "gray") + 
           scale_y_continuous(limits = c(0,7), breaks  =c(0,1,2,3,4,5,6,7)) + labs(y = "feet") +
           annotate(geom="text",x=as.POSIXct("2019-02-02 15:00"), y=1,label="Peak 1",fontface="bold") + 
           #annotate(geom="text",x=as.POSIXct("2019-02-02 15:00"), y=1,label="Peak 1",fontface="bold") + 
           #annotate(geom="text",x=as.POSIXct("2019-02-04 05:00"), y=1,label="Peak 2",fontface="bold") + 
  
  ggtitle("5-Day Max. Peak Stage Probabilities (3 days out)")
            
pday3_noleg

hefspk1 <- df_hefs %>% filter(fcast_t_pst >= pk1start, fcast_t_pst <= pk1end, fcast_i_pst == fcasts[4])
#hefspk2 <- df_hefs %>% filter(fcast_t_pst >= pk2start, fcast_t_pst <= pk2end, fcast_i_pst == fcasts[4])

dtrmnpk1 <- dtrmnstc %>% filter(fcast_t_pst >= pk1start, fcast_t_pst <= pk1end, fcast_i_pst == fcasts[4])
#dtrmnpk2 <- dtrmnstc %>% filter(fcast_t_pst >= pk2start, fcast_t_pst <= pk2end, fcast_i_pst == fcasts[4])

peak1p <- plot_s_5daypeakstage_web(hefspk1, dtrmnpk1, fcasts[4], 6) + theme(legend.position = "none") +ggtitle("Peak 1 Max. Peak Stage Prob.") +
          scale_y_continuous(limits = c(0,7), breaks  =c(0,1,2,3,4,5,6,7)) + labs(y = "feet") +
          geom_vline(xintercept = pk1start, linetype = "dashed", color = "grey25") +
          geom_vline(xintercept = pk1end, linetype = "dashed", color = "grey25")   +
          scale_x_datetime(date_breaks = "6 hours", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M") 
peak1p
#ggsave("peak1.jpg", dpi = 300, width = 7.5, height = 4, units = "in")

#peak2p <- plot_s_5daypeakstage_web(hefspk2, dtrmnpk2, fcasts[4], 6) + theme(legend.position = "none")  +ggtitle("Peak 2 Max Peak Prob.") +
 # scale_y_continuous(limits = c(0,7), breaks  =c(0,1,2,3,4,5,6,7)) + labs(y = "feet") + 
#  geom_vline(xintercept = pk2start, linetype = "dashed", color = "gray") +
#  geom_vline(xintercept = pk2end, linetype = "dashed", color = "gray") 
#peak2p



peaksplitslide <- plot_grid(pday3_noleg,peak1p, nrow = 2, rel_heights = c(1,1))
#peaksplitslide

pfin <- plot_grid(peaksplitslide, leg, rel_widths = c(3,1.25), nrow = 1)
pfin
ggsave("stage_peaksplit.jpg", dpi = 300, width = 7.5, height = 4, units = "in")


pday3_noleg <- pday3 + theme(legend.position = "none") +
  geom_vline(xintercept = pk1start, linetype = "dashed", color = "grey25") +
  geom_vline(xintercept = pk1end, linetype = "dashed", color = "grey25") +
  #geom_vline(xintercept = pk2start, linetype = "dashed", color = "gray") + 
  #geom_vline(xintercept = pk2end, linetype = "dashed", color = "gray") + 
  scale_y_continuous(limits = c(0,7), breaks  =c(0,1,2,3,4,5,6,7)) + labs(y = "feet") +
  #annotate(geom="text",x=as.POSIXct("2019-02-02 15:00"), y=1,label="Peak 1",fontface="bold") + 
  annotate(geom="text",x=as.POSIXct("2019-02-02 11:00"), y=1,label="Peak 1,\nRight\nPlot",fontface="bold") +
  ggtitle("5-Day Max. Peak Stage Prob. (3 days out)") + scale_x_datetime(expand = c(0.015, 0.015),
               date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M") 

peaksplitslide2 <- plot_grid(pday3_noleg,peak1p, nrow = 1, rel_widths = c(1,1))
pfin <- plot_grid(peaksplitslide2, leg, rel_widths = c(3.75,1), nrow = 1)
pfin
ggsave("stage_peaksplit2.jpg", dpi = 300, width = 9.5, height = 5, units = "in")
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
    
    #geom_hline(yintercept = monitorstage_kcfs, linetype = "dashed", color = "gray", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_kcfs, linetype = "dashed", color = "gray", show.legend = F, size = 2.000) + 
    
    #geom_hline(yintercept = 1, linetype = "dashed", color = "gray", show.legend = F, size = 1.5) + 
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = kcfs,color =  "Forecast flow\n(deterministic)" ), size  = 1) +
    #geom_label(data = filter(lowflow_levs, unit == "feet"), aes(x = midpoint, y = value, label = "action stage (1 ft)"), show.legend = F, alpha = 0.6) +
    #geom_label(data = filter(notify_levs, unit == "kcfs"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F, alpha = 0.4) +
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
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[2], 6) + ggtitle("5 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_5.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[3], 6) + ggtitle("4 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_4.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[4], 6) + ggtitle("3 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_3.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[5], 6) + ggtitle("2 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_2.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
  pday5 <- plot_s_5daypeakflow_web(df_hefs, dtrmnstc, fcasts[6], 6) + ggtitle("0-1 Days Out, Morning Forecast\n5-Day Maximum Peak Flow Probabilities")
  pday5
  ggsave("flow_thres_1.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
  
}









































