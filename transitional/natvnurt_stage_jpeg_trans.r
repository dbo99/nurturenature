###################### one off slides ##############################
#am_fcast <- ymd_hms(fcasts_firstam)
#fcasts_firstam_date <- date(fcasts_firstam)
## plot inputs


#dtrmnstc_test <- dtrmnstc %>% filter(fcast_i_pst == fcasts[5])
setwd("~/R/proj/natvnat/transitional")
source("vanduzen_ahps_natvnatoct14_withplotly.r")
#p1 <- ggplot(data = filter(dtrmnstc, fcast_i_pst == fcasts[5]), aes(x = fcast_t_pst, y  = kcfs)) + geom_line()
#ggplotly(p1)

#{
#
#am_fcast <- fcasts[1]
#fcast_start_date <- mdy(c("01-15-2016"))
#fcast_end_date <- mdy(c("01-17-2016"))
#fcaststartdate <- fcast_start_date
#fcastenddate <- fcast_end_date
#q_type <- 6
##obs_start_date <- mdy(c("10-23-2014"))
#}

#am_fcast <- fcasts[5]

plot_s_5daypeakstage_web_athold <- function(df_hefs, dtrmnstc, am_fcast, q_type) {
  
  dtrmnstc      <- dtrmnstc  %>% filter (fcast_i_pst == am_fcast)
  dtrmsntc_csv <- dtrmnstc %>% write.csv("dtrmsntc_csv.csv")
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
    geom_hline(yintercept = 1, linetype = "dashed", color = "deeppink", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "deeppink4", show.legend = F, size = 2.000) + 
    
    
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "Forecast stage" ), size  = 2) +
    geom_label(data = filter(lowflow_levs, unit == "feet"), aes(x = midpoint, y = value, label = "action stage (1 ft)"), show.legend = F, alpha = 0.6) +
    scale_x_datetime(expand = c(0.015, 0.015),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    scale_fill_manual(name = "Exceedance Probability",
                      
                      values = c(
                        
                        '>=90%' = 'red',
                        '75-90%' = 'yellow',
                        '50-75%' = 'chartreuse4',
                        '25-50%' = 'dodgerblue',
                        '10-25%' = 'blue4', 
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
                         'Forecast stage' = 'black'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( NA)))) + 
    
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL)) +   labs( x = NULL) + ggtitle("5-Day Maximum Peak Stage Probabilities")
  
  p1
}
{

  pday5 <- plot_s_5daypeakstage_web_athold(df_hefs, dtrmnstc, fcasts[5], 6) + ggtitle("5 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday5
  ggsave("5daysout.jpg", dpi = 300, width = 7.5, height = 5, units = "in")
  
  pday5 <- plot_s_5daypeakstage_web_athold(df_hefs, dtrmnstc, fcasts[6], 6) + ggtitle("4 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday5
  ggsave("4daysout.jpg", dpi = 300, width = 7.5, height = 5, units = "in")
  
  pday5 <- plot_s_5daypeakstage_web_athold(df_hefs, dtrmnstc, fcasts[7], 6) + ggtitle("3 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday5
  ggsave("3daysout.jpg", dpi = 300, width = 7.5, height = 5, units = "in")
  
  pday5 <- plot_s_5daypeakstage_web_athold(df_hefs, dtrmnstc, fcasts[8], 6) + ggtitle("2 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday5
  ggsave("2daysout.jpg", dpi = 300, width = 7.5, height = 5, units = "in")
  
  pday5 <- plot_s_5daypeakstage_web_athold(df_hefs, dtrmnstc, fcasts[9], 6) + ggtitle("0-1 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
  pday5
  ggsave("1daysout.jpg", dpi = 300, width = 7.5, height = 5, units = "in")


}

  dtrmnstc      <- dtrmnstc  %>% filter (fcast_i_pst == am_fcast)
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  lastdtmrnstc  <- max(dtrmnstc$fcast_t_pst)
  
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == am_fcast)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst <= lastdtmrnstc)

  
  df_qntls_p <- create_permax_quants_hourly(df_hefs, firstdtrmnstc, lastdtmrnstc, "mefp_yr", "feet", q_type) 

  
  p1 <- ggplot() + 
   

    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `75%`, ymax = `90%`,fill='10-25%')) + 
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `50%`, ymax = `75%`,fill='25-50%')) +  
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `25%`, ymax = `50%`,fill='50-75%')) +  
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `10%`, ymax = `25%`,fill='75-90%')) +  
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(firstdtrmnstc), xmax= as.POSIXct(lastdtmrnstc), ymin= `min`, ymax = `10%`,fill='>=90%')) +   
   
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "Forecast stage" ), size  = 2) +
     
    scale_x_datetime(expand = c(0.015, 0.015),
    date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    scale_fill_manual(name = "Exceedance Probability",
                      
                      values = c(

                        '>=90%' = 'red',
                        '75-90%' = 'yellow',
                        '50-75%' = 'chartreuse4',
                        '25-50%' = 'dodgerblue',
                        '10-25%' = 'blue4'), 

                     breaks = c(
                       '>=90%'  ,
                       '75-90%' ,
                       '50-75%' ,
                       '25-50%' ,
                       '10-25%' )) +
    
      scale_color_manual(name = NULL,
                         
                         values= c(
                           'Forecast stage' = 'black'),
                         guide = guide_legend(override.aes = list(
                           linetype = c("solid"),
                           shape = c( NA)))) + 

      scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL)) +   labs( x = NULL) + ggtitle("5-Day Maximum Peak Stage Probabilities")
  
p1

ggsave("day5threshold.jpg", dpi = 300, width = 10, height = 6.5, units = "in")



