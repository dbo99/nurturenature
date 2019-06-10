firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)

plot_s_ahpsnospag_med_likely <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  df_qntls_h    <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  midpoint <- as.POSIXct((as.numeric(max(df_hefs$fcast_t_pst)) + 
                            as.numeric(min(df_hefs$fcast_t_pst))) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2) +
    geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "Median"), color = "blue") +
    
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    #geom_ribbon(data = filter(df_qntls_h, qtype == 6),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "8AM Official\nForecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "8AM Official\nForecast")) +
    geom_vline(xintercept = firstdtrmnstc, linetype = "solid", color = "red", show.legend = F, size = 0.5) + 
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      #'<5% (top)' = '#e41a1c',         # '<5% (top)' = '#e41a1c',
      'Most Likely 25-75%' = 'blue',         # '50 - 25%' = '#984ea3',

      'Likely 10-90%' = 'green',          # '25 - 10%' = '#4daf4a', 
      ' Less Likely 5-95%' = 'yellow',         # '10 -  5%' = '#377eb8',
      
      #'median' = 'blue4',         # 'median' = 'blue4',
      'Most Likely 25-75%' = 'blue',    # '75 - 50%' = '#ff7f00',
      'Likely 10-90%' = 'green',       # '90 - 75%' = '#ffff33',
      'Less Likely 5-95%' = 'yellow'),              #'95 - 90%' = '#a65628',
      # '>95% (btm)' = '#f781bf'),             #'>95% (btm)' = '#f781bf'),
      breaks = c(
        # '<5% (top)' ,       # '<5% (top)' ,
        '10 -  5%' ,       # '10 -  5%' ,
        'Likely 10-90%' ,        # '25 - 10%' , 
        'Most Likely 25-75%' ,       # '50 - 25%' ,
        'median',       # 'median',
        'Most Likely 25-75%' ,       # '75 - 50%' ,
        'Likely 10-90%' ,       # '90 - 75%' ,
        'Less Likely 5-95%')) +       # '95 - 90%',
    #'>95% (btm)' )) +       # '>95% (btm)' )) +
    
    scale_color_manual(name = "", values= c(
      "8AM Official\nForecast" = 'black')) +
    labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(breaks = 
                         
                         c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)) + 
    scale_x_datetime(expand = c(0.015, 0.015),
                     date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    labs(y = "River Level (feet)")
}

pday5 <- plot_s_ahpsnospag_med_likely(df_hefs, dtrmnstc, mdy("1/15/2016") ,mdy("1/21/2016"),  fcasts[3], 6) #+ ggtitle("4 Days Out, Morning Forecast\n5-Day Maximum Peak Stage Probabilities")
pday5
ggsave("last_slide.jpg", dpi = 300, width = 7.5, height = 4, units = "in")
