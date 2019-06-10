####### hourly no spag #########


plot_s_ahpsnospag_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(name = "traces", values= c(
      'deterministic\nforecast' = 'black')) +
    labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}

plot_s_ahpsnospag_nomed_likely <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      #'<5% (top)' = '#e41a1c',         # '<5% (top)' = '#e41a1c',
      'Less Likely 5-95%' = 'yellow',         # '10 -  5%' = '#377eb8',
      'Likely 10-90%' = 'green',          # '25 - 10%' = '#4daf4a', 
      'Most Likely 25-75%' = 'blue',         # '50 - 25%' = '#984ea3',
      'median' = 'blue4',         # 'median' = 'blue4',
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
    
    scale_color_manual(name = "traces", values= c(
      'deterministic\nforecast' = 'black')) +
    labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}



plot_s_ahpsnospag_levs_nomed_likely <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      #'<5% (top)' = '#e41a1c',         # '<5% (top)' = '#e41a1c',
      'Less Likely 5-95%' = 'yellow',         # '10 -  5%' = '#377eb8',
      'Likely 10-90%' = 'green',          # '25 - 10%' = '#4daf4a', 
      'Most Likely 25-75%' = 'blue',         # '50 - 25%' = '#984ea3',
      'median' = 'blue4',         # 'median' = 'blue4',
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
    
    scale_color_manual(name = "traces", values= c(
      'deterministic\nforecast' = 'black')) + labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

plot_s_ahpsnospag_levs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(name = "traces", values= c(
      'deterministic\nforecast' = 'black')) + labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}





plot_s_ahpsnospag_obs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}

plot_s_ahpsnospag_obs_nomed_likely <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      #'<5% (top)' = '#e41a1c',         # '<5% (top)' = '#e41a1c',
      'Less Likely 5-95%' = 'yellow',         # '10 -  5%' = '#377eb8',
      'Likely 10-90%' = 'green',          # '25 - 10%' = '#4daf4a', 
      'Most Likely 25-75%' = 'blue',         # '50 - 25%' = '#984ea3',
      'median' = 'blue4',         # 'median' = 'blue4',
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
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpsnospag_obs_levs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpsnospag_obs_levs_nomed_likely <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill='Most Likely 25-75%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill='Likely 10-90%'),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill='Less Likely 5-95%'),   alpha=0.2) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      #'<5% (top)' = '#e41a1c',         # '<5% (top)' = '#e41a1c',
      'Less Likely 5-95%' = 'yellow',         # '10 -  5%' = '#377eb8',
      'Likely 10-90%' = 'green',          # '25 - 10%' = '#4daf4a', 
      'Most Likely 25-75%' = 'blue',         # '50 - 25%' = '#984ea3',
      'median' = 'blue4',         # 'median' = 'blue4',
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
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}
######################################
####### period no spag ###########
#####################################

plot_s_permaxwithenv_nomaxpntsnohrlymednospag <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black'),
                       # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                       #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( 16)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_levs_nomaxpntsnohrlymednospag  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black'),
                       # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                       #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c(16)))) + 
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_obs_nomaxpntsnohrlymednospag  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         #'individual\nensemble\#ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "solid"),
                         shape = c(16,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}

plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date,fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         #  'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "solid"),
                         shape = c(16,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}

#####################################
####### hourly with spag #########
#####################################


plot_s_ahpswithspag_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    #  scale_color_manual(name = "traces", values= c(
    #    'deterministic\nforecast' = 'black',
    #    'individual\nensemble\ntrace ("year")' = 'gray',
    #    'observed' = 'purple'), shape = c(16, NA, 16)) + 
    #  
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'gray'),
                       
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c( 16, NA)))) +
    
    
    labs(y = "feet", x = NULL) +
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpswithspag_levs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(name = "traces", values= c(
      'deterministic\nforecast' = 'black',
      'individual\nensemble\ntrace ("year")' = 'gray'),
      guide = guide_legend(override.aes = list(
        linetype = c("solid",    "solid"),
        shape = c( 16, NA)))) +
    
    
    labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

plot_s_ahpswithspag_obs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpswithspag_obs_levs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}



##########################################################
####### hourly no spag no dtrmnstc (ribbons only)#########
##########################################################


plot_s_ahpswithspag_nomed_justribs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    # geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    #geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    #  scale_color_manual(name = "traces", values= c(
    #    'deterministic\nforecast' = 'black',
    #    'individual\nensemble\ntrace ("year")' = 'gray',
    #    'observed' = 'purple'), shape = c(16, NA, 16)) + 
    #  
    #scale_color_manual(name = "traces",
    #                   
    #                   values= c(
    #                     'deterministic\nforecast' = 'black',
    #                     'individual\nensemble\ntrace ("year")' = 'gray'),
  #                   
  #                   guide = guide_legend(override.aes = list(
  #                     linetype = c("solid",    "solid"),
  #                     shape = c( 16, NA)))) +
  #
  
  labs(y = "feet", x = NULL) +
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}

plot_s_ahpswithspag_nomed_justribs_obs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    # geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    #geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         # 'deterministic\nforecast' = 'black',
                         'observed' = 'purple'),
                       
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( 16)))) +
    
    
    labs(y = "feet", x = NULL) +
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpswithspag_nomed_justribs_levs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
  
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
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    # geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    #geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    # geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    #geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    # scale_color_manual(name = "traces",
    #                    
    #                    values= c(
    #                      # 'deterministic\nforecast' = 'black',
    #                      'observed' = 'purple'),
    #                    
    #                    guide = guide_legend(override.aes = list(
    #                      linetype = c("solid"),
    #                      shape = c( 16)))) +
    
  
  labs(y = "feet", x = NULL) +
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpswithspag_nomed_justribs_obs_levs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    # geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    #geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         # 'deterministic\nforecast' = 'black',
                         'observed' = 'purple'),
                       
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( 16)))) +
    
    
    labs(y = "feet", x = NULL) +
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


###################################################################
################# per max ribbons #################################
###################################################################


plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = NULL) , size = 0.2, alpha = 0) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    #geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf'),
                      #'(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' )) +
    #'(envelope)')) +
    
    #  scale_color_manual(name = "traces",
    #                     
    #                     values= c(
    #                       'deterministic\nforecast' = 'black'),
    #                     # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
    #                     #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
    #                     guide = guide_legend(override.aes = list(
    #                       linetype = c("solid"),
    #                       shape = c( 16)))) + labs(x = NULL) +
  scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL)) +   labs( x = NULL) 
  
}

plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_obs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date,fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = NULL) , size = 0.2, alpha = 0.0) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    #geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    
    
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf'),
                      #'(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' )) +
    #'(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'observed' = 'purple'),
                       # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                       #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( 16)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  
}


plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_levs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = NULL) , size = 0.2, alpha = 0.0) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    #geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    
    #geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    #geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    
    
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf'),
                      #'(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' )) +
    #'(envelope)')) +
    
    #scale_color_manual(name = "traces",
    #                   
    #                   values= c(
    #                     'observed' = 'purple'),
    #                   # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
    #                   #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
    #                   guide = guide_legend(override.aes = list(
    #                     linetype = c("solid"),
    #                     shape = c( 16)))) + labs(x = NULL) +
  scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  
}


plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_obs_levs <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = NULL) , size = 0.2, alpha = 0.0) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    #geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    #geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    
    
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf'),
                      #'(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' )) +
    #'(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'observed' = 'purple'),
                       # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                       #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( 16)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  
}


#####################################
####### period with spag #########
#####################################


plot_s_permaxwithenv_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'individual\nensemble\ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  NA)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_levs_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'individual\nensemble\ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16, NA)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_obs_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date,fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         #'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid", "solid"),
                         shape = c(16, NA,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date,fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         #  'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


#####################################
####### hourly with spag #########
#####################################


plot_s_ahpswithspag_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    #  scale_color_manual(name = "traces", values= c(
    #    'deterministic\nforecast' = 'black',
    #    'individual\nensemble\ntrace ("year")' = 'gray',
    #    'observed' = 'purple'), shape = c(16, NA, 16)) + 
    #  
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'gray'),
                       
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c( 16, NA)))) +
    
    
    labs(y = "feet", x = NULL) +
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpswithspag_levs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    scale_color_manual(name = "traces", values= c(
      'deterministic\nforecast' = 'black',
      'individual\nensemble\ntrace ("year")' = 'gray'),
      guide = guide_legend(override.aes = list(
        linetype = c("solid",    "solid"),
        shape = c( 16, NA)))) +
    
    
    labs(y = "feet", x = NULL) +
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

plot_s_ahpswithspag_obs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


plot_s_ahpswithspag_obs_levs_nomed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  
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
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median, linetype = "median"), color = "blue") +
    
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=fcast_t_pst, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    geom_line  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    geom_point  (data = dtrmnstc,     aes(x=fcast_t_pst, y=feet, color = "deterministic\nforecast")) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = 'observed'))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet,  color = 'observed'))+
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
    #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(values = c(
      '<5% (top)' = '#e41a1c',
      '10 -  5%' = '#377eb8',
      '25 - 10%' = '#4daf4a', 
      '50 - 25%' = '#984ea3',
      'median' = 'blue4',
      '75 - 50%' = '#ff7f00',
      '90 - 75%' = '#ffff33',
      '95 - 90%' = '#a65628',
      '>95% (btm)' = '#f781bf'),
      breaks = c(
        '<5% (top)' ,
        '10 -  5%' ,
        '25 - 10%' , 
        '50 - 25%' ,
        'median',
        '75 - 50%' ,
        '90 - 75%' ,
        '95 - 90%',
        '>95% (btm)' )) +
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) +
    
    
    guides(fill=guide_legend(title="hourly,\nstage\nquantile")) +
    #guides(color=guide_legend(title="series")) + 
    scale_linetype_manual(name = "hourly,\nstage\nstatistic",values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
}


#####################################
####### period with spag #########
#####################################


plot_s_permaxwithenv_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'individual\nensemble\ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  NA)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_levs_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'individual\nensemble\ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16, NA)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_obs_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         #'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid", "solid"),
                         shape = c(16, NA,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         #  'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

###############################################
####### period with spag with labels #########
################################################


plot_s_permaxwithenv_nomaxpntsnohrlymed_withlab <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'individual\nensemble\ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16,  NA)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_levs_nomaxpntsnohrlymed_withlab <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'individual\nensemble\ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid"),
                         shape = c(16, NA)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_obs_nomaxpntsnohrlymed_withlab <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         #'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid", "solid"),
                         shape = c(16, NA,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date,  fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         'individual\nensemble\ntrace ("year")' = 'wheat4',
                         #  'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid",    "solid",  "solid"),
                         shape = c(16, NA, 16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(sec.axis = dup_axis(name = NULL))
  
}

####################################################
####### period without spag with labels #########
####################################################

plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015))+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black'),
                       # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                       #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c( 16)))) + labs(x = NULL) +
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_levs_nomaxpntsnohrlymednospag_withlab  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black'),
                       # 'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                       #'individual\nensemble\#ntrace ("year")' = 'wheat4'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid"),
                         shape = c(16)))) + 
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}


plot_s_permaxwithenv_obs_nomaxpntsnohrlymednospag_withlab  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    #geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    #geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    #geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         #'individual\nensemble\#ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "solid"),
                         shape = c(16,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}

plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag_withlab  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate,  obs_start_date, obs_end_date, fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "feet", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "feet", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(feet == max(feet)) %>% mutate(tracepermaxstage = feet)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
  ggplot() + 
    #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = feet, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2, alpha = 0.4) +
    #geom_line  (data = df_qntls_h,  aes(x=fcast_t_pst, y=median,  linetype = "median hourly peak"), color = "blue", alpha = 0.5) +
    #geom_point(data= df_mefpyr_max, aes(x = fcast_t_pst, y = tracepermaxstage, color = 'individual\nensemble\ntrace ("year") peak'), size = 2, alpha = 0.5) +
    geom_ribbon(data = df_qntls_h,  aes(x=fcast_t_pst, ymin= `min`, ymax = `max`,fill="(envelope)"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `95%`, ymax = max,  fill="<5% (top)"),  alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
    #  geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= median, ymax = median,fill="median"),   alpha=0.9) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
    geom_rect(data = df_qntls_p,  aes(xmin=as.POSIXct(fcaststart), xmax= as.POSIXct(fcastend), ymin= `min`, ymax = `5%`,fill=">95% (btm)"), alpha=0.2) +
    
    
    geom_segment(data = df_qntls_p, 
                 aes(x=as.POSIXct(fcaststart),xend=as.POSIXct(fcastend),
                     y=median,yend=median, linetype = "median\nperiod\npeak"), color = "blue", alpha = 0.7) +
    geom_line(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    geom_point(data = dtrmnstc, aes( x = fcast_t_pst, y = feet,color = "deterministic\nforecast" )) +
    
    geom_line(data = filter(usgs_obs, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+
    geom_point(data = filter(usgs_obs_hrly, pst >= obs_start_date, pst<= fcastenddate, pst <= obs_end_date), aes(x = pst, y = feet, color = "observed"))+ 
    
    geom_label(data = df_qntls_p_labs, aes(y = val, label = paste0(lab, " (",round(val,2), ")")), x = midpoint) + 
    
    
    geom_hline(yintercept = monitorstage_feet, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
    geom_hline(yintercept = floodstage_feet, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
    
    geom_label(data = filter(notify_levs, unit == "feet"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
    scale_x_datetime(expand = c(0.015, 0.015), name = NULL)+
    #date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
    
    
    
    scale_color_manual(name = "traces",
                       
                       values= c(
                         'deterministic\nforecast' = 'black',
                         #'individual\nensemble\#ntrace ("year")' = 'wheat4',
                         #  'individual\nensemble\ntrace ("year") peak' = 'wheat4',
                         'observed' = 'purple'),
                       guide = guide_legend(override.aes = list(
                         linetype = c("solid", "solid"),
                         shape = c(16,  16)))) + 
    scale_fill_manual(name = "period,\npeak stage\nquantile",
                      
                      values = c(
                        '<5% (top)' = '#e41a1c',
                        '10 -  5%' = '#377eb8',
                        '25 - 10%' = '#4daf4a', 
                        '50 - 25%' = '#984ea3',
                        #  'median' = 'blue4',
                        '75 - 50%' = '#ff7f00',
                        '90 - 75%' = '#ffff33',
                        '95 - 90%' = '#a65628',
                        '>95% (btm)' = '#f781bf',
                        '(envelope)' = 'wheat4'),
                      breaks = c(
                        '<5% (top)' ,
                        '10 -  5%' ,
                        '25 - 10%' , 
                        '50 - 25%' ,
                        #'median',
                        '75 - 50%' ,
                        '90 - 75%' ,
                        '95 - 90%',
                        '>95% (btm)' ,
                        '(envelope)')) +
    
    
    scale_linetype_manual(name = "peak stage\nstatistic",values = c(2, 2),
                          guide = guide_legend(override.aes = list(color = c("blue")))) +
    scale_y_continuous(name = "feet", sec.axis = dup_axis(name = NULL))
  
}