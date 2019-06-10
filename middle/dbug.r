source("vanduzen_ahps_middleroad_dfcreate.r")

fcast_start_date <- ymd("2019-01-29")
fcaststartdate <-  fcast_start_date


fcast_end_date <- ymd("2019-02-04")
fcastenddate <- fcast_end_date

am_fcast <- fcasts[1]
fcast_i_pddate <- am_fcast
fcast_i_pddate <- am_fcast
q_type <- 6
debugdate <- ymd("2019-01-29")


dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                     fcast_t_pst >= fcaststartdate,
                                     fcast_t_pst <= fcastenddate)

firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
df_hefs       <- df_hefs %>% filter(fcast_t_pst >= debugdate)


df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
df_qntls_h    <- create_hrly_quants(df_hefs, "fcast_t_pst", "kcfs", q_type) 
midpoint <- as.POSIXct((as.numeric(max(df_hefs$fcast_t_pst)) + 
                          as.numeric(min(df_hefs$fcast_t_pst))) / 2, origin = '1970-01-01')  %>% round_date("hours")

ggplot() + 
  #geom_line(data = df_hefs, aes(x = fcast_t_pst, y = kcfs, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2) +
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
  
  geom_line  (data = dtrmnstc,    aes(x=fcast_t_pst, y=kcfs, color = "deterministic\nforecast")) +
  geom_point  (data = dtrmnstc,   aes(x=fcast_t_pst, y=kcfs, color = "deterministic\nforecast")) +
  
  #geom_hline(yintercept = monitorstage_kcfs, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
  #geom_hline(yintercept = floodstage_kcfs, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
  
  #geom_label(data = filter(notify_levs, unit == "kcfs"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
  
  scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
  #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
  scale_fill_manual(values = c(
    '<5% (top)' = 'snow4',
    '10 -  5%' = 'yellow',
    '25 - 10%' = 'green', 
    '50 - 25%' = 'blue',
    'median' = 'blue4',
    '75 - 50%' = 'blue',
    '90 - 75%' = 'green',
    '95 - 90%' = 'yellow',
    '>95% (btm)' = 'snow4'),
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
  labs(y = "kcfs", x = NULL) +
  guides(fill=guide_legend(title="hourly,\nflow\nquantile")) +
  #guides(color=guide_legend(title="series")) + 
  scale_linetype_manual(name = "hourly,\nflow\nstatistic",values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL))
}