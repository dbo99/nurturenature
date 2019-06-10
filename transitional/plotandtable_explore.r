source("libs.r")
{
am_fcast <- fcasts[3]#ymd_hms("2016-01-15 08:00:00")
fcast_start_date <- ymd("2014-10-15")
#fcaststartdate <- ymd("2016-01-15")
fcast_end_date <- ymd("2014-10-25")
obs_start_date <- ymd("2014-10-15")
obs_end_date <- ymd("2014-10-20")
q_type <- 6

dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == am_fcast, 
                                     fcast_t_pst >= fcast_start_date,
                                     fcast_t_pst <= fcast_end_date)

firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
df_hefs       <- df_hefs %>% filter (fcast_i_pst == am_fcast)
df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcast_start_date, fcast_t_pst <= fcast_end_date)
}

#df_qntls_p <- create_permax_quants(df_hefs, fcast_start_date, fcast_end_date, "mefp_yr", "feet", q_type) 
#as_tibble(df_qntls_p)
df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcast_start_date, fcast_end_date, "mefp_yr", "kcfs", q_type)
as_tibble(df_qntls_p_labs)
df_qntls_p_labs2 <- create_permax_quants_sumtable_f(df_hefs, fcast_start_date, fcast_end_date, "mefp_yr", "kcfs", q_type) 
  as_tibble(df_qntls_p_labs2)  
p1a <- plot_s_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
p2c <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
p1a
p_table <- head(df_qntls_p_labs)
p_table <- grid.table(p_table)
tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
p_table <- tableGrob(df_qntls_p_labs, rows=NULL, theme=tt)
#plot_grid(p1a, p2c)
plot_grid(p1a, p_table)
p_table

p_table <- p_excd_table_f(df_hefs, fcast_start_date, fcast_end_date, am_fcast, q_type) 
plot_grid(p1a, p_table)

p1 <- p_excd_table_f (df_hefs, fcast_start_date, fcast_end_date, am_fcast, q_type)
plot_grid(p1, p1)


############################

plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, obs_start_date, obs_end_date,  fcast_i_pddate, q_type) {
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  #create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)  
  df_qntls_h <- create_hrly_quants(df_hefs, "fcast_t_pst", "kcfs", q_type) 
  
  df_qntls_p <- create_permax_quants(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "kcfs", q_type) 
  df_qntls_p_labs <- create_permax_quants_labs(df_hefs, fcaststartdate, fcastenddate, "mefp_yr", "kcfs", q_type) 
  
  df_mefpyr_max <- df_hefs  %>% group_by(mefp_yr) %>% filter(kcfs == max(kcfs)) %>% mutate(tracepermaxflow = kcfs)
  fcaststart <- min(df_hefs$fcast_t_pst)
  fcastend   <- max(df_hefs$fcast_t_pst)
  midpoint <- as.POSIXct((as.numeric(fcaststart) + 
                            as.numeric(fcastend)) / 2, origin = '1970-01-01')  %>% round_date("hours")
  
p_excd_table_f  <- function(df_hefs, dtrmnstc, fcaststartdate, fcastenddate, fcast_i_pddate, q_type) {
  
  dtrmnstc      <- dtrmnstc %>% filter(fcast_i_pst == fcast_i_pddate, 
                                       fcast_t_pst >= fcaststartdate,
                                       fcast_t_pst <= fcastenddate)
  
  firstdtrmnstc <- min(dtrmnstc$fcast_t_pst)
  df_hefs       <- df_hefs %>% filter (fcast_i_pst == fcast_i_pddate)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= firstdtrmnstc)
  df_hefs       <- df_hefs %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate)
  
  
  df_qntls_tablerows <- create_permax_quants_sumtable_f(df_hefs, fcast_start_date, fcast_end_date, "mefp_yr", "kcfs", q_type)
  
  
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tableGrob(df_qntls_tablerows, rows=NULL, theme=tt)
}