create_hrly_quants <- function(df, hourcolumn, value, qtype)   {
  
  value <- rlang::sym(value)
  
  df  %>% group_by_at(vars(hourcolumn)) %>%
    summarize(
      `5%`=quantile(!!value, probs=0.05, type = qtype), 
      `10%`=quantile(!!value, probs=0.10, type = qtype),
      `25%`=quantile(!!value, probs=0.25, type = qtype),
      `50%`=quantile(!!value, probs=0.5,  type = qtype),
      `75%`=quantile(!!value, probs=0.75, type = qtype),
      `90%`=quantile(!!value, probs=0.9,  type = qtype),
      `95%`=quantile(!!value, probs=0.95, type = qtype),
      `min`=min(!!value), `max`=max(!!value), mean=mean(!!value), median=median(!!value), 
      n=n()) %>% mutate(qtype = qtype)
}

create_hrly_quants_i <- function(df, hourcolumn, value, qtype, fcast_i_pst)   {
  
  value <- rlang::sym(value)
  
  df  %>% group_by_at(vars(hourcolumn)) %>%
    summarize(
      `5%`=quantile(!!value, probs=0.05, type = qtype), 
      `10%`=quantile(!!value, probs=0.10, type = qtype),
      `25%`=quantile(!!value, probs=0.25, type = qtype),
      `50%`=quantile(!!value, probs=0.5,  type = qtype),
      `75%`=quantile(!!value, probs=0.75, type = qtype),
      `90%`=quantile(!!value, probs=0.9,  type = qtype),
      `95%`=quantile(!!value, probs=0.95, type = qtype),
      `min`=min(!!value), `max`=max(!!value), mean=mean(!!value), median=median(!!value), 
      n=n()) %>% mutate(qtype = qtype, fcast_i_pst_hms = fcast_i_pst, fcast_i_pst_date = date(fcast_i_pst_hms))
}

create_permax_quants <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)    {
  
  value <- rlang::sym(value)
  
  df  %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate) %>%
    group_by_at(vars(yeartrace)) %>%
    summarize(maxtraceval = max(!!value))  %>%
    summarize(
      `5%`=quantile(maxtraceval, probs=0.05, type = qtype), 
      `10%`=quantile(maxtraceval, probs=0.10, type = qtype),
      `25%`=quantile(maxtraceval, probs=0.25, type = qtype),
      `50%`=quantile(maxtraceval, probs=0.5, type = qtype),
      `75%`=quantile(maxtraceval, probs=0.75, type = qtype),
      `90%`=quantile(maxtraceval, probs=0.9, type = qtype),
      `95%`=quantile(maxtraceval, probs=0.95, type = qtype),
      `min`=min(maxtraceval), `max`=max(maxtraceval), mean=mean(maxtraceval), median=median(maxtraceval), 
      n=n()) %>% mutate(qtype = qtype)
}

create_permax_quants_hourly <- function(df, startdatetime, enddatetime, yeartrace, value, qtype)    {
  
  value <- rlang::sym(value)
  
  df  %>% filter(fcast_t_pst >= startdatetime, fcast_t_pst <= enddatetime) %>%
    group_by_at(vars(yeartrace)) %>%
    summarize(maxtraceval = max(!!value))  %>%
    summarize(
      `5%`=quantile(maxtraceval, probs=0.05, type = qtype), 
      `10%`=quantile(maxtraceval, probs=0.10, type = qtype),
      `25%`=quantile(maxtraceval, probs=0.25, type = qtype),
      `50%`=quantile(maxtraceval, probs=0.5, type = qtype),
      `75%`=quantile(maxtraceval, probs=0.75, type = qtype),
      `90%`=quantile(maxtraceval, probs=0.9, type = qtype),
      `95%`=quantile(maxtraceval, probs=0.95, type = qtype),
      `min`=min(maxtraceval), `max`=max(maxtraceval), mean=mean(maxtraceval), median=median(maxtraceval), 
      n=n()) %>% mutate(qtype = qtype)
}













create_permax_quants_labs <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype)    {
  
  value <- rlang::sym(value)
  
  df  %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate) %>%
    group_by_at(vars(yeartrace)) %>%
    summarize(maxtraceval = max(!!value))  %>%
    summarize(
      `95%`=quantile(maxtraceval, probs=0.05, type = qtype), 
      `90%`=quantile(maxtraceval, probs=0.10, type = qtype),
      `75%`=quantile(maxtraceval, probs=0.25, type = qtype),
      `25%`=quantile(maxtraceval, probs=0.75, type = qtype),
      `10%`=quantile(maxtraceval, probs=0.9, type = qtype),
      `5%`=quantile(maxtraceval, probs=0.95, type = qtype), `max peak (all traces)`=max(maxtraceval),
      `median peak`=median(maxtraceval)) %>% gather(key = lab, value = val)
  
}

create_permax_quants_i <- function(df, fcaststartdate, fcastenddate, yeartrace, value, qtype, fcast_i_pst)    {
  
  value <- rlang::sym(value)
  
  df  %>% filter(fcast_t_pst >= fcaststartdate, fcast_t_pst <= fcastenddate) %>%
    group_by_at(vars(yeartrace)) %>%
    summarize(maxtraceval = max(!!value))  %>%
    summarize(
      `5%`=quantile(maxtraceval, probs=0.05, type = qtype), 
      `10%`=quantile(maxtraceval, probs=0.10, type = qtype),
      `25%`=quantile(maxtraceval, probs=0.25, type = qtype),
      `50%`=quantile(maxtraceval, probs=0.5, type = qtype),
      `75%`=quantile(maxtraceval, probs=0.75, type = qtype),
      `90%`=quantile(maxtraceval, probs=0.9, type = qtype),
      `95%`=quantile(maxtraceval, probs=0.95, type = qtype),
      `min`=min(maxtraceval), `max`=max(maxtraceval), mean=mean(maxtraceval), median=median(maxtraceval), 
      n=n()) %>% mutate(qtype = qtype, fcast_i_pst_hms = fcast_i_pst, fcast_i_pst_date = date(fcast_i_pst_hms))
}