Sys.setenv(TZ="America/Los_Angeles")
#source("libs.r")

source("vanduzen_ahps_natvnatoct14_withplotly.r")



fcasts_firstam <- rev(as.character(fcasts_firstam))
fcast_end_date_rev <-  rev(seq(date(min(df_hefs$fcast_t_pst)), date(max(df_hefs$fcast_t_pst)), by = '1 day'))



ui <- 
  
  shinyUI(fluidPage(
    
    shinyjs::useShinyjs(), # debugging
    withMathJax(), # to display LaTeX-like formulas
    
    tags$head( # CSS insert
      tags$style(HTML("      
                      td, th, tr {
                      padding-right: 10px;
                      padding-left: 10px;
                      border: 1px solid #d3d3d3;
                      }
                      "))
      ),
    
    #titlePanel("river forecast viewer"),
    
    br(),
    
    sidebarLayout(sidebarPanel(
      
      textInput("ticker", 
                HTML("<a target='_blank' href='https://www.cnrfc.noaa.gov/'>CNRFC </a>forecast point"),
                # "cnrfc forecast point", 
                
                value="brgc1"),
      
      #br(),
      
      radioButtons("focus", ("focus"),
                   c("Stage" = "stage",
                     "Flow" = "flow"),
                     #"Volume" = "volume"),
                   selected = "flow",
                   inline = TRUE),
      
      selectInput("am_fcast", 
                  "CNRFC forecast",
                  
                  fcasts_firstam, selected = fcasts_firstam[5]),
      
      
      selectInput("fcast_start_date", "period start",
                  date(fcasts_firstam), selected = date(fcasts_firstam[5])),
      
      selectInput("fcast_end_date", "period end",
                  fcast_end_date_rev , selected = fcast_end_date_rev[length(fcast_end_date_rev)-10] ),
      
      
      radioButtons("tholds", "thresholds",
                   c("On" = "on",
                     "Off" = "off"),
                   inline = TRUE,
                   selected = "off"),
      
      radioButtons("obs", "observations",
                   c("On" = "on",
                     "Off" = "off"),
                   inline = TRUE,
                   selected = "off"),
      

      
      selectInput("obs_start_date", "observation start",
                  fcast_end_date_rev , selected = fcast_end_date_rev[length(fcast_end_date_rev)] ),
      #date(fcasts_firstam), date(fcasts_firstam)),
      
      selectInput("obs_end_date", "observation end",
                  fcast_end_date_rev , selected = fcast_end_date_rev[length(fcast_end_date_rev)]+5 ),
      #date(fcasts_firstam), date(fcasts_firstam)),
      
      
      radioButtons("plot_type", "plot",                 
                   choices = c("(1) hourly peak perspective (current beta)" ,   "(1a)  (w/ individual HEFS traces (MEFP yr))",  "(2) period peak perspective" , 
                               "(2a) (w/ labels) (image only)", "(2b) (w/ individual HEFS traces (MEFP yr))" , "(2c) (w/ traces & labels) (image only)",
                               "(3a) hourly & period (image only) (just HEFS)", "(3b) hourly & period (image only)",
                               "(3c) hourly & period w/table (image only)"),
                   
                   
                   selected = "(3c) hourly & period w/table (image only)") , 
      
      selectInput("r_qtype",
                  
                  HTML("<a target='_blank' href='https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/quantile'>R's quantile type</a>"),
                  choices = c(4,5,6,7,8,9), selected = 6),
      
      
      
      #br(),
      width = 3
    ),
    
    mainPanel(
      
      tabsetPanel(position=c("right"),
                  tabPanel(strong("image"), 
                           br(),
                           plotOutput("reg_plot",  height = "750px")),
                  #plotlyOutput("reg_plot",  height = "auto")),
                  #plotOutput("scatterplot"),
                  #htmlOutput("metrics_stats"),
                  #htmlOutput("metrics_finance"),
                  #br()
                  #),
                  
                  tabPanel(strong("interactive (no new stats)"), 
                           br(),
                           plotlyOutput("plotly_plot",  height = 700)),
                  #p("..."),
                  #br(),
                  #tableOutput("table_tail"),
                  #code("https://dbo99.shinyapps.io/transitionalevent_oct14/"),
                  #p("threshold event:", a("https://dbo99.shinyapps.io/thresholdevent_jan1/", href="https://dbo99.shinyapps.io/transitionalevent_oct14/")),
                  #br()),
                  
                  
                #  tabPanel(strong("summary data table"), 
                #           br(),
                #           tableOutput("table_head"),
                #           p("..."),
                #           br(),
                #           tableOutput("table_tail"),
                #           #code("https://dbo99.shinyapps.io/transitionalevent_oct14/"),
                #           #p("threshold event:", a("https://dbo99.shinyapps.io/thresholdevent_jan1/", href="https://dbo99.shinyapps.io/transitionalevent_oct14/")),
                #           br()),
                  
                  tabPanel(strong("notes"),
                           br(),
                           tableOutput("notes"),
                           #p("volume: not hooked up"),
                           p("threshold event:", a("https://dbo99.shinyapps.io/thresholdevent_jan16/", href="https://dbo99.shinyapps.io/thresholdevent_jan16/")),
                           p("threshold event w/ unstacked/ranked traces w/volume:", a("https://dbo99.shinyapps.io/vanduzenridgeapp/", href="https://dbo99.shinyapps.io/vanduzenridgeapp/")),
                           p("middle of road event:", a("https://dbo99.shinyapps.io/middlescen_feb19/", href="https://dbo99.shinyapps.io/middlescen_feb19/")),
                           br()),
                  
  
                  br()

      )
    ))
      ))


server <- function(input, output) {
  
  
  output$reg_plot <- renderPlot({
    
    #########################################
    ############## flow  ####################
    #########################################
    
    ######## ahps ###########
    
    if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpsnospag_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpsnospag_obs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpsnospag_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpsnospag_obs_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    
    ######## ahps w/ spag ###########
    
    if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    
    ######## period w/o spag ###########
    
    if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    
    
    ######## period w/o spag ###########
    
    if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}   
    
    
    ######## period w/o spag with labels ###########
    
    if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    
    ######## period w/ spag with labels ###########
    
    if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    ############# 3a #####################
    
    if (input$focus == "flow" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_f_ahpswithspag_nomed_justribs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1b <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "on" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_f_ahpswithspag_nomed_justribs_obs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1b <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_obs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "off" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_f_ahpswithspag_nomed_justribs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1b <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    
    if (input$focus == "flow" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_f_ahpswithspag_nomed_justribs_obs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date,am_fcast, q_type) 
      p1b <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_obs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    
    ############# 3b #####################
    
    
    if (input$focus == "flow" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_f_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p2c <- plot_f_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_f_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p2c <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_f_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p2c <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_f_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p2c <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      print((p1))}  
    
    
    ############### 3c  ###############################
    
    if (input$focus == "flow" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_f_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      #p2c <- plot_f_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p2c <- p_excd_table_f(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type)
      p1 <- plot_grid(p1a, p2c, rel_widths = c(2,1))
      
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_f_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      #p2c <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p2c <- p_excd_table_f(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type)
      p1 <- plot_grid(p1a, p2c, rel_widths = c(2,1))
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_f_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      #p2c <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p2c <- p_excd_table_f(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c, rel_widths = c(2,1))
      print((p1))} 
    
    if (input$focus == "flow" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_f_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      #p1a <- plot_f_ahpswithspag_nomed_justribs_obs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date,am_fcast, q_type) 
      p1b <- p_excd_table_f(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b, rel_widths = c(2,1))
      
      print((p1))} 
    
    
    #########################################
    ############## stage  ####################
    #########################################
    
    ######## ahps ###########
    
    if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpsnospag_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpsnospag_obs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpsnospag_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpsnospag_obs_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    
    ######## ahps w/ spag ###########
    
    if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    
    ######## period w/o spag ###########
    
    if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    
    
    ######## period w/o spag ###########
    
    if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}   
    
    
    ######## period w/o spag with labels ###########
    
    if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    
    ######## period w/ spag with labels ###########
    
    if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      
      print((p1))}    
    
    ######## 3a ###########
    
    if (input$focus == "stage" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_s_ahpswithspag_nomed_justribs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1b <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "on" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_s_ahpswithspag_nomed_justribs_obs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1b <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_obs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "off" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_s_ahpswithspag_nomed_justribs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1b <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    
    if (input$focus == "stage" && input$plot_type == "(3a) hourly & period (image only) (just HEFS)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_s_ahpswithspag_nomed_justribs_obs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date,am_fcast, q_type) 
      p1b <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab_justribs_obs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b)
      
      print((p1))} 
    
    ######## 3b ###########
    
    if (input$focus == "stage" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_s_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p2c <- plot_s_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_s_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p2c <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_s_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p2c <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3b) hourly & period (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_s_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p2c <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c)
      print((p1))}    
    
    ############# 3c  #####################
    
    if (input$focus == "stage" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_s_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      #p2c <- plot_f_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p2c <- p_excd_table_s(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type)
      p1 <- plot_grid(p1a, p2c, rel_widths = c(2,1))
      
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_s_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      #p2c <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p2c <- p_excd_table_s(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type)
      p1 <- plot_grid(p1a, p2c, rel_widths = c(2,1))
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      p1a <- plot_s_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      #p2c <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p2c <- p_excd_table_s(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p2c, rel_widths = c(2,1))
      print((p1))} 
    
    if (input$focus == "stage" && input$plot_type == "(3c) hourly & period w/table (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1a <- plot_s_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      #p1a <- plot_f_ahpswithspag_nomed_justribs_obs_levs(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  obs_start_date, obs_end_date,am_fcast, q_type) 
      p1b <- p_excd_table_s(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- plot_grid(p1a, p1b, rel_widths = c(2,1))
      
      print((p1))} 
    
    
    
  } )
  
  ##################################################################
  ##################################################################
  ################### plotly plot ##################################
  ##################################################################
  ##################################################################
  
  output$plotly_plot <- renderPlotly({
    
    #########################################
    ############## flow  ####################
    #########################################
    
    ######## ahps ###########
    
    if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpsnospag_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpsnospag_obs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpsnospag_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpsnospag_obs_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    
    ######## ahps w/ spag ###########
    
    else if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    ######## period w/o spag ###########
    
    else if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    
    ######## period w/o spag ###########
    
    else if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}   
    
    
    ######## period w/o spag with labels ###########
    
    else if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    ######## period w/ spag with labels ###########
    
    else if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_f_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "flow" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_f_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    #########################################
    ############## stage  ####################
    #########################################
    
    ######## ahps ###########
    
    else if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpsnospag_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpsnospag_obs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpsnospag_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(1) hourly peak perspective (current beta)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpsnospag_obs_levs_nomed_likely(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    
    ######## ahps w/ spag ###########
    
    else if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpswithspag_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpswithspag_obs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_ahpswithspag_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(1a)  (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_ahpswithspag_obs_levs_nomed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    ######## period w/o spag ###########
    
    else if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2) period peak perspective" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    
    ######## period w/o spag ###########
    
    else if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2b) (w/ individual HEFS traces (MEFP yr))" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}   
    
    
    ######## period w/o spag with labels ###########
    
    else if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2a) (w/ labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymednospag_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
    
    ######## period w/ spag with labels ###########
    
    else if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off" && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date,  am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on"  && input$tholds == "off")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "off"  && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      
      
      p1 <- plot_s_permaxwithenv_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))} 
    
    else if (input$focus == "stage" && input$plot_type =="(2c) (w/ traces & labels) (image only)" && input$obs ==  "on" && input$tholds == "on")  {
      
      q_type <- as.integer(input$r_qtype)
      am_fcast <- input$am_fcast
      fcast_start_date <- ymd(input$fcast_start_date)
      fcast_end_date <- ymd(input$fcast_end_date)
      obs_start_date <- ymd(input$obs_start_date)
      obs_end_date <- ymd(input$obs_end_date)
      
      p1 <- plot_s_permaxwithenv_obs_levs_nomaxpntsnohrlymed_withlab(df_hefs, dtrmnstc, fcast_start_date, fcast_end_date, obs_start_date, obs_end_date, am_fcast, q_type) 
      p1 <- ggplotly(p1)
      print((p1))}    
    
  } )
  
  
  
  
  
  
  
  
  
  
  
  
  
}    

shinyApp(ui, server)
#}