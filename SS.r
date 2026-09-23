library(shiny)
library(DT)

# ==========================================
# Helper Functions
# ==========================================
fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x) | is.infinite(x), "", sprintf("%.4f", as.numeric(x)))
}

# Generaly data table setting
dt_opts <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all"))
)

# No column title data table setting
dt_opts_nohead <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  headerCallback = JS("function(t) {$(t).remove();}")
)

# ==========================================
# User interface
# ==========================================
ui <- fluidPage(
  titlePanel("Comprehensive Sample Size Calculators"),
  
  tags$head(
    tags$style(HTML("
      table.dataTable thead th, table.dataTable tbody td { text-align: center !important; }
      .well { background-color: #fdfdfd; }
      h4 { color: #2c3e50; font-weight: bold; margin-top: 15px; border-bottom: 2px solid #eee; padding-bottom: 5px;}
    "))
  ),
  
  tabsetPanel(
    # ----------------------------------------------------
    # 1. Continuous Outcome
    # ----------------------------------------------------
    tabPanel("Continuous Outcome",
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 radioButtons("cont_type", "Select Design:",
                              choices = c("One Sample", "Matched Sample", "Two Sample"))
               ),
               mainPanel(
                 width = 9,
                 # One Sample
                 conditionalPanel(
                   condition = "input.cont_type == 'One Sample'",
                   fluidRow(
                     column(6, 
                            h4("One Sample Confidence Interval"),
                            wellPanel(
                              numericInput("c1_ci_conf", "Confidence Level:", 0.95, step=0.01),
                              numericInput("c1_ci_sd", "SD:", 15),
                              numericInput("c1_ci_err", "Error:", 11)
                            ),
                            DTOutput("out_c1_ci_z"), br(), DTOutput("out_c1_ci_res")
                     ),
                     column(6,
                            h4("One Sample Mean Test"),
                            wellPanel(
                              numericInput("c1_t_h0", "mean H0:", 15),
                              numericInput("c1_t_h1", "mean H1:", 22),
                              numericInput("c1_t_sd", "SD:", 8),
                              numericInput("c1_t_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("c1_t_power", "power:", 0.80, step=0.01)
                            ),
                            DTOutput("out_c1_t_z"), br(), DTOutput("out_c1_t_res")
                     )
                   )
                 ),
                 # Matched Sample
                 conditionalPanel(
                   condition = "input.cont_type == 'Matched Sample'",
                   fluidRow(
                     column(6, 
                            h4("Matched Sample Confidence Interval"),
                            wellPanel(
                              numericInput("cm_ci_conf", "Confidence Level:", 0.90, step=0.01),
                              numericInput("cm_ci_sd", "SD difference:", 9.1),
                              numericInput("cm_ci_err", "Error:", 2)
                            ),
                            DTOutput("out_cm_ci_z"), br(), DTOutput("out_cm_ci_res")
                     ),
                     column(6,
                            h4("Matched Sample Mean Test"),
                            wellPanel(
                              numericInput("cm_t_diff", "mean difference:", 2),
                              numericInput("cm_t_sd", "SD difference:", 11.6),
                              numericInput("cm_t_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("cm_t_power", "power:", 0.80, step=0.01)
                            ),
                            DTOutput("out_cm_t_z"), br(), DTOutput("out_cm_t_res")
                     )
                   )
                 ),
                 # Two Sample
                 conditionalPanel(
                   condition = "input.cont_type == 'Two Sample'",
                   fluidRow(
                     column(6, 
                            h4("Two Sample Confidence Interval"),
                            wellPanel(
                              numericInput("c2_ci1_conf", "Confidence Level:", 0.90, step=0.01),
                              numericInput("c2_ci1_sd", "SD:", 16),
                              numericInput("c2_ci1_err", "Error:", 2)
                            ),
                            DTOutput("out_c2_ci1_z"), br(), DTOutput("out_c2_ci1_res"),
                            hr(),
                            wellPanel(
                              numericInput("c2_ci2_conf", "Confidence Level:", 0.95, step=0.01),
                              numericInput("c2_ci2_n1", "n 1:", 125),
                              numericInput("c2_ci2_sd1", "SD 1:", 17.1),
                              numericInput("c2_ci2_n2", "n 2:", 175),
                              numericInput("c2_ci2_sd2", "SD 2:", 17.1),
                              numericInput("c2_ci2_err", "Error:", 3)
                            ),
                            DTOutput("out_c2_ci2_z"), br(), DTOutput("out_c2_ci2_res")
                     ),
                     column(6,
                            h4("Two Sample Mean Test"),
                            wellPanel(
                              numericInput("c2_t1_diff", "mean difference:", 8),
                              numericInput("c2_t1_sd", "SD:", 5),
                              numericInput("c2_t1_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("c2_t1_power", "power:", 0.85, step=0.01)
                            ),
                            DTOutput("out_c2_t1_z"), br(), DTOutput("out_c2_t1_res"),
                            hr(),
                            wellPanel(
                              numericInput("c2_t2_m1", "mean 1:", 50),
                              numericInput("c2_t2_n1", "n 1:", 100),
                              numericInput("c2_t2_sd1", "SD 1:", 6),
                              numericInput("c2_t2_m2", "mean 2:", 41),
                              numericInput("c2_t2_n2", "n 2:", 100),
                              numericInput("c2_t2_sd2", "SD 2:", 6),
                              numericInput("c2_t2_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("c2_t2_power", "power:", 0.90, step=0.01)
                            ),
                            DTOutput("out_c2_t2_z"), br(), DTOutput("out_c2_t2_res")
                     )
                   )
                 )
               )
             )
    ),
    
    # ----------------------------------------------------
    # 2. Dichotomous Outcome
    # ----------------------------------------------------
    tabPanel("Dichotomous Outcome",
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 radioButtons("dich_type", "Select Design:",
                              choices = c("One Sample", "Two Sample"))
               ),
               mainPanel(
                 width = 9,
                 # One Sample
                 conditionalPanel(
                   condition = "input.dich_type == 'One Sample'",
                   fluidRow(
                     column(6, 
                            h4("One Sample Confidence Interval"),
                            wellPanel(
                              numericInput("d1_ci_conf", "Confidence Level:", 0.95, step=0.01),
                              numericInput("d1_ci_p", "p:", 0.50, step=0.05),
                              numericInput("d1_ci_err", "Error:", 0.05, step=0.01)
                            ),
                            DTOutput("out_d1_ci_z"), br(), DTOutput("out_d1_ci_res")
                     ),
                     column(6,
                            h4("One Sample Proportion Test"),
                            wellPanel(
                              numericInput("d1_t_p0", "prop H0:", 0.10, step=0.05),
                              numericInput("d1_t_p1", "prop H1:", 0.12, step=0.05),
                              numericInput("d1_t_sd", "SD:", 0.25, step=0.05),
                              numericInput("d1_t_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("d1_t_power", "power:", 0.85, step=0.01)
                            ),
                            DTOutput("out_d1_t_z"), br(), DTOutput("out_d1_t_res")
                     )
                   )
                 ),
                 # Two Sample
                 conditionalPanel(
                   condition = "input.dich_type == 'Two Sample'",
                   fluidRow(
                     column(6, 
                            h4("Two Sample Confidence Interval"),
                            wellPanel(
                              numericInput("d2_ci1_conf", "Confidence Level:", 0.95, step=0.01),
                              numericInput("d2_ci1_p1", "p 1:", 0.10, step=0.05),
                              numericInput("d2_ci1_p2", "p 2:", 0.30, step=0.05),
                              numericInput("d2_ci1_err", "Error:", 0.05, step=0.01)
                            ),
                            DTOutput("out_d2_ci1_z"), br(), DTOutput("out_d2_ci1_res"),
                            hr(),
                            wellPanel(
                              numericInput("d2_ci2_conf", "Confidence Level:", 0.95, step=0.01),
                              numericInput("d2_ci2_x1", "x 1:", 32),
                              numericInput("d2_ci2_n1", "n 1:", 100),
                              numericInput("d2_ci2_x2", "x 2:", 17),
                              numericInput("d2_ci2_n2", "n 2:", 100),
                              numericInput("d2_ci2_err", "Error:", 0.05, step=0.01)
                            ),
                            DTOutput("out_d2_ci2_z"), br(), DTOutput("out_d2_ci2_res")
                     ),
                     column(6,
                            h4("Two Sample Proportion Test"),
                            wellPanel(
                              numericInput("d2_t1_diff", "prop difference:", 0.10, step=0.05),
                              numericInput("d2_t1_meanp", "mean proportion:", 0.25, step=0.05),
                              numericInput("d2_t1_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("d2_t1_power", "power:", 0.85, step=0.01)
                            ),
                            DTOutput("out_d2_t1_z"), br(), DTOutput("out_d2_t1_res"),
                            hr(),
                            wellPanel(
                              numericInput("d2_t2_x1", "x 1:", 15),
                              numericInput("d2_t2_n1", "n 1:", 100),
                              numericInput("d2_t2_x2", "x 2:", 7),
                              numericInput("d2_t2_n2", "n 2:", 100),
                              numericInput("d2_t2_alpha", "alpha:", 0.05, step=0.01),
                              numericInput("d2_t2_power", "power:", 0.80, step=0.01)
                            ),
                            DTOutput("out_d2_t2_z"), br(), DTOutput("out_d2_t2_res")
                     )
                   )
                 )
               )
             )
    ),
    
    # ----------------------------------------------------
    # 3. Survey Sample Size
    # ----------------------------------------------------
    tabPanel("Survey Sample Size",
             br(),
             fluidRow(
               column(4, offset = 4,
                      h4("Survey Sample Size", align="center"),
                      wellPanel(
                        numericInput("surv_pop", "Population Size:", 9500),
                        numericInput("surv_conf", "Confidence Level:", 0.95, step=0.01),
                        numericInput("surv_err", "Margin of Error:", 0.05, step=0.01),
                        numericInput("surv_rate", "Response Rate (0-1):", 1.00, step=0.05)
                      ),
                      DTOutput("out_surv_z"), br(), DTOutput("out_surv_res")
               )
             )
    ),
    
    # ----------------------------------------------------
    # 4. Thematic Analysis
    # ----------------------------------------------------
    tabPanel("Thematic Analysis",
             br(),
             fluidRow(
               column(6,
                      h4("Sample Size Calculator", align="center"),
                      wellPanel(
                        numericInput("ta_s_power", "Power:", 0.95, step=0.01),
                        numericInput("ta_s_inst", "Number of Instances:", 4),
                        numericInput("ta_s_prev", "Theme Prevalence:", 0.20, step=0.05)
                      ),
                      DTOutput("out_ta_s_res")
               ),
               column(6,
                      h4("Power Calculator", align="center"),
                      wellPanel(
                        numericInput("ta_p_size", "Sample Size:", 35),
                        numericInput("ta_p_inst", "Number of Instances:", 5),
                        numericInput("ta_p_prev", "Theme Prevalence:", 0.25, step=0.05)
                      ),
                      DTOutput("out_ta_p_res")
               )
             )
    ),
    
    # ----------------------------------------------------
    # 5. P-Value from CI
    # ----------------------------------------------------
    tabPanel("P-Value from CI",
             br(),
             fluidRow(
               column(6,
                      h4("P-Value from CI for a Difference", align="center"),
                      wellPanel(
                        numericInput("pv_d_est", "Estimate:", 1.9),
                        numericInput("pv_d_lcl", "LCL:", -0.6),
                        numericInput("pv_d_ucl", "UCL:", 4.3),
                        numericInput("pv_d_ci", "CI %:", 95)
                      ),
                      DTOutput("out_pv_d_res")
               ),
               column(6,
                      h4("P-Value from CI for a Ratio", align="center"),
                      wellPanel(
                        numericInput("pv_r_est", "Estimate:", 0.12),
                        numericInput("pv_r_lcl", "LCL:", 0.04),
                        numericInput("pv_r_ucl", "UCL:", 0.25),
                        numericInput("pv_r_ci", "CI %:", 95)
                      ),
                      DTOutput("out_pv_r_res")
               )
             )
    )
  )
)

# ==========================================
# Server logic
# ==========================================
server <- function(input, output, session) {
  
  # ========================================================
  # 1. Continuous Outcome
  # ========================================================
  # One Sample CI
  output$out_c1_ci_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$c1_ci_conf)/2)
    datatable(data.frame(Metric = "z", Value = fmt(z_val)), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_c1_ci_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$c1_ci_conf)/2)
    calc <- (z_val * input$c1_ci_sd / input$c1_ci_err)^2
    df <- data.frame(` ` = "n", calculated = fmt(calc), final = ceiling(calc), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # One Sample Mean Test
  output$out_c1_t_z <- renderDT({
    es <- abs(input$c1_t_h1 - input$c1_t_h0) / input$c1_t_sd
    z_a_1 <- qnorm(1 - input$c1_t_alpha)
    z_a_2 <- qnorm(1 - input$c1_t_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$c1_t_power))
    z_b_2 <- qnorm(min(0.999999, input$c1_t_power + input$c1_t_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_c1_t_res <- renderDT({
    es <- abs(input$c1_t_h1 - input$c1_t_h0) / input$c1_t_sd
    z_a_1 <- qnorm(1 - input$c1_t_alpha) 
    z_a_2 <- qnorm(1 - input$c1_t_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$c1_t_power))
    z_b_2 <- qnorm(min(0.999999, input$c1_t_power + input$c1_t_alpha/2))
    n_1 <- ((z_a_1 + z_b_1) / es)^2; n_2 <- ((z_a_2 + z_b_2) / es)^2
    df <- data.frame(n = c("1-sided", "2-sided"), calculated = c(fmt(n_1), fmt(n_2)), final = c(ceiling(n_1), ceiling(n_2)))
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Matched Sample CI
  output$out_cm_ci_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$cm_ci_conf)/2)
    datatable(data.frame(Metric = "z", Value = fmt(z_val)), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_cm_ci_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$cm_ci_conf)/2)
    calc <- (z_val * input$cm_ci_sd / input$cm_ci_err)^2
    df <- data.frame(` ` = "n", calculated = fmt(calc), final = ceiling(calc), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Matched Sample Mean Test
  output$out_cm_t_z <- renderDT({
    es <- input$cm_t_diff / input$cm_t_sd
    z_a_1 <- qnorm(1 - input$cm_t_alpha)
    z_a_2 <- qnorm(1 - input$cm_t_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$cm_t_power))
    z_b_2 <- qnorm(min(0.999999, input$cm_t_power + input$cm_t_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_cm_t_res <- renderDT({
    es <- input$cm_t_diff / input$cm_t_sd
    z_a_1 <- qnorm(1 - input$cm_t_alpha)
    z_a_2 <- qnorm(1 - input$cm_t_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$cm_t_power))
    z_b_2 <- qnorm(min(0.999999, input$cm_t_power + input$cm_t_alpha/2))
    n_1 <- ((z_a_1 + z_b_1) / es)^2; n_2 <- ((z_a_2 + z_b_2) / es)^2
    df <- data.frame(n = c("1-sided", "2-sided"), calculated = c(fmt(n_1), fmt(n_2)), final = c(ceiling(n_1), ceiling(n_2)))
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample CI 1
  output$out_c2_ci1_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$c2_ci1_conf)/2)
    datatable(data.frame(Metric = "z", Value = fmt(z_val)), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_c2_ci1_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$c2_ci1_conf)/2)
    calc <- 2 * (z_val * input$c2_ci1_sd / input$c2_ci1_err)^2
    df <- data.frame(` ` = c("n 1", "n 2", "Total"), 
                     calculated = c(fmt(calc), fmt(calc), fmt(calc*2)), 
                     final = c(ceiling(calc), ceiling(calc), ceiling(calc)*2), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample CI 2
  output$out_c2_ci2_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$c2_ci2_conf)/2)
    sd_p <- sqrt(((input$c2_ci2_n1-1)*input$c2_ci2_sd1^2 + (input$c2_ci2_n2-1)*input$c2_ci2_sd2^2) / (input$c2_ci2_n1 + input$c2_ci2_n2 - 2))
    datatable(data.frame(Metric = c("z", "SD-pooled"), Value = c(fmt(z_val), fmt(sd_p))), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_c2_ci2_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$c2_ci2_conf)/2)
    sd_p <- sqrt(((input$c2_ci2_n1-1)*input$c2_ci2_sd1^2 + (input$c2_ci2_n2-1)*input$c2_ci2_sd2^2) / (input$c2_ci2_n1 + input$c2_ci2_n2 - 2))
    calc <- 2 * (z_val * sd_p / input$c2_ci2_err)^2
    df <- data.frame(` ` = c("n 1", "n 2", "Total"), 
                     calculated = c(fmt(calc), fmt(calc), fmt(calc*2)), 
                     final = c(ceiling(calc), ceiling(calc), ceiling(calc)*2), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample Mean Test 1
  output$out_c2_t1_z <- renderDT({
    es <- input$c2_t1_diff / input$c2_t1_sd
    z_a_1 <- qnorm(1 - input$c2_t1_alpha)
    z_a_2 <- qnorm(1 - input$c2_t1_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$c2_t1_power))
    z_b_2 <- qnorm(min(0.999999, input$c2_t1_power + input$c2_t1_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_c2_t1_res <- renderDT({
    es <- input$c2_t1_diff / input$c2_t1_sd
    z_a_1 <- qnorm(1 - input$c2_t1_alpha)
    z_a_2 <- qnorm(1 - input$c2_t1_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$c2_t1_power))
    z_b_2 <- qnorm(min(0.999999, input$c2_t1_power + input$c2_t1_alpha/2))
    n1_1s <- 2 * ((z_a_1 + z_b_1) / es)^2; n1_2s <- 2 * ((z_a_2 + z_b_2) / es)^2
    
    df <- data.frame(
      n = c("1-sided", "", "", "2-sided", "", ""),
      split = c("n 1", "n 2", "Total", "n 1", "n 2", "Total"),
      calculated = c(fmt(n1_1s), fmt(n1_1s), fmt(n1_1s*2), fmt(n1_2s), fmt(n1_2s), fmt(n1_2s*2)),
      final = c(ceiling(n1_1s), ceiling(n1_1s), ceiling(n1_1s)*2, ceiling(n1_2s), ceiling(n1_2s), ceiling(n1_2s)*2)
    )
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample Mean Test 2
  output$out_c2_t2_z <- renderDT({
    sd_p <- sqrt(((input$c2_t2_n1-1)*input$c2_t2_sd1^2 + (input$c2_t2_n2-1)*input$c2_t2_sd2^2) / (input$c2_t2_n1 + input$c2_t2_n2 - 2))
    es <- abs(input$c2_t2_m1 - input$c2_t2_m2) / sd_p
    z_a_1 <- qnorm(1 - input$c2_t2_alpha) 
    z_a_2 <- qnorm(1 - input$c2_t2_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$c2_t2_power))
    z_b_2 <- qnorm(min(0.999999, input$c2_t2_power + input$c2_t2_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)", "SD-pooled"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1), fmt(sd_p)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2), fmt(sd_p)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_c2_t2_res <- renderDT({
    sd_p <- sqrt(((input$c2_t2_n1-1)*input$c2_t2_sd1^2 + (input$c2_t2_n2-1)*input$c2_t2_sd2^2) / (input$c2_t2_n1 + input$c2_t2_n2 - 2))
    es <- abs(input$c2_t2_m1 - input$c2_t2_m2) / sd_p
    z_a_1 <- qnorm(1 - input$c2_t2_alpha) 
    z_a_2 <- qnorm(1 - input$c2_t2_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$c2_t2_power))
    z_b_2 <- qnorm(min(0.999999, input$c2_t2_power + input$c2_t2_alpha/2))
    n1_1s <- 2 * ((z_a_1 + z_b_1) / es)^2; n1_2s <- 2 * ((z_a_2 + z_b_2) / es)^2
    
    df <- data.frame(
      n = c("1-sided", "", "", "2-sided", "", ""),
      split = c("n 1", "n 2", "Total", "n 1", "n 2", "Total"),
      calculated = c(fmt(n1_1s), fmt(n1_1s), fmt(n1_1s*2), fmt(n1_2s), fmt(n1_2s), fmt(n1_2s*2)),
      final = c(ceiling(n1_1s), ceiling(n1_1s), ceiling(n1_1s)*2, ceiling(n1_2s), ceiling(n1_2s), ceiling(n1_2s)*2)
    )
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # ========================================================
  # 2. Dichotomous Outcome
  # ========================================================
  # One Sample CI
  output$out_d1_ci_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$d1_ci_conf)/2)
    datatable(data.frame(Metric = "z", Value = fmt(z_val)), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_d1_ci_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$d1_ci_conf)/2)
    calc <- (z_val / input$d1_ci_err)^2 * input$d1_ci_p * (1 - input$d1_ci_p)
    df <- data.frame(` ` = "n", calculated = fmt(calc), final = ceiling(calc), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # One Sample Test
  output$out_d1_t_z <- renderDT({
    es <- abs(input$d1_t_p1 - input$d1_t_p0) / input$d1_t_sd
    z_a_1 <- qnorm(1 - input$d1_t_alpha)
    z_a_2 <- qnorm(1 - input$d1_t_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$d1_t_power))
    z_b_2 <- qnorm(min(0.999999, input$d1_t_power + input$d1_t_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_d1_t_res <- renderDT({
    es <- abs(input$d1_t_p1 - input$d1_t_p0) / input$d1_t_sd
    z_a_1 <- qnorm(1 - input$d1_t_alpha)
    z_a_2 <- qnorm(1 - input$d1_t_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$d1_t_power))
    z_b_2 <- qnorm(min(0.999999, input$d1_t_power + input$d1_t_alpha/2))
    n_1 <- ((z_a_1 + z_b_1) / es)^2; n_2 <- ((z_a_2 + z_b_2) / es)^2
    df <- data.frame(n = c("1-sided", "2-sided"), calculated = c(fmt(n_1), fmt(n_2)), final = c(ceiling(n_1), ceiling(n_2)))
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample CI 1
  output$out_d2_ci1_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$d2_ci1_conf)/2)
    datatable(data.frame(Metric = "z", Value = fmt(z_val)), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_d2_ci1_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$d2_ci1_conf)/2)
    p1 <- input$d2_ci1_p1; p2 <- input$d2_ci1_p2
    calc <- 2 * (z_val / input$d2_ci1_err)^2 * ((p1*(1-p1) + p2*(1-p2))/2)
    df <- data.frame(` ` = c("n 1", "n 2", "Total"), 
                     calculated = c(fmt(calc), fmt(calc), fmt(calc*2)), 
                     final = c(ceiling(calc), ceiling(calc), ceiling(calc)*2), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample CI 2
  output$out_d2_ci2_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$d2_ci2_conf)/2)
    p1 <- input$d2_ci2_x1 / input$d2_ci2_n1
    p2 <- input$d2_ci2_x2 / input$d2_ci2_n2
    datatable(data.frame(Metric = c("z", "p 1", "p 2"), Value = c(fmt(z_val), fmt(p1), fmt(p2))), options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_d2_ci2_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$d2_ci2_conf)/2)
    p1 <- input$d2_ci2_x1 / input$d2_ci2_n1; p2 <- input$d2_ci2_x2 / input$d2_ci2_n2
    calc <- 2 * (z_val / input$d2_ci2_err)^2 * ((p1*(1-p1) + p2*(1-p2))/2)
    df <- data.frame(` ` = c("n 1", "n 2", "Total"), 
                     calculated = c(fmt(calc), fmt(calc), fmt(calc*2)), 
                     final = c(ceiling(calc), ceiling(calc), ceiling(calc)*2), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample Test 1
  output$out_d2_t1_z <- renderDT({
    es <- input$d2_t1_diff / sqrt(input$d2_t1_meanp * (1 - input$d2_t1_meanp))     
    z_a_1 <- qnorm(1 - input$d2_t1_alpha)
    z_a_2 <- qnorm(1 - input$d2_t1_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$d2_t1_power))
    z_b_2 <- qnorm(min(0.999999, input$d2_t1_power + input$d2_t1_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_d2_t1_res <- renderDT({
    es <- input$d2_t1_diff / sqrt(input$d2_t1_meanp * (1 - input$d2_t1_meanp))     
    z_a_1 <- qnorm(1 - input$d2_t1_alpha)
    z_a_2 <- qnorm(1 - input$d2_t1_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$d2_t1_power))
    z_b_2 <- qnorm(min(0.999999, input$d2_t1_power + input$d2_t1_alpha/2))
    n1_1s <- 2 * ((z_a_1 + z_b_1) / es)^2; n1_2s <- 2 * ((z_a_2 + z_b_2) / es)^2
    
    df <- data.frame(
      n = c("1-sided", "", "", "2-sided", "", ""),
      split = c("n 1", "n 2", "Total", "n 1", "n 2", "Total"),
      calculated = c(fmt(n1_1s), fmt(n1_1s), fmt(n1_1s*2), fmt(n1_2s), fmt(n1_2s), fmt(n1_2s*2)),
      final = c(ceiling(n1_1s), ceiling(n1_1s), ceiling(n1_1s)*2, ceiling(n1_2s), ceiling(n1_2s), ceiling(n1_2s)*2)
    )
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # Two Sample Test 2
  output$out_d2_t2_z <- renderDT({
    p1 <- input$d2_t2_x1 / input$d2_t2_n1; p2 <- input$d2_t2_x2 / input$d2_t2_n2
    p_pool <- (input$d2_t2_x1 + input$d2_t2_x2) / (input$d2_t2_n1 + input$d2_t2_n2)
    es <- abs(p1 - p2) / sqrt(p_pool * (1 - p_pool))
    z_a_1 <- qnorm(1 - input$d2_t2_alpha)
    z_a_2 <- qnorm(1 - input$d2_t2_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$d2_t2_power))
    z_b_2 <- qnorm(min(0.999999, input$d2_t2_power + input$d2_t2_alpha/2))
    df <- data.frame(Metric = c("ES", "z(alpha)", "z(beta)", "p 1", "p 2", "p-pooled"),
                     `1-sided` = c(fmt(es), fmt(z_a_1), fmt(z_b_1), fmt(p1), fmt(p2), fmt(p_pool)),
                     `2-sided` = c(fmt(es), fmt(z_a_2), fmt(z_b_2), fmt(p1), fmt(p2), fmt(p_pool)), check.names = FALSE)
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  output$out_d2_t2_res <- renderDT({
    p1 <- input$d2_t2_x1 / input$d2_t2_n1; p2 <- input$d2_t2_x2 / input$d2_t2_n2
    p_pool <- (input$d2_t2_x1 + input$d2_t2_x2) / (input$d2_t2_n1 + input$d2_t2_n2)
    es <- abs(p1 - p2) / sqrt(p_pool * (1 - p_pool))
    z_a_1 <- qnorm(1 - input$d2_t2_alpha)
    z_a_2 <- qnorm(1 - input$d2_t2_alpha/2)     
    z_b_1 <- qnorm(min(0.999999, input$d2_t2_power))
    z_b_2 <- qnorm(min(0.999999, input$d2_t2_power + input$d2_t2_alpha/2))
    n1_1s <- 2 * ((z_a_1 + z_b_1) / es)^2; n1_2s <- 2 * ((z_a_2 + z_b_2) / es)^2
    
    df <- data.frame(
      n = c("1-sided", "", "", "2-sided", "", ""),
      split = c("n 1", "n 2", "Total", "n 1", "n 2", "Total"),
      calculated = c(fmt(n1_1s), fmt(n1_1s), fmt(n1_1s*2), fmt(n1_2s), fmt(n1_2s), fmt(n1_2s*2)),
      final = c(ceiling(n1_1s), ceiling(n1_1s), ceiling(n1_1s)*2, ceiling(n1_2s), ceiling(n1_2s), ceiling(n1_2s)*2)
    )
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # ========================================================
  # 3. Survey Sample Size
  # ========================================================
  output$out_surv_z <- renderDT({
    z_val <- qnorm(1 - (1 - input$surv_conf)/2)
    samp_sz <- z_val^2 * 0.5 * 0.5 / input$surv_err^2
    df <- data.frame(Metric = c("z", "SampSize"), Value = c(fmt(z_val), fmt(samp_sz)))
    datatable(df, options = dt_opts_nohead, rownames = FALSE)
  })
  output$out_surv_res <- renderDT({
    z_val <- qnorm(1 - (1 - input$surv_conf)/2)
    samp_sz <- z_val^2 * 0.5 * 0.5 / input$surv_err^2
    est <- samp_sz / (1 + samp_sz / input$surv_pop)
    final <- ceiling(est / input$surv_rate)
    with_100 <- ceiling(est)
    df <- data.frame(Metric = c("Estimate", "Final", "With 100% Response Rate"),
                     Value = c(fmt(est), final, with_100))
    datatable(df, options = list(dom='t', ordering=F, columnDefs=list(list(className='dt-center', targets="_all"))), rownames=FALSE)
  })
  
  # ========================================================
  # 4. Thematic Analysis
  # ========================================================
  output$out_ta_s_res <- renderDT({
    k <- input$ta_s_inst
    p <- input$ta_s_prev
    tgt_pwr <- input$ta_s_power
    N <- k
    while (TRUE) {
      if ((1 - pbinom(k - 1, N, p)) >= tgt_pwr) break
      N <- N + 1
    }
    act_pwr <- 1 - pbinom(k - 1, N, p)
    df <- data.frame(Metric = c("Sample Size Needed", "Actual Power"), Value = c(N, fmt(act_pwr)))
    datatable(df, options = dt_opts_nohead, rownames = FALSE)
  })
  
  output$out_ta_p_res <- renderDT({
    k <- input$ta_p_inst
    p <- input$ta_p_prev
    N <- input$ta_p_size
    act_pwr <- 1 - pbinom(k - 1, N, p)
    df <- data.frame(Metric = "Actual Power", Value = fmt(act_pwr))
    datatable(df, options = dt_opts_nohead, rownames = FALSE)
  })
  
  # ========================================================
  # 5. P-Value from CI
  # ========================================================
  output$out_pv_d_res <- renderDT({
    z_ci <- qnorm(1 - (1 - input$pv_d_ci/100)/2)
    se <- (input$pv_d_ucl - input$pv_d_lcl) / (2 * z_ci)
    z_test <- abs(input$pv_d_est) / se
    pval <- 2 * (1 - pnorm(z_test))
    
    df <- data.frame(Metric = c("Reliability Coeff", "SE", "z", "P-value"),
                     Value = c(fmt(z_ci), fmt(se), fmt(z_test), p_fmt(pval)))
    datatable(df, options = dt_opts_nohead, rownames = FALSE)
  })
  
  output$out_pv_r_res <- renderDT({
    est_tr <- log(input$pv_r_est)
    lcl_tr <- log(input$pv_r_lcl)
    ucl_tr <- log(input$pv_r_ucl)
    z_ci <- qnorm(1 - (1 - input$pv_r_ci/100)/2)
    se <- (ucl_tr - lcl_tr) / (2 * z_ci)
    z_test <- abs(est_tr) / se
    pval <- 2 * (1 - pnorm(z_test))
    
    df <- data.frame(Metric = c("Estimate Transform", "LCL Transform", "UCL Transoform", "Reliability Coeff", "SE", "z", "P-value"),
                     Value = c(fmt(est_tr), fmt(lcl_tr), fmt(ucl_tr), fmt(z_ci), fmt(se), fmt(z_test), p_fmt(pval)))
    datatable(df, options = dt_opts_nohead, rownames = FALSE)
  })
  
}

# Run App
shinyApp(ui = ui, server = server)