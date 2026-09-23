library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Confidence Interval Calculators"),
  
  tabsetPanel(
    # ==========================================
    # Tab 1: Single Mean
    # ==========================================
    tabPanel("Single Mean",
             br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput("sm_n", "n:", value = 16),
                 numericInput("sm_mean", "Mean:", value = 1747.6000),
                 radioButtons("sm_sd_var", "Enter SD(x) or VAR(x):", 
                              choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("sm_val", "Value for SD(x) or VAR(x):", value = 350.0000),
                 numericInput("sm_ci", "CI %:", value = 95.0000),
                 radioButtons("sm_dist", "Distribution:", 
                              choices = c("z (Standard Normal)" = "z", "t Distribution" = "t"), inline = TRUE)
               ),
               mainPanel(
                 h4("Confidence Interval Results"),
                 tableOutput("sm_result_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 2: Difference of 2 Means
    # ==========================================
    tabPanel("Difference of 2 Means",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("d2m_n1", "n1:", value = 12),
                 numericInput("d2m_mean1", "Mean1:", value = 4.5000),
                 radioButtons("d2m_sd_var1", "Enter SD(x1) or VAR(x1):", 
                              choices = c("SD(x1)" = "sd", "VAR(x1)" = "var"), inline = TRUE),
                 numericInput("d2m_val1", "Value for SD(x1) or VAR(x1):", value = 1.0000),
                 
                 h4("Sample 2"),
                 numericInput("d2m_n2", "n2:", value = 15),
                 numericInput("d2m_mean2", "Mean2:", value = 3.4000),
                 radioButtons("d2m_sd_var2", "Enter SD(x2) or VAR(x2):", 
                              choices = c("SD(x2)" = "sd", "VAR(x2)" = "var"), inline = TRUE),
                 numericInput("d2m_val2", "Value for SD(x2) or VAR(x2):", value = 1.5000),
                 
                 hr(),
                 numericInput("d2m_ci", "CI %:", value = 95.0000),
                 radioButtons("d2m_dist", "Distribution:", 
                              choices = c("z (Standard Normal)" = "z", "t Distribution" = "t"), inline = TRUE)
               ),
               mainPanel(
                 h4("Results"),
                 uiOutput("d2m_output_ui")
               )
             )
    ),
    
    # ==========================================
    # Tab 3: Single Proportion
    # ==========================================
    tabPanel("Single Proportion",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Basic CI (using z)"),
                 numericInput("sp_x", "x (Optional):", value = NA),
                 numericInput("sp_n", "n:", value = 1460),
                 numericInput("sp_phat", "p(hat):", value = 0.9230, step = 0.001),
                 numericInput("sp_ci", "CI %:", value = 95.0000),
                 hr(),
                 h4("Additional Methods"),
                 numericInput("sp2_x", "x:", value = 14),
                 numericInput("sp2_n", "n:", value = 27),
                 numericInput("sp2_ci", "CI %:", value = 95.0000)
               ),
               mainPanel(
                 h4("CI for Single Proportion using z"),
                 tableOutput("sp_result_table"),
                 br(),
                 h4("CI for Single Proportion Additional Methods"),
                 tableOutput("sp2_result_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 4: Difference of 2 Proportions
    # ==========================================
    tabPanel("Difference of 2 Proportions",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("d2p_x1", "x1:", value = 114),
                 numericInput("d2p_n1", "n1:", value = 637),
                 
                 h4("Sample 2"),
                 numericInput("d2p_x2", "x2:", value = 57),
                 numericInput("d2p_n2", "n2:", value = 510),
                 
                 hr(),
                 numericInput("d2p_ci", "CI %:", value = 95.0000)
               ),
               mainPanel(
                 h4("Confidence Interval using z"),
                 tableOutput("d2p_result_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 5: Single Variance
    # ==========================================
    tabPanel("Single Variance",
             br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput("sv_n", "n:", value = 30),
                 radioButtons("sv_sd_var", "Enter SD(x) or VAR(x):", 
                              choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("sv_val", "Value for SD(x) or VAR(x):", value = 1.4663),
                 numericInput("sv_ci", "CI %:", value = 95.0000)
               ),
               mainPanel(
                 h4("Confidence Interval using χ²"),
                 tableOutput("sv_chi_table"),
                 tableOutput("sv_result_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 6: Ratio of 2 Variances
    # ==========================================
    tabPanel("Ratio of 2 Variances",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("r2v_n1", "n1:", value = 18),
                 radioButtons("r2v_sd_var1", "Enter SD(x1) or VAR(x1):", 
                              choices = c("SD(x1)" = "sd", "VAR(x1)" = "var"), inline = TRUE),
                 numericInput("r2v_val1", "Value for SD(x1) or VAR(x1):", value = 2.1000),
                 
                 h4("Sample 2"),
                 numericInput("r2v_n2", "n2:", value = 10),
                 radioButtons("r2v_sd_var2", "Enter SD(x2) or VAR(x2):", 
                              choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("r2v_val2", "Value for SD(x2) or VAR(x2):", value = 11.5000),
                 
                 hr(),
                 numericInput("r2v_ci", "CI %:", value = 99.0000)
               ),
               mainPanel(
                 h4("Confidence Interval using F"),
                 tableOutput("r2v_f_table"),
                 tableOutput("r2v_result_table")
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Helper formatter for 4 decimal places
  fmt <- function(x) ifelse(is.na(x), "n/a", sprintf("%.4f", x))
  
  # Helper to resolve SD or VAR
  get_var <- function(choice, val) {
    if(choice == "sd") val^2 else val
  }
  
  # ==========================================
  # Single Mean
  # ==========================================
  output$sm_result_table <- renderTable({
    var_x <- get_var(input$sm_sd_var, input$sm_val)
    se <- sqrt(var_x / input$sm_n)
    alpha <- 1 - input$sm_ci / 100
    
    if (input$sm_dist == "z") {
      crit <- qnorm(1 - alpha/2)
      err <- crit * se
      data.frame(
        Metric = c("Mean", "SD(x)", "VAR(x)", "LCL", "UCL"),
        Value = fmt(c(input$sm_mean, sqrt(var_x), var_x, input$sm_mean - err, input$sm_mean + err))
      )
    } else {
      df <- input$sm_n - 1
      crit <- qt(1 - alpha/2, df)
      err <- crit * se
      data.frame(
        Metric = c("Mean", "SD(x)", "VAR(x)", "df", "LCL", "UCL"),
        Value = fmt(c(input$sm_mean, sqrt(var_x), var_x, df, input$sm_mean - err, input$sm_mean + err))
      )
    }
  })
  
  # ==========================================
  # Difference of 2 Means
  # ==========================================
  output$d2m_output_ui <- renderUI({
    if (input$d2m_dist == "z") {
      tableOutput("d2m_z_table")
    } else {
      tagList(
        h5(strong("Population Variances Equal")),
        tableOutput("d2m_t_eq_table"),
        hr(),
        h5(strong("Population Variances Not Equal")),
        tableOutput("d2m_t_neq_details"),
        tableOutput("d2m_t_neq_table")
      )
    }
  })
  
  output$d2m_z_table <- renderTable({
    v1 <- get_var(input$d2m_sd_var1, input$d2m_val1)
    v2 <- get_var(input$d2m_sd_var2, input$d2m_val2)
    mean_diff <- input$d2m_mean1 - input$d2m_mean2
    se <- sqrt(v1/input$d2m_n1 + v2/input$d2m_n2)
    alpha <- 1 - input$d2m_ci / 100
    crit <- qnorm(1 - alpha/2)
    err <- crit * se
    
    data.frame(
      Metric = c("Mean Diff", "SD(pool)", "LCL", "UCL"),
      Value = fmt(c(mean_diff, se, mean_diff - err, mean_diff + err))
    )
  })
  
  output$d2m_t_eq_table <- renderTable({
    v1 <- get_var(input$d2m_sd_var1, input$d2m_val1)
    v2 <- get_var(input$d2m_sd_var2, input$d2m_val2)
    mean_diff <- input$d2m_mean1 - input$d2m_mean2
    df <- input$d2m_n1 + input$d2m_n2 - 2
    var_pool <- ((input$d2m_n1 - 1)*v1 + (input$d2m_n2 - 1)*v2) / df
    se <- sqrt(var_pool * (1/input$d2m_n1 + 1/input$d2m_n2))
    alpha <- 1 - input$d2m_ci / 100
    crit <- qt(1 - alpha/2, df)
    err <- crit * se
    
    data.frame(
      Metric = c("Mean Diff", "df", "SD(pool)", "LCL", "UCL"),
      Value = fmt(c(mean_diff, df, se, mean_diff - err, mean_diff + err))
    )
  })
  
  d2m_t_neq_data <- reactive({
    v1 <- get_var(input$d2m_sd_var1, input$d2m_val1)
    v2 <- get_var(input$d2m_sd_var2, input$d2m_val2)
    n1 <- input$d2m_n1
    n2 <- input$d2m_n2
    mean_diff <- input$d2m_mean1 - input$d2m_mean2
    alpha <- 1 - input$d2m_ci / 100
    
    w1 <- v1 / n1
    w2 <- v2 / n2
    se <- sqrt(w1 + w2)
    
    df1 <- n1 - 1
    df2 <- n2 - 1
    t1 <- qt(1 - alpha/2, df1)
    t2 <- qt(1 - alpha/2, df2)
    
    t_pool <- (w1*t1 + w2*t2) / (w1 + w2) 
    err_cochran <- t_pool * se
    
    # 針對 Welch 的 df 做無條件捨去，以完全符合 Excel 的截圖計算結果
    df_welch_exact <- (w1 + w2)^2 / ( (w1^2)/df1 + (w2^2)/df2 )
    df_welch <- floor(df_welch_exact)  
    
    t_welch <- qt(1 - alpha/2, df_welch)
    err_welch <- t_welch * se
    
    list(mean_diff = mean_diff, df1 = df1, df2 = df2, t1 = t1, t2 = t2, 
         t_pool = t_pool, df_welch_exact = df_welch_exact, t_welch = t_welch, 
         err_cochran = err_cochran, err_welch = err_welch)
  })
  
  output$d2m_t_neq_details <- renderTable({
    d <- d2m_t_neq_data()
    data.frame(
      Metric = c("Mean Diff", "df(1)", "df(2)", "t(1)", "t(2)", "t(pool)", 
                 "df(Welch)", "t(Welch)", "Error(Cochran)", "Error(Welch)"),
      Value = fmt(c(d$mean_diff, d$df1, d$df2, d$t1, d$t2, d$t_pool, 
                    d$df_welch_exact, d$t_welch, d$err_cochran, d$err_welch))
    )
  })
  
  output$d2m_t_neq_table <- renderTable({
    d <- d2m_t_neq_data()
    data.frame(
      Correction = c("Cochran", "Welch"),
      LCL = fmt(c(d$mean_diff - d$err_cochran, d$mean_diff - d$err_welch)),
      UCL = fmt(c(d$mean_diff + d$err_cochran, d$mean_diff + d$err_welch))
    )
  })
  
  # ==========================================
  # Single Proportion
  # ==========================================
  # Update p(hat) automatically if x is provided
  observeEvent(input$sp_x, {
    if(!is.na(input$sp_x) && !is.na(input$sp_n) && input$sp_n > 0) {
      updateNumericInput(session, "sp_phat", value = input$sp_x / input$sp_n)
    }
  })
  
  output$sp_result_table <- renderTable({
    p <- input$sp_phat
    n <- input$sp_n
    alpha <- 1 - input$sp_ci / 100
    se <- sqrt(p * (1 - p) / n)
    crit <- qnorm(1 - alpha/2)
    err <- crit * se
    
    data.frame(
      Metric = c("LCL", "UCL"),
      Value = fmt(c(p - err, p + err))
    )
  })
  
  output$sp2_result_table <- renderTable({
    x <- input$sp2_x
    n <- input$sp2_n
    alpha <- 1 - input$sp2_ci / 100
    z <- qnorm(1 - alpha/2)
    p <- x / n
    
    # 1. Wald
    err_w <- z * sqrt(p*(1-p)/n)
    w_L <- p - err_w
    w_U <- p + err_w
    
    # 2. Wilson
    denom <- 1 + z^2/n
    center <- (p + z^2/(2*n)) / denom
    err_wilson <- (z / denom) * sqrt(p*(1-p)/n + z^2/(4*n^2))
    wilson_L <- center - err_wilson
    wilson_U <- center + err_wilson
    
    # 3. Wilson CC
    pl <- max(0, p - 1/(2*n))
    pu <- min(1, p + 1/(2*n))
    wilson_cc_L <- (pl + z^2/(2*n) - z*sqrt(pl*(1-pl)/n + z^2/(4*n^2))) / (1 + z^2/n)
    wilson_cc_U <- (pu + z^2/(2*n) + z*sqrt(pu*(1-pu)/n + z^2/(4*n^2))) / (1 + z^2/n)
    if(x == 0) wilson_cc_L <- 0
    if(x == n) wilson_cc_U <- 1
    
    # 4. Agresti-Coull
    n_tilde <- n + z^2
    p_tilde <- (x + z^2/2) / n_tilde
    err_ac <- z * sqrt(p_tilde*(1-p_tilde)/n_tilde)
    ac_L <- p_tilde - err_ac
    ac_U <- p_tilde + err_ac
    
    # 5. Exact (Clopper-Pearson)
    ex_L <- if(x == 0) 0 else qbeta(alpha/2, x, n - x + 1)
    ex_U <- if(x == n) 1 else qbeta(1 - alpha/2, x + 1, n - x)
    
    # 6. Jeffries
    jf_L <- if(x == 0) 0 else qbeta(alpha/2, x + 0.5, n - x + 0.5)
    jf_U <- if(x == n) 1 else qbeta(1 - alpha/2, x + 0.5, n - x + 0.5)
    
    # 7. Arcsine UC
    asin_L <- sin(max(0, asin(sqrt(p)) - z/(2*sqrt(n))))^2
    asin_U <- sin(min(pi/2, asin(sqrt(p)) + z/(2*sqrt(n))))^2
    
    # 8 & 9. Rules of Thumb
    rot0_L <- if(p == 0) 0 else NA
    rot0_U <- if(p == 0) 3/n else NA
    rot1_L <- if(p == 1) 1 - 3/n else NA
    rot1_U <- if(p == 1) 1 else NA
    
    data.frame(
      Type = c("Wald (Normal)", "Wilson", "Wilson CC", "Agresti-Coull", "Exact", 
               "Jeffries", "Arcsine UC", "RoT (IF p=0)", "RoT (IF p=1)"),
      LCL = fmt(c(w_L, wilson_L, wilson_cc_L, ac_L, ex_L, jf_L, asin_L, rot0_L, rot1_L)),
      UCL = fmt(c(w_U, wilson_U, wilson_cc_U, ac_U, ex_U, jf_U, asin_U, rot0_U, rot1_U))
    )
  })
  
  # ==========================================
  # Difference of 2 Proportions
  # ==========================================
  output$d2p_result_table <- renderTable({
    p1 <- input$d2p_x1 / input$d2p_n1
    p2 <- input$d2p_x2 / input$d2p_n2
    p_diff <- p1 - p2
    se <- sqrt(p1*(1-p1)/input$d2p_n1 + p2*(1-p2)/input$d2p_n2)
    alpha <- 1 - input$d2p_ci / 100
    crit <- qnorm(1 - alpha/2)
    err <- crit * se
    
    data.frame(
      Metric = c("p(hat1)", "p(hat2)", "Prop. Diff", "SD(pool)", "LCL", "UCL"),
      Value = fmt(c(p1, p2, p_diff, se, p_diff - err, p_diff + err))
    )
  })
  
  # ==========================================
  # Single Variance
  # ==========================================
  output$sv_chi_table <- renderTable({
    alpha <- 1 - input$sv_ci / 100
    df <- input$sv_n - 1
    chi_lower <- qchisq(1 - alpha/2, df) 
    chi_upper <- qchisq(alpha/2, df)     
    
    data.frame(
      Metric = c("χ² - Lower", "χ² - Upper"),
      Value = fmt(c(chi_lower, chi_upper))
    )
  })
  
  output$sv_result_table <- renderTable({
    var_x <- get_var(input$sv_sd_var, input$sv_val)
    alpha <- 1 - input$sv_ci / 100
    df <- input$sv_n - 1
    chi_lower <- qchisq(1 - alpha/2, df)
    chi_upper <- qchisq(alpha/2, df)
    
    lcl_var <- (df * var_x) / chi_lower
    ucl_var <- (df * var_x) / chi_upper
    
    data.frame(
      Type = c("SD", "VAR"),
      LCL = fmt(c(sqrt(lcl_var), lcl_var)),
      UCL = fmt(c(sqrt(ucl_var), ucl_var))
    )
  })
  
  # ==========================================
  # Ratio of 2 Variances
  # ==========================================
  output$r2v_f_table <- renderTable({
    v1 <- get_var(input$r2v_sd_var1, input$r2v_val1)
    v2 <- get_var(input$r2v_sd_var2, input$r2v_val2)
    df1 <- input$r2v_n1 - 1
    df2 <- input$r2v_n2 - 1
    alpha <- 1 - input$r2v_ci / 100
    
    f_lower <- qf(1 - alpha/2, df1, df2) 
    f_upper <- qf(alpha/2, df1, df2)     
    
    data.frame(
      Metric = c("VAR(1)/VAR(2)", "VAR(2)/VAR(1)", "F - Lower", "F - Upper"),
      Value = fmt(c(v1/v2, v2/v1, f_lower, f_upper))
    )
  })
  
  output$r2v_result_table <- renderTable({
    v1 <- get_var(input$r2v_sd_var1, input$r2v_val1)
    v2 <- get_var(input$r2v_sd_var2, input$r2v_val2)
    ratio1 <- v1 / v2
    ratio2 <- v2 / v1
    
    df1 <- input$r2v_n1 - 1
    df2 <- input$r2v_n2 - 1
    alpha <- 1 - input$r2v_ci / 100
    
    f_lower <- qf(1 - alpha/2, df1, df2)
    f_upper <- qf(alpha/2, df1, df2)
    
    data.frame(
      Ratio = c("VAR(1)/VAR(2)", "VAR(2)/VAR(1)"),
      LCL = fmt(c(ratio1 / f_lower, ratio2 / f_lower)),
      UCL = fmt(c(ratio1 / f_upper, ratio2 / f_upper))
    )
  })
}

shinyApp(ui = ui, server = server)