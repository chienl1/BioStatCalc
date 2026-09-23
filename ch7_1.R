library(shiny)
library(DT)

# ==========================================
# Helper Functions
# ==========================================
fmt <- function(x) ifelse(is.na(x) | is.nan(x) | is.infinite(x), "#DIV/0!", sprintf("%.4f", x))
p_fmt <- function(x) {
  if (is.na(x) || is.nan(x)) return("#DIV/0!")
  ifelse(x < 0.0001, "< 0.0001", sprintf("%.4f", x))
}

get_es_size <- function(val, type="d") {
  if (is.na(val) || is.nan(val) || is.infinite(val)) return("#DIV/0!")
  v <- abs(val)
  if (type %in% c("d", "g", "delta", "h")) {
    if (v >= 0.8) return("Large")
    if (v >= 0.5) return("Moderate")
    return("Small")
  } else {
    if (v >= 0.5) return("Large")
    if (v >= 0.3) return("Moderate")
    return("Small")
  }
}

# ==========================================
# UI Definition
# ==========================================
ui <- fluidPage(
  titlePanel("Hypothesis Testing Calculators"),
  
  tabsetPanel(
    # ------------------------------------------
    # 1. One Sample t-test
    # ------------------------------------------
    tabPanel("1. Single Mean",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample Information"),
                 numericInput("sm_n", "n:", value = 49),
                 numericInput("sm_mean", "Mean:", value = 21.0000),
                 radioButtons("sm_sd_var", "Enter SD(x) or VAR(x):", 
                              choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("sm_val", "Value for SD(x) or VAR(x):", value = 11.0000),
                 hr(),
                 numericInput("sm_mu0", "Test Value:", value = 30.0000),
                 radioButtons("sm_dist", "Distribution:", 
                              choices = c("z (Standard Normal)" = "z", "t Distribution" = "t"), inline = TRUE)
               ),
               mainPanel(
                 h4("Test of 1 Mean"),
                 tableOutput("sm_stat_table"),
                 tableOutput("sm_p_table"),
                 tableOutput("sm_es_table")
               )
             )
    ),
    
    # ------------------------------------------
    # 2. Independent Sample t-test
    # ------------------------------------------
    tabPanel("2. Diff of 2 Means",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("dm_n1", "n1:", value = 194),
                 numericInput("dm_mean1", "Mean 1:", value = 76.8500),
                 radioButtons("dm_sv1", "Enter SD(x1) or VAR(x1):", choices = c("SD(x1)" = "sd", "VAR(x1)" = "var"), inline = TRUE),
                 numericInput("dm_val1", "Value for SD(x1) or VAR(x1):", value = 5.3200),
                 
                 h4("Sample 2"),
                 numericInput("dm_n2", "n2:", value = 152),
                 numericInput("dm_mean2", "Mean 2:", value = 71.2600),
                 radioButtons("dm_sv2", "Enter SD(x2) or VAR(x2):", choices = c("SD(x2)" = "sd", "VAR(x2)" = "var"), inline = TRUE),
                 numericInput("dm_val2", "Value SD(x2) or VAR(x2):", value = 6.1800),
                 
                 hr(),
                 numericInput("dm_mu0", "Test Value (Mean Diff):", value = 0.0000),
                 radioButtons("dm_dist", "Distribution:", choices = c("z (Normal)" = "z", "t Distribution" = "t"), inline = TRUE)
               ),
               mainPanel(
                 h4("Test for the Difference of 2 Means"),
                 uiOutput("dm_main_ui")
               )
             )
    ),
    
    # ------------------------------------------
    # 3. Paired t-test
    # ------------------------------------------
    tabPanel("3. Paired Differences",
             sidebarLayout(
               sidebarPanel(
                 h4("Data Input"),
                 helpText("Enter values separated by commas, spaces, or newlines."),
                 textAreaInput("pt_pair1", "Pair 1:", value = "63, 65, 56, 100, 88, 83, 77, 92, 90, 84, 68, 74, 87, 64, 71, 88", rows = 4),
                 textAreaInput("pt_pair2", "Pair 2:", value = "69, 65, 62, 91, 78, 87, 79, 88, 85, 92, 69, 81, 84, 75, 84, 82", rows = 4),
                 numericInput("pt_mu0", "Test Value:", value = 0.0000)
               ),
               mainPanel(
                 h4("Test of Paired Differences using t"),
                 tableOutput("pt_stat_table"),
                 tableOutput("pt_p_table"),
                 tableOutput("pt_es_table")
               )
             )
    ),
    
    # ------------------------------------------
    # 4. Single Proportion Test
    # ------------------------------------------
    tabPanel("4. Single Proportion",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample Information"),
                 numericInput("sp_x", "x (Optional):", value = NA),
                 numericInput("sp_n", "n:", value = 1460),
                 numericInput("sp_phat", "p(hat):", value = 0.9230, step = 0.01),
                 hr(),
                 numericInput("sp_p0", "Test Value:", value = 0.5000, step = 0.01)
               ),
               mainPanel(
                 h4("Test for Single Proportion using z"),
                 tableOutput("sp_stat_table"),
                 tableOutput("sp_p_table"),
                 tableOutput("sp_es_table")
               )
             )
    ),
    
    # ------------------------------------------
    # 5. Proportional Difference Test
    # ------------------------------------------
    tabPanel("5. Diff of 2 Proportions",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("dp_x1", "x1:", value = 30),
                 numericInput("dp_n1", "n1:", value = 100),
                 h4("Sample 2"),
                 numericInput("dp_x2", "x2:", value = 50),
                 numericInput("dp_n2", "n2:", value = 100),
                 hr(),
                 numericInput("dp_p0", "Test Value (Prop Diff):", value = 0.0000)
               ),
               mainPanel(
                 h4("Test for the Difference of 2 Proportions using z"),
                 tableOutput("dp_stat_table"),
                 tableOutput("dp_p_table"),
                 tableOutput("dp_es_table")
               )
             )
    ),
    
    # ------------------------------------------
    # 6. Single Variance Test
    # ------------------------------------------
    tabPanel("6. Single Variance",
             sidebarLayout(
               sidebarPanel(
                 numericInput("sv_n", "n:", value = 25),
                 radioButtons("sv_sv", "Enter SD(x) or VAR(x):", choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("sv_val", "Value for SD(x) or VAR(x):", value = 3.5000),
                 hr(),
                 radioButtons("sv_test_sv", "Hypothesized Type:", choices = c("Test SD" = "sd", "Test VAR" = "var"), inline = TRUE),
                 numericInput("sv_test_val", "Test Value:", value = 16.0000)
               ),
               mainPanel(
                 h4("Test for Single Variance using χ²"),
                 tableOutput("sv_stat_table"),
                 tableOutput("sv_p_table")
               )
             )
    ),
    
    # ------------------------------------------
    # 7. Variance Ratio Test
    # ------------------------------------------
    tabPanel("7. Ratio of 2 Variances",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("rv_n1", "n1:", value = 24),
                 radioButtons("rv_sv1", "Enter SD(x1) or VAR(x1):", choices = c("SD(x1)" = "sd", "VAR(x1)" = "var"), inline = TRUE),
                 numericInput("rv_val1", "Value for SD(x1) or VAR(x1):", value = 2.6400),
                 h4("Sample 2"),
                 numericInput("rv_n2", "n2:", value = 40),
                 radioButtons("rv_sv2", "Enter SD(x2) or VAR(x2):", choices = c("SD(x2)" = "sd", "VAR(x2)" = "var"), inline = TRUE),
                 numericInput("rv_val2", "Value for SD(x2) or VAR(x2):", value = 1.2700)
               ),
               mainPanel(
                 h4("Test for the Ratio of 2 Variances using F"),
                 tableOutput("rv_stat_table"),
                 tableOutput("rv_p_table")
               )
             )
    )
  )
)

# ==========================================
# Server Logic
# ==========================================
server <- function(input, output, session) {
  
  # ------------------------------------------
  # 1. Single Mean
  # ------------------------------------------
  output$sm_stat_table <- renderTable({
    v <- if(input$sm_sd_var == "sd") input$sm_val^2 else input$sm_val
    s <- sqrt(v)
    se <- s / sqrt(input$sm_n)
    stat <- (input$sm_mean - input$sm_mu0) / se
    
    if (input$sm_dist == "z") {
      data.frame(Metric = c("Test Value", "z"), Value = fmt(c(input$sm_mu0, stat)))
    } else {
      data.frame(Metric = c("Test Value", "df", "t"), Value = fmt(c(input$sm_mu0, input$sm_n - 1, stat)))
    }
  })
  
  output$sm_p_table <- renderTable({
    v <- if(input$sm_sd_var == "sd") input$sm_val^2 else input$sm_val
    se <- sqrt(v) / sqrt(input$sm_n)
    stat <- (input$sm_mean - input$sm_mu0) / se
    
    if (input$sm_dist == "z") {
      p_lt <- pnorm(stat)
      p_gt <- 1 - pnorm(stat)
      p_neq <- 2 * pnorm(-abs(stat))
    } else {
      df <- input$sm_n - 1
      p_lt <- pt(stat, df)
      p_gt <- 1 - pt(stat, df)
      p_neq <- 2 * pt(-abs(stat), df)
    }
    
    data.frame(
      `H(a)` = c("\"≠\"", "\"<\"", "\">\""),
      Prob. = fmt(c(p_neq, p_lt, p_gt)),
      `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)),
      check.names = FALSE
    )
  })
  
  output$sm_es_table <- renderTable({
    s <- if(input$sm_sd_var == "sd") input$sm_val else sqrt(input$sm_val)
    d <- abs(input$sm_mean - input$sm_mu0) / s
    data.frame(ES = "Cohen's d", Value = fmt(d), Size = get_es_size(d, "d"))
  })
  
  # ------------------------------------------
  # 2. Difference of 2 Means
  # ------------------------------------------
  dm_calcs <- reactive({
    n1 <- input$dm_n1; m1 <- input$dm_mean1; v1 <- if(input$dm_sv1 == "sd") input$dm_val1^2 else input$dm_val1
    n2 <- input$dm_n2; m2 <- input$dm_mean2; v2 <- if(input$dm_sv2 == "sd") input$dm_val2^2 else input$dm_val2
    mu0 <- input$dm_mu0
    m_diff <- m1 - m2
    
    # z / Equal t
    df_eq <- n1 + n2 - 2
    sp <- sqrt(((n1-1)*v1 + (n2-1)*v2) / df_eq)
    se_eq <- sp * sqrt(1/n1 + 1/n2)
    stat_eq <- (m_diff - mu0) / se_eq
    
    # Unequal t (Welch & Cochran)
    w1 <- v1/n1; w2 <- v2/n2
    se_uneq <- sqrt(w1 + w2)
    df1 <- n1 - 1; df2 <- n2 - 1
    t1 <- qt(0.975, df1); t2 <- qt(0.975, df2)
    t_pool <- (w1*t1 + w2*t2) / (w1 + w2)
    df_satter <- (w1 + w2)^2 / ((w1^2)/df1 + (w2^2)/df2)
    t_calc <- (m_diff - mu0) / se_uneq
    
    # Effect sizes
    d <- abs(m_diff) / sp
    g_delta <- abs(m_diff) / sqrt(v1)
    h_g <- d * (1 - 3/(4*(n1+n2)-9))
    
    list(m_diff=m_diff, sp=sp, stat_eq=stat_eq, df_eq=df_eq,
         t1=t1, t2=t2, t_pool=t_pool, df_satter=df_satter, t_calc=t_calc,
         d=d, g_delta=g_delta, h_g=h_g)
  })
  
  output$dm_main_ui <- renderUI({
    if (input$dm_dist == "z") {
      tagList(tableOutput("dm_z_stat"), tableOutput("dm_z_p"), tableOutput("dm_es"))
    } else {
      tagList(
        h5(strong("Population Variances Equal")),
        tableOutput("dm_t_eq_stat"), tableOutput("dm_t_eq_p"), tableOutput("dm_es"),
        hr(),
        h5(strong("Population Variances Not Equal")),
        tableOutput("dm_t_uneq_stat"), tableOutput("dm_t_uneq_p"), tableOutput("dm_es")
      )
    }
  })
  
  output$dm_z_stat <- renderTable({
    d <- dm_calcs()
    data.frame(Metric = c("Test Value", "Mean Diff", "SD(pool)", "z"), Value = fmt(c(input$dm_mu0, d$m_diff, d$sp, d$stat_eq)))
  })
  output$dm_z_p <- renderTable({
    d <- dm_calcs()
    p_lt <- pnorm(d$stat_eq); p_gt <- 1 - pnorm(d$stat_eq); p_neq <- 2 * pnorm(-abs(d$stat_eq))
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), Prob. = fmt(c(p_neq, p_lt, p_gt)), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
  
  output$dm_t_eq_stat <- renderTable({
    d <- dm_calcs()
    data.frame(Metric = c("Test Value", "Mean Diff", "df", "SD(pool)", "t"), Value = fmt(c(input$dm_mu0, d$m_diff, d$df_eq, d$sp, d$stat_eq)))
  })
  output$dm_t_eq_p <- renderTable({
    d <- dm_calcs()
    p_lt <- pt(d$stat_eq, d$df_eq); p_gt <- 1 - pt(d$stat_eq, d$df_eq); p_neq <- 2 * pt(-abs(d$stat_eq), d$df_eq)
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), Prob. = fmt(c(p_neq, p_lt, p_gt)), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
  
  output$dm_t_uneq_stat <- renderTable({
    d <- dm_calcs()
    data.frame(
      Metric = c("Test Value", "Mean Diff", "df(1)", "df(2)", "t(1)", "t(pool)", "t(2)", "df(satter)", "t'(calc)"),
      Value = fmt(c(input$dm_mu0, d$m_diff, input$dm_n1-1, input$dm_n2-1, d$t1, d$t_pool, d$t2, d$df_satter, d$t_calc))
    )
  })
  output$dm_t_uneq_p <- renderTable({
    d <- dm_calcs()
    # Using Satterthwaite df for p-values for both as typically requested in standard approximations
    p_lt <- pt(d$t_calc, d$df_satter); p_gt <- 1 - pt(d$t_calc, d$df_satter); p_neq <- 2 * pt(-abs(d$t_calc), d$df_satter)
    data.frame(
      `H(a)` = c("\"≠\"", "\"<\"", "\">\""),
      `Prob. (Cochran)` = fmt(c(p_neq, p_lt, p_gt)), `Prob. (Welch)` = fmt(c(p_neq, p_lt, p_gt)),
      `p-value (Cochran)` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), `p-value (Welch)` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)),
      check.names = FALSE
    )
  })
  output$dm_es <- renderTable({
    d <- dm_calcs()
    data.frame(ES = c("Cohen's d", "Glass' Δ", "Hedges' g"), Value = fmt(c(d$d, d$g_delta, d$h_g)), Size = c(get_es_size(d$d,"d"), get_es_size(d$g_delta,"d"), get_es_size(d$h_g,"d")))
  })
  
  # ------------------------------------------
  # 3. Paired t-test
  # ------------------------------------------
  pt_data <- reactive({
    p1 <- as.numeric(unlist(strsplit(input$pt_pair1, "[,\\s\\n]+")))
    p2 <- as.numeric(unlist(strsplit(input$pt_pair2, "[,\\s\\n]+")))
    p1 <- p1[!is.na(p1)]; p2 <- p2[!is.na(p2)]
    req(length(p1) == length(p2), length(p1) > 1)
    
    diffs <- p1 - p2
    n <- length(diffs)
    m <- mean(diffs)
    s <- sd(diffs)
    v <- var(diffs)
    t_val <- (m - input$pt_mu0) / (s / sqrt(n))
    d <- abs(m - input$pt_mu0) / s
    
    list(n=n, m=m, s=s, v=v, t=t_val, df=n-1, d=d)
  })
  
  output$pt_stat_table <- renderTable({
    d <- pt_data()
    data.frame(Metric = c("n", "Mean", "SD(x)", "VAR(x)", "Test Value", "df", "t"), Value = fmt(c(d$n, d$m, d$s, d$v, input$pt_mu0, d$df, d$t)))
  })
  output$pt_p_table <- renderTable({
    d <- pt_data()
    p_lt <- pt(d$t, d$df); p_gt <- 1 - pt(d$t, d$df); p_neq <- 2 * pt(-abs(d$t), d$df)
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), Prob. = fmt(c(p_neq, p_lt, p_gt)), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
  output$pt_es_table <- renderTable({
    d <- pt_data()
    data.frame(ES = "Cohen's d", Value = fmt(d$d), Size = get_es_size(d$d, "d"))
  })
  
  # ------------------------------------------
  # 4. Single Proportion
  # ------------------------------------------
  observeEvent(input$sp_x, { if(!is.na(input$sp_x) && input$sp_n > 0) updateNumericInput(session, "sp_phat", value = input$sp_x / input$sp_n) })
  
  output$sp_stat_table <- renderTable({
    se <- sqrt(input$sp_p0 * (1 - input$sp_p0) / input$sp_n)
    z <- (input$sp_phat - input$sp_p0) / se
    data.frame(Metric = c("Test Value", "p(hat)", "z"), Value = fmt(c(input$sp_p0, input$sp_phat, z)))
  })
  output$sp_p_table <- renderTable({
    se <- sqrt(input$sp_p0 * (1 - input$sp_p0) / input$sp_n)
    z <- (input$sp_phat - input$sp_p0) / se
    p_lt <- pnorm(z); p_gt <- 1 - pnorm(z); p_neq <- 2 * pnorm(-abs(z))
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), Prob. = fmt(c(p_neq, p_lt, p_gt)), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
  output$sp_es_table <- renderTable({
    h <- 2 * asin(sqrt(input$sp_phat)) - 2 * asin(sqrt(input$sp_p0))
    data.frame(ES = c("Cohen's h", "r"), Value = c(fmt(h), "#DIV/0!"), Size = c(get_es_size(h, "h"), "#DIV/0!"))
  })
  
  # ------------------------------------------
  # 5. Difference of 2 Proportions
  # ------------------------------------------
  dp_calcs <- reactive({
    p1 <- input$dp_x1 / input$dp_n1; p2 <- input$dp_x2 / input$dp_n2
    p_bar <- (input$dp_x1 + input$dp_x2) / (input$dp_n1 + input$dp_n2)
    p_diff <- p1 - p2
    se <- sqrt(p_bar * (1 - p_bar) * (1/input$dp_n1 + 1/input$dp_n2))
    z <- (p_diff - input$dp_p0) / se
    
    h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
    r <- z / sqrt(input$dp_n1 + input$dp_n2)
    odds1 <- (p1)/(1-p1); odds2 <- (p2)/(1-p2)
    OR <- odds1 / odds2
    yules_q <- (OR - 1) / (OR + 1)
    
    list(p1=p1, p2=p2, p_bar=p_bar, p_diff=p_diff, z=z, h=h, r=r, q=yules_q)
  })
  
  output$dp_stat_table <- renderTable({
    d <- dp_calcs()
    data.frame(Metric = c("Test Value", "Prop. Diff", "p(bar)", "z"), Value = fmt(c(input$dp_p0, d$p_diff, d$p_bar, d$z)))
  })
  output$dp_p_table <- renderTable({
    d <- dp_calcs()
    p_lt <- pnorm(d$z); p_gt <- 1 - pnorm(d$z); p_neq <- 2 * pnorm(-abs(d$z))
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), Prob. = fmt(c(p_neq, p_lt, p_gt)), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
  output$dp_es_table <- renderTable({
    d <- dp_calcs()
    data.frame(ES = c("Cohen's h", "r", "Yule's Q"), Value = fmt(c(d$h, d$r, d$q)), Size = c(get_es_size(d$h,"h"), get_es_size(d$r,"r"), get_es_size(d$q,"r")))
  })
  
  # ------------------------------------------
  # 6. Single Variance
  # ------------------------------------------
  output$sv_stat_table <- renderTable({
    v <- if(input$sv_sv == "sd") input$sv_val^2 else input$sv_val
    v_test <- if(input$sv_test_sv == "sd") input$sv_test_val^2 else input$sv_test_val
    chi2 <- (input$sv_n - 1) * v / v_test
    data.frame(Metric = c(if(input$sv_test_sv == "sd") "Test SD" else "Test VAR", "Test Value", "χ²"), Value = fmt(c(sqrt(v_test), v_test, chi2)))
  })
  output$sv_p_table <- renderTable({
    v <- if(input$sv_sv == "sd") input$sv_val^2 else input$sv_val
    v_test <- if(input$sv_test_sv == "sd") input$sv_test_val^2 else input$sv_test_val
    df <- input$sv_n - 1
    chi2 <- df * v / v_test
    
    p_lt <- pchisq(chi2, df)
    p_gt <- 1 - pchisq(chi2, df)
    p_neq <- 2 * min(p_lt, p_gt)
    
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
  
  # ------------------------------------------
  # 7. Variance Ratio
  # ------------------------------------------
  output$rv_stat_table <- renderTable({
    v1 <- if(input$rv_sv1 == "sd") input$rv_val1^2 else input$rv_val1
    v2 <- if(input$rv_sv2 == "sd") input$rv_val2^2 else input$rv_val2
    f_val <- v1 / v2
    data.frame(Metric = c("F"), Value = fmt(f_val))
  })
  output$rv_p_table <- renderTable({
    v1 <- if(input$rv_sv1 == "sd") input$rv_val1^2 else input$rv_val1
    v2 <- if(input$rv_sv2 == "sd") input$rv_val2^2 else input$rv_val2
    f_val <- v1 / v2
    df1 <- input$rv_n1 - 1; df2 <- input$rv_n2 - 1
    
    p_lt <- pf(f_val, df1, df2)
    p_gt <- 1 - pf(f_val, df1, df2)
    p_neq <- 2 * min(p_lt, p_gt)
    
    data.frame(`H(a)` = c("\"≠\"", "\"<\"", "\">\""), F = fmt(rep(f_val, 3)), `p-value` = c(p_fmt(p_neq), p_fmt(p_lt), p_fmt(p_gt)), check.names = FALSE)
  })
}

shinyApp(ui = ui, server = server)