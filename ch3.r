library(shiny)
library(DT)

# Define User Interface
ui <- fluidPage(
  titlePanel("Epidemiology & Screening Calculators"),
  
  tabsetPanel(
    # ==========================================
    # 1. Screening Test Statistics
    # ==========================================
    tabPanel("1. Screening Test",
             sidebarLayout(
               sidebarPanel(
                 h4("Disease Input"),
                 numericInput("st_a", "Present (D+) / Positive (T+) [True Positive]:", value = 5, min = 0),
                 numericInput("st_c", "Present (D+) / Negative (T-) [False Negative]:", value = 11, min = 0),
                 numericInput("st_b", "Absent (D-) / Positive (T+) [False Positive]:", value = 1, min = 0),
                 numericInput("st_d", "Absent (D-) / Negative (T-) [True Negative]:", value = 2, min = 0),
                 hr(),
                 numericInput("st_rate", "Rate in Population (%) (Note: This value will use Bayes' formula)", 
                              value = NA, min = 0, max = 100, step = 0.01)
               ),
               mainPanel(
                 h4("Screening Test Statistics"),
                 DTOutput("st_table_main"),
                 br(),
                 DTOutput("st_table_others")
               )
             )
    ),
    
    # ==========================================
    # 2. Risk Epidemiology Calculators
    # ==========================================
    tabPanel("2. Risk Epi",
             sidebarLayout(
               sidebarPanel(
                 h4("Outcome Input"),
                 numericInput("re_a", "Treatment / Present:", value = 22, min = 0),
                 numericInput("re_b", "Treatment / Absent:", value = 216, min = 0),
                 numericInput("re_c", "Control / Present:", value = 18, min = 0),
                 numericInput("re_d", "Control / Absent:", value = 199, min = 0),
                 hr(),
                 numericInput("re_ci", "CI %:", value = 95, min = 0, max = 99.99, step = 1)
               ),
               mainPanel(
                 h4("Risk Epidemiology Results"),
                 DTOutput("re_table_main"),
                 br(),
                 h4("Relative Risk CI"),
                 DTOutput("re_table_ci"),
                 br(),
                 DTOutput("re_table_z")
               )
             )
    ),
    
    # ==========================================
    # 3. Odds Epidemiology Calculators
    # ==========================================
    tabPanel("3. Odds Epi",
             sidebarLayout(
               sidebarPanel(
                 h4("Group Input"),
                 numericInput("oe_a", "Group 1 / Yes:", value = 29.2, min = 0),
                 numericInput("oe_b", "Group 1 / No:", value = 70.8, min = 0),
                 numericInput("oe_c", "Group 2 / Yes:", value = 41.3, min = 0),
                 numericInput("oe_d", "Group 2 / No:", value = 58.7, min = 0),
                 hr(),
                 numericInput("oe_ci", "CI %:", value = 95, min = 0, max = 99.99, step = 1)
               ),
               mainPanel(
                 h4("Odds Epidemiology Results"),
                 DTOutput("oe_table_main"),
                 br(),
                 h4("Odds Ratio CI"),
                 DTOutput("oe_table_ci"),
                 br(),
                 DTOutput("oe_table_z")
               )
             )
    ),
    
    # ==========================================
    # 4. Odds Ratio P-Value Calculator
    # ==========================================
    tabPanel("4. OR P-Value",
             fluidRow(
               column(6,
                      h4("Calculator 1: Given CI, LCL, UCL"),
                      wellPanel(
                        numericInput("pv1_ci", "CI:", value = 95, min = 0),
                        numericInput("pv1_lcl", "LCL:", value = 0.1368),
                        numericInput("pv1_ucl", "UCL:", value = 0.6086)
                      ),
                      DTOutput("pv1_table_calc"),
                      br(),
                      DTOutput("pv1_table_est")
               ),
               column(6,
                      h4("Calculator 2: Given OR, LCL, UCL"),
                      wellPanel(
                        numericInput("pv2_or", "OR:", value = 0.2885),
                        numericInput("pv2_lcl", "LCL:", value = 0.1368),
                        numericInput("pv2_ucl", "UCL:", value = 0.6286)
                      ),
                      DTOutput("pv2_table_calc"),
                      br(),
                      DTOutput("pv2_table_est")
               )
             )
    ),
    
    # ==========================================
    # 5. OR & RR Conversion Calculator
    # ==========================================
    tabPanel("5. OR & RR Conversion",
             fluidRow(
               column(12,
                      wellPanel(
                        numericInput("conv_prev", "Prevalence Rate in Reference Group (%):", value = 35, min = 0, max = 100)
                      )
               )
             ),
             fluidRow(
               column(6,
                      h4("Convert OR to RR"),
                      wellPanel(
                        numericInput("conv_or", "OR:", value = 0.7351),
                        numericInput("conv_or_lcl", "LCL:", value = 0.4469),
                        numericInput("conv_or_ucl", "UCL:", value = 1.3150)
                      ),
                      DTOutput("conv_table_or2rr")
               ),
               column(6,
                      h4("Convert RR to OR"),
                      wellPanel(
                        numericInput("conv_rr", "RR:", value = 0.8102),
                        numericInput("conv_rr_lcl", "LCL:", value = 0.5542),
                        numericInput("conv_rr_ucl", "UCL:", value = 1.1844)
                      ),
                      DTOutput("conv_table_rr2or")
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Format helper functions
  fmt_pct <- function(val) sprintf("%.4f%%", val * 100)
  fmt_dec <- function(val) sprintf("%.4f", val)
  fmt_int <- function(val) sprintf("%.0f", val)
  
  # ==========================================
  # 1. Screening Test Statistics
  # ==========================================
  output$st_table_main <- renderDT({
    a <- input$st_a; b <- input$st_b; c <- input$st_c; d <- input$st_d
    
    tpr <- a / (a + c)
    tnr <- d / (b + d)
    
    if (!is.na(input$st_rate) && input$st_rate > 0) {
      prev <- input$st_rate / 100
      ppv <- (tpr * prev) / ((tpr * prev) + ((1 - tnr) * (1 - prev)))
      npv <- (tnr * (1 - prev)) / ((tnr * (1 - prev)) + ((1 - tpr) * prev))
    } else {
      ppv <- a / (a + b)
      npv <- d / (c + d)
    }
    
    df <- data.frame(
      Result = c("TPR", "TNR", "PPV", "NPV"),
      Definition = c("Sensitivity (True Positive Rate)", "Specificity (True Negative Rate)", 
                     "Precision (Positive Predictive Value)", "Negative Predictive Value"),
      Value = c(fmt_pct(tpr), fmt_pct(tnr), fmt_pct(ppv), fmt_pct(npv))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$st_table_others <- renderDT({
    a <- input$st_a; b <- input$st_b; c <- input$st_c; d <- input$st_d
    
    tpr <- a / (a + c)
    tnr <- d / (b + d)
    ppv <- a / (a + b) # Used for F1 regardless of Bayes
    
    acc <- (a + d) / (a + b + c + d)
    inacc <- 1 - acc
    f1 <- (2 * a) / (2 * a + b + c)
    prev <- (a + c) / (a + b + c + d)
    fnr <- c / (a + c)
    fpr <- b / (b + d)
    for_val <- c / (c + d)
    fdr <- b / (a + b)
    lr_pos <- tpr / fpr
    lr_neg <- fnr / tnr
    dor <- lr_pos / lr_neg
    
    df <- data.frame(
      Others = c("ACC", "INACC", "F1 Score", "PREV", "FNR", "FPR", "FOR", "FDR", "LR+", "LR-", "DOR"),
      Definition = c("Accuracy", "Inaccuracy", "Harmonic mean of precision", "Prevalence",
                     "False Negative Rate (Miss Rate)", "False Positive Rate (Fall-Out Rate)",
                     "False Omission Rate", "False Discovery Rate", "Positive Likelihood Ratio",
                     "Negative Likelihood Ratio", "Diagnostic Odds Ratio"),
      Value = c(fmt_pct(acc), fmt_pct(inacc), fmt_dec(f1), fmt_pct(prev), fmt_pct(fnr), fmt_pct(fpr),
                fmt_pct(for_val), fmt_pct(fdr), fmt_dec(lr_pos), fmt_dec(lr_neg), fmt_dec(dor))
    )
    datatable(df, options = list(pageLength = 15, dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # ==========================================
  # 2. Risk Epidemiology Calculators
  # ==========================================
  re_calc <- reactive({
    a <- input$re_a; b <- input$re_b; c <- input$re_c; d <- input$re_d
    n1 <- a + b; n2 <- c + d
    p1 <- a / n1; p2 <- c / n2
    
    arr <- p1 - p2
    rr <- p1 / p2
    nnt <- 1 / abs(arr)
    
    z_crit <- qnorm(1 - (1 - input$re_ci/100)/2)
    se_ln_rr <- sqrt(1/a + 1/c - 1/n1 - 1/n2)
    
    lcl <- rr * exp(-z_crit * se_ln_rr)
    ucl <- rr * exp(z_crit * se_ln_rr)
    
    z_score <- abs(log(rr) / se_ln_rr)
    p_val <- 2 * (1 - pnorm(z_score))
    
    list(arr=arr, rr=rr, nnt=nnt, lcl=lcl, ucl=ucl, z=z_score, p=p_val, se=se_ln_rr)
  })
  
  output$re_table_main <- renderDT({
    res <- re_calc()
    df <- data.frame(
      Result = c("Absolute Risk Reduction", "Relative Risk", "Number Needed to Treat", "SD{ln(RR)}"),
      Value = c(fmt_dec(res$arr), fmt_dec(res$rr), fmt_int(res$nnt), fmt_dec(res$se))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$re_table_ci <- renderDT({
    res <- re_calc()
    df <- data.frame(RR = fmt_dec(res$rr), LCL = fmt_dec(res$lcl), UCL = fmt_dec(res$ucl))
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$re_table_z <- renderDT({
    res <- re_calc()
    df <- data.frame(`z-score` = fmt_dec(res$z), `p-value` = fmt_dec(res$p), check.names = FALSE)
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # ==========================================
  # 3. Odds Epidemiology Calculators
  # ==========================================
  oe_calc <- reactive({
    a <- input$oe_a; b <- input$oe_b; c <- input$oe_c; d <- input$oe_d
    
    p_event_g1 <- a / (a + b)
    p_not_event_g1 <- b / (a + b)
    odds_against <- b / a
    odds_favor <- a / b
    or_val <- (a / b) / (c / d)
    
    z_crit <- qnorm(1 - (1 - input$oe_ci/100)/2)
    se_ln_or <- sqrt(1/a + 1/b + 1/c + 1/d)
    
    lcl <- or_val * exp(-z_crit * se_ln_or)
    ucl <- or_val * exp(z_crit * se_ln_or)
    
    z_score <- abs(log(or_val) / se_ln_or)
    p_val <- 2 * (1 - pnorm(z_score))
    
    list(p_e=p_event_g1, p_ne=p_not_event_g1, oa=odds_against, of=odds_favor, 
         or=or_val, lcl=lcl, ucl=ucl, z=z_score, p=p_val, se=se_ln_or)
  })
  
  output$oe_table_main <- renderDT({
    res <- oe_calc()
    df <- data.frame(
      Result = c("P(Event for Group 1)", "P(NOT Event for Group 1)", "Actual Odds Against", "Actual Odds in Favor", "Odds Ratio", "SD{ln(OR)}"),
      Value = c(fmt_dec(res$p_e), fmt_dec(res$p_ne), fmt_dec(res$oa), fmt_dec(res$of), fmt_dec(res$or), fmt_dec(res$se))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$oe_table_ci <- renderDT({
    res <- oe_calc()
    df <- data.frame(OR = fmt_dec(res$or), LCL = fmt_dec(res$lcl), UCL = fmt_dec(res$ucl))
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$oe_table_z <- renderDT({
    res <- oe_calc()
    df <- data.frame(`z-score` = fmt_dec(res$z), `p-value` = fmt_dec(res$p), check.names = FALSE)
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # ==========================================
  # 4. Odds Ratio P-Value Calculator
  # ==========================================
  output$pv1_table_calc <- renderDT({
    lnlcl <- log(input$pv1_lcl)
    lnucl <- log(input$pv1_ucl)
    z <- qnorm(1 - (1 - input$pv1_ci/100)/2)
    se <- (lnucl - lnlcl) / (2 * z)
    b <- (lnlcl + lnucl) / 2
    
    df <- data.frame(
      Metric = c("lnLCL", "lnUCL", "z", "SE", "B"),
      Value = c(fmt_dec(lnlcl), fmt_dec(lnucl), fmt_dec(z), fmt_dec(se), fmt_dec(b))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$pv1_table_est <- renderDT({
    lnlcl <- log(input$pv1_lcl)
    lnucl <- log(input$pv1_ucl)
    z <- qnorm(1 - (1 - input$pv1_ci/100)/2)
    se <- (lnucl - lnlcl) / (2 * z)
    b <- (lnlcl + lnucl) / 2
    
    or_val <- exp(b)
    wald <- (b / se)^2
    p_val <- 1 - pchisq(wald, 1)
    
    df <- data.frame(
      Estimates = c("OR", "Wald X2", "p"),
      Value = c(fmt_dec(or_val), fmt_dec(wald), fmt_dec(p_val))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$pv2_table_calc <- renderDT({
    est <- log(input$pv2_or)
    l <- log(input$pv2_lcl)
    u <- log(input$pv2_ucl)
    se <- (u - l) / (2 * 1.96)
    
    df <- data.frame(
      Metric = c("Est", "L", "U", "SE"),
      Value = c(fmt_dec(est), fmt_dec(l), fmt_dec(u), fmt_dec(se))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$pv2_table_est <- renderDT({
    est <- log(input$pv2_or)
    l <- log(input$pv2_lcl)
    u <- log(input$pv2_ucl)
    se <- (u - l) / (2 * 1.96)
    
    z <- abs(est / se)
    p_val <- 2 * (1 - pnorm(z))
    
    df <- data.frame(
      Estimates = c("z", "p"),
      Value = c(fmt_dec(z), fmt_dec(p_val))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # ==========================================
  # 5. OR & RR Conversion Calculator
  # ==========================================
  output$conv_table_or2rr <- renderDT({
    p0 <- input$conv_prev / 100
    or_v <- input$conv_or
    lcl_v <- input$conv_or_lcl
    ucl_v <- input$conv_or_ucl
    
    rr <- or_v / ((1 - p0) + (or_v * p0))
    lcl <- lcl_v / ((1 - p0) + (lcl_v * p0))
    ucl <- ucl_v / ((1 - p0) + (ucl_v * p0))
    
    df <- data.frame(Estimates = c("RR", "LCL", "UCL"), Value = c(fmt_dec(rr), fmt_dec(lcl), fmt_dec(ucl)))
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$conv_table_rr2or <- renderDT({
    p0 <- input$conv_prev / 100
    rr_v <- input$conv_rr
    lcl_v <- input$conv_rr_lcl
    ucl_v <- input$conv_rr_ucl
    
    or_val <- (rr_v * (1 - p0)) / (1 - rr_v * p0)
    lcl <- (lcl_v * (1 - p0)) / (1 - lcl_v * p0)
    ucl <- (ucl_v * (1 - p0)) / (1 - ucl_v * p0)
    
    df <- data.frame(Estimates = c("OR", "LCL", "UCL"), Value = c(fmt_dec(or_val), fmt_dec(lcl), fmt_dec(ucl)))
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)