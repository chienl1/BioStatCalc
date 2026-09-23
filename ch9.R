# Install required packages: install.packages(c("shiny", "ggplot2", "lmtest", "randtests"))
library(shiny)
library(ggplot2)
library(lmtest)
library(randtests)

# --- UI Definition ---
ui <- fluidPage(
  withMathJax(), # Enable MathJax to render LaTeX math symbols
  titlePanel("Simple Linear Regression & Correlation Analysis Tools"),
  
  tabsetPanel(
    # ==========================================
    # Tab 1: Regression Analysis & Model Testing
    # ==========================================
    tabPanel("Regression Analysis & Model Testing",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Data Input Area"),
                 textAreaInput("x_data", "Enter X data (comma, space, or new line separated):", "1000, 550, 97, 90, 85, 91"),
                 textAreaInput("y_data", "Enter Y data (comma, space, or new line separated):", "600, 625, 560, 585, 590, 500"),
                 
                 hr(),
                 h4("Estimate Y for a Given X"),
                 numericInput("target_x", "Enter X value:", value = 45),
                 numericInput("ci_level", "C.I. % (Confidence Level):", value = 95, min = 1, max = 99),
                 actionButton("run_reg", "Run Regression & Prediction", class = "btn-primary")
               ),
               
               mainPanel(
                 h4("Fitted Model Equation"),
                 uiOutput("equation_ui"),
                 br(),
                 fluidRow(
                   column(6, plotOutput("scatter_plot")),
                   column(6, plotOutput("resid_plot"))
                 ),
                 br(),
                 h4("Model Diagnostic Test Results"),
                 tableOutput("diag_table"),
                 br(),
                 h4("Estimate y for a Given x"),
                 tableOutput("pred_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 2: Correlation Calculator Results
    # ==========================================
    tabPanel("Correlation Calculators",
             br(),
             fluidRow(
               # Left Column: CI for Correlation Coefficient
               column(6,
                      wellPanel(
                        h4("CI for Correlation Coefficient"),
                        numericInput("r_val", "Correlation coefficient (r):", value = 0.65, min = -1, max = 1, step = 0.01),
                        numericInput("n_val", "Sample size (n):", value = 50, min = 4),
                        numericInput("ci_corr", "C.I. %:", value = 95, min = 1, max = 99)
                      ),
                      h4("Result: CI for Correlation"),
                      tableOutput("corr_ci_table")
               ),
               
               # Right Column: Compare Two Correlation Coefficients
               column(6,
                      wellPanel(
                        h4("Compare Two Correlation Coefficients"),
                        numericInput("r1", "Sample 1 r(1):", value = 0.85, min = -1, max = 1, step = 0.01),
                        numericInput("n1", "Sample 1 n(1):", value = 42, min = 4),
                        numericInput("r2", "Sample 2 r(2):", value = 0.65, min = -1, max = 1, step = 0.01),
                        numericInput("n2", "Sample 2 n(2):", value = 42, min = 4)
                      ),
                      h4("Result: Compare Two Correlations"),
                      tableOutput("corr_comp_table")
               )
             )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # 1. Parse data and fit linear regression
  reg_data <- eventReactive(input$run_reg, {
    x_val <- as.numeric(unlist(strsplit(input$x_data, "[, \n]+")))
    y_val <- as.numeric(unlist(strsplit(input$y_data, "[, \n]+")))
    
    if (length(x_val) != length(y_val)) {
      showNotification("X and Y data lengths do not match!", type = "error")
      return(NULL)
    }
    if (length(x_val) < 3) {
      showNotification("Too few data points. Please provide at least 3 pairs of data.", type = "error")
      return(NULL)
    }
    
    df <- data.frame(X = x_val, Y = y_val)
    model <- lm(Y ~ X, data = df)
    list(df = df, model = model)
  }, ignoreNULL = FALSE)
  
  # Output fitted equation
  output$equation_ui <- renderUI({
    req(reg_data())
    mod <- reg_data()$model
    b0 <- round(coef(mod)[1], 4)
    b1 <- round(coef(mod)[2], 4)
    sign_b1 <- ifelse(b1 >= 0, "+", "-")
    
    # Output capital Y with a hat, and capital X
    eq_string <- sprintf("$$\\hat{Y} = %s %s %s X$$", b0, sign_b1, abs(b1))
    withMathJax(HTML(eq_string))
  })
  
  # Output Scatter Plot
  output$scatter_plot <- renderPlot({
    req(reg_data())
    df <- reg_data()$df
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(color = "steelblue", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "lightblue", linetype = "dotted") +
      theme_bw() +
      labs(title = "Scatterplot and Best Fit Line", x = "X", y = "Y")
  })
  
  # Output Residual Plot
  output$resid_plot <- renderPlot({
    req(reg_data())
    mod <- reg_data()$model
    df <- reg_data()$df
    df$Resid <- resid(mod)
    ggplot(df, aes(x = X, y = Resid)) +
      geom_point(color = "steelblue", size = 3) +
      geom_hline(yintercept = 0, color = "red") +
      theme_bw() +
      labs(title = "Residual Plot", x = "X", y = "Residuals")
  })
  
  # Model Diagnostic Tests Table
  output$diag_table <- renderTable({
    req(reg_data())
    mod <- reg_data()$model
    df <- reg_data()$df
    
    # Durbin-Watson Test
    dw <- lmtest::dwtest(mod)
    
    # White Test
    white <- lmtest::bptest(mod, ~ X + I(X^2), data = df)
    
    # Shapiro-Wilk Test
    shapiro <- shapiro.test(resid(mod))
    
    # Runs Test
    runs <- randtests::runs.test(resid(mod), threshold = 0)
    
    data.frame(
      Test = c("Durbin-Watson", "White Test", "Shapiro-Wilks", "Runs Test"),
      Statistic = c(dw$statistic, white$statistic, shapiro$statistic, runs$statistic),
      `p-value` = c(dw$p.value, white$p.value, shapiro$p.value, runs$p.value),
      check.names = FALSE
    )
  }, digits = 4)
  
  # Prediction Table
  output$pred_table <- renderTable({
    req(reg_data())
    mod <- reg_data()$model
    target_x <- data.frame(X = input$target_x)
    alpha <- input$ci_level / 100
    
    ci_pred <- predict(mod, newdata = target_x, interval = "confidence", level = alpha)
    pi_pred <- predict(mod, newdata = target_x, interval = "prediction", level = alpha)
    
    data.frame(
      Type = c("Estimated Y", "Confidence Interval", "Prediction Interval"),
      Value = c(ci_pred[1, "fit"], NA, NA),
      Lower = c(NA, ci_pred[1, "lwr"], pi_pred[1, "lwr"]),
      Upper = c(NA, ci_pred[1, "upr"], pi_pred[1, "upr"])
    )
  }, na = "", digits = 4)
  
  
  # 2. CI for Correlation Coefficient
  corr_ci_res <- reactive({
    req(input$r_val, input$n_val, input$ci_corr)
    
    r <- input$r_val
    n <- input$n_val
    conf <- input$ci_corr / 100
    
    # Fisher Z Transformation
    z_r <- 0.5 * log((1 + r) / (1 - r)) 
    se <- 1 / sqrt(n - 3)
    z_crit <- qnorm(1 - (1 - conf) / 2)
    
    ll_z <- z_r - z_crit * se
    ul_z <- z_r + z_crit * se
    
    # Inverse transform back to r
    lcl <- tanh(ll_z)
    ucl <- tanh(ul_z)
    
    data.frame(
      Metric = c("z(r)", "LL (Z)", "UL (Z)", "LCL (r)", "UCL (r)"),
      Value = c(z_r, ll_z, ul_z, lcl, ucl)
    )
  })
  
  output$corr_ci_table <- renderTable({
    corr_ci_res()
  }, digits = 4)
  
  
  # 3. Compare Two Correlation Coefficients
  corr_comp_res <- reactive({
    req(input$r1, input$n1, input$r2, input$n2)
    
    r1 <- input$r1; n1 <- input$n1
    r2 <- input$r2; n2 <- input$n2
    
    z1 <- 0.5 * log((1 + r1) / (1 - r1))
    z2 <- 0.5 * log((1 + r2) / (1 - r2))
    se_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
    
    z_stat <- (z1 - z2) / se_diff
    
    # Calculated based on the Excel screenshot's exact mathematical logic:
    z_lcl <- z_stat - se_diff
    z_ucl <- z_stat + se_diff
    
    p_val <- 2 * (1 - pnorm(abs(z_stat)))
    
    # Build Table
    data.frame(
      Metric = c("z(1)", "z(2)", "SE(Z(diff))", "Z(diff) Test Stat", "Z(diff) LCL (95%)", "Z(diff) UCL (95%)", "Prob.", "p-value"),
      Value = c(z1, z2, se_diff, z_stat, z_lcl, z_ucl, p_val, p_val)
    )
  })
  
  output$corr_comp_table <- renderTable({
    corr_comp_res()
  }, digits = 4)
  
}

# Run the App
shinyApp(ui = ui, server = server)