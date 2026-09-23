# Load required packages
library(shiny)
library(DT)

# Define User Interface (UI)
ui <- fluidPage(
  titlePanel("Normal Distribution Probability Calculators"),
  
  tabsetPanel(
    
    # ==========================================
    # Normal Distribution (Given x, mu, sd)
    # ==========================================
    tabPanel("Normal Distribution (x)",
             br(),
             fluidRow(
               # Combined Less/Greater/Equal Probability Box for x
               column(6,
                      wellPanel(
                        h4("Probability for x"),
                        numericInput("norm_mu", "mu (Mean):", value = 2.1000, step = 0.1),
                        numericInput("norm_sd", "SD(x):", value = 1.0000, min = 0.0001, step = 0.1),
                        numericInput("norm_var", "VAR(x):", value = 1.0000, min = 0.0001, step = 0.1),
                        helpText("Note: Input either SD(x) or VAR(x). Changing one updates the other."),
                        hr(),
                        numericInput("norm_x", "x:", value = 8.6500, step = 0.1),
                        br(),
                        DTOutput("norm_prob_table")
                      )
               ),
               # Between Box for x
               column(6,
                      wellPanel(
                        h4("Between..."),
                        numericInput("norm_mu2", "mu (Mean):", value = 491.0000, step = 1),
                        numericInput("norm_sd2", "SD(x):", value = 119.0000, min = 0.0001, step = 1),
                        numericInput("norm_var2", "VAR(x):", value = 14161.0000, min = 0.0001, step = 1),
                        helpText("Note: Input either SD(x) or VAR(x). Changing one updates the other."),
                        hr(),
                        numericInput("norm_xL", "xL (x lower):", value = 292.0000, step = 1),
                        numericInput("norm_xU", "xU (x upper):", value = 649.0000, step = 1),
                        br(),
                        DTOutput("norm_between_table")
                      )
               )
             )
    ),
    
    # ==========================================
    # Standard Normal Distribution (Given z)
    # ==========================================
    tabPanel("Standard Normal Distribution (z)",
             br(),
             fluidRow(
               # Combined Probability Box for z
               column(6,
                      wellPanel(
                        h4("Probability for z"),
                        numericInput("std_z", "z:", value = 1.9600, step = 0.01),
                        br(),
                        DTOutput("std_prob_table")
                      )
               ),
               # Between Box for z
               column(6,
                      wellPanel(
                        h4("Between..."),
                        numericInput("std_zL", "z-lower:", value = -2.7400, step = 0.01),
                        numericInput("std_zU", "z-upper:", value = 1.5300, step = 0.01),
                        br(),
                        DTOutput("std_between_table")
                      )
               )
             ),
             fluidRow(
               # Find z-score Box
               column(6,
                      wellPanel(
                        h4("Find z-score"),
                        numericInput("find_prob", "Probability:", value = 0.9750, min = 0.0001, max = 0.9999, step = 0.01),
                        br(),
                        DTOutput("find_z_table")
                      )
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  fmt_4dec <- function(x) sprintf("%.4f", x)
  
  # ==========================================
  # Dynamic UI Updates for SD and Variance
  # ==========================================
  observeEvent(input$norm_sd, {
    req(input$norm_sd)
    updateNumericInput(session, "norm_var", value = input$norm_sd^2)
  })
  observeEvent(input$norm_var, {
    req(input$norm_var)
    if(input$norm_var > 0) updateNumericInput(session, "norm_sd", value = sqrt(input$norm_var))
  })
  
  observeEvent(input$norm_sd2, {
    req(input$norm_sd2)
    updateNumericInput(session, "norm_var2", value = input$norm_sd2^2)
  })
  observeEvent(input$norm_var2, {
    req(input$norm_var2)
    if(input$norm_var2 > 0) updateNumericInput(session, "norm_sd2", value = sqrt(input$norm_var2))
  })
  
  # ==========================================
  # Normal Distribution Computations
  # ==========================================
  
  output$norm_prob_table <- renderDT({
    mu <- input$norm_mu
    sd <- input$norm_sd
    x <- input$norm_x
    req(mu, sd, x)
    
    z_score <- (x - mu) / sd
    prob_lt <- pnorm(x, mean = mu, sd = sd)
    prob_gt <- 1 - prob_lt
    # For continuous distributions, < and <= are equivalent
    
    df <- data.frame(
      Result = c("z", "\"< or ≤\"", "\"> or ≥\""),
      Value = c(fmt_4dec(z_score), fmt_4dec(prob_lt), fmt_4dec(prob_gt))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$norm_between_table <- renderDT({
    mu <- input$norm_mu2
    sd <- input$norm_sd2
    xL <- input$norm_xL
    xU <- input$norm_xU
    req(mu, sd, xL, xU)
    
    z_lower <- (xL - mu) / sd
    z_upper <- (xU - mu) / sd
    prob_between <- pnorm(xU, mean = mu, sd = sd) - pnorm(xL, mean = mu, sd = sd)
    
    df <- data.frame(
      Result = c("z (lower)", "z (upper)", "\"> or ≥ xL\" & \"< or ≤ xU\""),
      Value = c(fmt_4dec(z_lower), fmt_4dec(z_upper), fmt_4dec(prob_between))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # ==========================================
  # Standard Normal Computations
  # ==========================================
  
  output$std_prob_table <- renderDT({
    z <- input$std_z
    req(z)
    
    prob_lt <- pnorm(z)
    prob_gt <- 1 - prob_lt
    
    df <- data.frame(
      Result = c("\"< or ≤\"", "\"> or ≥\""),
      Prob = c(fmt_4dec(prob_lt), fmt_4dec(prob_gt))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$std_between_table <- renderDT({
    zL <- input$std_zL
    zU <- input$std_zU
    req(zL, zU)
    
    prob_between <- pnorm(zU) - pnorm(zL)
    
    df <- data.frame(
      Result = c("\"> or ≥ xL\" & \"< or ≤ xU\""),
      Prob = c(fmt_4dec(prob_between))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$find_z_table <- renderDT({
    prob <- input$find_prob
    req(prob)
    
    z_score <- qnorm(prob)
    
    df <- data.frame(
      Result = c("z"),
      Value = c(fmt_4dec(z_score))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)