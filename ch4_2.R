# Load required packages
library(shiny)
library(DT)

# Define User Interface (UI)
ui <- fluidPage(
  titlePanel("Discrete Distributions Probability Calculators"),
  
  tabsetPanel(
    
    # ==========================================
    # Binomial Distribution Tab
    # ==========================================
    tabPanel("Binomial Distribution",
             br(),
             fluidRow(
               # 1. Combined Probability Box
               column(4,
                      wellPanel(
                        h4("Probability for x"),
                        numericInput("bin_n1", "n:", value = 12, min = 1),
                        numericInput("bin_p1", "p:", value = 0.5500, min = 0, max = 1, step = 0.01),
                        numericInput("bin_x1", "x:", value = 7, min = 0),
                        br(),
                        DTOutput("bin_prob_table")
                      )
               ),
               # 2. Between Box
               column(4,
                      wellPanel(
                        h4("Between..."),
                        numericInput("bin_n2", "n:", value = 9, min = 1),
                        numericInput("bin_p2", "p:", value = 0.2500, min = 0, max = 1, step = 0.01),
                        numericInput("bin_xl", "xL (x lower):", value = 1, min = 0),
                        numericInput("bin_xu", "xU (x upper):", value = 5, min = 0),
                        br(),
                        DTOutput("bin_between_table")
                      )
               ),
               # 3. Mean, Variance, SD Box
               column(4,
                      wellPanel(
                        h4("Mean, Variance, SD"),
                        numericInput("bin_n3", "n:", value = 12, min = 1),
                        numericInput("bin_p3", "p:", value = 0.5500, min = 0, max = 1, step = 0.01),
                        tags$p(strong("q = 1 - p: "), textOutput("bin_q_val", inline = TRUE)),
                        br(),
                        DTOutput("bin_stats_table")
                      )
               )
             )
    ),
    
    # ==========================================
    # Poisson Distribution Tab
    # ==========================================
    tabPanel("Poisson Distribution",
             br(),
             fluidRow(
               # 1. Combined Probability Box
               column(4,
                      wellPanel(
                        h4("Probability for x"),
                        numericInput("poi_mean1", "mean:", value = 5.0000, min = 0, step = 0.1),
                        numericInput("poi_x1", "x:", value = 5, min = 0),
                        br(),
                        DTOutput("poi_prob_table")
                      )
               ),
               # 2. Between Box
               column(4,
                      wellPanel(
                        h4("Between..."),
                        numericInput("poi_mean2", "mean:", value = 11.5863, min = 0, step = 0.1),
                        numericInput("poi_xl", "xL (x lower):", value = 1, min = 0),
                        numericInput("poi_xu", "xU (x upper):", value = 2, min = 0),
                        br(),
                        DTOutput("poi_between_table")
                      )
               ),
               # 3. Mean, Variance, SD Box
               column(4,
                      wellPanel(
                        h4("Mean, Variance, SD"),
                        helpText("Note: This calculator is only necessary if converting to a different period of time/space."),
                        numericInput("poi_num", "Numerator:", value = 2.0000, min = 0),
                        numericInput("poi_den", "Denominator:", value = 12.0000, min = 0.0001),
                        br(),
                        DTOutput("poi_stats_table")
                      )
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Helper function to format exactly 4 decimal places
  fmt_4dec <- function(x) sprintf("%.4f", x)
  
  # ==========================================
  # Binomial Computations
  # ==========================================
  
  # 1. Combined Probabilities
  output$bin_prob_table <- renderDT({
    n <- input$bin_n1
    p <- input$bin_p1
    x <- input$bin_x1
    
    req(n, p, x)
    
    prob_eq <- dbinom(x, n, p)
    prob_lt <- pbinom(x - 1, n, p)
    prob_le <- pbinom(x, n, p)
    prob_gt <- 1 - pbinom(x, n, p)
    prob_ge <- 1 - pbinom(x - 1, n, p)
    
    df <- data.frame(
      Result = c("\"=\"", "\"<\"", "\"≤\"", "\">\"", "\"≥\""),
      Prob = c(fmt_4dec(prob_eq), fmt_4dec(prob_lt), fmt_4dec(prob_le), fmt_4dec(prob_gt), fmt_4dec(prob_ge))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # 2. Between Probabilities
  output$bin_between_table <- renderDT({
    n <- input$bin_n2
    p <- input$bin_p2
    xL <- input$bin_xl
    xU <- input$bin_xu
    
    req(n, p, xL, xU)
    
    p1 <- pbinom(xU - 1, n, p) - pbinom(xL, n, p)
    p2 <- pbinom(xU - 1, n, p) - pbinom(xL - 1, n, p)
    p3 <- pbinom(xU, n, p) - pbinom(xL, n, p)
    p4 <- pbinom(xU, n, p) - pbinom(xL - 1, n, p)
    
    df <- data.frame(
      Result = c("\"(xL < x < xU)\"", "\"(xL ≤ x < xU)\"", "\"(xL < x ≤ xU)\"", "\"(xL ≤ x ≤ xU)\""),
      Prob = c(fmt_4dec(max(0, p1)), fmt_4dec(max(0, p2)), fmt_4dec(max(0, p3)), fmt_4dec(max(0, p4)))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # 3. Mean, Variance, SD
  output$bin_q_val <- renderText({
    fmt_4dec(1 - input$bin_p3)
  })
  
  output$bin_stats_table <- renderDT({
    n <- input$bin_n3
    p <- input$bin_p3
    
    req(n, p)
    
    mean_val <- n * p
    var_val <- n * p * (1 - p)
    sd_val <- sqrt(var_val)
    
    df <- data.frame(
      Statistic = c("Mean", "Variance", "SD"),
      Value = c(fmt_4dec(mean_val), fmt_4dec(var_val), fmt_4dec(sd_val))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # ==========================================
  # Poisson Computations
  # ==========================================
  
  # 1. Combined Probabilities
  output$poi_prob_table <- renderDT({
    lambda <- input$poi_mean1
    x <- input$poi_x1
    
    req(lambda, x)
    
    prob_eq <- dpois(x, lambda)
    prob_lt <- ppois(x - 1, lambda)
    prob_le <- ppois(x, lambda)
    prob_gt <- 1 - ppois(x, lambda)
    prob_ge <- 1 - ppois(x - 1, lambda)
    
    df <- data.frame(
      Result = c("\"=\"", "\"<\"", "\"≤\"", "\">\"", "\"≥\""),
      Prob = c(fmt_4dec(prob_eq), fmt_4dec(prob_lt), fmt_4dec(prob_le), fmt_4dec(prob_gt), fmt_4dec(prob_ge))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # 2. Between Probabilities
  output$poi_between_table <- renderDT({
    lambda <- input$poi_mean2
    xL <- input$poi_xl
    xU <- input$poi_xu
    
    req(lambda, xL, xU)
    
    p1 <- ppois(xU - 1, lambda) - ppois(xL, lambda)
    p2 <- ppois(xU - 1, lambda) - ppois(xL - 1, lambda)
    p3 <- ppois(xU, lambda) - ppois(xL, lambda)
    p4 <- ppois(xU, lambda) - ppois(xL - 1, lambda)
    
    df <- data.frame(
      Result = c("\"(xL < x < xU)\"", "\"(xL ≤ x < xU)\"", "\"(xL < x ≤ xU)\"", "\"(xL ≤ x ≤ xU)\""),
      Prob = c(fmt_4dec(max(0, p1)), fmt_4dec(max(0, p2)), fmt_4dec(max(0, p3)), fmt_4dec(max(0, p4)))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # 3. Mean, Variance, SD
  output$poi_stats_table <- renderDT({
    num <- input$poi_num
    den <- input$poi_den
    
    req(num, den)
    
    mean_val <- num / den
    var_val <- mean_val  # For Poisson, Variance = Mean
    sd_val <- sqrt(var_val)
    
    df <- data.frame(
      Statistic = c("Mean", "Variance", "SD"),
      Value = c(fmt_4dec(mean_val), fmt_4dec(var_val), fmt_4dec(sd_val))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)