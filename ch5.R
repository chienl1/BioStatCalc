library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Probability Distribution & Sampling Calculator"),
  
  tabsetPanel(
    # ==========================================
    # Tab 1: Distribution of Mean
    # ==========================================
    tabPanel("Single Mean",
             sidebarLayout(
               sidebarPanel(
                 numericInput("sm_n", "n (Sample Size):", value = 50),
                 numericInput("sm_mu", "mu (Population Mean):", value = 721),
                 radioButtons("sm_sd_var_choice", "Select Input Type:",
                              choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("sm_sd_var_val", "Value for SD(x) or VAR(x):", value = 454),
                 hr(),
                 h4("Target Values (x-bar)"),
                 numericInput("sm_x", "x-bar (for Less/Greater than):", value = 700),
                 numericInput("sm_xL", "xL (x-bar lower for Between):", value = 115),
                 numericInput("sm_xU", "xU (x-bar upper for Between):", value = 125)
               ),
               mainPanel(
                 h4("Standard Error & Z-Scores"),
                 tableOutput("sm_stats_table"),
                 h4("Probability Results"),
                 tableOutput("sm_prob_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 2: Difference of 2 Means
    # ==========================================
    tabPanel("Difference of 2 Means",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("d2m_n1", "n1:", value = 35),
                 numericInput("d2m_mu1", "mu1:", value = 45),
                 radioButtons("d2m_sd_var_choice1", "Select Input:", choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("d2m_sd_var_val1", "Value for SD(x) or VAR(x) (Sample 1):", value = 15),
                 h4("Sample 2"),
                 numericInput("d2m_n2", "n2:", value = 40),
                 numericInput("d2m_mu2", "mu2:", value = 30),
                 radioButtons("d2m_sd_var_choice2", "Select Input:", choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
                 numericInput("d2m_sd_var_val2", "Value for SD(x) or VAR(x) (Sample 2):", value = 20),
                 hr(),
                 h4("Target Values (x-bar Diff)"),
                 numericInput("d2m_x", "x-bar Diff (for Less/Greater):", value = 20),
                 numericInput("d2m_xL", "xL (lower diff for Between):", value = 15),
                 numericInput("d2m_xU", "xU (upper diff for Between):", value = 25)
               ),
               mainPanel(
                 h4("Standard Error & Z-Scores"),
                 tableOutput("d2m_stats_table"),
                 h4("Probability Results"),
                 tableOutput("d2m_prob_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 3: Single Proportion
    # ==========================================
    tabPanel("Single Proportion",
             sidebarLayout(
               sidebarPanel(
                 numericInput("sp_n", "n (Sample Size):", value = 70),
                 numericInput("sp_p", "p (Population Proportion):", value = 0.13, step = 0.01),
                 hr(),
                 h4("Target Proportions (p-hat)"),
                 numericInput("sp_x", "p-hat (for Less/Greater than):", value = 0.10, step = 0.01),
                 numericInput("sp_xL", "pHL (p-hat lower for Between):", value = 0.50, step = 0.01),
                 numericInput("sp_xU", "pHU (p-hat upper for Between):", value = 0.60, step = 0.01)
               ),
               mainPanel(
                 h4("Standard Error & Z-Scores"),
                 tableOutput("sp_stats_table"),
                 h4("Probability Results"),
                 tableOutput("sp_prob_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 4: Difference of 2 Proportions
    # ==========================================
    tabPanel("Difference of 2 Proportions",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample 1"),
                 numericInput("d2p_n1", "n1:", value = 1200),
                 numericInput("d2p_p1", "p1:", value = 0.20, step = 0.01),
                 h4("Sample 2"),
                 numericInput("d2p_n2", "n2:", value = 600),
                 numericInput("d2p_p2", "p2:", value = 0.13, step = 0.01),
                 hr(),
                 h4("Target Values (p-hat Diff)"),
                 numericInput("d2p_x", "p-hat Diff (for Less/Greater):", value = 0.08, step = 0.01),
                 numericInput("d2p_xL", "pHLD (lower diff for Between):", value = 0.03, step = 0.01),
                 numericInput("d2p_xU", "pHUD (upper diff for Between):", value = 0.10, step = 0.01)
               ),
               mainPanel(
                 h4("Standard Error & Z-Scores"),
                 tableOutput("d2p_stats_table"),
                 h4("Probability Results"),
                 tableOutput("d2p_prob_table")
               )
             )
    ),
    
    # ==========================================
    # Tab 5: Sampling Without Replacement
    # ==========================================
    tabPanel("Sampling Without Replacement",
             sidebarLayout(
               sidebarPanel(
                 textInput("swr_pop", "Population Values (comma separated):", value = "6, 8, 10, 12, 14"),
                 numericInput("swr_n", "Sample Size (n):", value = 2),
                 helpText("Note: N is automatically calculated from the population values.")
               ),
               mainPanel(
                 h4("Calculated Statistics"),
                 tableOutput("swr_summary_table"),
                 hr(),
                 h4("All Combinations & Means (Values)"),
                 dataTableOutput("swr_combs_table")
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # ==========================================
  # Server: Single Mean
  # ==========================================
  output$sm_stats_table <- renderTable({
    sd_x <- if(input$sm_sd_var_choice == "sd") input$sm_sd_var_val else sqrt(input$sm_sd_var_val)
    se <- sd_x / sqrt(input$sm_n)
    
    z_single <- (input$sm_x - input$sm_mu) / se
    z_L <- (input$sm_xL - input$sm_mu) / se
    z_U <- (input$sm_xU - input$sm_mu) / se
    
    data.frame(
      Metric = c("SD(pool) / SE", "z (Single)", "z (Lower)", "z (Upper)"),
      Value = sprintf("%.4f", c(se, z_single, z_L, z_U))
    )
  })
  
  output$sm_prob_table <- renderTable({
    sd_x <- if(input$sm_sd_var_choice == "sd") input$sm_sd_var_val else sqrt(input$sm_sd_var_val)
    se <- sd_x / sqrt(input$sm_n)
    
    z_single <- (input$sm_x - input$sm_mu) / se
    z_L <- (input$sm_xL - input$sm_mu) / se
    z_U <- (input$sm_xU - input$sm_mu) / se
    
    prob_less <- pnorm(z_single)
    prob_greater <- 1 - pnorm(z_single)
    prob_between <- pnorm(z_U) - pnorm(z_L)
    
    data.frame(
      Result = c("< or ≤", "> or ≥", "> or ≥ xL & < or ≤ xU (Between)"),
      Prob = sprintf("%.4f", c(prob_less, prob_greater, prob_between))
    )
  })
  
  # ==========================================
  # Server: Difference of 2 Means
  # ==========================================
  output$d2m_stats_table <- renderTable({
    v1 <- if(input$d2m_sd_var_choice1 == "sd") input$d2m_sd_var_val1^2 else input$d2m_sd_var_val1
    v2 <- if(input$d2m_sd_var_choice2 == "sd") input$d2m_sd_var_val2^2 else input$d2m_sd_var_val2
    se <- sqrt(v1/input$d2m_n1 + v2/input$d2m_n2)
    mu_diff <- input$d2m_mu1 - input$d2m_mu2
    
    z_single <- (input$d2m_x - mu_diff) / se
    z_L <- (input$d2m_xL - mu_diff) / se
    z_U <- (input$d2m_xU - mu_diff) / se
    
    data.frame(
      Metric = c("mu Diff", "SD(pool) / SE", "z (Single)", "z (Lower)", "z (Upper)"),
      Value = sprintf("%.4f", c(mu_diff, se, z_single, z_L, z_U))
    )
  })
  
  output$d2m_prob_table <- renderTable({
    v1 <- if(input$d2m_sd_var_choice1 == "sd") input$d2m_sd_var_val1^2 else input$d2m_sd_var_val1
    v2 <- if(input$d2m_sd_var_choice2 == "sd") input$d2m_sd_var_val2^2 else input$d2m_sd_var_val2
    se <- sqrt(v1/input$d2m_n1 + v2/input$d2m_n2)
    mu_diff <- input$d2m_mu1 - input$d2m_mu2
    
    z_single <- (input$d2m_x - mu_diff) / se
    z_L <- (input$d2m_xL - mu_diff) / se
    z_U <- (input$d2m_xU - mu_diff) / se
    
    data.frame(
      Result = c("< or ≤", "> or ≥", "> or ≥ xL & < or ≤ xU (Between)"),
      Prob = sprintf("%.4f", c(pnorm(z_single), 1 - pnorm(z_single), pnorm(z_U) - pnorm(z_L)))
    )
  })
  
  # ==========================================
  # Server: Single Proportion
  # ==========================================
  output$sp_stats_table <- renderTable({
    se <- sqrt((input$sp_p * (1 - input$sp_p)) / input$sp_n)
    
    z_single <- (input$sp_x - input$sp_p) / se
    z_L <- (input$sp_xL - input$sp_p) / se
    z_U <- (input$sp_xU - input$sp_p) / se
    
    data.frame(
      Metric = c("SD(pool) / SE", "z (Single)", "z (Lower)", "z (Upper)"),
      Value = sprintf("%.4f", c(se, z_single, z_L, z_U))
    )
  })
  
  output$sp_prob_table <- renderTable({
    se <- sqrt((input$sp_p * (1 - input$sp_p)) / input$sp_n)
    z_single <- (input$sp_x - input$sp_p) / se
    z_L <- (input$sp_xL - input$sp_p) / se
    z_U <- (input$sp_xU - input$sp_p) / se
    
    data.frame(
      Result = c("< or ≤", "> or ≥", "> or ≥ xL & < or ≤ xU (Between)"),
      Prob = sprintf("%.4f", c(pnorm(z_single), 1 - pnorm(z_single), pnorm(z_U) - pnorm(z_L)))
    )
  })
  
  # ==========================================
  # Server: Difference of 2 Proportions
  # ==========================================
  output$d2p_stats_table <- renderTable({
    p_diff <- input$d2p_p1 - input$d2p_p2
    se <- sqrt((input$d2p_p1 * (1 - input$d2p_p1)) / input$d2p_n1 + (input$d2p_p2 * (1 - input$d2p_p2)) / input$d2p_n2)
    
    z_single <- (input$d2p_x - p_diff) / se
    z_L <- (input$d2p_xL - p_diff) / se
    z_U <- (input$d2p_xU - p_diff) / se
    
    data.frame(
      Metric = c("p Diff (mu)", "SD(pool) / SE", "z (Single)", "z (Lower)", "z (Upper)"),
      Value = sprintf("%.4f", c(p_diff, se, z_single, z_L, z_U))
    )
  })
  
  output$d2p_prob_table <- renderTable({
    p_diff <- input$d2p_p1 - input$d2p_p2
    se <- sqrt((input$d2p_p1 * (1 - input$d2p_p1)) / input$d2p_n1 + (input$d2p_p2 * (1 - input$d2p_p2)) / input$d2p_n2)
    
    z_single <- (input$d2p_x - p_diff) / se
    z_L <- (input$d2p_xL - p_diff) / se
    z_U <- (input$d2p_xU - p_diff) / se
    
    data.frame(
      Result = c("< or ≤", "> or ≥", "> or ≥ xL & < or ≤ xU (Between)"),
      Prob = sprintf("%.4f", c(pnorm(z_single), 1 - pnorm(z_single), pnorm(z_U) - pnorm(z_L)))
    )
  })
  
  # ==========================================
  # Server: Sampling Without Replacement
  # ==========================================
  swr_data <- reactive({
    req(input$swr_pop, input$swr_n)
    # Parse input string into numeric vector
    pop_vec <- as.numeric(unlist(strsplit(input$swr_pop, ",")))
    pop_vec <- pop_vec[!is.na(pop_vec)]
    n <- input$swr_n
    
    req(length(pop_vec) >= n) # Ensure population is larger than or equal to sample size
    
    N <- length(pop_vec)
    # Generate all combinations
    combs <- combn(pop_vec, n)
    means <- colMeans(combs)
    mean_of_means <- mean(means)
    
    # Calculate variances for the table
    vars <- (means - mean_of_means)^2
    
    list(N = N, n = n, means = means, vars = vars, pop_vec = pop_vec)
  })
  
  output$swr_combs_table <- renderDataTable({
    d <- swr_data()
    data.frame(
      `Data No.` = 1:length(d$means),
      Values = sprintf("%.4f", d$means),
      Var = sprintf("%.4f", d$vars)
    )
  }, options = list(pageLength = 10))
  
  output$swr_summary_table <- renderTable({
    d <- swr_data()
    means <- d$means
    K <- length(means) # Total combinations
    
    # Traditional Arithmetic Method (Sample statistics of the combinations)
    t_mean <- mean(means)
    t_var <- var(means) # Divides by K-1
    t_sd <- sqrt(t_var)
    t_se <- t_sd / sqrt(K)
    
    # Finite Population Sample Method (Population statistics of the combinations)
    f_mean <- mean(means)
    f_var <- t_var * ((K - 1) / K) # Divides by K
    f_sd <- sqrt(f_var)
    f_se <- f_sd / sqrt(K)
    
    data.frame(
      Statistic = c("Mean", "Variance", "SD", "SE"),
      `Traditional Method` = sprintf("%.4f", c(t_mean, t_var, t_sd, t_se)),
      `Finite Population Method` = sprintf("%.4f", c(f_mean, f_var, f_sd, f_se)),
      check.names = FALSE
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)