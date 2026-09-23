library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Chapter 6: Sample Size Calculators"),
  
  # Note from the screenshot
  tags$div(
    style = "color: red; font-style: italic; margin-bottom: 20px;",
    "Note: These calculators are specific to Chapter 6 to calculate sample size based on a given error and desired CI. Sample size calculators in the bonus section should be used to account for effect size and other options."
  ),
  
  fluidRow(
    # ==========================================
    # Calculator 1: Sample Size for Mean
    # ==========================================
    column(6,
           wellPanel(
             h3("Sample Size for Mean", style = "text-align: center; background-color: #FFE699; padding: 10px;"),
             
             numericInput("sm_error", "Error:", value = 0.5000, step = 0.01),
             
             radioButtons("sm_sd_var", "Enter SD(x) or VAR(x):", 
                          choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
             numericInput("sm_val", "Value for SD(x) or VAR(x):", value = 1.0000, step = 0.01),
             
             numericInput("sm_ci", "CI %:", value = 99.0000, step = 1),
             
             hr(),
             h4("Automatically Calculated Values"),
             tableOutput("sm_auto_table"),
             
             h4("Sample Size Result", style = "text-align: center; background-color: #8EAADB; padding: 5px; color: white;"),
             tableOutput("sm_result_table")
           )
    ),
    
    # ==========================================
    # Calculator 2: Sample Size for Proportion
    # ==========================================
    column(6,
           wellPanel(
             h3("Sample Size for Proportion", style = "text-align: center; background-color: #FFE699; padding: 10px;"),
             
             numericInput("sp_error", "Error:", value = 0.0300, step = 0.001),
             numericInput("sp_p", "p:", value = 0.2900, step = 0.01),
             numericInput("sp_ci", "CI %:", value = 90.0000, step = 1),
             
             hr(),
             h4("Automatically Calculated Values"),
             tableOutput("sp_auto_table"),
             
             h4("Sample Size Result", style = "text-align: center; background-color: #8EAADB; padding: 5px; color: white;"),
             tableOutput("sp_result_table")
           )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Formatting helper for 4 decimal places
  fmt <- function(x) sprintf("%.4f", x)
  
  # ==========================================
  # Logic for Sample Size for Mean
  # ==========================================
  
  # Calculate SD and VAR
  sm_data <- reactive({
    req(input$sm_val, input$sm_error, input$sm_ci)
    
    if (input$sm_sd_var == "sd") {
      sd_x <- input$sm_val
      var_x <- input$sm_val^2
    } else {
      sd_x <- sqrt(input$sm_val)
      var_x <- input$sm_val
    }
    
    alpha <- 1 - (input$sm_ci / 100)
    z_crit <- qnorm(1 - alpha/2)
    
    # Formula: n = (Z * SD / Error)^2
    n_est <- (z_crit * sd_x / input$sm_error)^2
    n_final <- ceiling(n_est) # Always round up to next whole number
    
    list(sd_x = sd_x, var_x = var_x, n_est = n_est, n_final = n_final)
  })
  
  # Render Automatically Calculated SD/VAR
  output$sm_auto_table <- renderTable({
    d <- sm_data()
    data.frame(
      Statistic = c("SD(x)", "VAR(x)"),
      Value = fmt(c(d$sd_x, d$var_x))
    )
  }, colnames = FALSE)
  
  # Render Sample Size Result
  output$sm_result_table <- renderTable({
    d <- sm_data()
    data.frame(
      Type = c("Estimate", "Final"),
      Value = c(fmt(d$n_est), as.character(d$n_final))
    )
  }, colnames = FALSE, align = "lc")
  
  
  # ==========================================
  # Logic for Sample Size for Proportion
  # ==========================================
  
  sp_data <- reactive({
    req(input$sp_p, input$sp_error, input$sp_ci)
    
    p <- input$sp_p
    q <- 1 - p
    
    alpha <- 1 - (input$sp_ci / 100)
    z_crit <- qnorm(1 - alpha/2)
    
    # Formula: n = p * q * (Z / Error)^2
    n_est <- p * q * (z_crit / input$sp_error)^2
    n_final <- ceiling(n_est) # Always round up to next whole number
    
    list(q = q, n_est = n_est, n_final = n_final)
  })
  
  # Render Automatically Calculated 'q'
  output$sp_auto_table <- renderTable({
    d <- sp_data()
    data.frame(
      Statistic = c("q"),
      Value = fmt(d$q)
    )
  }, colnames = FALSE)
  
  # Render Sample Size Result
  output$sp_result_table <- renderTable({
    d <- sp_data()
    data.frame(
      Type = c("Estimate", "Final"),
      Value = c(fmt(d$n_est), as.character(d$n_final))
    )
  }, colnames = FALSE, align = "lc")
  
}

# Run the application
shinyApp(ui = ui, server = server)