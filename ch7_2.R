library(shiny)
library(ggplot2)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Power Curve Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Hypothesis Settings"),
      numericInput("h0", "H0:", value = 10.0000, step = 0.1),
      numericInput("n", "n:", value = 30, min = 2),
      
      radioButtons("sd_var", "Enter SD(x) or VAR(x):", 
                   choices = c("SD(x)" = "sd", "VAR(x)" = "var"), inline = TRUE),
      numericInput("val", "Value for SD(x) or VAR(x)", value = 3.0000, step = 0.1),
      
      numericInput("alpha", "alpha:", value = 0.0500, step = 0.01),
      
      hr(),
      h4("Possible HA Range"),
      helpText("Set the range and step size to generate the table and curve."),
      numericInput("ha_min", "Min HA:", value = 7.5000, step = 0.1),
      numericInput("ha_max", "Max HA:", value = 12.5000, step = 0.1),
      numericInput("ha_step", "Step Size:", value = 0.2000, step = 0.1)
    ),
    
    mainPanel(
      # Top Section: Critical Values on the left, Power Curve on the right
      fluidRow(
        column(4,
               wellPanel(
                 h4("2 Tail Critical Values"),
                 tableOutput("crit_table")
               )
        ),
        column(8,
               h4("Power Curve", align = "center"),
               plotOutput("power_plot", height = "400px")
        )
      ),
      
      hr(),
      
      # Bottom Section: Power Table centered, no scrollbars (full display)
      fluidRow(
        column(12, align = "center",
               h4("Power Table"),
               div(style = "width: 80%;", # Limit the width slightly so it looks neat when centered
                   DTOutput("power_table")
               )
        )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Helper formatter for 4 decimal places
  fmt <- function(x) sprintf("%.4f", x)
  
  # Reactive calculations for base parameters
  base_calc <- reactive({
    req(input$h0, input$n, input$val, input$alpha)
    
    sd_x <- if(input$sd_var == "sd") input$val else sqrt(input$val)
    se <- sd_x / sqrt(input$n)
    
    # 2-tail z critical value
    z_crit <- qnorm(1 - input$alpha / 2)
    
    xL <- input$h0 - (z_crit * se)
    xU <- input$h0 + (z_crit * se)
    
    list(se = se, z_crit = z_crit, xL = xL, xU = xU)
  })
  
  # Render Critical Values Table
  output$crit_table <- renderTable({
    b <- base_calc()
    data.frame(
      Value = c("z", "xL", "xU"),
      `2 Tail` = fmt(c(b$z_crit, b$xL, b$xU)),
      check.names = FALSE
    )
  })
  
  # Reactive calculations for the Power Dataframe
  power_data <- reactive({
    req(input$ha_min, input$ha_max, input$ha_step)
    b <- base_calc()
    
    # Create sequence of HA values
    ha_seq <- seq(input$ha_min, input$ha_max, by = input$ha_step)
    
    z_lower <- (b$xL - ha_seq) / b$se
    z_upper <- (b$xU - ha_seq) / b$se
    
    # Beta = P(z_lower < Z < z_upper)
    beta <- pnorm(z_upper) - pnorm(z_lower)
    power <- 1 - beta
    
    data.frame(
      `Possible HA` = ha_seq,
      `z(lower)` = z_lower,
      `z(upper)` = z_upper,
      Beta = beta,
      Power = power,
      check.names = FALSE
    )
  })
  
  # Render Power Table (Full display, centered, no scrollbars)
  output$power_table <- renderDT({
    df <- power_data()
    # Format everything to 4 decimal places for the table display
    df_fmt <- df
    for(col in names(df_fmt)) {
      df_fmt[[col]] <- fmt(df_fmt[[col]])
    }
    
    datatable(df_fmt, 
              options = list(
                dom = 't',        # Only show the table (no search bar, no info text)
                paging = FALSE,   # Disable pagination to show all rows at once
                ordering = FALSE  # Disable sorting to keep the HA sequence in order
              ), 
              rownames = FALSE) %>%
      formatStyle(columns = names(df_fmt), textAlign = 'center')
  })
  
  # Render Power Curve Plot
  output$power_plot <- renderPlot({
    df <- power_data()
    
    ggplot(df, aes(x = `Possible HA`, y = Power)) +
      # Dotted red line matching the Excel chart
      geom_line(color = "red", linetype = "dotted", size = 1) +
      # Points matching the Excel chart
      geom_point(color = "black", size = 2) +
      # Text labels with 2 decimal places, positioned slightly above the points
      geom_text(aes(label = sprintf("%.2f", Power)), 
                vjust = -1, hjust = 0.5, size = 4, fontface = "bold") +
      scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.1), labels = fmt) +
      scale_x_continuous(breaks = df$`Possible HA`) +
      theme_minimal() +
      labs(x = "", y = "") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)