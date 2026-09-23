# Load required packages
library(shiny)
library(ggplot2)
library(DT)

# Define User Interface (UI)
ui <- fluidPage(
  titlePanel("Descriptive Statistics (Mean, Variance, and SD)"),
  
  tabsetPanel(
    # ==========================================
    # Tab 1: Summary Table Mean, Variance, SD Worksheet
    # ==========================================
    tabPanel("Summary Table",
             sidebarLayout(
               sidebarPanel(
                 helpText("Enter EITHER Frequency or Relative Frequency/Probability. Values can be separated by commas, spaces, or new lines (by row)."),
                 
                 textAreaInput("tab1_x", 
                               "Value or Class Midpoint (x):", 
                               value = "0\n1\n2\n3\n4\n5", 
                               rows = 6),
                 
                 textAreaInput("tab1_f", 
                               "Frequency or Probability:", 
                               value = "0.1\n0.4\n0.2\n0.2\n0.07\n0.03", 
                               rows = 6),
                 
                 hr(),
                 helpText("Note: The Probability Density plot is automatically generated based on the inputs.")
               ),
               mainPanel(
                 h4("Summary Table Mean, Variance, and SD Worksheet"),
                 DTOutput("tab1_table"),
                 br(),
                 fluidRow(
                   column(4, 
                          h4("Statistics"),
                          DTOutput("tab1_stats")
                   ),
                   column(8, 
                          h4("Probability Density Function"),
                          plotOutput("tab1_plot", height = "300px")
                   )
                 )
               )
             )
    ),
    
    # ==========================================
    # Tab 2: Weighted Mean, Variance, and SD Worksheet
    # ==========================================
    tabPanel("Weighted Summary Table",
             sidebarLayout(
               sidebarPanel(
                 helpText("Enter corresponding Values and Weights. Values can be separated by commas, spaces, or new lines (by row)."),
                 
                 textAreaInput("tab2_x", 
                               "Value:", 
                               value = "84\n78\n84\n72\n78\n96\n95", 
                               rows = 7),
                 
                 textAreaInput("tab2_w", 
                               "Weight:", 
                               value = "0.1\n0.1\n0.1\n0.1\n0.3\n0.1\n0.2", 
                               rows = 7)
               ),
               mainPanel(
                 h4("Weighted Mean, Variance, and SD Worksheet"),
                 DTOutput("tab2_table"),
                 br(),
                 column(5,
                        h4("Statistics"),
                        DTOutput("tab2_stats")
                 )
               )
             )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Formatting helper for exactly 4 decimal places
  fmt_4dec <- function(x) {
    ifelse(is.na(x), "", sprintf("%.4f", x))
  }
  
  # Helper to parse text input handling commas, spaces, and newlines
  parse_input <- function(text_input) {
    cleaned_text <- gsub("[,\\n]", " ", text_input)
    split_text <- unlist(strsplit(trimws(cleaned_text), "\\s+"))
    vals <- as.numeric(split_text)
    vals[!is.na(vals)]
  }
  
  # ==========================================
  # Tab 1 Logic
  # ==========================================
  data_tab1 <- reactive({
    x <- parse_input(input$tab1_x)
    f <- parse_input(input$tab1_f)
    
    if (length(x) == 0 || length(x) != length(f)) return(NULL)
    
    # Calculate relative frequency / probability universally
    p_x <- f / sum(f)
    x2 <- x^2
    x_px <- x * p_x
    x2_px <- x2 * p_x
    
    mean_val <- sum(x_px)
    var_val <- sum(x2_px) - (mean_val^2)
    sd_val <- sqrt(var_val)
    
    list(x = x, f = f, p_x = p_x, x2 = x2, x_px = x_px, x2_px = x2_px, 
         mean = mean_val, var = var_val, sd = sd_val)
  })
  
  output$tab1_table <- renderDT({
    res <- data_tab1()
    if (is.null(res)) return(datatable(data.frame(Error="Lengths of x and frequencies must match and be numeric.")))
    
    df <- data.frame(
      x = as.character(res$x),
      Input = fmt_4dec(res$f),
      p_x = fmt_4dec(res$p_x),
      x2 = as.character(res$x2),
      x_px = fmt_4dec(res$x_px),
      x2_px = fmt_4dec(res$x2_px)
    )
    colnames(df) <- c("Value or Class Midpoint (x)", "Frequency or Probability", 
                      "p(x)", "x^2", "x*p(x)", "x^2*p(x)")
    
    sum_row <- data.frame(
      `Value or Class Midpoint (x)` = "Sum",
      `Frequency or Probability` = fmt_4dec(sum(res$f)),
      `p(x)` = fmt_4dec(sum(res$p_x)),
      `x^2` = "",
      `x*p(x)` = fmt_4dec(sum(res$x_px)),
      `x^2*p(x)` = fmt_4dec(sum(res$x2_px)),
      check.names = FALSE
    )
    
    df_display <- rbind(df, sum_row)
    datatable(df_display, options = list(dom = 't', ordering = FALSE, pageLength = -1), rownames = FALSE)
  })
  
  output$tab1_stats <- renderDT({
    res <- data_tab1()
    if (is.null(res)) return(NULL)
    
    df <- data.frame(
      Statistic = c("Mean", "Variance", "SD"),
      Value = c(fmt_4dec(res$mean), fmt_4dec(res$var), fmt_4dec(res$sd))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  output$tab1_plot <- renderPlot({
    res <- data_tab1()
    if (is.null(res)) return(NULL)
    
    df <- data.frame(x = factor(res$x, levels = res$x), p_x = res$p_x)
    
    ggplot(df, aes(x = x, y = p_x)) +
      geom_bar(stat = "identity", fill = "lightgray", color = "black", width = 0.5) +
      theme_minimal() +
      labs(title = "Probability Density Function", x = "x", y = "p(x)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # ==========================================
  # Tab 2 Logic
  # ==========================================
  data_tab2 <- reactive({
    x <- parse_input(input$tab2_x)
    w <- parse_input(input$tab2_w)
    
    if (length(x) == 0 || length(x) != length(w)) return(NULL)
    
    n <- length(x)
    sum_w <- sum(w)
    
    x_w <- x * w
    mean_val <- sum(x_w) / sum_w
    
    w_x_mean2 <- w * (x - mean_val)^2
    sum_w_x_mean2 <- sum(w_x_mean2)
    
    # Apply sample adjustment for weighted variance matching the Excel standard (n / (n-1))
    if (n > 1 && sum_w > 0) {
      var_val <- (sum_w_x_mean2 / sum_w) * (n / (n - 1))
    } else {
      var_val <- NA
    }
    
    sd_val <- sqrt(var_val)
    
    list(x = x, w = w, x_w = x_w, w_x_mean2 = w_x_mean2, n = n,
         mean = mean_val, var = var_val, sd = sd_val)
  })
  
  output$tab2_table <- renderDT({
    res <- data_tab2()
    if (is.null(res)) return(datatable(data.frame(Error="Lengths of Value and Weight must match and be numeric.")))
    
    df <- data.frame(
      Value = as.character(res$x),
      Weight = as.character(res$w),
      xw = fmt_4dec(res$x_w),
      wxmean2 = fmt_4dec(res$w_x_mean2)
    )
    colnames(df) <- c("Value", "Weight", "x(w)", "w(x-mean)^2")
    
    sum_row <- data.frame(
      Value = "Sum",
      Weight = as.character(sum(res$w)),
      `x(w)` = fmt_4dec(sum(res$x_w)),
      `w(x-mean)^2` = fmt_4dec(sum(res$w_x_mean2)),
      check.names = FALSE
    )
    
    count_row <- data.frame(
      Value = "Count",
      Weight = as.character(res$n),
      `x(w)` = "",
      `w(x-mean)^2` = "",
      check.names = FALSE
    )
    
    df_display <- rbind(df, sum_row, count_row)
    datatable(df_display, options = list(dom = 't', ordering = FALSE, pageLength = -1), rownames = FALSE)
  })
  
  output$tab2_stats <- renderDT({
    res <- data_tab2()
    if (is.null(res)) return(NULL)
    
    df <- data.frame(
      Statistic = c("Mean", "Variance", "SD"),
      Value = c(fmt_4dec(res$mean), fmt_4dec(res$var), fmt_4dec(res$sd))
    )
    datatable(df, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)