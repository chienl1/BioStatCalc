# Install required packages: 
# install.packages(c("shiny", "ggplot2", "DT", "dplyr", "readxl", "lmtest"))
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(lmtest)

ui <- fluidPage(
  withMathJax(),
  titlePanel("Multiple Linear Regression Calculator"),
  
  tags$head(
    tags$style(HTML("table.dataTable thead th, table.dataTable tbody td { text-align: center !important; }"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Input"),
      helpText("Note: Raw data must include variable names (headers) in the first row."),
      
      radioButtons("input_type", "Choose Input Method:",
                   choices = c("Copy & Paste Data" = "paste", 
                               "Upload CSV / XLSX File" = "upload")),
      
      conditionalPanel(
        condition = "input.input_type == 'paste'",
        textAreaInput("pasted_data", "Paste your data here (Tab or Comma separated):", 
                      value = "", rows = 6)
      ),
      
      conditionalPanel(
        condition = "input.input_type == 'upload'",
        fileInput("file_data", "Upload File (.csv, .xlsx)", 
                  accept = c(".csv", ".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
      ),
      
      hr(),
      h4("2. Variable Selection"),
      uiOutput("var_selectors"),
      uiOutput("cat_var_selector"),
      
      hr(),
      h4("3. Interaction Terms"),
      checkboxInput("add_interaction", "Add Interaction Terms", value = FALSE),
      conditionalPanel(
        condition = "input.add_interaction == true",
        uiOutput("interaction_ui")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", 
                 br(), 
                 h4("Data Preview"), 
                 DTOutput("data_preview")
        ),
        tabPanel("Model Assessment & ANOVA",
                 br(), 
                 h4("Model Assessment (Summary)"), 
                 DTOutput("model_summary"),
                 br(), 
                 h4("ANOVA Table"), 
                 DTOutput("anova_table")
        ),
        tabPanel("Parameter Estimates",
                 br(), 
                 h4("Parameter Estimates"), 
                 DTOutput("param_table")
        ),
        tabPanel("Diagnostics",
                 br(), 
                 h4("Model Diagnostic Tests"), 
                 DTOutput("diag_table"),
                 br(),
                 h4("Diagnostic Plots"),
                 fluidRow(
                   column(6, plotOutput("plot_resid_fit", height = "350px")),
                   column(6, plotOutput("plot_qq", height = "350px"))
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  fmt <- function(x) {
    ifelse(is.na(x) | is.nan(x), "", sprintf("%.4f", as.numeric(x)))
  }
  
  p_fmt <- function(x) {
    ifelse(is.na(x), "", ifelse(as.numeric(x) < 0.0001, "< 0.0001", sprintf("%.4f", as.numeric(x))))
  }
  
  dt_opts <- list(
    dom = 't', 
    paging = FALSE, 
    ordering = FALSE, 
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  )
  
  raw_data <- reactive({
    df <- NULL
    if (input$input_type == "paste") {
      req(input$pasted_data != "")
      tryCatch({
        df <- read.table(text = input$pasted_data, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        if (ncol(df) == 1) { 
          df <- read.csv(text = input$pasted_data, header = TRUE, stringsAsFactors = FALSE) 
        }
      }, error = function(e) { NULL })
    } else {
      req(input$file_data)
      ext <- tools::file_ext(input$file_data$name)
      tryCatch({
        if (ext == "csv") { 
          df <- read.csv(input$file_data$datapath, stringsAsFactors = FALSE) 
        } else if (ext %in% c("xls", "xlsx")) { 
          df <- as.data.frame(readxl::read_excel(input$file_data$datapath)) 
        }
      }, error = function(e) { NULL })
    }
    
    return(df)
  })
  
  output$var_selectors <- renderUI({
    df <- raw_data()
    req(df)
    cols <- names(df)
    
    tagList(
      selectInput("var_DV", "Dependent Variable (Y):", choices = c("Select variable..." = "", cols), selected = ""),
      selectInput("var_IV", "Independent Variables (X):", choices = cols, multiple = TRUE)
    )
  })
  
  output$cat_var_selector <- renderUI({
    req(input$var_IV)
    selectInput("var_IV_cat", "Select Categorical Independent Variables (Optional):", 
                choices = input$var_IV, multiple = TRUE)
  })
  
  output$interaction_ui <- renderUI({
    req(input$var_IV)
    if (length(input$var_IV) < 2) {
      return(helpText("Please select at least 2 Independent Variables to create interactions."))
    }
    
    pairs <- combn(input$var_IV, 2)
    pair_names <- apply(pairs, 2, paste, collapse = ":")
    
    checkboxGroupInput("int_terms", "Select 2-Way Interactions:", choices = pair_names)
  })
  
  proc_data <- reactive({
    df <- raw_data()
    req(df)
    req(input$var_DV != "")
    req(length(input$var_IV) > 0)
    
    selected_cols <- c(input$var_DV, input$var_IV)
    if (!all(selected_cols %in% names(df))) return(NULL)
    
    # Process Dependent Variable (Must be numeric)
    df[[input$var_DV]] <- as.numeric(df[[input$var_DV]])
    
    # Process Independent Variables
    cat_vars <- input$var_IV_cat
    if (is.null(cat_vars)) cat_vars <- character(0)
    
    for (col in input$var_IV) {
      if (col %in% cat_vars) {
        df[[col]] <- as.factor(df[[col]])
      } else {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    # Keep only complete cases for selected variables
    df <- df[complete.cases(df[, selected_cols]), ]
    return(df)
  })
  
  output$data_preview <- renderDT({
    req(proc_data())
    datatable(head(proc_data(), 50), 
              options = list(pageLength = 10, dom = 'tip', columnDefs = list(list(className = 'dt-center', targets = "_all"))), 
              rownames = FALSE)
  })
  
  model_obj <- reactive({
    req(proc_data())
    df <- proc_data()
    
    dv <- paste0("`", input$var_DV, "`")
    ivs <- paste0("`", input$var_IV, "`", collapse = " + ")
    
    f_str <- paste(dv, "~", ivs)
    
    if (input$add_interaction && length(input$int_terms) > 0) {
      int_formatted <- sapply(input$int_terms, function(x) {
        vars <- strsplit(x, ":")[[1]]
        paste0("`", vars[1], "`:`", vars[2], "`")
      })
      f_str <- paste(f_str, "+", paste(int_formatted, collapse = " + "))
    }
    
    lm(as.formula(f_str), data = df)
  })
  
  output$model_summary <- renderDT({
    req(model_obj())
    mod_sum <- summary(model_obj())
    
    res_df <- data.frame(
      R = fmt(sqrt(mod_sum$r.squared)),
      `R Squared` = fmt(mod_sum$r.squared),
      `Adjusted R Squared` = fmt(mod_sum$adj.r.squared),
      `Residual Std. Error` = fmt(mod_sum$sigma),
      check.names = FALSE
    )
    
    datatable(res_df, options = dt_opts, rownames = FALSE)
  })
  
  output$anova_table <- renderDT({
    req(model_obj())
    aov_res <- anova(model_obj())
    
    res_df <- data.frame(
      Source = gsub("Residuals", "Error (Residuals)", rownames(aov_res)),
      df = aov_res$Df,
      `Sum Sq` = fmt(aov_res$`Sum Sq`),
      `Mean Sq` = fmt(aov_res$`Mean Sq`),
      `F value` = fmt(aov_res$`F value`),
      `p-value` = p_fmt(aov_res$`Pr(>F)`),
      check.names = FALSE
    )
    
    datatable(res_df, options = dt_opts, rownames = FALSE)
  })
  
  output$param_table <- renderDT({
    req(model_obj())
    mod <- model_obj()
    mod_sum <- summary(mod)
    coefs <- mod_sum$coefficients
    res_df_mod <- mod_sum$df[2] 
    ci_bounds <- suppressMessages(confint(mod, level = 0.95))
    
    # Get model matrix (automatically handles dummy variables)
    X <- model.matrix(mod)[, -1, drop = FALSE]
    vifs <- rep(NA, ncol(X))
    
    if (ncol(X) > 1) {
      tryCatch({
        vifs <- diag(solve(cor(X)))
      }, error = function(e) {
        vifs <- rep(NA, ncol(X))
      })
    }
    
    tols <- ifelse(!is.na(vifs), 1 / vifs, NA)
    
    vifs_full <- c(NA, vifs)
    tols_full <- c(NA, tols)
    
    param_df <- data.frame(
      Term = rownames(coefs),
      df = res_df_mod,
      `Estimate (Beta)` = fmt(coefs[, 1]),
      SE = fmt(coefs[, 2]),
      `95% CI Lower` = fmt(ci_bounds[, 1]),
      `95% CI Upper` = fmt(ci_bounds[, 2]),
      `t-value` = fmt(coefs[, 3]),
      `p-value` = p_fmt(coefs[, 4]),
      Tolerance = fmt(tols_full),
      VIF = fmt(vifs_full),
      check.names = FALSE
    )
    
    param_df$Term <- gsub("`", "", param_df$Term)
    
    datatable(param_df, options = dt_opts, rownames = FALSE)
  })
  
  output$diag_table <- renderDT({
    req(model_obj())
    mod <- model_obj()
    
    dw <- lmtest::dwtest(mod)
    sw <- shapiro.test(resid(mod))
    reset <- tryCatch(lmtest::resettest(mod), error = function(e) list(statistic = NA, p.value = NA))
    bp <- lmtest::bptest(mod)
    
    diag_df <- data.frame(
      Assumption = c("Independence", "Normality", "Linearity", "Equal Variance"),
      Test = c("Durbin-Watson", "Shapiro-Wilk", "Ramsey RESET", "Breusch-Pagan"),
      Statistic = c(fmt(dw$statistic), fmt(sw$statistic), fmt(reset$statistic), fmt(bp$statistic)),
      `p-value` = c(p_fmt(dw$p.value), p_fmt(sw$p.value), p_fmt(reset$p.value), p_fmt(bp$p.value)),
      check.names = FALSE
    )
    
    datatable(diag_df, options = dt_opts, rownames = FALSE)
  })
  
  output$plot_resid_fit <- renderPlot({
    req(model_obj())
    df_plot <- data.frame(Fitted = fitted(model_obj()), Residuals = resid(model_obj()))
    
    ggplot(df_plot, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "steelblue", size = 2) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      theme_bw() +
      labs(title = "Residuals vs Fitted (Linearity & Equal Variance)", x = "Fitted Values", y = "Residuals") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  output$plot_qq <- renderPlot({
    req(model_obj())
    df_plot <- data.frame(Residuals = resid(model_obj()))
    
    ggplot(df_plot, aes(sample = Residuals)) +
      stat_qq(color = "steelblue") +
      stat_qq_line(color = "red", linetype = "dashed") +
      theme_bw() +
      labs(title = "Normal Q-Q Plot (Normality)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
}

shinyApp(ui = ui, server = server)