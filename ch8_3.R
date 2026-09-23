# Load required packages
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(readxl)
library(emmeans)

# --- UI Definition ---
ui <- fluidPage(
  withMathJax(),
  titlePanel("Two-Way ANOVA Calculator"),
  
  # Inject CSS to guarantee all table headers and cells are centered
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
      
      hr(),
      h4("3. Model Settings"),
      checkboxInput("interaction", "Include Interaction Term", value = TRUE),
      numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0.001, max = 0.20, step = 0.01),
      selectInput("adj_method", "Pairwise Adjustment Method:", 
                  choices = c("Bonferroni" = "bonferroni", "Tukey" = "tukey", "Holm" = "holm"),
                  selected = "bonferroni")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data & Summary", 
                 br(), 
                 h4("Data Preview"), 
                 DTOutput("data_preview"),
                 br(), 
                 h4("Summary Table by Factor A"), 
                 DTOutput("summary_A"),
                 br(), 
                 h4("Summary Table by Factor B"), 
                 DTOutput("summary_B"),
                 uiOutput("summary_int_ui")
        ),
        tabPanel("ANOVA Table",
                 br(), 
                 h4("Two-Way ANOVA Table"), 
                 DTOutput("anova_table")
        ),
        tabPanel("LS Means",
                 br(), 
                 h4("Least Squares Means (Factor A)"), 
                 DTOutput("lsmean_A"),
                 br(), 
                 h4("Least Squares Means (Factor B)"), 
                 DTOutput("lsmean_B"),
                 uiOutput("lsmean_int_ui")
        ),
        tabPanel("Plots",
                 br(), 
                 h4("LS Means Plot: Factor A"), 
                 plotOutput("plot_A", height = "350px"),
                 br(), 
                 h4("LS Means Plot: Factor B"), 
                 plotOutput("plot_B", height = "350px"),
                 uiOutput("plot_int_ui")
        ),
        tabPanel("Pairwise Comparisons",
                 br(), 
                 h4("Pairwise Comparisons (Factor A)"), 
                 DTOutput("pair_A"),
                 br(), 
                 h4("Pairwise Comparisons (Factor B)"), 
                 DTOutput("pair_B"),
                 uiOutput("pair_int_ui")
        )
      )
    )
  )
)

# --- Server Logic ---
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
        if(ncol(df) == 1) { 
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
    
    if(!is.null(df)) {
      df <- na.omit(df)
    }
    
    return(df)
  })
  
  output$var_selectors <- renderUI({
    df <- raw_data()
    req(df)
    
    cols <- names(df)
    
    tagList(
      selectInput("var_A", "Factor A:", choices = c("Select variable..." = "", cols), selected = ""),
      selectInput("var_B", "Factor B:", choices = c("Select variable..." = "", cols), selected = ""),
      selectInput("var_DV", "Dependent Variable:", choices = c("Select variable..." = "", cols), selected = "")
    )
  })
  
  proc_data <- reactive({
    df <- raw_data()
    req(df)
    req(input$var_A != "")
    req(input$var_B != "")
    req(input$var_DV != "")
    
    chk_A <- input$var_A %in% names(df)
    chk_B <- input$var_B %in% names(df)
    chk_DV <- input$var_DV %in% names(df)
    
    if (!chk_A) return(NULL)
    if (!chk_B) return(NULL)
    if (!chk_DV) return(NULL)
    
    df[[input$var_A]] <- as.factor(df[[input$var_A]])
    df[[input$var_B]] <- as.factor(df[[input$var_B]])
    df[[input$var_DV]] <- as.numeric(df[[input$var_DV]])
    
    df <- df[!is.na(df[[input$var_DV]]), ]
    return(df)
  })
  
  output$data_preview <- renderDT({
    req(proc_data())
    datatable(head(proc_data(), 50), 
              options = list(pageLength = 5, dom = 'tip', columnDefs = list(list(className = 'dt-center', targets = "_all"))), 
              rownames = FALSE)
  })
  
  output$summary_A <- renderDT({
    req(proc_data())
    df <- proc_data()
    
    res <- df %>% 
      group_by(!!sym(input$var_A)) %>% 
      summarise(N = n(), Mean = mean(!!sym(input$var_DV)), SD = sd(!!sym(input$var_DV))) %>% 
      mutate(Mean = fmt(Mean), SD = fmt(SD))
    
    datatable(res, options = dt_opts, rownames = FALSE)
  })
  
  output$summary_B <- renderDT({
    req(proc_data())
    df <- proc_data()
    
    res <- df %>% 
      group_by(!!sym(input$var_B)) %>% 
      summarise(N = n(), Mean = mean(!!sym(input$var_DV)), SD = sd(!!sym(input$var_DV))) %>% 
      mutate(Mean = fmt(Mean), SD = fmt(SD))
    
    datatable(res, options = dt_opts, rownames = FALSE)
  })
  
  output$summary_int_ui <- renderUI({
    req(input$interaction)
    tagList(
      br(), 
      h4("Summary Table by Cell"), 
      DTOutput("summary_AB")
    )
  })
  
  output$summary_AB <- renderDT({
    req(proc_data())
    req(input$interaction)
    
    df <- proc_data()
    
    res <- df %>% 
      group_by(!!sym(input$var_A), !!sym(input$var_B)) %>% 
      summarise(N = n(), Mean = mean(!!sym(input$var_DV)), SD = sd(!!sym(input$var_DV)), .groups = "drop") %>% 
      mutate(Mean = fmt(Mean), SD = fmt(SD))
    
    datatable(res, options = dt_opts, rownames = FALSE)
  })
  
  model_obj <- reactive({
    req(proc_data())
    df <- proc_data()
    
    v_A <- paste0("`", input$var_A, "`")
    v_B <- paste0("`", input$var_B, "`")
    v_DV <- paste0("`", input$var_DV, "`")
    
    if (input$interaction) {
      f_str <- paste0(v_DV, " ~ ", v_A, " * ", v_B)
    } else {
      f_str <- paste0(v_DV, " ~ ", v_A, " + ", v_B)
    }
    
    lm(as.formula(f_str), data = df)
  })
  
  output$anova_table <- renderDT({     
    req(model_obj())     
    aov_res <- anova(model_obj())          
    SS <- aov_res$`Sum Sq`
  SS_total <- sum(SS)
  
  eta2 <- SS / SS_total
  eta2[length(eta2)] <- NA
  
  res_df <- data.frame(
    Source = gsub("Residuals", "Error (Residuals)", rownames(aov_res)),
    df = aov_res$Df,
    `Sum Sq` = fmt(aov_res$`Sum Sq`),
    `Mean Sq` = fmt(aov_res$`Mean Sq`),
    `F value` = fmt(aov_res$`F value`),
    `p-value` = p_fmt(aov_res$`Pr(>F)`),
    `Eta Squared` = fmt(eta2),
    check.names = FALSE
  )
  
  datatable(res_df, options = dt_opts, rownames = FALSE)
  })
  
  get_lsmeans <- function(mod, spec) {
    f_spec <- as.formula(paste("~", spec))
    em <- as.data.frame(emmeans(mod, specs = f_spec))
    
    names(em)[names(em) == "emmean"] <- "LS Mean"
    names(em)[names(em) == "lower.CL"] <- "LCL"
    names(em)[names(em) == "upper.CL"] <- "UCL"
    
    num_cols <- sapply(em, is.numeric)
    em[, num_cols] <- lapply(em[, num_cols], function(x) fmt(x))
    
    return(em)
  }
  
  output$lsmean_A <- renderDT({ 
    req(model_obj())
    datatable(get_lsmeans(model_obj(), paste0("`", input$var_A, "`")), options = dt_opts, rownames = FALSE) 
  })
  
  output$lsmean_B <- renderDT({ 
    req(model_obj())
    datatable(get_lsmeans(model_obj(), paste0("`", input$var_B, "`")), options = dt_opts, rownames = FALSE) 
  })
  
  output$lsmean_int_ui <- renderUI({
    req(input$interaction)
    tagList(
      br(), 
      h4("Least Squares Means by Cell"), 
      DTOutput("lsmean_AB")
    )
  })
  
  output$lsmean_AB <- renderDT({
    req(model_obj())
    req(input$interaction)
    
    s_val <- paste0("`", input$var_A, "`: `", input$var_B, "`")
    datatable(get_lsmeans(model_obj(), s_val), options = dt_opts, rownames = FALSE)
  })
  
  plot_lsmeans <- function(mod, spec, x_var, color_var = NULL) {
    f_spec <- as.formula(paste("~", spec))
    em <- as.data.frame(emmeans(mod, specs = f_spec))
    
    x_col <- paste0("`", x_var, "`")
    
    p <- ggplot(em, aes_string(x = x_col, y = "emmean")) + 
      theme_bw() + 
      labs(y = "Least Squares Mean") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    if (is.null(color_var)) {
      p <- p + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) + 
        geom_point(size = 3, color = "steelblue", shape = 15) + 
        geom_line(aes(group = 1), color = "steelblue", linetype = "dashed")
    } else {
      c_col <- paste0("`", color_var, "`")
      
      p <- p + geom_errorbar(aes_string(ymin = "lower.CL", ymax = "upper.CL", color = c_col), 
                             width = 0.2, position = position_dodge(0.2)) + 
        geom_point(aes_string(color = c_col), size = 3, shape = 15, position = position_dodge(0.2)) + 
        geom_line(aes_string(group = c_col, color = c_col), 
                  position = position_dodge(0.2), linetype = "dashed")
    }
    return(p)
  }
  
  output$plot_A <- renderPlot({ 
    req(model_obj())
    plot_lsmeans(model_obj(), paste0("`", input$var_A, "`"), input$var_A) 
  })
  
  output$plot_B <- renderPlot({ 
    req(model_obj())
    plot_lsmeans(model_obj(), paste0("`", input$var_B, "`"), input$var_B) 
  })
  
  output$plot_int_ui <- renderUI({
    req(input$interaction)
    tagList(
      br(), 
      h4("Interaction Plot"), 
      plotOutput("plot_AB", height = "400px")
    )
  })
  
  output$plot_AB <- renderPlot({
    req(model_obj())
    req(input$interaction)
    
    s_val <- paste0("`", input$var_A, "`: `", input$var_B, "`")
    plot_lsmeans(model_obj(), s_val, input$var_A, input$var_B) + labs(title = "Interaction Plot")
  })
  
  # --- Advanced Custom Pairwise Comparisons with Independent DF ---
  get_custom_pairwise <- function(mod, df_data, spec, by_var = NULL) {
    alpha <- input$alpha
    adj_method <- input$adj_method
    
    if (is.null(by_var)) {
      f_spec <- as.formula(paste("~", paste0("`", spec, "`")))
      em_obj <- emmeans(mod, specs = f_spec)
      pw <- as.data.frame(pairs(em_obj, adjust = "none"))
      
      lvl <- levels(em_obj@grid[[spec]])
      if (is.null(lvl)) lvl <- unique(em_obj@grid[[spec]])
      
      # Directly compute N from data for main effect comparisons
      ns <- sapply(lvl, function(x) sum(df_data[[spec]] == x, na.rm = TRUE))
      pairs_idx <- combn(length(lvl), 2)
      
      custom_dfs <- numeric(ncol(pairs_idx))
      for (i in 1:ncol(pairs_idx)) {
        n1 <- ns[pairs_idx[1, i]]
        n2 <- ns[pairs_idx[2, i]]
        custom_dfs[i] <- n1 + n2 - 2
      }
      
      m <- length(custom_dfs)
      ts <- pw$t.ratio
      diffs <- pw$estimate
      ses <- pw$SE
      
      unadj_p <- 2 * (1 - pt(abs(ts), custom_dfs))
      
      # Perform adjustments
      if (adj_method == "bonferroni") {
        adj_p <- p.adjust(unadj_p, method = "bonferroni")
        t_crit_adjs <- qt(1 - alpha / (2 * m), custom_dfs)
      } else if (adj_method == "holm") {
        adj_p <- p.adjust(unadj_p, method = "holm")
        t_crit_adjs <- qt(1 - alpha / (2 * m), custom_dfs)
      } else if (adj_method == "tukey") {
        k_total <- length(lvl)
        adj_p <- ptukey(abs(ts) * sqrt(2), k_total, custom_dfs, lower.tail = FALSE)
        t_crit_adjs <- qtukey(1 - alpha, k_total, custom_dfs) / sqrt(2)
      } else {
        adj_p <- unadj_p
        t_crit_adjs <- qt(1 - alpha / 2, custom_dfs)
      }
      
      adj_lcl <- diffs - t_crit_adjs * ses
      adj_ucl <- diffs + t_crit_adjs * ses
      
      out_df <- data.frame(
        Comparison = pw$contrast,
        Difference = fmt(diffs),
        `SE(Diff)` = fmt(ses),
        `t-value` = fmt(ts),
        df = fmt(custom_dfs),
        `Unadjusted p-value` = p_fmt(unadj_p),
        `Adj LCL` = fmt(adj_lcl),
        `Adj UCL` = fmt(adj_ucl),
        `Adjusted p-value` = p_fmt(adj_p),
        check.names = FALSE
      )
      
    } else {
      # For Simple Main Effects
      f_spec <- as.formula(paste("pairwise ~", paste0("`", spec, "`"), "|", paste0("`", by_var, "`")))
      em_res <- emmeans(mod, specs = f_spec)
      pw <- as.data.frame(em_res$contrasts)
      em_obj <- em_res$emmeans
      
      lvl_spec <- levels(em_obj@grid[[spec]])
      if (is.null(lvl_spec)) lvl_spec <- unique(em_obj@grid[[spec]])
      
      lvl_by <- levels(em_obj@grid[[by_var]])
      if (is.null(lvl_by)) lvl_by <- unique(em_obj@grid[[by_var]])
      
      pairs_idx <- combn(length(lvl_spec), 2)
      num_pairs <- ncol(pairs_idx)
      
      custom_dfs <- numeric(nrow(pw))
      
      # Step 1: Compute custom df by summing cell sample sizes exactly
      row_idx <- 1
      for (b in lvl_by) {
        ns <- sapply(lvl_spec, function(x) {
          sum(df_data[[spec]] == x & df_data[[by_var]] == b, na.rm = TRUE)
        })
        for (i in 1:num_pairs) {
          n1 <- ns[pairs_idx[1, i]]
          n2 <- ns[pairs_idx[2, i]]
          custom_dfs[row_idx] <- n1 + n2 - 2
          row_idx <- row_idx + 1
        }
      }
      
      ts <- pw$t.ratio
      diffs <- pw$estimate
      ses <- pw$SE
      unadj_p <- 2 * (1 - pt(abs(ts), custom_dfs))
      
      adj_p <- numeric(nrow(pw))
      t_crit_adjs <- numeric(nrow(pw))
      
      # Step 2: Apply adjustment methods within each split group level
      row_idx <- 1
      for (b in lvl_by) {
        idx <- row_idx:(row_idx + num_pairs - 1)
        m_sub <- num_pairs
        
        p_sub <- unadj_p[idx]
        t_sub <- ts[idx]
        df_sub <- custom_dfs[idx]
        
        if (adj_method == "bonferroni") {
          adj_p[idx] <- p.adjust(p_sub, method = "bonferroni")
          t_crit_adjs[idx] <- qt(1 - alpha / (2 * m_sub), df_sub)
        } else if (adj_method == "holm") {
          adj_p[idx] <- p.adjust(p_sub, method = "holm")
          t_crit_adjs[idx] <- qt(1 - alpha / (2 * m_sub), df_sub)
        } else if (adj_method == "tukey") {
          k_sub <- length(lvl_spec)
          adj_p[idx] <- ptukey(abs(t_sub) * sqrt(2), k_sub, df_sub, lower.tail = FALSE)
          t_crit_adjs[idx] <- qtukey(1 - alpha, k_sub, df_sub) / sqrt(2)
        } else {
          adj_p[idx] <- p_sub
          t_crit_adjs[idx] <- qt(1 - alpha / 2, df_sub)
        }
        
        row_idx <- row_idx + num_pairs
      }
      
      adj_lcl <- diffs - t_crit_adjs * ses
      adj_ucl <- diffs + t_crit_adjs * ses
      
      out_df <- data.frame(
        Comparison = paste0("[", pw[[by_var]], "] ", pw$contrast),
        Difference = fmt(diffs),
        `SE(Diff)` = fmt(ses),
        `t-value` = fmt(ts),
        df = fmt(custom_dfs),
        `Unadjusted p-value` = p_fmt(unadj_p),
        `Adj LCL` = fmt(adj_lcl),
        `Adj UCL` = fmt(adj_ucl),
        `Adjusted p-value` = p_fmt(adj_p),
        check.names = FALSE
      )
    }
    
    # Rename adjusted CI columns appropriately based on selected method
    prefix <- switch(adj_method, "bonferroni" = "Bon-Adj", "tukey" = "Tukey-Adj", "holm" = "Holm-Adj", "none" = "Unadj")
    colnames(out_df)[7:8] <- c(paste(prefix, "LCL"), paste(prefix, "UCL"))
    
    return(out_df)
  }
  
  output$pair_A <- renderDT({ 
    req(model_obj(), proc_data())
    res <- get_custom_pairwise(model_obj(), proc_data(), input$var_A)
    datatable(res, options = dt_opts, rownames = FALSE) 
  })
  
  output$pair_B <- renderDT({ 
    req(model_obj(), proc_data())
    res <- get_custom_pairwise(model_obj(), proc_data(), input$var_B)
    datatable(res, options = dt_opts, rownames = FALSE) 
  })
  
  output$pair_int_ui <- renderUI({
    req(input$interaction)
    tagList(
      br(), 
      h4(paste("Simple Main Effects: Compare", input$var_A, "within", input$var_B)), 
      DTOutput("pair_A_within_B"),
      br(), 
      h4(paste("Simple Main Effects: Compare", input$var_B, "within", input$var_A)), 
      DTOutput("pair_B_within_A")
    )
  })
  
  output$pair_A_within_B <- renderDT({     req(model_obj(), proc_data(), input$interaction)
    res <- get_custom_pairwise(model_obj(), proc_data(), input$var_A, input$var_B)
    datatable(res, options = dt_opts, rownames = FALSE)
  })
  
  output$pair_B_within_A <- renderDT({     req(model_obj(), proc_data(), input$interaction)
    res <- get_custom_pairwise(model_obj(), proc_data(), input$var_B, input$var_A)
    datatable(res, options = dt_opts, rownames = FALSE)
  })
}

# Run the App
shinyApp(ui = ui, server = server)