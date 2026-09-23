# Install required packages if you haven't already: 
# install.packages(c("shiny", "ggplot2", "DT"))

library(shiny)
library(ggplot2)
library(DT)

# --- UI Definition ---
ui <- fluidPage(
  titlePanel("One-Way ANOVA Calculator"),
  
  # Inject CSS to guarantee all table headers are centered
  tags$head(
    tags$style(HTML("
      table.dataTable thead th {
        text-align: center !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Input"),
      helpText("Enter data for each group. Values can be separated by commas, spaces, or newlines."),
      
      # Allow user to add or reduce the number of groups
      numericInput("num_groups", "Number of Groups:", value = 5, min = 2, max = 20, step = 1),
      
      # Dynamic UI for group inputs
      uiOutput("group_inputs_ui"),
      
      hr(),
      h4("Settings"),
      numericInput("conf_level", "Confidence Level (%):", value = 95.0000, step = 1),
      selectInput("adj_method", "Post-Hoc Adjustment Method:", 
                  choices = c("Bonferroni" = "bonferroni", 
                              "Tukey (HSD)" = "tukey", 
                              "Holm" = "holm"),
                  selected = "bonferroni")
    ),
    
    mainPanel(
      h4("Summary Table"),
      DTOutput("summary_table"),
      
      br(),
      h4("ANOVA Table"),
      DTOutput("anova_table"),
      
      br(),
      h4("Least Square Means Table"),
      DTOutput("lsmean_table"),
      
      br(),
      h4("Least Square Means with Confidence Interval"),
      plotOutput("lsmean_plot", height = "400px"),
      
      br(),
      h4("Post-Hoc Pairwise Comparisons"),
      DTOutput("posthoc_table")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Helper formatter for 4 decimal places (Vectorized safely)
  fmt <- function(x) {
    ifelse(is.na(x) | is.nan(x) | is.infinite(x), "#DIV/0!", sprintf("%.4f", x))
  }
  p_fmt <- function(x) {
    ifelse(is.na(x) | is.nan(x), "#DIV/0!", 
           ifelse(x < 0.0001, "< 0.0001", sprintf("%.4f", x)))
  }
  
  # Common DT options to center all content and headers
  dt_opts <- list(
    dom = 't', 
    paging = FALSE, 
    ordering = FALSE,
    columnDefs = list(list(className = 'dt-center', targets = "_all"))
  )
  
  # 1. Dynamic UI for group data entry
  output$group_inputs_ui <- renderUI({
    k <- input$num_groups
    req(k >= 2)
    
    # Pre-fill with some dummy data to prevent empty errors on launch
    dummy_data <- list(
      "63, 65, 56, 100, 88",
      "69, 65, 62, 91, 78",
      "55, 58, 49, 85, 70",
      "72, 68, 65, 95, 82",
      "60, 62, 54, 88, 75"
    )
    
    inputs <- lapply(1:k, function(i) {
      val <- if(i <= 5) dummy_data[[i]] else ""
      textAreaInput(paste0("group_", i), paste("Group", i, "Data:"), value = val, rows = 2)
    })
    do.call(tagList, inputs)
  })
  
  # 2. Parse data and compute base ANOVA statistics
  anova_data <- reactive({
    k <- input$num_groups
    req(k >= 2)
    
    data_list <- list()
    for (i in 1:k) {
      txt <- input[[paste0("group_", i)]]
      if (is.null(txt) || trimws(txt) == "") {
        data_list[[i]] <- numeric(0)
      } else {
        vals <- as.numeric(unlist(strsplit(trimws(txt), "[,\\s\\n]+", perl = TRUE)))
        data_list[[i]] <- vals[!is.na(vals)]
      }
    }
    
    ns <- sapply(data_list, length)
    valid_groups <- which(ns > 0)
    
    req(length(valid_groups) >= 2)
    
    means <- sapply(data_list, mean)
    vars <- sapply(data_list, var)
    sums <- sapply(data_list, sum)
    sds <- sapply(data_list, sd)
    
    # Calculate overall metrics for all valid data points
    all_data <- unlist(data_list[valid_groups])
    total_var <- if(length(all_data) > 1) var(all_data) else NA
    total_sd <- if(length(all_data) > 1) sd(all_data) else NA
    
    total_n <- sum(ns)
    grand_mean <- sum(sums) / total_n
    
    ssa_i <- ns * (means - grand_mean)^2
    sse_i <- (ns - 1) * vars
    
    SSA <- sum(ssa_i, na.rm = TRUE)
    SSE <- sum(sse_i, na.rm = TRUE)
    SST <- SSA + SSE
    
    df_A <- length(valid_groups) - 1
    df_E <- total_n - length(valid_groups)
    df_T <- total_n - 1
    
    MSA <- if(df_A > 0) SSA / df_A else NA
    MSE <- if(df_E > 0) SSE / df_E else NA
    
    F_stat <- MSA / MSE
    p_val <- if(!is.na(F_stat)) 1 - pf(F_stat, df_A, df_E) else NA
    eta2 <- if(!is.na(SST) && SST > 0) SSA / SST else NA
    
    list(
      k = k, valid_groups = valid_groups, ns = ns, sums = sums, means = means, vars = vars, sds = sds,
      ssa_i = ssa_i, sse_i = sse_i, 
      SSA = SSA, SSE = SSE, SST = SST,
      df_A = df_A, df_E = df_E, df_T = df_T,
      MSA = MSA, MSE = MSE, F_stat = F_stat, p_val = p_val, eta2 = eta2,
      grand_mean = grand_mean, total_var = total_var, total_sd = total_sd
    )
  })
  
  # 3. Render Summary Table
  output$summary_table <- renderDT({
    d <- anova_data()
    req(d)
    
    df <- data.frame(
      Group = paste("Group", 1:d$k),
      N = d$ns,
      Sum = fmt(d$sums),
      Mean = fmt(d$means),
      Variance = fmt(d$vars),
      SD = fmt(d$sds),
      `SSA(i)` = fmt(d$ssa_i),
      `Pool(VAR:SD)` = fmt(d$sse_i),
      check.names = FALSE
    )
    
    # Updated totals to include Variance and SD
    totals <- data.frame(
      Group = "Totals/Calc",
      N = sum(d$ns),
      Sum = fmt(sum(d$sums)),
      Mean = fmt(d$grand_mean),
      Variance = fmt(d$total_var),
      SD = fmt(d$total_sd),
      `SSA(i)` = fmt(d$SSA),
      `Pool(VAR:SD)` = fmt(d$SSE),
      check.names = FALSE
    )
    
    datatable(rbind(df, totals), options = dt_opts, rownames = FALSE)
  })
  
  # 4. Render ANOVA Table
  output$anova_table <- renderDT({
    d <- anova_data()
    req(d)
    
    df <- data.frame(
      Source = c("SSA", "SSE", "SST"),
      SS = fmt(c(d$SSA, d$SSE, d$SST)),
      df = c(d$df_A, d$df_E, d$df_T),
      MS = c(fmt(d$MSA), fmt(d$MSE), ""),
      F = c(fmt(d$F_stat), "", ""),
      `p-value` = c(p_fmt(d$p_val), "", ""),
      `Partial η2` = c(fmt(d$eta2), "", ""),
      check.names = FALSE
    )
    
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # 5. Render Least Square Means Table
  lsmeans_calc <- reactive({
    d <- anova_data()
    req(d)
    alpha <- 1 - (input$conf_level / 100)
    
    # 根據 Excel 的邏輯：使用各組獨立的 SD 與自由度 (n_i - 1)
    se <- d$sds / sqrt(d$ns)
    t_crit <- qt(1 - alpha/2, d$ns - 1)
    
    lcl <- d$means - t_crit * se
    ucl <- d$means + t_crit * se
    
    data.frame(
      Group = 1:d$k,
      Mean = d$means,
      SE = se,
      LCL = lcl,
      UCL = ucl
    )
  })
  
  output$lsmean_table <- renderDT({
    ls <- lsmeans_calc()
    req(ls)
    
    df <- data.frame(
      Group = paste("Group", ls$Group),
      Mean = fmt(ls$Mean),
      SE = fmt(ls$SE),
      LCL = fmt(ls$LCL),
      UCL = fmt(ls$UCL)
    )
    
    datatable(df, options = dt_opts, rownames = FALSE)
  })
  
  # 6. Render Least Square Means Plot
  output$lsmean_plot <- renderPlot({
    ls <- lsmeans_calc()
    req(ls)
    
    ls_clean <- ls[!is.na(ls$Mean), ]
    
    ggplot(ls_clean, aes(x = factor(Group), y = Mean)) +
      geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black") +
      geom_point(size = 3, color = "steelblue", shape = 15) +
      theme_bw() +
      labs(title = "Mean and Confidence Interval", x = "Group", y = "Mean") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # 7. Render Post-Hoc Pairwise Comparison Table
  output$posthoc_table <- renderDT({
    d <- anova_data()
    req(d, length(d$valid_groups) >= 2)
    
    alpha <- 1 - (input$conf_level / 100)
    method <- input$adj_method
    
    pairs <- combn(d$valid_groups, 2)
    m <- ncol(pairs)
    k_valid <- length(d$valid_groups)
    
    comp_names <- c()
    diffs <- c()
    ses <- c()
    ts <- c()
    dfs <- c()
    probs <- c()
    t_crit_adjs <- c()
    
    for (i in 1:m) {
      g1 <- pairs[1, i]
      g2 <- pairs[2, i]
      comp_names <- c(comp_names, paste0("G", g1, "-G", g2))
      
      mean_diff <- d$means[g1] - d$means[g2]
      
      # 根據 Excel 邏輯：SE 使用總體 MSE，但 df 使用獨立樣本的 n1 + n2 - 2
      se_diff <- sqrt(d$MSE * (1/d$ns[g1] + 1/d$ns[g2]))
      df_pair <- d$ns[g1] + d$ns[g2] - 2
      
      t_val <- mean_diff / se_diff
      prob <- 2 * (1 - pt(abs(t_val), df_pair))
      
      # 針對不同方法計算臨界 t 值
      if (method == "bonferroni") {
        t_c <- qt(1 - alpha / (2 * m), df_pair)
      } else if (method == "tukey") {
        t_c <- qtukey(1 - alpha, k_valid, df_pair) / sqrt(2)
      } else if (method == "holm") {
        t_c <- qt(1 - alpha / (2 * m), df_pair) 
      } else {
        t_c <- qt(1 - alpha / 2, df_pair)
      }
      
      diffs <- c(diffs, mean_diff)
      ses <- c(ses, se_diff)
      ts <- c(ts, t_val)
      dfs <- c(dfs, df_pair)
      probs <- c(probs, prob)
      t_crit_adjs <- c(t_crit_adjs, t_c)
    }
    
    adj_probs <- p.adjust(probs, method = if(method == "tukey") "none" else method)
    if (method == "tukey") {
      # Tukey p-value calculation
      adj_probs <- ptukey(abs(ts) * sqrt(2), k_valid, dfs, lower.tail = FALSE)
    }
    
    # 計算 Adjusted CI
    adj_lcl <- diffs - t_crit_adjs * ses
    adj_ucl <- diffs + t_crit_adjs * ses
    
    # Create final data frame (Removed Adjusted Prob.)
    df <- data.frame(
      Comparison = comp_names,
      Difference = fmt(diffs),
      `SE(Diff)` = fmt(ses),
      `t-value` = fmt(ts),
      df = fmt(dfs), 
      `Unadjusted p-value` = p_fmt(probs),
      `Adj LCL` = fmt(adj_lcl),
      `Adj UCL` = fmt(adj_ucl),
      `Adjusted p-value` = p_fmt(adj_probs),
      check.names = FALSE
    )
    
    prefix <- switch(method,
                     "bonferroni" = "Bon-Adj",
                     "tukey" = "Tukey-Adj",
                     "holm" = "Holm-Adj")
    
    colnames(df)[7:8] <- c(paste(prefix, "LCL"), paste(prefix, "UCL"))
    
    datatable(df, options = dt_opts, rownames = FALSE)
  })
}

# Run the App
shinyApp(ui = ui, server = server)