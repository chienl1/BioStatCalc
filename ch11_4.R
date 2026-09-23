# =========================================================================
# Ensure the following packages are installed:
# install.packages(c("shiny", "ggplot2", "DT", "dplyr", "readxl", "car", "pROC"))
# =========================================================================

library(shiny);
library(ggplot2);
library(DT);
library(dplyr);
library(readxl);
library(car);
library(pROC);

# ==========================================
# Helper Functions and Global Parameters
# ==========================================
fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x) | is.infinite(x), "", sprintf("%.4f", as.numeric(x)));
};

p_fmt <- function(x) {
  ifelse(is.na(x), "", ifelse(as.numeric(x) < 0.0001, "< 0.0001", sprintf("%.4f", as.numeric(x))));
};

dt_opts <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE, 
  columnDefs = list(list(className = 'dt-center', targets = "_all"))
);

# Custom Runs Test Function (Check residual independence)
runs_test_custom <- function(x) {
  x_bin <- as.numeric(x > median(x));
  n1 <- sum(x_bin == 1);
  n2 <- sum(x_bin == 0);
  runs <- 1;
  if(length(x_bin) > 1) {
    for(i in 2:length(x_bin)) {
      if(x_bin[i] != x_bin[i-1]) runs <- runs + 1;
    }
  }
  mu <- (2 * n1 * n2) / (n1 + n2) + 1;
  var <- (2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / ((n1 + n2)^2 * (n1 + n2 - 1));
  if(var <= 0 || is.na(var)) return(list(runs = runs, z = NA, p = NA));
  z <- (runs - mu) / sqrt(var);
  p_val <- 2 * (1 - pnorm(abs(z)));
  return(list(runs = runs, z = z, p = p_val));
};

# ==========================================
# UI Interface
# ==========================================
ui <- fluidPage(
  titlePanel("Logistic Regression Calculator (with Cross-Table Support)"),
  
  tags$head(
    tags$style(HTML("table.dataTable thead th, table.dataTable tbody td { text-align: center !important; }
                     h4 { color: #2c3e50; font-weight: bold; border-bottom: 2px solid #eee; padding-bottom: 5px; }"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data Input"),
      radioButtons("input_type", "Choose Input Method:",
                   choices = c("Copy & Paste Data" = "paste", "Upload CSV / XLSX File" = "upload")),
      conditionalPanel(
        condition = "input.input_type == 'paste'",
        textAreaInput("pasted_data", "Paste your data here (Tab or Comma separated):", value = "", rows = 6)
      ),
      conditionalPanel(
        condition = "input.input_type == 'upload'",
        fileInput("file_data", "Upload File (.csv, .xlsx)", 
                  accept = c(".csv", ".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
      ),
      
      hr(),
      h4("2. Variable Selection"),
      uiOutput("var_selectors"),
      uiOutput("dv_event_selector"),
      uiOutput("cat_var_selector"),
      uiOutput("ref_level_selectors"),
      
      hr(),
      h4("3. Interaction Terms"),
      checkboxInput("add_interaction", "Add 2-Way Interactions", value = FALSE),
      conditionalPanel(
        condition = "input.add_interaction == true",
        uiOutput("interaction_ui")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("1. Data & Profile", 
                 br(), h4("Response Profile"), DTOutput("resp_profile"),
                 br(), h4("Data Preview (Expanded)"), DTOutput("data_preview")
        ),
        tabPanel("2. Model Fit & Global Test",
                 br(), h4("Model Status"), htmlOutput("model_status"),
                 br(), h4("Model Fit Statistics"), DTOutput("fit_stats"),
                 br(), h4("Testing Global Null Hypothesis: BETA=0"), DTOutput("global_test")
        ),
        tabPanel("3. Type 3 Analysis & Parameters",
                 br(), h4("Type 3 Analysis of Effects"), DTOutput("type3_table"),
                 br(), h4("Analysis of Maximum Likelihood Estimates"), DTOutput("param_table"),
                 br(), h4("Odds Ratio Estimates"), DTOutput("or_table")
        ),
        tabPanel("4. Plots",
                 br(),
                 fluidRow(
                   column(6, h4("ROC Curve"), plotOutput("roc_plot", height = "450px")),
                   column(6, h4("Forest Plot (Odds Ratios)"), plotOutput("forest_plot", height = "450px"))
                 )
        ),
        tabPanel("5. Diagnostics",
                 br(), h4("Independence Assumption (Runs Test on Deviance Residuals)"), DTOutput("runs_test_table"),
                 br(), h4("Linearity Assumption (Box-Tidwell Test for Continuous IVs)"), htmlOutput("bt_status"), DTOutput("bt_test_table")
        )
      )
    )
  )
);

# ==========================================
# Server Logic
# ==========================================
server <- function(input, output, session) {
  
  # Read Data
  raw_data <- reactive({
    df <- NULL;
    if (input$input_type == "paste") {
      req(input$pasted_data != "");
      tryCatch({
        df <- read.table(text = input$pasted_data, header = TRUE, sep = "\t", stringsAsFactors = FALSE);
        if (ncol(df) == 1) { df <- read.csv(text = input$pasted_data, header = TRUE, stringsAsFactors = FALSE); }
      }, error = function(e) { NULL; });
    } else {
      req(input$file_data);
      ext <- tools::file_ext(input$file_data$name);
      tryCatch({
        if (ext == "csv") { df <- read.csv(input$file_data$datapath, stringsAsFactors = FALSE); }
        else if (ext %in% c("xls", "xlsx")) { df <- as.data.frame(readxl::read_excel(input$file_data$datapath)); }
      }, error = function(e) { NULL; });
    }
    return(df);
  });
  
  # Select DV, IV, and Frequency variables
  output$var_selectors <- renderUI({
    df <- raw_data(); req(df); cols <- names(df);
    tagList(
      selectInput("var_DV", "Dependent Variable (Y):", choices = c("Select variable..." = "", cols), selected = ""),
      selectInput("var_IV", "Independent Variables (X):", choices = cols, multiple = TRUE),
      selectInput("var_freq", "Frequency/Count Variable (Optional for Cross-Table):", choices = c("None (Individual Data)" = "", cols), selected = "")
    );
  });
  
  # Select Event of Interest (DV levels)
  output$dv_event_selector <- renderUI({     df <- raw_data(); req(df); req(input$var_DV);
  lvls <- unique(na.omit(df[[input$var_DV]]));
  if(length(lvls) < 2) return(helpText("DV must have at least 2 unique levels."));
  selectInput("dv_event", "Event of Interest (Target = 1):", choices = lvls, selected = lvls[1]);
  });
  
  # Select Categorical IVs
  output$cat_var_selector <- renderUI({
    req(input$var_IV);
    selectInput("var_IV_cat", "Select Categorical Independent Variables:", choices = input$var_IV, multiple = TRUE);
  });
  
  # Select Reference Level for each Categorical IV
  output$ref_level_selectors <- renderUI({
    req(input$var_IV_cat); df <- raw_data(); req(df);
    lapply(input$var_IV_cat, function(col) {
      lvls <- as.character(unique(na.omit(df[[col]])));
      selectInput(paste0("ref_", col), paste0("Reference Level for ", col, ":"), choices = lvls, selected = lvls[1]);
    });
  });
  
  # Interaction Terms UI
  output$interaction_ui <- renderUI({
    req(input$var_IV);
    if (length(input$var_IV) < 2) return(helpText("Select >= 2 IVs to create interactions."));
    pairs <- combn(input$var_IV, 2);
    pair_names <- apply(pairs, 2, paste, collapse = ":");
    checkboxGroupInput("int_terms", "Select 2-Way Interactions:", choices = pair_names);
  });
  
  # Data Expansion Processing (Expand Cross-Table Counts to Individual Level)
  proc_data <- reactive({
    df <- raw_data(); req(df); req(input$var_DV); req(input$dv_event); req(length(input$var_IV) > 0);
    
    # Expand Frequency Data (Uncount)
    if (!is.null(input$var_freq) && input$var_freq != "") {
      freq_col <- input$var_freq;
      req(freq_col %in% names(df));
      freqs <- as.numeric(df[[freq_col]]);
      freqs[is.na(freqs)] <- 0;
      freqs <- round(freqs); # Ensure frequencies are integers
      df <- df[rep(seq_len(nrow(df)), freqs), ];
    }
    
    selected_cols <- c(input$var_DV, input$var_IV);
    if (!all(selected_cols %in% names(df))) return(NULL);
    
    # Remove NAs
    df <- df[complete.cases(df[, selected_cols]), ];
    req(nrow(df) > 0);
    
    # Process DV (0 or 1)
    df[["Y_MODEL"]] <- ifelse(as.character(df[[input$var_DV]]) == as.character(input$dv_event), 1, 0);
    
    # Process IV
    cat_vars <- input$var_IV_cat;
    if (is.null(cat_vars)) cat_vars <- character(0);
    
    for (col in input$var_IV) {
      if (col %in% cat_vars) {
        ref_val <- input[[paste0("ref_", col)]];
        if (!is.null(ref_val)) {
          df[[col]] <- relevel(factor(as.character(df[[col]])), ref = as.character(ref_val));
        } else {
          df[[col]] <- as.factor(df[[col]]);
        }
      } else {
        df[[col]] <- as.numeric(df[[col]]);
      }
    }
    return(df);
  });
  
  # Build Model
  model_obj <- reactive({
    df <- proc_data(); req(df);
    ivs <- paste0("`", input$var_IV, "`", collapse = " + ");
    f_str <- paste("Y_MODEL ~", ivs);
    if (input$add_interaction && length(input$int_terms) > 0) {
      int_formatted <- sapply(input$int_terms, function(x) {
        vars <- strsplit(x, ":")[[1]];
        paste0("`", vars[1], "`:`", vars[2], "`");
      });
      f_str <- paste(f_str, "+", paste(int_formatted, collapse = " + "));
    }
    glm(as.formula(f_str), data = df, family = binomial(link = "logit"));
  });
  
  # ================= Outputs =================
  
  # 1) Response Profile
  output$resp_profile <- renderDT({
    df <- proc_data(); req(df);
    tbl <- table(df[[input$var_DV]]);
    res_df <- data.frame(Outcome = names(tbl), Frequency = as.integer(tbl));
    # Put Event of Interest in the first row
    res_df <- res_df[order(res_df$Outcome == as.character(input$dv_event), decreasing = TRUE), ];
    rownames(res_df) <- NULL;
    datatable(res_df, options = dt_opts, rownames = FALSE);
  });
  
  output$data_preview <- renderDT({
    req(proc_data());
    datatable(head(proc_data(), 50), options = list(pageLength = 10, dom = 'tip', className = 'dt-center'), rownames = FALSE);
  });
  
  # 2) & 3) Model Status & Separation
  output$model_status <- renderUI({
    mod <- model_obj(); req(mod);
    conv_txt <- ifelse(mod$converged, "<span style='color:green;'>Model Converged successfully.</span>", 
                       "<span style='color:red;'>Model DID NOT Converge.</span>");
    
    se <- summary(mod)$coefficients[, 2];
    fitted_p <- fitted(mod);
    sep_txt <- "<span style='color:green;'>No obvious quasi or semi-quasi separation detected.</span>";
    if (any(se > 20) || any(fitted_p < 1e-5) || any(fitted_p > 1 - 1e-5)) {
      sep_txt <- "<span style='color:red;'>Warning: Possible quasi-complete or complete separation detected (Standard errors are excessively large or fitted probabilities are exactly 0 or 1).</span>";
    }
    HTML(paste(conv_txt, sep_txt, sep = "<br><br>"));
  });
  
  # 4) Model Fit Statistics
  output$fit_stats <- renderDT({
    mod <- model_obj(); req(mod);
    n <- length(mod$y);
    k <- length(coef(mod));
    
    l0 <- mod$null.deviance;
    aic0 <- l0 + 2 * 1;
    sc0 <- l0 + log(n) * 1;
    
    l1 <- mod$deviance;
    aic1 <- mod$aic;
    sc1 <- l1 + log(n) * k;
    
    df <- data.frame(
      Criterion = c("AIC", "SC (BIC)", "-2 Log L"),
      `Intercept Only` = c(fmt(aic0), fmt(sc0), fmt(l0)),
      `Intercept and Covariates` = c(fmt(aic1), fmt(sc1), fmt(l1)),
      check.names = FALSE
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  # 5) Global Null Hypothesis
  output$global_test <- renderDT({
    mod <- model_obj(); req(mod);
    lr_stat <- mod$null.deviance - mod$deviance;
    df <- mod$df.null - mod$df.residual;
    p_val <- pchisq(lr_stat, df, lower.tail = FALSE);
    
    res_df <- data.frame(
      Test = "Likelihood Ratio",
      `Chi-Square` = fmt(lr_stat),
      DF = df,
      `Pr > ChiSq` = p_fmt(p_val),
      check.names = FALSE
    );
    datatable(res_df, options = dt_opts, rownames = FALSE);
  });
  
  # 6) Type 3 Analysis
  output$type3_table <- renderDT({
    mod <- model_obj(); req(mod);
    res_df <- data.frame(Effect=character(), DF=integer(), `Wald Chi-Square`=character(), `Pr > ChiSq`=character(), check.names=FALSE);
    tryCatch({
      t3 <- car::Anova(mod, type = "III", test.statistic = "Wald");
      res_df <- data.frame(
        Effect = rownames(t3),
        DF = t3$Df,
        `Wald Chi-Square` = fmt(t3$Chisq),
        `Pr > ChiSq` = p_fmt(t3$`Pr(>Chisq)`),
        check.names = FALSE
      );
      res_df <- res_df[res_df$Effect != "(Intercept)", ]; # Usually do not display intercept
    }, error = function(e) {
      res_df <- data.frame(Error = "Could not compute Type 3 tests. (Check for complete separation or aliased coefficients)");
    });
    datatable(res_df, options = dt_opts, rownames = FALSE);
  });
  
  # 7) Parameter Estimates
  output$param_table <- renderDT({
    mod <- model_obj(); req(mod);
    coefs <- summary(mod)$coefficients;
    wald_chi <- (coefs[, 1] / coefs[, 2])^2;
    
    res_df <- data.frame(
      Parameter = rownames(coefs),
      DF = 1,
      Estimate = fmt(coefs[, 1]),
      `Standard Error` = fmt(coefs[, 2]),
      `Wald Chi-Square` = fmt(wald_chi),
      `Pr > ChiSq` = p_fmt(coefs[, 4]),
      check.names = FALSE
    );
    res_df$Parameter <- gsub("`", "", res_df$Parameter);
    datatable(res_df, options = dt_opts, rownames = FALSE);
  });
  
  # 8) Odds Ratio Estimates
  output$or_table <- renderDT({
    mod <- model_obj(); req(mod);
    est <- coef(mod);
    # Use Wald CI for speed and standard default
    ci <- suppressMessages(confint.default(mod)); 
    or <- exp(est);
    or_l <- exp(ci[, 1]);
    or_u <- exp(ci[, 2]);
    
    res_df <- data.frame(
      Effect = rownames(ci),
      `Point Estimate` = fmt(or),
      `95% Wald CI Lower` = fmt(or_l),
      `95% Wald CI Upper` = fmt(or_u),
      check.names = FALSE
    );
    # Remove OR for intercept
    res_df <- res_df[res_df$Effect != "(Intercept)", ];
    res_df$Effect <- gsub("`", "", res_df$Effect);
    datatable(res_df, options = dt_opts, rownames = FALSE);
  });
  
  # 9) Forest Plot
  output$forest_plot <- renderPlot({
    mod <- model_obj(); req(mod);
    est <- coef(mod);
    ci <- suppressMessages(confint.default(mod));
    
    df_plot <- data.frame(
      Term = rownames(ci),
      OR = exp(est),
      LCL = exp(ci[, 1]),
      UCL = exp(ci[, 2])
    );
    df_plot <- df_plot[df_plot$Term != "(Intercept)", ];
    req(nrow(df_plot) > 0);
    df_plot$Term <- gsub("`", "", df_plot$Term);
    
    ggplot(df_plot, aes(x = OR, y = reorder(Term, OR))) +
      geom_point(size = 3, color = "steelblue") +
      geom_errorbarh(aes(xmin = LCL, xmax = UCL), height = 0.2, color = "steelblue") +
      geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
      scale_x_log10() +
      labs(title = "Odds Ratios with 95% Wald CI", x = "Odds Ratio (Log Scale)", y = "Effect") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"));
  });
  
  # 10) ROC Plot
  output$roc_plot <- renderPlot({
    mod <- model_obj(); req(mod);
    df <- proc_data(); req(df);
    
    roc_obj <- roc(df$Y_MODEL, fitted(mod), quiet = TRUE);
    auc_val <- auc(roc_obj);
    
    plot(roc_obj, main = "ROC Curve", col = "steelblue", lwd = 2);
    legend("bottomright", legend = sprintf("AUC = %.4f", auc_val), bty = "n", cex = 1.2, text.col = "red", text.font = 2);
  });
  
  # 11) Diagnostics
  output$runs_test_table <- renderDT({
    mod <- model_obj(); req(mod);
    res <- residuals(mod, type = "deviance");
    r_test <- runs_test_custom(res);
    
    df <- data.frame(
      Test = "Runs Test (Deviance Residuals)",
      Runs = r_test$runs,
      Z = fmt(r_test$z),
      `p-value` = p_fmt(r_test$p),
      check.names = FALSE
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$bt_status <- renderUI({
    df <- proc_data(); req(df);
    cat_vars <- input$var_IV_cat;
    num_vars <- setdiff(input$var_IV, cat_vars);
    if(length(num_vars) == 0) return(HTML("<span style='color:gray;'>No continuous independent variables selected. Box-Tidwell test is skipped.</span>"));
    return(HTML(""));
  });
  
  output$bt_test_table <- renderDT({
    df <- proc_data(); req(df);
    cat_vars <- input$var_IV_cat;
    if(is.null(cat_vars)) cat_vars <- character(0);
    num_vars <- setdiff(input$var_IV, cat_vars);
    req(length(num_vars) > 0);
    
    df_bt <- df;
    bt_terms <- c();
    for(col in num_vars) {
      val <- df_bt[[col]];
      # If values <= 0 exist, shift to > 0 before applying log
      if(any(val <= 0)) { val <- val - min(val) + 1e-5; }
      ln_col <- paste0(col, "_ln");
      df_bt[[ln_col]] <- val * log(val);
      bt_terms <- c(bt_terms, ln_col);
    }
    
    ivs <- paste0("`", input$var_IV, "`", collapse = " + ");
    bt_ivs <- paste0("`", bt_terms, "`", collapse = " + ");
    f_str <- paste("Y_MODEL ~", ivs, "+", bt_ivs);
    
    bt_mod <- glm(as.formula(f_str), data = df_bt, family = binomial(link = "logit"));
    coefs <- summary(bt_mod)$coefficients;
    
    res_df <- data.frame(Continuous_Variable=character(), Estimate=character(), `Pr > ChiSq`=character(), check.names=FALSE);
    
    for(i in 1:length(num_vars)) {
      term <- bt_terms[i];
      orig_term <- num_vars[i];
      idx <- grep(paste0("^`?", term, "`?$"), rownames(coefs));
      if(length(idx) > 0) {
        p_val <- coefs[idx, 4];
        est <- coefs[idx, 1];
        res_df <- rbind(res_df, data.frame(
          `Continuous Variable` = orig_term,
          `Interaction Term` = paste0(orig_term, " * ln(", orig_term, ")"),
          Estimate = fmt(est),
          `Pr > ChiSq` = p_fmt(p_val),
          check.names = FALSE
        ));
      }
    }
    datatable(res_df, options = dt_opts, rownames = FALSE);
  });
  
}

shinyApp(ui = ui, server = server);