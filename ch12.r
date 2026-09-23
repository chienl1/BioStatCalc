library(shiny);
library(DT);
library(readxl);

# ==========================================
# Helper Functions
# ==========================================
fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x), "", sprintf("%.4f", as.numeric(x)));
};

p_fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x), "", 
         ifelse(as.numeric(x) < 0.0001, "< 0.0001", sprintf("%.4f", as.numeric(x))));
};

get_es_size <- function(w) {
  if (is.na(w)) return("");
  if (w >= 0.5) return("Large");
  if (w >= 0.3) return("Moderate");
  return("Small");
};

get_or_size <- function(or_val) {
  if (is.na(or_val) || is.infinite(or_val)) return("");
  or_adj <- max(or_val, 1/or_val);
  if (or_adj >= 9.0) return("Large");
  if (or_adj >= 3.47) return("Moderate");
  return("Small");
};

dt_opts <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all"))
);

# ==========================================
# User Interface
# ==========================================
ui <- fluidPage(
  titlePanel("Categorical Data Analysis & Chi-Square Tests"),
  
  tags$head(
    tags$style(HTML("
      table.dataTable thead th, table.dataTable tbody td { text-align: center !important; }
      h4 { color: #2c3e50; font-weight: bold; margin-top: 20px;}
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. Data Input"),
      helpText("Input your count data (frequencies) below."),
      
      radioButtons("input_type", "Input Method:",
                   choices = c("Copy & Paste Data" = "paste", 
                               "Upload CSV / XLSX File" = "upload")),
      
      conditionalPanel(
        condition = "input.input_type == 'paste'",
        textAreaInput("pasted_data", "Paste Counts (sep by space, comma, or newline):", 
                      value = "14, 31\n21, 49", rows = 6),
        fluidRow(
          column(6, numericInput("num_rows", "Rows:", value = 2, min = 1)),
          column(6, numericInput("num_cols", "Columns:", value = 2, min = 1))
        ),
        helpText("The values will be filled into the matrix row by row.")
      ),
      
      conditionalPanel(
        condition = "input.input_type == 'upload'",
        fileInput("file_data", "Upload File (.csv, .xlsx)", 
                  accept = c(".csv", ".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
        checkboxInput("has_header", "First row/col are labels (will be removed)", value = FALSE)
      ),
      
      hr(),
      h4("2. Goodness-of-Fit Settings"),
      radioButtons("gof_dist", "Expected Distribution:", choices = c("Uniform", "User-Specified"), inline = TRUE),
      conditionalPanel(
        condition = "input.gof_dist == 'User-Specified'",
        textInput("gof_probs", "Probabilities (sep by space, comma, or newline):", "0.5625, 0.1875, 0.1875, 0.0625")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Preview",
                 br(),
                 h4("Data Matrix Preview"),
                 p("This is how your data is interpreted. Make sure the dimensions match the test requirements."),
                 DTOutput("data_preview")
        ),
        tabPanel("1. Goodness-of-Fit",
                 br(), h4("Working Table"), DTOutput("gof_table"),
                 br(), fluidRow(
                   column(5, h4("Summary"), DTOutput("gof_summary")),
                   column(7, h4("Effect Size"), DTOutput("gof_es"))
                 )
        ),
        tabPanel("2. McNemar's Test",
                 br(), h4("McNemar's Test Table"), DTOutput("mc_table"),
                 br(), fluidRow(
                   column(5, h4("Summary"), DTOutput("mc_summary")),
                   column(7, h4("Effect Size"), DTOutput("mc_es"))
                 )
        ),
        tabPanel("3. Fisher & Yates (2x2)",
                 br(), h4("Contingency Table"), DTOutput("fy_table"),
                 br(), h4("Fisher's Exact Test"), fluidRow(
                   column(4, DTOutput("fisher_working")),
                   column(4, DTOutput("fisher_summary")),
                   column(4, DTOutput("fisher_es"))
                 ),
                 br(), h4("Yates' Continuity-Corrected Chi-Square"), fluidRow(
                   column(5, DTOutput("yates_summary")),
                   column(7, DTOutput("yates_es"))
                 )
        ),
        tabPanel("4. Contingency Table Analysis",
                 br(), h4("OBSERVED VALUES"), DTOutput("rxc_obs"),
                 h4("PEARSON EXPECTED VALUES"), DTOutput("rxc_exp"),
                 h4("PEARSON CHI-SQUARE VALUE"), DTOutput("rxc_pearson_cell"),
                 fluidRow(
                   column(5, h4("Pearson Chi-Square Summary"), DTOutput("rxc_pearson_sum")),
                   column(7, h4("Pearson Effect Size"), DTOutput("rxc_pearson_es"))
                 ),
                 h4("LIKELIHOOD RATIO CHI-SQUARE VALUE"), DTOutput("rxc_lr_cell"),
                 fluidRow(
                   column(5, h4("Likelihood Ratio Chi-Square Summary"), DTOutput("rxc_lr_sum")),
                   column(7, h4("Likelihood Ratio Effect Size"), DTOutput("rxc_lr_es"))
                 )
        )
      )
    )
  )
);

# ==========================================
# Server logic
# ==========================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------
  # Core Data Matrix Parsing
  # ----------------------------------------------------
  data_matrix <- reactive({
    mat <- NULL;
    if (input$input_type == "paste") {
      req(input$pasted_data != "");
      # Parse space, comma, newline
      vals <- as.numeric(unlist(strsplit(trimws(input$pasted_data), "[,\\s\\n]+", perl = TRUE)));
      vals <- vals[!is.na(vals)];
      req(length(vals) > 0);
      req(input$num_rows > 0, input$num_cols > 0);
      
      # Pad with NA if not enough values
      expected_len <- input$num_rows * input$num_cols;
      if (length(vals) < expected_len) {
        vals <- c(vals, rep(NA, expected_len - length(vals)));
      }
      mat <- matrix(vals[1:expected_len], nrow = input$num_rows, ncol = input$num_cols, byrow = TRUE);
      
    } else {
      req(input$file_data);
      ext <- tools::file_ext(input$file_data$name);
      tryCatch({
        df <- if(ext == "csv") {
          read.csv(input$file_data$datapath, header = FALSE, stringsAsFactors = FALSE)
        } else {
          as.data.frame(readxl::read_excel(input$file_data$datapath, col_names = FALSE))
        };
        if (input$has_header) {
          df <- df[-1, -1, drop = FALSE];
        }
        mat <- as.matrix(df);
        suppressWarnings(mode(mat) <- "numeric");
        # Remove entirely NA rows/cols
        mat <- mat[!apply(is.na(mat), 1, all), !apply(is.na(mat), 2, all), drop = FALSE];
      }, error = function(e) { mat <- NULL; });
    }
    
    return(mat);
  });
  
  output$data_preview <- renderDT({
    mat <- data_matrix();
    req(mat);
    df <- as.data.frame(mat);
    colnames(df) <- paste("Col", 1:ncol(df));
    rownames(df) <- paste("Row", 1:nrow(df));
    datatable(df, options = dt_opts, rownames = TRUE);
  });
  
  # ----------------------------------------------------
  # 1. Goodness-of-Fit Logic
  # ----------------------------------------------------
  gof_calc <- reactive({
    mat <- data_matrix();
    req(mat);
    obs <- as.vector(t(mat));
    obs <- obs[!is.na(obs)];
    validate(need(length(obs) > 1, "Goodness-of-Fit requires at least 2 categories."));
    
    k <- length(obs);
    n <- sum(obs);
    
    if (input$gof_dist == "Uniform") {
      probs <- rep(1/k, k);
    } else {
      probs <- as.numeric(unlist(strsplit(trimws(input$gof_probs), "[,\\s\\n]+", perl = TRUE)));
      probs <- probs[!is.na(probs)];
      validate(need(length(probs) == k, "Number of probabilities must match number of observed categories."));
    }
    
    exp <- n * probs;
    chi_cell <- ((obs - exp)^2) / exp;
    chi_sq <- sum(chi_cell);
    df <- k - 1;
    p_val <- 1 - pchisq(chi_sq, df);
    
    adj_res <- (obs - exp) / sqrt(n * probs * (1 - probs));
    p_adj <- 2 * (1 - pnorm(abs(adj_res)));
    bon_sig <- ifelse(p_adj < (0.05 / k), "sig", "n.s.");
    cramer_v <- sqrt(chi_sq / (n * (k - 1)));
    
    list(obs = obs, probs = probs, exp = exp, chi_cell = chi_cell, 
         adj_res = adj_res, bon_sig = bon_sig, chi_sq = chi_sq, 
         df = df, p_val = p_val, cramer_v = cramer_v, n = n, k = k);
  });
  
  output$gof_table <- renderDT({
    res <- gof_calc();
    
    df <- data.frame(
      Category = 1:res$k,
      Observed = res$obs,
      Expected = fmt(res$exp),
      `((O-E)^2)/E` = fmt(res$chi_cell),
      `Adj. Stand. Resid.` = fmt(res$adj_res),
      `Bon Adj. (alpha = 0.05)` = res$bon_sig,
      check.names = FALSE
    );
    
    if (input$gof_dist == "User-Specified") {
      df <- data.frame(
        Category = 1:res$k,
        Observed = res$obs,
        Probability = fmt(res$probs),
        Expected = fmt(res$exp),
        `((O-E)^2)/E` = fmt(res$chi_cell),
        `Adj. Stand. Resid.` = fmt(res$adj_res),
        `Bon Adj. (alpha = 0.05)` = res$bon_sig,
        check.names = FALSE
      );
    }
    
    totals <- df[1, ];
    totals[1, ] <- NA;
    totals$Category <- "TOTALS";
    totals$Observed <- sum(res$obs);
    totals$Expected <- fmt(sum(res$exp));     totals$`((O-E)^2)/E` <- fmt(res$chi_sq);
    if(input$gof_dist == "User-Specified") { totals$Probability <- 1; }
    
    datatable(rbind(df, totals), options = dt_opts, rownames = FALSE);
  });
  
  output$gof_summary <- renderDT({
    res <- gof_calc();
    df <- data.frame(
      Metric = c("Chi-square", "d.f.", "Prob.", "p-value"),
      Value = c(fmt(res$chi_sq), res$df, fmt(res$p_val), p_fmt(res$p_val))
    );
    datatable(df, options = list(dom = 't', ordering = FALSE, headerCallback = JS("function(thead) {$(thead).remove();}")), rownames = FALSE);
  });
  
  output$gof_es <- renderDT({
    res <- gof_calc();
    w <- res$cramer_v * sqrt(res$k - 1);
    df <- data.frame(
      ES = "Cramer's V",
      Value = fmt(res$cramer_v),
      Size = get_es_size(w)
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  # ----------------------------------------------------
  # 2 & 3. 2x2 Matrix Validators (McNemar, Fisher, Yates)
  # ----------------------------------------------------
  get_2x2 <- reactive({
    mat <- data_matrix();
    req(mat);
    validate(need(nrow(mat) == 2 && ncol(mat) == 2, 
                  "This test requires a 2x2 table. Please adjust your input data or row/col settings."));
    mat;
  });
  
  mc_calc <- reactive({
    mat <- get_2x2();
    a <- mat[1,1]; b <- mat[1,2]; c <- mat[2,1]; d <- mat[2,2];
    n <- a + b + c + d;
    chi_sq <- ((abs(b - c) - 1)^2) / (b + c);
    p_val <- 1 - pchisq(chi_sq, 1);
    phi <- sqrt(chi_sq / n);
    list(a=a, b=b, c=c, d=d, n=n, chi_sq=chi_sq, p_val=p_val, phi=phi);
  });
  
  output$mc_table <- renderDT({
    res <- mc_calc();
    df <- data.frame(
      ` ` = c("Time 1: Category 1", "Time 1: Category 2", "TOTAL"),
      `Time 2: Category 1` = c(res$a, res$c, res$a + res$c),
      `Time 2: Category 2` = c(res$b, res$d, res$b + res$d),
      TOTAL = c(res$a + res$b, res$c + res$d, res$n),
      check.names = FALSE
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$mc_summary <- renderDT({
    res <- mc_calc();
    df <- data.frame(
      Metric = c("McNemar's Chi-square", "Prob.", "p-value"),
      Value = c(fmt(res$chi_sq), fmt(res$p_val), p_fmt(res$p_val))
    );
    datatable(df, options = list(dom = 't', ordering = FALSE, headerCallback = JS("function(thead) {$(thead).remove();}")), rownames = FALSE);
  });
  
  output$mc_es <- renderDT({
    res <- mc_calc();
    df <- data.frame(
      ES = c("Phi Coefficient", "Cramer's V"),
      Value = c(fmt(res$phi), fmt(res$phi)),
      Size = c(get_es_size(res$phi), get_es_size(res$phi))
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  fy_calc <- reactive({
    mat <- get_2x2();
    a <- mat[1,1]; b <- mat[1,2]; c <- mat[2,1]; d <- mat[2,2];
    n <- sum(mat);
    
    y_test <- suppressWarnings(chisq.test(mat, correct = TRUE));
    y_chi <- y_test$statistic;
    y_pval <- y_test$p.value;
    y_phi <- sqrt(y_chi / n);
    
    p_neq <- fisher.test(mat, alternative = "two.sided")$p.value;
    p_less <- fisher.test(mat, alternative = "less")$p.value;
    p_greater <- fisher.test(mat, alternative = "greater")$p.value;
    p_a <- dhyper(a, a+b, c+d, a+c);
    odds <- (a * d) / (b * c);
    
    list(a=a, b=b, c=c, d=d, n=n, mat=mat,
         y_chi=y_chi, y_pval=y_pval, y_phi=y_phi,
         p_neq=p_neq, p_less=p_less, p_greater=p_greater, p_a=p_a, odds=odds);
  });
  
  output$fy_table <- renderDT({
    res <- fy_calc();
    df <- data.frame(
      ` ` = c("Row 1", "Row 2", "TOTAL"),
      `Column 1` = c(res$a, res$c, res$a + res$c),
      `Column 2` = c(res$b, res$d, res$b + res$d),
      TOTAL = c(res$a + res$b, res$c + res$d, res$n),
      check.names = FALSE
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$fisher_working <- renderDT({
    res <- fy_calc();
    df <- data.frame(
      Metric = c("a", "a+b", "a+c", "n", "P(a)"),
      Value = c(res$a, res$a+res$b, res$a+res$c, res$n, fmt(res$p_a))
    );
    datatable(df, options = list(dom = 't', ordering = FALSE, headerCallback = JS("function(thead) {$(thead).remove();}")), rownames = FALSE);
  });
  
  output$fisher_summary <- renderDT({
    res <- fy_calc();
    df <- data.frame(
      `H(a)` = c("\"≠\"", "\"<\"", "\">\""),
      Prob. = c(fmt(res$p_neq), fmt(res$p_less), fmt(res$p_greater)),
      `p-value` = c(p_fmt(res$p_neq), p_fmt(res$p_less), p_fmt(res$p_greater)),
      check.names = FALSE
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$fisher_es <- renderDT({
    res <- fy_calc();
    p_chi <- suppressWarnings(chisq.test(res$mat, correct = FALSE))$statistic;
    f_phi <- sqrt(p_chi / res$n);
    
    df <- data.frame(
      ES = c("Odds Ratio", "Phi Coefficient", "Cramer's V"),
      Value = c(fmt(res$odds), fmt(f_phi), fmt(f_phi)),
      Size = c(get_or_size(res$odds), get_es_size(f_phi), get_es_size(f_phi))
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$yates_summary <- renderDT({
    res <- fy_calc();
    df <- data.frame(
      Metric = c("Yates' Chi-square", "d.f.", "Prob.", "p-value"),
      Value = c(fmt(res$y_chi), "1", fmt(res$y_pval), p_fmt(res$y_pval))
    );
    datatable(df, options = list(dom = 't', ordering = FALSE, headerCallback = JS("function(thead) {$(thead).remove();}")), rownames = FALSE);
  });
  
  output$yates_es <- renderDT({
    res <- fy_calc();
    df <- data.frame(
      ES = c("Phi Coefficient", "Cramer's V"),
      Value = c(fmt(res$y_phi), fmt(res$y_phi)),
      Size = c(get_es_size(res$y_phi), get_es_size(res$y_phi))
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  # ----------------------------------------------------
  # 4. Pearson & LR (RxC) Logic
  # ----------------------------------------------------
  rxc_calc <- reactive({
    mat <- data_matrix();
    req(mat);
    validate(need(nrow(mat) >= 2 && ncol(mat) >= 2, 
                  "RxC Test requires at least a 2x2 matrix. Please check your data dimensions."));
    
    n <- sum(mat);
    r <- nrow(mat);
    c <- ncol(mat);
    
    row_sums <- rowSums(mat);
    col_sums <- colSums(mat);
    
    exp <- outer(row_sums, col_sums) / n;
    
    p_chi_cell <- (mat - exp)^2 / exp;
    p_chi <- sum(p_chi_cell);
    df <- (r - 1) * (c - 1);
    p_pval <- 1 - pchisq(p_chi, df);
    
    lr_cell <- ifelse(mat > 0, mat * log(mat / exp), 0);
    lr_chi <- 2 * sum(lr_cell);
    lr_pval <- 1 - pchisq(lr_chi, df);
    
    df_star <- min(r - 1, c - 1);
    p_phi <- sqrt(p_chi / n);
    p_cramer <- sqrt(p_chi / (n * df_star));
    
    lr_phi <- sqrt(lr_chi / n);
    lr_cramer <- sqrt(lr_chi / (n * df_star));
    
    list(mat=mat, exp=exp, p_chi_cell=p_chi_cell, p_chi=p_chi, p_pval=p_pval,
         lr_cell=lr_cell, lr_chi=lr_chi, lr_pval=lr_pval, df=df, 
         p_phi=p_phi, p_cramer=p_cramer, lr_phi=lr_phi, lr_cramer=lr_cramer, df_star=df_star);
  });
  
  append_margins <- function(mat) {
    df <- as.data.frame(mat);
    df$TOTAL <- rowSums(mat);
    tot_row <- c(colSums(mat), sum(mat));
    df <- rbind(df, tot_row);
    df <- cbind(Row = c(1:nrow(mat), "TOTAL"), df);
    return(df);
  };
  
  output$rxc_obs <- renderDT({
    res <- rxc_calc();
    df <- append_margins(res$mat);
    colnames(df)[2:(ncol(df)-1)] <- 1:ncol(res$mat);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$rxc_exp <- renderDT({
    res <- rxc_calc();
    df <- append_margins(res$exp);
    df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], c(1,2), fmt);
    colnames(df)[2:(ncol(df)-1)] <- 1:ncol(res$mat);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$rxc_pearson_cell <- renderDT({
    res <- rxc_calc();
    df <- append_margins(res$p_chi_cell);
    df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], c(1,2), fmt);
    colnames(df)[2:(ncol(df)-1)] <- 1:ncol(res$mat);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$rxc_pearson_sum <- renderDT({
    res <- rxc_calc();
    df <- data.frame(
      Metric = c("Pearson Chi-Square", "d.f.", "Prob.", "p-value"),
      Value = c(fmt(res$p_chi), res$df, fmt(res$p_pval), p_fmt(res$p_pval))
    );
    datatable(df, options = list(dom = 't', ordering = FALSE, headerCallback = JS("function(thead) {$(thead).remove();}")), rownames = FALSE);
  });
  
  output$rxc_pearson_es <- renderDT({
    res <- rxc_calc();
    w <- res$p_cramer * sqrt(res$df_star);
    df <- data.frame(
      ES = c("Phi Coefficient", "Cramer's V"),
      Value = c(fmt(res$p_phi), fmt(res$p_cramer)),
      Size = c(get_es_size(w), get_es_size(w))
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$rxc_lr_cell <- renderDT({
    res <- rxc_calc();
    df <- append_margins(res$lr_cell);
    df[, 2:ncol(df)] <- apply(df[, 2:ncol(df)], c(1,2), fmt);
    colnames(df)[2:(ncol(df)-1)] <- 1:ncol(res$mat);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$rxc_lr_sum <- renderDT({
    res <- rxc_calc();
    df <- data.frame(
      Metric = c("LR Chi-square", "d.f.", "Prob.", "p-value"),
      Value = c(fmt(res$lr_chi), res$df, fmt(res$lr_pval), p_fmt(res$lr_pval))
    );
    datatable(df, options = list(dom = 't', ordering = FALSE, headerCallback = JS("function(thead) {$(thead).remove();}")), rownames = FALSE);
  });
  
  output$rxc_lr_es <- renderDT({
    res <- rxc_calc();
    w <- res$lr_cramer * sqrt(res$df_star);
    df <- data.frame(
      ES = c("Phi Coefficient", "Cramer's V"),
      Value = c(fmt(res$lr_phi), fmt(res$lr_cramer)),
      Size = c(get_es_size(w), get_es_size(w))
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
}

shinyApp(ui = ui, server = server);