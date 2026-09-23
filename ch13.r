library(shiny);
library(DT);
library(readxl);
library(dplyr);
library(tidyr);

# ==========================================
# Helper Functions & Options
# ==========================================
fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x) | is.infinite(x), "", sprintf("%.4f", as.numeric(x)));
};

p_fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x), "", 
         ifelse(as.numeric(x) < 0.0001, "< 0.0001", sprintf("%.4f", as.numeric(x))));
};

# General data table setting
dt_opts <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all"))
);

# Summary data table setting
sum_opts <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  headerCallback = JS("function(t) {$(t).remove();}")
);

# ==========================================
# User Interface
# ==========================================
ui <- fluidPage(
  titlePanel("Non-Parametric Tests Calculator"),
  
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
      helpText("Input raw data directly. No headers needed for manual paste."),
      
      radioButtons("input_type", "Input Method:",
                   choices = c("Copy & Paste Data" = "paste", 
                               "Upload CSV / XLSX File" = "upload")),
      
      conditionalPanel(
        condition = "input.input_type == 'paste'",
        textAreaInput("pasted_data", "Paste Data (No Headers):", 
                      value = "4.9100\t5.1200\n4.1000\t4.8000\n6.7400\n7.2700\t6.5000", rows = 6),
        helpText("Use Spaces, Commas, Semicolons, or Tabs. Unequal sample sizes are supported.")
      ),
      
      conditionalPanel(
        condition = "input.input_type == 'upload'",
        fileInput("file_data", "Upload File (.csv, .xlsx)", 
                  accept = c(".csv", ".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
        checkboxInput("has_header", "File has headers (will be ignored)", value = FALSE)
      ),
      
      hr(),
      h4("2. Test Settings"),
      conditionalPanel(
        condition = "input.main_tabs == 'sign1' || input.main_tabs == 'wilc1'",
        numericInput("val_1s", "Test Value (mu):", value = 5.05)
      ),
      conditionalPanel(
        condition = "input.main_tabs != 'sign1' && input.main_tabs != 'wilc1'",
        helpText("No additional settings required for this test. Columns 1 and 2 are used automatically (or all columns for Kruskal-Wallis).")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Data Preview", value = "data",
                 br(), h4("Data Matrix Preview"), DTOutput("data_preview")
        ),
        tabPanel("Sign Test (1S)", value = "sign1",
                 br(), fluidRow(column(6, h4("Data Table"), DTOutput("s1_table")), column(6, h4("Summary"), DTOutput("s1_sum")))
        ),
        tabPanel("Wilcoxon (1S)", value = "wilc1",
                 br(), fluidRow(column(8, h4("Data Table"), DTOutput("w1_table")), column(4, h4("Summary"), DTOutput("w1_sum")))
        ),
        tabPanel("Sign Test (Paired)", value = "sign2",
                 br(), fluidRow(column(6, h4("Data Table"), DTOutput("s2_table")), column(6, h4("Summary"), DTOutput("s2_sum")))
        ),
        tabPanel("Wilcoxon (Paired)", value = "wilc2",
                 br(), fluidRow(column(8, h4("Data Table"), DTOutput("w2_table")), column(4, h4("Summary"), DTOutput("w2_sum")))
        ),
        tabPanel("Mann-Whitney", value = "mwu",
                 br(), fluidRow(column(6, h4("Data Table"), DTOutput("mwu_table")), column(6, h4("Summary & ES"), DTOutput("mwu_sum")))
        ),
        tabPanel("Kruskal-Wallis", value = "kw",
                 br(), h4("Group Ranks"), DTOutput("kw_table"),
                 br(), fluidRow(column(6, h4("Kruskal-Wallis Summary"), DTOutput("kw_sum")), column(6, h4("Post-Hoc Pairwise (MWU Bonferroni)"), DTOutput("kw_post")))
        ),
        tabPanel("Spearman", value = "spearman",
                 br(), fluidRow(column(6, h4("Data Table"), DTOutput("sp_table")), column(6, h4("Correlation Summary"), DTOutput("sp_sum")))
        ),
        tabPanel("Kendall", value = "kendall",
                 br(), h4("Data & Concordance Table"), DTOutput("kd_table"),
                 br(), h4("Kendall's Tau Summary"), DTOutput("kd_sum")
        )
      )
    )
  )
);

# ==========================================
# Server logic
# ==========================================
server <- function(input, output, session) {
  
  raw_data <- reactive({
    df <- NULL;
    if (input$input_type == "paste") {
      req(input$pasted_data != "");
      tryCatch({
        lines <- unlist(strsplit(input$pasted_data, "\n"));
        lines <- lines[trimws(lines) != ""]; 
        
        if (any(grepl("\t", lines))) {
          df <- read.table(text = paste(lines, collapse="\n"), header = FALSE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"));
        } else if (any(grepl(",", lines))) {
          df <- read.table(text = paste(lines, collapse="\n"), header = FALSE, sep = ",", fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"));
        } else if (any(grepl(";", lines))) {
          df <- read.table(text = paste(lines, collapse="\n"), header = FALSE, sep = ";", fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"));
        } else {
          df <- read.table(text = paste(lines, collapse="\n"), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN"));
        }
      }, error = function(e) { NULL; });
    } else {
      req(input$file_data);
      ext <- tools::file_ext(input$file_data$name);
      tryCatch({
        if (ext == "csv") { 
          df <- read.csv(input$file_data$datapath, header = input$has_header, stringsAsFactors = FALSE, na.strings = c("", "NA", "NaN")); 
        } else if (ext %in% c("xls", "xlsx")) { 
          df <- as.data.frame(readxl::read_excel(input$file_data$datapath, col_names = input$has_header)); 
        }
      }, error = function(e) { NULL; });
    }
    return(df);
  });
  
  output$data_preview <- renderDT({
    req(raw_data());
    df <- head(raw_data(), 50);
    colnames(df) <- paste0("Col_", 1:ncol(df));
    datatable(df, options = list(pageLength = 10, dom = 'tip', className = 'dt-center'), rownames = FALSE);
  });
  
  # --- 1. Sign Test (1S) ---
  s1_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 1);
    v <- as.numeric(na.omit(df[[1]]));
    val <- input$val_1s;
    d <- v - val;
    s <- ifelse(d > 0, "+", ifelse(d < 0, "-", "0"));
    n_pos <- sum(s == "+"); n_neg <- sum(s == "-"); n_zero <- sum(s == "0");
    x_test <- min(n_pos, n_neg); n <- n_pos + n_neg;
    p_neq <- binom.test(n_pos, n, 0.5, "two.sided")$p.value;
    p_less <- binom.test(n_pos, n, 0.5, "less")$p.value;
    p_greater <- binom.test(n_pos, n, 0.5, "greater")$p.value;
    list(v=v, d=d, s=s, n_pos=n_pos, n_neg=n_neg, n_zero=n_zero, n=n, x_test=x_test, p_neq=p_neq, p_less=p_less, p_greater=p_greater, val=val);
  });
  
  output$s1_table <- renderDT({
    res <- s1_calc();
    df <- data.frame(`Data No.` = 1:length(res$v), Values = fmt(res$v), Difference = fmt(res$d), Sign = res$s, check.names = FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$s1_sum <- renderDT({
    res <- s1_calc();
    df1 <- data.frame(Metric = c("Test Value", "n(non-zero)", "n(pos)", "n(neg)", "x(test)"), Value = c(res$val, res$n, res$n_pos, res$n_neg, res$x_test));
    df_blank <- data.frame(Metric = "", Value = "");
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne;", "&lt;", "&gt;"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_less), p_fmt(res$p_greater)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
  # --- 2. Wilcoxon (1S) ---
  w1_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 1);
    v <- as.numeric(na.omit(df[[1]]));
    val <- input$val_1s;
    d <- v - val;
    d_nz <- d[d != 0];
    r <- rank(abs(d_nz));
    sr <- r * sign(d_nz);
    tp <- sum(r[d_nz > 0]); tn <- sum(r[d_nz < 0]);
    n <- length(d_nz);
    
    E_T <- n * (n + 1) / 4;
    S_stat <- tp - E_T;
    Var_T <- n * (n + 1) * (2*n + 1) / 24 - sum(table(r)^3 - table(r)) / 48;
    z_val <- ifelse(Var_T > 0, S_stat / sqrt(Var_T), 0);
    
    if (n <= 20) {
      sums <- 0;
      for(rank_val in r) { sums <- c(sums, sums + rank_val); }
      total_perms <- length(sums);
      
      if (tp > E_T) {
        p_g <- sum(sums >= tp) / total_perms;
        p_l <- 1.0 - p_g;
        p_neq <- min(1.0, 2 * p_g);
      } else if (tp < E_T) {
        p_l <- sum(sums <= tp) / total_perms;
        p_g <- 1.0 - p_l;
        p_neq <- min(1.0, 2 * p_l);
      } else {
        p_g <- sum(sums >= tp) / total_perms;
        p_l <- sum(sums <= tp) / total_perms;
        p_neq <- 1.0;
      }
    } else {
      if (tp > E_T) {
        z_g <- (tp - 0.5 - E_T) / sqrt(Var_T);
        p_g <- 1 - pnorm(z_g);
        p_l <- 1.0 - p_g;
        p_neq <- min(1.0, 2 * p_g);
      } else if (tp < E_T) {
        z_l <- (tp + 0.5 - E_T) / sqrt(Var_T);
        p_l <- pnorm(z_l);
        p_g <- 1.0 - p_l;
        p_neq <- min(1.0, 2 * p_l);
      } else {
        p_g <- 0.5; p_l <- 0.5; p_neq <- 1.0;
      }
    }
    
    list(v=v[d!=0], d=d_nz, r=r, sr=sr, n=n, tp=tp, tn=tn, S=S_stat, z=z_val, p_neq=p_neq, p_l=p_l, p_g=p_g, val=val);
  });
  
  output$w1_table <- renderDT({
    res <- w1_calc();
    df <- data.frame(`Data No.`=1:length(res$v), Match1=fmt(res$v), Difference=fmt(res$d), `|diff|`=fmt(abs(res$d)), `R|diff|`=fmt(res$r), `Signed(rank)`=fmt(res$sr), check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$w1_sum <- renderDT({
    res <- w1_calc();
    df1 <- data.frame(Metric=c("Test Value", "n", "S", "T+", "T-", "z"), Value=c(res$val, res$n, res$S, res$tp, res$tn, fmt(res$z)));
    df_blank <- data.frame(Metric = "", Value = "");
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne;", "&lt;", "&gt;"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_l), p_fmt(res$p_g)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
  # --- 3. Sign Test (Paired) ---
  s2_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 2);
    df_clean <- na.omit(df[, 1:2]); 
    v1 <- as.numeric(df_clean[[1]]); v2 <- as.numeric(df_clean[[2]]);
    d <- v1 - v2; s <- ifelse(d > 0, "+", ifelse(d < 0, "-", "0"));
    n_pos <- sum(s == "+"); n_neg <- sum(s == "-"); n_zero <- sum(s == "0");
    x_test <- min(n_pos, n_neg); n <- n_pos + n_neg;
    p_neq <- binom.test(n_pos, n, 0.5, "two.sided")$p.value;
    p_less <- binom.test(n_pos, n, 0.5, "less")$p.value;
    p_greater <- binom.test(n_pos, n, 0.5, "greater")$p.value;
    list(v1=v1, v2=v2, d=d, s=s, n_pos=n_pos, n_neg=n_neg, n_zero=n_zero, n=n, x_test=x_test, p_neq=p_neq, p_less=p_less, p_greater=p_greater);
  });
  
  output$s2_table <- renderDT({
    res <- s2_calc();
    df <- data.frame(`Data No.`=1:length(res$v1), Match1=fmt(res$v1), Match2=fmt(res$v2), Difference=fmt(res$d), Sign=res$s, check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$s2_sum <- renderDT({
    res <- s2_calc();
    df1 <- data.frame(Metric=c("n(non-zero)", "n(pos)", "n(neg)", "x(test)"), Value=c(res$n, res$n_pos, res$n_neg, res$x_test));
    df_blank <- data.frame(Metric = "", Value = "");
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne;", "&lt;", "&gt;"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_less), p_fmt(res$p_greater)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
  # --- 4. Wilcoxon (Paired) ---
  w2_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 2);
    df_clean <- na.omit(df[, 1:2]); 
    v1 <- as.numeric(df_clean[[1]]); v2 <- as.numeric(df_clean[[2]]);
    d <- v1 - v2; 
    idx <- d != 0; d_nz <- d[idx]; v1_nz <- v1[idx]; v2_nz <- v2[idx];
    r <- rank(abs(d_nz)); sr <- r * sign(d_nz);
    tp <- sum(r[d_nz > 0]); tn <- sum(r[d_nz < 0]); 
    n <- length(d_nz);
    
    E_T <- n * (n + 1) / 4;
    S_stat <- tp - E_T;
    Var_T <- n * (n + 1) * (2*n + 1) / 24 - sum(table(r)^3 - table(r)) / 48;
    z_val <- ifelse(Var_T > 0, S_stat / sqrt(Var_T), 0);
    
    if (n <= 20) {
      sums <- 0;
      for(rank_val in r) { sums <- c(sums, sums + rank_val); }
      total_perms <- length(sums);
      
      if (tp > E_T) {
        p_g <- sum(sums >= tp) / total_perms;
        p_l <- 1.0 - p_g;
        p_neq <- min(1.0, 2 * p_g);
      } else if (tp < E_T) {
        p_l <- sum(sums <= tp) / total_perms;
        p_g <- 1.0 - p_l;
        p_neq <- min(1.0, 2 * p_l);
      } else {
        p_g <- sum(sums >= tp) / total_perms;
        p_l <- sum(sums <= tp) / total_perms;
        p_neq <- 1.0;
      }
    } else {
      if (tp > E_T) {
        z_g <- (tp - 0.5 - E_T) / sqrt(Var_T);
        p_g <- 1 - pnorm(z_g);
        p_l <- 1.0 - p_g;
        p_neq <- min(1.0, 2 * p_g);
      } else if (tp < E_T) {
        z_l <- (tp + 0.5 - E_T) / sqrt(Var_T);
        p_l <- pnorm(z_l);
        p_g <- 1.0 - p_l;
        p_neq <- min(1.0, 2 * p_l);
      } else {
        p_g <- 0.5; p_l <- 0.5; p_neq <- 1.0;
      }
    }
    
    list(v1=v1_nz, v2=v2_nz, d=d_nz, r=r, sr=sr, n=n, tp=tp, tn=tn, S=S_stat, z=z_val, p_neq=p_neq, p_l=p_l, p_g=p_g);
  });
  
  output$w2_table <- renderDT({
    res <- w2_calc();
    df <- data.frame(`Data No.`=1:length(res$v1), Match1=fmt(res$v1), Match2=fmt(res$v2), Difference=fmt(res$d), `R|diff|`=fmt(res$r), `Signed(rank)`=fmt(res$sr), check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$w2_sum <- renderDT({
    res <- w2_calc();
    df1 <- data.frame(Metric=c("n", "S", "T+", "T-", "z"), Value=c(res$n, res$S, res$tp, res$tn, fmt(res$z)));
    df_blank <- data.frame(Metric = "", Value = "");
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne;", "&lt;", "&gt;"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_l), p_fmt(res$p_g)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
  # --- 5. Mann-Whitney ---
  mwu_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 2);
    v1 <- as.numeric(na.omit(df[[1]])); v2 <- as.numeric(na.omit(df[[2]]));
    n1 <- length(v1); n2 <- length(v2);
    r <- rank(c(v1, v2)); r1 <- r[1:n1]; r2 <- r[(n1+1):(n1+n2)];
    w <- sum(r1); u <- w - (n1*(n1+1))/2;
    res <- suppressWarnings(wilcox.test(v1, v2, exact=FALSE));
    res_l <- suppressWarnings(wilcox.test(v1, v2, exact=FALSE, alternative="less"));
    res_g <- suppressWarnings(wilcox.test(v1, v2, exact=FALSE, alternative="greater"));
    z <- qnorm(res$p.value/2) * sign(mean(v1)-mean(v2));
    grid <- expand.grid(x=v1, y=v2); cliff <- (sum(grid$x > grid$y) - sum(grid$x < grid$y))/(n1*n2);
    r_wendt <- z / sqrt(n1+n2);
    list(v1=v1, v2=v2, r1=r1, r2=r2, n1=n1, n2=n2, w=w, u=u, z=z, p_neq=res$p.value, p_l=res_l$p.value, p_g=res_g$p.value, cliff=cliff, rw=r_wendt);
  });
  
  output$mwu_table <- renderDT({
    res <- mwu_calc();
    max_len <- max(res$n1, res$n2);
    v1_pad <- c(res$v1, rep(NA, max_len - res$n1)); v2_pad <- c(res$v2, rep(NA, max_len - res$n2));
    r1_pad <- c(res$r1, rep(NA, max_len - res$n1)); r2_pad <- c(res$r2, rep(NA, max_len - res$n2));
    df <- data.frame(`Data No.`=1:max_len, Sample1=fmt(v1_pad), Sample2=fmt(v2_pad), `R(Sample 1)`=fmt(r1_pad), `R(Sample 2)`=fmt(r2_pad), check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$mwu_sum <- renderDT({
    res <- mwu_calc();
    df1 <- data.frame(Metric=c("n1", "n2", "W", "U", "z"), Value=c(res$n1, res$n2, fmt(res$w), fmt(res$u), fmt(res$z)));
    df_blank <- data.frame(Metric = "", Value = "");
    
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne;", "&lt;", "&gt;"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_l), p_fmt(res$p_g)));
    
    df3_head <- data.frame(Metric = "ES", Value = "Value");
    df3 <- data.frame(Metric = c("Wendt's r", "Cliff's &Delta;"), Value = c(fmt(res$rw), fmt(res$cliff)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2, df_blank, df3_head, df3);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
  # --- 6. Kruskal-Wallis ---
  kw_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 2);
    k <- ncol(df);
    dat_list <- lapply(1:k, function(i) as.numeric(na.omit(df[[i]])));
    all_dat <- unlist(dat_list); group <- factor(rep(1:k, times=sapply(dat_list, length)));
    r <- rank(all_dat);
    res <- suppressWarnings(kruskal.test(all_dat ~ group));
    N <- length(all_dat);
    sum_ranks <- tapply(r, group, sum); mean_ranks <- tapply(r, group, mean); ns <- tapply(r, group, length);
    list(dat_list=dat_list, k=k, N=N, sum_r=sum_ranks, mean_r=mean_ranks, ns=ns, chi=res$statistic, df=res$parameter, p=res$p.value);
  });
  
  output$kw_table <- renderDT({
    res <- kw_calc();
    df <- data.frame(Group=1:res$k, N=as.vector(res$ns), `Mean Rank`=fmt(as.vector(res$mean_r)), check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$kw_sum <- renderDT({
    res <- kw_calc();
    df <- data.frame(Metric=c("KW Chi-Square", "d.f.", "p-value"), Value=c(fmt(res$chi), res$df, p_fmt(res$p)));
    datatable(df, options = sum_opts, rownames=FALSE);
  });
  
  output$kw_post <- renderDT({
    res <- kw_calc();
    pairs <- combn(res$k, 2); m <- ncol(pairs);
    p_raw <- numeric(m); comp <- character(m);
    for(i in 1:m) {
      g1 <- pairs[1,i]; g2 <- pairs[2,i];
      comp[i] <- paste0("G", g1, "-G", g2);
      v1 <- res$dat_list[[g1]]; v2 <- res$dat_list[[g2]];
      p_raw[i] <- suppressWarnings(wilcox.test(v1, v2, exact=FALSE))$p.value;
    }
    p_adj <- pmin(1, p_raw * m);
    df <- data.frame(Comparison=comp, `Raw p-value`=p_fmt(p_raw), `Adjusted p-value`=p_fmt(p_adj), check.names=FALSE);
    datatable(df, options = sum_opts, rownames = FALSE);
  });
  
  # --- 7. Spearman ---
  sp_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 2);
    df_clean <- na.omit(df[, 1:2]); 
    x <- as.numeric(df_clean[[1]]); y <- as.numeric(df_clean[[2]]); n <- length(x);
    rx <- rank(x); ry <- rank(y);
    res <- suppressWarnings(cor.test(x, y, method="spearman"));
    res_l <- suppressWarnings(cor.test(x, y, method="spearman", alternative="less"));
    res_g <- suppressWarnings(cor.test(x, y, method="spearman", alternative="greater"));
    t_stat <- res$estimate * sqrt((n-2)/(1 - res$estimate^2));
    list(x=x, y=y, rx=rx, ry=ry, n=n, rho=res$estimate, t=t_stat, p_neq=res$p.value, p_l=res_l$p.value, p_g=res_g$p.value);
  });
  
  output$sp_table <- renderDT({
    res <- sp_calc();
    df <- data.frame(`Data No.`=1:res$n, x=fmt(res$x), y=fmt(res$y), `rank(x)`=fmt(res$rx), `rank(y)`=fmt(res$ry), check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$sp_sum <- renderDT({
    res <- sp_calc();
    df1 <- data.frame(Metric=c("n", "r(s)", "t"), Value=c(res$n, fmt(res$rho), fmt(res$t)));
    df_blank <- data.frame(Metric = "", Value = "");
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne; 0", "&lt; 0", "&gt; 0"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_l), p_fmt(res$p_g)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
  # --- 8. Kendall ---
  kd_calc <- reactive({
    df <- raw_data(); req(ncol(df) >= 2);
    df_clean <- na.omit(df[, 1:2]); 
    x <- as.numeric(df_clean[[1]]); y <- as.numeric(df_clean[[2]]); n <- length(x);
    res <- suppressWarnings(cor.test(x, y, method="kendall"));
    res_l <- suppressWarnings(cor.test(x, y, method="kendall", alternative="less"));
    res_g <- suppressWarnings(cor.test(x, y, method="kendall", alternative="greater"));
    list(x=x, y=y, n=n, tau=res$estimate, z=res$statistic, p_neq=res$p.value, p_l=res_l$p.value, p_g=res_g$p.value);
  });
  
  output$kd_table <- renderDT({
    res <- kd_calc();
    df <- data.frame(`Data No.`=1:res$n, x=fmt(res$x), y=fmt(res$y), check.names=FALSE);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$kd_sum <- renderDT({
    res <- kd_calc();
    df1 <- data.frame(Metric=c("n", "Tau-b", "z"), Value=c(res$n, fmt(res$tau), fmt(res$z)));
    df_blank <- data.frame(Metric = "", Value = "");
    df2_head <- data.frame(Metric = "H(a)", Value = "p-value");
    df2 <- data.frame(Metric = c("&ne; 0", "&lt; 0", "&gt; 0"), Value = c(p_fmt(res$p_neq), p_fmt(res$p_l), p_fmt(res$p_g)));
    
    final_df <- rbind(df1, df_blank, df2_head, df2);
    datatable(final_df, options = sum_opts, rownames=FALSE, escape=FALSE);
  });
  
}

shinyApp(ui = ui, server = server);