# install.packages(c("shiny", "DT"))

library(shiny);
library(DT);

# ==========================================
# 輔助函數 (Helper Functions)
# ==========================================
fmt <- function(x) {
  ifelse(is.na(x) | is.nan(x) | is.infinite(x), "", sprintf("%.4f", as.numeric(x)));
};

# 判斷效應量大小並加上星號 (下限設為 0.0，確保所有數值皆有歸屬)
get_es_stars <- function(val, type) {
  val <- abs(as.numeric(val));
  if (is.na(val)) return(c("", "", ""));
  
  # 定義閾值 (Small, Moderate, Large)
  thr <- switch(type,
                "d" = c(0.0, 0.5, 0.8),         # Cohen's d, Glass, Hedges, h
                "r" = c(0.0, 0.3, 0.5),         # r, phi, Cramer's V, W, Cliff, Wendt
                "f2" = c(0.0, 0.15, 0.35),      # Cohen's f2
                "eta2" = c(0.0, 0.06, 0.14),    # eta2, partial eta2, omega2
                c(0.0, 0.3, 0.5)                # 預設
  );
  
  res <- c("", "", "");
  if (val >= thr[3]) { res[3] <- "*"; }
  else if (val >= thr[2]) { res[2] <- "*"; }
  else if (val >= thr[1]) { res[1] <- "*"; }
  return(res);
};

# 一般資料表設定
dt_opts <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all"))
);

# 無表頭資料表設定
dt_opts_nohead <- list(
  dom = 't', 
  paging = FALSE, 
  ordering = FALSE,
  columnDefs = list(list(className = 'dt-center', targets = "_all")),
  headerCallback = JS("function(t) {$(t).remove();}")
);

# ==========================================
# UI 介面
# ==========================================
ui <- fluidPage(
  titlePanel("Effect Size Calculator & Converter"),
  
  tags$head(
    tags$style(HTML("
      table.dataTable thead th, table.dataTable tbody td { text-align: center !important; }
      .well { background-color: #fdfdfd; padding: 15px; border-radius: 5px; }
      h4 { color: #2c3e50; font-weight: bold; border-bottom: 2px solid #eee; padding-bottom: 5px; margin-top: 20px;}
      .conv-row { margin-bottom: 10px; align-items: center; }
    "))
  ),
  
  tabsetPanel(
    # ----------------------------------------------------
    # 1. Means
    # ----------------------------------------------------
    tabPanel("Means",
             fluidRow(
               # Single Mean
               column(4,
                      h4("Test of Single Mean"),
                      wellPanel(
                        numericInput("m1_mean", "Sample Mean:", -18.075),
                        numericInput("m1_mu0", "Mean under H0:", 0),
                        numericInput("m1_sd", "SD(x):", 32.6817)
                      ),
                      DTOutput("m1_var_out"), br(), DTOutput("m1_es_out")
               ),
               # Paired Means
               column(4,
                      h4("Test of Paired/Matched Means"),
                      wellPanel(
                        numericInput("mp_diff", "Mean Difference:", 21.0),
                        numericInput("mp_mu0", "Mean under H0:", 30.0),
                        numericInput("mp_sd", "SD(x) of Difference:", 11.0)
                      ),
                      DTOutput("mp_var_out"), br(), DTOutput("mp_es_out")
               ),
               # Difference of 2 Means
               column(4,
                      h4("Test for the Difference of 2 Means"),
                      wellPanel(
                        fluidRow(
                          column(6, strong("Sample 1 (Control)"),
                                 numericInput("m2_n1", "n:", 11),
                                 numericInput("m2_mean1", "Mean:", 1472.27),
                                 numericInput("m2_sd1", "SD(x):", 414.394)
                          ),
                          column(6, strong("Sample 2"),
                                 numericInput("m2_n2", "n:", 11),
                                 numericInput("m2_mean2", "Mean:", 805.91),
                                 numericInput("m2_sd2", "SD(x):", 364.704)
                          )
                        )
                      ),
                      DTOutput("m2_var_out"), br(), DTOutput("m2_sds_out"), br(), DTOutput("m2_es_out")
               )
             )
    ),
    
    # ----------------------------------------------------
    # 2. Proportions & Mann-Whitney
    # ----------------------------------------------------
    tabPanel("Proportions & Non-Parametric",
             fluidRow(
               # Single Proportion
               column(4,
                      h4("Test for Single Proportion"),
                      wellPanel(
                        numericInput("p1_n", "n (Successes):", 90),
                        numericInput("p1_tot", "Total:", 295),
                        numericInput("p1_test", "Test Value:", 0.35)
                      ),
                      DTOutput("p1_calc_out"), br(), DTOutput("p1_es_out")
               ),
               # Difference of 2 Proportions
               column(4,
                      h4("Test for the Difference of 2 Proportions"),
                      wellPanel(
                        fluidRow(
                          column(6, strong("Sample 1"),
                                 numericInput("p2_n1", "n:", 72),
                                 numericInput("p2_tot1", "Total:", 1222)
                          ),
                          column(6, strong("Sample 2"),
                                 numericInput("p2_n2", "n:", 30),
                                 numericInput("p2_tot2", "Total:", 282)
                          )
                        ),
                        numericInput("p2_test", "Test Value (Difference):", 0.0)
                      ),
                      DTOutput("p2_calc_out"), br(), DTOutput("p2_es_out")
               ),
               # Mann-Whitney Test
               column(4,
                      h4("Mann-Whitney Test"),
                      wellPanel(
                        numericInput("mw_n1", "n1:", 15),
                        numericInput("mw_n2", "n2:", 10),
                        numericInput("mw_u", "U:", 25)
                      ),
                      DTOutput("mw_es_out")
               )
             )
    ),
    
    # ----------------------------------------------------
    # 3. Regression, Chi-Square, ANOVA
    # ----------------------------------------------------
    tabPanel("Regression, Chi-Square, ANOVA",
             fluidRow(
               # Linear Regression
               column(4,
                      h4("Linear Regression"),
                      wellPanel(
                        numericInput("lr_r2", "R2 (R-squared):", 0.3510)
                      ),
                      DTOutput("lr_es_out")
               ),
               # Chi-Square
               column(4,
                      h4("Chi-square Tests & Frequencies"),
                      wellPanel(
                        numericInput("chi_n", "n (Total Sample):", 20),
                        numericInput("chi_x2", "X2 (Chi-Square):", 7.2),
                        numericInput("chi_r", "number of ROWS:", 2),
                        numericInput("chi_c", "number of COLUMNS:", 2)
                      ),
                      DTOutput("chi_es_out")
               ),
               # ANOVA
               column(4,
                      h4("ANOVA"),
                      wellPanel(
                        numericInput("aov_ss_a", "Among SS:", 21261.8289),
                        numericInput("aov_df_a", "Among df:", 3),
                        numericInput("aov_ss_w", "Within SS:", 36747.2267),
                        numericInput("aov_df_w", "Within df:", 140)
                      ),
                      DTOutput("aov_calc_out"), br(), DTOutput("aov_es_out")
               )
             )
    ),
    
    # ----------------------------------------------------
    # 4. Effect Size Converter
    # ----------------------------------------------------
    tabPanel("Effect Size Converter",
             br(), h4("Interactive Converters"),
             
             wellPanel(
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_d1", "Cohen's d", 0.20)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Pearson's r =")), textOutput("res_cv_r1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_r2", "Pearson's r", 0.50)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's d =")), textOutput("res_cv_d2"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_d3", "Cohen's d", 0.4438)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's f =")), textOutput("res_cv_f1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_f4", "Cohen's f", 0.3950)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's d =")), textOutput("res_cv_d4"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_d5", "Cohen's d", 1.0000)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Log Odds Ratio =")), textOutput("res_cv_lo1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_lo6", "Log Odds Ratio", 1.8138)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's d =")), textOutput("res_cv_d6"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_d7", "Cohen's d", -0.1777)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("AUC =")), textOutput("res_cv_auc1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_auc8", "AUC", 0.4500)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's d =")), textOutput("res_cv_d8"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_r9", "Correlation r", 0.4470)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Fisher's z =")), textOutput("res_cv_z1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_z10", "Fisher's z", 0.4809)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Correlation r =")), textOutput("res_cv_r10"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(2, numericInput("cv_n11", "n (sample size)", 144)),
                        column(2, numericInput("cv_d11", "Cohen's d", 0.8137)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(1, p(strong("Partial \u03B72 =")), textOutput("res_cv_peta1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_n12", "n (sample size)", 144)),
                        column(1, numericInput("cv_peta12", "Partial \u03B72", 0.4000)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(1, p(strong("Cohen's d =")), textOutput("res_cv_d12"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_f13", "Cohen's f", 0.8137)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Partial \u03B72 =")), textOutput("res_cv_peta13")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_peta14", "Partial \u03B72", 0.3984)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's f =")), textOutput("res_cv_f14"))
               ),
               hr(),
               fluidRow(class="conv-row",
                        column(3, numericInput("cv_f15", "Cohen's f", 0.7361)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("\u03C92 =")), textOutput("res_cv_w2_1")),
                        
                        column(1, HTML("<hr style='width:1px; height:50px; background-color:#ddd;'>")),
                        
                        column(2, numericInput("cv_w2_16", "\u03C92", 0.3514)),
                        column(1, HTML("<h4>&#8596;</h4>")),
                        column(2, p(strong("Cohen's f =")), textOutput("res_cv_f16"))
               )
             )
    )
  )
);

# ==========================================
# Server 邏輯
# ==========================================
server <- function(input, output, session) {
  
  # --- 1. Means ---
  output$m1_var_out <- renderDT({
    var_x <- input$m1_sd^2;
    df <- data.frame(Metric = "VAR(x)", Value = fmt(var_x));
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$m1_es_out <- renderDT({
    d <- abs(input$m1_mean - input$m1_mu0) / input$m1_sd;
    stars <- get_es_stars(d, "d");
    df <- data.frame(ES = "Cohen's d", Value = fmt(d), Small = stars[1], Moderate = stars[2], Large = stars[3]);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$mp_var_out <- renderDT({
    var_x <- input$mp_sd^2;
    df <- data.frame(Metric = "VAR(x) of Difference", Value = fmt(var_x));
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$mp_es_out <- renderDT({
    d <- abs(input$mp_diff - input$mp_mu0) / input$mp_sd;
    stars <- get_es_stars(d, "d");
    df <- data.frame(ES = "Cohen's d", Value = fmt(d), Small = stars[1], Moderate = stars[2], Large = stars[3]);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$m2_var_out <- renderDT({
    var1 <- input$m2_sd1^2;
    var2 <- input$m2_sd2^2;
    df <- data.frame(Metric = c("VAR(x) 1", "VAR(x) 2"), Value = c(fmt(var1), fmt(var2)));
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$m2_sds_out <- renderDT({
    sd_pool <- sqrt( ((input$m2_n1 - 1)*input$m2_sd1^2 + (input$m2_n2 - 1)*input$m2_sd2^2) / (input$m2_n1 + input$m2_n2 - 2) );
    df <- data.frame(Metric = c("Cohen's SD", "Glass' SD", "Hedges' SD"), 
                     Value = c(fmt(sd_pool), fmt(input$m2_sd1), fmt(sd_pool)));
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$m2_es_out <- renderDT({
    sd_pool <- sqrt( ((input$m2_n1 - 1)*input$m2_sd1^2 + (input$m2_n2 - 1)*input$m2_sd2^2) / (input$m2_n1 + input$m2_n2 - 2) );
    d <- abs(input$m2_mean1 - input$m2_mean2) / sd_pool;
    g_glass <- abs(input$m2_mean1 - input$m2_mean2) / input$m2_sd1;
    j_corr <- 1 - (3 / (4 * (input$m2_n1 + input$m2_n2) - 9));
    g_hedges <- d * j_corr;
    
    st_d <- get_es_stars(d, "d");
    st_g <- get_es_stars(g_glass, "d");
    st_h <- get_es_stars(g_hedges, "d");
    
    df <- data.frame(
      ES = c("Cohen's d", "Glass' \u0394", "Hedges' g"),
      Value = c(fmt(d), fmt(g_glass), fmt(g_hedges)),
      Small = c(st_d[1], st_g[1], st_h[1]),
      Moderate = c(st_d[2], st_g[2], st_h[2]),
      Large = c(st_d[3], st_g[3], st_h[3])
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  # --- 2. Proportions & Mann-Whitney ---
  output$p1_calc_out <- renderDT({
    phat <- input$p1_n / input$p1_tot;
    z <- (phat - input$p1_test) / sqrt(input$p1_test * (1 - input$p1_test) / input$p1_tot);
    df <- data.frame(Metric = c("p(hat)", "z"), Value = c(fmt(phat), fmt(z)));
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$p1_es_out <- renderDT({
    phat <- input$p1_n / input$p1_tot;
    z <- (phat - input$p1_test) / sqrt(input$p1_test * (1 - input$p1_test) / input$p1_tot);
    h <- 2 * asin(sqrt(phat)) - 2 * asin(sqrt(input$p1_test));
    r <- z / sqrt(input$p1_n);
    
    st_h <- get_es_stars(h, "d");
    st_r <- get_es_stars(r, "r");
    
    df <- data.frame(
      ES = c("Cohen's h", "r"),
      Value = c(fmt(h), fmt(r)),
      Small = c(st_h[1], st_r[1]),
      Moderate = c(st_h[2], st_r[2]),
      Large = c(st_h[3], st_r[3])
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$p2_calc_out <- renderDT({
    p1 <- input$p2_n1 / input$p2_tot1;
    p2 <- input$p2_n2 / input$p2_tot2;
    pbar <- (input$p2_n1 + input$p2_n2) / (input$p2_tot1 + input$p2_tot2);
    z <- (p1 - p2) / sqrt(pbar * (1 - pbar) * (1/input$p2_tot1 + 1/input$p2_tot2));
    df <- data.frame(Metric = c("p(hat1)", "p(hat2)", "p(bar)", "z"), 
                     Value = c(fmt(p1), fmt(p2), fmt(pbar), fmt(z)));
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$p2_es_out <- renderDT({
    p1 <- input$p2_n1 / input$p2_tot1;
    p2 <- input$p2_n2 / input$p2_tot2;
    pbar <- (input$p2_n1 + input$p2_n2) / (input$p2_tot1 + input$p2_tot2);
    z <- (p1 - p2) / sqrt(pbar * (1 - pbar) * (1/input$p2_tot1 + 1/input$p2_tot2));
    
    h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2));
    r <- abs(z) / sqrt(input$p2_n1 + input$p2_n2); 
    
    a <- input$p2_n1; b <- input$p2_tot1 - a;
    c <- input$p2_n2; d <- input$p2_tot2 - c;
    yules_q <- (a*d - b*c) / (a*d + b*c);
    
    st_h <- get_es_stars(h, "d");
    st_r <- get_es_stars(r, "r");
    st_q <- get_es_stars(yules_q, "r");
    
    df <- data.frame(
      ES = c("Cohen's h", "r", "Yule's Q"),
      Value = c(fmt(h), fmt(r), fmt(yules_q)),
      Small = c(st_h[1], st_r[1], st_q[1]),
      Moderate = c(st_h[2], st_r[2], st_q[2]),
      Large = c(st_h[3], st_r[3], st_q[3])
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$mw_es_out <- renderDT({
    n1 <- input$mw_n1; n2 <- input$mw_n2; u <- input$mw_u;
    w_r <- abs(1 - 2 * u / (n1 * n2));
    cliff <- abs(1 - 2 * u / (n1 * n2));
    
    st_w <- get_es_stars(w_r, "r");
    st_c <- get_es_stars(cliff, "r");
    
    df <- data.frame(
      ES = c("Wendt's r", "Cliff's \u0394"),
      Value = c(fmt(w_r), fmt(cliff)),
      Small = c(st_w[1], st_c[1]),
      Moderate = c(st_w[2], st_c[2]),
      Large = c(st_w[3], st_c[3])
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  # --- 3. Regression, Chi-Square, ANOVA ---
  output$lr_es_out <- renderDT({
    f2 <- input$lr_r2 / (1 - input$lr_r2);
    st <- get_es_stars(f2, "f2");
    df <- data.frame(ES = "Cohen's f2", Value = fmt(f2), Small = st[1], Moderate = st[2], Large = st[3]);
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$chi_es_out <- renderDT({
    n <- input$chi_n; x2 <- input$chi_x2; r <- input$chi_r; c <- input$chi_c;
    phi <- sqrt(x2 / n);
    w <- phi * sqrt(min(r, c)); 
    v <- sqrt(x2 / (n * (min(r, c) - 1)));
    cc <- sqrt(x2 / (x2 + n));
    fisher_z <- 0.5 * log((1 + phi) / (1 - phi));
    
    s_p <- get_es_stars(phi, "r"); s_w <- get_es_stars(w, "r"); s_v <- get_es_stars(v, "r");
    s_cc <- get_es_stars(cc, "r"); s_z <- get_es_stars(fisher_z, "r");
    
    df <- data.frame(
      ES = c("\u03D5 Coefficient", "Cohen's W", "Cramer's V", "Contingency Coefficient", "r", "Fisher's Z"),
      Value = c(fmt(phi), fmt(w), fmt(v), fmt(cc), fmt(phi), fmt(fisher_z)),
      Small = c(s_p[1], s_w[1], s_v[1], s_cc[1], s_p[1], s_z[1]),
      Moderate = c(s_p[2], s_w[2], s_v[2], s_cc[2], s_p[2], s_z[2]),
      Large = c(s_p[3], s_w[3], s_v[3], s_cc[3], s_p[3], s_z[3])
    );
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  output$aov_calc_out <- renderDT({
    ss_t <- input$aov_ss_a + input$aov_ss_w;
    df_t <- input$aov_df_a + input$aov_df_w;
    ms_a <- input$aov_ss_a / input$aov_df_a;
    ms_w <- input$aov_ss_w / input$aov_df_w;
    ms_t <- ss_t / df_t;
    
    df <- data.frame(
      Effect = c("Among", "Within", "Total"),
      SS = c(fmt(input$aov_ss_a), fmt(input$aov_ss_w), fmt(ss_t)),
      df = c(input$aov_df_a, input$aov_df_w, df_t),
      MS = c(fmt(ms_a), fmt(ms_w), fmt(ms_t))
    );
    datatable(df, options = dt_opts_nohead, rownames = FALSE);
  });
  
  output$aov_es_out <- renderDT({
    ss_t <- input$aov_ss_a + input$aov_ss_w;
    ms_w <- input$aov_ss_w / input$aov_df_w;
    eta2 <- input$aov_ss_a / ss_t;
    p_eta2 <- eta2; 
    omega2 <- (input$aov_ss_a - input$aov_df_a * ms_w) / (ss_t + ms_w);
    r_intra <- sqrt(input$aov_ss_a / (input$aov_ss_a + input$aov_df_a * ms_w));
    
    st_e <- get_es_stars(eta2, "eta2");
    st_o <- get_es_stars(omega2, "eta2");
    
    df <- data.frame(
      ES = c("\u03B72", "Partial \u03B72", "\u03C92", "r (intraCorr)"),
      Value = c(fmt(eta2), fmt(p_eta2), fmt(omega2), fmt(r_intra)),
      Small = c(st_e[1], st_e[1], st_o[1], ""),
      Moderate = c(st_e[2], st_e[2], st_o[2], ""),
      Large = c(st_e[3], st_e[3], st_o[3], "")
    );
    
    df[4, 5] <- "*"; 
    datatable(df, options = dt_opts, rownames = FALSE);
  });
  
  # --- 4. Effect Size Converter ---
  output$res_cv_r1 <- renderText({ fmt(input$cv_d1 / sqrt(input$cv_d1^2 + 4)) });
  output$res_cv_d2 <- renderText({ fmt(2 * input$cv_r2 / sqrt(1 - input$cv_r2^2)) });
  
  output$res_cv_f1 <- renderText({ fmt(input$cv_d3 / 2) });
  output$res_cv_d4 <- renderText({ fmt(input$cv_f4 * 2) });
  
  output$res_cv_lo1 <- renderText({ fmt(input$cv_d5 * pi / sqrt(3)) });
  output$res_cv_d6 <- renderText({ fmt(input$cv_lo6 * sqrt(3) / pi) });
  
  output$res_cv_auc1 <- renderText({ fmt(pnorm(input$cv_d7 / sqrt(2))) });
  output$res_cv_d8 <- renderText({ fmt(sqrt(2) * qnorm(input$cv_auc8)) });
  
  output$res_cv_z1 <- renderText({ fmt(0.5 * log((1 + input$cv_r9) / (1 - input$cv_r9))) });
  output$res_cv_r10 <- renderText({ fmt((exp(2 * input$cv_z10) - 1) / (exp(2 * input$cv_z10) + 1)) });
  
  output$res_cv_peta1 <- renderText({ 
    t_sq <- (input$cv_d11^2 * input$cv_n11) / 4; 
    fmt(t_sq / (t_sq + input$cv_n11 - 1)); 
  });
  output$res_cv_d12 <- renderText({ 
    fmt(sqrt( (input$cv_peta12 * 4 * (input$cv_n12 - 1)) / (input$cv_n12 * (1 - input$cv_peta12)) ));
  });
  
  output$res_cv_peta13 <- renderText({ fmt(input$cv_f13^2 / (1 + input$cv_f13^2)) });
  output$res_cv_f14 <- renderText({ fmt(sqrt(input$cv_peta14 / (1 - input$cv_peta14))) });
  
  output$res_cv_w2_1 <- renderText({ fmt(input$cv_f15^2 / (1 + input$cv_f15^2)) });
  output$res_cv_f16 <- renderText({ fmt(sqrt(input$cv_w2_16 / (1 - input$cv_w2_16))) });
  
}

shinyApp(ui = ui, server = server);