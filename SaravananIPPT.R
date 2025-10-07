# ------------------------------------------------------------
# IPPT Results Analysis — Shiny App
# ------------------------------------------------------------
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)

# ---------- Helpers ----------
REQUIRED <- c("NAME","SITUP_PRE","SITUP_POST","PUSHUP_PRE","PUSHUP_POST","RUN_PRE","RUN_POST")

# Normalize incoming column names to our canonical REQUIRED names (robust to case/spacing)
normalize_headers <- function(df) {
  nm  <- names(df)
  key <- tolower(nm)
  key <- gsub("\\s+", "_", key)
  key <- gsub("\\.", "_", key)
  key <- gsub("__+", "_", key)
  key <- trimws(key)
  
  map <- c(
    "name"        = "NAME",
    "situp_pre"   = "SITUP_PRE",
    "situp_post"  = "SITUP_POST",
    "pushup_pre"  = "PUSHUP_PRE",
    "pushup_post" = "PUSHUP_POST",
    "run_pre"     = "RUN_PRE",
    "run_post"    = "RUN_POST"
  )
  new_names <- ifelse(key %in% names(map), map[key], nm)
  names(df) <- new_names
  df
}

# Parse numeric; if value looks like time "mm:ss" or "hh:mm:ss", convert to seconds
parse_to_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  y <- as.character(x)
  y <- trimws(y)
  res <- suppressWarnings(as.numeric(y))
  
  need_time <- is.na(res) & grepl(":", y)
  if (any(need_time)) {
    secs <- sapply(strsplit(y[need_time], ":"), function(p) {
      p <- suppressWarnings(as.numeric(p))
      if (any(is.na(p))) return(NA_real_)
      if (length(p) == 2) return(p[1]*60 + p[2])
      if (length(p) == 3) return(p[1]*3600 + p[2]*60 + p[3])
      NA_real_
    })
    res[need_time] <- secs
  }
  res
}

# Coerce numeric measurement columns safely
coerce_numeric <- function(df) {
  for (c in REQUIRED[REQUIRED != "NAME"]) {
    if (c %in% names(df)) {
      df[[c]] <- parse_to_numeric(df[[c]])
    }
  }
  df
}

# Compute improvements per your rules; NA if either input is NA
add_improvements <- function(df) {
  df |>
    mutate(
      SITUP_IMPROVEMENT  = case_when(
        is.na(SITUP_POST) | is.na(SITUP_PRE) ~ NA_character_,
        SITUP_POST - SITUP_PRE > 0 ~ "YES",
        TRUE ~ "NO"
      ),
      PUSHUP_IMPROVEMENT = case_when(
        is.na(PUSHUP_POST) | is.na(PUSHUP_PRE) ~ NA_character_,
        PUSHUP_POST - PUSHUP_PRE > 0 ~ "YES",
        TRUE ~ "NO"
      ),
      RUN_IMPROVEMENT    = case_when(
        is.na(RUN_PRE) | is.na(RUN_POST) ~ NA_character_,
        RUN_PRE - RUN_POST > 0 ~ "YES",  # faster is better
        TRUE ~ "NO"
      )
    )
}

# Validate structure: ensure all REQUIRED columns exist
validate_structure <- function(df) {
  all(REQUIRED %in% names(df))
}

# ---------- UI ----------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card {
        border: 1px solid #e0e0e0; border-radius: 14px; padding: 18px 22px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06); background: #fff;
        max-width: 920px;
      }
      .kv { display:flex; gap:10px; align-items:baseline; margin: 6px 0; }
      .kv label { width: 180px; font-weight: 700; }
      .pill { padding: 3px 10px; border-radius: 999px; color: #fff; font-weight: 700; }
      .pill-yes { background: #2e7d32; }   /* green */
      .pill-no  { background: #c62828; }   /* red */
      .pill-na  { background: #9e9e9e; }   /* grey */
      .subtitle { font-size: 13px; color: #666; margin-top: -8px; }
      .stat-box {
        background: #f9fafb; border: 1px solid #e5e7eb; border-radius: 12px; padding: 14px 16px;
      }
    "))
  ),
  
  titlePanel("IPPT Results Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload IPPT_Template (.xlsx)", accept = c(".xlsx")),
      div(class = "subtitle",
          "Required columns (any case/spacing): NAME, SITUP_PRE, SITUP_POST, PUSHUP_PRE, PUSHUP_POST, RUN_PRE, RUN_POST"),
      hr(),
      h4("Preview (first 10 rows)"),
      DTOutput("preview")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("IPPT DETAILS",
                 br(),
                 uiOutput("name_selector"),
                 br(),
                 uiOutput("details_card")
        ),
        tabPanel("IPPT Statistics",
                 br(),
                 selectInput("metric", "Choose Metric:", c("SITUP","PUSHUP","RUN")),
                 br(),
                 plotOutput("scatter", height = "420px"),
                 br(),
                 div(class="stat-box",
                     h4("Paired t-test (POST vs PRE)"),
                     verbatimTextOutput("ttest_txt"),
                     h5("p-value"),
                     verbatimTextOutput("pval_txt")
                 )
        )
      )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # Reactive dataset: read + clean whenever a file is uploaded
  data_all <- reactive({
    req(input$file)
    df <- readxl::read_excel(input$file$datapath)
    df <- normalize_headers(df)
    validate(need(validate_structure(df),
                  "Uploaded file does not contain all required columns: NAME, SITUP_PRE, SITUP_POST, PUSHUP_PRE, PUSHUP_POST, RUN_PRE, RUN_POST."
    ))
    df <- coerce_numeric(df)
    df <- add_improvements(df)
    df
  })
  
  # Preview table (first 10 rows)
  output$preview <- renderDT({
    req(data_all())
    datatable(head(data_all(), 10), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # --- Tab 1: Name selector (alphabetical) ---
  output$name_selector <- renderUI({
    req(data_all())
    choices <- sort(unique(data_all()$NAME))
    selectInput("person", "Select Name:", choices = choices)
  })
  
  # --- Tab 1: Details card ---
  output$details_card <- renderUI({
    req(data_all(), input$person)
    row <- data_all() |> filter(NAME == input$person) |> slice(1)
    
    pill <- function(val) {
      if (is.na(val) || !nzchar(val)) return(tags$span(class="pill pill-na", "NA"))
      if (toupper(val) == "YES") return(tags$span(class="pill pill-yes", "YES"))
      tags$span(class="pill pill-no", "NO")
    }
    
    tags$div(class="card",
             div(class="kv", tags$label("NAME"),            tags$div(tags$strong(row$NAME))),
             tags$hr(),
             div(class="kv", tags$label("SITUP_PRE"),       tags$div(row$SITUP_PRE)),
             div(class="kv", tags$label("SITUP_POST"),      tags$div(row$SITUP_POST)),
             div(class="kv", tags$label("SITUP_IMPROVEMENT"),  tags$div(pill(row$SITUP_IMPROVEMENT))),
             tags$hr(),
             div(class="kv", tags$label("PUSHUP_PRE"),      tags$div(row$PUSHUP_PRE)),
             div(class="kv", tags$label("PUSHUP_POST"),     tags$div(row$PUSHUP_POST)),
             div(class="kv", tags$label("PUSHUP_IMPROVEMENT"), tags$div(pill(row$PUSHUP_IMPROVEMENT))),
             tags$hr(),
             div(class="kv", tags$label("RUN_PRE"),         tags$div(row$RUN_PRE)),
             div(class="kv", tags$label("RUN_POST"),        tags$div(row$RUN_POST)),
             div(class="kv", tags$label("RUN_IMPROVEMENT"), tags$div(pill(row$RUN_IMPROVEMENT)))
    )
  })
  
  # --- Tab 2: Scatter + Paired t-test ---
  metric_cols <- reactive({
    req(input$metric, data_all())
    switch(input$metric,
           "SITUP"  = list(pre = "SITUP_PRE",  post = "SITUP_POST",  xlab = "SITUP_PRE",  ylab = "SITUP_POST"),
           "PUSHUP" = list(pre = "PUSHUP_PRE", post = "PUSHUP_POST", xlab = "PUSHUP_PRE", ylab = "PUSHUP_POST"),
           "RUN"    = list(pre = "RUN_PRE",    post = "RUN_POST",    xlab = "RUN_PRE",    ylab = "RUN_POST")
    )
  })
  
  complete_pairs <- reactive({
    req(metric_cols(), data_all())
    cols <- metric_cols()
    data_all() |>
      select(all_of(c("NAME", cols$pre, cols$post))) |>
      filter(!is.na(.data[[cols$pre]]), !is.na(.data[[cols$post]]))
  })
  
  output$scatter <- renderPlot({
    df <- complete_pairs(); req(nrow(df) > 0)
    cols <- metric_cols()
    
    ggplot(df, aes(x = .data[[cols$pre]], y = .data[[cols$post]])) +
      geom_point(size = 2, alpha = 0.85) +
      geom_abline(slope = 1, intercept = 0, linetype = 2) +
      labs(x = cols$xlab, y = cols$ylab, title = paste0(input$metric, ": POST vs PRE")) +
      theme_minimal(base_size = 14)
  })
  
  # Paired t-test and p-value (no broom dependency)
  output$ttest_txt <- renderText({
    df <- complete_pairs(); req(nrow(df) >= 2)
    cols <- metric_cols()
    tt  <- t.test(df[[cols$post]], df[[cols$pre]], paired = TRUE)
    
    t_val   <- unname(tt$statistic)
    df_val  <- unname(tt$parameter)
    mean_d  <- as.numeric(tt$estimate)  # mean of (post - pre)
    ci_low  <- tt$conf.int[1]
    ci_high <- tt$conf.int[2]
    
    paste(
      sprintf("t = %.4f, df = %d", t_val, as.integer(df_val)),
      sprintf("mean diff (POST - PRE) = %.4f", mean_d),
      sprintf("95%% CI = [%.4f, %.4f]", ci_low, ci_high),
      sep = "\n"
    )
  })
  
  output$pval_txt <- renderText({
    df <- complete_pairs(); req(nrow(df) >= 2)
    cols <- metric_cols()
    tt <- t.test(df[[cols$post]], df[[cols$pre]], paired = TRUE)
    format.pval(tt$p.value, digits = 4, eps = 1e-04)
  })
}

shinyApp(ui, server)
