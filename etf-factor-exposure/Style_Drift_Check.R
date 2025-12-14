# ── Libraries ────────────────────────────────────────────────────────────────
library(shiny)
library(tidyverse)
library(lubridate)
library(quantmod)
library(broom)
library(zoo)

# ── Helper: Fama-French Loader ────────────────────────────────────────────────
load_ff_factors <- function(region) {
  
  urls <- list(
    "USA" = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",
    "Europe" = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_3_Factors_CSV.zip",
    "Developed ex US" = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_3_Factors_CSV.zip",
    "Emerging Markets" = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Emerging_3_Factors_CSV.zip"
  )
  
  temp <- tempfile()
  download.file(urls[[region]], temp, mode = "wb", quiet = TRUE)
  files <- unzip(temp, list = TRUE)
  
  raw <- read_csv(
    unzip(temp, files = files$Name[1]),
    skip = 3,
    col_names = FALSE,
    show_col_types = FALSE
  )
  
  raw %>%
    filter(str_detect(X1, "^\\d{6}$")) %>%   # entfernt Jahreszeilen
    select(X1, X2, X3, X4, X5) %>%
    rename(
      Month = X1,
      `Mkt-RF` = X2,
      SMB = X3,
      HML = X4,
      RF = X5
    ) %>%
    mutate(
      month = ymd(paste0(Month, "01")) + months(1) - days(1),
      across(`Mkt-RF`:RF, ~ as.numeric(str_replace_all(., "[^0-9\\.-]", "")))
    ) %>%
    select(month, `Mkt-RF`, SMB, HML, RF)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  titlePanel("ETF Factor Exposure Analysis (Fama–French 3 Factors)"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("etf", "ETF Ticker (Yahoo Finance):", value = "SPY"),
      
      selectInput(
        "region",
        "Factor Region:",
        choices = c(
          "USA",
          "Europe",
          "Developed ex US",
          "Emerging Markets"
        ),
        selected = "USA"
      ),
      
      dateRangeInput(
        "dates",
        "Zeitraum:",
        start = "2018-01-01",
        end = Sys.Date()
      ),
      
      numericInput(
        "window",
        "Rolling-Fenster (Monate):",
        value = 12,
        min = 6,
        max = 60
      ),
      
      actionButton("run", "Analyse starten")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Regression", tableOutput("reg_table")),
        tabPanel("Rolling Betas", plotOutput("rolling_plot"))
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output) {
  
  observeEvent(input$run, {
    
    # 1️⃣ Faktoren laden
    ff_factors <- load_ff_factors(input$region)
    
    # 2️⃣ ETF-Daten laden
    ret_xts <- tryCatch({
      getSymbols(
        input$etf,
        src = "yahoo",
        from = input$dates[1],
        to = input$dates[2],
        auto.assign = FALSE
      ) %>%
        Cl() %>%
        monthlyReturn()
    }, error = function(e) {
      showNotification("ETF konnte nicht geladen werden.", type = "error")
      return(NULL)
    })
    
    req(ret_xts)
    
    etf_returns <- tibble(
      month = index(ret_xts),
      GrossReturn = coredata(ret_xts)
    )
    
    # 3️⃣ Merge + Excess Returns
    data <- etf_returns %>%
      inner_join(ff_factors, by = "month") %>%
      mutate(ExcessReturn = GrossReturn * 100 - RF)
    
    validate(
      need(nrow(data) >= input$window, "Zu wenige Beobachtungen für Rolling Regression.")
    )
    
    # 4️⃣ Klassische Regression
    model <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = data)
    results <- tidy(model, conf.int = TRUE, digits = 3)
    
    output$reg_table <- renderTable({
      results
    })
    
    # 5️⃣ Rolling Regression
    roll_data <- data %>%
      select(ExcessReturn, `Mkt-RF`, SMB, HML)
    
    rolling_betas <- rollapply(
      roll_data,
      width = input$window,
      by.column = FALSE,
      align = "right",
      fill = NA,
      FUN = function(x) {
        fit <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = as.data.frame(x))
        coef(fit)[-1]
      }
    )
    
    rolling_df <- tibble(
      month = data$month,
      Mkt_RF = rolling_betas[, 1],
      SMB = rolling_betas[, 2],
      HML = rolling_betas[, 3]
    )
    
    # 6️⃣ Rolling Plot
    output$rolling_plot <- renderPlot({
      rolling_df %>%
        pivot_longer(
          cols = c(Mkt_RF, SMB, HML),
          names_to = "Factor",
          values_to = "Beta"
        ) %>%
        ggplot(aes(x = month, y = Beta, color = Factor)) +
        geom_line(linewidth = 1) +
        labs(
          title = paste(
            "Rolling Betas (",
            input$window,
            " Monate) –",
            input$etf,
            "|",
            input$region
          ),
          x = "Monat",
          y = "Beta"
        ) +
        theme_minimal()
    })
    
  })
}

# ── App starten ───────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
