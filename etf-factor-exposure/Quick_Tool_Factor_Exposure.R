# ── Libraries ────────────────────────────────────────────────────────────────
library(shiny)
library(tidyverse)
library(lubridate)
library(quantmod)
library(broom)

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
    filter(str_detect(X1, "^\\d{6}$")) %>%      # remove annual rows
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
        "Timeframe:",
        start = "2018-01-01",
        end = Sys.Date()
      ),
      
      actionButton("run", "Analyse starten")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Regression", tableOutput("reg_table")),
        tabPanel("Factor Betas", plotOutput("beta_plot"))
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output) {
  
  observeEvent(input$run, {
    
    # 1️⃣ Load factors
    ff_factors <- load_ff_factors(input$region)
    
    # 2️⃣ Load ETF data
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
    
    # 3️⃣ Merge & Excess returns
    data <- etf_returns %>%
      inner_join(ff_factors, by = "month") %>%
      mutate(ExcessReturn = GrossReturn * 100 - RF)
    
    validate(
      need(nrow(data) >= 24, "Zu wenige Beobachtungen für Regression.")
    )
    
    # 4️⃣ Regression
    model <- lm(ExcessReturn ~ `Mkt-RF` + SMB + HML, data = data)
    results <- tidy(model, conf.int = TRUE, digits = 3)
    
    # 5️⃣ Table
    output$reg_table <- renderTable({
      results
    })
    
    # 6️⃣ Plot
    output$beta_plot <- renderPlot({
      results %>%
        filter(term != "(Intercept)") %>%
        ggplot(aes(x = term, y = estimate)) +
        geom_col(fill = "steelblue") +
        geom_errorbar(
          aes(ymin = conf.low, ymax = conf.high),
          width = 0.2
        ) +
        labs(
          title = paste(
            input$etf,
            "Factor Exposure –",
            input$region
          ),
          y = "Beta",
          x = ""
        ) +
        theme_minimal()
    })
    
  })
}

# ── Run App ───────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
