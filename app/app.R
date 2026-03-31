library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(survival)
library(ggfortify) # 核心：替代 survminer 以避免網頁端報錯
library(plotly)
library(DT)
library(scales)
library(munsell)

# --- 1. 數據預處理 (修正解析失敗問題) ---
df <- read_csv("Netflix_Title_Cleaned.csv") %>%
  mutate(
    # 自動識別多種日期格式，解決 642 筆失敗問題
    date_added = parse_date_time(date_added, orders = c("mdy", "dmy", "ymd"))
  ) %>%
  filter(!is.na(date_added)) %>%
  mutate(
    lag_years = year(date_added) - release_year,
    status = 1,
    time = lag_years + 0.01,
    asset_class = case_when(
      lag_years <= 1 ~ "Fresh",
      lag_years <= 5 ~ "Mature",
      lag_years <= 20 ~ "Aging",
      TRUE ~ "Legacy"
    )
  ) %>%
  filter(lag_years >= 0)

# --- 2. UI 介面 (修正字體函式錯誤) ---
ui <- page_navbar(
  theme = bs_theme(
    version = 5, 
    preset = "flatly",
    base_font = "sans-serif" # 直接使用字串，避免 font_system 報錯
  ),
  
  sidebar = sidebar(
    title = uiOutput("side_title"),
    radioButtons("lang", "Interface Language", choices = c("English" = "en", "繁體中文" = "zh")),
    hr(),
    uiOutput("ui_controls"),
    hr(),
    uiOutput("source_note")
  ),
  
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = uiOutput("kpi_1_t"), value = textOutput("kpi_1_v"), showcase = bs_icon("cash-stack"), theme = "light"),
    value_box(title = uiOutput("kpi_2_t"), value = textOutput("kpi_2_v"), showcase = bs_icon("graph-down-arrow"), theme = "light"),
    value_box(title = uiOutput("kpi_3_t"), value = textOutput("kpi_3_v"), showcase = uiOutput("kpi_3_i"), theme = "light")
  ),
  
  layout_columns(
    col_widths = c(7, 5),
    card(
      navset_card_underline(
        title = uiOutput("main_plot_t"),
        nav_panel(uiOutput("nav_p"), plotOutput("survPlot", height = "400px")),
        nav_panel(uiOutput("nav_d"), DTOutput("detailTable"))
      )
    ),
    card(card_header(uiOutput("tree_t")), plotlyOutput("treePlot", height = "450px"))
  ),
  uiOutput("legal_f")
)

# --- 3. Server 邏輯 (修正繪圖函式) ---
server <- function(input, output, session) {
  
  i18n <- reactive({
    if (input$lang == "zh") {
      list(side_t = "分析面板", type_t = "內容類別", cost_t = "平均單部成本 (M USD)",
           year_t = "評估年限", k1_t = "預期資產殘值", k2_t = "預期折舊損失",
           k3_t = "資金安全指數", pt = "資產價值衰減分析", tt = "資產配置權重",
           nav_p = "衰減曲線", nav_d = "清單明細", s = "安全", w = "警戒", c = "危險",
           x_lab = "時間 (年)", y_lab = "存續機率", src = "數據來源：Kaggle Netflix",
           lgl = "聲明：本程式為技術演示。", class_map = c("Fresh"="核心", "Mature"="成熟", "Aging"="老化", "Legacy"="沈沒"))
    } else {
      list(side_t = "Decision Panel", type_t = "Asset Category", cost_t = "Avg. Cost (M USD)",
           year_t = "Eval Year", k1_t = "Residual Value", k2_t = "Capital Loss",
           k3_t = "Security Index", pt = "Asset Decay Analysis", tt = "Weight",
           nav_p = "Curve", nav_d = "Details", s = "Safe", w = "Warning", c = "Critical",
           x_lab = "Time (Years)", y_lab = "Probability", src = "Source: Kaggle",
           lgl = "Note: Technical demo only.", class_map = c("Fresh"="Fresh", "Mature"="Mature", "Aging"="Aging", "Legacy"="Legacy"))
    }
  })
  
  output$side_title <- renderUI(i18n()$side_t)
  output$ui_controls <- renderUI({
    tagList(
      selectInput("type_filter", i18n()$type_t, choices = c("All", "Movie", "TV Show")),
      sliderInput("unit_cost", i18n()$cost_t, min = 1, max = 10, value = 5, step = 1),
      sliderInput("year_range", i18n()$year_t, min = 1, max = 20, value = 5, step = 1)
    )
  })
  
  filtered_df <- reactive({
    d <- df
    if (!is.null(input$type_filter) && input$type_filter != "All") d <- d %>% filter(type == input$type_filter)
    d
  })
  
  fit_model <- reactive({ survfit(Surv(time, status) ~ type, data = filtered_df()) })
  
  # KPI 計算
  output$kpi_1_v <- renderText({
    summ <- summary(fit_model(), times = input$year_range)
    paste0("$", comma(round(nrow(filtered_df()) * input$unit_cost * mean(summ$surv), 0)), " M")
  })
  
  output$kpi_2_v <- renderText({
    summ <- summary(fit_model(), times = input$year_range)
    paste0("$", comma(round(nrow(filtered_df()) * input$unit_cost * (1-mean(summ$surv)), 0)), " M")
  })
  
  output$kpi_3_v <- renderText({
    pct <- round(mean(summary(fit_model(), times = input$year_range)$surv) * 100, 0)
    status_t <- if (pct >= 60) i18n()$s else if (pct >= 30) i18n()$w else i18n()$c
    paste0(status_t, " (", pct, "%)")
  })
  
  output$kpi_3_i <- renderUI({
    pct <- mean(summary(fit_model(), times = input$year_range)$surv) * 100
    color_v <- if (pct >= 60) "#2E8B57" else if (pct >= 30) "#DAA520" else "#B22222"
    bs_icon("shield-lock-fill", color = color_v)
  })
  
  # --- 修正後的繪圖函式 ---
  output$survPlot <- renderPlot({
    # 使用 autoplot 取代 ggsurvplot，避開 survMisc 依賴
    autoplot(fit_model(), conf.int = TRUE) +
      scale_fill_manual(values = c("#FFD700", "#FFD700")) +
      scale_color_manual(values = c("#B22222", "#4682B4")) +
      labs(x = i18n()$x_lab, y = i18n()$y_lab) +
      theme_minimal()
  })
  
  output$treePlot <- renderPlotly({
    t_data <- filtered_df() %>% count(asset_class) %>% mutate(display_name = i18n()$class_map[asset_class])
    plot_ly(t_data, type = "treemap", labels = ~display_name, parents = "", values = ~n)
  })
  
  output$detailTable <- renderDT({
    datatable(filtered_df() %>% select(title, type, release_year, asset_class), options = list(pageLength = 8))
  })
  
  output$kpi_1_t <- renderUI(i18n()$k1_t); output$kpi_2_t <- renderUI(i18n()$k2_t)
  output$kpi_3_t <- renderUI(i18n()$k3_t); output$main_plot_t <- renderUI(i18n()$pt)
  output$tree_t <- renderUI(i18n()$tt); output$nav_p <- renderUI(i18n()$nav_p)
  output$nav_d <- renderUI(i18n()$nav_d); output$source_note <- renderUI(i18n()$src)
  output$legal_f <- renderUI(i18n()$lgl)
}

shinyApp(ui, server)