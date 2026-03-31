library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(survival)
library(ggfortify)
library(plotly)
library(DT)
library(scales)
library(munsell)

# --- 1. 數據預處理 ---
df <- read_csv("Netflix_Title_Cleaned.csv") %>%
  mutate(date_added = parse_date_time(date_added, orders = c("mdy", "dmy", "ymd"))) %>%
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

# --- 2. UI 介面 ---
ui <- page_navbar(
  title = "Netflix Asset Analytics",
  theme = bs_theme(version = 5, preset = "flatly", base_font = "sans-serif"),
  
  sidebar = sidebar(
    title = uiOutput("side_title"),
    radioButtons("lang", "Language", choices = c("English" = "en", "繁體中文" = "zh")),
    hr(),
    uiOutput("ui_controls"),
    hr(),
    p("Source: Kaggle Netflix Dataset", style="font-size:0.8rem; color:gray;")
  ),
  
  # KPI 三大指標：加入動態顏色與修正 Tooltip
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = uiOutput("kpi_1_t"),
      value = textOutput("kpi_1_v"),
      showcase = bs_icon("cash-stack"),
      # 加入工具提示圖示
      title_side = tooltip(bs_icon("info-circle"), uiOutput("tip_k1")),
      theme = "primary"
    ),
    value_box(
      title = uiOutput("kpi_2_t"),
      value = textOutput("kpi_2_v"),
      showcase = bs_icon("graph-down-arrow"),
      title_side = tooltip(bs_icon("info-circle"), uiOutput("tip_k2")),
      theme = "secondary"
    ),
    # 這個指標顏色會隨狀態變動 (紅/黃/綠)
    uiOutput("kpi_3_box")
  ),
  
  layout_columns(
    col_widths = c(7, 5),
    card(
      full_screen = TRUE,
      card_header(uiOutput("main_plot_t")),
      plotOutput("survPlot", height = "400px")
    ),
    card(
      card_header(uiOutput("tree_t")),
      plotlyOutput("treePlot", height = "400px")
    )
  ),
  
  # 警示語放置處
  card(uiOutput("legal_f"), style = "margin-top: 20px; border: none; background: #f8f9fa;")
)

# --- 3. Server 邏輯 ---
server <- function(input, output, session) {
  
  i18n <- reactive({
    if (input$lang == "zh") {
      list(side_t = "決策面板", type_t = "內容類別", cost_t = "平均成本 (M USD)",
           year_t = "評估年限", k1_t = "預期殘值", k2_t = "預期折舊",
           k3_t = "安全指數", pt = "資產衰減分析", tt = "配置權重",
           s = "安全", w = "警戒", c = "危險", x_lab = "時間 (年)", y_lab = "存續率",
           t1 = "目標年限後剩餘價值", t2 = "預期損失金額", t3 = "資金留存百分比",
           lgl = "【法律聲明】本程式僅供技術演示，不構成投資建議。")
    } else {
      list(side_t = "Decision Panel", type_t = "Category", cost_t = "Avg Cost (M USD)",
           year_t = "Eval Year", k1_t = "Residual Value", k2_t = "Capital Loss",
           k3_t = "Security Index", pt = "Asset Decay", tt = "Weight",
           s = "Safe", w = "Warning", c = "Critical", x_lab = "Time (Y)", y_lab = "Prob.",
           t1 = "Value at target year", t2 = "Expected loss", t3 = "Capital retention %",
           lgl = "[Disclaimer] Tech demo only. No investment advice.")
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
  
  # 動態生成的第三個 KPI Box (處理顏色變動)
  output$kpi_3_box <- renderUI({
    pct <- round(mean(summary(fit_model(), times = input$year_range)$surv) * 100, 0)
    
    # 根據百分比決定主題顏色
    box_theme <- if (pct >= 60) "success" else if (pct >= 30) "warning" else "danger"
    status_t <- if (pct >= 60) i18n()$s else if (pct >= 30) i18n()$w else i18n()$c
    
    value_box(
      title = i18n()$k3_t,
      value = paste0(status_t, " (", pct, "%)"),
      showcase = bs_icon("shield-lock-fill"),
      title_side = tooltip(bs_icon("info-circle"), i18n()$t3),
      theme = box_theme
    )
  })
  
  # --- 修正後的繪圖：加入 coord_cartesian 確保 X 軸會動 ---
  output$survPlot <- renderPlot({
    autoplot(fit_model(), conf.int = TRUE) +
      scale_fill_manual(values = c("#FFD700", "#FFD700")) +
      scale_color_manual(values = c("#B22222", "#4682B4")) +
      # 這行是關鍵：強制 X 軸範圍等於滑桿數值
      coord_cartesian(xlim = c(0, input$year_range)) +
      labs(x = i18n()$x_lab, y = i18n()$y_lab) +
      theme_minimal()
  })
  
  output$treePlot <- renderPlotly({
    t_data <- filtered_df() %>% count(asset_class)
    plot_ly(t_data, type = "treemap", labels = ~asset_class, parents = "", values = ~n)
  })
  
  # 標籤與提示語
  output$kpi_1_t <- renderUI(i18n()$k1_t); output$kpi_2_t <- renderUI(i18n()$k2_t)
  output$tip_k1 <- renderUI(i18n()$t1); output$tip_k2 <- renderUI(i18n()$t2)
  output$main_plot_t <- renderUI(i18n()$pt); output$tree_t <- renderUI(i18n()$tt)
  output$legal_f <- renderUI(p(i18n()$lgl, style="color:gray; font-size:0.8rem;"))
}

shinyApp(ui, server)