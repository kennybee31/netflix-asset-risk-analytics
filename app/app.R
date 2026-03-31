library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(lubridate)
library(survival)

library(ggpubr)   # <--- survminer 的依賴
library(plotly)
library(DT)
library(scales)
library(munsell)

# --- 1. 數據預處理 (優化編碼以確保穩定) ---
df <- read_csv("Netflix_Title_Cleaned.csv") %>%
  mutate(date_added = mdy(date_added)) %>%
  filter(!is.na(date_added)) %>%
  mutate(
    lag_years = year(date_added) - release_year,
    status = 1,
    time = lag_years + 0.01,
    # 內部標籤維持英文，UI 再進行翻譯，避免系統編碼亂碼
    asset_class = case_when(
      lag_years <= 1 ~ "Fresh",
      lag_years <= 5 ~ "Mature",
      lag_years <= 20 ~ "Aging",
      TRUE ~ "Legacy"
    )
  ) %>%
  filter(lag_years >= 0)

# --- 2. UI 介面 ---
ui <- page_sidebar(
  title = "Netflix Asset Portfolio Strategic Report",
  theme = bs_theme(
    version = 5, bg = "#FFFFFF", fg = "#1A1A1A", 
    primary = "#003366", base_font = font_google("Inter")
  ),
  
  sidebar = sidebar(
    title = uiOutput("side_title"),
    radioButtons("lang", "Interface Language / 介面語言", 
                 choices = c("English" = "en", "繁體中文" = "zh")),
    hr(),
    uiOutput("ui_controls"), # 動態生成控制項以切換語言標籤
    hr(),
    uiOutput("source_note")
  ),
  
  # --- 指標卡片 (對齊優化) ---
  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(
      title = span(uiOutput("kpi_1_t"), tooltip(bs_icon("info-circle"), uiOutput("tip_k1"))),
      value = textOutput("kpi_1_v"), showcase = bs_icon("cash-stack"), theme = "light"
    ),
    value_box(
      title = span(uiOutput("kpi_2_t"), tooltip(bs_icon("info-circle"), uiOutput("tip_k2"))),
      value = textOutput("kpi_2_v"), showcase = bs_icon("graph-down-arrow"), theme = "light"
    ),
    value_box(
      title = span(uiOutput("kpi_3_t"), tooltip(bs_icon("info-circle"), uiOutput("tip_k3"))),
      value = textOutput("kpi_3_v"), showcase = uiOutput("kpi_3_i"), theme = "light"
    )
  ),
  
  # --- 圖表與資料細節 ---
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

# --- 3. Server 邏輯 ---
server <- function(input, output, session) {
  
  # --- 翻譯字典：深度對齊 ---
  i18n <- reactive({
    if (input$lang == "zh") {
      list(
        side_t = "分析面板", type_t = "內容類別", cost_t = "平均單部成本 (M USD)",
        year_t = "評估年限", k1_t = "預期資產殘值", k2_t = "預期折舊損失",
        k3_t = "資金安全指數", pt = "資產價值衰減分析", tt = "資產配置權重",
        nav_p = "衰減曲線", nav_d = "清單明細", 
        s = "安全", w = "警戒", c = "危險",
        t1 = "目標年限後剩餘資產估值", t2 = "因時間流逝導致的資本損失",
        t3 = "資金存續百分比", x_lab = "時間 (年)", y_lab = "存續機率",
        src = "數據來源：Kaggle Netflix Dataset",
        lgl = "【法律聲明】本程式僅供技術演示，不構成投資建議。金額均為假設模擬。",
        class_map = c("Fresh"="核心資產", "Mature"="成熟資產", "Aging"="老化資產", "Legacy"="沈沒成本")
      )
    } else {
      list(
        side_t = "Decision Panel", type_t = "Asset Category", cost_t = "Avg. Unit Cost (M USD)",
        year_t = "Evaluation Year", k1_t = "Residual Value", k2_t = "Capital Loss",
        k3_t = "Security Index", pt = "Asset Decay Analysis", tt = "Portfolio Weight",
        nav_p = "Curve", nav_d = "Details", 
        s = "Safe", w = "Warning", c = "Critical",
        t1 = "Est. value at target year", t2 = "Potential capital loss",
        t3 = "Capital retention percentage", x_lab = "Time (Years)", y_lab = "Survival Probability",
        src = "Source: Kaggle Netflix Dataset",
        lgl = "[Disclaimer] For tech demonstration only. No investment advice. Values are simulated.",
        class_map = c("Fresh"="Fresh", "Mature"="Mature", "Aging"="Aging", "Legacy"="Legacy")
      )
    }
  })
  
  # 動態渲染 UI
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
    if (!is.null(input$type_filter) && input$type_filter != "All") {
      d <- d %>% filter(type == input$type_filter)
    }
    d
  })
  
  fit_model <- reactive({ survfit(Surv(time, status) ~ type, data = filtered_df()) })
  
  # KPI 邏輯 (整數化)
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
  
  # 文字與標籤
  output$kpi_1_t <- renderUI(i18n()$k1_t); output$kpi_2_t <- renderUI(i18n()$k2_t)
  output$kpi_3_t <- renderUI(i18n()$k3_t); output$main_plot_t <- renderUI(i18n()$pt)
  output$tree_t <- renderUI(i18n()$tt); output$nav_p <- renderUI(i18n()$nav_p)
  output$nav_d <- renderUI(i18n()$nav_d); output$tip_k1 <- renderUI(i18n()$t1)
  output$tip_k2 <- renderUI(i18n()$t2); output$tip_k3 <- renderUI(i18n()$t3)
  output$source_note <- renderUI(p(i18n()$src, style = "font-size: 0.75rem; color: #003366; font-style: italic;"))
  output$legal_f <- renderUI(div(i18n()$lgl, style = "background:#F8F9FA; padding:12px; font-size:0.75rem; text-align:center; border-top:1px solid #DDD;"))
  
  # 圖表：座標軸也納入翻譯
  output$survPlot <- renderPlot({
    ggsurvplot(
      fit_model(), data = filtered_df(), 
      palette = c("#B22222", "#4682B4"),
      conf.int = TRUE, conf.int.fill = "#FFD700", 
      xlim = c(0, input$year_range), break.time.by = 2,
      xlab = i18n()$x_lab, ylab = i18n()$y_lab,
      ggtheme = theme_minimal() + theme(text = element_text(size = 14))
    )$plot
  })
  
  # 樹狀圖：類別顯示也翻譯
  output$treePlot <- renderPlotly({
    t_data <- filtered_df() %>% 
      count(asset_class) %>%
      mutate(display_name = i18n()$class_map[asset_class])
    
    plot_ly(t_data, type = "treemap", labels = ~display_name, parents = "", values = ~n,
            marker = list(colors = c("#B22222", "#4682B4", "#2E8B57", "#DAA520")))
  })
  
  # 表格：欄位標題翻譯
  output$detailTable <- renderDT({
    colnames_v <- if (input$lang == "zh") c("片名", "類型", "發行年份", "資產等級") else c("Title", "Type", "Release Year", "Asset Class")
    datatable(
      filtered_df() %>% select(title, type, release_year, asset_class),
      colnames = colnames_v,
      options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE
    )
  })
}

shinyApp(ui, server)