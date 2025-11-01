# Superstore Shiny Explorer — updated with stable level pickers & clean summary table
# install.packages(c("shiny","bslib","tidyverse","DT","shinycssloaders"))

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(shinycssloaders)

# ------------------ Load data (CSV in project root) ------------------
raw <- readr::read_csv("superstore_orders.csv",
                       col_types = cols(
                         `Row ID` = col_double(),
                         `Order ID` = col_character(),
                         `Order Date` = col_date(format = "%m/%d/%Y"),
                         `Ship Date` = col_date(format = "%m/%d/%Y"),
                         `Ship Mode` = col_character(),
                         `Customer ID` = col_character(),
                         `Customer Name` = col_character(),
                         Segment = col_character(),
                         Country = col_character(),
                         City = col_character(),
                         State = col_character(),
                         `Postal Code` = col_character(),
                         Region = col_character(),
                         `Product ID` = col_character(),
                         Category = col_character(),
                         `Sub-Category` = col_character(),
                         `Product Name` = col_character(),
                         Sales = col_double(),
                         Quantity = col_double(),
                         Discount = col_double(),
                         Profit = col_double()
                       )
) |>
  mutate(across(c(`Ship Mode`, Segment, Country, City, State, Region, Category, `Sub-Category`), as.factor))

cat_vars <- c("Ship Mode","Segment","Country","State","Region","Category","Sub-Category")
num_vars <- c("Sales","Quantity","Discount","Profit")

# initial level lists for static UI creation
initial_levs1 <- levels(raw[["Region"]])
initial_levs2 <- levels(raw[["Category"]])

# ------------------ UI ------------------
ui <- page_sidebar(
  title = "Superstore Shiny Explorer",
  sidebar = sidebar(
    width = 300,
    
    # categorical filters (inputs created once; levels updated in server)
    selectInput("cat1", "Categorical filter 1:", choices = cat_vars, selected = "Region"),
    selectizeInput(
      "cat1_levels", "Region levels:",
      choices  = initial_levs1, selected = initial_levs1,
      multiple = TRUE, options = list(plugins = list("remove_button"))
    ),
    
    selectInput("cat2", "Categorical filter 2:", choices = cat_vars, selected = "Category"),
    selectizeInput(
      "cat2_levels", "Category levels:",
      choices  = initial_levs2, selected = initial_levs2,
      multiple = TRUE, options = list(plugins = list("remove_button"))
    ),
    
    # numeric filters (dynamic ranges)
    selectInput("num1", "Numeric filter 1:", choices = num_vars, selected = "Sales"),
    uiOutput("num1_range_ui"),
    
    selectInput("num2", "Numeric filter 2:", choices = num_vars, selected = "Profit"),
    uiOutput("num2_range_ui"),
    
    actionButton("apply_filters", "Apply filters", class = "btn-primary")
  ),
  
  navset_tab(
    id = "tabs",
    
    nav_panel("About",
              layout_columns(
                col_widths = c(8,4),
                card(
                  card_header("About this app"),
                  p("Explore the Kaggle Superstore dataset. Use the sidebar to subset by categories and numeric ranges, then explore tables, stats, and plots."),
                  tags$ul(
                    tags$li("Pick filters in the sidebar, then click ", tags$code("Apply filters"), "."),
                    tags$li("Data Download: view and download the filtered data."),
                    tags$li("Data Exploration: summaries and six+ plots with coloring/faceting.")
                  ),
                  p(HTML('Data source: <a href="https://www.kaggle.com/datasets/juhi1994/superstore/data" target="_blank">Kaggle Superstore</a>'))
                ),
                card(
                  card_header("Tip"),
                  p("CSV should be in the same folder as app.R. Change the path above only if you move it.")
                )
              )
    ),
    
    nav_panel("Data Download",
              card(
                card_header("Filtered data"),
                div(DTOutput("tbl") %>% withSpinner()),
                downloadButton("download_csv", "Download CSV")
              )
    ),
    
    nav_panel("Data Exploration",
              layout_columns(
                col_widths = c(4,8),
                card(
                  card_header("Exploration controls"),
                  radioButtons("summary_type", "Summary type:", choices = c("Categorical"="cat","Numeric"="num"), inline = TRUE),
                  uiOutput("explore_ui")
                ),
                card(
                  card_header("Results"),
                  uiOutput("summary_title"),
                  tableOutput("summary_tbl"),
                  plotOutput("plot1") %>% withSpinner(),
                  plotOutput("plot2") %>% withSpinner(),
                  plotOutput("plot3") %>% withSpinner(),
                  plotOutput("plot4") %>% withSpinner(),
                  plotOutput("plot5") %>% withSpinner(),
                  plotOutput("plot6") %>% withSpinner()
                )
              )
    )
  )
)

# ------------------ Server ------------------
server <- function(input, output, session) {
  data_rv <- reactiveVal(raw)
  
  # ---- keep level selections stable; update choices when cat var changes ----
  observeEvent(input$cat1, {
    levs <- levels(raw[[input$cat1]])
    sel  <- isolate(input$cat1_levels)
    if (is.null(sel) || !all(sel %in% levs)) sel <- levs
    updateSelectizeInput(session, "cat1_levels",
                         label = paste0(input$cat1, " levels:"),
                         choices = levs, selected = sel, server = TRUE)
  })
  
  observeEvent(input$cat2, {
    levs <- levels(raw[[input$cat2]])
    sel  <- isolate(input$cat2_levels)
    if (is.null(sel) || !all(sel %in% levs)) sel <- levs
    updateSelectizeInput(session, "cat2_levels",
                         label = paste0(input$cat2, " levels:"),
                         choices = levs, selected = sel, server = TRUE)
  })
  
  # ---- numeric range UIs ----
  output$num1_range_ui <- renderUI({
    rng <- range(raw[[input$num1]], na.rm = TRUE)
    sliderInput("num1_rng", paste0(input$num1, " range:"),
                min = floor(rng[1]), max = ceiling(rng[2]),
                value = rng, step = signif(diff(rng)/100, 2))
  })
  output$num2_range_ui <- renderUI({
    rng <- range(raw[[input$num2]], na.rm = TRUE)
    sliderInput("num2_rng", paste0(input$num2, " range:"),
                min = floor(rng[1]), max = ceiling(rng[2]),
                value = rng, step = signif(diff(rng)/100, 2))
  })
  
  # ---- apply filters only when button pressed ----
  observeEvent(input$apply_filters, {
    req(input$cat1_levels, input$cat2_levels, input$num1_rng, input$num2_rng)
    df <- raw |>
      dplyr::filter(.data[[input$cat1]] %in% input$cat1_levels) |>
      dplyr::filter(.data[[input$cat2]] %in% input$cat2_levels) |>
      dplyr::filter(dplyr::between(.data[[input$num1]], input$num1_rng[1], input$num1_rng[2])) |>
      dplyr::filter(dplyr::between(.data[[input$num2]], input$num2_rng[1], input$num2_rng[2]))
    data_rv(df)
  }, ignoreInit = TRUE)
  
  # ---- table + download ----
  output$tbl <- renderDT({
    datatable(data_rv(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("superstore_filtered_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(data_rv(), file)
  )
  
  # ---- exploration controls ----
  output$explore_ui <- renderUI({
    if (input$summary_type == "cat") {
      tagList(
        selectInput("cat_var1", "Categorical var (rows):", choices = cat_vars, selected = input$cat1),
        selectInput("cat_var2", "Categorical var (cols, optional):", choices = c("(none)", cat_vars), selected = "(none)")
      )
    } else {
      tagList(
        selectInput("num_y", "Numeric variable:", choices = num_vars, selected = input$num1),
        selectInput("group_cat", "Group by (categorical):", choices = c("(none)", cat_vars), selected = input$cat2),
        selectInput("color_cat", "Color by:", choices = c("(none)", cat_vars), selected = "(none)"),
        selectInput("facet_cat", "Facet by:", choices = c("(none)", cat_vars), selected = "(none)")
      )
    }
  })
  
  # ---- clean summary title + table (no raw HTML) ----
  output$summary_title <- renderUI({
    if (input$summary_type == "cat") {
      h4(if (!is.null(input$cat_var2) && input$cat_var2 != "(none)") "Two-way table" else "One-way table")
    } else {
      h4("Numeric summary")
    }
  })
  
  output$summary_tbl <- renderTable({
    req(data_rv())
    df <- data_rv()
    
    if (input$summary_type == "cat") {
      req(input$cat_var1)
      if (!is.null(input$cat_var2) && input$cat_var2 != "(none)") {
        as.data.frame.matrix(table(df[[input$cat_var1]], df[[input$cat_var2]]))
      } else {
        as.data.frame(table(df[[input$cat_var1]]))
      }
    } else {
      req(input$num_y)
      if (!is.null(input$group_cat) && input$group_cat != "(none)") {
        df |>
          dplyr::group_by(.data[[input$group_cat]]) |>
          dplyr::summarize(
            n = dplyr::n(),
            mean   = mean(.data[[input$num_y]], na.rm = TRUE),
            sd     = sd(.data[[input$num_y]],   na.rm = TRUE),
            median = median(.data[[input$num_y]], na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        df |>
          dplyr::summarize(
            n = dplyr::n(),
            mean   = mean(.data[[input$num_y]], na.rm = TRUE),
            sd     = sd(.data[[input$num_y]],   na.rm = TRUE),
            median = median(.data[[input$num_y]], na.rm = TRUE)
          )
      }
    }
  }, rownames = TRUE)
  
  # ---- plotting helpers ----
  add_aes <- function(p, color_cat, facet_cat) {
    if (!is.null(color_cat) && color_cat != "(none)") p <- p + aes(color = .data[[color_cat]])
    if (!is.null(facet_cat) && facet_cat != "(none)") p <- p + facet_wrap(vars(.data[[facet_cat]]))
    p + theme_minimal() + labs(x = NULL, y = NULL)
  }
  
  # 1) Bar or mean-by-group
  output$plot1 <- renderPlot({
    df <- data_rv(); req(input$summary_type)
    if (input$summary_type == "cat") {
      req(input$cat_var1)
      second <- if (!is.null(input$cat_var2) && input$cat_var2 != "(none)") input$cat_var2 else NULL
      p <- ggplot(df, aes(x = .data[[input$cat_var1]], fill = if (!is.null(second)) .data[[second]] else NULL)) +
        geom_bar(position = "stack") + labs(title = "Bar chart")
      p + theme_minimal()
    } else {
      req(input$num_y, input$group_cat)
      p <- ggplot(df, aes(x = .data[[input$group_cat]], y = .data[[input$num_y]])) +
        stat_summary(fun = mean, geom = "col") + labs(title = "Mean by group")
      add_aes(p, input$color_cat, input$facet_cat)
    }
  })
  
  # 2) Histogram or stacked counts
  output$plot2 <- renderPlot({
    df <- data_rv(); req(input$summary_type)
    if (input$summary_type == "cat") {
      req(input$cat_var1)
      second <- if (!is.null(input$cat_var2) && input$cat_var2 != "(none)") input$cat_var2 else NULL
      p <- ggplot(df, aes(x = .data[[input$cat_var1]], fill = if (!is.null(second)) .data[[second]] else NULL)) +
        geom_bar(position = "stack") + labs(title = "Stacked counts")
      p + theme_minimal()
    } else {
      req(input$num_y)
      p <- ggplot(df, aes(x = .data[[input$num_y]])) + geom_histogram(bins = 30) + labs(title = "Histogram")
      add_aes(p, input$color_cat, input$facet_cat)
    }
  })
  
  # 3) Boxplot by group
  output$plot3 <- renderPlot({
    df <- data_rv(); req(input$num_y)
    grp <- if (!is.null(input$group_cat) && input$group_cat != "(none)") input$group_cat else NULL
    if (!is.null(grp)) {
      p <- ggplot(df, aes(x = .data[[grp]], y = .data[[input$num_y]])) + geom_boxplot() + labs(title = "Boxplot by group")
    } else {
      p <- ggplot(df, aes(y = .data[[input$num_y]])) + geom_boxplot() + labs(title = "Boxplot")
    }
    add_aes(p, input$color_cat, input$facet_cat)
  })
  
  # 4) Scatter with smoothing
  output$plot4 <- renderPlot({
    df <- data_rv(); req(input$num_y)
    xvar <- setdiff(num_vars, input$num_y)[1]
    p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[input$num_y]])) +
      geom_point(alpha = 0.6) + geom_smooth(method = "loess", se = FALSE) +
      labs(title = paste("Scatter:", xvar, "vs", input$num_y))
    add_aes(p, input$color_cat, input$facet_cat)
  })
  
  # 5) Heatmap (not covered in class)
  output$plot5 <- renderPlot({
    df2 <- data_rv() |>
      dplyr::group_by(Category, `Sub-Category`) |>
      dplyr::summarize(mean_sales = mean(Sales, na.rm = TRUE), .groups = "drop")
    ggplot(df2, aes(x = Category, y = `Sub-Category`, fill = mean_sales)) +
      geom_tile() +
      labs(title = "Heatmap: mean Sales by Category/Sub-Category", x = NULL, y = NULL) +
      theme_minimal()
  })
  
  # 6) Time series: daily sales
  output$plot6 <- renderPlot({
    df <- data_rv() |>
      dplyr::group_by(`Order Date`) |>
      dplyr::summarize(Sales = sum(Sales, na.rm = TRUE), .groups = "drop")
    ggplot(df, aes(x = `Order Date`, y = Sales)) +
      geom_line() +
      labs(title = "Daily Sales over time", x = NULL, y = "Sales") +
      theme_minimal()
  })
}

shinyApp(ui, server)
