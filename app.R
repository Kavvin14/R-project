
# Used Car Price Prediction - Shiny Dashboard
# Data preparation and models follow car_price_analysis.R exactly.

library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
library(DT)

options(scipen = 999)
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Please install: ", paste(missing, collapse = ", "))
}
invisible(lapply(pkgs, library, character.only = TRUE))
options(scipen = 999)

# Load and prepare data, following the main R analysis script
#csv <- file.path("data", "Car details v3.csv")
csv <- file.path("data", "Car details v3.csv")
if (!file.exists(csv)) stop("Car details v3.csv not found. Check the path in csv.")
raw <- readr::read_csv(csv, show_col_types = FALSE)

needed <- c("year", "selling_price", "km_driven", "fuel",
            "transmission", "mileage", "engine", "max_power")
if (!all(needed %in% names(raw))) {
  stop("Missing columns: ", paste(setdiff(needed, names(raw)), collapse = ", "))
}

#parse numbers and treat zero values as missing.
num <- function(x) na_if(suppressWarnings(readr::parse_number(x)), 0)
cars0 <- raw %>%
  mutate(
    mileage_num = num(mileage),   # km/l or km/kg
    engine_num  = num(engine),    # CC
    power_num   = num(max_power)  # bhp
  )

cars_dedup <- distinct(cars0)
cars <- cars_dedup %>%
  filter(
    !is.na(year), !is.na(selling_price), !is.na(km_driven),
    selling_price > 0, km_driven > 0, km_driven <= 300000,
    year >= 1983, year <= 2020
  ) %>%
  mutate(car_age = 2020 - year, km_10k = km_driven / 10000)

# Extension sample
ext <- cars %>%
  filter(!is.na(power_num), fuel %in% c("Diesel", "Petrol")) %>%
  mutate(
    power_10 = power_num / 10,
    log_price = log(selling_price),
    fuel = factor(fuel, levels = c("Diesel", "Petrol")),
    transmission = factor(transmission, levels = c("Manual", "Automatic"))
  )

if (nrow(cars) < 30 || nrow(ext) < 50) {
  stop("Too few rows after cleaning. Please check the CSV.")
}

# Used for the model summaries, diagnostics and the prediction tab.
m1 <- lm(selling_price ~ km_10k, data = cars)
m2 <- lm(selling_price ~ km_10k + car_age, data = cars)
m3 <- lm(log_price ~ km_10k + car_age, data = ext)
m4 <- lm(log_price ~ km_10k + car_age + power_10 + transmission + fuel, data = ext)

set.seed(123)
i_ext <- sample(nrow(ext), size = round(0.8 * nrow(ext)))
etr <- ext[i_ext, ]
ete <- ext[-i_ext, ]

# All four models refitted on the same training rows for the test-set comparison.
m1_eval <- lm(selling_price ~ km_10k, data = etr)
m2_eval <- lm(selling_price ~ km_10k + car_age, data = etr)
m3_eval <- lm(log_price ~ km_10k + car_age, data = etr)
m4_eval <- lm(log_price ~ km_10k + car_age + power_10 + transmission + fuel, data = etr)

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))

# Log-model predictions are back-transformed with exp(): they estimate the median price.
pred1 <- as.numeric(predict(m1_eval, newdata = ete))
pred2 <- as.numeric(predict(m2_eval, newdata = ete))
pred3 <- exp(as.numeric(predict(m3_eval, newdata = ete)))
pred4 <- exp(as.numeric(predict(m4_eval, newdata = ete)))

# Observed power range, used to limit the prediction inputs (avoid extrapolation).
power_rng <- range(ext$power_num)

ev <- tibble(
  Model = c("Model 1: Distance", "Model 2: Distance + age",
            "Model 3: Log price + distance + age",
            "Model 4: Extended log-price"),
  RMSE = c(rmse(ete$selling_price, pred1), rmse(ete$selling_price, pred2),
           rmse(ete$selling_price, pred3), rmse(ete$selling_price, pred4)),
  MAE = c(mae(ete$selling_price, pred1), mae(ete$selling_price, pred2),
          mae(ete$selling_price, pred3), mae(ete$selling_price, pred4))
)
money <- scales::label_number(prefix = "₹", big.mark = ",", accuracy = 1)

# Dashboard layout: original seven sections are retained.
ui <- dashboardPage(
  dashboardHeader(title = "Used Car Price"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "home", icon = icon("dashboard")),
    menuItem("Data Preparation", tabName = "data", icon = icon("database")),
    menuItem("Exploratory Analysis", tabName = "eda", icon = icon("chart-bar")),
    menuItem("Regression Models", tabName = "models", icon = icon("chart-line")),
    menuItem("Model Evaluation", tabName = "eval", icon = icon("table")),
    menuItem("Price Prediction", tabName = "predict", icon = icon("calculator")),
    menuItem("Conclusion", tabName = "end", icon = icon("clipboard-check"))
  )),
  dashboardBody(
    tags$head(tags$style(HTML(
      ".content-wrapper,.right-side{background:#ecf0f5}.box{border-top:3px solid #3c8dbc}"
    ))),
    tabItems(
      tabItem("home",
              h2("Used Car Price Prediction"),
              p("An analysis of used car prices using linear and multiple linear regression."),
              fluidRow(
                valueBox(comma(nrow(raw)), "Original dataset rows",
                         icon = icon("database"), color = "blue"),
                valueBox(comma(nrow(cars)), "Cleaned dataset rows",
                         icon = icon("check-circle"), color = "green"),
                valueBox("4", "Regression models",
                         icon = icon("chart-line"), color = "yellow")
              ),
              box(title = "Project Objective", status = "primary", width = 12,
                  p("Understand factors associated with used-car selling prices and develop regression models to estimate prices."),
                  p("The project examines distance driven, vehicle age, engine power, transmission and fuel type.")),
              box(title = "Dataset snapshot", status = "info", width = 12, DTOutput("snapshot"))
      ),
      tabItem("data",
              h2("Data Preparation"),
              fluidRow(
                valueBox(comma(nrow(raw)), "Original rows", color = "blue"),
                valueBox(comma(nrow(cars_dedup)), "After exact duplicate removal", color = "aqua"),
                valueBox(comma(nrow(cars)), "Main analysis rows", color = "green")
              ),
              box(title = "Cleaning steps", status = "primary", width = 12,
                  tags$ol(
                    tags$li("Removed exact duplicate rows."),
                    tags$li("Removed rows missing selling price, distance driven or year."),
                    tags$li("Kept positive selling prices and positive distances."),
                    tags$li("Kept distance driven up to 300,000 km."),
                    tags$li("Kept model years from 1983 to 2020."),
                    tags$li("Created car_age = 2020 - year and km_10k = km_driven / 10,000.")
                  ),
                  p("The reference year for car age is 2020, as defined in the project analysis.")),
              box(title = "Missing values in original data", status = "info", width = 12,
                  DTOutput("missing")),
              box(title = "Cleaned data preview", status = "info", width = 12,
                  DTOutput("preview"))
      ),
      tabItem("eda",
              h2("Exploratory Data Analysis"),
              p("Explore selling-price relationships. Axis labels use readable comma-separated numbers."),
              fluidRow(
                box(title = "Select Variable", status = "primary", width = 4,
                    selectInput("xvar", "Choose a variable:",
                                choices = c("Distance Driven (km)" = "km_driven",
                                            "Car Age (years)" = "car_age",
                                            "Engine Power (bhp)" = "power_num",
                                            "Engine Size (CC)" = "engine_num",
                                            "Mileage (numeric; units may vary)" = "mileage_num")),
                    checkboxInput("fullrange", "Show full selling-price range", FALSE),
                    p("Default view focuses on the 99th percentile of prices; no records are deleted.")),
                box(title = "Selling Price Relationship", status = "primary", width = 8,
                    plotOutput("scatter", height = 390))
              ),
              fluidRow(
                box(title = "Selling Price Distribution", status = "info", width = 6,
                    plotOutput("hist", height = 320)),
                box(title = "Log Selling Price Distribution", status = "info", width = 6,
                    plotOutput("loghist", height = 320))
              ),
              fluidRow(
                box(title = "Selling Price by Fuel", status = "info", width = 6,
                    plotOutput("fuel", height = 300)),
                box(title = "Selling Price by Transmission", status = "info", width = 6,
                    plotOutput("trans", height = 300))
              )
      ),
      tabItem("models",
              h2("Regression Models"),
              box(title = "Models used", status = "primary", width = 12,
                  tags$ol(
                    tags$li("Model 1: Price ~ distance driven."),
                    tags$li("Model 2: Price ~ distance driven + car age."),
                    tags$li("Model 3: Log price ~ distance driven + car age."),
                    tags$li("Model 4: Log price ~ distance + age + power + transmission + fuel.")
                  ),
                  p("Summaries below are fitted on the full samples, as in the analysis script: Models 1-2 on the cleaned sample, Models 3-4 on the extension sample (Diesel/Petrol cars with known power)."),
                  p("For the test-set comparison, all four models are refitted on the same 80% training split of the extension sample.")),
              fluidRow(
                box(title = "Model 1", status = "info", width = 6, verbatimTextOutput("s1")),
                box(title = "Model 2", status = "info", width = 6, verbatimTextOutput("s2"))
              ),
              fluidRow(
                box(title = "Model 3", status = "info", width = 6, verbatimTextOutput("s3")),
                box(title = "Model 4", status = "info", width = 6, verbatimTextOutput("s4"))
              )
      ),
      tabItem("eval",
              h2("Model Evaluation"),
              p("Test-set RMSE and MAE are shown in rupees. Lower values indicate smaller prediction errors."),
              p("Predictions from the log-price models (3 and 4) are back-transformed with exp(), so they estimate the median price. This slightly disadvantages them on RMSE, which is driven by the mean."),
              box(title = "Model comparison", status = "primary", width = 12, DTOutput("metrics")),
              fluidRow(
                box(title = "Test RMSE", status = "info", width = 6, plotOutput("rmseplot", height = 300)),
                box(title = "Test MAE", status = "info", width = 6, plotOutput("maeplot", height = 300))
              ),
              box(title = "Model 4 diagnostic plots", status = "info", width = 12,
                  plotOutput("diagnostics", height = 600)),
              box(title = "How to interpret diagnostics", status = "warning", width = 12,
                  p("The diagnostic plots help assess whether the linear regression assumptions are reasonable and whether unusual observations may affect the model."),
                  p("The Residuals vs Leverage panel shows Cook's distance contours. Cook's distance flags potentially influential observations for review; it does not automatically justify deleting them."))
      ),
      tabItem("predict",
              h2("Used Car Price Prediction"),
              p("Enter vehicle details to estimate a price using Model 4."),
              fluidRow(
                box(title = "Vehicle details", status = "primary", width = 4,
                    numericInput("distance", "Distance driven (km)", 50000, min = 1, max = 300000, step = 1000),
                    numericInput("age", "Car age (years)", 5, min = 0, max = 37),
                    numericInput("power", "Maximum power (bhp)", 80,
                                 min = floor(power_rng[1]), max = ceiling(power_rng[2])),
                    selectInput("transmission", "Transmission", c("Manual", "Automatic")),
                    selectInput("fueltype", "Fuel", c("Diesel", "Petrol")),
                    actionButton("go", "Estimate Price", class = "btn-primary")),
                box(title = "Estimated selling price", status = "success", width = 8,
                    h2(textOutput("priceout")),
                    p("This is the estimated typical (median) price for a car with these details, not a guaranteed market price. Model 4 applies to Petrol/Diesel cars with Manual/Automatic transmission."),
                    verbatimTextOutput("details"))
              )
      ),
      tabItem("end",
              h2("Project Conclusion"),
              box(title = "Key findings", status = "primary", width = 12,
                  p("The models examine associations between vehicle characteristics and used-car selling prices."),
                  tags$ul(
                    tags$li("Car age is the strongest predictor: in Model 4 each extra year is associated with about 11% lower price."),
                    tags$li("Distance alone is negatively associated with price (Model 1), but its effect largely disappears once car age is controlled (Model 2), because older cars have usually been driven further."),
                    tags$li("After controlling for power, transmission and fuel (Model 4), each extra 10,000 km is associated with about 0.5% lower price."),
                    tags$li("Higher power and automatic transmission are associated with higher prices; petrol cars sell for less than comparable diesel cars."),
                    tags$li("Model 4 has the lowest test-set RMSE and MAE.")
                  ),
                  p("These are associations, not proof of causal effects.")),
              box(title = "Best test-set RMSE in the displayed comparison",
                  status = "info", width = 12, DTOutput("best")),
              box(title = "Limitations", status = "warning", width = 12,
                  tags$ul(
                    tags$li("The data may not represent all used-car markets or current market prices."),
                    tags$li("Car age uses 2020 as the reference year."),
                    tags$li("Mileage may mix km/litre and km/kg units, so it is not used as a regression predictor."),
                    tags$li("Model 4 assumes the distance effect is the same at every car age; an interaction test suggests it weakens as cars get older."),
                    tags$li("Log-model predictions estimate the median rather than the mean price."),
                    tags$li("Predictions are estimates and depend on the quality and coverage of the dataset.")
                  ))
      )
    )
  )
)

server <- function(input, output, session) {
  output$snapshot <- renderDT({
    datatable(tibble(
      Measure = c("Original rows", "After deduplication", "Cleaned main sample",
                  "Extension sample (Diesel/Petrol, known power)", "Training rows", "Test rows",
                  "Original columns"),
      Value = c(nrow(raw), nrow(cars_dedup), nrow(cars),
                nrow(ext), nrow(etr), nrow(ete), ncol(raw))
    ), rownames = FALSE, options = list(dom = "t"))
  })
  
  output$missing <- renderDT({
    z <- tibble(
      Variable = names(raw),
      Missing = vapply(raw, function(x) sum(is.na(x)), numeric(1))
    ) %>% arrange(desc(Missing))
    datatable(z, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$preview <- renderDT({
    datatable(head(cars, 10), options = list(scrollX = TRUE, pageLength = 5))
  })
  
  edata <- reactive({
    req(input$xvar)
    z <- cars %>% select(selling_price, all_of(input$xvar)) %>%
      filter(is.finite(.data[[input$xvar]]), is.finite(selling_price))
    validate(need(nrow(z) > 5, "Not enough complete observations for this variable."))
    z
  })
  
  output$scatter <- renderPlot({
    z <- edata()
    v <- input$xvar
    labs <- c(km_driven = "Distance Driven (km)", car_age = "Car Age (years)",
              power_num = "Maximum Power (bhp)", engine_num = "Engine Size (CC)",
              mileage_num = "Mileage numeric value (units may vary)")
    g <- ggplot(z, aes(x = .data[[v]], y = selling_price)) +
      geom_point(alpha = 0.23, colour = "#2878A8", size = 1.5) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                  colour = "#D62728", fill = "#F4A6A6", linewidth = 1.1) +
      scale_x_continuous(labels = label_number(big.mark = ",")) +
      scale_y_continuous(labels = money) +
      labs(title = "Selling Price vs Selected Variable", x = labs[[v]],
           y = "Selling Price (₹)",
           caption = "Blue points: observed cars. Red line: fitted linear trend.") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
    if (!isTRUE(input$fullrange)) {
      g <- g + coord_cartesian(
        ylim = c(0, as.numeric(quantile(z$selling_price, 0.99, na.rm = TRUE)))
      )
    }
    g
  })
  
  output$hist <- renderPlot({
    ggplot(cars, aes(selling_price)) +
      geom_histogram(binwidth = 50000, boundary = 0, fill = "#4C89B8", colour = "white") +
      scale_x_continuous(labels = money) +
      coord_cartesian(xlim = c(0, 2000000)) +
      labs(x = "Selling Price (₹)", y = "Number of Cars") +
      theme_minimal(base_size = 12)
  })
  
  output$loghist <- renderPlot({
    ggplot(cars, aes(log(selling_price))) +
      geom_histogram(bins = 40, fill = "#16A6C9", colour = "white") +
      labs(x = "Natural log of selling price", y = "Number of Cars") +
      theme_minimal(base_size = 12)
  })
  
  output$fuel <- renderPlot({
    ggplot(filter(cars, !is.na(fuel)), aes(fuel, selling_price)) +
      geom_boxplot(fill = "#76A5C9") +
      scale_y_continuous(labels = money) +
      coord_cartesian(ylim = c(0, quantile(cars$selling_price, 0.99))) +
      labs(x = "Fuel", y = "Selling Price (₹)") +
      theme_minimal(base_size = 12)
  })
  
  output$trans <- renderPlot({
    ggplot(filter(cars, !is.na(transmission)), aes(transmission, selling_price)) +
      geom_boxplot(fill = "#8CC7A1") +
      scale_y_continuous(labels = money) +
      coord_cartesian(ylim = c(0, quantile(cars$selling_price, 0.99))) +
      labs(x = "Transmission", y = "Selling Price (₹)") +
      theme_minimal(base_size = 12)
  })
  
  output$s1 <- renderPrint(summary(m1))
  output$s2 <- renderPrint(summary(m2))
  output$s3 <- renderPrint(summary(m3))
  output$s4 <- renderPrint(summary(m4))
  
  output$metrics <- renderDT({
    datatable(ev, rownames = FALSE, options = list(dom = "t")) %>%
      formatCurrency(c("RMSE", "MAE"), currency = "₹", mark = ",", digits = 0)
  })
  
  output$rmseplot <- renderPlot({
    ggplot(ev, aes(reorder(Model, RMSE), RMSE)) +
      geom_col(fill = "#3C8DBC") + coord_flip() +
      scale_y_continuous(labels = money) +
      labs(x = NULL, y = "RMSE (₹)") + theme_minimal(base_size = 11)
  })
  
  output$maeplot <- renderPlot({
    ggplot(ev, aes(reorder(Model, MAE), MAE)) +
      geom_col(fill = "#00A65A") + coord_flip() +
      scale_y_continuous(labels = money) +
      labs(x = NULL, y = "MAE (₹)") + theme_minimal(base_size = 11)
  })
  
  output$diagnostics <- renderPlot({
    old <- par(no.readonly = TRUE)
    on.exit(par(old))
    par(mfrow = c(2, 2))
    plot(m4)
  })
  
  output$best <- renderDT({
    datatable(slice_min(ev, RMSE, n = 1), rownames = FALSE,
              options = list(dom = "t"))
  })
  
  result <- eventReactive(input$go, {
    validate(
      need(input$distance > 0 && input$distance <= 300000,
           "Distance must be 1 to 300,000 km."),
      need(input$age >= 0 && input$age <= 37, "Age must be 0 to 37 years."),
      need(input$power >= floor(power_rng[1]) && input$power <= ceiling(power_rng[2]),
           paste0("Power must be within the observed range (",
                  floor(power_rng[1]), " to ", ceiling(power_rng[2]), " bhp)."))
    )
    new_car <- tibble(
      km_10k = input$distance / 10000,
      car_age = input$age,
      power_10 = input$power / 10,
      transmission = factor(input$transmission, levels = c("Manual", "Automatic")),
      fuel = factor(input$fueltype, levels = c("Diesel", "Petrol"))
    )
    list(price = exp(as.numeric(predict(m4, newdata = new_car))))   # median price
  })
  
  output$priceout <- renderText({
    req(result())
    paste0("₹", format(round(result()$price), big.mark = ",", scientific = FALSE))
  })
  
  output$details <- renderPrint({
    req(result())
    cat("Model 4: extended log-price regression\n",
        "Distance:", input$distance, "km\n",
        "Car age:", input$age, "years\n",
        "Power:", input$power, "bhp\n",
        "Transmission:", input$transmission, "\n",
        "Fuel:", input$fueltype, "\n",
        "Back-transformation: exp() of predicted log price (median)")
  })
}

shinyApp(ui, server)
