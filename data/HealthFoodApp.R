library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(readr)

# 
# Data Loading & Cleaning

food_data <- tryCatch({
  read_csv("open_food_facts_10k.csv", show_col_types = FALSE) %>%
    filter(!is.na(`energy-kcal_100g`), `energy-kcal_100g` <= 900, !is.na(main_category_en)) %>%
    mutate(
      main_category_en = gsub("^\\w{2}:", "", main_category_en),
      main_category_en = tools::toTitleCase(gsub("-", " ", main_category_en)),
      macro_category = case_when(
        # 1. Intercept Sugars/Sweeteners/Syrups -> Pantry
        grepl("Cotton Candy sugars|Syrup|Corn syrups|Simple syrups|Syrups|Sweetener|Sweeteners|Sugar|Sugars", main_category_en, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        # 2. Intercept Ice Creams, Sorbets, Frozen Desserts -> Snacks & Sweets
        grepl("Cotton Candy|Ice Cream|Ice creams|Ice creams and sorbets|Frozen Dessert|Frozen desserts|Sorbets", main_category_en, ignore.case = TRUE) ~ "Snacks & Sweets",
        
        # 3. Catch Flours/Grains mislabeled as Beverages (checks both category AND product name)
        grepl("Beverage", main_category_en, ignore.case = TRUE) & grepl("Flour|Farina|Grano|Wheat|Grain", product_name, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        # 4. Catch Meat Alternatives / Analogues -> Prepared Meals & Frozen
        grepl("Meat alternative|Meat analogues", main_category_en, ignore.case = TRUE) ~ "Prepared Meals & Frozen",
        
        # 5. Catch Casseroles (Vegetable-based) and route to Meals based on product name
        grepl("casserole", product_name, ignore.case = TRUE) ~ "Prepared Meals & Frozen",
        
        # 6. Catch Muesli/Müsli and route to Pantry (checks product name)
        grepl("müesli|muesli|müsli", product_name, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        # 7. Catch Crispy Minis and route to Snacks & Sweets (checks product name)
        grepl("crispy minis", product_name, ignore.case = TRUE) ~ "Snacks & Sweets",
        
        # 8. Intercept Nut/Fruit Mixes (like Aperifruits) mislabeled as Fruits/Veggies -> Pantry
        grepl("melange de fruits", main_category_en, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        # 9. Intercept Fig Spreads, Chutneys, Fruit Preserves (checks category OR product name) -> Pantry
        grepl("Chutney|Chutneys|Confit|Preserve|Preserves|Sweet spreads|Fruit and vegetable preserves", main_category_en, ignore.case = TRUE) | grepl("spread", product_name, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        # 10. Intercept Concentrated / Frozen Orange Juices based on product name -> Beverages
        grepl("concentrated.*orange juice|orange juice", product_name, ignore.case = TRUE) ~ "Beverages",
        
        # 11. Intercept Jams, Spreads, Fruit Juices -> Pantry
        grepl("Jams|Spread|Spreads|Nectar|Nectars|Juice|Juices", main_category_en, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        # 12. Intercept Viennoiseries, Milk Bread Rolls, Pastries, Puddings, Tiramisu, Pies, Cakes, and Biscuits -> Snacks & Sweets
        grepl("Viennoiserie|Viennoiseries|Milk bread|Milk bread rolls|Pudding|Puddings|Riz|Tiramisu|Tart|Tarts|Pie|Pies|Cake|Cakes|Biscuit|Biscuits|Pastry|Pastries", main_category_en, ignore.case = TRUE) ~ "Snacks & Sweets",
        
        # 13. Explicitly map Confectionery, Lollipops, Sweet Snacks -> Snacks & Sweets
        grepl("Confectionery|Confectioneries|Sweet Snack|Sweet snacks|Lollipop|Lollipops|Snack|Snacks|Chocolate|Candy|Candies|Sweet|Dessert", main_category_en, ignore.case = TRUE) ~ "Snacks & Sweets",
        
        # 14. Beverage classifications (Strictly non-syrup, non-nectar drinks, avoiding general plant-based tags)
        grepl("Beverage|Beverages|Drink|Drinks|Coffee|Fruit juices|Tea|Water", main_category_en, ignore.case = TRUE) ~ "Beverages",
        
        # 15. Dairy and Cheeses
        grepl("Dairy|Cheese|Cheeses|Milk|Yogurt|Cream|Butter", main_category_en, ignore.case = TRUE) ~ "Dairy & Cheese",
        
        # 16. Prepared Meals, Frozen Foods, Noodle/Pizza Dishes -> Prepared Meals & Frozen
        grepl("Meal|Meals|Pizza|Pizzas|Pie|Pies|Sandwich|Sandwiches|Prepared|Frozen|Noodle|Noodles|Ramen|Pasta", main_category_en, ignore.case = TRUE) ~ "Prepared Meals & Frozen",
        
        # 17. Proteins and Meats (Pure, unadulterated meats/fish)
        grepl("Meat|Meat-based|Fish|Egg|Poultry|Charcuterie|Seafood", main_category_en, ignore.case = TRUE) ~ "Proteins & Meats",
        
        # 18. Fruits and Veggies
        grepl("Fruit|Fruits|Vegetable|Vegetables", main_category_en, ignore.case = TRUE) ~ "Fruits & Veggies",
        
        # 19. General Pantry, Groceries, Condiments
        grepl("Grocery|Groceries|Sauce|Sauces|Dip|Dips|Oil|Condiment|Condiments|Grain|Grain-based|Bakery|Breads|Cereal", main_category_en, ignore.case = TRUE) ~ "Pantry Staples & Dry Goods",
        
        TRUE ~ "Pantry Staples & Dry Goods"
      )
    )
}, error = function(e) {
  NULL
})



popular_categories <- if(!is.null(food_data)) {
  food_data %>%
    count(main_category_en, sort = TRUE) %>%
    head(15) %>%
    pull(main_category_en)
} else {
  c("Snacks", "Beverages", "Dairy")
}

predictor_cols <- c(
  "Energy (kcal/100g)" = "energy-kcal_100g",
  "Carbohydrates (g/100g)" = "carbohydrates_100g",
  "Fat (g/100g)" = "fat_100g"
)

response_cols <- c(
  "Sugars (g/100g)" = "sugars_100g",
  "Proteins (g/100g)" = "proteins_100g",
  "Energy (kcal/100g)" = "energy-kcal_100g"
)


# UI

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = ("Nutritional Analysis Explorer"),
    titleWidth = 300,
    tags$li(class = "dropdown", tags$head(tags$title("Open Foods Analysis"))),
    tags$li(class = "dropdown", tags$a(href = "https://github.com/ramyapinninti/553-Final-Project/blob/main/FinalProject.Rmd", target = "_blank", icon("code"), " Source Code", style = "color: #ffffff; font-size: 14px;")),
    tags$li(class = "dropdown", tags$a(href = "https://ramyapinninti.github.io/553-Final-Project/", target = "_blank", icon("file-alt"), " Project Report", style = "color: #ffffff; font-size: 14px;")),
    tags$li(class = "dropdown", tags$a(href = "https://www.kaggle.com/datasets/alexandrelemercier/food-detailed-nutritional-content", target = "_blank", icon("database"), " Data Source", style = "color: #ffffff; font-size: 14px;"))
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Analyze & Predict", tabName = "dash_tab", icon = icon("chart-bar")),
      menuItem("Raw Data Registry", tabName = "data_tab", icon = icon("database")),
      
      hr(),
      selectInput("selected_cat", "Filter by Detailed Food Category:", 
                  choices = c("Show All Categories" = "all", popular_categories), 
                  selected = "all"),
      
      # Add
      selectInput("selected_macro", "Filter by Macro Food Group:",
                  choices = c("Show All Groups" = "all", 
                              "Snacks & Sweets", "Beverages", "Dairy & Cheese", 
                              "Proteins & Meats", "Fruits & Veggies", 
                              "Prepared Meals & Frozen", "Pantry Staples & Dry Goods"),
                  selected = "all"),
      
      selectInput("X_var", "Predictor Variable (X):", choices = predictor_cols, selected = "carbohydrates_100g"),
      selectInput("Y_var", "Response Variable (Y):", choices = response_cols, selected = "sugars_100g"),
      
      sliderInput("newX", "Hypothetical Value for X (Prediction):", min = 0, max = 100, value = 10, step = 1),
      
      sliderInput("cal_slider", "Calorie Scope (kcal/100g):", min = 0, max = 900, value = c(0, 400)),
      
      br(),
      div(style = "text-align: center; padding: 15px;",
          img(src = "https://github.com/pengdsci/sta553/blob/main/image/goldenRamLogo.png?raw=true", width = "70px", height = "70px"),
          p(style = "font-family:Courier; font-size: 12px; margin-top: 10px;", 
            a("Report bugs to Administrator", href = "mailto:rpinninti101@wcupa.edu", style = "color: #b8c7ce;"))
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f8f9fa; }
      .nav-tabs-custom > .nav-tabs > li.active > a { background-color: #3c8dbc; color: #fff; border-top-color: #3c8dbc; }
      .box.box-solid.box-primary>.box-header { background: #3c8dbc; background-color: #3c8dbc; }
      .small-box { border-radius: 6px; }
      .form-group { margin-bottom: 12px; }
    "))),
    
    tabItems(
      tabItem(tabName = "dash_tab",
              fluidRow(
                valueBoxOutput("total_products_box", width = 4),
                valueBoxOutput("avg_cal_box", width = 4),
                valueBoxOutput("avg_sugar_box", width = 4)
              ),
              
              fluidRow(
                tabBox(
                  id = "main_tabs", width = 12,
                  tabPanel("Scatterplot", 
                           p(strong("Interactive Scatterplot:"), " Examine point distributions and use tooltips to identify outlier products."),
                           plotlyOutput("scatterPlot", height = "420px")),
                  tabPanel("Linear Regression Fit", 
                           p(strong("Ordinary Least Squares (OLS):"), " Bivariate regression line (red dashed) showing the mathematical relationship. Green dotted line represents dietary threshold reference marker."),
                           plotOutput("regPlot", height = "420px"),
                           br(),
                           h4(textOutput("regFormulaText"), style = "color: #3c8dbc; font-weight: bold; text-align: center;")
                  ),
                  tabPanel("Residual Diagnostics", 
                           p(strong("Model Diagnostic Checks:"), " Residual spread and quantile-quantile normality plots."),
                           plotOutput("resPlot", height = "420px")),
                  tabPanel("Predictive Value Model", 
                           p(strong("Hypothetical Prediction:"), " The gold diamond shows the predicted response value (Y) for the predictor value (X) set in the sidebar."),
                           plotOutput("predPlot", height = "420px")),
                  tabPanel("Nutrient Distribution Boxplot", 
                           p(strong("Distribution Boxplot (Health/Business Insight):"), " Compares grouped macro-categories to assess median nutritional bulk. Red diamond denotes the mean."),
                           plotOutput("boxPlot", height = "420px")),
                  tabPanel("Probability Density", 
                           p(strong("Probability Density Curve:"), " Continuous density footprint of the selected nutritional measure."),
                           plotlyOutput("densityPlot", height = "420px"))
                )
              )
      ),
      
      tabItem(tabName = "data_tab",
              fluidRow(
                box(
                  title = "Filtered Observational Registry", status = "primary", solidHeader = TRUE, width = 12,
                  p("Cleaned records matching your control parameters. Streamlined for clarity."),
                  div(style = 'overflow-x: scroll', DTOutput("raw_table"))
                )
              )
      )
    )
  )
)


# Server Logic

server <- function(input, output, session) {
  
  workDat <- reactive({
    if (is.null(food_data)) return(NULL)
    
    data <- food_data %>%
      filter(`energy-kcal_100g` >= input$cal_slider[1],
             `energy-kcal_100g` <= input$cal_slider[2])
    
    if (input$selected_cat != "all") {
      data <- data %>% filter(main_category_en == input$selected_cat)
    }
    
    if (input$selected_macro != "all") {
      data <- data %>% filter(macro_category == input$selected_macro)
    }
    
    return(data)
  })
  
  output$total_products_box <- renderValueBox({
    data <- workDat()
    valueBox(format(nrow(data), big.mark = ","), "Active Items", icon = icon("shopping-basket"), color = "light-blue")
  })
  
  output$avg_cal_box <- renderValueBox({
    data <- workDat()
    avg_cal <- mean(data$`energy-kcal_100g`, na.rm = TRUE)
    valueBox(round(avg_cal, 1), "Avg Energy (kcal/100g)", icon = icon("fire"), color = "aqua")
  })
  
  output$avg_sugar_box <- renderValueBox({
    data <- workDat()
    avg_sugar <- mean(data$sugars_100g, na.rm = TRUE)
    valueBox(round(avg_sugar, 1), "Avg Sugar (g/100g)", icon = icon("cubes"), color = "teal")
  })
  
  output$raw_table <- renderDT({
    data <- workDat()
    if(is.null(data)) return(NULL)
    
    datatable(
      data %>% select(product_name, brands, main_category_en, `energy-kcal_100g`, fat_100g, carbohydrates_100g, sugars_100g, proteins_100g),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$scatterPlot <- renderPlotly({
    data <- workDat()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    x_col <- input$X_var
    y_col <- input$Y_var
    
    p <- plot_ly(data = data, x = ~get(x_col), y = ~get(y_col), 
                 type = "scatter", mode = "markers", name = "", 
                 marker = list(size = 8, color = "#3c8dbc", opacity = 0.7, 
                               line = list(color = '#ffffff', width = 0.5)),
                 text = ~product_name,
                 customdata = ~brands,
                 hovertemplate = paste(
                   '<b>%{text}</b><br>',
                   'Brand: %{customdata}<br>',
                   'X (%{xaxis.title.text}): %{x}<br>',
                   'Y (%{yaxis.title.text}): %{y}<br>',
                   '<extra></extra>'
                 )) %>%
      layout(xaxis = list(title = names(predictor_cols)[predictor_cols == x_col], zeroline = FALSE),
             yaxis = list(title = names(response_cols)[response_cols == y_col], zeroline = FALSE),
             margin = list(t = 20),
             showlegend = FALSE,
             hoverlabel = list(namelength = -1))
    
    if (y_col == "sugars_100g") {
      p <- p %>% layout(
        shapes = list(
          list(
            type = "line", 
            x0 = 0, 
            x1 = max(data[[x_col]], na.rm = TRUE) * 1.1, 
            y0 = 25, 
            y1 = 25,
            line = list(color = "darkgreen", width = 2, dash = "dot")
          )
        )
      )
    }
    p
  })
  
  output$regPlot <- renderPlot({
    data <- workDat()
    if(is.null(data) || nrow(data) < 5) return(NULL)
    
    x_val <- data[[input$X_var]]
    y_val <- data[[input$Y_var]]
    
    m <- lm(y_val ~ x_val)
    
    par(mar = c(4.5, 4.5, 3.5, 1.5))
    plot(x_val, y_val, pch = 19, col = "#3c8dbc", bty = "l",
         xlab = names(predictor_cols)[predictor_cols == input$X_var],
         ylab = names(response_cols)[response_cols == input$Y_var],
         main = paste("Regression Model:", names(response_cols)[response_cols == input$Y_var], 
                      "~", names(predictor_cols)[predictor_cols == input$X_var]))
    
    abline(m, col = "red", lwd = 2, lty = 2)
    
    if(input$Y_var == "sugars_100g") {
      abline(h = 25, col = "darkgreen", lwd = 2, lty = 3)
      legend("topleft", 
             legend = c("Fitted Regression Line (OLS)", "AHA/WHO Daily Sugar Limit (25g)"), 
             col = c("red", "darkgreen"), 
             lty = c(2, 3), 
             lwd = 2, 
             bty = "n", 
             cex = 0.9)
    } else {
      legend("topleft", 
             legend = c("Fitted Regression Line (OLS)"), 
             col = c("red"), 
             lty = c(2), 
             lwd = 2, 
             bty = "n", 
             cex = 0.9)
    }
  })
  
  output$resPlot <- renderPlot({
    data <- workDat()
    if(is.null(data) || nrow(data) < 5) return(NULL)
    
    m <- lm(data[[input$Y_var]] ~ data[[input$X_var]])
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    plot(m, col = "#3c8dbc")
  })
  
  output$predPlot <- renderPlot({
    data <- workDat()
    if(is.null(data) || nrow(data) < 5) return(NULL)
    
    x_val <- data[[input$X_var]]
    y_val <- data[[input$Y_var]]
    
    m <- lm(y_val ~ x_val)
    pred.y = coef(m)[1] + coef(m)[2] * input$newX
    
    par(mar = c(4.5, 4.5, 2.5, 1.5))
    plot(x_val, y_val, bty = "l", col = "#3c8dbc", pch = 19,
         xlab = names(predictor_cols)[predictor_cols == input$X_var],
         ylab = names(response_cols)[response_cols == input$Y_var],
         main = "Hypothetical Prediction (Gold Diamond) on Regression Line")
    abline(m, col = "red", lwd = 1.5, lty = 2)
    points(input$newX, pred.y, pch = 23, bg = "gold", col = "red", cex = 2.5, lwd = 2)
  })
  
  output$boxPlot <- renderPlot({
    data <- workDat()
    if(is.null(data) || nrow(data) == 0) return(NULL)
    
    ggplot(data = data, aes(x = .data$macro_category, y = .data[[input$X_var]], fill = .data$macro_category)) +
      geom_boxplot(outlier.alpha = 0.4, outlier.size = 1.2) +
      scale_fill_brewer(palette = "Oranges") + 
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold", size = 11, margin = margin(t = 10)),
            panel.grid.major.x = element_blank()) +
      labs(x = "Broad Food Grouping", y = names(predictor_cols)[predictor_cols == input$X_var]) +
      guides(fill = "none") +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "darkred")
  })
  
  output$densityPlot <- renderPlotly({
    data <- workDat()
    if(is.null(data) || nrow(data) < 5) return(NULL)
    
    x_col_var <- input$X_var
    den <- density(data[[x_col_var]], na.rm = TRUE)
    
    p <- plot_ly(x = ~den$x, y = ~den$y, type = 'scatter', mode = 'lines', fill = 'tozeroy',
                 color = I("#3c8dbc"), name = "") %>%
      layout(xaxis = list(title = names(predictor_cols)[predictor_cols == x_col_var], zeroline = FALSE),
             yaxis = list(title = 'Density', zeroline = FALSE))
    p
  })
  
  output$regFormulaText <- renderText({
    data <- workDat()
    if(is.null(data) || nrow(data) < 5) return("")
    
    m <- lm(data[[input$Y_var]] ~ data[[input$X_var]])
    intercept <- round(coef(m)[1], 2)
    slope <- round(coef(m)[2], 2)
    
    paste0("Mathematical Formula for Selected Category: Y = ", slope, "X + ", intercept)
  })
  
}


# Launch 

shinyApp(ui = ui, server = server)
