library(shinydashboard)
library(dqshiny)
library(shiny)
library(dplyr)
library(ggplot2)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(Rcpp)
library(shinybusy)
library(DT)
library(hrbrthemes)
library(viridis)
library(plotly)

wine <- read.csv(file='wine.csv', header=T, sep=',')

## shiny application
# UI part
ui <- dashboardPage(
  skin = "blue",
  # Header
  dashboardHeader(title = "wineR"),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main_page"),
      menuItem("Search", tabName = "search"),
      menuItem("Data", tabName = 'eda'),
      menuItem("Recommendation", tabName = 'recommendation'),
      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #A94978;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #B44978;
        }
        .skin-blue .main-header .navbar {
          background-color: #B44978;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #B44978;
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu .a:hover{
        font-weight: bold; font-size: 14px;
        
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        font-weight: bold; font-size: 14px;
        background-color: #B44978;
        }
                                

      ')))
    )
  ),
  # Body
  dashboardBody(
    tabItems(
      tabItem(tabName = "main_page",
              img(src = 'wineR_logo2.png', height = 300, width = 300),
              h2("Welcome to wineR!"),
              h4("Our goal is to improve customers' experience in choosing wines."),
              h4("We hope you enjoy our application!"),
              align="center"),
      # UI Search tab
      tabItem(tabName = "search",
              h2("Welcome - Search and Learn about wines!"),
              autocomplete_input(id = "des_name_search",
                                 label = h3("Search Wine Name"),
                                 max_options = 100,
                                 wine$name_winery_region
                                 ),
              p(),
              fluidRow(
                # Description tab - Wine Summary box
                box(
                  title = "Summary", status = "primary", 
                  solidHeader = TRUE, width = 12,
                  collapsible = TRUE,
                  fluidRow(
                    tags$head(tags$style(HTML(".small-box {height: 100px}
                                              .small-box.bg-blue {
                                              background-color: #DDADB8 !important;
                                              }"))),
                    valueBoxOutput("country_box", width = 4),
                    valueBoxOutput("region_box", width = 4),
                    valueBoxOutput("winery_box", width = 4),
                    valueBoxOutput("type_box", width = 3),
                    valueBoxOutput("rating_box", width = 3),
                    valueBoxOutput("price_box", width = 3),
                    valueBoxOutput("year_box", width = 3)
                  ),
                  tags$style(HTML("
                                  .box.box-solid.box-primary>.box-header{
                                  background: #B44978;
                                  }
                                  .box.box-solid.box-primary{
                                  border-bottom-color:#B44978;
                                  border-left-color:#B44978;
                                  border-right-color:#B44978;
                                  border-top-color:#B44978;
                                  }
                                  "
                                  ))
                  ),
                # Description tab - Price range box
                box(
                  title = "Price Range of all types by country", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput("price_range", height = 250),
                  sliderInput(inputId="price_range2",
                              label = "Choose price range! (Max=$3410.79)",
                              min = 1, max = max(wine$Price), 
                              value = c(1, 150))
                  ),
                # Description tab - Rating range box
                box(
                  title = "Rating range of all types by country", status = "primary", 
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput("rating_range", height = 250),
                  sliderInput(inputId="rating_range2",
                              label = "Choose rating range! (Max=5)",
                              min = 1, max = 5,
                              value = c(3, 5), step=0.1)
                  )
                )
              ),
      # UI Data tab
      tabItem(tabName = "eda",
              h2("Explore our data of 13,090 wines and find insights!"),
              fluidRow(
                # EDA tab - 1. Avg. Rating by Wine Type box
                box(
                  title = "Avg. Rating by Wine Type", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput('eda_bar1', height = "250px")
                ),
                # EDA tab - 2. Avg. Price by Wine Type box
                box(
                  title = "Avg. Price by Wine Type", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput('eda_bar2', height = "250px")
                ),
                # EDA tab - 3. Avg. Rating by Country box
                box(
                  title = "Avg. Rating by country", status = "primary",
                  solidHeader = TRUE, width = 6, height="420px",
                  collapsible = TRUE,
                  autocomplete_input(id = "eda_coun_search1",
                                     label = "Type Country",
                                     max_options = 10,
                                     unique(wine$Country)),
                  
                  actionButton(inputId = "countrygo1", label = "Show"),
                  plotOutput(outputId = "eda_bar3", height="250px")
                ),
                # EDA tab - 4. Avg. Price by Country box
                box(
                  title = "Avg. Price by country", status = "primary",
                  solidHeader = TRUE, width = 6, height="420px",
                  collapsible = TRUE,
                  autocomplete_input(id = "eda_coun_search2",
                                     label = "Type Country",
                                     max_options = 10,
                                     unique(wine$Country)),
                  actionButton(inputId = "countrygo2", label = "Show"),
                  plotOutput(outputId = "eda_bar4", height="250px")
                ),
                # EDA tab - 5. Relationship between Rating and Price box
                box(
                  title = "Relationship between Rating and Price", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput('eda_scat5'),
                  sliderInput(inputId="price_range3",
                              label = "Choose price range! (max=$3410.79)",
                              min = 1, max = max(wine$Price), 
                              value = c(50, 500)),
                  sliderInput(inputId="rating_range3",
                              label = "Choose rating range! (Max=5)",
                              min = 1, max = 5, 
                              value = c(3, 5), step=0.1)
                ),
                # EDA tab - 6. Relationship between Rating and Number of Ratings box
                box(
                  title = "Relationship between Rating and Number of Ratings", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput('eda_scat6'),
                  sliderInput(inputId="rating_range4",
                              label = "Choose rating range! (max=5)",
                              min = 1, max = 5, 
                              value = c(3, 5), step=0.1),
                  sliderInput(inputId="num_rating",
                              label = "Choose numbers of rating range! (Max=94,287)",
                              min = 1, max = max(wine$NumberOfRatings), 
                              value = c(25, 10000))
                ),
                # EDA tab - 7. Top 10 Countries by Rating
                box(
                  title = "Top 10 Countries by Rating", status = "primary",
                  solidHeader = TRUE, width = 6, height="420px",
                  collapsible = TRUE,
                  checkboxGroupInput(inputId = "search_type1",
                                     label = "Choose wine type",
                                     c("Red" = "Red",
                                       "White" = "White",
                                       "Rose" = "Rose",
                                       "Sparkling" = 'Sparkling'),
                                     inline=TRUE),
                  actionButton(inputId = "typego1", label = "Show"),
                  plotOutput(outputId = "type_rating", height="250px")
                ),
                # EDA tab - 8. Top 10 Countries by Price
                box(
                  title = "Top 10 Countries by Price", status = "primary",
                  solidHeader = TRUE, width = 6, height="420px",
                  collapsible = TRUE,
                  checkboxGroupInput(inputId = "search_type2",
                                     label = "Choose wine type",
                                     c("Red" = "Red",
                                       "White" = "White",
                                       "Rose" = "Rose",
                                       "Sparkling" = 'Sparkling'),
                                     inline=TRUE),
                  actionButton(inputId = "typego2", label = "Show"),
                  plotOutput(outputId = "type_price", height="250px")
                ),
                # EDA tab - 9. Relationship between Average ratings by Year filtered by Country
                box(
                  title = "Relationship between Average ratings by Year filtered by Country", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput('graph_9'),
                  autocomplete_input(id = "search_country_9",
                                     label = "Type Country",
                                     max_options = 10,
                                     unique(wine$Country)),
                  
                  checkboxGroupInput("Wine_Type1", "Wine Type",
                                     c("Red" = "Red",
                                       "White" = "White",
                                       "Sparkling"="Sparkling",
                                       "Rose" = "Rose"
                                     ),
                                     inline=TRUE
                  )
                ),
                # EDA tab - 10. Relationship between Average Price by Year filtered by Wine Type
                box(
                  title = "Relationship between Average Price by Year filtered by Wine Type", status = "primary",
                  solidHeader = TRUE, width = 6,
                  collapsible = TRUE,
                  plotOutput('graph_10'),
                  autocomplete_input(id = "search_country_10",
                                     label = "Type Country",
                                     max_options = 10,
                                     unique(wine$Country)),
                  checkboxGroupInput("Wine_Type2", "Wine Type",
                                     c("Red" = "Red",
                                       "White" = "White",
                                       "Sparkling"="Sparkling",
                                       "Rose" = "Rose"
                                     ),
                                     inline=TRUE
                  )
                ),
                # EDA tab - 11. Average Rating and average price by different regions
                box(title = "Average Rating and average price by different regions", status = "primary",
                    solidHeader = TRUE, width = 6,
                    collapsible = TRUE,
                    plotlyOutput("graph_11"),
                    autocomplete_input(id = "search_country_11",
                                       label = "Type Country",
                                       max_options = 10,
                                       unique(wine$Country))
                    )
              )
              ),
      
      # UI Recommendation tab
      tabItem(tabName = "recommendation",
              h2("Get recommendations of list of wines!"),
              column(4,
                     wellPanel(
                       style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                       sliderInput(inputId = "budget",
                                   label = "1. Choose your budget",
                                   min = 10, max = 500, value = 250),
                       sliderInput(inputId = "pri_ran",
                                   label = "2. Choose preferred price",
                                   min = 1, max = 500, 
                                   value = 20),
                       sliderInput(inputId = 'target_rating',
                                   label = "3. Choose average rating",
                                   min = 1, max = 5,
                                   value = 3, step = 0.1),
                       
                       checkboxGroupInput(inputId = "type_wine",
                                          label = "4. Choose type of wine",
                                          c("Red" = "Red",
                                            "White" = "White",
                                            "Rose" = "Rose",
                                            "Sparkling" = 'Sparkling'))
                       ,actionButton(inputId = "shuffle", label = "Get wines!")
                     )),
              column(8,
                     add_busy_spinner(spin = "fading-circle",
                                      margin = c(65, 20),
                                      color = 'green'),
                       style = "height:450px; overflow-y: scroll;",
                       fluidRow(
                         box(title= "Average Rating", status = "primary",
                             width=4, height="90px",
                             solidHeader = TRUE,
                             textOutput("avg_rating"),
                             tags$style(type="text/css", 
                                        "#avg_rating 
                                        { width: 100%; 
                                        text-align:center; font-size: 30px; display: block;
                                        margin-top: -10px}
                                        div.box-header
                                        {text-align: center;}")),
                         box(title= "Total Price", status = "primary",
                             width=4, height="90px",
                             solidHeader = TRUE,
                             textOutput("sum_price"),
                             tags$style(type="text/css", 
                                        "#sum_price 
                                        { width: 100%; 
                                        text-align:center; font-size: 30px; display: block;
                                        margin-top: -10px}")),
                         box(title= "Number of wines", status = "primary",
                             width=4, height="90px",
                             solidHeader = TRUE,
                             textOutput("num_list"),
                             tags$style(type="text/css", 
                                        "#num_list
                                        { width: 100%; 
                                        text-align:center; font-size: 30px; display: block;
                                        margin-top: -10px}"))
                       ),
                     fluidRow(
                       DT::dataTableOutput("opt_show")
                       )
                     
                     )
              )
      )
    ),
    
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 18px;
                              }
                              ')))
  )
  

server <- function(input, output) {

  
  wine_coun <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Country']
  })
  
  wine_reg <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Region']
  })
  
  wine_win <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Winery']
  })
  
  wine_typ <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Type']
  })
  
  wine_rat <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Rating']
  })
  
  wine_pri <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Price']
  })
  
  wine_year <- reactive({
    wine[wine$name_winery_region == input$des_name_search, 'Year']
  })
  
  # Description part - 7 wine description box outputs
  output$country_box <- renderValueBox({
    valueBox(
      value = wine_coun(), 
      subtitle = "Country", 
      color = "blue",
      icon("globe")
    )
  })
  output$region_box <- renderValueBox({
    valueBox(
      value = wine_reg(), 
      subtitle = "Region", 
      color = "blue",
      icon("map")
    )
  })
  output$winery_box <- renderValueBox({
    valueBox(
      value = wine_win(), 
      subtitle = "Winery", 
      color = "blue",
      icon("map-marker-alt")
    )
  })
  output$type_box <- renderValueBox({
    valueBox(
      value = wine_typ(), 
      subtitle = "Type", 
      color = "blue",
      icon("wine-glass-alt")
    )
  })
  output$rating_box <- renderValueBox({
    valueBox(
      value = wine_rat(), 
      subtitle = "Rating", 
      color = "blue",
      icon("smile")
    )
  })
  output$price_box <- renderValueBox({
    valueBox(
      value = wine_pri(), 
      subtitle = c("Price (USD)"), 
      color = "blue",
      icon("dollar-sign")
    )
  })
  output$year_box <- renderValueBox({
    valueBox(
      value = wine_year(),
      subtitle = "Produced Year", 
      color = "blue",
      icon("calendar-alt")
    )
  })
  
  # Description tab - 2 wine description plots
  output$price_range <- renderPlot({
    price_coun <- wine[wine$Country ==wine_coun(), c("Price", "Type")]
    ggplot(price_coun, aes(x=Type, y=Price, fill=Type)) + geom_boxplot() +
      ylim(input$price_range2) +
      labs(x = "Type of wine", y = "Price(USD)") +
      geom_hline(yintercept=wine_pri(), color = "red", size=1)
  })
  output$rating_range <- renderPlot({
    price_rat <- wine[wine$Country==wine_coun(), c("Rating", "Type")]
    ggplot(price_rat, aes(x=Type, y=Rating, fill=Type)) + geom_boxplot() +
      ylim(input$rating_range2) +
      labs(x = "Type of wine", y = "Rating") +
      geom_hline(yintercept=wine_rat(), color = "red", size=1)
  })
  
  # EDA tab - graphs 1
  output$eda_bar1 <- renderPlot({
    wine_avg_rating <- summarise(group_by(wine,Type),'Avg.Rating' = mean(Rating,na.rm=TRUE))
    ggplot(wine_avg_rating,aes(x=Type,y=Avg.Rating, fill=Type)) +
      geom_bar(stat = 'identity')
  })
  # EDA tab - graphs 2
  output$eda_bar2 <- renderPlot({
    wine_avg_price<- summarise(group_by(wine,Type),'Avg.Price' = mean(Price,na.rm=TRUE))
    ggplot(wine_avg_price,aes(x=Type,y=Avg.Price, fill=Type)) +
    geom_bar(stat = 'identity')
  })
  # EDA tab - graphs 3
  box3_rating <- eventReactive(input$countrygo1,{
    aggregate(Rating ~ Type, 
              data = filter(wine, Country == input$eda_coun_search1), 
              FUN = mean)})
  output$eda_bar3 <- renderPlot({
    ggplot(data = box3_rating(), aes(x = Type,y = Rating, fill=Type)) +
      geom_bar(stat = 'identity') +
      labs(title = paste("Average rating by types in",isolate(input$eda_coun_search1))) +
      theme(plot.title = element_text(color="black", size=14, face="bold"))
  })
  # EDA tab - graphs 4
  box3_price <- eventReactive(input$countrygo2,{
    aggregate(Price ~ Type, 
              data = filter(wine, Country == input$eda_coun_search2), 
              FUN = mean)})
  output$eda_bar4 <- renderPlot({
    ggplot(data = box3_price(), aes(x = Type,y = Price, fill=Type)) +
      geom_bar(stat = 'identity') +
      labs(title = paste("Average price by types in",isolate(input$eda_coun_search2))) +
      theme(plot.title = element_text(color="black", size=14, face="bold"))
  })
  # EDA tab - graphs 5
  output$eda_scat5 <- renderPlot({
    ggplot(wine, aes(Rating,Price, color=Type)) + geom_point() +
      xlim(input$rating_range3) +
      ylim(input$price_range3)
  })
  # EDA tab - graphs 6
  output$eda_scat6 <- renderPlot({
      ggplot(wine, aes(Rating,NumberOfRatings, color=Type)) + geom_point() +
      xlim(input$rating_range4) +
      ylim(input$num_rating)
  })
  # EDA tab - graphs 7
  EDA7 <- eventReactive(input$typego1, {
    aggregate(Rating ~ Country, data = filter(wine, Type == input$search_type1), FUN = mean) %>%
      arrange(desc(Rating))})
  output$type_rating <- renderPlot({
    ggplot(EDA7()[1:10,], aes(x = reorder(Country,Rating), y = Rating, fill = Country))+
      geom_bar(stat = 'identity') +
      labs(title ="Top 10 countries with the highest average rating", x="Country", y="Avg. Rating") +
      coord_flip() +
      theme(plot.title = element_text(color="black", size=14, face="bold")
        ,legend.position = "none")
  })
  # EDA tab - graphs 8
  EDA8 <- eventReactive(input$typego2, {
    aggregate(Price ~ Country, data = filter(wine, Type == input$search_type2), FUN = mean) %>%
      arrange(desc(Price))})
  output$type_price <- renderPlot({
    ggplot(EDA8()[1:10,], aes(x = reorder(Country,Price), y = Price, fill = Country))+
      geom_bar(stat = 'identity') +
      labs(title ="Top 10 countries with the highest average price", x="Country", y="Avg. Price") +
      coord_flip() +
      theme(plot.title = element_text(color="black", size=14, face="bold")
        ,legend.position = "none")
  })   
  # EDA tab - graphs 9
  EDA9 <- reactive({
    wine %>% 
      filter(Country %in% input$search_country_9,
             Type %in% input$Wine_Type1) %>% 
      group_by(Year) %>%
      summarize(avg_rating = mean(Rating))
  })
  output$graph_9 <- renderPlot({
    ggplot(data=EDA9(),aes(x=Year,y=avg_rating)) +
      geom_line() +
      theme(axis.text.x = element_text(size = 7, angle = 45))
  })
  
  # EDA tab - graphs 10
  EDA10 <- reactive({
    wine %>% 
      filter(Country %in% input$search_country_10,
             Type %in% input$Wine_Type2) %>% 
      group_by(Year) %>%
      summarize(avg_price = mean(Price))
  })
  output$graph_10 <- renderPlot({
    ggplot(data=EDA10(),aes(x=Year,y = avg_price)) +
      geom_line()
  })
  
  # EDA tab - graphs 11
  EDA11 <- reactive({
    wine %>% 
      filter(Country %in% input$search_country_11) %>% 
      group_by(Country, Region) %>% 
      summarize(avg_rating = round(mean(Rating), 2),
                avg_price = round(mean(Price),2),
                number_of_wines = n())
  })
  
  output$graph_11 <- renderPlotly({
    ggplotly(ggplot(EDA11(), aes(avg_price, avg_rating, size=number_of_wines, color=Region)) + 
    geom_point(alpha=0.5) +
      scale_size(range = c(1, 18)) +
      theme(legend.position="none"))
    })

  # Optimization tab
  opt <- eventReactive(input$shuffle, {
    opt1 <- subset(wine, Type == input$type_wine & 
             Price >= input$pri_ran,
             select = -c(NumberOfRatings, name_winery_region))
    model <- MIPModel() %>%
      add_variable(x[i], i =1:nrow(opt1), type = "binary") %>%
      set_objective(sum_expr(x[i], i = 1:nrow(opt1)), "max") %>%
      add_constraint(sum_expr(opt1$Price[i]*x[i], i = 1:nrow(opt1)) <= input$budget)%>% 
      add_constraint(sum_expr(opt1$Rating[i] * x[i] - x[i]*input$target_rating, i = 1:nrow(opt1)) >= 0)
    result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
    df_solution <- as.data.frame(result$solution)
    names(df_solution) <- "id"
    opt1$id <- ifelse(df_solution$id==1.00, 1, 0)
    subset(opt1, id==1, select = -id) 
  }
  )

  output$opt_show <- DT::renderDataTable(
    opt(),
    options = list(scrollX = TRUE,
                   pageLength = 5),
    rownames = FALSE
    )

  output$avg_rating <- renderText({
      round(mean(opt()$Rating), 2)
  })
  
  output$sum_price <- renderText({
      paste("$", sum(opt()$Price))
  })
  
  output$num_list <- renderText({
      paste(nrow(opt()),"wines")
  })
  
}

shinyApp(ui=ui, server=server)