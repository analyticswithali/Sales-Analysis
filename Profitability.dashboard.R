## app.R ##

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(plotly)
library(rsconnect)

QP2 <- read.csv("D7_Office.csv")
attach(QP2)

ui <- dashboardPage(
   
   # Application title
  dashboardHeader(title = "Profitability Dashboard"),
   
   # Sidebar with Check Boxes to adjust output 
   dashboardSidebar(width = 150,
     fluidRow(
       #Filters by Region  
       checkboxGroupInput(inputId = "RegionInput",
                     label = "Region",
                    choices = c("Central", "East", "South", "West"),  selected = c("Central", "East", "South", "West")),
       #Filters by Category 
       checkboxGroupInput(inputId = "CategoryInput",
                           label = "Product Category",
                           choices = c("Furniture","Office Supplies", "Technology"), selected = c("Furniture","Office Supplies", "Technology")),
       #filters by Sub-Category 
       checkboxGroupInput(inputId = "SubInput",
                    label = "SubCategory",
                    choices = c("Accessories", "Appliances","Art","Binders","Bookcases","Chairs","Copiers", "Envelopes","Fasteners" , "Furnishings",
                                "Labels", "Machines", "Paper" ,"Phones", "Storage", "Supplies", "Tables"), 
                    selected = c("Accessories", "Appliances","Art","Binders","Bookcases","Chairs","Copiers", "Envelopes","Fasteners" , "Furnishings",
                                 "Labels", "Machines", "Paper" ,"Phones", "Storage", "Supplies", "Tables")))),
      
      # Body of the dashboard where the Charts will be plotted
      dashboardBody(
                    fluidRow( 
                              box( title = "Furniture is the least profitable accross all regions", status = "primary",
                                   collapsible = TRUE, solidHeader=TRUE,
                                plotOutput("RegionChart"),
                                 #   infoBox("Furniture is the least profitable accross all regions", 
                                  #          value = NULL, subtitle = NULL, width = 15,
                                   #         icon = shiny::icon("bar-chart")), 
                                    width = 6), 
                              box(title = "Tables has the largest degree of negative profit", status = "primary",
                                 collapsible = TRUE, solidHeader=TRUE,
                                 plotOutput("SubCatChart"), 
                                  #  infoBox("'Tables' has the largest degree of negative profits", width = 15), 
                                    width = 6)),
                    
                    fluidRow(
                              box( title = "Sales Quantities do not translate into profitability" , status = "primary",
                                    collapsible = TRUE, solidHeader=TRUE,
                                    plotOutput("SalesQuantChart"),
                                   # infoBox("Sales Quantities do not translate into profitability", width = 15),
                                    width = 6),
                              box( title = "Discounts decrease profits and inflate sales quantities" , status = "primary",
                                   collapsible = TRUE, solidHeader=TRUE,
                                   plotOutput("Discounts"),
                                   # infoBox("Discounts decrease profits and inflate sales quantities", width = 15),
                                    width = 6)),
                    fluidRow(
                      box( title = "East region lost most on Tables" , status = "primary",
                           collapsible = TRUE, solidHeader=TRUE,
                           plotOutput("SubCatR"),
                           # infoBox("Sales Quantities do not translate into profitability", width = 15),
                           width = 6),
                      box( title = "High discounts decrease profits across all reagion" , status = "primary",
                           collapsible = TRUE, solidHeader=TRUE,
                           plotOutput("DiscountsR"),
                           # infoBox("Discounts decrease profits and inflate sales quantities", width = 15),
                           width = 6))
                    
         
      )
   )


# Define server logic required to draw clustered barcharts
server <- function(input, output) {
  
  
  # create calculated columns using given values from the dataset 
  QP2$Cost <- round(QP2$Sales - Profit,2)
  QP2$DicountGiven <- round(QP2$Sales * Discount,2)
  
  # Top Left Chart {
   output$RegionChart <- renderPlot({
      
     # Grouping data by Category and Region so it can be filtered
     rc <- group_by(QP2, Region, Category) %>%
       summarise(TotalProfit = sum(Profit))
     
     # allows the data displayed in the charts to react to the Region and Product Category filters in the UI
     filtered <- 
       rc %>%
       filter(Region %in% input$RegionInput &
              Category %in% input$CategoryInput)

      # Clustered Bar Chart showing total profits Per Product Category by Region 
      ggplot(filtered, aes(x=Region, y= TotalProfit, fill = Category)) + 
        geom_bar(stat = "identity", position = "dodge", color = "black") + 
        ggtitle("Profits per Category by Region") + 
        labs(y="Total Profits ($)")
     
   })   
   # }
   
   # Top Right {
  output$SubCatChart <- renderPlot({
    
    # Grouping data by Category and Sub-Category so it can be filtered
    scc <- group_by(.data =  QP2, Sub.Category, Category) %>%
      summarise(TotalProfit = sum(Profit))
    
    # allows the data displayed in the charts to react to the Sub-Category and Product Category filters in the UI
    sliced <- 
      scc %>%
      filter(  Category %in% input$CategoryInput & 
               Sub.Category %in% input$SubInput)
    
    # Clustered Bar Chart showing total profits Per Sub-Category by Product Category
    ggplot(sliced, aes(x=Category, y= TotalProfit, fill = Sub.Category, color = Region)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Profits per Sub-Category") + 
      labs(y="Total Profits ($)")
  }) 
  # }

  # Bottom Left Chart {
 output$SalesQuantChart <- renderPlot({
   
   # Grouping data by Category and Sub-Category so it can be filtered 
   sqs <- group_by(QP2,Sub.Category, Category) %>%
      summarise(QuantitySold = sum(Quantity))
    
   # allows the data displayed in the charts to react to the Sub-Category and Product Category filters in the UI
    filters <- 
      sqs %>%
      filter(
               Category %in% input$CategoryInput& 
               Sub.Category %in% input$SubInput)
    
    # Clustered Bar Chart showing quantity Per Sub-Category by Main Category
    ggplot(filters, aes(x=Category, y= QuantitySold, fill = Sub.Category)) + 
      geom_bar(stat = "identity", position = "dodge", color = "black") + 
      ggtitle("Quantity Sold per Sub-Category") + 
      labs(y="Quantity Sold")
  })
 # }
 
 # Bottom Right Chart {
 output$Discounts <- renderPlot({
   
   # Grouping data by Category and Sub-Category so it can be filtered 
   dgsc <- group_by(QP2,Sub.Category, Category) %>%
     summarise(TotalDiscounts = sum(DicountGiven))
   
   # allows the data displayed in the charts to react to the Sub-Category and Product Category filters in the UI
   slices <- 
     dgsc %>%
     filter(
              Category %in% input$CategoryInput& 
              Sub.Category %in% input$SubInput)
   
   # Clustered Bar Chart showing to dicounts given in dollars Per Sub-Category by Product Category
   ggplot(slices, aes(x=Category, y= TotalDiscounts, fill = Sub.Category)) + 
     geom_bar(stat = "identity", position = "dodge", color = "black") + 
     ggtitle("Discounts Given per Sub-Category") + 
     labs(y="Total Discounts ($)")
 })
  # }
 
 # Bottom Right Chart {
 output$DiscountsR <- renderPlot({
   
   # Grouping data by Category and Sub-Category so it can be filtered 
   dgsc <- group_by(QP2,Sub.Category, Region) %>%
     summarise(TotalDiscounts = sum(DicountGiven))
   
   # allows the data displayed in the charts to react to the Sub-Category and Product Category filters in the UI
   slices <- 
     dgsc %>%
     filter(Region %in% input$RegionInput &
     #  Category %in% input$CategoryInput& 
         Sub.Category %in% input$SubInput)
   
   # Clustered Bar Chart showing to dicounts given in dollars Per Sub-Category by Product Category
   ggplot(slices, aes(x=Region, y= TotalDiscounts, fill = Sub.Category)) + 
     geom_bar(stat = "identity", position = "dodge", color = "black") + 
     ggtitle("Discounts by region per Sub-Category") + 
     labs(y="Total Discounts ($)")
 })
 # }
 
 # Bottom Right Chart {
 output$SubCatR <- renderPlot({
   
   # Grouping data by Category and Sub-Category so it can be filtered 
   dgsc <- group_by(QP2,Sub.Category, Region) %>%
     summarise(TotalProfit = sum(Profit))
   
   # allows the data displayed in the charts to react to the Sub-Category and Product Category filters in the UI
   slices <- 
     dgsc %>%
     filter(Region %in% input$RegionInput &
              #  Category %in% input$CategoryInput& 
              Sub.Category %in% c("Tables", "Bookcases", "Machines", "Fasteners"))
   
   # Clustered Bar Chart showing to dicounts given in dollars Per Sub-Category by Product Category
   ggplot(slices, aes(x=Sub.Category, y= TotalProfit, fill = Region)) + 
     geom_bar(stat = "identity", position = "dodge", color = "black") + 
     ggtitle("Profit by Region per problematic Sub-Category") + 
     labs(y="Total Profit ($)")
 })
 # }
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)

