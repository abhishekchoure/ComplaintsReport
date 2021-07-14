# Load R packages

####################################

#Abhishek Pramod Choure 1132200437
#Ajinkya Anand Kuchake 1132200438
#Ayeshwarya Suresh Gotkhindi 1132200511

####################################



library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(htmltools)
library(ECharts2Shiny)

dashboardUI <- dashboardPage(
  dashboardHeader(title = "Complaints Report (2017/2018)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Map", tabName = "Map", icon = icon("map-marked-alt")),
      menuItem("Statistics", tabName = "Statistics", icon = icon("chart-line")),
      menuItem("Media Analysis", tabName = "Media", icon = icon("photo-video"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
    # Boxes need to be put in a row (or column)
       fluidRow(
        # A static valueBox
        box(title="Complaints Overview", valueBox(nrow(FinalDataSet), "Total Number of complaints", icon = icon("exclamation")),
             valueBoxOutput("online"),
             valueBoxOutput("offline"), width="100%"),
        box(title="Overview of Dataset",
          valueBox(nlevels(as.factor(FinalDataSet$Ward.Office)), "Ward Offices", icon = icon("building") , color = "blue"),
          valueBox(nlevels(as.factor(FinalDataSet$Area)), "Areas Covered", icon = icon("map-marker-alt"), color = "olive"),
          valueBoxOutput("missingBox"),
          valueBoxOutput("missingBox2")
        ,width="100%"),
        
        box(title="Complaint Status",
            infoBoxOutput("closed"),   
            infoBoxOutput("inprocess"),
            infoBoxOutput("reject"),
            infoBoxOutput("reopen")
        ,width="100%")
       ),
       fluidRow(
         box(title="Top Five Types of Complaints",status="warning",tableOutput("categoryTable")),
         box(title="Complaints per Ward Office",status="warning",tableOutput("wardOfficeTable")),
        )
      ),
      tabItem(tabName = "Map",
          fluidRow(
            box(title="Inputs for the map",
              selectInput(inputId = "mapCategory", "Select Category :", categoryCountDF$x , selected = "Bhavan", multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL),
              
            leafletOutput("map"),width = "100%")
            ),
          fluidRow(
            
          ) ,
          fluidRow(
            box(
              selectInput(inputId = "mapSubCategory", "Select Sub-Category :", subCategoryCountDF$x , selected = "", multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL),
              leafletOutput("map2") ,width="100%")
          ),
          fluidRow(
            box(title="Distribution of Complaints",dataTableOutput("mapTable"),width = 500)
          ) 
      ),
      tabItem(tabName = "Statistics",
          fluidRow(
              box(
                selectInput(inputId = "selectCategory", "Select Category :", categoryCountDF$x , selected = "Bhavan", multiple = FALSE,
                            selectize = TRUE, width = NULL, size = NULL),
                
                box(title="Graph showing distribution complaints of selected Category",plotOutput("plot5"),width="100%")
              ,width="100%")
          ),
    
          fluidRow(
            box(plotOutput("plot4"),width="100%")
          ),
          fluidRow(
            box(
            selectInput(inputId = "selectSubCategory", "Select Sub-Category :", subCategoryCountDF$x , selected = "", multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            selectInput(inputId = "selectArea", "Select Area :", areaCountDF$x , selected = "", multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            box(title="Actual Complaints",dataTableOutput("table1"),width = "100%"),width="100%")
          )
      ),
      tabItem(tabName = "Media",
        fluidRow(
          box(title="Input",
            selectInput(inputId = "inpCategory", "Select Category :", categoryCountDF$x , selected = "Bhavan", multiple = FALSE,
              selectize = TRUE, width = NULL, size = NULL),
            box(valueBoxOutput("noofcomplaints"),
                valueBoxOutput("noofsubcategory"),width="100%"),
            box(plotOutput("plot6", height = 500),width="100%")
          , width="100%"),
        ),
        fluidRow(
          box(title="Media Type",dataTableOutput("mediaTable")),
          box(title="Media Used",dataTableOutput("table2"))
        )
      )
    )
  ), skin = "red"   
)


dashboardServer <-  function(input, output,session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  observe(
    {
      selectCategoryInput <- input$selectCategory
      if(is.null(selectCategoryInput)){
        selectCategoryInput <- character(0)
      }
      
      updateSelectInput(
        session,
        "selectSubCategory",
        choices = getSubCategoriesNames(selectCategoryInput)
      )
    }
  )
  
  observe(
    {
      selectCategoryInput <- input$mapCategory
      if(is.null(selectCategoryInput)){
        selectCategoryInput <- character(0)
      }
      
      updateSelectInput(
        session,
        "mapSubCategory",
        choices = getSubCategoriesNames(selectCategoryInput)
      )
    }
  )
  
  
  observe(
    {
      selectCategoryInput <- input$selectCategory
      selectSubCategoryInput <- input$selectSubCategory
      if(is.null(selectCategoryInput)){
        selectCategoryInput <- character(0)
      }
      
      updateSelectInput(
        session,
        "selectArea",
        choices = getAreaNames(selectCategoryInput,selectSubCategoryInput)
      )
    }
  )

  output$table1 <- renderDataTable({
    tableData <- getComplaintsByArea(input$selectCategory,input$selectSubCategory,input$selectArea)
    tableData
  })
  
  output$table2 <- renderDataTable({
    tableData <- getMediaByCategory(input$inpCategory)
    tableData
  })
  
  output$mapTable <- renderDataTable({
    data <- getMapData(input$mapCategory)
    tableData <- select(data,x,freq)
    colnames(tableData) <- c("Ward Office","Complaints")
    tableData
  })
  
  
  output$categoryTable <- renderTable({
    tableData <- getTopFiveCategory()
    colnames(tableData) <- c("Category" , "No of Complaints")
    tableData
  },width = "90%")
  
  
  output$wardOfficeTable <- renderTable({
    colnames(wardOfficeCategoryCountDF) <- c("Ward Office" , "Complaints")
    arrange(wardOfficeCategoryCountDF , desc(Complaints))
  },width = "90%")
  
  output$mediaTable <- renderDataTable({
    tableData <- getMediaTypes(input$inpCategory)
    tableData
  })
  
  output$plot2 <- renderPlot(
    barplot(
      categoryCountDF$freq[match(input$selectCategory,
                                 categoryCountDF$x)],names.arg=categoryCountDF$x[match(input$selectCategory,categoryCountDF$x)],
      main="Number of Complaints per Category",space=1)
  )
  
  output$plot3 <- renderPlot(
    barplot(subCategoryCountDF$freq[match(input$selectSubCategory,
                                          subCategoryCountDF$x)],names.arg=subCategoryCountDF$x[match(input$selectSubCategory,subCategoryCountDF$x)],
            main="Number of Complaints per Sub-Category",space=1)
  )
  
  output$plot4 <- renderPlot({
    x<-getSubCategoriesDF(input$selectCategory)
    #pie(x$freq,labels=x$x,main="Complaints of Category")
    pie <- ggplot(data=x, aes(x="", y=freq , fill=x)) +
      geom_bar(stat="identity",width = 1) + coord_polar("y", start=0) + theme_minimal() + theme(legend.position="bottom") 
    pie
  })

  
  output$plot5 <- renderPlot({
    x<-getSubCategoriesDF(input$selectCategory)
    ggplot(data=x, aes(x=x, y=freq , fill=x)) +
    geom_bar(stat="identity") +theme_minimal() + theme(legend.position="bottom") 
    #barplot(x$freq,names.arg = x$x,main="Complaints of Category")
  })
  
  output$plot6 <- renderPlot({
    x<-getMediaByCategory(input$inpCategory)
    ggplot(data=x, aes(x=x, y=freq , fill=x)) +
    geom_bar(stat="identity") +theme_minimal() + theme(legend.position="bottom") 
  })
  

  
  output$map <- renderLeaflet({
    df <- getMapData(input$mapCategory)
    map = leaflet(df) %>% addCircles(lng=~long,lat=~lat,
                               radius = ~sqrt(freq) * 50
                               ,weight = 1) %>% addMarkers(~long, ~lat, label = ~htmlEscape(x)) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
    
    map %>% setView(lng = 73.8567, lat = 18.5204, zoom = 12)
  })
  
  output$map2 <- renderLeaflet({
    df2 <- getMapDataSC(input$mapCategory,input$mapSubCategory)
    map = leaflet(df2) %>% addCircles(lng=~long,lat=~lat,
                                     radius = ~sqrt(freq) *100
                                     ,weight = 1,color = "red") %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
    
    map %>% setView(lng = 73.8567, lat = 18.5204, zoom = 12)
  })
  
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(95, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })

  
  output$missingBox <- renderValueBox({
    missingDF <- getMissingAreas()
    valueBox(
      nrow(missingDF), "Unknown Areas", icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$missingBox2 <- renderValueBox({
    missingDF <- getMissingOffices()
    valueBox(
      nrow(missingDF), "Unknown Ward Offices", icon = icon("question-circle"),
      color = "maroon"
    )
  })
  
  output$online <- renderValueBox({
    valueBox(
      nrow(onlineComplaints), "Online Complaints", icon = icon("globe"),
      color = "green"
    )
  })
  
  output$offline <- renderValueBox({
    valueBox(
      nrow(offlineComplaints), "Offline Complaints", icon = icon("scroll"),
      color = "maroon"
    )
  })
  
  output$complaints <- renderValueBox({
    valueBox(
      nrow(offlineComplaints), "Offline Complaints", icon = icon("scroll"),
      color = "maroon"
    )
  })
  
  output$closed <- renderInfoBox({
    infoBox(
      "Closed", statusCountDF$freq[1], icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$inprocess <- renderInfoBox({
    infoBox(
      "In Process", statusCountDF$freq[2], icon = icon("flag"),
      color = "olive"
    )
  })
  
  output$reject <- renderInfoBox({
    infoBox(
      "Rejected", statusCountDF$freq[3], icon = icon("trash-alt"),
      color = "red"
    )
  })
  
  output$reopen <- renderInfoBox({
    infoBox(
      "Reopened", statusCountDF$freq[4], icon = icon("lock-open"),
      color = "teal"
    )
  })
}


# Create Shiny object
shinyApp(ui = dashboardUI, server = dashboardServer)


