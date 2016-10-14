####################################################################################
#######          Shiny app for accuracy assessment design       ####################
#######    contributors:  Remi d'Annunzio, Yelena Finegold,     ####################
#######            Antonia Ortmann, Erik Lindquist              ####################
#######              FAO Open Foris SEPAL project               ####################
#######  contact: remi.dannunzio | yelena.finegold @fao.org     ####################
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or 
# software or in the documentation accompanying it, for program maintenance and 
# upgrading as well as for any # damage that may arise from them. FAO also declines 
# any responsibility for updating the data and assumes no responsibility for errors 
# and omissions in the data provided. Users are, however, kindly asked to report any 
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2016/09/29
####################################################################################


####################################################################################
#######          Set options and necessary packages       ##########################
####################################################################################
options(shiny.launch.browser=T)
options(stringsAsFactors=FALSE)

########################################
# include all the needed packages here #

# packages <- function(x){
#   x <- as.character(match.call()[[2]])
#   if (!require(x,character.only=TRUE)){
#     install.packages(pkgs=x,repos="http://cran.r-project.org")
#     require(x,character.only=TRUE)
#   }
# }

library(ggplot2)
library(xtable)
library(raster)
library(shiny)
library(shinydashboard)
library(dismo)
library(stringr)
library(snow)
library(plyr)
library(leaflet)
library(RColorBrewer)
library(DT)
library(rgeos)
library(rgdal)
library(shinyFiles)
library(shinyBS)
library(htmltools)

####################################################################################
#######       PART I : Setup the Graphic Interface        ##########################
####################################################################################
print("Starting the process")

ui <- dashboardPage(skin='green',
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
    title= 'Accuracy assessment analysis',
    titleWidth = 350),

    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
      menuItem('1: Input', tabName = 'Input', icon = icon("picture-o")),
      menuItem('2: Check', tabName = 'Check', icon = icon("area-chart")),
      menuItem('3: Results', tabName = 'Results', icon = icon("map-marker"))    )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
    tabItems(
      
      ####################################################################################
      # New Tab
      tabItem(tabName = "Intro",
               fluidRow(
                 
                 ####################################################################################
                 # New box
                 box(
                   title= "Description", status = "success", solidHeader= TRUE,
                   "This interactive tool calculates results from accuracy assessment.",
                   br(),
                   "The objective of this tool is to create confusion matrices and calculate bias corrected 
                   estimates and confidence intervals around these estimates.",
                   br(),
                   "For support, post on the ",
                   a(href="http://www.openforis.org/support"," Open Foris support forum.",target="_blank")
                  ),
                 
                 
                 ####################################################################################
                 # New box
                 box(
                    title= "Background", status = "success", solidHeader= TRUE,
                    "The aim of a map accuracy assessment is to characterize the frequency of errors (omission and commission) for each map class.
                    Differences in these two errors may be used to adjust area estimates and also to estimate the uncertainties (confidence intervals) for the areas for each class.
                    Bias corrected area estimates on the basis of a rigorous accuracy assessment represents an improvement over simply reporting the areas of map classes."
                 ),
                 
                
                 ####################################################################################
                 # New box
                 box(
                  title= "How to use the tool?", status = "success", solidHeader= TRUE,
                  "To use this tool, go through all the steps in the left panel, in order", 
                  br(),
                  tags$ol(
                    tags$li("Select your inputs"), 
                    tags$li("Verify that all classes with their areas are present"), 
                    tags$li("Filter the input and calculate the results")
                  )
                ),
                
                ####################################################################################
                # Change style of the CSS style of the tabBox, making the color green
                tags$style("
                           .nav-tabs-custom .nav-tabs li.active {
                           border-top-color: #00994d;
                           }"),
                
                ####################################################################################
                # New tabBox
                tabBox(
                  ####################################################################################
                  # New tabPanel
                  tabPanel("Disclaimer",
                            br(),
                           "FAO declines all responsibility for errors or deficiencies in the database or software or 
                            in the documentation accompanying it, for program maintenance and upgrading as well as for any 
                           damage that may arise from them. FAO also declines any responsibility for updating the data and 
                           assumes no responsibility for errors and omissions in the data provided. 
                           Users are, however, kindly asked to report any errors or deficiencies in this product to FAO.",
                           br(),
                           br(),
                           img(src="sepal-logo-EN-white.jpg", height = 100, width = 210),
                           img(src="UNREDD_LOGO_COLOUR.jpg", height = 80, width = 100),
                           img(src="Open-foris-Logo160.jpg", height = 70, width = 70),
                           br()
                  ), 
                  
                  ####################################################################################
                  # New tabPanel
                  tabPanel("References and Documents",
                           br(),
                           img(src="GFOI_MG_cover.PNG", height = 250, width = 200),
                           a(href="http://www.gfoi.org/wp-content/uploads/2015/04/GFOIMGD_English.pdf"," GFOI MGD Section 3.7 and Module 2.7",target="_blank"),
                           br(),
                           img(src="AA_cover.PNG", height = 250, width = 200),
                           a(href="https://dl.dropboxusercontent.com/u/11506740/AccuracyAssessment%20Final%20NFMA%2046%20A4.pdf"," FAO NFMA paper N46: Map accuracy assessment and area estimation",target="_blank"),
                           br(),
                           img(src="Olofsson2014_cover.PNG", height = 150, width = 200),
                           a(href="http://reddcommunity.org/sites/default/files/field/publications/Olofsson_et_al_2014_RSE.pdf"," Olofsson et al. (2014): Good practices for estimating area and assessing accuracy of land change",target="_blank")                          
                           
                  )
                )
              )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Input',
              fluidRow(
                ############################################################################## ######
                # New box
                ####################################################################################
                # New box
                box(title= "Select folder where the files are", status = "success", solidHeader= TRUE,
                    "Products from the AA design are stored there:
                    areas of the map, sampling sizes, point file. 
                    It should be the same as output folder from AA_design",
                    br(),
                    shinyDirButton('outdir', 'Select folder with collected points and area file', 'Please select a folder', FALSE),
                    textOutput("outdirpath")
                ),
                
                box(title= "Required input", status = "success", solidHeader= TRUE, width= 4,
                    "Select the validation file exported from CollectEarth. 
                    
                    The area file should contain the map areas and the corresponding map class.
                    The area file can be generated in the Accuracy Assessment Design application.
                    The validation file must contain a column with the classified reference data and a column with the original map data. 
                    The reference data will be compared with the map data at the same location.", 
                    
                    br(),
                    uiOutput('uice_filename'),
                    uiOutput('uiarea_filename'),
                    uiOutput("column_ref"),
                    uiOutput("column_map"),
                   
                    uiOutput("Xcrd"),
                    uiOutput("Ycrd")
              ),
              # New box
              box(title= "Display data", status = "success", solidHeader= TRUE, width= 8,
                  "View the validation on the fly. Select the columns of the validation to view.",
                  uiOutput('select_vars'),
                  dataTableOutput('inputTable')
              )
              )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Check',
              h4("Check inputs"),
              fluidRow(
                ####################################################################################
                # New box
                box(
                  title= "What to check", status = "success", solidHeader= TRUE,
                  "Let us check something",
                  htmlOutput("display_check_line"),
                  htmlOutput("display_check_cols"),
                  tableOutput("table_check"),
                  h4("Location of the points collected"),
                  leafletOutput("map_check")
                )
                )
      ),
      
      ####################################################################################
      # New Tab
      tabItem(tabName = 'Results',
                fluidRow(
                  
                  box(h4("Confusion Matrix"),
                      tableOutput("matrix_all"),
                      downloadButton('download_matrix', 'Download as CSV')
                  ),
                  
                  box(h4("Graph"),
                      plotOutput("histogram_all")
                  ),
                  
                  box(h4("Adjusted areas and accuracies"),
                      tableOutput("accuracy_all"),
                      downloadButton('download_accuracy', 'Download as CSV')
                  ),
                  
                  # New box
                  box(
                    title= "Filter the data", status = "success", solidHeader= TRUE,
                    "Something about filtering",
                    checkboxInput("filter_presence", label="Do you want to filter the data?"),
                    htmlOutput("column_to_filter"),
                    htmlOutput("value_to_filter"),
                    dataTableOutput('filteredDataTable'),
                    dataTableOutput("filtered_data")
                  )
                )
        ) 
      
      ####################################################################################
      # End of the tabItem list
      )
    
    ####################################################################################
    # End of the Dashboard Body
    )
    
####################################################################################
# End of the Graphic User Interface
)


server <- function(input, output,session) {    
####################################################################################
####### Step 1 : compute areas of each strata of the map ###########################
####################################################################################

  
  ##################################################################################################################################    
  ############### HARDCODED ROOT FOLDER : everything will be lower
  volumes <- c('User directory'=Sys.getenv("HOME"))
  
  ##################################################################################################################################    
  ############### Select output directory
  shinyDirChoose(input,
                 'outdir',
                 updateFreq = 1000,
                 session=session,
                 roots=volumes,
                 restrictions=system.file(package='base')
  )
  
  ################################# Output directory path
  outdir <- reactive({
    req(input$outdir)
    dirpath <- parseDirPath(volumes, input$outdir)
    if(is.null(dirpath)){
      cat(as.character("No directory selected"))
    }else{
        cat(dirpath)}
    gsub(" ","",dirpath)
  }) 
  
  ################################# Display output directory path
  output$outdirpath = renderPrint({
    outdir()
  })
  
  ################################# Select input files (area and point file of validation)
  output$uice_filename <-renderUI({
    req(input$outdir)
    mydir <- outdir()
    print("nom de nom")
    print(paste0(outdir(),"/"))
    selectInput('CEfilename',   
                label= 'Validation file (.csv)', 
                list.files(path=paste0(outdir(),"/"),recursive=FALSE,pattern = "\\.csv$"),
                selected = "collectedData_mockup_results")
    })
  

  ################################# Select input files (area and point file of validation)
  output$uiarea_filename <-renderUI({
    req(input$outdir)
    selectInput('areafilename',
              label= 'Area file (.csv)',
              list.files(path=paste0(outdir(),"/"),pattern = "\\.csv$"),
              selected = "area")
    })
  
  ## Map area CSV
    areas_i   <- reactive({
      req(input$areafilename)
      print("read data of area")
      ############### Read the name chosen from dropdown menu
      areas_i <- read.csv(paste(outdir(),"/",input$areafilename,sep="")) 
      })
  
    ## Collect earth output file
    df_i  <- reactive({
      req(input$CEfilename)
      print("read data of validation")
      ############### Read the name chosen from dropdown menu
      cefile <-input$CEfilename
      ############### Load the raster corresponding to the selected name
      datafolder <- paste0(outdir(),"/",cefile)
      df_i <- read.csv(gsub(" ","",datafolder))
      df_i
      })
    
    ## select column with reference data
    output$column_ref <- renderUI({
      selectInput('reference_data', 
                  'Choose the column with the reference data information', 
                  choices= names(df_i()),
                  multiple = FALSE,
                  selected = "ref_class")
    })
    
    ## select column with map data
    output$column_map <- renderUI({
      
        selectInput('map_data', 
                    'Choose the column with the map data information', 
                    choices= names(df_i()),
                    multiple = FALSE,
                    selected = "map_class")
      
    })
    
    ## select the column with the X coordinates
    output$Xcrd <- renderUI({
      
        selectInput('selectX', 
                    'Choose the column with the X coordinate', 
                    choices= names(df_i()),
                    multiple = FALSE,
                    selected = "location_x")
      
    })
    
    ## select the column with the Y coordinates
    output$Ycrd <- renderUI({
      
        selectInput('selectY', 
                    'Choose the column with the Y coordinate', 
                    choices= names(df_i()),
                    multiple = FALSE,
                    selected = "location_y")
      
    })

    ## columns in data table to display
    output$select_vars <- renderUI({
      selectInput('show_vars', 
                         'Columns to show:', 
                  choices= names(df_i()),
                  multiple = TRUE)
    })
    
    ## display the collect earth output file as a Data Table
    output$inputTable <- renderDataTable({
      req(input$show_vars)
      df_i <- df_i()
      df_i[, input$show_vars, drop = FALSE]
    })
    
   
    
    ## if a custom map is uploaded to compare with the reference data, create a new map data column 
    df_i_map <- reactive({
      req(input$CEfilename)
      df_i <- df_i()
      df_i_map <- as.data.frame(df_i)
      
    })
    
  ##################################################################################################################################
  ############### Filtering the data

  # Read the names of df_i() as choices for which column to filter
  output$column_to_filter <- renderUI({
    req(input$filter_presence)
    if(input$filter_presence==T){
     selectInput('input_column_to_filter', 
                 'Columns to filter:', 
                  choices= names(df_i()),
                  multiple = FALSE,
                  selected = "confidence")
    }
    })
  

  # Get the value of the filter
  output$value_to_filter <- renderUI({
    
    req(input$filter_presence)
    req(input$input_column_to_filter)
    
    if(input$filter_presence==T){
    filterColumnList <- input$input_column_to_filter
    
    filterColumnList <- (eval(parse(text = "filterColumnList")))
    
    df_i <- df_i()
    
    selectInput("input_value_to_filter", 
                    sprintf("Values  to filter from column:  %s", as.character(filterColumnList)),
                    choices= unique(df_i[,filterColumnList]),
                    multiple = TRUE
                )
    }
  })

  ##################################################################################################################################
  ############### Filtered data
  df_f  <- reactive({
      df_i_map <- df_i_map()
      
      filterColumnList <- input$input_column_to_filter
      filterColumnList <- (eval(parse(text = "filterColumnList")))
      
      filterColumnValue <- input$input_value_to_filter
            
      print("test filter")
      print(filterColumnValue)
      
      df_f <- df_i_map[df_i_map[,filterColumnList] %in% filterColumnValue,]
      
      head(df_f)
      df_f
      })
  
  
  ##################################################################################################################################
  ############### Legend used for the matrices
  legend_i  <- reactive({
    ############### Use multicore clusters   
    #beginCluster()
    print("Legend")
    df_i_map <- df_i_map()
    map_code <- input$map_data
    
    if(!is.null(input$map_data)){legend_i <- levels(as.factor(df_i_map[,map_code]))}
    if(!is.null(input$map)){
      lcmap
    }
    legend_i 
  })
  
  ##################################################################################################################################
  ############### Interactive selection of attributes for pivot check : lines
  output$display_check_line <- renderUI({
    df <- df_i_map()
    categories <- names(df)
    selectInput("check_inline",
                label = h5(paste("Lines of the pivot table")),
                choices = categories,
                selected = input$map_data
    )
  })
  
  ##################################################################################################################################
  ############### Interactive selection of attributes for pivot check : columns
  output$display_check_cols <- renderUI({
    df <- df_i_map()
    categories <- names(df)
    selectInput("check_incols",
                label = h5(paste("Columns of the pivot table")),
                choices = categories, 
                selected = input$reference_data
    )
  })

  ################################################    
  ################ Create a pivot check table
  ################################################
  output$table_check <- renderTable({
    lns   <- as.character(input$check_inline)
    clmns <- as.character(input$check_incols)
    df_i_map <- as.data.frame(df_i_map())
    table(df_i_map[,lns])
  }
  ,include.rownames = T,include.colnames = T)
  
  
  ################################################    
  ################ Display all the points
  ################################################
  
  output$map_check <-   renderLeaflet({
    df_i_map<- df_i_map()
    map_code <- input$map_data
    dfa <- SpatialPointsDataFrame(
      coords=data.frame(df_i_map[,c(3,4)]),
      data=data.frame(df_i_map[,map_code]),
      proj4string=CRS("+proj=longlat +datum=WGS84"),
      match.ID=F)
    names(dfa)<-"map_code"
    factpal <- colorFactor("Spectral", dfa$map_code)
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(data = dfa, color= ~factpal(map_code),
                       fillOpacity = 1,
                       radius = 1,
                       popup = ~paste(sprintf("Map code: %s", map_code))
      )
    m
  })
  
  
  ################################################    
  ################ Matrix for all classes
  ################################################
  matrix_all <- reactive({
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    
    # df <- read.csv("../../../../../aa_input/collectedData_mockup_aa_CE_2016-10-14_2016-10-14.csv")
    # areas <- read.csv("../../../../../aa_input/area_shp.csv")
    # ref_code <- "ref_class"
    # map_code <- "map_class"
    # legend <- levels(as.factor(df[,map_code]))
    
    areas <- areas_i()
    legend <- legend_i()
    ref_code <- input$reference_data
    map_code <- input$map_data
    
    
    print("test matrix")
    tmp <- as.matrix(table(df[,map_code,],df[,ref_code]))
  
    # tmp <- tapply(df$area,df[,c(map_code,ref_code)],sum)
    # tmp[is.na(tmp)]<- 0
    
    matrix<-matrix(0,nrow=length(legend),ncol=length(legend))
    
    for(i in 1:length(legend)){
      tryCatch({
        cat(paste(legend[i],"\n"))
        matrix[,i]<-tmp[,legend[i]]
      }, error=function(e){cat("Not relevant\n")}
      )
    }
    
    matrix
  })
  
  ################################################    
  ################ Table of accuracies
  ################################################
  
  accuracy_all <- reactive({
    matrix <- matrix_all()
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    areas <- areas_i()
    legend <- legend_i()
    
    matrix_w<-matrix
    for(i in 1:length(legend)){
      for(j in 1:length(legend)){
        tryCatch({
          matrix_w[i,j] <- matrix[i,j]/sum(matrix[i,])*areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)
        }, error=function(e){cat("Not relevant\n")}
        )
      }}
    
    matrix_se<-matrix
    for(i in 1:length(legend)){
      for(j in 1:length(legend)){
        tryCatch({
          matrix_se[i,j] <- areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)*
            areas[areas$map_class==legend[i],]$map_area/sum(areas$map_area)*
            matrix[i,j]/
            sum(matrix[i,])*
            (1-matrix[i,j]/sum(matrix[i,]))/
            (sum(matrix[i,])-1)
        }, error=function(e){cat("Not relevant\n")
          print(legend[i])}
        )
      }
    }
    
    confusion<-data.frame(matrix(nrow=length(legend)+1,ncol=9))
    names(confusion)<-c("class","code","Pa","PaW","Ua","area","area_adj","se","ci")
    
    ### Integration of all elements into one dataframe
    for(i in 1:length(legend)){
      confusion[i,]$class<-areas[areas$map_class==legend[i],]$map_class
      confusion[i,]$code<-areas[areas$map_class==legend[i],]$map_class
      confusion[i,]$Pa<-matrix[i,i]/sum(matrix[,i])
      confusion[i,]$Ua<-matrix[i,i]/sum(matrix[i,])
      confusion[i,]$PaW<-matrix_w[i,i]/sum(matrix_w[,i])
      confusion[i,]$area_adj<-sum(matrix_w[,i])*sum(areas$map_area)
      confusion[i,]$area<-areas[areas$map_class==legend[i],]$map_area
      confusion[i,]$se<-sqrt(sum(matrix_se[,i]))*sum(areas$map_area)
      confusion[i,]$ci<-confusion[i,]$se*1.96
    }
    
    ### Compute overall accuracy
    confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$map_area),sum(areas$map_area),"","")
    confusion
  })
  
  # ################################################    
  # ################ Output : Summary of accuracies 
  # ################################################
       
  output$accuracy_all <- renderTable({
    item<-data.frame(accuracy_all())
    item<-item[,c("class","PaW","Ua","area","area_adj","ci")]
    item$PaW<-floor(as.numeric(item$PaW)*100)
    item$Ua<-floor(as.numeric(item$Ua)*100)
    item$area<-floor(as.numeric(item$area))
    item$area_adj<-floor(as.numeric(item$area_adj))
    item$ci<-floor(as.numeric(item$ci))
    names(item) <-c("Class","PA","UA","Map areas","Bias corrected areas","CI")
    item
  },include.rownames=FALSE,digits=0)
     
  # #################################################    
  # ################ Output item  :  confusion matrix
  # #################################################
  
  output$matrix_all <- renderTable({
    if(input$filter_presence==T){    
      df <- df_f()
    }else{df <- df_i_map()}
    areas <- areas_i()
    legend <- legend_i()
    
    item<-as.matrix(matrix_all())
    dimnames(item) <- list(legend,legend)
    #dimnames(item) <- list(areas$class[areas$code %in% as.numeric(legend)],areas$class[areas$code %in% as.numeric(legend)])
    item                                  
  },digits=0)
  
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  ## maybe include if statement 
  ## if the difference between some of the categories is larger than some multiple, 
  ##  then split the large and small categories into two graphs
  
  ## could also include user specified axis and title labels
  output$histogram_all <- renderPlot({
    dfa<-as.data.frame(accuracy_all())
    legend <- legend_i()
    
    dfa<-dfa[c(1:length(legend)),]
    dfa[dfa=="NaN"]<-0
    dfa$ci<-as.numeric(dfa$ci)
    dfa$area_adj<-as.numeric(dfa$area_adj)
    
    avg.plot<-ggplot(data=dfa,
                     aes(x=class,y=area_adj))
    
    avg.plot+geom_bar(stat="identity",fill="darkgrey")+geom_errorbar(aes(ymax=area_adj+ci, ymin=area_adj-ci))+theme_bw()
  })
  
  
  # #################################################    
  # ################ Output confusion matrix
  # #################################################
  
  output$download_matrix <- downloadHandler(
    filename = function() { 
      paste('matrix_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      legend <- legend_i()
      item<-as.matrix(matrix_all())
      dimnames(item) <- list(legend,legend)
      write.csv(item,file)
    })
  
  # #################################################    
  # ################ Output histograms adjusted areas
  # #################################################
  
  output$download_accuracy <- downloadHandler(
    filename = function() { 
      paste('accuracy_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(accuracy_all(),file,row.names = F)
    })
  
  # #################################################    
  # ################ Output the validation file
  # #################################################
  
  output$download_input <- downloadHandler(
    filename = function() { 
      paste('input_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(df_i_map(),file,row.names = F)
    })
  
  # #################################################    
  # ################ Output the area file
  # #################################################
  
  output$download_area <- downloadHandler(
    filename = function() { 
      paste('area_', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(areas_i(),file,row.names = F)
    })
  
  
  ################## Stop the shiny server
  ####################################################################################
  
  }

shinyApp(ui, server)