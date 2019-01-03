# app.R - Shiny application of the program "POTW_compliance_v203"
#         WQ compliance assessment using 4 parameters: DO, pH, Transm and CHL
#         Last change: Instead of tab "Transmissivity" tab "t-test" with a choice of four "Outrange" parameters
#
library( PBSmapping )
if (packageVersion('DT') < '0.1.3') devtools::install_github('rstudio/DT')
library( DT )
library( oce )
library( pracma )
library( plyr )
library( signal )
library( stats )
library( shinyjs )
library(leaflet)
#library(sp)

library(sf)  # Changed by Marcus

source( "POTW_compliance_functions.R")
#
# Read geographic coordinates of the stations from the file "Stn_coords.csv", 
#     located in the subdirectory "data",
#     which must include the columns: Station, Latitude, Longitude, Depth, Agency
Stn_coords_fn <- file.path( getwd(), "Stn_coords.csv" )
Stn_coords <- read.csv( Stn_coords_fn, stringsAsFactors = FALSE )
Stn_coords <- Stn_coords[ , c("Station","Agency","Latitude","Longitude","Depth") ]
# # Load SCB coastline
# load( file.path( getwd(), "SCB.RData" ) )
# Read the shapefile with pipes

Pipes.shp.dir <- file.path( getwd(), "Pipes_shp" )   # Changed by Marcus
Pipes.shp <- st_read(paste0(Pipes.shp.dir, "/SCB_pipes.shp"))    # Changed by Marcus

st_crs( Pipes.shp ) <- "+proj=longlat" 
# Read the list of parameters and the names of parameters in the file
Param.names.fn <- file.path( getwd(), "Param_names.csv" )
Param.names <- read.csv( Param.names.fn, stringsAsFactors = FALSE )
# Read the "Plume settings" file
Plume.settings.fn <- file.path( getwd(), "CDOM_plume_settings.csv" )
Plume.settings.R <- read.csv( Plume.settings.fn, stringsAsFactors = FALSE )
#
Outr.param.settings.fn <- file.path( getwd(), "Outrange_params.csv" )
Outr.param.settings <- read.csv( Outr.param.settings.fn, stringsAsFactors = FALSE )
# Settings
settings <- settings.list.create( Plume.settings.R, Outr.param.settings )
#
# Get the list of data files 
#   (Starting "Central Bight Master Database") and ending "*.csv" )
data_files.list <- list.files( path = file.path( getwd(), "data" ), 
                               pattern = "Central Bight Master Database" )
indx.csv <- grep( "*.csv", data_files.list, ignore.case = TRUE )
data_files.list <- data_files.list[ indx.csv ]
#browser()
str_hr <- "--------------------------------------------------------\n"
#
CTD.param.list <- c( "TdegC","Spsu","Sigma","CDOM","CHL","DO","pH","Transm" )
#
#
# User interface - START *********************************************************
ui <- fluidPage( useShinyjs(),
                 title = "POTW compliance assessment - v.2.03",
                 #
                 titlePanel( h6( "POTW compliance assessment - v.2.03" ) ),
                 #
                 #    fluidRow( wellPanel( h5( htmlOutput( "data_selected_text" ) ) ) ),
                 fluidRow( h5( verbatimTextOutput( "summary_text" ) )
                           #h5( htmlOutput( "data_selected_text" ) )
                 ),
                 fluidRow( strong( verbatimTextOutput( "warning_message1" ) ) ),
                 #   #
                 tabsetPanel(
                   tabPanel( h5( strong( "Map" ) ),
                             column( width = 7,
                                     leafletOutput( "leaflet_map", width = "100%", height = "700px" ) ),
                             column( width = 5,
                                     DT::dataTableOutput( "map_stn_DT" ) )
                   ),
                   tabPanel( h5( strong( "Data" ) ),
                             fluidRow( 
                               column( width = 8,  uiOutput( "select_data_file_UI" ) ),
                               column( width = 4, br(), br(),
                                       fluidRow( actionButton( "read_data_file_AB", h5( strong( "  Read data file " ) ) ) )
                               )
                             ),
                             wellPanel( 
                               fluidRow(
                                 column( width = 3, uiOutput( "select_Agency_UI" ) ),
                                 column( width = 3, uiOutput( "select_Year_UI" ) ),
                                 column( width = 3, uiOutput( "select_Season_UI" ) ),
                                 column( width = 3, br(),
                                         hidden( actionButton( "select_data_AB", h5( strong( "  Select data  " ) ) ) )
                                 )
                               ),
                               column( width = 6, hidden( DT::dataTableOutput( "data_selected_DT" ) ) )
                             )
                   ),
                   tabPanel( h5( strong( "Profiles" ) ),
                             wellPanel( fluidRow( 
                               column( width = 3, uiOutput( "select_Parameter2View_UI" ) ),
                               column( width = 3, hidden( 
                                 selectInput( "profile_vert_axis_SI", label = "Vertical axis",
                                              choices = c( "Depth", "Density" ) ) ) ),
                               column( width = 3, br(), uiOutput( "show_ref_prof_UI" ) )
                             ) ),
                             fluidRow( 
                               tags$head(tags$style(HTML("#Profiles_list_DT tr.selected {background-color:#FF00FF}"))),
                               column( width = 6, 
                                       # Former "profiles_Z" object...
                                       plotOutput( "profiles_plot", click = "profiles_click",
                                                   dblclick = "profiles_dblclick",
                                                   brush = brushOpts( id = "profiles_brush", 
                                                                      resetOnNew = TRUE )
                                       )
                               ),
                               column( width = 4, hidden( DT::dataTableOutput( "Profiles_list_DT" ) ) )
                             )
                   ),
                   tabPanel( h5( strong( "CDOM plume" ) ), br(),
                             fluidRow( 
                               column( width = 4, 
                                       hidden( actionButton( "Detect_plume_AB", strong("Detect CDOM plume") ) ) ),
                               column( width = 4, 
                                       hidden( actionButton( "Edit_plume_list_AB", strong("Edit plume list") ) ) )
                             ), hr(),
                             fluidRow(
                               column( width = 6,
                                       h5( strong( "Plume detection settings:" ) ),
                                       fluidRow( 
                                         column( width = 2, br(), 
                                                 actionButton( "plume_settings_edit_AB", label = strong( "Edit" ) ), br() ),
                                         column( width = 4, uiOutput( "plume_setting_edit_UI" ) ),
                                         column( width = 2, br(),
                                                 hidden( actionButton( "plume_settings_accept_AB", label = strong( "Accept" ) ) ) ),
                                         column( width = 2, br(),
                                                 hidden( actionButton( "plume_settings_cancel_AB", label = strong( "Cancel" ) ) ) )
                                       ),
                                       fluidRow( 
                                         wellPanel( DT::dataTableOutput( "plume_settings_DT" ) )
                                       )
                               ),
                               column( width = 6,
                                       uiOutput( "plume_list_edit_UI" ),
                                       column( width = 5, 
                                               hidden( actionButton( "plume_list_accept_AB", label = strong( "Accept" ) ) ) ),
                                       column( width = 5, 
                                               hidden( actionButton( "plume_list_cancel_AB", label = strong( "Cancel" ) ) ) )
                               )
                             )
                   ),
                   tabPanel( h5( strong( "Reference stations" ) ), br(),
                             fluidRow( 
                               column( width = 4, 
                                       hidden( actionButton( "Select_ref_AB", strong("Auto select") ) ) ),
                               column( width = 4, 
                                       hidden( actionButton( "Edit_ref_list_AB", strong("Edit reference list") ) ) )
                             ), hr(),
                             fluidRow(
                               column( width = 6, h5( strong( "Automated selection settings:" ) ),
                                       fluidRow( 
                                         column( width = 2, br(), 
                                                 actionButton( "ref_settings_edit_AB", label = strong( "Edit" ) ), br() ),
                                         column( width = 4, uiOutput( "ref_setting_edit_UI" ) ),
                                         column( width = 2, br(),
                                                 hidden( actionButton( "ref_settings_accept_AB", label = strong( "Accept" ) ) ) ),
                                         column( width = 2, br(),
                                                 hidden( actionButton( "ref_settings_cancel_AB", label = strong( "Cancel" ) ) ) )
                                       ),
                                       fluidRow( 
                                         wellPanel( DT::dataTableOutput( "ref_settings_DT" ) )
                                       )
                               ),
                               column( width = 6,
                                       uiOutput( "ref_list_edit_UI" ),
                                       column( width = 5, 
                                               hidden( actionButton( "ref_list_accept_AB", label = strong( "Accept" ) ) ) ),
                                       column( width = 5, 
                                               hidden( actionButton( "ref_list_cancel_AB", label = strong( "Cancel" ) ) ) )
                               )
                             )
                   ),
                   tabPanel( h5( strong( "Outranges" ) ), br(),
                             fluidRow( 
                               column( width = 2, 
                                       uiOutput( "outrange_param_UI" )
                               ),
                               column( width = 2, br(),
                                       actionButton( "outrange_param_select_AB", 
                                                     label = strong( "Select" ) )
                               ),
                               column( width = 3, br(),
                                       hidden( actionButton( "calc_ref_profile_AB", strong( "Calculate profile" ) ) )
                               ),
                               column( width = 2, br(),
                                       hidden( actionButton( "detect_outranges_AB", strong( "Detect outranges" ) ) )
                               )
                             ),
                             fluidRow(
                               column( width = 6, h5( strong( "Outranges detection settings:" ) ),
                                       fluidRow( 
                                         column( width = 2, br(), 
                                                 actionButton( "outr_settings_edit_AB", label = strong( "Edit" ) ), br() ),
                                         column( width = 4, uiOutput( "outr_setting_edit_UI" ) ),
                                         column( width = 2, br(),
                                                 hidden( actionButton( "outr_settings_accept_AB", label = strong( "Accept" ) ) ) ),
                                         column( width = 2, br(),
                                                 hidden( actionButton( "outr_settings_cancel_AB", label = strong( "Cancel" ) ) ) )
                                       ),
                                       fluidRow( wellPanel( 
                                         DT::dataTableOutput( "outr_settings_DT" ),
                                         hr(),
                                         uiOutput( "prof_comp_method_UI" ),
                                         hr(),
                                         uiOutput( "entr_setting_UI" )
                                       ) )
                               ),
                               column( width = 6, 
                                       fluidRow( 
                                         hr(),
                                         wellPanel( 
                                           h5( strong( textOutput( "v_z_min_text" ) ) ),
                                           hidden( DT::dataTableOutput( "v_z_min_DT" ) ), 
                                           #hr(), br(),
                                           column( width = 5, plotOutput( "outr_prof_Z" ) ),
                                           column( width = 5, plotOutput( "outr_prof_Sigma" ) )
                                         )
                                       )                
                               )
                             ) # fluidRow(
                   ),  # tabPanel( h5( strong( "Outranges" ) )
                   tabPanel( h5( strong( "Entrainment" ) ), br(),
                             fluidRow( 
                               column( width = 4, 
                                       h5( strong( textOutput( "v_z_min_entr_text" ) ) ),
                                       DT::dataTableOutput( "v_z_min_entr_DT" )
                               ), 
                               column( width = 3, plotOutput( "entr_prof_Z" ) ),
                               column( width = 3, plotOutput( "entr_prof_Sigma" ) )
                             )
                   ), # tabPanel( h5( strong( "Entrainment" ) ), br()
                   tabPanel( h5( strong( "t-test" ) ), br(),
                             fluidRow(
                               #        column( width = 2, h5( strong( "Plume vs. reference:" ) ) ),
                               column( width = 2, uiOutput( "select_param_ttest_UI" ) ),
                               column( width = 2, selectInput( "ttest_alternative_SI", label = h5( strong( "t-test alternative" ) ),
                                                               choices = c( "two.sided", "less", "greater" ) ) ),
                               column( width = 4, uiOutput( "depth_range_SI" ) ),
                               #        column( width = 2 ),
                               column( width = 2, actionButton( "calc_ttest_AB", label = h5(strong( "Compare plume to reference" ) ) ) )
                             ),
                             # fluidRow( 
                             #   column( width = 1 ),
                             #   column( width = 4, uiOutput( "depth_range_SI" ) )
                             # ),
                             fluidRow(
                               column( width = 6, plotOutput( "transm_plume_ref_plot" ) ),
                               column( width = 5, verbatimTextOutput( "transm_plume_ref_stats" ) )
                             )
                   ),
                   tabPanel( h5( strong( "Report" ) ), br(),
                             fluidRow( 
                               downloadButton( "download_report", label = strong( "Download report" ) ),
                               h5( verbatimTextOutput( "report_text" ) )
                             )
                   )
                 )
)
# User interface - END ********************************************************
# 
#
#
# Server - START **************************************************************
server <- function( input, output ) {
  #
  # Reactive values -------------------------------------------------------
  v <- reactiveValues( Stn.list.surv = Stn_coords,
                       selected.data.file = NULL,
                       Surv.data.tot = NULL,
                       Agency.list = NULL, Agency.selected = NULL,
                       Year.list = NULL, Year.selected = NULL,
                       Season.list = NULL, Season.selected = NULL,
                       Surv.data = NULL, Valid.data = NULL,
                       StnID.clicked = NULL,
                       settings = settings,
                       Outrange.Param = NULL,
                       ttest.Param = NULL,
                       RefProf = NULL,
                       V.diff.list = NULL,
                       V.Z.min = NULL
  )
  #
  # Selected values -------------------------------------------------------
  output$summary_text <- renderText({
    if( is.null( v$selected.data.file ) ) {
      report_text <- "Nothing selected"
    } else {
      report_data_file <- report.data.file(  v$selected.data.file, v$Surv.data.tot )
      report_data_selected <- report.data.selected( 
        v$Agency.selected, v$Year.selected, v$Season.selected, v$Surv.data )
      report_plume_list <- report.plume.list( v$Stn.list.surv$Plume, v$Stn.list.surv$Profile )
      report_ref_list <- report.ref.list( v$Stn.list.surv$Ref, v$Stn.list.surv$Profile )
      report_outr_param <- report.outr.param( v$Outrange.Param )
      report_ref_prof <- report.ref.prof( v$Outrange.Param, v$RefProf )
      report_outr_list <- report.outr.list( v$Outrange.Param, v$V.diff.list, v$Stn.list.surv )
      #
      summary_text <- paste( report_data_file, 
                             report_data_selected, 
                             report_plume_list,
                             report_ref_list,
                             report_outr_param,
                             report_ref_prof,
                             report_outr_list,
                             sep = "\n" )
      return( summary_text )
    }
  })
  #
  # Map functions ------------------------------------------------------
  output$leaflet_map <- renderLeaflet({
    leaflet_map <- leaflet( v$Stn.list.surv ) 
    leaflet_map <- addProviderTiles( leaflet_map, "Esri.OceanBasemap",
                                     options = providerTileOptions(noWrap = TRUE) )
    leaflet_map <- fitBounds( leaflet_map, ~min(Longitude), ~min(Latitude),
                              ~max(Longitude), ~max(Latitude) )
    leaflet_map <- addPolygons( leaflet_map, data = Pipes.shp, color = "#444444", fillOpacity = 0, weight = 2, dashArray = "5,5" )
    popup <- paste( sep = "<br/>", 
                    paste( v$Stn.list.surv$Agency ), 
                    paste( "StnID =", v$Stn.list.surv$Station ),
                    paste( "Depth =", v$Stn.list.surv$Depth )
    )
    popup[ v$Stn.list.surv$Plume ] <- paste( sep = "<br/>", popup[ v$Stn.list.surv$Plume ], "Plume" )
    popup[ v$Stn.list.surv$Ref ] <- paste( sep = "<br/>", popup[ v$Stn.list.surv$Ref ], "Reference" )
    popup[ v$Stn.list.surv$Outrange ] <- paste( sep = "<br/>", popup[ v$Stn.list.surv$Outrange ], "Outrange" )
    cex.pt <- rep( 5, nrow( v$Stn.list.surv ) )
    cex.pt[ v$Stn.list.surv$Plume ] <- 10
    cex.pt[ v$Stn.list.surv$Ref ] <- 10
    cex.pt[ v$Stn.list.surv$Outrange ] <- 20
    col.pt <- rep( "#444", nrow( v$Stn.list.surv ) )
    fill.col <- rep( "#AAA", nrow( v$Stn.list.surv ) )
    col.pt[ v$Stn.list.surv$Plume ] <- "#F00"
    fill.col[ v$Stn.list.surv$Plume ] <- "#F00"
    col.pt[ v$Stn.list.surv$Ref ] <- "#03F"
    fill.col[ v$Stn.list.surv$Ref ] <- "#03F"
    fill.col[ v$Stn.list.surv$Outrange ] <- "#F80"
    leaflet_map <- addCircleMarkers( leaflet_map, lng = ~Longitude, lat = ~Latitude, 
                                     radius = cex.pt, weight = 1, opacity = 0.9, col = col.pt, 
                                     fillColor = fill.col,
                                     popup = popup )
  })
  #
  observe({
    #
    leaflet_proxy <- leafletProxy( "leaflet_map", data = v$Stn.list.surv )
    leaflet_proxy <- clearMarkers( leaflet_proxy )
    leaflet_proxy <- addPolygons( leaflet_proxy, data = Pipes.shp, color = "#444444", fillOpacity = 0, weight = 2, dashArray = "5,5" )
    popup <- paste( sep = "<br/>", 
                    paste( v$Stn.list.surv$Agency ), 
                    paste( "StnID =", v$Stn.list.surv$Station ),
                    paste( "Depth =", v$Stn.list.surv$Depth )
    )
    popup[ v$Stn.list.surv$Plume ] <- paste( sep = "<br/>", popup[ v$Stn.list.surv$Plume ], "Plume" )
    popup[ v$Stn.list.surv$Ref ] <- paste( sep = "<br/>", popup[ v$Stn.list.surv$Ref ], "Reference" )
    popup[ v$Stn.list.surv$Outrange ] <- paste( sep = "<br/>", popup[ v$Stn.list.surv$Outrange ], "Outrange" )
    cex.pt <- rep( 5, nrow( v$Stn.list.surv ) )
    cex.pt[ v$Stn.list.surv$Plume ] <- 10
    cex.pt[ v$Stn.list.surv$Ref ] <- 10
    cex.pt[ v$Stn.list.surv$Outrange ] <- 20
    col.pt <- rep( "#444", nrow( v$Stn.list.surv ) )
    fill.col <- rep( "#AAA", nrow( v$Stn.list.surv ) )
    col.pt[ v$Stn.list.surv$Plume ] <- "#F00"
    fill.col[ v$Stn.list.surv$Plume ] <- "#F00"
    col.pt[ v$Stn.list.surv$Ref ] <- "#03F"
    fill.col[ v$Stn.list.surv$Ref ] <- "#03F"
    fill.col[ v$Stn.list.surv$Outrange ] <- "#F80"
    leaflet_proxy <- addCircleMarkers( leaflet_proxy, lng = ~Longitude, lat = ~Latitude,
                                       radius = cex.pt, weight = 1, opacity = 0.9, col = col.pt, 
                                       fillColor = fill.col,
                                       popup = popup )    
    return( leaflet_proxy )
  })
  #  
  # Map_old functions ------------------------------------------------------
  stn.map.ranges <- reactiveValues(x = NULL, y = NULL)
  station.clicked <- reactiveValues( Station = NULL )
  #
  stn.map.xvar <- reactive({ "Longitude" })
  stn.map.yvar <- reactive({ "Latitude" })
  # When a click happens, demonstrate the selected station
  observeEvent(input$stn_map_click, {
    station.clicked <- nearPoints( Stn_coords, input$stn_map_click, 
                                   stn.map.xvar(), stn.map.yvar(),
                                   maxpoints = 1 )
    #browser()
    output$stn_selected_DT <- DT::renderDataTable({ 
      DT::datatable( station.clicked, rownames = FALSE, 
                     selection = "none",
                     options = list(dom = 't') ) } )
  })
  #
  output$stn_map <- renderPlot({
    map.lims <- data.frame( x = extendrange( v$Stn.list.surv$Longitude,
                                             f = 0.1 ),
                            y = extendrange( v$Stn.list.surv$Latitude,
                                             f = 0.1 ) )
    #                              c(-119.7,-117), y = c(32.2,34.5) )
    if( !is.null( stn.map.ranges$x ) & !is.null( stn.map.ranges$y ) ) {
      map.lims$x = stn.map.ranges$x
      map.lims$y = stn.map.ranges$y
    }
    SCB_map( SCB_usa, GSHHS_mex, Pipes_PBS, v$Stn.list.surv, map.lims )
  })
  #
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$stn_map_dblclick, {
    brush <- input$stn_map_brush
    if (!is.null(brush)) {
      stn.map.ranges$x <- c(brush$xmin, brush$xmax)
      stn.map.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      stn.map.ranges$x <- NULL
      stn.map.ranges$y <- NULL
    }
  })
  #
  # Data select functions -------------------------------------------------
  #
  output$select_data_file_UI <- renderUI({  
    selectInput( "select_data_file", 
                 label = h5(strong( "Select data file" )), 
                 choices = data_files.list, width = "100%" )
  })
  #
  observeEvent( input$read_data_file_AB, {
    #
    v$Season.selected <- NULL
    v$Surv.data <- NULL
    v$Stn.list.surv <- Stn_coords
    #
    shinyjs::hide( "data_selected_DT" )
    shinyjs::hide( "profile_vert_axis_SI" )
    shinyjs::hide( "Profiles_list_DT" )
    shinyjs::hide( "Detect_plume_AB" )
    shinyjs::hide( "Edit_plume_list_AB" )
    shinyjs::hide( "Select_ref_AB" )
    shinyjs::hide( "Edit_ref_list_AB" )
    shinyjs::hide( "calc_ref_profile_AB" )
    shinyjs::hide( "detect_outranges_AB" )
    shinyjs::hide( "v_z_min_DT" )
    #
    v$Outrange.Param <- NULL
    v$RefProf <- NULL
    v$V.diff.list <- NULL
    v$V.Z.min <- NULL
    #
    v$selected.data.file <- input$select_data_file
    v$Surv.data.tot <<- read.csv(
      file.path( getwd(), "data", input$select_data_file ),
      header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1" )
    #
    param.list <- c( "Agency","Date","Season" )
    param.sel <- calc.param.sel( param.list, Param.names, colnames( v$Surv.data.tot ) )
    if( is.null( param.sel ) ) {
      output$summary_text <- renderText({ "Check file Param_names.csv" })
      v$Surv.data.tot <- NULL
      return( NULL )
    }
    if( any( is.na(  param.sel$k.col ) ) ) {
      output$summary_text <- renderText({
        paste( "Absent in the data file - columns starting from:",
               paste( param.sel$col.name[ is.na(  param.sel$k.col ) ], collapse = "," )
        )
      })
      v$Surv.data.tot <- NULL
      v$Agency.list <- NULL
      v$Season.list <- NULL
      return( NULL )
    }
    v$Agency.list <- unique( v$Surv.data.tot[ , param.sel["Agency","k.col"] ] )
    v$Agency.selected <- NULL
    #
    v$Surv.data.tot$Year <- as.numeric( format( as.Date(
      v$Surv.data.tot[ , param.sel["Date","k.col"] ],
      format = Param.names$Format[ Param.names$Parameter == "Date" ] ), "%Y" ) )
    v$Year.list <- unique( v$Surv.data.tot$Year )
    #browser()
    if( all( is.na( v$Year.list ) ) ) {
      output$summary_text <- renderText({
        paste( Param.names$ParamNameStarts[ param.sel["Date","k.col"] ], "format", 
               Param.names$Format[ param.sel["Date","k.col"] ],
               "is incorrect" )
      })
      v$Year.list <- NULL
      return( NULL )
    }
    v$Year.selected <- NULL
    # 
    v$Season.list <- unique( v$Surv.data.tot[ , param.sel["Season","k.col"] ] )
    #
    if( !is.null( v$Agency.list ) & !is.null( v$Year.list ) &
        !is.null( v$Season.list ) ) {
      shinyjs::show( "select_data_AB" )
    }
    #
  })
  #
  output$select_Agency_UI <- renderUI({  
    if( !is.null( v$Agency.list ) ) {
      selectInput( "select_Agency", label = h5( strong( "Agency") ), 
                   choices = v$Agency.list )
    }
  })
  #
  output$select_Year_UI <- renderUI({  
    if( !is.null( v$Year.list ) ) {
      selectInput( "select_Year", label = h5( strong( "Year") ), 
                   choices = v$Year.list )
    }
  })
  #
  output$select_Season_UI <- renderUI({  
    if( !is.null( v$Season.list ) ) {
      selectInput( "select_Season", label = h5( strong( "Season") ), 
                   choices = v$Season.list )
    }
  })
  #
  observeEvent( input$select_data_AB, {
    #
    v$Outrange.Param <- NULL
    v$RefProf <- NULL
    v$V.diff.list <- NULL
    v$V.Z.min <- NULL
    #
    shinyjs::hide( "Edit_ref_list_AB" )
    shinyjs::hide( "calc_ref_profile_AB" )
    shinyjs::hide( "detect_outranges_AB" )
    shinyjs::hide( "v_z_min_DT" )
    shinyjs::hide( "Detect_plume_AB" )
    shinyjs::hide( "Edit_plume_list_AB" )
    shinyjs::hide( "Select_ref_AB" )
    
    shinyjs::hide( "data_selected_DT" )
    shinyjs::hide( "profile_vert_axis_SI" )
    shinyjs::hide( "Profiles_list_DT" )
    shinyjs::hide( "Detect_plume_AB" )
    shinyjs::hide( "Select_ref_AB" )
    #
    v$Agency.selected <- input$select_Agency
    v$Year.selected <- input$select_Year
    v$Season.selected <- input$select_Season
    #
    param.list <- c( "Agency","Season","StnID","FieldRep","Z", CTD.param.list )
    # "TdegC","Spsu","Sigma","CDOM",
    # "CHL","DO","pH","Transm" )
    param.sel <- calc.param.sel( param.list, Param.names, colnames( v$Surv.data.tot ) )
    #output$warning_message1 <- renderText({ dim( param.sel ) })
    #browser()
    if( is.null( param.sel ) ) {
      #output$warning_message1 <- renderText({ "Check absent params..." })
      output$summary_text <- renderText({ "Check file Param_names.csv" })
      v$Surv.data.tot <- NULL
      return( NULL )
    }
    if( any( is.na(  param.sel[ !( param.list == "FieldRep" ), "k.col" ] ) ) ) {
      output$summary_text <- renderText({
        paste( "Absent in the data file - columns starting from:",
               paste( param.sel$col.name[ is.na(  param.sel$k.col ) ], collapse = "," )
        )
      })
      return( NULL )
    }
    #
    indx.agency <- ( v$Surv.data.tot[ , param.sel["Agency","k.col"] ] == v$Agency.selected )
    indx.year <- ( v$Surv.data.tot$Year == v$Year.selected )
    indx.season <- ( v$Surv.data.tot[ , param.sel["Season","k.col"] ] == v$Season.selected )
    #  
    params.selected <- c("StnID", "Z", CTD.param.list )
    #browser()
    v$Surv.data <- v$Surv.data.tot[ indx.agency & indx.year & indx.season, 
                                    param.sel[ params.selected, "k.col" ] ]
    colnames( v$Surv.data ) <- params.selected
    if( nrow( v$Surv.data ) == 0 ) {
      v$Surv.data <- NULL
      v$Stn.list.surv <- Stn_coords
      return( NULL )
    }
    #
    if( !is.na( param.sel[ "FieldRep", "k.col" ] ) ) {
      v$Surv.data$Profile <- v$Surv.data.tot[ indx.agency & indx.year & indx.season,
                                              param.sel[ "FieldRep", "k.col" ] ]
    } else {
      v$Surv.data$Profile <- 1
    }
    #  Remove data with missed StnID and Z
    indx.na <- with( v$Surv.data, is.na( StnID ) | is.na( Z ) )
    v$Surv.data <- v$Surv.data[ !indx.na, ]
    # Make Profile column
    v$Surv.data$Profile <- paste( v$Surv.data$StnID, v$Surv.data$Profile, sep = "-" )
    # Replace NA codes (like -99) with NA
    NA.code <- Plume.settings.R$Value[ Plume.settings.R$ShortName == "indx_NA" ]
    nr.data <- nrow( v$Surv.data )
    indx.NA <- suppressWarnings( matrix( 
      ( as.numeric( as.matrix(v$Surv.data) ) %in% NA.code), 
      nrow = nr.data ) )
    v$Surv.data[ indx.NA ] <- NA
    # Subset of v$Stn.list.surv
    Stn.FR <- unique( v$Surv.data[ , c("StnID","Profile" ) ] )
    colnames( Stn.FR ) <- c( "Station","Profile" )
    v$Stn.list.surv <- merge( Stn.FR, Stn_coords )
    # Calculate distance from outfall for all stations
    Outfall.station <- settings$Outfall$Station[ 
      which( settings$Outfall$Agency == v$Agency.selected ) ]
    indx.outfl.stn <- which( v$Stn.list.surv$Station == Outfall.station )[1]
    v$Stn.list.surv$Dist.Outfl <- apply( 
      X = with( v$Stn.list.surv, cbind( Longitude, Latitude ) ), 
      MARGIN = 1, FUN = geodetic.distance,
      with( v$Stn.list.surv, 
            c( Longitude[indx.outfl.stn], Latitude[indx.outfl.stn] )
      )
    )
    # Calculate the number of valid data
    v$Valid.data <- data.frame( Parameter = colnames( v$Surv.data ),
                                Samples = NA, Stations = NA, Profiles = NA, 
                                Min = NA, Max = NA )
    v$Valid.data$Samples <- apply( !is.na( v$Surv.data ), 2, sum )
    calc.N.v.stns <- function( Surv.data.column, StnID ) {
      indx.val <- !is.na( Surv.data.column )
      N.v.stns <- length( unique( StnID[ indx.val ] ) )
      return( N.v.stns )
    }
    v$Valid.data$Stations <- apply( v$Surv.data, 2, calc.N.v.stns, 
                                    v$Surv.data$StnID )
    v$Valid.data$Profiles <- apply( v$Surv.data, 2, calc.N.v.stns, 
                                    v$Surv.data$Profile )
    v$Valid.data$Min <- suppressWarnings( apply( v$Surv.data, 2, min, na.rm = TRUE ) )
    v$Valid.data$Max <- suppressWarnings( apply( v$Surv.data, 2, max, na.rm = TRUE ) )
    output$data_selected_DT <- DT::renderDataTable({ 
      DT::datatable( v$Valid.data[ -c(1,2), ], 
                     selection = "none",
                     rownames = FALSE, options = list(dom = 't') ) })
    shinyjs::show( "data_selected_DT" )
    #
    shinyjs::show( "profile_vert_axis_SI" )
    tags$head(tags$style(HTML("#Profiles_list_DT tr.selected {background-color:#FF00FF}")))
    shinyjs::show( "Profiles_list_DT" )
    #
    prof.Z.ranges$x <- NULL
    prof.Z.ranges$y <- NULL
    #
    shinyjs::show( "Detect_plume_AB" )
    shinyjs::show( "Select_ref_AB" )
    #
  })
  #
  # Profiles functions -------------------------------------------------
  #
  output$select_Parameter2View_UI <- renderUI({  
    if( !is.null( v$Surv.data ) ) {
      #browser()
      indx.rem <- ( v$Valid.data$Parameter %in% c("StnID","Profile","Z") )
      selectInput( "select_Parameter2View", label = "Parameter", 
                   choices = as.vector( v$Valid.data$Parameter[ !indx.rem ] ) )
    }
  })
  #
  output$Profiles_list_DT <- DT::renderDataTable( {
    if( !is.null( v$Surv.data ) ) {
      Prof.list <- v$Stn.list.surv[ , c("Profile","Agency","Depth") ]
      # Add columns "Plume","Reference","Outrange"...
      if( "Plume" %in% colnames( v$Stn.list.surv ) ) {
        Prof.list$Plume <- ""
        Prof.list$Plume[ v$Stn.list.surv$Plume ] <- "Plume"
      }
      if( "Ref" %in% colnames( v$Stn.list.surv ) ) {
        Prof.list$Reference <- ""
        Prof.list$Reference[ v$Stn.list.surv$Ref ] <- "Reference"
      }
      if( "Outrange" %in% colnames( v$Stn.list.surv ) ) {
        Prof.list$Outrange <- ""
        Prof.list$Outrange[ v$Stn.list.surv$Outrange ] <- "Outrange"
      }
      Profiles_list_DT <- DT::datatable( Prof.list, rownames = FALSE,
                                         selection = "multiple" )
    } else {
      Profiles_list_DT <- matrix( , nrow = 0, ncol = 0 )
    }
  }, server = FALSE )
  #
  output$show_ref_prof_UI <- renderUI({ 
    if( !is.null( v$RefProf ) & !is.null( v$Outrange.Param ) & 
        !is.null( input$select_Parameter2View ) ) {
      if( input$select_Parameter2View == v$Outrange.Param ) {
        checkboxInput( "show_ref_prof_cbI", 
                       label = strong( span( "Show reference profile", style = "color: green" ) ), 
                       value = FALSE )
      }
    }
  })
  #
  prof.Z.ranges <- reactiveValues( x = NULL, y = NULL)
  #
  prof.Z.xvar <- reactive({ input$select_Parameter2View })
  #prof.Z.yvar <- reactive({ "Z" })
  prof.Z.yvar <- reactive( { switch( input$profile_vert_axis_SI, 
                                     Depth = "Z", Density = "Sigma" ) } )
  # When a click happens, select the station
  observeEvent( input$profiles_click, {
    prof.clicked <- nearPoints( v$Surv.data, input$profiles_click, 
                                prof.Z.xvar(), prof.Z.yvar(),
                                maxpoints = 1 )
    #browser()
    if( nrow( prof.clicked ) > 0 ) {
      v$Profile.clicked <- prof.clicked$Profile
    } else {
      v$Profile.clicked <- NULL
    }
  })
  #
  observeEvent( input$select_Parameter2View, {
    prof.Z.ranges$x <- NULL
    prof.Z.ranges$y <- NULL
  })
  #
  output$profiles_plot <- renderPlot({
    if( !is.null( v$Surv.data ) & !is.null( input$select_Parameter2View ) ) {
      x.data <- v$Surv.data[ , input$select_Parameter2View ]
      if( all( is.na( x.data ) ) ) return( NULL )
      z.data <- switch( input$profile_vert_axis_SI, 
                        Depth = v$Surv.data$Z, Density = v$Surv.data$Sigma )
      ylab<- switch( input$profile_vert_axis_SI, 
                     Depth = "Depth (m)", Density = "Specific Density (kg/m3)" )
      plot.prof.graph( x.data, z.data, v$Surv.data$Profile, 
                       prof.Z.ranges, 
                       main = input$select_Parameter2View,                       
                       ylab = ylab )
      indx_prof_sel_DT <- input$Profiles_list_DT_rows_selected
      prof_sel_DT <- v$Stn.list.surv[ indx_prof_sel_DT, "Profile", drop = FALSE ]
      if( nrow( prof_sel_DT ) > 0 ) {
        apply( X = prof_sel_DT, MARGIN = 1, FUN = plot.one.profile, 
               x.data, z.data, v$Surv.data$Profile, 
               lwd = 5, col = "#FF00FF" )
      }
      if( !is.null( v$Profile.clicked ) ) {
        if( v$Profile.clicked %in% v$Stn.list.surv$Profile ) {
          plot.one.profile( v$Profile.clicked, x.data, z.data, 
                            v$Surv.data$Profile, lwd = 5, "#0000FF" )
          legend( "bottomright", legend = v$Profile.clicked, 
                  lwd = 5, col = "#0000FF", cex = 1.2 )
        }      
      }
      if( !is.null( input$show_ref_prof_cbI ) & !is.null( v$RefProf ) & 
          !is.null( v$Outrange.Param ) & !is.null( input$select_Parameter2View ) ) {
        if( ( input$show_ref_prof_cbI ) & 
            ( input$select_Parameter2View == v$Outrange.Param ) ) {
          Ref.Prof.Kstd <- as.numeric( with( v$settings$Outr, Value[ which( ShortName == "outr_refprof_Kstd" )[1] ] ) )
          ref.prof.params <- switch( input$profile_vert_axis_SI, 
                                     Depth = c("Z","V.mean","V.std"), Density = c("Sigma","V.mean","V.std") )
          
          plot.ref.profile( v$RefProf[ , ref.prof.params ], Ref.Prof.Kstd )
        }
      }
    }
  })
  #
  observeEvent( input$profiles_dblclick, {
    brush <- input$profiles_brush
    if (!is.null(brush)) {
      prof.Z.ranges$x <- c(brush$xmin, brush$xmax)
      prof.Z.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      prof.Z.ranges$x <- NULL
      prof.Z.ranges$y <- NULL
    }
  })
  #
  #
  # Plume functions -------------------------------------------------
  #
  observeEvent( input$Detect_plume_AB, {
    #browser()
    if( !is.null( v$Surv.data) ) {
      CDOM.thrsh.pc <- with( v$settings$Plume, 
                             Value[ which( ShortName == "plume_CDOM_thrsh" )[1] ] )
      v$Surv.data$Plume <- CDOM.plume.detect( v$Stn.list.surv, 
                                              v$Surv.data, v$settings, CDOM.thrsh.pc )
      Plume.profiles <- unique( v$Surv.data$Profile[ v$Surv.data$Plume ] )
      if( length( Plume.profiles ) == 0 ) {
        v$Stn.list.surv$Plume <- FALSE
      } else {
        v$Stn.list.surv$Plume <- NULL
        v$Stn.list.surv <- merge( v$Stn.list.surv, 
                                  data.frame( Profile = Plume.profiles, Plume = TRUE ), all.x = TRUE )
        v$Stn.list.surv$Plume[ is.na( v$Stn.list.surv$Plume) ] <- FALSE
        shinyjs::show( "Edit_plume_list_AB" )
      }
    }
  })
  #
  observeEvent( input$Edit_plume_list_AB, {
    if( !is.null( v$Surv.data) & !is.null( v$Stn.list.surv ) & 
        sum( v$Stn.list.surv$Plume ) > 0 ) {
      output$plume_list_edit_UI <- renderUI({
        selectInput( "plume_list_edit_SI", 
                     "Edit the list of plume stations:", 
                     multiple = TRUE,
                     choices = v$Stn.list.surv$Profile, 
                     selected = v$Stn.list.surv$Profile[ v$Stn.list.surv$Plume ]
        )
      })
      shinyjs::show( "plume_list_accept_AB" )
      shinyjs::show( "plume_list_cancel_AB" )
    }
  })
  #
  observeEvent( input$plume_list_accept_AB, {
    Plume.list.indx <- which( v$Stn.list.surv$Profile %in% input$plume_list_edit_SI )
    v$Stn.list.surv$Plume <- FALSE
    v$Stn.list.surv$Plume[ Plume.list.indx ] <- TRUE
    v$Stn.list.surv$Outrange <- FALSE
    #
    output$plume_list_edit_UI <- renderUI({ NULL })
    shinyjs::hide( "plume_list_accept_AB" )
    shinyjs::hide( "plume_list_cancel_AB" )
  })
  #
  observeEvent( input$plume_list_cancel_AB, {
    output$plume_list_edit_UI <- renderUI({ NULL })
    shinyjs::hide( "plume_list_accept_AB" )
    shinyjs::hide( "plume_list_cancel_AB" )
  })
  #
  output$plume_settings_DT <- DT::renderDataTable(
    v$settings$Plume,
    rownames = FALSE,
    options = list( dom = 't'),
    server = FALSE,
    selection = list( mode = "single", selected = 1 ) 
  )
  #
  observeEvent( input$plume_settings_edit_AB, {
    indx_plume_setting <- input$plume_settings_DT_rows_selected
    if( length ( indx_plume_setting ) > 0 ) {
      output$plume_setting_edit_UI <- renderUI({
        textInput( "plume_settings_edit_TI", 
                   label = v$settings$Plume$ShortName[ indx_plume_setting ],
                   value = v$settings$Plume$Value[ indx_plume_setting ] )
      })
      shinyjs::show( "plume_settings_accept_AB" )
      shinyjs::show( "plume_settings_cancel_AB" )
      output$plume_settings_DT <- DT::renderDataTable(
        v$settings$Plume,
        rownames = FALSE,
        options = list( dom = 't'),
        server = FALSE,
        selection = list( mode = "none" ) 
      )
    }
  })
  # 
  observeEvent( input$plume_settings_accept_AB, {
    indx_plume_setting <- input$plume_settings_DT_rows_selected
    value <- as.numeric( input$plume_settings_edit_TI )
    if( is.finite( value ) ) {
      v$settings$Plume$Value[ indx_plume_setting ] <- value
    }
    output$plume_setting_edit_UI <- renderUI({NULL})
    shinyjs::hide( "plume_settings_accept_AB" )
    shinyjs::hide( "plume_settings_cancel_AB" )
    output$plume_settings_DT <- DT::renderDataTable(
      v$settings$Plume,
      rownames = FALSE,
      options = list( dom = 't'),
      server = FALSE,
      selection = list( mode = "single", selected = 1 ) 
    )
  })
  #
  observeEvent( input$plume_settings_cancel_AB, {
    output$plume_setting_edit_UI <- renderUI({NULL})
    shinyjs::hide( "plume_settings_accept_AB" )
    shinyjs::hide( "plume_settings_cancel_AB" )
    output$plume_settings_DT <- DT::renderDataTable(
      v$settings$Plume,
      rownames = FALSE,
      options = list( dom = 't'),
      server = FALSE,
      selection = list( mode = "single", selected = 1 ) 
    )
  })
  #
  # "Reference stations" functions --------------------------------------------
  #
  observeEvent( input$Select_ref_AB, {
    if( !is.null( v$Surv.data) ) {
      # Detect "large plume" ($RefPlume)
      CDOM.thrsh.pc <- with( v$settings$Ref, 
                             Value[ which( ShortName == "ref_CDOM_thrsh" )[1] ] )
      RefPlume <- CDOM.plume.detect( v$Stn.list.surv, 
                                     v$Surv.data, v$settings, CDOM.thrsh.pc )
      Plume.profiles <- unique( v$Surv.data$Profile[ RefPlume ] )
      if( length( Plume.profiles ) == 0 ) {
        v$Stn.list.surv$RefPlume <- FALSE
      } else {
        v$Stn.list.surv$RefPlume <- NULL
        v$Stn.list.surv <- merge( v$Stn.list.surv, 
                                  data.frame( Profile = Plume.profiles, RefPlume = TRUE ), all.x = TRUE )
        v$Stn.list.surv$RefPlume[ is.na( v$Stn.list.surv$RefPlume) ] <- FALSE
      }
      # Select reference stations 
      v$Stn.list.surv$Ref <- Ref.stns.select( v$Stn.list.surv, 
                                              v$Surv.data, v$settings )
      #
      shinyjs::show( "Edit_ref_list_AB" )
      if( !is.null( v$Outrange.Param ) ) {
        shinyjs::show( "calc_ref_profile_AB" )
      }
      shinyjs::hide( "detect_outranges_AB" )
      #
      v$RefProf <- NULL
      v$Stn.list.surv$Outrange <- NULL
    }
  })
  #
  observeEvent( input$Edit_ref_list_AB, {
    if( !is.null( v$Surv.data) & !is.null( v$Stn.list.surv ) & 
        sum( v$Stn.list.surv$Plume ) > 0 ) {
      output$ref_list_edit_UI <- renderUI({
        selectInput( "ref_list_edit_SI", 
                     "Edit the list of reference stations:", 
                     multiple = TRUE,
                     choices = v$Stn.list.surv$Profile[ !v$Stn.list.surv$Plume ], 
                     selected = v$Stn.list.surv$Profile[ v$Stn.list.surv$Ref ]
        )
      })
      shinyjs::show( "ref_list_accept_AB" )
      shinyjs::show( "ref_list_cancel_AB" )
    }
  })
  #
  observeEvent( input$ref_list_accept_AB, {
    Ref.list.indx <- which( v$Stn.list.surv$Profile %in% input$ref_list_edit_SI )
    v$Stn.list.surv$Ref <- FALSE
    v$Stn.list.surv$Ref[ Ref.list.indx ] <- TRUE
    shinyjs::hide( "detect_outranges_AB" )
    v$RefProf <- NULL
    v$Stn.list.surv$Outrange <- FALSE
    #
    output$ref_list_edit_UI <- renderUI({ NULL })
    shinyjs::hide( "ref_list_accept_AB" )
    shinyjs::hide( "ref_list_cancel_AB" )
  })
  #
  observeEvent( input$ref_list_cancel_AB, {
    output$ref_list_edit_UI <- renderUI({ NULL })
    shinyjs::hide( "ref_list_accept_AB" )
    shinyjs::hide( "ref_list_cancel_AB" )
  })
  #
  output$ref_settings_DT <- DT::renderDataTable(
    v$settings$Ref,
    rownames = FALSE, 
    selection = list( mode = "single", selected = 1 ), 
    options = list(dom = 't'),
    server = FALSE )
  #
  observeEvent( input$ref_settings_edit_AB, {
    indx_ref_setting <- input$ref_settings_DT_rows_selected
    if( length ( indx_ref_setting ) > 0 ) {
      output$ref_setting_edit_UI <- renderUI({
        textInput( "ref_settings_edit_TI", 
                   label = v$settings$Ref$ShortName[ indx_ref_setting ],
                   value = v$settings$Ref$Value[ indx_ref_setting ] )
      })
      shinyjs::show( "ref_settings_accept_AB" )
      shinyjs::show( "ref_settings_cancel_AB" )
      output$ref_settings_DT <- DT::renderDataTable(
        v$settings$Ref,
        rownames = FALSE, 
        selection = list( mode = "none" ), 
        options = list(dom = 't'),
        server = FALSE )
    }
  })
  # 
  observeEvent( input$ref_settings_accept_AB, {
    indx_ref_setting <- input$ref_settings_DT_rows_selected
    value <- as.numeric( input$ref_settings_edit_TI )
    if( is.finite( value ) ) {
      v$settings$Ref$Value[ indx_ref_setting ] <- value
    }
    output$ref_setting_edit_UI <- renderUI({NULL})
    shinyjs::hide( "ref_settings_accept_AB" )
    shinyjs::hide( "ref_settings_cancel_AB" )
    output$ref_settings_DT <- DT::renderDataTable(
      v$settings$Ref,
      rownames = FALSE, 
      selection = list( mode = "single", selected = 1 ), 
      options = list(dom = 't'),
      server = FALSE )
  })
  #
  observeEvent( input$ref_settings_cancel_AB, {
    output$ref_setting_edit_UI <- renderUI({NULL})
    shinyjs::hide( "ref_settings_accept_AB" )
    shinyjs::hide( "ref_settings_cancel_AB" )
    output$ref_settings_DT <- DT::renderDataTable(
      v$settings$Ref,
      rownames = FALSE, 
      selection = list( mode = "single", selected = 1 ), 
      options = list(dom = 't'),
      server = FALSE )
  })
  #
  # "Outrange" functions ---------------------------------------------------
  #
  output$outrange_param_UI <- renderUI({
    selectInput( "Outrange_param_SI", label = "Parameter", 
                 choices = v$settings$Outr.params$outr_param_name,
                 selected = v$Outrange.Param ) 
  })
  #
  observeEvent( input$outrange_param_select_AB, {
    v$Outrange.Param <- input$Outrange_param_SI
    v$settings <- settings.change.params( v$settings, v$Outrange.Param )
    v$RefProf <- NULL
    v$V.diff.list <- NULL
    shinyjs::hide( "v_z_min_DT" )
    v$V.Z.min <- NULL
    v$Stn.list.surv$Outrange <- FALSE
    if( sum( v$Stn.list.surv$Ref ) > 0 ) {
      shinyjs::show( "calc_ref_profile_AB" )
    }
    shinyjs::hide( "detect_outranges_AB" )
    shinyjs::disable( "prof_comp_method_RB" )
  })
  #
  observeEvent( input$calc_ref_profile_AB, {
    if( ( sum( v$Stn.list.surv$Ref ) > 0 ) & ( !is.null( v$Outrange.Param ) ) ) {
      #v$Outrange.Param <- input$Outrange_param_SI
      v$RefProf <- Ref.Prof.calc( v$Outrange.Param, 
                                  v$Surv.data, v$Stn.list.surv, v$settings )
      v$Stn.list.surv$Outrange <- FALSE
      v$V.diff.list <- NULL
      v$V.Z.min <- NULL
      shinyjs::show( "detect_outranges_AB" )
      shinyjs::hide( "v_z_min_DT" )
    }
  })
  #
  output$outr_settings_DT <- DT::renderDataTable(
    v$settings$Outr,
    rownames = FALSE, 
    selection = list( mode = "single", selected = 1 ), 
    options = list(dom = 't'),
    server = FALSE )
  #
  observeEvent( input$outr_settings_edit_AB, {
    indx_outr_setting <- input$outr_settings_DT_rows_selected
    if( length ( indx_outr_setting ) > 0 ) {
      output$outr_setting_edit_UI <- renderUI({
        textInput( "outr_settings_edit_TI", 
                   label = v$settings$Outr$ShortName[ indx_outr_setting ],
                   value = v$settings$Outr$Value[ indx_outr_setting ] )
      })
      shinyjs::show( "outr_settings_accept_AB" )
      shinyjs::show( "outr_settings_cancel_AB" )
      output$outr_settings_DT <- DT::renderDataTable(
        v$settings$Outr,
        rownames = FALSE, 
        selection = list( mode = "none" ), 
        options = list(dom = 't'),
        server = FALSE )
    }
    shinyjs::enable( "prof_comp_method_RB" )
    shinyjs::enable( "entr_setting_CbI" )
  })
  # 
  observeEvent( input$outr_settings_accept_AB, {
    indx_outr_setting <- input$outr_settings_DT_rows_selected
    value <- as.numeric( input$outr_settings_edit_TI )
    if( is.finite( value ) ) {
      v$settings$Outr$Value[ indx_outr_setting ] <- value
    }
    output$outr_setting_edit_UI <- renderUI({NULL})
    shinyjs::hide( "outr_settings_accept_AB" )
    shinyjs::hide( "outr_settings_cancel_AB" )
    output$outr_settings_DT <- DT::renderDataTable(
      v$settings$Outr,
      rownames = FALSE, 
      selection = list( mode = "single", selected = 1 ), 
      options = list(dom = 't'),
      server = FALSE )
    v$settings$Prof.comp$Value <- input$prof_comp_method_RB
    shinyjs::disable( "prof_comp_method_RB" )
    v$settings$Entr$Value <- input$entr_setting_CbI
    shinyjs::disable( "entr_setting_CbI" )
  })
  #
  observeEvent( input$outr_settings_cancel_AB, {
    output$outr_setting_edit_UI <- renderUI({NULL})
    shinyjs::hide( "outr_settings_accept_AB" )
    shinyjs::hide( "outr_settings_cancel_AB" )
    output$outr_settings_DT <- DT::renderDataTable(
      v$settings$Outr,
      rownames = FALSE, 
      selection = list( mode = "single", selected = 1 ), 
      options = list(dom = 't'),
      server = FALSE )
    output$prof_comp_method_UI <- renderUI({
      radioButtons( "prof_comp_method_RB", 
                    label = "Profile compare method", 
                    choices = unique( v$settings$Outr.params$prof_comp_method ),
                    selected = v$settings$Prof.comp$Value,
                    inline = TRUE ) 
    })
    shinyjs::disable( "prof_comp_method_RB" )
    output$entr_setting_UI <- renderUI({
      checkboxInput( "entr_setting_CbI", 
                     label = v$settings$Entr$Comment,
                     value = v$settings$Entr$Value )
    })
    shinyjs::disable( "entr_setting_CbI" )
  })
  #
  output$prof_comp_method_UI <- renderUI({
    shinyjs::disabled( radioButtons( "prof_comp_method_RB", 
                                     label = "Profile compare method", 
                                     choices = unique( v$settings$Outr.params$prof_comp_method ),
                                     selected = v$settings$Prof.comp$Value,
                                     inline = TRUE ) )
  })
  #
  output$entr_setting_UI <- renderUI({
    shinyjs::disabled( checkboxInput( "entr_setting_CbI", 
                                      label = v$settings$Entr$Comment,
                                      value = v$settings$Entr$Value
    ) )
  })
  #
  observeEvent( input$detect_outranges_AB, {
    if( !is.null( v$RefProf ) ) {
      # Calculate differences between plume profiles and the reference profile
      v$V.diff.list <- V.diff.calc( v$Outrange.Param, v$Stn.list.surv,
                                    v$Surv.data, v$RefProf, v$settings )
      # Find the minimum differences and their depths for each plume profile
      Prof.comp <- v$settings$Prof.comp$Value
      v$V.Z.min <- V.Z.min.calc( v$V.diff.list, Prof.comp )
      #browser()
      Outr.prof.list <- outr.detect( v$V.Z.min, v$settings, Prof.comp )
      v$Stn.list.surv$Outrange <- FALSE
      v$Stn.list.surv$Outrange[ v$Stn.list.surv$Profile %in% Outr.prof.list ] <- TRUE
      shinyjs::show( "v_z_min_DT" )
    }
  })
  #
  output$v_z_min_text <- renderText({
    if( !is.null( v$V.Z.min ) ) {
      v.z.min.text <- "Maximum decrease at depths:" 
    } else {
      v.z.min.text <- NULL
    }
  })
  #
  output$v_z_min_DT <- DT::renderDataTable({ 
    if( !is.null( v$V.Z.min ) ) {
      if( v$settings$Prof.comp$Value == "ttest" ) {
        V.Z.min.DT <- data.frame( Profile = rownames( v$V.Z.min ), 
                                  t = v$V.Z.min[,"t.min"],
                                  p = v$V.Z.min[,"p.min"],
                                  Z = v$V.Z.min[,"Z.min"] )
      } else {
        V.Z.min.DT <- data.frame( Profile = rownames( v$V.Z.min ), 
                                  V = v$V.Z.min[,"V.min"],
                                  Z = v$V.Z.min[,"Z.min"] )
        colnames( V.Z.min.DT )[2] <- v$Outrange.Param
      }
      v_z_min_DT <- V.Z.min.DT
    } else {
      matrix( , nrow = 0, ncol = 0 )
    }
  }, server = FALSE, rownames = FALSE, selection = "multiple",
  options = list(dom = 't') )
  #
  output$outr_prof_Z <- renderPlot({
    if( !is.null( v$V.diff.list ) ) {
      Prof.comp <- v$settings$Prof.comp$Value
      profile2View <- names(v$V.diff.list)[input$v_z_min_DT_rows_selected]
      plot.outr.profiles( v$V.diff.list, "Z.plume", Prof.comp, v$Outrange.Param,
                          profile2View, v$settings$Outr )
    }
  })
  #
  output$outr_prof_Sigma <- renderPlot({
    if( !is.null( v$V.diff.list ) ) {
      Prof.comp <- v$settings$Prof.comp$Value
      profile2View <- names(v$V.diff.list)[input$v_z_min_DT_rows_selected]
      plot.outr.profiles( v$V.diff.list, "Sigma", Prof.comp, v$Outrange.Param,
                          profile2View, v$settings$Outr )
    }
  })
  #
  # "Entrainment" functions ---------------------------------------------------
  #
  #
  output$v_z_min_entr_text <- renderText({
    if( !is.null( v$V.Z.min ) ) {
      v.z.min.text <- "Maximum decrease at depths:" 
    } else {
      v.z.min.text <- NULL
    }
  })
  #
  output$v_z_min_entr_DT <- DT::renderDataTable({ 
    if( !is.null( v$V.Z.min ) ) {
      if( v$settings$Prof.comp$Value == "ttest" ) {
        V.Z.min.DT <- data.frame( Profile = rownames( v$V.Z.min ), 
                                  t = v$V.Z.min[,"t.min"],
                                  p = v$V.Z.min[,"p.min"],
                                  Z = v$V.Z.min[,"Z.min"] )
      } else {
        V.Z.min.DT <- data.frame( Profile = rownames( v$V.Z.min ), 
                                  V = v$V.Z.min[,"V.min"],
                                  Z = v$V.Z.min[,"Z.min"] )
        colnames( V.Z.min.DT )[2] <- v$Outrange.Param
      }
      v_z_min_DT <- V.Z.min.DT
    } else {
      matrix( , nrow = 0, ncol = 0 )
    }
  }, server = FALSE, rownames = FALSE, 
  selection = list( mode = "single", selected = 1 ),
  options = list(dom = 't') )
  #
  output$entr_prof_Z <- renderPlot({
    if( !is.null( v$V.diff.list ) ) {
      Profile.selected <- input$v_z_min_entr_DT_rows_selected
      if( !is.null( Profile.selected ) ) {
        entr.plot( v$V.diff.list[[Profile.selected]], "Z.plume", v$Outrange.Param )
      }
    }
  })
  #
  output$entr_prof_Sigma <- renderPlot({
    if( !is.null( v$V.diff.list ) ) {
      Profile.selected <- input$v_z_min_entr_DT_rows_selected
      if( !is.null( Profile.selected ) ) {
        entr.plot( v$V.diff.list[[Profile.selected]], "Sigma", v$Outrange.Param )
      }
    }
  })
  #
  # "t-test" functions
  #
  output$select_param_ttest_UI <- renderUI({
    indx.rem <- ( v$Valid.data$Parameter %in% c("StnID","Profile","Z") )
    selectInput( "select_param_ttest_SI", label = h5( strong( "Parameter" ) ), 
                 as.vector( v$Valid.data$Parameter[ !indx.rem ] ),
                 selected = v$ttest.Param ) 
  })
  # 
  output$depth_range_SI <- renderUI({
    if( !is.null( v$Stn.list.surv ) & !is.null( v$Surv.data ) ) {
      #browser()
      Z.range <- range( v$Surv.data$Z )
      sliderInput( "depth_range_SlIn", label = "Depth range", min = Z.range[1], max = Z.range[2], value = Z.range )
    }
  })
  #
  observeEvent( input$calc_ttest_AB, { 
    if( !is.null( v$Surv.data ) ) {
      v$ttest.Param <- input$select_param_ttest_SI
      ttest.alt <- input$ttest_alternative_SI
      Z.range <- input$depth_range_SlIn
      #
      #browser()
      if( ( "Plume" %in% colnames( v$Stn.list.surv ) ) & 
          ( "Ref" %in% colnames( v$Stn.list.surv ) ) ) {
        indx.plume <- ( v$Surv.data$Profile %in% v$Stn.list.surv$Profile[ v$Stn.list.surv$Plume ] )
        indx.ref <- ( v$Surv.data$Profile %in% v$Stn.list.surv$Profile[ v$Stn.list.surv$Ref ] )
        indx.Z <- ( ( v$Surv.data$Z >= Z.range[1] ) & ( v$Surv.data$Z <= Z.range[2] ) )
        if( ( sum( indx.plume & indx.Z ) > 0 ) & ( sum( indx.ref & indx.Z ) > 0 ) ) {
          V.plume <- v$Surv.data[ indx.plume & indx.Z, v$ttest.Param ]
          V.ref <- v$Surv.data[ indx.ref & indx.Z, v$ttest.Param ]
          output$transm_plume_ref_plot <- renderPlot({
            boxplot( list( Plume = V.plume, Reference = V.ref ), 
                     ylab = v$ttest.Param )
          })
          output$transm_plume_ref_stats <- renderText({
            t.test.res <- t.test( V.plume, V.ref, alternative = ttest.alt )
            alt.hyp.text <- switch( ttest.alt,  two.sided = "not equal to 0",
                                    less = "less than 0",
                                    greater = "greater than 0" )
            conf.int <- switch( ttest.alt,  two.sided = paste( round( t.test.res$conf.int, digits = 3 ), collapse = " " ),
                                less = paste( "-Inf", round( t.test.res$conf.int[2], digits = 3 ) ),
                                greater = paste( round( t.test.res$conf.int[1], digits = 3 ), "Inf" ) )
            #
            t.res.text <- paste( "t-test statistics:",
                                 paste( "method:", t.test.res$method ),
                                 paste( "t =", round( t.test.res$statistic, digits = 3 ) ),
                                 paste( "degrees of freedom =", round( t.test.res$parameter, digits = 3 ) ),
                                 paste( "p =", signif( t.test.res$p.value, digits = 3 ) ), 
                                 paste( "alternative hypothesis: true difference in means is", alt.hyp.text ),
                                 paste( "95 percent confidence interval:", conf.int ),
                                 paste( "mean of", v$ttest.Param, "in plume =",
                                        round( t.test.res$estimate[1], digits = 3 ) ),
                                 paste( "mean of", v$ttest.Param, "in reference =",
                                        round( t.test.res$estimate[2], digits = 3 ) ),
                                 sep = "\r\n" )
            return( t.res.text )
          })
        }
      }
    } else {
      output$transm_plume_ref_plot <- renderPlot({ NULL })
      output$transm_plume_ref_stats <- renderText({ NULL })
    }
    #
  })
  #
  #
  # "Report" functions ---------------------------------------------------
  #
  output$report_text <- renderText({
    if( is.null( v$selected.data.file ) ) {
      report_text <- "Nothing selected"
    } else {
      report_data_file <- report.data.file(  v$selected.data.file, v$Surv.data.tot )
      report_data_selected <- report.data.selected( 
        v$Agency.selected, v$Year.selected, v$Season.selected, v$Surv.data )
      report_plume_list <- report.plume.list( v$Stn.list.surv$Plume, v$Stn.list.surv$Profile )
      report_plume_settings <- report.plume.settings( v$Stn.list.surv$Plume, v$settings$Plume )
      report_ref_list <- report.ref.list( v$Stn.list.surv$Ref, v$Stn.list.surv$Profile )
      report_ref_settings <- report.ref.settings( v$Stn.list.surv$Ref, v$settings$Ref )
      report_outr_param <- report.outr.param( v$Outrange.Param )
      report_ref_prof <- report.ref.prof( v$Outrange.Param, v$RefProf )
      report_outr_settings <- report.outr.settings( v$RefProf, v$settings$Outr )
      report_outr_method <- report.outr.method( v$RefProf, v$settings$Prof.comp$Value )
      report_entr_setting <- report.entr.setting( v$RefProf, v$settings$Entr )
      report_outr_list <- report.outr.list( v$Outrange.Param, v$V.diff.list, v$Stn.list.surv )
      report_max_decrease_depths <- report.max.decrease.depths( v$Outrange.Param, v$settings$Prof.comp$Value, 
                                                                v$V.Z.min )
      #
      report_text <- paste( report_data_file, 
                            report_data_selected, 
                            report_plume_list,
                            paste( report_plume_settings, collapse = "\r\n" ),
                            report_ref_list,
                            paste( report_ref_settings, collapse = "\r\n" ),
                            report_outr_param,
                            report_ref_prof,
                            paste( report_outr_settings, collapse = "\r\n" ),
                            report_outr_method,
                            report_entr_setting,
                            report_outr_list,
                            paste( report_max_decrease_depths, collapse = "\r\n" ),
                            sep = "\r\n" )
      return( report_text )
    }
  })
  #
  output$download_report <- downloadHandler(
    #
    filename = function() {
      report_fn <- paste( v$Agency.selected, " ", v$Year.selected,  " ", 
                          v$Season.selected, ".txt", sep = "" )
      return( report_fn )
    },
    content = function( con ) {
      report_data_file <- report.data.file(  v$selected.data.file, v$Surv.data.tot )
      report_data_selected <- report.data.selected( 
        v$Agency.selected, v$Year.selected, v$Season.selected, v$Surv.data )
      report_plume_list <- report.plume.list( v$Stn.list.surv$Plume, v$Stn.list.surv$Profile )
      report_plume_settings <- report.plume.settings( v$Stn.list.surv$Plume, v$settings$Plume )
      report_ref_list <- report.ref.list( v$Stn.list.surv$Ref, v$Stn.list.surv$Profile )
      report_ref_settings <- report.ref.settings( v$Stn.list.surv$Ref, v$settings$Ref )
      report_outr_param <- report.outr.param( v$Outrange.Param )
      report_ref_prof <- report.ref.prof( v$Outrange.Param, v$RefProf )
      report_outr_settings <- report.outr.settings( v$RefProf, v$settings$Outr )
      report_outr_method <- report.outr.method( v$RefProf, v$settings$Prof.comp$Value )
      report_entr_setting <- report.entr.setting( v$RefProf, v$settings$Entr )
      report_outr_list <- report.outr.list( v$Outrange.Param, v$V.diff.list, v$Stn.list.surv )
      report_max_decrease_depths <- report.max.decrease.depths( v$Outrange.Param, v$settings$Prof.comp$Value, 
                                                                v$V.Z.min )
      writeLines( c( report_data_file, 
                     report_data_selected,
                     report_plume_list,
                     report_plume_settings,
                     report_ref_list,
                     report_ref_settings, 
                     report_outr_param, 
                     report_ref_prof, 
                     report_outr_settings, 
                     report_outr_method, 
                     report_entr_setting, 
                     report_outr_list, 
                     report_max_decrease_depths
      ), 
      con = con, sep = "\r\n")
    }
  )
  #
}
# Server - END *****************************************************************

shinyApp( ui, server )
