library(shiny)
library(tidyverse)
library(leaflet)
library(shinyjs)
library(bslib)


# Data -----
trips <- read_delim("gtfs_blablacar_bus/trips.txt", delim = ",", show_col_types = FALSE)
stop_times <- read_delim("gtfs_blablacar_bus/stop_times.txt", delim = ",", show_col_types = FALSE)
stops <- read_delim("gtfs_blablacar_bus/stops.txt", delim = ",", show_col_types = FALSE)
calendar_dates <- read_delim("gtfs_blablacar_bus/calendar_dates.txt", delim = ",", show_col_types = FALSE)

trips <- trips %>% select(-c(bikes_allowed, wheelchair_accessible))
stops <- stops %>% select(stop_id, stop_name, stop_lat, stop_lon)
calendar_dates <- calendar_dates %>% select(-exception_type) %>% mutate(date = ymd(date))
stop_times <- stop_times %>% select(-c(drop_off_type, pickup_type, shape_dist_traveled))

calendar_dates <- calendar_dates %>% 
  left_join(trips, by="service_id") %>% 
  right_join(stop_times, by="trip_id", relationship = "many-to-many") %>% 
  left_join(stops, by="stop_id")

rm(stop_times, stops, trips)

# Shiny -----
ui <- fillPage(
  title = "BlablExplorer - Explore Bus Routes",
  theme = bs_theme(bootswatch = "flatly"),
  useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML("
      .sidebar-panel { width: 340px; padding: 14px; background: rgba(255,255,255,0.98);
                       box-shadow: 2px 6px 18px rgba(0,0,0,0.15); border-radius: 10px;
                       transition: transform 0.28s cubic-bezier(.2,.8,.2,1); z-index: 1400; }
      .sidebar-hidden { transform: translateX(-380px) !important; }
      .toggle-btn { position: absolute; top: 12px; left: 12px; z-index: 1600; }
      .info-box { position: absolute; bottom: 18px; left: 50%; transform: translateX(-50%);
                  z-index: 1400; padding: 8px 12px; background: rgba(255,255,255,0.95);
                  border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.18); }
      /* fallback CSS in case JS doesn't run */
      .leaflet-control-zoom { right: 12px !important; bottom: 18px !important;
                             top: auto !important; left: auto !important;
                             z-index: 1500 !important; }
    "))
  ),
  
  actionButton("toggleSidebar", "☰", class = "btn btn-primary toggle-btn"),
  
  absolutePanel(
    id = "sidebar", class = "sidebar-panel",
    top = 60, left = 12, width = 360, fixed = TRUE, draggable = FALSE,
    h4("🚌 BlablExplorer"),
    
    selectizeInput(
      "stop_name",
      "Where From?",
      choices = if (exists("calendar_dates")) sort(unique(calendar_dates$stop_name)) else NULL,
      selected = NULL,
      options = list(placeholder = 'Type to search a stop name...', maxOptions = 10, create = FALSE)
    ),
    dateInput("date", "Enter Date", value = today()),
    actionButton("submit", "Show Available Destinations", class = "btn-primary", width = "100%"),
    br(), br(),
    
    tags$div(
      style = "font-size: 0.9em; background-color: #fff8e1; border-left: 4px solid #ffc107;
             padding: 10px; border-radius: 6px; margin-bottom: 10px;",
      "⚠️ This is a personal project, not affiliated or endorsed by BlaBlaCar.
     Some destinations might appear in this app but not on the official BlablaCar site. 
     This happens because the open transport data may include planned trips that don’t 
     always match real availability."
    ),
    
    tags$p(
      style = "font-size: 0.95em; margin-top: 8px;",
      "For real trip bookings, visit ",
      tags$a(href = "https://www.blablacar.com", target = "_blank", "blablacar.com", 
             style = "font-weight: 600; color: #007bff; text-decoration: none;"),
      "."
    ),
    
    br(),
    tags$div(
      style = "display: flex; gap: 12px; justify-content: center; align-items: center; margin-top: 10px;",
      tags$a(
        href = "https://github.com/dufourlorenzo", target = "_blank",
        tags$i(class = "fab fa-github fa-lg", style = "color:#333;")
      ),
      tags$a(
        href = "https://www.linkedin.com/in/dufourlorenzo", target = "_blank",
        tags$i(class = "fab fa-linkedin fa-lg", style = "color:#0077b5;")
      )
    )
  )
  ,
  
  div(class = "info-box", textOutput("info_text")),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  shinyjs::useShinyjs()
  
  sidebar_open <- reactiveVal(TRUE)
  runjs("document.getElementById('sidebar').classList.remove('sidebar-hidden');")
  
  observe({
    if (exists("calendar_dates")) {
      choices_list <- sort(unique(calendar_dates$stop_name))
      updateSelectizeInput(session,
                           "stop_name",
                           choices = choices_list,
                           selected = character(0),
                           server = TRUE)
    }
    isolate(NULL)
  })
  
  moveZoomJS <- "
  (function(){
    var tryMove = function(){
      var z = document.querySelector('.leaflet-control-zoom');
      if(!z) return false;
      z.style.position = 'absolute';
      z.style.right = '12px';
      z.style.left = 'auto';
      z.style.top = 'auto';
      z.style.bottom = '18px';
      z.style.zIndex = 1600;
      return true;
    };
    if(tryMove()) return;
    var attempts = 0;
    var id = setInterval(function(){
      attempts++;
      if(tryMove() || attempts > 12) clearInterval(id);
    }, 250);
  })();
  "
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE, preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 5.45, lat = 43.5, zoom = 6)
  })
  
  observe({
    invalidateLater(500, session)
    runjs(moveZoomJS)
  })
  
  observeEvent(input$toggleSidebar, {
    if (sidebar_open()) {
      addClass("sidebar", "sidebar-hidden"); sidebar_open(FALSE)
    } else {
      removeClass("sidebar", "sidebar-hidden"); sidebar_open(TRUE)
    }
    runjs(moveZoomJS)
  })
  
  reachable_stops_data <- eventReactive(input$submit, {
    req(input$stop_name, input$stop_name != "", input$date)
    
    given_stop_name <- input$stop_name
    given_date <- input$date
    
    services_of_given_stop <- calendar_dates %>%
      filter(stop_name == given_stop_name, date == given_date) %>%
      select(service_id, stop_sequence)
    
    reachable_stops <- calendar_dates %>%
      filter(service_id %in% services_of_given_stop$service_id, date == given_date) %>%
      left_join(services_of_given_stop, by = "service_id", suffix = c("", "_given")) %>%
      filter(stop_sequence > stop_sequence_given)
    
    reachable_stops %>% distinct(stop_id, stop_name, stop_lat, stop_lon, arrival_time, departure_time)
  })
  
  observeEvent(input$submit, {
    reachable_stops <- reachable_stops_data()
    
    if (nrow(reachable_stops) == 0) {
      output$info_text <- renderText("⚠️ No reachable stops found for this stop or date.")
      leafletProxy("map") %>% clearMarkers()
    } else {
      output$info_text <- renderText(paste0("✅ Found ", nrow(reachable_stops), " reachable stops."))
      proxy <- leafletProxy("map")
      
      given_stop_name <- input$stop_name
      given_date <- input$date
      
      proxy %>% clearMarkers() %>%
        addCircleMarkers(
          data = reachable_stops,
          lat = ~stop_lat, lng = ~stop_lon,
          popup = ~paste0(
            "<strong>", stop_name, "</strong><br><br>",
            "<a href='https://www.blablacar.com/search?",
            "fn=", URLencode(given_stop_name),
            "&tn=", URLencode(stop_name),
            "&db=", given_date,
            "&seats=1' ",
            "target='_blank' style='color:#007bff; font-weight:600; text-decoration:none;'>",
            "Book this trip on BlaBlaCar.com</a>"
          ),
          radius = 6,
          stroke = FALSE,
          fillOpacity = 0.9,
          color = "#007bff"
        )
      
      lat_rng <- range(reachable_stops$stop_lat, na.rm = TRUE)
      lon_rng <- range(reachable_stops$stop_lon, na.rm = TRUE)
      if (all(is.finite(lat_rng)) && all(is.finite(lon_rng))) {
        proxy %>% fitBounds(lng1 = lon_rng[1], lat1 = lat_rng[1], lng2 = lon_rng[2], lat2 = lat_rng[2])
      }
      runjs(moveZoomJS)
    }
  })
}

shinyApp(ui = ui, server = server)
