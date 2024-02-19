library(shiny)
library(tidyverse)
library(RSelenium)
library(glue)
library(toastui)
library(bslib)
library(shinycssloaders)
library(shinyjs)

sys_info <- Sys.info()
if (sys_info[["sysname"]] == "Darwin") {
  system("docker container start sleepy_bhabha")
} else if (sys_info[["sysname"]] == "Linux") {
  system("docker container start frosty_hermann")
}
# system("docker run -d -p 4445:4444 -p 5900:5900 selenium/standalone-firefox-debug")


ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  useShinyjs(),
  fluidRow(
    column(
      width = 12,
      br(),
      h3("Harvard Tennis Availability"),
      actionButton("get_data", "Get Data"),
      textOutput("progress"),
      br(),
      br()
    )
  ),
  fluidRow(
    column(
      width = 12,
      calendarOutput("cal", height = 400) %>% withSpinner()
    )
  )
  
)

server <- function(input, output, session) {
  
  reactives <- reactiveValues(
    calendar_data = NULL
  )
  
  observeEvent(input$get_data, {
    shinyjs::html("progress", "Please wait...")
    
    remDr <- remoteDriver(port = 4445L)
    remDr$open()
    
    calendar_data <- NULL
    
    remDr$navigate("https://membership.gocrimson.com/Program?classificationId=dc42ec33-82df-44ca-b06c-109c3685395d")
    Sys.sleep(0.1)
    court_links <- remDr$findElements(using = "xpath", "//div[@class = 'program-list-item']/a") %>% 
      map(
        .f = ~ .x$getElementAttribute("href")
      ) %>% 
      unlist()
    
    for (i in 1:length(court_links)) {

      remDr$navigate(court_links[i])
      Sys.sleep(0.3)
      court_element <- remDr$findElement(using = "xpath", "//h1[@class = 'progTitle']") 
      court <- court_element$getElementText() %>% unlist()
      
      date_buttons <- remDr$findElements(using = "xpath", "//div[@class = 'd-none d-lg-block']/div/button[@type = 'button' and contains(@class, 'single-date-select-button ')]")
      
      dates <- date_buttons %>% 
        map(
          .f = ~ .x$getElementAttribute("data-date-text")
        ) %>% 
        unlist() %>% 
        unique()
      
      for (j in 1:length(date_buttons)) {
        
        # go to the date
        date_buttons[[j]]$clickElement()
        Sys.sleep(1)
        # get the slots
        slots <- remDr$findElements(using = "xpath", "//span[@class = 'instance-time-header']") %>% 
          map(
            .f = ~ .x$getElementText()
          ) %>% 
          unlist()
        
        # get the availability
        availability <- remDr$findElements(using = "xpath", "//div[contains(@class, 'spots-tag')]")  %>% 
          map(
            .f = ~ .x$getElementText()
          ) %>% 
          unlist()
        
        # add to result
        this_result <- tibble(
          title = court,
          body = glue("<a href='{court_links[i]}' target = '_blank'>Register</a> - {availability}"),
          date = as.Date(dates[j], format = '%b %d, %Y'),
          start_time = str_extract(slots,"^\\d{1,2}:\\d{2} [AP]M"),
          end_time = str_extract(slots,"\\d{1,2}:\\d{2} [AP]M$"),
          availability = availability
        ) %>% 
          rowwise() %>% 
          mutate(
            start = glue("{date} {start_time}") %>% as.POSIXct(format = "%Y-%m-%d %I:%M %p") ,
            end = glue("{date} {end_time}") %>% as.POSIXct(format = "%Y-%m-%d %I:%M %p") ,
            backgroundColor = if (grepl("Spot Left", availability)) {"#1EA62E"} else {"#DEDEDE"}
          ) %>% 
          select(-availability, -start_time, -end_time)
        
        calendar_data <- calendar_data %>% 
          bind_rows(this_result)
        
        shinyjs::html("progress", glue("Getting availability: Court {i} {dates[j]}"))
      }
    }
    
    
    output$cal <- renderCalendar({
      
      calendar_data %>% 
        arrange(title) %>% 
        calendar(
          view = "week",
          navigation = TRUE
        ) %>% 
        cal_week_options(
          hourStart = 7,
          eventView = "time",
          taskView = FALSE
        )
    })
    
  })
  
  
  
}

shinyApp(ui, server)