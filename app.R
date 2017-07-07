rm(list=ls())
library(shiny)
library(shinyjs)
library(shinysense)
library(dplyr)
library(googleAuthR)
library(googleID)
library(lubridate)
library(readr)
library(zoo)

opts = list()
force = FALSE

glogin = FALSE
if (file.exists("epi-click-auth.R")) {
  opts = list(port = 1221)
  
  source("epi-click-auth.R")
  options(googleAuthR.scopes.selected = 
            c("https://www.googleapis.com/auth/userinfo.email",
              "https://www.googleapis.com/auth/userinfo.profile"))
  options("googleAuthR.webapp.client_id" = gclient_id)
  options("googleAuthR.webapp.client_secret" = gsecret_id)
  glogin = TRUE
}
drawn_data = NULL

N = 200
levs = c(2010:2017, "projection")

pal = RColorBrewer::brewer.pal(
  n = length(levs) - 1, "RdYlBu")
pal = c(pal, "black")

###########################################
# Make a fake data.frame
###########################################
fname = "plot_data.rds"
if (file.exists(fname)) {
  data = readRDS(fname)
  x_min = min(data$x)
  y_min = 0
  x_max = max(data$x)
  
  number_of_add_weeks = 12
  lower_limit = NA
  eg = expand.grid(x = seq(x_max, x_max + number_of_add_weeks),
                   group = unique(data$group),
                   y = lower_limit)
  data = full_join(data, eg)
  data = data %>% arrange(group, x, y)
  data = data %>% mutate(y = na.locf(y))
  # need to figure out multiple groups
  data = data %>% filter(group == 1)
} else {
  data <- data_frame(
    x = 1:30)
  data = data %>% mutate(
    y = x*sin(x/6) + rnorm(30)
  )
  x_min = 15
  data = data %>% mutate(y = ifelse(x > x_min, -10, y))
  y_min = min(data$y[ data$x <= x_min])
  x_max
}
draw_start = x_max + 1


ui = fluidPage(titlePanel("Projection of Disease Incidence"),
               useShinyjs(),
               sidebarLayout(
                 sidebarPanel(
                   p("Welcome!"),
                   if (glogin) {
                     googleAuthUI("gauth_login")
                   } else {
                     NULL
                   },
                   downloadButton("saveProjection", "Save Projection")
                 ),
                 mainPanel(
                   textOutput("display_username"),
                   shinydrawrUI("outbreak_stats"),
                   tableOutput("displayDrawn")
                 )
               )
)


server = function(input, output, session) {
  shinyjs::disable("saveProjection")
  
  rv = reactiveValues(
    login = FALSE
  )  
  if (glogin) {
    ## Authentication
    accessToken <- callModule(googleAuth, "gauth_login",
                              login_class = "btn btn-primary",
                              logout_class = "btn btn-primary")
    userDetails <- reactive({
      validate(
        need(accessToken(), 
             "You are not logged in.  You must log in to save projections")
      )
      rv$login <- TRUE
      with_shiny(get_user_info, shiny_access_token = accessToken())
    })
    
    ## Display user's Google display name after successful login
    output$display_username <- renderText({
      validate(
        need(userDetails(), "getting user details")
      )
      paste0("You are logged in as ", userDetails()$displayName)
      # userDetails()$displayName
    })
    
    ## Workaround to avoid shinyaps.io URL problems
    observe({
      if (rv$login) {
        shinyjs::onclick(
          "gauth_login-googleAuthUi",
          shinyjs::runjs(
            "window.location.href = 'https://jmuschelli.shinyapps.io/epi_click';"))
      }
    })
  }
  
  


  #server side call of the drawr module
  drawChart <- callModule(shinydrawr,
                          "outbreak_stats",
                          data,
                          draw_start = draw_start,
                          x_key = "x",
                          y_key = "y",
                          y_min = y_min
                          )
  
  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  observeEvent(drawChart(), {
    drawnValues = drawChart()
    # drawnValues = drawnValues[-1]
    print(length(drawnValues))
    print(sum(data$x >= draw_start))
    
    drawn_data <- data %>%
      filter(x >= draw_start) %>%
      mutate(y = drawnValues)
    
    if (rv$login) {
      drawn_data$username = userDetails()$displayName
    } else {
      drawn_data$username = NA
    }
    shinyjs::enable("saveProjection")
    
    output$saveProjection = downloadHandler(
      filename = "projection.rda",
      content = function(file) {
        udets = NULL
        if (rv$login) {
          udets = userDetails()
        }
        timestamp = timestamp()
        save(drawn_data, timestamp, udets, file = file)
      }
    )
    
    output$displayDrawn <- renderTable(drawn_data)
  })
  
  

  
}

print(opts)
shinyApp(ui = ui, server = server, options = opts)
