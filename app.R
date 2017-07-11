rm(list = ls())
library(shiny)
library(shinyjs)
library(shinysense)
library(dplyr)
library(googleAuthR)
library(googleID)
library(lubridate)
library(readr)
library(zoo)

set.seed(20170707)
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
ngroups = 3
fname = "plot_data.rds"
if (file.exists(fname)) {
  data = readRDS(fname)
  data = data %>% filter(group %in% seq(ngroups))
} else {
  ntimes = 15
  n = ntimes * ngroups
  data <- data_frame(
    x = rep(1:ntimes, ngroups),
    group = factor(rep(1:20, each = ntimes)))
  data = data %>% mutate(
    # y = rpois(n, lambda = 99),
    y = rnorm(n, mean = 1.1029, sd = 0.5),
    y = 10^(y) - 1
  )
  data$y[ data$y < 0] = 0
}
ngroups = length(unique(data$group))
y_max = max(data$y)
x_min = min(data$x)
y_min = 0
x_max = max(data$x)

number_of_add_weeks = 12

lower_limit = NA_real_
eg = expand.grid(x = seq(x_max + 1, x_max + 1 + number_of_add_weeks),
                 group = unique(data$group),
                 y = lower_limit)
data = full_join(data, eg)
data = data %>% arrange(group, x, y)
data = data %>% mutate(y = na.locf(y))

# need to figure out multiple groups
# data = data %>% filter(group == 1)

draw_start = x_max + 1
script <- c("Intro", paste0("outbreak_stats", 1:ngroups), "End")
state <- 1

# hidden(  shinydrawrUI("outbreak_stats"))
# args = list(
#   textOutput("display_username"),
#   shinydrawrUI("outbreak_stats")
# )
# tableOutput("displayDrawn")

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
                   downloadButton("saveProjection", "Save Projection"),
                   disabled(actionButton("btn", "Next"))
                 ),
                 mainPanel(
                   textOutput("display_username"),
                   div(id = "Intro",
                       includeMarkdown("intro.md")
                   ),
                   shinydrawrUI("outbreak_stats1"),
                   shinydrawrUI("outbreak_stats2"),
                   shinydrawrUI("outbreak_stats3"),
                   # shinydrawrUI("outbreak_stats"),
                   hidden(div(id = "End",
                              includeMarkdown("end.md")
                   )),
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
      toggleState("btn")
      
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
  
  
  observeEvent(input$btn, {
    print(input$btn)
    print(state)
    print(script[state])
    if(state < length(script)){
      toggle(script[state])
      toggle(script[state + 1])
    }
    # print("State Now")
    state <<- state + 1
    curr_script = script[state]
    if (grepl("outbreak", curr_script)) {
      igroup = as.numeric(gsub("outbreak_stats", "", curr_script))
      print(paste0("igroup is ", igroup))
      dat = filter(data, group %in% igroup)
      print(head(dat))
      drawChart = callModule(shinydrawr,
                 curr_script,
                 dat,
                 draw_start = draw_start,
                 raw_draw = FALSE,
                 draw_after = FALSE,
                 x_key = "x",
                 y_key = "y",
                 y_min = y_min,
                 y_max = y_max
      ) 
      print(drawChart)
      drawChart()
      # "#" + params.id + 
    }
    prev_script = script[state - 1]
    if (grepl("outbreak", prev_script)) {
      shinyjs::hide(id = paste0(prev_script, "-youDrawIt"))
    }
    
  })
  
  
  
  #server side call of the drawr module
  # drawChart <- callModule(shinydrawr,
  #                         "outbreak_stats",
  #                         data,
  #                         draw_start = draw_start,
  #                         raw_draw = FALSE,
  #                         draw_after = FALSE,
  #                         x_key = "x",
  #                         y_key = "y",
  #                         y_min = y_min,
  #                         y_max = y_max
  #                         )
  
  #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  # observeEvent(drawChart(), {
  #   drawnValues = drawChart()
  #   # drawnValues = drawnValues[-1]
  #   print(length(drawnValues))
  #   print(sum(data$x >= draw_start))
  #   
  #   drawn_data <- data %>%
  #     filter(x >= draw_start) %>%
  #     mutate(y = drawnValues)
  #   
  #   if (rv$login) {
  #     drawn_data$username = userDetails()$displayName
  #   } else {
  #     drawn_data$username = NA
  #   }
  #   shinyjs::enable("saveProjection")
  #   
  #   output$saveProjection = downloadHandler(
  #     filename = "projection.rda",
  #     content = function(file) {
  #       udets = NULL
  #       if (rv$login) {
  #         udets = userDetails()
  #       }
  #       timestamp = timestamp()
  #       save(drawn_data, timestamp, udets, file = file)
  #     }
  #   )
  #   
  #   output$displayDrawn <- renderTable(drawn_data)
  # })
  
  
  
  
}

print(opts)
shinyApp(ui = ui, server = server, options = opts)
