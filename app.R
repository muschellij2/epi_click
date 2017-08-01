rm(list = ls())
library(shiny)
library(shinyjs)
library(shinysense)
library(shinydashboard)
library(dplyr)
library(googleAuthR)
library(googleID)
library(lubridate)
library(readr)
library(zoo)
library(markdown)
library(lubridate)
library(googlesheets)

options(digits = 20)

#########################################
# Sheet setup
#########################################
source("sheet_setup.R")
source("epi-click-auth.R")
source("create_data.R")

# if (file.exists("epi-click-auth.R")) {

drawn_data = NULL

###########################################
# Make a fake data.frame
###########################################
ngroups = 20
# fname = "plot_data.rds"
fname = "plot_data_date.rds"
# fname = "blah.rds"

L = create_data(fname, ngroups = ngroups,
                number_of_add_weeks = 12)
ngroups = L$ngroups
y_max = L$y_max
x_min = L$x_min
y_min = L$y_min
x_max = L$x_max
data = L$data

draw_start = x_max + 1
plot_names = paste0("outbreak_stats", 1:ngroups)
script <- c("Intro", plot_names, "End")
state <- 1
drawn = NULL
dc = vector(mode = "list", length = ngroups)
names(dc) = plot_names


###################################################
# All UI
###################################################

L = list(      
  textOutput("display_username"),
  div(id = "Intro",
      includeMarkdown("intro.md")
  ),
  h1(id = "group_name", "")
)
for (i in seq_len(ngroups)) {
  L = c(L, shinydrawrUI(paste0("outbreak_stats", i)))
}
L = c(L,
      list(hidden(
        div(id = "End",
            includeMarkdown("end.md")
        ))),
      list(tableOutput("displayDrawn"))
)

ui = fluidPage(
  titlePanel("Projection of Disease Incidence"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p(id = "welcome", "Welcome!"),
      googleAuthUI("gauth_login"),
      disabled(actionButton("btn", "Continue"))
    ),
    do.call("mainPanel", args = L)
  )
)

###################################################
# All Server
###################################################
server = function(input, output, session) {
  session_id = basename(tempfile(pattern = "sess"))
  rv = reactiveValues(
    login = FALSE
  )  
  
  
  ###################################################
  # Validating Google Auths
  ###################################################
  ## Authentication
  accessToken <- callModule(
    googleAuth, "gauth_login",
    login_class = "btn btn-primary",
    logout_class = "btn btn-primary")
  userDetails <- reactive({
    validate(
      need(accessToken(), 
           "You are not logged in.  You must log in to save projections")
    )
    rv$login <- TRUE
    toggleState("btn")
    
    shinyjs::html(id = "welcome", "", add = FALSE)
    shinyjs::html(id = "group_name", html = "Click Contiue to Begin!")        
    
    res = with_shiny(get_user_info, shiny_access_token = accessToken())
    email = res$emails$value[1]
    user = lookup_user(email)
    res$user = user
    res
  })
  
  ## Display user's Google display name after successful login
  output$display_username <- renderText({
    validate(
      need(userDetails(), "getting user details")
    )
    paste0("You are logged in as ", userDetails()$user,
           ", this is randomly generated.")
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
  
  ###################################################
  # End Validating Google Auths
  ###################################################
  run_tab = reactive({
    all_data = NULL
    for (iplot in plot_names) {
      drawChart = dc[[iplot]]$chart
      name = paste0(iplot, "-doneDragging")
      # print("grabbing the name!")
      drawnValues = input[[name]]
      print("drawn values are")
      print(drawnValues)
      
      name2 = paste0(iplot, "-myDrawr_drawnData")
      # print("grabbing the name!")
      drawnValues = input[[name]]
      print("drawn values are")
      print(drawnValues)
      
      if (!is.null(drawnValues)) {
        igroup = as.numeric(gsub("outbreak_stats", "", iplot))
        # need rounding for merge
        drawnValues = round(drawnValues, digits = 8)
        drawn_data <- data %>%
          filter(x >= draw_start,
                 group %in% igroup) %>%
          mutate(y = drawnValues)
        
        if (rv$login) {
          drawn_data$user = userDetails()$user
        } else {
          drawn_data$user = NA
        }
        all_data = rbind(all_data, drawn_data)
        # print(drawn_data)
        shinyjs::enable("btn")
      }
    }
    all_data
  })
  
  
  reset_table = reactive({
    drawn_data = run_tab()
    output$displayDrawn <- renderTable(drawn_data)
  })  
  
  # observeEvent(input$see_data, {
  #   reset_table()
  # })
  
  observeEvent(input$btn, {
    if(state < length(script)){
      # toggle(script[state])
      # toggle(script[state + 1])
      # print("State Now")
      state <<- state + 1      
    }
    
    if (state > length(script)) {
      state = length(script)
    }
    curr_script = script[state]
    
    # print(paste0("curr_script is ", curr_script))
    if (grepl("outbreak", curr_script) & !(curr_script %in% drawn)) {
      
      igroup = as.numeric(gsub("outbreak_stats", "", curr_script))
      # print(paste0("igroup is ", igroup))
      dat = filter(data, group %in% igroup)
      # print(head(dat))
      drawChart = callModule(
        shinydrawr,
        id = curr_script,
        data = dat,
        draw_start = draw_start,
        raw_draw = FALSE,
        x_key = "x",
        y_key = "y",
        y_min = y_min,
        y_max = y_max
      ) 
      print("I tried to draw ")
      print(drawChart)
      print(head(dat))
      dc[[curr_script]] <<- list(
        chart = drawChart,
        data = dat
      )
      print("Names of input")
      print(names(input))     
      print(drawChart())
      drawChart()
      drawn <<- c(drawn, curr_script)
      
    }
    
    if (grepl("outbreak", curr_script)) {
      igroup = as.numeric(gsub("outbreak_stats", "", 
                               curr_script))
      gname <- data %>% 
        filter(group %in% igroup) %>%
        select(group_name)
      gname = unique(gname$group_name)
      gname = gname[ !is.na(gname)]
      shinyjs::html(id = "group_name", 
                    paste0("Area is: ", gname))       
      curr_script = paste0(curr_script, "-youDrawIt")
      
      # reset_table()
    } else {
      output$displayDrawn = renderTable({
        validate(
          need(grepl("outbreak", curr_script), "")
        )
      })
    }
    shinyjs::show(id = curr_script)
    
    prev_script = script[state - 1]
    if (grepl("outbreak", prev_script)) {
      prev_script = paste0(prev_script, "-youDrawIt")
    }
    shinyjs::hide(id = prev_script)
    
    if (grepl("End", curr_script)) {
      shinyjs::show(id = "End")
      shinyjs::html(id = "group_name", html = "")
      shinyjs::disable(id = "btn")
      shinyjs::html(id = "btn", 
                    html = "Saving Data (Don't close until done)!", 
                    add = FALSE)
      
      drawn_data = run_tab()
      print(head(drawn_data))
      if (!is.null(drawn_data)) {
        if (nrow(drawn_data) > 0) {
          drawn_data$date = today()
          drawn_data$session_id = session_id
          drawn_data$group = as.character(drawn_data$group)
          drawn_data = diff_data(drawn_data)
          if ( nrow(drawn_data) > 0) {
            # result = drawn_gs %>% 
            #   gs_add_row(input = drawn_data, verbose = TRUE)
            # print(result)               
            nrows = nrow(drawn_data)
            withProgress(
              message = 'Saving Data', 
              min = 0, 
              max = 1,
              value = 0, 
              {
                for (i in seq_len(nrows)) {
                  result = drawn_gs %>% 
                    gs_add_row(input = drawn_data[i, ], 
                               verbose = TRUE)
                  # Increment the progress bar, and update the detail text.
                  setProgress(value = i/nrows)                  
                }
                
              })
            print("Data Saved!")
          }
        }
      }
      shinyjs::disable(id = "btn")
      
      
    }
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)
