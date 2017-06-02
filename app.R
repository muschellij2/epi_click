library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(googleAuthR)
library(googleID)

opts = list()
force = TRUE

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


N = 200
levs = c(2010:2017, "projection")

pal = RColorBrewer::brewer.pal(
  n = length(levs) - 1, "RdYlBu")
pal = c(pal, "black")

###########################################
# Make a fake data.frame
###########################################
data = data.frame(
  x = runif(N, min = 0, max = 10),
  y = runif(N, min = 0, max = 3),
  year = factor(
    sample(2010:2017, N, replace = TRUE),
    levels = levs),
  stringsAsFactors = FALSE
)
ux = sort(unique(data$x))

mean_curve = loess(y ~ x, data = data)
mean_curve = predict(mean_curve, newdata = ux)
mean_curve = data.frame(x = ux, y = mean_curve)
mean_curve$year = factor("projection", levels = levs)

data = full_join(data, mean_curve)
data = arrange(data, year, x, y)
last_point = filter(data, year == "projection") %>% 
  select(x, y) %>% slice(n())


min_x_year = min(data$x)
max_y = 10
max_x_year = max(data$x)
xmax = max_x_year + 10

g = ggplot(data,
           aes(x = x, y = y, colour = year)) +
  theme_bw()
g = g +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = max_x_year,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey50",
    alpha = 0.4,
    colour = NA
  )
g = g + geom_line() +
  xlim(c(min_x_year, xmax))
g = g + ylim(c(0, max_y))
g = g + 
  # scale_colour_discrete(drop = FALSE) +
  scale_color_manual(drop = FALSE, values = pal)
g

ui = fluidPage(titlePanel("Hey"),
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
                   plotOutput(
                     "plot1",
                     click = "plot1_click",
                     dblclick = "plot1_dblclick")
                 )
               )
)


server = function(input, output, session) {
  shinyjs::disable("saveProjection")
  
  v <- reactiveValues(
    imgclick.x = last_point$x,
    imgclick.y = last_point$y,
    # imgclick.x = NULL,
    # imgclick.y = NULL,    
    dbl.x = NULL,
    dbl.y = NULL
  )
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
            "window.location.href = 'https://yourdomain.shinyapps.io/appName';"))
      }
    })
  }
  
  ### Keep track of click locations if tracing paw or tumor
  observeEvent(input$plot1_click, {
    # Keep track of number of clicks for line drawing
    if (input$plot1_click$x > max_x_year & 
        input$plot1_click$x <= xmax) {
      v$imgclick.x <- c(v$imgclick.x, input$plot1_click$x)
      v$imgclick.y <- c(v$imgclick.y, input$plot1_click$y)
    }
    if (length(v$imgclick.x) <= 1) {
      disable("saveProjection")
    } else {
      if (glogin || force){
        enable("saveProjection")
      }
    } 
  })
  
  
  make_data = reactive({
    df = data.frame(
      x = v$imgclick.x,
      y = v$imgclick.y,
      stringsAsFactors = FALSE
    )
    df
  })
  
  observeEvent(input$plot1_dblclick, {
    print(input$plot1_dblclick)
    rm.x = input$plot1_dblclick$x
    if (rm.x > max_x_year) {
      
      rm.y = input$plot1_dblclick$y
      dist = (v$imgclick.x - rm.x) ^ 2 + (v$imgclick.y - rm.y) ^ 2
      rm_point = which.min(dist)
      v$imgclick.x = v$imgclick.x[-rm_point]
      v$imgclick.y = v$imgclick.y[-rm_point]
    }
    if (length(v$imgclick.x) <= 1) {
      disable("saveProjection")
    } else {
      if (glogin || force){
        enable("saveProjection")
      }
    }
  })
  
  ### Original Image
  output$plot1 <- renderPlot({
    df = make_data()
    # print(df)
    if (nrow(df) > 0) {
      df$year = factor("projection", levels = levs)
      df = arrange(df, year, x, y)
      print("in there")
      print("df is")
      print(df)
      gg = g +
        geom_point(data = df,
                   aes(
                     x = x,
                     y = y
                   ), colour = "black") +
        geom_line(data = df,
                  aes(
                    x = x,
                    y = y,
                    colour = year
                  ), colour = "black")
    } else {
      gg = g
    }
    gg
  })
  
  output$saveProjection = downloadHandler(
    filename = "projection.rda",
    content = function(file) {
      df = make_data()
      udets = NULL
      if (glogin) {
        udets = userDetails()  
      }
      timestamp = timestamp()
      save(df, timestamp, udets, file = file)
    }
  )
  
}

print(opts)
shinyApp(ui = ui, server = server, options = opts)
