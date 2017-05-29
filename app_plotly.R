library(shiny)
library(shinyjs)
library(dplyr)
library(plotly)
library(ggplot2)

N = 200
data = data.frame(x = runif(N, min = 0, max = 10),
                  y = rnorm(N),
                  region = factor(sample(1:5, N, replace = TRUE)),
                  stringsAsFactors = FALSE)
data = arrange(data, region, x, y)

g = plot_ly(data = data, 
            x = ~x, 
            y = ~y) %>% 
  add_markers(color = ~ region, 
              showlegend = FALSE) %>%
  add_paths(color = ~ region)


ui = fluidPage(
  titlePanel("Hey"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      p("Click the button to begin cropping the image")
    ),
    mainPanel(
      plotlyOutput("plot1"),
      verbatimTextOutput("click")
    )
  )
)


server = function(input, output, session) {
  
  v <- reactiveValues(
    imgclick.x = NULL,
    imgclick.y = NULL,
    dbl.x = NULL,
    dbl.y = NULL
  )
  
  
  ### Keep track of click locations if tracing paw or tumor 
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    message('it was clicked')
    print(d)
    if (is.null(d)) "Click on a state to view event data" else d
  })  
  
  
  make_data = reactive({
    df = data.frame(x = v$imgclick.x,
                    y = v$imgclick.y,
                    stringsAsFactors = FALSE)
    df
  })
  
  
  
  output$plot1 <- renderPlotly({
    df = make_data()
    # print(df)
    if (nrow(df) > 0) {
      df$region = factor(1, levels = levels(data$region))
      df = arrange(df, region, x, y)
      print("in there")
      print("df is")
      print(df)
      gg = g
      # gg = g +
      #   geom_point(data = df,
      #              aes(x = x, y = y, colour = region)) +
      #   geom_line(data = df,
      #             aes(x = x, y = y, colour = region))
    } else {
      gg = g
    }
    gg    
  })
  
  
}

shinyApp(ui = ui, server = server)
