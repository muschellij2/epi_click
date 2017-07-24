
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
                   disabled(actionButton("back_btn", "Back")),
                   disabled(actionButton("btn", "Next")),
                   br(),
                   disabled(actionButton("see_data", "See Drawn Data")),
                   br(),
                   downloadButton("saveProjection", "Save Projection")
                 ),
                 mainPanel(
                   textOutput("display_username"),
                   div(id = "Intro",
                       includeMarkdown("intro.md")
                   ),
                   shinydrawrUI("outbreak_stats1"),
                   shinydrawrUI("outbreak_stats2"),
                   shinydrawrUI("outbreak_stats3"),
                   shinydrawrUI("outbreak_stats4"),
                   shinydrawrUI("outbreak_stats5"),
                   # shinydrawrUI("outbreak_stats"),
                   hidden(div(id = "End",
                              includeMarkdown("end.md")
                   )),
                   tableOutput("displayDrawn")
                   
                 )
               )
)
