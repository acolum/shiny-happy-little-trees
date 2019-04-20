### R Shiny application to recommend Bob Ross episode(s)
### based off of characteristics outlined/collected in
### fivethirtyeight's dataset:
### https://github.com/fivethirtyeight/data/tree/master/bob-ross

library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(readr)
library(bubbles)
library(grDevices)
library(googleVis)

data <- read_csv("elements-by-episode.csv")
data <- data.frame(data)
data$SEASON <- substr(data$EPISODE, start = 2, stop = 3)
data$EPISODE <- substr(data$EPISODE, start = 5, stop = 6)

lower_names <- tolower(names(data)[-c(1,2,70)])
lower_names <- gsub("_"," ",lower_names)

######################################## Define UI ################################################
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage("Happy Little Trees!",
    tabPanel("Find a Painting", icon = icon("paint-brush"),
             sidebarLayout(
               sidebarPanel(
                 h4("Search for an episode of 'The Joy of Painting' 
                    by typing an element of a painting..."),
                 selectizeInput("feature", label = "Happy trees or happy clouds?",
                                choices = c("Apple Frame" = "APPLE_FRAME",
                                            "Aurora Borealis" = "AURORA_BOREALIS",
                                            "Barn" = "BARN",
                                            "Beach" = "BEACH",
                                            "Boat" = "BOAT",
                                            "Bridge" = "BRIDGE",
                                            "Building" = "BUILDING",
                                            "Bushes" = "BUSHES",
                                            "Cabin" = "CABIN",
                                            "Cactus" = "CACTUS",
                                            "Circle Frame" = "CIRCLE_FRAME",
                                            "Cirrus" = "CIRRUS",
                                            "Cliff" = "CLIFF",
                                            "Clouds" = "CLOUDS",
                                            "Conifer" = "CONIFER",
                                            "Cumulus" = "CUMULUS",
                                            "Deciduous" = "DECIDUOUS",
                                            "Diane Andre" = "DIANE_ANDRE",
                                            "Dock" = "DOCK",
                                            "Double Oval Frame" = "DOUBLE_OVAL_FRAME",
                                            "Farm" = "FARM",
                                            "Fence" = "FENCE",
                                            "Fire" = "FIRE",
                                            "Florida Frame" = "FLORIDA_FRAME",
                                            "Flowers" = "FLOWERS",
                                            "Fog" = "FOG",
                                            "Framed" = "FRAMED",
                                            "Grass" = "GRASS",
                                            "Guest" = "GUEST",
                                            "Half Circle Frame" = "HALF_CIRCLE_FRAME",
                                            "Half Oval Frame" = "HALF_OVAL_FRAME",
                                            "Hills" = "HILLS",
                                            "Lake" = "LAKE",
                                            "Lakes" = "LAKES",
                                            "Lighthouse" = "LIGHTHOUSE",
                                            "Mill" = "MILL",
                                            "Moon" = "MOON",
                                            "Mountain" = "MOUNTAIN",
                                            "Mountains" = "MOUNTAINS",
                                            "Night" = "NIGHT",
                                            "Ocean" = "OCEAN",
                                            "Oval Frame" = "OVAL_FRAME",
                                            "Palm Trees" = "PALM_TREES",
                                            "Path" = "PATH",
                                            "Person" = "PERSON",
                                            "Portrait" = "PORTRAIT",
                                            "Rectangle 3D Frame" = "RECTANGLE_3D_FRAME",
                                            "Rectangular Frame" = "RECTANGULAR_FRAME",
                                            "River" = "RIVER",
                                            "Rocks" = "ROCKS",
                                            "Seashell Frame" = "SEASHELL_FRAME",
                                            "Snow" = "SNOW",
                                            "Snowy Mountain" = "SNOWY_MOUNTAIN",
                                            "Split Frame" = "SPLIT_FRAME",
                                            "Son: Steve Ross" = "STEVE_ROSS",
                                            "Structure" = "STRUCTURE",
                                            "Sun" = "SUN",
                                            "Tomb Frame" = "TOMB_FRAME",
                                            "Tree" = "TREE",
                                            "Trees" = "TREES",
                                            "Triple Frame" = "TRIPLE_FRAME",
                                            "Waterfall" = "WATERFALL",
                                            "Waves" = "WAVES",
                                            "Windmill" = "WINDMILL",
                                            "Window Frame" = "WINDOW_FRAME",
                                            "Winter" = "WINTER",
                                            "Wood Framed" = "WOOD_FRAMED"),
                                multiple = T
                                ),
                img(src="BobRoss.jpg", align = "bottom", width = "100%") 
               ),
               mainPanel(dataTableOutput("suggestions"),
                         h4("All episodes can be found on", 
                            a("Bob Ross's YouTube Channel", href = "https://www.youtube.com/user/BobRossInc", target = "_blank"),
                            "or",
                            a("Netflix.", href = "https://www.netflix.com/", target = "_blank")
                            )
               )
             )
    ),
    tabPanel("Visualizations", icon = icon("area-chart"), 
             mainPanel(
                        tabsetPanel(
                          tabPanel("Bubble Chart", icon = icon("spinner"), 
                                   helpText("Counts of Trees, Clouds, and other Features in 'The Joy of Painting'"),
                                   bubblesOutput("bubbles")), 
                          tabPanel("Donut Chart", icon = icon("circle-o"), 
                                   fillPage(
                                     htmlOutput("donut"))
                                   )
             )
           )
    ),
    tabPanel("References", icon = icon("cloud"), 
             h4(a("The Joy of Painting", href = "https://www.bobross.com/", target = "_blank")),
             h4(a("FiveThirtyEight", href = "https://www.fivethirtyeight.com/", target = "_blank")),
                h5(tags$ul(tags$li(a("A Statistical Analysis of the Work of Bob Ross", href = "https://fivethirtyeight.com/features/a-statistical-analysis-of-the-work-of-bob-ross/", target = "_blank")),
                           tags$li(a("Happy Bob Ross Visualizations", href = "https://fivethirtyeight.com/datalab/happy-bob-ross-visualizations/", target = "_blank")),
                           tags$li(a("Raw Data", href = "https://github.com/fivethirtyeight/data/blob/master/bob-ross/elements-by-episode.csv", target = "_blank")))),
             h4("My session information:"), verbatimTextOutput("sessionInfo")),
    tabPanel("Contact", icon = icon("smile-o"),
             h3("Any bugs/issues?"),
                h4(tags$ul(tags$li(a("Please email me your concerns.", href = "mailto:hello@alyssacolumbus.com", target = "_blank")))),
             h3("Want to connect?"),
                h4(tags$ul(tags$li("Follow me on", a("GitHub.", href = "https://github.com/acolum", target = "_blank")),
                        tags$li(a("Tweet Me.", href = "https://twitter.com/alycolumbus", target = "_blank")))),
             img(src="oceansunset.jpg", align = "right", width = "50%") 
    )
  )
)
######################################## Define server ##########################################
server <- function(input, output, session) {
   output$suggestions <- renderDataTable({
     conditions <- NULL
     if(!is.null(input$feature)) {
       conditions <- paste(input$feature, "== 1", collapse = " & ")
     }
     filter_(data, .dots=conditions) %>% 
       select(SEASON, EPISODE, TITLE) %>% 
       select(Season = SEASON, Episode = EPISODE, Title = TITLE)
   }, escape = F)
### Visualizations
data_vis <- data.frame(features = lower_names, counts = colSums(data[-c(1,2,70)]), 
                       prop_100 = round(100*(colSums(data[-c(1,2,70)])/3221),2), 
                      prop_abs = round(100*(colSums(data[-c(1,2,70)])/403),2))

   output$bubbles <- renderBubbles(
     bubbles(value = sqrt(data_vis[,2]), label = lower_names, tooltip = as.character(data_vis[,2]),
             color = rainbow(67, alpha= NULL),
             width = "100%", height = "600px"
     )
   )
   output$donut <-  renderGvis({
     gvisPieChart(data_vis[,c(1,2)],
                  options = list(title = "Relative Frequencies of Trees, Clouds, and other Features in 'The Joy of Painting'", 
                                 width = "automatic", height = "automatic", pieHole = 0.5)
          )
   })
### Session Information
  output$sessionInfo <- renderPrint({
    capture.output(sessionInfo())
  })
}
################################## Run the application ##########################################
shinyApp(ui = ui, server = server)
