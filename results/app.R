#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

files <- dir("output/")
files_detect <- files[str_detect(files, "detect.csv$")]

df_detect <- map_df(files_detect, function(x){
    read_csv(paste0("output/", x))
})

files_perf <- files[str_detect(files, "perf.csv$")]
df_perf <- map_df(files_perf, function(x){
    read_csv(paste0("output/", x))
})

files_null <- files[str_detect(files, "null.csv$")]
df_null <- map_df(files_null, function(x){
    read_csv(paste0("output/", x))
})

files_time <- files[str_detect(files, "time.csv$")]
df_time <- map_df(files_time, function(x){
    read_csv(paste0("output/", x))
})

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Simulation results"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("effectsize", "Effect size?", choices = c(0.8, 0.9, 1, 1.1, 1.81), selected = 0.8)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("numbers")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$numbers <- renderPlot({
        df_detect %>%
            filter("d" == input$effectsize, "nsims" == 10) %>%
            #filter(! method %in% c("svm", "tweedie")) %>%
            ggplot(aes(x=p, y=mean_nr, fill="detection")) +
            geom_col(position = "dodge", show.legend = F) +
            geom_errorbar(aes(ymin=mean_nr-1.96*mean_se, ymax=mean_nr+1.96*mean_se), width = 0.3) +
            scale_x_log10(breaks=c(100,300,1000,3000,1e4))  +
            facet_grid(detection~method, scales="free") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
