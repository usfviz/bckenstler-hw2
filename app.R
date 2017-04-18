library(shiny)
library(ggplot2)

life_exp <- read.csv('data/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip = 4, stringsAsFactors = FALSE)
life_exp <- life_exp[life_exp$Country.Code != "INX",]

fert_rates <- read.csv('data/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip = 4, stringsAsFactors = FALSE)
fert_rates <- fert_rates[fert_rates$Country.Code != "INX",]

meta <- read.csv('data/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')


ui <- fluidPage(
   
   titlePanel("Feritility Rates v. Life Expectancy"),
   
   fluidRow(
     sidebarLayout(
        sidebarPanel(
           sliderInput("year",
                       "Year",
                       min = 1960,
                       max = 2014,
                       value = 2014,
                       sep='',
                       animate = T
                          )
        ),
        
        mainPanel(
           plotOutput("scatterPlot",
                      hover = hoverOpts(
                        id = "plot_hover"),
                      click = "plot_click"
                      )
        )
     )
   ),
   fluidRow(
     column(width = 3,
            verbatimTextOutput("click_country")
     ),
     column(width = 3,
            verbatimTextOutput("hover_country")
     ),
     column(width = 6,
            radioButtons(
              inputId = 'region',
              label = 'Region',
              choices = c("All" = -1, 
                          "East Asia & Pacific" = "East Asia & Pacific", 
                          "Europe & Central Asia" = "Europe & Central Asia",
                          "Latin America & Caribbean" = "Latin America & Caribbean",
                          "Middle East & North Africa" = "Middle East & North Africa",
                          "North America" = "North America",
                          "South Asia" = "South Asia",
                          "Sub-Saharan Africa" = "Sub-Saharan Africa"
                          )
              )
        )
   )
)

server <- function(input, output) {
   
   output$scatterPlot <- renderPlot({
      year <- input$year
      region <- input$region
      if(region !=-1){
        x <- eval(parse(text=(paste('life_exp$X', year, sep = ''))))[meta$Region==region]
        y <- eval(parse(text=(paste('fert_rates$X', year, sep = ''))))[meta$Region==region]
        qplot(x, y, xlim = c(10, 90), ylim=c(0,9), xlab = "Life expectancy", ylab = "Fertility Rates")
      }
      else{
        x <- eval(parse(text=(paste('life_exp$X', year, sep = ''))))
        y <- eval(parse(text=(paste('fert_rates$X', year, sep = ''))))
        qplot(x, y, xlim = c(10, 90), ylim=c(0,9), xlab = "Life expectancy", ylab = "Fertility Rates", color = meta$Region)
      }
   })
   output$click_country <- renderPrint({
     cat("Nearest country to click:\n")
     x <- input$plot_click$x
     y <- input$plot_click$y
     region <- input$region
     year <- input$year
     if(region !=-1){
       le <- eval(parse(text=(paste('life_exp$X', year, sep = ''))))[meta$Region==region]
       fr <- eval(parse(text=(paste('fert_rates$X', year, sep = ''))))[meta$Region==region]
       life_exp$Country.Name[meta$Region==region][which.min(sqrt((le-x)**2 + (fr-y)**2))]
     }
     else{
       le <- eval(parse(text=(paste('life_exp$X', year, sep = ''))))
       fr <- eval(parse(text=(paste('fert_rates$X', year, sep = '')))) 
       life_exp$Country.Name[which.min(sqrt((le-x)**2 + (fr-y)**2))]
     }
   })
   output$hover_country <- renderPrint({
     cat("Nearest country to hover:\n")
     x <- input$plot_hover$x
     y <- input$plot_hover$y
     region <- input$region
     year <- input$year
     if(region !=-1){
       le <- eval(parse(text=(paste('life_exp$X', year, sep = ''))))[meta$Region==region]
       fr <- eval(parse(text=(paste('fert_rates$X', year, sep = ''))))[meta$Region==region]
       life_exp$Country.Name[meta$Region==region][which.min(sqrt((le-x)**2 + (fr-y)**2))]
     }
     else{
       le <- eval(parse(text=(paste('life_exp$X', year, sep = ''))))
       fr <- eval(parse(text=(paste('fert_rates$X', year, sep = '')))) 
       life_exp$Country.Name[which.min(sqrt((le-x)**2 + (fr-y)**2))]
     }
   })
}

shinyApp(ui = ui, server = server)

