shinyUI(fluidPage(theme = shinytheme("cerulean"),
fluidPage(
  bootstrapPage('',
                
                navbarPage(title = 'SENTIMENTAPP'),
                
                tags$style(type = 'text/css', '.navbar { background-color: #1966CF;
                           font-family: Arial;
                           font-size: 13px;
                           color: #FF0000; }',
                           
                           '.navbar-dropdown { background-color: #1966CF;
                           font-family: Arial;
                           font-size: 13px;
                           color: #fff; }',
                           
                           '.navbar-default .navbar-brand {
                             color: #fff;
                           }'
                           
                )),
    strong("APP PARA EL ANALISIS DE SENTIMIENTOS SOBRE LOS TEMAS TENDENCIAS DE LA RED SOCIAL TWIITER.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
  br(), hr(),
  sidebarLayout(
    sidebarPanel(
      selectInput("hashtag", "Choose a book:",
                  choices = colombia),
      actionButton("update", "Procesar"),
      hr(),
      sliderInput("num_tweets",
                  "Numero de tweets:",
                  min = 100,  max = 1000, value = 200),
      hr(),
      sliderInput("freq",
                  "Frecuencia Minima:",
                  min = 1,  max = 50, value = 1),
      sliderInput("max",
                  "Max numero de palabras:",
                  min = 1,  max = 300,  value = 19)
    ),
    
    mainPanel(
      plotOutput("plot"),
      highchartOutput("pie")
    )
  )
)
)
)
