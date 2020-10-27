function(input, output, session) {
  sentimientos <- reactive({
    input$update
    isolate({
      withProgress({
        
        hashtag <- input$hashtag
        num_tweets <- input$num_tweets
        setProgress(message = "Procesando tweets...")
        tweet_df <- search_tweets(hashtag, n = num_tweets,lang="es",
                                  include_rts = FALSE)
        texto <- tweet_df$text
        v$texto = texto
        tweetResultado <- as.data.frame(sentimientosScore(texto, mNeg, neg, pos, mPos))
        tweetResultado <- cbind(tweetResultado,0,'Neutro')
        names(tweetResultado)<- c('texto',"muyNeg",'Neg','muyPos','Pos','Valor', 'Sentimiento')
        tweetResultado$Sentimiento <- as.character(tweetResultado$Sentimiento)
        tweetResultado$Valor <- (-5*as.numeric(tweetResultado$muyNeg) -2.5*as.numeric(tweetResultado$Neg) + 2.5*as.numeric(tweetResultado$Pos) +5*as.numeric(tweetResultado$muyPos))/(as.numeric(tweetResultado$muyNeg)+as.numeric(tweetResultado$Neg)+as.numeric(tweetResultado$muyPos)+as.numeric(tweetResultado$Pos))
        tweetResultado$Valor[is.nan(tweetResultado$Valor)] <- 0
        tweetResultado$Sentimiento[tweetResultado$Valor < -1] <- 'Muy Negativo'
        tweetResultado$Sentimiento[(tweetResultado$Valor >= -1) & (tweetResultado$Valor < 0)] <- 'Negativo'
        tweetResultado$Sentimiento[(tweetResultado$Valor <= 1) & (tweetResultado$Valor > 0)] <- 'Positivo'
        tweetResultado$Sentimiento[tweetResultado$Valor > 1] <- 'Muy Positivo'
        as.data.frame(table(tweetResultado$Sentimiento))
        conteo <- as.data.frame(table(tweetResultado$Sentimiento))
        valor =conteo$Freq[4]
        valor = valor/4
        valor = valor-73
        valor =floor(valor)
        conteo =conteo[conteo$Var!='Neutro',]
        
      })
    })
  })
  
 
 
  output$pie <- renderHighchart({
    v <- sentimientos()
    hc_pie <- highchart(width = 400, height = 400) %>%
      hc_title(text = "Sentimiento de polaridad") %>%
      hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45, beta =0)) %>%
      hc_plotOptions(pie = list(depth = 40)) %>%
      hc_add_series_labels_values(v$Var1, v$Freq+valor)
    
    hc_pie
  })
  terms <- reactive({
    
    withProgress({
      setProgress(message = "Procesando corpus...")
      getTermMatrix(v$texto)
    })
    
  })
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    x <- terms()
    text(x=0.5, y=0.5, "Nube de palabras")
    wordcloud_rep(names(x), x, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                 random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
  })

}


