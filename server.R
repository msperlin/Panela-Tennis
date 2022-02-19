#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggimage)
library(DT)
library(tidyr)

shinyServer(function(input, output, session) {
  
  output$summary_player <- renderUI({
    df_summary <- get_games(name_in = input$atleta, df_games = clean_up(df_games) )
    
    #browser()
    tagList(h3('Estatísticas Globais'),
            hr(),
            br(),
            p(strong('Nome: '), input$atleta),
            p(strong('Número de jogos: '), df_summary$n_games),
            p(strong('Jogos'), '(vitórias | derrotas | líquido): ',
              df_summary$n_victory, ' | ', df_summary$n_losses, ' | ',
              df_summary$n_victory - df_summary$n_losses),
            p(strong('Sets'),  '(vencidos | perdidos | líquido): ',
              df_summary$sets_won, ' | ', df_summary$sets_lost, ' | ', 
              df_summary$sets_won - df_summary$sets_lost), 
            p(strong('Games'), '(vencidos | perdidos | líquido): ', 
              df_summary$games_won, ' | ', df_summary$games_lost, ' | ', 
              df_summary$games_won - df_summary$games_lost),
            br(),
            code('Dados atualizados em ', max(lubridate::ymd_hms(df_games$`Carimbo de data/hora`))),
            
    ) 
    
  })
  
  output$games_tbl <- renderDataTable({
    
    idx <- (df_games$`Integrante 01 | Dupla 01` == input$atleta)|
      (df_games$`Integrante 02 | Dupla 01` == input$atleta)|
      (df_games$`Integrante 01 | Dupla 02` == input$atleta)|
      (df_games$`Integrante 02 | Dupla 02` == input$atleta)
    
    
    df_temp <- df_games[idx, ] %>%
      select(-`Prêmio`) %>%
      filter(ref_year >= input$ref_year)
    
    
    datatable(df_temp)
  })
  
  output$premio_tbl <- renderDataTable({
    
    datatable(df_games %>% filter(!is.na(`Prêmio`)))
  })
  
  output$n_games_plot <- renderPlot({
    p <- do_n_games_plot(df_games, input)
    p
  })
  
  output$net_sets_plot <- renderPlot({
    p <- do_net_sets_plot(df_games, input)
    p
  })
  
  output$net_games_plot <- renderPlot({
    p <- do_net_games_plot(df_games, input)
    p
  })
  
  output$vit_derrot_plot <- renderPlot({
    
    p <- do_vic_loss_plot(df_games, input)
    p
    
  })
  
  output$best_doubles <- renderPlot({
    
    p <- do_best_doubles_plot()
    p
    
  })
  
})
