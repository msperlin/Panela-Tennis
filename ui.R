library(shiny)
library(lubridate)
library(tidyverse)

my_plot_height <- '650px'
my_plot_width <- '750px'

httr::set_config(httr::config(http_version = 0))
df_games <- get_ss_data() %>%
  mutate(ref_year = year(ymd(`Data da Panela`)))

df_games <<- clean_up(df_games)

all_names <<- unique(c(df_games$`Integrante 01 | Dupla 01`,
                       df_games$`Integrante 02 | Dupla 01`,
                       df_games$`Integrante 01 | Dupla 02`,
                       df_games$`Integrante 02 | Dupla 02`))

all_years <- sort(unique(df_games$ref_year))

navbarPage("Panorama da Panela", #theme = shinytheme("united"),
           tabPanel("Atletas", 
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   selectInput("atleta", "Selecione o Atleta:",
                                               sort(all_names)),
                                   selectInput("ref_year", "Selecione o Ano:",
                                               sort(all_years), 
                                               selected = max(all_years)),
                                   p('Use esse ', a('formulário', href = 'https://forms.gle/BnciHbXBF8onFnMB7'),
                                     ' para alimentar o sistema.')),
                      mainPanel(
                        htmlOutput("summary_player"),
                        br(),
                        h3('Tabela de jogos'),
                        hr(),
                        br(),
                        DT::dataTableOutput('games_tbl')
                      )
                    )
           ),
           tabPanel("Número de Jogos",
                    fluidPage(fluidRow(plotOutput("n_games_plot", 
                                                  width = my_plot_width, 
                                                  height = my_plot_height) ) )
           ),
           tabPanel("Vitórias e Derrotas",
                    fluidPage(fluidRow(plotOutput("vit_derrot_plot", 
                                                  width = my_plot_width, 
                                                  height = my_plot_height) ) )
           ),
           tabPanel("Saldo de Sets",
                    fluidPage(fluidRow(plotOutput("net_sets_plot", 
                                                  width = my_plot_width, 
                                                  height = my_plot_height) ) )
           ),
           tabPanel("Saldo de Games",
                    fluidPage(fluidRow(plotOutput("net_games_plot", 
                                                  width = my_plot_width, 
                                                  height = my_plot_height) ) )
           ),
           tabPanel("Duplas Consagradas",
                    fluidPage(fluidRow(plotOutput("best_doubles", 
                                                  width = my_plot_width, 
                                                  height = my_plot_height) ) )
           ),
           tabPanel('Prêmios',
                    fluidPage(fluidRow(
                      h3('Tabela de prêmios'),
                      br(),
                      DT::dataTableOutput('premio_tbl'))))
           
)
