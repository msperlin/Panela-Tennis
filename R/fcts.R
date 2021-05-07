get_ss_data <- function() {
  require(googlesheets)
  
  my_ss <- gs_key('1WgWcxax8SlO7RhzjgJ01seWu-LFtFcZY7SZW3ywposg', visibility = 'public' )
  df <- gs_read(my_ss, col_types = readr::cols())
  
  return(df)
  
}

do_n_games_plot <- function(df_games, input) {
  require(ggimage)
  require(tidyverse)

  df_games <- df_games %>%
    filter(ref_year >= input$ref_year)
  
  tbl_games <- table(do.call(c, df_games[ , c(3,4,5, 6)]))
  
  df_tbl_games <- tibble(name = names(tbl_games),
                         n_games = as.numeric(tbl_games)) %>%
    arrange(-n_games) %>%
    mutate(image = 'figs/Wilson-US-Open-Tennis-Balls-4-Ball-Can-Ball-600x600.jpg')
  
  p <- ggplot(df_tbl_games, aes(x = n_games, y = reorder(name, n_games))) + 
    geom_point(size = 3) + 
    scale_x_continuous(breaks = 0:max(df_tbl_games$n_games)) + 
    labs(x = 'Número de Jogos', 
         y = '',
         title = paste0('Número de Jogos por Paneleiro (', input$ref_year, ')'),
         subtitle = paste0('Último jogo registrado em ', 
                           max(lubridate::dmy_hms(df_games$`Carimbo de data/hora` ))) ) + 
    theme_bw(base_size = 15) + geom_image(aes(image = image), size = 0.065)
  
  return(p)
}

do_net_games_plot <- function(df_games, input) {
  
  df_games <- df_games %>%
    filter(ref_year >= input$ref_year)
  
  df_net_points <- bind_rows(
    map(unique(c(df_games$`Integrante 01 | Dupla 01`,
                 df_games$`Integrante 02 | Dupla 01`,
                 df_games$`Integrante 01 | Dupla 02`,
                 df_games$`Integrante 02 | Dupla 02`)), .f = get_games, 
        df_games = df_games))
  
  p <- ggplot(df_net_points, aes(y = net_points, x = reorder(name, net_points)) ) + 
    geom_col() + coord_flip() + 
    scale_y_continuous(breaks = seq(min(df_net_points$net_points),
                                    max(df_net_points$net_points), 
                                    by = 10)) + 
    labs(x = '',
         y = 'Saldo de Games',
         title = paste0('Panorama do Saldo de Games (', input$ref_year, ')'),
         subtitle = paste0('Último jogo registrado em ', 
                           max(lubridate::dmy_hms(df_games$`Carimbo de data/hora`))) ) + 
    theme_bw(base_size = 15)
  
  return(p)
  
}

do_net_sets_plot <- function(df_games, input) {
  
  df_games <- df_games %>%
    filter(ref_year >= input$ref_year)
  
  df_net_points <- bind_rows(
    map(unique(c(df_games$`Integrante 01 | Dupla 01`,
                 df_games$`Integrante 02 | Dupla 01`,
                 df_games$`Integrante 01 | Dupla 02`,
                 df_games$`Integrante 02 | Dupla 02`)), .f = get_games, 
        df_games = df_games))
  
  p <- ggplot(df_net_points, aes(y = sets_net, x = reorder(name, sets_net)) ) + 
    geom_col() + coord_flip() + 
    scale_y_continuous(breaks = seq(min(df_net_points$sets_net),
                                    max(df_net_points$sets_net), 
                                    by = 5)) + 
    labs(x = '',
         y = 'Saldo de Sets',
         title = paste0('Panorama do Saldo de Sets (', input$ref_year, ')'),
         subtitle = paste0('Último jogo registrado em ', 
                           max(lubridate::dmy_hms(df_games$`Carimbo de data/hora`))) ) + 
    theme_bw(base_size = 15)
  
  return(p)
  
}

do_vic_loss_plot <- function(df_games, input) {
  
  df_games <- df_games %>%
    filter(ref_year >= input$ref_year)
  
  df_vic <- bind_rows(
    map(unique(c(df_games$`Integrante 01 | Dupla 01`,
                 df_games$`Integrante 02 | Dupla 01`,
                 df_games$`Integrante 01 | Dupla 02`,
                 df_games$`Integrante 02 | Dupla 02`)), .f = get_games, 
        df_games =  clean_up(df_games))) %>%
    mutate(perc_vic = n_victory/n_games )
  
  p <- ggplot(df_vic, aes(y = perc_vic, x = reorder(name, perc_vic)) ) + 
    geom_col() + coord_flip() + 
    #scale_y_continuous(breaks = seq(min(df_net_points$sets_net),
    #                               max(df_net_points$sets_net), 
    #                              by = 5)) + 
    labs(x = '',
         y = 'Percentagem de Vitórias',
         title = paste0('Panorama da Percentagem de Vitórias (', input$ref_year, ')'),
         subtitle = paste0('Último jogo registrado em ', 
                           max(lubridate::dmy_hms(df_games$`Carimbo de data/hora`))) ) + 
    theme_bw(base_size = 15) + 
    geom_text(aes(y = perc_vic, x = reorder(name, perc_vic),
                  label = paste0(n_victory, '|', n_games)), nudge_y = 0.05) + 
    scale_y_continuous(label = scales::percent)
  
  return(p)
  
}

do_best_doubles_plot <- function() {
  unique_doubles <- unique(c(df_games$name_double_1,
                             df_games$name_double_2))
  
  tbl_by_double <<-  bind_rows(map(unique_doubles, get_data_by_double ))
  
  p <- ggplot(data = tbl_by_double, aes(y = n_wins, 
                                        x = reorder(name_double, n_wins)) ) + 
    geom_col() + coord_flip() + 
    #scale_y_continuous(breaks = seq(min(df_net_points$sets_net),
    #                               max(df_net_points$sets_net), 
    #                              by = 5)) + 
    labs(x = '',
         y = 'Número de Vitórias',
         title = paste0('Vitórias por Dupla'),
         subtitle = paste0('Último jogo registrado em ', 
                           max(lubridate::dmy_hms(df_games$`Carimbo de data/hora`))) ) + 
    theme_bw(base_size = 15) + 
    geom_text(aes(y = n_wins, x = reorder(name_double, n_wins),
                  label = paste0(n_wins, '|', n_games,
                                 ' (', scales::percent(perc_win), ')')), 
              nudge_y = 0.25, hjust = 0) 
  
  #p
  return(p)
  
}


get_games <- function(name_in, df_games) {
  
  require(tibble)
  
  # double 01
  #browser()
  idx_1 <- (df_games$`Integrante 01 | Dupla 01` == name_in) |
    (df_games$`Integrante 02 | Dupla 01` == name_in)
  
  points_won_01 <- sum(df_games$`Resultado Set 01 [Dupla 01]`[idx_1], na.rm = TRUE) +
    sum(df_games$`Resultado Set 02 [Dupla 01]`[idx_1], na.rm = TRUE) +
    sum(df_games$`Resultado Set 03 [Dupla 01]`[idx_1], na.rm = TRUE)
  points_lost_01 <- sum(df_games$`Resultado Set 01 [Dupla 02]`[idx_1], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 02 [Dupla 02]`[idx_1], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 03 [Dupla 02]`[idx_1], na.rm = TRUE)
  
  sets_won_01 <- sum(df_games$`Resultado Set 01 [Dupla 01]`[idx_1] > 
                       df_games$`Resultado Set 01 [Dupla 02]`[idx_1], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 02 [Dupla 01]`[idx_1] > 
          df_games$`Resultado Set 02 [Dupla 02]`[idx_1], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 03 [Dupla 01]`[idx_1] > 
          df_games$`Resultado Set 03 [Dupla 02]`[idx_1], na.rm = TRUE)  
  
  sets_lost_01 <- sum(df_games$`Resultado Set 01 [Dupla 01]`[idx_1] < 
                        df_games$`Resultado Set 01 [Dupla 02]`[idx_1], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 02 [Dupla 01]`[idx_1] < 
          df_games$`Resultado Set 02 [Dupla 02]`[idx_1], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 03 [Dupla 01]`[idx_1] < 
          df_games$`Resultado Set 03 [Dupla 02]`[idx_1], na.rm = TRUE)
  
  vec_victories_set_1 <- df_games$`Resultado Set 01 [Dupla 01]`[idx_1] > 
    df_games$`Resultado Set 01 [Dupla 02]`[idx_1]
  
  vec_victories_set_2 <- df_games$`Resultado Set 02 [Dupla 01]`[idx_1] > 
    df_games$`Resultado Set 02 [Dupla 02]`[idx_1]
  
  vec_victories_set_3 <- df_games$`Resultado Set 03 [Dupla 01]`[idx_1] > 
    df_games$`Resultado Set 03 [Dupla 02]`[idx_1]
  
  n_sets <- any( (df_games$`Resultado Set 01 [Dupla 01]`[idx_1] != 0) | 
                   (df_games$`Resultado Set 01 [Dupla 02]`[idx_1] != 0) ) + 
    any( (df_games$`Resultado Set 02 [Dupla 01]`[idx_1] != 0) | 
           (df_games$`Resultado Set 02 [Dupla 02]`[idx_1] != 0) ) + 
    any( (df_games$`Resultado Set 03 [Dupla 01]`[idx_1] != 0) | 
           (df_games$`Resultado Set 03 [Dupla 02]`[idx_1] != 0) )
  
  
  
  vec_victories_set <- vec_victories_set_1 + vec_victories_set_2 + vec_victories_set_3
  
  n_victory_01 <- sum(vec_victories_set >= 2)
  n_loss_01 <- length(vec_victories_set) - n_victory_01
  
  # double 02
  idx_2 <- (df_games$`Integrante 01 | Dupla 02` == name_in) |
    (df_games$`Integrante 02 | Dupla 02` == name_in)
  
  points_won_02 <- sum(df_games$`Resultado Set 01 [Dupla 02]`[idx_2], na.rm = TRUE) +
    sum(df_games$`Resultado Set 02 [Dupla 02]`[idx_2], na.rm = TRUE) +
    sum(df_games$`Resultado Set 03 [Dupla 02]`[idx_2], na.rm = TRUE)
  points_lost_02 <- sum(df_games$`Resultado Set 01 [Dupla 01]`[idx_2], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 02 [Dupla 01]`[idx_2], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 03 [Dupla 01]`[idx_2], na.rm = TRUE)
  
  sets_won_02 <- sum(df_games$`Resultado Set 01 [Dupla 02]`[idx_2] > 
                       df_games$`Resultado Set 01 [Dupla 01]`[idx_2], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 02 [Dupla 02]`[idx_2] > 
          df_games$`Resultado Set 02 [Dupla 01]`[idx_2], na.rm = TRUE) +
    sum(df_games$`Resultado Set 03 [Dupla 02]`[idx_2] >
          df_games$`Resultado Set 03 [Dupla 01]`[idx_2], na.rm = TRUE)
  
  sets_lost_02 <- sum(df_games$`Resultado Set 01 [Dupla 02]`[idx_2] < 
                        df_games$`Resultado Set 01 [Dupla 01]`[idx_2], na.rm = TRUE) + 
    sum(df_games$`Resultado Set 02 [Dupla 02]`[idx_2] < 
          df_games$`Resultado Set 02 [Dupla 01]`[idx_2], na.rm = TRUE) +
    sum(df_games$`Resultado Set 03 [Dupla 02]`[idx_2] <
          df_games$`Resultado Set 03 [Dupla 01]`[idx_2], na.rm = TRUE)
  
  # victories
  vec_victories_set_1 <- df_games$`Resultado Set 01 [Dupla 02]`[idx_2] > 
    df_games$`Resultado Set 01 [Dupla 01]`[idx_2]
  
  vec_victories_set_2 <- df_games$`Resultado Set 02 [Dupla 02]`[idx_2] > 
    df_games$`Resultado Set 02 [Dupla 01]`[idx_2]
  
  vec_victories_set_3 <- df_games$`Resultado Set 03 [Dupla 02]`[idx_2] > 
    df_games$`Resultado Set 03 [Dupla 01]`[idx_2]
  
  vec_victories_set <- vec_victories_set_1 + vec_victories_set_2 + vec_victories_set_3
  
  n_victory_02 <- sum(vec_victories_set >= 2)
  n_loss_02 <- length(vec_victories_set) - n_victory_02
  
  df_out <- tibble(name = name_in,
                   n_victory = n_victory_01 +  n_victory_02,
                   n_losses = n_loss_01 +  n_loss_02,
                   games_won = points_won_01 + points_won_02,
                   games_lost = points_lost_01 + points_lost_02,
                   sets_won = sets_won_01 + sets_won_02,
                   sets_lost = sets_lost_01 + sets_lost_02,
                   sets_net = sets_won - sets_lost,
                   n_games = sum(idx_1) + sum(idx_2),
                   net_points = games_won - games_lost,
                   avg_net_pts = net_points/n_games)
  
  return(df_out)
}

clean_up <- function(df_games) {
  
  my_l <- list("Resultado Set 01 [Dupla 01]" = 0,
               "Resultado Set 01 [Dupla 02]" = 0,
               "Resultado Set 02 [Dupla 01]" = 0,
               "Resultado Set 02 [Dupla 02]" = 0,
               "Resultado Set 03 [Dupla 01]" = 0,
               "Resultado Set 03 [Dupla 02]" = 0)
  
  df_games <- tidyr::replace_na(df_games, replace = my_l) 
  
  # sort names out
  name_double_1 = as.character(
    map2(df_games$`Integrante 01 | Dupla 01`, df_games$`Integrante 02 | Dupla 01`,
         get_double_name)
  )
  
  name_double_2 = as.character(
    map2(df_games$`Integrante 01 | Dupla 02`, df_games$`Integrante 02 | Dupla 02`,
         get_double_name)
  )
  
  df_games$name_double_1 <- name_double_1
  df_games$name_double_2 <- name_double_2
  
  return(df_games)
}

get_double_name <- function(name_1, name_2) {
  
  name_out <- paste0(sort(c(name_1, name_2)), collapse = ' & ')
  return(name_out)
  
}

get_data_by_double <- function(double_in) {
  
  double_df1 <- df_games %>%
    filter(name_double_1 == double_in)
  
  vec_victories_set_1 <- double_df1$`Resultado Set 01 [Dupla 01]` > 
    double_df1$`Resultado Set 01 [Dupla 02]`
  
  vec_victories_set_2 <- double_df1$`Resultado Set 02 [Dupla 01]` > 
    double_df1$`Resultado Set 02 [Dupla 02]`
  
  vec_victories_set_3 <- double_df1$`Resultado Set 03 [Dupla 01]` > 
    double_df1$`Resultado Set 03 [Dupla 02]`
  
  vec_victories_set <- vec_victories_set_1 + vec_victories_set_2 + vec_victories_set_3
  
  n_victory_01 <- sum(vec_victories_set >= 2)
  
  ## double 2
  double_df2 <- df_games %>%
    filter(name_double_2 == double_in)
  
  vec_victories_set_1 <- double_df2$`Resultado Set 01 [Dupla 02]` > 
    double_df2$`Resultado Set 01 [Dupla 01]`
  
  vec_victories_set_2 <- double_df2$`Resultado Set 02 [Dupla 02]` > 
    double_df2$`Resultado Set 02 [Dupla 01]`
  
  vec_victories_set_3 <- double_df1$`Resultado Set 03 [Dupla 02]` > 
    double_df1$`Resultado Set 03 [Dupla 01]`
  
  vec_victories_set <- vec_victories_set_1 + vec_victories_set_2 + vec_victories_set_3
  
  n_victory_02 <- sum(vec_victories_set >= 2)
  
  n_wins = n_victory_01 + n_victory_02
  n_games = nrow(double_df1) + nrow(double_df2)
  
  out_df <- tibble(name_double = double_in,
                   n_wins = n_wins,
                   n_games = n_games,
                   perc_win = n_wins/n_games)
  
  return(out_df)
}