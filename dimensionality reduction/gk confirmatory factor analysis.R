setwd("C:/Users/Luca/Desktop/Python/Tesi")
library(lavaan)
library(dplyr)
library(ggplot2)

df = read.csv('gk_preprocc.csv')
nomi <- df$player
df$player <- NULL
rownames(df) = nomi


modelspec <- '
  Tradizionale =~ gk_passes_length_avg + gk_goal_kicks_per90 + gk_goal_kick_length_avg 
  Sweeper_Keeper =~ gk_passes_per90 + gk_def_actions_outside_pen_area_per90 + gk_avg_distance_def_actions + gk_goal_kicks_per90 
  Miracolo =~ gk_pens_save_pct + xg_saved_per90  
  Sotto_Pressione =~ gk_goals_against + gk_saves_per90 + gk_clean_sheets + gk_corner_kick_goals_against_perGame
  Difesa_di_Area =~ gk_passes_throws_per90 + gk_crosses_stopped_pct
  Sweeper_Keeper ~~ 1*Sweeper_Keeper
  Miracolo ~~ 1*Miracolo
  Difesa_di_Area ~~ 1*Difesa_di_Area
  Tradizionale ~~ 1*Tradizionale
  Sotto_Pressione ~~ 1*Sotto_Pressione 
  Tradizionale ~~ 0*Miracolo 
  Sweeper_Keeper ~~ 0*Miracolo
  Sweeper_Keeper ~~ 0*Sotto_Pressione
  Difesa_di_Area ~~ 0*Sotto_Pressione
'
fit <- cfa(model = modelspec, data = df, estimator = "MLM")
summary(fit, fit.measures = TRUE, standardized = TRUE)

mod_indices <- modindices(fit)
ordered_mods <- mod_indices[order(-mod_indices$mi), ]
print(ordered_mods)

modelspec2 <- '
  Tradizionale =~ gk_passes_length_avg + gk_goal_kicks_per90 + gk_goal_kick_length_avg + gk_avg_distance_def_actions
  Sweeper_Keeper =~ gk_passes_per90 + gk_def_actions_outside_pen_area_per90 + gk_avg_distance_def_actions + gk_goal_kicks_per90 + gk_crosses_stopped_pct + gk_passes_throws_per90
  Miracolo =~ xg_saved_per90 + gk_saves_per90 + gk_pens_save_pct + gk_goals_against
  Sotto_Pressione =~ gk_goals_against + gk_saves_per90 + gk_clean_sheets + gk_corner_kick_goals_against_perGame + gk_goals_against + gk_goal_kicks_per90
  Difesa_di_Area =~ NA*gk_passes_throws_per90 + gk_crosses_stopped_pct + gk_goal_kicks_per90 + gk_saves_per90
  Sweeper_Keeper ~~ 1*Sweeper_Keeper
  Miracolo ~~ 1*Miracolo
  Difesa_di_Area ~~ 1*Difesa_di_Area
  Tradizionale ~~ 1*Tradizionale
  Sotto_Pressione ~~ 1*Sotto_Pressione 
  Tradizionale ~~ 0*Miracolo 
  Sweeper_Keeper ~~ 0*Miracolo
  Sweeper_Keeper ~~ 0*Sotto_Pressione
  Sweeper_Keeper ~~ 0*Difesa_di_Area
  Difesa_di_Area ~~ 0*Sotto_Pressione
  Miracolo ~~ 0*Difesa_di_Area
  gk_passes_length_avg ~~ gk_goal_kick_length_avg
  gk_def_actions_outside_pen_area_per90 ~~ gk_avg_distance_def_actions
  gk_def_actions_outside_pen_area_per90 ~~ gk_crosses_stopped_pct
  gk_crosses_stopped_pct ~~ gk_passes_throws_per90
'

fit <- cfa(model = modelspec2, data = df, estimator = "MLM")
summary(fit, fit.measures = TRUE, standardized = TRUE)

used <- c('gk_passes_length_avg', 'gk_goal_kicks_per90', 'gk_goal_kick_length_avg',
          'gk_avg_distance_def_actions', 'gk_passes_per90', 'gk_def_actions_outside_pen_area_per90',
          'gk_crosses_stopped_pct', 'xg_saved_per90', 'gk_saves_per90', 'gk_pens_save_pct',
          'gk_goals_against', 'gk_clean_sheets', 'gk_corner_kick_goals_against_perGame')
factor_scores <- lavPredict(fit)
factor_scores = as.data.frame(factor_scores)
rownames(factor_scores) = nomi
cor(factor_scores)
lavInspect(fit, 'cor.lv')
inspect(fit, "std.all")$lambda
cor(df[used], factor_scores)

factor_scores <- factor_scores %>%
  mutate(across(everything(), ~ ntile(., 10), .names = "decile_{col}"))

factor_scores <- factor_scores %>%
  mutate(label = rownames(factor_scores))

factor_names <- c('Tradizionale', 'Sweeper_Keeper', 'Miracolo', 'Sotto_Pressione', 'Difesa_di_Area')
plot_factor_pairs <- function(df, x_factor, y_factor) {
  # Calcola i decili per i fattori specifici
  df <- df %>%
    mutate(
      decile_x = ntile(get(x_factor), 10),
      decile_y = ntile(get(y_factor), 10)
    )
  
  # Crea una colonna di etichette per i punti nei primi e ultimi decili specifici
  df <- df %>%
    mutate(
      label = ifelse(
        (decile_x == 1 | decile_x == 10) | (decile_y == 1 | decile_y == 10),
        as.character(label),
        NA
      )
    )
  
  # Crea lo scatterplot etichettando i punti nei decili estremi con il proprio indice
  p <- ggplot(df, aes_string(x = x_factor, y = y_factor)) +
  geom_point() +  # Aggiungi i punti
  geom_text(aes(label = label), hjust = 0, vjust = 0, size = 3, color = "red") +  # Etichette per i decili estremi
  labs(x = x_factor, y = y_factor, title = paste("Scatterplot:", x_factor, "vs", y_factor)) +
  theme_minimal()
  
  print(p)
}  

for (i in 1:(length(factor_names) - 1)) {
  for (j in (i + 1):length(factor_names)) {
    x_factor <- factor_names[i]
    y_factor <- factor_names[j]
    plot_factor_pairs(factor_scores, x_factor, y_factor)
  }
}

