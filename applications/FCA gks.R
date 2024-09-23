setwd("C:/Users/Luca/Desktop/Python/Tesi")
library(fcaR)

df <- read.csv('gk_concepts.csv')
nomi <- df$player
index <- c('age', 'nationality', 'position', 'team', 'minutes_90s', 'top5')
df$player <- NULL
rownames(df) = nomi

for (col in setdiff(colnames(df), index)){
  df[col] = ifelse(df[col] == 'True', TRUE, FALSE)
}
df_fc <- df[, !names(df) %in% index]

fc <- FormalContext$new(df_fc)
fc

S <- Set$new(attributes = fc$objects)
S$assign('Marco Carnesecchi' = 1)
fc$intent(S)
S$assign('Juan Musso' = 1, 'Rui Patrício' = 1)
i <- fc$intent(S)
i

e <- fc$extent(i)
e

Si <- Set$new(attributes = fc$attributes)
Si$assign('Sweeper_Keeper...High' = 1, 'Miracolo...High' = 1)
fc$extent(Si)
Si$assign('Tradizionale...Low' = 1)
fc$extent(Si)


fc$reduce(TRUE)

fc$find_concepts()
fc$concepts$size()

fc$concepts$join_irreducibles()
fc$concepts$meet_irreducibles()

serie_a <- c('Atalanta', 'Bologna', 'Cagliari', 'Empoli', 'Fiorentina',
             'Frosinone', 'Genoa', 'Hellas Verona', 'Inter', 'Juventus',
             'Lazio', 'Lecce', 'Milan', 'Monza', 'Napoli', 'Roma',
             'Salernitana', 'Sassuolo', 'Torino', 'Udinese')
fc_seriea <- FormalContext$new(df[df$team %in% serie_a, !names(df) %in% index])
fc_seriea

fc_seriea$find_concepts()
fc_seriea$concepts$size()
fc_seriea$concepts$plot()

fcs_dual <- fc_seriea$dual()
fcs_dual$find_concepts()
fcs_dual$concepts$plot()
