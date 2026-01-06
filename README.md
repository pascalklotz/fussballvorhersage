# Fussballvorhersage

## Beschreibung
Berechne eine Prognose des kommenden Spieltags der 1. Herren-Bundesliga im Fußball. Es wird die Tordifferenz aus Sicht der Heimmannschaft vorhergesagt (weitere Infos siehe unten). Die jeweiligen Tipps beruhen auf den 5 vorher stattgefundenen Spielen der Heim- & Auswärtsmannschaft. Die Skripte sind in R mit der dplyr Schreibweise aufgebaut. Die vorhergesagte Tendenz wird für das Tippspiel des Podcasts Rasenfunk verwendet: https://www.kicktipp.de/rasenfunk3/.


## Wichtig zu beachten
privates Projekt; kein Gewinn & aus Interesse; keine Garantie für Tipps; Keine Motivation zu wetten

## Ordnerstruktur
raw: Grundlegende .csv-Dateien wie historische Daten der vergangenen Saison \
data: Verarbeitete Daten \
src: .R-Dateien


## src Erklärungen
### dataloader2.R:
- lade aktuellen Spieltag von "https://www.google.com/calendar/ical/spielplan.1.bundesliga%40gmail.com/public/basic.ics"
    - ermöglicht automatische Updates
- lade Spieldaten von "http://www.football-data.co.uk/mmz4281/2122/D1.csv"
    - enthält Begegnungen und grundlegende Variablen
- dataloader2.R: 
    - lade deutlich mehr Variablen von https://fbref.com/
    - gezielte Variablen für Shooting, Goalkeeping, Passing, Pass Type, Goal and Shot Creation, Defensive Actions, Possession
- Spieldaten seit der Saison 2018/2019 in Ordner raw
- Durchschnitt je Team als Median für fehlende Werte (wichtig für die ersten 5 Spiele der Saison)
- erzeuge Spielid mit Spieltag für spätere Joins
- Berechne die vereinfachte Tabellenposition je Spiel nur anhand von Sieg / Unentschieden / Niederlage


### modeltuning.R
- 80% Trainingsdaten
    - großzügiges Training, da Test nur leichte Kontrolle
- preProcess method = "range" auf Trainingsdaten

tune_control <- caret::trainControl(
  method = "repeatedcv", # cross-validation
  repeats = 3,
  number = 10, # with n folds 
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results ,
)

tune_grid = expand.grid(nrounds = c(50,100),
                        max_depth = c(2,3,4),
                        eta = c(0.2,0.6,1),
                        min_child_weight = c(1),
                        subsample = c(0.4,0.7),
                        gamma = c(0.2,0.6),
                        colsample_bytree = c(0.3,0.4)


model <- caret::train(
  x = X_train,
  y = y_train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  metric = "RMSE"
)

    - "xgbTree" als stärkstes Modell
    - Regression wegen Tordifferenz
    

