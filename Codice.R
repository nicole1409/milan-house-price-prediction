library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(mice)
library(httr)
library(jsonlite)
library(geosphere)
library(caret)


training <- read_csv("training.csv")
test <- read_csv("test.csv")
test$selling_price <- NA  # Aggiungo la colonna selling_price a test

data <- rbind(training, test)
skimr::skim(data)
summary(data)

## variabili square_meters, rooms_number e bathrooms_number
skimr::skim(data$square_meters) #ci sono 0 missing ma valori anomali (min=1)
data[which(data$square_meters < 20), c("square_meters", "rooms_number", "bathrooms_number")] #controllo i valori anomali
data$square_meters[data$square_meters < 20 & (data$rooms_number %in% c("2", "3", "4", "5", "5+") |
                     data$bathrooms_number %in% c("2", "3", "3+"))] <- NA

data[which(data$square_meters > 300), c("square_meters", "rooms_number", "bathrooms_number")]
data$square_meters[data$square_meters > 300 & (data$rooms_number %in% c("1",  "2", "3") |
                     data$bathrooms_number %in% c("1"))] <- NA

skimr::skim(data$rooms_number) #ci sono 0 missinig
unique(data$rooms_number) 

skimr::skim(data$bathrooms_number) #ci sono 39 missing
unique(data$bathrooms_number)


#imputo gli NA di square_meters con MICE rispetto alle variabili rooms_number e bathrooms_number
df_imp <- data[, c("square_meters", "rooms_number", "bathrooms_number")]
df_imp$rooms_number <- as.factor(df_imp$rooms_number)
df_imp$bathrooms_number <- as.factor(df_imp$bathrooms_number)
str(df_imp)

set.seed(123)
imp <- mice(df_imp, 
            m = 1,           
            method = "pmm",  
            maxit = 20, 
            printFlag = TRUE)
df_complete <- complete(imp)
data$square_meters <- df_complete$square_meters

# imputo bathrooms_number
data$bathrooms_number[data$bathrooms_number == "3+"] <- NA
data$bathrooms_number <- as.numeric(data$bathrooms_number)
skimr::skim(data$bathrooms_number)
mod_bath <- lm(bathrooms_number ~ square_meters + rooms_number,
               data = data[!is.na(data$bathrooms_number), ])
summary(mod_bath)

idx_na_bath <- which(is.na(data$bathrooms_number))
data$bathrooms_number[idx_na_bath] <- round(predict(mod_bath, newdata = data[idx_na_bath, ]))
summary(data$bathrooms_number)

# imputo rooms_number
data$rooms_number[data$rooms_number == "5+"] <- NA
data$rooms_number <- as.numeric(data$rooms_number)
skimr::skim(data$rooms_number)

mod_rooms <- lm(rooms_number ~ square_meters + bathrooms_number,
                data = data[!is.na(data$rooms_number), ])
summary(mod_rooms)

idx_na_rooms <- which(is.na(data$rooms_number))
data$rooms_number[idx_na_rooms] <- round(predict(mod_rooms, newdata = data[idx_na_rooms, ]))
summary(data$rooms_number)

## variabili total_floors_in_building, floor e lift
table(data$total_floors_in_building, useNA = "always") #ci sono 111 NA
data$total_floors_in_building[data$total_floors_in_building == "1 floor"] <- "1"
data$total_floors_in_building <- as.numeric(data$total_floors_in_building)

table(data$floor, useNA = "always") #non ci sono NA
data$floor[data$floor == "ground floor"] <- "0"
data$floor[data$floor == "semi-basement"] <- "-1"
data$floor[data$floor == "mezzanine"] <- "0.5"
data$floor <- as.numeric(data$floor)
#verifico che il piano sia minore o uguale al numero di piani totali
check <- data$floor <= data$total_floors_in_building
sum(!check, na.rm = TRUE)
data[!check & !is.na(check), c("floor", "total_floors_in_building")]
print(data[which(data$floor > data$total_floors_in_building | is.na(data$total_floors_in_building) ), c("total_floors_in_building", "floor")], n = Inf)
#metto NA dove il piano è maggiore del numero di piani totali
data$total_floors_in_building[data$floor > data$total_floors_in_building] <- NA
sum(data$floor > data$total_floors_in_building, na.rm = TRUE) 
#dove c'è NA in total_floors_in_building e il floor è <= 1 metto 1 come total_floors_in_building
data$total_floors_in_building[is.na(data$total_floors_in_building) & data$floor <= 1] <- 1
#assegno come valore del numero di piani totali il piano dell'appartamento
data$total_floors_in_building[is.na(data$total_floors_in_building)] <- 
  data$floor[is.na(data$total_floors_in_building)]
sum(is.na(data$total_floors_in_building))  

table(data$lift, useNA = "always") #ci sono 231
# Distribuzione dell’ascensore per numero di piani
prop.table(table(data$total_floors_in_building, data$lift), 1)
# edifici bassi: ascensore raro
data$lift[is.na(data$lift) & data$total_floors_in_building <= 2] <- "no"
# edifici alti: ascensore quasi certo
data$lift[is.na(data$lift) & data$total_floors_in_building >= 5] <- "yes"
# per i piani 3 e 4: imputazione probabilistica coerente con la distribuzione osservata
set.seed(123)
idx_mid <- which(is.na(data$lift) & data$total_floors_in_building %in% 3:4)
prob_yes <- ifelse(data$total_floors_in_building[idx_mid] == 3, 0.55, 0.70)
data$lift[idx_mid] <- ifelse(runif(length(idx_mid)) < prob_yes, "yes", "no")
table(data$lift, useNA = "always")
data$lift[data$lift == "no"] <- 0
data$lift[data$lift == "yes"] <- 1
data$lift <- as.numeric(data$lift)






## car_parking
table(data$car_parking, useNA = "always") # 0 NA
#categorie: 1 garage, 2 o più garage, shared parking, no 
data$car_parking <- as.character(data$car_parking)
data$parking_category <- case_when(
  data$car_parking == "no" ~ "no",
  
  # Tutto ciò che contiene "1 in garage/box" (anche con shared parking)
  grepl("^1 in garage/box", data$car_parking) ~ "1_garage",
  
  # Tutto ciò che inizia con "2 in garage/box", "3 in garage/box", ecc.
  grepl("^[2-9]+ in garage/box", data$car_parking) ~ "2plus_garage",
  
  # Solo shared parking, senza menzione del garage
  grepl("shared parking", data$car_parking) & !grepl("garage/box", data$car_parking) ~ "shared_parking",
  
  TRUE ~ NA_character_
)
table(data$parking_category, useNA = "always")
data$car_parking <- data$parking_category
data$car_parking <- as.character(data$car_parking)
data$parking_category <- NULL

## availability
table(data$availability, useNA = "always") #ci sono 1680 NA

oggi <- as.Date("2025-11-18", format = "%Y-%m-%d")

# Creazione della variabile availability_clean
data <- data %>%
  mutate(
    availability_clean = case_when(
      # Categoria "available" se è esplicitamente disponibile
      availability == "available" ~ "available",
      
      # Se contiene una data, estrai la data e confronta
      grepl("^available from", availability) ~ if_else(
        dmy(str_extract(availability, "\\d{2}/\\d{2}/\\d{4}")) <= oggi,
        "available", "not available"
      ),
      
      # NA o altro → "available"
      TRUE ~ "available"
    )
  )
# Tabella di riepilogo
table(data$availability_clean, useNA = "always")
# Aggiorno la colonna originale
data$availability <- data$availability_clean
data$availability_clean <- NULL


# 1. Sostituiamo gli NA nella colonna other_features con stringa vuota
data$other_features[is.na(data$other_features)] <- ""

# 2. Creiamo un identificativo temporaneo per ogni riga
data <- data %>% mutate(id = row_number())

# 3. Espandiamo le stringhe in righe separate
data_expanded <- data %>%
  separate_rows(other_features, sep = "\\|") %>%
  mutate(other_features = str_trim(other_features))  # Rimuove spazi bianchi

# 4. Creiamo le variabili dummy (1 se presente, 0 se assente)
data_dummies <- data_expanded %>%
  filter(other_features != "") %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = other_features, values_from = value, values_fill = 0)

# 5. Ricolleghiamo le dummy al dataframe originale
data_final <- data %>%
  left_join(data_dummies, by = "id") %>%
  select(-id)  # Rimuoviamo la colonna temporanea ID

# 6. (Opzionale) Visualizziamo un riassunto
skimr::skim(data_final)

# Sostituisci NA con stringa vuota per evitare errori
data$other_features[is.na(data$other_features)] <- ""

# Dummy per vetri singoli
data$single_glass_dummies <- ifelse(grepl("window frames in glass", data$other_features), 1, 0)
sum(data$single_glass_dummies, na.rm = TRUE)

# Dummy per doppi vetri
data$double_glass_dummies <- ifelse(grepl("window frames in double glass", data$other_features), 1, 0)
sum(data$double_glass_dummies, na.rm = TRUE)

# Dummy per tripli vetri
data$triple_glass_dummies <- ifelse(grepl("window frames in triple glass", data$other_features), 1, 0)
sum(data$triple_glass_dummies, na.rm = TRUE)

# Dummy per finestre in pvc
data$window_pvc<-  ifelse(grepl("pvc", data$other_features), 1, 0)
#Dummy per finestre in legno
data$window_wood<- ifelse(grepl("wood", data$other_features), 1, 0)
#Dummy per finestre in metallo
data$window_metal<- ifelse(grepl("metal", data$other_features), 1, 0)
  
#Dummy per attico
data$attic_dummy <- data_final$attic

#Dummy per la cabina armadio
data$closet_dummy <- data_final$closet

#Dummy per la cantina
data$cellar_dummy <- data_final$cellar

#Dummy per l'esposizione interna
data$internal_exposure_dummy <- data_final$`internal exposure`

#Dummy per l'esposizione esterna
data$external_exposure_dummy <- data_final$`external exposure`

#Dummy per la doppia esposizione
data$double_exposure_dummy <- data_final$`double exposure`

#Dummy per le esposizioni 
data$exposure_north <- ifelse(grepl("north", data$other_features, ignore.case = TRUE), 1, 0)
data$exposure_south <- ifelse(grepl("south", data$other_features, ignore.case = TRUE), 1, 0)
data$exposure_east  <- ifelse(grepl("east",  data$other_features, ignore.case = TRUE), 1, 0)
data$exposure_west  <- ifelse(grepl("west",  data$other_features, ignore.case = TRUE), 1, 0)

#Dummy per la terrazza
data$terrace_dummy <- data_final$terrace

#Dummy per il giardino
data$garden_dummy <- data_final$`private garden`

#Dummy per l'idromassaggio e il camino
data$luxury_dummy_in_home <- data_final$hydromassage+ data_final$fireplace

#Dummy per piscina e tennis court
data$luxury_dummy_in_complex <- data_final$pool + data_final$`tennis court`

#Dummy cancello elettrico
data$electric_gate_dummy <- data_final$`electric gate`

#Dummy  tv system with satellite dish 
data$satellite_dish_dummy <- data_final$`tv system with satellite dish`

#Dummy per la taverna
data$tavern_dummy <- data_final$tavern

#Dummy per la fibra ottica
data$optic_fiber_dummy <- data_final$`optic fiber`

#Dummy per la porta blindata
data$security_door_dummy <- data_final$`security door`

#Dummy per l'allarme
data$alarm_dummy <- data_final$'alarm system'

#Dummy videocitofono
data$videophone_dummy <- data_final$'video entryphone'

#Dummy per il balcone 
data$balcony_dummy <- ifelse(grepl("balcony|balconies", data$other_features, ignore.case = TRUE), 1, 0)

#Dummy per full day concierge
data$full_day_concierge <- data_final$`full day concierge`
#Dummy per part-time concierge
data$part_time_concierge <- data_final$`half-day concierge`
#Dummy receptionist
data$receptionist <- data_final$reception

#Dummy per il giardino
data$private_garden_dummy <- ifelse(data_final$`private garden` + data_final$`private and shared garden` > 0, 1, 0)
data$shared_garden_dummy <- data_final$`shared garden`
#Dummy per parzialmento fornito
data$partially_furnished_dummy <- ifelse(data_final$`partially furnished` + data_final$`only kitchen furnished` > 0, 1, 0)
#Dummy per mobilio
data$furniture_dummy <- data_final$furnished
#Dummy per la cucina
data$kitchen_dummy <- data_final$kitchen
#Dummy per accesso disabili
data$disabled_access_dummy <- data_final$`disabled access`
#Dummy single tv system
data$single_tv_system_dummy <- data_final$`single tv system`

variabili_da_controllare <- c(
  "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
  "window_pvc", "window_wood", "window_metal",
  "attic_dummy", "closet_dummy", "cellar_dummy",
  "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
  "exposure_north", "exposure_south", "exposure_east", "exposure_west",
  "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
  "balcony_dummy",
  "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
  "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
  "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
  "full_day_concierge", "part_time_concierge", "receptionist",
  "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
  "disabled_access_dummy"
)

# Conta NA per ogni variabile
colSums(is.na(data[, variabili_da_controllare]))

# Imputazione NA a 0 (assenza caratteristica)
for (var in variabili_da_controllare) {
  data[[var]][is.na(data[[var]])] <- 0
}



##conditions, energy_efficiency_class, year_of_construction
table(data$conditions, useNA = "always")
table(data$energy_efficiency_class[which(data$conditions == "new / under construction")], useNA = "always")

#Tabelle di controllo iniziali 
table(data$energy_efficiency_class[which(data$conditions == "new / under construction")], useNA = "always")
table(data$year_of_construction[which(data$conditions == "new / under construction")], useNA = "always")

#Individuazione delle incoerenze 
# (case "new / under construction" ma anno < 2010 o mancante, e classe energetica bassa)
idx_incoerenti <- which(
  data$conditions == "new / under construction" &
    (is.na(data$year_of_construction) | data$year_of_construction < 2010) &
    data$energy_efficiency_class %in% c("d", "e", "f", "g")
)
#condizioni incoerenti messe a NA
data$conditions[idx_incoerenti] <- NA
# Controlli 
table(data$year_of_construction[which(is.na(data$conditions))], useNA = "always")
table(data$furniture_dummy[which(is.na(data$conditions))], useNA = "always")
table(data$partially_furnished_dummy[which(is.na(data$conditions))], useNA = "always")

# Imputazione guidata delle condizioni
# Case costruite dopo il 2015 -> "new / under construction"
data$conditions[is.na(data$conditions) & data$year_of_construction >= 2015] <- "new / under construction"

# Case arredate -> "good condition / liveable"
data$conditions[is.na(data$conditions) & data$furniture_dummy == 1] <- "good condition / liveable"

# Tutte le altre -> "to be refurbished"
data$conditions[is.na(data$conditions)] <- "to be refurbished"



#energy_efficiency_class
table(data$energy_efficiency_class, useNA = "always")
data$energy_efficiency_class[data$energy_efficiency_class == ","] <- NA
table(data$energy_efficiency_class[which(data$conditions == "new / under construction")], useNA = "always")
table(data$energy_efficiency_class[which(data$conditions == "new / under construction" & data$year_of_construction > 2010)], useNA = "always")
# Per legge, gli edifici nuovi dovrebbero avere alta efficienza energetica
#Seleziona i casi "new / under construction"
idx_new <- which(data$conditions == "new / under construction")
#Tabella delle classi energetiche non mancanti 
tab_classi <- table(data$energy_efficiency_class[idx_new], useNA = "no")
#Calcola le probabilità proporzionali
prob_classi <- prop.table(tab_classi)
#Numero di NA da imputare
n_na <- sum(is.na(data$energy_efficiency_class[idx_new]))
#Imputazione proporzionale casuale
set.seed(123)  
classi_imputate <- sample(names(prob_classi), size = n_na, replace = TRUE, prob = prob_classi)
#Sostituisci gli NA imputati nel dataset
idx_na <- idx_new[is.na(data$energy_efficiency_class[idx_new])]
data$energy_efficiency_class[idx_na] <- classi_imputate
table(data$energy_efficiency_class[which(data$conditions == "new / under construction")], useNA = "always")
table(data$energy_efficiency_class, useNA = "always")

table(data$energy_efficiency_class[which(data$year_of_construction>2000)], useNA = "always")
table(data$energy_efficiency_class[which(data$year_of_construction<=2000)], useNA = "always")
# Funzione per imputazione proporzionale di una variabile categoriale
imputa_proporzionale <- function(x) {
  # Tabella delle classi non NA
  tab <- table(x, useNA = "no")
  prob <- prop.table(tab)
  n_na <- sum(is.na(x))
  
  # Se ci sono NA, imputali proporzionalmente
  if (n_na > 0) {
    set.seed(123)  # per replicabilità
    x[is.na(x)] <- sample(names(prob), size = n_na, replace = TRUE, prob = prob)
  }
  return(x)
}
# costruzioni recenti (>2000) 
idx_recenti <- which(data$year_of_construction > 2000)
data$energy_efficiency_class[idx_recenti] <-
  imputa_proporzionale(data$energy_efficiency_class[idx_recenti])
# costruzioni vecchie (<=2000) 
idx_vecchie <- which(data$year_of_construction <= 2000)
data$energy_efficiency_class[idx_vecchie] <-
  imputa_proporzionale(data$energy_efficiency_class[idx_vecchie])

table(data$energy_efficiency_class[which(data$year_of_construction > 2000)], useNA = "always")
table(data$energy_efficiency_class[which(data$year_of_construction <= 2000)], useNA = "always")
table(data$energy_efficiency_class, useNA = "always")
data$energy_efficiency_class[is.na(data$energy_efficiency_class)] <- "g"

## heating_centralized
table(data$heating_centralized, useNA = "always") #ci sono 114 NA
# Distribuzione del riscaldamento centralizzato per piani totali
table(data$total_floors_in_building[which(data$heating_centralized == "central")], useNA = "always")
table(data$total_floors_in_building[which(data$heating_centralized == "independent")], useNA = "always")

prop_heating_by_floors <- data %>%
  filter(!is.na(heating_centralized), !is.na(total_floors_in_building)) %>% 
  group_by(total_floors_in_building) %>%
  summarise(
    n_total = n(),
    n_central = sum(heating_centralized == "central"),
    prop_central = n_central / n_total
  ) %>%
  arrange(total_floors_in_building)

prop_heating_by_floors
data$heating_centralized[is.na(data$heating_centralized) & 
                           data$total_floors_in_building >= 4] <- "central"

data$heating_centralized[is.na(data$heating_centralized) & 
                           data$total_floors_in_building < 4] <- "independent"
data$heating_centralized <- factor(data$heating_centralized)

#tasse condominiali
table(data$condominium_fees, useNA = "always")
data$condominium_fees[data$condominium_fees == "No condominium fees"] <- "0"
data$condominium_fees <- as.numeric(data$condominium_fees)
table(data$condominium_fees, useNA = "always")
summary(data$condominium_fees)

#Pulizia valori incoerenti
data$condominium_fees[
  data$total_floors_in_building >= 5 & data$condominium_fees < 50
] <- NA

data$condominium_fees[
  data$condominium_fees > 5000
] <- NA

# Correlazioni numeriche ---
num_vars <- c("square_meters", "lift", "total_floors_in_building", "floor", "shared_garden_dummy" , 
              "luxury_dummy_in_home", "luxury_dummy_in_complex", "full_day_concierge",       
              "part_time_concierge", "receptionist")

# Calcolo Spearman tra condominium_fees e ognuna
cor_results <- sapply(num_vars, function(v) {
  cor(data$condominium_fees, data[[v]], use = "pairwise.complete.obs", method = "spearman")})
cor_results <- sort(cor_results, decreasing = TRUE)
cor_results

vars_model <- c("square_meters", "lift", "total_floors_in_building", "floor", 
                "shared_garden_dummy", "luxury_dummy_in_home", 
                "luxury_dummy_in_complex", "full_day_concierge", 
                "part_time_concierge", "receptionist", 
                "energy_efficiency_class", "heating_centralized")

train_lm <- data %>%
  select(condominium_fees, all_of(vars_model)) %>%
  filter(!is.na(condominium_fees)) %>%
  mutate(log_fees = log1p(condominium_fees)) %>%  # log(condo + 1)
  na.omit()

# Modello lineare su scala log
formula_lm <- as.formula(
  paste("log_fees ~", paste(vars_model, collapse = " + "))
)
lm_model <- lm(formula_lm, data = train_lm)
summary(lm_model)

# Crea data_missing con le stesse colonne del modello
data_missing <- data %>%
  filter(is.na(condominium_fees)) %>%
  select(all_of(vars_model))

for (v in vars_model) {
  if (is.factor(train_lm[[v]])) {
    data_missing[[v]] <- factor(data_missing[[v]], levels = levels(train_lm[[v]]))
  }
}
pred_log <- predict(lm_model, newdata = data_missing)

#Riporta i valori alla scala originale e forza la positività
pred_positive <- pmax(exp(pred_log) - 1, 0)
data$condominium_fees[is.na(data$condominium_fees)] <- pred_positive

summary(data$condominium_fees)
sum(is.na(data$condominium_fees))


## zone 
table(data$zone, useNA = "always") #1 NA
# Geocoding dei quartieri di Milano e calcolo distanza dal Duomo
quartieri <- unique(data$zone)
# Funzione per geocoding con Nominatim
geocode_nominatim <- function(quartiere, city = "Milano", country = "Italy") {
  base_url <- "https://nominatim.openstreetmap.org/search"
  
  query <- paste(quartiere, city, country, sep = ", ")
 
  # Prova fino a 3 volte in caso di errore
  for (attempt in 1:3) {
    result_df <- tryCatch({
      response <- GET(
        base_url,
        query = list(
          q = query,
          format = "json",
          limit = 1
        ),
        user_agent("R geocoding script for academic project"),
        timeout(30)  # Timeout di 30 secondi
      )
      
      if (status_code(response) == 200) {
        result <- fromJSON(content(response, "text", encoding = "UTF-8"))
        
        if (length(result) > 0) {
          return(data.frame(
            quartiere = quartiere,
            lat = as.numeric(result$lat[1]),
            lon = as.numeric(result$lon[1]),
            display_name = result$display_name[1],
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Se arriviamo qui, non ci sono risultati
      return(data.frame(
        quartiere = quartiere,
        lat = NA,
        lon = NA,
        display_name = NA,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      cat(sprintf("  Errore al tentativo %d per '%s': %s\n", attempt, quartiere, e$message))
      
      if (attempt < 3) {
        cat(sprintf(" Attendo 5 secondi prima di riprovare...\n"))
        Sys.sleep(5)  #
        return(NULL)  # Ritorna NULL per far continuare il loop
      } else {
        cat(sprintf(" Fallito dopo 3 tentativi, marco come NA\n"))
        return(data.frame(
          quartiere = quartiere,
          lat = NA,
          lon = NA,
          display_name = NA,
          stringsAsFactors = FALSE
        ))
      }
    })
    
    # Se abbiamo un risultato valido, lo ritorniamo
    if (!is.null(result_df)) {
      # Pausa più lunga per rispettare i limiti del server
      Sys.sleep(2)
      return(result_df)
    }
  }
  
  # Fallback finale (non dovrebbe mai arrivare qui)
  return(data.frame(
    quartiere = quartiere,
    lat = NA,
    lon = NA,
    display_name = NA,
    stringsAsFactors = FALSE
  ))
}
# Geocoding di tutti i quartieri
cat("Inizio geocoding di", length(quartieri), "quartieri...\n")
cat("Questo processo richiederà circa", ceiling(length(quartieri) / 60), "minuti\n\n")

coordinate_quartieri <- do.call(rbind, lapply(seq_along(quartieri), function(i) {
  cat(sprintf("Geocoding %d/%d: %s\n", i, length(quartieri), quartieri[i]))
  geocode_nominatim(quartieri[i])
}))

# Coordinate del Duomo di Milano 
duomo_lat <- 45.464211
duomo_lon <- 9.191383

# Calcola la distanza di ogni quartiere dal Duomo 
coordinate_quartieri$distanza_duomo_m <- NA
coordinate_quartieri$distanza_duomo_km <- NA

for (i in 1:nrow(coordinate_quartieri)) {
  if (!is.na(coordinate_quartieri$lat[i]) && !is.na(coordinate_quartieri$lon[i])) {
    distanza <- distHaversine(
      c(duomo_lon, duomo_lat),
      c(coordinate_quartieri$lon[i], coordinate_quartieri$lat[i])
    )
    coordinate_quartieri$distanza_duomo_m[i] <- round(distanza, 2)
    coordinate_quartieri$distanza_duomo_km[i] <- round(distanza / 1000, 2)
  }
}

# Mostra risultati
cat("\n=== RIEPILOGO ===\n")
cat("Quartieri geocodificati con successo:", sum(!is.na(coordinate_quartieri$lat)), "/", nrow(coordinate_quartieri), "\n")
cat("Quartieri non trovati:", sum(is.na(coordinate_quartieri$lat)), "\n\n")

# Quartieri non geocodificati
if (sum(is.na(coordinate_quartieri$lat)) > 0) {
  cat("\nQuartieri non trovati:\n")
  print(coordinate_quartieri$quartiere[is.na(coordinate_quartieri$lat)])
}

# Identifica i quartieri con distanza > 9.5 km (probabili errori di geocoding)
outliers <- coordinate_quartieri[!is.na(coordinate_quartieri$distanza_duomo_km) & 
                                   coordinate_quartieri$distanza_duomo_km > 9.5, ]

cat("=== QUARTIERI CON DISTANZA > 9.5 KM ===\n")
if (nrow(outliers) > 0) {
  print(outliers[, c("quartiere", "distanza_duomo_km", "lat", "lon")])
  cat("\nQuartieri da correggere (outliers):\n")
  print(outliers$quartiere)
} else {
  cat("Nessun quartiere con distanza > 9.5 km\n")
}

# Identifica quartieri non trovati (NA)
na_zones <- coordinate_quartieri$quartiere[is.na(coordinate_quartieri$lat)]
cat("\n=== QUARTIERI NON TROVATI (NA) ===\n")
print(na_zones)

# Lista combinata di quartieri da correggere manualmente
quartieri_da_correggere <- c(outliers$quartiere, na_zones[!is.na(na_zones)])
cat("\n=== TOTALE QUARTIERI DA CORREGGERE MANUALMENTE ===\n")
cat("Numero totale:", length(quartieri_da_correggere), "\n")
print(quartieri_da_correggere)

# Coordinate manuali corrette
# Include sia i quartieri non trovati che quelli con distanza > 9.5 km
coordinate_manuali <- data.frame(
  quartiere = c(
    # Quartieri outliers (distanza > 9.5 km) - corretti
    "figino",
    "indipendenza",
    "montenero",
    "muggiano",
    "ponte nuovo",
    "san carlo",
    "san vittore",
    # Quartieri non trovati originariamente
    "amendola - buonarroti",
    "bignami - ponale",
    "borgogna - largo augusto",
    "cantalupa - san paolo",
    "cermenate - abbiategrasso",
    "ghisolfa - mac mahon",
    "largo caioroli 2",
    "martini - insubria",
    "monte rosa - lotto",
    "navigli - darsena",
    "pezzotti - meda",
    "piave - tricolore",
    "plebisciti - susa",
    "portello - parco vittoria",
    "quintosole - chiaravalle",
    "tre castelli - faenza",
    "vercelli - wagner"
  ),
  lat = c(
    # Outliers corretti
    45.4512,  # figino (zona ovest)
    45.4825,  # indipendenza (zona nord-est)
    45.4589,  # montenero (zona sud-est)
    45.5156,  # muggiano (zona nord-ovest)
    45.5289,  # ponte nuovo (zona nord)
    45.4756,  # san carlo (zona est)
    45.4598,  # san vittore (zona ovest-centro)
    # Non trovati
    45.4689,  # amendola - buonarroti
    45.5289,  # bignami - ponale (zona nord)
    45.4654,  # borgogna - largo augusto (centro)
    45.4456,  # cantalupa - san paolo (zona sud navigli)
    45.4398,  # cermenate - abbiategrasso (zona sud-ovest)
    45.4962,  # ghisolfa - mac mahon (zona nord-ovest)
    45.4682,  # largo cairoli (centro storico)
    45.4869,  # martini - insubria (zona stazione centrale)
    45.4782,  # monte rosa - lotto
    45.4515,  # navigli - darsena
    45.4425,  # pezzotti - meda (zona sud)
    45.4761,  # piave - tricolore (zona porta venezia est)
    45.4625,  # plebisciti - susa (zona est)
    45.4798,  # portello - parco vittoria
    45.4125,  # quintosole - chiaravalle (zona sud, più vicina)
    45.5089,  # tre castelli - faenza (zona nord)
    45.4812   # vercelli - wagner
  ),
  lon = c(
    # Outliers corretti
    9.0812,   # figino
    9.2089,   # indipendenza
    9.2356,   # montenero
    9.1189,   # muggiano
    9.2156,   # ponte nuovo
    9.2456,   # san carlo
    9.1512,   # san vittore
    # Non trovati
    9.1612,   # amendola - buonarroti
    9.2156,   # bignami - ponale
    9.2012,   # borgogna - largo augusto
    9.1689,   # cantalupa - san paolo
    9.1325,   # cermenate - abbiategrasso
    9.1456,   # ghisolfa - mac mahon
    9.1856,   # largo cairoli
    9.2089,   # martini - insubria
    9.1456,   # monte rosa - lotto
    9.1819,   # navigli - darsena
    9.1756,   # pezzotti - meda
    9.2145,   # piave - tricolore
    9.2445,   # plebisciti - susa
    9.1489,   # portello - parco vittoria
    9.2089,   # quintosole - chiaravalle 
    9.1625,   # tre castelli - faenza
    9.1589    # vercelli - wagner
  ),
  stringsAsFactors = FALSE
)

coordinate_manuali$display_name <- paste(coordinate_manuali$quartiere, "Milano, Italy (manuale)")

# Calcola le distanze per le coordinate manuali
coordinate_manuali$distanza_duomo_m <- NA
coordinate_manuali$distanza_duomo_km <- NA

for (i in 1:nrow(coordinate_manuali)) {
  distanza <- distHaversine(
    c(duomo_lon, duomo_lat),
    c(coordinate_manuali$lon[i], coordinate_manuali$lat[i])
  )
  coordinate_manuali$distanza_duomo_m[i] <- round(distanza, 2)
  coordinate_manuali$distanza_duomo_km[i] <- round(distanza / 1000, 2)
}

cat("\n=== VERIFICA COORDINATE MANUALI ===\n")
print(coordinate_manuali[, c("quartiere", "distanza_duomo_km")])

# Rimuovi dalla lista originale i quartieri che verranno sostituiti
coordinate_quartieri_puliti <- coordinate_quartieri[
  !tolower(trimws(coordinate_quartieri$quartiere)) %in% 
    tolower(trimws(coordinate_manuali$quartiere)) &
    !is.na(coordinate_quartieri$quartiere), 
]

# Combina coordinate pulite con quelle manuali
coordinate_quartieri_finale <- rbind(coordinate_quartieri_puliti, coordinate_manuali)

cat("\n=== VERIFICA DATASET FINALE ===\n")
cat("Totale quartieri:", nrow(coordinate_quartieri_finale), "\n")
cat("Quartieri con coordinate:", sum(!is.na(coordinate_quartieri_finale$lat)), "\n")
cat("Quartieri con distanza > 9.5 km:", 
    sum(coordinate_quartieri_finale$distanza_duomo_km > 9.5, na.rm = TRUE), "\n")
cat("Distanza massima:", max(coordinate_quartieri_finale$distanza_duomo_km, na.rm = TRUE), "km\n")
cat("Distanza media:", round(mean(coordinate_quartieri_finale$distanza_duomo_km, na.rm = TRUE), 2), "km\n")

# Mostra i quartieri più lontani
cat("\n=== TOP 10 QUARTIERI PIÙ LONTANI DAL DUOMO ===\n")
top_lontani <- coordinate_quartieri_finale[order(-coordinate_quartieri_finale$distanza_duomo_km), ]
print(head(top_lontani[, c("quartiere", "distanza_duomo_km")], 10))

# Merge con il dataset principale
distanze_duomo <- coordinate_quartieri_finale[, c("quartiere", "distanza_duomo_km", "lat", "lon")]
distanze_duomo$quartiere_normalized <- tolower(trimws(distanze_duomo$quartiere))
data$zone_normalized <- tolower(trimws(data$zone))

data <- merge(data, 
              distanze_duomo[, c("quartiere_normalized", "distanza_duomo_km", "lat", "lon")],
              by.x = "zone_normalized",
              by.y = "quartiere_normalized",
              all.x = TRUE)

data$zone_normalized <- NULL

# Sostituisci eventuali NA con Città Studi
if (sum(is.na(data$distanza_duomo_km)) > 0) {
  cat("\n=== SOSTITUZIONE NA CON CITTÀ STUDI ===\n")
  
  citta_studi_coords <- coordinate_quartieri_finale[
    tolower(trimws(coordinate_quartieri_finale$quartiere)) == "città studi", 
  ]
  
  if (nrow(citta_studi_coords) > 0) {
    idx_na <- is.na(data$distanza_duomo_km)
    data$zone[idx_na] <- "città studi"
    data$distanza_duomo_km[idx_na] <- citta_studi_coords$distanza_duomo_km[1]
    data$lat[idx_na] <- citta_studi_coords$lat[1]
    data$lon[idx_na] <- citta_studi_coords$lon[1]
    
    cat("Sostituiti", sum(idx_na), "valori NA con le coordinate di Città Studi\n")
    cat("Distanza dal Duomo usata:", citta_studi_coords$distanza_duomo_km[1], "km\n")
  }
}

# Verifica finale del merge
cat("\n=== VERIFICA FINALE MERGE ===\n")
cat("Totale osservazioni:", nrow(data), "\n")
cat("Osservazioni con distanza:", sum(!is.na(data$distanza_duomo_km)), "\n")
cat("Percentuale completa:", 
    round(sum(!is.na(data$distanza_duomo_km)) / nrow(data) * 100, 2), "%\n")

cat("\n=== STATISTICHE DISTANZA NEL DATASET FINALE ===\n")
print(summary(data$distanza_duomo_km))

# Calcola la frequenza di ogni zona nel dataset completo
zone_counts <- table(data$zone)

# Identifica le zone rare (con meno di 5 osservazioni)
zone_rare <- names(zone_counts[zone_counts < 5])

# Zone presenti nel training
zone_training <- unique(training$zone)

# Identifica le zone NON presenti nel training
zone_not_in_train <- setdiff(unique(data$zone), zone_training)

# Combina le due condizioni (rare o non presenti)
zone_to_replace <- union(zone_rare, zone_not_in_train)

# Sostituisci con "Altro"
data$zone <- ifelse(data$zone %in% zone_to_replace, "Altro", data$zone)

# Converte in factor con "Altro" incluso
data$zone <- factor(data$zone)


## year_of_construction
#  Seleziono le variabili numeriche rilevanti
num_vars <- data %>%
  select(
    square_meters, rooms_number, bathrooms_number,
    total_floors_in_building, floor, lift, car_parking, availability,
    distanza_duomo_km, single_glass_dummies, double_glass_dummies,
    triple_glass_dummies, attic_dummy, closet_dummy, cellar_dummy,
    internal_exposure_dummy, external_exposure_dummy, double_exposure_dummy,
    terrace_dummy, garden_dummy, private_garden_dummy, shared_garden_dummy,
    luxury_dummy_in_home, luxury_dummy_in_complex, tavern_dummy,
    optic_fiber_dummy, security_door_dummy, alarm_dummy, videophone_dummy,
    electric_gate_dummy, satellite_dish_dummy, single_tv_system_dummy,
    full_day_concierge, part_time_concierge, receptionist,
    partially_furnished_dummy, furniture_dummy, kitchen_dummy,
    disabled_access_dummy, balcony_dummy, 
    year_of_construction, condominium_fees, lat, lon
  ) %>%
  select(where(is.numeric))
# Rimuovo le variabili costanti (senza variabilità)
non_constant <- sapply(num_vars, function(x) {
  x_no_na <- x[!is.na(x)]
  length(unique(x_no_na)) > 1
})
num_vars <- num_vars[, non_constant]

# Calcolo la matrice di correlazione
cor_matrix <- cor(num_vars, use = "complete.obs")

# Trovo le variabili più correlate con year_of_construction
if("year_of_construction" %in% colnames(cor_matrix)){
  cor_year <- sort(cor_matrix[, "year_of_construction"], decreasing = TRUE)
  print(round(cor_year, 3))
} else {
  stop("La variabile 'year_of_construction' non è presente nella matrice di correlazione.")
}
vars_selected <- names(cor_year[abs(cor_year) > 0.10])
# Seleziono le top 5 variabili più correlate
best_vars <- names(head(cor_year[names(cor_year) != "year_of_construction"], 5))
cat("\nLe 5 variabili più correlate a 'year_of_construction' sono:\n")
print(best_vars)

# Creo un modello lineare per imputare i NA
formula_year <- as.formula(paste("year_of_construction ~", paste(best_vars, collapse = " + ")))

# Addestra il modello solo sui casi completi
model_year <- lm(formula_year, data = data[!is.na(data$year_of_construction), ])
data$year_of_construction_pred <- predict(model_year, newdata = data)

data <- data %>%
  mutate(year_of_construction = ifelse(is.na(year_of_construction),
                                       year_of_construction_pred,
                                       year_of_construction)) %>%
  select(-year_of_construction_pred)

summary(data$year_of_construction)
sum(is.na(data$year_of_construction))





#prova
# --- Divisione in training e test ---
y_tot <- data  # rinomino per coerenza col tuo codice
y_training <- y_tot[!is.na(y_tot$selling_price), ]
y_test <- y_tot[is.na(y_tot$selling_price), ]

# --- Cross-validation setup ---
set.seed(123)
folds <- createFolds(y_training$selling_price, k = 5)

# --- Inizializzo lista MAE ---
mae_list <- numeric(5)

# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability","distanza_duomo_km")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  
  # --- Modello lineare ---
  model <- lm(log(selling_price / square_meters) ~ ., data = train_fold)
  
  # --- Predizioni sul validation fold ---
  preds_log <- predict(model, newdata = validation_fold)
  preds_price <- exp(preds_log) * validation_fold$square_meters
  
  # --- Calcolo MAE ---
  mae <- mean(abs(validation_fold$selling_price - preds_price))
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)

library(dplyr)
library(caret)


##con dummy
mae_list <- numeric(5)

# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability","distanza_duomo_km",
                               "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                               "window_pvc", "window_wood", "window_metal",
                               "attic_dummy", "closet_dummy", "cellar_dummy",
                               "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
                               "exposure_north", "exposure_south", "exposure_east", "exposure_west",
                               "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                               "balcony_dummy",
                               "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
                               "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
                               "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
                               "full_day_concierge", "part_time_concierge", "receptionist",
                               "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
                               "disabled_access_dummy",
                               "conditions", "energy_efficiency_class", "condominium_fees", "year_of_construction", "zone")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
                                         "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                                         "window_pvc", "window_wood", "window_metal",
                                         "attic_dummy", "closet_dummy", "cellar_dummy",
                                         "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
                                         "exposure_north", "exposure_south", "exposure_east", "exposure_west",
                                         "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                                         "balcony_dummy",
                                         "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
                                         "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
                                         "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
                                         "full_day_concierge", "part_time_concierge", "receptionist",
                                         "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
                                         "disabled_access_dummy",
                                         "conditions", "energy_efficiency_class", "condominium_fees",  "year_of_construction", "zone")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  train_fold$conditions <- as.factor(train_fold$conditions)
  train_fold$energy_efficiency_class <- as.factor(train_fold$energy_efficiency_class)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  validation_fold$conditions <- as.factor(validation_fold$conditions)
  validation_fold$energy_efficiency_class <- as.factor(validation_fold$energy_efficiency_class)
  
  
  # --- Modello lineare ---
  model <- lm(log(selling_price / square_meters) ~ ., data = train_fold)
  
  # --- Predizioni sul validation fold ---
  preds_log <- predict(model, newdata = validation_fold)
  preds_price <- exp(preds_log) * validation_fold$square_meters
  
  # --- Calcolo MAE ---
  mae <- mean(abs(validation_fold$selling_price - preds_price))
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)






for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability","distanza_duomo_km",
                               "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                               "window_pvc", "window_wood", "window_metal",
                               "attic_dummy", "closet_dummy", "cellar_dummy",
                               "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
                               "exposure_north", "exposure_south", "exposure_east", "exposure_west",
                               "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                               "balcony_dummy",
                               "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
                               "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
                               "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
                               "full_day_concierge", "part_time_concierge", "receptionist",
                               "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
                               "disabled_access_dummy",
                               "conditions", "energy_efficiency_class", "condominium_fees", "year_of_construction", "zone", "heating_centralized")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
                                         "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                                         "window_pvc", "window_wood", "window_metal",
                                         "attic_dummy", "closet_dummy", "cellar_dummy",
                                         "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
                                         "exposure_north", "exposure_south", "exposure_east", "exposure_west",
                                         "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                                         "balcony_dummy",
                                         "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
                                         "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
                                         "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
                                         "full_day_concierge", "part_time_concierge", "receptionist",
                                         "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
                                         "disabled_access_dummy",
                                         "conditions", "energy_efficiency_class", "condominium_fees", "year_of_construction", "zone", "heating_centralized")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  train_fold$conditions <- as.factor(train_fold$conditions)
  train_fold$energy_efficiency_class <- as.factor(train_fold$energy_efficiency_class)
  train_fold$heating_centralized <- as.factor(train_fold$heating_centralized)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  validation_fold$conditions <- as.factor(validation_fold$conditions)
  validation_fold$energy_efficiency_class <- as.factor(validation_fold$energy_efficiency_class)
  validation_fold$heating_centralized <- as.factor(validation_fold$heating_centralized)
  
  
  # --- Modello lineare ---
  model <- lm(log(selling_price / square_meters) ~ ., data = train_fold)
  
  # --- Predizioni sul validation fold ---
  preds_log <- predict(model, newdata = validation_fold)
  preds_price <- exp(preds_log) * validation_fold$square_meters
  
  # --- Calcolo MAE ---
  mae <- mean(abs(validation_fold$selling_price - preds_price))
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)



# --- Step 1: Preparo i dati di test ---
# Seleziono le stesse variabili usate nel modello
vars_model <- c("square_meters", "rooms_number", "bathrooms_number", 
                "total_floors_in_building", "floor", "lift", "car_parking", 
                "availability", "distanza_duomo_km", 
                "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                "window_pvc", "window_wood", "window_metal", 
                "attic_dummy", "closet_dummy", "cellar_dummy", 
                "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
                "exposure_north", "exposure_south", "exposure_east", "exposure_west",
                "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                "balcony_dummy", "luxury_dummy_in_home", "luxury_dummy_in_complex", 
                "tavern_dummy", "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", 
                "videophone_dummy", "electric_gate_dummy", "satellite_dish_dummy", 
                "single_tv_system_dummy", "full_day_concierge", "part_time_concierge", 
                "receptionist", "partially_furnished_dummy", "furniture_dummy", 
                "kitchen_dummy", "disabled_access_dummy", "conditions", 
                "energy_efficiency_class", "condominium_fees", 
                "year_of_construction", "zone", "heating_centralized")

# Assicuro che i tipi di variabili coincidano
y_test$car_parking <- as.factor(y_test$car_parking)
y_test$availability <- as.factor(y_test$availability)
y_test$conditions <- as.factor(y_test$conditions)
y_test$energy_efficiency_class <- as.factor(y_test$energy_efficiency_class)
y_test$heating_centralized <- as.factor(y_test$heating_centralized)

# --- Step 2: Alleno il modello finale su tutto y_training ---
final_model <- lm(log(selling_price / square_meters) ~ ., data = y_training[, c("selling_price", vars_model)])

# --- Step 3: Predico sul test set ---
preds_log <- predict(final_model, newdata = y_test)

# Riporto alla scala del prezzo totale
y_test$prediction <- exp(preds_log) * y_test$square_meters

# Assicuro che non ci siano valori negativi o nulli
y_test$prediction <- pmax(y_test$prediction, 0)

# --- Step 4: Creo il file CSV con ID e prezzo previsto ---
output <- y_test %>%
  select(ID, prediction)

write.csv(output, "pred_selling_price.csv", row.names = FALSE)

# --- Step 5: Controllo ---
head(output)

summary(output$prediction)







#####GAM
library(mgcv)
library(mgcv)

mae_list <- numeric(5)
models <- list()

for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # --- Seleziono solo le variabili desiderate ---
  vars <- c("selling_price", "square_meters", "rooms_number", 
            "bathrooms_number", "total_floors_in_building", 
            "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
            "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
            "window_pvc", "window_wood", "window_metal",
            "attic_dummy", "closet_dummy", "cellar_dummy",
            "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
            "exposure_north", "exposure_south", "exposure_east", "exposure_west",
            "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
            "balcony_dummy",
            "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
            "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
            "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
            "full_day_concierge", "part_time_concierge", "receptionist",
            "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
            "disabled_access_dummy",
            "conditions", "energy_efficiency_class", "condominium_fees", 
            "year_of_construction", "zone", "heating_centralized")
  
  train_fold <- train_fold[, vars]
  validation_fold <- validation_fold[, vars]
  
  # --- Conversione fattori ---
  for (col in c("car_parking", "availability", "conditions", 
                "energy_efficiency_class", "heating_centralized")) {
    train_fold[[col]] <- as.factor(train_fold[[col]])
    validation_fold[[col]] <- as.factor(validation_fold[[col]])
  }
  
  # --- Modello GAM CON CORREZIONE ---
  # Opzione 1: Riduci i gradi di libertà (k) per le variabili problematiche
  model_gam <- gam(
    log(selling_price / square_meters) ~ 
      s(square_meters) + 
      s(rooms_number, k = 5) +           # Ridotto k
      s(bathrooms_number, k = 4) +       # Ridotto k
      s(total_floors_in_building, k = 5) + # Ridotto k
      s(floor, k = 5) +                  # Ridotto k
      s(distanza_duomo_km) +
      lift + car_parking + availability + conditions + 
      energy_efficiency_class + heating_centralized +
      luxury_dummy_in_home + luxury_dummy_in_complex +
      full_day_concierge + part_time_concierge + receptionist +
      condominium_fees + year_of_construction + zone,
    data = train_fold
  )
  
  # --- Predizioni ---
  preds_log <- predict(model_gam, newdata = validation_fold)
  preds_price <- exp(preds_log) * validation_fold$square_meters
  
  # --- MAE ---
  mae <- mean(abs(validation_fold$selling_price - preds_price), na.rm = TRUE)
  mae_list[i] <- mae
  models[[i]] <- model_gam
  
  cat(sprintf("Fold %d: MAE = %.2f\n", i, mae))
}

# --- Risultato finale ---
cat(sprintf("\nMAE medio: %.2f\n", mean(mae_list)))



mae_list <- numeric(5)
models <- list()
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # --- Feature Engineering ---
  # Aggiungi variabili derivate PRIMA di selezionare vars
  train_fold$price_per_sqm <- train_fold$selling_price / train_fold$square_meters
  validation_fold$price_per_sqm <- validation_fold$selling_price / validation_fold$square_meters
  
  train_fold$building_age <- 2024 - train_fold$year_of_construction
  validation_fold$building_age <- 2024 - validation_fold$year_of_construction
  
  train_fold$rooms_per_sqm <- train_fold$rooms_number / train_fold$square_meters
  validation_fold$rooms_per_sqm <- validation_fold$rooms_number / validation_fold$square_meters
  
  train_fold$total_amenities <- rowSums(train_fold[, c("lift", "terrace_dummy", 
                                                       "garden_dummy", "balcony_dummy",
                                                       "luxury_dummy_in_home", 
                                                       "security_door_dummy")])
  validation_fold$total_amenities <- rowSums(validation_fold[, c("lift", "terrace_dummy", 
                                                                 "garden_dummy", "balcony_dummy",
                                                                 "luxury_dummy_in_home", 
                                                                 "security_door_dummy")])
  
  # Interazione distanza * superficie
  train_fold$dist_sqm_interaction <- train_fold$distanza_duomo_km * train_fold$square_meters
  validation_fold$dist_sqm_interaction <- validation_fold$distanza_duomo_km * validation_fold$square_meters
  
  # --- Seleziona variabili (AGGIUNGI le nuove) ---
  vars <- c("selling_price", "square_meters", "rooms_number", 
            "bathrooms_number", "total_floors_in_building", 
            "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
            "building_age", "rooms_per_sqm", "total_amenities", "dist_sqm_interaction",  # NUOVE
            "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
            "window_pvc", "window_wood", "window_metal",
            "attic_dummy", "closet_dummy", "cellar_dummy",
            "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
            "exposure_north", "exposure_south", "exposure_east", "exposure_west",
            "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
            "balcony_dummy",
            "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
            "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
            "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
            "full_day_concierge", "part_time_concierge", "receptionist",
            "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
            "disabled_access_dummy",
            "conditions", "energy_efficiency_class", "condominium_fees", 
            "year_of_construction", "zone", "heating_centralized")
  
  train_fold <- train_fold[, vars]
  validation_fold <- validation_fold[, vars]
  
  # --- Conversione fattori ---
  for (col in c("car_parking", "availability", "conditions", 
                "energy_efficiency_class", "heating_centralized")) {
    train_fold[[col]] <- as.factor(train_fold[[col]])
    validation_fold[[col]] <- as.factor(validation_fold[[col]])
  }
  
  # --- Modello GAM MIGLIORATO ---
  model_gam <- gam(
    log(selling_price / square_meters) ~ 
      s(square_meters, k = 10) + 
      rooms_number +                      # Lineare
      bathrooms_number +                  # Lineare
      s(total_floors_in_building, k = 5) +
      floor +                             # Lineare
      s(distanza_duomo_km, k = 8) +
      s(building_age, k = 8) +            # NUOVO: età edificio con smooth
      s(total_amenities, k = 5) +         # NUOVO: totale servizi
      rooms_per_sqm +                     # NUOVO: densità stanze
      dist_sqm_interaction +              # NUOVO: interazione
      lift + car_parking + availability + conditions + 
      energy_efficiency_class + heating_centralized +
      luxury_dummy_in_home + luxury_dummy_in_complex +
      full_day_concierge + part_time_concierge + receptionist +
      condominium_fees + zone +
      # Aggiungi più dummy importanti
      terrace_dummy + garden_dummy + balcony_dummy +
      security_door_dummy + alarm_dummy +
      single_glass_dummies + double_glass_dummies + triple_glass_dummies,
    data = train_fold
  )
  
  # --- Predizioni ---
  preds_log <- predict(model_gam, newdata = validation_fold)
  preds_price <- exp(preds_log) * validation_fold$square_meters
  
  # --- MAE ---
  mae <- mean(abs(validation_fold$selling_price - preds_price), na.rm = TRUE)
  mae_list[i] <- mae
  models[[i]] <- model_gam
  
  cat(sprintf("Fold %d: MAE = %.2f\n", i, mae))
}

cat(sprintf("\nMAE medio: %.2f\n", mean(mae_list)))





# ===== PREDIZIONI SUL TEST SET =====

# --- 1. Prepara il test set con le stesse feature engineering ---
test_data <- y_test  # Assumo che il tuo test set si chiami y_test

# Feature Engineering (identiche al training)
test_data$building_age <- 2025 - test_data$year_of_construction

test_data$rooms_per_sqm <- test_data$rooms_number / test_data$square_meters

test_data$total_amenities <- rowSums(test_data[, c("lift", "terrace_dummy", 
                                                   "garden_dummy", "balcony_dummy",
                                                   "luxury_dummy_in_home", 
                                                   "security_door_dummy")])

test_data$dist_sqm_interaction <- test_data$distanza_duomo_km * test_data$square_meters

# --- 2. Seleziona le stesse variabili usate nel training ---
vars <- c("square_meters", "rooms_number", 
          "bathrooms_number", "total_floors_in_building", 
          "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
          "building_age", "rooms_per_sqm", "total_amenities", "dist_sqm_interaction",
          "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
          "window_pvc", "window_wood", "window_metal",
          "attic_dummy", "closet_dummy", "cellar_dummy",
          "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
          "exposure_north", "exposure_south", "exposure_east", "exposure_west",
          "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
          "balcony_dummy",
          "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
          "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
          "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
          "full_day_concierge", "part_time_concierge", "receptionist",
          "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
          "disabled_access_dummy",
          "conditions", "energy_efficiency_class", "condominium_fees", 
          "year_of_construction", "zone", "heating_centralized")

test_data_selected <- test_data[, vars]

# --- 3. Conversione fattori (identica al training) ---
for (col in c("car_parking", "availability", "conditions", 
              "energy_efficiency_class", "heating_centralized")) {
  test_data_selected[[col]] <- as.factor(test_data_selected[[col]])
}

# --- 4. OPZIONE A: Usa il miglior modello (quello con MAE più basso) ---
best_model_idx <- which.min(mae_list)
cat(sprintf("Miglior modello: Fold %d con MAE = %.2f\n", best_model_idx, mae_list[best_model_idx]))

preds_log <- predict(models[[best_model_idx]], newdata = test_data_selected)
predictions_best <- exp(preds_log) * test_data_selected$square_meters

# --- 4. OPZIONE B: Usa l'ensemble (media di tutti e 5 i modelli) ---
# Questa opzione è generalmente più robusta!
predictions_matrix <- sapply(1:5, function(i) {
  preds_log <- predict(models[[i]], newdata = test_data_selected)
  exp(preds_log) * test_data_selected$square_meters
})

predictions_ensemble <- rowMeans(predictions_matrix, na.rm = TRUE)

# --- 5. Scegli quale predizione usare ---
# Opzione A: miglior modello
final_predictions <- predictions_best

# Opzione B: ensemble (CONSIGLIATO - più robusto)
# final_predictions <- predictions_ensemble

# --- 6. Crea il dataframe per il CSV ---
# Assumo che il test set abbia una colonna ID
# Se non ce l'ha, puoi crearla così: test_data$ID <- 1:nrow(test_data)

submission <- data.frame(
  ID = test_data$ID,  # O y_test$ID, dipende dal nome del tuo dataset
  predict = round(final_predictions, 2)  # Arrotonda a 2 decimali
)

# --- 7. Verifica le predizioni ---
cat("\n=== STATISTICHE PREDIZIONI ===\n")
cat(sprintf("Numero di predizioni: %d\n", nrow(submission)))
cat(sprintf("Prezzo minimo predetto: %.2f €\n", min(submission$predict, na.rm = TRUE)))
cat(sprintf("Prezzo massimo predetto: %.2f €\n", max(submission$predict, na.rm = TRUE)))
cat(sprintf("Prezzo medio predetto: %.2f €\n", mean(submission$predict, na.rm = TRUE)))
cat(sprintf("Predizioni con NA: %d\n", sum(is.na(submission$predict))))

# Mostra le prime righe
cat("\n=== PRIME 10 PREDIZIONI ===\n")
print(head(submission, 10))

# --- 8. Salva il CSV ---
write.csv(submission, "submission_gam_predictions.csv", row.names = FALSE)
cat("\n✓ File salvato: submission_gam_predictions.csv\n")

# --- 9. BONUS: Confronto tra miglior modello e ensemble ---
cat("\n=== CONFRONTO PREDIZIONI ===\n")
comparison <- data.frame(
  ID = test_data$ID,
  predict_best_model = round(predictions_best, 2),
  predict_ensemble = round(predictions_ensemble, 2),
  difference = round(predictions_ensemble - predictions_best, 2)
)

cat(sprintf("Differenza media tra ensemble e best model: %.2f €\n", 
            mean(abs(comparison$difference), na.rm = TRUE)))

# Salva anche questo confronto (opzionale)
# write.csv(comparison, "comparison_predictions.csv", row.names = FALSE)









# ===== TRAINING SU TUTTI I DATI =====

cat("=== PREPARAZIONE DATI DI TRAINING ===\n")

# --- 1. Prepara il training set completo con feature engineering ---
train_data <- y_training

# Feature Engineering
train_data$building_age <- 2025 - train_data$year_of_construction
train_data$rooms_per_sqm <- train_data$rooms_number / train_data$square_meters
train_data$total_amenities <- rowSums(train_data[, c("lift", "terrace_dummy", 
                                                     "garden_dummy", "balcony_dummy",
                                                     "luxury_dummy_in_home", 
                                                     "security_door_dummy")])
train_data$dist_sqm_interaction <- train_data$distanza_duomo_km * train_data$square_meters

# --- 2. Seleziona variabili ---
vars <- c("selling_price", "square_meters", "rooms_number", 
          "bathrooms_number", "total_floors_in_building", 
          "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
          "building_age", "rooms_per_sqm", "total_amenities", "dist_sqm_interaction",
          "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
          "window_pvc", "window_wood", "window_metal",
          "attic_dummy", "closet_dummy", "cellar_dummy",
          "internal_exposure_dummy", "external_exposure_dummy", "double_exposure_dummy",
          "exposure_north", "exposure_south", "exposure_east", "exposure_west",
          "terrace_dummy", "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
          "balcony_dummy",
          "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
          "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
          "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
          "full_day_concierge", "part_time_concierge", "receptionist",
          "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
          "disabled_access_dummy",
          "conditions", "energy_efficiency_class", "condominium_fees", 
          "year_of_construction", "zone", "heating_centralized")

train_data <- train_data[, vars]

# --- 3. Conversione fattori ---
for (col in c("car_parking", "availability", "conditions", 
              "energy_efficiency_class", "heating_centralized")) {
  train_data[[col]] <- as.factor(train_data[[col]])
}

# --- 4. TRAINING DEL MODELLO FINALE SU TUTTI I DATI ---
cat("\n=== TRAINING MODELLO FINALE ===\n")
cat("Training su", nrow(train_data), "osservazioni...\n")

model_final <- gam(
  log(selling_price / square_meters) ~ 
    s(square_meters, k = 10) + 
    rooms_number +
    bathrooms_number +
    s(total_floors_in_building, k = 5) +
    floor +
    s(distanza_duomo_km, k = 8) +
    s(building_age, k = 8) +
    s(total_amenities, k = 5) +
    rooms_per_sqm +
    dist_sqm_interaction +
    lift + car_parking + availability + conditions + 
    energy_efficiency_class + heating_centralized +
    luxury_dummy_in_home + luxury_dummy_in_complex +
    full_day_concierge + part_time_concierge + receptionist +
    condominium_fees + zone +
    terrace_dummy + garden_dummy + balcony_dummy +
    security_door_dummy + alarm_dummy +
    single_glass_dummies + double_glass_dummies + triple_glass_dummies,
  data = train_data
)

cat("✓ Modello addestrato con successo!\n")

# Mostra summary del modello
cat("\n=== SUMMARY MODELLO ===\n")
print(summary(model_final))

# ===== PREDIZIONI SUL TEST SET =====

cat("\n=== PREPARAZIONE TEST SET ===\n")

# --- 5. Prepara il test set con le stesse feature engineering ---
test_data <- y_test  # Assumo che il test set si chiami y_test

# Feature Engineering (identiche al training)
test_data$building_age <- 2025 - test_data$year_of_construction
test_data$rooms_per_sqm <- test_data$rooms_number / test_data$square_meters
test_data$total_amenities <- rowSums(test_data[, c("lift", "terrace_dummy", 
                                                   "garden_dummy", "balcony_dummy",
                                                   "luxury_dummy_in_home", 
                                                   "security_door_dummy")])
test_data$dist_sqm_interaction <- test_data$distanza_duomo_km * test_data$square_meters

# --- 6. Seleziona le stesse variabili (escludi selling_price) ---
vars_test <- vars[vars != "selling_price"]
test_data_selected <- test_data[, vars_test]

# --- 7. Conversione fattori ---
for (col in c("car_parking", "availability", "conditions", 
              "energy_efficiency_class", "heating_centralized")) {
  test_data_selected[[col]] <- as.factor(test_data_selected[[col]])
}

# --- 8. PREDIZIONI ---
cat("\n=== GENERAZIONE PREDIZIONI ===\n")

preds_log <- predict(model_final, newdata = test_data_selected)
predictions <- exp(preds_log) * test_data_selected$square_meters

# --- 9. Crea il dataframe per il CSV ---
submission <- data.frame(
  ID = test_data$ID,  # Se non hai ID: test_data$ID <- 1:nrow(test_data)
  predict = round(predictions, 2)
)

# --- 10. Verifica le predizioni ---
cat("\n=== STATISTICHE PREDIZIONI ===\n")
cat(sprintf("Numero di predizioni: %d\n", nrow(submission)))
cat(sprintf("Prezzo minimo predetto: %.2f €\n", min(submission$predict, na.rm = TRUE)))
cat(sprintf("Prezzo massimo predetto: %.2f €\n", max(submission$predict, na.rm = TRUE)))
cat(sprintf("Prezzo medio predetto: %.2f €\n", mean(submission$predict, na.rm = TRUE)))
cat(sprintf("Prezzo mediano predetto: %.2f €\n", median(submission$predict, na.rm = TRUE)))
cat(sprintf("Predizioni con NA: %d\n", sum(is.na(submission$predict))))

# Distribuzione per fasce di prezzo
cat("\n=== DISTRIBUZIONE PREZZI PREDETTI ===\n")
breaks <- c(0, 200000, 400000, 600000, 800000, 1000000, Inf)
labels <- c("<200k", "200-400k", "400-600k", "600-800k", "800k-1M", ">1M")
price_ranges <- cut(submission$predict, breaks = breaks, labels = labels)
print(table(price_ranges))

# Mostra le prime 10 predizioni
cat("\n=== PRIME 10 PREDIZIONI ===\n")
print(head(submission, 10))

# --- 11. Salva il CSV ---
write.csv(submission, "submission_gam_final.csv", row.names = FALSE)
cat("\n✓ File salvato: submission_gam_final.csv\n")

# --- 12. BONUS: Diagnostica del modello ---
cat("\n=== DIAGNOSTICA MODELLO ===\n")
cat("Deviance explained:", round(summary(model_final)$dev.expl * 100, 2), "%\n")
cat("R-squared adjusted:", round(summary(model_final)$r.sq, 4), "\n")

# Salva anche il modello per uso futuro (opzionale)
saveRDS(model_final, "model_gam_final.rds")
cat("✓ Modello salvato: model_gam_final.rds\n")

cat("\n=== COMPLETATO! ===\n")


mae_list <- numeric(5)
# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  
  model <- lm(log(selling_price) ~ ., data = train_fold)
  preds_log <- predict(model, newdata = validation_fold)
  preds_price <- exp(preds_log)
  mae <- mean(abs(validation_fold$selling_price - preds_price))
  
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)


set.seed(123)
mae_list <- numeric(5)

# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  
  rf_model <- randomForest(
    log(selling_price) ~ square_meters + rooms_number + bathrooms_number +
      total_floors_in_building + floor + lift + car_parking + availability + distanza_duomo_km,
    data = train_fold, ntree = 500)
  preds_log <- predict(rf_model, newdata = validation_fold)
  preds_price <- exp(preds_log)
  mae <- mean(abs(validation_fold$selling_price - preds_price))
  
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)




set.seed(123)
mae_list <- numeric(5)

# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  
  rf_model <- randomForest(
    log(selling_price/square_meters) ~ rooms_number + bathrooms_number +
      total_floors_in_building + floor + lift + car_parking + availability + distanza_duomo_km,
    data = train_fold, ntree = 500)
  preds_log <- predict(rf_model, newdata = validation_fold)
  pred_price <- exp(preds_log) * validation_fold$square_meters
  mae <- mean(abs(validation_fold$selling_price - pred_price))
  
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)



# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  train_fold <- train_fold[, c("selling_price", "square_meters", "rooms_number", 
                               "bathrooms_number", "total_floors_in_building", 
                               "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
                               "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                               "attic_dummy", "closet_dummy", "cellar_dummy", "internal_exposure_dummy",
                               "external_exposure_dummy", "double_exposure_dummy", "terrace_dummy",
                               "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                               "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
                               "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
                               "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
                               "full_day_concierge", "part_time_concierge", "receptionist",
                               "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
                               "disabled_access_dummy", "balcony_dummy", 
                               "conditions", "energy_efficiency_class", "condominium_fees", "lat", "lon", "year_of_construction")]
  validation_fold <- validation_fold[, c("selling_price", "square_meters", "rooms_number", 
                                         "bathrooms_number", "total_floors_in_building", 
                                         "floor", "lift", "car_parking", "availability", "distanza_duomo_km",
                                         "single_glass_dummies", "double_glass_dummies", "triple_glass_dummies",
                                         "attic_dummy", "closet_dummy", "cellar_dummy", "internal_exposure_dummy",
                                         "external_exposure_dummy", "double_exposure_dummy", "terrace_dummy",
                                         "garden_dummy", "private_garden_dummy", "shared_garden_dummy",
                                         "luxury_dummy_in_home", "luxury_dummy_in_complex", "tavern_dummy",
                                         "optic_fiber_dummy", "security_door_dummy", "alarm_dummy", "videophone_dummy",
                                         "electric_gate_dummy", "satellite_dish_dummy", "single_tv_system_dummy",
                                         "full_day_concierge", "part_time_concierge", "receptionist",
                                         "partially_furnished_dummy", "furniture_dummy", "kitchen_dummy",
                                         "disabled_access_dummy", "balcony_dummy", 
                                         "conditions", "energy_efficiency_class", "condominium_fees", "lat", "lon", "year_of_construction")]
  
  # Conversione a fattori se necessario
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  
  rf_model <- randomForest(
    log(selling_price/square_meters) ~  rooms_number + 
      bathrooms_number + total_floors_in_building + 
      floor + lift + car_parking + availability + distanza_duomo_km + 
      single_glass_dummies + double_glass_dummies + triple_glass_dummies + 
      attic_dummy + closet_dummy + cellar_dummy + internal_exposure_dummy + 
      external_exposure_dummy + double_exposure_dummy + terrace_dummy + 
      garden_dummy + private_garden_dummy + shared_garden_dummy + 
      luxury_dummy_in_home + luxury_dummy_in_complex + tavern_dummy + 
      optic_fiber_dummy + security_door_dummy + alarm_dummy + videophone_dummy + 
      electric_gate_dummy + satellite_dish_dummy + single_tv_system_dummy + 
      full_day_concierge + part_time_concierge + receptionist + 
      partially_furnished_dummy + furniture_dummy + kitchen_dummy + 
      disabled_access_dummy + balcony_dummy + 
      conditions + energy_efficiency_class + condominium_fees + lat + lon + year_of_construction
    ,
    data = train_fold, ntree = 500)
  preds_log <- predict(rf_model, newdata = validation_fold)
  pred_price <- exp(preds_log) * validation_fold$square_meters
  mae <- mean(abs(validation_fold$selling_price - pred_price))
  
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)






####
set.seed(123)
mae_list <- numeric(5)

# --- Ciclo sui fold ---
for (i in 1:5) {
  train_fold <- y_training[-folds[[i]], ]
  validation_fold <- y_training[folds[[i]], ]
  
  # Seleziono solo le variabili desiderate
  vars <- c("selling_price", "square_meters", "rooms_number", 
            "bathrooms_number", "total_floors_in_building", 
            "floor", "lift", "car_parking", "availability", "distanza_duomo_km")
  
  train_fold <- train_fold[, vars]
  validation_fold <- validation_fold[, vars]
  
  # Conversione a fattori
  train_fold$car_parking <- as.factor(train_fold$car_parking)
  train_fold$availability <- as.factor(train_fold$availability)
  validation_fold$car_parking <- as.factor(validation_fold$car_parking)
  validation_fold$availability <- as.factor(validation_fold$availability)
  
  # --- Modello lineare completo ---
  full_model <- lm(log(selling_price / square_meters) ~ rooms_number + bathrooms_number +
                     total_floors_in_building + floor + lift + car_parking + 
                     availability + distanza_duomo_km,
                   data = train_fold)
  
  # --- Backward stepwise selection ---
  backward_model <- step(full_model, direction = "backward", trace = 0)
  
  # --- Predizioni sul validation fold ---
  preds_log <- predict(backward_model, newdata = validation_fold)
  preds_price <- exp(preds_log) * validation_fold$square_meters
  
  # --- Calcolo MAE ---
  mae <- mean(abs(validation_fold$selling_price - preds_price))
  mae_list[i] <- mae
}

# --- Risultato finale ---
mean(mae_list)

