library(mongolite)
library(dplyr)
library(ggplot2)
library(tidyr)

# OLA3 OPG 3 - Fodbold, kvinder og etik 

# OPG 3.1
#Er der forskel på kvinder og mænd, når det gælder events i fodboldkampe?
#Lav en optælling af udvalgte variabler – antal frispark, røde kort, fejlafleveringer og missede skud
#pr kamp fordelt på køn. Både absolutte tal og relative.

###Dataindhenting - 509 kampe i alt for kvinder

##Kvinde skud og afleveringer
events_connection <- mongo(collection = "events", db = "fb", url = "mongodb://localhost:27017")

#Henter alle events der enten er pass eller shot
events_query <- '{"type.name": { "$in": ["Pass", "Shot"] }}'

#Henter kampid og udfald af shots og passes 
fields <- '{"match_id": 1, "type.name": 1, "pass.outcome.name": 1, "shot.outcome.name": 1, "_id": 0}'

wshotpass_df <- events_connection$find(events_query, fields)

##Kvinde frispark og røde kort 
events_query <- '{
  "$or": [
    { "bad_behaviour.card.name": { "$in": ["Red Card", "Second Yellow", "Yellow Card"] } },
    { "type.name": "Foul Committed" }
  ]
}'

#Inkluder match_id, type, advantage, røde kort
fields <- '{"match_id": 1, "type.name": 1, "foul_committed.advantage": 1, "bad_behaviour.card.name": 1, "_id": 0}'

wbad <- events_connection$find(events_query, fields)

##Henter VM kampe ud fra matches
matches_connection <- mongo(collection = "matches", db = "fb", url = "mongodb://localhost:27017")

query <- '{
  "competition.competition_name": "Women\'s World Cup"
}'

fields <- '{
  "match_id": 1,
  "match_date": 1,
  "competition.competition_name": 1,
  "home_team.home_team_name": 1,
  "away_team.away_team_name": 1,
  "_id": 0
}'

wmatches <- matches_connection$find(query, fields)

###Datarensning

##Kvinde skud og passes 
# Udpakning af de indlejrede dataframes
wshotpass_df_clean <- wshotpass_df %>%
  mutate(
    type = type$name,
    pass_outcome = pass$outcome$name,
    shot_outcome = shot$outcome$name
  ) %>%
  select(match_id, type, pass_outcome, shot_outcome)  

#Ersatter NA med lykkedes pass
wshotpass_df_clean <- wshotpass_df_clean %>%
  mutate(pass_outcome = replace_na(pass_outcome, "Successful"))

##Kvinde røde kort og frispark 
# Udpakning 
wbad_clean <- wbad %>%
  mutate(
    type = type$name,  # Henter hændelsestypen (Foul Committed)
    foul_advantage = foul_committed$advantage,  # Henter fordelereglen (TRUE/FALSE/NA)
    card = bad_behaviour$card$name  # Henter røde kort
  ) %>%
  select(match_id, type, foul_advantage, card)  # Beholder kun relevante kolonner

###Optælling af hændelser 

#Kvinde statistikker
#Tæl total afleveringer
total_afleveringer <- wshotpass_df_clean %>%
  filter(type == "Pass") %>%
  group_by(match_id) %>%
  summarise(total_afleveringer = n())

#Optælling af fejlafleveringer
fejlafleveringer <- wshotpass_df_clean %>%
  filter(type == "Pass" & pass_outcome %in% c("Incomplete", "Out", "Pass Offside", "Unknown")) %>%  
  group_by(match_id) %>%
  summarise(fejlafleveringer = n())

#Tæl total skud 
total_skud <- wshotpass_df_clean %>%
  filter(type == "Shot") %>%
  group_by(match_id) %>%
  summarise(total_skud = n())

#Tæller missede skud
missede_skud <- wshotpass_df_clean %>%
  filter(
    type == "Shot",  # Kun skuddata
    shot_outcome %in% c("Off T", "Wayward", "Post", "Saved Off Target", "Saved to Post", "Saved", "Blocked")  
  ) %>%
  group_by(match_id) %>%
  summarise(missede_skud = n())

#Frispark
frispark <- wbad_clean %>%
  filter(type == "Foul Committed") %>%  #
  group_by(match_id) %>%  
  summarise(frispark = n(), .groups = "drop")  

#Røde kort
røde_kort <- wbad_clean %>%
  filter(card %in% c("Red Card", "Second Yellow", "Yellow Card")) %>%
  group_by(match_id) %>%
  summarise(røde_kort = n(), .groups = "drop")

# Kombiner alle optællinger pr. kamp og merge med vm data = 116 kampe 
kvinde_data <- wmatches %>%
  left_join(total_afleveringer, by = "match_id") %>%
  left_join(fejlafleveringer, by = "match_id") %>%
  left_join(total_skud, by = "match_id") %>%
  left_join(missede_skud, by = "match_id") %>%
  left_join(frispark, by = "match_id") %>%
  left_join(røde_kort, by = "match_id") %>%
  mutate(køn = "Kvinder")

### Mandedata 

###Dataindhenting

##Mande skud og afleveringer
events_connectionm <- mongo(collection = "maleevents", db = "fb", url = "mongodb://localhost:27017")

#Henter alle events der enten er pass eller shot
events_query <- '{"type.name": { "$in": ["Pass", "Shot"] }}'

#Henter kampid og udfald af shots og passes 
fields <- '{"matchId": 1, "type.name": 1, "pass.outcome.name": 1, "shot.outcome.name": 1, "_id": 0}'

mshotpass_df <- events_connectionm$find(events_query, fields)

##Mande frispark og kort 
events_query <- '{
  "$or": [
    { "bad_behaviour.card.name": { "$in": ["Red Card", "Second Yellow", "Yellow Card"] } },
    { "type.name": "Foul Committed" }
  ]
}'

#Inkluder match_id, type, advantage, røde kort
fields <- '{"matchId": 1, "type.name": 1, "foul_committed.advantage": 1, "bad_behaviour.card.name": 1, "_id": 0}'

mbad <- events_connectionm$find(events_query, fields)

## Henter VM kampe ud fra matches

matches_connectionm <- mongo(collection = "matches", db = "fb", url = "mongodb://localhost:27017")

# Definer query for at finde EM og VM kampe for mænd
query <- '{
  "competition.competition_name": "FIFA World Cup",
  "home_team.home_team_gender": "male",
  "away_team.away_team_gender": "male"
}'

# Definer de felter, der skal hentes
fields <- '{
  "match_id": 1,
  "match_date": 1,
  "competition.competition_name": 1,
  "home_team.home_team_name": 1,
  "away_team.away_team_name": 1,
  "_id": 0
}'

# Hent data fra MongoDB
mmatches <- matches_connectionm$find(query, fields)

###Datarensning

##Mande skud og passes 
# Udpakning af de indlejrede dataframes
mshotpass_df_clean <- mshotpass_df %>%
  mutate(
    type = type$name,
    pass_outcome = pass$outcome$name,
    shot_outcome = shot$outcome$name
  ) %>%
  select(matchId, type, pass_outcome, shot_outcome)  

#Ersatter NA med succesfulde passes
mshotpass_df_clean <- mshotpass_df_clean %>%
  mutate(pass_outcome = replace_na(pass_outcome, "Successful"))

##Mande røde kort og frispark 
# Udpakning 
mbad_clean <- mbad %>%
  mutate(
    type = type$name,  # Henter hændelsestypen (Foul Committed)
    foul_advantage = foul_committed$advantage,  # Henter fordelereglen (TRUE/FALSE/NA)
    card = bad_behaviour$card$name  # Henter røde kort
  ) %>%
  select(matchId, type, foul_advantage, card)  # Beholder kun relevante kolonner

mmatches <- mmatches %>% rename(matchId = match_id)

###Optælling af hændelser 

#Mande statistikker
#Tæl total afleveringer
total_afleveringerm <- mshotpass_df_clean %>%
  filter(type == "Pass") %>%
  group_by(matchId) %>%
  summarise(total_afleveringerm = n())

#Optælling af fejlafleveringer 
fejlafleveringerm <- mshotpass_df_clean %>%
  filter(type == "Pass" & pass_outcome %in% c("Incomplete", "Out", "Pass Offside", "Unknown")) %>%  
  group_by(matchId) %>%
  summarise(fejlafleveringerm = n())

#Tæl total skud 
total_skudm <- mshotpass_df_clean %>%
  filter(type == "Shot") %>%
  group_by(matchId) %>%
  summarise(total_skudm = n())

#Tæller missede skud 
missede_skudm <- mshotpass_df_clean %>%
  filter(
    type == "Shot",  # Kun skuddata
    shot_outcome %in% c("Off T", "Wayward", "Post", "Saved Off Target", "Saved to Post", "Saved", "Blocked")  
  ) %>%
  group_by(matchId) %>%
  summarise(missede_skudm = n())

#Frispark
frisparkm <- mbad_clean %>%
  filter(type == "Foul Committed") %>%  #
  group_by(matchId) %>%  
  summarise(frisparkm = n(), .groups = "drop")  

#Røde kort
røde_kortm <- mbad_clean %>%
  filter(card %in% c("Red Card", "Second Yellow", "Yellow Card")) %>%
  group_by(matchId) %>%
  summarise(røde_kortm = n(), .groups = "drop")

# Kombiner alle optællinger pr. kamp og merge med vm data = 147 kampe 
mande_data <- mmatches %>%
  left_join(total_afleveringerm, by = "matchId") %>%
  left_join(fejlafleveringerm, by = "matchId") %>%
  left_join(total_skudm, by = "matchId") %>%
  left_join(missede_skudm, by = "matchId") %>%
  left_join(frisparkm, by = "matchId") %>%
  left_join(røde_kortm, by = "matchId") %>%
  mutate(køn = "Mænd")

#mande_data <- mande_data %>%
#mutate(across(where(is.numeric), ~ replace_na(.x, 0)))  # Erstat NA med 0 i numeriske kolonner

#Visualisering 
sum(kvinde_data$røde_kort)
#Binder sammen 
mande_data <- mande_data %>%
  rename_with(~ gsub("m$", "", .x)) %>%         # fjerner m-endelser som fejlafleveringerm → fejlafleveringer
  rename(match_id = matchId)                    # sikrer at match_id hedder det samme

alle_data_vm <- bind_rows(kvinde_data, mande_data)

# Langt format: Fejlafleveringer, missede skud, frispark, røde kort
alle_data_vm <- alle_data_vm %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))




alle_long <- alle_data_vm %>%
  pivot_longer(cols = c(fejlafleveringer, missede_skud, frispark, røde_kort),
               names_to = "event",
               values_to = "antal") %>%
  mutate(event = case_when(
    event == "fejlafleveringer" ~ "Fejlafleveringer",
    event == "missede_skud" ~ "Missede skud",
    event == "frispark" ~ "Frispark",
    event == "røde_kort" ~ "Røde og gule kort"
  ))

# Sæt rækkefølge på events
alle_long$event <- factor(alle_long$event, levels = c(
  "Fejlafleveringer", "Missede skud", "Frispark", "Røde og gule kort"
))

#Plot 1 - absolutte hændelser 
alle_abs <- alle_long %>%
  group_by(køn, event) %>%
  summarise(total = sum(antal, na.rm = TRUE), .groups = "drop")

ggplot(alle_abs, aes(x = event, y = total, fill = køn)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = round(total, 0)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5) +
  labs(
    title = "Mænd har flest hændelser i VM-kampe på tværs af alle kategorier",
    subtitle = paste0("Kvinder: ", n_distinct(kvinde_data$match_id), " kampe, Mænd: ", n_distinct(mande_data$match_id), " kampe"),
    caption = "Kilde: Statsbomb",
    x = "Hændelsestype",
    y = "Antal forekomst af hændelse",
    fill = "Køn"
  ) +
  scale_fill_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF")) +
  theme_minimal()

#Plot 2 - gennemsnit pr kamp 
alle_gns <- alle_long %>%
  group_by(køn, event) %>%
  summarise(gennemsnit = mean(antal, na.rm = TRUE), .groups = "drop")

ggplot(alle_gns, aes(x = event, y = gennemsnit, fill = køn)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = round(gennemsnit, 1)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5) +
  labs(
    title = "Kvinder laver flere fejlafleveringer, mens mænd begår flere frispark og modtager flere kort",
    subtitle = "Gennemsnitlige hændelser pr. VM-kamp fordelt på køn - Kvinder: 116 kampe, Mænd: 147 kampe",
    caption = "Kilde: Statsbomb",
    x = "Hændelsestype",
    y = "Gennemsnit pr. kamp",
    fill = "Køn"
  ) +
  scale_fill_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF")) +
  theme_minimal()

#Plot 3 - relative andele 
andel_gns <- alle_gns %>%
  group_by(event) %>%
  mutate(
    sum_gns = sum(gennemsnit),
    andel_pct = round((gennemsnit / sum_gns) * 100, 1)
  )

ggplot(andel_gns, aes(x = event, y = andel_pct, fill = køn)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(andel_pct, "%")),
    position = position_stack(vjust = 0.5),
    color = "white", size = 4
  ) +
  labs(
    title = "Kvinder har flest fejlafleveringer – mænd flest fysiske hændelser i VM-kamp",
    subtitle = "Andel (%) af hændelser pr. type baseret på gennemsnit pr. kamp",
    caption = "Kilde: Statsbomb",
    x = "Hændelsestype",
    y = "Andel i %",
    fill = "Køn"
  ) +
  scale_fill_manual(values = c("Kvinder" = "red", "Mænd" = "#1E90FF")) +
  theme_minimal()

