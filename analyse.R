library(tidyverse)
library(dplyr)
library(ggrepel)
library(randomForest)
library(caret)

############################## Pré-traitement des données ###########################################################

#Importation des données 
footards <- read.csv2(file = "~/Documents/projet_R/final_data.csv", header = TRUE, sep = ',', dec = ',')

#Suppression des données N/A du dataset
footards <- footards[! is.na(footards$current_value), ]

#Conversion des données
tibble(footards) %>% 
  mutate(height = as.integer(height)) %>%
  mutate(goals = as.numeric(goals)) %>%
  mutate(age = as.integer(age))-> footards

#####################################################################################################################




############################## Analyse des données ##################################################################

##### Première Partie #####

# Analyse de la distribution de la taille des joueurs de foot
footards %>% ungroup %>%
  ggplot(aes(height)) + geom_density()

# Même chose en histogramme
footards %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) 

# On fais la même chose en fonction du poste de chaque joueur

# On mappe premièrement les rôles en quelques groupes principaux
main_roles = c('Defender', 'Attacker', 'Midfield', 'Goalkeeper')
roles = unique(footards$position)
mapping_roles = function(role){
  if (any(str_detect(str_to_lower(role),"attack"))) {
    return('Attacker')
  } else if (any(str_detect(str_to_lower(role),"defender"))) {
    return('Defender')
  } else if (any(str_detect(str_to_lower(role),"midfield"))) {
    return('Midfield')
  } else if (any(str_detect(str_to_lower(role),"goalkeeper"))) {
    return('Goalkeeper')
  }
} # idee ameliorer en utilisant des Factor avec operation de coarse

# On trace le graphique (attention très long à executer !!!!)
footards %>% 
  group_by(position) %>%
  mutate(role = mapping_roles(position)) %>%
  group_by(role) %>%
  mutate(taille_m = mean(height)) %>%
  group_by(role) %>%
  ggplot(aes(x=height,fill=role,alpha=0.1)) + geom_density() +
  geom_vline(aes(xintercept=taille_m, color=role), linetype="dashed")+
  geom_label(aes(x = taille_m, y = 0, label = round(taille_m, 1)), 
             color = "black", 
             fill = "white", 
             label.size = 0.25,  
             label.padding = unit(0.2, "lines"), 
             vjust = -0.5,
             hjust = 0,  
             size = 4) + 
  labs( x = "Taille des joueurs", 
        y = "Densité")  


mean(footards$height) #ça c'est pour la data




##### Deuxieme Partie #####

# On regroupe les équipes du top 5 des championnats
teams <- unique(footards$team)
teams_top5 <- top_5_teams <- c("Manchester United", "West Ham United", "Leicester City", "Aston Villa", "Wolverhampton Wanderers",
                               "Southampton FC", "Brighton & Hove Albion", "Everton FC", "Nottingham Forest", "Leeds United", 
                               "Crystal Palace", "Fulham FC", "AFC Bournemouth", "Brentford FC", "Liverpool FC", "Newcastle United", 
                               "Manchester City", "Arsenal FC", "Tottenham Hotspur", "Chelsea FC", "Borussia Mönchengladbach", 
                               "VfL Wolfsburg", "TSG 1899 Hoffenheim", "1.FC Union Berlin", "FC Augsburg", "VfB Stuttgart", 
                               "1.FSV Mainz 05", "1.FC Köln", "Hertha BSC", "FC Schalke 04", "VfL Bochum", "SV Werder Bremen", 
                               "Eintracht Frankfurt", "SC Freiburg", "Borussia Dortmund", "Bayer 04 Leverkusen", "Bayern Munich", 
                               "RB Leipzig", "Real Sociedad", "Villarreal CF", "Real Betis Balompié", "Athletic Bilbao", 
                               "Valencia CF", "Sevilla FC", "Celta de Vigo", "CA Osasuna", "Girona FC", "Rayo Vallecano", 
                               "RCD Mallorca", "UD Almería", "Real Valladolid CF", "Cádiz CF", "Elche CF", "RCD Espanyol Barcelona", 
                               "Getafe CF", "Atlético de Madrid", "Real Madrid", "FC Barcelona", "AS Roma", "SS Lazio", 
                               "ACF Fiorentina", "US Sassuolo", "Torino FC", "Udinese Calcio", "Bologna FC 1909", "AC Monza", 
                               "FC Empoli", "US Lecce", "Spezia Calcio", "Hellas Verona", "UC Sampdoria", "US Cremonese", 
                               "US Salernitana 1919", "Atalanta BC", "SSC Napoli", "Inter Milan", "AC Milan", "Juventus FC", 
                               "AS Monaco", "Stade Rennais FC", "Olympique Marseille", "OGC Nice", "RC Lens", "FC Nantes", 
                               "Montpellier HSC", "FC Lorient", "FC Toulouse", "ESTAC Troyes", "Stade Brestois 29", "Angers SCO", 
                               "AJ Auxerre", "Clermont Foot 63", "AC Ajaccio", "Olympique Lyon", "LOSC Lille", "Stade Reims", 
                               "RC Strasbourg Alsace", "Paris Saint-Germain")

mapping_team = function(team) {
  return(team %in% teams_top5) 
}

# Analyse de la valeur moyenne en fonction du poste 
# Deux barres : une si on est dans le top 5 et une autre sinon
footards %>%
  group_by(position) %>%
  mutate(top5 = mapping_team(team)) %>%
  group_by(position, top5) %>%
  summarise(avg_value = mean(current_value),top5) %>%   
  group_by(position) %>% 
  mutate(role = mapping_roles(position)) %>%
  ggplot(aes(x=reorder(role, -top5),y=avg_value,fill=top5)) +  geom_col(position="dodge") +
  scale_fill_manual(values = c("TRUE" = "gold", "FALSE" = "magenta")) +
  scale_y_continuous(labels = scales::comma_format()) +  
  labs(x = "Position", y = "Valeur moyenne des prix de transfert des joueurs",fill = "Top5"  )





##### Troisème Partie #####

# Valeur en fonction de l'âge du joueur et des awards qu'il a obtenu
# Avec médiane tracée en rouge

footards %>%
  ungroup() %>%
  ggplot(aes(x = age, y = current_value, color = award)) + 
  geom_point(alpha = 0.8) + 
  geom_vline(aes(xintercept = median(age, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) + 
  scale_color_gradientn(colors = c("gold", "magenta", "blue","black"),values = scales::rescale(c(0, 10, max(footards$award, na.rm = TRUE)))) + 
  scale_y_continuous(labels = scales::comma_format()) +
  labs( x = "Âge du joueur", y = "Valeur actuelle du joueur") +
  theme_gray()




###### Exprerience : pas retenu car pas très intéressant au final

#Rapport entre le nombre d'award et la valeur
#Ajout d'un mapping pour "regouper" un certain nombre d'ward dans la même catégorie (exemple, ceux qui ont moins de 5 award dans le même groupe,
#ceux qui en ont moins de 10 dans un deuxième..)
mapping_award = function(award){
  return(floor(award/10))
} 

footards %>%
  group_by(award) %>%
  mutate(award_5 = mapping_award(award)) %>%
  group_by(award_5) %>%
  summarise(avg_value=mean(current_value)) %>%
  ggplot(aes(x=award_5*10,y=avg_value)) + geom_point() +geom_line() 


#Rapport entre le nombre de but et la valeur
footards %>%
  ungroup %>%
  ggplot(aes(x=goals, y=current_value)) + geom_point() + theme_minimal()





#### Quatrième partie ####
# Entrainement du modèle sur tous les joueurs sauf les 20 meilleurs pour test sur le modèle plus tard
footards %>%
  arrange(highest_value) %>%
  head(10734) -> footards_nuls

#Partition du jeu de donnée
set.seed(42)
trainIndex <- createDataPartition(footards_nuls$current_value,p=0.8,list=FALSE)
trainData <- footards_nuls[trainIndex, ]
testData <- footards_nuls[-trainIndex, ]
set.seed(69)

#Création du modèle
model <- randomForest(current_value ~ age+height+goals+assists+days_injured+games_injured+award+minutes.played+appearance+position+team,data=trainData,ntree=500,importance=TRUE)
print(model)

#Calcul du score de prédiction sur le jeu de test
prediction <- predict(model,testData)
print(RMSE(prediction, testData$current_value))
print(sqrt(RMSE(prediction, testData$current_value)))

print(var(testData$current_value - prediction))
print(sqrt(var(testData$current_value - prediction)))

print(r_squared <- r_squared <- 1 - (sum((testData$current_value - prediction)^2) / sum((testData$current_value - mean(testData$current_value))^2)))

result <- data.frame(name=testData$name,Actual = testData$current_value,Predicted=prediction)


# Courbe des prédictions comparées aux réels valeurs
ggplot(result, aes(y = Actual, x = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Transfer Values",
       y = "Actual Transfer Value",
       x = "Predicted Transfer Value") +
  theme_minimal()

varImpPlot(model)

# Fonction pour réaliser une inférence
predict_transfer_value<-function(age, height, goals, assists, days_injured, games_injured, award, minutes.played, appearance, position, team) {
  player <- data.frame(
    age = age,
    height = height,
    goals=goals,
    assists=assists,
    days_injured=days_injured,
    games_injured=games_injured,
    award=award,
    minutes.played=minutes.played,
    appearance=appearance,
    position=position,
    team=team
  )
  
  prediction<-predict(model,player)
  return(prediction)
}

# predict_transfer_value(0,0,50,60,10000000000000,5000000000000,150,50000,110,"Attack Centre-Forward","Real Madrid")



#### Prediction des joueurs les plus chers ####

# Extraction des 20 joueurs les plus chers (en absolute value)
footards %>%
  arrange(desc(highest_value)) %>%
  head(20) -> plus_chers

# Predicton des 20 joueurs les plus cher
pred_plus_cher <- plus_chers %>%
  mutate(pred = predict_transfer_value(age, height,goals, assists, days_injured, games_injured, award, minutes.played, appearance, position, team))

# Test sur mbappé
mbappe <- plus_chers[1,]
print(mbappe)
predict_transfer_value(mbappe$age, mbappe$height, mbappe$goals, mbappe$assists, mbappe$days_injured, mbappe$games_injured, mbappe$award, mbappe$minutes.played, mbappe$appearance, mbappe$position, mbappe$team)     

# Plot des plus chers
plus_chers %>%
  ggplot(aes(x = current_value)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) 

# Plot des predictions vs prix réels sur les plus cher
pred_plus_cher %>%
  ggplot(aes(y = current_value, x = pred, color = team)) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    geom_text_repel(aes(label = name), vjust = -0.5, hjust = 1) + 
    labs(y = "Actual Transfer Value",
         x = "Predicted Transfer Value") +
    theme_minimal()



#### Creation d'une super équipe ####

# Vecteur roles
roles <- unique(footards$position)

# Recuperation du meilleur 11 en supprimant les roles redodants
footards %>%
  group_by(position) %>%
  arrange(desc(current_value)) %>%
  slice(1:1) %>%
  #top_n(1, current_value) %>%  
  filter(position!="Attack", position!="midfield", position!="Defender", position!="Attack-SecondStriker", position!="midfield-LeftMidfield") -> best_team

sum(best_team$current_value)

equipes_top <- unique(best_team$team)
best_team_prepared <- best_team %>% mutate(team = 'DreamTeam FC')
equipes_top_peuplees <- rbind(footards[which(footards$team %in% equipes_top),], best_team_prepared)

# Représentation de la valeur moyenne des joueurs des meilleurs équipes vs notre super équipe
equipes_top_peuplees %>%
  mutate(current_value = as.double(current_value)) %>%
  select(team, current_value) %>%
  group_by(team) %>%
  summarize(mean_val = mean(current_value)) %>%
  ungroup() %>%
  ggplot(aes(x=mean_val, y=fct_reorder(team, mean_val), fill=team)) +geom_col() + 
  labs(y = "Team",
       x = "Average player value") +
  theme_minimal()

#####################################################################################################################
