library(tidyverse)
library(dplyr)

footards <- read.csv2(file = "~/Documents/projet_R/final_data.csv", header = TRUE, sep = ',', dec = ',')



tibble(footards) %>% 
  mutate(height = as.integer(height)) %>%
  mutate(goals = as.numeric(goals)) %>%
  mutate(age = as.integer(age))-> footards




# distribution de la taille des joueurs de footsz
footards %>% ungroup %>%
  ggplot(aes(height)) + geom_density()

footards %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) 
#labs(title = "Répartition des tailles des joueurs de foot", x = "Taille (en cm)", y = "Fréquence")

#On peut essayer de faire l'analyse pour le gardien, l'ailier et le défenseur pour voir les différences (voir à la fin je l'ai fais)





# value en fonction de l'âge + nombre de awards
footards %>%
  ungroup %>%
  ggplot(aes(x=age, y=current_value, color=award)) + geom_point(alpha=0.2) + geom_vline(aes(xintercept = median(age, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) + theme_minimal()




# value en fonction de la position
# on mappe les rôle en quelques groupes principaux
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

footards %>%
  group_by(position) %>%
  summarise(avg_value = mean(current_value)) %>%   
  group_by(position) %>% 
  mutate(role = mapping_roles(position)) %>%
  ggplot(aes(x=role, y=avg_value)) +  geom_col()



# average valeur par position





# rapport awards et value et comparer avec rapport buts/value

#rapport awards et value
#Ajout d'un mapping pour "regouper" un certain nombre d'ward dans la même catégorie (exemple, ceux qui ont moins de 5 award dans le même groupe,
#ceux qui en ont moins de 10 dans un deuxième..)
mapping_award = function(award){
  return(floor(award/2))
} 

footards %>%
  group_by(award) %>%
  mutate(award_5 = mapping_award(award)) %>%
  group_by(award_5) %>%
  summarise(avg_value=mean(current_value)) %>%
  ggplot(aes(x=award_5*3,y=avg_value)) + geom_point() +geom_line() 


# value en fonction du nb de goals (pas interessant)
footards %>%
  ungroup %>%
  ggplot(aes(x=goals, y=current_value)) + geom_point() + theme_minimal()




# distribution de la taille des joueurs de foots en fonction de leur poste
footards %>% 
  group_by(position) %>%
  mutate(role = mapping_roles(position)) %>%
  ggplot(aes(x=height,fill=role,alpha=0.1)) + geom_density()

