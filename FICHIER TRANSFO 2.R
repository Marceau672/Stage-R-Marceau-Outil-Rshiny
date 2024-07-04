# ------------- librairies et Fonction -------------

library(tidyverse)
library(readxl)


ajouter_zero <- function(codes) {
  codes <- as.character(codes) # Assurez-vous que les codes sont de type caractère
  codes <- ifelse(nchar(codes) == 4, paste0("00", codes),
                  ifelse(nchar(codes) == 5, paste0("0", codes), codes))
  return(codes)
}

# ------------- Import -------------
# Initialisation des dataframes
BudgetTotal <- data.frame()
BudgetForet <- data.frame()
CommuneTYPE1 <- data.frame()
unique_communes_df <- data.frame()
Reserves_df <- data.frame()
Frais_Garderies_df <- data.frame()
Frais_Exploitation_df <- data.frame()
Ventes_Bois_df <- data.frame()
Affouage_df <- data.frame()

for (i in 2010:2022) {
  fich = paste0("Data/balances-comptables-des-communes-en-",i, ".csv")
  tab <- read_csv2(fich, show_col_types = FALSE)
  
  tab <- tab %>% 
    mutate(
      OBNETDEB = as.numeric(OBNETDEB),
      OBNETCRE = as.numeric(OBNETCRE),
      SD = as.numeric(SD)
    )
  
  if ("siren" %in% names(tab)) {
    tab <- tab %>% rename(SIREN = siren)
  }
  
  # Summarize total budget
  Total <- tab %>%
    group_by(SIREN, COMPTE) %>%
    summarise(SD = sum(OBNETDEB, na.rm = TRUE), SC = sum(OBNETCRE, na.rm = TRUE)) %>%
    mutate(Année = i)
  
  BudgetTotal <- bind_rows(BudgetTotal, Total)
  # Affectation des niveaux d'importance pour les comptes concennées 
  BudgetTotal <- BudgetTotal %>%
    mutate(
      Importance = case_when(
        COMPTE %in% c(7022, 7023, 7025, 61524) ~ "1",
        COMPTE %in% c(7021, 7024, 7028, 7035, 7037, 6282) ~ "2",
        TRUE ~ "3"
      )
    )
  
  print(paste("Budget total de l'année", i, "......... terminé"))
  
  
  
  Reserves <- tab %>%
    filter(COMPTE == 515) %>%
    group_by(SIREN) %>%
    summarise(Reserves_Finance = sum(SD, na.rm = TRUE)) %>%
    mutate(Année = i)
  Reserves_df <- bind_rows(Reserves_df, Reserves)
  
  Frais_Garderies <- tab %>%
    filter(COMPTE == 6282) %>%
    group_by(SIREN) %>%
    summarise(Frais_Garderies = sum(OBNETDEB, na.rm = TRUE)) %>%
    mutate(Année = i)
  Frais_Garderies_df <- bind_rows(Frais_Garderies_df, Frais_Garderies)
  
  Frais_Exploitation <- tab %>%
    filter(COMPTE == 61524) %>%
    group_by(SIREN) %>%
    summarise(Frais_Exploitation = sum(OBNETDEB, na.rm = TRUE)) %>%
    mutate(Année = i)
  Frais_Exploitation_df <- bind_rows(Frais_Exploitation_df, Frais_Exploitation)
  
  Ventes_Bois <- tab %>%
    filter(COMPTE == 7022) %>%
    group_by(SIREN) %>%
    summarise(Ventes_Bois = sum(OBNETCRE, na.rm = TRUE)) %>%
    mutate(Année = i)
  Ventes_Bois_df <- bind_rows(Ventes_Bois_df, Ventes_Bois)
  
  Affouage <- tab %>%
    filter(COMPTE == 7025) %>%
    group_by(SIREN) %>%
    summarise(Affouage = sum(OBNETCRE, na.rm = TRUE)) %>%
    mutate(Année = i)
  Affouage_df <- bind_rows(Affouage_df, Affouage)
  
  
  if (i >= 2010 & i <= 2015) {
    filtered_tab <- tab %>% 
      filter(BUDGET == "BP") %>%
      group_by(SIREN) %>%
      slice(which.max(!is.na(INSEE))) %>%
      summarise(
        LBUDG = first(LBUDG), 
        NDEPT = first(NDEPT),
        CREGI = first(CREGI),
        INSEE = as.character(first(INSEE))
      ) %>%
      mutate(Année = i) %>%
      distinct(SIREN, .keep_all = TRUE) %>%
      select(-Année)
  } else if (i >= 2016 & i <= 2022) {
    filtered_tab <- tab %>% 
      filter(CTYPE == 101) %>%
      group_by(SIREN) %>%
      slice(which.max(!is.na(INSEE))) %>%
      summarise(
        LBUDG = first(LBUDG), 
        NDEPT = first(NDEPT),
        CREGI = first(CREGI), 
        INSEE = as.character(first(INSEE))
      ) %>%
      mutate(Année = i) %>%
      distinct(SIREN, .keep_all = TRUE) %>%
      select(-Année)
  }
  
  # Ajouter les données filtrées à la dataframe globale
  if (exists("filtered_tab")) {
    unique_communes_df <- bind_rows(unique_communes_df, filtered_tab)
    rm(filtered_tab) # Supprimer la variable intermédiaire
  }
}


# Création de dataframe qui va pour le nom des différentes communes 
unique_communes_df <- unique_communes_df %>%
  distinct(SIREN, .keep_all = TRUE)

unique_communes_df$`INSEE commune` <- paste0(unique_communes_df$NDEPT,unique_communes_df$INSEE)
unique_communes_df$`INSEE commune` <- ajouter_zero(unique_communes_df$`INSEE commune`)

index <- which(unique_communes_df$LBUDG == "COMMUNE DE BORNEL")
unique_communes_df$`INSEE commune`[index] <- "060088"



# Import des donées concernant les caractéristiques des forets
surface_boise <- read_excel("surface_boise.xlsx")



surface_boise <- surface_boise %>%
  mutate(SIREN = as.character(SIREN_EPCI),
         Année = as.integer(`annee PVA`))
## A vérifier car nous avons toutes les dépenses et recettes additionner 


BudgetTotal <- BudgetTotal %>%
  left_join(Reserves, by = c("SIREN", "Année"))

aggregated_df <- BudgetTotal %>%
  group_by(SIREN, Année, Importance) %>%
  summarise(
    Dépenses = sum(SD, na.rm = TRUE),
    Recettes = sum(SC, na.rm = TRUE)
  )



# Pivoter la table pour avoir une seule ligne par SIREN avec des colonnes pour chaque importance
pivot_Total <- aggregated_df %>%
  pivot_wider(names_from = Importance, 
              values_from = c(Dépenses, Recettes), 
              names_sep = "_") %>%
  group_by(SIREN)


pivot_Total <- pivot_Total %>%
  left_join(Reserves_df, by = c("SIREN", "Année"))%>%
  left_join(Frais_Garderies_df, by = c("SIREN", "Année"))%>%
  left_join(Frais_Exploitation_df, by = c("SIREN", "Année"))%>%
  left_join(Ventes_Bois_df, by = c("SIREN", "Année"))%>%
  left_join(Affouage_df, by = c("SIREN", "Année"))

# Différencie les communes forestières économiquement et celle qui ne le sont pas 
#pivot_Total <- pivot_Total %>%
  #filter(any(Dépenses_1 >= 0 | Dépenses_2 >= 0 | Recettes_1 >= 0 | Recettes_2 >= 0))

pivot_Total <- pivot_Total %>%
  complete(Année = full_seq(c(2010:2022), 1), 
           fill = list(Dépenses_1 = NA, Dépenses_2 = NA, Recettes_1 = NA, Recettes_2 = NA)) %>%
  left_join(unique_communes_df, by = "SIREN")

#------------------------------------------------------------------------------------------------------------------------------------------------------------

# Appliquer la fonction à la colonne INSEE Commune
surface_boise$`INSEE commune` <- ajouter_zero(surface_boise$`INSEE commune`)

# Jointure 
pivot_Total <- pivot_Total %>% 
  left_join(select(surface_boise, `INSEE commune`, `Taux boisement`,`Surface commune`,`Surface foret`,feuillus,conifères,mixtes,`sans couvert`,`annee PVA`), by = "INSEE commune")
#------------------------------------------------------------------------------------------------------------------------------------------------------------



pivot_Total <- pivot_Total %>%
  rename( Surface_commune_ha = `Surface commune`,
          Surface_foret_ha = `Surface foret`,
          feuillus_ha = feuillus,
          conifères_ha = conifères,
          mixtes_ha = mixtes,
          sans_couvert_ha = `sans couvert`
          
  )


# Drop toutes les lignes ou surface foret est inférieur a 0.5 Ha
#pivot_Total <- subset(pivot_Total, pivot_Total$Surface_foret_ha >= 0.5)


surface_foret <- read.csv2("emplacement-forets-communales.csv")

surface_foret$CodeINSEE <- as.character(surface_foret$CodeINSEE)
surface_foret$`INSEE commune` <- ajouter_zero(surface_foret$CodeINSEE)
surface_foret$CodeINSEE <- NULL
surface_foret <- surface_foret %>%
  filter(frt_com == 1 ) %>%
  select(area_ha,frt_com,`INSEE commune`,coordX,coordY,geo_shape,iidtn_frt) 
surface_foret <- surface_foret %>%
  group_by(`INSEE commune`) %>%
  summarise( sum_area_ha_frt_com = sum(area_ha),
             Nb_foret_com = sum(frt_com),
             coordX = coordX,
             coordY = coordY,
             geo_shape = geo_shape,
             iidtn_frt = iidtn_frt)



pivot_Total <- pivot_Total %>%
  left_join(select(surface_foret,`INSEE commune`,Nb_foret_com,sum_area_ha_frt_com,iidtn_frt, coordX,coordY,geo_shape),by ="INSEE commune")


pivot_Total$Dépenses_Total <- rowSums(pivot_Total[, c("Dépenses_1", "Dépenses_2", "Dépenses_3")], na.rm = TRUE)
pivot_Total$Recettes_Total <- rowSums(pivot_Total[, c("Recettes_1", "Recettes_2", "Recettes_3")], na.rm = TRUE)
pivot_Total$Part_Dépenses_Foret <- round((rowSums(pivot_Total[, c("Dépenses_1", "Dépenses_2")]) / pivot_Total$Dépenses_Total)*100,6)
pivot_Total$Part_Recettes_Foret <- round((rowSums(pivot_Total[, c("Recettes_1", "Recettes_2")]) / pivot_Total$Recettes_Total)*100,6)



#Fichier de recensement de PROGEDO
bdcom <- read.csv2("Recensement/bdcom21.csv",
                   encoding = "UTF-8")

bdcom$`INSEE commune` <- ajouter_zero(bdcom$COM)
bdcom <- bdcom %>% 
  select(NCC,`INSEE commune`,PTOT19,PTOT13,PTOT08)






# Rassemblement de tout les arrondissement de paris ensemble sous le même 
donnees_paris <- bdcom %>% 
  filter(grepl("Paris [0-9]+", NCC))
donnees_paris$`INSEE commune` <- 075056
donnees_paris$NCC <- "Paris"
# Regrouper les données et sommer les valeurs numériques
donnees_paris$`INSEE commune` = as.character(donnees_paris$`INSEE commune`)
donnees_paris <- donnees_paris %>%
  group_by(NCC, `INSEE commune`) %>%
  summarise(PTOT19 = sum(PTOT19),
            PTOT13 = sum(PTOT13),
            PTOT08 = sum(PTOT08))

bdcom <- bdcom %>%
  filter(!grepl("Paris [0-9]+", NCC))

bdcom <- bind_rows(bdcom,donnees_paris)
bdcom$`INSEE commune` <- ajouter_zero(bdcom$`INSEE commune`)

donnees_combines <- pivot_Total %>%
  left_join(bdcom, by = "INSEE commune")%>%
  mutate(PTOT = case_when(
    Année %in% 2010:2012 ~ PTOT08,
    Année %in% 2013:2018 ~ PTOT13,
    Année %in% 2019:2022 ~ PTOT19,
    TRUE ~ NA_real_
  )) %>%
  select(-PTOT08, -PTOT13, -PTOT19, -NCC)





# On met les nouveaux noms de régions dans le fichier

region <- read_excel("region.xlsx")

region <- region %>%
  mutate(CREGI = as.numeric(`Anciens Code`),
         Nouveau_Code_Region = `Nouveau Code`,
         Nom_Region = `Nouveau Nom`)

region$SIREN <- NULL


donnees_combines$CREGI <- as.numeric(donnees_combines$CREGI)

donnees_combines <- donnees_combines %>%
  left_join(select(region, CREGI, Nom_Region,Nouveau_Code_Region), by ="CREGI")



donnees_combines$Recettes_par_hectares <- round((donnees_combines$Recettes_1 + donnees_combines$Recettes_2) / donnees_combines$Surface_foret_ha)
donnees_combines$Depenses_par_hectares <- round((donnees_combines$Dépenses_1 + donnees_combines$Dépenses_2) / donnees_combines$Surface_foret_ha)
donnees_combines$NDEPT <- as.character(donnees_combines$NDEPT)
donnees_combines$NDEPT <- enlever_zero(donnees_combines$NDEPT)



write.csv2(donnees_combines,file = "donnees_com_2.csv", row.names = TRUE)



