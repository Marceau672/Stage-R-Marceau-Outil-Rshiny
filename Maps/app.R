library(leaflet)
library(dplyr)
library(shiny)
library(ggplot2)
library(sf)

# Définir le répertoire de travail
setwd("C:/FIchier Projet R Marceau/Stage-R-Marceau")

# Charger les données de base et supprimer certaines régions
don <- read.csv2("donnees_com_2.csv")
don <- don[!(don$Nom_Region %in% c("Guyane", "Guadeloupe", "Mayotte", "La Réunion", "Martinique")), ]

# Charger les données géographiques des régions
region <- sf::st_read("carter/gadm41_FRA_1.shp") # 1 signifie qu'on a le découpage par région
region <- st_transform(region, 4326)

# Fonction pour créer une carte Leaflet
create_map <- function(region_data, variable, title, palette_colors) {
  min_val <- min(region_data[[variable]], na.rm = TRUE) # pour les valeurs de la légende
  max_val <- max(region_data[[variable]], na.rm = TRUE)
  custom_palette <- colorRampPalette(palette_colors)(25) # Dégradé de la palette de couleur 
  palette <- colorNumeric(palette = custom_palette, domain = c(min_val, max_val))
  
  leaflet(region_data) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~palette(get(variable)),
      color = "black",
      weight = 0.5,
      opacity = 0.6,
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
      label = ~paste(title, ":", get(variable))
    ) %>%
    addLegend(        # Légende de la carte 
      pal = palette,  # utilisation de la palette de couleur 
      values = ~get(variable),
      opacity = 0.7,
      title = title,
      position = "bottomright"
    )
}

# Interface utilisateur de l'application Shiny
ui <- fluidPage(
  titlePanel("Visualisation d'indicateur comptable lié à la forêt"), # Titre
  sidebarLayout( 
    sidebarPanel(
      selectInput("variable", "Sélectionner une variable:",
                  choices = c("Dépenses strictement forestières" = "Dépenses_1",
                              "Dépenses partiellement dédiées à la forêt" = "Dépenses_2",
                              "Recettes strictement forestières" = "Recettes_1",
                              "Recettes partiellement dédiées à la forêt" = "Recettes_2",
                              "Frais de Garderies" = "Frais_Garderies",
                              "Ventes de Bois" = "Ventes_Bois",
                              "Affouage" = "Affouage",
                              "Réserves Financières" = "Reserves_Finance",
                              "Part des dépenses forestières moyenne dans les dépenses totales (%)" = "Part_Dépenses_Foret",
                              "Part des recettes forestières moyenne dans les recettes totales (%)" = "Part_Recettes_Foret",
                              "Recettes par hectare de forêt" = "Recettes_par_hectares",
                              "Dépenses par hectare de forêt" = "Depenses_par_hectares")),
      checkboxGroupInput("annee", "Sélectionner une ou plusieurs années :", choices = unique(don$Année)) # Boutons pour séléctionner les années voulus
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Carte',
                 leafletOutput("ma_carte"), # Apparition de la carte 
                 textOutput("infoTexte") # Texte se trouvant en dessous de la carte, pour modification -> serveur  
        ),
        tabPanel('Région',
                 selectInput("regions", "Sélectionner une ou plusieurs régions :", choices = unique(don$Nom_Region), multiple = TRUE),
                 plotOutput("graphique") # Appartion de notre graphique pour voir l'évolution des régions sur une période choisi
        ),
        tabPanel('Commune',
                 selectizeInput("commune", "Saisir le nom de la commune :", choices = unique(don$LBUDG), multiple = FALSE, options = list(
                   placeholder = "Commencez à taper pour sélectionner" # Texte dans le widget ou se fera l'input de l'utilisateur
                 )),
                 tableOutput("table_moyennes") # Apparition de notre tableau de moyennes française et de la commnue
        ),
        tabPanel('Méta-données',
                 tags$div(                                  # Strucuture semblalble à celle d'un fichier HTML 
                   tags$p("Toutes les données présentées dans cet outil résultent de la fusion de multiples jeux de données :"),
                   tags$p(
                     tags$a("- La Direction des finances générales publiques, pour les données comptables des communes", 
                            href = "https://data.economie.gouv.fr/explore/?sort=modified&refine.publisher=DGFIP&q=Balances+comptables+communes")# HREF pour lien hypertexte
                   ),
                   tags$p(
                     tags$a("- Progedo, pour les données de recensement", 
                            href = "https://www.progedo.fr/") # HREF pour lien hypertexte
                   ),
                   tags$p(),
                   tags$p(
                     tags$a("- Données de l'IGN, concernant les superficies et caractéristiques des forêts par communes",
                            href="https://foret.ign.fr/catalogue/649adf3b99b2a690a04ef2c7")) # HREF pour lien hypertexte
                 ),
                 tags$p(),
                 tags$p("Traitement réalisé par Finance Marceau dans le cadre d'un stage réalisé au sein du Bureau d'Économie Théorique et Appliqué durant le mois de Juin-Juillet 2024."),
        )
      )
    )
  )
)

# Serveur de l'application Shiny
server <- function(input, output, session) {
  # Préparer les données agrégées par région et par variable
  data_list <- reactive({
    don_filtered <- don %>% filter(Année %in% input$annee)
    
    
    # Si aucune donnée  disponible sur les années sélectionné alors Notification d'erreur
    if (nrow(don_filtered) == 0) {
      stop("Aucune donnée disponible pour les années sélectionnées.")
    }
    
    # Création des listes de données associés aux input de l'utilisateur, ici Variable et nom de la région
    list(
      "Dépenses_1" = aggregate(Dépenses_1 ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Dépenses_2" = aggregate(Dépenses_2 ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Recettes_1" = aggregate(Recettes_1 ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Recettes_2" = aggregate(Recettes_2 ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Frais_Garderies" = aggregate(Frais_Garderies ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Ventes_Bois" = aggregate(Ventes_Bois ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Affouage" = aggregate(Affouage ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Reserves_Finance" = aggregate(Reserves_Finance ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Recettes_par_hectares" = aggregate(Recettes_par_hectares ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Depenses_par_hectares" = aggregate(Depenses_par_hectares ~ Nom_Region, data = don_filtered, FUN = sum, na.rm = TRUE),
      "Part_Dépenses_Foret" = aggregate(Part_Dépenses_Foret ~ Nom_Region, data = don_filtered, FUN = mean, na.rm = TRUE),
      "Part_Recettes_Foret" = aggregate(Part_Recettes_Foret ~ Nom_Region, data = don_filtered, FUN = mean, na.rm = TRUE)
    )
  })
  
  # Création de labels qui sont plus claire lors de l'appartition des variables dans les inputs et les légendes
  
  variable_labels <- c(
    "Dépenses_1" = "Dépenses strictement forestières en euros",
    "Dépenses_2" = "Dépenses partiellement dédiées à la forêt en euros",
    "Recettes_1" = "Recettes strictement forestières en euros",
    "Recettes_2" = "Recettes partiellement dédiées à la forêt en euros",
    "Frais_Garderies" = "Frais de Garderies en euros",
    "Ventes_Bois" = "Ventes de Bois en euros",
    "Affouage" = "Affouage en euros",
    "Reserves_Finance" = "Réserves Financières en euros",
    "Part_Dépenses_Foret" = "Part des dépenses forestières moyenne dans les dépenses totales (%)",
    "Part_Recettes_Foret" = "Part des recettes forestières moyenne dans les recettes totales (%)",
    "Recettes_par_hectares" = "Recettes par hectare de forêt en euros",
    "Depenses_par_hectares" = "Dépenses par hectare de forêt en euros"
  )
  
  # Palette de couleur pour les variables choisi, si vous rajoutez une variable il faut lui définir une palette de couleur pour la carte
  
  data_palette <- list(
    "Dépenses_1" = c("#BFFFC2", "#005004"),
    "Dépenses_2" = c("#BFFFC2", "#005004"),
    "Recettes_1" = c("#E5FF99", "#9ED500"),
    "Recettes_2" = c("#E5FF99", "#9ED500"),
    "Frais_Garderies" = c("#BCE9FF", "#005179"),
    "Ventes_Bois" = c("#D4CCFF", "#2700FF"),
    "Affouage" = c("#D4CCFF", "#2700FF"),
    "Reserves_Finance" = c("#FFD700", "#8B4513"),
    "Part_Dépenses_Foret" = c("#D4CCFF","#2700FF"),
    "Part_Recettes_Foret" = c("#D4CCFF","#2700FF"),
    "Recettes_par_hectares" = c("#E5FF99", "#9ED500"),
    "Depenses_par_hectares" = c("#BFFFC2", "#005004")
  )
  
  # Observer les changements d'input et mettre à jour la carte
  observe({
    selected_data <- tryCatch({
      data_list()[[input$variable]]
    }, 
    # Si aucune donnée  disponible sur les années sélectionné alors Notification d'erreur
    error = function(e) {
      showNotification("Erreur : Aucune donnée disponible pour les années sélectionnées.", type = "error")
      return(NULL)
    })
    
    if (is.null(selected_data) || nrow(selected_data) == 0) return(NULL)
    
    # fais apparâitre le bon nom et la bonne couleur en fonction de la variable choisi
    selected_palette <- data_palette[[input$variable]]
    variable_label <- variable_labels[[input$variable]]
    
    # Corriger le nom de la région Île-de-France
    selected_data <- selected_data %>%
      mutate(Nom_Region = recode(Nom_Region, "Ile-de-France" = "Île-de-France"))
    
    # Fusionner les données géographiques avec les données sélectionnées
    region_data <- region %>%
      left_join(selected_data, by = c("NAME_1" = "Nom_Region"))
    
    # Afficher la carte mise à jour
    output$ma_carte <- renderLeaflet({
      create_map(region_data, input$variable, variable_label, selected_palette)
    })
  })
  
  # Afficher un texte explicatif
  output$infoTexte <- renderText({
    "Lorsque plusieurs années sont sélectionnées il s'agit d'une somme de valeur et non d'une moyenne, sauf pour les parts de recettes et dépenses."
  })
  
  # Créer un graphique de l'évolution d'une variable par région et année
  output$graphique <- renderPlot({
    req(input$regions, input$variable, input$annee)
    
    region_data <- don %>%
      filter(Nom_Region %in% input$regions, Année %in% input$annee) %>%
      group_by(Nom_Region, Année) %>%
      summarise(mean_value = mean(!!sym(input$variable), na.rm = TRUE))
    # Si aucune donnée  disponible sur les années sélectionné alors Notification d'erreur
    if (nrow(region_data) == 0) {
      showNotification("Erreur : Aucune donnée disponible pour les années et les régions sélectionnées.", type = "error")
      return(NULL)
    }
    
    ggplot(region_data, aes(x = as.factor(Année), y = mean_value, color = Nom_Region, group = Nom_Region)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      labs(x = "Année", y = variable_labels[[input$variable]], title = paste("Évolution moyenne de", variable_labels[[input$variable]])) +
      theme_minimal() +
      scale_color_discrete(name = "Région")
  })
  
  # Afficher un tableau comparatif des moyennes par commune et pour la France entière
  output$table_moyennes <- renderTable({
    req(input$annee, input$commune)
    # filtrage en fonction des input de l'utilisateur 
    don_filtered <- don %>% filter(Année %in% input$annee)
    don_commune <- don_filtered %>% filter(LBUDG %in% input$commune)
    
    # Si aucune donnée  disponible sur les années sélectionné alors Notification d'erreur
    if (nrow(don_filtered) == 0) {
      showNotification("Erreur : Aucune donnée disponible pour les années sélectionnées.", type = "error")
      return(NULL)
    }
    
    # Calcul des moyennes francaise sur la période sélectionnée
    moyennes_france <- don_filtered %>%
      summarise(across(c(Dépenses_1, Dépenses_2, Recettes_1, Recettes_2, Frais_Garderies, Ventes_Bois, Affouage, Reserves_Finance, Recettes_par_hectares, Depenses_par_hectares, Part_Dépenses_Foret, Part_Recettes_Foret), mean, na.rm = TRUE))
    moyennes_france <- as.data.frame(t(moyennes_france))
    colnames(moyennes_france) <- c("Moyenne Française")
    
    # Calcul des moyennes de la commune sur la période sélectionnée
    moyennes_communes <- don_commune %>%
      summarise(across(c(Dépenses_1, Dépenses_2, Recettes_1, Recettes_2, Frais_Garderies, Ventes_Bois, Affouage, Reserves_Finance, Recettes_par_hectares, Depenses_par_hectares, Part_Dépenses_Foret, Part_Recettes_Foret), mean, na.rm = TRUE))
    moyennes_communes <- as.data.frame(t(moyennes_communes))
    colnames(moyennes_communes) <- paste("Moyenne ", unique(don_commune$LBUDG), sep = " ")
    
    # Aggrégation des données dans une matrice pour les faires apparaitres dans notre tableau
    moyennes <- cbind(moyennes_france, moyennes_communes)
    moyennes <- data.frame(Variable = rownames(moyennes), moyennes, row.names = NULL)
    moyennes$Variable <- recode(moyennes$Variable, !!!variable_labels)
    
    return(moyennes)
  })
  
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
