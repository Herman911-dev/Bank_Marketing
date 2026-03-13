library(ggplot2)
library(class)
library(shiny)
bank_data <- read.csv2("C:/Users/herma/Desktop/Bureau/Ynov/Analyse et exploration de données/TP5/bank-full.csv")
colnames(bank_data) <- c("age", "job", "marital", "education", "default", "balance", 
                       "housing", "loan", "contact", "day", "month", "duration",
                       "campaign", "pdays", "previous", "poutcome", "y")
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$education <- as.factor(bank_data$education)
bank_data$y <- as.factor(bank_data$y)
bank_data$month <- as.factor(bank_data$month)
View(bank_data)
summary(bank_data)

# On regarde la repartition de y
table(bank_data$y)
prop.table(table(bank_data$y)) * 100 #88.3% de non et 11.7% de oui

# On visualise y
barplot(table(bank_data$y),
        main="Répartition des souscriptions (y)", 
        col=c("red", "green"), 
        ylab="Nombre de clients")

# On cherche les segments à cibler en priorité 
ggplot(bank_data, aes(x = job, fill = y)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Proportion de souscription par métier", 
       y = "Pourcentage", x = "Type d'emploi")

#On voit ici que les segments à cibler en priorité sont les étudiants et les retraités,
#Ils sont donc les cibles principales pour la campagne

colSums(bank_data == "unknown")
#Nous avons beaucoup de cases vides notamments dans la catégorie poutcome

boxplot(bank_data$balance) 
#On voit ici qu'il y a beaucoup de valeurs aberrantes certains clients sont trop riches

# Calcul des quartiles
Q1 <- quantile(bank_data$balance, 0.25)
Q3 <- quantile(bank_data$balance, 0.75)
IQR_val <- Q3 - Q1

# Définir les limites 
borne_inf <- Q1 - 1.5 * IQR_val
borne_sup <- Q3 + 1.5 * IQR_val

sum(bank_data$balance > borne_sup) 
sum(bank_data$balance < borne_inf)
#On regarde combien de données sont considérés comme aberrantes on constate 4712 au dessus 
#et 17 en dessous donc 4729 !

#Créer une nouvelle base
bank_data2 <- subset(bank_data, balance >= borne_inf & balance <= borne_sup)

#Vérifier combien de lignes ont été supprimées
nrow(bank_data) - nrow(bank_data2)
#On retrouve bien le nombre 4729.

mean(bank_data$balance)
mean(bank_data2$balance)
sd(bank_data$balance)
sd(bank_data2$balance)

#Dans le but de notre démarche on peut supprimer plusieurs variable comme day, month
#Le mois et le jour du contact ne nous interessent pas plus que ça 
bank_data2$day <- NULL
bank_data2$month <- NULL
boxplot(bank_data2$balance)
#Avec notre boxplot on voit qu'il n'y a pas de valeurs aberrantes nos résultats ne seront pas faussés

#On va ensuite traiter les valeurs unknow en vérifiants où elle se trouve
colSums(bank_data2 == "unknown")

# On renomme "unknown" pour les variables où l'absence d'info a du sens
# Comme ce sont des facteurs, on modifie directement les niveaux 
bank_data2$poutcome[bank_data2$poutcome == "unknown"] <- "jamais_contacte"
bank_data2$contact[bank_data2$contact == "unknown"] <- "non_renseigne"

#On en profite pour transformer ces deux colonnes en facteurs
bank_data2$poutcome <- as.factor(bank_data2$poutcome)
bank_data2$contact <- as.factor(bank_data2$contact)

#On traite "unknown" comme de vraies valeurs manquantes (NA) pour 'job' et 'education'
#On remplace directement par NA
bank_data2$job[bank_data2$job == "unknown"] <- NA
bank_data2$education[bank_data2$education == "unknown"] <- NA

#On supprime les lignes qui contiennent maintenant des NA car jugés inutile 
bank_data2 <- na.omit(bank_data2)

#Nettoyage des niveaux vides
bank_data2$job <- droplevels(bank_data2$job)
bank_data2$education <- droplevels(bank_data2$education)

#On vérifie si tout à bien fonctionné et c'est le cas ici ! Plus de valeurs manquantes.
colSums(bank_data2 == "unknown")
View(bank_data2)

# --- ANALYSE BIVARIÉE : VARIABLES NUMÉRIQUES ---

#Sélection des variables strictement numériques de bank_data2
vars_num <- bank_data2[, c("age", "balance", "duration", "campaign", "pdays", "previous")]

#Calcul de la matrice de corrélation (coefficients de Pearson)
matrice_cor <- cor(vars_num, method = "pearson")

# On arrondit à 2 décimales pour que ce soit plus lisible dans la console
print(round(matrice_cor, 2))
# L'analyse bivariée numérique a démontré que le profil financier et démographique des clients 
# (âge, solde) est décorrélé de la manière dont ils sont démarchés (nombre d'appels, durée).
# Car on a un résultat assez loin de 0 ici 0.45

#Visualisation : Scatter plot 
#Exemple pour analyser la relation entre l'âge (age) et le solde bancaire (balance)
ggplot(bank_data2, aes(x = age, y = balance)) +
  geom_point(alpha = 0.4, color = "darkblue") + 
  theme_minimal() +
  labs(title = "Relation entre l'Âge et le Solde Bancaire",
       x = "Âge du client",
       y = "Solde moyen annuel (€)")
#Grâce à ce graphique on voit plusieurs choses notamment que la suppression de valeurs aberrantes
#à fonctionner, et que cela notre première trouvaille que les retraités sont une cible majoritaire
#Dans le but de notre démarche car très rarement dans le négatif !


ggplot(bank_data2, aes(x = housing, fill = y)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "palegreen3")) +
  theme_minimal() +
  labs(title = "Proportion de souscription selon le prêt immobilier",
       x = "Le client a-t-il un prêt immobilier en cours ? (housing)",
       y = "Pourcentage de clients (Proportion)",
       fill = "Souscription (y)")
#On voit ici que les clients sans prêt ont plus de chances d'accepter


#On va essayer de voir si le niveau d'éducation influence la souscription (y)
ggplot(bank_data2, aes(x = education, fill = y)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "palegreen3")) +
  theme_minimal() +
  labs(title = "Proportion de souscription au dépôt à terme par niveau d'éducation",
       x = "Niveau d'éducation",
       y = "Pourcentage de clients (Proportion)",
       fill = "Souscription (y)")
#Avec ce graphique de barplot on voit très clairement que le niveau d'études influe sur y
#Car plus le client à un niveau élevé d'étude plus il sera apte à souscrire à un prêt 

# --- TEST DU KHI-DEUX ---
#Prouver que la variable 'job' et 'education' influence la variable 'y'

#On crée un tableau croisé entre les deux variables
tableau <- table(bank_data2$job, bank_data2$y)
tableau2 <- table(bank_data2$education, bank_data2$y)
print(tableau)
print(tableau2)

#On lance le test statistique et on l'affiche
test_khi <- chisq.test(tableau)
test_khi2 <- chisq.test(tableau2)
print(test_khi)
print(test_khi2)
#Ici on voit clairement que notre p-value est < 0.05 
#donc la variable job et education influe énormement sur la variable y 
#Comme constaté sur le graphique un peu au dessus

# --- MODÉLISATION (KNN) ---

#Séparation de la cible (y) et des variables explicatives (X)
cible_y <- bank_data2$y
donnees_X <- bank_data2[, !names(bank_data2) %in% "y"]

#Encodage des variables catégoriques (One-Hot Encoding)
# 'model.matrix' transforme automatiquement le texte en colonnes de 0 et 1
X_encode <- model.matrix(~ . - 1, data = donnees_X)

#Normalisation (Mise à l'échelle des données)
# 'scale' centre et réduit toutes les colonnes pour qu'elles aient la même importance
X_scale <- scale(X_encode)
X_scale <- as.data.frame(X_scale)

#Création des échantillons d'entraînement (70%) et de test (30%)
set.seed(42) # Permet de figer l'aléatoire pour avoir toujours le même résultat
index_train <- sample(1:nrow(X_scale), 0.7 * nrow(X_scale))

X_train <- X_scale[index_train, ]
X_test  <- X_scale[-index_train, ]
y_train <- cible_y[index_train]
y_test  <- cible_y[-index_train]

#Entraînement du modèle KNN et Prédiction sur l'échantillon de test
#On choisit k = 5 (on regarde les 5 "voisins" les plus proches pour prendre la décision)
predictions_knn <- knn(train = X_train, test = X_test, cl = y_train, k = 5)

#On ffichage des 20 premières prédictions pour voir à quoi ça ressemble
print("--- Les 20 premières prédictions du modèle ---")
print(head(predictions_knn, 20))
#On voit ici que notre modèle à prédit 95% de non, en début de projet on avait trouvé 89%
#Le modèle semble être assez proche de la réalité 

# --- ÉVALUATION DU MODÈLE ---

#Création de la matrice de confusion
#On compare la réalité (y_test) avec les prédictions du modèle (predictions_knn)
matrice_confusion <- table(Realite = y_test, Prediction = predictions_knn)

print("--- Matrice de Confusion ---")
print(matrice_confusion)
#9996 (Vrais Négatifs) : Le modèle a dit "Ils vont refuser", et ils ont effectivement refusé. La banque a économisé 9996 coups de téléphone inutiles !
#407 (Vrais Positifs) : Le modèle a dit "Ils vont accepter", et ils ont souscrit. Ce sont les vrais succès de la campagne.
#289 (Faux Positifs) : Le modèle a dit "Ils vont accepter", mais le client a refusé au téléphone. Ce sont des appels passés pour rien.
#921 (Faux Négatifs) : Le modèle a dit "Ils vont refuser", donc on ne les a pas appelés... mais en réalité, ils auraient dit "Oui" !

#Calcul de l'Accuracy (pourcentage de prédictions correctes)
#On additionne la diagonale (les bonnes réponses) divisé par le total
accuracy <- sum(diag(matrice_confusion)) / sum(matrice_confusion)
print(paste("Accuracy :", round(accuracy * 100, 2), "%"))
#Accuracy de 89,58%, pas très pertinent ici car les clients disent très souvent non, 
#88.3% vu au début du projet, le modèle fait à peine mieux 

#Calcul de la Précision (parmi les "yes" prédits, combien sont vraiment "yes")
#Vrais Positifs (le modèle dit yes, le client dit yes)
vrais_positifs <- matrice_confusion["yes", "yes"]
#Faux Positifs (le modèle dit yes, mais le client a dit no)
faux_positifs <- matrice_confusion["no", "yes"]

precision <- vrais_positifs / (vrais_positifs + faux_positifs)
print(paste("Précision :", round(precision * 100, 2), "%"))
#On trouve 58.48% de précision, le modèle permet aux commerciaux de la banque d'être 5 fois plus 
#efficaces lors de leurs appels. Le retour sur investissement de ton analyse de données est prouvé.

#On transforme la table en format compatible avec ggplot
df_confusion <- as.data.frame(matrice_confusion)

#Création de la "Heatmap" 
ggplot(data = df_confusion, aes(x = Prediction, y = Realite, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6, fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Matrice de Confusion du Modèle KNN",
       subtitle = "Évaluation des performances de prédiction",
       x = "Prédiction du Modèle",
       y = "Réalité (Ce que le client a fait)")
#La Visualisation de la matrice obtenue plus haut

#Cibles prioritaires : Concentrer les efforts d'appels téléphoniques sur les étudiants et les retraités. 
#Les graphiques bivariés ont également montré que les clients avec un niveau d'éducation supérieur (tertiary) transforment beaucoup mieux.
#Cibles à éviter (ou à appeler en dernier) : Les profils ouvriers par exemple
#ou les profils ayant un prêt immobilier en cours car leur budget est souvent plus contraint.

#Scénario 1 : La campagne agressive (Maximiser les parts de marché). 
#On appelle tout le monde, quitte à déranger des gens pour rien (accepter les faux positifs). 
#Le coût marketing explose, mais on ne rate aucun client potentiel (on réduit les faux négatifs).

#Scénario 2 : La campagne économe (Maximiser le ROI). On utilise le modèle KNN. 
#On n'appelle que les clients que le modèle a classés "yes". 
#Le volume d'appels chute considérablement, les coûts du centre d'appels s'effondrent
#mais le taux de conversion passe de 11,7% à près de 58% ! 
#C'est le scénario idéal en cas de contraintes budgétaires.

# 1. L'Interface Utilisateur (UI) : Les filtres disponibles
ui <- fluidPage(
  titlePanel("Simulateur de Campagne Marketing Bancaire"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Définir le segment cible :"),
      helpText("Ajustez ces filtres pour simuler différentes cibles."),
      
      # Curseur pour l'âge maximum
      sliderInput("age_max", "Âge maximum du client :", 
                  min = 18, max = 100, value = 90),
      
      # Curseur pour le solde minimum (on met des limites logiques suite à ton nettoyage IQR)
      sliderInput("balance_min", "Solde bancaire minimum (€) :", 
                  min = -2000, max = 4000, value = 0),
      
      # Menu déroulant pour le métier (avec option "Tous")
      selectInput("job_input", "Métier :", 
                  choices = c("Tous", as.character(unique(bank_data2$job)))),
      
      # Menu déroulant pour le prêt immobilier (avec option "Tous")
      selectInput("housing_input", "A un prêt immobilier ? (housing) :", 
                  choices = c("Tous", "yes", "no"))
    ),
    
    # 2. Zone principale : Ce qui s'affiche
    mainPanel(
      h3("Analyse du segment"),
      # On affiche dynamiquement le nombre de clients trouvés
      h4(textOutput("nb_clients"), style = "color: #2c3e50; font-weight: bold;"),
      br(),
      plotOutput("plot_taux")
    )
  )
)

# 3. Le Serveur : La logique de filtrage en coulisse
server <- function(input, output) {
  
  # Création d'une base de données "réactive" qui s'adapte aux filtres
  data_filtree <- reactive({
    df <- bank_data2
    
    # On applique les filtres numériques
    df <- subset(df, age <= input$age_max)
    df <- subset(df, balance >= input$balance_min)
    
    # On applique les filtres textuels (uniquement si l'utilisateur n'a pas laissé "Tous")
    if (input$job_input != "Tous") {
      df <- subset(df, job == input$job_input)
    }
    if (input$housing_input != "Tous") {
      df <- subset(df, housing == input$housing_input)
    }
    
    return(df)
  })
  
  # Affichage du texte : Nombre de clients
  output$nb_clients <- renderText({
    paste("Nombre de clients dans ce segment :", nrow(data_filtree()))
  })
  
  # Génération du graphique
  output$plot_taux <- renderPlot({
    df_plot <- data_filtree()
    
    # Sécurité anti-bug : si les filtres sont trop stricts et qu'il n'y a plus personne
    if(nrow(df_plot) == 0) {
      return(ggplot() + 
               annotate("text", x = 1, y = 1, label = "Aucun client ne correspond à ces critères.", size = 6, color = "red") + 
               theme_void())
    }
    
    # Le graphique si on a des données
    ggplot(df_plot, aes(x = y, fill = y)) +
      geom_bar() +
      scale_fill_manual(values = c("no" = "lightcoral", "yes" = "palegreen3")) +
      theme_minimal() +
      labs(title = "Taux de souscription pour ce segment",
           x = "Le client a-t-il souscrit ? (y)",
           y = "Nombre de clients") +
      theme(plot.title = element_text(size = 16, face = "bold"))
  })
}

# 4. Lancement de l'application
shinyApp(ui = ui, server = server)
