🏦 Projet Data : Analyse Exploratoire & Modélisation pour une Campagne de Marketing Bancaire

📋 Contexte du projet
Dans un secteur bancaire très concurrentiel, la mise en place de campagnes marketing efficaces est essentielle. Ce projet s'appuie sur les données de campagnes de marketing direct (appels téléphoniques) d'une institution bancaire portugaise. L'objectif principal est d'utiliser l'exploration de données et les statistiques pour décider quels clients cibler en priorité pour la souscription d'un dépôt à terme (variable y).

🎯 Objectifs
Explorer et préparer les données (traitement des valeurs manquantes et aberrantes).
Extraire des insights stratégiques via des analyses univariées, bivariées et des tests statistiques (Khi-deux, corrélations de Pearson) .
Prédire le comportement client via un modèle de Machine Learning simple et interprétable (K-Nearest Neighbors - KNN).
Fournir des recommandations métier pour maximiser le retour sur investissement (ROI) sous contraintes budgétaires.
Mettre en production locale un tableau de bord interactif d'aide à la décision.

📊 Données
Le jeu de données utilisé (bank-full.csv) provient de l'UCI Machine Learning Repository. Il contient plus de 45 000 instances et regroupe des informations démographiques, financières et l'historique de contact des clients.

🛠️ Méthodologie & Technologies (R)
Ce projet suit la méthodologie CRISP-DM:
Préparation : Nettoyage des données, traitement des "unknown" et suppression des valeurs aberrantes sur les soldes bancaires via la méthode de l'écart interquartile (IQR).
Analyse Statistique : Découverte des segments prometteurs grâce aux visualisations (ggplot2) et validation mathématique (Test du Khi-Deux).
Modélisation (Machine Learning) : Encodage, normalisation et entraînement d'un modèle KNN ($k=5$). 
Utilisation du sous-échantillonnage (undersampling) pour pallier le déséquilibre des classes.
Évaluation : Analyse de la Matrice de Confusion avec un focus sur la métrique de Précision (plutôt que l'Accuracy) pour répondre aux enjeux de coûts marketing de la banque.
Déploiement : Création d'une application R Shiny interactive pour simuler l'impact de différents critères de ciblage.

💡 Résultats & Recommandations Stratégiques
Ciblage optimal : Les données montrent que les étudiants, les retraités, ainsi que les clients sans prêt immobilier en cours présentent les taux de conversion les plus élevés.
Efficacité multipliée par 5 : Le taux de succès naturel d'un appel à l'aveugle est de ~11,7%. 
En utilisant les prédictions du modèle KNN, la Précision monte à plus de 58%, permettant de réduire drastiquement les coûts du centre d'appels tout en maximisant les signatures.

🚀 Comment lancer le projet ?
Cloner ce dépôt.
Ouvrir le script principal sous RStudio.
S'assurer d'avoir installé les librairies suivantes : ggplot2, class, shiny.
Modifier le chemin d'accès au fichier CSV lors de l'importation (read.csv2).
Exécuter le code. 
Le tableau de bord interactif s'ouvrira à la toute fin du script.
