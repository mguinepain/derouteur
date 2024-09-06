# Effacement de l'espace de travail
rm(list = ls()) 
gc(full = T)

# Initialisation du moteur aléatoire
# → Si pour une raison ou une autre l'espace de travail a été sauvegardé ou est
# restauré, on veut malgré tout de nouveaux résultats !
set.seed(NULL)

# pour run le script
# source("derouteur_v3.R", print.eval=T)

# Vérification du paquet crayon à faire avant d'envoyer de la couleur !

if (! "crayon" %in% installed.packages())
{
  cat("Paquet crayon manquant\n")
  cat("Installation directe possible depuis le dépôt R.\n")
  if (readline(prompt = "Continuer ? (o / n) ") %in% c("o", "O", "y", "Y"))
  {
    install.packages("crayon")
  } else {
    stop("Programme arrêté : dépendances manquantes.")
  }
}

# On peut continuer

cat(crayon::bgBlack(crayon::green("\nProgramme Dérouteur")))
cat(crayon::silver("\nv3.1 - août 2024 - maxime guinepain\n"))

# Chargement des dépendances et vérification/installation préalable ====
# (pour rendre le programme user friendly même pour les newbies de R)

if (! "tidyverse" %in% installed.packages())
{
  cat("Paquet", crayon::cyan("tidyverse"), "manquant\n")
  cat("Sur Linux, installer le paquet avec le gestionnaire de paquets est préférable.\n")
  cat("Sur Windows, installation directe possible avec connexion internet.")
  if (readline(prompt = "Continuer ? (o / n) ") %in% c("o", "O", "y", "Y"))
  {
    install.packages("tidyverse")
  } else {
    stop("Programme arrêté : dépendances manquantes.")
  }
}

if (! "sf" %in% installed.packages())
{
  cat("Paquet", crayon::cyan("sf"), "manquant\n")
  cat("Sur Linux, installer le paquet avec le gestionnaire de paquets est préférable.\n")
  cat("Sur Windows, installation directe possible avec connexion internet.")
  if (readline(prompt = "Continuer ? (o / n) ") %in% c("o", "O", "y", "Y"))
  {
    install.packages("sf")
  } else {
    stop("Programme arrêté : dépendances manquantes.")
  }
}

if (! "osrm" %in% installed.packages())
{
  cat("Paquet", crayon::cyan("osrm"), "manquant\n")
  cat("Installation directe possible depuis le dépôt R.\n")
  if (readline(prompt = "Continuer ? (o / n) ") %in% c("o", "O", "y", "Y"))
  {
    install.packages("osrm")
  } else {
    stop("Programme arrêté : dépendances manquantes.")
  }
}

options(tidyverse.quiet = TRUE)
library(tidyverse)
library(sf)
library(osrm)


# Contrôle réinitialisation ====

if (!dir.exists("Sources")) { dir.create("Sources") }
if (!dir.exists("Routes")) { dir.create("Routes") }

if (file.exists("bano.rds") | file.exists("pool.rds")) {
  cat("Des données préchargées sont disponibles.\nTapez \"reset\" pour les supprimer ou n'importe quelle touche pour les utiliser.\n")
  cat("Cette opération est nécessaire si les bases ont été mises à jour.\n")
  if (readline("Garder les données ? (reset/*) ") == "reset")
  {
    if (file.exists("bano.rds")) { file.remove("bano.rds") }
    if (file.exists("pool.rds")) { file.remove("pool.rds") }
  }
}

# Fonctions générales ====

listeAdresses = function(points_bano)
{
  return(paste0("\n", points_bano$ad_num, " ", points_bano$ad_voie, ", ", points_bano$ad_com))
}

# Chargement base adresses ====

if (file.exists("bano.rds")) { load("bano.rds") } else
{
  cat("Chargement de la base d'adresses")
  
  listefichiers = list.files(path = "Sources")
  listefichiers = listefichiers[grep("bano-", listefichiers)]
  
  if(length(listefichiers) == 0) { stop("Veuillez placer au moins un fichier d'adresses BANO dans le dossier Sources") }
  
  cat(" →", length(listefichiers), "sources touvées")
  
  bano = tibble(code = character(),
                ad_num = character(),
                ad_voie = character(),
                ad_cp = character(),
                ad_com = character(),
                source = factor(),
                lat = double(),
                long = double())
  
  for (fichier in listefichiers)
  {
    nv = read_delim(paste0("Sources/", fichier),
                    col_names = c("code", "ad_num", "ad_voie", "ad_cp",
                                  "ad_com", "source", "lat", "long"),
                    show_col_types = F
    )
    bano = rbind(bano, nv)
  }
  
  # On vire les numéros d'arrondissement pour simplifier la saisie
  bano = mutate(bano, ad_com = case_when(substr(ad_com, 1, 6) == "Paris " ~ "Paris",
                                         T ~ ad_com))
  
  bano = st_as_sf(bano, coords=c("long", "lat"), crs=4326)
  bano = st_transform(bano, crs = 2154)
  
  save(bano, file = "bano.rds")
}

# Chargement échantillon de points ====

if (file.exists("pool.rds")) { load("pool.rds") } else
{
  listefichiers = list.dirs(path = "Sources")
  listefichiers = listefichiers[listefichiers != "Sources"]
  
  pool = NULL
  
  for (dirpath in listefichiers)
  {
    cat("\nChargement du graphe routier dans", dirpath)
    nv = read_sf(paste0(dirpath, "/gis_osm_roads_free_1.shp"))
    
    nv = filter(nv, fclass %in% c("cycleway", "busway", "living_street", "pedestrian",
                                  "primary", "residential", "secondary", "service",
                                  "tertiary", "track", "unclassified"))
    
    cat("\nPréparation des zones de circulation restreinte")
    
    ptr = read_sf(paste0(dirpath, "/gis_osm_landuse_a_free_1.shp"))
    ptr = filter(ptr, fclass %in% c("cemetery", "military", "quarry"))
    ptr = st_union(ptr)
    ptr = st_simplify(ptr, dTolerance = 50)
    
    cat("\nExclusion des zones de circulation restreinte")
    
    nv = st_difference(nv, ptr)
    remove(ptr)
    
    if (is.null(pool))
    {
      pool = nv
    } else {
      pool = rbind(pool, nv)
    }
  }
  remove(nv)
  
  if (is.null(pool)) { stop("Veuillez télécharger un graphe routier depuis OSM et le placer dans le répertoire Sources") }
  
  cat("\nExtraction d'un échantillon de points à partir du graphe")
  
  pool = st_transform(pool, crs=2154)
  pool = filter(pool, st_geometry_type(geometry) == "LINESTRING")
  pool = st_line_sample(pool, density = .01, type = "random") %>% st_cast("POINT")
  
  cat("\nÉchantillon de", nrow(pool), "points prêt\n")
  
  save(pool, file="pool.rds")
}

# Paramètres ====

compteurAppels = 0

if (file.exists("duree.rds")) { load("duree.rds")} else { duree = 60 }
if (file.exists("tolerance.rds")) { load("tolerance.rds")} else { tolerance = 4 ; duree = duree - tolerance/2 }
if (file.exists("mode.rds")) { load("mode.rds") } else { modeT = "vélo" }

cat("\nParamètres actuels : durée de", duree + tolerance/2, "min, mode =", modeT)
if (readline("Garder ces paramètres ? (o/n) ") %in% c("n", "N"))
{
  duree = readline("Durée de l'itinéraire en minutes ? ") %>% as.double()
  tolerance = 4
  duree = duree - (tolerance/2)
  save(duree, file="duree.rds")
  save(tolerance, file="tolerance.rds")
  
  modeT = readline("Mode de transport ? [vélo/v, marche/m] ")
  if (!modeT %in% c("vélo", "marche", "v", "m")) { stop("Mode non reconnu.") }
  if (modeT == "v") { modeT = "vélo" }
  if (modeT == "m") { modeT = "marche" }
  save(modeT, file = "mode.rds")
}

# Paramétrage mode ====

if(modeT == "vélo")
{
  routeMode = "bike"
  facteurVitesse = 12.5
}
if(modeT == "marche")
{
  routeMode = "foot"
  facteurVitesse = 3.5
}

# Paramètre de vitesse à ajuster...
rayonEvitement= facteurVitesse / 12 * 1000

# Sélection des adresses ====

reset=T
if (file.exists("points.rds")) { 
  load("points.rds")
  cat("\nPoints déjà paramétrés détectés.")
  cat(listeAdresses(pointsInit))
  reset = F }
if (!reset) { reset = readline("Garder les points de départ sauvegardés ? ") %in% c("N", "n")}

if (reset)
{
  cat("Saisie de l'itinéraire initial :\n",
      "- adresse au format", crayon::green("numéro, rue, commune"), "avec les virgules ;\n",
      "- commande", crayon::cyan("annuler"), "pour supprimer la dernière adresse ;\n",
      "- commande", crayon::cyan("retour"), "pour répéter la première adresse ;\n",
      "- commande", crayon::cyan("stop"), "pour terminer la saisie.\n")
  
  pointsInit = bano[0,]
  quitter = F
  while(quitter == F)
  {
    adresse = readline("Entrez une adresse ou une commande : ")
    if (!adresse %in% c("stop", "annuler", "retour")) {
      if(length(strsplit(adresse, ",")[[1]]) < 3)
      {
        cat(crayon::yellow("Syntaxe invalide. Merci d'entrer Numéro, Rue, Commune.\n"))
      } else {
        # La syntaxe est transcrite pour trouver une correspondance exacte dans la BANO
        adresseNum = strsplit(adresse, ",")[[1]][1] %>% toupper()
        adresseVoie = strsplit(adresse, ",")[[1]][2] %>% toupper()
        adresseVille = strsplit(adresse, ",")[[1]][3] %>% toupper()
        if (substr(adresseVoie,1,1) == " ") { adresseVoie = substr(adresseVoie, 2, nchar(adresseVoie)) }
        if (substr(adresseVille,1,1) == " ") { adresseVille = substr(adresseVille, 2, nchar(adresseVille)) }
        
        # L'adresse est comparée en majuscules, donc la casse n'a pas d'importance
        pt = bano[toupper(bano$ad_num) == adresseNum & toupper(bano$ad_voie) == adresseVoie & toupper(bano$ad_com) == adresseVille,]
        
        if (nrow(pt) == 1) { pointsInit = rbind(pointsInit, pt) ; cat(crayon::green("Adresse acquise !\n")) }
        else {
          cat(crayon::yellow("Adresse introuvable.\n"))
        }
      }
    } else {
      if (adresse == "stop")
      {
        quitter = T
        cat(nrow(pointsInit), "adresses entrées !")
        cat(listeAdresses(pointsInit))
        save(pointsInit, file = "points.rds")
      }
      if (adresse == "annuler")
      {
        if(nrow(pointsInit>0)) {
          pointsInit = pointsInit[c(1:nrow(pointsInit)-1),]
          cat("Il reste", nrow(pointsInit), "point(s).\n")
        } else {
          cat(crayon::yellow("La liste des points est déjà vide.\n"))
        }
      }
      if (adresse == "retour")
      {
        if (nrow(pointsInit)>0){
          pointsInit = rbind(pointsInit, pointsInit[1,])
          cat(crayon::green("Premier point répété.\n"))
        }
      }
    }
  }
}

if (nrow(pointsInit) < 2) { stop("Pas assez d'adresses pour calculer un itinéraire !")}

# Itinéraire de départ ====

routeInit = osrmRoute(loc = pointsInit, osrm.profile = routeMode)
compteurAppels = compteurAppels+1
routeInit = st_transform(routeInit, crs=2154)

if (routeInit$duration > duree + tolerance) {
  duree = routeInit$duration * 1.1
  
  cat(crayon::yellow("L'objectif fourni est insuffisant.\n"))
  cat("Objectif augmenté à", round(duree), "minutes.\n")
}

# Le "facteur de vitesse" sert à faire les conversions entre la distance euclidienne
# et la distance réelle que doit parcourir l'utilisateur. Il s'agit donc d'une
# mesure très variable, qui dépend pour beaucoup du réseau dans le voisinnage.
# Par ex, si le maillage est très fin, la distance réelle sera proche de la distance
# euclidienne, mais s'il est très relâché, l'écart peut être important.
# Pour cette raison, on essaye de moduler le facteur à l'aide de l'estimation
# d'itinéraire dans un premier temps car cela permet de calculer le coefficient
# en "conditions réelles" dans la région. Si ce n'est pas possible, on donne
# un facteur d'autant plus grand que la boucle ne va l'être, car d'expérience
# c'est ce qui donne le meilleur résultat.
# Un facteur de vitesse légèrement trop élevé rend la complétion de l'itinéraire
# trop "facile", ce qui fait calculer trop peu d'étapes intermédiaires et produit
# des itinéraires peu intéressants. Un facteur de vitesse trop irréaliste (trop
# bas ou trop élevé) empêche le calcul.
# Dans la màj de juillet 2024, on commence par calculer un facteur de vitesse bas
# puis on augmente jusqu'à ce que ça fonctionne si cela s'avère trop bas.
# Ainsi, on crée des itinéraires avec un plus grand nb d'étapes intermédiaires.
# Conçu pour le vélo uniquement...
# facteurVitesse = facteurVitesse + .3 * routeInit$distance
if (routeInit$distance == 0) { facteurVitesse = facteurVitesse + ((duree/60)*facteurVitesse*.1)  }

cat("Facteur de vitesse de", facteurVitesse)

# Cette fonction a été désactivée. Elle cause trop de soucis. En revanche je laisse
# l'augmentation automatique pour éviter de tomber sur des problèmes insolubles.
# if (routeInit$duration / (duree + tolerance) < 1.25)
# {
#   facteurVitesse = 1.1 * facteurVitesse
#   cat("\nBudget temps restreint, facteur vitesse augmenté à", facteurVitesse, "\n")
# }

isochrones = st_buffer(pointsInit, dist = (((duree/60) * facteurVitesse/2)*1000))
isochrone <- reduce(isochrones$geometry, st_intersection) %>% st_sfc() %>% st_as_sf(crs= 2154)
plot(isochrones$geometry, border = "gray", main = "Champ des possibles")
plot(isochrone, border = "black", lty=2, add=T)
plot(pointsInit$geometry, col="blue", add=T)
plot(routeInit$geometry, add=T)

# Augmentation de l'itinéraire ====

cat("\nLancement du calcul")
satisfait = F

while(!satisfait)
{
  routeActuelle = routeInit
  points = pointsInit
  dureeTraj = routeActuelle$duration
  
  nbEchecs = 0
  
  while(dureeTraj < duree)
  {
    # Calculons la route en ligne droite
    nSeg = nrow(points) - 1
    segments = lapply(X=1:nSeg, FUN = function (x) {
      paire = st_union(points[x,], points[x + 1,])
      seg = st_cast(paire, "LINESTRING")
      return (seg$geometry)
    })
    segments = list_c(segments)
    
    tSegments = tibble(n = 1:length(segments),
                       longueur = as.numeric(st_length(segments)),
                       geometry = segments)
    tSegments = st_as_sf(tSegments, crs = 2154)
    tSegments = mutate(tSegments, poids = ifelse(longueur < 100, 0, longueur))
    
    # Temps total estimé
    # temps = dureeTraj
    temps = ((sum(tSegments$longueur) / 1000) / facteurVitesse) * 60
    
    # Si on ne part pas d'un seul point (boucle)
    if (sum(tSegments$poids) > 0) {
      # Sélectionnons aléatoirement un segment, pondéré par leur longueur
      segment = slice_sample(tSegments, n = 1, weight_by = poids)
    } else {
      # Le premier segment (nul) sera le bon
      segment = tSegments[1,]
    }
    
    intervalleTan = 1.4 # aucune idée de comment il faut régler ça...
    
    # Paramètres ajustés pour une distribution semicirculaire de Wigner
    # Doit approximer la longueur de la tangente disponible où placer le point...
    # Il faudrait bcp plus de maths et de calculs pour que ce soit équivalent au
    # niveau des superficies potentielles mais bref
    tangente = ifelse(segment$longueur>0, rbeta(n = 1, shape1 = 3/2, shape2 = 3/2), .5)
    
    # Quel est notre budget temps ?
    # temps = temps - osrmRoute(loc = st_cast(segment, "POINT"), osrm.profile = routeMode)$duration
    # compteurAppels = compteurAppels + 1
    temps = temps - (((segment$longueur / 1000) / facteurVitesse) * 60)
    budget = duree - temps + tolerance
    
    # Le budget est réparti selon le point du segment (plus ou moins proche du départ)
    budgetA = budget * tangente
    budgetB = budget * (1-tangente)
    
    # # Calculons à présent dans quelle zone rajouter le point
    # Add (avril 24) : on s'arrange pour viser les 50-90% les plus éloignés
    # pour faciliter la génération de l'itinéraire (amplitude)
    distA = ((budgetA/60) * facteurVitesse)*1000
    distB = ((budgetB/60) * facteurVitesse)*1000

    isoA = st_difference(st_buffer(points[segment$n,], dist = .8 * distA),
                         st_buffer(points[segment$n,], dist = .5 * distA))
    isoB = st_difference(st_buffer(points[segment$n+1,], dist = .8 * distB),
                         st_buffer(points[segment$n+1,], dist = .5 * distB))
    
    iso = st_intersection(isoA, isoB)
  
    # On voudrait éviter les retours en arrière
    if(sum(tSegments$poids) > 0)
    {
      rayon = ((budget*1.5/60) * facteurVitesse)*1000
      limites180 = st_buffer(segment, dist=rayon, endCapStyle="FLAT") %>% st_union()
      iso = st_intersection(iso, limites180)
    }
    
    # # Retirons toutes les zones déjà trop proches d'un point existant
    isoRoute = st_buffer(routeActuelle, dist=rayonEvitement)
    iso = st_difference(iso, isoRoute)
    
    plot(isochrone, border="red", main = paste0(round(routeActuelle$duration, 0), " min. / ",
                                                round(routeActuelle$distance, 2), " km"))
    plot(routeActuelle$geometry, add=T)
    plot(points$geometry, col="blue", add=T)
    plot(iso$geometry, border="grey", lty=2, add=T)
    
    # N'a de sens que s'il y a déjà deux points distincts
    if (length(unique(points$geometry)) > 1)
    {
      pointsIti = unique(st_cast(routeActuelle$geometry, "POINT"))
      
      # Calcul sur le segment
      segmentReel = osrmRoute(loc = st_cast(segment$geometry, "POINT"), osrm.profile = routeMode) %>% st_transform(crs=2154)
      compteurAppels = compteurAppels+1
      
      pointsSegment = unique(st_cast(segmentReel$geometry, "POINT"))
      
      pointsIti_table = tibble(geometry = pointsIti, i = c(1 : length(pointsIti))) |>
        st_as_sf() |>
        st_set_crs(2154) |>
        rename(point = geometry)
      
      # identifions quels numéros sont dans le segment
      pointsSegment_table = tibble(geometry = pointsSegment) |>
        st_as_sf() |>
        st_set_crs(2154)
      pointsSegment_table = st_join(pointsSegment_table, pointsIti_table, left = T)
      
      # Calculons les voronois des points pour ne pas installer le nouveau point
      # dans le voisinage d'un segment déjà existant, au risque de créer des nœuds
      voronois = st_voronoi(do.call(c, unique(st_cast(routeActuelle$geometry, "POINT"))),
                            envelope = st_as_sfc(isochrone)) |>
        st_collection_extract() |>
        st_set_crs(st_crs(2154))
      voros = tibble(geometry = voronois) %>%
        st_as_sf() |>
        st_join(pointsIti_table) # pour récupérer les données des points
      
      # gardons les voros correspondants
      voros = filter(voros, i %in% pointsSegment_table$i)
      voro = summarise(voros)
      plot(voro, border = "pink", add=T)
      
      # On coupe l'iso pour ne garder que cette zone
      iso = st_intersection(iso, voro)
      remove(voronois, voros, pointsSegment, pointsIti, pointsIti_table, pointsSegment_table)
    }
    
    if (nrow(iso) > 0)
    {
      # Voyons quels points sont disponibles dans cet espace
      poolDispo = st_intersection(pool, iso)
      
      nvPoints = points
      
      if (length(poolDispo) > 0)
      {
        # Ajoutons un point aléatoire
        nouvPt = poolDispo[sample(c(1:length(poolDispo)), size = 1)]
        nouvPt = tibble(geometry = nouvPt) %>% st_as_sf(crs=2154)
        plot(nouvPt$geometry, col="darkgreen", add=T)
        
        # Données d'adresse
        nouvPtAdr = bano[st_nearest_feature(nouvPt, bano),]
        nouvPt = cbind(st_drop_geometry(nouvPtAdr), nouvPt)
        
        nvPoints = add_row(nvPoints, nouvPt, .before = segment$n+1)
        
        # Prenons en compte les nouveaux points
        points = nvPoints
        
        # Calculons l'itinéraire avec OSRM
        routeActuelle = osrmRoute(loc = points, osrm.profile = routeMode) %>% st_transform(crs=2154)
        compteurAppels = compteurAppels+1
        
        # Retirons les "pics"
        testPics = F
        while(testPics) {
          # Si un point est identique à un point situé deux rangs plus loin c'est qu'il y a un "pic"
          # Et on veut éviter cela
          routeActuelle_pts = st_cast(routeActuelle$geometry, "POINT")
          verifPics = tibble(i = c(1:length(routeActuelle_pts)),
                             point_i = routeActuelle_pts)
          
          # test 1 : vérifier s'il y a un point identique 2 rangs plus loin
          # test 2 : vérifier si le précédent et le suivant sont identiques
          
          ptsDecales = verifPics$point_i
          ptsDecales = ptsDecales[2:length(ptsDecales)]
          ptsDecales[length(ptsDecales)+1] = st_point(c(0,0))
          verifPics$point_iMoins = ptsDecales
          
          ptsDecales = verifPics$point_i
          ptsDecales[2:(length(ptsDecales)+1)] = ptsDecales
          ptsDecales[1] = st_point(c(0,0))
          verifPics$point_iPlus = ptsDecales[1:(length(ptsDecales)-1)]
          
          ptsDecales = verifPics$point_i
          ptsDecales[3:(length(ptsDecales)+2)] = ptsDecales
          ptsDecales[1] = st_point(c(0,0))
          ptsDecales[2] = st_point(c(0,0))
          verifPics$point_i2 = ptsDecales[1:(length(ptsDecales)-2)]
          
          verifPics$check = (verifPics$point_i == verifPics$point_i2) | (verifPics$point_iMoins == verifPics$point_iPlus)
          
          # Si des pics sont détectés
          if(any(verifPics$check))
          {
            # On les enlève du trajet
            verifPics = filter(verifPics, !check)
            
            verifPics = select(verifPics, -point_iPlus, -point_iMoins, -point_i2, -check)
            
            # Problème, il faut sûrement répercuter sur les points à la base du trajet
            # On va donc modifier les points initiaux pour qu'ils soient remplacés par l'un
            # des nouveaux points les plus proches
            
            voronois = st_voronoi(do.call(c, unique(verifPics$point_i))) |>
              st_collection_extract() |>
              st_set_crs(st_crs(2154))
            voros = tibble(geometry = voronois) %>%
              st_as_sf() |>
              st_join(st_as_sf(verifPics))
            
            points = st_join(points, voros)
            
            points = st_drop_geometry(points) |>
              left_join(verifPics, by = "i") |>
              rename(geometry = point_i) |>
              st_as_sf() |>
              select(-i)
            
            routeActuelle = osrmRoute(loc = points, osrm.profile = routeMode) %>% st_transform(crs=2154)
            compteurAppels = compteurAppels +1
            
          } else {
            testPics = T
          }
        }
        
        # Appliquons la durée
        dureeTraj = routeActuelle$duration
        
        # Affichons la route
        plot(routeActuelle$geometry, col="green", lty=2, add=T)
      }
    } else {
      nbEchecs = nbEchecs + 1
      
      
      if (nbEchecs > 0 & nbEchecs / 10 == as.integer(nbEchecs/10)) {
        cat("\n", crayon::red(nbEchecs, "échecs"), crayon::yellow("Impossible d'échantillonner les points\n"))
        facteurVitesse = facteurVitesse + .1
        cat("Augmentation du facteur de vitesse à", facteurVitesse, "\n")
      }
      
      if (nbEchecs == 149) { stop ("Trop d'échecs. Erreur fatale" )}
    }
  }
  
  cat("\nListe des adresses composant l'itinéraire proposé :")
  cat(listeAdresses(points))
  
  nouvelleRoute = osrmRoute(loc = points, osrm.profile = routeMode) %>% st_transform(crs=2154)
  plot(isochrone, border="red", main = paste0(round(nouvelleRoute$duration), " mn / ",
                                              round(nouvelleRoute$distance, 2), " km"))
  compteurAppels = compteurAppels + 1
  plot(nouvelleRoute$geometry, add=T)
  plot(points$geometry, col="blue", add=T)
  
  statut = readline(prompt = "Taper \"reset\" pour générer un nouveau tracé, ou autre touche pour terminer ") 
  if (statut == "reset")
  {
    cat("Nouveau calcul\n")
  } else {
    routeActuelle = nouvelleRoute
    satisfait = T
  }
}

cat(paste0(compteurAppels, " appels envoyés à OSRM\n"))

# Pour ne pas avoir de pbs de doublons de noms
if(file.exists(chemin))
{
  i = 2
  while(file.exists(paste0(nomSortie, " (", i, ").kml")))
  {
    i = i + 1
  }
  chemin = paste0(nomSortie, " (", i, ").kml")
}

st_write(routeActuelle, dsn = chemin, driver = "LIBKML", layer = "route")
st_write(pts, dsn = chemin, driver = "LIBKML", layer = "wp")

if (modeT == "vélo")
{
  # Préparation URL Géovélo
  pts = st_transform(points, crs=4326)
  pts$X = st_coordinates(pts$geometry)[,1]
  pts$Y = st_coordinates(pts$geometry)[,2]
  pts$coords = paste0(pts$X, "%2C", pts$Y)
  
  # Si 2 points seulement
  if (nrow(pts) == 2)
  { lienGeovelo = paste0("https://geovelo.app/fr/route/?bike-type=own&e-bike=false&from=",
                         pts[1,]$coords,
                         "&to=",
                         pts[2,]$coords) }
  if (nrow(pts) > 2)
  { lienGeovelo = paste0("https://geovelo.app/fr/route/?bike-type=own&e-bike=false&from=",
                         pts[1,]$coords,
                         "&steps=",
                         paste(pts[c(2:(nrow(pts)-1)),]$coords, collapse = "%3B"),
                         "&to=",
                         pts[nrow(pts),]$coords)
  }
  
  cat(crayon::bold("Lien pour afficher/calculer dans Géovélo : "), crayon::hyperlink(lienGeovelo, lienGeovelo), "\n")
}

nomSortie = readline("Nom à spécifier pour la sortie (sinon nommage par défaut / x pour ne rien sauvegarder) ? ")
if (nomSortie == "")
{
  listeComs = unique(points$ad_com) %>% paste(collapse = ", ")
  nomSortie = paste0("Routes/", Sys.Date(), " - ", listeComs)
}

if (nomSortie != "x")
{
  # Procédure pour 2 calques avec les points de passage
  routeActuelle = st_transform(routeActuelle, crs=4326)$geometry
  chemin = paste0(nomSortie, ".kml")
  
  pts = st_transform(points, crs = 4326)
  pts$i = c(1:nrow(pts))
  pts$Name = paste0(pts$i, ") ", pts$ad_voie, ", ", pts$ad_com)
  pts = select(pts, Name)
  
  cat(crayon::green(" Sortie exportée en KML. "))
}

cat("Au revoir et à bientôt !\n")