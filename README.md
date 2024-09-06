# dérouteur
*Un petit outil en R pour rallonger des itinéraires à vélo et occuper son temps libre.*

Fonctionne à présent avec les itinéraires à pied.

## Comment ça marche ?

Dérouteur utilise deux sources de données et se lance dans une console, sans interface, parce que pourquoi s'embêter à en faire une ? Il mobilise une base de données des adresses pour sélectionner et ajouter des points de parcours à l'itinéraire initial, et un extrait d'OpenStreetMap pour ajouter des points de parcours aléatoires.

Dérouteur repose également sur une instance d'OSRM en ligne, un calculateur d'itinéraire libre dont le projet est décrit sur cette [page](http://project-osrm.org/). Cela signifie que bien qu'il faille télécharger beaucoup de données pour le faire fonctionner, il lui faut aussi une connexion Internet. J'essayerai sans doute à l'avenir d'inclure à Dérouteur une instance de calcul d'itinéraire locale, mais c'est beaucoup plus compliqué que d'ajouter quelques points de parcours à un itinéraire calculé sur un serveur distant.

Tout d'abord, Dérouteur demande d'indiquer la longueur prévue du parcours et le mode (vélo ou marche). Ensuite, on peut ajouter les points de parcours initiaux. Dérouteur peut complexifier un itinéraire entre deux points (ou plus), mais aussi créer une boucle aléatoire à partir d'un seul point (qui doit être répété deux fois comme point de départ et d'arrivée). Pour entrer les points de parcours, il faut saisir une adresse dans la console, avec une syntaxe spécifique (numéro de rue, rue, commune, les virgules comptent). N'hésitez pas à visiter OpenStreetMap pour vérifier les adresses ; si elles y figurent, ellesd evraient être reconnues. La casse ne compte pas, mais les accents oui, et bien sûr les prénoms quand ils sont spécifiés sur OpenStreetMap (rue Jean Jaurès, pas rue Jaurès). Ensuite, Dérouteur propose un itinéraire de la longueur voulue.

Pour créer un itinéraire aléatoire, Dérouteur ajoute des points de parcours aléatoires à ceux spécifiés en entrée. En sortie, un fichier `.kml` calculé avec l'itinéraire d'OSRM peut être écrit dans le dossier "Routes". Dérouteur affiche également la liste des adresses approximatives des points de parcours ajoutés, et propose de calculer un itinéraire avec les points de parcours aléatoires dans Géovélo.

**NB :** les itinéraires proposés à l'aide d'OSRM ou de tout autre service dépendent de la qualité des données d'OpenStreetMap et du calculateur d'itinéraire. Ils sont en général satisfaisants, mais peuvent s'avérer dangereux, en particulier sur des portions aléatoires, même si Dérouteur ne suggèrera pas d'emprunter des autoroutes ou des pistes d'aéroport. Avant de partir, vérifiez soigneusement la qualité du trajet.

## Comment le faire marcher ?

Dérouteur est actuellement un simple script destiné à être interprété par le programme R, qui doit être installé sur votre ordinateur. Il nécessite aussi un certain nombre de dépendances, que Dérouteur peut installer pour vous s'il détecte qu'ils sont manquants. Si vous utilisez R sous Linux, préférez installer ces dépendances vous mêmes (à l'aide de votre gestionnaire de paquets) car les plus grosses (`tidyverse` et `sf`) sont mises à jour fréquemment et mettent beaucoup de temps à être compilées, il vaut donc mieux qu'elles soient installées comme des paquets systèmes.

Pour télécharger et installer R, rendez-vous sur : [https://www.r-project.org/](https://www.r-project.org/)

Une fois R installé sur l'ordinateur, il faut préparer les données nécessaires au script (les adresses et le graphe routier).

### 1) Préparer les données du script

Dérouteur nécessite deux sources de données :

* un extrait de la Base Nationale des adresses ordinaires (BANO). Téléchargez les fichiers `.csv` correspondant aux départements à visiter sur [ce lien](https://bano.openstreetmap.fr/data/). Ces adresses ne sont disponibles qu'en France. Notez que Dérouteur pré-chargera systématiquement toutes les adresses téléchargées lors de la préparation des données, donc il vaut mieux ne prendre que ce dont on a besoin. Copiez les fichiers `.csv` dans le répertoire "Sources" qu'il faudra créer dans le dossier où vous placerez le script de "Dérouteur".
* une image régionale d'OpenStreetMap pour préparer le graphe routier. Téléchargez les données correspondant à la région sur [geofabrik.de](https://download.geofabrik.de/) puis décompressez chaque téléchargement dans un sous-dossier séparé à l'intérieur du répertoire "Sources". Par exemple, pour l'Ile-de-France, téléchargez le fichier `.zip` et décompressez-le dans un dossier "Ile-de-France" à l'intérieur du répertoire "Source". Le nom du sous-dossier n'a aucune importance. Dérouteur ira directement chercher les fichiers dont il a besoin à l'intérieur.

Au premier lancement de Dérouteur, le script préparera sa base de données et les convertira en deux fichiers nommés `pool.rds` (contenant un ensemble de points servant à créer les itinéraires aléatoires) et `bano.rds` (contenant les adresses à utiliser pour créer les points de passage). Il n'est pas nécessaire de relancer la préparation des données à chaque fois, mais cela l'est pour mettre à jour les données ou lors d'une grosse mise à jour de Dérouteur.

### 2) Lancer le script

Lancer le script est plus facile ! Il suffit de lancer R (taper "R" dans une console, ou lancer R dans la liste des programmes). Précisez ensuite à R quel est son répertoire de travail au moyen de la commande `setwd()` dans laquelle il faut indiquer où se trouve le script (et, partant, les répertoires contenant les données). Voici le genre de chemin d'accès que vous devriez utiliser sous Linux pour un dossier nommé "Derouteur" dans votre espace utilisateur :

```
setwd("/home/username/derouteur")
```

Ou sous Windows :

```
setwd("C:/Users/Username/Derouteur")
```

Ensuite, lancez le script en utilisant la commande `source` :

```
source("derouteur_v3.R")
```

Il ne devrait pas être nécessaire de taper quoi que ce soit d'autre en langage R, donc il n'est pas nécessaire de le maîtriser pour utiliser Dérouteur.

## Retours

Si vous utilisez Dérouteur, n'hésitez pas à me contacter pour me faire part de vos expériences ! Vous pouvez bien sûr le faire si vous rencontrez des erreurs ou des comportements inattendus.

Profitez bien et bonne découverte de votre région ainsi que de l'univers fascinant des probabilités,

Maxime.

# derouteur
*A small program written in R to make bike routes more complicated for people with spare time.*

Also works to take a walk now.

## How does it work?

Derouteur uses two sources to be able to be ran directly from a console, without any interface, because I'm to lazy to create one. So it uses an addresses database to select and add waypoints to an initial route and OpenStreetMap to know where else you could go on randomly selected points it will add to the route.

Derouteur also relies on an online instance of OSRM (which is a route calculation feature offered by the [project](http://project-osrm.org/)), so even if you download a lot of data to make it work, you'll also need an Internet connection. Perhaps sometime in the future I'll be working on a local instance to perform this calculation, but that's actually a LOT more complicated than just playing with waypoints.

First, you'll set a few parameters to indicate the ideal duration of your trip. Then, you provide your initial waypoints. You can use a route between two or more points as a start or compute a completely random round route by adding twice the same as your departure and arrival. Waypoints are provided by typing the addresses in the console, with a specific syntax that sould be enough for the script to recognize them. If it fails, you can visit OpenStreetMap to look at surrounding addresses numbers; if they exist on the map, they should be recognised by the script, because the BANO database is extracted from OpenStreetMap. Then, you'll let Derouteur work.

Derouteur uses your initial points and recursively adds new, random ones until the route duration matches your wish. That is, on a linear trip between two points, it will add a few steps that will make the route more complicated. With the same point as departure and arrival, it will compute a round random route.

At the end of the process, you can visualise the result on a plot window. A `.kml` file will also be exported to the "Routes" directory.

**Please note** that computed routes are only as good as OSRM calculations and OSM data and can sometimes be dangerous, especialy because of the randomness of some waypoints, even if Derouteur will not send you to to motorways or airport tracks. Please carefuly check all computed routes before using them to go on a trip.

# How to make it work?

To make Derouteur work, you need to install R first on your computer. You'll also need a few dependencies. My script can install them for you if it detects they are not satisfied. Please not that, if you're running R on Linux, you should rather install them yourself by using packages provided by your distribution. Main depenencies are `tidyverse` and `sf` (they are very common but can take a while to install). I also use `osrm` to compute bike routes.

Please visit the official project webpage to install R: [https://www.r-project.org/](https://www.r-project.org/)

Once R is installed on your computer, you must first prepare all source data (i.e. points and roads) and then to run the script.

## 1) Preparing data for the script

Derouteur needs two main sources:

* an extract from the BANO database (National Addresses Database). Select all the `.csv` files related to the departments you live in or wish to visit on [this webpage](https://bano.openstreetmap.fr/data/). I'm sorry, this will only work in mainland France :'( Please note that Derouteur takes all of the departments you have downloaded into account when preparing the base, so if you download a lot of them, it can take time to compute. Copy all `.csv` files directly in the "Sources" directory that you might need to create alongside the main "Derouteur" script.
* an extract from OpenStreetMap to import the road graph. Download OSM Data from [geofabrik.de](https://download.geofabrik.de/) and uncompress each extract in a separate directory inside Routes. For example, if you want to use the programme in Ile-de-France, download the corresponding `.zip` file and uncompress it in an "Ile-de-France" directory inside Sources. The name of the directory does not matter. Derouteur will recognise needed files inside.

When first launching Derouteur, the script will prepare these sources and convert them to two temporary files named `pool.rds` (containing future waypoints to randomly select) and `bano.rds` (containing addresses). You won't have to regenerate them on each launch, but you will have to repeat this if you download updated versions of both the BANO and/or the OSM extract.

## 2) Running the script

Once this is done, running the script is way easier. Just launch R (by typing "R" on any console, on any type of operating system) and use the `source` command with the path leading to where you downloaded the script. For example, this will execute the script if it is left in your home directory on Linux:

```
source("/home/username/derouteur_v3.R")
```

Here is a path you could expect on Windows:

```
source("C:/Users/Username/derouteur_v3.R")
```

To make the instruction shorter, you can launch the console from the directory where the file resides. It should be possible with a right-mouse-button click. Just type the name of the script then:

```
source("derouteur_v3.R")
```

You should not have anything else to type in R, so you can use this even if you do not know this language.

# Feedback

If you're using Derouteur and having fun with it, feel free to tell it to me! And, of course, please contact me if you encounter errors or odd behaviours.

I might add some features later. Some things are quite easy to tweak. For example, with a few adjustments, Derouteur can be used to create walking or running tours. I'm also thinking about a way of extracting addresses from OSM easily to use them instead of my very French database. And maybe one day I'll be working on an interface, but hey, aren't these console programmes quite charming?

Have very nice trips exploring both your region and the fascinating universe of probabilities,

Maxime.
