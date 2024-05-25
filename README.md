# derouteur
*A small program written in R to make bike routes more complicated for people with time to waste.*

# How does it work?

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
