# NYC-Taxi-Data-Project

TITLE :: Visualizing the green taxi data for April over the years (2014-2016)

-----------------------------------
Data required for running Dashboard
-----------------------------------

1. The data required for running RShiny dashboard is kept in UBox and can be downloaded from the below link:

    https://buffalo.box.com/s/e3aof22qthkc9bgl6oatt7lf4pngboqu

2. Download the entire folder named "CSE587-Term Project-Data" and keep it in the same directory "TermProject" since it has all the other relevant R code. 

3. The folder "CSE587-Term Project-Data" contains the following 12 files:

* green_tripdata_2014-04.csv
* green_tripdata_2015-04.csv
* green_tripdata_2016-04.csv
* new_green_tripledata_2014-04.csv
* new_green_tripledata_2015-04.csv
* new_green_tripledata_2016-04.csv
* exp_green_tripledata_2014-04.csv
* exp_green_tripledata_2015-04.csv
* exp_green_tripledata_2016-04.csv
* lm_green_tripledata_2014-04.csv
* lm_green_tripledata_2015-04.csv
* lm_green_tripledata_2016-04.csv

----------------------------------------
Dashboard Development and Implementation
----------------------------------------

The building of dashboard proceeded with the below stages:

------------
Getting data
------------

1. The raw data is acquired from "Unified New York City Taxi and Uber data" which can be found in https://github.com/toddwschneider/nyc-taxi-data.
2. Here, we are merely concerned with Green Taxi data for the month of April over the three years from 2014 to 2016.
3. For the purpose of this project, the following files contains the raw data:

* green_tripdata_2014-04.csv
* green_tripdata_2015-04.csv
* green_tripdata_2016-04.csv

-------------
Cleaning data
-------------
1. For the purpose of cleaning data, a script "cleaning_script.R" is being developed in R which cleans the data and prepares the various data files to serve our purpose for this term project. 

2. The output of this script will result in the creation of the following files:

* new_green_tripledata_2014-04.csv
* new_green_tripledata_2015-04.csv
* new_green_tripledata_2016-04.csv
* exp_green_tripledata_2014-04.csv
* exp_green_tripledata_2015-04.csv
* exp_green_tripledata_2016-04.csv
* lm_green_tripledata_2014-04.csv
* lm_green_tripledata_2015-04.csv
* lm_green_tripledata_2016-04.csv


-------------------------------------
Visualization Dashboard / Exploration
-------------------------------------

1. For the purpose of this project, we are building the Dashboard in Rshiny using the R code residing in "visual_greenTaxi.R" -> This is the script that will launch the dashboard.

2. The first tab panel consists of the comparison of number of pickups for the green nyc taxi in different locations of New York City for the month April over the years (2014-2016). We can choose to change the interval of number of pickups and simultaneoulsy we get different visualization results.

3. The second tab panel consists of exploring of Trip Distance Vs Time of a day for the period of first seven days of April over the three years. 

4. The screenshots for the dashboard in kept in "Screenshots_Dashboard" folder.

-------------
Linear models 
-------------

1. The models have been used to predict the amount of tip given by passenger based on trip distance made by the passenger. 

2. The third tab panel consists of showing the linear models for the data of first day of april over the past three years (2014-2016).

3. The linear line in blue shows the initial fitting while the line in red displays a better model for the data.

---------
Storyline
---------

1. The purpose of this project is to analyze the pattern of usage of green taxi over the month of april across each year over the period from 2014 to 2016. 

2. The other purpose is to explore the tipping habits of passengers covering a trip for certain distance. This behavior is shown by linear models and can be visualized in this dashboard.

---------------
Technology used
---------------
RShiny[2], RShiny in RStudio[3]


----------
References
----------
[1] Tableau Tutorial. https://www.tableau.com/learn/training, last viewed 2017.

[2] RShiny Tutorial. https://shiny.rstudio.com/tutorial/, last viewed 2017.

[3] RShiny in RStudio. https://shiny.rstudio.com/, last viewed 2017.

