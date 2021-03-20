# game_analytics
Data processing codes for game analytics developed for MyTracker raw data.

The main focus is on cohort analysis with the ability to estimate the mean event values for the first n days of gameplay. The important note here is that these two groups must be differentiated:

* **Cohort groups** are users that installed the game in a particular time period (for instance, 18.01.2021-28.01.2021)
* **Cohorts** are users within cohort groups who installed the game on a particular day (19.01.2021)

So, the average values for cohorts are computed, and then the average of these averages results in means for cohort groups.

For reproducibility all codes use **optparse** package including different parameters.

**Note:** if the calculations should be done without filtering countries, specify "All" in country argument.

## cohort_ads_finished

The code helps to evaluate the average number of ads watched both for all users in cohorts and users in cohorts who watched at least one ad in the first n days of gameplay.

Optparse parameters:

* **-i** Input file is the raw dataset with the game custom events
* **-o** Output directory is the folder to put the resulting file in
* **-s** Date from which the first cohort starts
* **-e** Date in which the first cohort ends
* **-c** Country of the first cohort
* **-d** Date from which the second cohort starts
* **-r** Date in which the second cohort ends
* **-b** Country of the second cohort
* **-v** The number of first days in the game to calculate statistics on (eg, if 1 specified, then the statistics is calculated as the mean values for the first day in the game)
* **-q** Chosen event for calculation

## cohort_ads_funnel

The code calculates the differences between the number of actions made as a parts of watching ads funnel between two cohorts. Namely, there are three actions involved - number of window opens, clicks on the ad and finishes of the ad. The mean value of these actions on the first n days of gameplay are compared between specified cohorts.

## cohort_game_progress

The code calculates mean game progress for two specified cohorts in first n days of gameplay after the installation (note that in the last update the date difference is calculated with taking into account precise timestamp, but not just a date) and compares them.

* **-i** Input file is the raw dataset with the game custom events
* **-o** Output directory is the folder to put the resulting file in
* **-s** Date from which the first cohort starts
* **-e** Date in which the first cohort ends
* **-c** Country of the first cohort
* **-d** Date from which the second cohort starts
* **-r** Date in which the second cohort ends
* **-b** Country of the second cohort
* **-v** The number of first days in the game to calculate statistics on (eg, if 1 specified, then the statistics is calculated as the mean values for the first day in the game)
