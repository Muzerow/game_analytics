# game_analytics
Data processing codes for game analytics developed for MyTracker raw data.

The main focus is on cohort analysis with the ability of estimation the mean event values for the first n days of gameplay. The important note here is that these two groups must be differentiated:

* **Cohort groups** are users that installed the game in particular time period (for instance, 18.01.2021-28.01.2021)
* **Cohorts** are users within cohort groups who installed the game in particular day (19.01.2021)

So, the average values for cohorts are computed and then the average of these averages to result in means for cohort groups.

## cohort_ads_finished

The code helps to evaluate the average number of ads watched both for all users in cohorts and users in cohorts who watched at least one ad in the first n days of gameplay.

For reproducibility it uses **optparse** package and includes six required parameters:

* **-i** Input file is the raw dataset with the game custom events
* **-o** Output directory is the folder to put resulting file in
* **-s**
* **-e**
* **-d**
* **-r**
