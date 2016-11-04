# Real-time parking spots availabilty

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

This repository contains the source code for an interactive app that displays real-time utilization of free parking spots for 4 public parking lots in city of Wroclaw, Poland.  

## About
The app is built using [R](http://www.r-project.org) and [Shiny](http://shiny.rstudio.com) web framework (with [shinydashboard](https://rstudio.github.io/shinydashboard/) package), utilizing an excellent JavaScript library [Higcharts](http://www.highcharts.com/) and its R wrapper - [highcharter](http://jkunst.com/highcharter/) package.

## Data source
Publicly available data from  [Wroclaw Open Data](http://www.wroclaw.pl/open-data/index.php/zbiory-danych/17-transport/124-zapelnienie-parkingow) repository, updated every 5-10 minutes. 
The data is fetched from a CSV file containing the following information:
- timestamp (*Czas_Rejestracji*)
- number of available parking spots (*Liczba_Wolnych_Miejsc*)
- number of vehicles entering the parking lot (*Liczba_Poj_Wjezdzajacych*)
- number of vehicels leaving the parking lot (*Liczba_Poj_Wyjezdzajacych*)
- name of the parking lot (*Nazwa*)
```R
head(read.csv2("http://www.wroclaw.pl/open-data/opendata/its/parkingi/parkingi.csv"))
         Czas_Rejestracji Liczba_Wolnych_Miejsc Liczba_Poj_Wjezdzajacych Liczba_Poj_Wyjezdzajacych                 Nazwa
1 2016-11-03 00:00:00.020                   279                        1                         0             Nowy Targ
2 2016-11-03 00:00:01.383                   119                        0                         0     ul. sw. Antoniego
3 2016-11-03 00:00:01.943                   791                        0                         0 Parking Hala Stulecia
4 2016-11-03 00:05:00.017                   279                        0                         0             Nowy Targ
5 2016-11-03 00:05:01.190                   119                        0                         0     ul. sw. Antoniego
6 2016-11-03 00:05:01.427                   791                        0                         0 Parking Hala Stulecia
```
## How to run the app
To run the app locally you should install required packages: **shiny**,  **shinydashboard**,  and **highcharter** in R, for example: 
```R
if (!require('shiny')) install.packages("shiny")
```
and use the function `runGithub()` with specified repository name under my username:
```R
shiny::runGitHub("Parking", "holek10")
```
Once the app is loaded the dashboard is presented (*Przeglad*).  
Click *Dane* to see data overview.

## Licensing 
The app is provided for free under GNU General Public License
