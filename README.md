# hypergeomagic shiny app --work in progress

magic the gathering specific hypergeometric calculator

this is a small personal project to learn shiny apps (see https://www.shinyapps.io/)

the project is open-ended, the complete list of features still to be decided 

functionality so far: compute and display the probability of hitting at least X, or exactly X, cards of a certain kind, given a deck of N cards containing K successes, and a variable number of draws; on the play/on the draw is also implemented

how to run: 
- download the app
- install package "shiny" in R
```r
install.packages("shiny")
```

- set working directory to folder containing the app

- run the app:
```r
library(shiny)
runApp("hypergeomagic")
```

alternatively, download and run in one command:
```r
library(shiny)
runGitHub("hypergeomagic", "mic-he")
```
to do:

- add descriptive text
- improve overall look of the app
- ...
- deploy?
