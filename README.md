# hypergeomagic shiny app --work in progress

magic the gathering specific hypergeometric calculator

this is a small personal project to learn shiny apps (see https://www.shinyapps.io/)
the project is open-ended, the complete list of features still to be decided 

basic functionality so far: compute the probability of hitting at least X cards of a certain kind, given a deck of N cards containing K successes, and 7 to 17 draws (i.e., from turn 1 to turn 10, disregarding mulligans and card draw/scry effects)

how to run: 

- install package "shiny" in R
```r
install.packages("shiny")
```

- set working directory to folder containing the app

- run the command:
```r
runApp("hypergeomagic")
```

to do:

- add descriptive text, maybe some visualization
- improve overall look of the app
- ...
