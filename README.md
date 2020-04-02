# covid19_icu

This app implements a model for utilization of ICU beds, floor beds, and patient deaths due to COVID-19.  The model was developed by the COVID-19 Statistics,
Policy modeling and Epidemiology Collective (C-SPEC).  The app was written by [Soheil Eshghi](http://www.soheileshghi.com/), [Margret Erlendsdottir](https://medicine.yale.edu/profile/margret_erlendsdottir/), [Maile Thayer Phillips](https://medicine.yale.edu/profile/maile_phillips/), [Suzan Iloglu](https://medicine.yale.edu/profile/suzan_iloglu/), [Christian Testa](ctesta.com)  and [Forrest W. Crawford](http://www.crawfordlab.io) using the [R](http://www.r-project.org) [shiny](http://shiny.rstudio.com/) framework. 

We are especially grateful to [Gregg Gonsalves](https://medicine.yale.edu/profile/gregg_gonsalves/), [David Paltiel](https://medicine.yale.edu/profile/david_paltiel/),
Hanna Ehrlich, Raphael Sherak,  Melanie Chitwood, Thomas Thornhill, [Nicole Swartwood](https://prevention-policy-modeling-lab.sph.harvard.edu/nicole-swartwood/), 
and Stephanie Perniciaro for advice and comments.  


To cite this web app: 

> Soheil Eshghi, Margret Erlendsdottir, Maile Thayer Phillips, Suzan Iloglu, Forrest W. Crawford, and the COVID-19 Statistics,
Policy modeling and Epidemiology Collective (C-SPEC). "COVID-19 Hospital Capacity Model" 2020. 


To install this package, you will need devtools
```
library(devtools) # or install.packages("devtools")

# use devtools::load_all() for development or devtools::install() to be able to library(covid19icu)
devtools::install()
library(covid19icu)

# after installing and loading covid19icu, run the app with 
runApp()
```

The live version of the app is hosted here: https://forrestcrawford.shinyapps.io/covid19_icu/

The app is provided under the MIT license.


