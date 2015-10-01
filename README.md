# application-sample

## To run locally

#### In R, install the shiny and devtools packages 
```
install.packages(c("shiny","devtools"))
```

### Now install the shared library from github
```
devtools::install_github("keboola/shiny-lib")
```

#### Ok, switch to the directory where the cloned repo is
```
setwd("/location/of/application-sample")
```

#### Load the shiny library into your session, and run the app
```
library(shiny)
runApp()
```

#### Your default browser will load a tab with a random port that shows the application.
For example: http://127.0.0.1:3057/

#### Append bucket and token parameters
http://127.0.0.1:3057/?bucket=in.c-main&token=[my-kbc-token]

#### You should be good to go.
