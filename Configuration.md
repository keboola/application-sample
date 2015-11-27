# Deploying Applications with KBC

## Creating an Application in KBC

In the Applications section of KBC, click New Application in the top right corner and select Shiny Application.

You'll be asked to provide a name and a description.  Once you've done that you'll need to configure it.

## Component Configuration

Component configuration for now is a simple JSON block that accepts the following parameters:

#### Required parameters:

* repository: The url of the repository of your app. For example, this repository: "https://github.com/keboola/application-sample"
* bucket: The id of the bucket to examine.  Ex: "in.c-main".  Note only Redshift buckets are supported at this time.

#### Optional parameters:

* version: If you want to use a certain branch or tagged version of your repo, you can specify that here.
* cranPackages: Comma separated names of packages to be installed from CRAN.  Ex: "\"ggplot2, reshape2\""
* githubPackages: Comma separated names of packages to be installed from github.  Ex: "\"hadley/dplyr\""

Note: The packages lists right now need those escaped quotes as wrappers in order to work properly.  This will be fixed at some point most likely.

Note2: Private repository support is coming soon.

That's it.  When you hit the run button, 2 jobs will be created.  When they're finished you will have an active link on the right hand side to your newly deployed application.

Happy exploring!