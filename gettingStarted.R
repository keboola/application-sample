#' this script will get you going
#' 
#' 

installedPackages <- rownames(installed.packages())

cranPackages <- c("devtools", "shiny", "DT")

new.packages <- cranPackages[!(cranPackages %in% installedPackages)]
if(length(new.packages)) install.packages(new.packages)

library(devtools)

if (("aws.signature" %in% installedPackages) == FALSE) {
    devtools::install_github("cloudyr/aws.signature")    
}
if (("keboola.sapi.r.client" %in% installedPackages) == FALSE) {
    devtools::install_github("keboola/sapi-r-client")    
}
if (("keboola.shiny.lib" %in% installedPackages) == FALSE) {
    devtools::install_github("keboola/shiny-lib")    
}

library(keboola.sapi.r.client)
library(shiny)
token <- readline(prompt="Please enter your KBC token:")

client <- SapiClient$new(token)

print(unlist(lapply(client$listBuckets(),function(x){
    x$id
})))

bucket <- readline(prompt="Please enter the bucket to explore:")

launchKeboolaApp <- function(appUrl) {
    browseURL(paste0(appUrl,"?bucket=",bucket,"&token=",token))
}

runKeboolaApp <- function(){
    runApp(launch.browser=launchKeboolaApp)
}



