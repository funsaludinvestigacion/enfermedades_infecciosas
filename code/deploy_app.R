# ---------------------------------------------------------
# Using shinyapp.io
#install.packages("rsconnect", repos = "https://cran.rstudio.com")
library(rsconnect)

# Save secret token for connecting to R shiny account
shinyio_token <- Sys.getenv("shinyio_token")
shinyio_secret <- Sys.getenv("shinyio_secret")

# Connect the r script below to an R shiny account
rsconnect::setAccountInfo(name='funsaludinvestigacion', token=shinyio_token, secret= shinyio_secret)



rsconnect::setAccountInfo(name='funsaludinvestigacion',
                          token='4C87ED61532F4E5CE51893065807839B',
                          secret='UFdUc7Q3Bu3HPLsJMg//GdE2lT35bDTz5dOYP7TK')
# Deploy app
rsconnect::deployApp('code/', forceUpdate = TRUE, launch.browser = FALSE)

# Restart app (fixes timeout error)
rsconnect::restartApp('code')

# ----------------------------------------------------------
# Using Github (extremely slow)

#library(shinylive)
#library(httpuv)

# Deploy app
# rsconnect::deployApp('code/app.R')
# shinylive::export(appdir = "code/", destdir = "docs/")

# Run app from server
# httpuv::runStaticServer("docs/app/", port=8008)

# If do this option, need to go to github repository --> settings --> pages (left side of page) --> check deployment out of the main branch and docs/ folder. app.R must be in "code/" to work.