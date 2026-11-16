# ---------------------------------------------------------
# Using shinyapp.io
library(rsconnect)

getwd()

list.files()

# Same account/token as before
shinyio_token <- Sys.getenv("shinyio_token")
shinyio_secret <- Sys.getenv("shinyio_secret")

rsconnect::setAccountInfo(name = 'funsaludinvestigacion', token = shinyio_token, secret = shinyio_secret)

rsconnect::deployApp('.', appName = 'archivo_agri_agricasa',
                     forceUpdate = TRUE, 
                     launch.browser = FALSE)

# Restart if needed (fixes timeout error)
rsconnect::restartApp('archivo_agri_agricasa')

