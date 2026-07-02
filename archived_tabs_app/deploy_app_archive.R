# ---------------------------------------------------------
# Using shinyapp.io
library(rsconnect)

getwd()

list.files()

# Same account/token as before
shinyio_token <- "4C87ED61532F4E5CE51893065807839B"
shinyio_secret <- "UFdUc7Q3Bu3HPLsJMg//GdE2lT35bDTz5dOYP7TK"
rsconnect::setAccountInfo(name = 'funsaludinvestigacion', token = shinyio_token, secret = shinyio_secret)

rsconnect::deployApp('.', appName = 'archivo_agri_agricasa',
                     forceUpdate = TRUE, 
                     launch.browser = FALSE)

# Restart if needed (fixes timeout error)
rsconnect::restartApp('archivo_agri_agricasa')

