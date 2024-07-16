# Vigilancia de Enfermedades Infecciosas en Guatemala

Instrucciones y descripción en español (English can be found below): Este repositorio hace un seguimiento de la incidencia de enfermedades infectuosas en Trifinio, Guatemala según investigacciones por la Universidad de Colorado y el equipo de FunSalud.

El código en este repositorio público crea una aplicación de R Shiny de datos en Redcap. La aplicación está ubicada aquí.

Instructions and description in English: This repository tracks the tncidence of infectious disease in Trifinio, Guatemala as recorded by researchers in studies led by the University of Colorado and FunSalud. The code in this repository connects Redcap to an R Shiny app, which can be found here.

Parts of this workflow:

1. The first step was to create repository secrets holding a key for a Redcap API: On github, go to settings --> secrets --> actions --> new repository secret --> enter redcap API key
2. Create a repository secret to hold your token and secret stored on shiny.io (this is the server where the app is hosted).
3. Ensure the repository secret names you enter match the key names used in main.yml
4. Main.yml will run every Monday morning, or every time new information is pushed to the github repository.
5. Redcap_data_processing.R reads the Redcap database using the secret tokens and makes publicly available excel files for use by R Shiny. Recap_data_processing is written to ensure that no personal protected data is uploaded to this github repository.
6. app.R creates the interactive Shiny app interface.
7. deploy_app.R will restart the Shiny app when run. The app is re-deployed every monday morning, according to the main.yml document.
