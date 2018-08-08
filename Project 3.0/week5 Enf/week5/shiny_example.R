library(rsconnect)
library(shiny)

options(shiny.usecairo = FALSE)
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux') {
  dir.create(font_home())
  file.copy('wqy-zenhei.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

runApp()

#deployApp()

