
# Friss dashboard header
RdynamicsHeader <- function(){
  tags$head(
  # tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
  # tags$img(src="friss_small1.svg" , id = "FrissLogo"),
  # tags$img(src="friss_subtext.svg", id = "FrissLogoText"),
  # singleton(includeScript("www/d3.js")),
  # singleton(includeScript("www/underscore.js")),
  # singleton(includeScript("www/jquery-ui.js")),
  singleton(includeCSS("./www/app.css"))
)}
