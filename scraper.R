library(xml2)
library(rvest)
library(dplyr)

limpiar_string <- function(string) { return(gsub("^\\s+|\\s+$","",string)) }

continuar <- TRUE
pagina <- 1
while(continuar)
{
  html <- read_html(paste0("https://www.zonajobs.com.ar/ofertas-de-empleo-argentina-pagina-",pagina,".html?recientes=true"))
  listado <- html_nodes(html,'div.aviso-no-sponsor')
  elementos <- html_nodes(listado,'div')
  #titulos <- html_text(html_nodes(elementos,'h2.titulo-aviso'))
  links <- html_attr(html_node(html_nodes(elementos,'div.wrapper'),'a'),'href')
  for(i in 1:length(links))
  {
    html <- read_html(paste0("https://www.zonajobs.com.ar",links[i]))
    titulo <- limpiar_string(html_text(html_node(html,'h1.aviso_title')))
    empresa <- limpiar_string(html_text(html_node(html,'span#empresa')))
    
    specs <- html_nodes(html_nodes(html,'div.aviso_specs'),'div.spec_def')
    
  }
}
