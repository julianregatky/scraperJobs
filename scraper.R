library(xml2)
library(rvest)
library(dplyr)
library(mongolite)

# Las noticias en la página van a estar ordenadas de más
# reciente a más antigua. El scraper va a ir recorriendo
# hasta que supere el límite de días 'max_dias'.

# Creamos la conexión a la db y collection de mongo
conexionDB <- mongo(collection = "datos", db = "scraperJobs")
# Definimos una función que limpia texto, y la voy a necesitar seguido
limpiar_string <- function(string) { return(gsub("^\\s+|\\s+$","",string)) }
# Definimos la variable 'max_dias' antes descripta
max_dias <- 14

# ~~~~~~~~ Recorrido ~~~~~~~ #
continuar <- TRUE
pagina <- 1
anuncios_procesados <- 0
while(continuar)
{
  html <- read_html(paste0("https://www.zonajobs.com.ar/ofertas-de-empleo-argentina-pagina-",pagina,".html?recientes=true"))
  listado <- html_nodes(html,'div.aviso-no-sponsor')
  elementos <- html_nodes(listado,'div')
  links <- html_attr(html_node(html_nodes(elementos,'div.wrapper'),'a'),'href')
  i <- 1
  while(i <= length(links) & continuar)
  {
    html <- read_html(paste0("https://www.zonajobs.com.ar",links[i]))
    titulo <- limpiar_string(html_text(html_node(html,'h1.aviso_title')))
    empresa <- limpiar_string(html_text(html_node(html,'span#empresa')))
    
    specs <- html_nodes(html,'div.aviso_specs')
    specs_attr <- gsub(":","",limpiar_string(html_text(html_nodes(specs,'div.spec_attr'))))
    specs_def <- limpiar_string(html_text(html_nodes(specs,'div.spec_def')))
    
    datos_specs <- as.data.frame(matrix(specs_def,nrow=1))
    colnames(datos_specs) <- specs_attr
    datos_specs$Publicado <- if_else(as.character(datos_specs$Publicado) == "Hoy",Sys.Date(),
                                    Sys.Date()+1-as.numeric(gsub("[Publicado hace |días]","",datos_specs$Publicado)))
    
    descripcion <- gsub("[\n|\t]","",limpiar_string(html_text(html_node(html,'div.aviso_description'))))
    
    chequeo_si_existe <- conexionDB$find(paste0('{ "titulo" : "',titulo,'", "empresa" : "',empresa,'", "Lugar de Trabajo": "',datos_specs$`Lugar de Trabajo`,'" }'))
    if(nrow(chequeo_si_existe) == 0 & as.numeric(Sys.Date()-datos_specs$Publicado) <= max_dias)
    {
      registro <- cbind(data.frame(sitio_web = "zonajobs",
                                   titulo = titulo,
                                   empresa = empresa,
                                   descripcion = descripcion),
                        datos_specs)
      conexionDB$insert(registro)
    }
    else if(as.numeric(Sys.Date()-datos_specs$Publicado) > max_dias)
    {
      print(ifelse(nrow(chequeo_si_existe)>0,"Se detuvo porque encontró otro registro igual en la base de datos","Se detuvo porque alcanzó 'max_dias'"))
      continuar = FALSE
    }
    i <- i + 1
    anuncios_procesados <- anuncios_procesados + 1
    cat(anuncios_procesados," anuncios procesados en un total de ",pagina," páginas\r")
  }
  pagina <- pagina + 1
}
conexionDB$disconnect()


