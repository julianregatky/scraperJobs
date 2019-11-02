library(xml2)
library(rvest)
library(dplyr)
library(mongolite)

# Las noticias en la página van a estar ordenadas de más
# reciente a más antigua. El scraper va a ir recorriendo
# hasta que supere el límite de días 'max_dias'.

sitios_a_analizar <- data.frame(url1 = c("https://www.bumeran.com.ar",
                                        "https://www.laborum.cl",
                                        "https://www.bumeran.com.mx",
                                        "https://www.bumeran.com.pe",
                                        "https://www.multitrabajos.com",
                                        "https://www.konzerta.com"),
                                url2 = c("/empleos-argentina-pagina-",
                                         "/empleos-chile-pagina-",
                                         "/empleos-mexico-pagina-",
                                         "/empleos-peru-pagina-",
                                         "/empleos-ecuador-pagina-",
                                         "/empleos-panama-pagina-"),
                                nombre = c("Bumeran",
                                            "Laborum",
                                            "Bumeran",
                                            "Bumeran",
                                            "Multitrabajos",
                                            "Konzerta"),
                                pais = c("Argentina",
                                         "Chile",
                                         "México",
                                         "Perú",
                                         "Ecuador",
                                         "Panamá"))

# Creamos la conexión a la db y collection de mongo
conexionDB <- mongo(collection = "datos", db = "scraperJobs")
# Definimos una función que limpia texto, y la voy a necesitar seguido
limpiar_string <- function(string) { return(gsub("^\\s+|\\s+$|\"|\'","",string)) }
# Definimos la variable 'max_dias' antes descripta
max_dias <- 1

# ~~~~~~~~ Recorrido ~~~~~~~ #
for(j in 1:nrow(sitios_a_analizar))
{
  continuar <- TRUE
  pagina <- 1
  anuncios_procesados <- 0
  while(continuar)
  {
    html <- read_html(paste0(sitios_a_analizar$url1[j],sitios_a_analizar$url2[j],pagina,".html?recientes=true"))
    listado <- html_nodes(html,'div.aviso-no-sponsor')
    elementos <- html_nodes(listado,'div')
    links <- html_attr(html_node(html_nodes(elementos,'div.wrapper'),'a'),'href')
    i <- 1
    while(i <= length(links) & continuar)
    {
      html <- read_html(paste0(sitios_a_analizar$url1[1],links[i]))
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
      
      #chequeo_si_existe <- conexionDB$find(paste0('{ "titulo" : "',titulo,'", "empresa" : "',empresa,'", "Lugar de Trabajo": "',datos_specs$`Lugar de Trabajo`,'" }'))
      if(as.numeric(Sys.Date()-datos_specs$Publicado) < max_dias)
      {
        registro <- cbind(data.frame(sitio_web = as.character(sitios_a_analizar$nombre[j]),
                                     pais = as.character(sitios_a_analizar$pais[j]),
                                     titulo = titulo,
                                     empresa = empresa,
                                     descripcion = descripcion),
                          datos_specs)
        conexionDB$insert(registro)
      }
      else
      {
        continuar = FALSE
      }
      i <- i + 1
      anuncios_procesados <- anuncios_procesados + 1
      cat(as.character(sitios_a_analizar$nombre[j]),as.character(sitios_a_analizar$pais[j]),":",anuncios_procesados,"anuncios procesados en un total de",pagina,"páginas\r")
    }
    pagina <- pagina + 1
  } 
}
conexionDB$disconnect()


