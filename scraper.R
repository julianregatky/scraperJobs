library(rvest)

html <- read_html("https://www.zonajobs.com.ar/ofertas-de-empleo-argentina.html?recientes=true")
listado <- html_nodes(html,'div.aviso-no-sponsor')
elementos <- html_nodes(listado,'div')
titulos <- html_text(html_nodes(elementos,'h2.titulo-aviso'))
links <- html_attr(html_node(html_nodes(elementos,'div.wrapper'),'a'),'href')
