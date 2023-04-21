#Meli mex best


rm( list= ls() )

##funcion para traer MLX



library(rvest)
library(dplyr)
library(tidyverse)
library(readxl)


extraermlx <- function(link){
  ##mlx de 10
  mlx<- str_extract(link,"ML\\w-\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d" )
  mlx
  for(i in 1:length(mlx) )
  {
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d" )
    }
  }
  
  
  ##mlx de 9
  for(i in 1:length(mlx) )
  {
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w\\d\\d\\d\\d\\d\\d\\d\\d\\d" )
    }
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w-\\d\\d\\d\\d\\d\\d\\d\\d\\d" )
    }
  }
  
  
  ##mlx de 8
  for(i in 1:length(mlx) )
  {
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w\\d\\d\\d\\d\\d\\d\\d\\d" )
    }
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w-\\d\\d\\d\\d\\d\\d\\d\\d" )
    }
  }
  
  #mlx de 7
  for(i in 1:length(mlx) )
  {
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w\\d\\d\\d\\d\\d\\d\\d" )
    }
    if(is.na(mlx[i])){
      mlx[i]<- str_extract(link[i],"ML\\w-\\d\\d\\d\\d\\d\\d\\d" )
    }
  }
  
  
  mlx <- gsub('-','',mlx)
  mlx
}

setwd("C:/Users/ameso/OneDrive/Escritorio/Scraping/csvs/propios meli")
inputs <- read_excel("productos meli.xlsx")
#ajusto fecha
inputs$hasta_fecha <- as.Date(inputs$hasta_fecha)


#filtro la fecha

inputs=inputs[inputs$hasta_fecha>= Sys.Date(),]
inputs=inputs[,-2]

##se separa mlx de links

inputsmlx = inputs[!is.na(inputs$idpubli),]

crealinkmlx = function(mlx){
  x=substr(mlx, 3, 3)
  
  if(x == "A" || x == "a")
  {linkprod= paste0("https://www.mercadolibre.com.ar/p/",mlx)}
  else if (x == "C" || x == "c")
  {linkprod= paste0("https://www.mercadolibre.cl/p/",mlx)}
  else if (x == "M" || x == "m")
  {linkprod= paste0("https://www.mercadolibre.com.mx/p/",mlx)}
  else if (x == "O" || x == "o")
  {linkprod= paste0("https://www.mercadolibre.com.co/p/",mlx)}
  else if (x == "U" || x == "u")
  {linkprod= paste0("https://www.mercadolibre.com.uy/p/",mlx)}
  else if (x == "B" || x == "b")
  {linkprod= paste0("https://www.mercadolivre.com.br/p/",mlx)}
  else
  {linkprod= NA}
  return(linkprod)
}



#link="https://articulo.mercadolibre.com.ar/MLA-764828367-juego-bano-completo-griferias-sanitarios-wengue-acceso-envio-_JM"

#link=data3[1143,2]
obtenerdata = function(link){
  pagina_prod = read_html(link)
  titulo <- pagina_prod %>% html_nodes(".ui-pdp-title") %>% html_text()
  
  vendidos <- pagina_prod %>% html_nodes(".ui-pdp-subtitle") %>% html_text()
  vendidos=substr(vendidos, 11, 300)
  vendidos=as.numeric(sub(" vendidos","",vendidos))
  if(is_empty(vendidos)){
    vendidos = NA
  }
  
  stock<- pagina_prod %>% html_nodes(".ui-pdp-buybox__quantity__available") %>% html_text()
  stock=substr(stock, 2, 300)
  stock=sub(",","",stock)
  stock=as.numeric(sub(" disponibles)","",stock))
  if(is_empty(stock)){
    stock = NA
  }
  
  reviews = pagina_prod %>% html_nodes(".ui-pdp-review__amount") %>% html_text()
  reviews=sub(" opiniones","",reviews)
  reviews=sub(",","",reviews)
  reviews=as.numeric(reviews)
  if(is_empty(reviews)){
    reviews = NA
  }

  
  
  precio<- pagina_prod %>% html_nodes(".ui-pdp-price__second-line .andes-money-amount__fraction") %>% html_text()
  precio <- gsub('[,]','',precio) 
  precio <- gsub('[.]','',precio) 
  precio = as.numeric(precio)
  if(is_empty(precio)){
    precio = NA
  }
  
  preciolista<- pagina_prod %>% html_nodes(".andes-money-amount--previous .andes-money-amount__fraction") %>% html_text()
  preciolista <- gsub('[,]','',preciolista)
  preciolista <- gsub('[.]','',preciolista)
  preciolista = as.numeric(preciolista)
  if(is_empty(preciolista)){
    preciolista = NA
  }

  vendedor = pagina_prod %>% html_nodes(".ui-pdp-seller__header__title") %>% html_text()

  if(is_empty(vendedor)){
    vendedor = NA
  }
  
  data=rbind(vendidos,stock,reviews,precio,preciolista,titulo, vendedor)
  data=data[,1]
  return(data)
}

data=sapply(inputsmlx$idpubli, obtenerdata, USE.NAMES = FALSE)
#puede funcionar usar un mutate y meter la funcion ahi
#Bweight <- mutate(Bweight, m_wtgain_imp = ifelse(is.na(m_wtgain), imputado_g , m_wtgain))
data2=t(data)
data2=cbind(inputsmlx,data2)

data2$MLX = extraermlx(data2$idpubli)
data2 <- mutate(data2, Fecha = Sys.Date())


write.table(data2, file= "tabla_prod_meli.csv", sep = ";",  append= TRUE, row.names = FALSE, col.names = FALSE)
