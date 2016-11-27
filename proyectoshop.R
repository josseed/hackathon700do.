## creamos el modelo
predice <- function(v1,v2,v3,v4,v5,v6,v7){
  gustos <- read.csv("C:/Users/josseed/Desktop/eventloopR.csv", sep=";")
  modelo <- lm(Tipo ~ Sombrero + Pañuelo + Accesorios + Polera + Sweater + Pantalon + Falda, data = gustos)
  nuevo_gusto <- data.frame(Sombrero = as.numeric(v1),
                            Pañuelo= as.numeric(v2),
                            Accesorios= as.numeric(v3),
                            Polera= as.numeric(v4),
                            Sweater= as.numeric(v5),
                            Pantalon=as.numeric(v6),
                            Falda=as.numeric(v7))
  dato <-predict(modelo, newdata=nuevo_gusto, interval="prediction",level=0.95)
  return(dato[1])
}

## lanzamos la api 
jug() %>%
  cors() %>%
  ## 
  get("/mpg_api/(?<v1>.*)/(?<v2>.*)/(?<v3>.*)/(?<v4>.*)/(?<v5>.*)/(?<v6>.*)/(?<v7>.*)", function(req,res,err){
    #res$set_header("Access-Control-Allow-Methods", 'POST, GET, PUT, OPTIONS, DELETE, PATCH')
    #res$set_header("Access-Control-Allow-Origin", '*')
    #res$set_header("Access-Control-Max-Age", '3600')
    #res$set_header("Access-Control-Allow-Headers", 'Origin, X-Requested-With, Content-Type, Accept')
    
    res$json(predice(req$params$v1,req$params$v2,req$params$v3,req$params$v4,req$params$v5,req$params$v6,req$params$v7))
  }) %>%

  serve_it(verbose=TRUE)
