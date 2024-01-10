rm(list=ls())
library(jsonlite)
library(httr)

entremunics<-function(coordl) {
  require("jsonlite")
  require("httr")  
  coord<-coordl
  body<-list(locations=coord,
             metrics=list("distance","duration"),
             units="km")
  body <-  toJSON(body, auto_unbox = TRUE)
  url<-'https://api.openrouteservice.org/v2/matrix/driving-car'
  
  r <- POST(url,body = body, 
            add_headers(`Accept` = "application/json, application/geo+json, application/gpx+xml, img/png; charset=utf-8"),
            add_headers(`Authorization`= "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),
            add_headers(`Content-Type`= "application/json; charset=utf-8"))
  
  h<-r$content
  data0<-fromJSON(rawToChar(as.raw(strtoi(h, 16L))))
  data0$restantes<-as.numeric(r$headers$`x-ratelimit-remaining`)
  return(data0)
}


setwd('/Dados')
coords<-read.csv("coordenadas geográficas Municipios Brasil.csv")
coords<-coords[order(coords$codigo_ibge),]

#distance<-diag(nrow(coords)) # matriz do zero
distance<-as.matrix(fst::read_fst("distance_km.fst")) # matriz com dados de onde parou
#durations<-diag(nrow(coords)) 
durations<-as.matrix(fst::read_fst("durations_seconds.fst"))
rownames(distance)<-rownames(durations)<-coords$codigo_ibge
colnames(distance)<-colnames(durations)<-coords$codigo_ibge
i<-seq(1,nrow(coords),by=25)
j<-c(i[-1]-1,nrow(coords))

for (k1 in 48:(length(i)-1)) {
  s1<-c(i[k1]:j[k1])
  for (k2 in k1:(length(i)-1)) {
    s2<-i[k2+1]:j[k2+1]
    coordl<-as.matrix(coords[c(s1,s2),4:3])
    distbetween<-entremunics(coordl)
    distance[c(s1,s2),c(s1,s2)]<-distbetween$distances
    durations[c(s1,s2),c(s1,s2)]<-distbetween$durations
    Sys.sleep(2) 
    if(distbetween$restantes==1){
      break
    }
    # .3 a .6 ou .5 a .8 para 50 ou 40 por minuto varia com a velocidade de internet na hora
  }
  df.distance<-as.data.frame(distance)
  fst::write_fst(df.distance,"distance_km.fst", compress = 0)
  df.durations<-as.data.frame(durations)
  fst::write_fst(df.durations,"durations_seconds.fst", compress = 0)
  if(distbetween$restantes<(length(i)-k1)){
    break
  }
  cat(k1, "-")
}


df.distance<-as.data.frame(distance)
df.distance$cod_munic<-coords$codigo_ibge
fst::write_fst(df.distance,"distance_km.fst", compress = 100)
write.csv(df.distance,"distance_km.csv")

df.durations<-as.data.frame(durations)
df.durations$cod_munic<-coords$codigo_ibge
fst::write_fst(df.durations,"durations_seconds.fst", compress = 100)
write.csv(df.durations,"durations_seconds.csv")

df.durations<-durations/(60*60) # durations for seconds to hours 
df.durations<-round(df.durations,3) 
df.durations<-as.data.frame(df.durations)
df.durations$cod_munic<-coords$codigo_ibge
fst::write_fst(df.durations,"durations_hours.fst", compress = 100)



############################# CORREÇÕES


setwd('/mnt/Disco1/Jupyter/R/myprogs/11 Dados espaciais/Dados')
coords<-read.csv("coordenadas geográficas Municipios Brasil.csv")
coords<-coords[order(coords$codigo_ibge),]

#distance<-diag(nrow(coords))
df.distance<-fst::read_fst("distance_km.fst")
#durations<-diag(nrow(coords))
df.durations<-fst::read_fst("durations_seconds.fst")


df.distance$cod_munic<-NULL ### remover strings
for (i in 1:ncol(df.distance)) {
  df.distance[is.na(df.distance[,i]),i]<-0
}


df.durations$cod_munic<-NULL ### remover strings
for (i in 1:ncol(df.durations)) {
  df.durations[is.na(df.durations[,i]),i]<-0
}


###############vizualizar os que não foram identificados
erros<-coords[(df.durations[,1]==0 & df.durations[,2]==0),]
# or a matriz
#erros<-df.durations[(df.durations[,1]==0 & df.durations[,2]==0),(df.durations[1,]==0 & df.durations[2,]==0)]
