##R --vanilla
## load libraries
require(gdata)
require(scrapeR)
library(rjson)

##some useful functions:

## l(ength) u(ni)q(ue): how many unique values in a vector of names?
luq <- function(x) {
  if (any(is.na(x))) 
    x <- x[!is.na(x)]
  length(unique(x))
}

##Log in to EOL, generate key, and set value accordingly 
key <- "?"

## set accordingly
mi.dir <- "~/EOL/gitrepos/PoW"

## most recent version of the checklist of butterfly species names 
load(sprintf("%s/Rdata/PreliminaryChecklist.rda",mi.dir))
## 16,381 butterfly species (March 2013)
length(spps)

## list of known butterfly hostplants, according to Ferrer-Paris et al 2013
load(sprintf("%s/Rdata/KnownHostplants.rda",mi.dir))
## 6844 hostplant species reported 
luq(hst$host.val)

## set working directory accordingly
setwd("~/EOL/tmp")

## we use following directories:
## `EOL` for the taxon name search
## `rslt` for the search of data objects associated with a name
## `dO` for the data objects

## first we compare our list of names with the files already downloaded
lst <- file.info(dir("rslt"))
listos <- unique(sub("rslt_([A-Za-z]+_[a-z]+)_[0-9]+.json","\\1",rownames(lst)))
buscar <- spps[!spps %in% gsub("_"," ",listos)]

## this is the list of names to download
length(buscar)

for (i in buscar) {
  ## we use wget to call the EOL API and download the results in json format
  ## first name search
  if (!file.exists(sprintf("EOL/EOL_%s.json",sub(" ","_",i)))) {
    system(sprintf("wget 'http://www.eol.org/api/search/%s.json?key=%s' --output-document=EOL/EOL_%s.json",gsub(" ","+",i),key,gsub(" ","_",i)))## EOL API in json format
  }
  datos <- try(fromJSON(file=sprintf("EOL/EOL_%s.json",gsub(" ","_",i))))
  if (all(class(datos)!="try-error")) {
    n <- datos$totalResults
    n <- length(datos$results)
    cat(sprintf("Buscamos %s, %s resultados obtenidos\n", i,n))
    if (n>0){ 
      for (k in 1:n) {
        cat(sprintf("Especie %s, datos cargados, id=%s\n", datos$results[[k]]$title,  datos$results[[k]]$id))
        bsq <- sprintf("rslt/rslt_%s_%s.json",gsub(" ","_",i), datos$results[[k]]$id)
        if (!file.exists(bsq))
          ## now the summary of data objects for each name
          system(sprintf("wget 'http://www.eol.org/api/pages/%s.json?common_names=0&details=0&images=0&subjects=all&text=75&key=%s' --output-document=rslt/rslt_%s_%s.json",datos$results[[k]]$id, key, gsub(" ","_",i), datos$results[[k]]$id) )
        rslt <- try(fromJSON(file=bsq))
        if (any(class(rslt)=="try-error")) {
          if (file.info(paste("rslt/",bsq,sep=""))$size==0) {
            system(sprintf("rm rslt/%s",bsq))
          } else {
            system(sprintf("mv rslt/%s error/",bsq))
          }
        } else {
          if (any(names(rslt[[1]])=="error")) {
            cat(sprintf("Archivo %s tiene un error\n",arch))
          } else {
            n <- length(rslt$dataObjects)
            if (n>0) {
              for (j in 1:n) {
                dd <- rslt$dataObjects[[j]]$identifier
                bsq2 <- sprintf("dO/object_%s.json",dd)
                if (!file.exists(bsq2)) {
                  ## now the content of the data objects
                  system(sprintf("wget 'http://eol.org/api/data_objects/1.0/%s.json?key=%s' --output-document=%s",dd,key,bsq2) )         
                }
              }
            }
          }
        }
      }
    }
  }
}

## after all these steps, we should have three directories with the files that we need
lst <- file.info(paste("rslt",dir("rslt"),sep="/"))
listos <- unique(sub("rslt/rslt_([A-Za-z]+_[a-z]+)_[0-9]+.json","\\1",rownames(lst)))
buscar <- spps[!spps %in% gsub("_"," ",listos)]

## this should be zero:
length(buscar)

## now we make a summary of all the data in the files downloaded

EOL.nmbrs <- EOL.objs <- data.frame()
for (bsq in rownames(lst)) {
  rslt <- try(fromJSON(file=bsq))
  if (any(class(rslt)=="try-error")) {
    if (file.info(paste("rslt/",bsq,sep=""))$size==0) {
      system(sprintf("rm rslt/%s",bsq))
    } else {
      system(sprintf("mv rslt/%s error/",bsq))
    }
  } else {
    if (any(names(rslt[[1]])=="error")) {
      cat(sprintf("Archivo %s tiene un error\n",bsq))
    } else {
      nmbr <- rslt$scientificName
      id <- rslt$identifier
      ntaxa <- length(rslt$taxonConcepts)
      n <- length(rslt$dataObjects)
      rS <- rslt$richness_score
      ## data frame with summary for each species
      EOL.nmbrs <- rbind(EOL.nmbrs,data.frame(nmbr,id,ntaxa,bsq,n,rS))
      if (n>0) {
        for (j in 1:n) {
          dd <- rslt$dataObjects[[j]]
          dR <- dd$dataRating
          vS <- dd$vettedStatus
          oid <- dd$identifier
          vid <- dd$dataObjectVersionID
          
          bsq2 <- sprintf("dO/object_%s.json",oid)
          
          if (!exists(bsq2)) {
            ## data frame with summary for each data object
            EOL.objs <- rbind(EOL.objs,
                              data.frame(nmbr,id,j,dR,vS,oid,vid,
                                         ttl="error",lan=NA))
          } else {
            oo <- try(fromJSON(file=bsq2))
            ttl <- oo$dataObject[[1]]$title
            if (is.null(ttl))
              ttl <- NA
            lan <- oo$dataObject[[1]]$language
            if (is.null(lan))
              lan <- NA
            EOL.objs <- rbind(EOL.objs,
                              data.frame(nmbr,id,j,dR,vS,oid,vid,ttl,lan))
            rm(oo)
          }
        }
      }
    }
  }
}


aas <- unique(hst$host.val)
aas <- aas[!is.na(aas)]
EOLhst <- data.frame()

for (aa in aas) {
  cat(sprintf("Plant species %s: ",aa))
  ## we use `grep` to detect which data object contain the name of each hostplant species
  oids <- sub("dO/object_([a-z0-9]+).json","\\1",system(sprintf("grep -l '%s' dO/*",aa),intern=T))
  if (length(oids)>0) {            
    EOLhst <- rbind(EOLhst,data.frame(hostplant=aa,objectID=oids))
  }
  cat(sprintf("%s registros, van %s \n ",length(oids),nrow(EOLhst)))
}

EOLhst$butterfly <- EOL.objs$val[match(EOLhst$objectID,EOL.objs$oid)]


save(file=sprintf("%s/Rdata/SummaryEOLdata.rda",mi.dir),EOL.objs,EOL.nmbrs, EOLhst)
