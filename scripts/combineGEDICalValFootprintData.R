#!/usr/bin/env Rscript
#.libPaths("/Jupyter_Notebook/lib/R/library")
require(tidyverse)
require(terra)
require(jsonlite)
#library(data.table)
require(BIOMASS)
#library(optparse)

## GEDI waveform metrics
gedi.var <- c("waveID","trueground","truetop","groundslope","ALScover","gHeight","maxGround","inflGround","signaltop","signalbottom","cover","leadingedgeext","trailingedgeextent","rhGauss0","rhGauss1","rhGauss2","rhGauss3", "rhGauss4","rhGauss5","rhGauss6","rhGauss7","rhGauss8","rhGauss9","rhGauss10","rhGauss11","rhGauss12","rhGauss13","rhGauss14","rhGauss15","rhGauss16","rhGauss17","rhGauss18","rhGauss19","rhGauss20","rhGauss21", "rhGauss22","rhGauss23","rhGauss24","rhGauss25","rhGauss26","rhGauss27","rhGauss28","rhGauss29","rhGauss30","rhGauss31","rhGauss32","rhGauss33","rhGauss34","rhGauss35","rhGauss36","rhGauss37","rhGauss38",            "rhGauss39","rhGauss40","rhGauss41","rhGauss42","rhGauss43","rhGauss44","rhGauss45","rhGauss46","rhGauss47","rhGauss48","rhGauss49","rhGauss50","rhGauss51","rhGauss52","rhGauss53","rhGauss54","rhGauss55",
              "rhGauss56","rhGauss57","rhGauss58","rhGauss59","rhGauss60","rhGauss61","rhGauss62","rhGauss63","rhGauss64","rhGauss65","rhGauss66","rhGauss67","rhGauss68","rhGauss69","rhGauss70","rhGauss71","rhGauss72",            "rhGauss73","rhGauss74","rhGauss75","rhGauss76","rhGauss77","rhGauss78","rhGauss79","rhGauss80","rhGauss81","rhGauss82","rhGauss83","rhGauss84","rhGauss85","rhGauss86","rhGauss87","rhGauss88","rhGauss89",
              "rhGauss90","rhGauss91","rhGauss92","rhGauss93","rhGauss94","rhGauss95","rhGauss96","rhGauss97","rhGauss98","rhGauss99","rhGauss100","rhMax0","rhMax1","rhMax2","rhMax3","rhMax4","rhMax5","rhMax6","rhMax7",
              "rhMax8","rhMax9","rhMax10","rhMax11","rhMax12","rhMax13","rhMax14","rhMax15","rhMax16","rhMax17","rhMax18","rhMax19","rhMax20","rhMax21","rhMax22","rhMax23","rhMax24","rhMax25","rhMax26","rhMax27","rhMax28",
              "rhMax29","rhMax30","rhMax31","rhMax32","rhMax33","rhMax34","rhMax35","rhMax36","rhMax37","rhMax38","rhMax39","rhMax40","rhMax41","rhMax42","rhMax43","rhMax44","rhMax45","rhMax46","rhMax47","rhMax48","rhMax49",
              "rhMax50","rhMax51","rhMax52","rhMax53","rhMax54","rhMax55","rhMax56","rhMax57","rhMax58","rhMax59","rhMax60","rhMax61","rhMax62","rhMax63","rhMax64","rhMax65","rhMax66","rhMax67","rhMax68","rhMax69","rhMax70",
              "rhMax71","rhMax72","rhMax73","rhMax74","rhMax75","rhMax76","rhMax77","rhMax78","rhMax79","rhMax80","rhMax81","rhMax82","rhMax83","rhMax84","rhMax85","rhMax86","rhMax87","rhMax88","rhMax89","rhMax90","rhMax91",
              "rhMax92","rhMax93","rhMax94","rhMax95","rhMax96","rhMax97","rhMax98","rhMax99","rhMax100","rhInfl0","rhInfl1","rhInfl2","rhInfl3","rhInfl4","rhInfl5","rhInfl6","rhInfl7","rhInfl8","rhInfl9","rhInfl10","rhInfl11",
              "rhInfl12","rhInfl13","rhInfl14","rhInfl15","rhInfl16","rhInfl17","rhInfl18","rhInfl19","rhInfl20","rhInfl21","rhInfl22","rhInfl23","rhInfl24","rhInfl25","rhInfl26","rhInfl27","rhInfl28","rhInfl29","rhInfl30",
              "rhInfl31","rhInfl32","rhInfl33","rhInfl34","rhInfl35","rhInfl36","rhInfl37","rhInfl38","rhInfl39","rhInfl40","rhInfl41","rhInfl42","rhInfl43","rhInfl44","rhInfl45","rhInfl46","rhInfl47","rhInfl48","rhInfl49",
              "rhInfl50","rhInfl51","rhInfl52","rhInfl53","rhInfl54","rhInfl55","rhInfl56","rhInfl57","rhInfl58","rhInfl59","rhInfl60","rhInfl61","rhInfl62","rhInfl63","rhInfl64","rhInfl65","rhInfl66","rhInfl67","rhInfl68",
              "rhInfl69","rhInfl70","rhInfl71","rhInfl72","rhInfl73","rhInfl74","rhInfl75","rhInfl76","rhInfl77","rhInfl78","rhInfl79","rhInfl80","rhInfl81","rhInfl82","rhInfl83","rhInfl84","rhInfl85","rhInfl86","rhInfl87",
              "rhInfl88","rhInfl89","rhInfl90","rhInfl91","rhInfl92","rhInfl93","rhInfl94","rhInfl95","rhInfl96","rhInfl97","rhInfl98","rhInfl99","rhInfl100","rhReal0","rhReal1","rhReal2","rhReal3","rhReal4","rhReal5","rhReal6",
              "rhReal7","rhReal8","rhReal9","rhReal10","rhReal11","rhReal12","rhReal13","rhReal14","rhReal15","rhReal16","rhReal17","rhReal18","rhReal19","rhReal20","rhReal21","rhReal22","rhReal23","rhReal24","rhReal25","rhReal26",
              "rhReal27","rhReal28","rhReal29","rhReal30","rhReal31","rhReal32","rhReal33","rhReal34","rhReal35","rhReal36","rhReal37","rhReal38","rhReal39","rhReal40","rhReal41","rhReal42","rhReal43","rhReal44","rhReal45",
              "rhReal46","rhReal47","rhReal48","rhReal49","rhReal50","rhReal51","rhReal52","rhReal53","rhReal54","rhReal55","rhReal56","rhReal57","rhReal58","rhReal59","rhReal60","rhReal61","rhReal62","rhReal63","rhReal64",
              "rhReal65","rhReal66","rhReal67","rhReal68","rhReal69","rhReal70","rhReal71","rhReal72","rhReal73","rhReal74","rhReal75","rhReal76","rhReal77","rhReal78","rhReal79","rhReal80","rhReal81","rhReal82","rhReal83",
              "rhReal84","rhReal85","rhReal86","rhReal87","rhReal88","rhReal89","rhReal90","rhReal91","rhReal92","rhReal93","rhReal94","rhReal95","rhReal96","rhReal97","rhReal98","rhReal99","rhReal100","filename","gaussHalfCov",
              "maxHalfCov","infHalfCov","bayHalfCov","pSigma","fSigma","linkM","linkCov","lon","lat","groundOverlap","groundMin","groundInfl","waveEnergy","blairSense","pointDense","beamDense","zenith","FHD","niM2","niM2.1",
              "meanNoise","noiseStdev","noiseThresh","FHDhist","FHDcan","FHDcanHist","FHDcanGauss","FHDcanGhist","tLAI0t1","tLAI1t2","tLAI2t3","tLAI3t4","tLAI4t5","tLAI5t6","tLAI6t7","tLAI7t8","tLAI8t9","tLAI9t10","tLAI10t11",
              "tLAI11t12","tLAI12t13","tLAI13t14","tLAI14t15","tLAI15t16","tLAI16t17","tLAI17t18","tLAI18t19","tLAI19t20","tLAI20t21","tLAI21t22","tLAI22t23","tLAI23t24","tLAI24t25","tLAI25t26","tLAI26t27","tLAI27t28",
              "tLAI28t29","tLAI29t30","tLAI30t31","tLAI31t32","tLAI32t33","tLAI33t34","tLAI34t35","tLAI35t36","tLAI36t37","tLAI37t38","tLAI38t39","tLAI39t40","tLAI40t41","tLAI41t42","tLAI42t43","tLAI43t44","tLAI44t45",
              "tLAI45t46","tLAI46t47","tLAI47t48","tLAI48t49","tLAI49t50","tLAI50t51","tLAI51t52","tLAI52t53","tLAI53t54","tLAI54t55","tLAI55t56","tLAI56t57","tLAI57t58","tLAI58t59","tLAI59t60","tLAI60t61","gLAI0t1",
              "gLAI1t2","gLAI2t3","gLAI3t4","gLAI4t5","gLAI5t6","gLAI6t7","gLAI7t8","gLAI8t9","gLAI9t10","gLAI10t11","gLAI11t12","gLAI12t13","gLAI13t14","gLAI14t15","gLAI15t16","gLAI16t17","gLAI17t18","gLAI18t19","gLAI19t20",
              "gLAI20t21","gLAI21t22","gLAI22t23","gLAI23t24","gLAI24t25","gLAI25t26","gLAI26t27","gLAI27t28","gLAI28t29","gLAI29t30","gLAI30t31","gLAI31t32","gLAI32t33","gLAI33t34","gLAI34t35","gLAI35t36","gLAI36t37",
              "gLAI37t38","gLAI38t39","gLAI39t40","gLAI40t41","gLAI41t42","gLAI42t43","gLAI43t44","gLAI44t45","gLAI45t46","gLAI46t47","gLAI47t48","gLAI48t49","gLAI49t50","gLAI50t51","gLAI51t52","gLAI52t53","gLAI53t54",
              "gLAI54t55","gLAI55t56","gLAI56t57","gLAI57t58","gLAI58t59","gLAI59t60","gLAI60t61","hgLAI0t1","hgLAI1t2","hgLAI2t3","hgLAI3t4","hgLAI4t5","hgLAI5t6","hgLAI6t7","hgLAI7t8","hgLAI8t9","hgLAI9t10","hgLAI10t11",
              "hgLAI11t12","hgLAI12t13","hgLAI13t14","hgLAI14t15","hgLAI15t16","hgLAI16t17","hgLAI17t18","hgLAI18t19","hgLAI19t20","hgLAI20t21","hgLAI21t22","hgLAI22t23","hgLAI23t24","hgLAI24t25","hgLAI25t26","hgLAI26t27",
              "hgLAI27t28","hgLAI28t29","hgLAI29t30","hgLAI30t31","hgLAI31t32","hgLAI32t33","hgLAI33t34","hgLAI34t35","hgLAI35t36","hgLAI36t37","hgLAI37t38","hgLAI38t39","hgLAI39t40","hgLAI40t41","hgLAI41t42","hgLAI42t43",
              "hgLAI43t44","hgLAI44t45","hgLAI45t46","hgLAI46t47","hgLAI47t48","hgLAI48t49","hgLAI49t50","hgLAI50t51","hgLAI51t52","hgLAI52t53","hgLAI53t54","hgLAI54t55","hgLAI55t56","hgLAI56t57","hgLAI57t58","hgLAI58t59",
              "hgLAI59t60","hgLAI60t61","hiLAI0t1","hiLAI1t2","hiLAI2t3","hiLAI3t4","hiLAI4t5","hiLAI5t6","hiLAI6t7","hiLAI7t8","hiLAI8t9","hiLAI9t10","hiLAI10t11","hiLAI11t12","hiLAI12t13","hiLAI13t14","hiLAI14t15","hiLAI15t16",
              "hiLAI16t17","hiLAI17t18","hiLAI18t19","hiLAI19t20","hiLAI20t21","hiLAI21t22","hiLAI22t23","hiLAI23t24","hiLAI24t25","hiLAI25t26","hiLAI26t27","hiLAI27t28","hiLAI28t29","hiLAI29t30","hiLAI30t31","hiLAI31t32",
              "hiLAI32t33","hiLAI33t34","hiLAI34t35","hiLAI35t36","hiLAI36t37","hiLAI37t38","hiLAI38t39","hiLAI39t40","hiLAI40t41","hiLAI41t42","hiLAI42t43","hiLAI43t44","hiLAI44t45","hiLAI45t46","hiLAI46t47","hiLAI47t48",
              "hiLAI48t49","hiLAI49t50","hiLAI50t51","hiLAI51t52","hiLAI52t53","hiLAI53t54","hiLAI54t55","hiLAI55t56","hiLAI56t57","hiLAI57t58","hiLAI58t59","hiLAI59t60","hiLAI60t61","hmLAI0t1","hmLAI1t2","hmLAI2t3","hmLAI3t4",
              "hmLAI4t5","hmLAI5t6","hmLAI6t7","hmLAI7t8","hmLAI8t9","hmLAI9t10","hmLAI10t11","hmLAI11t12","hmLAI12t13","hmLAI13t14","hmLAI14t15","hmLAI15t16","hmLAI16t17","hmLAI17t18","hmLAI18t19","hmLAI19t20","hmLAI20t21",
              "hmLAI21t22","hmLAI22t23","hmLAI23t24","hmLAI24t25","hmLAI25t26","hmLAI26t27","hmLAI27t28","hmLAI28t29","hmLAI29t30","hmLAI30t31","hmLAI31t32","hmLAI32t33","hmLAI33t34","hmLAI34t35","hmLAI35t36","hmLAI36t37",
              "hmLAI37t38","hmLAI38t39","hmLAI39t40","hmLAI40t41","hmLAI41t42","hmLAI42t43","hmLAI43t44","hmLAI44t45","hmLAI45t46","hmLAI46t47","hmLAI47t48","hmLAI48t49","hmLAI49t50","hmLAI50t51","hmLAI51t52","hmLAI52t53",
              "hmLAI53t54","hmLAI54t55","hmLAI55t56","hmLAI56t57","hmLAI57t58","hmLAI58t59","hmLAI59t60","hmLAI60t61")


## Function to parse command line arguments

## Functions for combining the simulated GEDI waveforms in CSV format
read_gedi_metrics_csv <- function(project) {
  
  metric.file <- sprintf("unnoisedMetric.%s.metric.csv", project)
  metric.file <- file.path("gedi_database_sims","metrics","gedi_metric_csv", metric.file)
  
  if ( file.exists(metric.file) ) {
    if ( file.info(metric.file)$size > 0) {
      metric.data <- read.csv(metric.file, stringsAsFactors=FALSE) %>%
        select(-X)
      
      key.data <- plyr::adply(metric.data$waveID, 1, 
                              function(x) as.data.frame(t(strsplit(x,":")[[1]])))
      names(key.data) <- c("id","project","plot","survey","subplot","g.id")
      key.data$subplot[key.data$subplot == "NA"] <- NA
      
      data <- cbind(key.data[,names(key.data)[!(names(key.data) %in% "id")]], 
                    metric.data[,names(metric.data)[!(names(metric.data) %in% c("waveID","X","X1"))]], 
                    row.names=NULL)
    } else {
      data <- NULL
    }
  } else {
    data <- NULL
  }
  
  data
}

## Extract all trees in a GEDI footprint and extract estiamtes of biomass
extract_gedi_biomass <- function(data,treedata,fp_radius) {
  #browser()
  geom.obj <- as.data.frame( jsonlite::fromJSON(data$g.fp), stringsAsFactors=FALSE)
  data$plot <- ifelse(!is.na(data$plot), data$plot, "1")
  treedata$plot <- ifelse(!is.na(treedata$plot), treedata$plot, "1")
  ii <- (treedata$project == data$project) & (treedata$plot == data$plot) & (!is.na(treedata$x) & !is.na(treedata$y))
  
  if ( any(ii) ) {
    tree.coords <- data.frame(x=treedata$x[ii],y=treedata$y[ii],
                              X=treedata$x[ii],Y=treedata$y[ii])
    #coordinates(tree.coords) <- c("x","y")
    #proj4string(tree.coords) <- CRS(paste("+init=epsg",data$p.epsg,sep=":"))
    tree.coords <- vect(tree.coords, geom = c("X", "Y"), crs = paste0("EPSG:",data$p.epsg[1]))
    #crs(tree.coords) <- paste0("EPSG:",data$p.epsg[1])
    #tree.coords <- spTransform(tree.coords, CRS(paste("+init=epsg",data$l.epsg,sep=":")))
    tree.coords <- terra::project(tree.coords, paste0("EPSG:",data$l.epsg[1]))
    tree.coords <- as.data.frame(tree.coords)
    treedata$x[ii] <- tree.coords$x
    treedata$y[ii] <- tree.coords$y
    newdata <- plyr::adply(geom.obj, 1, extract_footprint_trees, treedata[ii,], fp_radius)
  } else {
    newdata <- cbind(g.id = sprintf("x%09iy%09i", round(geom.obj$x*100), round(geom.obj$y*100)),
                     g.x = geom.obj$x,
                     g.y = geom.obj$y,
                     g.ix = geom.obj$ix,
                     g.iy = geom.obj$iy,
                     g.edge = geom.obj$edge,
                     g.edge.frac = geom.obj$edge.frac,
                     g.agb = 0,
                     g.agb.valid = 1,
                     g.agbd.ha = 0,
                     g.sn = 0,
                     g.snd.ha = 0,
                     g.sba = 0,
                     g.sba.ha = 0,
                     g.wsg.ba = 0,
                     g.h.t.max = 0,
                     row.names = NULL, stringsAsFactors=FALSE)
  }
  
  cbind(data, newdata, row.names=NULL)
}

## Compute the estimate of biomass within a footprint
extract_footprint_trees <- function(geom.obj, treedata, fp_radius) {
  #browser()
  d <- sqrt((treedata$x-geom.obj$x)^2 + (treedata$y-geom.obj$y)^2)
  p.area.ha <- (pi * fp_radius^2) / 1e4
  ii <- d <= fp_radius
  
  if ( any(ii, na.rm=TRUE) ) {
    if ( all(is.na(treedata$d.stem.valid[ii])) ) {
      g.agb.valid <- 1
    } else {
      min.d.stem.valid <- min(treedata$d.stem.valid[ii], na.rm=TRUE)
      if ( is.infinite(min.d.stem.valid) ) {
        g.agb.valid <- 1
      } else {
        if ( min.d.stem.valid > 0 ) {
          g.agb.valid <- 1
        } else {
          g.agb.valid <- 0
        }
      }
    }
  } else {
    g.agb.valid <- 1
  }
  
  h.t <- ifelse(is.na(treedata$h.t), treedata$h.t.mod, treedata$h.t)
  
  cbind(g.id = sprintf("x%09iy%09i", round(geom.obj$x*100), round(geom.obj$y*100)),
        g.x = geom.obj$x,
        g.y = geom.obj$y,
        g.ix = geom.obj$ix,
        g.iy = geom.obj$iy,
        g.edge = geom.obj$edge,
        g.edge.frac = geom.obj$edge.frac,
        g.agb = sum(treedata$m.agb[ii], na.rm=TRUE),
        g.agb.valid = g.agb.valid,
        g.agbd.ha = sum( (treedata$m.agb[ii] / 1e3) / p.area.ha, na.rm=TRUE),
        g.sn = sum( ii, na.rm=TRUE),
        g.snd.ha = sum( ii, na.rm=TRUE) / p.area.ha,
        g.sba = sum( treedata$a.stem[ii], na.rm=TRUE),
        g.sba.ha = sum( treedata$a.stem[ii] / p.area.ha, na.rm=TRUE),
        g.wsg.ba = sum( treedata$wsg[ii]*treedata$a.stem[ii], na.rm=TRUE) / sum( treedata$a.stem[ii], na.rm=TRUE),
        g.h.t.max = ifelse(any(!is.na(h.t[ii]), na.rm=TRUE), max(h.t[ii], na.rm=TRUE), NA),
        row.names = NULL, stringsAsFactors=FALSE)
}

## Functions for footprints with no inventory data
stack_gedi_biomass <- function(data) {
  #browser()
  geom.obj <- as.data.frame( fromJSON(data$g.fp) )
  cbind(g.id = sprintf("x%09iy%09i", round(geom.obj$x*100), round(geom.obj$y*100)),
        g.x = geom.obj$x,
        g.y = geom.obj$y,
        g.ix = geom.obj$ix,
        g.iy = geom.obj$iy,
        g.edge = geom.obj$edge,
        g.edge.frac = geom.obj$edge.frac,
        g.agb = NA,
        g.agb.valid = NA,
        g.agbd.ha = NA,
        g.sn = NA,
        g.snd.ha = NA,
        g.sba = NA,
        g.sba.ha = NA,
        g.wsg.ba = NA,
        g.h.t.max = NA,
        row.names = NULL, stringsAsFactors=FALSE)
}

aggregateFootprintMetrics <- function(data, nfootprints, fp.radius, block=FALSE) {
  #browser()
  g.nfootprints <- nrow(data)
  
  valid <- (max(data$g.ix, na.rm=TRUE) >= nfootprints) & (max(data$g.iy, na.rm=TRUE) >= nfootprints) &
    ((g.nfootprints == nfootprints) | (g.nfootprints == nfootprints^2))
  
  if ( valid ) {
    
    exclude.gedi.var <- c("waveID","trueground","truetop","groundslope","ALScover","gHeight","maxGround","inflGround","signaltop","signalbottom","filename","pSigma","fSigma","linkM","linkCov","lon","lat","groundOverlap","groundMin","groundInfl","waveEnergy")
    include.gedi.var <- gedi.var[!(exclude.gedi.var %in% gedi.var)]
    exclude.plot.var <- c("wave_ID","g.id","g.ix","g.iy")
    
    include.var <- names(data)[!( (names(data) %in% exclude.plot.var) | (names(data) %in% exclude.gedi.var) )]
    newdata <- data.frame(data[1,include.var])
    
    newdata$g.edge <- max(data$g.edge, na.rm=TRUE)
    newdata$g.edge.frac <- max(data$g.edge.frac, na.rm=TRUE)
    
    newdata$g.agb <- sum(data$g.agb, na.rm=TRUE)
    newdata$g.agb.valid <- min(data$g.agb.valid, na.rm=TRUE)
    newdata$g.agbd.ha <- mean(data$g.agbd.ha, na.rm=TRUE)
    newdata$g.agbd.ha.sd <- sd(data$g.agbd.ha, na.rm=TRUE)
    
    newdata$g.sn <- sum(data$g.sn, na.rm=TRUE)
    newdata$g.snd.ha <- mean(data$g.snd.ha, na.rm=TRUE)
    newdata$g.snd.ha.sd <- sd(data$g.snd.ha, na.rm=TRUE)
    
    newdata$g.sba <- sum(data$g.sba, na.rm=TRUE)
    newdata$g.sba.ha <- mean(data$g.sba.ha, na.rm=TRUE)
    newdata$g.sba.ha.sd <- sd(data$g.sba.ha, na.rm=TRUE)
    
    newdata$g.wsg.ba <- mean(data$g.wsg.ba, na.rm=TRUE)
    newdata$g.wsg.ba.sd <- sd(data$g.wsg.ba, na.rm=TRUE)
    
    newdata$g.h.t.max <- ifelse(any(!is.na(data$g.h.t.max), na.rm=TRUE), max(data$g.h.t.max, na.rm=TRUE), NA)
    
    newdata$g.nfootprints <- nfootprints
    newdata$g.ntracks <- ifelse(block, nfootprints, 1) 
    
    newdata$g.colocation <- NA #calc_colocation(data, fp.radius)
    
    newdata[,include.gedi.var] <- colMeans(data[,include.gedi.var], na.rm=TRUE)
    
  } else {
    
    newdata <- data.frame()
    
  }
  
  newdata
  
}

calc_colocation <- function(data, fp.radius) {
  #browser()
  #p.proj4str <- CRS(paste("+init=epsg",data$p.epsg[1],sep=":"))
  p.proj4str <- crs(paste("+init=epsg",data$p.epsg[1],sep=":"))
  if ( !is.na(data$sp.geom[1]) ) {
    #p.geom <- readWKT(data$sp.geom[1], p4s=p.proj4str)
    p.geom <- vect(data$sp.geom[1], crs=p.proj4str)
  } else {
    p.geom <- vect(data$sp.geom[1], crs=p.proj4str)
  }
  
  l.proj4str <- crs(paste("EPSG",data$l.epsg[1],sep=":"))
  fp.data <- data.frame(x=data$g.x,y=data$g.y)
  #coordinates(fp.data) <- ~x+y
  #proj4string(fp.data) <- l.proj4str
  fp.data <- vect(fp.data, geom = c("x", "y"), crs = l.proj4str)
  
  fp.data <- terra::buffer(fp.data, width=fp.radius, quadsegs=10)
  #fp.data <- gBuffer(fp.data, width=fp.radius, quadsegs=10, byid=TRUE)
  #fp.data <- spTransform(fp.data, CRS(paste("+init=epsg",data$p.epsg[1],sep=":")))
  fp.data <- terra::project(fp.data, crs(paste("EPSG",data$p.epsg[1],sep=":"))) 
  
  fp.intersection <- relate(p.geom, fp.data, relation = "intersects")
  fp.intersection <- subset(p.geom, rowSums(fp.intersection) > 0)
  #fp.intersection <- gIntersection(as(fp.data,"Spatial"), as(p.geom,"Spatial"))
  
  if (data$p.stemmap == 0) {
    colocation <- ( expanse(fp.intersection) / expanse(p.geom) ) * ( expanse(fp.intersection) / expanse(fp.data) )
  } else {
    colocation <- expanse(fp.intersection) / expanse(fp.data)
  }
  
  colocation
}

combine_gedi_metrics <- function(gedicalval, fp_radius, nfootprints=1) {
  #browser()
  new.data.exists <- FALSE
  keyvar <- c("project","plot","survey","subplot","g.id")
  projects <- unique( gedicalval$plotdata$l.project )
  projects <- projects[!is.na(projects)]
  for (project in projects) {
    if ( !is.na(project) ) {
      
      # Subset the data to the desired project and survey event
      ii <- !is.na(gedicalval$plotdata$l.project) & (gedicalval$plotdata$l.project == project)
      plotdata <- gedicalval$plotdata[ii,]
      ii <- !is.na(gedicalval$gedidata$l.project) & (gedicalval$gedidata$l.project == project)
      gedidata <- gedicalval$gedidata[ii,]
      ii <- (gedicalval$treedata$project %in% plotdata$project) & (gedicalval$treedata$survey %in% plotdata$survey)
      treedata <- gedicalval$treedata[ii,]
      
      # Join the gedi footprints with the plotdata
      data <- cbind(plotdata, gedidata)
      remove(plotdata, gedidata)
      
      # Extract the footprint biomass for stem mapped plots/subplots
      ii <- (data$p.stemmap == 1) & !is.na(data$g.fp)
      stemmap.biomass <- plyr::adply(data[ii,], 1, extract_gedi_biomass, treedata, fp_radius)
      jj <- (data$p.stemmap == 0) & !is.na(data$g.fp)
      non.stemmap.biomass <- plyr::adply(data[jj,], 1, stack_gedi_biomass)
      remove(treedata)
      
      # Combine the datasets
      includevar <- c("project","plot","survey","subplot","p.geom","sp.geom","p.epsg","pft.modis","pft.name",
                      "wwf.ecoregion","date","region","vegetation","map","mat","latitude","longitude",
                      "p.sample","p.stemmap","p.origin","p.shape","p.area","p.mindiam",
                      "agb","agb.valid","agb.lower","agb.upper","agbd.ha","agbd.ha.lower","agbd.ha.upper",
                      "sn","snd.ha","sba","sba.ha","swsg.ba","h.t.max",
                      "l.project","l.instr","l.epsg","l.date",
                      "g.id","g.x","g.y","g.ix","g.iy","g.edge","g.edge.frac",
                      "g.agb","g.agb.valid","g.agbd.ha","g.sn","g.snd.ha","g.sba","g.sba.ha","g.wsg.ba","g.h.t.max")
      if ( any(ii) & any(jj) ) data <- plyr::arrange(rbind(stemmap.biomass[,includevar],non.stemmap.biomass[,includevar]), 
                                                     project, plot, survey, subplot)
      if ( any(ii) & !any(jj) ) data <- plyr::arrange(stemmap.biomass[,includevar], project, plot, survey, subplot)
      if ( !any(ii) & any(jj) ) data <- plyr::arrange(non.stemmap.biomass[,includevar], project, plot, survey, subplot)
      remove(non.stemmap.biomass,stemmap.biomass)
      gc(verbose=FALSE)
      
      # Join with the GEDI metrics
      if ( "g.id" %in% names(data) ) {
        
        # This is a hack because I have no idea why plyr is converting everything to factors
        data$g.x <- as.numeric(as.character(data$g.x))
        data$g.y <- as.numeric(as.character(data$g.y))
        data$g.ix <- as.integer(data$g.ix)
        data$g.iy <- as.integer(data$g.iy)
        data$g.edge <- as.integer(data$g.edge)
        data$g.edge.frac <- as.numeric(as.character(data$g.edge.frac))
        data$g.agb <- as.numeric(as.character(data$g.agb))
        data$g.agb.valid <- as.numeric(as.character(data$g.agb.valid))
        data$g.agbd.ha <- as.numeric(as.character(data$g.agbd.ha))
        data$g.sn <- as.numeric(as.character(data$g.sn))
        data$g.snd.ha <- as.numeric(as.character(data$g.snd.ha))
        data$g.sba <- as.numeric(as.character(data$g.sba))
        data$g.sba.ha <- as.numeric(as.character(data$g.sba.ha))
        data$g.wsg.ba <- as.numeric(as.character(data$g.wsg.ba))
        data$g.h.t.max <- as.numeric(as.character(data$g.h.t.max))
        
        gedimetrics.0 <- read_gedi_metrics_csv(project)
        
        for ( gedimetrics in list(gedimetrics.0) ) {
          
          if ( !is.null(gedimetrics) ) {
            #write(project, stdout())
            jj <- (data$l.project==project) & !is.na(data$project) & (data$g.edge.frac > 0.5)
            data.tmp <- data[jj,]
            
            # Join the waveform metrics
            data.tmp <- plyr::join(data.tmp, gedimetrics, by=keyvar, type="left", match="all")
            remove(gedimetrics)
            
            # Determine the clump ID
            if ( max(data.tmp$g.ix, na.rm=TRUE) >= max(data.tmp$g.iy, na.rm=TRUE) ) {
              data.tmp$g.clump <- as.integer( (data.tmp$g.ix - 1) / nfootprints ) + 1
              data.tmp$g.track <- data.tmp$g.iy
            } else {
              data.tmp$g.clump <- as.integer( (data.tmp$g.iy - 1) / nfootprints ) + 1
              data.tmp$g.track <- data.tmp$g.ix
            }
            
            # Determine the block ID
            if ( ( max(data.tmp$g.ix, na.rm=TRUE) >= nfootprints ) & ( max(data.tmp$g.iy, na.rm=TRUE) >= nfootprints ) ) {
              g.block.x <- as.integer( (data.tmp$g.ix - 1) / nfootprints ) + 1
              g.block.y <- as.integer( (data.tmp$g.iy - 1) / nfootprints ) + 1
              data.tmp$g.block <- max(g.block.x + 1) * g.block.y + g.block.x
            } else {
              data.tmp$g.block <- NA
            }
            
            # Aggregate the biomass and waveform metrics along track (1 x n)
            data.tmp.1 <- plyr::ddply(data.tmp, c("project","plot","subplot","survey","g.clump","g.track"), aggregateFootprintMetrics, nfootprints, fp_radius, block=FALSE)
            data.tmp.1 <- data.tmp.1[,!(names(data.tmp.1) %in% c("p.geom","sp.geom","p.epsg"))]
            
            # Aggregate the biomass and waveform metrics along and across track (n x n)
            if (nfootprints > 1) {
              data.tmp.2 <- plyr::ddply(data.tmp, c("project","plot","subplot","survey","g.block"), aggregateFootprintMetrics, nfootprints, fp_radius, block=TRUE)
              data.tmp.2 <- data.tmp.2[,!(names(data.tmp.2) %in% c("p.geom","sp.geom","p.epsg"))]
              data.tmp <- rbind(data.tmp.1,data.tmp.2) 
            } else {
              data.tmp <- data.tmp.1
            }
            
            # Update the data frame
            if ( new.data.exists ){
              if (ncol(data.tmp) == ncol(new.data)){
                new.data <- rbind(new.data, data.tmp)
              }
            } else {
              new.data <- data.tmp
              new.data.exists <- TRUE
            }
            
          }
          
        }
        
        gc(verbose=FALSE)
        
      }
    }
  }
  
  # Join the results with the input GEDICalVal data
  if ( !(new.data.exists) ){
    return(NULL)
  } else {
    return( new.data )
  }
  
}


## Run the footprint processing and merging
footprint.data <- function(project, inputlabel = "r01", outputlabel = "r03"){
  
  rdata.file <- file.path("shiny", sprintf("gedicalval_%s_%s.rds", project, inputlabel))
  gedicalval <- readRDS(rdata.file)
  
  # Create and append the footprint data for all noise cases
  footprint.data <- combine_gedi_metrics(gedicalval, gedicalval$fp_radius)#, nfootprints=opt$nfootprints)                                     
  if ( !is.null(footprint.data) ) {
    gedicalval[["fpdata"]] <- footprint.data
  }
  
  # Write to a new output file
  newfile <- file.path("shiny", sprintf("gedicalval_%s_%s.rds", project, outputlabel))
  saveRDS(gedicalval, file=newfile)
  
}
