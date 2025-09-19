checkGEDICalVal <- function(data.list, outfile){
  
  write("CHECK LOG", file=outfile)
  
  # Loop through each project
  projects <- unique(data.list$plotdata$project)
  for ( project in projects ) {
    write(project, file=outfile, append=TRUE)
    ii <- (data.list$plotdata$project == project)
    write(paste("  Number of plots",sum(ii),sep=": "), file=outfile, append=TRUE)
    write(paste("  Number stem-mapped",sum(data.list$plotdata$p.stemmap[ii]),sep=": "), file=outfile, append=TRUE)
    write("  Invalid plot observations", file=outfile, append=TRUE)
    
    jj <- is.na(data.list$plotdata$agbd.ha[ii]) | (data.list$plotdata$agbd.ha[ii] == 0)
    write(paste("    AGBD",sum(jj),sep=": "), file=outfile, append=TRUE)
    
    jj <- is.na(data.list$plotdata$sba.ha[ii]) | (data.list$plotdata$sba.ha[ii] == 0)
    write(paste("    SBA",sum(jj),sep=": "), file=outfile, append=TRUE)    
    
    jj <- is.na(data.list$plotdata$swsg.ba[ii]) | (data.list$plotdata$swsg.ba[ii] == 0)
    write(paste("    WSG",sum(jj),sep=": "), file=outfile, append=TRUE)
    
    if ( "fpdata" %in% names(data.list) ) {
      kk <- (data.list$fpdata$project == project)
      write(paste("  Number of footprints",sum(kk),sep=": "), file=outfile, append=TRUE)
      
      if ( sum(kk) > 0 ) {
        write("  Number of aggregates", file=outfile, append=TRUE)
        for ( tk in 1:max(data.list$fpdata$g.ntracks[kk], na.rm=TRUE) ) {
          for ( i in 1:max(data.list$fpdata$g.nfootprints[kk], na.rm=TRUE) ) {
            ll <- (data.list$fpdata$g.ntracks[kk] == tk) & (data.list$fpdata$g.nfootprints[kk] == i)
            if (i == 1) {
              cnt <- paste(sum(ll), sep="")
            } else {
              cnt <- paste(cnt, sum(ll), sep=",")
            }
          }
          write(paste("    ",tk," track: (", cnt, ")", sep=""), file=outfile, append=TRUE)
        }
      }
      
      write("  Invalid footprint observations", file=outfile, append=TRUE)
      
      ll <- is.na(data.list$fpdata$rhGauss100[kk])
      write(paste("    RH100",sum(ll),sep=": "), file=outfile, append=TRUE)
      
      ll <- is.na(data.list$fpdata$g.agbd.ha[kk]) | (data.list$fpdata$g.agbd.ha[kk] == 0)
      write(paste("    AGBD",sum(ll),sep=": "), file=outfile, append=TRUE)
    
      ll <- is.na(data.list$fpdata$g.sba.ha[kk]) | (data.list$fpdata$g.sba.ha[kk] == 0)
      write(paste("    SBA",sum(ll),sep=": "), file=outfile, append=TRUE)      
      
      ll <- is.na(data.list$fpdata$g.wsg.ba[kk]) | (data.list$fpdata$g.wsg.ba[kk] == 0)
      write(paste("    WSG",sum(ll),sep=": "), file=outfile, append=TRUE)
    }
    
  }
  
}

rdata.file <- file.path("shiny", "gedicalvaldata.rds")
gedicalval <- readRDS(rdata.file)

outfile <- paste( tools::file_path_sans_ext(rdata.file), "log", sep=".")

checkGEDICalVal(gedicalval,outfile)
