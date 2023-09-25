## This function is going to clip multiple raster maps based on one shape file. 
# For the first term of the function ("r") one provides the local folder path where the multiple rasters are located or the raster file. 
# For the second term of the fuction ("shpfile")one needs to provide the shape file.
# The last term ("outputPath")is the path one wants the clipped rasters to be stored. 
# P.S. Projections of both object can be different, the functions transforms the shape file crs to the one of the raster.

clipMaps<-function(r, shpfile, outputPath){

  if (grepl(".tif", r)==T){
   
      rasterToMatch <- terra::rast(file.path(r))
      
      studyArea <- terra::vect(file.path(shpfile))
      
      
      #postProcess studyArea
      studyArea <- reproducible::postProcess(studyArea,
                                             destinationPath = getwd(),
                                             filename2 = "studyArea",
                                             useTerra = TRUE,
                                             fun = "terra::vect", #use the function vect
                                             targetCRS = crs(rasterToMatch), #make crs same as rasterToMatch
                                             overwrite = TRUE,
                                             verbose = TRUE)
      #crop and mask rasterToMatch
      rasterToMatch <- terra::mask(terra::crop(rasterToMatch, studyArea), studyArea)
      names(rasterToMatch) <- "rasterToMatch"
      # clearPlot()
      # Plot(sim$rasterToMatch)
      # plot(rasterToMatch, colNA = "grey")
      
      
      loc0<-rev(setdiff(strsplit(r,"/|\\\\")[[1]], ""))
      loc1<-gsub('.tif','',loc0[1])
      loc2<-paste(loc1,"Clipped.tif", sep="")
      loc3<-paste(outputPath,"/", sep="")
      locf<-paste(loc3, loc2, sep="")
      
      f <- file.path(locf)
      
      writeRaster(rasterToMatch, f,overwrite=TRUE)
    
  } else{
    
  listb<-list.files(r)
  preloc<- paste (r, "/", sep = "")
  
  for (i in 1:length(listb)){ 
    loc<- paste (preloc, listb[i], sep = "")
    rasterToMatch <- terra::rast(file.path(loc))
    
    studyArea <- terra::vect(file.path(shpfile))
    
    
    #postProcess studyArea
    studyArea <- reproducible::postProcess(studyArea,
                                           destinationPath = getwd(),
                                           filename2 = "studyArea",
                                           useTerra = TRUE,
                                           fun = "terra::vect", #use the function vect
                                           targetCRS = crs(rasterToMatch), #make crs same as rasterToMatch
                                           overwrite = TRUE,
                                           verbose = TRUE)
    #crop and mask rasterToMatch
    rasterToMatch <- terra::mask(terra::crop(rasterToMatch, studyArea), studyArea)
    names(rasterToMatch) <- "rasterToMatch"
    # clearPlot()
    # Plot(sim$rasterToMatch)
   #plot(rasterToMatch, colNA = "grey")
    
    
    loc1<-gsub('.tif','',listb[i])
    loc2<-paste(loc1,"Clipped.tif", sep="")
    loc3<-paste(outputPath,"/", sep="")
    locf<-paste(loc3, loc2, sep="")
    
    f <- file.path(locf)
    
    writeRaster(rasterToMatch, f,overwrite=TRUE)
  }
  }
}
