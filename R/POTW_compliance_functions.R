# POTW_compliance_functions.R
#   Functions used in the code "app.R" in the program "POTW_compliance_v202_start.R"
#
#   List of functions:
#
# settings.list.create
# settings.change.params
# SCB_map
# grep_start
# grep.start.mult
# calc.param.sel
# plot.profiles
# plot.one.profile
# plot.ref.profile
# plot.prof.graph
# Sigma.UML.calc
# geodetic.distance
# CDOM.plume.detect
# Ref.stns.select
# UML.Z.calc
# Ref.Prof.calc
# filter_np
# V.diff.calc
# TdegC.V.fit.calc
# V.diff.Entr.calc
# V.Z.min.calc
# outr.detect
# report.data.file
# report.data.selected
# report.plume.list
# report.plume.settings
# report.ref.list
# report.ref.settings
# report.outr.param
# report.ref.prof
# report.outr.method
# report.outr.settings
# report.entr.setting
# report.outr.list
# report.max.decrease.depths
#
# Function settings.list.create ***********************************************
settings.list.create <- function( Plume.settings.R, Outr.param.settings ) {
  # Plume settings
  indx.plume.settings <- grep( "plume_", Plume.settings.R$ShortName )
  Plume.settings <- Plume.settings.R[ indx.plume.settings, ]
  Plume.settings$Value <- as.numeric( Plume.settings$Value )
  # Ref. settings
  indx.ref.settings <- grep( "ref_", Plume.settings.R$ShortName )
  Ref.settings <- Plume.settings.R[ indx.ref.settings, ]
  Ref.settings$Value <- as.numeric( Ref.settings$Value )
  # Outrange settings
  indx.outr.settings <- grep( "outr_", Plume.settings.R$ShortName )
  Outr.settings <- Plume.settings.R[ indx.outr.settings, ]
  Outr.settings$Value <- as.numeric( Outr.settings$Value )
  # Profile compare method setting
  indx.prof.comp.settings <- grep( "prof_comp_", Plume.settings.R$ShortName )
  Prof.comp.settings <- Plume.settings.R[ indx.prof.comp.settings, ]
  Outr.settings$Value <- as.character( Outr.settings$Value )
  # Entrainment settings
  indx.entr.settings <- grep( "entr_", Plume.settings.R$ShortName )
  Entr.settings <- Plume.settings.R[ indx.entr.settings, ]
  Entr.settings$Value <- as.logical( Entr.settings$Value )
  #  
  #browser()
  settings <- list( 
    d.rho.uml = 0.125,
    Outfall = data.frame( 
      Agency = c( "Hyperion","LACSD","OCSD","San Diego - Point Loma" ),
      Station = c( "3505","2903","2205","F30"),
      depth=c(60,60,57,93) 
    ),
    Plume = Plume.settings,
    Ref = Ref.settings,
    Outr = Outr.settings,
    Prof.comp = Prof.comp.settings,
    Entr = Entr.settings,
    Outr.params = Outr.param.settings
  )
  # Select Ref and Outr settings for first parameter in the list (DO)
  Param1 <- settings$Outr.params$outr_param_name[1]
  settings <- settings.change.params( settings, Param1 )
  #
  #browser()
  return( settings )
}
# End of function settings.list.create ****************************************
#
# Function settings.change.param **********************************************
settings.change.params <- function( settings, Param1 ) {
  #browser()
  #param.list <- colnames( settings$Outr.params )
  indx.param <- which( settings$Outr.params$outr_param_name == Param1 )[1]
  ref.list <- c( "ref_CDOM_thrsh", "ref_dfo_max", "ref_stn_min" )
  for( param.change in ref.list ) {
    #param.change <- param.list[k.param]
    settings$Ref$Value[ settings$Ref$ShortName == param.change ] <- 
      as.numeric( settings$Outr.params[ indx.param, param.change ] )
  }
  outr.list <- c( "outr_refprof_dRho", "outr_refprof_filtW", "outr_refprof_Kstd",
                  "outr_refprof_Zwindow", "outr_threshold" )
  for( param.change in outr.list ) {
    #param.change <- param.list[k.param]
    settings$Outr$Value[ settings$Outr$ShortName == param.change ] <- 
      as.numeric( settings$Outr.params[ indx.param, param.change ] )
  }
  param.change <- "prof_comp_method"
  settings$Prof.comp$Value[ settings$Prof.comp$ShortName == param.change ] <- 
    settings$Outr.params[ indx.param, param.change ]
  #
  #browser()
  return( settings )
}
# End of function settings.change.params ***************************************
#
# Function SCB_map ************************************************************
SCB_map <- function( SCB_usa, GSHHS_mex, Pipes_PBS, Stn_coords, map.lims ) {
  suppressWarnings( plotMap( SCB_usa,  # USA coastline
                             xlim = map.lims$x, ylim = map.lims$y, 
                             col="#CCCCCC", bg="#0088FF", lwd=1, xlab="", ylab="", 
                             plt = c(0,1,0,1),
                             xaxt = "n", yaxt = "n" ) )
  suppressWarnings( addPolys( GSHHS_mex,  # Mexico coastline
                              xlim = map.lims$x, ylim = map.lims$y, 
                              col="#CCCCCC", bg="#0088FF", lwd=1, xlab="", ylab="", 
                              plt = c(0,1,0,1),
                              xaxt = "n", yaxt = "n" ) )
  # Pipelines
  if( !is.null( Pipes_PBS ) ) {
    suppressWarnings( addLines( as.PolySet( Pipes_PBS, projection="LL" ), col = "black", lwd = 2 ) )
  }
  # Stations
  Stn_coords.PD <- data.frame( PID = 1:nrow(Stn_coords), 
                               X = Stn_coords$Longitude, 
                               Y = Stn_coords$Latitude )
  Stn_coords.PD <- as.PolyData( Stn_coords.PD, projection = "LL" )
  addPoints( Stn_coords.PD, pch = 21, cex = 1.6, bg = "#FFFF00" ) 
  legend.text <- "Stations"
  legend.pch <- 21
  legend.pt.cex <- 1.6
  legend.pt.bg <- "#FFFF00"
  #browser()
  if( sum( Stn_coords$Plume > 0 ) ) {
    addPoints( Stn_coords.PD[Stn_coords$Plume,], 
               pch = 21, cex = 3.0, bg = "#FF0000" ) 
    legend.text <- c( legend.text, "Plume" )
    legend.pch <- c( legend.pch, 21 )
    legend.pt.cex <- c( legend.pt.cex, 3.0 )
    legend.pt.bg <- c( legend.pt.bg, "#FF0000" )
  }
  if( sum( Stn_coords$Ref > 0 ) ) {
    addPoints( Stn_coords.PD[Stn_coords$Ref,], 
               pch = 21, cex = 3.0, bg = "#00FF00" ) 
    legend.text <- c( legend.text, "Reference" )
    legend.pch <- c( legend.pch, 21 )
    legend.pt.cex <- c( legend.pt.cex, 3.0 )
    legend.pt.bg <- c( legend.pt.bg, "#00FF00" )
  }
  if( sum( Stn_coords$Outrange > 0 ) ) {
    addPoints( Stn_coords.PD[Stn_coords$Outrange,], 
               pch = 21, cex = 5.0, bg = "#FFBB00" ) 
    legend.text <- c( legend.text, "Outranges" )
    legend.pch <- c( legend.pch, 21 )
    legend.pt.cex <- c( legend.pt.cex, 5.0 )
    legend.pt.bg <- c( legend.pt.bg, "#FFBB00" )
  }
  
  legend( "topright", legend = legend.text, 
          pch = legend.pch, pt.cex = legend.pt.cex, pt.bg = legend.pt.bg
          
  )
  
}
# End of function SCB_map ******************************************************

# Function grep_start **********************************************************
# Find the first index of the string (in str_list) starting 
#   with a sub_str
grep_start <- function( sub_str, str_list ){
  indx_sel <- grep( sub_str, substr( str_list, 1, nchar(sub_str) ) )
  if( length(indx_sel)>1 ) indx_sel <- indx_sel[1]
  indx_sel <- indx_sel
}
# End of function grep_start ***************************************************

# Function grep.start.mult *****************************************************
# Finds the first index of the string (in str_list) starting 
#   with a one of the elements of the "sub_str" (which may be several strings)
#  Returns NA if nothing was found
grep.start.mult <- function ( sub_str, str_list ) {
  #
  #indx_sel <- which( substr( str_list, 1, nchar(sub_str) ) %in% sub_str )
  indx_sel <- apply( as.data.frame( sub_str, stringsAsFactors = FALSE), 1, 
                     function(x) { grep( x, substr( str_list, 1, nchar(x) ) ) } )
  indx_sel <- as.numeric( indx_sel )
  indx_sel <- indx_sel[ is.finite( indx_sel ) ]
  if( length( indx_sel ) > 1 ) indx_sel <- indx_sel[1]
  if( length( indx_sel ) == 0 ) indx_sel <- NA
  #
  return( indx_sel )
}
# End of function grep.start.mult **********************************************

# Function calc.param.sel *****************************************************
# Calculates data frame param.sel, including columns:
#     col.name and k.col (the name(s) the column should start and its number)
calc.param.sel <- function( param.list, Param.names, data.col.names ) {
  n.param <- length( param.list )
  param.sel <- data.frame( k.col = rep( NA, n.param ), col.name = character( n.param ),
                           stringsAsFactors = FALSE )
  rownames( param.sel ) <- param.list
  for( k.param.sel in 1:n.param ) {
    k.param <- which( Param.names$Parameter == rownames( param.sel )[ k.param.sel ] )
    if( length( k.param ) == 0 ) {
      param.sel <- NULL    
    } else {
      param.sel$k.col[ k.param.sel ] <- grep.start.mult( Param.names$ParamNameStarts[ k.param ], data.col.names )
      if( is.na( param.sel$k.col[ k.param.sel ] ) ) {
        param.sel$col.name[ k.param.sel ] <- paste( Param.names$ParamNameStarts[ k.param ], collapse = "," ) 
      } else {
        param.sel$col.name[ k.param.sel ] <- paste( data.col.names[ param.sel$k.col[ k.param.sel ] ], collapse = "," )  
      }
    }
  }
  #browser()
  #
  return( param.sel )
}
# End of function calc.param.sel **********************************************

# Function plot.profiles *******************************************************
plot.profiles <- function( X, Z, StnID, lwd = 1, col = "#000000" ) {
  StnID.list <- data.frame( StnID = unique( StnID ) )
  #plot.one.profile( StnID.list[1], X, Z, StnID, lwd = 1, col = "#000000" )
  apply( X = StnID.list, MARGIN = 1, FUN = plot.one.profile, X, Z, StnID, 
         1, col )
}
# End of function plot.profiles ************************************************

# Function plot.one.profile ****************************************************
plot.one.profile <- function( StnID.plot, X, Z, StnID, lwd = 1, col = "#000000" ) {
  StnID.plot <- strsplit( StnID.plot, split = " " )[[1]][1]
  indx.stn <- ( StnID == StnID.plot )
  X.plot <- X[indx.stn]
  Z.plot <- Z[indx.stn]
  indx.sort <- order( Z.plot )
  X.plot <- X.plot[ indx.sort ]
  Z.plot <- Z.plot[ indx.sort ]
  lines( X[indx.stn], Z[indx.stn], lwd = lwd, col = col )
}
# End of function plot.one.profile *********************************************

# Function plot.ref.profile ****************************************************
plot.ref.profile <-function( RefProf, Ref.Prof.Kstd ) {
  # RefProf[ c("Vertical axis","V-mean","V-std")]
  #browser()
  RefProf <- RefProf[ order( RefProf[,1] ), ]
  lines( RefProf[,2], RefProf[,1], col = "green", lwd = 3, lty = 3 )
  lines( RefProf[,2] + Ref.Prof.Kstd * RefProf[,3], RefProf[,1] , 
         col = "green", lwd = 3, lty = 3 )
  lines( RefProf[,2] - Ref.Prof.Kstd * RefProf[,3], RefProf[,1], 
         col = "green", lwd = 3, lty = 1 )
}
# End of function plot.ref.profile *********************************************

# Function plot.prof.graph ****************************************************
plot.prof.graph <- function( x.data, z.data, StnID, prof.Z.ranges, main, ylab ) {
  prof.Z.lims <- data.frame( 
    x = extendrange( x.data, f = 0.1 ),
    y = extendrange( z.data, f = 0.1 ) )
  if( !is.null( prof.Z.ranges$x ) & !is.null( prof.Z.ranges$y ) ) {
    prof.Z.lims$x = prof.Z.ranges$x
    prof.Z.lims$y = prof.Z.ranges$y
  }
  plot( x.data, z.data, main = main,
        xlim = prof.Z.lims$x, 
        ylim = rev( prof.Z.lims$y ),
        xlab = "", ylab = ylab,
        font.main = 2, font.lab = 2, cex.main = 1.6, cex.lab = 1.2 )
  plot.profiles( x.data, z.data, StnID, 1, "#000000" )
}
# End of function plot.prof.graph *********************************************

# Function Sigma.UML.calc *********************************************
Sigma.UML.calc <- function( Surv.data, settings.Plume ) {
  #
  #Sigma.UML <- Surv.data$Sigma
  d.rho.uml <- with( settings.Plume, Value[ which( ShortName == "plume_uml_drho" )[1] ] )
  min.layer.uml <- with( settings.Plume, Value[ which( ShortName == "plume_uml_min_layer" )[1] ] )
  #browser()
  Profile.list <- unique( Surv.data$Profile )
  Sigma.UML <- rep( NA, nrow( Surv.data ) )
  for( k.prof in 1:length(Profile.list) ) {
    indx.prof1 <- which( Surv.data$Profile == Profile.list[k.prof] )
    if( length( indx.prof1 ) > 0 ) {
      Prof <- cbind( Surv.data$Z[ indx.prof1 ], Surv.data$Sigma[ indx.prof1 ] )
      # Remove all rows with non-finite values
      Prof <- Prof[ !rowSums( !is.finite( Prof ) ), ]
      if( nrow( Prof ) > 0 ) {
        Prof <- Prof[ order( Prof[,1] ), ]
        Prof <- Prof[ !duplicated(Prof[,1] ), ]        
        Z.stn1 <- Prof[,1]
        Sigma.stn1 <- Prof[,2]
        if( max( Z.stn1 ) > min.layer.uml ) {
          Sigma.10m <- approx( Z.stn1, Sigma.stn1, min.layer.uml )$y
          if( is.finite( Sigma.10m ) ) {
            Sigma.UML[ indx.prof1 ] <- Sigma.10m + d.rho.uml
          }
        }
      }
    }
  }
  return( Sigma.UML )
}
# End of function Sigma.UML.calc *******************************************
#
# Function geodetic.distance ***********************************************
# Calculate the distance between two geographic locations 
# Call: Dist <- geodetic.distance( c(Lon1,Lat1), c(Lon2,Lat2) )
geodetic.distance <- function( point1, point2 ) { 
  R <- 6371 
  p1rad <- point1 * pi/180 
  p2rad <- point2 * pi/180 
  d <- sin( p1rad[2] ) * sin( p2rad[2] ) + 
    cos( p1rad[2] ) * cos( p2rad[2] ) * cos( abs( p1rad[1]-p2rad[1] ) )  
  d <- acos(d) 
  return( R*d )
} 
# End of function geodetic.distance ***************************************** 

# Function CDOM.plume.detect **************************************************
CDOM.plume.detect <- function( Stn.list.surv, Surv.data, settings,
                               CDOM.thrsh.pc ) {
  #
  Agency <- unique( Stn.list.surv$Agency )[1]
  if( !( Agency %in% settings$Outfall$Agency ) ) {
    return( Surv.data$Plume <- FALSE )
  }
  # Calculate "Sigma.UML" for each sample (Sigma at the upper pycnocline boundary)
  Surv.data$Sigma.UML <- Sigma.UML.calc( Surv.data, settings$Plume ) 
  outfl.depth <- with( settings$Outfall, 
                       depth[ which( settings$Outfall$Agency == Agency )[1] ] )
  # Calculate "Stn.depth" and "dist_from_outfl" for all samples
  Stn.list.nodup <- with( Stn.list.surv, data.frame( StnID = Station, 
                                                     Stn.depth = Depth, Dist.Outfl = Dist.Outfl,
                                                     Latitude = Latitude, Longitude = Longitude ) )
  Stn.list.nodup <- Stn.list.nodup[ !duplicated( Stn.list.nodup$StnID ), ]
  #browser()
  #Surv.data1 <- merge( Surv.data, Stn.list.nodup, sort = FALSE )
  Surv.data <- plyr::join( Surv.data, Stn.list.nodup, by = "StnID" )
  # Extract plume detection settings
  #   plume.depth.min <- with( Plume.settings, Value[ which( ShortName == "plume_depth_min" )[1] ] )
  #   plume.d.outfl.km <- with( Plume.settings, Value[ which( ShortName == "plume_d_outfl_km" )[1] ] ) 
  #   plume.z.over.bottom <- with( Plume.settings, Value[ which( ShortName == "plume_z_over_bottom" )[1] ] ) 
  #   plume.d.km <- with( Plume.settings, Value[ which( ShortName == "plume_d_km" )[1] ] ) 
  #   plume.d.rho <- with( Plume.settings, Value[ which( ShortName == "plume_d_rho" )[1] ] ) 
  plume.depth.min <- with( settings$Plume, Value[ which( ShortName == "plume_depth_min" )[1] ] )
  plume.d.outfl.km <- with( settings$Plume, Value[ which( ShortName == "plume_d_outfl_km" )[1] ] ) 
  plume.z.over.bottom <- with( settings$Plume, Value[ which( ShortName == "plume_z_over_bottom" )[1] ] ) 
  plume.d.km <- with( settings$Plume, Value[ which( ShortName == "plume_d_km" )[1] ] ) 
  plume.d.rho <- with( settings$Plume, Value[ which( ShortName == "plume_d_rho" )[1] ] ) 
  # CDOM threshold
  indx.CDOM <- with( Surv.data, 
                     ( Sigma > Sigma.UML ) &
                       ( Z <= outfl.depth ) &
                       ( Stn.depth > plume.depth.min ) )  
  CDOM.thresh <-  as.numeric( 
    quantile( Surv.data$CDOM[ indx.CDOM ], probs = CDOM.thrsh.pc/100, 
              na.rm = TRUE ) )
  Surv.data$plume.trace <- FALSE
  Surv.data$Plume <- FALSE
  indx.plume.1 <- with( Surv.data, 
                        ( CDOM > CDOM.thresh ) &
                          ( Sigma > Sigma.UML ) &
                          ( Z <= outfl.depth ) &
                          ( Stn.depth > plume.depth.min ) )  
  Surv.data$plume.trace[ indx.plume.1 ] <- TRUE
  #browser()
  # Select "plume core" samples
  indx.plume.core <- with( Surv.data, 
                           ( plume.trace ) &
                             ( Dist.Outfl < plume.d.outfl.km ) ) 
  Surv.data$Plume[ indx.plume.core ] <- TRUE
  Surv.data$plume.trace[ indx.plume.core ] <- FALSE
  n.plume.samples <- sum( Surv.data$Plume )
  if( n.plume.samples > 0 ) {
    plume_increased <- TRUE
    while( plume_increased ) {
      plume_increased <- FALSE
      indx.plume2add <- which( with( Surv.data, 
                                     ( !Plume ) &
                                       ( plume.trace ) &
                                       ( Z <= Stn.depth - plume.z.over.bottom ) ) ) 
      indx.cont.plume <- which( Surv.data$Plume )
      n.plume2add <- length( indx.plume2add )
      if( n.plume2add > 0 ) {
        n.cont.plume <- length( indx.cont.plume )
        dist.btw.plumes <- matrix( data = NA, ncol = n.cont.plume, 
                                   nrow = n.plume2add )
        Lat.Lon.cont.plume <- 
          as.matrix( Surv.data[ indx.cont.plume, c("Longitude","Latitude")] )
        Lat.Lon.plume2add <- 
          as.matrix( Surv.data[ indx.plume2add, c("Longitude","Latitude")] )
        for( k.plume2add in 1:n.plume2add ) {
          dist.btw.plumes[ k.plume2add, ] <- 
            apply( Lat.Lon.cont.plume, 1, geodetic.distance, 
                   Lat.Lon.plume2add[ k.plume2add, ] )
        }
        sigma.cont.plume <- matrix( 
          data = rep( Surv.data$Sigma[ indx.cont.plume ], each = n.plume2add ),
          ncol = n.cont.plume, nrow = n.plume2add )
        sigma.plume2add <- matrix( 
          data = rep( Surv.data$Sigma[ indx.plume2add ], times = n.cont.plume ),
          ncol = n.cont.plume, nrow = n.plume2add )
        sigma.btw.plumes <- abs( sigma.cont.plume - sigma.plume2add )
        indx.plume.added.matrix <- ( ( dist.btw.plumes < plume.d.km ) &
                                       ( sigma.btw.plumes < plume.d.rho ) )
        
        if( any( indx.plume.added.matrix ) ) {
          indx.plume.added <- ( rowSums( indx.plume.added.matrix ) > 0 )
          Surv.data$Plume[ indx.plume2add ] <- TRUE
          Surv.data$plume.trace[ indx.plume2add ] <- FALSE
          plume_increased <- TRUE
        }
      }
    }
  }
  return( Surv.data$Plume )
}
# End of function CDOM.plume.detect *******************************************


# Function Ref.stns.select **************************************************
Ref.stns.select <- function( Stn.list.surv, Surv.data, settings ) {
  #
  #browser()
  Agency <- unique( Stn.list.surv$Agency )[1]
  if( !( Agency %in% settings$Outfall$Agency ) ) {
    return( Surv.data$Plume <- FALSE )
  }
  # Remove stations with "RefPlume"
  Stn.list.surv$Ref = TRUE
  Stn.list.surv$Ref[ Stn.list.surv$RefPlume ] <- FALSE
  #   Remove stations with no CDOM
  StnID.CDOM <- unique( Surv.data$StnID[ is.finite( Surv.data$CDOM ) ] )
  Stn.list.surv$Ref[ !(Stn.list.surv$Station %in% StnID.CDOM) ] <- FALSE
  # Remove stations which do not fit the "plume" depth limit
  plume.depth.min <- with( settings$Plume, Value[ which( ShortName == "plume_depth_min" )[1] ] )
  Stn.list.surv$Ref[ Stn.list.surv$Depth <= plume.depth.min ] <- FALSE
  # Remove stations with UML by the bottom
  Stn.list.surv$UML.Z <- UML.Z.calc( Stn.list.surv$Profile, Surv.data, 
                                     settings$Plume )
  Stn.list.surv$Ref[ is.na( Stn.list.surv$UML.Z ) ] <- FALSE
  Stn.list.surv$Ref[ ( Stn.list.surv$Depth <= Stn.list.surv$UML.Z ) ] <- FALSE
  # 
  ref.dfo.max <- with( settings$Ref, Value[ which( ShortName == "ref_dfo_max" )[1] ] )
  ref.stn.min <- with( settings$Ref, Value[ which( ShortName == "ref_stn_min" )[1] ] )
  # Find the indices ref.stn.min reference stations most close to the outfall
  Dist.outfl.ref <- Stn.list.surv$Dist.Outfl
  Dist.outfl.ref[ !Stn.list.surv$Ref ] <- NA
  Dist.outfl.ref.rank <- rank( Dist.outfl.ref )
  if( sum( Stn.list.surv$Dist.Outfl[ Stn.list.surv$Ref ] <= ref.dfo.max ) 
      >= ref.stn.min ) {
    Stn.list.surv$Ref[ Stn.list.surv$Dist.Outfl > ref.dfo.max ] <- FALSE
  } else {
    Stn.list.surv$Ref <- FALSE
    Stn.list.surv$Ref[ Dist.outfl.ref.rank <= ref.stn.min ] <- TRUE
  }
  return( Stn.list.surv$Ref )
}
# End of function Ref.stns.select *******************************************
#
# Function UML.Z.calc *********************************************
UML.Z.calc <- function( Profile.list, Surv.data, settings.Plume ) {
  #
  d.rho.uml <- with( settings.Plume, Value[ which( ShortName == "plume_uml_drho" )[1] ] )
  min.layer.uml <- with( settings.Plume, Value[ which( ShortName == "plume_uml_min_layer" )[1] ] )
  #browser()
  Z.UML <- rep( NA, length( Profile.list ) )
  for( k.prof in 1:length( Profile.list ) ) {
    indx.prof1 <- which( Surv.data$Profile == Profile.list[k.prof] )
    if( length( indx.prof1 ) > 0 ) {
      Prof <- cbind( Surv.data$Z[ indx.prof1 ], Surv.data$Sigma[ indx.prof1 ] )
      # Remove all rows with non-finite values
      Prof <- Prof[ !rowSums( !is.finite( Prof ) ), ]
      if( nrow( Prof ) > 0 ) {
        Prof <- Prof[ order( Prof[,1] ), ]
        Prof <- Prof[ !duplicated(Prof[,1] ), ]        
        Z.stn1 <- Prof[,1]
        Sigma.stn1 <- Prof[,2]
        #
        Sigma.10m <- approx( Z.stn1, Sigma.stn1, min.layer.uml )$y
        if ( is.finite( Sigma.10m ) ) {
          Sigma.UML <- Sigma.10m + d.rho.uml
          indx.fin <- is.finite( Sigma.stn1 )
          Z.UML[ k.prof ] <- approx( 
            Sigma.stn1[ indx.fin ], Z.stn1[ indx.fin ], Sigma.UML )$y
        } 
      }
    }
  }
  return( Z.UML )
}
# End of function UML.Z.calc ***********************************************
#
# Function Ref.Prof.calc ***************************************************
Ref.Prof.calc <- function( Outrange.Param, Surv.data, Stn.list.surv, 
                           settings ) {
  #
  #browser()
  Surv.data$Sigma.UML <- Sigma.UML.calc( Surv.data, settings$Plume )
  indx.bUML <- with( Surv.data, ( Sigma > Sigma.UML ) )
  Ref.Profile.list <- Stn.list.surv$Profile[ Stn.list.surv$Ref ]
  indx.ref <- ( Surv.data$Profile %in% Ref.Profile.list )
  if( sum( indx.bUML & indx.ref, na.rm = TRUE ) < 2 ) {
    RefProf <- NULL
    return( RefProf )
  }
  #
  Ref.Prof.dRho <- as.numeric( with( settings$Outr, Value[ which( ShortName == "outr_refprof_dRho" )[1] ] ) )
  Sigma.min <- floor( min( Surv.data$Sigma[ indx.bUML & indx.ref ],
                           na.rm=TRUE) / Ref.Prof.dRho ) * Ref.Prof.dRho  
  Sigma.max <- ceiling( max( Surv.data$Sigma[ indx.bUML & indx.ref ],
                             na.rm=TRUE) / Ref.Prof.dRho ) * Ref.Prof.dRho  
  #
  RefProf <- data.frame( Sigma = seq( from = Sigma.min, to = Sigma.max, by = Ref.Prof.dRho ), 
                         Z = NA, V.mean = NA, V.std = NA, TdegC = NA )  
  length.RefProf <- nrow( RefProf )
  n.Ref.Prof <- sum( Stn.list.surv$Ref )
  V.ref <- matrix( NA, nrow = length.RefProf, ncol = n.Ref.Prof )
  Z.ref <- matrix( NA, nrow = length.RefProf, ncol = n.Ref.Prof )
  TdegC.ref <- matrix( NA, nrow = length.RefProf, ncol = n.Ref.Prof )
  #
  for( k.Ref.Prof in 1:n.Ref.Prof ) {
    indx.prof1 <- ( Surv.data$Profile == Ref.Profile.list[ k.Ref.Prof ] )
    Prof.1 <- as.matrix( Surv.data[ indx.prof1, c( "Sigma","Z",Outrange.Param,"TdegC") ] )
    Prof.1 <- Prof.1[ order( Prof.1[,1] ), ]
    Prof.1 <- Prof.1[ !duplicated(Prof.1[,1] ), ]        
    indx.fin <- !apply( apply( Prof.1, 1, is.na ), 2, any )
    if( sum( indx.fin, na.rm = TRUE ) > 1 ) {
      V.ref[ , k.Ref.Prof ] <- signal::interp1( 
        Prof.1[indx.fin,"Sigma"], Prof.1[indx.fin, Outrange.Param ], 
        RefProf$Sigma, method="linear" )
      Z.ref[ , k.Ref.Prof ] <- signal::interp1( 
        Prof.1[indx.fin,"Sigma"], Prof.1[indx.fin, "Z" ], 
        RefProf$Sigma, method="linear" )
      TdegC.ref[ , k.Ref.Prof ] <- signal::interp1( 
        Prof.1[indx.fin,"Sigma"], Prof.1[indx.fin, "TdegC" ], 
        RefProf$Sigma, method="linear" )
    }
  }
  RefProf$Z <- apply( Z.ref, 1, mean, na.rm = TRUE )
  RefProf$TdegC <- apply( TdegC.ref, 1, mean, na.rm = TRUE )
  RefProf$V.mean <- apply( V.ref, 1, mean, na.rm = TRUE )
  RefProf$V.std <- apply( V.ref, 1, sd, na.rm = TRUE )
  RefProf$V.std[ is.na( RefProf$V.std ) ] <- 0
  # Filter
  RefProf.FiltW <- as.numeric( with( settings$Outr, Value[ which( ShortName == "outr_refprof_FiltW" )[1] ] ) )
  RefProf$V.mean <- filter_np( RefProf$V.mean, RefProf.FiltW, extend = TRUE )
  RefProf$V.std <- filter_np( RefProf$V.std, RefProf.FiltW, extend = TRUE )
  RefProf$Z <- filter_np( RefProf$Z, RefProf.FiltW, extend = TRUE )
  RefProf$TdegC <- filter_np( RefProf$TdegC, RefProf.FiltW, extend = TRUE )
  #
  #browser()
  return( RefProf )
}
# End of function Ref.Prof.calc ********************************************
#
# Function filter_np *******************************************************
# Filter a vector
filter_np <- function( Y, Filt_w, extend = TRUE ) {
  #
  n_obs <- length( Y )
  X <- seq( n_obs )
  indx_nan <- is.na( Y )
  if( sum( !indx_nan, na.rm = TRUE ) <= 1 ) {
    return( Y )
  } else {
    Y1 <- Y
    Y1[ indx_nan ] <- signal::interp1( X[!indx_nan], Y[!indx_nan], X[indx_nan] )
    indx_nan <- is.na( Y1 )
    if ( indx_nan[1] ) {
      indx_first <- min( which( !indx_nan ) )
      Y1[ 1 : indx_first-1 ] <- Y1[ indx_first ]
    }
    if( indx_nan[n_obs]) {
      indx_last <- max( which( !indx_nan ) )
      Y1[ seq( from = indx_last+1, to = n_obs ) ] <- Y1[ indx_last ]
    }
    #
    filt_window <- rep( 1, Filt_w ) / Filt_w
    Y3 <- c( rep( Y1[1], Filt_w ), Y1, rep( Y1[n_obs], Filt_w ) )
    Y3sm <- stats::filter( Y3, filt_window )
    Y1sm <- Y3sm[ seq( from = Filt_w+1, to = Filt_w+n_obs ) ]
    if( !extend ) Y1sm[ indx_nan ] <- NA
    return( Y1sm )
  }
}
# End of function filter_np ************************************************
#
# Function V.diff.calc *****************************************************
# Calculate the list of profiles (for each plume profile) of the 
#   "integrated" differences between the parameter and reference profile
V.diff.calc <- function( Outrange.Param, Stn.list.surv, Surv.data, RefProf,
                         settings ) {
  #
  Plume.prof.list <- Stn.list.surv$Profile[ Stn.list.surv$Plume ]
  n.plumes <- length( Plume.prof.list )
  V.diff.list <- as.list( rep( NA, n.plumes ) )
  names( V.diff.list ) <- Plume.prof.list
  if( sum( Surv.data$Plume ) > 0 ) {
    # Calculate the coefficients of entrainment
    n.poly <- 3
    TdegC.V.fit <- TdegC.V.fit.calc( Outrange.Param, Stn.list.surv, 
                                     Surv.data, n.poly, settings )
    for( k.plume in 1:n.plumes ) {
      # Calculate data frame for each plume
      V.diff.list[[ k.plume ]] <- V.diff.Entr.calc( Outrange.Param, Surv.data, 
                                                    Plume.prof.list[ k.plume ], RefProf, settings, TdegC.V.fit )
    }
  }
  return( V.diff.list )
}
# End of function V.diff.calc **********************************************
#
# Function TdegC.V.fit.calc ************************************************
TdegC.V.fit.calc <- function( Outrange.Param, Stn.list.surv, Surv.data, 
                              n.poly, settings ) {
  #
  indx.ref <- ( Surv.data$Profile %in% Stn.list.surv$Profile[Stn.list.surv$Ref] ) & 
    is.finite( Surv.data$TdegC ) & is.finite( Surv.data[ , Outrange.Param ] )
  Surv.data$Sigma.UML <- Sigma.UML.calc( Surv.data, settings$Plume )
  indx.bUML <- with( Surv.data, ( Sigma > Sigma.UML ) )
  
  TdegC.ref <- Surv.data$TdegC[ indx.ref & indx.bUML ]
  V.ref <- Surv.data[ indx.ref & indx.bUML, Outrange.Param ]
  indx.fin <- ( !is.na( TdegC.ref ) ) & ( !is.na( V.ref ) )
  V.ref <- V.ref[ indx.fin ]
  TdegC.ref <- TdegC.ref[ indx.fin ]
  TdegC.V.fit <- lm( V.ref ~ stats::poly( TdegC.ref, n.poly, raw=TRUE ) )
  #
  return( TdegC.V.fit)  
}
# End of function TdegC.V.fit.calc ********************************************
#
# Function V.diff.Entr.calc ***************************************************
V.diff.Entr.calc <- function( Outrange.Param, Surv.data, Plume.Profile, RefProf, 
                              settings, TdegC.V.fit ) {
  #
  Outr.settings <- settings$Outr
  Entr.settings <- settings$Entr
  #
  V.diff.DF <- RefProf[ , c("Sigma","TdegC") ]
  colnames( V.diff.DF)[2] <- "TdegC.ref"
  V.diff.DF$V.plume <- NA
  V.diff.DF$TdegC.plume <- NA
  V.diff.DF$Z.plume <- NA
  if( settings$Prof.comp$Value == "ttest" ) { 
    V.diff.DF$t.value <- NA
    V.diff.DF$p.value <- NA
  } else {
    V.diff.DF$V.diff <- NA
  }
  #V.diff.DF$V.diff.PC <- NA
  n.Sigma <- nrow( V.diff.DF )
  #
  Surv.data$Sigma.UML <- Sigma.UML.calc( Surv.data, settings$Plume )
  indx.bUML <- with( Surv.data, ( Sigma > Sigma.UML ) )
  RefProf.dSigma <- diff( V.diff.DF$Sigma )[1]
  indx.plume <- ( Surv.data$Profile == Plume.Profile )
  for( k.Sigma in 1:n.Sigma ) {
    indx.Sigma <- ( Surv.data$Sigma >= ( V.diff.DF$Sigma[ k.Sigma ] - RefProf.dSigma ) ) &
      ( Surv.data$Sigma <= ( V.diff.DF$Sigma[ k.Sigma ] + RefProf.dSigma ) )
    V.diff.DF$V.plume[ k.Sigma ] <- mean( Surv.data[ indx.plume & indx.Sigma & indx.bUML, Outrange.Param ], na.rm = TRUE )
    V.diff.DF$TdegC.plume[ k.Sigma ] <- mean( Surv.data$TdegC[ indx.plume & indx.Sigma & indx.bUML ], na.rm = TRUE )
    V.diff.DF$Z.plume[ k.Sigma ] <- mean( Surv.data$Z[ indx.plume & indx.Sigma & indx.bUML ], na.rm = TRUE )
  }
  #browser()
  indx.fin <- !is.na( V.diff.DF$V.plume )
  if( sum( indx.fin, na.rm = TRUE ) > 1 ) {
    V.diff.DF$V.plume <- signal::interp1( V.diff.DF$Sigma[indx.fin], 
                                          V.diff.DF$V.plume[indx.fin], V.diff.DF$Sigma, method="linear" )  
  }
  indx.fin <- !is.na( V.diff.DF$TdegC.plume )
  if( sum( indx.fin, na.rm = TRUE ) > 1 ) {
    V.diff.DF$TdegC.plume <- signal::interp1( V.diff.DF$Sigma[indx.fin], 
                                              V.diff.DF$TdegC.plume[indx.fin], V.diff.DF$Sigma, method="linear" )  
  }
  indx.fin <- !is.na( V.diff.DF$Z.plume )
  if( sum( indx.fin, na.rm = TRUE ) > 1 ) {
    V.diff.DF$Z.plume <- signal::interp1( V.diff.DF$Sigma[indx.fin], 
                                          V.diff.DF$Z.plume[indx.fin], V.diff.DF$Sigma, method="linear" )  
  }
  RefProf.FiltW <- as.numeric( with( Outr.settings, Value[ which( ShortName == "outr_refprof_FiltW" )[1] ] ) )
  V.diff.DF$V.plume <- filter_np( V.diff.DF$V.plume, RefProf.FiltW, extend = FALSE )
  V.diff.DF$TdegC.plume <- filter_np( V.diff.DF$TdegC.plume, RefProf.FiltW, extend = FALSE )
  V.diff.DF$Z.plume <- filter_np( V.diff.DF$Z.plume, RefProf.FiltW, extend = FALSE )
  #
  RefProf.Kstd <- as.numeric( with( Outr.settings, Value[ which( ShortName == "outr_refprof_Kstd" )[1] ] ) )
  V.diff.DF$V.ref <- RefProf$V.mean - RefProf$V.std * RefProf.Kstd
  #
  Entr.effect.on <- as.logical( with( Entr.settings, 
                                      Value[ which( ShortName == "entr_effectOnOff" )[1] ] ) )
  V.diff.DF$V.entr <- V.diff.DF$V.ref
  if( Entr.effect.on ) {
    indx.fin <- is.finite( V.diff.DF$TdegC.plume )
    V.diff.DF$V.entr[ indx.fin ] <- as.numeric( predict( TdegC.V.fit, 
                                                         data.frame( TdegC.ref = V.diff.DF$TdegC.plume[ indx.fin ] ) ) )
  }
  V.diff.DF$V.entr <- pmin( V.diff.DF$V.entr, V.diff.DF$V.ref ) 
  #
  RefProf.Z.window <- as.numeric( with( Outr.settings, 
                                        Value[ which( ShortName == "outr_refprof_Zwindow" )[1] ] ) )
  #browser()
  for( k.Sigma in 1:n.Sigma ) {
    if ( is.finite( V.diff.DF$V.plume[ k.Sigma ] ) ) {
      indx.layer <- ( abs( V.diff.DF$Z.plume - V.diff.DF$Z.plume[ k.Sigma ] ) <=
                        RefProf.Z.window/2 )
      indx.layer[ is.na( indx.layer ) ] <- FALSE
      if( settings$Prof.comp$Value == "ttest" ) {
        if( ( sum( is.finite( V.diff.DF$V.entr[ indx.layer ] ) ) > 1 ) & 
            ( sum( is.finite( V.diff.DF$V.entr[ indx.layer ] ) ) > 1 ) ) {
          t.test.res <- t.test( V.diff.DF$V.plume[ indx.layer ], 
                                V.diff.DF$V.entr[ indx.layer ] )
          V.diff.DF$t.value[ k.Sigma ] <- t.test.res$statistic
          V.diff.DF$p.value[ k.Sigma ] <- t.test.res$p.value
        } else {
          V.diff.DF$t.value[ k.Sigma ] <- NA
          V.diff.DF$p.value[ k.Sigma ] <- NA
        }
      } else {
        if ( sum( indx.layer, na.rm = TRUE ) > 1 ) {
          V.plume.trp <- pracma::trapz( V.diff.DF$Sigma[ indx.layer ], V.diff.DF$V.plume[ indx.layer ] )
          V.ref.trp <- pracma::trapz( V.diff.DF$Sigma[ indx.layer ], V.diff.DF$V.entr[ indx.layer ] )
        } else {
          V.plume.trp <- V.diff.DF$V.plume[ indx.layer ]
          V.ref.trp <- V.diff.DF$V.entr[ indx.layer ]        
        }
        if( settings$Prof.comp$Value == "percent" ) {
          V.diff.DF$V.diff[ k.Sigma ] <- 100 * ( V.plume.trp - V.ref.trp ) / V.ref.trp
        } else if ( settings$Prof.comp$Value == "absolute" ) {
          V.diff.DF$V.diff[ k.Sigma ] <- V.plume.trp - V.ref.trp
        } else {
          V.diff.DF$V.diff[ k.Sigma ] <- NA
        }
        V.diff.DF$V.diff <- filter_np( V.diff.DF$V.diff, RefProf.FiltW, 
                                       extend = FALSE )
      }
    }
  }
  #
  return( V.diff.DF )
}
# End of function V.diff.Entr.calc ********************************************

# Function V.Z.min.calc *******************************************************
V.Z.min.calc <- function( V.diff.list, Prof.comp ) {
  #
  n.plume <- length( V.diff.list )
  if( Prof.comp == "ttest" ) {
    V.Z.min <- data.frame( t.min = rep( NA, n.plume ), 
                           p.min = rep( NA, n.plume ), 
                           Z.min = rep( NA, n.plume ) )
  } else {
    V.Z.min <- data.frame( V.min = rep( NA, n.plume ), 
                           Z.min = rep( NA, n.plume ) )
  }
  rownames( V.Z.min ) <- names( V.diff.list )
  if( n.plume > 0 ) {
    #browser()
    for( k.plume in 1:n.plume ) {
      if( Prof.comp == "ttest" ) {
        indx.min <- which.min( V.diff.list[[k.plume]]$t.value )
        if( length( indx.min ) > 0 ) {
          V.Z.min$t.min[ k.plume ] <- V.diff.list[[k.plume]]$t.value[ indx.min[1] ]
          V.Z.min$p.min[ k.plume ] <- V.diff.list[[k.plume]]$p.value[ indx.min[1] ]
          V.Z.min$Z.min[ k.plume ] <- V.diff.list[[k.plume]]$Z.plume[ indx.min[1] ]
        }
      } else {
        indx.min <- which.min( V.diff.list[[k.plume]]$V.diff )
        if( length( indx.min ) > 0 ) {
          V.Z.min$V.min[ k.plume ] <- V.diff.list[[k.plume]]$V.diff[ indx.min[1] ]
          V.Z.min$Z.min[ k.plume ] <- V.diff.list[[k.plume]]$Z.plume[ indx.min[1] ]
        }
      }
    }
  }
  V.Z.min$Z.min <- round( V.Z.min$Z.min, digits = 1 )
  if( Prof.comp == "ttest" ) {
    V.Z.min$t.min <- round( V.Z.min$t.min, digits = 3 )
    V.Z.min$p.min <- round( V.Z.min$p.min, digits = 5 )
  } else {
    V.Z.min$V.min <- round( V.Z.min$V.min, digits = 3 )
  }
  return( V.Z.min )
}
# End of function V.Z.min.calc ************************************************

# Function outr.detect ********************************************************
outr.detect <- function( V.Z.min, settings, Prof.comp ) {
  #
  Outr.threshold <- as.numeric( with( settings$Outr, 
                                      Value[ which( ShortName == "outr_threshold" )[1] ] ) )
  if( Prof.comp == "ttest" ) { 
    Outr.prof.list <- rownames( V.Z.min[ V.Z.min$p.min < Outr.threshold, ] )
  } else {
    Outr.prof.list <- rownames( V.Z.min[ V.Z.min$V.min < Outr.threshold, ] )
  }
  #
  return( Outr.prof.list )
}
# End of function outr.detect ***************************************************

# Function plot.outr.profiles ***************************************************
plot.outr.profiles <- function( V.diff.list, Z.axis, Prof.comp, Outrange.Param,
                                Profiles.selected, Outr.settings ) {
  if( Prof.comp == "ttest" )  {
    x.lab <- "t-coefficient"
    X.param <- "t.value"
  } else {
    x.lab <- paste( Outrange.Param, "(plume minus reference)" )
    if( Prof.comp == "Percent" ) {
      x.lab <- paste( x.lab, "(%)" )
    }
    X.param <- "V.diff"
  }
  n.profiles <- length( V.diff.list )
  indx.fin <- is.finite( V.diff.list[[1]][,X.param] )
  x.range <- extendrange( V.diff.list[[1]][indx.fin,X.param] )  
  y.range <- extendrange( V.diff.list[[1]][indx.fin,Z.axis] ) 
  if( n.profiles > 1 ) {
    for( k.profile in 2:n.profiles ) {
      indx.fin <- is.finite( V.diff.list[[k.profile]][,X.param] )
      x.range.2 <- extendrange( V.diff.list[[k.profile]][indx.fin,X.param] )  
      x.range[1] <- min( x.range[1], x.range.2 )
      x.range[2] <- max( x.range[2], x.range.2 )
      y.range.2 <- extendrange( V.diff.list[[k.profile]][indx.fin,Z.axis] )  
      y.range[1] <- min( y.range[1], y.range.2 )
      y.range[2] <- max( y.range[2], y.range.2 )
    }
  }
  
  #browser()
  
  if( Z.axis == "Z.plume" ) {
    y.label = "Depth (m)"
  } else {
    y.label = "Specific Density (kg/m3)"
  }
  plot( V.diff.list[[1]][,X.param], V.diff.list[[1]][,Z.axis], 
        xlim = x.range, ylim = rev( y.range ), type = "n",
        ylab = y.label, xlab = x.lab )
  abline( v = 0 )
  Outr.threshold <- with( Outr.settings, 
                          Value[ which( ShortName == "outr_threshold" )[1] ] )
  abline( v = Outr.threshold, lwd = 3, lty = 3, col = "red" )
  for( k.profile in 1:n.profiles ) {
    lines( V.diff.list[[k.profile]][,X.param], 
           V.diff.list[[k.profile]][,Z.axis], lwd = 2, col = "gray" )
  }
  for( k.profile in 1:n.profiles ) {
    if( names( V.diff.list )[ k.profile ] %in% Profiles.selected ) {
      lines( V.diff.list[[k.profile]][,X.param], 
             V.diff.list[[k.profile]][,Z.axis], lwd = 4, col = "red" )
    }
  }
}
# End of function plot.outr.profiles ********************************************

# Function entr.plot ************************************************************
entr.plot <- function( V.diff.DF, Y.name, Outrange.Param ) {
  indx.fin <- is.finite( V.diff.DF[ , Y.name ] ) & is.finite( V.diff.DF$V.plume ) &
    is.finite( V.diff.DF$V.entr ) & is.finite( V.diff.DF$V.ref )
  y.range <- extendrange( V.diff.DF[ indx.fin, Y.name ] )
  y.range[2] <- y.range[2] + diff( y.range ) /5
  x.range.plume <- extendrange( V.diff.DF[ indx.fin, "V.plume" ] )
  x.range.entr <- extendrange( V.diff.DF[ indx.fin, "V.entr" ] )
  x.range.ref <- extendrange( V.diff.DF[ indx.fin, "V.ref" ] )
  x.range <- c( min( x.range.plume[1], x.range.entr[1], x.range.ref[1] ),
                max( x.range.plume[2], x.range.entr[2], x.range.ref[2] ) )
  #  
  if( Y.name == "Sigma" ) {
    y.lab <- "Specific density (kg/m3)"
  } else {
    y.lab <- "Depth (m)"
  }
  plot( V.diff.DF$V.plume, V.diff.DF[ , Y.name ], type = "n",
        ylim = rev( y.range ), xlim = x.range, ylab = y.lab, xlab = Outrange.Param )
  lines( V.diff.DF$V.plume, V.diff.DF[ , Y.name ], col = "red", lwd = 3 )
  lines( V.diff.DF$V.entr, V.diff.DF[ , Y.name ], col = "green", lwd = 3 )
  lines( V.diff.DF$V.ref, V.diff.DF[ , Y.name ], col = "blue", lwd = 3 )
  legend( "bottomright", col = c( "red","blue","green" ), lwd = 3,
          legend = c("Plume","Reference","Corrected for entrainment") )
}
# End of function entr.plot *****************************************************

# Function report.data.file ************************************************
report.data.file <- function( selected.data.file, Surv.data.tot ) {
  #
  if( is.null( selected.data.file ) ) {
    report_data_file <- NULL
  } else {
    report_data_file <- paste( "Data file:    ", selected.data.file, ", ", 
                               nrow( Surv.data.tot ), " obs.", sep = "" )
  }
  return( report_data_file )
}
# End of function report.data.file *****************************************


# Function report.data.selected ********************************************
report.data.selected <- function( Agency.selected, Year.selected, Season.selected,
                                  Surv.data ) {
  #
  if( is.null( Agency.selected ) & is.null( Year.selected ) & 
      is.null( Season.selected ) ) {
    report_data_selected <- NULL
  } else {
    if( is.null( Surv.data ) ) { n.surv.data <- 0
    } else { n.surv.data <- nrow( Surv.data ) }
    report_data_selected <- paste( "Selected:   ", Agency.selected,  ", ", 
                                   Year.selected,  ", ", Season.selected,  ", ", n.surv.data, " obs.",
                                   sep = "" )
  }
  
}
# End of function report.data.selected *************************************

# Function report.plume.list ********************************************
report.plume.list <- function( Stn.list.surv.Plume, Stn.list.surv.Profile ) {
  #
  if( sum( Stn.list.surv.Plume ) == 0 ) {
    report_plume_list <- NULL
  } else {
    report_plume_list <- paste( "CDOM plume: ", 
                                sum( Stn.list.surv.Plume ), " profiles (",
                                paste( Stn.list.surv.Profile[ Stn.list.surv.Plume ], 
                                       collapse = ", " ), ")", sep = "" )
  }
  #
  return( report_plume_list )
}
# End of function report.plume.list *************************************

# Function report.plume.settings ********************************************
report.plume.settings <- function( Stn.list.surv.Plume, settings.Plume ) {
  #
  if( sum( Stn.list.surv.Plume ) == 0 ) {
    report_plume_settings <- NULL
  } else {
    report_plume_settings <- paste( 
      paste( "        ", settings.Plume$Comment, " = ", settings.Plume$Value, sep = "" ), 
      collapse = NULL )
    report_plume_settings <- c( "        Plume detection settings:", report_plume_settings )
  }
  #
  return( report_plume_settings )
}
# End of function report.plume.settings *************************************

# Function report.ref.list ********************************************
report.ref.list <- function( Stn.list.surv.Ref, Stn.list.surv.Profile ) {
  #
  if( sum( Stn.list.surv.Ref ) == 0 ) {
    report_ref_list <- NULL
  } else {
    report_ref_list <- paste( "Reference: ", 
                              sum( Stn.list.surv.Ref ), " profiles (",
                              paste( Stn.list.surv.Profile[ Stn.list.surv.Ref ], 
                                     collapse = ", " ), ")", sep = "" )
  }
  #
  return( report_ref_list )
}
# End of function report.ref.list *************************************

# Function report.ref.settings ********************************************
report.ref.settings <- function( Stn.list.surv.Ref, settings.Ref ) {
  #
  if( sum( Stn.list.surv.Ref ) == 0 ) {
    report_ref_settings <- NULL
  } else {
    report_ref_settings <- paste( 
      paste( "        ", settings.Ref$Comment, " = ", settings.Ref$Value, sep = "" ), 
      collapse = NULL )
    report_ref_settings <- c( "        Reference profiles selection settings:", 
                              report_ref_settings )
  }
  #
  return( report_ref_settings )
}
# End of function report.ref.settings *************************************

# Function report.outr.param **********************************************
report.outr.param <- function( Outrange.Param ) {
  #
  if( is.null( Outrange.Param ) ) {
    report_outr_param <- NULL
  } else {
    report_outr_param <- paste( "Parameter selected for outranges detection: ",
                                Outrange.Param )   
  }
  return( report_outr_param )
}
# End of function report.outr.param ***************************************

# Function report.ref.prof **********************************************
report.ref.prof <- function( Outrange.Param, RefProf ) {
  #
  if( is.null( RefProf ) ) {
    report_ref_prof <- NULL
  } else {
    report_ref_prof <- paste( "Reference", Outrange.Param, "profile calculated" )
  }
  return( report_ref_prof )
}
# End of function report.ref.prof ***************************************

# Function report.outr.method **********************************************
report.outr.method <- function( RefProf, Prof.comp ) {
  if( is.null( RefProf ) | is.null( Prof.comp ) ) {
    report_outr_method <- NULL    
  } else {
    report_outr_method <- paste( "        Method of comparison between profiles:",
                                 Prof.comp )
  }
  return( report_outr_method )
}
# End of function report.outr.method ***************************************

# Function report.outr.settings **********************************************
report.outr.settings <- function( RefProf, settings.Outr ) {
  #
  if( is.null( RefProf ) ) {
    report_outr_settings <- NULL
  } else {
    report_outr_settings <- paste( 
      paste( "        ", settings.Outr$Comment, " = ", settings.Outr$Value, sep = "" ), 
      collapse = NULL )
    report_outr_settings <- c( 
      "        Reference profile calculation and outrange detection settings:", 
      report_outr_settings )
  }
  return( report_outr_settings )
}
# End of function report.outr.settings ***************************************

# Function report.outr.settings **********************************************
report.entr.setting <- function( RefProf, settings.Entr ) {
  #
  if( is.null( RefProf ) ) {
    report_entr_setting <- NULL
  } else {
    report_entr_setting <- paste( "        ", settings.Entr$Comment, ": ", 
                                  settings.Entr$Value,
                                  sep = "" )
  }
  return( report_entr_setting )
}
# End of function report.outr.method ***************************************

# Function report.outr.list **********************************************
report.outr.list <- function( Outrange.Param, V.diff.list, Stn.list.surv ) {
  #
  if( is.null( V.diff.list ) ) {
    report_outr_list <- NULL
  } else {
    report_outr_list <- paste( Outrange.Param, " outranges detected: ",
                               sum( Stn.list.surv$Outrange ), sep = "" )
    if( sum( Stn.list.surv$Outrange ) > 0 ) {
      report_outr_list <- paste( report_outr_list, " (",
                                 paste( Stn.list.surv$Profile[ Stn.list.surv$Outrange], collapse = ", ", sep = "" ),
                                 ")", sep = "" )
    }
  }
  return( report_outr_list )
}
# End of function report.outr.list ***************************************

# Function report.outr.list **********************************************
report.max.decrease.depths <- function( Outrange.Param, Prof.comp, V.Z.min ) {
  #
  if( is.null( V.Z.min ) ) {
    report_max_decrease_depths <- NULL
  } else {
    if( Prof.comp == "ttest" ) {
      report_max_decrease_depths <- paste( rownames( V.Z.min ), 
                                           V.Z.min$t.min, V.Z.min$p.min, V.Z.min$Z.min, sep = ", ", collapse = NULL )
      report_max_decrease_depths <- c( 
        paste( "Maximum", Outrange.Param, "decrease at depths (Profile, t, p, Depth):" ),
        report_max_decrease_depths )
    } else {
      report_max_decrease_depths <- paste( rownames( V.Z.min ), 
                                           V.Z.min$V.min, V.Z.min$Z.min, sep = ", ", collapse = NULL )
      report_max_decrease_depths <- c( 
        paste( "Maximum", Outrange.Param, "decrease at depths (Profile,Decrease,Depth):" ),
        report_max_decrease_depths ) 
    }            
  }
  return( report_max_decrease_depths )
}
# End of function report.outr.method ***************************************



