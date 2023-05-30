
#code credit: https://github.com/MikkoVihtakari/PlotSvalbard/blob/master/R/section_plot.R


interpolate_section <- function(df = NULL, x, y, z, method = "linear", format = "ggplot", nx = 100, ny = 100, scale_distances = FALSE, log_y = FALSE, extrapolate = TRUE, na.rm = TRUE, return_df_names = FALSE) {
  
  ## Tests ####
  
  if(is.null(df)) {
    if(!all(diff(c(length(x), length(y) ,length(z))) == 0)) stop("x, y, and z have to have equal length")
  } else {
    if(class(df) != "data.frame") stop("df has to be a data.frame")
  }
  
  ## Data ###
  
  if(is.null(df)) {
    dt <- data.frame(x = x, y = y, z = z)
  } else {
    dt <- df[c(x, y, z)]
    org_names <- names(dt)
    names(dt) <- c("x", "y", "z")
  }
  
  ## Remove NAs ##
  
  if(na.rm & any(is.na(dt))) dt <- stats::na.omit(dt)
  
  ## Scale & log transform ##
  
  if(scale_distances) {
    dt$x <- scales::rescale(dt$x)
    if(log_y) {
      dt$y <- scales::rescale(log(dt$y))
    } else {
      dt$y <- scales::rescale(dt$y)
    }
  } else if(log_y) {
    dt$y <- log10(dt$y+10)
  }
  
  
  ## Interpolation
  
  if(method == "linear") {
    
    out <- akima::interp(x = dt$x, y = dt$y, z = dt$z , xo = seq((min(dt$x)-5), (max(dt$x)+5), length = nx), yo = seq(min(dt$y), max(dt$y), length = 100), extrap = extrapolate, duplicate = "strip")
    
  } else if(method == "spline") {
    
    out <- akima::interp(x = dt$x, y = dt$y, z = dt$z , xo = seq(min(dt$x), max(dt$x), length = nx), yo = seq(min(dt$y), max(dt$y), length = 100), extrap = extrapolate, linear = FALSE)
    
  } else if(method == "idw") {
    
    sp::coordinates(dt) <- ~x+y
    
    grd <- expand.grid(x = seq(from = min(dt$x), to = max(dt$x), length.out = nx), y = seq(from = min(dt$y), to = max(dt$y), length.out = ny))  # expand points to grid
    
    sp::coordinates(grd) <- ~x+y
    sp::gridded(grd) <- TRUE
    
    intp <- gstat::idw(formula = z ~ 1, locations = dt, newdata = grd)
    
    intp_df <- as.data.frame(intp)
    
    out <- list(x = intp_df$x, y = intp_df$y, z = intp_df$var1.pred)
    
  } else if(method == "mba") {
    
    if(nx != ny) stop("The mba.surf function appears to manage only with square output matrices. Adjust the arguments such that nx == ny")
    
    out <- MBA::mba.surf(dt, no.X = nx, no.Y = ny, extend = extrapolate)
    
  } else {
    
    stop(paste(method, "is not an accepted interpolation method"))
  }
  
  ## Output format
  
  if(format == "ggplot") {
    
    if(method %in% c("linear", "spline")) {
      
      out_df <- reshape2::melt(out$z, na.rm = TRUE)
      names(out_df) <- c("x", "y", "z")
      out_df$x <- out$x[out_df$x]
      out_df$y <- out$y[out_df$y]
      
      if(!is.null(df) & return_df_names) {
        names(out_df) <- org_names
      }
      
      out_df
      
    } else if (method == "mba") {
      
      #colnames(out$xyz.est$z) <- out$xyz.est$x
      #rownames(out$xyz.est$z) <- out$xyz.est$y
      dimnames(out$xyz.est$z) <- list(out$xyz.est$x, out$xyz.est$y)
      
      out_df <- reshape2::melt(out$xyz.est$z, varnames = c("x", "y"), value.name = "z")
      #out_df <- out_df[c("x", "y", "z")]
      
      if(!is.null(df) & return_df_names) {
        names(out_df) <- org_names
      }
      
      out_df
      
    } else {
      
      data.frame(x = out$x, y = out$y, z = out$z)
      
    }
    
  } else if(format == "wide") {
    tmp <- data.frame(x = out$x, y = out$y, z = out$z)
    tmp <- reshape2::dcast(tmp, y ~ x, value.var = "z")
    ynams <- tmp[[1]]
    xnams <- as.numeric(names(tmp)[-1])
    tmp <- as.matrix(unname(tmp[-1]))
    colnames(tmp) <- xnams
    rownames(tmp) <- ynams
  } else {
    out
  }
}


section_plot <- function(df, x, y, z, bottom = NULL, MLD = NULL, interpolate = FALSE, interp_method = "mba", log_y = FALSE, xlab = "Distance", ylab = "Depth", zlab = "Variable", ybreaks = waiver(), xbreaks = waiver(), zbreaks = waiver(), contour = NULL, contour_label_cex = 0.8, contour_color = "white", xlim = NULL, ylim = NULL, zlim = NULL, zscale = "viridis", zcolor = "black", add_bottom = NULL, sampling_indicator = "lines", legend.position = "right", base_size = 10, ...) {
  
  ## Setup and tests
  
  
  ## Log_y
  
  #if(log_y) df[[y]] <- log10(df[[y]] + 10)
  
  ## Bottom ###
  
  if(!is.null(bottom)) {
    
    if(!any(class(bottom) %in% c("data.frame", "character"))) stop("bottom argument has to be a character or data.frame. See Arguments.")
    
    if(any(class(bottom) %in% "data.frame")) {
      if(ncol(bottom) < 2) stop("bottom data.frame has to contain at least two columns. See Arguments.")
      bd <- unique(bottom[1:2])
    } else {
      bd <- unique(df[c(x, bottom)])
    }
    
    names(bd) <- c("x", "y")
    if(!is.null(add_bottom)) bd <- rbind(bd, data.frame(x = range(xbreaks), y = add_bottom))
    bd <- bd[order(bd$x),]
    
    if(log_y) bd$y <- log10(bd$y + 10)
    
  }
  
  
  ## Interpolation
  if(interpolate) {
    dt <- interpolate_section(df = df, x = x, y = y, z = z, method = interp_method)
    
    samples <- df %>% group_by(get(x)) %>% dplyr::summarise(min = min(get(y)), max = max(get(y)))
    names(samples) <- c("x", "min", "max")
    
    if(!is.null(MLD)) {
      #add mixed layer
      dd <- df[,c("latitude", MLD)]
      dd$x <- dd$latitude
      samplesA <- merge(samples, unique(dd))
      } else {
      samplesA <- samples
    }

  } else {
    
    ## Bubbles
    
    dt <- df[c(x, y, z)]
    names(dt) <- c("x", "y", "z")
  }
  
  ## Parameters ###
  
  #if(log_y) {
  #  yzero <- 0.98
  #  ytick.lim <- 0.95
    
  #  if(class(ybreaks) == "waiver") {
  #    ybreaks <- pretty_log(10^pretty(range(dt$y), n = 10) - 10)
  #    ybreaks_actual <- log10(ybreaks + 10)
  #  } else {
  #    ybreaks_actual <- log10(ybreaks + 10)
  #  }
    
    
    
  #} else {
    yzero <- -2
    ytick.lim <- -diff(range(df[[y]]))*0.02
    
    if(class(ybreaks) == "waiver") {
      ybreaks_actual <- waiver()
    } else {
      ybreaks_actual <- ybreaks
    }
 # }
  
  # Add ≥ and ≤ signs to legend
  
  if(!is.null(zlim)) {
    
    if(class(zbreaks) == "waiver") {
      zbreaks <- pretty(zlim, n = 4)
    }
    
    zlabels <- zbreaks
    
    if(any(df[[z]] > zlim[2])) {
      zlabels[length(zlabels)] <- paste0(">", zlabels[length(zlabels)])
    }
    
    if(any(df[[z]] < zlim[1])) {
      zlabels[1] <- paste0("<", zlabels[1])
    }
    
  } else {
    if(class(zbreaks) == "waiver") {
      zlabels <- waiver()
    } else {
      zlabels <- zbreaks
    }
    
    
  }
  
  
  ## Plot ####
  
  if(interpolate) {
    
    p <- ggplot() +
      geom_tile(data = dt, aes(x = x, y = y, fill = z, color = z)) + {
        if(!is.null(contour)) geom_contour(data = dt, aes(x = x, y = y, z = z), color = contour_color, size = 0.5, breaks = contour)
      } + {
        if(!is.null(contour)) {
          directlabels::geom_dl(data = dt, aes(x = x, y = y, z = z, label = ..level..),
                                method = list("bottom.pieces", cex = contour_label_cex, vjust = 0.5),
                                stat = "contour", color = contour_color, breaks = contour)
        }
      } + {
        if(sampling_indicator == "lines") geom_segment(data = samplesA, aes(x = x, xend = x, y = min, yend = max), size = 0.5, color = "grey", linetype = 2)
      } + {
        if(sampling_indicator == "points") geom_point(data = df, aes(x = get(x), y = get(y)), size = contour_label_cex, color = "black")
      } + {
        if(sampling_indicator == "ticks") geom_segment(data = samples, aes(x = x, xend = x, y = ytick.lim, yend = yzero), size = LS(1), color = "black")
      } + {
        if(!is.null(bottom)) geom_ribbon(data = bd, aes(x = x, ymax = Inf, ymin = y), fill = "grey90")
      } + {
    if(!is.null(MLD)) geom_point(data = samplesA, aes(x= x, y=mld), fill = "red")
  } +
      scale_y_reverse(name = ylab, breaks = ybreaks_actual, labels = ybreaks, limits = ylim, expand = c(0.03, 0)) +
      scale_x_continuous(name = xlab, breaks = xbreaks, limits = xlim, expand = c(0, 0)) +
      theme_classic(base_size = base_size) +
      theme(legend.position = legend.position,
            legend.key.size = unit(0.8,"line"),
            legend.spacing.y = unit(0.1,"line"),
            legend.title = element_text(size = 0.8*base_size),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            legend.box.background = element_blank())
    
    ## Color scales
    
    if(zscale == "gradient2") {
      
      p + scale_fill_gradient2(name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...) +
        scale_colour_gradient2(name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...)
      
    } else {
      
      p + scale_fill_viridis_c(option = zscale, name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...) +
        scale_colour_viridis_c(option = zscale, name = zlab, na.value = "white", limits = zlim, breaks = zbreaks, labels = zlabels, oob = scales::squish, ...)
      
    }
    
    
  } else {
    
    ggplot() + #{
      #if(!is.null(bottom)) geom_ribbon(data = bd, aes(x = x, ymax = Inf, ymin = y), fill = "grey90")
    #} +
      geom_point(data = dt, aes(x = x, y = y, size = z), pch = 21, stroke = 0.5, color = zcolor) +
      scale_size_area(name = zlab, breaks = zbreaks, labels = zlabels, oob = scales::squish) + #limits = zlim,
      scale_y_reverse(name = ylab, breaks = ybreaks_actual, labels = ybreaks, limits = ylim, expand = c(0.03, 0)) +
      scale_x_continuous(name = xlab, breaks = xbreaks, expand = c(0, 0)) + #, limits = xlim
      coord_cartesian(xlim = xlim, ylim = ylim, clip = "off") +
      theme_classic(base_size = base_size) +
      theme(legend.position = legend.position,
            legend.key.size = unit(0.8,"line"),
            legend.spacing.y = unit(0.1,"line"),
            legend.title = element_text(size = 0.8*base_size),
            panel.background = element_blank(),
            plot.background = element_blank(),
            legend.background = element_blank(),
            legend.box.background = element_blank())
  }
  
  ## ####
  
}

ts_plot <- function(dt,
  temp_col,
  sal_col,
  WM,
  color_wmpoly = "grey30",
  xlim = NULL,
  ylim = NULL,
  color,
  zoom = TRUE,
  nlevels = 6,
  color_isopyc = "grey90",
  symbol_shape = 1,
  symbol_size = 3,
  symbol_alpha = 0.6,
  color_scale = NULL,
  color_var_name = NULL,
  margin_distr = FALSE,
  margin_width = 0.15,
  margin_height = 0.2,
  plot_data = TRUE,
  base_size = 10){
  
  ## Definitions ####
  
  if(color %in% colors()) {
    scale2color <- FALSE
  } else if(!is.null(color)) {
    scale2color <- TRUE
  } else {
    scale2color <- FALSE
    color <- "black"
  }
  
  ## Axis limits
  
  if(is.null(xlim) & zoom) {
    xbreaks <- pretty(range(dt[[sal_col]]), n = nlevels)
    xlim <- range(xbreaks)
    
    if(max(dt[[sal_col]]) < 35.15 & xlim[2] == 36) {
      xlim <- c(xlim[1], 35.15)
    }
    
  } else if(is.null(xlim)) {
    xbreaks <- pretty(range(c(dt[[sal_col]], c(32, 35))), n = nlevels)
    xlim <- range(xbreaks)
    
    if(max(dt[[sal_col]]) < 35.15 & xlim[2] == 36) {
      xlim <- c(xlim[1], 35.15)
    }
    
  } else {
    xbreaks <- pretty(xlim)
  }
  
  if(is.null(ylim) & zoom) {
    ybreaks <- pretty(range(dt[[temp_col]]), n = nlevels)
    ylim <- range(ybreaks)
  } else if(is.null(ylim)) {
    ybreaks <- pretty(range(c(dt[[temp_col]], c(-2, 8))), n = nlevels)
    ylim <- range(ybreaks)
  } else {
    ybreaks <- pretty(ylim)
  }
  
  ## Isopycnals
  
  if(nlevels > 0) {
    
    if(zoom) {
      rho <- oce::swRho(salinity = xlim, temperature = ylim, pressure = rep(1, length(xlim))) - 1000
      rho_breaks <- pretty(range(rho), n = nlevels, min.n = nlevels %/% 2)
    } else {
      rho <- oce::swRho(salinity = xlim, temperature = ylim, pressure = rep(1, length(xlim))) - 1000
      rho_breaks <- pretty(range(rho), n = nlevels-1)
      #rho_breaks <- seq(10, 30, length.out = nlevels)
    }
    
    temp_breaks <- seq(from = ylim[1], to = ylim[2], by = 0.1)
    
    isopycs <- lapply(seq_along(rho_breaks), function(i) {
      data.frame(temp = temp_breaks, rho = rho_breaks[i], sal = oce::swSTrho(temperature = temp_breaks, density = rho_breaks[i], pressure = 1))
    })
    
    isopycs <- do.call(rbind, isopycs)
    
  } else {
    isopycs <- data.frame(temp = NA, rho = NA, sal = NA)
  }
  
  
  ## Main plot ####
  
    p <- ggplot(data = dt, aes_string(x = sal_col, y = temp_col, color = color), alpha = symbol_alpha, size = symbol_size) +
      scale_y_continuous(expression(paste("Potential temperature (", degree, "C", ")", sep = "")), breaks = ybreaks) +
      coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
      theme_classic(base_size = base_size) +
      theme(axis.line = element_line(size = 0.5),
            axis.ticks = element_line(size = 0.5),
            panel.border = element_rect(color = "black", size = 1, fill = NA),
            legend.background = element_blank())
  
  ## Isopycals
  
  if(nlevels > 0) {
    p <- p +
      scale_x_continuous("Practical salinity", breaks = xbreaks,
                         sec.axis = sec_axis(~., breaks = isopycs[isopycs$temp == ylim[2], "sal"], labels = isopycs[isopycs$temp == ylim[2], "rho"], name = "Density")) +
      geom_line(data = isopycs, aes(x = sal, y = temp, group = rho), color = color_isopyc, size = 0.5)
  } else {
    p <- p +
      scale_x_continuous("Practical salinity", breaks = xbreaks)
  }
  
  ## Data points ###
  
  if(plot_data) {
    if(scale2color) {
      p <- p +
        geom_point(data = dt, aes_string(x = sal_col, y = temp_col, color = color), shape = symbol_shape, alpha = symbol_alpha, size = symbol_size)
    } else {
      p <- p +
        geom_point(data = dt, aes_string(x = sal_col, y = temp_col), shape = symbol_shape, alpha = symbol_alpha, size = symbol_size, color = color)
    }
  }
  
  
  ######################
  ## Marginal plots ####
  
  if(margin_distr & plot_data) {
    
    ## Marginal plot for x-axis
    
    if(scale2color) {
      px <- ggplot(data = dt, aes_string(x = sal_col, fill = color)) +
        geom_density(alpha = 0.5, size = 0.2) +
        coord_cartesian(xlim = xlim, expand = FALSE) +
        theme_classic(base_size = base_size) +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
      
    } else {
      px <- ggplot(data = dt, aes_string(x = sal_col)) +
        geom_density(alpha = 0.5, size = 0.2, fill = color) +
        coord_cartesian(xlim = xlim, expand = FALSE) +
        theme_classic(base_size = base_size) +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
      
    }
    
    
    ## Marginal plot for y-axis
    
    if(scale2color) {
      py <- ggplot(data = dt, aes_string(x = temp_col, fill = color)) +
        geom_density(alpha = 0.5, size = 0.2) +
        coord_flip(xlim = ylim, expand = FALSE) +
        theme_classic(base_size = base_size) +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
    } else {
      py <- ggplot(data = dt, aes_string(x = temp_col)) +
        geom_density(alpha = 0.5, size = 0.2, fill = color) +
        coord_flip(xlim = ylim, expand = FALSE) +
        theme_classic(base_size = base_size) +
        theme(axis.title = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
      
    }
    
    
  }
  
  
  ## Finally plotting ####
  
print(p)
}
