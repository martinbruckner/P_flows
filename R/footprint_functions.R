################################################################.
#             FABIO Footprints - function library
################################################################.


# this script creates the functions needed to calculate, organize, and visualize footprint results.
# NOTE: these functions do not calculate anything yet. They are used in the main code to calculate results based on given input parameters (e.g. country, year...)
# each function includes a short description of its arguments.

#----------------------------------------------------------------------------#
# ---- Functions to calculate footprints and organize results --------
#----------------------------------------------------------------------------#


## calculate footprints for a given final demand vector -------

# arguments:
# country = the country of interest
# consumption = the final demand type ("food", "other"...)
# allocation = one of "value" or "mass"
# year = the year of interest

footprint <- function(country = "AUT", consumption = "food", allocation = "value", 
                      year, y, X = X, E = E, v = vers, index, 
                      take.result = FALSE, result.dir = "data", result.suffix = ""){

  # extract data
  Xi <- X[, as.character(year)]
  #Yi <- Y[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  if(!take.result){
    if(allocation == "value") {
      L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",v,"/losses/",year,"_L_value.rds"))
    } else {
      L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",v,"/losses/",year,"_L_mass.rds"))
    }
  }

  # calculate environmental intensities
  E_int <- Ei[,c("landuse","biomass","blue","green","biodiv","n_application","p_application", "ghg_all", "ghg", "luh", "ghg_energy", "ghg_live", "ghg_other",  "ghg_pb")]
  E_int <- E_int/Xi
  E_int[!is.finite(E_int)] <- 0
  E_int <- cbind(Ei[,1:7], E_int)

  # extract relevant final demand vector
  Y_country <- y[, 8:(ncol(y)-1)]
  colnames(Y_country) <- substr(colnames(Y_country), 1, str_locate(colnames(Y_country), "_")[,"start"]-1)
  colnames(Y_country) <- regions$iso3c[match(colnames(Y_country), regions$code)]
  Y_country <- as.matrix(Y_country)[, colnames(Y_country) == country]
  Y_country <- as.numeric(agg(Y_country))
  
  # compute footprints
  short_index <- paste0(index$iso3c, "_", index$item)
  
  # production footprint (= production of each commodity triggered by consumption of each commodity in Y_target)
  #FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))
  if (!take.result){
    FP <- t(t(L) * Y_country)
    colnames(FP) <- rownames(FP) <- short_index <- paste0(index$iso3c, "_", index$item)
    saveRDS(FP, paste0(result.dir,"/v",vers,"/fp_",year,"_",result.suffix,"_",country,".rds"))
  } else if (file.exists(paste0(result.dir,"/v",vers,"/fp_",year,"_",result.suffix,"_",country,".rds"))){
    FP <- readRDS(paste0(result.dir,"/v",vers,"/fp_",year,"_",result.suffix,"_",country,".rds"))
  } else {
    stop("no production footprint result exsiting under", paste0(result.dir,"/v",vers,"/fp_",result.suffix,"_",country,".rds"))
  }
  FP <- as(FP, "dgTMatrix")
  results <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], production =FP@x)
  results[,`:=`(country_origin = substr(origin,1,3),
                item_origin = substr(origin,5,100),
                country_target = substr(target,1,3),
                item_target = substr(target,5,100),
                country_consumer = country,
                year = year)]

  # remove L and FP matrices from memory (they are large!)
  if(!take.result) rm(L)
  rm(FP, Xi, Ei); gc()

  # calculate environmental footprints for all extensions from production footprint
  match_origin = match(results$origin, short_index)
  match_target = match(results$target, short_index)

  results[, `:=` (origin = NULL, target = NULL) ]

  results[,`:=`(p_application   = production * E_int$p_application[match_origin])]

  # add direct consumption of each item for reference (optional)
  #results[, direct_consumption := Y_target[match_origin]]

  # add auxiliary info on origin and target commodities
  results[,`:=`(group_origin  = index$comm_group[match_origin],
                group_target  = index$comm_group[match_target],
                iso_origin = index$iso3c[match_origin],
                continent_origin  = index$continent[match_origin])]
  results[country_origin==country, continent_origin := country]

  # reorder columns
  results <- results %>%
    relocate(c(production, p_application), .after = continent_origin) %>%
    relocate(c(iso_origin, continent_origin), .after = country_origin) %>%
    relocate(group_origin, .after = item_origin) %>%
    relocate(group_target, .after = item_target)

  saveRDS(results, paste0("./output","/fp_v",vers,"_",year,"_", result.suffix,"_",country,".rds"))
  
  return(results)
}


## calculate footprints for a set of final demand vectors -------

# arguments:
# consumption = the final demand type ("food", "other"...)
# allocation = one of "value" or "mass"
# year = the year of interest
# ext = environmental extension

footprint_all <- function(consumption = "all", allocation = "value", 
                      year, y, X = X, E = E, v = vers, ext = c("p_application"), index, 
                      take.result = FALSE, result.dir = "data", result.suffix = ""){
  
  # extract data
  Xi <- X[, as.character(year)]
  #Yi <- Y[[as.character(year)]]
  Ei <- E[[as.character(year)]]
  
  if(!take.result){
    if(allocation == "value") {
      L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",v,"/",year,"_L_value.rds"))
    } else {
      L <- readRDS(file=paste0("/mnt/nfs_fineprint/tmp/fabio/v",v,"/",year,"_L_mass.rds"))
    }
  }
  
  # calculate environmental intensities
  E_int <- Ei[,c("landuse","biomass","blue","green","biodiv","n_application","p_application", "ghg_all", "ghg", "luh", "ghg_energy", "ghg_live", "ghg_other",  "ghg_pb")]
  E_int <- E_int/Xi
  E_int[!is.finite(E_int)] <- 0
  E_int <- cbind(Ei[,1:7], E_int)
  
  if(consumption != "all"){
    Y_target <- as.matrix(y)[, grepl(consumption, colnames(y))]
  } else {
    Y_target <- y[, 8:(ncol(y)-1)]
    colnames(Y_target) <- substr(colnames(Y_target), 1, str_locate(colnames(Y_target), "_")[,"start"]-1)
    Y_target <- agg(as.matrix(Y_target))
  }
  
  # compute footprints
  short_index <- paste0(index$iso3c, "_", index$item)
  
  # production footprint (= production of each commodity triggered by consumption of each commodity in Y_target)
  #FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))
  if (!take.result){
    FP <- L %*% Y_target
    rownames(FP) <- short_index <- paste0(index$iso3c, "_", index$item)
    saveRDS(FP, paste0(result.dir,"/v",vers,"/fp_",result.suffix,"_",year,".rds"))
  } else if (file.exists(paste0(result.dir,"/v",vers,"/fp_",result.suffix,"_",year,".rds"))){
    FP <- readRDS(paste0(result.dir,"/v",vers,"/fp_",result.suffix,"_",year,".rds"))
  } else {
    stop("no production footprint result exsiting under", paste0(result.dir,"/v",vers,"/fp_",result.suffix,"_",year,".rds"))
  }
  FP <- as(FP, "dgTMatrix")
  results <- data.table(origin=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], production =FP@x)
  results[,`:=`(iso_origin = substr(origin,1,3),
                item_origin = substr(origin,5,100),
                country_consumer = target,
                year = year)]
  
  # remove L and FP matrices from memory (they are large!)
  if(!take.result) rm(L)
  rm(FP, Xi, Ei); gc()
  
  # calculate environmental footprints for all extensions from production footprint
  match_origin = match(results$origin, short_index)
  match_target = match(results$country_consumer, regions$code)
  
  results[, `:=` (origin = NULL, target = NULL) ]
  results[,`:=`(p_application   = production * E_int$p_application[match_origin])]
  
  # add auxiliary info on origin and target commodities
  results[,`:=`(group_origin  = index$comm_group[match_origin],
                continent_origin  = index$continent[match_origin],
                iso_consumer = regions$iso3c[match_target],
                continent_consumer  = regions$continent[match_target],
                country_origin = regions$area[match(results$iso_origin, regions$iso3c)],
                country_consumer = regions$area[match_target])]
  
  # reorder columns
  results <- results %>%
    relocate(c(iso_origin, continent_origin), .after = country_origin) %>%
    relocate(c(production, p_application), .after = continent_origin) %>%
    relocate(group_origin, .after = item_origin) %>%
    relocate(year, .after = group_origin) %>% 
    relocate(c(iso_consumer, continent_consumer), .after = country_consumer)
  
  
  return(results)
}



##  aggregate columns of a matrix by their name -------
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }



##  extract final demand vector being investigated -------

# arguments:
# Y = the Y list
# year = the year of interest
# country = the country of interest
# type = the final demand type ("food", "other"...)

get_demand <- function(Y = Y, year = 2013, consumption = "food", country = NA){
  Yi <- Y[[as.character(year)]]
  Y_country <- Yi[, Y_codes$iso3c == country]
  colnames(Y_country) <- Y_codes$fd[Y_codes$iso3c == country]
  Y_target <- as.vector(as.matrix(Y_country[,consumption]))
}


## add per-capita-values to a footprint table -----------
# arguments:
# fp = the footprint data.table
add_per_capita <- function(fp){
  pop_data <- as.data.table(wbstats::wb_data(indicator = "SP.POP.TOTL", start_date = min(fp$year), end_date = max(fp$year)))
  pop_data <- pop_data[,.(iso3c, year = date, pop = SP.POP.TOTL)]
  attributes(pop_data$pop)$label <- NULL
  fp <- merge(fp, pop_data, by.x = c("country_consumer", "year"), by.y = c("iso3c", "year"))
  fp[, `:=` (production_pc = production/pop,
             landuse_pc = landuse/pop,
             biomass_pc = biomass/pop,
             blue_pc = blue/pop,
             green_pc = green/pop,
             ghg_pc = ghg/pop,
             luh_pc = luh/pop)]
}


##  easy aggregation of footprint table --------------------------------

# arguments:
# fp = the footprint data.table
# aggregate_by = a vector of columns to aggregate results by
# indicators = indicators to keep (default is all of them)
fp_aggregate <- function(fp, aggregate_by, indicators = c("landuse", "biomass", "green", "blue", "ghg", "luh", "ghg_energy",  "ghg_all", "ghg_pb", "biodiv", "n_application", "p_application")){
  indicators <- names(fp)[grep(paste(indicators, collapse = "|"), names(fp))]
  fp <- fp[, lapply(.SD, sum, na.rm=TRUE), by = aggregate_by, .SDcols = indicators]
}

## auxiliary functions  ------------
is.finite.data.frame <- function(x) do.call(cbind, lapply(x, is.finite))



#----------------------------------------------------------------------------#
# ----    Functions to visualize results         --------
#----------------------------------------------------------------------------#



## footprint map visualization  ----------------------------------------------------------------

# arguments:
# fp = the footprint table (can be the raw fp_country table without any aggregation)
# map = map file to be plotted (world_map)
# indicator = the indicator to be plotted
# per_capita = should results be by capita or aggregate (TRUE/FALSE)
# origin_items = either "ALL" for aggregating over all origin items or a vector of product names (e.g. "Soyabeans") or code (e.g. 2555)
# target_items = either "ALL" for aggregating over all target items or a vector of product names (e.g. "Pigmeat") or code (e.g. 2733)
# origin_groups = if "origin_items" is not used, you can instead provide a vector of comm_group names (e.g. "Cereals")
# target_groups = if "target_items" is not used, you can instead provide a vector of comm_group names (e.g. "Meat")



fp_map <- function(fp, map = world_map, indicator = "landuse", per_capita = FALSE,
                   origin_items = "ALL", target_items = "ALL", origin_groups, target_groups, 
                   title = "", lang = "de", ...){

  # extract and aggregate accorging to input
  if (!missing(origin_groups)){
    origin_items <- "ALL"
    fp <- fp[group_origin %in% origin_groups,]
    cat("origin_groups is used and overwrites the origin_item argument")
  }
  if (!missing(target_groups)) {
    target_items <- "ALL"
    fp <- fp[group_target %in% target_groups,]
    cat("target_groups is used and overwrites the target_item argument")
  }

  if(is.numeric(target_items[1])) target_items <- items$item[match(target_items, items$item_code)]
  if(is.numeric(origin_items[1])) target_items <- items$item[match(target_items, items$item_code)]

  # filter (if needed) and aggregate by origin country
  if (target_items != "ALL") fp <- fp[item_target %in% target_items,]
  if (origin_items != "ALL") fp <- fp[item_target %in% origin_items,]
  fp <- fp_aggregate(fp, aggregate_by = c("country_origin"))

  # merge fp with world map
  world_fp <- left_join(map, fp, by = c("ISO_A3" = "country_origin"))

  # set legend title and unit
  if (lang == "de") {
  indicators_long <-  c("landuse" = "Anbaufläche", "biomass" = "Biomasse", "blue" = "Wassereinsatz", "green" = "Grünes Wasser", "ghg" = "THG-Emissionen", "luh" = "THG-Emissionen", "ghg_all" = "THG-Emissionen",
                        "biodiv" = "Biodiversitätsverlust", "n_application" = "Stickstoffeinsatz", "p_application" = "Phosphoreinsatz")
  units <- c("landuse" = "m<sup>2</sup>", "biomass" = "t", "blue" = "m<sup>3</sup>", "green" = "m<sup>3</sup>", "ghg" = "t CO<sub>2</sub>-Äq.", "luh" = "t CO<sub>2</sub>-Äq.", "ghg_all" = "t CO<sub>2</sub>-Äq.",
             "biodiv" = "Arten pro Jahr", "n_application" = "kg", "p_application" = "kg")
  
  } else if (lang == "en") {
  indicators_long <- c("landuse" = "Cropland", "biomass" = "Biomass", "blue" = "Water use", "green" = "Green water use", "ghg" = "GHG emissions", "luh" = "GHG emissions", "ghg_all" = "GHG emissions",
                        "biodiv" = "Biodiversity loss", "n_application" = "Nitrogen use", "p_application" = "Phosphorous use")
  units <- c("landuse" = "m<sup>2</sup>", "biomass" = "t", "blue" = "m<sup>3</sup>", "green" = "m<sup>3</sup>", "ghg" = "t CO<sub>2</sub>-eq.", "luh" = "t CO<sub>2</sub>-eq.", "ghg_all" = "t CO<sub>2</sub>-eq.",
             "biodiv" = "extinct species <br> per year", "n_application" = "kg", "p_application" = "kg")
  }
  
  indicator_long <- indicators_long[indicator]
  unit = units[indicator]
  
  

  if(per_capita) indicator <- paste0(indicator,"_pc")

  # plot
  ggmap <- ggplot(data = world_fp) +
    geom_sf(aes(fill = .data[[indicator]]), size = 0.05) +
    labs(fill=paste(indicator_long, " <br> in", unit), title = title) +
    scale_fill_viridis_c(direction = -1, na.value = "lightgrey", ...) +
    coord_sf(crs = "+proj=robin") + # "+proj=moll"   "+proj=wintri"
    theme_map() +
    theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot", legend.title = element_markdown(size = 8)) #legend.position = "right"
  
  if(title == "") ggmap <- ggmap + theme(plot.title = element_blank())
  
  return(ggmap)

}



## mosaic plot -------------------------------------------------

# arguments:

# fp = the footprint table (can be the raw fp_country table without any aggregation)
# indicator = the indicator to be plotted
# per_capita = should results be by capita or aggregate (TRUE/FALSE)
# target_items = optional, a vector of target items to subset results
# target_groups = optional, a vector of target commodity groups to subset results
# divide_by_cells = a factor by which the values displayed in the cells are divided (default 1000)
# divide_by_axis = a factor by which the values displayed on the x-axis are divided (default 1000000)
# display_min = a threshold defining the minimum value to be plotted in the cells to avoid overplotting
# axis_label = character string giving the x-axis label (depends on indicator chose, e.g. "Million hectares")
# plot_title = character string giving the title of the plot
# tick_offset = a vector of length 9 offsetting the tick marks on the x-axis. This has to be adapted manually for each plot


fp_mosaic <- function(fp, indicator, per_captia = FALSE, consumer_country = "AUT",
                      target_items, target_groups, aggregate_by =  c("group_target", "continent_origin"),
                      divide_by_cells = 1000, divide_by_axis = 1000000, 
                      display_min = 10, round_digs = 0,
                      #plot_title = "Land-use footprint by origin country and commodity group",
                      #axis_label = "Million hectares",
                      tick_offset = c(0), 
                      row = c("ROW", "OCE"),
                      lang = "de") {


  # if target item or group was given, filter the fp table accordingly
  if(!(missing(target_items)) & !(missing(target_groups))) stop("either specify 'target_items' or 'target_groups', but not both")
  if(!missing(target_items)) fp <- fp[item_target %in% target_items,]
  if(!missing(target_groups)) fp <- fp[group_target %in% target_groups,]

  # aggregate data for mosaic format
  fp[continent_origin %in% row, continent_origin := "ROW"]
  fp_mosaic <- fp_aggregate(fp, aggregate_by = aggregate_by)
  setnames(fp_mosaic, aggregate_by, c("group", "region"))


  # select indicator
  if(per_captia) indicator <- paste0(indicator,"_pc")
  setnames(fp_mosaic, indicator, "value")

  # change unit of indicator

  fp_mosaic[,value := round(value/divide_by_cells, round_digs)]
  #fp_mosaic[,value := value/divide_by_cells]
  fp_mosaic <- fp_mosaic[value > 0,]

  mycols <- food_cols_vect[names(food_cols_vect) %in% unique(fp_mosaic$group)]
  region_levels <- c(consumer_country, "EU", "EUR", "ASI", "AFR", "LAM", "NAM", "OCE", "ROW")
  region_levels <- region_levels[!region_levels %in% row[row != "ROW"]]
  fp_mosaic[,`:=` (group = factor(group, levels = names(mycols)),
                   region = factor(region, levels = region_levels))]
  
  cat("\n region order from left to right:", region_levels) 
  if (length(row) > 1)  cat("\n", row[row != "ROW"], ifelse(length(row) > 2, paste("are"), paste("is")), "added to ROW via the 'row' argument \n")


  #mycols=c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black",
  #         "gold1", "skyblue2", "#FB9A99", "palegreen2", "maroon", "#CAB2D6", "orchid1", "deeppink1", "blue1",
  #         "steelblue4", "darkturquoise", "green1", "yellow4", "yellow3",
  #         "darkorange4")



  scale_x_productlist <- function (name = waiver(), breaks = ggmosaic:::product_breaks(), minor_breaks = NULL,
                                   labels = ggmosaic:::product_labels(), limits = NULL, expand = waiver(),
                                   oob = scales:::censor, na.value = NA_real_, trans = "identity",
                                   position = "bottom", sec.axis = waiver())
  {
    sc <- ggplot2::continuous_scale(c("x", "xmin", "xmax", "xend",
                                      "xintercept", "xmin_final", "xmax_final", "xlower",
                                      "xmiddle", "xupper"), "position_c", identity, name = name,
                                    breaks = breaks, minor_breaks = minor_breaks, labels = labels,
                                    limits = limits, expand = expand, oob = oob, na.value = na.value,
                                    trans = trans, guide = "none", position = position,
                                    super = ScaleContinuousProduct)
    if (!ggplot2:::is.waive(sec.axis)) {
      if (ggplot2:::is.formula(sec.axis))
        sec.axis <- ggplot2::sec_axis(sec.axis)
      is.sec_axis = getFromNamespace("is.sec_axis", "ggplot2")
      if (!ggplot2:::is.sec_axis(sec.axis))
        stop("Secondary axes must be specified using 'sec_axis()'")
      sc$secondary.axis <- sec.axis
    }
    sc
  }

  breaks_values <- fp_mosaic %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(value = round(sum(value) / (divide_by_axis/divide_by_cells)), round_digs) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = cumsum(value) )

  # change to string for labelling
  breaks_values$region <- as.character(breaks_values$region)
  
  
  # labels
  if (lang == "de") {
    titles <- c("landuse" = "Flächenfußabruck", 
              "biomass" = "Biomasse", 
              "blue" = "Wasserfußabruck", 
              "green" = "Grünes Wasser", 
              "ghg" = "Emissionsfußabruck", 
              "luh" = "Emissionsfußabruck", 
              "ghg_all" = "Emissionsfußabruck",
              "biodiv" = "Biodiversitätsfußabruck", 
              "n_application" = "Stickstoffeinsatz", 
              "p_application" = "Phosphoreinsatz")
    axis_labs <- c("landuse" = expression(paste("Anbaufläche in ", m^2)), 
                   "biomass" =  "", 
                   "blue" = expression(paste("Wasserinsatz in ", m^3)), 
                   "green" = "Grünes Wasser", 
                   "ghg" = expression(paste("THG-Emissionen in kg ", CO[2],"-Äq.")), 
                   "luh" = expression(paste("THG-Emissionen in kg ", CO[2],"-Äq.")), 
                   "ghg_all" = expression(paste("THG-Emissionen in kg ", CO[2],"-Äq.")),
                   "biodiv" = expression(paste("Biodiversitätsverlust in ", 10^-14, " Arten/Jahr")), 
                   "n_application" = "Stickstoffeinsatz in g", 
                   "p_application" = "Phosphoreinsatz in g")
  } else if (lang == "en") {
    titles <- c("landuse" = "Cropland footprint", 
                "biomass" = "Biomass footprint", 
                "blue" = "Water footprint", 
                "green" = "Green water footprint", 
                "ghg" = "Emission footpint", 
                "luh" = "Emission footpint", 
                "ghg_all" = "Emission footpint",
                "biodiv" = "Biodiversity footprint", 
                "n_application" = "Nitrogen footprint", 
                "p_application" = "Phosphorous footprint")
    axis_labs <- list("landuse" = expression(paste("Cropland in ", m^2)), 
                   "biomass" =  "", 
                   "blue" = expression(paste("Water use in ", m^3)), 
                   "green" = "Grünes Wasser", 
                   "ghg" = expression(paste("GHG emissions in kg ", CO[2]," eq.")), 
                   "luh" = expression(paste("GHG emissions in kg ", CO[2]," eq.")), 
                   "ghg_all" = expression(paste("GHG emissions in kg ", CO[2]," eq.")),
                   "biodiv" = expression(paste("Biodiversity loss in ", 10^-14, " species/year")), 
                   "n_application" = "Nitrogen use in g", 
                   "p_application" = "Phosphorous use in g")
  }
    
  plot_title <- titles[indicator]
  axis_label <- axis_labs[[indicator]]

  # debugonce(check_breaks_labels)
  # debugonce(geom_mosaic)
  mosaic <-
    ggplot(data = fp_mosaic) +
    geom_mosaic(aes(weight = value, x = product(group, region), fill = group), na.rm=T, divider=ddecker(), offset = 0.005) +
    #theme(axis.text.x=element_text(angle=-25, hjust= .1), legend.position="right") +
    theme(legend.position="right") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_productlist(position = "top", #  labels = breaks_values$region, breaks = breaks_values$value
                        sec.axis = ggplot2::sec_axis(~.*sum(fp_mosaic$value/(divide_by_axis/divide_by_cells)),
                                                     breaks = c(0,breaks_values$value) + tick_offset,
                                                     labels = round(c(0,breaks_values$value), 2), name = axis_label)) +
    # facet_grid(Group~.) +
    labs(x = "", y = ifelse (lang == "de", "Anteil je Produktgruppe", "Share per product group")) +
    guides(fill=guide_legend(title = "Commodities", reverse = TRUE)) +
    # viridis::scale_fill_viridis(option = "magma", discrete = TRUE)
    scale_fill_manual(values = mycols)
    #ggsci::scale_fill_npg()


  (mosaic <- mosaic +
      geom_text(data = ggplot_build(mosaic)$data[[1]], aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label=replace(.wt, .wt < display_min, "")))+
      ggtitle(label = paste0(plot_title, ifelse(lang == "de", " je Region und Produktgruppe", " per region and product group"))) +
      theme(axis.text.x = element_text(angle = 50, hjust=1), plot.title.position = "plot",
            plot.title = element_text(margin=margin(0,0,25,0))))

  return(mosaic)

}


# stacked bar chart by item ------------------

stacked_bars <- function(fp_list, indicator = "landuse", per_capita = FALSE,
                         aggregate_by =  c("comm_group_plot"), bound = TRUE,
                         origin_items = "ALL", target_items = "ALL", origin_groups, target_groups, 
                         axis_lab = "",
                         title = "",
                         lang = "de",
                         reverse_legend = FALSE){
  
  # aggregate footprints by consumer item and combine to table with diets in columns
  if (lang == "de") diet_labs = c("sq" = "Status \nQuo", "epo" = "Ernährungs-\npyramide", "eat" =  "Planetary \nHealth Diet", "epo2" = "Ernährungs-\npyramide 2.0")
  if (lang == "en") diet_labs = c("sq" = "Status \nQuo", "epo" = "Nutrition\nPyramid", "eat" =  "Planetary \nHealth Diet", "epo2" = "Nutrition\nPyramid 2.0")
    fp_agg_long <- lapply(names(fp_list), function(fp){
    if (indicator == "landuse") fp_list[[fp]] <- fp_list[[fp]][group_origin != "Grazing",]
    tab <- fp_aggregate(fp_list[[fp]], aggregate_by = "comm_group_plot", indicators = indicator) %>%
      #rename(!!sym(fp) := !!sym(indicator)) %>% 
      mutate(diet = fp, diet_lab = factor(diet_labs[fp], levels = diet_labs[names(diet_labs) %in% names(fp_list)]))
    if(indicator == "ghg") tab <- dplyr::select(tab, !ghg_all)
    return(tab)
    }) %>% rbindlist
  #fp_ind <- fp_list_agg %>% purrr::reduce(inner_join, by = "comm_group_plot") %>% 
  
  # plot
  food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(fp_agg_long$comm_group_plot)]
  
  fp_plot <- ggplot(fp_agg_long, aes(x = diet_lab, y = !!sym(indicator), fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
      geom_bar(stat="identity", alpha = 0.85) +
      scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend(reverse = reverse_legend)) +
      labs(y = axis_lab, x = "") +
      theme_minimal() +
      theme(legend.position = "bottom", legend.direction="horizontal", legend.box = "horizontal",
      axis.title.y = element_markdown(face = "bold", size = 10))
  
  if(bound) {
    if(lang == "de") {
      fp_plot <- fp_plot + 
        geom_hline(aes(yintercept = pbs[ifelse(indicator %in% c("ghg", "luh", "ghg_pb"), "ghg_all", indicator), "boundary"], color = "Planetare Belastungsgrenze"), size = 0.4, linetype = "solid") +
        geom_hline(aes(yintercept = pbs[ifelse(indicator %in% c("ghg", "luh", "ghg_pb"), "ghg_all", indicator), "upper"], color = "Obergrenze der Unsicherheitszone"), size = 1.0) +
        geom_hline(aes(yintercept = pbs[ifelse(indicator %in% c("ghg", "luh", "ghg_pb"), "ghg_all", indicator), "lower"], color = "Untergrenze der Unsicherheitszone"), size = 1.0)+
        scale_color_manual(values = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")) + 
        #scale_linetype_manual(values = c("Planetare Belastungsgrenze" = "solid",  "Obergrenze der Unsicherheitszone" = "solid", "Untergrenze der Unsicherheitszone" = "solid")) + 
        labs(color = NULL)+
        guides(color=guide_legend(direction='vertical'))
    } else if (lang == "en"){
      fp_plot <- fp_plot + 
        geom_hline(aes(yintercept = pbs[ifelse(indicator %in% c("ghg", "luh", "ghg_pb"), "ghg_all", indicator), "boundary"], color = "Planetary boundary"), size = 0.4, linetype = "solid") +
        geom_hline(aes(yintercept = pbs[ifelse(indicator %in% c("ghg", "luh", "ghg_pb"), "ghg_all", indicator), "upper"], color = "Upper limit of zone of unceartainty"), size = 1.0) +
        geom_hline(aes(yintercept = pbs[ifelse(indicator %in% c("ghg", "luh", "ghg_pb"), "ghg_all", indicator), "lower"], color = "Lower limit of zone of unceartainty"), size = 1.0)+
        scale_color_manual(values = c("Planetary boundary" = "black",  "Upper limit of zone of unceartainty" = "red", "Lower limit of zone of unceartainty" = "darkgreen")) + 
        #scale_linetype_manual(values = c("Planetare Belastungsgrenze" = "solid",  "Obergrenze der Unsicherheitszone" = "solid", "Untergrenze der Unsicherheitszone" = "solid")) + 
        labs(color = NULL)+
        guides(color=guide_legend(direction='vertical'))
    }
    }
  
  return(fp_plot)
}

# just the data
stacked_data <- function(fp_list, indicator = "landuse", per_capita = FALSE,
                        aggregate_by =  c("comm_group_plot")){
  
  # aggregate footprints by consumer item and combine to table with diets in columns
  diet_labs = c("sq" = "Status \nQuo", "eat" =  "Planetary \nHealth", "epo" = "Ernährungs-\npyramide")
  fp_agg_long <- lapply(names(fp_list), function(fp){
    if (indicator == "landuse") fp_list[[fp]] <- fp_list[[fp]][group_origin != "Grazing",]
    tab <- fp_aggregate(fp_list[[fp]], aggregate_by = "comm_group_plot", indicators = indicator) %>%
      #rename(!!sym(fp) := !!sym(indicator)) %>% 
      mutate(diet = fp, diet_lab = factor(diet_labs[fp], levels = diet_labs))
    if(indicator == "ghg") tab <- dplyr::select(tab, !ghg_all)
    return(tab)
  }) %>% rbindlist
  #fp_ind <- fp_list_agg %>% purrr::reduce(inner_join, by = "comm_group_plot") %>% 
  
}


# just the data
stacked_data <- function(fp_list, indicator = "landuse", per_capita = FALSE,
                         aggregate_by =  c("comm_group_plot")){
  
  # aggregate footprints by consumer item and combine to table with diets in columns
  diet_labs = c("sq" = "Status \nQuo", "eat" =  "Planetary \nHealth", "epo" = "Ernährungs-\npyramide", "epo2" = "Ernährungs-\npyramide 2.0")
  fp_agg_long <- lapply(names(fp_list), function(fp){
    if (indicator == "landuse") fp_list[[fp]] <- fp_list[[fp]][group_origin != "Grazing",]
    tab <- fp_aggregate(fp_list[[fp]], aggregate_by = "comm_group_plot", indicators = indicator) %>%
      #rename(!!sym(fp) := !!sym(indicator)) %>% 
      mutate(diet = fp, diet_lab = factor(diet_labs[fp], levels = diet_labs))
    if(indicator == "ghg") tab <- dplyr::select(tab, !ghg_all)
    return(tab)
  }) %>% rbindlist
  #fp_ind <- fp_list_agg %>% purrr::reduce(inner_join, by = "comm_group_plot") %>% 
  
}


# version for single scenario with indicators side-by-side
stacked_bars_single <- function(fp, ind_list = c("ghg", "luh"), ind_labs,
                         aggregate_by =  c("comm_group_plot"), 
                         origin_items = "ALL", target_items = "ALL", origin_groups, target_groups, 
                         axis_lab = "",
                         title = "",
                         legend_pos = "right", legend_dir = "vertical", legend_box = "horizontal",
                         reverse_legend = TRUE){
  
  if(missing(ind_labs)){
    ind_labs = ind_list
    names(ind_labs) <- ind_list
  } 
  
  # aggregate footprints by consumer item and combine to table with diets in columns
  #diet_labs = c("sq" = "Status \nQuo", "epo" = "Ernährungs-\npyramide", "eat" =  "Planetary \nHealth Diet", "epo2" = "Ernährungs-\npyramide 2.0")
#  fp_agg_long <- lapply(names(fp_list), function(fp){
#    if (indicator == "landuse") fp_list[[fp]] <- fp_list[[fp]][group_origin != "Grazing",]
  fp_agg_long <- fp_aggregate(fp, aggregate_by = "comm_group_plot", indicators = ind_list) %>%
      pivot_longer(cols = all_of(ind_list), names_to = "indicator", values_to = "value") %>%
      #rename(!!sym(fp) := !!sym(indicator)) %>% 
      mutate(indicator = factor(ind_labs[match(indicator, names(ind_labs))], levels = ind_labs))
#    if(indicator == "ghg") tab <- dplyr::select(tab, !ghg_all)
#    return(tab)
#  }) %>% rbindlist
  #fp_ind <- fp_list_agg %>% purrr::reduce(inner_join, by = "comm_group_plot") %>% 
  
  # plot
  food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(fp_agg_long$comm_group_plot)]
  
  fp_plot <- ggplot(fp_agg_long, aes(x = indicator, y = value, fill = factor(comm_group_plot, levels = rev(names(food_cols_vect_sel))))) +
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = food_cols_vect_sel, name = "", guide = guide_legend(reverse = reverse_legend)) +
    labs(y = axis_lab, x = "") +
    theme_minimal() +
    theme(legend.position = legend_pos, legend.direction=legend_dir, legend.box = legend_box,
          axis.title.y = element_markdown(face = "bold", size = 10))
  

  return(fp_plot)
}


# stack for emissions luh/ghg

stacked_bars_ghg <- function(fp, ind_list = c("ghg_pb", "ghg_energy", "luh"), mult_factor = 1, 
                                aggregate_by =  c("comm_group_plot"), 
                                origin_items = "ALL", target_items = "ALL", origin_groups, target_groups, 
                                axis_lab = "",
                                title = "",
                                legend_pos = "bottom", legend_dir = "vertical", legend_box = "vertical",
                                reverse_legend = TRUE){
  
  if(lang == "de") ind_labs <- c("ghg_pb" = "Landwirtschaft", 
                                 "ghg_energy" = "Energieverbrauch in der Landwirtschaft", 
                                 "ghg_live" = "Tierhaltung",
                                 "luh" = "Landnutzungsänderung",
                                  "ghg_other" = "andere Emissionen in der Landwirtschaft")
  if(lang == "en") ind_labs <- c("ghg_pb" = "Agriculture", 
                                 "ghg_energy" = "Energy",  
                                 "ghg_live" = "Animal husbandry",
                                 "luh" = "Land use change",
                                 "ghg_other" = "others")
  
  
  ind_labs <- ind_labs[ind_list]
  
  #if(missing(ind_labs)){
  #  ind_labs = ind_list
  #  names(ind_labs) <- ind_list
  #} 
  
  # aggregate footprints by consumer item and combine to table with diets in columns
  #diet_labs = c("sq" = "Status \nQuo", "epo" = "Ernährungs-\npyramide", "eat" =  "Planetary \nHealth Diet", "epo2" = "Ernährungs-\npyramide 2.0")
  #  fp_agg_long <- lapply(names(fp_list), function(fp){
  #    if (indicator == "landuse") fp_list[[fp]] <- fp_list[[fp]][group_origin != "Grazing",]
  comm_group_plot_lab <- unique(items_group$comm_group_plot)
  names(comm_group_plot_lab) <- comm_group_plot_lab
  if (lang == "de"){
    comm_group_plot_lab["Zucker und Süßungsmittel"] <- "Zucker und Süßungs-mittel"
    comm_group_plot_lab["Milchprodukte"] <- "Milch-produkte"
    comm_group_plot_lab["Pflanzenöle"] <- "Pflanzen-öle"
    comm_group_plot_lab["Fisch und Meeresfrüchte"] <- "Fisch und Meeres-früchte"
    comm_group_plot_lab["Hülsenfrüchte"] <- "Hülsen-früchte"
  } 
  

  
  fp_agg_long <- fp_aggregate(fp, aggregate_by = "comm_group_plot", indicators = c(ind_list)) %>% #, "ghg_all"
    pivot_longer(cols = all_of(c(ind_list)), names_to = "indicator", values_to = "value") %>% #  "ghg_all"
    #rename(!!sym(fp) := !!sym(indicator)) %>% 
    mutate(indicator_name = factor(ind_labs[match(indicator, names(ind_labs))], levels = rev(ind_labs))) %>%
    mutate(comm_group_plot = gsub(" ", "\n", comm_group_plot_lab[match(comm_group_plot, names(comm_group_plot_lab))])) %>%
    mutate(comm_group_plot = gsub("-", "-\n", comm_group_plot)) %>%
    mutate(value = value * mult_factor)
  #    if(indicator == "ghg") tab <- dplyr::select(tab, !ghg_all)
  #    return(tab)
  #  }) %>% rbindlist
  #fp_ind <- fp_list_agg %>% purrr::reduce(inner_join, by = "comm_group_plot") %>% 
  
  # plot
  food_cols_vect_sel <- food_cols_vect[names(food_cols_vect) %in% unique(fp_agg_long$comm_group_plot)]
  ghg_cols = viridis::viridis(length(ind_list))#c("purple", "seagreen")
  names(ghg_cols) <- ind_labs
  
  fp_plot <- ggplot(fp_agg_long, aes(x = reorder(comm_group_plot, value, FUN = function(x){-sum(x)}), y = value, fill = indicator_name)) + # factor(comm_group_plot, levels = rev(names(food_cols_vect_sel)))
    geom_bar(stat="identity", alpha = 0.85) +
    scale_fill_manual(values = ghg_cols, name = "", guide = guide_legend(reverse = reverse_legend)) +
    labs(y = axis_lab, x = "") +
    theme_minimal() +
    theme(legend.position = legend_pos, legend.direction=legend_dir, legend.box = legend_box,
          axis.title.y = element_markdown(face = "bold", size = 10))
  
  
  return(fp_plot)
}


# simple bars compared to planetary boundaries ---------------

pb_bars <- function(fp_agg, indicator, lang = "de") {
  
  if (lang == "de"){
    scale_col = c("Planetare Belastungsgrenze" = "black",  "Obergrenze der Unsicherheitszone" = "red", "Untergrenze der Unsicherheitszone" = "darkgreen")
    ylabs <-c(
      "landuse" = "Flächenverbrauch in in m<sup>2</sup>", 
      "biomass" = "Biomasse", 
      "blue" = "Wasserverbrauch in m<sup>3</sup>", 
      "green" = "Grüner Wasserverbrauch in m<sup>3</sup>", 
      "ghg_pb" = "Emissionen (exkl. Energie und LUC) in t CO<sub>2</sub>-Äq.",  
      "luh" = "Emissionen aus Landnutzungsänderung in t CO<sub>2</sub>-Äq.", 
      "ghg_all" = "Emissionen in t CO<sub>2</sub>-Äq.",
      "biodiv" = "Biodiversitätsverlust in Arten", 
      "n_application" = "Stickstoffeinsatz in kg", 
      "p_application" = "Phosphoreinsatz in kg")
  } else if (lang == "en") {
    scale_col = c("Planetary boundary" = "black",  "Upper limit of zone of unceartainty" = "red", "Lower limit of zone of unceartainty" = "darkgreen")
    ylabs <-c(
      "landuse" = "Cropland use in in m<sup>2</sup>", 
      "biomass" = "Biomass", 
      "blue" = "Water use in m<sup>3</sup>", 
      "green" = "Green water use in m<sup>3</sup>", 
      "ghg_pb" = "Emissions (exkl. energy and LUC) in t CO<sub>2</sub> eq.",  
      "luh" = "Emissions from LUC in t CO<sub>2</sub> eq.", 
      "ghg_all" = "Emissions in t CO<sub>2</sub> eq.",
      "biodiv" = "Biodiversity loss in species", 
      "n_application" = "Nitrogen use in kg", 
      "p_application" = "Phosphorous use in kg")
  }
  
  bar_cols <- c(
    "landuse" = viridis(6)[1], 
    "biomass" = "brown", 
    "blue" = viridis(6)[2], 
    "green" = viridis(6)[2], 
    "ghg_pb" = viridis(6)[3],  
    "luh" = viridis(6)[3], 
    "ghg_all" = viridis(6)[3],
    "biodiv" = viridis(6)[4], 
    "n_application" = viridis(6)[5], 
    "p_application" = viridis(6)[6]
  )
  
  pb_bar <- ggplot(fp_agg, aes(x = diet, y = !!sym(indicator))) + 
   geom_bar(stat="identity", fill = bar_cols[indicator]) +
   geom_hline(aes(yintercept = pbs[indicator, "boundary"], color =  names(scale_col)[1]), size = 0.4, linetype = "solid") +
   geom_hline(aes(yintercept = pbs[indicator, "upper"], color = names(scale_col)[2]), size = 1) +
   geom_hline(aes(yintercept = pbs[indicator, "lower"], color = names(scale_col)[3]), size = 1) +
   scale_color_manual(values = scale_col) + 
   labs(y = ylabs[indicator], x = NULL, color = NULL) +
   theme_minimal()+
   theme(axis.title.y = element_markdown())
  
  return(pb_bar)

}

# circle planetary boundary plot --------------------

circle_plot <- function(fp_table, diet, ylim.max = 4, ylim.min = 0, log = FALSE, legend = TRUE){
  
  diet_index <- ifelse(diet %in% c("sq", "Status Quo"),1, ifelse(diet %in% c("eat", "Planetary Health"),2,ifelse(diet %in% c("epo", "Ernährungspyramide"),3,NA)))
  title = ifelse(diet %in% c("sq", "Status Quo"),"Status Quo", ifelse(diet %in% c("eat", "Planetary Health"),"Planetary Health Diet",ifelse(diet %in% c("epo", "Ernährungspyramide"),"Ernährungspyramide",NA)))

  
  fp_circle_scen <- data.frame(ind = as.character(names(fp_table[-1])), 
                             value = unlist(fp_table[diet_index,][-1]),
                             limit = ifelse(unlist(fp_table[diet_index,][-1]) > ifelse(log, 0, 1), "yes", "no"))
  
  
  plt <- ggplot(fp_circle_scen) +
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0:(ylim.max-1))),
      color = "lightgrey",
      alpha = 0.9
    ) + 
    geom_col(
      aes(
        x = ind,
        y = value,
        fill = value
      ),
      color = "gray",
      position = "dodge2",
      show.legend = legend,
      alpha = .9
    ) +
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(ifelse(log, 0,1))),
      color = "blue",
      size = 0.8,
      alpha = 0.8
    ) + 
    # Make it circular!
    #scale_fill_manual(values = c("yes" = "red", "no" = "green")) +
    scale_fill_gradientn(colors = c("darkgreen", "green", "yellow", "red", "firebrick"), 
                         values = scales::rescale(c(0,0.8, 1.5, 2,4)),
                         limits = c(0, ylim.max),
                         name = "Value relative to \n planetary boundary") +
    #scale_fill_gradient2(low = "darkgreen", mid =  "yellow", high =  "firebrick", midpoint = 1.2, limits = c(0,4)) +
    
    scale_y_continuous(
      limits = c(ylim.min, ylim.max),
      expand = c(0, 0),
      breaks = c(seq(0,1,1)) 
    ) +
    coord_polar(clip = "off")+
    labs(title = title) + 
    #expand_limits(y = 10) + # or some other arbitrarily large number
    theme_minimal() +
    theme(
      # Remove axis ticks and text
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      # Use gray text for the region names
      axis.text.x = element_text(color = "gray12", size = 8, face="bold"),
      # Move the legend to the bottom
      legend.position = "bottom",
      panel.grid=element_blank(),
      plot.title = element_text(hjust = 0.5)
      #plot.margin = margin(0,1,0,1,unit = "cm")
    )
  
  return(plt)
  
}


circle_plot_grad <- function(fp_table, diet, ylim.max = 4, ylim.min = 0, log = FALSE, legend = TRUE, lang = "de"){
  
  if (lang == "de"){
    diet_index <- ifelse(diet %in% c("sq", "Status Quo"),1, 
                         ifelse(diet %in% c("eat", "Planetary Health"),3,
                                ifelse(diet %in% c("epo", "Ernährungspyramide"),2,
                                       ifelse(diet %in% c("epo2", "Ernährungspyramide 2.0"),4,NA))))
    title = ifelse(diet %in% c("sq", "Status Quo"),"Status Quo", 
                   ifelse(diet %in% c("eat", "Planetary Health"),"Planetary Health Diet",
                          ifelse(diet %in% c("epo", "Ernährungspyramide"),"Ernährungspyramide",
                                 ifelse(diet %in% c("epo2", "Ernährungspyramide 2.0"),"Ernährungspyramide 2.0", NA))))
    line_cols <- c( "darkgreen", "darkred")
    names(line_cols) <- c("Untergrenze der Unsicherheitszone", "Obergrenze der Unsicherheitszone")
  } else if (lang == "en"){
    diet_index <- ifelse(diet %in% c("sq", "Status Quo"),1, 
                         ifelse(diet %in% c("eat", "Planetary Health"),3,
                                ifelse(diet %in% c("epo", "Nutrition Pyramid"),2,
                                       ifelse(diet %in% c("epo2", "Nutrition Pyramid 2.0"),4,NA))))
    title = ifelse(diet %in% c("sq", "Status Quo"),"Status Quo", 
                   ifelse(diet %in% c("eat", "Planetary Health"),"Planetary Health Diet",
                          ifelse(diet %in% c("epo", "Nutrition Pyramid"),"Nutrition Pyramid",
                                 ifelse(diet %in% c("epo2", "Nutrition Pyramid 2.0"),"Nutrition Pyramid 2.0", NA))))
    line_cols <- c( "darkgreen", "darkred")
    names(line_cols) <- c("Lower limit of zone of unceartainty", "Upper limit of zone of unceartainty")
  }
  
  fp_circle_scen <- data.frame(ind = as.factor(names(fp_table[-1])), 
                               value = unlist(fp_table[diet_index,][-1]),
                               limit = ifelse(unlist(fp_table[diet_index,])[-1] > ifelse(log, 0, 1), "yes", "no"))
  
  
  fp_circle_scen_exp <- fp_circle_scen %>%
     rowwise() %>%
    summarise(ind = ind,
              value = list(seq(value,ylim.min, by = -0.01))) %>% # seq(value,0, by = -0.01)
    unnest(cols = value)
  
  plt <- ggplot(fp_circle_scen_exp) +
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0:(ylim.max-1))),
      color = "lightgrey",
      alpha = 0.9
    ) + 
    geom_hline(
      aes(yintercept = y), 
      data.frame(y = c(0.5:(ylim.max-1.5))),
      color = "lightgrey",
      alpha = 0.9,
      size = 0.2
    ) + 
   # geom_vline(
   #  #aes(xintercept = x), 
   #  xintercept = "Stickstoff",
   #  #data.frame(x = as.factor(names(fp_table[-1]))),
   #   color = "lightgrey",
   #   alpha = 0.9
   # ) + 
    geom_col(
      aes(
        x = ind,
        y = value,
        #group = seq(0,value, by = 0.05),
        fill = value
      ),
      #aes = 0.9,
      color = "transparent",
      position = "identity",
      show.legend = legend,
      alpha = 1,
      width = 0.95
    ) +
    geom_hline(
      aes(yintercept = y, color = ifelse(lang == "de", "Untergrenze der Unsicherheitszone", "Lower limit of zone of unceartainty")), 
      data.frame(y = c(ifelse(log, 0,1))),
      size = 0.8,
      alpha = 0.8
    ) + 
    geom_hline(
      aes(yintercept = y, color = ifelse(lang == "de", "Obergrenze der Unsicherheitszone", "Upper limit of zone of unceartainty")), 
      data.frame(y = c(ifelse(log, log(2),2))),
      size = 0.8,
      alpha = 0.8
    ) +
    # Make it circular!
    #scale_fill_manual(values = c("yes" = "red", "no" = "green")) +
    scale_fill_gradientn(colors = c("white", "green4", "chartreuse", "yellow", "orange", "red", "firebrick", "transparent"), 
                         values = scales::rescale(c(0, 0.5, 0.9, 1.1, 1.9, 2.1, 3, 4)),
                         limits = c(0, ylim.max),
                         name = "Wert relativ\nzum Grenzwert") + # "Value relative to\nplanetary boundary"
    #scale_fill_gradient2(low = "darkgreen", mid =  "yellow", high =  "firebrick", midpoint = 1.2, limits = c(0,4)) +
  
    scale_color_manual(values = line_cols) + 
  
    scale_y_continuous(
      limits = c(ylim.min, ylim.max),
      expand = c(0, 0),
      breaks = c(seq(0,1,1)) 
    ) +
    coord_polar(clip = "off")+
    labs(title = title, color = NULL) + 
    #expand_limits(y = 10) + # or some other arbitrarily large number
    theme_minimal() +
    theme(
      # Remove axis ticks and text
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      # Use gray text for the region names
      axis.text.x = element_text(color = "gray12", size = 8, face="bold"),
      # Move the legend to the bottom
      legend.position = "bottom",
      panel.grid=element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(0,1,0,1,unit = "cm"),
      legend.box.just = "center",
      legend.title = element_blank(),
      legend.title.align = 0.5,
      legend.margin=margin(0,0,0,0.5,unit = "cm")
    )
  
  return(plt)
  
}


# spiderweb/radar chart -----------------------------------
# adapted from https://rpubs.com/PaulWilliamson/5795

spiderweb <- function(plot.data,
                             axis.labels=colnames(plot.data)[-1],                             
                             grid.min=-0.5,  #10,
                             grid.mid=0,  #50,
                             grid.max=0.5,  #100,
                             grid.min.label,
                             grid.mid.label,
                             grid.max.label,
                             centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                             plot.extent.x.sf=1.2,
                             plot.extent.y.sf=1.2,
                             x.centre.range=0.02*(grid.max-centre.y),
                             label.centre.y=FALSE,
                             grid.line.width=0.5,
                             gridline.min.linetype="longdash",
                             gridline.mid.linetype="longdash",
                             gridline.max.linetype="longdash",
                             gridline.min.colour="grey",
                             gridline.mid.colour="blue",
                             gridline.max.colour="grey",
                             grid.label.size=4,
                             gridline.label.offset=-0.02*(grid.max-centre.y),
                             label.gridline.min=TRUE,
                             axis.label.offset=1.15,
                             axis.label.size=3,
                             axis.line.colour="grey",
                             axis.line.alpha = 1,
                             group.line.width=1,
                             group.point.size=4,
                             group.alpha = 0.9,
                             background.circle.colour="yellow",
                             background.circle.transparency=0.2,
                             plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                             legend.title="Cluster",
                             legend.text.size=grid.label.size ) {
  
  var.names <- colnames(plot.data)[-1]  #'Short version of variable names 
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n
  
  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf
  
  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1) 
    return("Error: 'axis.labels' contains the wrong number of axis labels") 
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")
  
  #Declare required internal functions
  
  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n
    
    path <- as.factor(as.character(df[,1]))
    
    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]
    
    for(i in levels(path)){
      
      pathData = subset(df, df[,1]==i)
      
      for(j in c(2:ncol(df))){
        
        #pathData[,j]= pathData[,j]
        
        graphData=rbind(graphData, data.frame(group=i, 
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i, 
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
      
    }
    
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    
    graphData$group <- factor(graphData$group, levels = levels(df$group))
    
    graphData #data frame returned by function
    
  }
  
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  
  
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  ### Convert supplied data into plottable format
  
  # (a) add abs(centre.y) to supplied plot data 
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  #print(group$path)
  
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  
  # (d) Create file containing axis labels + associated plotting coordinates
  
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)
  
  # (e) Create Circular grid-lines + labels
  
  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))
  
  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(ifelse(!missing(grid.min.label), grid.min.label, grid.min)))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(ifelse(!missing(grid.max.label), grid.max.label, grid.max)))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(ifelse(!missing(grid.mid.label), grid.mid.label, grid.mid)))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)
  
  
  ### Start building up the radar plot
  
  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw() + 
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"),
          legend.position = "bottom",
          axis.line=element_blank())
  
  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")
  
  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)], 
  # then centred labels for axis labels almost immediately above/below x= 0 
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly 
  # identify plot extent when plotting first (base) layer]
  
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
    scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) + 
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  
  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)
  
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)
  
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)
  
  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour,
                           alpha = axis.line.alpha)
  
  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width, alpha = group.alpha) #+
  #scale_color_discrete(breaks=levels(group))
  
  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size, alpha = group.alpha)
  
  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)
  
  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)
  
  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,fontface ="bold",size=grid.label.size, hjust=1) }
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,fontface ="bold",size=grid.label.size, hjust=1)
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,fontface ="bold",size=grid.label.size, hjust=1)
  
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,fontface ="bold",size=grid.label.size, hjust=0.5) }
  
  return(base)
  
}

#fp_spider$group




#----------------------------------------------------------------------------#
# ---- Auxiliary functions for definitions of diets  --------
#----------------------------------------------------------------------------#



dietshift_cond <- function(Y_food_dt = Y_food_aut, cond.var = "landuse", cond.reg = "AUT", diet.name, diet.suffix = "cond", 
                           rescale.var, rescale.group, fp_sq = fp_sq, x = X[,"2013"], skip.groups = NULL, tol = 1e-3,
                           add.newcols = FALSE){
  
  if(missing(cond.var)) stop("cond.var has to be specified")
  if(missing(cond.reg)) stop("cond.reg has to be specified")
  if(missing(diet.name)) stop("diet.name has to be specified")
  if(missing(diet.suffix)) stop("diet.suffix has to be specified")
  if(missing(rescale.var)) stop("rescale.var has to be specified")
  if(missing(rescale.group)) stop("rescale.group has to be specified (e.g. 'epo_subgroup'")
  
  
  # prepare data --------
  
  dt <- copy(Y_food_dt)
  
  rescaler_bygroup <- dt[,.(rescaler = unique(get(rescale.var))), by = c(rescale.group)]
  if(max(table(rescaler_bygroup[[rescale.group]])) > 1) stop("(some) rescaler groups have no unique scaling factor")
  
  
  # for increasing groups, is at least one item produced in home country?
  x <- cbind(dt[,c("area_code", "area", "area_iso", "item_code", "item", "comm_code", rescale.group, "food_t"), with = FALSE], x)
  x_reg <-  x[area_iso ==  cond.reg,]
  x_reg_pos <- x_reg[x > 0,]
  x_group <- x[, .(x = sum(x)), by = c(rescale.group, "area_iso")]
  x_group_reg <- x_group[area_iso ==  cond.reg,]
  cat("Increasing groups are ", paste(x_group_reg[get(rescale.group) %in% rescaler_bygroup[(!get(rescale.group) %in% skip.groups) & rescaler>1][[rescale.group]]][[rescale.group]], collapse = ", "), "\n")
  if(min(x_group_reg[get(rescale.group) %in% rescaler_bygroup[(!get(rescale.group) %in% skip.groups) & rescaler>1,][[rescale.group]]]$x) == 0) stop("Group(s) ", x_group_reg[get(rescale.group) %in% rescaler_bygroup[(!get(rescale.group) %in% skip.groups) & rescaler>1][[rescale.group]] & x == 0,][[rescale.group]],  " should be increased but are are produced in the target country. Adapt skip.groups or change function further if needed.")
  
  # calculate total consumption-based indicator-intensity of each product produced and consumed in target country
  fp_sq_reg <- fp_sq[country_origin == cond.reg & country_target == cond.reg & item_target %in% x_reg_pos$item,]
  fp_sq_reg <- fp_aggregate(fp_sq_reg, aggregate_by = c("item_target"))
  fp_sq_reg <- merge(fp_sq_reg, dt[area_iso == cond.reg, .(item, consumption_t_pc = food_t_pc)], by.x = "item_target", by.y = "item", all.x = TRUE, sort = FALSE)
  fp_sq_reg[, (paste0(cond.var,"_intensity_per_t")) := get(cond.var)/consumption_t_pc]
  
  
  # adapt rescaling factors to satisfy increased demand by domestic items  -----
  
  # get sum of domestic, foreign and total products in consumption per group
  dt[,`:=` (dom_group_sum = sum(food_t_pc_net[area_iso == cond.reg]),
            for_group_sum = sum(food_t_pc_net[area_iso != cond.reg]),
            group_sum = sum(food_t_pc_net)
  ), by = c(rescale.group)]
  
  all.equal(dt$group_sum, dt$dom_group_sum + dt$for_group_sum)
  
  # adapt rescaler:
  
  # for items of groups that should increase (and are not among skip.groups), adapt rescaler so that increased demand is filled by domestic products
  dt[get(rescale.var)>1 & area_iso == cond.reg & (!get(rescale.group) %in% skip.groups), (paste0(rescale.var, "_",diet.suffix)) := (get(rescale.var)*group_sum - for_group_sum)/dom_group_sum]
  # and keep foreign products at their status quo level
  dt[get(rescale.var)>1 & area_iso != cond.reg & (!get(rescale.group) %in% skip.groups), (paste0(rescale.var, "_",diet.suffix)) := 1]
  # items that should decrease or stay the same vis-a-vis status quo, or those among skip.groups keep their original rescaler
  dt[get(rescale.var)<=1 | (get(rescale.group) %in% skip.groups), (paste0(rescale.var, "_",diet.suffix)) := get(rescale.var)]
  
  # get new consumption according to updated rescaler
  dt[, (paste0(diet.name,"_t_pc")) := food_t_pc * get(rescale.var)]
  dt[, (paste0(diet.name,"_",diet.suffix,"_t_pc")) := food_t_pc * get(paste0(rescale.var, "_",diet.suffix))]
  # and check if the group-wise rescalers are still the same (as defined by the original diet change)
  rescalers_by_group <- dt[, .(#new_t_pc = sum(epo2_t_pc),
    rescaler = sum(get(paste0(diet.name,"_t_pc")))/sum(food_t_pc),
    #new_t_pc_cond = sum(epo2_t_pc_cond),
    rescaler_cond = sum(get(paste0(diet.name,"_",diet.suffix,"_t_pc")))/sum(food_t_pc)),
    by = c(rescale.group)]
  
  if(all.equal(rescalers_by_group$rescaler, rescalers_by_group$rescaler_cond, tolerance = tol) != TRUE) stop("Group-wise rescaling factors are not the same at mean relative tolerance of 1e-3. Check for correct rescale.group and function correctness")
  
  
  # check if increased demand for domestic goods keeps conditional indicator (.e.g landuse) within the target region within status quo level -------
  
  dt_reg <- merge(dt[area_iso == cond.reg,], fp_sq_reg[,c("item_target", paste0(cond.var,"_intensity_per_t")), with = FALSE], by.x = "item", by.y = "item_target", all.x = TRUE, sort = FALSE)
  setnafill(dt_reg, fill = 0, cols = paste0(cond.var,"_intensity_per_t"))
  
  if( (remaining_reserve <- sum(dt_reg$food_t_pc*dt_reg$landuse_intensity_per_t) - sum(dt_reg[[paste0(diet.name,"_",diet.suffix,"_t_pc")]]*dt_reg$landuse_intensity_per_t)) > 0){
    cat("Additional demand for increasing items can be fulfilled domestically from ", cond.var, " reserve with ", remaining_reserve, " m2 per person remaining free.")
  } else {
    
    cat("Additional demand for increasing items cannot be fulfilled exclusively domestically from ", cond.var, " reserve (excess requirement: ", -remaining_reserve, " m2 per person). The remainding demand is fulfilled with imported goods relative to their import shares.")
    
    # if demand cannot be fulfilled domestically, adapt rescaler to fill only up until limit
    reducer <- sum(dt_reg$food_t_pc*dt_reg$landuse_intensity_per_t)/sum(dt_reg[[paste0(diet.name,"_",diet.suffix,"_t_pc")]]*dt_reg$landuse_intensity_per_t)
    dt[get(rescale.var)>1 & area_iso == cond.reg, (paste0(rescale.var, "_",diet.suffix)) := get(paste0(rescale.var, "_",diet.suffix))*reducer]
    # and fill excess demand with imported products, proportionally to their import shares within each group
    dt[,`:=` (dom_group_sum_new_cond = sum(food_t_pc_net[area_iso == cond.reg]*get(paste0(rescale.var, "_",diet.suffix))[area_iso == cond.reg])), by = c(rescale.group)]
    dt[get(rescale.var)>1 & area_iso != cond.reg & group_sum > 0, (paste0(rescale.var, "_",diet.suffix)) := (get(rescale.var)*group_sum - dom_group_sum_new_cond)/for_group_sum]
    
    # check again if group-wise original target rescaling factors are maintained
    dt[, (paste0(diet.name,"_",diet.suffix,"_t_pc")) := food_t_pc * get(paste0(rescale.var, "_",diet.suffix))]
    
    rescalers_by_group <- dt[, .(#new_t_pc = sum(epo2_t_pc),
      rescaler = sum(get(paste0(diet.name,"_t_pc")))/sum(food_t_pc),
      rescaler_cond = sum(get(paste0(diet.name,"_",diet.suffix,"_t_pc")))/sum(food_t_pc)),
      by = c(rescale.group)]
    
    if(all.equal(rescalers_by_group$rescaler, rescalers_by_group$rescaler_cond, tolerance = 1e-3) != TRUE) stop("Group-wise rescaling factors are not the same at mean relative tolerance of 1e-3. Check for correct rescale.group and function correctness")
    
  }
  
  
  # save result -----
  
  # return initial food consumption table with new column of adapted re-scaling parameters and all required net consumption/nutrient intake values

  Y_food_dt <- merge(Y_food_dt, dt[,c("area_code", "item_code", "area", "comm_code", "item", paste0(rescale.var, "_",diet.suffix)), with = FALSE], by = c("area_code", "item_code", "area", "comm_code", "item"), all.x = TRUE, sort = FALSE)  #cbind(Y_food_dt, paste0(rescale.var, "_",diet.suffix) = dt$epo2_rescaler_sq_cond)
  
  if(add.newcols){
    rescale_cols <- c("g_pc_day_net", "kcal_pc_day_net", "prot_pc_day_net", "fat_pc_day_net", "port_pc_day_net", "g_pc_day", "g_pc", "t_pc")
    Y_food_dt[, (paste0(diet.name,"_",diet.suffix,"_",rescale_cols)) := lapply(.SD, function(x){x*get(paste0(rescale.var, "_",diet.suffix))}),
              .SDcols = paste0("food_",rescale_cols)]
  }
  
  return(Y_food_dt)
  
}


# check domestic vs imported shares per product 
#get_sources <- function(Y_food_dt, reg = "AUT", diet, var, group, area.var = "area_iso", mult.fact = 1){
#
#  if(diet == "sq") diet <- "food"
#  
#    Y_food_dt <- copy(Y_food_dt)[,.(dom_group_sum = sum(get(paste0(diet,"_",var))[get(area.var) == reg])*mult.fact,
#                                  for_group_sum = sum(get(paste0(diet,"_",var))[get(area.var) != reg])*mult.fact,
#                                  group_sum = sum(get(paste0(diet,"_",var))*mult.fact)
#                          ), by = c(group)]
#    
#    Y_food_dt[, `:=` (dom_group_share = dom_group_sum/group_sum,
#                      for_group_share = for_group_sum/group_sum)]
#  
#}


get_sources <- function(Y_food_dt, reg = "AUT", var, group, area.var = "area_iso", mult.fact = 1){
  
  #if(diet == "sq") diet <- "food"
  
  Y_food_dt <- copy(Y_food_dt)[,.(dom_group_sum = sum(get(var)[get(area.var) == reg])*mult.fact,
                                  for_group_sum = sum(get(var)[get(area.var) != reg])*mult.fact,
                                  group_sum = sum(get(var)*mult.fact)
  ), by = c(group)]
  
  Y_food_dt[, `:=` (dom_group_share = dom_group_sum/group_sum,
                    for_group_share = for_group_sum/group_sum)]
  
}

