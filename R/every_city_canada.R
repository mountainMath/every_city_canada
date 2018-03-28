#background_colour="white"
background_colour="grey92"

map_view_for_city<-function(city){
  datum=42304
  geo_data <- cancensus::get_census("CA16",regions=list(CMA=as.character(city$CMA_UID)),geo_format="sf",level="CSD")

  city_color="brown"
  rest_color="#aaaaaa"
  geo_colors=rlang::set_names(c(city_color,rep_len(rest_color,nrow(geo_data)-1)),c(city$region,setdiff(geo_data$GeoUID,city$region)))

  bbox=sf::st_bbox(geo_data)
  rmapzen::mz_set_tile_host_nextzen(getOption("nextzen_API_key"))
  mx_box=rmapzen::mz_rect(bbox$xmin,bbox$ymin,bbox$xmax,bbox$ymax)

  ucb_tiles <- rmapzen::mz_vector_tiles(mx_box)

  # vector tiles return all layers (roads, water, buildings, etc) in a list
  roads <- rmapzen::as_sf(ucb_tiles$roads) %>% dplyr::filter(kind != "ferry")
  water <- rmapzen::as_sf(ucb_tiles$water)

  # make a quick static map that includes roads and oceans as reference
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = geo_data,
            colour = "black", size = .1,
            alpha = 1,ggplot2::aes(fill=GeoUID)) +
    ggplot2::geom_sf(data = water,
            fill = "lightblue", colour = NA) +
    ggplot2::scale_fill_manual(values=geo_colors,guide=FALSE) +
    ggplot2::geom_sf(data = roads,
            size = .2, colour = "black") +
    cancensusHelpers::map_theme +
    ggplot2::coord_sf(datum=sf::st_crs(datum),
             xlim=c(bbox$xmin,bbox$xmax),
             ylim=c(bbox$ymin,bbox$ymax)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = background_colour),
          plot.background = ggplot2::element_rect(fill  = background_colour,
                                         colour = background_colour))
}

age_pyramid_for_city<-function(city){
  # styling for graph
  age_pyramid_styling <- list(
    ggplot2::scale_x_discrete(breaks=c(seq(0, 100, 5),"100+")),
    ggplot2::scale_y_continuous(labels = scales::comma),
    ggplot2::coord_flip(),
    ggplot2::scale_fill_manual(values = c("brown","lightblue")),
    ggplot2::theme_void()
  )

  # compute median age
  median_age<-function(age_data){
    total_age= age_data %>% dplyr::group_by(Age) %>% dplyr::summarize(Population=sum(abs(Population)))
    mid_pop=sum(total_age$Population)/2

    x=0
    index=0
    while(x<mid_pop && index<nrow(total_age)){
      index=index+1
      x=x+total_age$Population[index]
    }
    last_bracket=total_age$Population[index]
    over=x-mid_pop
    med_age=index-1-over/last_bracket

    med_age
  }


  age_data<-cancensusHelpers::get_age_data("CA16",cancensus::as_census_region_list(city))
  med_age<-median_age(age_data)

  ggplot2::ggplot(age_data, ggplot2::aes(x = Age, y = Population, fill = Gender)) +
    ggplot2::geom_bar(stat = "identity") +
    age_pyramid_styling +
    ggplot2::geom_vline(xintercept = med_age,size=1) +
    #annotate("text",label=format(city$pop,big.mark=","),x=95,y=min(age_data$Population)*0.9,color="black",size=7,hjust=0) +
    #annotate("text",label="Population",x=88,y=min(age_data$Population)*0.9,color="black",size=5,hjust=0) +
    ggplot2::annotate("text",label=round(med_age,1),x=95,y=max(age_data$Population)*0.9,color="black",size=6,hjust=1) +
    ggplot2::annotate("text",label="(median)",x=88,y=max(age_data$Population)*0.9,color="black",size=4,hjust=1) +
    ggplot2::guides(fill=FALSE) +
    ggplot2::labs(subtitle = 'Age distribution') +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = background_colour),
          plot.background = ggplot2::element_rect(fill  = background_colour,
                                         colour = background_colour))
}

get_2006_data<-function(city){
  inflation_2005_2015=1.1804
  census_data <- cancensus::get_census(dataset='CA06', regions=cancensus::as_census_region_list(city), vectors=c("v_CA06_2049","v_CA06_2052","v_CA06_2053","v_CA06_2057","v_CA06_1741","v_CA06_1100","v_CA06_1101","v_CA06_1248","v_CA06_1256"), labels="short", geo_format=NA, level='Regions') %>%
    dplyr::mutate(shelter=(v_CA06_2052+v_CA06_2057)/(v_CA06_2049+v_CA06_2053),
           income=v_CA06_1741*inflation_2005_2015,
           driver=v_CA06_1101/v_CA06_1100,
           bach=v_CA06_1256/v_CA06_1248) %>%
    dplyr::mutate(year=2006) %>%
    dplyr::select(GeoUID, Type, Population, name = `Region Name`, income, driver, bach, shelter, year)
  census_data
}

get_2016_data<-function(city){
  # Family income vectors
  median_fam <- "v_CA16_2447"
  fam_deciles <- cancensus::list_census_vectors("CA16") %>%
    dplyr::filter(vector == "v_CA16_2471") %>%
    cancensus::child_census_vectors(leaves_only = TRUE) %>%
    dplyr::pull(vector)

  # Commute
  commute_share <- "v_CA16_5795"
  commute_base <- "v_CA16_5792"

  # Educ
  bach_share <- "v_CA16_5123"
  bach_base <- "v_CA16_5096"

  # Shelter cost
  shelter_share <- "v_CA16_4889"
  shelter_base <- "v_CA16_4886"

  vec_list <- c(median_fam,
                commute_share, commute_base,
                bach_share, bach_base,
                shelter_base, shelter_share)

  regions <- list(CSD = city$region, CMA = as.character(city$CMA_UID))
  city_data <- cancensus::get_census("CA16", regions = regions, level = "Regions", vectors = vec_list)
  data_subset <- city_data %>%
    dplyr::mutate(income = `v_CA16_2447: Median total income of economic families in 2015 ($)`,
           driver = `v_CA16_5795: Car, truck, van - as a driver`/`v_CA16_5792: Total - Main mode of commuting for the employed labour force aged 15 years and over in private households with a usual place of work or no fixed workplace address - 25% sample data`,
           bach = `v_CA16_5123: University certificate, diploma or degree at bachelor level or above`/`v_CA16_5096: Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data`,
           shelter = `v_CA16_4889: 30% to less than 100%`/`v_CA16_4886: Total -  Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data`) %>%
    dplyr::mutate(year=2016) %>%
    dplyr::select(GeoUID, Type, Population, name = `Region Name`, income, driver, bach, shelter,year)

  data_subset
}

city_index_plot<-function(city,data_2016=NA){
  if (is.na(data_2016)) data_2016 = get_2016_data(city)
  city_index <- data_2016 %>%
    dplyr::mutate(Income = income/income[which(data_2016$Type == "CMA")],
           Driver = driver/driver[which(data_2016$Type == "CMA")],
           Bach = bach/bach[which(data_2016$Type == "CMA")],
           Shelter = shelter/shelter[which(data_2016$Type == "CMA")]) %>%
    dplyr::select(GeoUID, Type, name, Income, Driver, Bach, Shelter) %>%
    tidyr::gather(measure, index, Income:Shelter) %>%
    tidyr::spread(Type, index) %>%
    dplyr::filter(!is.na(CSD)) %>%
    dplyr::mutate(CMA =  1,
           measure = as.factor(measure))

  levels(city_index$measure) <- c("Share with Bach.\ndegree or higher",
                                  "Share drive\ncommute",
                                  "Median family\nincome",
                                  "Share with shelter\ncosts > 30%")

  city_index$measure <- factor(city_index$measure, levels(city_index$measure)[c(2,4,1,3)])

  index_plot <- ggplot2::ggplot(city_index, ggplot2::aes(y = measure, x = log2(CMA), xend = log2(CSD))) +
    ggalt::geom_dumbbell(colour_x = "black", colour_xend = "darkred",
                  size_x = 3, size_xend = 3, dot_guide = TRUE) +
    ggplot2::scale_x_continuous(breaks = c(log2(min(city_index$CMA, city_index$CSD)),  log2(max(city_index$CMA, city_index$CSD)*0.95)),
                       labels = c("Less than CMA","More than CMA")) +
    ggplot2::labs(y = "", x = "", subtitle = "Comparison against local Census Metro Area") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = background_colour),
          plot.background = ggplot2::element_rect(fill  = background_colour,
                                         colour = background_colour),
          panel.grid.major.x  =  ggplot2::element_blank(),
          panel.grid.minor.x  =  ggplot2::element_blank())
  index_plot
}

city_time_plot<-function(city,data_2016=NA,data_2006=NA){
  if (is.na(data_2016)) data_2016 = get_2016_data(city) %>% dplyr::filter(GeoUID==city$region)
  if (is.na(data_2006)) data_2006 = get_2006_data(city)
  city_time <- dplyr::bind_rows(data_2016 %>% dplyr::select(-name,Type),data_2006 %>% dplyr::select(-name,Type)) %>%
    dplyr::rename(Income = income,
           Driver = driver,
           Bach = bach,
           Shelter = shelter) %>%
    dplyr::mutate(name=data_2016$name) %>%
    dplyr::select(GeoUID, year, name, Population, Income, Driver, Bach, Shelter) %>%
    tidyr::gather(measure, index, Population:Shelter) %>%
    tidyr::spread(year, index) %>%
    dplyr::mutate(measure = as.factor(measure))

  levels(city_time$measure) <- c("Share with Bach.\ndegree or higher",
                                 "Share drive\ncommute",
                                 "Median family\nincome",
                                 "Population",
                                 "Share with shelter\ncosts > 30%")

  city_time$measure <- factor(city_time$measure, levels(city_time$measure)[c(2,5,1,3,4)])

  city_time <- city_time %>% dplyr::mutate(`2016`=`2016`/`2006`-1,`2006`=0)

  time_plot <- ggplot2::ggplot(city_time, ggplot2::aes(y = measure, x = `2006`,xend=`2016`)) +
    ggalt::geom_dumbbell(colour_x = "black", colour_xend = "darkred",
                  size_x = 3, size_xend = 3, dot_guide = TRUE) +
    ggplot2::scale_x_continuous(labels=scales::percent) +
    ggplot2::labs(y = "", x = "", subtitle = "Timeline 2006 to 2016") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = background_colour),
          plot.background = ggplot2::element_rect(fill  = background_colour,
                                         colour = background_colour),
          panel.grid.major.x  =  ggplot2::element_blank(),
          panel.grid.minor.x  =  ggplot2::element_blank())
  time_plot
}


city_incomes_histogram <- function(city){
  fam_deciles <- cancensus::list_census_vectors("CA16") %>%
    dplyr::filter(vector == "v_CA16_2471") %>%
    cancensus::child_census_vectors(leaves_only = TRUE) %>%
    dplyr::pull(vector)

  city_incomes <- cancensus::get_census("CA16", regions = list(CSD = city$region), level = "Regions", vectors = fam_deciles)

  city_incomes <- city_incomes %>%
    tidyr::gather(decile, value, 8:17)

  # Plot
  income_plot <- ggplot2::ggplot(city_incomes, ggplot2::aes(x = decile, y = value)) +
    ggplot2::geom_bar(stat = "identity", fill = "brown") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = "", y = "", subtitle = "Adjusted family income \nby deciles") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = background_colour,
                                         colour = background_colour),
          panel.background = ggplot2::element_rect(fill = background_colour),
          plot.title = ggplot2::element_text(hjust = 0.5))
  income_plot
}

#' List of eligible cities
#' @export
cities_list <- function(){
  cities_2006=cancensus::list_census_regions("CA06",use_cache = TRUE) %>% dplyr::filter(level=="CSD")
  cma_2016=cancensus::list_census_regions("CA16",use_cache = TRUE) %>% dplyr::filter(level=="CMA")
  cancensus::list_census_regions("CA16",use_cache = TRUE) %>% dplyr::filter(level=="CSD",pop>=4000,CMA_UID %in% cma_2016$region,region %in% cities_2006$region)
}

# Add data about CMA and province
city_details <- function(city) {
  province = cancensus::list_census_regions("CA16", use_cache = TRUE) %>%
    dplyr::filter(region == city$PR_UID) %>% pull(name)
  cma = cancensus::list_census_regions("CA16", use_cache = TRUE) %>%
    dplyr::filter(region == city$CMA_UID) %>% pull(name)
  cma_pop = cancensus::list_census_regions("CA16", use_cache = TRUE) %>%
    dplyr::filter(region == city$CMA_UID) %>% pull(pop)
  details = list(province = province, cma = cma, cma_pop = cma_pop)
  return(details)
}

#' Generate every city canada card for city, returns path to image file
#' @export
every_city_plot<-function(city,file_path=NA){
  if (is.na(file_path)) file_path <- tempfile(fileext= ".png")

  # Get city details
  city_deets <- city_details(city)

  # Set up Grid layout
  lay <- rbind(c(1,2,2,2,3,4,5),
               c(1,25,25,25,3,26,5),
               c(6,7,7,7,8,9,10),
               c(11,12,12,12,12,12,13),
               c(14,15,16,17,8,18,19),
               c(14,15,16,17,8,18,19),
               c(20,21,21,21,22,23,24))

  widths <- c(0.25,4.625,0.25,4.625,0.25,10.5,0.25)
  heights <- c(1,0.75,3.5,0.25,2,2,0.5)

  # Set up default background for grid elements
  rect_final<-grid::rectGrob(gp = grid::gpar(fill  = background_colour, col = background_colour))
  gs <- lapply(1:26, function(ii)
    grid::grobTree(rect_final, grid::textGrob(ii)))
  #grobTree(rectGrob(gp=gpar(fill=ii, alpha=0.5)), textGrob(ii)))
  gridExtra::grid.arrange(grobs=gs, ncol=4,
               top="top label", bottom="bottom\nlabel",
               left="left label", right="right label")
  ###

  gs[[1]] <- rect_final
  gs[[3]] <- rect_final
  gs[[4]] <- rect_final
  gs[[5]] <- rect_final
  gs[[6]] <- rect_final
  gs[[8]] <- rect_final
  gs[[10]] <- rect_final
  gs[[11]] <- rect_final
  gs[[12]] <- rect_final
  gs[[13]] <- rect_final
  gs[[14]] <- rect_final
  gs[[16]] <- rect_final
  gs[[19]] <- rect_final
  gs[[20]] <- rect_final
  gs[[22]] <- rect_final
  gs[[24]] <- rect_final
  gs[[25]] <- rect_final
  gs[[26]] <- rect_final
  #gs[[c(1,3,4,5,6,8,10,11,12,13,14,19,20,22,24)]] <- grid::rectGrob(gp = grid::gpar(fill  = "white", colour = "white"))

  gs[[2]] <- grid::grobTree(rect_final, grid::textGrob(label = paste0(city$name," (",city$municipal_status,")"), just = "left", x = 0.05, gp = grid::gpar(cex = 2, fontface = "bold")))
  gs[[25]] <- grid::grobTree(rect_final, grid::textGrob(label = paste0(city_deets$cma," (CMA), ",city_deets$province), just = "left", vjust = 0.1,  x = 0.05, gp = grid::gpar(cex = 1.25)))
  gs[[26]] <- grid::grobTree(rect_final, grid::textGrob(label = paste0(scales::percent(city$pop/city_deets$cma_pop)," of CMA total"), just = "right", vjust = 0.1,  x = 0.95, gp = grid::gpar(cex = 1.25)))
  gs[[21]] <- grid::grobTree(rect_final, grid::textGrob(label = "Made with <3 by @jens_vb and @dshkol", just = "left", x = 0.05))
  gs[[23]] <- grid::grobTree(rect_final, grid::textGrob(label = "Statistics Canada 2016 & 2006", just="right", x = 0.95))
  gs[[9]] <- city_index_plot(city)
  gs[[17]] <- city_incomes_histogram(city)
  gs[[7]] <- grid::grobTree(rect_final, ggplot2::ggplotGrob(map_view_for_city(city)))
  gs[[15]] <- age_pyramid_for_city(city)
  gs[[4]] <- grid::grobTree(rect_final, grid::textGrob(label = paste0("Population ", scales::comma(city$pop)), just = "right", x =0.95, gp = grid::gpar(cex = 1.75)))
  gs[[18]] <- city_time_plot(city)
  g<-gridExtra::arrangeGrob(grobs = gs, layout_matrix = lay, heights = heights, widths = widths)
  ggplot2::ggsave(file=file_path,g,dpi=102.4,width=10,height=5)
  file_path
}

#' Tweet out every city canada card for random city
#' @export
send_every_city_tweet<-function(city=NA,media_feedback=TRUE,tweeted_cities_path=file.path("data","tweeted_cities")){
  if (!file.exists(tweeted_cities_path)) stop("could not find tweeted cities list")
  tweeted_cities <- readRDS(file=tweeted_cities_path)
  tmp <- tempfile(fileext = ".png")
  if (is.na(city)) city <- cities_list() %>% dplyr::filter(!(region %in% tweeted_cities)) %>% sample_n(1)
  if (city$region %in% tweeted_cities) stop("already tweeted city")
  tweeted_cities <- c(tweeted_cities,city$region)
  p<-every_city_plot(city,tmp)
  if (media_feedback) browseURL(tmp)
  twitter_token=readRDS(file=file.path(getOption("custom_data_path"), "twitter_token.rds"))
  rtweet::post_tweet(status=paste0(city$name,", Population ",scales::comma(city$pop)),
             media=tmp,token=twitter_token)
  saveRDS(tweeted_cities,file=tweeted_cities_path)
  unlink(tmp)
}



#' @importFrom dplyr %>%
#' @importFrom rlang .data
NULL

