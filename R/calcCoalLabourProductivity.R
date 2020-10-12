#' CalcCoalLabourproductivity
#' @author Aman Malik
#' @param subtype Either "Labour productivity" or "Employment"
#' @importFrom dplyr left_join

calcCoalLabourProductivity <- function(subtype){
  Year <- NULL
  Variable.y <- NULL
  Value.y <- NULL
  Value.x <- NULL
  
  emp <- calcOutput("ILO",subtype="all",aggregate = F)[,,"Coal and Lignite"]
  dias <- readSource("Dias", subtype = "Employment")
  dias <- dias[,,"direct.Fuel_supply.Coal"]
  regs <- setdiff(getRegions(dias)[which(dias>0)],c("HUN","ITA","GBR")) #countries in Europe with coal employment
  prod <- readSource("BP","Production") # coal production from BP data
  prod <- prod[,,c("Coal_EJ","Coal_Ton")]
  prod["ZAF","y2019","Coal_Ton"] <- 258.5 
  # eur <- dimSums(prod[regs,,c("Coal_Ton","Coal_EJ")],dim = 1)# since EUR is not a region in BP, summing over all countries with coal production
  # getRegions(eur) <- "EUR" # making a region EUR
  # prod <- mbind(prod,eur)# adding it original data
  
  # Employment in coal mining from various local (not ILO) sources
  usa_loc <- new.magpie("USA",years = c(2012:2019),fill = c(84.65,78.1,73.3,64.05,50.73, 51.5,	51.59,	51.89),names = "Employment")
  usa_loc <- usa_loc*1000 # source: BLS
  ind_loc <- new.magpie("IND",years = c(2019:2010),fill = c(292118,304386,	316210,	327750,	339867,	352282,	364736,	377447,	390243,	404744),names = "Coal")
  ind_loc <- ind_loc/0.8 # the above numbers are only for Coal India Limited which produces almost 80% of Indian coal. 
  ind_loc <- magpiesort(ind_loc)# source: CIL operational statistics
  zaf_loc <- new.magpie("ZAF",years = c(2009:2019),fill=c(70791,74025,78580,83244,88039,86106,77747,77259,82372,89647,94297),names = "Employment")# source mining south africa
  aus_loc <- new.magpie("AUS",c(2000:2019),fill=c(17.1,	20.6,	15,	20.4,	19.4,	26.9,	28.9	,23.2,	29,	32.4,	36,	47.6,	60,	46.4,	58,	38.7,	42.9,	47.3,	53.8,	57.9),names = "Employment")
  aus_loc <- aus_loc*1000 # https://nationalindustryinsights.aisc.net.au/industries/mining-drilling-and-civil-infrastructure/coal-mining
  chn_loc <- new.magpie("CHN",years = c(2000,2005,2010,2015,2018),fill = c(3.99,4.36,5.27,4.43,3.21),names = "Employment") # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7442150/
  chn_loc <- chn_loc*1000000
  rus_prod <- new.magpie("RUS",years = c(1995,1997,1999,2001,2003,2005,2008,2009,2011,2012),fill = c(237,306, 449,563, 648,726,1151,1174,1350,1439))# from Rutovitz 2015
  rus_loc <- prod["RUS",getYears(rus_prod),"Coal_Ton"]*1000000/rus_prod
  
  all <- bind_rows(as.data.frame(usa_loc),as.data.frame(ind_loc),as.data.frame(zaf_loc),as.data.frame(aus_loc),
                   as.data.frame(chn_loc),as.data.frame(emp["IDN",c(2012:2015),"Coal and Lignite"]*1000),
                   as.data.frame(emp[regs,c(2008:2019),]*1000),as.data.frame(rus_loc)) %>% 
    select(-1) %>% 
    rename(Variable=3) %>% 
    mutate(Variable= "Coal") %>% 
    mutate(Variable="Employment") %>%
    mutate(Year=as.integer(Year))
  
  prod_df <- as.data.frame(prod) %>% select(-1) %>% rename(Variable=3) %>% mutate(Year=as.integer(as.character(Year)))
  
  all <- left_join(x = all,y = prod_df,by = c("Region","Year")) %>% filter(Variable.y=="Coal_Ton") %>% mutate(Lp=(Value.y*1000000)/Value.x)
  
  if (subtype=="Labour productivity")
  {
    all <- all %>% select(1,2,7)
    x <- as.magpie(all,spatial=1,temporal=2)
    x <- toolCountryFill(x,fill=0)
    x[is.na(x)] <- 0
    wt <- readSource("BP","Production")[,getYears(x),"Coal_Ton"]
   
    return(list(
      x=x,
      weight=wt,
      unit="000s",
      description="Labour productivity in coal mining sector in Tons/Person"))
  }
  if (subtype=="Employment"){
    all <- all %>% select(1,2,4)
    x <- as.magpie(all,spatial=1,temporal=2)
    x <- collapseNames(x = x,collapsedim = 2)
    x <- toolCountryFill(x,fill=0)
    x[is.na(x)] <- 0
    return(list(
      x=x,
      weight=NULL,
      unit="000s",
      description="Total Employment in coal and lignite sector"))
  }

  
}
    
 