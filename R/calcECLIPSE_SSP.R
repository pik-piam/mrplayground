#' @importFrom dplyr group_by_ summarise_ ungroup mutate_ rename_ filter_ select_
#' @importFrom magclass as.magpie getCells getSets<- getNames<- getSets getRegions<- mselect<- setNames write.magpie
#' @importFrom tidyr gather_
#' @importFrom utils read.csv read.csv2
#' @importFrom quitte as.quitte



calcECLIPSE_SSP <- function(subtype) {
  
  if (!(subtype %in% c("emission_factors", "emissions","activities"))) stop('subtype must be in c("emission_factors", "emissions","activities")')
  
  #-- INITIALISATION ----------------
  vcat(2,">> Initialization...\n")
  # local functions
  allocate_c2r_ef <- function(id_ef, ip_region, ip_country, ip_year, ip_scenario) {
    dummy                   <- id_ef[ip_region, ip_year, ip_scenario]             
    dummy[,,]               <- setCells(id_ef[ip_country, ip_year, ip_scenario], "GLO")
    #names(dimnames(dummy))  <- c("region", "years", "data1.data2.species.scenario")
    
    return(dummy)
  }
  
  allocate_min2r_ef <- function(id_ef, ip_region, ip_countryGroup, ip_year, ip_scenario) {
    
    dummy <- id_ef[ip_region, ip_year, ip_scenario]
    
    # Get minimum values across country group
    tmp <- as.quitte(id_ef[ip_countryGroup,ip_year,ip_scenario]) %>%    
      group_by_(~sector,~variable) %>% 
      summarise_(value=~ifelse(all(value == 0) , 0, min(value[value >0 ],na.rm= TRUE))) %>%  # a value 0 is often a sign for a NA that has been replaced with 0 for small countries
      ungroup() %>% 
      as.data.frame() %>% 
      as.quitte() %>% 
      as.magpie()
    
    # Allocate minimum values to region
    dummy[ip_region, ip_year, ip_scenario] <- setYears(tmp)
    
    return(dummy)
  }
  
  # user-defined parameters
  time     <- c(seq(2005,2055,5), seq(2060,2110,10), 2130, 2150)
  scenario <- c("SSP1","SSP2","SSP3","SSP4","SSP5","FLE", "MFR", "CLE", "MFR_Transports", "GlobalEURO6", "FLE_building_transport", "SLCF_building_transport") # These are additional scenarios to the CLE and MFR 
  
  p_dagg_year <- 2005
  p_dagg_pop  <- "pop_SSP2"
  p_dagg_gdp  <- "gdp_SSP2"

  p_countryCategories <- "useGAINSregions"
  p_dagg_map  <- "regionmappingGAINS.csv"
  
  # list of OECD countries
  #TODO: may want to place this in a mapping file or in a R library
  r_oecd <- c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", 
              "JPN", "KOR", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")
  
  # set of sectors for which no emission factor will be computed (because there is no activity reported, or not in terms of energy)  
  dimSector_skipEF = c("AACID", "CEMENT", "CHEM", "CHEMBULK", "CUSM", "NACID", "PAPER", "STEEL",
                       "Losses_Coal", "Losses_Distribution_Use", "Losses_Vent_Flare",
                       "Transformations_Coal", "Transformations_HLF", "Transformations_HLF_Refinery", "Transformations_LLF", "Transformations_NatGas")
  map_regions  <- read.csv2(toolGetMapping(type = "regional", name = p_dagg_map, returnPathOnly = TRUE),
                            stringsAsFactors = TRUE)[,c(2,3)]
  map_regions  <- map_regions %>%  
    filter_(~CountryCode != "ANT") %>% # Remove Netherland Antilles (not in REMIND regional mapping)
    filter_(~RegionCode != "") %>% 
    mutate_(RegionCode = ~gsub("\\ \\+", "\\+", gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", RegionCode)))) %>% 
    mutate_(CountryCode = ~factor(CountryCode))
  
  # Regional selections
  # select one country pertaining to WEU (all WEU countries should have the same EF). Used for SSP scenario rules
  select_weu <- paste(map_regions[which(map_regions$RegionCode == "Western Europe")[1],1])
  
  #-- READ IN DATA ------------------
  vcat(2,">> Read-in data... \n")
  # read in data generated by calcECLIPSE
  eclipse  <- calcOutput("ECLIPSE",aggregate=FALSE)
  emissions  <- collapseNames(eclipse[,,"emissions"])
  ef_eclipse <- collapseNames(eclipse[,,"ef_eclipse"])
  
  # read in population and GDP data. required to compute gdp per cap
  pop <- calcOutput("Population",aggregate=FALSE)[,p_dagg_year,p_dagg_pop]
  gdp <- calcOutput("GDPppp",    aggregate=FALSE)[,p_dagg_year,p_dagg_gdp]
  
  # calculate gdp per capita
  gdp_cap <- gdp/pop
  gdp_cap[is.na(gdp_cap)]   <- 0       # set NA to 0
 
  # define exogenous emission data
  emissions_exogenous <- emissions[,,dimSector_skipEF]
  
  # make output dummy "ef" and "emi" which then has to be filled by the data
  ef <- do.call('mbind', 
                lapply(scenario, 
                       function(s) {new.magpie(getRegions(ef_eclipse), 
                                               c(2005,2010,2030,2050,2100), 
                                               gsub("CLE", s, getNames(ef_eclipse[,,"CLE"])))
                       }))
  
  emi <- do.call('mbind', 
                 lapply(scenario, 
                        function(s) {new.magpie(getRegions(emissions_exogenous), 
                                                c(2005,2010,2030,2050,2100), 
                                                gsub("CLE", s, getNames(emissions_exogenous[,,"CLE"])))
                        }))
  
  # define country categories
  if (p_countryCategories == "perCountry") {
    # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
    r_L        <- dimnames(gdp_cap[getRegions(ef),,])$ISO3[which(gdp_cap[getRegions(ef),,] <= 2750)]
    # high and medium income countries
    r_HM       <- setdiff(getRegions(ef), r_L)
    # High-Medium income countries with strong pollution policies in place 
    r_HMStrong <- c("AUS", "CAN", "USA","JPN")                       # FIXME which definition???
    # High-Medium income countries with lower emissions goals
    r_HMRest   <- setdiff(r_HM,r_HMStrong)
  } else {
    # Compute mean GDP/Cap per GAINS region
    regionMean_gdppcap <- sapply(unique(map_regions$RegionCode), function(x) {mean(gdp_cap[map_regions$CountryCode[map_regions$RegionCode == x],,])})
    
    # low income countries (using World Bank definition < 2750 US$(2010)/Cap)
    r_L        <- map_regions$CountryCode[map_regions$RegionCode %in% names(regionMean_gdppcap[regionMean_gdppcap <= 2750])]
    # high and medium income countries
    r_HM       <- setdiff(getRegions(ef), r_L)
    # High-Medium income countries with strong pollution policies in place 
    r_HMStrong <- map_regions$CountryCode[map_regions$RegionCode %in% c("Western Europe", "Japan")]   # FIXME definition taken from JeS matlab script
    # High-Medium income countries with lower emissions goals
    r_HMRest   <- setdiff(r_HM,r_HMStrong)
  }
  
  
  # generate FLE and SSP scenarios
  # -------- Fix all scenarios to CLE in 2005 and 2010 ----------
  ef[,c(2005,2010),]  <- ef_eclipse[,c(2005,2010),"CLE"]
  emi[,c(2005,2010),] <- emissions_exogenous[,c(2005,2010),"CLE"]
  
  # ---------------- FLE ----------------------------------------
  # FLE: CLE 2010 emission factors and emissions are held constant
  ef[,,"FLE"]  <- setYears(ef[,2010,"FLE"], NULL)     # NULL is actually the default value, skipping afterwards
  emi[,,"FLE"] <- setYears(emi[,2010,"FLE"], NULL)
  
  # ---------------- SSP1 ---------------------------------------
  # Emission factors
  # low income countries   
  ef[r_L,2030,"SSP1"]   <- ef_eclipse[r_L, 2030, "CLE"]                                                          # 2030: CLE30
  ef[r_L,2050,"SSP1"]   <- pmin(setYears(ef[r_L, 2030, "SSP1"]), 
                                setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))             # 2050: CLE30 WEU, if not higher than 2030 value
  ef[r_L,2100,"SSP1"]   <- pmin(setYears(ef[r_L, 2050, "SSP1"]), setYears(ef_eclipse[r_L, 2030, "SLE"]))         # 2100: SLE30, if not higher than 2050 value
  # high income countries 
  ef[r_HM,2030,"SSP1"]  <- 0.75 * ef_eclipse[r_HM, 2030, "CLE"]                                                  # 2030: 75% of CLE30
  ef[r_HM,2050,"SSP1"]  <- pmin(setYears(ef[r_HM,  2030, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "SLE"]))      # 2050: SLE30, if not higher than 2030 value 
  ef[r_HM,2100,"SSP1"]  <- pmin(setYears(ef[r_HM,  2050, "SSP1"]), setYears(ef_eclipse[r_HM, 2030, "MFR"]))      # 2100: MFR, if not higher than 2050 value
  
  # Emissions
  # low income countries   
  emi[r_L,2030,"SSP1"]   <- emissions_exogenous[r_L, 2030, "CLE"]                                                           # 2030: CLE30
  emi[r_L,2050,"SSP1"]   <- pmin(setYears(emi[r_L, 2030, "SSP1"]), setYears(0.5*emissions_exogenous[r_L, 2030, "CLE"] 
                                                                          + 0.5*emissions_exogenous[r_L, 2030, "SLE"]))     # 2050: CLE30 WEU, if not higher than 2030 value
  emi[r_L,2100,"SSP1"]   <- pmin(setYears(emi[r_L, 2050, "SSP1"]), setYears(emissions_exogenous[r_L, 2030, "SLE"]))         # 2100: SLE30, if not higher than 2050 value
  # high income countries 
  emi[r_HM,2030,"SSP1"]  <- 0.75 * emissions_exogenous[r_HM, 2030, "CLE"]                                                   # 2030: 75% of CLE30
  emi[r_HM,2050,"SSP1"]  <- pmin(setYears(emi[r_HM,  2030, "SSP1"]), setYears(emissions_exogenous[r_HM, 2030, "SLE"]))      # 2050: SLE30, if not higher than 2030 value 
  emi[r_HM,2100,"SSP1"]  <- pmin(setYears(emi[r_HM,  2050, "SSP1"]), setYears(emissions_exogenous[r_HM, 2030, "MFR"]))      # 2100: MFR, if not higher than 2050 value
  
  # ----------------- SSP2 --------------------------------------
  # Emission factors
  # High-Medium income countries with strong pollution policies in place
  ef[r_HMStrong,2030,"SSP2"] <- ef_eclipse[r_HMStrong,2030,"CLE"]                                                # 2030: CLE30
  ef[r_HMStrong,2050,"SSP2"] <- pmin(setYears(ef[r_HMStrong,        2030,"SSP2"]),
                                     setYears(ef_eclipse[r_HMStrong,2030,"SLE"]))                                # 2050: SLE30
  ef[r_HMStrong,2100,"SSP2"] <- pmin(setYears(ef[r_HMStrong,        2050,"SSP2"]),
                                     setYears(allocate_min2r_ef(ef_eclipse, r_HMStrong, r_oecd, 2030, "SLE")))   # 2100: Lowest SLE30 or lower
  # High-Medium income countries with lower emissions goals
  ef[r_HMRest,2030,"SSP2"]  <- ef_eclipse[r_HMRest,2030,"CLE"]                                                   # 2030: CLE30
  ef[r_HMRest,2050,"SSP2"]  <- pmin(setYears(ef[r_HMRest,       2030,"SSP2"]),
                                    setYears(allocate_min2r_ef(ef_eclipse, r_HMRest, r_HMRest, 2030, "CLE")))    # 2050: Min CLE30
  ef[r_HMRest,2100,"SSP2"]  <- pmin(setYears(ef[r_HMRest,2050,"SSP2"]),
                                    setYears(allocate_c2r_ef(ef_eclipse, r_HMRest, select_weu, 2030, "SLE")))    # 2100: SLE30 WEU  
  # low income countries
  ef[r_L,2030,"SSP2"]       <- setYears(ef_eclipse[r_L, 2020, "CLE"])                                            # 2030: CLE20
  ef[r_L,2050,"SSP2"]       <- pmin(setYears(ef[r_L,       2030,"SSP2"]),
                                    setYears(allocate_min2r_ef(ef_eclipse, r_L, r_L, 2030, "CLE")))              # 2050: Min CLE30
  ef[r_L,2100,"SSP2"]       <- pmin(setYears(ef[r_L,2050,"SSP2"]),
                                    setYears(allocate_c2r_ef(ef_eclipse, r_L, select_weu, 2030, "CLE")))         # 2100: CLE30 WEU
  
  # Emissions
  # High-Medium income countries with strong pollution policies in place
  emi[r_HMStrong,2030,"SSP2"] <- emissions_exogenous[r_HMStrong,2030,"CLE"]                                               # 2030: CLE30
  emi[r_HMStrong,2050,"SSP2"] <- pmin(setYears(emi[r_HMStrong,        2030,"SSP2"]),
                                     setYears(emissions_exogenous[r_HMStrong,2030,"SLE"]))                                # 2050: SLE30
  emi[r_HMStrong,2100,"SSP2"] <- pmin(setYears(emi[r_HMStrong,        2050,"SSP2"]),
                                      setYears(emissions_exogenous[r_HMStrong,2030,"SLE"]*0.8))                           # 2100: Lowest SLE30 or lower -> 0.8*SLE30
  # High-Medium income countries with lower emissions goals
  emi[r_HMRest,2030,"SSP2"]  <- emissions_exogenous[r_HMRest,2030,"CLE"]                                                  # 2030: CLE30
  emi[r_HMRest,2050,"SSP2"]  <- pmin(setYears(emi[r_HMRest,       2030,"SSP2"]),
                                     setYears(emissions_exogenous[r_HMRest,2030,"SLE"]))                                  # 2050: Min CLE30 -> SLE30
  emi[r_HMRest,2100,"SSP2"]  <- pmin(setYears(emi[r_HMRest,2050,"SSP2"]),
                                     setYears(emissions_exogenous[r_HMRest,2030,"SLE"]*0.8))                              # 2100: SLE30 WEU -> 0.8*SLE30 
  # low income countries
  emi[r_L,2030,"SSP2"]       <- setYears(emissions_exogenous[r_L, 2020, "CLE"])                                           # 2030: CLE20
  emi[r_L,2050,"SSP2"]       <- pmin(setYears(emi[r_L, 2030,"SSP2"]),
                                     setYears(emissions_exogenous[r_L, 2030, "CLE"]))                                     # 2050: Min CLE30 -> CLE30
  emi[r_L,2100,"SSP2"]       <- pmin(setYears(emi[r_L,2050,"SSP2"]),
                                     setYears(emissions_exogenous[r_L, 2030, "SLE"]*0.95))                                # 2100: CLE30 WEU -> 0.95*SLE30
  # H-M-Strong:   2030 CLE30; 2050 SLE30;     2100 Lowest SLE30 or lower [EUR, JPN]                     = [3 5]
  # H-M-Rest:     2030 CLE30; 2050 Min CLE30; 2100 EUR SLE30             [CHN, LAM, MEA, ROW, RUS, USA] = [2 6 7 9 10 11]
  # Low:          2030 CLE20; 2050 Min CLE30; 2100 EUR CLE30             [AFR, IND, OAS]                = [1 4 8]
  
  # ----------------- SSP3 --------------------------------------
  # TODO
  
  # ----------------- SSP4 --------------------------------------
  # TODO
  
  # ----------------- SSP5 --------------------------------------
  # set SSP5 to the values of SSP1
  ef[,,"SSP5"]  <- ef[,,"SSP1"]     
  emi[,,"SSP5"] <- emi[,,"SSP1"] # does not really make sense...
  
  # Find occurences where the EF path is not monotonously decreasing  
#   for (kregi in getRegions(ef)) {
#     for (kdata in getNames(ef)) {
#       
#       y1 = ef[kregi,2005,kdata] %>% as.numeric()
#       y2 = ef[kregi,2010,kdata] %>% as.numeric()
#       y3 = ef[kregi,2030,kdata] %>% as.numeric()
#       y4 = ef[kregi,2050,kdata] %>% as.numeric()
#       y5 = ef[kregi,2100,kdata] %>% as.numeric()
#       
#       if (y1 > y2 || y2 > y3 || y3 > y4 || y4 > y5) {
#         print(paste0(kregi, ": ", kdata))
#       }
#     }
#     stop()
#   }
  
  # make sure that SSP2 is always higher than SSP1 (and SSP5)
  # Takes toooooooooooooo much time (~1h30). commented out for now
#   for (kregi in getRegions(ef)) {
#     for (kssp1 in getNames(ef[,,"SSP1"])) {
#       
#       kssp2 = paste0(strsplit(kdata, ".", fixed=TRUE)[[1]][1], ".", strsplit(kdata, ".", fixed=TRUE)[[1]][2], ".SSP2")
#       
#       for (kyear in getYears(ef)) {
#         y1 = ef[kregi,kyear,kssp1] %>% as.numeric()
#         y2 = ef[kregi,kyear,kssp2] %>% as.numeric()
#         
#         if (y1 > y2) {
#           ef[kregi,kyear,kssp2] = y1
#         }
#       }
#     }
#   }
#   for (kregi in getRegions(emi)) {
#     for (kssp1 in getNames(emi[,,"SSP1"])) {
#       
#       kssp2 = paste0(strsplit(kdata, ".", fixed=TRUE)[[1]][1], ".", strsplit(kdata, ".", fixed=TRUE)[[1]][2], ".SSP2")
#       
#       for (kyear in getYears(emi)) {
#         y1 = emi[kregi,kyear,kssp1] %>% as.numeric()
#         y2 = emi[kregi,kyear,kssp2] %>% as.numeric()
#         
#         if (y1 > y2) {
#           emi[kregi,kyear,kssp2] = y1
#         }
#       }
#     }
#   }
  
  # filter all regions and sectors that are constant between 2030 and 2050 and continue to decline afterwards. Replace by linear interpolation
  # between 2030 and 2100
  
  # Retrieve Transport and buildings names
  transportNames <- getNames(ef,dim=1)[grepl("End_Use_Transport", getNames(ef,dim=1))]
  buildingNames  <- getNames(ef,dim=1)[grepl("End_Use_Industry|End_Use_Residential|End_Use_Services", getNames(ef,dim=1))]
  
  
  # ----------------- CLE and MFR -------------------------------
  ef[,c(2005,2010,2030,2050),c("CLE","MFR")] <- ef_eclipse[,c(2005,2010,2030,2050),c("CLE","MFR")]
  ef[,2100,c("CLE","MFR")] <- setYears(ef_eclipse[,2050,c("CLE","MFR")])                           # for 2100, take the same values as in 2050
  
  emi[,c(2005,2010,2030,2050),c("CLE","MFR")] <- emissions_exogenous[,c(2005,2010,2030,2050),c("CLE","MFR")]
  emi[,2100,c("CLE","MFR")] <- setYears(emissions_exogenous[,2050,c("CLE","MFR")])                           # for 2100, take the same values as in 2050

  # ---------------- Global EURO6 for Transports -----------------------------
  ef[,c(2030,2050,2100),"GlobalEURO6"] <- ef[,c(2030,2050,2100),"SSP2"]
  ef[,c(2030,2050,2100),"GlobalEURO6"][,,transportNames] <- setCells(ef["FRA",c(2030,2050,2100),"GlobalEURO6"][,,transportNames], "GLO")

  emi[,c(2030,2050,2100),"GlobalEURO6"] <- emi[,c(2030,2050,2100),"SSP2"]  
    
  # ---------------- MFR Transports -----------------------------
  ef[,c(2030,2050,2100),"MFR_Transports"] <- ef[,c(2030,2050,2100),"SSP2"]
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "MFR_Transports") = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "MFR")
   
  emi[,c(2030,2050,2100),"MFR_Transports"] <- emi[,c(2030,2050,2100),"SSP2"]
  
  # ---------------- FLE_building_transport ------------------------------
  ef[,c(2030,2050,2100),"FLE_building_transport"] <- ef[,c(2030,2050,2100),"SSP2"]
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data3 = "FLE_building_transport")  = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data3 = "FLE")
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "FLE_building_transport") = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = transportNames, data3 = "FLE")
  
  emi[,c(2030,2050,2100),"FLE_building_transport"] <- emi[,c(2030,2050,2100),"SSP2"]
  
  # ---------------- SLCF_building_transport ------------------------------
  ef[,c(2030,2050,2100),"SLCF_building_transport"] <- ef[,c(2030,2050,2100),"SSP2"]
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data2=c("BC", "OC"), data3 = "SLCF_building_transport")  = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = buildingNames, data2=c("BC", "OC"), data3 = "FLE")
  mselect(ef, year = c("y2030", "y2050", "y2100"), data1 = transportNames, data2=c("BC", "OC"), data3 = "SLCF_building_transport") = mselect(ef,year = c("y2030", "y2050", "y2100"), data1 = transportNames, data2=c("BC", "OC"), data3 = "FLE")
  
  emi[,c(2030,2050,2100),"SLCF_building_transport"] <- emi[,c(2030,2050,2100),"SSP2"]
  
  # ----- EFs for advanced coal and biomass technologies -------
  map_sectors_ECLIPSE2REMIND <- read.csv(toolGetMapping(type = "sectoral",
                                                        name = "mappingECLIPSEtoREMINDsectors.csv",
                                                        returnPathOnly = TRUE),
                                         stringsAsFactors = TRUE)
  mapsec <- map_sectors_ECLIPSE2REMIND[map_sectors_ECLIPSE2REMIND$eclipse %in% getNames(ef, dim=1), c(1,3)]
  ef     <- toolAggregate(ef, mapsec, dim=3.1)
  
  adv_techs     = c("igcc", "igccc", "pcc", "pco", "coalgas", "bioigcc", "bioigccc", "biogas")
  adv_coaltechs = c("igcc", "igccc", "pcc", "pco")
  adv_specs  = c("NOx", "SO2", "BC", "OC")
  adv_factor = c(0.85,   0.6,  0.6,  0.6)
  
  for (kscen in getNames(ef,  dim=6)) {
    for (ktech in adv_techs) {
      curtech = ifelse(ktech %in% adv_coaltechs, "Power_Gen_Coal.", "Power_Gen_Bio_Trad.")
      for (kspec in adv_specs)
        nm = getNames(ef[,,paste0("power.",kspec)][,,ktech][,,kscen])
      ef[,,paste0("power.",kspec)][,,ktech][,,kscen] <- setNames(
        pmin(
          mbind(lapply(getYears(ef), function(x) {setYears(ef_eclipse[,2030,paste0(curtech,kspec,".MFR")]/adv_factor[adv_specs == kspec], x)})), 
          ef_eclipse[,,paste0(curtech,kspec,".CLE")]),
        nm)
    }
  }
  
  # ----- Aggregate back to REMIND regions (to speed up processing)
  emiNam <-getNames(ef,TRUE)[2:3]
  newdim <- apply(
    sapply(
      do.call("expand.grid", emiNam),as.character),
    1,paste, collapse = ".")
  
  activities.EF <- do.call('mbind', 
                             lapply(newdim, 
                                    function(scen) {setNames(emissions, paste(getNames(emissions), scen, sep = "."))}))
  #activities.full <- time_interpolate(activities.full, interpolated_year=time, integrate=TRUE, extrapolation_type="constant")
  #activities.full <- activities.full[,time,] #remove 2000 
  
  #ef_eclipseR = toolAggregate(ef_eclipse[,,dimSector_EF], map_REMINDregions[,2:3], weight=setYears(activities[,2010,dimSector_EF]))
  #ef_remind   = toolAggregate(ef, map_REMINDregions[,2:3], weight=setYears(activities.EF[,2010,]))
  
  getSets(ef)            <- c("region", "year", "sector.species.scenario")
  getSets(activities.EF) <- c("region", "year", "sector.species.scenario")
  getSets(emi)            <- c("region", "year", "sector.species.scenario")
  
  if(subtype=="emissions") {
    x <- emi
    w <- NULL
  } else if (subtype=="emission_factors") {
    x <- ef
    w <- setYears(activities.EF[,2010,])
  } else if (subtype=="activities") {
    x <- activities.EF
    w <- NULL    # is this correct?
  } else (stop("do not know which weight to use for acrivities")) 
  
  return(list(x           = x,
              weight      = w,
              unit        = "unit",
              description = "calcECLIPSE substitute"
              ))
}
