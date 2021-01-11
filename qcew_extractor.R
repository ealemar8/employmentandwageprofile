# This function extracts multiple years (as selected 
# by user in input) of QCEW data at quarterly/annual frequency for 
# all municipalities and industries in Puerto Rico

extract_dat = function(firstyear, averages = T){
  require(blsAPI)
  # munis
  areas = read.table('https://www.bls.gov/cew/classifications/areas/area-titles-txt.txt', fill = T)
  areas$area_fips = areas$V1
  areas$area_title = with(areas, paste(V2, V3, V4, V5, V6))
  areas$statecode = substring(areas$V1, 1, 2)
  areas = areas[, c('area_fips', 'area_title', 'statecode')]
  municodes = areas[areas$statecode=='72', ]
  municodes = municodes[grep('Municipio', municodes$area_title), ] 
  #dates
  lastyear = lubridate::year(Sys.Date())-1
  previoustolast = lubridate::year(Sys.Date())-2
  munidat = function(yea, qtr){
    datmun = NULL
    for(i in 1:dim(municodes)[1]){
      out = blsQCEW('Area', year = yea, 
                    area = municodes$area_fips[i], quarter = qtr)
      datmun = rbind(datmun, out)
    }
    return(datmun)
  }
  yeardat = function(quart, yearss){
    yeard = NULL
    for(i in 1:length(yearss)){
      out = munidat(yea = yearss[i], qtr = quart)
      yeard = rbind(yeard, out)
    }
    return(yeard)
  }
  years = firstyear:previoustolast
  datyear = NULL
  if(averages == F){
    quarts = 1:4
    for(i in 1:length(quarts)){
      out = yeardat(quart = quarts[i], yearss = years)
      datyear = rbind(out, datyear)
    }} else {
      datyear = yeardat(quart = 'a')
    }
 # datrecent=NULL
 # if(lubridate::month(Sys.Date())<=3){
 #   
#  }
  datyear = merge(datyear, areas,
                  by = c('area_fips'),
                  all.x = T, all.y = F)
  datyear$area_title = str_remove(datyear$area_title,
                                  ' Municipio, Puerto Rico')
  datyear$area_title = trimws(datyear$area_title)
  return(datyear)
}

# example: extract annual averages for all munis and industries for 2015 up to most recent available quarter (having package blsAPI already installed) and name extracted data as dat:
dat = extract_dat(2015, averages = F, installed_BLSAPI = T)
