# This function extracts multiple years (as selected 
# by user in input) of QCEW data at quarterly/annual frequency for 
# all municipalities and industries in Puerto Rico

extract_qcew= function(firstyear, averages = T, agg_code){
  require(blsAPI)
  require(stringr)
  # munis
  areas = read.table('https://www.bls.gov/cew/classifications/areas/area-titles-txt.txt', fill = T)
  areas$area_fips = areas$V1
  areas$area_title = with(areas, paste(V2, V3, V4, V5, V6))
  areas$statecode = substring(areas$V1, 1, 2)
  areas = areas[, c('area_fips', 'area_title', 'statecode')]
  municodes = areas[areas$statecode=='72', ]
  municodes = municodes[grep('Municipio', municodes$area_title), ] 
  #dates
  require(lubridate)
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
      datyear = yeardat(quart = 'a', yearss = years)
    }
  datrecent = NULL #download most recent and append
  if(Sys.Date()<=paste0(year(Sys.Date()), '-', '03-09')){
    quarts = 1:2
    for(i in 1:length(quarts)){
      out = yeardat(quart = quarts[i], yearss = lastyear)
      datrecent = rbind(out, datrecent)
    } } 
  else if(Sys.Date()>paste0(year(Sys.Date()), '-', '03-09')){
    quarts = 1:3
    for(i in 1:length(quarts)){
      out = yeardat(quart = quarts[i], yearss = lastyear)
      datrecent = rbind(out, datrecent)
    }
  } else if(Sys.Date()>paste0(year(Sys.Date()), '-', '06-02')){
    quarts = 1:4
    for(i in 1:length(quarts)){
      out = yeardat(quart = quarts[i], yearss = lastyear)
      datrecent = rbind(out, datrecent)
    }
  } else if(Sys.Date()>paste0(year(Sys.Date()), '-', '09-01')){
    quarts = 1:4
    pastyeardat = NULL
    for(i in 1:length(quarts)){
      out = yeardat(quart = quarts[i], yearss = lastyear)
      pastyeardat = rbind(out, pastyeardat)
    }
    thisyeardat = yeardat(quart = 1, yearss = lubridate::year(Sys.Date()))
    datrecent = rbind(pastyeardat, thisyeardat)
  } else if(Sys.Date()>paste0(year(Sys.Date()), '-', '12-01')){
    quarts = 1:4
    pastyeardat = NULL
    for(i in 1:length(quarts)){
      out = yeardat(quart = quarts[i], yearss = lastyear)
      pastyeardat = rbind(out, pastyeardat)
    }
    qts = 1:2
    thisyeardat = NULL
    for(i in 1:length(qts)){
      out = yeardat(quart = qts[i], yearss = lubridate::year(Sys.Date()))
      thisyeardat = rbind(out, thisyeardat)
    }
    datrecent = rbind(pastyeardat, thisyeardat)
  }
  datyear = rbind(datyear, datrecent)
  datyear = merge(datyear, areas,
                  by = c('area_fips'),
                  all.x = T, all.y = F)
  datyear$area_title = str_remove(datyear$area_title,
                                  ' Municipio, Puerto Rico')
  datyear$area_title = trimws(datyear$area_title)
  datyear = datyear[datyear$agglvl_code==agg_code, ]
  return(datyear)
}

