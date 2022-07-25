library(TheSource)
library(data.table)
library(openxlsx)


### GPS Files
dt = 1 # sec
gps.files = list.files('VIPF_LOGS/GPS Publisher/', full.names = T)

## Setup first file
gps = fread(gps.files[1], skip = 1)
colnames(gps) = c('Time', 'Message', 'datetime', 'Latitude', 'Hemisphere',
                  'Longitude', 'other hemi',
                  'v8', 'v9', 'v10', 'v11', 'v12', 'v13', 'v14','v15','vq6','v17','v18')


## Load rest of files
for (i in 2:length(gps.files)) {
  temp = fread(gps.files[i], skip = 1)
  
  ## 
  if (ncol(temp) == ncol(gps)) {
    colnames(temp) = colnames(gps)
    gps = rbind(gps, temp)
  }
}

## Fix structure and pull out good data only
gps$Latitude = floor(gps$Latitude/100) + (gps$Latitude - floor(gps$Latitude/100)*100)/60
gps$Longitude = -1 * (floor(gps$Longitude/100) + (gps$Longitude - floor(gps$Longitude/100)*100)/60)
gps$Time = as.POSIXct(round(gps$Time), origin = make.time(1904))
gps = gps[seq(1, nrow(gps), by = dt),]
gps = gps[,c(1,4,6)]
summary(gps)


## Break at time gaps
k = which(diff(gps$Time) > 3600)

dpi = list(
  dpi1 = gps[1:k[1],]
)

for (i in 2:length(k)) {
  dpi[[paste0('dpi', i)]] = gps[(k[i-1]+1):k[i],]  
}

dpi[[paste0('dpi', i+1)]] = gps[(k[i]+1):nrow(gps),] 


for (i in 1:length(dpi)) {
  message(i, '\t', nrow(dpi[[i]]))
}

dpi = list(dpi11 = dpi$dpi11)


#### CTD
## Initial setup
ctd.files = list.files('VIPF_LOGS/CTD 1 Publisher/', full.names = T)
ctd = fread(ctd.files[1], skip = 1)

## Read in rest of data
for (i in 2:length(ctd.files)) {
  ctd = rbind(ctd, fread(ctd.files[i], skip = 1))
}

colnames(ctd) = c('Time', 'Temperature', 'Conductivity', 'Pressue', 'Depth', 'Salinity', 'Sound.Velocity')
ctd = as.data.frame(ctd)

## Setup time
ctd$Time = round(ctd$Time/dt)*dt
ctd$Time = as.POSIXct(ctd$Time, origin = make.time(1904))

## Bin and interpolate
for (i in 1:length(dpi)) {
  message(i)
  dpi[[i]]$Temperature = approx(as.numeric(ctd$Time), ctd$Temperature, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$Conductivity = approx(as.numeric(ctd$Time), ctd$Conductivity, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$Pressure = approx(as.numeric(ctd$Time), ctd$Pressue, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$Depth = approx(as.numeric(ctd$Time), ctd$Depth, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$Salinity = approx(as.numeric(ctd$Time), ctd$Salinity, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$Sound.Velocity = approx(as.numeric(ctd$Time), ctd$Sound.Velocity, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
}



#### Analog
## Set up format
ad.files = list.files('VIPF_LOGS/AD1216 Publisher/', full.names = T)

ad = fread(ad.files[1])
colnames(ad) = c('Time', 'Oxygen', 'PAR', 'pH', 'REDOX', 'Checksum', 'checksum2')

## Load rest of files
for (i in 2:length(ad.files)) {
  temp = fread(ad.files[i], skip = 1)
  colnames(temp) = colnames(ad)
  
  ad = rbind(ad, temp)
}

## Fix time
ad$Time = round(ad$Time/dt)*dt
ad$Time = as.POSIXct(ad$Time, origin = make.time(1904))

## Bin and interpolate
for (i in 1:length(dpi)) {
  message(i)
  dpi[[i]]$Oxygen = approx(as.numeric(ad$Time), ad$Oxygen, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$PAR = approx(as.numeric(ad$Time), ad$PAR, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$pH = approx(as.numeric(ad$Time), ad$pH, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$REDOX = approx(as.numeric(ad$Time), ad$REDOX, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
}


#### Fluorometer
fl.files = list.files('VIPF_LOGS/Fluorometer 1 Publisher/', full.names = T)

fl = fread(fl.files[1], skip = 1)
for (i in 2:length(fl.files)) {
  fl = rbind(fl, fread(fl.files[i], skip = 1))
}
colnames(fl) = c('Time', 'Chl.wave', 'Chl', 'FDOm.wave', 'FDOM', 'Phycocyanin.wave', 'Phycocyanin')

fl$Time = round(fl$Time)
fl$Time = as.POSIXct(fl$Time, origin = make.time(1904))

for (i in 1:length(dpi)) {
  message(i)
  dpi[[i]]$Chl = approx(as.numeric(fl$Time), fl$Chl, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$FDOM = approx(as.numeric(fl$Time), fl$FDOM, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
  dpi[[i]]$Phycocyanin = approx(as.numeric(fl$Time), fl$Phycocyanin, xout = as.numeric(dpi[[i]]$Time), ties = mean)$y
}


#### Eng
eng.files = list.files('VIPF_LOGS/Inclinometer Publisher/', full.names = T)[-30][-49][-49]

eng = fread(eng.files[1], skip = 1)
for (i in 2:length(eng.files)) {
  if (file.size(eng.files[i]) > 5e3) {
    eng = rbind(eng, fread(eng.files[i], skip = 1))
  }
}
colnames(eng) = c('Time', 'AccelerationX', 'AccelerationY', 'AccelerationZ', 'Roll', 'Pitch', 'T', 'Check1', 'Check3', 'Check2')

eng$Time = round(eng$Time / dt) * dt
eng$Time = as.POSIXct(eng$Time, origin = make.time(1904))

gps$AccelerationX = approx(as.numeric(eng$Time), eng$AccelerationX, xout = as.numeric(gps$Time), ties = mean)$y
gps$AccelerationY = approx(as.numeric(eng$Time), eng$AccelerationY, xout = as.numeric(gps$Time), ties = mean)$y
gps$AccelerationZ = approx(as.numeric(eng$Time), eng$AccelerationZ, xout = as.numeric(gps$Time), ties = mean)$y
gps$Roll = approx(as.numeric(eng$Time), eng$Roll, xout = as.numeric(gps$Time), ties = mean)$y
gps$Pitch = approx(as.numeric(eng$Time), eng$Pitch, xout = as.numeric(gps$Time), ties = mean)$y


#### ACS
acs.files = list.files('Data/DPI/ACS/', pattern = '.dat')


#### output


if (F) {
  l = which(diff(gps$Time) > 10)
  l = l + 1
  for (i in 1:length(l)) {
    if (i == 1) {
      k = c(1:l[1])
    } else if (i == length(l)) {
      k = c(l[i]:nrow(gps))
    } else {
      k = c((l[i-1]-1):l[i])
    }
    #write.csv(gps[k,], row.names = F, col.names = T, file = paste0('Data/DPI/Compiled_output/compiled output ', as.numeric(gps$Time[k[1]]), '.csv'))
    write.xlsx(gps[k,], file = paste0('Data/DPI2/compiled output ', as.numeric(gps$Time[k[1]]), '.xlsx'))
  }
}

## LISST
lisst = read.csv('Data/DPI/LISST/SKQ202110S .csv', header = F)
lisst$datetime = make.time(year = lisst$V43, month = lisst$V44, day = lisst$V45, hour = lisst$V46, minute = lisst$V47, second = lisst$V48, tz = 'UTC')
lisst = lisst[seq(1, nrow(lisst), by = 10),]

lisst$m = apply(lisst, 1, function(x) {
  coef(lm(as.numeric(x[1:36]) ~ c(1:36), weights = as.numeric(x[1:32])))[2]
})
lisst$depth = lisst$V41

write.xlsx(gps, file = 'Data/DPI2/compiled output.xlsx')

m = build.section(lisst$datetime, lisst$depth, lisst$V50, x.factor = 1e2, nx = 100, ny = 100, uncertainty = 10)
plot.section(m, pal = 'cubicl', ylim = c(150, 0), main = 'Mean Particle Size (um)')

ppm = build.section(lisst$datetime, lisst$depth, log10(lisst$V51), x.factor = 1e2, nx = 100, ny = 100, uncertainty = 10)
plot.section(ppm, pal = 'cubicl', ylim = c(150, 0), zlim = c(-1, 1), main = 'Particle Concentration (ppm)')

pc = prcomp(x = lisst[,7:30], center = T, scale. = T)
lisst$pc1 = pc$x[,1]
lisst$pc2 = pc$x[,2]
lisst$pc3 = pc$x[,3]

pc1 = build.section(lisst$datetime, lisst$depth, lisst$pc1, x.factor = 1e2, nx = 100, ny = 100, uncertainty = 10)
pc2 = build.section(lisst$datetime, lisst$depth, lisst$pc2, x.factor = 1e2, nx = 100, ny = 100, uncertainty = 10)
pc3 = build.section(lisst$datetime, lisst$depth, lisst$pc3, x.factor = 1e2, nx = 100, ny = 100, uncertainty = 10)

plot.section(pc1, pal = 'cubicl', ylim = c(150, 0), main = 'PC1', zlim = c(-5, 5))
plot.section(pc2, pal = 'cubicl', ylim = c(150, 0), main = 'PC2', zlim = c(-5, 5))
plot.section(pc3, pal = 'cubicl', ylim = c(150, 0), main = 'PC3', zlim = c(-5, 5))


save(gps, file = '_rdata/SKQ202110S DPI Data MID.rdata')
load('_rdata/SKQ202110S DPI Data MID.rdata')



#### Plotting

#gps = gps[which(diff(gps$Pressure) < 0),]
section = list()

for (i in 1:length(dpi)) {
  message(i)
  section[[i]] = build.section(x = dpi[[i]]$Latitude,
                                              y = dpi[[i]]$Depth,  
                                              z = dpi[[i]]$Temperature,
                                              field.names = 'Temperature',
                                              #z = dpi[[i]][,c('Temperature', 'Conductivity', 'Chl', 'PAR')],
                                              #field.names = c('Temperature', 'Conductivity', 'Chl', 'PAR'),
                                              ylim = c(0,200),
                                              nx = 50,
                                              ny = 100,
                                              y.factor = 100,
                                              uncertainty = 10,
                                              lat = dpi[[i]]$Latitude,
                                              lon = dpi[[i]]$Longitude,
                                              gridder = gridBin,
                                              verbose = F)
}

plot.section(section[[1]],
             pal = 'inferno',
             ylim = c(150,10),
             mark.points = T,
             xlim = c(59.2, 59.7),
             include.cex = 0.2,
             ylab = 'Depth',
             xlab = 'Latitude')
add.section.contour(section[[1]], levels = pretty(range(section[[1]]$grid$Temperature, na.rm = T)))
add.section.bathy(section[[1]], bathy.pacific)

plot.section(section, field = 5, pal = 'inferno', ylim = c(80,5), xlim = c(59.7, 59.9),
             mark.points = T, include.cex = 0.1)

points(section$data$x, section$data$y, col = make.pal(c(0, diff(section$data$y)), pal = pals::kovesi.diverging_linear_bjr_30_55_c53, min = -0.1, max = 0.1))
add.section.contour(section, field = 5, col = 'white')


## map if
map = make.map('coastlineWorldFine', lon.min = -150, lon.max = -145, lat.min = 57.8, lat.max = 60.3,
               dlon = 1, dlat = 1, land.col = '#333333')
#add.map.bathy.shade(map, bathy.global, pal = 'ocean.deep', zlim = c(-5e3, -100))
#redraw.map(map)
add.map.points(gps$Longitude, gps$Latitude)

