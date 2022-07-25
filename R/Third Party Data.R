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
