library(TheSource)
library(png)

x.length = 2048
y.length = 2048
z.length = 100

png.files = list.files('../test10/', full.names = T)
png = array(integer(1), dim = c(x.length, y.length, z.length))


mean.field = array(0, dim = c(x.length, y.length))
pb = txtProgressBar(0, length(png.files), style = 3)

for (i in 1:length(png.files)) {
  mean.field = mean.field + readPNG(source = png.files[i], info = F)[,,1] / length(png.files)
  setTxtProgressBar(pb, i)
}


for (i in c(1:z.length)) {
  message(i)
  png[,,i] = as.integer(255*readPNG(source = png.files[i], info = F)[,,1])
}

mean.field = apply(png, MARGIN = c(1,2), mean)
sd.field = apply(png, MARGIN = c(1,2), sd)


png('comparison.png', width = 1024, height = 1024)
par(mfrow = c(2,2))
image.default(png[,,1], col = greyscale(255), zlim = c(0,255))
image.default(png[,,1] - mean.field, col = greyscale(255), zlim = c(0,255))

plot.image(x = c(1:x.length),
           y = c(1:y.length),
           z = png[,,1] - mean.field*255,
           pal = 'greyscale')


plot.image(x = c(1:x.length),
           y = c(1:y.length),
           z = mean.field,
           pal = 'greyscale')


png(width = x.length, height = y.length, 'test.png')
par(plt = c(0,1,0,1))
plot.image(x = c(1:x.length),
           y = c(1:y.length),
           z = png[,,1] - mean.field*255 + 128,
           pal = 'greyscale', 
           zlim = c(0,255),
           xlab = '',
           ylab = '')
dev.off()

png(width = x.length, height = y.length, 'test original.png')
par(plt = c(0,1,0,1))
plot.image(x = c(1:x.length),
           y = c(1:y.length),
           z = png[,,1],
           pal = 'greyscale', 
           zlim = c(0,255),
           xlab = '',
           ylab = '')
dev.off()

image.default(mean.field, col = greyscale(255), zlim = c(0,1))
dev.off()





