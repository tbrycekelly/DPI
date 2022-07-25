
## Input Information
input.dir = 'Z:/Data/DPI/' # Directory that it is on my system
frame.rate = 1/24
output.dir = tempdir() ## Can be anything, here we just ask R to make a temporary folder for us.
compression.level = 0 ## [0-100] Sets compression level of pngs. Larger value results in smaller filer but more compression/decompression time. 



## Automated script:

input.file = list.files(input.dir, '*.avi', full.names = T, recursive = T)

for (i in 1:length(input.file)) {
  ## Parse name
  output.str = gsub('.avi', '', input.file[i])
  output.str = strsplit(output.str, '/')[[1]]
  output.str = output.str[length(output.str)]
  
  message('Stating to processes file ', i, ' of ', length(input.file), ': ', output.str, ' (', round(file.size(input.file[i])/ 1e9, 2), ' GB)...')
  
  index = strsplit(output.str, split = '-|_')[[1]]
  
  ## Useful items for renaming output if desired:
  #year = as.numeric(index[length(index)-3])
  #month = as.numeric(index[length(index)-5])
  #day = as.numeric(index[length(index)-4])
  #hour = as.numeric(index[length(index)-2])
  #minute = as.numeric(index[length(index)-1])
  #second = as.numeric(index[length(index)])
  #datetime = as.POSIXct(paste0('20', year,'-', month, '-', day, ' ', hour, ':', minute, ':', second), tz = 'UTC')
  
  ## Start forming output png's
  call = paste0('./R/bin/ffmpeg.exe -y -i ', input.file[i], ' -compression_level 2 ', output.dir, '\\', output.str, '-%04d.png')
  ret = system(call)
}

browseURL(output.dir)
message('Done!')