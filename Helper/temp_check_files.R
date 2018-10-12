wd = getwd()
local_dir ="/Users/saundersk1/Documents/Git/RaingleExtremes"
server_dir = "/home/student.unimelb.edu.au/saundersk1/DataWrangle/"

if(wd == local_dir){
  data_dir = "/Users/saundersk1/Dropbox/Hard Drive/Data/GHCN-Daily/"
}else{
  data_dir = paste(wd, "GHCNDaily/", sep = '/')
}

list_files = list.files(path = data_dir)
prcp_files = list_files[stringr::str_detect(list_files, pattern = "PRCP")]
recon_files = list_files[stringr::str_detect(list_files, pattern = "RECON")]

saveRDS(prcp_files, 'prcp_files.rds')
saveRDS(recon_files, 'recon_files.rds')

server_prcp_rds = readRDS("~/Documents/Git/RaingleExtremes/Helper/prcp_files.rds")
server_recon_rds = readRDS("~/Documents/Git/RaingleExtremes/Helper/recon_files.rds")
