#####################################################################################################
# FUNCTIONS FOR DOWNLOADING AND IMPORTING SATELLITE TAG DATA VIA WILDLIFE COMPUTERS WEB SERVICES
#####################################################################################################

# Ensure that wcUtils is installed when this code is sourced

if (!require('wcUtils',character.only = TRUE)) {
  if (!require('devtools',character.only = TRUE)){install.packages('devtools')}
  devtools::install_github("jmlondon/wcUtils")}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wcMakeKey
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' A helper function to create a security key in .json format needed for accessing Wildlife Computers
#' webs services.
#' 
#'  @param access.key the Wildlife Computers access key
#'  @param access.key the Wildlife Computers secret key
#'  @param key.file filepath of keyfile to be created

wcMakeKey <- function(access.key, secret.key, key.file){
  
if (!require('jsonlite',character.only = TRUE)) {install.packages('jsonlite')}
library(jsonlite)
wcSecureKey = data.frame(wcAccessKey = access.key,wcSecretKey = secret.key)
write_json(wcSecureKey,key.file)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# wcDownload
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' This function is a wrapper around several functions from the \code{wcUtils} package by Jack London
#' 
#' The function takes a vector of PTT IDs or a project name and downloads and unzips all data files to 
#' a specified directory.
#' 
#' @param ptts a vector of ptt IDs
#' @param project If supplied, downloads data for all deployments in the named project and ignores \code{ptts}. 
#' Assumes there is a label named 'Project' (case sensitive) set up in the Wildlife Computers portal.
#' @param keyfile file path to a secure key containing the Wildlife Computers API secret key and access key
#' created using the \code{wcMakeKey} function above.
#' @param destination file path of the folder to download data to.
#' @param overwrite a logical indicating whether ot not to overwrite existing data for each PTT in the destination folder
#' @return Downloads data to the destination folder with a subfolder for each tag named by the PTT number.

wcDownload <- function(ptts,project=NULL,keyfile,destination=NULL,overwrite=FALSE){
  
  out = wcUtils::wcPOST(keyfile = keyfile, params="action=get_deployments") 
  
  doc <- xml2::read_xml(httr::content(out, 'raw'))
  doc <- XML::xmlParse(doc)
  
  WCids <- XML::xpathSApply(doc,paste0("//data/deployment/id"),XML::xmlValue)
  PTT   <- XML::xpathSApply(doc,paste0("//data/deployment/argos/ptt_decimal"),XML::xmlValue) 
  
  if(!is.null(project)){ 
    
    projids = wcUtils::wcGetProjectIDs(out,project)
    ptts = unique(PTT[WCids %in% projids])
    
  }
  
  WCids = subset(WCids,PTT %in% ptts)
  PTT = subset(PTT,PTT %in% ptts)
  unkid = subset(ptts,!ptts %in% PTT)
  if(length(unkid)>0) print(paste('The following PTTs do not exist in the portal:',unkid))
  
  if(is.null(destination)) {dest = tempdir()} else {dest = destination}
  
  # Iterate through ptt numbers
  
  for (i in 1:length(ptts)){
    
    temp_file <- tempfile(tmpdir = dest)
    dest0 = paste0(dest,"/",ptts[i])
    
    # if overwrite is F, skip any existing files  
    
    if(overwrite==F & file.exists(dest0)) {unlink(temp_file);next}
    
    # If a ptt has more than one WCid i.e. >1 deployment, use only most recent  
    
    id = WCids[PTT==ptts[i]]
    if (length(id)>1){print(paste("Multiple deployments for ptt",ptts[i],"\nDownloading the most recent"))
      id = id[length(id)] }
    
    # Web service bit
    
    download_params <- paste0("action=download_deployment&id=",id)
    
    r <- wcPOST(keyfile=keyfile,params=download_params)
    
    writeBin(httr::content(r, "raw"), temp_file)
    
    if(!file.exists(dest0)) {dir.create(dest0)} else if (overwrite==T) {unlink(dest0,recursive=T)}
    unzip.fail <- try(unzip(temp_file, exdir=dest0))
    kmz = list.files(dest0,full.names=TRUE,pattern="\\.kmz$")
    if(length(kmz)==1){try(unzip(kmz, exdir=dest0));unlink(kmz)}
    unlink(temp_file)
    
    
    if(is.null(destination)) {return(dest)}
    
  }  
}

# Example:
# wcDownload(project = 'MYPROJECT',destination = 'C:/Data',
# keyfile = "C:/wcSecureKey")
