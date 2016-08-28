# export

#' writeTemp
#' @note DevStatus: no pass - utility ?/5 - should probably be part of better organized functions keeping tracks of data versions and folder structures 
#' 
#' Write a data frame contained into an xls file in the temporary data folder
#' 
#' @export

writeTemp = function (data, fileName)
{
    if (any(class(data)=="data.frame"))
    {
        filePath = file.path(tempFilesPath,paste0(fileName,".xls"))
        WriteXLS(data, filePath)
        print(paste0("Data has been written into the temporary file directory at : \n",
                     normalizePath(filePath)))
    }
    else
    {
        stop("The data variable need to be of the class 'data.frame'")
    }

}







