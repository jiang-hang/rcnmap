library(ggplot2)
library(maptools)
library(mapproj)

rcnmapEnv<-new.env()
data('cnmapdata',envir=rcnmapEnv)
#' draw china Map with province
#' 
#' description
#' 
#' @param name value
#' @param color value
#' @param title value
#' @param fill value
#' @return returndes
#' @export 
#' @examples 
#'   cnmap()
#'   cnmap(name=c("shanghai","hubei"),color=c("red","blue"))
cnmap<-function(name=NA,color=NA,title="China Map",fill=NA)
{
        if(length(name) != length(color)){
		stop("the length of name and color should match")
	}
	if(is.null(get('cnmapdata',envir=rcnmapEnv))) {
		data('cnmapdata',envir=rcnmapEnv)
        }
        mymap = ggplot(data = fortify(rcnmapEnv$cnmapdata)) 
        if(is.na(fill)){
           mymap=mymap+geom_path(aes(x = long, y = lat, group=group), colour = "blue")
	}else{
           mymap=mymap+geom_polygon(aes(x = long, y = lat, group=group), colour = "blue")
	}
        mymap=mymap+theme_grey() + labs(title=title,x="",y="")

	if(length(name) >= 1){
	    for(k in seq(1:length(name))) {
	        provIdx=which(rcnmapEnv$cnmapdata$NAME == name[k])
                if(length(provIdx) > 0) {
                     prov=rcnmapEnv$cnmapdata[provIdx,]
                     mymap = mymap +
                        geom_polygon(aes(x = long, y = lat, group=group),
                        fill = color[k], data=prov )
                }else{
                     stop(paste("wrong province name:", name))
	        }
	    }
	}
        mymap=mymap+geom_path(aes(x = long, y = lat, group=group), colour = "blue",data=rcnmapEnv$cnmapdata)
	mymap
}


