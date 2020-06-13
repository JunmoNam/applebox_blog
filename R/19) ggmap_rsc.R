#======================================================================
# google map API in R
#
# Author : Junmo Nam
#======================================================================

cat('Loading dplyr, ggmap, ggplot2 and sf \n')
cat('Method provided : \n')
cat('           create_gmap : get ggmap from given x and y vector, or dataframe/matrix in x\n')
cat('           tr_to_4326 : convert katec coordinates to long+lat\n')
cat('           .katec : a katec string\n')
cat('           .fixed_theme : ggplot theme based on theme_bw + no legend + no axis text\n')

#load packages
sapply(c('dplyr','ggmap','ggplot2','sf'),require,character.only = T)

#katec string
.katec = '+proj=tmerc +lat_0=38 +lon_0=128 +k=0.9999 +x_0=400000 +y_0=600000 +ellps=bessel
          +units=m +no_defs +towgs84=-115.80,474.99,674.11,1.16,-2.31,-1.63,6.43'

#fixed theme for bw + no axis text + no legend
.fixed_theme = theme_bw()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = 'none')

#create ggmap from give coordinates
create_gmap = function(x = NULL, y = NULL,google_map_color = 'bw'){
  if(is.null(x) & is.null(y)) stop('No x and y provided')
  if(length(dim(x))==2){
    print('Get two-dimensional object in x, use x[,1] and x[,2] as X and Y coordinates')
    y <- x[,2]
    x <- x[,1]
  }
  get_map(c(min(x),min(y),max(x),max(y)),color = google_map_color) %>% ggmap
}


#given coordinates to longlat
tr_to_4326 = function(df,x_name = NULL, y_name = NULL,drop_cols = F,crs = .katec){
  if(is.null(x_name) & is.null(y_name)){
    print('use first and second column of df as x and y')
    x_name = names(df)[1]
    y_name = names(df)[2]
  }
  
  sf_df = df %>%
    #transform to sf object, use crs and x,y names
    st_as_sf(coords = c(x_name,y_name),crs = crs) %>% 
    #transform to 4326 crs
    st_transform(crs = 4326) %>%
    #get coordinates back to dataframe
    st_coordinates %>% as.data.frame %>% rename(long = X,lat = Y)
  #column binding and export
  if(drop_cols){
    return(sf_df)
  }else{
    return(df %>% bind_cols(sf_df))  
  }
}