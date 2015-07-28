
# This script is based codes shared by socialfunction.wordpress.com
# Modified by CuriosityBits (curiositybits.com)

#------install required R packages

#install.packages(c("httr", "rjson", "RCurl", "RODBC"))
#install.packages("httpuv")
#install.packages('httpuv')

#--------------------FOR AUTHENTIFICATION---------------------------------------
require(httr)
full_url <- oauth_callback()
full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
print(full_url)
full_url

libs <- c("httr", "rjson", "RCurl", "RODBC")
lapply(libs, library, character.only=TRUE)

#replace with your INSTAGRAM CLIENT info
app_name <- ""
client_id <- ""
client_secret <- ""
scope <- "basic"

instagram <- oauth_endpoint(
  authorize = "https://api.instagram.com/oauth/authorize",
  access = "https://api.instagram.com/oauth/access_token")
myapp <- oauth_app(app_name, client_id, client_secret)

ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic", type = "application/x-www-form-urlencoded",cache=FALSE)

tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
token <- tmp[[1]][4]
print (show<- paste("this is your token:",(token)))

# download certificate 
getwd()
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#libs <- c("rjson", "httr")
#lapply(libs, library, character.only=TRUE)

username = 'scottwalker'
user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
get_user_id <- user_info$data[[1]]["id"]
#get_user_id
user_id <- get_user_id
print (show<-paste("data grab focusing on:",user_id))
       
#Get recent media (20 pictures)
media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent/?access_token=',token,sep=""),
                         cainfo = "cacert.pem"))
posts <- data.frame() #main database for post_level data 
prof <- data.frame()
us_tagged <- data.frame() #main databe for storing the data of users tagged in photos 
comments <- data.frame() #main databse for storing comments
us_liked <- data.frame() #main databse for storing data of users who like a post 
med <- data.frame() #main database for storing image metadata

# set up a stoptime
stoptime <- "2015-07-28 13:40:27"
stoptime <- as.POSIXct(stoptime, origin = "1960-01-01", tz = "America/New_York")
print (currenttimedis<-paste("the current time is:", Sys.time()))
print (currenttimedis<-paste("the stoptime is set to:", stoptime))
# start the loop
#while (Sys.time() <= stoptime) {
while (Sys.Date() <= as.Date("2015-12-31")) {
  media <- fromJSON(getURL(paste(media[[1]]$next_url, sep=""),
                           cainfo = "cacert.pem"), 
                    unexpected.escape = "keep")
  print (medialong<-paste("the amount of data to be grabbed:",length(media$data)))
  
  if (length(media$data)) {
    posts <- data.frame(no = 1:length(media$data))
    #medias <- data.frame(no = 1:length(media$data))
    
    for (i in 1:length(media$data)) {
      print(paste("getting meta of media", i, "of", length(media$data), sep=" "))
      # profile
      posts$screenName[i] <- media$data['i']$user$username
      posts$name[i] <- media$data['i']$user$full_name
      
      if(length(media$data[[i]]$location$latitude)) {
        posts$latitude[i] <- media$data[[i]]$location$latitude
        posts$longitude[i] <- media$data[[i]]$location$longitude
      } else { # if(length(media$data[[i]]$location$latitude)) loop
        posts$latitude[i] <- NA
        posts$longitude[i] <- NA
      } # if(length(media$data[[i]]$location$latitude)) loop
      
      if (length(media$data[[i]]$location$name)) {
        posts$location[i] <- as.character(media$data[[i]]$location$name)
      } else { # if (length(media$data[[i]]$location$name)) loop
        posts$location[i] <- NA # if (length(media$data[[i]]$location$name)) loop
      }
      
      posts$profile_picture[i] <- media$data[[i]]$user$profile_picture
      posts$website[i] <- media$data[[i]]$user$website
      posts$bio[i] <- media$data[[i]]$user$bio
      posts$id[i] <-  media$data[[i]]$user$id
      
      if (length(media$data[[i]]$caption$text)) {
        posts$caption[i] <- media$data[[i]]$caption$text
      } else { # if(length(media$data....text)) loop
        posts$caption[i] <- NA
      } # if(length(media$data....text)) loop
      
      posts$tags_used[i] <- paste(media$data[[i]]$tags, collapse=",")
      posts$comments_count[i] <- media$data[[i]]$comments$count
      posts$likes_count[i] <- media$data[[i]]$likes$count
      posts$image_url[i] <- media$data[[i]]$link
      posts$type[i] <- media$data[[i]]$type
      posts$created[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin = "1970-01-01")) 
      posts$media_id[i] <- media$data[[i]]$id
      
      # comments
      
      if (length(media$data[[i]]$comments$data)) {
        for (x in 1:length(media$data[[i]]$comments$data)) {
          screenName <- media$data[[i]]$comments$data[[x]]$from$username
          name <- media$data[[i]]$comments$data[[x]]$from$full_name
          text <- media$data[[i]]$comments$data[[x]]$text
          reply_to <- media$data[[i]]$user$full_name
          reply_to_id <- media$data[[i]]$id
          created <- toString(as.POSIXct(as.numeric(media$data[[i]]$comments$data[[x]]$created), origin="1970-01-01"))
          id <- media$data[[i]]$comments$data[[x]]$id
          comm <- as.data.frame(cbind(screenName, name, text, created, reply_to,
                                      reply_to_id))
          comments <- as.data.frame(rbind(comments, comm))
        } #for (x in 1:length(media$data[[i]]$comments$data)) loop
      } else {
      } #if (length(media$data[[i]]$comments$data)) loop
      
      # user_tagged
      
      if (length(media$data[[i]]$users_in_photo)) {
        for (y in 1:length(media$data[[i]]$users_in_photo)) {
          y <- media$data[[i]]$users_in_photo[[y]]$position$y
          x <- media$data[[i]]$users_in_photo[[y]]$position$x
          username <- media$data[[i]]$users_in_photo[[y]]$user$username
          name <- media$data[[i]]$users_in_photo[[y]]$user$full_name
          profile_picture <- media$data[[i]]$users_in_photo[[y]]$user$profile_picture
          id <- media$data[[i]]$users_in_photo[[y]]$user$id
          tagged_in <- media$data[[i]]$user$username
          tagged_in_id <- media$data[[i]]$user$id
          us_tag <- as.data.frame(cbind(username, name, id, tagged_in, tagged_in_id,
                                        profile_picture, x, y))
          us_tagged <- as.data.frame(rbind(us_tagged, us_tag))
        }
      } else {
      } #if (length(media$data[[i]]$users_in_photo)) loop
      
      #user likes
      if(length(media$data[[i]]$likes$data)) {
        for (z in 1:length(media$data[[i]]$likes$data)) {
          media_liked_id <- posts$id[i] <- media$data[[i]]$id
          username <- media$data[[i]]$like$data[[z]]$username
          profile_picture <- media$data[[i]]$like$data[[z]]$profile_picture
          id <- media$data[[i]]$like$data[[z]]$id
          name <- media$data[[i]]$like$data[[z]]$full_name
          us_like <- as.data.frame(cbind(username, profile_picture, name, id,
                                         media_liked_id))
          us_liked <- as.data.frame(rbind(us_liked, us_like))
        }
      } else {
      } # if(length(media$data[[i]]$likes$data)) loop
      
    } # for i in 1:length loop
    print ("Sorting and combining metadata")
    prof <- as.data.frame(rbind(prof,posts))
    #med <- as.data.frame(rbind(med,medias))
    
  } else {
  }# if (length) loop
  Sys.sleep(30) 
} # the while Sys.Date loop
#if (Sys.time() <= stoptime) {
if (Sys.Date() <= as.Date("2015-12-31")) {
  print("Crawl ended")
} else {
  print(paste("Crawl stopped, an error occured at", Sys.time()))
}

#save database in CSV

write.csv(prof, "Walker_posts.csv")
write.csv(us_liked, "Walker_likes.csv")
write.csv(us_tagged, "Walker_tags.csv")
write.csv(comments, "Walker_comments.csv")
