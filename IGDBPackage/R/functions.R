
##LINK SCRAPE FUNCTION
linkScrape <- function(link){
  page = read_html(link)
  linkVec = page %>% html_nodes("td > a") %>% html_attr("href")
  linkVec = as.character(linkVec[linkVec != ""])
  fullLinkVec = length(linkVec)
  for(i in 1:length(linkVec)){
    fullLinkVec[i] = paste("https://www.igdb.com", linkVec[i], sep = "")
  }
  linkData = data.frame(fullLinkVec)
  colnames(linkData) = c("link")
  return(linkData)
}

##NAME SCRAPE FUNCTION
nameScrape <- function(link){
  page = read_html(link)
  nameVec = page %>% html_nodes("td > a") %>% html_text()
  nameVec = as.character(nameVec[nameVec != ""])
  nameData = data.frame(nameVec)
  colnames(nameData) = c("names")
  return(nameData)
}


##SCRAPE PAGES FUNCTION
pageScrape <- function(link){
  page = read_html(link)
  linkVec = page %>% html_nodes("td > a") %>% html_attr("href")
  linkVec = as.character(linkVec[linkVec != ""])
  fullLinkVec = length(linkVec)
  for(i in 1:length(linkVec)){
    fullLinkVec[i] = paste("https://www.igdb.com", linkVec[i], sep = "")
  }
  linkData = data.frame(fullLinkVec)
  colnames(linkData) = c("link")
  linkData = unique(linkData)
  len = length(linkData$link)
  title = len #.banner-title
  release = len #.banner-subheading > span span:nth-child(1)
  genre = len #.gamepage-tabs p:nth-child(1)
  story = len #p+ p
  for(i in 1:len){
    link = linkData$link[i]
    page = read_html(link)
    #pull titles
    titles = page %>% html_nodes(".banner-title") %>% html_text()
    titles = substr(titles, 1, nchar(titles)-4)
    title[i] = titles

    #pull release dates
    releases = page %>% html_nodes(".banner-subheading > span span:nth-child(1)") %>% html_text()
    releases = as.character(releases[releases != "..."])
    release[i] = releases

    #pull genres
    genres = page %>% html_nodes(".gamepage-tabs p:nth-child(1)") %>% html_text()
    genres = as.character(genres[genres != "No lists available, why not create one?"])
    genres = substring(genres, 8)
    genre[i] = genres

    #pull storyline
    storys = page %>% html_nodes("#game-storyline") %>% html_text()
    if(length(storys) == 0){
      story[i] = "na"
    }else{
      story[i] = storys
    }

    print(link)
  }
  dataOut = data.frame(title, release, genre, story)
  return(dataOut)
}
