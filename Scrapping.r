list.of.packages <- c("xml2","tm","SnowballC","dplyr",'rvest',"qdapRegex",'wordcloud','stringr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(xml2) #work with HTML and XML from R
library(tm) #text mining
library(SnowballC)#stemming
library(rvest)#get balise
library(dplyr)
library(qdapRegex)#delete email adress
library(wordcloud)
library(stringr)

home_url="https://fr.indeed.com/jobs?q=militaire&l=France&vjk=f27176ef24e95146"
home_page = read_html(home_url)

#Get total number of job to get number of page
get_tot_offer <- home_page %>%
  rvest::html_nodes("div")  %>% 
  rvest::html_nodes(xpath = '//*[@id="searchCountPages"]') %>% 
  rvest::html_text() %>%
  stringi::stri_trim_both()
tot_offer <- sub(" emplois.*", "", sub(".*de ", "", get_tot_offer))
tot_offer <-as.numeric(gsub(intToUtf8(160),"",tot_offer)) #no simple space between number but "non breaking space"
sprintf("total job are is %d" , tot_offer )

# Get number of page
tot_pages = ceiling( tot_offer / 15 )
tot_pages = 15
# About 1.5min / page ( 15 jobs / page)
sprintf("Il y a page %d, le processus devrait durer : %f minutes ou %f heures", tot_pages, tot_pages*2, tot_pages*2/60)

#Initialize 
contents <- list()
types <- list()
locations_tot <- list()
salaire_tot <- list()
date_tot <- list()
name_tot <- list()
titles <- list()
links <- list()
page <- list()
for( i in 1:tot_pages){
  start.time <- Sys.time()
  print(paste("Numero de page :",i,"sur",tot_pages))
  
  Sys.sleep(2) #add sleep to not be blocked by Indeed
  
  #Get url
  page_wise = read_html(url(paste(home_url,"&start=",(10 * i-1)+1, sep = ""), "rb")) #url( ..,"rb") resolve Error in open.connection(x, "rb") : Couldn't connect to server
  page <- c(page, (10 * i-1)+1)
  
  #Get the job description : need to recup url of the job from the front page for each job offer and 
  #then recup the link to its page
  get_links <- page_wise %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@data-hide-spinner="true"]') %>%
    rvest::html_attr("href")
  
  Sys.sleep(1) 
  
  #Get all location from page
  location_page <- page_wise %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@class="companyLocation"]') %>%
    rvest::html_text() %>%
    stringi::stri_trim_both()
  locations_tot <- c(locations_tot, location_page)
  
  Sys.sleep(2)
  
  #Get all salary from page
  salaire_page <- page_wise %>% 
    rvest::html_nodes("div") %>%
    rvest::html_nodes(xpath = '//*[@class="salary-snippet"]') %>%
    rvest::html_text() %>%
    stringi::stri_trim_both()
  salaire_tot <- c(salaire_tot, salaire_page)
  
  Sys.sleep(1)
  
  #Get date
  date_page <- page_wise %>%
    rvest::html_nodes("span")  %>% 
    rvest::html_nodes(xpath = '//*[@class="date"]') %>% 
    rvest::html_text() %>%
    stringi::stri_trim_both()
  date_tot <- c(date_tot, date_page)
  
  Sys.sleep(2)
  
  #Company name
  name_page <- page_wise %>%
    rvest::html_nodes("span")  %>% 
    rvest::html_nodes(xpath = '//*[@class="companyName"]') %>% 
    rvest::html_text() %>%
    stringi::stri_trim_both()
  name_tot <- c(name_tot, name_page)
  
  #For each links: recup content / type 
  for (j in 1:length(get_links)){
    print(paste('Lien',j,'/',length(get_links),":",get_links[j]))
    
    Sys.sleep(1) 
    
    #Add https://ca.indeed.com for the links to be valid
    get_links[j]= paste('https://ca.indeed.com', get_links[j], sep="")
    links <- c(links, get_links[j])
    
    job_page <- xml2::read_html(get_links[j])
    
    Sys.sleep(2)
    
    #Get content
    content <- job_page %>%
      rvest::html_nodes("div")  %>% 
      rvest::html_nodes(xpath = '//*[@id="jobDescriptionText"]') %>% 
      rvest::html_text() %>%
      stringi::stri_trim_both()
    if (length(content) == 0){
      contents <- c(contents, NA)
    } else {
      contents <- c(contents, content)}
    
    Sys.sleep(1)
    
    #Get type stage
    type <- job_page %>% 
      rvest::html_nodes("span") %>%
      rvest::html_nodes(xpath = '//*[@class="jobsearch-JobMetadataHeader-item  icl-u-xs-mt--xs"]') %>%
      rvest::html_text() %>%
      stringi::stri_trim_both()
    if (length(type) == 0){
      types <- c(types, NA)
    } else {
      types <- c(types, type)}
    
    Sys.sleep(2)
    
    #Get title
    title <- job_page %>% 
      rvest::html_nodes("div") %>%
      rvest::html_nodes(xpath = '//*[@class="jobsearch-JobInfoHeader-title-container "]') %>%
      rvest::html_text() %>%
      stringi::stri_trim_both()
    if (length(title) == 0){
      title <- job_page %>% 
        rvest::html_nodes("div") %>%
        rvest::html_nodes(xpath = '//*[@class="jobsearch-JobInfoHeader-title-container jobsearch-JobInfoHeader-title-containerEji"]') %>%
        rvest::html_text() %>%
        stringi::stri_trim_both()
      if (length(title) == 0){
        titles <- c(titles, NA)
      } else {
        titles <- c(titles, title)}
    } else {
      titles <- c(titles, title)
    }
  }
  end.time <- Sys.time()
  print(end.time-start.time)
}

links <- unlist(links)
titles <- unlist(titles)
name_tot <- unlist(name_tot)
date_tot <- unlist(date_tot)
contents <- unlist(contents)
types <- unlist(types)
locations_tot <- unlist(locations_tot)
salaire_tot <- unlist(salaire_tot)

length(links)
length(titles)
length(name_tot)
length(date_tot)
length(contents)
length(types)
length(locations_tot)
length(salaire_tot) #length different so reshape

########################
reshape_salaire <- rep(NA, length(contents))
for (i in 1:length(contents)){
  no_punc <- sub("\\€.*", "",contents[i]) #get all text before €
  no_punc <- str_sub(no_punc,start=-15) # get last 15 character 
  for (j in 1:length(salaire_tot)){
    first_sal <- sub(" €.*", "",salaire_tot[j]) #get first salary from salaire_tot
    keep_non_breaking_space <- first_sal 
    space <- gsub(intToUtf8(160)," ",first_sal) #replace non_breaking_space by normal space
    no_space <- gsub(intToUtf8(160),"",first_sal) #delete non_breaking_space
    if (length(grep(space, no_punc))!=0 
        || length(grep(no_space, no_punc))!=0
        || length(grep(keep_non_breaking_space, no_punc))!=0){
      reshape_salaire[i] <- salaire_tot[j] } #if one of the three is found : take salaire_tot
  }
} 
length(reshape_salaire) #OK
reshape_salaire #verify same shape reshape_salaire / salaire_tot
salaire_tot

#Regroup in dataframe
bd <- data.frame(links,types,titles, name_tot, date_tot, contents, locations_tot, reshape_salaire)
colnames(bd) <- c('Links', 'Type', 'Title','Company', 'Date', 'Contents','Locations','Salaires')

setwd("/Users/pierre/Documents/M2/Cours_M2/Text Mining/bd")
save(bd,file="bd_15.Rda")
#load("bd.Rda")

##################### Date ##################### 
bd$Date_clean <- sub("EmployerDernière activité.*", "NA",bd$Date) #to delete all ligne with after
bd$Date_clean <- sub(".*y a ", "",bd$Date_clean)
bd$Date_clean <- gsub(intToUtf8(160)," ",bd$Date_clean)
bd$Date_clean <- sub(" jours.*", "",bd$Date_clean)
bd$Date_clean <- sub(" jour.*", "",bd$Date_clean)
bd$Date_clean

#Transform in interval
for (i in 1:nrow(bd)){
  if (bd$Date_clean[i]=='NA'){
    bd$Date_inter[i]='NA'
  } else if (bd$Date_clean[i]=='30+'){
    bd$Date_inter[i]='Plus de 30 jours'
  } else if(bd$Date_clean[i]=="PostedAujourd'hui" | bd$Date_clean[i]=="PostedPubliée à l'instant"){
    bd$Date_inter[i]='Dans les 10 derniers jours'
  } else if(as.numeric(bd$Date_clean[i])>20){
    bd$Date_inter[i]='Entre 20 et 30 jours'
  } else if(as.numeric(bd$Date_clean[i])>10 & as.numeric(bd$Date_clean[i])<=20){
    bd$Date_inter[i]='Entre 10 et 20 jours'
  } else if(as.numeric(bd$Date_clean[i])<=10){
    bd$Date_inter[i]='Dans les 10 derniers jours'
  }
}
table(bd$Date_inter)
length(bd$Date_inter)

##################### Salary ##################### 
# If multiple salaries , recup first and second if there is + if heure / jour / mois / ans
first_salary <- list()
second_salary <- list()
periode <- list()

for (i in 1:length(bd$Contents)){
  
  if (!is.na(bd$Salaires[i])){
    
    periode <- c(periode, sub(".*par ", "",bd$Salaires[i]))
    
    index_pattern <- unlist(gregexpr(pattern ='€',bd$Salaires[i]))# places of pattern €
    n <- as.numeric(nchar(bd$Salaires[i]))# get number character
    
    if(length(index_pattern)>1){

      get_first_salary <- sub(" €.*", "",bd$Salaires[i])#get all character before €
      get_first_salary <- as.numeric(gsub(intToUtf8(160),"",get_first_salary)) #delete no white space between number
      first_salary <- c(first_salary, get_first_salary)
      
      place <- as.numeric(index_pattern[1]) #get first place of pattern €
      get_second_salary <- substr(bd$Salaires[i],place+4,n) #recup number after first pattern to get second salary 
      #+4 because not want to take - character
      get_second_salary <- as.numeric(gsub(intToUtf8(160),"",sub(" €.*", "",get_second_salary)))
      second_salary <- c(second_salary, get_second_salary)
      
    } else {
      get_first_salary <- sub(" €.*", "",bd$Salaires[i])#get all character before €
      get_first_salary <- as.numeric(gsub(intToUtf8(160),"",get_first_salary)) #delete no white space between number
      first_salary <- c(first_salary, get_first_salary)
      
      second_salary <- c(second_salary, NA)
    }
  } else {
    first_salary <- c(first_salary, NA)
    second_salary <- c(second_salary, NA)
    periode <- c(periode, NA)
  }
  
}
first_salary <- unlist(first_salary)
second_salary <- unlist(second_salary)
periode <- unlist(periode)

table(periode)

#If salaire per year, divide by 12
for (i in 1:length(first_salary))
{
  if (!is.na(first_salary[i]))
  {
    if (periode[i] == "an")
    {
      first_salary[i]<-first_salary[i]/12
      second_salary[i]<-second_salary[i]/12
    } else if (periode[i] == "jour")
    {
      first_salary[i]<-first_salary[i]*31
      second_salary[i]<-second_salary[i]*31
    } else if (periode[i] == "heure")
    {
      first_salary[i]<-first_salary[i]*151.67 #just for travail plein temps because for partiel, complicated to know
      second_salary[i]<-second_salary[i]*151.67
    }
  }
}

# Mean Salary
mean_salary <- list()
for (i in 1:length(first_salary)){
  if (is.na(second_salary[i])){
    mean_salary[i] <- first_salary[i]
  } else {
    mean_salary[i] <- (first_salary[i]+second_salary[i])/2
  }
}
mean_salary <- unlist(mean_salary)
boxplot(mean_salary)

##################### Cleaning content ##################### 
clean_content <- bd$Contents

# Lowercase
clean_content <- tolower(clean_content)

#Delete email adress
clean_content <- rm_email(clean_content)

#Delete \n
clean_content <- stringr::str_replace_all(clean_content,"\n", " ") 

#Delete punctuation
clean_content<- gsub('[[:punct:] ]+',' ',clean_content) 

#Delete number
clean_content <- gsub('[[:digit:]]+', ' ', clean_content)

#To change accent to non accent
clean_content <- iconv(clean_content, to="ASCII//TRANSLIT//IGNORE")
clean_content <- gsub("['`^~\"]", "", clean_content)

#Delete word length 1
clean_content <- rm_nchar_words(clean_content, "1") 
clean_content = removeWords(clean_content, stopwords("french"))
clean_content = removeWords(clean_content, stopwords("english"))

#Remove extra white space
clean_content <- gsub("\\s+"," ",clean_content)

clean_content


##################### Types ##################### 
for (i in 1:length(bd$Contents)){
  print(i)
  if(grepl('cdd', bd$Content[i], fixed = TRUE)){
    bd$Contract[i] = 'CDD'
    print('cdd')
  } else if(grepl('cdi', bd$Content[i], fixed = TRUE)){
    bd$Contract[i] = 'CDI'
    print('cdi')
  } else if(grepl('stage', bd$Content[i], fixed = TRUE) | grepl('internship', bd$Content[i], fixed = TRUE)){
    bd$Contract[i] = 'Stage'
    print('stage')
  }
}

##################### Locations ##################### 
locations_tot2  <- sub(").*", ")",locations_tot) 
locations_tot2  <- sub("•.*", "",locations_tot2) 
dep <- list()
for (i in 1:length(locations_tot2)){
  if(grepl("^[0-9]",locations_tot2[i]) ){
    dep[i] <- substr(locations_tot2[i],1,2)
  } else {
    dep[i] <-gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", locations_tot2[i], perl=T)
  }
  
}
dep <- gsub(" ", "", dep, fixed = TRUE)
ville <- gsub("\\([^()]*\\)", "", locations_tot2) #take just what in ()
ville <- gsub('[[:digit:]]+e', '', ville) #Delete 20e, 10e ,... 
ville <- gsub('[[:digit:]]+', '', ville)#Delete all number in front
ville <- trimws(ville)

##################### Clean title ##################### 
# Lowercase
titles <- tolower(titles)

#Delete punctuation
titles <- gsub('[[:punct:] ]+',' ',titles) 

#Delete number
titles <- gsub('[[:digit:]]+', ' ', titles)

#To change accent to non accent
titles <- iconv(titles, to="ASCII//TRANSLIT//IGNORE")
titles <- gsub("['`^~\"]", "", titles)

#Delete word length 1
titles <- rm_nchar_words(titles, "1") 

#Stop words
titles = removeWords(titles, stopwords("french"))
titles = removeWords(titles, stopwords("english"))

titles

############################## Recap ########################
titles
dep
ville
Contract_time
Contract_type
clean_content
mean_salary
bd$Date_inter
name_tot

#check if same length before dataframe
length(titles)
length(dep)
length(ville)
length(Contract_time)
length(Contract_type)
length(clean_content)
length(mean_salary)
length(bd$Date_inter)
length(name_tot)

#Regroup in dataframe
final <- data.frame(titles,name_tot,bd$Date_inter,clean_content, bd$Contract, ville, dep,
                 mean_salary)
colnames(final) <- c('Titre', 'Company', 'Date', 'Content','Contract','Ville','Dep','Mean_salary')

nrow(final)

final<-final[!duplicated(final), ] #Delete duplicate

if (length(grep("NA", final$Date))>1){ #Probleme employer derniere activite (due to rvest)
  final <- final[- grep("NA", final$Date),]
}
 
final <- final[final$Ville!='France',] #people enter wrong city
rownames(final) <- 1:nrow(final) # good row names

#setwd("/Users/pierre/Documents/M2/Cours_M2/Text Mining/bd")
#save(final,file="final.Rda")
#load("final.Rda")
