library(rvest)
library(stringr)
library(httr)

setwd("/Users/cavin/Desktop/Yellowpages Scraper")

#Business Directory website
#https://www.yellow-pages.ph/search/accounting/nationwide/page-1

#enter page number
fromTo = 1:12

# Get detail page links from list
getCompanyLinks = function(url){
  
  Link = read_html(url)
  
  q_link <- Link %>% html_nodes('.search-business-name > h2 > a') %>% html_attr('href')
  
  q_link = paste0('https://www.yellow-pages.ph', q_link)
  
  return(q_link)
  
}
#for testing purposes (should have 30 results)
#getCompanyLinks("https://www.yellow-pages.ph/search/accounting/nationwide/page-1")


#Get company info from detail page
extractDetails = function(url){
  
  page <- read_html(url)
  
  #company name
  company_name <- page %>% html_nodes('h1') %>% html_text()
  
  #company address
  company_address <- page %>% html_nodes(xpath = '//div[contains(text(),"Address")]/following-sibling::a[1]') %>% html_text()
  
  #phone number
  phone_number <- page %>% html_nodes(xpath = '//div[contains(text(),"Landline")]/following-sibling::a[1]') %>% html_text()
  
  #email
  email <- page %>% html_nodes(xpath = '//div[contains(text(),"Email")]/following-sibling::a') %>% html_text()
  
  #website
  website <- page %>% html_nodes(xpath = '//div[contains(text(),"Website")]/following-sibling::a') %>% html_attr('href')

  #contact person
  #contact_person <- page %>% html_nodes(xpath = '//div[ text() ="Contact Person"]/following-sibling::div') %>% html_text()
  
  #company manager
  #company_manager <- page %>% 
  #  html_nodes(xpath = '//span[ text() ="Company manager"]/parent::div') %>% html_text()
  #company_manager <- sub('Company manager ','', company_manager)
  
  businesslist <- data.frame(company_name=company_name[1], email=email[1], website=website[1], phone_number=phone_number[1], company_address=company_address[1])
 
  return (list(businesslist=businesslist)) 
}

#for testing purposes
#results = extractDetails("https://www.yellow-pages.ph/business/rs-bernaldo-associates")
#View(results$businesslist)



#define an empty vector for storing the links 
links = character()
for (page in fromTo){
  url = paste0('https://www.yellow-pages.ph/search/accounting/nationwide/page-', page)
  #sleep 0.2 second
  Sys.sleep(0.2)
  thisLinks = getCompanyLinks(url)
  for (link in thisLinks){
    if (!(link %in% links)){
      links = c(links, link) 
    }
  } 
}
print(links)

#define an empty data frame for storing the forum posts
businessListDF = data.frame(company_name=character(), email = character(), website=character(), phone_number=numeric(), company_address=character()) 

count = 0

#for each page, get the info 
for (url in links){
  #sleep 0.2 second 
  Sys.sleep(runif(1)*20)
  
  print(url)
  results = extractDetails(url)
  
  businessListDF = rbind(businessListDF, results$businesslist) 
  
  count = count + 1 
}

#export data to csv
write.csv(businessListDF, paste0("businesslist_page_", fromTo[1],"-",tail(fromTo,n=1),".csv")) 
