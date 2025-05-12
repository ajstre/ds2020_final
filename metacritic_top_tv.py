import requests
from bs4 import BeautifulSoup
import csv
import time

#Metacritic uses a page system. All we need to do is take the data from the 1st page and then change the URL to the next
#page and repeat.

#Headers for the request
headers = {
    "User-Agent": ("Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                   "AppleWebKit/537.36 (KHTML, like Gecko) "
                   "Chrome/135.0.0.0 Safari/537.36"),
    "Accept-Language": "en-US,en;q=0.9",
    "Referer": "https://www.metacritic.com/"
}

results = []

#Loop through each page
for page in range(1, 13):
    #The url is updated depending on what page we are on
    url = f"https://www.metacritic.com/browse/tv/?releaseYearMin=1910&releaseYearMax=2025&page={page}"
    
    response = requests.get(url, headers=headers)

    #Get the data
    soup = BeautifulSoup(response.text, "html.parser")
    
    #Each movie is wrapped so this gets each one
    cards = soup.find_all("div", class_="c-finderProductCard_info u-flexbox-column")
    
    for card in cards:
        #Get the title
        title_div = card.find("div", class_="c-finderProductCard_title")
        if not title_div:
            continue
        heading = title_div.find("h3", class_="c-finderProductCard_titleHeading")
        if not heading:
            continue
        spans = heading.find_all("span")
        if len(spans) < 2:
            continue
        title = spans[1].get_text(strip=True)
        
        #Get the score
        score = "N/A"
        score_container = card.find("span", class_="c-finderProductCard_metaItem c-finderProductCard_score")
        if score_container:
            score_div = score_container.find("div", class_="c-siteReviewScore")
            if score_div:
                score_span = score_div.find("span")
                if score_span:
                    score = score_span.get_text(strip=True)
                    
        results.append([title, score])
    
    time.sleep(1) 

output_filename = "metacritic_tv_scores.csv"
with open(output_filename, "w", newline="", encoding="utf-8") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["Title", "Score"])
    writer.writerows(results)

print("CSV Generated Succesfully")