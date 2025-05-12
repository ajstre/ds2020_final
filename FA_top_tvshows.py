import requests
from bs4 import BeautifulSoup
import csv

#credit to https://pypi.org/project/fa-scraper/ for helping me get started
#I was stuck on this website for a while but their code helped give me direction

#Basically the way filmaffinity works is that on its top 1000 TV shows of all time page it loads 30 at a time, and then
#if the user wants to view more, it makes a POST request to a server and gets the next 30 shows information in a response.
#Because of this what we have to do to get the data is initally get the first 30 shows results, and then simulate a post request
#and read the results until we've done this for 1000 TV shows


base_url = "https://www.filmaffinity.com/us/ranking.php"

session = requests.Session()

#Some general information to make everything work
headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
                  "(KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36",
    "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
    "x-requested-with": "XMLHttpRequest",
    "Origin": "https://www.filmaffinity.com",
    "Referer": "https://www.filmaffinity.com/us/ranking.php?rn=ranking_fa_series"
}

results = []

def parse_html(html):
    """
    Given an HTML fragment (which may contain 30 items),
    this function extracts the title, score, and user count for each item.
    """
    soup = BeautifulSoup(html, "lxml")
    #Finding the title for each show
    cards = soup.find_all("div", class_="fa-card")
    for card in cards:
        title_anchor = card.find("a", class_="d-none d-md-inline-block")
        if not title_anchor:
            continue
        title = title_anchor.get_text(strip=True)
        
        #The info we need is inside "fa-avg-rat-box"
        li_parent = card.find_parent("li")
        if li_parent:
            rating_box = li_parent.find("div", class_="fa-avg-rat-box")
        else:
            rating_box = card.find("div", class_="fa-avg-rat-box")
        
        score = "N/A"
        user_count = "N/A"
        if rating_box:
            avg_div = rating_box.find("div", class_="avg")
            count_div = rating_box.find("div", class_="count")
            if avg_div:
                score = avg_div.get_text(strip=True)
            if count_div:
                user_count = count_div.get_text(strip=True)
        results.append([title, score, user_count])

#The list is for 1000 TV shows
max_items = 1000

for offset in range(0, max_items, 30):
    payload = {
        "from": str(offset),            #This is the number of TV shows you've seen before (on the 1st request it is 30, then 60, ect.)
        "count": str(30),        #It does everything in 30 show blocks
        "rankingId": "ranking_fa_series",  #the ranking identifier
        "chv": ""                        #This doesnt matter (default is 0 or blank)
    }
    print(f"Fetching items {offset + 1} to {offset + 30}")
    r = session.post(base_url, headers=headers, data=payload)
    if r.status_code == 404:
        print(f"Received 404 at offset {offset};")
        break
    r.raise_for_status()
    parse_html(r.text) 

#Write to the CSV file
with open("FA_TV_Rankings.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(["Title", "Score", "Number_Votes"])
    writer.writerows(results)

print("CSV generated succesfully")