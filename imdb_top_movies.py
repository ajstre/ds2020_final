import requests
import json
import csv
from bs4 import BeautifulSoup
import os
print("Current working directory:", os.getcwd())

url = "https://www.imdb.com/chart/toptv/"
headers = {"User-Agent": "Mozilla/5.0"}
response = requests.get(url, headers=headers)

soup = BeautifulSoup(response.text, "html.parser")
script_tag = soup.find("script", type="application/ld+json")
if not script_tag:
    print("No JSON-LD script found.")
    exit()

data = json.loads(script_tag.string)
items = data.get("itemListElement", [])

with open("C:/Users/aiden/Documents/imdb_top_250_tv.csv", "w", newline="", encoding="utf-8") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["Title", "Years", "User Score", "Number of Votes"])
    for item in items:
        tv_show = item.get("item", {})
        title = tv_show.get("name", "")
        aggregate = tv_show.get("aggregateRating", {})
        user_score = aggregate.get("ratingValue", "")
        vote_count = aggregate.get("ratingCount", "")
        writer.writerow([title, "", user_score, vote_count])

print("CSV file 'imdb_top_250_tv.csv' generated successfully.")