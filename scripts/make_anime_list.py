from jikanpy import Jikan
import pandas as pd
import time

jikan = Jikan()

df = pd.read_csv("animelist.csv")

df = df[["/anime/series_title", "/anime/series_episodes",
         "/anime/series_type", "/anime/series_animedb_id", "/anime/my_score"]]

df = df.rename(columns={"/anime/series_title": "title",
                        "/anime/series_episodes": "episodes",
                        "/anime/series_type": "type",
                        "/anime/series_animedb_id": "id",
                        "/anime/my_score": "score"})

# org headers
total = "#+TITLE: My Anime List\n"
total += "#+AUTHOR: Sai Pandian\n"
total += "#+STARTUP: content\n"
total += "\n"

# first heading
total += "* Completed\n"

for index, row in df.iterrows():

    string = "** " + row["title"] + "\n"

    string += "- Episodes: [" + str(row["episodes"]) + \
              "/" + str(row["episodes"]) + "]\n"

    string += "- Type: " + str(row["type"]) + "\n"
    string += "- Score: " + str(row["score"]) + "\n"

    string += "- Link: [[" + \
              jikan.anime(row["id"])["url"] + "][" + row["title"] + "]]\n \n"

    total += string
    time.sleep(1)

text_file = open("animelist.org", "w")
output = text_file.write(total)
text_file.close()
