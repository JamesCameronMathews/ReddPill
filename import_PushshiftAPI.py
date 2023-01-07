
from pmaw import PushshiftAPI
from datetime import datetime

import pandas as pd

api = PushshiftAPI()
posts = api.search_submissions(q="pregabalin", subreddit="all", limit=100000, mem_safe=True)
post_list = [post for post in posts]

# create csv file
df = pd.DataFrame(posts)
df.to_csv('data.csv', sep=',', index=False)
