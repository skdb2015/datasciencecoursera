library(httr)
library(httpuv)
library(jsonlite)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
client_id = "ee4f451eedbef976a892"
client_secret = "fea4b1a26482f6839ee473667059a728557d00dc"
myapp <- oauth_app("skdbGetDataQuiz2", key=client_id, secret=client_secret)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp, cache=FALSE)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)

# extract the raw json data from the response
rawData = content(req)

#put it into a pretty data frame
prettyData = fromJSON(toJSON(rawData))

#get the json element that has the info regarding the datasharing repo
dataSharingRepo = prettyData[prettyData$name=="datasharing",]

#get the creation time
creationTimeofDataSharingRepo = dataSharingRepo$created_at

#return the creation time
creationTimeofDataSharingRepo
