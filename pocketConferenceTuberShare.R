library(tuber)
library(httr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)

yt_oauth(app_id, app_password, token = '')

#work on a single video
test_vid_id <- "-3NmTE-fJSo"

vid_stats <- get_stats(video_id = test_vid_id)
vid_details <- get_video_details(video_id = test_vid_id)
#vid_captions <- get_captions(video_id = test_vid_id)
#seems that captions aren't available for all vids: https://github.com/soodoku/tuber/issues/23
vid_coment_threads <- get_comment_threads(c(video_id = test_vid_id))

#work on a channel
test_channel_id = "UC9oGkwvSJtqryHe5hPqbx3w"

chan_stats <- get_channel_stats(channel_id = test_channel_id)

chan_vids <- list_channel_videos(channel_id = test_channel_id, 
                                 max_results = 100) #without the max_results function at all this returned 50 videos. With max_results set to 100 this returned 2121 results.

chan_vids$year <- year(as.Date(chan_vids$contentDetails.videoPublishedAt))

#attach video work to channel work
chan_vids$viewCount <- NA
chan_vids$likeCount <- NA
chan_vids$dislikeCount <- NA
chan_vids$favouritCount <- NA
chan_vids$commentCount <- NA
chan_vids$title <- NA

for (v in 1:nrow(chan_vids)){
  
  tempVidStats <- get_stats(video_id = as.character(chan_vids[v, ]$contentDetails.videoId))
  
  chan_vids[v, ]$viewCount <- tempVidStats$viewCount
  chan_vids[v, ]$likeCount <- tempVidStats$likeCount
  chan_vids[v, ]$dislikeCount <- tempVidStats$dislikeCount
  chan_vids[v, ]$favouritCount <- tempVidStats$favouriteCount
  chan_vids[v, ]$commentCount <- tempVidStats$commentCount
  
  rm(tempVidStats)
  
  tempVidDetails <- get_video_details(video_id = as.character(chan_vids[v, ]$contentDetails.videoId))
  
  chan_vids[v, ]$title <- tempVidDetails$items[[1]]$snippet$title
  
  rm(tempVidDetails)
  
}

chan_vids$wrap_title <- str_wrap(chan_vids$title, width = 30)

chan_vids_10 <- chan_vids[1:10, ]

ggplot(data = chan_vids_10, aes(x = wrap_title, y = viewCount)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

ggplot(data = chan_vids_10, aes(x = wrap_title, y = likeCount)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

chan_vids$short_title <- str_wrap(substr(chan_vids$title, 1, 20), width = 10)

ggplot(data = chan_vids, aes(x = as.numeric(viewCount), 
                             y = as.numeric(likeCount), 
                             colour = year)) + 
  geom_jitter() + 
  geom_text(aes(label = short_title), check_overlap = TRUE) + 
  scale_x_continuous(trans = "log") + 
  scale_y_continuous(trans = "log")

ggplot(data = chan_vids, aes(x = as.numeric(viewCount), 
                             y = year, 
                             colour = year)) + 
  geom_jitter() + 
  scale_x_continuous(trans = "log")  + 
  geom_text(aes(label = short_title), check_overlap = TRUE) + 
  guides(colour = FALSE) 

ggplot(data = chan_vids, aes(x = as.numeric(likeCount), 
                             y = year, 
                             colour = year)) + 
  geom_jitter() + 
  scale_x_continuous(trans = "log")  + 
  geom_text(aes(label = short_title), check_overlap = TRUE) + 
  guides(colour = FALSE) 