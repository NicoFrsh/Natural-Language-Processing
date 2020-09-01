#### Modeling

# sample data
if (!file.exists("Samples/blogsSample02.txt")){
        # start connection with file
        conBlogs <- file("https://www.dropbox.com/s/v2085oc901rx0xf/en_US.blogs.txt?raw=1", "r")
        # read all lines and then sample 10%
        set.seed(1234)
        blogs <- readLines(con)
        blogsSample <- blogs[rbinom(length(blogs)*0.2, length(blogs), 0.5)]
        writeLines(blogsSample, con = "Samples/blogsSample02.txt")
        close(conBlogs)
        rm(blogs, blogsSample)
}

if (!file.exists("Samples/newsSample02.txt")){
        conNews <- file("https://www.dropbox.com/s/pworwsmh2nhlub3/en_US.news.txt?raw=1", "r")
        news <- readLines(conNews)
        set.seed(1235)
        newsSample <- news[rbinom(length(news)*0.2, length(news), 0.5)]
        writeLines(newsSample, con = "Samples/newsSample02.txt")
        close(conNews)
        rm(news, newsSample)
}

if (!file.exists("Samples/twitterSample02.txt")){
        conTwitter <- file("https://www.dropbox.com/s/2ki49yjvuqadz16/en_US.twitter.txt?raw=1", "r")
        twitter <- readLines(conTwitter)
        set.seed(1236)
        twitterSample <- twitter[rbinom(length(twitter)*0.2, length(twitter), 0.5)]
        writeLines(twitterSample, con = "Samples/twitterSample02.txt")
        close(conTwitter)
        rm(twitter, twitterSample)
}


