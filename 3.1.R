install.packages("multilinguer"); library(multilinguer)
install_jdk() 

install.packages(c( 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools', 'bit', 'rex',
'lazyeval', 'htmlwidgets', 'crosstalk', 'promises', 'later', 'sessioninfo',
'xopen', 'bit64', 'blob', 'DBI', 'memoise', 'plogr', 'covr', 'DT', 'rcmdcheck',
'rversions'), type = 'binary')

install.packages("remotes"); library(remotes)
install_github( "haven-jeon/KoNLP", upgrade = "never", INSTALL_opts = c("--no-multiarch"))

download.file(url = "https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.11.8/scala-library-2.11.8.jar",
                destfile = paste0(.libPaths()[1], "/KoNLP/Java/scala-library-2.11.8.jar"))

library(KoNLP) 

useNIADic() 

library(tidyverse)

# 데이터 불러오기 
raw_data <- read_csv("./csv/월별_내림차순/파일명") %>%
  select(ID, 제품특성, everything()) %>% print()

easy_view <- raw_data %>% select(ID, 제품특성) %>% print()

# 기본적인 전처리
library(textclean)
data <- raw_data %>%
  filter(str_count(제품특성, " ") >= 1) %>% # 띄어쓰기 1개 이상 추출
  mutate(info_raw = str_squish(replace_html(제품특성)), # 원문 보유
         info = str_squish(제품특성)) %>% print() # 중복 공백 제거

# 주요 단어 추출 
library(tidytext)
word_noun <- data %>%
  unnest_tokens(input = 제품특성, output = word,
                token = extractNoun,
                drop = F)

word_noun %>% select(제품특성, word)

# 단어 빈도 구함
freq <- word_noun %>%
  count(word, sort = T) %>% # 단어 빈도 구해 내림차순 정렬
  filter(str_length(word) > 1) %>% print(n=Inf) # 두 글자 이상만 남기기

top40_noun <- freq %>%
  head(40) %>% print(n=Inf) 

# 막대 그래프 작성 
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
library(scales)
top40_noun %>% ggplot(aes(x = reorder(word, n), y = n,
                            fill =word)) +
  geom_col(show.legend = F) +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)), hjust = -0.3) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(title = "top_23_4 주요 키워드",
       subtitle = "언급 빈도 Top 40",
       x = NULL, y = "단어 수") +
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic", size = 12),
        plot.title = element_text(size = 14, face = "bold"), # 제목 폰트
        plot.subtitle = element_text(size = 13)) # 부제목 폰트

# 워드클라우드 
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()
library(ggwordcloud)
top40_noun %>% ggplot(aes(label = word, size = n,
                    color = factor(n))) + # 색상 지정
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(2, NA),
               range = c(3, 15)) +
  theme_minimal()

# '00' 단어가 들어간 문장 찾기(단어 결정해서 알려주기) 
'''
sentences <- raw_data %>%
  str_squish() %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences") %>% print()

filtered_섭취 <- easy_view %>%
  filter(str_detect(제품특성, "섭취")) %>% print()
'''