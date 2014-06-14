# Copyright (C) 2014 John Smith
#     http://あとで   
#
# Apache License
# Version 2.0, January 2004
# http://www.apache.org/licenses/
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

# 何も無かったことにして最初からやり直せる
# 人生も、ニ人関係もこんなに簡単にそんなことが出来たらいいのにね。。
rm(list=ls(all=TRUE))

# データ読み込むよっ！
file.name <- "in1.csv"
data <- read.csv(file.name, header = T)

# ぱらめーた関係ですの！
# 全体の単語数
v <- length(unique(data$word))
# トピック数
k = 5
# ハイパーパラメータ(強そう)
alpha = 50 / k
bata = 0.1

# ランダムにトピック番号の初期値を付与
# 機械学習のパラメータの初期値はいつだってランダムに決まる
# 収束してくれるってわかってるんなら何も苦労することなんて無いのにね
# 人間はどうしてこうも第一印象で有利不利が決まってしまうんだろう。。
data$topic <- sample(1:k, nrow(data), rep = T)

# まさかの関数名がサイコロ
saikoro <- function(doc, word) {
  p = numeric(5)
  # 確率、求めちゃう？
  for (topic in 1:k) {
    # 数式むずかしいよぅ〜(>ω<;)
    p[topic] <- (alpha + sum(data[data$topic == topic & data$doc == doc, "count"])) * 
      (bata + sum(data[data$topic == topic & data$word == word, "count"])) /
      (bata * v + sum(data[data$topic == topic && data$word == word, "count"]))
  }
  # ここでサイコロをふるのですよ
  # 日頃の行いが試される瞬間
  return(sample(x = 1:k, size = 1, prob = p))
}

# るーぷかいすー
iter <- 50
# 各結果を保持しとく人
# この人の記憶力は我々の想像を絶する
result <- matrix(nrow = nrow(data), ncol = iter)

# 回る回ーるよアルゴリズーム♪
for (i in 1:iter) {  
  result[, i] <- apply(data, 1, function(d) {
    doc <- as.numeric(d["doc"])
    word <- d["word"]
    return(saikoro(as.numeric(d[1]), d[2]))
  })
}

# ＿人人人人人人人＿
# ＞　突然の確率　＜
# ￣Y^Y^Y^Y^Y^Y^Y￣
# genereated by 突然の死ジェネレータ
#  http://starwing.net/suddenly_death.html
topic.prob <- apply(result, 1, function(d) {
  s <- length(d)
  probs <- numeric(k)
  for (i in 1:k) {
    probs[i] = sum(d == i) / s
  }
  return(probs)
})
topic.prob <- t(topic.prob)

# まとまるくん
# 参照 : http://www.amazon.co.jp/dp/B004WZ40XW
final.result <- data.frame(doc = data$doc, word = data$word,
                           topic1 = topic.prob[, 1],
                           topic2 = topic.prob[, 2],
                           topic3 = topic.prob[, 3],
                           topic4 = topic.prob[, 4],
                           topic5 = topic.prob[, 5])

# そして出力へ
# 参照 : http://www.amazon.co.jp/dp/B000068HX1/
write.csv(final.result, file = "topic.csv", quote = F, row.names = F)
