# 友達作り

## 問題文
今年度の小山高専にはN人が入学する予定で、名前順に1からNまでの学籍番号が割り当てられています。  
そのうちの一人である太郎君は、N-1人の新入生全員と友達になりたいと考えています。  
ここで、「太郎君と次郎君は友達である」とは、太郎君の友達の友達の…と交友関係をたどったとき、次郎君にたどり着けることを意味します。

太郎君を含む新入生のうち、同じ中学校出身といった関係で既に交友を持つ学生がいる場合があります。  
太郎君は、太郎君自身を含む任意の2人を「紹介」して友達にすることができますが、できるだけ少ない回数で全員と友達になりたいです。

太郎君が新入生全員と友達になるためには、最小で何回の紹介を行う必要があるか答えてください。  
ただし、太郎君が新たに紹介を行う必要がない場合は0を出力してください。

----

## 入力
入力は以下の形式で与えられる。
```
N M
a_1 b_1
a_2 b_2
：
a_M b_M
```

 * 1行目には新入生の数を表すN(2≦N≦100000)と、既に存在する友達関係の数を表すM(0≦M≦N(N-1)/2)が空白区切りで与えられる。
 * 続く2行目からM行にかけて、既に存在する友達関係の情報が与えられる。
   + このうちi行目では、i番目の友達関係における2人の学生の学籍番号を表す2つの整数a_i, b_i(1≦a_i＜b_i≦N)が空白区切りで与えられる。
     - 同一人物が友達関係になることはなく、`a_i ≠ b_i`を満たす。
     - 同じ内容の友達関係が与えられることはなく、`i ≠ j`ならば、`a_i ≠ a_j` または `b_i ≠ b_j`を満たす。

## 出力
太郎君が新入生全員と友達になるために行う紹介の最小の回数を出力せよ。