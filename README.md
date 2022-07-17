# 営業日カレンダーモデル

https://zenn.dev/zahn/articles/948fc5b66648a3

この記事で提案されている営業日カレンダーモデルを実装する

なおパーサーコンビネーターの都合上一部規則を変更している


## 変更後のEBNF

```
 binop    ::= '+'
            | '-'
 castop   ::= '_'
            | '^'
 cal      ::= 'jp'
            | 'us'
            | 'c'
            | 'jp&us'
            | 'jp|us'
 expr     ::= term cast_op cal [ binop num ]
 term     ::= "(" expr ")" | "T"
```

## 利用方法
WIP