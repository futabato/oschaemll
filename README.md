# Mini Shell (OCaml)

型駆動で設計された簡易シェル。ADT（代数的データ型）とパターンマッチの学習用。

## ビルド & 実行

```bash
opam install -y --deps-only --with-test .   # 初回のみ
make build                                    # ビルド（= 型チェック）
make test                                     # テスト
make run                                      # REPL起動
make fmt                                      # フォーマット
```

## ディレクトリ構成

```
lib/
├── types.ml       ← 型定義（ここが本体）
├── tokenizer.ml   ← string → token list
├── parser.ml      ← token list → command_line
├── executor.ml    ← command_line → pipeline_result
└── shell.ml       ← 上記を合成する公開API
bin/
└── main.ml        ← REPL エントリーポイント
test/
└── test_shell.ml  ← Alcotest によるユニット/統合テスト
```

## 対応構文

| 構文 | 例 |
|------|-----|
| 単純コマンド | `ls -la /tmp` |
| パイプライン | `cat file \| grep hello \| wc -l` |
| リダイレクト | `> out.txt`, `< in.txt`, `>> log.txt` |
| 接続子 | `cmd1 ; cmd2`, `make && make install`, `false \|\| echo fallback` |
| クォート | `echo 'hello world'`, `echo "hello world"` |
