# 図書館管理システム (Library Management System)

## 概要

このディレクトリには、GnuCOBOLで実装された図書館管理システムが含まれています。これは既存のシラバス管理システムに加えて実装された、学習・実践用の図書館業務システムです。

## システム構成

### メインプログラム
- **LIBMENU**: システムのメインメニュー
- **LIBINIT**: 初期データ作成プログラム

### 管理機能
- **LIBBOOK**: 蔵書管理（登録・照会・修正・削除）
- **LIBUSER**: 利用者管理（登録・照会・修正・削除）
- **LIBLOAN**: 貸出処理
- **LIBRETURN**: 返却処理

### レポート機能
- **LIBREPORT**: レポートメニュー
- **LIBRPT01**: 延滞者リスト
- **LIBRPT02**: 貸出統計
- **LIBRPT03**: 人気図書ランキング

### データファイル
- **book.dat**: 蔵書マスターファイル（索引順編成）
- **user.dat**: 利用者マスターファイル（索引順編成）
- **loan.dat**: 貸出トランザクションファイル（索引順編成）

### レポートファイル
- **overdue_report.txt**: 延滞者リスト
- **loan_stats.txt**: 貸出統計
- **popular_books.txt**: 人気図書ランキング

## セットアップと実行

### 1. コンパイル
```bash
# 全てのプログラムをコンパイル
make all

# または特定のプログラムのみ
make bin/LIBMENU
```

### 2. 初期データ作成
```bash
# データファイルとサンプルデータを作成
make init-library
```

### 3. システム起動
```bash
# 図書館管理システムを起動
make run-library

# または直接実行
cd bin && ./LIBMENU
```

## 機能説明

### 1. 蔵書管理 (LIBBOOK)
- 図書の新規登録
- 図書情報の照会・検索
- 図書情報の修正
- 図書の削除（貸出中は削除不可）

#### 図書データ項目
- 図書ID（10桁）
- ISBN（13桁）
- 書名（100文字）
- 著者名（60文字）
- 出版社（60文字）
- 出版年（4桁）
- 分類コード（3桁）
- 状態（A:貸出可、B:貸出中）

### 2. 利用者管理 (LIBUSER)
- 利用者の新規登録
- 利用者情報の照会
- 利用者情報の修正
- 利用者の削除（貸出中の図書がある場合は削除不可）

#### 利用者データ項目
- 利用者ID（8桁）
- 氏名（60文字）
- 住所（100文字）
- 電話番号（15文字）
- メールアドレス（50文字）
- 区分（1:一般、2:学生）
- 現在貸出数
- 状態（A:有効、S:停止）

### 3. 貸出処理 (LIBLOAN)
- 利用者IDと図書IDによる貸出
- 貸出可能チェック
  - 図書の状態確認
  - 利用者の貸出上限チェック（5冊まで）
- 返却期限設定（14日間）
- 貸出履歴の記録

### 4. 返却処理 (LIBRETURN)
- 図書IDによる返却
- 延滞チェック
- 延滞料金の通知
- 貸出状態の更新

### 5. レポート機能
#### 延滞者リスト (LIBRPT01)
- 返却期限を過ぎた貸出の一覧
- 延滞日数の計算
- 利用者情報と図書情報の表示

#### 貸出統計 (LIBRPT02)
- 総貸出件数
- 貸出中件数
- 返却済件数

#### 人気図書ランキング (LIBRPT03)
- 貸出回数による図書のランキング
- 上位10位まで表示

## データベース設計

### エラーコード
- E001: ファイルが見つかりません
- E002: 重複するIDが存在します
- E003: 貸出上限を超えています
- E004: この図書は貸出できません
- E005: 入力値が不正です
- E006: 該当するデータが見つかりません
- E007: ファイル書き込みエラー
- E008: システムエラー

## サンプルデータ

初期化時に以下のサンプルデータが作成されます：

### 図書データ
- B000000001: リーダブルコード
- B000000002: プログラマの数学
- B000000003: 基礎からのMySQL

### 利用者データ
- U0000001: 山田太郎
- U0000002: 鈴木花子

## 技術仕様

- **COBOL処理系**: GnuCOBOL 3.1以上
- **ファイル組織**: 索引順編成ファイル（ISAM）
- **文字コード**: UTF-8
- **画面形式**: テキストベースCUI

## 開発について

このシステムは学習目的で作成されており、以下の技術要素を含んでいます：

- COBOL の基本的なファイル処理
- 索引順編成ファイルの使用
- 画面セクションによるユーザーインターフェース
- プログラム間の呼び出し
- エラーハンドリング
- レポート生成

## 制限事項

- 同時実行制御は実装されていません
- バックアップ機能は基本的なもので、本格運用には適しません
- 画面表示は日本語フォントに依存します

## ファイル構成

```
src/
├── LIBMENU.cbl      # メインメニュー
├── LIBINIT.cbl      # 初期化プログラム
├── LIBBOOK.cbl      # 蔵書管理
├── LIBUSER.cbl      # 利用者管理
├── LIBLOAN.cbl      # 貸出処理
├── LIBRETURN.cbl    # 返却処理
├── LIBREPORT.cbl    # レポートメニュー
├── LIBRPT01.cbl     # 延滞者リスト
├── LIBRPT02.cbl     # 貸出統計
├── LIBRPT03.cbl     # 人気図書ランキング
└── copybooks/
    ├── BOOKFILE.cpy # 図書ファイル定義
    ├── USERFILE.cpy # 利用者ファイル定義
    ├── LOANFILE.cpy # 貸出ファイル定義
    └── LIBERROR.cpy # エラー処理定義
```