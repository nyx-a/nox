#! /usr/bin/env bb

;; flipflap フリフラ 2022

(require '[clojure.string :as s]
         '[babashka.fs :as f]
         '[clojure.tools.cli :refer [parse-opts]])

(defn join [coll]
  "空白でjoinする コレクション中のnilは無視する"
  (reduce #(cond
             (and %1 %2) (s/join " " [%1 %2])
             (every? nil? [%1 %2]) nil
             :else (or %1 %2))
          coll))

(defn bare? [s]
  "相対パスでも絶対パスでもないならtrue"
  (if (re-find #"^\.{0,2}/+" s) false true))

(defn confirm-command [c]
  "コマンド c の実在を確認 見つからなければ直ちに終了する"
  (let [e (some #(if (f/executable? %) %)
                (if (bare? c)
                  (->>
                   (f/exec-paths)
                   (map f/expand-home)
                   (map #(s/join [% f/file-separator c])))
                  [c]))]
    (when (nil? e)
      (println (format "no such command \"%s\"" c))
      (println)
      (System/exit 1))
    e))

(defn parse [s]
  "文字列 s を最初の空白でコマンドとオプションに分ける"
  (let [[fst snd] (if (or (nil? s) (empty? s))
                    []
                    (s/split s #"\s" 2))]
    {:command fst
     :option snd}))

(defn shell [s]
  "コマンド s を実行して標準出力を一時ファイルに出力し java.io.File を返す"
  (let [tmp (java.io.File/createTempFile "flfl." ".out")]
    (babashka.tasks/shell {:out tmp} s)
    tmp))

(defn move-into-tilde [s & {:keys [pretend]}]
  "ファイル s を ~ 付きの名前にリネームする"
  (let [t (s/join [s "~"])]
    (when-not pretend
      (when (f/exists? t)
        (println (format "already exists \"%s\"" t))
        (println)
        (System/exit 1))
      (f/move s t :replace-existing false :nofollow-links true))
    {:bare s :tilde t}))

(defn run [cmdopt org]
  (let [tmp (shell (join cmdopt org))]
    (if (zero? (.length tmp))
      (do
        (.delete tmp)
        :empty)
      (do
        (move-into-tilde org)
        (.renameTo tmp org)
        :success))))

(defn execute [command option files & {:keys [pretend]}]
  (let [c  (confirm-command command)
        co (join [c option])]
    (doseq [f files]
      (print co f)
      (if-not pretend
        (print "->" (run [co f])))
      (println))))

;; ------------------------------------------------------------------

(def args
  (parse-opts
   *command-line-args*
   [["-c" "--command Command to apply"]
    ["-o" "--option Options for the command"]
    ["-p" "--pretend" :default true]
    ["-h" "--help"]]))

(when (or (-> args :options :help)
          (-> args :errors seq))
  (doseq [i (:errors args)]
    (println i))
  (println "options:")
  (println (:summary args))
  (println "usage:")
  (println "  $" *file* "[command to apply] files..")
  (println)
  (System/exit 0))

(if-let [c (-> args :options :command)]
  ;; オプション -c の指定があった
  (let [o (-> args :options :option)
        f (-> args :arguments)]
    (execute c o f
             :pretend (-> args :options :pretend)))
  ;; なかった
  (let [a (parse (-> args :arguments first))
        c (a :command)
        o (join [(a :option) (-> args :options :option)])
        f (-> args :arguments rest)]
    (execute c o f
             :pretend (-> args :options :pretend))))

