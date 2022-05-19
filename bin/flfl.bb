#! /usr/bin/env bb

;; flipflap フリフラ 2022

(require '[clojure.string :refer [split join]]
         '[babashka.fs :as f]
         '[clojure.tools.cli :refer [parse-opts]])

;; ------------------------------------------------------------------

(defn confirm-command [c]
  (let [e (->>
           (split (System/getenv "PATH") (re-pattern f/path-separator))
           (map f/expand-home)
           (map #(join [% f/file-separator c]))
           (cons c)
           (some #(if (f/executable? %) %)))]
    (when (nil? e)
      (println (format "no such command \"%s\"" c))
      (println)
      (System/exit 1))
    e))

(defn shelter [s]
  (let [t (join [s "~"])]
    (when (f/exists? t)
      (println (format "already exists \"%s\"" t))
      (println)
      (System/exit 1))
    (f/move s t :replace-existing false :nofollow-links true)
    {:bare s :tilde t}))

;; ------------------------------------------------------------------

(def args
  (parse-opts
   *command-line-args*
   [["-x" "--command"]
    ["-o" "--option"]
    ["-p" "--pretend" :default false]
    ["-h" "--help"]]))

(when
    (or (-> args :options :help)
        (-> args :errors seq)
        (-> args :arguments count (< 2)))
  (doseq [i (:errors args)]
    (println i))
  (println "options:")
  (println (:summary args))
  (println "usage:")
  (println "  $" *file* "[command to apply] files..")
  (println)
  (System/exit 0))

(let [command-name      (-> args :arguments first)
      files             (-> args :arguments rest)
      confirmed-command (confirm-command command-name)]

  (cons confirmed-command files)






)
