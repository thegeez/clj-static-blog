(ns clj-static-blog.core
  (:use [clojure.pprint :only (pprint)])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [net.cgrand.enlive-html :as e]
            [clj-time.core :as time]
            [clj-time.format :as time-format])
  (:import [java.io File]))

;;;; enlive helpers
(defn render [tags]
  (apply str (e/emit* tags)))

(defn append-attr [attr v]
  (fn [match]
    (update-in match [:attrs attr] str v)))
;;;; end of enlive helpers

(defn list-files [^File root]
  (->> (tree-seq #(.isDirectory %)
                 #(.listFiles %)
                 root)
       (filter #(.isFile %))))

(comment
  (list-files (io/file "/home/mfex/Programming/thegeez.github.com/source"))
  )

(def date-print-format (time-format/formatter "dd MMM YYYY"))

(defn collect-posts [source]
  (let [dir (io/file source "posts")]
    (println "Finding posts with YYYY/MM/DD/ path in" (.toURI dir))
    (->> (for [post-path (->> (io/file source "posts")
                              list-files
                              (filter #(re-find #"\d{4}/\d{2}/\d{2}" (.getAbsolutePath %))))
               :let [path (.getAbsolutePath post-path)
                     [_ YYYY MM DD filename] (re-matches #".*/(\d{4})/(\d{2})/(\d{2})/(.*)" path)]]
           {:date [YYYY MM DD]
            :date-print (time-format/unparse date-print-format (apply time/date-time (map #(Long/parseLong %) [YYYY MM DD])))
            :filename filename
            :path path
            :page-path (str/join "/" [YYYY MM DD filename])})
         (sort-by (fn [post]
                    (let [[YYYY MM DD] (map #(Long/parseLong %) (:date post))]
                      (* -1 (+ (* YYYY 10000)
                               (* MM 100)
                               DD))))))))

(defn add-post-data [post-data]
  (let [path (:path post-data)
        html (e/html-resource (io/file path))
        post-title (-> (e/select html [:h1#post-title])
                       first
                       :content
                       first)]
    {:html html
     :post-title post-title}))

(defn build-post-page [template-html post-data]
  (render (e/at template-html
                [:title]
                (e/append (str " - " (:post-title post-data)))
                [:div#content]
                (e/content
                 (e/select
                  (e/at (:html post-data)
                        [:h1#post-title]
                        (e/after (:date-print post-data)))
                  [:html :body :> e/any-node])))))

(defn build-index-page [template-html post-list-template posts]
  (render (e/at template-html
                [:div#content]
                (e/substitute
                 (e/select
                  (e/at post-list-template
                        [[:li.entry (e/but e/first-child)]]
                        (e/substitute)
                        [[:li.entry e/first-child]]
                        (e/clone-for [{:keys [date-print post-title page-path]} posts]
                                     [:div.date] (e/content date-print)
                                     [:div.post-title :a] (e/do->
                                                           (e/content post-title)
                                                           (e/set-attr :href page-path))))
                  [:html :body :> e/any-node])))))

(defn build-atom-feed [template-xml posts]
  (let [time-print #(time-format/unparse (time-format/formatters :date-time-no-ms) %)]
    (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
         (render (e/at template-xml
                       [:feed] (e/set-attr :xmlns "http://www.w3.org/2005/Atom")
                       [:feed :updated]
                       (e/content (time-print (time/now)))
                       [:entry]
                       (e/clone-for [{:keys [date post-title page-path html]} posts
                                     :let [preview-part-count 6
                                           post-parts (e/select html
                                                                [:html :body :> e/any-node])
                                           [preview-parts rest-parts] (split-at preview-part-count post-parts)]]
                                    [:title] (e/content post-title)
                                    [:link] (append-attr :href page-path)
                                    [:updated] (let [[YYYY MM DD] date]
                                                 (e/content (time-print (apply time/date-time (map #(Long/parseLong %) [YYYY MM DD "23" "00"])))))
                                    [:id] (e/append page-path)
                                    [:content]
                                    (e/content
                                     (render (if (seq rest-parts)
                                               (concat preview-parts
                                                       [{:tag :a
                                                         :attrs {:href page-path}
                                                         :content ["Read more"]}])
                                               preview-parts)))))))))

(defn write-page [^File dir filename page]
  (when-not (.exists dir)
    (.mkdirs dir))
  (spit (io/file dir filename) page))

(defn generate []
  (let [settings (read-string (slurp "settings.clj"))
        source (io/file (:source settings))
        target (io/file (:target settings))]
    (println "Using settings:")
    (pprint settings)
    (let [posts (->> (:source settings)
                     collect-posts
                     (map #(merge % (add-post-data %))))]
      (let [template-html (e/html-resource (io/file source "_template.html"))
            post-list-template (e/html-resource (io/file source "_postlist.html"))
            index-page (build-index-page template-html post-list-template posts)]
        (write-page target "index.html" index-page)
        (doseq [post-data posts]
          (let [page (build-post-page template-html post-data)
                [YYYY MM DD] (:date post-data)
                filename (:filename post-data)]
            (write-page (io/file target YYYY MM DD) filename page))))
      (let [atom-xml (e/html-resource (io/file source "_atom.xml"))
            feed (build-atom-feed atom-xml posts)]
        (write-page target "atom.xml" feed)))))
