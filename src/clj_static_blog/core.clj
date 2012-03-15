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
  (loop [files []
         togo [root]]
    (if-let [file (first togo)]
      (cond
       (.isDirectory file)
       (recur
        files
        (into (rest togo) (doall (map (partial io/file file) (.list file)))))
       (.isFile file)
       (recur
        (conj files file)
        (rest togo)))
      files)))

(comment
  (list-files (io/file "/home/mfex/Programming/thegeez.github.com/source"))
  )

(def date-print-format (time-format/formatter "dd MMM YYYY"))

(defn collect-posts [source]
  (let [dir (io/file source "posts")]
    (println "Finding posts with YYYY/MM/DD/ path in" (.toURI dir))
    (for [post-path (->> (io/file source "posts")
                    list-files
                    (filter #(re-find #"\d{4}/\d{2}/\d{2}" (.getAbsolutePath %))))
          :let [path (.getAbsolutePath post-path)
                [_ YYYY MM DD filename] (re-matches #".*/(\d{4})/(\d{2})/(\d{2})/(.*)" path)]]
      {:date [YYYY MM DD]
       :date-print (time-format/unparse date-print-format (apply time/date-time (map #(Long/parseLong %) [YYYY MM DD])))
       :filename filename
       :path path
       :page-path (str/join "/" [YYYY MM DD filename])})))

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
                [:div#content]
                (e/content
                 (e/at (:html post-data)
                       [:h1#post-title]
                       (e/after (:date-print post-data)))))))

(defn build-index-page [template-html post-list-template posts]
  (render (e/at template-html
                [:div#content]
                (e/substitute
                 (e/at post-list-template
                       [[:li.entry (e/but e/first-child)]]
                       (e/substitute)
                       [[:li.entry e/first-child]]
                       (e/clone-for [{:keys [date-print post-title page-path]} posts]
                                    [:div.date] (e/content date-print)
                                    [:div.post-title :a] (e/do->
                                                          (e/content post-title)
                                                          (e/set-attr :href page-path))))))))

(defn build-atom-feed [template-xml posts]
  (let [time-print #(time-format/unparse (time-format/formatters :date-time-no-ms) %)]
    (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
         (render (e/at template-xml
                       [:feed] (e/set-attr :xmlns "http://www.w3.org/2005/Atom")
                       [:feed :updated]
                       (e/content (time-print (time/now)))
                       [:entry]
                       (e/clone-for [{:keys [date post-title page-path html]} posts]
                                    [:title] (e/content post-title)
                                    [:link] (append-attr :href page-path)
                                    [:updated] (let [[YYYY MM DD] date]
                                                 (e/content (time-print (apply time/date-time (map #(Long/parseLong %) [YYYY MM DD])))))
                                    [:id] (e/append page-path)
                                    [:content] (e/content (render html))))))))

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

(comment
  ;; html is seq of {:tag :attrs :content}
  (def html (e/html-resource (io/file "/path/to/source/_postlist.html")))
  ;; => ({:tag :html, :attrs nil, :content ({:tag :body, :attrs nil,
  ;; :content ({:tag ...
  
  ;; to textual html again
  (e/emit* html)
  ;; => ("<" "html" ">" "<" "body" ">" "<" "div" " " "id" "=\"" "home"
  ;;.....

  ;; to a string
  (apply str (e/emit* html))
  ;; => "<html><body><div id=\"home.....

  ;; enlive is about transformations of seq of nodes to seq of nodes
  (def date-node (e/select html [:div.date]))
  ;; => ({:tag :div, :attrs {:class "date"}, :content ("YYYY/MM/DD")})

  ;; tranform applies a transformation to a node
  (e/transform date-node
               [:div.date]
               (fn [match]
                 (assoc match :content "2012/03/15")))
  ;; => ({:tag :div, :attrs {:class "date"}, :content "2012/03/15"})
  
  ;; the results of a transformation may also be a seq of nodes
  (e/transform date-node
               [:div.date]
               (fn [match-node]
                 [{:tag "hr" :attrs nil :content []}
                  {:tag "p" :attrs nil :content "Whole new structure"}]))
  ;; => ({:tag "hr", :attrs nil, :content []} {:tag "p", :content "Whole new structure", :attrs nil})
  
  
  )
