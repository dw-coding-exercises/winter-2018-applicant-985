(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]))

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    [:div "Hello from search!"]))
