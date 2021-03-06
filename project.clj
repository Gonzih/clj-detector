(defproject me.gonzih.clj-detector "0.0.4"
  :description "Clojure interface to UADetector, a library to analyse User-Agent strings"
  :url "http://github.com/pingles/clj-detector"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.sf.uadetector/uadetector-core "0.9.22"]]
  :profiles {:dev {:dependencies [[net.sf.uadetector/uadetector-resources "2014.10"]
                                  [org.slf4j/slf4j-simple "1.7.21"]]}})
