(defproject merkle "0.1.1-SNAPSHOT"
  :description "Merkle trees for constructing efficient diffs over collections."
  :url "http://github.com/aphyr/merkle"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [primitive-math "0.1.3"]]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev {:dependencies [[org.clojure/data.generators "0.1.0"]
                                  [reiddraper/simple-check "0.4.0"]]}}
  :jvm-opts ^:replace ["-server"])
